-module(ppool_serv).
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% The friendly supervisor is started dynamically!
-define(SPEC(MFA),
        {worker_sup,
         {ppool_worker_sup, start_link, [MFA]},
         temporary,
         10000,
         supervisor,
         [ppool_worker_sup]}).

-record(state, {limit=0,
                sup,
                refs,
                queue=queue:new()}).

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
   gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
   gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
   gen_server:call(Name, {run, Args}).

sync_queue(Name, Args) ->
   gen_server:call(Name, {sync, Args}, infinity).

async_queue(Name, Args) ->
   gen_server:cast(Name, {async, Args}).

stop(Name) ->
   gen_server:call(Name, stop).

init({Limit, MFA, Sup}) ->
   %% We need to find the Pid of the worker supervisor from here,
   %% but alas, this would be calling the supervisor while it waits for us!
   %% So we send a message that will trigger the recuperation of this Pid.
   self() ! {start_worker_supervisor, Sup, MFA},
   {ok, #state{limit=Limit, refs=gb_sets:empty()}}.

%% Handle the message we send during init to ask the Pid of the worker supervisor
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
   {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
   link(Pid),
   {noreply, S#state{sup=Pid}};
handle_info(Msg, State) ->
   io:format("Unknown msg: ~p~n", [Msg]),
   {noreply, State}.
