-module(multiproc).
-compile(export_all).

%% Will wait time T before doing something
sleep(T) ->
   receive
   after T -> ok
   end.

%% will empty the mailbox recursively (match any message), and if there is
%% no message anymore, return ok.
flush() ->
   receive
      _ -> flush()
   after 0 ->
      ok
   end.

%% like flush, but with a possibility to prioritize
important() ->
   receive
      {Priority, Message} when Priority > 10 ->
         [Message | important()]
   after 0 ->
      normal()
   end.

normal() ->
   receive
      {_, Message} ->
         [Message | normal()]
   after 0 ->
      []
   end.
