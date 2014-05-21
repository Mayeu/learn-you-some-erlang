-module(functions).
-compile(export_all). %% Replace this with -export()!

%% Example of pattern matching function

%% Matching part of a list
head([H|_]) -> H.
second([_,X|_]) -> X.

%% Match only if the args are the same
same(X,X) ->
   true;
same(_,_) ->
   false.

%% Match twot tuple of three value each
valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
   io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n", [Date,Y,M,D]),
   io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
   io:format("Stop feeding me wrong data!~n").

