-module(guards).
-compile(export_all). %% Replace this with -export()!

%% Example of guards to check args
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

right_age(X) when X >= 16, X =< 104 ->
   true;
right_age(_) ->
   false.

%% same as previous but opposite logic
wrong_age(X) when X < 16; X > 104 ->
   true;
wrong_age(_) ->
   false.
