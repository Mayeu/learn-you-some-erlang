-module(recursive).
-export([fac/1, len/1, tail_fac/1, tail_len/1, duplicate/2, tail_duplicate/2, reverse/1, tail_reverse/1, sublist/2, tail_sublist/2, zip/2, lenient_zip/2, tail_zip/2, tail_lenient_zip/2, quicksort/1, lc_quicksort/1]).

fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

tail_fac(N) -> tail_fac(N,1).

tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) -> tail_fac(N-1, N*Acc).

tail_len(L) -> tail_len(L,0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc+1).

duplicate(0,_) ->
   [];
duplicate(N,Term) when N > 0 ->
   [Term|duplicate(N-1, Term)].

tail_duplicate(N,Term) ->
   tail_duplicate(N,Term,[]).

tail_duplicate(0,_,List) ->
   List;
tail_duplicate(N,Term,List) ->
   tail_duplicate(N-1, Term, [Term|List]).

reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

tail_reverse(L) -> tail_reverse(L,[]).

tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H|Acc]).

sublist(_,0)  -> [];
sublist([],_) -> [];
sublist([H|T],N)  -> [H|sublist(T, N-1)].

tail_sublist(L,N) -> tail_reverse(tail_sublist(L, N, [])).

tail_sublist(_, 0, Acc)  ->
   Acc;
tail_sublist([], _, Acc) ->
   Acc;
tail_sublist([H|T], N, Acc) -> tail_sublist(T, N-1, [H|Acc]).

zip([],[]) -> [];
zip([X|Xs], [Y|Ys]) -> [{X, Y}|zip(Xs,Ys)].

lenient_zip([],_) -> [];
lenient_zip(_,[]) -> [];
lenient_zip([X|Xs], [Y|Ys]) -> [{X, Y}|lenient_zip(Xs,Ys)].

tail_zip(X,Y) -> tail_reverse(tail_zip(X, Y, [])).

tail_zip([],[], Acc) ->
   Acc;
tail_zip([X|Xs], [Y|Ys], Acc) ->
   tail_zip(Xs, Ys, [{X, Y}|Acc]).


tail_lenient_zip(X,Y) -> tail_reverse(tail_lenient_zip(X, Y, [])).

tail_lenient_zip([],_,Acc) ->
   Acc;
tail_lenient_zip(_,[],Acc) ->
   Acc;
tail_lenient_zip([X|Xs], [Y|Ys], Acc) ->
   tail_lenient_zip(Xs, Ys, [{X, Y}|Acc]).

%% Naive quisort
quicksort([]) ->
   [];
quicksort([Pivot|Rest]) ->
   {Smaller, Larger} = partition(Pivot, Rest, [], []),
   quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) ->
   {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
   if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
      H > Pivot  -> partition(Pivot, T, Smaller, [H|Larger])
   end.

%% using list comprehension
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
   lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
   ++ [Pivot] ++
   lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).
