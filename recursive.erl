-module(recursive).
-export([fac/1, tail_fac/1, len/1, tail_len/1, dup/2, rev/2, sublist/2, zip/2]).

% n! = n * (n-1) * ... * 1
fac(N) when N == 0 -> 1;
fac(N) when N >= 1 -> N*fac(N-1).

% n! = n * ( n-1 * ( ... ) )
% n! = n * (n-1)!
%   3! = 3*1, 2!
%   2! = 2*(3*1), 1!
%   1! = 1*(2*(3*1)), 0!
%   f_o(3) -> f(3,1)
%   f(3,1) -> f(2,3*1)
%   f(2,3*1) -> f(1,2*3*1)
%   f(0,2*3*1) -> (1*2*3) -> 6
%
tail_fac(N) -> tail_fac(N, 1).
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).

len([]) -> 0;
len([_|T]) -> 1+len(T).

% tail recursion: keep ops in args rather than function body in order to
% circumvent long operations. Solve as you go.
%
% f_o([_,_,_,_]) -> f([_,_,_,_], 1)
% f([_,_,_,_], 1) -> f([_,_,_], 1+1)
% f([_,_,_], 1+1) -> f([_,_], 1+1+1)
% f([_,_], 1+1+1) -> f([_], 1+1+1+1) -> 4
tail_len(L) -> tail_len(L, 1).
tail_len([_], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, 1+Acc).

dup(X, N) -> dup(X, N, []).
dup(_, 0, Acc) -> Acc;
dup(X, N, Acc) when N > 0 -> dup(X, N-1, [X|Acc]).

rev([], Reversed) -> Reversed;
rev([Head|Tail], Reversed) -> rev(Tail, [Head|Reversed]).

% wondering why @mononcqc went through the trouble of executing
%   tail_sublist([H|T], N, SubList) when N > 0 ->
%     tail_sublist(T, N-1, [H|SubList]).
% and subsequently reversing the resulting list in order to maintain the order
% instead of just appending the head to the right end of the list right away:
%   tail_sublist([H|T], N, SubList) when N > 0 ->
%     tail_sublist(T, N-1, SubList++[H]).
% Is there a performance penalty in appending with `++` over adding a head with
% `|`?
% Oh goodness there was... just read http://vid.bina.me/tools/pros-of-conses
sublist(_, 0, Sublist) -> Sublist;
sublist([], _, Sublist) -> Sublist;
sublist([Head|Tail], N, Sublist) when N>0 ->
  sublist(Tail, N-1, [Head|Sublist]).

sublist(List, N) -> lists:reverse(sublist(List, N, [])).

% throws error on lists with different lengths.
zip([], [], ZippedList) -> ZippedList;
zip([HeadA|TailA], [HeadB|TailB], ZippedList) ->
  zip(TailA, TailB, [{HeadA, HeadB}|ZippedList]).

zip(ListA, ListB) -> lists:reverse(zip(ListA, ListB, [])).
