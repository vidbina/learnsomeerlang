-module(recursive).
-export([fac/1, tail_fac/1, len/1, tail_len/1, dup/2, rev/2]).

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
len([_]) -> 1;
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
