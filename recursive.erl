-module(recursive).
-export([fac/1]).

fac(N) when N == 0 -> 1;
fac(N) when N >= 1 -> N*fac(N-1).
