-module(road).
-compile(export_all).

load_map(Filename) ->
	{ ok, Bin } = file:read_file(Filename),
	tupelize(splitn(3, [list_to_integer(E) || E <- string:tokens(binary_to_list(Bin), "\n")])).

route(Filename) ->
	shortroute(load_map(Filename)).

split(L) -> split([], L).
split([{A}|Acc], [H|L]) -> split([{A, H}|Acc], L);
split([{A, B}|Acc], [H|L]) -> split([{A, B, H}|Acc], L);
split(Acc, [H|L]) -> split([{H}|Acc], L);
split(L, []) -> L.

splitn(N, L) -> splitn(N, L, []).

splitn(N, [Item|Remaining], [H|Acc]) when length(H) < N ->
	splitn(N, Remaining, [[Item|H]|Acc]);
splitn(N, [Item|Remaining], Acc) ->
	splitn(N, Remaining, [[Item]|Acc]);
% splitn(N, [Item|Remaining], [Last|Acc]) ->
%	  splitn(N, Remaining, [[Item]|[lists:reverse(Last)|Acc]]);
splitn(_, [], L) -> lists:reverse(lists:map(fun(X) -> lists:reverse(X) end, L)).

tupelize(L) -> lists:reverse(tupelize(L, [])).
tupelize([H|Rest], Acc) ->
	tupelize(Rest, [list_to_tuple(H)|Acc]);
tupelize([], Acc) -> Acc.

shortroute(Map) ->
	lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, Map).

% cheated by copy-pasting this one
shortest_step({A,B,X}, {{DistA,PathA}, {DistB,PathB}}) ->
  OptA1 = {DistA + A, [{a,A}|PathA]},
  OptA2 = {DistB + B + X, [{x,X}, {b,B}|PathB]},
  OptB1 = {DistB + B, [{b,B}|PathB]},
  OptB2 = {DistA + A + X, [{x,X}, {a,A}|PathA]},
  {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.
% ----x----x----x----x----x
%     |    |    |    |    
%     |    |    |    |    
%     |    |    |    |    
% ----x----x----x----x----x

% -90-x--5-x-40-x-10-x----x
%     |    |    |    |    
%    10   90    2    8    
%     |    |    |    |    
% -30-x-20-x-25-x--0-x----x

% -50-x--5-x-40-x-10-x----x
%     |    |    |    |    
%    10   90    2    8    
%     |    |    |    |    
% -30-x-20-x-25-x--0-x----x

% 1   2    3    4    5
% -50-x--5-x-40-x-10-x----x A
%     |    |    |    |    
%    30   20   25    0    
%     |    |    |    |    
% -10-x-90-x--2-x--8-x----x B

% A1:A2 A2:A3
% 50    5
% 10+30 

% B1:B2 B2:B3
% 10    90
%       30+5+20
