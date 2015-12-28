-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

% Use as add(fun one/0, fun two/0)
add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

double(Val) -> 2*Val.

% try hhfuns:map(fun hhfuns:double, [5,10,11]).
map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

% example of directly calling an anon func
% (fun(1) -> one; (2) -> two; (_) -> something end)(3).

% (fun(Room) ->
%   io:format("Alarm set in ~s.~n", [Room]),
%   fun() -> io:format("Alarm tripped in ~s! Call Batman!~n", [Room]) end
% end("office"))()

% Named anon funcs :)
% (fun Loop() -> timer:sleep(500), io:format("."), Loop() end)().

even(L) -> lists:reverse(even(L, [])).

even([], Acc) -> Acc;
even([H|T], Acc) when H rem 2 == 0 -> even(T, [H|Acc]);
even([_|T], Acc) -> even(T, Acc).

old_men(L) -> lists:reverse(old_men(L, [])).

old_men([], Acc) -> Acc;
old_men([H|T], Acc) when H > 60 -> old_men(T, [H|Acc]);
old_men([_|T], Acc) -> old_men(T, Acc).