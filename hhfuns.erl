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

filter(L, F) -> lists:reverse(filter(L, F, [])).

filter([], _, Acc) -> Acc;
% not using when doesn't work out here because when F(X) == something cannot be
% evaluated in a guard as specified in
% http://erlang.org/doc/reference_manual/expressions.html#id83710
% http://stackoverflow.com/questions/2241340/unable-to-use-function-call-in-function-guard
% Probably having something to do with determining the branches and non-reachable
% branches at compile-time. From my observations it appears that all guards can
% be determined at compile time. With a function in the guard, we open up the
% world to a mess of options and problems. The case evaluates during run-time,
% which means that on a guard level we've already entered the function branch.
filter([H|T], F, Acc) -> case F(H) of
                        true -> filter(T, F, [H|Acc]);
                        false -> filter(T, F, Acc)
                      end.

max2(L) -> max2(L, undefined).
max2([], Max) -> Max;
max2([H|T], undefined) -> max2(T, H);
max2([H|T], Max) when H > Max -> max2(T, H);
max2([_|T], Max) -> max2(T, Max).
