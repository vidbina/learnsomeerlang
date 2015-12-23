-module(functions).
-compile(export_all).

head([H|_]) -> H.
second([_, H|_]) -> H.

% Interesting syntax but the concept of unbound variables makes this possible
same(X,X) ->
  true;
same(_,_) ->
  false.

fac(0) -> 1;
fac(X) when X > 0 -> X * fac(X-1).

% finally a proper implementation because fac(-1) gets stuck everlasting
% recursion.
guarded_fac(X) when X =:= 0 -> 1;
guarded_fac(X) when X > 0 -> X*guarded_fac(X-1).

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
  io:format("Date tuple ~p represents ~p/~p/~p,~n", [Date, Y, M, D]),
  io:format("Time tuple ~p represents ~p:~p:~p.~n", [Time, H, Min, S]);
valid_time(_) ->
  io:format("Invalid time!~n").
