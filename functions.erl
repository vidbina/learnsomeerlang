-module(functions).
-compile(export_all).

head([H|_]) -> H.
second([_, H|_]) -> H.

% Interesting syntax but the concept of unbound variables makes this possible
same(X,X) ->
  true;
same(_,_) ->
  false.

fact(0) ->
  0;
fact(1) ->
  1;
fact(X) ->
  X * fact(X-1).

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
  io:format("Date tuple ~p represents ~p/~p/~p,~n", [Date, Y, M, D]),
  io:format("Time tuple ~p represents ~p:~p:~p.~n", [Time, H, Min, S]);
valid_time(_) ->
  io:format("Invalid time!~n").
