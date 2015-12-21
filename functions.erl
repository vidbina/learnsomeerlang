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
