-module(hhfuns).
-compile(export_all).

one() -> 1.
two() -> 2.

% Use as add(fun one/0, fun two/0)
add(X, Y) -> X() + Y().
