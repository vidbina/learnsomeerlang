-module(guards).
-compile([export_all]).

old_enough(X) when X =< 16 -> false;
old_enough(_) -> true.
