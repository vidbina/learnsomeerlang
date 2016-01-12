-module(use_behaviour).
-compile(export_all).
-behaviour(my_behaviour).

init(Input) -> Input.
some_fun() -> ok.
other(A, B, C) -> {A, B, C}.
