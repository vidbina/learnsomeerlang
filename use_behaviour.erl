-module(use_behaviour).
-compile(export_all).
-behaviour(my_behaviour).

fiddler() -> my_behaviour:music(use_behaviour).
init(Input) -> Input.
other(A, B, C) -> {A, B, C}.
