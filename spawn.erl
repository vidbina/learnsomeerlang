-module(spawn).
-compile(export_all).

announce(X) ->
  timer:sleep(5),
  io:format("~p ", [X]).

% try running spawner(100) to see how the jobs arent executed in order
spawner(Length) ->
  [spawn(fun() -> announce(X) end) || X <- lists:seq(1, 1+Length)].
