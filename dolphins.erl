-module(dolphins).
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip -> io:format("nope~n");
    fish -> io:format("thank you, buddy~n");
    _ -> io:format("doing the sonar thing~n")
  end.
