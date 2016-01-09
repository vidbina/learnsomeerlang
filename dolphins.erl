-module(dolphins).
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip -> flipping;
    fish -> thanks;
    _ -> sonar
  end.
