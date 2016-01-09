-module(dolphins).
-compile(export_all).

dolphin1() ->
  receive
    do_a_flip -> io:format("nope~n");
    fish -> io:format("thank you, buddy~n");
    _ -> io:format("doing the sonar thing~n")
  end.

dolphin2() ->
  receive
    { Sender, flip } -> Sender ! "Do one yourself";
    { Sender, fish } -> Sender ! "Thank you, buddy";
    _ -> sonar 
  end.

% Dolphin = spawn(dolphins, dolphin, []).
% Dolphin ! flip.
% Dolphin ! blah.
% Dolphin ! flip.
% Dolphin ! fish.
% Dolphin ! flip.
% flush()
dolphin3() ->
  receive
    { Sender, flip } -> Sender ! "Do one yourself", dolphin3();
    { Sender, fish } -> Sender ! "Thank you, buddy", dolphin3();
    { Sender, choo } -> Sender ! "Whatever";
    _ -> sonar, dolphin3() 
  end.
