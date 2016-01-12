-module(dog_fsm).
-export([start/0, squirel/1, pet/1]).

start() ->
  spawn(fun() -> sit() end).

squirel(Pid) ->
  Pid ! squirel.

pet(Pid) ->
  Pid ! pet.

sit() ->
  io:format("sitting~n"),
  receive
    squirel -> bark();
    _ -> sit()
  end.

bark() ->
  io:format("woof~n"),
  receive
    pet -> wag();
    _ -> bark()
  after 2000 -> bark()
  end.

wag() ->
  io:format("wag~n"),
  receive
    pet -> sit();
    _ -> wag()
  after 10000 -> bark()
  end.
