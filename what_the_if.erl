-module(what_the_if).
-export([heh_fine/0, bloo/0, evenness/1, help_me/1]).

heh_fine() ->
  if 1 =:= 1 ->
       works
  end,
  if 1 =:= 2; 1 =:= 1 ->
       works
  end,
  if 1 =:= 2, 1 =:= 1 -> fails; true -> something
  end.

% Erlang bitches whenever a guard definitely evaluates to false even if there
% is a true branch, since it is already known that the true branch is
% unreachable. Probably should have called this test function
% `unreachable_truth` :P.
problem_child() ->
  if false -> problem; true -> no_problem end.

evenness(N) ->
  if (N rem 2) =:= 0 ->
       even;
     (N rem 2) =:= 1 ->
       uneven
  end.

bloo() ->
  bloo,
  blah.

help_me(Animal) ->
  Talk = if Animal == cat -> "meow";
            Animal == beef -> "mooo";
            Animal == dog -> "bark";
            Animal == tree -> "bark";
            true -> "bliepbliepbloop"
         end,
  { Animal, "says " ++ Talk ++ "!" }.
