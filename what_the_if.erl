-module(what_the_if).
-export([heh_fine/0, bloo/0, evenness/1]).

heh_fine() ->
  if 1 =:= 1 ->
       works
  end,
  if 1 =:= 2; 1 =:= 1 ->
       works
  end,
  if 1 =:= 2, 1 =:= 1 -> fails; true -> something
  end.

evenness(N) ->
  if (N rem 2) =:= 0 -> even; true -> uneven end.

bloo() ->
  bloo,
  blah.
