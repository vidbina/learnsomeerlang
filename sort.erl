-module(sort).
-export([quicksort/1, partition/2]).

quicksort([]) -> []; % base case
quicksort([Pivot|Rest]) -> 
  {Smaller, Larger} = partition(Pivot, Rest),
  quicksort(Smaller) ++ [Pivot|quicksort(Larger)].

partition(_, [], Smaller, Larger) -> { Smaller, Larger }; % base case
partition(Pivot, [Compare|Rest], Smaller, Larger) ->
  if Pivot>Compare ->
       partition(Pivot, Rest, [Compare|Smaller], Larger);
     Pivot=<Compare ->
       partition(Pivot, Rest, Smaller, [Compare|Larger])
  end.
partition(Pivot, Rest) -> partition(Pivot, Rest, [], []).
