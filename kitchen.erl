-module(kitchen).
-compile(export_all).

fridge1() ->
  receive
    { From, { store, _Food } } ->
      From ! { self(), ok },
      fridge1();
    { From, { take, _Food } } ->
      From ! { self(), ok },
      fridge1();
    terminate -> ok
  end.

fridge() -> fridge([]).
fridge(Content) ->
  receive
    { From, { store, Food } } ->
      From ! { self(), ok },
      fridge([Food|Content]);
    { From, { take, Food } } ->
      case lists:member(Food, Content) of
        true -> From ! { self(), ok }, fridge(lists:delete(Food, Content));
        false -> From ! { self(), out }, fridge(Content)
      end;
    { _, { view } } ->
      io:format("~p~n", [Content]),
      fridge(Content);
    terminate -> ok
  end.
