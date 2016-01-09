-module(kitchen).
%-compile(export_all).
-export([start/1, store/2, take/2, view/1, fridge/1]).

%fridge() -> fridge([]).
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

start(Contents) -> spawn(?MODULE, fridge, [Contents]).
store(Pid, Item) -> Pid ! { self(), { store, Item } }.
take(Pid, Item) -> Pid ! { self(), { take, Item } }.
view(Pid) -> Pid ! { self(), { view } }.
