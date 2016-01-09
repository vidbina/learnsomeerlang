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
        false -> From ! { self(), not_found }, fridge(Content)
      end;
    { From, { view } } ->
      From ! { content, Content },
      fridge(Content);
    terminate -> ok
  end.

start(Contents) ->
  spawn(?MODULE, fridge, [Contents]).
store(Pid, Item) ->
  Pid ! { self(), { store, Item } },
  receive
    { _, Msg } -> Msg
  end.
take(Pid, Item) ->
  Pid ! { self(), { take, Item } },
  receive
    { _, Msg } -> Msg
  end.
view(Pid) ->
  Pid ! { self(), { view } },
  receive
    { content, List } -> List
  end.
