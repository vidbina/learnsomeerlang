-module(hotload).
-export([server/1, upgrade/1]).

server(State) ->
  receive
    update ->
      io:format("new stuff~n"),
      ?MODULE:server(?MODULE:upgrade(State));
    _ ->
      io:format("same old~n"),
      server(State)
  end.

upgrade(State) ->
  % change state such e.g.: convert dict to orddict because we needed better
  % performance for some behaviour.
  State.
