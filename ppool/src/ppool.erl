-module(ppool).
-behaviour(application).
-export([start/2, stop/1]).
-export([start_pool/3, run/2, sync_queue/2, async_queue/2, stop_pool/1]).
-export([test/1]).

start(normal, _Args) ->
  ppool_supersup:start_link().

stop(_State) ->
  ok.

start_pool(Name, Limit, {M,F,A}) ->
  ppool_supersup:start_pool(Name, Limit, {M,F,A}).

stop_pool(Name) ->
  ppool_supersup:stop_pool(Name).

run(Name, Args) ->
  ppool_serv:run(Name, Args).

async_queue(Name, Args) ->
  ppool_serv:async_queue(Name, Args).

sync_queue(Name, Args) ->
  ppool_serv:sync_queue(Name, Args).

test(setup) ->
  ppool:start_link(),
  ppool:start_pool(nagger, 2, {ppool_nagger, start_link, []});
test(sync_jobs) ->
  ppool:run(nagger, ["watch the wire!", 5000, 4, self()]),
  ppool:run(nagger, ["make some music", 10000, 2, self()]),
  ppool:run(nagger, ["clean up", 3000, 5, self()]),
  receive
  after 10000 ->
          io:format("after~n"),
          ppool:run(nagger, ["do something", 3000, 5, self()]),
          ppool:run(nagger, ["learn erl", 2000, 2, self()])
  end;
test(async_jobs) ->
  ppool:async_queue(nagger, ["pay bills", 10000, 1, self()]),
  ppool:async_queue(nagger, ["shower", 7500, 1, self()]),
  ppool:async_queue(nagger, ["plant a tree", 11000, 1, self()]);
test(a) ->
  io:format("PID before: ~p~n", [self()]),
  test(setup),
  test(sync_jobs),
  test(async_jobs),
  receive
  after 25000 ->
          io:format("termin8"),
%          try ppool:stop()
%          catch
%            Something -> io:format("Catch ~p~n", [Something])
%          end
          ok
  end,
  io:format("PID after: ~p~n", [self()]),
  passed;
test(b) ->
  test(setup),
  test(async_jobs),
  test(sync_jobs),
  receive
  after 25000 ->
          io:format("terminate"),
          ppool:stop()
  end,
  passed;
test(run) ->
  try test(a)
  catch
    Anything -> io:format("err: ~p~n", [Anything])
  end,
  io:format("done~n").
