-module(basic).
-behaviour(gen_server).
-export([init/1, code_change/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([start/1, ask/2, say/2, stop/1]).

%% behaviour implemenation
init(Args=[StartupTime|_]) ->
  process_flag(trap_exit, true),
  io:format("beginning with ~p~n", Args),
  receive
  after StartupTime ->
          io:format("started successfully~n")
  end,
  {ok, run}.
code_change(OldVsn, State, Extra) ->
  io:format("code update from ~p in ~p with ~p~n", [OldVsn, State, Extra]),
  {ok, State}.
handle_call(Request, From, State) ->
  io:format("~p asked ~p ~p in ~p~n", [From, self(), Request, State]),
  {reply, 42, State}.
handle_cast(Request, State) ->
  io:format("~p cast in ~p ~p~n", [Request, self(), State]),
  case Request of
    die -> {stop, dealing_with_asshole};
    _ ->{noreply, State}
  end.
handle_info(Info, State) ->
  io:format("info: ~p~n", [Info]),
  {noreply, State}.
terminate(Reason, State) ->
  io:format("terminate because ~p in ~p~n", [Reason, State]),
  exit.

%% helpers
start(StartupTime) ->
  gen_server:start_link(?MODULE, [StartupTime], [{timeout, 1000}]).
stop(Pid) ->
  gen_server:stop(Pid).
ask(Pid, Question) ->
  gen_server:call(Pid, Question).
say(Pid, Statement) ->
  gen_server:cast(Pid, Statement).
