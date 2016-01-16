-module(basic).
-behaviour(gen_server).
-export([init/1, code_change/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([start/0, ask/2, say/2, stop/1]).

%% behaviour implemenation
init(Args) ->
  io:format("beginning with ~p~n", [Args]),
  {ok, run}.
code_change(OldVsn, State, Extra) ->
  io:format("code update from ~p in ~p with ~p~n", [OldVsn, State, Extra]),
  {ok, State}.
handle_call(Request, From, State) ->
  io:format("~p asked ~p in ~p~n", [From, Request, State]),
  {reply, 42, State}.
handle_cast(Request, State) ->
  io:format("~p cast in ~p~n", [Request, State]),
  case Request of
    die -> {stop, dealing_with_asshole};
    _ ->{noreply, State}
  end.
handle_info(Info, State) ->
  io:format("info: ~p~n", [Info]),
  {noreply, State}.
terminate(Reason, State) ->
  io:format("exit because ~p in ~p~n", [Reason, State]),
  exit.

%% helpers
start() ->
  gen_server:start_link(?MODULE, [something], [{timeout, 1000}]).
stop(Pid) ->
  gen_server:stop(Pid).
ask(Pid, Question) ->
  gen_server:call(Pid, Question).
say(Pid, Statement) ->
  gen_server:cast(Pid, Statement).
