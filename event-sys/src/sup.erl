-module(sup).
-export([start/2, start_link/2, init/1, loop/1]).

start(Mod, Args) ->
  io:format("starting some shit~n"),
  spawn(?MODULE, init, [{Mod, Args}]).
start_link(Mod, Args) ->
  spawn_link(?MODULE, init, [{Mod, Args}]).

init({Mod, Args}) ->
  io:format("trapping evserv:start_link~n"),
  process_flag(trap_exit, true),
  loop({Mod,start_link,Args}).

loop({M,F,A}) ->
  Pid = apply(M,F,A),
  io:format("Pid for ~p:~p is ~p~n", [M,F,Pid]),
  receive
    { 'EXIT', _From, shutdown } ->
      io:format("SHUTDOWN happening~n"),
      exit(shutdown);
    { 'EXIT', Pid, Reason } ->
      io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
      loop({M,F,A});
    Unknown -> io:format("just got ~p~n", [Unknown])
  end.
