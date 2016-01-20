-module(ppool_nagger).
-behaviour(gen_server).
-export([start_link/4, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([cause_timeout/0]).

start_link(Task, Delay, Max, SendTo) ->
  gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

init({Task, Delay, Max, SendTo}) ->
  {ok, {Task, Delay, Max, SendTo}, Delay}.

% OTP Callbacks
handle_call(stop, _From, State) ->
  io:format("stop to call ~p~n", [State]),
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  io:format("noreply on call ~p~n", [State]),
  {noreply, State}.

handle_cast(_Msg, State) ->
  io:format("noreply on cast ~p~n", [State]),
  {noreply, State}.

% confirm that sending a message with a timeout hits this
handle_info(timeout, {Task, Delay, Max, SendTo}) ->
  io:format("info timeout for ~p~n", [Task]),
  SendTo ! {self(), Task},
  if Max =:= infinity ->
       {noreply, {Task, Delay, Max, SendTo}, Delay};
     Max =< 1 ->
       {stop, normal, {Task, Delay, 0, SendTo}};
     Max > 1 ->
       {noreply, {Task, Delay, Max-1, SendTo}, Delay}
  end;
handle_info(timeout, State) ->
  io:format("state is ~p~n", [State]),
  {noreply, State};
handle_info(Info, State) ->
  io:format("info ~p with state ~p~n", [Info, State]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  exit.

cause_timeout() ->
  {ok, Pid} = gen_server:start_link(?MODULE, {nag, 1000, 10, self()}, []),
  gen_server:call(Pid, {test, 1000, 10, self()}, 5000).
