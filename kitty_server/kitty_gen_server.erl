-module(kitty_gen_server).
-behaviour(gen_server).
-record(cat, {name, color=green, description}).
-export(
   [start_link/0, order_cat/4, return_cat/2, close_shop/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]
  ).

%%% Client API
start_link() ->
  %spawn_link(fun init/0),
  gen_server:start_link(?MODULE, [], []).
 
%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
  %my_server:call(Pid, {order, Name, Color, Description}).
  gen_server:call(Pid, {order, Name, Color, Description}).
 
%% This call is asynchronous
return_cat(Pid, Cat = #cat{}) ->
  %my_server:cast(Pid, {return, Cat}).
  gen_server:cast(Pid, {return, Cat}).
 
%% Synchronous call
close_shop(Pid) ->
  %my_server:call(Pid, terminate).
  gen_server:call(Pid, terminate).

init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
  if Cats =:= [] ->
       {reply, make_cat(Name, Color, Description), Cats};
     Cats =/= [] ->
       {reply, hd(Cats), tl(Cats)}
  end;
handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat=#cat{}}, Cats) ->
  {noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
  io:format("Unexpected message: ~p~n", [Msg]),
  {noreply, Cats}.
 
terminate(normal, Cats) ->
  [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% Private functions
make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description=Desc}.
