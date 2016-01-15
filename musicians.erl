-module(musicians).
-behaviour(gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
  gen_server:call(Role, stop).

init([Role, Skill]) ->
  process_flag(trap_exit, true),
  random:seed(now()),
  TimeToPlay = random:uniform(3000),
  Name = pick_name(),
  StrRole = atom_to_list(Role),
  io:format("Musician ~s, playing the ~s entered the room~n", [Name, StrRole]),
  {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

handle_call(stop, _From, S=#state{}) ->
  {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
  {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
  {noreply, S, ?DELAY}.

handle_info(timeout, S=#state{name=N, skill=good}) ->
  io:format("~s produced sound~n", [N]),
  {noreply, S, ?DELAY};
handle_info(timeout, S=#state{name=N, skill=bad}) ->
  case random:uniform(5) of
    1 ->
      io:format("~s played a false note. Uh oh~n", [N]),
      {stop, bad_note, S};
    _ ->
      io:format("~s produced sound~n", [N]),
      {noreply, S, ?DELAY}
  end;
handle_info(_Message, S) ->
  {noreply, S, ?DELAY}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, S) ->
  io:format("~s ~s left the room", [S#state.role, S#state.name]);
terminate(bad_note, S) ->
  io:format("~s ~s sucks and was kicked out", [S#state.role, S#state.name]);
terminate(shutdown, _) ->
  io:format("manager fired the whole band");
terminate(_Reason, S) ->
  io:format("~s ~s has been kicked out", [S#state.role, S#state.name]).

% 
pick_name() ->
  lists:nth(random:uniform(10), firstnames())
  ++ " " ++
  lists:nth(random:uniform(10), lastnames()).

firstnames() ->
  ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
   "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

lastnames() ->
  ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
   "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].
