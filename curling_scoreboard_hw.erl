-module(curling_scoreboard_hw).
-export([add_point/1, next_round/0, set_teams/2, reset_board/0]).

set_teams(TeamA, TeamB) ->
  io:format("~s vs ~s~n", [TeamA, TeamB]).

next_round() ->
  io:format("next round~n").

add_point(Team) ->
  io:format("Score for ~s~n", [Team]).

reset_board() ->
  io:format("reset~n").
