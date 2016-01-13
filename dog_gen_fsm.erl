-module(dog_gen_fsm).
-behaviour(gen_fsm).
-compile(export_all).

start() ->
  gen_fsm:start(?MODULE, [], []).

pet(Pid) ->
  gen_fsm:send_event(Pid, pet).
squirrels(Pid) ->
  gen_fsm:send_event(Pid, squirrels).
tease(Pid) ->
  gen_fsm:send_all_state_event(Pid, food_smell).
feed(Pid) ->
  gen_fsm:send_event(Pid, feed).

init(StartState) ->
  io:format("start sitting~n"),
  {ok, sitting, StartState}.

handle_event(food_smell, _AnyState, History) ->
  io:format("I smell food, where is it?~n"),
  {next_state, searching, [searching|History]}.

searching(feed, History) ->
  io:format("Yummy! Thanks this is delicious!~n"),
  {next_state, sitting, History};
searching(_, History) ->
  io:format("Leave me alone, I'm looking for food~n"),
  {next_state, searching, History}.

sitting(squirrels, History) ->
  io:format("I saw squirrels~n"),
  {next_state, barking, [sitting|History], 2000};
sitting(_, History) ->
  io:format("still sitting~n"),
  {next_state, sitting, History}.
  

barking(pet, History) ->
  io:format("Woof, hey... that feels nice.~n"),
  {next_state, wagging, [barking|History], 10000};
barking(_, History) ->
  io:format("I'll just keep on barking until you pet me~n"),
  {next_state, barking, History, 2000};
barking(timeout, History) ->
  io:format("Woof woof! Still barking.~n"),
  {next_state, barking, History, 2000}.

wagging(pet, History) ->
  io:format("Getting pet while happy makes me want to sit~n"),
  {next_state, sitting, [wagging|History]};
wagging(timeout, History) ->
  io:format("That was fun, now back to business~n"),
  {next_state, barking, [wagging|History], 2000}.
