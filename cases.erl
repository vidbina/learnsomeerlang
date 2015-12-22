-module(cases).
-export([insert/2, is_bigfoot/2, vehicle_by_cases/3, vehicle_by_cases_given_set/1, vehicle/2, fand/2]).

insert(X,[]) ->
  [X];
insert(X,Set) ->
  case lists:member(X,Set) of
    true -> Set;
    false -> [X|Set]
  end.

is_bigfoot(Region, Size) ->
  case { Region, Size } of
    { us, N } when N >= 16 -> true;
    { uk, N } when N >= 15.5 -> true;
    { eu, N } when N >= 49 -> true;
    { _, _ } -> false
  end.

vehicle_by_cases(Wheels, Doors, Engine) ->
  case { Wheels, Doors, Engine } of
    { 1, _, _ } -> unicycle;
    { 2, false, false } -> bike;
    { 2, false, true } -> moped;
    { 3, false, _ } -> tricicle;
    { 4, false, _ } -> cart;
    { 4, _, _ } -> automobile;
    { _, _, _ } -> vehicle
  end.

vehicle_by_cases_given_set({Wheels, Engine}) ->
  case { Wheels, Engine } of
    { 1, _ } -> unicycle;
    { 2, none } -> bike;
    { 2, _ } -> moped;
    { 3, _ } -> tricicle;
    { 4, none } -> cart;
    { 4, combustion } -> automobile;
    { _, electric } -> ev;
    { _, _ } -> vehicle
  end.

% most of what was done above could be done with simple function clauses, even
% if there were guard clauses involved... because function clauses can include
% guard clauses too.
vehicle(1, _) -> unicycle;
vehicle(2, none) -> bike;
vehicle(2, _) -> moped;
vehicle(3, _) -> tripod;
vehicle(4, none) -> cart;
vehicle(_, electric) -> ev;
vehicle(_, _) -> vehicle.

% Ha, after playing around the chapter http://learnyousomeerlang.com/syntax-in-functions#which-to-use
% discusses what I was asking myself when to use `case of` and when to use
% function heads... not function clauses... function heads. I keep getting it
% wrong.

% This should throw an error if both of the two arguments aren't `true` or
% `false`
fand(false, _) -> false;
fand(_, false) -> false;
fand(true, true) -> true.
