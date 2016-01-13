-module(type_check_guard).
-compile(export_all).

do(Number, Atom, Tuple, List)
  when
    is_number(Number),
    is_atom(Atom),
    is_tuple(Tuple),
    is_list(List) ->
%  io:format(
%    "Number: ~p~nAtom: ~p~nTuple: ~p~nList: ~p~n",
%    [Number, Atom, Tuple, List]),
  ok.

attempt(Number, _String, Atom, Tuple, List) ->
  try do(Number, Atom, Tuple, List)
  catch
    _:_ -> fail
  end.

test() ->
  ok = attempt(42, "hello", galaxy, {a, b, c}, [1,2,3]),
  % invalid number
  fail = attempt("hi", "hello", galaxy, {a, b, c}, [1,2,3]),
  fail = attempt(hi, "hello", galaxy, {a, b, c}, [1,2,3]),
  fail = attempt([], "hello", galaxy, {a, b, c}, [1,2,3]),
  % weird strings
  ok = attempt(42, [], galaxy, {a, b, c}, [1,2,3]),
  ok = attempt(42, "", galaxy, {a, b, c}, [1,2,3]),
  ok = attempt(42, [104, 105], galaxy, {a, b, c}, [1,2,3]),
% NOTE: Erlang has no strings, so we're just testing for lists later on  
%  % invalid strings
%  fail = attempt(42, 41, galaxy, {a, b, c}, [1,2,3]),
%  fail = attempt(42, hello, galaxy, {a, b, c}, [1,2,3]),
%  fail = attempt(42, {"hi"}, galaxy, {a, b, c}, [1,2,3]),
%  %fail = attempt(42, [104, 105, there], galaxy, {a, b, c}, [1,2,3]),
%  %fail = attempt(42, [104, 105, hi], galaxy, {a, b, c}, [1,2,3]),
  % invalid atom
  fail = attempt(42, "hello", 1, {a, b, c}, [1,2,3]),
  fail = attempt(42, "hello", "galaxy", {a, b, c}, [1,2,3]),
  fail = attempt(42, "hello", {}, {a, b, c}, [1,2,3]),
  % invalid tuple
  fail = attempt(42, "hello", galaxy, [a, b, c], [1,2,3]),
  fail = attempt(42, "hello", galaxy, abc, [1,2,3]),
  fail = attempt(42, "hello", galaxy, 123, [1,2,3]),
  % invalid list
  fail = attempt(42, "hello", galaxy, {a, b, c}, {1,2,3}),
  fail = attempt(42, "hello", galaxy, {a, b, c}, 123),
  fail = attempt(42, "hello", galaxy, {a, b, c}, abc),
  passed.
