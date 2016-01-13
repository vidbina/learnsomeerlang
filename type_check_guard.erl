-module(type_check_guard).
-compile(export_all).

do(Number, String, Atom, Tuple, List) when is_number(Number) ->
  io:format(
    "Number: ~p~nString: ~p~nAtom: ~p~nTuple: ~p~nList: ~p~n",
    [Number, String, Atom, Tuple, List]),
  ok.

attempt(Number, String, Atom, Tuple, List) ->
  try do(Number, String, Atom, Tuple, List)
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
  % invalid strings
  fail = attempt(42, 41, galaxy, {a, b, c}, [1,2,3]),
  fail = attempt(42, hello, galaxy, {a, b, c}, [1,2,3]),
  fail = attempt(42, {"hi"}, galaxy, {a, b, c}, [1,2,3]),
  fail = attempt(42, [104, 105, there], galaxy, {a, b, c}, [1,2,3]),
  fail = attempt(42, [104, 105, hi], galaxy, {a, b, c}, [1,2,3]),
  % invalid atom
  fail = attempt(42, "hello", 1, {a, b, c}, [1,2,3]),
  fail = attempt(42, "hello", "galaxy", {a, b, c}, [1,2,3]),
  fail = attempt(42, "hello", {}, {a, b, c}, [1,2,3]),
  % invalid tuple
  ok = attempt(42, "hello", galaxy, [a, b, c], [1,2,3]),
  ok = attempt(42, "hello", galaxy, abc, [1,2,3]),
  ok = attempt(42, "hello", galaxy, 123, [1,2,3]),
  % invalid list
  ok = attempt(42, "hello", galaxy, {a, b, c}, {1,2,3}),
  ok = attempt(42, "hello", galaxy, {a, b, c}, 123),
  ok = attempt(42, "hello", galaxy, {a, b, c}, abc),
  passed.
