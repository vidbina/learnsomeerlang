-module(exceptions).
-compile(export_all).

% works on
%   exceptions:throws(fun() -> erlang:throw(blah) end).
% but not on
%   exceptions:throws(fun() -> erlang:error(blah) end).
throws(F) ->
  try F() of
    _ -> ok
  catch
    Throw -> { throw, caught, Throw }
  end.

% catcher(12, 2)
% catcher(12, 0)
catcher(X, Y) ->
  case catch X/Y of
    { 'EXIT', { badarith, _ }} -> 'your math game sucks';
    N -> N
  end.

handle(F) ->
  try F() of
    _ -> ok
  catch
    error:badarith -> { error, caught, 'suck at math' };
    error:Reason -> { error, caught, Reason };
    throw:Reason -> { throw, caught, Reason };
    exit:Reason -> { exit, caught, Reason }
  end.

