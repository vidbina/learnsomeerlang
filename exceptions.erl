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
