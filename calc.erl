-module(calc).
-compile(export_all).

calc(Expr) -> calc(string:tokens(Expr, " "), []).

% calc(["1", "3", "+", "2", "/"], [])
% calc(["3", "+", "2", "/"], [1])
% calc(["+", "2", "/"], [3, 1])
% calc(["2", "/"], [4])
% calc(["/"], [2, 4])
% calc([], [2])
calc([], [Val|_]) -> Val;
calc(["+"|Tokens], [L|[R|Stack]]) ->
  calc(Tokens, [L+R|Stack]);
calc(["-"|Tokens], [Subtrahend|[Minuend|Stack]]) ->
  calc(Tokens, [Minuend-Subtrahend|Stack]);
calc(["*"|Tokens], [Multiplier|[Multiplicant|Stack]]) ->
  calc(Tokens, [Multiplicant*Multiplier|Stack]);
calc(["/"|Tokens], [Divisor|[Dividend|Stack]]) ->
  calc(Tokens, [Dividend/Divisor|Stack]);
calc([Val|Tokens], Stack) ->
  calc(Tokens, [read(Val)|Stack]).

% fold(fun rpn/2, [], ["1", "3", "+", "2", "/"])
% fold(rpn("1", []) -> [1], ["3", "+", "2", "/"])
% fold(rpn("3", [1]) -> [3, 1], ["+", "2", "/"])
% fold(rpn("+", [3, 1]) -> [4], ["2", "/"])
% fold(rpn("2", [4]) -> [2, 4], ["/"])
% fold(rpn("/", [2, 4]) -> [2], [])
rpn(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")), Res.
rpn("+", [A|[B|Rest]]) -> [A+B|Rest];
rpn("-", [Subtrahend|[Minuend|Rest]]) -> [Minuend-Subtrahend|Rest];
rpn("*", [Multiplier|[Multiplicant|Rest]]) -> [Multiplicant*Multiplier|Rest];
rpn("/", [Divisor|[Dividend|Rest]]) -> [Dividend/Divisor|Rest];
rpn(Operand, Acc) -> [read(Operand)|Acc].

list_to_number(N) ->
  case string:to_float(N) of
    { error, no_float } -> { Int, _ } = string:to_integer(N), Int;
    { Float, _ } -> Float
  end.

read(N) ->
  case string:to_float(N) of
    { error, no_float } -> list_to_integer(N);
    { F, _ } -> F
  end.
