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
rpn("ln", [Operand|Rest]) -> [math:log(Operand)|Rest];
rpn("log10", [Operand|Rest]) -> [math:log10(Operand)|Rest];
rpn("sqrt", [Operand|Rest]) -> [math:sqrt(Operand)|Rest];
rpn("^", [Exponent|[Base|Rest]]) -> [math:pow(Base, Exponent)|Rest];
rpn("e", Rest) -> [math:exp(1)|Rest];
rpn("pi", Rest) -> [math:pi()|Rest];
rpn("sum", L) -> [lists:sum(L)];
rpn("prod", L) -> [lists:foldl(fun (X, Acc) -> Acc*X end, 1, L)];
rpn(Operand, Acc) -> [read(Operand)|Acc].

rpn_test() ->
  5 = rpn("2 3 +"),
  87 = rpn("90 3 -"),
  -4 = rpn("10 4 3 + 2 * -"),
  -2.0 = rpn("10 4 3 + 2 * - 2 /"),
  fails = try
           rpn("90 34 12 33 55 66 + * - +")
         catch
           error:{badmatch,[_|_]} -> fails
          end,
  4037 = rpn("90 34 12 33 55 66 + * - + -"),
  8.0 = rpn("2 3 ^"),
  true = math:sqrt(2) == rpn("2 0.5 ^"),
  true = math:sqrt(10) == rpn("10 sqrt"),
  true = math:log(2.7) == rpn("2.7 ln"),
  true = math:log10(2.7) == rpn("2.7 log10"),
  50 = rpn("10 10 10 20 sum"),
  10.0 = rpn("10 10 10 20 sum 5 /"),
  1000.0 = rpn("10 10 20 0.5 prod"),
  ok.

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
