%% The shared nesting calibration set. Each function is one code shape; Elvis
%% no_deep_nesting at level 2 gives the verdict every SDK's linter is tuned to
%% reproduce.
-module(nesting_calibration).

-export([
    flat/1,
    one_branch/1,
    branch_in_branch/2,
    three_deep/3,
    closure_in_body/1,
    closure_in_branch/1,
    branch_in_closure/1
]).

%% S1: no control structure.
flat(X) ->
    X + 1.

%% S2: one control structure.
one_branch(X) ->
    case X of
        0 -> zero;
        _ -> other
    end.

%% S3: a control structure inside one.
branch_in_branch(X, Y) ->
    case X of
        0 ->
            case Y of
                0 -> both;
                _ -> first_only
            end;
        _ -> neither
    end.

%% S4: three control structures deep.
three_deep(X, Y, Z) ->
    case X of
        0 ->
            case Y of
                0 ->
                    case Z of
                        0 -> all;
                        _ -> two
                    end;
                _ -> one
            end;
        _ -> none
    end.

%% S5: a closure in the function body, no control structure around it.
closure_in_body(Xs) ->
    lists:map(fun(X) -> X + 1 end, Xs).

%% S6: a closure inside a branch.
closure_in_branch(Xs) ->
    case Xs of
        [] -> [];
        _ -> lists:map(fun(X) -> X + 1 end, Xs)
    end.

%% S7: a control structure inside a closure.
branch_in_closure(Xs) ->
    lists:map(
        fun(X) ->
            case X of
                0 -> zero;
                _ -> other
            end
        end,
        Xs
    ).
