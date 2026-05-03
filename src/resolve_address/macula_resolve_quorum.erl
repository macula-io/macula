%%%-------------------------------------------------------------------
%%% @doc Eclipse-mitigation wrapper for {@link macula_resolve_address}.
%%%
%%% Phase 4.4 — see PLAN_MACULA_NET_PHASE4_4_RESOLVE_QUORUM.md.
%%%
%%% Wraps a list of `find_fn' callbacks (each backed by a different
%%% DHT endpoint) into a single `find_fn' that fans out in parallel,
%%% requires M of N to return byte-equal records, and reports
%%% disagreement as `{error, eclipse_suspected}'.
%%%
%%% Signature-based comparison: two records "agree" iff their
%%% Ed25519 signatures are byte-equal — the strongest possible check
%%% short of full payload equality, and equivalent in practice
%%% because the signing operation is deterministic over the
%%% canonical CBOR encoding of the payload.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_resolve_quorum).

-export([wrap/2]).

-export_type([opts/0]).

-type find_fn() :: macula_resolve_address:find_fn().

-type opts() :: #{
    threshold  => pos_integer(),
    timeout_ms => pos_integer(),
    parallel   => boolean()
}.

-define(DEFAULT_TIMEOUT_MS, 1000).

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Returns a `find_fn' that queries every Fn in `FindFns' (in
%% parallel by default) and applies a quorum check.
-spec wrap([find_fn()], opts()) -> find_fn().
wrap([], _Opts) ->
    error(no_find_fns);
wrap([SingleFn], _Opts) when is_function(SingleFn, 1) ->
    %% Degenerate single-endpoint case — passthrough with telemetry.
    fun(Key) ->
        Result = SingleFn(Key),
        telemetry:execute([macula, net, resolve_quorum, decided],
                          #{count => 1},
                          #{outcome => <<"single_endpoint">>}),
        Result
    end;
wrap(FindFns, Opts) when is_list(FindFns) ->
    N = length(FindFns),
    Threshold = maps:get(threshold, Opts, default_threshold(N)),
    Timeout   = maps:get(timeout_ms, Opts, ?DEFAULT_TIMEOUT_MS),
    Parallel  = maps:get(parallel,   Opts, true),
    fun(Key) ->
        Responses = collect(FindFns, Key, Parallel, Timeout),
        decide(Responses, Threshold)
    end.

default_threshold(N) ->
    %% Strict majority: ceil((N + 1) / 2).
    (N + 1) div 2 + ((N + 1) rem 2).

%% =============================================================================
%% Fanout
%% =============================================================================

collect(FindFns, Key, true, Timeout) ->
    Self = self(),
    Refs = [erlang:spawn_monitor(fun() ->
                Self ! {self(), F(Key)}
            end) || F <- FindFns],
    gather(Refs, Timeout, []);
collect(FindFns, Key, false, _Timeout) ->
    [F(Key) || F <- FindFns].

gather([], _Timeout, Acc) ->
    Acc;
gather([{Pid, MonRef} | Rest], Timeout, Acc) ->
    receive
        {Pid, Result} ->
            erlang:demonitor(MonRef, [flush]),
            gather(Rest, Timeout, [Result | Acc]);
        {'DOWN', MonRef, process, Pid, Reason} ->
            gather(Rest, Timeout, [{error, {worker_died, Reason}} | Acc])
    after Timeout ->
        %% Per-worker timeout: kill the worker and tally as error.
        exit(Pid, kill),
        erlang:demonitor(MonRef, [flush]),
        gather(Rest, Timeout, [{error, timeout} | Acc])
    end.

%% =============================================================================
%% Decide
%% =============================================================================

decide(Responses, Threshold) ->
    Records = [R || {ok, R} <- Responses],
    case Records of
        [] -> emit_and_return(<<"insufficient_responses">>);
        _  -> tally_records(Records, Threshold)
    end.

tally_records(Records, Threshold) ->
    Tally = lists:foldl(
        fun(Rec, Acc) ->
            Sig = macula_record:signature(Rec),
            maps:update_with(Sig, fun(V) -> V + 1 end, 1, Acc)
        end, #{}, Records),
    pick_winner(Tally, Records, Threshold).

pick_winner(Tally, Records, Threshold) ->
    {WinnerSig, Count} =
        lists:foldl(fun({Sig, C}, {_BestSig, BestC} = Best) ->
                            decide_better({Sig, C}, BestC, Best)
                    end,
                    {undefined, 0},
                    maps:to_list(Tally)),
    pick_with_threshold(Count >= Threshold, WinnerSig, Records).

decide_better({Sig, C}, BestC, _Best) when C > BestC -> {Sig, C};
decide_better(_, _BestC, Best)                         -> Best.

pick_with_threshold(true, WinnerSig, Records) ->
    %% Find any record with the winning signature — they're all equal.
    [Winner | _] = [R || R <- Records,
                         macula_record:signature(R) =:= WinnerSig],
    telemetry:execute([macula, net, resolve_quorum, decided],
                      #{count => 1},
                      #{outcome => <<"consistent">>}),
    {ok, Winner};
pick_with_threshold(false, _WinnerSig, _Records) ->
    emit_and_return(<<"disagreement">>).

emit_and_return(Outcome) ->
    telemetry:execute([macula, net, resolve_quorum, decided],
                      #{count => 1},
                      #{outcome => Outcome}),
    error_for(Outcome).

error_for(<<"insufficient_responses">>) -> {error, insufficient_responses};
error_for(<<"disagreement">>)            -> {error, eclipse_suspected};
error_for(_)                             -> {error, unknown}.
