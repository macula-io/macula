%% EUnit tests for macula_diagnostics.
-module(macula_diagnostics_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Events
%%------------------------------------------------------------------

event_returns_ok_test() ->
    ?assertEqual(ok, macula_diagnostics:event(<<"_macula.test.event">>, #{a => 1})).

event_with_explicit_level_test() ->
    ?assertEqual(ok, macula_diagnostics:event(debug, <<"_macula.test.dbg">>, #{})).

event_rejects_non_binary_topic_test() ->
    ?assertError(_, macula_diagnostics:event(not_a_binary, #{})).

event_rejects_non_map_properties_test() ->
    ?assertError(_, macula_diagnostics:event(<<"x">>, "not a map")).

%%------------------------------------------------------------------
%% Domain filter
%%------------------------------------------------------------------

install_domain_filter_is_idempotent_test() ->
    ?assertEqual(ok, macula_diagnostics:install_domain_filter()),
    ?assertEqual(ok, macula_diagnostics:install_domain_filter()),
    %% Cleanup so other tests don't see the filter.
    _ = logger:remove_handler_filter(default, macula_domain),
    ok.

%% Reproduces the actual incident: OTP's stock `default' handler
%% ships `filter_default => stop' with only `[otp,sasl]' (or a
%% sub-domain) and no-domain events explicitly allowed — a `[macula]'
%% event is dropped before it ever reaches the handler's output.
%% `install_domain_filter/0' must make it reach the handler; without
%% it, this test's first assertion (the RED case) fails, matching the
%% live fleet symptom.
domain_filter_fixes_the_actual_drop_test() ->
    {ok, OrigPrimary} = {ok, logger:get_primary_config()},
    {ok, OrigDefault} = logger:get_handler_config(default),
    File = filename:join(
             "/tmp", "macula_diagnostics_domain_filter_test.log"),
    file:delete(File),
    try
        %% Match production's kernel `logger_level => info' — OTP's
        %% own stock default is `notice', which would drop `info'
        %% events at the primary filter regardless of the domain bug.
        ok = logger:set_primary_config(level, info),
        ok = logger:remove_handler(default),
        ok = logger:add_handler(default, logger_std_h, #{
            config => #{type => {file, File}},
            filter_default => stop,
            filters => [
                {remote_gl, {fun logger_filters:remote_gl/2, stop}},
                {domain, {fun logger_filters:domain/2,
                          {log, super, [otp, sasl]}}},
                {no_domain, {fun logger_filters:domain/2,
                             {log, undefined, []}}}
            ]
        }),

        ok = macula_diagnostics:event(<<"_macula.test.before_fix">>, #{}),
        ok = logger_std_h:filesync(default),
        ?assertEqual({ok, <<>>}, file:read_file(File)),

        ok = macula_diagnostics:install_domain_filter(),
        ok = macula_diagnostics:event(<<"_macula.test.after_fix">>, #{}),
        ok = logger_std_h:filesync(default),
        {ok, Contents} = file:read_file(File),
        ?assert(byte_size(Contents) > 0),
        ?assert(binary:match(Contents, <<"_macula.test.after_fix">>) =/= nomatch)
    after
        _ = logger:remove_handler(default),
        _ = logger:add_handler(default, maps:get(module, OrigDefault),
                                maps:without([id, module], OrigDefault)),
        _ = logger:set_primary_config(OrigPrimary),
        _ = file:delete(File)
    end.

%%------------------------------------------------------------------
%% Metrics
%%------------------------------------------------------------------

counter_starts_at_zero_test() ->
    fresh(),
    ok = macula_diagnostics:metric(<<"_macula.calls">>, counter, 1),
    ?assertEqual([{<<"_macula.calls">>, counter, 1}],
                 macula_diagnostics:snapshot()).

counter_accumulates_test() ->
    fresh(),
    ok = macula_diagnostics:metric(<<"_macula.calls">>, counter, 1),
    ok = macula_diagnostics:metric(<<"_macula.calls">>, counter, 5),
    ok = macula_diagnostics:metric(<<"_macula.calls">>, counter, 4),
    ?assertEqual([{<<"_macula.calls">>, counter, 10}],
                 macula_diagnostics:snapshot()).

counter_rejects_non_positive_test() ->
    ?assertError(_, macula_diagnostics:metric(<<"x">>, counter, 0)),
    ?assertError(_, macula_diagnostics:metric(<<"x">>, counter, -1)).

gauge_overwrites_previous_test() ->
    fresh(),
    ok = macula_diagnostics:metric(<<"_macula.q">>, gauge, 1),
    ok = macula_diagnostics:metric(<<"_macula.q">>, gauge, 7),
    ?assertEqual([{<<"_macula.q">>, gauge, 7}],
                 macula_diagnostics:snapshot()).

gauge_accepts_floats_test() ->
    fresh(),
    ok = macula_diagnostics:metric(<<"_macula.lat">>, gauge, 12.5),
    ?assertEqual([{<<"_macula.lat">>, gauge, 12.5}],
                 macula_diagnostics:snapshot()).

snapshot_returns_all_metrics_test() ->
    fresh(),
    ok = macula_diagnostics:metric(<<"_macula.a">>, counter, 1),
    ok = macula_diagnostics:metric(<<"_macula.b">>, gauge, 42),
    ok = macula_diagnostics:metric(<<"_hecate.c">>, counter, 3),
    Snap = lists:sort(macula_diagnostics:snapshot()),
    ?assertEqual(
        lists:sort([
            {<<"_macula.a">>, counter, 1},
            {<<"_macula.b">>, gauge, 42},
            {<<"_hecate.c">>, counter, 3}
        ]),
        Snap).

reset_clears_metrics_test() ->
    fresh(),
    ok = macula_diagnostics:metric(<<"_macula.a">>, counter, 1),
    ok = macula_diagnostics:metric(<<"_macula.b">>, gauge, 42),
    ok = macula_diagnostics:reset(),
    ?assertEqual([], macula_diagnostics:snapshot()).

reset_leaves_unrelated_dict_entries_alone_test() ->
    fresh(),
    erlang:put(other_key, foo),
    ok = macula_diagnostics:metric(<<"_macula.a">>, counter, 1),
    ok = macula_diagnostics:reset(),
    ?assertEqual(foo, erlang:get(other_key)).

snapshot_is_per_process_test() ->
    fresh(),
    Parent = self(),
    ok = macula_diagnostics:metric(<<"_macula.parent">>, counter, 1),
    spawn(fun() ->
        macula_diagnostics:metric(<<"_macula.child">>, counter, 5),
        Parent ! {child_snap, macula_diagnostics:snapshot()}
    end),
    receive
        {child_snap, ChildSnap} ->
            %% Child only sees its own metric.
            ?assertEqual([{<<"_macula.child">>, counter, 5}], ChildSnap),
            %% Parent only sees its own.
            ?assertEqual([{<<"_macula.parent">>, counter, 1}],
                         macula_diagnostics:snapshot())
    after 1000 ->
        ?assert(false)
    end.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

fresh() ->
    macula_diagnostics:reset().
