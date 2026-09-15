%% EUnit tests for keeping private keys out of what a node shows and logs: macula_node_keys:redacted/1, format_status/1
%% in every process that holds a private key, and the primary logger filter macula_node_keys:redacted_log_event/2 that
%% the application's start and every pool install, and that nothing removes. Each crash path is captured through a
%% logger handler, and no private key's bytes may appear in an event or in its formatted text. The gen_statem path
%% runs on a gen_statem of this module that holds a key: every state function of macula_peering_conn has a clause for
%% any event, and its format_status/1 is tested directly.
-module(macula_key_redaction_tests).

-include_lib("eunit/include/eunit.hrl").

%% The logger handler, the gen_statem and supervisor callbacks, and a helper of a module outside macula.
-export([log/2, callback_mode/0, init/1, holding/3, host_helper/1]).

-define(FILTER, macula_key_redaction).
-define(MARKER, <<"a host argument that stays in the report">>).
-define(PEERING_EXIT, <<"_macula.station_link.peering_exit">>).
%% A pq_hybrid key carries an RSA-4096 half, which takes up to about a second to generate.
-define(EU_TIMEOUT, 120).

%%------------------------------------------------------------------
%% redacted/1
%%------------------------------------------------------------------

every_private_half_is_redacted_and_the_rest_kept_test_() ->
    [?_test(assert_key_redacted(Purpose, pq_pure)) || Purpose <- [identity, connect, tls]].

a_hybrid_key_loses_both_private_halves_test_() ->
    {timeout, ?EU_TIMEOUT, ?_test(assert_key_redacted(identity, pq_hybrid))}.

a_key_pair_loses_its_private_half_test() ->
    Pair = macula_identity:generate(),
    ?assertEqual(Pair#{private := redacted}, macula_node_keys:redacted(Pair)).

keys_in_tuples_lists_and_maps_are_redacted_at_any_depth_test() ->
    Key = key(),
    Term = {state, [#{link_opts => #{node_identity => Key}}], {Key, [first | Key]}},
    ?assertEqual([], exposed(macula_node_keys:redacted(Term), privates(Key))).

%% A node identity key travels to a pool or an issuer as a function that returns it, and a supervisor keeps that
%% function in the child's start arguments, so a function that captured a key shows only its printed form.
a_function_that_captured_a_key_shows_as_its_printed_form_test() ->
    Key = key(),
    Fun = fun() -> Key end,
    ?assertEqual({held, erlang:fun_to_list(Fun)}, macula_node_keys:redacted({held, Fun})),
    ?assertEqual([], exposed(macula_node_keys:redacted({held, Fun}), privates(Key))).

a_function_that_captured_nothing_is_left_as_it_is_test_() ->
    [?_assertEqual(Fun, macula_node_keys:redacted(Fun)) || Fun <- [fun erlang:self/0, fun() -> ok end]].

a_map_without_both_halves_is_left_as_it_is_test_() ->
    [?_assertEqual(Map, macula_node_keys:redacted(Map))
     || Map <- [#{private => <<"a flag">>}, #{public => <<"a key">>}, #{private => true, name => <<"x">>}, #{}]].

%%------------------------------------------------------------------
%% redacted_log_event/2
%%------------------------------------------------------------------

a_frame_of_a_listed_module_shows_its_arity_only_test() ->
    Location = [{file, "macula_peering_conn.erl"}, {line, 1}],
    Frame = {macula_peering_conn, connected, [info, crypto:strong_rand_bytes(64), data], Location},
    ?assertEqual(#{reason => {function_clause, [{macula_peering_conn, connected, 3, Location}]}},
                 filtered(#{reason => {function_clause, [Frame]}}, [otp], #{macula_peering_conn => true})).

a_frame_of_another_module_keeps_its_arguments_test() ->
    Report = #{reason => {function_clause, [{host_module, handle_call, [?MARKER, from, state], [{line, 1}]}]}},
    ?assertEqual(Report, filtered(Report, [otp], #{macula_peering_conn => true})).

keys_are_redacted_in_reports_of_the_otp_and_macula_domains_test_() ->
    Key = key(),
    [?_assertEqual(#{held => redacted_key(Key)}, filtered(#{held => Key}, [Domain, sasl], #{}))
     || Domain <- [otp, macula]].

%% The filter never raises, whatever an event holds: logger removes a filter that raises, and every report after it
%% would go out unredacted with no signal. Each event comes back as an event, and a term that is no event comes back
%% as it is.
the_filter_returns_every_event_shape_without_raising_test_() ->
    Key = key(),
    {_Fun, Modules} = expected_filter(),
    Held = event({report, #{held => Key}}, [otp]),
    Shapes = [event({string, "a string message"}, [otp]),
              event({"~p and ~p", [one, Key]}, [otp]),
              Held,
              event({report, [{held, Key}, {other, 1}]}, [macula]),
              event({report, [improper | tail]}, [otp]),
              event({report, not_a_map_or_a_list}, [otp]),
              event({report, fun() -> Key end}, [otp]),
              event({report, #{reason => {function_clause, [{macula_client, init, [one | two], [{line, 1}]}]}}},
                    [otp]),
              Held#{meta := #{domain => [otp], report_cb => fun(Report) -> {"~p", [Report]} end}},
              Held#{meta := #{domain => not_a_list}},
              Held#{meta := #{domain => []}},
              Held#{meta := not_a_map},
              maps:remove(meta, Held),
              not_an_event],
    [?_assert(event_returned(Shape, macula_node_keys:redacted_log_event(Shape, Modules))) || Shape <- Shapes].

%% A crash report whose top stack frame is a macula function called with a key map, or with a loader that returns the
%% key, shows that frame's arity in place of its arguments, and no private half in the term or its formatted text.
a_top_frame_that_holds_a_key_or_its_loader_shows_its_arity_test_() ->
    Key = key(),
    {_Fun, Modules} = expected_filter(),
    Location = [{file, "macula_station_link.erl"}, {line, 1}],
    [?_test(begin
                Stack = [{macula_station_link, handle_call, [a_request, from, Holder], Location}],
                Report = #{label => {proc_lib, crash}, report => [[{error_info, {error, function_clause, Stack}}]]},
                Event = macula_node_keys:redacted_log_event(event({report, Report}, [otp, sasl]), Modules),
                #{msg := {report, #{report := [[{error_info, {error, function_clause, [Frame]}}]]}}} = Event,
                ?assertEqual({macula_station_link, handle_call, 3, Location}, Frame),
                ?assertEqual([], [Private || Private <- privates(Key),
                                             holds(Event, Private) orelse holds(formatted(Event), printed(Private))])
            end)
     || Holder <- [#{node_identity => Key}, fun() -> Key end]].

other_events_pass_unchanged_test_() ->
    Key = key(),
    Events = [event({report, #{held => Key}}, [host]), event({report, #{held => Key}}, []),
              event({"held ~p", [Key]}, [otp])],
    [?_assertEqual(Event, macula_node_keys:redacted_log_event(Event, #{})) || Event <- Events].

%%------------------------------------------------------------------
%% Status output
%%------------------------------------------------------------------

every_process_that_holds_a_key_redacts_it_in_each_part_of_its_status_test_() ->
    Key = key(),
    Pair = macula_identity:generate(),
    Privates = [maps:get(private, Pair) | privates(Key)],
    Held = {state, #{node_identity => Key, connect_key => Key}, Pair},
    ServerStatus = #{state => Held, message => {call, Key}, reason => {bad_return, [Pair]}, log => [{in, Key}]},
    StatemStatus = #{state => connected, data => Held, reason => {bad, Pair}, queue => [{info, Key}],
                     postponed => [{cast, Pair}], timeouts => [{state_timeout, Key}], log => [{in, Pair}]},
    [{atom_to_list(Module),
      ?_assertEqual({maps:keys(Status), [], true}, status_seen(Module, Status, Privates, maps:get(public, Pair)))}
     || {Module, Status} <- [{macula_client, ServerStatus}, {hecate_pubsub_server, ServerStatus},
                             {hecate_pubsub_registry, ServerStatus}, {macula_statement_issuer, ServerStatus},
                             {macula_station_link, ServerStatus}, {macula_peering_conn, StatemStatus}]].

the_status_of_a_started_pool_carries_no_private_key_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        started(),
        Key = key(),
        {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
        Status = sys:get_status(Pool),
        ok = macula_client:close(Pool),
        assert_no_private_half(Status, privates(Key))
    end}.

the_status_of_a_pubsub_server_carries_no_private_key_test() ->
    started(),
    Key = key(),
    {ok, Server} = hecate_pubsub_server:start_link(#{realm => crypto:strong_rand_bytes(32), identity => Key}),
    Status = sys:get_status(Server),
    ok = gen_server:stop(Server),
    assert_no_private_half(Status, privates(Key)).

the_status_of_a_pubsub_registry_carries_no_private_key_test() ->
    started(),
    Key = key(),
    {ok, Registry} = hecate_pubsub_registry:start_link(#{identity => Key}),
    %% The registry stops with reason shutdown.
    unlink(Registry),
    Status = sys:get_status(Registry),
    _ = hecate_pubsub_registry:stop(Registry),
    assert_no_private_half(Status, privates(Key)).

the_status_of_a_statement_issuer_carries_no_private_key_test() ->
    Identity = key(),
    {ok, Tls} = macula_node_keys:generate(tls, profile()),
    {ok, Issuer} = macula_statement_issuer:start_link(#{identity => fun() -> Identity end, owner => self()}),
    ok = macula_statement_issuer:register_tls_leaf(Issuer, <<"a leaf">>, Tls),
    #{connect_key := Connect} = macula_statement_issuer:connect_material(Issuer),
    Status = sys:get_status(Issuer),
    ok = gen_server:stop(Issuer),
    assert_no_private_half(Status, privates(Identity) ++ privates(Tls) ++ privates(Connect)).

%% A start the issuer refuses, here for a key that is no identity key, returns the refusal and reports nothing that
%% holds the key: the key reached the issuer only as a function that returns it.
a_refused_issuer_start_carries_no_private_key_test() ->
    started(),
    {ok, Connect} = macula_node_keys:generate(connect, profile()),
    Test = self(),
    Events = captured(fun() ->
                          Test ! {start_returned, catch macula_statement_issuer_sup:start_issuer(fun() -> Connect end,
                                                                                                self())}
                      end),
    Returned = receive {start_returned, Result} -> Result after 0 -> no_return end,
    ?assertEqual({error, {identity, not_an_identity_key}}, Returned),
    ?assertEqual([], exposed({Returned, Events}, privates(Connect))).

the_status_of_a_pool_carries_no_private_key_test() ->
    started(),
    Key = key(),
    {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
    Status = sys:get_status(Pool),
    ok = macula_client:close(Pool),
    ?assertEqual([], exposed(Status, privates(Key))).

the_status_of_a_station_link_carries_no_private_key_test() ->
    started(),
    Key = key(),
    {ok, NodeId} = macula_node_keys:node_id(Key),
    Issuer = spawn(fun() -> receive stop -> ok end end),
    {ok, Link} = macula_station_link:start_link(#{seed => #{host => <<"127.0.0.1">>, port => 1,
                                                            expected_node_id => NodeId},
                                                  node_identity => fun() -> Key end, issuer => Issuer,
                                                  connect => fun(_PeeringOpts) -> {error, not_dialed_here} end}),
    Status = sys:get_status(Link),
    ok = macula_station_link:stop(Link),
    Issuer ! stop,
    assert_no_private_half(Status, privates(Key)).

%%------------------------------------------------------------------
%% Crash and diagnostics reports, through the application's filter
%%------------------------------------------------------------------

crash_reports_carry_no_private_key_test_() ->
    {timeout, ?EU_TIMEOUT,
     [{"a function_clause in a gen_server callback", fun a_function_clause_in_a_gen_server_callback/0},
      {"a function_clause in a gen_statem callback", fun a_function_clause_in_a_gen_statem_callback/0},
      {"a function_clause in a helper called with a key map", fun a_function_clause_in_a_helper/0},
      {"a key map in the message queue at the crash", fun a_key_map_in_the_message_queue/0},
      {"a supervised child's crash", fun a_supervised_child_crash/0},
      {"a diagnostics event whose reason carries a crash", fun a_diagnostics_reason/0},
      {"a host module's function_clause keeps its arguments", fun a_host_function_clause_keeps_its_arguments/0}]}.

a_function_clause_in_a_gen_server_callback() ->
    started(),
    Identity = key(),
    {ok, Issuer} = gen_server:start(macula_statement_issuer, #{identity => fun() -> Identity end, owner => self()}, []),
    #{connect_key := Connect} = macula_statement_issuer:connect_material(Issuer),
    Events = captured(fun() -> catch gen_server:call(Issuer, not_a_request) end),
    assert_clean(Events, [{gen_server, terminate}, {proc_lib, crash}], privates(Identity) ++ privates(Connect)).

a_function_clause_in_a_gen_statem_callback() ->
    started(),
    Key = key(),
    {ok, Statem} = gen_statem:start(?MODULE, {statem, Key}, []),
    Events = captured(fun() -> catch gen_statem:call(Statem, not_a_request) end),
    assert_clean(Events, [{gen_statem, terminate}, {proc_lib, crash}], privates(Key)).

a_function_clause_in_a_helper() ->
    started(),
    Key = key(),
    Unsignable = unsignable(Key),
    Events = captured(fun() -> proc_lib:spawn(fun() -> macula_node_keys:sign(<<"a message">>, Unsignable) end) end),
    assert_clean(Events, [{proc_lib, crash}], privates(Key)).

a_key_map_in_the_message_queue() ->
    started(),
    Identity = key(),
    Carried = key(),
    {ok, Issuer} = gen_server:start(macula_statement_issuer, #{identity => fun() -> Identity end, owner => self()}, []),
    Events = captured(fun() ->
        ok = sys:suspend(Issuer),
        Issuer ! {'$gen_call', {self(), make_ref()}, not_a_request},
        Issuer ! {a_later_message, Carried},
        ok = sys:resume(Issuer)
    end),
    assert_clean(Events, [{gen_server, terminate}, {proc_lib, crash}], privates(Identity) ++ privates(Carried)).

a_supervised_child_crash() ->
    started(),
    Identity = key(),
    {ok, Supervisor} = supervisor:start_link(?MODULE, {supervisor, #{identity => fun() -> Identity end, owner => self()}}),
    unlink(Supervisor),
    [{issuer, Issuer, worker, _}] = supervisor:which_children(Supervisor),
    Events = captured(fun() -> catch gen_server:call(Issuer, not_a_request) end),
    ok = gen_server:stop(Supervisor),
    assert_clean(Events, [{gen_server, terminate}, {proc_lib, crash}, {supervisor, child_terminated}],
                 privates(Identity)).

a_diagnostics_reason() ->
    started(),
    Key = key(),
    {'EXIT', Reason} = (catch macula_node_keys:sign(<<"a message">>, unsignable(Key))),
    Events = captured(fun() ->
        macula_diagnostics:event(warning, ?PEERING_EXIT, #{peer_pid => self(), reason => Reason})
    end),
    assert_clean(Events, [?PEERING_EXIT], privates(Key)).

a_host_function_clause_keeps_its_arguments() ->
    started(),
    Events = captured(fun() -> proc_lib:spawn(fun() -> ?MODULE:host_helper(?MARKER) end) end),
    ?assertMatch([_ | _], [Event || Event <- Events, label(Event) =:= {proc_lib, crash},
                                    binary:match(formatted(Event), ?MARKER) =/= nomatch]).

%%------------------------------------------------------------------
%% The filter is installed once and stays
%%------------------------------------------------------------------

%% The application's start and every pool install the filter, and nothing removes it: a process that holds a key can
%% outlive the application, and the filter changes nothing but key material. A node holds one filter across a restart.
the_application_holds_one_redaction_filter_across_a_restart_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        started(),
        ?assertEqual(1, installed()),
        ok = application:stop(macula),
        ?assertEqual(1, installed()),
        started(),
        ?assertEqual(1, installed())
    end}.

a_start_replaces_a_filter_left_behind_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        started(),
        ok = application:stop(macula),
        _ = logger:remove_primary_filter(?FILTER),
        ok = logger:add_primary_filter(?FILTER, {fun(Event, _) -> Event end, left_behind}),
        started(),
        Held = [Filter || {Id, _} = Filter <- primary_filters(), Id =:= ?FILTER],
        _ = [logger:remove_primary_filter(?FILTER) || {_, {_, left_behind}} <- Held],
        ?assertMatch([{?FILTER, {_, #{macula_node_keys := true}}}], Held)
    end}.

stopping_the_application_removes_no_filter_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        started(),
        ok = logger:add_primary_filter(host_filter, {fun(Event, _) -> Event end, host}),
        Before = filter_ids(),
        ok = application:stop(macula),
        After = filter_ids(),
        started(),
        ok = logger:remove_primary_filter(host_filter),
        ?assertEqual({[], true}, {Before -- After, lists:member(?FILTER, After)})
    end}.

%% The installer keeps one filter, the external function redacted_log_event/2 with the application's modules, however
%% often it runs, and puts that filter in place of another value held under its id.
installing_again_keeps_the_one_filter_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        started(),
        Expected = expected_filter(),
        ok = macula_node_keys:install_log_redaction(),
        ok = macula_node_keys:install_log_redaction(),
        Held = [Filter || {Id, Filter} <- primary_filters(), Id =:= ?FILTER],
        _ = logger:remove_primary_filter(?FILTER),
        ok = logger:add_primary_filter(?FILTER, {fun(Event, _) -> Event end, left_behind}),
        ok = macula_node_keys:install_log_redaction(),
        Replaced = [Filter || {Id, Filter} <- primary_filters(), Id =:= ?FILTER],
        ?assertEqual({[Expected], [Expected]}, {Held, Replaced})
    end}.

%% A pool started without the macula application installs the filter itself, so a key holder in its tree that crashes
%% leaves reports with no private half: here the pool's statement issuer, which holds the pool's CONNECT key and a
%% loader of its identity key, hits a function_clause.
a_pool_started_without_the_application_redacts_its_crash_reports_test_() ->
    {timeout, ?EU_TIMEOUT, fun() ->
        started(),
        Identity = key(),
        ok = application:stop(macula),
        _ = logger:remove_primary_filter(?FILTER),
        Test = self(),
        Start = fun(Load, Owner) ->
                    {ok, Started} = Result = macula_statement_issuer:start_link(#{identity => Load, owner => Owner}),
                    Test ! {issuer_started, Started},
                    Result
                end,
        try
            {ok, Pool} = macula_client:connect([], #{node_identity => Identity, issuer_start => Start}),
            Issuer = receive {issuer_started, Pid} -> Pid after 5_000 -> erlang:error(no_issuer) end,
            #{connect_key := Connect} = macula_statement_issuer:connect_material(Issuer),
            Installed = installed(),
            Events = captured(fun() -> catch gen_server:call(Issuer, not_a_request) end),
            ok = macula_client:close(Pool),
            ?assertEqual(1, Installed),
            assert_clean(Events, [{gen_server, terminate}, {proc_lib, crash}],
                         privates(Identity) ++ privates(Connect))
        after
            started()
        end
    end}.

%%------------------------------------------------------------------
%% A gen_statem that holds a key, a supervisor of one issuer, and a host helper
%%------------------------------------------------------------------

callback_mode() ->
    state_functions.

init({statem, Key}) ->
    {ok, holding, #{key => Key}};
init({supervisor, Options}) ->
    Issuer = #{id => issuer, start => {macula_statement_issuer, start_link, [Options]}, restart => temporary},
    {ok, {#{strategy => one_for_one, intensity => 1, period => 5}, [Issuer]}}.

holding({call, From}, known, Data) ->
    {keep_state, Data, [{reply, From, ok}]}.

host_helper(known) ->
    ok.

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

started() ->
    {ok, _} = application:ensure_all_started(macula),
    ok.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

key() ->
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    Key.

%% A key map that no clause of macula_node_keys:sign/2 takes.
unsignable(#{components := [Component | _]} = Key) ->
    Key#{components := [Component#{algorithm := unknown}]}.

privates(#{components := Components}) ->
    [Private || #{private := Private} <- Components].

redacted_key(#{components := Components} = Key) ->
    Key#{components := [Component#{private := redacted} || Component <- Components]}.

assert_key_redacted(Purpose, Profile) ->
    {ok, Key} = macula_node_keys:generate(Purpose, Profile),
    Redacted = macula_node_keys:redacted(Key),
    ?assertEqual(redacted_key(Key), Redacted),
    ?assertEqual([], exposed(Redacted, privates(Key))).

%% The parts a formatted status keeps, the private halves it exposes, and whether it still shows a public half.
status_seen(Module, Status, Privates, Public) ->
    Formatted = Module:format_status(Status),
    {maps:keys(Formatted), exposed(Formatted, Privates), holds(Formatted, Public)}.

%% No private half's bytes are in the status, it holds keys, and every private half in it is redacted.
assert_no_private_half(Status, Privates) ->
    ?assertEqual([], exposed(Status, Privates)),
    Halves = private_halves(Status),
    ?assertNotEqual([], Halves),
    ?assertEqual([], [Half || Half <- Halves, Half =/= redacted]).

exposed(Term, Privates) ->
    [Private || Private <- Privates, holds(Term, Private)].

holds(Term, Bytes) ->
    binary:match(term_to_binary(Term), Bytes) =/= nomatch.

private_halves(#{public := _, private := Private} = Map) ->
    [Private | private_halves(maps:values(maps:remove(private, Map)))];
private_halves(Map) when is_map(Map) ->
    private_halves(maps:values(Map));
private_halves([Head | Tail]) ->
    private_halves(Head) ++ private_halves(Tail);
private_halves(Tuple) when is_tuple(Tuple) ->
    private_halves(tuple_to_list(Tuple));
private_halves(_Other) ->
    [].

filtered(Report, Domain, Modules) ->
    #{msg := {report, Filtered}} = macula_node_keys:redacted_log_event(event({report, Report}, Domain), Modules),
    Filtered.

event(Msg, Domain) ->
    #{level => error, msg => Msg, meta => #{domain => Domain, time => 0}}.

primary_filters() ->
    maps:get(filters, logger:get_primary_config()).

filter_ids() ->
    [Id || {Id, _} <- primary_filters()].

installed() ->
    length([Id || Id <- filter_ids(), Id =:= ?FILTER]).

%% The filter the installer puts in place: the external function redacted_log_event/2 with the application's modules.
expected_filter() ->
    {ok, Modules} = application:get_key(macula, modules),
    {fun macula_node_keys:redacted_log_event/2, maps:from_keys(Modules, true)}.

%% A filter's answer: an event for an event, and a term that is no event as it is.
event_returned(Given, Returned) when is_map(Given) -> is_map(Returned);
event_returned(Given, Returned) -> Given =:= Returned.

%% The events every handler receives while Act runs, through a handler added for the run.
captured(Act) ->
    Handler = list_to_atom("macula_key_redaction_capture_" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = logger:add_handler(Handler, ?MODULE, #{config => #{test => self()}, level => all, filter_default => log}),
    _ = Act(),
    timer:sleep(500),
    ok = logger:remove_handler(Handler),
    drained([]).

log(Event, #{config := #{test := Test}}) ->
    Test ! {captured, Event}.

drained(Events) ->
    receive
        {captured, Event} -> drained([Event | Events])
    after 0 ->
        lists:reverse(Events)
    end.

%% The expected reports were captured, and no private half shows in any of them: not its bytes in the event, and not
%% its last bytes as printed in the event's formatted text.
assert_clean(Events, Expected, Privates) ->
    ?assertEqual([], Expected -- [label(Event) || Event <- Events]),
    ?assertEqual([], [label(Event) || Event <- Events, Private <- Privates,
                                      holds(Event, Private) orelse holds(formatted(Event), printed(Private))]).

formatted(Event) ->
    Config = #{single_line => true, legacy_header => false, chars_limit => unlimited, depth => unlimited,
               max_size => unlimited},
    unicode:characters_to_binary(logger_formatter:format(Event, Config)).

%% The last 16 bytes of a private half, as printed. An ML-DSA-87 private key begins with the bytes its public key
%% begins with, and the public half stays in a report, so the check uses bytes only the private half holds.
printed(Private) ->
    Tail = binary:part(Private, byte_size(Private) - 16, 16),
    iolist_to_binary(lists:join(",", [integer_to_list(Byte) || <<Byte>> <= Tail])).

label(#{msg := {report, #{label := Label}}}) -> Label;
label(#{msg := {report, #{event := Topic}}}) -> Topic;
label(#{msg := {report, _}}) -> report;
label(#{msg := _}) -> other.
