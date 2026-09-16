%%%-------------------------------------------------------------------
%%% @doc Tests that a macula_station_link connects with the node identity key
%%% and the statement issuer it is started with, and with nothing else. A link
%%% started without either, with a key of another purpose as its identity, or
%%% with a seed that names no expected node_id, refuses to start and makes no
%%% key of its own. The identity key reaches the link as a function that
%%% returns it. A link ends when its issuer does. The dial options are read
%%% where the link's connect function receives them, a link option, so no
%%% shared module is replaced.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_station_link_identity_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EVENT_MS, 5_000).

identity_test_() ->
    {timeout, 60,
     {setup, fun started/0, fun stopped/1,
      fun(Keys) ->
          [{"a link started without a node identity key refuses to start",
            ?_test(refuses_without(node_identity, Keys))},
           {"a link started without a statement issuer refuses to start",
            ?_test(refuses_without(issuer, Keys))},
           {"a link started without a request admission refuses to start",
            ?_test(refuses_without(admission, Keys))},
           {"a link started without its share of the admission refuses to start",
            ?_test(refuses_without(share, Keys))},
           {"a link started with a key of another purpose as its identity refuses to start",
            ?_test(refuses_another_purpose(Keys))},
           {"a link whose seed names no expected node_id refuses to start",
            ?_test(refuses_a_seed_without_an_expected_node_id(Keys))},
           {"a link dials with its node identity key, its issuer and the seed's expected node_id",
            ?_test(dials_with_its_keys(Keys))},
           {"a seed's own expected node_id stands over one in the link's options",
            ?_test(a_seed_pin_stands_over_a_link_option(Keys))},
           {"a link ends when its issuer does",
            ?_test(ends_with_its_issuer(Keys))}]
      end}}.

refuses_without(Field, #{node_identity := Key} = Keys) ->
    process_flag(trap_exit, true),
    Opts = maps:remove(Field, link_opts(Key, issuer(Keys))),
    ?assertEqual({error, {Field, required}}, macula_station_link:start_link(Opts)).

refuses_another_purpose(#{node_identity := Key, profile := Profile} = Keys) ->
    process_flag(trap_exit, true),
    {ok, ConnectKey} = macula_node_keys:generate(connect, Profile),
    Opts = (link_opts(Key, issuer(Keys)))#{node_identity := fun() -> ConnectKey end},
    ?assertEqual({error, {node_identity, not_an_identity_key}}, macula_station_link:start_link(Opts)).

refuses_a_seed_without_an_expected_node_id(#{node_identity := Key} = Keys) ->
    process_flag(trap_exit, true),
    Opts = (link_opts(Key, issuer(Keys)))#{seed := #{host => <<"127.0.0.1">>, port => 1}},
    ?assertEqual({error, {seed, expected_node_id_required}}, macula_station_link:start_link(Opts)).

dials_with_its_keys(#{node_identity := Key} = Keys) ->
    Test = self(),
    Issuer = issuer(Keys),
    Connect = fun(PeeringOpts) -> Test ! {dialed, PeeringOpts}, {error, not_dialed_here} end,
    {ok, Link} = macula_station_link:start_link((link_opts(Key, Issuer))#{connect => Connect}),
    Dialed = receive {dialed, Opts} -> Opts after ?EVENT_MS -> erlang:error(no_dial) end,
    ok = macula_station_link:stop(Link),
    {ok, NodeId} = macula_node_keys:node_id(Key),
    ?assertMatch(#{role := client, identity := Key, issuer := Issuer, target := #{expected_node_id := NodeId}},
                 Dialed),
    ?assertEqual([], [Field || Field <- [node_id, realms], maps:is_key(Field, Dialed)]).

%% A link's expected_node_id option fills a seed that names none, and a seed map that names its own keeps it.
a_seed_pin_stands_over_a_link_option(#{node_identity := Key} = Keys) ->
    Test = self(),
    Connect = fun(PeeringOpts) -> Test ! {dialed, PeeringOpts}, {error, not_dialed_here} end,
    {ok, NodeId} = macula_node_keys:node_id(Key),
    Pinned = (link_opts(Key, issuer(Keys)))#{connect => Connect, expected_node_id => <<7:256>>},
    Unpinned = Pinned#{seed := #{host => <<"127.0.0.1">>, port => 1}},
    ?assertEqual([NodeId, <<7:256>>], [dialed_pin(Opts) || Opts <- [Pinned, Unpinned]]).

%% The expected node_id a link started with Opts dials.
dialed_pin(Opts) ->
    {ok, Link} = macula_station_link:start_link(Opts),
    #{target := #{expected_node_id := Pin}} = receive {dialed, Dial} -> Dial after ?EVENT_MS -> erlang:error(no_dial) end,
    ok = macula_station_link:stop(Link),
    Pin.

%% A link whose issuer ends stops with a shutdown reason, so its end is no crash report.
%% The issuer's reason reads killed, or noproc when the kill reaches the issuer
%% before the link's monitor does: the runtime orders signals only between one
%% sender and one receiver.
ends_with_its_issuer(#{node_identity := Key}) ->
    Issuer = spawn(fun() -> receive stop -> ok end end),
    {ok, Link} = macula_station_link:start_link(link_opts(Key, Issuer)),
    unlink(Link),
    Mon = erlang:monitor(process, Link),
    exit(Issuer, kill),
    ?assertMatch({shutdown, {issuer_down, Why}} when Why =:= killed; Why =:= noproc,
                 receive {'DOWN', Mon, process, Link, Reason} -> Reason after ?EVENT_MS -> still_running end).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

started() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    #{profile => Profile, node_identity => Key, issuers => ets:new(link_identity_test_issuers, [public, bag])}.

stopped(#{issuers := Issuers}) ->
    _ = [Pid ! stop || {issuer, Pid} <- ets:tab2list(Issuers)],
    true = ets:delete(Issuers),
    ok.

%% A process that stands for an issuer: these tests dial nothing, so nothing calls it.
issuer(#{issuers := Issuers}) ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    true = ets:insert(Issuers, {issuer, Pid}),
    Pid.

%% Start options a link starts with: its seed names the node_id it expects, here the node's own, its connect function
%% refuses, so it dials nothing, and it has an admission and its share in it.
link_opts(Key, Issuer) ->
    {ok, NodeId} = macula_node_keys:node_id(Key),
    #{seed => #{host => <<"127.0.0.1">>, port => 1, expected_node_id => NodeId},
      node_identity => fun() -> Key end, issuer => Issuer, admission => admission(),
      share => {seed, {<<"127.0.0.1">>, 1}}, connect_timeout_ms => 2000,
      connect => fun(_PeeringOpts) -> {error, not_dialed_here} end}.

%% An admission with the pool's default limits, for a link started without a pool.
admission() ->
    {ok, Admission} = macula_request_admission:start_link(#{caller_quota => 256, share => 1024, cap => 46080,
                                                             reply_bytes => 262144, reply_bytes_total => 16777216}),
    Admission.
