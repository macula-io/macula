%% EUnit tests for the facade's provider advertise: `macula:advertise/5'
%% resolves the pool's own D25 provider authorization — the
%% realm-signed org directory and the org-signed procedure delegation
%% naming the pool's node id — from the DHT, signs an advertisement,
%% verifies it against the realm key the pool pins, and hands the pool
%% fan-out the advertisement spec (the verified authorization and the
%% chain's bound), which each link signs naming its own station (#29).
%% Every missing piece of the chain fails fast with
%% `{error, {provider_authorization, _}}', and nothing is sent.
%%
%% The resolution's DHT calls and the fan-out itself are injected as
%% the seam entries `advertise/5''s `Opts' takes (`provider_io/0' plus
%% the `advertise' override) — no module is replaced, which is what
%% macula_shared_module_mocks_tests enforces. The records are real:
%% signed with realm, org and identity keys in the node's profile.
-module(macula_provider_advertise_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MINUTE, 60 * 1000).
-define(HOUR, 60 * ?MINUTE).
-define(REALM, <<7:256>>).
-define(ORG, <<"acme">>).
-define(PROC, <<"acme/count_v1">>).

provider_advertise_test_() ->
    {timeout, 120, {setup, fun start/0, fun stop/1, fun cases/1}}.

cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_complete_chain_hands_the_pool_a_spec_a_link_signs/1,
                 fun a_procedure_without_an_org_namespace_is_refused/1,
                 fun a_missing_org_directory_is_refused/1,
                 fun a_missing_delegation_is_refused/1,
                 fun an_unpinned_realm_key_is_refused/1,
                 fun a_delegation_ending_first_caps_the_advertisement/1,
                 fun an_org_directory_ending_first_caps_the_advertisement/1,
                 fun a_republished_chain_restores_the_full_lifetime/1,
                 fun an_own_namespace_procedure_needs_no_chain/1,
                 fun another_nodes_namespace_is_refused/1]].

%%------------------------------------------------------------------
%% Happy path
%%------------------------------------------------------------------

a_complete_chain_hands_the_pool_a_spec_a_link_signs(
  #{node := NodeId, realm_key := RealmPub} = Keys) ->
    Handler = fun(_Payload) -> {ok, counted} end,
    with_opts(Keys, #{}, Handler, fun(Opts) ->
        ?assertEqual(ok, macula:advertise(self(), ?REALM, ?PROC, Handler, Opts))
    end),
    Spec = advertise_sent(),
    %% The pool gets the spec, not a signed advertisement naming any
    %% station: the station is the link's to name.
    ?assertMatch(#{authorization := #{org_directory := _, procedure_delegation := _},
                   not_after := _}, Spec),
    %% What a link connected to a station makes of it names that
    %% station, is the pool's node's, and its authorization verifies
    %% against the pinned realm key.
    Station = <<9:256>>,
    Decoded = link_signed(Keys, Spec, Station),
    Read = macula_record:read_procedure_advertisement(Decoded),
    ?assertEqual(NodeId, maps:get(advertiser_node, Read)),
    ?assertEqual(Station, maps:get(serving_station, Read)),
    {ok, Profile} = macula_crypto_profile:configured(),
    ?assertEqual(ok,
                 macula_record:verify_authorization(
                   Decoded, #{profile => Profile, realm_key => RealmPub},
                   erlang:system_time(millisecond))).

%%------------------------------------------------------------------
%% The advertisement never outlives what authorizes it
%%
%% An advertisement is built with its type's own lifetime, 5 minutes,
%% which knows nothing about the chain it carries. Both verifiers
%% refuse one that outlives its authorization: macula_record's
%% delegation_matched/3 and, at admission, macula-station's. So in the
%% last 5 minutes of a delegation or an org directory the provider
%% signs an advertisement its own pool then refuses, and goes dark
%% before its authorization actually expires. The advertisement is
%% capped at the earlier of the two, which is what `not_after' is for.
%%
%% Two cases, one per side of the `min': a cap taken from whichever
%% record happens to be read first passes one and fails the other.
%%------------------------------------------------------------------

a_delegation_ending_first_caps_the_advertisement(Keys) ->
    capped_at(Keys, ?HOUR, 2 * ?MINUTE).

an_org_directory_ending_first_caps_the_advertisement(Keys) ->
    capped_at(Keys, 2 * ?MINUTE, ?HOUR).

%% The chain is published with these lifetimes, the advertisement is
%% sent, and it ends at the earlier of the two, not 5 minutes from now.
capped_at(Keys, DirTtl, DelTtl) ->
    Handler = fun(_P) -> {ok, counted} end,
    {Stub, Earliest} = short_chain_stub(Keys, DirTtl, DelTtl),
    with_opts(Keys, #{find_record => Stub}, Handler, fun(Opts) ->
        ?assertEqual(ok, macula:advertise(self(), ?REALM, ?PROC, Handler, Opts))
    end),
    Spec = advertise_sent(),
    ?assertEqual(Earliest, maps:get(not_after, Spec)),
    {ok, Profile} = macula_crypto_profile:configured(),
    Decoded = link_signed(Keys, Spec, <<9:256>>),
    ?assertEqual(Earliest, macula_record:expires_at(Decoded)),
    %% And the consequence that matters: the pool's own check of the
    %% chain, the one that refuses an advertisement outliving it.
    ?assertEqual(ok,
                 macula_record:verify_authorization(
                   Decoded, #{profile => Profile,
                              realm_key => maps:get(realm_key, Keys)},
                   erlang:system_time(millisecond))).

%% The cap follows the chain, so a provider that was advertising in
%% short bursts near its authorization's end goes back to its full
%% lifetime as soon as the org republishes. Without this, a cap taken
%% once and held would keep a recovered provider on short
%% advertisements for as long as the pool lived.
a_republished_chain_restores_the_full_lifetime(Keys) ->
    Handler = fun(_P) -> {ok, counted} end,
    {Ending, _Earliest} = short_chain_stub(Keys, ?HOUR, 2 * ?MINUTE),
    {Republished, _Fresh} = short_chain_stub(Keys, 6 * ?HOUR, 6 * ?HOUR),
    with_opts(Keys, #{find_record => Ending}, Handler, fun(Opts) ->
        ?assertEqual(ok, macula:advertise(self(), ?REALM, ?PROC, Handler, Opts))
    end),
    Near = expires_at_of(Keys, advertise_sent()),
    with_opts(Keys, #{find_record => Republished}, Handler, fun(Opts) ->
        ?assertEqual(ok, macula:advertise(self(), ?REALM, ?PROC, Handler, Opts))
    end),
    After = expires_at_of(Keys, advertise_sent()),
    %% The second is no longer held down by the chain that was ending:
    %% it runs the advertisement's own lifetime, which is longer than
    %% the 2 minutes the first was capped to.
    ?assert(After > Near),
    Left = After - erlang:system_time(millisecond),
    ?assert(Left > 4 * ?MINUTE),
    %% And no longer: the chain being long does not stretch the
    %% advertisement past its own 5-minute lifetime.
    ?assert(Left =< 5 * ?MINUTE).

expires_at_of(Keys, Spec) ->
    macula_record:expires_at(link_signed(Keys, Spec, <<9:256>>)).

%% The advertisement a link connected to `Station' sends for `Spec',
%% taken from the frame a real macula_station_link puts on the wire,
%% verified under the node's profile.
link_signed(Keys, Spec, Station) ->
    link_signed(Keys, Spec, Station, ?PROC).

link_signed(#{key := Key}, Spec, Station, Procedure) ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Issuer} = macula_statement_issuer_sup:start_issuer(fun() -> Key end, self()),
    {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
    {ok, Admission} = macula_request_admission:start_link(
                        #{caller_quota => 256, share => 1024, cap => 46080,
                          reply_bytes => 262144, reply_bytes_total => 16777216}),
    {ok, Link} = macula_station_link:start_link(
                   #{seed => #{host => <<"127.0.0.1">>, port => 1},
                     connect_timeout_ms => 2000,
                     node_identity => fun() -> Key end, issuer => Issuer,
                     admission => Admission, pool => Pool,
                     share => {seed, {<<"127.0.0.1">>, 1}},
                     expected_node_id => <<1:256>>}),
    Peer = self(),
    _ = sys:replace_state(Link, fun(S) ->
            setelement(macula_station_link:state_field_index(peer_pid), S, Peer)
        end),
    Link ! {macula_peering, connected, Peer, Station},
    ok = macula_station_link:advertise(Link, ?REALM, Procedure,
                                       fun(_) -> {ok, counted} end, open, Spec),
    Encoded = receive
                  {'$gen_cast', {send_frame, _, #{frame_type := advertise,
                                               advertisement := A}}} -> A
              after 1_000 -> error(no_advertise_frame)
              end,
    macula_station_link:stop(Link),
    ok = macula_client:close(Pool),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Decoded} = macula_record:verify(Encoded, Profile),
    Decoded.

%%------------------------------------------------------------------
%% A node's own namespace (D25 item 6, revised 2026-09-24)
%%------------------------------------------------------------------

%% `~<own node_id>/<name>' resolves no org directory and no delegation: the pool gets a spec with no authorization and
%% no bound, and what a link signs from it verifies with no realm key.
an_own_namespace_procedure_needs_no_chain(#{node := NodeId} = Keys) ->
    Handler = fun(_P) -> {ok, counted} end,
    Procedure = <<"~", (binary:encode_hex(NodeId, lowercase))/binary, "/ring">>,
    NoDirectory = fun(_Pool, _Key) -> error(no_lookup_expected) end,
    with_opts(Keys, #{find_record => NoDirectory}, Handler, fun(Opts) ->
        ?assertEqual(ok, macula:advertise(self(), ?REALM, Procedure, Handler, Opts))
    end),
    Spec = advertise_sent(),
    ?assertEqual(false, is_map_key(authorization, Spec)),
    ?assertEqual(false, is_map_key(not_after, Spec)),
    {ok, Profile} = macula_crypto_profile:configured(),
    Decoded = link_signed(Keys, Spec, <<9:256>>, Procedure),
    ?assertEqual(ok, macula_record:verify_authorization(Decoded, #{profile => Profile},
                                                        erlang:system_time(millisecond))).

another_nodes_namespace_is_refused(Keys) ->
    Handler = fun(_P) -> {ok, counted} end,
    Procedure = <<"~", (binary:encode_hex(<<1:256>>, lowercase))/binary, "/ring">>,
    with_opts(Keys, #{}, Handler, fun(Opts) ->
        ?assertEqual({error, {provider_authorization, not_own_namespace}},
                     macula:advertise(self(), ?REALM, Procedure, Handler, Opts))
    end),
    ?assertEqual(not_sent, advertise_sent()).

%%------------------------------------------------------------------
%% Refusals — nothing is sent under any of them
%%------------------------------------------------------------------

a_procedure_without_an_org_namespace_is_refused(Keys) ->
    Handler = fun(_P) -> {ok, counted} end,
    with_opts(Keys, #{}, Handler, fun(Opts) ->
        ?assertEqual({error, {provider_authorization, no_org_namespace}},
                     macula:advertise(self(), ?REALM, <<"_dht.nothing">>,
                                      Handler, Opts))
    end),
    ?assertEqual(not_sent, advertise_sent()).

a_missing_org_directory_is_refused(Keys) ->
    Handler = fun(_P) -> {ok, counted} end,
    with_opts(Keys, #{find_record => find_stub(Keys, org_directory)},
              Handler, fun(Opts) ->
        ?assertEqual({error, {provider_authorization, {org_directory, not_found}}},
                     macula:advertise(self(), ?REALM, ?PROC, Handler, Opts))
    end),
    ?assertEqual(not_sent, advertise_sent()).

a_missing_delegation_is_refused(Keys) ->
    Handler = fun(_P) -> {ok, counted} end,
    with_opts(Keys, #{find_record => find_stub(Keys, procedure_delegation)},
              Handler, fun(Opts) ->
        ?assertEqual({error, {provider_authorization,
                              {procedure_delegation, not_found}}},
                     macula:advertise(self(), ?REALM, ?PROC, Handler, Opts))
    end),
    ?assertEqual(not_sent, advertise_sent()).

an_unpinned_realm_key_is_refused(Keys) ->
    Handler = fun(_P) -> {ok, counted} end,
    with_opts(Keys, #{realm_key => fun(_Pool, _Realm) -> none end},
              Handler, fun(Opts) ->
        ?assertEqual({error, {provider_authorization, no_realm_key}},
                     macula:advertise(self(), ?REALM, ?PROC, Handler, Opts))
    end),
    ?assertEqual(not_sent, advertise_sent()).

%%------------------------------------------------------------------
%% The seam
%%------------------------------------------------------------------

%% The `Opts' the tests hand `advertise/5': the provider_io entries the
%% resolution reads its DHT calls from, plus the `advertise' fan-out
%% override recording the spec it is handed, each overridable per case.
with_opts(Keys, Overrides, _Handler, Fun) ->
    Self = self(),
    Base = #{status => fun(_Pool) ->
                           {ok, #{self_node_id => maps:get(node, Keys)}}
                       end,
             find_record => find_stub(Keys, none),
             %% As the pool's own bounded signing does it: one clock
             %% read stamps the record and ends it at the bound.
             sign_node_record => fun(_Pool, Unsigned, #{not_after := NotAfter}) ->
                                     macula_record:refresh(
                                       Unsigned, maps:get(key, Keys), NotAfter)
                                 end,
             realm_key => fun(_Pool, _Realm) ->
                              {ok, maps:get(realm_key, Keys)}
                          end,
             advertise => fun(_Pool, _Realm, _Proc, _Hand, _Policy,
                              Spec) ->
                              Self ! {advertised, Spec},
                              ok
                          end},
    Fun(maps:merge(Base, Overrides)).

%% The DHT lookups underneath the facade's find_record: the verified
%% record (the facade's own find_record/2 contract), or a not_found for
%% a missing piece (or anything else).
find_stub(Keys, Missing) ->
    {OrgDir, Deleg} = published_chain(Keys),
    OrgDirKey = maps:get(org_dir_key, Keys),
    DelegKey  = maps:get(deleg_key, Keys),
    fun(_Pool, Key) ->
        find_reply(Key, OrgDirKey, DelegKey, Missing, OrgDir, Deleg)
    end.

find_reply(OrgDirKey, OrgDirKey, _DelegKey, org_directory, _OrgDir, _Deleg) ->
    {error, not_found};
find_reply(DelegKey, _OrgDirKey, DelegKey, procedure_delegation, _OrgDir,
           _Deleg) ->
    {error, not_found};
find_reply(OrgDirKey, OrgDirKey, _DelegKey, _Missing, OrgDir, _Deleg) ->
    verified(OrgDir);
find_reply(DelegKey, _OrgDirKey, DelegKey, _Missing, _OrgDir, Deleg) ->
    verified(Deleg);
find_reply(_Key, _OrgDirKey, _DelegKey, _Missing, _OrgDir, _Deleg) ->
    {error, not_found}.

%% A find_record stub over a chain published with the given lifetimes,
%% and the earlier of the two expiries the advertisement must not pass.
short_chain_stub(#{node := NodeId, realm_kp := RealmKp, org_key := OrgKey,
                   org_key_id := OrgKeyId} = Keys, DirTtl, DelTtl) ->
    OrgDir = macula_record:sign(
               macula_record:org_directory(?REALM, ?ORG, OrgKeyId,
                                           #{ttl_ms => DirTtl}), RealmKp),
    Deleg = macula_record:sign(
              macula_record:procedure_delegation(OrgKeyId, NodeId,
                                                 #{ttl_ms => DelTtl}), OrgKey),
    OrgDirKey = maps:get(org_dir_key, Keys),
    DelegKey = maps:get(deleg_key, Keys),
    {fun(_Pool, Key) ->
         find_reply(Key, OrgDirKey, DelegKey, none, OrgDir, Deleg)
     end,
     min(macula_record:expires_at(OrgDir), macula_record:expires_at(Deleg))}.

%% The facade's find_record/2 returns records already verified under
%% the node's profile — the seam stub answers in the same shape.
verified(Record) ->
    {ok, Profile} = macula_crypto_profile:configured(),
    macula_record:verify(macula_record:encode(Record), Profile).

%% The advertisement spec the facade handed to the pool fan-out, or
%% not_sent.
advertise_sent() ->
    receive
        {advertised, Ad} -> Ad
    after 100 ->
        not_sent
    end.

%%------------------------------------------------------------------
%% Setup and records
%%------------------------------------------------------------------

start() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, NodeId} = macula_node_keys:node_id(Key),
    {ok, RealmKey} = macula_node_keys:generate(realm, Profile),
    {ok, OrgKey} = macula_node_keys:generate(org, Profile),
    OrgKeyId = macula_node_keys:key_id(OrgKey),
    #{profile => Profile, key => Key, node => NodeId,
      realm_key => macula_node_keys:public_key(RealmKey),
      realm_kp => RealmKey, org_key => OrgKey,
      org_key_id => OrgKeyId,
      org_dir_key => macula_record:org_directory_key(?REALM, ?ORG),
      deleg_key => macula_record:procedure_delegation_key(OrgKeyId, NodeId)}.

stop(_Keys) ->
    ok.

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

%% The chain the org published: a realm-signed org directory and an
%% org-signed delegation naming the pool's node.
published_chain(#{node := NodeId, realm_kp := RealmKp, org_key := OrgKey,
                  org_key_id := OrgKeyId}) ->
    OrgDir = macula_record:sign(
               macula_record:org_directory(?REALM, ?ORG, OrgKeyId), RealmKp),
    Deleg = macula_record:sign(
              macula_record:procedure_delegation(OrgKeyId, NodeId), OrgKey),
    {OrgDir, Deleg}.
