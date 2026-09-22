%% EUnit tests for D31's `member_endorsement' field in CONNECT.
%%
%% D31, decided by Raf 2026-09-22: a station may require the connecting node to
%% present a realm membership endorsement, and **the `connect' frame always
%% carries `member_endorsement', empty when the node has none, so changing the
%% setting never changes the wire**. A station with invite-only off looks the
%% same on the wire as one with it on, which is the point: the setting is a
%% deployment decision, not something a peer can probe for.
%%
%% ⛔ SDK SIDE ONLY. The station's enforcement -- the `invite_only' setting,
%% `off' / `log_only' / `enforce', and the `not_invited' refusal -- is
%% Neptunus's and starts after 12 lands. Nothing here checks an endorsement;
%% these are about the field being carried, parsed and handed to the station.
%%
%% == Two things this pins that are easy to lose ==
%%
%% The proof does NOT cover the field, and that is what let WP 1.5 ship before
%% it. `proof_message/5' signs a fixed-length concatenation ending in the hash
%% of the CHALLENGE frame, never of the connect frame, so a key added to
%% `connect' cannot disturb it. `the_proof_does_not_cover_the_field' pins that,
%% because folding the field into the proof later would look like a tightening
%% and would silently break every peer that had not.
%%
%% And the version is 4, not 3. The field makes the frame's key set differ, and
%% `frame_type/3' matches key sets EXACTLY, so an older peer would otherwise
%% answer `malformed_frame' -- which reads as corruption and sends someone
%% hunting a codec bug. Version 3 already did this to every published client
%% once. `unsupported_version' says the true thing.
-module(macula_handshake_member_endorsement_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NOW, 1789000000000).
-define(HOUR, 3_600_000).
-define(DAY, 86_400_000).
-define(MINUTE, 60_000).
-define(LEAF, <<"leaf-der-bytes">>).
-define(CLIENT_CAPABILITIES, 3).
-define(STATION_CAPABILITIES, 5).
-define(ENDORSEMENT, <<"a signed realm_member_endorsement, as wire bytes">>).

member_endorsement_test_() ->
    {setup, fun world/0,
     fun(World) ->
         [{"the handshake version is 4, so an older peer says unsupported_version "
           "rather than malformed_frame",
           fun() -> the_version_is_four(World) end},
          {"a version 3 peer is refused with unsupported_version, which is what "
           "the bump is FOR",
           fun() -> a_version_three_peer_is_told_the_version(World) end},
          {"a connect frame carries member_endorsement even when the node has none",
           fun() -> the_field_is_always_carried(World) end},
          {"a connect frame carries the endorsement the session was given",
           fun() -> an_endorsement_is_carried(World) end},
          {"the station is handed the endorsement it received",
           fun() -> the_station_is_handed_the_endorsement(World) end},
          {"a station with no endorsement to check still accepts, because "
           "enforcement is not the SDK's business",
           fun() -> an_empty_endorsement_still_connects(World) end},
          {"the proof still verifies when the endorsement is swapped after "
           "signing, so the field is genuinely outside it",
           fun() -> the_proof_does_not_cover_the_field(World) end},
          {"a CONNECT carrying a real endorsement fits the 64 KiB handshake "
           "frame cap, with the headroom stated",
           {timeout, 120, fun() -> a_connect_with_an_endorsement_fits_the_frame_cap() end}}]
     end}.

%%%===================================================================
%%% Test bodies
%%%===================================================================

the_version_is_four(World) ->
    ?assertMatch(#{{text, <<"version">>} := 4}, decoded(connect_of(World, <<>>))).

%% ⛔ THE POINT OF THE VERSION BUMP, AND THE ONLY TEST THAT SHOWS IT. Without
%% it the field would still work between two version 4 peers and everything
%% here would pass; what would differ is what a 12-pre peer HEARS.
%%
%% `frame_type/3' matches key sets exactly, so a version 3 CONNECT reaching a
%% version 4 station would answer `malformed_frame' — which reads as
%% corruption and sends someone hunting a codec bug. Version 3 already did
%% exactly this to every published client once. `unsupported_version' names
%% the cause and tells an operator what to do.
a_version_three_peer_is_told_the_version(World) ->
    V3 = as_version(connect_of(World, <<>>), 3),
    ?assertMatch({refused, unsupported_version, _}, accept(World, V3)).

%% Empty, not absent. A peer must not be able to tell from the wire whether
%% this node has an endorsement, nor whether the station asks for one.
the_field_is_always_carried(World) ->
    ?assertMatch(#{{text, <<"member_endorsement">>} := <<>>}, decoded(connect_of(World, <<>>))).

an_endorsement_is_carried(World) ->
    ?assertMatch(#{{text, <<"member_endorsement">>} := ?ENDORSEMENT},
                 decoded(connect_of(World, ?ENDORSEMENT))).

%% The SDK's whole job here: put it on the wire and hand it over. What the
%% station then does with it is D31's enforcement, which is not built yet.
the_station_is_handed_the_endorsement(World) ->
    {accepted, Client, _Hello} = accept(World, connect_of(World, ?ENDORSEMENT)),
    ?assertMatch(#{member_endorsement := ?ENDORSEMENT}, Client).

an_empty_endorsement_still_connects(World) ->
    {accepted, Client, _Hello} = accept(World, connect_of(World, <<>>)),
    ?assertMatch(#{member_endorsement := <<>>}, Client).

%% ⛔ THE ADDITIVE PROPERTY, PINNED BY SWAPPING THE FIELD AFTER SIGNING. The
%% proof signs a fixed-length concatenation ending in the hash of the CHALLENGE
%% frame, never of this one, so replacing the endorsement in a signed connect
%% leaves the proof valid and the station still accepts.
%%
%% ⚠ COMPARING THE PROOF BYTES OF TWO CONNECTS DOES NOT TEST THIS, and was the
%% first thing tried here. ML-DSA signing is randomised, so two proofs over the
%% IDENTICAL message differ anyway; the comparison fails whether or not the
%% field is covered, and would have "caught" a bug that is not there while
%% saying nothing about the one that would matter.
%%
%% ⚠ AND THE SWAP BEING POSSIBLE IS NOT A HOLE. D31: the endorsement binds
%% `member_node' to the node_id the proof establishes, so one copied from
%% another member is useless without that member's CONNECT key. The proof does
%% not need to cover the field because the field cannot be used out of place.
%% That is also why the station's check must run AFTER the proof: before it,
%% there is no established node_id to bind against.
the_proof_does_not_cover_the_field(World) ->
    Signed = connect_of(World, ?ENDORSEMENT),
    Swapped = with_endorsement(Signed, <<"a different member's endorsement">>),
    ?assertNotEqual(Signed, Swapped),

    {accepted, Client, _Hello} = accept(World, Swapped),
    ?assertMatch(#{member_endorsement := <<"a different member's endorsement">>}, Client).

%% ⛔ BOTH `macula_dist_tunnel' AND `macula_peering_conn' REFUSE A HANDSHAKE
%% FRAME OVER 65,536 BYTES, from the length header before the body is read. An
%% endorsement is a signed record, so this field is the largest single thing
%% ever added to CONNECT, and if a later field pushes the frame past the cap a
%% tunnel starts refusing frames a peering connection accepts -- an asymmetry
%% found by whoever is unlucky rather than by us.
%%
%% Measured here rather than assumed, both profiles, with a REAL signed
%% `realm_member_endorsement' rather than a stand-in:
%%
%%     pq_pure    endorsement 7,491  connect 19,685 -> 27,178  headroom 38,358
%%     pq_hybrid  endorsement 8,535  connect 22,285 -> 30,822  headroom 34,714
%%
%% So the worst case uses about 47% of the cap and no cap needs to move for
%% D31. This is a guard rather than a note: the next person adding a field to
%% CONNECT finds out here instead of on a tunnel.
a_connect_with_an_endorsement_fits_the_frame_cap() ->
    [fits(Profile) || Profile <- [pq_pure, pq_hybrid]],
    ok.

fits(Profile) ->
    World = world_in(Profile),
    Connect = connect_of(World, real_endorsement(Profile)),
    ?assert(byte_size(Connect) < 65_536),
    %% Half the cap, so this trips while there is still room to think rather
    %% than at the moment something breaks.
    ?assert(byte_size(Connect) < 32_768).

%% A real signed realm_member_endorsement, record type 0x05, the one D31 uses.
%% Its size is dominated by the ML-DSA signature and the realm key, so the
%% roles list barely moves it.
real_endorsement(Profile) ->
    {ok, RealmKey} = macula_node_keys:generate(realm, Profile),
    RealmId = macula_node_keys:node_id(macula_node_keys:public_key(RealmKey), Profile),
    macula_record:encode(
        macula_record:sign(
            macula_record:realm_member_endorsement(
                RealmId, #{realm => RealmId, member_node => <<7:256>>,
                           roles => [<<"member">>, <<"admin">>]}),
            RealmKey)).

%%%===================================================================
%%% Helpers
%%%===================================================================

%% One challenge, reused, so two connects differ only in the field under test:
%% the proof covers the challenge, so a fresh challenge would change the proof
%% for reasons that have nothing to do with the endorsement.
connect_of(#{challenge := Challenge} = World, Endorsement) ->
    {ok, Connect, _Station} =
        macula_handshake:answer_challenge(Challenge, client_session(World, Endorsement)),
    Connect.

accept(#{challenge := Challenge} = World, Connect) ->
    macula_handshake:accept_connect(Connect, station_session(World, Challenge)).

%% ⚠ The raw frame, with its keys still TAGGED as `{text, Key}'. The module's
%% own `decode/3' normalises them; reading the frame here rather than through
%% it is deliberate, so these assertions are about what goes ON THE WIRE and
%% not about what the decoder makes of it afterwards.
%% Re-encodes a frame claiming another protocol version, leaving every other
%% field as it was: what a peer on the older version would actually send,
%% minus the field it does not know about.
as_version(FrameBytes, Version) ->
    {ok, Fields} = macula_record_cbor:decode_strict(FrameBytes),
    macula_cbor_nif:pack_deterministic(
        maps:remove({text, <<"member_endorsement">>},
                    Fields#{{text, <<"version">>} => Version})).

%% Re-encodes a signed connect frame with a different endorsement in it,
%% leaving every other field and the proof untouched.
with_endorsement(FrameBytes, Endorsement) ->
    {ok, Fields} = macula_record_cbor:decode_strict(FrameBytes),
    macula_cbor_nif:pack_deterministic(
        Fields#{{text, <<"member_endorsement">>} => Endorsement}).

decoded(FrameBytes) ->
    {ok, Fields} = macula_record_cbor:decode_strict(FrameBytes),
    Fields.

world() ->
    world_in(configured_profile()).

world_in(Profile) ->
    {ok, StationId} = macula_node_keys:generate(identity, Profile),
    {ok, ClientId} = macula_node_keys:generate(identity, Profile),
    {ok, ClientConnect} = macula_node_keys:generate(connect, Profile),
    ConnectPublic = macula_node_keys:public_key(ClientConnect),
    TlsBinding = macula_key_bindings:tls_binding(StationId, ?LEAF, ?NOW, ?NOW + 7 * ?DAY),
    ConnectBinding = macula_key_bindings:connect_binding(ClientId, ConnectPublic, ?NOW, ?NOW + ?DAY),
    World = #{profile => Profile,
              station_public => macula_node_keys:public_key(StationId),
              client_public => macula_node_keys:public_key(ClientId),
              client_connect => ClientConnect,
              tls_binding => TlsBinding,
              tls_status => macula_key_bindings:status_statement(StationId, TlsBinding, ?NOW, ?NOW + ?HOUR),
              connect_binding => ConnectBinding,
              connect_status => macula_key_bindings:status_statement(ClientId, ConnectBinding, ?NOW,
                                                                     ?NOW + ?HOUR)},
    World#{challenge => macula_handshake:challenge(station_material(World))}.

configured_profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

station_material(#{profile := Profile, station_public := StationPublic, tls_binding := Binding,
                   tls_status := Status}) ->
    #{profile => Profile, identity_key => StationPublic, tls_binding => Binding, tls_status => Status}.

%% The session the SDK answers a challenge from, carrying the endorsement under
%% test. `member_endorsement' is the field D31 adds; everything else is the
%% existing client session.
client_session(#{profile := Profile, station_public := StationPublic, client_public := ClientPublic,
                 client_connect := ClientConnect, connect_binding := Binding, connect_status := Status},
               Endorsement) ->
    #{profile => Profile, expected_node_id => macula_node_keys:node_id(StationPublic, Profile),
      leaf => ?LEAF, identity_key => ClientPublic, connect_key => ClientConnect,
      connect_binding => Binding, connect_status => Status,
      capabilities => ?CLIENT_CAPABILITIES, member_endorsement => Endorsement,
      now => ?NOW + ?MINUTE}.

station_session(#{profile := Profile}, Challenge) ->
    #{profile => Profile, challenge => Challenge, leaf => ?LEAF,
      puzzle => #{difficulty => 0, mode => enforce},
      capabilities => ?STATION_CAPABILITIES, now => ?NOW + ?MINUTE}.
