%% EUnit tests for macula_hyparview_endorsement.
-module(macula_hyparview_endorsement_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Verify endorsement
%%---------------------------------------------------------------------

accepts_valid_endorsement_test() ->
    {AdminKp, RealmId} = admin(),
    Member = crypto:strong_rand_bytes(32),
    R = signed_endorsement(AdminKp, RealmId, Member, [<<"peer">>]),
    ?assertEqual({ok, [<<"peer">>]},
                 macula_hyparview_endorsement:verify_endorsement(R, RealmId, Member)).

accepts_multiple_roles_test() ->
    {AdminKp, RealmId} = admin(),
    Member = crypto:strong_rand_bytes(32),
    R = signed_endorsement(AdminKp, RealmId, Member,
                           [<<"peer">>, <<"directory">>, <<"replica">>]),
    ?assertEqual({ok, [<<"peer">>, <<"directory">>, <<"replica">>]},
                 macula_hyparview_endorsement:verify_endorsement(R, RealmId, Member)).

rejects_unsigned_endorsement_test() ->
    {_AdminKp, RealmId} = admin(),
    Member = crypto:strong_rand_bytes(32),
    Unsigned = macula_record:realm_member_endorsement(
                 RealmId,
                 #{realm => RealmId, member_node => Member, roles => []}),
    %% macula_record:verify returns {error, bad_record} for unsigned.
    ?assertEqual({error, bad_record},
                 macula_hyparview_endorsement:verify_endorsement(Unsigned, RealmId, Member)).

rejects_wrong_signer_test() ->
    {_AdminKp, RealmId} = admin(),
    Impostor = macula_identity:generate(),
    Member = crypto:strong_rand_bytes(32),
    R = macula_record:realm_member_endorsement(
          RealmId,
          #{realm => RealmId, member_node => Member, roles => [<<"peer">>]}),
    %% Impostor signs — signature verifies against payload's realm key,
    %% which is the admin key, so the signature check must fail.
    Bad = macula_record:sign(R, Impostor),
    ?assertEqual({error, signature_invalid},
                 macula_hyparview_endorsement:verify_endorsement(Bad, RealmId, Member)).

rejects_wrong_type_test() ->
    {AdminKp, RealmId} = admin(),
    %% realm_directory is a different type (0x03).
    R = macula_record:sign(macula_record:realm_directory(
                             RealmId, <<"demo">>, RealmId), AdminKp),
    Member = crypto:strong_rand_bytes(32),
    ?assertEqual({error, wrong_type},
                 macula_hyparview_endorsement:verify_endorsement(R, RealmId, Member)).

rejects_wrong_member_test() ->
    {AdminKp, RealmId} = admin(),
    Real  = crypto:strong_rand_bytes(32),
    Fake  = crypto:strong_rand_bytes(32),
    R = signed_endorsement(AdminKp, RealmId, Real, [<<"peer">>]),
    ?assertEqual({error, wrong_member},
                 macula_hyparview_endorsement:verify_endorsement(R, RealmId, Fake)).

rejects_wrong_realm_test() ->
    {AdminKp, RealmId} = admin(),
    {_, OtherRealm}    = admin(),
    Member = crypto:strong_rand_bytes(32),
    R = signed_endorsement(AdminKp, RealmId, Member, []),
    ?assertEqual({error, wrong_realm},
                 macula_hyparview_endorsement:verify_endorsement(R, OtherRealm, Member)).

rejects_not_yet_valid_test() ->
    {AdminKp, RealmId} = admin(),
    Member = crypto:strong_rand_bytes(32),
    Future = erlang:system_time(millisecond) + 60 * 1000,
    R0 = macula_record:realm_member_endorsement(
           RealmId,
           #{realm => RealmId, member_node => Member, roles => []},
           #{valid_from => Future, valid_until => Future + 1000}),
    R = macula_record:sign(R0, AdminKp),
    ?assertEqual({error, not_yet_valid},
                 macula_hyparview_endorsement:verify_endorsement(R, RealmId, Member)).

rejects_endorsement_expired_test() ->
    {AdminKp, RealmId} = admin(),
    Member = crypto:strong_rand_bytes(32),
    NowMs = erlang:system_time(millisecond),
    %% valid_until is in the past; but record envelope itself must
    %% not be expired — give envelope a fresh 5min ttl via opts.
    R0 = macula_record:realm_member_endorsement(
           RealmId,
           #{realm => RealmId, member_node => Member, roles => []},
           #{valid_from  => NowMs - 10_000,
             valid_until => NowMs - 5_000,
             ttl_ms      => 5 * 60 * 1000}),
    R = macula_record:sign(R0, AdminKp),
    ?assertEqual({error, endorsement_expired},
                 macula_hyparview_endorsement:verify_endorsement(R, RealmId, Member)).

%%---------------------------------------------------------------------
%% Build join frame
%%---------------------------------------------------------------------

build_join_frame_is_signed_hyparview_join_test() ->
    {AdminKp, RealmId} = admin(),
    MemberKp = macula_identity:generate(),
    Member   = macula_identity:public(MemberKp),
    Endorsement = signed_endorsement(AdminKp, RealmId, Member, [<<"peer">>]),
    F = macula_hyparview_endorsement:build_join(RealmId, Member, Endorsement, MemberKp),
    ?assertEqual(hyparview_join, macula_frame:frame_type(F)),
    ?assertEqual(RealmId, maps:get(realm, F)),
    ?assertEqual(Member,  maps:get(new_member, F)),
    %% Signed by the joining member → verifies against Member pubkey.
    ?assertMatch({ok, _}, macula_frame:verify(F, Member)).

%% THE gap that shipped broken: build_join computed the endorsement
%% and then discarded it (`_Encoded = macula_record:encode(Endorsement)`,
%% never attached to the frame). Assert it actually travels and is
%% independently verifiable on the far side -- not just that the frame
%% has the right type/realm/new_member, which the test above already
%% covered without ever catching this.
build_join_frame_carries_a_verifiable_endorsement_test() ->
    {AdminKp, RealmId} = admin(),
    MemberKp = macula_identity:generate(),
    Member   = macula_identity:public(MemberKp),
    Endorsement = signed_endorsement(AdminKp, RealmId, Member, [<<"peer">>]),
    F = macula_hyparview_endorsement:build_join(RealmId, Member, Endorsement, MemberKp),
    ?assertEqual(Endorsement, maps:get(record, F)),
    %% Round-trip through the wire, exactly as a receiving peer would.
    {ok, D, <<>>} = macula_frame:decode(macula_frame:encode(F)),
    ?assertEqual(Endorsement, maps:get(record, D)),
    ?assertMatch({ok, [<<"peer">>]},
                 macula_hyparview_endorsement:verify_endorsement(
                   maps:get(record, D), RealmId, Member)).

%%=====================================================================
%% Helpers
%%=====================================================================

admin() ->
    Kp = macula_identity:generate(),
    {Kp, macula_identity:public(Kp)}.

signed_endorsement(AdminKp, RealmId, Member, Roles) ->
    R = macula_record:realm_member_endorsement(
          RealmId,
          #{realm => RealmId, member_node => Member, roles => Roles}),
    macula_record:sign(R, AdminKp).
