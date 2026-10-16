%% EUnit tests for macula_record:custodian_refresh/4: a custodian applies a STORE_REFRESH (macula-station#107).
%%
%% A STORE_REFRESH carries only the re-signed expiry delta {version, created_at, expires_at, signature}, never the
%% record payload. The custodian reconstructs the signed tbs over the payload it ALREADY HOLDS with the frame's new
%% version and timestamps, and verifies the frame's signature against the held record's carried key. If it verifies
%% (and passes the ordinary custodian checks: clock, lifetime, class), the custodian has a record byte-identical to
%% the one a full STORE of the publisher's refresh would have produced. On ANY failure it answers `need_full', so
%% the sender falls back to a full STORE: never a lost or a stale record.
%%
%% Every case runs under both crypto profiles (macula-station#107, #83 rule 5): the reconstruct-and-verify is the signature
%% scheme's work, so pq_pure (ML-DSA-87) and pq_hybrid (ML-DSA-87 + RSA-4096-PSS, the live fleet profile) are both
%% exercised.
-module(macula_record_custodian_refresh_tests).

-include_lib("eunit/include/eunit.hrl").

%% The label macula_signed_object:sign/3 signs a record under (macula_record's ?LABEL).
-define(RECORD_LABEL, <<"MACULA-PQ-RECORD-V1">>).

both_profiles_test_() ->
    {timeout, 120,
     [{atom_to_list(Profile) ++ ": " ++ Name, fun() -> Case(Profile) end}
      || Profile <- [pq_pure, pq_hybrid],
         {Name, Case} <- cases()]}.

cases() ->
    [{"a refresh reconstructs the publisher's new version", fun reconstructs_new_version/1},
     {"a reconstructed record is wire-identical and verifies", fun wire_identical_and_verifies/1},
     {"a tampered signature needs the full record", fun tampered_signature_needs_full/1},
     {"a delta for another payload needs the full record", fun other_payload_needs_full/1},
     {"a refresh past the lifetime class needs the full record", fun past_lifetime_needs_full/1},
     {"an unknown-builtin-tag held record refreshes, never crashes", fun unknown_tag_held_refreshes/1},
     {"a tombstone of an unknown tag refreshes, never crashes", fun tombstone_held_refreshes/1},
     {"an out-of-range timestamp needs the full record, never crashes", fun out_of_range_timestamp_needs_full/1},
     {"held WIRE BYTES refresh to the publisher's new version", fun held_bytes_reconstructs/1},
     {"held bytes refresh identically to the decoded held map", fun held_bytes_equiv_map/1},
     {"held bytes with an out-of-range delta need the full record", fun held_bytes_out_of_range_needs_full/1},
     {"undecodable held bytes need the full record, never crash", fun held_garbage_needs_full/1}].

%% The happy path: the publisher refreshes a record it signed; the custodian, holding the original, applies the
%% delta and ends with the publisher's new version.
reconstructs_new_version(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    Refreshed = macula_record:refresh(Held, Key),
    {ok, Applied} = macula_record:custodian_refresh(Held, delta(Refreshed), Profile, now_ms()),
    ?assertEqual(macula_record:version(Refreshed), macula_record:version(Applied)),
    ?assertEqual(macula_record:created_at(Refreshed), macula_record:created_at(Applied)),
    ?assertEqual(macula_record:expires_at(Refreshed), macula_record:expires_at(Applied)),
    ?assertEqual(macula_record:signature(Refreshed), macula_record:signature(Applied)).

%% The reconstructed record is identical on the wire to a full STORE of the refreshed record, and verifies on its
%% own: the custodian stores exactly what it would have stored the expensive way.
wire_identical_and_verifies(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    Refreshed = macula_record:refresh(Held, Key),
    {ok, Applied} = macula_record:custodian_refresh(Held, delta(Refreshed), Profile, now_ms()),
    ?assertEqual(macula_record:encode(Refreshed), macula_record:encode(Applied)),
    ?assertMatch({ok, #{}}, macula_record:verify(macula_record:encode(Applied), Profile)).

%% A tampered signature cannot be made to verify over the held payload, so the custodian asks for the full record.
tampered_signature_needs_full(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    Refreshed = macula_record:refresh(Held, Key),
    Delta = (delta(Refreshed))#{signature => flip_first_byte(macula_record:signature(Refreshed))},
    ?assertEqual(need_full, macula_record:custodian_refresh(Held, Delta, Profile, now_ms())).

%% The security property of a payload-less refresh: a delta signed over one payload does not apply to a custodian
%% holding a DIFFERENT payload. The reconstructed tbs differs, so the signature fails and the answer is need_full.
%% This is what stops a refresh from silently leaving a custodian serving a stale record.
other_payload_needs_full(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key, <<"acme/echo_v1">>), Key), Profile),
    Other = macula_record:refresh(advertisement(Key, <<"acme/other_v1">>), Key),
    ?assertEqual(need_full, macula_record:custodian_refresh(Held, delta(Other), Profile, now_ms())).

%% A refresh whose expiry would run past the type's lifetime class is refused the same way a full STORE would be:
%% custodian_verify fails the lifetime bound, so the answer is need_full rather than a stored over-long record.
past_lifetime_needs_full(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    Refreshed = macula_record:refresh(Held, Key),
    %% Stretch the expiry a day past created_at: far beyond the 5-minute advertisement class. The signature no
    %% longer covers these timestamps, so this also exercises the first-failure-wins ordering ending in need_full.
    Delta = (delta(Refreshed))#{expires_at => macula_record:created_at(Refreshed) + 86_400_000},
    ?assertEqual(need_full, macula_record:custodian_refresh(Held, Delta, Profile, now_ms())).

%% A STORE_REFRESH's created_at/expires_at are validated on the wire by the `non_neg' rule, which reaches 2^64-1,
%% while a record timestamp is bounded by 2^53. A decoded frame carrying a larger value must answer need_full, not
%% crash the custodian (macula-station#107, Fable round 2). 2^53 is the first value a record cannot carry.
out_of_range_timestamp_needs_full(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    Refreshed = macula_record:refresh(Held, Key),
    OutOfRange = 1 bsl 53,
    Delta = (delta(Refreshed))#{created_at => OutOfRange, expires_at => OutOfRange},
    ?assertEqual(need_full, macula_record:custodian_refresh(Held, Delta, Profile, now_ms())).

%% A custodian STORES records whose built-in tag wire 1.0 does not define (row_matches(none, _) -> true), whose type
%% has no lifetime-class row. custodian_refresh must reconstruct such a record from the HELD class, not re-derive it,
%% or it crashes instead of answering need_full -- a wire-reachable fault, since the delta is unauthenticated until
%% verify (macula-station#107, Fable round 1). Built at the signed-object layer, as macula_record signs no unknown tag.
unknown_tag_held_refreshes(Profile) ->
    Now = now_ms(),
    Base = #{{text, <<"type">>} => 16#18, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
             {text, <<"schema">>} => 1, {text, <<"lifetime_class">>} => 3, {text, <<"created_at">>} => Now,
             {text, <<"expires_at">>} => Now + 3_600_000, {text, <<"payload">>} => #{{text, <<"a">>} => 1},
             {text, <<"subject">>} => <<"subject">>},
    refresh_roundtrip(Profile, Base, Now).

%% A tombstone (tag 0x0C) withdrawing an unknown tag is stored too, and derives its class from the withdrawn type, so
%% it hits the same re-derivation crash. Pin that it refreshes rather than crashes.
tombstone_held_refreshes(Profile) ->
    Now = now_ms(),
    Payload = #{{text, <<"withdrawn_type">>} => 16#18, {text, <<"withdrawn_version">>} => <<1:128>>,
                {text, <<"reason">>} => {text, <<"revoked">>}, {text, <<"subject">>} => <<"subject">>},
    Base = #{{text, <<"type">>} => 16#0C, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
             {text, <<"schema">>} => 1, {text, <<"lifetime_class">>} => 1, {text, <<"created_at">>} => Now,
             {text, <<"expires_at">>} => Now + 60_000, {text, <<"payload">>} => Payload},
    refresh_roundtrip(Profile, Base, Now).

%% Sign Base as a custodian would receive and store it, then apply the publisher's own legitimate refresh (same
%% Base, new version and timestamps): custodian_refresh returns {ok, _}; a tampered signature answers need_full.
refresh_roundtrip(Profile, Base, Now) ->
    Key = key(Profile),
    {ok, Held} = macula_record:custodian_verify(signed(Base, Key), Profile, Now),
    Later = Now + 1000,
    Expires = maps:get({text, <<"expires_at">>}, Base) + 1000,
    Obj = macula_signed_object:sign(?RECORD_LABEL,
            Base#{{text, <<"version">>} => macula_record_uuid:v7_monotonic(Later),
                  {text, <<"created_at">>} => Later, {text, <<"expires_at">>} => Expires}, Key),
    {ok, Fields} = macula_record_cbor:decode_strict(maps:get(tbs, Obj)),
    Delta = #{version => maps:get({text, <<"version">>}, Fields), created_at => Later, expires_at => Expires,
              signature => maps:get(signature, Obj)},
    ?assertMatch({ok, _}, macula_record:custodian_refresh(Held, Delta, Profile, Later)),
    ?assertEqual(need_full,
                 macula_record:custodian_refresh(Held, Delta#{signature => flip_first_byte(maps:get(signature, Obj))},
                                                  Profile, Later)).

signed(Base, Key) ->
    macula_signed_object:encode(macula_signed_object:sign(?RECORD_LABEL, Base, Key)).

%%------------------------------------------------------------------
%% Held as WIRE BYTES (macula-station#107 lever 1, Fable Design 3): a custodian applies a refresh against the record
%% bytes it holds in its slot, without first re-verifying them. custodian_refresh/4 decodes them with no signature
%% step and refreshes through the map clause, so the behaviour is identical to the decoded map and the forgery bar is
%% unchanged (the delta's signature is still verified over the reconstructed tbs).
%%------------------------------------------------------------------

%% The happy path through the bytes form: the custodian holds the original's wire bytes and applies the publisher's
%% refresh delta, ending with a record wire-identical to a full STORE of the refresh.
held_bytes_reconstructs(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    Refreshed = macula_record:refresh(Held, Key),
    {ok, Applied} = macula_record:custodian_refresh(macula_record:encode(Held), delta(Refreshed), Profile, now_ms()),
    ?assertEqual(macula_record:encode(Refreshed), macula_record:encode(Applied)),
    ?assertMatch({ok, #{}}, macula_record:verify(macula_record:encode(Applied), Profile)).

%% Required equivalence (Fable Design 3, round 1): refreshing the WIRE BYTES equals refreshing the record
%% custodian_verify decodes from them, for a record WITHOUT a subject (a procedure advertisement) and one WITH a
%% subject (an unknown builtin tag). Pins that the bytes clause reconstructs the same held fields the verified decode
%% does, so the only difference is the skipped signature check on bytes already admitted.
held_bytes_equiv_map(Profile) ->
    Now = now_ms(),
    %% Without a subject.
    Key = key(Profile),
    {ok, HeldNoSubj} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    NoSubjBytes = macula_record:encode(HeldNoSubj),
    DeltaNoSubj = delta(macula_record:refresh(HeldNoSubj, Key)),
    assert_bytes_equiv_map(NoSubjBytes, DeltaNoSubj, Profile, Now),
    %% With a subject (unknown builtin tag 0x18, which carries one).
    Later = Now + 1000,
    Base = #{{text, <<"type">>} => 16#18, {text, <<"version">>} => macula_record_uuid:v7_monotonic(Now),
             {text, <<"schema">>} => 1, {text, <<"lifetime_class">>} => 3, {text, <<"created_at">>} => Now,
             {text, <<"expires_at">>} => Now + 3_600_000, {text, <<"payload">>} => #{{text, <<"a">>} => 1},
             {text, <<"subject">>} => <<"subject">>},
    SubjBytes = signed(Base, Key),
    Expires = maps:get({text, <<"expires_at">>}, Base) + 1000,
    Obj = macula_signed_object:sign(?RECORD_LABEL,
            Base#{{text, <<"version">>} => macula_record_uuid:v7_monotonic(Later),
                  {text, <<"created_at">>} => Later, {text, <<"expires_at">>} => Expires}, Key),
    {ok, Fields} = macula_record_cbor:decode_strict(maps:get(tbs, Obj)),
    DeltaSubj = #{version => maps:get({text, <<"version">>}, Fields), created_at => Later, expires_at => Expires,
                  signature => maps:get(signature, Obj)},
    assert_bytes_equiv_map(SubjBytes, DeltaSubj, Profile, Later).

%% custodian_refresh on the bytes equals custodian_refresh on element(2, custodian_verify(bytes)); both must succeed,
%% so the equivalence is pinned on a reconstruction that actually happened, not on two matching need_fulls.
assert_bytes_equiv_map(Bytes, Delta, Profile, Now) ->
    {ok, Decoded} = macula_record:custodian_verify(Bytes, Profile, Now),
    ?assertMatch({ok, _}, macula_record:custodian_refresh(Bytes, Delta, Profile, Now)),
    ?assertEqual(macula_record:custodian_refresh(Decoded, Delta, Profile, Now),
                 macula_record:custodian_refresh(Bytes, Delta, Profile, Now)).

%% A delta whose created_at/expires_at exceed the record range (a wire non_neg reaches 2^64-1) answers need_full
%% through the bytes form too, by delegating to the map clause's out-of-range fallback rather than crashing.
held_bytes_out_of_range_needs_full(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    Refreshed = macula_record:refresh(Held, Key),
    OutOfRange = (1 bsl 64) - 1,
    Delta = (delta(Refreshed))#{created_at => OutOfRange, expires_at => OutOfRange},
    ?assertEqual(need_full,
                 macula_record:custodian_refresh(macula_record:encode(Held), Delta, Profile, now_ms())).

%% Held bytes that do not decode to a readable record answer need_full and never raise: empty, random, oversized, and
%% a held-only object (tbs+signature, no key). The frame is reachable from the wire on the DHT receive path.
held_garbage_needs_full(Profile) ->
    Key = key(Profile),
    {ok, Held} = verified(macula_record:refresh(advertisement(Key), Key), Profile),
    Delta = delta(macula_record:refresh(Held, Key)),
    Now = now_ms(),
    Oversize = binary:copy(<<0>>, (256 * 1024) + 1),
    HeldOnly = macula_signed_object:encode(macula_signed_object:sign_held(?RECORD_LABEL, record_tbs(Held), Key)),
    [?assertEqual(need_full, macula_record:custodian_refresh(Bytes, Delta, Profile, Now))
     || Bytes <- [<<>>, <<"not a record">>, crypto:strong_rand_bytes(64), Oversize, HeldOnly]].

%% The tbs field map of a held record, to re-sign as a held-only object (no key) for the garbage case.
record_tbs(Record) ->
    {ok, Fields} = macula_record_cbor:decode_strict(maps:get(tbs, Record)),
    maps:without([{text, <<"alg">>}], Fields).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

delta(Record) ->
    #{version => macula_record:version(Record),
      created_at => macula_record:created_at(Record),
      expires_at => macula_record:expires_at(Record),
      signature => macula_record:signature(Record)}.

verified(Signed, Profile) ->
    macula_record:verify(macula_record:encode(Signed), Profile).

advertisement(Key) ->
    advertisement(Key, <<"acme/echo_v1">>).

advertisement(Key, Procedure) ->
    {ok, NodeId} = macula_node_keys:node_id(Key),
    macula_record:procedure_advertisement(NodeId, <<1:256>>, Procedure, [<<7:256>>]).

flip_first_byte(<<B, Rest/binary>>) -> <<(B bxor 1), Rest/binary>>.

now_ms() ->
    erlang:system_time(millisecond).

%% One key per profile, cached: pq_hybrid keygen (RSA-4096) is slow, so generate each at most once per run.
key(Profile) ->
    case persistent_term:get({?MODULE, key, Profile}, undefined) of
        undefined ->
            {ok, Key} = macula_node_keys:generate(identity, Profile),
            persistent_term:put({?MODULE, key, Profile}, Key),
            Key;
        Key ->
            Key
    end.
