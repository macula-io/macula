%% @doc Realm-join handshake helpers (Phase 5.6).
%%
%% Building block for the admission flow of a new station into a realm. The station presents a
%% realm_member_endorsement record signed with the realm key, and the receiving peers verify it against the realm key
%% id they trust for that realm before admitting the station into the HyParView active or passive view.
%%
%% This module is pure: it does not talk to the network. Callers (typically the per-station dispatcher) feed the
%% endorsement, as its wire form as received, into verify_endorsement/3 and act on the outcome.
%%
%% == Acceptance rules ==
%%
%% <ul>
%%   <li>The record verifies under the verifier's profile (macula_record:verify/2): its carried key, its signature, and
%%       its created_at and expires_at.</li>
%%   <li>Its type is the realm member endorsement (0x05).</li>
%%   <li>Its signer's key id is the realm key id the node trusts for the realm.</li>
%%   <li>The payload realm_id equals the expected realm id.</li>
%%   <li>The payload member_node equals the node_id the joining peer claims, so no peer can present another member's
%%       endorsement.</li>
%%   <li>valid_from is at most now and valid_until at least now: the endorsement is active.</li>
%% </ul>
%%
%% Reference: plans/PLAN_MACULA_V2_PART6_PROTOCOL.md §9.6.
-module(macula_hyparview_endorsement).

-export([verify_endorsement/3, build_join/3]).

-export_type([trust/0, verify_error/0]).

-define(TYPE_REALM_MEMBER_ENDORSEMENT, 16#05).

-type realm()   :: <<_:256>>.
-type node_id() :: <<_:256>>.

%% What a node trusts for a realm: its crypto profile, the realm id, and the key id of the realm key.
-type trust() :: #{profile := macula_crypto_profile:profile(), realm := realm(), realm_key_id := <<_:256>>}.

-type verify_error() ::
        record_too_large | malformed | signature_invalid | alg_mismatch | not_yet_valid | expired | key_id_mismatch
      | wrong_type
      | untrusted_signer
      | wrong_realm
      | wrong_member
      | endorsement_expired.

%% @doc Verify that an endorsement, as its wire form or its {key, tbs, signature} map, admits Member to the realm that
%% Trust names. Returns {ok, Roles} with the endorsed roles, or {error, Reason}; callers treat any error as a refusal
%% and drop the pending join.
-spec verify_endorsement(binary() | map(), trust(), node_id()) -> {ok, [binary()]} | {error, verify_error()}.
verify_endorsement(Signed, #{profile := Profile, realm := <<_:256>>, realm_key_id := <<_:256>>} = Trust,
                   <<_:256>> = Member) ->
    verified(macula_record:verify(Signed, Profile), Trust, Member).

verified({ok, #{type := ?TYPE_REALM_MEMBER_ENDORSEMENT} = Record}, Trust, Member) ->
    check_signer(Record, Trust, Member);
verified({ok, _OtherType}, _Trust, _Member) ->
    {error, wrong_type};
verified({error, _} = Refusal, _Trust, _Member) ->
    Refusal.

check_signer(#{key_id := KeyId} = Record, #{realm_key_id := KeyId, realm := Realm}, Member) ->
    check_realm(Record, Realm, Member);
check_signer(_Record, _Trust, _Member) ->
    {error, untrusted_signer}.

check_realm(#{payload := #{{text, <<"realm_id">>} := Realm}} = Record, Realm, Member) ->
    check_member(Record, Member);
check_realm(_Record, _Realm, _Member) ->
    {error, wrong_realm}.

check_member(#{payload := #{{text, <<"member_node">>} := Member} = Payload}, Member) ->
    check_window(maps:get({text, <<"valid_from">>}, Payload, undefined),
                 maps:get({text, <<"valid_until">>}, Payload, undefined),
                 erlang:system_time(millisecond), Payload);
check_member(_Record, _Member) ->
    {error, wrong_member}.

check_window(From, Until, Now, Payload) when is_integer(From), is_integer(Until), From =< Now, Now =< Until ->
    endorsed_roles(maps:get({text, <<"roles">>}, Payload, []));
check_window(From, _Until, Now, _Payload) when is_integer(From), Now < From ->
    {error, not_yet_valid};
check_window(_From, _Until, _Now, _Payload) ->
    {error, endorsement_expired}.

endorsed_roles(Roles) when is_list(Roles) -> {ok, [Role || {text, Role} <- Roles]};
endorsed_roles(_NotAList) -> {error, malformed}.

%% @doc Build the JOIN frame a joining station sends to one of the realm's known stations, carrying its endorsement as
%% the record's wire form. The frame carries no signature of its own: neighbour signatures belong to the connection
%% (D17).
-spec build_join(realm(), node_id(), binary()) -> macula_frame:frame().
build_join(<<_:256>> = Realm, <<_:256>> = NewMember, Endorsement) when is_binary(Endorsement) ->
    macula_frame:hyparview_join(#{realm => Realm, new_member => NewMember, record => Endorsement}).
