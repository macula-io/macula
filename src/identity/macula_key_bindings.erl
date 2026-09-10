%% @doc Bindings of a node's TLS and CONNECT keys to its identity key, and the status statements that keep a binding
%% in force, as plans/DESIGN_PQ_HANDSHAKE_FRAMES.md lays them out (decisions D16 and D22).
%%
%% A binding or a statement travels as `#{tbs => Bytes, signature => Signature}'. The identity key signs
%% Label || 0x00 || tbs. A verifier checks that signature over the tbs bytes it received, and only then decodes them
%% with `macula_record_cbor:decode_strict/1': a duplicate key, bytes after the map, an unknown key, or a field of the
%% wrong type or length is refused as `malformed_frame'. Validity is checked with 5 minutes of clock tolerance.
%%
%% A binding for one use never verifies as the other use, because its signature covers the other use's label.
-module(macula_key_bindings).

-export([
    tls_binding/4,
    connect_binding/4,
    status_statement/4,
    verify_tls_binding/5,
    verify_connect_binding/5,
    verify_status/5
]).

-export_type([envelope/0, refusal/0]).

-type envelope() :: #{tbs := binary(), signature := binary()}.
-type refusal()  :: malformed_frame
                  | binding_signature_invalid
                  | binding_wrong_use
                  | binding_key_mismatch
                  | binding_expired
                  | binding_not_yet_valid
                  | node_id_mismatch
                  | status_signature_invalid
                  | status_binding_mismatch
                  | status_expired
                  | status_future_dated.

-define(TLS_LABEL, <<"MACULA-PQ-BINDING-TLS-V1">>).
-define(CONNECT_LABEL, <<"MACULA-PQ-BINDING-CONNECT-V1">>).
-define(STATUS_LABEL, <<"MACULA-PQ-STATUS-V1">>).
-define(MAX_BINDING_MS, 7 * 86400000).
-define(MAX_STATUS_MS, 3600000).
-define(TOLERANCE_MS, 5 * 60000).
-define(BINDING_KEYS, [<<"binding_id">>, <<"hash_alg">>, <<"label">>, <<"node_id">>, <<"not_after">>,
                       <<"not_before">>, <<"sig_alg">>, <<"subject_hash">>, <<"use">>]).
-define(STATUS_KEYS, [<<"binding_hash">>, <<"expires_at">>, <<"issued_at">>, <<"label">>, <<"node_id">>,
                      <<"sig_alg">>]).

%%------------------------------------------------------------------
%% Issuing
%%------------------------------------------------------------------

%% @doc Bind the TLS key of the leaf certificate a listener presents, by the SHA-384 of that leaf's DER.
-spec tls_binding(macula_node_keys:node_key(), binary(), non_neg_integer(), non_neg_integer()) -> envelope().
tls_binding(IdentityKey, LeafDer, NotBefore, NotAfter) when is_binary(LeafDer) ->
    binding(IdentityKey, tls, ?TLS_LABEL, crypto:hash(sha384, LeafDer), NotBefore, NotAfter).

%% @doc Bind a CONNECT key, by the SHA-384 of the key as carried.
-spec connect_binding(macula_node_keys:node_key(), binary(), non_neg_integer(), non_neg_integer()) -> envelope().
connect_binding(IdentityKey, ConnectKey, NotBefore, NotAfter) when is_binary(ConnectKey) ->
    binding(IdentityKey, connect, ?CONNECT_LABEL, crypto:hash(sha384, ConnectKey), NotBefore, NotAfter).

%% @doc A status statement that keeps a binding in force from IssuedAt to ExpiresAt, at most one hour.
-spec status_statement(macula_node_keys:node_key(), envelope(), non_neg_integer(), non_neg_integer()) -> envelope().
status_statement(#{purpose := identity, profile := Profile} = IdentityKey, #{tbs := BindingTbs}, IssuedAt, ExpiresAt)
  when is_integer(IssuedAt), is_integer(ExpiresAt), IssuedAt >= 0, IssuedAt =< ExpiresAt,
       ExpiresAt - IssuedAt =< ?MAX_STATUS_MS ->
    Tbs = macula_record_cbor:encode(#{
        {text, <<"label">>}        => {text, ?STATUS_LABEL},
        {text, <<"node_id">>}      => identity_node_id(IdentityKey),
        {text, <<"binding_hash">>} => crypto:hash(sha384, BindingTbs),
        {text, <<"issued_at">>}    => IssuedAt,
        {text, <<"expires_at">>}   => ExpiresAt,
        {text, <<"sig_alg">>}      => {text, sig_alg(Profile)}
    }),
    signed(IdentityKey, ?STATUS_LABEL, Tbs).

binding(#{purpose := identity, profile := Profile} = IdentityKey, Use, Label, SubjectHash, NotBefore, NotAfter)
  when is_integer(NotBefore), is_integer(NotAfter), NotBefore >= 0, NotBefore =< NotAfter,
       NotAfter - NotBefore =< ?MAX_BINDING_MS ->
    Tbs = macula_record_cbor:encode(#{
        {text, <<"label">>}        => {text, Label},
        {text, <<"node_id">>}      => identity_node_id(IdentityKey),
        {text, <<"use">>}          => {text, atom_to_binary(Use)},
        {text, <<"subject_hash">>} => SubjectHash,
        {text, <<"binding_id">>}   => crypto:strong_rand_bytes(16),
        {text, <<"not_before">>}   => NotBefore,
        {text, <<"not_after">>}    => NotAfter,
        {text, <<"hash_alg">>}     => {text, <<"SHA-384">>},
        {text, <<"sig_alg">>}      => {text, sig_alg(Profile)}
    }),
    signed(IdentityKey, Label, Tbs).

identity_node_id(IdentityKey) ->
    {ok, NodeId} = macula_node_keys:node_id(IdentityKey),
    NodeId.

signed(IdentityKey, Label, Tbs) ->
    #{tbs => Tbs, signature => macula_node_keys:sign([Label, 0, Tbs], IdentityKey)}.

%%------------------------------------------------------------------
%% Verifying bindings
%%------------------------------------------------------------------

%% @doc Verify a TLS-key binding against the carried identity key and the leaf this connection presented.
-spec verify_tls_binding(term(), binary(), macula_crypto_profile:profile(), binary(), integer()) ->
        {ok, #{use := tls | connect, node_id := <<_:256>>, not_after := non_neg_integer()}} | {error, refusal()}.
verify_tls_binding(Envelope, IdentityPublic, Profile, LeafDer, NowMs) when is_binary(LeafDer) ->
    verify_binding(Envelope, IdentityPublic, Profile, {tls, ?TLS_LABEL, crypto:hash(sha384, LeafDer)}, NowMs).

%% @doc Verify a CONNECT-key binding against the carried identity key and the carried CONNECT key.
-spec verify_connect_binding(term(), binary(), macula_crypto_profile:profile(), binary(), integer()) ->
        {ok, #{use := tls | connect, node_id := <<_:256>>, not_after := non_neg_integer()}} | {error, refusal()}.
verify_connect_binding(Envelope, IdentityPublic, Profile, ConnectKey, NowMs) when is_binary(ConnectKey) ->
    verify_binding(Envelope, IdentityPublic, Profile, {connect, ?CONNECT_LABEL, crypto:hash(sha384, ConnectKey)},
                   NowMs).

verify_binding(#{tbs := Tbs, signature := Signature} = Envelope, IdentityPublic, Profile, Expected, NowMs)
  when map_size(Envelope) =:= 2, is_binary(Tbs), is_binary(Signature), is_binary(IdentityPublic) ->
    {_Use, Label, _SubjectHash} = Expected,
    binding_signed(macula_node_keys:verify([Label, 0, Tbs], Signature, IdentityPublic, Profile),
                   Tbs, IdentityPublic, Profile, Expected, NowMs);
verify_binding(_Envelope, _IdentityPublic, _Profile, _Expected, _NowMs) ->
    {error, malformed_frame}.

binding_signed(false, _Tbs, _IdentityPublic, _Profile, _Expected, _NowMs) ->
    {error, binding_signature_invalid};
binding_signed(true, Tbs, IdentityPublic, Profile, Expected, NowMs) ->
    binding_decoded(decode_fields(Tbs, ?BINDING_KEYS), IdentityPublic, Profile, Expected, NowMs).

binding_decoded(error, _IdentityPublic, _Profile, _Expected, _NowMs) ->
    {error, malformed_frame};
binding_decoded({ok, F}, IdentityPublic, Profile, {Use, Label, SubjectHash}, NowMs) ->
    Checked = run_checks([
        fun() -> well_formed_binding(F, Profile) end,
        fun() -> expect(maps:get(<<"label">>, F) =:= {text, Label} andalso
                        maps:get(<<"use">>, F) =:= {text, atom_to_binary(Use)}, binding_wrong_use) end,
        fun() -> expect(maps:get(<<"node_id">>, F) =:= macula_node_keys:node_id(IdentityPublic, Profile),
                        node_id_mismatch) end,
        fun() -> expect(maps:get(<<"subject_hash">>, F) =:= SubjectHash, binding_key_mismatch) end,
        fun() -> expect(NowMs + ?TOLERANCE_MS >= maps:get(<<"not_before">>, F), binding_not_yet_valid) end,
        fun() -> expect(NowMs - ?TOLERANCE_MS =< maps:get(<<"not_after">>, F), binding_expired) end
    ]),
    binding_result(Checked, F, Use).

binding_result(ok, F, Use) ->
    {ok, #{use => Use, node_id => maps:get(<<"node_id">>, F), not_after => maps:get(<<"not_after">>, F)}};
binding_result({error, _} = Error, _F, _Use) ->
    Error.

well_formed_binding(#{<<"label">> := {text, _}, <<"node_id">> := NodeId, <<"use">> := {text, _},
                      <<"subject_hash">> := SubjectHash, <<"binding_id">> := BindingId,
                      <<"not_before">> := NotBefore, <<"not_after">> := NotAfter,
                      <<"hash_alg">> := {text, <<"SHA-384">>}, <<"sig_alg">> := {text, SigAlg}}, Profile)
  when is_binary(NodeId), byte_size(NodeId) =:= 32, is_binary(SubjectHash), byte_size(SubjectHash) =:= 48,
       is_binary(BindingId), byte_size(BindingId) =:= 16, is_integer(NotBefore), is_integer(NotAfter),
       NotBefore >= 0, NotBefore =< NotAfter, NotAfter - NotBefore =< ?MAX_BINDING_MS ->
    expect(SigAlg =:= sig_alg(Profile), malformed_frame);
well_formed_binding(_Fields, _Profile) ->
    {error, malformed_frame}.

%%------------------------------------------------------------------
%% Verifying status statements
%%------------------------------------------------------------------

%% @doc Verify a status statement for the binding it came with, against the carried identity key, and return when
%% the statement expires.
-spec verify_status(term(), envelope(), binary(), macula_crypto_profile:profile(), integer()) ->
        {ok, #{expires_at := non_neg_integer()}} | {error, refusal()}.
verify_status(#{tbs := Tbs, signature := Signature} = Envelope, #{tbs := BindingTbs}, IdentityPublic, Profile, NowMs)
  when map_size(Envelope) =:= 2, is_binary(Tbs), is_binary(Signature), is_binary(BindingTbs),
       is_binary(IdentityPublic) ->
    status_signed(macula_node_keys:verify([?STATUS_LABEL, 0, Tbs], Signature, IdentityPublic, Profile),
                  Tbs, BindingTbs, IdentityPublic, Profile, NowMs);
verify_status(_Envelope, _Binding, _IdentityPublic, _Profile, _NowMs) ->
    {error, malformed_frame}.

status_signed(false, _Tbs, _BindingTbs, _IdentityPublic, _Profile, _NowMs) ->
    {error, status_signature_invalid};
status_signed(true, Tbs, BindingTbs, IdentityPublic, Profile, NowMs) ->
    status_decoded(decode_fields(Tbs, ?STATUS_KEYS), BindingTbs, IdentityPublic, Profile, NowMs).

status_decoded(error, _BindingTbs, _IdentityPublic, _Profile, _NowMs) ->
    {error, malformed_frame};
status_decoded({ok, F}, BindingTbs, IdentityPublic, Profile, NowMs) ->
    status_result(run_checks([
        fun() -> well_formed_status(F, Profile) end,
        fun() -> expect(maps:get(<<"node_id">>, F) =:= macula_node_keys:node_id(IdentityPublic, Profile),
                        node_id_mismatch) end,
        fun() -> expect(maps:get(<<"binding_hash">>, F) =:= crypto:hash(sha384, BindingTbs),
                        status_binding_mismatch) end,
        fun() -> expect(maps:get(<<"issued_at">>, F) =< NowMs + ?TOLERANCE_MS, status_future_dated) end,
        fun() -> expect(NowMs - ?TOLERANCE_MS =< maps:get(<<"expires_at">>, F), status_expired) end
    ]), F).

status_result(ok, F) -> {ok, #{expires_at => maps:get(<<"expires_at">>, F)}};
status_result({error, _} = Error, _F) -> Error.

well_formed_status(#{<<"label">> := {text, ?STATUS_LABEL}, <<"node_id">> := NodeId,
                     <<"binding_hash">> := BindingHash, <<"issued_at">> := IssuedAt,
                     <<"expires_at">> := ExpiresAt, <<"sig_alg">> := {text, SigAlg}}, Profile)
  when is_binary(NodeId), byte_size(NodeId) =:= 32, is_binary(BindingHash), byte_size(BindingHash) =:= 48,
       is_integer(IssuedAt), is_integer(ExpiresAt), IssuedAt >= 0, IssuedAt =< ExpiresAt,
       ExpiresAt - IssuedAt =< ?MAX_STATUS_MS ->
    expect(SigAlg =:= sig_alg(Profile), malformed_frame);
well_formed_status(_Fields, _Profile) ->
    {error, malformed_frame}.

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

%% The received tbs, decoded strictly, with exactly the expected text keys.
decode_fields(Tbs, Keys) ->
    fields(macula_record_cbor:decode_strict(Tbs), Keys).

fields({ok, Map}, Keys) when is_map(Map), map_size(Map) =:= length(Keys) ->
    Plain = maps:fold(fun plain_field/3, #{}, Map),
    known_fields(lists:sort(maps:keys(Plain)) =:= Keys, Plain);
fields(_Decoded, _Keys) ->
    error.

plain_field({text, Key}, Value, Acc) -> Acc#{Key => Value};
plain_field(Key, Value, Acc) -> Acc#{{not_text, Key} => Value}.

known_fields(true, Plain) -> {ok, Plain};
known_fields(false, _Plain) -> error.

run_checks([]) ->
    ok;
run_checks([Check | Rest]) ->
    continue_checks(Check(), Rest).

continue_checks(ok, Rest) -> run_checks(Rest);
continue_checks({error, _} = Error, _Rest) -> Error.

expect(true, _Refusal) -> ok;
expect(false, Refusal) -> {error, Refusal}.

sig_alg(pq_pure) -> <<"ML-DSA-87">>;
sig_alg(pq_hybrid) -> <<"ML-DSA-87-PS384">>.
