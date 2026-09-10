%% EUnit tests for macula_key_bindings: TLS and CONNECT key bindings and their status statements, laid out in
%% plans/DESIGN_PQ_HANDSHAKE_FRAMES.md (decisions D16 and D22).
-module(macula_key_bindings_tests).

-include_lib("eunit/include/eunit.hrl").

%% RSA-4096 key generation takes up to about a second per key.
-define(EU_TIMEOUT, 120).
-define(NOW, 1789000000000).
-define(MINUTE, 60000).
-define(HOUR, 3600000).
-define(DAY, 86400000).
-define(TLS_LABEL, <<"MACULA-PQ-BINDING-TLS-V1">>).
-define(CONNECT_LABEL, <<"MACULA-PQ-BINDING-CONNECT-V1">>).
-define(STATUS_LABEL, <<"MACULA-PQ-STATUS-V1">>).
-define(LEAF, <<"the strict DER leaf certificate this connection presented">>).

pq_pure_test_() ->
    {setup, fun() -> keys(pq_pure) end, fun all_cases/1}.

pq_hybrid_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun() -> keys(pq_hybrid) end, fun all_cases/1}}.

all_cases(Keys) ->
    tls_binding_cases(Keys) ++ connect_binding_cases(Keys) ++ status_cases(Keys) ++ crafted_cases(Keys).

%%------------------------------------------------------------------
%% TLS-key binding
%%------------------------------------------------------------------

tls_binding_cases(#{identity := Id, public := Public, profile := Profile}) ->
    Binding = macula_key_bindings:tls_binding(Id, ?LEAF, ?NOW, ?NOW + 7 * ?DAY),
    Verify = fun(Envelope, Leaf, Now) ->
        macula_key_bindings:verify_tls_binding(Envelope, Public, Profile, Leaf, Now)
    end,
    [?_assertMatch({ok, #{use := tls}}, Verify(Binding, ?LEAF, ?NOW + ?HOUR)),
     ?_assertMatch({ok, _}, Verify(Binding, ?LEAF, ?NOW - 4 * ?MINUTE)),
     ?_assertMatch({ok, _}, Verify(Binding, ?LEAF, ?NOW + 7 * ?DAY + 4 * ?MINUTE)),
     ?_assertEqual({error, binding_not_yet_valid}, Verify(Binding, ?LEAF, ?NOW - 6 * ?MINUTE)),
     ?_assertEqual({error, binding_expired}, Verify(Binding, ?LEAF, ?NOW + 7 * ?DAY + 6 * ?MINUTE)),
     ?_assertEqual({error, binding_key_mismatch}, Verify(Binding, <<"another leaf">>, ?NOW)),
     ?_assertEqual({error, binding_signature_invalid}, Verify(flip_signature(Binding), ?LEAF, ?NOW)),
     ?_assertEqual({error, malformed_frame}, Verify(maps:remove(signature, Binding), ?LEAF, ?NOW)),
     ?_assertEqual({error, malformed_frame}, Verify(Binding#{extra => <<>>}, ?LEAF, ?NOW))].

%%------------------------------------------------------------------
%% CONNECT-key binding
%%------------------------------------------------------------------

connect_binding_cases(#{identity := Id, public := Public, profile := Profile, connect_public := Connect}) ->
    Binding = macula_key_bindings:connect_binding(Id, Connect, ?NOW, ?NOW + ?DAY),
    [?_assertMatch({ok, #{use := connect}},
                   macula_key_bindings:verify_connect_binding(Binding, Public, Profile, Connect, ?NOW)),
     ?_assertEqual({error, binding_key_mismatch},
                   macula_key_bindings:verify_connect_binding(Binding, Public, Profile, <<Connect/binary, 0>>, ?NOW)),
     %% A binding for one use never verifies as the other: its signature covers the other use's label.
     ?_assertEqual({error, binding_signature_invalid},
                   macula_key_bindings:verify_tls_binding(Binding, Public, Profile, Connect, ?NOW))].

%%------------------------------------------------------------------
%% Status statement
%%------------------------------------------------------------------

status_cases(#{identity := Id, public := Public, profile := Profile}) ->
    Binding = macula_key_bindings:tls_binding(Id, ?LEAF, ?NOW, ?NOW + 7 * ?DAY),
    Other = macula_key_bindings:tls_binding(Id, ?LEAF, ?NOW, ?NOW + 7 * ?DAY),
    Status = macula_key_bindings:status_statement(Id, Binding, ?NOW, ?NOW + ?HOUR),
    Later = macula_key_bindings:status_statement(Id, Binding, ?NOW + 10 * ?MINUTE, ?NOW + 70 * ?MINUTE),
    Verify = fun(Statement, ForBinding, Now) ->
        macula_key_bindings:verify_status(Statement, ForBinding, Public, Profile, Now)
    end,
    [?_assertEqual(ok, Verify(Status, Binding, ?NOW + 10 * ?MINUTE)),
     ?_assertEqual(ok, Verify(Status, Binding, ?NOW + ?HOUR + 4 * ?MINUTE)),
     ?_assertEqual({error, status_expired}, Verify(Status, Binding, ?NOW + ?HOUR + 6 * ?MINUTE)),
     ?_assertEqual({error, status_future_dated}, Verify(Later, Binding, ?NOW)),
     ?_assertEqual({error, status_binding_mismatch}, Verify(Status, Other, ?NOW)),
     ?_assertEqual({error, status_signature_invalid}, Verify(flip_signature(Status), Binding, ?NOW)),
     ?_assertEqual({error, malformed_frame}, Verify(maps:remove(tbs, Status), Binding, ?NOW))].

%%------------------------------------------------------------------
%% Signed, but not what the design allows
%%------------------------------------------------------------------

crafted_cases(#{identity := Id, public := Public, profile := Profile}) ->
    Fields = binding_fields(Id, Profile, <<"tls">>, ?TLS_LABEL, ?LEAF),
    Verify = fun(Envelope) -> macula_key_bindings:verify_tls_binding(Envelope, Public, Profile, ?LEAF, ?NOW) end,
    StatusFields = status_fields(Id, Profile, macula_key_bindings:tls_binding(Id, ?LEAF, ?NOW, ?NOW + ?DAY)),
    [?_assertMatch({ok, _}, Verify(signed(Id, ?TLS_LABEL, encode(Fields)))),
     ?_assertEqual({error, node_id_mismatch},
                   Verify(signed(Id, ?TLS_LABEL, encode(Fields#{{text, <<"node_id">>} => <<1:256>>})))),
     ?_assertEqual({error, binding_wrong_use},
                   Verify(signed(Id, ?TLS_LABEL, encode(Fields#{{text, <<"use">>} => {text, <<"connect">>}})))),
     ?_assertEqual({error, malformed_frame},
                   Verify(signed(Id, ?TLS_LABEL, encode(Fields#{{text, <<"comment">>} => {text, <<"x">>}})))),
     ?_assertEqual({error, malformed_frame},
                   Verify(signed(Id, ?TLS_LABEL, encode(Fields#{{text, <<"not_after">>} => ?NOW + 8 * ?DAY})))),
     ?_assertEqual({error, malformed_frame},
                   Verify(signed(Id, ?TLS_LABEL, encode(Fields#{{text, <<"subject_hash">>} => <<0:256>>})))),
     ?_assertEqual({error, malformed_frame},
                   Verify(signed(Id, ?TLS_LABEL, encode(Fields#{{text, <<"sig_alg">>} => {text, <<"EdDSA">>}})))),
     ?_assertEqual({error, malformed_frame}, Verify(signed(Id, ?TLS_LABEL, duplicate_use(Fields)))),
     ?_assertEqual({error, malformed_frame},
                   macula_key_bindings:verify_status(
                     signed(Id, ?STATUS_LABEL, encode(StatusFields#{{text, <<"expires_at">>} => ?NOW + 2 * ?HOUR})),
                     macula_key_bindings:tls_binding(Id, ?LEAF, ?NOW, ?NOW + ?DAY), Public, Profile, ?NOW))].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

keys(Profile) ->
    {ok, Identity} = macula_node_keys:generate(identity, Profile),
    {ok, Connect} = macula_node_keys:generate(connect, Profile),
    #{identity => Identity, public => macula_node_keys:public_key(Identity), profile => Profile,
      connect_public => macula_node_keys:public_key(Connect)}.

binding_fields(Id, Profile, Use, Label, Subject) ->
    {ok, NodeId} = macula_node_keys:node_id(Id),
    #{{text, <<"label">>} => {text, Label}, {text, <<"node_id">>} => NodeId, {text, <<"use">>} => {text, Use},
      {text, <<"subject_hash">>} => crypto:hash(sha384, Subject), {text, <<"binding_id">>} => <<0:128>>,
      {text, <<"not_before">>} => ?NOW, {text, <<"not_after">>} => ?NOW + ?DAY,
      {text, <<"hash_alg">>} => {text, <<"SHA-384">>}, {text, <<"sig_alg">>} => {text, sig_alg(Profile)}}.

status_fields(Id, Profile, #{tbs := BindingTbs}) ->
    {ok, NodeId} = macula_node_keys:node_id(Id),
    #{{text, <<"label">>} => {text, ?STATUS_LABEL}, {text, <<"node_id">>} => NodeId,
      {text, <<"binding_hash">>} => crypto:hash(sha384, BindingTbs), {text, <<"issued_at">>} => ?NOW,
      {text, <<"expires_at">>} => ?NOW + ?HOUR, {text, <<"sig_alg">>} => {text, sig_alg(Profile)}}.

sig_alg(pq_pure) -> <<"ML-DSA-87">>;
sig_alg(pq_hybrid) -> <<"ML-DSA-87-PS384">>.

encode(Fields) ->
    macula_record_cbor:encode(Fields).

signed(Id, Label, Tbs) ->
    #{tbs => Tbs, signature => macula_node_keys:sign([Label, 0, Tbs], Id)}.

%% The binding's fields with a second "use" key, which only a hand-built encoding can carry.
duplicate_use(Fields) ->
    Pairs = << <<(encode(K))/binary, (encode(V))/binary>> || K := V <- Fields >>,
    Duplicate = <<(encode({text, <<"use">>}))/binary, (encode({text, <<"connect">>}))/binary>>,
    <<(16#A0 + map_size(Fields) + 1), Pairs/binary, Duplicate/binary>>.

flip_signature(#{signature := <<Head:10/binary, Byte, Tail/binary>>} = Envelope) ->
    Envelope#{signature := <<Head/binary, (Byte bxor 1), Tail/binary>>}.
