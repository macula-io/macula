%% EUnit tests for macula_record_cbor.
-module(macula_record_cbor_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Round-trip
%%------------------------------------------------------------------

uint_roundtrip_test_() ->
    [?_assertEqual(N, roundtrip(N))
     || N <- [0, 1, 23, 24, 100, 255, 256, 65535, 65536,
              16#FFFFFFFF, 16#FFFFFFFF + 1, 16#FFFFFFFFFFFFFFFF]].

bytes_roundtrip_test() ->
    Bin = <<1, 2, 3, 4, 5>>,
    ?assertEqual(Bin, roundtrip(Bin)).

empty_bytes_roundtrip_test() ->
    ?assertEqual(<<>>, roundtrip(<<>>)).

text_roundtrip_test() ->
    T = {text, <<"hello macula">>},
    ?assertEqual(T, roundtrip(T)).

empty_text_roundtrip_test() ->
    ?assertEqual({text, <<>>}, roundtrip({text, <<>>})).

empty_array_test() ->
    ?assertEqual([], roundtrip([])).

mixed_array_roundtrip_test() ->
    L = [1, 2, {text, <<"three">>}, <<4, 5, 6>>, null],
    ?assertEqual(L, roundtrip(L)).

empty_map_test() ->
    ?assertEqual(#{}, roundtrip(#{})).

map_roundtrip_test() ->
    M = #{ {text, <<"a">>} => 1, {text, <<"b">>} => 2 },
    ?assertEqual(M, roundtrip(M)).

null_test() ->
    ?assertEqual(null, roundtrip(null)).

nested_map_test() ->
    M = #{
        {text, <<"t">>} => 1,
        {text, <<"k">>} => crypto:strong_rand_bytes(32),
        {text, <<"p">>} => #{
            {text, <<"foo">>} => [1, 2, 3],
            {text, <<"bar">>} => {text, <<"baz">>}
        }
    },
    ?assertEqual(M, roundtrip(M)).

%%------------------------------------------------------------------
%% Determinism — RFC 8949 §4.2.1
%%------------------------------------------------------------------

map_keys_sorted_independent_of_insertion_order_test() ->
    M1 = #{ {text, <<"c">>} => 3, {text, <<"a">>} => 1, {text, <<"b">>} => 2 },
    M2 = #{ {text, <<"a">>} => 1, {text, <<"b">>} => 2, {text, <<"c">>} => 3 },
    ?assertEqual(macula_record_cbor:encode(M1), macula_record_cbor:encode(M2)).

map_keys_sorted_by_encoded_bytes_test() ->
    %% Different key types compare by their CBOR encoding bytes.
    %% encode(1) = <<0>> ; encode({text, <<"a">>}) = <<97, 97>> (text, len 1, "a")
    %% So integer key 1 sorts before the text key.
    M = #{ {text, <<"a">>} => 1, 1 => 2 },
    Wire = macula_record_cbor:encode(M),
    %% First k/v pair should be (1, 2) — integer key first.
    %% map header: <<5:3, 2:5>> = 0xA2.
    %% 1 = <<0:3, 1:5>> = 0x01
    %% 2 = <<0:3, 2:5>> = 0x02
    ?assertEqual(<<16#A2, 16#01, 16#02, 16#61, "a", 1>>, Wire).

shortest_uint_encoding_test() ->
    ?assertEqual(<<0>>,                   macula_record_cbor:encode(0)),
    ?assertEqual(<<23>>,                  macula_record_cbor:encode(23)),
    ?assertEqual(<<24, 24>>,              macula_record_cbor:encode(24)),
    ?assertEqual(<<24, 255>>,             macula_record_cbor:encode(255)),
    ?assertEqual(<<25, 1, 0>>,            macula_record_cbor:encode(256)),
    ?assertEqual(<<25, 255, 255>>,        macula_record_cbor:encode(65535)),
    ?assertEqual(<<26, 0, 1, 0, 0>>,      macula_record_cbor:encode(65536)),
    ?assertEqual(<<27, 0, 0, 0, 1, 0, 0, 0, 0>>,
                 macula_record_cbor:encode(16#100000000)).

%%------------------------------------------------------------------
%% Atom encoding (round-trip robustness)
%%------------------------------------------------------------------
%%
%% Background: `macula_frame:from_wire_envelope/1' atomizes binary
%% keys it recognises via `binary_to_existing_atom/1'. When such a
%% record is fed back through `macula_record:verify/1', the verifier
%% re-encodes the envelope for signature verification — and the
%% recursive payload sub-map carries atom keys. The encoder MUST
%% accept atoms (and emit them as the same UTF-8 byte string the
%% binary key originally had) so the canonical bytes round-trip.

encode_atom_emits_text_string_test() ->
    %% Atom encodes identically to its `{text, atom_to_binary/1}' form.
    A     = encode_atom_emits_text_string_test,
    AsBin = atom_to_binary(A, utf8),
    ?assertEqual(macula_record_cbor:encode({text, AsBin}),
                 macula_record_cbor:encode(A)).

encode_atom_in_map_keys_test() ->
    %% Atom-keyed and text-keyed maps with the same name encode to
    %% identical bytes — the round-trip from wire (text) → frame
    %% decode (atom) → re-encode (text) is byte-for-byte stable.
    AtomMap = #{hostname => <<"beam00.lab">>},
    TextMap = #{{text, <<"hostname">>} => <<"beam00.lab">>},
    ?assertEqual(macula_record_cbor:encode(TextMap),
                 macula_record_cbor:encode(AtomMap)).

encode_null_still_uses_simple_value_test() ->
    %% Atom clause must NOT shadow the dedicated `null' clause —
    %% null still emits the major-7 simple value, not a text string.
    ?assertEqual(<<16#F6>>, macula_record_cbor:encode(null)).

verify_round_trip_with_atomized_payload_test() ->
    %% Real-world reproducer: build a node_record (text keys), sign,
    %% then simulate the wire round-trip by atomizing the payload's
    %% recognised text keys via `binary_to_existing_atom'. Verify
    %% must accept the atomized record (which means the encoder's
    %% canonical_unsigned re-encoding must handle atoms).
    Kp     = macula_identity:generate(),
    Pub    = macula_identity:public(Kp),
    Opts   = #{ttl_ms   => 60000,
               kind     => <<"daemon">>,
               hostname => <<"beam00.lab">>},
    Signed = macula_record:sign(macula_record:node_record(Pub, [], 0, Opts), Kp),
    %% Atomize keys recursively in the payload sub-map (mimicking
    %% `macula_frame:from_wire_envelope/1' on the receive path).
    Atomized = atomize_keys(Signed),
    ?assertMatch({ok, _}, macula_record:verify(Atomized)).

%% Recursive key-atomizer that mimics what `macula_frame:envelope_key/1'
%% does to a wire-decoded record. Top-level atom keys are kept; nested
%% map keys that match an existing atom become atoms.
atomize_keys(M) when is_map(M) ->
    maps:fold(fun(K, V, Acc) ->
                  K1 = key_to_atom(K),
                  V1 = atomize_keys(V),
                  Acc#{K1 => V1}
              end, #{}, M);
atomize_keys(L) when is_list(L) ->
    [atomize_keys(E) || E <- L];
atomize_keys(V) -> V.

key_to_atom({text, B}) when is_binary(B) ->
    try binary_to_existing_atom(B, utf8)
    catch error:badarg -> {text, B}
    end;
key_to_atom(B) when is_binary(B) ->
    try binary_to_existing_atom(B, utf8)
    catch error:badarg -> B
    end;
key_to_atom(K) -> K.

definite_lengths_only_test() ->
    %% Indefinite-length items would have AI=31 in the type byte.
    %% Confirm none of our encodings use that.
    Samples = [
        macula_record_cbor:encode([1, 2, 3]),
        macula_record_cbor:encode(#{ 1 => 2, 3 => 4 }),
        macula_record_cbor:encode({text, <<"x">>}),
        macula_record_cbor:encode(<<1, 2, 3>>)
    ],
    [?assert(no_indefinite(B)) || B <- Samples].

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

roundtrip(V) ->
    macula_record_cbor:decode(macula_record_cbor:encode(V)).

%% Walk a CBOR binary and assert no AI=31 (indefinite) markers.
no_indefinite(<<>>) -> true;
no_indefinite(<<_MT:3, 31:5, _/binary>>) -> false;
no_indefinite(<<_MT:3, _AI:5, Rest/binary>>) -> no_indefinite(Rest).
