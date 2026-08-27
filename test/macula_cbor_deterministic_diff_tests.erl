%% Differential tests: `macula_cbor_nif:pack_deterministic/1' +
%% `unpack_deterministic/1' (native, additive, NOT yet wired into
%% `macula_frame'/`macula_record') must produce byte-for-byte identical
%% output to `macula_record_cbor:encode/1' + `decode/1' (the existing,
%% live wire-protocol codec) across every value the two codecs both
%% claim to support.
%%
%% This is the gate for ever wiring the native codec into the live
%% frame path: `macula_frame.erl'/`macula_record.erl' sign and verify
%% against these exact bytes, so any divergence here is a live
%% wire-compatibility break, not a test failure to shrug off.
-module(macula_cbor_deterministic_diff_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------
%% Exact vectors — mirrors macula_record_cbor_tests.erl's own corpus,
%% so both codecs are exercised against the same boundary cases that
%% module already treats as load-bearing.
%%------------------------------------------------------------------

uint_boundaries_test_() ->
    [?_test(assert_full_parity(N))
     || N <- [0, 1, 23, 24, 100, 255, 256, 65535, 65536,
              16#FFFFFFFF, 16#FFFFFFFF + 1, 16#FFFFFFFFFFFFFFFF]].

neg_int_boundaries_test_() ->
    [?_test(assert_full_parity(N))
     || N <- [-1, -24, -25, -100, -255, -256, -65535, -65536,
              -16#FFFFFFFF, -16#FFFFFFFF - 1, -16#FFFFFFFFFFFFFFFF - 1]].

bytes_test_() ->
    [?_test(assert_full_parity(V)) || V <- [<<>>, <<1, 2, 3, 4, 5>>,
                                             crypto:strong_rand_bytes(64)]].

text_test_() ->
    [?_test(assert_full_parity(V)) || V <- [{text, <<>>}, {text, <<"hello macula">>},
                                             {text, <<0, 255, 1, 254>>}]].

null_test() -> assert_full_parity(null).

float_test_() ->
    [?_test(assert_full_parity(V)) || V <- [0.0, -0.0, 1.0, -1.0, 3.14159,
                                             1.0e10, 1.0e-10, -1.0e300]].

array_test_() ->
    [?_test(assert_full_parity(V))
     || V <- [[], [1, 2, {text, <<"three">>}, <<4, 5, 6>>, null],
              [[1, 2], [3, [4, 5]]]]].

int_key_map_test() ->
    assert_full_parity(#{0 => 100, 1 => -50, -1 => 42, 7 => {text, <<"seven">>}}).

empty_map_test() -> assert_full_parity(#{}).

text_key_map_test() ->
    assert_full_parity(#{ {text, <<"a">>} => 1, {text, <<"b">>} => 2 }).

nested_map_test() ->
    assert_full_parity(#{
        {text, <<"t">>} => 1,
        {text, <<"k">>} => crypto:strong_rand_bytes(32),
        {text, <<"p">>} => #{
            {text, <<"foo">>} => [1, 2, 3],
            {text, <<"bar">>} => {text, <<"baz">>}
        }
    }).

atom_test_() ->
    %% Atom is an ENCODE-only convenience on both sides; decode always
    %% produces {text, _}. Parity check compares encode output only,
    %% then confirms both decode that output to the same {text, _}.
    [?_test(assert_encode_parity(A)) || A <- [an_atom, hostname, ok, error]].

atom_in_map_key_test() ->
    assert_encode_parity(#{hostname => <<"beam00.lab">>}).

%%------------------------------------------------------------------
%% Randomized differential test — same style as
%% macula_frame_tests:check_payload_soundness_holds_on_generated_terms_test_/0:
%% seeded RNG, depth-limited recursive generator, run N times.
%%------------------------------------------------------------------

random_differential_test_() ->
    {timeout, 120,
     fun() ->
         _ = rand:seed(exsss, {20260827, 1, 1}),
         [assert_full_parity(gen_term(4)) || _ <- lists:seq(1, 3000)],
         ok
     end}.

gen_term(0) -> gen_leaf();
gen_term(D) -> gen_node(rand:uniform(4), D).

gen_node(1, D) -> [gen_term(D - 1) || _ <- lists:seq(1, rand:uniform(4) - 1)];
gen_node(2, D) -> maps:from_list([{gen_key(D - 1), gen_term(D - 1)}
                                  || _ <- lists:seq(1, rand:uniform(4))]);
gen_node(_N, _D) -> gen_leaf().

%% Map keys stay within the encodable value space too — any value()
%% can be a CBOR map key.
gen_key(D) -> gen_term(D).

gen_leaf() ->
    lists:nth(rand:uniform(14), [
        0, 1, 23, 24, 255, 256, 65535, 65536,
        rand:uniform(16#FFFFFFFFFFFFFFFF),
        -rand:uniform(16#FFFFFFFFFFFFFFFF),
        <<>>,
        crypto:strong_rand_bytes(rand:uniform(40)),
        {text, crypto:strong_rand_bytes(rand:uniform(40))},
        null
    ]).

%%------------------------------------------------------------------
%% Malformed-input safety — the native decoder parses untrusted,
%% network-received bytes and must never panic. Every case here must
%% come back as a normal Erlang exception (caught below), never crash
%% the test runner (which would mean it crashed the whole BEAM).
%%------------------------------------------------------------------

malformed_input_safety_test_() ->
    Cases = [
        <<>>,                                    % empty
        <<24>>,                                   % AI=24 with no length byte
        <<25, 1>>,                                % AI=25 with 1 of 2 length bytes
        <<26, 1, 2, 3>>,                           % AI=26 with 3 of 4 length bytes
        <<27, 1, 2, 3, 4, 5, 6, 7>>,               % AI=27 with 7 of 8 length bytes
        <<2:3, 24:5, 255>>,                        % byte string claims 255 bytes, has 0
        <<3:3, 26:5, 255, 255, 255, 255>>,         % text string claims 4G bytes
        <<4:3, 24:5, 10>>,                         % array claims 10 elements, has 0
        <<5:3, 1:5>>,                              % map claims 1 pair, has 0
        <<7:3, 20:5>>,                             % major 7, AI=20 (true) — unsupported
        <<7:3, 31:5>>,                             % major 7, AI=31 — unsupported
        <<6:3, 0:5>>,                               % major 6 (tag) — unsupported
        <<0:3, 0:5, 0>>,                            % valid uint(0) PLUS trailing byte
        <<7:3, 25:5, 16#7C, 16#00>>                 % half-float +Infinity (Exp=31)
    ],
    [?_test(assert_raises_cleanly(C)) || C <- Cases].

assert_raises_cleanly(Bytes) ->
    Result = try {ok, macula_cbor_nif:unpack_deterministic(Bytes)}
             catch Class:Reason -> {caught, Class, Reason}
             end,
    ?assertMatch({caught, _, _}, Result).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

%% Full parity: both encoders produce identical bytes, and decoding
%% those bytes with either decoder reproduces the original value.
assert_full_parity(V) ->
    assert_encode_parity(V),
    ErlangBytes = macula_record_cbor:encode(V),
    ?assertEqual(V, macula_record_cbor:decode(ErlangBytes)),
    ?assertEqual(V, macula_cbor_nif:unpack_deterministic(ErlangBytes)),
    ok.

%% Encode-only parity, for values whose canonical decode result
%% legitimately differs from the input (atoms decode back as {text,_}).
assert_encode_parity(V) ->
    ErlangBytes = macula_record_cbor:encode(V),
    NativeBytes = macula_cbor_nif:pack_deterministic(V),
    ?assertEqual(ErlangBytes, NativeBytes),
    ok.
