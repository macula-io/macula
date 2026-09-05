%% Differential tests: `macula_cbor_nif:pack_deterministic/1' +
%% `unpack_deterministic/1' (native) must produce byte-for-byte identical
%% output to `macula_record_cbor:encode/1' + `decode/1' (the pure-Erlang
%% codec, now test-only) across every value the two codecs both claim to
%% support. The native codec IS the live wire path today --
%% `macula_frame.erl'/`macula_record.erl' sign and verify against these
%% exact bytes -- so any divergence here is a live wire-compatibility
%% break, not a test failure to shrug off.
%%
%% This also means MAX_NESTING_DEPTH (deterministic.rs) is a deliberate,
%% one-sided divergence from the pure-Erlang reference: the reference
%% accepts arbitrarily deep nesting (it isn't reachable pre-auth on
%% attacker bytes the way the NIF is, running on a BEAM scheduler
%% thread), the NIF rejects anything past 128 levels. No real macula
%% wire value nests remotely that deep, so this isn't exercised as a
%% "divergence" here -- but it is one, by design, if anyone ever probes
%% for it.
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
        <<7:3, 25:5, 16#7C, 16#00>>,                % half-float +Infinity (Exp=31)
        %% float32/float64 NaN/Infinity — regression guard for a real
        %% VM-abort bug: decode_major7's AI=26/27 arms built an Erlang
        %% float term directly from the wire bits with no finiteness
        %% check (unlike this same AI=25/half-float case above, which
        %% already rejected it). BEAM has no NaN/Infinity float
        %% representation; the resulting invalid term worked fine right
        %% up until something called `encode_value` on it (which the
        %% decode_map dedup fix below does, for every key) — at which
        %% point it aborted the whole VM with an ERTS assertion failure
        %% (`tag_val_def()`), not a catchable Erlang exception. Found by
        %% adversarial review, confirmed empirically (isolated
        %% subprocess, SIGABRT, exit 134) before being fixed at the
        %% source in decode_major7.
        <<16#A1, 16#FA, 16#7F, 16#C0, 0, 0, 1>>,        % map key = float32 NaN
        <<16#A1, 16#FB, 16#7F, 16#F8, 0,0,0,0,0,0, 1>>, % map key = float64 NaN
        <<16#A1, 16#FB, 16#7F, 16#F0, 0,0,0,0,0,0, 1>>, % map key = float64 +Infinity
        <<16#A1, 16#FB, 16#FF, 16#F0, 0,0,0,0,0,0, 1>>, % map key = float64 -Infinity
        <<16#FA, 16#7F, 16#C0, 0, 0>>,                  % bare float32 NaN (not a key)
        <<16#FB, 16#7F, 16#F8, 0,0,0,0,0,0>>            % bare float64 NaN (not a key)
    ],
    [?_test(assert_raises_cleanly(C)) || C <- Cases].

assert_raises_cleanly(Bytes) ->
    Result = try {ok, macula_cbor_nif:unpack_deterministic(Bytes)}
             catch Class:Reason -> {caught, Class, Reason}
             end,
    ?assertMatch({caught, _, _}, Result).

%%------------------------------------------------------------------
%% Nesting depth — regression guard for a real pre-auth crash: plain
%% recursive descent with no depth limit let a chain of one-element
%% arrays (one byte per level, 0x81 repeated) overflow the stack.
%% Confirmed empirically pre-fix: this NIF, running directly on a BEAM
%% scheduler thread, segfaulted the WHOLE VM (not just one connection)
%% at a nesting depth of only 10,000 — a 10,001-byte frame, a tiny
%% fraction of the 16MB frame cap. MAX_NESTING_DEPTH (128, deterministic.rs)
%% rejects anything past that with a clean, catchable error instead.
%%------------------------------------------------------------------

nested_list_payload(Depth) ->
    Heads = binary:copy(<<16#81>>, Depth),
    <<Heads/binary, 0>>. % innermost value: uint(0), 0x00

decode_accepts_nesting_at_the_depth_limit_test() ->
    Bytes = nested_list_payload(128),
    ?assert(is_list(macula_cbor_nif:unpack_deterministic(Bytes))).

decode_rejects_nesting_one_past_the_depth_limit_test() ->
    Bytes = nested_list_payload(129),
    assert_raises_cleanly(Bytes).

decode_rejects_extreme_nesting_without_crashing_test() ->
    %% Far beyond both the limit and what actually crashed the pre-fix
    %% decoder (10,000) -- if this test process crashes instead of
    %% completing, the depth guard has regressed.
    Bytes = nested_list_payload(100000),
    assert_raises_cleanly(Bytes).

%%------------------------------------------------------------------
%% decode_map dedup — regression guards for the O(n^2) DoS this
%% function used to have, and for a second, subtler one the FIRST fix
%% attempt reintroduced.
%%------------------------------------------------------------------

%% A duplicate key overwrites its ORIGINAL insertion slot, not a new
%% one appended at the end. (Erlang map equality is order-free, so this
%% can't observe "every other key's position is undisturbed" -- what it
%% actually guards is "right slot, not appended": easy for a faster
%% implementation to accidentally dedupe into the wrong slot instead of
%% the one the duplicate key first claimed.)
decode_duplicate_map_key_overwrites_its_original_slot_test() ->
    Map = #{{text, <<"a">>} => 1, {text, <<"b">>} => 2, {text, <<"c">>} => 3},
    <<HeadByte, Rest/binary>> = macula_cbor_nif:pack_deterministic(Map),
    3 = HeadByte band 16#1F, % sanity: a 3-entry map header, as expected
    %% Bump the map header's entry count 3 -> 4, then append one more
    %% entry, "b" -> 99, so the wire form has 4 entries with "b" duplicated.
    NewHeadByte = (HeadByte band 16#E0) bor 4,
    ExtraKey = macula_cbor_nif:pack_deterministic({text, <<"b">>}),
    ExtraVal = macula_cbor_nif:pack_deterministic(99),
    Bytes = <<NewHeadByte, Rest/binary, ExtraKey/binary, ExtraVal/binary>>,
    Decoded = macula_cbor_nif:unpack_deterministic(Bytes),
    ?assertEqual(#{{text, <<"a">>} => 1, {text, <<"b">>} => 99, {text, <<"c">>} => 3}, Decoded).

%% Regression guards for the branch the tests above never exercise: a
%% NESTED map used as a KEY, where that inner map itself needs its own
%% dedup pass (need_canon=true propagated down) before the OUTER map can
%% even compute a canonical identity for it. The full differential fuzz
%% above can't reach this either -- it only ever feeds already-canonical
%% (duplicate-free, pre-sorted) bytes, since it round-trips through this
%% codec's own encoder. Found missing by adversarial review of the
%% depth/need_canon fix; a bug in the vals_canon update path or the
%% final key-sort step would either dedupe to the wrong outer value or
%% let two `=:=`-identical keys both reach `map_from_arrays` (which
%% errors).
decode_outer_map_dedupes_nested_map_keys_with_internal_duplicates_test() ->
    %% Outer map, 2 entries. Key 1: inner map {"x":1,"x":2} (a duplicate
    %% within the inner map itself, decodes to {"x":2}), value 10.
    %% Key 2: inner map {"x":2} (no duplicate), value 11. Both keys
    %% canonicalize to the identical inner map #{{text,<<"x">>} => 2},
    %% so the OUTER map must dedupe them too -- last write (11) wins.
    Bytes1 = <<16#A2,
               16#A2, 16#61, $x, 1, 16#61, $x, 2, 10,
               16#A1, 16#61, $x, 2, 11>>,
    ?assertEqual(#{#{{text, <<"x">>} => 2} => 11},
                 macula_cbor_nif:unpack_deterministic(Bytes1)).

decode_outer_map_dedupes_nested_map_keys_with_different_wire_order_test() ->
    %% Outer map, 2 entries. Key 1: inner map {"y":2,"x":1} in
    %% NON-canonical wire order (y before x). Key 2: inner map
    %% {"x":1,"y":2} already in canonical (sorted-by-encoded-bytes)
    %% order. Both decode to the same logical inner map regardless of
    %% wire order, so their canonical bytes must match too -- the OUTER
    %% map must dedupe them, last write (11) wins.
    Bytes2 = <<16#A2,
               16#A2, 16#61, $y, 2, 16#61, $x, 1, 10,
               16#A2, 16#61, $x, 1, 16#61, $y, 2, 11>>,
    ?assertEqual(#{#{{text, <<"x">>} => 1, {text, <<"y">>} => 2} => 11},
                 macula_cbor_nif:unpack_deterministic(Bytes2)).

%% Regression guard for the original bug: decode_map's dedup was a
%% linear Term-equality scan over every entry decoded so far, O(n^2) in
%% entry count. A ~350KB crafted map (well under the frame cap) took
%% 50+ seconds to decode as a result. This decodes many more entries in
%% low single-digit milliseconds; if dedup regresses to a linear scan,
%% this test times out long before it fails any assertion.
decode_map_with_many_distinct_keys_is_not_quadratic_test_() ->
    {timeout, 10, fun() ->
        N = 20000,
        Pairs = [{I, 0} || I <- lists:seq(0, N - 1)],
        Bytes = macula_cbor_nif:pack_deterministic(maps:from_list(Pairs)),
        {ElapsedUs, Decoded} = timer:tc(macula_cbor_nif, unpack_deterministic, [Bytes]),
        ?assertEqual(N, maps:size(Decoded)),
        ?assert(ElapsedUs < 2000000)
    end}.

%% A second, narrower regression the SAME bug had once already: the
%% first attempt at fixing the flat-map case above looked up each key
%% by re-encoding it fresh, which fixed that case but reintroduced
%% unbounded work for a map whose KEY is itself a large nested
%% structure -- re-encoding a key from scratch at every ancestor level
%% costs O(depth x key size), and a 128-level chain of single-entry
%% maps keyed by a multi-megabyte blob took real, measured seconds even
%% under the frame cap. Building each value's canonical bytes bottom-up
%% (this fix) bounds it to O(depth x size) instead of re-deriving
%% anything. This decodes a maximally-deep chain wrapping a 4MB blob
%% key in well under a second; if key canonicalization regresses to
%% re-deriving a key's bytes at every ancestor level, this test times
%% out long before it fails any assertion.
decode_map_with_a_large_deeply_nested_key_is_not_quadratic_in_depth_test_() ->
    {timeout, 20, fun() ->
        Depth = 128,
        BlobLen = 4 * 1024 * 1024,
        MapHeads = binary:copy(<<16#A1>>, Depth),
        BlobHeader = <<16#5A, BlobLen:32/big-unsigned-integer>>,
        Blob = binary:copy(<<16#41>>, BlobLen),
        Values = binary:copy(<<0>>, Depth),
        Bytes = <<MapHeads/binary, BlobHeader/binary, Blob/binary, Values/binary>>,
        {ElapsedUs, Decoded} = timer:tc(macula_cbor_nif, unpack_deterministic, [Bytes]),
        ?assert(is_map(Decoded)),
        ?assert(ElapsedUs < 5000000)
    end}.

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
