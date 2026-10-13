%% The wire 1.0 _dht.* conformance runner (macula#102): every case of the dht_procedures vectors
%% (priv/vectors/wire_1_0/, docs/design/WIRE_1_0.md section 6) through macula_dht_procedures. A station reads an
%% argument map (malformed_args); a caller reads a reply (reply_version_unsupported, malformed_reply) and keeps the
%% records that verify as a reader at the case's clock. What a node accepts it also builds: its own encoding of the
%% value read is the file's bytes.
-module(macula_dht_procedures_conformance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LANDS_WITH, <<"macula#102">>).

args_test_() ->
    [{binary_to_list(Id), fun() -> args_checked(procedure(C), binary:decode_hex(Hex), Expect) end}
     || #{<<"id">> := Id, <<"input">> := #{<<"args">> := Hex}, <<"expect">> := Expect} = C <- dht_cases()].

replies_test_() ->
    [{binary_to_list(Id), fun() -> reply_checked(C, binary:decode_hex(Hex), Expect) end}
     || #{<<"id">> := Id, <<"input">> := #{<<"reply">> := Hex}, <<"expect">> := Expect} = C <- dht_cases()].

args_checked(Proc, Bytes, #{<<"outcome">> := <<"accept">>}) ->
    {ok, Value} = macula_dht_procedures:read_args(Proc, decoded(Bytes)),
    ?assertEqual({ok, Bytes}, macula_frame:payload_plain(macula_dht_procedures:args(Proc, Value)));
args_checked(Proc, Bytes, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Reason}) ->
    ?assertEqual({error, binary_to_atom(Reason)}, macula_dht_procedures:read_args(Proc, decoded(Bytes))).

reply_checked(C, Bytes, #{<<"outcome">> := <<"accept">>, <<"result">> := Result} = Expect) ->
    Proc = procedure(C),
    {ok, Read} = macula_dht_procedures:read_reply(Proc, decoded(Bytes)),
    ?assertEqual(Result, result_name(Read)),
    reason_checked(Read, Expect),
    kept_checked(Read, C, Expect),
    ?assertEqual({ok, Bytes}, macula_frame:payload_plain(macula_dht_procedures:reply(Proc, Read)));
reply_checked(C, Bytes, #{<<"outcome">> := <<"refuse">>, <<"reason">> := Reason}) ->
    ?assertEqual({error, binary_to_atom(Reason)}, macula_dht_procedures:read_reply(procedure(C), decoded(Bytes))).

result_name(ok) -> <<"ok">>;
result_name({refused, _Reason}) -> <<"refused">>;
result_name({record, _Wire}) -> <<"record">>;
result_name(not_found) -> <<"not_found">>;
result_name({records, _Wires}) -> <<"records">>.

reason_checked({refused, Reason}, #{<<"reason">> := Expected}) -> ?assertEqual(Expected, Reason);
reason_checked(_Read, _Expect) -> ok.

%% A records reply's records_kept: how many of them verify as a reader under the case's profile and clock.
kept_checked({records, Wires}, #{<<"profile">> := P, <<"input">> := #{<<"now_ms">> := Now}},
             #{<<"records_kept">> := Kept}) ->
    ?assertEqual(Kept, length([R || W <- Wires, {ok, R} <- [macula_record:verify(W, binary_to_atom(P), Now)]]));
kept_checked(_Read, _Case, _Expect) ->
    ok.

%% The one key form a peer's payload arrives in (D26): text as {text, Bin}, bytes as binaries.
decoded(Bytes) ->
    {ok, Value} = macula_record_cbor:decode_strict(Bytes),
    Value.

procedure(#{<<"input">> := #{<<"procedure">> := Name}}) ->
    {ok, Proc} = macula_dht_procedures:procedure(Name),
    Proc.

dht_cases() ->
    [C || #{<<"lands_with">> := LandsWith} = C <- maps:get(<<"cases">>, vectors()),
          lists:member(?LANDS_WITH, LandsWith)].

vectors() ->
    {ok, Bytes} = file:read_file(filename:join([code:priv_dir(macula), "vectors", "wire_1_0", "dht_procedures.json"])),
    json:decode(Bytes).
