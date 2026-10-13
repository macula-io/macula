%% The wire 1.0 conformance runner for the cases that land with macula#99 (docs/design/WIRE_1_0.md sections 2, 3 and 8):
%% the envelope class, the enums class, the frames class and the tbs class, read from priv/vectors/wire_1_0/ as a
%% consumer of the hex package reads them. Each case is judged the way this node judges those bytes: a control-stream
%% frame as a connection reads one after HELLO (macula_frame:read_wire/1), a dedicated-stream frame as a station link
%% reads one (macula_frame:parse_received/1), a signed object by its verifier, and a tbs by the builder that writes it.
%% Cases that also name another issue in lands_with run here for their macula half; a case whose issues are all others'
%% is left to those issues' runners.
-module(macula_wire_1_0_envelope_runner_tests).

-include_lib("eunit/include/eunit.hrl").
-include("macula_quic_error_codes.hrl").

-define(ISSUE, <<"macula#99">>).

envelope_test_() ->
    cases_run(<<"envelope">>, fun envelope_checked/1).

enums_test_() ->
    cases_run(<<"enums">>, fun enum_checked/1).

frames_test_() ->
    cases_run(<<"frames">>, fun envelope_checked/1).

tbs_test_() ->
    cases_run(<<"tbs">>, fun tbs_checked/1).

cases_run(Class, Check) ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(macula) end, fun(_) -> ok end,
     [{binary_to_list(Id), fun() -> Check(C) end}
      || #{<<"id">> := Id, <<"lands_with">> := Lands} = C <- cases(Class), lists:member(?ISSUE, Lands)]}.

%%------------------------------------------------------------------
%% Envelope and frames: what a receiver does with the bytes
%%------------------------------------------------------------------

envelope_checked(#{<<"input">> := Input, <<"expect">> := Expect}) ->
    Before = macula_wire_tolerance:counters(),
    Judged = judged(Input),
    outcome_checked(Expect, Judged, Before).

outcome_checked(#{<<"outcome">> := <<"accept">>} = Expect, Judged, Before) ->
    ?assertMatch({accept, _}, Judged),
    counted(Expect, Before);
outcome_checked(#{<<"outcome">> := <<"ignore">>} = Expect, Judged, Before) ->
    ?assertEqual(ignore, Judged),
    counted(Expect, Before);
outcome_checked(#{<<"outcome">> := <<"refuse">>, <<"reason">> := Reason}, Judged, _Before) ->
    ?assertEqual({refuse, Reason}, Judged);
outcome_checked(#{<<"outcome">> := <<"abort_stream">>, <<"quic_code">> := Code}, Judged, _Before) ->
    ?assertEqual({abort_stream, Code}, Judged).

%% The counter a case names went up, under any label: once per frame, or once per extension a frame carries.
counted(#{<<"count">> := Name}, Before) ->
    ?assert(total(Name, macula_wire_tolerance:counters()) > total(Name, Before));
counted(_Expect, _Before) ->
    ok.

total(Name, Counters) ->
    Counter = binary_to_existing_atom(Name),
    lists:sum([N || {C, _Label} := N <- Counters, C =:= Counter] ++ [N || C := N <- Counters, C =:= Counter]).

judged(#{<<"length_header">> := Hex}) ->
    length_judged(macula_frame:parse_received(binary:decode_hex(Hex)));
judged(#{<<"frame">> := Hex, <<"position">> := <<"control_stream">>}) ->
    control_judged(macula_record_cbor:decode_strict(binary:decode_hex(Hex)));
judged(#{<<"frame">> := Hex, <<"position">> := <<"per_hop">>}) ->
    control_judged(macula_record_cbor:decode_strict(binary:decode_hex(Hex)));
judged(#{<<"frame">> := Hex, <<"position">> := <<"dedicated_stream">>}) ->
    dedicated_judged(macula_frame:parse_received(framed(Hex))).

length_judged({malformed, [], frame_too_large}) -> {refuse, <<"frame_too_large">>};
length_judged({ok, [], _Tail}) -> {accept, awaiting_body}.

%% After HELLO a connection decodes each frame once and reads it from its decoded value.
control_judged({ok, Wire}) -> read_judged(macula_frame:read_wire(Wire));
control_judged({error, _NotCbor}) -> {refuse, <<"malformed_frame">>}.

read_judged({ok, Frame}) -> {accept, Frame};
read_judged({error, {unknown_frame_type, _Name}}) -> ignore;
read_judged({error, extension_unsupported}) -> {refuse, <<"extension_unsupported">>};
read_judged({error, bad_frame}) -> {refuse, <<"malformed_frame">>}.

dedicated_judged({ok, [#{} = Frame], <<>>}) -> {accept, Frame};
dedicated_judged({ok, [{unknown_frame_type, _Name}], <<>>}) -> {abort_stream, ?QUIC_CODE_UNKNOWN_FRAME_TYPE};
dedicated_judged({ok, [{invalid_frame, _Type, _Field}], <<>>}) -> {refuse, <<"malformed_frame">>};
dedicated_judged({malformed, [], _Reason}) -> {refuse, <<"malformed_frame">>}.

framed(Hex) ->
    Bytes = binary:decode_hex(Hex),
    <<(byte_size(Bytes)):32/big, Bytes/binary>>.

%%------------------------------------------------------------------
%% Enums: a per-hop fallback, or a refusal by name inside a tbs
%%------------------------------------------------------------------

enum_checked(#{<<"input">> := #{<<"position">> := <<"tbs">>} = Input, <<"profile">> := P,
               <<"expect">> := Expect}) ->
    tbs_enum_checked(Expect, signed_judged(Input, binary_to_atom(P)));
enum_checked(#{<<"input">> := Input, <<"expect">> := #{<<"outcome">> := <<"fallback">>} = Expect}) ->
    Before = macula_wire_tolerance:counters(),
    {accept, Frame} = judged(Input),
    fallback_checked(Expect, Frame),
    #{<<"count">> := Name, <<"field">> := Field} = Expect,
    Counter = binary_to_existing_atom(Name),
    Label = binary_to_existing_atom(Field),
    ?assertEqual(maps:get({Counter, Label}, Before, 0) + 1, maps:get({Counter, Label}, macula_wire_tolerance:counters()));
enum_checked(#{<<"input">> := Input, <<"expect">> := #{<<"outcome">> := <<"accept">>, <<"field">> := Field,
                                                       <<"value">> := Value}}) ->
    {accept, Frame} = judged(Input),
    ?assertEqual(binary_to_existing_atom(Value), maps:get(binary_to_existing_atom(Field), Frame)).

fallback_checked(#{<<"value">> := <<"entry_ignored">>, <<"entries_read">> := Read}, #{piggyback := Updates}) ->
    ?assertEqual(Read, length(Updates));
fallback_checked(#{<<"field">> := Field, <<"value">> := Value}, Frame) ->
    ?assertEqual(binary_to_existing_atom(Value), maps:get(binary_to_existing_atom(Field), Frame)).

tbs_enum_checked(#{<<"outcome">> := <<"accept">>}, Judged) ->
    ?assertMatch({ok, _}, Judged);
tbs_enum_checked(#{<<"outcome">> := <<"refuse">>, <<"reason">> := <<"enum_unknown">>, <<"field">> := Field}, Judged) ->
    ?assertEqual({error, {enum_unknown, binary_to_existing_atom(Field)}}, Judged).

%% A signed object judged by its verifier, with the frames before it on the stream (its STREAM_OPEN) as context.
signed_judged(#{<<"frame">> := Hex, <<"context">> := Context}, P) ->
    Frame = decoded(Hex),
    signed_judged(Frame, [decoded(C) || C <- Context], P).

signed_judged(#{frame_type := stream_open} = Open, [], P) ->
    macula_frame:verify_request(Open, P);
signed_judged(#{frame_type := error} = Frame, [Open], P) ->
    {ok, Request} = macula_frame:verify_request(Open, P),
    macula_frame:verify_relay_error(Frame, Request, P, key_id(P, <<"station_identity">>));
signed_judged(#{caller_stream := _} = Frame, [Open], P) ->
    {ok, Request} = macula_frame:verify_request(Open, P),
    element_ok(macula_frame:verify_caller_stream(Frame, macula_frame:open_stream(Request), P)).

element_ok({ok, Read, _State}) -> {ok, Read};
element_ok(Refused) -> Refused.

decoded(Hex) ->
    {ok, [Frame], <<>>} = macula_frame:parse_received(framed(Hex)),
    Frame.

%%------------------------------------------------------------------
%% tbs: the builder that writes a signed structure writes exactly these bytes
%%------------------------------------------------------------------

tbs_checked(#{<<"id">> := Id, <<"profile">> := P, <<"input">> := #{<<"fields">> := Fields, <<"signer">> := Role},
              <<"expect">> := #{<<"tbs">> := Tbs}}) ->
    Key = private_key(binary_to_atom(P), binary_to_atom(Role)),
    ?assertEqual(binary:decode_hex(Tbs), built_tbs(structure(Id), from_tj(Fields), Key)).

structure(<<"tbs.request.", _/binary>>) -> request;
structure(<<"tbs.reply.", _/binary>>) -> reply;
structure(<<"tbs.relay_error.", _/binary>>) -> relay_error;
structure(<<"tbs.stream.provider.", _/binary>>) -> provider_stream;
structure(<<"tbs.stream.caller.", _/binary>>) -> caller_stream;
structure(<<"tbs.publication.", _/binary>>) -> publication.

built_tbs(request, #{frame_type := Type} = F, Key) ->
    Spec = maps:with([request_id, realm, procedure, target, deadline, payload, mode, ext], F),
    #{request := #{tbs := Tbs}} = request_built(Type, Spec, Key),
    Tbs;
built_tbs(reply, #{frame_type := result} = F, Key) ->
    #{reply := #{tbs := Tbs}} = macula_frame:result((maps:with([payload, ext], F))#{request => request_named(F)}, Key),
    Tbs;
built_tbs(reply, #{frame_type := error} = F, Key) ->
    #{reply := #{tbs := Tbs}} =
        macula_frame:provider_error((maps:with([code, detail, ext], F))#{request => request_named(F)}, Key),
    Tbs;
built_tbs(relay_error, #{frame_type := Type, code := Code} = F, Key) ->
    #{relay_error := #{tbs := Tbs}} =
        macula_frame:relay_error((maps:with([ext], F))#{frame_type => Type, request => request_named(F), code => Code},
                                 Key),
    Tbs;
built_tbs(provider_stream, F, Key) ->
    #{stream := #{tbs := Tbs}} = macula_frame:provider_stream(stream_spec(F), Key, open_named(F)),
    Tbs;
built_tbs(caller_stream, F, Key) ->
    #{caller_stream := #{tbs := Tbs}} = macula_frame:caller_stream(stream_spec(F), Key, open_named(F)),
    Tbs;
built_tbs(publication, F, Key) ->
    #{publication := #{tbs := Tbs}} =
        macula_frame:publish(maps:with([realm, topic, seq, published_at, payload, ext], F), Key),
    Tbs.

request_built(call, Spec, Key) -> macula_frame:call(Spec, Key);
request_built(stream_open, Spec, Key) -> macula_frame:stream_open(Spec, Key).

%% The request a reply or a relay error answers, named by its id and hash, as a verifier holds it; the target is the
%% responder, since only the target answers.
request_named(#{request_id := Id, request_hash := Hash} = F) ->
    #{frame_type => call, request_id => Id, request_hash => Hash,
      target => maps:get(responded_by, F, maps:get(reported_by, F, <<0:256>>))}.

%% The STREAM_OPEN a stream frame belongs to, named by its id and hash.
open_named(#{request_id := Id, request_hash := Hash}) ->
    #{frame_type => stream_open, request_id => Id, request_hash => Hash, mode => bidi}.

stream_spec(F) ->
    maps:with([frame_type, seq, encoding, body, role, code, message, payload, ext], F).

%% Typed JSON (FORMAT.md, Values) to a builder's spec: field names as atoms, text as binaries the builder writes as
%% text, enum values as atoms, bytes as binaries, and maps a builder carries as they are given.
from_tj(Fields) ->
    maps:from_list([{binary_to_atom(K), field_value(K, V)} || K := V <- Fields]).

field_value(<<"ext">>, Ext) -> #{Name => #{critical => C, value => tj(V)} || Name := #{<<"critical">> := C,
                                                                                    <<"value">> := V} <- Ext};
field_value(<<"payload">>, Payload) -> tj(Payload);
field_value(K, V) when K =:= <<"frame_type">>; K =:= <<"mode">>; K =:= <<"code">>, is_binary(V) ->
    enum_or_text(K, V);
field_value(K, V) when K =:= <<"encoding">>; K =:= <<"role">> -> binary_to_atom(V);
field_value(_K, #{<<"$hex">> := Hex}) -> binary:decode_hex(Hex);
field_value(_K, V) -> V.

%% A relay error's code is an enum; a provider error's code is text.
enum_or_text(<<"code">>, V) -> relay_code(lists:member(V, [<<"unknown_next_peer">>, <<"overloaded">>]), V);
enum_or_text(_K, V) -> binary_to_atom(V).

relay_code(true, V) -> binary_to_atom(V);
relay_code(false, V) -> V.

%% A carried value in the encoder's terms: text as {text, Bin}, and JSON null as undefined, which the encoder writes as
%% CBOR null.
tj(null) -> undefined;
tj(#{<<"$hex">> := Hex} = M) when map_size(M) =:= 1 -> binary:decode_hex(Hex);
tj(M) when is_map(M) -> #{{text, K} => tj(V) || K := V <- M};
tj(L) when is_list(L) -> [tj(V) || V <- L];
tj(B) when is_binary(B) -> {text, B};
tj(V) -> V.

%%------------------------------------------------------------------
%% Vectors and keys
%%------------------------------------------------------------------

key_id(P, Role) ->
    #{Role := #{<<"key_id">> := KeyId}} = maps:get(atom_to_binary(P), maps:get(<<"profiles">>, vectors(<<"keys">>))),
    binary:decode_hex(KeyId).

%% The generator's test-only private keys, so a builder can sign what the vectors hold.
private_key(P, Role) ->
    {ok, Bytes} = file:read_file("test/fixtures/wire_1_0_private_test_keys.etf"),
    maps:get({P, Role}, binary_to_term(Bytes)).

cases(Class) -> maps:get(<<"cases">>, vectors(Class)).

vectors(Class) ->
    {ok, Bytes} = file:read_file(filename:join([code:priv_dir(macula), "vectors", "wire_1_0",
                                                <<Class/binary, ".json">>])),
    json:decode(Bytes).
