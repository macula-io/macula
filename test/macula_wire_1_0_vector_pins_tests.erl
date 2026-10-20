%% The values the wire 1.0 vector generator pins, asserted over the committed vectors (priv/vectors/wire_1_0/,
%% macula#113). The drift check (scripts/wire_1_0_vectors_drift.erl) derives its per-run set from two generations of
%% the same code, so a pin that is removed, and goes back to being minted per run, leaves its coverage without a
%% failure. Here the pinned values are named, so a regeneration that stops pinning one fails. A pin added to the
%% generator later gets its line here when it lands.
-module(macula_wire_1_0_vector_pins_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CLASSES, [<<"verify">>, <<"custodian">>, <<"check_order">>, <<"envelope">>, <<"enums">>,
                  <<"dht_procedures">>, <<"handshake">>, <<"tbs">>, <<"frames">>]).
%% The vectors' clock, and the record version the generator mints at it with no randomness (macula#112).
-define(T0, 1791800000000).
-define(VERSION, <<?T0:48, 7:4, 0:12, 2:2, 0:62>>).
%% The version of the record a tombstone withdraws, one millisecond below ?VERSION.
-define(WITHDRAWN_VERSION, <<(?T0 - 1):48, 7:4, 0:12, 2:2, 0:62>>).
%% The literal versions a few cases set by hand: the tbs class's record, and a tombstone's withdrawn record.
-define(TBS_VERSION, <<16#0190:16, 16#7A:8, 0:104>>).
-define(LITERAL_VERSION, <<16#0190:16, 0:112>>).
-define(HOUR, 3600000).
%% The frame cases that carry a record, and the key it sits under (macula#109).
-define(EMBEDDED, [{<<"frames.value">>, <<"records">>}, {<<"frames.store">>, <<"record">>},
                   {<<"frames.advertise">>, <<"advertisement">>}, {<<"frames.unadvertise">>, <<"withdrawal">>}]).

%% Every record version is the pinned one, or one of the literals a case sets by hand. This also holds the
%% foundation seed list's and parameter's payload version, which copy the record's.
record_versions_test() ->
    ?assertEqual(lists:sort([?VERSION, ?TBS_VERSION]), carried(<<"version">>, fun uuid/1)).

withdrawn_versions_test() ->
    ?assertEqual(lists:sort([?WITHDRAWN_VERSION, ?LITERAL_VERSION]), carried(<<"withdrawn_version">>, fun uuid/1)).

%% Every created_at is the vectors' clock, or an offset from it a case sets on purpose: a record created two hours
%% back, and two created after the clock its reader judges at. The realm directory payload's created_at is pinned
%% too (macula#111).
created_at_test() ->
    ?assertEqual([?T0 - 2 * ?HOUR, ?T0, ?T0 + 361000, ?T0 + 601000],
                 carried(<<"created_at">>, fun erlang:is_integer/1)).

%% The record the value, store, advertise and unadvertise frames carry is built at the vectors' clock (macula#109):
%% version ?VERSION, created at ?T0, expiring a minute inside a node record's 48 hour bound.
embedded_records_test_() ->
    Frames = by_id(<<"frames">>),
    [{binary_to_list(Id), fun() -> embedded_checked(frame(maps:get(Id, Frames)), Key) end} || {Id, Key} <- ?EMBEDDED].

embedded_checked(Frame, Key) ->
    Tbs = tbs(embedded(maps:get({text, Key}, Frame))),
    ?assertEqual(?VERSION, maps:get({text, <<"version">>}, Tbs)),
    ?assertEqual(?T0, maps:get({text, <<"created_at">>}, Tbs)),
    ?assertEqual(?T0 + 48 * ?HOUR - 60000, maps:get({text, <<"expires_at">>}, Tbs)).

embedded([Bytes]) -> Bytes;
embedded(Bytes) when is_binary(Bytes) -> Bytes.

%% The frame overlay_relay carries has its id and clock pinned, so its payload does not move with the frame
%% encoder's own clock (macula#109).
relayed_frame_test() ->
    Relay = frame(maps:get(<<"frames.overlay_relay">>, by_id(<<"frames">>))),
    <<Len:32, Cbor:Len/binary>> = maps:get({text, <<"payload">>}, Relay),
    Inner = macula_record_cbor:decode(Cbor),
    ?assertEqual(<<16#30:128>>, maps:get({text, <<"frame_id">>}, Inner)),
    ?assertEqual(?T0, maps:get({text, <<"sent_at_ms">>}, Inner)).

%%------------------------------------------------------------------
%% Every value carried under Field in any class, at any depth, through the Cbor nested in hex and in bytes; Keep
%% picks the values that are the pinned field's kind (a frame's integer `version' is not a record's).
carried(Field, Keep) ->
    lists:usort([V || Class <- ?CLASSES, C <- cases(Class), V <- values(Field, C), Keep(V)]).

values(Field, M) when is_map(M) ->
    lists:append([[V || name(K) =:= Field] ++ values(Field, V) || K := V <- M]);
values(Field, L) when is_list(L) ->
    lists:append([values(Field, V) || V <- L]);
values(Field, B) when is_binary(B) ->
    nested(Field, cbor(hex(B)));
values(_Field, _V) ->
    [].

nested(Field, {ok, T}) -> values(Field, T);
nested(_Field, none) -> [].

hex(B) ->
    try binary:decode_hex(B) catch error:badarg -> B end.

cbor(Bytes) when byte_size(Bytes) > 1 ->
    try macula_record_cbor:decode(Bytes) of
        T when is_map(T); is_list(T) -> {ok, T};
        _ -> none
    catch _:_ -> none
    end;
cbor(_Bytes) ->
    none.

name({text, K}) -> K;
name(K) -> K.

uuid(V) -> is_binary(V) andalso byte_size(V) =:= 16.

tbs(Object) ->
    #{{text, <<"tbs">>} := Tbs} = macula_record_cbor:decode(Object),
    macula_record_cbor:decode(Tbs).

frame(#{<<"input">> := #{<<"frame">> := Hex}}) ->
    macula_record_cbor:decode(binary:decode_hex(Hex)).

by_id(Class) -> #{maps:get(<<"id">>, C) => C || C <- cases(Class)}.

cases(Class) ->
    {ok, Bytes} = file:read_file(filename:join([code:priv_dir(macula), "vectors", "wire_1_0",
                                                <<Class/binary, ".json">>])),
    maps:get(<<"cases">>, json:decode(Bytes)).
