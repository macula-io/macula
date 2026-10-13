%% @doc The argument and reply shapes of the `_dht.*' procedures every station serves in the zero realm (wire 1.0,
%% macula#102, docs/design/WIRE_1_0.md section 6). One argument form, a map with text keys and `v' 1, and one reply
%% form per procedure, so an LTS station and a client of any language keep reaching the DHT.
%%
%% A station reads arguments with read_args/2, refusing another `v', a missing or extra key, or a field of the wrong
%% type or length as `malformed_args' before any DHT work, and answers with reply/2. A caller builds arguments with
%% args/2 and reads the answer with read_reply/2: another `v' is `reply_version_unsupported', a missing or extra key or
%% a value of the wrong shape is `malformed_reply'. A caller verifies every record it gets back as a reader
%% (`macula_record:verify/3') and drops those that fail; that is the caller's, not this module's.
%%
%% Values travel in the frame payload's one key form (D26): text read as `{text, Bin}', bytes as binaries. Built
%% values use atom keys and atoms for text, which the frame encoder writes as text.
-module(macula_dht_procedures).

-export([procedure/1, procedure_name/1, args/2, read_args/2, reply/2, read_reply/2]).

-export_type([procedure/0, argument/0, result/0]).

-type procedure() :: put_record | find_record | find_records | find_records_by_type.
%% A put's wire record, a find's 32-byte storage key, or a record type tag.
-type argument() :: binary() | <<_:256>> | 0..255.
%% A put's answer, a find's record or its absence, or the records at a key or of a type.
-type result() :: ok | {refused, binary()} | {record, binary()} | not_found | {records, [binary()]}.

-define(V1, 1).
%% A refused put's reason is a custodian's refusal name (WIRE_1_0.md 5.2), at most this many bytes.
-define(MAX_REASON_BYTES, 64).

%%------------------------------------------------------------------
%% Procedure names
%%------------------------------------------------------------------

%% @doc The procedure a `_dht.*' name names, or `error' for any other name.
-spec procedure(binary()) -> {ok, procedure()} | error.
procedure(<<"_dht.put_record">>) -> {ok, put_record};
procedure(<<"_dht.find_record">>) -> {ok, find_record};
procedure(<<"_dht.find_records">>) -> {ok, find_records};
procedure(<<"_dht.find_records_by_type">>) -> {ok, find_records_by_type};
procedure(_Other) -> error.

%% @doc The wire name of a `_dht.*' procedure.
-spec procedure_name(procedure()) -> binary().
procedure_name(put_record) -> <<"_dht.put_record">>;
procedure_name(find_record) -> <<"_dht.find_record">>;
procedure_name(find_records) -> <<"_dht.find_records">>;
procedure_name(find_records_by_type) -> <<"_dht.find_records_by_type">>.

%%------------------------------------------------------------------
%% Arguments
%%------------------------------------------------------------------

%% @doc A procedure's arguments: `{v 1, record}' for a put, `{v 1, key}' for a find at a key, `{v 1, type}' for a
%% find by type.
-spec args(procedure(), argument()) -> map().
args(put_record, Wire) when is_binary(Wire) -> #{v => ?V1, record => Wire};
args(find_record, <<_:256>> = Key) -> #{v => ?V1, key => Key};
args(find_records, <<_:256>> = Key) -> #{v => ?V1, key => Key};
args(find_records_by_type, Type) when is_integer(Type), Type >= 0, Type =< 255 -> #{v => ?V1, type => Type}.

%% @doc The argument a station reads from a call's payload: exactly `v' 1 and the procedure's one field, of its type
%% and length, else `malformed_args'.
-spec read_args(procedure(), term()) -> {ok, argument()} | {error, malformed_args}.
read_args(put_record, #{{text, <<"v">>} := ?V1, {text, <<"record">>} := Wire} = Args)
  when map_size(Args) =:= 2, is_binary(Wire) ->
    {ok, Wire};
read_args(Find, #{{text, <<"v">>} := ?V1, {text, <<"key">>} := <<_:256>> = Key} = Args)
  when map_size(Args) =:= 2, (Find =:= find_record orelse Find =:= find_records) ->
    {ok, Key};
read_args(find_records_by_type, #{{text, <<"v">>} := ?V1, {text, <<"type">>} := Type} = Args)
  when map_size(Args) =:= 2, is_integer(Type), Type >= 0, Type =< 255 ->
    {ok, Type};
read_args(_Procedure, _Args) ->
    {error, malformed_args}.

%%------------------------------------------------------------------
%% Replies
%%------------------------------------------------------------------

%% @doc A station's reply: a put's `ok' or `refused' with the custodian's refusal name, a find's `record' or
%% `not_found', or the `records' at a key or of a type, possibly none.
-spec reply(procedure(), result() | {refused, atom()}) -> map().
reply(put_record, ok) ->
    #{v => ?V1, result => ok};
reply(put_record, {refused, Reason}) when is_atom(Reason) ->
    reply(put_record, {refused, atom_to_binary(Reason)});
reply(put_record, {refused, Reason}) when is_binary(Reason), byte_size(Reason) =< ?MAX_REASON_BYTES ->
    #{v => ?V1, result => refused, reason => {text, Reason}};
reply(find_record, {record, Wire}) when is_binary(Wire) ->
    #{v => ?V1, result => record, record => Wire};
reply(find_record, not_found) ->
    #{v => ?V1, result => not_found};
reply(Find, {records, Wires}) when Find =:= find_records; Find =:= find_records_by_type ->
    true = lists:all(fun erlang:is_binary/1, Wires),
    #{v => ?V1, result => records, records => Wires}.

%% @doc The result a caller reads from a station's reply. A `v' other than 1 is `reply_version_unsupported'; no `v',
%% a result the procedure does not answer, a missing or extra key or a value of the wrong shape is `malformed_reply'.
-spec read_reply(procedure(), term()) ->
          {ok, result()} | {error, reply_version_unsupported | malformed_reply}.
read_reply(Procedure, #{{text, <<"v">>} := ?V1, {text, <<"result">>} := {text, Result}} = Reply) ->
    replied(Procedure, Result, maps:without([{text, <<"v">>}, {text, <<"result">>}], Reply));
read_reply(_Procedure, #{{text, <<"v">>} := V}) when is_integer(V), V =/= ?V1 ->
    {error, reply_version_unsupported};
read_reply(_Procedure, _Reply) ->
    {error, malformed_reply}.

replied(put_record, <<"ok">>, Rest) when map_size(Rest) =:= 0 ->
    {ok, ok};
replied(put_record, <<"refused">>, #{{text, <<"reason">>} := {text, Reason}} = Rest)
  when map_size(Rest) =:= 1, is_binary(Reason), byte_size(Reason) =< ?MAX_REASON_BYTES ->
    {ok, {refused, Reason}};
replied(find_record, <<"record">>, #{{text, <<"record">>} := Wire} = Rest) when map_size(Rest) =:= 1, is_binary(Wire) ->
    {ok, {record, Wire}};
replied(find_record, <<"not_found">>, Rest) when map_size(Rest) =:= 0 ->
    {ok, not_found};
replied(Find, <<"records">>, #{{text, <<"records">>} := Wires} = Rest)
  when map_size(Rest) =:= 1, is_list(Wires), (Find =:= find_records orelse Find =:= find_records_by_type) ->
    records_read(lists:all(fun erlang:is_binary/1, Wires), Wires);
replied(_Procedure, _Result, _Rest) ->
    {error, malformed_reply}.

records_read(true, Wires) -> {ok, {records, Wires}};
records_read(false, _Wires) -> {error, malformed_reply}.
