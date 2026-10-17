%% Checks the committed wire 1.0 conformance vectors against two fresh generations of them.
%%
%% The per-run set is DERIVED, never listed. Whatever differs between two runs of the same code is per-run
%% material by definition, a signature or a v7 version or a frame id or a clock. Anything that differs from the
%% committed vectors OUTSIDE that set is drift, and this exits non-zero.
%%
%% The comparison is structural, below the hex leaf. A vector's object is Cbor carried as hex, so comparing
%% leaf strings would call every frame case per-run (each carries a random frame id and a wall-clock time) and
%% would see none of the drift: an object gaining a field changes its whole leaf. Nested signed objects and the
%% relayed inner frame are decoded and walked the same way, which is what makes drift visible.
%%
%% It also puts each frame case's embedded object to the reader at the vectors' clock, so an object whose clock
%% rots with the wall calendar, or whose lifetime outgrows its row, fails here.
%%
%% The limit worth knowing: a value that is per-run in the CURRENT code cannot be drift-checked, because two runs
%% disagree about it by construction. That is what pinning a field is for, and it is why the relayed frame's id
%% and clock are pinned rather than left to the encoder: a payload that moved on every run would hide a
%% frame-encoder change from this check instead of exposing it.
-module(wire_1_0_vectors_drift).

-export([main/1]).

%% The classes the generator writes, in the order it writes them.
-define(CLASSES, [<<"keys">>, <<"verify">>, <<"custodian">>, <<"check_order">>, <<"envelope">>, <<"enums">>,
                  <<"dht_procedures">>, <<"handshake">>, <<"tbs">>, <<"frames">>]).
%% The frame cases that carry a record, and the key it sits under. Each is put to the reader below.
-define(EMBEDDED, [{<<"frames.value">>, <<"records">>}, {<<"frames.store">>, <<"record">>},
                   {<<"frames.advertise">>, <<"advertisement">>}, {<<"frames.unadvertise">>, <<"withdrawal">>}]).
%% The profile the frames class is built for, and the clock its reader judges at, one second past the vectors'
%% own origin.
-define(PROFILE, pq_pure).
-define(NOW_MS, 1791800001000).

%% main([CommittedDir, FirstDir, SecondDir])
-spec main([string()]) -> no_return().
main([Committed, First, Second]) ->
    Drift = lists:append([class(Class, Committed, First, Second) || Class <- ?CLASSES]),
    Unread = embedded_checked(Committed),
    case Drift ++ Unread of
        [] ->
            io:format("~p classes agree with two fresh generations, and every embedded object reads~n",
                      [length(?CLASSES)]),
            halt(0);
        Problems ->
            [io:format("FAIL: ~s~n", [P]) || P <- Problems],
            halt(1)
    end.

%% One class across the three copies: the same cases in the same order, and no difference from the committed
%% file that two runs of the same code do not also produce.
class(Class, Committed, First, Second) ->
    C = read(Committed, Class),
    F = read(First, Class),
    S = read(Second, Class),
    File = <<Class/binary, ".json">>,
    case ids(F) =:= ids(C) andalso ids(S) =:= ids(C) of
        false ->
            [<<File/binary, ": the case ids, or their order, differ from the committed file">>];
        true ->
            PerRun = diff(F, S, <<>>),
            [<<File/binary, ": ", P/binary, " moved and is not per-run">>
             || P <- diff(C, F, <<>>), not lists:member(P, PerRun)]
    end.

ids(#{<<"cases">> := Cases}) -> [maps:get(<<"id">>, C) || C <- Cases];
ids(_File) -> undefined.

read(Dir, Class) ->
    {ok, Bin} = file:read_file(filename:join(Dir, <<Class/binary, ".json">>)),
    json:decode(Bin).

%%------------------------------------------------------------------
%% The structural walk

%% The paths at which two decoded documents differ, decoding Cbor as it goes. A path is its route through the
%% document, so it names the field rather than the bytes.
diff(A, B, _Path) when A =:= B -> [];
diff({decoded, A}, {decoded, B}, Path) -> diff(A, B, Path);
diff(A, B, Path) when is_binary(A), is_binary(B) ->
    case {decoded(A), decoded(B)} of
        {{decoded, TA}, {decoded, TB}} -> diff(TA, TB, Path);
        _ -> [Path]
    end;
diff(A, B, Path) when is_map(A), is_map(B) ->
    lists:append([key(K, A, B, Path) || K <- lists:usort(maps:keys(A) ++ maps:keys(B))]);
diff(A, B, Path) when is_list(A), is_list(B), length(A) =:= length(B) ->
    lists:append([diff(VA, VB, index(Path, I))
                  || {I, VA, VB} <- lists:zip3(lists:seq(1, length(A)), A, B)]);
diff(_A, _B, Path) -> [Path].

key(K, A, B, Path) ->
    Sub = <<Path/binary, "/", (name(K))/binary>>,
    case {maps:find(K, A), maps:find(K, B)} of
        {{ok, VA}, {ok, VB}} -> diff(VA, VB, Sub);
        _ -> [Sub]
    end.

%% A JSON string is hex at the top level and raw Cbor below it, so both are tried.
decoded(Bin) when byte_size(Bin) > 1 ->
    try binary:decode_hex(Bin) of
        Bytes -> cbor(Bytes, Bin)
    catch _:_ ->
        cbor(Bin, Bin)
    end;
decoded(Bin) ->
    Bin.

cbor(Bytes, Fallback) when byte_size(Bytes) > 1 ->
    try macula_record_cbor:decode(Bytes) of
        T when is_map(T); is_list(T) -> {decoded, T};
        _ -> Fallback
    catch _:_ -> Fallback
    end;
cbor(_Bytes, Fallback) ->
    Fallback.

%% Every element of a list carries its index, so a path names a field IN A CASE. Without the index the per-run
%% set would be per field NAME, and a field that is per-run in one case would excuse the same field name in a
%% sibling case, which is exactly where a drifted fixed value hides.
index(Path, I) -> <<Path/binary, "/", (integer_to_binary(I))/binary>>.

name(K) when is_binary(K) -> K;
name(K) -> iolist_to_binary(io_lib:format("~p", [K])).

%%------------------------------------------------------------------
%% The embedded objects

embedded_checked(Dir) ->
    #{<<"cases">> := Cases} = read(Dir, <<"frames">>),
    ById = maps:from_list([{maps:get(<<"id">>, C), C} || C <- Cases]),
    lists:append([check_embedded(Id, Key, ById) || {Id, Key} <- ?EMBEDDED]).

check_embedded(Id, Key, ById) ->
    Frame = frame_of(maps:get(Id, ById)),
    case maps:find({text, Key}, Frame) of
        {ok, Bytes} when is_binary(Bytes) -> verdict(Id, Bytes);
        {ok, [Bytes | _]} when is_binary(Bytes) -> verdict(Id, Bytes);
        Other -> [<<Id/binary, ": no object under ", Key/binary, ": ", (name(Other))/binary>>]
    end.

frame_of(#{<<"input">> := #{<<"frame">> := Hex}}) ->
    macula_record_cbor:decode(binary:decode_hex(Hex)).

verdict(Id, Bytes) ->
    case macula_record:verify(Bytes, ?PROFILE, ?NOW_MS) of
        {ok, _} -> [];
        Other -> [<<Id/binary, ": the embedded object does not read: ", (name(Other))/binary>>]
    end.
