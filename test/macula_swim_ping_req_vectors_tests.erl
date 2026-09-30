%% The PING-REQ vectors (test/vectors/swim_ping_req_v1.json, macula#59): encoded frames with what a reader must get
%% back, and frames it must refuse. The file is committed as generated (frame_id and sent_at_ms differ per run); this
%% module re-reads every case from it on each run, so the file can never drift from macula_frame.
-module(macula_swim_ping_req_vectors_tests).

-include_lib("eunit/include/eunit.hrl").

vectors_test_() ->
    [{binary_to_list(maps:get(<<"name">>, C)), fun() -> case_checked(C) end}
     || C <- maps:get(<<"cases">>, vectors())].

case_checked(#{<<"frame">> := Hex, <<"verdict">> := <<"accepted">>, <<"round">> := Round, <<"target">> := Target}) ->
    {ok, Frame, <<>>} = macula_frame:decode(binary:decode_hex(Hex)),
    ?assertEqual(swim_ping_req, macula_frame:frame_type(Frame)),
    ?assertEqual(Round, maps:get(round, Frame)),
    ?assertEqual(binary:decode_hex(Target), maps:get(target, Frame));
case_checked(#{<<"frame">> := Hex, <<"verdict">> := <<"refused">>, <<"reason">> := Reason}) ->
    ?assertEqual({error, refusal(Reason)}, macula_frame:decode(binary:decode_hex(Hex))).

refusal(<<"bad_frame">>) -> bad_frame;
refusal(<<"invalid_frame:", Field/binary>>) -> {invalid_frame, swim_ping_req, binary_to_atom(Field)}.

vectors() ->
    {ok, Bytes} = file:read_file(vector_file()),
    json:decode(Bytes).

%% The source tree's vector file, from the project root eunit runs in, or from the build tree's copy of the
%% application.
vector_file() ->
    Name = "swim_ping_req_v1.json",
    first_existing([filename:join("test/vectors", Name), filename:join("../../test/vectors", Name)]
                   ++ [filename:join([Dir, "..", "..", "..", "..", "test", "vectors", Name])
                       || Dir <- [code:lib_dir(macula)], is_list(Dir)]).

first_existing([F | Rest]) ->
    first_existing(filelib:is_regular(F), F, Rest).

first_existing(true, F, _Rest) -> F;
first_existing(false, _F, Rest) -> first_existing(Rest).
