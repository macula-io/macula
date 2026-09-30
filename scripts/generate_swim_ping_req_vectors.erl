%% Writes test/vectors/swim_ping_req_v1.json (macula#59): a PING-REQ frame with the round and target a reader must get
%% back, and frames a reader must refuse, each with the field its refusal names. Run through
%% scripts/generate-swim-ping-req-vectors.sh with the compiled tree on the code path. The frames are unsigned, as on a
%% v5 link; on a v4 pq_hybrid link the same frame travels inside its neighbour signature.
Hex = fun(Bin) -> binary:encode_hex(Bin, lowercase) end,
Target = binary:decode_hex(<<"65358c573190c2caf5b6c3ef18e8f4560f5743fdf67bd0a5b8f72bd6a662151e">>),
Valid = macula_frame:swim_ping_req(#{round => 7, target => Target}),
Wire = fun(Frame) -> Bytes = macula_frame:encode(Frame), {Bytes, macula_frame:decode(Bytes)} end,
Accepted = fun(Name, Frame) ->
    {Bytes, {ok, Read, <<>>}} = Wire(Frame),
    #{<<"name">> => Name, <<"frame">> => Hex(Bytes), <<"verdict">> => <<"accepted">>,
      <<"round">> => maps:get(round, Read), <<"target">> => Hex(maps:get(target, Read))}
end,
%% A reader refuses a field of the wrong kind while reading the field table (bad_frame), and a frame of the wrong
%% shape after it (invalid_frame, naming the first field refused).
Reason = fun(bad_frame) -> <<"bad_frame">>;
            ({invalid_frame, swim_ping_req, Field}) -> <<"invalid_frame:", (atom_to_binary(Field))/binary>>
         end,
Refused = fun(Name, Frame) ->
    {Bytes, {error, Refusal}} = Wire(Frame),
    #{<<"name">> => Name, <<"frame">> => Hex(Bytes), <<"verdict">> => <<"refused">>,
      <<"reason">> => Reason(Refusal)}
end,
Vectors = #{<<"scheme">> => 1,
            <<"generator">> => <<"scripts/generate-swim-ping-req-vectors.sh">>,
            <<"note">> => <<"frames are length-prefixed as on the wire; a refused frame gives the reader's reason, bad_frame or invalid_frame:<first field refused>">>,
            <<"cases">> => [Accepted(<<"round_and_target">>, Valid),
                            Refused(<<"target_31_bytes">>, Valid#{target := binary:part(Target, 0, 31)}),
                            Refused(<<"round_negative">>, Valid#{round := -1}),
                            Refused(<<"target_missing">>, maps:remove(target, Valid)),
                            Refused(<<"extra_responder">>, Valid#{responder => Target})]},
ok = file:write_file("test/vectors/swim_ping_req_v1.json", [json:format(Vectors), "\n"]),
io:format("wrote test/vectors/swim_ping_req_v1.json~n"),
halt(0).
