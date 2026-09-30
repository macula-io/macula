%% Writes test/vectors/station_endpoint_v1.json: signed station endpoint records in both crypto profiles, one naming the
%% station's release (station_version) and one from before the field, each with the QUIC port, advertised hosts and
%% release a reader of it must get back at the vector's clock. Run through scripts/generate-station-endpoint-vectors.sh
%% with the compiled tree on the code path.
%%
%% ML-DSA-87 signing is hedged and pq_hybrid's RSA-PSS half is randomized, so a second run signs other bytes with the
%% same readings: the committed file is the vector, generated once. macula_station_endpoint_vectors_tests re-derives
%% every reading from it on each run.
{ok, _} = application:ensure_all_started(crypto),
Hex = fun(Bin) -> binary:encode_hex(Bin, lowercase) end,
Port = 4433,
Hosts = [<<"2001:db8::7">>],
Release = <<"0.7.4">>,
Profile = fun(P) ->
    {ok, Station} = macula_node_keys:generate(identity, P),
    {ok, NodeId} = macula_node_keys:node_id(Station),
    Unsigned = fun(Opts) -> macula_record:station_endpoint(Port, Opts#{host_advertised => Hosts}) end,
    %% A payload the constructor would not make, signed as a station could: the reader's rule must read no release.
    Forged = fun(Value) ->
        #{payload := P0} = U = Unsigned(#{}),
        U#{payload := P0#{{text, <<"station_version">>} => Value}}
    end,
    Case = fun(Name, Record) ->
        Signed = macula_record:sign(Record, Station),
        Bytes = macula_record:encode(Signed),
        #{created_at := Created} = Signed,
        {ok, Read} = macula_record:verify(Bytes, P, Created + 1_000),
        Reading = macula_record:read_station_endpoint(Read),
        #{<<"name">> => Name, <<"record">> => Hex(Bytes), <<"now_ms">> => Created + 1_000,
          <<"quic_port">> => maps:get(quic_port, Reading),
          <<"host_advertised">> => maps:get(host_advertised, Reading),
          <<"station_version">> => maps:get(station_version, Reading, null)}
    end,
    {atom_to_binary(P),
     #{<<"signer_node_id">> => Hex(NodeId),
       <<"cases">> => [Case(<<"with_release">>, Unsigned(#{station_version => Release})),
                       Case(<<"without_release">>, Unsigned(#{})),
                       Case(<<"release_as_bytes">>, Forged(Release)),
                       Case(<<"release_empty_text">>, Forged({text, <<>>})),
                       Case(<<"release_past_64_bytes">>, Forged({text, binary:copy(<<"9">>, 65)}))]}}
end,
Vectors = #{<<"scheme">> => 1,
            <<"generator">> => <<"scripts/generate-station-endpoint-vectors.sh">>,
            <<"note">> => <<"station_version is null where a reader returns no release: none carried, or not text of 1 to 64 bytes">>,
            <<"profiles">> => maps:from_list([Profile(P) || P <- [pq_pure, pq_hybrid]])},
ok = file:write_file("test/vectors/station_endpoint_v1.json", [json:format(Vectors), "\n"]),
io:format("wrote test/vectors/station_endpoint_v1.json~n"),
halt(0).
