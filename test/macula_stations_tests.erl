%% EUnit tests for parsing MACULA_STATIONS: comma-separated <node id>@<host>:<port> entries, the node id as 64 lowercase
%% hexadecimal characters, the host a DNS name, an IPv4 address or an IPv6 address in brackets, the port 1 to 65535. A
%% station becomes the seed the pool takes, pinned to its node id, with its host as given. A refusal names the entry's
%% position and what is wrong with it, and carries neither the value, nor a node id, nor a host.
-module(macula_stations_tests).

-include_lib("eunit/include/eunit.hrl").

-define(F, binary:copy(<<16#AB>>, 32)).
-define(T, binary:copy(<<16#01>>, 32)).
-define(V, binary:copy(<<16#7E>>, 32)).

accepted_test_() ->
    [{"entries of each host form parse, in their order, to seeds",
      ?_assertEqual({ok, [#{host => <<"station-de-frankfurt.macula.io">>, port => 4433, expected_node_id => ?F},
                          #{host => <<"203.0.113.9">>, port => 4433, expected_node_id => ?T},
                          #{host => <<"2001:db8::1">>, port => 9443, expected_node_id => ?V}]},
                    macula:parse_stations(joined([entry(?F, <<"station-de-frankfurt.macula.io">>, <<"4433">>),
                                                  entry(?T, <<"203.0.113.9">>, <<"4433">>),
                                                  entry(?V, <<"[2001:db8::1]">>, <<"9443">>)])))},
     {"a bracketed IPv6 host is kept as given, without its brackets",
      ?_assertEqual({ok, [#{host => <<"2001:db8::1">>, port => 4433, expected_node_id => ?V}]},
                    macula:parse_stations(entry(?V, <<"[2001:db8::1]">>, <<"4433">>)))},
     {"an IPv4-mapped IPv6 host is kept as given",
      ?_assertEqual({ok, [#{host => <<"::ffff:192.0.2.1">>, port => 4433, expected_node_id => ?V}]},
                    macula:parse_stations(entry(?V, <<"[::ffff:192.0.2.1]">>, <<"4433">>)))},
     {"64 stations parse",
      ?_assertMatch({ok, Seeds} when length(Seeds) =:= 64, macula:parse_stations(numbered(64)))},
     {"a single IPv4 station parses",
      ?_assertEqual({ok, [#{host => <<"203.0.113.9">>, port => 4433, expected_node_id => ?T}]},
                    macula:parse_stations(entry(?T, <<"203.0.113.9">>, <<"4433">>)))}].

refused_test_() ->
    Good = entry(?F, <<"station-a.example.org">>, <<"4433">>),
    Hex = hex(?F),
    DnsLabel64 = <<(binary:copy(<<"a">>, 64))/binary, ".example.org">>,
    Dns259 = iolist_to_binary(lists:join(<<".">>, lists:duplicate(26, <<"abcdefghi">>))),
    [refused(Value, Refusal)
     || {Value, Refusal} <-
            [{<<>>, empty},
             {numbered(65), {too_many, 64}},
             {<<Good/binary, ",">>, {entry, 2, form}},
             {<<"station-a.example.org:4433">>, {entry, 1, form}},
             {<<Hex/binary, "@station-a.example.org">>, {entry, 1, form}},
             {<<Hex/binary, "@2001:db8::1:4433">>, {entry, 1, form}},
             {entry(?V, <<"[::1">>, <<"4433">>), {entry, 1, form}},
             {entry(?V, <<"[::1]]">>, <<"4433">>), {entry, 1, form}},
             {<<(binary:part(Hex, 1, 63))/binary, "@station-a.example.org:4433">>, {entry, 1, node_id}},
             {<<Hex/binary, "0@station-a.example.org:4433">>, {entry, 1, node_id}},
             {<<(string:uppercase(Hex))/binary, "@station-a.example.org:4433">>, {entry, 1, node_id}},
             {entry(?F, <<"station-a.example.org">>, <<"0">>), {entry, 1, port}},
             {entry(?F, <<"station-a.example.org">>, <<"65536">>), {entry, 1, port}},
             {entry(?F, <<"station-a.example.org">>, <<"04433">>), {entry, 1, port}},
             {entry(?F, <<"station a.example.org">>, <<"4433">>), {entry, 1, host_dns}},
             {entry(?F, <<"-station.example.org">>, <<"4433">>), {entry, 1, host_dns}},
             {entry(?F, DnsLabel64, <<"4433">>), {entry, 1, host_dns}},
             {entry(?F, Dns259, <<"4433">>), {entry, 1, host_dns}},
             {entry(?F, <<"10.0.0.256">>, <<"4433">>), {entry, 1, host_ipv4}},
             {entry(?F, <<"10.0.0.05">>, <<"4433">>), {entry, 1, host_ipv4}},
             {entry(?F, <<"10.0.0">>, <<"4433">>), {entry, 1, host_ipv4}},
             {entry(?F, <<"[2001:db8::zz]">>, <<"4433">>), {entry, 1, host_ipv6}},
             {entry(?V, <<"[fe80::1%eth0]">>, <<"4433">>), {entry, 1, host_ipv6}},
             {entry(?V, <<"[fe80::1%a b]">>, <<"4433">>), {entry, 1, host_ipv6}},
             {joined([Good, entry(?F, <<"station-b.example.org">>, <<"4433">>)]), {repeated_node_id, 2, 1}},
             {joined([Good, entry(?T, <<"station-a.example.org">>, <<"4433">>)]), {repeated_host_and_port, 2, 1}}]].

%% A refused value gets exactly its refusal, which carries none of the value's fragments.
refused(Value, Refusal) ->
    {lists:flatten(io_lib:format("~p", [Refusal])),
     ?_test(begin
                ?assertEqual({error, Refusal}, macula:parse_stations(Value)),
                ?assertEqual([], fragments_in(Value, Refusal))
            end)}.

%% The fragments of Value a refusal must not carry, found in its external form: the whole value, and each entry's node
%% id and host of five bytes or more.
fragments_in(Value, Refusal) ->
    Carried = term_to_binary(Refusal),
    [Fragment || Fragment <- [Value | lists:append([entry_fragments(Entry)
                                                    || Entry <- binary:split(Value, <<",">>, [global])])],
                 byte_size(Fragment) >= 5, binary:match(Carried, Fragment) =/= nomatch].

entry_fragments(Entry) ->
    split_entry(binary:split(Entry, <<"@">>)).

split_entry([NodeId, HostAndPort]) -> [NodeId, host_of(HostAndPort)];
split_entry([_NoNodeId]) -> [].

host_of(HostAndPort) ->
    iolist_to_binary(lists:join(<<":">>, lists:droplast(binary:split(HostAndPort, <<":">>, [global])))).

hex(Bytes) ->
    binary:encode_hex(Bytes, lowercase).

entry(NodeId, Host, Port) ->
    <<(hex(NodeId))/binary, "@", Host/binary, ":", Port/binary>>.

joined(Entries) ->
    iolist_to_binary(lists:join(<<",">>, Entries)).

numbered(Count) ->
    joined([entry(<<N:256>>, <<"station-", (integer_to_binary(N))/binary, ".example.org">>, <<"4433">>)
            || N <- lists:seq(1, Count)]).
