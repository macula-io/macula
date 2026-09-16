%%%-------------------------------------------------------------------
%%% @doc Parse the `MACULA_STATIONS' seed list into the pinned seeds a
%%% pool takes: comma-separated `<node id>@<host>:<port>' entries, the
%%% node id as 64 lowercase hexadecimal characters, the host a DNS
%%% name, an IPv4 address or an IPv6 address in brackets, the port 1
%%% to 65535. A station becomes the seed the pool takes, pinned to its
%%% node id, with its host as given. A refusal names the entry's
%%% position and what is wrong with it, and carries neither the value,
%%% nor a node id, nor a host.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stations).

-export([parse/1]).

-export_type([seed/0]).

-type seed() :: #{host := binary(), port := inet:port_number(),
                  expected_node_id := <<_:256>>}.

-define(MAX_STATIONS, 64).

%% @doc Parse a `MACULA_STATIONS' value. Up to 64 entries, each
%% `<node id>@<host>:<port>'. Refusals name the entry's position:
%% `{entry, N, form | node_id | port | host_dns | host_ipv4 | host_ipv6}',
%% or a duplicate across entries `{repeated_node_id, N, FirstN}' /
%% `{repeated_host_and_port, N, FirstN}', or `{too_many, 64}' past the
%% cap, or `empty'. No refusal carries a node id, a host, or the value.
-spec parse(binary()) -> {ok, [seed()]} | {error, term()}.
parse(<<>>) ->
    {error, empty};
parse(Value) when is_binary(Value) ->
    Entries = binary:split(Value, <<",">>, [global]),
    case length(Entries) =< ?MAX_STATIONS of
        false ->
            {error, {too_many, ?MAX_STATIONS}};
        true ->
            seeds(parse_entries(Entries, 1, []))
    end.

%% Every entry parses, or the first refusal is the answer.
parse_entries([], _N, Acc) ->
    {ok, lists:reverse(Acc)};
parse_entries([Entry | Rest], N, Acc) ->
    case parse_entry(Entry, N) of
        {ok, Seed} -> parse_entries(Rest, N + 1, [Seed | Acc]);
        {error, _} = Refusal -> Refusal
    end.

%% Once every entry parsed, the duplicates across entries are judged in
%% position order: a repeated node id first, then a repeated host+port.
seeds({error, _} = Refusal) ->
    Refusal;
seeds({ok, Seeds}) ->
    seeds_no_dup(Seeds, 1, #{}, #{}).

seeds_no_dup([], _N, _Ids, _Hosts) ->
    {ok, []};
seeds_no_dup([#{expected_node_id := Id} = Seed | Rest], N, Ids, Hosts) ->
    HostPort = {maps:get(host, Seed), maps:get(port, Seed)},
    case maps:find(Id, Ids) of
        {ok, FirstN} ->
            {error, {repeated_node_id, N, FirstN}};
        error ->
            case maps:find(HostPort, Hosts) of
                {ok, FirstN} ->
                    {error, {repeated_host_and_port, N, FirstN}};
                error ->
                    case seeds_no_dup(Rest, N + 1, Ids#{Id => N},
                                      Hosts#{HostPort => N}) of
                        {ok, Seeds} -> {ok, [Seed | Seeds]};
                        {error, _} = Dup -> Dup
                    end
            end
    end.

%% One entry: `<64 lowercase hex node id>@<host>:<port>'.
parse_entry(Entry, N) ->
    case binary:split(Entry, <<"@">>) of
        [NodeIdHex, HostAndPort] ->
            node_id_checked(NodeIdHex, HostAndPort, N);
        _ ->
            {error, {entry, N, form}}
    end.

node_id_checked(NodeIdHex, HostAndPort, N) ->
    case byte_size(NodeIdHex) =:= 64 andalso is_lower_hex(NodeIdHex) of
        true ->
            host_and_port_checked(binary:decode_hex(NodeIdHex), HostAndPort, N);
        false ->
            {error, {entry, N, node_id}}
    end.

is_lower_hex(Bin) ->
    lists:all(fun is_lower_hex_byte/1, binary_to_list(Bin)).

is_lower_hex_byte(C) when C >= $0, C =< $9 -> true;
is_lower_hex_byte(C) when C >= $a, C =< $f -> true;
is_lower_hex_byte(_) -> false.

%% The host:port pair: a bracketed IPv6 with a port after the bracket,
%% or a single host and a port.
host_and_port_checked(NodeId, <<"[", Rest/binary>>, N) ->
    bracketed_ipv6_checked(NodeId, Rest, N);
host_and_port_checked(NodeId, HostAndPort, N) ->
    case binary:split(HostAndPort, <<":">>, [global]) of
        [Host, Port] -> host_and_port_finish(NodeId, Host, Port, N);
        _ -> {error, {entry, N, form}}
    end.

bracketed_ipv6_checked(NodeId, Rest, N) ->
    case binary:split(Rest, <<"]">>) of
        [Inner, <<":", Port/binary>>] ->
            ipv6_checked(NodeId, Inner, Port, N);
        _ ->
            {error, {entry, N, form}}
    end.

ipv6_checked(NodeId, Host, Port, N) ->
    case port_number(Port) of
        {ok, PortNumber} ->
            ipv6_host_checked(NodeId, Host, PortNumber, N);
        error ->
            {error, {entry, N, port}}
    end.

%% A zone id (`%eth0') is refused: inet parses it, but no seed carries
%% one, and a zone id names an interface, not a station.
ipv6_host_checked(NodeId, Host, Port, N) ->
    case binary:match(Host, <<"%">>) of
        nomatch ->
            case valid_ipv6(Host) of
                true -> {ok, #{host => Host, port => Port, expected_node_id => NodeId}};
                false -> {error, {entry, N, host_ipv6}}
            end;
        _ ->
            {error, {entry, N, host_ipv6}}
    end.

%% inet:parse_ipv6_address/1 returns {error, einval} or raises on a
%% malformed address, so the parse is guarded.
valid_ipv6(Host) ->
    try inet:parse_ipv6_address(binary_to_list(Host)) of
        {ok, _} -> true;
        {error, _} -> false
    catch
        _:_ -> false
    end.

host_and_port_finish(NodeId, Host, Port, N) ->
    case port_number(Port) of
        {ok, PortNumber} -> host_checked(NodeId, Host, PortNumber, N);
        error -> {error, {entry, N, port}}
    end.

%% No leading zeros, 1 to 65535, decimal digits only.
port_number(Port) when byte_size(Port) > 0 ->
    case (Port =:= <<"0">> orelse binary:first(Port) =/= $0) andalso digits(Port) of
        true ->
            case binary_to_integer(Port) of
                P when P >= 1, P =< 65535 -> {ok, P};
                _ -> error
            end;
        false ->
            error
    end;
port_number(_) ->
    error.

digits(<<>>) -> true;
digits(<<C, _/binary>>) when C < $0; C > $9 -> false;
digits(<<_, Rest/binary>>) -> digits(Rest).

%% An IPv4-shaped host (only digits and dots) is judged as an IPv4
%% address; anything else as a DNS name.
host_checked(NodeId, Host, Port, N) ->
    case ipv4_shaped(Host) of
        true -> ipv4_checked(NodeId, Host, Port, N);
        false -> dns_checked(NodeId, Host, Port, N)
    end.

ipv4_shaped(Host) ->
    Host =/= <<>> andalso lists:all(fun is_ipv4_byte/1, binary_to_list(Host)).

is_ipv4_byte(C) when C >= $0, C =< $9 -> true;
is_ipv4_byte($.) -> true;
is_ipv4_byte(_) -> false.

ipv4_checked(NodeId, Host, Port, N) ->
    case canonical_ipv4(Host) of
        true -> {ok, #{host => Host, port => Port, expected_node_id => NodeId}};
        false -> {error, {entry, N, host_ipv4}}
    end.

%% Four octets, each without leading zeros, 0 to 255.
canonical_ipv4(Host) ->
    Parts = binary:split(Host, <<".">>, [global]),
    length(Parts) =:= 4 andalso lists:all(fun octet/1, Parts).

octet(Part) when byte_size(Part) >= 1, byte_size(Part) =< 3 ->
    (Part =:= <<"0">> orelse binary:first(Part) =/= $0)
        andalso digits(Part) andalso binary_to_integer(Part) =< 255;
octet(_) ->
    false.

dns_checked(NodeId, Host, Port, N) ->
    case valid_dns(Host) of
        true -> {ok, #{host => Host, port => Port, expected_node_id => NodeId}};
        false -> {error, {entry, N, host_dns}}
    end.

%% Labels of letters, digits and hyphens, no leading or trailing
%% hyphen, 1 to 63 characters, at most 253 in total.
valid_dns(Host) ->
    byte_size(Host) =< 253 andalso
        lists:all(fun label/1, binary:split(Host, <<".">>, [global])).

label(Label) when byte_size(Label) >= 1, byte_size(Label) =< 63 ->
    binary:first(Label) =/= $- andalso binary:last(Label) =/= $-
        andalso lists:all(fun dns_byte/1, binary_to_list(Label));
label(_) ->
    false.

dns_byte(C) when C >= $a, C =< $z -> true;
dns_byte(C) when C >= $A, C =< $Z -> true;
dns_byte(C) when C >= $0, C =< $9 -> true;
dns_byte($-) -> true;
dns_byte(_) -> false.
