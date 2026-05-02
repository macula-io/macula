%%%-------------------------------------------------------------------
%%% @doc Minimal IPv6 header parser for macula-net routing.
%%%
%%% We only ever care about source + destination addresses (the routing
%%% layer is dst-based; src is preserved for ICMPv6 error replies).
%%% Per RFC 8200 the IPv6 fixed header is 40 bytes:
%%%
%%% ```
%%%   bytes 0..1   version (4) + traffic class (8) + flow label start
%%%   bytes 2..3   flow label rest
%%%   bytes 4..5   payload length
%%%   byte 6       next header
%%%   byte 7       hop limit
%%%   bytes 8..23  source address (128 bits)
%%%   bytes 24..39 destination address (128 bits)
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_route_packet_ipv6).

-export([
    parse_header/1,
    src/1,
    dst/1
]).

-export_type([
    header/0,
    address/0
]).

-type address() :: <<_:128>>.

-type header() :: #{
    version       := 6,
    traffic_class := non_neg_integer(),
    flow_label    := non_neg_integer(),
    payload_len   := non_neg_integer(),
    next_header   := non_neg_integer(),
    hop_limit     := non_neg_integer(),
    src           := address(),
    dst           := address()
}.

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Parse the 40-byte IPv6 fixed header. Returns the full header map.
%% Raises `badarg' if the input is shorter than 40 bytes or version != 6.
-spec parse_header(binary()) -> header().
parse_header(<<6:4, TC:8, FL:20,
               PLen:16, NH:8, HL:8,
               Src:128/bits, Dst:128/bits,
               _Rest/binary>>) ->
    #{
        version       => 6,
        traffic_class => TC,
        flow_label    => FL,
        payload_len   => PLen,
        next_header   => NH,
        hop_limit     => HL,
        src           => Src,
        dst           => Dst
    };
parse_header(_) ->
    erlang:error(badarg).

%% @doc Quick extract of source address (16 bytes), without building the
%% full header map. Useful on the hot path.
-spec src(binary()) -> address().
src(<<6:4, _:28, _:16, _:8, _:8, Src:128/bits, _Rest/binary>>) ->
    Src;
src(_) ->
    erlang:error(badarg).

%% @doc Quick extract of destination address (16 bytes).
-spec dst(binary()) -> address().
dst(<<6:4, _:28, _:16, _:8, _:8, _:128, Dst:128/bits, _Rest/binary>>) ->
    Dst;
dst(_) ->
    erlang:error(badarg).
