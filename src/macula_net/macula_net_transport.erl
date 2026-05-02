%%%-------------------------------------------------------------------
%%% @doc Transport plugin contract for macula-net.
%%%
%%% Per the macula-net spec (PLAN_MACULA_NET.md §7.1), the macula-net
%%% core operates on abstract <em>links</em> — bidirectional, reliable-
%%% delivery, framed channels with station identity. Each transport
%%% (QUIC, BATMAN-adv, LoRa, satellite, ...) implements this behaviour
%%% to be pluggable.
%%%
%%% == Lifecycle ==
%%%
%%% <ol>
%%%   <li>Caller starts the transport's listener (impl-specific args).</li>
%%%   <li>Caller registers an inbound handler via {@link set_handler/1};
%%%       the transport calls this for every CBOR envelope it receives.</li>
%%%   <li>Caller establishes outbound links via the impl's `connect`
%%%       function (impl-specific).</li>
%%%   <li>{@link send/2} delivers an encoded envelope to a known station.</li>
%%% </ol>
%%% @end
%%%-------------------------------------------------------------------
-module(macula_net_transport).

-export_type([
    station_id/0,
    handler/0,
    cbor_envelope/0
]).

-type station_id()    :: binary().
-type cbor_envelope() :: binary().
-type handler()       :: fun((cbor_envelope()) -> any()).

%% Send a CBOR-encoded macula-net envelope to a known station.
%% Returns ok on successful enqueue (NOT delivery — the transport may
%% queue, retry, or drop based on its own policy).
-callback send(station_id(), cbor_envelope()) -> ok | {error, term()}.

%% Register the handler that will be called for every inbound envelope.
%% The handler is called from the transport's own process; it should be
%% non-blocking (typically a cast to macula_deliver_packet:handle_envelope/1).
-callback set_handler(handler()) -> ok.
