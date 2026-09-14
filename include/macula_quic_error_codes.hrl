%%%-------------------------------------------------------------------
%%% QUIC application error codes that macula sends when it resets or stops
%%% a stream, or closes a connection. Every code macula uses is defined
%%% here, once, with its meaning, and code refers to them by these names,
%%% never by number. The QUIC NIF defines the same table in
%%% native/macula_quic/src/error_codes.rs; a new code goes into both.
%%%
%%%   0  QUIC_CODE_CANCELLED              the sender cancelled the stream: a
%%%                                       content transfer cancel, or a stream
%%%                                       open cancelled after the peer
%%%                                       allowed it
%%%   1  QUIC_CODE_LINGER_EXPIRED         the stream closed, but its queued
%%%                                       data could not be written within
%%%                                       the linger bound
%%%   2  QUIC_CODE_REFUSED                the stream was refused before it was
%%%                                       served: its first frame was not a
%%%                                       STREAM_OPEN signed by its caller
%%%   3  QUIC_CODE_STREAM_PROTOCOL_ERROR  an established stream was aborted:
%%%                                       a frame on it did not decode
%%%   4  QUIC_CODE_REFUSED_BUSY           the node had no room: a connection
%%%                                       closed because the station had no
%%%                                       handshake slot free, or a relayed
%%%                                       stream reset because its reader did
%%%                                       not take data in time; the peer may
%%%                                       try again later, or another station
%%%-------------------------------------------------------------------
-ifndef(MACULA_QUIC_ERROR_CODES_HRL).
-define(MACULA_QUIC_ERROR_CODES_HRL, true).

%% The sender cancelled the stream.
-define(QUIC_CODE_CANCELLED, 0).

%% The stream closed, but its queued data could not be written within the
%% linger bound.
-define(QUIC_CODE_LINGER_EXPIRED, 1).

%% The stream was refused before it was served.
-define(QUIC_CODE_REFUSED, 2).

%% An established stream was aborted because a frame on it did not decode.
-define(QUIC_CODE_STREAM_PROTOCOL_ERROR, 3).

%% The node had no room: a connection closed because the station had no
%% handshake slot free, or a relayed stream reset because its reader did not
%% take data in time.
-define(QUIC_CODE_REFUSED_BUSY, 4).

-endif.
