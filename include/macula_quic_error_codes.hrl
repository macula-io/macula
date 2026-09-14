%%%-------------------------------------------------------------------
%%% QUIC application error codes that macula sends when it resets or stops
%%% a stream. Every code macula uses is defined here, once, with its
%%% meaning, and code refers to them by these names, never by number. The
%%% QUIC NIF names the codes it sends itself, 0 and 1, in
%%% native/macula_quic/src/error_codes.rs.
%%%
%%%   0  QUIC_CODE_CANCELLED       the sender cancelled the stream: a content
%%%                                transfer cancel, or a stream open
%%%                                cancelled after the peer allowed it
%%%   1  QUIC_CODE_LINGER_EXPIRED  the stream closed, but its queued data
%%%                                could not be written within the linger
%%%                                bound
%%%   2  QUIC_CODE_SESSION_ENDED   a streaming session's process ended before
%%%                                it sent its last frame
%%%-------------------------------------------------------------------
-ifndef(MACULA_QUIC_ERROR_CODES_HRL).
-define(MACULA_QUIC_ERROR_CODES_HRL, true).

%% The sender cancelled the stream.
-define(QUIC_CODE_CANCELLED, 0).

%% The stream closed, but its queued data could not be written within the
%% linger bound.
-define(QUIC_CODE_LINGER_EXPIRED, 1).

%% A streaming session's process ended before it sent its last frame.
-define(QUIC_CODE_SESSION_ENDED, 2).

-endif.
