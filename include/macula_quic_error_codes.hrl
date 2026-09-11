%%%-------------------------------------------------------------------
%%% QUIC application error codes that macula sends when it resets or stops
%%% a stream. Every code macula uses is defined here, once, with its
%%% meaning, and code refers to them by these names, never by number. The
%%% QUIC NIF defines the same table in native/macula_quic/src/error_codes.rs;
%%% a new code goes into both.
%%%
%%%   0  QUIC_CODE_CANCELLED       the sender cancelled the stream: a content
%%%                                transfer cancel, or a stream open
%%%                                cancelled after the peer allowed it
%%%   1  QUIC_CODE_LINGER_EXPIRED  the stream closed, but its queued data
%%%                                could not be written within the linger
%%%                                bound
%%%-------------------------------------------------------------------
-ifndef(MACULA_QUIC_ERROR_CODES_HRL).
-define(MACULA_QUIC_ERROR_CODES_HRL, true).

%% The sender cancelled the stream.
-define(QUIC_CODE_CANCELLED, 0).

%% The stream closed, but its queued data could not be written within the
%% linger bound.
-define(QUIC_CODE_LINGER_EXPIRED, 1).

-endif.
