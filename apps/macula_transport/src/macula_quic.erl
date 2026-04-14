%% @doc NIF stub module for the Quinn QUIC NIF (`libmacula_quic.so').
%%
%% Each function below is a stub that raises `nif_not_loaded' until the
%% NIF is loaded at module-load time. Build is wired via
%% `priv/build-macula-quic.sh' (rebar pre_hook).
%%
%% This module is the raw NIF surface — application code should use
%% `macula_transport' instead.
-module(macula_quic).

-on_load(init/0).

-export([
    %% Listener
    nif_listen/9,
    nif_close_listener/1,
    nif_async_accept/1,

    %% Connection
    nif_connect/7,
    nif_open_stream/1,
    nif_close_connection/1,
    nif_async_accept_stream/1,
    nif_controlling_process_conn/2,
    nif_peername/1,

    %% Stream
    nif_send/2,
    nif_async_send/2,
    nif_close_stream/1,
    nif_setopt_active/2,
    nif_controlling_process/2
]).

%%------------------------------------------------------------------
%% NIF loader
%%------------------------------------------------------------------

init() ->
    PrivDir = priv_dir(),
    SoName  = filename:join(PrivDir, "libmacula_quic"),
    erlang:load_nif(SoName, 0).

priv_dir() ->
    case code:priv_dir(macula_transport) of
        {error, bad_name} ->
            %% Fallback for development invocation outside a release.
            EbinDir = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(EbinDir), "priv");
        Dir ->
            Dir
    end.

%%------------------------------------------------------------------
%% NIF stubs — replaced by the loaded NIF at module-load time.
%%------------------------------------------------------------------

nif_listen(_BindAddr, _Port, _CertFile, _KeyFile, _Alpn,
           _IdleMs, _KeepAliveMs, _BidiStreams, _UniStreams) ->
    erlang:nif_error(nif_not_loaded).

nif_close_listener(_Listener) ->
    erlang:nif_error(nif_not_loaded).

nif_async_accept(_Listener) ->
    erlang:nif_error(nif_not_loaded).

nif_connect(_Host, _Port, _Alpn, _Verify, _IdleMs, _KeepAliveMs, _TimeoutMs) ->
    erlang:nif_error(nif_not_loaded).

nif_open_stream(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_close_connection(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_async_accept_stream(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_controlling_process_conn(_Conn, _Pid) ->
    erlang:nif_error(nif_not_loaded).

nif_peername(_Conn) ->
    erlang:nif_error(nif_not_loaded).

nif_send(_Stream, _Data) ->
    erlang:nif_error(nif_not_loaded).

nif_async_send(_Stream, _Data) ->
    erlang:nif_error(nif_not_loaded).

nif_close_stream(_Stream) ->
    erlang:nif_error(nif_not_loaded).

nif_setopt_active(_Stream, _Active) ->
    erlang:nif_error(nif_not_loaded).

nif_controlling_process(_Stream, _Pid) ->
    erlang:nif_error(nif_not_loaded).
