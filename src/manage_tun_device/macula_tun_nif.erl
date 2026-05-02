%%%-------------------------------------------------------------------
%%% @doc Native function stubs for the TUN device NIF.
%%%
%%% Loads `priv/macula_tun_nif.so' (built from `native/macula_tun_nif/'
%%% by `priv/build-nifs.sh' during compile).
%%%
%%% This module is the canonical NIF surface; higher-level callers should
%%% prefer {@link macula_tun} which adds the public API + types.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_tun_nif).

-export([
    nif_open/2,
    nif_name/1,
    nif_close/1,
    nif_write/2,
    nif_start_reader/2
]).

-on_load(init/0).

%% =============================================================================
%% NIF stubs
%% =============================================================================

nif_open(_IfName, _Mtu)         -> erlang:nif_error(nif_not_loaded).
nif_name(_Handle)               -> erlang:nif_error(nif_not_loaded).
nif_close(_Handle)              -> erlang:nif_error(nif_not_loaded).
nif_write(_Handle, _Packet)     -> erlang:nif_error(nif_not_loaded).
nif_start_reader(_Handle, _Pid) -> erlang:nif_error(nif_not_loaded).

%% =============================================================================
%% NIF loading (matches macula_crypto_nif convention)
%% =============================================================================

init() ->
    PrivDir = case code:priv_dir(macula) of
        {error, _} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    filename:join(filename:dirname(filename:dirname(Filename)),
                                  "priv");
                _ ->
                    "priv"
            end;
        Dir ->
            Dir
    end,
    Path = filename:join(PrivDir, "macula_tun_nif"),
    erlang:load_nif(Path, 0).
