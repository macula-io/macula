%%%-------------------------------------------------------------------
%%% @doc Owns the inbound TUN-reader mailbox.
%%%
%%% The native TUN reader (see {@link macula_tun:start_reader/2}) sends
%%% `{macula_net_packet, Handle, Payload}' to a registered consumer
%%% process. This module IS that consumer — a long-lived gen_server
%%% that dispatches every received payload to the configured
%%% `dispatch_fn'. Production wires it to
%%% {@link macula_route_packet:dispatch/1}; tests pass a capture fun.
%%%
%%% Replaces an earlier hand-rolled `receive...loop' inside
%%% {@link macula_net} per the workspace rule "every receive loop is a
%%% gen_server candidate". OTP supervision, restart semantics, and
%%% sys/dbg tracing all come for free.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_tun_reader_proxy).

-behaviour(gen_server).

-export([
    start_link/1,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([config/0]).

-type dispatch_fn() :: fun((Payload :: binary()) -> any()).

-type config() :: #{
    dispatch_fn := dispatch_fn()
}.

-record(state, {
    dispatch_fn :: dispatch_fn()
}).

%% =============================================================================
%% Public API
%% =============================================================================

-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(#{dispatch_fn := Fun} = Config) when is_function(Fun, 1) ->
    gen_server:start_link(?MODULE, Config, []).

-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    gen_server:stop(Pid),
    ok.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(#{dispatch_fn := Fun}) ->
    process_flag(trap_exit, true),
    {ok, #state{dispatch_fn = Fun}}.

handle_call(_Other, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% TUN reader thread delivers packets here. Dispatch + ignore the
%% return value — the dispatcher is responsible for its own logging.
handle_info({macula_net_packet, _Handle, Payload}, State) ->
    safe_dispatch(State#state.dispatch_fn, Payload),
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internals
%% =============================================================================

safe_dispatch(Fun, Payload) ->
    %% Boundary: the dispatcher is a user-supplied callback. A crash
    %% here must not take down the proxy, otherwise a single malformed
    %% packet would stop ingest.
    try Fun(Payload)
    catch _:_ -> ok
    end.
