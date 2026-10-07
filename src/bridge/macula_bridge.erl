%%%-------------------------------------------------------------------
%%% @doc The legacy-application bridge: an unmodified TCP application
%%% (a database client, ssh, a web dashboard) reaches an unmodified TCP
%%% service across the mesh, one bidi stream per TCP connection.
%%%
%%% The serving end (`serve/5') advertises a bidi stream procedure and, for
%%% each stream a caller opens, connects to the real service and pumps bytes
%%% both ways. Who may connect is the procedure's auth policy, checked at
%%% STREAM_OPEN against the caller's verified identity
%%% (`{realm_member_required, _, _}' or `{ucan_required, _}'); a caller the
%%% policy refuses gets the stream refused, and its local connection is
%%% closed at once and logged by the refusal's name. `serve/5' refuses to run
%%% without an explicit `auth': `open' has to be asked for.
%%%
%%% The listening end (`listen/4') accepts TCP connections on a local port
%%% (127.0.0.1 by default) and opens one stream per connection.
%%%
%%% Each connection is one `macula_bridge_pump' process: TCP reads go out as
%%% stream chunks under a credit window, so a slow reader holds the writer
%%% back; the stream is read one chunk at a time, so a peer that ignores the
%%% window trips the stream's own inbox bound; a TCP FIN travels in band and
%%% becomes a TCP write shutdown on the other side, which keeps crediting the
%%% answer it reads.
%%%
%%% `serve_with/3' and `listen_with/2' are the two ends over any advertiser
%%% and opener (`serve/5' and `listen/4' pass the pool's).
%%%
%%% Options (`serve/5', `serve_with/3', `listen/4', `listen_with/2'):
%%% <ul>
%%%   <li>`auth' (serve, required): the procedure's auth policy, as
%%%       `macula:advertise_stream/6' takes it.</li>
%%%   <li>`stations' (serve): the stations to advertise on.</li>
%%%   <li>`ucan_token' and `dial_timeout_ms' (listen): what each stream is
%%%       opened with, as `macula:call_stream/5' takes them.</li>
%%%   <li>`port' (listen, default 0: any free port) and `ip' (listen,
%%%       default `{127,0,0,1}').</li>
%%%   <li>`window_bytes' (default 1 MiB, 64 KiB to 8 MiB): the bytes a side
%%%       lets the other have in flight towards it (credit is the receiver's
%%%       to grant, so the two ends may differ). The serving end grants no
%%%       more than one served session's share of its caller's budget for
%%%       unread bytes (`macula_stream_sessions:session_share/0', 1 MiB by
%%%       default), and refuses to serve when that is under 64 KiB.</li>
%%%   <li>`chunk_bytes' (1 KiB to 1 MiB and at most half the window; by
%%%       default 64 KiB or half the window, whichever is less): the most
%%%       one socket read, and so one stream chunk, carries.</li>
%%%   <li>`idle_ms' (default infinity): no traffic either way for this long
%%%       closes the connection.</li>
%%%   <li>`write_timeout_ms' (default infinity): a socket that takes no write
%%%       for this long closes the connection.</li>
%%%   <li>`connect_timeout_ms' (serve, default 5 s): connecting to the
%%%       service.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge).

-behaviour(gen_server).

-export([serve/5, serve_with/3, listen/4, handler/2, listen_with/2, local_port/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type target() :: {inet:hostname() | inet:ip_address(), inet:port_number()}.
-type opener() :: fun((map()) -> {ok, pid()} | {error, term()}).
-export_type([target/0, opener/0]).

-define(DEFAULT_IP, {127, 0, 0, 1}).
%% At least the credit a sender starts with; at most half the stream's
%% 16 MiB inbox bound, so a sender within its credit never trips it.
-define(MIN_WINDOW_BYTES, 64 * 1024).
-define(MAX_WINDOW_BYTES, 8 * 1024 * 1024).
-define(DEFAULT_WINDOW_BYTES, 1024 * 1024).
-define(DEFAULT_CHUNK_BYTES, 64 * 1024).
%% At least big enough to be worth a signed frame, at most one that a
%% signature and a copy are still cheap for.
-define(MIN_CHUNK_BYTES, 1024).
-define(MAX_CHUNK_BYTES, 1024 * 1024).
-define(ACCEPT_RETRY_MS, 100).

%%--------------------------------------------------------------------
%% Serving end
%%--------------------------------------------------------------------

%% @doc Serve the TCP service at `Target' as the bidi stream procedure
%% `Procedure' in `Realm', through `Pool'. `Opts' must carry `auth'.
-spec serve(macula:pool(), macula:realm(), macula:procedure(), target(), map()) -> ok | {error, term()}.
serve(Pool, Realm, Procedure, Target, Opts) ->
    serve_with(fun(Handler, AdOpts) -> macula:advertise_stream(Pool, Realm, Procedure, bidi, Handler, AdOpts) end,
               Target, Opts).

%% @doc As `serve/5', advertising with `Advertise', which is given the
%% handler and exactly the advertisement's options (`auth', `stations').
-spec serve_with(fun((fun((pid(), term()) -> ok), map()) -> ok | {error, term()}), target(), map()) ->
    ok | {error, term()}.
serve_with(_Advertise, _Target, Opts) when not is_map_key(auth, Opts) ->
    {error, {auth, required}};
serve_with(Advertise, Target, Opts) ->
    served(share_checked(checked(Opts), Opts), Advertise, Target, Opts).

%% The serving end's receive window, held to one session's share of its
%% caller's budget, has to be at least the smallest window.
share_checked(ok, Opts) ->
    shared(maps:get(receive_window, resolved(serve, Opts)) >= ?MIN_WINDOW_BYTES);
share_checked(Refused, _Opts) ->
    Refused.

shared(true) -> ok;
shared(false) -> {error, {session_share, macula_stream_sessions:session_share()}}.

served(ok, Advertise, Target, Opts) ->
    Advertise(handler(Target, Opts), maps:with([auth, stations], Opts));
served(Refused, _Advertise, _Target, _Opts) ->
    Refused.

%% @doc The stream handler that bridges each stream to `Target': what
%% `serve/5' advertises, and what a test or an in-process advertisement
%% (`macula_stream_local') runs as it is. Its options are resolved here,
%% once: the share of the caller budget it grants under is the one this
%% node has now, whatever the budget becomes while it serves.
-spec handler(target(), map()) -> fun((pid(), term()) -> ok).
handler(Target, Opts) ->
    Resolved = resolved(serve, Opts),
    fun(Stream, Args) -> macula_bridge_pump:serve(Stream, Target, caller_of(Args), Resolved) end.

%% The options a pump runs with: the window, a chunk size (by default 64 KiB
%% and at most half the window) and the receive window it grants under,
%% which on the serving end is held to one served session's share of its
%% caller's budget.
resolved(Side, Opts) ->
    Window = maps:get(window_bytes, Opts, ?DEFAULT_WINDOW_BYTES),
    Opts#{window_bytes => Window,
          chunk_bytes => maps:get(chunk_bytes, Opts, min(?DEFAULT_CHUNK_BYTES, Window div 2)),
          receive_window => receive_window(Side, Window)}.

receive_window(listen, Window) -> Window;
receive_window(serve, Window) -> min(Window, macula_stream_sessions:session_share()).

caller_of(#{caller := Caller}) -> Caller;
caller_of(_Args) -> undefined.

%%--------------------------------------------------------------------
%% Listening end
%%--------------------------------------------------------------------

%% @doc Listen on a local port and bridge each accepted connection to the
%% bidi stream procedure `Procedure' in `Realm', through `Pool'. The
%% listener is linked to the caller.
-spec listen(macula:pool(), macula:realm(), macula:procedure(), map()) -> {ok, pid()} | {error, term()}.
listen(Pool, Realm, Procedure, Opts) ->
    listen_with(fun(CallOpts) -> macula:call_stream(Pool, Realm, Procedure, #{}, CallOpts) end, Opts).

%% @doc As `listen/4', opening each connection's stream with `Open', run in
%% that connection's own process (so the stream is owned by it) and given
%% the call options: `mode => bidi', `owner', and `ucan_token' and
%% `dial_timeout_ms' when `Opts' has them.
-spec listen_with(opener(), map()) -> {ok, pid()} | {error, term()}.
listen_with(Open, Opts) when is_function(Open, 1), is_map(Opts) ->
    listening_with(checked(Opts), Open, Opts).

listening_with(ok, Open, Opts) -> gen_server:start_link(?MODULE, {Open, resolved(listen, Opts)}, []);
listening_with(Refused, _Open, _Opts) -> Refused.

checked(Opts) ->
    chunk_checked(window_checked(maps:get(window_bytes, Opts, ?DEFAULT_WINDOW_BYTES)), Opts).

window_checked(W) when is_integer(W), W >= ?MIN_WINDOW_BYTES, W =< ?MAX_WINDOW_BYTES -> {ok, W};
window_checked(W) -> {error, {window_bytes, W}}.

chunk_checked({ok, Window}, #{chunk_bytes := C})
  when is_integer(C), C >= ?MIN_CHUNK_BYTES, C =< ?MAX_CHUNK_BYTES, C =< Window div 2 -> ok;
chunk_checked({ok, _Window}, #{chunk_bytes := C}) -> {error, {chunk_bytes, C}};
chunk_checked({ok, _Window}, _Opts) -> ok;
chunk_checked(Refused, _Opts) -> Refused.

%% @doc The port a listener accepts on (useful with `port => 0').
-spec local_port(pid()) -> {ok, inet:port_number()}.
local_port(Listener) ->
    gen_server:call(Listener, local_port).

%% @doc Stop a listener. Connections already bridged run on.
-spec stop(pid()) -> ok.
stop(Listener) ->
    gen_server:stop(Listener).

%%--------------------------------------------------------------------
%% The listener: one acceptor process, one pump per connection
%%--------------------------------------------------------------------

init({Open, Opts}) ->
    process_flag(trap_exit, true),
    Ip = maps:get(ip, Opts, ?DEFAULT_IP),
    listening(gen_tcp:listen(maps:get(port, Opts, 0),
                             [{ip, Ip}, {active, false}, {reuseaddr, true} | macula_bridge_pump:socket_opts(Opts)]),
              Open, Opts).

listening({ok, LSock}, Open, Opts) ->
    {ok, Port} = inet:port(LSock),
    Self = self(),
    Acceptor = spawn_link(fun() -> accept_loop(LSock, Open, Opts, Self) end),
    {ok, #{lsock => LSock, port => Port, acceptor => Acceptor}};
listening({error, Reason}, _Open, _Opts) ->
    {stop, {listen, Reason}}.

handle_call(local_port, _From, #{port := Port} = S) ->
    {reply, {ok, Port}, S};
handle_call(_Request, _From, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

%% The acceptor ending means the listening socket is gone: so is the listener.
handle_info({'EXIT', Acceptor, Reason}, #{acceptor := Acceptor} = S) ->
    {stop, {acceptor, Reason}, S};
handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, #{lsock := LSock}) ->
    _ = gen_tcp:close(LSock),
    ok.

accept_loop(LSock, Open, Opts, Listener) ->
    accepted(gen_tcp:accept(LSock), LSock, Open, Opts, Listener).

accepted({ok, Sock}, LSock, Open, Opts, Listener) ->
    Pump = spawn(fun() -> pump(Open, Opts) end),
    ok = gen_tcp:controlling_process(Sock, Pump),
    Pump ! {socket, Sock},
    accept_loop(LSock, Open, Opts, Listener);
%% Out of file descriptors is a moment, not the end of the listener.
accepted({error, Reason}, LSock, Open, Opts, Listener) when Reason =:= emfile; Reason =:= enfile ->
    logger:warning("[macula_bridge] cannot accept a connection now: ~0p", [Reason]),
    timer:sleep(?ACCEPT_RETRY_MS),
    accept_loop(LSock, Open, Opts, Listener);
accepted({error, Reason}, _LSock, _Open, _Opts, _Listener) ->
    exit({accept, Reason}).

%% The pump waits for its socket, handed over once it controls it.
pump(Open, Opts) ->
    receive {socket, S} -> macula_bridge_pump:connect(S, Open, Opts) end.
