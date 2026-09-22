%% @doc The socket a distribution tunnel's TLS session runs on.
%%
%% A distribution tunnel's data travels inside a TLS 1.3 session run end to end between the two nodes (D29): the
%% carrier underneath forwards the bytes and is not trusted with them, whether that carrier is a relay or the
%% stations of the pool path. OTP's `ssl' runs over whatever transport a `cb_info' option names, so this module
%% makes a macula QUIC stream look like a socket to it.
%%
%% One process owns the stream and is the socket: it takes the stream's events, hands them to `ssl' in the tags
%% `ssl' expects, and answers `recv' when `ssl' is passive. Sends go through it as well, because a QUIC stream is
%% written by its owner and `ssl' writes from a process of its own.
%%
%% Use it as
%%
%%   {ok, Socket} = macula_dist_tunnel_socket:own(Stream),
%%   {ok, Session} = ssl:connect(Socket, [{cb_info, ?MACULA_TUNNEL_CB_INFO} | Opts], Timeout)
%%
%% and on the accepting side `ssl:handshake/3' with the same `cb_info'.
%%
%% ⚠ What is NOT here: nothing in this module decides who the peer is. It carries bytes. The connection handshake
%% that names the peer runs inside the session, against the leaf the session presented.
-module(macula_dist_tunnel_socket).

-behaviour(gen_server).

%% API
-export([own/1, cb_info/0]).

%% The transport callbacks ssl calls. They take the socket this module returns, not a QUIC stream.
-export([send/2, recv/3, controlling_process/2, close/1, shutdown/2,
         setopts/2, getopts/2, peername/1, sockname/1, port/1, getstat/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DATA_TAG, macula_tunnel).
-define(CLOSED_TAG, macula_tunnel_closed).
-define(ERROR_TAG, macula_tunnel_error).
-define(PASSIVE_TAG, macula_tunnel_passive).

-type socket() :: pid().
-export_type([socket/0]).

-record(state, {
    stream              :: reference(),
    owner               :: pid(),
    monitor             :: reference(),
    active = false      :: false | once | true | pos_integer(),
    buffer = <<>>       :: binary(),
    waiting = none      :: none | {gen_server:from(), non_neg_integer()},
    closed = open       :: open | closed
}).

%%------------------------------------------------------------------
%% API
%%------------------------------------------------------------------

%% @doc Take over Stream and return the socket to hand `ssl'. The caller is the socket's owner: it receives the
%% active messages until `controlling_process/2' names another, and the socket ends when it does.
-spec own(reference()) -> {ok, socket()} | {error, term()}.
own(Stream) when is_reference(Stream) ->
    gen_server:start(?MODULE, {Stream, self()}, []).

%% @doc The `cb_info' option naming this module and its tags. The passive tag is the fifth, and it is not optional
%% here: `ssl' reads with `{active, N}', and without a passive tag to end a run it cannot tell that its count is
%% spent.
-spec cb_info() -> {module(), atom(), atom(), atom(), atom()}.
cb_info() ->
    {?MODULE, ?DATA_TAG, ?CLOSED_TAG, ?ERROR_TAG, ?PASSIVE_TAG}.

%%------------------------------------------------------------------
%% Transport callbacks
%%------------------------------------------------------------------

send(Socket, Data) ->
    call(Socket, {send, iolist_to_binary(Data)}, infinity).

%% Length 0 is whatever has arrived, as it is for a gen_tcp socket in binary mode; any other length waits for
%% exactly that many bytes.
recv(Socket, Length, Timeout) ->
    call(Socket, {recv, Length}, Timeout).

controlling_process(Socket, Pid) when is_pid(Pid) ->
    call(Socket, {controlling_process, Pid}, infinity).

close(Socket) ->
    _ = call(Socket, close, infinity),
    ok.

%% A tunnel has one direction to shut: the stream it rides. Either half closes the whole thing.
shutdown(Socket, _How) ->
    close(Socket).

setopts(Socket, Opts) ->
    call(Socket, {setopts, Opts}, infinity).

getopts(Socket, Opts) ->
    call(Socket, {getopts, Opts}, infinity).

%% A tunnel has no address of its own: it is a stream inside a connection something else carries. The shape is what
%% `ssl' and `inet' expect from a socket, and the address says it is not one.
peername(_Socket) ->
    {ok, {{0, 0, 0, 0}, 0}}.

sockname(_Socket) ->
    {ok, {{0, 0, 0, 0}, 0}}.

port(_Socket) ->
    {ok, 0}.

getstat(_Socket, _Opts) ->
    {ok, []}.

%% A socket whose process has gone is a closed socket, which is what a caller of a closed gen_tcp socket sees.
call(Socket, Request, Timeout) ->
    try
        gen_server:call(Socket, Request, Timeout)
    catch
        exit:{noproc, _} -> {error, closed};
        exit:{normal, _} -> {error, closed};
        exit:{timeout, _} -> {error, timeout}
    end.

%%------------------------------------------------------------------
%% gen_server
%%------------------------------------------------------------------

init({Stream, Owner}) ->
    _ = macula_quic:controlling_process(Stream, self()),
    ok = macula_quic:setopt(Stream, active, true),
    {ok, #state{stream = Stream, owner = Owner, monitor = erlang:monitor(process, Owner)}}.

handle_call({send, _Data}, _From, #state{closed = closed} = S) ->
    {reply, {error, closed}, S};
handle_call({send, Data}, _From, #state{stream = Stream} = S) ->
    {reply, macula_quic:send(Stream, Data), S};

handle_call({recv, Length}, From, S) ->
    answered_now(read(S#state{waiting = {From, Length}}));

handle_call({controlling_process, Pid}, _From, #state{monitor = Mon} = S) ->
    true = erlang:demonitor(Mon, [flush]),
    {reply, ok, S#state{owner = Pid, monitor = erlang:monitor(process, Pid)}};

handle_call(close, _From, S) ->
    {stop, normal, ok, S};

handle_call({setopts, Opts}, _From, S) ->
    delivered(S#state{active = active_of(Opts, S#state.active)}, ok);

handle_call({getopts, Opts}, _From, #state{active = Active} = S) ->
    {reply, {ok, [option(Name, Active) || Name <- names(Opts)]}, S};

handle_call(_Request, _From, S) ->
    {reply, {error, einval}, S}.

handle_cast(_Cast, S) ->
    {noreply, S}.

%% The stream's bytes. Whoever is waiting for them gets them: a pending recv first, then an active owner.
handle_info({quic, Bin, Stream, _Flags}, #state{stream = Stream, buffer = Buffer} = S) when is_binary(Bin) ->
    delivered(S#state{buffer = <<Buffer/binary, Bin/binary>>});
handle_info({quic, Closed, Stream, _Detail}, #state{stream = Stream} = S)
  when Closed =:= stream_closed; Closed =:= peer_send_shutdown; Closed =:= closed ->
    delivered(S#state{closed = closed});
handle_info({quic, _Other, _Stream, _Detail}, S) ->
    {noreply, S};
%% The owner has gone, so the socket has no one to serve.
handle_info({'DOWN', Mon, process, _Pid, _Reason}, #state{monitor = Mon} = S) ->
    {stop, normal, S};
handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, #state{stream = Stream}) ->
    _ = macula_quic:close_stream(Stream),
    ok.

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

%% From a stream event: there is no call of its own to answer.
delivered(S) ->
    {noreply, served(S)}.

%% From setopts: the call that changed the mode is answered with Reply, after whatever that change released.
delivered(S, Reply) ->
    {reply, Reply, served(S)}.

%% A pending recv is served before the active owner, so bytes are never handed out twice or out of order.
served(#state{waiting = {_From, _Length}} = S) ->
    woken(read(S));
served(#state{active = false} = S) ->
    S;
served(#state{buffer = <<>>, closed = open} = S) ->
    S;
served(#state{buffer = <<>>, closed = closed, owner = Owner} = S) ->
    Owner ! {?CLOSED_TAG, self()},
    S#state{active = false};
served(#state{buffer = Buffer, owner = Owner, active = Active} = S) ->
    Owner ! {?DATA_TAG, self(), Buffer},
    spent(S#state{buffer = <<>>, active = still_active(Active)}).

%% A recv that was waiting is answered where it stands. ⚠ It cannot be answered by returning a reply from here:
%% this runs under handle_info as well as handle_call, and a `{reply, _, _}' from handle_info is a bad return that
%% takes the socket down mid-session.
woken({done, From, Reply, S}) ->
    gen_server:reply(From, Reply),
    S;
woken({waiting, S}) ->
    S.

answered_now({done, _From, Reply, S}) -> {reply, Reply, S};
answered_now({waiting, S}) -> {noreply, S}.

still_active(once) -> false;
still_active(true) -> true;
still_active(N) when is_integer(N) -> counted(N - 1).

counted(0) -> false;
counted(N) -> N.

%% A run of `{active, N}' that has just delivered its last message says so, or `ssl' waits for a message that will
%% never be sent and the session stalls.
spent(#state{active = false, owner = Owner} = S) ->
    Owner ! {?PASSIVE_TAG, self()},
    S;
spent(S) ->
    S.

%% A recv is answered when the buffer holds what it asked for, and refused when the stream has closed and never
%% will. Anything short of that waits.
read(#state{waiting = {From, Length}, buffer = Buffer} = S) when Length > 0, byte_size(Buffer) >= Length ->
    <<Taken:Length/binary, Rest/binary>> = Buffer,
    {done, From, {ok, Taken}, S#state{buffer = Rest, waiting = none}};
read(#state{waiting = {From, 0}, buffer = Buffer} = S) when Buffer =/= <<>> ->
    {done, From, {ok, Buffer}, S#state{buffer = <<>>, waiting = none}};
read(#state{waiting = {From, _Length}, closed = closed} = S) ->
    {done, From, {error, closed}, S#state{waiting = none}};
read(S) ->
    {waiting, S}.

%% `{active, N}' adds to a run already in progress, as an inet socket does, so a reader that tops up its count does
%% not lose what is left of the old one.
active_of(Opts, Current) ->
    counted_from(proplists:get_value(active, Opts, Current), Current).

counted_from(N, Current) when is_integer(N), is_integer(Current) -> N + Current;
counted_from(Active, _Current) -> Active.

names(Opts) ->
    [name_of(Opt) || Opt <- Opts].

name_of(Name) when is_atom(Name) -> Name;
name_of({Name, _Value}) -> Name.

%% What a socket of this kind answers for the options `ssl' reads. A tunnel is a stream: it delivers binaries, it
%% frames nothing itself, and it never blocks a send on a timeout of its own.
option(active, Active) -> {active, Active};
option(mode, _Active) -> {mode, binary};
option(packet, _Active) -> {packet, 0};
option(packet_size, _Active) -> {packet_size, 0};
option(header, _Active) -> {header, 0};
option(nodelay, _Active) -> {nodelay, true};
option(delay_send, _Active) -> {delay_send, false};
option(send_timeout, _Active) -> {send_timeout, infinity};
option(send_timeout_close, _Active) -> {send_timeout_close, false};
option(keepalive, _Active) -> {keepalive, false};
option(reuseaddr, _Active) -> {reuseaddr, true};
option(buffer, _Active) -> {buffer, 65536};
option(high_watermark, _Active) -> {high_watermark, 131072};
option(low_watermark, _Active) -> {low_watermark, 65536};
option(Name, _Active) -> {Name, undefined}.
