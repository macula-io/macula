%%%-------------------------------------------------------------------
%%% @doc Macula Console Formatter - Beautiful colored terminal output.
%%%
%%% Provides ANSI-colored output for ping-pong demo and general logging.
%%% Makes demo output visually appealing and easy to read.
%%%
%%% Example output:
%%% <pre>
%%% --&gt; fc01 -&gt; rc05 [full_cone -&gt; restricted]
%%% &lt;-- fc01 &lt;- rc05 42ms [restricted]
%%% [!!] fc01 -&gt; sy07 TIMEOUT [symmetric]
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_console).

-export([
    %% Ping-pong demo output
    ping/4,
    pong/4,
    timeout/3,
    %% PubSub demo output
    pubsub_send/3,
    pubsub_recv/5,
    %% General output
    error/2,
    info/2,
    success/2,
    warning/2,
    %% Stats display
    stats/2,
    %% Mesh events
    node_connected/2,
    node_disconnected/2,
    %% Banner
    banner/1
]).

%%%===================================================================
%%% ANSI Color Codes
%%%===================================================================

-define(RESET,   "\e[0m").
-define(BOLD,    "\e[1m").
-define(DIM,     "\e[2m").
-define(ITALIC,  "\e[3m").
-define(UNDERLINE, "\e[4m").

%% Colors
-define(BLACK,   "\e[30m").
-define(RED,     "\e[31m").
-define(GREEN,   "\e[32m").
-define(YELLOW,  "\e[33m").
-define(BLUE,    "\e[34m").
-define(MAGENTA, "\e[35m").
-define(CYAN,    "\e[36m").
-define(WHITE,   "\e[37m").

%% Bright colors
-define(BRIGHT_RED,     "\e[91m").
-define(BRIGHT_GREEN,   "\e[92m").
-define(BRIGHT_YELLOW,  "\e[93m").
-define(BRIGHT_BLUE,    "\e[94m").
-define(BRIGHT_MAGENTA, "\e[95m").
-define(BRIGHT_CYAN,    "\e[96m").

%% Background colors
-define(BG_RED,    "\e[41m").
-define(BG_GREEN,  "\e[42m").
-define(BG_YELLOW, "\e[43m").

%%%===================================================================
%%% Icons
%%%===================================================================

-define(ARROW_RIGHT, "-->").
-define(ARROW_LEFT,  "<--").
-define(CHECK,       "[ok]").
-define(CROSS,       "[!!]").
-define(INFO,        "[i]").
-define(WARN,        "[!]").
-define(STAR,        "[*]").

%%%===================================================================
%%% Ping-Pong Demo Output
%%%===================================================================

%% @doc Output a PING message showing source to target with NAT types
-spec ping(binary() | string(), binary() | string(),
           binary() | string() | atom(), binary() | string() | atom()) -> ok.
ping(FromNode, ToNode, FromNat, ToNat) ->
    io:format("~s~s~s ~s~s~s -> ~s~s~s ~s[~s -> ~s]~s~n",
        [?CYAN, ?ARROW_RIGHT, ?RESET,
         ?BOLD, format_node(FromNode), ?RESET,
         ?BOLD, format_node(ToNode), ?RESET,
         ?DIM, format_nat(FromNat), format_nat(ToNat), ?RESET]).

%% @doc Output a PONG message with RTT
-spec pong(binary() | string(), binary() | string(),
           integer(), binary() | string() | atom()) -> ok.
pong(FromNode, ToNode, RTT, NatType) ->
    RttColor = rtt_color(RTT),
    io:format("~s~s~s ~s~s~s <- ~s~s~s ~s~pms~s ~s[~s]~s~n",
        [?GREEN, ?ARROW_LEFT, ?RESET,
         ?BOLD, format_node(FromNode), ?RESET,
         ?BOLD, format_node(ToNode), ?RESET,
         RttColor, RTT, ?RESET,
         ?DIM, format_nat(NatType), ?RESET]).

%% @doc Output a TIMEOUT message for a failed ping
-spec timeout(binary() | string(), binary() | string(),
              binary() | string() | atom()) -> ok.
timeout(FromNode, ToNode, NatInfo) ->
    io:format("~s~s~s ~s~s~s -> ~s~s~s ~sTIMEOUT~s ~s[~s]~s~n",
        [?RED, ?CROSS, ?RESET,
         ?BOLD, format_node(FromNode), ?RESET,
         ?BOLD, format_node(ToNode), ?RESET,
         ?RED, ?RESET,
         ?DIM, format_nat(NatInfo), ?RESET]).

%%%===================================================================
%%% PubSub Demo Output
%%%===================================================================

%% @doc Output a PubSub broadcast message with sequence number
-spec pubsub_send(binary() | string(), integer(), binary() | string() | atom()) -> ok.
pubsub_send(FromNode, SeqNum, NatType) ->
    io:format("~s[>>]~s ~s~s~s ~s#~p~s ~s[~s]~s~n",
        [?MAGENTA, ?RESET,
         ?BOLD, format_node(FromNode), ?RESET,
         ?MAGENTA, SeqNum, ?RESET,
         ?DIM, format_nat(NatType), ?RESET]).

%% @doc Output a PubSub receive message with delivery rate
-spec pubsub_recv(binary() | string(), binary() | string(), integer(),
                  binary() | string() | atom(), float()) -> ok.
pubsub_recv(ToNode, FromNode, SeqNum, SenderNat, DeliveryRate) ->
    RateColor = delivery_color(DeliveryRate),
    io:format("~s[<<]~s ~s~s~s <- ~s~s~s ~s#~p~s ~s[~s]~s ~s~.1f%%~s~n",
        [?BRIGHT_BLUE, ?RESET,
         ?BOLD, format_node(ToNode), ?RESET,
         ?BOLD, format_node(FromNode), ?RESET,
         ?BRIGHT_BLUE, SeqNum, ?RESET,
         ?DIM, format_nat(SenderNat), ?RESET,
         RateColor, DeliveryRate, ?RESET]).

%%%===================================================================
%%% General Output
%%%===================================================================

%% @doc Output an error message: [!!] Error message
-spec error(binary() | string(), binary() | string()) -> ok.
error(_Prefix, Msg) ->
    io:format("~s~s~s ~s~s~s~n",
        [?RED, ?CROSS, ?RESET, ?RED, format_msg(Msg), ?RESET]).

%% @doc Output an info message: [i] Info message
-spec info(binary() | string(), binary() | string()) -> ok.
info(_Prefix, Msg) ->
    io:format("~s~s~s ~s~n",
        [?CYAN, ?INFO, ?RESET, format_msg(Msg)]).

%% @doc Output a success message: [ok] Success message
-spec success(binary() | string(), binary() | string()) -> ok.
success(_Prefix, Msg) ->
    io:format("~s~s~s ~s~s~s~n",
        [?GREEN, ?CHECK, ?RESET, ?GREEN, format_msg(Msg), ?RESET]).

%% @doc Output a warning message: [!] Warning message
-spec warning(binary() | string(), binary() | string()) -> ok.
warning(_Prefix, Msg) ->
    io:format("~s~s~s ~s~s~s~n",
        [?YELLOW, ?WARN, ?RESET, ?YELLOW, format_msg(Msg), ?RESET]).

%%%===================================================================
%%% Stats Display
%%%===================================================================

%% @doc Output statistics for a node
%% StatsMap should contain: pings_sent, pongs_received, timeouts, avg_rtt_ms
-spec stats(binary() | string(), map()) -> ok.
stats(NodeId, StatsMap) ->
    PingsSent = maps:get(pings_sent, StatsMap, 0),
    PongsReceived = maps:get(pongs_received, StatsMap, 0),
    Timeouts = maps:get(timeouts, StatsMap, 0),
    AvgRtt = maps:get(avg_rtt_ms, StatsMap, 0),
    SuccessRate = calc_success_rate(PingsSent, PongsReceived),

    io:format("~n~s=== ~s Statistics ===~s~n", [?BOLD, format_node(NodeId), ?RESET]),
    io:format("  ~sSent:~s     ~p~n", [?DIM, ?RESET, PingsSent]),
    io:format("  ~sReceived:~s ~s~p~s~n", [?DIM, ?RESET, ?GREEN, PongsReceived, ?RESET]),
    io:format("  ~sTimeouts:~s ~s~p~s~n", [?DIM, ?RESET, timeout_color(Timeouts), Timeouts, ?RESET]),
    io:format("  ~sAvg RTT:~s  ~s~pms~s~n", [?DIM, ?RESET, rtt_color(AvgRtt), AvgRtt, ?RESET]),
    io:format("  ~sSuccess:~s ~s~.1f%~s~n", [?DIM, ?RESET, success_color(SuccessRate), SuccessRate, ?RESET]).

%%%===================================================================
%%% Mesh Events
%%%===================================================================

%% @doc Output a node connected event: [*] Node fc01 connected (full_cone)
-spec node_connected(binary() | string(), binary() | string() | atom()) -> ok.
node_connected(NodeId, NatType) ->
    io:format("~s~s~s ~sNode ~s~s~s connected~s ~s(~s)~s~n",
        [?GREEN, ?STAR, ?RESET,
         ?GREEN, ?BOLD, format_node(NodeId), ?RESET,
         ?RESET, ?DIM, format_nat(NatType), ?RESET]).

%% @doc Output a node disconnected event: [*] Node fc01 disconnected
-spec node_disconnected(binary() | string(), binary() | string()) -> ok.
node_disconnected(NodeId, Reason) ->
    io:format("~s~s~s ~sNode ~s~s~s disconnected~s ~s(~s)~s~n",
        [?YELLOW, ?STAR, ?RESET,
         ?YELLOW, ?BOLD, format_node(NodeId), ?RESET,
         ?RESET, ?DIM, format_msg(Reason), ?RESET]).

%%%===================================================================
%%% Banner
%%%===================================================================

%% @doc Display a startup banner for the node
-spec banner(map()) -> ok.
banner(Config) ->
    NodeId = maps:get(node_id, Config, <<"unknown">>),
    Realm = maps:get(realm, Config, <<"default">>),
    Mode = maps:get(mode, Config, <<"peer">>),

    io:format("~n"),
    io:format("~s╔══════════════════════════════════════════════╗~s~n", [?CYAN, ?RESET]),
    io:format("~s║~s  ~s~sMAC~s~sU~s~sLA~s  ~sMesh Network~s                       ~s║~s~n",
        [?CYAN, ?RESET, ?BOLD, ?RED, ?RESET, ?BOLD, ?RESET, ?BOLD, ?RED, ?RESET, ?DIM, ?CYAN, ?RESET]),
    io:format("~s╠══════════════════════════════════════════════╣~s~n", [?CYAN, ?RESET]),
    io:format("~s║~s  Node:  ~s~s~s                                   ~s║~s~n",
        [?CYAN, ?RESET, ?BOLD, format_padded(NodeId, 20), ?RESET, ?CYAN, ?RESET]),
    io:format("~s║~s  Realm: ~s~s                                   ~s║~s~n",
        [?CYAN, ?RESET, format_padded(Realm, 20), ?RESET, ?CYAN, ?RESET]),
    io:format("~s║~s  Mode:  ~s~s                                   ~s║~s~n",
        [?CYAN, ?RESET, format_padded(Mode, 20), ?RESET, ?CYAN, ?RESET]),
    io:format("~s╚══════════════════════════════════════════════╝~s~n", [?CYAN, ?RESET]),
    io:format("~n").

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Format a node ID for display
format_node(Node) when is_binary(Node) -> binary_to_list(Node);
format_node(Node) when is_atom(Node) -> atom_to_list(Node);
format_node(Node) when is_list(Node) -> Node.

%% @private Format a NAT type for display
format_nat(Nat) when is_atom(Nat) -> atom_to_list(Nat);
format_nat(Nat) when is_binary(Nat) -> binary_to_list(Nat);
format_nat(Nat) when is_list(Nat) -> Nat.

%% @private Format a message for display
format_msg(Msg) when is_binary(Msg) -> binary_to_list(Msg);
format_msg(Msg) when is_atom(Msg) -> atom_to_list(Msg);
format_msg(Msg) when is_list(Msg) -> Msg.

%% @private Format with padding
format_padded(Value, Width) ->
    Str = format_msg(Value),
    Len = length(Str),
    case Len >= Width of
        true -> string:slice(Str, 0, Width);
        false -> Str ++ lists:duplicate(Width - Len, $ )
    end.

%% @private Get color based on RTT
rtt_color(RTT) when RTT < 50 -> ?BRIGHT_GREEN;
rtt_color(RTT) when RTT < 100 -> ?GREEN;
rtt_color(RTT) when RTT < 200 -> ?YELLOW;
rtt_color(RTT) when RTT < 500 -> ?BRIGHT_YELLOW;
rtt_color(_) -> ?RED.

%% @private Get color based on timeout count
timeout_color(0) -> ?GREEN;
timeout_color(T) when T < 5 -> ?YELLOW;
timeout_color(_) -> ?RED.

%% @private Get color based on success rate
success_color(Rate) when Rate >= 90.0 -> ?BRIGHT_GREEN;
success_color(Rate) when Rate >= 70.0 -> ?GREEN;
success_color(Rate) when Rate >= 50.0 -> ?YELLOW;
success_color(_) -> ?RED.

%% @private Get color based on delivery rate (for pubsub)
delivery_color(Rate) when Rate >= 95.0 -> ?BRIGHT_GREEN;
delivery_color(Rate) when Rate >= 80.0 -> ?GREEN;
delivery_color(Rate) when Rate >= 60.0 -> ?YELLOW;
delivery_color(_) -> ?RED.

%% @private Calculate success rate
calc_success_rate(0, _) -> 0.0;
calc_success_rate(Sent, Received) ->
    (Received / Sent) * 100.0.
