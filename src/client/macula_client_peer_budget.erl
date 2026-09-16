%% @private
%% @doc The new peers a pool may link to per window.
%%
%% Every distinct peer a pool links to holds a share of the provider's seen
%% requests (`macula_request_admission') while those entries live, so the
%% set's cap bounds anything only when the distinct peers one window can see
%% are bounded. This budget is that bound: at most `budget' new peers per
%% `window_ms'. A peer is counted once per window: a later sighting inside
%% the window spends nothing and does not extend it, and its count holds
%% through the window's last millisecond, so no closed window of
%% `window_ms' ever holds more than `budget' new peers.
%%
%% The pool keeps two budgets of this kind: dials, keyed by normalized seed
%% before a link is dialed, and peers, keyed by node_id when a handshake
%% completes. An `exempt' peer never spends the budget, which is how a
%% configured seed stays reachable when the budget is spent.
%%
%% Pure: the pool holds the value in its state and passes the clock in, in
%% milliseconds.
-module(macula_client_peer_budget).

-export([new/1, spend/3, counted/2]).

-export_type([t/0]).

-record(budget, {
    budget    :: pos_integer(),
    window_ms :: pos_integer(),
    exempt    :: sets:set(term()),
    %% Each counted peer and when its window started.
    counted = #{} :: #{term() => integer()}
}).

-opaque t() :: #budget{}.

-spec new(#{budget := pos_integer(), window_ms := pos_integer(), exempt => [term()]}) -> t().
new(#{budget := Budget, window_ms := WindowMs} = Opts)
  when is_integer(Budget), Budget > 0, is_integer(WindowMs), WindowMs > 0 ->
    #budget{budget = Budget, window_ms = WindowMs,
            exempt = sets:from_list(maps:get(exempt, Opts, []), [{version, 2}])}.

%% @doc Spend the budget on `Peer' at `NowMs': `ok' when the peer is exempt,
%% already counted in its window, or fits the budget; `spent' otherwise, and
%% then nothing is counted.
-spec spend(t(), term(), integer()) -> {ok | spent, t()}.
spend(#budget{exempt = Exempt} = Budget, Peer, NowMs) when is_integer(NowMs) ->
    Live = in_window(Budget, NowMs),
    judged(sets:is_element(Peer, Exempt), maps:is_key(Peer, Live#budget.counted), Live, Peer, NowMs).

%% @doc The peers counted in the window at `NowMs'.
-spec counted(t(), integer()) -> non_neg_integer().
counted(#budget{} = Budget, NowMs) when is_integer(NowMs) ->
    map_size((in_window(Budget, NowMs))#budget.counted).

judged(true, _Counted, Budget, _Peer, _Now) ->
    {ok, Budget};
judged(false, true, Budget, _Peer, _Now) ->
    {ok, Budget};
judged(false, false, #budget{budget = Max, counted = Counted} = Budget, Peer, Now)
  when map_size(Counted) < Max ->
    {ok, Budget#budget{counted = Counted#{Peer => Now}}};
judged(false, false, Budget, _Peer, _Now) ->
    {spent, Budget}.

in_window(#budget{window_ms = WindowMs, counted = Counted} = Budget, Now) ->
    Budget#budget{counted = maps:filter(fun(_Peer, Since) -> Now - Since =< WindowMs end, Counted)}.
