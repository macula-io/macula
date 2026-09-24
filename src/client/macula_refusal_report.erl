%% @private
%% @doc Refusals counted by kind and reported at most once per window.
%%
%% A refusal a peer can provoke is counted every time, and each kind is
%% reported at most once per window with the count since its last report,
%% so a flood of refusals costs one log line per window instead of one per
%% refusal. The first refusal of a kind is reported at once. Refusals after a
%% report stay quiet until one arrives after the window, which then reports
%% them all; `counts/1' always has the totals, for status and health.
%%
%% A refusal may name its source (`refused/4'): who was refused, as the owner
%% chooses to say it. A report then lists the sources since the last report,
%% most refused first, at most five; a window keeps at most 64 distinct
%% sources and counts the rest together under `other', so a caller minting
%% identities cannot grow it. `last_sources/1' has each kind's latest list.
%%
%% Pure: the owner keeps the value and passes the clock in, in milliseconds.
-module(macula_refusal_report).

-export([new/1, refused/3, refused/4, counts/1, last_sources/1]).

-define(MAX_SOURCES, 64).
-define(REPORTED_SOURCES, 5).

-export_type([t/0]).

-record(kind, {
    total = 0          :: non_neg_integer(),
    since_report = 0   :: non_neg_integer(),
    reported_at        :: integer() | never,
    sources = #{}      :: #{term() => pos_integer()},
    last_sources = []  :: [{term(), pos_integer()}]
}).

-record(report, {
    window_ms   :: pos_integer(),
    kinds = #{} :: #{term() => #kind{}}
}).

-opaque t() :: #report{}.

-spec new(pos_integer()) -> t().
new(WindowMs) when is_integer(WindowMs), WindowMs > 0 ->
    #report{window_ms = WindowMs}.

%% @doc Count a refusal of `Kind' at `NowMs'. `report' with the count since
%% the kind's last report when one is due, `quiet' otherwise.
-spec refused(t(), term(), integer()) -> {report, pos_integer(), t()} | {quiet, t()}.
refused(#report{window_ms = WindowMs, kinds = Kinds} = Report, Kind, NowMs) when is_integer(NowMs) ->
    #kind{total = Total, since_report = Since, reported_at = At} = K =
        maps:get(Kind, Kinds, #kind{reported_at = never}),
    Counted = K#kind{total = Total + 1, since_report = Since + 1},
    due(report_due(At, NowMs, WindowMs), Kind, Counted, NowMs, Report).

%% @doc As `refused/3', naming the refusal's source. A report carries the
%% sources since the last report, most refused first.
-spec refused(t(), term(), term(), integer()) ->
    {report, pos_integer(), [{term(), pos_integer()}], t()} | {quiet, t()}.
refused(#report{kinds = Kinds} = Report, Kind, Source, NowMs) when is_integer(NowMs) ->
    #kind{sources = Sources} = K = maps:get(Kind, Kinds, #kind{reported_at = never}),
    sourced(refused(Report#report{kinds = Kinds#{Kind => K#kind{sources = counted(Source, Sources)}}},
                    Kind, NowMs), Kind).

%% @doc Each kind's sources as of its latest report.
-spec last_sources(t()) -> #{term() => [{term(), pos_integer()}]}.
last_sources(#report{kinds = Kinds}) ->
    #{Kind => Last || Kind := #kind{last_sources = Last} <- Kinds, Last =/= []}.

%% A known source, or a new one while there is room, is counted under its
%% own name; past the bound, under `other'.
counted(Source, Sources) when is_map_key(Source, Sources); map_size(Sources) < ?MAX_SOURCES ->
    maps:update_with(Source, fun(N) -> N + 1 end, 1, Sources);
counted(_Source, Sources) ->
    maps:update_with(other, fun(N) -> N + 1 end, 1, Sources).

%% On a report, the window's sources become the reported list and the
%% window starts empty; otherwise they keep counting.
sourced({report, Count, #report{kinds = Kinds} = Report}, Kind) ->
    #kind{sources = Sources} = K = maps:get(Kind, Kinds),
    Top = top(Sources),
    {report, Count, Top, Report#report{kinds = Kinds#{Kind => K#kind{sources = #{}, last_sources = Top}}}};
sourced({quiet, _} = Quiet, _Kind) ->
    Quiet.

top(Sources) ->
    lists:sublist(lists:sort(fun({A, NA}, {B, NB}) -> {NA, B} >= {NB, A} end, maps:to_list(Sources)),
                  ?REPORTED_SOURCES).

%% @doc Every refusal counted, by kind.
-spec counts(t()) -> #{term() => pos_integer()}.
counts(#report{kinds = Kinds}) ->
    #{Kind => Total || Kind := #kind{total = Total} <- Kinds}.

report_due(never, _Now, _WindowMs)             -> true;
report_due(At, Now, WindowMs)                  -> Now - At >= WindowMs.

due(true, Kind, #kind{since_report = Count} = Counted, Now, #report{kinds = Kinds} = Report) ->
    {report, Count, Report#report{kinds = Kinds#{Kind => Counted#kind{since_report = 0, reported_at = Now}}}};
due(false, Kind, Counted, _Now, #report{kinds = Kinds} = Report) ->
    {quiet, Report#report{kinds = Kinds#{Kind => Counted}}}.
