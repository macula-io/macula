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
%% Pure: the owner keeps the value and passes the clock in, in milliseconds.
-module(macula_refusal_report).

-export([new/1, refused/3, counts/1]).

-export_type([t/0]).

-record(kind, {
    total = 0          :: non_neg_integer(),
    since_report = 0   :: non_neg_integer(),
    reported_at        :: integer() | never
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
    #kind{total = Total, since_report = Since, reported_at = At} =
        maps:get(Kind, Kinds, #kind{reported_at = never}),
    Counted = #kind{total = Total + 1, since_report = Since + 1, reported_at = At},
    due(report_due(At, NowMs, WindowMs), Kind, Counted, NowMs, Report).

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
