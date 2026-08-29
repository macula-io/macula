%%%-------------------------------------------------------------------
%%% @doc Segment-wise wildcard matching for hierarchical mesh addresses
%%% — pubsub topics, RPC procedure names, capability advertisements —
%%% shared by any caller that needs "does this pattern match this
%%% concrete address" without committing to one fixed segment count.
%%%
%%% `*' is a literal, single-segment wildcard: it matches exactly one
%%% segment, in exactly that position, and nothing else — no multi-
%%% segment (`**') wildcards, no partial-segment globs. Both `Pattern'
%%% and `Concrete' are ordinary lists of binaries (already split on
%%% whatever delimiter the caller's own address format uses) — this
%%% module has no opinion on segment count or delimiter, deliberately:
%%% `hecate_om_capabilities''s capability names are 2 dynamic segments
%%% (org, name); `macula_topic''s pubsub/RPC topics are 4
%%% (org-or-`_org' sentinel, app-or-`_realm' sentinel, domain, name).
%%% Both fit this same primitive, unmodified — a mismatched segment
%%% count between `Pattern' and `Concrete' is simply not a match,
%%% never a crash.
%%%
%%% A concrete address never itself contains `*' — that is a query-side
%%% (or subscription-side) syntax only. This module does not enforce
%%% that; a caller advertising or publishing a literal `*' segment is a
%%% caller error, not something this module can or should police.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_topic_pattern).

-export([matches/2]).

-define(WILDCARD, <<"*">>).

%% @doc Whether `Pattern' matches `Concrete', segment by segment.
-spec matches([binary()], [binary()]) -> boolean().
matches(Pattern, Concrete)
  when is_list(Pattern), is_list(Concrete),
       length(Pattern) =:= length(Concrete) ->
    lists:all(fun segment_matches/1, lists:zip(Pattern, Concrete));
matches(_Pattern, _Concrete) ->
    false.

segment_matches({?WILDCARD, _Concrete}) -> true;
segment_matches({Same, Same})           -> true;
segment_matches({_Pattern, _Concrete})  -> false.
