%% @doc Source-route header codec (Part 6 §11).
%%
%% Wire layout (big-endian throughout):
%%
%% <pre>
%%   0       1       2       3
%%   +-------+-------+-------+-------+
%%   | ver   | total | curr  | dead-
%%   +-------+-------+-------+-------+
%%   ...    deadline (8 bytes)    ...
%%   +-------+-------+-------+-------+
%%   ...    path_hash (16 bytes)  ...
%%   +-------+-------+-------+-------+
%%   ...    hops[0..total_hops-1] (each 16 bytes — NodeId prefix)
%% </pre>
%%
%% <ul>
%%   <li><code>version</code> (1 byte) — currently `1'.</li>
%%   <li><code>total_hops</code> (1 byte) — `1..8'.</li>
%%   <li><code>current_hop</code> (1 byte) — index of the hop currently
%%       processing. `new/2,3' creates a header with `0'; each hop
%%       advances by 1 before forwarding; the destination
%%       observes `current_hop = total_hops - 1' and after one
%%       final advance sees `is_complete/1 = true'.</li>
%%   <li><code>deadline</code> (8 bytes, unsigned) — absolute Unix ms
%%       past which the CALL is dropped (BOLT#4 `expiry_too_soon').</li>
%%   <li><code>path_hash</code> (16 bytes) — first 16 bytes of
%%       `SHA-256(concat(hops))'. Computed once at the origin and
%%       checked at every hop. Tampering with the hop sequence
%%       breaks the hash.</li>
%%   <li><code>hops</code> — `total_hops × 16' bytes; each entry is
%%       the first 16 bytes of the NodeId at that hop.</li>
%% </ul>
%%
%% Fixed overhead is 27 bytes (`1 + 1 + 1 + 8 + 16'). A path of
%% `N' hops occupies `27 + 16*N' bytes; maximum (`N = 8') is
%% 155 bytes.
%%
%% Reference: plans/PLAN_MACULA_V2_PART6_PROTOCOL.md §11;
%% plans/PLAN_MACULA_V2_PART3_DISCOVERY.md §6.6;
%% plans/PLAN_PHASE_4_BREAKDOWN.md Session 4.2.
-module(macula_source_route).

-export([
    new/2, new/3,
    encode/1,
    decode/1,
    verify/1,
    advance/1,
    deadline/1,
    version/1,
    total_hops/1,
    current_hop/1,
    hops/1,
    path_hash/1,
    current_hop_id/1,
    next_hop_id/1,
    is_complete/1,
    is_final_hop/1,
    truncate_hop/1
]).

-export_type([header/0, hop_id/0, decode_error/0]).

-define(VERSION,        1).
-define(MAX_HOPS,       8).
-define(HOP_BYTES,     16).
-define(HASH_BYTES,    16).
-define(FIXED_OVERHEAD, 27).

-type hop_id() :: <<_:128>>.

-type header() :: #{
    version     := non_neg_integer(),
    total_hops  := pos_integer(),
    current_hop := non_neg_integer(),
    deadline    := non_neg_integer(),
    path_hash   := <<_:128>>,
    hops        := [hop_id(), ...]
}.

-type decode_error() ::
      bad_header
    | bad_version
    | bad_total_hops
    | bad_current_hop
    | path_hash_mismatch
    | truncated.

%%=====================================================================
%% Construction
%%=====================================================================

%% @doc Build a header from a list of hops + an absolute deadline
%% (Unix ms). Each hop may be an already-truncated 16-byte ID or a
%% full 32-byte NodeId; full IDs are truncated to their first 16
%% bytes per the wire format.
-spec new([hop_id() | macula_identity:pubkey()],
          non_neg_integer()) -> header().
new(Hops, DeadlineMs) ->
    new(Hops, DeadlineMs, 0).

-spec new([hop_id() | macula_identity:pubkey()],
          non_neg_integer(), non_neg_integer()) -> header().
new(Hops, DeadlineMs, CurrentHop)
  when is_list(Hops),
       is_integer(DeadlineMs), DeadlineMs >= 0,
       is_integer(CurrentHop), CurrentHop >= 0 ->
    Truncated = [truncate_hop(H) || H <- Hops],
    Total = length(Truncated),
    valid_total(Total),
    valid_current(CurrentHop, Total),
    #{
        version     => ?VERSION,
        total_hops  => Total,
        current_hop => CurrentHop,
        deadline    => DeadlineMs,
        path_hash   => compute_hash(Truncated),
        hops        => Truncated
    }.

%%=====================================================================
%% Wire codec
%%=====================================================================

-spec encode(header()) -> binary().
encode(#{version := V, total_hops := T, current_hop := C,
         deadline := D, path_hash := H, hops := Hops})
  when is_integer(V), V >= 0, V =< 255,
       is_integer(T), T >= 1, T =< ?MAX_HOPS,
       is_integer(C), C >= 0, C =< T,
       is_integer(D), D >= 0,
       is_binary(H), byte_size(H) =:= ?HASH_BYTES,
       length(Hops) =:= T ->
    HopsBin = << <<Hop:?HOP_BYTES/binary>> || Hop <- Hops >>,
    <<V:8, T:8, C:8, D:64/big-unsigned, H/binary, HopsBin/binary>>.

%% @doc Decode a wire-format header. Verifies the path_hash so a
%% caller can trust the structure was not tampered in flight.
-spec decode(binary()) -> {ok, header()} | {error, decode_error()}.
decode(Bin) when is_binary(Bin), byte_size(Bin) >= ?FIXED_OVERHEAD ->
    <<V:8, T:8, C:8, D:64/big-unsigned, H:?HASH_BYTES/binary,
      Rest/binary>> = Bin,
    parse_decoded(V, T, C, D, H, Rest);
decode(_) ->
    {error, truncated}.

-spec parse_decoded(byte(), byte(), byte(), non_neg_integer(),
                    binary(), binary()) ->
        {ok, header()} | {error, decode_error()}.
parse_decoded(V, _T, _C, _D, _H, _Rest) when V =/= ?VERSION ->
    {error, bad_version};
parse_decoded(_V, T, _C, _D, _H, _Rest) when T < 1; T > ?MAX_HOPS ->
    {error, bad_total_hops};
parse_decoded(_V, T, C, _D, _H, _Rest) when C > T ->
    {error, bad_current_hop};
parse_decoded(V, T, C, D, H, Rest) ->
    parse_hops(V, T, C, D, H, Rest).

-spec parse_hops(byte(), byte(), byte(), non_neg_integer(),
                 binary(), binary()) ->
        {ok, header()} | {error, decode_error()}.
parse_hops(V, T, C, D, H, Rest) when byte_size(Rest) >= T * ?HOP_BYTES ->
    HopsByteCount = T * ?HOP_BYTES,
    <<HopsBin:HopsByteCount/binary, _Tail/binary>> = Rest,
    Hops = [Hop || <<Hop:?HOP_BYTES/binary>> <= HopsBin],
    verify_hash(compute_hash(Hops) =:= H, V, T, C, D, H, Hops);
parse_hops(_V, _T, _C, _D, _H, _Rest) ->
    {error, truncated}.

-spec verify_hash(boolean(), byte(), byte(), byte(),
                  non_neg_integer(), binary(), [hop_id()]) ->
        {ok, header()} | {error, decode_error()}.
verify_hash(false, _V, _T, _C, _D, _H, _Hops) ->
    {error, path_hash_mismatch};
verify_hash(true, V, T, C, D, H, Hops) ->
    {ok, #{version     => V,
           total_hops  => T,
           current_hop => C,
           deadline    => D,
           path_hash   => H,
           hops        => Hops}}.

%%=====================================================================
%% Verification + position management
%%=====================================================================

%% @doc Recompute the path_hash and check it matches the header's
%% claim. `decode/1' already runs this check, so callers only need
%% `verify/1' when they synthesise headers in memory or want a
%% defensive check after a structural mutation.
-spec verify(header()) -> ok | {error, path_hash_mismatch}.
verify(#{path_hash := H, hops := Hops}) ->
    verify_compare(compute_hash(Hops) =:= H).

verify_compare(true)  -> ok;
verify_compare(false) -> {error, path_hash_mismatch}.

%% @doc Advance to the next hop. Errors if already complete.
-spec advance(header()) -> header().
advance(#{current_hop := C, total_hops := T} = H) when C < T ->
    H#{current_hop := C + 1};
advance(_) ->
    error(path_already_complete).

%%=====================================================================
%% Accessors
%%=====================================================================

-spec version(header())     -> non_neg_integer().
version(#{version := V})    -> V.

-spec total_hops(header())  -> pos_integer().
total_hops(#{total_hops := T}) -> T.

-spec current_hop(header()) -> non_neg_integer().
current_hop(#{current_hop := C}) -> C.

-spec deadline(header())    -> non_neg_integer().
deadline(#{deadline := D})  -> D.

-spec path_hash(header())   -> <<_:128>>.
path_hash(#{path_hash := H}) -> H.

-spec hops(header())        -> [hop_id(), ...].
hops(#{hops := Hops})       -> Hops.

%% @doc The hop currently processing this frame (i.e. the receiver
%% expected to handle this incoming CALL hop). `error' if the
%% header is already complete.
-spec current_hop_id(header()) -> {ok, hop_id()} | error.
current_hop_id(#{current_hop := C, total_hops := T}) when C >= T ->
    error;
current_hop_id(#{current_hop := C, hops := Hops}) ->
    {ok, lists:nth(C + 1, Hops)}.

%% @doc The hop the current receiver should forward to next.
%% Returns `error' if the current hop is the final one (no next
%% hop to forward to — the call is delivered locally).
-spec next_hop_id(header()) -> {ok, hop_id()} | error.
next_hop_id(#{current_hop := C, total_hops := T}) when C + 1 >= T ->
    error;
next_hop_id(#{current_hop := C, hops := Hops}) ->
    {ok, lists:nth(C + 2, Hops)}.

%% @doc `true' iff every hop has been traversed (`current_hop ==
%% total_hops'). The final receiver advances once after delivery
%% to mark the path complete.
-spec is_complete(header()) -> boolean().
is_complete(#{current_hop := C, total_hops := T}) -> C >= T.

%% @doc `true' iff the receiver of this frame is the destination —
%% no further forward needed.
-spec is_final_hop(header()) -> boolean().
is_final_hop(#{current_hop := C, total_hops := T}) -> C + 1 =:= T.

%%=====================================================================
%% Hop-ID truncation
%%=====================================================================

%% @doc Reduce a 32-byte NodeId (or anything ≥16 bytes) to its
%% first 16 bytes, matching the wire layout. 16-byte inputs are
%% returned unchanged.
-spec truncate_hop(binary()) -> hop_id().
truncate_hop(<<H:?HOP_BYTES/binary>>) ->
    H;
truncate_hop(<<H:?HOP_BYTES/binary, _Tail/binary>>) ->
    H.

%%=====================================================================
%% Internals
%%=====================================================================

-spec valid_total(integer()) -> ok.
valid_total(N) when N >= 1, N =< ?MAX_HOPS -> ok.

-spec valid_current(integer(), integer()) -> ok.
valid_current(C, T) when C >= 0, C =< T -> ok.

-spec compute_hash([binary()]) -> <<_:128>>.
compute_hash(Hops) ->
    Concat = iolist_to_binary(Hops),
    <<Trunc:?HASH_BYTES/binary, _/binary>> = crypto:hash(sha256, Concat),
    Trunc.
