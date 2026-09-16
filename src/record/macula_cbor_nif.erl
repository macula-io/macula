%%%-------------------------------------------------------------------
%%% @doc CBOR (RFC 8949) pack/unpack for the Macula mesh wire protocol.
%%%
%%% Replaces the pure-Erlang msgpack hex package as the per-frame
%%% serialization layer. CBOR was chosen because it composes with
%%% UCAN, DID, COSE, and IPLD (all of which the platform already
%%% uses for identity and auth), has deterministic encoding rules
%%% in the spec (RFC 8949 §4.2.1), and is an IETF standard.
%%%
%%% Implementation: thin wrapper over the macula_cbor_nif Rust NIF
%%% backed by the ciborium crate. There is NO Erlang fallback —
%%% the protocol layer is in the same critical path as macula_quic,
%%% which also has no Erlang fallback. Failing fast at NIF-load time
%%% is the right behavior; a slow Erlang fallback would silently
%%% halve throughput.
%%%
%%% Type mapping (Erlang -> CBOR):
%%%
%%%   atom (true, false)       -> bool
%%%   atom (nil, undefined)    -> null
%%%   atom (other)                 -> text string (LOSSY — decoder returns binary)
%%%   binary                       -> byte string (round-trips as binary)
%%%   integer                      -> uint / negative int
%%%   float                        -> float
%%%   list                         -> array
%%%   tuple                        -> array (LOSSY — decoder returns list)
%%%   map                          -> map
%%%
%%% Atoms and tuples lose their type information across the wire —
%%% same constraint as the previous msgpack-era protocol.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cbor_nif).

-export([pack/1, unpack/1, pack_deterministic/1, unpack_deterministic/1,
         unpack_deterministic/2, element_budget/0, is_nif_loaded/0]).

%% NIF stubs that the NIF replaces at load, called only from this module,
%% so none is exported.

-on_load(init/0).

-define(NIF_NOT_LOADED, erlang:nif_error(nif_not_loaded)).

%%%===================================================================
%%% NIF Loading
%%%===================================================================

init() ->
    PrivDir = code:priv_dir(macula),
    SoName = filename:join(PrivDir, "macula_cbor_nif"),
    case erlang:load_nif(SoName, 0) of
        ok ->
            persistent_term:put({?MODULE, nif_loaded}, true),
            ok;
        {error, {reload, _}} ->
            persistent_term:put({?MODULE, nif_loaded}, true),
            ok;
        {error, Reason} ->
            persistent_term:put({?MODULE, nif_loaded}, false),
            logger:warning("[macula_cbor] NIF load failed: ~p (path: ~s)",
                           [Reason, SoName]),
            ok
    end.

-spec is_nif_loaded() -> boolean().
is_nif_loaded() ->
    persistent_term:get({?MODULE, nif_loaded}, false).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Encode an Erlang term to CBOR bytes.
%% Crashes ({nif_error, ...}) if the NIF failed to load —
%% intentional, see module-level note on no-fallback.
-spec pack(term()) -> binary().
pack(Term) ->
    nif_pack(Term).

%% @doc Decode CBOR bytes to an Erlang term.
%% Returns an ok-tuple on success or an error-tuple on malformed input.
-spec unpack(binary()) -> {ok, term()} | {error, term()}.
unpack(Bytes) when is_binary(Bytes) ->
    nif_unpack(Bytes).

%% @doc Deterministic CBOR (RFC 8949 §4.2.1) encode, matching
%% `macula_record_cbor:encode/1' byte-for-byte — see that module and
%% `native/macula_cbor_nif/src/deterministic.rs' for the exact value
%% model. NOT wired into `macula_frame'/`macula_record' yet: additive
%% only, pending exhaustive differential testing against the existing
%% Erlang codec. Crashes on unencodable input, same as
%% `macula_record_cbor:encode/1' (no clause matches).
-spec pack_deterministic(macula_record_cbor:value()) -> binary().
pack_deterministic(Term) ->
    nif_pack_deterministic(Term).

%% @doc Deterministic CBOR decode, matching
%% `macula_record_cbor:decode/1' exactly, including its strict
%% "no trailing bytes" requirement and its crash-on-malformed-input
%% contract (this raises, it does not return an error tuple).
-spec unpack_deterministic(binary()) -> macula_record_cbor:value().
unpack_deterministic(Bytes) when is_binary(Bytes) ->
    nif_unpack_deterministic(Bytes).

%% @doc Deterministic CBOR decode within `Left' CBOR items, what a caller
%% has left of the element budget, returning `{Term, LeftAfter}' with what
%% is left after it. `Left' never counts for more than `element_budget/0'.
%% Raises as `unpack_deterministic/1' does, with `too_many_elements' when
%% the input holds more items than `Left'. A frame and the records nested
%% in it decode within one budget this way.
-spec unpack_deterministic(binary(), non_neg_integer()) ->
    {macula_record_cbor:value(), non_neg_integer()}.
unpack_deterministic(Bytes, Left) when is_binary(Bytes), is_integer(Left), Left >= 0 ->
    nif_unpack_deterministic_within(Bytes, Left).

%% @doc The element budget a decode starts with: the most CBOR items
%% `unpack_deterministic/1' reads from one input, and the most a frame and
%% the records in it hold together.
-spec element_budget() -> pos_integer().
element_budget() ->
    nif_element_budget().

%%%===================================================================
%%% NIF stubs (replaced at load time)
%%%===================================================================

nif_pack(_Term)    -> ?NIF_NOT_LOADED.
nif_unpack(_Bytes) -> ?NIF_NOT_LOADED.
nif_pack_deterministic(_Term)    -> ?NIF_NOT_LOADED.
nif_unpack_deterministic(_Bytes) -> ?NIF_NOT_LOADED.
nif_unpack_deterministic_within(_Bytes, _Left) -> ?NIF_NOT_LOADED.
nif_element_budget() -> ?NIF_NOT_LOADED.
