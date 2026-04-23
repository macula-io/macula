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

-export([pack/1, unpack/1, is_nif_loaded/0]).

%% NIF stubs
-export([nif_pack/1, nif_unpack/1]).

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

%%%===================================================================
%%% NIF stubs (replaced at load time)
%%%===================================================================

nif_pack(_Term)    -> ?NIF_NOT_LOADED.
nif_unpack(_Bytes) -> ?NIF_NOT_LOADED.
