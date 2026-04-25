%% @doc Ed25519 identities and the S/Kademlia crypto puzzle.
%%
%% A Macula NodeId is an Ed25519 public key (32 bytes). Identities are
%% optionally "puzzle-hardened": the pubkey satisfies
%% `SHA-256(pubkey)' having at least N leading zero bits. This raises the
%% cost of mass identity minting (Sybil defence).
%%
%% See `plans/PLAN_MACULA_V2_PART1_FOUNDATIONS.md' sections 4.1–4.4.
-module(macula_identity).

-export([
    generate/0,
    generate/1,
    load/1,
    save/2,
    public/1,
    private/1,
    node_id/1,
    sign/2,
    verify/3,
    puzzle_evidence/1,
    puzzle_valid/1,
    puzzle_valid/2
]).

-export_type([pubkey/0, privkey/0, sig/0, key_pair/0, node_id/0]).

-type pubkey()   :: <<_:256>>.
-type privkey()  :: <<_:256>>.
-type sig()      :: <<_:512>>.
-type node_id()  :: pubkey().
-type key_pair() :: #{public := pubkey(), private := privkey()}.

-define(KEY_FILE_MAGIC, "macula-v2-key\0").
-define(DEFAULT_PUZZLE_DIFFICULTY, 8).

%%------------------------------------------------------------------
%% Generation
%%------------------------------------------------------------------

%% @doc Generate a fresh Ed25519 key pair. Does not grind a puzzle.
-spec generate() -> key_pair().
generate() ->
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    #{public => Pub, private => Priv}.

%% @doc Generate a key pair, optionally grinding until the puzzle is satisfied.
%%
%% Opts:
%% <ul>
%%   <li>`puzzle' :: boolean() — default false</li>
%%   <li>`difficulty' :: non_neg_integer() — leading zero bits required</li>
%% </ul>
-spec generate(#{puzzle => boolean(), difficulty => non_neg_integer(), _ => _}) ->
    key_pair().
generate(#{puzzle := true} = Opts) ->
    Difficulty = maps:get(difficulty, Opts, default_difficulty()),
    grind(Difficulty);
generate(_Opts) ->
    generate().

%%------------------------------------------------------------------
%% Persistence — atomic write with 0600 permissions.
%%------------------------------------------------------------------

%% @doc Load a key pair from disk.
-spec load(file:name_all()) -> {ok, key_pair()} | {error, term()}.
load(Path) ->
    decode_key_file(file:read_file(Path)).

-spec decode_key_file({ok, binary()} | {error, term()}) ->
    {ok, key_pair()} | {error, term()}.
decode_key_file({ok, <<?KEY_FILE_MAGIC, Pub:32/binary, Priv:32/binary>>}) ->
    {ok, #{public => Pub, private => Priv}};
decode_key_file({ok, _Blob}) ->
    {error, bad_key_file};
decode_key_file({error, _} = Err) ->
    Err.

%% @doc Save a key pair to disk atomically (write-tmp + rename) with 0600 perms.
-spec save(file:name_all(), key_pair()) -> ok | {error, term()}.
save(Path, #{public := Pub, private := Priv})
  when byte_size(Pub) =:= 32, byte_size(Priv) =:= 32 ->
    Blob = <<?KEY_FILE_MAGIC, Pub/binary, Priv/binary>>,
    Tmp  = iolist_to_binary([Path, ".tmp"]),
    ok = filelib:ensure_dir(Path),
    write_and_rename(Tmp, Path, Blob).

-spec write_and_rename(file:name_all(), file:name_all(), binary()) ->
    ok | {error, term()}.
write_and_rename(Tmp, Path, Blob) ->
    case file:write_file(Tmp, Blob, [raw, binary]) of
        ok         -> finalise_key_file(Tmp, Path);
        {error, _} = E -> E
    end.

-spec finalise_key_file(file:name_all(), file:name_all()) -> ok | {error, term()}.
finalise_key_file(Tmp, Path) ->
    _ = file:change_mode(Tmp, 8#0600),
    file:rename(Tmp, Path).

%%------------------------------------------------------------------
%% Accessors
%%------------------------------------------------------------------

-spec public(key_pair()) -> pubkey().
public(#{public := Pub}) -> Pub.

-spec private(key_pair()) -> privkey().
private(#{private := Priv}) -> Priv.

%% @doc NodeId of an identity. Phase 1: NodeId == public key.
-spec node_id(key_pair() | pubkey()) -> node_id().
node_id(#{public := Pub}) -> Pub;
node_id(Pub) when is_binary(Pub), byte_size(Pub) =:= 32 -> Pub.

%%------------------------------------------------------------------
%% Sign / verify — raw Ed25519. Callers add domain separation
%% (see hecate_record, hecate_frame).
%%------------------------------------------------------------------

-spec sign(iodata(), key_pair() | privkey()) -> sig().
sign(Msg, #{private := Priv}) ->
    sign(Msg, Priv);
sign(Msg, Priv) when is_binary(Priv), byte_size(Priv) =:= 32 ->
    crypto:sign(eddsa, none, Msg, [Priv, ed25519]).

-spec verify(iodata(), sig(), pubkey()) -> boolean().
verify(Msg, Sig, Pub)
  when is_binary(Sig), byte_size(Sig) =:= 64,
       is_binary(Pub), byte_size(Pub) =:= 32 ->
    crypto:verify(eddsa, none, Msg, Sig, [Pub, ed25519]).

%%------------------------------------------------------------------
%% Crypto puzzle (S/Kademlia Sybil defence).
%%------------------------------------------------------------------

%% @doc SHA-256 of the public key — the proof-of-work output measured.
-spec puzzle_evidence(pubkey() | key_pair()) -> <<_:256>>.
puzzle_evidence(#{public := Pub}) ->
    puzzle_evidence(Pub);
puzzle_evidence(Pub) when is_binary(Pub), byte_size(Pub) =:= 32 ->
    crypto:hash(sha256, Pub).

%% @doc Puzzle validity against the application-configured difficulty.
-spec puzzle_valid(pubkey() | key_pair()) -> boolean().
puzzle_valid(X) ->
    puzzle_valid(X, default_difficulty()).

-spec puzzle_valid(pubkey() | key_pair(), non_neg_integer()) -> boolean().
puzzle_valid(X, Difficulty) when is_integer(Difficulty), Difficulty >= 0 ->
    has_leading_zero_bits(puzzle_evidence(X), Difficulty).

%%------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------

-spec default_difficulty() -> non_neg_integer().
default_difficulty() ->
    application:get_env(macula_identity, puzzle_difficulty, ?DEFAULT_PUZZLE_DIFFICULTY).

-spec grind(non_neg_integer()) -> key_pair().
grind(Difficulty) ->
    Kp = generate(),
    grind_loop(Kp, Difficulty, puzzle_valid(Kp, Difficulty)).

-spec grind_loop(key_pair(), non_neg_integer(), boolean()) -> key_pair().
grind_loop(Kp, _Difficulty, true) ->
    Kp;
grind_loop(_Kp, Difficulty, false) ->
    grind(Difficulty).

-spec has_leading_zero_bits(binary(), non_neg_integer()) -> boolean().
has_leading_zero_bits(_Bin, 0) ->
    true;
has_leading_zero_bits(Bin, N)
  when is_integer(N), N > 0, N =< bit_size(Bin) ->
    <<Prefix:N, _/bitstring>> = Bin,
    Prefix =:= 0;
has_leading_zero_bits(_Bin, _N) ->
    false.
