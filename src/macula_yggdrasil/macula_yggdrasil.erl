%% @doc Yggdrasil-network helpers — pubkey-derived IPv6 addressing
%% and self-signed Ed25519 cert generation.
%%
%% Phase 1 (Tier 3) of the sovereign-overlay rollout. See the
%% plan at PLAN_SOVEREIGN_OVERLAY_PHASE1.md (in macula-architecture)
%% — §4.2 for the address derivation algorithm and §4.3 for the
%% self-signed cert path.
%%
%% Pure functions only. No network I/O. The Yggdrasil sidecar
%% (yggdrasilnetwork/yggdrasil-go) is what actually routes the
%% packets through the overlay; this module is the
%% pubkey/address bridge and the cert generator.
%%
%% Addressing matches the upstream yggdrasil-go reference
%% implementation (src/address/address.go, AddrForKey):
%%
%%   1. Take a 32-byte Ed25519 public key.
%%   2. Bitwise invert it.
%%   3. Count leading 1-bits (call it N) — this becomes byte 1
%%      of the IPv6 address.
%%   4. Skip those N 1-bits and the single 0-bit that terminates
%%      the run.
%%   5. Take the next 112 bits — these are the low 14 bytes of
%%      the IPv6 address.
%%   6. Prepend 0x02 (Yggdrasil node-address prefix; subnet
%%      uses 0x03).
%%
%% The resulting 200::/7 address is the routable IPv6 of the
%% Yggdrasil node owning that keypair.
-module(macula_yggdrasil).

-export([address_for/1,
         format_address/1,
         cert_for/1,
         cert_for/2]).

-export_type([ipv6/0, key_pair/0, cert_pem/0]).

-type ipv6()    :: <<_:128>>.
-type key_pair() :: {Pubkey :: binary(), Privkey :: binary()}.
-type cert_pem() :: {CertPem :: binary(), KeyPem :: binary()}.

%%==================================================================
%% Public API
%%==================================================================

%% @doc Derive the Yggdrasil IPv6 address (16 raw bytes) for an
%% Ed25519 public key.
-spec address_for(macula_identity:pubkey()) -> ipv6().
address_for(Pubkey) when is_binary(Pubkey), byte_size(Pubkey) =:= 32 ->
    Inverted = invert(Pubkey),
    Ones = count_leading_ones(Inverted),
    Tail = take_after_skip(Inverted, Ones + 1, 14),
    <<16#02, Ones, Tail/binary>>.

%% @doc Format a 16-byte IPv6 binary as the canonical
%% colon-separated string. Useful for logs and config.
-spec format_address(ipv6()) -> binary().
format_address(<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>) ->
    iolist_to_binary(
        io_lib:format("~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b",
                      [A, B, C, D, E, F, G, H])).

%% @doc Generate a self-signed X.509 cert wrapping an Ed25519
%% identity keypair, with the derived Yggdrasil IPv6 as the sole
%% Subject Alternative Name. Used by stations seeding their
%% sovereign-overlay listener.
-spec cert_for(key_pair()) -> {ok, cert_pem()} | {error, term()}.
cert_for({Pubkey, Privkey}) ->
    cert_for({Pubkey, Privkey}, []).

%% @doc Same as `cert_for/1', plus extra DNS Subject Alternative
%% Names (e.g. the macula.io hostname for compatibility with
%% existing hostname-based dial code paths).
-spec cert_for(key_pair(), [binary() | string()]) ->
    {ok, cert_pem()} | {error, term()}.
cert_for({Pubkey, Privkey}, ExtraSans)
        when is_binary(Pubkey), byte_size(Pubkey) =:= 32,
             is_binary(Privkey), byte_size(Privkey) =:= 32 ->
    Addr = address_for(Pubkey),
    AddrSan = format_address(Addr),
    macula_quic:generate_self_signed_cert(Pubkey, Privkey,
                                          [AddrSan | ExtraSans]).

%%==================================================================
%% Internals
%%==================================================================

-spec invert(binary()) -> binary().
invert(Bin) ->
    << <<(B bxor 16#ff):8>> || <<B:8>> <= Bin >>.

-spec count_leading_ones(bitstring()) -> non_neg_integer().
count_leading_ones(Bits) ->
    count_leading_ones(Bits, 0).

count_leading_ones(<<1:1, Rest/bitstring>>, N) ->
    count_leading_ones(Rest, N + 1);
count_leading_ones(_, N) ->
    N.

-spec take_after_skip(bitstring(), non_neg_integer(), pos_integer()) ->
    binary().
take_after_skip(Bin, SkipBits, NBytes) ->
    NBits = NBytes * 8,
    <<_:SkipBits/bitstring, Taken:NBits/bitstring, _/bitstring>> = Bin,
    %% NBits is always a byte multiple → bitstring is a real binary.
    Taken.
