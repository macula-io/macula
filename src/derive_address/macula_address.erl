%%%-------------------------------------------------------------------
%%% @doc macula-net address derivation: pubkey -> IPv6.
%%%
%%% Per the macula-net spec (PLAN_MACULA_NET.md §3.1):
%%%
%%% ```
%%%   addr = 0xfd | blake3(realm_master_pubkey)[0:40 bits]
%%%               | blake3(identity_pubkey)[0:80 bits]
%%% ```
%%%
%%% Uses {@link macula_blake3_nif:hash/1} for the BLAKE3 primitive — no
%%% new NIF; reuses the SDK's existing crypto layer.
%%%
%%% == Realm-scoped identity ==
%%%
%%% An "identity" here is a realm-scoped Ed25519 keypair. A user/daemon
%%% in N realms holds N realm-scoped keypairs and gets N addresses (one
%%% per realm, all unlinkable at L3). See spec §3.6 for the full model.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_address).

-export([
    derive/2,
    format/1,
    derive_and_format/2
]).

-export_type([
    pubkey/0,
    ipv6_address/0,
    ipv6_text/0
]).

-type pubkey()       :: <<_:256>>.   %% 32-byte Ed25519 pubkey
-type ipv6_address() :: <<_:128>>.   %% 16-byte IPv6
-type ipv6_text()    :: binary().    %% RFC 5952 lowercase

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Derive a macula-net IPv6 address from a realm + identity keypair.
-spec derive(RealmMasterPubkey :: pubkey(), IdentityPubkey :: pubkey()) ->
    ipv6_address().
derive(RealmMasterPubkey, IdentityPubkey)
  when is_binary(RealmMasterPubkey), byte_size(RealmMasterPubkey) =:= 32,
       is_binary(IdentityPubkey),    byte_size(IdentityPubkey)    =:= 32 ->
    <<RealmHashHead:5/binary, _/binary>> =
        macula_blake3_nif:hash(RealmMasterPubkey),
    <<IdHashHead:10/binary, _/binary>> =
        macula_blake3_nif:hash(IdentityPubkey),
    <<16#fd, RealmHashHead/binary, IdHashHead/binary>>.

%% @doc Render a 16-byte IPv6 binary as RFC 5952 lowercase text.
-spec format(ipv6_address()) -> ipv6_text().
format(<<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>) ->
    Groups = [A, B, C, D, E, F, G, H],
    iolist_to_binary(format_rfc5952(Groups)).

%% @doc Convenience: derive + format in one call.
-spec derive_and_format(pubkey(), pubkey()) -> ipv6_text().
derive_and_format(RealmMasterPubkey, IdentityPubkey) ->
    format(derive(RealmMasterPubkey, IdentityPubkey)).

%% =============================================================================
%% RFC 5952 formatter
%% =============================================================================

%% Find the longest run of consecutive zero groups (length >= 2) and
%% replace it with '::'. The first zero of any tie wins per RFC 5952 §4.2.3.
format_rfc5952(Groups) ->
    {Start, Len} = longest_zero_run(Groups),
    case Len >= 2 of
        true  -> format_with_compression(Groups, Start, Len);
        false -> hex_groups_joined(Groups, ":")
    end.

format_with_compression(Groups, Start, Len) ->
    {Head, Tail0} = lists:split(Start, Groups),
    {_, Tail} = lists:split(Len, Tail0),
    HeadStr = case Head of
        []    -> "";
        _     -> hex_groups_joined(Head, ":")
    end,
    TailStr = case Tail of
        []    -> "";
        _     -> hex_groups_joined(Tail, ":")
    end,
    [HeadStr, "::", TailStr].

hex_groups_joined(Groups, Sep) ->
    lists:join(Sep, [io_lib:format("~.16b", [G]) || G <- Groups]).

%% Returns {StartIndex, RunLength} of the longest run of zeros.
longest_zero_run(Groups) ->
    longest_zero_run(Groups, 0, 0, 0, 0, 0).

longest_zero_run([], _Idx, _CurStart, _CurLen, BestStart, BestLen) ->
    {BestStart, BestLen};
longest_zero_run([0 | T], Idx, CurStart0, CurLen, BestStart, BestLen) ->
    {CurStart, NewCurLen} =
        case CurLen of
            0 -> {Idx, 1};
            _ -> {CurStart0, CurLen + 1}
        end,
    case NewCurLen > BestLen of
        true  -> longest_zero_run(T, Idx + 1, CurStart, NewCurLen, CurStart, NewCurLen);
        false -> longest_zero_run(T, Idx + 1, CurStart, NewCurLen, BestStart, BestLen)
    end;
longest_zero_run([_ | T], Idx, _CurStart, _CurLen, BestStart, BestLen) ->
    longest_zero_run(T, Idx + 1, 0, 0, BestStart, BestLen).
