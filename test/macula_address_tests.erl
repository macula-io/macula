%%% @doc Tests for macula-net address derivation.
-module(macula_address_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM_PK_A,
    <<16#a3, 16#b1, 16#c4, 16#d2, 16#e5, 16#f6, 16#a7, 16#b8,
      16#c9, 16#da, 16#eb, 16#fc, 16#ad, 16#be, 16#cf, 16#d0,
      16#a1, 16#b2, 16#c3, 16#d4, 16#e5, 16#f6, 16#07, 16#18,
      16#29, 16#3a, 16#4b, 16#5c, 16#6d, 16#7e, 16#8f, 16#90>>).

-define(IDENTITY_PK_ALICE,
    <<16#c4, 16#d2, 16#a3, 16#b1, 16#e5, 16#f6, 16#07, 16#18,
      16#29, 16#3a, 16#4b, 16#5c, 16#6d, 16#7e, 16#8f, 16#90,
      16#a1, 16#b2, 16#c3, 16#d4, 16#a5, 16#b6, 16#c7, 16#d8,
      16#e9, 16#fa, 16#0b, 16#1c, 16#2d, 16#3e, 16#4f, 16#50>>).

-define(IDENTITY_PK_BOB,
    <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07, 16#08,
      16#09, 16#0a, 16#0b, 16#0c, 16#0d, 16#0e, 16#0f, 16#10,
      16#11, 16#12, 16#13, 16#14, 16#15, 16#16, 16#17, 16#18,
      16#19, 16#1a, 16#1b, 16#1c, 16#1d, 16#1e, 16#1f, 16#20>>).

%% =============================================================================
%% Address shape
%% =============================================================================

derive_returns_16_bytes_test() ->
    Addr = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE),
    ?assertEqual(16, byte_size(Addr)).

derive_starts_with_fd_prefix_test() ->
    <<First:8, _/binary>> = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE),
    ?assertEqual(16#fd, First).

%% =============================================================================
%% Determinism
%% =============================================================================

derive_is_deterministic_test() ->
    A1 = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE),
    A2 = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE),
    ?assertEqual(A1, A2).

%% =============================================================================
%% Realm scoping
%% =============================================================================

different_realms_produce_different_addresses_test() ->
    RealmB = <<0:256>>,
    AddrA = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE),
    AddrB = macula_address:derive(RealmB,      ?IDENTITY_PK_ALICE),
    ?assertNotEqual(AddrA, AddrB).

same_realm_shares_48_prefix_test() ->
    AddrAlice = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE),
    AddrBob   = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_BOB),
    <<PrefixA:48, _RestA/bits>> = AddrAlice,
    <<PrefixB:48, _RestB/bits>> = AddrBob,
    ?assertEqual(PrefixA, PrefixB).

different_identities_in_same_realm_differ_test() ->
    AddrAlice = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE),
    AddrBob   = macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_BOB),
    ?assertNotEqual(AddrAlice, AddrBob).

%% =============================================================================
%% Validation
%% =============================================================================

short_realm_pubkey_fails_test() ->
    ShortPk = <<1, 2, 3>>,
    ?assertError(function_clause,
        macula_address:derive(ShortPk, ?IDENTITY_PK_ALICE)).

short_identity_pubkey_fails_test() ->
    ShortPk = <<1, 2, 3>>,
    ?assertError(function_clause,
        macula_address:derive(?REALM_PK_A, ShortPk)).

%% =============================================================================
%% Format (RFC 5952)
%% =============================================================================

format_starts_with_fd_test() ->
    Text = macula_address:format(macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE)),
    ?assertMatch(<<"fd", _/binary>>, Text).

derive_and_format_helper_test() ->
    Text = macula_address:derive_and_format(?REALM_PK_A, ?IDENTITY_PK_ALICE),
    Direct = macula_address:format(macula_address:derive(?REALM_PK_A, ?IDENTITY_PK_ALICE)),
    ?assertEqual(Direct, Text).

format_compresses_zero_run_test() ->
    %% Construct a 16-byte address with a run of zeros to verify ::-compression.
    Addr = <<16#fd, 0:40, 16#aa, 0:72>>,
    ?assertEqual(16, byte_size(Addr)),
    Text = macula_address:format(Addr),
    %% Expected: fd00:0:0:aa00:: — at minimum should contain "::"
    ?assertNotEqual(nomatch, binary:match(Text, <<"::">>)).
