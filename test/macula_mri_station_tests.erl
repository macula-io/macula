%%%-------------------------------------------------------------------
%%% @doc Tests for the `station' MRI type added in macula 4.3.0.
%%%
%%% Station MRIs are self-rooted: the realm field carries an
%%% Ed25519 pubkey (z-base-32 encoded, 52 chars), there is no
%%% reverse-domain notation, and the path must be empty.
%%%
%%% Form: `mri:station:<52-char-z32-pubkey>'.
%%%
%%% Required by hecate-daemon's serve_dns_over_mesh slice for
%%% synthesising station qnames (e.g., `<z32(pubkey)>._st.macula.io.').
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mri_station_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    case whereis(macula_mri_registry) of
        undefined -> {ok, _} = macula_mri_registry:start_link([]);
        _ -> ok
    end.

cleanup(_) -> ok.

station_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        fun parse_station_mri/0,
        fun format_station_mri/0,
        fun roundtrip_station/0,
        fun new_station_via_general_constructor/0,
        fun rejects_station_with_path/0,
        fun rejects_station_with_short_pubkey/0,
        fun rejects_station_with_invalid_z32/0,
        fun rejects_station_with_uppercase_pubkey/0,
        fun station_is_builtin_type/0
     ]}.

%%===================================================================
%% Parse / format / round-trip
%%===================================================================

parse_station_mri() ->
    Pubkey = crypto:strong_rand_bytes(32),
    Z32 = macula_z32:encode(Pubkey),
    Mri = <<"mri:station:", Z32/binary>>,
    {ok, Parsed} = macula_mri:parse(Mri),
    ?assertEqual(station, maps:get(type, Parsed)),
    ?assertEqual(Z32, maps:get(realm, Parsed)),
    ?assertEqual([], maps:get(path, Parsed)).

format_station_mri() ->
    Pubkey = crypto:strong_rand_bytes(32),
    Z32 = macula_z32:encode(Pubkey),
    Map = #{type => station, realm => Z32, path => []},
    Expected = <<"mri:station:", Z32/binary>>,
    ?assertEqual(Expected, macula_mri:format(Map)).

roundtrip_station() ->
    %% A station MRI parsed and re-formatted matches the original.
    Pubkey = crypto:strong_rand_bytes(32),
    Z32 = macula_z32:encode(Pubkey),
    Original = <<"mri:station:", Z32/binary>>,
    {ok, Parsed} = macula_mri:parse(Original),
    Reformatted = macula_mri:format(Parsed),
    ?assertEqual(Original, Reformatted).

new_station_via_general_constructor() ->
    %% macula_mri:new/3 should accept the station type.
    Pubkey = crypto:strong_rand_bytes(32),
    Z32 = macula_z32:encode(Pubkey),
    {ok, Mri} = macula_mri:new(station, Z32, []),
    Expected = <<"mri:station:", Z32/binary>>,
    ?assertEqual(Expected, Mri).

%%===================================================================
%% Validation rejects malformed station MRIs
%%===================================================================

rejects_station_with_path() ->
    Pubkey = crypto:strong_rand_bytes(32),
    Z32 = macula_z32:encode(Pubkey),
    %% Station type with non-empty path → must reject; stations
    %% are atomic identifiers.
    ?assertEqual({error, station_must_have_empty_path},
                 macula_mri:new(station, Z32, [<<"path">>])).

rejects_station_with_short_pubkey() ->
    %% z32 input that decodes to fewer than 32 bytes → reject.
    Short = macula_z32:encode(crypto:strong_rand_bytes(16)),
    {error, {invalid_station_pubkey_length, _}} =
        macula_mri:new(station, Short, []).

rejects_station_with_invalid_z32() ->
    %% Realm field that doesn't decode as z32 at all.
    Bad = <<"NOTVALIDZ32!">>,
    {error, {invalid_station_pubkey_encoding, _}} =
        macula_mri:new(station, Bad, []).

rejects_station_with_uppercase_pubkey() ->
    %% z-base-32 alphabet is lowercase only; uppercase chars are
    %% outside the alphabet and decode/1 must reject. Catches a
    %% common accidental-mangling pattern (DNS label
    %% case-insensitivity could "helpfully" upcase a label).
    Pubkey = crypto:strong_rand_bytes(32),
    Z32 = macula_z32:encode(Pubkey),
    Upper = string:uppercase(Z32),
    {error, {invalid_station_pubkey_encoding, _}} =
        macula_mri:new(station, Upper, []).

%%===================================================================
%% Type registration sanity
%%===================================================================

station_is_builtin_type() ->
    %% station is in the registry's builtin list, so the
    %% generic validation paths see it as valid (even though
    %% station has its own validation route in macula_mri).
    ?assert(macula_mri_registry:is_valid_type(station)),
    ?assert(macula_mri_registry:is_valid_type(<<"station">>)).
