%%% @doc The forms a node key's private halves can leak in, for tests that look for them in what a node logs or shows.
%%%
%%% Each private half is sampled at two secret slices, bytes its public key does not hold. For an ML-DSA-87 seed those
%%% are its first eight bytes, inside the window a depth-limited print shows, and its last 24. For an ML-DSA-87 key in
%%% the expanded form they are the eight bytes at 32, inside K and inside that window, and its last 24 bytes. For RSA they
%%% are eight bytes from the middle of the private exponent and the last 24 bytes of its DER. Each slice comes raw, as
%%% lowercase and uppercase hex, as the comma-led byte list a formatter prints, and as Base64 of six bytes at each of
%%% three alignments, standard and URL-safe. An RSA half also comes as the first 32 digits of its private exponent in
%%% decimal, as a printed decoded RSAPrivateKey shows it.
-module(macula_key_leak_sample).

-include_lib("public_key/include/public_key.hrl").

-export([forms/1, found/2]).

%% Where the slice of an expanded ML-DSA-87 key starts (a seed's starts at 0), and how long each inner slice is.
-define(SECRET_AT, 32).
-define(SECRET_BYTES, 8).
%% How many of a private half's last bytes the tail slice takes.
-define(TAIL_BYTES, 24).
%% How many bytes of a slice each Base64 form encodes.
-define(BASE64_BYTES, 6).
%% How many leading digits of an RSA private exponent the decimal form takes.
-define(EXPONENT_DIGITS, 32).

%% @doc Every form the private halves of `Key' can leak in.
-spec forms(macula_node_keys:node_key()) -> [binary()].
forms(#{components := Components}) ->
    lists:append([component_forms(Component) || Component <- Components]).

%% @doc The forms of `Key''s private halves that any of `Haystacks' holds, such as an event's external term format and
%% its formatted text.
-spec found([binary()], macula_node_keys:node_key()) -> [binary()].
found(Haystacks, Key) ->
    [Form || Form <- forms(Key), Haystack <- Haystacks, binary:match(Haystack, Form) =/= nomatch].

%% A slice the half's own public key holds would find public bytes, so none may.
component_forms(#{public := Public} = Component) ->
    Slices = secret_slices(Component),
    [] = [Slice || Slice <- Slices, binary:match(Public, Slice) =/= nomatch],
    lists:append([slice_forms(Slice) || Slice <- Slices]) ++ decimal_forms(Component).

secret_slices(#{algorithm := mldsa87, private := <<_:32/binary>> = Seed}) ->
    [binary:part(Seed, 0, ?SECRET_BYTES), tail(Seed)];
secret_slices(#{algorithm := mldsa87, private := Private}) ->
    [binary:part(Private, ?SECRET_AT, ?SECRET_BYTES), tail(Private)];
secret_slices(#{algorithm := rsa_pss, private := Der}) ->
    Exponent = binary:encode_unsigned(private_exponent(Der)),
    [binary:part(Exponent, byte_size(Exponent) div 2, ?SECRET_BYTES), tail(Der)].

tail(Bytes) ->
    binary:part(Bytes, byte_size(Bytes) - ?TAIL_BYTES, ?TAIL_BYTES).

slice_forms(Slice) ->
    [Slice, binary:encode_hex(Slice, lowercase), binary:encode_hex(Slice, uppercase), byte_list(Slice)
     | [Encode(binary:part(Slice, Offset, ?BASE64_BYTES))
        || Offset <- [0, 1, 2], Encode <- [fun base64:encode/1, fun url_base64/1]]].

decimal_forms(#{algorithm := rsa_pss, private := Der}) ->
    [binary:part(integer_to_binary(private_exponent(Der)), 0, ?EXPONENT_DIGITS)];
decimal_forms(_Component) ->
    [].

private_exponent(Der) ->
    #'RSAPrivateKey'{privateExponent = Exponent} = public_key:der_decode('RSAPrivateKey', Der),
    Exponent.

%% The bytes as a formatter prints them inside a longer list, each after a comma.
byte_list(Bytes) ->
    iolist_to_binary([[$,, integer_to_list(Byte)] || <<Byte>> <= Bytes]).

url_base64(Bytes) ->
    base64:encode(Bytes, #{mode => urlsafe, padding => false}).
