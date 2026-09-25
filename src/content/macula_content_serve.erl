%% @doc How a sharing node answers a fetch on its content procedure (D27). One content id per stream, a
%% `server_stream': the stream's args name the content id (`mcid') and what is wanted (`want'), `root' for the content
%% id a fetcher was given or `block' for a chunk of a manifest it holds. The answer is one DATA body, then the end of
%% the stream:
%%
%% <ul>
%%   <li>`#{kind => block, mcid => MCID, bytes => Bytes}' for a raw root or a chunk;</li>
%%   <li>`#{kind => manifest, mcid => MCID, manifest => Manifest}' for a manifest root.</li>
%% </ul>
%%
%% Content this node does not hold is refused with the stream error `not_shared', and args that name no content id,
%% or want anything else, with `malformed'. The args are read as a station link delivers them, keys and text values
%% tagged or not, as `macula_record:payload_field/2' reads any wire payload. The fetcher verifies every body against
%% the content id it asked for; nothing here is trusted by it.
-module(macula_content_serve).

-export([serve/3, lookup/3]).

-type want() :: root | block.
-type body() :: #{kind := block | manifest, mcid := macula:mcid(), bytes => binary(),
                  manifest => macula_manifest:manifest()}.
-type lookup() :: fun((want(), macula:mcid()) -> {ok, body()} | not_found).

-export_type([want/0, body/0, lookup/0]).

%% @doc Answer one fetch on `Stream', looking the content up with `Lookup'.
-spec serve(pid(), term(), lookup()) -> ok.
serve(Stream, Args, Lookup) ->
    answered(requested(Args), Stream, Lookup).

%% @doc What a store answers for `Want' of `MCID': the DATA body, or `not_found'.
-spec lookup(want(), macula:mcid(), macula_content_store:store()) -> {ok, body()} | not_found.
lookup(root, MCID, Store) ->
    root_body(macula_content_store:root(MCID, Store), MCID);
lookup(block, MCID, Store) ->
    block_body(macula_content_store:chunk(MCID, Store), MCID).

root_body({block, Bytes}, MCID) -> {ok, #{kind => block, mcid => MCID, bytes => Bytes}};
root_body({manifest, Manifest}, MCID) -> {ok, #{kind => manifest, mcid => MCID, manifest => Manifest}};
root_body(not_found, _MCID) -> not_found.

block_body({ok, Bytes}, MCID) -> {ok, #{kind => block, mcid => MCID, bytes => Bytes}};
block_body(not_found, _MCID) -> not_found.

answered({ok, MCID, Want}, Stream, Lookup) ->
    sent(Lookup(Want, MCID), Stream);
answered(malformed, Stream, _Lookup) ->
    refused(Stream, <<"malformed">>, <<"a fetch names one content id and wants root or block">>).

sent({ok, Body}, Stream) ->
    closed_after(macula:send(Stream, Body, msgpack), Stream);
sent(not_found, Stream) ->
    refused(Stream, <<"not_shared">>, <<"this node does not share that content">>).

%% A send refused because the fetcher has gone ends quietly: a fetch drops the chunk streams it no longer needs. Any
%% other refusal ends the stream with the reason, so a fetcher still waiting is answered at once.
closed_after(ok, Stream) ->
    macula:close_stream(Stream);
closed_after({error, Reason}, Stream) ->
    unsent(fetcher_gone(Reason), Reason, Stream).

unsent(true, _Reason, _Stream) ->
    ok;
unsent(false, Reason, Stream) ->
    logger:warning("[macula_content_serve] answer not sent: ~p", [Reason]),
    refused(Stream, <<"not_sent">>, <<"the answer could not be sent">>).

fetcher_gone(no_peer) -> true;
fetcher_gone(send_closed) -> true;
fetcher_gone(_Other) -> false.

refused(Stream, Code, Message) ->
    _ = macula_stream:abort(Stream, Code, Message),
    ok.

requested(Args) when is_map(Args) ->
    request_read(macula_record:payload_field(Args, <<"mcid">>), want(macula_record:payload_field(Args, <<"want">>)));
requested(_NotAMap) ->
    malformed.

request_read(<<2, _Codec:8, _Hash:48/binary>> = MCID, Want) when Want =/= undefined -> {ok, MCID, Want};
request_read(_MCID, _Want) -> malformed.

want(root) -> root;
want(block) -> block;
want(<<"root">>) -> root;
want(<<"block">>) -> block;
want(_Other) -> undefined.
