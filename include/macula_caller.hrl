%%%-------------------------------------------------------------------
%%% @doc
%%% Where the wire-authenticated caller of a served request lives.
%%%
%%% A process serving a request -- the handler process of a CALL or a
%%% served STREAM_OPEN, or a `macula_response' child answering one --
%%% holds the request's verified caller under this key, so `caller/0' in
%%% either module reads it whatever shape the payload has (macula#60).
%%%
%%% To use: -include("macula_caller.hrl").
%%% @end
%%%-------------------------------------------------------------------

%% The caller's key id, the identity `verify_request' authenticated for
%% the request this process serves; `undefined' outside a served
%% request. One definition for both modules, so the key cannot drift.
-define(CALLER_CONTEXT_KEY, '$macula_handler_caller').
