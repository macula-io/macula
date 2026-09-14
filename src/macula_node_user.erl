%%%-------------------------------------------------------------------
%%% @doc The user a node runs as.
%%%
%%% A secret macula reads from disk has to belong to this user, so that
%%% another user of the host cannot give the node a key file of their own. On
%%% a host with user ids it is the process's effective user id, the owner of
%%% every file the node creates, read with the crypto NIF. When that NIF did
%%% not load, the call raises nif_not_loaded instead of skipping the check. On
%%% a host without user ids it is none, and owner checks are skipped.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_node_user).

-export([effective_uid/0]).

%% @doc The effective user id the node runs as, or none on a host without
%% user ids.
-spec effective_uid() -> non_neg_integer() | none.
effective_uid() ->
    effective_uid(os:type()).

effective_uid({unix, _}) -> macula_crypto_nif:nif_effective_uid();
effective_uid({win32, _}) -> none.
