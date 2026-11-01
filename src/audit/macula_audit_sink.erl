%% @doc What a chain needs of whatever stores its entries (macula#175 WP1).
%%
%% A sink is a module and a state of the caller's choosing, held in the chain. Three calls are all the chain makes:
%% append an entry's bytes, sync what has been appended to the medium, and read back what is there. The sync is what
%% makes a checkpoint honest: `macula_audit_chain:checkpoint/3' asks for it first and signs nothing if it fails, so a
%% checkpoint can never cover an entry that a crash would lose.
%%
%% The station, the provider and the guard each keep their own durable store (WP2, WP3, WP5); this module is the shape
%% they answer, so the chain does not grow a second way to write bytes.
-module(macula_audit_sink).

-export_type([sink/0]).

%% The module and the state it carries, which the chain threads through every call.
-type sink() :: {module(), term()}.

-callback append(Bytes :: binary(), State :: term()) -> {ok, term()} | {error, term()}.
-callback sync(State :: term()) -> ok | {error, term()}.
-callback entries(State :: term()) -> {ok, [binary()]} | {error, term()}.
