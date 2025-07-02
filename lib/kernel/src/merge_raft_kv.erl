%%% % @format

-module(merge_raft_kv).
-compile(warn_missing_spec_all).
-moduledoc """
a simple key-value store with merge raft
""".

-behaviour(merge_raft).

%% OTP supervision
-export([
    child_spec/1,
    start_link/1,
    start/1
]).

%% API functions
-export([
    sync_put/3,
    sync_get/2,
    sync_delete/2,
    async_put/3,
    async_get/2,
    async_delete/2
]).

%% merge_raft callbacks
-export([
    db_init/2,
    apply_custom/3,
    apply_merge/4,
    apply_leave/3,
    apply_replace/2,
    serialize/1,
    reset/2
]).

-type server_name() :: merge_raft:server_name().
-type error() :: merge_raft:error().
-type peer() :: merge_raft:peer().
-type members() :: merge_raft:members().
-type commit_metadata() :: merge_raft:commit_metadata().

-type key() :: dynamic().
-type value() :: dynamic().
-type custom_db() :: ets:table().
-type custom_result() :: ok | value().
-type custom_log() :: {put, key(), value()} | {get, key()} | {delete, key()}.
-type custom_db_serialized() :: #{key() => value()}.

%==============================================================================
% OTP supervision
%==============================================================================

-spec child_spec(server_name()) -> supervisor:child_spec().
child_spec(Name) ->
    merge_raft:child_spec(#{name => Name, module => ?MODULE}).

-spec start_link(server_name()) -> gen_server:start_ret().
start_link(Name) ->
    merge_raft:start_link(#{name => Name, module => ?MODULE}).

-spec start(server_name()) -> gen_server:start_ret().
start(Name) ->
    merge_raft:start(#{name => Name, module => ?MODULE}).

%==============================================================================
% API functions
%==============================================================================

-spec sync_put(server_name(), key(), value()) -> {ok, ok} | error().
sync_put(Name, Key, Value) ->
    merge_raft:commit_sync(Name, {put, Key, Value}).

-spec sync_get(server_name(), key()) -> {ok, value()} | error().
sync_get(Name, Key) ->
    case merge_raft:commit_sync(Name, {get, Key}) of
        {ok, Result} ->
            Result;
        Error ->
            Error
    end.

-spec sync_delete(server_name(), key()) -> {ok, ok} | error().
sync_delete(Name, Key) ->
    merge_raft:commit_sync(Name, {delete, Key}).

-spec async_put(server_name(), key(), value()) -> ok.
async_put(Name, Key, Value) ->
    merge_raft:commit_async(Name, {put, Key, Value}).

-spec async_get(server_name(), key()) -> {ok, value()} | error().
async_get(Name, Key) ->
    case ets:lookup(Name, Key) of
        [] ->
            {error, not_found};
        [{Key, Value}] ->
            {ok, Value}
    end.

-spec async_delete(server_name(), key()) -> ok.
async_delete(Name, Key) ->
    merge_raft:commit_async(Name, {delete, Key}).

%==============================================================================
% merge_raft callbacks
%==============================================================================

-spec db_init(peer(), server_name()) -> {undefined | commit_metadata(), custom_db()}.
db_init(_Me, Name) ->
    % Future: write to disk and restore from disk
    {undefined, ets:new(Name, [set, protected, named_table])}.

-spec apply_custom(commit_metadata(), custom_log(), custom_db()) -> {custom_result(), custom_db()}.
apply_custom(_CommitMetadata, {put, Key, Value}, Db) ->
    ets:insert(Db, {Key, Value}),
    {ok, Db};
apply_custom(_CommitMetadata, {get, Key}, Db) ->
    case ets:lookup(Db, Key) of
        [] ->
            {{error, not_found}, Db};
        [{Key, Value}] ->
            {{ok, Value}, Db}
    end;
apply_custom(_CommitMetadata, {delete, Key}, Db) ->
    ets:delete(Db, Key),
    {ok, Db}.

-spec apply_merge(commit_metadata(), members(), custom_db_serialized(), custom_db()) -> custom_db().
apply_merge(_CommitMetadata, _Members, Data, Db) ->
    ets:insert(Db, maps:to_list(Data)),
    Db.

-spec apply_leave(commit_metadata(), peer(), custom_db()) -> custom_db().
apply_leave(_CommitMetadata, _Peer, Db) ->
    Db.

-spec apply_replace(custom_db_serialized(), custom_db()) -> custom_db().
apply_replace(Data, Db) ->
    % can do incremental update to have less churn
    ets:delete_all_objects(Db),
    ets:insert(Db, maps:to_list(Data)),
    Db.

-spec serialize(custom_db()) -> custom_db_serialized().
serialize(Db) ->
    maps:from_list(ets:tab2list(Db)).

-spec reset(peer(), custom_db()) -> custom_db().
reset(_Me, Db) ->
    ets:delete_all_objects(Db),
    Db.
