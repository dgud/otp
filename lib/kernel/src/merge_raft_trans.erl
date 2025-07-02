%%% % @format

-module(merge_raft_trans).
-compile(warn_missing_spec_all).
-moduledoc """
a simple transaction implementation with merge raft
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
    set_lock/3,
    del_lock/3
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

-type id() :: dynamic().
-type custom_db() :: ets:table().
-type custom_result() :: ok | error().
-type custom_log() :: {lock, id(), pid(), [node()]} | {unlock, id(), pid(), [node()]}.
-type custom_db_serialized() :: #{
    members => #{node() => peer()},
    {lock, id(), node()} => pid()
}.

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

-spec set_lock(server_name(), id(), [node()]) -> ok | error().
set_lock(Name, Id, Nodes) ->
    [net_adm:ping(Node) || Node <- Nodes -- nodes()],
    case merge_raft:commit_sync(Name, {lock, Id, self(), Nodes}) of
        {ok, Result} ->
            Result;
        Error ->
            Error
    end.

-spec del_lock(server_name(), id(), [node()]) -> ok | error().
del_lock(Name, Id, Nodes) ->
    case merge_raft:commit_sync(Name, {unlock, Id, self(), Nodes}) of
        {ok, Result} ->
            Result;
        Error ->
            Error
    end.

% For transaction, we can actually run it on leader node, but it needs async apply to be implemented in merge_raft
% -spec transaction(server_name(), id(), fun(() -> result()), [node()]) -> ok | error().

%==============================================================================
% merge_raft callbacks
%==============================================================================

-spec db_init(peer(), server_name()) -> {undefined | commit_metadata(), custom_db()}.
db_init(Me, Name) ->
    Table = ets:new(Name, [set, protected, named_table]),
    ets:insert(Table, {members, #{Me => []}}),
    {undefined, Table}.

-spec apply_custom(commit_metadata(), custom_log(), custom_db()) -> {custom_result(), custom_db()}.
apply_custom(_CommitMetadata, {lock, Id, Pid, Nodes}, Db) ->
    Members = ets:lookup_element(Db, members, 2),
    ExistLocks = [
        found
     || Node <- Nodes,
        {_, P} <- ets:lookup(Db, {lock, Id, maps:get(Node, Members, [])}),
        P =/= Pid
    ],
    case Nodes -- maps:keys(Members) =:= [] andalso ExistLocks =:= [] of
        true ->
            ets:insert(Db, [{{lock, Id, Node}, Pid} || Node <- Nodes]),
            {ok, Db};
        _ ->
            {{error, not_locked}, Db}
    end;
apply_custom(_CommitMetadata, {unlock, Id, Pid, Nodes}, Db) ->
    Members = ets:lookup_element(Db, members, 2),
    case Nodes -- maps:keys(Members) =:= [] of
        true ->
            [
                case ets:lookup(Db, {lock, Id, Node}) of
                    [{_, P}] when P =:= Pid ->
                        ets:delete(Db, {lock, Id, Node});
                    _ ->
                        ok
                end
             || Node <- Nodes
            ],
            {ok, Db};
        _ ->
            {{error, not_enough_nodes}, Db}
    end.

-spec apply_merge(commit_metadata(), members(), custom_db_serialized(), custom_db()) -> custom_db().
apply_merge(_CommitMetadata, Members, Data, Db) ->
    OldMembers = ets:lookup_element(Db, members, 2),
    NewMembers =
        maps:fold(
            fun({_, Pid} = Peer, _, Acc) ->
                case Acc of
                    #{node(Pid) := OldPeer} when OldPeer > Peer ->
                        Acc;
                    #{node(Pid) := OldPeer} when OldPeer < Peer ->
                        % Can add kill pid if lock is lost
                        % Performance can be improved by storing reverse map
                        ets:match_delete(Db, {{'_', '_', node(Pid)}, '_'}),
                        Acc#{node(Pid) => Peer};
                    _ ->
                        Acc#{node(Pid) => Peer}
                end
            end,
            OldMembers,
            Members
        ),
    NewData =
        [
            {{lock, Id, Node}, Pid}
         || {lock, Id, Node} := Pid <- Data,
            is_map_key(map_get(Node, NewMembers), Members)
        ],
    ets:insert(Db, [{members, NewMembers} | NewData]),
    Db.

-spec apply_leave(commit_metadata(), peer(), custom_db()) -> custom_db().
apply_leave(_CommitMetadata, {_, Pid} = Peer, Db) ->
    case ets:lookup_element(Db, members, 2) of
        #{node(Pid) := Peer} = Members ->
            % Can add kill pid if lock is lost
            % Performance can be improved by storing reverse map
            ets:match_delete(Db, {{'_', '_', node(Pid)}, '_'}),
            ets:insert(Db, {members, maps:remove(node(Pid), Members)});
        _ ->
            ok
    end,
    Db.

-spec apply_replace(custom_db_serialized(), custom_db()) -> custom_db().
apply_replace(Data, Db) ->
    % Can do incremental update to have less churn
    % Can add kill pid if lock is lost
    ets:delete_all_objects(Db),
    ets:insert(Db, maps:to_list(Data)),
    Db.

-spec serialize(custom_db()) -> custom_db_serialized().
serialize(Db) ->
    maps:from_list(ets:tab2list(Db)).

-spec reset(peer(), custom_db()) -> custom_db().
reset(_Me, Db) ->
    % Can add kill pid if lock is lost
    ets:delete_all_objects(Db),
    Db.
