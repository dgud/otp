%%% % @format

-module(merge_raft_reg).
-compile(warn_missing_spec_all).
-moduledoc """
a simple gen_server implementing pid register using merge raft
""".

-behaviour(merge_raft).

%% OTP supervision
-export([
    child_spec/0,
    start_link/0,
    start/0
]).

%% API functions
-export([
    reg/2,
    get/1,
    resolve/3
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

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-type server_name() :: merge_raft:server_name().
-type peer() :: merge_raft:peer().
-type members() :: merge_raft:members().
-type commit_metadata() :: merge_raft:commit_metadata().

-type name() :: atom().

-type custom_db() :: ets:table().
-type custom_result() :: ok | boolean().
-type custom_log() :: {reg, name(), pid()} | {unreg, name(), pid()}.
-type custom_db_serialized() :: #{}.

-type state() :: #{name() => pid()}.

-define(RAFT_SERVER, merge_raft_reg_server).

%==============================================================================
% OTP supervision
%==============================================================================

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id => ?MODULE,
        start => {?MODULE, start_link, []},
        restart => transient,
        shutdown => 1000,
        modules => [?MODULE]
    }.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start() -> gen_server:start_ret().
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%==============================================================================
% API functions
%==============================================================================

-spec reg(name(), pid()) -> boolean().
reg(Name, Pid) ->
    gen_server:call(?MODULE, {reg, Name, Pid}).

-spec get(name()) -> pid() | undefined.
get(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{_Name, Pid}] ->
            Pid;
        _ ->
            undefined
    end.

%==============================================================================
% merge_raft callbacks
%==============================================================================

-spec db_init(peer(), server_name()) -> {undefined | commit_metadata(), custom_db()}.
db_init(_Me, _Name) ->
    {undefined, ets:new(?MODULE, [set, protected, named_table])}.

-spec apply_custom(commit_metadata(), custom_log(), custom_db()) -> {custom_result(), custom_db()}.
apply_custom(_CommitMetadata, {reg, Name, Pid}, Db) ->
    case ets:lookup(Db, Name) of
        [{_Name, Pid1}] when Pid1 =:= Pid ->
            ok;
        [{_Name, Pid1}] ->
            ets:insert(Db, {Name, resolve(Name, Pid1, Pid)});
        _ ->
            ets:insert(Db, {Name, Pid})
    end,
    {ets:lookup_element(Db, Name, 2) =:= Pid, Db};
apply_custom(_CommitMetadata, {unreg, Name, Pid}, Db) ->
    case ets:lookup(Db, Name) of
        [{_Name, Pid}] ->
            ets:delete(Db, Name);
        _ ->
            ok
    end,
    {ok, Db}.

-spec apply_merge(commit_metadata(), members(), custom_db_serialized(), custom_db()) -> custom_db().
apply_merge(_CommitMetadata, _Members, Data, Db) ->
    [
        case ets:lookup(Db, Name) of
            [{_Name, Pid1}] when Pid1 =:= Pid ->
                ok;
            [{_Name, Pid1}] ->
                ets:insert(Db, {Name, resolve(Name, Pid1, Pid)});
            _ ->
                ets:insert(Db, {Name, Pid})
        end
     || Name := Pid <- Data
    ],
    Db.

-spec apply_leave(commit_metadata(), peer(), custom_db()) -> custom_db().
apply_leave(_CommitMetadata, _Peer, Db) ->
    % remove names related to the peer
    % however we should not remove the names new generation of peer is in members
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
    [
        ets:delete(Db, Name)
     || {Name, Pid} <- ets:tab2list(Db),
        node(Pid) =:= node(),
        is_process_alive(Pid)
    ],
    Db.

%==============================================================================
% gen_server callbacks
%==============================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    merge_raft:start_link(#{name => ?RAFT_SERVER, module => ?MODULE}),
    {ok, #{}}.

-spec handle_call(dynamic(), gen_server:from(), state()) -> {reply, boolean(), state()}.
handle_call({reg, Name, Pid}, _From, State) ->
    case merge_raft:commit_sync(?RAFT_SERVER, {reg, Name, Pid}) of
        {ok, true} ->
            monitor(process, Pid, [{tag, Name}]),
            {reply, true, State};
        _ ->
            {reply, false, State}
    end.

-spec handle_cast(dynamic(), state()) -> no_return().
handle_cast(_Request, _State) ->
    error(badarg).

-spec handle_info({name(), reference(), process, pid(), term()}, state()) -> {noreply, state()}.
handle_info({Name, _Mon, process, Pid, _Reason}, State) ->
    % should keep retry when failed
    % code example only, don't hanlde failures
    {ok, ok} = merge_raft:commit_sync(?RAFT_SERVER, {unreg, Name, Pid}),
    {noreply, State}.

%==============================================================================
% internal functions
%==============================================================================

-spec resolve(name(), pid(), pid()) -> pid().
resolve(_Name, Pid1, Pid2) when Pid1 < Pid2 ->
    exit(Pid2, conflict),
    Pid1;
resolve(_Name, Pid1, Pid2) ->
    exit(Pid1, conflict),
    Pid2.
