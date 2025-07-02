%%% % @format

-module(merge_raft_nodes).
-compile(warn_missing_spec_all).
-moduledoc """
a simple gen_server implementing connect_all feature using merge raft
""".

-behaviour(merge_raft).

%% OTP supervision
-export([
    child_spec/0,
    start_link/0,
    start/0
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

-type custom_db() :: #{node() => []}.
-type custom_result() :: ok.
-type custom_log() :: undefined.
-type custom_db_serialized() :: #{node() => []}.

-type state() :: #{node() => up | reference()}.

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
% merge_raft callbacks
%==============================================================================

-spec db_init(peer(), server_name()) -> {undefined | commit_metadata(), custom_db()}.
db_init({_, Pid} = _Me, _Name) ->
    {undefined, #{node(Pid) => []}}.

-spec apply_custom(commit_metadata(), custom_log(), custom_db()) -> {custom_result(), custom_db()}.
apply_custom(_CommitMetadata, _CustomLog, Db) ->
    {ok, Db}.

-spec apply_merge(commit_metadata(), members(), custom_db_serialized(), custom_db()) -> custom_db().
apply_merge(_CommitMetadata, Members, _Data, Db) ->
    ?MODULE ! [node(Pid) || {_, Pid} := _ <- Members],
    maps:merge(Db, #{node(Pid) => [] || {_, Pid} := _ <- Members}).

-spec apply_leave(commit_metadata(), peer(), custom_db()) -> custom_db().
apply_leave(_CommitMetadata, _Peer, Db) ->
    Db.

-spec apply_replace(custom_db_serialized(), custom_db()) -> custom_db().
apply_replace(Data, _Db) ->
    ?MODULE ! maps:keys(Data),
    Data.

-spec serialize(custom_db()) -> custom_db_serialized().
serialize(Db) ->
    Db.

-spec reset(peer(), custom_db()) -> custom_db().
reset({_, Pid} = _Me, _Db) ->
    #{node(Pid) => []}.

%==============================================================================
% gen_server callbacks
%==============================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    net_kernel:monitor_nodes(true),
    process_flag(async_dist, true),
    merge_raft:start_link(#{name => merge_raft_nodes_server, module => ?MODULE}),
    {ok, #{Node => up || Node <- nodes()}}.

-spec handle_call(dynamic(), gen_server:from(), state()) -> no_return().
handle_call(_Request, _From, _State) ->
    error(badarg).

-spec handle_cast(dynamic(), state()) -> no_return().
handle_cast(_Request, _State) ->
    error(badarg).

-spec handle_info({nodeup, node()} | {nodedown, node()} | {connect, node(), pos_integer()} | [node()], state()) ->
    {noreply, state()}.
handle_info({nodeup, Node}, State) ->
    case State of
        #{Node := Timer} when is_reference(Timer) ->
            erlang:cancel_timer(Timer),
            receive
                {connect, Node, _} ->
                    ok
            after 0 ->
                ok
            end;
        _ ->
            ok
    end,
    {noreply, State#{Node => up}};
handle_info({nodedown, Node}, State) ->
    case State of
        #{Node := Timer} when is_reference(Timer) ->
            {noreply, State};
        _ ->
            {noreply, State#{Node => erlang:send_after(0, self(), {connect, Node, 1})}}
    end;
handle_info({connect, Node, N}, State) ->
    case State of
        #{Node := up} ->
            {noreply, State};
        _ ->
            % Send a message to async connect to the node
            {undefined, Node} ! connect,
            {noreply, State#{Node => erlang:send_after((1 bsl N) * 1000, self(), {connect, Node, min(N + 1, 10)})}}
    end;
handle_info(Nodes, State) ->
    State1 = lists:foldl(
        fun(Node, Acc) ->
            case Acc of
                #{Node := _} ->
                    Acc;
                _ ->
                    Acc#{Node => erlang:send_after(0, self(), {connect, Node, 1})}
            end
        end,
        State,
        Nodes
    ),
    {noreply, State1}.

%==============================================================================
% internal functions
%==============================================================================
