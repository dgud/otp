%%% % @format

-module(merge_raft).
-compile(warn_missing_spec_all).
-moduledoc """
merge_raft behaviour
""".

-behaviour(gen_statem).

%% OTP supervision
-export([
    child_spec/1,
    start_link/1,
    start/1
]).

%% API functions
-export([
         commit_async/2,
         commit_sync/2,
         commit_sync/3,

         local_lookup/2,
         connect/2
        ]).

%% Debug functions
-export([get_info/1]).

%% gen_statem callbacks
-export(
   [init/1,
    callback_mode/0,
    follower/3,
    follower_wait/3,
    candidate/3,
    leader/3,
    terminate/2]).

-include_lib("kernel/include/logger.hrl").


-define(HEARTBEAT_TIMEOUT_MS, (900 + rand:uniform(200))).
-define(ELECTION_TIMEOUT_MS, (200 + rand:uniform(500))).
-define(LIVENESS_TIMEOUT_MS, (1 * 60 * 1000 + rand:uniform(5000))).
-define(RESET_TIMEOUT_MS, (5 * 60 * 1000)).
-define(TICK_MESSAGE, tick).
-define(TICK_TIMEOUT_MS, 100).
-define(MERGE_TIMEOUT_MS, (1 * 1000)).
-define(BATCH_SIZE, 100).
-define(REDIRECT, 1).

%% Send to state.peers
%% Vote if sender tenure is higher or equal and
%% receiver is not voted, even if sender is not in state.peers
-record(vote_request, {
    from :: peer(),
    to :: peer(),
    branch :: branch(),
    tenure_id :: tenure_id(),
    last_log_index :: log_id(),
    last_log_tenure :: tenure_id()
}).

% Store if voted in state.peers
% Count if in member list of most recent committed Log
-record(vote_reply, {
    from :: peer(),
    to :: peer(),
    branch :: branch(),
    tenure_id :: tenure_id(),
    succeeded :: boolean()
}).

% Only send to committed peers
% Accept if sender is leader
% Sender becomes leader if tenure is higher or equal, even if sender is not in state.peers
% Not update state.peers until receive membership updates
-record(append_request, {
    from :: peer(),
    to :: peer(),
    branch :: branch(),
    tenure_id :: tenure_id(),
    prev_log_index :: log_id(),
    prev_log_tenure :: tenure_id(),
    entries :: [log_entry()],
    leader_commit_index :: log_id(),
    leader_cleanup_index :: log_id()
}).

% Store if in state.peers
-record(append_reply, {
    from :: peer(),
    to :: peer(),
    branch :: branch(),
    tenure_id :: tenure_id(),
    result :: success | not_leader | need_older,
    append_id :: log_id()
}).

% Send to peer with largest match index (including not committed?)
% Initialize election if think sender is leader
-record(transfer_leader_request, {
    from :: peer(),
    to :: peer(),
    branch :: branch(),
    tenure_id :: tenure_id()
}).

-record(log_request,
        {
         redirect = ?REDIRECT :: non_neg_integer(),
         reply_to = undefined :: undefined | log_ref(),
         log :: log_message()
        }).

-record(local_lookup,
        {
         req :: dynamic()
        }).

-record(connect,
        {
         servers :: [node() | pid()]
        }).

-record(discover, {
    redirect = ?REDIRECT :: non_neg_integer(),
    from :: peer(),
    branch :: branch(),
    members :: members()
}).

-record(peer_state, {
    base_index = 0 :: log_id(),
    match_index = 0 :: log_id(),
    heartbeat_timeout_ms = 0 :: non_neg_integer(),
    liveness_timeout_ms = 0 :: non_neg_integer(),
    voted = false :: boolean()
}).

-type member_tree() :: gb_trees:tree(log_id(), members()).
-type peers() :: #{peer() => #peer_state{}}.

-record(sdata, {  %% State data
    name :: server_name(),
    module :: server_module(),
    options :: options(),
    me :: peer(),
    role :: role(),
    branch :: branch(),
    tenure_id :: tenure_id(),
    leader :: peer() | undefined,
    voted_for :: peer() | undefined,
    wait_snapshot :: boolean(),
    logs :: logs(),
    append_index :: log_id(),
    commit_index :: log_id(),
    apply_index :: log_id(),
    cleanup_index :: log_id(),
    member_tree :: member_tree(),
    paused :: paused(),
    % holds all the known peers in branch tree until the peer is committed to leave
    peers :: peers(),
    election_timeout_ms :: infinity | time_ms(),
    merge_timeout_ms :: time_ms(),
    reset_timeout_ms :: time_ms(),
    custom_db :: custom_db(),
    replies :: #{log_ref() => []}
}).

-type custom_log() :: dynamic().
-type custom_db() :: dynamic().
-type custom_db_serialized() :: dynamic().
-type custom_result() :: dynamic().

-type server_name() :: atom().
-type server_module() :: module().
-type options() ::
        #{
          module := server_module(),
          name => server_name(),
          members => [pid() | node()],
          extra_logs => non_neg_integer()
         }.
-type error() :: {error, dynamic()}.

-type time_ms() :: non_neg_integer().
-type time_ns() :: non_neg_integer().
-type peer() :: {time_ns(), pid()}.
-type members() :: #{peer() => []}.
-type paused() :: #{peer() => [], discover => #discover{}}.
% Future: add read_only
-type role() :: follower | follower_wait | candidate | leader.
-type branch() :: peer().
-type tenure_id() :: non_neg_integer().
-type log_id() :: non_neg_integer().
-type log_ref() :: {cast | internal, reference()} | {call, gen_statem:from()}.
-type log_message() ::
    {custom, custom_log()}
    | {merge, members(), custom_db_serialized()}
    | {leave, peer()}
    | {leader, peer()}
    | {pause, #discover{}}.
-type log() :: log_message() | {snapshot, members(), paused(), custom_db_serialized()}.
-type log_value() :: {tenure_id(), log_ref(), log()}.
-type log_entry() :: {log_id(), log_value()}.
-type logs() :: #{log_id() => log_value()}.

-type commit_metadata() :: {branch(), log_id(), tenure_id(), log_ref()}.

-export_type([
    % custom data structures
    custom_log/0,
    custom_db/0,
    custom_db_serialized/0,
    custom_result/0,
    % server API types
    server_name/0,
    server_module/0,
    options/0,
    error/0,
    % commit metadata types
    peer/0,
    members/0,
    branch/0,
    log_id/0,
    tenure_id/0,
    log_ref/0,
    commit_metadata/0
]).

%% Initialize an empty db or load from backup
%%
%% If the callback returns a commit_metadata(),
%% the server will pause itself until joined the target branch,
%% with assuming all data up to index is known
%% To be implemented
-callback db_init(peer(), server_name()) -> {undefined | commit_metadata(), custom_db()}.
%% custom_db is expected being able to be serialized and applied to
%% another custom_db as a result of merge To be implemented in async
%% way so that large amount of data can be transferred and applied
-callback apply_custom(commit_metadata(), custom_log(), custom_db()) ->
    {custom_result(), custom_db()}.
%% In very rare (unlikely ever happen) racing case the same
%% custom_db_serialized can be applied twice to the custom_db It is a
%% design choice to avoid aggregating unlimited data in RAFT It is up
%% for user to decide how to handle that, e.g.  1. Mark owner for every
%% data and cleanup related data if a member leaves 2. Store all past
%% merge histories (unlimited data usage but should be relatively small
%% amount of data) 3. Simply allow double merge
-callback apply_merge(commit_metadata(), members(), custom_db_serialized(), custom_db()) ->
    custom_db().
-callback apply_leave(commit_metadata(), peer(), custom_db()) -> custom_db().
-callback apply_replace(custom_db_serialized(), custom_db()) -> custom_db().
-callback serialize(custom_db()) -> custom_db_serialized().
-callback reset(peer(), custom_db()) -> custom_db().

%==============================================================================
% OTP supervision
%==============================================================================

-spec child_spec(options()) -> supervisor:child_spec().
child_spec(Option) ->
    #{
      id => ?MODULE,
      start => {?MODULE, start_link, [Option]},
      restart => transient,
      shutdown => 1000,
      modules => [?MODULE]
    }.

-spec start_link(options()) -> gen_server:start_ret().
start_link(#{name := Name, module := Module} = Option) when is_atom(Name), is_atom(Module) ->
    gen_statem:start_link({local, Name}, ?MODULE, Option, []);
start_link(#{module := Module} = Option) when is_atom(Module) ->
    gen_statem:start_link(?MODULE, Option, []).

-spec start(options()) -> gen_server:start_ret().
start(#{name := Name, module := Module} = Option) when is_atom(Name), is_atom(Module) ->
    gen_statem:start({local, Name}, ?MODULE, Option, []);
start(#{module := Module} = Option) when is_atom(Module) ->
    gen_statem:start(?MODULE, Option, []).

%==============================================================================
% API functions
%==============================================================================

-spec commit_async(server_name() | pid(), custom_log()) -> ok.
commit_async(Name, Log) ->
    gen_statem:cast(Name, #log_request{log = {custom, Log}}).

-spec commit_sync(server_name() | pid(), custom_log()) -> {ok, custom_result()} | error().
commit_sync(Name, Log) ->
    commit_sync(Name, Log, 5000).

-spec commit_sync(server_name() | pid(), custom_log(), timeout()) ->
          {ok, custom_result()} | error().
commit_sync(Name, Log, Timeout) ->
    gen_statem:call(Name, #log_request{log = {custom, Log}}, Timeout).

-spec local_lookup(server_name() | pid(), custom_log()) -> {ok, custom_result()} | error().
local_lookup(Name, Log) ->
    gen_statem:call(Name, #local_lookup{req = Log}).

-spec connect(server_name() | pid(), [server_name() | pid()]) -> [pid()].
connect(Name, Servers) when (is_pid(Name) orelse is_atom(Name)) andalso is_list(Servers) ->
    gen_statem:call(Name, #connect{servers = Servers}).


-spec get_info(server_name() | pid()) -> map().
get_info(Name) ->
    gen_statem:call(Name, get_info).


%%==============================================================================
%% gen_statem callbacks
%%==============================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> [state_functions].  %% state_enter ??

%% States

-spec init(options()) -> {ok, role(), #sdata{}}.
init(#{module := Module} = Options) ->
    net_kernel:monitor_nodes(true),
    process_flag(async_dist, true),
    process_flag(trap_exit, true),
    Me = {now_ns(), self()},
    Name = maps:get(name, Options, undefined),
    %% Future: implement read from backup
    {undefined, CustomDb} = Module:db_init(Me, Name),

    Imembers = initial_members(Name, Options),
    Role = case Imembers of
               [] -> leader;
               [_|_] -> follower
           end,

    State = #sdata{
               name = Name,
               module = Module,
               options = Options,
               me = Me,
               role = Role,
               branch = Me,
               tenure_id = 1,
               leader = Me,
               voted_for = Me,
               wait_snapshot = false,
               logs = #{1 => {1, {internal, make_ref()}, {leader, Me}}},
               append_index = 1,
               commit_index = 1,
               apply_index = 1,
               cleanup_index = 1,
               member_tree = gb_trees:from_orddict([{1, #{Me => []}}]),
               paused = #{},
               peers = #{},
               election_timeout_ms = infinity,
               merge_timeout_ms = 0,
               reset_timeout_ms = now_ms() + ?RESET_TIMEOUT_MS,
               custom_db = CustomDb,
               replies = #{}
              },
    State1 = discover(Imembers, State),

    erlang:send_after(?TICK_TIMEOUT_MS, self(), ?TICK_MESSAGE),
    {ok, Role, State1}.

-spec follower(Type, Request, #sdata{}) -> Result when
      Type :: cast | {call, gen_statem:from()} | info,
      Request :: #vote_request{}
               | #append_request{}
               | #transfer_leader_request{}
               | #log_request{}
               | #local_lookup{}
               | #discover{}
               | #connect{}
               | ?TICK_MESSAGE
               | {'EXIT', pid(), dynamic()}
               | {nodeup | nodedown, node()},
      Result :: gen_statem:event_handler_result(role(), #sdata{}).

follower(cast, #transfer_leader_request{to = Me} = TLR, #sdata{me = Me} = SData0) ->
    {NextState, SData} = handle_transfer_leader_request(TLR, SData0),
    {next_state, NextState, SData};
follower(info, {'EXIT', LPid, _Reason}, #sdata{me = Me, leader = Leader} = SData)
  when LPid =:= element(2, Leader) ->
    [Oldest|_] = lists:sort(lists:delete(Leader,maps:keys(appended_members(SData)))),
    %% FIXME cleanup remove leader from peers/members
    case Me =:= Oldest of
        true -> %% I'm the oldest, i.e likely to become a leader
            {next_state, candidate,
             initialize_election(SData#sdata{leader = undefined})};
        false ->
            {next_state, follower_wait,
             SData#sdata{leader = undefined,
                         election_timeout_ms = ?ELECTION_TIMEOUT_MS + now_ms()}}
    end;
follower(Type, Msg, SData) ->
    handle_common(?FUNCTION_NAME, Type, Msg, SData).

%% Sub state for follower we are waiting for a leader
%% postpone user-requests
-spec follower_wait(Type, Request, #sdata{}) -> Result when
      Type :: cast | {call, gen_statem:from()} | info,
      Request :: #vote_request{}
               | #append_request{}
               | #transfer_leader_request{}
               | #log_request{}
               | #local_lookup{}
               | #discover{}
               | #connect{}
               | ?TICK_MESSAGE
               | {'EXIT', pid(), dynamic()}
               | {nodeup | nodedown, node()},
      Result :: gen_statem:event_handler_result(role(), #sdata{}).
follower_wait(Type, Msg, SData0) ->
    case handle_common(?FUNCTION_NAME, Type, Msg, SData0) of
        {next_state, NextState, SData} ->
            {next_state, NextState, SData#sdata{election_timeout_ms = infinity}};
        {next_state, NextState, SData, Actions} ->
            {next_state, NextState, SData#sdata{election_timeout_ms = infinity}, Actions};
        KeepState ->
            KeepState
    end.

-spec candidate(Type, Request, #sdata{}) -> Result when
      Type :: cast | {call, gen_statem:from()} | info,
      Request :: #vote_request{}
               | #vote_reply{}
               | #append_request{}
               | #append_reply{}
               | #transfer_leader_request{}
               | #log_request{}
               | #local_lookup{}
               | #connect{}
               | #discover{}
               | ?TICK_MESSAGE
               | {nodeup | nodedown, node()},
      Result :: gen_statem:event_handler_result(role()).

candidate(cast, #vote_reply{to = Me} = VR, #sdata{me = Me} = SData0) ->
    {NextState, SData} = handle_vote_reply(VR, SData0),
    {next_state, NextState, SData};
candidate(Type, Msg, SData) ->
    handle_common(?FUNCTION_NAME, Type, Msg, SData).

-spec leader(Type, Request, #sdata{}) -> Result when
      Type :: cast | {call, gen_statem:from()} | info,
      Request :: #vote_request{}
               | #vote_reply{}
               | #append_request{}
               | #append_reply{}
               | #log_request{}
               | #local_lookup{}
               | #discover{}
               | ?TICK_MESSAGE
               | {'EXIT', pid(), dynamic()}
               | {nodeup | nodedown, node()},
      Result :: gen_statem:event_handler_result(role()).

leader(CallOrCast, #log_request{reply_to = ReplyTo0, log = Log} = LR0, #sdata{me=Me} = SData0) ->
    LogRef = case ReplyTo0 of
                 undefined ->
                     case CallOrCast of
                         cast -> {cast, make_ref()};
                         {call, From} -> {call, From}
                     end;
                 _ ->
                     ReplyTo0
             end,
    LR = LR0#log_request{reply_to = LogRef},
    SData1 = maybe_single_member_commit(insert(LogRef, Log, maybe_prepare_reply(LR, SData0))),
    %% Future: not always do a immediate send, batch a little bit
    Members = maps:keys(committed_members(SData1)) -- [Me],
    SData = lists:foldl(fun maybe_send_append/2, SData1, Members),
    {keep_state, SData};
leader(cast, #append_reply{to = Me} = AR, #sdata{me=Me} = SData0) ->
    {NextState, SData} = handle_append_reply(AR, SData0),
    {next_state, NextState, SData};
leader(cast, #discover{} = DC, SData0) ->
    SData = handle_discover(DC, SData0),
    %% Should we have a pause (merge) state here FIXME
    {keep_state, SData};
leader(info, ?TICK_MESSAGE, SData) ->
    {keep_state, handle_leader_tick(SData)};
leader(info, {NodeUpDown, Node}, SData0)
  when Node == node(), NodeUpDown == nodeup;  NodeUpDown == nodedown ->
    #sdata{role = Role} = SData = reset(SData0),
    {next_state, Role, SData};
leader(info, {'EXIT', Pid, _Reason}, SData) ->
    %% Questionable: leave all disconnected peers
    SData1 = lists:foldl(
               fun(ToLeave, SDataAcc) ->
                       insert({internal, make_ref()}, {leave, ToLeave}, SDataAcc)
               end,
               SData,
               [Peer || {_, PeerPid} = Peer := _ <- appended_members(SData), PeerPid =:= Pid]
              ),
    {keep_state, SData1};
leader(info, {nodedown, Node}, SData) ->
    %% Questionable: leave all disconnected peers
    SData1 = lists:foldl(
               fun(ToLeave, SDataAcc) ->
                       insert({internal, make_ref()}, {leave, ToLeave}, SDataAcc)
               end,
               SData,
               [Peer || {_, Pid} = Peer := _ <- appended_members(SData), node(Pid) =:= Node]
              ),
    {keep_state, SData1};
leader(info, {nodeup, Node}, #sdata{name = Name} = SData) ->
    case Name of
        undefined -> {keep_state, SData};
        _ -> {keep_state, discover([{Name, Node}], SData)}
    end;
leader(Type, Msg, SData) ->
    handle_common(?FUNCTION_NAME, Type, Msg, SData).

-spec handle_common(role(), Type, Request, #sdata{}) -> Result when
      Type :: cast | {call, gen_statem:from()} | info,
      Request ::  #append_request{}
                | #log_request{}
                | #vote_request{}
                | #local_lookup{}
                | #connect{}
                | #discover{}
                | ?TICK_MESSAGE
                | {nodeup | nodedown, node()}
                | dynamic(),  %% We handle error cases or why it is complaining?
      Result :: gen_statem:event_handler_result(role(), #sdata{}).

%% Should the leader get this !!!!
handle_common(_StateName, cast, #append_request{to = Me} = AR,  #sdata{me = Me} = SData0) ->
    #sdata{role = Role} = SData = handle_append_request(AR, SData0),
    {next_state, Role, SData};
handle_common(_StateName, Type, #log_request{redirect = Redirect} = LR,
              #sdata{leader = Leader} = SData) ->
    if Leader =:= undefined ->
            {keep_state_and_data, [postpone]};
       Redirect =:= 0 ->
            case Type of
                {call, From} ->
                    {keep_state_and_data, [{reply, From, {error, redirect}}]};
                cast ->
                    keep_state_and_data
            end;
       true ->
            %% FIXME we need to store these if leader crashes here..
            ReplyTo = case Type of
                          cast -> {cast, make_ref()};
                          {call, _} -> Type
                      end,
            peer_send(Leader, LR#log_request{redirect = Redirect - 1,
                                             reply_to = ReplyTo}),
            {keep_state, maybe_prepare_reply(LR, SData)}
    end;
handle_common(_StateName, {call, From}, #connect{servers = Servers0},
              #sdata{name=Name, peers=Peers, me=Me} = SData0) ->
    To = fun(Node, Acc) when is_atom(Node), Name =/= undefined ->
                 [{Name, Node}|Acc];
            (Pid, Acc) when is_pid(Pid) ->
                 [Pid|Acc];
            (_, Acc) ->
                 Acc
         end,
    Servers = lists:foldl(To, [], Servers0),
    SData = discover(Servers, SData0),
    Prev = [peer_pid(Peer) || Peer := _ <- Peers],
    {keep_state, SData, [{reply, From, [peer_pid(Me)|Prev]}]};
handle_common(StateName, cast, #vote_request{to = Me} = VR, #sdata{me = Me} = SData0) ->
    SData = handle_vote_request(StateName, VR, SData0),
    {keep_state, SData};
handle_common(_StateName, cast, #discover{redirect = Redirect} = DR, #sdata{leader = Leader})
  when Leader =/= undefined, Redirect > 0 ->
    peer_send(Leader, DR#discover{redirect = Redirect - 1}),
    keep_state_and_data;
handle_common(_StateName, info, ?TICK_MESSAGE, SData0) ->
    #sdata{role = Role} = SData = handle_tick(SData0),
    {next_state, Role, SData};
handle_common(_StateName, info, {NodeUpDown, Node}, SData0)
  when Node == node(), NodeUpDown == nodeup; NodeUpDown == nodedown ->
    #sdata{role = Role} = SData = reset(SData0),
    {next_state, Role, SData};
handle_common(_StateName, {call, From}, #local_lookup{req = Custom},
              #sdata{module=Mod, custom_db = DB0} = SData) ->
    CommitMetadata = {SData#sdata.branch, SData#sdata.apply_index,
                      SData#sdata.apply_index, SData#sdata.logs},
    {Result, _DB} = Mod:apply_custom(CommitMetadata, Custom, DB0),
    {keep_state_and_data, [{reply, From, {ok, Result}}]};
handle_common(_, {call, From}, get_info, SData) ->
    {keep_state_and_data, [{reply, From, make_info(SData)}]};
handle_common(StateName, {call, From}, Msg, _SData) ->
    ?LOG_DEBUG("~w (~w ~w) Dropped msg: ~P", [?MODULE, self(), StateName, Msg, 20]),
    {keep_state_and_data, [{reply, From, {error, bad_message}}]};
handle_common(StateName, Meta, Msg, _SData) ->
    ?LOG_DEBUG("~w (~w ~w) Dropped ~w msg: ~P", [?MODULE, self(), StateName, Meta, Msg, 20]),
    keep_state_and_data.


-spec handle_vote_request(role(), #vote_request{}, #sdata{}) -> #sdata{}.
handle_vote_request(_State,
                    #vote_request{
                       from = From,
                       to = To,
                       branch = PeerBranch,
                       tenure_id = PeerTenureId,
                       last_log_index = PeerLastLogIndex,
                       last_log_tenure = PeerLastLogTerm
                      },
                    SData) ->
    Succeeded =
        if
            PeerBranch < SData#sdata.branch -> true;
            PeerBranch > SData#sdata.branch -> false;
            %% Equal Branch
            PeerTenureId > SData#sdata.tenure_id -> true;
            PeerTenureId < SData#sdata.tenure_id -> false;
            %% And equal TenureId
            SData#sdata.leader =:= From orelse SData#sdata.leader =:= undefined ->
                if SData#sdata.voted_for =:= From -> true;
                   SData#sdata.voted_for =:= undefined ->
                        {PeerLastLogTerm, PeerLastLogIndex} >=
                            {last_log_tenure(SData), SData#sdata.append_index};
                   true -> false
                end;
            true -> false
        end,
    SData1 =
        case Succeeded of
            true ->
                (to_follower(PeerBranch, PeerTenureId, SData))#sdata{voted_for = From};
            _ ->
                SData
        end,
    peer_send(
        From,
        #vote_reply{
            from = To,
            to = From,
            branch = SData1#sdata.branch,
            tenure_id = SData1#sdata.tenure_id,
            succeeded = Succeeded
        }
    ),
    SData1.

-spec handle_vote_reply(#vote_reply{}, #sdata{}) -> {role(), #sdata{}}.
handle_vote_reply(#vote_reply{
                     from = From,
                     branch = PeerBranch,
                     tenure_id = PeerTenureId,
                     succeeded = Succeeded
                    }, #sdata{peers = Peers} = SData) ->
    if
        PeerBranch =:= SData#sdata.branch,
        PeerTenureId =:= SData#sdata.tenure_id,
        Succeeded,
        is_map_key(From, SData#sdata.peers) ->
            Peers1 = Peers#{From => (map_get(From, Peers))#peer_state{voted = true}},
            SData1 = SData#sdata{peers = Peers1},
            Members = committed_members(SData1),
            Quorum = (map_size(Members) + 1) div 2,
            %% Can be optimized to cache vote count but overkill right now
            Votes = [1 || Peer := _ <- Members, Peer =/= SData1#sdata.me,
                          (map_get(Peer, Peers1))#peer_state.voted],
            Voted = length(Votes) + 1,
            if   %% FIXME + 1 twice here ??
                Voted + 1 >= Quorum ->
                    [begin
                         link(peer_pid(Peer)),
                         send_empty_append(Peer, SData1)
                     end || Peer := _ <- Members, Peer =/= SData1#sdata.me],
                    NowMs = now_ms(),
                    SData2 = SData1#sdata{
                               role = leader,
                               leader = SData1#sdata.me,
                               peers =
                                   #{
                                     Peer => PeerState#peer_state{
                                               base_index = SData1#sdata.append_index,
                                               heartbeat_timeout_ms = NowMs + ?HEARTBEAT_TIMEOUT_MS,
                                               liveness_timeout_ms = NowMs + ?LIVENESS_TIMEOUT_MS
                                              }
                                     || Peer := PeerState <- Peers1
                                    }
                              },
                    SData3 = insert({internal, make_ref()}, {leader, SData#sdata.me}, SData2),
                    %% Questionable, maybe too expensive to do this on every leader change?
                    case SData3#sdata.name of
                        undefined -> {leader, SData3};
                        Name -> {leader, discover([{Name, N} || N <- nodes()], SData3)}
                    end;
                true ->
                    {candidate, SData1}
            end;
        PeerBranch < SData#sdata.branch;
        (PeerBranch =:= SData#sdata.branch andalso PeerTenureId > SData#sdata.tenure_id) ->
            {follower, to_follower(PeerBranch, PeerTenureId, SData)};
        true ->
            {candidate, SData}
    end.

-spec handle_append_request(#append_request{}, #sdata{}) -> #sdata{}.
handle_append_request(#append_request{
                         from = From,
                         to = To,
                         branch = PeerBranch,
                         tenure_id = PeerTenureId,
                         prev_log_index = PrevLogIndex,
                         prev_log_tenure = PrevLogTerm,
                         entries = Entries,
                         leader_commit_index = LeaderCommitIndex,
                         leader_cleanup_index = LeaderCleanupIndex
                        },
                      SData) ->
    SData1 =
        if
            PeerBranch < SData#sdata.branch;
            (PeerBranch =:= SData#sdata.branch andalso PeerTenureId > SData#sdata.tenure_id) ->
                SData0 = to_follower(PeerBranch, PeerTenureId, SData),
                link(peer_pid(From)),
                PrevLeader = SData#sdata.leader,
                case PrevLeader of
                    undefined -> ok;
                    _ -> unlink(peer_pid(PrevLeader))
                end,
                SData0#sdata{leader = From, voted_for = From};
            PeerBranch =:= SData#sdata.branch andalso PeerTenureId =:= SData#sdata.tenure_id ->
                %% This should never happen
                SData#sdata.leader =/= undefined andalso SData#sdata.leader =/= From
                    andalso error("wrong leader"),
                SData#sdata{
                  leader = From,
                  %% not necessary to update voted_for
                  voted_for =
                      case SData#sdata.voted_for of
                          undefined ->
                              From;
                          _ ->
                              SData#sdata.voted_for
                      end,
                  election_timeout_ms = infinity,
                  reset_timeout_ms = now_ms() + ?RESET_TIMEOUT_MS
                 };
            true ->
                SData
        end,
    Result =
        %% From the protocol, we expect append request can only be
        %% send from one leader per branch and tenure
        if
            SData1#sdata.branch =/= PeerBranch orelse SData1#sdata.tenure_id =/= PeerTenureId ->
                not_leader;
            %% Snapshot append
            PrevLogIndex =:= 0 ->
                success;
            SData1#sdata.wait_snapshot ->
                need_older;
            is_map_key(PrevLogIndex, SData1#sdata.logs),
            element(1, map_get(PrevLogIndex, SData1#sdata.logs)) =:= PrevLogTerm ->
                success;
            true ->
                need_older
        end,
    SData2 =
        case Result of
            success ->
                cleanup(LeaderCleanupIndex, commit(LeaderCommitIndex, append(Entries, SData1)));
            _ ->
                SData1
        end,
    peer_send(
      From,
      #append_reply{
         from = To,
         to = From,
         branch = SData2#sdata.branch,
         tenure_id = SData2#sdata.tenure_id,
         result = Result,
         append_id =
             case SData2#sdata.wait_snapshot of
                 true ->
                     0;
                 _ ->
                     SData2#sdata.append_index
             end
        }
     ),
    SData2.

-spec handle_append_reply(#append_reply{}, #sdata{}) -> {role(), #sdata{}}.
handle_append_reply(#append_reply{
                       from = From,
                       branch = PeerBranch,
                       tenure_id = PeerTenureId,
                       result = Result,
                       append_id = PeerAppendId
                      },
                    State) ->
    if
        PeerBranch =:= State#sdata.branch,
        PeerTenureId =:= State#sdata.tenure_id,
        State#sdata.role =:= leader,
        is_map_key(From, State#sdata.peers) ->
            Peers = State#sdata.peers,
            PeerState0 = map_get(From, Peers),
            PeerState =
                case Result of
                    success ->
                        PeerState0#peer_state{
                          match_index = max(PeerState0#peer_state.match_index, PeerAppendId),
                          liveness_timeout_ms = now_ms() + ?LIVENESS_TIMEOUT_MS
                         };
                    %% Change the sending base index if conflict happens or
                    %% peer needs older data
                    need_older ->
                        PeerState0#peer_state{
                          base_index = PeerAppendId,
                          liveness_timeout_ms = now_ms() + ?LIVENESS_TIMEOUT_MS
                         };
                    not_leader ->
                        PeerState0
                end,
            State1 = State#sdata{peers = Peers#{From => PeerState}},
            {leader, maybe_cleanup(maybe_commit(maybe_send_append(From, State1)))};
        PeerBranch < State#sdata.branch;
        (PeerBranch =:= State#sdata.branch andalso PeerTenureId > State#sdata.tenure_id) ->
            {follower, to_follower(PeerBranch, PeerTenureId, State)};
        true ->
            {leader, State}
    end.

-spec handle_discover(#discover{}, #sdata{}) -> #sdata{}.
handle_discover(#discover{from = From, branch = PeerBranch} = Disc, State) ->
    if
        is_map_key(From, State#sdata.peers) orelse
        PeerBranch =:= State#sdata.branch ->
            link(peer_pid(From)), %% Should already be linked
            State;  %% Ignore
        map_size(State#sdata.paused) =/= 0 ->
            %% Redirect discover to the leader to be
            DR = maps:get(discover, State#sdata.paused),
            peer_send(DR#discover.from, Disc),
            State;
        PeerBranch < State#sdata.branch ->
            link(peer_pid(From)),
            %% Newer tree will pause itself and join to older tree
            maybe_single_member_commit(insert({internal, make_ref()}, {pause, Disc}, State));
        true ->
            link(peer_pid(From)),
            discover([From], State)
    end.

% Speed up re-election on leader leave.
-spec handle_transfer_leader_request(#transfer_leader_request{}, #sdata{}) -> {role(), #sdata{}}.
handle_transfer_leader_request(#transfer_leader_request{
                                  from = From,
                                  branch = PeerBranch,
                                  tenure_id = PeerTenureId},
                               SData) ->
    SData1 =
        if  %% Don't understand this, remove??  FIXME
            %% transfer_leader_request should only be handled if we already are a follower,
            %% It is sent by the leader, and if we are a candidate we are already running an
            %% election.
            PeerBranch < SData#sdata.branch;
            (PeerBranch =:= SData#sdata.branch andalso PeerTenureId > SData#sdata.tenure_id) ->
                to_follower(PeerBranch, PeerTenureId, SData);
            true ->
                SData
        end,
    case
        PeerBranch =:= SData1#sdata.branch andalso
        PeerTenureId =:= SData1#sdata.tenure_id andalso
        SData1#sdata.leader =:= From
    of
        true ->
            {candidate, initialize_election(SData1)};
        false ->
            {follower, SData1}
    end.

%% non-leader tick
-spec handle_tick(#sdata{}) -> #sdata{}.
handle_tick(SData) ->
    %% We need to update reset_timer here ?? FIXME ???
    erlang:send_after(?TICK_TIMEOUT_MS, self(), ?TICK_MESSAGE),
    SData1 =
        case now_ms() > SData#sdata.election_timeout_ms of
            true ->
                initialize_election(SData);
            false ->
                SData
        end,
    maybe_reset(SData1).

%% leader tick
-spec handle_leader_tick(#sdata{}) -> #sdata{}.
handle_leader_tick(SData) ->
    %% We need to update reset_timer here ?? FIXME ???
    NowMs = now_ms(),
    erlang:send_after(?TICK_TIMEOUT_MS, self(), ?TICK_MESSAGE),
    SData1 = lists:foldl(fun maybe_send_append/2, SData,
                         maps:keys(committed_members(SData)) -- [SData#sdata.me]),
    SData2 =
        if
            map_size(SData1#sdata.paused) =:= 0 ->
                % Check below quorum here
                % Kick leaved node here
                SData1;
            map_size(SData1#sdata.paused) > 0,
            NowMs > SData1#sdata.merge_timeout_ms,
            % not SData#sdata.wait_snapshot,
            SData1#sdata.append_index =:= SData1#sdata.apply_index ->
                Destinations = maps:keys(SData1#sdata.paused) -- [discover],
                Dest = lists:nth(rand:uniform(length(Destinations)), Destinations),
                % We may want to let follower to be able to send this message too
                MergeLog = {merge, committed_members(SData1),
                            (SData1#sdata.module):serialize(SData1#sdata.custom_db)},
                peer_send(Dest, #log_request{log = MergeLog}),
                SData1#sdata{merge_timeout_ms = now_ms() + ?MERGE_TIMEOUT_MS};
            true ->
                SData1
        end,
    % Dedupe peers and kick dead peers
    % Can be optimized
    AppendMembers2 = appended_members(SData2),
    SData3 = lists:foldl(
               fun(ToLeave, SDataAcc) ->
                       insert({internal, make_ref()}, {leave, ToLeave}, SDataAcc)
               end,
               SData2,
               %% This code doesn't work with un-named processes
               %% this can be solved if leader monitor processes and remove them
               %% when they die.

               %% [
               %%     PeerA
               %%  || {_, PidA} = PeerA := _ <- AppendMembers2,
               %%     {_, PidB} = PeerB := _ <- AppendMembers2,
               %%     node(PidA) =:= node(PidB),
               %%     PeerA < PeerB
               %% ] ++
               [
                Peer
                || Peer := #peer_state{liveness_timeout_ms = LivenessTimeoutMs} <- SData2#sdata.peers,
                   is_map_key(Peer, AppendMembers2),
                   NowMs > LivenessTimeoutMs
               ]
              ),
    maybe_reset(SData3).

-spec terminate(Reason, SData) -> ok when
    Reason :: normal | shutdown | {shutdown, term()} | term(),
    SData :: #sdata{}.
terminate(_Reason, SData) ->
    maybe_leader_handover(SData),
    ok.

%==============================================================================
% leader functions
%==============================================================================

% Try to see if there is any other cluster that we can merge

-spec discover([pid() | peer() | {atom(), node()}], #sdata{}) -> #sdata{}.
discover(Peers, #sdata{name = _Name} = SData) when is_list(Peers) ->
    Msg = #discover{
             from = SData#sdata.me,
             branch = SData#sdata.branch,
             members = committed_members(SData)},
    Send = fun({_StartNs, Pid}, St) when is_pid(Pid) -> gen_statem:cast(Pid, Msg), St;
              (Process, St) -> gen_statem:cast(Process, Msg), St
           end,
    lists:foldl(Send, SData, Peers).

-spec send_empty_append(peer(), #sdata{}) -> term().
send_empty_append(Peer, SData) ->
    case (map_get(Peer, SData#sdata.peers))#peer_state.base_index of
        BaseIndex when BaseIndex =/= 0, is_map_key(BaseIndex, SData#sdata.logs) ->
            {BaseTanureId, _BaseLogRef, _BaseLog} = map_get(BaseIndex, SData#sdata.logs),
            peer_send(
                Peer,
                #append_request{
                    from = SData#sdata.me,
                    to = Peer,
                    branch = SData#sdata.branch,
                    tenure_id = SData#sdata.tenure_id,
                    prev_log_index = BaseIndex,
                    prev_log_tenure = BaseTanureId,
                    entries = [],
                    leader_commit_index = SData#sdata.commit_index,
                    leader_cleanup_index = SData#sdata.cleanup_index
                }
            );
        _ ->
            % Future: change this behavior to not double send snapshot in this case
            send_snapshot(Peer, SData)
    end.

-spec send_snapshot(peer(), #sdata{}) -> term().
send_snapshot(Peer, SData) ->
    {LogTenureId, LogRef, _Log} = map_get(SData#sdata.append_index, SData#sdata.logs),
    Log = {
        snapshot,
        committed_members(SData),
        maps:remove(discover, SData#sdata.paused),
        (SData#sdata.module):serialize(SData#sdata.custom_db)
    },
    peer_send(
        Peer,
        #append_request{
            from = SData#sdata.me,
            to = Peer,
            branch = SData#sdata.branch,
            tenure_id = SData#sdata.tenure_id,
            prev_log_index = 0,
            prev_log_tenure = 0,
            entries = [{SData#sdata.append_index, {LogTenureId, LogRef, Log}}],
            leader_commit_index = SData#sdata.commit_index,
            leader_cleanup_index = SData#sdata.cleanup_index
        }
    ).

%% Append an Log to the tree
-spec insert(log_ref(), log_message(), #sdata{}) -> #sdata{}.
insert(_LogRef, _Log, SData) when SData#sdata.role =/= leader ->
    % This should never happen
    error("not leader");
insert(LogRef, _Log, SData) when map_size(SData#sdata.paused) > 0 ->
    reply(LogRef, {error, paused}, SData);
%% Questionable: Assume no member change is pending commit?
insert(LogRef, {leave, LeavePeer} = Log, SData) ->
    %% LeavePeer should be a member of current branch
    case is_map_key(LeavePeer, appended_members(SData)) of
        true ->
            append({SData#sdata.append_index + 1, {SData#sdata.tenure_id, LogRef, Log}}, SData);
        _ ->
            reply(LogRef, {error, not_joined}, SData)
    end;
%% Questionable: Assume no member change is pending commit?
insert(LogRef, {merge, Members, _CustomDbSerialized} = Log, SData) ->
    %% Anyone is not supposed to join twice to the cluster
    %% If all members merged and leaved and somehow magically a merge message arrived,
    %% we are in trouble. It is solvable by passing Branch instead of Members,
    %% but it needs to permenately store all the past branches
    %% Right now we don't handle this
    case lists:all(fun(Peer) -> not is_map_key(Peer, SData#sdata.peers) end, maps:keys(Members)) of
        true ->
            append({SData#sdata.append_index + 1, {SData#sdata.tenure_id, LogRef, Log}}, SData);
        _ ->
            reply(LogRef, {error, duplicated}, SData)
    end;
insert(LogRef, Log, SData) ->
    append({SData#sdata.append_index + 1, {SData#sdata.tenure_id, LogRef, Log}}, SData).

-spec maybe_single_member_commit(#sdata{}) -> #sdata{}.
maybe_single_member_commit(SData) when SData#sdata.role =/= leader ->
    % This should never happen
    error("not leader");
maybe_single_member_commit(SData) ->
    case map_size(committed_members(SData)) of
        1 ->
            maybe_cleanup(maybe_commit(SData));
        _ ->
            SData
    end.

-spec maybe_send_append(peer(), #sdata{}) -> #sdata{}.
maybe_send_append(_Peer, SData) when SData#sdata.role =/= leader ->
    % This should never happen
    error("not leader");
maybe_send_append(Peer, SData) ->
    NowMs = now_ms(),
    #{
        Peer := #peer_state{
            base_index = BaseIndex,
            heartbeat_timeout_ms = HeartbeatTimeoutMs
        } = PeerSData
    } = Peers = SData#sdata.peers,
    % It is time to do heartbeat, or we have new data to send
    if
        NowMs > HeartbeatTimeoutMs ->
            send_empty_append(Peer, SData),
            SData#sdata{
                peers = Peers#{Peer := PeerSData#peer_state{
                                         heartbeat_timeout_ms = NowMs + ?HEARTBEAT_TIMEOUT_MS}}
             };
        % We don't have the log peer need, send snapshot
        BaseIndex =:= 0; not is_map_key(BaseIndex, SData#sdata.logs) ->
            send_snapshot(Peer, SData),
            SData#sdata{
                peers = Peers#{
                    Peer := PeerSData#peer_state{
                        base_index = SData#sdata.append_index,
                        heartbeat_timeout_ms = NowMs + ?HEARTBEAT_TIMEOUT_MS
                    }
                }
            };
        % Regular batch send
        BaseIndex < SData#sdata.append_index ->
            {LogTenureId, _LogRef, _Log} = map_get(BaseIndex, SData#sdata.logs),
            EndIndex = min(BaseIndex + ?BATCH_SIZE, SData#sdata.append_index),
            peer_send(
                Peer,
                #append_request{
                    from = SData#sdata.me,
                    to = Peer,
                    branch = SData#sdata.branch,
                    tenure_id = SData#sdata.tenure_id,
                    prev_log_index = BaseIndex,
                    prev_log_tenure = LogTenureId,
                    entries =
                        [
                            {LogIndex, maps:get(LogIndex, SData#sdata.logs)}
                         || LogIndex <- lists:seq(BaseIndex + 1, EndIndex)
                        ],
                    leader_commit_index = SData#sdata.commit_index,
                    leader_cleanup_index = SData#sdata.cleanup_index
                }
            ),
            SData#sdata{
                peers = Peers#{
                    Peer => PeerSData#peer_state{
                        base_index = EndIndex,
                        heartbeat_timeout_ms = NowMs + ?HEARTBEAT_TIMEOUT_MS
                    }
                }
            };
        true ->
            SData
    end.

-spec maybe_commit(#sdata{}) -> #sdata{}.
maybe_commit(SData) when SData#sdata.role =/= leader ->
    % This should never happen
    error("not leader");
maybe_commit(SData) when SData#sdata.append_index =:= SData#sdata.commit_index ->
    SData#sdata{reset_timeout_ms = now_ms() + ?RESET_TIMEOUT_MS};
maybe_commit(SData) ->
    NextIndex = SData#sdata.commit_index + 1,
    {IsMerge, Members} =
        case SData#sdata.logs of
            #{NextIndex := {_TenureId, _LogRef, {merge, _Members, _CustomDbSerialized}}} ->
                % Merge request quorum does not count new members
                {true, get_members(SData#sdata.commit_index, SData)};
            _ ->
                {false, get_members(NextIndex, SData)}
        end,
    % Can improve performance here but probably an over kill
    MatchList = lists:sort(
                  [SData#sdata.append_index] ++
                      [
                  (map_get(Peer, SData#sdata.peers))#peer_state.match_index
                       || Peer := _ <- Members,
                          is_map_key(Peer, SData#sdata.peers)
                      ]
                 ),
    CommitIndex = lists:nth((map_size(Members) + 1) div 2, MatchList),
    debug_leader_commit(MatchList, (map_size(Members) + 1) div 2, CommitIndex, SData#sdata.commit_index),
    case CommitIndex > SData#sdata.commit_index andalso
        element(1, map_get(CommitIndex, SData#sdata.logs)) =:= SData#sdata.tenure_id
    of
        true when IsMerge ->
            maybe_commit(commit(NextIndex, SData#sdata{reset_timeout_ms = now_ms() + ?RESET_TIMEOUT_MS}));
        true ->
            case gb_trees:larger(NextIndex, SData#sdata.member_tree) of
                {MemberChangeIndex, _} when MemberChangeIndex =< CommitIndex ->
                    maybe_commit(
                      commit(
                        MemberChangeIndex - 1,
                        SData#sdata{reset_timeout_ms = now_ms() + ?RESET_TIMEOUT_MS}
                       )
                     );
                _ ->
                    commit(CommitIndex, SData#sdata{reset_timeout_ms = now_ms() + ?RESET_TIMEOUT_MS})
            end;
        false ->
            SData
    end.

-spec debug_leader_commit(list(), integer(), integer(), integer()) -> ok.
debug_leader_commit(_MatchList, _MemberI, _CommitIndex, _MyCI) ->
    %% try io:format("~w: ~w: commit ~w(~w) => ~w > ~w = ~w~n",
    %%               [?LINE, self(), _MatchList, _MemberI,
    %%                _CommitIndex, _MyCI, _CommitIndex > _MyCI])
    %% catch _:_ -> ok end,
    ok.

-spec maybe_cleanup(#sdata{}) -> #sdata{}.
maybe_cleanup(SData) when SData#sdata.role =/= leader ->
    % This should never happen
    error("not leader");
maybe_cleanup(SData) ->
    % Cleanup if all members that is not committed leaved got the data
    % with a configurable buffer
    % Can improve performance here but probably an over kill
    CleanupIndex =
        lists:min(
            [SData#sdata.commit_index] ++
                [
                    (map_get(Peer, SData#sdata.peers))#peer_state.match_index
                 || Peer := _ <- get_members(SData#sdata.cleanup_index + 1, SData),
                    % peers that is committed to leave is not counted
                    is_map_key(Peer, SData#sdata.peers)
                ]
        ),
    case CleanupIndex > SData#sdata.cleanup_index of
        true ->
            case gb_trees:larger(CleanupIndex, SData#sdata.member_tree) of
                {MemberChangeIndex, _} when MemberChangeIndex =< CleanupIndex ->
                    maybe_cleanup(cleanup(MemberChangeIndex - 1, SData));
                _ ->
                    cleanup(CleanupIndex, SData)
            end;
        false ->
            SData
    end.

%==============================================================================
% candidate functions
%==============================================================================

-spec initialize_election(#sdata{}) -> #sdata{}.
initialize_election(SData) when SData#sdata.wait_snapshot ->
    % was just merged to another branch but not received data yet
    SData#sdata{election_timeout_ms = now_ms() + ?ELECTION_TIMEOUT_MS};
initialize_election(SData) ->
    SData1 = SData#sdata{
        role = candidate,
        tenure_id = SData#sdata.tenure_id + 1,
        leader = undefined,
        voted_for = SData#sdata.me,
        election_timeout_ms = now_ms() + ?ELECTION_TIMEOUT_MS,
        peers = #{Peer => #peer_state{} || Peer := _ <- SData#sdata.peers}
    },
    lists:foldl(fun send_election/2, SData1, maps:keys(SData1#sdata.peers)).

-spec send_election(peer(), #sdata{}) -> #sdata{}.
send_election(Peer, SData) ->
    peer_send(
        Peer,
        #vote_request{
            from = SData#sdata.me,
            to = Peer,
            branch = SData#sdata.branch,
            tenure_id = SData#sdata.tenure_id,
            last_log_index = SData#sdata.append_index,
            last_log_tenure = last_log_tenure(SData)
        }
    ),
    SData.

%==============================================================================
% follower functions
%==============================================================================

%==============================================================================
% common functions
%==============================================================================

-spec maybe_prepare_reply(#log_request{}, #sdata{}) -> #sdata{}.
maybe_prepare_reply(#log_request{redirect = ?REDIRECT, reply_to = {call, _} = ReplyTo}, SData) ->
    SData#sdata{replies = (SData#sdata.replies)#{ReplyTo => []}};
maybe_prepare_reply(_LogRequest, SData) ->
    SData.

-spec reply(log_ref(), {ok, custom_result()} | error(), #sdata{}) -> #sdata{}.
reply({call, From} = LogRef, Message, SData) ->
    ok = gen_statem:reply(From, Message),
    SData#sdata{replies = maps:remove(LogRef, SData#sdata.replies)};
reply(_, {error, _} = Error, SData) ->
    ?LOG_WARNING("Internal error: ~w~n", [Error]),
    SData;
reply(_LogRef, _Message, SData) ->
    SData.

% Switch to follower when peer tenure is higher than us
-spec to_follower(branch(), tenure_id(), #sdata{}) -> #sdata{}.
to_follower(PeerBranch, PeerTenureId, SData) ->
    SData#sdata{
        role = follower,
        branch = PeerBranch,
        tenure_id = PeerTenureId,
        leader = undefined,
        voted_for = undefined,
        % Wait for snapshot if my branch is being merged
        % Questionable if we should let candidate send to not committed merge peers
        % Leave it here for now
        wait_snapshot = SData#sdata.wait_snapshot orelse (PeerBranch < SData#sdata.branch),
        election_timeout_ms = now_ms() + ?ELECTION_TIMEOUT_MS
    }.

-spec maybe_leader_handover(#sdata{}) -> term().
maybe_leader_handover(SData) when SData#sdata.role =:= leader ->
    {MaxPeer, _MaxTreeInfo} =
        maps:fold(
            fun
                (Peer, #peer_state{match_index = MatchIndex}, {_, Max}) when MatchIndex > Max ->
                    {Peer, MatchIndex};
                (_, _, Acc) ->
                    Acc
            end,
            {undefined, 0},
            SData#sdata.peers
        ),
    MaxPeer =/= undefined andalso
        peer_send(
            MaxPeer,
            #transfer_leader_request{
                from = SData#sdata.me,
                to = MaxPeer,
                branch = SData#sdata.branch,
                tenure_id = SData#sdata.tenure_id
            }
        );
maybe_leader_handover(_SData) ->
    ok.

-spec reset(#sdata{}) -> #sdata{}.
reset(#sdata{me = {OldNs, Pid}} = SData) ->
    maybe_leader_handover(SData),
    NowNs =
        case {now_ns(), OldNs} of
            {Ns, _} when Ns > OldNs ->
                Ns;
            _ ->
                OldNs + 1
        end,
    Me = {NowNs, Pid},

    %% This is just plain wrong..  
    %% [gen_statem:Ref ! {error, reset} || {call, From} := _ <- SData#sdata.replies],
    #sdata{
        name = SData#sdata.name,
        module = SData#sdata.module,
        options = SData#sdata.options,
        me = Me,
        role = leader,
        branch = Me,
        tenure_id = 1,
        leader = Me,
        voted_for = Me,
        wait_snapshot = false,
        logs = #{1 => {1, {internal, make_ref()}, {leader, Me}}},
        append_index = 1,
        commit_index = 1,
        apply_index = 1,
        cleanup_index = 1,
        member_tree = gb_trees:from_orddict([{1, #{Me => []}}]),
        paused = #{},
        peers = #{},
        election_timeout_ms = 0,
        merge_timeout_ms = 0,
        reset_timeout_ms = now_ms() + ?RESET_TIMEOUT_MS,
        custom_db = (SData#sdata.module):reset(Me, SData#sdata.custom_db),
        replies = #{}
    }.

-spec maybe_reset(#sdata{}) -> #sdata{}.
maybe_reset(SData) ->
    case now_ms() > SData#sdata.reset_timeout_ms of
        true ->
            reset(SData);
        _ ->
            SData
    end.

-spec append(log_entry() | [log_entry()], #sdata{}) -> #sdata{}.
append([], SData) ->
    SData;
append([Head | Tail], SData) ->
    append(Tail, append(Head, SData));
append({LogId, {TenureId, LogRef, _Log}} = LogEntry, SData)
  when is_map_key(LogId, SData#sdata.logs) ->
    case SData#sdata.logs of
        #{LogId := {TenureId, LogRef, _}} ->
            SData;
        #{LogId := _} ->
            append(LogEntry, delete(LogId, SData))
    end;
% LogId should be append_index + 1, or Log is a snapshot
append({LogId, {_TenureId, _LogRef, Log} = LogValue}, SData) ->
    SData1 =
        case Log of
            {merge, Members, _CustomDbSerialized} ->
                NowMembers = maps:merge(get_members(LogId - 1, SData), Members),
                NowMs = now_ms(),
                SData#sdata{
                    member_tree = gb_trees:insert(LogId, NowMembers, SData#sdata.member_tree),
                    peers = maps:merge(
                        SData#sdata.peers,
                        #{
                            Peer =>
                                case SData#sdata.role of
                                    leader ->
                                        #peer_state{
                                            base_index = SData#sdata.append_index,
                                            heartbeat_timeout_ms = NowMs + ?HEARTBEAT_TIMEOUT_MS,
                                            liveness_timeout_ms = NowMs + ?LIVENESS_TIMEOUT_MS
                                        };
                                    _ ->
                                        #peer_state{}
                                end
                         || Peer := _ <- Members
                        }
                    )
                 };
            {leave, Peer} ->
                NowMembers = maps:remove(Peer, get_members(LogId - 1, SData)),
                SData#sdata{member_tree =
                                gb_trees:insert(LogId, NowMembers, SData#sdata.member_tree)};
            {pause, #discover{members = Members} = Discover} ->
                SData#sdata{paused = Members#{discover => Discover}};
            {snapshot, Members, Paused, CustomDbSerialized} ->
                CustomDb = (SData#sdata.module):apply_replace(CustomDbSerialized,
                                                              SData#sdata.custom_db),
                SData#sdata{
                    wait_snapshot = false,
                    logs = #{},
                    commit_index = LogId,
                    apply_index = LogId,
                    cleanup_index = LogId,
                    member_tree = gb_trees:from_orddict([{LogId, Members}]),
                    paused = Paused,
                    peers = #{Peer => #peer_state{} ||
                                Peer := _ <- Members,
                                Peer =/= SData#sdata.me},
                    custom_db = CustomDb
                };
            _ ->
                SData
        end,
    SData1#sdata{append_index = LogId, logs = (SData1#sdata.logs)#{LogId => LogValue}}.

-spec delete(log_id(), #sdata{}) -> #sdata{}.
delete(_LogId, SData) when SData#sdata.append_index < SData#sdata.commit_index ->
    % This should never happen
    error("commit conflict");
delete(LogId, SData) when SData#sdata.append_index < LogId ->
    SData;
delete(_LogId, SData) ->
    SData1 =
        case map_get(SData#sdata.append_index, SData#sdata.logs) of
            {_TenureId, LogRef, {merge, Members, _CustomDbSerialized}} ->
                SData#sdata{peers = maps:without(maps:keys(Members), SData#sdata.peers)};
            {_TenureId, LogRef, {pause, _Paused}} ->
                SData#sdata{paused = #{}};
            {_TenureId, LogRef, {snapshot, _Members, _Paused, _CustomDbSerialized}} ->
                % This should never happen
                error("snapshot conflict");
            {_TenureId, LogRef, _Log} ->
                SData
        end,
    SData2 = SData1#sdata{
        logs = maps:remove(SData1#sdata.append_index, SData1#sdata.logs),
        append_index = SData1#sdata.append_index - 1,
        member_tree = gb_trees:delete_any(SData1#sdata.append_index, SData1#sdata.member_tree)
    },
    reply(LogRef, {error, {failed, _LogId}}, SData2).

-spec commit(log_id(), #sdata{}) -> #sdata{}.
commit(CommitIndex, #sdata{apply_index = ApplyIndex} = SData0) ->
    %% Future: make this async
    do_apply(ApplyIndex, CommitIndex, SData0).

-spec do_apply(log_id(), log_id(), #sdata{}) -> #sdata{}.
do_apply(ApplyIndex, CommitIndex, SData0)
  when ApplyIndex < CommitIndex ->
    SData = do_apply(ApplyIndex, SData0),
    do_apply(ApplyIndex+1, CommitIndex, SData);
do_apply(ApplyIndex, CommitIndex, SData) ->
    SData#sdata{commit_index = CommitIndex, apply_index = ApplyIndex}.

-spec do_apply(log_id(), #sdata{}) -> #sdata{}.
do_apply(ApplyIndex, SData) ->
    case map_get(ApplyIndex + 1, SData#sdata.logs) of
        {TenureId, LogRef, {custom, CustomLog}} ->
            CommitMetadata = {SData#sdata.branch, ApplyIndex, TenureId, LogRef},
            {Result, CustomDb} = (SData#sdata.module):apply_custom(
                CommitMetadata,
                CustomLog,
                SData#sdata.custom_db
            ),
            reply(LogRef, {ok, Result}, SData#sdata{custom_db = CustomDb});
        {TenureId, LogRef, {merge, Members, CustomDbSerialized}} ->
            CommitMetadata = {SData#sdata.branch, ApplyIndex, TenureId, LogRef},
            CustomDb = (SData#sdata.module):apply_merge(
                CommitMetadata,
                Members,
                CustomDbSerialized,
                SData#sdata.custom_db
            ),
            SData#sdata{custom_db = CustomDb};
        {_TenureId, _LogRef, {leave, Peer}} when Peer =:= SData#sdata.me ->
            reset(SData);
        {TenureId, LogRef, {leave, Peer}} ->
            CommitMetadata = {SData#sdata.branch, SData#sdata.apply_index, TenureId, LogRef},
            CustomDb = (SData#sdata.module):apply_leave(CommitMetadata, Peer,
                                                        SData#sdata.custom_db),
            SData#sdata{
                custom_db = CustomDb,
                peers = maps:remove(Peer, SData#sdata.peers)
            };
        {_TenureId, _LogRef, {leader, _Peer}} ->
            SData;
        {_TenureId, _LogRef, {pause, _Paused}} ->
            SData;
        {_TenureId, _LogRef, {snapshot, _Members, _Paused, _CustomDbSerialized}} ->
            % This should never happen
            error("wrong apply")
    end.

-spec cleanup(log_id(), #sdata{}) -> #sdata{}.
cleanup(LogId, SData)
  when LogId < SData#sdata.cleanup_index; SData#sdata.cleanup_index >= SData#sdata.apply_index ->
    SData;
cleanup(_LogId, SData) ->
    CleanupLogId = SData#sdata.cleanup_index - maps:get(extra_logs, SData#sdata.options, 1000),
    SData#sdata{
        logs = maps:remove(CleanupLogId, SData#sdata.logs),
        member_tree =
            case gb_trees:is_defined(CleanupLogId + 1, SData#sdata.member_tree) of
                true ->
                    gb_trees:delete_any(CleanupLogId, SData#sdata.member_tree);
                _ ->
                    case gb_trees:take_any(CleanupLogId, SData#sdata.member_tree) of
                        {Members, MemberTree} ->
                            % eqwalizer:ignore gb_trees:take_any is not dynamic()
                            gb_trees:insert(CleanupLogId + 1, Members, MemberTree);
                        _ ->
                            SData#sdata.member_tree
                    end
            end,
        cleanup_index = SData#sdata.cleanup_index + 1
    }.

%==============================================================================
% util functions
%==============================================================================
-spec now_ms() -> time_ms().
now_ms() ->
    erlang:system_time(millisecond).

-spec now_ns() -> time_ns().
now_ns() ->
    erlang:system_time(nanosecond).

-spec peer_send(peer(), term()) -> term().
peer_send(Peer, Msg) ->
    % This required dist_auto_connect
    % Future: can be replaced by callback transports
    gen_statem:cast(peer_pid(Peer), Msg).

-spec last_log_tenure(#sdata{}) -> tenure_id().
last_log_tenure(SData) ->
    {LastLogTenure, _LastLogRef, _LastLog} = map_get(SData#sdata.append_index, SData#sdata.logs),
    LastLogTenure.

-spec committed_members(#sdata{}) -> members().
committed_members(SData) ->
    case gb_trees:smaller(SData#sdata.commit_index + 1, SData#sdata.member_tree) of
        {_, Members} ->
            Members;
        none ->
            error("bad member tree")
    end.

-spec appended_members(#sdata{}) -> members().
appended_members(SData) ->
    {_, Members} = gb_trees:largest(SData#sdata.member_tree),
    Members.

-spec get_members(log_id(), #sdata{}) -> members().
get_members(LogId, SData) ->
    case gb_trees:smaller(LogId + 1, SData#sdata.member_tree) of
        {_, Members} ->
            Members;
        none ->
            error("bad member tree")
    end.

-spec make_info(#sdata{}) -> map().
make_info(#sdata{me = Me, leader = Leader, role = Role,
                 append_index = Append,
                 commit_index = Commit,
                 apply_index  = Apply,
                 tenure_id = Tenure,
                 peers = Peers
                } = SData) ->
    #{a_id => Me, a_leader => Leader, a_role => Role,
      idx_tenure => Tenure, idx_append => Append, idx_commit => Commit, idx_apply => Apply,
      member_peers => maps:keys(Peers),
      member_all => appended_members(SData),
      member_links => element(2,erlang:process_info(self(), links))
     }.

-spec initial_members(undefined | atom(), map()) -> [pid() | {atom(), node()}].
initial_members(undefined, Options) ->
    [Pid || Pid <- maps:get(members, Options, []), is_pid(Pid)];
initial_members(Name, Options) ->
    [ case is_pid(PidOrNode) of
          true -> PidOrNode;
          false -> {Name, PidOrNode}
      end
      || PidOrNode <- maps:get(members, Options, nodes())].

-spec peer_pid(peer()) -> pid().
peer_pid({_, Pid}) when is_pid(Pid) ->
    Pid.

%% -spec peer_node(peer()) -> node().
%% peer_node({_, Pid}) when is_pid(Pid) ->
%%     node(Pid).

