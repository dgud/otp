%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2016, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created :  7 Jul 2016 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(choord).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% debug
-export([print_ring/1, print_state/1]).
-compile(export_all).

-define(KEY_SIZE(BIT_SIZE), (1 bsl (BIT_SIZE))).

-record(id, {key::range(), pid::pid()}).
-record(finger, {start::range(), last::range(), node::id()}).
-record(state, {id :: id(),
		pred :: id(),
		succ=[] :: [id()],
		fingers=[] :: [finger()],
		key_bit_sz :: integer()
	       }).

-type range() :: non_neg_integer().
-type id() :: #id{}.
-type finger() :: #finger{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link([{atom(), term()}]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Props) when is_list(Props) ->
    gen_server:start_link(?MODULE, Props, []).

-spec start([{atom(), term()}]) -> {ok, pid()} | ignore | {error, term()}.
start(Props) when is_list(Props) ->
    gen_server:start(?MODULE, Props, []).

-spec key(term(), range()) -> integer().
key(Atom, Size) when is_atom(Atom) -> erlang:phash2(atom_to_list(Atom), ?KEY_SIZE(Size));
key(Term, Size) -> erlang:phash2(Term, ?KEY_SIZE(Size)).

find_successor(Gate, Id) ->
    Res = {Pred, Succs} = find_predecessor(Gate, Id),
    case call(Pred, get_successor) of %% Assert
	Succs -> Res;
	Changed -> {Changed, Succs}
    end.

find_predecessor(Gate, Id) ->
    case call(Gate, {find_predecessor, Id}) of
	{ok, Pred, Succs} -> {Pred, Succs};
	{cont, Next} -> find_predecessor(Next, Id)
    end.

print_state(Gate) ->
    gen_server:call(Gate, {debug_state, max}).

print_ring(Gate) ->
    gen_server:call(Gate, {debug_state, ring}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Props) ->
    Gates = proplists:get_value(gates, Props, []),
    KeyBSZ = proplists:get_value(key_bit_sz, Props, 32),
    Key = proplists:get_value(key, Props, key(self(), KeyBSZ)),  %% For debugging only
    Id = #id{key=Key, pid=self()},
    Fingers0 = make_fingers(Id, KeyBSZ),
    {ok, init_neighbors(Gates, #state{id=Id, pred=Id, succ=[Id], fingers=Fingers0, key_bit_sz=KeyBSZ})}.

handle_call({set_predecessor, NewPred}, _From, #state{pred=undefined=OldPred}=State) ->
    setup_monitor(NewPred),
    {reply, {ok, OldPred}, State#state{pred=NewPred}};
handle_call({set_predecessor, NewPred}, _From, #state{id=Id, pred=OldPred}=State) ->
    case is_process_alive(get_pid(OldPred)) of
        true ->
            case memberIN(NewPred, OldPred, Id) of
                true ->
                    setup_monitor(NewPred),
                    %TODO demonitor OldPred
                    {reply, {ok, OldPred}, State#state{pred=NewPred}};
                false ->
                    {reply, {error, OldPred}, State}
            end;
        false ->
            setup_monitor(NewPred),
            {reply, {ok, undefined}, State#state{pred=NewPred}}
    end;
handle_call(get_successor, _From, #state{succ=[Succs|_]}=State) ->
    {reply, Succs, State};
handle_call({find_predecessor, Id}, _From, State) ->
    Reply = find_predecessor_impl(Id, State),
    {reply, Reply, State};
handle_call({debug_state, Level}, From, #state{id=Id, succ=[Succs|_]}=State) ->
    io:format("~n*****************~n"),
    print_state(State, Level),
    cast(Succs, {debug_state, Level, Id, From}),
    {noreply, State}.

handle_cast({update_finger_table, Id, _I}, #state{id=Id}=State) ->
    %% My own state is already correct
    {noreply, State};
handle_cast({update_finger_table, S, I}, State) ->
    Fingers = update_finger_table(S, I, State),
    [#finger{node=Succs}|_] = Fingers,
    {noreply, State#state{succ=[Succs], fingers=Fingers}};
handle_cast({debug_state, Level, Start, From}=Cont, #state{id=Id, succ=[Succs|_]}=State) ->
    case Start of
	Id -> gen_server:reply(From, ok);
	_ ->
	    print_state(State, Level),
	    cast(Succs, Cont)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("~p: Unhandled ~p~n", [?LINE, _Msg]),
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _} = Msg,
	    #state{id=Id, pred=Pred, succ=Succs, fingers=Fingers} = State) ->
    PredPid = get_pid(Pred),
    SuccPid = get_pid(hd(Succs)),
    UpdFingers = fix_fingers(Pid, Id, lists:reverse(Fingers), []),
    if PredPid == Pid, SuccPid == Pid -> %% We are the only left
	    {noreply, State#state{pred=Id, succ=[Id], fingers=UpdFingers}};
       PredPid == Pid ->
	    {noreply, State#state{pred=undefined, fingers=UpdFingers}};
      SuccPid == Pid ->
	    case handle_dead_successor(State#state{fingers=UpdFingers}, Pid) of
		error ->
		    self() ! Msg,
		    {noreply, State};
		NewState ->
		    {noreply, NewState}
	    end;
       true ->
	    {noreply, State#state{fingers=UpdFingers}}
    end;
handle_info(_Info, State) ->
    io:format("~p: Unhandled ~p~n", [?LINE, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_dead_successor(#state{id=Id, fingers=Fingers}=State, Pid) ->
    Next = next_succs(Fingers, Pid),
    case catch set_predecessor(Next, Id) of
	{'EXIT', _} ->
	    error;
	{_, #id{}=Succ} ->
	    State#state{succ=[Succ]}
    end.

set_predecessor(Succs, Id) ->
    case call(Succs, {set_predecessor, Id}) of
        {ok, Pred} ->
            {Pred, Succs};
        {error, NewSuccs} ->
            set_predecessor(NewSuccs, Id)
    end.

make_fingers(#id{key=N} = Id, KeyBSZ) ->
    Start = [(N + (1 bsl (K-1))) rem ?KEY_SIZE(KeyBSZ) ||
		K <- lists:seq(1, KeyBSZ)],
    make_fingers(Start, N, Id).

make_fingers([Start|[Next|_]=Rest], Last, Id) ->
    [#finger{start=Start, last=Next, node=Id}|make_fingers(Rest,Last,Id)];
make_fingers([Start], Last, Id) ->
    [#finger{start=Start, last=Last, node=Id}].

init_neighbors([Gate|Gates], #state{id=Id, fingers=[#finger{start=Start}=F0|Fingers]}=State0) ->
    case find_successor(Gate, Start) of
	{error, _} ->
	    init_neighbors(Gates, State0);
	{_, Succs0} ->
	    case catch set_predecessor(Succs0, Id) of
		{'EXIT', {noproc, _}} ->
		    init_neighbors([Gate|Gates], State0);
		{Pred, Succs} ->
		    setup_monitor(Pred),
		    setup_monitor(Succs),
		    State1 = State0#state{pred=Pred, succ=[Succs]},
		    F = F0#finger{node=Succs},
		    Fs = [F|init_fingers(Fingers, Id#id.key, Succs, Gate)],
		    spawn_link(fun() -> update_others(Id, 1, State0#state.key_bit_sz) end),
		    State1#state{fingers=Fs}
	    end
    end;
init_neighbors(_Gs, State) ->
    State.

init_fingers([#finger{start=Start}=F|Fs], N, #id{key=NodeId}=Prev, Gate) ->
    case memberIN(Start, N, NodeId) of
	true  ->
	    setup_monitor(Prev),
	    [F#finger{node=Prev}|init_fingers(Fs, N, Prev, Gate)];
	false ->
	    {_, Succs} = find_successor(Gate, Start),
	    setup_monitor(Succs),
	    [F#finger{node=Succs}|init_fingers(Fs, N, Succs, Gate)]
    end;
init_fingers([], _, _, _) -> [].

fix_fingers(Pid, Me, [#finger{node=#id{pid=Pid}}=F1|Fingers], Acc) ->
    spawn(fun() -> fix_finger(Me, F1, length(Fingers)) end),
    case Acc of
	[] ->
	    fix_fingers(Pid, Me, Fingers, [F1#finger{node=Me}|Acc]);
	[#finger{node=Next}|_] ->
	    fix_fingers(Pid, Me, Fingers, [F1#finger{node=Next}|Acc])
    end;
fix_fingers(Pid, Me, [F1|Fingers], Acc) ->
    fix_fingers(Pid, Me, Fingers, [F1|Acc]);
fix_fingers(_Pid, _Me, [], Acc) -> Acc.

fix_finger(Me, #finger{start=Start}=F, I) ->
    case catch find_successor(Me, Start) of
        {'EXIT', _} ->
            case is_process_alive(get_pid(Me)) of
                true ->
                    fix_finger(Me, F, I); %TODO should we spin here?
                false ->
                    ok
            end;
        {_, Succs} ->
            setup_monitor(Succs),
            cast(Me, {update_finger_table, Succs, I+1})
    end.

find_predecessor_impl(Id, #state{id=This, succ=[Succs|_], fingers=Fingers}) ->
    case memberNI(Id, This#id.key, Succs#id.key) of
	true ->
	    {ok, This, Succs};
	false ->
	    %% [io:put_chars(print_finger(F)) || F <- Fingers],
	    case closest_preceding_fingers(This, Id, Fingers) of
		This -> {ok, This, Succs};
		Next -> {cont, Next}
	    end
    end.

closest_preceding_fingers(#id{key=This}=N, Id, Fingers) ->
    Check = fun(#finger{node=#id{key=Key}}) ->
		    %% io:format("mem ~p < ~p < ~p~n",[This, Key, Id]),
		    not memberNN(Key, This, Id)
	    end,
    case lists:dropwhile(Check, lists:reverse(Fingers)) of
	[] -> N;
	[#finger{node=Node}|_] -> Node
    end.

next_succs([#finger{node=#id{pid=Pid}}|Fingers], Pid) ->
    next_succs(Fingers, Pid);
next_succs([#finger{node=Next}|_], _) ->
    Next.

update_others(#id{key=Key}=Id, I, KeyBSZ)
  when I =< KeyBSZ ->
    KeySize = ?KEY_SIZE(KeyBSZ),
    Prev = (KeySize + Key - (1 bsl (I-1)) + 1) rem KeySize,
    {Pred,_} = find_predecessor(Id, Prev),
    %io:format("UPD ~p (~p) => Pred ~p ~s~n", [I, Key, Prev, print_key(Pred)]),
    cast(Pred, {update_finger_table, Id, I}),
    update_others(Id, I+1, KeyBSZ);
update_others(_, _, _) -> ok.

update_finger_table(#id{key=SKey}=S, I,
		    #state{id=#id{key=N}, fingers=Fingers0, pred=Pred}) ->
    {Part1, [F0|Part2]} = lists:split(I-1, Fingers0),
    #finger{node=#id{key=Node}} = F0,
    case memberIN(SKey, N, Node) of
	false ->
	    Fingers0;
	true ->
	    setup_monitor(S),
	    F = F0#finger{node=S},
	    cast(Pred, {update_finger_table, S, I}),
	    Part1 ++ [F|Part2]
    end.

%% Closed range
memberNN(Id, Near, Far)
  when Near < Id andalso Id < Far ->
    true;
memberNN(Id, Near, Far)
  when Near > Far andalso ((Near < Id) orelse (Id < Far)) ->
    true;
memberNN(Id, Near, Near) when Id =/= Near ->
    true;
memberNN(_, _, _) ->
    false.

%% open range post
memberNI(Id, Near, Far)
  when Near < Id andalso Id =< Far ->
    true;
memberNI(Id, Near, Far)
  when Near >= Far andalso ((Near < Id) orelse (Id =< Far)) ->
    true;
%%memberNI(Near, Near, Near) -> true;  % Near = Far max set
memberNI(_, _, _) ->
    false.

%% open range pre
memberIN(Id, Near, Far)
  when Near =< Id andalso Id < Far ->
    true;
memberIN(Id, Near, Far)
  when Near >= Far andalso ((Near =< Id) orelse (Id < Far)) ->
    true;
%%memberIN(Near, Near, Near) -> io:format("~p ", [?LINE]), true;    % Near = Far  min set
memberIN(_, _, _) ->
    false.

cast(undefined, _Msg) ->
    ok; %TODO is it safe to drop here?
cast(#id{pid=Pid}, Msg) ->
    gen_server:cast(Pid, Msg);
cast(Pid, Msg) when is_pid(Pid) ->
    gen_server:cast(Pid, Msg).

call(#id{pid=Pid}, Msg) ->
    call(Pid, Msg);
call(Pid, Msg) when is_pid(Pid) ->
    gen_server:call(Pid, Msg, infinity).

get_pid(#id{pid=Pid}) -> Pid;
get_pid(Pid) -> Pid.

setup_monitor(S) ->
    Pid = get_pid(S),
    {monitors, Ms} = process_info(self(), monitors),
    case lists:member({process,Pid}, Ms) of
	true  -> already_monitored;
	false -> monitor(process, Pid)
    end.

%%% Debug

print_state(#state{id=This, pred=Pred, succ=[Succ|_]}, ring) ->
    io:format("~s <- ~s -> ~s~n", [print_key(Pred), print_key(This), print_key(Succ)]);
print_state(#state{id=This, pred=Pred, succ=Succs, fingers=Fingers}, _) ->
    io:format("~nTHIS: ~p ~s~n", [self(), print_key(This)]),
    io:format("PRED: ~s~n", [print_key(Pred)]),
    io:format("SUCC: ~p~n", [[lists:flatten(print_key(S)) || S <- Succs]]),
    io:format("Fingers: ~n", []),
    [io:put_chars(print_finger(F)) || F <- Fingers].

print_finger(#finger{start=Start, last=Last, node=Node}) ->
    io_lib:format(" {~.3w ~.5w ~s}~n",[Start, Last, print_key(Node)]).

print_key(#id{key=Key, pid=Pid}) ->
    io_lib:format("<~.3p ~p>", [Key,Pid]).

