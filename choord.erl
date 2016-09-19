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
-export([start/1, start_link/1, find_successor/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%% debug
-export([print_ring/1, print_state/1, print_state/2, print_states/1]).
-compile(export_all).

-define(KEY_SIZE(BIT_SIZE), (1 bsl (BIT_SIZE))).

-record(id, {key::range(), pid::pid()}).
-record(finger, {start::range(), last::range(), node::id()}).
-record(state, {id :: id(),
		pred :: id(),
		succs=[] :: [id()],
		fingers=[] :: [finger()],
		key_bit_sz :: integer(),
                succ_list_sz :: integer()
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
    Res = {Pred, Succ} = find_predecessor(Gate, Id),
    case call(Succ, get_predecessor) of
        Pred -> Res;
        Changed ->
            find_successor(Succ, Id)
    end.

find_successors(Gate, #id{key=Key}=Id, ListSz) ->
    {Pred, _} = find_predecessor(Gate, Key),
    Succs = call(Pred, get_successors),
    find_successors(hd(lists:reverse(Succs)), Id, Succs, ListSz).

find_successors(_, _, Succs, ListSz) when length(Succs) >= ListSz ->
    lists:sublist(Succs, ListSz);
find_successors(Gate, Id, Succs0, ListSz) ->
    GSuccs = call(Gate, get_successors),
    %case insert_successors(GSuccs, Id, Succs0, ListSz) of
    case insert_successors_1(GSuccs, Id, Succs0, ListSz, [], []) of
        {Succs, []} -> Succs;
        {Succs, _} -> find_successors(hd(GSuccs), Id, Succs, ListSz)
    end.

find_predecessor(Gate, Id) ->
    case call(Gate, {find_predecessor, Id}) of
	{ok, Pred, Succ} -> {Pred, Succ};
	{cont, Next} -> find_predecessor(Next, Id)
    end.

print_state(Gate) ->
    gen_server:call(Gate, {debug_state, single}).

print_states(Gate) ->
    gen_server:call(Gate, {debug_state, max}).

print_ring(Gate) ->
    gen_server:call(Gate, {debug_state, ring}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Props) ->
    Gates = proplists:get_value(gates, Props, []),
    KeyBSZ = proplists:get_value(key_bit_sz, Props, 32),
    SuccListSZ = proplists:get_value(succ_list_sz, Props, 5),
    Key = proplists:get_value(key, Props, key(self(), KeyBSZ)),  %% For debugging only
    Id = #id{key=Key, pid=self()},
    Fingers0 = make_fingers(Id, KeyBSZ),
    {ok, init_neighbors(Gates, #state{id=Id, pred=Id, succs=[Id], fingers=Fingers0,
                                      key_bit_sz=KeyBSZ, succ_list_sz=SuccListSZ})}.


handle_call({set_predecessor, Pred}, _From, %TODO can we handle this special case differently?
            #state{id=Id, pred=Id, succs=[Id], fingers=[F|Fs]}=State0) ->
    setup_monitor(Pred),
    State = State0#state{pred=Pred, succs=[Pred], fingers=[F#finger{node=Pred}|Fs]},
    {reply, {ok, Id, [Pred]}, State};
handle_call({set_predecessor, Pred}, _From, State0) ->
    {Res, State} = set_predecessor_impl(Pred, State0),
    {reply, Res, State};

handle_call(get_predecessor, _From, #state{pred=Pred}=State) ->
    {reply, Pred, State};
handle_call(get_successors, _From, #state{succs=Succs}=State) ->
    {reply, Succs, State};
handle_call({find_predecessor, Id}, _From, State) ->
    Reply = find_predecessor_impl(Id, State),
    {reply, Reply, State};
handle_call({debug_state, Level}, From, #state{id=Id, succs=[Succ|_]}=State) ->
    io:format("~n*****************~n"),
    print_state(State, Level),
    case Level of
        single -> {reply, ok, State};
        _ ->
            cast(Succ, {debug_state, Level, Id, From}),
            {noreply, State}
    end.

handle_cast({set_predecessor, Pred}, State0) -> %% This always comes from the Pred
    State = handle_set_predecessor(Pred, State0),
    {noreply, State};
handle_cast({set_successors, NewSuccs},
            #state{id=Me, succs=Succs0, succ_list_sz=SLSz}=State0) ->
    SuccsData = insert_successors(NewSuccs, Me, Succs0, SLSz),
    State = handle_set_successor(SuccsData, State0),
    {noreply, State};
handle_cast({update_finger_table, S, I}, State0) ->
    State = handle_update_finger_table(S, I, State0),
    {noreply, State};
handle_cast({debug_state, Level, Start, From}=Cont, #state{id=Id, succs=[Succ|_]}=State) ->
    case Start of
	Id -> gen_server:reply(From, ok);
	_ ->
	    print_state(State, Level),
	    cast(Succ, Cont)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    io:format("~p: Unhandled ~p~n", [?LINE, _Msg]),
    {noreply, State}.


handle_info({'DOWN', _, process, Pid, _} = Msg,
	    #state{id=Id, pred=Pred, succs=Succs0, fingers=Fingers} = State0) ->
    PredPid = get_pid(Pred),
    SuccPid = get_pid(hd(Succs0)),
    LastKnown = if Pred =:= undefined -> Id;
		   PredPid =:= Pid -> Id;
		   true -> Pred
		end,
    UpdFingers = fix_fingers(Pid, LastKnown, Id, lists:reverse(Fingers), []),
    State = State0#state{fingers=UpdFingers},
    if PredPid == Pid, SuccPid == Pid -> %% We are the only left
	    {noreply, State#state{pred=Id, succs=[Id]}};
       PredPid == Pid ->
            Succs = fix_successors(Pid, Succs0, Id, State0#state.succ_list_sz, []),
	    {noreply, State#state{succs=Succs, pred=undefined}};
       SuccPid == Pid ->
	    case handle_dead_successor(State#state{succs=tl(Succs0)}, Pid) of
		error ->
		    self() ! Msg,
		    {noreply, State0#state{fingers=UpdFingers}};
		NewState ->
		    {noreply, NewState}
	    end;
       true ->
            Succs = fix_successors(Pid, Succs0, Id, State0#state.succ_list_sz, []),
	    {noreply, State#state{succs=Succs}}
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

%% Successor list handling
%%--------------------------------------------------------------------

%% update

insert_successors(Ns, #id{}=Me, Ss, Sz) ->
    %io:format("ME: ~s~nIS: ~p~nTO: ~p~n", [print_key(Me),Ns,Ss]),
    {Nss, New} = Res = insert_successors_1(Ns, Me, Ss, Sz, [], []),
    %io:format("   =>~s~n NEW:~s~n~n", [print_keys(Nss), print_keys(New)]),
    Res.

insert_successors_1(_, _, _, Sz, Acc, New) when Sz =< 0 ->
    {lists:reverse(Acc), lists:reverse(New)};
insert_successors_1([Me|Ns], Me, Ss, Sz, Acc, New) ->
    insert_successors_1(Ns, Me, Ss, Sz, Acc, New);
insert_successors_1([Succ|Ns], Me, [Succ|Ss], Sz, Acc, New) ->
    insert_successors_1(Ns, Me, Ss, Sz-1, [Succ|Acc], New);
insert_successors_1([NewSucc|Ns1], Me, [Me|Ss1], Sz, Acc, New) ->
    insert_successors_1(Ns1, Me, Ss1, Sz-1,
                        add_succ(NewSucc,Acc), add_succ(NewSucc,New));
insert_successors_1([#id{key=NewKey}=NewSucc|Ns1]=Ns0,
                    #id{key=MyKey}=Me,
                    [#id{key=SuccKey}=Succ|Ss1]=Ss0, Sz, Acc, New) ->
    case memberNN(NewKey, MyKey, SuccKey) of
        true ->
            insert_successors_1(Ns1, Me, Ss0, Sz-1,
                                add_succ(NewSucc,Acc), add_succ(NewSucc,New));
        false ->
            insert_successors_1(Ns0, Me, Ss1, Sz-1, [Succ|Acc], New)
    end;
insert_successors_1([], Me, [Me|Ss], Sz, [_|_] = Acc, New) ->
    insert_successors_1([], Me, Ss, Sz-1, Acc, New);
insert_successors_1([], Me, [Succ|Ss], Sz, Acc, New) ->
    insert_successors_1([], Me, Ss, Sz-1, [Succ|Acc], New);
insert_successors_1([Succ|Ss], Me, [], Sz, Acc, New) ->
    insert_successors_1(Ss, Me, [], Sz-1, add_succ(Succ,Acc), add_succ(Succ,New));
insert_successors_1([], _, [], _, Acc, New) ->
    {lists:reverse(Acc), lists:reverse(New)}.

% update messages

handle_set_successor({Succs, []}, #state{succs=Succs}=State) ->
    State;
handle_set_successor({[Succ|_]=Succs, New},
                     #state{id=Me, pred=Pred, fingers=[F1|Fs0], succs=Old}=State) ->
    [setup_monitor(NewSucc) || NewSucc <- New],
%    io:format("~s: UPDATE SUCCS ~s => ~s~n",
%	      [print_key(Me), print_key(hd(Succs)), print_key(NewSucc)]),
    case Old of
        [Succ|_] -> ok;
        _Changed -> cast(Succ, {set_predecessor, Me})
    end,
    cast(Pred, {set_successors, Succs}),
    Fs = update_fingers([F1#finger{node=Succ}|Fs0], Succs, Pred, 1),
    State#state{succs=Succs, fingers=Fs}.

update_fingers([#finger{node=Succ}=F1|Fs], [Succ|_]=Succs, Pred, Index) ->
    [F1|update_fingers(Fs, Succs, Pred, Index+1)];
update_fingers([F|Fs]=Fs0, [Succ|Succs]=Succs0, Pred, Index) ->
    case update_finger(Succ, F) of
        true  ->
            cast(Pred, {update_finger_table, Succ, Index}),
            [F#finger{node=Succ}|update_fingers(Fs, Succs0, Pred, Index+1)];
        false ->
            update_fingers(Fs0, Succs, Pred, Index)
    end;
update_fingers([], _, _, _) -> [];
update_fingers([F|Fs], [], _, _) ->
    [F|Fs].


%% update / handle DOWN

handle_dead_successor(#state{id=Id, pred=Pred, succs=Succs, fingers=Fingers}=State, Pid) ->
    case next_succ(Pid, Id, Pred, Succs, Fingers) of
	Id ->
            %%   io:format("~p: ~p~n",[?LINE,Id]),
	    State#state{pred=Id, succs=[Id]};
	Next ->
	    try
                cast(Next, {set_predecessor, Id}),
                NewSuccs = insert_successors([Next], Id, Succs, State#state.succ_list_sz),
                handle_set_successor(NewSuccs, State)
	    catch exit:_ -> error
	    end
    end.

next_succ(Pid, Id, Pred, Succs, Fingers) ->
    case next_succ(Succs, Fingers, Pid) of
        Id when Pred =:= undefined -> Id;
        Id -> Pred;
        Other -> Other
    end.

next_succ([], Fingers, Pid) ->
    next_succ(Fingers, Pid);
next_succ([Next|_], _, _) ->
    Next.

next_succ([#finger{node=#id{pid=Pid}}|Fingers], Pid) ->
    next_succ(Fingers, Pid);
next_succ([#finger{node=Next}|_], _) ->
    Next.

fix_successors(_Pid, [], _Me, _Sz, Acc) ->
    lists:reverse(Acc);
fix_successors(Pid, [#id{pid=Pid}|Succs], Me, Sz, Acc) ->
    Ask = lists:reverse(Succs, Acc),
    spawn_link(fun() -> update_successors(Ask, Me, Sz) end),
    lists:reverse(Acc, Succs);
fix_successors(Pid, [Succ|Succs], Me, Sz, Acc) ->
    fix_successors(Pid, Succs, Me, Sz, [Succ|Acc]).

update_successors([Me|Ask], Me, ListSz) ->
    update_successors(Ask, Me, ListSz);
update_successors([Ask|Rest], Me, ListSz) ->
    %%find_successors(Me, Me, ListSz),
    try call(Ask, get_successors) of
        Succs ->
            cast(Me, {set_successors, Succs})
    catch _:_R ->
            update_successors(Rest, Me, ListSz)
    end;
update_successors([], _Me, _) -> normal.


%% Predecessor handling
%%--------------------------------------------------------------------
set_predecessor(Id, Id) -> myself;
set_predecessor(Succ, Id) ->
    case call(Succ, {set_predecessor, Id}) of
        {ok, Pred, Succs} ->
            {Pred, [Succ|Succs]};
        {error, NewSucc} ->
            set_predecessor(NewSucc, Id)
    end.

handle_set_predecessor(Pred, #state{id=Id, succs=Succs}=State0) ->
    case set_predecessor_impl(Pred, State0) of
	{{ok, undefined, _Succs}, State} ->
	    cast(Pred, {set_successors, add_succ(Id, Succs)}),
	    State;
	{{ok, PrevPred, _Succs}, State} ->
	    cast(PrevPred, {set_successors, add_succ(Pred,add_succ(Id,Succs))}),
            cast(Pred, {set_successors, [Id|Succs]}),
	    State;
	{{error, _}, #state{pred=Orig}} -> %% Redir Pred's successor
	    io:format("Redir: ~p succs ~p~n", [Pred, Orig]),
	    cast(Pred, {set_successors, add_succ(Orig,add_succ(Id,Succs))}),
	    State0
    end.

add_succ(Id, Succs) ->
    case lists:member(Id, Succs) of
        true  -> Succs;
        false -> [Id|Succs]
    end.

set_predecessor_impl(Pred, #state{pred=Pred, succs=Succs}=State) ->
    {{ok, Pred, Succs}, State};
set_predecessor_impl(NewPred, #state{pred=undefined=OldPred, succs=Succs}=State) ->
    setup_monitor(NewPred),
    {{ok, OldPred, Succs}, State#state{pred=NewPred}};
set_predecessor_impl(NewPred, #state{id=Id, pred=OldPred, succs=Succs}=State) ->
    case is_process_alive(get_pid(OldPred)) of
        true ->
            case memberIN(NewPred, OldPred, Id) of
                true ->
                    setup_monitor(NewPred),
                    {{ok, OldPred, Succs}, State#state{pred=NewPred}};
                false ->
                    {{error, OldPred}, State}
            end;
        false ->
            setup_monitor(NewPred),
            {{ok, undefined, Succs}, State#state{pred=NewPred}}
    end.

find_predecessor_impl(Id, #state{id=This, succs=[Succ|_], fingers=Fingers}) ->
    case memberNI(Id, This#id.key, Succ#id.key) of
	true ->
	    {ok, This, Succ};
	false ->
	    %% [io:put_chars(print_finger(F)) || F <- Fingers],
	    case closest_preceding_fingers(This, Id, Fingers) of
                This when Succ =:= This -> {ok, This, Succ};
                This -> {cont, Succ};
		Next -> {cont, Next}
	    end
    end.

%% Finger table handling
%%--------------------------------------------------------------------

%% init

make_fingers(#id{key=N} = Id, KeyBSZ) ->
    Start = [(N + (1 bsl (K-1))) rem ?KEY_SIZE(KeyBSZ) ||
		K <- lists:seq(1, KeyBSZ)],
    make_fingers(Start, N, Id).

make_fingers([Start|[Next|_]=Rest], Last, Id) ->
    [#finger{start=Start, last=Next, node=Id}|make_fingers(Rest,Last,Id)];
make_fingers([Start], Last, Id) ->
    [#finger{start=Start, last=Last, node=Id}].

init_fingers([#finger{start=Start}=F|Fs], N, #id{key=NodeId}=Prev, [Gate|Gates]) ->
    case memberIN(Start, N, NodeId) of
	true  ->
	    setup_monitor(Prev),
	    [F#finger{node=Prev}|init_fingers(Fs, N, Prev, [Gate|Gates])];
	false ->
            try find_successor(Gate, Start) of
                {_, Succ} ->
                    setup_monitor(Succ),
                    [F#finger{node=Succ}|init_fingers(Fs, N, Succ, [Gate|Gates])]
            catch _:_Reason ->
                %io:format("Retry after _Reason in init_fingers: ~p~n", [_Reason]),
                case is_process_alive(Gate) of
                    true -> init_fingers([F|Fs], N, Prev, [Gate|Gates]);
                    false -> init_fingers([F|Fs], N, Prev, Gates)
                end
            end
    end;
init_fingers([], _, _, _) -> [];
init_fingers(Fs, N, Prev, []) ->
    case is_process_alive(Prev#id.pid) of
        true -> init_fingers(Fs, N, Prev, [Prev#id.pid]);
        false -> exit(failed_init_fingers)
    end.

update_others(#id{key=Key}=Id, I, KeyBSZ)
  when I =< KeyBSZ ->
    KeySize = ?KEY_SIZE(KeyBSZ),
    Prev = (KeySize + Key - (1 bsl (I-1)) + 1) rem KeySize,
    try find_predecessor(Id, Prev) of
        {Pred, _} ->
            %io:format("UPD ~p (~p) => Pred ~p ~s~n", [I, Key, Prev, print_key(Pred)]),
            cast(Pred, {update_finger_table, Id, I}),
            update_others(Id, I+1, KeyBSZ)
    catch _:_Reason ->
        %io:format("Retry after _Reason in update_others: ~p~n", [_Reason]),
        update_others(Id, I, KeyBSZ)
    end;
update_others(_, _, _) -> ok.

check_fingers([F|Fs], Id) ->
    spawn_link(fun() -> fix_finger(Id, F, length([F|Fs])) end),
    check_fingers(Fs, Id);
check_fingers([], _Id) ->
    ok.

%% lookup

closest_preceding_fingers(#id{key=This}=N, Id, Fingers) ->
    Check = fun(#finger{node=#id{key=Key}}) ->
		    %% io:format("mem ~p < ~p < ~p~n",[This, Key, Id]),
		    not memberNN(Key, This, Id)
	    end,
    case lists:dropwhile(Check, lists:reverse(Fingers)) of
	[] -> N;
	[#finger{node=Node}|_] -> Node
    end.

%% update / handle DOWN

fix_fingers(Pid, Last, Me, [#finger{node=#id{pid=Pid}}=F1|Fingers], Acc) ->
    spawn_link(fun() -> fix_finger(Me, F1, length([F1|Fingers])) end),
    case Acc of
	[] ->
            %io:format("~s:* dead ~s => ~s~n", [print_key(Me), print_finger(F1), print_key(Last)]),
	    fix_fingers(Pid, Last, Me, Fingers, [F1#finger{node=Last}|Acc]);
	[#finger{node=Next}|_] ->
            %io:format("~s: dead ~s => ~s~n", [print_key(Me), print_finger(F1), print_key(Next)]),
	    fix_fingers(Pid, Last, Me, Fingers, [F1#finger{node=Next}|Acc])
    end;
fix_fingers(Pid, Last, Me, [F1|Fingers], Acc) ->
    fix_fingers(Pid, Last, Me, Fingers, [F1|Acc]);
fix_fingers(_Pid, _, _Me, [], Acc) -> Acc.

fix_finger(Me, #finger{start=Start}=F, I) ->
    case catch find_successor(Me, Start) of
        {'EXIT', _} ->
            fix_finger(Me, F, I);
        {_Pred, Succ} ->
            %io:format("Fix: ~s ~p ~p ~s From ~s~n",[print_key(Me), I, Start, print_key(Succ),print_key(_Pred)]),
            cast(Me, {update_finger_table, Succ, I})
    end.

%% update messages

handle_update_finger_table(Id, Indx, #state{id=Me, succs=[Orig|_]=Succs0} = State) ->
    case update_finger(Id, Indx, State) of
        [#finger{node=Orig}|_] = Fingers ->
            State#state{fingers=Fingers};
        [#finger{node=Succ}|_] = Fingers ->
            %%    io:format("~s: UPDATE SUCCS2 ~s => ~s~n",
            %%	      [print_key(State#state.id), print_key(_Orig), print_key(Succ)]),
            SuccsData = insert_successors([Succ], Me, Succs0, State#state.succ_list_sz),
            handle_set_successor(SuccsData, State#state{fingers=Fingers})
    end.

update_finger(S, I, #state{id=Id, fingers=Fingers0, pred=Pred}) ->
    {Part1, [F0|Part2]} = lists:split(I-1, Fingers0),
    case update_finger(S, F0) of
        true ->
            setup_monitor(S),
            (Id =/= Pred) andalso cast(Pred, {update_finger_table, S, I}),
            Part1 ++ [F0#finger{node=S}|Part2];
        false ->
            Fingers0
    end.

update_finger(_, #finger{start=Start, node=#id{key=Start}}) ->
    false;
update_finger(#id{key=SKey}, #finger{start=Start, node=#id{key=Node}}) ->
    memberIN(SKey, Start, Node).


%% init functions
%%--------------------------------------------------------------------
init_neighbors([Gate|Gates], #state{id=Id, fingers=[F|Fs0]}=State0) ->
    case catch find_successors(Gate, Id, State0#state.succ_list_sz) of
	[#id{}=Succ0|_] = Succs0 ->
	    Fs = init_fingers(Fs0, Id#id.key, Succ0, [Gate|Gates]),
	    try set_predecessor(Succ0, Id) of
		myself ->
		    init_neighbors([Gate|Gates], State0);
		{Pred, Succs1} ->
		    setup_monitor(Pred),
                    {Succs,_} = insert_successors(Succs1, Id, Succs0, State0#state.succ_list_sz),
		    [setup_monitor(S) || S <- Succs],
                    Fingers = [F#finger{node=hd(Succs)}|Fs],
                    update_pred_fingers(Pred, Fingers, 1),
		    State1 = State0#state{pred=Pred, succs=Succs},
		    check_fingers(lists:reverse([F#finger{node=hd(Succs)}|Fs]), Id),
		    spawn_link(fun() -> update_others(Id, 1, State0#state.key_bit_sz) end),
                    spawn_link(fun() -> update_successors(Succs, Id, State1#state.succ_list_sz) end),
		    State1#state{fingers=Fingers}
	    catch _:{noproc, _} ->
		    init_neighbors([Gate|Gates], State0)
	    end;
	_Error ->
            io:format("Retry after _Error in init_neighbors: ~p~n", [_Error]),
            case is_process_alive(Gate) of
                true -> init_neighbors([Gate|Gates], State0);
                false -> init_neighbors(Gates, State0)
            end
    end;
init_neighbors(_Gs, State) ->
    State.

update_pred_fingers(_, [], _) ->
    ok;
update_pred_fingers(Pred, [#finger{node=Node}|Fs], I) ->
    cast(Pred, {update_finger_table, Node, I}),
    update_pred_fingers(Pred, Fs, I+1).

%% utility functions
%%--------------------------------------------------------------------

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

print_state(#state{id=This, pred=Pred, succs=Succs}, ring) ->
    io:format("~s <- ~s -> [~s]~n", [print_key(Pred), print_key(This), lists:flatten([print_key(Succ) || Succ <- Succs])]);
print_state(#state{id=This, pred=Pred, succs=Succs, fingers=Fingers}, _) ->
    io:format("~nNode ~s <- ~s -> [~s]~n", [print_key(Pred), print_key(This), lists:flatten([print_key(Succ) || Succ <- Succs])]),
    io:format("Fs: ", []),
    Print = fun(F, Acc0) ->
		    IO = print_finger(F),
		    Len0 = length(IO),
		    Len = Acc0 + length(IO),
		    Acc = if Len > 85 -> io:format("~n    "), Len0;
			     true -> Len
			  end,
		    io:put_chars(IO),
		    Acc
	    end,
    lists:foldl(Print, 0, Fingers),
    io:format("~n").

print_finger(#finger{start=Start, last=Last, node=Node}) ->
    lists:flatten(io_lib:format(" {~.5w ~.5w ~s} ",[Start, Last, print_key(Node)])).

print_keys(List) ->
    [print_key(Key) || Key <- List].

print_key(undefined) -> "undefined";
print_key(#id{key=Key, pid=Pid}) ->
    io_lib:format(" {~.3p,~p} ", [Key,Pid]).

