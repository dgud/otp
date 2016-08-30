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
-export([print_ring/1, print_state/1, print_state/2]).
-compile(export_all).

-define(KEY_SIZE(BIT_SIZE), (1 bsl (BIT_SIZE))).

-record(id, {key::range(), pid::pid()}).
-record(finger, {start::range(), last::range(), node::id()}).
-record(state, {id :: id(),
		pred :: id(),
		succs=[] :: [id()],
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
    Res = {Pred, Succ} = find_predecessor(Gate, Id),
    case call(Pred, get_successor) of %% Assert
	Succ -> Res;
	Changed -> {Pred, Changed}
    end.

find_predecessor(Gate, Id) ->
    case call(Gate, {find_predecessor, Id}) of
	{ok, Pred, Succ} -> {Pred, Succ};
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
    {ok, init_neighbors(Gates, #state{id=Id, pred=Id, succs=[Id], fingers=Fingers0, key_bit_sz=KeyBSZ})}.


handle_call({set_predecessor, Pred}, _From, State0) ->
    {Res, State} = set_predecessor_impl(Pred, State0),
    {reply, Res, State};

handle_call(get_successor, _From, #state{succs=[Succ|_]}=State) ->
    {reply, Succ, State};
handle_call({find_predecessor, Id}, _From, State) ->
    Reply = find_predecessor_impl(Id, State),
    {reply, Reply, State};
handle_call({debug_state, Level}, From, #state{id=Id, succs=[Succ|_]}=State) ->
    io:format("~n*****************~n"),
    print_state(State, Level),
    cast(Succ, {debug_state, Level, Id, From}),
    {noreply, State}.

handle_cast({set_predecessor, Pred}, State0) -> %% This always comes from the Pred
    case set_predecessor_impl(Pred, State0) of
	{{ok, undefined}, State} ->
            cast(Pred, {set_successor, State#state.id}),
	    {noreply, State};
	{{ok, PrevPred}, State} ->
            cast(PrevPred, {set_successor, Pred}),
	    cast(Pred, {set_successor, State#state.id}),
	    {noreply, State};
	{{error, _}, #state{pred=Orig}} -> %% Redir Pred's successor
	    io:format("Redir: ~p succs ~p~n", [Pred, Orig]),
	    cast(Pred, {set_successor, Orig}),
	    {noreply, State0}
    end;
handle_cast({set_successor, Me}, #state{id=Me}=State) ->
    {noreply, State};
handle_cast({set_successor, Succ}, #state{succs=[Succ|_]}=State) ->
    {noreply, State};
handle_cast({set_successor, #id{key=NewKey}=NewSucc},
	    #state{id=#id{key=MeKey}=Me, succs=[#id{key=SuccKey}=_Orig|_],fingers=[F1|Fs]}=State) ->
    case memberIN(NewKey, MeKey, SuccKey) of
	true ->
	    setup_monitor(NewSucc),
	    io:format("~s: UPDATE SUCCS ~s => ~s~n",
		      [print_key(Me), print_key(_Orig), print_key(NewSucc)]),
	    cast(NewSucc, {set_predecessor, Me}),
	    {noreply, State#state{succs=[NewSucc], fingers=[F1#finger{node=NewSucc}|Fs]}};
	false ->
	    io:format("Set succ failed: ~p (~p)succ ~p~n", [Me, _Orig, NewSucc]),
	    {noreply, State}
    end;
handle_cast({update_finger_table, Id, _I}, #state{id=Id}=State) ->
    %% My own state is already correct
    {noreply, State};
handle_cast({update_finger_table, S, I}, State) ->
    Fingers = update_finger_table(S, I, State),
    case I of
	1 ->
	    [#finger{node=Succ}|_] = Fingers,
	    case hd(State#state.succs) of
		Succ ->
		    {noreply, State#state{fingers=Fingers}};
		_Orig ->
		    io:format("~s: UPDATE SUCCS2 ~s => ~s~n",
			      [print_key(State#state.id), print_key(_Orig), print_key(Succ)]),
		    cast(Succ, {set_predecessor, State#state.id}),
		    {noreply, State#state{succs=[Succ], fingers=Fingers}}
	    end;
	_ ->
	    {noreply, State#state{fingers=Fingers}}
    end;
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
	    #state{id=Id, pred=Pred, succs=Succs, fingers=Fingers} = State) ->
    PredPid = get_pid(Pred),
    SuccPid = get_pid(hd(Succs)),
    LastKnown = if Pred =:= undefined -> Id;
		   PredPid =:= Pid -> Id;
		   true -> Pred
		end,
    UpdFingers = fix_fingers(Pid, LastKnown, Id, lists:reverse(Fingers), []),
    if PredPid == Pid, SuccPid == Pid -> %% We are the only left
	    {noreply, State#state{pred=Id, succs=[Id], fingers=UpdFingers}};
       PredPid == Pid ->
	    {noreply, State#state{pred=undefined, fingers=UpdFingers}};
       SuccPid == Pid ->
	    case handle_dead_successor(State#state{fingers=UpdFingers}, Pid) of
		error ->
		    self() ! Msg,
		    {noreply, State#state{fingers=UpdFingers}};
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

handle_dead_successor(#state{id=Id, pred=Pred, fingers=[F0|Fs]=Fingers}=State, Pid) ->
    Next = case next_succ(Fingers, Pid) of
	       Id    -> Pred;
	       Other -> Other
	   end,
    case Next of
	undefined ->
%	    io:format("~p: ~p~n",[?LINE,Id]),
	    State#state{pred=Id, succs=[Id]};
	_ ->
	    try set_predecessor(Next, Id) of
		myself ->
%		    io:format("~p: ~p~n",[?LINE,Id]),
		    State#state{succs=[Id]};
		{OldPred, #id{}=Succ} ->
		    io:format("~s: UPDATE dead SUCCS ~s => ~s~n",
			      [print_key(State#state.id), print_key(hd(State#state.succs)), print_key(Succ)]),
		    case OldPred of
			undefined -> ok;
			Id -> ok;
			_  -> cast(OldPred, {set_successor, Id})
		    end,
		    State#state{succs=[Succ], fingers=[F0#finger{node=Succ}|Fs]}
	    catch exit:_ -> error
	    end
    end.

make_fingers(#id{key=N} = Id, KeyBSZ) ->
    Start = [(N + (1 bsl (K-1))) rem ?KEY_SIZE(KeyBSZ) ||
		K <- lists:seq(1, KeyBSZ)],
    make_fingers(Start, N, Id).

make_fingers([Start|[Next|_]=Rest], Last, Id) ->
    [#finger{start=Start, last=Next, node=Id}|make_fingers(Rest,Last,Id)];
make_fingers([Start], Last, Id) ->
    [#finger{start=Start, last=Last, node=Id}].

init_neighbors([Gate|Gates], #state{id=Id, fingers=[#finger{start=Start}=F|Fingers]}=State0) ->
    case catch find_successor(Gate, Start) of
	{_, #id{}=Succ0} ->
	    Fs = init_fingers(Fingers, Id#id.key, Succ0, [Gate|Gates]),
	    try set_predecessor(Succ0, Id) of
		myself ->
		    init_neighbors([Gate|Gates], State0);
		{Pred, Succ} ->
		    setup_monitor(Pred),
		    setup_monitor(Succ),
		    State1 = State0#state{pred=Pred, succs=[Succ]},
		    update_fingers(lists:reverse([F#finger{node=Succ}|Fs]), Id),
		    spawn_link(fun() -> update_others(Id, 1, State0#state.key_bit_sz) end),
		    State1#state{fingers=[F#finger{node=Succ}|Fs]}
	    catch _:{noproc, _} ->
		    init_neighbors([Gate|Gates], State0)
	    end;
	_Error ->
            %io:format("Retry after _Error in init_neighbors: ~p~n", [_Error]),
            case is_process_alive(Gate) of
                true -> init_neighbors([Gate|Gates], State0);
                false -> init_neighbors(Gates, State0)
            end
    end;
init_neighbors(_Gs, State) ->
    State.

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

update_fingers([F|Fs], Id) ->
    spawn_link(fun() -> fix_finger(Id, F, length([F|Fs])) end),
    update_fingers(Fs, Id);
update_fingers([], _Id) ->
    ok.

fix_fingers(Pid, Last, Me, [#finger{node=#id{pid=Pid}}=F1|Fingers], Acc) ->
    spawn_link(fun() -> fix_finger(Me, F1, length([F1|Fingers])) end),
    case Acc of
	[] ->
	    fix_fingers(Pid, Last, Me, Fingers, [F1#finger{node=Last}|Acc]);
	[#finger{node=Next}|_] ->
	    fix_fingers(Pid, Last, Me, Fingers, [F1#finger{node=Next}|Acc])
    end;
fix_fingers(Pid, Last, Me, [F1|Fingers], Acc) ->
    fix_fingers(Pid, Last, Me, Fingers, [F1|Acc]);
fix_fingers(_Pid, _, _Me, [], Acc) -> Acc.

fix_finger(Me, #finger{start=Start}=F, I) ->
    case catch find_successor(Me, Start) of
        {'EXIT', _} ->
            case is_process_alive(get_pid(Me)) of
                true ->
                    fix_finger(Me, F, I); %TODO should we spin here?
                false ->
                    ok
            end;
        {_, Succ} ->
            cast(Me, {update_finger_table, Succ, I})
    end.

find_predecessor_impl(Id, #state{id=This, succs=[Succ|_], fingers=Fingers}) ->
    case memberNI(Id, This#id.key, Succ#id.key) of
	true ->
	    {ok, This, Succ};
	false ->
	    %% [io:put_chars(print_finger(F)) || F <- Fingers],
	    case closest_preceding_fingers(This, Id, Fingers) of
		This -> {ok, This, Succ};
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

set_predecessor(Id, Id) -> myself;
set_predecessor(Succ, Id) ->
    case call(Succ, {set_predecessor, Id}) of
        {ok, Pred} ->
            {Pred, Succ};
        {error, NewSucc} ->
            set_predecessor(NewSucc, Id)
    end.

set_predecessor_impl(Pred, #state{pred=Pred}=State) ->
    {{ok, Pred}, State};
set_predecessor_impl(NewPred, #state{pred=undefined=OldPred}=State) ->
    setup_monitor(NewPred),
    {{ok, OldPred}, State#state{pred=NewPred}};
set_predecessor_impl(NewPred, #state{id=Id, pred=OldPred}=State) ->
    case is_process_alive(get_pid(OldPred)) of
        true ->
            case memberIN(NewPred, OldPred, Id) of
                true ->
                    setup_monitor(NewPred),
		    %%TODO demonitor OldPred
                    {{ok, OldPred}, State#state{pred=NewPred}};
                false ->
                    {{error, OldPred}, State}
            end;
        false ->
            setup_monitor(NewPred),
            {{ok, undefined}, State#state{pred=NewPred}}
    end.

next_succ([#finger{node=#id{pid=Pid}}|Fingers], Pid) ->
    next_succ(Fingers, Pid);
next_succ([#finger{node=Next}|_], _) ->
    Next.

update_others(#id{key=Key, pid=Self}=Id, I, KeyBSZ)
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
        case is_process_alive(Self) of
            true -> update_others(Id, I, KeyBSZ);
            false -> ok
        end
    end;
update_others(_, _, _) -> ok.

update_finger_table(#id{key=SKey}=S, I,
		    #state{id=#id{key=N}=Id, fingers=Fingers0, pred=Pred}) ->
    {Part1, [F0|Part2]} = lists:split(I-1, Fingers0),
    #finger{node=#id{key=Node}} = F0,
    case memberIN(SKey, N, Node) of
	false ->
	    Fingers0;
	true ->
	    setup_monitor(S),
	    (Id =/= Pred) andalso cast(Pred, {update_finger_table, S, I}),
	    Part1 ++ [F0#finger{node=S}|Part2]
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

print_state(#state{id=This, pred=Pred, succs=[Succ|_]}, ring) ->
    io:format("~s <- ~s -> ~s~n", [print_key(Pred), print_key(This), print_key(Succ)]);
print_state(#state{id=This, pred=Pred, succs=[Succ|_], fingers=Fingers}, _) ->
    io:format("~nNode ~s <- ~s -> ~s~n", [print_key(Pred), print_key(This), print_key(Succ)]),
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

print_key(undefined) -> "undefined";
print_key(#id{key=Key, pid=Pid}) ->
    io_lib:format("{~.3p,~p}", [Key,Pid]).

