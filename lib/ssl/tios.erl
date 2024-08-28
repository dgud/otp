-module(tios).

-compile([export_all, nowarn_export_all]).

-define(PORT, 5678).

test() ->
    Me = self(),
    logger:set_module_level([tcp_iostream], info),
    Server = spawn(fun() -> server(Me) end),
    receive {sync, Server} -> ok end,
    client(gen_tcp),
    client(tcp_iostream),
    client_active(gen_tcp),
    Server ! close,
    ok.


server(Sync) ->
    Listen = catch gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("server: ~p listen ~p ~n", [self(), Listen]),
    {ok, LSock} = Listen,
    Sync ! {sync, self()},
    listen_loop(LSock),
    ok = gen_tcp:close(LSock),
    io:format("server: listen close~n", []).

listen_loop(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("server: connected~n", []),
    {ok, Bin} = echo_server(Sock, []),
    ok = gen_tcp:close(Sock),
    io:format("server: socket close ~p~n", [Bin]),
    receive
        close -> ok
    after 500 ->
            listen_loop(LSock)
    end.

echo_server(Sock, Bs) ->
    case iolist_size(Bs) > 5 of
        true ->
            io:format("server: -> Send ~p~n", [Bs]),
            gen_tcp:send(Sock, Bs),
            echo_server(Sock, []);
        false ->
            case gen_tcp:recv(Sock, 0) of
                {ok, B} ->
                    io:format("server: <- Got ~p~n", [B]),
                    echo_server(Sock, [Bs, B]);
                {error, closed} ->
                    {ok, list_to_binary(Bs)}
            end
    end.

client(Mod) ->
    Host  = {127,0,0,1},
    {ok, Sock} = Mod:connect(Host, ?PORT, [binary, {packet, 0}, {active, false}]),
    io:format("~p PASSIVE client: connected ~p~n", [Mod, Sock]),
    try
        Mod:send(Sock, <<"Hej ">>),
        io:format("~p client: -> send Hej~n", [Mod]),
        {error, timeout} = Mod:recv(Sock, 0, 500),
        {error, timeout} = Mod:recv(Sock, 5, 500),
        io:format("~p client: wait~n", [Mod]),
        Mod:send(Sock, <<"foobar">>),
        io:format("~p client: -> send foobar~n", [Mod]),
        {ok, Packets1} = Mod:recv(Sock, 10),
        io:format("~p client: -> Got ~ts~n", [Mod, Packets1])
    catch Execp:Reason:St ->
            io:format("~p Client: ~p:~p~n  ~p~n",[Mod, Execp, Reason, St]),
            error
    after
        io:format("~p client: FLUSH: ~p~n", [Mod, flush()]),
        _ = Mod:close(Sock)
    end.


client_active(Mod) ->
    Host  = {127,0,0,1},
    {ok, Sock} = Mod:connect(Host, ?PORT, [binary, {packet, 0}, {active, true}]),
    io:format("~n~p ACTIVE client: connected ~p~n~n", [Mod, Sock]),
    try
        Mod:send(Sock, <<"Hej ">>),
        io:format("~p client: -> send Hej~n", [Mod]),
        [] = flush(),
        io:format("~p client: wait~n", [Mod]),
        Mod:send(Sock, <<"foobar">>),
        io:format("~p client: -> send foobar~n", [Mod]),
        timer:sleep(500),
        Packets1 = flush(),
        io:format("~p client: -> Got: ~tp~n", [Mod, Packets1])
    catch Execp:Reason:St ->
            io:format("~p Client: ~p:~p~n  ~p~n",[Mod, Execp, Reason, St]),
            error
    after
        io:format("~p client: FLUSH: ~p~n", [Mod, flush()]),
        _ = Mod:close(Sock)
    end.

flush() ->
    receive
        {tcp, _Socket, Msg} ->
            [Msg|flush()];
        {tcp_iostream, _Socket, Stream} ->
            iostream:notify(Stream),
            {[], Data} = iostream:read(Stream, 0),
            [Data|flush()];
        Msg ->
            [Msg|flush()]
    after 50 ->
            []
    end.
