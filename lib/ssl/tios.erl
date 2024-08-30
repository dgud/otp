-module(tios).

-compile([export_all, nowarn_export_all]).

-define(PORT, 5678).

test() ->
    Me = self(),
    logger:set_module_level([tcp_iostream], debug),
    Server = spawn(fun() -> server(Me) end),
    receive {sync, Server} -> ok end,
    maybe
        ok ?= client(gen_tcp),
        ok ?= client(tcp_iostream),
        ok ?= client_active(gen_tcp),
        ok ?= client_active(tcp_iostream),
        ok ?= client_active_once(gen_tcp),
        ok ?= client_active_once(tcp_iostream),
        ok ?= client_active_n(gen_tcp),
        ok ?= client_active_n(tcp_iostream),
        ok
    end,
    Server ! close,
    ok.

ssl_test() ->
    CB = {cb_info, {tcp_iostream, tcp_iostream_closed, tcp_iostream_error, tcp_iostream_passive}},
    ssl:start(),
    {ok, Sock} = ssl:connect("google.com", 443,
                             [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}, CB]),
    ssl:close(Sock).



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
    io:format("~n~p PASSIVE client: connected ~p~n", [Mod, Sock]),
    try
        Mod:send(Sock, <<"Hej ">>),
        io:format("~p client: -> send Hej~n", [Mod]),
        {error, timeout} = Mod:recv(Sock, 0, 500),
        {error, timeout} = Mod:recv(Sock, 5, 500),
        io:format("~p client: wait~n", [Mod]),
        Mod:send(Sock, <<"foobar">>),
        io:format("~p client: -> send foobar~n", [Mod]),
        {ok, Packets1} = Mod:recv(Sock, 10),
        verify(~"Hej foobar", [iolist_to_binary(Packets1)], Mod),
        ok
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
    io:format("~n~p ACTIVE client: connected ~p~n", [Mod, Sock]),
    try
        Mod:send(Sock, <<"Hej ">>),
        io:format("~p client: -> send Hej~n", [Mod]),
        [] = flush(),
        io:format("~p client: wait~n", [Mod]),
        Mod:send(Sock, <<"foobar">>),
        io:format("~p client: -> send foobar~n", [Mod]),
        Packets1 = flush(),
        verify(~"Hej foobar", Packets1, Mod),
        Mod:send(Sock, <<"Next packet">>),
        io:format("~p client: -> send Next packet~n", [Mod]),
        Packets2 = flush(),
        verify(~"Next packet", Packets2, Mod),
        ok
    catch Execp:Reason:St ->
            io:format("~p Client: ~p:~p~n  ~p~n",[Mod, Execp, Reason, St]),
            error
    after
        io:format("~p client: FLUSH: ~p~n", [Mod, flush()]),
        _ = Mod:close(Sock)
    end.


client_active_once(Mod) ->
    Host  = {127,0,0,1},
    {ok, Sock} = Mod:connect(Host, ?PORT, [binary, {packet, 0}, {active, false}]),
    io:format("~n~p ACTIVE ONCE client: connected ~p~n", [Mod, Sock]),
    try
        Mod:send(Sock, <<"Hej ">>),
        io:format("~p client: -> send Hej~n", [Mod]),
        [] = flush(),
        setopts(Mod, Sock, [{active, once}]),
        [] = flush(),
        io:format("~p client: wait~n", [Mod]),
        Mod:send(Sock, <<"foobar">>),
        io:format("~p client: -> send foobar~n", [Mod]),
        Packets1 = flush(),
        verify(~"Hej foobar", Packets1, Mod),
        Mod:send(Sock, <<"Next packet">>),
        io:format("~p client: -> send Next packet~n", [Mod]),
        [] = flush(),
        setopts(Mod, Sock, [{active, once}]),
        Packets2 = flush(),
        verify(~"Next packet", Packets2, Mod),
        setopts(Mod, Sock, [{active, once}]),
        timer:sleep(200),
        ok
    catch Execp:Reason:St ->
            io:format("~p Client: ~p:~p~n  ~p~n",[Mod, Execp, Reason, St]),
            error
    after
        io:format("~p client: FLUSH: ~p~n", [Mod, flush()]),
        _ = Mod:close(Sock)
    end.

client_active_n(Mod) ->
    Host  = {127,0,0,1},
    {ok, Sock} = Mod:connect(Host, ?PORT, [binary, {packet, 0}, {active, false}]),
    io:format("~n~p ACTIVE N client: connected ~p~n", [Mod, Sock]),
    try
        Mod:send(Sock, <<"Hej ">>),
        io:format("~p client: -> send Hej~n", [Mod]),
        [] = flush(),
        setopts(Mod, Sock, [{active, 3}]),
        [] = flush(),
        io:format("~p client: wait~n", [Mod]),
        Mod:send(Sock, <<"foobar">>),
        io:format("~p client: -> send foobar~n", [Mod]),
        Packets1 = flush(),
        verify(~"Hej foobar", Packets1, Mod),
        Mod:send(Sock, <<"Next packet">>),
        io:format("~p client: -> send Next packet~n", [Mod]),
        Packets2 = flush(),
        verify(~"Next packet", Packets2, Mod),
        Mod:send(Sock, <<"Last packet">>),
        io:format("~p client: -> send Last packet~n", [Mod]),
        Packets3 = flush(),
        Rest = verify(~"Last packet", Packets3, Mod),
        verify(passive, Rest, Mod),
        [] = flush(),
        ok
    catch Execp:Reason:St ->
            io:format("~p Client: ~p:~p~n  ~p~n",[Mod, Execp, Reason, St]),
            error
    after
        io:format("~p client: FLUSH: ~p~n", [Mod, flush()]),
        _ = Mod:close(Sock)
    end.

setopts(gen_tcp,  Sock, Opts) ->
    inet:setopts(Sock, Opts);
setopts(Mod, Sock, Opts) ->
    ok = Mod:setopts(Sock, Opts).

flush() ->
    receive
        {tcp, _Socket, Msg} ->
            [Msg|flush()];
        {tcp_passive, _Socket} ->
            [passive|flush()];
        {tcp_iostream, _Socket, Stream} ->
            {[], Data} = iostream:read(Stream, all),
            iostream:notify(Stream),
            [iolist_to_binary(Data)|flush()];
        {tcp_iostream_passive, _Socket} ->
            [passive|flush()];
        Msg ->
            [Msg|flush()]
    after 300 ->
            []
    end.


verify(MsgBin, All, Mod) ->
    case All of
        [MsgBin|Rest] ->
            io:format("~p client: -> Got ~ts~n", [Mod, MsgBin]),
            Rest;
        All ->
            io:format("~p client: -> Got ~p expected [~p|_] ~n", [Mod, All, MsgBin]),
            exit({fail, All})
    end.
