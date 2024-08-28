%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(tcp_iostream).
-moduledoc """
""".

-export([connect/2, connect/3, connect/4,
         %% listen/2,
         %% accept/1, accept/2,
	 shutdown/2, close/1]).
-export([send/2, recv/2, recv/3]). %%, unrecv/2]).
-export([controlling_process/2]).

-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").
%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(reader,
        {
         from :: pid(),
         read_ahead :: non_neg_integer(),  %% No of packets, similar to active n
         length :: non_neg_integer(),
         handle :: undefined | socket:select_handle() | socket:completion_handle()
        }).

-record(state,
        {
         role :: 'client' | 'server',
         pid :: pid(),
         read :: iostream:handle(),
         write :: iostream:handle(),
         user_read :: iostream:handle(),
         user_write :: iostream:handle(),
         socket :: socket:socket(),
         reader :: #reader{}
        }).

-record(?MODULE,
        {
         pid :: pid(),
         read :: iostream:handle(),
         write :: iostream:handle()
        }).

-opaque socket() :: #?MODULE{}.
-export_type([socket/0]).

%%
%% Connect a socket
%%

-spec connect(SockAddr, Opts) -> {ok, Socket} | {error, Reason} when
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Opts     :: gen_tcp:connect_option(),
      Socket   :: socket(),
      Reason   :: inet:posix().

connect(SockAddr, Opts) ->
    connect(SockAddr, Opts, infinity).

-spec connect(Address, Port, Opts) -> {ok, Socket} | {error, Reason}  when
      Address  :: inet:socket_address() | inet:hostname(),
      Port     :: inet:port_number(),
      Opts     :: gen_tcp:connect_option(),
      Socket   :: socket(),
      Reason   :: inet:posix();
             (SockAddr, Opts, Timeout) -> {ok, Socket} | {error, Reason} when
      SockAddr :: socket:sockaddr_in() | socket:sockaddr_in6(),
      Opts     :: gen_tcp:connect_option(),
      Timeout  :: timeout(),
      Socket   :: socket(),
      Reason   :: timeout | inet:posix().

connect(Address, Port, Opts)
  when is_tuple(Address) orelse
       is_list(Address)  orelse
       is_atom(Address)  orelse
       (Address =:= any) orelse
       (Address =:= loopback) ->
    connect(Address, Port, Opts, infinity);
connect(#{family := Fam} = SockAddr, Opts, Timeout)
  when ((Fam =:= inet) orelse (Fam =:= inet6)) ->
    SockAddr2 = inet:ensure_sockaddr(SockAddr),
    start_client(SockAddr2, Opts, Timeout).

-spec connect(Address, Port, Opts, Timeout) ->
                     {ok, Socket} | {error, Reason} when
      Address :: inet:socket_address() | inet:hostname(),
      Port    :: inet:port_number(),
      Opts    :: [inet:inet_backend() | gen_tcp:connect_option()],
      Timeout :: timeout(),
      Socket  :: socket(),
      Reason  :: timeout | inet:posix().

connect(Address, Port, Opts, Timeout) ->
    start_client(Address, Port, Opts, Timeout).

%%
%% Generic tcp shutdown
%%

-spec shutdown(Socket, How) -> ok | {error, Reason} when
      Socket :: socket(),
      How :: read | write | read_write,
      Reason :: inet:posix().
shutdown(#?MODULE{pid = Pid}, How) ->
    gen_server:call(Pid, {shutdown, How}).

%%
%% Close
%%

-spec close(Socket) -> ok when
      Socket :: socket().
close(#?MODULE{pid = Pid}) ->
    gen_server:call(Pid, close).

%%
%% Send
%%

-spec send(Socket, Packet) -> ok | {error, Reason} when
      Socket   :: socket(),
      Packet   :: erlang:iovec(),
      Reason   :: closed | {timeout, RestData} | inet:posix(),
      RestData :: binary() | erlang:iovec().

send(#?MODULE{pid = Pid, write = Stream}, Packet) ->
    Ev = iostream:write(Stream, Packet),
    ?LOG(debug, "~p:~p: ~p ~p~n", [?MODULE, ?LINE, Stream, Ev]),
    case Ev of
        [] ->
            ok;
        [notify] ->
            gen_server:cast(Pid, send);
        [closed] ->
            {error, closed}
    end.

%%
%% Receive data from a socket (passive mode)
%%

-doc(#{equiv => recv(Socket, Length, infinity)}).
-spec recv(Socket, Length) -> {ok, Packet} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Packet :: binary(),
      Reason :: closed | inet:posix().

recv(Socket, Length) ->
    recv(Socket, Length, infinity).

-spec recv(Socket, Length, Timeout) -> {ok, Packet} | {error, Reason} when
      Socket :: socket(),
      Length :: non_neg_integer(),
      Timeout :: timeout(),
      Packet :: erlang:iovec(),
      Reason :: closed | timeout | inet:posix().

recv(#?MODULE{read = Stream} = Socket, Length, Timeout) ->
    case iostream:read(Stream, Length) of
        {[], {incomplete, Available}} ->
            recv_wait(Socket, Length-Available, Timeout);
        {[], []} ->
            recv_wait(Socket, Length, Timeout);
        {[], Data} ->
            {ok, Data};
        {[closed], {_, _}} ->
            {error, closed};
        {[closed], Data} ->
            {ok, Data}
    end.

recv_wait(#?MODULE{pid = Pid} = Socket, Length, Timeout) ->
    try gen_server:call(Pid, {more_data, Length}, Timeout) of
        ok ->
            recv(Socket, Length, 0);
        Error ->
            Error
    catch exit:{timeout, _} ->
            gen_server:cast(Pid, {cancel_req, self()}),
            {error, timeout}
    end.

%% %%
%% %% Put back data ??
%% %%

%% -doc false.
%% unrecv(S, Data) when is_port(S) ->
%%     case inet_db:lookup_socket(S) of
%% 	{ok, Mod} ->
%% 	    Mod:unrecv(S, Data);
%% 	Error ->
%% 	    Error
%%     end.

%%
%% Change controlling process
%%

-spec controlling_process(Socket, Pid) -> ok | {error, Reason} when
      Socket :: socket(),
      Pid :: pid(),
      Reason :: closed | not_owner | badarg | inet:posix().

controlling_process(S, NewOwner) ->
    gen_server:call(S, {controlling_process, NewOwner}).

%%
%% Helpers and local functions
%%

start_client(SockAddr, Opts0, Timeout) ->
    Opts = check_opts(Opts0),
    {ok, Pid} = gen_server:start(?MODULE, [client, self()], []),
    gen_server:call(Pid, {connect, SockAddr, Opts, Timeout}).

start_client(Address, Port, Opts0, Timeout) ->
    Opts = check_opts(Opts0),
    {ok, Pid} = gen_server:start(?MODULE, [client, self()], []),
    SockAddr = #{family => inet, addr => Address, port => Port},
    gen_server:call(Pid, {connect, SockAddr, Opts, Timeout}).

%%
%% Server functionality
%%

init([_Role, Owner]) ->
    process_flag(trap_exit, true),
    {ReadIn, WriteIn} = iostream:new(),
    {ReadOut, WriteOut} = iostream:new(),
    iostream:notify(ReadOut),
    {ok, Socket} = socket:open(inet, stream, tcp),
    {ok, #state{pid = Owner,
                read = ReadOut, write = WriteIn,
                user_read = ReadIn, user_write = WriteOut,
                socket = Socket,
                reader = #reader{from = Owner, read_ahead = 0, length = 0, handle = undefined}
               }}.

handle_call({connect, SockAddr, _Opts, Timeout}, _From,
            #state{role = undefined, socket = Sock} = State) ->
    %% io:format("~p:~p: ~p ~n",[?MODULE, ?LINE, SockAddr]),
    case socket:connect(Sock, SockAddr, Timeout) of
        ok ->
            {reply, {ok, make_socket(State)}, State#state{role = client}};
        {error, _} = Error ->
            ok = socket:close(Sock),
            {stop, normal, Error, State}
    end;

handle_call({more_data, Length}, From, #state{write = Write, socket = Sock, reader = Reader} = State) ->
    case socket:recv(Sock, Length, 0) of
        {error, timeout} ->
            {noreply, State#state{reader = fetch_data(From, Length, Sock, Write, Reader)}};
        {error, {timeout, Data}} ->
            [] = iostream:write(Write, Data),
            {noreply, State#state{reader = fetch_data(From, Length-byte_size(Data), Sock, Write, Reader)}};
        {ok, Data} ->
            [] = iostream:write(Write, Data),
            {reply, ok, State#state{reader = fetch_data(undefined, 0, Sock, Write, Reader)}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(close, _From, State) ->
    {stop, normal, {reply, ok}, State};

handle_call(Op, _, State) ->
    ?LOG_INFO("~w: could not handle ~P", [?MODULE, Op, 20]),
    {reply, {error, badarg}, State}.


handle_cast(send, #state{read = Stream, socket = Sock} = State) ->
    iostream:notify(Stream),
    %% Should be done by socket nif anyway
    {_, Data} = iostream:read(Stream, all),   %% FIXME ASYNC
    ok = socket:sendv(Sock, Data),
    {noreply, State};

handle_cast({cancel_req, Pid}, #state{reader = Reader} = State) ->
    ?LOG(debug, "~p:~p: ~p ~p~n", [?MODULE, ?LINE, Pid, Reader]),
    case Reader of
        #reader{from = {Pid, _}} ->
            {noreply, State#state{reader = Reader#reader{from = undefined, length = 0}}};
        _ ->
            ?LOG_INFO("~w: Unknown cancel req from ~p~n",[?MODULE, Pid]),
            {noreply, State}
    end;

handle_cast(Info, State) ->
    ?LOG_INFO("~w: Unknown cast ~P", [?MODULE, Info, 20]),
    {noreply, State}.


handle_info({'$socket', Sock, select, Handle},
            #state{socket = Sock, write = Stream,
                   reader = #reader{handle = Handle,
                                    length = Size,
                                    from = From
                                   } = Reader
                  } = State) ->
    R = fetch_data(From, Size, Sock, Stream, Reader#reader{handle = undefined}),
    {noreply, State#state{reader = R}};

handle_info(Info, State) ->
    ?LOG_INFO("~w: Unknown info ~P", [?MODULE, Info, 20]),
    {noreply, State}.

terminate(_Reason, #state{socket = Sock}) ->
    ?LOG(debug, "~w: closing socket~n", [?MODULE]),
    socket:close(Sock).

%%
make_socket(#state{user_read = Read, user_write = Write}) ->
    #?MODULE{pid = self(), read = Read, write = Write}.

fetch_data(From, Size, Sock, Stream, #reader{handle = undefined} = Reader) ->
    case socket:recv(Sock, Size, nowait) of
        {select, {select_info, _, Handle}} ->
            Reader#reader{from = From, length = Size, handle = Handle};
        {select, {{select_info, _, Handle}, Data}} ->
            _ = iostream:write(Stream, Data),
            Reader#reader{from = From, length = Size-byte_size(Data), handle = Handle};
        %% FIXME Windows stuff
        {ok, Data} ->
            _ = iostream:write(Stream, Data),
            reply_data(From),
            read_ahead(Sock, Stream, Reader#reader{from=undefined, length=0})
    end;
fetch_data(From, Size, _Sock, _Stream, #reader{length = 0} = Reader) ->
    Reader#reader{length = Size, from = From}.

read_ahead(Sock, Stream, #reader{read_ahead = ReadAHead} = Reader) ->
    case ReadAHead > 0 of
        true ->
            fetch_data(undefined, 0, Sock, Stream, Reader#reader{read_ahead = ReadAHead-1});
        false ->
            Reader
    end.

reply_data(From) when is_pid(From) ->
    fixme;
reply_data(From) ->
    gen_server:reply(From, ok).

check_opts(Opts0) ->
    lists:foldr(fun check_opts_1/2, #{}, Opts0).

check_opts_1(binary, Opts) ->
    Opts#{binary => true};
check_opts_1({active, Active}, Opts) ->
    Opts#{active => Active};
check_opts_1({packet, Raw}, Opts) ->
    if Raw =:= 0 -> Opts;
       Raw =:= raw -> Opts;
       true -> error({badarg, {unsupported, {packet, Raw}}})
    end;
check_opts_1(Opt, _Opts) ->
    error({badarg, {unsupported, Opt}}).
