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
-export([controlling_process/2, setopts/2]).

-behavior(gen_server).
-include_lib("kernel/include/logger.hrl").
%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(reader,
        {
         from :: pid(),
         read_ahead :: integer(),  %% No of packets, similar to active n (-1 is active)
         length :: non_neg_integer(),
         handle :: undefined | socket:select_handle() | socket:completion_handle(),
         user_read :: iostream:handle(),
         user_write :: iostream:handle()
        }).

-record(state,
        {
         role :: 'client' | 'server',
         pid :: pid(),
         read :: iostream:handle(),
         write :: iostream:handle(),
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
%% setopts ({active,...)
%%

-spec setopts(Socket, Opts) -> ok | {error, Reason} when
      Socket :: socket(),
      Opts :: [{active, false|true|once|integer()}],
      Reason :: closed | not_owner | badarg | inet:posix().

setopts(#?MODULE{pid = Pid}, [{active, What}])
  when is_boolean(What); is_integer(What); What =:= once ->
    gen_server:call(Pid, {active, What}).


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
                socket = Socket,
                reader = #reader{from = Owner, read_ahead = 0, length = 0, handle = undefined,
                                 user_read = ReadIn, user_write = WriteOut}
               }
    }.

handle_call({connect, SockAddr, #{active := Active}, Timeout}, From,
            #state{role = undefined, socket = Sock, reader=Reader} = State0) ->
    %% io:format("~p:~p: ~p ~n",[?MODULE, ?LINE, SockAddr]),
    case socket:connect(Sock, SockAddr, Timeout) of
        ok ->
            #state{} = State = handle_active(Active, From, State0#state{role = client}),
            {reply, {ok, make_socket(Reader)}, State};
        {error, _} = Error ->
            ok = socket:close(Sock),
            {stop, normal, Error, State0}
    end;

handle_call({more_data, Length}, From0,
            #state{pid = Pid, write = Write, socket = Sock, reader = Reader} = State) ->
    case socket:recv(Sock, Length, 0) of
        {error, timeout} ->
            From = #{owner => Pid, from => From0},
            {noreply,
             State#state{reader = fetch_data(Length, Sock, Write,
                                             Reader#reader{from=From})}};
        {error, {timeout, Data}} ->
            From = #{owner => Pid, from => From0},
            [] = iostream:write(Write, Data),
            {noreply,
             State#state{reader = fetch_data(Length-byte_size(Data), Sock, Write,
                                             Reader#reader{from=From})}};
        {ok, Data} ->
            [] = iostream:write(Write, Data),
            {reply, ok, State#state{reader = fetch_data(0, Sock, Write, Reader)}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({active, N}, From, State0) ->
    case handle_active(N, From, State0) of
        #state{} = State ->
            {reply, ok, State};
        Error ->
            {reply, Error, State0}
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
        #reader{from = #{from := {Pid, _}, owner := CPid}} ->
            {noreply, State#state{reader = Reader#reader{from = CPid, length = 0}}};
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
                                    length = Size
                                   } = Reader
                  } = State) ->
    R = fetch_data(Size, Sock, Stream, Reader#reader{handle = undefined}),
    {noreply, State#state{reader = R}};

handle_info(Info, State) ->
    ?LOG_INFO("~w: Unknown info ~P", [?MODULE, Info, 20]),
    {noreply, State}.

terminate(_Reason, #state{socket = Sock}) ->
    ?LOG(debug, "~w: closing socket~n", [?MODULE]),
    socket:close(Sock).

%%
make_socket(#reader{user_read = Read, user_write = Write}) ->
    #?MODULE{pid = self(), read = Read, write = Write}.

fetch_data(Size, Sock, Stream, #reader{handle = undefined, from = From} = Reader0) ->
    case socket:recv(Sock, Size, nowait) of
        {select, {select_info, _, Handle}} ->
            Reader0#reader{length = Size, handle = Handle};
        {select, {{select_info, _, Handle}, Data}} ->
            _ = iostream:write(Stream, Data),
            Reader0#reader{length = Size-byte_size(Data), handle = Handle};
        %% FIXME Windows stuff
        {ok, Data} ->
            Ev = iostream:write(Stream, Data),
            Reader = reply_data(Ev, From, Reader0),
            read_ahead(Sock, Stream, Reader)
    end;
fetch_data(Size, _Sock, _Stream, #reader{length = 0} = Reader) ->
    Reader#reader{length = Size}.

read_ahead(_Sock, _Stream, #reader{read_ahead = 0} = Reader) ->
    Reader;
read_ahead(Sock, Stream, Reader) ->
    fetch_data(0, Sock, Stream, Reader).

handle_active(false, {Pid, _}, #state{pid = Pid, reader = Reader0} = State) ->
    State#state{reader = Reader0#reader{read_ahead = 0}};
handle_active(true, {Pid, _}, #state{pid = Pid, socket = Sock, write = Stream,
                                     reader = Reader0} = State) ->
    iostream:notify(Reader0#reader.user_read),
    Reader = read_ahead(Sock, Stream, Reader0#reader{from = Pid, read_ahead = -1}),
    State#state{reader = Reader};
handle_active(once, {Pid, _}, #state{pid = Pid, socket = Sock, write = Stream,
                                     reader = Reader0} = State) ->
    iostream:notify(Reader0#reader.user_read),
    Reader = read_ahead(Sock, Stream, Reader0#reader{from = Pid, read_ahead = once}),
    State#state{reader = Reader};
handle_active(N, {Pid, _}, #state{pid = Pid, socket = Sock, write = Stream,
                                  reader = #reader{read_ahead = Prev, user_read = User} = Reader0} = State) ->
    ReadAHead = if Prev == -1 -> max(0, N);
                   true -> max(0, Prev+N)
                end,
    ReadAHead > 0 andalso iostream:notify(User),
    Reader = read_ahead(Sock, Stream, Reader0#reader{read_ahead = ReadAHead}),
    State#state{reader = Reader};
handle_active(_Active, {Pid, _},  #state{pid = Pid2}) ->
    ?LOG_DEBUG("ACTIVE: ~p ~p ~p~n",[_Active, Pid, Pid2]),
    {error, {not_controlling_process, Pid, Pid2}}.

reply_data([], From, Reader) when is_pid(From) ->
    Reader;
reply_data([notify], From, #reader{user_read = Stream, read_ahead = RA0} = Reader) when is_pid(From) ->
    From ! {?MODULE, make_socket(Reader), Stream},
    RA = if RA0 =:= once ->
                 0;
            RA0 =:= 1 ->
                 From ! {tcp_iostream_passive, make_socket(Reader)},
                 0;
            RA0 > 0 ->
                 RA0 -1;
            RA0 =:= -1 ->  %% active mode
                 RA0
         end,
    Reader#reader{read_ahead = RA};
reply_data(_Ev, #{from := From, owner:= Pid}, Reader) ->
    gen_server:reply(From, ok),
    Reader#reader{from = Pid, length = 0}.

check_opts(Opts0) ->
    lists:foldr(fun check_opts_1/2, #{active => true}, Opts0).

check_opts_1(binary, Opts) ->
    Opts#{binary => true};
check_opts_1({mode, binary}, Opts) ->
    Opts#{binary => true};
check_opts_1({active, Active}, Opts) ->
    Opts#{active => Active};
check_opts_1({packet, Raw}, Opts) ->
    if Raw =:= 0 -> Opts;
       Raw =:= raw -> Opts;
       true -> error({badarg, {unsupported, {packet, Raw}}})
    end;
check_opts_1({header, 0}, Opts) ->
    Opts;
check_opts_1({packet_size, 0}, Opts) ->
    Opts;
check_opts_1(Opt, _Opts) ->
    error({badarg, {unsupported, Opt}}).
