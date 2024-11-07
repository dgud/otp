%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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

-module(inet_epmd_socket_iostream).
-moduledoc false.

%% DistMod API
-export([net_address/0, listen_open/2, listen_port/3, listen_close/1,
         accept_open/2, accept_controller/3, accepted/3,
         connect/3]).

-export([supported/0]).

-export([start_dist_ctrl/2]).

-include("net_address.hrl").
-include("dist.hrl").
-include("dist_util.hrl").

-define(PROTOCOL, tcp).
-define(FAMILY, inet).

%% ------------------------------------------------------------
net_address() ->
    #net_address{
       protocol = ?PROTOCOL,
       family = ?FAMILY }.

%% ------------------------------------------------------------
listen_open(#net_address{ family = Family}, ListenOptions) ->
    maybe
        Key = backlog,
        Default = 128,
        Backlog = proplists:get_value(Key, ListenOptions, Default),
        {ok, ListenSocket} ?=
            socket:open(Family, stream),
        ok ?=
            setopts(
              ListenSocket,
              [inet_epmd_dist:nodelay() |
               proplists:delete(
                 Key,
                 proplists:delete(nodelay, ListenOptions))]),
        {ok, {ListenSocket, Backlog}}
    else
        {error, _} = Error ->
            Error
    end.

setopts(Socket, Options) ->
    gen_tcp_socket:socket_setopts(Socket, Options).

%% ------------------------------------------------------------
listen_port(
  #net_address{ family = Family }, Port, {ListenSocket, Backlog}) ->
    maybe
        Sockaddr =
            #{family => Family,
              addr => any,
              port => Port},
        ok ?=
            socket:bind(ListenSocket, Sockaddr),
        ok ?=
            socket:listen(ListenSocket, Backlog),
        {ok, #{ addr := Ip, port := ListenPort}} ?=
            socket:sockname(ListenSocket),
        {ok, {ListenSocket, {Ip, ListenPort}}}
    else
        {error, _} = Error ->
            Error
    end.

%% ------------------------------------------------------------
listen_close(ListenSocket) ->
    socket:close(ListenSocket).

%% ------------------------------------------------------------
accept_open(_NetAddress, ListenSocket) ->
    maybe
        {ok, Socket} ?=
            socket:accept(ListenSocket),
        {ok, #{ addr := Ip }} ?=
            socket:sockname(Socket),
        {ok, #{ addr := PeerIp, port := PeerPort }} ?=
            socket:peername(Socket),
        inet_epmd_dist:check_ip(Ip, PeerIp),
        {Socket, {PeerIp, PeerPort}}
    else
        {error, Reason} ->
            exit({?FUNCTION_NAME, Reason})
    end.

%% ------------------------------------------------------------
accept_controller(_NetAddress, Controller, Socket) ->
    maybe
        ok ?=
            socket:setopt(Socket, {otp,controlling_process}, Controller),
        Socket
    else
        {error, Reason} ->
            exit({?FUNCTION_NAME, Reason})
    end.

%% ------------------------------------------------------------
accepted(NetAddress, _Timer, Socket) ->
    start_dist_ctrl(NetAddress, Socket).

%% ------------------------------------------------------------
connect(
  #net_address{ address = {Ip, Port}, family = Family } = NetAddress,
  _Timer, ConnectOptions) ->
    maybe
        {ok, Socket} ?=
            socket:open(Family, stream),
        ok ?=
            setopts(Socket, ConnectOptions),
        ConnectAddress =
            #{ family => Family,
               addr => Ip,
               port => Port },
        ok ?=
            socket:connect(Socket, ConnectAddress),
        start_dist_ctrl(NetAddress, Socket)
    else
        {error, _} = Error ->
            Error
    end.

%% ------------------------------------------------------------
start_dist_ctrl(NetAddress, Socket) ->
    Controller = self(),
    DistCtrlTag = make_ref(),
    DistCtrl =
        spawn_link(
          fun () ->
                  receive
                      {DistCtrlTag, handshake_complete, From, DistHandle} ->
                          Sync = make_ref(),
                          DistC = self(),
                          InputHandler =
                              spawn_link(
                                fun () ->
                                        link(Controller),
                                        DistC ! Sync,
                                        receive Sync -> ok end,
                                        input_handler_start(
                                          Socket, DistHandle)
                                end),
                          false =
                              erlang:dist_ctrl_set_opt(
                                DistHandle, get_size, true),
                          ok =
                              erlang:dist_ctrl_input_handler(
                                DistHandle, InputHandler),
                          receive Sync -> InputHandler ! Sync end,
                          From ! {DistCtrlTag, handshake_complete}, % Reply
                          output_handler_start(Socket, DistHandle)
                  end
          end),
    #hs_data{
       socket = Socket,
       f_send =
           fun (S, Packet) when S =:= Socket ->
                   send_packet_2(S, Packet)
           end,
       f_recv =
           fun (S, 0, infinity) when S =:= Socket ->
                   recv_packet_2(S)
           end,
       f_setopts_pre_nodeup = f_ok(Socket),
%%%            fun (S) when S =:= Socket ->
%%%                    socket:setopt(S, {otp,debug}, true)
%%%            end,
       f_setopts_post_nodeup = f_ok(Socket),
       f_address =
           fun (S, Node) when S =:= Socket ->
                   inet_epmd_dist:f_address(NetAddress, Node)
           end,
       f_getll =
           fun (S) when S =:= Socket ->
                   {ok, DistCtrl}
           end,

       f_handshake_complete =
           fun (S, _Node, DistHandle) when S =:= Socket ->
                   handshake_complete(DistCtrl, DistCtrlTag, DistHandle)
           end,

       mf_tick =
           fun (S) when S =:= Socket ->
                   tick(DistCtrl)
           end }.

%%%        mf_getstat =
%%%            fun (S) when S =:= Socket ->
%%%                    getstat(S)
%%%            end,
%%%        mf_setopts = mf_setopts(Socket),
%%%        mf_getopts = mf_getopts(Socket) }.

send_packet_2(Socket, Packet) ->
    Size = iolist_size(Packet),
    true = Size < 1 bsl 16,
    socket:send(Socket, [<<Size:16>>, Packet]).

recv_packet_2(Socket) ->
    maybe
        {ok, <<Size:16>>} ?=
            socket:recv(Socket, 2),
        {ok, Data} ?=
            socket:recv(Socket, Size),
        {ok, binary_to_list(Data)}
    else
        {error, _} = Error ->
            Error
    end.

f_ok(Socket) ->
    fun (S) when S =:= Socket ->
            ok
    end.

-ifdef(undefined).
getstat(S) ->
    #{ counters :=
          #{ read_pkg := ReadPkg,
             write_pkg := WritePkg } } = socket:info(S),
    %% Ignoring that the counters may wrap since dist_util
    %% only looks for changing values anyway
    {ok, [{recv_cnt, ReadPkg}, {send_cnt, WritePkg}, {send_pend, 0}]}.

mf_setopts(Socket) ->
    f_ok(Socket).

mf_getopts(Socket) ->
    fun (S, Opts) when S =:= Socket, is_list(Opts) ->
            {ok, []}
    end.
-endif.

handshake_complete(DistCtrl, DistCtrlTag, DistHandle) ->
    DistCtrl ! {DistCtrlTag, handshake_complete, self(), DistHandle},
    receive
        {DistCtrlTag, handshake_complete} ->
            ok
    end.

tick(DistCtrl) ->
    DistCtrl ! dist_tick,
    ok.

%% ------------------------------------------------------------
-record(ohp, %% Output Handler Parameters
        {socket, dist_handle, watermark, ios}).

-spec output_handler_start(_, _) -> no_return(). % Server loop
output_handler_start(Socket, DistHandle) ->
    try
        erlang:dist_ctrl_get_data_notification(DistHandle),
        {ok, SndbufSize} = socket:getopt(Socket, {socket,sndbuf}),
        OHP =
            #ohp{
               socket      = Socket,
               dist_handle = DistHandle,
               watermark   = SndbufSize bsr 1,
               ios         = iostream:new()},
        Size               = 0,
        DistData           = false,
        SelectHandle       = nowait,
        output_handler(OHP, Size, DistData, SelectHandle)
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_logger:error_report(
              [output_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

output_handler(OHP, Size, DistData, SelectHandle) ->
    if
        DistData, OHP#ohp.watermark > Size->
            %% There is dist_data from the emulator,
            %% and we have buffer space for it
            DistHandle = OHP#ohp.dist_handle,
            case erlang:dist_ctrl_get_data(DistHandle) of
                none ->
                    erlang:dist_ctrl_get_data_notification(DistHandle),
                    output_handler(OHP, Size, false, SelectHandle);
                {Len, Iovec} ->
                    %% erlang:display({?FUNCTION_NAME, ?LINE, Len}),
                    #ohp{ios = {_, WriteHandle}} = OHP,
                    _ = iostream:write(WriteHandle, [<<Len:32>> | Iovec]),
                    Size_1 = Len + 4 + Size,
                    output_handler(OHP, Size_1, DistData, SelectHandle)
            end;
        SelectHandle =:= nowait, Size > 0 ->
            %% We are not waiting for a send to complete,
            %% and we have buffered data
            output_handler_send(OHP, Size, DistData);
        true ->
            %% Wait for something to happen
            output_handler_wait(OHP, Size, DistData, SelectHandle, infinity)
    end.

%% Wait for an external event (message)
output_handler_wait(
  #ohp{socket = Socket} = OHP, Size, DistData, SelectHandle, Tick) ->
    receive
        dist_data ->
            output_handler(OHP, Size, true, SelectHandle);
        dist_tick
          when SelectHandle =:= nowait, not DistData, Size == 0 ->
            %% Tick only when we don't wait for a send to complete
            %% and there is no dist_data to send,
            %% but receive all dist_tick messages first
            %% by looping with after timeout Tick = 0
            output_handler_wait(OHP, Size, DistData, SelectHandle, 0);
        {'$socket', Socket, select, SelectHandle}
          when SelectHandle =/= nowait ->
            %% Send no longer pending; try to send again
            output_handler_send(OHP, Size, DistData);
        _ -> % Ignore
            output_handler_wait(OHP, Size, DistData, SelectHandle, Tick)
    after Tick ->
            %% We can only get here after a dist_tick
            %%
            %% Send a tick
            Size     = 0,       % Assert
            DistData = false,   % Assert
            #ohp{ios = {_, WriteHandle}} = OHP,
            _ = iostream:write(WriteHandle, [<<0:32>>]),
            output_handler_send(OHP, Size + 4, DistData)
    end.

%% Output data to socket
output_handler_send(
  #ohp{socket = Socket, ios = IOS} = OHP, _Size, DistData) ->
    case socket:send_iostream(Socket, element(1, IOS), nowait) of
        {_Events, ok, _Sent, Remaining} ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, _Sent}),
            output_handler(OHP, Remaining, DistData, nowait);
        {_Events, {select, {select_info, _, SelectHandle}},
         _Sent, Remaining} ->
            output_handler(OHP, Remaining, DistData, SelectHandle);
        {_Events, {error, Reason}, _Sent, _Remaining} ->
            exit(Reason)
    end.

%% ------------------------------------------------------------
-record(ihp, %% Input Handler Parameters
        {socket, dist_handle, watermark, ios}).

-spec input_handler_start(_, _) -> no_return(). % Server loop
input_handler_start(Socket, DistHandle) ->
    try
        {ok, RcvbufSize} = socket:getopt(Socket, {socket,rcvbuf}),
        IHP =
            #ihp{
               socket      = Socket,
               dist_handle = DistHandle,
               watermark   = RcvbufSize,
               ios         = iostream:new()},
        Size               = 0,         % iostream size
        PacketSize         = undefined, % packet header not received
        SelectHandle       = undefined, % no receive in progress
        %% erlang:display({?FUNCTION_NAME, Socket, DistHandle}),
        input_handler(IHP, Size, PacketSize, SelectHandle)
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_logger:error_report(
              [input_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

input_handler(IHP, Size, undefined = _PacketSize, SelectHandle) ->
    if
        IHP#ihp.watermark > Size, SelectHandle =:= undefined ->
            %% We have room and no receive in progress
            input_handler_recv(IHP, Size, undefined, undefined);
        Size >= 4 ->
            %% We have a header to decode
            #ihp{ios = {ReadHandle, _}} = IHP,
            {_, Hdr} = iostream:read(ReadHandle, 4),
            <<PacketSize:32>> = iolist_to_binary(Hdr),
            %% erlang:display({?FUNCTION_NAME, ?LINE, hdr, Size, PacketSize}),
            input_handler(IHP, Size - 4, PacketSize, SelectHandle);
        true ->
            input_handler_recv(IHP, Size, undefined, SelectHandle)
    end;
input_handler(IHP, Size, PacketSize, SelectHandle) ->
    %% PacketSize is defined; we have a decoded header
    if
        PacketSize == 0 ->
            %% A Tick (empty packet)
            %% erlang:display({?FUNCTION_NAME, ?LINE, tick, Size}),
            put_data(IHP#ihp.dist_handle, []),
            input_handler(IHP, Size, undefined, SelectHandle);
        Size >= PacketSize ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, packet, Size, PacketSize}),
            %% We have a complete packet
            #ihp{dist_handle = DistHandle, ios = {ReadHandle, _}} = IHP,
            {_, Packet} = iostream:read(ReadHandle, PacketSize),
            put_data(DistHandle, Packet),
            input_handler(IHP, Size - PacketSize, undefined, SelectHandle);
        true ->
            input_handler_recv(IHP, Size, PacketSize, SelectHandle)
    end.

input_handler_recv(IHP, Size, PacketSize, undefined = _SelectHandle) ->
    #ihp{
       watermark = Watermark,
       socket = Socket,
       ios = {_, WriteHandle}} = IHP,
    RecvSize =
        if  PacketSize =:= undefined    -> 0;
            PacketSize  >  Size ->
                Missing = PacketSize - Size,
                if  Watermark > Missing -> 0;
                    true                -> Missing
                end
        end,
    case socket:recv_iostream(Socket, RecvSize, [], WriteHandle, nowait) of
        {_Events, ok, Received} ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, RecvSize, ok, Received}),
            input_handler(IHP, Size + Received, PacketSize, undefined);
        {_Events, {select, {select_info, _, SelectHandle}}, Received} ->
            %% erlang:display(
              %% {?FUNCTION_NAME, ?LINE, RecvSize, select, Received}),
            input_handler(IHP, Size + Received, PacketSize, SelectHandle);
        {_Events, {error, Reason}, _Received} ->
            exit(Reason)
    end;
input_handler_recv(IHP, Size, PacketSize, SelectHandle) ->
    %% SelectHandle is defined; we have a receive in progress
    input_handler_wait(IHP, Size, PacketSize, SelectHandle).

input_handler_wait(IHP, Size, PacketSize, SelectHandle) ->
    %% erlang:display(
    %%   {?FUNCTION_NAME, ?LINE, SelectHandle}),
    Socket = IHP#ihp.socket,
    receive
        {'$socket', Socket, select, SelectHandle} ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, select_received}),
            %% Go ahead with the receive
            input_handler_recv(IHP, Size, PacketSize, undefined);
        _Ignore ->
            %% erlang:display({?FUNCTION_NAME, ?LINE, ignore, _Ignore}),
            input_handler_wait(IHP, Size, PacketSize, SelectHandle)
    end.

%%% put_data(_DistHandle, 0, _) ->
%%%     ok;
%% We deliver ticks (packets size 0) to the VM,
%% so that erlang:dist_get_stat(DistHandle) that
%% dist_util:getstat/3 falls back to becomes good enough
put_data(DistHandle, Packet) ->
    %% erlang:display({'<<==', iolist_size(Packet)}),
    erlang:dist_ctrl_put_data(DistHandle, Packet).

%% ------------------------------------------------------------
supported() ->
    try socket:info() of
	#{io_backend := #{name := BackendName}}
          when (BackendName =/= win_esaio) ->
            ok;
        _ ->
            {skip, "Temporary exclusion"}
    catch
        error : notsup ->
            {skip, "esock not supported"};
        error : undef ->
            {skip, "esock not configured"}
    end.
    %% try socket:is_supported(ipv6) of
    %%     _ ->
    %%         ok
    %% catch error : notsup ->
    %%         "Module 'socket' not supported"
    %% end.
