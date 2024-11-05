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
-spec output_handler_start(_, _) -> no_return(). % Server loop
output_handler_start(Socket, DistHandle) ->
    try
        erlang:dist_ctrl_get_data_notification(DistHandle),
        IOS = iostream:new(),
        output_handler(Socket, DistHandle, IOS)
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_logger:error_report(
              [output_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

output_handler(Socket, DistHandle, IOS) ->
    receive
        dist_tick ->
            output_handler_tick(Socket, DistHandle, IOS);
        dist_data ->
            output_handler_data(Socket, DistHandle, IOS);
        _ -> % Ignore
            output_handler(Socket, DistHandle, IOS)
    end.

output_handler_tick(Socket, DistHandle, IOS) ->
    receive
        dist_tick ->
            output_handler_tick(Socket, DistHandle, IOS);
        dist_data ->
            output_handler_data(Socket, DistHandle, IOS);
        _ -> % Ignore
            output_handler_tick(Socket, DistHandle, IOS)
    after 0 ->
            _ = iostream:write(element(2, IOS), [<<0:32>>]),
            output_handler_send(Socket, DistHandle, IOS)
    end.

output_handler_data(Socket, DistHandle, IOS) ->
    output_handler_data(Socket, DistHandle, IOS, 0).
%%
output_handler_data(Socket, DistHandle, IOS, Size)
  when 1 bsl 16 =< Size ->
    output_handler_send(Socket, DistHandle, IOS);
output_handler_data(Socket, DistHandle, IOS, Size) ->
    case erlang:dist_ctrl_get_data(DistHandle) of
        none ->
            if
                Size =:= 0 ->
                    erlang:dist_ctrl_get_data_notification(DistHandle),
                    output_handler(Socket, DistHandle, IOS);
                true ->
                    output_handler_send(Socket, DistHandle, IOS)
            end;
        {Len, Iovec} ->
            %% erlang:display({Len, '==>>'}),
            _ = iostream:write(element(2, IOS), [<<Len:32>> | Iovec]),
            output_handler_data(Socket, DistHandle, IOS, Size + Len + 4)
    end.

%% Output data to socket
output_handler_send(Socket, DistHandle, IOS) ->
    case socket:send_iostream(Socket, element(1, IOS), nowait) of
        {_Events, ok, _Sent, 0} ->
            output_handler_data(Socket, DistHandle, IOS);
        {_Events, ok, _Sent, _Remaining} ->
            output_handler_send(Socket, DistHandle, IOS);
        {_Events, {select, {select_info, _, SelectHandle}},
         _Sent, _Remaining} ->
            output_handler_select(Socket, DistHandle, IOS, SelectHandle);
        {_Events, {error, Reason}, _Sent, _Remaining} ->
            exit(Reason)
    end.

output_handler_select(Socket, DistHandle, IOS, SelectHandle) ->
    receive
        {'$socket', Socket, select, SelectHandle} ->
            output_handler_send(Socket, DistHandle, IOS);
        _ -> % Ignore, also dist_tick
            output_handler_select(Socket, DistHandle, IOS, SelectHandle)
    end.

%% ------------------------------------------------------------
-spec input_handler_start(_, _) -> no_return(). % Server loop
input_handler_start(Socket, DistHandle) ->
    try input_handler(Socket, DistHandle)
    catch
        Class : Reason : Stacktrace when Class =:= error ->
            error_logger:error_report(
              [input_handler_exception,
               {class, Class},
               {reason, Reason},
               {stacktrace, Stacktrace}]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

input_handler(Socket, DistHandle) ->
    IOS = iostream:new(),
    input_handler(Socket, DistHandle, IOS, 0).

input_handler(Socket, DistHandle, IOS, Size) when 4 =< Size ->
    <<PacketSize:32>> =
        case iostream:read(element(1, IOS), 4) of
            {_Events, [Bin]} -> Bin;
            {_Events, Iovec} -> iolist_to_binary(Iovec)
        end,
    input_handler_packet(Socket, DistHandle, IOS, Size - 4, PacketSize);
input_handler(Socket, DistHandle, IOS, Size) ->
    Size_1 = input_data(Socket, IOS, Size),
    input_handler(Socket, DistHandle, IOS, Size_1).

input_handler_packet(Socket, DistHandle, IOS, Size, PacketSize)
  when PacketSize =< Size ->
    {_Events_2, Packet} = iostream:read(element(1, IOS), PacketSize),
    put_data(DistHandle, PacketSize, Packet),
    input_handler(Socket, DistHandle, IOS, Size - PacketSize);
input_handler_packet(Socket, DistHandle, IOS, Size, PacketSize) ->
    Size_1 = input_data(Socket, IOS, Size),
    input_handler_packet(Socket, DistHandle, IOS, Size_1, PacketSize).

%% Input data from socket
input_data(Socket, IOS, Size) ->
    case socket:recv_iostream(Socket, 0, [], element(2, IOS), nowait) of
        {_Events, ok, Received} ->
            Size + Received;
        {_Events, {select, {select_info, _, SelectHandle}}, Received} ->
            receive
                {'$socket', Socket, select, SelectHandle} ->
                    input_data(Socket, IOS, Size + Received)
            end;
        {_Events, {error, Reason}, _Received} ->
            exit(Reason)
    end.


%%% put_data(_DistHandle, 0, _) ->
%%%     ok;
%% We deliver ticks (packets size 0) to the VM,
%% so that erlang:dist_get_stat(DistHandle) that
%% dist_util:getstat/3 falls back to becomes good enough
put_data(DistHandle, _PacketSize, Packet) ->
    %% erlang:display({'<<==', _PacketSize}),
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
