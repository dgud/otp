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

-module(tls_record_nif).
-moduledoc false.

-export([create_pack_nif/3, create_unpack_nif/3]).
-export([process_nif/1, update_nif/2]).

-include("tls_record.hrl").
-include("ssl_internal.hrl").
-include("ssl_alert.hrl").
-include("tls_handshake.hrl").
-include("ssl_cipher.hrl").
-include_lib("kernel/include/logger.hrl").

-on_load(on_load/0).
-define(TLS_RECORD_NIF_VSN,1).

create_pack_nif(_, _, _) -> erlang:nif_error(undefined).
create_unpack_nif(_, _, _) -> erlang:nif_error(undefined).
process_nif(_) -> erlang:nif_error(undefined).
update_nif(_, _) -> erlang:nif_error(undefined).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

on_load() ->
    erlang:display(loaded),
    LibBaseName = "tls_record_nif",
    PrivDir = code:priv_dir(ssl),
    LibName = case erlang:system_info(build_type) of
                  opt ->
                      LibBaseName;
                  Type ->
                      LibTypeName = LibBaseName ++ "." ++ atom_to_list(Type),
                      case (filelib:wildcard(
                              filename:join(
                              [PrivDir,
                                  "lib",
                                  LibTypeName ++ "*"]),
                              erl_prim_loader) /= []) orelse
                          (filelib:wildcard(
                              filename:join(
                              [PrivDir,
                              "lib",
                              erlang:system_info(system_architecture),
                              LibTypeName ++ "*"]),
                              erl_prim_loader) /= []) of
                          true -> LibTypeName;
                          false -> LibBaseName
                      end
              end,
    Lib = filename:join([PrivDir, "lib", LibName]),
    LibBin = path2bin(Lib),
    Status = case erlang:load_nif(Lib, {?TLS_RECORD_NIF_VSN,
                                        LibBin}) of
                 ok -> ok;
                 {error, {load_failed, _}}=Error ->
                     ArchLibDir =
                         filename:join([PrivDir, "lib",
                                     erlang:system_info(system_architecture)]),
                     Candidate =
                         filelib:wildcard(
                         filename:join(
                             [ArchLibDir,LibName ++ "*" ]),
                         erl_prim_loader),
                     case Candidate of
                         [] ->
                            Error;
                         _ ->
                             ArchLib = filename:join([ArchLibDir, LibName]),
                             ArchBin = path2bin(ArchLib),
                             erlang:load_nif(ArchLib, {?TLS_RECORD_NIF_VSN,
                                                       ArchBin})
                     end;
                 Error ->
                    Error
             end,
    case Status of
        ok ->
            ok;
        {error, {E, Str}} ->
            error_logger:error_msg("Unable to load tls record library. Failed "
                                   "with error:~n\"~p, ~s\"~n", [E,Str]),
            Status
    end.

path2bin(Path) when is_list(Path) ->
    Encoding = file:native_name_encoding(),
    case unicode:characters_to_binary(Path, Encoding, Encoding) of
        Bin when is_binary(Bin) ->
            Bin
    end.
