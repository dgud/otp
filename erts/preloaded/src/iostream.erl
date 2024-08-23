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
-module(iostream).
-moduledoc """
PLACEHOLDER
""".

-export([new/0]).
-export([close/1, notify/1, read/1, read/2, write/2]).

-define(ERTS_IO_STREAM_EVENT_NONE, 0).
-define(ERTS_IO_STREAM_EVENT_CLOSED, 1).
-define(ERTS_IO_STREAM_EVENT_NOTIFY, 2).

-doc """
PLACEHOLDER
""".
-type event() :: notify | closed.
-export_type([event/0]).

-doc """
PLACEHOLDER
""".
-opaque handle() :: reference().
-export_type([handle/0]).

-doc """
PLACEHOLDER
""".
-spec new() -> {Read :: handle(), Write :: handle()}.
new() ->
    erlang:nif_error(undefined).

-doc """
PLACEHOLDER
""".
-spec close(handle()) -> ok.
close(_Handle) ->
    erlang:nif_error(undefined).

-doc """
PLACEHOLDER
""".
-spec notify(handle()) -> ok.
notify(_Handle) ->
    erlang:nif_error(undefined).

-doc """
PLACEHOLDER
""".
-spec write(handle(), erlang:iovec()) -> [event()].
write(Handle, Data) ->
    events(write_internal(Handle, Data)).

-doc """
PLACEHOLDER
""".
-spec read(handle()) -> {[event()], erlang:iovec()}.
read(Handle) ->
    {Events, Res} = read_internal(Handle, all),
    {events(Events), Res}.

-doc """
PLACEHOLDER
""".
-spec read(handle(), non_neg_integer()) -> {[event()],
                                            {incomplete, non_neg_integer()}
                                            | erlang:iovec()}.
read(Handle, Size) ->
    {Events, Res} = read_internal(Handle, Size),
    {events(Events), Res}.

write_internal(_Handle, _Data) ->
    erlang:nif_error(undefined).
read_internal(_Handle, _Size) ->
    erlang:nif_error(undefined).

%% Converts the returned event mask to a list. Note that some events have
%% precedence and keep others from appearing; for example 'notify' is pointless
%% when the stream is closed.
%%
%% At present it only makes sense to return one event rather than a list, but
%% this may change during the course of the experiment: if we make streams
%% bounded, it could make sense to return both 'blocked' and 'notify'.
events(Events) when (Events band ?ERTS_IO_STREAM_EVENT_CLOSED) =/= 0 ->
    [closed];
events(Events) when (Events band ?ERTS_IO_STREAM_EVENT_NOTIFY) =/= 0 ->
    [notify];
events(0) ->
    [].
