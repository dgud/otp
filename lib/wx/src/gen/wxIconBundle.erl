%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2019. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html">wxIconBundle</a>.
%% @type wxIconBundle().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxIconBundle).
-include("wxe.hrl").
-export([addIcon/2,addIcon/3,destroy/1,getIcon/1,getIcon/2,new/0,new/1,new/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxIconBundle/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxIconBundle() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
-spec new() -> wxIconBundle().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxIconBundle_new_0),
  wxe_util:rec(?wxIconBundle_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
-spec new(Ic) -> wxIconBundle() when
	Ic::wxIconBundle() | wxIcon:wxIcon().
new(#wx_ref{type=IcT}=Ic) ->
  IcOP = case ?CLASS_T(IcT,wxIconBundle) of
     true ->
       ?wxIconBundle_new_1_1;
     _ -> ?CLASS(IcT,wxIcon),
       ?wxIconBundle_new_1_0
     end,
  wxe_util:queue_cmd(Ic,?get_env(),IcOP),
  wxe_util:rec(IcOP).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlewxiconbundle">external documentation</a>.
-spec new(File, Type) -> wxIconBundle() when
	File::unicode:chardata(), Type::integer().
new(File,Type)
 when ?is_chardata(File),is_integer(Type) ->
  File_UC = unicode:characters_to_binary([File,0]),
  wxe_util:queue_cmd(File_UC,Type,?get_env(),?wxIconBundle_new_2),
  wxe_util:rec(?wxIconBundle_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundleaddicon">external documentation</a>.
-spec addIcon(This, Icon) -> 'ok' when
	This::wxIconBundle(), Icon::wxIcon:wxIcon().
addIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon) ->
  ?CLASS(ThisT,wxIconBundle),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,?get_env(),?wxIconBundle_AddIcon_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundleaddicon">external documentation</a>.
-spec addIcon(This, File, Type) -> 'ok' when
	This::wxIconBundle(), File::unicode:chardata(), Type::integer().
addIcon(#wx_ref{type=ThisT}=This,File,Type)
 when ?is_chardata(File),is_integer(Type) ->
  ?CLASS(ThisT,wxIconBundle),
  File_UC = unicode:characters_to_binary([File,0]),
  wxe_util:queue_cmd(This,File_UC,Type,?get_env(),?wxIconBundle_AddIcon_2).

%% @equiv getIcon(This, [])
-spec getIcon(This) -> wxIcon:wxIcon() when
	This::wxIconBundle().

getIcon(This)
 when is_record(This, wx_ref) ->
  getIcon(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxiconbundle.html#wxiconbundlegeticon">external documentation</a>.
%% <br /> Also:<br />
%% getIcon(This, Size) -> wxIcon:wxIcon() when<br />
%% 	This::wxIconBundle(), Size::{W::integer(), H::integer()}.<br />
%% 
-spec getIcon(This, [Option]) -> wxIcon:wxIcon() when
	This::wxIconBundle(),
	Option :: {'size', integer()};
      (This, Size) -> wxIcon:wxIcon() when
	This::wxIconBundle(), Size::{W::integer(), H::integer()}.
getIcon(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxIconBundle),
  wxe_util:queue_cmd(This, Options,?get_env(),?wxIconBundle_GetIcon_1_0),
  wxe_util:rec(?wxIconBundle_GetIcon_1_0);
getIcon(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxIconBundle),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxIconBundle_GetIcon_1_1),
  wxe_util:rec(?wxIconBundle_GetIcon_1_1).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxIconBundle()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxIconBundle),
  wxe_util:destroy(?wxIconBundle_destruct,Obj),
  ok.
