%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2019. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html">wxAuiManagerEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>aui_pane_button</em>, <em>aui_pane_close</em>, <em>aui_pane_maximize</em>, <em>aui_pane_restore</em>, <em>aui_pane_activated</em>, <em>aui_render</em>, <em>aui_find_manager</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxAuiManager(). #wxAuiManager{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxAuiManagerEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxAuiManagerEvent).
-include("wxe.hrl").
-export([canVeto/1,getButton/1,getDC/1,getManager/1,getPane/1,getVeto/1,setButton/2,
  setCanVeto/2,setDC/2,setManager/2,setPane/2,veto/1,veto/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-export_type([wxAuiManagerEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxAuiManagerEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetmanager">external documentation</a>.
-spec setManager(This, Mgr) -> 'ok' when
	This::wxAuiManagerEvent(), Mgr::wxAuiManager:wxAuiManager().
setManager(#wx_ref{type=ThisT}=This,#wx_ref{type=MgrT}=Mgr) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(MgrT,wxAuiManager),
  wxe_util:queue_cmd(This,Mgr,?get_env(),?wxAuiManagerEvent_SetManager).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetmanager">external documentation</a>.
-spec getManager(This) -> wxAuiManager:wxAuiManager() when
	This::wxAuiManagerEvent().
getManager(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetManager),
  wxe_util:rec(?wxAuiManagerEvent_GetManager).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetpane">external documentation</a>.
-spec setPane(This, P) -> 'ok' when
	This::wxAuiManagerEvent(), P::wxAuiPaneInfo:wxAuiPaneInfo().
setPane(#wx_ref{type=ThisT}=This,#wx_ref{type=PT}=P) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(PT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,P,?get_env(),?wxAuiManagerEvent_SetPane).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetpane">external documentation</a>.
-spec getPane(This) -> wxAuiPaneInfo:wxAuiPaneInfo() when
	This::wxAuiManagerEvent().
getPane(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetPane),
  wxe_util:rec(?wxAuiManagerEvent_GetPane).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetbutton">external documentation</a>.
-spec setButton(This, B) -> 'ok' when
	This::wxAuiManagerEvent(), B::integer().
setButton(#wx_ref{type=ThisT}=This,B)
 when is_integer(B) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,B,?get_env(),?wxAuiManagerEvent_SetButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetbutton">external documentation</a>.
-spec getButton(This) -> integer() when
	This::wxAuiManagerEvent().
getButton(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetButton),
  wxe_util:rec(?wxAuiManagerEvent_GetButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetdc">external documentation</a>.
-spec setDC(This, Pdc) -> 'ok' when
	This::wxAuiManagerEvent(), Pdc::wxDC:wxDC().
setDC(#wx_ref{type=ThisT}=This,#wx_ref{type=PdcT}=Pdc) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(PdcT,wxDC),
  wxe_util:queue_cmd(This,Pdc,?get_env(),?wxAuiManagerEvent_SetDC).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetdc">external documentation</a>.
-spec getDC(This) -> wxDC:wxDC() when
	This::wxAuiManagerEvent().
getDC(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetDC),
  wxe_util:rec(?wxAuiManagerEvent_GetDC).

%% @equiv veto(This, [])
-spec veto(This) -> 'ok' when
	This::wxAuiManagerEvent().

veto(This)
 when is_record(This, wx_ref) ->
  veto(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventveto">external documentation</a>.
-spec veto(This, [Option]) -> 'ok' when
	This::wxAuiManagerEvent(),
	Option :: {'veto', boolean()}.
veto(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  MOpts = fun({veto, _veto} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxAuiManagerEvent_Veto).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetveto">external documentation</a>.
-spec getVeto(This) -> boolean() when
	This::wxAuiManagerEvent().
getVeto(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_GetVeto),
  wxe_util:rec(?wxAuiManagerEvent_GetVeto).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetcanveto">external documentation</a>.
-spec setCanVeto(This, Can_veto) -> 'ok' when
	This::wxAuiManagerEvent(), Can_veto::boolean().
setCanVeto(#wx_ref{type=ThisT}=This,Can_veto)
 when is_boolean(Can_veto) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,Can_veto,?get_env(),?wxAuiManagerEvent_SetCanVeto).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventcanveto">external documentation</a>.
-spec canVeto(This) -> boolean() when
	This::wxAuiManagerEvent().
canVeto(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManagerEvent_CanVeto),
  wxe_util:rec(?wxAuiManagerEvent_CanVeto).

 %% From wxEvent
%% @hidden
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
skip(This) -> wxEvent:skip(This).
%% @hidden
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
getId(This) -> wxEvent:getId(This).
