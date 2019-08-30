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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html">wxXmlResource</a>.
%% @type wxXmlResource().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxXmlResource).
-include("wxe.hrl").
-export([ xrcctrl/3 ,attachUnknownControl/3,attachUnknownControl/4,clearHandlers/1,
  compareVersion/5,destroy/1,get/0,getFlags/1,getVersion/1,getXRCID/1,
  getXRCID/2,initAllHandlers/1,load/2,loadBitmap/2,loadDialog/3,loadDialog/4,
  loadFrame/3,loadFrame/4,loadIcon/2,loadMenu/2,loadMenuBar/2,loadMenuBar/3,
  loadPanel/3,loadPanel/4,loadToolBar/3,new/0,new/1,new/2,set/1,setFlags/2,
  unload/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxXmlResource/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxXmlResource() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxXmlResource().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcewxxmlresource">external documentation</a>.
-spec new([Option]) -> wxXmlResource() when
	Option :: {'flags', integer()}
		 | {'domain', unicode:chardata()}.
new(Options)
 when is_list(Options) ->
  wxe_util:construct(?wxXmlResource_new_1,[Options]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcewxxmlresource">external documentation</a>.
-spec new(Filemask, [Option]) -> wxXmlResource() when
	Filemask::unicode:chardata(),
	Option :: {'flags', integer()}
		 | {'domain', unicode:chardata()}.
new(Filemask, Options)
 when ?is_chardata(Filemask),is_list(Options) ->
  Filemask_UC = unicode:characters_to_binary([Filemask,0]),
  wxe_util:construct(?wxXmlResource_new_2,[Filemask_UC, Options]).

%% @equiv attachUnknownControl(This,Name,Control, [])
-spec attachUnknownControl(This, Name, Control) -> boolean() when
	This::wxXmlResource(), Name::unicode:chardata(), Control::wxWindow:wxWindow().

attachUnknownControl(This,Name,Control)
 when is_record(This, wx_ref),?is_chardata(Name),is_record(Control, wx_ref) ->
  attachUnknownControl(This,Name,Control, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceattachunknowncontrol">external documentation</a>.
-spec attachUnknownControl(This, Name, Control, [Option]) -> boolean() when
	This::wxXmlResource(), Name::unicode:chardata(), Control::wxWindow:wxWindow(),
	Option :: {'parent', wxWindow:wxWindow()}.
attachUnknownControl(#wx_ref{type=ThisT,ref=ThisRef},Name,#wx_ref{type=ControlT,ref=ControlRef}, Options)
 when ?is_chardata(Name),is_list(Options) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  ?CLASS(ControlT,wxWindow),
  wxe_util:call(?wxXmlResource_AttachUnknownControl,[ThisRef,Name_UC,ControlRef, Options]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceclearhandlers">external documentation</a>.
-spec clearHandlers(This) -> 'ok' when
	This::wxXmlResource().
clearHandlers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_ClearHandlers,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcecompareversion">external documentation</a>.
-spec compareVersion(This, Major, Minor, Release, Revision) -> integer() when
	This::wxXmlResource(), Major::integer(), Minor::integer(), Release::integer(), Revision::integer().
compareVersion(#wx_ref{type=ThisT,ref=ThisRef},Major,Minor,Release,Revision)
 when is_integer(Major),is_integer(Minor),is_integer(Release),is_integer(Revision) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_CompareVersion,[ThisRef,Major,Minor,Release,Revision]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceget">external documentation</a>.
-spec get() -> wxXmlResource().
get() ->
  wxe_util:call(?wxXmlResource_Get,[]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetflags">external documentation</a>.
-spec getFlags(This) -> integer() when
	This::wxXmlResource().
getFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_GetFlags,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetversion">external documentation</a>.
-spec getVersion(This) -> integer() when
	This::wxXmlResource().
getVersion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_GetVersion,[ThisRef]).

%% @equiv getXRCID(Str_id, [])
-spec getXRCID(Str_id) -> integer() when
	Str_id::[unicode:chardata()].

getXRCID(Str_id)
 when is_list(Str_id) ->
  getXRCID(Str_id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetxrcid">external documentation</a>.
-spec getXRCID(Str_id, [Option]) -> integer() when
	Str_id::[unicode:chardata()],
	Option :: {'value_if_not_found', integer()}.
getXRCID(Str_id, Options)
 when is_list(Str_id),is_list(Options) ->
  Str_id_UC = unicode:characters_to_binary([Str_id,0]),
  wxe_util:call(?wxXmlResource_GetXRCID,[Str_id_UC, Options]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceinitallhandlers">external documentation</a>.
-spec initAllHandlers(This) -> 'ok' when
	This::wxXmlResource().
initAllHandlers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_InitAllHandlers,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceload">external documentation</a>.
-spec load(This, Filemask) -> boolean() when
	This::wxXmlResource(), Filemask::unicode:chardata().
load(#wx_ref{type=ThisT,ref=ThisRef},Filemask)
 when ?is_chardata(Filemask) ->
  ?CLASS(ThisT,wxXmlResource),
  Filemask_UC = unicode:characters_to_binary([Filemask,0]),
  wxe_util:call(?wxXmlResource_Load,[ThisRef,Filemask_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadbitmap">external documentation</a>.
-spec loadBitmap(This, Name) -> wxBitmap:wxBitmap() when
	This::wxXmlResource(), Name::unicode:chardata().
loadBitmap(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadBitmap,[ThisRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
-spec loadDialog(This, Parent, Name) -> wxDialog:wxDialog() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadDialog(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadDialog_2,[ThisRef,ParentRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
-spec loadDialog(This, Dlg, Parent, Name) -> boolean() when
	This::wxXmlResource(), Dlg::wxDialog:wxDialog(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadDialog(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DlgT,ref=DlgRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(DlgT,wxDialog),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadDialog_3,[ThisRef,DlgRef,ParentRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadframe">external documentation</a>.
-spec loadFrame(This, Parent, Name) -> wxFrame:wxFrame() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadFrame_2,[ThisRef,ParentRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadframe">external documentation</a>.
-spec loadFrame(This, Frame, Parent, Name) -> boolean() when
	This::wxXmlResource(), Frame::wxFrame:wxFrame(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(FrameT,wxFrame),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadFrame_3,[ThisRef,FrameRef,ParentRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadicon">external documentation</a>.
-spec loadIcon(This, Name) -> wxIcon:wxIcon() when
	This::wxXmlResource(), Name::unicode:chardata().
loadIcon(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadIcon,[ThisRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenu">external documentation</a>.
-spec loadMenu(This, Name) -> wxMenu:wxMenu() when
	This::wxXmlResource(), Name::unicode:chardata().
loadMenu(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenu,[ThisRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenubar">external documentation</a>.
-spec loadMenuBar(This, Name) -> wxMenuBar:wxMenuBar() when
	This::wxXmlResource(), Name::unicode:chardata().
loadMenuBar(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenuBar_1,[ThisRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenubar">external documentation</a>.
-spec loadMenuBar(This, Parent, Name) -> wxMenuBar:wxMenuBar() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadMenuBar(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenuBar_2,[ThisRef,ParentRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadpanel">external documentation</a>.
-spec loadPanel(This, Parent, Name) -> wxPanel:wxPanel() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadPanel(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadPanel_2,[ThisRef,ParentRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadpanel">external documentation</a>.
-spec loadPanel(This, Panel, Parent, Name) -> boolean() when
	This::wxXmlResource(), Panel::wxPanel:wxPanel(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadPanel(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PanelT,ref=PanelRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(PanelT,wxPanel),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadPanel_3,[ThisRef,PanelRef,ParentRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadtoolbar">external documentation</a>.
-spec loadToolBar(This, Parent, Name) -> wxToolBar:wxToolBar() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadToolBar(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadToolBar,[ThisRef,ParentRef,Name_UC]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceset">external documentation</a>.
-spec set(Res) -> wxXmlResource() when
	Res::wxXmlResource().
set(#wx_ref{type=ResT,ref=ResRef}) ->
  ?CLASS(ResT,wxXmlResource),
  wxe_util:call(?wxXmlResource_Set,[ResRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcesetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxXmlResource(), Flags::integer().
setFlags(#wx_ref{type=ThisT,ref=ThisRef},Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_SetFlags,[ThisRef,Flags]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceunload">external documentation</a>.
-spec unload(This, Filename) -> boolean() when
	This::wxXmlResource(), Filename::unicode:chardata().
unload(#wx_ref{type=ThisT,ref=ThisRef},Filename)
 when ?is_chardata(Filename) ->
  ?CLASS(ThisT,wxXmlResource),
  Filename_UC = unicode:characters_to_binary([Filename,0]),
  wxe_util:call(?wxXmlResource_Unload,[ThisRef,Filename_UC]).


%% @doc Looks up a control with Name in a window created with XML
%% resources. You can use it to set/get values from controls.
%% The object is type casted to <b>Type</b>.
%% Example: <br />
%%  Xrc = wxXmlResource:get(), <br />
%%  Dlg = wxDialog:new(), <br />
%%  true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "controls_dialog"), <br />
%%  LCtrl = xrcctrl(Dlg, "controls_listctrl", wxListCtrl), <br />
%%  wxListCtrl:insertColumn(LCtrl, 0, "Name", [{width, 200}]), <br />
-spec xrcctrl(Window, Name, Type) -> wx:wx_object() when
      Window::wxWindow:wxWindow(),
      Name::string(),
      Type::atom().

xrcctrl(Window = #wx_ref{}, Name, Type) when is_list(Name), is_atom(Type) ->
    %% Func Id ?wxXmlResource_xrcctrl 
    ID  = wxXmlResource:getXRCID(Name),
    Res = wxWindow:findWindow(Window,ID),
    wx:typeCast(Res, Type).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxXmlResource()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxXmlResource),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
