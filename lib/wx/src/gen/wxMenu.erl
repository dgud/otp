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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html">wxMenu</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxMenu().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxMenu).
-include("wxe.hrl").
-export(['Destroy'/2,append/2,append/3,append/4,append/5,appendCheckItem/3,appendCheckItem/4,
  appendRadioItem/3,appendRadioItem/4,appendSeparator/1,break/1,check/3,
  delete/2,destroy/1,enable/3,findItem/2,findItemByPosition/2,getHelpString/2,
  getLabel/2,getMenuItemCount/1,getMenuItems/1,getTitle/1,insert/3,insert/4,
  insert/5,insert/6,insertCheckItem/4,insertCheckItem/5,insertRadioItem/4,
  insertRadioItem/5,insertSeparator/2,isChecked/2,isEnabled/2,new/0,
  new/1,new/2,prepend/2,prepend/3,prepend/4,prepend/5,prependCheckItem/3,
  prependCheckItem/4,prependRadioItem/3,prependRadioItem/4,prependSeparator/1,
  remove/2,setHelpString/3,setLabel/3,setTitle/2]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-export_type([wxMenu/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxMenu() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxMenu().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
-spec new([Option]) -> wxMenu() when
	Option :: {'style', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({style, _style} = Arg, Acc) -> [Arg|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxMenu_new_1),
  wxe_util:rec(?wxMenu_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
-spec new(Title, [Option]) -> wxMenu() when
	Title::unicode:chardata(),
	Option :: {'style', integer()}.
new(Title, Options)
 when ?is_chardata(Title),is_list(Options) ->
  Title_UC = unicode:characters_to_binary(Title),
  MOpts = fun({style, _style} = Arg, Acc) -> [Arg|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(Title_UC, Opts,?get_env(),?wxMenu_new_2),
  wxe_util:rec(?wxMenu_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
-spec append(This, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
append(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Append_1),
  wxe_util:rec(?wxMenu_Append_1).

%% @equiv append(This,Itemid,Text, [])
-spec append(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

append(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),?is_chardata(Text) ->
  append(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
%% <br /> Also:<br />
%% append(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),<br />
%% 	Option :: {'help', unicode:chardata()}<br />
%% 		 | {'kind', wx:wx_enum()}.<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec append(This, Itemid, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu();
      (This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.

append(This,Itemid,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Itemid),?is_chardata(Text),is_record(Submenu, wx_ref) ->
  append(This,Itemid,Text,Submenu, []);
append(#wx_ref{type=ThisT}=This,Itemid,Text, Options)
 when is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          ({kind, _kind} = Arg, Acc) -> [Arg|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Itemid,Text_UC, Opts,?get_env(),?wxMenu_Append_3),
  wxe_util:rec(?wxMenu_Append_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
%% <br /> Also:<br />
%% append(This, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),<br />
%% 	Option :: {'help', unicode:chardata()}.<br />
%% 
-spec append(This, Itemid, Text, Help, IsCheckable) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Help::unicode:chardata(), IsCheckable::boolean();
      (This, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
append(#wx_ref{type=ThisT}=This,Itemid,Text,Help,IsCheckable)
 when is_integer(Itemid),?is_chardata(Text),?is_chardata(Help),is_boolean(IsCheckable) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  Help_UC = unicode:characters_to_binary(Help),
  wxe_util:queue_cmd(This,Itemid,Text_UC,Help_UC,IsCheckable,?get_env(),?wxMenu_Append_4_0);
append(#wx_ref{type=ThisT}=This,Itemid,Text,#wx_ref{type=SubmenuT}=Submenu, Options)
 when is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  ?CLASS(SubmenuT,wxMenu),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Itemid,Text_UC,Submenu, Opts,?get_env(),?wxMenu_Append_4_1),
  wxe_util:rec(?wxMenu_Append_4_1).

%% @equiv appendCheckItem(This,Itemid,Text, [])
-spec appendCheckItem(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

appendCheckItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),?is_chardata(Text) ->
  appendCheckItem(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendcheckitem">external documentation</a>.
-spec appendCheckItem(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
appendCheckItem(#wx_ref{type=ThisT}=This,Itemid,Text, Options)
 when is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Itemid,Text_UC, Opts,?get_env(),?wxMenu_AppendCheckItem),
  wxe_util:rec(?wxMenu_AppendCheckItem).

%% @equiv appendRadioItem(This,Itemid,Text, [])
-spec appendRadioItem(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

appendRadioItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),?is_chardata(Text) ->
  appendRadioItem(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendradioitem">external documentation</a>.
-spec appendRadioItem(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
appendRadioItem(#wx_ref{type=ThisT}=This,Itemid,Text, Options)
 when is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Itemid,Text_UC, Opts,?get_env(),?wxMenu_AppendRadioItem),
  wxe_util:rec(?wxMenu_AppendRadioItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendseparator">external documentation</a>.
-spec appendSeparator(This) -> wxMenuItem:wxMenuItem() when
	This::wxMenu().
appendSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_AppendSeparator),
  wxe_util:rec(?wxMenu_AppendSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenubreak">external documentation</a>.
-spec break(This) -> 'ok' when
	This::wxMenu().
break(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_Break).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenucheck">external documentation</a>.
-spec check(This, Itemid, Check) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Check::boolean().
check(#wx_ref{type=ThisT}=This,Itemid,Check)
 when is_integer(Itemid),is_boolean(Check) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,Check,?get_env(),?wxMenu_Check).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenudelete">external documentation</a>.
%% <br /> Also:<br />
%% delete(This, Item) -> boolean() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec delete(This, Itemid) -> boolean() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> boolean() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
delete(#wx_ref{type=ThisT}=This,Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,?get_env(),?wxMenu_Delete_1_0),
  wxe_util:rec(?wxMenu_Delete_1_0);
delete(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Delete_1_1),
  wxe_util:rec(?wxMenu_Delete_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenudestroy">external documentation</a>.
%% <br /> Also:<br />
%% 'Destroy'(This, Item) -> boolean() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec 'Destroy'(This, Itemid) -> boolean() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> boolean() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
'Destroy'(#wx_ref{type=ThisT}=This,Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,?get_env(),?wxMenu_Destroy_1_0),
  wxe_util:rec(?wxMenu_Destroy_1_0);
'Destroy'(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Destroy_1_1),
  wxe_util:rec(?wxMenu_Destroy_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuenable">external documentation</a>.
-spec enable(This, Itemid, Enable) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Enable::boolean().
enable(#wx_ref{type=ThisT}=This,Itemid,Enable)
 when is_integer(Itemid),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,Enable,?get_env(),?wxMenu_Enable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenufinditem">external documentation</a>.
%% <br /> Also:<br />
%% findItem(This, Item) -> integer() when<br />
%% 	This::wxMenu(), Item::unicode:chardata().<br />
%% 
-spec findItem(This, Itemid) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> integer() when
	This::wxMenu(), Item::unicode:chardata().
findItem(#wx_ref{type=ThisT}=This,Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,?get_env(),?wxMenu_FindItem_2),
  wxe_util:rec(?wxMenu_FindItem_2);
findItem(#wx_ref{type=ThisT}=This,Item)
 when ?is_chardata(Item) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  wxe_util:queue_cmd(This,Item_UC,?get_env(),?wxMenu_FindItem_1),
  wxe_util:rec(?wxMenu_FindItem_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenufinditembyposition">external documentation</a>.
-spec findItemByPosition(This, Position) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Position::integer().
findItemByPosition(#wx_ref{type=ThisT}=This,Position)
 when is_integer(Position) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Position,?get_env(),?wxMenu_FindItemByPosition),
  wxe_util:rec(?wxMenu_FindItemByPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugethelpstring">external documentation</a>.
-spec getHelpString(This, Itemid) -> unicode:charlist() when
	This::wxMenu(), Itemid::integer().
getHelpString(#wx_ref{type=ThisT}=This,Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,?get_env(),?wxMenu_GetHelpString),
  wxe_util:rec(?wxMenu_GetHelpString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetlabel">external documentation</a>.
-spec getLabel(This, Itemid) -> unicode:charlist() when
	This::wxMenu(), Itemid::integer().
getLabel(#wx_ref{type=ThisT}=This,Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,?get_env(),?wxMenu_GetLabel),
  wxe_util:rec(?wxMenu_GetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetmenuitemcount">external documentation</a>.
-spec getMenuItemCount(This) -> integer() when
	This::wxMenu().
getMenuItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetMenuItemCount),
  wxe_util:rec(?wxMenu_GetMenuItemCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetmenuitems">external documentation</a>.
-spec getMenuItems(This) -> [wxMenuItem:wxMenuItem()] when
	This::wxMenu().
getMenuItems(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetMenuItems),
  wxe_util:rec(?wxMenu_GetMenuItems).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugettitle">external documentation</a>.
-spec getTitle(This) -> unicode:charlist() when
	This::wxMenu().
getTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetTitle),
  wxe_util:rec(?wxMenu_GetTitle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%% <br /> Also:<br />
%% insert(This, Pos, Item) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Pos::integer(), Item::wxMenuItem:wxMenuItem().<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec insert(This, Pos, Itemid) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer();
      (This, Pos, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Item::wxMenuItem:wxMenuItem().

insert(This,Pos,Itemid)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid) ->
  insert(This,Pos,Itemid, []);
insert(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=ItemT}=Item)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Pos,Item,?get_env(),?wxMenu_Insert_2),
  wxe_util:rec(?wxMenu_Insert_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec insert(This, Pos, Itemid, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(),
	Option :: {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.
insert(#wx_ref{type=ThisT}=This,Pos,Itemid, Options)
 when is_integer(Pos),is_integer(Itemid),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  MOpts = fun({text, Text}, Acc) ->   Text_UC = unicode:characters_to_binary(Text),[{text,Text_UC}|Acc];
          ({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          ({kind, _kind} = Arg, Acc) -> [Arg|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Pos,Itemid, Opts,?get_env(),?wxMenu_Insert_3),
  wxe_util:rec(?wxMenu_Insert_3).

%% @equiv insert(This,Pos,Itemid,Text,Submenu, [])
-spec insert(This, Pos, Itemid, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu().

insert(This,Pos,Itemid,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),?is_chardata(Text),is_record(Submenu, wx_ref) ->
  insert(This,Pos,Itemid,Text,Submenu, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%% <br /> Also:<br />
%% insert(This, Pos, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),<br />
%% 	Option :: {'help', unicode:chardata()}.<br />
%% 
-spec insert(This, Pos, Itemid, Text, Help, IsCheckable) -> 'ok' when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(), Help::unicode:chardata(), IsCheckable::boolean();
      (This, Pos, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
insert(#wx_ref{type=ThisT}=This,Pos,Itemid,Text,Help,IsCheckable)
 when is_integer(Pos),is_integer(Itemid),?is_chardata(Text),?is_chardata(Help),is_boolean(IsCheckable) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  Help_UC = unicode:characters_to_binary(Help),
  wxe_util:queue_cmd(This,Pos,Itemid,Text_UC,Help_UC,IsCheckable,?get_env(),?wxMenu_Insert_5_0);
insert(#wx_ref{type=ThisT}=This,Pos,Itemid,Text,#wx_ref{type=SubmenuT}=Submenu, Options)
 when is_integer(Pos),is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  ?CLASS(SubmenuT,wxMenu),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Pos,Itemid,Text_UC,Submenu, Opts,?get_env(),?wxMenu_Insert_5_1),
  wxe_util:rec(?wxMenu_Insert_5_1).

%% @equiv insertCheckItem(This,Pos,Itemid,Text, [])
-spec insertCheckItem(This, Pos, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata().

insertCheckItem(This,Pos,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),?is_chardata(Text) ->
  insertCheckItem(This,Pos,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertcheckitem">external documentation</a>.
-spec insertCheckItem(This, Pos, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
insertCheckItem(#wx_ref{type=ThisT}=This,Pos,Itemid,Text, Options)
 when is_integer(Pos),is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Pos,Itemid,Text_UC, Opts,?get_env(),?wxMenu_InsertCheckItem),
  wxe_util:rec(?wxMenu_InsertCheckItem).

%% @equiv insertRadioItem(This,Pos,Itemid,Text, [])
-spec insertRadioItem(This, Pos, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata().

insertRadioItem(This,Pos,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),?is_chardata(Text) ->
  insertRadioItem(This,Pos,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertradioitem">external documentation</a>.
-spec insertRadioItem(This, Pos, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
insertRadioItem(#wx_ref{type=ThisT}=This,Pos,Itemid,Text, Options)
 when is_integer(Pos),is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Pos,Itemid,Text_UC, Opts,?get_env(),?wxMenu_InsertRadioItem),
  wxe_util:rec(?wxMenu_InsertRadioItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertseparator">external documentation</a>.
-spec insertSeparator(This, Pos) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer().
insertSeparator(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenu_InsertSeparator),
  wxe_util:rec(?wxMenu_InsertSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuischecked">external documentation</a>.
-spec isChecked(This, Itemid) -> boolean() when
	This::wxMenu(), Itemid::integer().
isChecked(#wx_ref{type=ThisT}=This,Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,?get_env(),?wxMenu_IsChecked),
  wxe_util:rec(?wxMenu_IsChecked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuisenabled">external documentation</a>.
-spec isEnabled(This, Itemid) -> boolean() when
	This::wxMenu(), Itemid::integer().
isEnabled(#wx_ref{type=ThisT}=This,Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,?get_env(),?wxMenu_IsEnabled),
  wxe_util:rec(?wxMenu_IsEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%% <br /> Also:<br />
%% prepend(This, Item) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec prepend(This, Itemid) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().

prepend(This,Itemid)
 when is_record(This, wx_ref),is_integer(Itemid) ->
  prepend(This,Itemid, []);
prepend(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Prepend_1),
  wxe_util:rec(?wxMenu_Prepend_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec prepend(This, Itemid, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(),
	Option :: {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.
prepend(#wx_ref{type=ThisT}=This,Itemid, Options)
 when is_integer(Itemid),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  MOpts = fun({text, Text}, Acc) ->   Text_UC = unicode:characters_to_binary(Text),[{text,Text_UC}|Acc];
          ({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          ({kind, _kind} = Arg, Acc) -> [Arg|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Itemid, Opts,?get_env(),?wxMenu_Prepend_2),
  wxe_util:rec(?wxMenu_Prepend_2).

%% @equiv prepend(This,Itemid,Text,Submenu, [])
-spec prepend(This, Itemid, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu().

prepend(This,Itemid,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Itemid),?is_chardata(Text),is_record(Submenu, wx_ref) ->
  prepend(This,Itemid,Text,Submenu, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%% <br /> Also:<br />
%% prepend(This, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),<br />
%% 	Option :: {'help', unicode:chardata()}.<br />
%% 
-spec prepend(This, Itemid, Text, Help, IsCheckable) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Help::unicode:chardata(), IsCheckable::boolean();
      (This, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
prepend(#wx_ref{type=ThisT}=This,Itemid,Text,Help,IsCheckable)
 when is_integer(Itemid),?is_chardata(Text),?is_chardata(Help),is_boolean(IsCheckable) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  Help_UC = unicode:characters_to_binary(Help),
  wxe_util:queue_cmd(This,Itemid,Text_UC,Help_UC,IsCheckable,?get_env(),?wxMenu_Prepend_4_0);
prepend(#wx_ref{type=ThisT}=This,Itemid,Text,#wx_ref{type=SubmenuT}=Submenu, Options)
 when is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  ?CLASS(SubmenuT,wxMenu),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Itemid,Text_UC,Submenu, Opts,?get_env(),?wxMenu_Prepend_4_1),
  wxe_util:rec(?wxMenu_Prepend_4_1).

%% @equiv prependCheckItem(This,Itemid,Text, [])
-spec prependCheckItem(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

prependCheckItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),?is_chardata(Text) ->
  prependCheckItem(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependcheckitem">external documentation</a>.
-spec prependCheckItem(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
prependCheckItem(#wx_ref{type=ThisT}=This,Itemid,Text, Options)
 when is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Itemid,Text_UC, Opts,?get_env(),?wxMenu_PrependCheckItem),
  wxe_util:rec(?wxMenu_PrependCheckItem).

%% @equiv prependRadioItem(This,Itemid,Text, [])
-spec prependRadioItem(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

prependRadioItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),?is_chardata(Text) ->
  prependRadioItem(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependradioitem">external documentation</a>.
-spec prependRadioItem(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
prependRadioItem(#wx_ref{type=ThisT}=This,Itemid,Text, Options)
 when is_integer(Itemid),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary(Help),[{help,Help_UC}|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:foldr(MOpts, [], Options),
  wxe_util:queue_cmd(This,Itemid,Text_UC, Opts,?get_env(),?wxMenu_PrependRadioItem),
  wxe_util:rec(?wxMenu_PrependRadioItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependseparator">external documentation</a>.
-spec prependSeparator(This) -> wxMenuItem:wxMenuItem() when
	This::wxMenu().
prependSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_PrependSeparator),
  wxe_util:rec(?wxMenu_PrependSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuremove">external documentation</a>.
%% <br /> Also:<br />
%% remove(This, Item) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec remove(This, Itemid) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
remove(#wx_ref{type=ThisT}=This,Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Itemid,?get_env(),?wxMenu_Remove_1_0),
  wxe_util:rec(?wxMenu_Remove_1_0);
remove(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Remove_1_1),
  wxe_util:rec(?wxMenu_Remove_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusethelpstring">external documentation</a>.
-spec setHelpString(This, Itemid, HelpString) -> 'ok' when
	This::wxMenu(), Itemid::integer(), HelpString::unicode:chardata().
setHelpString(#wx_ref{type=ThisT}=This,Itemid,HelpString)
 when is_integer(Itemid),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxMenu),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,Itemid,HelpString_UC,?get_env(),?wxMenu_SetHelpString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusetlabel">external documentation</a>.
-spec setLabel(This, Itemid, Label) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT}=This,Itemid,Label)
 when is_integer(Itemid),?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenu),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Itemid,Label_UC,?get_env(),?wxMenu_SetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusettitle">external documentation</a>.
-spec setTitle(This, Title) -> 'ok' when
	This::wxMenu(), Title::unicode:chardata().
setTitle(#wx_ref{type=ThisT}=This,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Title_UC,?get_env(),?wxMenu_SetTitle).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxMenu()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenu),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxEvtHandler
%% @hidden
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
