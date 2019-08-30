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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html">wxBrush</a>.
%% @type wxBrush().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxBrush).
-include("wxe.hrl").
-export([destroy/1,getColour/1,getStipple/1,getStyle/1,isHatch/1,isOk/1,new/0,
  new/1,new/2,setColour/2,setColour/4,setStipple/2,setStyle/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxBrush/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxBrush() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushwxbrush">external documentation</a>.
-spec new() -> wxBrush().
new() ->
  wxe_util:construct(?wxBrush_new_0,[]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushwxbrush">external documentation</a>.
%% <br /> Also:<br />
%% new(StippleBitmap) -> wxBrush() when<br />
%% 	StippleBitmap::wxBitmap:wxBitmap().<br />
%% 
-spec new(Colour) -> wxBrush() when
	Colour::wx:wx_colour();
      (StippleBitmap) -> wxBrush() when
	StippleBitmap::wxBitmap:wxBitmap().

new(Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  new(Colour, []);
new(#wx_ref{type=StippleBitmapT,ref=StippleBitmapRef}) ->
  ?CLASS(StippleBitmapT,wxBitmap),
  wxe_util:construct(?wxBrush_new_1,[StippleBitmapRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushwxbrush">external documentation</a>.
-spec new(Colour, [Option]) -> wxBrush() when
	Colour::wx:wx_colour(),
	Option :: {'style', integer()}.
new(Colour, Options)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4,is_list(Options) ->
  wxe_util:construct(?wxBrush_new_2,[Colour, Options]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetcolour">external documentation</a>.
-spec getColour(This) -> wx:wx_colour4() when
	This::wxBrush().
getColour(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:call(?wxBrush_GetColour,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetstipple">external documentation</a>.
-spec getStipple(This) -> wxBitmap:wxBitmap() when
	This::wxBrush().
getStipple(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:call(?wxBrush_GetStipple,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushgetstyle">external documentation</a>.
-spec getStyle(This) -> integer() when
	This::wxBrush().
getStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:call(?wxBrush_GetStyle,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushishatch">external documentation</a>.
-spec isHatch(This) -> boolean() when
	This::wxBrush().
isHatch(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:call(?wxBrush_IsHatch,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxBrush().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:call(?wxBrush_IsOk,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetcolour">external documentation</a>.
-spec setColour(This, Col) -> 'ok' when
	This::wxBrush(), Col::wx:wx_colour().
setColour(#wx_ref{type=ThisT,ref=ThisRef},Col)
 when tuple_size(Col) =:= 3; tuple_size(Col) =:= 4 ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:cast(?wxBrush_SetColour_1,[ThisRef,Col]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetcolour">external documentation</a>.
-spec setColour(This, R, G, B) -> 'ok' when
	This::wxBrush(), R::integer(), G::integer(), B::integer().
setColour(#wx_ref{type=ThisT,ref=ThisRef},R,G,B)
 when is_integer(R),is_integer(G),is_integer(B) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:cast(?wxBrush_SetColour_3,[ThisRef,R,G,B]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetstipple">external documentation</a>.
-spec setStipple(This, Stipple) -> 'ok' when
	This::wxBrush(), Stipple::wxBitmap:wxBitmap().
setStipple(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=StippleT,ref=StippleRef}) ->
  ?CLASS(ThisT,wxBrush),
  ?CLASS(StippleT,wxBitmap),
  wxe_util:cast(?wxBrush_SetStipple,[ThisRef,StippleRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbrush.html#wxbrushsetstyle">external documentation</a>.
-spec setStyle(This, Style) -> 'ok' when
	This::wxBrush(), Style::integer().
setStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxBrush),
  wxe_util:cast(?wxBrush_SetStyle,[ThisRef,Style]).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxBrush()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBrush),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
