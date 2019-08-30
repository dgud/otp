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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html">wxImageList</a>.
%% @type wxImageList().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxImageList).
-include("wxe.hrl").
-export([add/2,add/3,create/3,create/4,destroy/1,draw/5,draw/6,getBitmap/2,getIcon/2,
  getImageCount/1,getSize/2,new/0,new/2,new/3,remove/2,removeAll/1,replace/3,
  replace/4]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxImageList/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxImageList() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistwximagelist">external documentation</a>.
-spec new() -> wxImageList().
new() ->
  wxe_util:construct(?wxImageList_new_0,[]).

%% @equiv new(Width,Height, [])
-spec new(Width, Height) -> wxImageList() when
	Width::integer(), Height::integer().

new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistwximagelist">external documentation</a>.
-spec new(Width, Height, [Option]) -> wxImageList() when
	Width::integer(), Height::integer(),
	Option :: {'mask', boolean()}
		 | {'initialCount', integer()}.
new(Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  wxe_util:construct(?wxImageList_new_3,[Width,Height, Options]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistadd">external documentation</a>.
-spec add(This, Bitmap) -> integer() when
	This::wxImageList(), Bitmap::wxBitmap:wxBitmap().
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxImageList_Add_1,[ThisRef,BitmapRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistadd">external documentation</a>.
%% <br /> Also:<br />
%% add(This, Bitmap, MaskColour) -> integer() when<br />
%% 	This::wxImageList(), Bitmap::wxBitmap:wxBitmap(), MaskColour::wx:wx_colour().<br />
%% 
-spec add(This, Bitmap, Mask) -> integer() when
	This::wxImageList(), Bitmap::wxBitmap:wxBitmap(), Mask::wxBitmap:wxBitmap();
      (This, Bitmap, MaskColour) -> integer() when
	This::wxImageList(), Bitmap::wxBitmap:wxBitmap(), MaskColour::wx:wx_colour().
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef},#wx_ref{type=MaskT,ref=MaskRef}) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(MaskT,wxBitmap),
  wxe_util:call(?wxImageList_Add_2_0,[ThisRef,BitmapRef,MaskRef]);
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef},MaskColour)
 when tuple_size(MaskColour) =:= 3; tuple_size(MaskColour) =:= 4 ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxImageList_Add_2_1,[ThisRef,BitmapRef,MaskColour]).

%% @equiv create(This,Width,Height, [])
-spec create(This, Width, Height) -> boolean() when
	This::wxImageList(), Width::integer(), Height::integer().

create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistcreate">external documentation</a>.
-spec create(This, Width, Height, [Option]) -> boolean() when
	This::wxImageList(), Width::integer(), Height::integer(),
	Option :: {'mask', boolean()}
		 | {'initialCount', integer()}.
create(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_Create,[ThisRef,Width,Height, Options]).

%% @equiv draw(This,Index,Dc,X,Y, [])
-spec draw(This, Index, Dc, X, Y) -> boolean() when
	This::wxImageList(), Index::integer(), Dc::wxDC:wxDC(), X::integer(), Y::integer().

draw(This,Index,Dc,X,Y)
 when is_record(This, wx_ref),is_integer(Index),is_record(Dc, wx_ref),is_integer(X),is_integer(Y) ->
  draw(This,Index,Dc,X,Y, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistdraw">external documentation</a>.
-spec draw(This, Index, Dc, X, Y, [Option]) -> boolean() when
	This::wxImageList(), Index::integer(), Dc::wxDC:wxDC(), X::integer(), Y::integer(),
	Option :: {'flags', integer()}
		 | {'solidBackground', boolean()}.
draw(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=DcT,ref=DcRef},X,Y, Options)
 when is_integer(Index),is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(DcT,wxDC),
  wxe_util:call(?wxImageList_Draw,[ThisRef,Index,DcRef,X,Y, Options]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistgetbitmap">external documentation</a>.
-spec getBitmap(This, Index) -> wxBitmap:wxBitmap() when
	This::wxImageList(), Index::integer().
getBitmap(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_GetBitmap,[ThisRef,Index]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistgeticon">external documentation</a>.
-spec getIcon(This, Index) -> wxIcon:wxIcon() when
	This::wxImageList(), Index::integer().
getIcon(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_GetIcon,[ThisRef,Index]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistgetimagecount">external documentation</a>.
-spec getImageCount(This) -> integer() when
	This::wxImageList().
getImageCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_GetImageCount,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistgetsize">external documentation</a>.
-spec getSize(This, Index) -> Result when
	Result ::{Res ::boolean(), Width::integer(), Height::integer()},
	This::wxImageList(), Index::integer().
getSize(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_GetSize,[ThisRef,Index]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistremove">external documentation</a>.
-spec remove(This, Index) -> boolean() when
	This::wxImageList(), Index::integer().
remove(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_Remove,[ThisRef,Index]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistremoveall">external documentation</a>.
-spec removeAll(This) -> boolean() when
	This::wxImageList().
removeAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_RemoveAll,[ThisRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistreplace">external documentation</a>.
-spec replace(This, Index, Bitmap) -> boolean() when
	This::wxImageList(), Index::integer(), Bitmap::wxBitmap:wxBitmap().
replace(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=BitmapT,ref=BitmapRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxImageList_Replace_2,[ThisRef,Index,BitmapRef]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wximagelist.html#wximagelistreplace">external documentation</a>.
-spec replace(This, Index, Bitmap, Mask) -> boolean() when
	This::wxImageList(), Index::integer(), Bitmap::wxBitmap:wxBitmap(), Mask::wxBitmap:wxBitmap().
replace(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=BitmapT,ref=BitmapRef},#wx_ref{type=MaskT,ref=MaskRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(MaskT,wxBitmap),
  wxe_util:call(?wxImageList_Replace_3,[ThisRef,Index,BitmapRef,MaskRef]).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxImageList()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxImageList),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
