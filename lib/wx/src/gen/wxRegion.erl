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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html">wxRegion</a>.
%% @type wxRegion().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxRegion).
-include("wxe.hrl").
-export(['Xor'/2,'Xor'/5,clear/1,contains/2,contains/3,contains/5,convertToBitmap/1,
  destroy/1,getBox/1,intersect/2,intersect/5,isEmpty/1,new/0,new/1,new/2,
  new/4,offset/2,offset/3,subtract/2,subtract/5,union/2,union/3,union/4,
  union/5]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxRegion/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxRegion() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionwxregion">external documentation</a>.
-spec new() -> wxRegion().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxRegion_new_0),
  wxe_util:rec(?wxRegion_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionwxregion">external documentation</a>.
%% <br /> Also:<br />
%% new(Rect) -> wxRegion() when<br />
%% 	Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec new(Bmp) -> wxRegion() when
	Bmp::wxBitmap:wxBitmap();
      (Rect) -> wxRegion() when
	Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
new(#wx_ref{type=BmpT}=Bmp) ->
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(Bmp,?get_env(),?wxRegion_new_1_0),
  wxe_util:rec(?wxRegion_new_1_0);
new({RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  wxe_util:queue_cmd(Rect,?get_env(),?wxRegion_new_1_1),
  wxe_util:rec(?wxRegion_new_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionwxregion">external documentation</a>.
-spec new(TopLeft, BottomRight) -> wxRegion() when
	TopLeft::{X::integer(), Y::integer()}, BottomRight::{X::integer(), Y::integer()}.
new({TopLeftX,TopLeftY} = TopLeft,{BottomRightX,BottomRightY} = BottomRight)
 when is_integer(TopLeftX),is_integer(TopLeftY),is_integer(BottomRightX),is_integer(BottomRightY) ->
  wxe_util:queue_cmd(TopLeft,BottomRight,?get_env(),?wxRegion_new_2),
  wxe_util:rec(?wxRegion_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionwxregion">external documentation</a>.
-spec new(X, Y, W, H) -> wxRegion() when
	X::integer(), Y::integer(), W::integer(), H::integer().
new(X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  wxe_util:queue_cmd(X,Y,W,H,?get_env(),?wxRegion_new_4),
  wxe_util:rec(?wxRegion_new_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxRegion().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregioncontains">external documentation</a>.
%% <br /> Also:<br />
%% contains(This, Rect) -> wx:wx_enum() when<br />
%% 	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
%%<br /> Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
-spec contains(This, Pt) -> wx:wx_enum() when
	This::wxRegion(), Pt::{X::integer(), Y::integer()};
      (This, Rect) -> wx:wx_enum() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
contains(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxRegion_Contains_1_0),
  wxe_util:rec(?wxRegion_Contains_1_0);
contains(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Contains_1_1),
  wxe_util:rec(?wxRegion_Contains_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregioncontains">external documentation</a>.
%%<br /> Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
-spec contains(This, X, Y) -> wx:wx_enum() when
	This::wxRegion(), X::integer(), Y::integer().
contains(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxRegion_Contains_2),
  wxe_util:rec(?wxRegion_Contains_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregioncontains">external documentation</a>.
%%<br /> Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
-spec contains(This, X, Y, W, H) -> wx:wx_enum() when
	This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer().
contains(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxRegion_Contains_4),
  wxe_util:rec(?wxRegion_Contains_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionconverttobitmap">external documentation</a>.
-spec convertToBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxRegion().
convertToBitmap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_ConvertToBitmap),
  wxe_util:rec(?wxRegion_ConvertToBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregiongetbox">external documentation</a>.
-spec getBox(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxRegion().
getBox(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_GetBox),
  wxe_util:rec(?wxRegion_GetBox).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionintersect">external documentation</a>.
%% <br /> Also:<br />
%% intersect(This, Rect) -> boolean() when<br />
%% 	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec intersect(This, Region) -> boolean() when
	This::wxRegion(), Region::wxRegion();
      (This, Rect) -> boolean() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
intersect(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:queue_cmd(This,Region,?get_env(),?wxRegion_Intersect_1_0),
  wxe_util:rec(?wxRegion_Intersect_1_0);
intersect(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Intersect_1_1),
  wxe_util:rec(?wxRegion_Intersect_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionintersect">external documentation</a>.
-spec intersect(This, X, Y, W, H) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer().
intersect(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxRegion_Intersect_4),
  wxe_util:rec(?wxRegion_Intersect_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionisempty">external documentation</a>.
-spec isEmpty(This) -> boolean() when
	This::wxRegion().
isEmpty(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_IsEmpty),
  wxe_util:rec(?wxRegion_IsEmpty).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionsubtract">external documentation</a>.
%% <br /> Also:<br />
%% subtract(This, Rect) -> boolean() when<br />
%% 	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec subtract(This, Region) -> boolean() when
	This::wxRegion(), Region::wxRegion();
      (This, Rect) -> boolean() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
subtract(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:queue_cmd(This,Region,?get_env(),?wxRegion_Subtract_1_0),
  wxe_util:rec(?wxRegion_Subtract_1_0);
subtract(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Subtract_1_1),
  wxe_util:rec(?wxRegion_Subtract_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionsubtract">external documentation</a>.
-spec subtract(This, X, Y, W, H) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer().
subtract(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxRegion_Subtract_4),
  wxe_util:rec(?wxRegion_Subtract_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionoffset">external documentation</a>.
-spec offset(This, Pt) -> boolean() when
	This::wxRegion(), Pt::{X::integer(), Y::integer()}.
offset(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxRegion_Offset_1),
  wxe_util:rec(?wxRegion_Offset_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionoffset">external documentation</a>.
-spec offset(This, X, Y) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer().
offset(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxRegion_Offset_2),
  wxe_util:rec(?wxRegion_Offset_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionunion">external documentation</a>.
%% <br /> Also:<br />
%% union(This, Rect) -> boolean() when<br />
%% 	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec union(This, Region) -> boolean() when
	This::wxRegion(), Region::wxRegion() | wxBitmap:wxBitmap();
      (This, Rect) -> boolean() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
union(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxRegion),
  RegionOP = case ?CLASS_T(RegionT,wxRegion) of
     true ->
       ?wxRegion_Union_1_1;
     _ -> ?CLASS(RegionT,wxBitmap),
       ?wxRegion_Union_1_0
     end,
  wxe_util:queue_cmd(This,Region,?get_env(),RegionOP),
  wxe_util:rec(RegionOP);
union(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Union_1_2),
  wxe_util:rec(?wxRegion_Union_1_2).

%% @equiv union(This,Bmp,Transp, [])
-spec union(This, Bmp, Transp) -> boolean() when
	This::wxRegion(), Bmp::wxBitmap:wxBitmap(), Transp::wx:wx_colour().

union(This,Bmp,Transp)
 when is_record(This, wx_ref),is_record(Bmp, wx_ref),tuple_size(Transp) =:= 3; tuple_size(Transp) =:= 4 ->
  union(This,Bmp,Transp, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionunion">external documentation</a>.
-spec union(This, Bmp, Transp, [Option]) -> boolean() when
	This::wxRegion(), Bmp::wxBitmap:wxBitmap(), Transp::wx:wx_colour(),
	Option :: {'tolerance', integer()}.
union(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp,Transp, Options)
 when tuple_size(Transp) =:= 3; tuple_size(Transp) =:= 4,is_list(Options) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Bmp,wxe_util:color(Transp), Options,?get_env(),?wxRegion_Union_3),
  wxe_util:rec(?wxRegion_Union_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionunion">external documentation</a>.
-spec union(This, X, Y, W, H) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer().
union(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxRegion_Union_4),
  wxe_util:rec(?wxRegion_Union_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionxor">external documentation</a>.
%% <br /> Also:<br />
%% 'Xor'(This, Rect) -> boolean() when<br />
%% 	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec 'Xor'(This, Region) -> boolean() when
	This::wxRegion(), Region::wxRegion();
      (This, Rect) -> boolean() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
'Xor'(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:queue_cmd(This,Region,?get_env(),?wxRegion_Xor_1_0),
  wxe_util:rec(?wxRegion_Xor_1_0);
'Xor'(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Xor_1_1),
  wxe_util:rec(?wxRegion_Xor_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionxor">external documentation</a>.
-spec 'Xor'(This, X, Y, W, H) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), W::integer(), H::integer().
'Xor'(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_integer(X),is_integer(Y),is_integer(W),is_integer(H) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxRegion_Xor_4),
  wxe_util:rec(?wxRegion_Xor_4).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxRegion()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxRegion),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
