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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html">wxGraphicsRenderer</a>.
%% @type wxGraphicsRenderer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGraphicsRenderer).
-include("wxe.hrl").
-export([createBrush/2,createContext/2,createFont/2,createFont/3,createLinearGradientBrush/7,
  createMatrix/1,createMatrix/2,createPath/1,createPen/2,createRadialGradientBrush/8,
  getDefaultRenderer/0]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxGraphicsRenderer/0]).
-deprecated([createLinearGradientBrush/7,createRadialGradientBrush/8]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGraphicsRenderer() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderergetdefaultrenderer">external documentation</a>.
-spec getDefaultRenderer() -> wxGraphicsRenderer().
getDefaultRenderer() ->
  wxe_util:queue_cmd(?get_env(), ?wxGraphicsRenderer_GetDefaultRenderer),
  wxe_util:rec(?wxGraphicsRenderer_GetDefaultRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatecontext">external documentation</a>.
-spec createContext(This, Dc) -> wxGraphicsContext:wxGraphicsContext() when
	This::wxGraphicsRenderer(), Dc::wxWindowDC:wxWindowDC() | wxWindow:wxWindow().
createContext(#wx_ref{type=ThisT}=This,#wx_ref{type=DcT}=Dc) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  DcOP = case ?CLASS_T(DcT,wxWindowDC) of
     true ->
       ?wxGraphicsRenderer_CreateContext_1_1;
     _ -> ?CLASS(DcT,wxWindow),
       ?wxGraphicsRenderer_CreateContext_1_0
     end,
  wxe_util:queue_cmd(This,Dc,?get_env(),DcOP),
  wxe_util:rec(DcOP).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatepen">external documentation</a>.
-spec createPen(This, Pen) -> wxGraphicsPen:wxGraphicsPen() when
	This::wxGraphicsRenderer(), Pen::wxPen:wxPen().
createPen(#wx_ref{type=ThisT}=This,#wx_ref{type=PenT}=Pen) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(PenT,wxPen),
  wxe_util:queue_cmd(This,Pen,?get_env(),?wxGraphicsRenderer_CreatePen),
  wxe_util:rec(?wxGraphicsRenderer_CreatePen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatebrush">external documentation</a>.
-spec createBrush(This, Brush) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), Brush::wxBrush:wxBrush().
createBrush(#wx_ref{type=ThisT}=This,#wx_ref{type=BrushT}=Brush) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(BrushT,wxBrush),
  wxe_util:queue_cmd(This,Brush,?get_env(),?wxGraphicsRenderer_CreateBrush),
  wxe_util:rec(?wxGraphicsRenderer_CreateBrush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatelineargradientbrush">external documentation</a>.
-spec createLinearGradientBrush(This, X1, Y1, X2, Y2, C1, C2) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), X1::number(), Y1::number(), X2::number(), Y2::number(), C1::wx:wx_colour(), C2::wx:wx_colour().
createLinearGradientBrush(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2,C1,C2)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2),tuple_size(C1) =:= 3; tuple_size(C1) =:= 4,tuple_size(C2) =:= 3; tuple_size(C2) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,wxe_util:color(C1),wxe_util:color(C2),?get_env(),?wxGraphicsRenderer_CreateLinearGradientBrush),
  wxe_util:rec(?wxGraphicsRenderer_CreateLinearGradientBrush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreateradialgradientbrush">external documentation</a>.
-spec createRadialGradientBrush(This, Xo, Yo, Xc, Yc, Radius, OColor, CColor) -> wxGraphicsBrush:wxGraphicsBrush() when
	This::wxGraphicsRenderer(), Xo::number(), Yo::number(), Xc::number(), Yc::number(), Radius::number(), OColor::wx:wx_colour(), CColor::wx:wx_colour().
createRadialGradientBrush(#wx_ref{type=ThisT}=This,Xo,Yo,Xc,Yc,Radius,OColor,CColor)
 when is_number(Xo),is_number(Yo),is_number(Xc),is_number(Yc),is_number(Radius),tuple_size(OColor) =:= 3; tuple_size(OColor) =:= 4,tuple_size(CColor) =:= 3; tuple_size(CColor) =:= 4 ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:queue_cmd(This,Xo,Yo,Xc,Yc,Radius,wxe_util:color(OColor),wxe_util:color(CColor),?get_env(),?wxGraphicsRenderer_CreateRadialGradientBrush),
  wxe_util:rec(?wxGraphicsRenderer_CreateRadialGradientBrush).

%% @equiv createFont(This,Font, [])
-spec createFont(This, Font) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsRenderer(), Font::wxFont:wxFont().

createFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  createFont(This,Font, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatefont">external documentation</a>.
-spec createFont(This, Font, [Option]) -> wxGraphicsFont:wxGraphicsFont() when
	This::wxGraphicsRenderer(), Font::wxFont:wxFont(),
	Option :: {'col', wx:wx_colour()}.
createFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font, Options,?get_env(),?wxGraphicsRenderer_CreateFont),
  wxe_util:rec(?wxGraphicsRenderer_CreateFont).

%% @equiv createMatrix(This, [])
-spec createMatrix(This) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsRenderer().

createMatrix(This)
 when is_record(This, wx_ref) ->
  createMatrix(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatematrix">external documentation</a>.
-spec createMatrix(This, [Option]) -> wxGraphicsMatrix:wxGraphicsMatrix() when
	This::wxGraphicsRenderer(),
	Option :: {'a', number()}
		 | {'b', number()}
		 | {'c', number()}
		 | {'d', number()}
		 | {'tx', number()}
		 | {'ty', number()}.
createMatrix(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:queue_cmd(This, Options,?get_env(),?wxGraphicsRenderer_CreateMatrix),
  wxe_util:rec(?wxGraphicsRenderer_CreateMatrix).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsrenderer.html#wxgraphicsrenderercreatepath">external documentation</a>.
-spec createPath(This) -> wxGraphicsPath:wxGraphicsPath() when
	This::wxGraphicsRenderer().
createPath(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsRenderer),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsRenderer_CreatePath),
  wxe_util:rec(?wxGraphicsRenderer_CreatePath).

