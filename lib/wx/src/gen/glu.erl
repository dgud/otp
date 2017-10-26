%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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

%% OPENGL UTILITY API

%% This file is generated DO NOT EDIT

%% @doc  A part of the standard OpenGL Utility api.
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">www.khronos.org</a>
%%
%% Booleans are represented by integers 0 and 1.

-module(glu).
-compile(inline).
-type vertex() :: {float(), float(), float()}.
-type enum() :: non_neg_integer().   %% See wx/include/gl.hrl or glu.hrl
-type matrix12() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix16() :: {float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float(),
                   float(),float(),float(),float()}.
-type matrix() :: matrix12() | matrix16().
-type mem() :: binary() | tuple().   %% Memory block

-export([tesselate/2,build1DMipmapLevels/9,build1DMipmaps/6,build2DMipmapLevels/10,
  build2DMipmaps/7,build3DMipmapLevels/11,build3DMipmaps/8,checkExtension/2,
  cylinder/6,deleteQuadric/1,disk/5,errorString/1,getString/1,lookAt/9,
  newQuadric/0,ortho2D/4,partialDisk/7,perspective/4,pickMatrix/5,project/6,
  quadricDrawStyle/2,quadricNormals/2,quadricOrientation/2,quadricTexture/2,
  scaleImage/9,sphere/4,unProject/6,unProject4/9]).

-import(gl, [get_interface/0, rec/1, lookup_func/1]).

%% API

%% @doc General purpose polygon triangulation.
%% The first argument is the normal and the second a list of
%% vertex positions. Returned is a list of indecies of the vertices
%% and a binary (64bit native float) containing an array of
%% vertex positions, it starts with the vertices in Vs and
%% may contain newly created vertices in the end.
-spec tesselate(Normal, [Vs]) -> {Triangles, VertexPos}
                  when Normal :: vertex(), Vs :: vertex(),
                  Triangles :: [integer()], VertexPos :: binary().
tesselate(Normal, Vs) ->
  IF = get_interface(),
  OP = lookup_func(5009),
  IF:queue_cmd(Normal,Vs,OP,0),
  rec(OP).

%% @doc gluBuild1DMipmapLevels
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec build1DMipmapLevels(Target, InternalFormat, Width, Format, Type, Level, Base, Max, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Format :: enum(),Type :: enum(),Level :: integer(),Base :: integer(),Max :: integer(),Data :: binary().
build1DMipmapLevels(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data) ->
  IF = get_interface(),
  OP = lookup_func(5010),
  IF:queue_cmd(Target,InternalFormat,Width,Format,Type,Level,Base,Max,Data,OP,0),
  rec(OP).

%% @doc gluBuild1DMipmaps
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec build1DMipmaps(Target, InternalFormat, Width, Format, Type, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Format :: enum(),Type :: enum(),Data :: binary().
build1DMipmaps(Target,InternalFormat,Width,Format,Type,Data) ->
  IF = get_interface(),
  OP = lookup_func(5011),
  IF:queue_cmd(Target,InternalFormat,Width,Format,Type,Data,OP,0),
  rec(OP).

%% @doc gluBuild2DMipmapLevels
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec build2DMipmapLevels(Target, InternalFormat, Width, Height, Format, Type, Level, Base, Max, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Level :: integer(),Base :: integer(),Max :: integer(),Data :: binary().
build2DMipmapLevels(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data) ->
  IF = get_interface(),
  OP = lookup_func(5012),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Format,Type,Level,Base,Max,Data,OP,0),
  rec(OP).

%% @doc gluBuild2DMipmaps
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec build2DMipmaps(Target, InternalFormat, Width, Height, Format, Type, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Format :: enum(),Type :: enum(),Data :: binary().
build2DMipmaps(Target,InternalFormat,Width,Height,Format,Type,Data) ->
  IF = get_interface(),
  OP = lookup_func(5013),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Format,Type,Data,OP,0),
  rec(OP).

%% @doc gluBuild3DMipmapLevels
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec build3DMipmapLevels(Target, InternalFormat, Width, Height, Depth, Format, Type, Level, Base, Max, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),Type :: enum(),Level :: integer(),Base :: integer(),Max :: integer(),Data :: binary().
build3DMipmapLevels(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data) ->
  IF = get_interface(),
  OP = lookup_func(5014),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Depth,Format,Type,Level,Base,Max,Data,OP,0),
  rec(OP).

%% @doc gluBuild3DMipmaps
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec build3DMipmaps(Target, InternalFormat, Width, Height, Depth, Format, Type, Data) -> integer() when Target :: enum(),InternalFormat :: integer(),Width :: integer(),Height :: integer(),Depth :: integer(),Format :: enum(),Type :: enum(),Data :: binary().
build3DMipmaps(Target,InternalFormat,Width,Height,Depth,Format,Type,Data) ->
  IF = get_interface(),
  OP = lookup_func(5015),
  IF:queue_cmd(Target,InternalFormat,Width,Height,Depth,Format,Type,Data,OP,0),
  rec(OP).

%% @doc gluCheckExtension
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec checkExtension(ExtName, ExtString) -> 0|1 when ExtName :: string(),ExtString :: string().
checkExtension(ExtName,ExtString) ->
  IF = get_interface(),
  OP = lookup_func(5016),
  ExtNameBin = unicode:characters_to_binary([ExtName|[0]]),
  ExtStringBin = unicode:characters_to_binary([ExtString|[0]]),
  IF:queue_cmd(ExtNameBin,ExtStringBin,OP,0),
  rec(OP).

%% @doc gluCylinder
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec cylinder(Quad, Base, Top, Height, Slices, Stacks) -> 'ok' when Quad :: integer(),Base :: float(),Top :: float(),Height :: float(),Slices :: integer(),Stacks :: integer().
cylinder(Quad,Base,Top,Height,Slices,Stacks) ->
  IF = get_interface(),
  OP = lookup_func(5017),
  IF:queue_cmd(Quad,Base,Top,Height,Slices,Stacks,OP,1),
  ok.

%% @doc gluDeleteQuadric
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec deleteQuadric(Quad) -> 'ok' when Quad :: integer().
deleteQuadric(Quad) ->
  IF = get_interface(),
  OP = lookup_func(5018),
  IF:queue_cmd(Quad,OP,1),
  ok.

%% @doc gluDisk
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec disk(Quad, Inner, Outer, Slices, Loops) -> 'ok' when Quad :: integer(),Inner :: float(),Outer :: float(),Slices :: integer(),Loops :: integer().
disk(Quad,Inner,Outer,Slices,Loops) ->
  IF = get_interface(),
  OP = lookup_func(5019),
  IF:queue_cmd(Quad,Inner,Outer,Slices,Loops,OP,1),
  ok.

%% @doc gluErrorString
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec errorString(Error) -> string() when Error :: enum().
errorString(Error) ->
  IF = get_interface(),
  OP = lookup_func(5020),
  IF:queue_cmd(Error,OP,0),
  rec(OP).

%% @doc gluGetString
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec getString(Name) -> string() when Name :: enum().
getString(Name) ->
  IF = get_interface(),
  OP = lookup_func(5021),
  IF:queue_cmd(Name,OP,0),
  rec(OP).

%% @doc gluLookAt
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec lookAt(EyeX, EyeY, EyeZ, CenterX, CenterY, CenterZ, UpX, UpY, UpZ) -> 'ok' when EyeX :: float(),EyeY :: float(),EyeZ :: float(),CenterX :: float(),CenterY :: float(),CenterZ :: float(),UpX :: float(),UpY :: float(),UpZ :: float().
lookAt(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ) ->
  IF = get_interface(),
  OP = lookup_func(5022),
  IF:queue_cmd(EyeX,EyeY,EyeZ,CenterX,CenterY,CenterZ,UpX,UpY,UpZ,OP,1),
  ok.

%% @doc gluNewQuadric
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec newQuadric() -> integer().
newQuadric() ->
  IF = get_interface(),
  OP = lookup_func(5023),
  IF:queue_cmd(OP,0),
  rec(OP).

%% @doc gluOrtho2D
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec ortho2D(Left, Right, Bottom, Top) -> 'ok' when Left :: float(),Right :: float(),Bottom :: float(),Top :: float().
ortho2D(Left,Right,Bottom,Top) ->
  IF = get_interface(),
  OP = lookup_func(5024),
  IF:queue_cmd(Left,Right,Bottom,Top,OP,1),
  ok.

%% @doc gluPartialDisk
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec partialDisk(Quad, Inner, Outer, Slices, Loops, Start, Sweep) -> 'ok' when Quad :: integer(),Inner :: float(),Outer :: float(),Slices :: integer(),Loops :: integer(),Start :: float(),Sweep :: float().
partialDisk(Quad,Inner,Outer,Slices,Loops,Start,Sweep) ->
  IF = get_interface(),
  OP = lookup_func(5025),
  IF:queue_cmd(Quad,Inner,Outer,Slices,Loops,Start,Sweep,OP,1),
  ok.

%% @doc gluPerspective
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec perspective(Fovy, Aspect, ZNear, ZFar) -> 'ok' when Fovy :: float(),Aspect :: float(),ZNear :: float(),ZFar :: float().
perspective(Fovy,Aspect,ZNear,ZFar) ->
  IF = get_interface(),
  OP = lookup_func(5026),
  IF:queue_cmd(Fovy,Aspect,ZNear,ZFar,OP,1),
  ok.

%% @doc gluPickMatrix
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec pickMatrix(X, Y, DelX, DelY, Viewport) -> 'ok' when X :: float(),Y :: float(),DelX :: float(),DelY :: float(),Viewport :: {integer(),integer(),integer(),integer()}.
pickMatrix(X,Y,DelX,DelY,Viewport) ->
  IF = get_interface(),
  OP = lookup_func(5027),
  IF:queue_cmd(X,Y,DelX,DelY,Viewport,OP,1),
  ok.

%% @doc gluProject
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec project(ObjX, ObjY, ObjZ, Model, Proj, View) -> {integer(),WinX :: float(),WinY :: float(),WinZ :: float()} when ObjX :: float(),ObjY :: float(),ObjZ :: float(),Model :: matrix(),Proj :: matrix(),View :: {integer(),integer(),integer(),integer()}.
project(ObjX,ObjY,ObjZ,Model,Proj,View) ->
  IF = get_interface(),
  OP = lookup_func(5028),
  IF:queue_cmd(ObjX,ObjY,ObjZ,Model,Proj,View,OP,0),
  rec(OP).

%% @doc gluQuadricDrawStyle
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec quadricDrawStyle(Quad, Draw) -> 'ok' when Quad :: integer(),Draw :: enum().
quadricDrawStyle(Quad,Draw) ->
  IF = get_interface(),
  OP = lookup_func(5029),
  IF:queue_cmd(Quad,Draw,OP,1),
  ok.

%% @doc gluQuadricNormals
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec quadricNormals(Quad, Normal) -> 'ok' when Quad :: integer(),Normal :: enum().
quadricNormals(Quad,Normal) ->
  IF = get_interface(),
  OP = lookup_func(5030),
  IF:queue_cmd(Quad,Normal,OP,1),
  ok.

%% @doc gluQuadricOrientation
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec quadricOrientation(Quad, Orientation) -> 'ok' when Quad :: integer(),Orientation :: enum().
quadricOrientation(Quad,Orientation) ->
  IF = get_interface(),
  OP = lookup_func(5031),
  IF:queue_cmd(Quad,Orientation,OP,1),
  ok.

%% @doc gluQuadricTexture
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec quadricTexture(Quad, Texture) -> 'ok' when Quad :: integer(),Texture :: 0|1.
quadricTexture(Quad,Texture) ->
  IF = get_interface(),
  OP = lookup_func(5032),
  IF:queue_cmd(Quad,Texture,OP,1),
  ok.

%% @doc gluScaleImage
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec scaleImage(Format, WIn, HIn, TypeIn, DataIn, WOut, HOut, TypeOut, DataOut) -> integer() when Format :: enum(),WIn :: integer(),HIn :: integer(),TypeIn :: enum(),DataIn :: binary(),WOut :: integer(),HOut :: integer(),TypeOut :: enum(),DataOut :: mem().
scaleImage(Format,WIn,HIn,TypeIn,DataIn,WOut,HOut,TypeOut,DataOut) ->
  IF = get_interface(),
  OP = lookup_func(5033),
  IF:queue_cmd(Format,WIn,HIn,TypeIn,DataIn,WOut,HOut,TypeOut,DataOut,OP,0),
  rec(OP).

%% @doc gluSphere
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec sphere(Quad, Radius, Slices, Stacks) -> 'ok' when Quad :: integer(),Radius :: float(),Slices :: integer(),Stacks :: integer().
sphere(Quad,Radius,Slices,Stacks) ->
  IF = get_interface(),
  OP = lookup_func(5034),
  IF:queue_cmd(Quad,Radius,Slices,Stacks,OP,1),
  ok.

%% @doc gluUnProject
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec unProject(WinX, WinY, WinZ, Model, Proj, View) -> {integer(),ObjX :: float(),ObjY :: float(),ObjZ :: float()} when WinX :: float(),WinY :: float(),WinZ :: float(),Model :: matrix(),Proj :: matrix(),View :: {integer(),integer(),integer(),integer()}.
unProject(WinX,WinY,WinZ,Model,Proj,View) ->
  IF = get_interface(),
  OP = lookup_func(5035),
  IF:queue_cmd(WinX,WinY,WinZ,Model,Proj,View,OP,0),
  rec(OP).

%% @doc gluUnProject
%%
%% See <a href="https://www.khronos.org/registry/OpenGL-Refpages/">external</a> documentation.
-spec unProject4(WinX, WinY, WinZ, ClipW, Model, Proj, View, NearVal, FarVal) -> {integer(),ObjX :: float(),ObjY :: float(),ObjZ :: float(),ObjW :: float()} when WinX :: float(),WinY :: float(),WinZ :: float(),ClipW :: float(),Model :: matrix(),Proj :: matrix(),View :: {integer(),integer(),integer(),integer()},NearVal :: float(),FarVal :: float().
unProject4(WinX,WinY,WinZ,ClipW,Model,Proj,View,NearVal,FarVal) ->
  IF = get_interface(),
  OP = lookup_func(5036),
  IF:queue_cmd(WinX,WinY,WinZ,ClipW,Model,Proj,View,NearVal,FarVal,OP,0),
  rec(OP).

