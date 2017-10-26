/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2017. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd% 
*/
/***** This file is generated do not edit ****/

#include <stdio.h>
#include <string.h>
#include "../egl_impl.h"
#include "gl_fdefs.h"

extern gl_fns_t gl_fns[];

static void ecb_gluBuild1DMipmapLevels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLenum format;
  GLenum type;
  GLint level;
  GLint base;
  GLint max;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5010,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5010,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5010,"width");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5010,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5010,"type");
  if(!enif_get_int(env, argv[5],  &level)) Badarg(5010,"level");
  if(!enif_get_int(env, argv[6],  &base)) Badarg(5010,"base");
  if(!enif_get_int(env, argv[7],  &max)) Badarg(5010,"max");
  if(!enif_inspect_binary(env, argv[8], &data)) Badarg(5010,"data");
  result = wegluBuild1DMipmapLevels(target,internalFormat,width,format,type,level,base,max,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluBuild1DMipmaps(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5011,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5011,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5011,"width");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5011,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5011,"type");
  if(!enif_inspect_binary(env, argv[5], &data)) Badarg(5011,"data");
  result = wegluBuild1DMipmaps(target,internalFormat,width,format,type,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluBuild2DMipmapLevels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  GLint level;
  GLint base;
  GLint max;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5012,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5012,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5012,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5012,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5012,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5012,"type");
  if(!enif_get_int(env, argv[6],  &level)) Badarg(5012,"level");
  if(!enif_get_int(env, argv[7],  &base)) Badarg(5012,"base");
  if(!enif_get_int(env, argv[8],  &max)) Badarg(5012,"max");
  if(!enif_inspect_binary(env, argv[9], &data)) Badarg(5012,"data");
  result = wegluBuild2DMipmapLevels(target,internalFormat,width,height,format,type,level,base,max,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluBuild2DMipmaps(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5013,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5013,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5013,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5013,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5013,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5013,"type");
  if(!enif_inspect_binary(env, argv[6], &data)) Badarg(5013,"data");
  result = wegluBuild2DMipmaps(target,internalFormat,width,height,format,type,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluBuild3DMipmapLevels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLenum type;
  GLint level;
  GLint base;
  GLint max;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5014,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5014,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5014,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5014,"height");
  if(!enif_get_int(env, argv[4],  &depth)) Badarg(5014,"depth");
  if(!enif_get_uint(env, argv[5],  &format)) Badarg(5014,"format");
  if(!enif_get_uint(env, argv[6],  &type)) Badarg(5014,"type");
  if(!enif_get_int(env, argv[7],  &level)) Badarg(5014,"level");
  if(!enif_get_int(env, argv[8],  &base)) Badarg(5014,"base");
  if(!enif_get_int(env, argv[9],  &max)) Badarg(5014,"max");
  if(!enif_inspect_binary(env, argv[10], &data)) Badarg(5014,"data");
  result = wegluBuild3DMipmapLevels(target,internalFormat,width,height,depth,format,type,level,base,max,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluBuild3DMipmaps(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5015,"target");
  if(!enif_get_int(env, argv[1],  &internalFormat)) Badarg(5015,"internalFormat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5015,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5015,"height");
  if(!enif_get_int(env, argv[4],  &depth)) Badarg(5015,"depth");
  if(!enif_get_uint(env, argv[5],  &format)) Badarg(5015,"format");
  if(!enif_get_uint(env, argv[6],  &type)) Badarg(5015,"type");
  if(!enif_inspect_binary(env, argv[7], &data)) Badarg(5015,"data");
  result = wegluBuild3DMipmaps(target,internalFormat,width,height,depth,format,type,(void *) data.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluCheckExtension(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  ErlNifBinary extName;
  ErlNifBinary extString;
  if(!enif_inspect_binary(env, argv[0], &extName)) Badarg(5016,"extName");
  if(!enif_inspect_binary(env, argv[1], &extString)) Badarg(5016,"extString");
  result = wegluCheckExtension((GLubyte *) extName.data,(GLubyte *) extString.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluCylinder(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLdouble base;
  GLdouble top;
  GLdouble height;
  GLint slices;
  GLint stacks;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5017,"quad");
  if(!enif_get_double(env, argv[1],  &base)) Badarg(5017,"base");
  if(!enif_get_double(env, argv[2],  &top)) Badarg(5017,"top");
  if(!enif_get_double(env, argv[3],  &height)) Badarg(5017,"height");
  if(!enif_get_int(env, argv[4],  &slices)) Badarg(5017,"slices");
  if(!enif_get_int(env, argv[5],  &stacks)) Badarg(5017,"stacks");
  wegluCylinder(quad,base,top,height,slices,stacks);
}

static void ecb_gluDeleteQuadric(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5018,"quad");
  wegluDeleteQuadric(quad);
}

static void ecb_gluDisk(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLdouble inner;
  GLdouble outer;
  GLint slices;
  GLint loops;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5019,"quad");
  if(!enif_get_double(env, argv[1],  &inner)) Badarg(5019,"inner");
  if(!enif_get_double(env, argv[2],  &outer)) Badarg(5019,"outer");
  if(!enif_get_int(env, argv[3],  &slices)) Badarg(5019,"slices");
  if(!enif_get_int(env, argv[4],  &loops)) Badarg(5019,"loops");
  wegluDisk(quad,inner,outer,slices,loops);
}

static void ecb_gluErrorString(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  const GLubyte *  result;
  ERL_NIF_TERM reply;
  GLenum error;
  if(!enif_get_uint(env, argv[0],  &error)) Badarg(5020,"error");
  result = wegluErrorString(error);
  reply =      enif_make_string(env, (const char *) result, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluGetString(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  const GLubyte *  result;
  ERL_NIF_TERM reply;
  GLenum name;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5021,"name");
  result = wegluGetString(name);
  reply =      enif_make_string(env, (const char *) result, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluLookAt(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble eyeX;
  GLdouble eyeY;
  GLdouble eyeZ;
  GLdouble centerX;
  GLdouble centerY;
  GLdouble centerZ;
  GLdouble upX;
  GLdouble upY;
  GLdouble upZ;
  if(!enif_get_double(env, argv[0],  &eyeX)) Badarg(5022,"eyeX");
  if(!enif_get_double(env, argv[1],  &eyeY)) Badarg(5022,"eyeY");
  if(!enif_get_double(env, argv[2],  &eyeZ)) Badarg(5022,"eyeZ");
  if(!enif_get_double(env, argv[3],  &centerX)) Badarg(5022,"centerX");
  if(!enif_get_double(env, argv[4],  &centerY)) Badarg(5022,"centerY");
  if(!enif_get_double(env, argv[5],  &centerZ)) Badarg(5022,"centerZ");
  if(!enif_get_double(env, argv[6],  &upX)) Badarg(5022,"upX");
  if(!enif_get_double(env, argv[7],  &upY)) Badarg(5022,"upY");
  if(!enif_get_double(env, argv[8],  &upZ)) Badarg(5022,"upZ");
  wegluLookAt(eyeX,eyeY,eyeZ,centerX,centerY,centerZ,upX,upY,upZ);
}

static void ecb_gluNewQuadric(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *  result;
  ERL_NIF_TERM reply;
  result = wegluNewQuadric();
  reply =      enif_make_uint64(env, (uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluOrtho2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble left;
  GLdouble right;
  GLdouble bottom;
  GLdouble top;
  if(!enif_get_double(env, argv[0],  &left)) Badarg(5024,"left");
  if(!enif_get_double(env, argv[1],  &right)) Badarg(5024,"right");
  if(!enif_get_double(env, argv[2],  &bottom)) Badarg(5024,"bottom");
  if(!enif_get_double(env, argv[3],  &top)) Badarg(5024,"top");
  wegluOrtho2D(left,right,bottom,top);
}

static void ecb_gluPartialDisk(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLdouble inner;
  GLdouble outer;
  GLint slices;
  GLint loops;
  GLdouble start;
  GLdouble sweep;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5025,"quad");
  if(!enif_get_double(env, argv[1],  &inner)) Badarg(5025,"inner");
  if(!enif_get_double(env, argv[2],  &outer)) Badarg(5025,"outer");
  if(!enif_get_int(env, argv[3],  &slices)) Badarg(5025,"slices");
  if(!enif_get_int(env, argv[4],  &loops)) Badarg(5025,"loops");
  if(!enif_get_double(env, argv[5],  &start)) Badarg(5025,"start");
  if(!enif_get_double(env, argv[6],  &sweep)) Badarg(5025,"sweep");
  wegluPartialDisk(quad,inner,outer,slices,loops,start,sweep);
}

static void ecb_gluPerspective(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble fovy;
  GLdouble aspect;
  GLdouble zNear;
  GLdouble zFar;
  if(!enif_get_double(env, argv[0],  &fovy)) Badarg(5026,"fovy");
  if(!enif_get_double(env, argv[1],  &aspect)) Badarg(5026,"aspect");
  if(!enif_get_double(env, argv[2],  &zNear)) Badarg(5026,"zNear");
  if(!enif_get_double(env, argv[3],  &zFar)) Badarg(5026,"zFar");
  wegluPerspective(fovy,aspect,zNear,zFar);
}

static void ecb_gluPickMatrix(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble delX;
  GLdouble delY;
  GLint viewport[4];
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5027,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5027,"y");
  if(!enif_get_double(env, argv[2],  &delX)) Badarg(5027,"delX");
  if(!enif_get_double(env, argv[3],  &delY)) Badarg(5027,"delY");
  {
   int viewport_a;
   const ERL_NIF_TERM *viewport_t;
   if(!enif_get_tuple(env, argv[4], &viewport_a, &viewport_t) || viewport_a != 4) {
     Badarg(5027,"viewport");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, viewport_t[i1++], &viewport[0])) Badarg(5027,"viewport");
     if(!enif_get_int(env, viewport_t[i1++], &viewport[1])) Badarg(5027,"viewport");
     if(!enif_get_int(env, viewport_t[i1++], &viewport[2])) Badarg(5027,"viewport");
     if(!enif_get_int(env, viewport_t[i1++], &viewport[3])) Badarg(5027,"viewport");
   }};
  wegluPickMatrix(x,y,delX,delY,viewport);
}

static void ecb_gluProject(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLdouble objX;
  GLdouble objY;
  GLdouble objZ;
  GLdouble model[16];
  GLdouble proj[16];
  GLint view[4];
  GLdouble winX;
  GLdouble winY;
  GLdouble winZ;
  if(!enif_get_double(env, argv[0],  &objX)) Badarg(5028,"objX");
  if(!enif_get_double(env, argv[1],  &objY)) Badarg(5028,"objY");
  if(!enif_get_double(env, argv[2],  &objZ)) Badarg(5028,"objZ");
  {
   int model_a;
   const ERL_NIF_TERM *model_t;
   if(!enif_get_tuple(env, argv[3], &model_a, &model_t)
       || (model_a != 12 && model_a != 16)) {
     Badarg(5028,"model");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, model_t[i1++], &model[0])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[1])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[2])) Badarg(5028,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[3])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[4])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[5])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[6])) Badarg(5028,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[7])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[8])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[9])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[10])) Badarg(5028,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[11])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[12])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[13])) Badarg(5028,"model");
     if(!enif_get_double(env, model_t[i1++], &model[14])) Badarg(5028,"model");
     if(model_a == 16) {
        if(!enif_get_double(env, model_t[i1++], &model[15])) Badarg(5028,"model");
     } else {
       model[3] = 0.0; model[7] = 0.0; model[11] = 0.0; model[15] = 1.0;
     }
   }};
  {
   int proj_a;
   const ERL_NIF_TERM *proj_t;
   if(!enif_get_tuple(env, argv[4], &proj_a, &proj_t)
       || (proj_a != 12 && proj_a != 16)) {
     Badarg(5028,"proj");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, proj_t[i1++], &proj[0])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[1])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[2])) Badarg(5028,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[3])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[4])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[5])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[6])) Badarg(5028,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[7])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[8])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[9])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[10])) Badarg(5028,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[11])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[12])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[13])) Badarg(5028,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[14])) Badarg(5028,"proj");
     if(proj_a == 16) {
        if(!enif_get_double(env, proj_t[i1++], &proj[15])) Badarg(5028,"proj");
     } else {
       proj[3] = 0.0; proj[7] = 0.0; proj[11] = 0.0; proj[15] = 1.0;
     }
   }};
  {
   int view_a;
   const ERL_NIF_TERM *view_t;
   if(!enif_get_tuple(env, argv[5], &view_a, &view_t) || view_a != 4) {
     Badarg(5028,"view");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, view_t[i1++], &view[0])) Badarg(5028,"view");
     if(!enif_get_int(env, view_t[i1++], &view[1])) Badarg(5028,"view");
     if(!enif_get_int(env, view_t[i1++], &view[2])) Badarg(5028,"view");
     if(!enif_get_int(env, view_t[i1++], &view[3])) Badarg(5028,"view");
   }};
  result = wegluProject(objX,objY,objZ,model,proj,view,&winX,&winY,&winZ);
  reply = enif_make_tuple4(env,
          enif_make_int(env, result),
     enif_make_double(env, winX),
     enif_make_double(env, winY),
     enif_make_double(env, winZ) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluQuadricDrawStyle(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLenum draw;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5029,"quad");
  if(!enif_get_uint(env, argv[1],  &draw)) Badarg(5029,"draw");
  wegluQuadricDrawStyle(quad,draw);
}

static void ecb_gluQuadricNormals(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLenum normal;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5030,"quad");
  if(!enif_get_uint(env, argv[1],  &normal)) Badarg(5030,"normal");
  wegluQuadricNormals(quad,normal);
}

static void ecb_gluQuadricOrientation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLenum orientation;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5031,"quad");
  if(!enif_get_uint(env, argv[1],  &orientation)) Badarg(5031,"orientation");
  wegluQuadricOrientation(quad,orientation);
}

static void ecb_gluQuadricTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLboolean texture;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5032,"quad");
  if(!egl_get_ubyte(env, argv[1],  &texture)) Badarg(5032,"texture");
  wegluQuadricTexture(quad,texture);
}

static void ecb_gluScaleImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum format;
  GLsizei wIn;
  GLsizei hIn;
  GLenum typeIn;
  ErlNifBinary dataIn;
  GLsizei wOut;
  GLsizei hOut;
  GLenum typeOut;
  ErlNifBinary dataOut;
  if(!enif_get_uint(env, argv[0],  &format)) Badarg(5033,"format");
  if(!enif_get_int(env, argv[1],  &wIn)) Badarg(5033,"wIn");
  if(!enif_get_int(env, argv[2],  &hIn)) Badarg(5033,"hIn");
  if(!enif_get_uint(env, argv[3],  &typeIn)) Badarg(5033,"typeIn");
  if(!enif_inspect_binary(env, argv[4], &dataIn)) Badarg(5033,"dataIn");
  if(!enif_get_int(env, argv[5],  &wOut)) Badarg(5033,"wOut");
  if(!enif_get_int(env, argv[6],  &hOut)) Badarg(5033,"hOut");
  if(!enif_get_uint(env, argv[7],  &typeOut)) Badarg(5033,"typeOut");
  if(enif_is_binary(env, argv[8]))
    enif_inspect_binary(env, argv[8], &dataOut);
  else if(enif_is_tuple(env, argv[8])) {
    int dataOut_a;
    const ERL_NIF_TERM *dataOut_t;
    if(enif_get_tuple(env, argv[8], &dataOut_a, &dataOut_t) ||
         enif_is_binary(env, dataOut_t[1]))
       enif_inspect_binary(env, dataOut_t[1], &dataOut);
    else Badarg(5033, "dataOut");
  } else Badarg(5033, "dataOut");
  result = wegluScaleImage(format,wIn,hIn,typeIn,(void *) dataIn.data,wOut,hOut,typeOut,(GLvoid *) dataOut.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluSphere(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLUquadric *quad;
  GLdouble radius;
  GLint slices;
  GLint stacks;
  if(!egl_get_ptr(env, argv[0], (void *) &quad)) Badarg(5034,"quad");
  if(!enif_get_double(env, argv[1],  &radius)) Badarg(5034,"radius");
  if(!enif_get_int(env, argv[2],  &slices)) Badarg(5034,"slices");
  if(!enif_get_int(env, argv[3],  &stacks)) Badarg(5034,"stacks");
  wegluSphere(quad,radius,slices,stacks);
}

static void ecb_gluUnProject(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLdouble winX;
  GLdouble winY;
  GLdouble winZ;
  GLdouble model[16];
  GLdouble proj[16];
  GLint view[4];
  GLdouble objX;
  GLdouble objY;
  GLdouble objZ;
  if(!enif_get_double(env, argv[0],  &winX)) Badarg(5035,"winX");
  if(!enif_get_double(env, argv[1],  &winY)) Badarg(5035,"winY");
  if(!enif_get_double(env, argv[2],  &winZ)) Badarg(5035,"winZ");
  {
   int model_a;
   const ERL_NIF_TERM *model_t;
   if(!enif_get_tuple(env, argv[3], &model_a, &model_t)
       || (model_a != 12 && model_a != 16)) {
     Badarg(5035,"model");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, model_t[i1++], &model[0])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[1])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[2])) Badarg(5035,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[3])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[4])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[5])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[6])) Badarg(5035,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[7])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[8])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[9])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[10])) Badarg(5035,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[11])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[12])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[13])) Badarg(5035,"model");
     if(!enif_get_double(env, model_t[i1++], &model[14])) Badarg(5035,"model");
     if(model_a == 16) {
        if(!enif_get_double(env, model_t[i1++], &model[15])) Badarg(5035,"model");
     } else {
       model[3] = 0.0; model[7] = 0.0; model[11] = 0.0; model[15] = 1.0;
     }
   }};
  {
   int proj_a;
   const ERL_NIF_TERM *proj_t;
   if(!enif_get_tuple(env, argv[4], &proj_a, &proj_t)
       || (proj_a != 12 && proj_a != 16)) {
     Badarg(5035,"proj");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, proj_t[i1++], &proj[0])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[1])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[2])) Badarg(5035,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[3])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[4])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[5])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[6])) Badarg(5035,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[7])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[8])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[9])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[10])) Badarg(5035,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[11])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[12])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[13])) Badarg(5035,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[14])) Badarg(5035,"proj");
     if(proj_a == 16) {
        if(!enif_get_double(env, proj_t[i1++], &proj[15])) Badarg(5035,"proj");
     } else {
       proj[3] = 0.0; proj[7] = 0.0; proj[11] = 0.0; proj[15] = 1.0;
     }
   }};
  {
   int view_a;
   const ERL_NIF_TERM *view_t;
   if(!enif_get_tuple(env, argv[5], &view_a, &view_t) || view_a != 4) {
     Badarg(5035,"view");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, view_t[i1++], &view[0])) Badarg(5035,"view");
     if(!enif_get_int(env, view_t[i1++], &view[1])) Badarg(5035,"view");
     if(!enif_get_int(env, view_t[i1++], &view[2])) Badarg(5035,"view");
     if(!enif_get_int(env, view_t[i1++], &view[3])) Badarg(5035,"view");
   }};
  result = wegluUnProject(winX,winY,winZ,model,proj,view,&objX,&objY,&objZ);
  reply = enif_make_tuple4(env,
          enif_make_int(env, result),
     enif_make_double(env, objX),
     enif_make_double(env, objY),
     enif_make_double(env, objZ) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_gluUnProject4(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLdouble winX;
  GLdouble winY;
  GLdouble winZ;
  GLdouble clipW;
  GLdouble model[16];
  GLdouble proj[16];
  GLint view[4];
  GLdouble nearVal;
  GLdouble farVal;
  GLdouble objX;
  GLdouble objY;
  GLdouble objZ;
  GLdouble objW;
  if(!enif_get_double(env, argv[0],  &winX)) Badarg(5036,"winX");
  if(!enif_get_double(env, argv[1],  &winY)) Badarg(5036,"winY");
  if(!enif_get_double(env, argv[2],  &winZ)) Badarg(5036,"winZ");
  if(!enif_get_double(env, argv[3],  &clipW)) Badarg(5036,"clipW");
  {
   int model_a;
   const ERL_NIF_TERM *model_t;
   if(!enif_get_tuple(env, argv[4], &model_a, &model_t)
       || (model_a != 12 && model_a != 16)) {
     Badarg(5036,"model");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, model_t[i1++], &model[0])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[1])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[2])) Badarg(5036,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[3])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[4])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[5])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[6])) Badarg(5036,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[7])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[8])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[9])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[10])) Badarg(5036,"model");
     if(model_a == 16)
        if(!enif_get_double(env, model_t[i1++], &model[11])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[12])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[13])) Badarg(5036,"model");
     if(!enif_get_double(env, model_t[i1++], &model[14])) Badarg(5036,"model");
     if(model_a == 16) {
        if(!enif_get_double(env, model_t[i1++], &model[15])) Badarg(5036,"model");
     } else {
       model[3] = 0.0; model[7] = 0.0; model[11] = 0.0; model[15] = 1.0;
     }
   }};
  {
   int proj_a;
   const ERL_NIF_TERM *proj_t;
   if(!enif_get_tuple(env, argv[5], &proj_a, &proj_t)
       || (proj_a != 12 && proj_a != 16)) {
     Badarg(5036,"proj");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, proj_t[i1++], &proj[0])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[1])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[2])) Badarg(5036,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[3])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[4])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[5])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[6])) Badarg(5036,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[7])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[8])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[9])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[10])) Badarg(5036,"proj");
     if(proj_a == 16)
        if(!enif_get_double(env, proj_t[i1++], &proj[11])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[12])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[13])) Badarg(5036,"proj");
     if(!enif_get_double(env, proj_t[i1++], &proj[14])) Badarg(5036,"proj");
     if(proj_a == 16) {
        if(!enif_get_double(env, proj_t[i1++], &proj[15])) Badarg(5036,"proj");
     } else {
       proj[3] = 0.0; proj[7] = 0.0; proj[11] = 0.0; proj[15] = 1.0;
     }
   }};
  {
   int view_a;
   const ERL_NIF_TERM *view_t;
   if(!enif_get_tuple(env, argv[6], &view_a, &view_t) || view_a != 4) {
     Badarg(5036,"view");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, view_t[i1++], &view[0])) Badarg(5036,"view");
     if(!enif_get_int(env, view_t[i1++], &view[1])) Badarg(5036,"view");
     if(!enif_get_int(env, view_t[i1++], &view[2])) Badarg(5036,"view");
     if(!enif_get_int(env, view_t[i1++], &view[3])) Badarg(5036,"view");
   }};
  if(!enif_get_double(env, argv[7],  &nearVal)) Badarg(5036,"nearVal");
  if(!enif_get_double(env, argv[8],  &farVal)) Badarg(5036,"farVal");
  result = wegluUnProject4(winX,winY,winZ,clipW,model,proj,view,nearVal,farVal,&objX,&objY,&objZ,&objW);
  reply = enif_make_tuple5(env,
          enif_make_int(env, result),
     enif_make_double(env, objX),
     enif_make_double(env, objY),
     enif_make_double(env, objZ),
     enif_make_double(env, objW) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glClearIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat c;
  if(!egl_get_float(env, argv[0],  &c)) Badarg(5037,"c");
  weglClearIndex(c);
}

static void ecb_glClearColor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf red;
  GLclampf green;
  GLclampf blue;
  GLclampf alpha;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5038,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5038,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5038,"blue");
  if(!egl_get_float(env, argv[3],  &alpha)) Badarg(5038,"alpha");
  weglClearColor(red,green,blue,alpha);
}

static void ecb_glClear(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5039,"mask");
  weglClear(mask);
}

static void ecb_glIndexMask(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5040,"mask");
  weglIndexMask(mask);
}

static void ecb_glColorMask(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean red;
  GLboolean green;
  GLboolean blue;
  GLboolean alpha;
  if(!egl_get_ubyte(env, argv[0],  &red)) Badarg(5041,"red");
  if(!egl_get_ubyte(env, argv[1],  &green)) Badarg(5041,"green");
  if(!egl_get_ubyte(env, argv[2],  &blue)) Badarg(5041,"blue");
  if(!egl_get_ubyte(env, argv[3],  &alpha)) Badarg(5041,"alpha");
  weglColorMask(red,green,blue,alpha);
}

static void ecb_glAlphaFunc(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum func;
  GLclampf ref;
  if(!enif_get_uint(env, argv[0],  &func)) Badarg(5042,"func");
  if(!egl_get_float(env, argv[1],  &ref)) Badarg(5042,"ref");
  weglAlphaFunc(func,ref);
}

static void ecb_glBlendFunc(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum sfactor;
  GLenum dfactor;
  if(!enif_get_uint(env, argv[0],  &sfactor)) Badarg(5043,"sfactor");
  if(!enif_get_uint(env, argv[1],  &dfactor)) Badarg(5043,"dfactor");
  weglBlendFunc(sfactor,dfactor);
}

static void ecb_glLogicOp(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum opcode;
  if(!enif_get_uint(env, argv[0],  &opcode)) Badarg(5044,"opcode");
  weglLogicOp(opcode);
}

static void ecb_glCullFace(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5045,"mode");
  weglCullFace(mode);
}

static void ecb_glFrontFace(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5046,"mode");
  weglFrontFace(mode);
}

static void ecb_glPointSize(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat size;
  if(!egl_get_float(env, argv[0],  &size)) Badarg(5047,"size");
  weglPointSize(size);
}

static void ecb_glLineWidth(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat width;
  if(!egl_get_float(env, argv[0],  &width)) Badarg(5048,"width");
  weglLineWidth(width);
}

static void ecb_glLineStipple(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint factor;
  GLushort pattern;
  if(!enif_get_int(env, argv[0],  &factor)) Badarg(5049,"factor");
  if(!egl_get_ushort(env, argv[1],  &pattern)) Badarg(5049,"pattern");
  weglLineStipple(factor,pattern);
}

static void ecb_glPolygonMode(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5050,"face");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5050,"mode");
  weglPolygonMode(face,mode);
}

static void ecb_glPolygonOffset(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat factor;
  GLfloat units;
  if(!egl_get_float(env, argv[0],  &factor)) Badarg(5051,"factor");
  if(!egl_get_float(env, argv[1],  &units)) Badarg(5051,"units");
  weglPolygonOffset(factor,units);
}

static void ecb_glPolygonStipple(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ErlNifBinary mask;
  if(!enif_inspect_binary(env, argv[0], &mask)) Badarg(5052,"mask");
  weglPolygonStipple((GLubyte *) mask.data);
}

static void ecb_glGetPolygonStipple(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  ErlNifBinary mask;
  enif_alloc_binary((int) 128*sizeof(GLubyte), &mask);
  weglGetPolygonStipple((GLubyte *) mask.data);
  reply =      enif_make_binary(env, &mask);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glEdgeFlag(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean flag;
  if(!egl_get_ubyte(env, argv[0],  &flag)) Badarg(5054,"flag");
  weglEdgeFlag(flag);
}

static void ecb_glScissor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5055,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5055,"y");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5055,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5055,"height");
  weglScissor(x,y,width,height);
}

static void ecb_glClipPlane(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum plane;
  GLdouble equation[4];
  if(!enif_get_uint(env, argv[0],  &plane)) Badarg(5056,"plane");
  {
   int equation_a;
   const ERL_NIF_TERM *equation_t;
   if(!enif_get_tuple(env, argv[1], &equation_a, &equation_t) || equation_a != 4) {
     Badarg(5056,"equation");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, equation_t[i1++], &equation[0])) Badarg(5056,"equation");
     if(!enif_get_double(env, equation_t[i1++], &equation[1])) Badarg(5056,"equation");
     if(!enif_get_double(env, equation_t[i1++], &equation[2])) Badarg(5056,"equation");
     if(!enif_get_double(env, equation_t[i1++], &equation[3])) Badarg(5056,"equation");
   }};
  weglClipPlane(plane,equation);
}

static void ecb_glGetClipPlane(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum plane;
  GLdouble equation[4];
  if(!enif_get_uint(env, argv[0],  &plane)) Badarg(5057,"plane");
  weglGetClipPlane(plane,equation);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, equation[0]),
            enif_make_double(env, equation[1]),
            enif_make_double(env, equation[2]),
            enif_make_double(env, equation[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDrawBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5058,"mode");
  weglDrawBuffer(mode);
}

static void ecb_glReadBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5059,"mode");
  weglReadBuffer(mode);
}

static void ecb_glEnable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5060,"cap");
  weglEnable(cap);
}

static void ecb_glDisable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5061,"cap");
  weglDisable(cap);
}

static void ecb_glIsEnabled(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5062,"cap");
  result = weglIsEnabled(cap);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glEnableClientState(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5063,"cap");
  weglEnableClientState(cap);
}

static void ecb_glDisableClientState(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum cap;
  if(!enif_get_uint(env, argv[0],  &cap)) Badarg(5064,"cap");
  weglDisableClientState(cap);
}

static void ecb_glGetBooleanv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLboolean params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5065,"pname");
  weglGetBooleanv(pname,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetDoublev(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLdouble params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5066,"pname");
  weglGetDoublev(pname,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_double(env, params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetFloatv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLfloat params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5067,"pname");
  weglGetFloatv(pname,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_double(env, (double) params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetIntegerv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5068,"pname");
  weglGetIntegerv(pname,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glPushAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5069,"mask");
  weglPushAttrib(mask);
}

static void ecb_glPopAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopAttrib();
}

static void ecb_glPushClientAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5071,"mask");
  weglPushClientAttrib(mask);
}

static void ecb_glPopClientAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopClientAttrib();
}

static void ecb_glRenderMode(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5073,"mode");
  result = weglRenderMode(mode);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetError(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum result;
  ERL_NIF_TERM reply;
  result = weglGetError();
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetString(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  const GLubyte *  result;
  ERL_NIF_TERM reply;
  GLenum name;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5075,"name");
  result = weglGetString(name);
  reply =      enif_make_string(env, (const char *) result, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glFinish(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglFinish();
}

static void ecb_glFlush(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglFlush();
}

static void ecb_glHint(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5078,"target");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5078,"mode");
  weglHint(target,mode);
}

static void ecb_glClearDepth(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampd depth;
  if(!enif_get_double(env, argv[0],  &depth)) Badarg(5079,"depth");
  weglClearDepth(depth);
}

static void ecb_glDepthFunc(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum func;
  if(!enif_get_uint(env, argv[0],  &func)) Badarg(5080,"func");
  weglDepthFunc(func);
}

static void ecb_glDepthMask(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean flag;
  if(!egl_get_ubyte(env, argv[0],  &flag)) Badarg(5081,"flag");
  weglDepthMask(flag);
}

static void ecb_glDepthRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampd near_val;
  GLclampd far_val;
  if(!enif_get_double(env, argv[0],  &near_val)) Badarg(5082,"near_val");
  if(!enif_get_double(env, argv[1],  &far_val)) Badarg(5082,"far_val");
  weglDepthRange(near_val,far_val);
}

static void ecb_glClearAccum(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  GLfloat alpha;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5083,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5083,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5083,"blue");
  if(!egl_get_float(env, argv[3],  &alpha)) Badarg(5083,"alpha");
  weglClearAccum(red,green,blue,alpha);
}

static void ecb_glAccum(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum op;
  GLfloat value;
  if(!enif_get_uint(env, argv[0],  &op)) Badarg(5084,"op");
  if(!egl_get_float(env, argv[1],  &value)) Badarg(5084,"value");
  weglAccum(op,value);
}

static void ecb_glMatrixMode(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5085,"mode");
  weglMatrixMode(mode);
}

static void ecb_glOrtho(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble left;
  GLdouble right;
  GLdouble bottom;
  GLdouble top;
  GLdouble near_val;
  GLdouble far_val;
  if(!enif_get_double(env, argv[0],  &left)) Badarg(5086,"left");
  if(!enif_get_double(env, argv[1],  &right)) Badarg(5086,"right");
  if(!enif_get_double(env, argv[2],  &bottom)) Badarg(5086,"bottom");
  if(!enif_get_double(env, argv[3],  &top)) Badarg(5086,"top");
  if(!enif_get_double(env, argv[4],  &near_val)) Badarg(5086,"near_val");
  if(!enif_get_double(env, argv[5],  &far_val)) Badarg(5086,"far_val");
  weglOrtho(left,right,bottom,top,near_val,far_val);
}

static void ecb_glFrustum(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble left;
  GLdouble right;
  GLdouble bottom;
  GLdouble top;
  GLdouble near_val;
  GLdouble far_val;
  if(!enif_get_double(env, argv[0],  &left)) Badarg(5087,"left");
  if(!enif_get_double(env, argv[1],  &right)) Badarg(5087,"right");
  if(!enif_get_double(env, argv[2],  &bottom)) Badarg(5087,"bottom");
  if(!enif_get_double(env, argv[3],  &top)) Badarg(5087,"top");
  if(!enif_get_double(env, argv[4],  &near_val)) Badarg(5087,"near_val");
  if(!enif_get_double(env, argv[5],  &far_val)) Badarg(5087,"far_val");
  weglFrustum(left,right,bottom,top,near_val,far_val);
}

static void ecb_glViewport(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5088,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5088,"y");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5088,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5088,"height");
  weglViewport(x,y,width,height);
}

static void ecb_glPushMatrix(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPushMatrix();
}

static void ecb_glPopMatrix(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopMatrix();
}

static void ecb_glLoadIdentity(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglLoadIdentity();
}

static void ecb_glLoadMatrixd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5092,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5092,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5092,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5092,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5092,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5092,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5092,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadMatrixd(m);
}

static void ecb_glLoadMatrixf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5093,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5093,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5093,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5093,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5093,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5093,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5093,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadMatrixf(m);
}

static void ecb_glMultMatrixd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5094,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5094,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5094,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5094,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5094,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5094,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5094,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultMatrixd(m);
}

static void ecb_glMultMatrixf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5095,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5095,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5095,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5095,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5095,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5095,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5095,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultMatrixf(m);
}

static void ecb_glRotated(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble angle;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &angle)) Badarg(5096,"angle");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5096,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5096,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5096,"z");
  weglRotated(angle,x,y,z);
}

static void ecb_glRotatef(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat angle;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &angle)) Badarg(5097,"angle");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5097,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5097,"y");
  if(!egl_get_float(env, argv[3],  &z)) Badarg(5097,"z");
  weglRotatef(angle,x,y,z);
}

static void ecb_glScaled(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5098,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5098,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5098,"z");
  weglScaled(x,y,z);
}

static void ecb_glScalef(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5099,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5099,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5099,"z");
  weglScalef(x,y,z);
}

static void ecb_glTranslated(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5100,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5100,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5100,"z");
  weglTranslated(x,y,z);
}

static void ecb_glTranslatef(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5101,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5101,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5101,"z");
  weglTranslatef(x,y,z);
}

static void ecb_glIsList(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint list;
  if(!enif_get_uint(env, argv[0],  &list)) Badarg(5102,"list");
  result = weglIsList(list);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDeleteLists(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint list;
  GLsizei range;
  if(!enif_get_uint(env, argv[0],  &list)) Badarg(5103,"list");
  if(!enif_get_int(env, argv[1],  &range)) Badarg(5103,"range");
  weglDeleteLists(list,range);
}

static void ecb_glGenLists(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLsizei range;
  if(!enif_get_int(env, argv[0],  &range)) Badarg(5104,"range");
  result = weglGenLists(range);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glNewList(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint list;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &list)) Badarg(5105,"list");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5105,"mode");
  weglNewList(list,mode);
}

static void ecb_glEndList(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEndList();
}

static void ecb_glCallList(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint list;
  if(!enif_get_uint(env, argv[0],  &list)) Badarg(5107,"list");
  weglCallList(list);
}

static void ecb_glCallLists(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int lists_free = 0;
  GLuint *lists;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5108,"n");
  if(!enif_is_list(env, argv[2])) { Badarg(5108, "lists"); }
  else {
    int i=0;
    ERL_NIF_TERM lists_l, lists_h, lists_t;
    lists_free = 1;
    lists = (GLuint *) enif_alloc(n * sizeof(GLuint));
    lists_l = argv[2];
    while(enif_get_list_cell(env, lists_l, &lists_h, &lists_t)) {
        if(!enif_get_uint(env, lists_h, &lists[i++])) Badarg(5108,"lists");
        lists_l = lists_t;
    };
  }
  weglCallLists(n,GL_UNSIGNED_INT,lists);
 if(lists_free) enif_free(lists);
}

static void ecb_glListBase(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint base;
  if(!enif_get_uint(env, argv[0],  &base)) Badarg(5109,"base");
  weglListBase(base);
}

static void ecb_glBegin(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5110,"mode");
  weglBegin(mode);
}

static void ecb_glEnd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEnd();
}

static void ecb_glVertex2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5112,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5112,"y");
  weglVertex2d(x,y);
}

static void ecb_glVertex2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5113,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5113,"y");
  weglVertex2f(x,y);
}

static void ecb_glVertex2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5114,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5114,"y");
  weglVertex2i(x,y);
}

static void ecb_glVertex2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5115,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5115,"y");
  weglVertex2s(x,y);
}

static void ecb_glVertex3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5116,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5116,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5116,"z");
  weglVertex3d(x,y,z);
}

static void ecb_glVertex3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5117,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5117,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5117,"z");
  weglVertex3f(x,y,z);
}

static void ecb_glVertex3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5118,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5118,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5118,"z");
  weglVertex3i(x,y,z);
}

static void ecb_glVertex3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5119,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5119,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5119,"z");
  weglVertex3s(x,y,z);
}

static void ecb_glVertex4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5120,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5120,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5120,"z");
  if(!enif_get_double(env, argv[3],  &w)) Badarg(5120,"w");
  weglVertex4d(x,y,z,w);
}

static void ecb_glVertex4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5121,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5121,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5121,"z");
  if(!egl_get_float(env, argv[3],  &w)) Badarg(5121,"w");
  weglVertex4f(x,y,z,w);
}

static void ecb_glVertex4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  GLint w;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5122,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5122,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5122,"z");
  if(!enif_get_int(env, argv[3],  &w)) Badarg(5122,"w");
  weglVertex4i(x,y,z,w);
}

static void ecb_glVertex4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  GLshort w;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5123,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5123,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5123,"z");
  if(!egl_get_short(env, argv[3],  &w)) Badarg(5123,"w");
  weglVertex4s(x,y,z,w);
}

static void ecb_glNormal3b(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbyte nx;
  GLbyte ny;
  GLbyte nz;
  if(!egl_get_byte(env, argv[0],  &nx)) Badarg(5124,"nx");
  if(!egl_get_byte(env, argv[1],  &ny)) Badarg(5124,"ny");
  if(!egl_get_byte(env, argv[2],  &nz)) Badarg(5124,"nz");
  weglNormal3b(nx,ny,nz);
}

static void ecb_glNormal3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble nx;
  GLdouble ny;
  GLdouble nz;
  if(!enif_get_double(env, argv[0],  &nx)) Badarg(5125,"nx");
  if(!enif_get_double(env, argv[1],  &ny)) Badarg(5125,"ny");
  if(!enif_get_double(env, argv[2],  &nz)) Badarg(5125,"nz");
  weglNormal3d(nx,ny,nz);
}

static void ecb_glNormal3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat nx;
  GLfloat ny;
  GLfloat nz;
  if(!egl_get_float(env, argv[0],  &nx)) Badarg(5126,"nx");
  if(!egl_get_float(env, argv[1],  &ny)) Badarg(5126,"ny");
  if(!egl_get_float(env, argv[2],  &nz)) Badarg(5126,"nz");
  weglNormal3f(nx,ny,nz);
}

static void ecb_glNormal3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint nx;
  GLint ny;
  GLint nz;
  if(!enif_get_int(env, argv[0],  &nx)) Badarg(5127,"nx");
  if(!enif_get_int(env, argv[1],  &ny)) Badarg(5127,"ny");
  if(!enif_get_int(env, argv[2],  &nz)) Badarg(5127,"nz");
  weglNormal3i(nx,ny,nz);
}

static void ecb_glNormal3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort nx;
  GLshort ny;
  GLshort nz;
  if(!egl_get_short(env, argv[0],  &nx)) Badarg(5128,"nx");
  if(!egl_get_short(env, argv[1],  &ny)) Badarg(5128,"ny");
  if(!egl_get_short(env, argv[2],  &nz)) Badarg(5128,"nz");
  weglNormal3s(nx,ny,nz);
}

static void ecb_glIndexd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble c;
  if(!enif_get_double(env, argv[0],  &c)) Badarg(5129,"c");
  weglIndexd(c);
}

static void ecb_glIndexf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat c;
  if(!egl_get_float(env, argv[0],  &c)) Badarg(5130,"c");
  weglIndexf(c);
}

static void ecb_glIndexi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint c;
  if(!enif_get_int(env, argv[0],  &c)) Badarg(5131,"c");
  weglIndexi(c);
}

static void ecb_glIndexs(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort c;
  if(!egl_get_short(env, argv[0],  &c)) Badarg(5132,"c");
  weglIndexs(c);
}

static void ecb_glIndexub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLubyte c;
  if(!egl_get_ubyte(env, argv[0],  &c)) Badarg(5133,"c");
  weglIndexub(c);
}

static void ecb_glColor3b(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbyte red;
  GLbyte green;
  GLbyte blue;
  if(!egl_get_byte(env, argv[0],  &red)) Badarg(5134,"red");
  if(!egl_get_byte(env, argv[1],  &green)) Badarg(5134,"green");
  if(!egl_get_byte(env, argv[2],  &blue)) Badarg(5134,"blue");
  weglColor3b(red,green,blue);
}

static void ecb_glColor3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble red;
  GLdouble green;
  GLdouble blue;
  if(!enif_get_double(env, argv[0],  &red)) Badarg(5135,"red");
  if(!enif_get_double(env, argv[1],  &green)) Badarg(5135,"green");
  if(!enif_get_double(env, argv[2],  &blue)) Badarg(5135,"blue");
  weglColor3d(red,green,blue);
}

static void ecb_glColor3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5136,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5136,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5136,"blue");
  weglColor3f(red,green,blue);
}

static void ecb_glColor3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint red;
  GLint green;
  GLint blue;
  if(!enif_get_int(env, argv[0],  &red)) Badarg(5137,"red");
  if(!enif_get_int(env, argv[1],  &green)) Badarg(5137,"green");
  if(!enif_get_int(env, argv[2],  &blue)) Badarg(5137,"blue");
  weglColor3i(red,green,blue);
}

static void ecb_glColor3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort red;
  GLshort green;
  GLshort blue;
  if(!egl_get_short(env, argv[0],  &red)) Badarg(5138,"red");
  if(!egl_get_short(env, argv[1],  &green)) Badarg(5138,"green");
  if(!egl_get_short(env, argv[2],  &blue)) Badarg(5138,"blue");
  weglColor3s(red,green,blue);
}

static void ecb_glColor3ub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLubyte red;
  GLubyte green;
  GLubyte blue;
  if(!egl_get_ubyte(env, argv[0],  &red)) Badarg(5139,"red");
  if(!egl_get_ubyte(env, argv[1],  &green)) Badarg(5139,"green");
  if(!egl_get_ubyte(env, argv[2],  &blue)) Badarg(5139,"blue");
  weglColor3ub(red,green,blue);
}

static void ecb_glColor3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint red;
  GLuint green;
  GLuint blue;
  if(!enif_get_uint(env, argv[0],  &red)) Badarg(5140,"red");
  if(!enif_get_uint(env, argv[1],  &green)) Badarg(5140,"green");
  if(!enif_get_uint(env, argv[2],  &blue)) Badarg(5140,"blue");
  weglColor3ui(red,green,blue);
}

static void ecb_glColor3us(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLushort red;
  GLushort green;
  GLushort blue;
  if(!egl_get_ushort(env, argv[0],  &red)) Badarg(5141,"red");
  if(!egl_get_ushort(env, argv[1],  &green)) Badarg(5141,"green");
  if(!egl_get_ushort(env, argv[2],  &blue)) Badarg(5141,"blue");
  weglColor3us(red,green,blue);
}

static void ecb_glColor4b(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbyte red;
  GLbyte green;
  GLbyte blue;
  GLbyte alpha;
  if(!egl_get_byte(env, argv[0],  &red)) Badarg(5142,"red");
  if(!egl_get_byte(env, argv[1],  &green)) Badarg(5142,"green");
  if(!egl_get_byte(env, argv[2],  &blue)) Badarg(5142,"blue");
  if(!egl_get_byte(env, argv[3],  &alpha)) Badarg(5142,"alpha");
  weglColor4b(red,green,blue,alpha);
}

static void ecb_glColor4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble red;
  GLdouble green;
  GLdouble blue;
  GLdouble alpha;
  if(!enif_get_double(env, argv[0],  &red)) Badarg(5143,"red");
  if(!enif_get_double(env, argv[1],  &green)) Badarg(5143,"green");
  if(!enif_get_double(env, argv[2],  &blue)) Badarg(5143,"blue");
  if(!enif_get_double(env, argv[3],  &alpha)) Badarg(5143,"alpha");
  weglColor4d(red,green,blue,alpha);
}

static void ecb_glColor4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  GLfloat alpha;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5144,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5144,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5144,"blue");
  if(!egl_get_float(env, argv[3],  &alpha)) Badarg(5144,"alpha");
  weglColor4f(red,green,blue,alpha);
}

static void ecb_glColor4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint red;
  GLint green;
  GLint blue;
  GLint alpha;
  if(!enif_get_int(env, argv[0],  &red)) Badarg(5145,"red");
  if(!enif_get_int(env, argv[1],  &green)) Badarg(5145,"green");
  if(!enif_get_int(env, argv[2],  &blue)) Badarg(5145,"blue");
  if(!enif_get_int(env, argv[3],  &alpha)) Badarg(5145,"alpha");
  weglColor4i(red,green,blue,alpha);
}

static void ecb_glColor4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort red;
  GLshort green;
  GLshort blue;
  GLshort alpha;
  if(!egl_get_short(env, argv[0],  &red)) Badarg(5146,"red");
  if(!egl_get_short(env, argv[1],  &green)) Badarg(5146,"green");
  if(!egl_get_short(env, argv[2],  &blue)) Badarg(5146,"blue");
  if(!egl_get_short(env, argv[3],  &alpha)) Badarg(5146,"alpha");
  weglColor4s(red,green,blue,alpha);
}

static void ecb_glColor4ub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLubyte red;
  GLubyte green;
  GLubyte blue;
  GLubyte alpha;
  if(!egl_get_ubyte(env, argv[0],  &red)) Badarg(5147,"red");
  if(!egl_get_ubyte(env, argv[1],  &green)) Badarg(5147,"green");
  if(!egl_get_ubyte(env, argv[2],  &blue)) Badarg(5147,"blue");
  if(!egl_get_ubyte(env, argv[3],  &alpha)) Badarg(5147,"alpha");
  weglColor4ub(red,green,blue,alpha);
}

static void ecb_glColor4ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint red;
  GLuint green;
  GLuint blue;
  GLuint alpha;
  if(!enif_get_uint(env, argv[0],  &red)) Badarg(5148,"red");
  if(!enif_get_uint(env, argv[1],  &green)) Badarg(5148,"green");
  if(!enif_get_uint(env, argv[2],  &blue)) Badarg(5148,"blue");
  if(!enif_get_uint(env, argv[3],  &alpha)) Badarg(5148,"alpha");
  weglColor4ui(red,green,blue,alpha);
}

static void ecb_glColor4us(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLushort red;
  GLushort green;
  GLushort blue;
  GLushort alpha;
  if(!egl_get_ushort(env, argv[0],  &red)) Badarg(5149,"red");
  if(!egl_get_ushort(env, argv[1],  &green)) Badarg(5149,"green");
  if(!egl_get_ushort(env, argv[2],  &blue)) Badarg(5149,"blue");
  if(!egl_get_ushort(env, argv[3],  &alpha)) Badarg(5149,"alpha");
  weglColor4us(red,green,blue,alpha);
}

static void ecb_glTexCoord1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble s;
  if(!enif_get_double(env, argv[0],  &s)) Badarg(5150,"s");
  weglTexCoord1d(s);
}

static void ecb_glTexCoord1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat s;
  if(!egl_get_float(env, argv[0],  &s)) Badarg(5151,"s");
  weglTexCoord1f(s);
}

static void ecb_glTexCoord1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5152,"s");
  weglTexCoord1i(s);
}

static void ecb_glTexCoord1s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort s;
  if(!egl_get_short(env, argv[0],  &s)) Badarg(5153,"s");
  weglTexCoord1s(s);
}

static void ecb_glTexCoord2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble s;
  GLdouble t;
  if(!enif_get_double(env, argv[0],  &s)) Badarg(5154,"s");
  if(!enif_get_double(env, argv[1],  &t)) Badarg(5154,"t");
  weglTexCoord2d(s,t);
}

static void ecb_glTexCoord2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat s;
  GLfloat t;
  if(!egl_get_float(env, argv[0],  &s)) Badarg(5155,"s");
  if(!egl_get_float(env, argv[1],  &t)) Badarg(5155,"t");
  weglTexCoord2f(s,t);
}

static void ecb_glTexCoord2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  GLint t;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5156,"s");
  if(!enif_get_int(env, argv[1],  &t)) Badarg(5156,"t");
  weglTexCoord2i(s,t);
}

static void ecb_glTexCoord2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort s;
  GLshort t;
  if(!egl_get_short(env, argv[0],  &s)) Badarg(5157,"s");
  if(!egl_get_short(env, argv[1],  &t)) Badarg(5157,"t");
  weglTexCoord2s(s,t);
}

static void ecb_glTexCoord3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble s;
  GLdouble t;
  GLdouble r;
  if(!enif_get_double(env, argv[0],  &s)) Badarg(5158,"s");
  if(!enif_get_double(env, argv[1],  &t)) Badarg(5158,"t");
  if(!enif_get_double(env, argv[2],  &r)) Badarg(5158,"r");
  weglTexCoord3d(s,t,r);
}

static void ecb_glTexCoord3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat s;
  GLfloat t;
  GLfloat r;
  if(!egl_get_float(env, argv[0],  &s)) Badarg(5159,"s");
  if(!egl_get_float(env, argv[1],  &t)) Badarg(5159,"t");
  if(!egl_get_float(env, argv[2],  &r)) Badarg(5159,"r");
  weglTexCoord3f(s,t,r);
}

static void ecb_glTexCoord3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  GLint t;
  GLint r;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5160,"s");
  if(!enif_get_int(env, argv[1],  &t)) Badarg(5160,"t");
  if(!enif_get_int(env, argv[2],  &r)) Badarg(5160,"r");
  weglTexCoord3i(s,t,r);
}

static void ecb_glTexCoord3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort s;
  GLshort t;
  GLshort r;
  if(!egl_get_short(env, argv[0],  &s)) Badarg(5161,"s");
  if(!egl_get_short(env, argv[1],  &t)) Badarg(5161,"t");
  if(!egl_get_short(env, argv[2],  &r)) Badarg(5161,"r");
  weglTexCoord3s(s,t,r);
}

static void ecb_glTexCoord4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble s;
  GLdouble t;
  GLdouble r;
  GLdouble q;
  if(!enif_get_double(env, argv[0],  &s)) Badarg(5162,"s");
  if(!enif_get_double(env, argv[1],  &t)) Badarg(5162,"t");
  if(!enif_get_double(env, argv[2],  &r)) Badarg(5162,"r");
  if(!enif_get_double(env, argv[3],  &q)) Badarg(5162,"q");
  weglTexCoord4d(s,t,r,q);
}

static void ecb_glTexCoord4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat s;
  GLfloat t;
  GLfloat r;
  GLfloat q;
  if(!egl_get_float(env, argv[0],  &s)) Badarg(5163,"s");
  if(!egl_get_float(env, argv[1],  &t)) Badarg(5163,"t");
  if(!egl_get_float(env, argv[2],  &r)) Badarg(5163,"r");
  if(!egl_get_float(env, argv[3],  &q)) Badarg(5163,"q");
  weglTexCoord4f(s,t,r,q);
}

static void ecb_glTexCoord4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  GLint t;
  GLint r;
  GLint q;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5164,"s");
  if(!enif_get_int(env, argv[1],  &t)) Badarg(5164,"t");
  if(!enif_get_int(env, argv[2],  &r)) Badarg(5164,"r");
  if(!enif_get_int(env, argv[3],  &q)) Badarg(5164,"q");
  weglTexCoord4i(s,t,r,q);
}

static void ecb_glTexCoord4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort s;
  GLshort t;
  GLshort r;
  GLshort q;
  if(!egl_get_short(env, argv[0],  &s)) Badarg(5165,"s");
  if(!egl_get_short(env, argv[1],  &t)) Badarg(5165,"t");
  if(!egl_get_short(env, argv[2],  &r)) Badarg(5165,"r");
  if(!egl_get_short(env, argv[3],  &q)) Badarg(5165,"q");
  weglTexCoord4s(s,t,r,q);
}

static void ecb_glRasterPos2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5166,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5166,"y");
  weglRasterPos2d(x,y);
}

static void ecb_glRasterPos2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5167,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5167,"y");
  weglRasterPos2f(x,y);
}

static void ecb_glRasterPos2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5168,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5168,"y");
  weglRasterPos2i(x,y);
}

static void ecb_glRasterPos2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5169,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5169,"y");
  weglRasterPos2s(x,y);
}

static void ecb_glRasterPos3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5170,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5170,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5170,"z");
  weglRasterPos3d(x,y,z);
}

static void ecb_glRasterPos3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5171,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5171,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5171,"z");
  weglRasterPos3f(x,y,z);
}

static void ecb_glRasterPos3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5172,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5172,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5172,"z");
  weglRasterPos3i(x,y,z);
}

static void ecb_glRasterPos3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5173,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5173,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5173,"z");
  weglRasterPos3s(x,y,z);
}

static void ecb_glRasterPos4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5174,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5174,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5174,"z");
  if(!enif_get_double(env, argv[3],  &w)) Badarg(5174,"w");
  weglRasterPos4d(x,y,z,w);
}

static void ecb_glRasterPos4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5175,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5175,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5175,"z");
  if(!egl_get_float(env, argv[3],  &w)) Badarg(5175,"w");
  weglRasterPos4f(x,y,z,w);
}

static void ecb_glRasterPos4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  GLint w;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5176,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5176,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5176,"z");
  if(!enif_get_int(env, argv[3],  &w)) Badarg(5176,"w");
  weglRasterPos4i(x,y,z,w);
}

static void ecb_glRasterPos4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  GLshort w;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5177,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5177,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5177,"z");
  if(!egl_get_short(env, argv[3],  &w)) Badarg(5177,"w");
  weglRasterPos4s(x,y,z,w);
}

static void ecb_glRectd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x1;
  GLdouble y1;
  GLdouble x2;
  GLdouble y2;
  if(!enif_get_double(env, argv[0],  &x1)) Badarg(5178,"x1");
  if(!enif_get_double(env, argv[1],  &y1)) Badarg(5178,"y1");
  if(!enif_get_double(env, argv[2],  &x2)) Badarg(5178,"x2");
  if(!enif_get_double(env, argv[3],  &y2)) Badarg(5178,"y2");
  weglRectd(x1,y1,x2,y2);
}

static void ecb_glRectf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x1;
  GLfloat y1;
  GLfloat x2;
  GLfloat y2;
  if(!egl_get_float(env, argv[0],  &x1)) Badarg(5179,"x1");
  if(!egl_get_float(env, argv[1],  &y1)) Badarg(5179,"y1");
  if(!egl_get_float(env, argv[2],  &x2)) Badarg(5179,"x2");
  if(!egl_get_float(env, argv[3],  &y2)) Badarg(5179,"y2");
  weglRectf(x1,y1,x2,y2);
}

static void ecb_glRecti(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x1;
  GLint y1;
  GLint x2;
  GLint y2;
  if(!enif_get_int(env, argv[0],  &x1)) Badarg(5180,"x1");
  if(!enif_get_int(env, argv[1],  &y1)) Badarg(5180,"y1");
  if(!enif_get_int(env, argv[2],  &x2)) Badarg(5180,"x2");
  if(!enif_get_int(env, argv[3],  &y2)) Badarg(5180,"y2");
  weglRecti(x1,y1,x2,y2);
}

static void ecb_glRects(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x1;
  GLshort y1;
  GLshort x2;
  GLshort y2;
  if(!egl_get_short(env, argv[0],  &x1)) Badarg(5181,"x1");
  if(!egl_get_short(env, argv[1],  &y1)) Badarg(5181,"y1");
  if(!egl_get_short(env, argv[2],  &x2)) Badarg(5181,"x2");
  if(!egl_get_short(env, argv[3],  &y2)) Badarg(5181,"y2");
  weglRects(x1,y1,x2,y2);
}

static void ecb_glRectdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble v1[2];
  GLdouble v2[2];
  {
   int v1_a;
   const ERL_NIF_TERM *v1_t;
   if(!enif_get_tuple(env, argv[0], &v1_a, &v1_t) || v1_a != 2) {
     Badarg(5182,"v1");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, v1_t[i1++], &v1[0])) Badarg(5182,"v1");
     if(!enif_get_double(env, v1_t[i1++], &v1[1])) Badarg(5182,"v1");
   }};
  {
   int v2_a;
   const ERL_NIF_TERM *v2_t;
   if(!enif_get_tuple(env, argv[1], &v2_a, &v2_t) || v2_a != 2) {
     Badarg(5182,"v2");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, v2_t[i1++], &v2[0])) Badarg(5182,"v2");
     if(!enif_get_double(env, v2_t[i1++], &v2[1])) Badarg(5182,"v2");
   }};
  weglRectdv(v1,v2);
}

static void ecb_glRectfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat v1[2];
  GLfloat v2[2];
  {
   int v1_a;
   const ERL_NIF_TERM *v1_t;
   if(!enif_get_tuple(env, argv[0], &v1_a, &v1_t) || v1_a != 2) {
     Badarg(5183,"v1");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, v1_t[i1++], &v1[0])) Badarg(5183,"v1");
     if(!egl_get_float(env, v1_t[i1++], &v1[1])) Badarg(5183,"v1");
   }};
  {
   int v2_a;
   const ERL_NIF_TERM *v2_t;
   if(!enif_get_tuple(env, argv[1], &v2_a, &v2_t) || v2_a != 2) {
     Badarg(5183,"v2");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, v2_t[i1++], &v2[0])) Badarg(5183,"v2");
     if(!egl_get_float(env, v2_t[i1++], &v2[1])) Badarg(5183,"v2");
   }};
  weglRectfv(v1,v2);
}

static void ecb_glRectiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint v1[2];
  GLint v2[2];
  {
   int v1_a;
   const ERL_NIF_TERM *v1_t;
   if(!enif_get_tuple(env, argv[0], &v1_a, &v1_t) || v1_a != 2) {
     Badarg(5184,"v1");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v1_t[i1++], &v1[0])) Badarg(5184,"v1");
     if(!enif_get_int(env, v1_t[i1++], &v1[1])) Badarg(5184,"v1");
   }};
  {
   int v2_a;
   const ERL_NIF_TERM *v2_t;
   if(!enif_get_tuple(env, argv[1], &v2_a, &v2_t) || v2_a != 2) {
     Badarg(5184,"v2");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v2_t[i1++], &v2[0])) Badarg(5184,"v2");
     if(!enif_get_int(env, v2_t[i1++], &v2[1])) Badarg(5184,"v2");
   }};
  weglRectiv(v1,v2);
}

static void ecb_glRectsv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort v1[2];
  GLshort v2[2];
  {
   int v1_a;
   const ERL_NIF_TERM *v1_t;
   if(!enif_get_tuple(env, argv[0], &v1_a, &v1_t) || v1_a != 2) {
     Badarg(5185,"v1");
   } else {
    int i1 = 0;
     if(!egl_get_short(env, v1_t[i1++], &v1[0])) Badarg(5185,"v1");
     if(!egl_get_short(env, v1_t[i1++], &v1[1])) Badarg(5185,"v1");
   }};
  {
   int v2_a;
   const ERL_NIF_TERM *v2_t;
   if(!enif_get_tuple(env, argv[1], &v2_a, &v2_t) || v2_a != 2) {
     Badarg(5185,"v2");
   } else {
    int i1 = 0;
     if(!egl_get_short(env, v2_t[i1++], &v2[0])) Badarg(5185,"v2");
     if(!egl_get_short(env, v2_t[i1++], &v2[1])) Badarg(5185,"v2");
   }};
  weglRectsv(v1,v2);
}

static void ecb_glVertexPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5186,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5186,"type");
  if(!enif_get_int(env, argv[2],  &stride)) Badarg(5186,"stride");
  if(!egl_get_ptr(env, argv[3], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5186,"ptr");
  }
  weglVertexPointer(size,type,stride,ptr_idx);
}

static void ecb_glNormalPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5188,"type");
  if(!enif_get_int(env, argv[1],  &stride)) Badarg(5188,"stride");
  if(!egl_get_ptr(env, argv[2], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5188,"ptr");
  }
  weglNormalPointer(type,stride,ptr_idx);
}

static void ecb_glColorPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5190,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5190,"type");
  if(!enif_get_int(env, argv[2],  &stride)) Badarg(5190,"stride");
  if(!egl_get_ptr(env, argv[3], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5190,"ptr");
  }
  weglColorPointer(size,type,stride,ptr_idx);
}

static void ecb_glIndexPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5192,"type");
  if(!enif_get_int(env, argv[1],  &stride)) Badarg(5192,"stride");
  if(!egl_get_ptr(env, argv[2], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5192,"ptr");
  }
  weglIndexPointer(type,stride,ptr_idx);
}

static void ecb_glTexCoordPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5194,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5194,"type");
  if(!enif_get_int(env, argv[2],  &stride)) Badarg(5194,"stride");
  if(!egl_get_ptr(env, argv[3], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5194,"ptr");
  }
  weglTexCoordPointer(size,type,stride,ptr_idx);
}

static void ecb_glEdgeFlagPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei stride;
  ErlNifBinary ptr;
  GLvoid *ptr_idx;
  if(!enif_get_int(env, argv[0],  &stride)) Badarg(5196,"stride");
  if(!egl_get_ptr(env, argv[1], (void **) &ptr_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[1], &ptr))
        ptr_idx = (GLvoid *) ptr.data;
    else Badarg(5196,"ptr");
  }
  weglEdgeFlagPointer(stride,ptr_idx);
}

static void ecb_glArrayElement(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint i;
  if(!enif_get_int(env, argv[0],  &i)) Badarg(5198,"i");
  weglArrayElement(i);
}

static void ecb_glDrawArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint first;
  GLsizei count;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5199,"mode");
  if(!enif_get_int(env, argv[1],  &first)) Badarg(5199,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5199,"count");
  weglDrawArrays(mode,first,count);
}

static void ecb_glDrawElements(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  GLvoid *indices_idx;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5200,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5200,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5200,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (GLvoid *) indices.data;
    else Badarg(5200,"indices");
  }
  weglDrawElements(mode,count,type,indices_idx);
}

static void ecb_glInterleavedArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum format;
  GLsizei stride;
  ErlNifBinary pointer;
  GLvoid *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &format)) Badarg(5202,"format");
  if(!enif_get_int(env, argv[1],  &stride)) Badarg(5202,"stride");
  if(!egl_get_ptr(env, argv[2], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &pointer))
        pointer_idx = (GLvoid *) pointer.data;
    else Badarg(5202,"pointer");
  }
  weglInterleavedArrays(format,stride,pointer_idx);
}

static void ecb_glShadeModel(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5204,"mode");
  weglShadeModel(mode);
}

static void ecb_glLightf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum light;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5205,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5205,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5205,"param");
  weglLightf(light,pname,param);
}

static void ecb_glLighti(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum light;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5206,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5206,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5206,"param");
  weglLighti(light,pname,param);
}

static void ecb_glLightfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum light;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5207,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5207,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5207,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5207,"params");
   }};
  weglLightfv(light,pname,params);
}

static void ecb_glLightiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum light;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5208,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5208,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5208,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5208,"params");
   }};
  weglLightiv(light,pname,params);
}

static void ecb_glGetLightfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum light;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5209,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5209,"pname");
  weglGetLightfv(light,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetLightiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum light;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &light)) Badarg(5210,"light");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5210,"pname");
  weglGetLightiv(light,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glLightModelf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5211,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5211,"param");
  weglLightModelf(pname,param);
}

static void ecb_glLightModeli(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5212,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5212,"param");
  weglLightModeli(pname,param);
}

static void ecb_glLightModelfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5213,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5213,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5213,"params");
   }};
  weglLightModelfv(pname,params);
}

static void ecb_glLightModeliv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5214,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5214,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5214,"params");
   }};
  weglLightModeliv(pname,params);
}

static void ecb_glMaterialf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5215,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5215,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5215,"param");
  weglMaterialf(face,pname,param);
}

static void ecb_glMateriali(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5216,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5216,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5216,"param");
  weglMateriali(face,pname,param);
}

static void ecb_glMaterialfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5217,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5217,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5217,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5217,"params");
   }};
  weglMaterialfv(face,pname,params);
}

static void ecb_glMaterialiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5218,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5218,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5218,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5218,"params");
   }};
  weglMaterialiv(face,pname,params);
}

static void ecb_glGetMaterialfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum face;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5219,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5219,"pname");
  weglGetMaterialfv(face,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetMaterialiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum face;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5220,"face");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5220,"pname");
  weglGetMaterialiv(face,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glColorMaterial(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5221,"face");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5221,"mode");
  weglColorMaterial(face,mode);
}

static void ecb_glPixelZoom(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat xfactor;
  GLfloat yfactor;
  if(!egl_get_float(env, argv[0],  &xfactor)) Badarg(5222,"xfactor");
  if(!egl_get_float(env, argv[1],  &yfactor)) Badarg(5222,"yfactor");
  weglPixelZoom(xfactor,yfactor);
}

static void ecb_glPixelStoref(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5223,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5223,"param");
  weglPixelStoref(pname,param);
}

static void ecb_glPixelStorei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5224,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5224,"param");
  weglPixelStorei(pname,param);
}

static void ecb_glPixelTransferf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5225,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5225,"param");
  weglPixelTransferf(pname,param);
}

static void ecb_glPixelTransferi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5226,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5226,"param");
  weglPixelTransferi(pname,param);
}

static void ecb_glPixelMapfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  GLsizei mapsize;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5227,"map");
  if(!enif_get_int(env, argv[1],  &mapsize)) Badarg(5227,"mapsize");
  if(!enif_inspect_binary(env, argv[2], &values)) Badarg(5227,"values");
  weglPixelMapfv(map,mapsize,(GLfloat *) values.data);
}

static void ecb_glPixelMapuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  GLsizei mapsize;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5228,"map");
  if(!enif_get_int(env, argv[1],  &mapsize)) Badarg(5228,"mapsize");
  if(!enif_inspect_binary(env, argv[2], &values)) Badarg(5228,"values");
  weglPixelMapuiv(map,mapsize,(GLuint *) values.data);
}

static void ecb_glPixelMapusv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  GLsizei mapsize;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5229,"map");
  if(!enif_get_int(env, argv[1],  &mapsize)) Badarg(5229,"mapsize");
  if(!enif_inspect_binary(env, argv[2], &values)) Badarg(5229,"values");
  weglPixelMapusv(map,mapsize,(GLushort *) values.data);
}

static void ecb_glGetPixelMapfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5230,"map");
  if(enif_is_binary(env, argv[1]))
    enif_inspect_binary(env, argv[1], &values);
  else if(enif_is_tuple(env, argv[1])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[1], &values_a, &values_t) ||
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5230, "values");
  } else Badarg(5230, "values");
  weglGetPixelMapfv(map,(GLfloat *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetPixelMapuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5231,"map");
  if(enif_is_binary(env, argv[1]))
    enif_inspect_binary(env, argv[1], &values);
  else if(enif_is_tuple(env, argv[1])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[1], &values_a, &values_t) ||
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5231, "values");
  } else Badarg(5231, "values");
  weglGetPixelMapuiv(map,(GLuint *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetPixelMapusv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum map;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &map)) Badarg(5232,"map");
  if(enif_is_binary(env, argv[1]))
    enif_inspect_binary(env, argv[1], &values);
  else if(enif_is_tuple(env, argv[1])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[1], &values_a, &values_t) ||
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5232, "values");
  } else Badarg(5232, "values");
  weglGetPixelMapusv(map,(GLushort *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glBitmap(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei width;
  GLsizei height;
  GLfloat xorig;
  GLfloat yorig;
  GLfloat xmove;
  GLfloat ymove;
  ErlNifBinary bitmap;
  GLubyte *bitmap_idx;
  if(!enif_get_int(env, argv[0],  &width)) Badarg(5233,"width");
  if(!enif_get_int(env, argv[1],  &height)) Badarg(5233,"height");
  if(!egl_get_float(env, argv[2],  &xorig)) Badarg(5233,"xorig");
  if(!egl_get_float(env, argv[3],  &yorig)) Badarg(5233,"yorig");
  if(!egl_get_float(env, argv[4],  &xmove)) Badarg(5233,"xmove");
  if(!egl_get_float(env, argv[5],  &ymove)) Badarg(5233,"ymove");
  if(!egl_get_ptr(env, argv[6], (void **) &bitmap_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &bitmap))
        bitmap_idx = (GLubyte *) bitmap.data;
    else Badarg(5233,"bitmap");
  }
  weglBitmap(width,height,xorig,yorig,xmove,ymove,bitmap_idx);
}

static void ecb_glReadPixels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5235,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5235,"y");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5235,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5235,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5235,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5235,"type");
  if(enif_is_binary(env, argv[6]))
    enif_inspect_binary(env, argv[6], &pixels);
  else if(enif_is_tuple(env, argv[6])) {
    int pixels_a;
    const ERL_NIF_TERM *pixels_t;
    if(enif_get_tuple(env, argv[6], &pixels_a, &pixels_t) ||
         enif_is_binary(env, pixels_t[1]))
       enif_inspect_binary(env, pixels_t[1], &pixels);
    else Badarg(5235, "pixels");
  } else Badarg(5235, "pixels");
  weglReadPixels(x,y,width,height,format,type,(GLvoid *) pixels.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glDrawPixels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_int(env, argv[0],  &width)) Badarg(5236,"width");
  if(!enif_get_int(env, argv[1],  &height)) Badarg(5236,"height");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5236,"format");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5236,"type");
  if(!egl_get_ptr(env, argv[4], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[4], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5236,"pixels");
  }
  weglDrawPixels(width,height,format,type,pixels_idx);
}

static void ecb_glCopyPixels(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  GLenum type;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5238,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5238,"y");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5238,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5238,"height");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5238,"type");
  weglCopyPixels(x,y,width,height,type);
}

static void ecb_glStencilFunc(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum func;
  GLint ref;
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &func)) Badarg(5239,"func");
  if(!enif_get_int(env, argv[1],  &ref)) Badarg(5239,"ref");
  if(!enif_get_uint(env, argv[2],  &mask)) Badarg(5239,"mask");
  weglStencilFunc(func,ref,mask);
}

static void ecb_glStencilMask(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &mask)) Badarg(5240,"mask");
  weglStencilMask(mask);
}

static void ecb_glStencilOp(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum fail;
  GLenum zfail;
  GLenum zpass;
  if(!enif_get_uint(env, argv[0],  &fail)) Badarg(5241,"fail");
  if(!enif_get_uint(env, argv[1],  &zfail)) Badarg(5241,"zfail");
  if(!enif_get_uint(env, argv[2],  &zpass)) Badarg(5241,"zpass");
  weglStencilOp(fail,zfail,zpass);
}

static void ecb_glClearStencil(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint s;
  if(!enif_get_int(env, argv[0],  &s)) Badarg(5242,"s");
  weglClearStencil(s);
}

static void ecb_glTexGend(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLdouble param;
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5243,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5243,"pname");
  if(!enif_get_double(env, argv[2],  &param)) Badarg(5243,"param");
  weglTexGend(coord,pname,param);
}

static void ecb_glTexGenf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5244,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5244,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5244,"param");
  weglTexGenf(coord,pname,param);
}

static void ecb_glTexGeni(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5245,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5245,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5245,"param");
  weglTexGeni(coord,pname,param);
}

static void ecb_glTexGendv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5246,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5246,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5246,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_double(env, params_t[i], &params[i])) Badarg(5246,"params");
   }};
  weglTexGendv(coord,pname,params);
}

static void ecb_glTexGenfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5247,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5247,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5247,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5247,"params");
   }};
  weglTexGenfv(coord,pname,params);
}

static void ecb_glTexGeniv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum coord;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5248,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5248,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5248,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5248,"params");
   }};
  weglTexGeniv(coord,pname,params);
}

static void ecb_glGetTexGendv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum coord;
  GLenum pname;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5249,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5249,"pname");
  weglGetTexGendv(coord,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetTexGenfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum coord;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5250,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5250,"pname");
  weglGetTexGenfv(coord,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetTexGeniv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum coord;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &coord)) Badarg(5251,"coord");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5251,"pname");
  weglGetTexGeniv(coord,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glTexEnvf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5252,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5252,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5252,"param");
  weglTexEnvf(target,pname,param);
}

static void ecb_glTexEnvi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5253,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5253,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5253,"param");
  weglTexEnvi(target,pname,param);
}

static void ecb_glTexEnvfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5254,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5254,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5254,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5254,"params");
   }};
  weglTexEnvfv(target,pname,params);
}

static void ecb_glTexEnviv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5255,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5255,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5255,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5255,"params");
   }};
  weglTexEnviv(target,pname,params);
}

static void ecb_glGetTexEnvfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5256,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5256,"pname");
  weglGetTexEnvfv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetTexEnviv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5257,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5257,"pname");
  weglGetTexEnviv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glTexParameterf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5258,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5258,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5258,"param");
  weglTexParameterf(target,pname,param);
}

static void ecb_glTexParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5259,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5259,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5259,"param");
  weglTexParameteri(target,pname,param);
}

static void ecb_glTexParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5260,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5260,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5260,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5260,"params");
   }};
  weglTexParameterfv(target,pname,params);
}

static void ecb_glTexParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5261,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5261,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5261,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5261,"params");
   }};
  weglTexParameteriv(target,pname,params);
}

static void ecb_glGetTexParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5262,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5262,"pname");
  weglGetTexParameterfv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetTexParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5263,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5263,"pname");
  weglGetTexParameteriv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetTexLevelParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLint level;
  GLenum pname;
  GLfloat params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5264,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5264,"level");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5264,"pname");
  weglGetTexLevelParameterfv(target,level,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_double(env, (double) params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetTexLevelParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLint level;
  GLenum pname;
  GLint params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5265,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5265,"level");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5265,"pname");
  weglGetTexLevelParameteriv(target,level,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_int(env, params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glTexImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint internalFormat;
  GLsizei width;
  GLint border;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5266,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5266,"level");
  if(!enif_get_int(env, argv[2],  &internalFormat)) Badarg(5266,"internalFormat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5266,"width");
  if(!enif_get_int(env, argv[4],  &border)) Badarg(5266,"border");
  if(!enif_get_uint(env, argv[5],  &format)) Badarg(5266,"format");
  if(!enif_get_uint(env, argv[6],  &type)) Badarg(5266,"type");
  if(!egl_get_ptr(env, argv[7], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[7], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5266,"pixels");
  }
  weglTexImage1D(target,level,internalFormat,width,border,format,type,pixels_idx);
}

static void ecb_glTexImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLint border;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5268,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5268,"level");
  if(!enif_get_int(env, argv[2],  &internalFormat)) Badarg(5268,"internalFormat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5268,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5268,"height");
  if(!enif_get_int(env, argv[5],  &border)) Badarg(5268,"border");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5268,"format");
  if(!enif_get_uint(env, argv[7],  &type)) Badarg(5268,"type");
  if(!egl_get_ptr(env, argv[8], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5268,"pixels");
  }
  weglTexImage2D(target,level,internalFormat,width,height,border,format,type,pixels_idx);
}

static void ecb_glGetTexImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5270,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5270,"level");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5270,"format");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5270,"type");
  if(enif_is_binary(env, argv[4]))
    enif_inspect_binary(env, argv[4], &pixels);
  else if(enif_is_tuple(env, argv[4])) {
    int pixels_a;
    const ERL_NIF_TERM *pixels_t;
    if(enif_get_tuple(env, argv[4], &pixels_a, &pixels_t) ||
         enif_is_binary(env, pixels_t[1]))
       enif_inspect_binary(env, pixels_t[1], &pixels);
    else Badarg(5270, "pixels");
  } else Badarg(5270, "pixels");
  weglGetTexImage(target,level,format,type,(GLvoid *) pixels.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGenTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * textures;
  ERL_NIF_TERM *textures_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5271,"n");
  textures = enif_alloc((int)n*sizeof(GLuint));
  textures_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenTextures(n,textures);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      textures_ts[ri] =      enif_make_int(env, textures[ri]);}
  reply =      enif_make_list_from_array(env, textures_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(textures_ts);
 enif_free(textures);
}

static void ecb_glDeleteTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int textures_free = 0;
  GLuint *textures;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5272,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5272, "textures"); }
  else {
    int i=0;
    ERL_NIF_TERM textures_l, textures_h, textures_t;
    textures_free = 1;
    textures = (GLuint *) enif_alloc(n * sizeof(GLuint));
    textures_l = argv[1];
    while(enif_get_list_cell(env, textures_l, &textures_h, &textures_t)) {
        if(!enif_get_uint(env, textures_h, &textures[i++])) Badarg(5272,"textures");
        textures_l = textures_t;
    };
  }
  weglDeleteTextures(n,textures);
 if(textures_free) enif_free(textures);
}

static void ecb_glBindTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint texture;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5273,"target");
  if(!enif_get_uint(env, argv[1],  &texture)) Badarg(5273,"texture");
  weglBindTexture(target,texture);
}

static void ecb_glPrioritizeTextures(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int textures_free = 0;
  GLuint *textures;
  int priorities_free = 0;
  GLclampf *priorities;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5274,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5274, "textures"); }
  else {
    int i=0;
    ERL_NIF_TERM textures_l, textures_h, textures_t;
    textures_free = 1;
    textures = (GLuint *) enif_alloc(n * sizeof(GLuint));
    textures_l = argv[1];
    while(enif_get_list_cell(env, textures_l, &textures_h, &textures_t)) {
        if(!enif_get_uint(env, textures_h, &textures[i++])) Badarg(5274,"textures");
        textures_l = textures_t;
    };
  }
  if(!enif_is_list(env, argv[2])) { Badarg(5274, "priorities"); }
  else {
    int i=0;
    ERL_NIF_TERM priorities_l, priorities_h, priorities_t;
    unsigned int priorities_len;
    enif_get_list_length(env, argv[2], &priorities_len);
    priorities_free = 1;
    priorities = (GLclampf *) enif_alloc(priorities_len * sizeof(GLclampf));
    priorities_l = argv[2];
    while(enif_get_list_cell(env, priorities_l, &priorities_h, &priorities_t)) {
        if(!egl_get_float(env, priorities_h, &priorities[i++])) Badarg(5274,"priorities");
        priorities_l = priorities_t;
    };
  }
  weglPrioritizeTextures(n,textures,priorities);
 if(priorities_free) enif_free(priorities);
 if(textures_free) enif_free(textures);
}

static void ecb_glAreTexturesResident(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLsizei n;
  int textures_free = 0;
  GLuint *textures;
  GLboolean * residences;
  ERL_NIF_TERM *residences_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5275,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5275, "textures"); }
  else {
    int i=0;
    ERL_NIF_TERM textures_l, textures_h, textures_t;
    textures_free = 1;
    textures = (GLuint *) enif_alloc(n * sizeof(GLuint));
    textures_l = argv[1];
    while(enif_get_list_cell(env, textures_l, &textures_h, &textures_t)) {
        if(!enif_get_uint(env, textures_h, &textures[i++])) Badarg(5275,"textures");
        textures_l = textures_t;
    };
  }
  residences = enif_alloc((int)n*sizeof(GLboolean));
  residences_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  result = weglAreTexturesResident(n,textures,residences);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      residences_ts[ri] =      enif_make_int(env, residences[ri]);}
  reply = enif_make_tuple2(env,
          enif_make_int(env, result),
     enif_make_list_from_array(env, residences_ts, n) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(residences_ts);
 enif_free(residences);
 if(textures_free) enif_free(textures);
}

static void ecb_glIsTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint texture;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5276,"texture");
  result = weglIsTexture(texture);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glTexSubImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLsizei width;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5277,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5277,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5277,"xoffset");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5277,"width");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5277,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5277,"type");
  if(!egl_get_ptr(env, argv[6], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5277,"pixels");
  }
  weglTexSubImage1D(target,level,xoffset,width,format,type,pixels_idx);
}

static void ecb_glTexSubImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5279,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5279,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5279,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5279,"yoffset");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5279,"width");
  if(!enif_get_int(env, argv[5],  &height)) Badarg(5279,"height");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5279,"format");
  if(!enif_get_uint(env, argv[7],  &type)) Badarg(5279,"type");
  if(!egl_get_ptr(env, argv[8], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5279,"pixels");
  }
  weglTexSubImage2D(target,level,xoffset,yoffset,width,height,format,type,pixels_idx);
}

static void ecb_glCopyTexImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  GLint border;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5281,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5281,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5281,"internalformat");
  if(!enif_get_int(env, argv[3],  &x)) Badarg(5281,"x");
  if(!enif_get_int(env, argv[4],  &y)) Badarg(5281,"y");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5281,"width");
  if(!enif_get_int(env, argv[6],  &border)) Badarg(5281,"border");
  weglCopyTexImage1D(target,level,internalformat,x,y,width,border);
}

static void ecb_glCopyTexImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  GLint border;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5282,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5282,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5282,"internalformat");
  if(!enif_get_int(env, argv[3],  &x)) Badarg(5282,"x");
  if(!enif_get_int(env, argv[4],  &y)) Badarg(5282,"y");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5282,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5282,"height");
  if(!enif_get_int(env, argv[7],  &border)) Badarg(5282,"border");
  weglCopyTexImage2D(target,level,internalformat,x,y,width,height,border);
}

static void ecb_glCopyTexSubImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint x;
  GLint y;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5283,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5283,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5283,"xoffset");
  if(!enif_get_int(env, argv[3],  &x)) Badarg(5283,"x");
  if(!enif_get_int(env, argv[4],  &y)) Badarg(5283,"y");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5283,"width");
  weglCopyTexSubImage1D(target,level,xoffset,x,y,width);
}

static void ecb_glCopyTexSubImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5284,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5284,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5284,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5284,"yoffset");
  if(!enif_get_int(env, argv[4],  &x)) Badarg(5284,"x");
  if(!enif_get_int(env, argv[5],  &y)) Badarg(5284,"y");
  if(!enif_get_int(env, argv[6],  &width)) Badarg(5284,"width");
  if(!enif_get_int(env, argv[7],  &height)) Badarg(5284,"height");
  weglCopyTexSubImage2D(target,level,xoffset,yoffset,x,y,width,height);
}

static void ecb_glMap1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble u1;
  GLdouble u2;
  GLint stride;
  GLint order;
  ErlNifBinary points;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5285,"target");
  if(!enif_get_double(env, argv[1],  &u1)) Badarg(5285,"u1");
  if(!enif_get_double(env, argv[2],  &u2)) Badarg(5285,"u2");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5285,"stride");
  if(!enif_get_int(env, argv[4],  &order)) Badarg(5285,"order");
  if(!enif_inspect_binary(env, argv[5], &points)) Badarg(5285,"points");
  weglMap1d(target,u1,u2,stride,order,(GLdouble *) points.data);
}

static void ecb_glMap1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat u1;
  GLfloat u2;
  GLint stride;
  GLint order;
  ErlNifBinary points;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5286,"target");
  if(!egl_get_float(env, argv[1],  &u1)) Badarg(5286,"u1");
  if(!egl_get_float(env, argv[2],  &u2)) Badarg(5286,"u2");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5286,"stride");
  if(!enif_get_int(env, argv[4],  &order)) Badarg(5286,"order");
  if(!enif_inspect_binary(env, argv[5], &points)) Badarg(5286,"points");
  weglMap1f(target,u1,u2,stride,order,(GLfloat *) points.data);
}

static void ecb_glMap2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble u1;
  GLdouble u2;
  GLint ustride;
  GLint uorder;
  GLdouble v1;
  GLdouble v2;
  GLint vstride;
  GLint vorder;
  ErlNifBinary points;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5287,"target");
  if(!enif_get_double(env, argv[1],  &u1)) Badarg(5287,"u1");
  if(!enif_get_double(env, argv[2],  &u2)) Badarg(5287,"u2");
  if(!enif_get_int(env, argv[3],  &ustride)) Badarg(5287,"ustride");
  if(!enif_get_int(env, argv[4],  &uorder)) Badarg(5287,"uorder");
  if(!enif_get_double(env, argv[5],  &v1)) Badarg(5287,"v1");
  if(!enif_get_double(env, argv[6],  &v2)) Badarg(5287,"v2");
  if(!enif_get_int(env, argv[7],  &vstride)) Badarg(5287,"vstride");
  if(!enif_get_int(env, argv[8],  &vorder)) Badarg(5287,"vorder");
  if(!enif_inspect_binary(env, argv[9], &points)) Badarg(5287,"points");
  weglMap2d(target,u1,u2,ustride,uorder,v1,v2,vstride,vorder,(GLdouble *) points.data);
}

static void ecb_glMap2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat u1;
  GLfloat u2;
  GLint ustride;
  GLint uorder;
  GLfloat v1;
  GLfloat v2;
  GLint vstride;
  GLint vorder;
  ErlNifBinary points;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5288,"target");
  if(!egl_get_float(env, argv[1],  &u1)) Badarg(5288,"u1");
  if(!egl_get_float(env, argv[2],  &u2)) Badarg(5288,"u2");
  if(!enif_get_int(env, argv[3],  &ustride)) Badarg(5288,"ustride");
  if(!enif_get_int(env, argv[4],  &uorder)) Badarg(5288,"uorder");
  if(!egl_get_float(env, argv[5],  &v1)) Badarg(5288,"v1");
  if(!egl_get_float(env, argv[6],  &v2)) Badarg(5288,"v2");
  if(!enif_get_int(env, argv[7],  &vstride)) Badarg(5288,"vstride");
  if(!enif_get_int(env, argv[8],  &vorder)) Badarg(5288,"vorder");
  if(!enif_inspect_binary(env, argv[9], &points)) Badarg(5288,"points");
  weglMap2f(target,u1,u2,ustride,uorder,v1,v2,vstride,vorder,(GLfloat *) points.data);
}

static void ecb_glGetMapdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum query;
  ErlNifBinary v;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5289,"target");
  if(!enif_get_uint(env, argv[1],  &query)) Badarg(5289,"query");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &v);
  else if(enif_is_tuple(env, argv[2])) {
    int v_a;
    const ERL_NIF_TERM *v_t;
    if(enif_get_tuple(env, argv[2], &v_a, &v_t) ||
         enif_is_binary(env, v_t[1]))
       enif_inspect_binary(env, v_t[1], &v);
    else Badarg(5289, "v");
  } else Badarg(5289, "v");
  weglGetMapdv(target,query,(GLdouble *) v.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetMapfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum query;
  ErlNifBinary v;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5290,"target");
  if(!enif_get_uint(env, argv[1],  &query)) Badarg(5290,"query");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &v);
  else if(enif_is_tuple(env, argv[2])) {
    int v_a;
    const ERL_NIF_TERM *v_t;
    if(enif_get_tuple(env, argv[2], &v_a, &v_t) ||
         enif_is_binary(env, v_t[1]))
       enif_inspect_binary(env, v_t[1], &v);
    else Badarg(5290, "v");
  } else Badarg(5290, "v");
  weglGetMapfv(target,query,(GLfloat *) v.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetMapiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum query;
  ErlNifBinary v;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5291,"target");
  if(!enif_get_uint(env, argv[1],  &query)) Badarg(5291,"query");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &v);
  else if(enif_is_tuple(env, argv[2])) {
    int v_a;
    const ERL_NIF_TERM *v_t;
    if(enif_get_tuple(env, argv[2], &v_a, &v_t) ||
         enif_is_binary(env, v_t[1]))
       enif_inspect_binary(env, v_t[1], &v);
    else Badarg(5291, "v");
  } else Badarg(5291, "v");
  weglGetMapiv(target,query,(GLint *) v.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glEvalCoord1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble u;
  if(!enif_get_double(env, argv[0],  &u)) Badarg(5292,"u");
  weglEvalCoord1d(u);
}

static void ecb_glEvalCoord1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat u;
  if(!egl_get_float(env, argv[0],  &u)) Badarg(5293,"u");
  weglEvalCoord1f(u);
}

static void ecb_glEvalCoord2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble u;
  GLdouble v;
  if(!enif_get_double(env, argv[0],  &u)) Badarg(5294,"u");
  if(!enif_get_double(env, argv[1],  &v)) Badarg(5294,"v");
  weglEvalCoord2d(u,v);
}

static void ecb_glEvalCoord2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat u;
  GLfloat v;
  if(!egl_get_float(env, argv[0],  &u)) Badarg(5295,"u");
  if(!egl_get_float(env, argv[1],  &v)) Badarg(5295,"v");
  weglEvalCoord2f(u,v);
}

static void ecb_glMapGrid1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint un;
  GLdouble u1;
  GLdouble u2;
  if(!enif_get_int(env, argv[0],  &un)) Badarg(5296,"un");
  if(!enif_get_double(env, argv[1],  &u1)) Badarg(5296,"u1");
  if(!enif_get_double(env, argv[2],  &u2)) Badarg(5296,"u2");
  weglMapGrid1d(un,u1,u2);
}

static void ecb_glMapGrid1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint un;
  GLfloat u1;
  GLfloat u2;
  if(!enif_get_int(env, argv[0],  &un)) Badarg(5297,"un");
  if(!egl_get_float(env, argv[1],  &u1)) Badarg(5297,"u1");
  if(!egl_get_float(env, argv[2],  &u2)) Badarg(5297,"u2");
  weglMapGrid1f(un,u1,u2);
}

static void ecb_glMapGrid2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint un;
  GLdouble u1;
  GLdouble u2;
  GLint vn;
  GLdouble v1;
  GLdouble v2;
  if(!enif_get_int(env, argv[0],  &un)) Badarg(5298,"un");
  if(!enif_get_double(env, argv[1],  &u1)) Badarg(5298,"u1");
  if(!enif_get_double(env, argv[2],  &u2)) Badarg(5298,"u2");
  if(!enif_get_int(env, argv[3],  &vn)) Badarg(5298,"vn");
  if(!enif_get_double(env, argv[4],  &v1)) Badarg(5298,"v1");
  if(!enif_get_double(env, argv[5],  &v2)) Badarg(5298,"v2");
  weglMapGrid2d(un,u1,u2,vn,v1,v2);
}

static void ecb_glMapGrid2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint un;
  GLfloat u1;
  GLfloat u2;
  GLint vn;
  GLfloat v1;
  GLfloat v2;
  if(!enif_get_int(env, argv[0],  &un)) Badarg(5299,"un");
  if(!egl_get_float(env, argv[1],  &u1)) Badarg(5299,"u1");
  if(!egl_get_float(env, argv[2],  &u2)) Badarg(5299,"u2");
  if(!enif_get_int(env, argv[3],  &vn)) Badarg(5299,"vn");
  if(!egl_get_float(env, argv[4],  &v1)) Badarg(5299,"v1");
  if(!egl_get_float(env, argv[5],  &v2)) Badarg(5299,"v2");
  weglMapGrid2f(un,u1,u2,vn,v1,v2);
}

static void ecb_glEvalPoint1(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint i;
  if(!enif_get_int(env, argv[0],  &i)) Badarg(5300,"i");
  weglEvalPoint1(i);
}

static void ecb_glEvalPoint2(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint i;
  GLint j;
  if(!enif_get_int(env, argv[0],  &i)) Badarg(5301,"i");
  if(!enif_get_int(env, argv[1],  &j)) Badarg(5301,"j");
  weglEvalPoint2(i,j);
}

static void ecb_glEvalMesh1(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint i1;
  GLint i2;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5302,"mode");
  if(!enif_get_int(env, argv[1],  &i1)) Badarg(5302,"i1");
  if(!enif_get_int(env, argv[2],  &i2)) Badarg(5302,"i2");
  weglEvalMesh1(mode,i1,i2);
}

static void ecb_glEvalMesh2(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint i1;
  GLint i2;
  GLint j1;
  GLint j2;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5303,"mode");
  if(!enif_get_int(env, argv[1],  &i1)) Badarg(5303,"i1");
  if(!enif_get_int(env, argv[2],  &i2)) Badarg(5303,"i2");
  if(!enif_get_int(env, argv[3],  &j1)) Badarg(5303,"j1");
  if(!enif_get_int(env, argv[4],  &j2)) Badarg(5303,"j2");
  weglEvalMesh2(mode,i1,i2,j1,j2);
}

static void ecb_glFogf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5304,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5304,"param");
  weglFogf(pname,param);
}

static void ecb_glFogi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5305,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5305,"param");
  weglFogi(pname,param);
}

static void ecb_glFogfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5306,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5306,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5306,"params");
   }};
  weglFogfv(pname,params);
}

static void ecb_glFogiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5307,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5307,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5307,"params");
   }};
  weglFogiv(pname,params);
}

static void ecb_glFeedbackBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei size;
  GLenum type;
  ErlNifBinary buffer;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5308,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5308,"type");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &buffer);
  else if(enif_is_tuple(env, argv[2])) {
    int buffer_a;
    const ERL_NIF_TERM *buffer_t;
    if(enif_get_tuple(env, argv[2], &buffer_a, &buffer_t) ||
         enif_is_binary(env, buffer_t[1]))
       enif_inspect_binary(env, buffer_t[1], &buffer);
    else Badarg(5308, "buffer");
  } else Badarg(5308, "buffer");
  weglFeedbackBuffer(size,type,(GLfloat *) buffer.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glPassThrough(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat token;
  if(!egl_get_float(env, argv[0],  &token)) Badarg(5309,"token");
  weglPassThrough(token);
}

static void ecb_glSelectBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei size;
  ErlNifBinary buffer;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5310,"size");
  if(enif_is_binary(env, argv[1]))
    enif_inspect_binary(env, argv[1], &buffer);
  else if(enif_is_tuple(env, argv[1])) {
    int buffer_a;
    const ERL_NIF_TERM *buffer_t;
    if(enif_get_tuple(env, argv[1], &buffer_a, &buffer_t) ||
         enif_is_binary(env, buffer_t[1]))
       enif_inspect_binary(env, buffer_t[1], &buffer);
    else Badarg(5310, "buffer");
  } else Badarg(5310, "buffer");
  weglSelectBuffer(size,(GLuint *) buffer.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glInitNames(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglInitNames();
}

static void ecb_glLoadName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint name;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5312,"name");
  weglLoadName(name);
}

static void ecb_glPushName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint name;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5313,"name");
  weglPushName(name);
}

static void ecb_glPopName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPopName();
}

static void ecb_glBlendColor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf red;
  GLclampf green;
  GLclampf blue;
  GLclampf alpha;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5315,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5315,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5315,"blue");
  if(!egl_get_float(env, argv[3],  &alpha)) Badarg(5315,"alpha");
  weglBlendColor(red,green,blue,alpha);
}

static void ecb_glBlendEquation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5316,"mode");
  weglBlendEquation(mode);
}

static void ecb_glDrawRangeElements(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint start;
  GLuint end;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  GLvoid *indices_idx;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5317,"mode");
  if(!enif_get_uint(env, argv[1],  &start)) Badarg(5317,"start");
  if(!enif_get_uint(env, argv[2],  &end)) Badarg(5317,"end");
  if(!enif_get_int(env, argv[3],  &count)) Badarg(5317,"count");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5317,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &indices))
        indices_idx = (GLvoid *) indices.data;
    else Badarg(5317,"indices");
  }
  weglDrawRangeElements(mode,start,end,count,type,indices_idx);
}

static void ecb_glTexImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint internalFormat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLint border;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5319,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5319,"level");
  if(!enif_get_int(env, argv[2],  &internalFormat)) Badarg(5319,"internalFormat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5319,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5319,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5319,"depth");
  if(!enif_get_int(env, argv[6],  &border)) Badarg(5319,"border");
  if(!enif_get_uint(env, argv[7],  &format)) Badarg(5319,"format");
  if(!enif_get_uint(env, argv[8],  &type)) Badarg(5319,"type");
  if(!egl_get_ptr(env, argv[9], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[9], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5319,"pixels");
  }
  weglTexImage3D(target,level,internalFormat,width,height,depth,border,format,type,pixels_idx);
}

static void ecb_glTexSubImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLenum type;
  ErlNifBinary pixels;
  GLvoid *pixels_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5321,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5321,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5321,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5321,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5321,"zoffset");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5321,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5321,"height");
  if(!enif_get_int(env, argv[7],  &depth)) Badarg(5321,"depth");
  if(!enif_get_uint(env, argv[8],  &format)) Badarg(5321,"format");
  if(!enif_get_uint(env, argv[9],  &type)) Badarg(5321,"type");
  if(!egl_get_ptr(env, argv[10], (void **) &pixels_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[10], &pixels))
        pixels_idx = (GLvoid *) pixels.data;
    else Badarg(5321,"pixels");
  }
  weglTexSubImage3D(target,level,xoffset,yoffset,zoffset,width,height,depth,format,type,pixels_idx);
}

static void ecb_glCopyTexSubImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5323,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5323,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5323,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5323,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5323,"zoffset");
  if(!enif_get_int(env, argv[5],  &x)) Badarg(5323,"x");
  if(!enif_get_int(env, argv[6],  &y)) Badarg(5323,"y");
  if(!enif_get_int(env, argv[7],  &width)) Badarg(5323,"width");
  if(!enif_get_int(env, argv[8],  &height)) Badarg(5323,"height");
  weglCopyTexSubImage3D(target,level,xoffset,yoffset,zoffset,x,y,width,height);
}

static void ecb_glColorTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLenum format;
  GLenum type;
  ErlNifBinary table;
  GLvoid *table_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5324,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5324,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5324,"width");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5324,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5324,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &table_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &table))
        table_idx = (GLvoid *) table.data;
    else Badarg(5324,"table");
  }
  weglColorTable(target,internalformat,width,format,type,table_idx);
}

static void ecb_glColorTableParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5326,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5326,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5326,"params");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, params_t[i1++], &params[0])) Badarg(5326,"params");
     if(!egl_get_float(env, params_t[i1++], &params[1])) Badarg(5326,"params");
     if(!egl_get_float(env, params_t[i1++], &params[2])) Badarg(5326,"params");
     if(!egl_get_float(env, params_t[i1++], &params[3])) Badarg(5326,"params");
   }};
  weglColorTableParameterfv(target,pname,params);
}

static void ecb_glColorTableParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5327,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5327,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5327,"params");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, params_t[i1++], &params[0])) Badarg(5327,"params");
     if(!enif_get_int(env, params_t[i1++], &params[1])) Badarg(5327,"params");
     if(!enif_get_int(env, params_t[i1++], &params[2])) Badarg(5327,"params");
     if(!enif_get_int(env, params_t[i1++], &params[3])) Badarg(5327,"params");
   }};
  weglColorTableParameteriv(target,pname,params);
}

static void ecb_glCopyColorTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5328,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5328,"internalformat");
  if(!enif_get_int(env, argv[2],  &x)) Badarg(5328,"x");
  if(!enif_get_int(env, argv[3],  &y)) Badarg(5328,"y");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5328,"width");
  weglCopyColorTable(target,internalformat,x,y,width);
}

static void ecb_glGetColorTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum format;
  GLenum type;
  ErlNifBinary table;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5329,"target");
  if(!enif_get_uint(env, argv[1],  &format)) Badarg(5329,"format");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5329,"type");
  if(enif_is_binary(env, argv[3]))
    enif_inspect_binary(env, argv[3], &table);
  else if(enif_is_tuple(env, argv[3])) {
    int table_a;
    const ERL_NIF_TERM *table_t;
    if(enif_get_tuple(env, argv[3], &table_a, &table_t) ||
         enif_is_binary(env, table_t[1]))
       enif_inspect_binary(env, table_t[1], &table);
    else Badarg(5329, "table");
  } else Badarg(5329, "table");
  weglGetColorTable(target,format,type,(GLvoid *) table.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetColorTableParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5330,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5330,"pname");
  weglGetColorTableParameterfv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetColorTableParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5331,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5331,"pname");
  weglGetColorTableParameteriv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glColorSubTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei start;
  GLsizei count;
  GLenum format;
  GLenum type;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5332,"target");
  if(!enif_get_int(env, argv[1],  &start)) Badarg(5332,"start");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5332,"count");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5332,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5332,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5332,"data");
  }
  weglColorSubTable(target,start,count,format,type,data_idx);
}

static void ecb_glCopyColorSubTable(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei start;
  GLint x;
  GLint y;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5334,"target");
  if(!enif_get_int(env, argv[1],  &start)) Badarg(5334,"start");
  if(!enif_get_int(env, argv[2],  &x)) Badarg(5334,"x");
  if(!enif_get_int(env, argv[3],  &y)) Badarg(5334,"y");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5334,"width");
  weglCopyColorSubTable(target,start,x,y,width);
}

static void ecb_glConvolutionFilter1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLenum format;
  GLenum type;
  ErlNifBinary image;
  GLvoid *image_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5335,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5335,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5335,"width");
  if(!enif_get_uint(env, argv[3],  &format)) Badarg(5335,"format");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5335,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &image_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &image))
        image_idx = (GLvoid *) image.data;
    else Badarg(5335,"image");
  }
  weglConvolutionFilter1D(target,internalformat,width,format,type,image_idx);
}

static void ecb_glConvolutionFilter2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary image;
  GLvoid *image_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5337,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5337,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5337,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5337,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5337,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5337,"type");
  if(!egl_get_ptr(env, argv[6], (void **) &image_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &image))
        image_idx = (GLvoid *) image.data;
    else Badarg(5337,"image");
  }
  weglConvolutionFilter2D(target,internalformat,width,height,format,type,image_idx);
}

static void ecb_glConvolutionParameterf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5339,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5339,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5339,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5339,"params");
   }};
  weglConvolutionParameterf(target,pname,params);
}

static void ecb_glConvolutionParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5340,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5340,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5340,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5340,"params");
   }};
  weglConvolutionParameterfv(target,pname,params);
}

static void ecb_glConvolutionParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5341,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5341,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5341,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5341,"params");
   }};
  weglConvolutionParameteri(target,pname,params);
}

static void ecb_glConvolutionParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5342,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5342,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5342,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5342,"params");
   }};
  weglConvolutionParameteriv(target,pname,params);
}

static void ecb_glCopyConvolutionFilter1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5343,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5343,"internalformat");
  if(!enif_get_int(env, argv[2],  &x)) Badarg(5343,"x");
  if(!enif_get_int(env, argv[3],  &y)) Badarg(5343,"y");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5343,"width");
  weglCopyConvolutionFilter1D(target,internalformat,x,y,width);
}

static void ecb_glCopyConvolutionFilter2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLint x;
  GLint y;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5344,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5344,"internalformat");
  if(!enif_get_int(env, argv[2],  &x)) Badarg(5344,"x");
  if(!enif_get_int(env, argv[3],  &y)) Badarg(5344,"y");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5344,"width");
  if(!enif_get_int(env, argv[5],  &height)) Badarg(5344,"height");
  weglCopyConvolutionFilter2D(target,internalformat,x,y,width,height);
}

static void ecb_glGetConvolutionFilter(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum format;
  GLenum type;
  ErlNifBinary image;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5345,"target");
  if(!enif_get_uint(env, argv[1],  &format)) Badarg(5345,"format");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5345,"type");
  if(enif_is_binary(env, argv[3]))
    enif_inspect_binary(env, argv[3], &image);
  else if(enif_is_tuple(env, argv[3])) {
    int image_a;
    const ERL_NIF_TERM *image_t;
    if(enif_get_tuple(env, argv[3], &image_a, &image_t) ||
         enif_is_binary(env, image_t[1]))
       enif_inspect_binary(env, image_t[1], &image);
    else Badarg(5345, "image");
  } else Badarg(5345, "image");
  weglGetConvolutionFilter(target,format,type,(GLvoid *) image.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetConvolutionParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5346,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5346,"pname");
  weglGetConvolutionParameterfv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetConvolutionParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5347,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5347,"pname");
  weglGetConvolutionParameteriv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glSeparableFilter2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLenum type;
  ErlNifBinary row;
  GLvoid *row_idx;
  ErlNifBinary column;
  GLvoid *column_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5348,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5348,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5348,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5348,"height");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5348,"format");
  if(!enif_get_uint(env, argv[5],  &type)) Badarg(5348,"type");
  if(!egl_get_ptr(env, argv[6], (void **) &row_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &row))
        row_idx = (GLvoid *) row.data;
    else Badarg(5348,"row");
  }
  if(!egl_get_ptr(env, argv[7], (void **) &column_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[7], &column))
        column_idx = (GLvoid *) column.data;
    else Badarg(5348,"column");
  }
  weglSeparableFilter2D(target,internalformat,width,height,format,type,row_idx,column_idx);
}

static void ecb_glGetHistogram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLboolean reset;
  GLenum format;
  GLenum type;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5350,"target");
  if(!egl_get_ubyte(env, argv[1],  &reset)) Badarg(5350,"reset");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5350,"format");
  if(!enif_get_uint(env, argv[3],  &type)) Badarg(5350,"type");
  if(enif_is_binary(env, argv[4]))
    enif_inspect_binary(env, argv[4], &values);
  else if(enif_is_tuple(env, argv[4])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[4], &values_a, &values_t) ||
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5350, "values");
  } else Badarg(5350, "values");
  weglGetHistogram(target,reset,format,type,(GLvoid *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetHistogramParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5351,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5351,"pname");
  weglGetHistogramParameterfv(target,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_double(env, (double) params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetHistogramParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5352,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5352,"pname");
  weglGetHistogramParameteriv(target,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_int(env, params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetMinmax(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLboolean reset;
  GLenum format;
  GLenum types;
  ErlNifBinary values;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5353,"target");
  if(!egl_get_ubyte(env, argv[1],  &reset)) Badarg(5353,"reset");
  if(!enif_get_uint(env, argv[2],  &format)) Badarg(5353,"format");
  if(!enif_get_uint(env, argv[3],  &types)) Badarg(5353,"types");
  if(enif_is_binary(env, argv[4]))
    enif_inspect_binary(env, argv[4], &values);
  else if(enif_is_tuple(env, argv[4])) {
    int values_a;
    const ERL_NIF_TERM *values_t;
    if(enif_get_tuple(env, argv[4], &values_a, &values_t) ||
         enif_is_binary(env, values_t[1]))
       enif_inspect_binary(env, values_t[1], &values);
    else Badarg(5353, "values");
  } else Badarg(5353, "values");
  weglGetMinmax(target,reset,format,types,(GLvoid *) values.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetMinmaxParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLfloat params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5354,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5354,"pname");
  weglGetMinmaxParameterfv(target,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_double(env, (double) params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetMinmaxParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[1];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5355,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5355,"pname");
  weglGetMinmaxParameteriv(target,pname,params);
  reply =      enif_make_tuple1(env,
     enif_make_int(env, params[0]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glHistogram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei width;
  GLenum internalformat;
  GLboolean sink;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5356,"target");
  if(!enif_get_int(env, argv[1],  &width)) Badarg(5356,"width");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5356,"internalformat");
  if(!egl_get_ubyte(env, argv[3],  &sink)) Badarg(5356,"sink");
  weglHistogram(target,width,internalformat,sink);
}

static void ecb_glMinmax(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLboolean sink;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5357,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5357,"internalformat");
  if(!egl_get_ubyte(env, argv[2],  &sink)) Badarg(5357,"sink");
  weglMinmax(target,internalformat,sink);
}

static void ecb_glResetHistogram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5358,"target");
  weglResetHistogram(target);
}

static void ecb_glResetMinmax(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5359,"target");
  weglResetMinmax(target);
}

static void ecb_glActiveTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum texture;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5360,"texture");
  weglActiveTexture(texture);
}

static void ecb_glSampleCoverage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf value;
  GLboolean invert;
  if(!egl_get_float(env, argv[0],  &value)) Badarg(5361,"value");
  if(!egl_get_ubyte(env, argv[1],  &invert)) Badarg(5361,"invert");
  weglSampleCoverage(value,invert);
}

static void ecb_glCompressedTexImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLint border;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5362,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5362,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5362,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5362,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5362,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5362,"depth");
  if(!enif_get_int(env, argv[6],  &border)) Badarg(5362,"border");
  if(!enif_get_int(env, argv[7],  &imageSize)) Badarg(5362,"imageSize");
  if(!egl_get_ptr(env, argv[8], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5362,"data");
  }
  weglCompressedTexImage3D(target,level,internalformat,width,height,depth,border,imageSize,data_idx);
}

static void ecb_glCompressedTexImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLint border;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5364,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5364,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5364,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5364,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5364,"height");
  if(!enif_get_int(env, argv[5],  &border)) Badarg(5364,"border");
  if(!enif_get_int(env, argv[6],  &imageSize)) Badarg(5364,"imageSize");
  if(!egl_get_ptr(env, argv[7], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[7], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5364,"data");
  }
  weglCompressedTexImage2D(target,level,internalformat,width,height,border,imageSize,data_idx);
}

static void ecb_glCompressedTexImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLenum internalformat;
  GLsizei width;
  GLint border;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5366,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5366,"level");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5366,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5366,"width");
  if(!enif_get_int(env, argv[4],  &border)) Badarg(5366,"border");
  if(!enif_get_int(env, argv[5],  &imageSize)) Badarg(5366,"imageSize");
  if(!egl_get_ptr(env, argv[6], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5366,"data");
  }
  weglCompressedTexImage1D(target,level,internalformat,width,border,imageSize,data_idx);
}

static void ecb_glCompressedTexSubImage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLint zoffset;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5368,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5368,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5368,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5368,"yoffset");
  if(!enif_get_int(env, argv[4],  &zoffset)) Badarg(5368,"zoffset");
  if(!enif_get_int(env, argv[5],  &width)) Badarg(5368,"width");
  if(!enif_get_int(env, argv[6],  &height)) Badarg(5368,"height");
  if(!enif_get_int(env, argv[7],  &depth)) Badarg(5368,"depth");
  if(!enif_get_uint(env, argv[8],  &format)) Badarg(5368,"format");
  if(!enif_get_int(env, argv[9],  &imageSize)) Badarg(5368,"imageSize");
  if(!egl_get_ptr(env, argv[10], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[10], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5368,"data");
  }
  weglCompressedTexSubImage3D(target,level,xoffset,yoffset,zoffset,width,height,depth,format,imageSize,data_idx);
}

static void ecb_glCompressedTexSubImage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLint yoffset;
  GLsizei width;
  GLsizei height;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5370,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5370,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5370,"xoffset");
  if(!enif_get_int(env, argv[3],  &yoffset)) Badarg(5370,"yoffset");
  if(!enif_get_int(env, argv[4],  &width)) Badarg(5370,"width");
  if(!enif_get_int(env, argv[5],  &height)) Badarg(5370,"height");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5370,"format");
  if(!enif_get_int(env, argv[7],  &imageSize)) Badarg(5370,"imageSize");
  if(!egl_get_ptr(env, argv[8], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[8], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5370,"data");
  }
  weglCompressedTexSubImage2D(target,level,xoffset,yoffset,width,height,format,imageSize,data_idx);
}

static void ecb_glCompressedTexSubImage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint level;
  GLint xoffset;
  GLsizei width;
  GLenum format;
  GLsizei imageSize;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5372,"target");
  if(!enif_get_int(env, argv[1],  &level)) Badarg(5372,"level");
  if(!enif_get_int(env, argv[2],  &xoffset)) Badarg(5372,"xoffset");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5372,"width");
  if(!enif_get_uint(env, argv[4],  &format)) Badarg(5372,"format");
  if(!enif_get_int(env, argv[5],  &imageSize)) Badarg(5372,"imageSize");
  if(!egl_get_ptr(env, argv[6], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[6], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5372,"data");
  }
  weglCompressedTexSubImage1D(target,level,xoffset,width,format,imageSize,data_idx);
}

static void ecb_glGetCompressedTexImage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint lod;
  ErlNifBinary img;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5374,"target");
  if(!enif_get_int(env, argv[1],  &lod)) Badarg(5374,"lod");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &img);
  else if(enif_is_tuple(env, argv[2])) {
    int img_a;
    const ERL_NIF_TERM *img_t;
    if(enif_get_tuple(env, argv[2], &img_a, &img_t) ||
         enif_is_binary(env, img_t[1]))
       enif_inspect_binary(env, img_t[1], &img);
    else Badarg(5374, "img");
  } else Badarg(5374, "img");
  weglGetCompressedTexImage(target,lod,(GLvoid *) img.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glClientActiveTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum texture;
  if(!enif_get_uint(env, argv[0],  &texture)) Badarg(5375,"texture");
  weglClientActiveTexture(texture);
}

static void ecb_glMultiTexCoord1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble s;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5376,"target");
  if(!enif_get_double(env, argv[1],  &s)) Badarg(5376,"s");
  weglMultiTexCoord1d(target,s);
}

static void ecb_glMultiTexCoord1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat s;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5377,"target");
  if(!egl_get_float(env, argv[1],  &s)) Badarg(5377,"s");
  weglMultiTexCoord1f(target,s);
}

static void ecb_glMultiTexCoord1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint s;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5378,"target");
  if(!enif_get_int(env, argv[1],  &s)) Badarg(5378,"s");
  weglMultiTexCoord1i(target,s);
}

static void ecb_glMultiTexCoord1s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLshort s;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5379,"target");
  if(!egl_get_short(env, argv[1],  &s)) Badarg(5379,"s");
  weglMultiTexCoord1s(target,s);
}

static void ecb_glMultiTexCoord2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble s;
  GLdouble t;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5380,"target");
  if(!enif_get_double(env, argv[1],  &s)) Badarg(5380,"s");
  if(!enif_get_double(env, argv[2],  &t)) Badarg(5380,"t");
  weglMultiTexCoord2d(target,s,t);
}

static void ecb_glMultiTexCoord2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat s;
  GLfloat t;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5381,"target");
  if(!egl_get_float(env, argv[1],  &s)) Badarg(5381,"s");
  if(!egl_get_float(env, argv[2],  &t)) Badarg(5381,"t");
  weglMultiTexCoord2f(target,s,t);
}

static void ecb_glMultiTexCoord2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint s;
  GLint t;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5382,"target");
  if(!enif_get_int(env, argv[1],  &s)) Badarg(5382,"s");
  if(!enif_get_int(env, argv[2],  &t)) Badarg(5382,"t");
  weglMultiTexCoord2i(target,s,t);
}

static void ecb_glMultiTexCoord2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLshort s;
  GLshort t;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5383,"target");
  if(!egl_get_short(env, argv[1],  &s)) Badarg(5383,"s");
  if(!egl_get_short(env, argv[2],  &t)) Badarg(5383,"t");
  weglMultiTexCoord2s(target,s,t);
}

static void ecb_glMultiTexCoord3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble s;
  GLdouble t;
  GLdouble r;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5384,"target");
  if(!enif_get_double(env, argv[1],  &s)) Badarg(5384,"s");
  if(!enif_get_double(env, argv[2],  &t)) Badarg(5384,"t");
  if(!enif_get_double(env, argv[3],  &r)) Badarg(5384,"r");
  weglMultiTexCoord3d(target,s,t,r);
}

static void ecb_glMultiTexCoord3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat s;
  GLfloat t;
  GLfloat r;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5385,"target");
  if(!egl_get_float(env, argv[1],  &s)) Badarg(5385,"s");
  if(!egl_get_float(env, argv[2],  &t)) Badarg(5385,"t");
  if(!egl_get_float(env, argv[3],  &r)) Badarg(5385,"r");
  weglMultiTexCoord3f(target,s,t,r);
}

static void ecb_glMultiTexCoord3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint s;
  GLint t;
  GLint r;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5386,"target");
  if(!enif_get_int(env, argv[1],  &s)) Badarg(5386,"s");
  if(!enif_get_int(env, argv[2],  &t)) Badarg(5386,"t");
  if(!enif_get_int(env, argv[3],  &r)) Badarg(5386,"r");
  weglMultiTexCoord3i(target,s,t,r);
}

static void ecb_glMultiTexCoord3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLshort s;
  GLshort t;
  GLshort r;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5387,"target");
  if(!egl_get_short(env, argv[1],  &s)) Badarg(5387,"s");
  if(!egl_get_short(env, argv[2],  &t)) Badarg(5387,"t");
  if(!egl_get_short(env, argv[3],  &r)) Badarg(5387,"r");
  weglMultiTexCoord3s(target,s,t,r);
}

static void ecb_glMultiTexCoord4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLdouble s;
  GLdouble t;
  GLdouble r;
  GLdouble q;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5388,"target");
  if(!enif_get_double(env, argv[1],  &s)) Badarg(5388,"s");
  if(!enif_get_double(env, argv[2],  &t)) Badarg(5388,"t");
  if(!enif_get_double(env, argv[3],  &r)) Badarg(5388,"r");
  if(!enif_get_double(env, argv[4],  &q)) Badarg(5388,"q");
  weglMultiTexCoord4d(target,s,t,r,q);
}

static void ecb_glMultiTexCoord4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLfloat s;
  GLfloat t;
  GLfloat r;
  GLfloat q;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5389,"target");
  if(!egl_get_float(env, argv[1],  &s)) Badarg(5389,"s");
  if(!egl_get_float(env, argv[2],  &t)) Badarg(5389,"t");
  if(!egl_get_float(env, argv[3],  &r)) Badarg(5389,"r");
  if(!egl_get_float(env, argv[4],  &q)) Badarg(5389,"q");
  weglMultiTexCoord4f(target,s,t,r,q);
}

static void ecb_glMultiTexCoord4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLint s;
  GLint t;
  GLint r;
  GLint q;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5390,"target");
  if(!enif_get_int(env, argv[1],  &s)) Badarg(5390,"s");
  if(!enif_get_int(env, argv[2],  &t)) Badarg(5390,"t");
  if(!enif_get_int(env, argv[3],  &r)) Badarg(5390,"r");
  if(!enif_get_int(env, argv[4],  &q)) Badarg(5390,"q");
  weglMultiTexCoord4i(target,s,t,r,q);
}

static void ecb_glMultiTexCoord4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLshort s;
  GLshort t;
  GLshort r;
  GLshort q;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5391,"target");
  if(!egl_get_short(env, argv[1],  &s)) Badarg(5391,"s");
  if(!egl_get_short(env, argv[2],  &t)) Badarg(5391,"t");
  if(!egl_get_short(env, argv[3],  &r)) Badarg(5391,"r");
  if(!egl_get_short(env, argv[4],  &q)) Badarg(5391,"q");
  weglMultiTexCoord4s(target,s,t,r,q);
}

static void ecb_glLoadTransposeMatrixf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5392,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5392,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5392,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5392,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5392,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5392,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5392,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadTransposeMatrixf(m);
}

static void ecb_glLoadTransposeMatrixd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5393,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5393,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5393,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5393,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5393,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5393,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5393,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadTransposeMatrixd(m);
}

static void ecb_glMultTransposeMatrixf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5394,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5394,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5394,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5394,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5394,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5394,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5394,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultTransposeMatrixf(m);
}

static void ecb_glMultTransposeMatrixd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5395,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5395,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5395,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5395,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5395,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5395,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5395,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultTransposeMatrixd(m);
}

static void ecb_glBlendFuncSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum sfactorRGB;
  GLenum dfactorRGB;
  GLenum sfactorAlpha;
  GLenum dfactorAlpha;
  if(!enif_get_uint(env, argv[0],  &sfactorRGB)) Badarg(5396,"sfactorRGB");
  if(!enif_get_uint(env, argv[1],  &dfactorRGB)) Badarg(5396,"dfactorRGB");
  if(!enif_get_uint(env, argv[2],  &sfactorAlpha)) Badarg(5396,"sfactorAlpha");
  if(!enif_get_uint(env, argv[3],  &dfactorAlpha)) Badarg(5396,"dfactorAlpha");
  weglBlendFuncSeparate(sfactorRGB,dfactorRGB,sfactorAlpha,dfactorAlpha);
}

static void ecb_glMultiDrawArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  ErlNifBinary first_bin;
  int first_free = 0;
  unsigned int first_len;
  GLint *first;
  ErlNifBinary count_bin;
  int count_free = 0;
  unsigned int count_len;
  GLsizei *count;
  GLsizei primcount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5397,"mode");
  if(!enif_is_list(env, argv[1])) {
    if(!enif_is_binary(env, argv[1])) Badarg(5397, "first");
    enif_inspect_binary(env, argv[1], &first_bin);
    first = (GLint *) first_bin.data;
    first_len = first_bin.size / sizeof(GLint);
  }  else {
    int i=0;
    ERL_NIF_TERM first_l, first_h, first_t;
    enif_get_list_length(env, argv[1], &first_len);
    first_free = 1;
    first = (GLint *) enif_alloc(first_len * sizeof(GLint));
    first_l = argv[1];
    while(enif_get_list_cell(env, first_l, &first_h, &first_t)) {
        if(!enif_get_int(env, first_h, &first[i++])) Badarg(5397,"first");
        first_l = first_t;
    };
  }
  if(!enif_is_list(env, argv[2])) {
    if(!enif_is_binary(env, argv[2])) Badarg(5397, "count");
    enif_inspect_binary(env, argv[2], &count_bin);
    count = (GLsizei *) count_bin.data;
    count_len = count_bin.size / sizeof(GLsizei);
  }  else {
    int i=0;
    ERL_NIF_TERM count_l, count_h, count_t;
    enif_get_list_length(env, argv[2], &count_len);
    count_free = 1;
    count = (GLsizei *) enif_alloc(count_len * sizeof(GLsizei));
    count_l = argv[2];
    while(enif_get_list_cell(env, count_l, &count_h, &count_t)) {
        if(!enif_get_int(env, count_h, &count[i++])) Badarg(5397,"count");
        count_l = count_t;
    };
  }
 primcount = (GLsizei) count_len;
  weglMultiDrawArrays(mode,first,count,primcount);
 if(count_free) enif_free(count);
 if(first_free) enif_free(first);
}

static void ecb_glPointParameterf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5399,"pname");
  if(!egl_get_float(env, argv[1],  &param)) Badarg(5399,"param");
  weglPointParameterf(pname,param);
}

static void ecb_glPointParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5400,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5400,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!egl_get_float(env, params_t[i], &params[i])) Badarg(5400,"params");
   }};
  weglPointParameterfv(pname,params);
}

static void ecb_glPointParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5401,"pname");
  if(!enif_get_int(env, argv[1],  &param)) Badarg(5401,"param");
  weglPointParameteri(pname,param);
}

static void ecb_glPointParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5402,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[1], &params_a, &params_t)) {
     Badarg(5402,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5402,"params");
   }};
  weglPointParameteriv(pname,params);
}

static void ecb_glFogCoordf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat coord;
  if(!egl_get_float(env, argv[0],  &coord)) Badarg(5403,"coord");
  weglFogCoordf(coord);
}

static void ecb_glFogCoordd(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble coord;
  if(!enif_get_double(env, argv[0],  &coord)) Badarg(5404,"coord");
  weglFogCoordd(coord);
}

static void ecb_glFogCoordPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum type;
  GLsizei stride;
  ErlNifBinary pointer;
  GLvoid *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5405,"type");
  if(!enif_get_int(env, argv[1],  &stride)) Badarg(5405,"stride");
  if(!egl_get_ptr(env, argv[2], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &pointer))
        pointer_idx = (GLvoid *) pointer.data;
    else Badarg(5405,"pointer");
  }
  weglFogCoordPointer(type,stride,pointer_idx);
}

static void ecb_glSecondaryColor3b(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbyte red;
  GLbyte green;
  GLbyte blue;
  if(!egl_get_byte(env, argv[0],  &red)) Badarg(5407,"red");
  if(!egl_get_byte(env, argv[1],  &green)) Badarg(5407,"green");
  if(!egl_get_byte(env, argv[2],  &blue)) Badarg(5407,"blue");
  weglSecondaryColor3b(red,green,blue);
}

static void ecb_glSecondaryColor3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble red;
  GLdouble green;
  GLdouble blue;
  if(!enif_get_double(env, argv[0],  &red)) Badarg(5408,"red");
  if(!enif_get_double(env, argv[1],  &green)) Badarg(5408,"green");
  if(!enif_get_double(env, argv[2],  &blue)) Badarg(5408,"blue");
  weglSecondaryColor3d(red,green,blue);
}

static void ecb_glSecondaryColor3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat red;
  GLfloat green;
  GLfloat blue;
  if(!egl_get_float(env, argv[0],  &red)) Badarg(5409,"red");
  if(!egl_get_float(env, argv[1],  &green)) Badarg(5409,"green");
  if(!egl_get_float(env, argv[2],  &blue)) Badarg(5409,"blue");
  weglSecondaryColor3f(red,green,blue);
}

static void ecb_glSecondaryColor3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint red;
  GLint green;
  GLint blue;
  if(!enif_get_int(env, argv[0],  &red)) Badarg(5410,"red");
  if(!enif_get_int(env, argv[1],  &green)) Badarg(5410,"green");
  if(!enif_get_int(env, argv[2],  &blue)) Badarg(5410,"blue");
  weglSecondaryColor3i(red,green,blue);
}

static void ecb_glSecondaryColor3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort red;
  GLshort green;
  GLshort blue;
  if(!egl_get_short(env, argv[0],  &red)) Badarg(5411,"red");
  if(!egl_get_short(env, argv[1],  &green)) Badarg(5411,"green");
  if(!egl_get_short(env, argv[2],  &blue)) Badarg(5411,"blue");
  weglSecondaryColor3s(red,green,blue);
}

static void ecb_glSecondaryColor3ub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLubyte red;
  GLubyte green;
  GLubyte blue;
  if(!egl_get_ubyte(env, argv[0],  &red)) Badarg(5412,"red");
  if(!egl_get_ubyte(env, argv[1],  &green)) Badarg(5412,"green");
  if(!egl_get_ubyte(env, argv[2],  &blue)) Badarg(5412,"blue");
  weglSecondaryColor3ub(red,green,blue);
}

static void ecb_glSecondaryColor3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint red;
  GLuint green;
  GLuint blue;
  if(!enif_get_uint(env, argv[0],  &red)) Badarg(5413,"red");
  if(!enif_get_uint(env, argv[1],  &green)) Badarg(5413,"green");
  if(!enif_get_uint(env, argv[2],  &blue)) Badarg(5413,"blue");
  weglSecondaryColor3ui(red,green,blue);
}

static void ecb_glSecondaryColor3us(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLushort red;
  GLushort green;
  GLushort blue;
  if(!egl_get_ushort(env, argv[0],  &red)) Badarg(5414,"red");
  if(!egl_get_ushort(env, argv[1],  &green)) Badarg(5414,"green");
  if(!egl_get_ushort(env, argv[2],  &blue)) Badarg(5414,"blue");
  weglSecondaryColor3us(red,green,blue);
}

static void ecb_glSecondaryColorPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary pointer;
  GLvoid *pointer_idx;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5415,"size");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5415,"type");
  if(!enif_get_int(env, argv[2],  &stride)) Badarg(5415,"stride");
  if(!egl_get_ptr(env, argv[3], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &pointer))
        pointer_idx = (GLvoid *) pointer.data;
    else Badarg(5415,"pointer");
  }
  weglSecondaryColorPointer(size,type,stride,pointer_idx);
}

static void ecb_glWindowPos2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5417,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5417,"y");
  weglWindowPos2d(x,y);
}

static void ecb_glWindowPos2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5418,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5418,"y");
  weglWindowPos2f(x,y);
}

static void ecb_glWindowPos2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5419,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5419,"y");
  weglWindowPos2i(x,y);
}

static void ecb_glWindowPos2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5420,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5420,"y");
  weglWindowPos2s(x,y);
}

static void ecb_glWindowPos3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_double(env, argv[0],  &x)) Badarg(5421,"x");
  if(!enif_get_double(env, argv[1],  &y)) Badarg(5421,"y");
  if(!enif_get_double(env, argv[2],  &z)) Badarg(5421,"z");
  weglWindowPos3d(x,y,z);
}

static void ecb_glWindowPos3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!egl_get_float(env, argv[0],  &x)) Badarg(5422,"x");
  if(!egl_get_float(env, argv[1],  &y)) Badarg(5422,"y");
  if(!egl_get_float(env, argv[2],  &z)) Badarg(5422,"z");
  weglWindowPos3f(x,y,z);
}

static void ecb_glWindowPos3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint x;
  GLint y;
  GLint z;
  if(!enif_get_int(env, argv[0],  &x)) Badarg(5423,"x");
  if(!enif_get_int(env, argv[1],  &y)) Badarg(5423,"y");
  if(!enif_get_int(env, argv[2],  &z)) Badarg(5423,"z");
  weglWindowPos3i(x,y,z);
}

static void ecb_glWindowPos3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLshort x;
  GLshort y;
  GLshort z;
  if(!egl_get_short(env, argv[0],  &x)) Badarg(5424,"x");
  if(!egl_get_short(env, argv[1],  &y)) Badarg(5424,"y");
  if(!egl_get_short(env, argv[2],  &z)) Badarg(5424,"z");
  weglWindowPos3s(x,y,z);
}

static void ecb_glGenQueries(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * ids;
  ERL_NIF_TERM *ids_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5425,"n");
  ids = enif_alloc((int)n*sizeof(GLuint));
  ids_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenQueries(n,ids);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      ids_ts[ri] =      enif_make_int(env, ids[ri]);}
  reply =      enif_make_list_from_array(env, ids_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(ids_ts);
 enif_free(ids);
}

static void ecb_glDeleteQueries(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int ids_free = 0;
  GLuint *ids;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5426,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5426, "ids"); }
  else {
    int i=0;
    ERL_NIF_TERM ids_l, ids_h, ids_t;
    ids_free = 1;
    ids = (GLuint *) enif_alloc(n * sizeof(GLuint));
    ids_l = argv[1];
    while(enif_get_list_cell(env, ids_l, &ids_h, &ids_t)) {
        if(!enif_get_uint(env, ids_h, &ids[i++])) Badarg(5426,"ids");
        ids_l = ids_t;
    };
  }
  weglDeleteQueries(n,ids);
 if(ids_free) enif_free(ids);
}

static void ecb_glIsQuery(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5427,"id");
  result = weglIsQuery(id);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBeginQuery(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5428,"target");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5428,"id");
  weglBeginQuery(target,id);
}

static void ecb_glEndQuery(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5429,"target");
  weglEndQuery(target);
}

static void ecb_glGetQueryiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5430,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5430,"pname");
  weglGetQueryiv(target,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetQueryObjectiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint id;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5431,"id");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5431,"pname");
  weglGetQueryObjectiv(id,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetQueryObjectuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint id;
  GLenum pname;
  GLuint params;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5432,"id");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5432,"pname");
  weglGetQueryObjectuiv(id,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBindBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5433,"target");
  if(!enif_get_uint(env, argv[1],  &buffer)) Badarg(5433,"buffer");
  weglBindBuffer(target,buffer);
}

static void ecb_glDeleteBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int buffers_free = 0;
  GLuint *buffers;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5434,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5434, "buffers"); }
  else {
    int i=0;
    ERL_NIF_TERM buffers_l, buffers_h, buffers_t;
    buffers_free = 1;
    buffers = (GLuint *) enif_alloc(n * sizeof(GLuint));
    buffers_l = argv[1];
    while(enif_get_list_cell(env, buffers_l, &buffers_h, &buffers_t)) {
        if(!enif_get_uint(env, buffers_h, &buffers[i++])) Badarg(5434,"buffers");
        buffers_l = buffers_t;
    };
  }
  weglDeleteBuffers(n,buffers);
 if(buffers_free) enif_free(buffers);
}

static void ecb_glGenBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * buffers;
  ERL_NIF_TERM *buffers_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5435,"n");
  buffers = enif_alloc((int)n*sizeof(GLuint));
  buffers_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenBuffers(n,buffers);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      buffers_ts[ri] =      enif_make_int(env, buffers[ri]);}
  reply =      enif_make_list_from_array(env, buffers_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(buffers_ts);
 enif_free(buffers);
}

static void ecb_glIsBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5436,"buffer");
  result = weglIsBuffer(buffer);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBufferData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizeiptr size;
  ErlNifBinary data;
  GLvoid *data_idx;
  GLenum usage;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5437,"target");
  if(!enif_get_int64(env, argv[1],  &size)) Badarg(5437,"size");
  if(!egl_get_ptr(env, argv[2], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5437,"data");
  }
  if(!enif_get_uint(env, argv[3],  &usage)) Badarg(5437,"usage");
  weglBufferData(target,size,data_idx,usage);
}

static void ecb_glBufferSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLintptr offset;
  GLsizeiptr size;
  ErlNifBinary data;
  GLvoid *data_idx;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5439,"target");
  if(!enif_get_int64(env, argv[1],  &offset)) Badarg(5439,"offset");
  if(!enif_get_int64(env, argv[2],  &size)) Badarg(5439,"size");
  if(!egl_get_ptr(env, argv[3], (void **) &data_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &data))
        data_idx = (GLvoid *) data.data;
    else Badarg(5439,"data");
  }
  weglBufferSubData(target,offset,size,data_idx);
}

static void ecb_glGetBufferSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLintptr offset;
  GLsizeiptr size;
  ErlNifBinary data;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5441,"target");
  if(!enif_get_int64(env, argv[1],  &offset)) Badarg(5441,"offset");
  if(!enif_get_int64(env, argv[2],  &size)) Badarg(5441,"size");
  if(enif_is_binary(env, argv[3]))
    enif_inspect_binary(env, argv[3], &data);
  else if(enif_is_tuple(env, argv[3])) {
    int data_a;
    const ERL_NIF_TERM *data_t;
    if(enif_get_tuple(env, argv[3], &data_a, &data_t) ||
         enif_is_binary(env, data_t[1]))
       enif_inspect_binary(env, data_t[1], &data);
    else Badarg(5441, "data");
  } else Badarg(5441, "data");
  weglGetBufferSubData(target,offset,size,(GLvoid *) data.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetBufferParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5442,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5442,"pname");
  weglGetBufferParameteriv(target,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBlendEquationSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum modeRGB;
  GLenum modeAlpha;
  if(!enif_get_uint(env, argv[0],  &modeRGB)) Badarg(5443,"modeRGB");
  if(!enif_get_uint(env, argv[1],  &modeAlpha)) Badarg(5443,"modeAlpha");
  weglBlendEquationSeparate(modeRGB,modeAlpha);
}

static void ecb_glDrawBuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int bufs_free = 0;
  GLenum *bufs;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5444,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5444, "bufs"); }
  else {
    int i=0;
    ERL_NIF_TERM bufs_l, bufs_h, bufs_t;
    bufs_free = 1;
    bufs = (GLenum *) enif_alloc(n * sizeof(GLenum));
    bufs_l = argv[1];
    while(enif_get_list_cell(env, bufs_l, &bufs_h, &bufs_t)) {
        if(!enif_get_uint(env, bufs_h, &bufs[i++])) Badarg(5444,"bufs");
        bufs_l = bufs_t;
    };
  }
  weglDrawBuffers(n,bufs);
 if(bufs_free) enif_free(bufs);
}

static void ecb_glStencilOpSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum sfail;
  GLenum dpfail;
  GLenum dppass;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5445,"face");
  if(!enif_get_uint(env, argv[1],  &sfail)) Badarg(5445,"sfail");
  if(!enif_get_uint(env, argv[2],  &dpfail)) Badarg(5445,"dpfail");
  if(!enif_get_uint(env, argv[3],  &dppass)) Badarg(5445,"dppass");
  weglStencilOpSeparate(face,sfail,dpfail,dppass);
}

static void ecb_glStencilFuncSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLenum func;
  GLint ref;
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5446,"face");
  if(!enif_get_uint(env, argv[1],  &func)) Badarg(5446,"func");
  if(!enif_get_int(env, argv[2],  &ref)) Badarg(5446,"ref");
  if(!enif_get_uint(env, argv[3],  &mask)) Badarg(5446,"mask");
  weglStencilFuncSeparate(face,func,ref,mask);
}

static void ecb_glStencilMaskSeparate(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum face;
  GLuint mask;
  if(!enif_get_uint(env, argv[0],  &face)) Badarg(5447,"face");
  if(!enif_get_uint(env, argv[1],  &mask)) Badarg(5447,"mask");
  weglStencilMaskSeparate(face,mask);
}

static void ecb_glAttachShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5448,"program");
  if(!enif_get_uint(env, argv[1],  &shader)) Badarg(5448,"shader");
  weglAttachShader(program,shader);
}

static void ecb_glBindAttribLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint index;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5449,"program");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5449,"index");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5449,"name");
  weglBindAttribLocation(program,index,(GLchar *) name.data);
}

static void ecb_glCompileShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5450,"shader");
  weglCompileShader(shader);
}

static void ecb_glCreateProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  result = weglCreateProgram();
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glCreateShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLenum type;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5452,"type");
  result = weglCreateShader(type);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDeleteProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5453,"program");
  weglDeleteProgram(program);
}

static void ecb_glDeleteShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5454,"shader");
  weglDeleteShader(shader);
}

static void ecb_glDetachShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5455,"program");
  if(!enif_get_uint(env, argv[1],  &shader)) Badarg(5455,"shader");
  weglDetachShader(program,shader);
}

static void ecb_glDisableVertexAttribArray(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5456,"index");
  weglDisableVertexAttribArray(index);
}

static void ecb_glEnableVertexAttribArray(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5457,"index");
  weglEnableVertexAttribArray(index);
}

static void ecb_glGetActiveAttrib(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint index;
  GLsizei bufSize;
  GLsizei length;
  GLint size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5458,"program");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5458,"index");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5458,"bufSize");
  name = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetActiveAttrib(program,index,bufSize,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

static void ecb_glGetActiveUniform(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint index;
  GLsizei bufSize;
  GLsizei length;
  GLint size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5459,"program");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5459,"index");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5459,"bufSize");
  name = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetActiveUniform(program,index,bufSize,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

static void ecb_glGetAttachedShaders(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei maxCount;
  GLsizei count;
  GLuint * obj;
  ERL_NIF_TERM *obj_ts;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5460,"program");
  if(!enif_get_int(env, argv[1],  &maxCount)) Badarg(5460,"maxCount");
  obj = enif_alloc((int)maxCount*sizeof(GLuint));
  obj_ts = (ERL_NIF_TERM *) enif_alloc((int)maxCount*sizeof(ERL_NIF_TERM));
  weglGetAttachedShaders(program,maxCount,&count,obj);
  { int ri;
    for(ri=0; ri < (int) count; ri++)
      obj_ts[ri] =      enif_make_int(env, obj[ri]);}
  reply =      enif_make_list_from_array(env, obj_ts, count);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(obj_ts);
 enif_free(obj);
}

static void ecb_glGetAttribLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5461,"program");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5461,"name");
  result = weglGetAttribLocation(program,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetProgramiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5462,"program");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5462,"pname");
  weglGetProgramiv(program,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetProgramInfoLog(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *infoLog;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5463,"program");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5463,"bufSize");
  infoLog = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetProgramInfoLog(program,bufSize,&length,(GLchar *) infoLog);
  reply =      enif_make_string(env, (const char *) infoLog, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(infoLog);
}

static void ecb_glGetShaderiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint shader;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5464,"shader");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5464,"pname");
  weglGetShaderiv(shader,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetShaderInfoLog(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint shader;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *infoLog;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5465,"shader");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5465,"bufSize");
  infoLog = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetShaderInfoLog(shader,bufSize,&length,(GLchar *) infoLog);
  reply =      enif_make_string(env, (const char *) infoLog, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(infoLog);
}

static void ecb_glGetShaderSource(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint shader;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *source;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5466,"shader");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5466,"bufSize");
  source = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetShaderSource(shader,bufSize,&length,(GLchar *) source);
  reply =      enif_make_string(env, (const char *) source, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(source);
}

static void ecb_glGetUniformLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5467,"program");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5467,"name");
  result = weglGetUniformLocation(program,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetUniformfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLfloat params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5468,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5468,"location");
  weglGetUniformfv(program,location,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_double(env, (double) params[ri]);}
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetUniformiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5469,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5469,"location");
  weglGetUniformiv(program,location,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetVertexAttribdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5470,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5470,"pname");
  weglGetVertexAttribdv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetVertexAttribfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5471,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5471,"pname");
  weglGetVertexAttribfv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetVertexAttribiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5472,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5472,"pname");
  weglGetVertexAttribiv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glIsProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5473,"program");
  result = weglIsProgram(program);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glIsShader(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint shader;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5474,"shader");
  result = weglIsShader(shader);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glLinkProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5475,"program");
  weglLinkProgram(program);
}

static void ecb_glShaderSource(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint shader;
  GLsizei count;
  GLchar ** string;
  ErlNifBinary string_all;
  GLchar *string_ptr;
  int i;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5476,"shader");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5476,"count");
  if(!enif_inspect_binary(env, argv[2], &string_all)) Badarg(5476,"string");
  string = (GLchar **) enif_alloc(sizeof(GLchar *) * (int) argv[1]);
  string_ptr = (GLchar *) string_all.data;
  for(i=0; i < (int) argv[1]; i++) {     string[i] = (GLchar *) string_ptr;
     string_ptr += 1+strlen(string_ptr);
  }
  weglShaderSource(shader,count,(const GLchar **) string,NULL);
 enif_free(string);
}

static void ecb_glUseProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5477,"program");
  weglUseProgram(program);
}

static void ecb_glUniform1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLfloat v0;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5478,"location");
  if(!egl_get_float(env, argv[1],  &v0)) Badarg(5478,"v0");
  weglUniform1f(location,v0);
}

static void ecb_glUniform2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLfloat v0;
  GLfloat v1;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5479,"location");
  if(!egl_get_float(env, argv[1],  &v0)) Badarg(5479,"v0");
  if(!egl_get_float(env, argv[2],  &v1)) Badarg(5479,"v1");
  weglUniform2f(location,v0,v1);
}

static void ecb_glUniform3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLfloat v0;
  GLfloat v1;
  GLfloat v2;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5480,"location");
  if(!egl_get_float(env, argv[1],  &v0)) Badarg(5480,"v0");
  if(!egl_get_float(env, argv[2],  &v1)) Badarg(5480,"v1");
  if(!egl_get_float(env, argv[3],  &v2)) Badarg(5480,"v2");
  weglUniform3f(location,v0,v1,v2);
}

static void ecb_glUniform4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLfloat v0;
  GLfloat v1;
  GLfloat v2;
  GLfloat v3;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5481,"location");
  if(!egl_get_float(env, argv[1],  &v0)) Badarg(5481,"v0");
  if(!egl_get_float(env, argv[2],  &v1)) Badarg(5481,"v1");
  if(!egl_get_float(env, argv[3],  &v2)) Badarg(5481,"v2");
  if(!egl_get_float(env, argv[4],  &v3)) Badarg(5481,"v3");
  weglUniform4f(location,v0,v1,v2,v3);
}

static void ecb_glUniform1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint v0;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5482,"location");
  if(!enif_get_int(env, argv[1],  &v0)) Badarg(5482,"v0");
  weglUniform1i(location,v0);
}

static void ecb_glUniform2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint v0;
  GLint v1;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5483,"location");
  if(!enif_get_int(env, argv[1],  &v0)) Badarg(5483,"v0");
  if(!enif_get_int(env, argv[2],  &v1)) Badarg(5483,"v1");
  weglUniform2i(location,v0,v1);
}

static void ecb_glUniform3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint v0;
  GLint v1;
  GLint v2;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5484,"location");
  if(!enif_get_int(env, argv[1],  &v0)) Badarg(5484,"v0");
  if(!enif_get_int(env, argv[2],  &v1)) Badarg(5484,"v1");
  if(!enif_get_int(env, argv[3],  &v2)) Badarg(5484,"v2");
  weglUniform3i(location,v0,v1,v2);
}

static void ecb_glUniform4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLint v0;
  GLint v1;
  GLint v2;
  GLint v3;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5485,"location");
  if(!enif_get_int(env, argv[1],  &v0)) Badarg(5485,"v0");
  if(!enif_get_int(env, argv[2],  &v1)) Badarg(5485,"v1");
  if(!enif_get_int(env, argv[3],  &v2)) Badarg(5485,"v2");
  if(!enif_get_int(env, argv[4],  &v3)) Badarg(5485,"v3");
  weglUniform4i(location,v0,v1,v2,v3);
}

static void ecb_glUniform1fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  int value_free = 0;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5486,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5486,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5486, "value"); }
  else {
    int i=0;
    ERL_NIF_TERM value_l, value_h, value_t;
    value_free = 1;
    value = (GLfloat *) enif_alloc(count * sizeof(GLfloat));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!egl_get_float(env, value_h, &value[i++])) Badarg(5486,"value");
        value_l = value_t;
    };
  }
  weglUniform1fv(location,count,value);
 if(value_free) enif_free(value);
}

static void ecb_glUniform2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5487,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5487,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5487,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(2*count*sizeof(GLfloat));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) {
            Badarg(5487,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5487,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5487,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform2fv(location,count,value);
}

static void ecb_glUniform3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5488,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5488,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5488,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(3*count*sizeof(GLfloat));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) {
            Badarg(5488,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5488,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5488,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5488,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform3fv(location,count,value);
}

static void ecb_glUniform4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5489,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5489,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5489,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(4*count*sizeof(GLfloat));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5489,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5489,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5489,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5489,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5489,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform4fv(location,count,value);
}

static void ecb_glUniform1iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  int value_free = 0;
  GLint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5490,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5490,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5490, "value"); }
  else {
    int i=0;
    ERL_NIF_TERM value_l, value_h, value_t;
    value_free = 1;
    value = (GLint *) enif_alloc(count * sizeof(GLint));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_int(env, value_h, &value[i++])) Badarg(5490,"value");
        value_l = value_t;
    };
  }
  weglUniform1iv(location,count,value);
 if(value_free) enif_free(value);
}

static void ecb_glUniform2iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5491,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5491,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5491,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLint *) enif_alloc(2*count*sizeof(GLint));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) {
            Badarg(5491,"value");
        } else {
        if(!enif_get_int(env, value_tpl[0], &value[0])) Badarg(5491,"value");
        if(!enif_get_int(env, value_tpl[1], &value[1])) Badarg(5491,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform2iv(location,count,value);
}

static void ecb_glUniform3iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5492,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5492,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5492,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLint *) enif_alloc(3*count*sizeof(GLint));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) {
            Badarg(5492,"value");
        } else {
        if(!enif_get_int(env, value_tpl[0], &value[0])) Badarg(5492,"value");
        if(!enif_get_int(env, value_tpl[1], &value[1])) Badarg(5492,"value");
        if(!enif_get_int(env, value_tpl[2], &value[2])) Badarg(5492,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform3iv(location,count,value);
}

static void ecb_glUniform4iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5493,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5493,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5493,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLint *) enif_alloc(4*count*sizeof(GLint));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5493,"value");
        } else {
        if(!enif_get_int(env, value_tpl[0], &value[0])) Badarg(5493,"value");
        if(!enif_get_int(env, value_tpl[1], &value[1])) Badarg(5493,"value");
        if(!enif_get_int(env, value_tpl[2], &value[2])) Badarg(5493,"value");
        if(!enif_get_int(env, value_tpl[3], &value[3])) Badarg(5493,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform4iv(location,count,value);
}

static void ecb_glUniformMatrix2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5494,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5494,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5494,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5494,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(4*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5494,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5494,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5494,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5494,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5494,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix2fv(location,count,transpose,value);
}

static void ecb_glUniformMatrix3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5495,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5495,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5495,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5495,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(9*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 9) {
            Badarg(5495,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5495,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5495,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5495,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5495,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5495,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5495,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5495,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5495,"value");
        if(!egl_get_float(env, value_tpl[8], &value[8])) Badarg(5495,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix3fv(location,count,transpose,value);
}

static void ecb_glUniformMatrix4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5496,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5496,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5496,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5496,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(16*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 16) {
            Badarg(5496,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[8], &value[8])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[9], &value[9])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[10], &value[10])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[11], &value[11])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[12], &value[12])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[13], &value[13])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[14], &value[14])) Badarg(5496,"value");
        if(!egl_get_float(env, value_tpl[15], &value[15])) Badarg(5496,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix4fv(location,count,transpose,value);
}

static void ecb_glValidateProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5497,"program");
  weglValidateProgram(program);
}

static void ecb_glVertexAttrib1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5498,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5498,"x");
  weglVertexAttrib1d(index,x);
}

static void ecb_glVertexAttrib1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5499,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5499,"x");
  weglVertexAttrib1f(index,x);
}

static void ecb_glVertexAttrib1s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5500,"index");
  if(!egl_get_short(env, argv[1],  &x)) Badarg(5500,"x");
  weglVertexAttrib1s(index,x);
}

static void ecb_glVertexAttrib2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5501,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5501,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5501,"y");
  weglVertexAttrib2d(index,x,y);
}

static void ecb_glVertexAttrib2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  GLfloat y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5502,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5502,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5502,"y");
  weglVertexAttrib2f(index,x,y);
}

static void ecb_glVertexAttrib2s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort x;
  GLshort y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5503,"index");
  if(!egl_get_short(env, argv[1],  &x)) Badarg(5503,"x");
  if(!egl_get_short(env, argv[2],  &y)) Badarg(5503,"y");
  weglVertexAttrib2s(index,x,y);
}

static void ecb_glVertexAttrib3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5504,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5504,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5504,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5504,"z");
  weglVertexAttrib3d(index,x,y,z);
}

static void ecb_glVertexAttrib3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5505,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5505,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5505,"y");
  if(!egl_get_float(env, argv[3],  &z)) Badarg(5505,"z");
  weglVertexAttrib3f(index,x,y,z);
}

static void ecb_glVertexAttrib3s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort x;
  GLshort y;
  GLshort z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5506,"index");
  if(!egl_get_short(env, argv[1],  &x)) Badarg(5506,"x");
  if(!egl_get_short(env, argv[2],  &y)) Badarg(5506,"y");
  if(!egl_get_short(env, argv[3],  &z)) Badarg(5506,"z");
  weglVertexAttrib3s(index,x,y,z);
}

static void ecb_glVertexAttrib4Nbv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLbyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5507,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5507,"v");
   } else {
    int i1 = 0;
     if(!egl_get_byte(env, v_t[i1++], &v[0])) Badarg(5507,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[1])) Badarg(5507,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[2])) Badarg(5507,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[3])) Badarg(5507,"v");
   }};
  weglVertexAttrib4Nbv(index,v);
}

static void ecb_glVertexAttrib4Niv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5508,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5508,"v");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v_t[i1++], &v[0])) Badarg(5508,"v");
     if(!enif_get_int(env, v_t[i1++], &v[1])) Badarg(5508,"v");
     if(!enif_get_int(env, v_t[i1++], &v[2])) Badarg(5508,"v");
     if(!enif_get_int(env, v_t[i1++], &v[3])) Badarg(5508,"v");
   }};
  weglVertexAttrib4Niv(index,v);
}

static void ecb_glVertexAttrib4Nsv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5509,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5509,"v");
   } else {
    int i1 = 0;
     if(!egl_get_short(env, v_t[i1++], &v[0])) Badarg(5509,"v");
     if(!egl_get_short(env, v_t[i1++], &v[1])) Badarg(5509,"v");
     if(!egl_get_short(env, v_t[i1++], &v[2])) Badarg(5509,"v");
     if(!egl_get_short(env, v_t[i1++], &v[3])) Badarg(5509,"v");
   }};
  weglVertexAttrib4Nsv(index,v);
}

static void ecb_glVertexAttrib4Nub(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLubyte x;
  GLubyte y;
  GLubyte z;
  GLubyte w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5510,"index");
  if(!egl_get_ubyte(env, argv[1],  &x)) Badarg(5510,"x");
  if(!egl_get_ubyte(env, argv[2],  &y)) Badarg(5510,"y");
  if(!egl_get_ubyte(env, argv[3],  &z)) Badarg(5510,"z");
  if(!egl_get_ubyte(env, argv[4],  &w)) Badarg(5510,"w");
  weglVertexAttrib4Nub(index,x,y,z,w);
}

static void ecb_glVertexAttrib4Nuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5511,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5511,"v");
   } else {
    int i1 = 0;
     if(!enif_get_uint(env, v_t[i1++], &v[0])) Badarg(5511,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[1])) Badarg(5511,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[2])) Badarg(5511,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[3])) Badarg(5511,"v");
   }};
  weglVertexAttrib4Nuiv(index,v);
}

static void ecb_glVertexAttrib4Nusv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLushort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5512,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5512,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ushort(env, v_t[i1++], &v[0])) Badarg(5512,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[1])) Badarg(5512,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[2])) Badarg(5512,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[3])) Badarg(5512,"v");
   }};
  weglVertexAttrib4Nusv(index,v);
}

static void ecb_glVertexAttrib4bv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLbyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5513,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5513,"v");
   } else {
    int i1 = 0;
     if(!egl_get_byte(env, v_t[i1++], &v[0])) Badarg(5513,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[1])) Badarg(5513,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[2])) Badarg(5513,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[3])) Badarg(5513,"v");
   }};
  weglVertexAttrib4bv(index,v);
}

static void ecb_glVertexAttrib4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5514,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5514,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5514,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5514,"z");
  if(!enif_get_double(env, argv[4],  &w)) Badarg(5514,"w");
  weglVertexAttrib4d(index,x,y,z,w);
}

static void ecb_glVertexAttrib4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5515,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5515,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5515,"y");
  if(!egl_get_float(env, argv[3],  &z)) Badarg(5515,"z");
  if(!egl_get_float(env, argv[4],  &w)) Badarg(5515,"w");
  weglVertexAttrib4f(index,x,y,z,w);
}

static void ecb_glVertexAttrib4iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5516,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5516,"v");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v_t[i1++], &v[0])) Badarg(5516,"v");
     if(!enif_get_int(env, v_t[i1++], &v[1])) Badarg(5516,"v");
     if(!enif_get_int(env, v_t[i1++], &v[2])) Badarg(5516,"v");
     if(!enif_get_int(env, v_t[i1++], &v[3])) Badarg(5516,"v");
   }};
  weglVertexAttrib4iv(index,v);
}

static void ecb_glVertexAttrib4s(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort x;
  GLshort y;
  GLshort z;
  GLshort w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5517,"index");
  if(!egl_get_short(env, argv[1],  &x)) Badarg(5517,"x");
  if(!egl_get_short(env, argv[2],  &y)) Badarg(5517,"y");
  if(!egl_get_short(env, argv[3],  &z)) Badarg(5517,"z");
  if(!egl_get_short(env, argv[4],  &w)) Badarg(5517,"w");
  weglVertexAttrib4s(index,x,y,z,w);
}

static void ecb_glVertexAttrib4ubv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLubyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5518,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5518,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ubyte(env, v_t[i1++], &v[0])) Badarg(5518,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[1])) Badarg(5518,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[2])) Badarg(5518,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[3])) Badarg(5518,"v");
   }};
  weglVertexAttrib4ubv(index,v);
}

static void ecb_glVertexAttrib4uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5519,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5519,"v");
   } else {
    int i1 = 0;
     if(!enif_get_uint(env, v_t[i1++], &v[0])) Badarg(5519,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[1])) Badarg(5519,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[2])) Badarg(5519,"v");
     if(!enif_get_uint(env, v_t[i1++], &v[3])) Badarg(5519,"v");
   }};
  weglVertexAttrib4uiv(index,v);
}

static void ecb_glVertexAttrib4usv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLushort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5520,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5520,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ushort(env, v_t[i1++], &v[0])) Badarg(5520,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[1])) Badarg(5520,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[2])) Badarg(5520,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[3])) Badarg(5520,"v");
   }};
  weglVertexAttrib4usv(index,v);
}

static void ecb_glVertexAttribPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint size;
  GLenum type;
  GLboolean normalized;
  GLsizei stride;
  ErlNifBinary pointer;
  GLvoid *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5521,"index");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5521,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5521,"type");
  if(!egl_get_ubyte(env, argv[3],  &normalized)) Badarg(5521,"normalized");
  if(!enif_get_int(env, argv[4],  &stride)) Badarg(5521,"stride");
  if(!egl_get_ptr(env, argv[5], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &pointer))
        pointer_idx = (GLvoid *) pointer.data;
    else Badarg(5521,"pointer");
  }
  weglVertexAttribPointer(index,size,type,normalized,stride,pointer_idx);
}

static void ecb_glUniformMatrix2x3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5523,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5523,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5523,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5523,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(6*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) {
            Badarg(5523,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5523,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5523,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5523,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5523,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5523,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5523,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix2x3fv(location,count,transpose,value);
}

static void ecb_glUniformMatrix3x2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5524,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5524,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5524,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5524,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(6*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) {
            Badarg(5524,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5524,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5524,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5524,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5524,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5524,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5524,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix3x2fv(location,count,transpose,value);
}

static void ecb_glUniformMatrix2x4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5525,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5525,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5525,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5525,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(8*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) {
            Badarg(5525,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5525,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5525,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5525,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5525,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5525,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5525,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5525,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5525,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix2x4fv(location,count,transpose,value);
}

static void ecb_glUniformMatrix4x2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5526,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5526,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5526,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5526,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(8*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) {
            Badarg(5526,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5526,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5526,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5526,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5526,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5526,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5526,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5526,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5526,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix4x2fv(location,count,transpose,value);
}

static void ecb_glUniformMatrix3x4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5527,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5527,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5527,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5527,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(12*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) {
            Badarg(5527,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[8], &value[8])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[9], &value[9])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[10], &value[10])) Badarg(5527,"value");
        if(!egl_get_float(env, value_tpl[11], &value[11])) Badarg(5527,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix3x4fv(location,count,transpose,value);
}

static void ecb_glUniformMatrix4x3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5528,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5528,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5528,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5528,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(12*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) {
            Badarg(5528,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[8], &value[8])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[9], &value[9])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[10], &value[10])) Badarg(5528,"value");
        if(!egl_get_float(env, value_tpl[11], &value[11])) Badarg(5528,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix4x3fv(location,count,transpose,value);
}

static void ecb_glColorMaski(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLboolean r;
  GLboolean g;
  GLboolean b;
  GLboolean a;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5529,"index");
  if(!egl_get_ubyte(env, argv[1],  &r)) Badarg(5529,"r");
  if(!egl_get_ubyte(env, argv[2],  &g)) Badarg(5529,"g");
  if(!egl_get_ubyte(env, argv[3],  &b)) Badarg(5529,"b");
  if(!egl_get_ubyte(env, argv[4],  &a)) Badarg(5529,"a");
  weglColorMaski(index,r,g,b,a);
}

static void ecb_glGetBooleani_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLboolean data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5530,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5530,"index");
  weglGetBooleani_v(target,index,data);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       data_ts[ri] =      enif_make_int(env, data[ri]);}
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetIntegeri_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLint data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5531,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5531,"index");
  weglGetIntegeri_v(target,index,data);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       data_ts[ri] =      enif_make_int(env, data[ri]);}
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glEnablei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5532,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5532,"index");
  weglEnablei(target,index);
}

static void ecb_glDisablei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5533,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5533,"index");
  weglDisablei(target,index);
}

static void ecb_glIsEnabledi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5534,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5534,"index");
  result = weglIsEnabledi(target,index);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBeginTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum primitiveMode;
  if(!enif_get_uint(env, argv[0],  &primitiveMode)) Badarg(5535,"primitiveMode");
  weglBeginTransformFeedback(primitiveMode);
}

static void ecb_glEndTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEndTransformFeedback();
}

static void ecb_glBindBufferRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLuint buffer;
  GLintptr offset;
  GLsizeiptr size;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5537,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5537,"index");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5537,"buffer");
  if(!enif_get_int64(env, argv[3],  &offset)) Badarg(5537,"offset");
  if(!enif_get_int64(env, argv[4],  &size)) Badarg(5537,"size");
  weglBindBufferRange(target,index,buffer,offset,size);
}

static void ecb_glBindBufferBase(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5538,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5538,"index");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5538,"buffer");
  weglBindBufferBase(target,index,buffer);
}

static void ecb_glTransformFeedbackVaryings(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLsizei count;
  GLchar ** varyings;
  ErlNifBinary varyings_all;
  GLchar *varyings_ptr;
  int i;
  GLenum bufferMode;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5539,"program");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5539,"count");
  if(!enif_inspect_binary(env, argv[2], &varyings_all)) Badarg(5539,"varyings");
  varyings = (GLchar **) enif_alloc(sizeof(GLchar *) * (int) argv[1]);
  varyings_ptr = (GLchar *) varyings_all.data;
  for(i=0; i < (int) argv[1]; i++) {     varyings[i] = (GLchar *) varyings_ptr;
     varyings_ptr += 1+strlen(varyings_ptr);
  }
  if(!enif_get_uint(env, argv[3],  &bufferMode)) Badarg(5539,"bufferMode");
  weglTransformFeedbackVaryings(program,count,(const GLchar **) varyings,bufferMode);
 enif_free(varyings);
}

static void ecb_glGetTransformFeedbackVarying(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint index;
  GLsizei bufSize;
  GLsizei length;
  GLsizei size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5540,"program");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5540,"index");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5540,"bufSize");
  name = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetTransformFeedbackVarying(program,index,bufSize,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

static void ecb_glClampColor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum clamp;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5541,"target");
  if(!enif_get_uint(env, argv[1],  &clamp)) Badarg(5541,"clamp");
  weglClampColor(target,clamp);
}

static void ecb_glBeginConditionalRender(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint id;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5542,"id");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5542,"mode");
  weglBeginConditionalRender(id,mode);
}

static void ecb_glEndConditionalRender(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglEndConditionalRender();
}

static void ecb_glVertexAttribIPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary pointer;
  GLvoid *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5544,"index");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5544,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5544,"type");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5544,"stride");
  if(!egl_get_ptr(env, argv[4], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[4], &pointer))
        pointer_idx = (GLvoid *) pointer.data;
    else Badarg(5544,"pointer");
  }
  weglVertexAttribIPointer(index,size,type,stride,pointer_idx);
}

static void ecb_glGetVertexAttribIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5546,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5546,"pname");
  weglGetVertexAttribIiv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetVertexAttribIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLuint params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5547,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5547,"pname");
  weglGetVertexAttribIuiv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glVertexAttribI1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5548,"index");
  if(!enif_get_int(env, argv[1],  &x)) Badarg(5548,"x");
  weglVertexAttribI1i(index,x);
}

static void ecb_glVertexAttribI2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint x;
  GLint y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5549,"index");
  if(!enif_get_int(env, argv[1],  &x)) Badarg(5549,"x");
  if(!enif_get_int(env, argv[2],  &y)) Badarg(5549,"y");
  weglVertexAttribI2i(index,x,y);
}

static void ecb_glVertexAttribI3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint x;
  GLint y;
  GLint z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5550,"index");
  if(!enif_get_int(env, argv[1],  &x)) Badarg(5550,"x");
  if(!enif_get_int(env, argv[2],  &y)) Badarg(5550,"y");
  if(!enif_get_int(env, argv[3],  &z)) Badarg(5550,"z");
  weglVertexAttribI3i(index,x,y,z);
}

static void ecb_glVertexAttribI4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint x;
  GLint y;
  GLint z;
  GLint w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5551,"index");
  if(!enif_get_int(env, argv[1],  &x)) Badarg(5551,"x");
  if(!enif_get_int(env, argv[2],  &y)) Badarg(5551,"y");
  if(!enif_get_int(env, argv[3],  &z)) Badarg(5551,"z");
  if(!enif_get_int(env, argv[4],  &w)) Badarg(5551,"w");
  weglVertexAttribI4i(index,x,y,z,w);
}

static void ecb_glVertexAttribI1ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5552,"index");
  if(!enif_get_uint(env, argv[1],  &x)) Badarg(5552,"x");
  weglVertexAttribI1ui(index,x);
}

static void ecb_glVertexAttribI2ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint x;
  GLuint y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5553,"index");
  if(!enif_get_uint(env, argv[1],  &x)) Badarg(5553,"x");
  if(!enif_get_uint(env, argv[2],  &y)) Badarg(5553,"y");
  weglVertexAttribI2ui(index,x,y);
}

static void ecb_glVertexAttribI3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint x;
  GLuint y;
  GLuint z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5554,"index");
  if(!enif_get_uint(env, argv[1],  &x)) Badarg(5554,"x");
  if(!enif_get_uint(env, argv[2],  &y)) Badarg(5554,"y");
  if(!enif_get_uint(env, argv[3],  &z)) Badarg(5554,"z");
  weglVertexAttribI3ui(index,x,y,z);
}

static void ecb_glVertexAttribI4ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint x;
  GLuint y;
  GLuint z;
  GLuint w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5555,"index");
  if(!enif_get_uint(env, argv[1],  &x)) Badarg(5555,"x");
  if(!enif_get_uint(env, argv[2],  &y)) Badarg(5555,"y");
  if(!enif_get_uint(env, argv[3],  &z)) Badarg(5555,"z");
  if(!enif_get_uint(env, argv[4],  &w)) Badarg(5555,"w");
  weglVertexAttribI4ui(index,x,y,z,w);
}

static void ecb_glVertexAttribI4bv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLbyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5556,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5556,"v");
   } else {
    int i1 = 0;
     if(!egl_get_byte(env, v_t[i1++], &v[0])) Badarg(5556,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[1])) Badarg(5556,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[2])) Badarg(5556,"v");
     if(!egl_get_byte(env, v_t[i1++], &v[3])) Badarg(5556,"v");
   }};
  weglVertexAttribI4bv(index,v);
}

static void ecb_glVertexAttribI4sv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLshort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5557,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5557,"v");
   } else {
    int i1 = 0;
     if(!egl_get_short(env, v_t[i1++], &v[0])) Badarg(5557,"v");
     if(!egl_get_short(env, v_t[i1++], &v[1])) Badarg(5557,"v");
     if(!egl_get_short(env, v_t[i1++], &v[2])) Badarg(5557,"v");
     if(!egl_get_short(env, v_t[i1++], &v[3])) Badarg(5557,"v");
   }};
  weglVertexAttribI4sv(index,v);
}

static void ecb_glVertexAttribI4ubv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLubyte v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5558,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5558,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ubyte(env, v_t[i1++], &v[0])) Badarg(5558,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[1])) Badarg(5558,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[2])) Badarg(5558,"v");
     if(!egl_get_ubyte(env, v_t[i1++], &v[3])) Badarg(5558,"v");
   }};
  weglVertexAttribI4ubv(index,v);
}

static void ecb_glVertexAttribI4usv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLushort v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5559,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5559,"v");
   } else {
    int i1 = 0;
     if(!egl_get_ushort(env, v_t[i1++], &v[0])) Badarg(5559,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[1])) Badarg(5559,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[2])) Badarg(5559,"v");
     if(!egl_get_ushort(env, v_t[i1++], &v[3])) Badarg(5559,"v");
   }};
  weglVertexAttribI4usv(index,v);
}

static void ecb_glGetUniformuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLuint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5560,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5560,"location");
  weglGetUniformuiv(program,location,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBindFragDataLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint color;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5561,"program");
  if(!enif_get_uint(env, argv[1],  &color)) Badarg(5561,"color");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5561,"name");
  weglBindFragDataLocation(program,color,(GLchar *) name.data);
}

static void ecb_glGetFragDataLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5562,"program");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5562,"name");
  result = weglGetFragDataLocation(program,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glUniform1ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint v0;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5563,"location");
  if(!enif_get_uint(env, argv[1],  &v0)) Badarg(5563,"v0");
  weglUniform1ui(location,v0);
}

static void ecb_glUniform2ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint v0;
  GLuint v1;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5564,"location");
  if(!enif_get_uint(env, argv[1],  &v0)) Badarg(5564,"v0");
  if(!enif_get_uint(env, argv[2],  &v1)) Badarg(5564,"v1");
  weglUniform2ui(location,v0,v1);
}

static void ecb_glUniform3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint v0;
  GLuint v1;
  GLuint v2;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5565,"location");
  if(!enif_get_uint(env, argv[1],  &v0)) Badarg(5565,"v0");
  if(!enif_get_uint(env, argv[2],  &v1)) Badarg(5565,"v1");
  if(!enif_get_uint(env, argv[3],  &v2)) Badarg(5565,"v2");
  weglUniform3ui(location,v0,v1,v2);
}

static void ecb_glUniform4ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLuint v0;
  GLuint v1;
  GLuint v2;
  GLuint v3;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5566,"location");
  if(!enif_get_uint(env, argv[1],  &v0)) Badarg(5566,"v0");
  if(!enif_get_uint(env, argv[2],  &v1)) Badarg(5566,"v1");
  if(!enif_get_uint(env, argv[3],  &v2)) Badarg(5566,"v2");
  if(!enif_get_uint(env, argv[4],  &v3)) Badarg(5566,"v3");
  weglUniform4ui(location,v0,v1,v2,v3);
}

static void ecb_glUniform1uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  int value_free = 0;
  GLuint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5567,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5567,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5567, "value"); }
  else {
    int i=0;
    ERL_NIF_TERM value_l, value_h, value_t;
    value_free = 1;
    value = (GLuint *) enif_alloc(count * sizeof(GLuint));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_uint(env, value_h, &value[i++])) Badarg(5567,"value");
        value_l = value_t;
    };
  }
  weglUniform1uiv(location,count,value);
 if(value_free) enif_free(value);
}

static void ecb_glUniform2uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5568,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5568,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5568,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLuint *) enif_alloc(2*count*sizeof(GLuint));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) {
            Badarg(5568,"value");
        } else {
        if(!enif_get_uint(env, value_tpl[0], &value[0])) Badarg(5568,"value");
        if(!enif_get_uint(env, value_tpl[1], &value[1])) Badarg(5568,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform2uiv(location,count,value);
}

static void ecb_glUniform3uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5569,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5569,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5569,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLuint *) enif_alloc(3*count*sizeof(GLuint));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) {
            Badarg(5569,"value");
        } else {
        if(!enif_get_uint(env, value_tpl[0], &value[0])) Badarg(5569,"value");
        if(!enif_get_uint(env, value_tpl[1], &value[1])) Badarg(5569,"value");
        if(!enif_get_uint(env, value_tpl[2], &value[2])) Badarg(5569,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform3uiv(location,count,value);
}

static void ecb_glUniform4uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5570,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5570,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5570,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLuint *) enif_alloc(4*count*sizeof(GLuint));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5570,"value");
        } else {
        if(!enif_get_uint(env, value_tpl[0], &value[0])) Badarg(5570,"value");
        if(!enif_get_uint(env, value_tpl[1], &value[1])) Badarg(5570,"value");
        if(!enif_get_uint(env, value_tpl[2], &value[2])) Badarg(5570,"value");
        if(!enif_get_uint(env, value_tpl[3], &value[3])) Badarg(5570,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform4uiv(location,count,value);
}

static void ecb_glTexParameterIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5571,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5571,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5571,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_int(env, params_t[i], &params[i])) Badarg(5571,"params");
   }};
  weglTexParameterIiv(target,pname,params);
}

static void ecb_glTexParameterIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  GLuint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5572,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5572,"pname");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t)) {
     Badarg(5572,"params");
   } else {
     for(i = 0; i < params_a; i++)
       if(!enif_get_uint(env, params_t[i], &params[i])) Badarg(5572,"params");
   }};
  weglTexParameterIuiv(target,pname,params);
}

static void ecb_glGetTexParameterIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5573,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5573,"pname");
  weglGetTexParameterIiv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetTexParameterIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLuint params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5574,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5574,"pname");
  weglGetTexParameterIuiv(target,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glClearBufferiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum buffer;
  GLint drawbuffer;
  GLint value[4];
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5575,"buffer");
  if(!enif_get_int(env, argv[1],  &drawbuffer)) Badarg(5575,"drawbuffer");
  {
   int value_a;
   const ERL_NIF_TERM *value_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &value_a, &value_t)) {
     Badarg(5575,"value");
   } else {
     for(i = 0; i < value_a; i++)
       if(!enif_get_int(env, value_t[i], &value[i])) Badarg(5575,"value");
   }};
  weglClearBufferiv(buffer,drawbuffer,value);
}

static void ecb_glClearBufferuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum buffer;
  GLint drawbuffer;
  GLuint value[4];
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5576,"buffer");
  if(!enif_get_int(env, argv[1],  &drawbuffer)) Badarg(5576,"drawbuffer");
  {
   int value_a;
   const ERL_NIF_TERM *value_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &value_a, &value_t)) {
     Badarg(5576,"value");
   } else {
     for(i = 0; i < value_a; i++)
       if(!enif_get_uint(env, value_t[i], &value[i])) Badarg(5576,"value");
   }};
  weglClearBufferuiv(buffer,drawbuffer,value);
}

static void ecb_glClearBufferfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum buffer;
  GLint drawbuffer;
  GLfloat value[4];
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5577,"buffer");
  if(!enif_get_int(env, argv[1],  &drawbuffer)) Badarg(5577,"drawbuffer");
  {
   int value_a;
   const ERL_NIF_TERM *value_t;
   int i;
   if(!enif_get_tuple(env, argv[2], &value_a, &value_t)) {
     Badarg(5577,"value");
   } else {
     for(i = 0; i < value_a; i++)
       if(!egl_get_float(env, value_t[i], &value[i])) Badarg(5577,"value");
   }};
  weglClearBufferfv(buffer,drawbuffer,value);
}

static void ecb_glClearBufferfi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum buffer;
  GLint drawbuffer;
  GLfloat depth;
  GLint stencil;
  if(!enif_get_uint(env, argv[0],  &buffer)) Badarg(5578,"buffer");
  if(!enif_get_int(env, argv[1],  &drawbuffer)) Badarg(5578,"drawbuffer");
  if(!egl_get_float(env, argv[2],  &depth)) Badarg(5578,"depth");
  if(!enif_get_int(env, argv[3],  &stencil)) Badarg(5578,"stencil");
  weglClearBufferfi(buffer,drawbuffer,depth,stencil);
}

static void ecb_glGetStringi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  const GLubyte *  result;
  ERL_NIF_TERM reply;
  GLenum name;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &name)) Badarg(5579,"name");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5579,"index");
  result = weglGetStringi(name,index);
  reply =      enif_make_string(env, (const char *) result, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDrawArraysInstanced(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint first;
  GLsizei count;
  GLsizei primcount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5580,"mode");
  if(!enif_get_int(env, argv[1],  &first)) Badarg(5580,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5580,"count");
  if(!enif_get_int(env, argv[3],  &primcount)) Badarg(5580,"primcount");
  weglDrawArraysInstanced(mode,first,count,primcount);
}

static void ecb_glDrawElementsInstanced(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  GLvoid *indices_idx;
  GLsizei primcount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5581,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5581,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5581,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (GLvoid *) indices.data;
    else Badarg(5581,"indices");
  }
  if(!enif_get_int(env, argv[4],  &primcount)) Badarg(5581,"primcount");
  weglDrawElementsInstanced(mode,count,type,indices_idx,primcount);
}

static void ecb_glTexBuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLuint buffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5583,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5583,"internalformat");
  if(!enif_get_uint(env, argv[2],  &buffer)) Badarg(5583,"buffer");
  weglTexBuffer(target,internalformat,buffer);
}

static void ecb_glPrimitiveRestartIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5584,"index");
  weglPrimitiveRestartIndex(index);
}

static void ecb_glGetInteger64i_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLint64 data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5585,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5585,"index");
  weglGetInteger64i_v(target,index,data);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       data_ts[ri] =      enif_make_int64(env, (int64_t) data[ri]);}
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetBufferParameteri64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint64 params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5586,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5586,"pname");
  weglGetBufferParameteri64v(target,pname,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int64(env, (int64_t) params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glFramebufferTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLuint texture;
  GLint level;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5587,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5587,"attachment");
  if(!enif_get_uint(env, argv[2],  &texture)) Badarg(5587,"texture");
  if(!enif_get_int(env, argv[3],  &level)) Badarg(5587,"level");
  weglFramebufferTexture(target,attachment,texture,level);
}

static void ecb_glVertexAttribDivisor(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLuint divisor;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5588,"index");
  if(!enif_get_uint(env, argv[1],  &divisor)) Badarg(5588,"divisor");
  weglVertexAttribDivisor(index,divisor);
}

static void ecb_glMinSampleShading(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf value;
  if(!egl_get_float(env, argv[0],  &value)) Badarg(5589,"value");
  weglMinSampleShading(value);
}

static void ecb_glBlendEquationi(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buf;
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &buf)) Badarg(5590,"buf");
  if(!enif_get_uint(env, argv[1],  &mode)) Badarg(5590,"mode");
  weglBlendEquationi(buf,mode);
}

static void ecb_glBlendEquationSeparatei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buf;
  GLenum modeRGB;
  GLenum modeAlpha;
  if(!enif_get_uint(env, argv[0],  &buf)) Badarg(5591,"buf");
  if(!enif_get_uint(env, argv[1],  &modeRGB)) Badarg(5591,"modeRGB");
  if(!enif_get_uint(env, argv[2],  &modeAlpha)) Badarg(5591,"modeAlpha");
  weglBlendEquationSeparatei(buf,modeRGB,modeAlpha);
}

static void ecb_glBlendFunci(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buf;
  GLenum src;
  GLenum dst;
  if(!enif_get_uint(env, argv[0],  &buf)) Badarg(5592,"buf");
  if(!enif_get_uint(env, argv[1],  &src)) Badarg(5592,"src");
  if(!enif_get_uint(env, argv[2],  &dst)) Badarg(5592,"dst");
  weglBlendFunci(buf,src,dst);
}

static void ecb_glBlendFuncSeparatei(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint buf;
  GLenum srcRGB;
  GLenum dstRGB;
  GLenum srcAlpha;
  GLenum dstAlpha;
  if(!enif_get_uint(env, argv[0],  &buf)) Badarg(5593,"buf");
  if(!enif_get_uint(env, argv[1],  &srcRGB)) Badarg(5593,"srcRGB");
  if(!enif_get_uint(env, argv[2],  &dstRGB)) Badarg(5593,"dstRGB");
  if(!enif_get_uint(env, argv[3],  &srcAlpha)) Badarg(5593,"srcAlpha");
  if(!enif_get_uint(env, argv[4],  &dstAlpha)) Badarg(5593,"dstAlpha");
  weglBlendFuncSeparatei(buf,srcRGB,dstRGB,srcAlpha,dstAlpha);
}

static void ecb_glLoadTransposeMatrixfARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5594,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5594,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5594,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5594,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5594,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5594,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5594,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadTransposeMatrixfARB(m);
}

static void ecb_glLoadTransposeMatrixdARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5595,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5595,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5595,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5595,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5595,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5595,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5595,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglLoadTransposeMatrixdARB(m);
}

static void ecb_glMultTransposeMatrixfARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLfloat m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5596,"m");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, m_t[i1++], &m[0])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[1])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[2])) Badarg(5596,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[3])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[4])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[5])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[6])) Badarg(5596,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[7])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[8])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[9])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[10])) Badarg(5596,"m");
     if(m_a == 16)
        if(!egl_get_float(env, m_t[i1++], &m[11])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[12])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[13])) Badarg(5596,"m");
     if(!egl_get_float(env, m_t[i1++], &m[14])) Badarg(5596,"m");
     if(m_a == 16) {
        if(!egl_get_float(env, m_t[i1++], &m[15])) Badarg(5596,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultTransposeMatrixfARB(m);
}

static void ecb_glMultTransposeMatrixdARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLdouble m[16];
  {
   int m_a;
   const ERL_NIF_TERM *m_t;
   if(!enif_get_tuple(env, argv[0], &m_a, &m_t)
       || (m_a != 12 && m_a != 16)) {
     Badarg(5597,"m");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, m_t[i1++], &m[0])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[1])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[2])) Badarg(5597,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[3])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[4])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[5])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[6])) Badarg(5597,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[7])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[8])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[9])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[10])) Badarg(5597,"m");
     if(m_a == 16)
        if(!enif_get_double(env, m_t[i1++], &m[11])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[12])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[13])) Badarg(5597,"m");
     if(!enif_get_double(env, m_t[i1++], &m[14])) Badarg(5597,"m");
     if(m_a == 16) {
        if(!enif_get_double(env, m_t[i1++], &m[15])) Badarg(5597,"m");
     } else {
       m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
     }
   }};
  weglMultTransposeMatrixdARB(m);
}

static void ecb_glWeightbvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int weights_free = 0;
  GLbyte *weights;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5598,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5598, "weights"); }
  else {
    int i=0;
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    weights_free = 1;
    weights = (GLbyte *) enif_alloc(size * sizeof(GLbyte));
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_byte(env, weights_h, &weights[i++])) Badarg(5598,"weights");
        weights_l = weights_t;
    };
  }
  weglWeightbvARB(size,weights);
 if(weights_free) enif_free(weights);
}

static void ecb_glWeightsvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int weights_free = 0;
  GLshort *weights;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5599,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5599, "weights"); }
  else {
    int i=0;
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    weights_free = 1;
    weights = (GLshort *) enif_alloc(size * sizeof(GLshort));
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_short(env, weights_h, &weights[i++])) Badarg(5599,"weights");
        weights_l = weights_t;
    };
  }
  weglWeightsvARB(size,weights);
 if(weights_free) enif_free(weights);
}

static void ecb_glWeightivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int weights_free = 0;
  GLint *weights;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5600,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5600, "weights"); }
  else {
    int i=0;
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    weights_free = 1;
    weights = (GLint *) enif_alloc(size * sizeof(GLint));
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!enif_get_int(env, weights_h, &weights[i++])) Badarg(5600,"weights");
        weights_l = weights_t;
    };
  }
  weglWeightivARB(size,weights);
 if(weights_free) enif_free(weights);
}

static void ecb_glWeightfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int weights_free = 0;
  GLfloat *weights;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5601,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5601, "weights"); }
  else {
    int i=0;
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    weights_free = 1;
    weights = (GLfloat *) enif_alloc(size * sizeof(GLfloat));
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_float(env, weights_h, &weights[i++])) Badarg(5601,"weights");
        weights_l = weights_t;
    };
  }
  weglWeightfvARB(size,weights);
 if(weights_free) enif_free(weights);
}

static void ecb_glWeightdvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int weights_free = 0;
  GLdouble *weights;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5602,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5602, "weights"); }
  else {
    int i=0;
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    weights_free = 1;
    weights = (GLdouble *) enif_alloc(size * sizeof(GLdouble));
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!enif_get_double(env, weights_h, &weights[i++])) Badarg(5602,"weights");
        weights_l = weights_t;
    };
  }
  weglWeightdvARB(size,weights);
 if(weights_free) enif_free(weights);
}

static void ecb_glWeightubvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int weights_free = 0;
  GLubyte *weights;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5603,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5603, "weights"); }
  else {
    int i=0;
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    weights_free = 1;
    weights = (GLubyte *) enif_alloc(size * sizeof(GLubyte));
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_ubyte(env, weights_h, &weights[i++])) Badarg(5603,"weights");
        weights_l = weights_t;
    };
  }
  weglWeightubvARB(size,weights);
 if(weights_free) enif_free(weights);
}

static void ecb_glWeightusvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int weights_free = 0;
  GLushort *weights;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5604,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5604, "weights"); }
  else {
    int i=0;
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    weights_free = 1;
    weights = (GLushort *) enif_alloc(size * sizeof(GLushort));
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!egl_get_ushort(env, weights_h, &weights[i++])) Badarg(5604,"weights");
        weights_l = weights_t;
    };
  }
  weglWeightusvARB(size,weights);
 if(weights_free) enif_free(weights);
}

static void ecb_glWeightuivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int weights_free = 0;
  GLuint *weights;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5605,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5605, "weights"); }
  else {
    int i=0;
    ERL_NIF_TERM weights_l, weights_h, weights_t;
    weights_free = 1;
    weights = (GLuint *) enif_alloc(size * sizeof(GLuint));
    weights_l = argv[1];
    while(enif_get_list_cell(env, weights_l, &weights_h, &weights_t)) {
        if(!enif_get_uint(env, weights_h, &weights[i++])) Badarg(5605,"weights");
        weights_l = weights_t;
    };
  }
  weglWeightuivARB(size,weights);
 if(weights_free) enif_free(weights);
}

static void ecb_glVertexBlendARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint count;
  if(!enif_get_int(env, argv[0],  &count)) Badarg(5606,"count");
  weglVertexBlendARB(count);
}

static void ecb_glCurrentPaletteMatrixARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint index;
  if(!enif_get_int(env, argv[0],  &index)) Badarg(5607,"index");
  weglCurrentPaletteMatrixARB(index);
}

static void ecb_glMatrixIndexubvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int indices_free = 0;
  GLubyte *indices;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5608,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5608, "indices"); }
  else {
    int i=0;
    ERL_NIF_TERM indices_l, indices_h, indices_t;
    indices_free = 1;
    indices = (GLubyte *) enif_alloc(size * sizeof(GLubyte));
    indices_l = argv[1];
    while(enif_get_list_cell(env, indices_l, &indices_h, &indices_t)) {
        if(!egl_get_ubyte(env, indices_h, &indices[i++])) Badarg(5608,"indices");
        indices_l = indices_t;
    };
  }
  weglMatrixIndexubvARB(size,indices);
 if(indices_free) enif_free(indices);
}

static void ecb_glMatrixIndexusvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int indices_free = 0;
  GLushort *indices;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5609,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5609, "indices"); }
  else {
    int i=0;
    ERL_NIF_TERM indices_l, indices_h, indices_t;
    indices_free = 1;
    indices = (GLushort *) enif_alloc(size * sizeof(GLushort));
    indices_l = argv[1];
    while(enif_get_list_cell(env, indices_l, &indices_h, &indices_t)) {
        if(!egl_get_ushort(env, indices_h, &indices[i++])) Badarg(5609,"indices");
        indices_l = indices_t;
    };
  }
  weglMatrixIndexusvARB(size,indices);
 if(indices_free) enif_free(indices);
}

static void ecb_glMatrixIndexuivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint size;
  int indices_free = 0;
  GLuint *indices;
  if(!enif_get_int(env, argv[0],  &size)) Badarg(5610,"size");
  if(!enif_is_list(env, argv[1])) { Badarg(5610, "indices"); }
  else {
    int i=0;
    ERL_NIF_TERM indices_l, indices_h, indices_t;
    indices_free = 1;
    indices = (GLuint *) enif_alloc(size * sizeof(GLuint));
    indices_l = argv[1];
    while(enif_get_list_cell(env, indices_l, &indices_h, &indices_t)) {
        if(!enif_get_uint(env, indices_h, &indices[i++])) Badarg(5610,"indices");
        indices_l = indices_t;
    };
  }
  weglMatrixIndexuivARB(size,indices);
 if(indices_free) enif_free(indices);
}

static void ecb_glProgramStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum format;
  ErlNifBinary string;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5611,"target");
  if(!enif_get_uint(env, argv[1],  &format)) Badarg(5611,"format");
  if(!enif_inspect_binary(env, argv[3], &string)) Badarg(5611,"string");
  weglProgramStringARB(target,format,(GLsizei) string.size,(GLvoid *) string.data);
}

static void ecb_glBindProgramARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5612,"target");
  if(!enif_get_uint(env, argv[1],  &program)) Badarg(5612,"program");
  weglBindProgramARB(target,program);
}

static void ecb_glDeleteProgramsARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int programs_free = 0;
  GLuint *programs;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5613,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5613, "programs"); }
  else {
    int i=0;
    ERL_NIF_TERM programs_l, programs_h, programs_t;
    programs_free = 1;
    programs = (GLuint *) enif_alloc(n * sizeof(GLuint));
    programs_l = argv[1];
    while(enif_get_list_cell(env, programs_l, &programs_h, &programs_t)) {
        if(!enif_get_uint(env, programs_h, &programs[i++])) Badarg(5613,"programs");
        programs_l = programs_t;
    };
  }
  weglDeleteProgramsARB(n,programs);
 if(programs_free) enif_free(programs);
}

static void ecb_glGenProgramsARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * programs;
  ERL_NIF_TERM *programs_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5614,"n");
  programs = enif_alloc((int)n*sizeof(GLuint));
  programs_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenProgramsARB(n,programs);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      programs_ts[ri] =      enif_make_int(env, programs[ri]);}
  reply =      enif_make_list_from_array(env, programs_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(programs_ts);
 enif_free(programs);
}

static void ecb_glProgramEnvParameter4dARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5615,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5615,"index");
  if(!enif_get_double(env, argv[2],  &x)) Badarg(5615,"x");
  if(!enif_get_double(env, argv[3],  &y)) Badarg(5615,"y");
  if(!enif_get_double(env, argv[4],  &z)) Badarg(5615,"z");
  if(!enif_get_double(env, argv[5],  &w)) Badarg(5615,"w");
  weglProgramEnvParameter4dARB(target,index,x,y,z,w);
}

static void ecb_glProgramEnvParameter4dvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5616,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5616,"index");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5616,"params");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, params_t[i1++], &params[0])) Badarg(5616,"params");
     if(!enif_get_double(env, params_t[i1++], &params[1])) Badarg(5616,"params");
     if(!enif_get_double(env, params_t[i1++], &params[2])) Badarg(5616,"params");
     if(!enif_get_double(env, params_t[i1++], &params[3])) Badarg(5616,"params");
   }};
  weglProgramEnvParameter4dvARB(target,index,params);
}

static void ecb_glProgramEnvParameter4fARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5617,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5617,"index");
  if(!egl_get_float(env, argv[2],  &x)) Badarg(5617,"x");
  if(!egl_get_float(env, argv[3],  &y)) Badarg(5617,"y");
  if(!egl_get_float(env, argv[4],  &z)) Badarg(5617,"z");
  if(!egl_get_float(env, argv[5],  &w)) Badarg(5617,"w");
  weglProgramEnvParameter4fARB(target,index,x,y,z,w);
}

static void ecb_glProgramEnvParameter4fvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5618,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5618,"index");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5618,"params");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, params_t[i1++], &params[0])) Badarg(5618,"params");
     if(!egl_get_float(env, params_t[i1++], &params[1])) Badarg(5618,"params");
     if(!egl_get_float(env, params_t[i1++], &params[2])) Badarg(5618,"params");
     if(!egl_get_float(env, params_t[i1++], &params[3])) Badarg(5618,"params");
   }};
  weglProgramEnvParameter4fvARB(target,index,params);
}

static void ecb_glProgramLocalParameter4dARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5619,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5619,"index");
  if(!enif_get_double(env, argv[2],  &x)) Badarg(5619,"x");
  if(!enif_get_double(env, argv[3],  &y)) Badarg(5619,"y");
  if(!enif_get_double(env, argv[4],  &z)) Badarg(5619,"z");
  if(!enif_get_double(env, argv[5],  &w)) Badarg(5619,"w");
  weglProgramLocalParameter4dARB(target,index,x,y,z,w);
}

static void ecb_glProgramLocalParameter4dvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5620,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5620,"index");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5620,"params");
   } else {
    int i1 = 0;
     if(!enif_get_double(env, params_t[i1++], &params[0])) Badarg(5620,"params");
     if(!enif_get_double(env, params_t[i1++], &params[1])) Badarg(5620,"params");
     if(!enif_get_double(env, params_t[i1++], &params[2])) Badarg(5620,"params");
     if(!enif_get_double(env, params_t[i1++], &params[3])) Badarg(5620,"params");
   }};
  weglProgramLocalParameter4dvARB(target,index,params);
}

static void ecb_glProgramLocalParameter4fARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat z;
  GLfloat w;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5621,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5621,"index");
  if(!egl_get_float(env, argv[2],  &x)) Badarg(5621,"x");
  if(!egl_get_float(env, argv[3],  &y)) Badarg(5621,"y");
  if(!egl_get_float(env, argv[4],  &z)) Badarg(5621,"z");
  if(!egl_get_float(env, argv[5],  &w)) Badarg(5621,"w");
  weglProgramLocalParameter4fARB(target,index,x,y,z,w);
}

static void ecb_glProgramLocalParameter4fvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5622,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5622,"index");
  {
   int params_a;
   const ERL_NIF_TERM *params_t;
   if(!enif_get_tuple(env, argv[2], &params_a, &params_t) || params_a != 4) {
     Badarg(5622,"params");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, params_t[i1++], &params[0])) Badarg(5622,"params");
     if(!egl_get_float(env, params_t[i1++], &params[1])) Badarg(5622,"params");
     if(!egl_get_float(env, params_t[i1++], &params[2])) Badarg(5622,"params");
     if(!egl_get_float(env, params_t[i1++], &params[3])) Badarg(5622,"params");
   }};
  weglProgramLocalParameter4fvARB(target,index,params);
}

static void ecb_glGetProgramEnvParameterdvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5623,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5623,"index");
  weglGetProgramEnvParameterdvARB(target,index,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetProgramEnvParameterfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5624,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5624,"index");
  weglGetProgramEnvParameterfvARB(target,index,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetProgramLocalParameterdvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5625,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5625,"index");
  weglGetProgramLocalParameterdvARB(target,index,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetProgramLocalParameterfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5626,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5626,"index");
  weglGetProgramLocalParameterfvARB(target,index,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetProgramStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum pname;
  ErlNifBinary string;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5627,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5627,"pname");
  if(enif_is_binary(env, argv[2]))
    enif_inspect_binary(env, argv[2], &string);
  else if(enif_is_tuple(env, argv[2])) {
    int string_a;
    const ERL_NIF_TERM *string_t;
    if(enif_get_tuple(env, argv[2], &string_a, &string_t) ||
         enif_is_binary(env, string_t[1]))
       enif_inspect_binary(env, string_t[1], &string);
    else Badarg(5627, "string");
  } else Badarg(5627, "string");
  weglGetProgramStringARB(target,pname,(GLvoid *) string.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetBufferParameterivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5628,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5628,"pname");
  weglGetBufferParameterivARB(target,pname,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDeleteObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t obj;
  if(!enif_get_uint64(env, argv[0], (void *) &obj)) Badarg(5629,"obj");
  weglDeleteObjectARB((GLhandleARB) obj);
}

static void ecb_glGetHandleARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLhandleARB result;
  ERL_NIF_TERM reply;
  GLenum pname;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5630,"pname");
  result = weglGetHandleARB(pname);
  reply =      enif_make_uint64(env, (uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDetachObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t containerObj;
  uint64_t attachedObj;
  if(!enif_get_uint64(env, argv[0], (void *) &containerObj)) Badarg(5631,"containerObj");
  if(!enif_get_uint64(env, argv[1], (void *) &attachedObj)) Badarg(5631,"attachedObj");
  weglDetachObjectARB((GLhandleARB) containerObj,(GLhandleARB) attachedObj);
}

static void ecb_glCreateShaderObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLhandleARB result;
  ERL_NIF_TERM reply;
  GLenum shaderType;
  if(!enif_get_uint(env, argv[0],  &shaderType)) Badarg(5632,"shaderType");
  result = weglCreateShaderObjectARB(shaderType);
  reply =      enif_make_uint64(env, (uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glShaderSourceARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t shaderObj;
  GLsizei count;
  GLchar ** string;
  ErlNifBinary string_all;
  GLchar *string_ptr;
  int i;
  if(!enif_get_uint64(env, argv[0], (void *) &shaderObj)) Badarg(5633,"shaderObj");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5633,"count");
  if(!enif_inspect_binary(env, argv[2], &string_all)) Badarg(5633,"string");
  string = (GLchar **) enif_alloc(sizeof(GLchar *) * (int) argv[1]);
  string_ptr = (GLchar *) string_all.data;
  for(i=0; i < (int) argv[1]; i++) {     string[i] = (GLchar *) string_ptr;
     string_ptr += 1+strlen(string_ptr);
  }
  weglShaderSourceARB((GLhandleARB) shaderObj,count,(const GLchar **) string,NULL);
 enif_free(string);
}

static void ecb_glCompileShaderARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t shaderObj;
  if(!enif_get_uint64(env, argv[0], (void *) &shaderObj)) Badarg(5634,"shaderObj");
  weglCompileShaderARB((GLhandleARB) shaderObj);
}

static void ecb_glCreateProgramObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLhandleARB result;
  ERL_NIF_TERM reply;
  result = weglCreateProgramObjectARB();
  reply =      enif_make_uint64(env, (uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glAttachObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t containerObj;
  uint64_t obj;
  if(!enif_get_uint64(env, argv[0], (void *) &containerObj)) Badarg(5636,"containerObj");
  if(!enif_get_uint64(env, argv[1], (void *) &obj)) Badarg(5636,"obj");
  weglAttachObjectARB((GLhandleARB) containerObj,(GLhandleARB) obj);
}

static void ecb_glLinkProgramARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t programObj;
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5637,"programObj");
  weglLinkProgramARB((GLhandleARB) programObj);
}

static void ecb_glUseProgramObjectARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t programObj;
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5638,"programObj");
  weglUseProgramObjectARB((GLhandleARB) programObj);
}

static void ecb_glValidateProgramARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t programObj;
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5639,"programObj");
  weglValidateProgramARB((GLhandleARB) programObj);
}

static void ecb_glGetObjectParameterfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t obj;
  GLenum pname;
  GLfloat params;
  if(!enif_get_uint64(env, argv[0], (void *) &obj)) Badarg(5640,"obj");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5640,"pname");
  weglGetObjectParameterfvARB((GLhandleARB) obj,pname,&params);
  reply =      enif_make_double(env, (double) params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetObjectParameterivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t obj;
  GLenum pname;
  GLint params;
  if(!enif_get_uint64(env, argv[0], (void *) &obj)) Badarg(5641,"obj");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5641,"pname");
  weglGetObjectParameterivARB((GLhandleARB) obj,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetInfoLogARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t obj;
  GLsizei maxLength;
  GLsizei length;
  unsigned char *infoLog;
  if(!enif_get_uint64(env, argv[0], (void *) &obj)) Badarg(5642,"obj");
  if(!enif_get_int(env, argv[1],  &maxLength)) Badarg(5642,"maxLength");
  infoLog = enif_alloc((int) maxLength*sizeof(GLchar));
  weglGetInfoLogARB((GLhandleARB) obj,maxLength,&length,(GLchar *) infoLog);
  reply =      enif_make_string(env, (const char *) infoLog, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(infoLog);
}

static void ecb_glGetAttachedObjectsARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t containerObj;
  GLsizei maxCount;
  GLsizei count;
  GLhandleARB * obj;
  ERL_NIF_TERM *obj_ts;
  if(!enif_get_uint64(env, argv[0], (void *) &containerObj)) Badarg(5643,"containerObj");
  if(!enif_get_int(env, argv[1],  &maxCount)) Badarg(5643,"maxCount");
  obj = enif_alloc((int)maxCount*sizeof(GLhandleARB));
  obj_ts = (ERL_NIF_TERM *) enif_alloc((int)maxCount*sizeof(ERL_NIF_TERM));
  weglGetAttachedObjectsARB((GLhandleARB) containerObj,maxCount,&count,obj);
  { int ri;
    for(ri=0; ri < (int) count; ri++)
      obj_ts[ri] =      enif_make_uint64(env, (uint64_t) obj[ri]);}
  reply =      enif_make_list_from_array(env, obj_ts, count);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(obj_ts);
 enif_free(obj);
}

static void ecb_glGetUniformLocationARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  uint64_t programObj;
  ErlNifBinary name;
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5644,"programObj");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5644,"name");
  result = weglGetUniformLocationARB((GLhandleARB) programObj,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetActiveUniformARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t programObj;
  GLuint index;
  GLsizei maxLength;
  GLsizei length;
  GLint size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5645,"programObj");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5645,"index");
  if(!enif_get_int(env, argv[2],  &maxLength)) Badarg(5645,"maxLength");
  name = enif_alloc((int) maxLength*sizeof(GLchar));
  weglGetActiveUniformARB((GLhandleARB) programObj,index,maxLength,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

static void ecb_glGetUniformfvARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t programObj;
  GLint location;
  GLfloat params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5646,"programObj");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5646,"location");
  weglGetUniformfvARB((GLhandleARB) programObj,location,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_double(env, (double) params[ri]);}
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetUniformivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t programObj;
  GLint location;
  GLint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5647,"programObj");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5647,"location");
  weglGetUniformivARB((GLhandleARB) programObj,location,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetShaderSourceARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t obj;
  GLsizei maxLength;
  GLsizei length;
  unsigned char *source;
  if(!enif_get_uint64(env, argv[0], (void *) &obj)) Badarg(5648,"obj");
  if(!enif_get_int(env, argv[1],  &maxLength)) Badarg(5648,"maxLength");
  source = enif_alloc((int) maxLength*sizeof(GLchar));
  weglGetShaderSourceARB((GLhandleARB) obj,maxLength,&length,(GLchar *) source);
  reply =      enif_make_string(env, (const char *) source, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(source);
}

static void ecb_glBindAttribLocationARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t programObj;
  GLuint index;
  ErlNifBinary name;
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5649,"programObj");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5649,"index");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5649,"name");
  weglBindAttribLocationARB((GLhandleARB) programObj,index,(GLchar *) name.data);
}

static void ecb_glGetActiveAttribARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t programObj;
  GLuint index;
  GLsizei maxLength;
  GLsizei length;
  GLint size;
  GLenum type;
  unsigned char *name;
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5650,"programObj");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5650,"index");
  if(!enif_get_int(env, argv[2],  &maxLength)) Badarg(5650,"maxLength");
  name = enif_alloc((int) maxLength*sizeof(GLchar));
  weglGetActiveAttribARB((GLhandleARB) programObj,index,maxLength,&length,&size,&type,(GLchar *) name);
  reply = enif_make_tuple3(env,
          enif_make_int(env, size),
     enif_make_int(env, type),
     enif_make_string(env, (const char *) name, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

static void ecb_glGetAttribLocationARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  uint64_t programObj;
  ErlNifBinary name;
  if(!enif_get_uint64(env, argv[0], (void *) &programObj)) Badarg(5651,"programObj");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5651,"name");
  result = weglGetAttribLocationARB((GLhandleARB) programObj,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glIsRenderbuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint renderbuffer;
  if(!enif_get_uint(env, argv[0],  &renderbuffer)) Badarg(5652,"renderbuffer");
  result = weglIsRenderbuffer(renderbuffer);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBindRenderbuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint renderbuffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5653,"target");
  if(!enif_get_uint(env, argv[1],  &renderbuffer)) Badarg(5653,"renderbuffer");
  weglBindRenderbuffer(target,renderbuffer);
}

static void ecb_glDeleteRenderbuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int renderbuffers_free = 0;
  GLuint *renderbuffers;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5654,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5654, "renderbuffers"); }
  else {
    int i=0;
    ERL_NIF_TERM renderbuffers_l, renderbuffers_h, renderbuffers_t;
    renderbuffers_free = 1;
    renderbuffers = (GLuint *) enif_alloc(n * sizeof(GLuint));
    renderbuffers_l = argv[1];
    while(enif_get_list_cell(env, renderbuffers_l, &renderbuffers_h, &renderbuffers_t)) {
        if(!enif_get_uint(env, renderbuffers_h, &renderbuffers[i++])) Badarg(5654,"renderbuffers");
        renderbuffers_l = renderbuffers_t;
    };
  }
  weglDeleteRenderbuffers(n,renderbuffers);
 if(renderbuffers_free) enif_free(renderbuffers);
}

static void ecb_glGenRenderbuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * renderbuffers;
  ERL_NIF_TERM *renderbuffers_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5655,"n");
  renderbuffers = enif_alloc((int)n*sizeof(GLuint));
  renderbuffers_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenRenderbuffers(n,renderbuffers);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      renderbuffers_ts[ri] =      enif_make_int(env, renderbuffers[ri]);}
  reply =      enif_make_list_from_array(env, renderbuffers_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(renderbuffers_ts);
 enif_free(renderbuffers);
}

static void ecb_glRenderbufferStorage(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5656,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5656,"internalformat");
  if(!enif_get_int(env, argv[2],  &width)) Badarg(5656,"width");
  if(!enif_get_int(env, argv[3],  &height)) Badarg(5656,"height");
  weglRenderbufferStorage(target,internalformat,width,height);
}

static void ecb_glGetRenderbufferParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5657,"target");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5657,"pname");
  weglGetRenderbufferParameteriv(target,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glIsFramebuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint framebuffer;
  if(!enif_get_uint(env, argv[0],  &framebuffer)) Badarg(5658,"framebuffer");
  result = weglIsFramebuffer(framebuffer);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBindFramebuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint framebuffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5659,"target");
  if(!enif_get_uint(env, argv[1],  &framebuffer)) Badarg(5659,"framebuffer");
  weglBindFramebuffer(target,framebuffer);
}

static void ecb_glDeleteFramebuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int framebuffers_free = 0;
  GLuint *framebuffers;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5660,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5660, "framebuffers"); }
  else {
    int i=0;
    ERL_NIF_TERM framebuffers_l, framebuffers_h, framebuffers_t;
    framebuffers_free = 1;
    framebuffers = (GLuint *) enif_alloc(n * sizeof(GLuint));
    framebuffers_l = argv[1];
    while(enif_get_list_cell(env, framebuffers_l, &framebuffers_h, &framebuffers_t)) {
        if(!enif_get_uint(env, framebuffers_h, &framebuffers[i++])) Badarg(5660,"framebuffers");
        framebuffers_l = framebuffers_t;
    };
  }
  weglDeleteFramebuffers(n,framebuffers);
 if(framebuffers_free) enif_free(framebuffers);
}

static void ecb_glGenFramebuffers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * framebuffers;
  ERL_NIF_TERM *framebuffers_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5661,"n");
  framebuffers = enif_alloc((int)n*sizeof(GLuint));
  framebuffers_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenFramebuffers(n,framebuffers);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      framebuffers_ts[ri] =      enif_make_int(env, framebuffers[ri]);}
  reply =      enif_make_list_from_array(env, framebuffers_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(framebuffers_ts);
 enif_free(framebuffers);
}

static void ecb_glCheckFramebufferStatus(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum result;
  ERL_NIF_TERM reply;
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5662,"target");
  result = weglCheckFramebufferStatus(target);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glFramebufferTexture1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLenum textarget;
  GLuint texture;
  GLint level;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5663,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5663,"attachment");
  if(!enif_get_uint(env, argv[2],  &textarget)) Badarg(5663,"textarget");
  if(!enif_get_uint(env, argv[3],  &texture)) Badarg(5663,"texture");
  if(!enif_get_int(env, argv[4],  &level)) Badarg(5663,"level");
  weglFramebufferTexture1D(target,attachment,textarget,texture,level);
}

static void ecb_glFramebufferTexture2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLenum textarget;
  GLuint texture;
  GLint level;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5664,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5664,"attachment");
  if(!enif_get_uint(env, argv[2],  &textarget)) Badarg(5664,"textarget");
  if(!enif_get_uint(env, argv[3],  &texture)) Badarg(5664,"texture");
  if(!enif_get_int(env, argv[4],  &level)) Badarg(5664,"level");
  weglFramebufferTexture2D(target,attachment,textarget,texture,level);
}

static void ecb_glFramebufferTexture3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLenum textarget;
  GLuint texture;
  GLint level;
  GLint zoffset;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5665,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5665,"attachment");
  if(!enif_get_uint(env, argv[2],  &textarget)) Badarg(5665,"textarget");
  if(!enif_get_uint(env, argv[3],  &texture)) Badarg(5665,"texture");
  if(!enif_get_int(env, argv[4],  &level)) Badarg(5665,"level");
  if(!enif_get_int(env, argv[5],  &zoffset)) Badarg(5665,"zoffset");
  weglFramebufferTexture3D(target,attachment,textarget,texture,level,zoffset);
}

static void ecb_glFramebufferRenderbuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLenum renderbuffertarget;
  GLuint renderbuffer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5666,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5666,"attachment");
  if(!enif_get_uint(env, argv[2],  &renderbuffertarget)) Badarg(5666,"renderbuffertarget");
  if(!enif_get_uint(env, argv[3],  &renderbuffer)) Badarg(5666,"renderbuffer");
  weglFramebufferRenderbuffer(target,attachment,renderbuffertarget,renderbuffer);
}

static void ecb_glGetFramebufferAttachmentParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum attachment;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5667,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5667,"attachment");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5667,"pname");
  weglGetFramebufferAttachmentParameteriv(target,attachment,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGenerateMipmap(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5668,"target");
  weglGenerateMipmap(target);
}

static void ecb_glBlitFramebuffer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint srcX0;
  GLint srcY0;
  GLint srcX1;
  GLint srcY1;
  GLint dstX0;
  GLint dstY0;
  GLint dstX1;
  GLint dstY1;
  GLbitfield mask;
  GLenum filter;
  if(!enif_get_int(env, argv[0],  &srcX0)) Badarg(5669,"srcX0");
  if(!enif_get_int(env, argv[1],  &srcY0)) Badarg(5669,"srcY0");
  if(!enif_get_int(env, argv[2],  &srcX1)) Badarg(5669,"srcX1");
  if(!enif_get_int(env, argv[3],  &srcY1)) Badarg(5669,"srcY1");
  if(!enif_get_int(env, argv[4],  &dstX0)) Badarg(5669,"dstX0");
  if(!enif_get_int(env, argv[5],  &dstY0)) Badarg(5669,"dstY0");
  if(!enif_get_int(env, argv[6],  &dstX1)) Badarg(5669,"dstX1");
  if(!enif_get_int(env, argv[7],  &dstY1)) Badarg(5669,"dstY1");
  if(!enif_get_uint(env, argv[8],  &mask)) Badarg(5669,"mask");
  if(!enif_get_uint(env, argv[9],  &filter)) Badarg(5669,"filter");
  weglBlitFramebuffer(srcX0,srcY0,srcX1,srcY1,dstX0,dstY0,dstX1,dstY1,mask,filter);
}

static void ecb_glRenderbufferStorageMultisample(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei samples;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5670,"target");
  if(!enif_get_int(env, argv[1],  &samples)) Badarg(5670,"samples");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5670,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5670,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5670,"height");
  weglRenderbufferStorageMultisample(target,samples,internalformat,width,height);
}

static void ecb_glFramebufferTextureLayer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLuint texture;
  GLint level;
  GLint layer;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5671,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5671,"attachment");
  if(!enif_get_uint(env, argv[2],  &texture)) Badarg(5671,"texture");
  if(!enif_get_int(env, argv[3],  &level)) Badarg(5671,"level");
  if(!enif_get_int(env, argv[4],  &layer)) Badarg(5671,"layer");
  weglFramebufferTextureLayer(target,attachment,texture,level,layer);
}

static void ecb_glFramebufferTextureFaceARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLenum attachment;
  GLuint texture;
  GLint level;
  GLenum face;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5672,"target");
  if(!enif_get_uint(env, argv[1],  &attachment)) Badarg(5672,"attachment");
  if(!enif_get_uint(env, argv[2],  &texture)) Badarg(5672,"texture");
  if(!enif_get_int(env, argv[3],  &level)) Badarg(5672,"level");
  if(!enif_get_uint(env, argv[4],  &face)) Badarg(5672,"face");
  weglFramebufferTextureFaceARB(target,attachment,texture,level,face);
}

static void ecb_glFlushMappedBufferRange(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLintptr offset;
  GLsizeiptr length;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5673,"target");
  if(!enif_get_int64(env, argv[1],  &offset)) Badarg(5673,"offset");
  if(!enif_get_int64(env, argv[2],  &length)) Badarg(5673,"length");
  weglFlushMappedBufferRange(target,offset,length);
}

static void ecb_glBindVertexArray(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint array;
  if(!enif_get_uint(env, argv[0],  &array)) Badarg(5674,"array");
  weglBindVertexArray(array);
}

static void ecb_glDeleteVertexArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int arrays_free = 0;
  GLuint *arrays;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5675,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5675, "arrays"); }
  else {
    int i=0;
    ERL_NIF_TERM arrays_l, arrays_h, arrays_t;
    arrays_free = 1;
    arrays = (GLuint *) enif_alloc(n * sizeof(GLuint));
    arrays_l = argv[1];
    while(enif_get_list_cell(env, arrays_l, &arrays_h, &arrays_t)) {
        if(!enif_get_uint(env, arrays_h, &arrays[i++])) Badarg(5675,"arrays");
        arrays_l = arrays_t;
    };
  }
  weglDeleteVertexArrays(n,arrays);
 if(arrays_free) enif_free(arrays);
}

static void ecb_glGenVertexArrays(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * arrays;
  ERL_NIF_TERM *arrays_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5676,"n");
  arrays = enif_alloc((int)n*sizeof(GLuint));
  arrays_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenVertexArrays(n,arrays);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      arrays_ts[ri] =      enif_make_int(env, arrays[ri]);}
  reply =      enif_make_list_from_array(env, arrays_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(arrays_ts);
 enif_free(arrays);
}

static void ecb_glIsVertexArray(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint array;
  if(!enif_get_uint(env, argv[0],  &array)) Badarg(5677,"array");
  result = weglIsVertexArray(array);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetUniformIndices(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei uniformCount;
  GLchar ** uniformNames;
  ErlNifBinary uniformNames_all;
  GLchar *uniformNames_ptr;
  int i;
  GLuint * uniformIndices;
  ERL_NIF_TERM *uniformIndices_ts;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5678,"program");
  if(!enif_get_int(env, argv[1],  &uniformCount)) Badarg(5678,"uniformCount");
  if(!enif_inspect_binary(env, argv[2], &uniformNames_all)) Badarg(5678,"uniformNames");
  uniformNames = (GLchar **) enif_alloc(sizeof(GLchar *) * (int) argv[1]);
  uniformNames_ptr = (GLchar *) uniformNames_all.data;
  for(i=0; i < (int) argv[1]; i++) {     uniformNames[i] = (GLchar *) uniformNames_ptr;
     uniformNames_ptr += 1+strlen(uniformNames_ptr);
  }
  uniformIndices = enif_alloc((int)uniformCount*sizeof(GLuint));
  uniformIndices_ts = (ERL_NIF_TERM *) enif_alloc((int)uniformCount*sizeof(ERL_NIF_TERM));
  weglGetUniformIndices(program,uniformCount,(const GLchar **) uniformNames,uniformIndices);
  { int ri;
    for(ri=0; ri < (int) uniformCount; ri++)
      uniformIndices_ts[ri] =      enif_make_int(env, uniformIndices[ri]);}
  reply =      enif_make_list_from_array(env, uniformIndices_ts, uniformCount);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(uniformIndices_ts);
 enif_free(uniformIndices);
 enif_free(uniformNames);
}

static void ecb_glGetActiveUniformsiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei uniformCount;
  int uniformIndices_free = 0;
  GLuint *uniformIndices;
  GLenum pname;
  GLint * params;
  ERL_NIF_TERM *params_ts;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5679,"program");
  if(!enif_get_int(env, argv[1],  &uniformCount)) Badarg(5679,"uniformCount");
  if(!enif_is_list(env, argv[2])) { Badarg(5679, "uniformIndices"); }
  else {
    int i=0;
    ERL_NIF_TERM uniformIndices_l, uniformIndices_h, uniformIndices_t;
    uniformIndices_free = 1;
    uniformIndices = (GLuint *) enif_alloc(uniformCount * sizeof(GLuint));
    uniformIndices_l = argv[2];
    while(enif_get_list_cell(env, uniformIndices_l, &uniformIndices_h, &uniformIndices_t)) {
        if(!enif_get_uint(env, uniformIndices_h, &uniformIndices[i++])) Badarg(5679,"uniformIndices");
        uniformIndices_l = uniformIndices_t;
    };
  }
  if(!enif_get_uint(env, argv[3],  &pname)) Badarg(5679,"pname");
  params = enif_alloc((int)uniformCount*sizeof(GLint));
  params_ts = (ERL_NIF_TERM *) enif_alloc((int)uniformCount*sizeof(ERL_NIF_TERM));
  weglGetActiveUniformsiv(program,uniformCount,uniformIndices,pname,params);
  { int ri;
    for(ri=0; ri < (int) uniformCount; ri++)
      params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, uniformCount);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(params_ts);
 enif_free(params);
 if(uniformIndices_free) enif_free(uniformIndices);
}

static void ecb_glGetActiveUniformName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint uniformIndex;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *uniformName;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5680,"program");
  if(!enif_get_uint(env, argv[1],  &uniformIndex)) Badarg(5680,"uniformIndex");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5680,"bufSize");
  uniformName = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetActiveUniformName(program,uniformIndex,bufSize,&length,(GLchar *) uniformName);
  reply =      enif_make_string(env, (const char *) uniformName, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(uniformName);
}

static void ecb_glGetUniformBlockIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary uniformBlockName;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5681,"program");
  if(!enif_inspect_binary(env, argv[1], &uniformBlockName)) Badarg(5681,"uniformBlockName");
  result = weglGetUniformBlockIndex(program,(GLchar *) uniformBlockName.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetActiveUniformBlockiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint uniformBlockIndex;
  GLenum pname;
  ErlNifBinary params;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5682,"program");
  if(!enif_get_uint(env, argv[1],  &uniformBlockIndex)) Badarg(5682,"uniformBlockIndex");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5682,"pname");
  if(enif_is_binary(env, argv[3]))
    enif_inspect_binary(env, argv[3], &params);
  else if(enif_is_tuple(env, argv[3])) {
    int params_a;
    const ERL_NIF_TERM *params_t;
    if(enif_get_tuple(env, argv[3], &params_a, &params_t) ||
         enif_is_binary(env, params_t[1]))
       enif_inspect_binary(env, params_t[1], &params);
    else Badarg(5682, "params");
  } else Badarg(5682, "params");
  weglGetActiveUniformBlockiv(program,uniformBlockIndex,pname,(GLint *) params.data);
  enif_send(NULL, self, env,
    enif_make_tuple2(env,EGL_ATOM_REPLY,
                         EGL_ATOM_OK));
}

static void ecb_glGetActiveUniformBlockName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLuint uniformBlockIndex;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *uniformBlockName;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5683,"program");
  if(!enif_get_uint(env, argv[1],  &uniformBlockIndex)) Badarg(5683,"uniformBlockIndex");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5683,"bufSize");
  uniformBlockName = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetActiveUniformBlockName(program,uniformBlockIndex,bufSize,&length,(GLchar *) uniformBlockName);
  reply =      enif_make_string(env, (const char *) uniformBlockName, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(uniformBlockName);
}

static void ecb_glUniformBlockBinding(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint uniformBlockIndex;
  GLuint uniformBlockBinding;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5684,"program");
  if(!enif_get_uint(env, argv[1],  &uniformBlockIndex)) Badarg(5684,"uniformBlockIndex");
  if(!enif_get_uint(env, argv[2],  &uniformBlockBinding)) Badarg(5684,"uniformBlockBinding");
  weglUniformBlockBinding(program,uniformBlockIndex,uniformBlockBinding);
}

static void ecb_glCopyBufferSubData(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum readTarget;
  GLenum writeTarget;
  GLintptr readOffset;
  GLintptr writeOffset;
  GLsizeiptr size;
  if(!enif_get_uint(env, argv[0],  &readTarget)) Badarg(5685,"readTarget");
  if(!enif_get_uint(env, argv[1],  &writeTarget)) Badarg(5685,"writeTarget");
  if(!enif_get_int64(env, argv[2],  &readOffset)) Badarg(5685,"readOffset");
  if(!enif_get_int64(env, argv[3],  &writeOffset)) Badarg(5685,"writeOffset");
  if(!enif_get_int64(env, argv[4],  &size)) Badarg(5685,"size");
  weglCopyBufferSubData(readTarget,writeTarget,readOffset,writeOffset,size);
}

static void ecb_glDrawElementsBaseVertex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  GLvoid *indices_idx;
  GLint basevertex;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5686,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5686,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5686,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (GLvoid *) indices.data;
    else Badarg(5686,"indices");
  }
  if(!enif_get_int(env, argv[4],  &basevertex)) Badarg(5686,"basevertex");
  weglDrawElementsBaseVertex(mode,count,type,indices_idx,basevertex);
}

static void ecb_glDrawRangeElementsBaseVertex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint start;
  GLuint end;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  GLvoid *indices_idx;
  GLint basevertex;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5688,"mode");
  if(!enif_get_uint(env, argv[1],  &start)) Badarg(5688,"start");
  if(!enif_get_uint(env, argv[2],  &end)) Badarg(5688,"end");
  if(!enif_get_int(env, argv[3],  &count)) Badarg(5688,"count");
  if(!enif_get_uint(env, argv[4],  &type)) Badarg(5688,"type");
  if(!egl_get_ptr(env, argv[5], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[5], &indices))
        indices_idx = (GLvoid *) indices.data;
    else Badarg(5688,"indices");
  }
  if(!enif_get_int(env, argv[6],  &basevertex)) Badarg(5688,"basevertex");
  weglDrawRangeElementsBaseVertex(mode,start,end,count,type,indices_idx,basevertex);
}

static void ecb_glDrawElementsInstancedBaseVertex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  GLvoid *indices_idx;
  GLsizei primcount;
  GLint basevertex;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5690,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5690,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5690,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (GLvoid *) indices.data;
    else Badarg(5690,"indices");
  }
  if(!enif_get_int(env, argv[4],  &primcount)) Badarg(5690,"primcount");
  if(!enif_get_int(env, argv[5],  &basevertex)) Badarg(5690,"basevertex");
  weglDrawElementsInstancedBaseVertex(mode,count,type,indices_idx,primcount,basevertex);
}

static void ecb_glProvokingVertex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5692,"mode");
  weglProvokingVertex(mode);
}

static void ecb_glFenceSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsync result;
  ERL_NIF_TERM reply;
  GLenum condition;
  GLbitfield flags;
  if(!enif_get_uint(env, argv[0],  &condition)) Badarg(5693,"condition");
  if(!enif_get_uint(env, argv[1],  &flags)) Badarg(5693,"flags");
  result = weglFenceSync(condition,flags);
  reply =      enif_make_uint64(env, (uint64_t) result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glIsSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  uint64_t sync;
  if(!egl_get_ptr(env, argv[0], (void *) &sync)) Badarg(5694,"sync");
  result = weglIsSync((GLsync) sync);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDeleteSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t sync;
  if(!egl_get_ptr(env, argv[0], (void *) &sync)) Badarg(5695,"sync");
  weglDeleteSync((GLsync) sync);
}

static void ecb_glClientWaitSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum result;
  ERL_NIF_TERM reply;
  uint64_t sync;
  GLbitfield flags;
  GLuint64 timeout;
  if(!egl_get_ptr(env, argv[0], (void *) &sync)) Badarg(5696,"sync");
  if(!enif_get_uint(env, argv[1],  &flags)) Badarg(5696,"flags");
  if(!enif_get_uint64(env, argv[2],  &timeout)) Badarg(5696,"timeout");
  result = weglClientWaitSync((GLsync) sync,flags,timeout);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glWaitSync(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  uint64_t sync;
  GLbitfield flags;
  GLuint64 timeout;
  if(!egl_get_ptr(env, argv[0], (void *) &sync)) Badarg(5697,"sync");
  if(!enif_get_uint(env, argv[1],  &flags)) Badarg(5697,"flags");
  if(!enif_get_uint64(env, argv[2],  &timeout)) Badarg(5697,"timeout");
  weglWaitSync((GLsync) sync,flags,timeout);
}

static void ecb_glGetInteger64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLint64 params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5698,"pname");
  weglGetInteger64v(pname,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int64(env, (int64_t) params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetSynciv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  uint64_t sync;
  GLenum pname;
  GLsizei bufSize;
  GLsizei length;
  GLint * values;
  ERL_NIF_TERM *values_ts;
  if(!egl_get_ptr(env, argv[0], (void *) &sync)) Badarg(5699,"sync");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5699,"pname");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5699,"bufSize");
  values = enif_alloc((int)bufSize*sizeof(GLint));
  values_ts = (ERL_NIF_TERM *) enif_alloc((int)bufSize*sizeof(ERL_NIF_TERM));
  weglGetSynciv((GLsync) sync,pname,bufSize,&length,values);
  { int ri;
    for(ri=0; ri < (int) length; ri++)
      values_ts[ri] =      enif_make_int(env, values[ri]);}
  reply =      enif_make_list_from_array(env, values_ts, length);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(values_ts);
 enif_free(values);
}

static void ecb_glTexImage2DMultisample(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei samples;
  GLint internalformat;
  GLsizei width;
  GLsizei height;
  GLboolean fixedsamplelocations;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5700,"target");
  if(!enif_get_int(env, argv[1],  &samples)) Badarg(5700,"samples");
  if(!enif_get_int(env, argv[2],  &internalformat)) Badarg(5700,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5700,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5700,"height");
  if(!egl_get_ubyte(env, argv[5],  &fixedsamplelocations)) Badarg(5700,"fixedsamplelocations");
  weglTexImage2DMultisample(target,samples,internalformat,width,height,fixedsamplelocations);
}

static void ecb_glTexImage3DMultisample(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei samples;
  GLint internalformat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  GLboolean fixedsamplelocations;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5701,"target");
  if(!enif_get_int(env, argv[1],  &samples)) Badarg(5701,"samples");
  if(!enif_get_int(env, argv[2],  &internalformat)) Badarg(5701,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5701,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5701,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5701,"depth");
  if(!egl_get_ubyte(env, argv[6],  &fixedsamplelocations)) Badarg(5701,"fixedsamplelocations");
  weglTexImage3DMultisample(target,samples,internalformat,width,height,depth,fixedsamplelocations);
}

static void ecb_glGetMultisamplefv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum pname;
  GLuint index;
  GLfloat val[2];
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5702,"pname");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5702,"index");
  weglGetMultisamplefv(pname,index,val);
  reply =      enif_make_tuple2(env,
     enif_make_double(env, (double) val[0]),
            enif_make_double(env, (double) val[1]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glSampleMaski(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLbitfield mask;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5703,"index");
  if(!enif_get_uint(env, argv[1],  &mask)) Badarg(5703,"mask");
  weglSampleMaski(index,mask);
}

static void ecb_glNamedStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum type;
  ErlNifBinary name;
  ErlNifBinary string;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5704,"type");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5704,"name");
  if(!enif_inspect_binary(env, argv[4], &string)) Badarg(5704,"string");
  weglNamedStringARB(type,(GLint) name.size,(GLchar *) name.data,(GLint) string.size,(GLchar *) string.data);
}

static void ecb_glDeleteNamedStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ErlNifBinary name;
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5705,"name");
  weglDeleteNamedStringARB((GLint) name.size,(GLchar *) name.data);
}

static void ecb_glCompileShaderIncludeARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint shader;
  GLsizei count;
  GLchar ** path;
  ErlNifBinary path_all;
  GLchar *path_ptr;
  int i;
  if(!enif_get_uint(env, argv[0],  &shader)) Badarg(5706,"shader");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5706,"count");
  if(!enif_inspect_binary(env, argv[2], &path_all)) Badarg(5706,"path");
  path = (GLchar **) enif_alloc(sizeof(GLchar *) * (int) argv[1]);
  path_ptr = (GLchar *) path_all.data;
  for(i=0; i < (int) argv[1]; i++) {     path[i] = (GLchar *) path_ptr;
     path_ptr += 1+strlen(path_ptr);
  }
  weglCompileShaderIncludeARB(shader,count,(const GLchar **) path,NULL);
 enif_free(path);
}

static void ecb_glIsNamedStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  ErlNifBinary name;
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5707,"name");
  result = weglIsNamedStringARB((GLint) name.size,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetNamedStringARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  ErlNifBinary name;
  GLsizei bufSize;
  GLint stringlen;
  unsigned char *string;
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5708,"name");
  if(!enif_get_int(env, argv[2],  &bufSize)) Badarg(5708,"bufSize");
  string = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetNamedStringARB((GLint) name.size,(GLchar *) name.data,bufSize,&stringlen,(GLchar *) string);
  reply =      enif_make_string(env, (const char *) string, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(string);
}

static void ecb_glGetNamedStringivARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  ErlNifBinary name;
  GLenum pname;
  GLint params;
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5709,"name");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5709,"pname");
  weglGetNamedStringivARB((GLint) name.size,(GLchar *) name.data,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBindFragDataLocationIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLuint colorNumber;
  GLuint index;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5710,"program");
  if(!enif_get_uint(env, argv[1],  &colorNumber)) Badarg(5710,"colorNumber");
  if(!enif_get_uint(env, argv[2],  &index)) Badarg(5710,"index");
  if(!enif_inspect_binary(env, argv[3], &name)) Badarg(5710,"name");
  weglBindFragDataLocationIndexed(program,colorNumber,index,(GLchar *) name.data);
}

static void ecb_glGetFragDataIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5711,"program");
  if(!enif_inspect_binary(env, argv[1], &name)) Badarg(5711,"name");
  result = weglGetFragDataIndex(program,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGenSamplers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei count;
  GLuint * samplers;
  ERL_NIF_TERM *samplers_ts;
  if(!enif_get_int(env, argv[0],  &count)) Badarg(5712,"count");
  samplers = enif_alloc((int)count*sizeof(GLuint));
  samplers_ts = (ERL_NIF_TERM *) enif_alloc((int)count*sizeof(ERL_NIF_TERM));
  weglGenSamplers(count,samplers);
  { int ri;
    for(ri=0; ri < (int) count; ri++)
      samplers_ts[ri] =      enif_make_int(env, samplers[ri]);}
  reply =      enif_make_list_from_array(env, samplers_ts, count);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(samplers_ts);
 enif_free(samplers);
}

static void ecb_glDeleteSamplers(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei count;
  int samplers_free = 0;
  GLuint *samplers;
  if(!enif_get_int(env, argv[0],  &count)) Badarg(5713,"count");
  if(!enif_is_list(env, argv[1])) { Badarg(5713, "samplers"); }
  else {
    int i=0;
    ERL_NIF_TERM samplers_l, samplers_h, samplers_t;
    samplers_free = 1;
    samplers = (GLuint *) enif_alloc(count * sizeof(GLuint));
    samplers_l = argv[1];
    while(enif_get_list_cell(env, samplers_l, &samplers_h, &samplers_t)) {
        if(!enif_get_uint(env, samplers_h, &samplers[i++])) Badarg(5713,"samplers");
        samplers_l = samplers_t;
    };
  }
  weglDeleteSamplers(count,samplers);
 if(samplers_free) enif_free(samplers);
}

static void ecb_glIsSampler(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint sampler;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5714,"sampler");
  result = weglIsSampler(sampler);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glBindSampler(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint unit;
  GLuint sampler;
  if(!enif_get_uint(env, argv[0],  &unit)) Badarg(5715,"unit");
  if(!enif_get_uint(env, argv[1],  &sampler)) Badarg(5715,"sampler");
  weglBindSampler(unit,sampler);
}

static void ecb_glSamplerParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  GLint param;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5716,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5716,"pname");
  if(!enif_get_int(env, argv[2],  &param)) Badarg(5716,"param");
  weglSamplerParameteri(sampler,pname,param);
}

static void ecb_glSamplerParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  int param_free = 0;
  GLint *param;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5717,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5717,"pname");
  if(!enif_is_list(env, argv[2])) { Badarg(5717, "param"); }
  else {
    int i=0;
    ERL_NIF_TERM param_l, param_h, param_t;
    unsigned int param_len;
    enif_get_list_length(env, argv[2], &param_len);
    param_free = 1;
    param = (GLint *) enif_alloc(param_len * sizeof(GLint));
    param_l = argv[2];
    while(enif_get_list_cell(env, param_l, &param_h, &param_t)) {
        if(!enif_get_int(env, param_h, &param[i++])) Badarg(5717,"param");
        param_l = param_t;
    };
  }
  weglSamplerParameteriv(sampler,pname,param);
 if(param_free) enif_free(param);
}

static void ecb_glSamplerParameterf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  GLfloat param;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5718,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5718,"pname");
  if(!egl_get_float(env, argv[2],  &param)) Badarg(5718,"param");
  weglSamplerParameterf(sampler,pname,param);
}

static void ecb_glSamplerParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  int param_free = 0;
  GLfloat *param;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5719,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5719,"pname");
  if(!enif_is_list(env, argv[2])) { Badarg(5719, "param"); }
  else {
    int i=0;
    ERL_NIF_TERM param_l, param_h, param_t;
    unsigned int param_len;
    enif_get_list_length(env, argv[2], &param_len);
    param_free = 1;
    param = (GLfloat *) enif_alloc(param_len * sizeof(GLfloat));
    param_l = argv[2];
    while(enif_get_list_cell(env, param_l, &param_h, &param_t)) {
        if(!egl_get_float(env, param_h, &param[i++])) Badarg(5719,"param");
        param_l = param_t;
    };
  }
  weglSamplerParameterfv(sampler,pname,param);
 if(param_free) enif_free(param);
}

static void ecb_glSamplerParameterIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  int param_free = 0;
  GLint *param;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5720,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5720,"pname");
  if(!enif_is_list(env, argv[2])) { Badarg(5720, "param"); }
  else {
    int i=0;
    ERL_NIF_TERM param_l, param_h, param_t;
    unsigned int param_len;
    enif_get_list_length(env, argv[2], &param_len);
    param_free = 1;
    param = (GLint *) enif_alloc(param_len * sizeof(GLint));
    param_l = argv[2];
    while(enif_get_list_cell(env, param_l, &param_h, &param_t)) {
        if(!enif_get_int(env, param_h, &param[i++])) Badarg(5720,"param");
        param_l = param_t;
    };
  }
  weglSamplerParameterIiv(sampler,pname,param);
 if(param_free) enif_free(param);
}

static void ecb_glSamplerParameterIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint sampler;
  GLenum pname;
  int param_free = 0;
  GLuint *param;
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5721,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5721,"pname");
  if(!enif_is_list(env, argv[2])) { Badarg(5721, "param"); }
  else {
    int i=0;
    ERL_NIF_TERM param_l, param_h, param_t;
    unsigned int param_len;
    enif_get_list_length(env, argv[2], &param_len);
    param_free = 1;
    param = (GLuint *) enif_alloc(param_len * sizeof(GLuint));
    param_l = argv[2];
    while(enif_get_list_cell(env, param_l, &param_h, &param_t)) {
        if(!enif_get_uint(env, param_h, &param[i++])) Badarg(5721,"param");
        param_l = param_t;
    };
  }
  weglSamplerParameterIuiv(sampler,pname,param);
 if(param_free) enif_free(param);
}

static void ecb_glGetSamplerParameteriv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint sampler;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5722,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5722,"pname");
  weglGetSamplerParameteriv(sampler,pname,params);
  reply =      enif_make_list4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetSamplerParameterIiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint sampler;
  GLenum pname;
  GLint params[4];
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5723,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5723,"pname");
  weglGetSamplerParameterIiv(sampler,pname,params);
  reply =      enif_make_list4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetSamplerParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint sampler;
  GLenum pname;
  GLfloat params[4];
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5724,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5724,"pname");
  weglGetSamplerParameterfv(sampler,pname,params);
  reply =      enif_make_list4(env,
     enif_make_double(env, (double) params[0]),
            enif_make_double(env, (double) params[1]),
            enif_make_double(env, (double) params[2]),
            enif_make_double(env, (double) params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetSamplerParameterIuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint sampler;
  GLenum pname;
  GLuint params[4];
  if(!enif_get_uint(env, argv[0],  &sampler)) Badarg(5725,"sampler");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5725,"pname");
  weglGetSamplerParameterIuiv(sampler,pname,params);
  reply =      enif_make_list4(env,
     enif_make_int(env, params[0]),
            enif_make_int(env, params[1]),
            enif_make_int(env, params[2]),
            enif_make_int(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glQueryCounter(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint id;
  GLenum target;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5726,"id");
  if(!enif_get_uint(env, argv[1],  &target)) Badarg(5726,"target");
  weglQueryCounter(id,target);
}

static void ecb_glGetQueryObjecti64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint id;
  GLenum pname;
  GLint64 params;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5727,"id");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5727,"pname");
  weglGetQueryObjecti64v(id,pname,&params);
  reply =      enif_make_int64(env, (int64_t) params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetQueryObjectui64v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint id;
  GLenum pname;
  GLuint64 params;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5728,"id");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5728,"pname");
  weglGetQueryObjectui64v(id,pname,&params);
  reply =      enif_make_int64(env, (int64_t) params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDrawArraysIndirect(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  ErlNifBinary indirect;
  GLvoid *indirect_idx;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5729,"mode");
  if(!egl_get_ptr(env, argv[1], (void **) &indirect_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[1], &indirect))
        indirect_idx = (GLvoid *) indirect.data;
    else Badarg(5729,"indirect");
  }
  weglDrawArraysIndirect(mode,indirect_idx);
}

static void ecb_glDrawElementsIndirect(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLenum type;
  ErlNifBinary indirect;
  GLvoid *indirect_idx;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5731,"mode");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5731,"type");
  if(!egl_get_ptr(env, argv[2], (void **) &indirect_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[2], &indirect))
        indirect_idx = (GLvoid *) indirect.data;
    else Badarg(5731,"indirect");
  }
  weglDrawElementsIndirect(mode,type,indirect_idx);
}

static void ecb_glUniform1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLdouble x;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5733,"location");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5733,"x");
  weglUniform1d(location,x);
}

static void ecb_glUniform2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLdouble x;
  GLdouble y;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5734,"location");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5734,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5734,"y");
  weglUniform2d(location,x,y);
}

static void ecb_glUniform3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5735,"location");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5735,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5735,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5735,"z");
  weglUniform3d(location,x,y,z);
}

static void ecb_glUniform4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5736,"location");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5736,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5736,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5736,"z");
  if(!enif_get_double(env, argv[4],  &w)) Badarg(5736,"w");
  weglUniform4d(location,x,y,z,w);
}

static void ecb_glUniform1dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  int value_free = 0;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5737,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5737,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5737, "value"); }
  else {
    int i=0;
    ERL_NIF_TERM value_l, value_h, value_t;
    value_free = 1;
    value = (GLdouble *) enif_alloc(count * sizeof(GLdouble));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_double(env, value_h, &value[i++])) Badarg(5737,"value");
        value_l = value_t;
    };
  }
  weglUniform1dv(location,count,value);
 if(value_free) enif_free(value);
}

static void ecb_glUniform2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5738,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5738,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5738,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(2*count*sizeof(GLdouble));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) {
            Badarg(5738,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5738,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5738,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform2dv(location,count,value);
}

static void ecb_glUniform3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5739,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5739,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5739,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(3*count*sizeof(GLdouble));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) {
            Badarg(5739,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5739,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5739,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5739,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform3dv(location,count,value);
}

static void ecb_glUniform4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5740,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5740,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5740,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(4*count*sizeof(GLdouble));
    value_l = argv[2];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5740,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5740,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5740,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5740,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5740,"value");
        };
        value_l = value_t;
    };
  }
  weglUniform4dv(location,count,value);
}

static void ecb_glUniformMatrix2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5741,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5741,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5741,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5741,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(4*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5741,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5741,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5741,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5741,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5741,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix2dv(location,count,transpose,value);
}

static void ecb_glUniformMatrix3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5742,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5742,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5742,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5742,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(9*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 9) {
            Badarg(5742,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5742,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5742,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5742,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5742,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5742,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5742,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5742,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5742,"value");
        if(!enif_get_double(env, value_tpl[8], &value[8])) Badarg(5742,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix3dv(location,count,transpose,value);
}

static void ecb_glUniformMatrix4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5743,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5743,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5743,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5743,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(16*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 16) {
            Badarg(5743,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[8], &value[8])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[9], &value[9])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[10], &value[10])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[11], &value[11])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[12], &value[12])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[13], &value[13])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[14], &value[14])) Badarg(5743,"value");
        if(!enif_get_double(env, value_tpl[15], &value[15])) Badarg(5743,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix4dv(location,count,transpose,value);
}

static void ecb_glUniformMatrix2x3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5744,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5744,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5744,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5744,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(6*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) {
            Badarg(5744,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5744,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5744,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5744,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5744,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5744,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5744,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix2x3dv(location,count,transpose,value);
}

static void ecb_glUniformMatrix2x4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5745,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5745,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5745,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5745,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(8*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) {
            Badarg(5745,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5745,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5745,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5745,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5745,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5745,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5745,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5745,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5745,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix2x4dv(location,count,transpose,value);
}

static void ecb_glUniformMatrix3x2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5746,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5746,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5746,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5746,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(6*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) {
            Badarg(5746,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5746,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5746,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5746,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5746,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5746,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5746,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix3x2dv(location,count,transpose,value);
}

static void ecb_glUniformMatrix3x4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5747,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5747,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5747,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5747,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(12*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) {
            Badarg(5747,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[8], &value[8])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[9], &value[9])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[10], &value[10])) Badarg(5747,"value");
        if(!enif_get_double(env, value_tpl[11], &value[11])) Badarg(5747,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix3x4dv(location,count,transpose,value);
}

static void ecb_glUniformMatrix4x2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5748,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5748,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5748,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5748,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(8*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) {
            Badarg(5748,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5748,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5748,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5748,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5748,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5748,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5748,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5748,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5748,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix4x2dv(location,count,transpose,value);
}

static void ecb_glUniformMatrix4x3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_int(env, argv[0],  &location)) Badarg(5749,"location");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5749,"count");
  if(!egl_get_ubyte(env, argv[2],  &transpose)) Badarg(5749,"transpose");
  if(!enif_is_list(env, argv[3])) { Badarg(5749,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(12*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) {
            Badarg(5749,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[8], &value[8])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[9], &value[9])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[10], &value[10])) Badarg(5749,"value");
        if(!enif_get_double(env, value_tpl[11], &value[11])) Badarg(5749,"value");
        };
        value_l = value_t;
    };
  }
  weglUniformMatrix4x3dv(location,count,transpose,value);
}

static void ecb_glGetUniformdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLint location;
  GLdouble params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5750,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5750,"location");
  weglGetUniformdv(program,location,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_double(env, params[ri]);}
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetSubroutineUniformLocation(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLint result;
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5751,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5751,"shadertype");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5751,"name");
  result = weglGetSubroutineUniformLocation(program,shadertype,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetSubroutineIndex(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  ErlNifBinary name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5752,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5752,"shadertype");
  if(!enif_inspect_binary(env, argv[2], &name)) Badarg(5752,"name");
  result = weglGetSubroutineIndex(program,shadertype,(GLchar *) name.data);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetActiveSubroutineUniformName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  GLuint index;
  GLsizei bufsize;
  GLsizei length;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5753,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5753,"shadertype");
  if(!enif_get_uint(env, argv[2],  &index)) Badarg(5753,"index");
  if(!enif_get_int(env, argv[3],  &bufsize)) Badarg(5753,"bufsize");
  name = enif_alloc((int) bufsize*sizeof(GLchar));
  weglGetActiveSubroutineUniformName(program,shadertype,index,bufsize,&length,(GLchar *) name);
  reply =      enif_make_string(env, (const char *) name, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

static void ecb_glGetActiveSubroutineName(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  GLuint index;
  GLsizei bufsize;
  GLsizei length;
  unsigned char *name;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5754,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5754,"shadertype");
  if(!enif_get_uint(env, argv[2],  &index)) Badarg(5754,"index");
  if(!enif_get_int(env, argv[3],  &bufsize)) Badarg(5754,"bufsize");
  name = enif_alloc((int) bufsize*sizeof(GLchar));
  weglGetActiveSubroutineName(program,shadertype,index,bufsize,&length,(GLchar *) name);
  reply =      enif_make_string(env, (const char *) name, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(name);
}

static void ecb_glUniformSubroutinesuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum shadertype;
  GLsizei count;
  int indices_free = 0;
  GLuint *indices;
  if(!enif_get_uint(env, argv[0],  &shadertype)) Badarg(5755,"shadertype");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5755,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5755, "indices"); }
  else {
    int i=0;
    ERL_NIF_TERM indices_l, indices_h, indices_t;
    indices_free = 1;
    indices = (GLuint *) enif_alloc(count * sizeof(GLuint));
    indices_l = argv[2];
    while(enif_get_list_cell(env, indices_l, &indices_h, &indices_t)) {
        if(!enif_get_uint(env, indices_h, &indices[i++])) Badarg(5755,"indices");
        indices_l = indices_t;
    };
  }
  weglUniformSubroutinesuiv(shadertype,count,indices);
 if(indices_free) enif_free(indices);
}

static void ecb_glGetUniformSubroutineuiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum shadertype;
  GLint location;
  GLuint params[16];
  ERL_NIF_TERM params_ts[16];
  if(!enif_get_uint(env, argv[0],  &shadertype)) Badarg(5756,"shadertype");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5756,"location");
  weglGetUniformSubroutineuiv(shadertype,location,params);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_tuple_from_array(env, params_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetProgramStageiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLenum shadertype;
  GLenum pname;
  GLint values;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5757,"program");
  if(!enif_get_uint(env, argv[1],  &shadertype)) Badarg(5757,"shadertype");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5757,"pname");
  weglGetProgramStageiv(program,shadertype,pname,&values);
  reply =      enif_make_int(env, values);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glPatchParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  GLint value;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5758,"pname");
  if(!enif_get_int(env, argv[1],  &value)) Badarg(5758,"value");
  weglPatchParameteri(pname,value);
}

static void ecb_glPatchParameterfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum pname;
  int values_free = 0;
  GLfloat *values;
  if(!enif_get_uint(env, argv[0],  &pname)) Badarg(5759,"pname");
  if(!enif_is_list(env, argv[1])) { Badarg(5759, "values"); }
  else {
    int i=0;
    ERL_NIF_TERM values_l, values_h, values_t;
    unsigned int values_len;
    enif_get_list_length(env, argv[1], &values_len);
    values_free = 1;
    values = (GLfloat *) enif_alloc(values_len * sizeof(GLfloat));
    values_l = argv[1];
    while(enif_get_list_cell(env, values_l, &values_h, &values_t)) {
        if(!egl_get_float(env, values_h, &values[i++])) Badarg(5759,"values");
        values_l = values_t;
    };
  }
  weglPatchParameterfv(pname,values);
 if(values_free) enif_free(values);
}

static void ecb_glBindTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5760,"target");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5760,"id");
  weglBindTransformFeedback(target,id);
}

static void ecb_glDeleteTransformFeedbacks(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int ids_free = 0;
  GLuint *ids;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5761,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5761, "ids"); }
  else {
    int i=0;
    ERL_NIF_TERM ids_l, ids_h, ids_t;
    ids_free = 1;
    ids = (GLuint *) enif_alloc(n * sizeof(GLuint));
    ids_l = argv[1];
    while(enif_get_list_cell(env, ids_l, &ids_h, &ids_t)) {
        if(!enif_get_uint(env, ids_h, &ids[i++])) Badarg(5761,"ids");
        ids_l = ids_t;
    };
  }
  weglDeleteTransformFeedbacks(n,ids);
 if(ids_free) enif_free(ids);
}

static void ecb_glGenTransformFeedbacks(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * ids;
  ERL_NIF_TERM *ids_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5762,"n");
  ids = enif_alloc((int)n*sizeof(GLuint));
  ids_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenTransformFeedbacks(n,ids);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      ids_ts[ri] =      enif_make_int(env, ids[ri]);}
  reply =      enif_make_list_from_array(env, ids_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(ids_ts);
 enif_free(ids);
}

static void ecb_glIsTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &id)) Badarg(5763,"id");
  result = weglIsTransformFeedback(id);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glPauseTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglPauseTransformFeedback();
}

static void ecb_glResumeTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglResumeTransformFeedback();
}

static void ecb_glDrawTransformFeedback(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5766,"mode");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5766,"id");
  weglDrawTransformFeedback(mode,id);
}

static void ecb_glDrawTransformFeedbackStream(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint id;
  GLuint stream;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5767,"mode");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5767,"id");
  if(!enif_get_uint(env, argv[2],  &stream)) Badarg(5767,"stream");
  weglDrawTransformFeedbackStream(mode,id,stream);
}

static void ecb_glBeginQueryIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  GLuint id;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5768,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5768,"index");
  if(!enif_get_uint(env, argv[2],  &id)) Badarg(5768,"id");
  weglBeginQueryIndexed(target,index,id);
}

static void ecb_glEndQueryIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLuint index;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5769,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5769,"index");
  weglEndQueryIndexed(target,index);
}

static void ecb_glGetQueryIndexediv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5770,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5770,"index");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5770,"pname");
  weglGetQueryIndexediv(target,index,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glReleaseShaderCompiler(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  weglReleaseShaderCompiler();
}

static void ecb_glShaderBinary(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei count;
  int shaders_free = 0;
  GLuint *shaders;
  GLenum binaryformat;
  ErlNifBinary binary;
  if(!enif_get_int(env, argv[0],  &count)) Badarg(5772,"count");
  if(!enif_is_list(env, argv[1])) { Badarg(5772, "shaders"); }
  else {
    int i=0;
    ERL_NIF_TERM shaders_l, shaders_h, shaders_t;
    shaders_free = 1;
    shaders = (GLuint *) enif_alloc(count * sizeof(GLuint));
    shaders_l = argv[1];
    while(enif_get_list_cell(env, shaders_l, &shaders_h, &shaders_t)) {
        if(!enif_get_uint(env, shaders_h, &shaders[i++])) Badarg(5772,"shaders");
        shaders_l = shaders_t;
    };
  }
  if(!enif_get_uint(env, argv[2],  &binaryformat)) Badarg(5772,"binaryformat");
  if(!enif_inspect_binary(env, argv[3], &binary)) Badarg(5772,"binary");
  weglShaderBinary(count,shaders,binaryformat,(GLvoid *) binary.data,(GLsizei) binary.size);
 if(shaders_free) enif_free(shaders);
}

static void ecb_glGetShaderPrecisionFormat(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum shadertype;
  GLenum precisiontype;
  GLint range[2];
  GLint precision;
  if(!enif_get_uint(env, argv[0],  &shadertype)) Badarg(5773,"shadertype");
  if(!enif_get_uint(env, argv[1],  &precisiontype)) Badarg(5773,"precisiontype");
  weglGetShaderPrecisionFormat(shadertype,precisiontype,range,&precision);
  reply = enif_make_tuple2(env,
          enif_make_tuple2(env,
     enif_make_int(env, range[0]),
            enif_make_int(env, range[1])),
     enif_make_int(env, precision) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDepthRangef(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf n;
  GLclampf f;
  if(!egl_get_float(env, argv[0],  &n)) Badarg(5774,"n");
  if(!egl_get_float(env, argv[1],  &f)) Badarg(5774,"f");
  weglDepthRangef(n,f);
}

static void ecb_glClearDepthf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampf d;
  if(!egl_get_float(env, argv[0],  &d)) Badarg(5775,"d");
  weglClearDepthf(d);
}

static void ecb_glGetProgramBinary(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint program;
  GLsizei bufSize;
  GLsizei length;
  GLenum binaryFormat;
  ErlNifBinary binary;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5776,"program");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5776,"bufSize");
  enif_alloc_binary((int) bufSize*sizeof(GLvoid), &binary);
  weglGetProgramBinary(program,bufSize,&length,&binaryFormat,(GLvoid *) binary.data);
  reply = enif_make_tuple2(env,
          enif_make_int(env, binaryFormat),
     enif_make_binary(env, &binary) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glProgramBinary(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLenum binaryFormat;
  ErlNifBinary binary;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5777,"program");
  if(!enif_get_uint(env, argv[1],  &binaryFormat)) Badarg(5777,"binaryFormat");
  if(!enif_inspect_binary(env, argv[2], &binary)) Badarg(5777,"binary");
  weglProgramBinary(program,binaryFormat,(GLvoid *) binary.data,(GLsizei) binary.size);
}

static void ecb_glProgramParameteri(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLenum pname;
  GLint value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5778,"program");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5778,"pname");
  if(!enif_get_int(env, argv[2],  &value)) Badarg(5778,"value");
  weglProgramParameteri(program,pname,value);
}

static void ecb_glUseProgramStages(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint pipeline;
  GLbitfield stages;
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5779,"pipeline");
  if(!enif_get_uint(env, argv[1],  &stages)) Badarg(5779,"stages");
  if(!enif_get_uint(env, argv[2],  &program)) Badarg(5779,"program");
  weglUseProgramStages(pipeline,stages,program);
}

static void ecb_glActiveShaderProgram(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint pipeline;
  GLuint program;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5780,"pipeline");
  if(!enif_get_uint(env, argv[1],  &program)) Badarg(5780,"program");
  weglActiveShaderProgram(pipeline,program);
}

static void ecb_glCreateShaderProgramv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLenum type;
  GLsizei count;
  GLchar ** strings;
  ErlNifBinary strings_all;
  GLchar *strings_ptr;
  int i;
  if(!enif_get_uint(env, argv[0],  &type)) Badarg(5781,"type");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5781,"count");
  if(!enif_inspect_binary(env, argv[2], &strings_all)) Badarg(5781,"strings");
  strings = (GLchar **) enif_alloc(sizeof(GLchar *) * (int) argv[1]);
  strings_ptr = (GLchar *) strings_all.data;
  for(i=0; i < (int) argv[1]; i++) {     strings[i] = (GLchar *) strings_ptr;
     strings_ptr += 1+strlen(strings_ptr);
  }
  result = weglCreateShaderProgramv(type,count,(const GLchar **) strings);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(strings);
}

static void ecb_glBindProgramPipeline(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint pipeline;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5782,"pipeline");
  weglBindProgramPipeline(pipeline);
}

static void ecb_glDeleteProgramPipelines(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei n;
  int pipelines_free = 0;
  GLuint *pipelines;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5783,"n");
  if(!enif_is_list(env, argv[1])) { Badarg(5783, "pipelines"); }
  else {
    int i=0;
    ERL_NIF_TERM pipelines_l, pipelines_h, pipelines_t;
    pipelines_free = 1;
    pipelines = (GLuint *) enif_alloc(n * sizeof(GLuint));
    pipelines_l = argv[1];
    while(enif_get_list_cell(env, pipelines_l, &pipelines_h, &pipelines_t)) {
        if(!enif_get_uint(env, pipelines_h, &pipelines[i++])) Badarg(5783,"pipelines");
        pipelines_l = pipelines_t;
    };
  }
  weglDeleteProgramPipelines(n,pipelines);
 if(pipelines_free) enif_free(pipelines);
}

static void ecb_glGenProgramPipelines(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLsizei n;
  GLuint * pipelines;
  ERL_NIF_TERM *pipelines_ts;
  if(!enif_get_int(env, argv[0],  &n)) Badarg(5784,"n");
  pipelines = enif_alloc((int)n*sizeof(GLuint));
  pipelines_ts = (ERL_NIF_TERM *) enif_alloc((int)n*sizeof(ERL_NIF_TERM));
  weglGenProgramPipelines(n,pipelines);
  { int ri;
    for(ri=0; ri < (int) n; ri++)
      pipelines_ts[ri] =      enif_make_int(env, pipelines[ri]);}
  reply =      enif_make_list_from_array(env, pipelines_ts, n);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(pipelines_ts);
 enif_free(pipelines);
}

static void ecb_glIsProgramPipeline(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLboolean result;
  ERL_NIF_TERM reply;
  GLuint pipeline;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5785,"pipeline");
  result = weglIsProgramPipeline(pipeline);
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetProgramPipelineiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint pipeline;
  GLenum pname;
  GLint params;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5786,"pipeline");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5786,"pname");
  weglGetProgramPipelineiv(pipeline,pname,&params);
  reply =      enif_make_int(env, params);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glProgramUniform1i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint v0;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5787,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5787,"location");
  if(!enif_get_int(env, argv[2],  &v0)) Badarg(5787,"v0");
  weglProgramUniform1i(program,location,v0);
}

static void ecb_glProgramUniform1iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  int value_free = 0;
  GLint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5788,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5788,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5788,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5788, "value"); }
  else {
    int i=0;
    ERL_NIF_TERM value_l, value_h, value_t;
    value_free = 1;
    value = (GLint *) enif_alloc(count * sizeof(GLint));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_int(env, value_h, &value[i++])) Badarg(5788,"value");
        value_l = value_t;
    };
  }
  weglProgramUniform1iv(program,location,count,value);
 if(value_free) enif_free(value);
}

static void ecb_glProgramUniform1f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLfloat v0;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5789,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5789,"location");
  if(!egl_get_float(env, argv[2],  &v0)) Badarg(5789,"v0");
  weglProgramUniform1f(program,location,v0);
}

static void ecb_glProgramUniform1fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  int value_free = 0;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5790,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5790,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5790,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5790, "value"); }
  else {
    int i=0;
    ERL_NIF_TERM value_l, value_h, value_t;
    value_free = 1;
    value = (GLfloat *) enif_alloc(count * sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!egl_get_float(env, value_h, &value[i++])) Badarg(5790,"value");
        value_l = value_t;
    };
  }
  weglProgramUniform1fv(program,location,count,value);
 if(value_free) enif_free(value);
}

static void ecb_glProgramUniform1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLdouble v0;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5791,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5791,"location");
  if(!enif_get_double(env, argv[2],  &v0)) Badarg(5791,"v0");
  weglProgramUniform1d(program,location,v0);
}

static void ecb_glProgramUniform1dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  int value_free = 0;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5792,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5792,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5792,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5792, "value"); }
  else {
    int i=0;
    ERL_NIF_TERM value_l, value_h, value_t;
    value_free = 1;
    value = (GLdouble *) enif_alloc(count * sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_double(env, value_h, &value[i++])) Badarg(5792,"value");
        value_l = value_t;
    };
  }
  weglProgramUniform1dv(program,location,count,value);
 if(value_free) enif_free(value);
}

static void ecb_glProgramUniform1ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint v0;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5793,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5793,"location");
  if(!enif_get_uint(env, argv[2],  &v0)) Badarg(5793,"v0");
  weglProgramUniform1ui(program,location,v0);
}

static void ecb_glProgramUniform1uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  int value_free = 0;
  GLuint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5794,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5794,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5794,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5794, "value"); }
  else {
    int i=0;
    ERL_NIF_TERM value_l, value_h, value_t;
    value_free = 1;
    value = (GLuint *) enif_alloc(count * sizeof(GLuint));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_uint(env, value_h, &value[i++])) Badarg(5794,"value");
        value_l = value_t;
    };
  }
  weglProgramUniform1uiv(program,location,count,value);
 if(value_free) enif_free(value);
}

static void ecb_glProgramUniform2i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint v0;
  GLint v1;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5795,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5795,"location");
  if(!enif_get_int(env, argv[2],  &v0)) Badarg(5795,"v0");
  if(!enif_get_int(env, argv[3],  &v1)) Badarg(5795,"v1");
  weglProgramUniform2i(program,location,v0,v1);
}

static void ecb_glProgramUniform2iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5796,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5796,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5796,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5796,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLint *) enif_alloc(2*count*sizeof(GLint));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) {
            Badarg(5796,"value");
        } else {
        if(!enif_get_int(env, value_tpl[0], &value[0])) Badarg(5796,"value");
        if(!enif_get_int(env, value_tpl[1], &value[1])) Badarg(5796,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform2iv(program,location,count,value);
}

static void ecb_glProgramUniform2f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLfloat v0;
  GLfloat v1;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5797,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5797,"location");
  if(!egl_get_float(env, argv[2],  &v0)) Badarg(5797,"v0");
  if(!egl_get_float(env, argv[3],  &v1)) Badarg(5797,"v1");
  weglProgramUniform2f(program,location,v0,v1);
}

static void ecb_glProgramUniform2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5798,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5798,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5798,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5798,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(2*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) {
            Badarg(5798,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5798,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5798,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform2fv(program,location,count,value);
}

static void ecb_glProgramUniform2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLdouble v0;
  GLdouble v1;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5799,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5799,"location");
  if(!enif_get_double(env, argv[2],  &v0)) Badarg(5799,"v0");
  if(!enif_get_double(env, argv[3],  &v1)) Badarg(5799,"v1");
  weglProgramUniform2d(program,location,v0,v1);
}

static void ecb_glProgramUniform2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5800,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5800,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5800,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5800,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(2*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) {
            Badarg(5800,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5800,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5800,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform2dv(program,location,count,value);
}

static void ecb_glProgramUniform2ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint v0;
  GLuint v1;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5801,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5801,"location");
  if(!enif_get_uint(env, argv[2],  &v0)) Badarg(5801,"v0");
  if(!enif_get_uint(env, argv[3],  &v1)) Badarg(5801,"v1");
  weglProgramUniform2ui(program,location,v0,v1);
}

static void ecb_glProgramUniform2uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5802,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5802,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5802,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5802,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLuint *) enif_alloc(2*count*sizeof(GLuint));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 2) {
            Badarg(5802,"value");
        } else {
        if(!enif_get_uint(env, value_tpl[0], &value[0])) Badarg(5802,"value");
        if(!enif_get_uint(env, value_tpl[1], &value[1])) Badarg(5802,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform2uiv(program,location,count,value);
}

static void ecb_glProgramUniform3i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint v0;
  GLint v1;
  GLint v2;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5803,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5803,"location");
  if(!enif_get_int(env, argv[2],  &v0)) Badarg(5803,"v0");
  if(!enif_get_int(env, argv[3],  &v1)) Badarg(5803,"v1");
  if(!enif_get_int(env, argv[4],  &v2)) Badarg(5803,"v2");
  weglProgramUniform3i(program,location,v0,v1,v2);
}

static void ecb_glProgramUniform3iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5804,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5804,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5804,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5804,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLint *) enif_alloc(3*count*sizeof(GLint));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) {
            Badarg(5804,"value");
        } else {
        if(!enif_get_int(env, value_tpl[0], &value[0])) Badarg(5804,"value");
        if(!enif_get_int(env, value_tpl[1], &value[1])) Badarg(5804,"value");
        if(!enif_get_int(env, value_tpl[2], &value[2])) Badarg(5804,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform3iv(program,location,count,value);
}

static void ecb_glProgramUniform3f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLfloat v0;
  GLfloat v1;
  GLfloat v2;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5805,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5805,"location");
  if(!egl_get_float(env, argv[2],  &v0)) Badarg(5805,"v0");
  if(!egl_get_float(env, argv[3],  &v1)) Badarg(5805,"v1");
  if(!egl_get_float(env, argv[4],  &v2)) Badarg(5805,"v2");
  weglProgramUniform3f(program,location,v0,v1,v2);
}

static void ecb_glProgramUniform3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5806,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5806,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5806,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5806,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(3*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) {
            Badarg(5806,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5806,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5806,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5806,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform3fv(program,location,count,value);
}

static void ecb_glProgramUniform3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLdouble v0;
  GLdouble v1;
  GLdouble v2;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5807,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5807,"location");
  if(!enif_get_double(env, argv[2],  &v0)) Badarg(5807,"v0");
  if(!enif_get_double(env, argv[3],  &v1)) Badarg(5807,"v1");
  if(!enif_get_double(env, argv[4],  &v2)) Badarg(5807,"v2");
  weglProgramUniform3d(program,location,v0,v1,v2);
}

static void ecb_glProgramUniform3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5808,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5808,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5808,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5808,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(3*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) {
            Badarg(5808,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5808,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5808,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5808,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform3dv(program,location,count,value);
}

static void ecb_glProgramUniform3ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint v0;
  GLuint v1;
  GLuint v2;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5809,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5809,"location");
  if(!enif_get_uint(env, argv[2],  &v0)) Badarg(5809,"v0");
  if(!enif_get_uint(env, argv[3],  &v1)) Badarg(5809,"v1");
  if(!enif_get_uint(env, argv[4],  &v2)) Badarg(5809,"v2");
  weglProgramUniform3ui(program,location,v0,v1,v2);
}

static void ecb_glProgramUniform3uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5810,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5810,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5810,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5810,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLuint *) enif_alloc(3*count*sizeof(GLuint));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 3) {
            Badarg(5810,"value");
        } else {
        if(!enif_get_uint(env, value_tpl[0], &value[0])) Badarg(5810,"value");
        if(!enif_get_uint(env, value_tpl[1], &value[1])) Badarg(5810,"value");
        if(!enif_get_uint(env, value_tpl[2], &value[2])) Badarg(5810,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform3uiv(program,location,count,value);
}

static void ecb_glProgramUniform4i(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLint v0;
  GLint v1;
  GLint v2;
  GLint v3;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5811,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5811,"location");
  if(!enif_get_int(env, argv[2],  &v0)) Badarg(5811,"v0");
  if(!enif_get_int(env, argv[3],  &v1)) Badarg(5811,"v1");
  if(!enif_get_int(env, argv[4],  &v2)) Badarg(5811,"v2");
  if(!enif_get_int(env, argv[5],  &v3)) Badarg(5811,"v3");
  weglProgramUniform4i(program,location,v0,v1,v2,v3);
}

static void ecb_glProgramUniform4iv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5812,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5812,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5812,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5812,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLint *) enif_alloc(4*count*sizeof(GLint));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5812,"value");
        } else {
        if(!enif_get_int(env, value_tpl[0], &value[0])) Badarg(5812,"value");
        if(!enif_get_int(env, value_tpl[1], &value[1])) Badarg(5812,"value");
        if(!enif_get_int(env, value_tpl[2], &value[2])) Badarg(5812,"value");
        if(!enif_get_int(env, value_tpl[3], &value[3])) Badarg(5812,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform4iv(program,location,count,value);
}

static void ecb_glProgramUniform4f(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLfloat v0;
  GLfloat v1;
  GLfloat v2;
  GLfloat v3;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5813,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5813,"location");
  if(!egl_get_float(env, argv[2],  &v0)) Badarg(5813,"v0");
  if(!egl_get_float(env, argv[3],  &v1)) Badarg(5813,"v1");
  if(!egl_get_float(env, argv[4],  &v2)) Badarg(5813,"v2");
  if(!egl_get_float(env, argv[5],  &v3)) Badarg(5813,"v3");
  weglProgramUniform4f(program,location,v0,v1,v2,v3);
}

static void ecb_glProgramUniform4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5814,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5814,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5814,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5814,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(4*count*sizeof(GLfloat));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5814,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5814,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5814,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5814,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5814,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform4fv(program,location,count,value);
}

static void ecb_glProgramUniform4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLdouble v0;
  GLdouble v1;
  GLdouble v2;
  GLdouble v3;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5815,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5815,"location");
  if(!enif_get_double(env, argv[2],  &v0)) Badarg(5815,"v0");
  if(!enif_get_double(env, argv[3],  &v1)) Badarg(5815,"v1");
  if(!enif_get_double(env, argv[4],  &v2)) Badarg(5815,"v2");
  if(!enif_get_double(env, argv[5],  &v3)) Badarg(5815,"v3");
  weglProgramUniform4d(program,location,v0,v1,v2,v3);
}

static void ecb_glProgramUniform4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5816,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5816,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5816,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5816,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(4*count*sizeof(GLdouble));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5816,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5816,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5816,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5816,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5816,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform4dv(program,location,count,value);
}

static void ecb_glProgramUniform4ui(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLuint v0;
  GLuint v1;
  GLuint v2;
  GLuint v3;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5817,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5817,"location");
  if(!enif_get_uint(env, argv[2],  &v0)) Badarg(5817,"v0");
  if(!enif_get_uint(env, argv[3],  &v1)) Badarg(5817,"v1");
  if(!enif_get_uint(env, argv[4],  &v2)) Badarg(5817,"v2");
  if(!enif_get_uint(env, argv[5],  &v3)) Badarg(5817,"v3");
  weglProgramUniform4ui(program,location,v0,v1,v2,v3);
}

static void ecb_glProgramUniform4uiv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLuint *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5818,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5818,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5818,"count");
  if(!enif_is_list(env, argv[3])) { Badarg(5818,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLuint *) enif_alloc(4*count*sizeof(GLuint));
    value_l = argv[3];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5818,"value");
        } else {
        if(!enif_get_uint(env, value_tpl[0], &value[0])) Badarg(5818,"value");
        if(!enif_get_uint(env, value_tpl[1], &value[1])) Badarg(5818,"value");
        if(!enif_get_uint(env, value_tpl[2], &value[2])) Badarg(5818,"value");
        if(!enif_get_uint(env, value_tpl[3], &value[3])) Badarg(5818,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniform4uiv(program,location,count,value);
}

static void ecb_glProgramUniformMatrix2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5819,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5819,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5819,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5819,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5819,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(4*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5819,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5819,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5819,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5819,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5819,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix2fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5820,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5820,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5820,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5820,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5820,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(9*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 9) {
            Badarg(5820,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5820,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5820,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5820,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5820,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5820,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5820,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5820,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5820,"value");
        if(!egl_get_float(env, value_tpl[8], &value[8])) Badarg(5820,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix3fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5821,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5821,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5821,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5821,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5821,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(16*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 16) {
            Badarg(5821,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[8], &value[8])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[9], &value[9])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[10], &value[10])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[11], &value[11])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[12], &value[12])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[13], &value[13])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[14], &value[14])) Badarg(5821,"value");
        if(!egl_get_float(env, value_tpl[15], &value[15])) Badarg(5821,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix4fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5822,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5822,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5822,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5822,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5822,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(4*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 4) {
            Badarg(5822,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5822,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5822,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5822,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5822,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix2dv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5823,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5823,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5823,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5823,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5823,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(9*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 9) {
            Badarg(5823,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5823,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5823,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5823,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5823,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5823,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5823,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5823,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5823,"value");
        if(!enif_get_double(env, value_tpl[8], &value[8])) Badarg(5823,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix3dv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5824,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5824,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5824,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5824,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5824,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(16*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 16) {
            Badarg(5824,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[8], &value[8])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[9], &value[9])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[10], &value[10])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[11], &value[11])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[12], &value[12])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[13], &value[13])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[14], &value[14])) Badarg(5824,"value");
        if(!enif_get_double(env, value_tpl[15], &value[15])) Badarg(5824,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix4dv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix2x3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5825,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5825,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5825,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5825,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5825,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(6*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) {
            Badarg(5825,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5825,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5825,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5825,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5825,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5825,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5825,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix2x3fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix3x2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5826,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5826,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5826,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5826,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5826,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(6*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) {
            Badarg(5826,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5826,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5826,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5826,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5826,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5826,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5826,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix3x2fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix2x4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5827,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5827,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5827,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5827,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5827,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(8*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) {
            Badarg(5827,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5827,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5827,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5827,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5827,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5827,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5827,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5827,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5827,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix2x4fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix4x2fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5828,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5828,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5828,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5828,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5828,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(8*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) {
            Badarg(5828,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5828,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5828,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5828,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5828,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5828,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5828,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5828,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5828,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix4x2fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix3x4fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5829,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5829,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5829,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5829,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5829,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(12*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) {
            Badarg(5829,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[8], &value[8])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[9], &value[9])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[10], &value[10])) Badarg(5829,"value");
        if(!egl_get_float(env, value_tpl[11], &value[11])) Badarg(5829,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix3x4fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix4x3fv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLfloat *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5830,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5830,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5830,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5830,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5830,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLfloat *) enif_alloc(12*count*sizeof(GLfloat));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) {
            Badarg(5830,"value");
        } else {
        if(!egl_get_float(env, value_tpl[0], &value[0])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[1], &value[1])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[2], &value[2])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[3], &value[3])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[4], &value[4])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[5], &value[5])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[6], &value[6])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[7], &value[7])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[8], &value[8])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[9], &value[9])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[10], &value[10])) Badarg(5830,"value");
        if(!egl_get_float(env, value_tpl[11], &value[11])) Badarg(5830,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix4x3fv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix2x3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5831,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5831,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5831,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5831,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5831,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(6*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) {
            Badarg(5831,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5831,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5831,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5831,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5831,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5831,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5831,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix2x3dv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix3x2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5832,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5832,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5832,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5832,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5832,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(6*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 6) {
            Badarg(5832,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5832,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5832,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5832,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5832,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5832,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5832,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix3x2dv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix2x4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5833,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5833,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5833,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5833,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5833,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(8*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) {
            Badarg(5833,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5833,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5833,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5833,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5833,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5833,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5833,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5833,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5833,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix2x4dv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix4x2dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5834,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5834,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5834,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5834,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5834,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(8*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 8) {
            Badarg(5834,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5834,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5834,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5834,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5834,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5834,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5834,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5834,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5834,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix4x2dv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix3x4dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5835,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5835,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5835,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5835,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5835,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(12*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) {
            Badarg(5835,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[8], &value[8])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[9], &value[9])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[10], &value[10])) Badarg(5835,"value");
        if(!enif_get_double(env, value_tpl[11], &value[11])) Badarg(5835,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix3x4dv(program,location,count,transpose,value);
}

static void ecb_glProgramUniformMatrix4x3dv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint program;
  GLint location;
  GLsizei count;
  GLboolean transpose;
  GLdouble *value;
  if(!enif_get_uint(env, argv[0],  &program)) Badarg(5836,"program");
  if(!enif_get_int(env, argv[1],  &location)) Badarg(5836,"location");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5836,"count");
  if(!egl_get_ubyte(env, argv[3],  &transpose)) Badarg(5836,"transpose");
  if(!enif_is_list(env, argv[4])) { Badarg(5836,"value")}
  else {
    int value_a;
    const ERL_NIF_TERM *value_tpl;
    ERL_NIF_TERM value_l, value_h, value_t;
    value = (GLdouble *) enif_alloc(12*count*sizeof(GLdouble));
    value_l = argv[4];
    while(enif_get_list_cell(env, value_l, &value_h, &value_t)) {
        if(!enif_get_tuple(env, value_h, &value_a, &value_tpl) || value_a != 12) {
            Badarg(5836,"value");
        } else {
        if(!enif_get_double(env, value_tpl[0], &value[0])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[1], &value[1])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[2], &value[2])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[3], &value[3])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[4], &value[4])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[5], &value[5])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[6], &value[6])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[7], &value[7])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[8], &value[8])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[9], &value[9])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[10], &value[10])) Badarg(5836,"value");
        if(!enif_get_double(env, value_tpl[11], &value[11])) Badarg(5836,"value");
        };
        value_l = value_t;
    };
  }
  weglProgramUniformMatrix4x3dv(program,location,count,transpose,value);
}

static void ecb_glValidateProgramPipeline(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint pipeline;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5837,"pipeline");
  weglValidateProgramPipeline(pipeline);
}

static void ecb_glGetProgramPipelineInfoLog(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint pipeline;
  GLsizei bufSize;
  GLsizei length;
  unsigned char *infoLog;
  if(!enif_get_uint(env, argv[0],  &pipeline)) Badarg(5838,"pipeline");
  if(!enif_get_int(env, argv[1],  &bufSize)) Badarg(5838,"bufSize");
  infoLog = enif_alloc((int) bufSize*sizeof(GLchar));
  weglGetProgramPipelineInfoLog(pipeline,bufSize,&length,(GLchar *) infoLog);
  reply =      enif_make_string(env, (const char *) infoLog, ERL_NIF_LATIN1);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(infoLog);
}

static void ecb_glVertexAttribL1d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5839,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5839,"x");
  weglVertexAttribL1d(index,x);
}

static void ecb_glVertexAttribL2d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5840,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5840,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5840,"y");
  weglVertexAttribL2d(index,x,y);
}

static void ecb_glVertexAttribL3d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5841,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5841,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5841,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5841,"z");
  weglVertexAttribL3d(index,x,y,z);
}

static void ecb_glVertexAttribL4d(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLdouble x;
  GLdouble y;
  GLdouble z;
  GLdouble w;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5842,"index");
  if(!enif_get_double(env, argv[1],  &x)) Badarg(5842,"x");
  if(!enif_get_double(env, argv[2],  &y)) Badarg(5842,"y");
  if(!enif_get_double(env, argv[3],  &z)) Badarg(5842,"z");
  if(!enif_get_double(env, argv[4],  &w)) Badarg(5842,"w");
  weglVertexAttribL4d(index,x,y,z,w);
}

static void ecb_glVertexAttribLPointer(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint size;
  GLenum type;
  GLsizei stride;
  ErlNifBinary pointer;
  GLvoid *pointer_idx;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5843,"index");
  if(!enif_get_int(env, argv[1],  &size)) Badarg(5843,"size");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5843,"type");
  if(!enif_get_int(env, argv[3],  &stride)) Badarg(5843,"stride");
  if(!egl_get_ptr(env, argv[4], (void **) &pointer_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[4], &pointer))
        pointer_idx = (GLvoid *) pointer.data;
    else Badarg(5843,"pointer");
  }
  weglVertexAttribLPointer(index,size,type,stride,pointer_idx);
}

static void ecb_glGetVertexAttribLdv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLuint index;
  GLenum pname;
  GLdouble params[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5845,"index");
  if(!enif_get_uint(env, argv[1],  &pname)) Badarg(5845,"pname");
  weglGetVertexAttribLdv(index,pname,params);
  reply =      enif_make_tuple4(env,
     enif_make_double(env, params[0]),
            enif_make_double(env, params[1]),
            enif_make_double(env, params[2]),
            enif_make_double(env, params[3]));
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glViewportArrayv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLfloat *v;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5846,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5846,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5846,"v")}
  else {
    int v_a;
    const ERL_NIF_TERM *v_tpl;
    ERL_NIF_TERM v_l, v_h, v_t;
    v = (GLfloat *) enif_alloc(4*count*sizeof(GLfloat));
    v_l = argv[2];
    while(enif_get_list_cell(env, v_l, &v_h, &v_t)) {
        if(!enif_get_tuple(env, v_h, &v_a, &v_tpl) || v_a != 4) {
            Badarg(5846,"v");
        } else {
        if(!egl_get_float(env, v_tpl[0], &v[0])) Badarg(5846,"v");
        if(!egl_get_float(env, v_tpl[1], &v[1])) Badarg(5846,"v");
        if(!egl_get_float(env, v_tpl[2], &v[2])) Badarg(5846,"v");
        if(!egl_get_float(env, v_tpl[3], &v[3])) Badarg(5846,"v");
        };
        v_l = v_t;
    };
  }
  weglViewportArrayv(first,count,v);
}

static void ecb_glViewportIndexedf(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat x;
  GLfloat y;
  GLfloat w;
  GLfloat h;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5847,"index");
  if(!egl_get_float(env, argv[1],  &x)) Badarg(5847,"x");
  if(!egl_get_float(env, argv[2],  &y)) Badarg(5847,"y");
  if(!egl_get_float(env, argv[3],  &w)) Badarg(5847,"w");
  if(!egl_get_float(env, argv[4],  &h)) Badarg(5847,"h");
  weglViewportIndexedf(index,x,y,w,h);
}

static void ecb_glViewportIndexedfv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLfloat v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5848,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5848,"v");
   } else {
    int i1 = 0;
     if(!egl_get_float(env, v_t[i1++], &v[0])) Badarg(5848,"v");
     if(!egl_get_float(env, v_t[i1++], &v[1])) Badarg(5848,"v");
     if(!egl_get_float(env, v_t[i1++], &v[2])) Badarg(5848,"v");
     if(!egl_get_float(env, v_t[i1++], &v[3])) Badarg(5848,"v");
   }};
  weglViewportIndexedfv(index,v);
}

static void ecb_glScissorArrayv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLint *v;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5849,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5849,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5849,"v")}
  else {
    int v_a;
    const ERL_NIF_TERM *v_tpl;
    ERL_NIF_TERM v_l, v_h, v_t;
    v = (GLint *) enif_alloc(4*count*sizeof(GLint));
    v_l = argv[2];
    while(enif_get_list_cell(env, v_l, &v_h, &v_t)) {
        if(!enif_get_tuple(env, v_h, &v_a, &v_tpl) || v_a != 4) {
            Badarg(5849,"v");
        } else {
        if(!enif_get_int(env, v_tpl[0], &v[0])) Badarg(5849,"v");
        if(!enif_get_int(env, v_tpl[1], &v[1])) Badarg(5849,"v");
        if(!enif_get_int(env, v_tpl[2], &v[2])) Badarg(5849,"v");
        if(!enif_get_int(env, v_tpl[3], &v[3])) Badarg(5849,"v");
        };
        v_l = v_t;
    };
  }
  weglScissorArrayv(first,count,v);
}

static void ecb_glScissorIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint left;
  GLint bottom;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5850,"index");
  if(!enif_get_int(env, argv[1],  &left)) Badarg(5850,"left");
  if(!enif_get_int(env, argv[2],  &bottom)) Badarg(5850,"bottom");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5850,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5850,"height");
  weglScissorIndexed(index,left,bottom,width,height);
}

static void ecb_glScissorIndexedv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLint v[4];
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5851,"index");
  {
   int v_a;
   const ERL_NIF_TERM *v_t;
   if(!enif_get_tuple(env, argv[1], &v_a, &v_t) || v_a != 4) {
     Badarg(5851,"v");
   } else {
    int i1 = 0;
     if(!enif_get_int(env, v_t[i1++], &v[0])) Badarg(5851,"v");
     if(!enif_get_int(env, v_t[i1++], &v[1])) Badarg(5851,"v");
     if(!enif_get_int(env, v_t[i1++], &v[2])) Badarg(5851,"v");
     if(!enif_get_int(env, v_t[i1++], &v[3])) Badarg(5851,"v");
   }};
  weglScissorIndexedv(index,v);
}

static void ecb_glDepthRangeArrayv(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint first;
  GLsizei count;
  GLclampd *v;
  if(!enif_get_uint(env, argv[0],  &first)) Badarg(5852,"first");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5852,"count");
  if(!enif_is_list(env, argv[2])) { Badarg(5852,"v")}
  else {
    int v_a;
    const ERL_NIF_TERM *v_tpl;
    ERL_NIF_TERM v_l, v_h, v_t;
    v = (GLclampd *) enif_alloc(2*count*sizeof(GLclampd));
    v_l = argv[2];
    while(enif_get_list_cell(env, v_l, &v_h, &v_t)) {
        if(!enif_get_tuple(env, v_h, &v_a, &v_tpl) || v_a != 2) {
            Badarg(5852,"v");
        } else {
        if(!enif_get_double(env, v_tpl[0], &v[0])) Badarg(5852,"v");
        if(!enif_get_double(env, v_tpl[1], &v[1])) Badarg(5852,"v");
        };
        v_l = v_t;
    };
  }
  weglDepthRangeArrayv(first,count,v);
}

static void ecb_glDepthRangeIndexed(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint index;
  GLclampd n;
  GLclampd f;
  if(!enif_get_uint(env, argv[0],  &index)) Badarg(5853,"index");
  if(!enif_get_double(env, argv[1],  &n)) Badarg(5853,"n");
  if(!enif_get_double(env, argv[2],  &f)) Badarg(5853,"f");
  weglDepthRangeIndexed(index,n,f);
}

static void ecb_glGetFloati_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLfloat data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5854,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5854,"index");
  weglGetFloati_v(target,index,data);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       data_ts[ri] =      enif_make_double(env, (double) data[ri]);}
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glGetDoublei_v(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLuint index;
  GLdouble data[16];
  ERL_NIF_TERM data_ts[16];
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5855,"target");
  if(!enif_get_uint(env, argv[1],  &index)) Badarg(5855,"index");
  weglGetDoublei_v(target,index,data);
  { int ri;
    for(ri=0; ri < (int) 16; ri++)
       data_ts[ri] =      enif_make_double(env, data[ri]);}
  reply =      enif_make_list_from_array(env, data_ts, 16);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDebugMessageControlARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum source;
  GLenum type;
  GLenum severity;
  GLsizei count;
  int ids_free = 0;
  GLuint *ids;
  GLboolean enabled;
  if(!enif_get_uint(env, argv[0],  &source)) Badarg(5856,"source");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5856,"type");
  if(!enif_get_uint(env, argv[2],  &severity)) Badarg(5856,"severity");
  if(!enif_get_int(env, argv[3],  &count)) Badarg(5856,"count");
  if(!enif_is_list(env, argv[4])) { Badarg(5856, "ids"); }
  else {
    int i=0;
    ERL_NIF_TERM ids_l, ids_h, ids_t;
    ids_free = 1;
    ids = (GLuint *) enif_alloc(count * sizeof(GLuint));
    ids_l = argv[4];
    while(enif_get_list_cell(env, ids_l, &ids_h, &ids_t)) {
        if(!enif_get_uint(env, ids_h, &ids[i++])) Badarg(5856,"ids");
        ids_l = ids_t;
    };
  }
  if(!egl_get_ubyte(env, argv[5],  &enabled)) Badarg(5856,"enabled");
  weglDebugMessageControlARB(source,type,severity,count,ids,enabled);
 if(ids_free) enif_free(ids);
}

static void ecb_glDebugMessageInsertARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum source;
  GLenum type;
  GLuint id;
  GLenum severity;
  ErlNifBinary buf;
  if(!enif_get_uint(env, argv[0],  &source)) Badarg(5857,"source");
  if(!enif_get_uint(env, argv[1],  &type)) Badarg(5857,"type");
  if(!enif_get_uint(env, argv[2],  &id)) Badarg(5857,"id");
  if(!enif_get_uint(env, argv[3],  &severity)) Badarg(5857,"severity");
  if(!enif_inspect_binary(env, argv[5], &buf)) Badarg(5857,"buf");
  weglDebugMessageInsertARB(source,type,id,severity,(GLsizei) buf.size,(GLchar *) buf.data);
}

static void ecb_glGetDebugMessageLogARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint result;
  ERL_NIF_TERM reply;
  GLuint count;
  GLsizei bufsize;
  GLenum * sources;
  ERL_NIF_TERM *sources_ts;
  GLenum * types;
  ERL_NIF_TERM *types_ts;
  GLuint * ids;
  ERL_NIF_TERM *ids_ts;
  GLenum * severities;
  ERL_NIF_TERM *severities_ts;
  GLsizei * lengths;
  ERL_NIF_TERM *lengths_ts;
  unsigned char *messageLog;
  if(!enif_get_uint(env, argv[0],  &count)) Badarg(5858,"count");
  if(!enif_get_int(env, argv[1],  &bufsize)) Badarg(5858,"bufsize");
  sources = enif_alloc((int)count*sizeof(GLenum));
  sources_ts = (ERL_NIF_TERM *) enif_alloc((int)count*sizeof(ERL_NIF_TERM));
  types = enif_alloc((int)count*sizeof(GLenum));
  types_ts = (ERL_NIF_TERM *) enif_alloc((int)count*sizeof(ERL_NIF_TERM));
  ids = enif_alloc((int)count*sizeof(GLuint));
  ids_ts = (ERL_NIF_TERM *) enif_alloc((int)count*sizeof(ERL_NIF_TERM));
  severities = enif_alloc((int)count*sizeof(GLenum));
  severities_ts = (ERL_NIF_TERM *) enif_alloc((int)count*sizeof(ERL_NIF_TERM));
  lengths = enif_alloc((int)count*sizeof(GLsizei));
  lengths_ts = (ERL_NIF_TERM *) enif_alloc((int)count*sizeof(ERL_NIF_TERM));
  messageLog = enif_alloc((int) bufsize*sizeof(GLchar));
  result = weglGetDebugMessageLogARB(count,bufsize,sources,types,ids,severities,lengths,(GLchar *) messageLog);
  { int ri;
    for(ri=0; ri < (int) result; ri++)
      sources_ts[ri] =      enif_make_int(env, sources[ri]);}
  { int ri;
    for(ri=0; ri < (int) result; ri++)
      types_ts[ri] =      enif_make_int(env, types[ri]);}
  { int ri;
    for(ri=0; ri < (int) result; ri++)
      ids_ts[ri] =      enif_make_int(env, ids[ri]);}
  { int ri;
    for(ri=0; ri < (int) result; ri++)
      severities_ts[ri] =      enif_make_int(env, severities[ri]);}
  reply = enif_make_tuple6(env,
          enif_make_int(env, result),
     enif_make_list_from_array(env, sources_ts, result),
     enif_make_list_from_array(env, types_ts, result),
     enif_make_list_from_array(env, ids_ts, result),
     enif_make_list_from_array(env, severities_ts, result),
     enif_make_string(env, (const char *) messageLog, ERL_NIF_LATIN1) );
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(messageLog);
 enif_free(lengths_ts);
 enif_free(lengths);
 enif_free(severities_ts);
 enif_free(severities);
 enif_free(ids_ts);
 enif_free(ids);
 enif_free(types_ts);
 enif_free(types);
 enif_free(sources_ts);
 enif_free(sources);
}

static void ecb_glGetGraphicsResetStatusARB(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum result;
  ERL_NIF_TERM reply;
  result = weglGetGraphicsResetStatusARB();
  reply =      enif_make_int(env, result);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
}

static void ecb_glDrawArraysInstancedBaseInstance(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLint first;
  GLsizei count;
  GLsizei primcount;
  GLuint baseinstance;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5860,"mode");
  if(!enif_get_int(env, argv[1],  &first)) Badarg(5860,"first");
  if(!enif_get_int(env, argv[2],  &count)) Badarg(5860,"count");
  if(!enif_get_int(env, argv[3],  &primcount)) Badarg(5860,"primcount");
  if(!enif_get_uint(env, argv[4],  &baseinstance)) Badarg(5860,"baseinstance");
  weglDrawArraysInstancedBaseInstance(mode,first,count,primcount,baseinstance);
}

static void ecb_glDrawElementsInstancedBaseInstance(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  void *indices_idx;
  GLsizei primcount;
  GLuint baseinstance;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5861,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5861,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5861,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (void *) indices.data;
    else Badarg(5861,"indices");
  }
  if(!enif_get_int(env, argv[4],  &primcount)) Badarg(5861,"primcount");
  if(!enif_get_uint(env, argv[5],  &baseinstance)) Badarg(5861,"baseinstance");
  weglDrawElementsInstancedBaseInstance(mode,count,type,indices_idx,primcount,baseinstance);
}

static void ecb_glDrawElementsInstancedBaseVertexBaseInstance(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLsizei count;
  GLenum type;
  ErlNifBinary indices;
  void *indices_idx;
  GLsizei primcount;
  GLint basevertex;
  GLuint baseinstance;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5863,"mode");
  if(!enif_get_int(env, argv[1],  &count)) Badarg(5863,"count");
  if(!enif_get_uint(env, argv[2],  &type)) Badarg(5863,"type");
  if(!egl_get_ptr(env, argv[3], (void **) &indices_idx)) {
    if(enif_inspect_binary(env, (ERL_NIF_TERM) argv[3], &indices))
        indices_idx = (void *) indices.data;
    else Badarg(5863,"indices");
  }
  if(!enif_get_int(env, argv[4],  &primcount)) Badarg(5863,"primcount");
  if(!enif_get_int(env, argv[5],  &basevertex)) Badarg(5863,"basevertex");
  if(!enif_get_uint(env, argv[6],  &baseinstance)) Badarg(5863,"baseinstance");
  weglDrawElementsInstancedBaseVertexBaseInstance(mode,count,type,indices_idx,primcount,basevertex,baseinstance);
}

static void ecb_glDrawTransformFeedbackInstanced(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint id;
  GLsizei primcount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5865,"mode");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5865,"id");
  if(!enif_get_int(env, argv[2],  &primcount)) Badarg(5865,"primcount");
  weglDrawTransformFeedbackInstanced(mode,id,primcount);
}

static void ecb_glDrawTransformFeedbackStreamInstanced(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum mode;
  GLuint id;
  GLuint stream;
  GLsizei primcount;
  if(!enif_get_uint(env, argv[0],  &mode)) Badarg(5866,"mode");
  if(!enif_get_uint(env, argv[1],  &id)) Badarg(5866,"id");
  if(!enif_get_uint(env, argv[2],  &stream)) Badarg(5866,"stream");
  if(!enif_get_int(env, argv[3],  &primcount)) Badarg(5866,"primcount");
  weglDrawTransformFeedbackStreamInstanced(mode,id,stream,primcount);
}

static void ecb_glGetInternalformativ(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM reply;
  GLenum target;
  GLenum internalformat;
  GLenum pname;
  GLsizei bufSize;
  GLint * params;
  ERL_NIF_TERM *params_ts;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5867,"target");
  if(!enif_get_uint(env, argv[1],  &internalformat)) Badarg(5867,"internalformat");
  if(!enif_get_uint(env, argv[2],  &pname)) Badarg(5867,"pname");
  if(!enif_get_int(env, argv[3],  &bufSize)) Badarg(5867,"bufSize");
  params = enif_alloc((int)bufSize*sizeof(GLint));
  params_ts = (ERL_NIF_TERM *) enif_alloc((int)bufSize*sizeof(ERL_NIF_TERM));
  weglGetInternalformativ(target,internalformat,pname,bufSize,params);
  { int ri;
    for(ri=0; ri < (int) bufSize; ri++)
      params_ts[ri] =      enif_make_int(env, params[ri]);}
  reply =      enif_make_list_from_array(env, params_ts, bufSize);
  enif_send(NULL, self, env,
   enif_make_tuple2(env, EGL_ATOM_REPLY, reply));
 enif_free(params_ts);
 enif_free(params);
}

static void ecb_glBindImageTexture(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLuint unit;
  GLuint texture;
  GLint level;
  GLboolean layered;
  GLint layer;
  GLenum access;
  GLenum format;
  if(!enif_get_uint(env, argv[0],  &unit)) Badarg(5868,"unit");
  if(!enif_get_uint(env, argv[1],  &texture)) Badarg(5868,"texture");
  if(!enif_get_int(env, argv[2],  &level)) Badarg(5868,"level");
  if(!egl_get_ubyte(env, argv[3],  &layered)) Badarg(5868,"layered");
  if(!enif_get_int(env, argv[4],  &layer)) Badarg(5868,"layer");
  if(!enif_get_uint(env, argv[5],  &access)) Badarg(5868,"access");
  if(!enif_get_uint(env, argv[6],  &format)) Badarg(5868,"format");
  weglBindImageTexture(unit,texture,level,layered,layer,access,format);
}

static void ecb_glMemoryBarrier(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLbitfield barriers;
  if(!enif_get_uint(env, argv[0],  &barriers)) Badarg(5869,"barriers");
  weglMemoryBarrier(barriers);
}

static void ecb_glTexStorage1D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei levels;
  GLenum internalformat;
  GLsizei width;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5870,"target");
  if(!enif_get_int(env, argv[1],  &levels)) Badarg(5870,"levels");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5870,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5870,"width");
  weglTexStorage1D(target,levels,internalformat,width);
}

static void ecb_glTexStorage2D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei levels;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5871,"target");
  if(!enif_get_int(env, argv[1],  &levels)) Badarg(5871,"levels");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5871,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5871,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5871,"height");
  weglTexStorage2D(target,levels,internalformat,width,height);
}

static void ecb_glTexStorage3D(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLenum target;
  GLsizei levels;
  GLenum internalformat;
  GLsizei width;
  GLsizei height;
  GLsizei depth;
  if(!enif_get_uint(env, argv[0],  &target)) Badarg(5872,"target");
  if(!enif_get_int(env, argv[1],  &levels)) Badarg(5872,"levels");
  if(!enif_get_uint(env, argv[2],  &internalformat)) Badarg(5872,"internalformat");
  if(!enif_get_int(env, argv[3],  &width)) Badarg(5872,"width");
  if(!enif_get_int(env, argv[4],  &height)) Badarg(5872,"height");
  if(!enif_get_int(env, argv[5],  &depth)) Badarg(5872,"depth");
  weglTexStorage3D(target,levels,internalformat,width,height,depth);
}

static void ecb_glDepthBoundsEXT(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLclampd zmin;
  GLclampd zmax;
  if(!enif_get_double(env, argv[0],  &zmin)) Badarg(5873,"zmin");
  if(!enif_get_double(env, argv[1],  &zmax)) Badarg(5873,"zmax");
  weglDepthBoundsEXT(zmin,zmax);
}

static void ecb_glStencilClearTagEXT(ErlNifEnv* env, ErlNifPid *self, ERL_NIF_TERM argv[])
{
  GLsizei stencilTagBits;
  GLuint stencilClearTag;
  if(!enif_get_int(env, argv[0],  &stencilTagBits)) Badarg(5874,"stencilTagBits");
  if(!enif_get_uint(env, argv[1],  &stencilClearTag)) Badarg(5874,"stencilClearTag");
  weglStencilClearTagEXT(stencilTagBits,stencilClearTag);
}



static ErlNifFunc egl_funcs[] = 
{
  {"lookup_func", 1, egl_lookup_func}
};
static int egl_init(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
  EGL_ATOM_OK = enif_make_atom(env, "ok");
  EGL_ATOM_BADARG = enif_make_atom(env, "badarg");
  EGL_ATOM_REPLY = enif_make_atom(env, "_egl_result_");
  EGL_ATOM_ERROR = enif_make_atom(env, "_egl_error_");

  if(!egl_load_functions()) {
    init_tess();
    return 0;
  } else return 1;
}

 ERL_NIF_INIT(gl, egl_funcs, egl_init, NULL, NULL, NULL)

 #include "gl_finit.h"

