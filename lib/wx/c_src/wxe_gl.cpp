/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

#include <stdio.h>
#include <string.h>
#ifndef _WIN32
#include <dlfcn.h>
#else
#include <windows.h>
#endif
#include "wxe_impl.h"
#include "wxe_return.h"
#include "wxe_gl.h"

/* ****************************************************************************
 * Opengl context management *
 * ****************************************************************************/

ErlNifUInt64 gl_active_index = 0;
ErlNifPid gl_active_pid;

wxeGLC glc;
typedef void * (*WXE_GL_LOOKUP) (int);
WXE_GL_LOOKUP wxe_gl_lookup_func = NULL;
typedef void * (*WXE_GL_FUNC) (ErlNifEnv*, ErlNifPid*, const ERL_NIF_TERM argv[]);

extern "C" {
void wxe_initOpenGL(void * fptr) {
  wxe_gl_lookup_func = (WXE_GL_LOOKUP) fptr;
  enif_set_pid_undefined(&gl_active_pid);
}
}

ErlNifUInt64 wxe_make_hash(ErlNifEnv *env, ErlNifPid *pid)
{
  ERL_NIF_TERM term = enif_make_pid(env, pid);
  return enif_hash(ERL_NIF_INTERNAL_HASH, term, 786234121);
}

void setActiveGL(wxeMemEnv *memenv, ErlNifPid caller, wxGLCanvas *canvas)
{
  gl_active_index = wxe_make_hash(memenv->tmp_env, &caller);
  glc[gl_active_index] = canvas;
  //fprintf(stderr, "set caller %p => %p\r\n", caller, canvas);
}

void deleteActiveGL(wxGLCanvas *canvas)
{
  gl_active_index = 0;
  enif_set_pid_undefined(&gl_active_pid);

  wxeGLC::iterator it;
  for(it = glc.begin(); it != glc.end(); ++it) {
    if(it->second == canvas) {
      it->second = (wxGLCanvas *) 0;
    }
  }
}

void gl_dispatch(wxeCommand *event) {
  WXE_GL_FUNC fptr;
  if(gl_active_index && wxe_gl_lookup_func) {
    if(enif_compare_pids(&(event->caller),&gl_active_pid) != 0) {
      ErlNifUInt64 caller_index =  wxe_make_hash(event->env, &(event->caller));
      wxGLCanvas * current = glc[caller_index];
      if(current) {
        if(current != glc[gl_active_index]) {
          current->SetCurrent();
        }
        gl_active_index = caller_index;
        gl_active_pid = event->caller;
      }
    }
  } else {
    enif_send(NULL, &event->caller, event->env,
              enif_make_tuple3(event->env,
                               enif_make_atom(event->env, "_egl_error_"),
                               enif_make_int(event->env, event->op),
                               enif_make_atom(event->env, "no_gl_context")));
    event->op = -1;
    enif_clear_env(event->env);
    return ;
  }
  if((fptr = (WXE_GL_FUNC) wxe_gl_lookup_func(event->op))) {
    // enif_fprintf(stderr, "GL: caller %T gl_active %T %d\r\n", event->caller, gl_active_pid, event->op);
    fptr(event->env, &event->caller, event->args);
  } else {
    enif_send(NULL, &event->caller, event->env,
              enif_make_tuple3(event->env,
                               enif_make_atom(event->env, "_egl_error_"),
                               enif_make_int(event->env, event->op),
                               enif_make_atom(event->env, "undef")));
  }
  event->op = -1;
  enif_clear_env(event->env);
}

