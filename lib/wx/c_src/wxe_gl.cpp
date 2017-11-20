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

int erl_gl_initiated = FALSE;
ErlDrvTermData gl_active = 0;
wxeGLC glc;

void wxe_initOpenGL(wxeReturn *rt, char *bp) {
  rt->addAtom((char *) "ok");
  rt->add(wxString::FromAscii("already initilized"));
  rt->addTupleCount(2);
  rt->send();
}

void setActiveGL(ErlDrvTermData caller, wxGLCanvas *canvas)
{
  gl_active = caller;
  glc[caller] = canvas;
  //fprintf(stderr, "set caller %p => %p\r\n", caller, canvas);
  canvas->SetCurrent();
}

void deleteActiveGL(wxGLCanvas *canvas)
{
  gl_active = 0;
  wxeGLC::iterator it;
  for(it = glc.begin(); it != glc.end(); ++it) {
    if(it->second == canvas) {
      it->second = (wxGLCanvas *) 0;
    }
  }
}

void gl_dispatch(wxeCommand *event){
  //fprintf(stderr, "caller %p gl_active %p\r\n", event->pid, gl_active);
  if(gl_active) {
  // if(caller != gl_active) {
  //   wxGLCanvas * current = glc[caller];
  //   if(current) {
  //     if(current != glc[gl_active]) {
  //       current->SetCurrent();
  //     }
  //     gl_active = caller;
  } else {
    enif_send(NULL, &event->pid, event->env,
              enif_make_tuple3(event->env,
                               enif_make_atom(event->env, "_egl_error_"),
                               enif_make_int(event->env, event->op),
                               enif_make_atom(event->env, "no_gl_context")));
    event->op = -1;
    enif_clear_env(event->env);
    return ;
  }
  event->op = -1;
  event->fptr(event->env, &event->pid, event->args);
  enif_clear_env(event->env);
}

