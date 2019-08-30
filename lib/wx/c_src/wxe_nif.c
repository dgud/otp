/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2017. All Rights Reserved.
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
#include <stdlib.h>
#include "wxe_nif.h"

ERL_NIF_TERM WXE_ATOM_ok;
ERL_NIF_TERM WXE_ATOM_undefined;
ERL_NIF_TERM WXE_ATOM_badarg;
ERL_NIF_TERM WXE_ATOM_true;
ERL_NIF_TERM WXE_ATOM_false;

ERL_NIF_TERM WXE_ATOM_wx;
ERL_NIF_TERM WXE_ATOM_reply;
ERL_NIF_TERM WXE_ATOM_error;
ERL_NIF_TERM WXE_ATOM_wx_ref;
ERL_NIF_TERM WXE_ATOM__wx_invoke_cb_;

ErlNifResourceType* wxeMemEnvRt = NULL;
int wxe_debug = 0;

extern void wxe_initOpenGL(void * fptr);

// void destroyMemEnv(wxeMemEnv *memenv);

int get_ptr(ErlNifEnv* env, ERL_NIF_TERM term, void** dp)
{
    ErlNifUInt64 temp;
    if(enif_get_uint64(env, term, &temp)) {
        *dp = (void *) temp;
        return 1;
    } else return 0;
}

int wxe_get_float(ErlNifEnv* env, ERL_NIF_TERM term, float* dp)
{
    double temp;
    if(enif_get_double(env, term, &temp)) {
        *dp = (float) temp;
        return 1;
    } else return 0;
}

static ERL_NIF_TERM wx_setup_cmd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int op;
    void *ptr;
    if(!enif_get_int(env, argv[argc-1], &op))
        return enif_make_badarg(env);
    if(argc < 2 || !enif_get_resource(env, argv[argc-2], wxeMemEnvRt, &ptr)) {
        push_nif(env, argc-1, argv, op, NULL);
    } else {
        push_nif(env, argc-2, argv, op, ptr);
    }
    return WXE_ATOM_ok;
}

static ERL_NIF_TERM wx_init_opengl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void * fptr;
    if(!get_ptr(env, argv[0], &fptr))
        return enif_make_badarg(env);
    wxe_initOpenGL(fptr);
    return WXE_ATOM_ok;
}

static ERL_NIF_TERM wxe_make_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    /* The returned memenv must only be manipulated by the wx_thread */
    return newMemEnv(env);
}

static ERL_NIF_TERM wxe_delete_env(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void * obj;
    if(!enif_get_resource(env, argv[argc-1], wxeMemEnvRt, &obj))
        obj = NULL;
    meta_command(env, WXE_DELETE_ENV, obj);
    return WXE_ATOM_ok;
}

static ERL_NIF_TERM wxe_debug_driver(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int debug;
    if(enif_get_int(env, argv[0], &debug)) {
        if(debug) wxe_debug = 1;
        else wxe_debug = 0;
    }
    return enif_make_int(env, wxe_debug);
}


// Callback
static void wxe_destroy_env(ErlNifEnv* env, void *obj)
{
    // wxeMemEnv * mem = (wxeMemEnv *) obj;
    // Clean up all references (delete all windows) ??
    fprintf(stderr, "Deleting memenv\r\n");
    // enif_free(mem->ref2ptr);
}

static void wxe_process_down(ErlNifEnv* env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    meta_command(env, WXE_CB_DIED, obj);
}

static ErlNifFunc nif_funcs[] =
{
    {"queue_cmd", 1, wx_setup_cmd},
    {"queue_cmd", 2, wx_setup_cmd},
    {"queue_cmd", 3, wx_setup_cmd},
    {"queue_cmd", 4, wx_setup_cmd},
    {"queue_cmd", 5, wx_setup_cmd},
    {"queue_cmd", 6, wx_setup_cmd},
    {"queue_cmd", 7, wx_setup_cmd},
    {"queue_cmd", 8, wx_setup_cmd},
    {"queue_cmd", 9, wx_setup_cmd},
    {"queue_cmd",10, wx_setup_cmd},
    {"queue_cmd",11, wx_setup_cmd},
    {"queue_cmd",12, wx_setup_cmd},
    {"queue_cmd",13, wx_setup_cmd},
    {"queue_cmd",14, wx_setup_cmd},
    {"init_opengl", 1, wx_init_opengl},
    {"make_env", 0, wxe_make_env},
    {"debug_driver", 1, wxe_debug_driver}
};

void wxe_init_atoms(ErlNifEnv *env) {
    WXE_ATOM_ok = enif_make_atom(env, "ok");
    WXE_ATOM_badarg = enif_make_atom(env, "badarg");
    WXE_ATOM_undefined = enif_make_atom(env, "undefined");
    WXE_ATOM_true = enif_make_atom(env, "true");
    WXE_ATOM_false = enif_make_atom(env, "false");

    WXE_ATOM_wx = enif_make_atom(env, "wx");
    WXE_ATOM_wx_ref = enif_make_atom(env, "wx_ref");
    WXE_ATOM_reply = enif_make_atom(env, "_wxe_result_");
    WXE_ATOM_error = enif_make_atom(env, "_wxe_error_");
    WXE_ATOM__wx_invoke_cb_ = enif_make_atom(env, "_wx_invoke_cb_");
}

static int wxe_init(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    ErlNifResourceTypeInit init = {wxe_destroy_env, NULL, wxe_process_down};

    wxe_init_atoms(env);

    wxeMemEnvRt = enif_open_resource_type_x(env, "wxMemEnv", &init, ERL_NIF_RT_CREATE, NULL);

    if(start_native_gui(env) == WXE_INITIATED)
        return 0;
    else
        return 1;
}

static void wxe_unload(ErlNifEnv *env, void *priv_data)
{
    stop_native_gui(env);
}

ERL_NIF_INIT(wxe_util,nif_funcs,wxe_init,NULL,NULL,wxe_unload)
