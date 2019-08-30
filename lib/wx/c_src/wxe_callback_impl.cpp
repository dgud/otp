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

#include <wx/wx.h>
#include "wxe_impl.h"
#include "wxe_return.h"
#include "wxe_events.h"
#include "wxe_gl.h"
#include "gen/wxe_macros.h"
#include "gen/wxe_derived_dest.h"


/* ****************************************************************************
 * CallbackData *
 * ****************************************************************************/

wxeEvtListener::wxeEvtListener(ErlNifPid caller, int req, ERL_NIF_TERM req_type,
			       int funcb, int skip_ev, wxeErlTerm * userData,
			       wxeMemEnv *menv)
  : wxEvtHandler()
{
  memenv=menv;
  listener = caller;
  obj = req;
  fun_id = funcb;
  class_name = req_type;
  skip = skip_ev;
  user_data = userData;
}

wxeEvtListener::~wxeEvtListener() {
    // fprintf(stderr, "CBD Deleteing %p %s\r\n", this, class_name); fflush(stderr);
  if(user_data) {
    delete user_data;
  }
  ptrMap::iterator it;
  it = ((WxeApp *)wxTheApp)->ptr2ref.find(this);
  if(it != ((WxeApp *)wxTheApp)->ptr2ref.end()) {
    wxeRefData *refd = it->second;
    wxeReturn rt = wxeReturn(memenv, memenv->owner, false);
    rt.send(enif_make_tuple4(rt.env,
                             rt.make_atom("wx_delete_cb"),
                             rt.make_int(fun_id),
                             rt.make_ref(refd->ref, "wxeEvtListener"),
                             class_name));
  }
  ((WxeApp *)wxTheApp)->clearPtr(this);
}

void wxeEvtListener::forward(wxEvent& event)
{
  sendevent(&event, memenv);
}

/* *****************************************************************/
/* Special Class impls */

#define INVOKE_CALLBACK_INIT(memenv, callback, class_str, args)		\
  {									\
    wxeReturn rt = wxeReturn(memenv, memenv->owner, false);	\
    ERL_NIF_TERM cb_term = enif_make_tuple4(rt.env,                     \
      rt.make_atom("_wx_invoke_cb_"),                                   \
      rt.make_int(callback),                                            \
      enif_make_list(rt.env, 1+(args),                                  \
         rt.make_ref(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), class_str)

#define INVOKE_CALLBACK_END(memenv)  				        \
         ),                                                             \
     rt.make_atom("undefined"));                                        \
   rt.send(cb_term);							\
   handle_event_callback(memenv, memenv->owner);                        \
 }

#define INVOKE_CALLBACK(memenv, callback, class_str)	\
  INVOKE_CALLBACK_INIT(memenv, callback, class_str, 0)	\
  INVOKE_CALLBACK_END(memenv)

/* *****************************************************************/
/* Printing special */

wxEPrintout::~wxEPrintout() {
  clear_cb(memenv, onPrintPage);
  clear_cb(memenv, onPreparePrinting);
  clear_cb(memenv, onBeginPrinting);
  clear_cb(memenv, onEndPrinting);
  clear_cb(memenv, onBeginDocument);
  clear_cb(memenv, onEndDocument);
  clear_cb(memenv, hasPage);
  clear_cb(memenv, getPageInfo);

  ((WxeApp *)wxTheApp)->clearPtr(this);
}

bool wxEPrintout::OnBeginDocument(int startPage, int endPage)
{
  if(onBeginDocument) {
    INVOKE_CALLBACK_INIT(memenv, onBeginDocument, "wxPrintout",2)
    ,rt.make_int(startPage)
    ,rt.make_int(endPage)
    INVOKE_CALLBACK_END(memenv);
    wxeCommand *cb = ((WxeApp *) wxTheApp)->cb_return;
    int ret_value;
    if(cb && enif_get_int(cb->env, cb->args[0], &ret_value)) {
      cb->Delete();
      return ret_value;
    }
  }
  return wxPrintout::OnBeginDocument(startPage,endPage);
}

void wxEPrintout::OnEndDocument()
{
  if(onEndDocument) {
    INVOKE_CALLBACK(memenv, onEndDocument, "wxPrintout");
  } else {
    wxPrintout::OnEndDocument();
  }
}

void wxEPrintout::OnBeginPrinting()
{

  if(onBeginPrinting) {
    INVOKE_CALLBACK(memenv, onBeginPrinting, "wxPrintout");
  } else {
    wxPrintout::OnBeginPrinting();
  }
}

void wxEPrintout::OnEndPrinting()
{

  if(onEndPrinting) {
    INVOKE_CALLBACK(memenv, onEndPrinting, "wxPrintout");
  } else {
    wxPrintout::OnEndPrinting();
  }
}

void wxEPrintout::OnPreparePrinting()
{

  if(onPreparePrinting) {
    INVOKE_CALLBACK(memenv, onPreparePrinting, "wxPrintout");
  } else {
    wxPrintout::OnPreparePrinting();
  }
}

bool wxEPrintout::HasPage(int page)
{

  if(hasPage) {
    INVOKE_CALLBACK_INIT(memenv, hasPage, "wxPrintout",1)
    ,rt.make_int(page)
    INVOKE_CALLBACK_END(memenv);
    wxeCommand *cb = ((WxeApp *) wxTheApp)->cb_return;
    int ret_value;
    if(cb && enif_get_int(cb->env, cb->args[0], &ret_value)) {
      cb->Delete();
      return ret_value;
    }
  }
  return wxPrintout::HasPage(page);
}

bool wxEPrintout::OnPrintPage(int page)
{
  INVOKE_CALLBACK_INIT(memenv, onPrintPage, "wxPrintout",1)
  ,rt.make_int(page)
  INVOKE_CALLBACK_END(memenv);
  wxeCommand *cb = ((WxeApp *) wxTheApp)->cb_return;
  int ret_value;
  if(cb && enif_get_int(cb->env, cb->args[0], &ret_value)) {
    cb->Delete();
    return ret_value;
  }
  return FALSE;
}

void wxEPrintout::GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo)
{
  if(getPageInfo) {
    INVOKE_CALLBACK(memenv, getPageInfo, "wxPrintout");
    wxeCommand *cb = ((WxeApp *) wxTheApp)->cb_return;
    if(cb
       && enif_get_int(cb->env, cb->args[0], minPage)
       && enif_get_int(cb->env, cb->args[0], maxPage)
       && enif_get_int(cb->env, cb->args[0], pageFrom)
       && enif_get_int(cb->env, cb->args[0], pageTo)
       ) {
      cb->Delete();
    }
  }
  wxPrintout::GetPageInfo(minPage, maxPage, pageFrom, pageTo);
}

/* *****************************************************************/
// ListCtrl with callbacks for VIRTUAL_TABLES

wxString EwxListCtrl::OnGetItemText(long item, long col) const {
  if(onGetItemText) {
    INVOKE_CALLBACK_INIT(memenv, onGetItemText, "wxListCtrl", 2)
    ,rt.make_int(item)
    ,rt.make_int(col)
    INVOKE_CALLBACK_END(memenv);
    wxeCommand *cb = ((WxeApp *) wxTheApp)->cb_return;
    ErlNifBinary bin;
    if(cb && enif_inspect_binary(cb->env, cb->args[0], &bin)) {
      wxString str = wxString(bin.data, wxConvUTF8);
      cb->Delete();
      return str;
    }
  }
  return wxT("OnGetItemText not correctly defined");
}

wxListItemAttr* EwxListCtrl::OnGetItemAttr(long item) const {
  if(onGetItemAttr) {
    INVOKE_CALLBACK_INIT(memenv, onGetItemAttr, "wxListCtrl",1)
    ,rt.make_int(item)
    INVOKE_CALLBACK_END(memenv);
    wxeCommand *cb = ((WxeApp *) wxTheApp)->cb_return;
    if(cb) {
      wxListItemAttr * result = (wxListItemAttr *) memenv->getPtr(cb->env, cb->args[0], "CB item");
      cb->Delete();
      return result;
    }
  }
  return NULL;
}

int EwxListCtrl::OnGetItemImage(long item) const {
  return OnGetItemColumnImage(item, 0);
}

int EwxListCtrl::OnGetItemColumnImage(long item, long col) const {
  if(onGetItemColumnImage) {
    INVOKE_CALLBACK_INIT(memenv, onGetItemColumnImage, "wxListCtrl",2)
    ,rt.make_int(item)
    ,rt.make_int(col)
    INVOKE_CALLBACK_END(memenv);
    wxeCommand *cb = ((WxeApp *) wxTheApp)->cb_return;
    int ret_value;
    if(cb && enif_get_int(cb->env, cb->args[0], &ret_value)) {
      cb->Delete();
      return ret_value;
    }
  }
  return -1;
}

EwxListCtrl::~EwxListCtrl() {
  clear_cb(memenv, onGetItemText);
  clear_cb(memenv, onGetItemAttr);
  clear_cb(memenv, onGetItemColumnImage);
  ((WxeApp *)wxTheApp)->clearPtr(this);
}

/* ****************************************************************************
 * wxListCtrlCompare wrapper
 * ****************************************************************************/

int wxCALLBACK wxEListCtrlCompare(wxeIntPtr item1, wxeIntPtr item2, wxeIntPtr callbackInfoPtr)
{
  callbackInfo * cbi = (callbackInfo *)callbackInfoPtr;
  wxeMemEnv *memenv = cbi->memenv;
  wxeReturn rt = wxeReturn(memenv, memenv->owner, false);
  ERL_NIF_TERM cb_msg =
    enif_make_tuple4(rt.env,
                     rt.make_atom("_wx_invoke_cb_"),
                     rt.make_int(cbi->callbackID),
                     enif_make_list2(rt.env,
                                     rt.make_int(item1),
                                     rt.make_int(item2)),
                     rt.make_atom("undefined"));
  rt.send(cb_msg);
  handle_event_callback(memenv, memenv->owner);

  wxeCommand *cb = ((WxeApp *) wxTheApp)->cb_return;
  int ret_value;
  if(cb && enif_get_int(cb->env, cb->args[0], &ret_value)) {
    cb->Delete();
    return ret_value;
  }

  return 0;
}


// tools

void clear_cb(wxeMemEnv *memenv, int callback)
{
  if(callback > 0) {
    wxeReturn rt = wxeReturn(memenv, memenv->owner, false);
    ERL_NIF_TERM cb_msg =
      enif_make_tuple2(rt.env,
                       rt.make_atom("wx_delete_cb"),
                       rt.make_int(callback));
    rt.send(cb_msg);
  }
}
