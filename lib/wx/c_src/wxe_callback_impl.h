/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

#ifndef _WXE_CALLBACK_IMPL_H
#define	_WXE_CALLBACK_IMPL_H

void pre_callback();
void handle_event_callback(wxeMemEnv *memenv, ErlNifPid process);

#if wxCHECK_VERSION(2,9,0)
    #define wxeIntPtr wxIntPtr
#else
    // This is bad but how it was in wx-2.8
    #define wxeIntPtr long
#endif

/* Fun Callback id */ 
class wxeEvtListener : public wxEvtHandler
{
public:
   wxeEvtListener(ErlNifPid caller, int req, ERL_NIF_TERM req_type,
		  int funcb, int skip_ev, wxeErlTerm * userData, wxeMemEnv *menv);
   ~wxeEvtListener();
   void forward(wxEvent& event);
   ErlNifPid    listener;
   int          fun_id;
   int          obj;
   ERL_NIF_TERM class_name;
   int          skip;
   wxeErlTerm * user_data;
   wxeMemEnv  * memenv;
};

class wxEPrintout : public wxPrintout
{
 public:
 wxEPrintout(wxString Title, int onPrintP, int onPrepareP,
	     int onBeginP, int onEndP,
	     int onBeginD, int onEndD,
	     int hasP, int getPageI) :
    wxPrintout(Title),
	onPrintPage(onPrintP), onPreparePrinting(onPrepareP),
	onBeginPrinting(onBeginP), onEndPrinting(onEndP),
	onBeginDocument(onBeginD), onEndDocument(onEndD), hasPage(hasP), getPageInfo(getPageI)
	{ } ;

    ~wxEPrintout();

    bool OnBeginDocument(int startPage, int endPage);
    void OnEndDocument();
    void OnBeginPrinting();
    void OnEndPrinting();

    void OnPreparePrinting();

    bool HasPage(int page);
    bool OnPrintPage(int page);
    void GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo);

    int onPrintPage;
    int onPreparePrinting;
    int onBeginPrinting;
    int onEndPrinting;
    int onBeginDocument;
    int onEndDocument;
    int hasPage;
    int getPageInfo;

    wxeMemEnv  * memenv;
};

void clear_cb(wxeMemEnv *, int callback);

// Implementation of wxListCtrlCompare
struct callbackInfo {
    wxeMemEnv * memenv;
    int callbackID;
};

int wxCALLBACK wxEListCtrlCompare(wxeIntPtr item1, wxeIntPtr item2, wxeIntPtr callbackInfoPtr);

#endif
