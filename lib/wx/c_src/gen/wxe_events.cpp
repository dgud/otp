/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2019. All Rights Reserved.
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

#include <wx/wx.h>
#include "../wxe_impl.h"

#include "wxe_macros.h"
#include "../wxe_events.h"

#include "../wxe_return.h"

wxeEtype::wxeEtype(const char *name, int Id) {eName = name;cID = Id;}

WX_DECLARE_HASH_MAP(int, wxeEtype*, wxIntegerHash, wxIntegerEqual, wxeETmap );

wxeETmap etmap;

int wxeEventTypeFromAtom(char *etype_atom) {
  wxeETmap::iterator it;
  for(it = etmap.begin(); it != etmap.end(); ++it) {
       wxeEtype * value = it->second;
       if(strcmp(value->eName, etype_atom) == 0) {
	 if(it->first > wxEVT_USER_FIRST) {
	       return it->first - wxEVT_USER_FIRST;
	    } else {
	       return it->first;
	    }
       }
  }
  return -1;
}

void initEventTable()
{
  struct { int ev_type;  int class_id; const char * ev_name;} event_types[] =
  {
   {wxEVT_NULL, 0, "null"},
   {wxEVT_COMMAND_BUTTON_CLICKED, 165, "command_button_clicked"},
   {wxEVT_COMMAND_CHECKBOX_CLICKED, 165, "command_checkbox_clicked"},
   {wxEVT_COMMAND_CHOICE_SELECTED, 165, "command_choice_selected"},
   {wxEVT_COMMAND_LISTBOX_SELECTED, 165, "command_listbox_selected"},
   {wxEVT_COMMAND_LISTBOX_DOUBLECLICKED, 165, "command_listbox_doubleclicked"},
   {wxEVT_COMMAND_TEXT_UPDATED, 165, "command_text_updated"},
   {wxEVT_COMMAND_TEXT_ENTER, 165, "command_text_enter"},
   {wxEVT_COMMAND_MENU_SELECTED, 165, "command_menu_selected"},
   {wxEVT_COMMAND_SLIDER_UPDATED, 165, "command_slider_updated"},
   {wxEVT_COMMAND_RADIOBOX_SELECTED, 165, "command_radiobox_selected"},
   {wxEVT_COMMAND_RADIOBUTTON_SELECTED, 165, "command_radiobutton_selected"},
   {wxEVT_COMMAND_SCROLLBAR_UPDATED, 165, "command_scrollbar_updated"},
   {wxEVT_COMMAND_VLBOX_SELECTED, 165, "command_vlbox_selected"},
   {wxEVT_COMMAND_COMBOBOX_SELECTED, 165, "command_combobox_selected"},
   {wxEVT_COMMAND_TOOL_RCLICKED, 165, "command_tool_rclicked"},
   {wxEVT_COMMAND_TOOL_ENTER, 165, "command_tool_enter"},
   {wxEVT_COMMAND_CHECKLISTBOX_TOGGLED, 165, "command_checklistbox_toggled"},
   {wxEVT_COMMAND_TOGGLEBUTTON_CLICKED, 165, "command_togglebutton_clicked"},
   {wxEVT_COMMAND_LEFT_CLICK, 165, "command_left_click"},
   {wxEVT_COMMAND_LEFT_DCLICK, 165, "command_left_dclick"},
   {wxEVT_COMMAND_RIGHT_CLICK, 165, "command_right_click"},
   {wxEVT_COMMAND_SET_FOCUS, 165, "command_set_focus"},
   {wxEVT_COMMAND_KILL_FOCUS, 165, "command_kill_focus"},
   {wxEVT_COMMAND_ENTER, 165, "command_enter"},
   {wxEVT_SCROLL_TOP, 166, "scroll_top"},
   {wxEVT_SCROLL_BOTTOM, 166, "scroll_bottom"},
   {wxEVT_SCROLL_LINEUP, 166, "scroll_lineup"},
   {wxEVT_SCROLL_LINEDOWN, 166, "scroll_linedown"},
   {wxEVT_SCROLL_PAGEUP, 166, "scroll_pageup"},
   {wxEVT_SCROLL_PAGEDOWN, 166, "scroll_pagedown"},
   {wxEVT_SCROLL_THUMBTRACK, 166, "scroll_thumbtrack"},
   {wxEVT_SCROLL_THUMBRELEASE, 166, "scroll_thumbrelease"},
   {wxEVT_SCROLL_CHANGED, 166, "scroll_changed"},
   {wxEVT_SCROLLWIN_TOP, 167, "scrollwin_top"},
   {wxEVT_SCROLLWIN_BOTTOM, 167, "scrollwin_bottom"},
   {wxEVT_SCROLLWIN_LINEUP, 167, "scrollwin_lineup"},
   {wxEVT_SCROLLWIN_LINEDOWN, 167, "scrollwin_linedown"},
   {wxEVT_SCROLLWIN_PAGEUP, 167, "scrollwin_pageup"},
   {wxEVT_SCROLLWIN_PAGEDOWN, 167, "scrollwin_pagedown"},
   {wxEVT_SCROLLWIN_THUMBTRACK, 167, "scrollwin_thumbtrack"},
   {wxEVT_SCROLLWIN_THUMBRELEASE, 167, "scrollwin_thumbrelease"},
   {wxEVT_LEFT_DOWN, 168, "left_down"},
   {wxEVT_LEFT_UP, 168, "left_up"},
   {wxEVT_MIDDLE_DOWN, 168, "middle_down"},
   {wxEVT_MIDDLE_UP, 168, "middle_up"},
   {wxEVT_RIGHT_DOWN, 168, "right_down"},
   {wxEVT_RIGHT_UP, 168, "right_up"},
   {wxEVT_MOTION, 168, "motion"},
   {wxEVT_ENTER_WINDOW, 168, "enter_window"},
   {wxEVT_LEAVE_WINDOW, 168, "leave_window"},
   {wxEVT_LEFT_DCLICK, 168, "left_dclick"},
   {wxEVT_MIDDLE_DCLICK, 168, "middle_dclick"},
   {wxEVT_RIGHT_DCLICK, 168, "right_dclick"},
   {wxEVT_MOUSEWHEEL, 168, "mousewheel"},
   {wxEVT_SET_CURSOR, 169, "set_cursor"},
   {wxEVT_CHAR, 170, "char"},
   {wxEVT_CHAR_HOOK, 170, "char_hook"},
   {wxEVT_KEY_DOWN, 170, "key_down"},
   {wxEVT_KEY_UP, 170, "key_up"},
   {wxEVT_SIZE, 171, "size"},
   {wxEVT_MOVE, 172, "move"},
   {wxEVT_PAINT, 173, "paint"},
   {wxEVT_ERASE_BACKGROUND, 174, "erase_background"},
   {wxEVT_SET_FOCUS, 175, "set_focus"},
   {wxEVT_KILL_FOCUS, 175, "kill_focus"},
   {wxEVT_CHILD_FOCUS, 176, "child_focus"},
   {wxEVT_MENU_OPEN, 177, "menu_open"},
   {wxEVT_MENU_CLOSE, 177, "menu_close"},
   {wxEVT_MENU_HIGHLIGHT, 177, "menu_highlight"},
   {wxEVT_CLOSE_WINDOW, 178, "close_window"},
   {wxEVT_END_SESSION, 178, "end_session"},
   {wxEVT_QUERY_END_SESSION, 178, "query_end_session"},
   {wxEVT_SHOW, 179, "show"},
   {wxEVT_ICONIZE, 180, "iconize"},
   {wxEVT_MAXIMIZE, 181, "maximize"},
   {wxEVT_JOY_BUTTON_DOWN, 182, "joy_button_down"},
   {wxEVT_JOY_BUTTON_UP, 182, "joy_button_up"},
   {wxEVT_JOY_MOVE, 182, "joy_move"},
   {wxEVT_JOY_ZMOVE, 182, "joy_zmove"},
   {wxEVT_UPDATE_UI, 183, "update_ui"},
   {wxEVT_SYS_COLOUR_CHANGED, 184, "sys_colour_changed"},
   {wxEVT_MOUSE_CAPTURE_CHANGED, 185, "mouse_capture_changed"},
   {wxEVT_DISPLAY_CHANGED, 186, "display_changed"},
   {wxEVT_PALETTE_CHANGED, 187, "palette_changed"},
   {wxEVT_QUERY_NEW_PALETTE, 188, "query_new_palette"},
   {wxEVT_NAVIGATION_KEY, 189, "navigation_key"},
   {wxEVT_CREATE, 190, "create"},
   {wxEVT_DESTROY, 191, "destroy"},
   {wxEVT_HELP, 192, "help"},
   {wxEVT_DETAILED_HELP, 192, "detailed_help"},
   {wxEVT_CONTEXT_MENU, 193, "context_menu"},
   {wxEVT_IDLE, 194, "idle"},
   {wxEVT_GRID_CELL_LEFT_CLICK, 195, "grid_cell_left_click"},
   {wxEVT_GRID_CELL_RIGHT_CLICK, 195, "grid_cell_right_click"},
   {wxEVT_GRID_CELL_LEFT_DCLICK, 195, "grid_cell_left_dclick"},
   {wxEVT_GRID_CELL_RIGHT_DCLICK, 195, "grid_cell_right_dclick"},
   {wxEVT_GRID_LABEL_LEFT_CLICK, 195, "grid_label_left_click"},
   {wxEVT_GRID_LABEL_RIGHT_CLICK, 195, "grid_label_right_click"},
   {wxEVT_GRID_LABEL_LEFT_DCLICK, 195, "grid_label_left_dclick"},
   {wxEVT_GRID_LABEL_RIGHT_DCLICK, 195, "grid_label_right_dclick"},
   {wxEVT_GRID_ROW_SIZE, 195, "grid_row_size"},
   {wxEVT_GRID_COL_SIZE, 195, "grid_col_size"},
   {wxEVT_GRID_RANGE_SELECT, 195, "grid_range_select"},
   {wxEVT_GRID_CELL_CHANGE, 195, "grid_cell_change"},
   {wxEVT_GRID_SELECT_CELL, 195, "grid_select_cell"},
   {wxEVT_GRID_EDITOR_SHOWN, 195, "grid_editor_shown"},
   {wxEVT_GRID_EDITOR_HIDDEN, 195, "grid_editor_hidden"},
   {wxEVT_GRID_EDITOR_CREATED, 195, "grid_editor_created"},
   {wxEVT_GRID_CELL_BEGIN_DRAG, 195, "grid_cell_begin_drag"},
   {wxEVT_SASH_DRAGGED, 197, "sash_dragged"},
   {wxEVT_COMMAND_LIST_BEGIN_DRAG, 198, "command_list_begin_drag"},
   {wxEVT_COMMAND_LIST_BEGIN_RDRAG, 198, "command_list_begin_rdrag"},
   {wxEVT_COMMAND_LIST_BEGIN_LABEL_EDIT, 198, "command_list_begin_label_edit"},
   {wxEVT_COMMAND_LIST_END_LABEL_EDIT, 198, "command_list_end_label_edit"},
   {wxEVT_COMMAND_LIST_DELETE_ITEM, 198, "command_list_delete_item"},
   {wxEVT_COMMAND_LIST_DELETE_ALL_ITEMS, 198, "command_list_delete_all_items"},
   {wxEVT_COMMAND_LIST_KEY_DOWN, 198, "command_list_key_down"},
   {wxEVT_COMMAND_LIST_INSERT_ITEM, 198, "command_list_insert_item"},
   {wxEVT_COMMAND_LIST_COL_CLICK, 198, "command_list_col_click"},
   {wxEVT_COMMAND_LIST_COL_RIGHT_CLICK, 198, "command_list_col_right_click"},
   {wxEVT_COMMAND_LIST_COL_BEGIN_DRAG, 198, "command_list_col_begin_drag"},
   {wxEVT_COMMAND_LIST_COL_DRAGGING, 198, "command_list_col_dragging"},
   {wxEVT_COMMAND_LIST_COL_END_DRAG, 198, "command_list_col_end_drag"},
   {wxEVT_COMMAND_LIST_ITEM_SELECTED, 198, "command_list_item_selected"},
   {wxEVT_COMMAND_LIST_ITEM_DESELECTED, 198, "command_list_item_deselected"},
   {wxEVT_COMMAND_LIST_ITEM_RIGHT_CLICK, 198, "command_list_item_right_click"},
   {wxEVT_COMMAND_LIST_ITEM_MIDDLE_CLICK, 198, "command_list_item_middle_click"},
   {wxEVT_COMMAND_LIST_ITEM_ACTIVATED, 198, "command_list_item_activated"},
   {wxEVT_COMMAND_LIST_ITEM_FOCUSED, 198, "command_list_item_focused"},
   {wxEVT_COMMAND_LIST_CACHE_HINT, 198, "command_list_cache_hint"},
   {wxEVT_DATE_CHANGED, 199, "date_changed"},
   {wxEVT_CALENDAR_SEL_CHANGED, 200, "calendar_sel_changed"},
   {wxEVT_CALENDAR_DAY_CHANGED, 200, "calendar_day_changed"},
   {wxEVT_CALENDAR_MONTH_CHANGED, 200, "calendar_month_changed"},
   {wxEVT_CALENDAR_YEAR_CHANGED, 200, "calendar_year_changed"},
   {wxEVT_CALENDAR_DOUBLECLICKED, 200, "calendar_doubleclicked"},
   {wxEVT_CALENDAR_WEEKDAY_CLICKED, 200, "calendar_weekday_clicked"},
   {wxEVT_COMMAND_FILEPICKER_CHANGED, 201, "command_filepicker_changed"},
   {wxEVT_COMMAND_DIRPICKER_CHANGED, 201, "command_dirpicker_changed"},
   {wxEVT_COMMAND_COLOURPICKER_CHANGED, 202, "command_colourpicker_changed"},
   {wxEVT_COMMAND_FONTPICKER_CHANGED, 203, "command_fontpicker_changed"},
   {wxEVT_STC_CHANGE, 204, "stc_change"},
   {wxEVT_STC_STYLENEEDED, 204, "stc_styleneeded"},
   {wxEVT_STC_CHARADDED, 204, "stc_charadded"},
   {wxEVT_STC_SAVEPOINTREACHED, 204, "stc_savepointreached"},
   {wxEVT_STC_SAVEPOINTLEFT, 204, "stc_savepointleft"},
   {wxEVT_STC_ROMODIFYATTEMPT, 204, "stc_romodifyattempt"},
   {wxEVT_STC_KEY, 204, "stc_key"},
   {wxEVT_STC_DOUBLECLICK, 204, "stc_doubleclick"},
   {wxEVT_STC_UPDATEUI, 204, "stc_updateui"},
   {wxEVT_STC_MODIFIED, 204, "stc_modified"},
   {wxEVT_STC_MACRORECORD, 204, "stc_macrorecord"},
   {wxEVT_STC_MARGINCLICK, 204, "stc_marginclick"},
   {wxEVT_STC_NEEDSHOWN, 204, "stc_needshown"},
   {wxEVT_STC_PAINTED, 204, "stc_painted"},
   {wxEVT_STC_USERLISTSELECTION, 204, "stc_userlistselection"},
   {wxEVT_STC_URIDROPPED, 204, "stc_uridropped"},
   {wxEVT_STC_DWELLSTART, 204, "stc_dwellstart"},
   {wxEVT_STC_DWELLEND, 204, "stc_dwellend"},
   {wxEVT_STC_START_DRAG, 204, "stc_start_drag"},
   {wxEVT_STC_DRAG_OVER, 204, "stc_drag_over"},
   {wxEVT_STC_DO_DROP, 204, "stc_do_drop"},
   {wxEVT_STC_ZOOM, 204, "stc_zoom"},
   {wxEVT_STC_HOTSPOT_CLICK, 204, "stc_hotspot_click"},
   {wxEVT_STC_HOTSPOT_DCLICK, 204, "stc_hotspot_dclick"},
   {wxEVT_STC_CALLTIP_CLICK, 204, "stc_calltip_click"},
   {wxEVT_STC_AUTOCOMP_SELECTION, 204, "stc_autocomp_selection"},
   {wxEVT_COMMAND_TREE_BEGIN_DRAG, 210, "command_tree_begin_drag"},
   {wxEVT_COMMAND_TREE_BEGIN_RDRAG, 210, "command_tree_begin_rdrag"},
   {wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT, 210, "command_tree_begin_label_edit"},
   {wxEVT_COMMAND_TREE_END_LABEL_EDIT, 210, "command_tree_end_label_edit"},
   {wxEVT_COMMAND_TREE_DELETE_ITEM, 210, "command_tree_delete_item"},
   {wxEVT_COMMAND_TREE_GET_INFO, 210, "command_tree_get_info"},
   {wxEVT_COMMAND_TREE_SET_INFO, 210, "command_tree_set_info"},
   {wxEVT_COMMAND_TREE_ITEM_EXPANDED, 210, "command_tree_item_expanded"},
   {wxEVT_COMMAND_TREE_ITEM_EXPANDING, 210, "command_tree_item_expanding"},
   {wxEVT_COMMAND_TREE_ITEM_COLLAPSED, 210, "command_tree_item_collapsed"},
   {wxEVT_COMMAND_TREE_ITEM_COLLAPSING, 210, "command_tree_item_collapsing"},
   {wxEVT_COMMAND_TREE_SEL_CHANGED, 210, "command_tree_sel_changed"},
   {wxEVT_COMMAND_TREE_SEL_CHANGING, 210, "command_tree_sel_changing"},
   {wxEVT_COMMAND_TREE_KEY_DOWN, 210, "command_tree_key_down"},
   {wxEVT_COMMAND_TREE_ITEM_ACTIVATED, 210, "command_tree_item_activated"},
   {wxEVT_COMMAND_TREE_ITEM_RIGHT_CLICK, 210, "command_tree_item_right_click"},
   {wxEVT_COMMAND_TREE_ITEM_MIDDLE_CLICK, 210, "command_tree_item_middle_click"},
   {wxEVT_COMMAND_TREE_END_DRAG, 210, "command_tree_end_drag"},
   {wxEVT_COMMAND_TREE_STATE_IMAGE_CLICK, 210, "command_tree_state_image_click"},
   {wxEVT_COMMAND_TREE_ITEM_GETTOOLTIP, 210, "command_tree_item_gettooltip"},
   {wxEVT_COMMAND_TREE_ITEM_MENU, 210, "command_tree_item_menu"},
   {wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGED, 211, "command_notebook_page_changed"},
   {wxEVT_COMMAND_NOTEBOOK_PAGE_CHANGING, 211, "command_notebook_page_changing"},
   {wxEVT_COMMAND_TEXT_COPY, 217, "command_text_copy"},
   {wxEVT_COMMAND_TEXT_CUT, 217, "command_text_cut"},
   {wxEVT_COMMAND_TEXT_PASTE, 217, "command_text_paste"},
   {wxEVT_COMMAND_SPINCTRL_UPDATED, 218, "command_spinctrl_updated"},
   {wxEVT_SCROLL_LINEUP + wxEVT_USER_FIRST, 166, "spin_up"},
   {wxEVT_SCROLL_LINEDOWN + wxEVT_USER_FIRST, 166, "spin_down"},
   {wxEVT_SCROLL_THUMBTRACK + wxEVT_USER_FIRST, 166, "spin"},
   {wxEVT_COMMAND_SPLITTER_SASH_POS_CHANGED, 220, "command_splitter_sash_pos_changed"},
   {wxEVT_COMMAND_SPLITTER_SASH_POS_CHANGING, 220, "command_splitter_sash_pos_changing"},
   {wxEVT_COMMAND_SPLITTER_DOUBLECLICKED, 220, "command_splitter_doubleclicked"},
   {wxEVT_COMMAND_SPLITTER_UNSPLIT, 220, "command_splitter_unsplit"},
   {wxEVT_COMMAND_HTML_LINK_CLICKED, 222, "command_html_link_clicked"},
   {wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSE, 225, "command_auinotebook_page_close"},
   {wxEVT_COMMAND_AUINOTEBOOK_PAGE_CHANGED, 225, "command_auinotebook_page_changed"},
   {wxEVT_COMMAND_AUINOTEBOOK_PAGE_CHANGING, 225, "command_auinotebook_page_changing"},
   {wxEVT_COMMAND_AUINOTEBOOK_BUTTON, 225, "command_auinotebook_button"},
   {wxEVT_COMMAND_AUINOTEBOOK_BEGIN_DRAG, 225, "command_auinotebook_begin_drag"},
   {wxEVT_COMMAND_AUINOTEBOOK_END_DRAG, 225, "command_auinotebook_end_drag"},
   {wxEVT_COMMAND_AUINOTEBOOK_DRAG_MOTION, 225, "command_auinotebook_drag_motion"},
   {wxEVT_COMMAND_AUINOTEBOOK_ALLOW_DND, 225, "command_auinotebook_allow_dnd"},
#if wxCHECK_VERSION(2,8,5)
   {wxEVT_COMMAND_AUINOTEBOOK_TAB_MIDDLE_DOWN, 225, "command_auinotebook_tab_middle_down"},
#endif
#if wxCHECK_VERSION(2,8,5)
   {wxEVT_COMMAND_AUINOTEBOOK_TAB_MIDDLE_UP, 225, "command_auinotebook_tab_middle_up"},
#endif
#if wxCHECK_VERSION(2,8,5)
   {wxEVT_COMMAND_AUINOTEBOOK_TAB_RIGHT_DOWN, 225, "command_auinotebook_tab_right_down"},
#endif
#if wxCHECK_VERSION(2,8,5)
   {wxEVT_COMMAND_AUINOTEBOOK_TAB_RIGHT_UP, 225, "command_auinotebook_tab_right_up"},
#endif
#if wxCHECK_VERSION(2,8,5)
   {wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSED, 225, "command_auinotebook_page_closed"},
#endif
#if wxCHECK_VERSION(2,8,5)
   {wxEVT_COMMAND_AUINOTEBOOK_DRAG_DONE, 225, "command_auinotebook_drag_done"},
#endif
#if wxCHECK_VERSION(2,8,5)
   {wxEVT_COMMAND_AUINOTEBOOK_BG_DCLICK, 225, "command_auinotebook_bg_dclick"},
#endif
   {wxEVT_AUI_PANE_BUTTON, 226, "aui_pane_button"},
   {wxEVT_AUI_PANE_CLOSE, 226, "aui_pane_close"},
   {wxEVT_AUI_PANE_MAXIMIZE, 226, "aui_pane_maximize"},
   {wxEVT_AUI_PANE_RESTORE, 226, "aui_pane_restore"},
#if wxCHECK_VERSION(2,9,5)
   {wxEVT_AUI_PANE_ACTIVATED, 226, "aui_pane_activated"},
#endif
   {wxEVT_AUI_RENDER, 226, "aui_render"},
   {wxEVT_AUI_FIND_MANAGER, 226, "aui_find_manager"},
   {wxEVT_TASKBAR_MOVE, 229, "taskbar_move"},
   {wxEVT_TASKBAR_LEFT_DOWN, 229, "taskbar_left_down"},
   {wxEVT_TASKBAR_LEFT_UP, 229, "taskbar_left_up"},
   {wxEVT_TASKBAR_RIGHT_DOWN, 229, "taskbar_right_down"},
   {wxEVT_TASKBAR_RIGHT_UP, 229, "taskbar_right_up"},
   {wxEVT_TASKBAR_LEFT_DCLICK, 229, "taskbar_left_dclick"},
   {wxEVT_TASKBAR_RIGHT_DCLICK, 229, "taskbar_right_dclick"},
   {wxEVT_INIT_DIALOG, 230, "init_dialog"},
   {wxEVT_ACTIVATE, 232, "activate"},
   {wxEVT_ACTIVATE_APP, 232, "activate_app"},
   {wxEVT_HIBERNATE, 232, "hibernate"},
   {wxEVT_MOUSE_CAPTURE_LOST, 235, "mouse_capture_lost"},
   {wxEVT_DROP_FILES, 238, "drop_files"},
   {-1, 0, }
  };
  for(int i=0; event_types[i].ev_type != -1; i++) {
     if(NULL == etmap[event_types[i].ev_type]) {
       etmap[event_types[i].ev_type] =
        new wxeEtype(event_types[i].ev_name, event_types[i].class_id);
     } else {
       wxeEtype *prev = etmap[event_types[i].ev_type];
       wxString msg(wxT("Duplicate event defs: "));
       msg += wxString::FromAscii(event_types[i].ev_name);
       msg += wxString::Format(wxT(" %d "), event_types[i].class_id);
       msg += wxString::FromAscii(prev->eName);
       msg += wxString::Format(wxT(" %d"), prev->cID);
       send_msg("internal_error", &msg);
     }
  }
}

bool sendevent(wxEvent *event, wxeMemEnv *memenv)
{
  int send_res ;
  char * evClass = NULL;
  wxMBConvUTF32 UTFconverter;
  wxeEtype *Etype = etmap[event->GetEventType()];
  wxeEvtListener *cb = (wxeEvtListener *)event->m_callbackUserData;
  WxeApp * app = (WxeApp *) wxTheApp;
  if(!memenv) return 0;

  wxeReturn rt = wxeReturn(memenv->tmp_env, cb->listener);
  ERL_NIF_TERM ev_term;
  switch(Etype->cID) {
  case 165: {// wxCommandEvent
    wxCommandEvent * ev = (wxCommandEvent *) event;
    evClass = (char*)"wxCommandEvent";
    ev_term = enif_make_tuple5(rt.env,
        rt.make_atom((char*)"wxCommand"),
        rt.make_atom(Etype->eName),
        rt.make(ev->GetString()),
        rt.make_int(ev->GetInt()),
        rt.make_int(ev->GetExtraLong()));
    break;
  }
  case 166: {// wxScrollEvent or wxSpinEvent
    if(event->IsKindOf(CLASSINFO(wxScrollEvent))) {
    wxScrollEvent * ev = (wxScrollEvent *) event;
    evClass = (char*)"wxScrollEvent";
    ev_term = enif_make_tuple4(rt.env,
        rt.make_atom((char*)"wxScroll"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetInt()),
        rt.make_int(ev->GetExtraLong()));
    } else {
      Etype = etmap[event->GetEventType() + wxEVT_USER_FIRST];
    wxSpinEvent * ev = (wxSpinEvent *) event;
    evClass = (char*)"wxSpinEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxSpin"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetInt()));
  }
    break;
  }
  case 167: {// wxScrollWinEvent
    wxScrollWinEvent * ev = (wxScrollWinEvent *) event;
    evClass = (char*)"wxScrollWinEvent";
    ev_term = enif_make_tuple4(rt.env,
        rt.make_atom((char*)"wxScrollWin"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetPosition()),
        rt.make_int(ev->GetOrientation()));
    break;
  }
  case 168: {// wxMouseEvent
    wxMouseEvent * ev = (wxMouseEvent *) event;
    evClass = (char*)"wxMouseEvent";
    ev_term = enif_make_tuple(rt.env,14,
        rt.make_atom((char*)"wxMouse"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->m_x),
        rt.make_int(ev->m_y),
        rt.make_bool(ev->m_leftDown),
        rt.make_bool(ev->m_middleDown),
        rt.make_bool(ev->m_rightDown),
        rt.make_bool(ev->m_controlDown),
        rt.make_bool(ev->m_shiftDown),
        rt.make_bool(ev->m_altDown),
        #if wxCHECK_VERSION(2,9,0) && defined(_MACOSX)
 rt.make_bool(ev->m_rawControlDown)
#else
 rt.make_bool(ev->m_metaDown)
#endif
,
        rt.make_int(ev->m_wheelRotation),
        rt.make_int(ev->m_wheelDelta),
        rt.make_int(ev->m_linesPerAction));
    break;
  }
  case 169: {// wxSetCursorEvent
    wxSetCursorEvent * ev = (wxSetCursorEvent *) event;
    wxCursor * GetCursor = new wxCursor(ev->GetCursor());
    app->newPtr((void *) GetCursor,3, memenv);
    evClass = (char*)"wxSetCursorEvent";
    ev_term = enif_make_tuple5(rt.env,
        rt.make_atom((char*)"wxSetCursor"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetX()),
        rt.make_int(ev->GetY()),
        rt.make_ref(app->getRef((void *)GetCursor,memenv), "wxCursor"));
    break;
  }
  case 170: {// wxKeyEvent
    wxKeyEvent * ev = (wxKeyEvent *) event;
    evClass = (char*)"wxKeyEvent";
    ev_term = enif_make_tuple(rt.env,13,
        rt.make_atom((char*)"wxKey"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->m_x),
        rt.make_int(ev->m_y),
        rt.make_int(ev->m_keyCode),
        rt.make_bool(ev->m_controlDown),
        rt.make_bool(ev->m_shiftDown),
        rt.make_bool(ev->m_altDown),
        #if wxCHECK_VERSION(2,9,0) && defined(_MACOSX)
 rt.make_bool(ev->m_rawControlDown)
#else
 rt.make_bool(ev->m_metaDown)
#endif
,
        #if !wxCHECK_VERSION(2,9,0)
 rt.make_bool(ev->m_scanCode)
#else
 rt.make_bool(false)
#endif
,
        rt.make_int(ev->m_uniChar),
        rt.make_uint(ev->m_rawCode),
        rt.make_uint(ev->m_rawFlags));
    break;
  }
  case 171: {// wxSizeEvent
    wxSizeEvent * ev = (wxSizeEvent *) event;
    evClass = (char*)"wxSizeEvent";
    ev_term = enif_make_tuple4(rt.env,
        rt.make_atom((char*)"wxSize"),
        rt.make_atom(Etype->eName),
        rt.make(ev->m_size),
        rt.make(ev->m_rect));
    break;
  }
  case 172: {// wxMoveEvent
    wxMoveEvent * ev = (wxMoveEvent *) event;
    evClass = (char*)"wxMoveEvent";
    ev_term = enif_make_tuple4(rt.env,
        rt.make_atom((char*)"wxMove"),
        rt.make_atom(Etype->eName),
        rt.make(ev->GetPosition()),
        rt.make(ev->GetRect()));
    break;
  }
  case 173: {// wxPaintEvent
    evClass = (char*)"wxPaintEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxPaint"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 174: {// wxEraseEvent
    wxEraseEvent * ev = (wxEraseEvent *) event;
     wxDC * GetDC = ev->GetDC();
    evClass = (char*)"wxEraseEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxErase"),
        rt.make_atom(Etype->eName),
        rt.make_ref(app->getRef((void *)GetDC,memenv), "wxDC"));
    break;
  }
  case 175: {// wxFocusEvent
    wxFocusEvent * ev = (wxFocusEvent *) event;
     wxWindow * GetWindow = ev->GetWindow();
    evClass = (char*)"wxFocusEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxFocus"),
        rt.make_atom(Etype->eName),
        rt.make_ref(app->getRef((void *)GetWindow,memenv), "wxWindow"));
    break;
  }
  case 176: {// wxChildFocusEvent
    evClass = (char*)"wxChildFocusEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxChildFocus"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 177: {// wxMenuEvent
    wxMenuEvent * ev = (wxMenuEvent *) event;
     wxMenu * GetMenu = ev->GetMenu();
    evClass = (char*)"wxMenuEvent";
    ev_term = enif_make_tuple4(rt.env,
        rt.make_atom((char*)"wxMenu"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetMenuId()),
        rt.make_ref(app->getRef((void *)GetMenu,memenv), "wxMenu"));
    break;
  }
  case 178: {// wxCloseEvent
    evClass = (char*)"wxCloseEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxClose"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 179: {// wxShowEvent
    wxShowEvent * ev = (wxShowEvent *) event;
    evClass = (char*)"wxShowEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxShow"),
        rt.make_atom(Etype->eName),
        rt.make_bool(ev->GetShow()));
    break;
  }
  case 180: {// wxIconizeEvent
    wxIconizeEvent * ev = (wxIconizeEvent *) event;
    evClass = (char*)"wxIconizeEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxIconize"),
        rt.make_atom(Etype->eName),
        rt.make_bool(ev->Iconized()));
    break;
  }
  case 181: {// wxMaximizeEvent
    evClass = (char*)"wxMaximizeEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxMaximize"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 182: {// wxJoystickEvent
    wxJoystickEvent * ev = (wxJoystickEvent *) event;
    evClass = (char*)"wxJoystickEvent";
    ev_term = enif_make_tuple7(rt.env,
        rt.make_atom((char*)"wxJoystick"),
        rt.make_atom(Etype->eName),
        rt.make(ev->GetPosition()),
        rt.make_int(ev->GetZPosition()),
        rt.make_int(ev->GetButtonChange()),
        rt.make_int(ev->GetButtonState()),
        rt.make_int(ev->GetJoystick()));
    break;
  }
  case 183: {// wxUpdateUIEvent
    evClass = (char*)"wxUpdateUIEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxUpdateUI"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 184: {// wxSysColourChangedEvent
    evClass = (char*)"wxSysColourChangedEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxSysColourChanged"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 185: {// wxMouseCaptureChangedEvent
    evClass = (char*)"wxMouseCaptureChangedEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxMouseCaptureChanged"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 186: {// wxDisplayChangedEvent
    evClass = (char*)"wxDisplayChangedEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxDisplayChanged"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 187: {// wxPaletteChangedEvent
    evClass = (char*)"wxPaletteChangedEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxPaletteChanged"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 188: {// wxQueryNewPaletteEvent
    evClass = (char*)"wxQueryNewPaletteEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxQueryNewPalette"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 189: {// wxNavigationKeyEvent
    wxNavigationKeyEvent * ev = (wxNavigationKeyEvent *) event;
    evClass = (char*)"wxNavigationKeyEvent";
    ev_term = enif_make_tuple4(rt.env,
        rt.make_atom((char*)"wxNavigationKey"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->m_flags),
        rt.make_ref(app->getRef((void *)ev->m_focus,memenv), "wxWindow"));
    break;
  }
  case 190: {// wxWindowCreateEvent
    evClass = (char*)"wxWindowCreateEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxWindowCreate"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 191: {// wxWindowDestroyEvent
    evClass = (char*)"wxWindowDestroyEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxWindowDestroy"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 192: {// wxHelpEvent
    evClass = (char*)"wxHelpEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxHelp"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 193: {// wxContextMenuEvent
    wxContextMenuEvent * ev = (wxContextMenuEvent *) event;
    evClass = (char*)"wxContextMenuEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxContextMenu"),
        rt.make_atom(Etype->eName),
        rt.make(ev->GetPosition()));
    break;
  }
  case 194: {// wxIdleEvent
    evClass = (char*)"wxIdleEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxIdle"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 195: {// wxGridEvent
    wxGridEvent * ev = (wxGridEvent *) event;
    evClass = (char*)"wxGridEvent";
    ev_term = enif_make_tuple(rt.env,11,
        rt.make_atom((char*)"wxGrid"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetRow()),
        rt.make_int(ev->GetCol()),
        rt.make_int(ev->GetPosition().x),
        rt.make_int(ev->GetPosition().y),
        rt.make_bool(ev->Selecting()),
        rt.make_bool(ev->ControlDown()),
        rt.make_bool(ev->MetaDown()),
        rt.make_bool(ev->ShiftDown()),
        rt.make_bool(ev->AltDown()));
    break;
  }
  case 197: {// wxSashEvent
    wxSashEvent * ev = (wxSashEvent *) event;
    evClass = (char*)"wxSashEvent";
    ev_term = enif_make_tuple5(rt.env,
        rt.make_atom((char*)"wxSash"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetEdge()),
        rt.make(ev->GetDragRect()),
        rt.make_int(ev->GetDragStatus()));
    break;
  }
  case 198: {// wxListEvent
    wxListEvent * ev = (wxListEvent *) event;
    evClass = (char*)"wxListEvent";
    ev_term = enif_make_tuple7(rt.env,
        rt.make_atom((char*)"wxList"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetKeyCode()),
        rt.make_int(ev->m_oldItemIndex),
        rt.make_int(ev->GetIndex()),
        rt.make_int(ev->m_col),
        rt.make(ev->GetPoint()));
    break;
  }
  case 199: {// wxDateEvent
    wxDateEvent * ev = (wxDateEvent *) event;
    evClass = (char*)"wxDateEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxDate"),
        rt.make_atom(Etype->eName),
        rt.make(ev->GetDate()));
    break;
  }
  case 200: {// wxCalendarEvent
    wxCalendarEvent * ev = (wxCalendarEvent *) event;
    evClass = (char*)"wxCalendarEvent";
    ev_term = enif_make_tuple4(rt.env,
        rt.make_atom((char*)"wxCalendar"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetWeekDay()),
        rt.make(ev->GetDate()));
    break;
  }
  case 201: {// wxFileDirPickerEvent
    wxFileDirPickerEvent * ev = (wxFileDirPickerEvent *) event;
    evClass = (char*)"wxFileDirPickerEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxFileDirPicker"),
        rt.make_atom(Etype->eName),
        rt.make(ev->GetPath()));
    break;
  }
  case 202: {// wxColourPickerEvent
    wxColourPickerEvent * ev = (wxColourPickerEvent *) event;
    evClass = (char*)"wxColourPickerEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxColourPicker"),
        rt.make_atom(Etype->eName),
        rt.make(ev->GetColour()));
    break;
  }
  case 203: {// wxFontPickerEvent
    wxFontPickerEvent * ev = (wxFontPickerEvent *) event;
    wxFont * GetFont = new wxFont(ev->GetFont());
    app->newPtr((void *) GetFont,3, memenv);
    evClass = (char*)"wxFontPickerEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxFontPicker"),
        rt.make_atom(Etype->eName),
        rt.make_ref(app->getRef((void *)GetFont,memenv), "wxFont"));
    break;
  }
  case 204: {// wxStyledTextEvent
    wxStyledTextEvent * ev = (wxStyledTextEvent *) event;
    evClass = (char*)"wxStyledTextEvent";
    ev_term = enif_make_tuple(rt.env,22,
        rt.make_atom((char*)"wxStyledText"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetPosition()),
        rt.make_int(ev->GetKey()),
        rt.make_int(ev->GetModifiers()),
        rt.make_int(ev->GetModificationType()),
        rt.make(ev->GetText()),
        rt.make_int(ev->GetLength()),
        rt.make_int(ev->GetLinesAdded()),
        rt.make_int(ev->GetLine()),
        rt.make_int(ev->GetFoldLevelNow()),
        rt.make_int(ev->GetFoldLevelPrev()),
        rt.make_int(ev->GetMargin()),
        rt.make_int(ev->GetMessage()),
        rt.make_int(ev->GetWParam()),
        rt.make_int(ev->GetLParam()),
        rt.make_int(ev->GetListType()),
        rt.make_int(ev->GetX()),
        rt.make_int(ev->GetY()),
        rt.make(ev->GetDragText()),
        rt.make_bool(ev->GetDragAllowMove()),
        rt.make_int(ev->GetDragResult()));
    break;
  }
  case 210: {// wxTreeEvent
    wxTreeEvent * ev = (wxTreeEvent *) event;
    evClass = (char*)"wxTreeEvent";
    ev_term = enif_make_tuple5(rt.env,
        rt.make_atom((char*)"wxTree"),
        rt.make_atom(Etype->eName),
        rt.make((wxUIntPtr *) ev->GetItem().m_pItem),
        rt.make((wxUIntPtr *) ev->GetOldItem().m_pItem),
        rt.make(ev->GetPoint()));
    break;
  }
  case 211: {// wxNotebookEvent
    wxNotebookEvent * ev = (wxNotebookEvent *) event;
    evClass = (char*)"wxNotebookEvent";
    ev_term = enif_make_tuple4(rt.env,
        rt.make_atom((char*)"wxNotebook"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetSelection()),
        rt.make_int(ev->GetOldSelection()));
    break;
  }
  case 217: {// wxClipboardTextEvent
    evClass = (char*)"wxClipboardTextEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxClipboardText"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 218: {// wxSpinEvent
    wxSpinEvent * ev = (wxSpinEvent *) event;
    evClass = (char*)"wxSpinEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxSpin"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetInt()));
    break;
  }
  case 220: {// wxSplitterEvent
    evClass = (char*)"wxSplitterEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxSplitter"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 222: {// wxHtmlLinkEvent
    wxHtmlLinkEvent * ev = (wxHtmlLinkEvent *) event;
    evClass = (char*)"wxHtmlLinkEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxHtmlLink"),
        rt.make_atom(Etype->eName),
        rt.make(ev->GetLinkInfo()));
    break;
  }
  case 225: {// wxAuiNotebookEvent
    wxAuiNotebookEvent * ev = (wxAuiNotebookEvent *) event;
     wxAuiNotebook * GetDragSource = ev->GetDragSource();
    evClass = (char*)"wxAuiNotebookEvent";
    ev_term = enif_make_tuple5(rt.env,
        rt.make_atom((char*)"wxAuiNotebook"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->GetOldSelection()),
        rt.make_int(ev->GetSelection()),
        rt.make_ref(app->getRef((void *)GetDragSource,memenv), "wxAuiNotebook"));
    break;
  }
  case 226: {// wxAuiManagerEvent
    wxAuiManagerEvent * ev = (wxAuiManagerEvent *) event;
     wxAuiManager * GetManager = ev->GetManager();
     wxAuiPaneInfo * GetPane = ev->GetPane();
     wxDC * GetDC = ev->GetDC();
    evClass = (char*)"wxAuiManagerEvent";
    ev_term = enif_make_tuple(rt.env,8,
        rt.make_atom((char*)"wxAuiManager"),
        rt.make_atom(Etype->eName),
        rt.make_ref(app->getRef((void *)GetManager,memenv), "wxAuiManager"),
        rt.make_ref(app->getRef((void *)GetPane,memenv), "wxAuiPaneInfo"),
        rt.make_int(ev->GetButton()),
        rt.make_bool(ev->veto_flag),
        rt.make_bool(ev->canveto_flag),
        rt.make_ref(app->getRef((void *)GetDC,memenv), "wxDC"));
    break;
  }
  case 229: {// wxTaskBarIconEvent
    evClass = (char*)"wxTaskBarIconEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxTaskBarIcon"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 230: {// wxInitDialogEvent
    evClass = (char*)"wxInitDialogEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxInitDialog"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 232: {// wxActivateEvent
    wxActivateEvent * ev = (wxActivateEvent *) event;
    evClass = (char*)"wxActivateEvent";
    ev_term = enif_make_tuple3(rt.env,
        rt.make_atom((char*)"wxActivate"),
        rt.make_atom(Etype->eName),
        rt.make_bool(ev->GetActive()));
    break;
  }
  case 235: {// wxMouseCaptureLostEvent
    evClass = (char*)"wxMouseCaptureLostEvent";
    ev_term = enif_make_tuple2(rt.env,
        rt.make_atom((char*)"wxMouseCaptureLost"),
        rt.make_atom(Etype->eName)
);
    break;
  }
  case 238: {// wxDropFilesEvent
    wxDropFilesEvent * ev = (wxDropFilesEvent *) event;
    evClass = (char*)"wxDropFilesEvent";
    ev_term = enif_make_tuple5(rt.env,
        rt.make_atom((char*)"wxDropFiles"),
        rt.make_atom(Etype->eName),
        rt.make_int(ev->m_noFiles),
        rt.make(ev->m_pos),
        rt.make_list_strings(ev->m_noFiles, ev->m_files)
);
    break;
  }
  }

  ERL_NIF_TERM wx_ev =
    enif_make_tuple5(rt.env,
                     rt.make_atom((char*)"wx"),
                     rt.make_int((int) event->GetId()),
                     rt.make_ref(cb->obj, cb->class_name),
                     rt.make_ext2term(cb->user_data),
                     ev_term);

  if(cb->fun_id) {
    ERL_NIF_TERM wx_cb =
      enif_make_tuple4(rt.env,
                       rt.make_atom("_wx_invoke_cb_"),
                       rt.make_int(cb->fun_id),
                       wx_ev,
                       rt.make_ref(app->getRef((void *)event,memenv), evClass)
                       );
    pre_callback();
    send_res =  rt.send(wx_cb);
    if(send_res) handle_event_callback(memenv, cb->listener);
    app->clearPtr((void *) event);
  } else {
    send_res =  rt.send(wx_ev);
    if(cb->skip) event->Skip();
    if(app->recurse_level < 1 && (Etype->cID == 171 || Etype->cID == 172)) {
      app->recurse_level++;
      app->dispatch_cmds();
      app->recurse_level--;
    }
  };
  return send_res;
}
