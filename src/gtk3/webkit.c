/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */





#line 4 "codegen-3.0/webkit.override"
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <webkit/webkit.h>
#include <nsp/nsp.h>
#include <nsp/object.h>
#include <nsp/smatrix.h>
#include <nsp/matrix.h>
#include <nsp/list.h>
#include <nsp/hash.h>
#include <nsp/hobj.h>
#include <nsp/file.h>
#include <nsp/type.h>

/* on windows TRUE and FALSE are undef by
 * "nsp/object.h"
 */

#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

#include "nsp/interf.h"

#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gobject-util.h>

#ifndef HAVE_WEBKIT_ZOOM
gfloat webkit_web_view_get_zoom_level(WebKitWebView        *web_view) {return 1.0;}
void webkit_web_view_set_zoom_level (WebKitWebView        *web_view,
				     gfloat                zoom_level) {}
void webkit_web_view_zoom_in(WebKitWebView        *web_view) {};
void webkit_web_view_zoom_out(WebKitWebView        *web_view) {};
gboolean webkit_web_view_get_full_content_zoom(WebKitWebView *web_view) {return TRUE;}
void webkit_web_view_set_full_content_zoom(WebKitWebView *web_view,
					   gboolean full_content_zoom) {};
#endif


#line 71 "webkit.c"
/* ---------- forward type declarations ---------- */
#include <nsp/gtk/webkitwebview.h>
#include <nsp/gtk/webkitwebframe.h>
#include <nsp/gtk/webkitwebhistoryitem.h>
#include <nsp/gtk/webkitwebbackforwardlist.h>
#include <nsp/gtk/webkitwebsettings.h>
#include <nsp/gtk/webkitnetworkrequest.h>


/* -----------NspWebKitWebView ----------- */


#define  NspWebKitWebView_Private 
#include <nsp/objects.h>
#include <nsp/gtk/webkitwebview.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspWebKitWebView inherits from GtkContainer 
 */

int nsp_type_webkitwebview_id=0;
NspTypeWebKitWebView *nsp_type_webkitwebview=NULL;

/*
 * Type object for NspWebKitWebView 
 * all the instance of NspTypeWebKitWebView share the same id. 
 * nsp_type_webkitwebview: is an instance of NspTypeWebKitWebView 
 *    used for objects of NspWebKitWebView type (i.e built with new_webkitwebview) 
 * other instances are used for derived classes 
 */
NspTypeWebKitWebView *new_type_webkitwebview(type_mode mode)
{
  NspTypeWebKitWebView *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_webkitwebview != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_webkitwebview;
    }
  if (( type =  malloc(sizeof(NspTypeGtkContainer))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtkcontainer(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = webkitwebview_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = webkitwebview_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_webkitwebview;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for webkitwebview */ 

  top->s_type =  (s_type_func *) nsp_webkitwebview_type_as_string;
  top->sh_type = (sh_type_func *) nsp_webkitwebview_type_short_string;
  /* top->create = (create_func*) int_webkitwebview_create;*/

  /* specific methods for webkitwebview */

  type->init = (init_func *) init_webkitwebview;

  /* 
   * NspWebKitWebView interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_webkitwebview_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeWebKitWebView called nsp_type_webkitwebview
       */
      type->id =  nsp_type_webkitwebview_id = nsp_new_type_id();
      nsp_type_webkitwebview = type;
      if ( nsp_register_type(nsp_type_webkitwebview) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_webkitwebview, WEBKIT_TYPE_WEB_VIEW);
      return ( mode == T_BASE ) ? type : new_type_webkitwebview(mode);
    }
  else 
    {
      type->id = nsp_type_webkitwebview_id;
      return type;
    }
}

/*
 * initialize NspWebKitWebView instances 
 * locally and by calling initializer on parent class 
 */

static int init_webkitwebview(NspWebKitWebView *Obj,NspTypeWebKitWebView *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspWebKitWebView 
 */

NspWebKitWebView *new_webkitwebview() 
{
  NspWebKitWebView *loc;
  /* type must exists */
  nsp_type_webkitwebview = new_type_webkitwebview(T_BASE);
  if ( (loc = malloc(sizeof(NspWebKitWebView)))== NULLWEBKITWEBVIEW) return loc;
  /* initialize object */
  if ( init_webkitwebview(loc,nsp_type_webkitwebview) == FAIL) return NULLWEBKITWEBVIEW;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspWebKitWebView 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char webkitwebview_type_name[]="WebKitWebView";
static char webkitwebview_short_type_name[]="WebKitWebView";

static char *nsp_webkitwebview_type_as_string(void)
{
  return(webkitwebview_type_name);
}

static char *nsp_webkitwebview_type_short_string(NspObject *v)
{
  return(webkitwebview_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspWebKitWebView objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspWebKitWebView   *nsp_webkitwebview_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_webkitwebview_id)  == TRUE  ) return ((NspWebKitWebView *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_webkitwebview));
  return NULL;
}

int IsWebKitWebViewObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_webkitwebview_id);
}

int IsWebKitWebView(NspObject *O)
{
  return nsp_object_type(O,nsp_type_webkitwebview_id);
}

NspWebKitWebView  *GetWebKitWebViewCopy(Stack stack, int i)
{
  if (  GetWebKitWebView(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspWebKitWebView  *GetWebKitWebView(Stack stack, int i)
{
  NspWebKitWebView *M;
  if (( M = nsp_webkitwebview_object(NthObj(i))) == NULLWEBKITWEBVIEW)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspWebKitWebView *webkitwebview_copy(NspWebKitWebView *self)
{
  /* return gtkcontainer_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebview);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebview);
}

/*-------------------------------------------------------------------
 * wrappers for the WebKitWebView
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_webkit_web_view_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)webkit_web_view_new())== NULL) return RET_BUG;

  nsp_type_webkitwebview = new_type_webkitwebview(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebview);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_view_set_maintains_back_forward_list(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int flag;
  if ( GetArgs(stack,rhs,opt,T,&flag) == FAIL) return RET_BUG;
    webkit_web_view_set_maintains_back_forward_list(WEBKIT_WEB_VIEW(self->obj),flag);
  return 0;
}

static int _wrap_webkit_web_view_get_back_forward_list(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebBackForwardList *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_view_get_back_forward_list(WEBKIT_WEB_VIEW(self->obj));
  nsp_type_webkitwebbackforwardlist = new_type_webkitwebbackforwardlist(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebbackforwardlist))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_view_go_to_back_forward_item(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *item;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_webkitwebhistoryitem, &item) == FAIL) return RET_BUG;
    ret =webkit_web_view_go_to_back_forward_item(WEBKIT_WEB_VIEW(self->obj),WEBKIT_WEB_HISTORY_ITEM(item->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_can_go_back(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_view_can_go_back(WEBKIT_WEB_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_can_go_forward(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_view_can_go_forward(WEBKIT_WEB_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_go_back(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_go_back(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_go_forward(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_go_forward(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_go_back_or_forward(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int steps;
  if ( GetArgs(stack,rhs,opt,T,&steps) == FAIL) return RET_BUG;
    webkit_web_view_go_back_or_forward(WEBKIT_WEB_VIEW(self->obj),steps);
  return 0;
}

static int _wrap_webkit_web_view_stop_loading(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_stop_loading(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_open(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *uri;
  if ( GetArgs(stack,rhs,opt,T,&uri) == FAIL) return RET_BUG;
    webkit_web_view_open(WEBKIT_WEB_VIEW(self->obj),uri);
  return 0;
}

static int _wrap_webkit_web_view_reload(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_reload(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_load_html_string(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *content, *base_uri;
  if ( GetArgs(stack,rhs,opt,T,&content, &base_uri) == FAIL) return RET_BUG;
    webkit_web_view_load_html_string(WEBKIT_WEB_VIEW(self->obj),content,base_uri);
  return 0;
}

static int _wrap_webkit_web_view_load_string(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,string,string, t_end};
  char *content, *content_mimetype, *content_encoding, *base_uri;
  if ( GetArgs(stack,rhs,opt,T,&content, &content_mimetype, &content_encoding, &base_uri) == FAIL) return RET_BUG;
    webkit_web_view_load_string(WEBKIT_WEB_VIEW(self->obj),content,content_mimetype,content_encoding,base_uri);
  return 0;
}

static int _wrap_webkit_web_view_get_main_frame(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebFrame *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_view_get_main_frame(WEBKIT_WEB_VIEW(self->obj));
  nsp_type_webkitwebframe = new_type_webkitwebframe(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebframe))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_view_get_focused_frame(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebFrame *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_view_get_focused_frame(WEBKIT_WEB_VIEW(self->obj));
  nsp_type_webkitwebframe = new_type_webkitwebframe(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebframe))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_view_execute_script(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *script;
  if ( GetArgs(stack,rhs,opt,T,&script) == FAIL) return RET_BUG;
    webkit_web_view_execute_script(WEBKIT_WEB_VIEW(self->obj),script);
  return 0;
}

static int _wrap_webkit_web_view_get_editable(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_view_get_editable(WEBKIT_WEB_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_set_editable(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int flag;
  if ( GetArgs(stack,rhs,opt,T,&flag) == FAIL) return RET_BUG;
    webkit_web_view_set_editable(WEBKIT_WEB_VIEW(self->obj),flag);
  return 0;
}

static int _wrap_webkit_web_view_search_text(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_bool,s_bool,s_bool, t_end};
  char *string;
  int case_sensitive, forward, wrap, ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &case_sensitive, &forward, &wrap) == FAIL) return RET_BUG;
    ret =webkit_web_view_search_text(WEBKIT_WEB_VIEW(self->obj),string,case_sensitive,forward,wrap);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_mark_text_matches(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_bool,s_int, t_end};
  char *string;
  int case_sensitive, limit, ret;
  if ( GetArgs(stack,rhs,opt,T,&string, &case_sensitive, &limit) == FAIL) return RET_BUG;
    ret =webkit_web_view_mark_text_matches(WEBKIT_WEB_VIEW(self->obj),string,case_sensitive,limit);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_set_highlight_text_matches(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int highlight;
  if ( GetArgs(stack,rhs,opt,T,&highlight) == FAIL) return RET_BUG;
    webkit_web_view_set_highlight_text_matches(WEBKIT_WEB_VIEW(self->obj),highlight);
  return 0;
}

static int _wrap_webkit_web_view_can_cut_clipboard(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_can_cut_clipboard(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_can_copy_clipboard(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_can_copy_clipboard(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_can_paste_clipboard(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_can_paste_clipboard(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_paste_clipboard(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_paste_clipboard(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_delete_selection(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_delete_selection(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_has_selection(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_view_has_selection(WEBKIT_WEB_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_select_all(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_select_all(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_set_settings(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *settings;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_webkitwebsettings, &settings) == FAIL) return RET_BUG;
    webkit_web_view_set_settings(WEBKIT_WEB_VIEW(self->obj),WEBKIT_WEB_SETTINGS(settings->obj));
  return 0;
}

static int _wrap_webkit_web_view_get_settings(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebSettings *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_view_get_settings(WEBKIT_WEB_VIEW(self->obj));
  nsp_type_webkitwebsettings = new_type_webkitwebsettings(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebsettings))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_view_get_transparent(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_view_get_transparent(WEBKIT_WEB_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_set_transparent(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int flag;
  if ( GetArgs(stack,rhs,opt,T,&flag) == FAIL) return RET_BUG;
    webkit_web_view_set_transparent(WEBKIT_WEB_VIEW(self->obj),flag);
  return 0;
}

static int _wrap_webkit_web_view_get_zoom_level(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  double ret;
  CheckRhs(0,0);
    ret =webkit_web_view_get_zoom_level(WEBKIT_WEB_VIEW(self->obj));
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_set_zoom_level(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_double, t_end};
  double zoom_level;
  if ( GetArgs(stack,rhs,opt,T,&zoom_level) == FAIL) return RET_BUG;
    webkit_web_view_set_zoom_level(WEBKIT_WEB_VIEW(self->obj),zoom_level);
  return 0;
}

static int _wrap_webkit_web_view_zoom_in(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_zoom_in(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_zoom_out(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_view_zoom_out(WEBKIT_WEB_VIEW(self->obj));
  return 0;
}

static int _wrap_webkit_web_view_get_full_content_zoom(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_view_get_full_content_zoom(WEBKIT_WEB_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_view_set_full_content_zoom(NspWebKitWebView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int full_content_zoom;
  if ( GetArgs(stack,rhs,opt,T,&full_content_zoom) == FAIL) return RET_BUG;
    webkit_web_view_set_full_content_zoom(WEBKIT_WEB_VIEW(self->obj),full_content_zoom);
  return 0;
}

static NspMethods webkitwebview_methods[] = {
  {"set_maintains_back_forward_list",(nsp_method *) _wrap_webkit_web_view_set_maintains_back_forward_list},
  {"get_back_forward_list",(nsp_method *) _wrap_webkit_web_view_get_back_forward_list},
  {"go_to_back_forward_item",(nsp_method *) _wrap_webkit_web_view_go_to_back_forward_item},
  {"can_go_back",(nsp_method *) _wrap_webkit_web_view_can_go_back},
  {"can_go_forward",(nsp_method *) _wrap_webkit_web_view_can_go_forward},
  {"go_back",(nsp_method *) _wrap_webkit_web_view_go_back},
  {"go_forward",(nsp_method *) _wrap_webkit_web_view_go_forward},
  {"go_back_or_forward",(nsp_method *) _wrap_webkit_web_view_go_back_or_forward},
  {"stop_loading",(nsp_method *) _wrap_webkit_web_view_stop_loading},
  {"open",(nsp_method *) _wrap_webkit_web_view_open},
  {"reload",(nsp_method *) _wrap_webkit_web_view_reload},
  {"load_html_string",(nsp_method *) _wrap_webkit_web_view_load_html_string},
  {"load_string",(nsp_method *) _wrap_webkit_web_view_load_string},
  {"get_main_frame",(nsp_method *) _wrap_webkit_web_view_get_main_frame},
  {"get_focused_frame",(nsp_method *) _wrap_webkit_web_view_get_focused_frame},
  {"execute_script",(nsp_method *) _wrap_webkit_web_view_execute_script},
  {"get_editable",(nsp_method *) _wrap_webkit_web_view_get_editable},
  {"set_editable",(nsp_method *) _wrap_webkit_web_view_set_editable},
  {"search_text",(nsp_method *) _wrap_webkit_web_view_search_text},
  {"mark_text_matches",(nsp_method *) _wrap_webkit_web_view_mark_text_matches},
  {"set_highlight_text_matches",(nsp_method *) _wrap_webkit_web_view_set_highlight_text_matches},
  {"can_cut_clipboard",(nsp_method *) _wrap_webkit_web_view_can_cut_clipboard},
  {"can_copy_clipboard",(nsp_method *) _wrap_webkit_web_view_can_copy_clipboard},
  {"can_paste_clipboard",(nsp_method *) _wrap_webkit_web_view_can_paste_clipboard},
  {"paste_clipboard",(nsp_method *) _wrap_webkit_web_view_paste_clipboard},
  {"delete_selection",(nsp_method *) _wrap_webkit_web_view_delete_selection},
  {"has_selection",(nsp_method *) _wrap_webkit_web_view_has_selection},
  {"select_all",(nsp_method *) _wrap_webkit_web_view_select_all},
  {"set_settings",(nsp_method *) _wrap_webkit_web_view_set_settings},
  {"get_settings",(nsp_method *) _wrap_webkit_web_view_get_settings},
  {"get_transparent",(nsp_method *) _wrap_webkit_web_view_get_transparent},
  {"set_transparent",(nsp_method *) _wrap_webkit_web_view_set_transparent},
  {"get_zoom_level",(nsp_method *) _wrap_webkit_web_view_get_zoom_level},
  {"set_zoom_level",(nsp_method *) _wrap_webkit_web_view_set_zoom_level},
  {"zoom_in",(nsp_method *) _wrap_webkit_web_view_zoom_in},
  {"zoom_out",(nsp_method *) _wrap_webkit_web_view_zoom_out},
  {"get_full_content_zoom",(nsp_method *) _wrap_webkit_web_view_get_full_content_zoom},
  {"set_full_content_zoom",(nsp_method *) _wrap_webkit_web_view_set_full_content_zoom},
  { NULL, NULL}
};

static NspMethods *webkitwebview_get_methods(void) { return webkitwebview_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab webkitwebview_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspWebKitWebFrame ----------- */


#define  NspWebKitWebFrame_Private 
#include <nsp/objects.h>
#include <nsp/gtk/webkitwebframe.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspWebKitWebFrame inherits from GObject 
 */

int nsp_type_webkitwebframe_id=0;
NspTypeWebKitWebFrame *nsp_type_webkitwebframe=NULL;

/*
 * Type object for NspWebKitWebFrame 
 * all the instance of NspTypeWebKitWebFrame share the same id. 
 * nsp_type_webkitwebframe: is an instance of NspTypeWebKitWebFrame 
 *    used for objects of NspWebKitWebFrame type (i.e built with new_webkitwebframe) 
 * other instances are used for derived classes 
 */
NspTypeWebKitWebFrame *new_type_webkitwebframe(type_mode mode)
{
  NspTypeWebKitWebFrame *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_webkitwebframe != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_webkitwebframe;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = webkitwebframe_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = webkitwebframe_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_webkitwebframe;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for webkitwebframe */ 

  top->s_type =  (s_type_func *) nsp_webkitwebframe_type_as_string;
  top->sh_type = (sh_type_func *) nsp_webkitwebframe_type_short_string;
  /* top->create = (create_func*) int_webkitwebframe_create;*/

  /* specific methods for webkitwebframe */

  type->init = (init_func *) init_webkitwebframe;

  /* 
   * NspWebKitWebFrame interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_webkitwebframe_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeWebKitWebFrame called nsp_type_webkitwebframe
       */
      type->id =  nsp_type_webkitwebframe_id = nsp_new_type_id();
      nsp_type_webkitwebframe = type;
      if ( nsp_register_type(nsp_type_webkitwebframe) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_webkitwebframe, WEBKIT_TYPE_WEB_FRAME);
      return ( mode == T_BASE ) ? type : new_type_webkitwebframe(mode);
    }
  else 
    {
      type->id = nsp_type_webkitwebframe_id;
      return type;
    }
}

/*
 * initialize NspWebKitWebFrame instances 
 * locally and by calling initializer on parent class 
 */

static int init_webkitwebframe(NspWebKitWebFrame *Obj,NspTypeWebKitWebFrame *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspWebKitWebFrame 
 */

NspWebKitWebFrame *new_webkitwebframe() 
{
  NspWebKitWebFrame *loc;
  /* type must exists */
  nsp_type_webkitwebframe = new_type_webkitwebframe(T_BASE);
  if ( (loc = malloc(sizeof(NspWebKitWebFrame)))== NULLWEBKITWEBFRAME) return loc;
  /* initialize object */
  if ( init_webkitwebframe(loc,nsp_type_webkitwebframe) == FAIL) return NULLWEBKITWEBFRAME;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspWebKitWebFrame 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char webkitwebframe_type_name[]="WebKitWebFrame";
static char webkitwebframe_short_type_name[]="WebKitWebFrame";

static char *nsp_webkitwebframe_type_as_string(void)
{
  return(webkitwebframe_type_name);
}

static char *nsp_webkitwebframe_type_short_string(NspObject *v)
{
  return(webkitwebframe_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspWebKitWebFrame objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspWebKitWebFrame   *nsp_webkitwebframe_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_webkitwebframe_id)  == TRUE  ) return ((NspWebKitWebFrame *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_webkitwebframe));
  return NULL;
}

int IsWebKitWebFrameObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_webkitwebframe_id);
}

int IsWebKitWebFrame(NspObject *O)
{
  return nsp_object_type(O,nsp_type_webkitwebframe_id);
}

NspWebKitWebFrame  *GetWebKitWebFrameCopy(Stack stack, int i)
{
  if (  GetWebKitWebFrame(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspWebKitWebFrame  *GetWebKitWebFrame(Stack stack, int i)
{
  NspWebKitWebFrame *M;
  if (( M = nsp_webkitwebframe_object(NthObj(i))) == NULLWEBKITWEBFRAME)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspWebKitWebFrame *webkitwebframe_copy(NspWebKitWebFrame *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebframe);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebframe);
}

/*-------------------------------------------------------------------
 * wrappers for the WebKitWebFrame
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_webkit_web_frame_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *view;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_webkitwebview, &view) == FAIL) return RET_BUG;
  if ((ret = (GObject *)webkit_web_frame_new(WEBKIT_WEB_VIEW(view->obj)))== NULL) return RET_BUG;

  nsp_type_webkitwebframe = new_type_webkitwebframe(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebframe);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_frame_get_web_view(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebView *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_frame_get_web_view(WEBKIT_WEB_FRAME(self->obj));
  nsp_type_webkitwebview = new_type_webkitwebview(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebview))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_frame_get_name(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =webkit_web_frame_get_name(WEBKIT_WEB_FRAME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_frame_get_title(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =webkit_web_frame_get_title(WEBKIT_WEB_FRAME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_frame_get_uri(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =webkit_web_frame_get_uri(WEBKIT_WEB_FRAME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_frame_get_parent(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebFrame *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_frame_get_parent(WEBKIT_WEB_FRAME(self->obj));
  nsp_type_webkitwebframe = new_type_webkitwebframe(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebframe))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_frame_load_request(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *request;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_webkitnetworkrequest, &request) == FAIL) return RET_BUG;
    webkit_web_frame_load_request(WEBKIT_WEB_FRAME(self->obj),WEBKIT_NETWORK_REQUEST(request->obj));
  return 0;
}

static int _wrap_webkit_web_frame_stop_loading(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_frame_stop_loading(WEBKIT_WEB_FRAME(self->obj));
  return 0;
}

static int _wrap_webkit_web_frame_reload(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_frame_reload(WEBKIT_WEB_FRAME(self->obj));
  return 0;
}

static int _wrap_webkit_web_frame_find_frame(NspWebKitWebFrame *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  WebKitWebFrame *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =webkit_web_frame_find_frame(WEBKIT_WEB_FRAME(self->obj),name);
  nsp_type_webkitwebframe = new_type_webkitwebframe(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebframe))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods webkitwebframe_methods[] = {
  {"get_web_view",(nsp_method *) _wrap_webkit_web_frame_get_web_view},
  {"get_name",(nsp_method *) _wrap_webkit_web_frame_get_name},
  {"get_title",(nsp_method *) _wrap_webkit_web_frame_get_title},
  {"get_uri",(nsp_method *) _wrap_webkit_web_frame_get_uri},
  {"get_parent",(nsp_method *) _wrap_webkit_web_frame_get_parent},
  {"load_request",(nsp_method *) _wrap_webkit_web_frame_load_request},
  {"stop_loading",(nsp_method *) _wrap_webkit_web_frame_stop_loading},
  {"reload",(nsp_method *) _wrap_webkit_web_frame_reload},
  {"find_frame",(nsp_method *) _wrap_webkit_web_frame_find_frame},
  { NULL, NULL}
};

static NspMethods *webkitwebframe_get_methods(void) { return webkitwebframe_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab webkitwebframe_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspWebKitWebHistoryItem ----------- */


#define  NspWebKitWebHistoryItem_Private 
#include <nsp/objects.h>
#include <nsp/gtk/webkitwebhistoryitem.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspWebKitWebHistoryItem inherits from GObject 
 */

int nsp_type_webkitwebhistoryitem_id=0;
NspTypeWebKitWebHistoryItem *nsp_type_webkitwebhistoryitem=NULL;

/*
 * Type object for NspWebKitWebHistoryItem 
 * all the instance of NspTypeWebKitWebHistoryItem share the same id. 
 * nsp_type_webkitwebhistoryitem: is an instance of NspTypeWebKitWebHistoryItem 
 *    used for objects of NspWebKitWebHistoryItem type (i.e built with new_webkitwebhistoryitem) 
 * other instances are used for derived classes 
 */
NspTypeWebKitWebHistoryItem *new_type_webkitwebhistoryitem(type_mode mode)
{
  NspTypeWebKitWebHistoryItem *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_webkitwebhistoryitem != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_webkitwebhistoryitem;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = webkitwebhistoryitem_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = webkitwebhistoryitem_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_webkitwebhistoryitem;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for webkitwebhistoryitem */ 

  top->s_type =  (s_type_func *) nsp_webkitwebhistoryitem_type_as_string;
  top->sh_type = (sh_type_func *) nsp_webkitwebhistoryitem_type_short_string;
  /* top->create = (create_func*) int_webkitwebhistoryitem_create;*/

  /* specific methods for webkitwebhistoryitem */

  type->init = (init_func *) init_webkitwebhistoryitem;

  /* 
   * NspWebKitWebHistoryItem interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_webkitwebhistoryitem_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeWebKitWebHistoryItem called nsp_type_webkitwebhistoryitem
       */
      type->id =  nsp_type_webkitwebhistoryitem_id = nsp_new_type_id();
      nsp_type_webkitwebhistoryitem = type;
      if ( nsp_register_type(nsp_type_webkitwebhistoryitem) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_webkitwebhistoryitem, WEBKIT_TYPE_WEB_HISTORY_ITEM);
      return ( mode == T_BASE ) ? type : new_type_webkitwebhistoryitem(mode);
    }
  else 
    {
      type->id = nsp_type_webkitwebhistoryitem_id;
      return type;
    }
}

/*
 * initialize NspWebKitWebHistoryItem instances 
 * locally and by calling initializer on parent class 
 */

static int init_webkitwebhistoryitem(NspWebKitWebHistoryItem *Obj,NspTypeWebKitWebHistoryItem *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspWebKitWebHistoryItem 
 */

NspWebKitWebHistoryItem *new_webkitwebhistoryitem() 
{
  NspWebKitWebHistoryItem *loc;
  /* type must exists */
  nsp_type_webkitwebhistoryitem = new_type_webkitwebhistoryitem(T_BASE);
  if ( (loc = malloc(sizeof(NspWebKitWebHistoryItem)))== NULLWEBKITWEBHISTORYITEM) return loc;
  /* initialize object */
  if ( init_webkitwebhistoryitem(loc,nsp_type_webkitwebhistoryitem) == FAIL) return NULLWEBKITWEBHISTORYITEM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspWebKitWebHistoryItem 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char webkitwebhistoryitem_type_name[]="WebKitWebHistoryItem";
static char webkitwebhistoryitem_short_type_name[]="WebKitWebHistoryItem";

static char *nsp_webkitwebhistoryitem_type_as_string(void)
{
  return(webkitwebhistoryitem_type_name);
}

static char *nsp_webkitwebhistoryitem_type_short_string(NspObject *v)
{
  return(webkitwebhistoryitem_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspWebKitWebHistoryItem objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspWebKitWebHistoryItem   *nsp_webkitwebhistoryitem_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_webkitwebhistoryitem_id)  == TRUE  ) return ((NspWebKitWebHistoryItem *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_webkitwebhistoryitem));
  return NULL;
}

int IsWebKitWebHistoryItemObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_webkitwebhistoryitem_id);
}

int IsWebKitWebHistoryItem(NspObject *O)
{
  return nsp_object_type(O,nsp_type_webkitwebhistoryitem_id);
}

NspWebKitWebHistoryItem  *GetWebKitWebHistoryItemCopy(Stack stack, int i)
{
  if (  GetWebKitWebHistoryItem(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspWebKitWebHistoryItem  *GetWebKitWebHistoryItem(Stack stack, int i)
{
  NspWebKitWebHistoryItem *M;
  if (( M = nsp_webkitwebhistoryitem_object(NthObj(i))) == NULLWEBKITWEBHISTORYITEM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspWebKitWebHistoryItem *webkitwebhistoryitem_copy(NspWebKitWebHistoryItem *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebhistoryitem);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebhistoryitem);
}

/*-------------------------------------------------------------------
 * wrappers for the WebKitWebHistoryItem
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_webkit_web_history_item_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)webkit_web_history_item_new())== NULL) return RET_BUG;

  nsp_type_webkitwebhistoryitem = new_type_webkitwebhistoryitem(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebhistoryitem);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_history_item_get_title(NspWebKitWebHistoryItem *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =webkit_web_history_item_get_title(WEBKIT_WEB_HISTORY_ITEM(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_history_item_get_alternate_title(NspWebKitWebHistoryItem *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =webkit_web_history_item_get_alternate_title(WEBKIT_WEB_HISTORY_ITEM(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_history_item_set_alternate_title(NspWebKitWebHistoryItem *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *title;
  if ( GetArgs(stack,rhs,opt,T,&title) == FAIL) return RET_BUG;
    webkit_web_history_item_set_alternate_title(WEBKIT_WEB_HISTORY_ITEM(self->obj),title);
  return 0;
}

static int _wrap_webkit_web_history_item_get_uri(NspWebKitWebHistoryItem *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =webkit_web_history_item_get_uri(WEBKIT_WEB_HISTORY_ITEM(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_history_item_get_original_uri(NspWebKitWebHistoryItem *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =webkit_web_history_item_get_original_uri(WEBKIT_WEB_HISTORY_ITEM(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_history_item_get_last_visited_time(NspWebKitWebHistoryItem *self,Stack stack,int rhs,int opt,int lhs)
{
  double ret;
  CheckRhs(0,0);
    ret =webkit_web_history_item_get_last_visited_time(WEBKIT_WEB_HISTORY_ITEM(self->obj));
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods webkitwebhistoryitem_methods[] = {
  {"get_title",(nsp_method *) _wrap_webkit_web_history_item_get_title},
  {"get_alternate_title",(nsp_method *) _wrap_webkit_web_history_item_get_alternate_title},
  {"set_alternate_title",(nsp_method *) _wrap_webkit_web_history_item_set_alternate_title},
  {"get_uri",(nsp_method *) _wrap_webkit_web_history_item_get_uri},
  {"get_original_uri",(nsp_method *) _wrap_webkit_web_history_item_get_original_uri},
  {"get_last_visited_time",(nsp_method *) _wrap_webkit_web_history_item_get_last_visited_time},
  { NULL, NULL}
};

static NspMethods *webkitwebhistoryitem_get_methods(void) { return webkitwebhistoryitem_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab webkitwebhistoryitem_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspWebKitWebBackForwardList ----------- */


#define  NspWebKitWebBackForwardList_Private 
#include <nsp/objects.h>
#include <nsp/gtk/webkitwebbackforwardlist.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspWebKitWebBackForwardList inherits from GObject 
 */

int nsp_type_webkitwebbackforwardlist_id=0;
NspTypeWebKitWebBackForwardList *nsp_type_webkitwebbackforwardlist=NULL;

/*
 * Type object for NspWebKitWebBackForwardList 
 * all the instance of NspTypeWebKitWebBackForwardList share the same id. 
 * nsp_type_webkitwebbackforwardlist: is an instance of NspTypeWebKitWebBackForwardList 
 *    used for objects of NspWebKitWebBackForwardList type (i.e built with new_webkitwebbackforwardlist) 
 * other instances are used for derived classes 
 */
NspTypeWebKitWebBackForwardList *new_type_webkitwebbackforwardlist(type_mode mode)
{
  NspTypeWebKitWebBackForwardList *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_webkitwebbackforwardlist != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_webkitwebbackforwardlist;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = webkitwebbackforwardlist_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = webkitwebbackforwardlist_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_webkitwebbackforwardlist;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for webkitwebbackforwardlist */ 

  top->s_type =  (s_type_func *) nsp_webkitwebbackforwardlist_type_as_string;
  top->sh_type = (sh_type_func *) nsp_webkitwebbackforwardlist_type_short_string;
  /* top->create = (create_func*) int_webkitwebbackforwardlist_create;*/

  /* specific methods for webkitwebbackforwardlist */

  type->init = (init_func *) init_webkitwebbackforwardlist;

  /* 
   * NspWebKitWebBackForwardList interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_webkitwebbackforwardlist_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeWebKitWebBackForwardList called nsp_type_webkitwebbackforwardlist
       */
      type->id =  nsp_type_webkitwebbackforwardlist_id = nsp_new_type_id();
      nsp_type_webkitwebbackforwardlist = type;
      if ( nsp_register_type(nsp_type_webkitwebbackforwardlist) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_webkitwebbackforwardlist, WEBKIT_TYPE_WEB_BACK_FORWARD_LIST);
      return ( mode == T_BASE ) ? type : new_type_webkitwebbackforwardlist(mode);
    }
  else 
    {
      type->id = nsp_type_webkitwebbackforwardlist_id;
      return type;
    }
}

/*
 * initialize NspWebKitWebBackForwardList instances 
 * locally and by calling initializer on parent class 
 */

static int init_webkitwebbackforwardlist(NspWebKitWebBackForwardList *Obj,NspTypeWebKitWebBackForwardList *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspWebKitWebBackForwardList 
 */

NspWebKitWebBackForwardList *new_webkitwebbackforwardlist() 
{
  NspWebKitWebBackForwardList *loc;
  /* type must exists */
  nsp_type_webkitwebbackforwardlist = new_type_webkitwebbackforwardlist(T_BASE);
  if ( (loc = malloc(sizeof(NspWebKitWebBackForwardList)))== NULLWEBKITWEBBACKFORWARDLIST) return loc;
  /* initialize object */
  if ( init_webkitwebbackforwardlist(loc,nsp_type_webkitwebbackforwardlist) == FAIL) return NULLWEBKITWEBBACKFORWARDLIST;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspWebKitWebBackForwardList 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char webkitwebbackforwardlist_type_name[]="WebKitWebBackForwardList";
static char webkitwebbackforwardlist_short_type_name[]="WebKitWebBackForwardList";

static char *nsp_webkitwebbackforwardlist_type_as_string(void)
{
  return(webkitwebbackforwardlist_type_name);
}

static char *nsp_webkitwebbackforwardlist_type_short_string(NspObject *v)
{
  return(webkitwebbackforwardlist_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspWebKitWebBackForwardList objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspWebKitWebBackForwardList   *nsp_webkitwebbackforwardlist_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_webkitwebbackforwardlist_id)  == TRUE  ) return ((NspWebKitWebBackForwardList *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_webkitwebbackforwardlist));
  return NULL;
}

int IsWebKitWebBackForwardListObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_webkitwebbackforwardlist_id);
}

int IsWebKitWebBackForwardList(NspObject *O)
{
  return nsp_object_type(O,nsp_type_webkitwebbackforwardlist_id);
}

NspWebKitWebBackForwardList  *GetWebKitWebBackForwardListCopy(Stack stack, int i)
{
  if (  GetWebKitWebBackForwardList(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspWebKitWebBackForwardList  *GetWebKitWebBackForwardList(Stack stack, int i)
{
  NspWebKitWebBackForwardList *M;
  if (( M = nsp_webkitwebbackforwardlist_object(NthObj(i))) == NULLWEBKITWEBBACKFORWARDLIST)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspWebKitWebBackForwardList *webkitwebbackforwardlist_copy(NspWebKitWebBackForwardList *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebbackforwardlist);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebbackforwardlist);
}

/*-------------------------------------------------------------------
 * wrappers for the WebKitWebBackForwardList
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_webkit_web_back_forward_list_new_with_web_view (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *view;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_webkitwebview, &view) == FAIL) return RET_BUG;
  if ((ret = (GObject *)webkit_web_back_forward_list_new_with_web_view(WEBKIT_WEB_VIEW(view->obj)))== NULL) return RET_BUG;

  nsp_type_webkitwebbackforwardlist = new_type_webkitwebbackforwardlist(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebbackforwardlist);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_back_forward_list_go_forward(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_back_forward_list_go_forward(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj));
  return 0;
}

static int _wrap_webkit_web_back_forward_list_go_back(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    webkit_web_back_forward_list_go_back(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj));
  return 0;
}

static int _wrap_webkit_web_back_forward_list_contains_item(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *history_item;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_webkitwebhistoryitem, &history_item) == FAIL) return RET_BUG;
    ret =webkit_web_back_forward_list_contains_item(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj),WEBKIT_WEB_HISTORY_ITEM(history_item->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_back_forward_list_go_to_item(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *history_item;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_webkitwebhistoryitem, &history_item) == FAIL) return RET_BUG;
    webkit_web_back_forward_list_go_to_item(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj),WEBKIT_WEB_HISTORY_ITEM(history_item->obj));
  return 0;
}

static int _wrap_webkit_web_back_forward_list_get_forward_list_with_limit(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int limit;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&limit) == FAIL) return RET_BUG;
    ret =webkit_web_back_forward_list_get_forward_list_with_limit(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj),limit);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_webkit_web_back_forward_list_get_back_list_with_limit(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int limit;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&limit) == FAIL) return RET_BUG;
    ret =webkit_web_back_forward_list_get_back_list_with_limit(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj),limit);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_webkit_web_back_forward_list_get_back_item(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebHistoryItem *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_back_forward_list_get_back_item(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj));
  nsp_type_webkitwebhistoryitem = new_type_webkitwebhistoryitem(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebhistoryitem))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_back_forward_list_get_current_item(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebHistoryItem *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_back_forward_list_get_current_item(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj));
  nsp_type_webkitwebhistoryitem = new_type_webkitwebhistoryitem(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebhistoryitem))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_back_forward_list_get_forward_item(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebHistoryItem *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_back_forward_list_get_forward_item(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj));
  nsp_type_webkitwebhistoryitem = new_type_webkitwebhistoryitem(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebhistoryitem))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_back_forward_list_get_nth_item(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int index;
  WebKitWebHistoryItem *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&index) == FAIL) return RET_BUG;
    ret =webkit_web_back_forward_list_get_nth_item(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj),index);
  nsp_type_webkitwebhistoryitem = new_type_webkitwebhistoryitem(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebhistoryitem))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_back_forward_list_get_back_length(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_back_forward_list_get_back_length(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_back_forward_list_get_forward_length(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_back_forward_list_get_forward_length(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_back_forward_list_get_limit(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =webkit_web_back_forward_list_get_limit(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_webkit_web_back_forward_list_set_limit(NspWebKitWebBackForwardList *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int limit;
  if ( GetArgs(stack,rhs,opt,T,&limit) == FAIL) return RET_BUG;
    webkit_web_back_forward_list_set_limit(WEBKIT_WEB_BACK_FORWARD_LIST(self->obj),limit);
  return 0;
}

static NspMethods webkitwebbackforwardlist_methods[] = {
  {"go_forward",(nsp_method *) _wrap_webkit_web_back_forward_list_go_forward},
  {"go_back",(nsp_method *) _wrap_webkit_web_back_forward_list_go_back},
  {"contains_item",(nsp_method *) _wrap_webkit_web_back_forward_list_contains_item},
  {"go_to_item",(nsp_method *) _wrap_webkit_web_back_forward_list_go_to_item},
  {"get_forward_list_with_limit",(nsp_method *) _wrap_webkit_web_back_forward_list_get_forward_list_with_limit},
  {"get_back_list_with_limit",(nsp_method *) _wrap_webkit_web_back_forward_list_get_back_list_with_limit},
  {"get_back_item",(nsp_method *) _wrap_webkit_web_back_forward_list_get_back_item},
  {"get_current_item",(nsp_method *) _wrap_webkit_web_back_forward_list_get_current_item},
  {"get_forward_item",(nsp_method *) _wrap_webkit_web_back_forward_list_get_forward_item},
  {"get_nth_item",(nsp_method *) _wrap_webkit_web_back_forward_list_get_nth_item},
  {"get_back_length",(nsp_method *) _wrap_webkit_web_back_forward_list_get_back_length},
  {"get_forward_length",(nsp_method *) _wrap_webkit_web_back_forward_list_get_forward_length},
  {"get_limit",(nsp_method *) _wrap_webkit_web_back_forward_list_get_limit},
  {"set_limit",(nsp_method *) _wrap_webkit_web_back_forward_list_set_limit},
  { NULL, NULL}
};

static NspMethods *webkitwebbackforwardlist_get_methods(void) { return webkitwebbackforwardlist_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab webkitwebbackforwardlist_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspWebKitWebSettings ----------- */


#define  NspWebKitWebSettings_Private 
#include <nsp/objects.h>
#include <nsp/gtk/webkitwebsettings.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspWebKitWebSettings inherits from GObject 
 */

int nsp_type_webkitwebsettings_id=0;
NspTypeWebKitWebSettings *nsp_type_webkitwebsettings=NULL;

/*
 * Type object for NspWebKitWebSettings 
 * all the instance of NspTypeWebKitWebSettings share the same id. 
 * nsp_type_webkitwebsettings: is an instance of NspTypeWebKitWebSettings 
 *    used for objects of NspWebKitWebSettings type (i.e built with new_webkitwebsettings) 
 * other instances are used for derived classes 
 */
NspTypeWebKitWebSettings *new_type_webkitwebsettings(type_mode mode)
{
  NspTypeWebKitWebSettings *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_webkitwebsettings != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_webkitwebsettings;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = webkitwebsettings_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = webkitwebsettings_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_webkitwebsettings;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for webkitwebsettings */ 

  top->s_type =  (s_type_func *) nsp_webkitwebsettings_type_as_string;
  top->sh_type = (sh_type_func *) nsp_webkitwebsettings_type_short_string;
  /* top->create = (create_func*) int_webkitwebsettings_create;*/

  /* specific methods for webkitwebsettings */

  type->init = (init_func *) init_webkitwebsettings;

  /* 
   * NspWebKitWebSettings interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_webkitwebsettings_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeWebKitWebSettings called nsp_type_webkitwebsettings
       */
      type->id =  nsp_type_webkitwebsettings_id = nsp_new_type_id();
      nsp_type_webkitwebsettings = type;
      if ( nsp_register_type(nsp_type_webkitwebsettings) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_webkitwebsettings, WEBKIT_TYPE_WEB_SETTINGS);
      return ( mode == T_BASE ) ? type : new_type_webkitwebsettings(mode);
    }
  else 
    {
      type->id = nsp_type_webkitwebsettings_id;
      return type;
    }
}

/*
 * initialize NspWebKitWebSettings instances 
 * locally and by calling initializer on parent class 
 */

static int init_webkitwebsettings(NspWebKitWebSettings *Obj,NspTypeWebKitWebSettings *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspWebKitWebSettings 
 */

NspWebKitWebSettings *new_webkitwebsettings() 
{
  NspWebKitWebSettings *loc;
  /* type must exists */
  nsp_type_webkitwebsettings = new_type_webkitwebsettings(T_BASE);
  if ( (loc = malloc(sizeof(NspWebKitWebSettings)))== NULLWEBKITWEBSETTINGS) return loc;
  /* initialize object */
  if ( init_webkitwebsettings(loc,nsp_type_webkitwebsettings) == FAIL) return NULLWEBKITWEBSETTINGS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspWebKitWebSettings 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char webkitwebsettings_type_name[]="WebKitWebSettings";
static char webkitwebsettings_short_type_name[]="WebKitWebSettings";

static char *nsp_webkitwebsettings_type_as_string(void)
{
  return(webkitwebsettings_type_name);
}

static char *nsp_webkitwebsettings_type_short_string(NspObject *v)
{
  return(webkitwebsettings_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspWebKitWebSettings objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspWebKitWebSettings   *nsp_webkitwebsettings_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_webkitwebsettings_id)  == TRUE  ) return ((NspWebKitWebSettings *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_webkitwebsettings));
  return NULL;
}

int IsWebKitWebSettingsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_webkitwebsettings_id);
}

int IsWebKitWebSettings(NspObject *O)
{
  return nsp_object_type(O,nsp_type_webkitwebsettings_id);
}

NspWebKitWebSettings  *GetWebKitWebSettingsCopy(Stack stack, int i)
{
  if (  GetWebKitWebSettings(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspWebKitWebSettings  *GetWebKitWebSettings(Stack stack, int i)
{
  NspWebKitWebSettings *M;
  if (( M = nsp_webkitwebsettings_object(NthObj(i))) == NULLWEBKITWEBSETTINGS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspWebKitWebSettings *webkitwebsettings_copy(NspWebKitWebSettings *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebsettings);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitwebsettings);
}

/*-------------------------------------------------------------------
 * wrappers for the WebKitWebSettings
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_webkit_web_settings_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)webkit_web_settings_new())== NULL) return RET_BUG;

  nsp_type_webkitwebsettings = new_type_webkitwebsettings(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebsettings);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_web_settings_copy(NspWebKitWebSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  WebKitWebSettings *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =webkit_web_settings_copy(WEBKIT_WEB_SETTINGS(self->obj));
  nsp_type_webkitwebsettings = new_type_webkitwebsettings(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitwebsettings))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods webkitwebsettings_methods[] = {
  {"copy",(nsp_method *) _wrap_webkit_web_settings_copy},
  { NULL, NULL}
};

static NspMethods *webkitwebsettings_get_methods(void) { return webkitwebsettings_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab webkitwebsettings_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspWebKitNetworkRequest ----------- */


#define  NspWebKitNetworkRequest_Private 
#include <nsp/objects.h>
#include <nsp/gtk/webkitnetworkrequest.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspWebKitNetworkRequest inherits from GObject 
 */

int nsp_type_webkitnetworkrequest_id=0;
NspTypeWebKitNetworkRequest *nsp_type_webkitnetworkrequest=NULL;

/*
 * Type object for NspWebKitNetworkRequest 
 * all the instance of NspTypeWebKitNetworkRequest share the same id. 
 * nsp_type_webkitnetworkrequest: is an instance of NspTypeWebKitNetworkRequest 
 *    used for objects of NspWebKitNetworkRequest type (i.e built with new_webkitnetworkrequest) 
 * other instances are used for derived classes 
 */
NspTypeWebKitNetworkRequest *new_type_webkitnetworkrequest(type_mode mode)
{
  NspTypeWebKitNetworkRequest *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_webkitnetworkrequest != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_webkitnetworkrequest;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = webkitnetworkrequest_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = webkitnetworkrequest_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_webkitnetworkrequest;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for webkitnetworkrequest */ 

  top->s_type =  (s_type_func *) nsp_webkitnetworkrequest_type_as_string;
  top->sh_type = (sh_type_func *) nsp_webkitnetworkrequest_type_short_string;
  /* top->create = (create_func*) int_webkitnetworkrequest_create;*/

  /* specific methods for webkitnetworkrequest */

  type->init = (init_func *) init_webkitnetworkrequest;

  /* 
   * NspWebKitNetworkRequest interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_webkitnetworkrequest_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeWebKitNetworkRequest called nsp_type_webkitnetworkrequest
       */
      type->id =  nsp_type_webkitnetworkrequest_id = nsp_new_type_id();
      nsp_type_webkitnetworkrequest = type;
      if ( nsp_register_type(nsp_type_webkitnetworkrequest) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_webkitnetworkrequest, WEBKIT_TYPE_NETWORK_REQUEST);
      return ( mode == T_BASE ) ? type : new_type_webkitnetworkrequest(mode);
    }
  else 
    {
      type->id = nsp_type_webkitnetworkrequest_id;
      return type;
    }
}

/*
 * initialize NspWebKitNetworkRequest instances 
 * locally and by calling initializer on parent class 
 */

static int init_webkitnetworkrequest(NspWebKitNetworkRequest *Obj,NspTypeWebKitNetworkRequest *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspWebKitNetworkRequest 
 */

NspWebKitNetworkRequest *new_webkitnetworkrequest() 
{
  NspWebKitNetworkRequest *loc;
  /* type must exists */
  nsp_type_webkitnetworkrequest = new_type_webkitnetworkrequest(T_BASE);
  if ( (loc = malloc(sizeof(NspWebKitNetworkRequest)))== NULLWEBKITNETWORKREQUEST) return loc;
  /* initialize object */
  if ( init_webkitnetworkrequest(loc,nsp_type_webkitnetworkrequest) == FAIL) return NULLWEBKITNETWORKREQUEST;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspWebKitNetworkRequest 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char webkitnetworkrequest_type_name[]="WebKitNetworkRequest";
static char webkitnetworkrequest_short_type_name[]="WebKitNetworkRequest";

static char *nsp_webkitnetworkrequest_type_as_string(void)
{
  return(webkitnetworkrequest_type_name);
}

static char *nsp_webkitnetworkrequest_type_short_string(NspObject *v)
{
  return(webkitnetworkrequest_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspWebKitNetworkRequest objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspWebKitNetworkRequest   *nsp_webkitnetworkrequest_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_webkitnetworkrequest_id)  == TRUE  ) return ((NspWebKitNetworkRequest *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_webkitnetworkrequest));
  return NULL;
}

int IsWebKitNetworkRequestObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_webkitnetworkrequest_id);
}

int IsWebKitNetworkRequest(NspObject *O)
{
  return nsp_object_type(O,nsp_type_webkitnetworkrequest_id);
}

NspWebKitNetworkRequest  *GetWebKitNetworkRequestCopy(Stack stack, int i)
{
  if (  GetWebKitNetworkRequest(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspWebKitNetworkRequest  *GetWebKitNetworkRequest(Stack stack, int i)
{
  NspWebKitNetworkRequest *M;
  if (( M = nsp_webkitnetworkrequest_object(NthObj(i))) == NULLWEBKITNETWORKREQUEST)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspWebKitNetworkRequest *webkitnetworkrequest_copy(NspWebKitNetworkRequest *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitnetworkrequest);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_webkitnetworkrequest);
}

/*-------------------------------------------------------------------
 * wrappers for the WebKitNetworkRequest
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_webkit_network_request_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *uri;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uri) == FAIL) return RET_BUG;
  if ((ret = (GObject *)webkit_network_request_new(uri))== NULL) return RET_BUG;

  nsp_type_webkitnetworkrequest = new_type_webkitnetworkrequest(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_webkitnetworkrequest);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_webkit_network_request_set_uri(NspWebKitNetworkRequest *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *uri;
  if ( GetArgs(stack,rhs,opt,T,&uri) == FAIL) return RET_BUG;
    webkit_network_request_set_uri(WEBKIT_NETWORK_REQUEST(self->obj),uri);
  return 0;
}

static int _wrap_webkit_network_request_get_uri(NspWebKitNetworkRequest *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =webkit_network_request_get_uri(WEBKIT_NETWORK_REQUEST(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods webkitnetworkrequest_methods[] = {
  {"set_uri",(nsp_method *) _wrap_webkit_network_request_set_uri},
  {"get_uri",(nsp_method *) _wrap_webkit_network_request_get_uri},
  { NULL, NULL}
};

static NspMethods *webkitnetworkrequest_get_methods(void) { return webkitnetworkrequest_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab webkitnetworkrequest_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 64 "codegen-3.0/webkit.override"

static NspSMatrix *nsp_smatrix_from_data(const char *s,int lenght)
{
  return nsp_smatrix_split_string(s, "\n", TRUE);
}

static NspSMatrix *get_url (SoupSession *session,const char *url,
			    gboolean debug, gboolean quiet )
{
  gboolean head = FALSE;
  const char *name;
  SoupMessage *msg;
  const char *header;

  msg = soup_message_new (head ? "HEAD" : "GET", url);
  soup_message_set_flags (msg, SOUP_MESSAGE_NO_REDIRECT);

  soup_session_send_message (session, msg);

  name = soup_message_get_uri (msg)->path;

  if (debug || head)
    {
      SoupMessageHeadersIter iter;
      const char *hname, *value;
      char *path = soup_uri_to_string (soup_message_get_uri (msg), TRUE);

      g_print ("%s %s HTTP/1.%d\n", msg->method, path,
	       soup_message_get_http_version (msg));
      soup_message_headers_iter_init (&iter, msg->request_headers);
      while (soup_message_headers_iter_next (&iter, &hname, &value))
	{
	  g_print ("%s: %s\r\n", hname, value);
	}
      g_print ("\n");

      g_print ("HTTP/1.%d %d %s\n",
	       soup_message_get_http_version (msg),
	       msg->status_code, msg->reason_phrase);

      soup_message_headers_iter_init (&iter, msg->response_headers);
      while (soup_message_headers_iter_next (&iter, &hname, &value))
	{
	  g_print ("%s: %s\r\n", hname, value);
	}
      g_print ("\n");
    }
  else if (msg->status_code == SOUP_STATUS_SSL_FAILED)
    {
      /*
      GTlsCertificateFlags flags;
      if (soup_message_get_https_status (msg, NULL, &flags))
	g_print ("%s: %d %s (0x%x)\n", name, msg->status_code, msg->reason_phrase, flags);
      else
	g_print ("%s: %d %s (no handshake status)\n", name, msg->status_code, msg->reason_phrase);
      */
    }
  else if (!quiet || SOUP_STATUS_IS_TRANSPORT_ERROR (msg->status_code))
    {
      g_print ("%s: %d %s\n", name, msg->status_code, msg->reason_phrase);
    }

  if (SOUP_STATUS_IS_REDIRECTION (msg->status_code))
    {
      header = soup_message_headers_get_one (msg->response_headers,
					     "Location");
      if (header)
	{
	  SoupURI *uri;
	  char *uri_string;
	  if (!debug && !quiet)
	    g_print ("  -> %s\n", header);
	  uri = soup_uri_new_with_base (soup_message_get_uri (msg), header);
	  uri_string = soup_uri_to_string (uri, FALSE);
	  get_url (session, uri_string, debug,quiet);
	  g_free (uri_string);
	  soup_uri_free (uri);
	}
    }
  else if (SOUP_STATUS_IS_SUCCESSFUL (msg->status_code))
    {
      NspSMatrix *S;
      S = nsp_smatrix_from_data(msg->response_body->data,
				msg->response_body->length);
      return S;
    }
  return NULL;
}

NspSMatrix *get_file(const char *url, const char *proxy, const char *ca_file,
		     gboolean quiet, gboolean debug)
{
  NspSMatrix *S;
  SoupSession *session = NULL;
  GMainLoop *loop = NULL;
  gboolean synchronous=TRUE ;
  SoupURI *proxy_uri, *parsed;

  parsed = soup_uri_new (url);
  if (!parsed) {
    Scierror("Error: could not parse '%s' as a URL\n", url);
    return NULL;
  }
  soup_uri_free (parsed);

  session = g_object_new (synchronous ? SOUP_TYPE_SESSION_SYNC : SOUP_TYPE_SESSION_ASYNC,
			  SOUP_SESSION_SSL_CA_FILE, ca_file,
			  SOUP_SESSION_ADD_FEATURE_BY_TYPE, SOUP_TYPE_CONTENT_DECODER,
			  SOUP_SESSION_ADD_FEATURE_BY_TYPE, SOUP_TYPE_COOKIE_JAR,
			  SOUP_SESSION_USER_AGENT, "get ",
#ifdef SOUP_SESSION_ACCEPT_LANGUAGE_AUTO
			  SOUP_SESSION_ACCEPT_LANGUAGE_AUTO, TRUE,
#endif
			  /* SOUP_SESSION_USE_NTLM, ntlm, */
			  NULL);
  if (proxy)
    {
      proxy_uri = soup_uri_new (proxy);
      if (!proxy_uri)
	{
	   Scierror("Error: could not parse '%s' as URI for proxy\n",
		    proxy);
	   soup_uri_free (parsed);
	   return NULL;
	}
      g_object_set (G_OBJECT (session),
		    SOUP_SESSION_PROXY_URI, proxy_uri,
		    NULL);
      soup_uri_free (proxy_uri);
    }
  else
    {
      /* soup_session_add_feature_by_type (session, SOUP_TYPE_PROXY_RESOLVER_DEFAULT); */
    }

  if (!synchronous)
    {

      loop = g_main_loop_new (NULL, TRUE);
    }

  S = get_url (session, url, debug,quiet);

  if (!synchronous)
    {
      g_main_loop_unref (loop);
    }

  soup_session_abort (session);
  g_object_unref (session);

  return S;
}

static int _wrap_webkit_getfile(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  int debug=FALSE, quiet = TRUE;
  char *proxy=NULL, *ca_file= NULL, *url;
  CheckStdRhs(1,1);
  CheckLhs(0,1);
  nsp_option opts[] ={{ "proxy", string,NULLOBJ,-1},
		      { "ca_file", string,NULLOBJ,-1},
		      { "quiet", s_bool,NULLOBJ,-1},
		      { "debug", s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ((url=GetString(stack,1)) == NULL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&proxy,&ca_file,&quiet,&debug) == FAIL)
    return RET_BUG;
  if ((S=get_file(url, proxy,ca_file, quiet, debug)) == NULL)
    {
      Scierror("Error: failed to get file \"%s\"\n",url);
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}
#line 2292 "webkit.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab webkit_func[]={
  { "webkit_web_view_new", _wrap_webkit_web_view_new},
  { "webkitwebview_new", _wrap_webkit_web_view_new},
  { "webkit_web_frame_new", _wrap_webkit_web_frame_new},
  { "webkitwebframe_new", _wrap_webkit_web_frame_new},
  { "webkit_web_history_item_new", _wrap_webkit_web_history_item_new},
  { "webkitwebhistoryitem_new", _wrap_webkit_web_history_item_new},
  { "webkit_web_back_forward_list_new_with_web_view", _wrap_webkit_web_back_forward_list_new_with_web_view},
  { "webkit_web_settings_new", _wrap_webkit_web_settings_new},
  { "webkitwebsettings_new", _wrap_webkit_web_settings_new},
  { "webkit_network_request_new", _wrap_webkit_network_request_new},
  { "webkitnetworkrequest_new", _wrap_webkit_network_request_new},
  { "webkit_getfile", _wrap_webkit_getfile},
  { NULL, NULL}
};

/* call ith function in the webkit interface */

int webkit_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,webkit_func[i].fonc,
  					       &stack,rhs,opt,lhs);
#else
  return (*(webkit_func[i].fonc))(stack,rhs,opt,lhs);
#endif
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void webkit_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = webkit_func[i].name;
  *f = webkit_func[i].fonc;
}
void nsp_initialize_webkit_types(void)
{
  new_type_webkitwebview(T_BASE);
  new_type_webkitwebframe(T_BASE);
  new_type_webkitwebhistoryitem(T_BASE);
  new_type_webkitwebbackforwardlist(T_BASE);
  new_type_webkitwebsettings(T_BASE);
  new_type_webkitnetworkrequest(T_BASE);
}

#line 2346 "webkit.c"
