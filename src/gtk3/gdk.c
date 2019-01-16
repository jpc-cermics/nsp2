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





#line 4 "codegen-3.0/gdk.override"

#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <cairo/cairo.h>
#include <cairo/cairo-gobject.h>
#include <nsp/nsp.h>
#include <nsp/none.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gobject-util.h>
#include <nsp/gtk/cairo_t.h>

#include <nsp/gtk/cairo_surface_t.h>
#include <nsp/gtk/cairo_region_t.h>
#include <nsp/gtk/cairo_pattern_t.h>

#line 45 "gdk.c"
/* ---------- types from other modules ---------- */
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/pangocontext.h>
#include <nsp/gtk/pangofont.h>
#include <nsp/gtk/pangolayout.h>
#include <nsp/gtk/gtkwidget.h>
#include <nsp/gtk/gdkatom.h>
/* ---------- forward type declarations ---------- */
#include <nsp/gtk/gdkevent.h>
#include <nsp/gtk/gdkcolor.h>
#include <nsp/gtk/gdkrectangle.h>
#include <nsp/gtk/gdkrgba.h>
#include <nsp/gtk/gdkdevice.h>
#include <nsp/gtk/gdkdevicemanager.h>
#include <nsp/gtk/gdkdisplay.h>
#include <nsp/gtk/gdkdisplaymanager.h>
#include <nsp/gtk/gdkdragcontext.h>
#include <nsp/gtk/gdkframeclock.h>
#include <nsp/gtk/gdkglcontext.h>
#include <nsp/gtk/gdkwindow.h>
#include <nsp/gtk/gdkkeymap.h>
#include <nsp/gtk/gdkscreen.h>
#include <nsp/gtk/gdkvisual.h>
#include <nsp/gtk/gdkcursor.h>
#include <nsp/gtk/gdkpixbuf.h>
#include <nsp/gtk/gdkpixbufanimation.h>
#include <nsp/gtk/gdkpixbufanimationiter.h>
#include <nsp/gtk/gdkpixbufloader.h>


/* -----------NspGdkEvent ----------- */

static int _wrap_gdk_event_tp_getattr(Stack stack, int rhs, int opt, int lhs);

#define  NspGdkEvent_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkevent.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkEvent inherits from GBoxed 
 */

int nsp_type_gdkevent_id=0;
NspTypeGdkEvent *nsp_type_gdkevent=NULL;

/*
 * Type object for NspGdkEvent 
 * all the instance of NspTypeGdkEvent share the same id. 
 * nsp_type_gdkevent: is an instance of NspTypeGdkEvent 
 *    used for objects of NspGdkEvent type (i.e built with new_gdkevent) 
 * other instances are used for derived classes 
 */
NspTypeGdkEvent *new_type_gdkevent(type_mode mode)
{
  NspTypeGdkEvent *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkevent != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkevent;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkevent_attrs;
  type->get_attrs = (attrs_func *) _wrap_gdk_event_tp_getattr;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkevent_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkevent;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkevent */ 

  top->s_type =  (s_type_func *) nsp_gdkevent_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkevent_type_short_string;
  /* top->create = (create_func*) int_gdkevent_create;*/

  /* specific methods for gdkevent */

  type->init = (init_func *) init_gdkevent;

  /* 
   * NspGdkEvent interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkevent_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkEvent called nsp_type_gdkevent
       */
      type->id =  nsp_type_gdkevent_id = nsp_new_type_id();
      nsp_type_gdkevent = type;
      if ( nsp_register_type(nsp_type_gdkevent) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkevent, GDK_TYPE_EVENT);
      return ( mode == T_BASE ) ? type : new_type_gdkevent(mode);
    }
  else 
    {
      type->id = nsp_type_gdkevent_id;
      return type;
    }
}

/*
 * initialize NspGdkEvent instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkevent(NspGdkEvent *Obj,NspTypeGdkEvent *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkEvent 
 */

NspGdkEvent *new_gdkevent() 
{
  NspGdkEvent *loc;
  /* type must exists */
  nsp_type_gdkevent = new_type_gdkevent(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkEvent)))== NULLGDKEVENT) return loc;
  /* initialize object */
  if ( init_gdkevent(loc,nsp_type_gdkevent) == FAIL) return NULLGDKEVENT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkEvent 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkevent_type_name[]="GdkEvent";
static char gdkevent_short_type_name[]="GdkEvent";

static char *nsp_gdkevent_type_as_string(void)
{
  return(gdkevent_type_name);
}

static char *nsp_gdkevent_type_short_string(NspObject *v)
{
  return(gdkevent_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkEvent objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkEvent   *nsp_gdkevent_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkevent_id)  == TRUE  ) return ((NspGdkEvent *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkevent));
  return NULL;
}

int IsGdkEventObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkevent_id);
}

int IsGdkEvent(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkevent_id);
}

NspGdkEvent  *GetGdkEventCopy(Stack stack, int i)
{
  if (  GetGdkEvent(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkEvent  *GetGdkEvent(Stack stack, int i)
{
  NspGdkEvent *M;
  if (( M = nsp_gdkevent_object(NthObj(i))) == NULLGDKEVENT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGdkEvent *gdkevent_copy(NspGdkEvent *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gdkevent);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkEvent
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gdk_event_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, t_end};
  GdkEventType type;
  NspObject *nsp_type = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_EVENT_TYPE, nsp_type, &type)== FAIL)
      return RET_BUG;
  if ((ret = (GObject *)gdk_event_new(type))== NULL) return RET_BUG;

  nsp_type_gdkevent = new_type_gdkevent(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret,TRUE,TRUE,(NspTypeBase *) nsp_type_gdkevent);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_event_put(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  gdk_event_put(NSP_GBOXED_GET(self, GdkEvent));
  return 0;
}

static int _wrap_gdk_event_copy(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkEvent *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =gdk_event_copy(NSP_GBOXED_GET(self, GdkEvent));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_event_free(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  gdk_event_free(NSP_GBOXED_GET(self, GdkEvent));
  return 0;
}

#if GTK_CHECK_VERSION(3,10,0)
static int _wrap_gdk_event_get_window(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =gdk_event_get_window(NSP_GBOXED_GET(self, GdkEvent));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_event_get_window(Stack stack, int rhs, int opt, int lhs) /* get_window */
{
  Scierror("Error: function gdk_event_get_window not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_event_get_time(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  gulong ret;
  CheckRhs(0,0);
  ret =gdk_event_get_time(NSP_GBOXED_GET(self, GdkEvent));
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

#line 1111 "codegen-3.0/gdk.override"
static int
_wrap_gdk_event_get_coords(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GdkEvent *event = nspg_boxed_get(self, GdkEvent);
  gdouble x, y;
  int n = 0;
  if (gdk_event_get_coords(event, &x, &y)) n = 2;
  if ( nsp_move_doubles(stack,1,1,n,(double) x,(double) y) == FAIL) return RET_BUG;
  return 1;
}
#line 354 "gdk.c"


#line 1123 "codegen-3.0/gdk.override"
static int
_wrap_gdk_event_get_root_coords(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GdkEvent *event = nspg_boxed_get(self, GdkEvent);
  gdouble x, y;
  int n = 0;
  if (gdk_event_get_root_coords(event, &x, &y)) n= 2;
  if ( nsp_move_doubles(stack,1,1,n,(double) x,(double) y) == FAIL) return RET_BUG;
  return 1;
}
#line 368 "gdk.c"


#if GTK_CHECK_VERSION(3,4,0)
static int _wrap_gdk_event_get_scroll_deltas(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_double,s_double, t_end};
  double delta_x, delta_y;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&delta_x, &delta_y) == FAIL) return RET_BUG;
  ret =gdk_event_get_scroll_deltas(NSP_GBOXED_GET(self, GdkEvent),&delta_x,&delta_y);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_event_get_scroll_deltas(Stack stack, int rhs, int opt, int lhs) /* get_scroll_deltas */
{
  Scierror("Error: function gdk_event_get_scroll_deltas not available\n");
  return RET_BUG;
}
#endif
#line 1095 "codegen-3.0/gdk.override"
static int
_wrap_gdk_event_get_axis(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GdkEvent *event = nspg_boxed_get(self, GdkEvent);
  gint axis_use;
  gdouble value;
  int n= 0;
  CheckRhs(1,1);
  if (GetScalarDouble(stack,1,&value) == FAIL) return RET_BUG;
  axis_use = (int) value;
  if ( gdk_event_get_axis(event, axis_use, &value)) n = 1;
  if ( nsp_move_doubles(stack,1,1,n,(double) value) == FAIL) return RET_BUG;
  return 1;
}
#line 405 "gdk.c"


static int _wrap_gdk_event_set_device(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *device;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device) == FAIL) return RET_BUG;
  gdk_event_set_device(NSP_GBOXED_GET(self, GdkEvent),GDK_DEVICE(device->obj));
  return 0;
}

static int _wrap_gdk_event_get_device(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDevice *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =gdk_event_get_device(NSP_GBOXED_GET(self, GdkEvent));
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdevice))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_event_set_source_device(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *device;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device) == FAIL) return RET_BUG;
  gdk_event_set_source_device(NSP_GBOXED_GET(self, GdkEvent),GDK_DEVICE(device->obj));
  return 0;
}

static int _wrap_gdk_event_get_source_device(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDevice *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =gdk_event_get_source_device(NSP_GBOXED_GET(self, GdkEvent));
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdevice))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(3,4,0)
static int _wrap_gdk_event_triggers_context_menu(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret =gdk_event_triggers_context_menu(NSP_GBOXED_GET(self, GdkEvent));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_event_triggers_context_menu(Stack stack, int rhs, int opt, int lhs) /* triggers_context_menu */
{
  Scierror("Error: function gdk_event_triggers_context_menu not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_events_get_distance(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_double, t_end};
  GdkEvent *event2 = NULL;
  NspObject *nsp_event2 = NULL;
  double distance;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_event2, &distance) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_event2, GDK_TYPE_EVENT))
      event2 = nspg_boxed_get(nsp_event2, GdkEvent);
  else {
      Scierror( "Error: event2 should be a GdkEvent\n");
      return RET_BUG;
  }
  ret =gdk_events_get_distance(NSP_GBOXED_GET(self, GdkEvent),event2,&distance);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_events_get_angle(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_double, t_end};
  GdkEvent *event2 = NULL;
  NspObject *nsp_event2 = NULL;
  double angle;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_event2, &angle) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_event2, GDK_TYPE_EVENT))
      event2 = nspg_boxed_get(nsp_event2, GdkEvent);
  else {
      Scierror( "Error: event2 should be a GdkEvent\n");
      return RET_BUG;
  }
  ret =gdk_events_get_angle(NSP_GBOXED_GET(self, GdkEvent),event2,&angle);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_events_get_center(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_double,s_double, t_end};
  GdkEvent *event2 = NULL;
  NspObject *nsp_event2 = NULL;
  double x, y;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_event2, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_event2, GDK_TYPE_EVENT))
      event2 = nspg_boxed_get(nsp_event2, GdkEvent);
  else {
      Scierror( "Error: event2 should be a GdkEvent\n");
      return RET_BUG;
  }
  ret =gdk_events_get_center(NSP_GBOXED_GET(self, GdkEvent),event2,&x,&y);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_event_set_screen(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *screen;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkscreen, &screen) == FAIL) return RET_BUG;
  gdk_event_set_screen(NSP_GBOXED_GET(self, GdkEvent),GDK_SCREEN(screen->obj));
  return 0;
}

static int _wrap_gdk_event_get_screen(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkScreen *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =gdk_event_get_screen(NSP_GBOXED_GET(self, GdkEvent));
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(3,10,0)
static int _wrap_gdk_event_get_event_type(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
  ret =gdk_event_get_event_type(NSP_GBOXED_GET(self, GdkEvent));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_event_get_event_type(Stack stack, int rhs, int opt, int lhs) /* get_event_type */
{
  Scierror("Error: function gdk_event_get_event_type not available\n");
  return RET_BUG;
}
#endif
static NspMethods gdkevent_methods[] = {
  {"put",(nsp_method *) _wrap_gdk_event_put},
  {"copy",(nsp_method *) _wrap_gdk_event_copy},
  {"free",(nsp_method *) _wrap_gdk_event_free},
  {"get_window",(nsp_method *) _wrap_gdk_event_get_window},
  {"get_time",(nsp_method *) _wrap_gdk_event_get_time},
  {"get_coords",(nsp_method *) _wrap_gdk_event_get_coords},
  {"get_root_coords",(nsp_method *) _wrap_gdk_event_get_root_coords},
  {"get_scroll_deltas",(nsp_method *) _wrap_gdk_event_get_scroll_deltas},
  {"get_axis",(nsp_method *) _wrap_gdk_event_get_axis},
  {"set_device",(nsp_method *) _wrap_gdk_event_set_device},
  {"get_device",(nsp_method *) _wrap_gdk_event_get_device},
  {"set_source_device",(nsp_method *) _wrap_gdk_event_set_source_device},
  {"get_source_device",(nsp_method *) _wrap_gdk_event_get_source_device},
  {"triggers_context_menu",(nsp_method *) _wrap_gdk_event_triggers_context_menu},
  {"s_get_distance",(nsp_method *) _wrap_gdk_events_get_distance},
  {"s_get_angle",(nsp_method *) _wrap_gdk_events_get_angle},
  {"s_get_center",(nsp_method *) _wrap_gdk_events_get_center},
  {"set_screen",(nsp_method *) _wrap_gdk_event_set_screen},
  {"get_screen",(nsp_method *) _wrap_gdk_event_get_screen},
  {"get_event_type",(nsp_method *) _wrap_gdk_event_get_event_type},
  { NULL, NULL}
};

static NspMethods *gdkevent_get_methods(void) { return gdkevent_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkevent_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;
#line 751 "codegen-3.0/gdk.override"

static NspObject *
_wrap_gdk_event_tp_getattr1(NspObject *self, char *attr);

static int _wrap_gdk_event_tp_getattr(Stack stack, int rhs, int opt, int lhs)
{
  char *attr;
  NspObject *ob;
  CheckRhs(2,100); /* XXXXXX */
  CheckLhs(-1,1);
  if ((ob =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  if ((attr = GetString(stack,2)) == (char*)0) return RET_BUG;
  ob = _wrap_gdk_event_tp_getattr1(ob,attr);
  if ( ob == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,ob);
  return 1;
}

static NspObject *
_wrap_gdk_event_tp_getattr1(NspObject *self, char *attr)
{
  GdkEvent *event = nspg_boxed_get(self, GdkEvent);
  guint i;
  switch(event->type) {
  case GDK_NOTHING: break;
  case GDK_DELETE: break;
  case GDK_DESTROY: break;
  case GDK_EXPOSE:
    {
      static char *tab[] = { "type", "window", "send_event",  "area", "count", NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "area"))
	return (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE,
					   &event->expose.area,TRUE, TRUE,
					   (NspTypeBase *) nsp_type_gdkrectangle);
      if (!strcmp(attr, "count"))
	return nsp_new_double_obj((double) (event->expose.count));
      break;
    }
  case GDK_MOTION_NOTIFY:     /* GdkEventMotion            motion*/
    {
      static char *tab[] = { "type", "window", "send_event",
			    "time", "x", "y", "axes", "state",
			    "is_hint", "device", "x_root", "y_root", NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->motion.time));
      if (!strcmp(attr, "x"))
	return nsp_new_double_obj((double)event->motion.x);
      if (!strcmp(attr, "y"))
	return nsp_new_double_obj((double)event->motion.y);
      if (!strcmp(attr, "axes")) {
	int n= 0;
	NspMatrix  *v;
	if (event->motion.axes) n =  gdk_device_get_n_axes(event->motion.device);
	if (( v = nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return NULL;
	for (i = 0; i < n ; i++) v->R[i]=  event->motion.axes[i];
	return (NspObject *) v;
      }
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->motion.state));
      if (!strcmp(attr, "is_hint"))
	return nsp_new_double_obj((double) (event->motion.is_hint));
      if (!strcmp(attr, "device"))
	return (NspObject *) nspgobject_new(NVOID,(GObject *)event->motion.device);
      if (!strcmp(attr, "x_root"))
	return nsp_new_double_obj((double)event->motion.x_root);
      if (!strcmp(attr, "y_root"))
	return nsp_new_double_obj((double)event->motion.y_root);
      break;
    }
  case GDK_BUTTON_PRESS:      /*GdkEventButton            button*/
  case GDK_2BUTTON_PRESS:     /*GdkEventButton            button*/
  case GDK_3BUTTON_PRESS:     /*GdkEventButton            button*/
  case GDK_BUTTON_RELEASE:    /*GdkEventButton            button*/
    {
      static char *tab[] = { "type", "window", "send_event",
			    "time", "x", "y", "axes", "state",
			     "button", "device", "x_root", "y_root", NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->button.time));
      if (!strcmp(attr, "x"))
	return nsp_new_double_obj((double)event->button.x);
      if (!strcmp(attr, "y"))
	return nsp_new_double_obj((double)event->button.y);
      if (!strcmp(attr, "axes")) {
	int n= 0;
	NspMatrix  *v;
	if (event->button.axes) n =  gdk_device_get_n_axes(event->button.device);
	if (( v = nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return NULL;
	for (i = 0; i < n ; i++) v->R[i]=  event->button.axes[i];
	return (NspObject *) v;
      }
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->button.state));
      if (!strcmp(attr, "button"))
	return nsp_new_double_obj((double) (event->button.button));
      if (!strcmp(attr, "device"))
	return (NspObject *) nspgobject_new(NVOID,(GObject *)event->button.device);
      if (!strcmp(attr, "x_root"))
	return nsp_new_double_obj((double)event->button.x_root);
      if (!strcmp(attr, "y_root"))
	return nsp_new_double_obj((double)event->button.y_root);
      break;
    }
  case GDK_KEY_PRESS:         /*GdkEventKey               key*/
  case GDK_KEY_RELEASE:       /*GdkEventKey               key*/
    {
      static char *tab[] = { "type", "window", "send_event",
			     "time", "state", "keyval", "string", NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->key.time));
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->key.state));
      if (!strcmp(attr, "keyval"))
	return nsp_new_double_obj((double) (event->key.keyval));
      if (!strcmp(attr, "string"))
	return nsp_new_string_obj(NVOID,event->key.string,event->key.length);
      break;
    }
  case GDK_ENTER_NOTIFY:      /*GdkEventCrossing          crossing*/
  case GDK_LEAVE_NOTIFY:      /*GdkEventCrossing          crossing*/
    {
      static char *tab[] = {"type", "window", "send_event",
			    "subwindow", "time", "x", "y",
			    "x_root", "y_root", "mode", "detail",
			    "focus", "state", NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "subwindow"))
	return (NspObject *) nspgobject_new(NVOID,(GObject *)event->crossing.subwindow);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->crossing.time));
      if (!strcmp(attr, "x"))
	return nsp_new_double_obj((double)event->crossing.x);
      if (!strcmp(attr, "y"))
	return nsp_new_double_obj((double)event->crossing.y);
      if (!strcmp(attr, "x_root"))
	return nsp_new_double_obj((double)event->crossing.x_root);
      if (!strcmp(attr, "y_root"))
	return nsp_new_double_obj((double)event->crossing.y_root);
      if (!strcmp(attr, "mode"))
	return nsp_new_double_obj((double) (event->crossing.mode));
      if (!strcmp(attr, "detail"))
	return nsp_new_double_obj((double) (event->crossing.detail));
      if (!strcmp(attr, "focus"))
	return nsp_new_double_obj((double) (event->crossing.focus));
      if (!strcmp(attr, "state"))
      return nsp_new_double_obj((double) (event->crossing.state));
      break;
    }
  case GDK_FOCUS_CHANGE:      /*GdkEventFocus             focus_change*/
    {
      static char *tab[] = { "type", "window", "send_event", "in_",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "in_"))
	return nsp_new_double_obj((double) (event->focus_change.in));
      break;
    }
  case GDK_CONFIGURE:         /*GdkEventConfigure         configure*/
    {
      static char *tab[] = { "type", "window", "send_event",  "x", "y", "width", "height",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "x"))
	return nsp_new_double_obj((double) (event->configure.x));
      if (!strcmp(attr, "y"))
	return nsp_new_double_obj((double) (event->configure.y));
      if (!strcmp(attr, "width"))
	return nsp_new_double_obj((double) (event->configure.width));
      if (!strcmp(attr, "height"))
	return nsp_new_double_obj((double) (event->configure.height));
      break;
    }
  case GDK_MAP: break;
  case GDK_UNMAP: break;
  case GDK_PROPERTY_NOTIFY:   /*GdkEventProperty          property*/
    {
      static char *tab[] = { "type", "window", "send_event", "atom", "time", "state",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "atom"))
	return (NspObject *) gdkatom_create(NVOID,NULL,event->property.atom,NULL);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->property.time));
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->property.state));
      break;
    }
  case GDK_SELECTION_CLEAR:   /*GdkEventSelection         selection*/
  case GDK_SELECTION_REQUEST: /*GdkEventSelection         selection*/
  case GDK_SELECTION_NOTIFY:  /*GdkEventSelection         selection*/
    {
      static char *tab[] = { "type", "window", "send_event","selection", "target", "property","requestor", "time",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "selection"))
	return (NspObject *) gdkatom_create(NVOID,NULL,event->selection.selection,NULL);
      if (!strcmp(attr, "target"))
	return (NspObject *) gdkatom_create(NVOID,NULL,event->selection.target,NULL);
      if (!strcmp(attr, "property"))
	return (NspObject *) gdkatom_create(NVOID,NULL,event->selection.property,NULL);
      if (!strcmp(attr, "requestor"))
	/* XXXX return nsp_new_double_obj((double) (event->selection.requestor));
	 * should be a GdkNativeWindow requestor on recent gtk
	 */
	return nsp_new_double_obj((double) 0);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->selection.time));
      break;
    }
  case GDK_PROXIMITY_IN:      /*GdkEventProximity         proximity*/
  case GDK_PROXIMITY_OUT:     /*GdkEventProximity         proximity*/
    {
      static char *tab[] ={ "type", "window", "send_event", "time", "device",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->proximity.time));
      if (!strcmp(attr, "device"))
	return (NspObject *) nspgobject_new(NVOID,(GObject *)event->proximity.device);
      break;
    }
  case GDK_DRAG_ENTER:        /* GdkEventDND               dnd*/
  case GDK_DRAG_LEAVE:        /*GdkEventDND               dnd*/
  case GDK_DRAG_MOTION:       /*GdkEventDND               dnd*/
  case GDK_DRAG_STATUS:       /*GdkEventDND               dnd*/
  case GDK_DROP_START:        /*GdkEventDND               dnd*/
  case GDK_DROP_FINISHED:     /*GdkEventDND               dnd*/
    {
      static char *tab[] ={ "type", "window", "send_event","context", "time", "x_root", "y_root",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "context"))
	return (NspObject *) nspgobject_new(NVOID,(GObject *)event->dnd.context);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->dnd.time));
      if (!strcmp(attr, "x_root"))
	return nsp_new_double_obj((double)event->dnd.x_root);
      if (!strcmp(attr, "y_root"))
	return nsp_new_double_obj((double)event->dnd.y_root);
      break;
    }
#if 0
  case GDK_CLIENT_EVENT:      /*GdkEventClient            client*/
    {
      static char *tab[] ={ "type", "window", "send_event", "message_type", "data_format", "data",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "message_type"))
	return (NspObject *) gdkatom_create(NVOID,NULL,event->client.message_type,NULL);
      if (!strcmp(attr, "data_format"))
	return nsp_new_double_obj((double) (event->client.data_format));
      if (!strcmp(attr, "data"))
	return nsp_new_string_obj(NVOID,event->client.data.b, 20);
      break;
    }
#endif
  case GDK_VISIBILITY_NOTIFY: /*GdkEventVisibility        visibility*/
    {
      static char *tab[] ={ "type", "window", "send_event","state",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->visibility.state));
      break;
    }
#if 0
  case GDK_NO_EXPOSE:         /*GdkEventNoExpose          no_expose*/
    break;
#endif
  case GDK_SCROLL:            /*GdkEventScroll            scroll*/
    {
      static char *tab[] ={ "type", "window", "send_event",
			    "time", "x", "y", "state", "direction",
			    "device", "x_root", "y_root",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->scroll.time));
      if (!strcmp(attr, "x"))
	return nsp_new_double_obj((double)event->scroll.x);
      if (!strcmp(attr, "y"))
	return nsp_new_double_obj((double)event->scroll.y);
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->scroll.state));
      if (!strcmp(attr, "direction"))
	return nsp_new_double_obj((double) (event->scroll.direction));
      if (!strcmp(attr, "device"))
	return (NspObject *) nspgobject_new(NVOID,(GObject *)event->scroll.device);
      if (!strcmp(attr, "x_root"))
	return nsp_new_double_obj((double)event->scroll.x_root);
      if (!strcmp(attr, "y_root"))
	return nsp_new_double_obj((double)event->scroll.y_root);
      break;
    }
  case GDK_WINDOW_STATE:      /*GdkEventWindowState       window_state*/
    {
      static char *tab[] ={ "type", "window", "send_event", "changed_mask", "new_window_state",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "changed_mask"))
	return nsp_new_double_obj((double) (event->window_state.changed_mask));
      if (!strcmp(attr, "new_window_state"))
	return nsp_new_double_obj((double) (event->window_state.new_window_state));
      break;
    }
  case GDK_SETTING:           /*GdkEventSetting           setting*/
    {
      static char *tab[] ={ "type", "window", "send_event","action", "name",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "action"))
	return nsp_new_double_obj((double) (event->setting.action));
      if (!strcmp(attr, "name"))
	return nsp_new_string_obj(NVOID,event->setting.name,-1);
      break;
    }
  default:
    break;
  }
  if (!strcmp(attr, "type"))
    return nsp_new_double_obj((double) (event->type));
  if (!strcmp(attr, "window"))
    return (NspObject *) nspgobject_new(NVOID,(GObject *)event->any.window);
  if (!strcmp(attr, "send_event"))
    return nsp_new_double_obj((double) (event->any.send_event));
  if (!strcmp(attr, "__attrs"))
    {
      static char *tab[] ={ "type", "window", "send_event",NULL};
      return  (NspObject *)nsp_smatrix_create_from_table(tab);
    }
  Scierror("Error: Event attributes not found \n");
  return NULL;
}
#line 935 "gdk.c"


/* -----------NspGdkColor ----------- */

static int _wrap_gdk_color_tp_setattr(Stack stack, int rhs, int opt, int lhs);

#define  NspGdkColor_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkcolor.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkColor inherits from GBoxed 
 */

int nsp_type_gdkcolor_id=0;
NspTypeGdkColor *nsp_type_gdkcolor=NULL;

/*
 * Type object for NspGdkColor 
 * all the instance of NspTypeGdkColor share the same id. 
 * nsp_type_gdkcolor: is an instance of NspTypeGdkColor 
 *    used for objects of NspGdkColor type (i.e built with new_gdkcolor) 
 * other instances are used for derived classes 
 */
NspTypeGdkColor *new_type_gdkcolor(type_mode mode)
{
  NspTypeGdkColor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkcolor != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkcolor;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkcolor_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) _wrap_gdk_color_tp_setattr;
  type->methods = gdkcolor_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkcolor;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkcolor */ 

  top->s_type =  (s_type_func *) nsp_gdkcolor_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkcolor_type_short_string;
  /* top->create = (create_func*) int_gdkcolor_create;*/

  /* specific methods for gdkcolor */

  type->init = (init_func *) init_gdkcolor;

  /* 
   * NspGdkColor interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkcolor_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkColor called nsp_type_gdkcolor
       */
      type->id =  nsp_type_gdkcolor_id = nsp_new_type_id();
      nsp_type_gdkcolor = type;
      if ( nsp_register_type(nsp_type_gdkcolor) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkcolor, GDK_TYPE_COLOR);
      return ( mode == T_BASE ) ? type : new_type_gdkcolor(mode);
    }
  else 
    {
      type->id = nsp_type_gdkcolor_id;
      return type;
    }
}

/*
 * initialize NspGdkColor instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkcolor(NspGdkColor *Obj,NspTypeGdkColor *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkColor 
 */

NspGdkColor *new_gdkcolor() 
{
  NspGdkColor *loc;
  /* type must exists */
  nsp_type_gdkcolor = new_type_gdkcolor(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkColor)))== NULLGDKCOLOR) return loc;
  /* initialize object */
  if ( init_gdkcolor(loc,nsp_type_gdkcolor) == FAIL) return NULLGDKCOLOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkColor 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkcolor_type_name[]="GdkColor";
static char gdkcolor_short_type_name[]="GdkColor";

static char *nsp_gdkcolor_type_as_string(void)
{
  return(gdkcolor_type_name);
}

static char *nsp_gdkcolor_type_short_string(NspObject *v)
{
  return(gdkcolor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkColor objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkColor   *nsp_gdkcolor_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkcolor_id)  == TRUE  ) return ((NspGdkColor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkcolor));
  return NULL;
}

int IsGdkColorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkcolor_id);
}

int IsGdkColor(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkcolor_id);
}

NspGdkColor  *GetGdkColorCopy(Stack stack, int i)
{
  if (  GetGdkColor(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkColor  *GetGdkColor(Stack stack, int i)
{
  NspGdkColor *M;
  if (( M = nsp_gdkcolor_object(NthObj(i))) == NULLGDKCOLOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGdkColor *gdkcolor_copy(NspGdkColor *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gdkcolor);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkColor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 400 "codegen-3.0/gdk.override"
static int
_wrap_gdk_color_new(Stack stack, int rhs, int opt, int lhs)
{
  /* static char *kwlist[] = {"red", "green", "blue", "pixel", NULL };*/
  GdkColor colour = {0, 0, 0, 0};
  int_types T[] = {s_int, s_int, s_int, s_int,t_end};

  NspObject *nsp_ret;
  if (GetArgs(stack,rhs,opt,T, &(colour.red), &(colour.green),
	      &(colour.blue), &(colour.pixel)) == FAIL ) return RET_BUG;
  nsp_type_gdkcolor  = new_type_gdkcolor(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_COLOR,&colour,TRUE,TRUE,(NspTypeBase *) nsp_type_gdkcolor );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}
#line 1147 "gdk.c"


static NspMethods *gdkcolor_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_color__get_pixel(NspObject *self,char *attr)
{
  gulong ret;
  NspObject *nsp_ret;
  ret = NSP_GBOXED_GET(self, GdkColor)->pixel;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_gdk_color__get_red(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, GdkColor)->red;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_color__get_green(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, GdkColor)->green;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_color__get_blue(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, GdkColor)->blue;
  return nsp_new_double_obj((double) ret);
}

static AttrTab gdkcolor_attrs[] = {
  { "pixel", (attr_get_function * )_wrap_gdk_color__get_pixel, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "red", (attr_get_function * )_wrap_gdk_color__get_red, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "green", (attr_get_function * )_wrap_gdk_color__get_green, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "blue", (attr_get_function * )_wrap_gdk_color__get_blue, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { NULL,NULL,NULL,NULL,NULL },
};

#line 418 "codegen-3.0/gdk.override"

static int
_wrap_gdk_color_tp_setattr1(NspObject *self, char *attr, NspObject *value);

static int
_wrap_gdk_color_tp_setattr(Stack stack, int rhs, int opt, int lhs)
{
  char *attr;
  NspObject *ob;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((ob =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  if ((attr = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ( _wrap_gdk_color_tp_setattr1(ob,attr,NthObj(3)) == FAIL) return RET_BUG;
  NthObj(1)->ret_pos = 1;
  return 1;
}

static int
_wrap_gdk_color_tp_setattr1(NspObject *self, char *attr, NspObject *value)
{
  int val;
  if ( IntScalar(value,&val) == FAIL) return FAIL;
  if (!strcmp(attr, "red")) nspg_boxed_get(self, GdkColor)->red = val;
  else if (!strcmp(attr, "green"))  nspg_boxed_get(self, GdkColor)->green = val;
  else if (!strcmp(attr, "blue"))   nspg_boxed_get(self, GdkColor)->blue = val;
  else if (!strcmp(attr, "pixel"))  nspg_boxed_get(self, GdkColor)->pixel = val;
  return OK;
}

#line 1224 "gdk.c"


/* -----------NspGdkRectangle ----------- */


#define  NspGdkRectangle_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkrectangle.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkRectangle inherits from GBoxed 
 */

int nsp_type_gdkrectangle_id=0;
NspTypeGdkRectangle *nsp_type_gdkrectangle=NULL;

/*
 * Type object for NspGdkRectangle 
 * all the instance of NspTypeGdkRectangle share the same id. 
 * nsp_type_gdkrectangle: is an instance of NspTypeGdkRectangle 
 *    used for objects of NspGdkRectangle type (i.e built with new_gdkrectangle) 
 * other instances are used for derived classes 
 */
NspTypeGdkRectangle *new_type_gdkrectangle(type_mode mode)
{
  NspTypeGdkRectangle *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkrectangle != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkrectangle;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkrectangle_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkrectangle_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkrectangle;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkrectangle */ 

  top->s_type =  (s_type_func *) nsp_gdkrectangle_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkrectangle_type_short_string;
  /* top->create = (create_func*) int_gdkrectangle_create;*/

  /* specific methods for gdkrectangle */

  type->init = (init_func *) init_gdkrectangle;

  /* 
   * NspGdkRectangle interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkrectangle_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkRectangle called nsp_type_gdkrectangle
       */
      type->id =  nsp_type_gdkrectangle_id = nsp_new_type_id();
      nsp_type_gdkrectangle = type;
      if ( nsp_register_type(nsp_type_gdkrectangle) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkrectangle, GDK_TYPE_RECTANGLE);
      return ( mode == T_BASE ) ? type : new_type_gdkrectangle(mode);
    }
  else 
    {
      type->id = nsp_type_gdkrectangle_id;
      return type;
    }
}

/*
 * initialize NspGdkRectangle instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkrectangle(NspGdkRectangle *Obj,NspTypeGdkRectangle *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkRectangle 
 */

NspGdkRectangle *new_gdkrectangle() 
{
  NspGdkRectangle *loc;
  /* type must exists */
  nsp_type_gdkrectangle = new_type_gdkrectangle(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkRectangle)))== NULLGDKRECTANGLE) return loc;
  /* initialize object */
  if ( init_gdkrectangle(loc,nsp_type_gdkrectangle) == FAIL) return NULLGDKRECTANGLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkRectangle 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkrectangle_type_name[]="GdkRectangle";
static char gdkrectangle_short_type_name[]="GdkRectangle";

static char *nsp_gdkrectangle_type_as_string(void)
{
  return(gdkrectangle_type_name);
}

static char *nsp_gdkrectangle_type_short_string(NspObject *v)
{
  return(gdkrectangle_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkRectangle objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkRectangle   *nsp_gdkrectangle_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkrectangle_id)  == TRUE  ) return ((NspGdkRectangle *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkrectangle));
  return NULL;
}

int IsGdkRectangleObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkrectangle_id);
}

int IsGdkRectangle(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkrectangle_id);
}

NspGdkRectangle  *GetGdkRectangleCopy(Stack stack, int i)
{
  if (  GetGdkRectangle(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkRectangle  *GetGdkRectangle(Stack stack, int i)
{
  NspGdkRectangle *M;
  if (( M = nsp_gdkrectangle_object(NthObj(i))) == NULLGDKRECTANGLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGdkRectangle *gdkrectangle_copy(NspGdkRectangle *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gdkrectangle);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkRectangle
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 2235 "codegen-3.0/gdk.override"
static int
_wrap_gdk_rectangle_new(Stack stack,int rhs,int opt,int lhs)
{
  GdkRectangle re;
  NspGBoxed *H;
  if (rhs == 1 )
    {
      NspMatrix *r;
      if (( r = GetRealMat(stack,1))== NULLMAT) return RET_BUG;
      CheckLength(NspFname(stack),1,r,4);
      re.x = r->R[0];
      re.y = r->R[1];
      re.width = r->R[2];
      re.height  = r->R[3];
    }
  else if ( rhs == 4)
    {
      int val;
      if (GetScalarInt(stack,1,&val) == FAIL) return RET_BUG;
      re.x = val;
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      re.y = val;
      if (GetScalarInt(stack,3,&val) == FAIL) return RET_BUG;
      re.width = val;
      if (GetScalarInt(stack,4,&val) == FAIL) return RET_BUG;
      re.height = val;
    }
  else
    {
      Scierror("%s: rhs should be 1 or 4 \n",NspFname(stack));
      return RET_BUG;
    }
  if(( H = gboxed_create(NVOID,GDK_TYPE_RECTANGLE, &re, TRUE, TRUE,
			 nsp_type_gdkrectangle)) == NULLGBOXED)
    return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
}
#line 1457 "gdk.c"


#line 2275 "codegen-3.0/gdk.override"
static int
_wrap_gdk_rectangle_intersect(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gboolean ret;
  GdkRectangle intersect ={0,0,0,0};
  NspObject *nsp_src;
  int_types T[] = { obj_check ,t_end};
  CheckLhs(0,2);
  GdkRectangle *r = nspg_boxed_get(self, GdkRectangle);
  if (GetArgs(stack,rhs,opt,T,&nsp_type_gdkrectangle,&nsp_src) == FAIL )
    return RET_BUG;
  ret = gdk_rectangle_intersect(r, nspg_boxed_get(nsp_src, GdkRectangle), &intersect);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  if ( lhs == 2 )
    {
      NspObject *nsp_intersect = NULL;
      if (( nsp_intersect = (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE, &intersect, TRUE, TRUE,(NspTypeBase *) nsp_type_gdkrectangle)) == NULL)
	return RET_BUG;
      MoveObj(stack,2, nsp_intersect);
    }
  return Max(lhs,1);
}

#line 1484 "gdk.c"


#line 2300 "codegen-3.0/gdk.override"
static int
_wrap_gdk_rectangle_union(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_src;
  int_types T[] = { obj_check ,t_end};
  GdkRectangle *r = nspg_boxed_get(self, GdkRectangle);
  if (GetArgs(stack,rhs,opt,T,&nsp_type_gdkrectangle,&nsp_src) == FAIL )
    return RET_BUG;

  gdk_rectangle_union(r,nspg_boxed_get(nsp_src, GdkRectangle),r);
  NthObj(1)->ret_pos = 1;
  return 1;
}
#line 1501 "gdk.c"


static NspMethods gdkrectangle_methods[] = {
  {"intersect",(nsp_method *) _wrap_gdk_rectangle_intersect},
  {"union",(nsp_method *) _wrap_gdk_rectangle_union},
  { NULL, NULL}
};

static NspMethods *gdkrectangle_get_methods(void) { return gdkrectangle_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_rectangle__get_x(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, GdkRectangle)->x;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_rectangle__get_y(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, GdkRectangle)->y;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_rectangle__get_width(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, GdkRectangle)->width;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_rectangle__get_height(NspObject *self,char *attr)
{
  int ret;
  ret = NSP_GBOXED_GET(self, GdkRectangle)->height;
  return nsp_new_double_obj((double) ret);
}

static AttrTab gdkrectangle_attrs[] = {
  { "x", (attr_get_function * )_wrap_gdk_rectangle__get_x, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "y", (attr_get_function * )_wrap_gdk_rectangle__get_y, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "width", (attr_get_function * )_wrap_gdk_rectangle__get_width, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "height", (attr_get_function * )_wrap_gdk_rectangle__get_height, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { NULL,NULL,NULL,NULL,NULL },
};



/* -----------NspGdkRGBA ----------- */


#define  NspGdkRGBA_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkrgba.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkRGBA inherits from GBoxed 
 */

int nsp_type_gdkrgba_id=0;
NspTypeGdkRGBA *nsp_type_gdkrgba=NULL;

/*
 * Type object for NspGdkRGBA 
 * all the instance of NspTypeGdkRGBA share the same id. 
 * nsp_type_gdkrgba: is an instance of NspTypeGdkRGBA 
 *    used for objects of NspGdkRGBA type (i.e built with new_gdkrgba) 
 * other instances are used for derived classes 
 */
NspTypeGdkRGBA *new_type_gdkrgba(type_mode mode)
{
  NspTypeGdkRGBA *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkrgba != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkrgba;
    }
  if (( type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkrgba_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkrgba_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkrgba;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkrgba */ 

  top->s_type =  (s_type_func *) nsp_gdkrgba_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkrgba_type_short_string;
  /* top->create = (create_func*) int_gdkrgba_create;*/

  /* specific methods for gdkrgba */

  type->init = (init_func *) init_gdkrgba;

  /* 
   * NspGdkRGBA interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkrgba_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkRGBA called nsp_type_gdkrgba
       */
      type->id =  nsp_type_gdkrgba_id = nsp_new_type_id();
      nsp_type_gdkrgba = type;
      if ( nsp_register_type(nsp_type_gdkrgba) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkrgba, GDK_TYPE_RGBA);
      return ( mode == T_BASE ) ? type : new_type_gdkrgba(mode);
    }
  else 
    {
      type->id = nsp_type_gdkrgba_id;
      return type;
    }
}

/*
 * initialize NspGdkRGBA instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkrgba(NspGdkRGBA *Obj,NspTypeGdkRGBA *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkRGBA 
 */

NspGdkRGBA *new_gdkrgba() 
{
  NspGdkRGBA *loc;
  /* type must exists */
  nsp_type_gdkrgba = new_type_gdkrgba(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkRGBA)))== NULLGDKRGBA) return loc;
  /* initialize object */
  if ( init_gdkrgba(loc,nsp_type_gdkrgba) == FAIL) return NULLGDKRGBA;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkRGBA 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkrgba_type_name[]="GdkRGBA";
static char gdkrgba_short_type_name[]="GdkRGBA";

static char *nsp_gdkrgba_type_as_string(void)
{
  return(gdkrgba_type_name);
}

static char *nsp_gdkrgba_type_short_string(NspObject *v)
{
  return(gdkrgba_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkRGBA objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkRGBA   *nsp_gdkrgba_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkrgba_id)  == TRUE  ) return ((NspGdkRGBA *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkrgba));
  return NULL;
}

int IsGdkRGBAObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkrgba_id);
}

int IsGdkRGBA(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkrgba_id);
}

NspGdkRGBA  *GetGdkRGBACopy(Stack stack, int i)
{
  if (  GetGdkRGBA(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkRGBA  *GetGdkRGBA(Stack stack, int i)
{
  NspGdkRGBA *M;
  if (( M = nsp_gdkrgba_object(NthObj(i))) == NULLGDKRGBA)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGdkRGBA *gdkrgba_copy(NspGdkRGBA *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gdkrgba);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkRGBA
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_rgba_to_string(NspGdkRGBA *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
  ret =gdk_rgba_to_string(NSP_GBOXED_GET(self, GdkRGBA));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static NspMethods gdkrgba_methods[] = {
  {"to_string",(nsp_method *) _wrap_gdk_rgba_to_string},
  { NULL, NULL}
};

static NspMethods *gdkrgba_get_methods(void) { return gdkrgba_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_rgba__get_red(NspObject *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = NSP_GBOXED_GET(self, GdkRGBA)->red;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_gdk_rgba__get_green(NspObject *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = NSP_GBOXED_GET(self, GdkRGBA)->green;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_gdk_rgba__get_blue(NspObject *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = NSP_GBOXED_GET(self, GdkRGBA)->blue;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_gdk_rgba__get_alpha(NspObject *self,char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = NSP_GBOXED_GET(self, GdkRGBA)->alpha;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static AttrTab gdkrgba_attrs[] = {
  { "red", (attr_get_function * )_wrap_gdk_rgba__get_red, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "green", (attr_get_function * )_wrap_gdk_rgba__get_green, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "blue", (attr_get_function * )_wrap_gdk_rgba__get_blue, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { "alpha", (attr_get_function * )_wrap_gdk_rgba__get_alpha, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { NULL,NULL,NULL,NULL,NULL },
};



/* -----------NspGdkDevice ----------- */


#define  NspGdkDevice_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkdevice.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkDevice inherits from GObject 
 */

int nsp_type_gdkdevice_id=0;
NspTypeGdkDevice *nsp_type_gdkdevice=NULL;

/*
 * Type object for NspGdkDevice 
 * all the instance of NspTypeGdkDevice share the same id. 
 * nsp_type_gdkdevice: is an instance of NspTypeGdkDevice 
 *    used for objects of NspGdkDevice type (i.e built with new_gdkdevice) 
 * other instances are used for derived classes 
 */
NspTypeGdkDevice *new_type_gdkdevice(type_mode mode)
{
  NspTypeGdkDevice *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdevice != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdevice;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdevice_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkdevice_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkdevice;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkdevice */ 

  top->s_type =  (s_type_func *) nsp_gdkdevice_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkdevice_type_short_string;
  /* top->create = (create_func*) int_gdkdevice_create;*/

  /* specific methods for gdkdevice */

  type->init = (init_func *) init_gdkdevice;

  /* 
   * NspGdkDevice interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkdevice_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkDevice called nsp_type_gdkdevice
       */
      type->id =  nsp_type_gdkdevice_id = nsp_new_type_id();
      nsp_type_gdkdevice = type;
      if ( nsp_register_type(nsp_type_gdkdevice) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkdevice, GDK_TYPE_DEVICE);
      return ( mode == T_BASE ) ? type : new_type_gdkdevice(mode);
    }
  else 
    {
      type->id = nsp_type_gdkdevice_id;
      return type;
    }
}

/*
 * initialize NspGdkDevice instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdevice(NspGdkDevice *Obj,NspTypeGdkDevice *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkDevice 
 */

NspGdkDevice *new_gdkdevice() 
{
  NspGdkDevice *loc;
  /* type must exists */
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkDevice)))== NULLGDKDEVICE) return loc;
  /* initialize object */
  if ( init_gdkdevice(loc,nsp_type_gdkdevice) == FAIL) return NULLGDKDEVICE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkDevice 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkdevice_type_name[]="GdkDevice";
static char gdkdevice_short_type_name[]="GdkDevice";

static char *nsp_gdkdevice_type_as_string(void)
{
  return(gdkdevice_type_name);
}

static char *nsp_gdkdevice_type_short_string(NspObject *v)
{
  return(gdkdevice_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkDevice objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkDevice   *nsp_gdkdevice_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkdevice_id)  == TRUE  ) return ((NspGdkDevice *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdevice));
  return NULL;
}

int IsGdkDeviceObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkdevice_id);
}

int IsGdkDevice(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkdevice_id);
}

NspGdkDevice  *GetGdkDeviceCopy(Stack stack, int i)
{
  if (  GetGdkDevice(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkDevice  *GetGdkDevice(Stack stack, int i)
{
  NspGdkDevice *M;
  if (( M = nsp_gdkdevice_object(NthObj(i))) == NULLGDKDEVICE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkDevice *gdkdevice_copy(NspGdkDevice *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdevice);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdevice);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkDevice
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_device_get_name(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gdk_device_get_name(GDK_DEVICE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_get_has_cursor(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_device_get_has_cursor(GDK_DEVICE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_get_source(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_device_get_source(GDK_DEVICE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_get_mode(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_device_get_mode(GDK_DEVICE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_set_mode(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkInputMode mode;
  NspObject *nsp_mode = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_mode) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INPUT_MODE, nsp_mode, &mode)== FAIL)
      return RET_BUG;
    ret =gdk_device_set_mode(GDK_DEVICE(self->obj),mode);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_get_n_keys(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_device_get_n_keys(GDK_DEVICE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_set_key(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,obj, t_end};
  int index_, keyval;
  GdkModifierType modifiers;
  NspObject *nsp_modifiers = NULL;
  if ( GetArgs(stack,rhs,opt,T,&index_, &keyval, &nsp_modifiers) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_MODIFIER_TYPE, nsp_modifiers, &modifiers)==FAIL)
      return RET_BUG;
    gdk_device_set_key(GDK_DEVICE(self->obj),index_,keyval,modifiers);
  return 0;
}

static int _wrap_gdk_device_get_axis_use(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int index_;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&index_) == FAIL) return RET_BUG;
    ret =gdk_device_get_axis_use(GDK_DEVICE(self->obj),index_);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_set_axis_use(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj, t_end};
  int index_;
  GdkAxisUse use;
  NspObject *nsp_use = NULL;
  if ( GetArgs(stack,rhs,opt,T,&index_, &nsp_use) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_AXIS_USE, nsp_use, &use)== FAIL)
      return RET_BUG;
    gdk_device_set_axis_use(GDK_DEVICE(self->obj),index_,use);
  return 0;
}

#line 1172 "codegen-3.0/gdk.override"
static int
_wrap_gdk_device_get_state(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "window", NULL };*/
  GdkDevice *device = GDK_DEVICE(self->obj);
  NspGObject *window;
  NspMatrix *axes;
  GdkModifierType mask;
  int_types T[] = {obj ,t_end};
  if (GetArgs(stack,rhs,opt,T, &window) == FAIL )  return RET_BUG;
  if (!nspgobject_check(window,(NspTypeBase *) nsp_type_gdkwindow)) {
    Scierror( "Error: window should be a GdkWindow\n");
    return RET_BUG;
  }
  int num_axes = gdk_device_get_n_axes(device);
  if (( axes = nsp_matrix_create(NVOID,'r',1,num_axes)) == NULLMAT) return RET_BUG;
  gdk_device_get_state(device, GDK_WINDOW(window->obj), axes->R, &mask);
  MoveObj(stack,1,(NspObject *) axes);
  if ( lhs == 2 )
    if (( nsp_move_double(stack,2,(double) mask))==FAIL) return RET_BUG;
  return 2;
}
#line 2120 "gdk.c"


static int _wrap_gdk_device_get_window_at_position(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int win_x, win_y;
  GdkWindow *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&win_x, &win_y) == FAIL) return RET_BUG;
    ret =gdk_device_get_window_at_position(GDK_DEVICE(self->obj),&win_x,&win_y);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(3,10,0)
static int _wrap_gdk_device_get_window_at_position_double(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_double,s_double, t_end};
  double win_x, win_y;
  GdkWindow *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&win_x, &win_y) == FAIL) return RET_BUG;
    ret =gdk_device_get_window_at_position_double(GDK_DEVICE(self->obj),&win_x,&win_y);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_device_get_window_at_position_double(Stack stack, int rhs, int opt, int lhs) /* get_window_at_position_double */
{
  Scierror("Error: function gdk_device_get_window_at_position_double not available\n");
  return RET_BUG;
}
#endif
#line 1198 "codegen-3.0/gdk.override"
static int
_wrap_gdk_device_get_history(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "window", "start", "stop", NULL };*/
  GdkDevice *device = GDK_DEVICE(self->obj);
  NspGObject *window;
  guint start, stop;
  GdkTimeCoord **events;
  gint n_events;
  /*
  NspObject *pyevents;
  guint i;
  */
  int_types T[] = {obj, s_int, s_int ,t_end};

  if (GetArgs(stack,rhs,opt,T, &window, &start, &stop) == FAIL ) return RET_BUG;
  if (!nspgobject_check(window,(NspTypeBase *) nsp_type_gdkwindow)) {
    Scierror( "Error: window should be a GdkWindow\n");
    return RET_BUG;
  }
  gdk_device_get_history(device, GDK_WINDOW(window->obj), start, stop,
			 &events, &n_events);
  /*
  pyevents = NspTuple_New(n_events);
  for (i = 0; i < n_events; i++) {
    NspObject *axes;
    gint j;
    axes = NspTuple_New(device->num_axes);
    for (j = 0; j < device->num_axes; j++)
    NspTuple_SetItem(axes, j, NspFloat_FromDouble(events[i]->axes[j]));
    NspTuple_SetItem(pyevents, i, Nsp_BuildValue("(iN)", events[i]->time,
						 axes));
						 }
  gdk_device_free_history(events, n_events);
  return pyevents;
  */
  Scierror("Error: function _wrap_gdk_device_get_history not finished\n");
  return RET_BUG;
}
#line 2199 "gdk.c"


static int _wrap_gdk_device_get_n_axes(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_device_get_n_axes(GDK_DEVICE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_list_axes(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_device_list_axes(GDK_DEVICE(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_gdk_device_get_axis_value(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_double,obj,s_double, t_end};
  double axes, value;
  GdkAtom axis_label;
  NspObject *nsp_axis_label = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&axes, &nsp_axis_label, &value) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_axis_label,&axis_label)==FAIL) return RET_BUG;
    ret =gdk_device_get_axis_value(GDK_DEVICE(self->obj),&axes,axis_label,&value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#line 1239 "codegen-3.0/gdk.override"
static int
_wrap_gdk_device_get_axis(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "axes", "use", NULL };*/
  GdkDevice *device = GDK_DEVICE(self->obj);
  NspMatrix *nsp_axes;
  gdouble *axes=NULL, value;
  GdkAxisUse use;
  gboolean ret;
  int_types T[] = {mat , s_int, t_end};
  int num_axes = gdk_device_get_n_axes(device);

  if (GetArgs(stack,rhs,opt,T, &nsp_axes, &use) == FAIL ) return RET_BUG;

  CheckLength(NspFname(stack),1,nsp_axes, num_axes);

  ret = gdk_device_get_axis(device, axes, use, &value);
  if (ret)
    {
      if ( nsp_move_doubles(stack,1,1,1,(double) value) == FAIL) return RET_BUG;
    }
  else
    {
      if ( nsp_move_doubles(stack,1,0,0) == FAIL) return RET_BUG;
    }
  return 1;
}
#line 2263 "gdk.c"


static int _wrap_gdk_device_get_display(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_device_get_display(GDK_DEVICE(self->obj));
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_device_get_associated_device(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDevice *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_device_get_associated_device(GDK_DEVICE(self->obj));
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdevice))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_device_list_slave_devices(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_device_list_slave_devices(GDK_DEVICE(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_gdk_device_get_device_type(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_device_get_device_type(GDK_DEVICE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_grab(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj,s_bool,obj,obj_check,s_int, t_end};
  NspGObject *window, *cursor;
  GdkGrabOwnership grab_ownership;
  NspObject *nsp_grab_ownership = NULL, *nsp_event_mask = NULL;
  int owner_events;
  GdkEventMask event_mask;
  gulong time_;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window, &nsp_grab_ownership, &owner_events, &nsp_event_mask, &nsp_type_gdkcursor, &cursor, &time_) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_GRAB_OWNERSHIP, nsp_grab_ownership, &grab_ownership)== FAIL)
      return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_EVENT_MASK, nsp_event_mask, &event_mask)==FAIL)
      return RET_BUG;
    ret =gdk_device_grab(GDK_DEVICE(self->obj),GDK_WINDOW(window->obj),grab_ownership,owner_events,event_mask,GDK_CURSOR(cursor->obj),time_);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_ungrab(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&time_) == FAIL) return RET_BUG;
    gdk_device_ungrab(GDK_DEVICE(self->obj),time_);
  return 0;
}

static int _wrap_gdk_device_warp(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int,s_int, t_end};
  NspGObject *screen;
  int x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkscreen, &screen, &x, &y) == FAIL) return RET_BUG;
    gdk_device_warp(GDK_DEVICE(self->obj),GDK_SCREEN(screen->obj),x,y);
  return 0;
}

#if GTK_CHECK_VERSION(3,12,0)
static int _wrap_gdk_device_get_last_event_window(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_device_get_last_event_window(GDK_DEVICE(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_device_get_last_event_window(Stack stack, int rhs, int opt, int lhs) /* get_last_event_window */
{
  Scierror("Error: function gdk_device_get_last_event_window not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_device_get_vendor_id(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gdk_device_get_vendor_id(GDK_DEVICE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_device_get_vendor_id(Stack stack, int rhs, int opt, int lhs) /* get_vendor_id */
{
  Scierror("Error: function gdk_device_get_vendor_id not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_device_get_product_id(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gdk_device_get_product_id(GDK_DEVICE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_device_get_product_id(Stack stack, int rhs, int opt, int lhs) /* get_product_id */
{
  Scierror("Error: function gdk_device_get_product_id not available\n");
  return RET_BUG;
}
#endif
static NspMethods gdkdevice_methods[] = {
  {"get_name",(nsp_method *) _wrap_gdk_device_get_name},
  {"get_has_cursor",(nsp_method *) _wrap_gdk_device_get_has_cursor},
  {"get_source",(nsp_method *) _wrap_gdk_device_get_source},
  {"get_mode",(nsp_method *) _wrap_gdk_device_get_mode},
  {"set_mode",(nsp_method *) _wrap_gdk_device_set_mode},
  {"get_n_keys",(nsp_method *) _wrap_gdk_device_get_n_keys},
  {"set_key",(nsp_method *) _wrap_gdk_device_set_key},
  {"get_axis_use",(nsp_method *) _wrap_gdk_device_get_axis_use},
  {"set_axis_use",(nsp_method *) _wrap_gdk_device_set_axis_use},
  {"get_state",(nsp_method *) _wrap_gdk_device_get_state},
  {"get_window_at_position",(nsp_method *) _wrap_gdk_device_get_window_at_position},
  {"get_window_at_position_double",(nsp_method *) _wrap_gdk_device_get_window_at_position_double},
  {"get_history",(nsp_method *) _wrap_gdk_device_get_history},
  {"get_n_axes",(nsp_method *) _wrap_gdk_device_get_n_axes},
  {"list_axes",(nsp_method *) _wrap_gdk_device_list_axes},
  {"get_axis_value",(nsp_method *) _wrap_gdk_device_get_axis_value},
  {"get_axis",(nsp_method *) _wrap_gdk_device_get_axis},
  {"get_display",(nsp_method *) _wrap_gdk_device_get_display},
  {"get_associated_device",(nsp_method *) _wrap_gdk_device_get_associated_device},
  {"list_slave_devices",(nsp_method *) _wrap_gdk_device_list_slave_devices},
  {"get_device_type",(nsp_method *) _wrap_gdk_device_get_device_type},
  {"grab",(nsp_method *) _wrap_gdk_device_grab},
  {"ungrab",(nsp_method *) _wrap_gdk_device_ungrab},
  {"warp",(nsp_method *) _wrap_gdk_device_warp},
  {"get_last_event_window",(nsp_method *) _wrap_gdk_device_get_last_event_window},
  {"get_vendor_id",(nsp_method *) _wrap_gdk_device_get_vendor_id},
  {"get_product_id",(nsp_method *) _wrap_gdk_device_get_product_id},
  { NULL, NULL}
};

static NspMethods *gdkdevice_get_methods(void) { return gdkdevice_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkdevice_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkDeviceManager ----------- */


#define  NspGdkDeviceManager_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkdevicemanager.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkDeviceManager inherits from GObject 
 */

int nsp_type_gdkdevicemanager_id=0;
NspTypeGdkDeviceManager *nsp_type_gdkdevicemanager=NULL;

/*
 * Type object for NspGdkDeviceManager 
 * all the instance of NspTypeGdkDeviceManager share the same id. 
 * nsp_type_gdkdevicemanager: is an instance of NspTypeGdkDeviceManager 
 *    used for objects of NspGdkDeviceManager type (i.e built with new_gdkdevicemanager) 
 * other instances are used for derived classes 
 */
NspTypeGdkDeviceManager *new_type_gdkdevicemanager(type_mode mode)
{
  NspTypeGdkDeviceManager *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdevicemanager != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdevicemanager;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdevicemanager_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkdevicemanager_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkdevicemanager;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkdevicemanager */ 

  top->s_type =  (s_type_func *) nsp_gdkdevicemanager_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkdevicemanager_type_short_string;
  /* top->create = (create_func*) int_gdkdevicemanager_create;*/

  /* specific methods for gdkdevicemanager */

  type->init = (init_func *) init_gdkdevicemanager;

  /* 
   * NspGdkDeviceManager interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkdevicemanager_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkDeviceManager called nsp_type_gdkdevicemanager
       */
      type->id =  nsp_type_gdkdevicemanager_id = nsp_new_type_id();
      nsp_type_gdkdevicemanager = type;
      if ( nsp_register_type(nsp_type_gdkdevicemanager) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkdevicemanager, GDK_TYPE_DEVICE_MANAGER);
      return ( mode == T_BASE ) ? type : new_type_gdkdevicemanager(mode);
    }
  else 
    {
      type->id = nsp_type_gdkdevicemanager_id;
      return type;
    }
}

/*
 * initialize NspGdkDeviceManager instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdevicemanager(NspGdkDeviceManager *Obj,NspTypeGdkDeviceManager *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkDeviceManager 
 */

NspGdkDeviceManager *new_gdkdevicemanager() 
{
  NspGdkDeviceManager *loc;
  /* type must exists */
  nsp_type_gdkdevicemanager = new_type_gdkdevicemanager(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkDeviceManager)))== NULLGDKDEVICEMANAGER) return loc;
  /* initialize object */
  if ( init_gdkdevicemanager(loc,nsp_type_gdkdevicemanager) == FAIL) return NULLGDKDEVICEMANAGER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkDeviceManager 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkdevicemanager_type_name[]="GdkDeviceManager";
static char gdkdevicemanager_short_type_name[]="GdkDeviceManager";

static char *nsp_gdkdevicemanager_type_as_string(void)
{
  return(gdkdevicemanager_type_name);
}

static char *nsp_gdkdevicemanager_type_short_string(NspObject *v)
{
  return(gdkdevicemanager_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkDeviceManager objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkDeviceManager   *nsp_gdkdevicemanager_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkdevicemanager_id)  == TRUE  ) return ((NspGdkDeviceManager *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdevicemanager));
  return NULL;
}

int IsGdkDeviceManagerObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkdevicemanager_id);
}

int IsGdkDeviceManager(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkdevicemanager_id);
}

NspGdkDeviceManager  *GetGdkDeviceManagerCopy(Stack stack, int i)
{
  if (  GetGdkDeviceManager(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkDeviceManager  *GetGdkDeviceManager(Stack stack, int i)
{
  NspGdkDeviceManager *M;
  if (( M = nsp_gdkdevicemanager_object(NthObj(i))) == NULLGDKDEVICEMANAGER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkDeviceManager *gdkdevicemanager_copy(NspGdkDeviceManager *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdevicemanager);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdevicemanager);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkDeviceManager
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_device_manager_get_display(NspGdkDeviceManager *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_device_manager_get_display(GDK_DEVICE_MANAGER(self->obj));
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_device_manager_list_devices(NspGdkDeviceManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkDeviceType type;
  NspObject *nsp_type = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_DEVICE_TYPE, nsp_type, &type)== FAIL)
      return RET_BUG;
    ret =gdk_device_manager_list_devices(GDK_DEVICE_MANAGER(self->obj),type);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_gdk_device_manager_get_client_pointer(NspGdkDeviceManager *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDevice *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_device_manager_get_client_pointer(GDK_DEVICE_MANAGER(self->obj));
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdevice))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdkdevicemanager_methods[] = {
  {"get_display",(nsp_method *) _wrap_gdk_device_manager_get_display},
  {"list_devices",(nsp_method *) _wrap_gdk_device_manager_list_devices},
  {"get_client_pointer",(nsp_method *) _wrap_gdk_device_manager_get_client_pointer},
  { NULL, NULL}
};

static NspMethods *gdkdevicemanager_get_methods(void) { return gdkdevicemanager_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkdevicemanager_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkDisplay ----------- */


#define  NspGdkDisplay_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkdisplay.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkDisplay inherits from GObject 
 */

int nsp_type_gdkdisplay_id=0;
NspTypeGdkDisplay *nsp_type_gdkdisplay=NULL;

/*
 * Type object for NspGdkDisplay 
 * all the instance of NspTypeGdkDisplay share the same id. 
 * nsp_type_gdkdisplay: is an instance of NspTypeGdkDisplay 
 *    used for objects of NspGdkDisplay type (i.e built with new_gdkdisplay) 
 * other instances are used for derived classes 
 */
NspTypeGdkDisplay *new_type_gdkdisplay(type_mode mode)
{
  NspTypeGdkDisplay *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdisplay != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdisplay;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdisplay_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkdisplay_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkdisplay;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkdisplay */ 

  top->s_type =  (s_type_func *) nsp_gdkdisplay_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkdisplay_type_short_string;
  /* top->create = (create_func*) int_gdkdisplay_create;*/

  /* specific methods for gdkdisplay */

  type->init = (init_func *) init_gdkdisplay;

  /* 
   * NspGdkDisplay interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkdisplay_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkDisplay called nsp_type_gdkdisplay
       */
      type->id =  nsp_type_gdkdisplay_id = nsp_new_type_id();
      nsp_type_gdkdisplay = type;
      if ( nsp_register_type(nsp_type_gdkdisplay) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkdisplay, GDK_TYPE_DISPLAY);
      return ( mode == T_BASE ) ? type : new_type_gdkdisplay(mode);
    }
  else 
    {
      type->id = nsp_type_gdkdisplay_id;
      return type;
    }
}

/*
 * initialize NspGdkDisplay instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdisplay(NspGdkDisplay *Obj,NspTypeGdkDisplay *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkDisplay 
 */

NspGdkDisplay *new_gdkdisplay() 
{
  NspGdkDisplay *loc;
  /* type must exists */
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkDisplay)))== NULLGDKDISPLAY) return loc;
  /* initialize object */
  if ( init_gdkdisplay(loc,nsp_type_gdkdisplay) == FAIL) return NULLGDKDISPLAY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkDisplay 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkdisplay_type_name[]="GdkDisplay";
static char gdkdisplay_short_type_name[]="GdkDisplay";

static char *nsp_gdkdisplay_type_as_string(void)
{
  return(gdkdisplay_type_name);
}

static char *nsp_gdkdisplay_type_short_string(NspObject *v)
{
  return(gdkdisplay_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkDisplay objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkDisplay   *nsp_gdkdisplay_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkdisplay_id)  == TRUE  ) return ((NspGdkDisplay *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdisplay));
  return NULL;
}

int IsGdkDisplayObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkdisplay_id);
}

int IsGdkDisplay(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkdisplay_id);
}

NspGdkDisplay  *GetGdkDisplayCopy(Stack stack, int i)
{
  if (  GetGdkDisplay(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkDisplay  *GetGdkDisplay(Stack stack, int i)
{
  NspGdkDisplay *M;
  if (( M = nsp_gdkdisplay_object(NthObj(i))) == NULLGDKDISPLAY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkDisplay *gdkdisplay_copy(NspGdkDisplay *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdisplay);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdisplay);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkDisplay
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_display_get_name(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gdk_display_get_name(GDK_DISPLAY(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(3,10,0)
int _wrap_gdk_display_get_n_screens(Stack stack, int rhs, int opt, int lhs) /* get_n_screens */
{
  Scierror("Error: function gdk_display_get_n_screens is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_display_get_n_screens(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_get_n_screens(GDK_DISPLAY(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#endif
static int _wrap_gdk_display_get_screen(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int screen_num;
  GdkScreen *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&screen_num) == FAIL) return RET_BUG;
    ret =gdk_display_get_screen(GDK_DISPLAY(self->obj),screen_num);
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_get_default_screen(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkScreen *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_get_default_screen(GDK_DISPLAY(self->obj));
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_device_is_grabbed(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *device;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device) == FAIL) return RET_BUG;
    ret =gdk_display_device_is_grabbed(GDK_DISPLAY(self->obj),GDK_DEVICE(device->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_beep(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_display_beep(GDK_DISPLAY(self->obj));
  return 0;
}

static int _wrap_gdk_display_sync(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_display_sync(GDK_DISPLAY(self->obj));
  return 0;
}

static int _wrap_gdk_display_flush(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_display_flush(GDK_DISPLAY(self->obj));
  return 0;
}

static int _wrap_gdk_display_close(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_display_close(GDK_DISPLAY(self->obj));
  return 0;
}

static int _wrap_gdk_display_is_closed(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_is_closed(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_get_event(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkEvent *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_get_event(GDK_DISPLAY(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_peek_event(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkEvent *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_peek_event(GDK_DISPLAY(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_put_event(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkEvent *event = NULL;
  NspObject *nsp_event = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_event) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_event, GDK_TYPE_EVENT))
      event = nspg_boxed_get(nsp_event, GdkEvent);
  else {
      Scierror( "Error: event should be a GdkEvent\n");
      return RET_BUG;
  }
    gdk_display_put_event(GDK_DISPLAY(self->obj),event);
  return 0;
}

static int _wrap_gdk_display_has_pending(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_has_pending(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_set_double_click_time(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int msec;
  if ( GetArgs(stack,rhs,opt,T,&msec) == FAIL) return RET_BUG;
    gdk_display_set_double_click_time(GDK_DISPLAY(self->obj),msec);
  return 0;
}

static int _wrap_gdk_display_set_double_click_distance(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int distance;
  if ( GetArgs(stack,rhs,opt,T,&distance) == FAIL) return RET_BUG;
    gdk_display_set_double_click_distance(GDK_DISPLAY(self->obj),distance);
  return 0;
}

static int _wrap_gdk_display_supports_cursor_alpha(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_supports_cursor_alpha(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_supports_cursor_color(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_supports_cursor_color(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_get_default_cursor_size(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_get_default_cursor_size(GDK_DISPLAY(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_get_default_group(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_get_default_group(GDK_DISPLAY(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_supports_selection_notification(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_supports_selection_notification(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_request_selection_notification(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkAtom selection;
  NspObject *nsp_selection = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_selection) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
    ret =gdk_display_request_selection_notification(GDK_DISPLAY(self->obj),selection);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_supports_clipboard_persistence(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_supports_clipboard_persistence(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_supports_shapes(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_supports_shapes(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_supports_input_shapes(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_supports_input_shapes(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_display_supports_composite(Stack stack, int rhs, int opt, int lhs) /* supports_composite */
{
  Scierror("Error: function gdk_display_supports_composite is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_display_supports_composite(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_display_supports_composite(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#endif
static int _wrap_gdk_display_notify_startup_complete(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *startup_id;
  if ( GetArgs(stack,rhs,opt,T,&startup_id) == FAIL) return RET_BUG;
    gdk_display_notify_startup_complete(GDK_DISPLAY(self->obj),startup_id);
  return 0;
}

static int _wrap_gdk_display_get_device_manager(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDeviceManager *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_get_device_manager(GDK_DISPLAY(self->obj));
  nsp_type_gdkdevicemanager = new_type_gdkdevicemanager(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdevicemanager))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdkdisplay_methods[] = {
  {"get_name",(nsp_method *) _wrap_gdk_display_get_name},
  {"get_n_screens",(nsp_method *) _wrap_gdk_display_get_n_screens},
  {"get_screen",(nsp_method *) _wrap_gdk_display_get_screen},
  {"get_default_screen",(nsp_method *) _wrap_gdk_display_get_default_screen},
  {"device_is_grabbed",(nsp_method *) _wrap_gdk_display_device_is_grabbed},
  {"beep",(nsp_method *) _wrap_gdk_display_beep},
  {"sync",(nsp_method *) _wrap_gdk_display_sync},
  {"flush",(nsp_method *) _wrap_gdk_display_flush},
  {"close",(nsp_method *) _wrap_gdk_display_close},
  {"is_closed",(nsp_method *) _wrap_gdk_display_is_closed},
  {"get_event",(nsp_method *) _wrap_gdk_display_get_event},
  {"peek_event",(nsp_method *) _wrap_gdk_display_peek_event},
  {"put_event",(nsp_method *) _wrap_gdk_display_put_event},
  {"has_pending",(nsp_method *) _wrap_gdk_display_has_pending},
  {"set_double_click_time",(nsp_method *) _wrap_gdk_display_set_double_click_time},
  {"set_double_click_distance",(nsp_method *) _wrap_gdk_display_set_double_click_distance},
  {"supports_cursor_alpha",(nsp_method *) _wrap_gdk_display_supports_cursor_alpha},
  {"supports_cursor_color",(nsp_method *) _wrap_gdk_display_supports_cursor_color},
  {"get_default_cursor_size",(nsp_method *) _wrap_gdk_display_get_default_cursor_size},
  {"get_default_group",(nsp_method *) _wrap_gdk_display_get_default_group},
  {"supports_selection_notification",(nsp_method *) _wrap_gdk_display_supports_selection_notification},
  {"request_selection_notification",(nsp_method *) _wrap_gdk_display_request_selection_notification},
  {"supports_clipboard_persistence",(nsp_method *) _wrap_gdk_display_supports_clipboard_persistence},
  {"supports_shapes",(nsp_method *) _wrap_gdk_display_supports_shapes},
  {"supports_input_shapes",(nsp_method *) _wrap_gdk_display_supports_input_shapes},
  {"supports_composite",(nsp_method *) _wrap_gdk_display_supports_composite},
  {"notify_startup_complete",(nsp_method *) _wrap_gdk_display_notify_startup_complete},
  {"get_device_manager",(nsp_method *) _wrap_gdk_display_get_device_manager},
  { NULL, NULL}
};

static NspMethods *gdkdisplay_get_methods(void) { return gdkdisplay_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkdisplay_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkDisplayManager ----------- */


#define  NspGdkDisplayManager_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkdisplaymanager.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkDisplayManager inherits from GObject 
 */

int nsp_type_gdkdisplaymanager_id=0;
NspTypeGdkDisplayManager *nsp_type_gdkdisplaymanager=NULL;

/*
 * Type object for NspGdkDisplayManager 
 * all the instance of NspTypeGdkDisplayManager share the same id. 
 * nsp_type_gdkdisplaymanager: is an instance of NspTypeGdkDisplayManager 
 *    used for objects of NspGdkDisplayManager type (i.e built with new_gdkdisplaymanager) 
 * other instances are used for derived classes 
 */
NspTypeGdkDisplayManager *new_type_gdkdisplaymanager(type_mode mode)
{
  NspTypeGdkDisplayManager *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdisplaymanager != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdisplaymanager;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdisplaymanager_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkdisplaymanager_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkdisplaymanager;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkdisplaymanager */ 

  top->s_type =  (s_type_func *) nsp_gdkdisplaymanager_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkdisplaymanager_type_short_string;
  /* top->create = (create_func*) int_gdkdisplaymanager_create;*/

  /* specific methods for gdkdisplaymanager */

  type->init = (init_func *) init_gdkdisplaymanager;

  /* 
   * NspGdkDisplayManager interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkdisplaymanager_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkDisplayManager called nsp_type_gdkdisplaymanager
       */
      type->id =  nsp_type_gdkdisplaymanager_id = nsp_new_type_id();
      nsp_type_gdkdisplaymanager = type;
      if ( nsp_register_type(nsp_type_gdkdisplaymanager) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkdisplaymanager, GDK_TYPE_DISPLAY_MANAGER);
      return ( mode == T_BASE ) ? type : new_type_gdkdisplaymanager(mode);
    }
  else 
    {
      type->id = nsp_type_gdkdisplaymanager_id;
      return type;
    }
}

/*
 * initialize NspGdkDisplayManager instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdisplaymanager(NspGdkDisplayManager *Obj,NspTypeGdkDisplayManager *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkDisplayManager 
 */

NspGdkDisplayManager *new_gdkdisplaymanager() 
{
  NspGdkDisplayManager *loc;
  /* type must exists */
  nsp_type_gdkdisplaymanager = new_type_gdkdisplaymanager(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkDisplayManager)))== NULLGDKDISPLAYMANAGER) return loc;
  /* initialize object */
  if ( init_gdkdisplaymanager(loc,nsp_type_gdkdisplaymanager) == FAIL) return NULLGDKDISPLAYMANAGER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkDisplayManager 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkdisplaymanager_type_name[]="GdkDisplayManager";
static char gdkdisplaymanager_short_type_name[]="GdkDisplayManager";

static char *nsp_gdkdisplaymanager_type_as_string(void)
{
  return(gdkdisplaymanager_type_name);
}

static char *nsp_gdkdisplaymanager_type_short_string(NspObject *v)
{
  return(gdkdisplaymanager_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkDisplayManager objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkDisplayManager   *nsp_gdkdisplaymanager_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkdisplaymanager_id)  == TRUE  ) return ((NspGdkDisplayManager *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdisplaymanager));
  return NULL;
}

int IsGdkDisplayManagerObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkdisplaymanager_id);
}

int IsGdkDisplayManager(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkdisplaymanager_id);
}

NspGdkDisplayManager  *GetGdkDisplayManagerCopy(Stack stack, int i)
{
  if (  GetGdkDisplayManager(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkDisplayManager  *GetGdkDisplayManager(Stack stack, int i)
{
  NspGdkDisplayManager *M;
  if (( M = nsp_gdkdisplaymanager_object(NthObj(i))) == NULLGDKDISPLAYMANAGER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkDisplayManager *gdkdisplaymanager_copy(NspGdkDisplayManager *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdisplaymanager);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdisplaymanager);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkDisplayManager
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_display_manager_get_default_display(NspGdkDisplayManager *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_manager_get_default_display(GDK_DISPLAY_MANAGER(self->obj));
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_manager_set_default_display(NspGdkDisplayManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *display;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdisplay, &display) == FAIL) return RET_BUG;
    gdk_display_manager_set_default_display(GDK_DISPLAY_MANAGER(self->obj),GDK_DISPLAY(display->obj));
  return 0;
}

static int _wrap_gdk_display_manager_list_displays(NspGdkDisplayManager *self,Stack stack,int rhs,int opt,int lhs)
{
  GSList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_display_manager_list_displays(GDK_DISPLAY_MANAGER(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

}

static int _wrap_gdk_display_manager_open_display(NspGdkDisplayManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  GdkDisplay *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =gdk_display_manager_open_display(GDK_DISPLAY_MANAGER(self->obj),name);
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdkdisplaymanager_methods[] = {
  {"get_default_display",(nsp_method *) _wrap_gdk_display_manager_get_default_display},
  {"set_default_display",(nsp_method *) _wrap_gdk_display_manager_set_default_display},
  {"list_displays",(nsp_method *) _wrap_gdk_display_manager_list_displays},
  {"open_display",(nsp_method *) _wrap_gdk_display_manager_open_display},
  { NULL, NULL}
};

static NspMethods *gdkdisplaymanager_get_methods(void) { return gdkdisplaymanager_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkdisplaymanager_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkDragContext ----------- */


#define  NspGdkDragContext_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkdragcontext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkDragContext inherits from GObject 
 */

int nsp_type_gdkdragcontext_id=0;
NspTypeGdkDragContext *nsp_type_gdkdragcontext=NULL;

/*
 * Type object for NspGdkDragContext 
 * all the instance of NspTypeGdkDragContext share the same id. 
 * nsp_type_gdkdragcontext: is an instance of NspTypeGdkDragContext 
 *    used for objects of NspGdkDragContext type (i.e built with new_gdkdragcontext) 
 * other instances are used for derived classes 
 */
NspTypeGdkDragContext *new_type_gdkdragcontext(type_mode mode)
{
  NspTypeGdkDragContext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdragcontext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdragcontext;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdragcontext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkdragcontext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkdragcontext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkdragcontext */ 

  top->s_type =  (s_type_func *) nsp_gdkdragcontext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkdragcontext_type_short_string;
  /* top->create = (create_func*) int_gdkdragcontext_create;*/

  /* specific methods for gdkdragcontext */

  type->init = (init_func *) init_gdkdragcontext;

  /* 
   * NspGdkDragContext interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkdragcontext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkDragContext called nsp_type_gdkdragcontext
       */
      type->id =  nsp_type_gdkdragcontext_id = nsp_new_type_id();
      nsp_type_gdkdragcontext = type;
      if ( nsp_register_type(nsp_type_gdkdragcontext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkdragcontext, GDK_TYPE_DRAG_CONTEXT);
      return ( mode == T_BASE ) ? type : new_type_gdkdragcontext(mode);
    }
  else 
    {
      type->id = nsp_type_gdkdragcontext_id;
      return type;
    }
}

/*
 * initialize NspGdkDragContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdragcontext(NspGdkDragContext *Obj,NspTypeGdkDragContext *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkDragContext 
 */

NspGdkDragContext *new_gdkdragcontext() 
{
  NspGdkDragContext *loc;
  /* type must exists */
  nsp_type_gdkdragcontext = new_type_gdkdragcontext(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkDragContext)))== NULLGDKDRAGCONTEXT) return loc;
  /* initialize object */
  if ( init_gdkdragcontext(loc,nsp_type_gdkdragcontext) == FAIL) return NULLGDKDRAGCONTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkDragContext 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkdragcontext_type_name[]="GdkDragContext";
static char gdkdragcontext_short_type_name[]="GdkDragContext";

static char *nsp_gdkdragcontext_type_as_string(void)
{
  return(gdkdragcontext_type_name);
}

static char *nsp_gdkdragcontext_type_short_string(NspObject *v)
{
  return(gdkdragcontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkDragContext objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkDragContext   *nsp_gdkdragcontext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkdragcontext_id)  == TRUE  ) return ((NspGdkDragContext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdragcontext));
  return NULL;
}

int IsGdkDragContextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkdragcontext_id);
}

int IsGdkDragContext(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkdragcontext_id);
}

NspGdkDragContext  *GetGdkDragContextCopy(Stack stack, int i)
{
  if (  GetGdkDragContext(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkDragContext  *GetGdkDragContext(Stack stack, int i)
{
  NspGdkDragContext *M;
  if (( M = nsp_gdkdragcontext_object(NthObj(i))) == NULLGDKDRAGCONTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkDragContext *gdkdragcontext_copy(NspGdkDragContext *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdragcontext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdragcontext);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkDragContext
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_drag_context_set_device(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *device;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device) == FAIL) return RET_BUG;
    gdk_drag_context_set_device(GDK_DRAG_CONTEXT(self->obj),GDK_DEVICE(device->obj));
  return 0;
}

static int _wrap_gdk_drag_context_get_device(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDevice *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_drag_context_get_device(GDK_DRAG_CONTEXT(self->obj));
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdevice))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_drag_context_list_targets(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_drag_context_list_targets(GDK_DRAG_CONTEXT(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_gdk_drag_context_get_actions(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gdk_drag_context_get_actions(GDK_DRAG_CONTEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_drag_context_get_suggested_action(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gdk_drag_context_get_suggested_action(GDK_DRAG_CONTEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_drag_context_get_selected_action(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gdk_drag_context_get_selected_action(GDK_DRAG_CONTEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_drag_context_get_source_window(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_drag_context_get_source_window(GDK_DRAG_CONTEXT(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_drag_context_get_dest_window(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_drag_context_get_dest_window(GDK_DRAG_CONTEXT(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_drag_context_get_protocol(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_drag_context_get_protocol(GDK_DRAG_CONTEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_drag_finish(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,s_bool,new_opts, t_end};
  nsp_option opts[] = {
	{"time",s_int,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  int success, del;
  gulong time = GDK_CURRENT_TIME;
  if ( GetArgs(stack,rhs,opt,T,&success, &del, opts, &time) == FAIL) return RET_BUG;
    gtk_drag_finish(GDK_DRAG_CONTEXT(self->obj),success,del,time);
  return 0;
}

static int _wrap_gtk_drag_set_icon_pixbuf(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int,s_int, t_end};
  NspGObject *pixbuf;
  int hot_x, hot_y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &pixbuf, &hot_x, &hot_y) == FAIL) return RET_BUG;
    gtk_drag_set_icon_pixbuf(GDK_DRAG_CONTEXT(self->obj),GDK_PIXBUF(pixbuf->obj),hot_x,hot_y);
  return 0;
}

static int _wrap_gtk_drag_set_icon_default(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_drag_set_icon_default(GDK_DRAG_CONTEXT(self->obj));
  return 0;
}

static NspMethods gdkdragcontext_methods[] = {
  {"set_device",(nsp_method *) _wrap_gdk_drag_context_set_device},
  {"get_device",(nsp_method *) _wrap_gdk_drag_context_get_device},
  {"list_targets",(nsp_method *) _wrap_gdk_drag_context_list_targets},
  {"get_actions",(nsp_method *) _wrap_gdk_drag_context_get_actions},
  {"get_suggested_action",(nsp_method *) _wrap_gdk_drag_context_get_suggested_action},
  {"get_selected_action",(nsp_method *) _wrap_gdk_drag_context_get_selected_action},
  {"get_source_window",(nsp_method *) _wrap_gdk_drag_context_get_source_window},
  {"get_dest_window",(nsp_method *) _wrap_gdk_drag_context_get_dest_window},
  {"get_protocol",(nsp_method *) _wrap_gdk_drag_context_get_protocol},
  {"finish",(nsp_method *) _wrap_gtk_drag_finish},
  {"set_icon_pixbuf",(nsp_method *) _wrap_gtk_drag_set_icon_pixbuf},
  {"set_icon_default",(nsp_method *) _wrap_gtk_drag_set_icon_default},
  { NULL, NULL}
};

static NspMethods *gdkdragcontext_get_methods(void) { return gdkdragcontext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkdragcontext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkFrameClock ----------- */


#define  NspGdkFrameClock_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkframeclock.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkFrameClock inherits from GObject 
 */

int nsp_type_gdkframeclock_id=0;
NspTypeGdkFrameClock *nsp_type_gdkframeclock=NULL;

/*
 * Type object for NspGdkFrameClock 
 * all the instance of NspTypeGdkFrameClock share the same id. 
 * nsp_type_gdkframeclock: is an instance of NspTypeGdkFrameClock 
 *    used for objects of NspGdkFrameClock type (i.e built with new_gdkframeclock) 
 * other instances are used for derived classes 
 */
NspTypeGdkFrameClock *new_type_gdkframeclock(type_mode mode)
{
  NspTypeGdkFrameClock *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkframeclock != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkframeclock;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkframeclock_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkframeclock_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkframeclock;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkframeclock */ 

  top->s_type =  (s_type_func *) nsp_gdkframeclock_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkframeclock_type_short_string;
  /* top->create = (create_func*) int_gdkframeclock_create;*/

  /* specific methods for gdkframeclock */

  type->init = (init_func *) init_gdkframeclock;

  /* 
   * NspGdkFrameClock interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkframeclock_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkFrameClock called nsp_type_gdkframeclock
       */
      type->id =  nsp_type_gdkframeclock_id = nsp_new_type_id();
      nsp_type_gdkframeclock = type;
      if ( nsp_register_type(nsp_type_gdkframeclock) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkframeclock, GDK_TYPE_FRAME_CLOCK);
      return ( mode == T_BASE ) ? type : new_type_gdkframeclock(mode);
    }
  else 
    {
      type->id = nsp_type_gdkframeclock_id;
      return type;
    }
}

/*
 * initialize NspGdkFrameClock instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkframeclock(NspGdkFrameClock *Obj,NspTypeGdkFrameClock *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkFrameClock 
 */

NspGdkFrameClock *new_gdkframeclock() 
{
  NspGdkFrameClock *loc;
  /* type must exists */
  nsp_type_gdkframeclock = new_type_gdkframeclock(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkFrameClock)))== NULLGDKFRAMECLOCK) return loc;
  /* initialize object */
  if ( init_gdkframeclock(loc,nsp_type_gdkframeclock) == FAIL) return NULLGDKFRAMECLOCK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkFrameClock 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkframeclock_type_name[]="GdkFrameClock";
static char gdkframeclock_short_type_name[]="GdkFrameClock";

static char *nsp_gdkframeclock_type_as_string(void)
{
  return(gdkframeclock_type_name);
}

static char *nsp_gdkframeclock_type_short_string(NspObject *v)
{
  return(gdkframeclock_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkFrameClock objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkFrameClock   *nsp_gdkframeclock_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkframeclock_id)  == TRUE  ) return ((NspGdkFrameClock *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkframeclock));
  return NULL;
}

int IsGdkFrameClockObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkframeclock_id);
}

int IsGdkFrameClock(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkframeclock_id);
}

NspGdkFrameClock  *GetGdkFrameClockCopy(Stack stack, int i)
{
  if (  GetGdkFrameClock(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkFrameClock  *GetGdkFrameClock(Stack stack, int i)
{
  NspGdkFrameClock *M;
  if (( M = nsp_gdkframeclock_object(NthObj(i))) == NULLGDKFRAMECLOCK)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkFrameClock *gdkframeclock_copy(NspGdkFrameClock *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkframeclock);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkframeclock);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkFrameClock
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#if GTK_CHECK_VERSION(3,8,0)
static int _wrap_gdk_frame_clock_get_frame_time(NspGdkFrameClock *self,Stack stack,int rhs,int opt,int lhs)
{
  gint64 ret;
  CheckRhs(0,0);
    ret =gdk_frame_clock_get_frame_time(GDK_FRAME_CLOCK(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_frame_clock_get_frame_time(Stack stack, int rhs, int opt, int lhs) /* get_frame_time */
{
  Scierror("Error: function gdk_frame_clock_get_frame_time not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,8,0)
static int _wrap_gdk_frame_clock_request_phase(NspGdkFrameClock *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkFrameClockPhase phase;
  NspObject *nsp_phase = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_phase) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_FRAME_CLOCK_PHASE, nsp_phase, &phase)==FAIL)
      return RET_BUG;
    gdk_frame_clock_request_phase(GDK_FRAME_CLOCK(self->obj),phase);
  return 0;
}

#else
int _wrap_gdk_frame_clock_request_phase(Stack stack, int rhs, int opt, int lhs) /* request_phase */
{
  Scierror("Error: function gdk_frame_clock_request_phase not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,8,0)
static int _wrap_gdk_frame_clock_begin_updating(NspGdkFrameClock *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_frame_clock_begin_updating(GDK_FRAME_CLOCK(self->obj));
  return 0;
}

#else
int _wrap_gdk_frame_clock_begin_updating(Stack stack, int rhs, int opt, int lhs) /* begin_updating */
{
  Scierror("Error: function gdk_frame_clock_begin_updating not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,8,0)
static int _wrap_gdk_frame_clock_end_updating(NspGdkFrameClock *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_frame_clock_end_updating(GDK_FRAME_CLOCK(self->obj));
  return 0;
}

#else
int _wrap_gdk_frame_clock_end_updating(Stack stack, int rhs, int opt, int lhs) /* end_updating */
{
  Scierror("Error: function gdk_frame_clock_end_updating not available\n");
  return RET_BUG;
}
#endif
static NspMethods gdkframeclock_methods[] = {
  {"get_frame_time",(nsp_method *) _wrap_gdk_frame_clock_get_frame_time},
  {"request_phase",(nsp_method *) _wrap_gdk_frame_clock_request_phase},
  {"begin_updating",(nsp_method *) _wrap_gdk_frame_clock_begin_updating},
  {"end_updating",(nsp_method *) _wrap_gdk_frame_clock_end_updating},
  { NULL, NULL}
};

static NspMethods *gdkframeclock_get_methods(void) { return gdkframeclock_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkframeclock_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

#if GTK_CHECK_VERSION(3,16,0) 

/* -----------NspGdkGLContext ----------- */


#define  NspGdkGLContext_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkglcontext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkGLContext inherits from GObject 
 */

int nsp_type_gdkglcontext_id=0;
NspTypeGdkGLContext *nsp_type_gdkglcontext=NULL;

/*
 * Type object for NspGdkGLContext 
 * all the instance of NspTypeGdkGLContext share the same id. 
 * nsp_type_gdkglcontext: is an instance of NspTypeGdkGLContext 
 *    used for objects of NspGdkGLContext type (i.e built with new_gdkglcontext) 
 * other instances are used for derived classes 
 */
NspTypeGdkGLContext *new_type_gdkglcontext(type_mode mode)
{
  NspTypeGdkGLContext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkglcontext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkglcontext;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkglcontext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkglcontext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkglcontext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkglcontext */ 

  top->s_type =  (s_type_func *) nsp_gdkglcontext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkglcontext_type_short_string;
  /* top->create = (create_func*) int_gdkglcontext_create;*/

  /* specific methods for gdkglcontext */

  type->init = (init_func *) init_gdkglcontext;

  /* 
   * NspGdkGLContext interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkglcontext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkGLContext called nsp_type_gdkglcontext
       */
      type->id =  nsp_type_gdkglcontext_id = nsp_new_type_id();
      nsp_type_gdkglcontext = type;
      if ( nsp_register_type(nsp_type_gdkglcontext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkglcontext, GDK_TYPE_GL_CONTEXT);
      return ( mode == T_BASE ) ? type : new_type_gdkglcontext(mode);
    }
  else 
    {
      type->id = nsp_type_gdkglcontext_id;
      return type;
    }
}

/*
 * initialize NspGdkGLContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkglcontext(NspGdkGLContext *Obj,NspTypeGdkGLContext *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkGLContext 
 */

NspGdkGLContext *new_gdkglcontext() 
{
  NspGdkGLContext *loc;
  /* type must exists */
  nsp_type_gdkglcontext = new_type_gdkglcontext(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkGLContext)))== NULLGDKGLCONTEXT) return loc;
  /* initialize object */
  if ( init_gdkglcontext(loc,nsp_type_gdkglcontext) == FAIL) return NULLGDKGLCONTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkGLContext 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkglcontext_type_name[]="GdkGLContext";
static char gdkglcontext_short_type_name[]="GdkGLContext";

static char *nsp_gdkglcontext_type_as_string(void)
{
  return(gdkglcontext_type_name);
}

static char *nsp_gdkglcontext_type_short_string(NspObject *v)
{
  return(gdkglcontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkGLContext objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkGLContext   *nsp_gdkglcontext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkglcontext_id)  == TRUE  ) return ((NspGdkGLContext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkglcontext));
  return NULL;
}

int IsGdkGLContextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkglcontext_id);
}

int IsGdkGLContext(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkglcontext_id);
}

NspGdkGLContext  *GetGdkGLContextCopy(Stack stack, int i)
{
  if (  GetGdkGLContext(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkGLContext  *GetGdkGLContext(Stack stack, int i)
{
  NspGdkGLContext *M;
  if (( M = nsp_gdkglcontext_object(NthObj(i))) == NULLGDKGLCONTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkGLContext *gdkglcontext_copy(NspGdkGLContext *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkglcontext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkglcontext);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkGLContext
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
#endif /* GTK_CHECK_VERSION */
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#if GTK_CHECK_VERSION(3,16,0) 
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_get_display(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_gl_context_get_display(GDK_GL_CONTEXT(self->obj));
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_gl_context_get_display(Stack stack, int rhs, int opt, int lhs) /* get_display */
{
  Scierror("Error: function gdk_gl_context_get_display not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_get_window(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_gl_context_get_window(GDK_GL_CONTEXT(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_gl_context_get_window(Stack stack, int rhs, int opt, int lhs) /* get_window */
{
  Scierror("Error: function gdk_gl_context_get_window not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_get_shared_context(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkGLContext *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_gl_context_get_shared_context(GDK_GL_CONTEXT(self->obj));
  nsp_type_gdkglcontext = new_type_gdkglcontext(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkglcontext))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_gl_context_get_shared_context(Stack stack, int rhs, int opt, int lhs) /* get_shared_context */
{
  Scierror("Error: function gdk_gl_context_get_shared_context not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_get_version(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int major, minor;
  if ( GetArgs(stack,rhs,opt,T,&major, &minor) == FAIL) return RET_BUG;
    gdk_gl_context_get_version(GDK_GL_CONTEXT(self->obj),&major,&minor);
  return 0;
}

#else
int _wrap_gdk_gl_context_get_version(Stack stack, int rhs, int opt, int lhs) /* get_version */
{
  Scierror("Error: function gdk_gl_context_get_version not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_set_required_version(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int major, minor;
  if ( GetArgs(stack,rhs,opt,T,&major, &minor) == FAIL) return RET_BUG;
    gdk_gl_context_set_required_version(GDK_GL_CONTEXT(self->obj),major,minor);
  return 0;
}

#else
int _wrap_gdk_gl_context_set_required_version(Stack stack, int rhs, int opt, int lhs) /* set_required_version */
{
  Scierror("Error: function gdk_gl_context_set_required_version not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_get_required_version(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int major, minor;
  if ( GetArgs(stack,rhs,opt,T,&major, &minor) == FAIL) return RET_BUG;
    gdk_gl_context_get_required_version(GDK_GL_CONTEXT(self->obj),&major,&minor);
  return 0;
}

#else
int _wrap_gdk_gl_context_get_required_version(Stack stack, int rhs, int opt, int lhs) /* get_required_version */
{
  Scierror("Error: function gdk_gl_context_get_required_version not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_set_debug_enabled(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int enabled;
  if ( GetArgs(stack,rhs,opt,T,&enabled) == FAIL) return RET_BUG;
    gdk_gl_context_set_debug_enabled(GDK_GL_CONTEXT(self->obj),enabled);
  return 0;
}

#else
int _wrap_gdk_gl_context_set_debug_enabled(Stack stack, int rhs, int opt, int lhs) /* set_debug_enabled */
{
  Scierror("Error: function gdk_gl_context_set_debug_enabled not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_get_debug_enabled(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_gl_context_get_debug_enabled(GDK_GL_CONTEXT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_gl_context_get_debug_enabled(Stack stack, int rhs, int opt, int lhs) /* get_debug_enabled */
{
  Scierror("Error: function gdk_gl_context_get_debug_enabled not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_set_forward_compatible(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int compatible;
  if ( GetArgs(stack,rhs,opt,T,&compatible) == FAIL) return RET_BUG;
    gdk_gl_context_set_forward_compatible(GDK_GL_CONTEXT(self->obj),compatible);
  return 0;
}

#else
int _wrap_gdk_gl_context_set_forward_compatible(Stack stack, int rhs, int opt, int lhs) /* set_forward_compatible */
{
  Scierror("Error: function gdk_gl_context_set_forward_compatible not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_get_forward_compatible(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_gl_context_get_forward_compatible(GDK_GL_CONTEXT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_gl_context_get_forward_compatible(Stack stack, int rhs, int opt, int lhs) /* get_forward_compatible */
{
  Scierror("Error: function gdk_gl_context_get_forward_compatible not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_realize(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =gdk_gl_context_realize(GDK_GL_CONTEXT(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_gl_context_realize(Stack stack, int rhs, int opt, int lhs) /* realize */
{
  Scierror("Error: function gdk_gl_context_realize not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_gl_context_make_current(NspGdkGLContext *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_gl_context_make_current(GDK_GL_CONTEXT(self->obj));
  return 0;
}

#else
int _wrap_gdk_gl_context_make_current(Stack stack, int rhs, int opt, int lhs) /* make_current */
{
  Scierror("Error: function gdk_gl_context_make_current not available\n");
  return RET_BUG;
}
#endif
static NspMethods gdkglcontext_methods[] = {
  {"get_display",(nsp_method *) _wrap_gdk_gl_context_get_display},
  {"get_window",(nsp_method *) _wrap_gdk_gl_context_get_window},
  {"get_shared_context",(nsp_method *) _wrap_gdk_gl_context_get_shared_context},
  {"get_version",(nsp_method *) _wrap_gdk_gl_context_get_version},
  {"set_required_version",(nsp_method *) _wrap_gdk_gl_context_set_required_version},
  {"get_required_version",(nsp_method *) _wrap_gdk_gl_context_get_required_version},
  {"set_debug_enabled",(nsp_method *) _wrap_gdk_gl_context_set_debug_enabled},
  {"get_debug_enabled",(nsp_method *) _wrap_gdk_gl_context_get_debug_enabled},
  {"set_forward_compatible",(nsp_method *) _wrap_gdk_gl_context_set_forward_compatible},
  {"get_forward_compatible",(nsp_method *) _wrap_gdk_gl_context_get_forward_compatible},
  {"realize",(nsp_method *) _wrap_gdk_gl_context_realize},
  {"make_current",(nsp_method *) _wrap_gdk_gl_context_make_current},
  { NULL, NULL}
};

static NspMethods *gdkglcontext_get_methods(void) { return gdkglcontext_methods;};
#endif /* GTK_CHECK_VERSION */
#if GTK_CHECK_VERSION(3,16,0) 
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkglcontext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;
#endif /* GTK_CHECK_VERSION */


/* -----------NspGdkWindow ----------- */


#define  NspGdkWindow_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkwindow.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkWindow inherits from GObject 
 */

int nsp_type_gdkwindow_id=0;
NspTypeGdkWindow *nsp_type_gdkwindow=NULL;

/*
 * Type object for NspGdkWindow 
 * all the instance of NspTypeGdkWindow share the same id. 
 * nsp_type_gdkwindow: is an instance of NspTypeGdkWindow 
 *    used for objects of NspGdkWindow type (i.e built with new_gdkwindow) 
 * other instances are used for derived classes 
 */
NspTypeGdkWindow *new_type_gdkwindow(type_mode mode)
{
  NspTypeGdkWindow *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkwindow != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkwindow;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkwindow_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkwindow_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkwindow;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkwindow */ 

  top->s_type =  (s_type_func *) nsp_gdkwindow_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkwindow_type_short_string;
  /* top->create = (create_func*) int_gdkwindow_create;*/

  /* specific methods for gdkwindow */

  type->init = (init_func *) init_gdkwindow;

  /* 
   * NspGdkWindow interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkwindow_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkWindow called nsp_type_gdkwindow
       */
      type->id =  nsp_type_gdkwindow_id = nsp_new_type_id();
      nsp_type_gdkwindow = type;
      if ( nsp_register_type(nsp_type_gdkwindow) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkwindow, GDK_TYPE_WINDOW);
      return ( mode == T_BASE ) ? type : new_type_gdkwindow(mode);
    }
  else 
    {
      type->id = nsp_type_gdkwindow_id;
      return type;
    }
}

/*
 * initialize NspGdkWindow instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkwindow(NspGdkWindow *Obj,NspTypeGdkWindow *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkWindow 
 */

NspGdkWindow *new_gdkwindow() 
{
  NspGdkWindow *loc;
  /* type must exists */
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkWindow)))== NULLGDKWINDOW) return loc;
  /* initialize object */
  if ( init_gdkwindow(loc,nsp_type_gdkwindow) == FAIL) return NULLGDKWINDOW;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkWindow 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkwindow_type_name[]="GdkWindow";
static char gdkwindow_short_type_name[]="GdkWindow";

static char *nsp_gdkwindow_type_as_string(void)
{
  return(gdkwindow_type_name);
}

static char *nsp_gdkwindow_type_short_string(NspObject *v)
{
  return(gdkwindow_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkWindow objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkWindow   *nsp_gdkwindow_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkwindow_id)  == TRUE  ) return ((NspGdkWindow *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkwindow));
  return NULL;
}

int IsGdkWindowObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkwindow_id);
}

int IsGdkWindow(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkwindow_id);
}

NspGdkWindow  *GetGdkWindowCopy(Stack stack, int i)
{
  if (  GetGdkWindow(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkWindow  *GetGdkWindow(Stack stack, int i)
{
  NspGdkWindow *M;
  if (( M = nsp_gdkwindow_object(NthObj(i))) == NULLGDKWINDOW)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkWindow *gdkwindow_copy(NspGdkWindow *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkwindow);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkwindow);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkWindow
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 1681 "codegen-3.0/gdk.override"
static int
_wrap_gdk_property_get(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "property", "type", "pdelete", NULL };*/
  NspObject *nsp_property, *nsp_type = NULL;
  GdkAtom property, type;
  gint pdelete = FALSE;

  GdkAtom atype;
  gint aformat, alength;
  guchar *data;
  int_types T[] = { obj, new_opts, t_end};

  CheckLhs(3,3);
  nsp_option opts[] ={{ "type",obj,NULLOBJ,-1},
		      { "pdelete",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if (GetArgs(stack,rhs,opt,T, &nsp_property, &opts, &nsp_type, &pdelete) == FAIL )
    return RET_BUG;

  if ( nsp_gdk_atom_from_object(nsp_property,&property)== FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_type,&type)== FAIL) return RET_BUG;

  if (gdk_property_get(GDK_WINDOW(self->obj), property, type, 0, 9999,
		       pdelete, &atype, &aformat, &alength, &data)) {
    /* success */
    NspObject *ret;
    NspObject *pdata = NULL;
    gint i;
    guint16 *data16;
    guint32 *data32;
    switch (aformat) {
    case 8:
      if ((pdata = nsp_new_string_obj(NVOID,(const char *)data, alength)) == NULL) return RET_BUG;
      break;
    case 16:
      data16 = (guint16 *)data;
      if ((pdata = (NspObject *) nsp_matrix_create(NVOID,'r',1,alength)) == NULL) return RET_BUG;
      for (i = 0; i < alength; i++) ((NspMatrix *) pdata)->R[i]= data16[i];
      break;
    case 32:
      data32 = (guint32 *)data;
      if ((pdata = (NspObject *) nsp_matrix_create(NVOID,'r',1,alength)) == NULL) return RET_BUG;
      for (i = 0; i < alength; i++) ((NspMatrix *) pdata)->R[i]= data32[i];
      break;
    default:
      g_warning("got a property format != 8, 16 or 32");
      g_assert_not_reached();
    }
    g_free(data);
    if ((ret = (NspObject *) gdkatom_create(NVOID,NULL,atype,NULL)) == NULL) return RET_BUG;
    MoveObj(stack,1,ret);
    if ( nsp_move_double(stack,2,(double) aformat) == FAIL) return RET_BUG;
    MoveObj(stack,3,pdata);
    return 3;
  } else {
    return RET_BUG;
  }
}
#line 4763 "gdk.c"


#line 1743 "codegen-3.0/gdk.override"
static int
_wrap_gdk_property_change(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "property","type","format","mode","data",NULL };*/
  /*
  NspObject *nsp_property, *nsp_type;
  GdkAtom property, type;
  gint format;
  NspObject *nsp_mode, *pdata;
  GdkPropMode mode;
  guchar *data = NULL;
  gint nelements;
  int_types T[] = {obj,obj, s_int,obj,obj, t_end};
  if (GetArgs(stack,rhs,opt,T,
      / * "OOiOO:GdkWindow.property_change" * /
	      &nsp_property, &nsp_type, &format, &nsp_mode,
	      &pdata) == FAIL ) {
    return RET_BUG;
  }
  if ( nsp_gdk_atom_from_object(nsp_property,&property)== FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_type,&type) == FAIL) return RET_BUG;

  if (nspg_enum_get_value(GDK_TYPE_PROP_MODE, nsp_mode,&mode))
    return RET_BUG;
  switch (format) {
  case 8:
    if (!NspString_Check(pdata)) {
      Scierror( "data not a string and format=8");
      return RET_BUG;
    }
    data = NspString_AsString(pdata);
    nelements = NspString_Size(pdata);
    break;
  case 16:
    {
      guint16 *data16;
      gint i;

      if (!NspSequence_Check(pdata)) {
	Scierror(
		 "data not a sequence and format=16");
	return RET_BUG;
      }
      nelements = NspSequence_Length(pdata);
      data16 = g_new(guint16, nelements);
      data = (guchar *)data16;
      for (i = 0; i < nelements; i++) {
	NspObject *item = NspSequence_GetItem(pdata, i);
	Nsp_DECREF(item);
	item = NspNumber_Int(item);
	if (!item) {
	  g_free(data16);
	  NspErr_Clear();
	  Scierror("data element not an int");
	  return RET_BUG;
	}
	data16[i] = NspInt_AsLong(item);
	Nsp_DECREF(item);
      }
    }
    break;
  case 32:
    {
      guint32 *data32;
      gint i;

      if (!NspSequence_Check(pdata)) {
	Scierror(
		 "data not a sequence and format=32");
	return RET_BUG;
      }
      nelements = NspSequence_Length(pdata);
      data32 = g_new(guint32, nelements);
      data = (guchar *)data32;
      for (i = 0; i < nelements; i++) {
	NspObject *item = NspSequence_GetItem(pdata, i);
	Nsp_DECREF(item);
	item = NspNumber_Int(item);
	if (!item) {
	  g_free(data32);
	  NspErr_Clear();
	  Scierror("data element not an int");
	  return RET_BUG;
	}
	data32[i] = NspInt_AsLong(item);
	Nsp_DECREF(item);
      }
    }
    break;
  default:
    Scierror( "format must be 8, 16 or 32");
    return RET_BUG;
    break;
  }
  gdk_property_change(GDK_WINDOW(self->obj), property, type, format, mode,
		      data, nelements);
  if (format != 8)
    g_free(data);
    */
  Scierror("To be done XXXXXX ");
  return RET_BUG;
}
#line 4869 "gdk.c"


static int _wrap_gdk_property_delete(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkAtom property;
  NspObject *nsp_property = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_property) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_property,&property)==FAIL) return RET_BUG;
    gdk_property_delete(GDK_WINDOW(self->obj),property);
  return 0;
}

static int _wrap_gdk_window_destroy(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_destroy(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_get_window_type(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_window_get_window_type(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_is_destroyed(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_is_destroyed(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_get_visual(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkVisual *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_visual(GDK_WINDOW(self->obj));
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_get_screen(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkScreen *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_screen(GDK_WINDOW(self->obj));
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_get_display(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_display(GDK_WINDOW(self->obj));
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_show(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_show(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_hide(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_hide(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_withdraw(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_withdraw(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_show_unraised(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_show_unraised(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_move(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int x, y;
  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
    gdk_window_move(GDK_WINDOW(self->obj),x,y);
  return 0;
}

static int _wrap_gdk_window_resize(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int width, height;
  if ( GetArgs(stack,rhs,opt,T,&width, &height) == FAIL) return RET_BUG;
    gdk_window_resize(GDK_WINDOW(self->obj),width,height);
  return 0;
}

static int _wrap_gdk_window_move_resize(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int,s_int, t_end};
  int x, y, width, height;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &width, &height) == FAIL) return RET_BUG;
    gdk_window_move_resize(GDK_WINDOW(self->obj),x,y,width,height);
  return 0;
}

static int _wrap_gdk_window_reparent(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int,s_int, t_end};
  NspGObject *new_parent;
  int x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &new_parent, &x, &y) == FAIL) return RET_BUG;
    gdk_window_reparent(GDK_WINDOW(self->obj),GDK_WINDOW(new_parent->obj),x,y);
  return 0;
}

static int _wrap_gdk_window_raise(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_raise(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_lower(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_lower(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_restack(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_bool, t_end};
  NspGObject *sibling;
  int above;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &sibling, &above) == FAIL) return RET_BUG;
    gdk_window_restack(GDK_WINDOW(self->obj),GDK_WINDOW(sibling->obj),above);
  return 0;
}

static int _wrap_gdk_window_focus(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {new_opts, t_end};
  nsp_option opts[] = {
	{"timestamp",s_int,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  gulong timestamp = GDK_CURRENT_TIME;
  if ( GetArgs(stack,rhs,opt,T,opts, &timestamp) == FAIL) return RET_BUG;
    gdk_window_focus(GDK_WINDOW(self->obj),timestamp);
  return 0;
}

static int _wrap_gdk_window_set_override_redirect(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int override_redirect;
  if ( GetArgs(stack,rhs,opt,T,&override_redirect) == FAIL) return RET_BUG;
    gdk_window_set_override_redirect(GDK_WINDOW(self->obj),override_redirect);
  return 0;
}

static int _wrap_gdk_window_get_accept_focus(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_accept_focus(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_accept_focus(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int accept_focus;
  if ( GetArgs(stack,rhs,opt,T,&accept_focus) == FAIL) return RET_BUG;
    gdk_window_set_accept_focus(GDK_WINDOW(self->obj),accept_focus);
  return 0;
}

static int _wrap_gdk_window_get_focus_on_map(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_focus_on_map(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_focus_on_map(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int focus_on_map;
  if ( GetArgs(stack,rhs,opt,T,&focus_on_map) == FAIL) return RET_BUG;
    gdk_window_set_focus_on_map(GDK_WINDOW(self->obj),focus_on_map);
  return 0;
}

static int _wrap_gdk_window_scroll(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int dx, dy;
  if ( GetArgs(stack,rhs,opt,T,&dx, &dy) == FAIL) return RET_BUG;
    gdk_window_scroll(GDK_WINDOW(self->obj),dx,dy);
  return 0;
}

static int _wrap_gdk_window_move_region(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_int,s_int, t_end};
  cairo_region_t *region = NULL;
  NspObject *nsp_region = NULL;
  int dx, dy;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region, &dx, &dy) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    gdk_window_move_region(GDK_WINDOW(self->obj),region,dx,dy);
  return 0;
}

static int _wrap_gdk_window_ensure_native(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_ensure_native(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_shape_combine_region(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_int,s_int, t_end};
  cairo_region_t *shape_region = NULL;
  NspObject *nsp_shape_region = NULL;
  int offset_x, offset_y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_shape_region, &offset_x, &offset_y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_shape_region, CAIRO_GOBJECT_TYPE_REGION))
      shape_region = nspg_boxed_get(nsp_shape_region, cairo_region_t);
  else {
      Scierror( "Error: shape_region should be a cairo_region_t\n");
      return RET_BUG;
  }
    gdk_window_shape_combine_region(GDK_WINDOW(self->obj),shape_region,offset_x,offset_y);
  return 0;
}

static int _wrap_gdk_window_set_child_shapes(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_set_child_shapes(GDK_WINDOW(self->obj));
  return 0;
}

#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_window_get_composited(Stack stack, int rhs, int opt, int lhs) /* get_composited */
{
  Scierror("Error: function gdk_window_get_composited is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_window_get_composited(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_composited(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#endif
#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_window_set_composited(Stack stack, int rhs, int opt, int lhs) /* set_composited */
{
  Scierror("Error: function gdk_window_set_composited is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_window_set_composited(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int composited;
  if ( GetArgs(stack,rhs,opt,T,&composited) == FAIL) return RET_BUG;
    gdk_window_set_composited(GDK_WINDOW(self->obj),composited);
  return 0;
}

#endif
static int _wrap_gdk_window_merge_child_shapes(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_merge_child_shapes(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_input_shape_combine_region(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_int,s_int, t_end};
  cairo_region_t *shape_region = NULL;
  NspObject *nsp_shape_region = NULL;
  int offset_x, offset_y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_shape_region, &offset_x, &offset_y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_shape_region, CAIRO_GOBJECT_TYPE_REGION))
      shape_region = nspg_boxed_get(nsp_shape_region, cairo_region_t);
  else {
      Scierror( "Error: shape_region should be a cairo_region_t\n");
      return RET_BUG;
  }
    gdk_window_input_shape_combine_region(GDK_WINDOW(self->obj),shape_region,offset_x,offset_y);
  return 0;
}

static int _wrap_gdk_window_set_child_input_shapes(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_set_child_input_shapes(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_merge_child_input_shapes(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_merge_child_input_shapes(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_is_visible(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_is_visible(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_is_viewable(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_is_viewable(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_is_input_only(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_is_input_only(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_is_shaped(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_is_shaped(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_get_state(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gdk_window_get_state(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_window_set_static_gravities(Stack stack, int rhs, int opt, int lhs) /* set_static_gravities */
{
  Scierror("Error: function gdk_window_set_static_gravities is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_window_set_static_gravities(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int use_static, ret;
  if ( GetArgs(stack,rhs,opt,T,&use_static) == FAIL) return RET_BUG;
    ret =gdk_window_set_static_gravities(GDK_WINDOW(self->obj),use_static);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#endif
static int _wrap_gdk_window_has_native(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_has_native(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_type_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkWindowTypeHint hint;
  NspObject *nsp_hint = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_hint) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_WINDOW_TYPE_HINT, nsp_hint, &hint)== FAIL)
      return RET_BUG;
    gdk_window_set_type_hint(GDK_WINDOW(self->obj),hint);
  return 0;
}

static int _wrap_gdk_window_get_type_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_window_get_type_hint(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_get_modal_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_modal_hint(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_modal_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int modal;
  if ( GetArgs(stack,rhs,opt,T,&modal) == FAIL) return RET_BUG;
    gdk_window_set_modal_hint(GDK_WINDOW(self->obj),modal);
  return 0;
}

static int _wrap_gdk_window_set_skip_taskbar_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int skips_taskbar;
  if ( GetArgs(stack,rhs,opt,T,&skips_taskbar) == FAIL) return RET_BUG;
    gdk_window_set_skip_taskbar_hint(GDK_WINDOW(self->obj),skips_taskbar);
  return 0;
}

static int _wrap_gdk_window_set_skip_pager_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int skips_pager;
  if ( GetArgs(stack,rhs,opt,T,&skips_pager) == FAIL) return RET_BUG;
    gdk_window_set_skip_pager_hint(GDK_WINDOW(self->obj),skips_pager);
  return 0;
}

static int _wrap_gdk_window_set_urgency_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int urgent;
  if ( GetArgs(stack,rhs,opt,T,&urgent) == FAIL) return RET_BUG;
    gdk_window_set_urgency_hint(GDK_WINDOW(self->obj),urgent);
  return 0;
}

static int _wrap_gdk_window_get_clip_region(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  cairo_region_t *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_clip_region(GDK_WINDOW(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_REGION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_region_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_get_visible_region(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  cairo_region_t *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_visible_region(GDK_WINDOW(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_REGION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_region_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_window_mark_paint_from_clip(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    gdk_window_mark_paint_from_clip(GDK_WINDOW(self->obj),cr);
  return 0;
}

#else
int _wrap_gdk_window_mark_paint_from_clip(Stack stack, int rhs, int opt, int lhs) /* mark_paint_from_clip */
{
  Scierror("Error: function gdk_window_mark_paint_from_clip not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_window_begin_paint_region(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  cairo_region_t *region = NULL;
  NspObject *nsp_region = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    gdk_window_begin_paint_region(GDK_WINDOW(self->obj),region);
  return 0;
}

static int _wrap_gdk_window_end_paint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_end_paint(GDK_WINDOW(self->obj));
  return 0;
}

#if GTK_CHECK_VERSION(3,14,0)
int _wrap_gdk_window_flush(Stack stack, int rhs, int opt, int lhs) /* flush */
{
  Scierror("Error: function gdk_window_flush is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_window_flush(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_flush(GDK_WINDOW(self->obj));
  return 0;
}

#endif
static int _wrap_gdk_window_set_title(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *title;
  if ( GetArgs(stack,rhs,opt,T,&title) == FAIL) return RET_BUG;
    gdk_window_set_title(GDK_WINDOW(self->obj),title);
  return 0;
}

static int _wrap_gdk_window_set_role(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *role;
  if ( GetArgs(stack,rhs,opt,T,&role) == FAIL) return RET_BUG;
    gdk_window_set_role(GDK_WINDOW(self->obj),role);
  return 0;
}

static int _wrap_gdk_window_set_startup_id(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *startup_id;
  if ( GetArgs(stack,rhs,opt,T,&startup_id) == FAIL) return RET_BUG;
    gdk_window_set_startup_id(GDK_WINDOW(self->obj),startup_id);
  return 0;
}

static int _wrap_gdk_window_set_transient_for(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *parent;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &parent) == FAIL) return RET_BUG;
    gdk_window_set_transient_for(GDK_WINDOW(self->obj),GDK_WINDOW(parent->obj));
  return 0;
}

static int _wrap_gdk_window_set_background_rgba(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkRGBA *rgba = NULL;
  NspObject *nsp_rgba = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_rgba) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_rgba, GDK_TYPE_RGBA))
      rgba = nspg_boxed_get(nsp_rgba, GdkRGBA);
  else {
      Scierror( "Error: rgba should be a GdkRGBA\n");
      return RET_BUG;
  }
    gdk_window_set_background_rgba(GDK_WINDOW(self->obj),rgba);
  return 0;
}

static int _wrap_gdk_window_set_background_pattern(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  cairo_pattern_t *pattern = NULL;
  NspObject *nsp_pattern = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pattern) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_pattern, CAIRO_GOBJECT_TYPE_PATTERN))
      pattern = nspg_boxed_get(nsp_pattern, cairo_pattern_t);
  else {
      Scierror( "Error: pattern should be a cairo_pattern_t\n");
      return RET_BUG;
  }
    gdk_window_set_background_pattern(GDK_WINDOW(self->obj),pattern);
  return 0;
}

static int _wrap_gdk_window_get_background_pattern(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  cairo_pattern_t *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_background_pattern(GDK_WINDOW(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_PATTERN, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_pattern_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_set_cursor(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {new_opts, t_end};
  nsp_option opts[] = {
	{"cursor",obj,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  GdkCursor *cursor = NULL;
  NspGObject *nsp_cursor = NULL;
  if ( GetArgs(stack,rhs,opt,T,opts, &nsp_cursor) == FAIL) return RET_BUG;
  if ( nsp_cursor != NULL ) {
    if ( IsGdkCursor((NspObject *)nsp_cursor))
      cursor = GDK_CURSOR(nsp_cursor->obj);
    else if (! IsNone((NspObject *)nsp_cursor)) {
         Scierror( "Error: cursor should be a GdkCursor or None\n");
         return RET_BUG;
    }
  }
    gdk_window_set_cursor(GDK_WINDOW(self->obj),cursor);
  return 0;
}

static int _wrap_gdk_window_get_cursor(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkCursor *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_cursor(GDK_WINDOW(self->obj));
  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcursor))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_set_device_cursor(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *device, *cursor;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device, &nsp_type_gdkcursor, &cursor) == FAIL) return RET_BUG;
    gdk_window_set_device_cursor(GDK_WINDOW(self->obj),GDK_DEVICE(device->obj),GDK_CURSOR(cursor->obj));
  return 0;
}

static int _wrap_gdk_window_get_device_cursor(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *device;
  GdkCursor *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device) == FAIL) return RET_BUG;
    ret =gdk_window_get_device_cursor(GDK_WINDOW(self->obj),GDK_DEVICE(device->obj));
  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcursor))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 1847 "codegen-3.0/gdk.override"
static int
_wrap_gdk_window_get_geometry(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y, width, height;
  gdk_window_get_geometry(GDK_WINDOW(self->obj), &x, &y, &width, &height);
  if ( nsp_move_doubles(stack,1,1,4,(double) x,(double) y,
			(double) width,(double) height) == FAIL) return RET_BUG;
  return 1;
}
#line 5594 "gdk.c"


static int _wrap_gdk_window_get_width(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_width(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_get_height(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_height(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 1858 "codegen-3.0/gdk.override"
static int
_wrap_gdk_window_get_position(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;

  gdk_window_get_position(GDK_WINDOW(self->obj), &x, &y);
  if ( nsp_move_doubles(stack,1,1,2,(double) x,(double) y) == FAIL) return RET_BUG;
  return 1;
}
#line 5625 "gdk.c"


#line 1869 "codegen-3.0/gdk.override"
static int
_wrap_gdk_window_get_origin(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;

  gdk_window_get_origin(GDK_WINDOW(self->obj), &x, &y);
  if ( nsp_move_doubles(stack,1,1,2,(double) x,(double) y) == FAIL) return RET_BUG;
  return 1;
}
#line 5638 "gdk.c"


static int _wrap_gdk_window_get_root_coords(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int,s_int, t_end};
  int x, y, root_x, root_y;
  if ( GetArgs(stack,rhs,opt,T,&x, &y, &root_x, &root_y) == FAIL) return RET_BUG;
    gdk_window_get_root_coords(GDK_WINDOW(self->obj),x,y,&root_x,&root_y);
  return 0;
}

#line 1932 "codegen-3.0/gdk.override"
static int
_wrap_gdk_window_get_pointer(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;
  GdkModifierType mask;

  gdk_window_get_pointer(GDK_WINDOW(self->obj), &x, &y, &mask);
  if ( nsp_move_doubles(stack,1,1,3,(double) x,(double) y, (double) mask) == FAIL) return RET_BUG;
  return 1;
}
#line 5661 "gdk.c"


#line 1892 "codegen-3.0/gdk.override"

static int _wrap_gdk_window_coords_to_parent(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckLhs(0,1);
  int_types T[] = {s_double,s_double ,t_end};
  double x, y, parent_x, parent_y;
  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
  gdk_window_coords_to_parent(GDK_WINDOW(self->obj),x,y,&parent_x,&parent_y);
  if ( nsp_move_doubles(stack,1,1,2,(double) parent_x,(double) parent_y) == FAIL) return RET_BUG;
  return 1;
}

#line 5677 "gdk.c"


#line 1906 "codegen-3.0/gdk.override"

static int _wrap_gdk_window_coords_from_parent(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckLhs(0,1);
  int_types T[] = {s_double,s_double, t_end};
  double parent_x, parent_y, x, y;
  if ( GetArgs(stack,rhs,opt,T,&parent_x, &parent_y) == FAIL) return RET_BUG;
  gdk_window_coords_from_parent(GDK_WINDOW(self->obj),parent_x,parent_y,&x,&y);
  if ( nsp_move_doubles(stack,1,1,2,(double) x,(double) y) == FAIL) return RET_BUG;
  return 1;
}


#line 5694 "gdk.c"


#line 1921 "codegen-3.0/gdk.override"
static int
_wrap_gdk_window_get_root_origin(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;

  gdk_window_get_root_origin(GDK_WINDOW(self->obj), &x, &y);
  if ( nsp_move_doubles(stack,1,1,2,(double) x,(double) y) == FAIL) return RET_BUG;
  return 1;
}
#line 5707 "gdk.c"


static int _wrap_gdk_window_get_frame_extents(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkRectangle rect = { 0, 0, 0, 0 };
  NspObject *nsp_rect;
  if ( GetArgs(stack,rhs,opt,T,&nsp_rect) == FAIL) return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_rect, &rect))
      return RET_BUG;
    gdk_window_get_frame_extents(GDK_WINDOW(self->obj),&rect);
  return 0;
}

#if GTK_CHECK_VERSION(3,10,0)
static int _wrap_gdk_window_get_scale_factor(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_scale_factor(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_window_get_scale_factor(Stack stack, int rhs, int opt, int lhs) /* get_scale_factor */
{
  Scierror("Error: function gdk_window_get_scale_factor not available\n");
  return RET_BUG;
}
#endif
#line 2574 "codegen-3.0/gdk.override"

/* XXXX Comprendre pourquoi GdkModifierType n'est pas reconnu dans la traduction
 * en enlevant le override
 * peut-etre parce qu'il est en GdkModifierType*
 */
static int
_wrap_gdk_window_get_device_position(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GdkModifierType state;
  gint x,y;
  GdkWindow *window = GDK_WINDOW(self->obj);
  NspGdkDevice *nsp_device;
  int_types T[] = { obj_check, t_end};
  if (GetArgs(stack,rhs,opt,T, &nsp_type_gdkdevice, &nsp_device) == FAIL)
    return RET_BUG;
  gdk_window_get_device_position(window, GDK_DEVICE(nsp_device->obj),&x,&y,&state);
  if ( nsp_move_doubles(stack,1,1,3,(double) x,(double) y,(double) state) == FAIL) return RET_BUG;
  return 1;
}
#line 5759 "gdk.c"


static int _wrap_gdk_window_get_parent(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_parent(GDK_WINDOW(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_get_toplevel(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_toplevel(GDK_WINDOW(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_get_effective_parent(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_effective_parent(GDK_WINDOW(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_get_effective_toplevel(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_effective_toplevel(GDK_WINDOW(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 1944 "codegen-3.0/gdk.override"
static int
_wrap_gdk_window_get_children(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GList *children, *tmp;
  NspList *ret;

  if (( ret =nsp_list_create(NVOID) ) == NULLLIST) return RET_BUG;
  children = gdk_window_get_children(GDK_WINDOW(self->obj));

  for (tmp = children; tmp != NULL; tmp = tmp->next) {
    NspObject *item = (NspObject *) nspgobject_new("lel",(GObject *)tmp->data);
    if ( item == NULLOBJ) goto clean;
    if ( nsp_list_end_insert(ret, item) == FAIL ) goto clean;
  }
  g_list_free(children);
  MoveObj(stack,1,(NspObject *) ret);
  return 1;
  clean :
    {
      g_list_free(children);
      nsp_list_destroy(ret);
      return RET_BUG;
    }
}
#line 5835 "gdk.c"


static int _wrap_gdk_window_peek_children(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_window_peek_children(GDK_WINDOW(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_gdk_window_get_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gdk_window_get_events(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkEventMask event_mask;
  NspObject *nsp_event_mask = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_event_mask) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_EVENT_MASK, nsp_event_mask, &event_mask)==FAIL)
      return RET_BUG;
    gdk_window_set_events(GDK_WINDOW(self->obj),event_mask);
  return 0;
}

static int _wrap_gdk_window_set_device_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *device;
  GdkEventMask event_mask;
  NspObject *nsp_event_mask = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device, &nsp_event_mask) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_EVENT_MASK, nsp_event_mask, &event_mask)==FAIL)
      return RET_BUG;
    gdk_window_set_device_events(GDK_WINDOW(self->obj),GDK_DEVICE(device->obj),event_mask);
  return 0;
}

static int _wrap_gdk_window_get_device_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *device;
  guint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device) == FAIL) return RET_BUG;
    ret =gdk_window_get_device_events(GDK_WINDOW(self->obj),GDK_DEVICE(device->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_source_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj, t_end};
  GdkInputSource source;
  NspObject *nsp_source = NULL, *nsp_event_mask = NULL;
  GdkEventMask event_mask;
  if ( GetArgs(stack,rhs,opt,T,&nsp_source, &nsp_event_mask) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INPUT_SOURCE, nsp_source, &source)== FAIL)
      return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_EVENT_MASK, nsp_event_mask, &event_mask)==FAIL)
      return RET_BUG;
    gdk_window_set_source_events(GDK_WINDOW(self->obj),source,event_mask);
  return 0;
}

static int _wrap_gdk_window_get_source_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkInputSource source;
  NspObject *nsp_source = NULL;
  guint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_source) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INPUT_SOURCE, nsp_source, &source)== FAIL)
      return RET_BUG;
    ret =gdk_window_get_source_events(GDK_WINDOW(self->obj),source);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_icon_list(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {list, t_end};
  NspList *nsp_pixbufs;
  GList *pixbufs;
  if ( GetArgs(stack,rhs,opt,T,&nsp_pixbufs) == FAIL) return RET_BUG;
  pixbufs=nsp_glist_from_nsplist(stack,nsp_pixbufs);
  if (pixbufs== NULL) return RET_BUG;
    gdk_window_set_icon_list(GDK_WINDOW(self->obj),pixbufs);
  return 0;
}

static int _wrap_gdk_window_set_icon_name(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    gdk_window_set_icon_name(GDK_WINDOW(self->obj),name);
  return 0;
}

static int _wrap_gdk_window_set_group(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *leader;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &leader) == FAIL) return RET_BUG;
    gdk_window_set_group(GDK_WINDOW(self->obj),GDK_WINDOW(leader->obj));
  return 0;
}

static int _wrap_gdk_window_get_group(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_group(GDK_WINDOW(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_set_decorations(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkWMDecoration decorations;
  NspObject *nsp_decorations = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_decorations) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_WM_DECORATION, nsp_decorations, &decorations)==FAIL)
      return RET_BUG;
    gdk_window_set_decorations(GDK_WINDOW(self->obj),decorations);
  return 0;
}

static int _wrap_gdk_window_set_functions(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkWMFunction functions;
  NspObject *nsp_functions = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_functions) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_WM_FUNCTION, nsp_functions, &functions)==FAIL)
      return RET_BUG;
    gdk_window_set_functions(GDK_WINDOW(self->obj),functions);
  return 0;
}

static int _wrap_gdk_window_create_similar_surface(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_int,s_int, t_end};
  cairo_content_t content;
  NspObject *nsp_content = NULL, *nsp_ret;
  int width, height;
  cairo_surface_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_content, &width, &height) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_CONTENT, nsp_content, &content)== FAIL)
      return RET_BUG;
    ret =gdk_window_create_similar_surface(GDK_WINDOW(self->obj),content,width,height);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(3,10,0)
static int _wrap_gdk_window_create_similar_image_surface(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_int,s_int,s_int, t_end};
  cairo_format_t format;
  NspObject *nsp_format = NULL, *nsp_ret;
  int width, height, scale;
  cairo_surface_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_format, &width, &height, &scale) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(CAIRO_GOBJECT_TYPE_FORMAT, nsp_format, &format)== FAIL)
      return RET_BUG;
    ret =gdk_window_create_similar_image_surface(GDK_WINDOW(self->obj),format,width,height,scale);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_window_create_similar_image_surface(Stack stack, int rhs, int opt, int lhs) /* create_similar_image_surface */
{
  Scierror("Error: function gdk_window_create_similar_image_surface not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_window_beep(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_beep(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_iconify(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_iconify(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_deiconify(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_deiconify(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_stick(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_stick(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_unstick(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_unstick(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_maximize(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_maximize(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_unmaximize(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_unmaximize(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_fullscreen(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_fullscreen(GDK_WINDOW(self->obj));
  return 0;
}

#if GTK_CHECK_VERSION(3,8,0)
static int _wrap_gdk_window_set_fullscreen_mode(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkFullscreenMode mode;
  NspObject *nsp_mode = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_mode) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_FULLSCREEN_MODE, nsp_mode, &mode)== FAIL)
      return RET_BUG;
    gdk_window_set_fullscreen_mode(GDK_WINDOW(self->obj),mode);
  return 0;
}

#else
int _wrap_gdk_window_set_fullscreen_mode(Stack stack, int rhs, int opt, int lhs) /* set_fullscreen_mode */
{
  Scierror("Error: function gdk_window_set_fullscreen_mode not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,8,0)
static int _wrap_gdk_window_get_fullscreen_mode(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_window_get_fullscreen_mode(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_window_get_fullscreen_mode(Stack stack, int rhs, int opt, int lhs) /* get_fullscreen_mode */
{
  Scierror("Error: function gdk_window_get_fullscreen_mode not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_window_unfullscreen(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_unfullscreen(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_set_keep_above(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int setting;
  if ( GetArgs(stack,rhs,opt,T,&setting) == FAIL) return RET_BUG;
    gdk_window_set_keep_above(GDK_WINDOW(self->obj),setting);
  return 0;
}

static int _wrap_gdk_window_set_keep_below(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int setting;
  if ( GetArgs(stack,rhs,opt,T,&setting) == FAIL) return RET_BUG;
    gdk_window_set_keep_below(GDK_WINDOW(self->obj),setting);
  return 0;
}

static int _wrap_gdk_window_set_opacity(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_double, t_end};
  double opacity;
  if ( GetArgs(stack,rhs,opt,T,&opacity) == FAIL) return RET_BUG;
    gdk_window_set_opacity(GDK_WINDOW(self->obj),opacity);
  return 0;
}

static int _wrap_gdk_window_register_dnd(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_register_dnd(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_begin_resize_drag(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_int,s_int,s_int,s_int, t_end};
  GdkWindowEdge edge;
  NspObject *nsp_edge = NULL;
  int button, root_x, root_y;
  gulong timestamp;
  if ( GetArgs(stack,rhs,opt,T,&nsp_edge, &button, &root_x, &root_y, &timestamp) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_WINDOW_EDGE, nsp_edge, &edge)== FAIL)
      return RET_BUG;
    gdk_window_begin_resize_drag(GDK_WINDOW(self->obj),edge,button,root_x,root_y,timestamp);
  return 0;
}

#if GTK_CHECK_VERSION(3,4,0)
static int _wrap_gdk_window_begin_resize_drag_for_device(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj_check,s_int,s_int,s_int,s_int, t_end};
  GdkWindowEdge edge;
  NspObject *nsp_edge = NULL;
  NspGObject *device;
  int button, root_x, root_y;
  gulong timestamp;
  if ( GetArgs(stack,rhs,opt,T,&nsp_edge, &nsp_type_gdkdevice, &device, &button, &root_x, &root_y, &timestamp) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_WINDOW_EDGE, nsp_edge, &edge)== FAIL)
      return RET_BUG;
    gdk_window_begin_resize_drag_for_device(GDK_WINDOW(self->obj),edge,GDK_DEVICE(device->obj),button,root_x,root_y,timestamp);
  return 0;
}

#else
int _wrap_gdk_window_begin_resize_drag_for_device(Stack stack, int rhs, int opt, int lhs) /* begin_resize_drag_for_device */
{
  Scierror("Error: function gdk_window_begin_resize_drag_for_device not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_window_begin_move_drag(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int,s_int, t_end};
  int button, root_x, root_y;
  gulong timestamp;
  if ( GetArgs(stack,rhs,opt,T,&button, &root_x, &root_y, &timestamp) == FAIL) return RET_BUG;
    gdk_window_begin_move_drag(GDK_WINDOW(self->obj),button,root_x,root_y,timestamp);
  return 0;
}

#if GTK_CHECK_VERSION(3,4,0)
static int _wrap_gdk_window_begin_move_drag_for_device(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int,s_int,s_int,s_int, t_end};
  NspGObject *device;
  int button, root_x, root_y;
  gulong timestamp;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdevice, &device, &button, &root_x, &root_y, &timestamp) == FAIL) return RET_BUG;
    gdk_window_begin_move_drag_for_device(GDK_WINDOW(self->obj),GDK_DEVICE(device->obj),button,root_x,root_y,timestamp);
  return 0;
}

#else
int _wrap_gdk_window_begin_move_drag_for_device(Stack stack, int rhs, int opt, int lhs) /* begin_move_drag_for_device */
{
  Scierror("Error: function gdk_window_begin_move_drag_for_device not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_window_invalidate_rect(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {new_opts, t_end};
  nsp_option opts[] = {
	{"rect",obj,NULLOBJ,-1},
	{"invalidate_children",s_bool,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  GdkRectangle rect_rect = { 0, 0, 0, 0 }, *rect;
  NspObject *nsp_rect = NULL;
  int invalidate_children;
  if ( GetArgs(stack,rhs,opt,T,opts, &nsp_rect, &invalidate_children) == FAIL) return RET_BUG;
  if (nsp_rect == NULL)
      rect = NULL;
  else if (nsp_gdk_rectangle_from_object(nsp_rect, &rect_rect))
      rect = &rect_rect;
  else
          return RET_BUG;
    gdk_window_invalidate_rect(GDK_WINDOW(self->obj),rect,invalidate_children);
  return 0;
}

static int _wrap_gdk_window_invalidate_region(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,s_bool, t_end};
  cairo_region_t *region = NULL;
  NspObject *nsp_region = NULL;
  int invalidate_children;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region, &invalidate_children) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    gdk_window_invalidate_region(GDK_WINDOW(self->obj),region,invalidate_children);
  return 0;
}

static int _wrap_gdk_window_get_update_area(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  cairo_region_t *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_update_area(GDK_WINDOW(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_REGION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_region_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_freeze_updates(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_freeze_updates(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_thaw_updates(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_thaw_updates(GDK_WINDOW(self->obj));
  return 0;
}

#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_window_freeze_toplevel_updates_libgtk_only(Stack stack, int rhs, int opt, int lhs) /* freeze_toplevel_updates_libgtk_only */
{
  Scierror("Error: function gdk_window_freeze_toplevel_updates_libgtk_only is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_window_freeze_toplevel_updates_libgtk_only(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_freeze_toplevel_updates_libgtk_only(GDK_WINDOW(self->obj));
  return 0;
}

#endif
#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_window_thaw_toplevel_updates_libgtk_only(Stack stack, int rhs, int opt, int lhs) /* thaw_toplevel_updates_libgtk_only */
{
  Scierror("Error: function gdk_window_thaw_toplevel_updates_libgtk_only is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_window_thaw_toplevel_updates_libgtk_only(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_thaw_toplevel_updates_libgtk_only(GDK_WINDOW(self->obj));
  return 0;
}

#endif
static int _wrap_gdk_window_process_updates(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int update_children;
  if ( GetArgs(stack,rhs,opt,T,&update_children) == FAIL) return RET_BUG;
    gdk_window_process_updates(GDK_WINDOW(self->obj),update_children);
  return 0;
}

#if GTK_CHECK_VERSION(3,8,0)
int _wrap_gdk_window_enable_synchronized_configure(Stack stack, int rhs, int opt, int lhs) /* enable_synchronized_configure */
{
  Scierror("Error: function gdk_window_enable_synchronized_configure is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_window_enable_synchronized_configure(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_enable_synchronized_configure(GDK_WINDOW(self->obj));
  return 0;
}

#endif
#if GTK_CHECK_VERSION(3,8,0)
int _wrap_gdk_window_configure_finished(Stack stack, int rhs, int opt, int lhs) /* configure_finished */
{
  Scierror("Error: function gdk_window_configure_finished is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_gdk_window_configure_finished(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_configure_finished(GDK_WINDOW(self->obj));
  return 0;
}

#endif
static int _wrap_gdk_window_geometry_changed(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gdk_window_geometry_changed(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_set_support_multidevice(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int support_multidevice;
  if ( GetArgs(stack,rhs,opt,T,&support_multidevice) == FAIL) return RET_BUG;
    gdk_window_set_support_multidevice(GDK_WINDOW(self->obj),support_multidevice);
  return 0;
}

static int _wrap_gdk_window_get_support_multidevice(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_support_multidevice(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(3,8,0)
static int _wrap_gdk_window_get_frame_clock(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkFrameClock *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_get_frame_clock(GDK_WINDOW(self->obj));
  nsp_type_gdkframeclock = new_type_gdkframeclock(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkframeclock))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_window_get_frame_clock(Stack stack, int rhs, int opt, int lhs) /* get_frame_clock */
{
  Scierror("Error: function gdk_window_get_frame_clock not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,10,0)
static int _wrap_gdk_window_set_opaque_region(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  cairo_region_t *region = NULL;
  NspObject *nsp_region = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_region) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    gdk_window_set_opaque_region(GDK_WINDOW(self->obj),region);
  return 0;
}

#else
int _wrap_gdk_window_set_opaque_region(Stack stack, int rhs, int opt, int lhs) /* set_opaque_region */
{
  Scierror("Error: function gdk_window_set_opaque_region not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,12,0)
static int _wrap_gdk_window_set_event_compression(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int event_compression;
  if ( GetArgs(stack,rhs,opt,T,&event_compression) == FAIL) return RET_BUG;
    gdk_window_set_event_compression(GDK_WINDOW(self->obj),event_compression);
  return 0;
}

#else
int _wrap_gdk_window_set_event_compression(Stack stack, int rhs, int opt, int lhs) /* set_event_compression */
{
  Scierror("Error: function gdk_window_set_event_compression not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,12,0)
static int _wrap_gdk_window_get_event_compression(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_window_get_event_compression(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_window_get_event_compression(Stack stack, int rhs, int opt, int lhs) /* get_event_compression */
{
  Scierror("Error: function gdk_window_get_event_compression not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,12,0)
static int _wrap_gdk_window_set_shadow_width(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int,s_int, t_end};
  int left, right, top, bottom;
  if ( GetArgs(stack,rhs,opt,T,&left, &right, &top, &bottom) == FAIL) return RET_BUG;
    gdk_window_set_shadow_width(GDK_WINDOW(self->obj),left,right,top,bottom);
  return 0;
}

#else
int _wrap_gdk_window_set_shadow_width(Stack stack, int rhs, int opt, int lhs) /* set_shadow_width */
{
  Scierror("Error: function gdk_window_set_shadow_width not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,14,0)
static int _wrap_gdk_window_show_window_menu(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkEvent *event = NULL;
  NspObject *nsp_event = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_event) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_event, GDK_TYPE_EVENT))
      event = nspg_boxed_get(nsp_event, GdkEvent);
  else {
      Scierror( "Error: event should be a GdkEvent\n");
      return RET_BUG;
  }
    ret =gdk_window_show_window_menu(GDK_WINDOW(self->obj),event);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_window_show_window_menu(Stack stack, int rhs, int opt, int lhs) /* show_window_menu */
{
  Scierror("Error: function gdk_window_show_window_menu not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
static int _wrap_gdk_window_create_gl_context(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  GdkGLContext *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_window_create_gl_context(GDK_WINDOW(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  nsp_type_gdkglcontext = new_type_gdkglcontext(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkglcontext))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_window_create_gl_context(Stack stack, int rhs, int opt, int lhs) /* create_gl_context */
{
  Scierror("Error: function gdk_window_create_gl_context not available\n");
  return RET_BUG;
}
#endif
static NspMethods gdkwindow_methods[] = {
  {"property_get",(nsp_method *) _wrap_gdk_property_get},
  {"property_change",(nsp_method *) _wrap_gdk_property_change},
  {"property_delete",(nsp_method *) _wrap_gdk_property_delete},
  {"destroy",(nsp_method *) _wrap_gdk_window_destroy},
  {"get_window_type",(nsp_method *) _wrap_gdk_window_get_window_type},
  {"is_destroyed",(nsp_method *) _wrap_gdk_window_is_destroyed},
  {"get_visual",(nsp_method *) _wrap_gdk_window_get_visual},
  {"get_screen",(nsp_method *) _wrap_gdk_window_get_screen},
  {"get_display",(nsp_method *) _wrap_gdk_window_get_display},
  {"show",(nsp_method *) _wrap_gdk_window_show},
  {"hide",(nsp_method *) _wrap_gdk_window_hide},
  {"withdraw",(nsp_method *) _wrap_gdk_window_withdraw},
  {"show_unraised",(nsp_method *) _wrap_gdk_window_show_unraised},
  {"move",(nsp_method *) _wrap_gdk_window_move},
  {"resize",(nsp_method *) _wrap_gdk_window_resize},
  {"move_resize",(nsp_method *) _wrap_gdk_window_move_resize},
  {"reparent",(nsp_method *) _wrap_gdk_window_reparent},
  {"raise",(nsp_method *) _wrap_gdk_window_raise},
  {"lower",(nsp_method *) _wrap_gdk_window_lower},
  {"restack",(nsp_method *) _wrap_gdk_window_restack},
  {"focus",(nsp_method *) _wrap_gdk_window_focus},
  {"set_override_redirect",(nsp_method *) _wrap_gdk_window_set_override_redirect},
  {"get_accept_focus",(nsp_method *) _wrap_gdk_window_get_accept_focus},
  {"set_accept_focus",(nsp_method *) _wrap_gdk_window_set_accept_focus},
  {"get_focus_on_map",(nsp_method *) _wrap_gdk_window_get_focus_on_map},
  {"set_focus_on_map",(nsp_method *) _wrap_gdk_window_set_focus_on_map},
  {"scroll",(nsp_method *) _wrap_gdk_window_scroll},
  {"move_region",(nsp_method *) _wrap_gdk_window_move_region},
  {"ensure_native",(nsp_method *) _wrap_gdk_window_ensure_native},
  {"shape_combine_region",(nsp_method *) _wrap_gdk_window_shape_combine_region},
  {"set_child_shapes",(nsp_method *) _wrap_gdk_window_set_child_shapes},
  {"get_composited",(nsp_method *) _wrap_gdk_window_get_composited},
  {"set_composited",(nsp_method *) _wrap_gdk_window_set_composited},
  {"merge_child_shapes",(nsp_method *) _wrap_gdk_window_merge_child_shapes},
  {"input_shape_combine_region",(nsp_method *) _wrap_gdk_window_input_shape_combine_region},
  {"set_child_input_shapes",(nsp_method *) _wrap_gdk_window_set_child_input_shapes},
  {"merge_child_input_shapes",(nsp_method *) _wrap_gdk_window_merge_child_input_shapes},
  {"is_visible",(nsp_method *) _wrap_gdk_window_is_visible},
  {"is_viewable",(nsp_method *) _wrap_gdk_window_is_viewable},
  {"is_input_only",(nsp_method *) _wrap_gdk_window_is_input_only},
  {"is_shaped",(nsp_method *) _wrap_gdk_window_is_shaped},
  {"get_state",(nsp_method *) _wrap_gdk_window_get_state},
  {"set_static_gravities",(nsp_method *) _wrap_gdk_window_set_static_gravities},
  {"has_native",(nsp_method *) _wrap_gdk_window_has_native},
  {"set_type_hint",(nsp_method *) _wrap_gdk_window_set_type_hint},
  {"get_type_hint",(nsp_method *) _wrap_gdk_window_get_type_hint},
  {"get_modal_hint",(nsp_method *) _wrap_gdk_window_get_modal_hint},
  {"set_modal_hint",(nsp_method *) _wrap_gdk_window_set_modal_hint},
  {"set_skip_taskbar_hint",(nsp_method *) _wrap_gdk_window_set_skip_taskbar_hint},
  {"set_skip_pager_hint",(nsp_method *) _wrap_gdk_window_set_skip_pager_hint},
  {"set_urgency_hint",(nsp_method *) _wrap_gdk_window_set_urgency_hint},
  {"get_clip_region",(nsp_method *) _wrap_gdk_window_get_clip_region},
  {"get_visible_region",(nsp_method *) _wrap_gdk_window_get_visible_region},
  {"mark_paint_from_clip",(nsp_method *) _wrap_gdk_window_mark_paint_from_clip},
  {"begin_paint_region",(nsp_method *) _wrap_gdk_window_begin_paint_region},
  {"end_paint",(nsp_method *) _wrap_gdk_window_end_paint},
  {"flush",(nsp_method *) _wrap_gdk_window_flush},
  {"set_title",(nsp_method *) _wrap_gdk_window_set_title},
  {"set_role",(nsp_method *) _wrap_gdk_window_set_role},
  {"set_startup_id",(nsp_method *) _wrap_gdk_window_set_startup_id},
  {"set_transient_for",(nsp_method *) _wrap_gdk_window_set_transient_for},
  {"set_background_rgba",(nsp_method *) _wrap_gdk_window_set_background_rgba},
  {"set_background_pattern",(nsp_method *) _wrap_gdk_window_set_background_pattern},
  {"get_background_pattern",(nsp_method *) _wrap_gdk_window_get_background_pattern},
  {"set_cursor",(nsp_method *) _wrap_gdk_window_set_cursor},
  {"get_cursor",(nsp_method *) _wrap_gdk_window_get_cursor},
  {"set_device_cursor",(nsp_method *) _wrap_gdk_window_set_device_cursor},
  {"get_device_cursor",(nsp_method *) _wrap_gdk_window_get_device_cursor},
  {"get_geometry",(nsp_method *) _wrap_gdk_window_get_geometry},
  {"get_width",(nsp_method *) _wrap_gdk_window_get_width},
  {"get_height",(nsp_method *) _wrap_gdk_window_get_height},
  {"get_position",(nsp_method *) _wrap_gdk_window_get_position},
  {"get_origin",(nsp_method *) _wrap_gdk_window_get_origin},
  {"get_root_coords",(nsp_method *) _wrap_gdk_window_get_root_coords},
  {"get_pointer",(nsp_method *) _wrap_gdk_window_get_pointer},
  {"coords_to_parent",(nsp_method *) _wrap_gdk_window_coords_to_parent},
  {"coords_from_parent",(nsp_method *) _wrap_gdk_window_coords_from_parent},
  {"get_root_origin",(nsp_method *) _wrap_gdk_window_get_root_origin},
  {"get_frame_extents",(nsp_method *) _wrap_gdk_window_get_frame_extents},
  {"get_scale_factor",(nsp_method *) _wrap_gdk_window_get_scale_factor},
  {"get_device_position",(nsp_method *) _wrap_gdk_window_get_device_position},
  {"get_parent",(nsp_method *) _wrap_gdk_window_get_parent},
  {"get_toplevel",(nsp_method *) _wrap_gdk_window_get_toplevel},
  {"get_effective_parent",(nsp_method *) _wrap_gdk_window_get_effective_parent},
  {"get_effective_toplevel",(nsp_method *) _wrap_gdk_window_get_effective_toplevel},
  {"get_children",(nsp_method *) _wrap_gdk_window_get_children},
  {"peek_children",(nsp_method *) _wrap_gdk_window_peek_children},
  {"get_events",(nsp_method *) _wrap_gdk_window_get_events},
  {"set_events",(nsp_method *) _wrap_gdk_window_set_events},
  {"set_device_events",(nsp_method *) _wrap_gdk_window_set_device_events},
  {"get_device_events",(nsp_method *) _wrap_gdk_window_get_device_events},
  {"set_source_events",(nsp_method *) _wrap_gdk_window_set_source_events},
  {"get_source_events",(nsp_method *) _wrap_gdk_window_get_source_events},
  {"set_icon_list",(nsp_method *) _wrap_gdk_window_set_icon_list},
  {"set_icon_name",(nsp_method *) _wrap_gdk_window_set_icon_name},
  {"set_group",(nsp_method *) _wrap_gdk_window_set_group},
  {"get_group",(nsp_method *) _wrap_gdk_window_get_group},
  {"set_decorations",(nsp_method *) _wrap_gdk_window_set_decorations},
  {"set_functions",(nsp_method *) _wrap_gdk_window_set_functions},
  {"create_similar_surface",(nsp_method *) _wrap_gdk_window_create_similar_surface},
  {"create_similar_image_surface",(nsp_method *) _wrap_gdk_window_create_similar_image_surface},
  {"beep",(nsp_method *) _wrap_gdk_window_beep},
  {"iconify",(nsp_method *) _wrap_gdk_window_iconify},
  {"deiconify",(nsp_method *) _wrap_gdk_window_deiconify},
  {"stick",(nsp_method *) _wrap_gdk_window_stick},
  {"unstick",(nsp_method *) _wrap_gdk_window_unstick},
  {"maximize",(nsp_method *) _wrap_gdk_window_maximize},
  {"unmaximize",(nsp_method *) _wrap_gdk_window_unmaximize},
  {"fullscreen",(nsp_method *) _wrap_gdk_window_fullscreen},
  {"set_fullscreen_mode",(nsp_method *) _wrap_gdk_window_set_fullscreen_mode},
  {"get_fullscreen_mode",(nsp_method *) _wrap_gdk_window_get_fullscreen_mode},
  {"unfullscreen",(nsp_method *) _wrap_gdk_window_unfullscreen},
  {"set_keep_above",(nsp_method *) _wrap_gdk_window_set_keep_above},
  {"set_keep_below",(nsp_method *) _wrap_gdk_window_set_keep_below},
  {"set_opacity",(nsp_method *) _wrap_gdk_window_set_opacity},
  {"register_dnd",(nsp_method *) _wrap_gdk_window_register_dnd},
  {"begin_resize_drag",(nsp_method *) _wrap_gdk_window_begin_resize_drag},
  {"begin_resize_drag_for_device",(nsp_method *) _wrap_gdk_window_begin_resize_drag_for_device},
  {"begin_move_drag",(nsp_method *) _wrap_gdk_window_begin_move_drag},
  {"begin_move_drag_for_device",(nsp_method *) _wrap_gdk_window_begin_move_drag_for_device},
  {"invalidate_rect",(nsp_method *) _wrap_gdk_window_invalidate_rect},
  {"invalidate_region",(nsp_method *) _wrap_gdk_window_invalidate_region},
  {"get_update_area",(nsp_method *) _wrap_gdk_window_get_update_area},
  {"freeze_updates",(nsp_method *) _wrap_gdk_window_freeze_updates},
  {"thaw_updates",(nsp_method *) _wrap_gdk_window_thaw_updates},
  {"freeze_toplevel_updates_libgtk_only",(nsp_method *) _wrap_gdk_window_freeze_toplevel_updates_libgtk_only},
  {"thaw_toplevel_updates_libgtk_only",(nsp_method *) _wrap_gdk_window_thaw_toplevel_updates_libgtk_only},
  {"process_updates",(nsp_method *) _wrap_gdk_window_process_updates},
  {"enable_synchronized_configure",(nsp_method *) _wrap_gdk_window_enable_synchronized_configure},
  {"configure_finished",(nsp_method *) _wrap_gdk_window_configure_finished},
  {"geometry_changed",(nsp_method *) _wrap_gdk_window_geometry_changed},
  {"set_support_multidevice",(nsp_method *) _wrap_gdk_window_set_support_multidevice},
  {"get_support_multidevice",(nsp_method *) _wrap_gdk_window_get_support_multidevice},
  {"get_frame_clock",(nsp_method *) _wrap_gdk_window_get_frame_clock},
  {"set_opaque_region",(nsp_method *) _wrap_gdk_window_set_opaque_region},
  {"set_event_compression",(nsp_method *) _wrap_gdk_window_set_event_compression},
  {"get_event_compression",(nsp_method *) _wrap_gdk_window_get_event_compression},
  {"set_shadow_width",(nsp_method *) _wrap_gdk_window_set_shadow_width},
  {"show_window_menu",(nsp_method *) _wrap_gdk_window_show_window_menu},
  {"create_gl_context",(nsp_method *) _wrap_gdk_window_create_gl_context},
  { NULL, NULL}
};

static NspMethods *gdkwindow_get_methods(void) { return gdkwindow_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkwindow_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkKeymap ----------- */


#define  NspGdkKeymap_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkkeymap.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkKeymap inherits from GObject 
 */

int nsp_type_gdkkeymap_id=0;
NspTypeGdkKeymap *nsp_type_gdkkeymap=NULL;

/*
 * Type object for NspGdkKeymap 
 * all the instance of NspTypeGdkKeymap share the same id. 
 * nsp_type_gdkkeymap: is an instance of NspTypeGdkKeymap 
 *    used for objects of NspGdkKeymap type (i.e built with new_gdkkeymap) 
 * other instances are used for derived classes 
 */
NspTypeGdkKeymap *new_type_gdkkeymap(type_mode mode)
{
  NspTypeGdkKeymap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkkeymap != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkkeymap;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkkeymap_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkkeymap_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkkeymap;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkkeymap */ 

  top->s_type =  (s_type_func *) nsp_gdkkeymap_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkkeymap_type_short_string;
  /* top->create = (create_func*) int_gdkkeymap_create;*/

  /* specific methods for gdkkeymap */

  type->init = (init_func *) init_gdkkeymap;

  /* 
   * NspGdkKeymap interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkkeymap_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkKeymap called nsp_type_gdkkeymap
       */
      type->id =  nsp_type_gdkkeymap_id = nsp_new_type_id();
      nsp_type_gdkkeymap = type;
      if ( nsp_register_type(nsp_type_gdkkeymap) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkkeymap, GDK_TYPE_KEYMAP);
      return ( mode == T_BASE ) ? type : new_type_gdkkeymap(mode);
    }
  else 
    {
      type->id = nsp_type_gdkkeymap_id;
      return type;
    }
}

/*
 * initialize NspGdkKeymap instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkkeymap(NspGdkKeymap *Obj,NspTypeGdkKeymap *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkKeymap 
 */

NspGdkKeymap *new_gdkkeymap() 
{
  NspGdkKeymap *loc;
  /* type must exists */
  nsp_type_gdkkeymap = new_type_gdkkeymap(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkKeymap)))== NULLGDKKEYMAP) return loc;
  /* initialize object */
  if ( init_gdkkeymap(loc,nsp_type_gdkkeymap) == FAIL) return NULLGDKKEYMAP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkKeymap 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkkeymap_type_name[]="GdkKeymap";
static char gdkkeymap_short_type_name[]="GdkKeymap";

static char *nsp_gdkkeymap_type_as_string(void)
{
  return(gdkkeymap_type_name);
}

static char *nsp_gdkkeymap_type_short_string(NspObject *v)
{
  return(gdkkeymap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkKeymap objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkKeymap   *nsp_gdkkeymap_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkkeymap_id)  == TRUE  ) return ((NspGdkKeymap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkkeymap));
  return NULL;
}

int IsGdkKeymapObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkkeymap_id);
}

int IsGdkKeymap(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkkeymap_id);
}

NspGdkKeymap  *GetGdkKeymapCopy(Stack stack, int i)
{
  if (  GetGdkKeymap(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkKeymap  *GetGdkKeymap(Stack stack, int i)
{
  NspGdkKeymap *M;
  if (( M = nsp_gdkkeymap_object(NthObj(i))) == NULLGDKKEYMAP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkKeymap *gdkkeymap_copy(NspGdkKeymap *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkkeymap);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkkeymap);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkKeymap
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_keymap_get_direction(NspGdkKeymap *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_keymap_get_direction(GDK_KEYMAP(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdkkeymap_methods[] = {
  {"get_direction",(nsp_method *) _wrap_gdk_keymap_get_direction},
  { NULL, NULL}
};

static NspMethods *gdkkeymap_get_methods(void) { return gdkkeymap_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkkeymap_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkScreen ----------- */


#define  NspGdkScreen_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkscreen.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkScreen inherits from GObject 
 */

int nsp_type_gdkscreen_id=0;
NspTypeGdkScreen *nsp_type_gdkscreen=NULL;

/*
 * Type object for NspGdkScreen 
 * all the instance of NspTypeGdkScreen share the same id. 
 * nsp_type_gdkscreen: is an instance of NspTypeGdkScreen 
 *    used for objects of NspGdkScreen type (i.e built with new_gdkscreen) 
 * other instances are used for derived classes 
 */
NspTypeGdkScreen *new_type_gdkscreen(type_mode mode)
{
  NspTypeGdkScreen *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkscreen != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkscreen;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkscreen_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkscreen_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkscreen;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkscreen */ 

  top->s_type =  (s_type_func *) nsp_gdkscreen_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkscreen_type_short_string;
  /* top->create = (create_func*) int_gdkscreen_create;*/

  /* specific methods for gdkscreen */

  type->init = (init_func *) init_gdkscreen;

  /* 
   * NspGdkScreen interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkscreen_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkScreen called nsp_type_gdkscreen
       */
      type->id =  nsp_type_gdkscreen_id = nsp_new_type_id();
      nsp_type_gdkscreen = type;
      if ( nsp_register_type(nsp_type_gdkscreen) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkscreen, GDK_TYPE_SCREEN);
      return ( mode == T_BASE ) ? type : new_type_gdkscreen(mode);
    }
  else 
    {
      type->id = nsp_type_gdkscreen_id;
      return type;
    }
}

/*
 * initialize NspGdkScreen instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkscreen(NspGdkScreen *Obj,NspTypeGdkScreen *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkScreen 
 */

NspGdkScreen *new_gdkscreen() 
{
  NspGdkScreen *loc;
  /* type must exists */
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkScreen)))== NULLGDKSCREEN) return loc;
  /* initialize object */
  if ( init_gdkscreen(loc,nsp_type_gdkscreen) == FAIL) return NULLGDKSCREEN;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkScreen 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkscreen_type_name[]="GdkScreen";
static char gdkscreen_short_type_name[]="GdkScreen";

static char *nsp_gdkscreen_type_as_string(void)
{
  return(gdkscreen_type_name);
}

static char *nsp_gdkscreen_type_short_string(NspObject *v)
{
  return(gdkscreen_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkScreen objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkScreen   *nsp_gdkscreen_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkscreen_id)  == TRUE  ) return ((NspGdkScreen *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkscreen));
  return NULL;
}

int IsGdkScreenObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkscreen_id);
}

int IsGdkScreen(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkscreen_id);
}

NspGdkScreen  *GetGdkScreenCopy(Stack stack, int i)
{
  if (  GetGdkScreen(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkScreen  *GetGdkScreen(Stack stack, int i)
{
  NspGdkScreen *M;
  if (( M = nsp_gdkscreen_object(NthObj(i))) == NULLGDKSCREEN)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkScreen *gdkscreen_copy(NspGdkScreen *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkscreen);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkscreen);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkScreen
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_screen_get_system_visual(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkVisual *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_system_visual(GDK_SCREEN(self->obj));
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_rgba_visual(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkVisual *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_rgba_visual(GDK_SCREEN(self->obj));
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_is_composited(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_is_composited(GDK_SCREEN(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_root_window(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_root_window(GDK_SCREEN(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_display(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_display(GDK_SCREEN(self->obj));
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_number(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_number(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_width(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_width(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_height(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_height(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_width_mm(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_width_mm(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_height_mm(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_height_mm(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_list_visuals(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_screen_list_visuals(GDK_SCREEN(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_gdk_screen_get_toplevel_windows(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_screen_get_toplevel_windows(GDK_SCREEN(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_gdk_screen_make_display_name(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gdk_screen_make_display_name(GDK_SCREEN(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gdk_screen_get_n_monitors(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_n_monitors(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_primary_monitor(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_primary_monitor(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_monitor_geometry(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj, t_end};
  int monitor_num;
  GdkRectangle dest = { 0, 0, 0, 0 };
  NspObject *nsp_dest;
  if ( GetArgs(stack,rhs,opt,T,&monitor_num, &nsp_dest) == FAIL) return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_dest, &dest))
      return RET_BUG;
    gdk_screen_get_monitor_geometry(GDK_SCREEN(self->obj),monitor_num,&dest);
  return 0;
}

#if GTK_CHECK_VERSION(3,4,0)
static int _wrap_gdk_screen_get_monitor_workarea(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj, t_end};
  int monitor_num;
  GdkRectangle dest = { 0, 0, 0, 0 };
  NspObject *nsp_dest;
  if ( GetArgs(stack,rhs,opt,T,&monitor_num, &nsp_dest) == FAIL) return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_dest, &dest))
      return RET_BUG;
    gdk_screen_get_monitor_workarea(GDK_SCREEN(self->obj),monitor_num,&dest);
  return 0;
}

#else
int _wrap_gdk_screen_get_monitor_workarea(Stack stack, int rhs, int opt, int lhs) /* get_monitor_workarea */
{
  Scierror("Error: function gdk_screen_get_monitor_workarea not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_screen_get_monitor_at_point(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int x, y, ret;
  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
    ret =gdk_screen_get_monitor_at_point(GDK_SCREEN(self->obj),x,y);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_monitor_at_window(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *window;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window) == FAIL) return RET_BUG;
    ret =gdk_screen_get_monitor_at_window(GDK_SCREEN(self->obj),GDK_WINDOW(window->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_monitor_width_mm(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int monitor_num, ret;
  if ( GetArgs(stack,rhs,opt,T,&monitor_num) == FAIL) return RET_BUG;
    ret =gdk_screen_get_monitor_width_mm(GDK_SCREEN(self->obj),monitor_num);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_monitor_height_mm(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int monitor_num, ret;
  if ( GetArgs(stack,rhs,opt,T,&monitor_num) == FAIL) return RET_BUG;
    ret =gdk_screen_get_monitor_height_mm(GDK_SCREEN(self->obj),monitor_num);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_monitor_plug_name(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int monitor_num;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&monitor_num) == FAIL) return RET_BUG;
    ret =gdk_screen_get_monitor_plug_name(GDK_SCREEN(self->obj),monitor_num);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

#if GTK_CHECK_VERSION(3,10,0)
static int _wrap_gdk_screen_get_monitor_scale_factor(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int monitor_num, ret;
  if ( GetArgs(stack,rhs,opt,T,&monitor_num) == FAIL) return RET_BUG;
    ret =gdk_screen_get_monitor_scale_factor(GDK_SCREEN(self->obj),monitor_num);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_gdk_screen_get_monitor_scale_factor(Stack stack, int rhs, int opt, int lhs) /* get_monitor_scale_factor */
{
  Scierror("Error: function gdk_screen_get_monitor_scale_factor not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_screen_set_resolution(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_double, t_end};
  double dpi;
  if ( GetArgs(stack,rhs,opt,T,&dpi) == FAIL) return RET_BUG;
    gdk_screen_set_resolution(GDK_SCREEN(self->obj),dpi);
  return 0;
}

static int _wrap_gdk_screen_get_resolution(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  double ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_resolution(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_active_window(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_active_window(GDK_SCREEN(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_window_stack(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_screen_get_window_stack(GDK_SCREEN(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static NspMethods gdkscreen_methods[] = {
  {"get_system_visual",(nsp_method *) _wrap_gdk_screen_get_system_visual},
  {"get_rgba_visual",(nsp_method *) _wrap_gdk_screen_get_rgba_visual},
  {"is_composited",(nsp_method *) _wrap_gdk_screen_is_composited},
  {"get_root_window",(nsp_method *) _wrap_gdk_screen_get_root_window},
  {"get_display",(nsp_method *) _wrap_gdk_screen_get_display},
  {"get_number",(nsp_method *) _wrap_gdk_screen_get_number},
  {"get_width",(nsp_method *) _wrap_gdk_screen_get_width},
  {"get_height",(nsp_method *) _wrap_gdk_screen_get_height},
  {"get_width_mm",(nsp_method *) _wrap_gdk_screen_get_width_mm},
  {"get_height_mm",(nsp_method *) _wrap_gdk_screen_get_height_mm},
  {"list_visuals",(nsp_method *) _wrap_gdk_screen_list_visuals},
  {"get_toplevel_windows",(nsp_method *) _wrap_gdk_screen_get_toplevel_windows},
  {"make_display_name",(nsp_method *) _wrap_gdk_screen_make_display_name},
  {"get_n_monitors",(nsp_method *) _wrap_gdk_screen_get_n_monitors},
  {"get_primary_monitor",(nsp_method *) _wrap_gdk_screen_get_primary_monitor},
  {"get_monitor_geometry",(nsp_method *) _wrap_gdk_screen_get_monitor_geometry},
  {"get_monitor_workarea",(nsp_method *) _wrap_gdk_screen_get_monitor_workarea},
  {"get_monitor_at_point",(nsp_method *) _wrap_gdk_screen_get_monitor_at_point},
  {"get_monitor_at_window",(nsp_method *) _wrap_gdk_screen_get_monitor_at_window},
  {"get_monitor_width_mm",(nsp_method *) _wrap_gdk_screen_get_monitor_width_mm},
  {"get_monitor_height_mm",(nsp_method *) _wrap_gdk_screen_get_monitor_height_mm},
  {"get_monitor_plug_name",(nsp_method *) _wrap_gdk_screen_get_monitor_plug_name},
  {"get_monitor_scale_factor",(nsp_method *) _wrap_gdk_screen_get_monitor_scale_factor},
  {"set_resolution",(nsp_method *) _wrap_gdk_screen_set_resolution},
  {"get_resolution",(nsp_method *) _wrap_gdk_screen_get_resolution},
  {"get_active_window",(nsp_method *) _wrap_gdk_screen_get_active_window},
  {"get_window_stack",(nsp_method *) _wrap_gdk_screen_get_window_stack},
  { NULL, NULL}
};

static NspMethods *gdkscreen_get_methods(void) { return gdkscreen_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkscreen_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkVisual ----------- */


#define  NspGdkVisual_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkvisual.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkVisual inherits from GObject 
 */

int nsp_type_gdkvisual_id=0;
NspTypeGdkVisual *nsp_type_gdkvisual=NULL;

/*
 * Type object for NspGdkVisual 
 * all the instance of NspTypeGdkVisual share the same id. 
 * nsp_type_gdkvisual: is an instance of NspTypeGdkVisual 
 *    used for objects of NspGdkVisual type (i.e built with new_gdkvisual) 
 * other instances are used for derived classes 
 */
NspTypeGdkVisual *new_type_gdkvisual(type_mode mode)
{
  NspTypeGdkVisual *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkvisual != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkvisual;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkvisual_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkvisual_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkvisual;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkvisual */ 

  top->s_type =  (s_type_func *) nsp_gdkvisual_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkvisual_type_short_string;
  /* top->create = (create_func*) int_gdkvisual_create;*/

  /* specific methods for gdkvisual */

  type->init = (init_func *) init_gdkvisual;

  /* 
   * NspGdkVisual interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkvisual_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkVisual called nsp_type_gdkvisual
       */
      type->id =  nsp_type_gdkvisual_id = nsp_new_type_id();
      nsp_type_gdkvisual = type;
      if ( nsp_register_type(nsp_type_gdkvisual) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkvisual, GDK_TYPE_VISUAL);
      return ( mode == T_BASE ) ? type : new_type_gdkvisual(mode);
    }
  else 
    {
      type->id = nsp_type_gdkvisual_id;
      return type;
    }
}

/*
 * initialize NspGdkVisual instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkvisual(NspGdkVisual *Obj,NspTypeGdkVisual *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkVisual 
 */

NspGdkVisual *new_gdkvisual() 
{
  NspGdkVisual *loc;
  /* type must exists */
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkVisual)))== NULLGDKVISUAL) return loc;
  /* initialize object */
  if ( init_gdkvisual(loc,nsp_type_gdkvisual) == FAIL) return NULLGDKVISUAL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkVisual 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkvisual_type_name[]="GdkVisual";
static char gdkvisual_short_type_name[]="GdkVisual";

static char *nsp_gdkvisual_type_as_string(void)
{
  return(gdkvisual_type_name);
}

static char *nsp_gdkvisual_type_short_string(NspObject *v)
{
  return(gdkvisual_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkVisual objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkVisual   *nsp_gdkvisual_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkvisual_id)  == TRUE  ) return ((NspGdkVisual *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkvisual));
  return NULL;
}

int IsGdkVisualObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkvisual_id);
}

int IsGdkVisual(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkvisual_id);
}

NspGdkVisual  *GetGdkVisualCopy(Stack stack, int i)
{
  if (  GetGdkVisual(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkVisual  *GetGdkVisual(Stack stack, int i)
{
  NspGdkVisual *M;
  if (( M = nsp_gdkvisual_object(NthObj(i))) == NULLGDKVISUAL)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkVisual *gdkvisual_copy(NspGdkVisual *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkvisual);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkvisual);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkVisual
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 2319 "codegen-3.0/gdk.override"
static const char *_visual[]={ "best_depth", "best_type", "get_system", NULL };
static int
_wrap_gdk_visual_new(Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  if ( rhs - opt == 1)
    {
      int ret_s,rep;
      if ((rep= GetStringInArray(stack,1,_visual,1)) == -1) return RET_BUG;
      switch (rep )
	{
	case 0:
	  ret_s = gdk_visual_get_best_depth();
	  if ( nsp_move_double(stack,1,(double) ret_s)==FAIL) return RET_BUG;
	  return 1;
	case 1:
	  ret_s = gdk_visual_get_best_type();
	  if ( nsp_move_double(stack,1,(double) ret_s)==FAIL) return RET_BUG;
	  return 1;
	case 2:
	  ret = (GObject *)  gdk_visual_get_system();
	  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
	  if ((nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL)
	    return RET_BUG;
	  MoveObj(stack,1,nsp_ret);
	  return 1;
	}
    }
  else if ( rhs -opt == 0)
    {
      int depth = -1;
      int type = -1;
      /* 	 int_types T[] = { new_opts, t_end}; */
      nsp_option opts[] ={{ "depth",s_int,NULLOBJ,-1},
			  { "type",s_int,NULLOBJ,-1},
			  { NULL,t_end,NULLOBJ,-1}};
      if ( depth == -1 )
	{
	  if ( type == -1 )
	    ret = (GObject *) gdk_visual_get_best();
	  else
	    {
	      gint visual_type;
	      if (nspg_enum_get_value(GDK_TYPE_VISUAL_TYPE, opts[1].obj, &visual_type)== FAIL)
		return RET_BUG;
	      ret =(GObject *)  gdk_visual_get_best_with_type(visual_type);
	    }
	}
      else
	{
	  if ( type == -1 )
	    ret =(GObject *)  gdk_visual_get_best_with_depth(depth);
	  else
	    {
	      gint visual_type;
	      if (nspg_enum_get_value(GDK_TYPE_VISUAL_TYPE, opts[1].obj,&visual_type)== FAIL)
		return RET_BUG;
	      ret =(GObject *) gdk_visual_get_best_with_both(depth, visual_type);
	    }
	}
      if ( ret == NULL)
	{
	  Scierror("Error: Unable to create visual \n");
	  return RET_BUG;
	}
      nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
      nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkvisual );
      if ( nsp_ret == NULL) return RET_BUG;
      MoveObj(stack,1,nsp_ret);
      return 1;
    }
  else
    {
      Scierror("Usage: one string argument or two optionals depth and type\n");
      return RET_BUG;
    }
  return RET_BUG;
}
#line 7697 "gdk.c"


static int _wrap_gdk_visual_get_screen(NspGdkVisual *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkScreen *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_visual_get_screen(GDK_VISUAL(self->obj));
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_visual_get_visual_type(NspGdkVisual *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_visual_get_visual_type(GDK_VISUAL(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_visual_get_depth(NspGdkVisual *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_visual_get_depth(GDK_VISUAL(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_visual_get_byte_order(NspGdkVisual *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_visual_get_byte_order(GDK_VISUAL(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_visual_get_colormap_size(NspGdkVisual *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_visual_get_colormap_size(GDK_VISUAL(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_visual_get_bits_per_rgb(NspGdkVisual *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_visual_get_bits_per_rgb(GDK_VISUAL(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdkvisual_methods[] = {
  {"get_screen",(nsp_method *) _wrap_gdk_visual_get_screen},
  {"get_visual_type",(nsp_method *) _wrap_gdk_visual_get_visual_type},
  {"get_depth",(nsp_method *) _wrap_gdk_visual_get_depth},
  {"get_byte_order",(nsp_method *) _wrap_gdk_visual_get_byte_order},
  {"get_colormap_size",(nsp_method *) _wrap_gdk_visual_get_colormap_size},
  {"get_bits_per_rgb",(nsp_method *) _wrap_gdk_visual_get_bits_per_rgb},
  { NULL, NULL}
};

static NspMethods *gdkvisual_get_methods(void) { return gdkvisual_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkvisual_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkCursor ----------- */


#define  NspGdkCursor_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkcursor.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkCursor inherits from GObject 
 */

int nsp_type_gdkcursor_id=0;
NspTypeGdkCursor *nsp_type_gdkcursor=NULL;

/*
 * Type object for NspGdkCursor 
 * all the instance of NspTypeGdkCursor share the same id. 
 * nsp_type_gdkcursor: is an instance of NspTypeGdkCursor 
 *    used for objects of NspGdkCursor type (i.e built with new_gdkcursor) 
 * other instances are used for derived classes 
 */
NspTypeGdkCursor *new_type_gdkcursor(type_mode mode)
{
  NspTypeGdkCursor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkcursor != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkcursor;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkcursor_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkcursor_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkcursor;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkcursor */ 

  top->s_type =  (s_type_func *) nsp_gdkcursor_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkcursor_type_short_string;
  /* top->create = (create_func*) int_gdkcursor_create;*/

  /* specific methods for gdkcursor */

  type->init = (init_func *) init_gdkcursor;

  /* 
   * NspGdkCursor interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkcursor_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkCursor called nsp_type_gdkcursor
       */
      type->id =  nsp_type_gdkcursor_id = nsp_new_type_id();
      nsp_type_gdkcursor = type;
      if ( nsp_register_type(nsp_type_gdkcursor) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkcursor, GDK_TYPE_CURSOR);
      return ( mode == T_BASE ) ? type : new_type_gdkcursor(mode);
    }
  else 
    {
      type->id = nsp_type_gdkcursor_id;
      return type;
    }
}

/*
 * initialize NspGdkCursor instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkcursor(NspGdkCursor *Obj,NspTypeGdkCursor *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkCursor 
 */

NspGdkCursor *new_gdkcursor() 
{
  NspGdkCursor *loc;
  /* type must exists */
  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkCursor)))== NULLGDKCURSOR) return loc;
  /* initialize object */
  if ( init_gdkcursor(loc,nsp_type_gdkcursor) == FAIL) return NULLGDKCURSOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkCursor 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkcursor_type_name[]="GdkCursor";
static char gdkcursor_short_type_name[]="GdkCursor";

static char *nsp_gdkcursor_type_as_string(void)
{
  return(gdkcursor_type_name);
}

static char *nsp_gdkcursor_type_short_string(NspObject *v)
{
  return(gdkcursor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkCursor objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkCursor   *nsp_gdkcursor_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkcursor_id)  == TRUE  ) return ((NspGdkCursor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkcursor));
  return NULL;
}

int IsGdkCursorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkcursor_id);
}

int IsGdkCursor(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkcursor_id);
}

NspGdkCursor  *GetGdkCursorCopy(Stack stack, int i)
{
  if (  GetGdkCursor(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkCursor  *GetGdkCursor(Stack stack, int i)
{
  NspGdkCursor *M;
  if (( M = nsp_gdkcursor_object(NthObj(i))) == NULLGDKCURSOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkCursor *gdkcursor_copy(NspGdkCursor *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkcursor);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkcursor);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkCursor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 655 "codegen-3.0/gdk.override"

/* changed not to perform a copy */
static int
_wrap_gdk_cursor_new_from_name(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,string, t_end};
  NspGObject *display;
  char *name;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdisplay, &display, &name) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_cursor_new_from_name(GDK_DISPLAY(display->obj),name))== NULL) return RET_BUG;

  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_CURSOR, ret,FALSE,TRUE,(NspTypeBase *) nsp_type_gdkcursor);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 7986 "gdk.c"


static int
_wrap_gdk_cursor_new_from_surface (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj,s_double,s_double, t_end};
  NspGObject *display;
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL;
  double x, y;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdisplay, &display, &nsp_surface, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
  if ((ret = (GObject *)gdk_cursor_new_from_surface(GDK_DISPLAY(display->obj),surface,x,y))== NULL) return RET_BUG;

  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcursor);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_cursor_new_from_pixbuf (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check,s_int,s_int, t_end};
  NspGObject *display, *pixbuf;
  int x, y;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdisplay, &display, &nsp_type_gdkpixbuf, &pixbuf, &x, &y) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_cursor_new_from_pixbuf(GDK_DISPLAY(display->obj),GDK_PIXBUF(pixbuf->obj),x,y))== NULL) return RET_BUG;

  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcursor);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 696 "codegen-3.0/gdk.override"

static int
_wrap_gdk_cursor_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, t_end};
  GdkCursorType cursor_type;
  NspObject *nsp_cursor_type = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cursor_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_CURSOR_TYPE, nsp_cursor_type, &cursor_type)== FAIL)
      return RET_BUG;
  if ((ret = (GObject *)gdk_cursor_new(cursor_type))== NULL) return RET_BUG;

  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_CURSOR, ret,FALSE,TRUE,(NspTypeBase *) nsp_type_gdkcursor);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 8052 "gdk.c"


#line 718 "codegen-3.0/gdk.override"

static int
_wrap_gdk_cursor_new_for_display (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *display;
  GdkCursorType cursor_type;
  NspObject *nsp_cursor_type = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdisplay, &display, &nsp_cursor_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_CURSOR_TYPE, nsp_cursor_type, &cursor_type)== FAIL)
      return RET_BUG;
  if ((ret = (GObject *)gdk_cursor_new_for_display(GDK_DISPLAY(display->obj),cursor_type))== NULL) return RET_BUG;

  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_CURSOR, ret,FALSE,TRUE,(NspTypeBase *) nsp_type_gdkcursor);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 8077 "gdk.c"


static int _wrap_gdk_cursor_get_display(NspGdkCursor *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_cursor_get_display(GDK_CURSOR(self->obj));
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_cursor_get_image(NspGdkCursor *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_cursor_get_image(GDK_CURSOR(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(3,10,0)
static int _wrap_gdk_cursor_get_surface(NspGdkCursor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_double,s_double, t_end};
  double x_hot, y_hot;
  cairo_surface_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&x_hot, &y_hot) == FAIL) return RET_BUG;
    ret =gdk_cursor_get_surface(GDK_CURSOR(self->obj),&x_hot,&y_hot);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_cursor_get_surface(Stack stack, int rhs, int opt, int lhs) /* get_surface */
{
  Scierror("Error: function gdk_cursor_get_surface not available\n");
  return RET_BUG;
}
#endif
static int _wrap_gdk_cursor_get_cursor_type(NspGdkCursor *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_cursor_get_cursor_type(GDK_CURSOR(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdkcursor_methods[] = {
  {"get_display",(nsp_method *) _wrap_gdk_cursor_get_display},
  {"get_image",(nsp_method *) _wrap_gdk_cursor_get_image},
  {"get_surface",(nsp_method *) _wrap_gdk_cursor_get_surface},
  {"get_cursor_type",(nsp_method *) _wrap_gdk_cursor_get_cursor_type},
  { NULL, NULL}
};

static NspMethods *gdkcursor_get_methods(void) { return gdkcursor_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkcursor_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkPixbuf ----------- */


#define  NspGdkPixbuf_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkpixbuf.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkPixbuf inherits from GObject 
 */

int nsp_type_gdkpixbuf_id=0;
NspTypeGdkPixbuf *nsp_type_gdkpixbuf=NULL;

/*
 * Type object for NspGdkPixbuf 
 * all the instance of NspTypeGdkPixbuf share the same id. 
 * nsp_type_gdkpixbuf: is an instance of NspTypeGdkPixbuf 
 *    used for objects of NspGdkPixbuf type (i.e built with new_gdkpixbuf) 
 * other instances are used for derived classes 
 */
NspTypeGdkPixbuf *new_type_gdkpixbuf(type_mode mode)
{
  NspTypeGdkPixbuf *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixbuf != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixbuf;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixbuf_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkpixbuf_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkpixbuf;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkpixbuf */ 

  top->s_type =  (s_type_func *) nsp_gdkpixbuf_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkpixbuf_type_short_string;
  /* top->create = (create_func*) int_gdkpixbuf_create;*/

  /* specific methods for gdkpixbuf */

  type->init = (init_func *) init_gdkpixbuf;

  /* 
   * NspGdkPixbuf interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkpixbuf_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkPixbuf called nsp_type_gdkpixbuf
       */
      type->id =  nsp_type_gdkpixbuf_id = nsp_new_type_id();
      nsp_type_gdkpixbuf = type;
      if ( nsp_register_type(nsp_type_gdkpixbuf) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkpixbuf, GDK_TYPE_PIXBUF);
      return ( mode == T_BASE ) ? type : new_type_gdkpixbuf(mode);
    }
  else 
    {
      type->id = nsp_type_gdkpixbuf_id;
      return type;
    }
}

/*
 * initialize NspGdkPixbuf instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixbuf(NspGdkPixbuf *Obj,NspTypeGdkPixbuf *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkPixbuf 
 */

NspGdkPixbuf *new_gdkpixbuf() 
{
  NspGdkPixbuf *loc;
  /* type must exists */
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkPixbuf)))== NULLGDKPIXBUF) return loc;
  /* initialize object */
  if ( init_gdkpixbuf(loc,nsp_type_gdkpixbuf) == FAIL) return NULLGDKPIXBUF;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkPixbuf 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkpixbuf_type_name[]="GdkPixbuf";
static char gdkpixbuf_short_type_name[]="GdkPixbuf";

static char *nsp_gdkpixbuf_type_as_string(void)
{
  return(gdkpixbuf_type_name);
}

static char *nsp_gdkpixbuf_type_short_string(NspObject *v)
{
  return(gdkpixbuf_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkPixbuf objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkPixbuf   *nsp_gdkpixbuf_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkpixbuf_id)  == TRUE  ) return ((NspGdkPixbuf *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixbuf));
  return NULL;
}

int IsGdkPixbufObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkpixbuf_id);
}

int IsGdkPixbuf(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkpixbuf_id);
}

NspGdkPixbuf  *GetGdkPixbufCopy(Stack stack, int i)
{
  if (  GetGdkPixbuf(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkPixbuf  *GetGdkPixbuf(Stack stack, int i)
{
  NspGdkPixbuf *M;
  if (( M = nsp_gdkpixbuf_object(NthObj(i))) == NULLGDKPIXBUF)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkPixbuf *gdkpixbuf_copy(NspGdkPixbuf *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixbuf);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixbuf);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkPixbuf
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 2423 "codegen-3.0/gdk.override"
static int
_wrap_gdk_pixbuf_new_from_xpm_data(Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *data;
  NspObject *ret;
  GdkPixbuf *pixbuf;

  CheckRhs(1,1);
  if (( data = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (( pixbuf = gdk_pixbuf_new_from_xpm_data((const char**)data->S)) == NULL)
    {
      Scierror("%s:can't load pixbuf\n",NspFname(stack));
      return RET_BUG;
    }

  if(( ret = (NspObject *) gobject_create(NVOID,(GObject *)pixbuf, (NspTypeBase *) nsp_type_gdkpixbuf)) == NULL) return RET_BUG;
  /* g_object_unref(pixbuf); XXXX */
  MoveObj(stack,1,ret);
  return 1;
}

#line 8365 "gdk.c"


static int
_wrap_gdk_pixbuf_new_from_resource_at_scale (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,s_int,s_int,s_bool, t_end};
  char *resource_path;
  int width, height, preserve_aspect_ratio;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&resource_path, &width, &height, &preserve_aspect_ratio) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_new_from_resource_at_scale(resource_path,width,height,preserve_aspect_ratio,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_new_from_resource (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *resource_path;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&resource_path) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_new_from_resource(resource_path,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_new_from_file_at_scale (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,s_int,s_int,s_bool, t_end};
  char *filename;
  int width, height, preserve_aspect_ratio;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename, &width, &height, &preserve_aspect_ratio) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_new_from_file_at_scale(filename,width,height,preserve_aspect_ratio,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_new_from_file_at_size (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,s_int,s_int, t_end};
  char *filename;
  int width, height;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename, &width, &height) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_new_from_file_at_size(filename,width,height,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_new_from_file (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *filename;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_new_from_file(filename,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_new_subpixbuf (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,s_int,s_int,s_int,s_int, t_end};
  NspGObject *src_pixbuf;
  int src_x, src_y, width, height;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &src_pixbuf, &src_x, &src_y, &width, &height) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_new_subpixbuf(GDK_PIXBUF(src_pixbuf->obj),src_x,src_y,width,height))== NULL) return RET_BUG;

  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj,s_bool,s_int,s_int,s_int, t_end};
  GdkColorspace colorspace;
  NspObject *nsp_colorspace = NULL;
  int has_alpha, bits_per_sample, width, height;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_colorspace, &has_alpha, &bits_per_sample, &width, &height) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_COLORSPACE, nsp_colorspace, &colorspace)== FAIL)
      return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_new(colorspace,has_alpha,bits_per_sample,width,height))== NULL) return RET_BUG;

  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_get_colorspace(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_colorspace(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_n_channels(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_n_channels(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_has_alpha(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_has_alpha(GDK_PIXBUF(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_bits_per_sample(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_bits_per_sample(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 2041 "codegen-3.0/gdk.override"
static int
_wrap_gdk_pixbuf_get_pixels(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *pixbuf;
  guchar *pixels;
  gint rowstride, height;
  NspObject *ret;

  pixbuf = GDK_PIXBUF(self->obj);
  pixels = gdk_pixbuf_get_pixels(pixbuf);
  rowstride = gdk_pixbuf_get_rowstride(pixbuf);
  height = gdk_pixbuf_get_height(pixbuf);

  if (pixels == NULL) {
    Scierror("could not get pixel data\n");
    return RET_BUG;
  }
  if ((ret = nsp_new_string_obj(NVOID,(const char *) pixels, rowstride*height)) == NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 8571 "gdk.c"


static int _wrap_gdk_pixbuf_get_width(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_width(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_height(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_height(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_rowstride(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_rowstride(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_byte_length(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_byte_length(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_copy(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_copy(GDK_PIXBUF(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_fill(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  gulong pixel;
  if ( GetArgs(stack,rhs,opt,T,&pixel) == FAIL) return RET_BUG;
    gdk_pixbuf_fill(GDK_PIXBUF(self->obj),pixel);
  return 0;
}

#line 2064 "codegen-3.0/gdk.override"

static NspSMatrix *writable_formats() ;

static int
_wrap_gdk_pixbuf_save(NspGObject *self, Stack stack,int rhs,int opt,int lhs)

{
  const char *opt_key;
  char *filename,*type,*opt_value;
  int i,len;
  char **option_keys,**option_values;
  GError *error = NULL;
  /* static char *kwlist[] = {"filename", "type", "options", NULL};*/
  /* GdkPixbuf *pixbuf = GDK_PIXBUF(self->obj); */
  CheckStdRhs(2,2);
  CheckLhs(0,1);
  if ((filename = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((type = GetString(stack,2)) == (char*)0) return RET_BUG;
  len = opt;
  option_keys = g_new(gchar *, len + 1);
  option_values = g_new(gchar *, len + 1);
  for ( i = 3 ; i <= rhs ; i++)
    {
      NspObject * option;
      if ((option =nsp_get_object(stack,i)) == NULLOBJ ) return RET_BUG;
      opt_key = nsp_object_get_name(option);
      if ( IsString(option))
	{
	  opt_value = ((NspSMatrix *) option)->S[0];
	}
      else
	{
	  Scierror("%s option value is not a string !\n",opt_key);
	  g_free(option_keys);
	  g_free(option_values);
	  return RET_BUG;
	}
      if ((option_keys[i] = new_nsp_string(opt_key)) == NULLSTRING)
	{
	  g_free(option_keys);
	  g_free(option_values);
	  return RET_BUG;
	}
      if ((option_values[i] = new_nsp_string(opt_value)) == NULLSTRING)
	{
	  g_free(option_keys);
	  g_free(option_values);
	  return RET_BUG;
	}
    }
  option_keys[len] = NULL;
  option_values[len] = NULL;
  gdk_pixbuf_savev(GDK_PIXBUF(self->obj), filename, type,
		   option_keys, option_values, &error);
  /*
  if (nspg_error_check(&error)) return RET_BUG;
  */
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  g_free(option_keys);
  g_free(option_values);
  if ( lhs == 1 )
    {
      NspSMatrix *S = writable_formats();
      if ( S == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(S));
      return 1;
    }
  return 0;
}


static void add_if_writable  (gpointer data, gpointer user_data)
{
  GdkPixbufFormat *pdata=data;
  NspSMatrix *S=user_data;
  if (gdk_pixbuf_format_is_writable (pdata))
    {
      nsp_row_smatrix_append_string(S, gdk_pixbuf_format_get_name(pdata));
    }
}

static NspSMatrix *writable_formats()
{
  GSList *formats = gdk_pixbuf_get_formats ();
  NspSMatrix *S;
  if (( S = nsp_smatrix_create(NVOID,0,0,NULL,0)) == NULLSMAT ) return NULL;
  g_slist_foreach (formats, add_if_writable,S);
  g_slist_free (formats);
  return S;
}

#line 8726 "gdk.c"


static int _wrap_gdk_pixbuf_savev(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,obj,obj, t_end};
  char *filename, *type;
  gchar **option_keys = NULL, **option_values = NULL;
  NspObject *nsp_option_keys = NULL, *nsp_option_values = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&filename, &type, &nsp_option_keys, &nsp_option_values) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_option_keys))
    { option_keys =  ((NspSMatrix *) nsp_option_keys)->S;}
  else
    {
      Scierror("Error: option_keys should be of type SMat\n");
      return RET_BUG;
    }
  if ( IsSMat(nsp_option_values))
    { option_values =  ((NspSMatrix *) nsp_option_values)->S;}
  else
    {
      Scierror("Error: option_values should be of type SMat\n");
      return RET_BUG;
    }
    ret =gdk_pixbuf_savev(GDK_PIXBUF(self->obj),filename,type,option_keys,option_values,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_add_alpha(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,s_int,s_int,s_int, t_end};
  int substitute_color, r, g, b;
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&substitute_color, &r, &g, &b) == FAIL) return RET_BUG;
    ret =gdk_pixbuf_add_alpha(GDK_PIXBUF(self->obj),substitute_color,r,g,b);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_copy_area(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int,s_int,obj_check,s_int,s_int, t_end};
  int src_x, src_y, width, height, dest_x, dest_y;
  NspGObject *dest_pixbuf;
  if ( GetArgs(stack,rhs,opt,T,&src_x, &src_y, &width, &height, &nsp_type_gdkpixbuf, &dest_pixbuf, &dest_x, &dest_y) == FAIL) return RET_BUG;
    gdk_pixbuf_copy_area(GDK_PIXBUF(self->obj),src_x,src_y,width,height,GDK_PIXBUF(dest_pixbuf->obj),dest_x,dest_y);
  return 0;
}

static int _wrap_gdk_pixbuf_saturate_and_pixelate(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_double,s_bool, t_end};
  NspGObject *dest;
  double saturation;
  int pixelate;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &dest, &saturation, &pixelate) == FAIL) return RET_BUG;
    gdk_pixbuf_saturate_and_pixelate(GDK_PIXBUF(self->obj),GDK_PIXBUF(dest->obj),saturation,pixelate);
  return 0;
}

static int _wrap_gdk_pixbuf_apply_embedded_orientation(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_apply_embedded_orientation(GDK_PIXBUF(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_get_option(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =gdk_pixbuf_get_option(GDK_PIXBUF(self->obj),key);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_scale(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int,s_int,s_int,s_int,s_double,s_double,s_double,s_double,obj, t_end};
  NspGObject *dest;
  int dest_x, dest_y, dest_width, dest_height;
  double offset_x, offset_y, scale_x, scale_y;
  GdkInterpType interp_type;
  NspObject *nsp_interp_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &dest, &dest_x, &dest_y, &dest_width, &dest_height, &offset_x, &offset_y, &scale_x, &scale_y, &nsp_interp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, &interp_type)== FAIL)
      return RET_BUG;
    gdk_pixbuf_scale(GDK_PIXBUF(self->obj),GDK_PIXBUF(dest->obj),dest_x,dest_y,dest_width,dest_height,offset_x,offset_y,scale_x,scale_y,interp_type);
  return 0;
}

static int _wrap_gdk_pixbuf_composite(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int,s_int,s_int,s_int,s_double,s_double,s_double,s_double,obj,s_int, t_end};
  NspGObject *dest;
  int dest_x, dest_y, dest_width, dest_height, overall_alpha;
  double offset_x, offset_y, scale_x, scale_y;
  GdkInterpType interp_type;
  NspObject *nsp_interp_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &dest, &dest_x, &dest_y, &dest_width, &dest_height, &offset_x, &offset_y, &scale_x, &scale_y, &nsp_interp_type, &overall_alpha) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, &interp_type)== FAIL)
      return RET_BUG;
    gdk_pixbuf_composite(GDK_PIXBUF(self->obj),GDK_PIXBUF(dest->obj),dest_x,dest_y,dest_width,dest_height,offset_x,offset_y,scale_x,scale_y,interp_type,overall_alpha);
  return 0;
}

static int _wrap_gdk_pixbuf_composite_color(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int,s_int,s_int,s_int,s_double,s_double,s_double,s_double,obj,s_int,s_int,s_int,s_int,s_int,s_int, t_end};
  NspGObject *dest;
  int dest_x, dest_y, dest_width, dest_height, overall_alpha, check_x, check_y, check_size;
  double offset_x, offset_y, scale_x, scale_y;
  GdkInterpType interp_type;
  NspObject *nsp_interp_type = NULL;
  gulong color1, color2;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &dest, &dest_x, &dest_y, &dest_width, &dest_height, &offset_x, &offset_y, &scale_x, &scale_y, &nsp_interp_type, &overall_alpha, &check_x, &check_y, &check_size, &color1, &color2) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, &interp_type)== FAIL)
      return RET_BUG;
    gdk_pixbuf_composite_color(GDK_PIXBUF(self->obj),GDK_PIXBUF(dest->obj),dest_x,dest_y,dest_width,dest_height,offset_x,offset_y,scale_x,scale_y,interp_type,overall_alpha,check_x,check_y,check_size,color1,color2);
  return 0;
}

static int _wrap_gdk_pixbuf_scale_simple(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,obj, t_end};
  int dest_width, dest_height;
  GdkInterpType interp_type;
  NspObject *nsp_interp_type = NULL, *nsp_ret;
  GdkPixbuf *ret;
  if ( GetArgs(stack,rhs,opt,T,&dest_width, &dest_height, &nsp_interp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, &interp_type)== FAIL)
      return RET_BUG;
    ret =gdk_pixbuf_scale_simple(GDK_PIXBUF(self->obj),dest_width,dest_height,interp_type);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_composite_color_simple(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,obj,s_int,s_int,s_int,s_int, t_end};
  int dest_width, dest_height, overall_alpha, check_size;
  GdkInterpType interp_type;
  NspObject *nsp_interp_type = NULL, *nsp_ret;
  gulong color1, color2;
  GdkPixbuf *ret;
  if ( GetArgs(stack,rhs,opt,T,&dest_width, &dest_height, &nsp_interp_type, &overall_alpha, &check_size, &color1, &color2) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, &interp_type)== FAIL)
      return RET_BUG;
    ret =gdk_pixbuf_composite_color_simple(GDK_PIXBUF(self->obj),dest_width,dest_height,interp_type,overall_alpha,check_size,color1,color2);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_rotate_simple(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkPixbufRotation angle;
  NspObject *nsp_angle = NULL, *nsp_ret;
  GdkPixbuf *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_angle) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_PIXBUF_ROTATION, nsp_angle, &angle)== FAIL)
      return RET_BUG;
    ret =gdk_pixbuf_rotate_simple(GDK_PIXBUF(self->obj),angle);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_flip(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int horizontal;
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&horizontal) == FAIL) return RET_BUG;
    ret =gdk_pixbuf_flip(GDK_PIXBUF(self->obj),horizontal);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdkpixbuf_methods[] = {
  {"get_colorspace",(nsp_method *) _wrap_gdk_pixbuf_get_colorspace},
  {"get_n_channels",(nsp_method *) _wrap_gdk_pixbuf_get_n_channels},
  {"get_has_alpha",(nsp_method *) _wrap_gdk_pixbuf_get_has_alpha},
  {"get_bits_per_sample",(nsp_method *) _wrap_gdk_pixbuf_get_bits_per_sample},
  {"get_pixels",(nsp_method *) _wrap_gdk_pixbuf_get_pixels},
  {"get_width",(nsp_method *) _wrap_gdk_pixbuf_get_width},
  {"get_height",(nsp_method *) _wrap_gdk_pixbuf_get_height},
  {"get_rowstride",(nsp_method *) _wrap_gdk_pixbuf_get_rowstride},
  {"get_byte_length",(nsp_method *) _wrap_gdk_pixbuf_get_byte_length},
  {"copy",(nsp_method *) _wrap_gdk_pixbuf_copy},
  {"fill",(nsp_method *) _wrap_gdk_pixbuf_fill},
  {"save",(nsp_method *) _wrap_gdk_pixbuf_save},
  {"savev",(nsp_method *) _wrap_gdk_pixbuf_savev},
  {"add_alpha",(nsp_method *) _wrap_gdk_pixbuf_add_alpha},
  {"copy_area",(nsp_method *) _wrap_gdk_pixbuf_copy_area},
  {"saturate_and_pixelate",(nsp_method *) _wrap_gdk_pixbuf_saturate_and_pixelate},
  {"apply_embedded_orientation",(nsp_method *) _wrap_gdk_pixbuf_apply_embedded_orientation},
  {"get_option",(nsp_method *) _wrap_gdk_pixbuf_get_option},
  {"scale",(nsp_method *) _wrap_gdk_pixbuf_scale},
  {"composite",(nsp_method *) _wrap_gdk_pixbuf_composite},
  {"composite_color",(nsp_method *) _wrap_gdk_pixbuf_composite_color},
  {"scale_simple",(nsp_method *) _wrap_gdk_pixbuf_scale_simple},
  {"composite_color_simple",(nsp_method *) _wrap_gdk_pixbuf_composite_color_simple},
  {"rotate_simple",(nsp_method *) _wrap_gdk_pixbuf_rotate_simple},
  {"flip",(nsp_method *) _wrap_gdk_pixbuf_flip},
  { NULL, NULL}
};

static NspMethods *gdkpixbuf_get_methods(void) { return gdkpixbuf_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

#line 2021 "codegen-3.0/gdk.override"
static NspObject *
_wrap_gdk_pixbuf__get_pixel_array(NspGObject *self, char *attr)
{
  GdkPixbuf *pixbuf = GDK_PIXBUF(self->obj);
  NspMatrix  *array;
  char *data = (char *)gdk_pixbuf_get_pixels(pixbuf);
  int m,n,i;
  n = gdk_pixbuf_get_width(pixbuf)* gdk_pixbuf_get_height(pixbuf);
  if (gdk_pixbuf_get_has_alpha(pixbuf))
    m = 4;
  else
    m = 3;
  if ((array = nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return  NULL;
  for ( i = 0 ; i < m*n ; i++)  array->R[i] = data[i];

  /* array->strides[0] = gdk_pixbuf_get_rowstride(pixbuf);*/
  return (NspObject *)array;
}
#line 8983 "gdk.c"
static AttrTab gdkpixbuf_attrs[] = {
  { "pixel_array", (attr_get_function * )_wrap_gdk_pixbuf__get_pixel_array, (attr_set_function * )int_set_failed, (attr_get_object_function * )int_get_object_failed, NULL },
  { NULL,NULL,NULL,NULL,NULL },
};



/* -----------NspGdkPixbufAnimation ----------- */


#define  NspGdkPixbufAnimation_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkpixbufanimation.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkPixbufAnimation inherits from GObject 
 */

int nsp_type_gdkpixbufanimation_id=0;
NspTypeGdkPixbufAnimation *nsp_type_gdkpixbufanimation=NULL;

/*
 * Type object for NspGdkPixbufAnimation 
 * all the instance of NspTypeGdkPixbufAnimation share the same id. 
 * nsp_type_gdkpixbufanimation: is an instance of NspTypeGdkPixbufAnimation 
 *    used for objects of NspGdkPixbufAnimation type (i.e built with new_gdkpixbufanimation) 
 * other instances are used for derived classes 
 */
NspTypeGdkPixbufAnimation *new_type_gdkpixbufanimation(type_mode mode)
{
  NspTypeGdkPixbufAnimation *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixbufanimation != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixbufanimation;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixbufanimation_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkpixbufanimation_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkpixbufanimation;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkpixbufanimation */ 

  top->s_type =  (s_type_func *) nsp_gdkpixbufanimation_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkpixbufanimation_type_short_string;
  /* top->create = (create_func*) int_gdkpixbufanimation_create;*/

  /* specific methods for gdkpixbufanimation */

  type->init = (init_func *) init_gdkpixbufanimation;

  /* 
   * NspGdkPixbufAnimation interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkpixbufanimation_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkPixbufAnimation called nsp_type_gdkpixbufanimation
       */
      type->id =  nsp_type_gdkpixbufanimation_id = nsp_new_type_id();
      nsp_type_gdkpixbufanimation = type;
      if ( nsp_register_type(nsp_type_gdkpixbufanimation) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkpixbufanimation, GDK_TYPE_PIXBUF_ANIMATION);
      return ( mode == T_BASE ) ? type : new_type_gdkpixbufanimation(mode);
    }
  else 
    {
      type->id = nsp_type_gdkpixbufanimation_id;
      return type;
    }
}

/*
 * initialize NspGdkPixbufAnimation instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixbufanimation(NspGdkPixbufAnimation *Obj,NspTypeGdkPixbufAnimation *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkPixbufAnimation 
 */

NspGdkPixbufAnimation *new_gdkpixbufanimation() 
{
  NspGdkPixbufAnimation *loc;
  /* type must exists */
  nsp_type_gdkpixbufanimation = new_type_gdkpixbufanimation(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkPixbufAnimation)))== NULLGDKPIXBUFANIMATION) return loc;
  /* initialize object */
  if ( init_gdkpixbufanimation(loc,nsp_type_gdkpixbufanimation) == FAIL) return NULLGDKPIXBUFANIMATION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkPixbufAnimation 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkpixbufanimation_type_name[]="GdkPixbufAnimation";
static char gdkpixbufanimation_short_type_name[]="GdkPixbufAnimation";

static char *nsp_gdkpixbufanimation_type_as_string(void)
{
  return(gdkpixbufanimation_type_name);
}

static char *nsp_gdkpixbufanimation_type_short_string(NspObject *v)
{
  return(gdkpixbufanimation_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkPixbufAnimation objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkPixbufAnimation   *nsp_gdkpixbufanimation_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkpixbufanimation_id)  == TRUE  ) return ((NspGdkPixbufAnimation *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixbufanimation));
  return NULL;
}

int IsGdkPixbufAnimationObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkpixbufanimation_id);
}

int IsGdkPixbufAnimation(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkpixbufanimation_id);
}

NspGdkPixbufAnimation  *GetGdkPixbufAnimationCopy(Stack stack, int i)
{
  if (  GetGdkPixbufAnimation(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkPixbufAnimation  *GetGdkPixbufAnimation(Stack stack, int i)
{
  NspGdkPixbufAnimation *M;
  if (( M = nsp_gdkpixbufanimation_object(NthObj(i))) == NULLGDKPIXBUFANIMATION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkPixbufAnimation *gdkpixbufanimation_copy(NspGdkPixbufAnimation *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixbufanimation);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixbufanimation);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkPixbufAnimation
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gdk_pixbuf_animation_new_from_resource (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *resource_path;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&resource_path) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_animation_new_from_resource(resource_path,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbufanimation = new_type_gdkpixbufanimation(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbufanimation);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_animation_new_from_file (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *filename;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_animation_new_from_file(filename,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbufanimation = new_type_gdkpixbufanimation(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbufanimation);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_animation_get_width(NspGdkPixbufAnimation *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_animation_get_width(GDK_PIXBUF_ANIMATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_animation_get_height(NspGdkPixbufAnimation *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_animation_get_height(GDK_PIXBUF_ANIMATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_animation_is_static_image(NspGdkPixbufAnimation *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_animation_is_static_image(GDK_PIXBUF_ANIMATION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_animation_get_static_image(NspGdkPixbufAnimation *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_animation_get_static_image(GDK_PIXBUF_ANIMATION(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdkpixbufanimation_methods[] = {
  {"get_width",(nsp_method *) _wrap_gdk_pixbuf_animation_get_width},
  {"get_height",(nsp_method *) _wrap_gdk_pixbuf_animation_get_height},
  {"is_static_image",(nsp_method *) _wrap_gdk_pixbuf_animation_is_static_image},
  {"get_static_image",(nsp_method *) _wrap_gdk_pixbuf_animation_get_static_image},
  { NULL, NULL}
};

static NspMethods *gdkpixbufanimation_get_methods(void) { return gdkpixbufanimation_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkpixbufanimation_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkPixbufAnimationIter ----------- */


#define  NspGdkPixbufAnimationIter_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkpixbufanimationiter.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkPixbufAnimationIter inherits from GObject 
 */

int nsp_type_gdkpixbufanimationiter_id=0;
NspTypeGdkPixbufAnimationIter *nsp_type_gdkpixbufanimationiter=NULL;

/*
 * Type object for NspGdkPixbufAnimationIter 
 * all the instance of NspTypeGdkPixbufAnimationIter share the same id. 
 * nsp_type_gdkpixbufanimationiter: is an instance of NspTypeGdkPixbufAnimationIter 
 *    used for objects of NspGdkPixbufAnimationIter type (i.e built with new_gdkpixbufanimationiter) 
 * other instances are used for derived classes 
 */
NspTypeGdkPixbufAnimationIter *new_type_gdkpixbufanimationiter(type_mode mode)
{
  NspTypeGdkPixbufAnimationIter *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixbufanimationiter != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixbufanimationiter;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixbufanimationiter_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkpixbufanimationiter_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkpixbufanimationiter;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkpixbufanimationiter */ 

  top->s_type =  (s_type_func *) nsp_gdkpixbufanimationiter_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkpixbufanimationiter_type_short_string;
  /* top->create = (create_func*) int_gdkpixbufanimationiter_create;*/

  /* specific methods for gdkpixbufanimationiter */

  type->init = (init_func *) init_gdkpixbufanimationiter;

  /* 
   * NspGdkPixbufAnimationIter interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkpixbufanimationiter_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkPixbufAnimationIter called nsp_type_gdkpixbufanimationiter
       */
      type->id =  nsp_type_gdkpixbufanimationiter_id = nsp_new_type_id();
      nsp_type_gdkpixbufanimationiter = type;
      if ( nsp_register_type(nsp_type_gdkpixbufanimationiter) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkpixbufanimationiter, GDK_TYPE_PIXBUF_ANIMATION_ITER);
      return ( mode == T_BASE ) ? type : new_type_gdkpixbufanimationiter(mode);
    }
  else 
    {
      type->id = nsp_type_gdkpixbufanimationiter_id;
      return type;
    }
}

/*
 * initialize NspGdkPixbufAnimationIter instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixbufanimationiter(NspGdkPixbufAnimationIter *Obj,NspTypeGdkPixbufAnimationIter *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkPixbufAnimationIter 
 */

NspGdkPixbufAnimationIter *new_gdkpixbufanimationiter() 
{
  NspGdkPixbufAnimationIter *loc;
  /* type must exists */
  nsp_type_gdkpixbufanimationiter = new_type_gdkpixbufanimationiter(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkPixbufAnimationIter)))== NULLGDKPIXBUFANIMATIONITER) return loc;
  /* initialize object */
  if ( init_gdkpixbufanimationiter(loc,nsp_type_gdkpixbufanimationiter) == FAIL) return NULLGDKPIXBUFANIMATIONITER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkPixbufAnimationIter 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkpixbufanimationiter_type_name[]="GdkPixbufAnimationIter";
static char gdkpixbufanimationiter_short_type_name[]="GdkPixbufAnimationIter";

static char *nsp_gdkpixbufanimationiter_type_as_string(void)
{
  return(gdkpixbufanimationiter_type_name);
}

static char *nsp_gdkpixbufanimationiter_type_short_string(NspObject *v)
{
  return(gdkpixbufanimationiter_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkPixbufAnimationIter objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkPixbufAnimationIter   *nsp_gdkpixbufanimationiter_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkpixbufanimationiter_id)  == TRUE  ) return ((NspGdkPixbufAnimationIter *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixbufanimationiter));
  return NULL;
}

int IsGdkPixbufAnimationIterObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkpixbufanimationiter_id);
}

int IsGdkPixbufAnimationIter(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkpixbufanimationiter_id);
}

NspGdkPixbufAnimationIter  *GetGdkPixbufAnimationIterCopy(Stack stack, int i)
{
  if (  GetGdkPixbufAnimationIter(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkPixbufAnimationIter  *GetGdkPixbufAnimationIter(Stack stack, int i)
{
  NspGdkPixbufAnimationIter *M;
  if (( M = nsp_gdkpixbufanimationiter_object(NthObj(i))) == NULLGDKPIXBUFANIMATIONITER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkPixbufAnimationIter *gdkpixbufanimationiter_copy(NspGdkPixbufAnimationIter *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixbufanimationiter);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixbufanimationiter);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkPixbufAnimationIter
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gdk_pixbuf_animation_iter_get_delay_time(NspGdkPixbufAnimationIter *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_animation_iter_get_delay_time(GDK_PIXBUF_ANIMATION_ITER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_animation_iter_get_pixbuf(NspGdkPixbufAnimationIter *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_animation_iter_get_pixbuf(GDK_PIXBUF_ANIMATION_ITER(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_animation_iter_on_currently_loading_frame(NspGdkPixbufAnimationIter *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_animation_iter_on_currently_loading_frame(GDK_PIXBUF_ANIMATION_ITER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdkpixbufanimationiter_methods[] = {
  {"get_delay_time",(nsp_method *) _wrap_gdk_pixbuf_animation_iter_get_delay_time},
  {"get_pixbuf",(nsp_method *) _wrap_gdk_pixbuf_animation_iter_get_pixbuf},
  {"on_currently_loading_frame",(nsp_method *) _wrap_gdk_pixbuf_animation_iter_on_currently_loading_frame},
  { NULL, NULL}
};

static NspMethods *gdkpixbufanimationiter_get_methods(void) { return gdkpixbufanimationiter_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkpixbufanimationiter_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGdkPixbufLoader ----------- */


#define  NspGdkPixbufLoader_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkpixbufloader.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGdkPixbufLoader inherits from GObject 
 */

int nsp_type_gdkpixbufloader_id=0;
NspTypeGdkPixbufLoader *nsp_type_gdkpixbufloader=NULL;

/*
 * Type object for NspGdkPixbufLoader 
 * all the instance of NspTypeGdkPixbufLoader share the same id. 
 * nsp_type_gdkpixbufloader: is an instance of NspTypeGdkPixbufLoader 
 *    used for objects of NspGdkPixbufLoader type (i.e built with new_gdkpixbufloader) 
 * other instances are used for derived classes 
 */
NspTypeGdkPixbufLoader *new_type_gdkpixbufloader(type_mode mode)
{
  NspTypeGdkPixbufLoader *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixbufloader != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixbufloader;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixbufloader_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdkpixbufloader_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdkpixbufloader;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdkpixbufloader */ 

  top->s_type =  (s_type_func *) nsp_gdkpixbufloader_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdkpixbufloader_type_short_string;
  /* top->create = (create_func*) int_gdkpixbufloader_create;*/

  /* specific methods for gdkpixbufloader */

  type->init = (init_func *) init_gdkpixbufloader;

  /* 
   * NspGdkPixbufLoader interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdkpixbufloader_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkPixbufLoader called nsp_type_gdkpixbufloader
       */
      type->id =  nsp_type_gdkpixbufloader_id = nsp_new_type_id();
      nsp_type_gdkpixbufloader = type;
      if ( nsp_register_type(nsp_type_gdkpixbufloader) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkpixbufloader, GDK_TYPE_PIXBUF_LOADER);
      return ( mode == T_BASE ) ? type : new_type_gdkpixbufloader(mode);
    }
  else 
    {
      type->id = nsp_type_gdkpixbufloader_id;
      return type;
    }
}

/*
 * initialize NspGdkPixbufLoader instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixbufloader(NspGdkPixbufLoader *Obj,NspTypeGdkPixbufLoader *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGdkPixbufLoader 
 */

NspGdkPixbufLoader *new_gdkpixbufloader() 
{
  NspGdkPixbufLoader *loc;
  /* type must exists */
  nsp_type_gdkpixbufloader = new_type_gdkpixbufloader(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkPixbufLoader)))== NULLGDKPIXBUFLOADER) return loc;
  /* initialize object */
  if ( init_gdkpixbufloader(loc,nsp_type_gdkpixbufloader) == FAIL) return NULLGDKPIXBUFLOADER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGdkPixbufLoader 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdkpixbufloader_type_name[]="GdkPixbufLoader";
static char gdkpixbufloader_short_type_name[]="GdkPixbufLoader";

static char *nsp_gdkpixbufloader_type_as_string(void)
{
  return(gdkpixbufloader_type_name);
}

static char *nsp_gdkpixbufloader_type_short_string(NspObject *v)
{
  return(gdkpixbufloader_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkPixbufLoader objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGdkPixbufLoader   *nsp_gdkpixbufloader_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkpixbufloader_id)  == TRUE  ) return ((NspGdkPixbufLoader *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixbufloader));
  return NULL;
}

int IsGdkPixbufLoaderObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdkpixbufloader_id);
}

int IsGdkPixbufLoader(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkpixbufloader_id);
}

NspGdkPixbufLoader  *GetGdkPixbufLoaderCopy(Stack stack, int i)
{
  if (  GetGdkPixbufLoader(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkPixbufLoader  *GetGdkPixbufLoader(Stack stack, int i)
{
  NspGdkPixbufLoader *M;
  if (( M = nsp_gdkpixbufloader_object(NthObj(i))) == NULLGDKPIXBUFLOADER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkPixbufLoader *gdkpixbufloader_copy(NspGdkPixbufLoader *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixbufloader);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixbufloader);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkPixbufLoader
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gdk_pixbuf_loader_new_with_mime_type (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *mime_type;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&mime_type) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_loader_new_with_mime_type(mime_type,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbufloader = new_type_gdkpixbufloader(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbufloader);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_loader_new_with_type (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *image_type;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&image_type) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_loader_new_with_type(image_type,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }

  nsp_type_gdkpixbufloader = new_type_gdkpixbufloader(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbufloader);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gdk_pixbuf_loader_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gdk_pixbuf_loader_new())== NULL) return RET_BUG;

  nsp_type_gdkpixbufloader = new_type_gdkpixbufloader(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbufloader);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_loader_set_size(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int width, height;
  if ( GetArgs(stack,rhs,opt,T,&width, &height) == FAIL) return RET_BUG;
    gdk_pixbuf_loader_set_size(GDK_PIXBUF_LOADER(self->obj),width,height);
  return 0;
}

static int _wrap_gdk_pixbuf_loader_write(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  guchar *buf;
  int count, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&buf, &count) == FAIL) return RET_BUG;
    ret =gdk_pixbuf_loader_write(GDK_PIXBUF_LOADER(self->obj),buf,count,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_loader_get_pixbuf(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_loader_get_pixbuf(GDK_PIXBUF_LOADER(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_loader_get_animation(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbufAnimation *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_loader_get_animation(GDK_PIXBUF_LOADER(self->obj));
  nsp_type_gdkpixbufanimation = new_type_gdkpixbufanimation(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbufanimation))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_loader_close(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =gdk_pixbuf_loader_close(GDK_PIXBUF_LOADER(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdkpixbufloader_methods[] = {
  {"set_size",(nsp_method *) _wrap_gdk_pixbuf_loader_set_size},
  {"write",(nsp_method *) _wrap_gdk_pixbuf_loader_write},
  {"get_pixbuf",(nsp_method *) _wrap_gdk_pixbuf_loader_get_pixbuf},
  {"get_animation",(nsp_method *) _wrap_gdk_pixbuf_loader_get_animation},
  {"close",(nsp_method *) _wrap_gdk_pixbuf_loader_close},
  { NULL, NULL}
};

static NspMethods *gdkpixbufloader_get_methods(void) { return gdkpixbufloader_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkpixbufloader_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_gdk_pixbuf_get_formats(Stack stack, int rhs, int opt, int lhs) /* gdk_pixbuf_get_formats */
{
  GSList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_pixbuf_get_formats();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

}

int _wrap_gdk_notify_startup_complete(Stack stack, int rhs, int opt, int lhs) /* notify_startup_complete */
{
  CheckRhs(0,0);
    gdk_notify_startup_complete();
  return 0;
}

int _wrap_gdk_get_display_arg_name(Stack stack, int rhs, int opt, int lhs) /* get_display_arg_name */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gdk_get_display_arg_name();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_get_program_class(Stack stack, int rhs, int opt, int lhs) /* get_program_class */
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gdk_get_program_class();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_set_program_class(Stack stack, int rhs, int opt, int lhs) /* set_program_class */
{
  int_types T[] = {string, t_end};
  char *program_class;
  if ( GetArgs(stack,rhs,opt,T,&program_class) == FAIL) return RET_BUG;
    gdk_set_program_class(program_class);
  return 0;
}

int _wrap_gdk_screen_width(Stack stack, int rhs, int opt, int lhs) /* screen_width */
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_width();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_screen_height(Stack stack, int rhs, int opt, int lhs) /* screen_height */
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_height();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_screen_width_mm(Stack stack, int rhs, int opt, int lhs) /* screen_width_mm */
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_width_mm();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_screen_height_mm(Stack stack, int rhs, int opt, int lhs) /* screen_height_mm */
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_screen_height_mm();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_flush(Stack stack, int rhs, int opt, int lhs) /* flush */
{
  CheckRhs(0,0);
    gdk_flush();
  return 0;
}

int _wrap_gdk_beep(Stack stack, int rhs, int opt, int lhs) /* beep */
{
  CheckRhs(0,0);
    gdk_beep();
  return 0;
}

int _wrap_gdk_set_double_click_time(Stack stack, int rhs, int opt, int lhs) /* set_double_click_time */
{
  int_types T[] = {s_int, t_end};
  int msec;
  if ( GetArgs(stack,rhs,opt,T,&msec) == FAIL) return RET_BUG;
    gdk_set_double_click_time(msec);
  return 0;
}

#line 43 "codegen-3.0/gdk.override"

/* block/unblock threads implementations using GDK lock to handle
 * recursion (as in 1.2 version).  Here is the comments about it from
 * the 1.2 version of pygtk:*/

/* The threading hacks are based on ones supplied by Duncan Grisby
 * of AT&T Labs Cambridge.  Since then they have been modified a bit. */

/* The threading code has been enhanced to be a little better with multiple
 * threads accessing GTK+.  Here are some notes on the changes by
 * Paul Fisher:
 *
 * If threading is enabled, we create a recursive version of Nspthon's
 * global interpreter mutex using TSD.  This scheme makes it possible,
 * although rather hackish, for any thread to make a call into NspGTK,
 * as long as the GDK lock is held (that is, Nspthon code is wrapped
 * around a threads_{enter,leave} pair).
 *
 * A viable alternative would be to wrap each and every GTK call, at
 * the Nspthon/C level, with Nsp_{BEGIN,END}_ALLOW_THREADS.  However,
 * given the nature of Nspthon threading, this option is not
 * particularly appealing.
 */
#ifdef ENABLE_PYGTK_THREADING
static GStaticPrivate pythreadstate_key = G_STATIC_PRIVATE_INIT;
static GStaticPrivate lock_count_key = G_STATIC_PRIVATE_INIT;
static NspInterpreterState *pyinterpstate = NULL;

static void
nspgdk_block_threads (void)
{
  gint lock_count = GPOINTER_TO_INT(g_static_private_get(&lock_count_key));

  if (lock_count == 0) {
    NspThreadState *_save;

    _save = g_static_private_get(&pythreadstate_key);
    if (_save == NULL) {
      _save = NspThreadState_New(pyinterpstate);
    }
    Nsp_BLOCK_THREADS;
  }
  lock_count++;
  g_static_private_set(&lock_count_key, GINT_TO_POINTER(lock_count), NULL);
}
static void
nspgdk_unblock_threads (void)
{
  gint lock_count = GPOINTER_TO_INT(g_static_private_get(&lock_count_key));

  lock_count--;
  if (lock_count == 0) {
    NspThreadState *_save;

    Nsp_UNBLOCK_THREADS;
    g_static_private_set(&pythreadstate_key, _save, NULL);
  }
  g_static_private_set(&lock_count_key, GINT_TO_POINTER(lock_count), NULL);
}
#endif

static int
_wrap_gdk_threads_init(Stack stack,int rhs,int opt,int lhs)
{
#ifdef ENABLE_PYGTK_THREADING
  /* register gdk thread block/unblock routines with gobjectmodule */
  nspg_set_thread_block_funcs (nspgdk_block_threads, nspgdk_unblock_threads);

  NspEval_InitThreads();
  gdk_threads_init();
  g_static_private_set(&lock_count_key, GINT_TO_POINTER(1), NULL);

  pyinterpstate = NspThreadState_Get()->interp;
  return 0;
#else
  Scierror("nspgtk threading disabled at compile time \n");
  return 0;
#endif
}
#line 10027 "gdk.c"


int _wrap_gdk_pre_parse_libgtk_only(Stack stack, int rhs, int opt, int lhs) /* pre_parse_libgtk_only */
{
  CheckRhs(0,0);
    gdk_pre_parse_libgtk_only();
  return 0;
}

int _wrap_gdk_error_trap_push(Stack stack, int rhs, int opt, int lhs) /* error_trap_push */
{
  CheckRhs(0,0);
    gdk_error_trap_push();
  return 0;
}

int _wrap_gdk_error_trap_pop(Stack stack, int rhs, int opt, int lhs) /* error_trap_pop */
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_error_trap_pop();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 450 "codegen-3.0/gdk.override"
static int
_wrap_gdk_color_parse(Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "spec", NULL };*/
  const char *spec;
  GdkColor colour;
  int_types T[] = { string ,t_end};
  NspObject *ret;

  if (GetArgs(stack,rhs,opt,T, &spec) == FAIL ) return RET_BUG;
  gdk_color_parse (spec, &colour);
  if ((ret = (NspObject *) gboxed_create (NVOID,GDK_TYPE_COLOR, &colour, TRUE, TRUE,(NspTypeBase *) nsp_type_gdkcolor)) == NULL)
    return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 10070 "gdk.c"


int _wrap_gdk_drag_status(Stack stack, int rhs, int opt, int lhs) /* gdk_drag_status */
{
  int_types T[] = {obj_check,obj,s_int, t_end};
  NspGObject *context;
  GdkDragAction action;
  NspObject *nsp_action = NULL;
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdragcontext, &context, &nsp_action, &time_) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_DRAG_ACTION, nsp_action, &action)==FAIL)
      return RET_BUG;
    gdk_drag_status(GDK_DRAG_CONTEXT(context->obj),action,time_);
  return 0;
}

int _wrap_gdk_drop_reply(Stack stack, int rhs, int opt, int lhs) /* gdk_drop_reply */
{
  int_types T[] = {obj_check,s_bool,s_int, t_end};
  NspGObject *context;
  int accepted;
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdragcontext, &context, &accepted, &time_) == FAIL) return RET_BUG;
    gdk_drop_reply(GDK_DRAG_CONTEXT(context->obj),accepted,time_);
  return 0;
}

int _wrap_gdk_drop_finish(Stack stack, int rhs, int opt, int lhs) /* gdk_drop_finish */
{
  int_types T[] = {obj_check,s_bool,s_int, t_end};
  NspGObject *context;
  int success;
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdragcontext, &context, &success, &time_) == FAIL) return RET_BUG;
    gdk_drop_finish(GDK_DRAG_CONTEXT(context->obj),success,time_);
  return 0;
}

int _wrap_gdk_drag_get_selection(Stack stack, int rhs, int opt, int lhs) /* gdk_drag_get_selection */
{
  int_types T[] = {obj_check, t_end};
  NspGObject *context;
  GdkAtom ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdragcontext, &context) == FAIL) return RET_BUG;
    ret =gdk_drag_get_selection(GDK_DRAG_CONTEXT(context->obj));
  if (( nsp_ret = (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 1655 "codegen-3.0/gdk.override"

static int
_wrap_gdk_drag_begin(Stack stack,int rhs,int opt,int lhs)
{
  NspGObject *nsp_window;
  int_types T[] = {obj_check,realmat, t_end};
  NspMatrix * nsp_targets;
  GList *targets = NULL;
  guint i;
  GdkDragContext *context;
  NspObject *nsp_context;

  if (GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &nsp_window, &nsp_targets) == FAIL )  return RET_BUG;

  for (i = 0; i < nsp_targets->mn ; i++)
    targets = g_list_append(targets, GUINT_TO_POINTER((guint)nsp_targets->R[i]));
  context = gdk_drag_begin(GDK_WINDOW(nsp_window->obj), targets);
  g_list_free(targets);
  nsp_context = (NspObject *) nspgobject_new(NVOID,(GObject *)context);
  g_object_unref(G_OBJECT(context));
  MoveObj(stack,1, nsp_context);
  return 1;
}

#line 10148 "gdk.c"


int _wrap_gdk_drag_begin_for_device(Stack stack, int rhs, int opt, int lhs) /* gdk_drag_begin_for_device */
{
  int_types T[] = {obj_check,obj_check,list, t_end};
  NspGObject *window, *device;
  NspList *nsp_targets;
  GList *targets;
  GdkDragContext *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window, &nsp_type_gdkdevice, &device, &nsp_targets) == FAIL) return RET_BUG;
  targets=nsp_glist_from_nsplist(stack,nsp_targets);
  if (targets== NULL) return RET_BUG;
    ret =gdk_drag_begin_for_device(GDK_WINDOW(window->obj),GDK_DEVICE(device->obj),targets);
  nsp_type_gdkdragcontext = new_type_gdkdragcontext(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdragcontext))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_drag_motion(Stack stack, int rhs, int opt, int lhs) /* gdk_drag_motion */
{
  int_types T[] = {obj_check,obj_check,obj,s_int,s_int,obj,obj,s_int, t_end};
  NspGObject *context, *dest_window;
  GdkDragProtocol protocol;
  NspObject *nsp_protocol = NULL, *nsp_suggested_action = NULL, *nsp_possible_actions = NULL;
  int x_root, y_root, ret;
  GdkDragAction suggested_action, possible_actions;
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdragcontext, &context, &nsp_type_gdkwindow, &dest_window, &nsp_protocol, &x_root, &y_root, &nsp_suggested_action, &nsp_possible_actions, &time_) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_DRAG_PROTOCOL, nsp_protocol, &protocol)== FAIL)
      return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_DRAG_ACTION, nsp_suggested_action, &suggested_action)==FAIL)
      return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_DRAG_ACTION, nsp_possible_actions, &possible_actions)==FAIL)
      return RET_BUG;
    ret =gdk_drag_motion(GDK_DRAG_CONTEXT(context->obj),GDK_WINDOW(dest_window->obj),protocol,x_root,y_root,suggested_action,possible_actions,time_);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_drag_drop(Stack stack, int rhs, int opt, int lhs) /* gdk_drag_drop */
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *context;
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdragcontext, &context, &time_) == FAIL) return RET_BUG;
    gdk_drag_drop(GDK_DRAG_CONTEXT(context->obj),time_);
  return 0;
}

int _wrap_gdk_drag_abort(Stack stack, int rhs, int opt, int lhs) /* gdk_drag_abort */
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *context;
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdragcontext, &context, &time_) == FAIL) return RET_BUG;
    gdk_drag_abort(GDK_DRAG_CONTEXT(context->obj),time_);
  return 0;
}

int _wrap_gdk_drag_drop_succeeded(Stack stack, int rhs, int opt, int lhs) /* gdk_drag_drop_succeeded */
{
  int_types T[] = {obj_check, t_end};
  NspGObject *context;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdragcontext, &context) == FAIL) return RET_BUG;
    ret =gdk_drag_drop_succeeded(GDK_DRAG_CONTEXT(context->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_events_pending(Stack stack, int rhs, int opt, int lhs) /* gdk_events_pending */
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_events_pending();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_event_get(Stack stack, int rhs, int opt, int lhs) /* gdk_event_get */
{
  GdkEvent *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_event_get();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_event_peek(Stack stack, int rhs, int opt, int lhs) /* gdk_event_peek */
{
  GdkEvent *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_event_peek();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_set_show_events(Stack stack, int rhs, int opt, int lhs) /* gdk_set_show_events */
{
  int_types T[] = {s_bool, t_end};
  int show_events;
  if ( GetArgs(stack,rhs,opt,T,&show_events) == FAIL) return RET_BUG;
    gdk_set_show_events(show_events);
  return 0;
}

int _wrap_gdk_get_show_events(Stack stack, int rhs, int opt, int lhs) /* gdk_get_show_events */
{
  int ret;
  CheckRhs(0,0);
    ret =gdk_get_show_events();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_gl_context_get_current(Stack stack, int rhs, int opt, int lhs) /* gdk_gl_context_get_current */
{
  GdkGLContext *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_gl_context_get_current();
  nsp_type_gdkglcontext = new_type_gdkglcontext(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkglcontext))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_gdk_gl_context_get_current(Stack stack, int rhs, int opt, int lhs) /* gdk_gl_context_get_current */
{
  Scierror("Error: function gdk_gl_context_get_current not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_gl_context_clear_current(Stack stack, int rhs, int opt, int lhs) /* gdk_gl_context_clear_current */
{
  CheckRhs(0,0);
    gdk_gl_context_clear_current();
  return 0;
}

#else
int _wrap_gdk_gl_context_clear_current(Stack stack, int rhs, int opt, int lhs) /* gdk_gl_context_clear_current */
{
  Scierror("Error: function gdk_gl_context_clear_current not available\n");
  return RET_BUG;
}
#endif
int _wrap_gdk_keymap_get_default(Stack stack, int rhs, int opt, int lhs) /* keymap_get_default */
{
  GdkKeymap *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_keymap_get_default();
  nsp_type_gdkkeymap = new_type_gdkkeymap(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkkeymap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_keyval_name(Stack stack, int rhs, int opt, int lhs) /* keyval_name */
{
  int_types T[] = {s_int, t_end};
  int keyval;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret =gdk_keyval_name(keyval);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_from_name(Stack stack, int rhs, int opt, int lhs) /* keyval_from_name */
{
  int_types T[] = {string, t_end};
  char *keyval_name;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&keyval_name) == FAIL) return RET_BUG;
    ret =gdk_keyval_from_name(keyval_name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_to_upper(Stack stack, int rhs, int opt, int lhs) /* keyval_to_upper */
{
  int_types T[] = {s_int, t_end};
  int keyval, ret;
  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret =gdk_keyval_to_upper(keyval);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_to_lower(Stack stack, int rhs, int opt, int lhs) /* keyval_to_lower */
{
  int_types T[] = {s_int, t_end};
  int keyval, ret;
  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret =gdk_keyval_to_lower(keyval);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_is_upper(Stack stack, int rhs, int opt, int lhs) /* keyval_is_upper */
{
  int_types T[] = {s_int, t_end};
  int keyval, ret;
  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret =gdk_keyval_is_upper(keyval);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_is_lower(Stack stack, int rhs, int opt, int lhs) /* keyval_is_lower */
{
  int_types T[] = {s_int, t_end};
  int keyval, ret;
  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret =gdk_keyval_is_lower(keyval);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_to_unicode(Stack stack, int rhs, int opt, int lhs) /* keyval_to_unicode */
{
  int_types T[] = {s_int, t_end};
  int keyval;
  gulong ret;
  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret =gdk_keyval_to_unicode(keyval);
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_unicode_to_keyval(Stack stack, int rhs, int opt, int lhs) /* unicode_to_keyval */
{
  int_types T[] = {s_int, t_end};
  gulong wc;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&wc) == FAIL) return RET_BUG;
    ret =gdk_unicode_to_keyval(wc);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_atom_intern(Stack stack, int rhs, int opt, int lhs) /* atom_intern */
{
  int_types T[] = {string,new_opts, t_end};
  nsp_option opts[] = {
	{"only_if_exists",s_bool,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  char *atom_name;
  int only_if_exists = FALSE;
  GdkAtom ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&atom_name, opts, &only_if_exists) == FAIL) return RET_BUG;
    ret =gdk_atom_intern(atom_name,only_if_exists);
  if (( nsp_ret = (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_selection_owner_set(Stack stack, int rhs, int opt, int lhs) /* gdk_selection_owner_set */
{
  int_types T[] = {obj_check,obj,s_int,s_bool, t_end};
  NspGObject *owner;
  GdkAtom selection;
  NspObject *nsp_selection = NULL;
  gulong time_;
  int send_event, ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &owner, &nsp_selection, &time_, &send_event) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
    ret =gdk_selection_owner_set(GDK_WINDOW(owner->obj),selection,time_,send_event);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_selection_owner_get(Stack stack, int rhs, int opt, int lhs) /* gdk_selection_owner_get */
{
  int_types T[] = {obj, t_end};
  GdkAtom selection;
  NspObject *nsp_selection = NULL, *nsp_ret;
  GdkWindow *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_selection) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
    ret =gdk_selection_owner_get(selection);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_selection_owner_set_for_display(Stack stack, int rhs, int opt, int lhs) /* gdk_selection_owner_set_for_display */
{
  int_types T[] = {obj_check,obj_check,obj,s_int,s_bool, t_end};
  NspGObject *display, *owner;
  GdkAtom selection;
  NspObject *nsp_selection = NULL;
  gulong time_;
  int send_event, ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdisplay, &display, &nsp_type_gdkwindow, &owner, &nsp_selection, &time_, &send_event) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
    ret =gdk_selection_owner_set_for_display(GDK_DISPLAY(display->obj),GDK_WINDOW(owner->obj),selection,time_,send_event);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_selection_owner_get_for_display(Stack stack, int rhs, int opt, int lhs) /* gdk_selection_owner_get_for_display */
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *display;
  GdkAtom selection;
  NspObject *nsp_selection = NULL, *nsp_ret;
  GdkWindow *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdisplay, &display, &nsp_selection) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
    ret =gdk_selection_owner_get_for_display(GDK_DISPLAY(display->obj),selection);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_selection_convert(Stack stack, int rhs, int opt, int lhs) /* gdk_selection_convert */
{
  int_types T[] = {obj_check,obj,obj,s_int, t_end};
  NspGObject *requestor;
  GdkAtom selection, target;
  NspObject *nsp_selection = NULL, *nsp_target = NULL;
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &requestor, &nsp_selection, &nsp_target, &time_) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_target,&target)==FAIL) return RET_BUG;
    gdk_selection_convert(GDK_WINDOW(requestor->obj),selection,target,time_);
  return 0;
}

#line 2516 "codegen-3.0/gdk.override"

int _wrap_gdk_selection_send_notify(Stack stack, int rhs, int opt, int lhs) /* selection_send_notify */
{
  int_types T[] = {obj_check, obj, obj, obj, s_int,t_end};
  gulong time;
  NspGObject *nsp_window;
  GdkAtom selection, target, property;
  NspObject *nsp_selection = NULL, *nsp_target = NULL, *nsp_property = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &nsp_window, &nsp_selection, &nsp_target, &nsp_property, &time)
       == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_target,&target)==FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_property,&property)==FAIL) return RET_BUG;
  gdk_selection_send_notify(GDK_WINDOW(nsp_window->obj), selection, target, property, time);
  return 0;
}

#line 10516 "gdk.c"


int _wrap_gdk_selection_send_notify_for_display(Stack stack, int rhs, int opt, int lhs) /* gdk_selection_send_notify_for_display */
{
  int_types T[] = {obj_check,obj_check,obj,obj,obj,s_int, t_end};
  NspGObject *display, *requestor;
  GdkAtom selection, target, property;
  NspObject *nsp_selection = NULL, *nsp_target = NULL, *nsp_property = NULL;
  gulong time_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdisplay, &display, &nsp_type_gdkwindow, &requestor, &nsp_selection, &nsp_target, &nsp_property, &time_) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_target,&target)==FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_property,&property)==FAIL) return RET_BUG;
    gdk_selection_send_notify_for_display(GDK_DISPLAY(display->obj),GDK_WINDOW(requestor->obj),selection,target,property,time_);
  return 0;
}

int _wrap_gdk_visual_get_best_with_both(Stack stack, int rhs, int opt, int lhs) /* gdk_visual_get_best_with_both */
{
  int_types T[] = {s_int,obj, t_end};
  int depth;
  GdkVisualType visual_type;
  NspObject *nsp_visual_type = NULL, *nsp_ret;
  GdkVisual *ret;
  if ( GetArgs(stack,rhs,opt,T,&depth, &nsp_visual_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_VISUAL_TYPE, nsp_visual_type, &visual_type)== FAIL)
      return RET_BUG;
    ret =gdk_visual_get_best_with_both(depth,visual_type);
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_list_visuals(Stack stack, int rhs, int opt, int lhs) /* gdk_list_visuals */
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gdk_list_visuals();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_gdk_window_process_all_updates(Stack stack, int rhs, int opt, int lhs) /* gdk_window_process_all_updates */
{
  CheckRhs(0,0);
    gdk_window_process_all_updates();
  return 0;
}

int _wrap_gdk_window_set_debug_updates(Stack stack, int rhs, int opt, int lhs) /* gdk_window_set_debug_updates */
{
  int_types T[] = {s_bool, t_end};
  int setting;
  if ( GetArgs(stack,rhs,opt,T,&setting) == FAIL) return RET_BUG;
    gdk_window_set_debug_updates(setting);
  return 0;
}

int _wrap_gdk_get_default_root_window(Stack stack, int rhs, int opt, int lhs) /* gdk_get_default_root_window */
{
  GdkWindow *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_get_default_root_window();
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_offscreen_window_get_surface(Stack stack, int rhs, int opt, int lhs) /* gdk_offscreen_window_get_surface */
{
  int_types T[] = {obj_check, t_end};
  NspGObject *window;
  cairo_surface_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window) == FAIL) return RET_BUG;
    ret =gdk_offscreen_window_get_surface(GDK_WINDOW(window->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_SURFACE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_surface_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_offscreen_window_set_embedder(Stack stack, int rhs, int opt, int lhs) /* gdk_offscreen_window_set_embedder */
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *window, *embedder;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window, &nsp_type_gdkwindow, &embedder) == FAIL) return RET_BUG;
    gdk_offscreen_window_set_embedder(GDK_WINDOW(window->obj),GDK_WINDOW(embedder->obj));
  return 0;
}

int _wrap_gdk_offscreen_window_get_embedder(Stack stack, int rhs, int opt, int lhs) /* gdk_offscreen_window_get_embedder */
{
  int_types T[] = {obj_check, t_end};
  NspGObject *window;
  GdkWindow *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window) == FAIL) return RET_BUG;
    ret =gdk_offscreen_window_get_embedder(GDK_WINDOW(window->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_screen_get_default(Stack stack, int rhs, int opt, int lhs) /* gdk_screen_get_default */
{
  GdkScreen *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_screen_get_default();
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 2399 "codegen-3.0/gdk.override"

int _wrap_gdk_display_open(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *display_name;
  GdkDisplay *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&display_name) == FAIL) return RET_BUG;
  ret = gdk_display_open(display_name);
  if ( ret == NULL)
    {
      if (( nsp_ret = (NspObject *) nsp_none_create(NVOID,NULL))== NULL) return RET_BUG;
    }
  else
    {
      nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
      if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
    }
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 10662 "gdk.c"


int _wrap_gdk_display_get_default(Stack stack, int rhs, int opt, int lhs) /* gdk_display_get_default */
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_get_default();
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_display_open_default_libgtk_only(Stack stack, int rhs, int opt, int lhs) /* gdk_display_open_default_libgtk_only */
{
  GdkDisplay *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_open_default_libgtk_only();
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_display_manager_get(Stack stack, int rhs, int opt, int lhs) /* gdk_display_manager_get */
{
  GdkDisplayManager *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gdk_display_manager_get();
  nsp_type_gdkdisplaymanager = new_type_gdkdisplaymanager(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplaymanager))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 2595 "codegen-3.0/gdk.override"

int _wrap_gdk_rgba_new(Stack stack, int rhs, int opt, int lhs) /* gdk_rgba_new */
{
  GdkRGBA ret={0,0,0,0};
  int_types T[] = {string, t_end};
  char *spec;
  NspObject *nsp_ret;
  CheckLhs(0,1);
  if ( rhs != 0)
    {
      if ( GetArgs(stack,rhs,opt,T,&spec) == FAIL) return RET_BUG;
      if ( gdk_rgba_parse (&ret,spec) == FALSE) return RET_BUG;
    }
  /* create a boxed with a copy of ret */
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_RGBA, &ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkrgba))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 10723 "gdk.c"


int _wrap_gdk_cairo_create(Stack stack, int rhs, int opt, int lhs) /* gdk_cairo_create */
{
  int_types T[] = {obj_check, t_end};
  NspGObject *window;
  cairo_t *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window) == FAIL) return RET_BUG;
    ret =gdk_cairo_create(GDK_WINDOW(window->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_CONTEXT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_cairo_get_clip_rectangle(Stack stack, int rhs, int opt, int lhs) /* gdk_cairo_get_clip_rectangle */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_rect;
  GdkRectangle rect = { 0, 0, 0, 0 };
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_rect) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (!nsp_gdk_rectangle_from_object(nsp_rect, &rect))
      return RET_BUG;
    ret =gdk_cairo_get_clip_rectangle(cr,&rect);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_cairo_set_source_rgba(Stack stack, int rhs, int opt, int lhs) /* gdk_cairo_set_source_rgba */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_rgba = NULL;
  GdkRGBA *rgba = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_rgba) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_rgba, GDK_TYPE_RGBA))
      rgba = nspg_boxed_get(nsp_rgba, GdkRGBA);
  else {
      Scierror( "Error: rgba should be a GdkRGBA\n");
      return RET_BUG;
  }
    gdk_cairo_set_source_rgba(cr,rgba);
  return 0;
}

#line 2618 "codegen-3.0/gdk.override"

int _wrap_gdk_cairo_set_source_pixbuf(Stack stack, int rhs, int opt, int lhs)
{
  double x,y;
  int_types T[] = {obj, obj,s_double,s_double , t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_pixbuf=NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr,&nsp_pixbuf,&x,&y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
    cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
    Scierror("Error: first argument should be a cairo_t");
    return RET_BUG;
  }
  if ( IsGdkPixbuf(nsp_pixbuf) == FALSE )
    {
      Scierror("Error: second argument should be a pixbuf");
      return RET_BUG;
    }
  gdk_cairo_set_source_pixbuf(cr,GDK_PIXBUF(((NspGdkPixbuf *) nsp_pixbuf)->obj),x,y);
  return 0;
}
#line 10808 "gdk.c"


int _wrap_gdk_cairo_set_source_window(Stack stack, int rhs, int opt, int lhs) /* gdk_cairo_set_source_window */
{
  int_types T[] = {obj,obj_check,s_double,s_double, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  NspGObject *window;
  double x, y;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_type_gdkwindow, &window, &x, &y) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    gdk_cairo_set_source_window(cr,GDK_WINDOW(window->obj),x,y);
  return 0;
}

int _wrap_gdk_cairo_region(Stack stack, int rhs, int opt, int lhs) /* gdk_cairo_region */
{
  int_types T[] = {obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_region = NULL;
  cairo_region_t *region = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_region) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_region, CAIRO_GOBJECT_TYPE_REGION))
      region = nspg_boxed_get(nsp_region, cairo_region_t);
  else {
      Scierror( "Error: region should be a cairo_region_t\n");
      return RET_BUG;
  }
    gdk_cairo_region(cr,region);
  return 0;
}

int _wrap_gdk_cairo_region_create_from_surface(Stack stack, int rhs, int opt, int lhs) /* gdk_cairo_region_create_from_surface */
{
  int_types T[] = {obj, t_end};
  cairo_surface_t *surface = NULL;
  NspObject *nsp_surface = NULL, *nsp_ret;
  cairo_region_t *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_surface) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_surface, CAIRO_GOBJECT_TYPE_SURFACE))
      surface = nspg_boxed_get(nsp_surface, cairo_surface_t);
  else {
      Scierror( "Error: surface should be a cairo_surface_t\n");
      return RET_BUG;
  }
    ret =gdk_cairo_region_create_from_surface(surface);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,CAIRO_GOBJECT_TYPE_REGION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_cairo_region_t))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(3,16,0)
int _wrap_gdk_cairo_draw_from_gl(Stack stack, int rhs, int opt, int lhs) /* gdk_cairo_draw_from_gl */
{
  int_types T[] = {obj,obj_check,s_int,s_int,s_int,s_int,s_int,s_int,s_int, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL;
  NspGObject *window;
  int source, source_type, buffer_scale, x, y, width, height;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_type_gdkwindow, &window, &source, &source_type, &buffer_scale, &x, &y, &width, &height) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
    gdk_cairo_draw_from_gl(cr,GDK_WINDOW(window->obj),source,source_type,buffer_scale,x,y,width,height);
  return 0;
}

#else
int _wrap_gdk_cairo_draw_from_gl(Stack stack, int rhs, int opt, int lhs) /* gdk_cairo_draw_from_gl */
{
  Scierror("Error: function gdk_cairo_draw_from_gl not available\n");
  return RET_BUG;
}
#endif
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gdk_func[]={
  { "gdk_event_new", _wrap_gdk_event_new},
  { "gdkevent_new", _wrap_gdk_event_new},
  { "gdk_color_new", _wrap_gdk_color_new},
  { "gdkcolor_new", _wrap_gdk_color_new},
  { "gdk_rectangle_new", _wrap_gdk_rectangle_new},
  { "gdkrectangle_new", _wrap_gdk_rectangle_new},
 /* gdkwindow_new gdk_window_new */
  { "gdk_visual_new", _wrap_gdk_visual_new},
  { "gdkvisual_new", _wrap_gdk_visual_new},
  { "gdk_cursor_new_for_display", _wrap_gdk_cursor_new_for_display},
  { "gdk_cursor_new", _wrap_gdk_cursor_new},
  { "gdkcursor_new", _wrap_gdk_cursor_new},
  { "gdk_cursor_new_from_pixbuf", _wrap_gdk_cursor_new_from_pixbuf},
  { "gdk_cursor_new_from_surface", _wrap_gdk_cursor_new_from_surface},
  { "gdk_cursor_new_from_name", _wrap_gdk_cursor_new_from_name},
  { "gdk_pixbuf_new", _wrap_gdk_pixbuf_new},
  { "gdkpixbuf_new", _wrap_gdk_pixbuf_new},
  { "gdk_pixbuf_new_subpixbuf", _wrap_gdk_pixbuf_new_subpixbuf},
  { "gdk_pixbuf_new_from_file", _wrap_gdk_pixbuf_new_from_file},
  { "gdk_pixbuf_new_from_file_at_size", _wrap_gdk_pixbuf_new_from_file_at_size},
  { "gdk_pixbuf_new_from_file_at_scale", _wrap_gdk_pixbuf_new_from_file_at_scale},
  { "gdk_pixbuf_new_from_resource", _wrap_gdk_pixbuf_new_from_resource},
  { "gdk_pixbuf_new_from_resource_at_scale", _wrap_gdk_pixbuf_new_from_resource_at_scale},
 /* gdkpixbuf_new gdk_pixbuf_new_from_data */
  { "gdk_pixbuf_new_from_xpm_data", _wrap_gdk_pixbuf_new_from_xpm_data},
 /* gdkpixbuf_new gdk_pixbuf_new_from_inline */
 /* gdkpixbuf_new gdk_pixbuf_new_from_stream */
 /* gdkpixbuf_new gdk_pixbuf_new_from_stream_finish */
 /* gdkpixbuf_new gdk_pixbuf_new_from_stream_at_scale */
  { "gdk_pixbuf_animation_new_from_file", _wrap_gdk_pixbuf_animation_new_from_file},
 /* gdkpixbufanimation_new gdk_pixbuf_animation_new_from_stream */
 /* gdkpixbufanimation_new gdk_pixbuf_animation_new_from_stream_finish */
  { "gdk_pixbuf_animation_new_from_resource", _wrap_gdk_pixbuf_animation_new_from_resource},
  { "gdk_pixbuf_loader_new", _wrap_gdk_pixbuf_loader_new},
  { "gdkpixbufloader_new", _wrap_gdk_pixbuf_loader_new},
  { "gdk_pixbuf_loader_new_with_type", _wrap_gdk_pixbuf_loader_new_with_type},
  { "gdk_pixbuf_loader_new_with_mime_type", _wrap_gdk_pixbuf_loader_new_with_mime_type},
  { "gdk_pixbuf_get_formats", _wrap_gdk_pixbuf_get_formats},
  { "gdk_notify_startup_complete", _wrap_gdk_notify_startup_complete},
  { "gdk_get_display_arg_name", _wrap_gdk_get_display_arg_name},
  { "gdk_get_program_class", _wrap_gdk_get_program_class},
  { "gdk_set_program_class", _wrap_gdk_set_program_class},
  { "gdk_screen_width", _wrap_gdk_screen_width},
  { "gdk_screen_height", _wrap_gdk_screen_height},
  { "gdk_screen_width_mm", _wrap_gdk_screen_width_mm},
  { "gdk_screen_height_mm", _wrap_gdk_screen_height_mm},
  { "gdk_flush", _wrap_gdk_flush},
  { "gdk_beep", _wrap_gdk_beep},
  { "gdk_set_double_click_time", _wrap_gdk_set_double_click_time},
  { "gdk_threads_init", _wrap_gdk_threads_init},
  { "gdk_pre_parse_libgtk_only", _wrap_gdk_pre_parse_libgtk_only},
  { "gdk_error_trap_push", _wrap_gdk_error_trap_push},
  { "gdk_error_trap_pop", _wrap_gdk_error_trap_pop},
  { "gdk_color_parse", _wrap_gdk_color_parse},
  { "gdk_drag_status", _wrap_gdk_drag_status},
  { "gdk_drop_reply", _wrap_gdk_drop_reply},
  { "gdk_drop_finish", _wrap_gdk_drop_finish},
  { "gdk_drag_get_selection", _wrap_gdk_drag_get_selection},
  { "gdk_drag_begin", _wrap_gdk_drag_begin},
  { "gdk_drag_begin_for_device", _wrap_gdk_drag_begin_for_device},
  { "gdk_drag_motion", _wrap_gdk_drag_motion},
  { "gdk_drag_drop", _wrap_gdk_drag_drop},
  { "gdk_drag_abort", _wrap_gdk_drag_abort},
  { "gdk_drag_drop_succeeded", _wrap_gdk_drag_drop_succeeded},
  { "gdk_events_pending", _wrap_gdk_events_pending},
  { "gdk_event_get", _wrap_gdk_event_get},
  { "gdk_event_peek", _wrap_gdk_event_peek},
  { "gdk_set_show_events", _wrap_gdk_set_show_events},
  { "gdk_get_show_events", _wrap_gdk_get_show_events},
  { "gdk_gl_context_get_current", _wrap_gdk_gl_context_get_current},
  { "gdk_gl_context_clear_current", _wrap_gdk_gl_context_clear_current},
  { "gdk_keymap_get_default", _wrap_gdk_keymap_get_default},
  { "gdk_keyval_name", _wrap_gdk_keyval_name},
  { "gdk_keyval_from_name", _wrap_gdk_keyval_from_name},
  { "gdk_keyval_to_upper", _wrap_gdk_keyval_to_upper},
  { "gdk_keyval_to_lower", _wrap_gdk_keyval_to_lower},
  { "gdk_keyval_is_upper", _wrap_gdk_keyval_is_upper},
  { "gdk_keyval_is_lower", _wrap_gdk_keyval_is_lower},
  { "gdk_keyval_to_unicode", _wrap_gdk_keyval_to_unicode},
  { "gdk_unicode_to_keyval", _wrap_gdk_unicode_to_keyval},
  { "gdk_atom_intern", _wrap_gdk_atom_intern},
  { "gdk_selection_owner_set", _wrap_gdk_selection_owner_set},
  { "gdk_selection_owner_get", _wrap_gdk_selection_owner_get},
  { "gdk_selection_owner_set_for_display", _wrap_gdk_selection_owner_set_for_display},
  { "gdk_selection_owner_get_for_display", _wrap_gdk_selection_owner_get_for_display},
  { "gdk_selection_convert", _wrap_gdk_selection_convert},
  { "gdk_selection_send_notify", _wrap_gdk_selection_send_notify},
  { "gdk_selection_send_notify_for_display", _wrap_gdk_selection_send_notify_for_display},
  { "gdk_visual_get_best_with_both", _wrap_gdk_visual_get_best_with_both},
  { "gdk_list_visuals", _wrap_gdk_list_visuals},
  { "gdk_window_process_all_updates", _wrap_gdk_window_process_all_updates},
  { "gdk_window_set_debug_updates", _wrap_gdk_window_set_debug_updates},
  { "gdk_get_default_root_window", _wrap_gdk_get_default_root_window},
  { "gdk_offscreen_window_get_surface", _wrap_gdk_offscreen_window_get_surface},
  { "gdk_offscreen_window_set_embedder", _wrap_gdk_offscreen_window_set_embedder},
  { "gdk_offscreen_window_get_embedder", _wrap_gdk_offscreen_window_get_embedder},
  { "gdk_screen_get_default", _wrap_gdk_screen_get_default},
  { "gdk_display_open", _wrap_gdk_display_open},
  { "gdk_display_get_default", _wrap_gdk_display_get_default},
  { "gdk_display_open_default_libgtk_only", _wrap_gdk_display_open_default_libgtk_only},
  { "gdk_display_manager_get", _wrap_gdk_display_manager_get},
  { "gdk_rgba_new", _wrap_gdk_rgba_new},
  { "gdk_cairo_create", _wrap_gdk_cairo_create},
  { "gdk_cairo_get_clip_rectangle", _wrap_gdk_cairo_get_clip_rectangle},
  { "gdk_cairo_set_source_rgba", _wrap_gdk_cairo_set_source_rgba},
  { "gdk_cairo_set_source_pixbuf", _wrap_gdk_cairo_set_source_pixbuf},
  { "gdk_cairo_set_source_window", _wrap_gdk_cairo_set_source_window},
  { "gdk_cairo_region", _wrap_gdk_cairo_region},
  { "gdk_cairo_region_create_from_surface", _wrap_gdk_cairo_region_create_from_surface},
  { "gdk_cairo_draw_from_gl", _wrap_gdk_cairo_draw_from_gl},
  { NULL, NULL}
};

/* call ith function in the gdk interface */

int gdk_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,gdk_func[i].fonc,
  					       &stack,rhs,opt,lhs);
#else
  return (*(gdk_func[i].fonc))(stack,rhs,opt,lhs);
#endif
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void gdk_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = gdk_func[i].name;
  *f = gdk_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
gdk_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_CURSOR_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_INPUT_SOURCE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_INPUT_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_AXIS_USE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_DEVICE_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_DRAG_ACTION, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_DRAG_PROTOCOL, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_FILTER_RETURN, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_EVENT_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_VISIBILITY_STATE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_SCROLL_DIRECTION, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_NOTIFY_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_CROSSING_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_PROPERTY_STATE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_WINDOW_STATE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_SETTING_ACTION, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_OWNER_CHANGE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_FRAME_CLOCK_PHASE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_PROP_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_BYTE_ORDER, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_MODIFIER_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_MODIFIER_INTENT, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_STATUS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_GRAB_STATUS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_GRAB_OWNERSHIP, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_EVENT_MASK, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_VISUAL_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_WINDOW_WINDOW_CLASS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_WINDOW_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_WINDOW_ATTRIBUTES_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_WINDOW_HINTS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_WINDOW_TYPE_HINT, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_WM_DECORATION, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GDK_TYPE_WM_FUNCTION, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_GRAVITY, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_WINDOW_EDGE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_FULLSCREEN_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_PIXBUF_ALPHA_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_COLORSPACE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_PIXBUF_ERROR, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_INTERP_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GDK_TYPE_PIXBUF_ROTATION, strip_prefix);
}

void nsp_initialize_gdk_types(void)
{
  new_type_gdkevent(T_BASE);
  new_type_gdkcolor(T_BASE);
  new_type_gdkrectangle(T_BASE);
  new_type_gdkrgba(T_BASE);
  new_type_gdkdevice(T_BASE);
  new_type_gdkdevicemanager(T_BASE);
  new_type_gdkdisplay(T_BASE);
  new_type_gdkdisplaymanager(T_BASE);
  new_type_gdkdragcontext(T_BASE);
  new_type_gdkframeclock(T_BASE);
#if GTK_CHECK_VERSION(3,16,0) 
  new_type_gdkglcontext(T_BASE);
#endif /* GTK_CHECK_VERSION */
  new_type_gdkwindow(T_BASE);
  new_type_gdkkeymap(T_BASE);
  new_type_gdkscreen(T_BASE);
  new_type_gdkvisual(T_BASE);
  new_type_gdkcursor(T_BASE);
  new_type_gdkpixbuf(T_BASE);
  new_type_gdkpixbufanimation(T_BASE);
  new_type_gdkpixbufanimationiter(T_BASE);
  new_type_gdkpixbufloader(T_BASE);
}

#line 11114 "gdk.c"
