/* -*- Mode: C -*- */


#line 4 "gdk.override"
#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>

#define GDK_DISPLAY(object) GDK_DISPLAY_OBJECT(object) 

/* #include "pygtk-private.h" */

#line 15 "gdk.c"


/* ---------- types from other modules ---------- */
#include "nsp/gtk/gobject.h"
#include "nsp/gtk/pangocontext.h"
#include "nsp/gtk/pangofont.h"
#include "nsp/gtk/pangolayout.h"
#include "nsp/gtk/gtkwidget.h"
#include "nsp/gtk/gdkatom.h"


/* ---------- forward type declarations ---------- */
#define GdkEvent_Private
#include "nsp/gtk/gdkevent.h"
#define GdkFont_Private
#include "nsp/gtk/gdkfont.h"
#define GdkColor_Private
#include "nsp/gtk/gdkcolor.h"
#define GdkCursor_Private
#include "nsp/gtk/gdkcursor.h"
#define GdkRectangle_Private
#include "nsp/gtk/gdkrectangle.h"
#define GdkColormap_Private
#include "nsp/gtk/gdkcolormap.h"
#define GdkDevice_Private
#include "nsp/gtk/gdkdevice.h"
#define GdkDragContext_Private
#include "nsp/gtk/gdkdragcontext.h"
#define GdkDrawable_Private
#include "nsp/gtk/gdkdrawable.h"
#define GdkWindow_Private
#include "nsp/gtk/gdkwindow.h"
#define GdkPixmap_Private
#include "nsp/gtk/gdkpixmap.h"
#define GdkBitmap_Private
#include "nsp/gtk/gdkbitmap.h"
#define GdkGC_Private
#include "nsp/gtk/gdkgc.h"
#define GdkImage_Private
#include "nsp/gtk/gdkimage.h"
#define GdkKeymap_Private
#include "nsp/gtk/gdkkeymap.h"
#define GdkPixbuf_Private
#include "nsp/gtk/gdkpixbuf.h"
#define GdkPixbufAnimation_Private
#include "nsp/gtk/gdkpixbufanimation.h"
#define GdkPixbufAnimationIter_Private
#include "nsp/gtk/gdkpixbufanimationiter.h"
#define GdkPixbufLoader_Private
#include "nsp/gtk/gdkpixbufloader.h"
#define GdkVisual_Private
#include "nsp/gtk/gdkvisual.h"
#define GdkScreen_Private
#include "nsp/gtk/gdkscreen.h"
#define GdkDisplay_Private
#include "nsp/gtk/gdkdisplay.h"


/* ----------- GdkEvent ----------- */

static int _wrap_gdk_event_tp_getattr(Stack stack, int rhs, int opt, int lhs);
/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkEvent_Private 
#include "nsp/gtk/gdkevent.h"
#include "nsp/interf.h"

/* NspGdkEvent inherits from NspGBoxed */ 

int nsp_type_gdkevent_id=0;
NspTypeGdkEvent *nsp_type_gdkevent=NULL;

NspTypeGdkEvent *new_type_gdkevent(type_mode mode)
{
  NspTypeGdkEvent *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkevent != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkevent;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkevent_attrs ; 
  type->get_attrs = (attrs_func *)  _wrap_gdk_event_tp_getattr;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkevent_get_methods; 
  type->new = (new_func *) new_gdkevent;

  /* specific methods for gdkevent */
      
  type->init = (init_func *) init_gdkevent;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkevent */ 

  top->s_type =  (s_type_func *) gdkevent_type_as_string;    
  top->sh_type = (sh_type_func *) gdkevent_type_short_string;
  /* top->create = (create_func*) int_gdkevent_create;*/ 
  
  /* specific methods for gdkevent */
      
  type->init = (init_func *) init_gdkevent;

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
 * initialize GdkEvent instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkevent(NspGdkEvent *o,NspTypeGdkEvent *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkEvent 
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
 * Object method redefined for GdkEvent 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkevent_type_name[]="GdkEvent";
static char gdkevent_short_type_name[]="GdkEvent";

static char *gdkevent_type_as_string(void)
{
  return(gdkevent_type_name);
}

static char *gdkevent_type_short_string(void)
{
  return(gdkevent_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkEvent objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkEvent   *gdkevent_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkevent_id) ) return ((NspGdkEvent *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkevent));
  return NULL;
}

int IsGdkEventObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkevent_id);
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
  if (( M = gdkevent_object(NthObj(i))) == NULLGDKEVENT)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkEvent *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkevent is initialized * /
  nsp_type_gdkevent = new_type_gdkevent(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_gdkevent)) == NULLGDKEVENT) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_gdk_event_put(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_event_put(NSP_GBOXED_GET(self, GdkEvent));
  return 0;
}

static int _wrap_gdk_event_copy(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkEvent *ret;
  NspObject *nsp_ret;

  ret = gdk_event_copy(NSP_GBOXED_GET(self, GdkEvent));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_event_free(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_event_free(NSP_GBOXED_GET(self, GdkEvent));
  return 0;
}

static int _wrap_gdk_event_get_time(NspGdkEvent *self,Stack stack,int rhs,int opt,int lhs)
{
  gulong ret;

  ret = gdk_event_get_time(NSP_GBOXED_GET(self, GdkEvent));
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

#line 1065 "gdk.override"
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
#line 313 "gdk.c"


#line 1077 "gdk.override"
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
#line 327 "gdk.c"


#line 1049 "gdk.override"
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
#line 345 "gdk.c"


static NspMethods gdkevent_methods[] = {
  {"put",(nsp_method *) _wrap_gdk_event_put},
  {"copy",(nsp_method *) _wrap_gdk_event_copy},
  {"free",(nsp_method *) _wrap_gdk_event_free},
  {"get_time",(nsp_method *) _wrap_gdk_event_get_time},
  {"get_coords",(nsp_method *) _wrap_gdk_event_get_coords},
  {"get_root_coords",(nsp_method *) _wrap_gdk_event_get_root_coords},
  {"get_axis",(nsp_method *) _wrap_gdk_event_get_axis},
  { NULL, NULL}
};

static NspMethods *gdkevent_get_methods(void) { return gdkevent_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkevent_attrs[]={{NULL,NULL,NULL}} ;
#line 714 "gdk.override"

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
	return (NspObject *) gboxed_create(NVOID,GDK_TYPE_RECTANGLE, &event->expose.area,TRUE, TRUE,(NspTypeBase *) nsp_type_gdkrectangle);
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
	if (event->motion.axes) n = event->motion.device->num_axes; 
	if (( v = nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return NULL;
	for (i = 0; i < n ; i++) v->R[i]=  event->motion.axes[i];
	return (NspObject *) v;
      }
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->motion.state));
      if (!strcmp(attr, "is_hint"))
	return nsp_new_double_obj((double) (event->motion.is_hint));
      if (!strcmp(attr, "device"))
	return (NspObject *) nspgobject_new((GObject *)event->motion.device);
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
	if (event->button.axes) n = event->button.device->num_axes; 
	if (( v = nsp_matrix_create(NVOID,'r',1,n)) == NULLMAT) return NULL;
	for (i = 0; i < n ; i++) v->R[i]=  event->button.axes[i];
	return (NspObject *) v;
      }
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->button.state));
      if (!strcmp(attr, "button"))
	return nsp_new_double_obj((double) (event->button.button));
      if (!strcmp(attr, "device"))
	return (NspObject *) nspgobject_new((GObject *)event->button.device);
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
	(NspObject *)nsp_smatrix_create_from_table(tab);
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
	return (NspObject *) nspgobject_new((GObject *)event->crossing.subwindow);
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
	return nsp_new_double_obj((double) (event->selection.requestor));
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
	return (NspObject *) nspgobject_new((GObject *)event->proximity.device);
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
	return (NspObject *) nspgobject_new((GObject *)event->dnd.context);
      if (!strcmp(attr, "time"))
	return nsp_new_double_obj((double) (event->dnd.time));
      if (!strcmp(attr, "x_root"))
	return nsp_new_double_obj((double)event->dnd.x_root);
      if (!strcmp(attr, "y_root"))
	return nsp_new_double_obj((double)event->dnd.y_root);
      break;
    }
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
  case GDK_VISIBILITY_NOTIFY: /*GdkEventVisibility        visibility*/
    { 
      static char *tab[] ={ "type", "window", "send_event","state",NULL};
      if (!strcmp(attr, "__attrs"))
	return (NspObject *)nsp_smatrix_create_from_table(tab);
      if (!strcmp(attr, "state"))
	return nsp_new_double_obj((double) (event->visibility.state));
      break;
    }
  case GDK_NO_EXPOSE:         /*GdkEventNoExpose          no_expose*/
				  break; 
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
	return (NspObject *) nspgobject_new((GObject *)event->scroll.device);
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
    return (NspObject *) nspgobject_new((GObject *)event->any.window);
  if (!strcmp(attr, "send_event"))
    return nsp_new_double_obj((double) (event->any.send_event));
  if (!strcmp(attr, "__attrs"))
    {
      static char *tab[] ={ "type", "window", "send_event",NULL};
      return  (NspObject *)nsp_smatrix_create_from_table(tab);
    }
  Scierror("Event attributes not found \n");
  return NULL;
}
#line 699 "gdk.c"




/* ----------- GdkFont ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkFont_Private 
#include "nsp/gtk/gdkfont.h"
#include "nsp/interf.h"

/* NspGdkFont inherits from NspGBoxed */ 

int nsp_type_gdkfont_id=0;
NspTypeGdkFont *nsp_type_gdkfont=NULL;

NspTypeGdkFont *new_type_gdkfont(type_mode mode)
{
  NspTypeGdkFont *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkfont != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkfont;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkfont_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkfont_get_methods; 
  type->new = (new_func *) new_gdkfont;

  /* specific methods for gdkfont */
      
  type->init = (init_func *) init_gdkfont;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkfont */ 

  top->s_type =  (s_type_func *) gdkfont_type_as_string;    
  top->sh_type = (sh_type_func *) gdkfont_type_short_string;
  /* top->create = (create_func*) int_gdkfont_create;*/ 
  
  /* specific methods for gdkfont */
      
  type->init = (init_func *) init_gdkfont;

  if ( nsp_type_gdkfont_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkFont called nsp_type_gdkfont
       */
      type->id =  nsp_type_gdkfont_id = nsp_new_type_id();
      nsp_type_gdkfont = type;
      if ( nsp_register_type(nsp_type_gdkfont) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkfont, GDK_TYPE_FONT);
      return ( mode == T_BASE ) ? type : new_type_gdkfont(mode);
    }
  else 
    {
       type->id = nsp_type_gdkfont_id;
       return type;
    }
}

/*
 * initialize GdkFont instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkfont(NspGdkFont *o,NspTypeGdkFont *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkFont 
 */

NspGdkFont *new_gdkfont() 
{
  NspGdkFont *loc; 
  /* type must exists */
  nsp_type_gdkfont = new_type_gdkfont(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkFont)))== NULLGDKFONT) return loc;
  /* initialize object */
  if ( init_gdkfont(loc,nsp_type_gdkfont) == FAIL) return NULLGDKFONT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GdkFont 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkfont_type_name[]="GdkFont";
static char gdkfont_short_type_name[]="GdkFont";

static char *gdkfont_type_as_string(void)
{
  return(gdkfont_type_name);
}

static char *gdkfont_type_short_string(void)
{
  return(gdkfont_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkFont objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkFont   *gdkfont_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkfont_id) ) return ((NspGdkFont *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkfont));
  return NULL;
}

int IsGdkFontObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkfont_id);
}

int IsGdkFont(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkfont_id);
}

NspGdkFont  *GetGdkFontCopy(Stack stack, int i)
{
  if (  GetGdkFont(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkFont  *GetGdkFont(Stack stack, int i)
{
  NspGdkFont *M;
  if (( M = gdkfont_object(NthObj(i))) == NULLGDKFONT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGdkFont *gdkfont_copy(NspGdkFont *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gdkfont);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkFont
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkFont *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkfont is initialized * /
  nsp_type_gdkfont = new_type_gdkfont(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_gdkfont)) == NULLGDKFONT) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_gdkfont_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *font_name;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&font_name) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_font_load(font_name))== NULL) return RET_BUG;

  nsp_type_gdkfont = new_type_gdkfont(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_FONT, ret,TRUE,TRUE,(NspTypeBase *) nsp_type_gdkfont );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_font_id(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_font_id(NSP_GBOXED_GET(self, GdkFont));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_string_width(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *string;
  int ret;

  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
  ret = gdk_string_width(NSP_GBOXED_GET(self, GdkFont), string);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_text_width(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,new_opts,t_end};
  nsp_option opts[] = {
	{"text_length",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  char *text;
  int text_length = -1, ret;

  if ( GetArgs(stack,rhs,opt,T,&text, opts, &text_length) == FAIL) return RET_BUG;
  ret = gdk_text_width(NSP_GBOXED_GET(self, GdkFont), text, text_length);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 482 "gdk.override"
static int _wrap_gdk_char_width(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  char *str;
  int i;
  NspMatrix *ret;
  CheckRhs(1,1) ;     
  if ((str = GetString(stack,1))== NULL) return RET_BUG;
  if ((ret = nsp_matrix_create(NVOID,'r',1,strlen(str)))== NULLMAT) return RET_BUG; 
  for ( i=0 ; i < ret->mn; i++) 
    ret->R[i] = gdk_char_width(NSP_GBOXED_GET(self, GdkFont),str[i]);
  MoveObj(stack,1,(NspObject * ) ret);
  return 1; 
}
#line 964 "gdk.c"


static int _wrap_gdk_string_measure(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *string;
  int ret;

  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
  ret = gdk_string_measure(NSP_GBOXED_GET(self, GdkFont), string);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_text_measure(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,new_opts,t_end};
  nsp_option opts[] = {
	{"text_length",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  char *text;
  int text_length = -1, ret;

  if ( GetArgs(stack,rhs,opt,T,&text, opts, &text_length) == FAIL) return RET_BUG;
  ret = gdk_text_measure(NSP_GBOXED_GET(self, GdkFont), text, text_length);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 497 "gdk.override"
static int _wrap_gdk_char_measure(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  char *str;
  int i;
  NspMatrix *ret;
  CheckRhs(1,1) ;     
  if ((str = GetString(stack,1))== NULL) return RET_BUG;
  if ((ret = nsp_matrix_create(NVOID,'r',1,strlen(str)))== NULLMAT) return RET_BUG; 
  for ( i=0 ; i < ret->mn; i++) 
    ret->R[i] = gdk_char_measure(NSP_GBOXED_GET(self, GdkFont), str[i]);
  MoveObj(stack,1,(NspObject * ) ret);
  return 1; 
}
#line 1008 "gdk.c"


static int _wrap_gdk_string_height(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *string;
  int ret;

  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
  ret = gdk_string_height(NSP_GBOXED_GET(self, GdkFont), string);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_text_height(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,new_opts,t_end};
  nsp_option opts[] = {
	{"text_length",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  char *text;
  int text_length = -1, ret;

  if ( GetArgs(stack,rhs,opt,T,&text, opts, &text_length) == FAIL) return RET_BUG;
  ret = gdk_text_height(NSP_GBOXED_GET(self, GdkFont), text, text_length);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 512 "gdk.override"
static int _wrap_gdk_char_height(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  char *str;
  int i;
  NspMatrix *ret;
  CheckRhs(1,1) ;     
  if ((str = GetString(stack,1))== NULL) return RET_BUG;
  if ((ret = nsp_matrix_create(NVOID,'r',1,strlen(str)))== NULLMAT) return RET_BUG; 
  for ( i=0 ; i < ret->mn; i++) 
    ret->R[i] = gdk_char_height(NSP_GBOXED_GET(self, GdkFont), str[i]);
  MoveObj(stack,1,(NspObject * ) ret);
  return 1; 
}

#line 1053 "gdk.c"


#line 528 "gdk.override"
static int
_wrap_gdk_text_extents(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "text", NULL };*/
  gchar *text;
  gint length;
  gint lbearing, rbearing, width, ascent, descent;
  int_types T[] = { string, t_end};
  if (GetArgs(stack,rhs,opt,T, /* "s#:GdkFont.extents", */
	      &text) == FAIL )
    return RET_BUG;
  length= strlen(text);
  gdk_text_extents(nspg_boxed_get(self, GdkFont), text, length,
		   &lbearing, &rbearing, &width, &ascent, &descent);

  if ( nsp_move_doubles(stack,1,1,5,(double) lbearing,(double)  rbearing,(double)  width,
			(double) ascent, (double) descent) == FAIL) return RET_BUG; 
  return 1;
}
#line 1076 "gdk.c"


static int _wrap_gdk_string_extents(NspGdkFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, s_int, s_int, s_int, s_int, s_int,t_end};
  char *string;
  int lbearing, rbearing, width, ascent, descent;

  if ( GetArgs(stack,rhs,opt,T,&string, &lbearing, &rbearing, &width, &ascent, &descent) == FAIL) return RET_BUG;
  gdk_string_extents(NSP_GBOXED_GET(self, GdkFont), string, &lbearing, &rbearing, &width, &ascent, &descent);
  return 0;
}

static NspMethods gdkfont_methods[] = {
  {"id",(nsp_method *) _wrap_gdk_font_id},
  {"string_width",(nsp_method *) _wrap_gdk_string_width},
  {"width",(nsp_method *) _wrap_gdk_text_width},
  {"char_width",(nsp_method *) _wrap_gdk_char_width},
  {"string_measure",(nsp_method *) _wrap_gdk_string_measure},
  {"measure",(nsp_method *) _wrap_gdk_text_measure},
  {"char_measure",(nsp_method *) _wrap_gdk_char_measure},
  {"string_height",(nsp_method *) _wrap_gdk_string_height},
  {"height",(nsp_method *) _wrap_gdk_text_height},
  {"char_height",(nsp_method *) _wrap_gdk_char_height},
  {"extents",(nsp_method *) _wrap_gdk_text_extents},
  {"string_extents",(nsp_method *) _wrap_gdk_string_extents},
  { NULL, NULL}
};

static NspMethods *gdkfont_get_methods(void) { return gdkfont_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_font__get_type(NspObject *self,char *attr)
{
  gint ret;

  ret = NSP_GBOXED_GET(self, GdkFont)->type;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_font__get_ascent(NspObject *self,char *attr)
{
  int ret;

  ret = NSP_GBOXED_GET(self, GdkFont)->ascent;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_font__get_descent(NspObject *self,char *attr)
{
  int ret;

  ret = NSP_GBOXED_GET(self, GdkFont)->descent;
  return nsp_new_double_obj((double) ret);
}

static AttrTab gdkfont_attrs[] = {
  { "type", (attr_get_function *)_wrap_gdk_font__get_type, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "ascent", (attr_get_function *)_wrap_gdk_font__get_ascent, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "descent", (attr_get_function *)_wrap_gdk_font__get_descent, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- GdkColor ----------- */

static int _wrap_gdk_color_tp_setattr(Stack stack, int rhs, int opt, int lhs);
/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkColor_Private 
#include "nsp/gtk/gdkcolor.h"
#include "nsp/interf.h"

/* NspGdkColor inherits from NspGBoxed */ 

int nsp_type_gdkcolor_id=0;
NspTypeGdkColor *nsp_type_gdkcolor=NULL;

NspTypeGdkColor *new_type_gdkcolor(type_mode mode)
{
  NspTypeGdkColor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkcolor != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkcolor;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkcolor_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  _wrap_gdk_color_tp_setattr;
  type->methods = gdkcolor_get_methods; 
  type->new = (new_func *) new_gdkcolor;

  /* specific methods for gdkcolor */
      
  type->init = (init_func *) init_gdkcolor;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkcolor */ 

  top->s_type =  (s_type_func *) gdkcolor_type_as_string;    
  top->sh_type = (sh_type_func *) gdkcolor_type_short_string;
  /* top->create = (create_func*) int_gdkcolor_create;*/ 
  
  /* specific methods for gdkcolor */
      
  type->init = (init_func *) init_gdkcolor;

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
 * initialize GdkColor instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkcolor(NspGdkColor *o,NspTypeGdkColor *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkColor 
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
 * Object method redefined for GdkColor 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkcolor_type_name[]="GdkColor";
static char gdkcolor_short_type_name[]="GdkColor";

static char *gdkcolor_type_as_string(void)
{
  return(gdkcolor_type_name);
}

static char *gdkcolor_type_short_string(void)
{
  return(gdkcolor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkColor objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkColor   *gdkcolor_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkcolor_id) ) return ((NspGdkColor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkcolor));
  return NULL;
}

int IsGdkColorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkcolor_id);
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
  if (( M = gdkcolor_object(NthObj(i))) == NULLGDKCOLOR)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkColor *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkcolor is initialized * /
  nsp_type_gdkcolor = new_type_gdkcolor(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_gdkcolor)) == NULLGDKCOLOR) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 397 "gdk.override"
static int
_wrap_gdkcolor_new(Stack stack, int rhs, int opt, int lhs)
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
#line 1355 "gdk.c"


static NspMethods *gdkcolor_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_color__get_pixel(NspObject *self,char *attr)
{
  NspObject *nsp_ret;
  gulong ret;

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
  { "pixel", (attr_get_function *)_wrap_gdk_color__get_pixel, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "red", (attr_get_function *)_wrap_gdk_color__get_red, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "green", (attr_get_function *)_wrap_gdk_color__get_green, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "blue", (attr_get_function *)_wrap_gdk_color__get_blue, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};

#line 415 "gdk.override"

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
  if ( IntScalar(self,&val) == FAIL) return FAIL;
  if (!strcmp(attr, "red")) nspg_boxed_get(self, GdkColor)->red = val;
  else if (!strcmp(attr, "green"))  nspg_boxed_get(self, GdkColor)->green = val;	    
  else if (!strcmp(attr, "blue"))   nspg_boxed_get(self, GdkColor)->blue = val;
  else if (!strcmp(attr, "pixel"))  nspg_boxed_get(self, GdkColor)->pixel = val;
  return OK;
}

#line 1436 "gdk.c"




/* ----------- GdkCursor ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkCursor_Private 
#include "nsp/gtk/gdkcursor.h"
#include "nsp/interf.h"

/* NspGdkCursor inherits from NspGBoxed */ 

int nsp_type_gdkcursor_id=0;
NspTypeGdkCursor *nsp_type_gdkcursor=NULL;

NspTypeGdkCursor *new_type_gdkcursor(type_mode mode)
{
  NspTypeGdkCursor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkcursor != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkcursor;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkcursor_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkcursor_get_methods; 
  type->new = (new_func *) new_gdkcursor;

  /* specific methods for gdkcursor */
      
  type->init = (init_func *) init_gdkcursor;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkcursor */ 

  top->s_type =  (s_type_func *) gdkcursor_type_as_string;    
  top->sh_type = (sh_type_func *) gdkcursor_type_short_string;
  /* top->create = (create_func*) int_gdkcursor_create;*/ 
  
  /* specific methods for gdkcursor */
      
  type->init = (init_func *) init_gdkcursor;

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
 * initialize GdkCursor instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkcursor(NspGdkCursor *o,NspTypeGdkCursor *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkCursor 
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
 * Object method redefined for GdkCursor 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkcursor_type_name[]="GdkCursor";
static char gdkcursor_short_type_name[]="GdkCursor";

static char *gdkcursor_type_as_string(void)
{
  return(gdkcursor_type_name);
}

static char *gdkcursor_type_short_string(void)
{
  return(gdkcursor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkCursor objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkCursor   *gdkcursor_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkcursor_id) ) return ((NspGdkCursor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkcursor));
  return NULL;
}

int IsGdkCursorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkcursor_id);
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
  if (( M = gdkcursor_object(NthObj(i))) == NULLGDKCURSOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspGdkCursor *gdkcursor_copy(NspGdkCursor *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_gdkcursor);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkCursor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkCursor *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkcursor is initialized * /
  nsp_type_gdkcursor = new_type_gdkcursor(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_gdkcursor)) == NULLGDKCURSOR) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 652 "gdk.override"
static int
_wrap_gdkcursor_new(Stack stack,int rhs,int opt,int lhs)
{
  /* 
  static char *kwlist1[] = { "cursor_type", NULL };
  static char *kwlist2[] = { "source", "mask", "fg", "bg", "x", "y", NULL };
  */
  NspObject *nsp_cursor_type, *nsp_ret;
  gpointer gb; 
  
  CheckRhs(1,6); 
  if ( rhs == 1 ) 
    {
      GdkCursorType cursor_type;
      if (( nsp_cursor_type =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
      if (nspg_enum_get_value(GDK_TYPE_CURSOR_TYPE, nsp_cursor_type,(gint *)&cursor_type)== FAIL)
	return RET_BUG; 
      gb = gdk_cursor_new(cursor_type);
    }
  else 
    {
      NspGObject *source, *mask;
      NspObject  *fg, *bg;
      gint x, y;
      int_types T[] = { obj,obj,obj,obj, s_int, s_int,t_end};
      if (GetArgs(stack,rhs,opt,T, &source, &mask, &fg, &bg, &x, &y) == FAIL )
	return RET_BUG;
      if (!nspgobject_check(source, (NspTypeBase *) nsp_type_gdkpixmap)) {
	Scierror( "source should be a GdkPixmap");
	return RET_BUG;
      }
      if (!nspgobject_check(mask, (NspTypeBase *) nsp_type_gdkpixmap)) {
	Scierror( "mask should be a GdkPixmap");
	return RET_BUG;
      }
      if (!nspg_boxed_check(fg, GDK_TYPE_COLOR)) {
	Scierror( "fg should be a GdkColor");
	return RET_BUG;
      }
      if (!nspg_boxed_check(bg, GDK_TYPE_COLOR)) {
	Scierror( "bg should be a GdkColor");
	return RET_BUG;
      }
      gb = gdk_cursor_new_from_pixmap(GDK_PIXMAP(source->obj),
				      GDK_PIXMAP(mask->obj),
				      nspg_boxed_get(fg, GdkColor),
				      nspg_boxed_get(bg, GdkColor),
				      x, y);
    }
  if ( gb == NULL) 
    {
      Scierror("could not create GdkCursor object");
      return RET_BUG;
    }
  nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_CURSOR,gb, TRUE, TRUE,/* XXXX */
			  (NspTypeBase *) nsp_type_gdkcursor);
  if ( nsp_ret == NULLOBJ) return RET_BUG; 
  MoveObj(stack,1,(NspObject *) nsp_ret);
  return 1;
}
#line 1695 "gdk.c"


static NspMethods *gdkcursor_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_cursor__get_type(NspObject *self,char *attr)
{
  gint ret;

  ret = NSP_GBOXED_GET(self, GdkCursor)->type;
  return nsp_new_double_obj((double) ret);
}

static AttrTab gdkcursor_attrs[] = {
  { "type", (attr_get_function *)_wrap_gdk_cursor__get_type, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- GdkRectangle ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkRectangle_Private 
#include "nsp/gtk/gdkrectangle.h"
#include "nsp/interf.h"

/* NspGdkRectangle inherits from NspGBoxed */ 

int nsp_type_gdkrectangle_id=0;
NspTypeGdkRectangle *nsp_type_gdkrectangle=NULL;

NspTypeGdkRectangle *new_type_gdkrectangle(type_mode mode)
{
  NspTypeGdkRectangle *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkrectangle != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkrectangle;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkrectangle_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkrectangle_get_methods; 
  type->new = (new_func *) new_gdkrectangle;

  /* specific methods for gdkrectangle */
      
  type->init = (init_func *) init_gdkrectangle;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkrectangle */ 

  top->s_type =  (s_type_func *) gdkrectangle_type_as_string;    
  top->sh_type = (sh_type_func *) gdkrectangle_type_short_string;
  /* top->create = (create_func*) int_gdkrectangle_create;*/ 
  
  /* specific methods for gdkrectangle */
      
  type->init = (init_func *) init_gdkrectangle;

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
 * initialize GdkRectangle instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkrectangle(NspGdkRectangle *o,NspTypeGdkRectangle *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkRectangle 
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
 * Object method redefined for GdkRectangle 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkrectangle_type_name[]="GdkRectangle";
static char gdkrectangle_short_type_name[]="GdkRectangle";

static char *gdkrectangle_type_as_string(void)
{
  return(gdkrectangle_type_name);
}

static char *gdkrectangle_type_short_string(void)
{
  return(gdkrectangle_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkRectangle objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkRectangle   *gdkrectangle_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkrectangle_id) ) return ((NspGdkRectangle *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkrectangle));
  return NULL;
}

int IsGdkRectangleObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkrectangle_id);
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
  if (( M = gdkrectangle_object(NthObj(i))) == NULLGDKRECTANGLE)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkRectangle *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkrectangle is initialized * /
  nsp_type_gdkrectangle = new_type_gdkrectangle(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_gdkrectangle)) == NULLGDKRECTANGLE) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 2136 "gdk.override"
static int
_wrap_gdkrectangle_new(Stack stack,int rhs,int opt,int lhs)
{
  GdkRectangle re;
  NspGBoxed *H;
  if (rhs == 1 ) 
    {
      NspMatrix *r; 
      if (( r = GetRealMat(stack,1))== NULLMAT) return RET_BUG; 
      CheckLength(stack,1,r,4);
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
      Scierror("%s: rhs should be 1 or 4 \n",stack.fname);
      return RET_BUG;
    }
  if(( H = gboxed_create(NVOID,GDK_TYPE_RECTANGLE, &re, TRUE, TRUE,
			 nsp_type_gdkrectangle)) == NULLGBOXED)
    return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
}
#line 1950 "gdk.c"


#line 2176 "gdk.override"
static int
_wrap_gdk_rectangle_intersect(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_src;
  int_types T[] = { obj_check ,t_end};
  GdkRectangle *r = nspg_boxed_get(self, GdkRectangle);
  if (GetArgs(stack,rhs,opt,T,&nsp_type_gdkrectangle,&nsp_src) == FAIL ) 
    return RET_BUG; 

  gdk_rectangle_intersect(r, nspg_boxed_get(nsp_src, GdkRectangle),r);
  NthObj(1)->ret_pos = 1;
  return 1;
}
#line 1967 "gdk.c"


#line 2191 "gdk.override"
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
#line 1984 "gdk.c"


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
  { "x", (attr_get_function *)_wrap_gdk_rectangle__get_x, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "y", (attr_get_function *)_wrap_gdk_rectangle__get_y, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "width", (attr_get_function *)_wrap_gdk_rectangle__get_width, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "height", (attr_get_function *)_wrap_gdk_rectangle__get_height, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- GdkColormap ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkColormap_Private 
#include "nsp/gtk/gdkcolormap.h"
#include "nsp/interf.h"

/* NspGdkColormap inherits from NspGObject */ 

int nsp_type_gdkcolormap_id=0;
NspTypeGdkColormap *nsp_type_gdkcolormap=NULL;

NspTypeGdkColormap *new_type_gdkcolormap(type_mode mode)
{
  NspTypeGdkColormap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkcolormap != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkcolormap;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkcolormap_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkcolormap_get_methods; 
  type->new = (new_func *) new_gdkcolormap;

  /* specific methods for gdkcolormap */
      
  type->init = (init_func *) init_gdkcolormap;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkcolormap */ 

  top->s_type =  (s_type_func *) gdkcolormap_type_as_string;    
  top->sh_type = (sh_type_func *) gdkcolormap_type_short_string;
  /* top->create = (create_func*) int_gdkcolormap_create;*/ 
  
  /* specific methods for gdkcolormap */
      
  type->init = (init_func *) init_gdkcolormap;

  if ( nsp_type_gdkcolormap_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkColormap called nsp_type_gdkcolormap
       */
      type->id =  nsp_type_gdkcolormap_id = nsp_new_type_id();
      nsp_type_gdkcolormap = type;
      if ( nsp_register_type(nsp_type_gdkcolormap) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkcolormap, GDK_TYPE_COLORMAP);
      return ( mode == T_BASE ) ? type : new_type_gdkcolormap(mode);
    }
  else 
    {
       type->id = nsp_type_gdkcolormap_id;
       return type;
    }
}

/*
 * initialize GdkColormap instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkcolormap(NspGdkColormap *o,NspTypeGdkColormap *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkColormap 
 */

NspGdkColormap *new_gdkcolormap() 
{
  NspGdkColormap *loc; 
  /* type must exists */
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkColormap)))== NULLGDKCOLORMAP) return loc;
  /* initialize object */
  if ( init_gdkcolormap(loc,nsp_type_gdkcolormap) == FAIL) return NULLGDKCOLORMAP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GdkColormap 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkcolormap_type_name[]="GdkColormap";
static char gdkcolormap_short_type_name[]="GdkColormap";

static char *gdkcolormap_type_as_string(void)
{
  return(gdkcolormap_type_name);
}

static char *gdkcolormap_type_short_string(void)
{
  return(gdkcolormap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkColormap objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkColormap   *gdkcolormap_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkcolormap_id) ) return ((NspGdkColormap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkcolormap));
  return NULL;
}

int IsGdkColormapObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkcolormap_id);
}

int IsGdkColormap(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkcolormap_id);
}

NspGdkColormap  *GetGdkColormapCopy(Stack stack, int i)
{
  if (  GetGdkColormap(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkColormap  *GetGdkColormap(Stack stack, int i)
{
  NspGdkColormap *M;
  if (( M = gdkcolormap_object(NthObj(i))) == NULLGDKCOLORMAP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkColormap *gdkcolormap_copy(NspGdkColormap *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkcolormap);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkcolormap);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkColormap
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkColormap *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkcolormap is initialized * /
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkcolormap)) == NULLGDKCOLORMAP) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_gdkcolormap_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, s_bool,t_end};
  int allocate;
  NspGObject *visual;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkvisual, &visual, &allocate) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_colormap_new(GDK_VISUAL(visual->obj), allocate))== NULL) return RET_BUG;

  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkcolormap );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 2061 "gdk.override"
static int
_wrap_gdk_colormap_alloc_color(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* 
     static char *kwlist1[] = { "red", "green", "blue", "writeable", "best_match", NULL };
     static char *kwlist2[] = { "spec", "writeable", "best_match", NULL };
  */
  int i;
  GdkColor colour = { 0, 0, 0, 0 };
  gboolean writeable = FALSE;
  gboolean best_match = TRUE;
  nsp_option opts[] ={{ "writeable",s_bool,NULLOBJ,-1},
		      { "best_match",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,3);
  if ( IsMatObj(stack,1)) 
    {
      NspMatrix *colours;
      int_types T[] = { realmat , new_opts, t_end};
      if (GetArgs(stack,rhs,opt,T, &colours, &opts, &writeable, &best_match) == FAIL ) 
	return RET_BUG;
      CheckCols(stack.fname,1,colours,3);
      for ( i= 0 ; i < colours->m ; i++)
	{
	  colour.red = colours->R[i];
	  colour.green = colours->R[i+colours->m];
	  colour.blue = colours->R[i+2*colours->m];
	  if (!gdk_colormap_alloc_color(GDK_COLORMAP(self->obj),
					&colour, writeable, best_match))
	    {
	      Scierror("%s: couldn't allocate colour number %d",stack.fname,i);
	      return RET_BUG;
	    }
	}
    }
  else if ( IsSMatObj(stack,1)) 
    {
      NspSMatrix *colours=NULL;
      int_types T[] = { smat , new_opts, t_end};
      if (GetArgs(stack,rhs,opt,T, &colours,&opts, &writeable, &best_match) == FAIL )
	return RET_BUG;
      for ( i= 0 ; i < colours->m ; i++) 
	{
	  if (!gdk_color_parse( colours->S[i], &colour))
	    {
	      Scierror("%s: unable to parse colour specification %s\n",stack.fname,colours->S[i]);
	      return RET_BUG;
	    }
	  if (!gdk_colormap_alloc_color(GDK_COLORMAP(self->obj),
					&colour, writeable, best_match))
	    {
	      Scierror("%s: couldn't allocate colour %s\n",stack.fname,colours->S[i]);
	      return RET_BUG;
	    }
	}
    }
  else {
    Scierror("%s: first arhument must be a String or scalar matrix \n", stack.fname);
    return RET_BUG;
  }
  return 0;
}
#line 2315 "gdk.c"


static int _wrap_gdk_colormap_get_visual(NspGdkColormap *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkVisual *ret;

  ret = gdk_colormap_get_visual(GDK_COLORMAP(self->obj));
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_color_white(NspGdkColormap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  int ret;
  NspObject *nsp_color;
  GdkColor *color = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_COLOR))
      color = nspg_boxed_get(nsp_color, GdkColor);
  else {
      Scierror( "color should be a GdkColor");
      return RET_BUG;
  }
  ret = gdk_color_white(GDK_COLORMAP(self->obj), color);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_color_black(NspGdkColormap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  int ret;
  NspObject *nsp_color;
  GdkColor *color = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_COLOR))
      color = nspg_boxed_get(nsp_color, GdkColor);
  else {
      Scierror( "color should be a GdkColor");
      return RET_BUG;
  }
  ret = gdk_color_black(GDK_COLORMAP(self->obj), color);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 2126 "gdk.override"
static int
_wrap_gdk_color_alloc(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  Scierror("use GdkColormap.alloc_color"); 
  return RET_BUG;
}
#line 2375 "gdk.c"


static int _wrap_gdk_color_change(NspGdkColormap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  int ret;
  NspObject *nsp_color;
  GdkColor *color = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_COLOR))
      color = nspg_boxed_get(nsp_color, GdkColor);
  else {
      Scierror( "color should be a GdkColor");
      return RET_BUG;
  }
  ret = gdk_color_change(GDK_COLORMAP(self->obj), color);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdkcolormap_methods[] = {
  {"alloc_color",(nsp_method *) _wrap_gdk_colormap_alloc_color},
  {"get_visual",(nsp_method *) _wrap_gdk_colormap_get_visual},
  {"white",(nsp_method *) _wrap_gdk_color_white},
  {"black",(nsp_method *) _wrap_gdk_color_black},
  {"alloc",(nsp_method *) _wrap_gdk_color_alloc},
  {"change",(nsp_method *) _wrap_gdk_color_change},
  { NULL, NULL}
};

static NspMethods *gdkcolormap_get_methods(void) { return gdkcolormap_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkcolormap_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkDevice ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkDevice_Private 
#include "nsp/gtk/gdkdevice.h"
#include "nsp/interf.h"

/* NspGdkDevice inherits from NspGObject */ 

int nsp_type_gdkdevice_id=0;
NspTypeGdkDevice *nsp_type_gdkdevice=NULL;

NspTypeGdkDevice *new_type_gdkdevice(type_mode mode)
{
  NspTypeGdkDevice *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdevice != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdevice;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdevice_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkdevice_get_methods; 
  type->new = (new_func *) new_gdkdevice;

  /* specific methods for gdkdevice */
      
  type->init = (init_func *) init_gdkdevice;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkdevice */ 

  top->s_type =  (s_type_func *) gdkdevice_type_as_string;    
  top->sh_type = (sh_type_func *) gdkdevice_type_short_string;
  /* top->create = (create_func*) int_gdkdevice_create;*/ 
  
  /* specific methods for gdkdevice */
      
  type->init = (init_func *) init_gdkdevice;

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
 * initialize GdkDevice instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdevice(NspGdkDevice *o,NspTypeGdkDevice *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkDevice 
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
 * Object method redefined for GdkDevice 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkdevice_type_name[]="GdkDevice";
static char gdkdevice_short_type_name[]="GdkDevice";

static char *gdkdevice_type_as_string(void)
{
  return(gdkdevice_type_name);
}

static char *gdkdevice_type_short_string(void)
{
  return(gdkdevice_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkDevice objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkDevice   *gdkdevice_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkdevice_id) ) return ((NspGdkDevice *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdevice));
  return NULL;
}

int IsGdkDeviceObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkdevice_id);
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
  if (( M = gdkdevice_object(NthObj(i))) == NULLGDKDEVICE)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkDevice *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkdevice is initialized * /
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkdevice)) == NULLGDKDEVICE) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_gdk_device_set_source(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_source = NULL;
  GdkInputSource source;

  if ( GetArgs(stack,rhs,opt,T,&nsp_source) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INPUT_SOURCE, nsp_source, (gint *)&source)== FAIL)
      return RET_BUG;
  gdk_device_set_source(GDK_DEVICE(self->obj), source);
  return 0;
}

static int _wrap_gdk_device_set_mode(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  int ret;
  GdkInputMode mode;
  NspObject *nsp_mode = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_mode) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INPUT_MODE, nsp_mode, (gint *)&mode)== FAIL)
      return RET_BUG;
  ret = gdk_device_set_mode(GDK_DEVICE(self->obj), mode);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_device_set_key(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, obj,t_end};
  int index, keyval;
  NspObject *nsp_modifiers = NULL;
  GdkModifierType modifiers;

  if ( GetArgs(stack,rhs,opt,T,&index, &keyval, &nsp_modifiers) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_MODIFIER_TYPE, nsp_modifiers, (gint *)&modifiers)==FAIL)
      return RET_BUG;
  gdk_device_set_key(GDK_DEVICE(self->obj), index, keyval, modifiers);
  return 0;
}

static int _wrap_gdk_device_set_axis_use(NspGdkDevice *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, obj,t_end};
  int index;
  GdkAxisUse use;
  NspObject *nsp_use = NULL;

  if ( GetArgs(stack,rhs,opt,T,&index, &nsp_use) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_AXIS_USE, nsp_use, (gint *)&use)== FAIL)
      return RET_BUG;
  gdk_device_set_axis_use(GDK_DEVICE(self->obj), index, use);
  return 0;
}

#line 1124 "gdk.override"
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
    Scierror( "window should be a GdkWindow");
    return RET_BUG;
  }
  if (( axes = nsp_matrix_create(NVOID,'r',1,device->num_axes)) == NULLMAT) return RET_BUG;
  gdk_device_get_state(device, GDK_WINDOW(window->obj), axes->R, &mask);
  MoveObj(stack,1,(NspObject *) axes);
  if ( lhs == 2 ) 
    if (( nsp_move_double(stack,2,(double) mask))==FAIL) return RET_BUG;
  return 2;
}
#line 2686 "gdk.c"


#line 1149 "gdk.override"
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
    Scierror( "window should be a GdkWindow");
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
  Scierror("To be done XXXX ");
  return 0;
}
#line 2729 "gdk.c"


#line 1190 "gdk.override"
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

  if (GetArgs(stack,rhs,opt,T, &nsp_axes, &use) == FAIL ) return RET_BUG;

  CheckLength(stack,1,nsp_axes, device->num_axes); 

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
#line 2759 "gdk.c"


static NspMethods gdkdevice_methods[] = {
  {"set_source",(nsp_method *) _wrap_gdk_device_set_source},
  {"set_mode",(nsp_method *) _wrap_gdk_device_set_mode},
  {"set_key",(nsp_method *) _wrap_gdk_device_set_key},
  {"set_axis_use",(nsp_method *) _wrap_gdk_device_set_axis_use},
  {"get_state",(nsp_method *) _wrap_gdk_device_get_state},
  {"get_history",(nsp_method *) _wrap_gdk_device_get_history},
  {"get_axis",(nsp_method *) _wrap_gdk_device_get_axis},
  { NULL, NULL}
};

static NspMethods *gdkdevice_get_methods(void) { return gdkdevice_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_device__get_name(NspObject *self,char *attr)
{
  const gchar *ret;
  NspObject *nsp_ret;

  ret = (gchar*) GDK_DEVICE(NSP_GOBJECT_GET(self))->name;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static NspObject *_wrap_gdk_device__get_source(NspObject *self,char *attr)
{
  gint ret;

  ret = (GdkInputSource) GDK_DEVICE(NSP_GOBJECT_GET(self))->source;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_device__get_mode(NspObject *self,char *attr)
{
  gint ret;

  ret = (GdkInputMode) GDK_DEVICE(NSP_GOBJECT_GET(self))->mode;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_device__get_has_cursor(NspObject *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = (gboolean) GDK_DEVICE(NSP_GOBJECT_GET(self))->has_cursor;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static NspObject *_wrap_gdk_device__get_num_axes(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_DEVICE(NSP_GOBJECT_GET(self))->num_axes;
  return nsp_new_double_obj((double) ret);
}

#line 1089 "gdk.override"
static NspObject *
_wrap_gdk_device__get_axes(NspGObject *self, char *attr)
{
  GdkDevice *device = GDK_DEVICE(self->obj);
  gint i;
  NspMatrix *ret ; 
  if ((ret = nsp_matrix_create(NVOID,'r',device->num_axes,3)) == NULLMAT) return NULLOBJ;
								    
  for (i = 0; i < device->num_axes; i++)
    {
      ret->R[i] = device->axes[i].use;
      ret->R[i +  device->num_axes] = device->axes[i].min;
      ret->R[i + 2*device->num_axes] = device->axes[i].max;
    }
  return (NspObject *) ret;
}
#line 2839 "gdk.c"
static NspObject *_wrap_gdk_device__get_num_keys(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_DEVICE(NSP_GOBJECT_GET(self))->num_keys;
  return nsp_new_double_obj((double) ret);
}

#line 1107 "gdk.override"
static NspObject *
_wrap_gdk_device__get_keys(NspGObject *self, char *attr)
{
  GdkDevice *device = GDK_DEVICE(self->obj);
  gint i;
  NspMatrix *ret ; 

  if ((ret = nsp_matrix_create(NVOID,'r',device->num_keys,2)) == NULLMAT) return NULL;
  for (i = 0; i < device->num_keys; i++)
    {
      ret->R[i] = device->keys[i].keyval;
      ret->R[i +  device->num_keys] = device->keys[i].modifiers;
    }
  return (NspObject *) ret;
}
#line 2864 "gdk.c"
static AttrTab gdkdevice_attrs[] = {
  { "name", (attr_get_function *)_wrap_gdk_device__get_name, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "source", (attr_get_function *)_wrap_gdk_device__get_source, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "mode", (attr_get_function *)_wrap_gdk_device__get_mode, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "has_cursor", (attr_get_function *)_wrap_gdk_device__get_has_cursor, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "num_axes", (attr_get_function *)_wrap_gdk_device__get_num_axes, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "axes", (attr_get_function *)_wrap_gdk_device__get_axes, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "num_keys", (attr_get_function *)_wrap_gdk_device__get_num_keys, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "keys", (attr_get_function *)_wrap_gdk_device__get_keys, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- GdkDragContext ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkDragContext_Private 
#include "nsp/gtk/gdkdragcontext.h"
#include "nsp/interf.h"

/* NspGdkDragContext inherits from NspGObject */ 

int nsp_type_gdkdragcontext_id=0;
NspTypeGdkDragContext *nsp_type_gdkdragcontext=NULL;

NspTypeGdkDragContext *new_type_gdkdragcontext(type_mode mode)
{
  NspTypeGdkDragContext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdragcontext != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdragcontext;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdragcontext_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkdragcontext_get_methods; 
  type->new = (new_func *) new_gdkdragcontext;

  /* specific methods for gdkdragcontext */
      
  type->init = (init_func *) init_gdkdragcontext;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkdragcontext */ 

  top->s_type =  (s_type_func *) gdkdragcontext_type_as_string;    
  top->sh_type = (sh_type_func *) gdkdragcontext_type_short_string;
  /* top->create = (create_func*) int_gdkdragcontext_create;*/ 
  
  /* specific methods for gdkdragcontext */
      
  type->init = (init_func *) init_gdkdragcontext;

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
 * initialize GdkDragContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdragcontext(NspGdkDragContext *o,NspTypeGdkDragContext *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkDragContext 
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
 * Object method redefined for GdkDragContext 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkdragcontext_type_name[]="GdkDragContext";
static char gdkdragcontext_short_type_name[]="GdkDragContext";

static char *gdkdragcontext_type_as_string(void)
{
  return(gdkdragcontext_type_name);
}

static char *gdkdragcontext_type_short_string(void)
{
  return(gdkdragcontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkDragContext objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkDragContext   *gdkdragcontext_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkdragcontext_id) ) return ((NspGdkDragContext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdragcontext));
  return NULL;
}

int IsGdkDragContextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkdragcontext_id);
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
  if (( M = gdkdragcontext_object(NthObj(i))) == NULLGDKDRAGCONTEXT)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkDragContext *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkdragcontext is initialized * /
  nsp_type_gdkdragcontext = new_type_gdkdragcontext(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkdragcontext)) == NULLGDKDRAGCONTEXT) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_gdkdragcontext_new(Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gdk_drag_context_new())== NULL) return RET_BUG;

  nsp_type_gdkdragcontext = new_type_gdkdragcontext(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkdragcontext );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_drag_status(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,new_opts,t_end};
  nsp_option opts[] = {
	{"time",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  NspObject *nsp_action = NULL;
  gulong time = GDK_CURRENT_TIME;
  GdkDragAction action;

  if ( GetArgs(stack,rhs,opt,T,&nsp_action, opts, &time) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_DRAG_ACTION, nsp_action, (gint *)&action)==FAIL)
      return RET_BUG;
  gdk_drag_status(GDK_DRAG_CONTEXT(self->obj), action, time);
  return 0;
}

static int _wrap_gdk_drop_reply(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,new_opts,t_end};
  nsp_option opts[] = {
	{"time",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  int ok;
  gulong time = GDK_CURRENT_TIME;

  if ( GetArgs(stack,rhs,opt,T,&ok, opts, &time) == FAIL) return RET_BUG;
  gdk_drop_reply(GDK_DRAG_CONTEXT(self->obj), ok, time);
  return 0;
}

static int _wrap_gdk_drop_finish(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,new_opts,t_end};
  nsp_option opts[] = {
	{"time",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  int success;
  gulong time = GDK_CURRENT_TIME;

  if ( GetArgs(stack,rhs,opt,T,&success, opts, &time) == FAIL) return RET_BUG;
  gdk_drop_finish(GDK_DRAG_CONTEXT(self->obj), success, time);
  return 0;
}

static int _wrap_gdk_drag_get_selection(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkAtom ret;

  ret = gdk_drag_get_selection(GDK_DRAG_CONTEXT(self->obj));
  if (( nsp_ret = (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL))== NULL);
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 1218 "gdk.override"
static int
_wrap_gdk_drag_find_window(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "drag_window", "x_root", "y_root", NULL };*/
  NspGObject *drag_window;
  gint x_root, y_root;
  GdkWindow *dest_window;
  GdkDragProtocol protocol;
  NspObject *ret;
  int_types T[] = {obj, s_int,s_int, t_end};

  CheckLhs(2,2);
  if (GetArgs(stack,rhs,opt,T, &drag_window, &x_root, &y_root) == FAIL )
    return RET_BUG;
  if (!nspgobject_check(drag_window,(NspTypeBase *) &nsp_type_gdkwindow)) {
    Scierror( "drag_window must be a GdkWindow");
    return RET_BUG;
  }
  gdk_drag_find_window(GDK_DRAG_CONTEXT(self->obj),
		       GDK_WINDOW(drag_window->obj), x_root, y_root,
		       &dest_window, &protocol);
  if ((ret = (NspObject *) nspgobject_new((GObject *)dest_window)) == NULL)  return RET_BUG;
  MoveObj(stack,1,ret);
  if ( nsp_move_double(stack,2,(double) protocol)== FAIL) return RET_BUG;
  return 2;
}
#line 3170 "gdk.c"


static int _wrap_gdk_drag_motion(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj, s_int, s_int, obj, obj, s_int,t_end};
  NspObject *nsp_protocol = NULL, *nsp_suggested_action = NULL, *nsp_possible_actions = NULL;
  int x_root, y_root, ret;
  NspGObject *dest_window;
  gulong time;
  GdkDragProtocol protocol;
  GdkDragAction suggested_action, possible_actions;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &dest_window, &nsp_protocol, &x_root, &y_root, &nsp_suggested_action, &nsp_possible_actions, &time) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_DRAG_PROTOCOL, nsp_protocol, (gint *)&protocol)== FAIL)
      return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_DRAG_ACTION, nsp_suggested_action, (gint *)&suggested_action)==FAIL)
      return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_DRAG_ACTION, nsp_possible_actions, (gint *)&possible_actions)==FAIL)
      return RET_BUG;
  ret = gdk_drag_motion(GDK_DRAG_CONTEXT(self->obj), GDK_WINDOW(dest_window->obj), protocol, x_root, y_root, suggested_action, possible_actions, time);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_drag_drop(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  gulong time;

  if ( GetArgs(stack,rhs,opt,T,&time) == FAIL) return RET_BUG;
  gdk_drag_drop(GDK_DRAG_CONTEXT(self->obj), time);
  return 0;
}

static int _wrap_gdk_drag_abort(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  gulong time;

  if ( GetArgs(stack,rhs,opt,T,&time) == FAIL) return RET_BUG;
  gdk_drag_abort(GDK_DRAG_CONTEXT(self->obj), time);
  return 0;
}

static int _wrap_gtk_drag_finish(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, s_bool,new_opts,t_end};
  nsp_option opts[] = {
	{"time",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  int success, del;
  gulong time = GDK_CURRENT_TIME;

  if ( GetArgs(stack,rhs,opt,T,&success, &del, opts, &time) == FAIL) return RET_BUG;
  gtk_drag_finish(GDK_DRAG_CONTEXT(self->obj), success, del, time);
  return 0;
}

static int _wrap_gtk_drag_get_source_widget(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GtkWidget *ret;

  ret = gtk_drag_get_source_widget(GDK_DRAG_CONTEXT(self->obj));
  nsp_type_gtkwidget = new_type_gtkwidget(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtkwidget))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_drag_set_icon_widget(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int,t_end};
  int hot_x, hot_y;
  NspGObject *widget;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtkwidget, &widget, &hot_x, &hot_y) == FAIL) return RET_BUG;
  gtk_drag_set_icon_widget(GDK_DRAG_CONTEXT(self->obj), GTK_WIDGET(widget->obj), hot_x, hot_y);
  return 0;
}

static int _wrap_gtk_drag_set_icon_pixmap(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check, obj_check, s_int, s_int,t_end};
  int hot_x, hot_y;
  NspGObject *colormap, *pixmap, *mask;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkcolormap, &colormap, &nsp_type_gdkpixmap, &pixmap, &nsp_type_gdkbitmap, &mask, &hot_x, &hot_y) == FAIL) return RET_BUG;
  gtk_drag_set_icon_pixmap(GDK_DRAG_CONTEXT(self->obj), GDK_COLORMAP(colormap->obj), GDK_PIXMAP(pixmap->obj), GDK_DRAWABLE(mask->obj), hot_x, hot_y);
  return 0;
}

static int _wrap_gtk_drag_set_icon_pixbuf(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int,t_end};
  int hot_x, hot_y;
  NspGObject *pixbuf;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &pixbuf, &hot_x, &hot_y) == FAIL) return RET_BUG;
  gtk_drag_set_icon_pixbuf(GDK_DRAG_CONTEXT(self->obj), GDK_PIXBUF(pixbuf->obj), hot_x, hot_y);
  return 0;
}

static int _wrap_gtk_drag_set_icon_default(NspGdkDragContext *self,Stack stack,int rhs,int opt,int lhs)
{
  gtk_drag_set_icon_default(GDK_DRAG_CONTEXT(self->obj));
  return 0;
}

static NspMethods gdkdragcontext_methods[] = {
  {"drag_status",(nsp_method *) _wrap_gdk_drag_status},
  {"drop_reply",(nsp_method *) _wrap_gdk_drop_reply},
  {"drop_finish",(nsp_method *) _wrap_gdk_drop_finish},
  {"drag_get_selection",(nsp_method *) _wrap_gdk_drag_get_selection},
  {"drag_find_window",(nsp_method *) _wrap_gdk_drag_find_window},
  {"drag_motion",(nsp_method *) _wrap_gdk_drag_motion},
  {"drag_drop",(nsp_method *) _wrap_gdk_drag_drop},
  {"drag_abort",(nsp_method *) _wrap_gdk_drag_abort},
  {"finish",(nsp_method *) _wrap_gtk_drag_finish},
  {"get_source_widget",(nsp_method *) _wrap_gtk_drag_get_source_widget},
  {"set_icon_widget",(nsp_method *) _wrap_gtk_drag_set_icon_widget},
  {"set_icon_pixmap",(nsp_method *) _wrap_gtk_drag_set_icon_pixmap},
  {"set_icon_pixbuf",(nsp_method *) _wrap_gtk_drag_set_icon_pixbuf},
  {"set_icon_default",(nsp_method *) _wrap_gtk_drag_set_icon_default},
  { NULL, NULL}
};

static NspMethods *gdkdragcontext_get_methods(void) { return gdkdragcontext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_drag_context__get_protocol(NspObject *self,char *attr)
{
  gint ret;

  ret = (GdkDragProtocol) GDK_DRAG_CONTEXT(NSP_GOBJECT_GET(self))->protocol;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_drag_context__get_is_source(NspObject *self,char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = (gboolean) GDK_DRAG_CONTEXT(NSP_GOBJECT_GET(self))->is_source;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static NspObject *_wrap_gdk_drag_context__get_source_window(NspObject *self,char *attr)
{
  GdkWindow *ret;

  ret = (GdkWindow*) GDK_DRAG_CONTEXT(NSP_GOBJECT_GET(self))->source_window;
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  return (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow);
}

static NspObject *_wrap_gdk_drag_context__get_dest_window(NspObject *self,char *attr)
{
  GdkWindow *ret;

  ret = (GdkWindow*) GDK_DRAG_CONTEXT(NSP_GOBJECT_GET(self))->dest_window;
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  return (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow);
}

#line 1246 "gdk.override"
static NspObject *
_wrap_gdk_drag_context__get_targets(NspGObject *self, char *attr)
{
  NspObject *atom; 
  NspList *ret;
  GList *tmp;
  Cell *cloc = NULLCELL, *cloc1; 
  
  if (( ret =nsp_list_create(NVOID,NULL) ) == NULLLIST) return NULL;
  for (tmp = GDK_DRAG_CONTEXT(self->obj)->targets; tmp; tmp = tmp->next) {
    if ((atom = (NspObject *) gdkatom_create(NVOID,NULL,GDK_POINTER_TO_ATOM(tmp->data),NULL)) == NULL)  goto clean; 
    if ((cloc1 =nsp_cell_create(NVOID,atom))== NULLCELL) goto clean;
    cloc1->next = NULLCELL; 
    if ( cloc == NULLCELL) 
      { 
	ret->first = cloc1;
      } 
    else
      { 
	cloc1->prev = cloc; cloc->next = cloc1; 
      } 
    cloc= cloc1;
  }
  return (NspObject *) ret; 
  clean : 
    {
 nsp_list_destroy(ret); 
      return NULL;
    }
}
#line 3370 "gdk.c"
static NspObject *_wrap_gdk_drag_context__get_actions(NspObject *self,char *attr)
{
  guint ret;

  ret = (GdkDragAction) GDK_DRAG_CONTEXT(NSP_GOBJECT_GET(self))->actions;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_drag_context__get_suggested_action(NspObject *self,char *attr)
{
  guint ret;

  ret = (GdkDragAction) GDK_DRAG_CONTEXT(NSP_GOBJECT_GET(self))->suggested_action;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_drag_context__get_action(NspObject *self,char *attr)
{
  guint ret;

  ret = (GdkDragAction) GDK_DRAG_CONTEXT(NSP_GOBJECT_GET(self))->action;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_drag_context__get_start_time(NspObject *self,char *attr)
{
  NspObject *nsp_ret;
  gulong ret;

  ret = (guint32) GDK_DRAG_CONTEXT(NSP_GOBJECT_GET(self))->start_time;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static AttrTab gdkdragcontext_attrs[] = {
  { "protocol", (attr_get_function *)_wrap_gdk_drag_context__get_protocol, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "is_source", (attr_get_function *)_wrap_gdk_drag_context__get_is_source, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "source_window", (attr_get_function *)_wrap_gdk_drag_context__get_source_window, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "dest_window", (attr_get_function *)_wrap_gdk_drag_context__get_dest_window, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "targets", (attr_get_function *)_wrap_gdk_drag_context__get_targets, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "actions", (attr_get_function *)_wrap_gdk_drag_context__get_actions, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "suggested_action", (attr_get_function *)_wrap_gdk_drag_context__get_suggested_action, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "action", (attr_get_function *)_wrap_gdk_drag_context__get_action, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "start_time", (attr_get_function *)_wrap_gdk_drag_context__get_start_time, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- GdkDrawable ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkDrawable_Private 
#include "nsp/gtk/gdkdrawable.h"
#include "nsp/interf.h"

/* NspGdkDrawable inherits from NspGObject */ 

int nsp_type_gdkdrawable_id=0;
NspTypeGdkDrawable *nsp_type_gdkdrawable=NULL;

NspTypeGdkDrawable *new_type_gdkdrawable(type_mode mode)
{
  NspTypeGdkDrawable *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdrawable != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdrawable;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdrawable_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkdrawable_get_methods; 
  type->new = (new_func *) new_gdkdrawable;

  /* specific methods for gdkdrawable */
      
  type->init = (init_func *) init_gdkdrawable;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkdrawable */ 

  top->s_type =  (s_type_func *) gdkdrawable_type_as_string;    
  top->sh_type = (sh_type_func *) gdkdrawable_type_short_string;
  /* top->create = (create_func*) int_gdkdrawable_create;*/ 
  
  /* specific methods for gdkdrawable */
      
  type->init = (init_func *) init_gdkdrawable;

  if ( nsp_type_gdkdrawable_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkDrawable called nsp_type_gdkdrawable
       */
      type->id =  nsp_type_gdkdrawable_id = nsp_new_type_id();
      nsp_type_gdkdrawable = type;
      if ( nsp_register_type(nsp_type_gdkdrawable) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkdrawable, GDK_TYPE_DRAWABLE);
      return ( mode == T_BASE ) ? type : new_type_gdkdrawable(mode);
    }
  else 
    {
       type->id = nsp_type_gdkdrawable_id;
       return type;
    }
}

/*
 * initialize GdkDrawable instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdrawable(NspGdkDrawable *o,NspTypeGdkDrawable *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkDrawable 
 */

NspGdkDrawable *new_gdkdrawable() 
{
  NspGdkDrawable *loc; 
  /* type must exists */
  nsp_type_gdkdrawable = new_type_gdkdrawable(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkDrawable)))== NULLGDKDRAWABLE) return loc;
  /* initialize object */
  if ( init_gdkdrawable(loc,nsp_type_gdkdrawable) == FAIL) return NULLGDKDRAWABLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GdkDrawable 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkdrawable_type_name[]="GdkDrawable";
static char gdkdrawable_short_type_name[]="GdkDrawable";

static char *gdkdrawable_type_as_string(void)
{
  return(gdkdrawable_type_name);
}

static char *gdkdrawable_type_short_string(void)
{
  return(gdkdrawable_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkDrawable objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkDrawable   *gdkdrawable_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkdrawable_id) ) return ((NspGdkDrawable *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdrawable));
  return NULL;
}

int IsGdkDrawableObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkdrawable_id);
}

int IsGdkDrawable(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkdrawable_id);
}

NspGdkDrawable  *GetGdkDrawableCopy(Stack stack, int i)
{
  if (  GetGdkDrawable(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkDrawable  *GetGdkDrawable(Stack stack, int i)
{
  NspGdkDrawable *M;
  if (( M = gdkdrawable_object(NthObj(i))) == NULLGDKDRAWABLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkDrawable *gdkdrawable_copy(NspGdkDrawable *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdrawable);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkdrawable);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkDrawable
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkDrawable *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkdrawable is initialized * /
  nsp_type_gdkdrawable = new_type_gdkdrawable(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkdrawable)) == NULLGDKDRAWABLE) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 1604 "gdk.override"
static int
_wrap_gdk_drawable_get_size(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint width;
  gint height;
  
  gdk_drawable_get_size(GDK_DRAWABLE(self->obj), &width, &height);
  if ( nsp_move_doubles(stack,1,1,2,(double) width, (double) height) == FAIL) return RET_BUG; 
  return 1;
}
#line 3624 "gdk.c"


static int _wrap_gdk_drawable_set_colormap(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *colormap;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkcolormap, &colormap) == FAIL) return RET_BUG;
  gdk_drawable_set_colormap(GDK_DRAWABLE(self->obj), GDK_COLORMAP(colormap->obj));
  return 0;
}

static int _wrap_gdk_drawable_get_colormap(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkColormap *ret;

  ret = gdk_drawable_get_colormap(GDK_DRAWABLE(self->obj));
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_drawable_get_visual(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkVisual *ret;

  ret = gdk_drawable_get_visual(GDK_DRAWABLE(self->obj));
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_drawable_get_depth(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_drawable_get_depth(GDK_DRAWABLE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_draw_point(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int,t_end};
  int x, y;
  NspGObject *gc;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &x, &y) == FAIL) return RET_BUG;
  gdk_draw_point(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), x, y);
  return 0;
}

static int _wrap_gdk_draw_line(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int, s_int, s_int,t_end};
  int x1, y1, x2, y2;
  NspGObject *gc;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &x1, &y1, &x2, &y2) == FAIL) return RET_BUG;
  gdk_draw_line(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), x1, y1, x2, y2);
  return 0;
}

static int _wrap_gdk_draw_rectangle(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_bool, s_int, s_int, s_int, s_int,t_end};
  int filled, x, y, width, height;
  NspGObject *gc;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &filled, &x, &y, &width, &height) == FAIL) return RET_BUG;
  gdk_draw_rectangle(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), filled, x, y, width, height);
  return 0;
}

static int _wrap_gdk_draw_arc(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_bool, s_int, s_int, s_int, s_int, s_int, s_int,t_end};
  int filled, x, y, width, height, angle1, angle2;
  NspGObject *gc;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &filled, &x, &y, &width, &height, &angle1, &angle2) == FAIL) return RET_BUG;
  gdk_draw_arc(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), filled, x, y, width, height, angle1, angle2);
  return 0;
}

#line 136 "gdk.override"
static int
_wrap_gdk_draw_polygon(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "gc", "filled", "points", NULL }; */

  int_types T[] = { obj_check, s_bool, realmat ,t_end};
  NspGObject *gc;
  NspMatrix *nsp_points;
  gint filled, npoints, i;
  GdkPoint *points;

  if (GetArgs(stack,rhs,opt,T, &nsp_type_gdkgc, &gc, &filled, &nsp_points) == FAIL) 
    return RET_BUG;
  CheckCols(stack.fname,3,nsp_points,2);

  npoints = nsp_points->m;
  points = g_new(GdkPoint, npoints);
  for (i = 0; i < npoints ; i++) {
    points[i].x = nsp_points->R[i] ; 
    points[i].y = nsp_points->R[i +nsp_points->m ];
    }
  gdk_draw_polygon(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), filled,points, npoints);
  g_free(points);
  return 0;
}
#line 3740 "gdk.c"


static int _wrap_gdk_draw_string(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, obj_check, s_int, s_int, string,t_end};
  GdkFont *font = NULL;
  NspGObject *gc;
  int x, y;
  NspObject *nsp_font;
  char *string;

  if ( GetArgs(stack,rhs,opt,T,&nsp_font, &nsp_type_gdkgc, &gc, &x, &y, &string) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_font, GDK_TYPE_FONT))
      font = nspg_boxed_get(nsp_font, GdkFont);
  else {
      Scierror( "font should be a GdkFont");
      return RET_BUG;
  }
  gdk_draw_string(GDK_DRAWABLE(self->obj), font, GDK_GC(gc->obj), x, y, string);
  return 0;
}

#line 163 "gdk.override"
static int
_wrap_gdk_draw_text(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "font", "gc", "x", "y", "text",NULL};*/
  NspGObject *gc;
  NspObject *font;
  int x, y; 
  char *text;
  int_types T[] = {obj, obj_check, s_int, s_int, string ,t_end};

  if (GetArgs(stack,rhs,opt,T, &font, &nsp_type_gdkgc, &gc, &x, &y, &text ) == FAIL )
    return RET_BUG;
  if (!nspg_boxed_check(font, GDK_TYPE_FONT)) {
    Scierror( "font must be a GdkFont");
    return RET_BUG;
  }
  gdk_draw_text(GDK_DRAWABLE(self->obj), nspg_boxed_get(font, GdkFont),
		GDK_GC(gc->obj), x, y, text, strlen(text));
  return 0;
}
#line 3784 "gdk.c"


static int _wrap_gdk_draw_drawable(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check, s_int, s_int, s_int, s_int, s_int, s_int,t_end};
  int xsrc, ysrc, xdest, ydest, width, height;
  NspGObject *gc, *src;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &nsp_type_gdkdrawable, &src, &xsrc, &ysrc, &xdest, &ydest, &width, &height) == FAIL) return RET_BUG;
  gdk_draw_drawable(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), GDK_DRAWABLE(src->obj), xsrc, ysrc, xdest, ydest, width, height);
  return 0;
}

static int _wrap_gdk_draw_image(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check, s_int, s_int, s_int, s_int, s_int, s_int,t_end};
  int xsrc, ysrc, xdest, ydest, width, height;
  NspGObject *gc, *image;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &nsp_type_gdkimage, &image, &xsrc, &ysrc, &xdest, &ydest, &width, &height) == FAIL) return RET_BUG;
  gdk_draw_image(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), GDK_IMAGE(image->obj), xsrc, ysrc, xdest, ydest, width, height);
  return 0;
}

#line 185 "gdk.override"
static int
_wrap_gdk_draw_points(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "gc", "points", NULL };*/
  NspGObject *gc;
  NspMatrix *nsp_points;
  gint npoints, i;
  GdkPoint *points;
  int_types T[] = {obj_check, realmat ,t_end};
  if (GetArgs(stack,rhs,opt,T,
	      /* "O!O:GdkDrawable.draw_points" */
	      &nsp_type_gdkgc, &gc, &nsp_points) == FAIL )
    return RET_BUG;

  CheckCols(stack.fname,2,nsp_points,2);

  npoints = nsp_points->m;
  points = g_new(GdkPoint, npoints);
  for (i = 0; i < npoints ; i++) {
    points[i].x = nsp_points->R[i] ; 
    points[i].y = nsp_points->R[i +nsp_points->m ];
    }
  gdk_draw_points(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), points, npoints);
  g_free(points);
  return 0;
}
#line 3836 "gdk.c"


#line 213 "gdk.override"
static int
_wrap_gdk_draw_segments(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "gc", "segs", NULL };*/
  NspGObject *gc;
  NspMatrix *nsp_xsegs;
  NspMatrix *nsp_ysegs;
  gint nsegs, i;
  GdkSegment *segs;
  int_types T[] = {obj_check, realmat , realmat , t_end};
  if (GetArgs(stack,rhs,opt,T, &nsp_type_gdkgc, &gc, &nsp_xsegs,&nsp_ysegs) == FAIL )
    return RET_BUG;
  CheckSameDims(stack.fname,2,3,nsp_xsegs,nsp_ysegs);
  CheckCols(stack.fname,2,nsp_xsegs,2);
  CheckCols(stack.fname,3,nsp_ysegs,2);

  nsegs = nsp_xsegs->mn/2; 
  segs = g_new(GdkSegment, nsegs);
  for (i = 0; i < nsegs; i++) {
    segs[i].x1 = nsp_xsegs->R[2*i];
    segs[i].y1 = nsp_ysegs->R[2*i+1];
    segs[i].x2 = nsp_xsegs->R[2*i] ;
    segs[i].y2 = nsp_ysegs->R[2*i+1];
  }
  gdk_draw_segments(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), segs, nsegs);
  g_free(segs);
  return 0;
}
#line 3868 "gdk.c"


#line 243 "gdk.override"
static int
_wrap_gdk_draw_lines(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "gc", "points", NULL };*/
  NspGObject *gc;
  NspMatrix *nsp_points;
  gint npoints, i;
  GdkPoint *points;
  int_types T[] = {obj_check, realmat ,t_end};
  if (GetArgs(stack,rhs,opt,T, &nsp_type_gdkgc, &gc, &nsp_points) == FAIL )
    return RET_BUG;
  CheckCols(stack.fname,2,nsp_points,2);
  npoints = nsp_points->m;
  points = g_new(GdkPoint, npoints);
  for (i = 0; i < npoints ; i++) {
    points[i].x = nsp_points->R[i] ; 
    points[i].y = nsp_points->R[i +nsp_points->m ];
    }
  gdk_draw_lines(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), points, npoints);
  g_free(points);
  return 0;
}
#line 3894 "gdk.c"


static int _wrap_gdk_draw_glyphs(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check, s_int, s_int, obj,t_end};
  int x, y;
  NspGObject *gc, *font;
  PangoGlyphString *glyphs = NULL;
  NspObject *nsp_glyphs;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &nsp_type_pangofont, &font, &x, &y, &nsp_glyphs) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_glyphs, PANGO_TYPE_GLYPH_STRING))
      glyphs = nspg_boxed_get(nsp_glyphs, PangoGlyphString);
  else {
      Scierror( "glyphs should be a PangoGlyphString");
      return RET_BUG;
  }
  gdk_draw_glyphs(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), PANGO_FONT(font->obj), x, y, glyphs);
  return 0;
}

static int _wrap_gdk_draw_layout(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int, obj_check,t_end};
  int x, y;
  NspGObject *gc, *layout;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &x, &y, &nsp_type_pangolayout, &layout) == FAIL) return RET_BUG;
  gdk_draw_layout(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), x, y, PANGO_LAYOUT(layout->obj));
  return 0;
}

static int _wrap_gdk_drawable_get_image(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int, s_int,t_end};
  int x, y, width, height;
  NspObject *nsp_ret;
  GdkImage *ret;

  if ( GetArgs(stack,rhs,opt,T,&x, &y, &width, &height) == FAIL) return RET_BUG;
  ret = gdk_drawable_get_image(GDK_DRAWABLE(self->obj), x, y, width, height);
  nsp_type_gdkimage = new_type_gdkimage(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkimage))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_image_get(NspGdkDrawable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int, s_int,t_end};
  int x, y, width, height;
  NspObject *nsp_ret;
  GdkImage *ret;

  if ( GetArgs(stack,rhs,opt,T,&x, &y, &width, &height) == FAIL) return RET_BUG;
  Scierror("%s: deprecated use GdkDrawable.get_image",stack.fname); return RET_BUG;
  ret = gdk_image_get(GDK_DRAWABLE(self->obj), x, y, width, height);
  nsp_type_gdkimage = new_type_gdkimage(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkimage))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 267 "gdk.override"
static int
_wrap_gdk_draw_rgb_image(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "gc", "x", "y", "width", "height", "dith",
     "rgb_buf", "rowstride", "xdith", "ydith", NULL };*/
  /* 
  NspGObject *gc;
  NspObject *nsp_dith;
  gint x, y, width, height, rowstride = -1, xdith = 0, ydith = 0, len;
  GdkRgbDither dith;
  guchar *rgb_buf;

  if (GetArgs(stack,rhs,opt,T,
	      / * "O!iiiiOs#|iii:GdkDrawable.draw_rgb_image" * /
	      &nsp_type_gdkgc, &gc, &x, &y,
	      &width, &height, &nsp_dith,
	      &rgb_buf, &len, &rowstride,
	      &xdith, &ydith) == FAIL )
    return RET_BUG;

  if (nspg_enum_get_value(GDK_TYPE_RGB_DITHER, nsp_dith, (gint *)&dith))
    return RET_BUG;
  if (!(width > 0 && height > 0)) {
    NspErr_SetString(NspExc_ValueError,
		     "height and width must be greater than zero");
    return RET_BUG;
  }
  if (rowstride == -1) rowstride = width * 3;
  if (len < rowstride * (height - 1) + width*3) {
    NspErr_SetString(NspExc_IndexError, "rgb_buf is not large enough");
    return RET_BUG;
  }
  gdk_draw_rgb_image_dithalign(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj),
			       x, y, width, height, dith, rgb_buf,
			       rowstride, xdith, ydith);
  */
  Scierror("To be done XXXXXXXXX");
  return 0;
}
#line 3998 "gdk.c"


#line 310 "gdk.override"
static int
_wrap_gdk_draw_rgb_32_image(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "gc", "x", "y", "width", "height", "dith",
     "rgb_buf", "rowstride", "xdith", "ydith", NULL };*/
  /* 
  NspGObject *gc;
  NspObject *nsp_dith;
  gint x, y, width, height, rowstride = -1, xdith = 0, ydith = 0, len;
  GdkRgbDither dith;
  guchar *rgb_buf;

  if (GetArgs(stack,rhs,opt,T,
	      / * "O!iiiiOs#|iii:GdkDrawable.draw_rgb_32_image" * /
	      &nsp_type_gdkgc, &gc, &x, &y,
	      &width, &height, &nsp_dith,
	      &rgb_buf, &len, &rowstride,
	      &xdith, &ydith) == FAIL )
    return RET_BUG;

  if (nspg_enum_get_value(GDK_TYPE_RGB_DITHER, nsp_dith, (gint *)&dith))
    return RET_BUG;
  if (!(width > 0 && height > 0)) {
    NspErr_SetString(NspExc_ValueError,
		     "height and width must be greater than zero");
    return RET_BUG;
  }
  if (rowstride == -1) rowstride = width * 4;
  if (len < rowstride * (height - 1) + width*4) {
    NspErr_SetString(NspExc_IndexError, "rgb_buf is not large enough");
    return RET_BUG;
  }
  gdk_draw_rgb_32_image_dithalign(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj),
				  x, y, width, height, dith, rgb_buf,
				  rowstride, xdith, ydith);
  */
  Scierror("To be done XXXXXX ");  
  return 0;
}
#line 4041 "gdk.c"


#line 353 "gdk.override"
static int
_wrap_gdk_draw_gray_image(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "gc", "x", "y", "width", "height", "dith",
     "buf", "rowstride", NULL };*/
  /* 
  NspGObject *gc;
  NspObject *nsp_dith;
  gint x, y, width, height, rowstride = -1, len;
  GdkRgbDither dith;
  guchar *buf;

  if (GetArgs(stack,rhs,opt,T,
  / * "O!iiiiOs#|i:GdkDrawable.draw_gray_image", * /
	      &nsp_type_gdkgc, &gc, &x, &y,
	      &width, &height, &nsp_dith,
	      &buf, &len, &rowstride) == FAIL )
    return RET_BUG;

  if (nspg_enum_get_value(GDK_TYPE_RGB_DITHER, nsp_dith, (gint *)&dith))
    return RET_BUG;
  if (!(width > 0 && height > 0)) {
    NspErr_SetString(NspExc_ValueError,
		     "height and width must be greater than zero");
    return RET_BUG;
  }
  if (rowstride == -1) rowstride = width;
  if (len < rowstride * (height - 1) + width) {
    NspErr_SetString(NspExc_IndexError, "buf is not large enough");
    return RET_BUG;
  }
  gdk_draw_gray_image(GDK_DRAWABLE(self->obj), GDK_GC(gc->obj), x, y,
		      width, height, dith, buf, rowstride);
  return 0;
  */
  Scierror("To be done XXXXXX ");
  return 0;
}
#line 4083 "gdk.c"


static NspMethods gdkdrawable_methods[] = {
  {"get_size",(nsp_method *) _wrap_gdk_drawable_get_size},
  {"set_colormap",(nsp_method *) _wrap_gdk_drawable_set_colormap},
  {"get_colormap",(nsp_method *) _wrap_gdk_drawable_get_colormap},
  {"get_visual",(nsp_method *) _wrap_gdk_drawable_get_visual},
  {"get_depth",(nsp_method *) _wrap_gdk_drawable_get_depth},
  {"draw_point",(nsp_method *) _wrap_gdk_draw_point},
  {"draw_line",(nsp_method *) _wrap_gdk_draw_line},
  {"draw_rectangle",(nsp_method *) _wrap_gdk_draw_rectangle},
  {"draw_arc",(nsp_method *) _wrap_gdk_draw_arc},
  {"draw_polygon",(nsp_method *) _wrap_gdk_draw_polygon},
  {"draw_string",(nsp_method *) _wrap_gdk_draw_string},
  {"draw_text",(nsp_method *) _wrap_gdk_draw_text},
  {"draw_drawable",(nsp_method *) _wrap_gdk_draw_drawable},
  {"draw_image",(nsp_method *) _wrap_gdk_draw_image},
  {"draw_points",(nsp_method *) _wrap_gdk_draw_points},
  {"draw_segments",(nsp_method *) _wrap_gdk_draw_segments},
  {"draw_lines",(nsp_method *) _wrap_gdk_draw_lines},
  {"draw_glyphs",(nsp_method *) _wrap_gdk_draw_glyphs},
  {"draw_layout",(nsp_method *) _wrap_gdk_draw_layout},
  {"get_image",(nsp_method *) _wrap_gdk_drawable_get_image},
  {"image_get",(nsp_method *) _wrap_gdk_image_get},
  {"draw_rgb_image",(nsp_method *) _wrap_gdk_draw_rgb_image},
  {"draw_rgb_32_image",(nsp_method *) _wrap_gdk_draw_rgb_32_image},
  {"draw_gray_image",(nsp_method *) _wrap_gdk_draw_gray_image},
  { NULL, NULL}
};

static NspMethods *gdkdrawable_get_methods(void) { return gdkdrawable_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkdrawable_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkWindow ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkWindow_Private 
#include "nsp/gtk/gdkwindow.h"
#include "nsp/interf.h"

/* NspGdkWindow inherits from NspGdkDrawable */ 

int nsp_type_gdkwindow_id=0;
NspTypeGdkWindow *nsp_type_gdkwindow=NULL;

NspTypeGdkWindow *new_type_gdkwindow(type_mode mode)
{
  NspTypeGdkWindow *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkwindow != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkwindow;
    }
  if ((type =  malloc(sizeof(NspTypeGdkDrawable))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gdkdrawable(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkwindow_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkwindow_get_methods; 
  type->new = (new_func *) new_gdkwindow;

  /* specific methods for gdkwindow */
      
  type->init = (init_func *) init_gdkwindow;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkwindow */ 

  top->s_type =  (s_type_func *) gdkwindow_type_as_string;    
  top->sh_type = (sh_type_func *) gdkwindow_type_short_string;
  /* top->create = (create_func*) int_gdkwindow_create;*/ 
  
  /* specific methods for gdkwindow */
      
  type->init = (init_func *) init_gdkwindow;

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
 * initialize GdkWindow instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkwindow(NspGdkWindow *o,NspTypeGdkWindow *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkWindow 
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
 * Object method redefined for GdkWindow 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkwindow_type_name[]="GdkWindow";
static char gdkwindow_short_type_name[]="GdkWindow";

static char *gdkwindow_type_as_string(void)
{
  return(gdkwindow_type_name);
}

static char *gdkwindow_type_short_string(void)
{
  return(gdkwindow_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkWindow objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkWindow   *gdkwindow_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkwindow_id) ) return ((NspGdkWindow *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkwindow));
  return NULL;
}

int IsGdkWindowObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkwindow_id);
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
  if (( M = gdkwindow_object(NthObj(i))) == NULLGDKWINDOW)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkWindow *gdkwindow_copy(NspGdkWindow *self)
{
  /* return gdkdrawable_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkwindow);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkwindow);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkWindow
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkWindow *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkwindow is initialized * /
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if(( H = gdkdrawable_create(NVOID,(NspTypeBase *) nsp_type_gdkwindow)) == NULLGDKWINDOW) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 1616 "gdk.override"
static int
_wrap_gdk_drag_begin(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "targets", NULL };*/
  NspMatrix * nsp_targets;
  GList *targets = NULL;
  guint i;
  GdkDragContext *context;
  NspObject *nsp_context;
  int_types T[] = { realmat, t_end};

  if (GetArgs(stack,rhs,opt,T, &nsp_targets) == FAIL )  return RET_BUG;

  for (i = 0; i < nsp_targets->mn ; i++) 
    targets = g_list_append(targets, GUINT_TO_POINTER((guint)nsp_targets->R[i]));
  context = gdk_drag_begin(GDK_WINDOW(self->obj), targets);
  g_list_free(targets);
  nsp_context = (NspObject *) nspgobject_new((GObject *)context);
  gdk_drag_context_unref(context);
  MoveObj(stack,1, nsp_context);
  return 1;
}
#line 4338 "gdk.c"


static int _wrap_gdk_input_set_extension_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, obj,t_end};
  int mask;
  NspObject *nsp_mode = NULL;
  GdkExtensionMode mode;

  if ( GetArgs(stack,rhs,opt,T,&mask, &nsp_mode) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_EXTENSION_MODE, nsp_mode, (gint *)&mode)== FAIL)
      return RET_BUG;
  gdk_input_set_extension_events(GDK_WINDOW(self->obj), mask, mode);
  return 0;
}

#line 1640 "gdk.override"
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
      if ((pdata = nsp_new_string_obj(NVOID,data, alength)) == NULL) return RET_BUG;
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
#line 4416 "gdk.c"


#line 1702 "gdk.override"
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
    
  if (nspg_enum_get_value(GDK_TYPE_PROP_MODE, nsp_mode, (gint *)&mode))
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
  return 0;
}
#line 4522 "gdk.c"


static int _wrap_gdk_property_delete(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_property = NULL;
  GdkAtom property;

  if ( GetArgs(stack,rhs,opt,T,&nsp_property) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_property,&property)==FAIL) return RET_BUG;
  gdk_property_delete(GDK_WINDOW(self->obj), property);
  return 0;
}

static int _wrap_gdk_selection_convert(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, obj, s_int,t_end};
  gulong time;
  NspObject *nsp_selection = NULL, *nsp_target = NULL;
  GdkAtom selection, target;

  if ( GetArgs(stack,rhs,opt,T,&nsp_selection, &nsp_target, &time) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_target,&target)==FAIL) return RET_BUG;
  gdk_selection_convert(GDK_WINDOW(self->obj), selection, target, time);
  return 0;
}

static int _wrap_gdk_window_destroy(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_destroy(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_get_window_type(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = gdk_window_get_window_type(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_show(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_show(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_hide(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_hide(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_withdraw(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_withdraw(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_move(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int x, y;

  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
  gdk_window_move(GDK_WINDOW(self->obj), x, y);
  return 0;
}

static int _wrap_gdk_window_resize(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int width, height;

  if ( GetArgs(stack,rhs,opt,T,&width, &height) == FAIL) return RET_BUG;
  gdk_window_resize(GDK_WINDOW(self->obj), width, height);
  return 0;
}

static int _wrap_gdk_window_move_resize(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int, s_int,t_end};
  int x, y, width, height;

  if ( GetArgs(stack,rhs,opt,T,&x, &y, &width, &height) == FAIL) return RET_BUG;
  gdk_window_move_resize(GDK_WINDOW(self->obj), x, y, width, height);
  return 0;
}

static int _wrap_gdk_window_reparent(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int,t_end};
  int x, y;
  NspGObject *new_parent;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &new_parent, &x, &y) == FAIL) return RET_BUG;
  gdk_window_reparent(GDK_WINDOW(self->obj), GDK_WINDOW(new_parent->obj), x, y);
  return 0;
}

static int _wrap_gdk_window_clear(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_clear(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_clear_area(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int, s_int,t_end};
  int x, y, width, height;

  if ( GetArgs(stack,rhs,opt,T,&x, &y, &width, &height) == FAIL) return RET_BUG;
  gdk_window_clear_area(GDK_WINDOW(self->obj), x, y, width, height);
  return 0;
}

static int _wrap_gdk_window_clear_area_e(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int, s_int,t_end};
  int x, y, width, height;

  if ( GetArgs(stack,rhs,opt,T,&x, &y, &width, &height) == FAIL) return RET_BUG;
  gdk_window_clear_area_e(GDK_WINDOW(self->obj), x, y, width, height);
  return 0;
}

static int _wrap_gdk_window_raise(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_raise(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_lower(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_lower(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_focus(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {new_opts,t_end};
  nsp_option opts[] = {
	{"timestamp",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  gulong timestamp = GDK_CURRENT_TIME;

  if ( GetArgs(stack,rhs,opt,T,opts, &timestamp) == FAIL) return RET_BUG;
  gdk_window_focus(GDK_WINDOW(self->obj), timestamp);
  return 0;
}

static int _wrap_gdk_window_set_override_redirect(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int override_redirect;

  if ( GetArgs(stack,rhs,opt,T,&override_redirect) == FAIL) return RET_BUG;
  gdk_window_set_override_redirect(GDK_WINDOW(self->obj), override_redirect);
  return 0;
}

static int _wrap_gdk_window_scroll(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int dx, dy;

  if ( GetArgs(stack,rhs,opt,T,&dx, &dy) == FAIL) return RET_BUG;
  gdk_window_scroll(GDK_WINDOW(self->obj), dx, dy);
  return 0;
}

static int _wrap_gdk_window_shape_combine_mask(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int,t_end};
  int offset_x, offset_y;
  NspGObject *shape_mask;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkbitmap, &shape_mask, &offset_x, &offset_y) == FAIL) return RET_BUG;
  gdk_window_shape_combine_mask(GDK_WINDOW(self->obj), GDK_DRAWABLE(shape_mask->obj), offset_x, offset_y);
  return 0;
}

static int _wrap_gdk_window_set_child_shapes(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_set_child_shapes(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_merge_child_shapes(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_merge_child_shapes(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_is_visible(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_window_is_visible(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_is_viewable(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_window_is_viewable(GDK_WINDOW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_get_state(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;

  ret = gdk_window_get_state(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_static_gravities(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int use_static, ret;

  if ( GetArgs(stack,rhs,opt,T,&use_static) == FAIL) return RET_BUG;
  ret = gdk_window_set_static_gravities(GDK_WINDOW(self->obj), use_static);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_hints(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int, s_int, s_int, s_int, s_int,t_end};
  int x, y, min_width, min_height, max_width, max_height, flags;

  if ( GetArgs(stack,rhs,opt,T,&x, &y, &min_width, &min_height, &max_width, &max_height, &flags) == FAIL) return RET_BUG;
  gdk_window_set_hints(GDK_WINDOW(self->obj), x, y, min_width, min_height, max_width, max_height, flags);
  return 0;
}

static int _wrap_gdk_window_set_type_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_hint = NULL;
  GdkWindowTypeHint hint;

  if ( GetArgs(stack,rhs,opt,T,&nsp_hint) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_WINDOW_TYPE_HINT, nsp_hint, (gint *)&hint)== FAIL)
      return RET_BUG;
  gdk_window_set_type_hint(GDK_WINDOW(self->obj), hint);
  return 0;
}

static int _wrap_gdk_window_set_modal_hint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int modal;

  if ( GetArgs(stack,rhs,opt,T,&modal) == FAIL) return RET_BUG;
  gdk_window_set_modal_hint(GDK_WINDOW(self->obj), modal);
  return 0;
}

static int _wrap_gdk_window_begin_paint_rect(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GdkRectangle rectangle = { 0, 0, 0, 0 };
  NspObject *nsp_rectangle;

  if ( GetArgs(stack,rhs,opt,T,&nsp_rectangle) == FAIL) return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_rectangle, &rectangle))
      return RET_BUG;
  gdk_window_begin_paint_rect(GDK_WINDOW(self->obj), &rectangle);
  return 0;
}

static int _wrap_gdk_window_end_paint(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_end_paint(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_set_title(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *title;

  if ( GetArgs(stack,rhs,opt,T,&title) == FAIL) return RET_BUG;
  gdk_window_set_title(GDK_WINDOW(self->obj), title);
  return 0;
}

static int _wrap_gdk_window_set_role(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *role;

  if ( GetArgs(stack,rhs,opt,T,&role) == FAIL) return RET_BUG;
  gdk_window_set_role(GDK_WINDOW(self->obj), role);
  return 0;
}

static int _wrap_gdk_window_set_transient_for(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *leader;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &leader) == FAIL) return RET_BUG;
  gdk_window_set_transient_for(GDK_WINDOW(self->obj), GDK_WINDOW(leader->obj));
  return 0;
}

static int _wrap_gdk_window_set_background(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_color;
  GdkColor *color = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_COLOR))
      color = nspg_boxed_get(nsp_color, GdkColor);
  else {
      Scierror( "color should be a GdkColor");
      return RET_BUG;
  }
  gdk_window_set_background(GDK_WINDOW(self->obj), color);
  return 0;
}

static int _wrap_gdk_window_set_back_pixmap(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, s_bool,t_end};
  int parent_relative;
  NspGObject *nsp_pixmap;
  GdkPixmap *pixmap = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_pixmap, &parent_relative) == FAIL) return RET_BUG;
  if ( IsGdkPixmap((NspObject *)nsp_pixmap))
      pixmap = GDK_PIXMAP(nsp_pixmap->obj);
  else if ( ! IsNone((NspObject *) nsp_pixmap))  {
      Scierror( "pixmap should be a GdkPixmap or None");
      return RET_BUG;
  }
  gdk_window_set_back_pixmap(GDK_WINDOW(self->obj), pixmap, parent_relative);
  return 0;
}

static int _wrap_gdk_window_set_cursor(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_cursor = NULL;
  GdkCursor *cursor = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_cursor) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cursor, GDK_TYPE_CURSOR))
      cursor = nspg_boxed_get(nsp_cursor, GdkCursor);
  else if (! IsNone(nsp_cursor)) {
      Scierror("cursor should be a GdkCursor or None");
      return RET_BUG;
  }
  gdk_window_set_cursor(GDK_WINDOW(self->obj), cursor);
  return 0;
}

#line 1806 "gdk.override"
static int
_wrap_gdk_window_get_geometry(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y, width, height, depth;

  gdk_window_get_geometry(GDK_WINDOW(self->obj), &x, &y, &width, &height, &depth);
  if ( nsp_move_doubles(stack,1,1,5,(double) x,(double) y,
			(double) width,(double) height,(double) depth) == FAIL) return RET_BUG; 
  return 1;
}
#line 4902 "gdk.c"


#line 1818 "gdk.override"
static int
_wrap_gdk_window_get_position(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;

  gdk_window_get_position(GDK_WINDOW(self->obj), &x, &y);
  if ( nsp_move_doubles(stack,1,1,2,(double) x,(double) y) == FAIL) return RET_BUG; 
  return 1;
}
#line 4915 "gdk.c"


#line 1829 "gdk.override"
static int
_wrap_gdk_window_get_origin(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;

  gdk_window_get_origin(GDK_WINDOW(self->obj), &x, &y);
  if ( nsp_move_doubles(stack,1,1,2,(double) x,(double) y) == FAIL) return RET_BUG; 
  return 1;
}
#line 4928 "gdk.c"


#line 1840 "gdk.override"
static int
_wrap_gdk_window_get_deskrelative_origin(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;

  gdk_window_get_deskrelative_origin(GDK_WINDOW(self->obj), &x, &y);
  if ( nsp_move_doubles(stack,1,1,2,(double) x,(double) y) == FAIL) return RET_BUG; 
  return 1;
}
#line 4941 "gdk.c"


#line 1851 "gdk.override"
static int
_wrap_gdk_window_get_root_origin(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;

  gdk_window_get_root_origin(GDK_WINDOW(self->obj), &x, &y);
  if ( nsp_move_doubles(stack,1,1,2,(double) x,(double) y) == FAIL) return RET_BUG; 
  return 1;
}
#line 4954 "gdk.c"


static int _wrap_gdk_window_get_frame_extents(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GdkRectangle rect = { 0, 0, 0, 0 };
  NspObject *nsp_rect;

  if ( GetArgs(stack,rhs,opt,T,&nsp_rect) == FAIL) return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_rect, &rect))
      return RET_BUG;
  gdk_window_get_frame_extents(GDK_WINDOW(self->obj), &rect);
  return 0;
}

#line 1862 "gdk.override"
static int
_wrap_gdk_window_get_pointer(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint x, y;
  GdkModifierType mask;

  gdk_window_get_pointer(GDK_WINDOW(self->obj), &x, &y, &mask);
  if ( nsp_move_doubles(stack,1,1,3,(double) x,(double) y, (double) mask) == FAIL) return RET_BUG; 
  return 1;
}
#line 4981 "gdk.c"


static int _wrap_gdk_window_get_parent(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkWindow *ret;

  ret = gdk_window_get_parent(GDK_WINDOW(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_window_get_toplevel(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkWindow *ret;

  ret = gdk_window_get_toplevel(GDK_WINDOW(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 1874 "gdk.override"
static int
_wrap_gdk_window_get_children(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  GList *children, *tmp;
  NspList *ret; 
  Cell *cloc = NULLCELL, *cloc1; 
  
  if (( ret =nsp_list_create(NVOID,NULL) ) == NULLLIST) return RET_BUG;
  children = gdk_window_get_children(GDK_WINDOW(self->obj));

  for (tmp = children; tmp != NULL; tmp = tmp->next) {
    NspObject *item = (NspObject *) nspgobject_new((GObject *)tmp->data);
    if ( item == NULLOBJ) goto clean;
    if ((cloc1 =nsp_cell_create(NVOID,item))== NULLCELL) goto clean;
    cloc1->next = NULLCELL; 
    if ( cloc == NULLCELL) 
      { 
	ret->first = cloc1;
      } 
    else
      { 
	cloc1->prev = cloc; cloc->next = cloc1; 
      } 
    cloc= cloc1;
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
#line 5044 "gdk.c"


static int _wrap_gdk_window_get_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;

  ret = gdk_window_get_events(GDK_WINDOW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_window_set_events(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_event_mask = NULL;
  GdkEventMask event_mask;

  if ( GetArgs(stack,rhs,opt,T,&nsp_event_mask) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_EVENT_MASK, nsp_event_mask, (gint *)&event_mask)==FAIL)
      return RET_BUG;
  gdk_window_set_events(GDK_WINDOW(self->obj), event_mask);
  return 0;
}

static int _wrap_gdk_window_set_icon(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check, obj_check,t_end};
  NspGObject *icon_window, *pixmap, *mask;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &icon_window, &nsp_type_gdkpixmap, &pixmap, &nsp_type_gdkbitmap, &mask) == FAIL) return RET_BUG;
  gdk_window_set_icon(GDK_WINDOW(self->obj), GDK_WINDOW(icon_window->obj), GDK_PIXMAP(pixmap->obj), GDK_DRAWABLE(mask->obj));
  return 0;
}

static int _wrap_gdk_window_set_icon_name(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *name;

  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
  gdk_window_set_icon_name(GDK_WINDOW(self->obj), name);
  return 0;
}

static int _wrap_gdk_window_set_group(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *leader;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &leader) == FAIL) return RET_BUG;
  gdk_window_set_group(GDK_WINDOW(self->obj), GDK_WINDOW(leader->obj));
  return 0;
}

static int _wrap_gdk_window_set_decorations(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_decorations = NULL;
  GdkWMDecoration decorations;

  if ( GetArgs(stack,rhs,opt,T,&nsp_decorations) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_WM_DECORATION, nsp_decorations, (gint *)&decorations)==FAIL)
      return RET_BUG;
  gdk_window_set_decorations(GDK_WINDOW(self->obj), decorations);
  return 0;
}

static int _wrap_gdk_window_set_functions(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_functions = NULL;
  GdkWMFunction functions;

  if ( GetArgs(stack,rhs,opt,T,&nsp_functions) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GDK_TYPE_WM_FUNCTION, nsp_functions, (gint *)&functions)==FAIL)
      return RET_BUG;
  gdk_window_set_functions(GDK_WINDOW(self->obj), functions);
  return 0;
}

static int _wrap_gdk_window_iconify(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_iconify(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_deiconify(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_deiconify(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_stick(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_stick(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_unstick(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_unstick(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_maximize(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_maximize(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_unmaximize(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_unmaximize(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_register_dnd(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_register_dnd(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_begin_resize_drag(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, s_int, s_int, s_int, s_int,t_end};
  int button, root_x, root_y;
  NspObject *nsp_edge = NULL;
  gulong timestamp;
  GdkWindowEdge edge;

  if ( GetArgs(stack,rhs,opt,T,&nsp_edge, &button, &root_x, &root_y, &timestamp) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_WINDOW_EDGE, nsp_edge, (gint *)&edge)== FAIL)
      return RET_BUG;
  gdk_window_begin_resize_drag(GDK_WINDOW(self->obj), edge, button, root_x, root_y, timestamp);
  return 0;
}

static int _wrap_gdk_window_begin_move_drag(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int, s_int,t_end};
  int button, root_x, root_y;
  gulong timestamp;

  if ( GetArgs(stack,rhs,opt,T,&button, &root_x, &root_y, &timestamp) == FAIL) return RET_BUG;
  gdk_window_begin_move_drag(GDK_WINDOW(self->obj), button, root_x, root_y, timestamp);
  return 0;
}

static int _wrap_gdk_window_invalidate_rect(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, s_bool,t_end};
  int invalidate_children;
  GdkRectangle rect_rect = { 0, 0, 0, 0 }, *rect;
  NspObject *nsp_rect = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_rect, &invalidate_children) == FAIL) return RET_BUG;
  if (nsp_rect == NULL)
      rect = NULL;
  else if (nsp_gdk_rectangle_from_object(nsp_rect, &rect_rect))
      rect = &rect_rect;
  else
          return RET_BUG;
  gdk_window_invalidate_rect(GDK_WINDOW(self->obj), rect, invalidate_children);
  return 0;
}

static int _wrap_gdk_window_freeze_updates(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_freeze_updates(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_thaw_updates(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_window_thaw_updates(GDK_WINDOW(self->obj));
  return 0;
}

static int _wrap_gdk_window_process_updates(NspGdkWindow *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int update_children;

  if ( GetArgs(stack,rhs,opt,T,&update_children) == FAIL) return RET_BUG;
  gdk_window_process_updates(GDK_WINDOW(self->obj), update_children);
  return 0;
}

static NspMethods gdkwindow_methods[] = {
  {"drag_begin",(nsp_method *) _wrap_gdk_drag_begin},
  {"input_set_extension_events",(nsp_method *) _wrap_gdk_input_set_extension_events},
  {"property_get",(nsp_method *) _wrap_gdk_property_get},
  {"property_change",(nsp_method *) _wrap_gdk_property_change},
  {"property_delete",(nsp_method *) _wrap_gdk_property_delete},
  {"selection_convert",(nsp_method *) _wrap_gdk_selection_convert},
  {"destroy",(nsp_method *) _wrap_gdk_window_destroy},
  {"get_window_type",(nsp_method *) _wrap_gdk_window_get_window_type},
  {"show",(nsp_method *) _wrap_gdk_window_show},
  {"hide",(nsp_method *) _wrap_gdk_window_hide},
  {"withdraw",(nsp_method *) _wrap_gdk_window_withdraw},
  {"move",(nsp_method *) _wrap_gdk_window_move},
  {"resize",(nsp_method *) _wrap_gdk_window_resize},
  {"move_resize",(nsp_method *) _wrap_gdk_window_move_resize},
  {"reparent",(nsp_method *) _wrap_gdk_window_reparent},
  {"clear",(nsp_method *) _wrap_gdk_window_clear},
  {"clear_area",(nsp_method *) _wrap_gdk_window_clear_area},
  {"clear_area_e",(nsp_method *) _wrap_gdk_window_clear_area_e},
  {"raise_",(nsp_method *) _wrap_gdk_window_raise},
  {"lower",(nsp_method *) _wrap_gdk_window_lower},
  {"focus",(nsp_method *) _wrap_gdk_window_focus},
  {"set_override_redirect",(nsp_method *) _wrap_gdk_window_set_override_redirect},
  {"scroll",(nsp_method *) _wrap_gdk_window_scroll},
  {"shape_combine_mask",(nsp_method *) _wrap_gdk_window_shape_combine_mask},
  {"set_child_shapes",(nsp_method *) _wrap_gdk_window_set_child_shapes},
  {"merge_child_shapes",(nsp_method *) _wrap_gdk_window_merge_child_shapes},
  {"is_visible",(nsp_method *) _wrap_gdk_window_is_visible},
  {"is_viewable",(nsp_method *) _wrap_gdk_window_is_viewable},
  {"get_state",(nsp_method *) _wrap_gdk_window_get_state},
  {"set_static_gravities",(nsp_method *) _wrap_gdk_window_set_static_gravities},
  {"set_hints",(nsp_method *) _wrap_gdk_window_set_hints},
  {"set_type_hint",(nsp_method *) _wrap_gdk_window_set_type_hint},
  {"set_modal_hint",(nsp_method *) _wrap_gdk_window_set_modal_hint},
  {"begin_paint_rect",(nsp_method *) _wrap_gdk_window_begin_paint_rect},
  {"end_paint",(nsp_method *) _wrap_gdk_window_end_paint},
  {"set_title",(nsp_method *) _wrap_gdk_window_set_title},
  {"set_role",(nsp_method *) _wrap_gdk_window_set_role},
  {"set_transient_for",(nsp_method *) _wrap_gdk_window_set_transient_for},
  {"set_background",(nsp_method *) _wrap_gdk_window_set_background},
  {"set_back_pixmap",(nsp_method *) _wrap_gdk_window_set_back_pixmap},
  {"set_cursor",(nsp_method *) _wrap_gdk_window_set_cursor},
  {"get_geometry",(nsp_method *) _wrap_gdk_window_get_geometry},
  {"get_position",(nsp_method *) _wrap_gdk_window_get_position},
  {"get_origin",(nsp_method *) _wrap_gdk_window_get_origin},
  {"get_deskrelative_origin",(nsp_method *) _wrap_gdk_window_get_deskrelative_origin},
  {"get_root_origin",(nsp_method *) _wrap_gdk_window_get_root_origin},
  {"get_frame_extents",(nsp_method *) _wrap_gdk_window_get_frame_extents},
  {"get_pointer",(nsp_method *) _wrap_gdk_window_get_pointer},
  {"get_parent",(nsp_method *) _wrap_gdk_window_get_parent},
  {"get_toplevel",(nsp_method *) _wrap_gdk_window_get_toplevel},
  {"get_children",(nsp_method *) _wrap_gdk_window_get_children},
  {"get_events",(nsp_method *) _wrap_gdk_window_get_events},
  {"set_events",(nsp_method *) _wrap_gdk_window_set_events},
  {"set_icon",(nsp_method *) _wrap_gdk_window_set_icon},
  {"set_icon_name",(nsp_method *) _wrap_gdk_window_set_icon_name},
  {"set_group",(nsp_method *) _wrap_gdk_window_set_group},
  {"set_decorations",(nsp_method *) _wrap_gdk_window_set_decorations},
  {"set_functions",(nsp_method *) _wrap_gdk_window_set_functions},
  {"iconify",(nsp_method *) _wrap_gdk_window_iconify},
  {"deiconify",(nsp_method *) _wrap_gdk_window_deiconify},
  {"stick",(nsp_method *) _wrap_gdk_window_stick},
  {"unstick",(nsp_method *) _wrap_gdk_window_unstick},
  {"maximize",(nsp_method *) _wrap_gdk_window_maximize},
  {"unmaximize",(nsp_method *) _wrap_gdk_window_unmaximize},
  {"register_dnd",(nsp_method *) _wrap_gdk_window_register_dnd},
  {"begin_resize_drag",(nsp_method *) _wrap_gdk_window_begin_resize_drag},
  {"begin_move_drag",(nsp_method *) _wrap_gdk_window_begin_move_drag},
  {"invalidate_rect",(nsp_method *) _wrap_gdk_window_invalidate_rect},
  {"freeze_updates",(nsp_method *) _wrap_gdk_window_freeze_updates},
  {"thaw_updates",(nsp_method *) _wrap_gdk_window_thaw_updates},
  {"process_updates",(nsp_method *) _wrap_gdk_window_process_updates},
  { NULL, NULL}
};

static NspMethods *gdkwindow_get_methods(void) { return gdkwindow_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkwindow_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkPixmap ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkPixmap_Private 
#include "nsp/gtk/gdkpixmap.h"
#include "nsp/interf.h"

/* NspGdkPixmap inherits from NspGdkDrawable */ 

int nsp_type_gdkpixmap_id=0;
NspTypeGdkPixmap *nsp_type_gdkpixmap=NULL;

NspTypeGdkPixmap *new_type_gdkpixmap(type_mode mode)
{
  NspTypeGdkPixmap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixmap != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixmap;
    }
  if ((type =  malloc(sizeof(NspTypeGdkDrawable))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gdkdrawable(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixmap_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkpixmap_get_methods; 
  type->new = (new_func *) new_gdkpixmap;

  /* specific methods for gdkpixmap */
      
  type->init = (init_func *) init_gdkpixmap;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkpixmap */ 

  top->s_type =  (s_type_func *) gdkpixmap_type_as_string;    
  top->sh_type = (sh_type_func *) gdkpixmap_type_short_string;
  /* top->create = (create_func*) int_gdkpixmap_create;*/ 
  
  /* specific methods for gdkpixmap */
      
  type->init = (init_func *) init_gdkpixmap;

  if ( nsp_type_gdkpixmap_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkPixmap called nsp_type_gdkpixmap
       */
      type->id =  nsp_type_gdkpixmap_id = nsp_new_type_id();
      nsp_type_gdkpixmap = type;
      if ( nsp_register_type(nsp_type_gdkpixmap) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkpixmap, GDK_TYPE_PIXMAP);
      return ( mode == T_BASE ) ? type : new_type_gdkpixmap(mode);
    }
  else 
    {
       type->id = nsp_type_gdkpixmap_id;
       return type;
    }
}

/*
 * initialize GdkPixmap instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixmap(NspGdkPixmap *o,NspTypeGdkPixmap *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkPixmap 
 */

NspGdkPixmap *new_gdkpixmap() 
{
  NspGdkPixmap *loc; 
  /* type must exists */
  nsp_type_gdkpixmap = new_type_gdkpixmap(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkPixmap)))== NULLGDKPIXMAP) return loc;
  /* initialize object */
  if ( init_gdkpixmap(loc,nsp_type_gdkpixmap) == FAIL) return NULLGDKPIXMAP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GdkPixmap 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkpixmap_type_name[]="GdkPixmap";
static char gdkpixmap_short_type_name[]="GdkPixmap";

static char *gdkpixmap_type_as_string(void)
{
  return(gdkpixmap_type_name);
}

static char *gdkpixmap_type_short_string(void)
{
  return(gdkpixmap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkPixmap objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkPixmap   *gdkpixmap_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkpixmap_id) ) return ((NspGdkPixmap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixmap));
  return NULL;
}

int IsGdkPixmapObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkpixmap_id);
}

int IsGdkPixmap(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkpixmap_id);
}

NspGdkPixmap  *GetGdkPixmapCopy(Stack stack, int i)
{
  if (  GetGdkPixmap(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkPixmap  *GetGdkPixmap(Stack stack, int i)
{
  NspGdkPixmap *M;
  if (( M = gdkpixmap_object(NthObj(i))) == NULLGDKPIXMAP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkPixmap *gdkpixmap_copy(NspGdkPixmap *self)
{
  /* return gdkdrawable_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixmap);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkpixmap);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkPixmap
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkPixmap *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkpixmap is initialized * /
  nsp_type_gdkpixmap = new_type_gdkpixmap(T_BASE);
  if(( H = gdkdrawable_create(NVOID,(NspTypeBase *) nsp_type_gdkpixmap)) == NULLGDKPIXMAP) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_gdkpixmap_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, s_int, s_int,new_opts,t_end};
  nsp_option opts[] = {
	{"depth",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  int width, height, depth = -1;
  GdkDrawable *drawable = NULL;
  NspGObject *nsp_drawable;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_drawable, &width, &height, opts, &depth) == FAIL) return RET_BUG;
  if ( IsGdkDrawable((NspObject *)nsp_drawable))
      drawable = GDK_DRAWABLE(nsp_drawable->obj);
  else if ( ! IsNone((NspObject *) nsp_drawable))  {
      Scierror( "drawable should be a GdkDrawable or None");
      return RET_BUG;
  }
  if ((ret = (GObject *)gdk_pixmap_new(drawable, width, height, depth))== NULL) return RET_BUG;

  nsp_type_gdkpixmap = new_type_gdkpixmap(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkpixmap );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *gdkpixmap_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkpixmap_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkBitmap ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkBitmap_Private 
#include "nsp/gtk/gdkbitmap.h"
#include "nsp/interf.h"

/* NspGdkBitmap inherits from NspGdkDrawable */ 

int nsp_type_gdkbitmap_id=0;
NspTypeGdkBitmap *nsp_type_gdkbitmap=NULL;

NspTypeGdkBitmap *new_type_gdkbitmap(type_mode mode)
{
  NspTypeGdkBitmap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkbitmap != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkbitmap;
    }
  if ((type =  malloc(sizeof(NspTypeGdkDrawable))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gdkdrawable(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkbitmap_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkbitmap_get_methods; 
  type->new = (new_func *) new_gdkbitmap;

  /* specific methods for gdkbitmap */
      
  type->init = (init_func *) init_gdkbitmap;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkbitmap */ 

  top->s_type =  (s_type_func *) gdkbitmap_type_as_string;    
  top->sh_type = (sh_type_func *) gdkbitmap_type_short_string;
  /* top->create = (create_func*) int_gdkbitmap_create;*/ 
  
  /* specific methods for gdkbitmap */
      
  type->init = (init_func *) init_gdkbitmap;

  if ( nsp_type_gdkbitmap_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkBitmap called nsp_type_gdkbitmap
       */
      type->id =  nsp_type_gdkbitmap_id = nsp_new_type_id();
      nsp_type_gdkbitmap = type;
      if ( nsp_register_type(nsp_type_gdkbitmap) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkbitmap, GDK_TYPE_DRAWABLE);
      return ( mode == T_BASE ) ? type : new_type_gdkbitmap(mode);
    }
  else 
    {
       type->id = nsp_type_gdkbitmap_id;
       return type;
    }
}

/*
 * initialize GdkBitmap instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkbitmap(NspGdkBitmap *o,NspTypeGdkBitmap *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkBitmap 
 */

NspGdkBitmap *new_gdkbitmap() 
{
  NspGdkBitmap *loc; 
  /* type must exists */
  nsp_type_gdkbitmap = new_type_gdkbitmap(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkBitmap)))== NULLGDKBITMAP) return loc;
  /* initialize object */
  if ( init_gdkbitmap(loc,nsp_type_gdkbitmap) == FAIL) return NULLGDKBITMAP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GdkBitmap 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkbitmap_type_name[]="GdkBitmap";
static char gdkbitmap_short_type_name[]="GdkBitmap";

static char *gdkbitmap_type_as_string(void)
{
  return(gdkbitmap_type_name);
}

static char *gdkbitmap_type_short_string(void)
{
  return(gdkbitmap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkBitmap objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkBitmap   *gdkbitmap_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkbitmap_id) ) return ((NspGdkBitmap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkbitmap));
  return NULL;
}

int IsGdkBitmapObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkbitmap_id);
}

int IsGdkBitmap(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkbitmap_id);
}

NspGdkBitmap  *GetGdkBitmapCopy(Stack stack, int i)
{
  if (  GetGdkBitmap(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkBitmap  *GetGdkBitmap(Stack stack, int i)
{
  NspGdkBitmap *M;
  if (( M = gdkbitmap_object(NthObj(i))) == NULLGDKBITMAP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkBitmap *gdkbitmap_copy(NspGdkBitmap *self)
{
  /* return gdkdrawable_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkbitmap);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkbitmap);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkBitmap
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkBitmap *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkbitmap is initialized * /
  nsp_type_gdkbitmap = new_type_gdkbitmap(T_BASE);
  if(( H = gdkdrawable_create(NVOID,(NspTypeBase *) nsp_type_gdkbitmap)) == NULLGDKBITMAP) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static NspMethods *gdkbitmap_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkbitmap_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkGC ----------- */

static int _wrap_gdk_gc_tp_getattr(Stack stack, int rhs, int opt, int lhs);
static int _wrap_gdk_gc_tp_setattr(Stack stack, int rhs, int opt, int lhs);
/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkGC_Private 
#include "nsp/gtk/gdkgc.h"
#include "nsp/interf.h"

/* NspGdkGC inherits from NspGObject */ 

int nsp_type_gdkgc_id=0;
NspTypeGdkGC *nsp_type_gdkgc=NULL;

NspTypeGdkGC *new_type_gdkgc(type_mode mode)
{
  NspTypeGdkGC *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkgc != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkgc;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkgc_attrs ; 
  type->get_attrs = (attrs_func *)  _wrap_gdk_gc_tp_getattr;
  type->set_attrs = (attrs_func *)  _wrap_gdk_gc_tp_setattr;
  type->methods = gdkgc_get_methods; 
  type->new = (new_func *) new_gdkgc;

  /* specific methods for gdkgc */
      
  type->init = (init_func *) init_gdkgc;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkgc */ 

  top->s_type =  (s_type_func *) gdkgc_type_as_string;    
  top->sh_type = (sh_type_func *) gdkgc_type_short_string;
  /* top->create = (create_func*) int_gdkgc_create;*/ 
  
  /* specific methods for gdkgc */
      
  type->init = (init_func *) init_gdkgc;

  if ( nsp_type_gdkgc_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkGC called nsp_type_gdkgc
       */
      type->id =  nsp_type_gdkgc_id = nsp_new_type_id();
      nsp_type_gdkgc = type;
      if ( nsp_register_type(nsp_type_gdkgc) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkgc, GDK_TYPE_GC);
      return ( mode == T_BASE ) ? type : new_type_gdkgc(mode);
    }
  else 
    {
       type->id = nsp_type_gdkgc_id;
       return type;
    }
}

/*
 * initialize GdkGC instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkgc(NspGdkGC *o,NspTypeGdkGC *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkGC 
 */

NspGdkGC *new_gdkgc() 
{
  NspGdkGC *loc; 
  /* type must exists */
  nsp_type_gdkgc = new_type_gdkgc(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkGC)))== NULLGDKGC) return loc;
  /* initialize object */
  if ( init_gdkgc(loc,nsp_type_gdkgc) == FAIL) return NULLGDKGC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GdkGC 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkgc_type_name[]="GdkGC";
static char gdkgc_short_type_name[]="GdkGC";

static char *gdkgc_type_as_string(void)
{
  return(gdkgc_type_name);
}

static char *gdkgc_type_short_string(void)
{
  return(gdkgc_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkGC objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkGC   *gdkgc_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkgc_id) ) return ((NspGdkGC *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkgc));
  return NULL;
}

int IsGdkGCObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkgc_id);
}

int IsGdkGC(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkgc_id);
}

NspGdkGC  *GetGdkGCCopy(Stack stack, int i)
{
  if (  GetGdkGC(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkGC  *GetGdkGC(Stack stack, int i)
{
  NspGdkGC *M;
  if (( M = gdkgc_object(NthObj(i))) == NULLGDKGC)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkGC *gdkgc_copy(NspGdkGC *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkgc);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkgc);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkGC
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkGC *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkgc is initialized * /
  nsp_type_gdkgc = new_type_gdkgc(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkgc)) == NULLGDKGC) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 1280 "gdk.override"
static int
_wrap_gdkgc_new(Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, new_opts, t_end};
  nsp_option opts[] ={
    {"foreground",obj_check,NULLOBJ,-1},
    {"background",obj_check,NULLOBJ,-1},
    {"font",obj_check,NULLOBJ,-1},
    {"function",s_int,NULLOBJ,-1},
    {"fill",s_int,NULLOBJ,-1},
    {"tile",obj_check,NULLOBJ,-1},
    {"stipple",obj_check,NULLOBJ,-1},
    {"clip_mask",obj_check,NULLOBJ,-1},
    {"subwindow_mode",s_int,NULLOBJ,-1},
    {"ts_x_origin",s_int,NULLOBJ,-1},
    {"ts_y_origin",s_int,NULLOBJ,-1},
    {"clip_x_origin",s_int,NULLOBJ,-1},
    {"clip_y_origin",s_int,NULLOBJ,-1},
    {"graphics_exposures",s_int,NULLOBJ,-1},
    {"line_width",s_int,NULLOBJ,-1},
    {"line_style",s_int,NULLOBJ,-1},
    {"cap_style",s_int,NULLOBJ,-1},
    {"join_style",s_int,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};

  NspObject *nsp_drawable,*nsp_ret ;
  NspObject *foreground = NULL, *background = NULL;
  NspObject *font = NULL;
  gint function = -1, fill = -1;
  NspObject *tile = NULL, *stipple = NULL, *clip_mask = NULL;
  gint subwindow_mode = -1, ts_x_origin = -1, ts_y_origin = -1;
  gint clip_x_origin = -1, clip_y_origin = -1, graphics_exposures = -1;
  gint line_width = -1, line_style = -1, cap_style = -1, join_style = -1;
  GdkGCValues values;
  GdkGCValuesMask mask = 0;
  GdkGC *gc;

  if (GetArgs(stack,rhs,opt,T,&nsp_type_gdkdrawable, &nsp_drawable,
	      &opts,
	      &nsp_type_gdkcolor,&foreground, 
	      &nsp_type_gdkcolor,&background,
	      &nsp_type_gdkfont,&font,
	      &function, 
	      &fill, 
	      &nsp_type_gdkpixmap,&tile, 
	      &nsp_type_gdkpixmap,&stipple,
	      &nsp_type_gdkpixmap,&clip_mask,
	      &subwindow_mode, &ts_x_origin, &ts_y_origin, &clip_x_origin, &clip_y_origin,  
	      &graphics_exposures, &line_width, &line_style, &cap_style, &join_style) == FAIL )
     return RET_BUG;
  
  if (foreground != NULL) 
    {
      mask |= GDK_GC_FOREGROUND;
      values.foreground = *nspg_boxed_get(foreground, GdkColor);
    }
  if (background != NULL) 
    {
      mask |= GDK_GC_BACKGROUND;
      values.background = *nspg_boxed_get(background, GdkColor);
    }

  if (font != NULL) 
    {
      mask |= GDK_GC_FONT;
      values.font = nspg_boxed_get(font, GdkFont);
    }

  if (function != -1) {
    mask |= GDK_GC_FUNCTION;
    values.function = function;
  }
  if (fill != -1) {
    mask |= GDK_GC_FILL;
    values.fill = fill;
  }
  if ( tile != NULL) 
    {
      mask |= GDK_GC_TILE;
      values.tile = GDK_PIXMAP(nspgobject_get(tile));
    }
  if ( stipple != NULL) 
    {
      mask |= GDK_GC_STIPPLE;
      values.stipple = GDK_PIXMAP(nspgobject_get(stipple));
    }
  if ( clip_mask!= NULL) 
    {
      mask |= GDK_GC_CLIP_MASK;
      values.clip_mask = GDK_PIXMAP(nspgobject_get(clip_mask));
    }

  if (subwindow_mode != -1) {
    mask |= GDK_GC_SUBWINDOW;
    values.subwindow_mode = subwindow_mode;
  }
  if (ts_x_origin != -1) {
    mask |= GDK_GC_TS_X_ORIGIN;
    values.ts_x_origin = ts_x_origin;
  }
  if (ts_y_origin != -1) {
    mask |= GDK_GC_TS_Y_ORIGIN;
    values.ts_y_origin = ts_y_origin;
  }
  if (clip_x_origin != -1) {
    mask |= GDK_GC_CLIP_X_ORIGIN;
    values.clip_x_origin = clip_x_origin;
  }
  if (clip_y_origin != -1) {
    mask |= GDK_GC_CLIP_Y_ORIGIN;
    values.clip_y_origin = clip_y_origin;
  }
  if (graphics_exposures != -1) {
    mask |= GDK_GC_EXPOSURES;
    values.graphics_exposures = graphics_exposures;
  }
  if (line_width != -1) {
    mask |= GDK_GC_LINE_WIDTH;
    values.line_width = line_width;
  }
  if (line_style != -1) {
    mask |= GDK_GC_LINE_STYLE;
    values.line_style = line_style;
  }
  if (cap_style != -1) {
    mask |= GDK_GC_CAP_STYLE;
    values.cap_style = cap_style;
  }
  if (join_style != -1) {
    mask |= GDK_GC_JOIN_STYLE;
    values.join_style = join_style;
  }
  gc = gdk_gc_new_with_values( GDK_DRAWABLE(nspgobject_get(nsp_drawable)), &values, mask);
  nsp_type_gdkgc = new_type_gdkgc(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *) gc,(NspTypeBase *) nsp_type_gdkgc );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}
#line 6081 "gdk.c"


static int _wrap_gdk_gc_set_foreground(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_color;
  GdkColor *color = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_COLOR))
      color = nspg_boxed_get(nsp_color, GdkColor);
  else {
      Scierror( "color should be a GdkColor");
      return RET_BUG;
  }
  gdk_gc_set_foreground(GDK_GC(self->obj), color);
  return 0;
}

static int _wrap_gdk_gc_set_background(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_color;
  GdkColor *color = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_COLOR))
      color = nspg_boxed_get(nsp_color, GdkColor);
  else {
      Scierror( "color should be a GdkColor");
      return RET_BUG;
  }
  gdk_gc_set_background(GDK_GC(self->obj), color);
  return 0;
}

static int _wrap_gdk_gc_set_font(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GdkFont *font = NULL;
  NspObject *nsp_font;

  if ( GetArgs(stack,rhs,opt,T,&nsp_font) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_font, GDK_TYPE_FONT))
      font = nspg_boxed_get(nsp_font, GdkFont);
  else {
      Scierror( "font should be a GdkFont");
      return RET_BUG;
  }
  gdk_gc_set_font(GDK_GC(self->obj), font);
  return 0;
}

static int _wrap_gdk_gc_set_function(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GdkFunction function;
  NspObject *nsp_function = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_function) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_FUNCTION, nsp_function, (gint *)&function)== FAIL)
      return RET_BUG;
  gdk_gc_set_function(GDK_GC(self->obj), function);
  return 0;
}

static int _wrap_gdk_gc_set_fill(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_fill = NULL;
  GdkFill fill;

  if ( GetArgs(stack,rhs,opt,T,&nsp_fill) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_FILL, nsp_fill, (gint *)&fill)== FAIL)
      return RET_BUG;
  gdk_gc_set_fill(GDK_GC(self->obj), fill);
  return 0;
}

static int _wrap_gdk_gc_set_tile(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *tile;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixmap, &tile) == FAIL) return RET_BUG;
  gdk_gc_set_tile(GDK_GC(self->obj), GDK_PIXMAP(tile->obj));
  return 0;
}

static int _wrap_gdk_gc_set_stipple(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *stipple;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixmap, &stipple) == FAIL) return RET_BUG;
  gdk_gc_set_stipple(GDK_GC(self->obj), GDK_PIXMAP(stipple->obj));
  return 0;
}

static int _wrap_gdk_gc_set_ts_origin(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int x, y;

  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
  gdk_gc_set_ts_origin(GDK_GC(self->obj), x, y);
  return 0;
}

static int _wrap_gdk_gc_set_clip_origin(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int x, y;

  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
  gdk_gc_set_clip_origin(GDK_GC(self->obj), x, y);
  return 0;
}

static int _wrap_gdk_gc_set_clip_mask(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *mask;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkbitmap, &mask) == FAIL) return RET_BUG;
  gdk_gc_set_clip_mask(GDK_GC(self->obj), GDK_DRAWABLE(mask->obj));
  return 0;
}

static int _wrap_gdk_gc_set_clip_rectangle(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GdkRectangle rectangle = { 0, 0, 0, 0 };
  NspObject *nsp_rectangle;

  if ( GetArgs(stack,rhs,opt,T,&nsp_rectangle) == FAIL) return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_rectangle, &rectangle))
      return RET_BUG;
  gdk_gc_set_clip_rectangle(GDK_GC(self->obj), &rectangle);
  return 0;
}

static int _wrap_gdk_gc_set_subwindow(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_mode = NULL;
  GdkSubwindowMode mode;

  if ( GetArgs(stack,rhs,opt,T,&nsp_mode) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_SUBWINDOW_MODE, nsp_mode, (gint *)&mode)== FAIL)
      return RET_BUG;
  gdk_gc_set_subwindow(GDK_GC(self->obj), mode);
  return 0;
}

static int _wrap_gdk_gc_set_exposures(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int exposures;

  if ( GetArgs(stack,rhs,opt,T,&exposures) == FAIL) return RET_BUG;
  gdk_gc_set_exposures(GDK_GC(self->obj), exposures);
  return 0;
}

static int _wrap_gdk_gc_set_line_attributes(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, obj, obj, obj,t_end};
  int line_width;
  GdkLineStyle line_style;
  GdkJoinStyle join_style;
  NspObject *nsp_line_style = NULL, *nsp_cap_style = NULL, *nsp_join_style = NULL;
  GdkCapStyle cap_style;

  if ( GetArgs(stack,rhs,opt,T,&line_width, &nsp_line_style, &nsp_cap_style, &nsp_join_style) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_LINE_STYLE, nsp_line_style, (gint *)&line_style)== FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_CAP_STYLE, nsp_cap_style, (gint *)&cap_style)== FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_JOIN_STYLE, nsp_join_style, (gint *)&join_style)== FAIL)
      return RET_BUG;
  gdk_gc_set_line_attributes(GDK_GC(self->obj), line_width, line_style, cap_style, join_style);
  return 0;
}

#line 1579 "gdk.override"
static int
_wrap_gdk_gc_set_dashes(NspGObject *self, Stack stack,int rhs,int opt,int lhs) 
{
  gint dash_offset, i; 
  NspMatrix *list; 
  guchar *dash_list ;
  int_types T[] = { s_int, realmat , t_end};

  if (GetArgs(stack,rhs,opt,T,&dash_offset, &list)== FAIL)  return RET_BUG;
  dash_list = g_new(char, list->mn);
  for (i = 0; i < list->mn ; i++)
    {
      dash_list[i]= list->R[i];
      if (dash_list[i] == 0) {
	Scierror( "sequence member must not be 0");
	g_free(dash_list);
	return RET_BUG;
      }
    }
  gdk_gc_set_dashes(GDK_GC(self->obj), dash_offset, dash_list, list->mn);
  g_free(dash_list);
  return 0;
}
#line 6291 "gdk.c"


static int _wrap_gdk_gc_offset(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int x_offset, y_offset;

  if ( GetArgs(stack,rhs,opt,T,&x_offset, &y_offset) == FAIL) return RET_BUG;
  gdk_gc_offset(GDK_GC(self->obj), x_offset, y_offset);
  return 0;
}

static int _wrap_gdk_gc_copy(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *src_gc;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &src_gc) == FAIL) return RET_BUG;
  gdk_gc_copy(GDK_GC(self->obj), GDK_GC(src_gc->obj));
  return 0;
}

static int _wrap_gdk_gc_set_colormap(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *colormap;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkcolormap, &colormap) == FAIL) return RET_BUG;
  gdk_gc_set_colormap(GDK_GC(self->obj), GDK_COLORMAP(colormap->obj));
  return 0;
}

static int _wrap_gdk_gc_get_colormap(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkColormap *ret;

  ret = gdk_gc_get_colormap(GDK_GC(self->obj));
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_gc_set_rgb_fg_color(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_color;
  GdkColor *color = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_COLOR))
      color = nspg_boxed_get(nsp_color, GdkColor);
  else {
      Scierror( "color should be a GdkColor");
      return RET_BUG;
  }
  gdk_gc_set_rgb_fg_color(GDK_GC(self->obj), color);
  return 0;
}

static int _wrap_gdk_gc_set_rgb_bg_color(NspGdkGC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_color;
  GdkColor *color = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_COLOR))
      color = nspg_boxed_get(nsp_color, GdkColor);
  else {
      Scierror( "color should be a GdkColor");
      return RET_BUG;
  }
  gdk_gc_set_rgb_bg_color(GDK_GC(self->obj), color);
  return 0;
}

static NspMethods gdkgc_methods[] = {
  {"set_foreground",(nsp_method *) _wrap_gdk_gc_set_foreground},
  {"set_background",(nsp_method *) _wrap_gdk_gc_set_background},
  {"set_font",(nsp_method *) _wrap_gdk_gc_set_font},
  {"set_function",(nsp_method *) _wrap_gdk_gc_set_function},
  {"set_fill",(nsp_method *) _wrap_gdk_gc_set_fill},
  {"set_tile",(nsp_method *) _wrap_gdk_gc_set_tile},
  {"set_stipple",(nsp_method *) _wrap_gdk_gc_set_stipple},
  {"set_ts_origin",(nsp_method *) _wrap_gdk_gc_set_ts_origin},
  {"set_clip_origin",(nsp_method *) _wrap_gdk_gc_set_clip_origin},
  {"set_clip_mask",(nsp_method *) _wrap_gdk_gc_set_clip_mask},
  {"set_clip_rectangle",(nsp_method *) _wrap_gdk_gc_set_clip_rectangle},
  {"set_subwindow",(nsp_method *) _wrap_gdk_gc_set_subwindow},
  {"set_exposures",(nsp_method *) _wrap_gdk_gc_set_exposures},
  {"set_line_attributes",(nsp_method *) _wrap_gdk_gc_set_line_attributes},
  {"set_dashes",(nsp_method *) _wrap_gdk_gc_set_dashes},
  {"offset",(nsp_method *) _wrap_gdk_gc_offset},
  {"copy",(nsp_method *) _wrap_gdk_gc_copy},
  {"set_colormap",(nsp_method *) _wrap_gdk_gc_set_colormap},
  {"get_colormap",(nsp_method *) _wrap_gdk_gc_get_colormap},
  {"set_rgb_fg_color",(nsp_method *) _wrap_gdk_gc_set_rgb_fg_color},
  {"set_rgb_bg_color",(nsp_method *) _wrap_gdk_gc_set_rgb_bg_color},
  { NULL, NULL}
};

static NspMethods *gdkgc_get_methods(void) { return gdkgc_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkgc_attrs[]={{NULL,NULL,NULL}} ;
#line 1421 "gdk.override"

static char *_gdkgc_attrs[]={ "__attrs", "background", "cap_style",
			     "clip_mask", "clip_x_origin", "clip_y_origin",
			     "fill", "font", "foreground", "function",
			     "graphics_exposures", "join_style", "line_style",
			     "line_width", "stipple", "sub_window", "tile",
			     "ts_x_origin", "ts_y_origin", NULL 
};


static NspObject *_wrap_gdk_gc_tp_getattr1(NspObject *self, char *attr);

static int _wrap_gdk_gc_tp_getattr(Stack stack, int rhs, int opt, int lhs)
{
  char *attr;
  NspObject *ob;
  CheckRhs(2,100); /* XXXXXX */
  CheckLhs(-1,1);
  if ((ob =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  if ((attr = GetString(stack,2)) == (char*)0) return RET_BUG;  
  ob = _wrap_gdk_gc_tp_getattr1(ob,attr);
  if ( ob == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,ob);
  return 1;
}

static NspObject *_wrap_gdk_gc_tp_getattr1(NspObject *self, char *attr)
{
  GdkGCValues gc;
  int rep;
  if ((rep= is_string_in_array(attr, _gdkgc_attrs, 0)) < 0 ) 
    {
      Scierror("attribute %s not found or ambiguous\n",attr);
      return NULL;
    }
  switch (rep) {
  case 0 : return (NspObject *)nsp_smatrix_create_from_table(_gdkgc_attrs); break; 
  case 1: /* "background" */ return (NspObject *) gboxed_create(NVOID,GDK_TYPE_COLOR, &gc.background, TRUE, TRUE,(NspTypeBase *) nsp_type_gdkcolor);
  case 2: /* "cap_style" */  return nsp_new_double_obj((double) (gc.cap_style));
  case 3: /* "clip_mask" */  return (NspObject *)nspgobject_new((GObject *)gc.clip_mask);
  case 4: /* "clip_x_origin" */  return nsp_new_double_obj((double) (gc.clip_x_origin));
  case 5: /* "clip_y_origin" */  return nsp_new_double_obj((double) (gc.clip_y_origin));
  case 6: /* "fill" */      return nsp_new_double_obj((double) (gc.fill));
  case 7: /* "font" */      return (NspObject *) gboxed_create(NVOID,GDK_TYPE_FONT, gc.font, TRUE, TRUE,(NspTypeBase *) nsp_type_gdkfont);
  case 8: /* "foreground" */ return (NspObject *) gboxed_create(NVOID,GDK_TYPE_COLOR, &gc.foreground, TRUE, TRUE,(NspTypeBase *) nsp_type_gdkcolor);
  case 9: /* "function" */      return nsp_new_double_obj((double) (gc.function));
  case 10: /* "graphics_exposures"*/      return nsp_new_double_obj((double) (gc.graphics_exposures));
  case 11: /* "join_style" */      return nsp_new_double_obj((double) (gc.join_style));
  case 12: /* "line_style" */      return nsp_new_double_obj((double) (gc.line_style));
  case 13: /* "line_width" */      return nsp_new_double_obj((double) (gc.line_width));
  case 14: /* "stipple" */      return (NspObject *)nspgobject_new((GObject *)gc.stipple);
  case 15: /* "subwindow_mode"*/      return nsp_new_double_obj((double) (gc.subwindow_mode));
  case 16: /* "tile" */      return (NspObject *)nspgobject_new((GObject *)gc.tile);
  case 17: /* "ts_x_origin" */      return nsp_new_double_obj((double) (gc.ts_x_origin));
  case 18: /* "ts_y_origin" */      return nsp_new_double_obj((double) (gc.ts_y_origin));
  }
  return NULL;
}
#line 6460 "gdk.c"


#line 1481 "gdk.override"
static int
_wrap_gdk_gc_tp_setattr1(NspObject *self, char *attr, NspObject *value);

static int
_wrap_gdk_gc_tp_setattr(Stack stack, int rhs, int opt, int lhs)
{
  char *attr;
  NspObject *ob;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((ob =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  if ((attr = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ( _wrap_gdk_gc_tp_setattr1(ob,attr,NthObj(3)) == FAIL) return RET_BUG;
  NthObj(1)->ret_pos = 1;
  return 1;
}

static int
_wrap_gdk_gc_tp_setattr1(NspObject *self, char *attr, NspObject *value)
{
  int rep;
  GdkGC *gc = GDK_GC(((NspGObject *)self)->obj);
  
  if ((rep= is_string_in_array(attr, _gdkgc_attrs, 0)) < 0 ) 
    {
      Scierror("attribute %s not found or ambiguous\n",attr);
      return RET_BUG;
    }
  if ( IsMat(value) ) 
    {
      int i;
      double d;
      GdkGCValues v;
      if ( DoubleScalar(value,&d) == FAIL) return RET_BUG;
      i = (int) d;
      gdk_gc_get_values(gc, &v);
      switch (rep) {
      case 2: /* "cap_style" */          gdk_gc_set_line_attributes(gc, v.line_width, v.line_style,
								    i, v.join_style);break;
      case 4: /* "clip_x_origin" */   gdk_gc_set_clip_origin(gc, i, v.clip_y_origin);break;
      case 5: /* "clip_y_origin" */  gdk_gc_set_clip_origin(gc, v.clip_x_origin, i);break;
      case 6: /* "fill" */      gdk_gc_set_fill(gc, i);break;
      case 9: /* "function" */      gdk_gc_set_function(gc, i); break;
      case 10: /* "graphics_exposures"*/    gdk_gc_set_exposures(gc, i);break;
      case 11: /* "join_style" */           gdk_gc_set_line_attributes(gc, v.line_width, v.line_style,
								       v.cap_style, i);break;
      case 12: /* "line_style" */          gdk_gc_set_line_attributes(gc, v.line_width, i,
								      v.cap_style, v.join_style);break;
      case 13: /* "line_width" */           gdk_gc_set_line_attributes(gc, i, v.line_style,
								       v.cap_style, v.join_style);break;
      case 15: /* "subwindow_mode"*/     gdk_gc_set_subwindow(gc, i);break;
      case 17: /* "ts_x_origin" */     gdk_gc_set_ts_origin(gc, i, v.ts_y_origin);break;
      case 18: /* "ts_y_origin" */     gdk_gc_set_ts_origin(gc, v.ts_x_origin, i);break;
      default: 
	Scierror("attribute value for %s should not be an int\n",attr);
	return RET_BUG;
      }
    } 
  else if (nspg_boxed_check(value, GDK_TYPE_COLOR)) 
    {
      GdkColor *c = nspg_boxed_get(value, GdkColor);
      switch (rep) {
      case 1: /* "background" */    gdk_gc_set_foreground(gc, c);break;
      case 8: /* "foreground" */    gdk_gc_set_background(gc, c);break;
      default: 
	Scierror("attribute value for %s should not be a color\n",attr);
	return RET_BUG;
      }
    }
  else if (nspg_boxed_check(value, GDK_TYPE_FONT)) 
    {
      switch (rep) {
      case 7: /* "font" */   gdk_gc_set_font(gc, nspg_boxed_get(value, GdkFont));break;
      default: 
	Scierror("attribute value for %s should not be a font\n",attr);
	return RET_BUG;
      }
    }
  else if ( nspgobject_check((NspGObject *) value,(NspTypeBase *) nsp_type_gdkpixmap)) 
    {
      GdkPixmap *w =  GDK_PIXMAP(nspgobject_get((NspGObject *) value));
      switch (rep) {
      case 3: /* "clip_mask" */  gdk_gc_set_clip_mask(gc, w);break;
      case 14: /* "stipple" */   gdk_gc_set_stipple(gc, w);break;
      case 16: /* "tile" */      gdk_gc_set_tile(gc, w);break;
      default: 
	Scierror("attribute value for %s should be a pixmap\n",attr);
	return RET_BUG;
      }
    }
  else
    {
      Scierror("attribute %s has a wrong value\n",attr);
    }
  return 0;
}
#line 6560 "gdk.c"




/* ----------- GdkImage ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkImage_Private 
#include "nsp/gtk/gdkimage.h"
#include "nsp/interf.h"

/* NspGdkImage inherits from NspGObject */ 

int nsp_type_gdkimage_id=0;
NspTypeGdkImage *nsp_type_gdkimage=NULL;

NspTypeGdkImage *new_type_gdkimage(type_mode mode)
{
  NspTypeGdkImage *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkimage != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkimage;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkimage_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkimage_get_methods; 
  type->new = (new_func *) new_gdkimage;

  /* specific methods for gdkimage */
      
  type->init = (init_func *) init_gdkimage;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkimage */ 

  top->s_type =  (s_type_func *) gdkimage_type_as_string;    
  top->sh_type = (sh_type_func *) gdkimage_type_short_string;
  /* top->create = (create_func*) int_gdkimage_create;*/ 
  
  /* specific methods for gdkimage */
      
  type->init = (init_func *) init_gdkimage;

  if ( nsp_type_gdkimage_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGdkImage called nsp_type_gdkimage
       */
      type->id =  nsp_type_gdkimage_id = nsp_new_type_id();
      nsp_type_gdkimage = type;
      if ( nsp_register_type(nsp_type_gdkimage) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdkimage, GDK_TYPE_IMAGE);
      return ( mode == T_BASE ) ? type : new_type_gdkimage(mode);
    }
  else 
    {
       type->id = nsp_type_gdkimage_id;
       return type;
    }
}

/*
 * initialize GdkImage instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkimage(NspGdkImage *o,NspTypeGdkImage *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkImage 
 */

NspGdkImage *new_gdkimage() 
{
  NspGdkImage *loc; 
  /* type must exists */
  nsp_type_gdkimage = new_type_gdkimage(T_BASE);
  if ( (loc = malloc(sizeof(NspGdkImage)))== NULLGDKIMAGE) return loc;
  /* initialize object */
  if ( init_gdkimage(loc,nsp_type_gdkimage) == FAIL) return NULLGDKIMAGE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GdkImage 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkimage_type_name[]="GdkImage";
static char gdkimage_short_type_name[]="GdkImage";

static char *gdkimage_type_as_string(void)
{
  return(gdkimage_type_name);
}

static char *gdkimage_type_short_string(void)
{
  return(gdkimage_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkImage objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkImage   *gdkimage_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkimage_id) ) return ((NspGdkImage *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkimage));
  return NULL;
}

int IsGdkImageObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkimage_id);
}

int IsGdkImage(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdkimage_id);
}

NspGdkImage  *GetGdkImageCopy(Stack stack, int i)
{
  if (  GetGdkImage(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGdkImage  *GetGdkImage(Stack stack, int i)
{
  NspGdkImage *M;
  if (( M = gdkimage_object(NthObj(i))) == NULLGDKIMAGE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGdkImage *gdkimage_copy(NspGdkImage *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkimage);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdkimage);
}

/*-------------------------------------------------------------------
 * wrappers for the GdkImage
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkImage *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkimage is initialized * /
  nsp_type_gdkimage = new_type_gdkimage(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkimage)) == NULLGDKIMAGE) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_gdkimage_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, obj_check, s_int, s_int,t_end};
  int width, height;
  NspGObject *visual;
  GdkImageType type;
  NspObject *nsp_type = NULL;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type, &nsp_type_gdkvisual, &visual, &width, &height) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_IMAGE_TYPE, nsp_type, (gint *)&type)== FAIL)
      return RET_BUG;
  if ((ret = (GObject *)gdk_image_new(type, GDK_VISUAL(visual->obj), width, height))== NULL) return RET_BUG;

  nsp_type_gdkimage = new_type_gdkimage(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkimage );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_image_put_pixel(NspGdkImage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int,t_end};
  int x, y;
  gulong pixel;

  if ( GetArgs(stack,rhs,opt,T,&x, &y, &pixel) == FAIL) return RET_BUG;
  gdk_image_put_pixel(GDK_IMAGE(self->obj), x, y, pixel);
  return 0;
}

static int _wrap_gdk_image_get_pixel(NspGdkImage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int x, y;
  gulong ret;

  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
  ret = gdk_image_get_pixel(GDK_IMAGE(self->obj), x, y);
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_image_set_colormap(NspGdkImage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *colormap;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkcolormap, &colormap) == FAIL) return RET_BUG;
  gdk_image_set_colormap(GDK_IMAGE(self->obj), GDK_COLORMAP(colormap->obj));
  return 0;
}

static int _wrap_gdk_image_get_colormap(NspGdkImage *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkColormap *ret;

  ret = gdk_image_get_colormap(GDK_IMAGE(self->obj));
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdkimage_methods[] = {
  {"put_pixel",(nsp_method *) _wrap_gdk_image_put_pixel},
  {"get_pixel",(nsp_method *) _wrap_gdk_image_get_pixel},
  {"set_colormap",(nsp_method *) _wrap_gdk_image_set_colormap},
  {"get_colormap",(nsp_method *) _wrap_gdk_image_get_colormap},
  { NULL, NULL}
};

static NspMethods *gdkimage_get_methods(void) { return gdkimage_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkimage_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkKeymap ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkKeymap_Private 
#include "nsp/gtk/gdkkeymap.h"
#include "nsp/interf.h"

/* NspGdkKeymap inherits from NspGObject */ 

int nsp_type_gdkkeymap_id=0;
NspTypeGdkKeymap *nsp_type_gdkkeymap=NULL;

NspTypeGdkKeymap *new_type_gdkkeymap(type_mode mode)
{
  NspTypeGdkKeymap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkkeymap != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkkeymap;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkkeymap_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkkeymap_get_methods; 
  type->new = (new_func *) new_gdkkeymap;

  /* specific methods for gdkkeymap */
      
  type->init = (init_func *) init_gdkkeymap;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkkeymap */ 

  top->s_type =  (s_type_func *) gdkkeymap_type_as_string;    
  top->sh_type = (sh_type_func *) gdkkeymap_type_short_string;
  /* top->create = (create_func*) int_gdkkeymap_create;*/ 
  
  /* specific methods for gdkkeymap */
      
  type->init = (init_func *) init_gdkkeymap;

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
 * initialize GdkKeymap instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkkeymap(NspGdkKeymap *o,NspTypeGdkKeymap *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkKeymap 
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
 * Object method redefined for GdkKeymap 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkkeymap_type_name[]="GdkKeymap";
static char gdkkeymap_short_type_name[]="GdkKeymap";

static char *gdkkeymap_type_as_string(void)
{
  return(gdkkeymap_type_name);
}

static char *gdkkeymap_type_short_string(void)
{
  return(gdkkeymap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkKeymap objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkKeymap   *gdkkeymap_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkkeymap_id) ) return ((NspGdkKeymap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkkeymap));
  return NULL;
}

int IsGdkKeymapObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkkeymap_id);
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
  if (( M = gdkkeymap_object(NthObj(i))) == NULLGDKKEYMAP)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkKeymap *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkkeymap is initialized * /
  nsp_type_gdkkeymap = new_type_gdkkeymap(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkkeymap)) == NULLGDKKEYMAP) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_gdk_keymap_get_direction(NspGdkKeymap *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = gdk_keymap_get_direction(GDK_KEYMAP(self->obj));
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

static AttrTab gdkkeymap_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkPixbuf ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkPixbuf_Private 
#include "nsp/gtk/gdkpixbuf.h"
#include "nsp/interf.h"

/* NspGdkPixbuf inherits from NspGObject */ 

int nsp_type_gdkpixbuf_id=0;
NspTypeGdkPixbuf *nsp_type_gdkpixbuf=NULL;

NspTypeGdkPixbuf *new_type_gdkpixbuf(type_mode mode)
{
  NspTypeGdkPixbuf *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixbuf != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixbuf;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixbuf_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkpixbuf_get_methods; 
  type->new = (new_func *) new_gdkpixbuf;

  /* specific methods for gdkpixbuf */
      
  type->init = (init_func *) init_gdkpixbuf;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkpixbuf */ 

  top->s_type =  (s_type_func *) gdkpixbuf_type_as_string;    
  top->sh_type = (sh_type_func *) gdkpixbuf_type_short_string;
  /* top->create = (create_func*) int_gdkpixbuf_create;*/ 
  
  /* specific methods for gdkpixbuf */
      
  type->init = (init_func *) init_gdkpixbuf;

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
 * initialize GdkPixbuf instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixbuf(NspGdkPixbuf *o,NspTypeGdkPixbuf *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkPixbuf 
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
 * Object method redefined for GdkPixbuf 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkpixbuf_type_name[]="GdkPixbuf";
static char gdkpixbuf_short_type_name[]="GdkPixbuf";

static char *gdkpixbuf_type_as_string(void)
{
  return(gdkpixbuf_type_name);
}

static char *gdkpixbuf_type_short_string(void)
{
  return(gdkpixbuf_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkPixbuf objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkPixbuf   *gdkpixbuf_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkpixbuf_id) ) return ((NspGdkPixbuf *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixbuf));
  return NULL;
}

int IsGdkPixbufObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkpixbuf_id);
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
  if (( M = gdkpixbuf_object(NthObj(i))) == NULLGDKPIXBUF)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkPixbuf *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkpixbuf is initialized * /
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkpixbuf)) == NULLGDKPIXBUF) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_gdk_pixbuf_render_to_drawable(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check, s_int, s_int, s_int, s_int, s_int, s_int, obj, s_int, s_int,t_end};
  int src_x, src_y, dest_x, dest_y, width, height, x_dither, y_dither;
  NspGObject *drawable, *gc;
  NspObject *nsp_dither = NULL;
  GdkRgbDither dither;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdrawable, &drawable, &nsp_type_gdkgc, &gc, &src_x, &src_y, &dest_x, &dest_y, &width, &height, &nsp_dither, &x_dither, &y_dither) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_RGB_DITHER, nsp_dither, (gint *)&dither)== FAIL)
      return RET_BUG;
  gdk_pixbuf_render_to_drawable(GDK_PIXBUF(self->obj), GDK_DRAWABLE(drawable->obj), GDK_GC(gc->obj), src_x, src_y, dest_x, dest_y, width, height, dither, x_dither, y_dither);
  return 0;
}

static int _wrap_gdk_pixbuf_render_to_drawable_alpha(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int, s_int, s_int, s_int, s_int, obj, s_int, obj, s_int, s_int,t_end};
  int src_x, src_y, dest_x, dest_y, width, height, alpha_threshold, x_dither, y_dither;
  NspGObject *drawable;
  NspObject *nsp_alpha_mode = NULL, *nsp_dither = NULL;
  GdkPixbufAlphaMode alpha_mode;
  GdkRgbDither dither;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdrawable, &drawable, &src_x, &src_y, &dest_x, &dest_y, &width, &height, &nsp_alpha_mode, &alpha_threshold, &nsp_dither, &x_dither, &y_dither) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_PIXBUF_ALPHA_MODE, nsp_alpha_mode, (gint *)&alpha_mode)== FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_RGB_DITHER, nsp_dither, (gint *)&dither)== FAIL)
      return RET_BUG;
  gdk_pixbuf_render_to_drawable_alpha(GDK_PIXBUF(self->obj), GDK_DRAWABLE(drawable->obj), src_x, src_y, dest_x, dest_y, width, height, alpha_mode, alpha_threshold, dither, x_dither, y_dither);
  return 0;
}

#line 1911 "gdk.override"
static int
_wrap_gdk_pixbuf_render_pixmap_and_mask(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "alpha_threshold", NULL};*/
  int alpha_threshold = 127;
  GdkPixmap *pixmap;
  GdkBitmap *mask;
  NspObject *nsp_pixmap, *nsp_mask;

  CheckRhs(0,1);
  CheckLhs(1,2);
  if ( rhs == 1) 
    {
      if ( GetScalarInt(stack,1,&alpha_threshold) == FAIL) return RET_BUG; 
    }
  if ( lhs == 2) 
    {
      gdk_pixbuf_render_pixmap_and_mask(GDK_PIXBUF(self->obj), &pixmap, &mask, alpha_threshold);
      if (pixmap == NULL ) 
	{
	  Scierror("%s: can't get pixmap from pixbuf\n",stack.fname);
	  return RET_BUG; 
	}

	if ( mask == NULL ) 
	{
	  Scierror("%s: can't get mask from pixbuf\n",stack.fname);
	  return RET_BUG; 
	}
      nsp_pixmap = (NspObject *) nspgobject_new((GObject *) pixmap);
      nsp_mask = (NspObject *) nspgobject_new((GObject *) mask);
      if ( nsp_pixmap == NULL || nsp_mask == NULL )  return RET_BUG; 
      MoveObj(stack,1,nsp_pixmap);
      MoveObj(stack,2,nsp_mask);
      return 2;
    }
  else 
    {
      gdk_pixbuf_render_pixmap_and_mask(GDK_PIXBUF(self->obj), &pixmap,NULL, alpha_threshold);
      if (pixmap == NULL || mask == NULL ) 
	{
	  return RET_BUG; 
	}
      nsp_pixmap = (NspObject *) nspgobject_new((GObject *) pixmap);
      if ( nsp_pixmap == NULL )  return RET_BUG; 
      MoveObj(stack,1,nsp_pixmap);
      return 1;
    }
}
#line 7332 "gdk.c"


static int _wrap_gdk_pixbuf_get_from_drawable(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check, s_int, s_int, s_int, s_int, s_int, s_int,t_end};
  int src_x, src_y, dest_x, dest_y, width, height;
  NspGObject *src, *cmap;
  GdkPixbuf *ret;
  NspObject *nsp_ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdrawable, &src, &nsp_type_gdkcolormap, &cmap, &src_x, &src_y, &dest_x, &dest_y, &width, &height) == FAIL) return RET_BUG;
  ret = gdk_pixbuf_get_from_drawable(GDK_PIXBUF(self->obj), GDK_DRAWABLE(src->obj), GDK_COLORMAP(cmap->obj), src_x, src_y, dest_x, dest_y, width, height);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_get_from_image(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check, s_int, s_int, s_int, s_int, s_int, s_int,t_end};
  int src_x, src_y, dest_x, dest_y, width, height;
  NspGObject *src, *cmap;
  GdkPixbuf *ret;
  NspObject *nsp_ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkimage, &src, &nsp_type_gdkcolormap, &cmap, &src_x, &src_y, &dest_x, &dest_y, &width, &height) == FAIL) return RET_BUG;
  ret = gdk_pixbuf_get_from_image(GDK_PIXBUF(self->obj), GDK_IMAGE(src->obj), GDK_COLORMAP(cmap->obj), src_x, src_y, dest_x, dest_y, width, height);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_get_n_channels(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_get_n_channels(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_has_alpha(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_get_has_alpha(GDK_PIXBUF(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_bits_per_sample(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_get_bits_per_sample(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 1982 "gdk.override"
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
    Scierror("could not get pixel data");
    return RET_BUG;
  }
  if ((ret = nsp_new_string_obj(NVOID,pixels, rowstride*height)) == NULL) return RET_BUG; 
  MoveObj(stack,1,ret);
  return 1;
}
#line 7416 "gdk.c"


static int _wrap_gdk_pixbuf_get_width(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_get_width(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_height(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_get_height(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_rowstride(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_get_rowstride(GDK_PIXBUF(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_get_option(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *key;
  const gchar *ret;

  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
  ret = gdk_pixbuf_get_option(GDK_PIXBUF(self->obj), key);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_copy(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;

  ret = gdk_pixbuf_copy(GDK_PIXBUF(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  g_object_unref(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_fill(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int pixel;

  if ( GetArgs(stack,rhs,opt,T,&pixel) == FAIL) return RET_BUG;
  gdk_pixbuf_fill(GDK_PIXBUF(self->obj), pixel);
  return 0;
}

#line 2005 "gdk.override"
static int
_wrap_gdk_pixbuf_save(NspGObject *self, Stack stack,int rhs,int opt,int lhs)

{
  /* static char *kwlist[] = {"filename", "type", "options", NULL};*/
  /* 
     gchar *filename, *type, **option_keys = NULL, **option_values = NULL;
  NspObject *nsp_options = NULL;
  GError *error = NULL;

  if (GetArgs(stack,rhs,opt,T,
	      / * "ss|O!:GdkPixbuf.save" * /
	      &filename, &type,
	      &nsp_type_dict, &nsp_options) == FAIL )
    return RET_BUG;

  if (nsp_options != NULL) {
    guint len;
    gint pos = 0, i = 0;
    NspObject *key, *value;
	
    len = NspDict_Size(nsp_options);
    option_keys = g_new(gchar *, len + 1);
    option_values = g_new(gchar *, len + 1);

    while (NspDict_Next(nsp_options, &pos, &key, &value)) {
      if (!NspString_Check(key) || !NspString_Check(value)) {
	g_free(option_keys);
	g_free(option_values);
	Scierror(
		 "keys and values must be strings");
	return RET_BUG;
      }

      option_keys[i]   = NspString_AsString(key);
      option_values[i] = NspString_AsString(value);
      i++;
    }
	
    option_keys[len] = NULL;
    option_values[len] = NULL;
  }
  gdk_pixbuf_savev(GDK_PIXBUF(self->obj), filename, type,
		   option_keys, option_values, &error);
	
  g_free(option_keys);
  g_free(option_values);
 
  if (nspg_error_check(&error))
    return RET_BUG;
  */
  Scierror("To be done XXXXXXX ");
  return 0;
}
#line 7536 "gdk.c"


static int _wrap_gdk_pixbuf_add_alpha(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, s_int, s_int, s_int,t_end};
  int substitute_color, r, g, b;
  GdkPixbuf *ret;
  NspObject *nsp_ret;

  if ( GetArgs(stack,rhs,opt,T,&substitute_color, &r, &g, &b) == FAIL) return RET_BUG;
  ret = gdk_pixbuf_add_alpha(GDK_PIXBUF(self->obj), substitute_color, r, g, b);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  g_object_unref(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_copy_area(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, s_int, s_int, obj_check, s_int, s_int,t_end};
  int src_x, src_y, width, height, dest_x, dest_y;
  NspGObject *dest_pixbuf;

  if ( GetArgs(stack,rhs,opt,T,&src_x, &src_y, &width, &height, &nsp_type_gdkpixbuf, &dest_pixbuf, &dest_x, &dest_y) == FAIL) return RET_BUG;
  gdk_pixbuf_copy_area(GDK_PIXBUF(self->obj), src_x, src_y, width, height, GDK_PIXBUF(dest_pixbuf->obj), dest_x, dest_y);
  return 0;
}

static int _wrap_gdk_pixbuf_saturate_and_pixelate(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_double, s_bool,t_end};
  int pixelate;
  NspGObject *dest;
  double saturation;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &dest, &saturation, &pixelate) == FAIL) return RET_BUG;
  gdk_pixbuf_saturate_and_pixelate(GDK_PIXBUF(self->obj), GDK_PIXBUF(dest->obj), saturation, pixelate);
  return 0;
}

static int _wrap_gdk_pixbuf_scale(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int, s_int, s_int, s_double, s_double, s_double, s_double, obj,t_end};
  int dest_x, dest_y, dest_width, dest_height;
  NspGObject *dest;
  GdkInterpType interp_type;
  NspObject *nsp_interp_type = NULL;
  double offset_x, offset_y, scale_x, scale_y;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &dest, &dest_x, &dest_y, &dest_width, &dest_height, &offset_x, &offset_y, &scale_x, &scale_y, &nsp_interp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, (gint *)&interp_type)== FAIL)
      return RET_BUG;
  gdk_pixbuf_scale(GDK_PIXBUF(self->obj), GDK_PIXBUF(dest->obj), dest_x, dest_y, dest_width, dest_height, offset_x, offset_y, scale_x, scale_y, interp_type);
  return 0;
}

static int _wrap_gdk_pixbuf_composite(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int, s_int, s_int, s_double, s_double, s_double, s_double, obj, s_int,t_end};
  int dest_x, dest_y, dest_width, dest_height, overall_alpha;
  NspGObject *dest;
  GdkInterpType interp_type;
  NspObject *nsp_interp_type = NULL;
  double offset_x, offset_y, scale_x, scale_y;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &dest, &dest_x, &dest_y, &dest_width, &dest_height, &offset_x, &offset_y, &scale_x, &scale_y, &nsp_interp_type, &overall_alpha) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, (gint *)&interp_type)== FAIL)
      return RET_BUG;
  gdk_pixbuf_composite(GDK_PIXBUF(self->obj), GDK_PIXBUF(dest->obj), dest_x, dest_y, dest_width, dest_height, offset_x, offset_y, scale_x, scale_y, interp_type, overall_alpha);
  return 0;
}

static int _wrap_gdk_pixbuf_composite_color(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, s_int, s_int, s_int, s_int, s_double, s_double, s_double, s_double, obj, s_int, s_int, s_int, s_int, s_int, s_int,t_end};
  NspObject *nsp_interp_type = NULL;
  int dest_x, dest_y, dest_width, dest_height, overall_alpha, check_x, check_y, check_size;
  NspGObject *dest;
  gulong color1, color2;
  double offset_x, offset_y, scale_x, scale_y;
  GdkInterpType interp_type;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &dest, &dest_x, &dest_y, &dest_width, &dest_height, &offset_x, &offset_y, &scale_x, &scale_y, &nsp_interp_type, &overall_alpha, &check_x, &check_y, &check_size, &color1, &color2) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, (gint *)&interp_type)== FAIL)
      return RET_BUG;
  gdk_pixbuf_composite_color(GDK_PIXBUF(self->obj), GDK_PIXBUF(dest->obj), dest_x, dest_y, dest_width, dest_height, offset_x, offset_y, scale_x, scale_y, interp_type, overall_alpha, check_x, check_y, check_size, color1, color2);
  return 0;
}

static int _wrap_gdk_pixbuf_scale_simple(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, obj,t_end};
  int dest_width, dest_height;
  GdkPixbuf *ret;
  NspObject *nsp_interp_type = NULL, *nsp_ret;
  GdkInterpType interp_type;

  if ( GetArgs(stack,rhs,opt,T,&dest_width, &dest_height, &nsp_interp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, (gint *)&interp_type)== FAIL)
      return RET_BUG;
  ret = gdk_pixbuf_scale_simple(GDK_PIXBUF(self->obj), dest_width, dest_height, interp_type);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  g_object_unref(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_composite_color_simple(NspGdkPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int, obj, s_int, s_int, s_int, s_int,t_end};
  int dest_width, dest_height, overall_alpha, check_size;
  GdkPixbuf *ret;
  NspObject *nsp_interp_type = NULL, *nsp_ret;
  gulong color1, color2;
  GdkInterpType interp_type;

  if ( GetArgs(stack,rhs,opt,T,&dest_width, &dest_height, &nsp_interp_type, &overall_alpha, &check_size, &color1, &color2) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GDK_TYPE_INTERP_TYPE, nsp_interp_type, (gint *)&interp_type)== FAIL)
      return RET_BUG;
  ret = gdk_pixbuf_composite_color_simple(GDK_PIXBUF(self->obj), dest_width, dest_height, interp_type, overall_alpha, check_size, color1, color2);
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  g_object_unref(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdkpixbuf_methods[] = {
  {"render_to_drawable",(nsp_method *) _wrap_gdk_pixbuf_render_to_drawable},
  {"render_to_drawable_alpha",(nsp_method *) _wrap_gdk_pixbuf_render_to_drawable_alpha},
  {"render_pixmap_and_mask",(nsp_method *) _wrap_gdk_pixbuf_render_pixmap_and_mask},
  {"get_from_drawable",(nsp_method *) _wrap_gdk_pixbuf_get_from_drawable},
  {"get_from_image",(nsp_method *) _wrap_gdk_pixbuf_get_from_image},
  {"get_n_channels",(nsp_method *) _wrap_gdk_pixbuf_get_n_channels},
  {"get_has_alpha",(nsp_method *) _wrap_gdk_pixbuf_get_has_alpha},
  {"get_bits_per_sample",(nsp_method *) _wrap_gdk_pixbuf_get_bits_per_sample},
  {"get_pixels",(nsp_method *) _wrap_gdk_pixbuf_get_pixels},
  {"get_width",(nsp_method *) _wrap_gdk_pixbuf_get_width},
  {"get_height",(nsp_method *) _wrap_gdk_pixbuf_get_height},
  {"get_rowstride",(nsp_method *) _wrap_gdk_pixbuf_get_rowstride},
  {"get_option",(nsp_method *) _wrap_gdk_pixbuf_get_option},
  {"copy",(nsp_method *) _wrap_gdk_pixbuf_copy},
  {"fill",(nsp_method *) _wrap_gdk_pixbuf_fill},
  {"save",(nsp_method *) _wrap_gdk_pixbuf_save},
  {"add_alpha",(nsp_method *) _wrap_gdk_pixbuf_add_alpha},
  {"copy_area",(nsp_method *) _wrap_gdk_pixbuf_copy_area},
  {"saturate_and_pixelate",(nsp_method *) _wrap_gdk_pixbuf_saturate_and_pixelate},
  {"scale",(nsp_method *) _wrap_gdk_pixbuf_scale},
  {"composite",(nsp_method *) _wrap_gdk_pixbuf_composite},
  {"composite_color",(nsp_method *) _wrap_gdk_pixbuf_composite_color},
  {"scale_simple",(nsp_method *) _wrap_gdk_pixbuf_scale_simple},
  {"composite_color_simple",(nsp_method *) _wrap_gdk_pixbuf_composite_color_simple},
  { NULL, NULL}
};

static NspMethods *gdkpixbuf_get_methods(void) { return gdkpixbuf_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

#line 1962 "gdk.override"
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
#line 7718 "gdk.c"
static AttrTab gdkpixbuf_attrs[] = {
  { "pixel_array", (attr_get_function *)_wrap_gdk_pixbuf__get_pixel_array, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- GdkPixbufAnimation ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkPixbufAnimation_Private 
#include "nsp/gtk/gdkpixbufanimation.h"
#include "nsp/interf.h"

/* NspGdkPixbufAnimation inherits from NspGObject */ 

int nsp_type_gdkpixbufanimation_id=0;
NspTypeGdkPixbufAnimation *nsp_type_gdkpixbufanimation=NULL;

NspTypeGdkPixbufAnimation *new_type_gdkpixbufanimation(type_mode mode)
{
  NspTypeGdkPixbufAnimation *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixbufanimation != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixbufanimation;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixbufanimation_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkpixbufanimation_get_methods; 
  type->new = (new_func *) new_gdkpixbufanimation;

  /* specific methods for gdkpixbufanimation */
      
  type->init = (init_func *) init_gdkpixbufanimation;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkpixbufanimation */ 

  top->s_type =  (s_type_func *) gdkpixbufanimation_type_as_string;    
  top->sh_type = (sh_type_func *) gdkpixbufanimation_type_short_string;
  /* top->create = (create_func*) int_gdkpixbufanimation_create;*/ 
  
  /* specific methods for gdkpixbufanimation */
      
  type->init = (init_func *) init_gdkpixbufanimation;

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
 * initialize GdkPixbufAnimation instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixbufanimation(NspGdkPixbufAnimation *o,NspTypeGdkPixbufAnimation *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkPixbufAnimation 
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
 * Object method redefined for GdkPixbufAnimation 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkpixbufanimation_type_name[]="GdkPixbufAnimation";
static char gdkpixbufanimation_short_type_name[]="GdkPixbufAnimation";

static char *gdkpixbufanimation_type_as_string(void)
{
  return(gdkpixbufanimation_type_name);
}

static char *gdkpixbufanimation_type_short_string(void)
{
  return(gdkpixbufanimation_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkPixbufAnimation objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkPixbufAnimation   *gdkpixbufanimation_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkpixbufanimation_id) ) return ((NspGdkPixbufAnimation *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixbufanimation));
  return NULL;
}

int IsGdkPixbufAnimationObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkpixbufanimation_id);
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
  if (( M = gdkpixbufanimation_object(NthObj(i))) == NULLGDKPIXBUFANIMATION)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkPixbufAnimation *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkpixbufanimation is initialized * /
  nsp_type_gdkpixbufanimation = new_type_gdkpixbufanimation(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkpixbufanimation)) == NULLGDKPIXBUFANIMATION) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_gdkpixbufanimation_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *filename;
  GError *error = NULL;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_animation_new_from_file(filename, &error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",stack.fname);
    return RET_BUG;
  }

  nsp_type_gdkpixbufanimation = new_type_gdkpixbufanimation(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkpixbufanimation );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_animation_get_width(NspGdkPixbufAnimation *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_animation_get_width(GDK_PIXBUF_ANIMATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_animation_get_height(NspGdkPixbufAnimation *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_animation_get_height(GDK_PIXBUF_ANIMATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_animation_is_static_image(NspGdkPixbufAnimation *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_animation_is_static_image(GDK_PIXBUF_ANIMATION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_animation_get_static_image(NspGdkPixbufAnimation *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;

  ret = gdk_pixbuf_animation_get_static_image(GDK_PIXBUF_ANIMATION(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
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

static AttrTab gdkpixbufanimation_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkPixbufAnimationIter ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkPixbufAnimationIter_Private 
#include "nsp/gtk/gdkpixbufanimationiter.h"
#include "nsp/interf.h"

/* NspGdkPixbufAnimationIter inherits from NspGObject */ 

int nsp_type_gdkpixbufanimationiter_id=0;
NspTypeGdkPixbufAnimationIter *nsp_type_gdkpixbufanimationiter=NULL;

NspTypeGdkPixbufAnimationIter *new_type_gdkpixbufanimationiter(type_mode mode)
{
  NspTypeGdkPixbufAnimationIter *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixbufanimationiter != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixbufanimationiter;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixbufanimationiter_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkpixbufanimationiter_get_methods; 
  type->new = (new_func *) new_gdkpixbufanimationiter;

  /* specific methods for gdkpixbufanimationiter */
      
  type->init = (init_func *) init_gdkpixbufanimationiter;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkpixbufanimationiter */ 

  top->s_type =  (s_type_func *) gdkpixbufanimationiter_type_as_string;    
  top->sh_type = (sh_type_func *) gdkpixbufanimationiter_type_short_string;
  /* top->create = (create_func*) int_gdkpixbufanimationiter_create;*/ 
  
  /* specific methods for gdkpixbufanimationiter */
      
  type->init = (init_func *) init_gdkpixbufanimationiter;

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
 * initialize GdkPixbufAnimationIter instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixbufanimationiter(NspGdkPixbufAnimationIter *o,NspTypeGdkPixbufAnimationIter *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkPixbufAnimationIter 
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
 * Object method redefined for GdkPixbufAnimationIter 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkpixbufanimationiter_type_name[]="GdkPixbufAnimationIter";
static char gdkpixbufanimationiter_short_type_name[]="GdkPixbufAnimationIter";

static char *gdkpixbufanimationiter_type_as_string(void)
{
  return(gdkpixbufanimationiter_type_name);
}

static char *gdkpixbufanimationiter_type_short_string(void)
{
  return(gdkpixbufanimationiter_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkPixbufAnimationIter objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkPixbufAnimationIter   *gdkpixbufanimationiter_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkpixbufanimationiter_id) ) return ((NspGdkPixbufAnimationIter *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixbufanimationiter));
  return NULL;
}

int IsGdkPixbufAnimationIterObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkpixbufanimationiter_id);
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
  if (( M = gdkpixbufanimationiter_object(NthObj(i))) == NULLGDKPIXBUFANIMATIONITER)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkPixbufAnimationIter *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkpixbufanimationiter is initialized * /
  nsp_type_gdkpixbufanimationiter = new_type_gdkpixbufanimationiter(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkpixbufanimationiter)) == NULLGDKPIXBUFANIMATIONITER) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_gdk_pixbuf_animation_iter_get_delay_time(NspGdkPixbufAnimationIter *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_animation_iter_get_delay_time(GDK_PIXBUF_ANIMATION_ITER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_animation_iter_get_pixbuf(NspGdkPixbufAnimationIter *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;

  ret = gdk_pixbuf_animation_iter_get_pixbuf(GDK_PIXBUF_ANIMATION_ITER(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_animation_iter_on_currently_loading_frame(NspGdkPixbufAnimationIter *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_pixbuf_animation_iter_on_currently_loading_frame(GDK_PIXBUF_ANIMATION_ITER(self->obj));
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

static AttrTab gdkpixbufanimationiter_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkPixbufLoader ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkPixbufLoader_Private 
#include "nsp/gtk/gdkpixbufloader.h"
#include "nsp/interf.h"

/* NspGdkPixbufLoader inherits from NspGObject */ 

int nsp_type_gdkpixbufloader_id=0;
NspTypeGdkPixbufLoader *nsp_type_gdkpixbufloader=NULL;

NspTypeGdkPixbufLoader *new_type_gdkpixbufloader(type_mode mode)
{
  NspTypeGdkPixbufLoader *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkpixbufloader != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkpixbufloader;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkpixbufloader_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkpixbufloader_get_methods; 
  type->new = (new_func *) new_gdkpixbufloader;

  /* specific methods for gdkpixbufloader */
      
  type->init = (init_func *) init_gdkpixbufloader;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkpixbufloader */ 

  top->s_type =  (s_type_func *) gdkpixbufloader_type_as_string;    
  top->sh_type = (sh_type_func *) gdkpixbufloader_type_short_string;
  /* top->create = (create_func*) int_gdkpixbufloader_create;*/ 
  
  /* specific methods for gdkpixbufloader */
      
  type->init = (init_func *) init_gdkpixbufloader;

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
 * initialize GdkPixbufLoader instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkpixbufloader(NspGdkPixbufLoader *o,NspTypeGdkPixbufLoader *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkPixbufLoader 
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
 * Object method redefined for GdkPixbufLoader 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkpixbufloader_type_name[]="GdkPixbufLoader";
static char gdkpixbufloader_short_type_name[]="GdkPixbufLoader";

static char *gdkpixbufloader_type_as_string(void)
{
  return(gdkpixbufloader_type_name);
}

static char *gdkpixbufloader_type_short_string(void)
{
  return(gdkpixbufloader_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkPixbufLoader objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkPixbufLoader   *gdkpixbufloader_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkpixbufloader_id) ) return ((NspGdkPixbufLoader *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkpixbufloader));
  return NULL;
}

int IsGdkPixbufLoaderObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkpixbufloader_id);
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
  if (( M = gdkpixbufloader_object(NthObj(i))) == NULLGDKPIXBUFLOADER)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkPixbufLoader *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkpixbufloader is initialized * /
  nsp_type_gdkpixbufloader = new_type_gdkpixbufloader(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkpixbufloader)) == NULLGDKPIXBUFLOADER) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_gdkpixbufloader_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *image_type;
  GError *error = NULL;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&image_type) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_pixbuf_loader_new_with_type(image_type, &error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",stack.fname);
    return RET_BUG;
  }

  nsp_type_gdkpixbufloader = new_type_gdkpixbufloader(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkpixbufloader );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_loader_write(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, s_int,t_end};
  int count, ret;
  guchar *buf;
  GError *error = NULL;

  if ( GetArgs(stack,rhs,opt,T,&buf, &count) == FAIL) return RET_BUG;
  ret = gdk_pixbuf_loader_write(GDK_PIXBUF_LOADER(self->obj), buf, count, &error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",stack.fname);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_pixbuf_loader_get_pixbuf(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;

  ret = gdk_pixbuf_loader_get_pixbuf(GDK_PIXBUF_LOADER(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_loader_get_animation(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkPixbufAnimation *ret;

  ret = gdk_pixbuf_loader_get_animation(GDK_PIXBUF_LOADER(self->obj));
  nsp_type_gdkpixbufanimation = new_type_gdkpixbufanimation(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbufanimation))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_pixbuf_loader_close(NspGdkPixbufLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  GError *error = NULL;

  ret = gdk_pixbuf_loader_close(GDK_PIXBUF_LOADER(self->obj), &error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",stack.fname);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdkpixbufloader_methods[] = {
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

static AttrTab gdkpixbufloader_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkVisual ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkVisual_Private 
#include "nsp/gtk/gdkvisual.h"
#include "nsp/interf.h"

/* NspGdkVisual inherits from NspGObject */ 

int nsp_type_gdkvisual_id=0;
NspTypeGdkVisual *nsp_type_gdkvisual=NULL;

NspTypeGdkVisual *new_type_gdkvisual(type_mode mode)
{
  NspTypeGdkVisual *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkvisual != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkvisual;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkvisual_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkvisual_get_methods; 
  type->new = (new_func *) new_gdkvisual;

  /* specific methods for gdkvisual */
      
  type->init = (init_func *) init_gdkvisual;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkvisual */ 

  top->s_type =  (s_type_func *) gdkvisual_type_as_string;    
  top->sh_type = (sh_type_func *) gdkvisual_type_short_string;
  /* top->create = (create_func*) int_gdkvisual_create;*/ 
  
  /* specific methods for gdkvisual */
      
  type->init = (init_func *) init_gdkvisual;

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
 * initialize GdkVisual instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkvisual(NspGdkVisual *o,NspTypeGdkVisual *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkVisual 
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
 * Object method redefined for GdkVisual 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkvisual_type_name[]="GdkVisual";
static char gdkvisual_short_type_name[]="GdkVisual";

static char *gdkvisual_type_as_string(void)
{
  return(gdkvisual_type_name);
}

static char *gdkvisual_type_short_string(void)
{
  return(gdkvisual_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkVisual objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkVisual   *gdkvisual_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkvisual_id) ) return ((NspGdkVisual *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkvisual));
  return NULL;
}

int IsGdkVisualObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkvisual_id);
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
  if (( M = gdkvisual_object(NthObj(i))) == NULLGDKVISUAL)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkVisual *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkvisual is initialized * /
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkvisual)) == NULLGDKVISUAL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 2210 "gdk.override"
static char *_visual[]={ "best_depth", "best_type", "get_system", NULL };
static int
_wrap_gdkvisual_new(Stack stack, int rhs, int opt, int lhs)
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
	      if (nspg_enum_get_value(GDK_TYPE_VISUAL_TYPE, opts[1].obj, (gint *)&visual_type)== FAIL)
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
	      if (nspg_enum_get_value(GDK_TYPE_VISUAL_TYPE, opts[1].obj, (gint *)&visual_type)== FAIL)
		return RET_BUG;
	      ret =(GObject *) gdk_visual_get_best_with_both(depth, visual_type);
	    }
	}
      if ( ret == NULL) 
	{
	  Scierror("Unable to create visual \n");
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
#line 8792 "gdk.c"


static NspMethods *gdkvisual_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdk_visual__get_type(NspObject *self,char *attr)
{
  gint ret;

  ret = (GdkVisualType) GDK_VISUAL(NSP_GOBJECT_GET(self))->type;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_depth(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->depth;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_byte_order(NspObject *self,char *attr)
{
  gint ret;

  ret = (GdkByteOrder) GDK_VISUAL(NSP_GOBJECT_GET(self))->byte_order;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_colormap_size(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->colormap_size;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_bits_per_rgb(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->bits_per_rgb;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_red_mask(NspObject *self,char *attr)
{
  NspObject *nsp_ret;
  gulong ret;

  ret = (guint32) GDK_VISUAL(NSP_GOBJECT_GET(self))->red_mask;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_gdk_visual__get_red_shift(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->red_shift;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_red_prec(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->red_prec;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_green_mask(NspObject *self,char *attr)
{
  NspObject *nsp_ret;
  gulong ret;

  ret = (guint32) GDK_VISUAL(NSP_GOBJECT_GET(self))->green_mask;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_gdk_visual__get_green_shift(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->green_shift;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_green_prec(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->green_prec;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_blue_mask(NspObject *self,char *attr)
{
  NspObject *nsp_ret;
  gulong ret;

  ret = (guint32) GDK_VISUAL(NSP_GOBJECT_GET(self))->blue_mask;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static NspObject *_wrap_gdk_visual__get_blue_shift(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->blue_shift;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_gdk_visual__get_blue_prec(NspObject *self,char *attr)
{
  int ret;

  ret = (gint) GDK_VISUAL(NSP_GOBJECT_GET(self))->blue_prec;
  return nsp_new_double_obj((double) ret);
}

static AttrTab gdkvisual_attrs[] = {
  { "type", (attr_get_function *)_wrap_gdk_visual__get_type, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "depth", (attr_get_function *)_wrap_gdk_visual__get_depth, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "byte_order", (attr_get_function *)_wrap_gdk_visual__get_byte_order, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "colormap_size", (attr_get_function *)_wrap_gdk_visual__get_colormap_size, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "bits_per_rgb", (attr_get_function *)_wrap_gdk_visual__get_bits_per_rgb, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "red_mask", (attr_get_function *)_wrap_gdk_visual__get_red_mask, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "red_shift", (attr_get_function *)_wrap_gdk_visual__get_red_shift, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "red_prec", (attr_get_function *)_wrap_gdk_visual__get_red_prec, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "green_mask", (attr_get_function *)_wrap_gdk_visual__get_green_mask, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "green_shift", (attr_get_function *)_wrap_gdk_visual__get_green_shift, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "green_prec", (attr_get_function *)_wrap_gdk_visual__get_green_prec, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "blue_mask", (attr_get_function *)_wrap_gdk_visual__get_blue_mask, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "blue_shift", (attr_get_function *)_wrap_gdk_visual__get_blue_shift, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "blue_prec", (attr_get_function *)_wrap_gdk_visual__get_blue_prec, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- GdkScreen ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkScreen_Private 
#include "nsp/gtk/gdkscreen.h"
#include "nsp/interf.h"

/* NspGdkScreen inherits from NspGObject */ 

int nsp_type_gdkscreen_id=0;
NspTypeGdkScreen *nsp_type_gdkscreen=NULL;

NspTypeGdkScreen *new_type_gdkscreen(type_mode mode)
{
  NspTypeGdkScreen *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkscreen != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkscreen;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkscreen_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkscreen_get_methods; 
  type->new = (new_func *) new_gdkscreen;

  /* specific methods for gdkscreen */
      
  type->init = (init_func *) init_gdkscreen;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkscreen */ 

  top->s_type =  (s_type_func *) gdkscreen_type_as_string;    
  top->sh_type = (sh_type_func *) gdkscreen_type_short_string;
  /* top->create = (create_func*) int_gdkscreen_create;*/ 
  
  /* specific methods for gdkscreen */
      
  type->init = (init_func *) init_gdkscreen;

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
 * initialize GdkScreen instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkscreen(NspGdkScreen *o,NspTypeGdkScreen *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkScreen 
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
 * Object method redefined for GdkScreen 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkscreen_type_name[]="GdkScreen";
static char gdkscreen_short_type_name[]="GdkScreen";

static char *gdkscreen_type_as_string(void)
{
  return(gdkscreen_type_name);
}

static char *gdkscreen_type_short_string(void)
{
  return(gdkscreen_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkScreen objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkScreen   *gdkscreen_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkscreen_id) ) return ((NspGdkScreen *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkscreen));
  return NULL;
}

int IsGdkScreenObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkscreen_id);
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
  if (( M = gdkscreen_object(NthObj(i))) == NULLGDKSCREEN)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkScreen *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkscreen is initialized * /
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkscreen)) == NULLGDKSCREEN) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_gdk_screen_get_default_colormap(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkColormap *ret;

  ret = gdk_screen_get_default_colormap(GDK_SCREEN(self->obj));
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_set_default_colormap(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *colormap;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkcolormap, &colormap) == FAIL) return RET_BUG;
  gdk_screen_set_default_colormap(GDK_SCREEN(self->obj), GDK_COLORMAP(colormap->obj));
  return 0;
}

static int _wrap_gdk_screen_get_system_colormap(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkColormap *ret;

  ret = gdk_screen_get_system_colormap(GDK_SCREEN(self->obj));
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_system_visual(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkVisual *ret;

  ret = gdk_screen_get_system_visual(GDK_SCREEN(self->obj));
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_rgb_colormap(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkColormap *ret;

  ret = gdk_screen_get_rgb_colormap(GDK_SCREEN(self->obj));
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_rgb_visual(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkVisual *ret;

  ret = gdk_screen_get_rgb_visual(GDK_SCREEN(self->obj));
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_root_window(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkWindow *ret;

  ret = gdk_screen_get_root_window(GDK_SCREEN(self->obj));
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_display(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;

  ret = gdk_screen_get_display(GDK_SCREEN(self->obj));
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_screen_get_number(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_screen_get_number(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_width(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_screen_get_width(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_height(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_screen_get_height(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_width_mm(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_screen_get_width_mm(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_height_mm(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_screen_get_height_mm(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_make_display_name(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;

  ret = gdk_screen_make_display_name(GDK_SCREEN(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gdk_screen_get_n_monitors(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_screen_get_n_monitors(GDK_SCREEN(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_monitor_geometry(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, obj,t_end};
  int monitor_num;
  GdkRectangle dest = { 0, 0, 0, 0 };
  NspObject *nsp_dest;

  if ( GetArgs(stack,rhs,opt,T,&monitor_num, &nsp_dest) == FAIL) return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_dest, &dest))
      return RET_BUG;
  gdk_screen_get_monitor_geometry(GDK_SCREEN(self->obj), monitor_num, &dest);
  return 0;
}

static int _wrap_gdk_screen_get_monitor_at_point(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int x, y, ret;

  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
  ret = gdk_screen_get_monitor_at_point(GDK_SCREEN(self->obj), x, y);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_get_monitor_at_window(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  int ret;
  NspGObject *window;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window) == FAIL) return RET_BUG;
  ret = gdk_screen_get_monitor_at_window(GDK_SCREEN(self->obj), GDK_WINDOW(window->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_screen_broadcast_client_message(NspGdkScreen *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GdkEvent *event = NULL;
  NspObject *nsp_event;

  if ( GetArgs(stack,rhs,opt,T,&nsp_event) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_event, GDK_TYPE_EVENT))
      event = nspg_boxed_get(nsp_event, GdkEvent);
  else {
      Scierror( "event should be a GdkEvent");
      return RET_BUG;
  }
  gdk_screen_broadcast_client_message(GDK_SCREEN(self->obj), event);
  return 0;
}

static NspMethods gdkscreen_methods[] = {
  {"get_default_colormap",(nsp_method *) _wrap_gdk_screen_get_default_colormap},
  {"set_default_colormap",(nsp_method *) _wrap_gdk_screen_set_default_colormap},
  {"get_system_colormap",(nsp_method *) _wrap_gdk_screen_get_system_colormap},
  {"get_system_visual",(nsp_method *) _wrap_gdk_screen_get_system_visual},
  {"get_rgb_colormap",(nsp_method *) _wrap_gdk_screen_get_rgb_colormap},
  {"get_rgb_visual",(nsp_method *) _wrap_gdk_screen_get_rgb_visual},
  {"get_root_window",(nsp_method *) _wrap_gdk_screen_get_root_window},
  {"get_display",(nsp_method *) _wrap_gdk_screen_get_display},
  {"get_number",(nsp_method *) _wrap_gdk_screen_get_number},
  {"get_width",(nsp_method *) _wrap_gdk_screen_get_width},
  {"get_height",(nsp_method *) _wrap_gdk_screen_get_height},
  {"get_width_mm",(nsp_method *) _wrap_gdk_screen_get_width_mm},
  {"get_height_mm",(nsp_method *) _wrap_gdk_screen_get_height_mm},
  {"make_display_name",(nsp_method *) _wrap_gdk_screen_make_display_name},
  {"get_n_monitors",(nsp_method *) _wrap_gdk_screen_get_n_monitors},
  {"get_monitor_geometry",(nsp_method *) _wrap_gdk_screen_get_monitor_geometry},
  {"get_monitor_at_point",(nsp_method *) _wrap_gdk_screen_get_monitor_at_point},
  {"get_monitor_at_window",(nsp_method *) _wrap_gdk_screen_get_monitor_at_window},
  {"broadcast_client_message",(nsp_method *) _wrap_gdk_screen_broadcast_client_message},
  { NULL, NULL}
};

static NspMethods *gdkscreen_get_methods(void) { return gdkscreen_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkscreen_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- GdkDisplay ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GdkDisplay_Private 
#include "nsp/gtk/gdkdisplay.h"
#include "nsp/interf.h"

/* NspGdkDisplay inherits from NspGObject */ 

int nsp_type_gdkdisplay_id=0;
NspTypeGdkDisplay *nsp_type_gdkdisplay=NULL;

NspTypeGdkDisplay *new_type_gdkdisplay(type_mode mode)
{
  NspTypeGdkDisplay *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdkdisplay != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdkdisplay;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdkdisplay_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gdkdisplay_get_methods; 
  type->new = (new_func *) new_gdkdisplay;

  /* specific methods for gdkdisplay */
      
  type->init = (init_func *) init_gdkdisplay;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkdisplay */ 

  top->s_type =  (s_type_func *) gdkdisplay_type_as_string;    
  top->sh_type = (sh_type_func *) gdkdisplay_type_short_string;
  /* top->create = (create_func*) int_gdkdisplay_create;*/ 
  
  /* specific methods for gdkdisplay */
      
  type->init = (init_func *) init_gdkdisplay;

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
 * initialize GdkDisplay instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkdisplay(NspGdkDisplay *o,NspTypeGdkDisplay *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GdkDisplay 
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
 * Object method redefined for GdkDisplay 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkdisplay_type_name[]="GdkDisplay";
static char gdkdisplay_short_type_name[]="GdkDisplay";

static char *gdkdisplay_type_as_string(void)
{
  return(gdkdisplay_type_name);
}

static char *gdkdisplay_type_short_string(void)
{
  return(gdkdisplay_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GdkDisplay objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGdkDisplay   *gdkdisplay_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast (O,nsp_type_gdkdisplay_id) ) return ((NspGdkDisplay *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdkdisplay));
  return NULL;
}

int IsGdkDisplayObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdkdisplay_id);
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
  if (( M = gdkdisplay_object(NthObj(i))) == NULLGDKDISPLAY)
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

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGdkDisplay *H;
  CheckRhs(0,0);
  / * want to be sure that type gdkdisplay is initialized * /
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gdkdisplay)) == NULLGDKDISPLAY) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_gdk_display_get_name(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;

  ret = gdk_display_get_name(GDK_DISPLAY(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_get_n_screens(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_display_get_n_screens(GDK_DISPLAY(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_get_screen(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int screen_num;
  NspObject *nsp_ret;
  GdkScreen *ret;

  if ( GetArgs(stack,rhs,opt,T,&screen_num) == FAIL) return RET_BUG;
  ret = gdk_display_get_screen(GDK_DISPLAY(self->obj), screen_num);
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_get_default_screen(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkScreen *ret;

  ret = gdk_display_get_default_screen(GDK_DISPLAY(self->obj));
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_pointer_ungrab(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  gulong time_;

  if ( GetArgs(stack,rhs,opt,T,&time_) == FAIL) return RET_BUG;
  gdk_display_pointer_ungrab(GDK_DISPLAY(self->obj), time_);
  return 0;
}

static int _wrap_gdk_display_keyboard_ungrab(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  gulong time_;

  if ( GetArgs(stack,rhs,opt,T,&time_) == FAIL) return RET_BUG;
  gdk_display_keyboard_ungrab(GDK_DISPLAY(self->obj), time_);
  return 0;
}

static int _wrap_gdk_display_pointer_is_grabbed(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gdk_display_pointer_is_grabbed(GDK_DISPLAY(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_display_beep(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_display_beep(GDK_DISPLAY(self->obj));
  return 0;
}

static int _wrap_gdk_display_sync(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_display_sync(GDK_DISPLAY(self->obj));
  return 0;
}

static int _wrap_gdk_display_close(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  gdk_display_close(GDK_DISPLAY(self->obj));
  return 0;
}

static int _wrap_gdk_display_get_event(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkEvent *ret;
  NspObject *nsp_ret;

  ret = gdk_display_get_event(GDK_DISPLAY(self->obj));
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

  ret = gdk_display_peek_event(GDK_DISPLAY(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_put_event(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GdkEvent *event = NULL;
  NspObject *nsp_event;

  if ( GetArgs(stack,rhs,opt,T,&nsp_event) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_event, GDK_TYPE_EVENT))
      event = nspg_boxed_get(nsp_event, GdkEvent);
  else {
      Scierror( "event should be a GdkEvent");
      return RET_BUG;
  }
  gdk_display_put_event(GDK_DISPLAY(self->obj), event);
  return 0;
}

static int _wrap_gdk_display_set_double_click_time(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int msec;

  if ( GetArgs(stack,rhs,opt,T,&msec) == FAIL) return RET_BUG;
  gdk_display_set_double_click_time(GDK_DISPLAY(self->obj), msec);
  return 0;
}

static int _wrap_gdk_display_get_core_pointer(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  GdkDevice *ret;

  ret = gdk_display_get_core_pointer(GDK_DISPLAY(self->obj));
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdevice))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_display_get_window_at_pointer(NspGdkDisplay *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int win_x, win_y;
  NspObject *nsp_ret;
  GdkWindow *ret;

  if ( GetArgs(stack,rhs,opt,T,&win_x, &win_y) == FAIL) return RET_BUG;
  ret = gdk_display_get_window_at_pointer(GDK_DISPLAY(self->obj), &win_x, &win_y);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdkdisplay_methods[] = {
  {"get_name",(nsp_method *) _wrap_gdk_display_get_name},
  {"get_n_screens",(nsp_method *) _wrap_gdk_display_get_n_screens},
  {"get_screen",(nsp_method *) _wrap_gdk_display_get_screen},
  {"get_default_screen",(nsp_method *) _wrap_gdk_display_get_default_screen},
  {"pointer_ungrab",(nsp_method *) _wrap_gdk_display_pointer_ungrab},
  {"keyboard_ungrab",(nsp_method *) _wrap_gdk_display_keyboard_ungrab},
  {"pointer_is_grabbed",(nsp_method *) _wrap_gdk_display_pointer_is_grabbed},
  {"beep",(nsp_method *) _wrap_gdk_display_beep},
  {"sync",(nsp_method *) _wrap_gdk_display_sync},
  {"close",(nsp_method *) _wrap_gdk_display_close},
  {"get_event",(nsp_method *) _wrap_gdk_display_get_event},
  {"peek_event",(nsp_method *) _wrap_gdk_display_peek_event},
  {"put_event",(nsp_method *) _wrap_gdk_display_put_event},
  {"set_double_click_time",(nsp_method *) _wrap_gdk_display_set_double_click_time},
  {"get_core_pointer",(nsp_method *) _wrap_gdk_display_get_core_pointer},
  {"get_window_at_pointer",(nsp_method *) _wrap_gdk_display_get_window_at_pointer},
  { NULL, NULL}
};

static NspMethods *gdkdisplay_get_methods(void) { return gdkdisplay_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkdisplay_attrs[]={{NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_gdk_pointer_grab(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,new_opts,t_end};
  nsp_option opts[] = {
	{"owner_events",s_bool,NULLOBJ,-1}, 
	{"event_mask",obj,NULLOBJ,-1}, 
	{"confine_to",obj,NULLOBJ,-1}, 
	{"cursor",obj,NULLOBJ,-1}, 
	{"time",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  NspObject *nsp_event_mask = NULL, *nsp_cursor = NULL;
  int owner_events = FALSE;
  NspGObject *window, *nsp_confine_to = NULL;
  gulong time = GDK_CURRENT_TIME;
  GdkEventMask event_mask = 0;
  GdkWindow *confine_to = NULL;
  GdkCursor *cursor = NULL;
  gint ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window, opts, &owner_events, &nsp_event_mask, &nsp_confine_to, &nsp_cursor, &time) == FAIL) return RET_BUG;
  if (nsp_event_mask && nspg_flags_get_value(GDK_TYPE_EVENT_MASK, nsp_event_mask, (gint *)&event_mask)==FAIL)
      return RET_BUG;
  if ( nsp_confine_to != NULL ) {
    if ( IsGdkWindow((NspObject *)nsp_confine_to))
      confine_to = GDK_WINDOW(nsp_confine_to->obj);
    else if (! IsNone((NspObject *)nsp_confine_to)) {
         Scierror( "confine_to should be a GdkWindow or None");
         return RET_BUG;
    }
  }
  if (nspg_boxed_check(nsp_cursor, GDK_TYPE_CURSOR))
      cursor = nspg_boxed_get(nsp_cursor, GdkCursor);
  else if (! IsNone(nsp_cursor)) {
      Scierror("cursor should be a GdkCursor or None");
      return RET_BUG;
  }
    ret = gdk_pointer_grab(GDK_WINDOW(window->obj), owner_events, event_mask, confine_to, cursor, time);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_pointer_ungrab(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {new_opts,t_end};
  nsp_option opts[] = {
	{"time",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  gulong time = GDK_CURRENT_TIME;

  if ( GetArgs(stack,rhs,opt,T,opts, &time) == FAIL) return RET_BUG;
    gdk_pointer_ungrab(time);
  return 0;
}

int _wrap_gdk_keyboard_grab(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,new_opts,t_end};
  nsp_option opts[] = {
	{"owner_events",s_bool,NULLOBJ,-1}, 
	{"time",s_int,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  int owner_events = FALSE;
  NspGObject *window;
  gulong time = GDK_CURRENT_TIME;
  gint ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window, opts, &owner_events, &time) == FAIL) return RET_BUG;
    ret = gdk_keyboard_grab(GDK_WINDOW(window->obj), owner_events, time);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_pointer_is_grabbed(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_pointer_is_grabbed();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_screen_width(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_screen_width();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_screen_height(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_screen_height();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_screen_width_mm(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_screen_width_mm();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_screen_height_mm(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_screen_height_mm();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_flush(Stack stack, int rhs, int opt, int lhs)
{
    gdk_flush();
  return 0;
}

int _wrap_gdk_beep(Stack stack, int rhs, int opt, int lhs)
{
    gdk_beep();
  return 0;
}

int _wrap_gdk_set_double_click_time(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int msec;

  if ( GetArgs(stack,rhs,opt,T,&msec) == FAIL) return RET_BUG;
    gdk_set_double_click_time(msec);
  return 0;
}

#line 121 "gdk.override"
static int 
_wrap_gdk_threads_enter(Stack stack,int rhs,int opt,int lhs)
{
  /* must allow threads while acquiring lock, or no other python
   * code will execute while we wait! */
  CheckRhs(0,0);
  /* Nsp_BEGIN_ALLOW_THREADS;
  gdk_threads_enter();
  Nsp_END_ALLOW_THREADS;
  */
  Scierror("To be done XXXXX ");
  return 0;
}
#line 9922 "gdk.c"


int _wrap_gdk_threads_leave(Stack stack, int rhs, int opt, int lhs)
{
    gdk_threads_leave();
  return 0;
}

#line 40 "gdk.override"

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
  Scierror("nspgtk threading disabled at compile time");
  return 0;
#endif
}
#line 10011 "gdk.c"


int _wrap_gdk_colormap_get_system(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  GdkColormap *ret;

    ret = gdk_colormap_get_system();
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_colormap_get_system_size(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_colormap_get_system_size();
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 447 "gdk.override"
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
#line 10052 "gdk.c"


int _wrap_gdk_draw_layout_with_colors(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, obj_check, s_int, s_int, obj_check, obj, obj,t_end};
  int x, y;
  NspGObject *drawable, *gc, *layout;
  NspObject *nsp_foreground, *nsp_background;
  GdkColor *foreground = NULL, *background = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdrawable, &drawable, &nsp_type_gdkgc, &gc, &x, &y, &nsp_type_pangolayout, &layout, &nsp_foreground, &nsp_background) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_foreground, GDK_TYPE_COLOR))
      foreground = nspg_boxed_get(nsp_foreground, GdkColor);
  else {
      Scierror( "foreground should be a GdkColor");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_background, GDK_TYPE_COLOR))
      background = nspg_boxed_get(nsp_background, GdkColor);
  else {
      Scierror( "background should be a GdkColor");
      return RET_BUG;
  }
    gdk_draw_layout_with_colors(GDK_DRAWABLE(drawable->obj), GDK_GC(gc->obj), x, y, PANGO_LAYOUT(layout->obj), foreground, background);
  return 0;
}

int _wrap_gdk_events_pending(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_events_pending();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_event_get(Stack stack, int rhs, int opt, int lhs)
{
  GdkEvent *ret;
  NspObject *nsp_ret;

    ret = gdk_event_get();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_event_peek(Stack stack, int rhs, int opt, int lhs)
{
  GdkEvent *ret;
  NspObject *nsp_ret;

    ret = gdk_event_peek();
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_event_get_graphics_expose(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,t_end};
  GdkEvent *ret;
  NspGObject *window;
  NspObject *nsp_ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkwindow, &window) == FAIL) return RET_BUG;
    ret = gdk_event_get_graphics_expose(GDK_WINDOW(window->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_EVENT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkevent))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_set_show_events(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_bool,t_end};
  int show_events;

  if ( GetArgs(stack,rhs,opt,T,&show_events) == FAIL) return RET_BUG;
    gdk_set_show_events(show_events);
  return 0;
}

int _wrap_gdk_get_show_events(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_get_show_events();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#line 465 "gdk.override"
static int
_wrap_gdk_fontset_load( Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "fontset_name", NULL };*/
  char *fontset_name;
  NspObject *ret; 
  int_types T[] = { string , t_end};
  if (GetArgs(stack,rhs,opt,T, &fontset_name) == FAIL ) return RET_BUG;
  /* nspg_boxed_new handles NULL checking */
  if ((ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_FONT,gdk_fontset_load(fontset_name),
					 FALSE, TRUE,(NspTypeBase *)nsp_type_gdkfont )) == NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}

#line 10166 "gdk.c"


int _wrap_gdk_font_from_description(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj,t_end};
  GdkFont *ret;
  PangoFontDescription *font_desc = NULL;
  NspObject *nsp_font_desc, *nsp_ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_font_desc) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_font_desc, PANGO_TYPE_FONT_DESCRIPTION))
      font_desc = nspg_boxed_get(nsp_font_desc, PangoFontDescription);
  else {
      Scierror( "font_desc should be a PangoFontDescription");
      return RET_BUG;
  }
    ret = gdk_font_from_description(font_desc);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,GDK_TYPE_FONT, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gdkfont))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_device_get_core_pointer(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  GdkDevice *ret;

    ret = gdk_device_get_core_pointer();
  nsp_type_gdkdevice = new_type_gdkdevice(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdevice))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_keymap_get_default(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  GdkKeymap *ret;

    ret = gdk_keymap_get_default();
  nsp_type_gdkkeymap = new_type_gdkkeymap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkkeymap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_keyval_name(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int keyval;
  const gchar *ret;

  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret = gdk_keyval_name(keyval);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_from_name(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *keyval_name;
  int ret;

  if ( GetArgs(stack,rhs,opt,T,&keyval_name) == FAIL) return RET_BUG;
    ret = gdk_keyval_from_name(keyval_name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_to_upper(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int keyval, ret;

  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret = gdk_keyval_to_upper(keyval);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_to_lower(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int keyval, ret;

  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret = gdk_keyval_to_lower(keyval);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_is_upper(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int keyval, ret;

  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret = gdk_keyval_is_upper(keyval);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_is_lower(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int keyval, ret;

  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret = gdk_keyval_is_lower(keyval);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_keyval_to_unicode(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int keyval;
  gulong ret;

  if ( GetArgs(stack,rhs,opt,T,&keyval) == FAIL) return RET_BUG;
    ret = gdk_keyval_to_unicode(keyval);
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_unicode_to_keyval(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int ret;
  gulong wc;

  if ( GetArgs(stack,rhs,opt,T,&wc) == FAIL) return RET_BUG;
    ret = gdk_unicode_to_keyval(wc);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_pixmap_create_from_data(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, string, s_int, s_int, s_int, obj, obj,t_end};
  NspObject *nsp_fg, *nsp_bg, *nsp_ret;
  GdkPixmap *ret;
  int width, height, depth;
  NspGObject *drawable;
  GdkColor *fg = NULL, *bg = NULL;
  guchar *data;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkdrawable, &drawable, &data, &width, &height, &depth, &nsp_fg, &nsp_bg) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_fg, GDK_TYPE_COLOR))
      fg = nspg_boxed_get(nsp_fg, GdkColor);
  else {
      Scierror( "fg should be a GdkColor");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_bg, GDK_TYPE_COLOR))
      bg = nspg_boxed_get(nsp_bg, GdkColor);
  else {
      Scierror( "bg should be a GdkColor");
      return RET_BUG;
  }
    ret = gdk_pixmap_create_from_data(GDK_DRAWABLE(drawable->obj), data, width, height, depth, fg, bg);
  nsp_type_gdkpixmap = new_type_gdkpixmap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixmap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 551 "gdk.override"
static int
_wrap_gdk_pixmap_create_from_xpm( Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "drawable" or "colormap" ,"transparent_color" or None , "filename" or data };*/
  GdkDrawable *drawable = NULL;
  GdkColormap *colormap = NULL;
  GdkColor *trans_color = NULL;
  NspObject *nsp_trans_color, *ret, *arg1;
  NspSMatrix *data;

  GdkPixmap *pixmap;
  GdkBitmap *mask ;

  int_types T[] = {obj, obj ,smat ,t_end};
  CheckLhs(1,2);
  if (GetArgs(stack,rhs,opt,T,  &arg1, &nsp_trans_color, &data) == FAIL ) return RET_BUG;

  if (IsGdkDrawable(arg1)) 
    {
      drawable = GDK_DRAWABLE(nspgobject_get(arg1));
    }
  else if ( IsGdkColormap(arg1) )
    {
      colormap =  GDK_COLORMAP(nspgobject_get(arg1));
    }
  else 
    {
      Scierror("%s: first argument must be a drawable or a colormap\n",stack.fname);
      return RET_BUG; 
    }

  if (nspg_boxed_check(nsp_trans_color, GDK_TYPE_COLOR))
    trans_color = nspg_boxed_get(nsp_trans_color, GdkColor);
  else if ( !IsNone(nsp_trans_color)) {
    Scierror("%s: transparent_color must be a colour or None\n",stack.fname);
    return RET_BUG;
  }
  
  if (data->mn == 0) 
    {
      Scierror("%s: third argument must be of size > 0 \n",stack.fname);
      return RET_BUG;
    }
  if ( drawable != NULL ) 
    {
      if ( lhs == 1 ) 
	{
	  if ( data->mn == 1) 
	    pixmap = gdk_pixmap_create_from_xpm(drawable,NULL, trans_color, data->S[0]);
	  else
	    pixmap = gdk_pixmap_create_from_xpm_d(drawable,NULL,trans_color,data->S);
	}
      else 
	{
	  if ( data->mn == 1) 
	    pixmap = gdk_pixmap_create_from_xpm(drawable, &mask, trans_color, data->S[0]);
	  else 
	    pixmap = gdk_pixmap_create_from_xpm_d(drawable, &mask,trans_color,data->S);
	}
    }
  else 
    {
      if ( lhs == 1 ) 
	{
	  if ( data->mn == 1) 
	    pixmap = gdk_pixmap_colormap_create_from_xpm(drawable, colormap,NULL, trans_color,data->S[0]);
	  else 
	    pixmap = gdk_pixmap_colormap_create_from_xpm_d(drawable, colormap,NULL, trans_color, data->S);

	}
      else 
	{
	  if ( data->mn == 1) 
	    pixmap = gdk_pixmap_colormap_create_from_xpm(drawable, colormap, &mask, trans_color,data->S[0]);
	  else 
	    pixmap = gdk_pixmap_colormap_create_from_xpm_d(drawable, colormap, &mask, trans_color, data->S);
	}
    }
  if (pixmap == NULL) {
    Scierror("%s: can't create pixmap\n");
    return RET_BUG;
  }
  if ((ret = (NspObject *) nspgobject_new((GObject *)pixmap))== NULL) return RET_BUG; 
  MoveObj(stack,1,ret);
  if ( lhs == 2 ) 
    {
      if ((ret = (NspObject *) nspgobject_new((GObject *)mask))== NULL) return RET_BUG; 
      MoveObj(stack,2,ret);
      return 2;
    }
  return 1; 
  /*
    XXX à clarifier ce serait bien que le display d'un object gtk ou gdk affiche son nombre de ref 
    gdk_pixmap_unref(pixmap);
    gdk_bitmap_unref(mask);
  */
}
#line 10435 "gdk.c"


int _wrap_gdk_bitmap_create_from_data(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, string, s_int, s_int,t_end};
  GdkBitmap *ret;
  NspObject *nsp_ret;
  int width, height;
  NspGObject *nsp_drawable;
  GdkDrawable *drawable = NULL;
  guchar *data;

  if ( GetArgs(stack,rhs,opt,T,&nsp_drawable, &data, &width, &height) == FAIL) return RET_BUG;
  if ( IsGdkDrawable((NspObject *)nsp_drawable))
      drawable = GDK_DRAWABLE(nsp_drawable->obj);
  else if ( ! IsNone((NspObject *) nsp_drawable))  {
      Scierror( "drawable should be a GdkDrawable or None");
      return RET_BUG;
  }
    ret = gdk_bitmap_create_from_data(drawable, data, width, height);
  nsp_type_gdkbitmap = new_type_gdkbitmap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkbitmap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_pixmap_foreign_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  NspObject *nsp_ret;
  gulong anid;
  GdkPixmap *ret;

  if ( GetArgs(stack,rhs,opt,T,&anid) == FAIL) return RET_BUG;
    ret = gdk_pixmap_foreign_new(anid);
  nsp_type_gdkpixmap = new_type_gdkpixmap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixmap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_pixmap_lookup(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  NspObject *nsp_ret;
  gulong anid;
  GdkPixmap *ret;

  if ( GetArgs(stack,rhs,opt,T,&anid) == FAIL) return RET_BUG;
    ret = gdk_pixmap_lookup(anid);
  nsp_type_gdkpixmap = new_type_gdkpixmap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixmap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_atom_intern(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,new_opts,t_end};
  nsp_option opts[] = {
	{"only_if_exists",s_bool,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  char *atom_name;
  int only_if_exists = FALSE;
  NspObject *nsp_ret;
  GdkAtom ret;

  if ( GetArgs(stack,rhs,opt,T,&atom_name, opts, &only_if_exists) == FAIL) return RET_BUG;
    ret = gdk_atom_intern(atom_name, only_if_exists);
  if (( nsp_ret = (NspObject *) gdkatom_create(NVOID,NULL,ret,NULL))== NULL);
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_rgb_xpixel_from_rgb(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  gulong rgb, ret;

  if ( GetArgs(stack,rhs,opt,T,&rgb) == FAIL) return RET_BUG;
    ret = gdk_rgb_xpixel_from_rgb(rgb);
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_rgb_gc_set_foreground(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, s_int,t_end};
  NspGObject *gc;
  gulong rgb;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &rgb) == FAIL) return RET_BUG;
    gdk_rgb_gc_set_foreground(GDK_GC(gc->obj), rgb);
  return 0;
}

int _wrap_gdk_rgb_gc_set_background(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, s_int,t_end};
  NspGObject *gc;
  gulong rgb;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkgc, &gc, &rgb) == FAIL) return RET_BUG;
    gdk_rgb_gc_set_background(GDK_GC(gc->obj), rgb);
  return 0;
}

int _wrap_gdk_rgb_ditherable(Stack stack, int rhs, int opt, int lhs)
{
  int ret;

    ret = gdk_rgb_ditherable();
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_gdk_rgb_set_verbose(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_bool,t_end};
  int verbose;

  if ( GetArgs(stack,rhs,opt,T,&verbose) == FAIL) return RET_BUG;
    gdk_rgb_set_verbose(verbose);
  return 0;
}

int _wrap_gdk_rgb_set_install(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_bool,t_end};
  int install;

  if ( GetArgs(stack,rhs,opt,T,&install) == FAIL) return RET_BUG;
    gdk_rgb_set_install(install);
  return 0;
}

int _wrap_gdk_rgb_set_min_colors(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int min_colors;

  if ( GetArgs(stack,rhs,opt,T,&min_colors) == FAIL) return RET_BUG;
    gdk_rgb_set_min_colors(min_colors);
  return 0;
}

int _wrap_gdk_rgb_get_cmap(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  GdkColormap *ret;

    ret = gdk_rgb_get_cmap();
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_rgb_get_visual(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  GdkVisual *ret;

    ret = gdk_rgb_get_visual();
  nsp_type_gdkvisual = new_type_gdkvisual(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkvisual))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_selection_owner_get(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_selection = NULL, *nsp_ret;
  GdkAtom selection;
  GdkWindow *ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_selection) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
    ret = gdk_selection_owner_get(selection);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_selection_send_notify(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, obj, obj, obj, s_int,t_end};
  GdkAtom selection, target, property;
  NspObject *nsp_selection = NULL, *nsp_target = NULL, *nsp_property = NULL;
  gulong requestor, time;

  if ( GetArgs(stack,rhs,opt,T,&requestor, &nsp_selection, &nsp_target, &nsp_property, &time) == FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_selection,&selection)==FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_target,&target)==FAIL) return RET_BUG;
  if ( nsp_gdk_atom_from_object(nsp_property,&property)==FAIL) return RET_BUG;
    gdk_selection_send_notify(requestor, selection, target, property, time);
  return 0;
}

int _wrap_gdk_window_at_pointer(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, s_int,t_end};
  int win_x, win_y;
  NspObject *nsp_ret;
  GdkWindow *ret;

  if ( GetArgs(stack,rhs,opt,T,&win_x, &win_y) == FAIL) return RET_BUG;
    ret = gdk_window_at_pointer(&win_x, &win_y);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_window_foreign_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  NspObject *nsp_ret;
  gulong anid;
  GdkWindow *ret;

  if ( GetArgs(stack,rhs,opt,T,&anid) == FAIL) return RET_BUG;
    ret = gdk_window_foreign_new(anid);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_window_lookup(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  NspObject *nsp_ret;
  gulong anid;
  GdkWindow *ret;

  if ( GetArgs(stack,rhs,opt,T,&anid) == FAIL) return RET_BUG;
    ret = gdk_window_lookup(anid);
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_set_sm_client_id(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *sm_client_id;

  if ( GetArgs(stack,rhs,opt,T,&sm_client_id) == FAIL) return RET_BUG;
    gdk_set_sm_client_id(sm_client_id);
  return 0;
}

int _wrap_gdk_window_process_all_updates(Stack stack, int rhs, int opt, int lhs)
{
    gdk_window_process_all_updates();
  return 0;
}

int _wrap_gdk_window_set_debug_updates(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_bool,t_end};
  int setting;

  if ( GetArgs(stack,rhs,opt,T,&setting) == FAIL) return RET_BUG;
    gdk_window_set_debug_updates(setting);
  return 0;
}

int _wrap_gdk_get_default_root_window(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  GdkWindow *ret;

    ret = gdk_get_default_root_window();
  nsp_type_gdkwindow = new_type_gdkwindow(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkwindow))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_pixbuf_new_from_file(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *filename;
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  GError *error = NULL;

  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
    ret = gdk_pixbuf_new_from_file(filename, &error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",stack.fname);
    return RET_BUG;
  }
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  g_object_unref(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 2314 "gdk.override"
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
      Scierror("%s:can't load pixbuf",stack.fname);
      return RET_BUG;
    }
  
  if(( ret = (NspObject *) gobject_create(NVOID,(GObject *)pixbuf, (NspTypeBase *) nsp_type_gdkpixbuf)) == NULL) return RET_BUG;
  /* g_object_unref(pixbuf); XXXX */
  MoveObj(stack,1,ret);
  return 1;
}

#line 10764 "gdk.c"


int _wrap_gdk_pixbuf_new_from_inline(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, string, s_bool,t_end};
  int data_length, copy_pixels;
  guchar *data;
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  GError *error = NULL;

  if ( GetArgs(stack,rhs,opt,T,&data_length, &data, &copy_pixels) == FAIL) return RET_BUG;
    ret = gdk_pixbuf_new_from_inline(data_length, data, copy_pixels, &error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",stack.fname);
    return RET_BUG;
  }
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  g_object_unref(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_pixbuf_loader_new(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  GdkPixbufLoader *ret;

    ret = gdk_pixbuf_loader_new();
  nsp_type_gdkpixbufloader = new_type_gdkpixbufloader(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbufloader))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_screen_get_default(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  GdkScreen *ret;

    ret = gdk_screen_get_default();
  nsp_type_gdkscreen = new_type_gdkscreen(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkscreen))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 2290 "gdk.override"

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
      if (( nsp_ret = (NspObject *) none_create(NVOID,NULL))== NULL) return RET_BUG; 
    }
  else 
    {
      nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
      if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
    }
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 10836 "gdk.c"


int _wrap_gdk_display_get_default(Stack stack, int rhs, int opt, int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;

    ret = gdk_display_get_default();
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gdk_display_open_default_libgtk_only(Stack stack, int rhs, int opt, int lhs)
{
  GdkDisplay *ret;
  NspObject *nsp_ret;

    ret = gdk_display_open_default_libgtk_only();
  nsp_type_gdkdisplay = new_type_gdkdisplay(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkdisplay))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gdk_func[]={
  {"gdkfont_new", _wrap_gdkfont_new},
  {"gdkcolor_new", _wrap_gdkcolor_new},
  {"gdkcursor_new", _wrap_gdkcursor_new},
  {"gdkrectangle_new", _wrap_gdkrectangle_new},
  {"gdkcolormap_new", _wrap_gdkcolormap_new},
  {"gdkdragcontext_new", _wrap_gdkdragcontext_new},
  {"gdkpixmap_new", _wrap_gdkpixmap_new},
  {"gdkgc_new", _wrap_gdkgc_new},
  {"gdkimage_new", _wrap_gdkimage_new},
  {"gdkpixbufanimation_new", _wrap_gdkpixbufanimation_new},
  {"gdkpixbufloader_new", _wrap_gdkpixbufloader_new},
  {"gdkvisual_new", _wrap_gdkvisual_new},
  {"gdk_pointer_grab", _wrap_gdk_pointer_grab},
  {"gdk_pointer_ungrab", _wrap_gdk_pointer_ungrab},
  {"gdk_keyboard_grab", _wrap_gdk_keyboard_grab},
  {"gdk_pointer_is_grabbed", _wrap_gdk_pointer_is_grabbed},
  {"gdk_screen_width", _wrap_gdk_screen_width},
  {"gdk_screen_height", _wrap_gdk_screen_height},
  {"gdk_screen_width_mm", _wrap_gdk_screen_width_mm},
  {"gdk_screen_height_mm", _wrap_gdk_screen_height_mm},
  {"gdk_flush", _wrap_gdk_flush},
  {"gdk_beep", _wrap_gdk_beep},
  {"gdk_set_double_click_time", _wrap_gdk_set_double_click_time},
  {"gdk_threads_enter", _wrap_gdk_threads_enter},
  {"gdk_threads_leave", _wrap_gdk_threads_leave},
  {"gdk_threads_init", _wrap_gdk_threads_init},
  {"gdk_colormap_get_system", _wrap_gdk_colormap_get_system},
  {"gdk_colormap_get_system_size", _wrap_gdk_colormap_get_system_size},
  {"gdk_color_parse", _wrap_gdk_color_parse},
  {"gdk_draw_layout_with_colors", _wrap_gdk_draw_layout_with_colors},
  {"gdk_events_pending", _wrap_gdk_events_pending},
  {"gdk_event_get", _wrap_gdk_event_get},
  {"gdk_event_peek", _wrap_gdk_event_peek},
  {"gdk_event_get_graphics_expose", _wrap_gdk_event_get_graphics_expose},
  {"gdk_set_show_events", _wrap_gdk_set_show_events},
  {"gdk_get_show_events", _wrap_gdk_get_show_events},
  {"gdk_fontset_load", _wrap_gdk_fontset_load},
  {"gdk_font_from_description", _wrap_gdk_font_from_description},
  {"gdk_device_get_core_pointer", _wrap_gdk_device_get_core_pointer},
  {"gdk_keymap_get_default", _wrap_gdk_keymap_get_default},
  {"gdk_keyval_name", _wrap_gdk_keyval_name},
  {"gdk_keyval_from_name", _wrap_gdk_keyval_from_name},
  {"gdk_keyval_to_upper", _wrap_gdk_keyval_to_upper},
  {"gdk_keyval_to_lower", _wrap_gdk_keyval_to_lower},
  {"gdk_keyval_is_upper", _wrap_gdk_keyval_is_upper},
  {"gdk_keyval_is_lower", _wrap_gdk_keyval_is_lower},
  {"gdk_keyval_to_unicode", _wrap_gdk_keyval_to_unicode},
  {"gdk_unicode_to_keyval", _wrap_gdk_unicode_to_keyval},
  {"gdk_pixmap_create_from_data", _wrap_gdk_pixmap_create_from_data},
  {"gdk_pixmap_create_from_xpm", _wrap_gdk_pixmap_create_from_xpm},
  {"gdk_bitmap_create_from_data", _wrap_gdk_bitmap_create_from_data},
  {"gdk_pixmap_foreign_new", _wrap_gdk_pixmap_foreign_new},
  {"gdk_pixmap_lookup", _wrap_gdk_pixmap_lookup},
  {"gdk_atom_intern", _wrap_gdk_atom_intern},
  {"gdk_rgb_xpixel_from_rgb", _wrap_gdk_rgb_xpixel_from_rgb},
  {"gdk_rgb_gc_set_foreground", _wrap_gdk_rgb_gc_set_foreground},
  {"gdk_rgb_gc_set_background", _wrap_gdk_rgb_gc_set_background},
  {"gdk_rgb_ditherable", _wrap_gdk_rgb_ditherable},
  {"gdk_rgb_set_verbose", _wrap_gdk_rgb_set_verbose},
  {"gdk_rgb_set_install", _wrap_gdk_rgb_set_install},
  {"gdk_rgb_set_min_colors", _wrap_gdk_rgb_set_min_colors},
  {"gdk_rgb_get_cmap", _wrap_gdk_rgb_get_cmap},
  {"gdk_rgb_get_visual", _wrap_gdk_rgb_get_visual},
  {"gdk_selection_owner_get", _wrap_gdk_selection_owner_get},
  {"gdk_selection_send_notify", _wrap_gdk_selection_send_notify},
  {"gdk_window_at_pointer", _wrap_gdk_window_at_pointer},
  {"gdk_window_foreign_new", _wrap_gdk_window_foreign_new},
  {"gdk_window_lookup", _wrap_gdk_window_lookup},
  {"gdk_set_sm_client_id", _wrap_gdk_set_sm_client_id},
  {"gdk_window_process_all_updates", _wrap_gdk_window_process_all_updates},
  {"gdk_window_set_debug_updates", _wrap_gdk_window_set_debug_updates},
  {"gdk_get_default_root_window", _wrap_gdk_get_default_root_window},
  {"gdk_pixbuf_new_from_file", _wrap_gdk_pixbuf_new_from_file},
  {"gdk_pixbuf_new_from_xpm_data", _wrap_gdk_pixbuf_new_from_xpm_data},
  {"gdk_pixbuf_new_from_inline", _wrap_gdk_pixbuf_new_from_inline},
  {"gdk_pixbuf_loader_new", _wrap_gdk_pixbuf_loader_new},
  {"gdk_screen_get_default", _wrap_gdk_screen_get_default},
  {"gdk_display_open", _wrap_gdk_display_open},
  {"gdk_display_get_default", _wrap_gdk_display_get_default},
  {"gdk_display_open_default_libgtk_only", _wrap_gdk_display_open_default_libgtk_only},
  { NULL, NULL}
};

/** call ith function in the gdk interface **/

int gdk_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(gdk_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void gdk_Interf_Info(int i, char **fname, function (**f))
{
  *fname = gdk_func[i].name;
  *f = gdk_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
gdk_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_COLORSPACE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_CURSOR_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_DRAG_ACTION, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_DRAG_PROTOCOL, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_FILTER_RETURN, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_EVENT_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_EVENT_MASK, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_VISIBILITY_STATE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_SCROLL_DIRECTION, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_NOTIFY_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_CROSSING_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_PROPERTY_STATE, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_WINDOW_STATE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_SETTING_ACTION, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_FONT_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_CAP_STYLE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_FILL, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_FUNCTION, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_JOIN_STYLE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_LINE_STYLE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_SUBWINDOW_MODE, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_GC_VALUES_MASK, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_IMAGE_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_EXTENSION_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_INPUT_SOURCE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_INPUT_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_AXIS_USE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_PROP_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_FILL_RULE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_OVERLAP_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_RGB_DITHER, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_BYTE_ORDER, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_MODIFIER_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_INPUT_CONDITION, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_STATUS, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_GRAB_STATUS, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_VISUAL_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_WINDOW_CLASS, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_WINDOW_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_WINDOW_ATTRIBUTES_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_WINDOW_HINTS, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_WINDOW_TYPE_HINT, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_WM_DECORATION, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, GDK_TYPE_WM_FUNCTION, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_GRAVITY, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_WINDOW_EDGE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_PIXBUF_ALPHA_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_PIXBUF_ERROR, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, GDK_TYPE_INTERP_TYPE, strip_prefix);
}

/* intialise stuff extension classes */
/* void
gdk_register_classes(NspObject *d)
{
  NspObject *module;

  if ((module = PyImport_ImportModule("gdk")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _GdkAtom = (PyTypeObject *)PyDict_GetItemString(moddict, "Atom");
  } else {
      Py_FatalError("could not import gdk");
      return;
  }
  if ((module = PyImport_ImportModule("gobject")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _GObject = (PyTypeObject *)PyDict_GetItemString(moddict, "GObject");
  } else {
      Py_FatalError("could not import gobject");
      return;
  }
  if ((module = PyImport_ImportModule("pango")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _PangoContext = (PyTypeObject *)PyDict_GetItemString(moddict, "Context");
      _PangoFont = (PyTypeObject *)PyDict_GetItemString(moddict, "Font");
      _PangoLayout = (PyTypeObject *)PyDict_GetItemString(moddict, "Layout");
  } else {
      Py_FatalError("could not import pango");
      return;
  }
  if ((module = PyImport_ImportModule("gtk")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _GtkWidget = (PyTypeObject *)PyDict_GetItemString(moddict, "Widget");
  } else {
      Py_FatalError("could not import gtk");
      return;
  }


#line 24 "gdk.override"
/ *
  #ifdef HAVE_NUMPY
  import_array();
  #endif
* /

#line 11073 "gdk.c"
  nspg_register_boxed(d, "Event", GDK_TYPE_EVENT, &PyGdkEvent_Type);
  nspg_register_boxed(d, "Font", GDK_TYPE_FONT, &PyGdkFont_Type);
  nspg_register_boxed(d, "Color", GDK_TYPE_COLOR, &PyGdkColor_Type);
  nspg_register_boxed(d, "Cursor", GDK_TYPE_CURSOR, &PyGdkCursor_Type);
  nspg_register_boxed(d, "Rectangle", GDK_TYPE_RECTANGLE, &PyGdkRectangle_Type);
  nspgobject_register_class(d, "GdkColormap", GDK_TYPE_COLORMAP, &PyGdkColormap_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkDevice", GDK_TYPE_DEVICE, &PyGdkDevice_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkDragContext", GDK_TYPE_DRAG_CONTEXT, &PyGdkDragContext_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkDrawable", GDK_TYPE_DRAWABLE, &PyGdkDrawable_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkWindow", GDK_TYPE_WINDOW, &PyGdkWindow_Type, Py_BuildValue("(O)", &PyGdkDrawable_Type));
  nspgobject_register_class(d, "GdkPixmap", GDK_TYPE_PIXMAP, &PyGdkPixmap_Type, Py_BuildValue("(O)", &PyGdkDrawable_Type));
  nspgobject_register_class(d, "GdkBitmap", GDK_TYPE_DRAWABLE, &PyGdkBitmap_Type, Py_BuildValue("(O)", &PyGdkDrawable_Type));
  nspgobject_register_class(d, "GdkGC", GDK_TYPE_GC, &PyGdkGC_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkImage", GDK_TYPE_IMAGE, &PyGdkImage_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkKeymap", GDK_TYPE_KEYMAP, &PyGdkKeymap_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkPixbuf", GDK_TYPE_PIXBUF, &PyGdkPixbuf_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkPixbufAnimation", GDK_TYPE_PIXBUF_ANIMATION, &PyGdkPixbufAnimation_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkPixbufAnimationIter", GDK_TYPE_PIXBUF_ANIMATION_ITER, &PyGdkPixbufAnimationIter_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkPixbufLoader", GDK_TYPE_PIXBUF_LOADER, &PyGdkPixbufLoader_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkVisual", GDK_TYPE_VISUAL, &PyGdkVisual_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkScreen", GDK_TYPE_SCREEN, &PyGdkScreen_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "GdkDisplay", GDK_TYPE_DISPLAY, &PyGdkDisplay_Type, Py_BuildValue("(O)", &PyGObject_Type));
}
*/
