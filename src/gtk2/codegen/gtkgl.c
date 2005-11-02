/* -*- Mode: C -*- */


#line 4 "gtkgl.override"
#define NO_IMPORT_PYGOBJECT
#include "pygobject.h"
#include "pygtk.h"

#include <gtkgl/gtkglarea.h>
#include <gtk/gtk.h>

#line 13 "gtkgl.c"


/* ---------- types from other modules ---------- */
#include "nsp/gtk/nspgobject_type.h"
#include "nsp/gtk/nspgdkvisual_type.h"
#include "nsp/gtk/nspgtkdrawingarea_type.h"


/* ---------- forward type declarations ---------- */
#define GtkGLArea_Private
#include "nsp/gtk/gtkglarea.h"


/* ----------- GtkGLArea ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GtkGLArea_Private 
#include "nsp/gtk/gtkglarea.h"
#include "nsp/interf.h"

/* NspGtkGLArea inherits from NspGtkDrawingArea */ 

int nsp_type_gtkglarea_id=0;
NspTypeGtkGLArea *nsp_type_gtkglarea=NULL;

NspTypeGtkGLArea *new_type_gtkglarea(type_mode mode)
{
  NspTypeGtkGLArea *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtkglarea != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtkglarea;
    }
  if ((type =  malloc(sizeof(NspTypeGtkDrawingArea))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtkdrawingarea(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtkglarea_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gtkglarea_get_methods; 
  type->new = (new_func *) new_gtkglarea;

  /* specific methods for gtkglarea */
      
  type->init = (init_func *) init_gtkglarea;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gtkglarea */ 

  top->s_type =  (s_type_func *) gtkglarea_type_as_string;    
  top->sh_type = (sh_type_func *) gtkglarea_type_short_string;
  /* top->create = (create_func*) int_gtkglarea_create;*/ 
  
  /* specific methods for gtkglarea */
      
  type->init = (init_func *) init_gtkglarea;

  if ( nsp_type_gtkglarea_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkGLArea called nsp_type_gtkglarea
       */
      type->id =  nsp_type_gtkglarea_id = nsp_new_type_id();
      nsp_type_gtkglarea = type;
      if ( nsp_register_type(nsp_type_gtkglarea) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtkglarea, GTK_TYPE_GL_AREA);
      return ( mode == T_BASE ) ? type : new_type_gtkglarea(mode);
    }
  else 
    {
       type->id = nsp_type_gtkglarea_id;
       return type;
    }
}

/*
 * initialize GtkGLArea instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtkglarea(NspGtkGLArea *o,NspTypeGtkGLArea *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GtkGLArea 
 */

NspGtkGLArea *new_gtkglarea() 
{
  NspGtkGLArea *loc; 
  /* type must exists */
  nsp_type_gtkglarea = new_type_gtkglarea(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkGLArea)))== NULLGTKGLAREA) return loc;
  /* initialize object */
  if ( init_gtkglarea(loc,nsp_type_gtkglarea) == FAIL) return NULLGTKGLAREA;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GtkGLArea 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gtkglarea_type_name[]="GtkGLArea";
static char gtkglarea_short_type_name[]="GtkGLArea";

static char *gtkglarea_type_as_string(void)
{
  return(gtkglarea_type_name);
}

static char *gtkglarea_type_short_string(void)
{
  return(gtkglarea_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GtkGLArea objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGtkGLArea   *gtkglarea_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_gtkglarea_id) ) return ((NspGtkGLArea *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtkglarea));
  return NULL;
}

int IsGtkGLAreaObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gtkglarea_id);
}

int IsGtkGLArea(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtkglarea_id);
}

NspGtkGLArea  *GetGtkGLAreaCopy(Stack stack, int i)
{
  if (  GetGtkGLArea(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkGLArea  *GetGtkGLArea(Stack stack, int i)
{
  NspGtkGLArea *M;
  if (( M = gtkglarea_object(NthObj(i))) == NULLGTKGLAREA)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkGLArea *gtkglarea_copy(NspGtkGLArea *self)
{
  /* return gtkdrawingarea_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtkglarea);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtkglarea);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkGLArea
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGtkGLArea *H;
  CheckRhs(0,0);
  / * want to be sure that type gtkglarea is initialized * /
  nsp_type_gtkglarea = new_type_gtkglarea(T_BASE);
  if(( H = gtkdrawingarea_create(NVOID,(NspTypeBase *) nsp_type_gtkglarea)) == NULLGTKGLAREA) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 40 "gtkgl.override"
static int
_wrap_gtk_gl_area_share_new(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
    char *kwlist[] = { "attr_list", "share", NULL };
    gint *attr_list;
    GtkGLArea *share = NULL;
    NspObject *py_attr_list, *py_share = Nsp_None;
    gint i, len;

    if (GetArgs(stack,rhs,opt,T, "O|O:GtkGLArea.__init__",
				     kwlist, &py_attr_list, &py_share))
	return -1;
    if (!NspSequence_Check(py_attr_list)) {
	Scierror( "attr_list must be a sequence");
	return -1;
    }
    len = NspSequence_Length(py_attr_list);
    attr_list = g_new(gint, len+1);
    for (i = 0; i < len; i++) {
	NspObject *item = NspSequence_GetItem(py_attr_list, i);

	Nsp_DECREF(item);
	if (!NspInt_Check(item)) {
	    g_free(attr_list);
	    Scierror( "attr_list items must be ints");
	    return -1;
	}
	attr_list[i] = NspInt_AsLong(item);
    }
    attr_list[len] = GDK_GL_NONE; /* sentinel */

    if (!pygobject_check(py_share, &nsp_type_gtkglarea)) {
	if ( pygobject_get(py_share) != NULL )
	    share = GTK_GL_AREA(pygobject_get(py_share));
    } else if ( ! IsNone(py_share )) {
	Scierror( "share must be a GtkGLArea or None");
	return -1;
    }

    pygobject_get(self) = (GObject *)gtk_gl_area_share_new(attr_list, share);
    if (!self->obj) {
	NspErr_SetString(NspExc_RuntimeError,
			"could not create GtkGLArea object");
	return -1;
    }
    pygobject_register_wrapper((NspObject *)self);
    //Nsp_INCREF(Nsp_None);
    //return Nsp_None;
    return 0;
}
#line 271 "gtkgl.c"


static int _wrap_gtk_gl_area_make_current(NspGtkGLArea *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = gtk_gl_area_make_current(GTK_GL_AREA(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_gl_area_swap_buffers(NspGtkGLArea *self,Stack stack,int rhs,int opt,int lhs)
{
  gtk_gl_area_swap_buffers(GTK_GL_AREA(self->obj));
  return 0;
}

static NspMethods gtkglarea_methods[] = {
  {"make_current",(nsp_method *) _wrap_gtk_gl_area_make_current},
  {"swap_buffers",(nsp_method *) _wrap_gtk_gl_area_swap_buffers},
  { NULL, NULL}
};

static NspMethods *gtkglarea_get_methods(void) { return gtkglarea_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtkglarea_attrs[]={{NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_gdk_gl_get_info(Stack stack, int rhs, int opt, int lhs)
{
  gchar *ret;

    ret = gdk_gl_get_info();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_gdk_gl_get_config(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, s_int,t_end};
  int attrib, ret;
  NspGObject *visual;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkvisual, &visual, &attrib) == FAIL) return RET_BUG;
    ret = gdk_gl_get_config(GDK_VISUAL(visual->obj), attrib);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 92 "gtkgl.override"
static NspObject *
_wrap_gdk_gl_wait_gdk(NspGObject *self)
{
    pyg_unblock_threads();
    gdk_gl_wait_gdk();
    pyg_block_threads();
    Nsp_INCREF(Nsp_None);
    return Nsp_None;
}
#line 337 "gtkgl.c"


#line 103 "gtkgl.override"
static NspObject *
_wrap_gdk_gl_wait_gl(NspGObject *self)
{
    pyg_unblock_threads();
    gdk_gl_wait_gl();
    pyg_block_threads();
    Nsp_INCREF(Nsp_None);
    return Nsp_None;
}

/* you should add appropriate ignore, ignore-glob and
 * override sections here */
#line 353 "gtkgl.c"


int _wrap_gdk_gl_use_gdk_font(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, s_int, s_int, s_int,t_end};
  GdkFont *font = NULL;
  int first, count, list_base;
  NspObject *nsp_font;

  if ( GetArgs(stack,rhs,opt,T,&nsp_font, &first, &count, &list_base) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_font, GDK_TYPE_FONT))
      font = nspg_boxed_get(nsp_font, GdkFont);
  else {
      Scierror( "font should be a GdkFont");
      return RET_BUG;
  }
    gdk_gl_use_gdk_font(font, first, count, list_base);
  return 0;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gtkgl_func[]={
  {"gtkglarea_new", _wrap_gtkglarea_new},
  {"gdk_gl_get_info", _wrap_gdk_gl_get_info},
  {"gdk_gl_get_config", _wrap_gdk_gl_get_config},
  {"gdk_gl_wait_gdk", _wrap_gdk_gl_wait_gdk},
  {"gdk_gl_wait_gl", _wrap_gdk_gl_wait_gl},
  {"gdk_gl_use_gdk_font", _wrap_gdk_gl_use_gdk_font},
  { NULL, NULL}
};

/** call ith function in the gtkgl interface **/

int gtkgl_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(gtkgl_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void gtkgl_Interf_Info(int i, char **fname, function (**f))
{
  *fname = gtkgl_func[i].name;
  *f = gtkgl_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
gtkgl_register_classes(NspObject *d)
{
  NspObject *module;

  if ((module = PyImport_ImportModule("gobject")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _NspGObject_Type = (PyTypeObject *)PyDict_GetItemString(moddict, "GObject");
  } else {
      Py_FatalError("could not import gobject");
      return;
  }
  if ((module = PyImport_ImportModule("gtk")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _NspGtkDrawingArea_Type = (PyTypeObject *)PyDict_GetItemString(moddict, "DrawingArea");
  } else {
      Py_FatalError("could not import gtk");
      return;
  }
  if ((module = PyImport_ImportModule("gtk.gdk")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _NspGdkVisual_Type = (PyTypeObject *)PyDict_GetItemString(moddict, "Visual");
  } else {
      Py_FatalError("could not import gtk.gdk");
      return;
  }


#line 436 "gtkgl.c"
  nspgobject_register_class(d, "GtkGLArea", GTK_TYPE_GL_AREA, &PyGtkGLArea_Type, Py_BuildValue("(O)", &PyGtkDrawingArea_Type));
}
*/
