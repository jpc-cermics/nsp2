/* -*- Mode: C -*- */


#line 4 "dialog.override"
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
/* XXXX */
#define GtkObject_Private
#define GtkCellRenderer_Private
#define GtkTreeModel_Private 

enum {STYLE_COLOUR_ARRAY, STYLE_GC_ARRAY, STYLE_PIXMAP_ARRAY} type;

/* XXXXX a revoir */

#define PYGTK_TYPE_GENERIC_TREE_MODEL G_TYPE_INVALID
#define PYGTK_TYPE_GENERIC_CELL_RENDERER G_TYPE_INVALID
#define  GtkCellEditable_Private 
#include "nsp/gtk/gtkcelleditable.h"

#line 24 "dialog.c"


/* ---------- types from other modules ---------- */
#include "nsp/gtk/gdkatom.h"
#include "nsp/gtk/gdkpixmap.h"
#include "nsp/gtk/gdkwindow.h"
#include "nsp/gtk/gdkgc.h"
#include "nsp/gtk/gdkfont.h"
#include "nsp/gtk/gdkpixbuf.h"
#include "nsp/gtk/gdkpixbufanimation.h"
#include "nsp/gtk/gdkdragcontext.h"
#include "nsp/gtk/gdkcolormap.h"
#include "nsp/gtk/gdkcolor.h"
#include "nsp/gtk/gdkimage.h"
#include "nsp/gtk/gdkvisual.h"
#include "nsp/gtk/gtktreemodel.h"
#include "nsp/gtk/pangolayout.h"
#include "nsp/gtk/pangolanguage.h"
#include "nsp/gtk/pangofontdescription.h"
#include "nsp/gtk/pangoattrlist.h"
#include "nsp/gtk/pangotabarray.h"
#include "nsp/gtk/pangocontext.h"


/* ---------- forward type declarations ---------- */
#define GdkBitmap_Private
#include "nsp/gtk/gdkbitmap.h"


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
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
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
  return ObjType(NthObj(i) , nsp_type_gdkbitmap_id);
}

int IsGdkBitmap(NspObject *O)
{
  return ObjType(O,nsp_type_gdkbitmap_id);
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

/*-------------------------------------------
 * function 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab dialog_func[]={
  { NULL, NULL}
};

/** call ith function in the dialog interface **/

int dialog_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(dialog_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void dialog_Interf_Info(int i, char **fname, function (**f))
{
  *fname = dialog_func[i].name;
  *f = dialog_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
dialog_register_classes(NspObject *d)
{
  NspObject *module;

  if ((module = PyImport_ImportModule("gtk")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _GtkTreeModel = (PyTypeObject *)PyDict_GetItemString(moddict, "Treemodel");
  } else {
      Py_FatalError("could not import gtk");
      return;
  }
  if ((module = PyImport_ImportModule("pango")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _PangoLayout = (PyTypeObject *)PyDict_GetItemString(moddict, "Layout");
      _PangoLanguage = (PyTypeObject *)PyDict_GetItemString(moddict, "Language");
      _PangoFontDescription = (PyTypeObject *)PyDict_GetItemString(moddict, "FontDescription");
      _PangoAttrList = (PyTypeObject *)PyDict_GetItemString(moddict, "AttrList");
      _PangoTabArray = (PyTypeObject *)PyDict_GetItemString(moddict, "TabArray");
      _PangoContext = (PyTypeObject *)PyDict_GetItemString(moddict, "Context");
  } else {
      Py_FatalError("could not import pango");
      return;
  }
  if ((module = PyImport_ImportModule("gdk")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _GdkAtom = (PyTypeObject *)PyDict_GetItemString(moddict, "Atom");
      _GdkPixmap = (PyTypeObject *)PyDict_GetItemString(moddict, "Pixmap");
      _GdkWindow = (PyTypeObject *)PyDict_GetItemString(moddict, "Window");
      _GdkGc = (PyTypeObject *)PyDict_GetItemString(moddict, "Gc");
      _GdkFont = (PyTypeObject *)PyDict_GetItemString(moddict, "Font");
      _GdkPixbuf = (PyTypeObject *)PyDict_GetItemString(moddict, "Pixbuf");
      _GdkPixbufAnimation = (PyTypeObject *)PyDict_GetItemString(moddict, "PixbufAnimation");
      _GdkDragContext = (PyTypeObject *)PyDict_GetItemString(moddict, "DragContext");
      _GdkColormap = (PyTypeObject *)PyDict_GetItemString(moddict, "Colormap");
      _GdkColor = (PyTypeObject *)PyDict_GetItemString(moddict, "Color");
      _GdkImage = (PyTypeObject *)PyDict_GetItemString(moddict, "Image");
      _GdkVisual = (PyTypeObject *)PyDict_GetItemString(moddict, "Visual");
  } else {
      Py_FatalError("could not import gdk");
      return;
  }


#line 24 "dialog.override"

#line 331 "dialog.c"
  nspgobject_register_class(d, "GdkBitmap", GDK_TYPE_DRAWABLE, &PyGdkBitmap_Type, Py_BuildValue("(O)", &PyGdkDrawable_Type));
}
*/
