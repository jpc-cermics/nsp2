/* -*- Mode: C -*- */


#line 4 "libglade.override"
#define NO_IMPORT_PYGOBJECT
#include <pygobject.h>
#include <glade/glade.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <libintl.h>

#line 14 "libglade.c"


/* ---------- types from other modules ---------- */
#include "nsp/gtk/nspgobject_type.h"
#include "nsp/gtk/nspgtkwidget_type.h"


/* ---------- forward type declarations ---------- */
#define GladeXML_Private
#include "nsp/gtk/gladexml.h"


/* ----------- GladeXML ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  GladeXML_Private 
#include "nsp/gtk/gladexml.h"
#include "nsp/interf.h"

/* NspGladeXML inherits from NspGObject */ 

int nsp_type_gladexml_id=0;
NspTypeGladeXML *nsp_type_gladexml=NULL;

NspTypeGladeXML *new_type_gladexml(type_mode mode)
{
  NspTypeGladeXML *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gladexml != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gladexml;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gladexml_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = gladexml_get_methods; 
  type->new = (new_func *) new_gladexml;

  /* specific methods for gladexml */
      
  type->init = (init_func *) init_gladexml;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gladexml */ 

  top->s_type =  (s_type_func *) gladexml_type_as_string;    
  top->sh_type = (sh_type_func *) gladexml_type_short_string;
  /* top->create = (create_func*) int_gladexml_create;*/ 
  
  /* specific methods for gladexml */
      
  type->init = (init_func *) init_gladexml;

  if ( nsp_type_gladexml_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGladeXML called nsp_type_gladexml
       */
      type->id =  nsp_type_gladexml_id = nsp_new_type_id();
      nsp_type_gladexml = type;
      if ( nsp_register_type(nsp_type_gladexml) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gladexml, GLADE_TYPE_XML);
      return ( mode == T_BASE ) ? type : new_type_gladexml(mode);
    }
  else 
    {
       type->id = nsp_type_gladexml_id;
       return type;
    }
}

/*
 * initialize GladeXML instances 
 * locally and by calling initializer on parent class 
 */

static int init_gladexml(NspGladeXML *o,NspTypeGladeXML *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GladeXML 
 */

NspGladeXML *new_gladexml() 
{
  NspGladeXML *loc; 
  /* type must exists */
  nsp_type_gladexml = new_type_gladexml(T_BASE);
  if ( (loc = malloc(sizeof(NspGladeXML)))== NULLGLADEXML) return loc;
  /* initialize object */
  if ( init_gladexml(loc,nsp_type_gladexml) == FAIL) return NULLGLADEXML;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GladeXML 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gladexml_type_name[]="GladeXML";
static char gladexml_short_type_name[]="GladeXML";

static char *gladexml_type_as_string(void)
{
  return(gladexml_type_name);
}

static char *gladexml_type_short_string(void)
{
  return(gladexml_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GladeXML objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGladeXML   *gladexml_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_gladexml_id) ) return ((NspGladeXML *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gladexml));
  return NULL;
}

int IsGladeXMLObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gladexml_id);
}

int IsGladeXML(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gladexml_id);
}

NspGladeXML  *GetGladeXMLCopy(Stack stack, int i)
{
  if (  GetGladeXML(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGladeXML  *GetGladeXML(Stack stack, int i)
{
  NspGladeXML *M;
  if (( M = gladexml_object(NthObj(i))) == NULLGLADEXML)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGladeXML *gladexml_copy(NspGladeXML *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gladexml);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gladexml);
}

/*-------------------------------------------------------------------
 * wrappers for the GladeXML
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGladeXML *H;
  CheckRhs(0,0);
  / * want to be sure that type gladexml is initialized * /
  nsp_type_gladexml = new_type_gladexml(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_gladexml)) == NULLGLADEXML) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_gladexml_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,new_opts,t_end};
  nsp_option opts[] = {
	{"root",string,NULLOBJ,-1}, 
	{"domain",string,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  char *fname, *root = NULL, *domain = NULL;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&fname, opts, &root, &domain) == FAIL) return RET_BUG;
  if ((ret = (GObject *)glade_xml_new(fname, root, domain))== NULL) return RET_BUG;

  nsp_type_gladexml = new_type_gladexml(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gladexml );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 31 "libglade.override"
static void
connect_one(const gchar *handler_name, GObject *obj,
            const gchar *signal_name, const gchar *signal_data,
            GObject *connect_object, gboolean after, gpointer user_data)
{
    GClosure *closure = NULL;
    NspObject *callback = NspTuple_GetItem((NspObject *)user_data, 0);
    NspObject *extra = NspTuple_GetItem((NspObject *)user_data, 1);
    NspObject *self;

    if (connect_object) {
        NspObject *other;

        other = pygobject_new(connect_object);
	closure = pyg_closure_new(callback, extra, other);
    } else {
	closure = pyg_closure_new(callback, extra, NULL);
    }

    self = pygobject_new(obj);
    g_signal_connect_closure(obj, signal_name, closure, after);
    pygobject_watch_closure(self, closure);
    Nsp_DECREF(self);
}

static NspObject *
_wrap_glade_xml_signal_connect(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
    guint len;
    NspObject *first, *callback, *extra_args = NULL, *data;
    gchar *handler_name;

    len = NspTuple_Size(args);
    if (len < 2) {
	Scierror(
		"GladeXML.signal_connect requires at least 2 arguments");
	return NULL;
    }
    first = NspSequence_GetSlice(args, 0, 2);
    if (!NspArg_ParseTuple(first, "sO:GladeXML.signal_connect", &handler_name,
			  &callback)) {
	Nsp_DECREF(first);
	return NULL;
    }
    Nsp_DECREF(first);
    if (!NspCallable_Check(callback)) {
	Scierror( "second argument must be callable");
	return NULL;
    }
    extra_args = NspSequence_GetSlice(args, 2, len);
    if (extra_args == NULL)
	return NULL;
    data = Nsp_BuildValue("(ON)", callback, extra_args);
    glade_xml_signal_connect_full(GLADE_XML(self->obj), handler_name,
				  connect_one, data);
    Nsp_DECREF(data);
    Nsp_INCREF(Nsp_None);
    return Nsp_None;
}
#line 301 "libglade.c"


#line 92 "libglade.override"
static void
connect_many(const gchar *handler_name, GObject *obj,
	     const gchar *signal_name, const gchar *signal_data,
	     GObject *connect_object, gboolean after, gpointer user_data)
{
    NspObject *handler_dict = user_data;
    NspObject *tuple, *self;
    GClosure *closure = NULL;

    tuple = NspMapping_GetItemString(handler_dict, (gchar *)handler_name);
    if (!tuple) {
	NspErr_Clear();	
        tuple = NspObject_GetAttrString(handler_dict, (gchar *)handler_name);
        if (!tuple) {
            NspErr_Clear();
            return;
        }
    }
    
    if (NspTuple_Check(tuple)) {
	NspObject *callback = NspTuple_GetItem(tuple, 0);
	NspObject *extra = NspSequence_GetSlice(tuple, 1, NspTuple_Size(tuple));
	NspObject *other = NULL;

	if (connect_object)
	    other = pygobject_new((GObject *)connect_object);

	closure = pyg_closure_new(callback, extra, other);
	Nsp_DECREF(extra);
    } else if (NspCallable_Check(tuple)) {
	NspObject *other = NULL;

	if (connect_object)
	    other = pygobject_new((GObject *)connect_object);

	closure = pyg_closure_new(tuple, NULL, other);
    } else {
	g_warning("handler for `%s' not callable or a tuple", handler_name);
	Nsp_DECREF(tuple);
	return;
    }

    self = pygobject_new(obj);
    g_signal_connect_closure(obj, signal_name, closure, after);
    pygobject_watch_closure(self, closure);
    Nsp_DECREF(self);
}

static NspObject *
_wrap_glade_xml_signal_autoconnect(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
    static char *kwlist[] = { "dict", NULL };
    NspObject *object;

    if (GetArgs(stack,rhs,opt,T,
				     "O:GladeXML.signal_autoconnect", kwlist,
				     &object))
	return NULL;

    glade_xml_signal_autoconnect_full(GLADE_XML(self->obj),
				      connect_many,
				      object);
    Nsp_INCREF(Nsp_None);
    return Nsp_None;
}
#line 370 "libglade.c"


static int _wrap_glade_xml_get_widget(NspGladeXML *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *name;
  NspObject *nsp_ret;
  GtkWidget *ret;

  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
  ret = glade_xml_get_widget(GLADE_XML(self->obj), name);
  nsp_type_gtkwidget = new_type_gtkwidget(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtkwidget))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 159 "libglade.override"
static NspObject *
_wrap_glade_xml_get_widget_prefix(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
    static char *kwlist[] = { "name", NULL };
    char *name;
    GList *ret, *tmp;
    NspObject *py_ret;

    if (GetArgs(stack,rhs,opt,T,
				     "s:GladeXML.get_widget_prefix", kwlist,
				     &name))
	return NULL;
    ret = glade_xml_get_widget_prefix(GLADE_XML(self->obj), name);
    py_ret = NspList_New(0);
    for (tmp = ret; tmp != NULL; tmp = tmp->next) {
	GtkWidget *widget = tmp->data;
	NspObject *py_widget = pygobject_new((GObject *)widget);

	if (!py_widget) {
	    g_list_free(ret);
	    Nsp_DECREF(py_ret);
	    return NULL;
	}
	NspList_Append(py_ret, py_widget);
	Nsp_DECREF(py_widget);
    }
    g_list_free(ret);
    return py_ret;
}
#line 418 "libglade.c"


static int _wrap_glade_xml_relative_file(NspGladeXML *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *filename;
  gchar *ret;

  if ( GetArgs(stack,rhs,opt,T,&filename) == FAIL) return RET_BUG;
  ret = glade_xml_relative_file(GLADE_XML(self->obj), filename);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static NspMethods gladexml_methods[] = {
  {"signal_connect",(nsp_method *) _wrap_glade_xml_signal_connect},
  {"signal_autoconnect",(nsp_method *) _wrap_glade_xml_signal_autoconnect},
  {"get_widget",(nsp_method *) _wrap_glade_xml_get_widget},
  {"get_widget_prefix",(nsp_method *) _wrap_glade_xml_get_widget_prefix},
  {"relative_file",(nsp_method *) _wrap_glade_xml_relative_file},
  { NULL, NULL}
};

static NspMethods *gladexml_get_methods(void) { return gladexml_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gladexml_attrs[]={{NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_glade_xml_new_from_buffer(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, s_int,new_opts,t_end};
  nsp_option opts[] = {
	{"root",string,NULLOBJ,-1}, 
	{"domain",string,NULLOBJ,-1}, 
	{NULL,t_end,NULLOBJ,-1} };
  char *buffer, *root = NULL, *domain = NULL;
  int size;
  NspObject *nsp_ret;
  GladeXML *ret;

  if ( GetArgs(stack,rhs,opt,T,&buffer, &size, opts, &root, &domain) == FAIL) return RET_BUG;
    ret = glade_xml_new_from_buffer(buffer, size, root, domain);
  nsp_type_gladexml = new_type_gladexml(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gladexml))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_glade_get_widget_name(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *widget;
  const gchar *ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtkwidget, &widget) == FAIL) return RET_BUG;
    ret = glade_get_widget_name(GTK_WIDGET(widget->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

int _wrap_glade_get_widget_tree(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *widget;
  NspObject *nsp_ret;
  GladeXML *ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtkwidget, &widget) == FAIL) return RET_BUG;
    ret = glade_get_widget_tree(GTK_WIDGET(widget->obj));
  nsp_type_gladexml = new_type_gladexml(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gladexml))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 190 "libglade.override"
static NspObject *
_wrap_glade_bindtextdomain(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
    static char *kwlist[] = { "domainname", "dirname", NULL };
    char *domainname, *dirname = NULL, *ret;
    
    if (GetArgs(stack,rhs,opt,T,
				     "s|s:glade.bindtextdomain", kwlist,
				     &domainname, &dirname))
	return NULL;
    ret = bindtextdomain(domainname, dirname);
    if (!ret) {
	NspErr_SetString(NspExc_MemoryError, "Not enough memory available.");
	return NULL;
    }
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
    bind_textdomain_codeset(domainname, "UTF-8");
#endif
    return nsp_new_string_obj(NVOID,ret,-1);
}
#line 521 "libglade.c"


#line 212 "libglade.override"
static NspObject *
_wrap_glade_textdomain(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
    static char *kwlist[] = { "domainname", NULL };
    char *domainname = NULL, *ret;
    
    if (GetArgs(stack,rhs,opt,T,
				     "|s:glade.textdomain", kwlist,
				     &domainname))
	return NULL;
    ret = textdomain(domainname);
    if (!ret) {
	NspErr_SetString(NspExc_MemoryError, "Not enough memory available.");
	return NULL;
    }
    return nsp_new_string_obj(NVOID,ret,-1);
}

#line 543 "libglade.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab libglade_func[]={
  {"gladexml_new", _wrap_gladexml_new},
  {"glade_xml_new_from_buffer", _wrap_glade_xml_new_from_buffer},
  {"glade_get_widget_name", _wrap_glade_get_widget_name},
  {"glade_get_widget_tree", _wrap_glade_get_widget_tree},
  {"glade_bindtextdomain", _wrap_glade_bindtextdomain},
  {"glade_textdomain", _wrap_glade_textdomain},
  { NULL, NULL}
};

/** call ith function in the libglade interface **/

int libglade_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(libglade_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void libglade_Interf_Info(int i, char **fname, function (**f))
{
  *fname = libglade_func[i].name;
  *f = libglade_func[i].fonc;
}
/* intialise stuff extension classes */
/* void
libglade_register_classes(NspObject *d)
{
  NspObject *module;

  if ((module = PyImport_ImportModule("gobject")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _NspGObject_Type = (PyTypeObject *)PyDict_GetItemString(moddict, "GObject");
  } else {
      Py_FatalError("could not import gobject");
      return;
  }
  if ((module = PyImport_ImportModule("gtk._gtk")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _NspGtkWidget_Type = (PyTypeObject *)PyDict_GetItemString(moddict, "Widget");
  } else {
      Py_FatalError("could not import gtk._gtk");
      return;
  }


#line 600 "libglade.c"
  nspgobject_register_class(d, "GladeXML", GLADE_TYPE_XML, &PyGladeXML_Type, Py_BuildValue("(O)", &PyGObject_Type));
}
*/
