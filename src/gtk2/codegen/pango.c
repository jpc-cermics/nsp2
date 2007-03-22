/* -*- Mode: C -*- */


#line 4 "pango.override"
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <pango/pango.h>
#include "nsp/gtk/pangoattribute.h"
#include "nsp/all-pango.h"

#line 14 "pango.c"


/* ---------- types from other modules ---------- */
#include "nsp/gtk/gobject.h"


/* ---------- forward type declarations ---------- */
#include "nsp/gtk/pangoattrlist.h"
#include "nsp/gtk/pangocolor.h"
#include "nsp/gtk/pangofontdescription.h"
#include "nsp/gtk/pangofontmetrics.h"
#include "nsp/gtk/pangoglyphstring.h"
#include "nsp/gtk/pangolanguage.h"
#include "nsp/gtk/pangotabarray.h"
#include "nsp/gtk/pangocontext.h"
#include "nsp/gtk/pangofont.h"
#include "nsp/gtk/pangofontface.h"
#include "nsp/gtk/pangofontfamily.h"
#include "nsp/gtk/pangofontmap.h"
#include "nsp/gtk/pangofontset.h"
#include "nsp/gtk/pangolayout.h"


/* ----------- PangoAttrList ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoAttrList_Private 
#include "nsp/gtk/pangoattrlist.h"
#include "nsp/interf.h"

/* NspPangoAttrList inherits from NspGBoxed */ 

int nsp_type_pangoattrlist_id=0;
NspTypePangoAttrList *nsp_type_pangoattrlist=NULL;

NspTypePangoAttrList *new_type_pangoattrlist(type_mode mode)
{
  NspTypePangoAttrList *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangoattrlist != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangoattrlist;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangoattrlist_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangoattrlist_get_methods; 
  type->new = (new_func *) new_pangoattrlist;

  /* specific methods for pangoattrlist */
      
  type->init = (init_func *) init_pangoattrlist;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangoattrlist */ 

  top->s_type =  (s_type_func *) pangoattrlist_type_as_string;    
  top->sh_type = (sh_type_func *) pangoattrlist_type_short_string;
  /* top->create = (create_func*) int_pangoattrlist_create;*/ 
  
  /* specific methods for pangoattrlist */
      
  type->init = (init_func *) init_pangoattrlist;

  if ( nsp_type_pangoattrlist_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoAttrList called nsp_type_pangoattrlist
       */
      type->id =  nsp_type_pangoattrlist_id = nsp_new_type_id();
      nsp_type_pangoattrlist = type;
      if ( nsp_register_type(nsp_type_pangoattrlist) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangoattrlist, PANGO_TYPE_ATTR_LIST);
      return ( mode == T_BASE ) ? type : new_type_pangoattrlist(mode);
    }
  else 
    {
       type->id = nsp_type_pangoattrlist_id;
       return type;
    }
}

/*
 * initialize PangoAttrList instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangoattrlist(NspPangoAttrList *o,NspTypePangoAttrList *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoAttrList 
 */

NspPangoAttrList *new_pangoattrlist() 
{
  NspPangoAttrList *loc; 
  /* type must exists */
  nsp_type_pangoattrlist = new_type_pangoattrlist(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoAttrList)))== NULLPANGOATTRLIST) return loc;
  /* initialize object */
  if ( init_pangoattrlist(loc,nsp_type_pangoattrlist) == FAIL) return NULLPANGOATTRLIST;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoAttrList 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangoattrlist_type_name[]="PangoAttrList";
static char pangoattrlist_short_type_name[]="PangoAttrList";

static char *pangoattrlist_type_as_string(void)
{
  return(pangoattrlist_type_name);
}

static char *pangoattrlist_type_short_string(NspObject *v)
{
  return(pangoattrlist_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoAttrList objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoAttrList   *pangoattrlist_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangoattrlist_id) ) return ((NspPangoAttrList *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangoattrlist));
  return NULL;
}

int IsPangoAttrListObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangoattrlist_id);
}

int IsPangoAttrList(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangoattrlist_id);
}

NspPangoAttrList  *GetPangoAttrListCopy(Stack stack, int i)
{
  if (  GetPangoAttrList(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoAttrList  *GetPangoAttrList(Stack stack, int i)
{
  NspPangoAttrList *M;
  if (( M = pangoattrlist_object(NthObj(i))) == NULLPANGOATTRLIST)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspPangoAttrList *pangoattrlist_copy(NspPangoAttrList *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_pangoattrlist);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoAttrList
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoAttrList *H;
  CheckRhs(0,0);
  / * want to be sure that type pangoattrlist is initialized * /
  nsp_type_pangoattrlist = new_type_pangoattrlist(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_pangoattrlist)) == NULLPANGOATTRLIST) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_pangoattrlist_new(Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)pango_attr_list_new())== NULL) return RET_BUG;

  nsp_type_pangoattrlist = new_type_pangoattrlist(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_ATTR_LIST, ret,TRUE,TRUE,(NspTypeBase *) nsp_type_pangoattrlist );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_attr_list_copy(NspPangoAttrList *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  PangoAttrList *ret;

  ret = pango_attr_list_copy(NSP_GBOXED_GET(self, PangoAttrList));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_ATTR_LIST, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangoattrlist))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 303 "pango.override"
static int
_wrap_pango_attr_list_insert(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj_check , t_end} ;
  NspPangoAttribute *py_attr;
  PangoAttribute *attr;

  if (GetArgs(stack,rhs,opt,T, &nsp_type_pangoattribute, &py_attr)  == FAIL ) return RET_BUG;
  attr = pango_attribute_copy(py_attr->attr);
  pango_attr_list_insert(nspg_boxed_get(self, PangoAttrList), attr);
  return 0;
}
#line 271 "pango.c"


#line 317 "pango.override"
static int
_wrap_pango_attr_list_insert_before(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj_check , t_end} ;

  NspPangoAttribute *py_attr;
  PangoAttribute *attr;

  if (GetArgs(stack,rhs,opt,T, &nsp_type_pangoattribute, &py_attr)   == FAIL ) return RET_BUG;
  attr = pango_attribute_copy(py_attr->attr);
  pango_attr_list_insert_before(nspg_boxed_get(self, PangoAttrList), attr);
  return 0;
}
#line 288 "pango.c"


#line 332 "pango.override"
static int
_wrap_pango_attr_list_change(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj_check, t_end} ;
  NspPangoAttribute *py_attr;
  PangoAttribute *attr;

  if (GetArgs(stack,rhs,opt,T,  &nsp_type_pangoattribute, &py_attr)   == FAIL ) return RET_BUG;

  attr = pango_attribute_copy(py_attr->attr);
  pango_attr_list_change(nspg_boxed_get(self, PangoAttrList), attr);
  return 0;
}
#line 305 "pango.c"


static int _wrap_pango_attr_list_splice(NspPangoAttrList *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, s_int, s_int,t_end};
  int pos, len;
  NspObject *nsp_other;
  PangoAttrList *other = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_other, &pos, &len) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_other, PANGO_TYPE_ATTR_LIST))
      other = nspg_boxed_get(nsp_other, PangoAttrList);
  else {
      Scierror( "other should be a PangoAttrList");
      return RET_BUG;
  }
  pango_attr_list_splice(NSP_GBOXED_GET(self, PangoAttrList), other, pos, len);
  return 0;
}

static NspMethods pangoattrlist_methods[] = {
  {"copy",(nsp_method *) _wrap_pango_attr_list_copy},
  {"insert",(nsp_method *) _wrap_pango_attr_list_insert},
  {"insert_before",(nsp_method *) _wrap_pango_attr_list_insert_before},
  {"change",(nsp_method *) _wrap_pango_attr_list_change},
  {"splice",(nsp_method *) _wrap_pango_attr_list_splice},
  { NULL, NULL}
};

static NspMethods *pangoattrlist_get_methods(void) { return pangoattrlist_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangoattrlist_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoColor ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoColor_Private 
#include "nsp/gtk/pangocolor.h"
#include "nsp/interf.h"

/* NspPangoColor inherits from NspGBoxed */ 

int nsp_type_pangocolor_id=0;
NspTypePangoColor *nsp_type_pangocolor=NULL;

NspTypePangoColor *new_type_pangocolor(type_mode mode)
{
  NspTypePangoColor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangocolor != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangocolor;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangocolor_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangocolor_get_methods; 
  type->new = (new_func *) new_pangocolor;

  /* specific methods for pangocolor */
      
  type->init = (init_func *) init_pangocolor;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangocolor */ 

  top->s_type =  (s_type_func *) pangocolor_type_as_string;    
  top->sh_type = (sh_type_func *) pangocolor_type_short_string;
  /* top->create = (create_func*) int_pangocolor_create;*/ 
  
  /* specific methods for pangocolor */
      
  type->init = (init_func *) init_pangocolor;

  if ( nsp_type_pangocolor_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoColor called nsp_type_pangocolor
       */
      type->id =  nsp_type_pangocolor_id = nsp_new_type_id();
      nsp_type_pangocolor = type;
      if ( nsp_register_type(nsp_type_pangocolor) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangocolor, PANGO_TYPE_COLOR);
      return ( mode == T_BASE ) ? type : new_type_pangocolor(mode);
    }
  else 
    {
       type->id = nsp_type_pangocolor_id;
       return type;
    }
}

/*
 * initialize PangoColor instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangocolor(NspPangoColor *o,NspTypePangoColor *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoColor 
 */

NspPangoColor *new_pangocolor() 
{
  NspPangoColor *loc; 
  /* type must exists */
  nsp_type_pangocolor = new_type_pangocolor(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoColor)))== NULLPANGOCOLOR) return loc;
  /* initialize object */
  if ( init_pangocolor(loc,nsp_type_pangocolor) == FAIL) return NULLPANGOCOLOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoColor 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangocolor_type_name[]="PangoColor";
static char pangocolor_short_type_name[]="PangoColor";

static char *pangocolor_type_as_string(void)
{
  return(pangocolor_type_name);
}

static char *pangocolor_type_short_string(NspObject *v)
{
  return(pangocolor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoColor objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoColor   *pangocolor_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangocolor_id) ) return ((NspPangoColor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangocolor));
  return NULL;
}

int IsPangoColorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangocolor_id);
}

int IsPangoColor(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangocolor_id);
}

NspPangoColor  *GetPangoColorCopy(Stack stack, int i)
{
  if (  GetPangoColor(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoColor  *GetPangoColor(Stack stack, int i)
{
  NspPangoColor *M;
  if (( M = pangocolor_object(NthObj(i))) == NULLPANGOCOLOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspPangoColor *pangocolor_copy(NspPangoColor *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_pangocolor);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoColor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoColor *H;
  CheckRhs(0,0);
  / * want to be sure that type pangocolor is initialized * /
  nsp_type_pangocolor = new_type_pangocolor(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_pangocolor)) == NULLPANGOCOLOR) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_pango_color_parse(NspPangoColor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *spec;
  int ret;

  if ( GetArgs(stack,rhs,opt,T,&spec) == FAIL) return RET_BUG;
  ret = pango_color_parse(NSP_GBOXED_GET(self, PangoColor), spec);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods pangocolor_methods[] = {
  {"parse",(nsp_method *) _wrap_pango_color_parse},
  { NULL, NULL}
};

static NspMethods *pangocolor_get_methods(void) { return pangocolor_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_pango_color__get_red(NspObject *self,char *attr)
{
  int ret;

  ret = NSP_GBOXED_GET(self, PangoColor)->red;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_pango_color__get_green(NspObject *self,char *attr)
{
  int ret;

  ret = NSP_GBOXED_GET(self, PangoColor)->green;
  return nsp_new_double_obj((double) ret);
}

static NspObject *_wrap_pango_color__get_blue(NspObject *self,char *attr)
{
  int ret;

  ret = NSP_GBOXED_GET(self, PangoColor)->blue;
  return nsp_new_double_obj((double) ret);
}

static AttrTab pangocolor_attrs[] = {
  { "red", (attr_get_function *)_wrap_pango_color__get_red, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "green", (attr_get_function *)_wrap_pango_color__get_green, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { "blue", (attr_get_function *)_wrap_pango_color__get_blue, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- PangoFontDescription ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoFontDescription_Private 
#include "nsp/gtk/pangofontdescription.h"
#include "nsp/interf.h"

/* NspPangoFontDescription inherits from NspGBoxed */ 

int nsp_type_pangofontdescription_id=0;
NspTypePangoFontDescription *nsp_type_pangofontdescription=NULL;

NspTypePangoFontDescription *new_type_pangofontdescription(type_mode mode)
{
  NspTypePangoFontDescription *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangofontdescription != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangofontdescription;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangofontdescription_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangofontdescription_get_methods; 
  type->new = (new_func *) new_pangofontdescription;

  /* specific methods for pangofontdescription */
      
  type->init = (init_func *) init_pangofontdescription;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontdescription */ 

  top->s_type =  (s_type_func *) pangofontdescription_type_as_string;    
  top->sh_type = (sh_type_func *) pangofontdescription_type_short_string;
  /* top->create = (create_func*) int_pangofontdescription_create;*/ 
  
  /* specific methods for pangofontdescription */
      
  type->init = (init_func *) init_pangofontdescription;

  if ( nsp_type_pangofontdescription_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoFontDescription called nsp_type_pangofontdescription
       */
      type->id =  nsp_type_pangofontdescription_id = nsp_new_type_id();
      nsp_type_pangofontdescription = type;
      if ( nsp_register_type(nsp_type_pangofontdescription) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangofontdescription, PANGO_TYPE_FONT_DESCRIPTION);
      return ( mode == T_BASE ) ? type : new_type_pangofontdescription(mode);
    }
  else 
    {
       type->id = nsp_type_pangofontdescription_id;
       return type;
    }
}

/*
 * initialize PangoFontDescription instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontdescription(NspPangoFontDescription *o,NspTypePangoFontDescription *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoFontDescription 
 */

NspPangoFontDescription *new_pangofontdescription() 
{
  NspPangoFontDescription *loc; 
  /* type must exists */
  nsp_type_pangofontdescription = new_type_pangofontdescription(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoFontDescription)))== NULLPANGOFONTDESCRIPTION) return loc;
  /* initialize object */
  if ( init_pangofontdescription(loc,nsp_type_pangofontdescription) == FAIL) return NULLPANGOFONTDESCRIPTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoFontDescription 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontdescription_type_name[]="PangoFontDescription";
static char pangofontdescription_short_type_name[]="PangoFontDescription";

static char *pangofontdescription_type_as_string(void)
{
  return(pangofontdescription_type_name);
}

static char *pangofontdescription_type_short_string(NspObject *v)
{
  return(pangofontdescription_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoFontDescription objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoFontDescription   *pangofontdescription_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangofontdescription_id) ) return ((NspPangoFontDescription *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangofontdescription));
  return NULL;
}

int IsPangoFontDescriptionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangofontdescription_id);
}

int IsPangoFontDescription(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangofontdescription_id);
}

NspPangoFontDescription  *GetPangoFontDescriptionCopy(Stack stack, int i)
{
  if (  GetPangoFontDescription(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoFontDescription  *GetPangoFontDescription(Stack stack, int i)
{
  NspPangoFontDescription *M;
  if (( M = pangofontdescription_object(NthObj(i))) == NULLPANGOFONTDESCRIPTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspPangoFontDescription *pangofontdescription_copy(NspPangoFontDescription *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_pangofontdescription);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoFontDescription
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoFontDescription *H;
  CheckRhs(0,0);
  / * want to be sure that type pangofontdescription is initialized * /
  nsp_type_pangofontdescription = new_type_pangofontdescription(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_pangofontdescription)) == NULLPANGOFONTDESCRIPTION) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 349 "pango.override"
static int
_wrap_pangofontdescription_new( Stack stack,int rhs,int opt,int lhs)
{
  gpointer  *boxed ;
  NspObject *ret;
  char *str = NULL;
  
  CheckRhs(0,1); 
  if ( rhs == 1 ) 
    { 
      if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
      boxed = (gpointer  *) pango_font_description_from_string(str);      
    }
  else 
    boxed = (gpointer  *) pango_font_description_new();
  if (boxed == NULL ) {
    Scierror("could not create PangoFontDescription object");
    return RET_BUG; 
  }
  if ((ret =(NspObject *)gboxed_create(NVOID,PANGO_TYPE_FONT_DESCRIPTION, boxed, TRUE, TRUE, 
				       (NspTypeBase *) nsp_type_pangofontdescription))  == NULL) 
    return RET_BUG; 
  MoveObj(stack,1,ret);
  return 1;
}
#line 810 "pango.c"


#line 397 "pango.override"
static NspObject *
_wrap_pango_font_description_copy(NspObject *self)
{
  return (NspObject *)gboxed_create(NVOID,PANGO_TYPE_FONT_DESCRIPTION,
				    nspg_boxed_get(self, PangoFontDescription),
				    TRUE, TRUE, NULL);
}
#line 821 "pango.c"


static int _wrap_pango_font_description_copy_static(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoFontDescription *ret;
  NspObject *nsp_ret;

  ret = pango_font_description_copy_static(NSP_GBOXED_GET(self, PangoFontDescription));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_FONT_DESCRIPTION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangofontdescription))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_font_description_hash(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_font_description_hash(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_set_family(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *family;

  if ( GetArgs(stack,rhs,opt,T,&family) == FAIL) return RET_BUG;
  pango_font_description_set_family(NSP_GBOXED_GET(self, PangoFontDescription), family);
  return 0;
}

static int _wrap_pango_font_description_set_family_static(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *family;

  if ( GetArgs(stack,rhs,opt,T,&family) == FAIL) return RET_BUG;
  pango_font_description_set_family_static(NSP_GBOXED_GET(self, PangoFontDescription), family);
  return 0;
}

static int _wrap_pango_font_description_get_family(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;

  ret = pango_font_description_get_family(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_set_style(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoStyle style;
  NspObject *nsp_style = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_style) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_STYLE, nsp_style, &style)== FAIL)
      return RET_BUG;
  pango_font_description_set_style(NSP_GBOXED_GET(self, PangoFontDescription), style);
  return 0;
}

static int _wrap_pango_font_description_get_style(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = pango_font_description_get_style(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_set_variant(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_variant = NULL;
  PangoVariant variant;

  if ( GetArgs(stack,rhs,opt,T,&nsp_variant) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_VARIANT, nsp_variant, &variant)== FAIL)
      return RET_BUG;
  pango_font_description_set_variant(NSP_GBOXED_GET(self, PangoFontDescription), variant);
  return 0;
}

static int _wrap_pango_font_description_get_variant(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = pango_font_description_get_variant(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_set_weight(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoWeight weight;
  NspObject *nsp_weight = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_weight) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_WEIGHT, nsp_weight, &weight)== FAIL)
      return RET_BUG;
  pango_font_description_set_weight(NSP_GBOXED_GET(self, PangoFontDescription), weight);
  return 0;
}

static int _wrap_pango_font_description_get_weight(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = pango_font_description_get_weight(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_set_stretch(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoStretch stretch;
  NspObject *nsp_stretch = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_stretch) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_STRETCH, nsp_stretch, &stretch)== FAIL)
      return RET_BUG;
  pango_font_description_set_stretch(NSP_GBOXED_GET(self, PangoFontDescription), stretch);
  return 0;
}

static int _wrap_pango_font_description_get_stretch(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = pango_font_description_get_stretch(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_set_size(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int size;

  if ( GetArgs(stack,rhs,opt,T,&size) == FAIL) return RET_BUG;
  pango_font_description_set_size(NSP_GBOXED_GET(self, PangoFontDescription), size);
  return 0;
}

static int _wrap_pango_font_description_get_size(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_font_description_get_size(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_get_set_fields(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;

  ret = pango_font_description_get_set_fields(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_unset_fields(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_to_unset = NULL;
  PangoFontMask to_unset;

  if ( GetArgs(stack,rhs,opt,T,&nsp_to_unset) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(PANGO_TYPE_FONT_MASK, nsp_to_unset, &to_unset)==FAIL)
      return RET_BUG;
  pango_font_description_unset_fields(NSP_GBOXED_GET(self, PangoFontDescription), to_unset);
  return 0;
}

static int _wrap_pango_font_description_merge(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, s_bool,t_end};
  int replace_existing;
  PangoFontDescription *desc_to_merge = NULL;
  NspObject *nsp_desc_to_merge;

  if ( GetArgs(stack,rhs,opt,T,&nsp_desc_to_merge, &replace_existing) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc_to_merge, PANGO_TYPE_FONT_DESCRIPTION))
      desc_to_merge = nspg_boxed_get(nsp_desc_to_merge, PangoFontDescription);
  else {
      Scierror( "desc_to_merge should be a PangoFontDescription");
      return RET_BUG;
  }
  pango_font_description_merge(NSP_GBOXED_GET(self, PangoFontDescription), desc_to_merge, replace_existing);
  return 0;
}

static int _wrap_pango_font_description_merge_static(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, s_bool,t_end};
  int replace_existing;
  PangoFontDescription *desc_to_merge = NULL;
  NspObject *nsp_desc_to_merge;

  if ( GetArgs(stack,rhs,opt,T,&nsp_desc_to_merge, &replace_existing) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc_to_merge, PANGO_TYPE_FONT_DESCRIPTION))
      desc_to_merge = nspg_boxed_get(nsp_desc_to_merge, PangoFontDescription);
  else {
      Scierror( "desc_to_merge should be a PangoFontDescription");
      return RET_BUG;
  }
  pango_font_description_merge_static(NSP_GBOXED_GET(self, PangoFontDescription), desc_to_merge, replace_existing);
  return 0;
}

static int _wrap_pango_font_description_better_match(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, obj,t_end};
  int ret;
  PangoFontDescription *old_match = NULL, *new_match = NULL;
  NspObject *nsp_old_match, *nsp_new_match;

  if ( GetArgs(stack,rhs,opt,T,&nsp_old_match, &nsp_new_match) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_old_match, PANGO_TYPE_FONT_DESCRIPTION))
      old_match = nspg_boxed_get(nsp_old_match, PangoFontDescription);
  else {
      Scierror( "old_match should be a PangoFontDescription");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_new_match, PANGO_TYPE_FONT_DESCRIPTION))
      new_match = nspg_boxed_get(nsp_new_match, PangoFontDescription);
  else {
      Scierror( "new_match should be a PangoFontDescription");
      return RET_BUG;
  }
  ret = pango_font_description_better_match(NSP_GBOXED_GET(self, PangoFontDescription), old_match, new_match);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_to_string(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;

  ret = pango_font_description_to_string(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_pango_font_description_to_filename(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;

  ret = pango_font_description_to_filename(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static NspMethods pangofontdescription_methods[] = {
  {"copy",(nsp_method *) _wrap_pango_font_description_copy},
  {"copy_static",(nsp_method *) _wrap_pango_font_description_copy_static},
  {"hash",(nsp_method *) _wrap_pango_font_description_hash},
  {"set_family",(nsp_method *) _wrap_pango_font_description_set_family},
  {"set_family_static",(nsp_method *) _wrap_pango_font_description_set_family_static},
  {"get_family",(nsp_method *) _wrap_pango_font_description_get_family},
  {"set_style",(nsp_method *) _wrap_pango_font_description_set_style},
  {"get_style",(nsp_method *) _wrap_pango_font_description_get_style},
  {"set_variant",(nsp_method *) _wrap_pango_font_description_set_variant},
  {"get_variant",(nsp_method *) _wrap_pango_font_description_get_variant},
  {"set_weight",(nsp_method *) _wrap_pango_font_description_set_weight},
  {"get_weight",(nsp_method *) _wrap_pango_font_description_get_weight},
  {"set_stretch",(nsp_method *) _wrap_pango_font_description_set_stretch},
  {"get_stretch",(nsp_method *) _wrap_pango_font_description_get_stretch},
  {"set_size",(nsp_method *) _wrap_pango_font_description_set_size},
  {"get_size",(nsp_method *) _wrap_pango_font_description_get_size},
  {"get_set_fields",(nsp_method *) _wrap_pango_font_description_get_set_fields},
  {"unset_fields",(nsp_method *) _wrap_pango_font_description_unset_fields},
  {"merge",(nsp_method *) _wrap_pango_font_description_merge},
  {"merge_static",(nsp_method *) _wrap_pango_font_description_merge_static},
  {"better_match",(nsp_method *) _wrap_pango_font_description_better_match},
  {"to_string",(nsp_method *) _wrap_pango_font_description_to_string},
  {"to_filename",(nsp_method *) _wrap_pango_font_description_to_filename},
  { NULL, NULL}
};

static NspMethods *pangofontdescription_get_methods(void) { return pangofontdescription_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangofontdescription_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoFontMetrics ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoFontMetrics_Private 
#include "nsp/gtk/pangofontmetrics.h"
#include "nsp/interf.h"

/* NspPangoFontMetrics inherits from NspGBoxed */ 

int nsp_type_pangofontmetrics_id=0;
NspTypePangoFontMetrics *nsp_type_pangofontmetrics=NULL;

NspTypePangoFontMetrics *new_type_pangofontmetrics(type_mode mode)
{
  NspTypePangoFontMetrics *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangofontmetrics != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangofontmetrics;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangofontmetrics_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangofontmetrics_get_methods; 
  type->new = (new_func *) new_pangofontmetrics;

  /* specific methods for pangofontmetrics */
      
  type->init = (init_func *) init_pangofontmetrics;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontmetrics */ 

  top->s_type =  (s_type_func *) pangofontmetrics_type_as_string;    
  top->sh_type = (sh_type_func *) pangofontmetrics_type_short_string;
  /* top->create = (create_func*) int_pangofontmetrics_create;*/ 
  
  /* specific methods for pangofontmetrics */
      
  type->init = (init_func *) init_pangofontmetrics;

  if ( nsp_type_pangofontmetrics_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoFontMetrics called nsp_type_pangofontmetrics
       */
      type->id =  nsp_type_pangofontmetrics_id = nsp_new_type_id();
      nsp_type_pangofontmetrics = type;
      if ( nsp_register_type(nsp_type_pangofontmetrics) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangofontmetrics, PANGO_TYPE_FONT_METRICS);
      return ( mode == T_BASE ) ? type : new_type_pangofontmetrics(mode);
    }
  else 
    {
       type->id = nsp_type_pangofontmetrics_id;
       return type;
    }
}

/*
 * initialize PangoFontMetrics instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontmetrics(NspPangoFontMetrics *o,NspTypePangoFontMetrics *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoFontMetrics 
 */

NspPangoFontMetrics *new_pangofontmetrics() 
{
  NspPangoFontMetrics *loc; 
  /* type must exists */
  nsp_type_pangofontmetrics = new_type_pangofontmetrics(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoFontMetrics)))== NULLPANGOFONTMETRICS) return loc;
  /* initialize object */
  if ( init_pangofontmetrics(loc,nsp_type_pangofontmetrics) == FAIL) return NULLPANGOFONTMETRICS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoFontMetrics 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontmetrics_type_name[]="PangoFontMetrics";
static char pangofontmetrics_short_type_name[]="PangoFontMetrics";

static char *pangofontmetrics_type_as_string(void)
{
  return(pangofontmetrics_type_name);
}

static char *pangofontmetrics_type_short_string(NspObject *v)
{
  return(pangofontmetrics_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoFontMetrics objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoFontMetrics   *pangofontmetrics_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangofontmetrics_id) ) return ((NspPangoFontMetrics *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangofontmetrics));
  return NULL;
}

int IsPangoFontMetricsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangofontmetrics_id);
}

int IsPangoFontMetrics(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangofontmetrics_id);
}

NspPangoFontMetrics  *GetPangoFontMetricsCopy(Stack stack, int i)
{
  if (  GetPangoFontMetrics(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoFontMetrics  *GetPangoFontMetrics(Stack stack, int i)
{
  NspPangoFontMetrics *M;
  if (( M = pangofontmetrics_object(NthObj(i))) == NULLPANGOFONTMETRICS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspPangoFontMetrics *pangofontmetrics_copy(NspPangoFontMetrics *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_pangofontmetrics);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoFontMetrics
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoFontMetrics *H;
  CheckRhs(0,0);
  / * want to be sure that type pangofontmetrics is initialized * /
  nsp_type_pangofontmetrics = new_type_pangofontmetrics(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_pangofontmetrics)) == NULLPANGOFONTMETRICS) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_pango_font_metrics_get_ascent(NspPangoFontMetrics *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_font_metrics_get_ascent(NSP_GBOXED_GET(self, PangoFontMetrics));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_metrics_get_descent(NspPangoFontMetrics *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_font_metrics_get_descent(NSP_GBOXED_GET(self, PangoFontMetrics));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_metrics_get_approximate_char_width(NspPangoFontMetrics *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_font_metrics_get_approximate_char_width(NSP_GBOXED_GET(self, PangoFontMetrics));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_metrics_get_approximate_digit_width(NspPangoFontMetrics *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_font_metrics_get_approximate_digit_width(NSP_GBOXED_GET(self, PangoFontMetrics));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods pangofontmetrics_methods[] = {
  {"get_ascent",(nsp_method *) _wrap_pango_font_metrics_get_ascent},
  {"get_descent",(nsp_method *) _wrap_pango_font_metrics_get_descent},
  {"get_approximate_char_width",(nsp_method *) _wrap_pango_font_metrics_get_approximate_char_width},
  {"get_approximate_digit_width",(nsp_method *) _wrap_pango_font_metrics_get_approximate_digit_width},
  { NULL, NULL}
};

static NspMethods *pangofontmetrics_get_methods(void) { return pangofontmetrics_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangofontmetrics_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoGlyphString ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoGlyphString_Private 
#include "nsp/gtk/pangoglyphstring.h"
#include "nsp/interf.h"

/* NspPangoGlyphString inherits from NspGBoxed */ 

int nsp_type_pangoglyphstring_id=0;
NspTypePangoGlyphString *nsp_type_pangoglyphstring=NULL;

NspTypePangoGlyphString *new_type_pangoglyphstring(type_mode mode)
{
  NspTypePangoGlyphString *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangoglyphstring != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangoglyphstring;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangoglyphstring_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangoglyphstring_get_methods; 
  type->new = (new_func *) new_pangoglyphstring;

  /* specific methods for pangoglyphstring */
      
  type->init = (init_func *) init_pangoglyphstring;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangoglyphstring */ 

  top->s_type =  (s_type_func *) pangoglyphstring_type_as_string;    
  top->sh_type = (sh_type_func *) pangoglyphstring_type_short_string;
  /* top->create = (create_func*) int_pangoglyphstring_create;*/ 
  
  /* specific methods for pangoglyphstring */
      
  type->init = (init_func *) init_pangoglyphstring;

  if ( nsp_type_pangoglyphstring_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoGlyphString called nsp_type_pangoglyphstring
       */
      type->id =  nsp_type_pangoglyphstring_id = nsp_new_type_id();
      nsp_type_pangoglyphstring = type;
      if ( nsp_register_type(nsp_type_pangoglyphstring) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangoglyphstring, PANGO_TYPE_GLYPH_STRING);
      return ( mode == T_BASE ) ? type : new_type_pangoglyphstring(mode);
    }
  else 
    {
       type->id = nsp_type_pangoglyphstring_id;
       return type;
    }
}

/*
 * initialize PangoGlyphString instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangoglyphstring(NspPangoGlyphString *o,NspTypePangoGlyphString *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoGlyphString 
 */

NspPangoGlyphString *new_pangoglyphstring() 
{
  NspPangoGlyphString *loc; 
  /* type must exists */
  nsp_type_pangoglyphstring = new_type_pangoglyphstring(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoGlyphString)))== NULLPANGOGLYPHSTRING) return loc;
  /* initialize object */
  if ( init_pangoglyphstring(loc,nsp_type_pangoglyphstring) == FAIL) return NULLPANGOGLYPHSTRING;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoGlyphString 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangoglyphstring_type_name[]="PangoGlyphString";
static char pangoglyphstring_short_type_name[]="PangoGlyphString";

static char *pangoglyphstring_type_as_string(void)
{
  return(pangoglyphstring_type_name);
}

static char *pangoglyphstring_type_short_string(NspObject *v)
{
  return(pangoglyphstring_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoGlyphString objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoGlyphString   *pangoglyphstring_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangoglyphstring_id) ) return ((NspPangoGlyphString *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangoglyphstring));
  return NULL;
}

int IsPangoGlyphStringObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangoglyphstring_id);
}

int IsPangoGlyphString(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangoglyphstring_id);
}

NspPangoGlyphString  *GetPangoGlyphStringCopy(Stack stack, int i)
{
  if (  GetPangoGlyphString(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoGlyphString  *GetPangoGlyphString(Stack stack, int i)
{
  NspPangoGlyphString *M;
  if (( M = pangoglyphstring_object(NthObj(i))) == NULLPANGOGLYPHSTRING)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspPangoGlyphString *pangoglyphstring_copy(NspPangoGlyphString *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_pangoglyphstring);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoGlyphString
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoGlyphString *H;
  CheckRhs(0,0);
  / * want to be sure that type pangoglyphstring is initialized * /
  nsp_type_pangoglyphstring = new_type_pangoglyphstring(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_pangoglyphstring)) == NULLPANGOGLYPHSTRING) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_pangoglyphstring_new(Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)pango_glyph_string_new())== NULL) return RET_BUG;

  nsp_type_pangoglyphstring = new_type_pangoglyphstring(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_GLYPH_STRING, ret,TRUE,TRUE,(NspTypeBase *) nsp_type_pangoglyphstring );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_glyph_string_set_size(NspPangoGlyphString *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int new_len;

  if ( GetArgs(stack,rhs,opt,T,&new_len) == FAIL) return RET_BUG;
  pango_glyph_string_set_size(NSP_GBOXED_GET(self, PangoGlyphString), new_len);
  return 0;
}

static int _wrap_pango_glyph_string_copy(NspPangoGlyphString *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoGlyphString *ret;
  NspObject *nsp_ret;

  ret = pango_glyph_string_copy(NSP_GBOXED_GET(self, PangoGlyphString));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_GLYPH_STRING, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangoglyphstring))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 485 "pango.override"
static int
_wrap_pango_glyph_string_extents(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj, t_end} ;
  /* "fonts" */
  NspObject *font;
  PangoRectangle ink_rect, logical_rect;
  
  if (GetArgs(stack,rhs,opt,T,&font) == FAIL) return RET_BUG;

  if (!nspgobject_check(font, &nsp_type_pangofont)) {
    Scierror( "font must be a PangoFont");
    return RET_BUG;
  }

  pango_glyph_string_extents(nspg_boxed_get(self, PangoGlyphString),
			     PANGO_FONT(nspgobject_get(font)),
			     &ink_rect, &logical_rect);
  if ( nsp_move_doubles(stack,1,1,4,(double)ink_rect.x,(double) ink_rect.y,
			(double)ink_rect.width,(double) ink_rect.height) == FAIL) return RET_BUG ; 
  if ( nsp_move_doubles(stack,2,1,4,(double)logical_rect.x, (double)logical_rect.y,
			(double)logical_rect.width,(double) logical_rect.height) == FAIL) return RET_BUG ; 
  return 2; 
}
#line 1620 "pango.c"


#line 511 "pango.override"
static int
_wrap_pango_glyph_string_extents_range(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int ,s_int , obj , t_end} ; /* "start" "end"  "font" */ 

  gint start, end;
  NspObject *font;
  PangoRectangle ink_rect, logical_rect;

  if (GetArgs(stack,rhs,opt,T,&start, &end, &font)== FAIL) return RET_BUG;

  if (!nspgobject_check(font, &nsp_type_pangofont)) {
    Scierror( "font must be a PangoFont");
    return RET_BUG;
  }

  pango_glyph_string_extents_range(nspg_boxed_get(self, PangoGlyphString),
				   start, end,
				   PANGO_FONT(nspgobject_get(font)),
				   &ink_rect, &logical_rect);
  if ( nsp_move_doubles(stack,1,1,4,(double)ink_rect.x,(double) ink_rect.y,
			(double)ink_rect.width,(double) ink_rect.height) == FAIL) return RET_BUG ; 
  if ( nsp_move_doubles(stack,2,1,4,(double)logical_rect.x, (double)logical_rect.y,
			(double)logical_rect.width,(double) logical_rect.height) == FAIL) return RET_BUG ; 
  return 2; 
}
#line 1650 "pango.c"


#line 539 "pango.override"
static int
_wrap_pango_glyph_string_get_logical_widths(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { string ,s_int , t_end} ;
  const char *text;
  gint length, embedding_level, *logical_widths;
  gint i, slen;
  NspMatrix *ret;

  if (GetArgs(stack,rhs,opt,T, &text, &embedding_level)== FAIL) return RET_BUG;
  length = strlen(text);
  slen = g_utf8_strlen(text, length);
  logical_widths = g_new(int, slen);
  pango_glyph_string_get_logical_widths(nspg_boxed_get(self,PangoGlyphString),
					text, length, embedding_level,
					logical_widths);
  if (( ret = nsp_matrix_create(NVOID,'r',1, slen))== NULLMAT) return RET_BUG; 
  for (i = 0; i < slen ; i++)  ret->R[i ] = logical_widths[i];
  g_free(logical_widths);
  MoveObj(stack,1,(NspObject *)ret);
  return 1;
}
#line 1676 "pango.c"


static NspMethods pangoglyphstring_methods[] = {
  {"set_size",(nsp_method *) _wrap_pango_glyph_string_set_size},
  {"copy",(nsp_method *) _wrap_pango_glyph_string_copy},
  {"extents",(nsp_method *) _wrap_pango_glyph_string_extents},
  {"extents_range",(nsp_method *) _wrap_pango_glyph_string_extents_range},
  {"get_logical_widths",(nsp_method *) _wrap_pango_glyph_string_get_logical_widths},
  { NULL, NULL}
};

static NspMethods *pangoglyphstring_get_methods(void) { return pangoglyphstring_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_pango_glyph_string__get_num_glyphs(NspObject *self,char *attr)
{
  int ret;

  ret = NSP_GBOXED_GET(self, PangoGlyphString)->num_glyphs;
  return nsp_new_double_obj((double) ret);
}

static AttrTab pangoglyphstring_attrs[] = {
  { "num_glyphs", (attr_get_function *)_wrap_pango_glyph_string__get_num_glyphs, (attr_set_function *)int_set_failed,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};



/* ----------- PangoLanguage ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoLanguage_Private 
#include "nsp/gtk/pangolanguage.h"
#include "nsp/interf.h"

/* NspPangoLanguage inherits from NspGBoxed */ 

int nsp_type_pangolanguage_id=0;
NspTypePangoLanguage *nsp_type_pangolanguage=NULL;

NspTypePangoLanguage *new_type_pangolanguage(type_mode mode)
{
  NspTypePangoLanguage *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangolanguage != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangolanguage;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangolanguage_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangolanguage_get_methods; 
  type->new = (new_func *) new_pangolanguage;

  /* specific methods for pangolanguage */
      
  type->init = (init_func *) init_pangolanguage;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangolanguage */ 

  top->s_type =  (s_type_func *) pangolanguage_type_as_string;    
  top->sh_type = (sh_type_func *) pangolanguage_type_short_string;
  /* top->create = (create_func*) int_pangolanguage_create;*/ 
  
  /* specific methods for pangolanguage */
      
  type->init = (init_func *) init_pangolanguage;

  if ( nsp_type_pangolanguage_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoLanguage called nsp_type_pangolanguage
       */
      type->id =  nsp_type_pangolanguage_id = nsp_new_type_id();
      nsp_type_pangolanguage = type;
      if ( nsp_register_type(nsp_type_pangolanguage) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangolanguage, PANGO_TYPE_LANGUAGE);
      return ( mode == T_BASE ) ? type : new_type_pangolanguage(mode);
    }
  else 
    {
       type->id = nsp_type_pangolanguage_id;
       return type;
    }
}

/*
 * initialize PangoLanguage instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangolanguage(NspPangoLanguage *o,NspTypePangoLanguage *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoLanguage 
 */

NspPangoLanguage *new_pangolanguage() 
{
  NspPangoLanguage *loc; 
  /* type must exists */
  nsp_type_pangolanguage = new_type_pangolanguage(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoLanguage)))== NULLPANGOLANGUAGE) return loc;
  /* initialize object */
  if ( init_pangolanguage(loc,nsp_type_pangolanguage) == FAIL) return NULLPANGOLANGUAGE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoLanguage 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangolanguage_type_name[]="PangoLanguage";
static char pangolanguage_short_type_name[]="PangoLanguage";

static char *pangolanguage_type_as_string(void)
{
  return(pangolanguage_type_name);
}

static char *pangolanguage_type_short_string(NspObject *v)
{
  return(pangolanguage_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoLanguage objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoLanguage   *pangolanguage_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangolanguage_id) ) return ((NspPangoLanguage *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangolanguage));
  return NULL;
}

int IsPangoLanguageObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangolanguage_id);
}

int IsPangoLanguage(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangolanguage_id);
}

NspPangoLanguage  *GetPangoLanguageCopy(Stack stack, int i)
{
  if (  GetPangoLanguage(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoLanguage  *GetPangoLanguage(Stack stack, int i)
{
  NspPangoLanguage *M;
  if (( M = pangolanguage_object(NthObj(i))) == NULLPANGOLANGUAGE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspPangoLanguage *pangolanguage_copy(NspPangoLanguage *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_pangolanguage);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoLanguage
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoLanguage *H;
  CheckRhs(0,0);
  / * want to be sure that type pangolanguage is initialized * /
  nsp_type_pangolanguage = new_type_pangolanguage(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_pangolanguage)) == NULLPANGOLANGUAGE) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_pango_language_matches(NspPangoLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *range_list;
  int ret;

  if ( GetArgs(stack,rhs,opt,T,&range_list) == FAIL) return RET_BUG;
  ret = pango_language_matches(NSP_GBOXED_GET(self, PangoLanguage), range_list);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods pangolanguage_methods[] = {
  {"matches",(nsp_method *) _wrap_pango_language_matches},
  { NULL, NULL}
};

static NspMethods *pangolanguage_get_methods(void) { return pangolanguage_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangolanguage_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoTabArray ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoTabArray_Private 
#include "nsp/gtk/pangotabarray.h"
#include "nsp/interf.h"

/* NspPangoTabArray inherits from NspGBoxed */ 

int nsp_type_pangotabarray_id=0;
NspTypePangoTabArray *nsp_type_pangotabarray=NULL;

NspTypePangoTabArray *new_type_pangotabarray(type_mode mode)
{
  NspTypePangoTabArray *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangotabarray != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangotabarray;
    }
  if ((type =  malloc(sizeof(NspTypeGBoxed))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gboxed(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangotabarray_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangotabarray_get_methods; 
  type->new = (new_func *) new_pangotabarray;

  /* specific methods for pangotabarray */
      
  type->init = (init_func *) init_pangotabarray;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangotabarray */ 

  top->s_type =  (s_type_func *) pangotabarray_type_as_string;    
  top->sh_type = (sh_type_func *) pangotabarray_type_short_string;
  /* top->create = (create_func*) int_pangotabarray_create;*/ 
  
  /* specific methods for pangotabarray */
      
  type->init = (init_func *) init_pangotabarray;

  if ( nsp_type_pangotabarray_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoTabArray called nsp_type_pangotabarray
       */
      type->id =  nsp_type_pangotabarray_id = nsp_new_type_id();
      nsp_type_pangotabarray = type;
      if ( nsp_register_type(nsp_type_pangotabarray) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangotabarray, PANGO_TYPE_TAB_ARRAY);
      return ( mode == T_BASE ) ? type : new_type_pangotabarray(mode);
    }
  else 
    {
       type->id = nsp_type_pangotabarray_id;
       return type;
    }
}

/*
 * initialize PangoTabArray instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangotabarray(NspPangoTabArray *o,NspTypePangoTabArray *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoTabArray 
 */

NspPangoTabArray *new_pangotabarray() 
{
  NspPangoTabArray *loc; 
  /* type must exists */
  nsp_type_pangotabarray = new_type_pangotabarray(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoTabArray)))== NULLPANGOTABARRAY) return loc;
  /* initialize object */
  if ( init_pangotabarray(loc,nsp_type_pangotabarray) == FAIL) return NULLPANGOTABARRAY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoTabArray 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangotabarray_type_name[]="PangoTabArray";
static char pangotabarray_short_type_name[]="PangoTabArray";

static char *pangotabarray_type_as_string(void)
{
  return(pangotabarray_type_name);
}

static char *pangotabarray_type_short_string(NspObject *v)
{
  return(pangotabarray_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoTabArray objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoTabArray   *pangotabarray_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangotabarray_id) ) return ((NspPangoTabArray *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangotabarray));
  return NULL;
}

int IsPangoTabArrayObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangotabarray_id);
}

int IsPangoTabArray(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangotabarray_id);
}

NspPangoTabArray  *GetPangoTabArrayCopy(Stack stack, int i)
{
  if (  GetPangoTabArray(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoTabArray  *GetPangoTabArray(Stack stack, int i)
{
  NspPangoTabArray *M;
  if (( M = pangotabarray_object(NthObj(i))) == NULLPANGOTABARRAY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for boxed 
 */

NspPangoTabArray *pangotabarray_copy(NspPangoTabArray *self)
{
  return gboxed_create(NVOID,((NspGBoxed *) self)->gtype,((NspGBoxed *) self)->boxed, TRUE, TRUE,
                              (NspTypeBase *) nsp_type_pangotabarray);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoTabArray
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoTabArray *H;
  CheckRhs(0,0);
  / * want to be sure that type pangotabarray is initialized * /
  nsp_type_pangotabarray = new_type_pangotabarray(T_BASE);
  if(( H = gboxed_create(NVOID,(NspTypeBase *) nsp_type_pangotabarray)) == NULLPANGOTABARRAY) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_pangotabarray_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, s_bool,t_end};
  int initial_size, positions_in_pixels;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&initial_size, &positions_in_pixels) == FAIL) return RET_BUG;
  if ((ret = (GObject *)pango_tab_array_new(initial_size, positions_in_pixels))== NULL) return RET_BUG;

  nsp_type_pangotabarray = new_type_pangotabarray(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_TAB_ARRAY, ret,TRUE,TRUE,(NspTypeBase *) nsp_type_pangotabarray );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_tab_array_copy(NspPangoTabArray *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  PangoTabArray *ret;

  ret = pango_tab_array_copy(NSP_GBOXED_GET(self, PangoTabArray));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_TAB_ARRAY, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangotabarray))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_tab_array_get_size(NspPangoTabArray *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_tab_array_get_size(NSP_GBOXED_GET(self, PangoTabArray));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_tab_array_resize(NspPangoTabArray *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int new_size;

  if ( GetArgs(stack,rhs,opt,T,&new_size) == FAIL) return RET_BUG;
  pango_tab_array_resize(NSP_GBOXED_GET(self, PangoTabArray), new_size);
  return 0;
}

static int _wrap_pango_tab_array_set_tab(NspPangoTabArray *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, obj, s_int,t_end};
  int tab_index, location;
  NspObject *nsp_alignment = NULL;
  PangoTabAlign alignment;

  if ( GetArgs(stack,rhs,opt,T,&tab_index, &nsp_alignment, &location) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_TAB_ALIGN, nsp_alignment, &alignment)== FAIL)
      return RET_BUG;
  pango_tab_array_set_tab(NSP_GBOXED_GET(self, PangoTabArray), tab_index, alignment, location);
  return 0;
}

#line 784 "pango.override"
static int
_wrap_pango_tab_array_get_tab(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int , t_end} ;
  /* static char *kwlist[] = { "tab_index", NULL };*/ 
  gint tab_index, location;
  PangoTabAlign alignment;
  if (GetArgs(stack,rhs,opt,T, &tab_index) == FAIL) return RET_BUG;

  pango_tab_array_get_tab(nspg_boxed_get(self, PangoTabArray),
			  tab_index, &alignment, &location);
  if ( nsp_move_doubles(stack,1,1,2,(double)alignment,(double) location)  == FAIL) return RET_BUG;
  return 1;
}
#line 2197 "pango.c"


#line 800 "pango.override"
static int
_wrap_pango_tab_array_get_tabs(NspObject *self, Stack stack,int rhs,int opt,int lhs)
{
  PangoTabAlign *alignments;
  gint *locations, length, i;
  NspMatrix  *ret;

  length = pango_tab_array_get_size(nspg_boxed_get(self, PangoTabArray));
  pango_tab_array_get_tabs(nspg_boxed_get(self, PangoTabArray),
			   &alignments, &locations);

  if (( ret = nsp_matrix_create(NVOID,'r',length,2))== NULLMAT) return RET_BUG; 
  for (i = 0; i < ret->m; i++) {
    ret->R[i ] = alignments[i];
    ret->R[i + ret->m ]= locations[i];
  }
  MoveObj(stack,1,(NspObject *)ret);
  return 1;
}
#line 2220 "pango.c"


static int _wrap_pango_tab_array_get_positions_in_pixels(NspPangoTabArray *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_tab_array_get_positions_in_pixels(NSP_GBOXED_GET(self, PangoTabArray));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods pangotabarray_methods[] = {
  {"copy",(nsp_method *) _wrap_pango_tab_array_copy},
  {"get_size",(nsp_method *) _wrap_pango_tab_array_get_size},
  {"resize",(nsp_method *) _wrap_pango_tab_array_resize},
  {"set_tab",(nsp_method *) _wrap_pango_tab_array_set_tab},
  {"get_tab",(nsp_method *) _wrap_pango_tab_array_get_tab},
  {"get_tabs",(nsp_method *) _wrap_pango_tab_array_get_tabs},
  {"get_positions_in_pixels",(nsp_method *) _wrap_pango_tab_array_get_positions_in_pixels},
  { NULL, NULL}
};

static NspMethods *pangotabarray_get_methods(void) { return pangotabarray_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangotabarray_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoContext ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoContext_Private 
#include "nsp/gtk/pangocontext.h"
#include "nsp/interf.h"

/* NspPangoContext inherits from NspGObject */ 

int nsp_type_pangocontext_id=0;
NspTypePangoContext *nsp_type_pangocontext=NULL;

NspTypePangoContext *new_type_pangocontext(type_mode mode)
{
  NspTypePangoContext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangocontext != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangocontext;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangocontext_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangocontext_get_methods; 
  type->new = (new_func *) new_pangocontext;

  /* specific methods for pangocontext */
      
  type->init = (init_func *) init_pangocontext;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangocontext */ 

  top->s_type =  (s_type_func *) pangocontext_type_as_string;    
  top->sh_type = (sh_type_func *) pangocontext_type_short_string;
  /* top->create = (create_func*) int_pangocontext_create;*/ 
  
  /* specific methods for pangocontext */
      
  type->init = (init_func *) init_pangocontext;

  if ( nsp_type_pangocontext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoContext called nsp_type_pangocontext
       */
      type->id =  nsp_type_pangocontext_id = nsp_new_type_id();
      nsp_type_pangocontext = type;
      if ( nsp_register_type(nsp_type_pangocontext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangocontext, PANGO_TYPE_CONTEXT);
      return ( mode == T_BASE ) ? type : new_type_pangocontext(mode);
    }
  else 
    {
       type->id = nsp_type_pangocontext_id;
       return type;
    }
}

/*
 * initialize PangoContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangocontext(NspPangoContext *o,NspTypePangoContext *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoContext 
 */

NspPangoContext *new_pangocontext() 
{
  NspPangoContext *loc; 
  /* type must exists */
  nsp_type_pangocontext = new_type_pangocontext(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoContext)))== NULLPANGOCONTEXT) return loc;
  /* initialize object */
  if ( init_pangocontext(loc,nsp_type_pangocontext) == FAIL) return NULLPANGOCONTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoContext 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangocontext_type_name[]="PangoContext";
static char pangocontext_short_type_name[]="PangoContext";

static char *pangocontext_type_as_string(void)
{
  return(pangocontext_type_name);
}

static char *pangocontext_type_short_string(NspObject *v)
{
  return(pangocontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoContext objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoContext   *pangocontext_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangocontext_id) ) return ((NspPangoContext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangocontext));
  return NULL;
}

int IsPangoContextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangocontext_id);
}

int IsPangoContext(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangocontext_id);
}

NspPangoContext  *GetPangoContextCopy(Stack stack, int i)
{
  if (  GetPangoContext(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoContext  *GetPangoContext(Stack stack, int i)
{
  NspPangoContext *M;
  if (( M = pangocontext_object(NthObj(i))) == NULLPANGOCONTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspPangoContext *pangocontext_copy(NspPangoContext *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangocontext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangocontext);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoContext
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoContext *H;
  CheckRhs(0,0);
  / * want to be sure that type pangocontext is initialized * /
  nsp_type_pangocontext = new_type_pangocontext(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_pangocontext)) == NULLPANGOCONTEXT) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 406 "pango.override"
static  NspObject *
_wrap_pango_context_list_families(NspGObject *self)
{
  PangoFontFamily **families;
  gint n_families, i;
  NspObject *ret;

  pango_context_list_families(PANGO_CONTEXT(self->obj), &families, &n_families);
  ret = NspTuple_New(n_families);
  for (i = 0; i < n_families; i++) {
    NspObject *family;

    family = (NspObject *)nspgobject_new((GObject *)families[i]);
    NspTuple_SetItem(ret, i, family);
  }
  g_free(families);
  return ret;
}
#line 2463 "pango.c"


static int _wrap_pango_context_load_font(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoFont *ret;
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc, *nsp_ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_desc) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc, PANGO_TYPE_FONT_DESCRIPTION))
      desc = nspg_boxed_get(nsp_desc, PangoFontDescription);
  else {
      Scierror( "desc should be a PangoFontDescription");
      return RET_BUG;
  }
  ret = pango_context_load_font(PANGO_CONTEXT(self->obj), desc);
  nsp_type_pangofont = new_type_pangofont(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_pangofont))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_context_load_fontset(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, obj,t_end};
  PangoFontset *ret;
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc, *nsp_language, *nsp_ret;
  PangoLanguage *language = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_desc, &nsp_language) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc, PANGO_TYPE_FONT_DESCRIPTION))
      desc = nspg_boxed_get(nsp_desc, PangoFontDescription);
  else {
      Scierror( "desc should be a PangoFontDescription");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_language, PANGO_TYPE_LANGUAGE))
      language = nspg_boxed_get(nsp_language, PangoLanguage);
  else {
      Scierror( "language should be a PangoLanguage");
      return RET_BUG;
  }
  ret = pango_context_load_fontset(PANGO_CONTEXT(self->obj), desc, language);
  nsp_type_pangofontset = new_type_pangofontset(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_pangofontset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_context_get_metrics(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, obj,t_end};
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc, *nsp_language, *nsp_ret;
  PangoLanguage *language = NULL;
  PangoFontMetrics *ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_desc, &nsp_language) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc, PANGO_TYPE_FONT_DESCRIPTION))
      desc = nspg_boxed_get(nsp_desc, PangoFontDescription);
  else {
      Scierror( "desc should be a PangoFontDescription");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_language, PANGO_TYPE_LANGUAGE))
      language = nspg_boxed_get(nsp_language, PangoLanguage);
  else {
      Scierror( "language should be a PangoLanguage");
      return RET_BUG;
  }
  ret = pango_context_get_metrics(PANGO_CONTEXT(self->obj), desc, language);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_FONT_METRICS, ret, FALSE, TRUE,
                                             (NspTypeBase *) nsp_type_pangofontmetrics))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_context_set_font_description(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc;

  if ( GetArgs(stack,rhs,opt,T,&nsp_desc) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc, PANGO_TYPE_FONT_DESCRIPTION))
      desc = nspg_boxed_get(nsp_desc, PangoFontDescription);
  else {
      Scierror( "desc should be a PangoFontDescription");
      return RET_BUG;
  }
  pango_context_set_font_description(PANGO_CONTEXT(self->obj), desc);
  return 0;
}

static int _wrap_pango_context_get_font_description(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoFontDescription *ret;
  NspObject *nsp_ret;

  ret = pango_context_get_font_description(PANGO_CONTEXT(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_FONT_DESCRIPTION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangofontdescription))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_context_get_language(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  PangoLanguage *ret;

  ret = pango_context_get_language(PANGO_CONTEXT(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_LANGUAGE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangolanguage))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_context_set_language(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_language;
  PangoLanguage *language = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_language) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_language, PANGO_TYPE_LANGUAGE))
      language = nspg_boxed_get(nsp_language, PangoLanguage);
  else {
      Scierror( "language should be a PangoLanguage");
      return RET_BUG;
  }
  pango_context_set_language(PANGO_CONTEXT(self->obj), language);
  return 0;
}

static int _wrap_pango_context_set_base_dir(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoDirection direction;
  NspObject *nsp_direction = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_direction) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_DIRECTION, nsp_direction, &direction)== FAIL)
      return RET_BUG;
  pango_context_set_base_dir(PANGO_CONTEXT(self->obj), direction);
  return 0;
}

static int _wrap_pango_context_get_base_dir(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = pango_context_get_base_dir(PANGO_CONTEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods pangocontext_methods[] = {
  {"list_families",(nsp_method *) _wrap_pango_context_list_families},
  {"load_font",(nsp_method *) _wrap_pango_context_load_font},
  {"load_fontset",(nsp_method *) _wrap_pango_context_load_fontset},
  {"get_metrics",(nsp_method *) _wrap_pango_context_get_metrics},
  {"set_font_description",(nsp_method *) _wrap_pango_context_set_font_description},
  {"get_font_description",(nsp_method *) _wrap_pango_context_get_font_description},
  {"get_language",(nsp_method *) _wrap_pango_context_get_language},
  {"set_language",(nsp_method *) _wrap_pango_context_set_language},
  {"set_base_dir",(nsp_method *) _wrap_pango_context_set_base_dir},
  {"get_base_dir",(nsp_method *) _wrap_pango_context_get_base_dir},
  { NULL, NULL}
};

static NspMethods *pangocontext_get_methods(void) { return pangocontext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangocontext_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoFont ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoFont_Private 
#include "nsp/gtk/pangofont.h"
#include "nsp/interf.h"

/* NspPangoFont inherits from NspGObject */ 

int nsp_type_pangofont_id=0;
NspTypePangoFont *nsp_type_pangofont=NULL;

NspTypePangoFont *new_type_pangofont(type_mode mode)
{
  NspTypePangoFont *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangofont != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangofont;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangofont_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangofont_get_methods; 
  type->new = (new_func *) new_pangofont;

  /* specific methods for pangofont */
      
  type->init = (init_func *) init_pangofont;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofont */ 

  top->s_type =  (s_type_func *) pangofont_type_as_string;    
  top->sh_type = (sh_type_func *) pangofont_type_short_string;
  /* top->create = (create_func*) int_pangofont_create;*/ 
  
  /* specific methods for pangofont */
      
  type->init = (init_func *) init_pangofont;

  if ( nsp_type_pangofont_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoFont called nsp_type_pangofont
       */
      type->id =  nsp_type_pangofont_id = nsp_new_type_id();
      nsp_type_pangofont = type;
      if ( nsp_register_type(nsp_type_pangofont) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangofont, PANGO_TYPE_FONT);
      return ( mode == T_BASE ) ? type : new_type_pangofont(mode);
    }
  else 
    {
       type->id = nsp_type_pangofont_id;
       return type;
    }
}

/*
 * initialize PangoFont instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofont(NspPangoFont *o,NspTypePangoFont *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoFont 
 */

NspPangoFont *new_pangofont() 
{
  NspPangoFont *loc; 
  /* type must exists */
  nsp_type_pangofont = new_type_pangofont(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoFont)))== NULLPANGOFONT) return loc;
  /* initialize object */
  if ( init_pangofont(loc,nsp_type_pangofont) == FAIL) return NULLPANGOFONT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoFont 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofont_type_name[]="PangoFont";
static char pangofont_short_type_name[]="PangoFont";

static char *pangofont_type_as_string(void)
{
  return(pangofont_type_name);
}

static char *pangofont_type_short_string(NspObject *v)
{
  return(pangofont_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoFont objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoFont   *pangofont_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangofont_id) ) return ((NspPangoFont *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangofont));
  return NULL;
}

int IsPangoFontObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangofont_id);
}

int IsPangoFont(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangofont_id);
}

NspPangoFont  *GetPangoFontCopy(Stack stack, int i)
{
  if (  GetPangoFont(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoFont  *GetPangoFont(Stack stack, int i)
{
  NspPangoFont *M;
  if (( M = pangofont_object(NthObj(i))) == NULLPANGOFONT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspPangoFont *pangofont_copy(NspPangoFont *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofont);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofont);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoFont
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoFont *H;
  CheckRhs(0,0);
  / * want to be sure that type pangofont is initialized * /
  nsp_type_pangofont = new_type_pangofont(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_pangofont)) == NULLPANGOFONT) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_pango_font_describe(NspPangoFont *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoFontDescription *ret;
  NspObject *nsp_ret;

  ret = pango_font_describe(PANGO_FONT(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_FONT_DESCRIPTION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangofontdescription))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_font_get_metrics(NspPangoFont *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_language, *nsp_ret;
  PangoLanguage *language = NULL;
  PangoFontMetrics *ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_language) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_language, PANGO_TYPE_LANGUAGE))
      language = nspg_boxed_get(nsp_language, PangoLanguage);
  else {
      Scierror( "language should be a PangoLanguage");
      return RET_BUG;
  }
  ret = pango_font_get_metrics(PANGO_FONT(self->obj), language);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_FONT_METRICS, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangofontmetrics))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 426 "pango.override"
static int
_wrap_pango_font_get_glyph_extents(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int , t_end} ;
  gint glyph;
  PangoRectangle ink_rect, logical_rect;

  if (GetArgs(stack,rhs,opt,T, &glyph) == FAIL) return RET_BUG; 

  pango_font_get_glyph_extents(PANGO_FONT(self->obj), (PangoGlyph)glyph,
			       &ink_rect, &logical_rect);
  
  if ( nsp_move_doubles(stack,1,1,4,(double)ink_rect.x,(double) ink_rect.y,
			(double)ink_rect.width,(double) ink_rect.height) == FAIL) return RET_BUG ; 
  if ( nsp_move_doubles(stack,2,1,4,(double)logical_rect.x, (double)logical_rect.y,
			(double)logical_rect.width,(double) logical_rect.height) == FAIL) return RET_BUG ; 
  return 2; 
}
#line 2895 "pango.c"


static NspMethods pangofont_methods[] = {
  {"describe",(nsp_method *) _wrap_pango_font_describe},
  {"get_metrics",(nsp_method *) _wrap_pango_font_get_metrics},
  {"get_glyph_extents",(nsp_method *) _wrap_pango_font_get_glyph_extents},
  { NULL, NULL}
};

static NspMethods *pangofont_get_methods(void) { return pangofont_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangofont_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoFontFace ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoFontFace_Private 
#include "nsp/gtk/pangofontface.h"
#include "nsp/interf.h"

/* NspPangoFontFace inherits from NspGObject */ 

int nsp_type_pangofontface_id=0;
NspTypePangoFontFace *nsp_type_pangofontface=NULL;

NspTypePangoFontFace *new_type_pangofontface(type_mode mode)
{
  NspTypePangoFontFace *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangofontface != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangofontface;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangofontface_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangofontface_get_methods; 
  type->new = (new_func *) new_pangofontface;

  /* specific methods for pangofontface */
      
  type->init = (init_func *) init_pangofontface;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontface */ 

  top->s_type =  (s_type_func *) pangofontface_type_as_string;    
  top->sh_type = (sh_type_func *) pangofontface_type_short_string;
  /* top->create = (create_func*) int_pangofontface_create;*/ 
  
  /* specific methods for pangofontface */
      
  type->init = (init_func *) init_pangofontface;

  if ( nsp_type_pangofontface_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoFontFace called nsp_type_pangofontface
       */
      type->id =  nsp_type_pangofontface_id = nsp_new_type_id();
      nsp_type_pangofontface = type;
      if ( nsp_register_type(nsp_type_pangofontface) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangofontface, PANGO_TYPE_FONT_FACE);
      return ( mode == T_BASE ) ? type : new_type_pangofontface(mode);
    }
  else 
    {
       type->id = nsp_type_pangofontface_id;
       return type;
    }
}

/*
 * initialize PangoFontFace instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontface(NspPangoFontFace *o,NspTypePangoFontFace *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoFontFace 
 */

NspPangoFontFace *new_pangofontface() 
{
  NspPangoFontFace *loc; 
  /* type must exists */
  nsp_type_pangofontface = new_type_pangofontface(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoFontFace)))== NULLPANGOFONTFACE) return loc;
  /* initialize object */
  if ( init_pangofontface(loc,nsp_type_pangofontface) == FAIL) return NULLPANGOFONTFACE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoFontFace 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontface_type_name[]="PangoFontFace";
static char pangofontface_short_type_name[]="PangoFontFace";

static char *pangofontface_type_as_string(void)
{
  return(pangofontface_type_name);
}

static char *pangofontface_type_short_string(NspObject *v)
{
  return(pangofontface_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoFontFace objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoFontFace   *pangofontface_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangofontface_id) ) return ((NspPangoFontFace *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangofontface));
  return NULL;
}

int IsPangoFontFaceObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangofontface_id);
}

int IsPangoFontFace(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangofontface_id);
}

NspPangoFontFace  *GetPangoFontFaceCopy(Stack stack, int i)
{
  if (  GetPangoFontFace(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoFontFace  *GetPangoFontFace(Stack stack, int i)
{
  NspPangoFontFace *M;
  if (( M = pangofontface_object(NthObj(i))) == NULLPANGOFONTFACE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspPangoFontFace *pangofontface_copy(NspPangoFontFace *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofontface);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofontface);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoFontFace
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoFontFace *H;
  CheckRhs(0,0);
  / * want to be sure that type pangofontface is initialized * /
  nsp_type_pangofontface = new_type_pangofontface(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_pangofontface)) == NULLPANGOFONTFACE) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_pango_font_face_describe(NspPangoFontFace *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoFontDescription *ret;
  NspObject *nsp_ret;

  ret = pango_font_face_describe(PANGO_FONT_FACE(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_FONT_DESCRIPTION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangofontdescription))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_font_face_get_face_name(NspPangoFontFace *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;

  ret = pango_font_face_get_face_name(PANGO_FONT_FACE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods pangofontface_methods[] = {
  {"describe",(nsp_method *) _wrap_pango_font_face_describe},
  {"get_face_name",(nsp_method *) _wrap_pango_font_face_get_face_name},
  { NULL, NULL}
};

static NspMethods *pangofontface_get_methods(void) { return pangofontface_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangofontface_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoFontFamily ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoFontFamily_Private 
#include "nsp/gtk/pangofontfamily.h"
#include "nsp/interf.h"

/* NspPangoFontFamily inherits from NspGObject */ 

int nsp_type_pangofontfamily_id=0;
NspTypePangoFontFamily *nsp_type_pangofontfamily=NULL;

NspTypePangoFontFamily *new_type_pangofontfamily(type_mode mode)
{
  NspTypePangoFontFamily *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangofontfamily != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangofontfamily;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangofontfamily_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangofontfamily_get_methods; 
  type->new = (new_func *) new_pangofontfamily;

  /* specific methods for pangofontfamily */
      
  type->init = (init_func *) init_pangofontfamily;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontfamily */ 

  top->s_type =  (s_type_func *) pangofontfamily_type_as_string;    
  top->sh_type = (sh_type_func *) pangofontfamily_type_short_string;
  /* top->create = (create_func*) int_pangofontfamily_create;*/ 
  
  /* specific methods for pangofontfamily */
      
  type->init = (init_func *) init_pangofontfamily;

  if ( nsp_type_pangofontfamily_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoFontFamily called nsp_type_pangofontfamily
       */
      type->id =  nsp_type_pangofontfamily_id = nsp_new_type_id();
      nsp_type_pangofontfamily = type;
      if ( nsp_register_type(nsp_type_pangofontfamily) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangofontfamily, PANGO_TYPE_FONT_FAMILY);
      return ( mode == T_BASE ) ? type : new_type_pangofontfamily(mode);
    }
  else 
    {
       type->id = nsp_type_pangofontfamily_id;
       return type;
    }
}

/*
 * initialize PangoFontFamily instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontfamily(NspPangoFontFamily *o,NspTypePangoFontFamily *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoFontFamily 
 */

NspPangoFontFamily *new_pangofontfamily() 
{
  NspPangoFontFamily *loc; 
  /* type must exists */
  nsp_type_pangofontfamily = new_type_pangofontfamily(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoFontFamily)))== NULLPANGOFONTFAMILY) return loc;
  /* initialize object */
  if ( init_pangofontfamily(loc,nsp_type_pangofontfamily) == FAIL) return NULLPANGOFONTFAMILY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoFontFamily 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontfamily_type_name[]="PangoFontFamily";
static char pangofontfamily_short_type_name[]="PangoFontFamily";

static char *pangofontfamily_type_as_string(void)
{
  return(pangofontfamily_type_name);
}

static char *pangofontfamily_type_short_string(NspObject *v)
{
  return(pangofontfamily_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoFontFamily objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoFontFamily   *pangofontfamily_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangofontfamily_id) ) return ((NspPangoFontFamily *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangofontfamily));
  return NULL;
}

int IsPangoFontFamilyObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangofontfamily_id);
}

int IsPangoFontFamily(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangofontfamily_id);
}

NspPangoFontFamily  *GetPangoFontFamilyCopy(Stack stack, int i)
{
  if (  GetPangoFontFamily(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoFontFamily  *GetPangoFontFamily(Stack stack, int i)
{
  NspPangoFontFamily *M;
  if (( M = pangofontfamily_object(NthObj(i))) == NULLPANGOFONTFAMILY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspPangoFontFamily *pangofontfamily_copy(NspPangoFontFamily *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofontfamily);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofontfamily);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoFontFamily
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoFontFamily *H;
  CheckRhs(0,0);
  / * want to be sure that type pangofontfamily is initialized * /
  nsp_type_pangofontfamily = new_type_pangofontfamily(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_pangofontfamily)) == NULLPANGOFONTFAMILY) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

#line 446 "pango.override"
static  NspObject *
_wrap_pango_font_family_list_faces(NspGObject *self)
{
  PangoFontFace **faces;
  gint n_faces, i;
  NspObject *ret;

  pango_font_family_list_faces(PANGO_FONT_FAMILY(self->obj),
			       &faces, &n_faces);
  
  ret = NspTuple_New(n_faces);
  for (i = 0; i < n_faces; i++) {
    NspObject *face = (NspObject *)nspgobject_new((GObject *)faces[i]);
    NspTuple_SetItem(ret, i, face);
  }
  g_free(faces);
  return ret;
}
#line 3354 "pango.c"


static int _wrap_pango_font_family_get_name(NspPangoFontFamily *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;

  ret = pango_font_family_get_name(PANGO_FONT_FAMILY(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods pangofontfamily_methods[] = {
  {"list_faces",(nsp_method *) _wrap_pango_font_family_list_faces},
  {"get_name",(nsp_method *) _wrap_pango_font_family_get_name},
  { NULL, NULL}
};

static NspMethods *pangofontfamily_get_methods(void) { return pangofontfamily_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangofontfamily_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoFontMap ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoFontMap_Private 
#include "nsp/gtk/pangofontmap.h"
#include "nsp/interf.h"

/* NspPangoFontMap inherits from NspGObject */ 

int nsp_type_pangofontmap_id=0;
NspTypePangoFontMap *nsp_type_pangofontmap=NULL;

NspTypePangoFontMap *new_type_pangofontmap(type_mode mode)
{
  NspTypePangoFontMap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangofontmap != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangofontmap;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangofontmap_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangofontmap_get_methods; 
  type->new = (new_func *) new_pangofontmap;

  /* specific methods for pangofontmap */
      
  type->init = (init_func *) init_pangofontmap;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontmap */ 

  top->s_type =  (s_type_func *) pangofontmap_type_as_string;    
  top->sh_type = (sh_type_func *) pangofontmap_type_short_string;
  /* top->create = (create_func*) int_pangofontmap_create;*/ 
  
  /* specific methods for pangofontmap */
      
  type->init = (init_func *) init_pangofontmap;

  if ( nsp_type_pangofontmap_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoFontMap called nsp_type_pangofontmap
       */
      type->id =  nsp_type_pangofontmap_id = nsp_new_type_id();
      nsp_type_pangofontmap = type;
      if ( nsp_register_type(nsp_type_pangofontmap) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangofontmap, PANGO_TYPE_FONT_MAP);
      return ( mode == T_BASE ) ? type : new_type_pangofontmap(mode);
    }
  else 
    {
       type->id = nsp_type_pangofontmap_id;
       return type;
    }
}

/*
 * initialize PangoFontMap instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontmap(NspPangoFontMap *o,NspTypePangoFontMap *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoFontMap 
 */

NspPangoFontMap *new_pangofontmap() 
{
  NspPangoFontMap *loc; 
  /* type must exists */
  nsp_type_pangofontmap = new_type_pangofontmap(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoFontMap)))== NULLPANGOFONTMAP) return loc;
  /* initialize object */
  if ( init_pangofontmap(loc,nsp_type_pangofontmap) == FAIL) return NULLPANGOFONTMAP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoFontMap 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontmap_type_name[]="PangoFontMap";
static char pangofontmap_short_type_name[]="PangoFontMap";

static char *pangofontmap_type_as_string(void)
{
  return(pangofontmap_type_name);
}

static char *pangofontmap_type_short_string(NspObject *v)
{
  return(pangofontmap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoFontMap objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoFontMap   *pangofontmap_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangofontmap_id) ) return ((NspPangoFontMap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangofontmap));
  return NULL;
}

int IsPangoFontMapObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangofontmap_id);
}

int IsPangoFontMap(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangofontmap_id);
}

NspPangoFontMap  *GetPangoFontMapCopy(Stack stack, int i)
{
  if (  GetPangoFontMap(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoFontMap  *GetPangoFontMap(Stack stack, int i)
{
  NspPangoFontMap *M;
  if (( M = pangofontmap_object(NthObj(i))) == NULLPANGOFONTMAP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspPangoFontMap *pangofontmap_copy(NspPangoFontMap *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofontmap);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofontmap);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoFontMap
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoFontMap *H;
  CheckRhs(0,0);
  / * want to be sure that type pangofontmap is initialized * /
  nsp_type_pangofontmap = new_type_pangofontmap(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_pangofontmap)) == NULLPANGOFONTMAP) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_pango_font_map_load_font(NspPangoFontMap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj,t_end};
  NspGObject *context;
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc, *nsp_ret;
  PangoFont *ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_pangocontext, &context, &nsp_desc) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc, PANGO_TYPE_FONT_DESCRIPTION))
      desc = nspg_boxed_get(nsp_desc, PangoFontDescription);
  else {
      Scierror( "desc should be a PangoFontDescription");
      return RET_BUG;
  }
  ret = pango_font_map_load_font(PANGO_FONT_MAP(self->obj), PANGO_CONTEXT(context->obj), desc);
  nsp_type_pangofont = new_type_pangofont(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_pangofont))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_font_map_load_fontset(NspPangoFontMap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj, obj,t_end};
  NspGObject *context;
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc, *nsp_language, *nsp_ret;
  PangoLanguage *language = NULL;
  PangoFontset *ret;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_pangocontext, &context, &nsp_desc, &nsp_language) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc, PANGO_TYPE_FONT_DESCRIPTION))
      desc = nspg_boxed_get(nsp_desc, PangoFontDescription);
  else {
      Scierror( "desc should be a PangoFontDescription");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_language, PANGO_TYPE_LANGUAGE))
      language = nspg_boxed_get(nsp_language, PangoLanguage);
  else {
      Scierror( "language should be a PangoLanguage");
      return RET_BUG;
  }
  ret = pango_font_map_load_fontset(PANGO_FONT_MAP(self->obj), PANGO_CONTEXT(context->obj), desc, language);
  nsp_type_pangofontset = new_type_pangofontset(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_pangofontset))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 466 "pango.override"
static NspObject *
_wrap_pango_font_map_list_families(NspGObject *self)
{
  PangoFontFamily **families;
  gint n_families, i;
  NspObject *ret;

  pango_font_map_list_families(PANGO_FONT_MAP(self->obj), &families,
			       &n_families);
  ret = NspTuple_New(n_families);
  for (i = 0; i < n_families; i++) {
    NspObject *family= (NspObject *) nspgobject_new((GObject *)families[i]);
    NspTuple_SetItem(ret, i, family);
  }
  g_free(families);
  return ret;
}
#line 3642 "pango.c"


static NspMethods pangofontmap_methods[] = {
  {"load_font",(nsp_method *) _wrap_pango_font_map_load_font},
  {"load_fontset",(nsp_method *) _wrap_pango_font_map_load_fontset},
  {"list_families",(nsp_method *) _wrap_pango_font_map_list_families},
  { NULL, NULL}
};

static NspMethods *pangofontmap_get_methods(void) { return pangofontmap_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangofontmap_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoFontset ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoFontset_Private 
#include "nsp/gtk/pangofontset.h"
#include "nsp/interf.h"

/* NspPangoFontset inherits from NspGObject */ 

int nsp_type_pangofontset_id=0;
NspTypePangoFontset *nsp_type_pangofontset=NULL;

NspTypePangoFontset *new_type_pangofontset(type_mode mode)
{
  NspTypePangoFontset *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangofontset != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangofontset;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangofontset_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangofontset_get_methods; 
  type->new = (new_func *) new_pangofontset;

  /* specific methods for pangofontset */
      
  type->init = (init_func *) init_pangofontset;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontset */ 

  top->s_type =  (s_type_func *) pangofontset_type_as_string;    
  top->sh_type = (sh_type_func *) pangofontset_type_short_string;
  /* top->create = (create_func*) int_pangofontset_create;*/ 
  
  /* specific methods for pangofontset */
      
  type->init = (init_func *) init_pangofontset;

  if ( nsp_type_pangofontset_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoFontset called nsp_type_pangofontset
       */
      type->id =  nsp_type_pangofontset_id = nsp_new_type_id();
      nsp_type_pangofontset = type;
      if ( nsp_register_type(nsp_type_pangofontset) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangofontset, PANGO_TYPE_FONTSET);
      return ( mode == T_BASE ) ? type : new_type_pangofontset(mode);
    }
  else 
    {
       type->id = nsp_type_pangofontset_id;
       return type;
    }
}

/*
 * initialize PangoFontset instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontset(NspPangoFontset *o,NspTypePangoFontset *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoFontset 
 */

NspPangoFontset *new_pangofontset() 
{
  NspPangoFontset *loc; 
  /* type must exists */
  nsp_type_pangofontset = new_type_pangofontset(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoFontset)))== NULLPANGOFONTSET) return loc;
  /* initialize object */
  if ( init_pangofontset(loc,nsp_type_pangofontset) == FAIL) return NULLPANGOFONTSET;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoFontset 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontset_type_name[]="PangoFontset";
static char pangofontset_short_type_name[]="PangoFontset";

static char *pangofontset_type_as_string(void)
{
  return(pangofontset_type_name);
}

static char *pangofontset_type_short_string(NspObject *v)
{
  return(pangofontset_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoFontset objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoFontset   *pangofontset_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangofontset_id) ) return ((NspPangoFontset *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangofontset));
  return NULL;
}

int IsPangoFontsetObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangofontset_id);
}

int IsPangoFontset(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangofontset_id);
}

NspPangoFontset  *GetPangoFontsetCopy(Stack stack, int i)
{
  if (  GetPangoFontset(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoFontset  *GetPangoFontset(Stack stack, int i)
{
  NspPangoFontset *M;
  if (( M = pangofontset_object(NthObj(i))) == NULLPANGOFONTSET)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspPangoFontset *pangofontset_copy(NspPangoFontset *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofontset);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangofontset);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoFontset
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoFontset *H;
  CheckRhs(0,0);
  / * want to be sure that type pangofontset is initialized * /
  nsp_type_pangofontset = new_type_pangofontset(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_pangofontset)) == NULLPANGOFONTSET) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int _wrap_pango_fontset_get_font(NspPangoFontset *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int wc;
  PangoFont *ret;
  NspObject *nsp_ret;

  if ( GetArgs(stack,rhs,opt,T,&wc) == FAIL) return RET_BUG;
  ret = pango_fontset_get_font(PANGO_FONTSET(self->obj), wc);
  nsp_type_pangofont = new_type_pangofont(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_pangofont))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_fontset_get_metrics(NspPangoFontset *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  PangoFontMetrics *ret;

  ret = pango_fontset_get_metrics(PANGO_FONTSET(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_FONT_METRICS, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangofontmetrics))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods pangofontset_methods[] = {
  {"get_font",(nsp_method *) _wrap_pango_fontset_get_font},
  {"get_metrics",(nsp_method *) _wrap_pango_fontset_get_metrics},
  { NULL, NULL}
};

static NspMethods *pangofontset_get_methods(void) { return pangofontset_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangofontset_attrs[]={{NULL,NULL,NULL}} ;


/* ----------- PangoLayout ----------- */

/* -*- Mode: C -*- */
/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998-2003 )                          
 * Jean-Philippe Chancelier Enpc/Cermics 
 *-------------------------------------------------------------------*/

#include "nsp/object.h"
#define  PangoLayout_Private 
#include "nsp/gtk/pangolayout.h"
#include "nsp/interf.h"

/* NspPangoLayout inherits from NspGObject */ 

int nsp_type_pangolayout_id=0;
NspTypePangoLayout *nsp_type_pangolayout=NULL;

NspTypePangoLayout *new_type_pangolayout(type_mode mode)
{
  NspTypePangoLayout *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_pangolayout != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pangolayout;
    }
  if ((type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = pangolayout_attrs ; 
  type->get_attrs = (attrs_func *)  int_get_attribute;
  type->set_attrs = (attrs_func *)  int_set_attribute;
  type->methods = pangolayout_get_methods; 
  type->new = (new_func *) new_pangolayout;

  /* specific methods for pangolayout */
      
  type->init = (init_func *) init_pangolayout;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangolayout */ 

  top->s_type =  (s_type_func *) pangolayout_type_as_string;    
  top->sh_type = (sh_type_func *) pangolayout_type_short_string;
  /* top->create = (create_func*) int_pangolayout_create;*/ 
  
  /* specific methods for pangolayout */
      
  type->init = (init_func *) init_pangolayout;

  if ( nsp_type_pangolayout_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypePangoLayout called nsp_type_pangolayout
       */
      type->id =  nsp_type_pangolayout_id = nsp_new_type_id();
      nsp_type_pangolayout = type;
      if ( nsp_register_type(nsp_type_pangolayout) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_pangolayout, PANGO_TYPE_LAYOUT);
      return ( mode == T_BASE ) ? type : new_type_pangolayout(mode);
    }
  else 
    {
       type->id = nsp_type_pangolayout_id;
       return type;
    }
}

/*
 * initialize PangoLayout instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangolayout(NspPangoLayout *o,NspTypePangoLayout *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of PangoLayout 
 */

NspPangoLayout *new_pangolayout() 
{
  NspPangoLayout *loc; 
  /* type must exists */
  nsp_type_pangolayout = new_type_pangolayout(T_BASE);
  if ( (loc = malloc(sizeof(NspPangoLayout)))== NULLPANGOLAYOUT) return loc;
  /* initialize object */
  if ( init_pangolayout(loc,nsp_type_pangolayout) == FAIL) return NULLPANGOLAYOUT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for PangoLayout 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangolayout_type_name[]="PangoLayout";
static char pangolayout_short_type_name[]="PangoLayout";

static char *pangolayout_type_as_string(void)
{
  return(pangolayout_type_name);
}

static char *pangolayout_type_short_string(NspObject *v)
{
  return(pangolayout_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for PangoLayout objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspPangoLayout   *pangolayout_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast (O,nsp_type_pangolayout_id) ) return ((NspPangoLayout *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_pangolayout));
  return NULL;
}

int IsPangoLayoutObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pangolayout_id);
}

int IsPangoLayout(NspObject *O)
{
  return nsp_object_type(O,nsp_type_pangolayout_id);
}

NspPangoLayout  *GetPangoLayoutCopy(Stack stack, int i)
{
  if (  GetPangoLayout(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspPangoLayout  *GetPangoLayout(Stack stack, int i)
{
  NspPangoLayout *M;
  if (( M = pangolayout_object(NthObj(i))) == NULLPANGOLAYOUT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspPangoLayout *pangolayout_copy(NspPangoLayout *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangolayout);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_pangolayout);
}

/*-------------------------------------------------------------------
 * wrappers for the PangoLayout
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* int int_clc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspPangoLayout *H;
  CheckRhs(0,0);
  / * want to be sure that type pangolayout is initialized * /
  nsp_type_pangolayout = new_type_pangolayout(T_BASE);
  if(( H = gobject_create(NVOID,(NspTypeBase *) nsp_type_pangolayout)) == NULLPANGOLAYOUT) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 
*/ 

static int
_wrap_pangolayout_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *context;

  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_pangocontext, &context) == FAIL) return RET_BUG;
  if ((ret = (GObject *)pango_layout_new(PANGO_CONTEXT(context->obj)))== NULL) return RET_BUG;

  nsp_type_pangolayout = new_type_pangolayout(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_pangolayout );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_layout_copy(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  PangoLayout *ret;

  ret = pango_layout_copy(PANGO_LAYOUT(self->obj));
  nsp_type_pangolayout = new_type_pangolayout(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_pangolayout))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_layout_get_context(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoContext *ret;
  NspObject *nsp_ret;

  ret = pango_layout_get_context(PANGO_LAYOUT(self->obj));
  nsp_type_pangocontext = new_type_pangocontext(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_pangocontext))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_layout_set_attributes(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_attrs;
  PangoAttrList *attrs = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_attrs) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_attrs, PANGO_TYPE_ATTR_LIST))
      attrs = nspg_boxed_get(nsp_attrs, PangoAttrList);
  else {
      Scierror( "attrs should be a PangoAttrList");
      return RET_BUG;
  }
  pango_layout_set_attributes(PANGO_LAYOUT(self->obj), attrs);
  return 0;
}

static int _wrap_pango_layout_get_attributes(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  PangoAttrList *ret;

  ret = pango_layout_get_attributes(PANGO_LAYOUT(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_ATTR_LIST, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangoattrlist))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 821 "pango.override"
static int
_wrap_pango_layout_set_text(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { string , t_end} ;
  /* static char *kwlist[] = { "text", NULL };*/ 
  char *text;
  gint length;
  if (GetArgs(stack,rhs,opt,T,  &text, &length) == FAIL) return RET_BUG;
  pango_layout_set_text(PANGO_LAYOUT(self->obj), text, length);
  return 0;
}
#line 4171 "pango.c"


static int _wrap_pango_layout_get_text(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;

  ret = pango_layout_get_text(PANGO_LAYOUT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#line 563 "pango.override"
static int
_wrap_pango_layout_set_markup(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { string , t_end} ;
  char *markup;
  gint length;
  if (GetArgs(stack,rhs,opt,T,&markup) == FAIL) return RET_BUG;
  length = strlen(markup);
  pango_layout_set_markup(PANGO_LAYOUT(self->obj), markup, length);
  return 0;
}
#line 4195 "pango.c"


#line 576 "pango.override"
static int
_wrap_pango_layout_set_markup_with_accel(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* 
  static char *kwlist[] = { "markup", "accel_marker", NULL };
  char *markup;
  gint length, accel_length;
  Nsp_UNICODE *accel_marker, pychr;
  gunichar accel_char;

  if (GetArgs(stack,rhs,opt,T,
	      "s#u#:PangoLayout.set_markup_with_accel",
	      kwlist, &markup, &length,
	      &accel_marker, &accel_length))
    return RET_BUG;
  if (accel_length != 1) {
    Scierror( "accel_marker must be a unicode string of length 1");
    return RET_BUG;
  }
  pango_layout_set_markup_with_accel(PANGO_LAYOUT(self->obj), markup, length,
				     (gunichar)accel_marker[0], &accel_char);

#if !defined(Nsp_UNICODE_SIZE) || Nsp_UNICODE_SIZE == 2
  if (accel_char >= 0xffff) {
    NspErr_SetString(NspExc_ValueError, "unicode character is too big to fit in a 16-bit unicode character");
    return RET_BUG;
  }
#endif
  pychr = (Nsp_UNICODE)accel_char;
  return NspUnicode_FromUnicode(&pychr, 1);
  */
  Scierror("%s: To be done \n",NspFname(stack)); 
  return RET_BUG; 
}
#line 4233 "pango.c"


static int _wrap_pango_layout_set_font_description(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc;

  if ( GetArgs(stack,rhs,opt,T,&nsp_desc) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_desc, PANGO_TYPE_FONT_DESCRIPTION))
      desc = nspg_boxed_get(nsp_desc, PangoFontDescription);
  else {
      Scierror( "desc should be a PangoFontDescription");
      return RET_BUG;
  }
  pango_layout_set_font_description(PANGO_LAYOUT(self->obj), desc);
  return 0;
}

static int _wrap_pango_layout_set_width(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int width;

  if ( GetArgs(stack,rhs,opt,T,&width) == FAIL) return RET_BUG;
  pango_layout_set_width(PANGO_LAYOUT(self->obj), width);
  return 0;
}

static int _wrap_pango_layout_get_width(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_layout_get_width(PANGO_LAYOUT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_set_wrap(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoWrapMode wrap;
  NspObject *nsp_wrap = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_wrap) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_WRAP_MODE, nsp_wrap, &wrap)== FAIL)
      return RET_BUG;
  pango_layout_set_wrap(PANGO_LAYOUT(self->obj), wrap);
  return 0;
}

static int _wrap_pango_layout_get_wrap(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = pango_layout_get_wrap(PANGO_LAYOUT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_set_indent(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int indent;

  if ( GetArgs(stack,rhs,opt,T,&indent) == FAIL) return RET_BUG;
  pango_layout_set_indent(PANGO_LAYOUT(self->obj), indent);
  return 0;
}

static int _wrap_pango_layout_get_indent(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_layout_get_indent(PANGO_LAYOUT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_set_spacing(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int spacing;

  if ( GetArgs(stack,rhs,opt,T,&spacing) == FAIL) return RET_BUG;
  pango_layout_set_spacing(PANGO_LAYOUT(self->obj), spacing);
  return 0;
}

static int _wrap_pango_layout_get_spacing(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_layout_get_spacing(PANGO_LAYOUT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_set_justify(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int justify;

  if ( GetArgs(stack,rhs,opt,T,&justify) == FAIL) return RET_BUG;
  pango_layout_set_justify(PANGO_LAYOUT(self->obj), justify);
  return 0;
}

static int _wrap_pango_layout_get_justify(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_layout_get_justify(PANGO_LAYOUT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_set_alignment(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_alignment = NULL;
  PangoAlignment alignment;

  if ( GetArgs(stack,rhs,opt,T,&nsp_alignment) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_ALIGNMENT, nsp_alignment, &alignment)== FAIL)
      return RET_BUG;
  pango_layout_set_alignment(PANGO_LAYOUT(self->obj), alignment);
  return 0;
}

static int _wrap_pango_layout_get_alignment(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = pango_layout_get_alignment(PANGO_LAYOUT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_set_tabs(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_tabs;
  PangoTabArray *tabs = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_tabs) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_tabs, PANGO_TYPE_TAB_ARRAY))
      tabs = nspg_boxed_get(nsp_tabs, PangoTabArray);
  else {
      Scierror( "tabs should be a PangoTabArray");
      return RET_BUG;
  }
  pango_layout_set_tabs(PANGO_LAYOUT(self->obj), tabs);
  return 0;
}

static int _wrap_pango_layout_get_tabs(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  NspObject *nsp_ret;
  PangoTabArray *ret;

  ret = pango_layout_get_tabs(PANGO_LAYOUT(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_TAB_ARRAY, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangotabarray))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_layout_set_single_paragraph_mode(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int setting;

  if ( GetArgs(stack,rhs,opt,T,&setting) == FAIL) return RET_BUG;
  pango_layout_set_single_paragraph_mode(PANGO_LAYOUT(self->obj), setting);
  return 0;
}

static int _wrap_pango_layout_get_single_paragraph_mode(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_layout_get_single_paragraph_mode(PANGO_LAYOUT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_context_changed(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  pango_layout_context_changed(PANGO_LAYOUT(self->obj));
  return 0;
}

#line 612 "pango.override"
static int
_wrap_pango_layout_index_to_pos(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int , t_end} ;
  gint index;
  PangoRectangle pos;
  if (GetArgs(stack,rhs,opt,T, &index)== FAIL) return RET_BUG;
  pango_layout_index_to_pos(PANGO_LAYOUT(self->obj), index, &pos);
  if ( nsp_move_doubles(stack,1,1,4,(double) pos.x,(double) pos.y,(double) pos.width,(double) pos.height)== FAIL) 
    return RET_BUG;
  return 1;
}
#line 4441 "pango.c"


#line 626 "pango.override"
static int
_wrap_pango_layout_get_cursor_pos(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int , t_end} ;
  gint index;
  PangoRectangle strong_pos, weak_pos;

  if (GetArgs(stack,rhs,opt,T, &index) == FAIL) return RET_BUG;

  pango_layout_get_cursor_pos(PANGO_LAYOUT(self->obj), index,
			      &strong_pos, &weak_pos);

  if ( nsp_move_doubles(stack,1,1,4,(double)strong_pos.x,(double) strong_pos.y,
			(double)	strong_pos.width, (double)strong_pos.height) == FAIL) return RET_BUG ; 
  if ( nsp_move_doubles(stack,2,1,4,(double)weak_pos.x, weak_pos.y,
			(double)	weak_pos.width,(double) weak_pos.height) == FAIL) return RET_BUG ; 
  return 2; 
}
#line 4463 "pango.c"


#line 646 "pango.override"
static int
_wrap_pango_layout_move_cursor_visually(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  /* static char *kwlist[] = { "strong", "old_index", "old_trailing", "direction", NULL }; */

  int_types T[] = { s_int , s_int , s_int , s_int , t_end} ;
  gboolean strong;
  gint old_index, old_trailing, direction, new_index = 0, new_trailing = 0;
  
  if (GetArgs(stack,rhs,opt,T, &strong, &old_index, &old_trailing, &direction) == FAIL) return RET_BUG;

  pango_layout_move_cursor_visually(PANGO_LAYOUT(self->obj), strong,
				    old_index, old_trailing, direction,
				    &new_index, &new_trailing);
  if ( nsp_move_doubles(stack,1,1,2,(double) new_index,(double) new_trailing)  == FAIL) return RET_BUG;
  return 1;
}
#line 4484 "pango.c"


#line 665 "pango.override"
static int
_wrap_pango_layout_xy_to_index(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int , s_int  , t_end} ;
  /* static char *kwlist[] = { "x", "y", NULL }; */ 
  gint x, y, index, trailing;

  if (GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;

  pango_layout_xy_to_index(PANGO_LAYOUT(self->obj), x, y, &index, &trailing);

  if ( nsp_move_doubles(stack,1,1,2,(double) index,(double) trailing)== FAIL)  return RET_BUG;
  return 1;
}
#line 4502 "pango.c"


#line 681 "pango.override"
static int
_wrap_pango_layout_get_extents(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  PangoRectangle ink_rect, logical_rect;

  pango_layout_get_extents(PANGO_LAYOUT(self->obj),
			   &ink_rect, &logical_rect);
  if ( nsp_move_doubles(stack,1,1,4,(double)ink_rect.x,(double) ink_rect.y,
			(double)ink_rect.width,(double) ink_rect.height) == FAIL) return RET_BUG ; 
  if ( nsp_move_doubles(stack,2,1,4,(double)logical_rect.x, (double)logical_rect.y,
			(double)logical_rect.width,(double) logical_rect.height) == FAIL) return RET_BUG ; 
  return 2; 
}
#line 4519 "pango.c"


#line 696 "pango.override"
static int
_wrap_pango_layout_get_pixel_extents(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  PangoRectangle ink_rect, logical_rect;

  pango_layout_get_pixel_extents(PANGO_LAYOUT(self->obj),
				 &ink_rect, &logical_rect);
  if ( nsp_move_doubles(stack,1,1,4,(double)ink_rect.x,(double) ink_rect.y,
			(double)ink_rect.width,(double) ink_rect.height) == FAIL) return RET_BUG ; 
  if ( nsp_move_doubles(stack,2,1,4,(double)logical_rect.x, (double)logical_rect.y,
			(double)logical_rect.width,(double) logical_rect.height) == FAIL) return RET_BUG ; 
  return 2; 

}
#line 4537 "pango.c"


#line 712 "pango.override"
static int
_wrap_pango_layout_get_size(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint width, height;

  pango_layout_get_size(PANGO_LAYOUT(self->obj), &width, &height);

  if ( nsp_move_doubles(stack,1,1,2,(double) width,(double) height) == FAIL) return RET_BUG;
  return 1;
}
#line 4551 "pango.c"


#line 724 "pango.override"
static int
_wrap_pango_layout_get_pixel_size(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint width, height;

  pango_layout_get_pixel_size(PANGO_LAYOUT(self->obj), &width, &height);

  if ( nsp_move_doubles(stack,1,1,2,(double) width,(double) height)  == FAIL) return RET_BUG;
  return 1;
}
#line 4565 "pango.c"


static int _wrap_pango_layout_get_line_count(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = pango_layout_get_line_count(PANGO_LAYOUT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods pangolayout_methods[] = {
  {"copy",(nsp_method *) _wrap_pango_layout_copy},
  {"get_context",(nsp_method *) _wrap_pango_layout_get_context},
  {"set_attributes",(nsp_method *) _wrap_pango_layout_set_attributes},
  {"get_attributes",(nsp_method *) _wrap_pango_layout_get_attributes},
  {"set_text",(nsp_method *) _wrap_pango_layout_set_text},
  {"get_text",(nsp_method *) _wrap_pango_layout_get_text},
  {"set_markup",(nsp_method *) _wrap_pango_layout_set_markup},
  {"set_markup_with_accel",(nsp_method *) _wrap_pango_layout_set_markup_with_accel},
  {"set_font_description",(nsp_method *) _wrap_pango_layout_set_font_description},
  {"set_width",(nsp_method *) _wrap_pango_layout_set_width},
  {"get_width",(nsp_method *) _wrap_pango_layout_get_width},
  {"set_wrap",(nsp_method *) _wrap_pango_layout_set_wrap},
  {"get_wrap",(nsp_method *) _wrap_pango_layout_get_wrap},
  {"set_indent",(nsp_method *) _wrap_pango_layout_set_indent},
  {"get_indent",(nsp_method *) _wrap_pango_layout_get_indent},
  {"set_spacing",(nsp_method *) _wrap_pango_layout_set_spacing},
  {"get_spacing",(nsp_method *) _wrap_pango_layout_get_spacing},
  {"set_justify",(nsp_method *) _wrap_pango_layout_set_justify},
  {"get_justify",(nsp_method *) _wrap_pango_layout_get_justify},
  {"set_alignment",(nsp_method *) _wrap_pango_layout_set_alignment},
  {"get_alignment",(nsp_method *) _wrap_pango_layout_get_alignment},
  {"set_tabs",(nsp_method *) _wrap_pango_layout_set_tabs},
  {"get_tabs",(nsp_method *) _wrap_pango_layout_get_tabs},
  {"set_single_paragraph_mode",(nsp_method *) _wrap_pango_layout_set_single_paragraph_mode},
  {"get_single_paragraph_mode",(nsp_method *) _wrap_pango_layout_get_single_paragraph_mode},
  {"context_changed",(nsp_method *) _wrap_pango_layout_context_changed},
  {"index_to_pos",(nsp_method *) _wrap_pango_layout_index_to_pos},
  {"get_cursor_pos",(nsp_method *) _wrap_pango_layout_get_cursor_pos},
  {"move_cursor_visually",(nsp_method *) _wrap_pango_layout_move_cursor_visually},
  {"xy_to_index",(nsp_method *) _wrap_pango_layout_xy_to_index},
  {"get_extents",(nsp_method *) _wrap_pango_layout_get_extents},
  {"get_pixel_extents",(nsp_method *) _wrap_pango_layout_get_pixel_extents},
  {"get_size",(nsp_method *) _wrap_pango_layout_get_size},
  {"get_pixel_size",(nsp_method *) _wrap_pango_layout_get_pixel_size},
  {"get_line_count",(nsp_method *) _wrap_pango_layout_get_line_count},
  { NULL, NULL}
};

static NspMethods *pangolayout_get_methods(void) { return pangolayout_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab pangolayout_attrs[]={{NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_pango_attr_type_register(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *name;
  gint ret;

  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret = pango_attr_type_register(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 53 "pango.override"
static int
_wrap_pango_attr_language_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { string , t_end} ;  /*  "language" */
  char *slanguage;
  PangoLanguage *language;
  NspObject *ret; 
  if (GetArgs(stack,rhs,opt,T, &slanguage) == FAIL) return RET_BUG;
  language = pango_language_from_string(slanguage);
  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_language_new(language),
						NULL))== NULL) return RET_BUG;
  MoveObj(stack,1,ret) ;
  return 1;
}
#line 4653 "pango.c"


#line 69 "pango.override"
static int 
_wrap_pango_attr_family_new(Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { string, t_end} ;  /*  "family" */
  char *family;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T,&family)  == FAIL) return RET_BUG;
  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_family_new(family),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret) ;
  return 1;
}
#line 4669 "pango.c"


#line 83 "pango.override"
static int
_wrap_pango_attr_foreground_new(Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int,s_int,s_int, t_end} ;
  /* r,g,b */
  guint16 red, green, blue;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T, &red, &green, &blue)  == FAIL) return RET_BUG;
  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_foreground_new(red, green, blue),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4686 "pango.c"


#line 98 "pango.override"
static int
_wrap_pango_attr_background_new(Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int,s_int,s_int, t_end} ;
  /* r,g,b */
  guint16 red, green, blue;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T, &red, &green, &blue)  == FAIL) return RET_BUG;
  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_background_new(red, green, blue),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4703 "pango.c"


#line 113 "pango.override"
static int
_wrap_pango_attr_size_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_int , t_end} ; /*  "size" */
  int size;
  NspObject *ret; 
  if (GetArgs(stack,rhs,opt,T, &size)  == FAIL) return RET_BUG;
  if ((ret =  (NspObject *)pangoattribute_create(NVOID,pango_attr_size_new(size),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;

}
#line 4719 "pango.c"


#line 127 "pango.override"
static int
_wrap_pango_attr_style_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj , t_end} ; /* "style" */ 
  NspObject *py_style;
  PangoStyle style;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T,&py_style)  == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_STYLE, py_style, &style))
    return RET_BUG;
  if ((ret =  (NspObject *)pangoattribute_create(NVOID,pango_attr_style_new(style),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4738 "pango.c"


#line 144 "pango.override"
static int
_wrap_pango_attr_weight_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj , t_end} ; /* weight */
  NspObject *py_weight;
  PangoWeight weight;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T, &py_weight)   == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_WEIGHT, py_weight,&weight))
    return RET_BUG;
  if ((ret =   (NspObject *)pangoattribute_create(NVOID,pango_attr_weight_new(weight),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4757 "pango.c"


#line 161 "pango.override"
static int
_wrap_pango_attr_variant_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj , t_end} ; /* "variant" */
  NspObject *py_variant;
  PangoVariant variant;
  NspObject *ret; 
  
  if (GetArgs(stack,rhs,opt,T, &py_variant)    == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_VARIANT, py_variant,&variant))
    return RET_BUG;

  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_variant_new(variant),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;

}
#line 4778 "pango.c"


#line 180 "pango.override"
static int
_wrap_pango_attr_stretch_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj , t_end} ; 
  NspObject *py_stretch;
  PangoStretch stretch;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T, &py_stretch)    == FAIL) return RET_BUG;

  if (nspg_enum_get_value(PANGO_TYPE_STRETCH, py_stretch,&stretch))
    return RET_BUG;

  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_stretch_new(stretch),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4799 "pango.c"


#line 199 "pango.override"
static int
_wrap_pango_attr_font_desc_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj , t_end} ; /* desc */

  NspObject *font_desc;
  PangoFontDescription *desc;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T, &font_desc) == FAIL ) return RET_BUG;

  if (!nspg_boxed_check(font_desc, PANGO_TYPE_FONT_DESCRIPTION)) {
    Scierror("desc must be a PangoFontDescription");
    return RET_BUG;
  }
  desc = nspg_boxed_get(font_desc, PangoFontDescription);
  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_font_desc_new(desc),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4823 "pango.c"


#line 221 "pango.override"
static int
_wrap_pango_attr_underline_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { obj , t_end} ;

  NspObject *py_underline;
  PangoUnderline underline;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T, &py_underline)  == FAIL ) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_UNDERLINE, py_underline,
			  &underline))
    return RET_BUG;

  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_underline_new(underline),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4845 "pango.c"


#line 241 "pango.override"
static int 
_wrap_pango_attr_strikethrough_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool , t_end} ;
  gboolean strikethrough;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T, &strikethrough)  == FAIL ) return RET_BUG;

  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_strikethrough_new(strikethrough),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4862 "pango.c"


#line 256 "pango.override"
static int
_wrap_pango_attr_rise_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int , t_end} ;
  gint rise;
  NspObject *ret; 

  if (GetArgs(stack,rhs,opt,T, &rise) == FAIL ) return RET_BUG;
  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_rise_new(rise),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4878 "pango.c"


#line 270 "pango.override"
static int
_wrap_pango_attr_shape_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {realmat,realmat , t_end} ;
  /* static char *kwlist[] = { "ink_rect", "logical_rect", NULL }; */
  PangoRectangle ink_rect, logical_rect;
  NspObject *ret; 
  NspMatrix *r1,*r2; 
  if (GetArgs(stack,rhs,opt,T,&r1,&r2) == FAIL) return RET_BUG; 
  CheckLength(NspFname(stack),1,r1,4);
  CheckLength(NspFname(stack),2,r2,4);
  ink_rect.x=r1->R[0] ; ink_rect.y=r1->R[1] ;  ink_rect.width= r1->R[2]; ink_rect.height = r1->R[3]; 
  logical_rect.x= r2->R[0];logical_rect.y= r2->R[1];  logical_rect.width= r2->R[2];logical_rect.height= r2->R[3];
  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_shape_new(&ink_rect, &logical_rect),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4899 "pango.c"


#line 289 "pango.override"
static int
_wrap_pango_attr_scale_new( Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = { s_double, t_end} ;
  double scale;
  NspObject *ret; 
  
  if (GetArgs(stack,rhs,opt,T, &scale) == FAIL ) return RET_BUG; 
  if ((ret = (NspObject *)pangoattribute_create(NVOID,pango_attr_scale_new(scale),NULL))==  NULL) return RET_BUG;
  MoveObj(stack,1,ret);
  return 1;
}
#line 4915 "pango.c"


#line 736 "pango.override"
static int
_wrap_pango_parse_markup( Stack stack,int rhs,int opt,int lhs)
{
  /* 
  static char *kwlist[] = { "markup_text", "accel_marker", NULL };
  char *markup_text, *text = NULL;
  gint length;
  Nsp_UNICODE *py_accel_marker, py_accel_char;
  gint py_accel_marker_len;
  gunichar accel_marker, accel_char = 0;
  PangoAttrList *attr_list = NULL;
  GError *error = NULL;
  gboolean ret;
  NspObject *py_ret;

  if (GetArgs(stack,rhs,opt,T, "s#u#:pango.parse_markup",
	      kwlist, &markup_text, &length,
	      &py_accel_marker, &py_accel_marker_len))
    return RET_BUG;
  if (py_accel_marker_len != 1) {
    Scierror( "accel_mark must be one character");
    return RET_BUG;
  }
  accel_marker = py_accel_marker[0];
  ret = pango_parse_markup(markup_text, length, accel_marker,
			   &attr_list, &text, &accel_char, &error);
  if (nspg_error_check(&error))
    return RET_BUG;

#if !defined(Nsp_UNICODE_SIZE) || Nsp_UNICODE_SIZE == 2
  if (accel_char >= 0xffff) {
    NspErr_SetString(NspExc_ValueError, "unicode character is too big to fit in a 16-bit unicode character");
    return RET_BUG;
  }
#endif
  py_accel_char = (Nsp_UNICODE)accel_char;

  py_ret = Nsp_BuildValue("(Nsu#)", nspg_boxed_new(PANGO_TYPE_ATTR_LIST,
						  attr_list, FALSE, TRUE),
			  text, &py_accel_char, 1);
  g_free(text);
  return py_ret;
  */
  Scierror("%s: To be done \n",NspFname(stack)); 
  return RET_BUG; 
}
#line 4965 "pango.c"


int _wrap_pango_find_paragraph_boundary(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, s_int, s_int, s_int,t_end};
  char *text;
  int length, paragraph_delimiter_index, next_paragraph_start;

  if ( GetArgs(stack,rhs,opt,T,&text, &length, &paragraph_delimiter_index, &next_paragraph_start) == FAIL) return RET_BUG;
    pango_find_paragraph_boundary(text, length, &paragraph_delimiter_index, &next_paragraph_start);
  return 0;
}

int _wrap_pango_language_from_string(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char *language;
  NspObject *nsp_ret;
  PangoLanguage *ret;

  if ( GetArgs(stack,rhs,opt,T,&language) == FAIL) return RET_BUG;
    ret = pango_language_from_string(language);
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_LANGUAGE, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangolanguage))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab pango_func[]={
  {"pangoattrlist_new", _wrap_pangoattrlist_new},
  {"pangofontdescription_new", _wrap_pangofontdescription_new},
  {"pangoglyphstring_new", _wrap_pangoglyphstring_new},
  {"pangotabarray_new", _wrap_pangotabarray_new},
  {"pangolayout_new", _wrap_pangolayout_new},
  {"pango_attr_type_register", _wrap_pango_attr_type_register},
  {"pango_attr_language_new", _wrap_pango_attr_language_new},
  {"pango_attr_family_new", _wrap_pango_attr_family_new},
  {"pango_attr_foreground_new", _wrap_pango_attr_foreground_new},
  {"pango_attr_background_new", _wrap_pango_attr_background_new},
  {"pango_attr_size_new", _wrap_pango_attr_size_new},
  {"pango_attr_style_new", _wrap_pango_attr_style_new},
  {"pango_attr_weight_new", _wrap_pango_attr_weight_new},
  {"pango_attr_variant_new", _wrap_pango_attr_variant_new},
  {"pango_attr_stretch_new", _wrap_pango_attr_stretch_new},
  {"pango_attr_font_desc_new", _wrap_pango_attr_font_desc_new},
  {"pango_attr_underline_new", _wrap_pango_attr_underline_new},
  {"pango_attr_strikethrough_new", _wrap_pango_attr_strikethrough_new},
  {"pango_attr_rise_new", _wrap_pango_attr_rise_new},
  {"pango_attr_shape_new", _wrap_pango_attr_shape_new},
  {"pango_attr_scale_new", _wrap_pango_attr_scale_new},
  {"pango_parse_markup", _wrap_pango_parse_markup},
  {"pango_find_paragraph_boundary", _wrap_pango_find_paragraph_boundary},
  {"pango_language_from_string", _wrap_pango_language_from_string},
  { NULL, NULL}
};

/* call ith function in the pango interface */

int pango_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(pango_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void pango_Interf_Info(int i, char **fname, function (**f))
{
  *fname = pango_func[i].name;
  *f = pango_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
pango_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_ATTR_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_UNDERLINE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_COVERAGE_LEVEL, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_STYLE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_VARIANT, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_WEIGHT, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_STRETCH, strip_prefix);
  nsp_flags_add_constants((NspHash *)module, PANGO_TYPE_FONT_MASK, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_ALIGNMENT, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_WRAP_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_TAB_ALIGN, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, PANGO_TYPE_DIRECTION, strip_prefix);
}

/* intialise stuff extension classes */
/* void
pango_register_classes(NspObject *d)
{
  NspObject *module;

  if ((module = PyImport_ImportModule("gobject")) != NULL) {
      NspObject *moddict = PyModule_GetDict(module);

      _GObject = (PyTypeObject *)PyDict_GetItemString(moddict, "GObject");
  } else {
      Py_FatalError("could not import gobject");
      return;
  }


#line 14 "pango.override"


#line 5082 "pango.c"
  nspg_register_boxed(d, "AttrList", PANGO_TYPE_ATTR_LIST, &PyPangoAttrList_Type);
  nspg_register_boxed(d, "Color", PANGO_TYPE_COLOR, &PyPangoColor_Type);
  nspg_register_boxed(d, "FontDescription", PANGO_TYPE_FONT_DESCRIPTION, &PyPangoFontDescription_Type);
  nspg_register_boxed(d, "FontMetrics", PANGO_TYPE_FONT_METRICS, &PyPangoFontMetrics_Type);
  nspg_register_boxed(d, "GlyphString", PANGO_TYPE_GLYPH_STRING, &PyPangoGlyphString_Type);
  nspg_register_boxed(d, "Language", PANGO_TYPE_LANGUAGE, &PyPangoLanguage_Type);
  nspg_register_boxed(d, "TabArray", PANGO_TYPE_TAB_ARRAY, &PyPangoTabArray_Type);
  nspgobject_register_class(d, "PangoContext", PANGO_TYPE_CONTEXT, &PyPangoContext_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "PangoFont", PANGO_TYPE_FONT, &PyPangoFont_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "PangoFontFace", PANGO_TYPE_FONT_FACE, &PyPangoFontFace_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "PangoFontFamily", PANGO_TYPE_FONT_FAMILY, &PyPangoFontFamily_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "PangoFontMap", PANGO_TYPE_FONT_MAP, &PyPangoFontMap_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "PangoFontset", PANGO_TYPE_FONTSET, &PyPangoFontset_Type, Py_BuildValue("(O)", &PyGObject_Type));
  nspgobject_register_class(d, "PangoLayout", PANGO_TYPE_LAYOUT, &PyPangoLayout_Type, Py_BuildValue("(O)", &PyGObject_Type));
}
*/
