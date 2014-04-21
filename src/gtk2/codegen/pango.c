/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2014 Jean-Philippe Chancelier Enpc/Cermics
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



#line 4 "pango.override"
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gobject-util.h>
#include <pango/pango.h>
#include "nsp/gtk/pangoattribute.h"
#include "nsp/all-pango.h"

#line 36 "pango.c"


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


/* ----------- NspPangoAttrList ----------- */


#define  PangoAttrList_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangoattrlist.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoAttrList inherits from GBoxed 
 */

int nsp_type_pangoattrlist_id=0;
NspTypePangoAttrList *nsp_type_pangoattrlist=NULL;

/*
 * Type object for NspPangoAttrList 
 * all the instance of NspTypePangoAttrList share the same id. 
 * nsp_type_pangoattrlist: is an instance of NspTypePangoAttrList 
 *    used for objects of NspPangoAttrList type (i.e built with new_pangoattrlist) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangoattrlist;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangoattrlist */ 

  top->s_type =  (s_type_func *) nsp_pangoattrlist_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangoattrlist_type_short_string;
  /* top->create = (create_func*) int_pangoattrlist_create;*/ 
  
  /* specific methods for pangoattrlist */
      
  type->init = (init_func *) init_pangoattrlist;

  /* 
   * NspPangoAttrList interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoAttrList instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangoattrlist(NspPangoAttrList *Obj,NspTypePangoAttrList *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoAttrList 
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
 * Object method redefined for NspPangoAttrList 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangoattrlist_type_name[]="PangoAttrList";
static char pangoattrlist_short_type_name[]="PangoAttrList";

static char *nsp_pangoattrlist_type_as_string(void)
{
  return(pangoattrlist_type_name);
}

static char *nsp_pangoattrlist_type_short_string(NspObject *v)
{
  return(pangoattrlist_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoAttrList objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoAttrList *nsp_pangoattrlist_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangoattrlist_id)  == TRUE  ) return ((NspPangoAttrList *) O);
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
  if (( M = nsp_pangoattrlist_object(NthObj(i))) == NULLPANGOATTRLIST)
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
  PangoAttrList *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret = pango_attr_list_copy(NSP_GBOXED_GET(self, PangoAttrList));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_ATTR_LIST, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangoattrlist))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 322 "pango.override"
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
#line 301 "pango.c"


#line 336 "pango.override"
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
#line 318 "pango.c"


#line 351 "pango.override"
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
#line 335 "pango.c"


static int _wrap_pango_attr_list_splice(NspPangoAttrList *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, s_int, s_int,t_end};
  PangoAttrList *other = NULL;
  NspObject *nsp_other;
  int pos, len;
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


/* ----------- NspPangoColor ----------- */


#define  PangoColor_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangocolor.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoColor inherits from GBoxed 
 */

int nsp_type_pangocolor_id=0;
NspTypePangoColor *nsp_type_pangocolor=NULL;

/*
 * Type object for NspPangoColor 
 * all the instance of NspTypePangoColor share the same id. 
 * nsp_type_pangocolor: is an instance of NspTypePangoColor 
 *    used for objects of NspPangoColor type (i.e built with new_pangocolor) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangocolor;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangocolor */ 

  top->s_type =  (s_type_func *) nsp_pangocolor_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangocolor_type_short_string;
  /* top->create = (create_func*) int_pangocolor_create;*/ 
  
  /* specific methods for pangocolor */
      
  type->init = (init_func *) init_pangocolor;

  /* 
   * NspPangoColor interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoColor instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangocolor(NspPangoColor *Obj,NspTypePangoColor *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoColor 
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
 * Object method redefined for NspPangoColor 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangocolor_type_name[]="PangoColor";
static char pangocolor_short_type_name[]="PangoColor";

static char *nsp_pangocolor_type_as_string(void)
{
  return(pangocolor_type_name);
}

static char *nsp_pangocolor_type_short_string(NspObject *v)
{
  return(pangocolor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoColor objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoColor *nsp_pangocolor_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangocolor_id)  == TRUE  ) return ((NspPangoColor *) O);
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
  if (( M = nsp_pangocolor_object(NthObj(i))) == NULLPANGOCOLOR)
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

#line 55 "pango.override"
static int
_wrap_pangocolor_new(Stack stack, int rhs, int opt, int lhs)
{
  PangoColor src={0,0,0}; 
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)pango_color_copy(&src))== NULL) return RET_BUG;
  nsp_type_pangocolor = new_type_pangocolor(T_BASE);
  nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_COLOR, ret,TRUE,TRUE,(NspTypeBase *) nsp_type_pangocolor );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}


#line 589 "pango.c"


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



/* ----------- NspPangoFontDescription ----------- */


#define  PangoFontDescription_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangofontdescription.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoFontDescription inherits from GBoxed 
 */

int nsp_type_pangofontdescription_id=0;
NspTypePangoFontDescription *nsp_type_pangofontdescription=NULL;

/*
 * Type object for NspPangoFontDescription 
 * all the instance of NspTypePangoFontDescription share the same id. 
 * nsp_type_pangofontdescription: is an instance of NspTypePangoFontDescription 
 *    used for objects of NspPangoFontDescription type (i.e built with new_pangofontdescription) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangofontdescription;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontdescription */ 

  top->s_type =  (s_type_func *) nsp_pangofontdescription_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangofontdescription_type_short_string;
  /* top->create = (create_func*) int_pangofontdescription_create;*/ 
  
  /* specific methods for pangofontdescription */
      
  type->init = (init_func *) init_pangofontdescription;

  /* 
   * NspPangoFontDescription interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoFontDescription instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontdescription(NspPangoFontDescription *Obj,NspTypePangoFontDescription *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoFontDescription 
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
 * Object method redefined for NspPangoFontDescription 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontdescription_type_name[]="PangoFontDescription";
static char pangofontdescription_short_type_name[]="PangoFontDescription";

static char *nsp_pangofontdescription_type_as_string(void)
{
  return(pangofontdescription_type_name);
}

static char *nsp_pangofontdescription_type_short_string(NspObject *v)
{
  return(pangofontdescription_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoFontDescription objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoFontDescription *nsp_pangofontdescription_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangofontdescription_id)  == TRUE  ) return ((NspPangoFontDescription *) O);
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
  if (( M = nsp_pangofontdescription_object(NthObj(i))) == NULLPANGOFONTDESCRIPTION)
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

#line 368 "pango.override"
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
#line 870 "pango.c"


#line 416 "pango.override"
static NspObject *
_wrap_pango_font_description_copy(NspObject *self)
{
  return (NspObject *)gboxed_create(NVOID,PANGO_TYPE_FONT_DESCRIPTION,
				    nspg_boxed_get(self, PangoFontDescription),
				    TRUE, TRUE, NULL);
}
#line 881 "pango.c"


static int _wrap_pango_font_description_copy_static(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoFontDescription *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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
  CheckRhs(0,0);
  ret = pango_font_description_get_style(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_set_variant(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoVariant variant;
  NspObject *nsp_variant = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_variant) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_VARIANT, nsp_variant, &variant)== FAIL)
      return RET_BUG;
  pango_font_description_set_variant(NSP_GBOXED_GET(self, PangoFontDescription), variant);
  return 0;
}

static int _wrap_pango_font_description_get_variant(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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
  CheckRhs(0,0);
  ret = pango_font_description_get_size(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_get_set_fields(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
  ret = pango_font_description_get_set_fields(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_description_unset_fields(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoFontMask to_unset;
  NspObject *nsp_to_unset = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_to_unset) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(PANGO_TYPE_FONT_MASK, nsp_to_unset, &to_unset)==FAIL)
      return RET_BUG;
  pango_font_description_unset_fields(NSP_GBOXED_GET(self, PangoFontDescription), to_unset);
  return 0;
}

static int _wrap_pango_font_description_merge(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, s_bool,t_end};
  PangoFontDescription *desc_to_merge = NULL;
  NspObject *nsp_desc_to_merge;
  int replace_existing;
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
  PangoFontDescription *desc_to_merge = NULL;
  NspObject *nsp_desc_to_merge;
  int replace_existing;
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
  PangoFontDescription *old_match = NULL, *new_match = NULL;
  NspObject *nsp_old_match, *nsp_new_match;
  int ret;
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
  CheckRhs(0,0);
  ret = pango_font_description_to_string(NSP_GBOXED_GET(self, PangoFontDescription));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_pango_font_description_to_filename(NspPangoFontDescription *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
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


/* ----------- NspPangoFontMetrics ----------- */


#define  PangoFontMetrics_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangofontmetrics.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoFontMetrics inherits from GBoxed 
 */

int nsp_type_pangofontmetrics_id=0;
NspTypePangoFontMetrics *nsp_type_pangofontmetrics=NULL;

/*
 * Type object for NspPangoFontMetrics 
 * all the instance of NspTypePangoFontMetrics share the same id. 
 * nsp_type_pangofontmetrics: is an instance of NspTypePangoFontMetrics 
 *    used for objects of NspPangoFontMetrics type (i.e built with new_pangofontmetrics) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangofontmetrics;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontmetrics */ 

  top->s_type =  (s_type_func *) nsp_pangofontmetrics_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangofontmetrics_type_short_string;
  /* top->create = (create_func*) int_pangofontmetrics_create;*/ 
  
  /* specific methods for pangofontmetrics */
      
  type->init = (init_func *) init_pangofontmetrics;

  /* 
   * NspPangoFontMetrics interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoFontMetrics instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontmetrics(NspPangoFontMetrics *Obj,NspTypePangoFontMetrics *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoFontMetrics 
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
 * Object method redefined for NspPangoFontMetrics 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontmetrics_type_name[]="PangoFontMetrics";
static char pangofontmetrics_short_type_name[]="PangoFontMetrics";

static char *nsp_pangofontmetrics_type_as_string(void)
{
  return(pangofontmetrics_type_name);
}

static char *nsp_pangofontmetrics_type_short_string(NspObject *v)
{
  return(pangofontmetrics_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoFontMetrics objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoFontMetrics *nsp_pangofontmetrics_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangofontmetrics_id)  == TRUE  ) return ((NspPangoFontMetrics *) O);
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
  if (( M = nsp_pangofontmetrics_object(NthObj(i))) == NULLPANGOFONTMETRICS)
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
  CheckRhs(0,0);
  ret = pango_font_metrics_get_ascent(NSP_GBOXED_GET(self, PangoFontMetrics));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_metrics_get_descent(NspPangoFontMetrics *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = pango_font_metrics_get_descent(NSP_GBOXED_GET(self, PangoFontMetrics));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_metrics_get_approximate_char_width(NspPangoFontMetrics *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = pango_font_metrics_get_approximate_char_width(NSP_GBOXED_GET(self, PangoFontMetrics));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_font_metrics_get_approximate_digit_width(NspPangoFontMetrics *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
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


/* ----------- NspPangoGlyphString ----------- */


#define  PangoGlyphString_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangoglyphstring.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoGlyphString inherits from GBoxed 
 */

int nsp_type_pangoglyphstring_id=0;
NspTypePangoGlyphString *nsp_type_pangoglyphstring=NULL;

/*
 * Type object for NspPangoGlyphString 
 * all the instance of NspTypePangoGlyphString share the same id. 
 * nsp_type_pangoglyphstring: is an instance of NspTypePangoGlyphString 
 *    used for objects of NspPangoGlyphString type (i.e built with new_pangoglyphstring) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangoglyphstring;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangoglyphstring */ 

  top->s_type =  (s_type_func *) nsp_pangoglyphstring_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangoglyphstring_type_short_string;
  /* top->create = (create_func*) int_pangoglyphstring_create;*/ 
  
  /* specific methods for pangoglyphstring */
      
  type->init = (init_func *) init_pangoglyphstring;

  /* 
   * NspPangoGlyphString interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoGlyphString instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangoglyphstring(NspPangoGlyphString *Obj,NspTypePangoGlyphString *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoGlyphString 
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
 * Object method redefined for NspPangoGlyphString 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangoglyphstring_type_name[]="PangoGlyphString";
static char pangoglyphstring_short_type_name[]="PangoGlyphString";

static char *nsp_pangoglyphstring_type_as_string(void)
{
  return(pangoglyphstring_type_name);
}

static char *nsp_pangoglyphstring_type_short_string(NspObject *v)
{
  return(pangoglyphstring_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoGlyphString objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoGlyphString *nsp_pangoglyphstring_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangoglyphstring_id)  == TRUE  ) return ((NspPangoGlyphString *) O);
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
  if (( M = nsp_pangoglyphstring_object(NthObj(i))) == NULLPANGOGLYPHSTRING)
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
  CheckRhs(0,0);
  ret = pango_glyph_string_copy(NSP_GBOXED_GET(self, PangoGlyphString));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_GLYPH_STRING, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangoglyphstring))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 504 "pango.override"
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
#line 1684 "pango.c"


#line 530 "pango.override"
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
#line 1714 "pango.c"


#line 558 "pango.override"
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
#line 1740 "pango.c"


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



/* ----------- NspPangoLanguage ----------- */


#define  PangoLanguage_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangolanguage.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoLanguage inherits from GBoxed 
 */

int nsp_type_pangolanguage_id=0;
NspTypePangoLanguage *nsp_type_pangolanguage=NULL;

/*
 * Type object for NspPangoLanguage 
 * all the instance of NspTypePangoLanguage share the same id. 
 * nsp_type_pangolanguage: is an instance of NspTypePangoLanguage 
 *    used for objects of NspPangoLanguage type (i.e built with new_pangolanguage) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangolanguage;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangolanguage */ 

  top->s_type =  (s_type_func *) nsp_pangolanguage_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangolanguage_type_short_string;
  /* top->create = (create_func*) int_pangolanguage_create;*/ 
  
  /* specific methods for pangolanguage */
      
  type->init = (init_func *) init_pangolanguage;

  /* 
   * NspPangoLanguage interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoLanguage instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangolanguage(NspPangoLanguage *Obj,NspTypePangoLanguage *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoLanguage 
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
 * Object method redefined for NspPangoLanguage 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangolanguage_type_name[]="PangoLanguage";
static char pangolanguage_short_type_name[]="PangoLanguage";

static char *nsp_pangolanguage_type_as_string(void)
{
  return(pangolanguage_type_name);
}

static char *nsp_pangolanguage_type_short_string(NspObject *v)
{
  return(pangolanguage_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoLanguage objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoLanguage *nsp_pangolanguage_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangolanguage_id)  == TRUE  ) return ((NspPangoLanguage *) O);
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
  if (( M = nsp_pangolanguage_object(NthObj(i))) == NULLPANGOLANGUAGE)
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


/* ----------- NspPangoTabArray ----------- */


#define  PangoTabArray_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangotabarray.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoTabArray inherits from GBoxed 
 */

int nsp_type_pangotabarray_id=0;
NspTypePangoTabArray *nsp_type_pangotabarray=NULL;

/*
 * Type object for NspPangoTabArray 
 * all the instance of NspTypePangoTabArray share the same id. 
 * nsp_type_pangotabarray: is an instance of NspTypePangoTabArray 
 *    used for objects of NspPangoTabArray type (i.e built with new_pangotabarray) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangotabarray;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangotabarray */ 

  top->s_type =  (s_type_func *) nsp_pangotabarray_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangotabarray_type_short_string;
  /* top->create = (create_func*) int_pangotabarray_create;*/ 
  
  /* specific methods for pangotabarray */
      
  type->init = (init_func *) init_pangotabarray;

  /* 
   * NspPangoTabArray interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoTabArray instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangotabarray(NspPangoTabArray *Obj,NspTypePangoTabArray *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoTabArray 
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
 * Object method redefined for NspPangoTabArray 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangotabarray_type_name[]="PangoTabArray";
static char pangotabarray_short_type_name[]="PangoTabArray";

static char *nsp_pangotabarray_type_as_string(void)
{
  return(pangotabarray_type_name);
}

static char *nsp_pangotabarray_type_short_string(NspObject *v)
{
  return(pangotabarray_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoTabArray objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoTabArray *nsp_pangotabarray_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangotabarray_id)  == TRUE  ) return ((NspPangoTabArray *) O);
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
  if (( M = nsp_pangotabarray_object(NthObj(i))) == NULLPANGOTABARRAY)
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
  PangoTabArray *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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
  PangoTabAlign alignment;
  NspObject *nsp_alignment = NULL;
  if ( GetArgs(stack,rhs,opt,T,&tab_index, &nsp_alignment, &location) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_TAB_ALIGN, nsp_alignment, &alignment)== FAIL)
      return RET_BUG;
  pango_tab_array_set_tab(NSP_GBOXED_GET(self, PangoTabArray), tab_index, alignment, location);
  return 0;
}

#line 803 "pango.override"
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
#line 2272 "pango.c"


#line 819 "pango.override"
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
#line 2295 "pango.c"


static int _wrap_pango_tab_array_get_positions_in_pixels(NspPangoTabArray *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
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


/* ----------- NspPangoContext ----------- */


#define  PangoContext_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangocontext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoContext inherits from GObject 
 */

int nsp_type_pangocontext_id=0;
NspTypePangoContext *nsp_type_pangocontext=NULL;

/*
 * Type object for NspPangoContext 
 * all the instance of NspTypePangoContext share the same id. 
 * nsp_type_pangocontext: is an instance of NspTypePangoContext 
 *    used for objects of NspPangoContext type (i.e built with new_pangocontext) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangocontext;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangocontext */ 

  top->s_type =  (s_type_func *) nsp_pangocontext_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangocontext_type_short_string;
  /* top->create = (create_func*) int_pangocontext_create;*/ 
  
  /* specific methods for pangocontext */
      
  type->init = (init_func *) init_pangocontext;

  /* 
   * NspPangoContext interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangocontext(NspPangoContext *Obj,NspTypePangoContext *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoContext 
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
 * Object method redefined for NspPangoContext 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangocontext_type_name[]="PangoContext";
static char pangocontext_short_type_name[]="PangoContext";

static char *nsp_pangocontext_type_as_string(void)
{
  return(pangocontext_type_name);
}

static char *nsp_pangocontext_type_short_string(NspObject *v)
{
  return(pangocontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoContext objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoContext *nsp_pangocontext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangocontext_id)  == TRUE  ) return ((NspPangoContext *) O);
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
  if (( M = nsp_pangocontext_object(NthObj(i))) == NULLPANGOCONTEXT)
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

#line 425 "pango.override"
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

    family = (NspObject *)nspgobject_new(NVOID,(GObject *)families[i]);
    NspTuple_SetItem(ret, i, family);
  }
  g_free(families);
  return ret;
}
#line 2534 "pango.c"


static int _wrap_pango_context_load_font(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc, *nsp_ret;
  PangoFont *ret;
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
  PangoFontDescription *desc = NULL;
  NspObject *nsp_desc, *nsp_language, *nsp_ret;
  PangoLanguage *language = NULL;
  PangoFontset *ret;
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
  CheckRhs(0,0);
  ret = pango_context_get_font_description(PANGO_CONTEXT(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_FONT_DESCRIPTION, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangofontdescription))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_context_get_language(NspPangoContext *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoLanguage *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
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
  PangoLanguage *language = NULL;
  NspObject *nsp_language;
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
  CheckRhs(0,0);
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


/* ----------- NspPangoFont ----------- */


#define  PangoFont_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangofont.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoFont inherits from GObject 
 */

int nsp_type_pangofont_id=0;
NspTypePangoFont *nsp_type_pangofont=NULL;

/*
 * Type object for NspPangoFont 
 * all the instance of NspTypePangoFont share the same id. 
 * nsp_type_pangofont: is an instance of NspTypePangoFont 
 *    used for objects of NspPangoFont type (i.e built with new_pangofont) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangofont;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofont */ 

  top->s_type =  (s_type_func *) nsp_pangofont_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangofont_type_short_string;
  /* top->create = (create_func*) int_pangofont_create;*/ 
  
  /* specific methods for pangofont */
      
  type->init = (init_func *) init_pangofont;

  /* 
   * NspPangoFont interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoFont instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofont(NspPangoFont *Obj,NspTypePangoFont *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoFont 
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
 * Object method redefined for NspPangoFont 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofont_type_name[]="PangoFont";
static char pangofont_short_type_name[]="PangoFont";

static char *nsp_pangofont_type_as_string(void)
{
  return(pangofont_type_name);
}

static char *nsp_pangofont_type_short_string(NspObject *v)
{
  return(pangofont_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoFont objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoFont *nsp_pangofont_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangofont_id)  == TRUE  ) return ((NspPangoFont *) O);
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
  if (( M = nsp_pangofont_object(NthObj(i))) == NULLPANGOFONT)
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

static int _wrap_pango_font_describe(NspPangoFont *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoFontDescription *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
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
  PangoLanguage *language = NULL;
  NspObject *nsp_language, *nsp_ret;
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

#line 445 "pango.override"
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
#line 2955 "pango.c"


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


/* ----------- NspPangoFontFace ----------- */


#define  PangoFontFace_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangofontface.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoFontFace inherits from GObject 
 */

int nsp_type_pangofontface_id=0;
NspTypePangoFontFace *nsp_type_pangofontface=NULL;

/*
 * Type object for NspPangoFontFace 
 * all the instance of NspTypePangoFontFace share the same id. 
 * nsp_type_pangofontface: is an instance of NspTypePangoFontFace 
 *    used for objects of NspPangoFontFace type (i.e built with new_pangofontface) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangofontface;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontface */ 

  top->s_type =  (s_type_func *) nsp_pangofontface_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangofontface_type_short_string;
  /* top->create = (create_func*) int_pangofontface_create;*/ 
  
  /* specific methods for pangofontface */
      
  type->init = (init_func *) init_pangofontface;

  /* 
   * NspPangoFontFace interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoFontFace instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontface(NspPangoFontFace *Obj,NspTypePangoFontFace *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoFontFace 
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
 * Object method redefined for NspPangoFontFace 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontface_type_name[]="PangoFontFace";
static char pangofontface_short_type_name[]="PangoFontFace";

static char *nsp_pangofontface_type_as_string(void)
{
  return(pangofontface_type_name);
}

static char *nsp_pangofontface_type_short_string(NspObject *v)
{
  return(pangofontface_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoFontFace objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoFontFace *nsp_pangofontface_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangofontface_id)  == TRUE  ) return ((NspPangoFontFace *) O);
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
  if (( M = nsp_pangofontface_object(NthObj(i))) == NULLPANGOFONTFACE)
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

static int _wrap_pango_font_face_describe(NspPangoFontFace *self,Stack stack,int rhs,int opt,int lhs)
{
  PangoFontDescription *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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


/* ----------- NspPangoFontFamily ----------- */


#define  PangoFontFamily_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangofontfamily.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoFontFamily inherits from GObject 
 */

int nsp_type_pangofontfamily_id=0;
NspTypePangoFontFamily *nsp_type_pangofontfamily=NULL;

/*
 * Type object for NspPangoFontFamily 
 * all the instance of NspTypePangoFontFamily share the same id. 
 * nsp_type_pangofontfamily: is an instance of NspTypePangoFontFamily 
 *    used for objects of NspPangoFontFamily type (i.e built with new_pangofontfamily) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangofontfamily;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontfamily */ 

  top->s_type =  (s_type_func *) nsp_pangofontfamily_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangofontfamily_type_short_string;
  /* top->create = (create_func*) int_pangofontfamily_create;*/ 
  
  /* specific methods for pangofontfamily */
      
  type->init = (init_func *) init_pangofontfamily;

  /* 
   * NspPangoFontFamily interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoFontFamily instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontfamily(NspPangoFontFamily *Obj,NspTypePangoFontFamily *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoFontFamily 
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
 * Object method redefined for NspPangoFontFamily 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontfamily_type_name[]="PangoFontFamily";
static char pangofontfamily_short_type_name[]="PangoFontFamily";

static char *nsp_pangofontfamily_type_as_string(void)
{
  return(pangofontfamily_type_name);
}

static char *nsp_pangofontfamily_type_short_string(NspObject *v)
{
  return(pangofontfamily_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoFontFamily objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoFontFamily *nsp_pangofontfamily_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangofontfamily_id)  == TRUE  ) return ((NspPangoFontFamily *) O);
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
  if (( M = nsp_pangofontfamily_object(NthObj(i))) == NULLPANGOFONTFAMILY)
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

#line 465 "pango.override"
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
    NspObject *face = (NspObject *)nspgobject_new(NVOID,(GObject *)faces[i]);
    NspTuple_SetItem(ret, i, face);
  }
  g_free(faces);
  return ret;
}
#line 3406 "pango.c"


static int _wrap_pango_font_family_get_name(NspPangoFontFamily *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
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


/* ----------- NspPangoFontMap ----------- */


#define  PangoFontMap_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangofontmap.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoFontMap inherits from GObject 
 */

int nsp_type_pangofontmap_id=0;
NspTypePangoFontMap *nsp_type_pangofontmap=NULL;

/*
 * Type object for NspPangoFontMap 
 * all the instance of NspTypePangoFontMap share the same id. 
 * nsp_type_pangofontmap: is an instance of NspTypePangoFontMap 
 *    used for objects of NspPangoFontMap type (i.e built with new_pangofontmap) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangofontmap;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontmap */ 

  top->s_type =  (s_type_func *) nsp_pangofontmap_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangofontmap_type_short_string;
  /* top->create = (create_func*) int_pangofontmap_create;*/ 
  
  /* specific methods for pangofontmap */
      
  type->init = (init_func *) init_pangofontmap;

  /* 
   * NspPangoFontMap interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoFontMap instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontmap(NspPangoFontMap *Obj,NspTypePangoFontMap *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoFontMap 
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
 * Object method redefined for NspPangoFontMap 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontmap_type_name[]="PangoFontMap";
static char pangofontmap_short_type_name[]="PangoFontMap";

static char *nsp_pangofontmap_type_as_string(void)
{
  return(pangofontmap_type_name);
}

static char *nsp_pangofontmap_type_short_string(NspObject *v)
{
  return(pangofontmap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoFontMap objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoFontMap *nsp_pangofontmap_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangofontmap_id)  == TRUE  ) return ((NspPangoFontMap *) O);
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
  if (( M = nsp_pangofontmap_object(NthObj(i))) == NULLPANGOFONTMAP)
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

#line 485 "pango.override"
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
    NspObject *family= (NspObject *) nspgobject_new(NVOID,(GObject *)families[i]);
    NspTuple_SetItem(ret, i, family);
  }
  g_free(families);
  return ret;
}
#line 3688 "pango.c"


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


/* ----------- NspPangoFontset ----------- */


#define  PangoFontset_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangofontset.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoFontset inherits from GObject 
 */

int nsp_type_pangofontset_id=0;
NspTypePangoFontset *nsp_type_pangofontset=NULL;

/*
 * Type object for NspPangoFontset 
 * all the instance of NspTypePangoFontset share the same id. 
 * nsp_type_pangofontset: is an instance of NspTypePangoFontset 
 *    used for objects of NspPangoFontset type (i.e built with new_pangofontset) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangofontset;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangofontset */ 

  top->s_type =  (s_type_func *) nsp_pangofontset_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangofontset_type_short_string;
  /* top->create = (create_func*) int_pangofontset_create;*/ 
  
  /* specific methods for pangofontset */
      
  type->init = (init_func *) init_pangofontset;

  /* 
   * NspPangoFontset interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoFontset instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangofontset(NspPangoFontset *Obj,NspTypePangoFontset *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoFontset 
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
 * Object method redefined for NspPangoFontset 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangofontset_type_name[]="PangoFontset";
static char pangofontset_short_type_name[]="PangoFontset";

static char *nsp_pangofontset_type_as_string(void)
{
  return(pangofontset_type_name);
}

static char *nsp_pangofontset_type_short_string(NspObject *v)
{
  return(pangofontset_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoFontset objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoFontset *nsp_pangofontset_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangofontset_id)  == TRUE  ) return ((NspPangoFontset *) O);
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
  if (( M = nsp_pangofontset_object(NthObj(i))) == NULLPANGOFONTSET)
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
  PangoFontMetrics *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
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


/* ----------- NspPangoLayout ----------- */


#define  PangoLayout_Private 
#include <nsp/objects.h>
#include <nsp/gtk/pangolayout.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspPangoLayout inherits from GObject 
 */

int nsp_type_pangolayout_id=0;
NspTypePangoLayout *nsp_type_pangolayout=NULL;

/*
 * Type object for NspPangoLayout 
 * all the instance of NspTypePangoLayout share the same id. 
 * nsp_type_pangolayout: is an instance of NspTypePangoLayout 
 *    used for objects of NspPangoLayout type (i.e built with new_pangolayout) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_pangolayout;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for pangolayout */ 

  top->s_type =  (s_type_func *) nsp_pangolayout_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_pangolayout_type_short_string;
  /* top->create = (create_func*) int_pangolayout_create;*/ 
  
  /* specific methods for pangolayout */
      
  type->init = (init_func *) init_pangolayout;

  /* 
   * NspPangoLayout interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspPangoLayout instances 
 * locally and by calling initializer on parent class 
 */

static int init_pangolayout(NspPangoLayout *Obj,NspTypePangoLayout *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspPangoLayout 
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
 * Object method redefined for NspPangoLayout 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char pangolayout_type_name[]="PangoLayout";
static char pangolayout_short_type_name[]="PangoLayout";

static char *nsp_pangolayout_type_as_string(void)
{
  return(pangolayout_type_name);
}

static char *nsp_pangolayout_type_short_string(NspObject *v)
{
  return(pangolayout_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspPangoLayout objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspPangoLayout *nsp_pangolayout_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_pangolayout_id)  == TRUE  ) return ((NspPangoLayout *) O);
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
  if (( M = nsp_pangolayout_object(NthObj(i))) == NULLPANGOLAYOUT)
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
  PangoLayout *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
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
  CheckRhs(0,0);
  ret = pango_layout_get_context(PANGO_LAYOUT(self->obj));
  nsp_type_pangocontext = new_type_pangocontext(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_pangocontext))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_pango_layout_set_attributes(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoAttrList *attrs = NULL;
  NspObject *nsp_attrs;
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
  PangoAttrList *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret = pango_layout_get_attributes(PANGO_LAYOUT(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,PANGO_TYPE_ATTR_LIST, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_pangoattrlist))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#line 840 "pango.override"
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




#line 4210 "pango.c"


static int _wrap_pango_layout_get_text(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
  ret = pango_layout_get_text(PANGO_LAYOUT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#line 582 "pango.override"
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
#line 4234 "pango.c"


#line 595 "pango.override"
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
#line 4272 "pango.c"


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
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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
  CheckRhs(0,0);
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
  CheckRhs(0,0);
  ret = pango_layout_get_justify(PANGO_LAYOUT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_set_alignment(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoAlignment alignment;
  NspObject *nsp_alignment = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_alignment) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(PANGO_TYPE_ALIGNMENT, nsp_alignment, &alignment)== FAIL)
      return RET_BUG;
  pango_layout_set_alignment(PANGO_LAYOUT(self->obj), alignment);
  return 0;
}

static int _wrap_pango_layout_get_alignment(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
  ret = pango_layout_get_alignment(PANGO_LAYOUT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_set_tabs(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  PangoTabArray *tabs = NULL;
  NspObject *nsp_tabs;
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
  PangoTabArray *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
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
  CheckRhs(0,0);
  ret = pango_layout_get_single_paragraph_mode(PANGO_LAYOUT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_context_changed(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  pango_layout_context_changed(PANGO_LAYOUT(self->obj));
  return 0;
}

#line 631 "pango.override"
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
#line 4472 "pango.c"


#line 645 "pango.override"
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
#line 4494 "pango.c"


#line 665 "pango.override"
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
#line 4515 "pango.c"


#line 684 "pango.override"
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
#line 4533 "pango.c"


#line 700 "pango.override"
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
#line 4550 "pango.c"


#line 715 "pango.override"
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
#line 4568 "pango.c"


#line 731 "pango.override"
static int
_wrap_pango_layout_get_size(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint width, height;

  pango_layout_get_size(PANGO_LAYOUT(self->obj), &width, &height);

  if ( nsp_move_doubles(stack,1,1,2,(double) width,(double) height) == FAIL) return RET_BUG;
  return 1;
}
#line 4582 "pango.c"


#line 743 "pango.override"
static int
_wrap_pango_layout_get_pixel_size(NspGObject *self, Stack stack,int rhs,int opt,int lhs)
{
  gint width, height;

  pango_layout_get_pixel_size(PANGO_LAYOUT(self->obj), &width, &height);

  if ( nsp_move_doubles(stack,1,1,2,(double) width,(double) height)  == FAIL) return RET_BUG;
  return 1;
}
#line 4596 "pango.c"


static int _wrap_pango_layout_get_line_count(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
  ret = pango_layout_get_line_count(PANGO_LAYOUT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_pango_layout_get_lines(NspPangoLayout *self,Stack stack,int rhs,int opt,int lhs)
{
  GSList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
  ret = pango_layout_get_lines(PANGO_LAYOUT(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

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
  {"get_lines",(nsp_method *) _wrap_pango_layout_get_lines},
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
int _wrap_pango_attr_type_register(Stack stack, int rhs, int opt, int lhs) /* pango_attr_type_register */
{
  int_types T[] = {string,t_end};
  char *name;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
ret = pango_attr_type_register(name);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#line 72 "pango.override"
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
#line 4694 "pango.c"


#line 88 "pango.override"
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
#line 4710 "pango.c"


#line 102 "pango.override"
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
#line 4727 "pango.c"


#line 117 "pango.override"
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
#line 4744 "pango.c"


#line 132 "pango.override"
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
#line 4760 "pango.c"


#line 146 "pango.override"
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
#line 4779 "pango.c"


#line 163 "pango.override"
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
#line 4798 "pango.c"


#line 180 "pango.override"
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
#line 4819 "pango.c"


#line 199 "pango.override"
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
#line 4840 "pango.c"


#line 218 "pango.override"
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
#line 4864 "pango.c"


#line 240 "pango.override"
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
#line 4886 "pango.c"


#line 260 "pango.override"
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
#line 4903 "pango.c"


#line 275 "pango.override"
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
#line 4919 "pango.c"


#line 289 "pango.override"
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
#line 4940 "pango.c"


#line 308 "pango.override"
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
#line 4956 "pango.c"


#line 755 "pango.override"
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
#line 5006 "pango.c"


int _wrap_pango_find_paragraph_boundary(Stack stack, int rhs, int opt, int lhs) /* find_paragraph_boundary */
{
  int_types T[] = {string, s_int, s_int, s_int,t_end};
  char *text;
  int length, paragraph_delimiter_index, next_paragraph_start;
  if ( GetArgs(stack,rhs,opt,T,&text, &length, &paragraph_delimiter_index, &next_paragraph_start) == FAIL) return RET_BUG;
pango_find_paragraph_boundary(text, length, &paragraph_delimiter_index, &next_paragraph_start);
  return 0;
}

int _wrap_pango_reorder_items(Stack stack, int rhs, int opt, int lhs) /* reorder_items */
{
  int_types T[] = {list,t_end};
  NspList *nsp_logical_items, *nsp_list;
  GList *logical_items, *ret, *tmp;
  if ( GetArgs(stack,rhs,opt,T,&nsp_logical_items) == FAIL) return RET_BUG;
  logical_items=nsp_glist_from_nsplist(stack,nsp_logical_items);
  if (logical_items== NULL) return RET_BUG;
ret = pango_reorder_items(logical_items);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_pango_language_from_string(Stack stack, int rhs, int opt, int lhs) /* pango_language_from_string */
{
  int_types T[] = {string,t_end};
  char *language;
  PangoLanguage *ret;
  NspObject *nsp_ret;
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
  {"pangocolor_new", _wrap_pangocolor_new},
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
  {"pango_reorder_items", _wrap_pango_reorder_items},
  {"pango_language_from_string", _wrap_pango_language_from_string},
  { NULL, NULL}
};

/* call ith function in the pango interface */

int pango_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,pango_func[i].fonc,
  					       &stack,rhs,opt,lhs);
#else
  return (*(pango_func[i].fonc))(stack,rhs,opt,lhs);
#endif
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

