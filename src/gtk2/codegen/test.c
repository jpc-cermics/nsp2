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





/* ---------- types from other modules ---------- */


/* ---------- forward type declarations ---------- */
#include "nsp/gtk/gdkcolormap.h"


/* ----------- NspGdkColormap ----------- */


#define  GdkColormap_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdkcolormap.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>
/* 
 * NspGdkColormap inherits from GObject 
 */

int nsp_type_gdkcolormap_id=0;
NspTypeGdkColormap *nsp_type_gdkcolormap=NULL;

/*
 * Type object for NspGdkColormap 
 * all the instance of NspTypeGdkColormap share the same id. 
 * nsp_type_gdkcolormap: is an instance of NspTypeGdkColormap 
 *    used for objects of NspGdkColormap type (i.e built with new_gdkcolormap) 
 * other instances are used for derived classes 
 */
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
  type->gtk_methods = TRUE; 
  type->new = (new_func *) new_gdkcolormap;
  
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdkcolormap */ 

  top->s_type =  (s_type_func *) nsp_gdkcolormap_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_gdkcolormap_type_short_string;
  /* top->create = (create_func*) int_gdkcolormap_create;*/ 
  
  /* specific methods for gdkcolormap */
      
  type->init = (init_func *) init_gdkcolormap;

  /* 
   * NspGdkColormap interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
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
 * initialize NspGdkColormap instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdkcolormap(NspGdkColormap *Obj,NspTypeGdkColormap *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type; 
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspGdkColormap 
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
 * Object method redefined for NspGdkColormap 
 *-----------------------------------------------*/

/*
 * type as string 
 */

static char gdkcolormap_type_name[]="GdkColormap";
static char gdkcolormap_short_type_name[]="GdkColormap";

static char *nsp_gdkcolormap_type_as_string(void)
{
  return(gdkcolormap_type_name);
}

static char *nsp_gdkcolormap_type_short_string(NspObject *v)
{
  return(gdkcolormap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGdkColormap objects 
 * Note that some of these functions could become MACROS 
 *-----------------------------------------------------*/

NspGdkColormap *nsp_gdkcolormap_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdkcolormap_id)  == TRUE  ) return ((NspGdkColormap *) O);
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
  if (( M = nsp_gdkcolormap_object(NthObj(i))) == NULLGDKCOLORMAP)
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

static int
_wrap_gdkcolormap_new(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_bool,t_end};
  int allocate;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&allocate) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gdk_colormap_new(allocate))== NULL) return RET_BUG;

  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdkcolormap );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gdk_colormap_alloc_color(NspGdkColormap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, s_bool,t_end};
  int writeable, best_match, ret;
  if ( GetArgs(stack,rhs,opt,T,&writeable, &best_match) == FAIL) return RET_BUG;
  ret = gdk_colormap_alloc_color(GDK_COLORMAP(self->obj), writeable, best_match);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gdk_drawable_set_colormap(NspGdkColormap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *colormap;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkcolormap, &colormap) == FAIL) return RET_BUG;
  gdk_drawable_set_colormap(GDK_COLORMAP(self->obj), GDK_COLORMAP(colormap->obj));
  return 0;
}

static NspMethods gdkcolormap_methods[] = {
  {"alloc_color",(nsp_method *) _wrap_gdk_colormap_alloc_color},
  {"set_colormap",(nsp_method *) _wrap_gdk_drawable_set_colormap},
  { NULL, NULL}
};

static NspMethods *gdkcolormap_get_methods(void) { return gdkcolormap_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdkcolormap_attrs[]={{NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_gdk_colormap_get_system(Stack stack, int rhs, int opt, int lhs) /* colormap_get_system */
{
  GdkColormap *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
ret = gdk_colormap_get_system();
  nsp_type_gdkcolormap = new_type_gdkcolormap(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkcolormap))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_Ctestzzz(Stack stack, int rhs, int opt, int lhs) /* testzzz */
{
  int_types T[] = {s_double, s_int, s_int, s_int,t_end};
  double zz;
  int xx1, xx2, xx3, ret;
  if ( GetArgs(stack,rhs,opt,T,&zz, &xx1, &xx2, &xx3) == FAIL) return RET_BUG;
ret = Ctestzzz(zz, xx1, xx2, xx3);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_Ctestxxx(Stack stack, int rhs, int opt, int lhs) /* testxxx */
{
  int_types T[] = {s_double, s_double, s_double,t_end};
  double zz1, zz2, zz3;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&zz1, &zz2, &zz3) == FAIL) return RET_BUG;
ret = Ctestxxx(zz1, zz2, zz3);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab test_func[]={
  {"gdkcolormap_new", _wrap_gdkcolormap_new},
  {"gdk_colormap_get_system", _wrap_gdk_colormap_get_system},
  {"Ctestzzz", _wrap_Ctestzzz},
  {"Ctestxxx", _wrap_Ctestxxx},
  { NULL, NULL}
};

/* call ith function in the test interface */

int test_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,test_func[i].fonc,
  					       &stack,rhs,opt,lhs);
#else
  return (*(test_func[i].fonc))(stack,rhs,opt,lhs);
#endif
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void test_Interf_Info(int i, char **fname, function (**f))
{
  *fname = test_func[i].name;
  *f = test_func[i].fonc;
}
