/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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





#line 4 "codegen/groot.override"
#include <nsp/objects.h> 
#include <nsp/figuredata.h>
#include <nsp/figure.h>

#line 33 "groot.c"

/* -----------NspGRoot ----------- */


#define  NspGRoot_Private 
#include <nsp/objects.h>
#include <nsp/groot.h>
#include <nsp/interf.h>

/* 
 * NspGRoot inherits from Object 
 */

int nsp_type_groot_id=0;
NspTypeGRoot *nsp_type_groot=NULL;

/*
 * Type object for NspGRoot 
 * all the instance of NspTypeGRoot share the same id. 
 * nsp_type_groot: is an instance of NspTypeGRoot 
 *    used for objects of NspGRoot type (i.e built with new_groot) 
 * other instances are used for derived classes 
 */
NspTypeGRoot *new_type_groot(type_mode mode)
{
  NspTypeGRoot *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_groot != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_groot;
    }
  if (( type =  malloc(sizeof(NspTypeGRoot))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = groot_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = groot_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_groot;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for groot */ 

  top->pr = (print_func *) nsp_groot_print;
  top->dealloc = (dealloc_func *) nsp_groot_destroy;
  top->copy  =  (copy_func *) nsp_groot_copy;
  top->size  = (size_func *) nsp_groot_size;
  top->s_type =  (s_type_func *) nsp_groot_type_as_string;
  top->sh_type = (sh_type_func *) nsp_groot_type_short_string;
  top->info = (info_func *) nsp_groot_info;
  /* top->is_true = (is_true_func  *) nsp_groot_is_true; */
  /* top->loop =(loop_func *) nsp_groot_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_groot_object;
  top->eq  = (eq_func *) nsp_groot_eq;
  top->neq  = (eq_func *) nsp_groot_neq;
  top->save  = (save_func *) nsp_groot_xdr_save;
  top->load  = (load_func *) nsp_groot_xdr_load;
  top->create = (create_func*) int_groot_create;
  top->latex = (print_func *) nsp_groot_latex;
  top->full_copy = (copy_func *) nsp_groot_full_copy;

  /* specific methods for groot */

  type->init = (init_func *) init_groot;

  /* 
   * NspGRoot interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_groot_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGRoot called nsp_type_groot
       */
      type->id =  nsp_type_groot_id = nsp_new_type_id();
      nsp_type_groot = type;
      if ( nsp_register_type(nsp_type_groot) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_groot(mode);
    }
  else 
    {
      type->id = nsp_type_groot_id;
      return type;
    }
}

/*
 * initialize NspGRoot instances 
 * locally and by calling initializer on parent class 
 */

static int init_groot(NspGRoot *Obj,NspTypeGRoot *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
 return OK;
}

/*
 * new instance of NspGRoot 
 */

NspGRoot *new_groot() 
{
  NspGRoot *loc;
  /* type must exists */
  nsp_type_groot = new_type_groot(T_BASE);
  if ( (loc = malloc(sizeof(NspGRoot)))== NULLGROOT) return loc;
  /* initialize object */
  if ( init_groot(loc,nsp_type_groot) == FAIL) return NULLGROOT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGRoot 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_groot_size(NspGRoot *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char groot_type_name[]="GRoot";
static char groot_short_type_name[]="groot";

static char *nsp_groot_type_as_string(void)
{
  return(groot_type_name);
}

static char *nsp_groot_type_short_string(NspObject *v)
{
  return(groot_short_type_name);
}

/*
 * A == B 
 */

static int nsp_groot_eq(NspGRoot *A, NspObject *B)
{
  NspGRoot *loc = (NspGRoot *) B;
  if ( check_cast(B,nsp_type_groot_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->figures)->type->eq(A->obj->figures,loc->obj->figures) == FALSE ) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_groot_neq(NspGRoot *A, NspObject *B)
{
  return ( nsp_groot_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_groot_xdr_save(XDR *xdrs, NspGRoot *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_groot)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->figures)) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGRoot  *nsp_groot_xdr_load_partial(XDR *xdrs, NspGRoot *M)
{
  M->obj->ref_count=1;
  if ((M->obj->figures =(NspList *) nsp_object_xdr_load(xdrs))== NULLLIST) return NULL;
 return M;
}

static NspGRoot  *nsp_groot_xdr_load(XDR *xdrs)
{
  NspGRoot *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGROOT;
  if ((H  = nsp_groot_create_void(name,(NspTypeBase *) nsp_type_groot))== NULLGROOT) return H;
  if ( nsp_groot_create_partial(H) == FAIL) return NULLGROOT;
  if ((H  = nsp_groot_xdr_load_partial(xdrs,H))== NULLGROOT) return H;
  if ( nsp_groot_check_values(H) == FAIL) return NULLGROOT;
  return H;
}

/*
 * delete 
 */

void nsp_groot_destroy_partial(NspGRoot *H)
{
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    if ( H->obj->figures != NULL ) 
      nsp_list_destroy(H->obj->figures);
    FREE(H->obj);
   }
}

void nsp_groot_destroy(NspGRoot *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_groot_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_groot_info(NspGRoot *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGROOT) 
    {
      Sciprintf("Null Pointer NspGRoot \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_groot_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_groot_print(NspGRoot *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGROOT) 
    {
      Sciprintf("Null Pointer NspGRoot \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_groot_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_groot_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->figures != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->figures),indent+2,"figures", rec_level+1)== FALSE ) return FALSE ;
    }
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_groot_latex(NspGRoot *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_groot_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  if ( M->obj->figures != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->figures),FALSE,"figures", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(2,"\\\\\n");
  Sciprintf1(indent+1,"\n");
  Sciprintf("\\end{array}\n");

  Sciprintf("\\right.\n");

  if ( use_math ) Sciprintf("\\end{equation*}\n");

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGRoot objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGRoot   *nsp_groot_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_groot_id)  == TRUE  ) return ((NspGRoot *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_groot));
  return NULL;
}

int IsGRootObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_groot_id);
}

int IsGRoot(NspObject *O)
{
  return nsp_object_type(O,nsp_type_groot_id);
}

NspGRoot  *GetGRootCopy(Stack stack, int i)
{
  if (  GetGRoot(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGRoot  *GetGRoot(Stack stack, int i)
{
  NspGRoot *M;
  if (( M = nsp_groot_object(NthObj(i))) == NULLGROOT)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGRoot instance 
 *-----------------------------------------------------*/

static NspGRoot *nsp_groot_create_void(const char *name,NspTypeBase *type)
{
 NspGRoot *H  = (type == NULL) ? new_groot() : type->new();
 if ( H ==  NULLGROOT)
  {
   Sciprintf("No more memory\n");
   return NULLGROOT;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGROOT;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_groot_create_partial(NspGRoot *H)
{
  if((H->obj = calloc(1,sizeof(nsp_groot)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->figures = NULLLIST;
  return OK;
}

int nsp_groot_check_values(NspGRoot *H)
{
  if ( H->obj->figures == NULLLIST) 
    {
     if (( H->obj->figures = nsp_list_create("figures")) == NULLLIST)
       return FAIL;
    }
  return OK;
}

NspGRoot *nsp_groot_create(const char *name,NspList* figures,NspTypeBase *type)
{
  NspGRoot *H  = nsp_groot_create_void(name,type);
  if ( H ==  NULLGROOT) return NULLGROOT;
  if ( nsp_groot_create_partial(H) == FAIL) return NULLGROOT;
  H->obj->figures= figures;
  if ( nsp_groot_check_values(H) == FAIL) return NULLGROOT;
  return H;
}


NspGRoot *nsp_groot_create_default(const char *name)
{
 NspGRoot *H  = nsp_groot_create_void(name,NULL);
 if ( H ==  NULLGROOT) return NULLGROOT;
  if ( nsp_groot_create_partial(H) == FAIL) return NULLGROOT;
  if ( nsp_groot_check_values(H) == FAIL) return NULLGROOT;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGRoot *nsp_groot_copy_partial(NspGRoot *H,NspGRoot *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGRoot *nsp_groot_copy(NspGRoot *self)
{
  NspGRoot *H  =nsp_groot_create_void(NVOID,(NspTypeBase *) nsp_type_groot);
  if ( H ==  NULLGROOT) return NULLGROOT;
  if ( nsp_groot_copy_partial(H,self)== NULL) return NULLGROOT;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGRoot *nsp_groot_full_copy_partial(NspGRoot *H,NspGRoot *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_groot))) == NULL) return NULLGROOT;
  H->obj->ref_count=1;
  if ( self->obj->figures == NULL )
    { H->obj->figures = NULL;}
  else
    {
      if ((H->obj->figures = (NspList *) nsp_object_full_copy_and_name("figures", NSP_OBJECT(self->obj->figures))) == NULLLIST) return NULL;
    }
  return H;
}

NspGRoot *nsp_groot_full_copy(NspGRoot *self)
{
  NspGRoot *H  =nsp_groot_create_void(NVOID,(NspTypeBase *) nsp_type_groot);
  if ( H ==  NULLGROOT) return NULLGROOT;
  if ( nsp_groot_full_copy_partial(H,self)== NULL) return NULLGROOT;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGRoot
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_groot_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGRoot *H;
  CheckStdRhs(0,0);
  /* want to be sure that type groot is initialized */
  nsp_type_groot = new_type_groot(T_BASE);
  if(( H = nsp_groot_create_void(NVOID,(NspTypeBase *) nsp_type_groot)) == NULLGROOT) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_groot_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_groot_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *groot_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_groot_get_figures(void *self,const char *attr)
{
  NspList *ret;
  ret = ((NspGRoot *) self)->obj->figures;
  return (NspObject *) ret;
}

static NspObject *_wrap_groot_get_obj_figures(void *self,const char *attr, int *copy)
{
  NspList *ret;
  *copy = FALSE;
  ret = ((NspList*) ((NspGRoot *) self)->obj->figures);
  return (NspObject *) ret;
}

static int _wrap_groot_set_figures(void *self,const char *attr, NspObject *O)
{
  NspList *figures;
  if ( ! IsList(O) ) return FAIL;
  if ((figures = (NspList *) nsp_object_copy_and_name(attr,O)) == NULLLIST) return FAIL;
  if (((NspGRoot *) self)->obj->figures != NULL ) 
    nsp_list_destroy(((NspGRoot *) self)->obj->figures);
  ((NspGRoot *) self)->obj->figures= figures;
  return OK;
}

static AttrTab groot_attrs[] = {
  { "figures", (attr_get_function * )_wrap_groot_get_figures, (attr_set_function * )_wrap_groot_set_figures, (attr_get_object_function * )_wrap_groot_get_obj_figures, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GRoot_func[]={
  { "groot_create", int_groot_create},
  { NULL, NULL}
};

/* call ith function in the GRoot interface */

int GRoot_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(GRoot_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GRoot_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = GRoot_func[i].name;
  *f = GRoot_func[i].fonc;
}
void nsp_initialize_GRoot_types(void)
{
  new_type_groot(T_BASE);
}

#line 19 "codegen/groot.override"


#line 594 "groot.c"
