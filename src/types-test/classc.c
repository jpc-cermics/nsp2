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





#line 4 "codegen/classc.override"

#line 30 "classc.c"

/* -----------NspClassC ----------- */


#define  NspClassC_Private 
#include <nsp/objects.h>
#include <nsp/classc.h>
#include <nsp/interf.h>

/* 
 * NspClassC inherits from Object 
 */

int nsp_type_classc_id=0;
NspTypeClassC *nsp_type_classc=NULL;

/*
 * Type object for NspClassC 
 * all the instance of NspTypeClassC share the same id. 
 * nsp_type_classc: is an instance of NspTypeClassC 
 *    used for objects of NspClassC type (i.e built with new_classc) 
 * other instances are used for derived classes 
 */
NspTypeClassC *new_type_classc(type_mode mode)
{
  NspTypeClassC *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_classc != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_classc;
    }
  if (( type =  malloc(sizeof(NspTypeClassC))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = classc_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = classc_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_classc;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for classc */ 

  top->pr = (print_func *) nsp_classc_print;
  top->dealloc = (dealloc_func *) nsp_classc_destroy;
  top->copy  =  (copy_func *) nsp_classc_copy;
  top->size  = (size_func *) nsp_classc_size;
  top->s_type =  (s_type_func *) nsp_classc_type_as_string;
  top->sh_type = (sh_type_func *) nsp_classc_type_short_string;
  top->info = (info_func *) nsp_classc_info;
  /* top->is_true = (is_true_func  *) nsp_classc_is_true; */
  /* top->loop =(loop_func *) nsp_classc_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_classc_object;
  top->eq  = (eq_func *) nsp_classc_eq;
  top->neq  = (eq_func *) nsp_classc_neq;
  top->save  = (save_func *) nsp_classc_xdr_save;
  top->load  = (load_func *) nsp_classc_xdr_load;
  top->create = (create_func*) int_classc_create;
  top->latex = (print_func *) nsp_classc_latex;
  top->full_copy = (copy_func *) nsp_classc_full_copy;

  /* specific methods for classc */

  type->init = (init_func *) init_classc;

  /* 
   * NspClassC interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_classc_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeClassC called nsp_type_classc
       */
      type->id =  nsp_type_classc_id = nsp_new_type_id();
      nsp_type_classc = type;
      if ( nsp_register_type(nsp_type_classc) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_classc(mode);
    }
  else 
    {
      type->id = nsp_type_classc_id;
      return type;
    }
}

/*
 * initialize NspClassC instances 
 * locally and by calling initializer on parent class 
 */

static int init_classc(NspClassC *Obj,NspTypeClassC *type)
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
 * new instance of NspClassC 
 */

NspClassC *new_classc() 
{
  NspClassC *loc;
  /* type must exists */
  nsp_type_classc = new_type_classc(T_BASE);
  if ( (loc = malloc(sizeof(NspClassC)))== NULLCLASSC) return loc;
  /* initialize object */
  if ( init_classc(loc,nsp_type_classc) == FAIL) return NULLCLASSC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspClassC 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_classc_size(NspClassC *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char classc_type_name[]="ClassC";
static char classc_short_type_name[]="classc";

static char *nsp_classc_type_as_string(void)
{
  return(classc_type_name);
}

static char *nsp_classc_type_short_string(NspObject *v)
{
  return(classc_short_type_name);
}

/*
 * A == B 
 */

static int nsp_classc_eq(NspClassC *A, NspObject *B)
{
  NspClassC *loc = (NspClassC *) B;
  if ( check_cast(B,nsp_type_classc_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->value != loc->obj->value) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_classc_neq(NspClassC *A, NspObject *B)
{
  return ( nsp_classc_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_classc_xdr_save(XDR *xdrs, NspClassC *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_classc)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if ( nsp_save_ClassC(xdrs,M->obj->value,M) == FAIL ) return FAIL;
  return OK;
}

/*
 * load 
 */

NspClassC  *nsp_classc_xdr_load_partial(XDR *xdrs, NspClassC *M)
{
  M->obj->ref_count=1;
  if ( nsp_load_ClassC(xdrs,M->obj->value,M) == FAIL ) return NULL;
 return M;
}

static NspClassC  *nsp_classc_xdr_load(XDR *xdrs)
{
  NspClassC *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCLASSC;
  if ((H  = nsp_classc_create_void(name,(NspTypeBase *) nsp_type_classc))== NULLCLASSC) return H;
  if ( nsp_classc_create_partial(H) == FAIL) return NULLCLASSC;
  if ((H  = nsp_classc_xdr_load_partial(xdrs,H))== NULLCLASSC) return H;
  if ( nsp_classc_check_values(H) == FAIL) return NULLCLASSC;
#line 39 "codegen/classc.override"
  /* verbatim in create interface  */

#line 247 "classc.c"
  return H;
}

/*
 * delete 
 */

void nsp_classc_destroy_partial(NspClassC *H)
{
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 43 "codegen/classc.override"
  /* verbatim in destroy */

#line 263 "classc.c"
  if (H->obj->value != NULL)
    { nsp_destroy_ClassC(H->obj->value,H);}
    FREE(H->obj);
   }
}

void nsp_classc_destroy(NspClassC *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_classc_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_classc_info(NspClassC *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCLASSC) 
    {
      Sciprintf("Null Pointer NspClassC \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_classc_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_classc_print(NspClassC *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCLASSC) 
    {
      Sciprintf("Null Pointer NspClassC \n");
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
          nsp_classc_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_classc_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  nsp_print_ClassC(indent+2,M->obj->value,M);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_classc_latex(NspClassC *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_classc_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  nsp_print_ClassC(indent+2,M->obj->value,M);
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
 * for NspClassC objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspClassC   *nsp_classc_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_classc_id)  == TRUE  ) return ((NspClassC *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_classc));
  return NULL;
}

int IsClassCObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_classc_id);
}

int IsClassC(NspObject *O)
{
  return nsp_object_type(O,nsp_type_classc_id);
}

NspClassC  *GetClassCCopy(Stack stack, int i)
{
  if (  GetClassC(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspClassC  *GetClassC(Stack stack, int i)
{
  NspClassC *M;
  if (( M = nsp_classc_object(NthObj(i))) == NULLCLASSC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassC instance 
 *-----------------------------------------------------*/

static NspClassC *nsp_classc_create_void(const char *name,NspTypeBase *type)
{
 NspClassC *H  = (type == NULL) ? new_classc() : type->new();
 if ( H ==  NULLCLASSC)
  {
   Sciprintf("No more memory\n");
   return NULLCLASSC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCLASSC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_classc_create_partial(NspClassC *H)
{
  if((H->obj = calloc(1,sizeof(nsp_classc)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->value = NULL;
  return OK;
}

int nsp_classc_check_values(NspClassC *H)
{
  if ( nsp_check_ClassC(H->obj->value,H) == FAIL ) return FAIL;
  return OK;
}

NspClassC *nsp_classc_create(const char *name,ClassC* value,NspTypeBase *type)
{
  NspClassC *H  = nsp_classc_create_void(name,type);
  if ( H ==  NULLCLASSC) return NULLCLASSC;
  if ( nsp_classc_create_partial(H) == FAIL) return NULLCLASSC;
  H->obj->value = value;
  if ( nsp_classc_check_values(H) == FAIL) return NULLCLASSC;
#line 39 "codegen/classc.override"
  /* verbatim in create interface  */

#line 442 "classc.c"
  return H;
}


NspClassC *nsp_classc_create_default(const char *name)
{
 NspClassC *H  = nsp_classc_create_void(name,NULL);
 if ( H ==  NULLCLASSC) return NULLCLASSC;
  if ( nsp_classc_create_partial(H) == FAIL) return NULLCLASSC;
  if ( nsp_classc_check_values(H) == FAIL) return NULLCLASSC;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspClassC *nsp_classc_copy_partial(NspClassC *H,NspClassC *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspClassC *nsp_classc_copy(NspClassC *self)
{
  NspClassC *H  =nsp_classc_create_void(NVOID,(NspTypeBase *) nsp_type_classc);
  if ( H ==  NULLCLASSC) return NULLCLASSC;
  if ( nsp_classc_copy_partial(H,self)== NULL) return NULLCLASSC;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspClassC *nsp_classc_full_copy_partial(NspClassC *H,NspClassC *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_classc))) == NULL) return NULLCLASSC;
  H->obj->ref_count=1;
  if( nsp_ClassC_full_copy(H,H->obj->value,self)== FAIL) return NULL;
  return H;
}

NspClassC *nsp_classc_full_copy(NspClassC *self)
{
  NspClassC *H  =nsp_classc_create_void(NVOID,(NspTypeBase *) nsp_type_classc);
  if ( H ==  NULLCLASSC) return NULLCLASSC;
  if ( nsp_classc_full_copy_partial(H,self)== NULL) return NULLCLASSC;
#line 39 "codegen/classc.override"
  /* verbatim in create interface  */

#line 494 "classc.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspClassC
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_classc_create(Stack stack, int rhs, int opt, int lhs)
{
  NspClassC *H;
  CheckStdRhs(0,0);
  /* want to be sure that type classc is initialized */
  nsp_type_classc = new_type_classc(T_BASE);
  if(( H = nsp_classc_create_void(NVOID,(NspTypeBase *) nsp_type_classc)) == NULLCLASSC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_classc_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_classc_check_values(H) == FAIL) return RET_BUG;
  #line 39 "codegen/classc.override"
  /* verbatim in create interface  */

#line 517 "classc.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_action_get_parameter_type(NspClassC *self,Stack stack,int rhs,int opt,int lhs)
{
  ClassC *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
 ret=NULL; 
  nsp_type_classc= new_type_classc(T_BASE);
  if((ret = nsp_copy_ClassC(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_classc_create(NVOID,ret,(NspTypeBase *) nsp_type_classc);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_change_state(NspClassC *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  ClassC *value = NULL;
  NspObject *nsp_value = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_value) == FAIL) return RET_BUG;
  if ( IsClassC(nsp_value))
    { value = ((NspClassC *) nsp_value)->obj->value;
      if((value = nsp_copy_ClassC(value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: value should be of type ClassC\n");
      return RET_BUG;
    }
  
  return 0;
}

static NspMethods classc_methods[] = {
  {"test_return",(nsp_method *) _wrap_g_action_get_parameter_type},
  {"test_arg",(nsp_method *) _wrap_g_action_change_state},
  { NULL, NULL}
};

static NspMethods *classc_get_methods(void) { return classc_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_classc_get_value(void *self,const char *attr)
{
  ClassC *ret;
  NspObject *nsp_ret;
  ret = ((NspClassC *) self)->obj->value;
  nsp_type_classc= new_type_classc(T_BASE);
  if((ret = nsp_copy_ClassC(ret))==NULL) return NULL;
  nsp_ret =(NspObject*) nsp_classc_create(NVOID,ret,(NspTypeBase *) nsp_type_classc);
  return nsp_ret;
}

static int _wrap_classc_set_value(void *self,const char *attr, NspObject *O)
{
  ClassC *value = NULL;
  NspObject *nsp_value= O;
  if ( IsClassC(nsp_value))
    { value = ((NspClassC *) nsp_value)->obj->value;
      if((value = nsp_copy_ClassC(value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: value should be of type ClassC\n");
      return RET_BUG;
    }
  /* ((NspClassC *) self)->obj->value= value;*/
  return OK;
}

static AttrTab classc_attrs[] = {
  { "value", (attr_get_function * )_wrap_classc_get_value, (attr_set_function * )_wrap_classc_set_value, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab ClassC_func[]={
  { "classc_create", int_classc_create},
  { NULL, NULL}
};

/* call ith function in the ClassC interface */

int ClassC_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(ClassC_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void ClassC_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = ClassC_func[i].name;
  *f = ClassC_func[i].fonc;
}
void nsp_initialize_ClassC_types(void)
{
  new_type_classc(T_BASE);
}

#line 47 "codegen/classc.override"

/* A set of function provided for ClassC */

static int nsp_destroy_ClassC(ClassC *value,NspClassC *H)
{
  return OK;
}

static int nsp_print_ClassC(int indent,ClassC *v,NspClassC *M)
{
  Sciprintf1(indent+1,"print\n");
  return 0;
}

static int nsp_check_ClassC(ClassC *v,NspClassC *H)
{
  return OK;
}

static int nsp_ClassC_full_copy(NspClassC *H,ClassC *value,NspClassC *self)
{
  return OK;
}

ClassC *nsp_copy_ClassC(ClassC *gv)
{
  return gv;
}

/* needed if the field is not hidden */
static int nsp_save_ClassC(XDR *xdrs,ClassC *value,NspClassC *self) 
{
  return OK;
}

/* needed if the field is not hidden */
static int nsp_load_ClassC(XDR *xdrs,ClassC *value,NspClassC *self) 
{
  return OK;
}

#line 678 "classc.c"
