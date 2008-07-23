/*------------------------------------------------------------------------
 * Copyright (C) 2007-2008 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library emulates Matlab' API functions.
 * It is a fully rewriten version of Scilab mexlib.c file 
 * since Scilab and nsp object are totally different 
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
 *
 * objects for swig interface library 
 *--------------------------------------------------------------------------*/

#include <nsp/object.h>
#include <gtk/gtk.h>
#define  SwigGlobalVar_Private 
#include "swigglobalvar.h"
#include "nsp/interf.h"

/* 
 * NspSwigGlobalVar inherits from NspObject 
 */

int nsp_type_swigglobalvar_id=0;
NspTypeSwigGlobalVar *nsp_type_swigglobalvar=NULL;

/*
 * Type object for SwigGlobalVar 
 * all the instance of NspTypeSwigGlobalVar share the same id. 
 * nsp_type_swigglobalvar: is an instance of NspTypeSwigGlobalVar 
 *    used for objects of NspSwigGlobalVar type (i.e built with new_swigglobalvar) 
 * other instances are used for derived classes 
 */
NspTypeSwigGlobalVar *new_type_swigglobalvar(type_mode mode)
{
  NspTypeSwigGlobalVar *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_swigglobalvar != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_swigglobalvar;
    }
  if ((type =  malloc(sizeof(NspTypeSwigGlobalVar))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = swigglobalvar_get_methods; 
  type->new = (new_func *) new_swigglobalvar;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for swigglobalvar */ 

  top->pr = (print_func *) nsp_swigglobalvar_print;                  
  top->dealloc = (dealloc_func *) nsp_swigglobalvar_destroy;
  top->copy  =  (copy_func *) nsp_swigglobalvar_copy;                 
  top->size  = (size_func *) nsp_swigglobalvar_size;                
  top->s_type =  (s_type_func *) nsp_swigglobalvar_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_swigglobalvar_type_short_string;
  top->info = (info_func *) nsp_swigglobalvar_info ;                  
  /* top->is_true = (is_true_func  *) nsp_swigglobalvar_is_true; */
  /* top->loop =(loop_func *) nsp_swigglobalvar_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_swigglobalvar_object;
  top->eq  = (eq_func *) nsp_swigglobalvar_eq;
  top->neq  = (eq_func *) nsp_swigglobalvar_neq;
  top->save  = (save_func *) nsp_swigglobalvar_xdr_save;
  top->load  = (load_func *) nsp_swigglobalvar_xdr_load;
  top->create = (create_func*) int_swigglobalvar_create;
  top->latex = (print_func *) nsp_swigglobalvar_latex_print;
  
  /* specific methods for swigglobalvar */
      
  type->init = (init_func *) init_swigglobalvar;

  /* 
   * SwigGlobalVar interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_swigglobalvar_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeSwigGlobalVar called nsp_type_swigglobalvar
       */
      type->id =  nsp_type_swigglobalvar_id = nsp_new_type_id();
      nsp_type_swigglobalvar = type;
      if ( nsp_register_type(nsp_type_swigglobalvar) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_swigglobalvar(mode);
    }
  else 
    {
       type->id = nsp_type_swigglobalvar_id;
       return type;
    }
}

/*
 * initialize SwigGlobalVar instances 
 * locally and by calling initializer on parent class 
 */

static int init_swigglobalvar(NspSwigGlobalVar *o,NspTypeSwigGlobalVar *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of SwigGlobalVar 
 */

NspSwigGlobalVar *new_swigglobalvar() 
{
  NspSwigGlobalVar *loc; 
  /* type must exists */
  nsp_type_swigglobalvar = new_type_swigglobalvar(T_BASE);
  if ( (loc = malloc(sizeof(NspSwigGlobalVar)))== NULLSWIGGLOBALVAR) return loc;
  /* initialize object */
  if ( init_swigglobalvar(loc,nsp_type_swigglobalvar) == FAIL) return NULLSWIGGLOBALVAR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for SwigGlobalVar 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_swigglobalvar_size(NspSwigGlobalVar *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char swigglobalvar_type_name[]="SwigGlobalVar";
static char swigglobalvar_short_type_name[]="swig_gv";

static char *nsp_swigglobalvar_type_as_string(void)
{
  return(swigglobalvar_type_name);
}

static char *nsp_swigglobalvar_type_short_string(NspObject *v)
{
  return(swigglobalvar_short_type_name);
}

/*
 * A == B 
 */

static int nsp_swigglobalvar_eq(NspSwigGlobalVar *A, NspObject *B)
{
  NspSwigGlobalVar *loc = (NspSwigGlobalVar *) B;
  if ( check_cast(B,nsp_type_swigglobalvar_id) == FALSE) return FALSE ;
  if ( A->get_attr != loc->get_attr) return FALSE;
  if ( A->set_attr != loc->set_attr) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_swigglobalvar_neq(NspSwigGlobalVar *A, NspObject *B)
{
  return ( nsp_swigglobalvar_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_swigglobalvar_xdr_save(XDR *xdrs, NspSwigGlobalVar *M)
{
  Scierror("Error: swig_gv objects cannot be saved\n");
  return FAIL;
}

/*
 * load 
 */

static NspSwigGlobalVar  *nsp_swigglobalvar_xdr_load(XDR *xdrs)
{
  return NULL;
}

/*
 * delete 
 */

void nsp_swigglobalvar_destroy(NspSwigGlobalVar *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  FREE(H);
}

/*
 * info 
 */

void nsp_swigglobalvar_info(NspSwigGlobalVar *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSWIGGLOBALVAR) 
    {
      Sciprintf("Null Pointer SwigGlobalVar \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_swigglobalvar_type_short_string(NSP_OBJECT(M)));
}

/*
 * print 
 */

int nsp_swigglobalvar_print(NspSwigGlobalVar *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSWIGGLOBALVAR) 
    {
      Sciprintf("Null Pointer SwigGlobalVar \n");
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
          nsp_swigglobalvar_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_swigglobalvar_type_short_string(NSP_OBJECT(M)));
    }
  return TRUE;
}

/*
 * latex print 
 */

void nsp_swigglobalvar_latex_print(NspSwigGlobalVar *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_swigglobalvar_type_short_string(NSP_OBJECT(M)));
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for SwigGlobalVar objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspSwigGlobalVar   *nsp_swigglobalvar_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_swigglobalvar_id) == TRUE ) return ((NspSwigGlobalVar *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_swigglobalvar));
  return NULL;
}

int IsSwigGlobalVarObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_swigglobalvar_id);
}

int IsSwigGlobalVar(NspObject *O)
{
  return nsp_object_type(O,nsp_type_swigglobalvar_id);
}

NspSwigGlobalVar  *GetSwigGlobalVarCopy(Stack stack, int i)
{
  if (  GetSwigGlobalVar(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSwigGlobalVar  *GetSwigGlobalVar(Stack stack, int i)
{
  NspSwigGlobalVar *M;
  if (( M = nsp_swigglobalvar_object(NthObj(i))) == NULLSWIGGLOBALVAR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspSwigGlobalVar *swigglobalvar_create_void(const char *name,NspTypeBase *type)
{
 NspSwigGlobalVar *H  = (type == NULL) ? new_swigglobalvar() : type->new();
 if ( H ==  NULLSWIGGLOBALVAR)
  {
   Sciprintf("No more memory\n");
   return NULLSWIGGLOBALVAR;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSWIGGLOBALVAR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->get_attr = NULL;
 H->set_attr = NULL;
 return H;
}


NspSwigGlobalVar *swigglobalvar_create(const char *name,swig_gv_get_attr *get_attr,swig_gv_set_attr *set_attr,
				       NspTypeBase *type)
{
 NspSwigGlobalVar *H  = swigglobalvar_create_void(name,type);
 if ( H ==  NULLSWIGGLOBALVAR) return NULLSWIGGLOBALVAR;
 H->get_attr=get_attr;
 H->set_attr=set_attr;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspSwigGlobalVar *nsp_swigglobalvar_copy(NspSwigGlobalVar *self)
{
  NspSwigGlobalVar *H  =swigglobalvar_create_void(NVOID,(NspTypeBase *) nsp_type_swigglobalvar);
  if ( H ==  NULLSWIGGLOBALVAR) return NULLSWIGGLOBALVAR;
  H->get_attr=self->get_attr;
  H->set_attr=self->set_attr;
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the SwigGlobalVar
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_swigglobalvar_create(Stack stack, int rhs, int opt, int lhs)
{
  NspSwigGlobalVar *H;
  CheckStdRhs(0,0);
  /* want to be sure that type swigglobalvar is initialized */
  nsp_type_swigglobalvar = new_type_swigglobalvar(T_BASE);
  if(( H = swigglobalvar_create_void(NVOID,(NspTypeBase *) nsp_type_swigglobalvar)) == NULLSWIGGLOBALVAR) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static NspMethods *swigglobalvar_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab swigglobalvar_func[]={
  { "swigglobalvar_create", int_swigglobalvar_create},
  { NULL, NULL}
};

/* call ith function in the swigglobalvar interface */

int swigglobalvar_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(swigglobalvar_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void swigglobalvar_Interf_Info(int i, char **fname, function (**f))
{
  *fname = swigglobalvar_func[i].name;
  *f = swigglobalvar_func[i].fonc;
}


