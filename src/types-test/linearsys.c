/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
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





#line 21 "codegen/linearsys.override"
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h> 

#line 32 "linearsys.c"

/* -----------NspLinearSys ----------- */


#define  NspLinearSys_Private 
#include <nsp/objects.h>
#include <nsp/linearsys.h>
#include <nsp/interf.h>

/* 
 * NspLinearSys inherits from Object 
 */

int nsp_type_linearsys_id=0;
NspTypeLinearSys *nsp_type_linearsys=NULL;

/*
 * Type object for NspLinearSys 
 * all the instance of NspTypeLinearSys share the same id. 
 * nsp_type_linearsys: is an instance of NspTypeLinearSys 
 *    used for objects of NspLinearSys type (i.e built with new_linearsys) 
 * other instances are used for derived classes 
 */
NspTypeLinearSys *new_type_linearsys(type_mode mode)
{
  NspTypeLinearSys *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_linearsys != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_linearsys;
    }
  if (( type =  malloc(sizeof(NspTypeLinearSys))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = linearsys_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = linearsys_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_linearsys;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for linearsys */ 

  top->pr = (print_func *) nsp_linearsys_print;
  top->dealloc = (dealloc_func *) nsp_linearsys_destroy;
  top->copy  =  (copy_func *) nsp_linearsys_copy;
  top->size  = (size_func *) nsp_linearsys_size;
  top->s_type =  (s_type_func *) nsp_linearsys_type_as_string;
  top->sh_type = (sh_type_func *) nsp_linearsys_type_short_string;
  top->info = (info_func *) nsp_linearsys_info;
  /* top->is_true = (is_true_func  *) nsp_linearsys_is_true; */
  /* top->loop =(loop_func *) nsp_linearsys_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_linearsys_object;
  top->eq  = (eq_func *) nsp_linearsys_eq;
  top->neq  = (eq_func *) nsp_linearsys_neq;
  top->save  = (save_func *) nsp_linearsys_xdr_save;
  top->load  = (load_func *) nsp_linearsys_xdr_load;
  top->create = (create_func*) int_linearsys_create;
  top->latex = (print_func *) nsp_linearsys_latex;
  top->full_copy = (copy_func *) nsp_linearsys_full_copy;

  /* specific methods for linearsys */

  type->init = (init_func *) init_linearsys;

  /* 
   * NspLinearSys interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_linearsys_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeLinearSys called nsp_type_linearsys
       */
      type->id =  nsp_type_linearsys_id = nsp_new_type_id();
      nsp_type_linearsys = type;
      if ( nsp_register_type(nsp_type_linearsys) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_linearsys(mode);
    }
  else 
    {
      type->id = nsp_type_linearsys_id;
      return type;
    }
}

/*
 * initialize NspLinearSys instances 
 * locally and by calling initializer on parent class 
 */

static int init_linearsys(NspLinearSys *Obj,NspTypeLinearSys *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->A = NULLMAT;
  Obj->B = NULLMAT;
  Obj->C = NULLMAT;
  Obj->D = NULLMAT;
  Obj->X0 = NULLMAT;
  Obj->dom = NULL;
  Obj->dt = 0.0;
 return OK;
}

/*
 * new instance of NspLinearSys 
 */

NspLinearSys *new_linearsys() 
{
  NspLinearSys *loc;
  /* type must exists */
  nsp_type_linearsys = new_type_linearsys(T_BASE);
  if ( (loc = malloc(sizeof(NspLinearSys)))== NULLLINEARSYS) return loc;
  /* initialize object */
  if ( init_linearsys(loc,nsp_type_linearsys) == FAIL) return NULLLINEARSYS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspLinearSys 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_linearsys_size(NspLinearSys *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char linearsys_type_name[]="LinearSys";
static char linearsys_short_type_name[]="linearsys";

static char *nsp_linearsys_type_as_string(void)
{
  return(linearsys_type_name);
}

static char *nsp_linearsys_type_short_string(NspObject *v)
{
  return(linearsys_short_type_name);
}

/*
 * A == B 
 */

static int nsp_linearsys_eq(NspLinearSys *A, NspObject *B)
{
  NspLinearSys *loc = (NspLinearSys *) B;
  if ( check_cast(B,nsp_type_linearsys_id) == FALSE) return FALSE ;
  if ( NSP_OBJECT(A->A)->type->eq(A->A,loc->A) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->B)->type->eq(A->B,loc->B) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->C)->type->eq(A->C,loc->C) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->D)->type->eq(A->D,loc->D) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->X0)->type->eq(A->X0,loc->X0) == FALSE ) return FALSE;
  if ( strcmp(A->dom,loc->dom) != 0) return FALSE;
  if ( A->dt != loc->dt) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_linearsys_neq(NspLinearSys *A, NspObject *B)
{
  return ( nsp_linearsys_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_linearsys_xdr_save(XDR *xdrs, NspLinearSys *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_linearsys)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->A)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->B)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->C)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->D)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->X0)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,M->dom) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->dt) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspLinearSys  *nsp_linearsys_xdr_load_partial(XDR *xdrs, NspLinearSys *M)
{
  if ((M->A =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->B =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->C =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->D =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->X0 =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_new_string(xdrs,&(M->dom)) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->dt) == FAIL) return NULL;
 return M;
}

static NspLinearSys  *nsp_linearsys_xdr_load(XDR *xdrs)
{
  NspLinearSys *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLLINEARSYS;
  if ((H  = nsp_linearsys_create_void(name,(NspTypeBase *) nsp_type_linearsys))== NULLLINEARSYS) return H;
  if ( nsp_linearsys_create_partial(H) == FAIL) return NULLLINEARSYS;
  if ((H  = nsp_linearsys_xdr_load_partial(xdrs,H))== NULLLINEARSYS) return H;
  if ( nsp_linearsys_check_values(H) == FAIL) return NULLLINEARSYS;
#line 39 "codegen/linearsys.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
#line 270 "linearsys.c"
  return H;
}

/*
 * delete 
 */

void nsp_linearsys_destroy_partial(NspLinearSys *H)
{
#line 42 "codegen/linearsys.override"
  /* verbatim in destroy */

#line 283 "linearsys.c"
  if ( H->A != NULL ) 
    nsp_matrix_destroy(H->A);
  if ( H->B != NULL ) 
    nsp_matrix_destroy(H->B);
  if ( H->C != NULL ) 
    nsp_matrix_destroy(H->C);
  if ( H->D != NULL ) 
    nsp_matrix_destroy(H->D);
  if ( H->X0 != NULL ) 
    nsp_matrix_destroy(H->X0);
  nsp_string_destroy(&(H->dom));
}

void nsp_linearsys_destroy(NspLinearSys *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_linearsys_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_linearsys_info(NspLinearSys *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLLINEARSYS) 
    {
      Sciprintf("Null Pointer NspLinearSys \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_linearsys_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_linearsys_print(NspLinearSys *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLLINEARSYS) 
    {
      Sciprintf("Null Pointer NspLinearSys \n");
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
          nsp_linearsys_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_linearsys_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
  if ( M->A != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->A),indent+2,"A", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->B != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->B),indent+2,"B", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->C != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->C),indent+2,"C", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->D != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->D),indent+2,"D", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->X0 != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->X0),indent+2,"X0", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"dom=%s\n",M->dom);
  Sciprintf1(indent+2,"dt=%f\n", M->dt);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_linearsys_latex(NspLinearSys *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_linearsys_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->A != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->A),indent+2,"A", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->B != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->B),indent+2,"B", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->C != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->C),indent+2,"C", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->D != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->D),indent+2,"D", rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->X0 != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->X0),indent+2,"X0", rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"dom=%s\n",M->dom);
  Sciprintf1(indent+2,"dt=%f\n", M->dt);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspLinearSys objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspLinearSys   *nsp_linearsys_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_linearsys_id)  == TRUE  ) return ((NspLinearSys *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_linearsys));
  return NULL;
}

int IsLinearSysObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_linearsys_id);
}

int IsLinearSys(NspObject *O)
{
  return nsp_object_type(O,nsp_type_linearsys_id);
}

NspLinearSys  *GetLinearSysCopy(Stack stack, int i)
{
  if (  GetLinearSys(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspLinearSys  *GetLinearSys(Stack stack, int i)
{
  NspLinearSys *M;
  if (( M = nsp_linearsys_object(NthObj(i))) == NULLLINEARSYS)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspLinearSys instance 
 *-----------------------------------------------------*/

static NspLinearSys *nsp_linearsys_create_void(const char *name,NspTypeBase *type)
{
 NspLinearSys *H  = (type == NULL) ? new_linearsys() : type->new();
 if ( H ==  NULLLINEARSYS)
  {
   Sciprintf("No more memory\n");
   return NULLLINEARSYS;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLLINEARSYS;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_linearsys_create_partial(NspLinearSys *H)
{
  return OK;
}

int nsp_linearsys_check_values(NspLinearSys *H)
{
  if ( H->A == NULLMAT) 
    {
       if (( H->A = nsp_matrix_create("A",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->B == NULLMAT) 
    {
       if (( H->B = nsp_matrix_create("B",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->C == NULLMAT) 
    {
       if (( H->C = nsp_matrix_create("C",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->D == NULLMAT) 
    {
       if (( H->D = nsp_matrix_create("D",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->X0 == NULLMAT) 
    {
       if (( H->X0 = nsp_matrix_create("X0",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->dom == NULL) 
    {
  if (( H->dom = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  return OK;
}

NspLinearSys *nsp_linearsys_create(const char *name,NspMatrix* A,NspMatrix* B,NspMatrix* C,NspMatrix* D,NspMatrix* X0,char* dom,double dt,NspTypeBase *type)
{
  NspLinearSys *H  = nsp_linearsys_create_void(name,type);
  if ( H ==  NULLLINEARSYS) return NULLLINEARSYS;
  H->A= A;
  H->B= B;
  H->C= C;
  H->D= D;
  H->X0= X0;
  H->dom = dom;
  H->dt=dt;
  if ( nsp_linearsys_check_values(H) == FAIL) return NULLLINEARSYS;
#line 39 "codegen/linearsys.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
#line 519 "linearsys.c"
  return H;
}


NspLinearSys *nsp_linearsys_create_default(const char *name)
{
 NspLinearSys *H  = nsp_linearsys_create_void(name,NULL);
 if ( H ==  NULLLINEARSYS) return NULLLINEARSYS;
  if ( nsp_linearsys_check_values(H) == FAIL) return NULLLINEARSYS;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspLinearSys *nsp_linearsys_copy_partial(NspLinearSys *H,NspLinearSys *self)
{
  if ( self->A == NULL )
    { H->A = NULL;}
  else
    {
      if ((H->A = (NspMatrix *) nsp_object_copy_and_name("A", NSP_OBJECT(self->A))) == NULLMAT) return NULL;
    }
  if ( self->B == NULL )
    { H->B = NULL;}
  else
    {
      if ((H->B = (NspMatrix *) nsp_object_copy_and_name("B", NSP_OBJECT(self->B))) == NULLMAT) return NULL;
    }
  if ( self->C == NULL )
    { H->C = NULL;}
  else
    {
      if ((H->C = (NspMatrix *) nsp_object_copy_and_name("C", NSP_OBJECT(self->C))) == NULLMAT) return NULL;
    }
  if ( self->D == NULL )
    { H->D = NULL;}
  else
    {
      if ((H->D = (NspMatrix *) nsp_object_copy_and_name("D", NSP_OBJECT(self->D))) == NULLMAT) return NULL;
    }
  if ( self->X0 == NULL )
    { H->X0 = NULL;}
  else
    {
      if ((H->X0 = (NspMatrix *) nsp_object_copy_and_name("X0", NSP_OBJECT(self->X0))) == NULLMAT) return NULL;
    }
  if ((H->dom = nsp_string_copy(self->dom)) == NULL) return NULL;
  H->dt=self->dt;
  return H;
}

NspLinearSys *nsp_linearsys_copy(NspLinearSys *self)
{
  NspLinearSys *H  =nsp_linearsys_create_void(NVOID,(NspTypeBase *) nsp_type_linearsys);
  if ( H ==  NULLLINEARSYS) return NULLLINEARSYS;
  if ( nsp_linearsys_copy_partial(H,self)== NULL) return NULLLINEARSYS;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspLinearSys *nsp_linearsys_full_copy_partial(NspLinearSys *H,NspLinearSys *self)
{
  if ( self->A == NULL )
    { H->A = NULL;}
  else
    {
      if ((H->A = (NspMatrix *) nsp_object_full_copy_and_name("A", NSP_OBJECT(self->A))) == NULLMAT) return NULL;
    }
  if ( self->B == NULL )
    { H->B = NULL;}
  else
    {
      if ((H->B = (NspMatrix *) nsp_object_full_copy_and_name("B", NSP_OBJECT(self->B))) == NULLMAT) return NULL;
    }
  if ( self->C == NULL )
    { H->C = NULL;}
  else
    {
      if ((H->C = (NspMatrix *) nsp_object_full_copy_and_name("C", NSP_OBJECT(self->C))) == NULLMAT) return NULL;
    }
  if ( self->D == NULL )
    { H->D = NULL;}
  else
    {
      if ((H->D = (NspMatrix *) nsp_object_full_copy_and_name("D", NSP_OBJECT(self->D))) == NULLMAT) return NULL;
    }
  if ( self->X0 == NULL )
    { H->X0 = NULL;}
  else
    {
      if ((H->X0 = (NspMatrix *) nsp_object_full_copy_and_name("X0", NSP_OBJECT(self->X0))) == NULLMAT) return NULL;
    }
  if ((H->dom = nsp_string_copy(self->dom)) == NULL) return NULL;
  H->dt=self->dt;
  return H;
}

NspLinearSys *nsp_linearsys_full_copy(NspLinearSys *self)
{
  NspLinearSys *H  =nsp_linearsys_create_void(NVOID,(NspTypeBase *) nsp_type_linearsys);
  if ( H ==  NULLLINEARSYS) return NULLLINEARSYS;
  if ( nsp_linearsys_full_copy_partial(H,self)== NULL) return NULLLINEARSYS;

#line 39 "codegen/linearsys.override"
  /* verbatim in create/load/full_copy interface use NULL for returned value */
#line 630 "linearsys.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspLinearSys
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

#line 73 "codegen/linearsys.override"

int int_linearsys_create(Stack stack, int rhs, int opt, int lhs)
{
  NspLinearSys *Lss;
  char *dom = "c", *dom1;
  double sample = 1;
  NspMatrix *A, *B, *C, *D, *X0;
  int_types T[] = {mat,mat,mat,mat,mat,new_opts, t_end} ;
  nsp_option opts[] ={{ "dom", string,NULLOBJ,-1},
		      { "sample",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&A,&B,&C,&D,&X0,&opts,&dom,&sample) == FAIL)
    return RET_BUG;
  if ( A->m != A->n )
    {
      Scierror("Error: A should be square");
      return RET_BUG;
    }
  if ( B->m != A->m)
    {
      Scierror("Error: B number of rows, %d, is incompatible with A size %dx%d",B->m,A->m,A->m);
      return RET_BUG;
      
    }
  if ( C->n != A->m)
    {
      Scierror("Error: C number of columns, %d, is incompatible with A size %dx%d",C->n,A->m,A->m);
      return RET_BUG;
    }
  if ( D->m != C->m || D->n != B->n)
    {
      Scierror("Error: D should be %dx%d", C->m,B->n);
      return RET_BUG;
    }
  if ( X0->m != A->m || X0->n != 1)
    {
      Scierror("Error: X0 should be %dx%d",A->m,1);
      return RET_BUG;
    }
  if (! ( strcmp(dom,"c") == 0 ||  strcmp(dom,"d") == 0 ||  strcmp(dom,"u") == 0 ))
    {
      Scierror("Error: dom should be \"c\", \"d\", \"s\" or \"u\"");
      return RET_BUG;
    }
  if (( dom1 = nsp_string_copy(dom)) == NULL) return RET_BUG;
  if (( A = (NspMatrix *) nsp_object_copy_and_name("A",NSP_OBJECT(A))) == NULL) return RET_BUG;
  if (( B = (NspMatrix *)nsp_object_copy_and_name("B",NSP_OBJECT(B))) == NULL) return RET_BUG;
  if (( C = (NspMatrix *)nsp_object_copy_and_name("C",NSP_OBJECT(C))) == NULL) return RET_BUG;
  if (( D = (NspMatrix *)nsp_object_copy_and_name("D",NSP_OBJECT(D)) )== NULL) return RET_BUG;
  if (( X0 = (NspMatrix *)nsp_object_copy_and_name("X0",NSP_OBJECT(X0))) == NULL) return RET_BUG;
  Lss= nsp_linearsys_create(NVOID,A,B,C,D,X0,dom1,sample,NULL);
  if ( Lss == NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Lss));
  return Max(lhs,1);
}

#line 696 "linearsys.c"
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *linearsys_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

#line 46 "codegen/linearsys.override"

static NspObject *_wrap_linearsys_get_A(void *self,const char *attr)
{
  /* O */
  NspMatrix *ret;
  ret = ((NspLinearSys *) self)->A;
  return (NspObject *) ret;
}

static int _wrap_linearsys_set_A(void *self, char *attr, NspObject *O)
{
  NspMatrix *A = (NspMatrix *) O;
  NspMatrix *As = ((NspLinearSys *) self)->A;
  if ( ! IsMat(O) ) return FAIL;
  if ( As->m != A->m || As->n != A->n)
    {
      Scierror("Error: attribute A should be a %dx%d matrix\n",As->m,As->n);
      return FAIL;
    }
  if (( A = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULL) return FAIL;
  if ( As != NULL ) nsp_matrix_destroy(As);
  ((NspLinearSys *) self)->A= A;
  return OK;
}

#line 731 "linearsys.c"
static NspObject *_wrap_linearsys_get_B(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspLinearSys *) self)->B;
  return (NspObject *) ret;
}

static NspObject *_wrap_linearsys_get_obj_B(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspLinearSys *) self)->B);
  return (NspObject *) ret;
}

static int _wrap_linearsys_set_B(void *self,const char *attr, NspObject *O)
{
  NspMatrix *B;
  if ( ! IsMat(O) ) return FAIL;
  if ((B = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspLinearSys *) self)->B != NULL ) 
  nsp_matrix_destroy(((NspLinearSys *) self)->B);
  ((NspLinearSys *) self)->B= B;
  return OK;
}

static NspObject *_wrap_linearsys_get_C(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspLinearSys *) self)->C;
  return (NspObject *) ret;
}

static NspObject *_wrap_linearsys_get_obj_C(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspLinearSys *) self)->C);
  return (NspObject *) ret;
}

static int _wrap_linearsys_set_C(void *self,const char *attr, NspObject *O)
{
  NspMatrix *C;
  if ( ! IsMat(O) ) return FAIL;
  if ((C = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspLinearSys *) self)->C != NULL ) 
  nsp_matrix_destroy(((NspLinearSys *) self)->C);
  ((NspLinearSys *) self)->C= C;
  return OK;
}

static NspObject *_wrap_linearsys_get_D(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspLinearSys *) self)->D;
  return (NspObject *) ret;
}

static NspObject *_wrap_linearsys_get_obj_D(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspLinearSys *) self)->D);
  return (NspObject *) ret;
}

static int _wrap_linearsys_set_D(void *self,const char *attr, NspObject *O)
{
  NspMatrix *D;
  if ( ! IsMat(O) ) return FAIL;
  if ((D = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspLinearSys *) self)->D != NULL ) 
  nsp_matrix_destroy(((NspLinearSys *) self)->D);
  ((NspLinearSys *) self)->D= D;
  return OK;
}

static NspObject *_wrap_linearsys_get_X0(void *self,const char *attr)
{
  NspMatrix *ret;
  ret = ((NspLinearSys *) self)->X0;
  return (NspObject *) ret;
}

static NspObject *_wrap_linearsys_get_obj_X0(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;
  *copy = FALSE;
  ret = ((NspMatrix*) ((NspLinearSys *) self)->X0);
  return (NspObject *) ret;
}

static int _wrap_linearsys_set_X0(void *self,const char *attr, NspObject *O)
{
  NspMatrix *X0;
  if ( ! IsMat(O) ) return FAIL;
  if ((X0 = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspLinearSys *) self)->X0 != NULL ) 
  nsp_matrix_destroy(((NspLinearSys *) self)->X0);
  ((NspLinearSys *) self)->X0= X0;
  return OK;
}

static NspObject *_wrap_linearsys_get_dom(void *self,const char *attr)
{
  NspObject *nsp_ret;
  const gchar *ret;
  ret = ((NspLinearSys *) self)->dom;
  nsp_ret = nsp_new_string_obj(NVOID,ret,-1);
  return nsp_ret;
}

static int _wrap_linearsys_set_dom(void *self,const char *attr, NspObject *O)
{
  char *dom;
  if ((dom = nsp_string_object(O))==NULL) return FAIL;
  if ((dom = nsp_string_copy(dom)) ==NULL) return FAIL;
  nsp_string_destroy(&((NspLinearSys *) self)->dom);
  ((NspLinearSys *) self)->dom= dom;
  return OK;
}

static NspObject *_wrap_linearsys_get_dt(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;
  ret = ((NspLinearSys *) self)->dt;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_linearsys_set_dt(void *self,const char *attr, NspObject *O)
{
  double dt;
  if ( DoubleScalar(O,&dt) == FAIL) return FAIL;
  ((NspLinearSys *) self)->dt= dt;
  return OK;
}

static AttrTab linearsys_attrs[] = {
  { "A", (attr_get_function * )_wrap_linearsys_get_A, (attr_set_function * )_wrap_linearsys_set_A, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "B", (attr_get_function * )_wrap_linearsys_get_B, (attr_set_function * )_wrap_linearsys_set_B, (attr_get_object_function * )_wrap_linearsys_get_obj_B, (attr_set_object_function * )int_set_object_failed },
  { "C", (attr_get_function * )_wrap_linearsys_get_C, (attr_set_function * )_wrap_linearsys_set_C, (attr_get_object_function * )_wrap_linearsys_get_obj_C, (attr_set_object_function * )int_set_object_failed },
  { "D", (attr_get_function * )_wrap_linearsys_get_D, (attr_set_function * )_wrap_linearsys_set_D, (attr_get_object_function * )_wrap_linearsys_get_obj_D, (attr_set_object_function * )int_set_object_failed },
  { "X0", (attr_get_function * )_wrap_linearsys_get_X0, (attr_set_function * )_wrap_linearsys_set_X0, (attr_get_object_function * )_wrap_linearsys_get_obj_X0, (attr_set_object_function * )int_set_object_failed },
  { "dom", (attr_get_function * )_wrap_linearsys_get_dom, (attr_set_function * )_wrap_linearsys_set_dom, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { "dt", (attr_get_function * )_wrap_linearsys_get_dt, (attr_set_function * )_wrap_linearsys_set_dt, (attr_get_object_function * )int_get_object_failed, (attr_set_object_function * )int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab LinearSys_func[]={
  { "linearsys_create", int_linearsys_create},
  { NULL, NULL}
};

/* call ith function in the LinearSys interface */

int LinearSys_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(LinearSys_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void LinearSys_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = LinearSys_func[i].name;
  *f = LinearSys_func[i].fonc;
}
void nsp_initialize_LinearSys_types(void)
{
  new_type_linearsys(T_BASE);
}

#line 917 "linearsys.c"
