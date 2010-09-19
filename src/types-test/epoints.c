/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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





#line 20 "codegen/epoints.override"
/* headers in C-file */

#line 31 "epoints.c"

/* ----------- NspEpoints ----------- */


#define  NspEpoints_Private 
#include <nsp/objects.h>
#include <nsp/epoints.h>
#include <nsp/interf.h>

/* 
 * NspEpoints inherits from Object 
 */

int nsp_type_epoints_id=0;
NspTypeEpoints *nsp_type_epoints=NULL;

/*
 * Type object for NspEpoints 
 * all the instance of NspTypeEpoints share the same id. 
 * nsp_type_epoints: is an instance of NspTypeEpoints 
 *    used for objects of NspEpoints type (i.e built with new_epoints) 
 * other instances are used for derived classes 
 */
NspTypeEpoints *new_type_epoints(type_mode mode)
{
  NspTypeEpoints *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_epoints != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_epoints;
    }
  if (( type =  malloc(sizeof(NspTypeEpoints))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = epoints_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = epoints_get_methods;
  type->new = (new_func *) new_epoints;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for epoints */ 

  top->pr = (print_func *) nsp_epoints_print;
  top->dealloc = (dealloc_func *) nsp_epoints_destroy;
  top->copy  =  (copy_func *) nsp_epoints_copy;
  top->size  = (size_func *) nsp_epoints_size;
  top->s_type =  (s_type_func *) nsp_epoints_type_as_string;
  top->sh_type = (sh_type_func *) nsp_epoints_type_short_string;
  top->info = (info_func *) nsp_epoints_info;
  /* top->is_true = (is_true_func  *) nsp_epoints_is_true; */
  /* top->loop =(loop_func *) nsp_epoints_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_epoints_object;
  top->eq  = (eq_func *) nsp_epoints_eq;
  top->neq  = (eq_func *) nsp_epoints_neq;
  top->save  = (save_func *) nsp_epoints_xdr_save;
  top->load  = (load_func *) nsp_epoints_xdr_load;
  top->create = (create_func*) int_epoints_create;
  top->latex = (print_func *) nsp_epoints_latex;
  top->full_copy = (copy_func *) nsp_epoints_full_copy;

  /* specific methods for epoints */

  type->init = (init_func *) init_epoints;

#line 29 "codegen/epoints.override"
  /* inserted verbatim in the type definition */

#line 106 "epoints.c"
  /* 
   * NspEpoints interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_epoints_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeEpoints called nsp_type_epoints
       */
      type->id =  nsp_type_epoints_id = nsp_new_type_id();
      nsp_type_epoints = type;
      if ( nsp_register_type(nsp_type_epoints) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_epoints(mode);
    }
  else 
    {
      type->id = nsp_type_epoints_id;
      return type;
    }
}

/*
 * initialize NspEpoints instances 
 * locally and by calling initializer on parent class 
 */

static int init_epoints(NspEpoints *Obj,NspTypeEpoints *type)
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
 * new instance of NspEpoints 
 */

NspEpoints *new_epoints() 
{
  NspEpoints *loc;
  /* type must exists */
  nsp_type_epoints = new_type_epoints(T_BASE);
  if ( (loc = malloc(sizeof(NspEpoints)))== NULLEPOINTS) return loc;
  /* initialize object */
  if ( init_epoints(loc,nsp_type_epoints) == FAIL) return NULLEPOINTS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspEpoints 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_epoints_size(NspEpoints *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char epoints_type_name[]="Epoints";
static char epoints_short_type_name[]="epoints";

static char *nsp_epoints_type_as_string(void)
{
  return(epoints_type_name);
}

static char *nsp_epoints_type_short_string(NspObject *v)
{
  return(epoints_short_type_name);
}

/*
 * A == B 
 */

static int nsp_epoints_eq(NspEpoints *A, NspObject *B)
{
  NspEpoints *loc = (NspEpoints *) B;
  if ( check_cast(B,nsp_type_epoints_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->func != loc->obj->func) return FALSE;
  if ( A->obj->shid != loc->obj->shid) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_epoints_neq(NspEpoints *A, NspObject *B)
{
  return ( nsp_epoints_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_epoints_xdr_save(XDR *xdrs, NspEpoints *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_epoints)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspEpoints  *nsp_epoints_xdr_load_partial(XDR *xdrs, NspEpoints *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspEpoints  *nsp_epoints_xdr_load(XDR *xdrs)
{
  NspEpoints *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLEPOINTS;
  if ((H  = nsp_epoints_create_void(name,(NspTypeBase *) nsp_type_epoints))== NULLEPOINTS) return H;
  if ( nsp_epoints_create_partial(H) == FAIL) return NULLEPOINTS;
  if ((H  = nsp_epoints_xdr_load_partial(xdrs,H))== NULLEPOINTS) return H;
  if ( nsp_epoints_check_values(H) == FAIL) return NULLEPOINTS;
  return H;
}

/*
 * delete 
 */

void nsp_epoints_destroy_partial(NspEpoints *H)
{
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
}

void nsp_epoints_destroy(NspEpoints *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_epoints_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_epoints_info(NspEpoints *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLEPOINTS) 
    {
      Sciprintf("Null Pointer NspEpoints \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_epoints_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_epoints_print(NspEpoints *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLEPOINTS) 
    {
      Sciprintf("Null Pointer NspEpoints \n");
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
          nsp_epoints_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_epoints_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"func=%xl\n",M->obj->func);
  Sciprintf1(indent+2,"shid=%d\n",M->obj->shid);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_epoints_latex(NspEpoints *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_epoints_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"func=%xl\n",M->obj->func);
  Sciprintf1(indent+2,"shid=%d\n",M->obj->shid);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspEpoints objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspEpoints   *nsp_epoints_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_epoints_id) == TRUE ) return ((NspEpoints *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_epoints));
  return NULL;
}

int IsEpointsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_epoints_id);
}

int IsEpoints(NspObject *O)
{
  return nsp_object_type(O,nsp_type_epoints_id);
}

NspEpoints  *GetEpointsCopy(Stack stack, int i)
{
  if (  GetEpoints(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspEpoints  *GetEpoints(Stack stack, int i)
{
  NspEpoints *M;
  if (( M = nsp_epoints_object(NthObj(i))) == NULLEPOINTS)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspEpoints instance 
 *-----------------------------------------------------*/

static NspEpoints *nsp_epoints_create_void(const char *name,NspTypeBase *type)
{
 NspEpoints *H  = (type == NULL) ? new_epoints() : type->new();
 if ( H ==  NULLEPOINTS)
  {
   Sciprintf("No more memory\n");
   return NULLEPOINTS;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLEPOINTS;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_epoints_create_partial(NspEpoints *H)
{
  if((H->obj = calloc(1,sizeof(nsp_epoints)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->func = NULL;
  H->obj->shid = -1;
  return OK;
}

int nsp_epoints_check_values(NspEpoints *H)
{
  return OK;
}

NspEpoints *nsp_epoints_create(const char *name,void* func,int shid,NspTypeBase *type)
{
  NspEpoints *H  = nsp_epoints_create_void(name,type);
  if ( H ==  NULLEPOINTS) return NULLEPOINTS;
  if ( nsp_epoints_create_partial(H) == FAIL) return NULLEPOINTS;
  H->obj->func = func;
  H->obj->shid=shid;
  if ( nsp_epoints_check_values(H) == FAIL) return NULLEPOINTS;
  return H;
}


NspEpoints *nsp_epoints_create_default(const char *name)
{
 NspEpoints *H  = nsp_epoints_create_void(name,NULL);
 if ( H ==  NULLEPOINTS) return NULLEPOINTS;
  if ( nsp_epoints_create_partial(H) == FAIL) return NULLEPOINTS;
 if ( nsp_epoints_check_values(H) == FAIL) return NULLEPOINTS;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspEpoints *nsp_epoints_copy_partial(NspEpoints *H,NspEpoints *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspEpoints *nsp_epoints_copy(NspEpoints *self)
{
  NspEpoints *H  =nsp_epoints_create_void(NVOID,(NspTypeBase *) nsp_type_epoints);
  if ( H ==  NULLEPOINTS) return NULLEPOINTS;
  if ( nsp_epoints_copy_partial(H,self)== NULL) return NULLEPOINTS;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspEpoints *nsp_epoints_full_copy_partial(NspEpoints *H,NspEpoints *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_epoints))) == NULL) return NULLEPOINTS;
  H->obj->ref_count=1;
  H->obj->func = self->obj->func;
  H->obj->shid=self->obj->shid;
  return H;
}

NspEpoints *nsp_epoints_full_copy(NspEpoints *self)
{
  NspEpoints *H  =nsp_epoints_create_void(NVOID,(NspTypeBase *) nsp_type_epoints);
  if ( H ==  NULLEPOINTS) return NULLEPOINTS;
  if ( nsp_epoints_full_copy_partial(H,self)== NULL) return NULLEPOINTS;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspEpoints
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_epoints_create(Stack stack, int rhs, int opt, int lhs)
{
  NspEpoints *H;
  CheckStdRhs(0,0);
  /* want to be sure that type epoints is initialized */
  nsp_type_epoints = new_type_epoints(T_BASE);
  if(( H = nsp_epoints_create_void(NVOID,(NspTypeBase *) nsp_type_epoints)) == NULLEPOINTS) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_epoints_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_epoints_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *epoints_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab epoints_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Epoints_func[]={
  { "epoints_create", int_epoints_create},
  { NULL, NULL}
};

/* call ith function in the Epoints interface */

int Epoints_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Epoints_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Epoints_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Epoints_func[i].name;
  *f = Epoints_func[i].fonc;
}

#line 41 "codegen/epoints.override"
/* inserted verbatim at the end */

static NspHash *Epoints = NULL;

static int nsp_init_shared_epoints_table(void)
{
  if ( Epoints != NULL ) return OK;
  if (( Epoints = nsp_hash_create("epoints",256)) == NULL) 
    {
      return FAIL;
    }
  return OK;
}

int  nsp_insert_epoint(const char *name, void *func, int sharedid)
{
  NspEpoints *ep = NULL;
  if ( Epoints == NULL ) 
    {
      if ( nsp_init_shared_epoints_table() == FAIL) return FAIL;
    }
  if ((ep = nsp_epoints_create(name,func,sharedid, NULL))== NULL) 
    {
      return FAIL;
    }
  if ( nsp_hash_enter(Epoints,NSP_OBJECT(ep)) == FAIL ) 
    {
      nsp_epoints_destroy(ep);
      return FAIL;
    }
  return OK;
}

NspEpoints *nsp_find_epoint(const char *name) 
{
  NspObject *Obj;
  if ( Epoints == NULL ) return NULL;
  if ( nsp_hash_find(Epoints,name,&Obj) == FAIL) 
    return NULL;
  return ( NspEpoints *) Obj;
}

void nsp_show_epoints()
{
  if ( Epoints == NULL ) return;
  nsp_hash_print(Epoints,0,0,0);
}

static void epoint_default() 
{
}

void nsp_remove_sharedlib_epoints(int shid)
{
  NspObject *Obj;
  int i=0;
  while (1) 
    {
      int rep = nsp_hash_get_next_object(Epoints,&i,&Obj);
      if ( Obj != NULLOBJ )
	{ 
	  NspEpoints *ep = (NspEpoints *) Obj;
	  if ( ep->obj->shid == shid) 
	    {
	      ep->obj->func = epoint_default;
	      nsp_hash_remove(Epoints,nsp_object_get_name(Obj));
	    }
	}
      if ( rep == FAIL) break;
    }
}


#line 603 "epoints.c"
