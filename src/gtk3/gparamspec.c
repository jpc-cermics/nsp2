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





#line 4 "codegen-3.0/gparamspec.override"
#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/gtk/gobject.h>

#ifdef GetGParamSpec
#undef GetGParamSpec
#endif

#line 37 "gparamspec.c"

/* -----------NspGParamSpec ----------- */


#define  NspGParamSpec_Private 
#include <nsp/objects.h>
#include <nsp/gparamspec.h>
#include <nsp/interf.h>

/* 
 * NspGParamSpec inherits from Object 
 */

int nsp_type_gparamspec_id=0;
NspTypeGParamSpec *nsp_type_gparamspec=NULL;

/*
 * Type object for NspGParamSpec 
 * all the instance of NspTypeGParamSpec share the same id. 
 * nsp_type_gparamspec: is an instance of NspTypeGParamSpec 
 *    used for objects of NspGParamSpec type (i.e built with new_gparamspec) 
 * other instances are used for derived classes 
 */
NspTypeGParamSpec *new_type_gparamspec(type_mode mode)
{
  NspTypeGParamSpec *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gparamspec != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gparamspec;
    }
  if (( type =  malloc(sizeof(NspTypeGParamSpec))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gparamspec_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gparamspec_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_gparamspec;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gparamspec */ 

  top->pr = (print_func *) nsp_gparamspec_print;
  top->dealloc = (dealloc_func *) nsp_gparamspec_destroy;
  top->copy  =  (copy_func *) nsp_gparamspec_copy;
  top->size  = (size_func *) nsp_gparamspec_size;
  top->s_type =  (s_type_func *) nsp_gparamspec_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gparamspec_type_short_string;
  top->info = (info_func *) nsp_gparamspec_info;
  /* top->is_true = (is_true_func  *) nsp_gparamspec_is_true; */
  /* top->loop =(loop_func *) nsp_gparamspec_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gparamspec_object;
  top->eq  = (eq_func *) nsp_gparamspec_eq;
  top->neq  = (eq_func *) nsp_gparamspec_neq;
  top->save  = (save_func *) nsp_gparamspec_xdr_save;
  top->load  = (load_func *) nsp_gparamspec_xdr_load;
  top->create = (create_func*) int_gparamspec_create;
  top->latex = (print_func *) nsp_gparamspec_latex;
  top->full_copy = (copy_func *) nsp_gparamspec_full_copy;

  /* specific methods for gparamspec */

  type->init = (init_func *) init_gparamspec;

  /* 
   * NspGParamSpec interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gparamspec_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGParamSpec called nsp_type_gparamspec
       */
      type->id =  nsp_type_gparamspec_id = nsp_new_type_id();
      nsp_type_gparamspec = type;
      if ( nsp_register_type(nsp_type_gparamspec) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gparamspec(mode);
    }
  else 
    {
      type->id = nsp_type_gparamspec_id;
      return type;
    }
}

/*
 * initialize NspGParamSpec instances 
 * locally and by calling initializer on parent class 
 */

static int init_gparamspec(NspGParamSpec *Obj,NspTypeGParamSpec *type)
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
 * new instance of NspGParamSpec 
 */

NspGParamSpec *new_gparamspec() 
{
  NspGParamSpec *loc;
  /* type must exists */
  nsp_type_gparamspec = new_type_gparamspec(T_BASE);
  if ( (loc = malloc(sizeof(NspGParamSpec)))== NULLGPARAMSPEC) return loc;
  /* initialize object */
  if ( init_gparamspec(loc,nsp_type_gparamspec) == FAIL) return NULLGPARAMSPEC;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGParamSpec 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gparamspec_size(NspGParamSpec *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char gparamspec_type_name[]="GParamSpec";
static char gparamspec_short_type_name[]="gparamspec";

static char *nsp_gparamspec_type_as_string(void)
{
  return(gparamspec_type_name);
}

static char *nsp_gparamspec_type_short_string(NspObject *v)
{
  return(gparamspec_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gparamspec_eq(NspGParamSpec *A, NspObject *B)
{
  NspGParamSpec *loc = (NspGParamSpec *) B;
  if ( check_cast(B,nsp_type_gparamspec_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->value != loc->obj->value) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_gparamspec_neq(NspGParamSpec *A, NspObject *B)
{
  return ( nsp_gparamspec_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gparamspec_xdr_save(XDR *xdrs, NspGParamSpec *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gparamspec)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGParamSpec  *nsp_gparamspec_xdr_load_partial(XDR *xdrs, NspGParamSpec *M)
{
  M->obj->ref_count=1;
 return M;
}

static NspGParamSpec  *nsp_gparamspec_xdr_load(XDR *xdrs)
{
  NspGParamSpec *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGPARAMSPEC;
  if ((H  = nsp_gparamspec_create_void(name,(NspTypeBase *) nsp_type_gparamspec))== NULLGPARAMSPEC) return H;
  if ( nsp_gparamspec_create_partial(H) == FAIL) return NULLGPARAMSPEC;
  if ((H  = nsp_gparamspec_xdr_load_partial(xdrs,H))== NULLGPARAMSPEC) return H;
  if ( nsp_gparamspec_check_values(H) == FAIL) return NULLGPARAMSPEC;
  return H;
}

/*
 * delete 
 */

void nsp_gparamspec_destroy_partial(NspGParamSpec *H)
{
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
  if (H->obj->value != NULL)
    { nsp_destroy_GParamSpec(H->obj->value,H);}
    FREE(H->obj);
   }
}

void nsp_gparamspec_destroy(NspGParamSpec *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gparamspec_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gparamspec_info(NspGParamSpec *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGPARAMSPEC) 
    {
      Sciprintf("Null Pointer NspGParamSpec \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gparamspec_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gparamspec_print(NspGParamSpec *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGPARAMSPEC) 
    {
      Sciprintf("Null Pointer NspGParamSpec \n");
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
          nsp_gparamspec_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_gparamspec_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  nsp_print_GParamSpec(indent+2,M->obj->value,M);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gparamspec_latex(NspGParamSpec *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gparamspec_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  nsp_print_GParamSpec(indent+2,M->obj->value,M);
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
 * for NspGParamSpec objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGParamSpec   *nsp_gparamspec_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gparamspec_id)  == TRUE  ) return ((NspGParamSpec *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gparamspec));
  return NULL;
}

int IsGParamSpecObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gparamspec_id);
}

int IsGParamSpec(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gparamspec_id);
}

NspGParamSpec  *GetGParamSpecCopy(Stack stack, int i)
{
  if (  GetGParamSpec(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGParamSpec  *GetGParamSpec(Stack stack, int i)
{
  NspGParamSpec *M;
  if (( M = nsp_gparamspec_object(NthObj(i))) == NULLGPARAMSPEC)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGParamSpec instance 
 *-----------------------------------------------------*/
#line 41 "codegen-3.0/gparamspec.override"

/* override the code for gparamspec creation
 * we change the function nsp_gparamspec_create
 * since H->value = value does not work for Gvalue
 */

static NspGParamSpec *nsp_gparamspec_create_void(const char *name,NspTypeBase *type)
{
 NspGParamSpec *H  = (type == NULL) ? new_gparamspec() : type->new();
 if ( H ==  NULLGPARAMSPEC)
  {
   Sciprintf("No more memory\n");
   return NULLGPARAMSPEC;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGPARAMSPEC;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gparamspec_create_partial(NspGParamSpec *H)
{
  return OK;
}

int nsp_gparamspec_check_values(NspGParamSpec *H)
{
  if ( nsp_check_GParamSpec(H->obj->value,H) == FAIL ) return FAIL;
  return OK;
}

NspGParamSpec *nsp_gparamspec_create(const char *name,GParamSpec *value,NspTypeBase *type)
{
  NspGParamSpec *H  = nsp_gparamspec_create_void(name,type);
  if ( H ==  NULLGPARAMSPEC) return NULLGPARAMSPEC;
  if ((H->obj = calloc(1,sizeof(nsp_gparamspec))) == NULL) return NULLGPARAMSPEC;
  H->obj->ref_count=1;
  H->obj->value = value;
  g_param_spec_ref (value);
  return H;
}

NspGParamSpec *nsp_gparamspec_create_default(const char *name)
{
 NspGParamSpec *H  = nsp_gparamspec_create_void(name,NULL);
 if ( H ==  NULLGPARAMSPEC) return NULLGPARAMSPEC;
 H->obj = NULL; 
 return H;
}

#line 451 "gparamspec.c"
/*
 * copy for gobject derived class  
 */

NspGParamSpec *nsp_gparamspec_copy_partial(NspGParamSpec *H,NspGParamSpec *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGParamSpec *nsp_gparamspec_copy(NspGParamSpec *self)
{
  NspGParamSpec *H  =nsp_gparamspec_create_void(NVOID,(NspTypeBase *) nsp_type_gparamspec);
  if ( H ==  NULLGPARAMSPEC) return NULLGPARAMSPEC;
  if ( nsp_gparamspec_copy_partial(H,self)== NULL) return NULLGPARAMSPEC;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGParamSpec *nsp_gparamspec_full_copy_partial(NspGParamSpec *H,NspGParamSpec *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_gparamspec))) == NULL) return NULLGPARAMSPEC;
  H->obj->ref_count=1;
  if( nsp_GParamSpec_full_copy(H,H->obj->value,self)== FAIL) return NULL;
  return H;
}

NspGParamSpec *nsp_gparamspec_full_copy(NspGParamSpec *self)
{
  NspGParamSpec *H  =nsp_gparamspec_create_void(NVOID,(NspTypeBase *) nsp_type_gparamspec);
  if ( H ==  NULLGPARAMSPEC) return NULLGPARAMSPEC;
  if ( nsp_gparamspec_full_copy_partial(H,self)== NULL) return NULLGPARAMSPEC;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGParamSpec
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gparamspec_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGParamSpec *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gparamspec is initialized */
  nsp_type_gparamspec = new_type_gparamspec(T_BASE);
  if(( H = nsp_gparamspec_create_void(NVOID,(NspTypeBase *) nsp_type_gparamspec)) == NULLGPARAMSPEC) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_gparamspec_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gparamspec_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gparamspec_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gparamspec_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gparamspec_func[]={
  { "gparamspec_create", int_gparamspec_create},
  { NULL, NULL}
};

/* call ith function in the gparamspec interface */

int gparamspec_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(gparamspec_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void gparamspec_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = gparamspec_func[i].name;
  *f = gparamspec_func[i].fonc;
}
void nsp_initialize_gparamspec_types(void)
{
  new_type_gparamspec(T_BASE);
}

#line 92 "codegen-3.0/gparamspec.override"

static int nsp_print_GParamSpec(int indent,GParamSpec *v,NspGParamSpec *M)
{
  Sciprintf1(indent+1,"gparamspec of type %s\n",G_PARAM_SPEC_TYPE_NAME(v));
  return 0;
}

/* we do not really make a full copy */

static int nsp_GParamSpec_full_copy(NspGParamSpec *H,GParamSpec *value,NspGParamSpec *self)
{
  H->obj->ref_count=1;
  H->obj->value = value;
  g_param_spec_ref (value);
  return OK;
}

static int nsp_destroy_GParamSpec(GParamSpec *value,NspGParamSpec *H)
{
  g_param_spec_unref (value);
  return OK;
}

static int nsp_check_GParamSpec(GParamSpec *v,NspGParamSpec *H)
{
  return OK;
}


#line 583 "gparamspec.c"
