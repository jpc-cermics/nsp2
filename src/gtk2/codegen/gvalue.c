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





#line 4 "gvalue.override"
#include <nsp/gtk/gobject.h>

#line 31 "gvalue.c"

/* -----------NspGValue ----------- */


#define  NspGValue_Private 
#include <nsp/objects.h>
#include <nsp/gvalue.h>
#include <nsp/interf.h>

/* 
 * NspGValue inherits from Object 
 */

int nsp_type_gvalue_id=0;
NspTypeGValue *nsp_type_gvalue=NULL;

/*
 * Type object for NspGValue 
 * all the instance of NspTypeGValue share the same id. 
 * nsp_type_gvalue: is an instance of NspTypeGValue 
 *    used for objects of NspGValue type (i.e built with new_gvalue) 
 * other instances are used for derived classes 
 */
NspTypeGValue *new_type_gvalue(type_mode mode)
{
  NspTypeGValue *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gvalue != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gvalue;
    }
  if (( type =  malloc(sizeof(NspTypeGValue))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gvalue_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gvalue_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_gvalue;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gvalue */ 

  top->pr = (print_func *) nsp_gvalue_print;
  top->dealloc = (dealloc_func *) nsp_gvalue_destroy;
  top->copy  =  (copy_func *) nsp_gvalue_copy;
  top->size  = (size_func *) nsp_gvalue_size;
  top->s_type =  (s_type_func *) nsp_gvalue_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gvalue_type_short_string;
  top->info = (info_func *) nsp_gvalue_info;
  /* top->is_true = (is_true_func  *) nsp_gvalue_is_true; */
  /* top->loop =(loop_func *) nsp_gvalue_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gvalue_object;
  top->eq  = (eq_func *) nsp_gvalue_eq;
  top->neq  = (eq_func *) nsp_gvalue_neq;
  top->save  = (save_func *) nsp_gvalue_xdr_save;
  top->load  = (load_func *) nsp_gvalue_xdr_load;
  top->create = (create_func*) int_gvalue_create;
  top->latex = (print_func *) nsp_gvalue_latex;
  top->full_copy = (copy_func *) nsp_gvalue_full_copy;

  /* specific methods for gvalue */

  type->init = (init_func *) init_gvalue;

  /* 
   * NspGValue interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gvalue_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGValue called nsp_type_gvalue
       */
      type->id =  nsp_type_gvalue_id = nsp_new_type_id();
      nsp_type_gvalue = type;
      if ( nsp_register_type(nsp_type_gvalue) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gvalue(mode);
    }
  else 
    {
      type->id = nsp_type_gvalue_id;
      return type;
    }
}

/*
 * initialize NspGValue instances 
 * locally and by calling initializer on parent class 
 */

static int init_gvalue(NspGValue *Obj,NspTypeGValue *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  nsp_init_GValue(&Obj->value);
 return OK;
}

/*
 * new instance of NspGValue 
 */

NspGValue *new_gvalue() 
{
  NspGValue *loc;
  /* type must exists */
  nsp_type_gvalue = new_type_gvalue(T_BASE);
  if ( (loc = malloc(sizeof(NspGValue)))== NULLGVALUE) return loc;
  /* initialize object */
  if ( init_gvalue(loc,nsp_type_gvalue) == FAIL) return NULLGVALUE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGValue 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gvalue_size(NspGValue *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char gvalue_type_name[]="GValue";
static char gvalue_short_type_name[]="gvalue";

static char *nsp_gvalue_type_as_string(void)
{
  return(gvalue_type_name);
}

static char *nsp_gvalue_type_short_string(NspObject *v)
{
  return(gvalue_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gvalue_eq(NspGValue *A, NspObject *B)
{
  NspGValue *loc = (NspGValue *) B;
  if ( check_cast(B,nsp_type_gvalue_id) == FALSE) return FALSE ;
  if ( nsp_eq_GValue(&A->value,&loc->value)== FALSE) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_gvalue_neq(NspGValue *A, NspObject *B)
{
  return ( nsp_gvalue_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gvalue_xdr_save(XDR *xdrs, NspGValue *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gvalue)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGValue  *nsp_gvalue_xdr_load_partial(XDR *xdrs, NspGValue *M)
{
 return M;
}

static NspGValue  *nsp_gvalue_xdr_load(XDR *xdrs)
{
  NspGValue *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGVALUE;
  if ((H  = nsp_gvalue_create_void(name,(NspTypeBase *) nsp_type_gvalue))== NULLGVALUE) return H;
  if ( nsp_gvalue_create_partial(H) == FAIL) return NULLGVALUE;
  if ((H  = nsp_gvalue_xdr_load_partial(xdrs,H))== NULLGVALUE) return H;
  if ( nsp_gvalue_check_values(H) == FAIL) return NULLGVALUE;
  return H;
}

/*
 * delete 
 */

void nsp_gvalue_destroy_partial(NspGValue *H)
{
  nsp_destroy_GValue(&H->value,H); 
}

void nsp_gvalue_destroy(NspGValue *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gvalue_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gvalue_info(NspGValue *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGVALUE) 
    {
      Sciprintf("Null Pointer NspGValue \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gvalue_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gvalue_print(NspGValue *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGVALUE) 
    {
      Sciprintf("Null Pointer NspGValue \n");
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
          nsp_gvalue_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_gvalue_type_short_string(NSP_OBJECT(M)));
      Sciprintf1(indent+1,"{\n");
  nsp_print_GValue(indent+2,&M->value,M);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gvalue_latex(NspGValue *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gvalue_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  nsp_print_GValue(indent+2,&M->value,M);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGValue objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGValue   *nsp_gvalue_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gvalue_id)  == TRUE  ) return ((NspGValue *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gvalue));
  return NULL;
}

int IsGValueObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gvalue_id);
}

int IsGValue(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gvalue_id);
}

NspGValue  *GetGValueCopy(Stack stack, int i)
{
  if (  GetGValue(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGValue  *GetGValue(Stack stack, int i)
{
  NspGValue *M;
  if (( M = nsp_gvalue_object(NthObj(i))) == NULLGVALUE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGValue instance 
 *-----------------------------------------------------*/
#line 18 "gvalue.override"
/* override the code for gvalue creation 
 * we change the function nsp_gvalue_create 
 * since H->value = value does not work for Gvalue 
 */


static NspGValue *nsp_gvalue_create_void(const char *name,NspTypeBase *type)
{
 NspGValue *H  = (type == NULL) ? new_gvalue() : type->new();
 if ( H ==  NULLGVALUE)
  {
   Sciprintf("No more memory\n");
   return NULLGVALUE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGVALUE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gvalue_create_partial(NspGValue *H)
{
  return OK;
}

int nsp_gvalue_check_values(NspGValue *H)
{
  if ( nsp_check_GValue(&H->value,H) == FAIL ) return FAIL;
  return OK;
}

NspGValue *nsp_gvalue_create(const char *name,GValue value,NspTypeBase *type)
{
  NspGValue *H  = nsp_gvalue_create_void(name,type);
  if ( H ==  NULLGVALUE) return NULLGVALUE;
  memset(&H->value, 0, sizeof(H->value));
  g_value_init (&H->value, G_VALUE_TYPE(&value));
  g_value_copy (&value,&H->value);
  return H;
}

NspGValue *nsp_gvalue_create_default(const char *name)
{
 NspGValue *H  = nsp_gvalue_create_void(name,NULL);
 if ( H ==  NULLGVALUE) return NULLGVALUE;
 memset(&H->value, 0, sizeof(H->value));
 return H;
}

#line 419 "gvalue.c"
/*
 * copy for gobject derived class  
 */

NspGValue *nsp_gvalue_copy_partial(NspGValue *H,NspGValue *self)
{
  H->value = self->value;
  return H;
}

NspGValue *nsp_gvalue_copy(NspGValue *self)
{
  NspGValue *H  =nsp_gvalue_create_void(NVOID,(NspTypeBase *) nsp_type_gvalue);
  if ( H ==  NULLGVALUE) return NULLGVALUE;
  if ( nsp_gvalue_copy_partial(H,self)== NULL) return NULLGVALUE;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGValue *nsp_gvalue_full_copy_partial(NspGValue *H,NspGValue *self)
{
  if( nsp_GValue_full_copy(H,&H->value,self)== FAIL) return NULL;
  return H;
}

NspGValue *nsp_gvalue_full_copy(NspGValue *self)
{
  NspGValue *H  =nsp_gvalue_create_void(NVOID,(NspTypeBase *) nsp_type_gvalue);
  if ( H ==  NULLGVALUE) return NULLGVALUE;
  if ( nsp_gvalue_full_copy_partial(H,self)== NULL) return NULLGVALUE;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGValue
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

#line 68 "gvalue.override"

int int_gvalue_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGValue *H;
  int_types T[] = {s_int,t_end};
  gulong test;
  if ( GetArgs(stack,rhs,opt,T,&test) == FAIL) return RET_BUG;
  /* want to be sure that type gvalue is initialized */
  nsp_type_gvalue = new_type_gvalue(T_BASE);
  if ((H = nsp_gvalue_create_default(NVOID)) == NULLGVALUE) return RET_BUG;
  g_value_init (&H->value, G_TYPE_INT);
  g_value_set_int (&H->value, test);
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

#line 479 "gvalue.c"
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 86 "gvalue.override"

static int _wrap_get_value(NspGValue *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
  if (G_VALUE_HOLDS_INT(&self->value)) 
    {
      ret = g_value_get_int (&self->value);
      if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
    }
  else
    {
      Scierror("Error: this gvalue does not contains an int \n");
      return RET_BUG;
    }
  return 1;
}

#line 502 "gvalue.c"


static NspMethods gvalue_methods[] = {
  {"get_value",(nsp_method *) _wrap_get_value},
  { NULL, NULL}
};

static NspMethods *gvalue_get_methods(void) { return gvalue_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gvalue_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gvalue_func[]={
  { "gvalue_create", int_gvalue_create},
  { NULL, NULL}
};

/* call ith function in the gvalue interface */

int gvalue_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(gvalue_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void gvalue_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = gvalue_func[i].name;
  *f = gvalue_func[i].fonc;
}

#line 106 "gvalue.override"

static void nsp_init_GValue(GValue *value)
{
  memset(value, 0, sizeof(*value));
}

static int nsp_eq_GValue(GValue *v1, GValue *v2)
{
  return TRUE;
}

static int nsp_print_GValue(int indent,GValue *v,NspGValue *M)
{
  
  return 0;
}

static int nsp_GValue_full_copy(NspGValue *H,GValue *value,NspGValue *self)
{
  /* v is not initialized */
  memset(value, 0, sizeof(*value));
  g_value_init (value, G_VALUE_TYPE(&self->value));
  g_value_copy (&self->value,value);
  return OK;
}

static int nsp_destroy_GValue(GValue *value,NspGValue *H)
{
  g_value_unset (value);
  return OK;
}

static int nsp_check_GValue(GValue *v,NspGValue *H)
{
  return OK;
}







#line 590 "gvalue.c"
