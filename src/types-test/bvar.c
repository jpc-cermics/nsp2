/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2011 Jean-Philippe Chancelier Enpc/Cermics
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





#line 20 "codegen/bvar.override"
#include <nsp/objects.h>
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/parse.h>

#line 34 "bvar.c"

/* ----------- NspBvar ----------- */


#define  NspBvar_Private 
#include <nsp/objects.h>
#include <nsp/bvar.h>
#include <nsp/interf.h>

/* 
 * NspBvar inherits from Object 
 */

int nsp_type_bvar_id=0;
NspTypeBvar *nsp_type_bvar=NULL;

/*
 * Type object for NspBvar 
 * all the instance of NspTypeBvar share the same id. 
 * nsp_type_bvar: is an instance of NspTypeBvar 
 *    used for objects of NspBvar type (i.e built with new_bvar) 
 * other instances are used for derived classes 
 */
NspTypeBvar *new_type_bvar(type_mode mode)
{
  NspTypeBvar *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_bvar != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_bvar;
    }
  if (( type =  malloc(sizeof(NspTypeBvar))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = bvar_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = bvar_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_bvar;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for bvar */ 

  top->pr = (print_func *) nsp_bvar_print;
  top->dealloc = (dealloc_func *) nsp_bvar_destroy;
  top->copy  =  (copy_func *) nsp_bvar_copy;
  top->size  = (size_func *) nsp_bvar_size;
  top->s_type =  (s_type_func *) nsp_bvar_type_as_string;
  top->sh_type = (sh_type_func *) nsp_bvar_type_short_string;
  top->info = (info_func *) nsp_bvar_info;
  /* top->is_true = (is_true_func  *) nsp_bvar_is_true; */
  /* top->loop =(loop_func *) nsp_bvar_loop;*/
#line 120 "codegen/bvar.override"
top->path_extract = (path_func *) NULL; /* path extract as for matrix type */

#line 96 "bvar.c"
  top->get_from_obj = (get_from_obj_func *) nsp_bvar_object;
  top->eq  = (eq_func *) nsp_bvar_eq;
  top->neq  = (eq_func *) nsp_bvar_neq;
  top->save  = (save_func *) nsp_bvar_xdr_save;
  top->load  = (load_func *) nsp_bvar_xdr_load;
  top->create = (create_func*) int_bvar_create;
  top->latex = (print_func *) nsp_bvar_latex;
  top->full_copy = (copy_func *) nsp_bvar_full_copy;

  /* specific methods for bvar */

  type->init = (init_func *) init_bvar;

  /* 
   * NspBvar interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_bvar_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeBvar called nsp_type_bvar
       */
      type->id =  nsp_type_bvar_id = nsp_new_type_id();
      nsp_type_bvar = type;
      if ( nsp_register_type(nsp_type_bvar) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_bvar(mode);
    }
  else 
    {
      type->id = nsp_type_bvar_id;
      return type;
    }
}

/*
 * initialize NspBvar instances 
 * locally and by calling initializer on parent class 
 */

static int init_bvar(NspBvar *Obj,NspTypeBvar *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->sym = FALSE;
  Obj->value = NULLOBJ;
  Obj->varname = nsp_new_string("",-1);
  return OK;
}

/*
 * new instance of NspBvar 
 */

NspBvar *new_bvar() 
{
  NspBvar *loc;
  /* type must exists */
  nsp_type_bvar = new_type_bvar(T_BASE);
  if ( (loc = malloc(sizeof(NspBvar)))== NULLBVAR) return loc;
  /* initialize object */
  if ( init_bvar(loc,nsp_type_bvar) == FAIL) return NULLBVAR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspBvar 
 *-----------------------------------------------*/
#line 135 "codegen/bvar.override"

/*
 * size can be overriden here
 */

static int nsp_bvar_size(NspBvar *Mat, int flag)
{
  /* redirect the size value to Mat->value */
  if ( Mat->value == NULL) 
    return 0;
  else
    return nsp_object_get_size(Mat->value,flag);
}

#line 185 "bvar.c"
/*
 * type as string 
 */

static char bvar_type_name[]="Bvar";
static char bvar_short_type_name[]="bvar";

static char *nsp_bvar_type_as_string(void)
{
  return(bvar_type_name);
}

static char *nsp_bvar_type_short_string(NspObject *v)
{
  return(bvar_short_type_name);
}

#line 184 "codegen/bvar.override"

/*
 * A == B 
 */

static int nsp_bvar_eq(NspBvar *A, NspObject *B)
{
  NspBvar *loc = (NspBvar *) B;
  if ( check_cast(B,nsp_type_bvar_id) == FALSE) return FALSE ;
  if ( A->value == NULL && loc->value == NULL) return TRUE;
  if ( A->value != NULL && loc->value != NULL) 
    {
      if ( NSP_OBJECT(A->value)->type->eq(A->value,loc->value) == FALSE ) return FALSE;
    }
  else
    {
      return FALSE;
    }
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_bvar_neq(NspBvar *A, NspObject *B)
{
  return ( nsp_bvar_eq(A,B) == TRUE ) ? FALSE : TRUE;
}


#line 235 "bvar.c"
int nsp_bvar_xdr_save(XDR *xdrs, NspBvar *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_bvar)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspBvar  *nsp_bvar_xdr_load_partial(XDR *xdrs, NspBvar *M)
{
 return M;
}

static NspBvar  *nsp_bvar_xdr_load(XDR *xdrs)
{
  NspBvar *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLBVAR;
  if ((H  = nsp_bvar_create_void(name,(NspTypeBase *) nsp_type_bvar))== NULLBVAR) return H;
  if ( nsp_bvar_create_partial(H) == FAIL) return NULLBVAR;
  if ((H  = nsp_bvar_xdr_load_partial(xdrs,H))== NULLBVAR) return H;
  if ( nsp_bvar_check_values(H) == FAIL) return NULLBVAR;
#line 40 "codegen/bvar.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 266 "bvar.c"
  return H;
}

/*
 * delete 
 */

void nsp_bvar_destroy_partial(NspBvar *H)
{
#line 43 "codegen/bvar.override"
/* verbatim in destroy */

#line 279 "bvar.c"
  if ( H->value != NULL ) 
    nsp_object_destroy(&H->value);
  nsp_string_destroy(&(H->varname));
}

void nsp_bvar_destroy(NspBvar *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_bvar_destroy_partial(H);
  FREE(H);
}

#line 124 "codegen/bvar.override"
/*
 * info overriden 
 */

int nsp_bvar_info(NspBvar *M, int indent,const char *name, int rec_level)
{
  return nsp_bvar_print(M,indent,name,rec_level);
}

#line 302 "bvar.c"
#line 151 "codegen/bvar.override"
/*
 * print overriden 
 */

int nsp_bvar_print(NspBvar *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if (user_pref.pr_as_read_syntax)
    {
      if ( strcmp(pname,NVOID) != 0) 
	{
	  nsp_object_print(M->value,indent,"_val",rec_level);
	  Sciprintf1(indent,"%s=bvar(sym=%s,varname=\"%s\",value=_val);",pname,
		     (M->sym == TRUE) ? "%t" : "%f");
	}
      else 
	{
	  nsp_object_print(M->value,indent,"_val",rec_level);
	  Sciprintf1(indent,"%s=bvar(sym=%s,varname=\"%s\",value=_val);",pname,
		     (M->sym == TRUE) ? "%t" : "%f");
	}
    }
  else 
    {
      Sciprintf1(indent,"%s\t= \"%s\",%s,%s\n",pname,M->varname, (M->sym == TRUE ) ? "%t" : "%f",
		 nsp_object_type_as_string(M->value));
    }
  return TRUE;
}


#line 335 "bvar.c"
/*
 * latex print 
 */

int nsp_bvar_latex(NspBvar *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_bvar_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"sym	= %s\n", ( M->sym == TRUE) ? "T" : "F" );
        if ( M->value->type->pr(M->value,indent+2,"value",rec_level+1)==FALSE) return FALSE;
  Sciprintf1(indent+2,"varname=%s\n",M->varname);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspBvar objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspBvar   *nsp_bvar_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_bvar_id) == TRUE ) return ((NspBvar *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_bvar));
  return NULL;
}

int IsBvarObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_bvar_id);
}

int IsBvar(NspObject *O)
{
  return nsp_object_type(O,nsp_type_bvar_id);
}

NspBvar  *GetBvarCopy(Stack stack, int i)
{
  if (  GetBvar(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspBvar  *GetBvar(Stack stack, int i)
{
  NspBvar *M;
  if (( M = nsp_bvar_object(NthObj(i))) == NULLBVAR)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspBvar instance 
 *-----------------------------------------------------*/

static NspBvar *nsp_bvar_create_void(const char *name,NspTypeBase *type)
{
 NspBvar *H  = (type == NULL) ? new_bvar() : type->new();
 if ( H ==  NULLBVAR)
  {
   Sciprintf("No more memory\n");
   return NULLBVAR;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLBVAR;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_bvar_create_partial(NspBvar *H)
{
  return OK;
}

int nsp_bvar_check_values(NspBvar *H)
{
  if ( H->value == NULLOBJ) 
    {
     if (( H->value =(NspObject*) nsp_matrix_create("value",'r',0,0)) == NULL)
       return FAIL;
    }
  if ( H->varname == NULL) 
    {
     if (( H->varname = nsp_string_copy("")) == NULL)
       return FAIL;
    }
  return OK;
}

NspBvar *nsp_bvar_create(const char *name,gboolean sym,NspObject* value,char* varname,NspTypeBase *type)
{
  NspBvar *H  = nsp_bvar_create_void(name,type);
  if ( H ==  NULLBVAR) return NULLBVAR;
  H->sym=sym;
  H->value= value;
  H->varname = varname;
  if ( nsp_bvar_check_values(H) == FAIL) return NULLBVAR;
#line 40 "codegen/bvar.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 443 "bvar.c"
  return H;
}


NspBvar *nsp_bvar_create_default(const char *name)
{
 NspBvar *H  = nsp_bvar_create_void(name,NULL);
 if ( H ==  NULLBVAR) return NULLBVAR;
 if ( nsp_bvar_check_values(H) == FAIL) return NULLBVAR;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspBvar *nsp_bvar_copy_partial(NspBvar *H,NspBvar *self)
{
  H->sym=self->sym;
  if ( self->value == NULL )
    { H->value = NULL;}
  else
    {
      if ((H->value = (NspObject *) nsp_object_copy_and_name("value",NSP_OBJECT(self->value))) == NULLOBJ) return NULL;
    }
  if ((H->varname = nsp_string_copy(self->varname)) == NULL) return NULL;
  return H;
}

NspBvar *nsp_bvar_copy(NspBvar *self)
{
  NspBvar *H  =nsp_bvar_create_void(NVOID,(NspTypeBase *) nsp_type_bvar);
  if ( H ==  NULLBVAR) return NULLBVAR;
  if ( nsp_bvar_copy_partial(H,self)== NULL) return NULLBVAR;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspBvar *nsp_bvar_full_copy_partial(NspBvar *H,NspBvar *self)
{
  H->sym=self->sym;
  if ( self->value == NULL )
    { H->value = NULL;}
  else
    {
      if ((H->value = (NspObject *) nsp_object_full_copy_and_name("value",NSP_OBJECT(self->value))) == NULLOBJ) return NULL;
    }
  if ((H->varname = nsp_string_copy(self->varname)) == NULL) return NULL;
  return H;
}

NspBvar *nsp_bvar_full_copy(NspBvar *self)
{
  NspBvar *H  =nsp_bvar_create_void(NVOID,(NspTypeBase *) nsp_type_bvar);
  if ( H ==  NULLBVAR) return NULLBVAR;
  if ( nsp_bvar_full_copy_partial(H,self)== NULL) return NULLBVAR;

#line 40 "codegen/bvar.override"
/* verbatim in create/load/full_copy interface use NULL for returned value */
#line 506 "bvar.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspBvar
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_bvar_create(Stack stack, int rhs, int opt, int lhs)
{
  NspBvar *H;
  CheckStdRhs(0,0);
  /* want to be sure that type bvar is initialized */
  nsp_type_bvar = new_type_bvar(T_BASE);
  if(( H = nsp_bvar_create_void(NVOID,(NspTypeBase *) nsp_type_bvar)) == NULLBVAR) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_bvar_check_values(H) == FAIL) return RET_BUG;
#line 40 "codegen/bvar.override"
/* verbatim in create/load/full_copy interface use RET_BUG for returned value */
#line 527 "bvar.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 47 "codegen/bvar.override"
/* override a method */
static int _wrap_bvar_get_value(NspBvar *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1); 
  MoveObj(stack,1, self->value);
  return Max(lhs,1);
}

#line 545 "bvar.c"


#line 58 "codegen/bvar.override"
/* override a method */
static int _wrap_bvar_set_value(NspBvar *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  CheckRhs(1,1);
  CheckLhs(0,1); 
  if ((Obj = nsp_object_copy_and_name("ud",NthObj(1))) == NULLOBJ) 
    return RET_BUG;
  if ( self->value != NULL )
    {
      nsp_object_destroy(&self->value);
    }
  self->value = Obj;
  return 0;
}

#line 565 "bvar.c"


#line 76 "codegen/bvar.override"

static int _wrap_bvar_get_varname(NspBvar *self,Stack stack,int rhs,int opt,int lhs)
{
  if ( nsp_move_string(stack,1,self->varname,-1) ==FAIL)   return RET_BUG;
  return 1;
}

#line 576 "bvar.c"


#line 85 "codegen/bvar.override"

static int _wrap_bvar_set_varname(NspBvar *self,Stack stack,int rhs,int opt,int lhs)
{
  char *str;
  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((str = nsp_string_copy(str)) ==NULL) return RET_BUG;
  free(self->varname);
  self->varname=str;
  return 0;
}

#line 591 "bvar.c"


#line 98 "codegen/bvar.override"

static int _wrap_bvar_is_symbolic(NspBvar *self,Stack stack,int rhs,int opt,int lhs)
{
  if ( nsp_move_boolean(stack,1,(self->sym == TRUE))==FAIL) return RET_BUG;
  return 1;
}

#line 602 "bvar.c"


#line 107 "codegen/bvar.override"

static int _wrap_bvar_set_symbolic(NspBvar *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,t_end};
  int status;
  if ( GetArgs(stack,rhs,opt,T,&status) == FAIL) return RET_BUG;
  self->sym = status;
  return 0;
}


#line 617 "bvar.c"


static NspMethods bvar_methods[] = {
  {"get_value",(nsp_method *) _wrap_bvar_get_value},
  {"set_value",(nsp_method *) _wrap_bvar_set_value},
  {"get_varname",(nsp_method *) _wrap_bvar_get_varname},
  {"set_varname",(nsp_method *) _wrap_bvar_set_varname},
  {"is_symbolic",(nsp_method *) _wrap_bvar_is_symbolic},
  {"set_symbolic",(nsp_method *) _wrap_bvar_set_symbolic},
  { NULL, NULL}
};

static NspMethods *bvar_get_methods(void) { return bvar_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab bvar_attrs[] = {{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Bvar_func[]={
  { "bvar_create", int_bvar_create},
  { NULL, NULL}
};

/* call ith function in the Bvar interface */

int Bvar_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Bvar_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Bvar_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Bvar_func[i].name;
  *f = Bvar_func[i].fonc;
}

#line 217 "codegen/bvar.override"

NspBvar *nsp_bvar(NspObject *Obj,int sym)
{
  NspObject *O1;
  NspBvar *H;
  /* want to be sure that type bvar is initialized */
  nsp_type_bvar = new_type_bvar(T_BASE);
  if(( H = nsp_bvar_create_void(NVOID,(NspTypeBase *) nsp_type_bvar)) == NULLBVAR) return NULL;
  if ((O1 = nsp_object_copy_and_name("ud",Obj)) == NULLOBJ) 
    {
      nsp_bvar_destroy(H);
      return NULL;
    }
  if ( H->value != NULL )
    {
      nsp_object_destroy(&H->value);
    }
  H->value = O1;
  H->sym = sym;
  return H;
}



#line 691 "bvar.c"
