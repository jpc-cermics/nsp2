/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SpMatrix_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

/*
 * NspSpMatrix inherits from NspObject 
 */

int nsp_type_spmatrix_id=0;
NspTypeSpMatrix *nsp_type_spmatrix=NULL;
int nsp_type_spmatrix_init();

NspTypeSpMatrix *new_type_spmatrix(type_mode mode)
{
  NspTypeSpMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_spmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_spmatrix;
    }
  if ((type =  malloc(sizeof(NspTypeSpMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* spmatrix_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = NULL; /*spmatrix_get_methods; */
  type->new = (new_func *) new_spmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for spmatrix */ 

  top->pr = (print_func *)nsp_spmatrix_print;                  /* printing*/   
  top->dealloc = (dealloc_func *)nsp_spmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_spmatrix_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_spmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_spmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_spmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_spmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) SpMatIsTrue;*/      /* check if object can be considered as true */  
  /*top->loop =(loop_func *) SpLoopExtract ; */               /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_spmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_spmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_spmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_spmatrix_xdr_save;
  top->load  = (load_func *)nsp_spmatrix_xdr_load;

  /* specific methods for spmatrix */
  type->init = (init_func *) init_spmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_spmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_spmatrix
       */
      type->id =  nsp_type_spmatrix_id = nsp_new_type_id();
      nsp_type_spmatrix = type;
      if ( nsp_register_type(nsp_type_spmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_spmatrix(mode); 
    }
  else 
    {
      type->id = nsp_type_spmatrix_id;
      return type;
    }
}
/*
 * initialize Spmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_spmatrix(NspSpMatrix *o,NspTypeSpMatrix *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Spmatrix 
 */

NspSpMatrix *new_spmatrix() 
{
  NspSpMatrix *loc; 
  /* type must exists */
  nsp_type_spmatrix = new_type_spmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspSpMatrix)))== NULLSP) return loc;
  /* initialize object */
  if ( init_spmatrix(loc,nsp_type_spmatrix) == FAIL) return NULLSP;
  return loc;
}


/*
 *nsp_spmatrix_size: returns filled,hsize,or hsize 
 */

int nsp_spmatrix_size(NspSpMatrix *H, int flag)
{
  switch (flag) 
    {
    case 0: return H->mn;
    case 1: return H->m;
    case 2: return H->n;
    }
  return 0;
}

/*
 * MatType 
 */

static char sp_type_name[]="SpMat";
static char sp_short_type_name[]="sp";

char *nsp_spmatrix_type_as_string(void)
{
  return(sp_type_name);
}

char *nsp_spmatrix_type_short_string(void)
{
  return(sp_short_type_name);
}


int SpMatFullComp(NspSpMatrix * A,NspSpMatrix * B,char *op,int *err)
{
  Scierror("SpMatFullComp: to be implemented \n");
  return FALSE;
}


int nsp_spmatrix_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_spmatrix_id) == FALSE) return FALSE ;
  rep = SpMatFullComp((NspSpMatrix *) A,(NspSpMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_spmatrix_neq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_spmatrix_id) == FALSE) return TRUE;
  rep = SpMatFullComp((NspSpMatrix *) A,(NspSpMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}



/*
 * Save a Matrix in a file stream 
 */

static int nsp_spmatrix_xdr_save(NspFile  *F, NspSpMatrix *M)
{
  NspMatrix *RC, *Values;
  if ( nsp_spmatrix_get(M,&RC,&Values) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F, M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F, M->n) == FAIL) return FAIL;
  if ( NSP_OBJECT(RC)->type->save (F,RC) == FAIL) return FAIL;
  if ( NSP_OBJECT(Values)->type->save (F,Values) == FAIL) return FAIL;
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspSpMatrix *nsp_spmatrix_xdr_load(NspFile  *F)
{
  int m,n;
  NspObject *RC, *Values;
  NspSpMatrix *Loc;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F,name,NAME_MAXL) == FAIL) return NULLSP;
  if (nsp_xdr_load_i(F, &m) == FAIL)  return NULLSP;
  if (nsp_xdr_load_i(F, &n) == FAIL)  return NULLSP;
  if ( (RC= nsp_object_xdr_load(F) ) == NULL) return NULLSP;
  if ( (Values= nsp_object_xdr_load(F) ) == NULL) return NULLSP;
  if ((Loc = nsp_spmatrix_sparse(name,(NspMatrix *)RC,(NspMatrix *)Values,m,n)) == NULLSP) return NULLSP;
  return Loc;
}


/*
 * A =nsp_spmatrix_object(O);
 * checks that O is an object of NspSpMatrix type. 
 * or a Hobj which points to an object of type SpMatrix
 * if so, returns a pointer to that NspSpMatrix and else returns NULL
 */

NspSpMatrix   *nsp_spmatrix_object(NspObject *O)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type **/
  if ( check_cast(O,nsp_type_spmatrix_id) == TRUE) return ((NspSpMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_spmatrix));
  return(NULL);
}


/*
 * IsSpMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  SpMatrix 
 * or a Hobj which points to an object of type SpMatrix
 */

int IsSpMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_spmatrix_id);
}

/*
 * IsSpMat(O)
 * only checks that object is an object of type  SpMatrix 
 * or a Hobj which points to an object of type SpMatrix
 */

int IsSpMat(NspObject *O)
{
  return nsp_object_type(O , nsp_type_spmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a Sp and returns that Sp  
 * or a copy of that Sp if its name 
 * is != NVOID 
 */

NspSpMatrix *GetSpCopy(Stack stack, int i)
{
  if (  GetSp(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a SpMatrix and returns that SpMatrix  
 */

NspSpMatrix *GetSp(Stack stack, int i)
{
  NspSpMatrix *M;
  if (( M =nsp_spmatrix_object(NthObj(i))) == NULLSP )
     ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a SpMatrix and returns that SpMatrix  
 */

NspSpMatrix *GetRealSp(Stack stack, int i)
{
  NspSpMatrix *M;
  if (( M =nsp_spmatrix_object(NthObj(i))) == NULLSP )
     ArgMessage(stack,i);
  if (M->rc_type == 'i')
    {
      Scierror ("\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should not be complex\n", stack.fname);
      return NULLSP;
    }
  return M;
}

