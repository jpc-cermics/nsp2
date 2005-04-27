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

#define PMatrix_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

/*
 * NspPMatrix inherits from NspObject 
 * Polynomial matrices 
 */

int nsp_type_pmatrix_id=0;
NspTypePMatrix *nsp_type_pmatrix=NULL;
int nsp_type_pmatrix_init();

NspTypePMatrix *new_type_pmatrix(type_mode mode)
{
  NspTypePMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_pmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_pmatrix;
    }
  if (( type =  malloc(sizeof(NspTypePMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* pmatrix_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = NULL; /*pmatrix_get_methods; */
  type->new = (new_func *) new_pmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for pmatrix */ 

  top->pr = (print_func *)nsp_pmatrix_print;                    /* printing*/   
  top->dealloc = (dealloc_func *)nsp_pmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_pmatrix_copy;                   /* copy object */  
  top->size  = (size_func *) PMatSize;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *) PMatType;                /* type as a String */  
  top->sh_type = (sh_type_func *) PMatShType ;              /* type as a short string */  
  top->info = (info_func *)nsp_pmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) PMatIsTrue; */
  /* top->loop =(loop_func *) PMatLoopExtract ;   */             /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)  PMatObj ;    /* get object stored in SciObj */  
  top->eq  = (eq_func *) PMatObjEq;                       /* equality check */  
  top->neq  = (eq_func *) PMatObjNeq;                      /* non-equality check */

  top->save  = (save_func *) PMatXdrSave;
  top->load  = (load_func *) PMatXdrLoad;

  /* specific methods for pmatrix */
  type->init = (init_func *) init_pmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_pmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_pmatrix
       */
      type->id =  nsp_type_pmatrix_id = nsp_new_type_id();
      nsp_type_pmatrix = type;
      if ( nsp_register_type(nsp_type_pmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_pmatrix(mode);
    }
  else 
    {
      type->id = nsp_type_pmatrix_id;
      return type;
    }

}
/*
 * initialize Pmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_pmatrix(NspPMatrix *o,NspTypePMatrix *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Pmatrix 
 */

NspPMatrix *new_pmatrix() 
{
  NspPMatrix *loc; 
  /* type must exists */
  nsp_type_pmatrix = new_type_pmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspPMatrix)))== NULLPMAT) return loc;
  /* initialize object */
  if ( init_pmatrix(loc,nsp_type_pmatrix) == FAIL) return NULLPMAT;
  return loc;
}


/*
 * MatSize : returns m,n,or m*n 
 */

int PMatSize(NspPMatrix *Mat, int flag)
{
  switch (flag) 
    {
    case 0: return Mat->mn;
    case 1: return Mat->m;
    case 2: return Mat->n;
    }
  return 0;
}

/*
 * MatType 
 */

static char pmat_type_name[]="PMat";
static char pmat_short_type_name[]="p";

char *PMatType(void)
{
  return(pmat_type_name);
}

char *PMatShType(void)
{
  return(pmat_short_type_name);
}


int PMatFullComp(NspPMatrix * A,NspPMatrix * B,char *op,int *err)
{
  Scierror("PMatFullComp: to be implemented \n");
  return FALSE;
}



int PMatObjEq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_pmatrix_id) == FALSE) return FALSE ;
  rep = PMatFullComp((NspPMatrix *) A,(NspPMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int PMatObjNeq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_pmatrix_id) == FALSE) return TRUE;
  rep = PMatFullComp((NspPMatrix *) A,(NspPMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * Mat == TRUE ? 
 *  if Mat != [] and all the elements of Mat are != 0
 * PMatIsTrue(NspPMatrix *M)
 */


/*
 * Save a Matrix in a file stream 
 */

static int PMatXdrSave(NspFile  *F, NspMatrix *M)
{
  if (nsp_xdr_save_i(F->xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F->xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspPMatrix *PMatXdrLoad(NspFile  *F)
{
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F->xdrs,name,NAME_MAXL) == FAIL) return NULLPMAT;
  Scierror("pmat_xdr_load: to be implemented \n");
  return NULLPMAT;
}


/*
 * A = PMatObj(O);
 * checks that O is an object of NspPMatrix type. 
 * or a Hobj which points to an object of type PMatrix
 * if so, returns a pointer to that NspPMatrix and else returns NULL
 */

NspPMatrix   *PMatObj(NspObject *O)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type **/
  if ( check_cast(O,nsp_type_pmatrix_id) == TRUE) return ((NspPMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_pmatrix));
  return(NULL);
}


/*
 * IsPMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  PMatrix 
 * or a Hobj which points to an object of type PMatrix
 */

int IsPMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_pmatrix_id);
}

/*
 * IsPMat(O)
 * only checks that object is an object of type  PMatrix 
 * or a Hobj which points to an object of type PMatrix
 */

int IsPMat(NspObject *O)
{
  return nsp_object_type(O , nsp_type_pmatrix_id);
}


/*-------------------------------------------------------------------
 * wrappers for the PMatrix 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 * Matrix ( used ar row vector 1xn ) -> 1x1 polymatrix filled 
 * with one polynom of degree n-1
 */

int int_mx2pmx(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P; NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A=GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( P= Mat2Poly(A))== NULLPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

/*
 * Creation of a PMatrix 
 */

int int_pmxcreate(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  doubleC def ={0,0} ;
  NspPMatrix *P; 
  CheckRhs(1,3);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if (rhs == 3) 
    {
      if (GetScalarDouble(stack,1,&def.r) == FAIL) return RET_BUG;
    }
  if ( (P =nsp_pmatrix_create(NVOID,m1,n1,&def,(rhs==3)? 1: 0)) == NULLPMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}









