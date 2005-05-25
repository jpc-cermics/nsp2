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
#include "nsp/matint.h"

/*
 * NspPMatrix inherits from NspObject 
 * Polynomial matrices 
 */

int nsp_type_pmatrix_id=0;
NspTypePMatrix *nsp_type_pmatrix=NULL;
int nsp_type_pmatrix_init();

NspTypePMatrix *new_type_pmatrix(type_mode mode)
{
  NspTypeMatint *mati;/* interface */
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
  top->size  = (size_func *)nsp_pmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_pmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_pmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_pmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) PMatIsTrue; */
  /* top->loop =(loop_func *)nsp_pmatrix_loop_extract;   */             /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_pmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_pmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_pmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_pmatrix_xdr_save;
  top->load  = (load_func *)nsp_pmatrix_xdr_load;

  /* specific methods for pmatrix */
  type->init = (init_func *) init_pmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods; 
  mati->redim = (matint_redim *) nsp_pmatrix_redim; 
  mati->resize = (matint_resize  *) nsp_pmatrix_resize; 
  mati->free_elt = (matint_free_elt *) nsp_polynom_destroy;
  mati->elt_size = (matint_elt_size *) nsp_pmatrix_elt_size ;

  type->interface = (NspTypeBase *) mati;
  
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

int nsp_pmatrix_size(NspPMatrix *Mat, int flag)
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

char *nsp_pmatrix_type_as_string(void)
{
  return(pmat_type_name);
}

char *nsp_pmatrix_type_short_string(void)
{
  return(pmat_short_type_name);
}


int PMatFullComp(NspPMatrix * A,NspPMatrix * B,char *op,int *err)
{
  Scierror("PMatFullComp: to be implemented \n");
  return FALSE;
}



int nsp_pmatrix_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_pmatrix_id) == FALSE) return FALSE ;
  rep = PMatFullComp((NspPMatrix *) A,(NspPMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_pmatrix_neq(NspObject *A, NspObject *B)
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

static int nsp_pmatrix_xdr_save(NspFile  *F, NspMatrix *M)
{
  if (nsp_xdr_save_i(F->xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F->xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("pmat_xdr_save: to be implemented \n");
  return OK;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspPMatrix *nsp_pmatrix_xdr_load(NspFile  *F)
{
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(F->xdrs,name,NAME_MAXL) == FAIL) return NULLPMAT;
  Scierror("pmat_xdr_load: to be implemented \n");
  return NULLPMAT;
}


/*
 * A =nsp_pmatrix_object(O);
 * checks that O is an object of NspPMatrix type. 
 * or a Hobj which points to an object of type PMatrix
 * if so, returns a pointer to that NspPMatrix and else returns NULL
 */

NspPMatrix   *nsp_pmatrix_object(NspObject *O)
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

/*
 * Checks that first+i object on the stack 
 * is a NspPMatrix and returns that NspPMatrix  
 * or a copy of that NspPMatrix if its name 
 * is != NVOID 
 */

NspPMatrix*GetPMatCopy(Stack stack, int i)
{
  if (  GetPMat(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a NspPMatrix and returns that NspPMatrix  
 */

NspPMatrix*GetPMat(Stack stack, int i)
{
  NspPMatrix *M;
  if (( M =nsp_pmatrix_object(NthObj(i))) == NULLPMAT  )
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i objects on the stack 
 * is a spolynom and returns a pointer to that string
 */

nsp_polynom GetPolynom(Stack stack, int i)
{
  NspPMatrix *M;
  if (( M =nsp_pmatrix_object(NthObj(i))) == NULLPMAT 
      || ( M->mn != 1 ))
    {
      Scierror("Error:\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should be a string\n",stack.fname);
      return NULLPOLY ;
    }
  return M->S[0];
}

/*-------------------------------------------------------------------
 * wrappers for the PMatrix 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 * Matrix ( used ar row vector 1xn ) -> 1x1 polymatrix filled 
 * with one polynom of degree n-1
 */

static int int_matrix_to_pmatrix(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *P; NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A=GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( P=nsp_matrix_to_polynom(A))== NULLPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

/*
 * Creation of a PMatrix 
 */

static int int_pmatrix_create(Stack stack, int rhs, int opt, int lhs)
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

/*
 * nsp_smatrix_redim: Changes matrix dimensions
 * m*n must be unchanged 
 * The NspSMatrix is changed (m,n are changed ) 
 * return 0 on failure 
 */

int int_pmatrix_redim(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspPMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetPMat(stack,1))== NULLPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_redim(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * changes a copy of matrix stack object to column vector 
 */

int int_pmatrix_2vect (Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((HMat = GetPMatCopy (stack, 1)) == NULLPMAT) return RET_BUG;
  if (nsp_pmatrix_redim (HMat, HMat->mn, 1) != OK) return RET_BUG;
  NSP_OBJECT (HMat)->ret_pos = 1;
  return 1;
}


/*
 * Right Concatenation 
 * A= [A,B] 
 * return 0 on failure ( incompatible size or No more space )
 */

int int_pmatrix_concatr(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetPMat(stack,1))  == NULLPMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetPMatCopy(stack,1))  == NULLPMAT) return RET_BUG;
      if (nsp_pmatrix_concat_right(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  return 1;
}

/*
 * Right Concatenation 
 * Res = [A,B]  when A is a scalar matrix 
 * usefull when A=[]
 */

int int_pmatrix_concatr_m_s(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_pmatrix(HMat1,NULL,0)) == NULLPMAT) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if (nsp_pmatrix_concat_right(Res,HMat2)!= OK) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

typedef NspPMatrix * (*FSconcat) (const NspPMatrix *,const NspPMatrix *);

int int_pmatrix__concat(Stack stack, int rhs, int opt, int lhs, FSconcat F)
{
  NspPMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetPMat(stack,1))  == NULLPMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      NspPMatrix *HMat3;
      if (( HMat3 = (*F)(HMat1,HMat2)) == NULLPMAT)  return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

int int_pmatrix_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix__concat(stack,rhs,opt,lhs,nsp_pmatrix_concat_down);
}


/*
 * Down Concatenation 
 * Res = [A;B]  when A is a scalar matrix 
 * usefull when A=[]
 */

int int_pmatrix_concatd_m_s(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_pmatrix(HMat1,NULL,0)) == NULLPMAT) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if ((Res=nsp_pmatrix_concat_down(Res,HMat2))== NULLPMAT ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Diag Concatenation
 * Res = [A,0;0,B]
 * return NULLMAT on failure ( No more space )
 * A and B are left unchanged
 */

int int_pmatrix_concatdiag(Stack stack, int rhs, int opt, int lhs)
{
  Sciprintf("smxconcatdiag: A FAIRE XXXX");
  /*
    return int_pmatrix__concat(stack,rhs,opt,lhs,nsp_pmatrix_concat_diag); */
  return 0;
}

/*
 *nsp_pmatrix_add_columns: add n cols of zero to NspPMatrix A 
 * A= [A,ones(m,n)] 
 * return 0 on failure ( No more space )
 * A is changed 
 */

int int_pmatrix_addcols(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspPMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetPMatCopy(stack,1))== NULLPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * AddRows : Add m rows of zero to a NspPMatrix A 
 * A = [A;ones(m,n)]
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

int int_pmatrix_addrows(Stack stack, int rhs, int opt, int lhs)
{
  int m1;
  NspPMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetPMatCopy(stack,1))== NULLPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *            and the adress of A must not be changed 
 *  =======
 *  A can be a String Matrix 
 *  A(x)=B is not allowed when A and B do not have the same type 
 */

int int_pmatrix_setrc(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B;
  NspMatrix *Rows,*Cols=NULLMAT;
  CheckRhs(3,4);
  CheckLhs(1,1);
  if ( IsSMatObj(stack,1)  ) 
    {
      /* A is string matrix **/
      if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
    }
  else 
    {
      Scierror("Error: A(...)= B, A and B must be of the same type\n");
      return RET_BUG;
    }
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Rows is boolean : use find(Rows) **/
      NspBMatrix *BRows ;
      if ((BRows = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Rows =nsp_bmatrix_find(BRows)) == NULLMAT) return RET_BUG;
    }
  else
    {
      /* Rows is a real matrix **/
      if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }
  if ( rhs == 4 )
    {
      /* Cols is boolean : use find(Cols) **/
      if ( IsBMatObj(stack,3)  ) 
	{
	  NspBMatrix *BCols ;
	  if ((BCols = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
	  if ((Cols =nsp_bmatrix_find(BCols)) == NULLMAT) return RET_BUG;
	}  
      else
	{
	  if ((Cols = GetRealMat(stack,3)) == NULLMAT ) return RET_BUG;
	}
    }
  /* last argument is B a String NspMatrix **/
  if ((B = GetPMat(stack,rhs)) == NULLPMAT) return RET_BUG;
  if ( B == A) 
    {
      if ((B = GetPMatCopy(stack,rhs)) == NULLPMAT) return RET_BUG;
    }
  if ( rhs == 3 )
    {  if ( nsp_pmatrix_set_rows( A, Rows,B) != OK) return RET_BUG; }
  else
    {  if ( nsp_pmatrix_set_submatrix( A, Rows,Cols,B) != OK) return RET_BUG;}


  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * Res=BMatDeletecols(A,Cols)
 *     Cols unchanged  ( restored at end of function if necessary)
 *     WARNING: A must be changed by this routine
 */

typedef int (*delf) (NspPMatrix *M,NspMatrix *Elts);

static int int_pmatrix_deleteelts_gen(Stack stack, int rhs, int opt, int lhs, delf F)
{
  int alloc=FALSE;
  NspPMatrix *A;
  NspBMatrix *BElts=NULLBMAT;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Elts is boolean: use find(Elts) **/
      if ((BElts = GetBMat(stack,2)) == NULLBMAT) 
	return RET_BUG;
      if ((Elts =nsp_bmatrix_find(BElts)) == NULLMAT) 
	return RET_BUG;
      alloc=TRUE;
    }
  else
    {
      if ((Elts = GetRealMat(stack,2)) == NULLMAT) 
	return RET_BUG;
    }
  if ( (*F)( A, Elts) == FAIL )
    {
      if ( alloc ) nsp_matrix_destroy(Elts) ;
      return RET_BUG;
    }
  /* take care that A and Elts can be the same */
  /* if ( A == BElts ) NthObj(2)=NULLOBJ; */
  NSP_OBJECT(A)->ret_pos = 1;
  if ( alloc ) nsp_matrix_destroy(Elts) ;
  return 1;
}

static int int_pmatrix_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_deleteelts_gen(stack,rhs,opt,lhs,
				    (delf) nsp_smatrix_delete_columns);
}

/*
 * Res=BMatDeleterows(A,Rows)
 *     Rows unchanged  ( restored at end of function if necessary)
 * WARNING: A must be changed by this routine
 */

static int int_pmatrix_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_deleteelts_gen(stack,rhs,opt,lhs,
				    (delf) nsp_smatrix_delete_rows);
}

/*
 * Res=BMatDeleteelts(A,Elts)
 *     Elts unchanged  ( restored at end of function if necessary)
 *     redirect to  nsp_smatrix_delete_elements which is generic now 
 */

static int int_pmatrix_deleteelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_deleteelts_gen(stack,rhs,opt,lhs,
				    (delf) nsp_smatrix_delete_elements);
}

/*
 * Res=nsp_pmatrix_extract(A,Rows,Cols)
 * A unchanged, Rows and Cols are changed (i.e converted to int) 
 * 
 */	

int int_pmatrix_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*Res; 
  NspMatrix *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((Rows = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetMat(stack,3)) == NULLMAT) return RET_BUG;
  Res =nsp_pmatrix_extract( A, Rows,Cols);
  if ( Res == NULLPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res=nsp_matrix_extract_elements(Elts,A)
 * A unchanged, Elts
 */	

/* generic function for elts extraction */

typedef NspPMatrix * (*extrf) (NspPMatrix*M,NspMatrix *Elts,int *err);

int int_pmatrix_extractelts_gen(Stack stack, int rhs, int opt, int lhs, extrf F)
{
  int err;
  NspPMatrix *A,*Res;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;

  if ( IsBMatObj(stack,2)  ) 
    {
      /* Elts is boolean : use find(Elts) **/
      NspBMatrix *BElts;
      if ((BElts = GetBMat(stack,2)) == NULLBMAT) return RET_BUG;
      if ((Elts =nsp_bmatrix_find(BElts)) == NULLMAT) return RET_BUG;
    }
  else
    {
      /* Elts is a real matrix  **/
      if ((Elts = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
    }

  Res = (*F)( A, Elts,&err);
  if ( err == 1) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return RET_BUG;
    }
  if ( Res  == NULLPMAT) return RET_BUG;

  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_extractelts_gen(stack,rhs,opt,lhs,nsp_pmatrix_extract_elements);
}

/*
 * columns extraction  Cols A --> A(Cols)
 */	

int int_pmatrix_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_extractelts_gen(stack,rhs,opt,lhs,nsp_pmatrix_extract_columns);
}

/*
 * rows extraction 					   
 */	

int int_pmatrix_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_extractelts_gen(stack,rhs,opt,lhs,nsp_pmatrix_extract_rows);
}

/*
 * columns extraction for do loop
 * Cols A --> (Cols,A,Cols(A))
 */

int int_pmatrix_extractcolforloop(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*Res;
  NspMatrix *Cols;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(3,3);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((Cols = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_pmatrix_extract_columns( A,Cols,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLPMAT) return RET_BUG;
  NthObj(3) = (NspObject *) Res;
  return 3;
}

/*
 *nsp_pmatrix_resize: Changes NspPMatrix dimensions
 * Warning : this routine only enlarges the array 
 * of the NspPMatrix storage so as to contain mxn 
 * elements : the previous datas are not moved and 
 * occupy the first array cells 
 * The NspPMatrix is changed 
 * return 0 on failure 
 */

int int_pmatrix_resize(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspPMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetPMatCopy(stack,1))== NULLPMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 *nsp_pmatrix_enlarge(A,m,n) 
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for "." strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 *  The result is stored in A 
 * WARNING : no copy 
 */

int int_pmatrix_enlarge(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A;
  int m1,n1;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if (GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_pmatrix_enlarge(A,m1,n1)== FAIL)  return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}



/*
 * Operation leading to Boolean result 
 */

/* A < B */ 

int int_pmatrix_lt(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,"<");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_le(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,"<=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B;
  NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,"==");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_gt(Stack stack, int rhs, int opt, int lhs)
{

  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,">");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


int int_pmatrix_ge(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  Res = PMatCompOp(A,B,">=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean 
 */

static int int_pmatrix_f_gen(Stack stack, int rhs, int opt, int lhs,char *op)
{
  int rep,err;
  NspPMatrix *A,*B; NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if ((B = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
  rep = PMatFullComp(A,B,op,&err);
  if ( err == 1) 
    {
      Scierror("Error: operator %s , arguments with incompatible dimensions\n",op);
      return RET_BUG;
    }
  if ( rep == TRUE ) 
    {
      if (( Res =nsp_create_true_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  else 
    {
      if (( Res =nsp_create_false_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

int int_pmatrix_flt(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<");
}

int int_pmatrix_fle(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<=");
}


int int_pmatrix_fneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"<>");
}

int int_pmatrix_feq(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,"==");
}

int int_pmatrix_fgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,">");
}

int int_pmatrix_fge(Stack stack, int rhs, int opt, int lhs)
{
  return int_pmatrix_f_gen(stack,rhs,opt,lhs,">=");
}



/*
 * Res =nsp_pmatrix_copy(A) 
 * Creates a Copy of NspPMatrix A : A is not checked 
 */

int int_pmatrix_transpose(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( HMat1 = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
  if (( HMat2 =nsp_pmatrix_transpose(HMat1))  == NULLPMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}


/*
 * The Interface for basic matrices operation 
 */

static OpTab PMatrix_func[]={
  {"resize2vect_p", int_pmatrix_2vect},	
  {"extractcols_p",int_pmatrix_extractcols},	
  {"extractrows_p",int_pmatrix_extractrows},
  {"extractelts_p",int_pmatrix_extractelts},
  {"loopextract_m_p",int_pmatrix_extractcolforloop},
  {"deletecols_p_m", int_pmatrix_deletecols},
  {"deleterows_p_m", int_pmatrix_deleterows},
  {"deleteelts_p_m", int_pmatrix_deleteelts},
  {"setrowscols_p",int_pmatrix_setrc},
  {"smat_create",int_pmatrix_create},
  {"redim_p",int_pmatrix_redim},
  {"concatr_p_p",int_pmatrix_concatr},
  {"concatr_m_p",int_pmatrix_concatr_m_s},
  {"addcols_p_m",int_pmatrix_addcols},
  {"concatd_p_p",int_pmatrix_concatd},
  {"concatd_m_p",int_pmatrix_concatd_m_s},
  {"addrows_p",int_pmatrix_addrows},
  {"setrc_p",int_pmatrix_setrc},
  {"extract_p",int_pmatrix_extract},
  {"resize_p",int_pmatrix_resize},
  {"enlarge_p", int_pmatrix_enlarge },
  {"eq_p_p" ,  int_pmatrix_eq },
  {"feq_p_p" ,  int_pmatrix_feq },
  {"fge_p_p" ,  int_pmatrix_fge },
  {"fgt_p_p" ,  int_pmatrix_fgt },
  {"fle_p_p" ,  int_pmatrix_fle },
  {"flt_p_p" ,  int_pmatrix_flt },
  {"fneq_p_p" ,  int_pmatrix_fneq },
  {"ge_p_p" ,  int_pmatrix_ge },
  {"gt_p_p" ,  int_pmatrix_gt },
  {"le_p_p" ,  int_pmatrix_le },
  {"lt_p_p" ,  int_pmatrix_lt },
  {"ne_p_p" ,  int_pmatrix_neq },
  {"m2p", int_matrix_to_pmatrix},
  {"quote_p", int_pmatrix_transpose},
  {(char *) 0, NULL}
};

int PMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(PMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) */

void PMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = PMatrix_func[i].name;
  *f = PMatrix_func[i].fonc;
}










