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

#define SpRowMatrix_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

#include "nsp/spmatrix-in.h"
#include "nsp/spmatops-in.h"

/*
 * NspSpMatrix inherits from NspObject 
 */

int nsp_type_sprowmatrix_id=0;
NspTypeSpRowMatrix *nsp_type_sprowmatrix=NULL;
int nsp_type_sprowmatrix_init();

NspTypeSpRowMatrix *new_type_sprowmatrix(type_mode mode)
{
  NspTypeSpRowMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_sprowmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_sprowmatrix;
    }
  if ((type =  malloc(sizeof(NspTypeSpRowMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* spmatrix_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = NULL; /*spmatrix_get_methods; */
  type->new = (new_func *) new_sprowmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for spmatrix */ 

  top->pr = (print_func *)nsp_sprowmatrix_print;                  /* printing*/   
  top->dealloc = (dealloc_func *)nsp_sprowmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_sprowmatrix_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_sprowmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_sprowmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_sprowmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_sprowmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) SpMatIsTrue;*/      /* check if object can be considered as true */  
  /*top->loop =(loop_func *) SpLoopExtract ; */               /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_sprowmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_sprowmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_sprowmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_sprowmatrix_xdr_save;
  top->load  = (load_func *)nsp_sprowmatrix_xdr_load;

  /* specific methods for spmatrix */
  type->init = (init_func *) init_sprowmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_sprowmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_sprowmatrix
       */
      type->id =  nsp_type_sprowmatrix_id = nsp_new_type_id();
      nsp_type_sprowmatrix = type;
      if ( nsp_register_type(nsp_type_sprowmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_sprowmatrix(mode); 
    }
  else 
    {
      type->id = nsp_type_sprowmatrix_id;
      return type;
    }
}
/*
 * initialize Spmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_sprowmatrix(NspSpRowMatrix *o,NspTypeSpRowMatrix *type)
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

NspSpRowMatrix *new_sprowmatrix() 
{
  NspSpRowMatrix *loc; 
  /* type must exists */
  nsp_type_sprowmatrix = new_type_sprowmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspSpRowMatrix)))== NULLSPROW) return loc;
  /* initialize object */
  if ( init_sprowmatrix(loc,nsp_type_sprowmatrix) == FAIL) return NULLSPROW;
  return loc;
}


/*
 *nsp_sprowmatrix_size: returns filled,hsize,or hsize 
 */

int nsp_sprowmatrix_size(NspSpRowMatrix *H, int flag)
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

static char sp_type_name[]="SpRowMat";
static char sp_short_type_name[]="sprow";

char *nsp_sprowmatrix_type_as_string(void)
{
  return(sp_type_name);
}

char *nsp_sprowmatrix_type_short_string(NspObject *v)
{
  return(sp_short_type_name);
}


int nsp_sprowmatrix_fullcomp(NspSpRowMatrix * A,NspSpRowMatrix * B,char *op,int *err)
{
  Scierror("SpMatFullComp: to be implemented \n");
  return FALSE;
}


int nsp_sprowmatrix_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_sprowmatrix_id) == FALSE) return FALSE ;
  rep =nsp_sprowmatrix_fullcomp((NspSpRowMatrix *) A,(NspSpRowMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_sprowmatrix_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_sprowmatrix_id) == FALSE) return TRUE;
  rep =nsp_sprowmatrix_fullcomp((NspSpRowMatrix *) A,(NspSpRowMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}



/*
 * Save a Matrix in a file stream 
 */

static int nsp_sprowmatrix_xdr_save(XDR *xdrs, NspSpRowMatrix *M)
{
  int rep=FAIL;
  NspMatrix *RC=NULL, *Values=NULL;
  if ( nsp_sprowmatrix_get(M,&RC,&Values) == FAIL) goto fail;
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) goto fail;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) goto fail;
  if (nsp_xdr_save_i(xdrs, M->m) == FAIL) goto fail;
  if (nsp_xdr_save_i(xdrs, M->n) == FAIL) goto fail;
  if ( NSP_OBJECT(RC)->type->save (xdrs,RC) == FAIL) goto fail;
  if ( NSP_OBJECT(Values)->type->save (xdrs,Values) == FAIL) goto fail;
  rep=OK;
 fail:
  if ( RC != NULL) nsp_matrix_destroy(RC);
  if ( Values != NULL) nsp_matrix_destroy(Values);
  return rep;
}

/*
 * Load a Matrix from a file stream 
 **/

static NspSpRowMatrix *nsp_sprowmatrix_xdr_load(XDR *xdrs)
{
  int m,n;
  NspObject *RC=NULL, *Values=NULL;
  NspSpRowMatrix *Loc;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSPROW;
  if (nsp_xdr_load_i(xdrs, &m) == FAIL)  return NULLSPROW;
  if (nsp_xdr_load_i(xdrs, &n) == FAIL)  return NULLSPROW;
  if ( (RC= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSPROW;
  if ( (Values= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSPROW;
  if ((Loc = nsp_sprowmatrix_sparse(name,(NspMatrix *)RC,(NspMatrix *)Values,m,n)) == NULLSPROW) return NULLSPROW;
  if ( RC != NULL) nsp_object_destroy(&RC);
  if ( Values != NULL) nsp_object_destroy(&Values);
  return Loc;
}


/*
 * A =nsp_sprowmatrix_object(O);
 * checks that O is an object of NspSpRowMatrix type. 
 * or a Hobj which points to an object of type SpRowMatrix
 * if so, returns a pointer to that NspSpRowMatrix and else returns NULL
 */

NspSpRowMatrix   *nsp_sprowmatrix_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_sprowmatrix_id) == TRUE) return ((NspSpRowMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_sprowmatrix));
  return(NULL);
}


/*
 * IsSpMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  SpRowMatrix 
 * or a Hobj which points to an object of type SpRowMatrix
 */

int IsSpMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_sprowmatrix_id);
}

/*
 * IsSpMat(O)
 * only checks that object is an object of type  SpRowMatrix 
 * or a Hobj which points to an object of type SpRowMatrix
 */

int IsSpMat(const NspObject *O)
{
  return nsp_object_type(O , nsp_type_sprowmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a Sp and returns that Sp  
 * or a copy of that Sp if its name 
 * is != NVOID 
 */

NspSpRowMatrix *GetSpRowCopy(Stack stack, int i)
{
  if (  GetSpRow(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a SpRowMatrix and returns that SpRowMatrix  
 */

NspSpRowMatrix *GetSpRow(Stack stack, int i)
{
  NspSpRowMatrix *M;
  if (( M =nsp_sprowmatrix_object(NthObj(i))) == NULLSPROW )
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a SpRowMatrix and returns that SpRowMatrix  
 */

NspSpRowMatrix *GetRealSpRow(Stack stack, int i)
{
  NspSpRowMatrix *M;
  if (( M =nsp_sprowmatrix_object(NthObj(i))) == NULLSPROW )
    ArgMessage(stack,i);
  if (M->rc_type == 'c')
    {
      Scierror ("\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should not be complex\n", NspFname(stack));
      return NULLSPROW;
    }
  return M;
}


/*
 * Now the interfaced function for basic sparse operations
 */

typedef int (*SpC) (NspSpRowMatrix *A,NspSpRowMatrix *B);

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPROW on failure 
 * The matrix is created with no initial value 
 */

static int int_sprowmatrix_create(Stack stack, int rhs, int opt, int lhs)
{  
  int m1,n1;
  NspSpRowMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ((HMat =nsp_sprowmatrix_create(NVOID,'r',m1,n1) ) == NULLSPROW) return RET_BUG;
  MoveObj(stack,1, (NspObject *) HMat);
  return 1;
}

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPROW on failure 
 * sparse(rc,values [, [m,n]])
 */

static int int_sprowmatrix_sparse(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpRowMatrix *A;
  NspMatrix *RC,*Values,*MN;
  if ( rhs == 1) 
    return  int_sprowmatrix_m2sp(stack,rhs,opt,lhs);
  CheckRhs(2,3);
  CheckLhs(1,1);
  if ((RC = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  if (RC->mn != 0 &&  RC->n != 2)
    {
      Scierror("Error: first argument of function %s must have 2 columns\n\n",
	       NspFname(stack));
      return RET_BUG;
    }
  if ((Values = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( RC->m != Values->mn  ) 
    {
      Scierror("Error: first and second arguments of function %s have incompatible sizes\n",
	       NspFname(stack));
      return RET_BUG;
    }
  if ( rhs == 3 )
    {
      if ((MN = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;
      if (( A =nsp_sprowmatrix_sparse(NVOID,RC,Values,(int) MN->R[0],(int) MN->R[1])) == NULLSPROW ) 
	return RET_BUG;
    }
  else
    {
      if (( A =nsp_sprowmatrix_sparse(NVOID,RC,Values,-1,-1)) == NULLSPROW ) 
	return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


static int int_sprowmatrix_sparse_sprow(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpRowMatrix *A;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((A = GetSpRowCopy(stack,1)) == NULLSPROW) return RET_BUG;
  NSP_OBJECT(A)->ret_pos=1;
  return Max(lhs,1);
}

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPROW on failure 
 * sparse(rc,values [, [m,n]])
 */

static int int_sprowmatrix_get(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpRowMatrix *A;
  NspMatrix *RC,*Values,*MN=NULL;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ( nsp_sprowmatrix_get(A,&RC,&Values) == FAIL) return RET_BUG;
  if ( lhs == 3) 
    {
      if (( MN = nsp_matrix_create(NVOID,'r',1,2)) == NULLMAT) return RET_BUG;
      MN->R[0] = A->m; MN->R[1] = A->n;
    }
  if ( lhs >= 2) 
    {
      NthObj(2) = (NspObject *)Values;
      NthObj(2)->ret_pos = 2;
    }
  if ( lhs >= 3) 
    {
      NthObj(3) = (NspObject *)MN;
      NthObj(3)->ret_pos = 3;
    }
  MoveObj(stack,1,(NspObject *) RC);
  return Max(lhs,1);
}


/*
 * Matredim : changes Matrix dimensions
 * but keeps m*n constant
 */

static int int_sprowmatrix_redim(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspSpRowMatrix  *A;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if (( A =nsp_sprowmatrix_redim(A,m1,n1)) == NULLSPROW ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * Right Concatenation 
 * A= [A,B] 
 */


static int int_sprowmatrix_concat_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpRowMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpRow(stack,1))  == NULLSPROW) return RET_BUG;
  if ( HMat1->mn == 0) 
    {
      /* return 2 */
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSpRow(stack,2)) == NULLSPROW) return RET_BUG;
  if ( HMat2->mn == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetSpRowCopy(stack,1))  == NULLSPROW) return RET_BUG;
      if ( (*F)(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}


static int int_sprowmatrix_concatr(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix_concat_gen(stack,rhs,opt,lhs,nsp_sprowmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPROW on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_sprowmatrix_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix_concat_gen(stack,rhs,opt,lhs,nsp_sprowmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPROW on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_sprowmatrix_concatdiag(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix_concat_gen(stack,rhs,opt,lhs,nsp_sprowmatrix_concatdiag);
}


/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *  =======
 */

static int int_sprowmatrix_setrc(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A,*B;
  NspMatrix *Rows,*Rows1=NULLMAT,*Cols=NULLMAT,*Cols1=NULLMAT;
  CheckRhs(3,4);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) goto ret_bug;
  if ( IsBMatObj(stack,2)  ) 
    {
      /* Rows is boolean : use find(Rows) **/
      NspBMatrix *BRows ;
      if ((BRows = GetBMat(stack,2)) == NULLBMAT) goto ret_bug;
      if ((Rows = Rows1 = nsp_bmatrix_find(BRows)) == NULLMAT) goto ret_bug;
    }
  else
    {
      if ((Rows = GetRealMat(stack,2)) == NULLMAT) goto ret_bug;
    }
  if ( rhs == 4 )
    {
      /* Cols is boolean : use find(Cols) **/
      if ( IsBMatObj(stack,3)  ) 
	{
	  NspBMatrix *BCols ;
	  if ((BCols = GetBMat(stack,2)) == NULLBMAT) goto ret_bug;
	  if ((Cols = Cols1 = nsp_bmatrix_find(BCols)) == NULLMAT) goto ret_bug;
	}  
      else
	{
	  if ((Cols = GetRealMat(stack,3)) == NULLMAT ) goto ret_bug;
	}
    }
  if ((B = GetSpRow(stack,rhs)) == NULLSPROW ) goto ret_bug;
  if ( B == A ) 
    { if ((B = GetSpRowCopy(stack,rhs)) == NULLSPROW ) goto ret_bug;}
  if ( rhs == 3 ) 
    { if (nsp_sprowmatrix_set_row( A, Rows,B) == FAIL) goto ret_bug; }
  else 
    { if (nsp_sprowmatrix_set_rowcol( A, Rows,Cols,B) == FAIL )  goto ret_bug;} 
  NSP_OBJECT(A)->ret_pos = 1;
  nsp_matrix_destroy(Rows1);
  nsp_matrix_destroy(Cols1);
  return 1;
 ret_bug: 
  /* delete if non null; */
  nsp_matrix_destroy(Rows1);
  nsp_matrix_destroy(Cols1);
  return RET_BUG;
}


/*
 * Res=MatDeletecols(A,Cols)
 *     Cols unchanged  ( restored at end of function if necessary)
 * WARNING : A must be changed by this routine
 * =======
 */	

static int int_sprowmatrix_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A;
  NspMatrix *Cols;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  /* A and Cols can't point to the same object **/
  if ((Cols = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if (nsp_sprowmatrix_delete_cols( A, Cols) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=MatDeleterows(A,Rows)
 * WARNING : A must be changed by this routine
 * =======
 */	

static int int_sprowmatrix_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A;
  NspMatrix *Rows;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  /* A and Cols can't point to the same object **/
  if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if (nsp_sprowmatrix_delete_rows( A, Rows) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=nsp_sprowmatrix_extract(Rows,Cols,A)
 * A unchanged, Rows and Cols are unchanged 
 */	

static int int_sprowmatrix_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A,*Res;
  NspMatrix *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;
  Res =nsp_sprowmatrix_extract( A, Rows,Cols);
  if ( Res == NULLSPROW) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * Res=nsp_matrix_extract_elements(Elts,A)
 * A unchanged, Elts
 * WARNING note that on the stack we have Elts,A 
 */	

static int int_sprowmatrix_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A,*Res;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((Elts = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Res =nsp_sprowmatrix_extract_elts( A, Elts)) == NULLSPROW) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * columns extraction  Cols A --> A(Cols)				  * 
 */	

static int int_sprowmatrix_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A,*Res;
  NspMatrix *Cols;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((Cols = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_sprowmatrix_extract_cols( A,Cols,&err);
  /* XXXXX Attention ici il faut un message d''erreur **/
  if ( err == 1) return RET_ENDFOR; 
  if ( Res == NULLSPROW) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * rows extraction 			
 * Rows , A -> A(Rows,:)
 */	

static int int_sprowmatrix_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A,*Res;
  NspMatrix *Rows;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_sprowmatrix_extract_rows( A,Rows,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLSPROW) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


/*
 * diag interface 
 */

int int_sprowmatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspSpRowMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetSpRow (stack, 1)) == NULLSPROW)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_sprowmatrix_diag_create (A, k1);
  else
    Res = nsp_sprowmatrix_diag_extract (A, k1);
  if (Res == NULLSPROW)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Returns the kthe diag of a Matrix 
 */

static int int_sprowmatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpRowMatrix *A,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    { if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;}
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  Res =nsp_sprowmatrix_diag_extract( A,k1);
  if ( Res == NULLSPROW)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Set the kth Diag of A to Diag 
 *  A is enlarged & comlexified if necessary 
 *  int nsp_matrix_create_diag(A,Diag,k)
 * WARNING : A is not copied we want this routine to change A
 */

static int int_sprowmatrix_diagset(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspSpRowMatrix *A,*Diag;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((Diag = GetSpRow(stack,2)) == NULLSPROW) return RET_BUG;
  if ( GetScalarInt(stack,3,&k1) == FAIL) return RET_BUG;
  if (nsp_sprowmatrix_diag_set( A, Diag,k1) != OK) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 *  Creates a Matrix with kth diag set to Diag 
 */

static int int_sprowmatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpRowMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_sprowmatrix_diag_create(Diag,k1)) == NULLSPROW ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= mx2spmx(A) 
 * A is not changed 
 */

static int int_sprowmatrix_m2sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *Res;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT)  return RET_BUG;
  if (( Res=nsp_sprowmatrix_from_mat(A)) == NULLSPROW) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Res= full(A) 
 * A is not changed 
 */

static int int_sprowmatrix_sp2m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A;
  NspMatrix *Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetSpRow(stack,1)) == NULLSPROW)  return RET_BUG;
  if (( Res=nsp_sprowmatrix_to_mat(A)) == NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= full(A) when A is not sparse 
 * do nothing 
 */

static int int_sprowmatrix_m2m(Stack stack, int rhs, int opt, int lhs)
{
  /* full(x) when x is already full **/
  NthObj(1)->ret_pos = 1;
  return 1;
}


/*
 * Res= A*B 
 * return NULLSPROW on failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

/* Generic function for * or .* **/
typedef NspSpRowMatrix * (*Sp21) (NspSpRowMatrix *A,NspSpRowMatrix *B);

static int int_sprowmatrix_mult_gen(Stack stack, int rhs, int opt, int lhs, Sp21 F)
{
  NspSpRowMatrix *HMat1,*HMat2,*HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ( HMat1->mn == 0)  
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSpRow(stack,2)) == NULLSPROW) return RET_BUG;
  if ( HMat2->mn == 0 )
    {
      /* flag == 1 ==> A op [] returns [] **/
      NSP_OBJECT(HMat2)->ret_pos = 1;
      return 1;
    }
  if ( HMat2->mn == 1) 
    {
      if ( HMat2->D[0]->size == 0) 
	{
	  /* HMat2 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_sprowmatrix_create(NVOID,'r',HMat1->m,HMat1->n)) == NULLSPROW) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else 
	{
	  /* A * <non-nul-scalar> **/
	  if ((HMat1 = GetSpRowCopy(stack,1)) == NULLSPROW) return RET_BUG;
	  if (nsp_sprowmatrix_mult_scal(HMat1,HMat2) != OK) return RET_BUG;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
    }
  else if ( HMat1->mn == 1 ) 
    {
      if ( HMat1->D[0]->size == 0) 
	{
	  /* HMat1 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_sprowmatrix_create(NVOID,'r',HMat2->m,HMat2->n)) == NULLSPROW) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else
	{
	  /* since Mat1 is scalar we store the result in Mat2 so we 
	     must copy it **/
	  if ((HMat2 = GetSpRowCopy(stack,2)) == NULLSPROW) return RET_BUG;
	  if (nsp_sprowmatrix_mult_scal(HMat2,HMat1) != OK) return RET_BUG;
	  NSP_OBJECT(HMat2)->ret_pos = 1;
	}
    }
  else 
    {
      if ((HMat3=(*F)(HMat1,HMat2)) == NULLSPROW) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

static int int_sprowmatrix_mult(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix_mult_gen(stack,rhs,opt,lhs,nsp_sprowmatrix_mult);
}

/*
 *   Res = A * X , A sparse matrix, X full matrix
 *   A and X are left unchanged
 *   added by Bruno
 */
static int int_sprowmatrix_mult_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *HMat1;
  NspMatrix *HMat2, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((HMat2 = GetMat (stack, 2)) == NULLMAT) return RET_BUG;

  if ( HMat1->mn == 0)  
    {
      /* Not so good we need to return a full XXXX */
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else if ( HMat2->mn == 0 )
    {
      NSP_OBJECT(HMat2)->ret_pos = 1;
      return 1;
    }

  if ( HMat1->n != HMat2->m )
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return RET_BUG;
    }

  if ( (HMat3 = nsp_sprowmatrix_mult_sp_m(HMat1, HMat2)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat3);
  return 1;
}


/*
 *   Res = X * A , A sparse matrix, X full matrix
 *   A and X are left unchanged
 *   added by Bruno
 */

static int int_sprowmatrix_mult_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *HMat2;
  NspMatrix *HMat1, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  if ((HMat2 = GetSpRow(stack,2)) == NULLSPROW) return RET_BUG;

  if ( HMat1->mn == 0)  
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else if ( HMat2->mn == 0 )
    {
      /* Not so good we need to return a full XXXX */
      NSP_OBJECT(HMat2)->ret_pos = 1;
      return 1;
    }

  if ( HMat1->n != HMat2->m )
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return RET_BUG;
    }
  if ( (HMat3 = nsp_sprowmatrix_mult_m_sp(HMat1, HMat2)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat3);
  return 1;
}

/*
 * Res= A'
 * return NULLSPROW on failure 
 * A is left unchanged 
 */

static int int_sprowmatrix_quote(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((A =nsp_sprowmatrix_transpose(A)) == NULLSPROW )  return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * _
 * A'
 */

static int int_sprowmatrix_dquote (Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetSpRow(stack, 1)) == NULLSPROW)
    return RET_BUG;
  if ((B = nsp_sprowmatrix_transpose (A)) == NULLSPROW)
    return RET_BUG;
 nsp_sprowmatrix_conj(B);
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}



/*
 * C = A + B with special cases [] and [x] 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

static int int_sprowmatrix_plus(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpRowMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((B = GetSpRow(stack,2)) == NULLSPROW) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      NspSpRowMatrix *Res =nsp_sprowmatrix_add(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  if ( A->mn <= 1) 
    {
      NspMatrix *C=nsp_sprowmatrix_op_scal(B,A,&flag,'+');
      if ( flag == 1) 
	{
	  NSP_OBJECT(B)->ret_pos = 1;
	  return 1; 	  /* A was [] or [0] **/
	}
      else
	{
	  /* C = A + scal is returned in a Matrix **/
	  MoveObj(stack,1,(NspObject *)C);
	  return 1;
	}
    }
  if ( B->mn <= 1) 
    {
      NspMatrix *C=nsp_sprowmatrix_op_scal(A,B,&flag,'+');
      if ( flag == 1) 
	{
	  NSP_OBJECT(A)->ret_pos = 1;
	  return 1; 	  /* B was [] or [0] **/
	}
      else
	{
	  /* C = A + scal is returned in a Matrix **/
	  MoveObj(stack,1,(NspObject *)C);
	  return 1;
	}
    }
  return 1;
}



/*
 * OHMat3 = OHMat1-OHMat2 
 * return NULLSPROW on failure 
 */

static int int_sprowmatrix_sub(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpRowMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((B = GetSpRow(stack,2)) == NULLSPROW) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      NspSpRowMatrix *Res =nsp_sprowmatrix_sub(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  if ( A->mn <= 1) 
    {
      NspMatrix *C=nsp_sprowmatrix_op_scal(B,A,&flag,'#'); /* -B + scalar **/
      if ( flag == 1) 
	{
	  /* B -> -B **/
	  stack.first += 1;
	  int_sprowmatrix_minus(stack,1,opt,1);
	  stack.first -= 1;
	  NSP_OBJECT(B)->ret_pos = 1;
	  return 1; 	  /* A was [] or [0] **/
	}
      else
	{
	  /* C = A - scal is returned in a Matrix **/
	  MoveObj(stack,1,(NspObject *)C);
	  return 1;
	}
    }
  if ( B->mn <= 1) 
    {
      NspMatrix *C=nsp_sprowmatrix_op_scal(A,B,&flag,'-');
      if ( flag == 1) 
	{
	  NSP_OBJECT(A)->ret_pos = 1;
	  return 1; 	  /* B was [] or [0] **/
	}
      else
	{
	  /* C = A + scal is returned in a Matrix **/
	  MoveObj(stack,1,(NspObject *)C);
	  return 1;
	}
    }
  return 1;
}

/*
 * OHMat3 = OHMat1 .* OHMat2 
 * return NULLSPROW on failure 
 */

static int int_sprowmatrix_multt(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix_mult_gen(stack,rhs,opt,lhs,nsp_sprowmatrix_multtt);
}

/*
 *nsp_mat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspSpRowMatrix *(*SuPro) (NspSpRowMatrix *A,char *);

static int int_sprowmatrix__sum(Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  char *str;
  NspSpRowMatrix *Res,*HMat; 
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((HMat = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
    }
  else 
    { str = "F"; }
  if ((Res= (*F)(HMat,str)) == NULLSPROW ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/* added by Bruno : return the number of non zero elements */
static int int_sprowmatrix_nnz(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *HMat; 
  NspMatrix *Res;
  int  nnz;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
  if ((Res = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) return FAIL;

  nnz = nsp_sprowmatrix_nnz(HMat);
  Res->R[0] = (double) nnz;
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_sprowmatrix_sum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_sprowmatrix__sum(stack,rhs,opt,lhs,nsp_sprowmatrix_sum) );
}



typedef NspSpRowMatrix *(*SpMiMax) (NspSpRowMatrix *A,char *,NspMatrix **Imax,int lhs);

static int int_sprowmatrix__maxi(Stack stack, int rhs, int opt, int lhs, SpMiMax F,int minmax)
{
  char *str;
  NspSpRowMatrix *A,*M;
  NspMatrix *Imax;
  if ( rhs < 1) 
    { 
      Scierror("Error:\t Rhs must be >= 1 for function %s\n",NspFname(stack));
      return RET_BUG;
    }
  CheckLhs(1,2);
  if ( rhs == 1 || ( rhs == 2 && IsSMatObj(stack,2)  ))
    {
      /* maxi(A) or maxi(A,'c' or 'r' or 'F') where A is a matrix 
       * idem for mini 
       * XXXXXX : Attention pas fini ici il faut un getrealsp 
       */
      if ((A = GetSpRow(stack,1)) == NULLSPROW) return RET_BUG;
      if ( rhs == 2) 
	{
	  if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
	}
      else 
	{ str = "F"; }
      if (( M= (*F)(A,str,&Imax,lhs)) == NULLSPROW) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(M));
      if ( lhs == 2)
	{
	  MoveObj(stack,2,NSP_OBJECT(Imax));
	}
    }
  else
    {
      int index= (lhs == 2 ) ? 1 : 0, err;
      NspSpRowMatrix *Index=NULL;
      if ((A = GetSpRowCopy(stack,1))  == NULLSPROW) return RET_BUG;
      if ((M = GetSpRowCopy(stack,2))  == NULLSPROW) return RET_BUG;
      Index = nsp_sprowmatrix_maximinitt_g(A,M,index,minmax,&err);
      if (err == TRUE ) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      if ( lhs == 2 ) 
	{
	  MoveObj(stack,2,NSP_OBJECT(Index));
	}
    }
  return Max(lhs,1);
}


static int int_sprowmatrix_maxi(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_sprowmatrix__maxi(stack,rhs,opt,lhs,nsp_sprowmatrix_maxi,1));
}

static int int_sprowmatrix_mini(Stack stack, int rhs, int opt, int lhs)
{
  /* return ( int_sprowmatrix__maxi(stack,rhs,opt,lhs,nsp_sprowmatrix_mini,-1)); */
  return RET_BUG;
}

/* interface for triu 
 *
 */

static int int_sprowmatrix_triu(Stack stack, int rhs, int opt, int lhs)
{
  int k=0;
  NspSpRowMatrix *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
    }
  if ((A=GetSpRowCopy(stack,1))== NULLSPROW) return RET_BUG;
  if ( nsp_sprowmatrix_triu(A,k)==FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;

}

/* interface for tril
 *
 */

static int int_sprowmatrix_tril(Stack stack, int rhs, int opt, int lhs)
{
  int k=0;
  NspSpRowMatrix *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
    }
  if ((A=GetSpRowCopy(stack,1))== NULLSPROW) return RET_BUG;
  if ( nsp_sprowmatrix_tril(A,k)==FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;

}

/* interface for eye,ones,zeros
 *
 */

typedef NspSpRowMatrix *(Feoz)(int m,int n);

static int int_sprowmatrix_eoz(Stack stack, int rhs, int opt, int lhs,Feoz *F)
{
  int m,n;
  NspSpRowMatrix *A;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
  if ((A= (*F)(m,n))== NULLSPROW) return RET_BUG;
  MoveObj(stack,1, NSP_OBJECT(A));
  return 1;
}

static int int_sprowmatrix_ones(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix_eoz(stack,rhs,opt,lhs,nsp_sprowmatrix_ones);
}

static int int_sprowmatrix_eye(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix_eoz(stack,rhs,opt,lhs,nsp_sprowmatrix_eye);
}

static int int_sprowmatrix_zeros(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix_eoz(stack,rhs,opt,lhs,nsp_sprowmatrix_zeros);
}


static int int_sprowmatrix_clean(Stack stack, int rhs, int opt, int lhs)
{
  double epsa =1.e-10, epsr = 1.e-10;
  NspSpRowMatrix *A;
  CheckRhs (1, 3);
  CheckLhs (1, 1);
  if (rhs >= 2)
    {
      if (GetScalarDouble (stack, 2, &epsa) == FAIL)
	return RET_BUG;
    }
  if (rhs >= 3)
    {
      if (GetScalarDouble (stack, 3, &epsr) == FAIL)
	return RET_BUG;
    }
  if ((A = GetSpRowCopy(stack, 1)) == NULL)
    return RET_BUG;
  nsp_sprowmatrix_clean (A, rhs, epsa, epsr);
  NSP_OBJECT (A)->ret_pos = 1;
  return 1;
}

static int int_sprowmatrix_sprand(Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  char crand='u';
  double sparsity=0.1;
  int m,n;
  NspSpRowMatrix *A;
  CheckRhs(2,4);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
  if ( rhs >= 3 )
    if (GetScalarDouble(stack,3,&sparsity) == FAIL) return RET_BUG;
  if ( rhs >= 4 )
    {
      if ((str=GetString(stack,4)) == NULL) return RET_BUG;
      if ( strlen(str) >= 1 ) crand=str[0];
    }
  if ((A= nsp_sprowmatrix_rand(m,n,sparsity,crand))== NULLSPROW) return RET_BUG;
  MoveObj(stack,1, NSP_OBJECT(A));
  return 1;
}


/*
 *  A=op(A) 
 */

typedef int (*M11) (NspSpRowMatrix *A);
typedef void (*VM11) (NspSpRowMatrix *A);

/* generic function for ones,rand,eyes **/
 
static int int_sprowmatrix__gen11(Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspSpRowMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpRowCopy(stack,1))== NULLSPROW) return RET_BUG;
  if ((*F)(A) == FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

static int int_sprowmatrix__genv11(Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspSpRowMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpRowCopy(stack,1))== NULLSPROW) return RET_BUG;
  (*F)(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 *nsp_sprowmatrix_minus: A=-(A)
 * A is changed  
 * return 0 if error 
 */

int int_sprowmatrix_minus(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_minus);
}


/*
 * A=Abs(A), absolue value or module of each element 
 */

static int int_sprowmatrix_abs(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_abs);
}

/*
 * A=Erf(A)
 */

/*
 * A=Erfc(A),  * A is changed 
 */

/*
 * A=Arg(A),  * A is changed 
 */

static int int_sprowmatrix_arg(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_arg);
}

/*
 *nsp_sprowmatrix_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

typedef NspMatrix* (*SpM) (NspSpRowMatrix *A);

static int int_sprowmatrix__m_gen11(Stack stack, int rhs, int opt, int lhs, SpM F)
{
  NspMatrix *Loc;
  NspSpRowMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ((Loc = (*F)(A)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Loc);
  return 1;
}

static int int_sprowmatrix_cos(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__m_gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_cos);
}

/*
 *nsp_sprowmatrix_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */
static int int_sprowmatrix_cosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__m_gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_cosh);
}

/*
 * SpExpl : Exponentiation terme a term 
 * A is changed 
 */

static int int_sprowmatrix_expel(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__m_gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_expel);
}

/*
 * SpLog : A=LogEl(A) 
 */

static int int_sprowmatrix_logel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpRowCopy(stack,1))== NULLSPROW) return RET_BUG;
  if (nsp_sprowmatrix_logel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_sprowmatrix_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_sprowmatrix_sin(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_sin);
}

/*
 *nsp_sprowmatrix_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_sprowmatrix_sinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_sinh);
}

/*
 *nsp_sprowmatrix_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

static int int_sprowmatrix_sqrtel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpRowCopy(stack,1))== NULLSPROW) return RET_BUG;
  if (nsp_sprowmatrix_sqrtel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_sprowmatrix_acos: A=Acos(A)
 * A is changed  
 * return 0 if error 
 */

static int int_sprowmatrix_acos(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__m_gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_acos);
}

/*
 *nsp_sprowmatrix_acosh: A=Acosh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_sprowmatrix_acosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__m_gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_acosh);
}

/*
 *nsp_sprowmatrix_asin: A=Asin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_sprowmatrix_asin(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_asin);
}

/*
 *nsp_sprowmatrix_asinh: A=Asinh(A)
 * A is changed  
 * return 0 if error 
 */


static int int_sprowmatrix_asinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_asinh);
}

/*
 * SpATan : res= arctang(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_sprowmatrix_atan(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_atan);
}

/*
 * SpArcTangH : res= atanh(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_sprowmatrix_atanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_atanh);
}

/*
 *nsp_sprowmatrix_ceil: A=Ceil(A)
 * A is changed  
 */

static int int_sprowmatrix_ceil(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_ceil);
}

/*
 *nsp_sprowmatrix_int: A=Int(A)
 * A is changed  
 */

static int int_sprowmatrix_int(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_int);
}

/*
 *nsp_sprowmatrix_floor: A=Floor(A)
 * A is changed  
 */
 
static int int_sprowmatrix_floor(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_floor);
}

/*
 *nsp_sprowmatrix_round: A=Round(A)
 * A is changed  
 */
 
static int int_sprowmatrix_round(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_round);
}

/*
 *nsp_sprowmatrix_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_sprowmatrix_sign(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_sign);
}

/*
 *nsp_sprowmatrix_tan: A=Tan(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_sprowmatrix_tan(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_tan);
}

/*
 *nsp_sprowmatrix_tanh: A=Tanh(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_sprowmatrix_tanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__genv11(stack,rhs,opt,lhs,nsp_sprowmatrix_tanh);
}

/*
 * A=Polar(A,B),  * A is changed 
 */

/*
 *nsp_sprowmatrix_conj: A=real(A)-i*Imag(A)
 * A is changed  if imaginary not changed if real 
 */

static int int_sprowmatrix_conj(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpRow(stack,1))== NULLSPROW) return RET_BUG;
  if ( A->mn == 0 || A->rc_type == 'r' )
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpRowCopy(stack,1))== NULLSPROW) return RET_BUG;
 nsp_sprowmatrix_conj(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * A=nsp_mat_pow_el(A,B), A.^ B 
 * with special cases Mat.^[]  and Mat.^scalar
 *                    [].^Mat ans scalar.^Mat
 */
/*
 * A=DivEl(A,B),  A ./ B 
 */
/*
 * A=BackDivEl(A,B),  A .\ B 
 */
/*
 * A=MultEl(A,B),  A .* B 
 */

/*
 * Matrix multiplication  Res= A*B  
 * with special cases Mat * [] and Mat * scalar
 * very similar to mopscal but MatMult returns a new matrix 
 */

/*
 * returns in a Matrix the indices for which the Matrix is true 
 */

static int int_sprowmatrix_find(Stack stack, int rhs, int opt, int lhs)
{
  NspSpRowMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetSpRow(stack,1)) == NULLSPROW)  return RET_BUG;
  if (nsp_sprowmatrix_find(A,Max(lhs,1),&Rr,&Rc) == FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Rr));
  if ( lhs == 2 )
    {
      NthObj(2) = (NspObject *) Rc;
      NthObj(2)->ret_pos = 2;
      return 2;
    }
  return 1;
}


static int int_sprowmatrix_real(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_realpart);
}

static int int_sprowmatrix_imag(Stack stack, int rhs, int opt, int lhs)
{
  return int_sprowmatrix__gen11(stack,rhs,opt,lhs,nsp_sprowmatrix_imagpart);
}


/*
 * The Interface for sparse ops
 */

static OpTab SpRowMatrix_func[]={
  {"sprow_create",int_sprowmatrix_create},
  {"m2sprow",int_sprowmatrix_m2sp},
  {"sprow2m",int_sprowmatrix_sp2m},
  {"dst_sprow_sprow",int_sprowmatrix_multt},
  {"mult_sprow_sprow",int_sprowmatrix_mult},
  {"mult_sprow_m",int_sprowmatrix_mult_sp_m},
  {"mult_m_sprow",int_sprowmatrix_mult_m_sp},

  {"plus_sprow_sprow",int_sprowmatrix_plus},
  {"minus_sprow_sprow",int_sprowmatrix_sub},
  {"minus_sprow_m",int_sprowmatrix_sub},
  {"minus_m_sprow",int_sprowmatrix_sub},
  {"minus_sprow",int_sprowmatrix_minus},
  {"quote_sprow",int_sprowmatrix_quote},
  {"dprim_sprow", int_sprowmatrix_dquote},
  {"multt_sprow_sprow",int_sprowmatrix_multt}, 
  {"redim_sprow",int_sprowmatrix_redim},
  {"matrix_sprow",int_sprowmatrix_redim},
  {"concatd_sprow_sprow" ,  int_sprowmatrix_concatd },
  {"concatr_sprow_sprow" ,  int_sprowmatrix_concatr },
  {"concatdiag_sprow_sprow" ,  int_sprowmatrix_concatdiag },
  {"deletecols_sprow_m", int_sprowmatrix_deletecols},
  {"deleterows_sprow_m", int_sprowmatrix_deleterows},
  {"extract_sprow",int_sprowmatrix_extract},
  {"extractrows_sprow",int_sprowmatrix_extractrows},
  {"extractcols_sprow",int_sprowmatrix_extractcols},
  {"diage_sprow" ,  int_sprowmatrix_diage },
  {"diage_sprow_m" ,  int_sprowmatrix_diage },
  {"diagset_sprow" ,  int_sprowmatrix_diagset },
  {"diagcre_sprow" ,  int_sprowmatrix_diagcre },
  {"diagcre_sprow_m" ,  int_sprowmatrix_diagcre },
  {"diag_sprow", int_sprowmatrix_diag},
  {"diag_sprow_m", int_sprowmatrix_diag},
  {"sprow_sparse", int_sprowmatrix_sparse},
  {"sprow_sparse_sprow", int_sprowmatrix_sparse_sprow},
  {"spget_sprow", int_sprowmatrix_get},
  {"full_sprow",int_sprowmatrix_sp2m},
  {"full_m",int_sprowmatrix_m2m},
  {"sum_sprow_s" ,  int_sprowmatrix_sum },
  {"sum_sprow" ,  int_sprowmatrix_sum },
  {"setrowscols_sprow",int_sprowmatrix_setrc},
  {"max_sprow" ,  int_sprowmatrix_maxi },
  {"max_sprow_s" ,  int_sprowmatrix_maxi },
  {"extractelts_sprow",int_sprowmatrix_extractelts},
  {"nnz_sprow",int_sprowmatrix_nnz},
  {"triu_sprow", int_sprowmatrix_triu},
  {"tril_sprow", int_sprowmatrix_tril},
  {"sprow_eye", int_sprowmatrix_eye},
  {"sprow_ones", int_sprowmatrix_ones},
  {"sprow_zeros", int_sprowmatrix_zeros},
  /* ops */
  {"abs_sprow",int_sprowmatrix_abs},
  {"arg_sprow",int_sprowmatrix_arg},
  {"sin_sprow",int_sprowmatrix_sin},
  {"sinh_sprow",int_sprowmatrix_sinh},
  {"asin_sprow",int_sprowmatrix_asin},
  {"asinh_sprow",int_sprowmatrix_asinh},
  {"cos_sprow",int_sprowmatrix_cos},
  {"cosh_sprow",int_sprowmatrix_cosh},
  {"acos_sprow",int_sprowmatrix_acos},
  {"acosh_sprow",int_sprowmatrix_acosh},
  {"atan_sprow",int_sprowmatrix_atan},
  {"atanh_sprow",int_sprowmatrix_atanh},
  {"ceil_sprow",int_sprowmatrix_ceil},
  {"int_sprow",int_sprowmatrix_int},
  {"floor_sprow",int_sprowmatrix_floor},
  {"round_sprow",int_sprowmatrix_round},
  {"sign_sprow",int_sprowmatrix_sign},
  {"tan_sprow",int_sprowmatrix_tan},
  {"tanh_sprow",int_sprowmatrix_tanh},
  {"conj_sprow",int_sprowmatrix_conj},
  {"find_sprow", int_sprowmatrix_find},
  {"sqrt_sprow",int_sprowmatrix_sqrtel},
  {"log_sprow",int_sprowmatrix_logel},
  {"exp_sprow",int_sprowmatrix_expel},
  {"sprow_rand",int_sprowmatrix_sprand},  
  {"clean_sprow",int_sprowmatrix_clean},  
  {"real_sprow", int_sprowmatrix_real},
  {"imag_sprow", int_sprowmatrix_imag},
  {(char *) 0, NULL}
};

int SpRowMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SpRowMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void SpRowMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SpRowMatrix_func[i].name;
  *f = SpRowMatrix_func[i].fonc;
}








