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

#define SpColMatrix_Private 
#include "nsp/object.h"

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

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"

/*
 * NspSpColMatrix inherits from NspObject 
 */

int nsp_type_spcolmatrix_id=0;
NspTypeSpColMatrix *nsp_type_spcolmatrix=NULL;
int nsp_type_spcolmatrix_init();

NspTypeSpColMatrix *new_type_spcolmatrix(type_mode mode)
{
  NspTypeSpColMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_spcolmatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_spcolmatrix;
    }
  if ((type =  malloc(sizeof(NspTypeSpColMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* spcolmatrix_attrs ;  */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = NULL; /*spcolmatrix_get_methods; */
  type->new = (new_func *) new_spcolmatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for spcolmatrix */ 

  top->pr = (print_func *)nsp_spcolmatrix_print;                  /* printing*/   
  top->dealloc = (dealloc_func *)nsp_spcolmatrix_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_spcolmatrix_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_spcolmatrix_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_spcolmatrix_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_spcolmatrix_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_spcolmatrix_info;                    /* info */  
  /* top->is_true = (is_true_func  *) SpColMatIsTrue;*/      /* check if object can be considered as true */  
  /*top->loop =(loop_func *) SpColLoopExtract ; */               /* for loops */  
  top->path_extract =  NULL;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_spcolmatrix_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_spcolmatrix_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_spcolmatrix_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_spcolmatrix_xdr_save;
  top->load  = (load_func *)nsp_spcolmatrix_xdr_load;

  /* specific methods for spcolmatrix */
  type->init = (init_func *) init_spcolmatrix;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_spcolmatrix_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_spcolmatrix
       */
      type->id =  nsp_type_spcolmatrix_id = nsp_new_type_id();
      nsp_type_spcolmatrix = type;
      if ( nsp_register_type(nsp_type_spcolmatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_spcolmatrix(mode); 
    }
  else 
    {
      type->id = nsp_type_spcolmatrix_id;
      return type;
    }
}
/*
 * initialize SpColmatrix instances 
 * locally and by calling initializer on parent class 
 */

static int init_spcolmatrix(NspSpColMatrix *o,NspTypeSpColMatrix *type)
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

NspSpColMatrix *new_spcolmatrix() 
{
  NspSpColMatrix *loc; 
  /* type must exists */
  nsp_type_spcolmatrix = new_type_spcolmatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspSpColMatrix)))== NULLSPCOL) return loc;
  /* initialize object */
  if ( init_spcolmatrix(loc,nsp_type_spcolmatrix) == FAIL) return NULLSPCOL;
  return loc;
}


/*
 *nsp_spcolmatrix_size: returns filled,hsize,or hsize 
 */

int nsp_spcolmatrix_size(NspSpColMatrix *H, int flag)
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

static char sp_type_name[]="SpColMat";
static char sp_short_type_name[]="sp";

char *nsp_spcolmatrix_type_as_string(void)
{
  return(sp_type_name);
}

char *nsp_spcolmatrix_type_short_string(NspObject *v)
{
  return(sp_short_type_name);
}

int nsp_spcolmatrix_fullcomp(NspSpColMatrix * A,NspSpColMatrix * B,char *op,int *err)
{
  Scierror("SpMatFullComp: to be implemented \n");
  return FALSE;
}

int nsp_spcolmatrix_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_spcolmatrix_id) == FALSE) return FALSE ;
  rep =nsp_spcolmatrix_fullcomp((NspSpColMatrix *) A,(NspSpColMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_spcolmatrix_neq(NspObject *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_spcolmatrix_id) == FALSE) return TRUE;
  rep =nsp_spcolmatrix_fullcomp((NspSpColMatrix *) A,(NspSpColMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}



/*
 * Save a Matrix in a file stream 
 */

static int nsp_spcolmatrix_xdr_save(XDR *xdrs, NspSpColMatrix *M)
{
  int rep=FAIL;
  NspMatrix *RC=NULL, *Values=NULL;
  if ( nsp_spcolmatrix_get(M,&RC,&Values) == FAIL) goto fail;
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

static NspSpColMatrix *nsp_spcolmatrix_xdr_load(XDR *xdrs)
{
  int m,n;
  NspObject *RC=NULL, *Values=NULL;
  NspSpColMatrix *Loc;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSPCOL;
  if (nsp_xdr_load_i(xdrs, &m) == FAIL)  return NULLSPCOL;
  if (nsp_xdr_load_i(xdrs, &n) == FAIL)  return NULLSPCOL;
  if ( (RC= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSPCOL;
  if ( (Values= nsp_object_xdr_load(xdrs) ) == NULL) return NULLSPCOL;
  if ((Loc = nsp_spcolmatrix_sparse(name,(NspMatrix *)RC,(NspMatrix *)Values,m,n)) == NULLSPCOL) return NULLSPCOL;
  if ( RC != NULL) nsp_object_destroy(&RC);
  if ( Values != NULL) nsp_object_destroy(&Values);
  return Loc;
}


/*
 * A =nsp_spcolmatrix_object(O);
 * checks that O is an object of NspSpColMatrix type. 
 * or a Hobj which points to an object of type SpColMatrix
 * if so, returns a pointer to that NspSpColMatrix and else returns NULL
 */

NspSpColMatrix   *nsp_spcolmatrix_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_spcolmatrix_id) == TRUE) return ((NspSpColMatrix *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_spcolmatrix));
  return(NULL);
}


/*
 * IsSpMatObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  SpColMatrix 
 * or a Hobj which points to an object of type SpColMatrix
 */

int IsSpColMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_spcolmatrix_id);
}

/*
 * IsSpColMat(O)
 * only checks that object is an object of type  SpColMatrix 
 * or a Hobj which points to an object of type SpColMatrix
 */

int IsSpColMat(const NspObject *O)
{
  return nsp_object_type(O , nsp_type_spcolmatrix_id);
}

/*
 * Checks that first+i object on the stack 
 * is a Sp and returns that Sp  
 * or a copy of that Sp if its name 
 * is != NVOID 
 */

NspSpColMatrix *GetSpColCopy(Stack stack, int i)
{
  if (  GetSpCol(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a SpColMatrix and returns that SpColMatrix  
 */

NspSpColMatrix *GetSpCol(Stack stack, int i)
{
  NspSpColMatrix *M;
  if (( M =nsp_spcolmatrix_object(NthObj(i))) == NULLSPCOL )
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i object on the stack 
 * is a SpColMatrix and returns that SpColMatrix  
 */

NspSpColMatrix *GetRealSpCol(Stack stack, int i)
{
  NspSpColMatrix *M;
  if (( M =nsp_spcolmatrix_object(NthObj(i))) == NULLSPCOL )
    ArgMessage(stack,i);
  if (M->rc_type == 'c')
    {
      Scierror ("\t%s", ArgPosition (i));
      ArgName (stack, i);
      Scierror (" of function %s should not be complex\n", NspFname(stack));
      return NULLSPCOL ;
    }
  return M;
}


typedef int (*SpC) (NspSpColMatrix *A,NspSpColMatrix *B);


/*
 * Now the interfaced function for basic sparse operations
 */

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPCOLon failure 
 * The matrix is created with no initial value 
 */

static int int_spcolmatrix_create(Stack stack, int rhs, int opt, int lhs)
{  
  int m1,n1;
  NspSpColMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ((HMat =nsp_spcolmatrix_create(NVOID,'r',m1,n1) ) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1, (NspObject *) HMat);
  return 1;
}

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPCOLon failure 
 * sparse(rc,values [, [m,n]])
 */
static int int_spcolmatrix_m2sp(Stack stack, int rhs, int opt, int lhs);

static int int_spcolmatrix_sparse(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpColMatrix *A;
  NspMatrix *RC,*Values,*MN;
  if ( rhs == 1) 
    return  int_spcolmatrix_m2sp(stack,rhs,opt,lhs);
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
      if (( A =nsp_spcolmatrix_sparse(NVOID,RC,Values,(int) MN->R[0],(int) MN->R[1])) == NULLSPCOL) 
	return RET_BUG;
    }
  else
    {
      if (( A =nsp_spcolmatrix_sparse(NVOID,RC,Values,-1,-1)) == NULLSPCOL) 
	return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}

static int int_spcolmatrix_sparse_sp(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((A = GetSpColCopy(stack,1)) == NULLSPCOL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos=1;
  return Max(lhs,1);
}

/*
 * Creation of a Sparse Matrix 
 * returns NULLSPCOLon failure 
 * sparse(rc,values [, [m,n]])
 */

static int int_spcolmatrix_get(Stack stack, int rhs, int opt, int lhs)
{  
  NspSpColMatrix *A;
  NspMatrix *RC,*Values,*MN=NULL;
  CheckRhs(1,1);
  CheckLhs(1,3);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ( nsp_spcolmatrix_get(A,&RC,&Values) == FAIL) return RET_BUG;
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

static int int_spcolmatrix_redim(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspSpColMatrix  *A;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if (( A =nsp_spcolmatrix_redim(A,m1,n1)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * Right Concatenation 
 * A= [A,B] 
 */


static int int_spcolmatrix_concat_gen(Stack stack, int rhs, int opt, int lhs, SpC F)
{
  NspSpColMatrix *HMat1,*HMat2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpCol(stack,1))  == NULLSPCOL) return RET_BUG;
  if ( HMat1->mn == 0) 
    {
      /* return 2 */
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( HMat2->mn == 0) 
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  else
    {
      if ((HMat1 = GetSpColCopy(stack,1))  == NULLSPCOL) return RET_BUG;
      if ( (*F)(HMat1,HMat2)!= OK) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  return 1;
}


static int int_spcolmatrix_concatr(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatr);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spcolmatrix_concatd(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatd);
}

/*
 * Down Concatenation 
 * Res = [A;B] 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

static int int_spcolmatrix_concatdiag(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_concat_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_concatdiag);
}


/*
 *  A(Rows,Cols) = B 
 *  A is changed and enlarged if necessary 
 *  Size Compatibility is checked 
 *  WARNING : A is not Copied we want this routine to change A
 *  =======
 */

static int int_spcolmatrix_setrc(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*B;
  NspMatrix *Rows,*Rows1=NULLMAT,*Cols=NULLMAT,*Cols1=NULLMAT;
  CheckRhs(3,4);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) goto ret_bug;
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
  if ((B = GetSpCol(stack,rhs)) == NULLSPCOL) goto ret_bug;
  if ( B == A ) 
    { if ((B = GetSpColCopy(stack,rhs)) == NULLSPCOL) goto ret_bug;}
  if ( rhs == 3 ) 
    { if (nsp_spcolmatrix_set_row( A, Rows,B) == FAIL) goto ret_bug; }
  else 
    { if (nsp_spcolmatrix_set_rowcol( A, Rows,Cols,B) == FAIL )  goto ret_bug;} 
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

static int int_spcolmatrix_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *Cols;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /* A and Cols can't point to the same object **/
  if ((Cols = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if (nsp_spcolmatrix_delete_cols( A, Cols) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=MatDeleterows(A,Rows)
 * WARNING : A must be changed by this routine
 * =======
 */	

static int int_spcolmatrix_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *Rows;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /* A and Cols can't point to the same object **/
  if ((Rows = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if (nsp_spcolmatrix_delete_rows( A, Rows) < 0) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res=nsp_spcolmatrix_extract(Rows,Cols,A)
 * A unchanged, Rows and Cols are unchanged 
 */	

static int int_spcolmatrix_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*Res;
  NspMatrix *Rows,*Cols;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /* Rows id changed by nsp_spcolmatrix_extract */
  if ((Rows = GetRealMatCopy(stack,2)) == NULLMAT) return RET_BUG;
  if ((Cols = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;
  Res =nsp_spcolmatrix_extract( A, Rows,Cols);
  if ( Res == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * Res=nsp_matrix_extract_elements(Elts,A)
 * A unchanged, Elts
 * WARNING note that on the stack we have Elts,A 
 */	

static int int_spcolmatrix_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*Res;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((Elts = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((Res =nsp_spcolmatrix_extract_elts( A, Elts)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * columns extraction  Cols A --> A(Cols)				  * 
 */	

static int int_spcolmatrix_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*Res;
  NspMatrix *Cols;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((Cols = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_spcolmatrix_extract_cols( A,Cols,&err);
  /* XXXXX Attention ici il faut un message d''erreur **/
  if ( err == 1) return RET_ENDFOR; 
  if ( Res == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * rows extraction 			
 * Rows , A -> A(Rows,:)
 */	

static int int_spcolmatrix_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A,*Res;
  NspMatrix *Rows;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  /* Rows is changed by nsp_spcolmatrix_extract_rows */
  if ((Rows = GetRealMatCopy(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_spcolmatrix_extract_rows( A,Rows,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


/*
 * diag interface 
 */

int int_spcolmatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspSpColMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetSpCol(stack, 1)) == NULLSPCOL)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_spcolmatrix_diag_create (A, k1);
  else
    Res = nsp_spcolmatrix_diag_extract (A, k1);
  if (Res == NULLSPCOL)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Returns the kthe diag of a Matrix 
 */

static int int_spcolmatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpColMatrix *A,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 2) 
    { if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;}
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  Res =nsp_spcolmatrix_diag_extract( A,k1);
  if ( Res == NULLSPCOL)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Set the kth Diag of A to Diag 
 *  A is enlarged & comlexified if necessary 
 *  int nsp_matrix_create_diag(A,Diag,k)
 * WARNING : A is not copied we want this routine to change A
 */

static int int_spcolmatrix_diagset(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspSpColMatrix *A,*Diag;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((Diag = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( GetScalarInt(stack,3,&k1) == FAIL) return RET_BUG;
  if (nsp_spcolmatrix_diag_set( A, Diag,k1) != OK) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 *  Creates a Matrix with kth diag set to Diag 
 */

static int int_spcolmatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSpColMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ( rhs == 2 ) 
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_spcolmatrix_diag_create(Diag,k1)) == NULLSPCOL) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= mx2spmx(A) 
 * A is not changed 
 */

static int int_spcolmatrix_m2sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *Res;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT)  return RET_BUG;
  if (( Res=nsp_spcolmatrix_from_mat(A)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Res= full(A) 
 * A is not changed 
 */

static int int_spcolmatrix_sp2m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *Res;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetSpCol(stack,1)) == NULLSPCOL)  return RET_BUG;
  if (( Res=nsp_spcolmatrix_to_mat(A)) == NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res= full(A) when A is not sparse 
 * do nothing 
 */
#if 0
static int int_spcolmatrix_m2m(Stack stack, int rhs, int opt, int lhs)
{
  /* full(x) when x is already full **/
  NthObj(1)->ret_pos = 1;
  return 1;
}
#endif 


/*
 * Res= A*B 
 * return NULLSPCOLon failure ( incompatible size or No more space )
 * A and B are left unchanged 
 */

/* Generic function for * or .* **/
typedef NspSpColMatrix * (*Sp21) (NspSpColMatrix *A,NspSpColMatrix *B);

static int int_spcolmatrix_mult_gen(Stack stack, int rhs, int opt, int lhs, Sp21 F)
{
  NspSpColMatrix *HMat1,*HMat2,*HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ( HMat1->mn == 0)  
    {
      NSP_OBJECT(HMat1)->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
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
	  if ((HMat3 =nsp_spcolmatrix_create(NVOID,'r',HMat1->m,HMat1->n)) == NULLSPCOL) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else 
	{
	  /* A * <non-nul-scalar> **/
	  if ((HMat1 = GetSpColCopy(stack,1)) == NULLSPCOL) return RET_BUG;
	  if (nsp_spcolmatrix_mult_scal(HMat1,HMat2) != OK) return RET_BUG;
	  NSP_OBJECT(HMat1)->ret_pos = 1;
	}
    }
  else if ( HMat1->mn == 1 ) 
    {
      if ( HMat1->D[0]->size == 0) 
	{
	  /* HMat1 == 0 ==> XXXX res is a null Matrix **/
	  if ((HMat3 =nsp_spcolmatrix_create(NVOID,'r',HMat2->m,HMat2->n)) == NULLSPCOL) 
	    return RET_BUG;
	  MoveObj(stack,1,(NspObject *) HMat3);
	}
      else
	{
	  /* since Mat1 is scalar we store the result in Mat2 so we 
	     must copy it **/
	  if ((HMat2 = GetSpColCopy(stack,2)) == NULLSPCOL) return RET_BUG;
	  if (nsp_spcolmatrix_mult_scal(HMat2,HMat1) != OK) return RET_BUG;
	  NSP_OBJECT(HMat2)->ret_pos = 1;
	}
    }
  else 
    {
      if ((HMat3=(*F)(HMat1,HMat2)) == NULLSPCOL) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  return 1;
}

static int int_spcolmatrix_mult(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_mult_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_mult);
}

/*
 *   Res = A * X , A sparse matrix, X full matrix
 */
static int int_spcolmatrix_mult_sp_m(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat1;
  NspMatrix *HMat2, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
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

  if ( HMat2->m == 1 && HMat2->n == 1 )
    {
      if ((HMat1 = GetSpColCopy(stack,1)) == NULLSPCOL) return RET_BUG;
      if ( nsp_spcolmatrix_mult_scalar(HMat2->R, HMat2->rc_type, HMat1) == FAIL ) return RET_BUG;
      NSP_OBJECT(HMat1)->ret_pos = 1;
    }
  else if ( HMat1->n == HMat2->m )
    {
      if ( (HMat3 = nsp_spcolmatrix_mult_sp_m(HMat1, HMat2)) == NULLMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  else
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return RET_BUG;
    }

  return 1;
}

/*
 *   Res = X * A , A sparse matrix, X full matrix
 *   A and X are left unchanged
 *   added by Bruno
 */

static int int_spcolmatrix_mult_m_sp(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat2;
  NspMatrix *HMat1, *HMat3;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ((HMat1 = GetMat (stack, 1)) == NULLMAT) return RET_BUG;
  if ((HMat2 = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;

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

  if ( HMat1->m == 1 && HMat1->n == 1 )
    {
      if ((HMat2 = GetSpColCopy(stack,2)) == NULLSPCOL) return RET_BUG;
      if ( nsp_spcolmatrix_mult_scalar(HMat1->R, HMat1->rc_type, HMat2) == FAIL ) return RET_BUG;
      NSP_OBJECT(HMat2)->ret_pos = 1;
    }
  else if ( HMat1->n == HMat2->m )
    {
      if ( (HMat3 = nsp_spcolmatrix_mult_m_sp(HMat1, HMat2)) == NULLMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject *) HMat3);
    }
  else
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return RET_BUG;
    }

  return 1;

}

/*
 * Res= A'
 * return NULLSPCOLon failure 
 * A is left unchanged 
 */

static int int_spcolmatrix_quote(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((A =nsp_spcolmatrix_transpose(A)) == NULLSPCOL)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


/*
 * _
 * A'
 */

static int int_spcolmatrix_dquote (Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A, *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);
  if ((A = GetSpCol(stack, 1)) == NULLSPCOL)
    return RET_BUG;
  if ((B = nsp_spcolmatrix_transpose (A)) == NULLSPCOL)
    return RET_BUG;
  nsp_spcolmatrix_conj(B);
  MoveObj (stack, 1, (NspObject *) B);
  return 1;
}



/*
 * C = A + B with special cases [] and [x] 
 */

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

static int int_spcolmatrix_plus(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpColMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((B = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      NspSpColMatrix *Res =nsp_spcolmatrix_add(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  if ( A->mn <= 1) 
    {
      NspMatrix *C=nsp_spcolmatrix_op_scal(B,A,&flag,'+');
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
      NspMatrix *C=nsp_spcolmatrix_op_scal(A,B,&flag,'+');
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
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
}



/*
 * OHMat3 = OHMat1-OHMat2 
 * return NULLSPCOLon failure 
 */
static int int_spcolmatrix_minus(Stack stack, int rhs, int opt, int lhs);

static int int_spcolmatrix_sub(Stack stack, int rhs, int opt, int lhs)
{
  int flag ; 
  NspSpColMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((B = GetSpCol(stack,2)) == NULLSPCOL) return RET_BUG;
  if ( SameDim(A,B) ) 
    {
      NspSpColMatrix *Res =nsp_spcolmatrix_sub(A,B);
      MoveObj(stack,1,(NspObject *) Res);
      return 1;
    }
  if ( A->mn <= 1) 
    {
      NspMatrix *C=nsp_spcolmatrix_op_scal(B,A,&flag,'#'); /* -B + scalar **/
      if ( flag == 1) 
	{
	  /* B -> -B **/
	  stack.first += 1;
	  int_spcolmatrix_minus(stack,1,opt,1);
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
      NspMatrix *C=nsp_spcolmatrix_op_scal(A,B,&flag,'-');
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
  Scierror("Error: incompatible dimensions\n");
  return RET_BUG;
}

/*
 * OHMat3 = OHMat1 .* OHMat2 
 * return NULLSPCOLon failure 
 */

static int int_spcolmatrix_multt(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_mult_gen(stack,rhs,opt,lhs,nsp_spcolmatrix_multtt);
}

/*
 *nsp_mat_sum: sum=Sum(a[,b]) 
 * a is unchanged 
 */

typedef NspSpColMatrix *(*SuPro) (NspSpColMatrix *A,char *);

static int int_spcolmatrix__sum(Stack stack, int rhs, int opt, int lhs, SuPro F)
{
  char *str;
  NspSpColMatrix *Res,*HMat; 
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((HMat = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
    }
  else 
    { str = "F"; }
  if ((Res= (*F)(HMat,str)) == NULLSPCOL) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/* added by Bruno : return the number of non zero elements */
static int int_spcolmatrix_nnz(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *HMat; 
  NspMatrix *Res;
  int  nnz;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
  if ((Res = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT ) return FAIL;

  nnz = nsp_spcolmatrix_nnz(HMat);
  Res->R[0] = (double) nnz;
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_spcolmatrix_sum(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spcolmatrix__sum(stack,rhs,opt,lhs,nsp_spcolmatrix_sum) );
}


/*
 * nsp_mat_maxi: Maxi(*HMat);
 * A is unchanged 
 * XXXXXX : pas fini 
 */

typedef NspSpColMatrix *(*SpMiMax) (NspSpColMatrix *A,char *,NspMatrix **Imax,int lhs);

static int int_spcolmatrix__maxi(Stack stack, int rhs, int opt, int lhs, SpMiMax F,int minmax)
{
  char *str;
  NspSpColMatrix *A,*M;
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
      if ((A = GetSpCol(stack,1)) == NULLSPCOL) return RET_BUG;
      if ( rhs == 2) 
	{
	  if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
	}
      else 
	{ str = "F"; }
      if (( M= (*F)(A,str,&Imax,lhs)) == NULLSPCOL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(M));
      if ( lhs == 2)
	{
	  MoveObj(stack,2,NSP_OBJECT(Imax));
	}
    }
  else
    {
      int index= (lhs == 2 ) ? 1 : 0, err;
      NspSpColMatrix *Index=NULL;
      if ((A = GetSpColCopy(stack,1))  == NULLSPCOL) return RET_BUG;
      if ((M = GetSpColCopy(stack,2))  == NULLSPCOL) return RET_BUG;
      Index = nsp_spcolmatrix_maximinitt_g(A,M,index,minmax,&err);
      if (err == TRUE ) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      if ( lhs == 2 ) 
	{
	  MoveObj(stack,2,NSP_OBJECT(Index));
	}
    }
  return Max(lhs,1);
}


static int int_spcolmatrix_maxi(Stack stack, int rhs, int opt, int lhs)
{
  return ( int_spcolmatrix__maxi(stack,rhs,opt,lhs,nsp_spcolmatrix_maxi,1));
}

static int int_spcolmatrix_mini(Stack stack, int rhs, int opt, int lhs)
{
  /* return ( int_spcolmatrix__maxi(stack,rhs,opt,lhs,nsp_spcolmatrix_mini,-1)); */
  return RET_BUG;
}

/* interface for triu 
 *
 */

static int int_spcolmatrix_triu(Stack stack, int rhs, int opt, int lhs)
{
  int k=0;
  NspSpColMatrix *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if ( nsp_spcolmatrix_triu(A,k)==FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;

}

/* interface for tril
 *
 */

static int int_spcolmatrix_tril(Stack stack, int rhs, int opt, int lhs)
{
  int k=0;
  NspSpColMatrix *A;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( rhs == 2) 
    {
      if (GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if ( nsp_spcolmatrix_tril(A,k)==FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/* interface for eye,ones,zeros
 *
 */

typedef NspSpColMatrix *(Feoz)(int m,int n);

static int int_spcolmatrix_eoz(Stack stack, int rhs, int opt, int lhs,Feoz *F)
{
  int m,n;
  NspSpColMatrix *A;
  CheckStdRhs(1,2);
  CheckLhs(1,1);
  if ( rhs == 1 ) 
    {
      m=nsp_object_get_size(NthObj(1),1);
      n=nsp_object_get_size(NthObj(1),2);
    }
  else 
    {
      if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
    }
  if ((A= (*F)(m,n))== NULLSPCOL) return RET_BUG;
  MoveObj(stack,1, NSP_OBJECT(A));
  return 1;
}

static int int_spcolmatrix_ones(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_eoz(stack,rhs,opt,lhs,nsp_spcolmatrix_ones);
}

static int int_spcolmatrix_eye(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_eoz(stack,rhs,opt,lhs,nsp_spcolmatrix_eye);
}

static int int_spcolmatrix_zeros(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix_eoz(stack,rhs,opt,lhs,nsp_spcolmatrix_zeros);
}


static int int_spcolmatrix_clean(Stack stack, int rhs, int opt, int lhs)
{
  double epsa =1.e-10, epsr = 1.e-10;
  NspSpColMatrix *A;
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
  if ((A = GetSpColCopy(stack, 1)) == NULL)
    return RET_BUG;
  nsp_spcolmatrix_clean (A, rhs, epsa, epsr);
  NSP_OBJECT (A)->ret_pos = 1;
  return 1;
}

static int int_spcolmatrix_sprand(Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  char crand='u';
  double sparsity=0.1;
  int m,n;
  NspSpColMatrix *A;
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
  if ((A= nsp_spcolmatrix_rand(m,n,sparsity,crand))== NULLSPCOL) return RET_BUG;
  MoveObj(stack,1, NSP_OBJECT(A));
  return 1;
}

/*
 *  A=op(A) 
 */

typedef int (*M11) (NspSpColMatrix *A);
typedef void (*VM11) (NspSpColMatrix *A);
 
static int int_spcolmatrix__gen11(Stack stack, int rhs, int opt, int lhs, M11 F)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if ((*F)(A) == FAIL) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

static int int_spcolmatrix__genv11(Stack stack, int rhs, int opt, int lhs, VM11 F)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  (*F)(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}




/*
 *nsp_spmatrix_minus: A=-(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_minus(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_minus);
}


/*
 * A=Abs(A), absolue value or module of each element 
 */

static int int_spcolmatrix_abs(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_abs);
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

static int int_spcolmatrix_arg(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_arg);
}

/*
 *nsp_spmatrix_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */

typedef NspMatrix* (*SpM) (NspSpColMatrix *A);

static int int_spcolmatrix__m_gen11(Stack stack, int rhs, int opt, int lhs, SpM F)
{
  NspMatrix *Loc;
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ((Loc = (*F)(A)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Loc);
  return 1;
}

static int int_spcolmatrix_cos(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_cos);
}

/*
 *nsp_spmatrix_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */
static int int_spcolmatrix_cosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_cosh);
}

/*
 * SpExpl : Exponentiation terme a term 
 * A is changed 
 */

static int int_spcolmatrix_expel(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_expel);
}

/*
 * SpLog : A=LogEl(A) 
 */

static int int_spcolmatrix_logel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if (nsp_spcolmatrix_logel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_spmatrix_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_sin(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_sin);
}

/*
 *nsp_spmatrix_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_sinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_sinh);
}

/*
 *nsp_spmatrix_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_sqrtel(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->mn == 0) 
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
  if (nsp_spcolmatrix_sqrtel(A) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 *nsp_spmatrix_acos: A=Acos(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_acos(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_acos);
}

/*
 *nsp_spmatrix_acosh: A=Acosh(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_acosh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__m_gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_acosh);
}

/*
 *nsp_spmatrix_asin: A=Asin(A)
 * A is changed  
 * return 0 if error 
 */

static int int_spcolmatrix_asin(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_asin);
}

/*
 *nsp_spmatrix_asinh: A=Asinh(A)
 * A is changed  
 * return 0 if error 
 */


static int int_spcolmatrix_asinh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_asinh);
}

/*
 * SpATan : res= arctang(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spcolmatrix_atan(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_atan);
}

/*
 * SpArcTangH : res= atanh(A) 
 * A is not changed, A must be squared
 * returns 0 on failure 
 */

static int int_spcolmatrix_atanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_atanh);
}

/*
 *nsp_spmatrix_ceil: A=Ceil(A)
 * A is changed  
 */

static int int_spcolmatrix_ceil(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_ceil);
}

/*
 *nsp_spmatrix_int: A=Int(A)
 * A is changed  
 */

static int int_spcolmatrix_int(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_int);
}

/*
 *nsp_spmatrix_floor: A=Floor(A)
 * A is changed  
 */
 
static int int_spcolmatrix_floor(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_floor);
}

/*
 *nsp_spmatrix_round: A=Round(A)
 * A is changed  
 */
 
static int int_spcolmatrix_round(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_round);
}

/*
 *nsp_spmatrix_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spcolmatrix_sign(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_sign);
}

/*
 *nsp_spmatrix_tan: A=Tan(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spcolmatrix_tan(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_tan);
}

/*
 *nsp_spmatrix_tanh: A=Tanh(A)
 * A is changed  
 * return 0 if error 
 */
 
static int int_spcolmatrix_tanh(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__genv11(stack,rhs,opt,lhs,nsp_spcolmatrix_tanh);
}

/*
 * A=Polar(A,B),  * A is changed 
 */

/*
 *nsp_spmatrix_conj: A=real(A)-i*Imag(A)
 * A is changed  if imaginary not changed if real 
 */

static int int_spcolmatrix_conj(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A=GetSpCol(stack,1))== NULLSPCOL) return RET_BUG;
  if ( A->mn == 0 || A->rc_type == 'r' )
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ((A=GetSpColCopy(stack,1))== NULLSPCOL) return RET_BUG;
 nsp_spcolmatrix_conj(A);
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

static int int_spcolmatrix_find(Stack stack, int rhs, int opt, int lhs)
{
  NspSpColMatrix *A;
  NspMatrix *Rc,*Rr;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((A = GetSpCol(stack,1)) == NULLSPCOL)  return RET_BUG;
  if (nsp_spcolmatrix_find(A,Max(lhs,1),&Rr,&Rc) == FAIL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Rr));
  if ( lhs == 2 )
    {
      NthObj(2) = (NspObject *) Rc;
      NthObj(2)->ret_pos = 2;
      return 2;
    }
  return 1;
}

static int int_spcolmatrix_real(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_realpart);
}

static int int_spcolmatrix_imag(Stack stack, int rhs, int opt, int lhs)
{
  return int_spcolmatrix__gen11(stack,rhs,opt,lhs,nsp_spcolmatrix_imagpart);
}


/*
 * The Interface for basic numerical sparse matrices operation 
 * we use sp for spcol 
 */

static OpTab SpColMatrix_func[]={
  {"sp_create",int_spcolmatrix_create},
  {"spcol_create",int_spcolmatrix_create},
  {"m2sp",int_spcolmatrix_m2sp},
  {"sp2m",int_spcolmatrix_sp2m},
  {"dst_sp_sp",int_spcolmatrix_multt},
  {"mult_sp_sp",int_spcolmatrix_mult},
  {"mult_sp_m",int_spcolmatrix_mult_sp_m},
  {"mult_m_sp",int_spcolmatrix_mult_m_sp},
  {"plus_sp_sp",int_spcolmatrix_plus},
  {"minus_sp_sp",int_spcolmatrix_sub},
  {"minus_sp_m",int_spcolmatrix_sub},
  {"minus_m_sp",int_spcolmatrix_sub},
  {"minus_sp",int_spcolmatrix_minus},
  {"quote_sp",int_spcolmatrix_quote},
  {"dprim_sp", int_spcolmatrix_dquote},
  {"multt_sp_sp",int_spcolmatrix_multt}, 
  {"redim_sp",int_spcolmatrix_redim},
  {"matrix_sp",int_spcolmatrix_redim},
  {"concatd_sp_sp" ,  int_spcolmatrix_concatd },
  {"concatr_sp_sp" ,  int_spcolmatrix_concatr },
  {"concatdiag_sp_sp" ,  int_spcolmatrix_concatdiag },
  {"deletecols_sp_m", int_spcolmatrix_deletecols},
  {"deleterows_sp_m", int_spcolmatrix_deleterows},
  {"extract_sp",int_spcolmatrix_extract},
  {"extractrows_sp",int_spcolmatrix_extractrows},
  {"extractcols_sp",int_spcolmatrix_extractcols},
  {"diage_sp" ,  int_spcolmatrix_diage },
  {"diage_sp_m" ,  int_spcolmatrix_diage },
  {"diagset_sp" ,  int_spcolmatrix_diagset },
  {"diagcre_sp" ,  int_spcolmatrix_diagcre },
  {"diagcre_sp_m" ,  int_spcolmatrix_diagcre },
  {"diag_sp", int_spcolmatrix_diag},
  {"diag_sp_m", int_spcolmatrix_diag},
  {"sparse", int_spcolmatrix_sparse},
  {"sparse_sp", int_spcolmatrix_sparse_sp},
  {"spget", int_spcolmatrix_get},
  {"full_sp",int_spcolmatrix_sp2m},
  {"sum_sp_s" ,  int_spcolmatrix_sum },
  {"sum_sp" ,  int_spcolmatrix_sum },
  {"setrowscols_sp",int_spcolmatrix_setrc},
  {"max_sp" ,  int_spcolmatrix_maxi },
  {"max_sp_s" ,  int_spcolmatrix_maxi },
  {"extractelts_sp",int_spcolmatrix_extractelts},
  {"nnz_sp",int_spcolmatrix_nnz},
  {"triu_sp", int_spcolmatrix_triu},
  {"tril_sp", int_spcolmatrix_tril},
  {"sp_eye", int_spcolmatrix_eye}, /* to be removed ? */
  {"sp_ones", int_spcolmatrix_ones},/* to be removed ? */
  {"sp_zeros", int_spcolmatrix_zeros},/* to be removed ? */
  {"speye", int_spcolmatrix_eye},
  {"spones", int_spcolmatrix_ones},
  {"spzeros", int_spcolmatrix_zeros},
  /* now operations */
  {"abs_sp",int_spcolmatrix_abs},
  {"arg_sp",int_spcolmatrix_arg},
  {"sin_sp",int_spcolmatrix_sin},
  {"sinh_sp",int_spcolmatrix_sinh},
  {"asin_sp",int_spcolmatrix_asin},
  {"asinh_sp",int_spcolmatrix_asinh},
  {"cos_sp",int_spcolmatrix_cos},
  {"cosh_sp",int_spcolmatrix_cosh},
  {"acos_sp",int_spcolmatrix_acos},
  {"acosh_sp",int_spcolmatrix_acosh},
  {"atan_sp",int_spcolmatrix_atan},
  {"atanh_sp",int_spcolmatrix_atanh},
  {"ceil_sp",int_spcolmatrix_ceil},
  {"int_sp",int_spcolmatrix_int},
  {"floor_sp",int_spcolmatrix_floor},
  {"round_sp",int_spcolmatrix_round},
  {"sign_sp",int_spcolmatrix_sign},
  {"tan_sp",int_spcolmatrix_tan},
  {"tanh_sp",int_spcolmatrix_tanh},
  {"conj_sp",int_spcolmatrix_conj},
  {"find_sp", int_spcolmatrix_find},
  {"sqrt_sp",int_spcolmatrix_sqrtel},
  {"log_sp",int_spcolmatrix_logel},
  {"exp_sp",int_spcolmatrix_expel},
  {"sprand",int_spcolmatrix_sprand},  
  {"clean_sp",int_spcolmatrix_clean},  
  {"real_sp", int_spcolmatrix_real},
  {"imag_sp", int_spcolmatrix_imag},
  {(char *) 0, NULL}
};

/*
 * The Interface for basic matrices operation
 */

int SpColMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SpColMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) **/

void SpColMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SpColMatrix_func[i].name;
  *f = SpColMatrix_func[i].fonc;
}




 


