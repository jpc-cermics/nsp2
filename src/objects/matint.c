/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2006-2009 Bruno Pincon Esial/Iecn
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
#include <string.h>

#define  Matint_Private 
#include "nsp/object.h"
#include "nsp/matint.h" 
#include "nsp/interf.h"
#include "nsp/gsort-p.h"
#include "../interp/Eval.h"
#include "nsp/imatrix.h"
/* 
 * Interface NspMatint 
 * FIXME: should contains methods which are to be implemented 
 *        by all Objects which behaves like matrices. 
 * the only method actually implemented is redim 
 * Note that the interface is to be used at nsp level and 
 * internally.
 */

int nsp_type_matint_id=0;
NspTypeMatint *nsp_type_matint=NULL;

NspTypeMatint *new_type_matint(type_mode mode)
{
  NspTypeObject *top;
  NspTypeMatint *type = NULL;
  if ( nsp_type_matint != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_matint;
    }

  if ((type =  calloc(1,sizeof(NspTypeMatint)))==NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;

  top = NSP_TYPE_OBJECT (type->surtype);
  while (top->surtype != NULL)
    top = NSP_TYPE_OBJECT (top->surtype);

  /* object methods redefined for matint */

  top->s_type = (s_type_func *) matint_type_as_string;

  /* specific methods for matint */
      
  type->init = NULL;
  type->redim =  nsp_matint_redim; 
  type->copy_ind = nsp_matint_basic_copy;
      
  /* 
   * Matint interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_matint_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatint called nsp_type_matint
       */
      type->id =  nsp_type_matint_id = nsp_new_type_id();
      nsp_type_matint = type;
      if ( nsp_register_type(nsp_type_matint) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_matint(mode);
    }
  else 
    {
      type->id = nsp_type_matint_id;
      return type;
    }
}

/* these method is necessary when registering types */

static char matint_type_name[] = "Matint";

static char *matint_type_as_string (void)
{
  return (matint_type_name);
}



/* 
 * method redim 
 * A.redim[m,n] at nsp level 
 *   i.e an interface for redim
 *   this interface is in the methods table of the interface matint.
 *   we call the associated redim function which is supposed to be
 *   implemented in the matint interface. 
 */

static int int_matint_meth_redim(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  NspTypeBase *type;
  NspMatrix *B;
  int mm,nn;
  CheckRhs (1,2);
  CheckLhs (0,0); 

 if ( rhs == 1 )
    { 
      if ((B = GetRealMat (stack, 1)) == NULLMAT)
	return RET_BUG;
      if (B->mn != 2)
	{
	  Scierror ("Error:\t expecting a vector of size 2\n");
	  return RET_BUG;
	}
      mm = (int) B->R[0];
      nn = (int) B->R[1];
    }
  else
    {
      if (GetScalarInt (stack, 1, &mm) == FAIL) return RET_BUG;
      if (GetScalarInt (stack, 2, &nn) == FAIL) return RET_BUG;
    }

  if ( mm < -1 || nn < -1 )
    {
      Scierror("Error:\tBad arguments (must be >= -1)\n");
      return RET_BUG;
    }

  /* interface is supposed to be initialized 
   *  nsp_type_matint= new_type_matint(T_BASE); 
   */
  /* search nsp_type_matint interface which is implemented 
   * by self 
   * FIXME: note that we have already searched for matint 
   *        before calling this function: maybe we should 
   *        add here the good type in the arg list ? ... 
   */
  if (( type = check_implements(self,nsp_type_matint_id)) == NULL )
    {
      Scierror("Object do not implements matint interface\n");
      return RET_BUG;
    }
  /* 
   * here we could call directly 
   * return nsp_matint_redim(self, m, n) == OK ? 0 : RET_BUG;
   * considering that all the matrices implementing matint 
   * will use default redim function. But just in case one 
   * class decided to particularize redim we have to 
   * call MAT_INT(type)->redim(self,m1,n1)
   * 
   */
  return MAT_INT(type)->redim(self,mm,nn)  == OK ? 0 : RET_BUG;
}


static int int_matint_meth_concatr(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  NspObject *B;
  NspTypeBase *typeSelf,*typeB;
  CheckRhs (1,1);
  CheckLhs (0,0);
  if ((B = nsp_get_object(stack, 1)) == NULL)   return RET_BUG;
  typeB = check_implements(B, nsp_type_matint_id);
  typeSelf = check_implements(self, nsp_type_matint_id);
  
  if ( typeB != typeSelf )
    {
      Scierror("Error: A.concatr[B], A and B must be of the same type\n");
      return RET_BUG;
    }
  if ( nsp_matint_concat_right_bis(self,B)==FAIL) 
    return RET_BUG;
  return 0;
}

static int int_matint_meth_concatd(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  NspObject *B;
  NspTypeBase *typeSelf,*typeB;
  CheckRhs (1,1);
  CheckLhs (0,0);
  if ((B = nsp_get_object(stack, 1)) == NULL)   return RET_BUG;
  typeB = check_implements(B, nsp_type_matint_id);
  typeSelf = check_implements(self, nsp_type_matint_id);
  
  if ( typeB != typeSelf )
    {
      Scierror("Error: A.concatd[B], A and B must be of the same type\n");
      return RET_BUG;
    }
  if ( nsp_matint_concat_down_bis(self,B)==FAIL) 
    return RET_BUG;
  return 0;
}

/* 
 * method perm_elem
 */

static int int_matint_perm_elem(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  int p, q, dim=0;
  CheckRhs (2,3);
  CheckLhs (0,0); 

  if ( GetScalarInt(stack, 1, &p) == FAIL ) return RET_BUG;
  if ( GetScalarInt(stack, 2, &q) == FAIL ) return RET_BUG;
  if (rhs == 3)
    {
      if ( GetDimArg(stack, 3, &dim) == FAIL )
	return RET_BUG;
    }

  if ( nsp_matint_perm_elem(self, p, q, dim) == FAIL )
    return RET_BUG;

  return 0;
}

static NspCells *nsp_matint_to_cells(NspSMatrix *A,int  direction);


/* 
 * method to_cells
 */
static int int_matint_to_cells(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  NspCells *C;
  int dim=0;
  CheckRhs (0,1);
  CheckLhs (0,1); 
  if (rhs == 1)
    {
      if ( GetDimArg(stack, 1, &dim) == FAIL )
	return RET_BUG;
    }
  if ( (C = nsp_matint_to_cells((NspSMatrix *)self,dim)) == NULLCELLS) 
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(C));
  return 1;
}

/* method set_diag for matint objects  
 *
 */

static int nsp_matint_set_diag(NspObject *ObjA,NspObject *ObjB,int k);

static int int_matint_meth_set_diag(NspObject *self, Stack stack, int rhs, int opt, int lhs) 
{
  int k=0;
  NspObject *B;
  NspTypeBase *typeSelf,*typeB;
  CheckRhs (1,2);
  CheckLhs (0,0);
  if ((B = nsp_get_object(stack, 1)) == NULL)   return RET_BUG;
  typeB = check_implements(B, nsp_type_matint_id);
  typeSelf = check_implements(self, nsp_type_matint_id);
  if ( typeB != typeSelf )
    {
      Scierror("Error: A.set_diag[B], A and B must be of the same type\n");
      return RET_BUG;
    }
  if ( rhs == 2 )
    {
      if (GetScalarInt (stack, 2, &k) == FAIL) return RET_BUG;
    }
  if ( nsp_matint_set_diag(self,B,k)==FAIL) 
    return RET_BUG;
  return 0;
}


static NspMethods matint_methods[] = {
  {"redim",(nsp_method *) int_matint_meth_redim},
  {"concatr",(nsp_method *) int_matint_meth_concatr},
  {"concatd",(nsp_method *) int_matint_meth_concatd},
  {"perm_elem",(nsp_method *) int_matint_perm_elem},
  {"to_cells",(nsp_method *) int_matint_to_cells},
  {"set_diag",(nsp_method *) int_matint_meth_set_diag},
  { NULL, NULL}
};


NspMethods *matint_get_methods(void) { return matint_methods;};


/**
 * nsp_matint_indices_for_deletions:
 * @nb_elts: integer giving the size of @ind
 * @ind: array of integers 
 * @count: integer pointer 
 * 
 * reorders if needed the array vector @ind and in case of duplicated 
 * indices it compress the array
 * used for deletions operations (A(ind,:)=[], A(:,ind)=[], A(ind)=[])
 * 
 **/

static void nsp_matint_indices_for_deletions(int nb_elts, int *ind, int *count)
{
  int i, j, in_order=1, in_strict_order=1;

      
  for ( i = 1 ; i < nb_elts ; i++ )
    {
      if ( ind[i] <= ind[i-1] )
	{
	  in_strict_order = 0;
	  if ( ind[i] != ind[i-1] )  /* so ind[i] < ind[i-1] */
	    in_order = 0;
	}
    }

  if ( ! in_order )
    nsp_qsort_int(ind,NULL,FALSE,nb_elts,'i');
      
  if ( ! in_strict_order )  /* may be there are duplicated indices */
    {
      i = 0;
      j = 0;
      while ( j < nb_elts )
	{
	  /*  here ind[j] is the current element to examine for duplication
           *  all elements before (if any) are such that ind[k] < ind[j]
           */
	  if ( j == nb_elts-1 )
	    ind[i] = ind[j];
	  else
	    {
	      while ( j+1 < nb_elts  &&  ind[j+1] == ind[j] )
		j++;
	      ind[i] = ind[j];
	    }
	  i++; j++;
	}
      *count = i;
    }
  else
    *count = nb_elts;
}



/**
 * nsp_get_index_vector:
 * @stack: stack object 
 * @ipos: position to check in stack 
 * @Obj: a #NspObject pointer 
 * @index: an #index_vector
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_get_index_vector(Stack stack, int ipos,NspObject **Obj,index_vector *index)
{
  NspObject *Index;
  if ((Index =nsp_get_object(stack,ipos)) == NULLOBJ ) 
    return FAIL;
  if ( Obj != NULL ) *Obj = NSP_OBJECT(Index); 
  return  NSP_OBJECT(Index)->type->as_index(NSP_OBJECT(Index),index);
}


/**
 * nsp_get_index_vector_from_object:
 * @Obj: an Object 
 * @index: an #index_vector
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_get_index_vector_from_object(NspObject *Obj,index_vector *index) 
{
  HOBJ_GET_OBJECT(Obj,FAIL); /* message for FAIL */
  return  NSP_OBJECT(Obj)->type->as_index(NSP_OBJECT(Obj),index);
}


/**
 * nsp_matint_delete_columns:
 * @Obj: a #NspObject which implements #matint
 * @index: an #index_vector
 *
 * delete columns (specified by ind) of object @Obj which must implements the #matint interface 
 * A(:,ind) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_columns(NspObject  *Obj,index_vector *index)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val;
  unsigned int elt_size; /* size in number of bytes */
  NspTypeBase *type; 
  int i, j, k1, k2, nn, ncol, ioff=0;

  if ( index->nval == 0) return OK;

  if ( index->min < 1 || index->max > A->n )
    {
      Scierror("Error:\tIndices out of bounds\n"); 
      return FAIL;
    }

  nsp_matint_indices_for_deletions(index->nval, index->val, &ncol);

  if ( (type= check_implements(A,nsp_type_matint_id)) == NULL ) 
    { 
      Scierror("Object do not implements matint interface\n"); 
      return FAIL; 
    } 


  MAT_INT(type)->canonic(Obj);
  Val = (char *) A->S;

  elt_size = MAT_INT(type)->elt_size(A); 
  if ( MAT_INT(type)->free_elt != NULL)  
    {
      for ( j = 0 ; j < ncol ; j++ )  
	{
	  int k=index->val[j]*A->m;
	  for ( i = 0 ; i < A->m ; i++ )  
	    MAT_INT(type)->free_elt((void **) &(A->S[i+k])); 
	}
    }
  
  for ( i = 0 ; i < ncol ; i++)
    {
      k1 = index->val[i];
      k2 = (i < ncol-1 ) ? index->val[i+1] : A->n;
      nn = (k2-k1-1)*A->m;
      /* nb of elts to move = nb of elts strictly between columns k1 and k2 */
      if ( nn != 0) 
	memmove(Val +(k1-ioff)*A->m*elt_size,Val + (k1+1)*A->m*elt_size, nn*elt_size);
      ioff++;
    }

  if ( MAT_INT(type)->free_elt != NULL) 
    for ( i = A->mn - ncol*A->m ; i < A->mn ; i++ ) A->S[i]= NULL;

  if ( MAT_INT(type)->resize(A,A->m,A->n-ncol) == FAIL) return FAIL;
  return OK;
}

/**
 * nsp_matint_delete_rows:
 * @Obj: a #NspObject which implements #matint
 * @index: an #index_vector
 *
 * delete rows (specified by ind) of object @Obj which must implements the #matint interface 
 * A(ind,:) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_rows(NspObject *Obj,index_vector *index)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val;
  unsigned int elt_size; /* size in number of bytes */
  NspTypeBase *type; 
  int i, j, k1, k2, nn, nrow, stride=0, ioff=0;

  if ( index->nval == 0) return OK;

  if ( index->min < 1 || index->max > A->m )
    {
      Scierror("Error:\tIndices out of bounds\n"); 
      return FAIL;
    }

  nsp_matint_indices_for_deletions(index->nval, index->val , &nrow);

  if ( (type =check_implements(A,nsp_type_matint_id)) == NULL ) 
    {  
      Scierror("Object do not implements matint interface\n"); 
      return FAIL; 
    } 

  MAT_INT(type)->canonic(Obj);
  Val = (char *) A->S;

  elt_size = MAT_INT(type)->elt_size(A); 

  if ( MAT_INT(type)->free_elt != NULL)  
    {
      for ( i = 0 ; i < nrow ; i++ ) 
	{
	  int k=index->val[i];
	  for ( j = 0 ; j < A->n ; j++ ) 
	    MAT_INT(type)->free_elt((void **) &(A->S[k+A->m*j])); 
	}
    }

  for ( j = 0 ; j < A->n  ; j++)
    {
      k1 = index->val[0] + stride;
      for ( i = 0 ; i < nrow ; i++)
	{
	  if ( i < nrow-1 ) 
	    k2 =  index->val[i+1] + stride;
	  else 
	    k2 = ( j < A->n-1) ? index->val[0] + stride + A->m : A->mn;
	  nn = k2-k1-1;
	  if ( nn != 0) 
	    memmove(Val + (k1-ioff)*elt_size, Val + (k1+1)*elt_size, nn*elt_size);
	  ioff++;
	  k1 = k2;
	}
      stride += A->m;
    }

  if ( MAT_INT(type)->free_elt != NULL) 
    {
      for ( i = A->mn- nrow*A->n ; i < A->mn ; i++ ) 
	A->S[i] = NULL;
    }

  if ( MAT_INT(type)->resize(A, A->m-nrow, A->n) == FAIL) return FAIL;
  return OK;
}

/**
 * nsp_matint_tozero:
 * @Obj: a #NspObject which implements #matint
 *
 * delete all the elements of object @Obj which must implements the #matint interface 
 * Obj(:) = [], Obj(:,:) = []
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_matint_tozero(NspObject *Obj)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  NspTypeBase *type; 
  int i;

  if (( type = check_implements(A,nsp_type_matint_id)) == NULL ) 
    { 
      Scierror("Object do not implements matint interface\n"); 
      return FAIL; 
    } 

  if ( MAT_INT(type)->free_elt != NULL)  
    for ( i = 0 ; i < A->mn ; i++ )  MAT_INT(type)->free_elt((void **) &(A->S[i])); 

  FREE(A->S); A->S = NULL;
  A->mn = A->m = A->n = 0;

  MAT_INT(type)->canonic(Obj);

  return OK;
}



/**
 * nsp_matint_delete_elements:
 * @Obj: a #NspObject which implements #matint
 * @index: an #index_vector
 *
 * delete the elements of object @Obj which must implements the #matint interface 
 * A(ind) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_elements(NspObject *Obj,index_vector *index)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val;
  unsigned int elt_size; /* size in number of bytes */
  int i, k1, k2, nn, ne, ioff=0; 
  NspTypeBase *type; 

  if ( index->nval == 0) return OK;

  if ( index->min < 1 || index->max > A->mn )
    {
      Scierror("Error:\tIndices out of bounds\n"); 
      return FAIL;
    }
  
  nsp_matint_indices_for_deletions(index->nval, index->val, &ne);

  if (( type = check_implements(A,nsp_type_matint_id)) == NULL ) 
    { 
      Scierror("Object do not implements matint interface\n"); 
      return FAIL; 
    } 

  MAT_INT(type)->canonic(Obj);
  Val = (char *) A->S;

  elt_size = MAT_INT(type)->elt_size(A); 

  if ( MAT_INT(type)->free_elt != NULL)  
    for ( i = 0 ; i < ne ; i++ )  MAT_INT(type)->free_elt((void **) &(A->S[index->val[i]])); 

  k1 = index->val[0];
  for ( i = 0 ; i < ne ; i++)
    {
      k2 = ( i < ne-1 ) ? index->val[i+1] : A->mn;
      nn = k2-k1-1;
      if ( nn != 0) 
	memmove(Val + (k1-ioff)*elt_size, Val + (k1+1)*elt_size, nn*elt_size);
      ioff++;
      k1 = k2;
    }

  if ( MAT_INT(type)->free_elt != NULL) 
    for ( i = A->mn-ne ; i < A->mn ; i++ ) A->S[i]= NULL;

  if ( A->m == 1)
    {
      if ( MAT_INT(type)->resize(A,1,A->mn-ne) == FAIL) return FAIL;
    }
  else
    {
      if ( MAT_INT(type)->resize(A,A->mn-ne,1) == FAIL) return FAIL;
    }
  return OK;
}

/**
 * nsp_matint_delete_elements2:
 * @Obj: a #NspObject which implements #matint
 * @indrow: integer vecteur with the row indices (0-based)
 * @nr: length of @row that is number of row indices
 * @rmin: min index (1-based) in row
 * @rmax: max index (1-based) in row
 * @indcol: integer vecteur with the column indices (0-based)
 * @nc: length of @col that is number of column indices
 * @cmin: min index (1-based) in col
 * @cmax: max index (1-based) in col
 *
 * delete elements of object @Obj which must implements the #matint interface 
 * A(indrow,indcol) = []
 * 
 * Return value: %OK or %FAIL.
 **/
int nsp_matint_delete_elements2(NspObject *Obj, 
				int *indrow, int nr, int rmin, int rmax,
				int *indcol, int nc, int cmin, int cmax)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val;
  unsigned int elt_size; /* size in number of bytes */
  int i, j, ne, k1, k2, nn, nrow, ncol, ioff=0; 
  NspTypeBase *type; 

  if ( nr == 0 || nc == 0 ) return OK;

  if ( rmin < 1 || rmax > A->m || cmin < 1 || cmax > A->n )
    {
      Scierror("Error:\tIndices out of bounds\n"); 
      return FAIL;
    }

  nsp_matint_indices_for_deletions(nr, indrow, &nrow);
  nsp_matint_indices_for_deletions(nc, indcol, &ncol);

  if (( type = check_implements(A,nsp_type_matint_id)) == NULL ) 
    { 
      Scierror("Object do not implements matint interface\n"); 
      return FAIL; 
    } 
  
  MAT_INT(type)->canonic(Obj);
  Val = (char *) A->S;
  
  elt_size = MAT_INT(type)->elt_size(A);
 
  if ( MAT_INT(type)->free_elt != NULL)  
    for ( i = 0 ; i < nrow ; i++ ) 
      for ( j = 0 ; j < ncol ; j++ ) 
	MAT_INT(type)->free_elt((void **) &(A->S[indrow[i]+A->m*indcol[j]])); 

  k1 = indrow[0]+A->m*indcol[0];
  for ( j = 0 ; j < ncol ; j++ ) 
    {
      int offset = A->m*indcol[j];
      for ( i = 0 ; i < nrow ; i++)
	{
	  /* compute in k2 the point to delete after the current 
	   * one or return A->mn at the end 
	   */
	  if ( i < nrow-1 )
	    {
	      k2 = indrow[i+1] + offset;
	    }
	  else 
	    {
	      if ( j == ncol-1 ) 
		k2 = A->mn;
	      else 
		k2 = indrow[0] + A->m*indcol[j+1];
	    }
	  nn = k2-k1-1;
	  if ( nn != 0) 
	    {
	      memmove(Val + (k1-ioff)*elt_size, Val + (k1+1)*elt_size, nn*elt_size);
	    }
	  ioff++;
	  k1 = k2;
	}
    }
  
  ne = nrow*ncol;
  if ( MAT_INT(type)->free_elt != NULL) 
    for ( i = A->mn-ne ; i < A->mn ; i++ ) A->S[i]= NULL;

  
  if ( nrow == A->m )       /* just resize columns */
    {
      if ( MAT_INT(type)->resize(A,A->m,A->n-ncol) == FAIL) return FAIL;
    }
  else if ( ncol == A->n )  /* just resize rows */
    {
      if ( MAT_INT(type)->resize(A,A->m-nrow,A->n) == FAIL) return FAIL;
    }
  else if ( A->m == 1)
    {
      if ( MAT_INT(type)->resize(A,1,A->mn-ne) == FAIL) return FAIL;
    }
  else
    {
      if ( MAT_INT(type)->resize(A,A->mn-ne,1) == FAIL) return FAIL;
    }
  return OK;
}



/**
 * nsp_matint_extract_elements:
 * @Obj: a #Matrix (that is a #NspObject which implements the matint interface)
 * @Elt: a NspObject from which @index was extracted
 * @index: an #index_vector
 *
 * Compute Obj(ind) and returns the new #Matrix 
 * 
 * returns: a #Matrix or %NULLOBJ
 */

static NspObject *nsp_matint_extract_elements(NspObject *Obj,NspObject *Elts,index_vector *index)
{
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *from,*to;
  NspObject *Loc; 
  NspSMatrix *B;
  int i;
  NspTypeBase *type; 
  unsigned int elt_size; /* size in number of bytes */

  if (( type = check_implements(Obj,nsp_type_matint_id)) == NULL )
    {
      Scierror("Error: first argument does not implements matint interface\n");
      return NULLOBJ;
    }

  /*
   * be sure that Obj is in a proper state.
   */ 

  MAT_INT(type)->canonic(Obj);
  from = (char *) A->S;
  
  if ( index->nval == 0 )
    {
      /* Matlab mode ! */
      int m= nsp_object_get_size(Elts,1);
      int n= nsp_object_get_size(Elts,2);
      if ( m== 1 || n==1) 
	{
	  /* here the returned size is Ox1,or 1x0, or 0x0 
	   */
	  if ( A->m == 1 ) 
	    {
	      if ( A->n == 1) 
		return MAT_INT(type)->clone(NVOID, Obj, 0, 0, FALSE);
	      else 
		return MAT_INT(type)->clone(NVOID, Obj, 1, 0, FALSE);
	    }
	  else if ( A->n == 1)
	    return MAT_INT(type)->clone(NVOID, Obj, 0, 1, FALSE);
	}
      return MAT_INT(type)->clone(NVOID, Obj, m, n, FALSE);
    }

  elt_size = MAT_INT(type)->elt_size(Obj); 

  if ( A->m == 0 || A->n == 0) 
    {
      return nsp_object_copy(Obj);
    }

  if ( index->min < 1 || index->max > A->mn )
    {
      Scierror("Error:\tIndices out of bound\n");
      return NULLOBJ;
    }

  if ( A->m == 1 && A->n > 1 )
    {
      if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, 1, index->nval, FALSE)) == NULLOBJ ) 
	return NULLOBJ;
    }
  else
    {
      if ( (Loc =MAT_INT(type)->clone(NVOID, Obj,index->nval, 1,FALSE)) == NULLOBJ ) 
	return NULLOBJ;
    }

  B = (NspSMatrix *) Loc; to = (char *) B->S;

  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  
    {
      /* Matrix of numbers or booleans 
       */
      if ( index->flag )   /* val goes from min to max by step of 1 */
	{
	  memcpy(to,from + elt_size*index->val[0], elt_size*index->nval);
	}
      else if ( elt_size == sizeof(double) )
	{
	  double *fromd = (double *) from, *tod = (double *) to;
	  for ( i = 0 ; i < index->nval ; i++ )
	    tod[i] = fromd[index->val[i]];
	}
      else if ( elt_size == sizeof(doubleC) )
	{
	  doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
	  for ( i = 0 ; i < index->nval ; i++ )
	    toc[i] = fromc[index->val[i]];
	}
      else if ( elt_size == sizeof(int) )
	{
	  int *fromi = (int *) from, *toi = (int *) to;
	  for ( i = 0 ; i < index->nval ; i++ )
	    toi[i] = fromi[index->val[i]];
	}
      else
	{
	  for ( i = 0 ; i < index->nval ; i++ )
	    memcpy(to + i*elt_size, from + index->val[i]*elt_size, elt_size);
	}
    }
  else                                                        
    {
      /* Matrix of pointers (cells, strings, poly,...) 
       */
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      for ( i = 0 ; i < index->nval ; i++ )
	{
	  if ( fromv[index->val[i]] == NULL )  
	    {
	      /* just for cells which may have undefined elements */
	      NspMatrix *M= nsp_matrix_create("ce",'r',0,0);
	      if ( M == NULLMAT) return NULLOBJ;
	      fromv[index->val[i]] = (char *)  M;
	    }
	  if ( (elt = (char *) MAT_INT(type)->copy_elt(fromv[index->val[i]])) == NULL )
	    {
	      nsp_object_destroy(&Loc); 
	      return NULLOBJ;
	    }
	  tov[i] = elt;
	}
    }
  return Loc;
}

/**
 * nsp_matint_extract_elements1:
 * @Obj: a #Matrix (that is a #NspObject which implements the matint interface)
 * @Elts: a NspObject 
 *
 * Computes @Obj(@Elts) and returns the new #Matrix 
 *
 * Returns: a new #NspObject with same type as @Obj or %NULLOBJ
 **/

NspObject *nsp_matint_extract_elements1(NspObject *Obj,NspObject *Elts)
{
  index_vector index={0};
  NspObject *Res;
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Elts,&index) == FAIL) return NULLOBJ ;
  Res = nsp_matint_extract_elements(Obj,Elts,&index);
  nsp_free_index_vector_cache(&index);
  return Res;
}


/**
 * nsp_matint_extract_columns:
 * @Obj: a #Matrix (that is a #NspObject which implements the matint interface)
 * @Elt: a NspObject from which @index was extracted
 * @index: an #index_vector
 * 
 * Obj(:,ind) and returns the new #Matrix 
 * 
 * returns: a #Matrix or %NULLOBJ
 */

static NspObject *nsp_matint_extract_columns(NspObject *Obj,NspObject *Elts,index_vector *index)
{
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *from, *to;
  NspObject *Loc;
  NspSMatrix *B;
  int i, j, k, ij, L;
  NspTypeBase *type; 
  unsigned int elt_size; /* size in number of bytes */

  type = check_implements(Obj, nsp_type_matint_id);

  if ( index->nval == 0 )
    return MAT_INT(type)->clone(NVOID, Obj, A->m, 0, FALSE);

  if ( index->min < 1 || index->max > A->n ) 
    {
      Scierror("Error:\tIndices out of bound\n");
      return NULLOBJ;
    }

  /* be sure that Obj is in a canonic form 
   * Note also that it can change elt_size which must 
   * be called after 
   */

  MAT_INT(type)->canonic(Obj);
  elt_size = MAT_INT(type)->elt_size(Obj); 

  from = (char *) A->S;

  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, A->m, index->nval, FALSE)) == NULLOBJ ) 
    return NULLOBJ;

  B = (NspSMatrix *) Loc; to = (char *) B->S;
  
  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      L = A->m * elt_size;
      for ( j = 0 ; j < index->nval ; j++, to += L )
	memcpy(to, from + L*index->val[j], L);
    }
  else                                                     /* Matrix of pointers (cells, strings, poly,...) */
    {
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      for ( j = 0, k = 0 ; j < index->nval ; j++ )
	for ( i = 0, ij = A->m*index->val[j] ; i < A->m ; i++, ij++, k++ )
	  {
	    if ( fromv[ij] != NULL )   /* just for cells which may have undefined elements */
	      {
		if ( (elt = (char *) MAT_INT(type)->copy_elt(fromv[ij])) == NULL )
		  {
		    nsp_object_destroy(&Loc); 
		    return NULLOBJ;
		  }
		tov[k] = elt;
	      }
	  }
    }
  return Loc;
}

/**
 * nsp_matint_extract_columns1:
 * @Obj: a #NspObject which implements the matint interface.
 * @Cols: a #NspObject
 * 
 * Compute Obj(:,Cols) and returns the new #Matrix 
 * 
 * Returns: an object with same type as @Obj or %NULLOBJ
 **/

NspObject *nsp_matint_extract_columns1(NspObject *Obj,NspObject *Cols)
{
  index_vector index={0};
  NspObject *Res;
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Cols,&index) == FAIL) return NULLOBJ ;
  Res = nsp_matint_extract_columns(Obj,Cols,&index);
  nsp_free_index_vector_cache(&index);
  return Res; 
}


/*  Here we split the previous function according to the data 
 *  size of the array to be copied. 
 */

#define EXTRACT_ROW_PREAMBLE(Type)					\
  int i, j;								\
  Type *A = (Type *) Obj;						\
  NspObject *Loc;							\
  NspTypeBase *type = check_implements(Obj, nsp_type_matint_id);	\
									\
  if ( index->nval == 0 )							\
    return MAT_INT(type)->clone(NVOID, Obj, 0, A->n, FALSE);		\
									\
  if ( index->min < 1 || index->max > A->m )				\
    {									\
      Scierror("Error:\tIndices out of bound\n");			\
      return NULLOBJ;							\
    }									\
  									\
  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, index->nval, A->n, FALSE)) == NULLOBJ ) \
    return NULLOBJ;


static NspObject *nsp_matint_extract_rows_mat(NspObject *Obj,NspObject *Elts, index_vector *index)
{
  EXTRACT_ROW_PREAMBLE(NspMatrix);
  
  MAT_INT(type)->canonic(Obj);

  if ( A->rc_type == 'r') 
    {
      double *from = A->R,*from1 = from, *to = ((NspMatrix *) Loc)->R;
      for ( j = 0 ; j < (A)->n ; j++ )
	{
	  for ( i = 0 ; i < index->nval ; i++ )
	    *(to++) = from1[index->val[i]];
	  from1 += (A)->m;
	}
    }
  else 
    {
      doubleC *from = (A)->C,*from1 = from, *to = ((NspMatrix *) Loc)->C;
      for ( j = 0 ; j < (A)->n ; j++ )
	{
	  for ( i = 0 ; i < index->nval ; i++ )
	    *(to++) = from1[index->val[i]];
	  from1 += (A)->m;
	}
    }
  return Loc;
}

static NspObject *nsp_matint_extract_rows_int(NspObject *Obj,NspObject *Elts, index_vector *index)
{
  int *from, *from1, *to;
  EXTRACT_ROW_PREAMBLE(NspBMatrix);
  from = from1= A->B; 
  to = ((NspBMatrix *) Loc)->B;
  for ( j = 0 ; j < ((NspSMatrix *) A)->n ; j++ )
    {
      for ( i = 0 ; i < index->nval ; i++ )
	*(to++) = from1[index->val[i]];
      from1 += ((NspMatrix *) A)->m;
    }
  return Loc;
}

static NspObject *nsp_matint_extract_rows_gen(NspObject *Obj,NspObject *Elts,  index_vector *index)
{
  unsigned int elt_size;
  char *from, *to; 
  EXTRACT_ROW_PREAMBLE(NspSMatrix);
  from = (char *) A->S;
  to = (char *) ((NspSMatrix *) Loc)->S;
  elt_size = MAT_INT(type)->elt_size(A); /* size in number of bytes */
  for ( j = 0 ; j < ((NspSMatrix *) A)->n ; j++ )
    {
      int jm = j*((NspSMatrix *) A)->m;
      for ( i = 0 ; i < index->nval ; i++, to += elt_size )
	memcpy(to, from + (index->val[i]+ jm)*elt_size, elt_size);
    }
  return Loc;
}

static NspObject *nsp_matint_extract_rows_pointer(NspObject *Obj,NspObject *Elts, index_vector *index)
{
  char **fromv, **fromv1, **tov, *elt;
  EXTRACT_ROW_PREAMBLE(NspSMatrix);
  fromv = (char **) A->S; 
  fromv1 = fromv ;
  tov = (char **) ((NspSMatrix *) Loc)->S;
  for ( j = 0 ; j < A->n ; j++ )
    {
      for ( i = 0 ; i < index->nval ; i++ )
	{
	  char *fromvi = fromv1[index->val[i]];
	  if ( fromvi != NULL )   /* just for cells which may have undefined elements */
	    {
	      if ( (elt = (char *) MAT_INT(type)->copy_elt(fromvi)) == NULL )
		{
		  nsp_object_destroy(&Loc); 
		  return NULLOBJ;
		}
	      *(tov++) = elt;
	    }
	}
      fromv1 += A->m;
    }
  return Loc;
}

/**
 * nsp_matint_extract_rows:
 * @Obj: a #Matrix (that is a #NspObject which implements the matint interface)
 * @Elt: a NspObject from which @index was extracted
 * @index: an #index_vector
 *
 * Compute Obj(ind,:) and returns the new #Matrix 
 * 
 * returns: a #Matrix or %NULLOBJ
 */

static NspObject *nsp_matint_extract_rows(NspObject *Obj,NspObject *Elts, index_vector *index)
{
  NspTypeBase *type= check_implements(Obj, nsp_type_matint_id);
  switch ( MAT_INT(type)->copy_ind  ) 
    {
    case nsp_matint_basic_copy_pointer:
      return nsp_matint_extract_rows_pointer(Obj,Elts,index);
    case nsp_matint_basic_copy_mat:
      return nsp_matint_extract_rows_mat(Obj,Elts,index);
    case nsp_matint_basic_copy_int:
      return nsp_matint_extract_rows_int(Obj,Elts,index);
    case nsp_matint_basic_copy: 
      return nsp_matint_extract_rows_gen(Obj,Elts,index);
    }
  return NULLOBJ;
}


/**
 * nsp_matint_extract_rows1:
 * @Obj: a #NspObject which implements the matint interface.
 * @Rows: a #NspObject
 * 
 * Compute Obj(Rows,:) and returns the new #Matrix 
 * 
 * Returns: an object with same type as @Obj or %NULLOBJ
 **/

NspObject *nsp_matint_extract_rows1(NspObject *Obj,NspObject *Rows)
{
  index_vector index={0};
  NspObject *Res;
  index.iwork = matint_iwork1;
  if ( nsp_get_index_vector_from_object(Rows,&index) == FAIL) return NULLOBJ ;
  Res =  nsp_matint_extract_rows(Obj,Rows,&index);
  nsp_free_index_vector_cache(&index);
  return Res;
}

/**
 * nsp_matint_extract:
 * @Obj: a #Matrix (that is a #NspObject which implements the matint interface)
 * @row: integer vecteur with the row indices (0-based)
 * @nr: length of @row that is number of row indices
 * @rmin: min index (1-based) in row
 * @rmax: max index (1-based) in row
 * @col: integer vecteur with the column indices (0-based)
 * @nc: length of @col that is number of column indices
 * @cmin: min index (1-based) in col
 * @cmax: max index (1-based) in col
 *
 * Compute Obj(row,col) and returns the new #Matrix 
 * 
 * returns: a #Matrix or %NULLOBJ
 */

static NspObject *nsp_matint_extract(NspObject *Obj, index_vector *index_r, index_vector *index_c)
{
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *from, *to;
  NspObject *Loc;
  NspSMatrix *B;
  int i, j, k, stride;
  NspTypeBase *type; 
  unsigned int elt_size; /* size in number of bytes */

  type = check_implements(Obj, nsp_type_matint_id);

  if ( index_r->nval == 0 || index_c->nval == 0 )
     return MAT_INT(type)->clone(NVOID, Obj, index_r->nval, index_c->nval, FALSE);

  if ( index_r->min < 1 || index_r->max > A->m ||  index_c->min < 1 ||  index_c->max > A->n )
    {
      Scierror("Error:\tIndices out of bound\n");
      return NULLOBJ;
    }
  
  /* be sure that Obj is in a canonic form 
   * Note also that it can change elt_size which must 
   * be called after 
   */
  
  MAT_INT(type)->canonic(Obj);
  elt_size = MAT_INT(type)->elt_size(Obj); 

  from = (char *) A->S;

  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, index_r->nval,index_c->nval, FALSE)) == NULLOBJ ) 
    return NULLOBJ;

  B = (NspSMatrix *) Loc; to = (char *) B->S;

  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  
    {
      if ( nsp_object_type (Obj, nsp_type_matrix_id) || 
	   nsp_object_type (Obj, nsp_type_mpmatrix_id)
	   )
	{
	  /* Matrix of numbers or booleans */
	  if ( elt_size == sizeof(double) )
	    {
	      double *fromd = (double *) from, *tod = (double *) to;
	      for ( j = 0, k = 0 ; j < index_c->nval ; j++ )
		{        
		  stride =  index_c->val[j]*A->m;
		  for ( i = 0 ; i <  index_r->nval ; i++, k++ )
		    tod[k] = fromd[index_r->val[i]+ stride];
		}
	    }
	  else if ( elt_size == sizeof(doubleC) )
	    {
	      doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
	      for ( j = 0, k = 0 ; j < index_c->nval ; j++ )
		{        
		  stride =  index_c->val[j]*A->m;
		  for ( i = 0 ; i < index_r->nval  ; i++, k++ )
		    toc[k] = fromc[index_r->val[i]+ stride];
		}
	    }
	}
      else if ( elt_size == sizeof(int) )
	{
	  int *fromi = (int *) from, *toi = (int *) to;
	  for ( j = 0, k = 0 ; j <  index_c->nval ; j++ )
	    {        
	      stride =  index_c->val[j]*A->m;
	      for ( i = 0 ; i < index_r->nval ; i++, k++ )
		toi[k] = fromi[index_r->val[i]+ stride];
	    }
	}
      else
	{
	  for ( j = 0 ; j < index_c->nval ; j++ )
	    {
	      stride =  index_c->val[j]*A->m;
	      for ( i = 0 ; i < index_r->nval ; i++, to += elt_size )
		memcpy(to, from + (index_r->val[i]+ stride)*elt_size, elt_size);
	    }
	}
    }
  else                                            
    {
      /* Matrix of pointers (cells, strings, poly,...) */
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      for ( j = 0, k = 0 ; j < index_c->nval ; j++ )
	{
	  stride = index_c->val[j]*A->m;
	  for ( i = 0 ; i < index_r->nval ; i++, k++ )
	    {
	      if ( fromv[index_r->val[i] + stride] != NULL )
		{
		  /* just for cells which may have undefined elements */
		  if ( (elt = (char *) MAT_INT(type)->copy_elt(fromv[index_r->val[i]+stride])) == NULL )
		    {
		      nsp_object_destroy(&Loc); 
		      return NULLOBJ;
		    }
		  tov[k] = elt;
		}
	    }
	}
    }
  return Loc;
}


/**
 * nsp_matint_extract1:
 * @Obj: a #NspObject
 * @Rows: a #NspObject
 * @Cols: a #NspObject
 * 
 * returns in a new NspObject @Obj(@Rows,@Cols).
 * 
 * Returns: a #NspObject or %NULL 
 **/
NspObject *nsp_matint_extract1(NspObject *Obj,NspObject *Rows, NspObject *Cols)
{
  index_vector index_r={0},index_c={0};
  NspObject *Res;
  index_r.iwork = matint_iwork1;
  index_c.iwork = matint_iwork2;
  if ( nsp_get_index_vector_from_object(Rows,&index_r) == FAIL) return NULLOBJ ;
  if ( nsp_get_index_vector_from_object(Cols,&index_c) == FAIL) return NULLOBJ ;
  Res = nsp_matint_extract(Obj,&index_r,&index_c);
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  return Res;
}


static int nsp_matint_special_set_submatrix(NspObject *ObjA, 
					    const int *row, int nr, const int *col, int nc, 
					    NspObject *ObjB, NspTypeBase *type)
{
  /* just for insertion A(row,col) = B when A and B are Matrix or MaxpMatrix
   * and when A is complex and B is real
   */
  NspMatrix *A = (NspMatrix *) ObjA, *B =  (NspMatrix *) ObjB;
  int i, j, k, stride, inc;
  /*   if ( ! ( type == nsp_type_matrix ||  type == nsp_type_maxpmatrix ) ) */
  /*     { */
  /*       Scierror("Error:\tsomething wrong in nsp_matint_special_set_submatrix\n"); */
  /*       return FAIL; */
  /*     } */
  
  inc = B->mn == 1 ? 0 : 1;
  for ( j = 0, k = 0 ; j < nc ; j++ )
    {
      stride = col[j]*A->m;
      for ( i = 0 ; i < nr ; i++, k+=inc )
	{
	  A->C[row[i] + stride].r = B->R[k];
	  A->C[row[i] + stride].i = 0.0;
	}
    }
  return OK;
}

static int nsp_matint_special_set_elts(NspObject *ObjA, 
				       const int *ind, int nb_elts,
				       NspObject *ObjB, NspTypeBase *type)
{
  /* just for insertion A(ind) = B when A and B are Matrix or MaxpMatrix
   * and when A is complex and B is real
   */
  NspMatrix *A = (NspMatrix *) ObjA, *B =  (NspMatrix *) ObjB;
  int i, k, inc;
  /*   if ( ! ( type == nsp_type_matrix ||  type == nsp_type_maxpmatrix ) ) */
  /*     { */
  /*       Scierror("Error:\tsomething wrong in nsp_matint_special_set_submatrix\n"); */
  /*       return FAIL; */
  /*     } */
  
  inc = B->mn == 1 ? 0 : 1;
  for ( i = 0, k = 0 ; i < nb_elts ; i++, k+=inc )
    {
      A->C[ind[i]].r = B->R[k];
      A->C[ind[i]].i = 0.0;
    }
  return OK;
}

/**
 * nsp_matint_set_submatrix:
 * @ObjA: #NspObject which implements the matint interface
 * @index_r: an index_vector for row
 * @index_c: an index_vector for column 
 * @ObjB: #NspObject which implements the matint interface
 * 
 * 
 * Compute ObjA(row,col) = ObjB modifying ObjA in place
 * 
 * Returns:  %OK or %FAIL.
 **/
int nsp_matint_set_submatrix(NspObject *ObjA, index_vector *index_r, index_vector *index_c, NspObject *ObjB)
{
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB;
  char *to, *from;
  int i, j, k, stride;
  NspTypeBase *typeA, *typeB; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  typeA = check_implements(ObjA, nsp_type_matint_id);
  typeB = check_implements(ObjB, nsp_type_matint_id);

  if ( typeA != typeB )
    {
      Scierror("Error: in A(i,j)=B, A and B must be of same type\n");
      return FAIL;
    }

  if ( B->mn != 1)
    {
      if ( index_r->nval != B->m || index_c->nval  != B->n )
	{
	  Scierror("Error: in A(i,j)=B, incompatible dimensions (indices range versus B size)\n");
	  return FAIL;
	}
    }
  else if ( index_r->nval == 0 || index_c->nval == 0 )
    {
      /* case: A(indr,indc) = scalar , with an empty range for indr x indc  */
      /* do nothing (this is the matlab behavior)                           */
      return OK;
    }

  if ( index_r->min < 1 || index_c->min < 1 ) 
    {
      Scierror("Error: in A(i,j)=B, non positive indices are not allowed\n");
      return FAIL;
    }

  MAT_INT(typeA)->canonic(ObjA);
  MAT_INT(typeB)->canonic(ObjB);

  elt_size_A = MAT_INT(typeA)->elt_size(ObjA); 
  elt_size_B = MAT_INT(typeB)->elt_size(ObjB); 


  if ( index_r->max > A->m ||  index_c->max > A->n )
    {
      if ( MAT_INT(typeA)->enlarge(ObjA, index_r->max, index_c->max) == FAIL )
	return FAIL;
    }

  if ( nsp_object_type (ObjA, nsp_type_matrix_id) || 
       nsp_object_type (ObjA, nsp_type_mpmatrix_id))
    {
      if ( elt_size_A != elt_size_B )  /* FIXME: explain these lines ! */
	{
	  /* FIXME: add a test to verify if typeA is NspMatrix or NspMaxpMatrix */
	  NspMatrix *AA = (NspMatrix *) ObjA, *BB = (NspMatrix *) ObjB;
	  AA = Mat2double(AA); 
	  elt_size_A = AA->rc_type == 'r' ? sizeof(double) : 2*sizeof(double);
	  BB = Mat2double(BB);
	  elt_size_B = BB->rc_type == 'r' ? sizeof(double) : 2*sizeof(double);      
	}

      if ( elt_size_A < elt_size_B )  /* just because A is real and B complex... */
	{
	  /* FIXME: add a test to verify if typeA is NspMatrix or NspMaxpMatrix */
	  NspMatrix *AA = (NspMatrix *) ObjA;
	  if ( nsp_mat_complexify(AA,0.00) == FAIL ) return FAIL; 
	  elt_size_A = elt_size_B;
	}
    }

  to = (char *) A->S; from = (char *) B->S;
  
  if ( MAT_INT(typeA)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      if ( elt_size_B < elt_size_A )  /* just because A is complex and B real... */
	return nsp_matint_special_set_submatrix(ObjA, index_r->val,index_r->nval, index_c->val,index_c->nval, ObjB, typeA);

      if ( nsp_object_type (ObjA, nsp_type_matrix_id) || 
	   nsp_object_type (ObjA, nsp_type_mpmatrix_id))
	{
	  if ( elt_size_A == sizeof(double) )
	    {
	      double *fromd = (double *) from, *tod = (double *) to;
	      int inc = B->mn == 1 ? 0 : 1;
	      for ( j = 0, k = 0 ; j < index_c->nval ; j++ )
		{
		  stride =  index_c->val[j]*A->m;
		  for ( i = 0 ; i < index_r->nval ; i++, k+=inc )
		    tod[index_r->val[i]+ stride] = fromd[k];
		}
	    }
	  else if ( elt_size_A == sizeof(doubleC) )
	    {
	      doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
	      int inc = B->mn == 1 ? 0 : 1; 
	      for ( j = 0, k = 0 ; j < index_c->nval ; j++ )
		{        
		  stride =  index_c->val[j]*A->m;
		  for ( i = 0 ; i < index_r->nval ; i++, k+=inc )
		    toc[index_r->val[i]+ stride] = fromc[k];
		}
	    }
	}
      else if ( elt_size_A == sizeof(int) )
	{
	  int *fromi = (int *) from, *toi = (int *) to;
	  int inc = B->mn == 1 ? 0 : 1; 
	  for ( j = 0, k = 0 ; j < index_c->nval ; j++ )
	    {        
	      stride =  index_c->val[j]*A->m;
	      for ( i = 0 ; i < index_r->nval ; i++, k+=inc )
		toi[index_r->val[i]+ stride] = fromi[k];
	    }
	}
      else
	{
	  int inc = B->mn == 1 ? 0 : elt_size_A; 
	  for ( j = 0 ; j < index_c->nval ; j++ )
	    {
	      stride =  index_c->val[j]*A->m;
	      for ( i = 0 ; i < index_r->nval ; i++, from += inc )
		memcpy(to + (index_r->val[i]+ stride)*elt_size_A, from, elt_size_A);
	    }
	}
    }
  else                                                    
    {
      /* Matrix of pointers (cells, strings, poly,...) */
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      int ij, inc = B->mn == 1 ? 0 : 1; 
      for ( j = 0, k = 0 ; j < index_c->nval ; j++ )
	{
	  stride = index_c->val[j]*A->m;
	  for ( i = 0 ; i < index_r->nval ; i++, k+=inc )
	    {
	      ij = index_r->val[i]+stride;
	      MAT_INT(typeA)->free_elt( (void **) (tov + ij) ); 
	      if ( fromv[k] != NULL )          /* just for cells which may have undefined elements */
		{
		  if ( (elt = (char *) MAT_INT(typeA)->copy_elt(fromv[k])) == NULL ) return FAIL;
		  tov[ij] = elt;
		}
	    }
	}
    }
  return OK;
}

/**
 * nsp_matint_set_submatrix1:
 * @ObjA: a #NspObject which implements the matint interface.
 * @Row: a #NspObject
 * @Col: a #NspObject
 * @ObjB: a #NspObject
 * 
 * @ObjA(@Row,@Col) = @ObjB
 * 
 * Returns:  %OK or %FAIL 
 **/
int nsp_matint_set_submatrix1(NspObject *ObjA,NspObject *Row, NspObject *Col, NspObject *ObjB)
{
  index_vector index_r={0},index_c={0};
  int Res;
  index_r.iwork = matint_iwork1;
  index_c.iwork = matint_iwork2;
  if ( nsp_get_index_vector_from_object(Row,&index_r) == FAIL) return FAIL;
  if ( nsp_get_index_vector_from_object(Col,&index_c) == FAIL) return FAIL;
  Res = nsp_matint_set_submatrix(ObjA,&index_r, &index_c, ObjB);
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  return Res;
}


/**
 * nsp_matint_set_elts:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @index: an #index_vector
 * @ObjB: a #Matrix (that is a #NspObject which implements the matint interface)
 *
 * Compute ObjA(ind) = ObjB modifying ObjA in place
 * 
 * returns:  %OK or %FAIL.
 */


int nsp_matint_set_elts(NspObject *ObjA, index_vector *index, 	NspObject *ObjB)
{
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB;
  char *to, *from;
  int i, k;
  NspTypeBase *typeA, *typeB; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */


  typeA = check_implements(ObjA, nsp_type_matint_id);
  typeB = check_implements(ObjB, nsp_type_matint_id);

  if ( typeA != typeB )
    {
      Scierror("Error:\tA(...)= B, A and B must be of the same type\n");
      return FAIL;
    }

  if ( B->m != 1 && B->n != 1 ) 
    {
      Scierror("Error:\tA(ind)=B, B must be a vector");
      return FAIL;
    }

  if ( index->nval == 0 && B->mn <= 1)
    {
      /* ignore the set elts */
      return OK;
    }

  if ( index->min < 1 )
    {
      Scierror("Error:\tNon Positive indices are not allowed\n");
      return FAIL;
    }

  if ( A->m == 1  &&  A->n > 1  &&  B->m != 1 )
    {
      Scierror("Error:\tA(ind)=B, B must be row when A is a row\n");
      return FAIL;
    } 
  
  if ( A->n == 1 &&  A->m > 1  &&  B->n != 1 )
    {
      Scierror("Error:\tA(ind)=B, B must be column when A is a column\n");
      return FAIL;
    }

  if ( B->mn != 1)
    {
      if ( index->nval  != B->mn )
	{
	  Scierror("Error:\tA(ind)=B, ind and B have incompatible sizes\n");
	  return FAIL;
	}
    }

  MAT_INT(typeA)->canonic(ObjA);
  MAT_INT(typeB)->canonic(ObjB);

  elt_size_A = MAT_INT(typeA)->elt_size(ObjA); 
  elt_size_B = MAT_INT(typeB)->elt_size(ObjB); 

  if ( index->max > A->mn )  /* enlarge A */
    {
      if ( A->mn == 0) 
	{
	  if  ( B->m == 1 )  /* ( B->n != 1) */
	    { 
	      if ( MAT_INT(typeA)->enlarge(ObjA, 1, index->max) == FAIL ) return FAIL;
	    }
	  else
	    { 
	      if ( MAT_INT(typeA)->enlarge(ObjA, index->max, 1) == FAIL ) return FAIL;
	    }
	}
      else if ( A->m == 1 )
	{
	  if ( A->n == 1 )
	    {
	      if ( B->m == 1 )   /* ( B->n > 1 ) */
		{ 
		  if ( MAT_INT(typeA)->enlarge(ObjA, 1, index->max) == FAIL ) return FAIL;
		}
	      else
		{
		  if ( MAT_INT(typeA)->enlarge(ObjA, index->max, 1) == FAIL ) return FAIL;
		}
	    }
	  else
	    {
	      if ( MAT_INT(typeA)->enlarge(ObjA, 1, index->max) == FAIL ) return FAIL;
	    }
	}
      else if ( A->n == 1)
	{
	  if ( MAT_INT(typeA)->enlarge(ObjA, index->max, 1) == FAIL ) return FAIL;
	}
      else
	{
	  Scierror("Error:\tA(ind)=B, ind must be inside A range when A is not a vector\n");
	  return FAIL;
	}
    }

  if ( nsp_object_type (ObjA, nsp_type_matrix_id)|| 
       nsp_object_type (ObjA, nsp_type_mpmatrix_id))
    {
      if ( elt_size_A != elt_size_B )  /* FIXME: explain these lines ! */
	{
	  /* FIXME: add a test to verify if typeA is NspMatrix or NspMaxpMatrix */
	  NspMatrix *AA = (NspMatrix *) ObjA, *BB = (NspMatrix *) ObjB;
	  AA = Mat2double(AA); 
	  elt_size_A = AA->rc_type == 'r' ? sizeof(double) : 2*sizeof(double);
	  BB = Mat2double(BB);
	  elt_size_B = BB->rc_type == 'r' ? sizeof(double) : 2*sizeof(double);      
	}
      if ( elt_size_A < elt_size_B )  /* just because A is real and B complex... */
	{
	  /* FIXME: add a test to verify if typeA is NspMatrix or NspMaxpMatrix */
	  NspMatrix *AA = (NspMatrix *) ObjA;
	  if ( nsp_mat_complexify(AA,0.00) == FAIL ) return FAIL; 
	  elt_size_A = elt_size_B;
	}
    }
  
  to = (char *) A->S; from = (char *) B->S;

  if ( MAT_INT(typeA)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      if ( elt_size_B < elt_size_A )    /* just because A is complex and B real... */
	return nsp_matint_special_set_elts(ObjA, index->val, index->nval, ObjB, typeA);

      if ( index->flag  &&  B->mn == index->nval )  /* values go from min to max by step of 1 */
	{
	  memcpy(to+ elt_size_A*index->val[0],from, elt_size_A*index->nval);
	}
      else 
	{
	  if ( nsp_object_type (ObjA, nsp_type_matrix_id)|| 
	       nsp_object_type (ObjA, nsp_type_mpmatrix_id))
	    {
	      if ( elt_size_A == sizeof(double) )
		{
		  double *fromd = (double *) from, *tod = (double *) to;
		  int inc = B->mn == 1 ? 0 : 1; 
		  for ( i = 0, k=0 ; i < index->nval ; i++, k+=inc )
		    tod[index->val[i]] = fromd[k];
		}
	      else if ( elt_size_A == sizeof(doubleC) )
		{
		  doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
		  int inc = B->mn == 1 ? 0 : 1; 
		  for ( i = 0, k=0 ; i < index->nval ; i++, k+=inc )
		    toc[index->val[i]] = fromc[k];
		}
	    }
	  else if ( elt_size_A == sizeof(int) )
	    {
	      int *fromi = (int *) from, *toi = (int *) to;
	      int inc = B->mn == 1 ? 0 : 1; 
	      for ( i = 0, k=0 ; i < index->nval ; i++, k+=inc )
		toi[index->val[i]] = fromi[k];
	    }
	  else
	    {
	      int inc = B->mn == 1 ? 0 : elt_size_A; 
	      for ( i = 0 ; i < index->nval ; i++, from += inc )
		memcpy(to + index->val[i]*elt_size_A, from, elt_size_A);
	    }
	}
    }
  else                                                     /* Matrix of pointers (cells, strings, poly,...) */
    {
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      int inc = B->mn == 1 ? 0 : 1; 
      for ( i = 0, k = 0 ; i < index->nval ; i++, k+=inc )
	{
	  MAT_INT(typeA)->free_elt( (void **) (tov + index->val[i]) ); 
	  if ( fromv[k] != NULL )          /* just for cells which may have undefined elements */
	    {
	      if ( (elt = (char *) MAT_INT(typeA)->copy_elt(fromv[k])) == NULL ) return FAIL;
	      tov[index->val[i]] = elt;
	    }
	}
    }
  return OK;
}


/**
 * nsp_matint_set_elts1:
 * @ObjA: a #NspObject which implements the matint interface.
 * @Elts:  a #NspObject
 * @ObjB:  a #NspObject
 * 
 * sets the elements of @ObjA i.e @ObjA(@Elts)= @ObjB.
 * 
 * Returns: %OK or %FAIL
 **/
int nsp_matint_set_elts1(NspObject *ObjA, NspObject *Elts, NspObject *ObjB)
{
  index_vector index={0};
  int Res;
  index.iwork = matint_iwork1;  
  if ( nsp_get_index_vector_from_object(Elts,&index) == FAIL) return FAIL;
  Res = nsp_matint_set_elts(ObjA,&index,ObjB);
  nsp_free_index_vector_cache(&index);
  return Res; 
}


/**
 * nsp_matint_concat_right:
 * @ObjA:  a #NspObject which implements the matint interface.
 * @ObjB:  a #NspObject
 * 
 * computes and retuns [@ObjA,@ObjB]
 * 
 * Return value: returns [@ObjA,@ObjB] or %NULLOBJ 
 **/

NspObject *nsp_matint_concat_right( NspObject *ObjA, NspObject *ObjB)
{
  NspObject *ObjC=NULLOBJ;
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB, *C;
  int i, j;
  NspTypeBase *type; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  type = check_implements(ObjA, nsp_type_matint_id);  /* ObjA and ObjB must have the same type to send here 
                                                         (so we don't check) */

  MAT_INT(type)->canonic(ObjA);
  MAT_INT(type)->canonic(ObjB);

  elt_size_A = MAT_INT(type)->elt_size(ObjA);  /* but there is the problem real/complex */
  elt_size_B = MAT_INT(type)->elt_size(ObjB);  /* for Matrix and MaxpMatrix */

  if ( A->m == B->m )   
    {
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrices of numbers or booleans */
	{
	  if ( elt_size_A == elt_size_B )
	    {
	      if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA, A->m, A->n + B->n, FALSE)) != NULLOBJ )
		{
		  char *to;
		  C = (NspSMatrix *) ObjC;  to = (char *) C->S;
		  memcpy(to, A->S, elt_size_A*A->mn);
		  to += elt_size_A*A->mn;
		  memcpy(to, B->S, elt_size_A*B->mn);
		}
	    }
	  else 
	    {
	      if ( nsp_object_type (ObjA, nsp_type_matrix_id)|| 
		   nsp_object_type (ObjA, nsp_type_mpmatrix_id))
		{
		  /* one matrix is real and the other is complex */
		  NspMatrix *AA = (NspMatrix *) ObjA, *BB = (NspMatrix *) ObjB, *CC;
		  if ( elt_size_A > elt_size_B ) 
		    {
		      /* A is complex, B real */
		      ObjC = MAT_INT(type)->clone(NVOID, ObjA, A->m, A->n + B->n, FALSE);
		      if ( ObjC != NULLOBJ )
			{
			  CC = (NspMatrix *) ObjC;
			  memcpy(CC->C, AA->C, elt_size_A*A->mn);
			  for ( i = 0 ; i < BB->mn ; i++ )
			    {
			      CC->C[A->mn+i].r = BB->R[i]; CC->C[A->mn+i].i = 0.0;
			    }
			}
		    }
		  else 
		    {                           
		      /* A is real, B complex */
		      ObjC = MAT_INT(type)->clone(NVOID, ObjB, A->m, A->n + B->n, FALSE);
		      if ( ObjC != NULLOBJ )
			{
			  CC = (NspMatrix *) ObjC;
			  for ( i = 0 ; i < AA->mn ; i++ )
			    {
			      CC->C[i].r = AA->R[i]; CC->C[i].i = 0.0;
			    }
			  memcpy(CC->C+ A->mn, BB->C, elt_size_B*B->mn);
			}
		    }
		}
	      else
		{
		  /* int matrices of different sizes 
		   * 
		   */
		  NspIMatrix *AA = (NspIMatrix *) ObjA, *BB = (NspIMatrix *) ObjB, *CC;
		  if ( elt_size_A < elt_size_B )  
		    {
		      if ((CC = nsp_imatrix_create(NVOID,A->m,A->n + B->n,BB->itype) )== NULL)
			return NULLOBJ;
		    }
		  else
		    {
		      if ((CC = nsp_imatrix_create(NVOID,A->m,A->n +B->n,((NspIMatrix *)A)->itype) )== NULL)
			return NULLOBJ;
		    }
		  ObjC = (NspObject *) CC;
		  NSP_COPY_ITYPE_TO_ITYPE(CC,0,CC->itype,i,0,1,AA->mn,AA->Iv,AA->itype);
		  NSP_COPY_ITYPE_TO_ITYPE(CC,AA->mn,CC->itype,i,0,1,BB->mn,BB->Iv,BB->itype);
		}
	    }
	}
      else                                                
	{
	  /* Matrices of pointers (String, cells, poly,...) */
	  if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA, A->m, A->n + B->n, FALSE)) != NULLOBJ )
	    {
	      C = (NspSMatrix *) ObjC;
	      char **from = (char **) A->S, **to = (char **) C->S, *elt;
	      for ( i = 0 ; i < A->mn ; i++ )
		if ( from[i] != NULL )   /* just for cells which may have undefined elements */
		  {
		    if ( (elt = (char *) MAT_INT(type)->copy_elt(from[i])) == NULL ) goto err;
		    to[i] = elt;
		  }
	      from = (char **) B->S;
	      for ( j = 0 ; j < B->mn ; j++ )
		if ( from[j] != NULL )   /* just for cells which may have undefined elements */
		  {
		    if ( (elt = (char *) MAT_INT(type)->copy_elt(from[j])) == NULL ) goto err;
		    to[i++] = elt;
		  }
	    }
	}
    }
  else if ( A->m == 0  &&  A->n == 0 )
    ObjC = nsp_object_copy(ObjB);
  else if ( B->m == 0  &&  B->n == 0 )
    ObjC = nsp_object_copy(ObjA);
  else
    Scierror("Error: in [A,B] A and B must have the same number of rows\n");
      
  return ObjC;

 err:
  nsp_object_destroy(&ObjC); 
  return NULLOBJ;
}

/**
 * nsp_matint_concat_right_bis:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ObjB: a #Matrix (that is a #NspObject which implements the matint interface)
 * 
 * changes @ObjA to [@ObjA,@ObjB] 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_matint_concat_right_bis(NspObject *ObjA, NspObject *ObjB)
{
  int copy = FALSE;
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB;
  int i, nA = A->n, nB = B->n, mnA = A->mn;
  NspTypeBase *type;
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  /* ObjA and ObjB must have the same type when we get here */
  type = check_implements(ObjA, nsp_type_matint_id);  
  
  if ( ObjA == ObjB ) 
    {
      copy = TRUE;
      if ((ObjB = nsp_object_copy(ObjB)) == NULLOBJ ) 
	return FAIL;
      B = (NspSMatrix *) ObjB;
    }

  MAT_INT(type)->canonic(ObjA);
  MAT_INT(type)->canonic(ObjB);
  
  elt_size_A = MAT_INT(type)->elt_size(ObjA);  /* but there is the problem real/complex */
  elt_size_B = MAT_INT(type)->elt_size(ObjB);  /* for Matrix and MaxpMatrix */

  if ( A->m == B->m || (A->m == 0 && A->n == 0) )
    {
      int Am= B->m ; /* to cover the case A->m==0 */
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  
	{
	  /* Matrices of numbers or booleans */
	  if ( elt_size_A == elt_size_B )
	    {
	      if ( MAT_INT(type)->enlarge(ObjA, Am, nA + nB) == FAIL ) return FAIL;
	      {
		char *to = ((char *) A->S) + elt_size_A*mnA;
		memcpy(to, B->S, elt_size_A*B->mn);
	      }
	    }
	  else    
	    {
	      /* we have here to switch according to types
	       * this part should be rejected in function to be implemented 
	       * in each type. 
	       */
	      if ( nsp_object_type (ObjA, nsp_type_matrix_id)|| 
		   nsp_object_type (ObjA, nsp_type_mpmatrix_id)) 
		{
		  /* one matrix is real and the other is complex */
		  NspMatrix *AA = (NspMatrix *) ObjA, *BB = (NspMatrix *) ObjB;
		  if ( elt_size_A > elt_size_B )  /* A is complex, B real */
		    {
		      if ( nsp_matrix_resize(AA, Am, nA + nB) == FAIL ) return FAIL;
		      for ( i = 0 ; i < BB->mn ; i++ )
			{
			  AA->C[mnA+i].r = BB->R[i]; AA->C[mnA+i].i = 0.0;
			}
		    }
		  else
		    {                             /* A is real, B complex */
		      if ( nsp_matrix_resize(AA, Am, nA + nB) == FAIL ) return FAIL;
		      if ( nsp_mat_complexify(AA, 0.00) == FAIL ) return FAIL;
		      memcpy( &(AA->C[mnA]), BB->C, elt_size_B*B->mn);
		    }
		}
	      else
		{
		  /* int matrices of different sizes 
		   * 
		   */
		  NspIMatrix *AA = (NspIMatrix *) ObjA, *BB = (NspIMatrix *) ObjB;
		  if ( elt_size_A < elt_size_B )  
		    {
		      /* this could be removed */
		      if ( nsp_imatrix_change_itype(AA,BB->itype)== FAIL) return FAIL;
		    }
		  if ( nsp_imatrix_resize(AA, Am, nA + nB) == FAIL ) return FAIL;
		  NSP_COPY_ITYPE_TO_ITYPE(AA,mnA,AA->itype,i,0,1,BB->mn,BB->Iv,BB->itype);
		}
	    }
	}
      else
	{
	  /* generic code for Matrices of pointers (String, cells, poly,...)
	   */
	  if ( MAT_INT(type)->enlarge(ObjA, Am, nA + nB) == FAIL ) return FAIL;
	    {
	      char **from = (char **) B->S, **to = ((char **) A->S) + mnA, *elt;
	      for ( i = 0 ; i < B->mn ; i++ )
		if ( from[i] != NULL )   /* just for cells which may have undefined elements */
		  {
		    if ( to[i] != NULL) MAT_INT(type)->free_elt((void **) &to[i]);
		    if ( (elt = (char *) MAT_INT(type)->copy_elt(from[i])) == NULL ) return FAIL;
		    to[i] = elt;
		  }
	    }
	}
    }
  else if ( !(B->m == 0 && B->n == 0) )
    {
      Scierror("Error: in [A,B] or A.concatr[B], A and B must have the same number of rows\n");
      return FAIL; 
    }

  if ( copy == TRUE ) nsp_object_destroy(&ObjB);


  return OK;
}



/**
 * nsp_matint_concat_down:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ObjB: a #Matrix (that is a #NspObject which implements the matint interface)
 * 
 * returns [@ObjA;@ObjB]
 * 
 * Return value: a new #NspObject or %NULLOBJ.
 **/


NspObject *nsp_matint_concat_down(NspObject *ObjA, NspObject *ObjB)
{
  NspObject *ObjC=NULLOBJ;
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB, *C;
  int i, j;
  NspTypeBase *type; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  type = check_implements(ObjA, nsp_type_matint_id);  /* ObjA and ObjB must have the same type to send here 
                                                         (so we don't check) */

  MAT_INT(type)->canonic(ObjA);
  MAT_INT(type)->canonic(ObjB);

  elt_size_A = MAT_INT(type)->elt_size(ObjA);  /* but there is the problem real/complex */
  elt_size_B = MAT_INT(type)->elt_size(ObjB);  /* for Matrix and MaxpMatrix */

  if ( A->n == B->n ) 
    {
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  
	{
	  /* Matrices of numbers or booleans */
	  if ( elt_size_A == elt_size_B )
	    {
 	      if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n, FALSE)) != NULLOBJ )
		{
		  int step=elt_size_A;
		  char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
		  C = (NspSMatrix *) ObjC;  to = (char *) C->S;
		  for ( j = 0 ; j < A->n ; j++ ) 
		    { 
		      memcpy(to+j*(C->m)*step,fromA+j*A->m*step,A->m*step);
		      memcpy(to+j*(C->m)*step + A->m*step,fromB+j*B->m*step,B->m*step);
		    }
		}
	    }
	  else
	    {
	      if ( nsp_object_type (ObjA, nsp_type_matrix_id)|| 
		   nsp_object_type (ObjA, nsp_type_mpmatrix_id))
		{
		  /* one matrix is real and the other is complex */
		  if ( elt_size_A > elt_size_B )  
		    {
		      ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n, FALSE);
		      if ( ObjC != NULLOBJ )
			{
			  /* A is complex, B real */
			  int stepA=elt_size_A;
			  int stepB=elt_size_B;
			  char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
			  C = (NspSMatrix *) ObjC;  to = (char *) C->S;
			  for ( j = 0 ; j < A->n ; j++ ) 
			    { 
			      doubleC *elt =(doubleC *) (to +j*(C->m)*stepA + A->m*stepA);
			      memcpy(to+j*(C->m)*stepA,fromA+j*A->m*stepA,A->m*stepA);
			      for ( i = 0 ; i < B->m ; i++)
				{
				  elt[i].r = *(((double *) (fromB+j*B->m*stepB)) +i);
				  elt[i].i = 0.0;
				}
			    }
			}
		    }
		  else 
		    { 
		      /* A is real, B complex */
		      ObjC = MAT_INT(type)->clone(NVOID, ObjB,A->m+B->m,A->n, FALSE);
		      if ( ObjC != NULLOBJ )
			{
			  int stepA=elt_size_A;
			  int stepB=elt_size_B;
			  char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
			  C = (NspSMatrix *) ObjC;  to = (char *) C->S;
			  for ( j = 0 ; j < A->n ; j++ ) 
			    { 
			      doubleC *elt =(doubleC *) (to +j*(C->m)*stepB);
			      /* copy column j of A which is real */
			      for ( i = 0 ; i < A->m ; i++)
				{
				  elt[i].r = *(((double *) (fromA+j*A->m*stepA)) +i);
				  elt[i].i = 0.0;
				}
			      /* copy column j of B which is complex as C */
			      memcpy(to+j*(C->m)*stepB + A->m*stepB,fromB+j*B->m*stepB,B->m*stepB);
			    }
			}
		    }
		}
	      else
		{
		  NspIMatrix *C; 
		  /* mixed integer matrices  */
		  if ( elt_size_A < elt_size_B )  
		    {
		      if ((C = nsp_imatrix_create(NVOID,A->m,A->n,((NspIMatrix *)B)->itype) )== NULL)
			return NULLOBJ;
		      NSP_COPY_ITYPE_TO_ITYPE(C,0,C->itype,i,0,1,C->mn,((NspIMatrix *)A)->Iv,
					      ((NspIMatrix *)A)->itype);
		      ObjC =nsp_matint_concat_down((NspObject *) C, ObjB);
		      nsp_imatrix_destroy(C);
		    }
		  else
		    {
		      if ((C = nsp_imatrix_create(NVOID,B->m,B->n,((NspIMatrix *)A)->itype) )== NULL)
			return NULLOBJ;
		      NSP_COPY_ITYPE_TO_ITYPE(C,0,C->itype,i,0,1,C->mn,((NspIMatrix *)B)->Iv,
					      ((NspIMatrix *)B)->itype);
		      ObjC= nsp_matint_concat_down(ObjA, (NspObject *) C);
		      nsp_imatrix_destroy(C);
		    }
		}
	    }
	}
      else                                                      
	{
	  /* Matrices of pointers (String, cells, poly,...) */
	  if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n, FALSE)) != NULLOBJ )
	    {
	      C = (NspSMatrix *) ObjC;
	      char *elt;
	      for ( j = 0 ; j < A->n ; j++ ) 
		{
		  char **fromA= A->S+j*A->m;
		  char **fromB= B->S+j*B->m;
		  char **toA = C->S+j*(C->m),**toB = toA+A->m;
		  for ( i = 0 ; i < A->m ; i++ )
		    {
		      if ( (elt = (char *) MAT_INT(type)->copy_elt(fromA[i])) == NULL ) goto err;
		      toA[i]=elt;
		    }
		  for ( i = 0 ; i < B->m ; i++ )
		    {
		      if ( (elt = (char *) MAT_INT(type)->copy_elt(fromB[i])) == NULL ) goto err;
		      toB[i]=elt;
		    }
		}
	    }
	}
    }
  else if ( A->m == 0  &&  A->n == 0 )
    ObjC = nsp_object_copy(ObjB);
  else if ( B->m == 0  &&  B->n == 0 )
    ObjC = nsp_object_copy(ObjA);
  else
    Scierror("Error: in [A;B], A and B must have the same number of columns\n");

  return ObjC;

 err:
  nsp_object_destroy(&ObjC); 
  return NULLOBJ;
}


/**
 * nsp_matint_concat_down_bis:
 * @ObjA: a #NspObject 
 * @ObjB: a #NspObject 
 * 
 * performe @ObjA =  [@ObjA;@ObjB]
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_matint_concat_down_bis(NspObject *ObjA, NspObject *ObjB)
{
  int copy=FALSE;
  NspObject *ObjC=NULLOBJ;
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB, *C=NULL;
  int i, j;
  NspTypeBase *type; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  /* ObjA and ObjB must have the same type to send here 
   * (so we don't check) 
   */
  type = check_implements(ObjA, nsp_type_matint_id);  

  if ( ObjA == ObjB ) 
    {
      copy = TRUE;
      if ((ObjB = nsp_object_copy(ObjB)) == NULLOBJ ) 
	return FAIL;
      B = (NspSMatrix *) ObjB;
    }
  
  MAT_INT(type)->canonic(ObjA);
  MAT_INT(type)->canonic(ObjB);
  elt_size_A = MAT_INT(type)->elt_size(ObjA);  /* but there is the problem real/complex */
  elt_size_B = MAT_INT(type)->elt_size(ObjB);  /* for Matrix and MaxpMatrix */
  
  if ( A->n == B->n  || (A->n == 0 && A->m == 0) ) 
    {
      A->n = B->n;  /* to cover the case when A = [] */
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )     
	{
	  /* Matrices of numbers or booleans */
	  if ( elt_size_A == elt_size_B )
	    {
	      /* same type .. in size  */
	      int Am=A->m;
	      if ( MAT_INT(type)->resize(ObjA,A->m+B->m,A->n) == OK )
		{
		  int step=elt_size_A;
		  char *to=(char *) A->S, *fromA = (char *) A->S, *fromB = (char *) B->S;
		  for ( j = A->n-1  ; j >= 1 ; j-- ) 
		    {
		      memmove(to +j*(A->m)*step,fromA+j*Am*step,Am*step);
		    }
		  for ( j = A->n-1  ; j >= 0 ; j-- ) 
		    {
		      memcpy(to+j*(A->m)*step + Am*step,fromB+j*B->m*step,B->m*step);
		    }
		}
	    }
	  else    
	    {
	      if ( nsp_object_type (ObjA, nsp_type_matrix_id)|| 
		   nsp_object_type (ObjA, nsp_type_mpmatrix_id)) 
		{
		  /* one matrix is real and the other is complex */
		  if ( elt_size_A > elt_size_B )  
		    {
		      /* A is complex, B is real we enlarge it and store the result */
		      int Am=A->m;
		      if ( MAT_INT(type)->resize(ObjA,A->m+B->m,A->n) == OK )
			{
			  /* A is complex, B real */
			  int stepA=elt_size_A;
			  int stepB=elt_size_B;
			  char *to=(char *) A->S, *fromA = (char *) A->S, *fromB = (char *) B->S;
			  for ( j = A->n-1  ; j >= 1 ; j-- ) 
			    {
			      memmove(to +j*(A->m)*stepA,fromA+j*Am*stepA,Am*stepA);
			    }
			  for ( j = A->n-1  ; j >= 0 ; j-- ) 
			    {
			      doubleC *elt =(doubleC *) (to +j*(A->m)*stepA + Am*stepA);
			      for ( i = 0 ; i < B->m ; i++)
				{
				  elt[i].r = *(((double *) (fromB+j*B->m*stepB)) +i);
				  elt[i].i = 0.0;
				}
			    }
			}
		    }
		  else 
		    { 
		      /* A is real, B complex, A must be enlarged and complexified */
		      ObjC = MAT_INT(type)->clone(NVOID, ObjB,A->m+B->m,A->n, FALSE);
		      if ( ObjC != NULLOBJ )
			{
			  int stepA=elt_size_A;
			  int stepB=elt_size_B;
			  char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
			  C = (NspSMatrix *) ObjC;  to = (char *) C->S;
			  for ( j = 0 ; j < A->n ; j++ ) 
			    { 
			      doubleC *elt =(doubleC *) (to +j*(C->m)*stepB);
			      /* copy column j of A which is real */
			      for ( i = 0 ; i < A->m ; i++)
				{
				  elt[i].r = *(((double *) (fromA+j*A->m*stepA)) +i);
				  elt[i].i = 0.0;
				}
			      /* copy column j of B which is complex as C */
			      memcpy(to+j*(C->m)*stepB + A->m*stepB,fromB+j*B->m*stepB,B->m*stepB);
			    }
			}
		      else 
			{
			  return FAIL;
			}
		      /* store the data in A and clean ObjC */
		      A->m = C->m;
		      A->n = C->n;
		      ((NspMatrix *) A)->rc_type = 'c';
		      A->mn = C->mn;
		      FREE(A->S);
		      A->S = C->S;
		      C->S = NULL;
		      C->m = C->n = C->mn = 0;
		      nsp_object_destroy(&ObjC);
		    }
		}
	      else
		{
		  /* mixed integer matrices  */
		  int rep;
		  if ( elt_size_A < elt_size_B )  
		    {
		      /* this could be removed */
		      if ( nsp_imatrix_change_itype(((NspIMatrix *)A),
						    ((NspIMatrix *)B)->itype)== FAIL) return FAIL;
		      return nsp_matint_concat_down_bis(ObjA, ObjB);
		    }
		  else
		    {
		      NspIMatrix *C; 
		      if ((C = nsp_imatrix_create(NVOID,B->m,B->n,((NspIMatrix *)A)->itype) )== NULL)
			goto err;
		      NSP_COPY_ITYPE_TO_ITYPE(C,0,C->itype,i,0,1,C->mn,((NspIMatrix *)B)->Iv,
					      ((NspIMatrix *)B)->itype);
		      rep=nsp_matint_concat_down_bis(ObjA, (NspObject *) C);
		      nsp_imatrix_destroy(C);
		      return rep;
		    }
		}
	    }
	}
      else                                                      
	{
	  int Am=A->m;
	  if ( MAT_INT(type)->resize(ObjA,A->m+B->m,A->n) == OK )
	    {
	      char *elt;
	      int step=elt_size_A;
	      char *to=(char *) A->S, *fromA = (char *) A->S;
	      /* take care that resize can allocate values thus we have to free. 
	       * give an option to resize ? 
	       */
	      if (A->S+Am*A->n  != NULL)
		for ( j= Am*A->n ; j < A->mn ; j++)
		  MAT_INT(type)->free_elt((void **) A->S+j);
	      for ( j = A->n-1  ; j >= 1 ; j-- ) 
		{
		  memmove(to +j*(A->m)*step,fromA+j*Am*step,Am*step);
		}
	      for ( j = 0 ; j < A->n ; j++ ) 
		{
		  char **fromB= B->S+j*B->m;
		  char **toB =A->S+j*(A->m)+ Am;
		  for ( i = 0 ; i < B->m ; i++ )
		    {
		      if ( (elt = (char *) MAT_INT(type)->copy_elt(fromB[i])) == NULL ) goto err;
		      toB[i]=elt;
		    }
		}
	    }
	}
    }
  else if ( !(B->m == 0 && B->n == 0) )
    {
      Scierror("Error: in [A;B] or A.concatd[B], A and B must have the same number of columns\n");
      return FAIL;
    }

  if ( copy == TRUE ) nsp_object_destroy(&ObjB);
  return OK;
 err:
  if ( copy == TRUE ) nsp_object_destroy(&ObjB);
  return FAIL;
}


/**
 * nsp_matint_redim:
 * @Obj: a #NspMatrix
 * @m: number of rows 
 * @n: number of columns
 * 
 * Checks that the #NspObject @Obj (which is supposed to implement matint)
 * of size m' x n' satisfy m'*n' = @m * @n and reshapes 
 * @Obj to size @m x @n.
 * 
 * Return value: returns %OK or %FAIL. In case of %FAIL an error is raised.
 **/

int nsp_matint_redim(NspObject *Obj, int m, int n)
{
  int mm=m,nn=n;
  NspSMatrix *A= (NspSMatrix *) Obj;
  if ( m == -1 ) m = (n > 0) ? A->mn/n : 0;
  if ( n == -1 ) n = (m > 0) ? A->mn/m : 0;
  if ( A->mn == m*n ) 
    {
      A->m =m;
      A->n =n;
      return OK;
    }
  else 
    {
      Scierror("Error:\tCannot change size to (%dx%d) since matrix has %d elements\n",mm,nn,A->mn);
      return FAIL;
    }
}

/**
 * nsp_matint_repmat:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @m: integer 
 * @n: integer
 * 
 * Return value: returns ObjA replicated m x n  or %NULLOBJ 
 **/

NspObject *nsp_matint_repmat(const NspObject *ObjA, int m, int n)
{
  NspObject *ObjB=NULLOBJ;
  NspSMatrix *A = (NspSMatrix *) ObjA, *B;
  int i, j;
  NspTypeBase *type; 
  unsigned int elt_size_A; /* size in number of bytes */

  type = check_implements(ObjA, nsp_type_matint_id);

  MAT_INT(type)->canonic(NSP_OBJECT(ObjA));
  elt_size_A = MAT_INT(type)->elt_size(ObjA);

  if ( (ObjB = MAT_INT(type)->clone(NVOID, ObjA, m*A->m, n*A->n, FALSE)) == NULLOBJ )
    return ObjB;

  B = (NspSMatrix *) ObjB;
  if ( B->mn == 0 )
    return ObjB;

  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  
    {
      /* Matrices of numbers or booleans */
      char *to = (char *) B->S;

      if ( m == 1 )
	for ( j = 0 ; j < n ; j++ )
	  {
	    memcpy(to, A->S, elt_size_A*A->mn);
	    to += elt_size_A*A->mn;
	  }
      else
	{
	  int blk_size = elt_size_A*A->m;
	  char *from = (char *) A->S;
	  for ( j = 0 ; j < A->n ; j++ )
	    for ( i = 0 ; i < m ; i++ )
	      {
		memcpy(to, from + j*blk_size, blk_size);
		to += blk_size;
	      }
	  blk_size *= m*A->n;
	  for ( j = 1 ; j < n ; j++ )
	    {
	      memcpy(to, B->S, blk_size);
	      to += blk_size;
	    }
	}
    }
  else                                                      
    {
      /* Matrices of pointers (String, cells, poly,...) */
      char **from = (char **) A->S, **to = (char **) B->S, *elt;
      int ii, jj, k = 0, kk = 0;
 
      for ( j = 0 ; j < A->n ; j++ ) 
	for ( i = 0 ; i < A->m ; i++ )
	  {
	    if ( from[k] != NULL )   /* just for cells which may have undefined elements */
	      for ( jj = 0 ; jj < n ; jj++ )	      
		{
		  kk = i + (j + jj*A->n)*B->m;
		  for ( ii = 0 ; ii < m ; ii++, kk+= A->m )
		    {
		      if ( (elt = (char *) MAT_INT(type)->copy_elt(from[k])) == NULL ) goto err;
		      to[kk] = elt;
		    }
		}
	    k++;
	  }
    }
  return ObjB;

 err:
  nsp_object_destroy(&ObjB); 
  return NULLOBJ;
}


#define APPLY_PERM()						     \
  if ( dim_flag == 0 )						     \
    {								     \
      temp = Amat[p-1]; Amat[p-1] = Amat[q-1]; Amat[q-1] = temp;     \
    }								     \
  else if ( dim_flag == 1 )					     \
    {								     \
      int j, pj = p-1, qj = q-1;				     \
      for ( j = 0 ; j < A->n ; j++ )				     \
	{							     \
	  temp = Amat[pj]; Amat[pj] = Amat[qj]; Amat[qj] = temp;     \
	  pj += A->m; qj += A->m;				     \
	}							     \
    }								     \
  else if ( dim_flag == 2 )					     \
    {								     \
      int i, pi = (p-1)*A->m, qi = (q-1)*A->m;			     \
      for ( i = 0 ; i < A->m ; i++ )				     \
	{							     \
	  temp = Amat[pi]; Amat[pi] = Amat[qi]; Amat[qi] = temp;     \
	  pi++; qi++;						     \
	}							     \
    }


/**
 * nsp_matint_perm_elem:
 * @ObjA: a #NspObject which implements the matint interface.
 * @p: an integer 
 * @q: an integer 
 * @dim_flag: an integer 
 * 
 * permutes elemnts, rows or columns of an object which implements the 
 * matint interface. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_matint_perm_elem(NspObject *ObjA, int p, int q, int dim_flag)
{
  NspSMatrix *A = (NspSMatrix *) ObjA;
  NspTypeBase *type; 
  unsigned int elt_size_A; /* size in number of bytes */
  
  type = check_implements(ObjA, nsp_type_matint_id);

  MAT_INT(type)->canonic(ObjA);
  elt_size_A = MAT_INT(type)->elt_size(ObjA);

  if ( p < 1 || q < 1 )
    goto err;

  if ( dim_flag == 0 )
    {
      if ( p > A->mn || q > A->mn ) goto err;
    }
  else if ( dim_flag == 1 )
    {
      if ( p > A->m || q > A->m ) goto err;
    }
  else if ( dim_flag == 2 )
    {
      if ( p > A->n || q > A->n ) goto err;
    }
  else
    {
      Scierror("Error: Invalid dim flag '%d' (must be 0, 1 or 2)\n", dim_flag);
      return FAIL;
    }

  if ( p == q )
    return OK;

  if ( nsp_object_type (ObjA, nsp_type_matrix_id)|| 
       nsp_object_type (ObjA, nsp_type_mpmatrix_id)) 
    {
      if ( elt_size_A == sizeof(double) )
	{
	  double *Amat = (double *) A->S, temp;
	  APPLY_PERM();
	}
      else if ( elt_size_A == sizeof(doubleC) )
	{
	  doubleC *Amat = (doubleC *) A->S, temp;
	  APPLY_PERM();
	}
    }
  else if ( elt_size_A == sizeof(int) )
    {
      int *Amat = (int *) A->S, temp;
      APPLY_PERM();
    }
  else
    {
      char *temp, *pp, *qq;
      unsigned int temp_size = dim_flag == 2 ? A->m*elt_size_A : elt_size_A;
      int j;
      if ( (temp = malloc(temp_size*sizeof(char))) == NULL )
	return FAIL;
      if ( dim_flag == 1 )
	{ 
	  pp =  ((char *) A->S) + (p-1)*elt_size_A;
	  qq =  ((char *) A->S) + (q-1)*elt_size_A;
	  for ( j = 0 ; j < A->n ; j++ )
	    { 
	      memcpy(temp, pp, elt_size_A);
	      memcpy(pp, qq, elt_size_A);
	      memcpy(qq, temp, elt_size_A);	  
	      pp += elt_size_A*A->m; qq += elt_size_A*A->m; 
	    } 
	}
      else /* dim_flag == 0 or 2  */
	{ 
	  pp =  ((char *) A->S) + (p-1)*temp_size;
	  qq =  ((char *) A->S) + (q-1)*temp_size;
	  memcpy(temp, pp, temp_size);
	  memcpy(pp, qq, temp_size);
	  memcpy(qq, temp, temp_size);	  
	}
      free(temp);
    }
  return OK;

 err:
  Scierror("Error: invalid permutation indices (outside matrix or vector range)\n");
  return FAIL;
}


/* interface functions which are called by other interfaces 
 * or called in Eval.c 
 */

/**
 * int_matint_tozero:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 *
 * generic interface which can be used by objects which implement 
 * matint in order to perform  <literal>Obj(:) = [], Obj(:,:) = []</literal> 
 * 
 * Returns: 1 
 **/

int int_matint_tozero(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  Obj = NthObj(1);
  nsp_matint_tozero(Obj);
  Obj->ret_pos = 1;
  return 1;
}

typedef int (*delfunc) (NspObject *Obj,index_vector *index);

static int int_matint_delete_gen(Stack stack, int rhs, int opt, int lhs, delfunc F)
{
  index_vector index={0};
  NspObject *Obj;
  Obj = NthObj(1);
  index.iwork = matint_iwork1;  
  if ( nsp_get_index_vector(stack, 2,NULL,&index) == FAIL )
    return RET_BUG;
  if ( (*F)(Obj, &index) == FAIL )
    goto err;
  nsp_free_index_vector_cache(&index);
  Obj->ret_pos = 1;
  return 1;
 err:
  nsp_free_index_vector_cache(&index);
  return RET_BUG;
}

/**
 * int_matint_deleteelts:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform elements deletions
 * 
 * 
 * Returns: 1 or %RET_BUG
 **/

int int_matint_deleteelts(Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs == 3 )
    return int_matint_deleteelts2(stack, rhs, opt, lhs);
  else
    return int_matint_delete_gen(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_elements);
}

/**
 * int_matint_deletecols:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform  column deletion
 * 
 * 
 * Returns:  1 or %RET_BUG
 **/

int int_matint_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_delete_gen(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_columns);
}

/**
 * int_matint_deleterows:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform  row deletion
 * 
 * 
 * Returns:  1 or %RET_BUG
 **/
int int_matint_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_delete_gen(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_rows);
}


/**
 * int_matint_deleteelts2:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform  elements deletion <literal>A(I,J)=[]</literal> 
 * 
 * 
 * Returns:  1 or %RET_BUG
 **/
int int_matint_deleteelts2(Stack stack, int rhs, int opt, int lhs)
{
  index_vector index_r={0},index_c={0};
  NspObject *Obj;
  
  Obj = NthObj(1);

  index_r.iwork = matint_iwork1;  
  if ( nsp_get_index_vector(stack, 2,NULL,&index_r) == FAIL )
    return RET_BUG;
  
  index_c.iwork = matint_iwork2;  
  if ( nsp_get_index_vector(stack, 3,NULL,&index_c) == FAIL )
    goto err;
  
  if ( nsp_matint_delete_elements2(Obj,
				   index_r.val, index_r.nval , index_r.min, index_r.max,
				   index_c.val, index_c.nval , index_c.min, index_c.max) 
       == FAIL) 
    goto err;

  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  Obj->ret_pos = 1;
  return 1;
  
 err:
  return RET_BUG;
}


/**
 * int_matint_resize2vect:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform  <literal>A(:)</literal> operation.
 * 
 * 
 * Returns:  1 or %RET_BUG
 **/
/* various extractions interfaces */

int int_matint_resize2vect(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj, *Res;
  NspSMatrix *B;

  CheckLhs (1, 1);
  Obj = NthObj(1);
    
  if ( (Res =nsp_object_copy(Obj)) == NULLOBJ )
    return RET_BUG;

  B = (NspSMatrix *) Res;
  B->m = B->mn;
  B->n = 1;   /* a voir pour le mtlb_mode */

  MoveObj (stack, 1, Res);
  return 1;
}


typedef NspObject * (*extractfunc) (NspObject *Obj,NspObject *Elts,index_vector *index);

static int int_matint_extract_gen(Stack stack, int rhs, int opt, int lhs, extractfunc F)
{
  index_vector index={0};
  NspObject *Obj, *Res,*Elts;

  CheckLhs (1, 1);
  Obj = NthObj(1);

  index.iwork = matint_iwork1;  
  if ( nsp_get_index_vector(stack, 2,&Elts,&index) == FAIL )
    {
      int rep,n ;
      char name[NAME_MAXL];
      if ( rhs != 2 || IsListObj(stack,2) == FALSE ) return RET_BUG;
      /* check if we are using a list access */
      /* we must clear the error raised by nsp_get_index_vector */
      nsp_error_message_clear();
      if ( (rep = ListFollowExtract(stack,rhs,opt,lhs)) < 0 ) return rep; 
      if ( rep == 3 ) 
	{
	  /* last extraction : here O can be anything */ 
	  nsp_build_funcname("extractelts",&stack,stack.first+1,1,name);
	  if ((n=nsp_eval_func(NULLOBJ,name,2,stack,stack.first+1,2,0,1)) < 0)
	    {
	      return RET_BUG;
	    }
	}
      nsp_void_object_destroy(&NthObj(1));
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ( (Res = (*F)(Obj,Elts,&index)) == NULLOBJ )
    goto err;

  nsp_free_index_vector_cache(&index);
  MoveObj (stack, 1, Res);
  return 1;

 err:
  nsp_free_index_vector_cache(&index);
  return RET_BUG;
}

/**
 * int_matint_extractelts:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform elements extractions.
 * 
 * Returns:  1 or %RET_BUG
 **/

int int_matint_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_elements);
}

/**
 * int_matint_extractcols:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform column extractions
 * 
 * 
 * Returns:  1 or %RET_BUG
 **/
int int_matint_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_columns);
}

/**
 * int_matint_extractrows:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform row extractions
 * 
 * Returns:  1 or %RET_BUG
 **/

int int_matint_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_rows);
}

int int_matint_extractrows_mat(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_rows_mat);
}
int int_matint_extractrows_int(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_rows_int);
}
int int_matint_extractrows_gen(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_rows_gen);
}
int int_matint_extractrows_pointer(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_rows_pointer);
}



/**
 * int_matint_extract:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform  extractions.
 * 
 * 
 * Returns:  1 or %RET_BUG
 **/

int int_matint_extract(Stack stack, int rhs, int opt, int lhs)
{
  index_vector index_r={0},index_c={0};
  NspObject *Obj, *Res;
  
  CheckLhs (1, 1);
  Obj = NthObj(1);

  index_r.iwork = matint_iwork1;  
  if ( nsp_get_index_vector(stack, 2,NULL, &index_r)== FAIL )
    return RET_BUG;
  
  index_c.iwork = matint_iwork2;  
  if ( nsp_get_index_vector(stack, 3,NULL, &index_c)== FAIL )
    goto err;
  
  if ( (Res =nsp_matint_extract(Obj, &index_r, &index_c)) == NULLOBJ )
    goto err;
  
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  MoveObj (stack, 1, Res);
  return 1;
  
 err:
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  return RET_BUG;
}


/**
 * int_matint_setrowscols:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform <literal>A(I,J)=B</literal> 
 * 
 * 
 * Returns:  1 or %RET_BUG
 **/
/* insertion interface */

int int_matint_setrowscols(Stack stack, int rhs, int opt, int lhs)
{
  index_vector index_r={0},index_c={0};
  NspObject *ObjA, *ObjB;

  CheckRhs (3, 4);
  CheckLhs (1, 1);

  ObjA = NthObj(1); HOBJ_GET_OBJECT(ObjA, RET_BUG);
  ObjB = NthObj(rhs); HOBJ_GET_OBJECT(ObjB, RET_BUG);

  if ( ObjB == ObjA )
    {
      if ( (ObjB = nsp_object_copy(ObjA)) == NULLOBJ )
	return RET_BUG;
      NthObj(rhs) = ObjB; /* we want be to be cleared at return */
    }

  if ( rhs == 3 )
    {
      index_r.iwork = matint_iwork1;  
      if ( nsp_get_index_vector(stack, 2,NULL,&index_r)== FAIL )
	return RET_BUG;
      if ( nsp_matint_set_elts(ObjA,&index_r, ObjB) 
	   == FAIL )
	goto err;
    }
  else /* rhs = 4 */
    {
      index_r.iwork = matint_iwork1;  
      if ( nsp_get_index_vector(stack, 2,NULL,&index_r)== FAIL )
	return RET_BUG;
      index_c.iwork = matint_iwork2;  
      if ( nsp_get_index_vector(stack, 3,NULL,&index_c)== FAIL )
	goto err;
      if ( nsp_matint_set_submatrix(ObjA,&index_r,&index_c, ObjB) == FAIL ) 
	goto err;
    }

  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  ObjA->ret_pos = 1;
  return 1;
 err:
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  return RET_BUG;
}

/**
 * int_matint_concatr:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * This interface is used for concatr_x_x operations with 
 * x implementing the matint interface. This interface is 
 * called through the accelerated tab mechanism and thus 
 * objects can be selected via NthObj. 
 * 
 * Return value: 1 or %RET_BUG.
 **/

int int_matint_concatr(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *ObjA, *ObjB, *Res;

  CheckRhs (2, 2);
  CheckLhs (1, 1);
  ObjA = NthObj(1);  ObjB = NthObj(2);

  if ( Ocheckname(ObjA, NVOID) )   
    {
      /* here ObjA has no name we can return the result in ObjA 
       * there's a special case when A=0x0 in that case we can directly 
       * return B. 
       */
      if (((NspSMatrix *) ObjA)->m == 0 && ((NspSMatrix *) ObjA)->n == 0 )
	{
	  /* we can return B */
	  /* this is a bit tricky since A and B may point to the same object */
	  if ( ObjA == ObjB ) 
	    {
	      NthObj(2) = NULLOBJ;
	      NSP_OBJECT(ObjA)->ret_pos = 1;
	    }
	  else 
	    {
	      NSP_OBJECT(ObjB)->ret_pos = 1;
	    }
	  return 1;
	}
      if ( nsp_matint_concat_right_bis(ObjA, ObjB) == FAIL )
	return RET_BUG;
      ObjA->ret_pos = 1;
    }
  else
    {
      if ( (Res =nsp_matint_concat_right(ObjA, ObjB)) == NULLOBJ )
	return RET_BUG;
      MoveObj (stack, 1, Res);
    }
  return 1;
}

/**
 * int_matint_concat_emptymat_and_mat:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generix interface for [A,B] and [A;B] with A the real empty 0x0 matrix
 *
 * B is supposed to be of different type than A...
 * 
 * Return value: 1 or %RET_BUG.
 **/

int int_matint_concat_emptymat_and_mat(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *ObjA, *ObjB;
  int m,n;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  ObjA = NthObj(1);  ObjB = NthObj(2);

  m = nsp_object_get_size(ObjA,1);
  n = nsp_object_get_size(ObjA,2);

  if ( m  != 0 || n != 0 )
    {      
      Scierror("Error:\t forbidden concatenation\n");
      return RET_BUG;
    }
  
  ObjB->ret_pos = 1;
  return 1;
}

/**
 * int_matint_concatd:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generix interface for [A;B]
 * This interface is used for concatr_x_x operations with 
 * x implementing the matint interface. This interface is 
 * called through the accelerated tab machanism and thus 
 * objects can be selected via NthObj. 
 * 
 * Return value: 1 or %RET_BUG.
 **/


int int_matint_concatd(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *ObjA, *ObjB, *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  ObjA = NthObj(1);  ObjB = NthObj(2);

  if ( Ocheckname(ObjA, NVOID) )   /* ObjA has no name */ 
    {
      if (((NspSMatrix *) ObjA)->m == 0 && ((NspSMatrix *) ObjA)->n == 0)
	{
	  /* when A is 0x0 we can return B 
	   * this is a bit tricky since A and B may point to the same object 
	   */
	  if ( ObjA == ObjB ) 
	    {
	      NthObj(2) = NULLOBJ;
	      NSP_OBJECT(ObjA)->ret_pos = 1;
	    }
	  else 
	    {
	      NSP_OBJECT(ObjB)->ret_pos = 1;
	    }
	  return 1;
	}
      if ( nsp_matint_concat_down_bis(ObjA, ObjB) == FAIL )
	return RET_BUG;
      ObjA->ret_pos = 1;
    }
  else
    {
      if ( (Res =nsp_matint_concat_down(ObjA, ObjB)) == NULLOBJ )
	return RET_BUG;
      MoveObj (stack, 1, Res);
    }
  return 1;
}

/**
 * int_matint_concat_down:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * @F: function to be used for performing concatenation.
 * 
 * an other interface for concatd, this one 
 * can be used when we do not go through the 
 * accelerated table mechanism (i.e objects can still be 
 * pointers).
 *
 * Return value: 1 or %RET_BUG.
 **/

int int_matint_concat_down(Stack stack, int rhs, int opt, int lhs, Fconcat_d F)
{
  /* objects with matint interface can be casted to NspSMatrix */
  NspSMatrix *A,*B;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = (NspSMatrix *)nsp_get_object(stack, 1)) == NULL)   return RET_BUG;
  if ((B = (NspSMatrix *)nsp_get_object(stack, 2)) == NULL)   return RET_BUG;
  if (A->mn == 0)
    {
      /* this is a bit tricky since A and B may point to the same object */
      if ( A == B ) 
	{
	  NthObj(2) = NULLOBJ;
	  NSP_OBJECT(A)->ret_pos = 1;
	}
      else 
	{
	  NSP_OBJECT(B)->ret_pos = 1;
	}
      return 1;
    }

  if (B->mn == 0)
    {
      /* this is a bit tricky since A and B may point to the same object */
      if ( A == B )  NthObj(2) = NULLOBJ;
      NSP_OBJECT(A)->ret_pos = 1;
    }
  else
    {
      NspObject *HMat3;
      if ((HMat3 = (*F) ((const NspObject *) A,(const NspObject *) B)) == NULLOBJ)
	return RET_BUG;
      MoveObj (stack, 1, HMat3);
    }
  return 1;
}

/**
 * int_matint_cells_setrowscols:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * interface for set cells elements C{exps}=(....) or C{exp,exp,..}=(...)
 * 
 * Return value:  1 or %RET_BUG
 **/

int int_matint_cells_setrowscols(Stack stack, int rhs, int opt, int lhs)
{
  index_vector index={0},index_r={0},index_c={0};
  int i, j, ind, nind;
  NspCells *C;
  CheckRhs (3, 1000);

  if ( rhs >= 1 ) 
    {
      /* last elt gives us the number of indices cells(ind1,...,indn)=rhs */
      if ( GetScalarInt(stack,rhs,&nind) == FAIL) return RET_BUG;
    }

  if ((C = GetCells(stack, 1)) == NULLCELLS) return RET_BUG;
  switch (nind ) 
    {
    case 1: 
      index.iwork = matint_iwork1;  
      if ( nsp_get_index_vector(stack, 2,NULL,&index)== FAIL )
	goto err1;
      if ( index.nval != rhs - 3 )
	{
	  Scierror("Error: internal error, less rhs arguments %d than expected %d\n",
		   rhs- 3,index.nval);
	  goto err1;
	}
      if ( index.min <= 0 )
	{
	  Scierror("Error:\tNon Positive indices are not allowed\n");
	  goto err1;
	}
      if (  index.max > C->mn ) 
	{
	  /* we must enlarge C ZZZ */
	  if ( C->m == 1 || C->m == 0 )
	    {
	      if ( nsp_cells_enlarge(C, 1, index.max) == FAIL ) goto err1;
	    }
	  else if ( C->n == 1 || C->n == 0) 
	    {
	      if ( nsp_cells_enlarge(C, index.max, 1) == FAIL ) goto err1;
	    } 
	  else
	    {
	      Scierror("Error: indice %d is out of bounds for affectation in %dx%d\n",
		       index.max,C->m,C->n);
	    }
	}

      for ( i = 0 ; i < index.nval ; i++)
	{
	  int cij = index.val[i];
	  NspObject *Ob = nsp_get_object(stack,i+3);
	  if ( Ocheckname(Ob,NVOID) ) 
	    {
	      if (nsp_object_set_name(Ob,"ce") == FAIL) goto err1;
	    }
	  else 
	    {
	      if ((Ob =nsp_object_copy_and_name("ce",Ob))== NULLOBJ) goto err1;
	    }
	  if ( C->objs[cij] != NULLOBJ) nsp_object_destroy(&C->objs[cij]);
	  C->objs[cij]= Ob;
	}
      NthObj(1)->ret_pos = 1;
      nsp_free_index_vector_cache(&index);
      break;
      
    case 2: 
      index_r.iwork = matint_iwork1;  
      if ( nsp_get_index_vector(stack, 2,NULL,&index_r)== FAIL )
	goto err2;
      index_c.iwork = matint_iwork2;  
      if ( nsp_get_index_vector(stack, 3,NULL,&index_c)== FAIL )
	goto err2;
      if ( index_r.nval*index_c.nval != rhs - 4 )
	{
	  Scierror("Error: internal error, less rhs arguments %d than expected %d\n",
		   rhs- 4, index_r.nval*index_c.nval);
	  goto err2;
	}

      if ( index_r.max > C->m || index_c.max > C->n ) 
	if ( nsp_cells_enlarge(C, index_r.max, index_c.max) == FAIL ) goto err2;

      ind = 4;
      for ( j = 0 ; j < index_c.nval ; j++)
	for ( i = 0 ; i < index_r.nval ; i++)
	  {
	    int cij= index_r.val[i] + C->m*index_c.val[j];
	    NspObject *Ob = nsp_get_object(stack,ind);
	    if ( Ocheckname(Ob,NVOID) ) 
	      {
		if (nsp_object_set_name(Ob,"ce") == FAIL) goto err2;
	      }
	    else 
	      {
		if ((Ob =nsp_object_copy_and_name("ce",Ob))== NULLOBJ) goto err2;
	      }
	    if ( C->objs[cij] != NULLOBJ) nsp_object_destroy(&C->objs[cij]);
	    C->objs[cij] = Ob;
	    ind++;
	  }
      NthObj(1)->ret_pos = 1;
      nsp_free_index_vector_cache(&index_r);
      nsp_free_index_vector_cache(&index_c);
      break;

    default: 
      Scierror("Error: cells with more than 2 indices are not yet implemented\n");
      return RET_BUG;

    }
  return 1;
  
 err1:
  nsp_free_index_vector_cache(&index);
  return RET_BUG;

 err2:
  nsp_free_index_vector_cache(&index_r);
  nsp_free_index_vector_cache(&index_c);
  return RET_BUG;
}

/**
 * int_matint_isvector:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * generic interface which can be used by objects which implement 
 * matint in order to perform  the isvector test
 * 
 * Returns:  1 or %RET_BUG
 **/
int int_matint_isvector(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *B;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (B = (NspSMatrix *) nsp_get_object(stack,1)) == NULLSMAT )
    return RET_BUG;

  if ( nsp_move_boolean(stack,1, B->m == 1 || B->n == 1) == FAIL )
    return RET_BUG;
  return 1;
}

/**
 * int_matint_redim:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * redim interface for objects which implements matint 
 * interface 
 * 
 * 
 * Return value:  1 or %RET_BUG
 **/
int int_matint_redim(Stack stack, int rhs, int opt, int lhs) 
{
  NspTypeBase *type;
  int mm,nn;
  CheckRhs (2,3);
  CheckLhs (0,1);
  NspObject *Obj;
  NspMatrix *B;

  if ((Obj = nsp_get_object_copy(stack,1))== NULL) return RET_BUG;

  if ( rhs == 2 )
    { 
      if ((B = GetRealMat (stack, 2)) == NULLMAT)
	return RET_BUG;
      if (B->mn != 2)
	{
	  Scierror ("Error:\t expecting a vector of size 2\n");
	  return RET_BUG;
	}
      mm = (int) B->R[0];
      nn = (int) B->R[1];
    }
  else
    {
      if (GetScalarInt (stack, 2, &mm) == FAIL) return RET_BUG;
      if (GetScalarInt (stack, 3, &nn) == FAIL) return RET_BUG;
    }

  if ( mm < -1 || nn < -1 )
    {
      Scierror("Error:\tBad second or third arguments (must be >= -1)\n");
      return RET_BUG;
    }
  if (( type = check_implements(Obj,nsp_type_matint_id)) == NULL )
    {
      Scierror("Error: first argument does not implements matint interface\n");
      return RET_BUG;
    }
  /* 
   * here we could call directly 
   * return nsp_matint_redim(self, m, n) == OK ? 0 : RET_BUG;
   * considering that all the matrices implementing matint 
   * will use default redim function. But just in case one 
   * class decided to particularize redim we have to 
   * call MAT_INT(type)->redim(self,m1,n1)
   * 
   */
  if ( MAT_INT(type)->redim(Obj,mm,nn)  == FAIL) return  RET_BUG;
  MoveObj(stack,1,Obj);
  return 1;
}

/**
 * int_matint_repmat:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * repmat interface for objects which implements matint interface 
 * 
 * 
 * Return value:  1 or %RET_BUG
 **/

int int_matint_repmat(Stack stack, int rhs, int opt, int lhs) 
{
  NspTypeBase *type;
  int m,n;
  CheckRhs (3,3);
  CheckLhs (1,1);
  NspObject *ObjA, *ObjB;

  if ((ObjA = nsp_get_object(stack,1))== NULL) return RET_BUG;

  if (GetScalarInt (stack, 2, &m) == FAIL) return RET_BUG;
  CheckNonNegative(NspFname(stack),m,2);

  if (GetScalarInt (stack, 3, &n) == FAIL) return RET_BUG;
  CheckNonNegative(NspFname(stack),n,3);

  if (( type = check_implements(ObjA,nsp_type_matint_id)) == NULL )
    {
      Scierror("Error: first argument does not implements matint interface\n");
      return RET_BUG;
    }

  if ( (ObjB = nsp_matint_repmat(ObjA, m, n)) == NULLOBJ ) return  RET_BUG;
  MoveObj(stack,1,ObjB);
  return 1;
}

/**
 * nsp_matint_concat_diag:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ObjB: a #Matrix (that is a #NspObject which implements the matint interface)
 * 
 * returns [@ObjA # @ObjB]
 * 
 * Return value: a new #NspObject or %NULLOBJ. 
 **/

NspObject *nsp_matint_concat_diag( NspObject *ObjA, NspObject *ObjB)
{
  NspObject *ObjC=NULLOBJ;
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB, *C;
  int i, j;
  NspTypeBase *type; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  type = check_implements(ObjA, nsp_type_matint_id);  /* ObjA and ObjB must have the same type to send here 
                                                         (so we don't check) */

  /* be sure that we are back converted */
  MAT_INT(type)->canonic(ObjA);  
  MAT_INT(type)->canonic(ObjB);

  elt_size_A = MAT_INT(type)->elt_size(ObjA);  /* but there is the problem real/complex */
  elt_size_B = MAT_INT(type)->elt_size(ObjB);  /* for Matrix and MaxpMatrix */

  if ( A->m == 0  &&  A->n == 0 )
    {
      ObjC = nsp_object_copy(ObjB); 
      return ObjC;
    }     
  else if ( B->m == 0  &&  B->n == 0 )
    {
      ObjC = nsp_object_copy(ObjA);
      return ObjC;
    }
  /* Matrices of numbers or booleans */
  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  
    {
      if ( elt_size_A == elt_size_B )
	{
	  if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n+B->n, TRUE)) != NULLOBJ )
	    {
	      int step=elt_size_A;
	      char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
	      C = (NspSMatrix *) ObjC;  to = (char *) C->S;
	      for ( j = 0 ; j < A->n ; j++ ) 
		memcpy(to+j*(C->m)*step,fromA+j*A->m*step,A->m*step);
	      for ( j = 0 ; j < B->n ; j++ ) 
		memcpy(to+(j+A->n)*(C->m)*step+A->m*step,fromB+j*B->m*step,B->m*step);
	    }
	}
      else   
	{
	  if ( nsp_object_type (ObjA, nsp_type_matrix_id) || 
	       nsp_object_type (ObjA, nsp_type_mpmatrix_id))
	    {
	      /* one matrix is real and the other is complex */
	      if ( elt_size_A > elt_size_B )  
		{
		  ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n+B->n, TRUE);
		  if ( ObjC != NULLOBJ )
		    {
		      /* A is complex, B real */
		      int stepA=elt_size_A;
		      int stepB=elt_size_B;
		      char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
		      C = (NspSMatrix *) ObjC;  to = (char *) C->S;
		      for ( j = 0 ; j < A->n ; j++ ) 
			{ 
			  memcpy(to+j*(C->m)*stepA,fromA+j*A->m*stepA,A->m*stepA);
			}
		      for ( j = 0 ; j < B->n ; j++ ) 
			{
			  doubleC *elt =(doubleC *) (to +(j+A->n)*(C->m)*stepA + A->m*stepA);
			  for ( i = 0 ; i < B->m ; i++)
			    {
			      elt[i].r = *(((double *) (fromB+j*B->m*stepB)) +i);
			      elt[i].i = 0.0;
			    }
			}
		    }
		}
	      else 
		{ 
		  /* A is real, B complex */
		  ObjC = MAT_INT(type)->clone(NVOID, ObjB,A->m+B->m,A->n+B->n, TRUE);
		  if ( ObjC != NULLOBJ )
		    {
		      int stepA=elt_size_A;
		      int stepB=elt_size_B;
		      char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
		      C = (NspSMatrix *) ObjC;  to = (char *) C->S;
		      for ( j = 0 ; j < A->n ; j++ ) 
			{ 
			  doubleC *elt =(doubleC *) (to +j*(C->m)*stepB);
			  /* copy column j of A which is real */
			  for ( i = 0 ; i < A->m ; i++)
			    {
			      elt[i].r = *(((double *) (fromA+j*A->m*stepA)) +i);
			      elt[i].i = 0.0;
			    }
			}
		      for ( j = 0 ; j < B->n ; j++ ) 
			{
			  /* copy column j of B which is complex as C */
			  memcpy(to+(j+A->n)*(C->m)*stepB+A->m*stepB,fromB+j*B->m*stepB,B->m*stepB);
			}
		    }
		}
	    }
	  else
	    {
	      /* int matrices of different types */
	      NspIMatrix *AA = (NspIMatrix *) ObjA, *BB = (NspIMatrix *) ObjB, *CC;
	      nsp_itype itype = ( elt_size_A < elt_size_B ) ? AA->itype : BB->itype;
	      if ((CC = nsp_imatrix_create(NVOID,A->m+B->m,A->n+B->n,itype) )== NULL)
		return NULLOBJ;
	      ObjC = (NspObject *) CC;
	      /* 
	      NSP_COPY_ITYPE_TO_ITYPE(CC,0,CC->itype,i,0,1,AA->mn,AA->Iv,AA->itype);
	      NSP_COPY_ITYPE_TO_ITYPE(CC,AA->mn,CC->itype,i,0,1,BB->mn,BB->Iv,BB->itype);
	      */
	      if ( elt_size_A > elt_size_B )  
		{
		  int stepA=elt_size_A;
		  int stepB=elt_size_B;
		  char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
		  C = (NspSMatrix *) ObjC;  to = (char *) C->S;
		  for ( j = 0 ; j < A->n ; j++ ) 
		    { 
		      memcpy(to+j*(C->m)*stepA,fromA+j*A->m*stepA,A->m*stepA);
		    }
		  for ( j = 0 ; j < B->n ; j++ ) 
		    {
		      int offset = (j+A->n)*(C->m)*stepA + A->m*stepA;
		      NSP_COPY_ITYPE_TO_ITYPE(CC,offset,CC->itype,i,0,1,BB->m,(fromB+j*B->m*stepB),BB->itype);
		    }
		}
	      else 
		{ 
		  int stepA=elt_size_A;
		  int stepB=elt_size_B;
		  char *to, *fromA = (char *) A->S, *fromB = (char *) B->S;
		  C = (NspSMatrix *) ObjC;  to = (char *) C->S;
		  for ( j = 0 ; j < A->n ; j++ ) 
		    { 
		      /* copy column j of A */
		      int offset = j*(C->m)*stepB;
		      NSP_COPY_ITYPE_TO_ITYPE(CC,offset,CC->itype,i,0,1,A->m,(fromA+j*A->m*stepA),AA->itype);
		    }
		  for ( j = 0 ; j < B->n ; j++ ) 
		    {
		      /* copy column j of B which is complex as C */
		      memcpy(to+(j+A->n)*(C->m)*stepB+A->m*stepB,fromB+j*B->m*stepB,B->m*stepB);
		    }
		}
	    }
	}
    }
  else                                                      
    {
      /* Matrices of pointers (String, cells, poly,...) */
      if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n+B->n, TRUE)) != NULLOBJ )
	{
	  C = (NspSMatrix *) ObjC;
	  char *elt;
	  for ( j = 0 ; j < A->n ; j++ ) 
	    {
	      char **fromA= A->S+j*A->m;
	      char **fromB= B->S+j*B->m;
	      char **toA = C->S+j*(C->m),**toB = C->S+(j+A->n)*(C->m)+A->m;
	      for ( i = 0 ; i < A->m ; i++ )
		{
		  if ( (elt = (char *) MAT_INT(type)->copy_elt(fromA[i])) == NULL ) goto err;
		  toA[i]=elt;
		}
	      for ( i = 0 ; i < B->m ; i++ )
		{
		  if ( (elt = (char *) MAT_INT(type)->copy_elt(fromB[i])) == NULL ) goto err;
		  toB[i]=elt;
		}
	    }
	}
    }
  return ObjC;
 err:
  nsp_object_destroy(&ObjC); 
  return NULLOBJ;
}


/**
 * int_matint_concat_diag:
 * @stack: #Stack object 
 * @rhs: number of arguments 
 * @opt: number of optional arguments 
 * @lhs: expected returned arguments 
 * 
 * interface for concat_diag which is to be used 
 * when calling concatdiag_x_x and x is supposed 
 * to implement the matint interface 
 * similar to concat_diag
 * 
 * Return value: 1 or %RET_BUG
 **/

int int_matint_concat_diag(Stack stack, int rhs, int opt, int lhs) 
{
  /* objects with matint interface can be casted to NspSMatrix */
  NspSMatrix *A,*B;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ((A = (NspSMatrix *)nsp_get_object(stack, 1)) == NULL)   return RET_BUG;
  if ((B = (NspSMatrix *)nsp_get_object(stack, 2)) == NULL)   return RET_BUG;
  if (A->mn == 0)
    {
      /* this is a bit tricky since A and B may point to the same object */
      if ( A == B ) 
	{
	  NthObj(2) = NULLOBJ;
	  NSP_OBJECT(A)->ret_pos = 1;
	}
      else 
	{
	  NSP_OBJECT(B)->ret_pos = 1;
	}
      return 1;
    }

  if (B->mn == 0)
    {
      /* this is a bit tricky since A and B may point to the same object */
      if ( A == B )  NthObj(2) = NULLOBJ;
      NSP_OBJECT(A)->ret_pos = 1;
    }
  else
    {
      NspObject *HMat3;
      if ((HMat3 = nsp_matint_concat_diag(NSP_OBJECT(A), NSP_OBJECT(B))) == NULLOBJ)
	return RET_BUG;
      MoveObj (stack, 1, HMat3);
    }
  return 1;
}


/**
 * nsp_matint_canonic:
 * @obj: a #NspObject
 * 
 * sets a matint object to its canonic type. This is 
 * mainly used to back-convert matrices to double since 
 * matint functions won't work on non canonic matrices. 
 * The function given here is to be used as defaut value.
 * 
 * Return value: a back converted object. 
 **/

NspObject * nsp_matint_canonic(NspObject *obj)
{
  return obj;
}


/**
 * nsp_matint_to_cells:
 * @A: a #NspObject which implements MatInt 
 * @dim: an integer 
 * 
 * create a new #NspCells C 
 * for dim -1 or 0 : C is such that C{i,j}==A(i,j).
 * for dim 1 : C is a row cell C{i}= A(i,:);
 * for dim 2 : C is a column cell C{j}= A(:,j);
 * 
 * Returns: a new #NspCells or NULLCELLS
 **/

static NspCells *nsp_matint_to_cells(NspSMatrix *A,int  dim)
{
  int i;
  NspObject *Res= NULLOBJ;
  NspCells *Loc=NULLCELLS; 
  NspMatrix *Elts=NULLMAT; 
  switch (dim) 
    {
    case 0:
    case -1: 
      if ((Loc = nsp_cells_create(NVOID, A->m,A->n)) == NULLCELLS) 
	return NULLCELLS;
      if ((Elts = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT )
	goto fail;
      for ( i = 0 ; i < A->mn; i++) 
	{
	  Elts->R[0]=i+1;
	  if ((Res= nsp_matint_extract_elements1((NspObject *) A,(NspObject *)Elts))== NULLOBJ)
	    goto fail;
	  Loc->objs[i]=Res;
	}
      nsp_matrix_destroy(Elts);
      return Loc;
    case 1:
      /* row case */
      if ((Loc = nsp_cells_create(NVOID, A->m,1)) == NULLCELLS) 
	return NULLCELLS;
      if ((Elts = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT )
	goto fail;
      for ( i = 0 ; i < A->m; i++) 
	{
	  Elts->R[0]=i+1;
	  if ((Res = nsp_matint_extract_rows1((NspObject *) A,(NspObject *)Elts))== NULLOBJ)
	    goto fail;
	  Loc->objs[i]=Res;
	}
      nsp_matrix_destroy(Elts);
      return Loc;
    case 2:
      /* column */
      if ((Loc = nsp_cells_create(NVOID, 1, A->n)) == NULLCELLS) 
	return NULLCELLS;
      if ((Elts = nsp_matrix_create(NVOID,'r',1,1))== NULLMAT )
	goto fail;
      for ( i = 0 ; i < A->n ; i++) 
	{
	  Elts->R[0]=i+1;
	  if ((Res = nsp_matint_extract_columns1((NspObject *) A,(NspObject *)Elts))== NULLOBJ)
	    goto fail;
	  Loc->objs[i]=Res;
	}
      nsp_matrix_destroy(Elts);
      return Loc;
      
    default:
      Scierror("Error: dim specification not valid\n");
      goto fail;
    }
 fail:
  nsp_matrix_destroy(Elts);
  nsp_cells_destroy(Loc);
  return NULLCELLS;
}

/**
 * nsp_matint_set_diag:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ObjB: a #Matrix (that is a #NspObject which implements the matint interface)
 * @k: diagonal 
 * 
 * set the k-th diagonal of matrix @A using matrix @B. This function is used 
 * in the set_diag method, note that for #NspMatrix and #NspBMatrix we directly 
 * use nsp_matrix_set_diag() and nsp_bmatrix_set_diag(). 
 * 
 * Returns: %OK or %FAIL
 **/


static int nsp_matint_set_diag(NspObject *ObjA,NspObject *ObjB,int k)
{
  NspSMatrix *A = (NspSMatrix *) ObjA, *Diag = (NspSMatrix *) ObjB;
  char *to, *from;
  int i,j;
  int imin,imax,isize;
  NspTypeBase *typeA, *typeB; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  typeA = check_implements(ObjA, nsp_type_matint_id);
  typeB = check_implements(ObjB, nsp_type_matint_id);

  if ( typeA != typeB )
    {
      Scierror("Error: in set_diag method matrix and diag matrix must be of the same type\n");
      return FAIL;
    }
  
  MAT_INT(typeA)->canonic(ObjA);
  MAT_INT(typeB)->canonic(ObjB);
  
  elt_size_A = MAT_INT(typeA)->elt_size(ObjA); 
  elt_size_B = MAT_INT(typeB)->elt_size(ObjB); 
  
  imin = Max(0,-k);
  imax = Min(A->m,A->n -k );
  isize = imax-imin ;

  if ( isize > Diag->mn ) 
    {
      Scierror("Error:\tGiven diagonal vector is too small \n");
      return(FAIL);
    }
  if ( isize < Diag->mn ) 
    {
      imax = Diag->mn +imin;
      if ( MAT_INT(typeA)->enlarge(ObjA,imax,imax+k) == FAIL )
	return FAIL;
    }

  to = (char *) A->S; from = (char *) Diag->S;
  
  if ( MAT_INT(typeA)->free_elt == (matint_free_elt *) 0 ) 
    {
      /* object not of pointer type */
      if ( nsp_object_type (ObjA, nsp_type_matrix_id) || 
	   nsp_object_type (ObjA, nsp_type_mpmatrix_id) )
	{
	  return nsp_matrix_set_diag((NspMatrix *)A, (NspMatrix *)Diag, k);
	}
      else if ( nsp_object_type (ObjA, nsp_type_imatrix_id)) 
	{
	  return nsp_imatrix_set_diag((NspIMatrix *)A, (NspIMatrix *)Diag, k);
	}
      else if ( elt_size_A == sizeof(int) )
	{
	  return nsp_bmatrix_set_diag((NspBMatrix *)A, (NspBMatrix *)Diag, k);
	}
      else
	{
	  j=0;
	  for ( i = imin ; i < imax ; i++ ) 
	    memcpy(((NspBMatrix *)A)->B+ (i+(i+k)*A->m)*elt_size_A, 
		   ((NspBMatrix *) Diag)->B + (j++)*elt_size_A,elt_size_A );
	}
    }
  else
    {
      j=0;
      for ( i = imin ; i < imax ; i++ ) 
	{
	  char **fromv = (char **) from, **tov = (char **) to, *elt;
	  if (fromv[j] != NULL) 
	    {
	      MAT_INT(typeA)->free_elt( (void **) (tov +i+(i+k)*A->m ) ); 
	      if ( (elt = (char *) MAT_INT(typeA)->copy_elt(fromv[j++])) == NULL ) return FAIL;
	      tov[i+(i+k)*A->m] = elt;
	    }
	  else 
	    {
	      tov[i+(i+k)*A->m] = NULL;
	    }
	}
    }
  return OK;
}

