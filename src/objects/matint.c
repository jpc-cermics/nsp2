/* Nsp
 * Copyright (C) 1998-2006 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2006      Bruno Pincon Esial/Iecn
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

#define WORK_SIZE 100
static int matint_work[2][WORK_SIZE];

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

  if ((type =  malloc(sizeof(NspTypeMatint)))==NULL) return NULL;
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
    if ( GetDimArg(stack, 3, &dim) == FAIL )
      return RET_BUG;

  if ( nsp_matint_perm_elem(self, p, q, dim) == FAIL )
    return RET_BUG;

  return 0;
}

static NspMethods matint_methods[] = {
  {"redim",(nsp_method *) int_matint_meth_redim},
  {"concatr",(nsp_method *) int_matint_meth_concatr},
  {"concatd",(nsp_method *) int_matint_meth_concatd},
  {"perm_elem",(nsp_method *) int_matint_perm_elem},
  { NULL, NULL}
};


NspMethods *matint_get_methods(void) { return matint_methods;};

/**
 * nsp_matint_bounds:
 * @A:  a #NspMatrix supposed to be a vector of indices
 * @ind: a int vector (with length >= length(A)  
 * @imin: min of @A after cast to int
 * @imax: max of @A after cast to int
 *
 * cast elements of A to integers in ind (minus 1 such that ind is "0-based")
 * and computes the min and max of A (here staying "1-based")   
 */

static void nsp_matint_bounds(const NspMatrix *A, int *ind, int *imin, int *imax)
{
  int i, ival;
  *imax = 1;
  *imin = 1;
  switch (  A->convert ) 
    {
    case 'd':
      for (i = 0; i < A->mn; i++)
	{
	  ival = (int) A->R[i];
	  if (ival > *imax)
	    *imax = ival;
	  else if (ival < *imin)
	    *imin = ival;
	  ind[i] = ival-1;
	}
      break;
    case 'i' : 
      for (i = 0; i < A->mn; i++)
	{
	  ival = A->I[i];
	  if (ival > *imax)
	    *imax = ival;
	  else if (ival < *imin)
	    *imin = ival;
	  ind[i] = ival-1;
	}
      break;
    default:
      { 
	float *AF = (float *) A->R;
	for (i = 0; i < A->mn; i++)
	  {
	    ival = (int) AF[i];
	    if (ival > *imax)
	      *imax = ival;
	    else if (ival < *imin)
	      *imin = ival;
	    ind[i] = ival-1;
	  }
      }
      break;
    }
}

/**
 * nsp_matint_indices_for_deletions:
 *
 * used for deletions operations (A(ind,:)=[], A(:,ind)=[], A(ind)=[])
 * performs:
 *       re-ordering of indices (if needed) 
 *       in case of duplicated indices it compress the array
 */

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


static int *get_index_vector(Stack stack, int ipos, int *Nb_elts, int *Rmin, int *Rmax, matint_workid iwork)
{
  int nb_elts=0, rmin, rmax, *ind=NULL;

  if ( IsBMatObj(stack, ipos) )
    {
      int i, j=0;
      NspBMatrix *BElts = GetBMat(stack, ipos);
      for ( i = 0 ; i < BElts->mn ; i++ ) 
	if ( BElts->B[i] ) nb_elts++;

      if ( nb_elts > WORK_SIZE )
	{ if ( (ind = nsp_alloc_int(nb_elts)) == NULL ) return NULL; }
      else
	ind = matint_work[iwork];

      for ( i = 0 ; i < BElts->mn ; i++ ) 
	if ( BElts->B[i] ) ind[j++] = i;
      rmin = ind[0]+1; rmax = ind[nb_elts-1]+1;
    }
  else if ( IsMatObj(stack, ipos) )
    {
      NspMatrix *Elts;
      /* Elts must be a real matrix  * */
      if ( (Elts =GetRealMat_G(stack, ipos)) == NULLMAT )
	return NULL;
      nb_elts = Elts->mn; 
      if (  nb_elts > WORK_SIZE ) 
	{ if ( (ind = nsp_alloc_int(nb_elts)) == NULL ) return NULL; }
      else
	ind = matint_work[iwork];
      nsp_matint_bounds(Elts, ind, &rmin, &rmax);
    }

  *Nb_elts = nb_elts; *Rmin = rmin; *Rmax = rmax;
  return ind;
}


/**
 * get_index_vector_from_object:
 * @Obj: 
 * @Nb_elts: 
 * @Rmin: 
 * @Rmax: 
 * @iwork: 
 * 
 * 
 * Return value: 
 **/
/* previous function could use this one XXX */

int *get_index_vector_from_object(NspObject *Obj, int *Nb_elts, int *Rmin, int *Rmax,matint_workid iwork)
{
  int nb_elts=0, rmin, rmax, *ind=NULL;

  if ( IsBMat(Obj))
    {
      int i, j=0;
      NspBMatrix *BElts = (NspBMatrix *) Obj;
      for ( i = 0 ; i < BElts->mn ; i++ ) 
	if ( BElts->B[i] ) nb_elts++;
      if ( nb_elts > WORK_SIZE )
	{ if ( (ind = nsp_alloc_int(nb_elts)) == NULL ) return NULL; }
      else
	ind = matint_work[iwork];
      for ( i = 0 ; i < BElts->mn ; i++ ) 
	if ( BElts->B[i] ) ind[j++] = i;
      rmin = ind[0]+1; rmax = ind[nb_elts-1]+1;
    }
  else
    {
      NspMatrix *Elts = (NspMatrix *) Obj;
      if ( Elts->rc_type != 'r') return NULL;
      nb_elts = Elts->mn; 
      if (  nb_elts > WORK_SIZE ) 
	{ if ( (ind = nsp_alloc_int(nb_elts)) == NULL ) return NULL; }
      else
	ind = matint_work[iwork];
      nsp_matint_bounds(Elts, ind, &rmin, &rmax);
    }

  *Nb_elts = nb_elts; *Rmin = rmin; *Rmax = rmax;
  return ind;
}


/**
 * nsp_matint_delete_columns:
 * @Obj: a #NspObject which implements #matint
 * @ind: an integer vector giving the indices of the columns to be deleted.
 * @nb_elts: size of @ind
 * @cmin: min of @ind
 * @cmax: max of @ind
 *
 * delete columns (specified by ind) of object @Obj which must implements the #matint interface 
 * A(:,ind) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_columns(NspObject  *Obj, int *ind, int nb_elts, int cmin, int cmax)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val = (char *) A->S;
  unsigned int elt_size; /* size in number of bytes */
  NspTypeBase *type; 
  int i, j, k1, k2, nn, ncol, ioff=0;

  if ( nb_elts == 0) return OK;

  if ( cmin < 1 || cmax > A->n )
    {
      Scierror("Error:\tIndices out of bounds\n"); 
      return FAIL;
    }

  nsp_matint_indices_for_deletions(nb_elts, ind, &ncol);

  if ( (type= check_implements(A,nsp_type_matint_id)) == NULL ) 
    { 
      Scierror("Object do not implements matint interface\n"); 
      return FAIL; 
    } 

  elt_size = MAT_INT(type)->elt_size(A); 
  if ( MAT_INT(type)->free_elt != NULL)  
    for ( j = 0 ; j < ncol ; j++ )  
      {
	int k=ind[j]*A->m;
	for ( i = 0 ; i < A->m ; i++ )  
	  MAT_INT(type)->free_elt((void **) &(A->S[i+k])); 
      }

  for ( i = 0 ; i < ncol ; i++)
    {
      k1 = ind[i];
      k2 = (i < ncol-1 ) ? ind[i+1] : A->n;
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
 * @ind: an integer vector giving the indices of the columns to be deleted.
 * @nb_elts: size of @ind
 * @rmin: min of @ind
 * @rmax: max of @ind
 *
 * delete rows (specified by ind) of object @Obj which must implements the #matint interface 
 * A(ind,:) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_rows(NspObject *Obj, int *ind, int nb_elts, int rmin, int rmax)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val = (char *) A->S;
  unsigned int elt_size; /* size in number of bytes */
  NspTypeBase *type; 
  int i, j, k1, k2, nn, nrow, stride=0, ioff=0;

  if ( nb_elts == 0) return OK;

  if ( rmin < 1 || rmax > A->m )
    {
      Scierror("Error:\tIndices out of bounds\n"); 
      return FAIL;
    }

  nsp_matint_indices_for_deletions(nb_elts, ind, &nrow);

  if ( (type =check_implements(A,nsp_type_matint_id)) == NULL ) 
    {  
      Scierror("Object do not implements matint interface\n"); 
      return FAIL; 
    } 
  elt_size = MAT_INT(type)->elt_size(A); 

  if ( MAT_INT(type)->free_elt != NULL)  
    for ( i = 0 ; i < nrow ; i++ ) 
      {
	int k=ind[i];
	for ( j = 0 ; j < A->n ; j++ ) 
	  MAT_INT(type)->free_elt((void **) &(A->S[k+A->m*j])); 
      }
  for ( j = 0 ; j < A->n  ; j++)
    {
      k1 = ind[0] + stride;
      for ( i = 0 ; i < nrow ; i++)
	{
	  if ( i < nrow-1 ) 
	    k2 =  ind[i+1] + stride;
	  else 
	    k2 = ( j < A->n-1) ? ind[0] + stride + A->m : A->mn;
	  nn = k2-k1-1;
	  if ( nn != 0) 
	    memmove(Val + (k1-ioff)*elt_size, Val + (k1+1)*elt_size, nn*elt_size);
	  ioff++;
	  k1 = k2;
	}
      stride += A->m;
    }

  if ( MAT_INT(type)->free_elt != NULL) 
    for ( i = A->mn- nrow*A->n ; i < A->mn ; i++ ) 
      A->S[i] = NULL;

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

  free(A->S); A->S = NULL;
  A->mn = A->m = A->n = 0;
  return OK;
}


/**
 * nsp_matint_delete_elements:
 * @Obj: a #NspObject which implements #matint
 * @ind: an integer vector giving element indices to be deleted.
 * @nb_elts: size of @ind
 * @rmin: min of @ind
 * @rmax: max of @ind
 *
 * delete the elements of object @Obj which must implements the #matint interface 
 * A(ind) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_elements(NspObject *Obj, int *ind, int nb_elts, int rmin, int rmax)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val = (char *) A->S;
  unsigned int elt_size; /* size in number of bytes */
  int i, k1, k2, nn, ne, ioff=0; 
  NspTypeBase *type; 

  if ( nb_elts == 0) return OK;

  if ( rmin < 1 || rmax > A->mn )
    {
      Scierror("Error:\tIndices out of bounds\n"); 
      return FAIL;
    }

  nsp_matint_indices_for_deletions(nb_elts, ind, &ne);

  if (( type = check_implements(A,nsp_type_matint_id)) == NULL ) 
    { 
      Scierror("Object do not implements matint interface\n"); 
      return FAIL; 
    } 
  elt_size = MAT_INT(type)->elt_size(A); 

  if ( MAT_INT(type)->free_elt != NULL)  
    for ( i = 0 ; i < ne ; i++ )  MAT_INT(type)->free_elt((void **) &(A->S[ind[i]])); 

  k1 = ind[0];
  for ( i = 0 ; i < ne ; i++)
    {
      k2 = ( i < ne-1 ) ? ind[i+1] : A->mn;
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
  char *Val = (char *) A->S;
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
 * @ind: integer vecteur with the indices (0-based)
 * @nb_elts: length of @ind that is number of indices
 * @rmin: min index (1-based)
 * @rmax: max index (1-based)
 *
 * Compute Obj(ind) and returns the new #Matrix 
 * 
 * returns a #Matrix or %NULLOBJ
 */

NspObject *nsp_matint_extract_elements(NspObject *Obj, const int *ind, int nb_elts, int rmin, int rmax)
{
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *from = (char *) A->S, *to;
  NspObject *Loc;
  NspSMatrix *B;
  int i;
  NspTypeBase *type; 
  unsigned int elt_size; /* size in number of bytes */

  type = check_implements(Obj, nsp_type_matint_id);

  if ( nb_elts == 0 )
    return MAT_INT(type)->clone(NVOID, Obj, 0, 0, FALSE);  /* ici a adapter pour mtlb mode */

  elt_size = MAT_INT(type)->elt_size(Obj); 

  if ( A->m == 0 || A->n == 0) 
    {
      return nsp_object_copy(Obj);
    }

  if ( rmin < 1 || rmax > A->mn )
    {
      Scierror("Error:\tIndices out of bound\n");
      return NULLOBJ;
    }

  if ( A->m == 1 && A->n > 1 )
    {
      if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, 1, nb_elts, FALSE)) == NULLOBJ ) 
	return NULLOBJ;
    }
  else
    {
      if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, nb_elts, 1,FALSE)) == NULLOBJ ) 
	return NULLOBJ;
    }

  B = (NspSMatrix *) Loc; to = (char *) B->S;

  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      if ( elt_size == sizeof(double) )
	{
	  double *fromd = (double *) from, *tod = (double *) to;
	  for ( i = 0 ; i < nb_elts ; i++ )
	    tod[i] = fromd[ind[i]];
	}
      else if ( elt_size == sizeof(doubleC) )
	{
	  doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
	  for ( i = 0 ; i < nb_elts ; i++ )
	    toc[i] = fromc[ind[i]];
	}
      else if ( elt_size == sizeof(int) )
	{
	  int *fromi = (int *) from, *toi = (int *) to;
	  for ( i = 0 ; i < nb_elts ; i++ )
	    toi[i] = fromi[ind[i]];
	}
      else
	{
	  for ( i = 0 ; i < nb_elts ; i++ )
	    memcpy(to + i*elt_size, from + ind[i]*elt_size, elt_size);
	}
    }
  else                                                        /* Matrix of pointers (cells, strings, poly,...) */
    {
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      for ( i = 0 ; i < nb_elts ; i++ )
	{
	  if ( fromv[ind[i]] != NULL )   /* just for cells which may have undefined elements */
	    {
	      if ( (elt = (char *) MAT_INT(type)->copy_elt(fromv[ind[i]])) == NULL )
		{
		  nsp_object_destroy(&Loc); 
		  return NULLOBJ;
		}
	      tov[i] = elt;
	    }
	}
    }
  return Loc;
}

NspObject *nsp_matint_extract_elements1(NspObject *Obj,NspObject *Elts)
{
  int *elts,nr, rmin, rmax;
  elts = get_index_vector_from_object(Elts,&nr, &rmin, &rmax,matint_iwork1);
  if ( elts == NULL) return NULLOBJ;
  return nsp_matint_extract_elements(Obj,elts,nr, rmin, rmax);
}


/**
 * nsp_matint_extract_columns:
 * @Obj: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ind: integer vecteur with the indices (0-based)
 * @nb_elts: length of @ind that is number of indices
 * @cmin: min index (1-based)
 * @cmax: max index (1-based)
 *
 * Compute Obj(:,ind) and returns the new #Matrix 
 * 
 * returns a #Matrix or %NULLOBJ
 */

NspObject *nsp_matint_extract_columns(NspObject *Obj, const int *ind, int nb_elts, int cmin, int cmax)
{
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *from = (char *) A->S, *to;
  NspObject *Loc;
  NspSMatrix *B;
  int i, j, k, ij, L;
  NspTypeBase *type; 
  unsigned int elt_size; /* size in number of bytes */

  type = check_implements(Obj, nsp_type_matint_id);

  if ( nb_elts == 0 )
    return MAT_INT(type)->clone(NVOID, Obj, A->m, 0, FALSE);

  elt_size = MAT_INT(type)->elt_size(Obj); 
  if ( A->m == 0 || A->n == 0) 
    {
      return nsp_object_copy(Obj);
    }
  if ( cmin < 1 || cmax > A->n )
    {
      Scierror("Error:\tIndices out of bound\n");
      return NULLOBJ;
    }

  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, A->m, nb_elts, FALSE)) == NULLOBJ ) 
    return NULLOBJ;

  B = (NspSMatrix *) Loc; to = (char *) B->S;

  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      L = A->m * elt_size;
      for ( j = 0 ; j < nb_elts ; j++, to += L )
	memcpy(to, from + L*ind[j], L);
    }
  else                                                     /* Matrix of pointers (cells, strings, poly,...) */
    {
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      for ( j = 0, k = 0 ; j < nb_elts ; j++ )
	for ( i = 0, ij = A->m*ind[j] ; i < A->m ; i++, ij++, k++ )
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

NspObject *nsp_matint_extract_columns1(NspObject *Obj,NspObject *Cols)
{
  int *cols,nr, rmin, rmax;
  cols = get_index_vector_from_object(Cols,&nr, &rmin, &rmax,matint_iwork1);
  if ( cols == NULL) return NULLOBJ;
  return nsp_matint_extract_columns(Obj,cols,nr, rmin, rmax);
}



/**
 * nsp_matint_extract_rows:
 * @Obj: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ind: integer vecteur with the indices (0-based)
 * @nb_elts: length of @ind that is number of indices
 * @rmin: min index (1-based)
 * @rmax: max index (1-based)
 *
 * Compute Obj(ind,:) and returns the new #Matrix 
 * 
 * returns a #Matrix or %NULLOBJ
 */

NspObject *nsp_matint_extract_rows(NspObject *Obj, const int *ind, int nb_elts, int rmin, int rmax)
{
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *from = (char *) A->S, *to;
  NspObject *Loc;
  NspSMatrix *B;
  int i, j, k;
  NspTypeBase *type; 
  unsigned int elt_size; /* size in number of bytes */

  type = check_implements(Obj, nsp_type_matint_id);

  if ( nb_elts == 0 )
    return MAT_INT(type)->clone(NVOID, Obj, 0, A->n, FALSE);

  elt_size = MAT_INT(type)->elt_size(Obj); 

  if ( A->m == 0 || A->n == 0) 
    {
      return nsp_object_copy(Obj);
    }

  if ( rmin < 1 || rmax > A->m )
    {
      Scierror("Error:\tIndices out of bound\n");
      return NULLOBJ;
    }

  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, nb_elts, A->n, FALSE)) == NULLOBJ ) 
    return NULLOBJ;

  B = (NspSMatrix *) Loc; to = (char *) B->S;

  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      if ( elt_size == sizeof(double) )
	{
	  double *fromd = (double *) from, *tod = (double *) to;
	  for ( j = 0, k = 0 ; j < A->n ; j++ )
	    for ( i = 0 ; i < nb_elts ; i++, k++ )
	      tod[k] = fromd[ind[i]+ j*A->m];
	}
      else if ( elt_size == sizeof(doubleC) )
	{
	  doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
	  for ( j = 0, k = 0 ; j < A->n ; j++ )
	    for ( i = 0 ; i < nb_elts ; i++, k++ )
	      toc[k] = fromc[ind[i]+ j*A->m];
	}
      else if ( elt_size == sizeof(int) )
	{
	  int *fromi = (int *) from, *toi = (int *) to;
	  for ( j = 0, k = 0 ; j < A->n ; j++ )
	    for ( i = 0 ; i < nb_elts ; i++, k++ )
	      toi[k] = fromi[ind[i]+ j*A->m];
	}
      else
	{
	  for ( j = 0 ; j < A->n ; j++ )
	    for ( i = 0 ; i < nb_elts ; i++, to += elt_size )
	      memcpy(to, from + (ind[i]+ j*A->m)*elt_size, elt_size);
	}
    }
  else                                                     /* Matrix of pointers (cells, strings, poly,...) */
    {
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      for ( j = 0, k = 0 ; j < A->n ; j++ )
	for ( i = 0 ; i < nb_elts ; i++, k++ )
	  {
	    if ( fromv[ind[i] + j*A->m] != NULL )   /* just for cells which may have undefined elements */
	      {
		if ( (elt = (char *) MAT_INT(type)->copy_elt(fromv[ind[i] + j*A->m])) == NULL )
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

NspObject *nsp_matint_extract_rows1(NspObject *Obj,NspObject *Rows)
{
  int *rows,nr, rmin, rmax;
  rows = get_index_vector_from_object(Rows,&nr, &rmin, &rmax,matint_iwork1);
  if ( rows == NULL) return NULLOBJ;
  return nsp_matint_extract_rows(Obj,rows,nr, rmin, rmax);
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
 * returns a #Matrix or %NULLOBJ
 */

NspObject *nsp_matint_extract(NspObject *Obj, 
			      const int *row, int nr, int rmin, int rmax,
			      const int *col, int nc, int cmin, int cmax)
{
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *from = (char *) A->S, *to;
  NspObject *Loc;
  NspSMatrix *B;
  int i, j, k, stride;
  NspTypeBase *type; 
  unsigned int elt_size; /* size in number of bytes */

  type = check_implements(Obj, nsp_type_matint_id);

  if ( nr == 0 || nc == 0 )
     return MAT_INT(type)->clone(NVOID, Obj, nr, nc, FALSE);

  elt_size = MAT_INT(type)->elt_size(Obj); 

  if ( rmin < 1 || rmax > A->m || cmin < 1 || cmax > A->n )
    {
      Scierror("Error:\tIndices out of bound\n");
      return NULLOBJ;
    }

  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, nr, nc, FALSE)) == NULLOBJ ) 
    return NULLOBJ;

  B = (NspSMatrix *) Loc; to = (char *) B->S;

  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      if ( elt_size == sizeof(double) )
	{
	  double *fromd = (double *) from, *tod = (double *) to;
	  for ( j = 0, k = 0 ; j < nc ; j++ )
	    {        
	      stride =  col[j]*A->m;
	      for ( i = 0 ; i < nr ; i++, k++ )
		tod[k] = fromd[row[i]+ stride];
	    }
	}
      else if ( elt_size == sizeof(doubleC) )
	{
	  doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
	  for ( j = 0, k = 0 ; j < nc ; j++ )
	    {        
	      stride =  col[j]*A->m;
	      for ( i = 0 ; i < nr ; i++, k++ )
		toc[k] = fromc[row[i]+ stride];
	    }
	}
      else if ( elt_size == sizeof(int) )
	{
	  int *fromi = (int *) from, *toi = (int *) to;
	  for ( j = 0, k = 0 ; j < nc ; j++ )
	    {        
	      stride =  col[j]*A->m;
	      for ( i = 0 ; i < nr ; i++, k++ )
		toi[k] = fromi[row[i]+ stride];
	    }
	}
      else
	{
	  for ( j = 0 ; j < nc ; j++ )
	    {
	      stride =  col[j]*A->m;
	      for ( i = 0 ; i < nr ; i++, to += elt_size )
		memcpy(to, from + (row[i]+ stride)*elt_size, elt_size);
	    }
	}
    }
  else                                                     /* Matrix of pointers (cells, strings, poly,...) */
    {
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      for ( j = 0, k = 0 ; j < nc ; j++ )
	{
	  stride = col[j]*A->m;
	  for ( i = 0 ; i < nr ; i++, k++ )
	    {
	      if ( fromv[row[i] + stride] != NULL )   /* just for cells which may have undefined elements */
		{
		  if ( (elt = (char *) MAT_INT(type)->copy_elt(fromv[row[i]+stride])) == NULL )
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


NspObject *nsp_matint_extract1(NspObject *Obj,NspObject *Rows, NspObject *Cols)
{
  int *row,*col,nr, rmin, rmax, nc, cmin, cmax;
  row = get_index_vector_from_object(Rows,&nr, &rmin, &rmax,matint_iwork1);
  if ( row == NULL) return NULLOBJ;
  col = get_index_vector_from_object(Cols,&nc, &cmin, &cmax,matint_iwork1);
  if ( col == NULL) return NULLOBJ;
  return nsp_matint_extract(Obj,row,nr, rmin, rmax,col, nc, cmin, cmax);
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
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @row: integer vecteur with the row indices (0-based)
 * @nr: length of @row that is number of row indices
 * @rmin: min index (1-based) in row
 * @rmax: max index (1-based) in row
 * @col: integer vecteur with the column indices (0-based)
 * @nc: length of @col that is number of column indices
 * @cmin: min index (1-based) in col
 * @cmax: max index (1-based) in col
 * @ObjB: a #Matrix (that is a #NspObject which implements the matint interface)
 *
 * Compute ObjA(row,col) = ObjB modifying ObjA in place
 * 
 * returns  %OK or %FAIL.
 */

int nsp_matint_set_submatrix(NspObject *ObjA, 
			     const int *row, int nr, int rmin, int rmax,
			     const int *col, int nc, int cmin, int cmax,
			     NspObject *ObjB)
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
      if ( nr != B->m ||  nc != B->n )
	{
	  Scierror("Error: in A(i,j)=B, incompatible dimensions (indices range versus B size)\n");
	  return FAIL;
	}
    }

  if ( rmin < 1 || cmin < 1 ) 
    {
      Scierror("Error: in A(i,j)=B, non positive indices are not allowed\n");
      return FAIL;
    }

  MAT_INT(typeA)->canonic(ObjA);
  MAT_INT(typeB)->canonic(ObjB);

  elt_size_A = MAT_INT(typeA)->elt_size(ObjA); 
  elt_size_B = MAT_INT(typeB)->elt_size(ObjB); 


  if ( rmax > A->m ||  cmax > A->n )
    {
      if ( MAT_INT(typeA)->enlarge(ObjA, rmax, cmax) == FAIL )
	return FAIL;
    }

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

  to = (char *) A->S; from = (char *) B->S;

  if ( MAT_INT(typeA)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      if ( elt_size_B < elt_size_A )  /* just because A is complex and B real... */
	return nsp_matint_special_set_submatrix(ObjA, row, nr, col, nc, ObjB, typeA);

      if ( elt_size_A == sizeof(double) )
	{
	  double *fromd = (double *) from, *tod = (double *) to;
	  int inc = B->mn == 1 ? 0 : 1;
	  for ( j = 0, k = 0 ; j < nc ; j++ )
	    {
	      stride =  col[j]*A->m;
	      for ( i = 0 ; i < nr ; i++, k+=inc )
		tod[row[i]+ stride] = fromd[k];
	    }

	}
      else if ( elt_size_A == sizeof(doubleC) )
	{
	  doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
	  int inc = B->mn == 1 ? 0 : 1; 
	  for ( j = 0, k = 0 ; j < nc ; j++ )
	    {        
	      stride =  col[j]*A->m;
	      for ( i = 0 ; i < nr ; i++, k+=inc )
		toc[row[i]+ stride] = fromc[k];
	    }
	}
      else if ( elt_size_A == sizeof(int) )
	{
	  int *fromi = (int *) from, *toi = (int *) to;
	  int inc = B->mn == 1 ? 0 : 1; 
	  for ( j = 0, k = 0 ; j < nc ; j++ )
	    {        
	      stride =  col[j]*A->m;
	      for ( i = 0 ; i < nr ; i++, k+=inc )
		toi[row[i]+ stride] = fromi[k];
	    }
	}
      else
	{
	  int inc = B->mn == 1 ? 0 : elt_size_A; 
	  for ( j = 0 ; j < nc ; j++ )
	    {
	      stride =  col[j]*A->m;
	      for ( i = 0 ; i < nr ; i++, from += inc )
		memcpy(to + (row[i]+ stride)*elt_size_A, from, elt_size_A);
	    }
	}
    }
  else                                                     /* Matrix of pointers (cells, strings, poly,...) */
    {
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      int ij, inc = B->mn == 1 ? 0 : 1; 
      for ( j = 0, k = 0 ; j < nc ; j++ )
	{
	  stride = col[j]*A->m;
	  for ( i = 0 ; i < nr ; i++, k+=inc )
	    {
	      ij = row[i]+stride;
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

int nsp_matint_set_submatrix1(NspObject *ObjA,NspObject *Row, NspObject *Col, NspObject *ObjB)
{
  int *row,*col,nr, rmin, rmax, nc, cmin, cmax;
  row = get_index_vector_from_object(Row,&nr, &rmin, &rmax,matint_iwork1);
  if ( row == NULL) return FAIL;
  col = get_index_vector_from_object(Col,&nc, &cmin, &cmax,matint_iwork1);
  if ( col == NULL) return FAIL;
  return nsp_matint_set_submatrix(ObjA,row,nr, rmin, rmax,col, nc, cmin, cmax,ObjB);
}


/**
 * nsp_matint_set_elts:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ind: integer vecteur with the indices (0-based). ind must be a vector that is
 *       @nr == 1 or @nc == 1.
 * @nb_elts: nb of elts in @ind
 * @imin: min index (1-based) in ind
 * @imax: max index (1-based) in ind
 * @ObjB: a #Matrix (that is a #NspObject which implements the matint interface)
 *
 * Compute ObjA(ind) = ObjB modifying ObjA in place
 * 
 * returns  %OK or %FAIL.
 */

int nsp_matint_set_elts(NspObject *ObjA, 
			const int *ind, int nb_elts, int imin, int imax,
			NspObject *ObjB)
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

  if ( imin < 1 )
    {
      Scierror("Error:\tNon Positive indices are not allowed\n");
      return FAIL;
    }

  if ( B->m != 1 && B->n != 1 ) 
    {
      Scierror("Error:\tA(ind)=B, B must be a vector");
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
      if ( nb_elts != B->mn )
	{
	  Scierror("Error:\tA(ind)=B, ind and B have incompatible sizes\n");
	  return FAIL;
	}
    }

  MAT_INT(typeA)->canonic(ObjA);
  MAT_INT(typeB)->canonic(ObjB);

  elt_size_A = MAT_INT(typeA)->elt_size(ObjA); 
  elt_size_B = MAT_INT(typeB)->elt_size(ObjB); 

  if ( imax > A->mn )  /* enlarge A */
    {
      if ( A->mn == 0) 
	{
	  if  ( B->m == 1 )  /* ( B->n != 1) */
	    { 
	      if ( MAT_INT(typeA)->enlarge(ObjA, 1, imax) == FAIL ) return FAIL;
	    }
	  else
	    { 
	      if ( MAT_INT(typeA)->enlarge(ObjA, imax, 1) == FAIL ) return FAIL;
	    }
	}
      else if ( A->m == 1 )
	{
	  if ( A->n == 1 )
	    {
	      if ( B->m == 1 )   /* ( B->n > 1 ) */
		{ 
		  if ( MAT_INT(typeA)->enlarge(ObjA, 1, imax) == FAIL ) return FAIL;
		}
	      else
		{
		  if ( MAT_INT(typeA)->enlarge(ObjA, imax, 1) == FAIL ) return FAIL;
		}
	    }
	  else
	    {
	      if ( MAT_INT(typeA)->enlarge(ObjA, 1, imax) == FAIL ) return FAIL;
	    }
	}
      else if ( A->n == 1)
	{
	  if ( MAT_INT(typeA)->enlarge(ObjA, imax, 1) == FAIL ) return FAIL;
	}
      else
	{
	  Scierror("Error:\tA(ind)=B, ind must be inside A range when A is not a vector\n");
	  return FAIL;
	}
    }

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
  
  to = (char *) A->S; from = (char *) B->S;

  if ( MAT_INT(typeA)->free_elt == (matint_free_elt *) 0 )  /* Matrix of numbers or booleans */
    {
      if ( elt_size_B < elt_size_A )    /* just because A is complex and B real... */
	return nsp_matint_special_set_elts(ObjA, ind, nb_elts, ObjB, typeA);

      if ( elt_size_A == sizeof(double) )
	{
	  double *fromd = (double *) from, *tod = (double *) to;
	  int inc = B->mn == 1 ? 0 : 1; 
	  for ( i = 0, k=0 ; i < nb_elts ; i++, k+=inc )
	    tod[ind[i]] = fromd[k];
	}
      else if ( elt_size_A == sizeof(doubleC) )
	{
	  doubleC *fromc = (doubleC *) from, *toc = (doubleC *) to;
	  int inc = B->mn == 1 ? 0 : 1; 
	  for ( i = 0, k=0 ; i < nb_elts ; i++, k+=inc )
	    toc[ind[i]] = fromc[k];
	}
      else if ( elt_size_A == sizeof(int) )
	{
	  int *fromi = (int *) from, *toi = (int *) to;
	  int inc = B->mn == 1 ? 0 : 1; 
	  for ( i = 0, k=0 ; i < nb_elts ; i++, k+=inc )
	    toi[ind[i]] = fromi[k];
	}
      else
	{
	  int inc = B->mn == 1 ? 0 : elt_size_A; 
	  for ( i = 0 ; i < nb_elts ; i++, from += inc )
	    memcpy(to + ind[i]*elt_size_A, from, elt_size_A);
	}
    }
  else                                                     /* Matrix of pointers (cells, strings, poly,...) */
    {
      char **fromv = (char **) from, **tov = (char **) to, *elt;
      int inc = B->mn == 1 ? 0 : 1; 
      for ( i = 0, k = 0 ; i < nb_elts ; i++, k+=inc )
	{
	  MAT_INT(typeA)->free_elt( (void **) (tov + ind[i]) ); 
	  if ( fromv[k] != NULL )          /* just for cells which may have undefined elements */
	    {
	      if ( (elt = (char *) MAT_INT(typeA)->copy_elt(fromv[k])) == NULL ) return FAIL;
	      tov[ind[i]] = elt;
	    }
	}
    }
  return OK;
}


int nsp_matint_set_elts1(NspObject *ObjA, NspObject *Elts, NspObject *ObjB)
{
  int *elts,nr, rmin, rmax;
  elts = get_index_vector_from_object(Elts,&nr, &rmin, &rmax,matint_iwork1);
  if ( elts == NULL) return FAIL;
  return nsp_matint_set_elts(ObjA,elts,nr, rmin, rmax,ObjB);
}


/**
 * nsp_matint_concat_right:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ObjB: 
 * 
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
	  else    /* one matrix is real and the other is complex */
	    {
	      NspMatrix *AA = (NspMatrix *) ObjA, *BB = (NspMatrix *) ObjB, *CC;
	      if ( elt_size_A > elt_size_B )  /* A is complex, B real */
		{
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
		{                             /* A is real, B complex */
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
	}
      else                                                      /* Matrices of pointers (String, cells, poly,...) */
	{
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

  type = check_implements(ObjA, nsp_type_matint_id);  /* ObjA and ObjB must have the same type to send here
                                                         (so we don't check) */

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
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrices of numbers or booleans */
	{
	  if ( elt_size_A == elt_size_B )
	    {
	      if ( MAT_INT(type)->enlarge(ObjA, Am, nA + nB) == FAIL ) return FAIL;
	      {
		char *to = ((char *) A->S) + elt_size_A*mnA;
		memcpy(to, B->S, elt_size_A*B->mn);
	      }
	    }
	  else    /* one matrix is real and the other is complex */
	    {
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
	}
      else                                                      /* Matrices of pointers (String, cells, poly,...) */
	{
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
      /* Matrices of numbers or booleans */
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  
	{
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
	  else    /* one matrix is real and the other is complex */
	    {
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


int nsp_matint_concat_down_bis(NspObject *ObjA, NspObject *ObjB)
{
  int copy=FALSE;
  NspObject *ObjC=NULLOBJ;
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB, *C=NULL;
  int i, j;
  NspTypeBase *type; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  type = check_implements(ObjA, nsp_type_matint_id);  /* ObjA and ObjB must have the same type to send here 
                                                         (so we don't check) */

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
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )     /* Matrices of numbers or booleans */
	{
	  if ( elt_size_A == elt_size_B )
	    {
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
	  else    /* one matrix is real and the other is complex */
	    {
	      if ( elt_size_A > elt_size_B )  
		{
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
		  /* we need here a resize which can change the elt_size too */
		  /* the solution adopted in concatr is to use complexify which is not generic */
		  /* A is real, B complex */
		  /* see the trick below */
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
		  /* a little trick waiting for XXXX above  */
		  A->m = C->m;
		  A->n = C->n;
		  ((NspMatrix *) A)->rc_type = 'c';
		  A->mn = C->mn;
		  A->S = C->S;
		  C->S = NULL;
		  C->m = C->n = C->mn = 0;
		  nsp_object_destroy(&ObjC);
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

  elt_size_A = MAT_INT(type)->elt_size(ObjA);

  if ( (ObjB = MAT_INT(type)->clone(NVOID, ObjA, m*A->m, n*A->n, FALSE)) == NULLOBJ )
    return ObjB;

  B = (NspSMatrix *) ObjB;
  if ( B->mn == 0 )
    return ObjB;

  if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrices of numbers or booleans */
    {
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
  else                                                      /* Matrices of pointers (String, cells, poly,...) */
    {
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
 * @ObjA: 
 * @p: 
 * @q: 
 * @dim_flag: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_matint_perm_elem(NspObject *ObjA, int p, int q, int dim_flag)
{
  NspSMatrix *A = (NspSMatrix *) ObjA;
  NspTypeBase *type; 
  unsigned int elt_size_A; /* size in number of bytes */
  
  type = check_implements(ObjA, nsp_type_matint_id);
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

/* various deletions functions */

int int_matint_tozero(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;

  Obj = NthObj(1);
  nsp_matint_tozero(Obj);
  Obj->ret_pos = 1;
  return 1;
}

typedef int (*delfunc) (NspObject *Obj, int *ind, int nb_elts, int imin, int imax);

static int int_matint_delete_gen(Stack stack, int rhs, int opt, int lhs, delfunc F)
{
  NspObject *Obj;
  int nb_elts, *ind=NULL, imin, imax;

  Obj = NthObj(1);

  if ( (ind =get_index_vector(stack, 2, &nb_elts, &imin, &imax, matint_iwork1)) == NULL )
    return RET_BUG;

  if ( (*F)(Obj, ind, nb_elts, imin, imax) == FAIL )
    goto err;

  if ( nb_elts > WORK_SIZE ) FREE(ind);
  Obj->ret_pos = 1;
  return 1;

 err:
  if ( nb_elts > WORK_SIZE ) FREE(ind);
  return RET_BUG;
}

int int_matint_deleteelts(Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs == 3 )
    return int_matint_deleteelts2(stack, rhs, opt, lhs);
  else
    return int_matint_delete_gen(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_elements);
}

int int_matint_deletecols(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_delete_gen(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_columns);
}

int int_matint_deleterows(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_delete_gen(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_rows);
}


int int_matint_deleteelts2(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  int nr, nc, *row=NULL, *col=NULL, rmin, rmax, cmin, cmax;

  Obj = NthObj(1);

  if ( (row =get_index_vector(stack, 2, &nr, &rmin, &rmax, matint_iwork1)) == NULL )
    return RET_BUG;
  
  if ( (col =get_index_vector(stack, 3, &nc, &cmin, &cmax, matint_iwork2)) == NULL )
    goto err;
  
  if ( nsp_matint_delete_elements2(Obj, row, nr, rmin, rmax, col, nc, cmin, cmax) == FAIL )
    goto err;

  if ( nr > WORK_SIZE ) FREE(row);
  if ( nc > WORK_SIZE ) FREE(col);
  Obj->ret_pos = 1;
  return 1;
  
 err:
  if ( nr > WORK_SIZE ) FREE(row);
  if ( nc > WORK_SIZE ) FREE(col);
  return RET_BUG;
}


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

typedef NspObject * (*extractfunc) (NspObject *Obj, int *ind, int nb_elts, int imin, int imax);

static int int_matint_extract_gen(Stack stack, int rhs, int opt, int lhs, extractfunc F)
{
  NspObject *Obj, *Res;
  int nb_elts, *ind=NULL, imin, imax;

  CheckLhs (1, 1);
  Obj = NthObj(1);

  if ( (ind =get_index_vector(stack, 2, &nb_elts, &imin, &imax, matint_iwork1)) == NULL )
    {
      if ( rhs != 2 || IsListObj(stack,2) == FALSE ) return RET_BUG;
      /* check if we are using a list access */
      int rep,n ;
      if ( (rep = ListFollowExtract(stack,rhs,opt,lhs)) < 0 ) return rep; 
      /* last extraction : here O can be anything */ 
      if ((n=nsp_eval_func(NULLOBJ,"extractelts",1,stack,stack.first+1,2,0,1)) < 0)
	{
	  return RET_BUG;
	}
      nsp_void_object_destroy(&NthObj(1));
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ( (Res = (*F)(Obj, ind, nb_elts, imin, imax)) == NULLOBJ )
    goto err;

  if ( nb_elts > WORK_SIZE ) FREE(ind);
  MoveObj (stack, 1, Res);
  return 1;

 err:
  if ( nb_elts > WORK_SIZE ) FREE(ind);
  return RET_BUG;
}

int int_matint_extractelts(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_elements);
}

int int_matint_extractcols(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_columns);
}

int int_matint_extractrows(Stack stack, int rhs, int opt, int lhs)
{
  return int_matint_extract_gen(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_rows);
}


int int_matint_extract(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj, *Res;
  int nr, nc, *row=NULL, *col=NULL, rmin, rmax, cmin, cmax;

  CheckLhs (1, 1);
  Obj = NthObj(1);

  if ( (row =get_index_vector(stack, 2, &nr, &rmin, &rmax, matint_iwork1)) == NULL )
    return RET_BUG;
  
  if ( (col =get_index_vector(stack, 3, &nc, &cmin, &cmax, matint_iwork2)) == NULL )
    goto err;
  
  if ( (Res =nsp_matint_extract(Obj, row, nr, rmin, rmax, col, nc, cmin, cmax)) == NULLOBJ )
    goto err;

  if ( nr > WORK_SIZE ) FREE(row);  if ( nc > WORK_SIZE ) FREE(col);
  MoveObj (stack, 1, Res);
  return 1;
  
 err:
  if ( nr > WORK_SIZE ) FREE(row);
  if ( nc > WORK_SIZE ) FREE(col);
  return RET_BUG;
}


/* insertion interface */

int int_matint_setrowscols(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *ObjA, *ObjB;
  int nr=0, nc=0, *row=NULL, *col=NULL, rmin, rmax, cmin, cmax;

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
      if ( (row =get_index_vector(stack, 2, &nr, &rmin, &rmax, matint_iwork1)) == NULL )
	return RET_BUG;
      if ( nsp_matint_set_elts(ObjA, row, nr, rmin, rmax, ObjB) == FAIL )
	goto err;
    }
  else /* rhs = 4 */
    {
      if ( (row =get_index_vector(stack, 2, &nr, &rmin, &rmax, matint_iwork1)) == NULL )
	return RET_BUG;
      if ( (col =get_index_vector(stack, 3, &nc, &cmin, &cmax, matint_iwork2)) == NULL )
	goto err;
      if ( nsp_matint_set_submatrix(ObjA, row, nr, rmin, rmax, col, nc, cmin, cmax, ObjB) == FAIL )
	goto err;
    }

  if ( nr > WORK_SIZE ) FREE(row);
  if ( nc > WORK_SIZE ) FREE(col);
  ObjA->ret_pos = 1;
  return 1;
 err:
  if ( nr > WORK_SIZE ) FREE(row);
  if ( nc > WORK_SIZE ) FREE(col);
  return RET_BUG;
}

/**
 * int_matint_concatr:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
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
      /* ObjA has no name */ 
      if (((NspSMatrix *) ObjA)->mn == 0)
	{
	  /* return ObjB */
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
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
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
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
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
      if (((NspSMatrix *) ObjA)->mn == 0)
	{
	  /* return B */
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
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * @F: 
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
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for set cells elements C{exps}=(....) or C{exp,exp,..}=(...)
 * 
 * Return value: 
 **/

int int_matint_cells_setrowscols(Stack stack, int rhs, int opt, int lhs)
{
  int i, j, ind, nind;
  int nr=0, nc=0, *row=NULL, *col=NULL, rmin, rmax, cmin, cmax;
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
      if ( (row =get_index_vector(stack, 2, &nr, &rmin, &rmax, matint_iwork1)) == NULL )
	goto err;
      if ( nr != rhs - 3 )
	{
	  Scierror("Error: internal error, less rhs arguments %d than expected %d\n",
		   rhs- 3, nr);
	  goto err;
	}
      if ( rmin <= 0 )
	{
	  Scierror("Error:\tNon Positive indices are not allowed\n");
	  goto err;
	}
      if ( rmax > C->mn ) 
	{
	  /* we must enlarge C */
	  if ( C->m == 1 )
	    {
	      if ( nsp_cells_enlarge(C, 1, rmax) == FAIL ) goto err;
	    }
	  else if ( C->n == 1 )
	    {
	      if ( nsp_cells_enlarge(C, rmax, 1) == FAIL ) goto err;
	    } 
	  else  /* C is a matrix or a empty matrix */
	    {
	      if ( nsp_cells_enlarge(C, rmax, 1) == FAIL ) goto err;
	    }
	}

      for ( i = 0 ; i < nr ; i++)
	{
	  int cij = row[i];
	  NspObject *Ob = nsp_get_object(stack,i+3);
	  if ( Ocheckname(Ob,NVOID) ) 
	    {
	      if (nsp_object_set_name(Ob,"ce") == FAIL) goto err;
	    }
	  else 
	    {
	      if ((Ob =nsp_object_copy_and_name("ce",Ob))== NULLOBJ) goto err;
	    }
	  if ( C->objs[cij] != NULLOBJ) nsp_object_destroy(&C->objs[cij]);
	  C->objs[cij]= Ob;
	}
      break;

    case 2: 
      if ( (row =get_index_vector(stack, 2, &nr, &rmin, &rmax, matint_iwork1)) == NULL )
	goto err;
      if ( (col =get_index_vector(stack, 3, &nc, &cmin, &cmax, matint_iwork2)) == NULL )
	goto err;
      if ( nr*nc != rhs - 4 )
	{
	  Scierror("Error: internal error, less rhs arguments %d than expected %d\n",
		   rhs- 4, nr*nc);
	  goto err;
	}

      if ( rmax > C->m || cmax > C->n ) 
	if ( nsp_cells_enlarge(C, rmax, cmax) == FAIL ) goto err;

      ind = 4;
      for ( j = 0 ; j < nc ; j++)
	for ( i = 0 ; i < nr ; i++)
	  {
	    int cij= row[i] + C->m*col[j];
	    NspObject *Ob = nsp_get_object(stack,ind);
	    if ( Ocheckname(Ob,NVOID) ) 
	      {
		if (nsp_object_set_name(Ob,"ce") == FAIL) goto err;
	      }
	    else 
	      {
		if ((Ob =nsp_object_copy_and_name("ce",Ob))== NULLOBJ) goto err;
	      }
	    if ( C->objs[cij] != NULLOBJ) nsp_object_destroy(&C->objs[cij]);
	    C->objs[cij] = Ob;
	    ind++;
	  }
      break;

    default: 
      Scierror("Error: cells with more than 2 indices are not yet implemented\n");
      return RET_BUG;

    }
  NthObj(1)->ret_pos = 1;
  if ( nr > WORK_SIZE ) FREE(row);
  if ( nc > WORK_SIZE ) FREE(col);
  return 1;
  
 err:
  if ( nr > WORK_SIZE ) FREE(row);
  if ( nc > WORK_SIZE ) FREE(col);
  return RET_BUG;
}


/**
 * int_matint_redim:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * redim interface for objects which implements matint 
 * interface 
 * 
 * 
 * Return value: 
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
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * repmat interface for objects which implements matint interface 
 * 
 * 
 * Return value: 
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
  MAT_INT(type)->canonic(ObjA);  MAT_INT(type)->canonic(ObjB);

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
      else    /* one matrix is real and the other is complex */
	{
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
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for concat_diag which is to be used 
 * when calling concatdiag_x_x and x is supposed 
 * to implement the matint interface 
 * similar to concat_diag
 * 
 * Return value: 
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
 * @obj: 
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
