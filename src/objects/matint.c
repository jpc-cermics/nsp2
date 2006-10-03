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
  int mm,nn;
  CheckRhs (2,2);
  CheckLhs (0,0);
  if (GetScalarInt (stack, 1, &mm) == FAIL)    return RET_BUG;
  if (GetScalarInt (stack, 2, &nn) == FAIL)    return RET_BUG;

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

static NspMethods matint_methods[] = {
  {"redim",(nsp_method *) int_matint_meth_redim},
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
  else
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
     return MAT_INT(type)->clone(NVOID, Obj, 0, 0);  /* ici a adapter pour mtlb mode */

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
      if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, 1, nb_elts)) == NULLOBJ ) 
	return NULLOBJ;
    }
  else
    {
      if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, nb_elts, 1)) == NULLOBJ ) 
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
     return MAT_INT(type)->clone(NVOID, Obj, A->m, 0);

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

  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, A->m, nb_elts)) == NULLOBJ ) 
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
     return MAT_INT(type)->clone(NVOID, Obj, 0, A->n);

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

  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, nb_elts, A->n)) == NULLOBJ ) 
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
     return MAT_INT(type)->clone(NVOID, Obj, nr, nc);

  elt_size = MAT_INT(type)->elt_size(Obj); 

  if ( rmin < 1 || rmax > A->m || cmin < 1 || cmax > A->n )
    {
      Scierror("Error:\tIndices out of bound\n");
      return NULLOBJ;
    }

  if ( (Loc =MAT_INT(type)->clone(NVOID, Obj, nr, nc)) == NULLOBJ ) 
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
      Scierror("Error: A(...)= B, A and B must be of the same type\n");
      return FAIL;
    }

  if ( B->mn != 1)
    {
      if ( nr != B->m ||  nc != B->n )
	{
	  Scierror("Error:\tIncompatible dimensions\n");
	  return FAIL;
	}
    }

  if ( rmin < 1 || cmin < 1 ) 
    {
      Scierror("Error:\tNon Positive indices are not allowed\n");
      return FAIL;
    }

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

  if ( A->m == 1  &&  B->m != 1 ) 
    {
      Scierror("Error:\tA(ind)=B, B must be row when A is a row\n");
      return FAIL;
    } 

  if ( A->n == 1 && B->n != 1 )
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

  elt_size_A = MAT_INT(typeA)->elt_size(ObjA); 
  elt_size_B = MAT_INT(typeB)->elt_size(ObjB); 

  if ( imax > A->mn )  /* enlarge A */
    {
      if ( A->mn == 0) 
	{
	  if ( B->n != 1)
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
	      if ( B->n > 1 )
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

/**
 * nsp_matint_concat_right:
 * @ObjA: a #Matrix (that is a #NspObject which implements the matint interface)
 * @ObjB: 
 * 
 * 
 * Return value: returns [@ObjA,@ObjB] or %NULLOBJ 
 **/

NspObject *nsp_matint_concat_right(const NspObject *ObjA,const NspObject *ObjB)
{
  NspObject *ObjC=NULLOBJ;
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB, *C;
  int i, j;
  NspTypeBase *type; 
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  type = check_implements(ObjA, nsp_type_matint_id);  /* ObjA and ObjB must have the same type to send here 
                                                         (so we don't check) */

  elt_size_A = MAT_INT(type)->elt_size(ObjA);  /* but there is the problem real/complex */
  elt_size_B = MAT_INT(type)->elt_size(ObjB);  /* for Matrix and MaxpMatrix */

  if ( A->m == B->m )   
    {
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrices of numbers or booleans */
	{
	  if ( elt_size_A == elt_size_B )
	    {
	      if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA, A->m, A->n + B->n)) != NULLOBJ )
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
		  ObjC = MAT_INT(type)->clone(NVOID, ObjA, A->m, A->n + B->n);
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
		  ObjC = MAT_INT(type)->clone(NVOID, ObjB, A->m, A->n + B->n);
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
	  if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA, A->m, A->n + B->n)) != NULLOBJ )
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
    Scierror("Error:\tIncompatible dimensions\n");

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

int nsp_matint_concat_right_bis(NspObject *ObjA,const NspObject *ObjB)
{
  NspSMatrix *A = (NspSMatrix *) ObjA, *B = (NspSMatrix *) ObjB;
  int i, nA = A->n, nB = B->n, mnA = A->mn;
  NspTypeBase *type;
  unsigned int elt_size_A, elt_size_B; /* size in number of bytes */

  type = check_implements(ObjA, nsp_type_matint_id);  /* ObjA and ObjB must have the same type to send here
                                                         (so we don't check) */

  elt_size_A = MAT_INT(type)->elt_size(ObjA);  /* but there is the problem real/complex */
  elt_size_B = MAT_INT(type)->elt_size(ObjB);  /* for Matrix and MaxpMatrix */

  if ( A->m == B->m )
    {
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  /* Matrices of numbers or booleans */
	{
	  if ( elt_size_A == elt_size_B )
	    {
	      if ( MAT_INT(type)->enlarge(ObjA, A->m, nA + nB) == FAIL ) return FAIL;
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
		  if ( nsp_matrix_resize(AA, A->m, nA + nB) == FAIL ) return FAIL;
		  for ( i = 0 ; i < BB->mn ; i++ )
		    {
		      AA->C[mnA+i].r = BB->R[i]; AA->C[mnA+i].i = 0.0;
		    }
		}
	      else
		{                             /* A is real, B complex */
		  if ( nsp_matrix_resize(AA, A->m, nA + nB) == FAIL ) return FAIL;
		  if ( nsp_mat_complexify(AA, 0.00) == FAIL ) return FAIL;
		  memcpy( &(AA->C[mnA]), BB->C, elt_size_B*B->mn);
		}
	    }
	}
      else                                                      /* Matrices of pointers (String, cells, poly,...) */
	{
	  if ( MAT_INT(type)->enlarge(ObjA, A->m, nA + nB) == FAIL ) return FAIL;
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
  else if ( A->m == 0 && A->n == 0 )
    {
      nsp_object_destroy(&ObjA);
      if ( (ObjA =nsp_object_copy(ObjB)) == NULLOBJ ) return FAIL;
    }
  else if ( !(B->m == 0 && B->n == 0) )
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return FAIL; 
    }

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

  elt_size_A = MAT_INT(type)->elt_size(ObjA);  /* but there is the problem real/complex */
  elt_size_B = MAT_INT(type)->elt_size(ObjB);  /* for Matrix and MaxpMatrix */

  if ( A->n == B->n ) 
    {
      /* Matrices of numbers or booleans */
      if ( MAT_INT(type)->free_elt == (matint_free_elt *) 0 )  
	{
	  if ( elt_size_A == elt_size_B )
	    {
 	      if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n)) != NULLOBJ )
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
		  ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n);
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
		  ObjC = MAT_INT(type)->clone(NVOID, ObjB,A->m+B->m,A->n);
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
	  if ( (ObjC = MAT_INT(type)->clone(NVOID, ObjA,A->m+B->m,A->n)) != NULLOBJ )
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
    Scierror("Error:\tIncompatible dimensions\n");
  return ObjC;
 err:
  nsp_object_destroy(&ObjC); 
  return NULLOBJ;
}


/**
 * nsp_matint_redim:
 * @A: a #NspMatrix
 * @m: number of rows 
 * @n: number of columns
 * 
 * Checks that the #NspObject @A (which is supposed to implement matint)
 * of size m' x n' satisfy m'*n' = @m * @n and reshapes 
 * @A to size @m x @n.
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


/* interface functions which are called by other interfaces 
 * or called in Eval.c 
 */

/* various deletions functions */

int nsp_matint_tozero_xx(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;

  Obj = NthObj(1);
  nsp_matint_tozero(Obj);
  Obj->ret_pos = 1;
  return 1;
}

typedef int (*delfunc) (NspObject *Obj, int *ind, int nb_elts, int imin, int imax);

static int nsp_matint_delete_gen_xx(Stack stack, int rhs, int opt, int lhs, delfunc F)
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

int nsp_matint_deleteelts_xx(Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs == 3 )
    return nsp_matint_deleteelts2_xx(stack, rhs, opt, lhs);
  else
    return nsp_matint_delete_gen_xx(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_elements);
}

int nsp_matint_deletecols_xx(Stack stack, int rhs, int opt, int lhs)
{
  return nsp_matint_delete_gen_xx(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_columns);
}

int nsp_matint_deleterows_xx(Stack stack, int rhs, int opt, int lhs)
{
  return nsp_matint_delete_gen_xx(stack, rhs, opt, lhs, (delfunc) nsp_matint_delete_rows);
}


int nsp_matint_deleteelts2_xx(Stack stack, int rhs, int opt, int lhs)
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

int nsp_matint_resize2vect_xx(Stack stack, int rhs, int opt, int lhs)
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

static int nsp_matint_extract_gen_xx(Stack stack, int rhs, int opt, int lhs, extractfunc F)
{
  NspObject *Obj, *Res;
  int nb_elts, *ind=NULL, imin, imax;

  CheckLhs (1, 1);
  Obj = NthObj(1);

  if ( (ind =get_index_vector(stack, 2, &nb_elts, &imin, &imax, matint_iwork1)) == NULL )
    return RET_BUG;
    
  if ( (Res = (*F)(Obj, ind, nb_elts, imin, imax)) == NULLOBJ )
    goto err;

  if ( nb_elts > WORK_SIZE ) FREE(ind);
  MoveObj (stack, 1, Res);
  return 1;

 err:
  if ( nb_elts > WORK_SIZE ) FREE(ind);
  return RET_BUG;
}

int nsp_matint_extractelts_xx(Stack stack, int rhs, int opt, int lhs)
{
  return nsp_matint_extract_gen_xx(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_elements);
}

int nsp_matint_extractcols_xx(Stack stack, int rhs, int opt, int lhs)
{
  return nsp_matint_extract_gen_xx(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_columns);
}

int nsp_matint_extractrows_xx(Stack stack, int rhs, int opt, int lhs)
{
  return nsp_matint_extract_gen_xx(stack, rhs, opt, lhs, (extractfunc) nsp_matint_extract_rows);
}


int nsp_matint_extract_xx(Stack stack, int rhs, int opt, int lhs)
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

int nsp_matint_setrowscols_xx(Stack stack, int rhs, int opt, int lhs)
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

int nsp_matint_concatr_xx(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *ObjA, *ObjB, *Res;

  CheckRhs (2, 2);
  CheckLhs (1, 1);
  ObjA = NthObj(1);  ObjB = NthObj(2);

  if ( Ocheckname(ObjA, NVOID) )   /* ObjA has no name */ 
    {
      int m=nsp_object_get_size(ObjA,1);
      int n=nsp_object_get_size(ObjA,2);
      /* XXX quick and dirty hack to correct 
       * nsp_matint_concat_right_bis which is wrong when 
       * ObjA == []
       * ObjA should be transmited by adress to 
       *  nsp_matint_concat_right_bis.
       */
      if ( m  == 0 && n == 0 )
	{
	  if ( (ObjA =nsp_object_copy(ObjB)) == NULLOBJ ) return FAIL;
	  /* this will clean the previously stored ObjA */
	  MoveObj (stack, 1, ObjA);
	}
      else 
	{
	  if ( nsp_matint_concat_right_bis(ObjA, ObjB) == FAIL )
	    return RET_BUG;
	  ObjA->ret_pos = 1;
	}
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
 * nsp_matint_concat_down_xx:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * generix interface for [A;B]
 * 
 * Return value: 1 or %RET_BUG.
 **/

int nsp_matint_concatd_xx(Stack stack, int rhs, int opt, int lhs)
{
  /* note that pointers have already been changed thus we can use NthObj */
  NspObject *ObjA= NthObj(1), *ObjB= NthObj(2), *Res;
  CheckRhs (2, 2);
  CheckLhs (1, 1);
  if ( (Res =nsp_matint_concat_down(ObjA, ObjB)) == NULLOBJ )
    return RET_BUG;
  MoveObj (stack, 1, Res);
  return 1;
}

/* an other interface for concatd 
 * This one is used when concatd_x_x is first searched 
 * when x is supposed to implement the matint interface 
 * 
 *
 */ 

int int_matint_concat_down_yy(Stack stack, int rhs, int opt, int lhs, Fconcat_d F)
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

/*
 * set cells elements 
 * C{exps}=(....)
 * or C{exp,exp,..}=(...)
 */

int nsp_matint_cells_setrowscols_xx(Stack stack, int rhs, int opt, int lhs)
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
	  if ( C->n == 1 )
	    {
	      if ( nsp_cells_enlarge(C, rmax, 1) == FAIL ) goto err;
	    }
	  else if ( C->m == 1 )
	    {
	      if ( nsp_cells_enlarge(C, 1, rmax) == FAIL ) goto err;
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




/*
 * redim interface for objects which implements matint 
 * interface 
 **/

int int_matint_redim(Stack stack, int rhs, int opt, int lhs) 
{
  NspTypeBase *type;
  int mm,nn;
  CheckRhs (2,3);
  CheckLhs (0,1);
  NspObject *Obj;

  if ((Obj = nsp_get_object_copy(stack,1))== NULL) return RET_BUG;
  if (GetScalarInt (stack, 2, &mm) == FAIL)    return RET_BUG;
  if (rhs == 3) 
    {
      if (GetScalarInt (stack, 3, &nn) == FAIL)    return RET_BUG;
    }
  else 
    {
      nn=-1;
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

