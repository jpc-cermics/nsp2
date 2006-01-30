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
#include <string.h>

#define  Matint_Private 
#include "nsp/object.h"
#include "nsp/matint.h" 
#include "nsp/interf.h"

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

static int int_matint_redim(NspMatrix *self,Stack stack,int rhs,int opt,int lhs) 
{
  NspTypeBase *type;
  int m1, n1;
  CheckRhs (2,2);
  CheckLhs (0,0);
  if (GetScalarInt (stack, 1, &m1) == FAIL)    return RET_BUG;
  if (GetScalarInt (stack, 2, &n1) == FAIL)    return RET_BUG;
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
  if ( MAT_INT(type)->redim(self,m1,n1) != OK) return RET_BUG;
  return 0;
}

static NspMethods matint_methods[] = {
  {"redim",(nsp_method *) int_matint_redim},
  { NULL, NULL}
};


NspMethods *matint_get_methods(void) { return matint_methods;};


/**
 * nsp_matint_delete_columns:
 * @Obj: a #NspObject which implements #matint
 * @Cols: a #NspMatrix giving column indices to be deleted.
 * 
 * delete the columns of object @Obj which must implements the #matint interface 
 * A(Rows,:) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_columns(NspObject  *Obj, NspMatrix *Cols)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val = (char *) A->S;
  unsigned int elt_size; /* size in number of bytes */
  NspTypeBase *type; 
  int i,j,*ind,k1,k2,nn,ncol,ioff=0;

  if ( Cols->mn == 0) return OK;

  if ( (ind = nsp_indices_for_deletions(A->n, Cols, &ncol)) == NULL ) 
    return FAIL;

  if (( type = check_implements(A,nsp_type_matint_id)) == NULL ) 
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
	{
	  memmove(Val +(k1-ioff)*A->m*elt_size,Val + (k1+1)*A->m*elt_size, nn*elt_size);
	}
      ioff++;
    }
  FREE(ind);
  if ( MAT_INT(type)->free_elt != NULL) 
    for ( i = A->mn - ncol*A->m ; i < A->mn ; i++ ) A->S[i]= NULL;
  if ( MAT_INT(type)->resize(A,A->m,A->n-ncol) == FAIL) return FAIL;
  return OK;
}

/**
 * nsp_matint_delete_rows:
 * @Obj: a #NspObject which implements #matint
 * @Rows: a #NspMatrix giving rows indices to be deleted.
 * 
 * delete the rows of object @Obj which must implements the #matint interface 
 * A(Rows,:) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_rows(NspObject *Obj, NspMatrix *Rows)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val = (char *) A->S;
  unsigned int elt_size; /* size in number of bytes */
  NspTypeBase *type; 
  int i,j,*ind,k1,k2,nn,nrow,stride=0,ioff=0;

  if ( Rows->mn == 0) return OK;

  if ( (ind = nsp_indices_for_deletions(A->m, Rows, &nrow)) == NULL ) 
    return FAIL;

  if (( type = check_implements(A,nsp_type_matint_id)) == NULL ) 
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
	    {
	      memmove(Val + (k1-ioff)*elt_size, Val + (k1+1)*elt_size, nn*elt_size);
	    }
	  ioff++;
	  k1 = k2;
	}
      stride += A->m;
    }
  FREE(ind);
  if ( MAT_INT(type)->free_elt != NULL) 
    for ( i = A->mn- nrow*A->n ; i < A->mn ; i++ ) A->S[i]= NULL;
  if ( MAT_INT(type)->resize(A,A->m-nrow,A->n) == FAIL) return FAIL;
  return OK;
}


/**
 * nsp_matint_delete_elements:
 * @Obj: a #NspObject which implements #matint
 * @Elts: a #NspMatrix giving element indices to be deleted.
 * 
 * delete the elements of object @Obj which must implements the #matint interface 
 * A(elts) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_elements(NspObject *Obj, NspMatrix *Elts)
{
  /*  modified by Bruno (same modifs than in nsp_matrix_delete_elements).
   *  The algorithm uses now the function nsp_complement_for_deletions). 
   *  Indices from Elts do not need to be in increasing order.
   */
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val = (char *) A->S;
  unsigned int elt_size; /* size in number of bytes */
  int i,*ind,k1,k2,nn,ne,ioff=0; 
  NspTypeBase *type; 

  if ( Elts->mn == 0) return OK;

  if ( (ind = nsp_indices_for_deletions(A->mn, Elts, &ne)) == NULL )  
    return FAIL; 

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
	{
	  memmove(Val + (k1-ioff)*elt_size, Val + (k1+1)*elt_size, nn*elt_size);
	}
      ioff++;
      k1 = k2;
    }
  FREE(ind);

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
 * @EltsR: a #NspMatrix giving rows indices to be deleted.
 * @EltsC: a #NspMatrix giving column indices to be deleted.
 * 
 * delete elements of object @Obj which must implements the #matint interface 
 * A(R,C) = []
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_matint_delete_elements2(NspObject *Obj, NspMatrix *EltsR, NspMatrix *EltsC)
{
  /* all objects which implements matint can be casted 
   * to NspSMatrix for accessing common fields.
   */
  NspSMatrix *A = (NspSMatrix *) Obj;
  char *Val = (char *) A->S;
  unsigned int elt_size; /* size in number of bytes */
  int i,j,ne,*indrow,*indcol,k1,k2,nn,nrow,ncol,ioff=0; 
  NspTypeBase *type; 

  if ( EltsR->mn == 0 || EltsC->mn == 0 ) return OK;

  if ( (indrow = nsp_indices_for_deletions(A->m, EltsR, &nrow)) == NULL )  
    return FAIL; 
  if ( (indcol = nsp_indices_for_deletions(A->n, EltsC, &ncol)) == NULL )  
    return FAIL; 

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
  FREE(indrow);
  FREE(indcol);
  ne = nrow*ncol;
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

