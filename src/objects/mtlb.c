/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
 *                    2007 Bruno Pincon Esial/Iecn (changes in GetDimArg func) 
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
 *
 * A set of interfaced function to emulate Matlab frequently used 
 * functions. 
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/matint.h"


/* mtlb issparse
 * 
 */

static int int_nsp_issparse(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,1);
  CheckLhs(0,1);
  NspObject *Ob, *ret=NULL;
  if ((Ob =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ((ret = nsp_create_boolean_object(NVOID, IsSpColMat(Ob))) == NULLOBJ) return RET_BUG; 
  MoveObj(stack,1,ret);
  return 1;
}  

/* mtlb isstruct 
 */

static int int_nsp_isstruct(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,1);
  CheckLhs(0,1);
  NspObject *Ob, *ret=NULL;
  if ((Ob =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ((ret = nsp_create_boolean_object(NVOID, IsHash(Ob))) == NULLOBJ) return RET_BUG; 
  MoveObj(stack,1,ret);
  return 1;
}  

/* mtlb islogical
 *
 */

static int int_nsp_islogical(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,1);
  CheckLhs(0,1);
  NspObject *Ob, *ret=NULL;
  if ((Ob =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ((ret = nsp_create_boolean_object(NVOID, IsBMat(Ob))) == NULLOBJ) return RET_BUG; 
  MoveObj(stack,1,ret);
  return 1;
}  

/* mtlb isfield 
 *
 */

static int int_nsp_isfield(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  char *key;
  NspObject *ret,*Ob;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((H = GetHash(stack,1)) == NULLHASH) return RET_BUG;
  if ((key = GetString(stack,2)) == (char*)0) return RET_BUG;  
  if ((ret = nsp_create_boolean_object(NVOID,nsp_hash_find(H,key,&Ob) == OK)) == NULLOBJ) 
    return RET_BUG; 
  MoveObj(stack,1,ret);
  return 1;
}  

/* mtlb any(Mat [,dim])
 *
 */

static int int_nsp_any_m(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *ret;
  NspMatrix *M;
  int dim=1,i,j;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((M = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ( GetDimArg(stack, 2, &dim) == FAIL ) return RET_BUG;
      if ( dim == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(M);
    }

  if ( rhs == 1 ) /* here in case dim is not given we use the matlab convention (ie like for dim = -2 or '.') */
    dim = (M->m == 1 || M->n == 1) ? 0 : 1;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0: 
      if ((ret =(NspBMatrix *) nsp_bmatrix_create(NVOID,1,1)) == NULL) return RET_BUG;
      ret->B[0]= FALSE;
      for ( i= 0 ; i < M->mn; i++)
	if ( ( M->rc_type == 'r' &&  M->R[i] != 0.0 ) 
	     || ( M->rc_type == 'c' &&  (M->C[i].r != 0.0 && M->C[i].i != 0.0))) 
	  {
	    ret->B[0] = TRUE; break;
	  }
      break;
  
    case 1:
      if ((ret =(NspBMatrix *) nsp_bmatrix_create(NVOID,1,M->n)) == NULL) return RET_BUG;
      for ( j= 0 ; j < M->n ; j++)
	{
	  ret->B[j]= FALSE;
	  for ( i= 0 ; i < M->m; i++)
	    if ( ( M->rc_type == 'r' &&  M->R[i+M->m*j] != 0.0 ) 
		 || ( M->rc_type == 'c' &&  (M->C[i+M->m*j].r != 0.0 && M->C[i+M->m*j].i != 0.0))) 
	      {
		ret->B[j] = TRUE; break;
	      }
	}
      break;
 
    case 2:
      if ((ret =(NspBMatrix *)  nsp_bmatrix_create(NVOID,M->m,1)) == NULL) return RET_BUG;
      for ( i = 0 ; i < M->m ; i++)
	{
	  ret->B[i]= FALSE;
	  for ( j = 0 ; j < M->n ; j++)
	    if ( ( M->rc_type == 'r' &&  M->R[i+M->m*j] != 0.0 ) 
		 || ( M->rc_type == 'c' &&  (M->C[i+M->m*j].r != 0.0 && M->C[i+M->m*j].i != 0.0))) 
	      {
		ret->B[i] = TRUE; break;
	      }
	}
      break;
    }

  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

/* mtlb all(Mat [,dim])
 *
 */

static int int_nsp_all_m(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *ret;
  NspMatrix *M;
  int dim=1,i,j;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((M = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ( GetDimArg(stack, 2, &dim) == FAIL ) return RET_BUG;
      if ( dim == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(M);
    }

  if ( rhs == 1 ) /* here in case dim is not given we use the matlab convention (ie like for dim = -2 or '.') */
    dim = (M->m == 1 || M->n == 1) ? 0 : 1;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0:
      if ((ret =(NspBMatrix *) nsp_bmatrix_create(NVOID,1,1)) == NULL) return RET_BUG;
      ret->B[0]= TRUE;
      for ( i= 0 ; i < M->mn; i++)
	if ( ( M->rc_type == 'r' &&  M->R[i] == 0.0 ) 
	     || ( M->rc_type == 'c' &&  (M->C[i].r == 0.0 && M->C[i].i == 0.0))) 
	  {
	    ret->B[0] = FALSE;break;
	  }
      break;

    case 1:
      if ((ret =(NspBMatrix *) nsp_bmatrix_create(NVOID,1,M->n)) == NULL) return RET_BUG;
      for ( j= 0 ; j < M->n ; j++)
	{
	  ret->B[j]= TRUE;
	  for ( i= 0 ; i < M->m; i++)
	    if ( ( M->rc_type == 'r' &&  M->R[i+M->m*j] == 0.0 ) 
		 || ( M->rc_type == 'c' &&  (M->C[i+M->m*j].r == 0.0 && M->C[i+M->m*j].i == 0.0))) 
	      {
		ret->B[j] = FALSE;break;
	      }
	}
      break;

    case 2:
      if ((ret =(NspBMatrix *)  nsp_bmatrix_create(NVOID,M->m,1)) == NULL) return RET_BUG;
      for ( i = 0 ; i < M->m ; i++)
	{
	  ret->B[i]= TRUE;
	  for ( j = 0 ; j < M->n ; j++)
	    if ( ( M->rc_type == 'r' &&  M->R[i+M->m*j] == 0.0 ) 
		 || ( M->rc_type == 'c' &&  (M->C[i+M->m*j].r == 0.0 && M->C[i+M->m*j].i == 0.0))) 
	      {
		ret->B[i] = FALSE;break;
	      }
	}
      break;
    }

  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}


/* mtlb any(BMat)
 *
 */


static int int_nsp_any_b(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *ret;
  NspBMatrix *M;
  int dim=1,i,j;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((M = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ( GetDimArg(stack, 2, &dim) == FAIL ) return RET_BUG;
      if ( dim == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(M);
    }

  if ( rhs == 1 ) /* here in case dim is not given we use the matlab convention (ie like for dim = -2 or '.') */
    dim = (M->m == 1 || M->n == 1) ? 0 : 1;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0:
      if ((ret =(NspBMatrix *) nsp_bmatrix_create(NVOID,1,1)) == NULL) return RET_BUG;
      ret->B[0]= FALSE;
      for ( i= 0 ; i < M->mn; i++)
	if ( M->B[i] == TRUE ) 
	  {
	    ret->B[0] = TRUE;break;
	  }
      break;

    case 1:
      if ((ret =(NspBMatrix *) nsp_bmatrix_create(NVOID,1,M->n)) == NULL) return RET_BUG;
      for ( j= 0 ; j < M->n ; j++)
	{
	  ret->B[j]= FALSE;
	  for ( i= 0 ; i < M->m; i++)
	    if ( M->B[i+M->m*j] == TRUE ) 
	      {
		ret->B[j] = TRUE;break;
	      }
	}
      break;

    case 2:
      if ((ret =(NspBMatrix *)  nsp_bmatrix_create(NVOID,M->m,1)) == NULL) return RET_BUG;
      for ( i = 0 ; i < M->m ; i++)
	{
	  ret->B[i]= FALSE;
	  for ( j = 0 ; j < M->n ; j++)
	    if ( M->B[i+M->m*j] == TRUE ) 
	      {
		ret->B[i] = TRUE;break;
	      }
	}
      break;
    }

  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

/* mtlb all(BMat)
 *
 */

static int int_nsp_all_b(Stack stack, int rhs, int opt, int lhs)
{
  NspBMatrix *ret;
  NspBMatrix *M;
  int dim=1,i,j;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((M = GetBMat(stack,1)) == NULLBMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ( GetDimArg(stack, 2, &dim) == FAIL ) return RET_BUG;
      if ( dim == -1 )
	{
	  Scierror ("Error:\t dim flag equal to -1 or '.' not supported for function %s\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( dim == -2 )  /* matlab compatibility flag */
	dim = GiveMatlabDimFlag(M);
    }

  if ( rhs == 1 ) /* here in case dim is not given we use the matlab convention (ie like for dim = -2 or '.') */
    dim = (M->m == 1 || M->n == 1) ? 0 : 1;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0:
      if ((ret =(NspBMatrix *) nsp_bmatrix_create(NVOID,1,1)) == NULL) return RET_BUG;
      ret->B[0]= TRUE;
      for ( i= 0 ; i < M->mn; i++)
	if ( M->B[i] == FALSE ) 
	  {
	    ret->B[0] = FALSE;break;
	  }
      break;

    case 1:
      if ((ret =(NspBMatrix *) nsp_bmatrix_create(NVOID,1,M->n)) == NULL) return RET_BUG;
      for ( j= 0 ; j < M->n ; j++)
	{
	  ret->B[j]= TRUE;
	  for ( i= 0 ; i < M->m; i++)
	    if ( M->B[i+M->m*j] == FALSE ) 
	      {
		ret->B[j] = FALSE;break;
	      }
	}
    break;

    case 2:
      if ((ret =(NspBMatrix *)  nsp_bmatrix_create(NVOID,M->m,1)) == NULL) return RET_BUG;
      for ( i = 0 ; i < M->m ; i++)
	{
	  ret->B[i]= TRUE;
	  for ( j = 0 ; j < M->n ; j++)
	    if ( M->B[i+M->m*j] == FALSE ) 
	      {
		ret->B[i] = FALSE;break;
	      }
	}
      break;
    }

  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}


/*
 * The Interface for basic Matlab operations 
 */

static OpTab Mtlb_func[]={
  {"issparse" ,  int_nsp_issparse },
  {"isstruct" ,  int_nsp_isstruct },
  {"islogical",  int_nsp_islogical},
  {"isfield",    int_nsp_isfield},
  {"any_m",      int_nsp_any_m},
  {"all_m",      int_nsp_all_m},
  {"any_b",      int_nsp_any_b},
  {"all_b",      int_nsp_all_b},
  {(char *) 0, NULL}
};

int Mtlb_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Mtlb_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Mtlb_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Mtlb_func[i].name;
  *f = Mtlb_func[i].fonc;
}

