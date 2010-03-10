/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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

#include "nsp/math.h"
#include "nsp/object.h" 

/*
 * Allocation or Reallocation 
 */

/**
 *nsp_alloc_doubles:
 * @n: number of doubles to allocate 
 *
 * malloc an array of @n doubles.
 *
 * Return value: a pointer to the allocated zone. 
 */

double *nsp_alloc_doubles(unsigned int n)
{
  if ( n <= 0 ) 
    return((double *) 0);
  else
    {
      if ( n <= 10 ) n = 10;  /* XXXXXX just to accelerate the realloc */
      return((double*) MALLOC(n*sizeof(double)));
    }
}

/**
 * nsp_realloc_doubles:
 * @dp: %NULL or a pointer to a former allocated zone?
 * @n: number of doubles to allocate 
 *
 * realloc or malloc (if @dp is %NULL) an array of @n doubles.
 * Return value: a pointer to the allocated zone. 
 */

double *nsp_realloc_doubles(double *dp,unsigned int n)
{
  if ( n <= 0 )
    {
      FREE(dp);
      return((double *) 0);
    }
  else 
    {
      if ( dp == (double * ) 0) 
	return((double*) MALLOC(n*sizeof(double)));
      else 
	return((double *) REALLOC(dp,n*sizeof(double)));
    }
}


/**
 * nsp_alloc_int: 
 * @n: number of int to allocate 
 *
 * malloc an array of @n int 
 *
 * Return value: a pointer to the allocated zone. 
 */


int *nsp_alloc_int(unsigned int  n)
{
  if ( n <= 0 ) 
    return NULL;
  else
    return((int *) MALLOC(n*sizeof(int)));
}


/**
 * nsp_realloc_int:
 * @dp: %NULL or a pointer to a former allocated zone?
 * @n: number of int to allocate 
 *
 * realloc or malloc (if @dp is %NULL) an array of @n integers.
 *
 * Return value: a pointer to the allocated zone. 
 */


int *nsp_realloc_int(int *dp, unsigned int n)
{
  if ( n <= 0 ) 
    {
      FREE(dp);
      return((int *) 0);
    }
  else
    {
      if ( dp == (int * ) 0) 
	return((int*) MALLOC(n*sizeof(int)));
      else
	return((int *) REALLOC(dp,n*sizeof(int)));
    }
}


/**
 *nsp_alloc_doubleC:
 * @n: number of complex to allocate 
 *
 * malloc an array of @n complex (#doubleC).
 *
 * Return value: a pointer to the allocated zone. 
 */

doubleC *nsp_alloc_doubleC( unsigned int n)
{
  if ( n <= 0 ) 
    return((doubleC *) 0);
  else
    return((doubleC*) MALLOC(n*sizeof(doubleC)));
}

/**
 *nsp_realloc_doubleC:
 * @dp: %NULL or a pointer to a former allocated zone?
 * @n: number of complex to allocate 
 *
 * realloc or malloc (if @dp is %NULL) an array of @n #doubleC.
 *
 * Return value: a pointer to the allocated zone. 
 */

doubleC *nsp_realloc_doubleC(doubleC *dp, unsigned int  n)
{
  if ( n <= 0 )
    {
      FREE(dp);
      return((doubleC *) 0);
    }
  else 
    {
      if ( dp == (doubleC * ) 0) 
	return((doubleC*) MALLOC(n*sizeof(doubleC)));
      else 
	return((doubleC *) REALLOC(dp,n*sizeof(doubleC)));
    }
}

/**
 * nsp_alloc_work_doubles:
 * @n: nb of elements of the double work array 
 * 
 * similar to #nsp_alloc_doubles but sends an error 
 * message in case of allocation failure.
 * 
 * Return value: the work array or %NULL
 **/

double *nsp_alloc_work_doubles(unsigned int n)
{
  double *p = MALLOC(n*sizeof(double));
  if ( p == NULL ) Scierror("Error:\tRunning out of memory\n");
  return p;
}

/**
 * nsp_alloc_work_doubleC:
 * @n:  nb of elements of the doubleC work array
 * 
 * similar to #nsp_alloc_doubleC but sends
 * an error message in case of alloction failure.
 * 
 * Return value: the work array or %NULL
 **/

doubleC *nsp_alloc_work_doubleC(unsigned int n)
{
  doubleC *p= MALLOC(n*sizeof(doubleC));
  if ( p == NULL ) Scierror("Error:\tRunning out of memory\n");
  return p;
}

/**
 * nsp_alloc_work_int:
 * @n:  nb of elements of the int work array
 * 
 * similar to #nsp_alloc_doubleC but sends 
 * an error message in case of alloc failure.
 * 
 * Return value: the work array or %NULL
 **/

int *nsp_alloc_work_int(unsigned int n)
{
  int *p = MALLOC(n*sizeof(int));
  if ( p == NULL ) Scierror("Error:\tRunning out of memory\n");
  return p;
}

   
/*  
 * nsp_alloc_index:
 * @m:  nb of rows
 * @n: nb of columns
 *
 *  this routine allocates an NspMatrix or an NspIMatrix to store indices
 *  depending upon the ind_type parameter (currently 'd' or 'i'). It sets 
 *  also the int pointer index. 
 */
NspObject *nsp_alloc_mat_or_imat(int m, int n, char ind_type, int **index)
{
  if ( ind_type == 'd' )
    {
      NspMatrix *RIndex = NULLMAT;
      if ( (RIndex = nsp_matrix_create(NVOID,'r',m,n) ) == NULLMAT ) 
	return NULLOBJ;
      RIndex->convert='i';
      *index = RIndex->I;
      return (NspObject *) RIndex; 
    }
  else  /* assume ind_type == 'i' */
    {
      NspIMatrix *IIndex = NULLIMAT;
      if ( (IIndex = nsp_imatrix_create(NVOID,m,n,nsp_gint32) ) == NULLIMAT ) 
	return NULLOBJ;
      *index = IIndex->Gint;
      return (NspObject *) IIndex; 
    }
}
