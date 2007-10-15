/* Nsp
 * Copyright (C) 2006 Bruno Pincon Esial/Iecn
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

#include "grand.h"
#include <math.h>
#include <stdlib.h>
#include <strings.h>
#include <float.h>

#define SMALL_STACK_NOT_EMPTY is > -1 
#define BIG_STACK_NOT_EMPTY ib < n 
#define PUSH_SMALL_STACK(k) stack[++is]=k
#define POP_SMALL_STACK() stack[is--]
#define PUSH_BIG_STACK(k) stack[--ib]=k
#define POP_BIG_STACK() stack[ib++]

/**
 * nsp_alias_method:
 * @p: (input) array of doubles of size n (p[i] probability of category i)
 * @q: (output) array of doubles of size n (must be allocated)
 * @j: (output) array of int of size n (must be allocated)
 * @n: (input) size of @p, @q and @j
 * 
 * initialize the arrays q and j for random generation of a discrete
 * distribution with #nsp_rand_discrete_alias (sum of the
 * probability vector components must sum up to 1 (up to roundoff 
 * errors)
 *
 * method: Alias method see chapter III, section 4 (p 107) of 
 * Luc Devroye 's book, "Non-Uniform Random Variate Generation".  
 * Springer-Verlag, New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_alias_method(double *p, double *q, int *j, int n)
{
  int k, l, is, ib, *stack;
  double sum_p = 0.0, tol = n*DBL_EPSILON;

  /* verify probabilities p[i], computes q */
  for ( k = 0 ; k < n ; k++ )
    {
      if ( ! (p[k] >= 0) )
	return FAIL;
      sum_p += p[k];
      q[k] = n * p[k];
    }

  /* probabilities must sum up to 1 */
  if ( ! (1.0-tol <= sum_p  &&   sum_p <= 1.0+tol) )
    return FAIL;

  /* init stacks */
  if ( (stack = malloc(n*sizeof(int))) == NULL )
    return FAIL;
  is = -1; ib = n;

  for ( k = 0 ; k < n ; k++ )
    {
      if ( q[k] < 1.0 )
	PUSH_SMALL_STACK(k);
      else
	PUSH_BIG_STACK(k);
    }

  while ( SMALL_STACK_NOT_EMPTY )
    {
      l = POP_SMALL_STACK();

      if ( BIG_STACK_NOT_EMPTY ) /* normal case */
	{
	  k = POP_BIG_STACK();
	  j[l] = k;
	  q[k] -= (1.0 - q[l]);
	  if ( q[k] < 1 )
	    PUSH_SMALL_STACK(k);
	  else
	    PUSH_BIG_STACK(k);
	}
      else                       /* problem certainly due to roundoff errors : */
	{                        /* as the probability vector seems good we could complete by 1 in every case */
	  q[l] = 1.0;
/* 	  if ( q[l] >= 1 - 100*tol )  */
/* 	    q[l] = 1.0; */
/* 	  else */
/* 	    { */
/* 	      free(stack);  */
/* 	      Sciprintf("big stack empty while small q is %18.16f\n",q[l]);  */
/* 	      Sciprintf("stack : is = %d, ib = %d\n",is, ib);  */
/* 	      return FAIL; */
/* 	    } */
	}
    }

  free(stack);

  return OK;
}

/**
 * nsp_rand_discrete_alias:
 * @q: (input) array of doubles of size n
 * @j: (intput) array of int of size n
 * @n: (input) size of @q and @j
 * 
 * random generation of an integer between 0 and n-1
 * from a discrete distribution. Arrays @q and @j must
 * initialized with #nsp_alias_method.
 *
 * method: see #nsp_alias_method
 *
 * Returns an int
 *
 **/
int nsp_rand_discrete_alias(double *q, int *j, int n)
{
  double V;
  int X;

  V = n*rand_ranf();
  X = V;
  V -= X;

  if ( V <= q[X] )
    return X;
  else
    return j[X];
}


/**
 * nsp_guide_table_method
 * @p: (input) array of doubles of size n (p[i*inc] probability of category i)
 * @inc: (input) integer increment between 2 components of p (useful for markov)
 * @q: (output) array of doubles of size n+1 (must be allocated)
 * @key: (output) array of int of size n (must be allocated)
 * @n: (input) size of @p
 * 
 * initialize the arrays q and key for random generation of a discrete
 * distribution with #nsp_rand_discrete_guide (sum of the
 * probability vector components must sum up to 1 (up to roundoff 
 * errors)
 *
 * method: table guide method see chapter III, section 2.4 (p 96) of 
 * Luc Devroye 's book, "Non-Uniform Random Variate Generation".  
 * Springer-Verlag, New York, 1986.
 * (available at the Luc Devroye 's home page :
 * http://cg.scs.carleton.ca/~luc/rnbookindex.html)
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_guide_table_method(double *p, int inc, double *q, int *key, int n)
{
  int i, j, k;
  double tol = n*DBL_EPSILON, lim;

  /* compute cumulative probabilities q[i] with some verifications */
  q[0] = 0.0;
  for ( k = 0, j = 0 ; k < n ; k++, j+=inc )
    {
      if ( ! (p[j] >= 0) )
	return FAIL;
      q[k+1] = q[k] + p[j];
    }

  /* probabilities must sum up to 1 */
  if ( ! (1.0-tol <= q[n]  &&  q[n] <= 1.0+tol) )
    return FAIL;

  q[n] = 1.0;
  k = n-1;
  while( q[k] > 1.0 )
    {
      q[k] = 1.0; k--;
    }

  /* computes guide keys for fast generation  */
  /*      keys[i] =   max j                   */
  /*                q[j] < (i+1)/n            */
  j = 0;
  for ( i = 0 ; i < n-1 ; i++ )
    {
      lim = (i+1.0) / n;
      while ( q[j] < lim ) j++;
      key[i] = j-1;
    }
  key[n-1] = n-1;

  return OK;
}

/**
 * nsp_guide_table_method_bis
 * @p: (input) array of doubles of size n-1 (p[i] probability of category i)
 * @q: (output) array of doubles of size n+1 (must be allocated)
 * @key: (output) array of int of size n (must be allocated)
 * @n: (input) size of @p
 * 
 * see #nsp_guide_table_method. This version "bis" is taylored for a 
 * probability vector with n-1 components, the last one being supposed 
 * to be 1- sum_k p_k
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_guide_table_method_bis(double *p, double *q, int *key, int n)
{
  int i, j, k;
  double  lim;
  /* double tol = n*DBL_EPSILON*/

  /* compute cumulative probabilities q[i] with some verifications */
  q[0] = 0.0;
  for ( k = 0 ; k < n-1 ; k++)
    {
      if ( ! (p[k] >= 0) )
	return FAIL;
      q[k+1] = q[k] + p[k];
    }

  /* probabilities must sum up to 1 */
  if ( ! (q[n-1] < 1.0) )
    return FAIL;

  q[n] = 1.0;

  /* computes guide keys for fast generation  */
  /*      keys[i] =   max j                   */
  /*                q[j] < (i+1)/n            */
  j = 0;
  for ( i = 0 ; i < n-1 ; i++ )
    {
      lim = (i+1.0) / n;
      while ( q[j] < lim ) j++;
      key[i] = j-1;
    }
  key[n-1] = n-1;

  return OK;
}
  

/**
 * nsp_rand_discrete_guide
 * @q: (input) array of doubles of size n+1
 * @key: (intput) array of int of size n
 * @n: (input) size of @key 
 * 
 * random generation of an integer between 0 and n-1
 * from a discrete distribution. Arrays @q and @key must
 * initialized with #nsp_guide_table_method.or with #nsp_guide_table_method_bis
 *
 * method: see #nsp_guide_table_method
 *
 * Returns %OK or %FAIL
 *
 **/
int nsp_rand_discrete_guide(double *q, int *key, int n)
{
  double u = rand_ranf();
  int X = n*u;
  X = key[X];
  while ( u < q[X] ) 
    X--;

  return X;
}

int nsp_rand_discrete(double *p, double *q, double *Res, int *key, int n, int mn)
{
  int i;

  if ( nsp_guide_table_method(p, 1, q, key, n) == FAIL ) 
    return FAIL;
  
  for ( i = 0 ; i < mn ; i++ )
    Res[i] = 1.0 + (double) nsp_rand_discrete_guide(q, key, n);

  return OK;
}



	 

	 
