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

int nsp_guide_table_method_bis(double *p, double *q, int *key, int n)
{
  /* this version is taylored for a probability vector with n-1 
   * components, the last one being supposed to be 1- sum_k p_k
   */
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
  

int nsp_verify_probability_vector(double *p, int n)
{
  /* p is a probability vector for a discrete distribution 
   * with n events ; only the probability of the n-1 first events
   * is given, the last one being supposed to be 1- sum_k p_k.
   * The code verify that all probability are positive or null.
   */
  int k;
  double q;

  q = 0.0;
  for ( k = 0 ; k < n-1 ; k++ )
    {
      if ( ! (p[k] >= 0.0) )
	return FAIL;
      q += p[k];
    }

  /* probabilities must sum up to 1 */
  if ( ! (q <= 1.0) )
    return FAIL;

  return OK;
}

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

/* multinomial distribution for n not too big compared to ncat */
void nsp_rand_multinomial1(double *q, int *key, int *ix, int ncat, int n)
{
  int i;
  for ( i = 0 ; i < ncat ; i++ )
    ix[i] = 0;

  for ( i = 0 ; i < n ; i++ )
    ix[nsp_rand_discrete_guide(q, key, ncat)]++;
}

/* multinomial distribution for n enough bigger than ncat */
void nsp_rand_multinomial2(double *p, int *ix, int ncat, int n)
{
  int i, j;
  double ptot = 1;
  for ( i = 0 ; i < ncat-1 ; i++ )
    {
      ix[i] = nsp_rand_binomial_direct(n,p[i]/ptot);
      n -= ix[i];
      if ( n == 0 )
	{
	  for ( j = i+1 ; j <= ncat-1 ; j++ ) ix[j] = 0;
	  return;
	}
      ptot -= p[i];
    }
  ix[ncat-1] = n;
}



int nsp_verif_markov_initial_state(double *X0, int mnX0, int n)
{
  int i, j;
  for ( i = 0 ; i < mnX0 ; i++ )
    {
      j = (int) X0[i];
      if ( j < 1 || j > n )
	{
	  Scierror("Error: X0(%d) must be in the range [1,%d]\n", i+1, n);
	  return FAIL;
	}
    }
  return OK;
}

int nsp_markov_setup(double *p, double *q, int *key, int n)
{
  int i;
  for ( i = 0 ; i < n ; i++ )
    if ( nsp_guide_table_method(&(p[i]), n, &(q[(n+1)*i]), &(key[n*i]), n) == FAIL )
      return FAIL;
  return OK;
}


void nsp_rand_markov(double *q, int *key, double *X0, double *X, int n, int X0mn, int m)
{
  int i, j, k, icur;

  for ( i = 0 ; i < X0mn ; i++ )
    {
      icur = (int) X0[i] - 1;
      for ( j = 0, k = i ; j < m ; j++, k+=X0mn )
	{
	  icur = nsp_rand_discrete_guide(&q[(n+1)*icur], &key[n*icur], n);
	  X[k] = (double) (icur + 1);
	}
    }
}
	 
	 

	 
