/* n-dim linear interpolation routine for Nsp 
 * together with its interface.
 *
 * Copyright (C) 2005  Bruno Pincon
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


#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <strings.h>
#include <nsp/machine.h>
#include <nsp/math.h>
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"

enum {NATURAL, PERIODIC, BY_ZERO, C0, BY_NAN};

static int good_order(double x[], int n)
{
  /*  test if x[i-1] < x[i] */
  int i;

  if (fabs(x[0]) > DBL_MAX  ||  x[n-1] >  DBL_MAX)
    return 0;

  for ( i = 1 ; i < n ; i++ ) 
    if ( ! (x[i-1] < x[i]) )   /* form of this test for detecting nan ... */ 
      return 0;

  return 1;
}

static int isearch(double t, double x[], int n) 
{
  /*     PURPOSE
   *        x[0..n-1] being an array (with strict increasing order and n >=2)
   *        representing intervals, this routine return i such that :
   *           
   *           x[i] <= t <= x[i+1]
   *          
   *        and -1 if t is not in [x[0], x[n-1]] 
   */
  int i1, i2, i;
  if ( x[0] <= t  &&  t <= x[n-1] )
    {
     i1 = 0; i2 = n-1;
     while ( i2 - i1 > 1 )
       {
	 i = (i1 + i2)/2;
	 if ( t <= x[i] )
	   i2 = i;
	 else
	   i1 = i;
       }
     return (i1);
    }
  else
    return (-1);
}

static void fast_int_search(double xx, double x[], int nx, int *i)
{
  if ( *i == -1 )
    *i = isearch(xx, x, nx);
  else if ( !  (x[*i] <= xx && xx <= x[*i+1]) )
    *i = isearch(xx, x, nx);
}


static void coord_by_periodicity(double *t, double x[], int n, int *i)
{
  /*
   *     PURPOSE
   *        recompute t such that t in [x[0], x[n-1]] by periodicity :
   *        and then the interval i of this new t
   */
  double r, L;
  L = x[n-1] - x[0];
  r = (*t - x[0]) / L;
  if (r >= 0.0)
    *t = x[0] + (r - floor(r))*L;
  else
    *t = x[n-1] + (r - ceil(r))*L;

  /*  some cautions in case of roundoff errors (is necessary ?) */
  if (*t < x[0])
    {
      *t = x[0];
      *i = 0;
    }
  else if (*t > x[n-1])
    {
      *t = x[n-1];
      *i  = n-2;
    }
  else
    *i = isearch(*t, x, n);
}

void nlinear_interp(double **x , double val[], int dim[], int n,
		    double **xp, double yp[], int np, int outmode, 
		    double u[], double v[], int ad[], int k[])
{
  /*  interpolation lineaire nb_dim-dimensionnelle
   *  --------------------------------------------

   interface nsp
   
   yp = linear_interpn(xp1, x1, val, outmode)
   yp = linear_interpn(xp1, xp2, x1, x2, val, outmode)
   .....
   yp = linear_interpn(xp1, ..., xpN, x1, ..., xN, val, outmode)



   *     x[j][] : the grid abscissae in the dim j
   *     dim[j] : nb of points in the dim j
   *     n      : number of dimension
   *     val[]  : array of the grid node values, for instance if nbdim = 3
   *              and dim = [nx ny nz] then val(i,j,k) is stored in
   *              i + nx( j + ny k ) 
   *     xp[][] : the coordinates where we have to interpolate
   *              the coordinate of the i th point are stored 
   *              at xp[0][i] ..... xp[n-1][i]
   *     yp[]   : the result (an array 0...np-1)
   *     np     : nb of points for the evaluation
   *     outmode: specify the method of evaluation when a point is 
   *              outside the grid
   *     u, v, ad, k : work arrays
   */  

  int i, j, l, p, temp, b,/* toto,*/ two_p_n;
  double xx;
  double NAN = (2*DBL_MAX)*0.0;

  /*   
   *   calcul des decalages d'indices pour retrouver les valeurs
   *   de l'hypercube encadrant le point à interpoler 
   */
  ad[0] = 0; ad[1] = 1;
  temp = 1 ; p = 1;
  for ( j = 0; j < n-1; j++)
    {
      temp = temp * dim[j];
      p = 2*p;
      for ( i = 0; i < p; i++ )
	ad[p+i] = ad[i] + temp;
    };
  /* a ce niveau on a  p = 2^(n-1)  */
  two_p_n = 2*p;

  /* initialisation pour recherche d'intervalle rapide */
  for ( j = 0; j < n; j++ ) k[j] = -1;

  for ( i = 0; i < np; i++ )
    {
      /* interpolation du i eme point */
      
      /*  1 - recherche des intervalles  */
      for ( j = 0; j < n; j++ )
	{
	  xx = xp[j][i];
	  if ( ISNAN(xx) )
	    {
	      v[0] = NAN; goto fin;
	    }
	  fast_int_search(xx, x[j], dim[j], &(k[j]));
	  if ( k[j] == -1 )   /* le point est a l'exterieur */ 
	    switch (outmode)
	      {
	      case BY_NAN :
		v[0] = NAN;
		goto fin;

	      case BY_ZERO :
		v[0] = 0.0;
		goto fin;

	      case NATURAL :
		if (xx < x[j][0])
		  k[j] = 0;
		else
		  k[j] = dim[j]-2;
		break;

	      case C0 :
		if (xx < x[j][0])
		  { 
		    u[j] = 0.0; k[j] = 0;
		  }  
		else
		  {
		    u[j] = 1.0; k[j] = dim[j]-2;
		  }
		continue;

	      case PERIODIC :
		coord_by_periodicity(&xx, x[j], dim[j], &(k[j]));
		break;

	      }
	  u[j] = (xx - x[j][k[j]])/( x[j][k[j]+1] -  x[j][k[j]]);  /* coord bary */
	}

      /* 2 - calcul de l'indice de base */
      b = k[n-1];
      for ( j = n-2; j >= 0; j-- )
	b = k[j] + dim[j]*b;

      /* 3 - mise des valeurs de l'hypercube dans v */
      for ( j = 0; j < two_p_n; j++ )
	v[j] = val[b + ad[j]];

      /* 4 - interpolation */
      temp = 1; p = two_p_n;
      for ( j = 0; j < n ; j++ )
	{
	  for ( l = 0; l < two_p_n; l+=2*temp)
	    {
	      v[l] = v[l]*(1.0 - u[j]) + v[l+temp]*u[j];
	    }
	  p = p/2; 
	  temp = 2*temp; 
	}

      /* 5 - on met le resultat a sa place */
    fin:
      yp[i] = v[0];
    
    }
}



int int_nsp_linear_interpn( Stack stack, int rhs, int opt, int lhs)
{ 
/*  interface on the ndim linear interpolation routine
 *
 *   yp = linear_interpn(xp1, ..., xpn, x1, ..., xn, val, outmode)
 */
  char *str;
  int n = (rhs+1)/2 - 1, m, mxp, nxp, mnxp, i, *ndim, ndim_prod=1, *ad, *k, outmode;
  double **xp, **x, *u, *v, *val;
  NspMatrix *B;

  if ( n < 1 )
    { 
      Scierror("%s: too few arg \r\n", stack.fname);
      return RET_BUG;
    }
  m = 1; for ( i = 1 ; i <= n ; i++) m = 2*m;

  /* allocate some work arrays */
  xp = malloc(n*sizeof(double *));
  x = malloc(n*sizeof(double *));
  ndim = malloc(n*sizeof(int));
  u = malloc(n*sizeof(double));
  v = malloc(m*sizeof(double));
  ad = malloc(m*sizeof(int));
  k = malloc(n*sizeof(int));
  if ( ! (xp && x && ndim && u && v && ad && k) )
    {
      free(xp); free(x); free(ndim); free(u); free(v); free(ad); free(k);
      Scierror("%s: not enough memory \n", stack.fname);
      return  RET_BUG;
    } 

  /* parse input arg */
  if ((B = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  mxp = B->m; nxp = B->n; mnxp = B->mn; xp[0] = B->R;

  for ( i = 2 ; i <= n ; i++ )
    {
      if ((B = GetMat(stack,i)) == NULLMAT) return RET_BUG;
      if ( mxp != B->m || nxp != B->n )
	{ 
	  Scierror("%s: bad inputs for xp1, xp2, ...., \r\n", stack.fname);
	  return  RET_BUG;
	}
      xp[i-1] = B->R;
    }

  for ( i = 1 ; i <= n ; i++ )
    {
      if ((B = GetMat(stack,i+n)) == NULLMAT) return RET_BUG;
      CheckVector(stack.fname,i+n,B);
      if (B->mn < 2)
	{ 
	  Scierror("%s: bad arg number %d \r\n", stack.fname, n+i);
	  return  RET_BUG;
	}
      if ( !good_order(B->R, B->mn) )
	{
	  Scierror("%s: grid abscissae of dim %d not in strict increasing order \r\n", stack.fname, n+i);
	  return 0;
	}
      ndim[i-1] = B->mn;
      ndim_prod *= B->mn;
      x[i-1] = B->R;
     }
  
  /* value of the function on the grid (when nsp will support nd arrays this would change) */
  if ((B = GetMat(stack,2*n+1)) == NULLMAT) return RET_BUG;
  if ( B->mn != ndim_prod )
    {
      Scierror("%s: bad dimension for the grid values \n", stack.fname);
      return 0;
    }
  val = B->R;

  /* get the outmode */
  if ( rhs == 2*n + 2 )
    {
      if ((str = GetString(stack,2*n+2)) == (char*)0) return RET_BUG;
      if (strcmp(str,"C0")==0) 
	outmode = C0;
      else if (strcmp(str,"by_zero")==0) 
	outmode = BY_ZERO;
      else if (strcmp(str,"natural")==0) 
	outmode = NATURAL;
      else if (strcmp(str,"periodic")==0) 
	outmode =  PERIODIC;
      else if (strcmp(str,"by_nan")==0) 
	outmode =  BY_NAN;
      else
	{
	  Scierror("%s: %s unknown or supported outmode type\n\r",stack.fname, str);
	  return  RET_BUG;
	}
    }
  else
    outmode = C0;

  /* allocate memory for the output */
  if ((B = nsp_matrix_create(NVOID,'r',mxp,nxp))== NULLMAT) return RET_BUG;

  nlinear_interp(x, val, ndim, n, xp, B->R, mnxp, outmode, u, v, ad, k);

  free(xp); free(x); free(ndim); free(u); free(v); free(ad); free(k);

  MoveObj(stack,1,(NspObject *) B);
  return 1;
}

