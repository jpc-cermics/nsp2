/* 
 * some interpolation routines for Nsp 
 * Copyright (C) 2006-2019  Bruno Pincon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as      
 * published by the Free Software Foundation, either version 3 of the  
 * License, or (at your option) any later version.                     
 *                                                                     
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of          
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   
 * Lesser General Public License for more details.                     
 *                                                                     
 * You should have received a copy of the GNU Lesser General Public    
 * License along with this program.  If not, see                       
 * <http://www.gnu.org/licenses/>.                                     
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <string.h>
#include <nsp/math.h>
#include "nsp/approx.h"
#include <nsp/object.h>
#include <nsp/matrix.h>

/**
 * SECTION:approx
 * @Title: LibApprox 
 * @Short_Description:  some interpolation routines
 * @Stability_Level: 
 * @See_Also: 
 *
 * 
 */

/**
 * isearch:
 * @t: a double 
 * @x: an array of double of size @n
 * @n: size of array @x
 * 
 * x[0..n-1] being an array (with strict increasing order and n >=2)
 * representing intervals, 
 * 
 * Returns: i such that x[i] <= t <= x[i+1] and -1 
 *   if t is not in [x[0], x[n-1]] 
 **/

static int isearch(double t,const double x[], int n) 
{
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

/**
 * fast_int_search:
 * @xx: 
 * @x: 
 * @nx: 
 * @i: 
 * 
 * 
 **/

static void fast_int_search(double xx,const double x[], int nx, int *i)
{
  if ( *i == -1 )
    *i = isearch(xx, x, nx);
  else if ( !  (x[*i] <= xx && xx <= x[*i+1]) )
    *i = isearch(xx, x, nx);
}


/**
 * coord_by_periodicity:
 * @t: 
 * @x: 
 * @n: 
 * @i: 
 * 
 *        recompute t such that t in [x[0], x[n-1]] by periodicity :
 *        and then the interval i of this new t
 * 
 **/
static void coord_by_periodicity(double *t,const double x[], int n, int *i)
{
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

/**
 * nsp_nlinear_interp:
 * @x: the grid abscissae  (x[j][] are the grid abscissae in the dimension j)
 * @dim: nb of grid points in each dimension (@dim[j] is the nb of points in dimension j)
 * @n: number of dimensions (@n=1 for linear interpolation, @n=2 for bilinear interpolation, etc...) 
 * @val: array of the grid node values, for instance if nbdim = 3
 *       and dim = [nx ny nz] then @val(i,j,k) is stored in i + nx( j + ny k ) 
 * @xp: the coordinates where we have to interpolate (the coordinate of the 
 *      i th point are stored at @xp[0][i] ..... @xp[@n-1][i])
 * @yp: the result (an array 0...@np-1)
 * @np: nb of points for the evaluation
 * @outmode: specify the method of evaluation when a point is outside the grid
 * @u: work array of size @n
 * @v: work array of size 2^@n
 * @ad: work array of size 2^@n
 * @k: work array of size @n
 *
 * computes @n-dimensionnal linear interpolation.
 * 
 */

void nsp_nlinear_interp(double **x , double val[], int dim[], int n,
			double **xp, double yp[], int np, int outmode, 
			double u[], double v[], int ad[], int k[])
{
  int i, j, l, p, temp, b, two_p_n;
  double xx;
  double nan_val = 0.0/0.0;

  /*   
   *   calcul des decalages d'indices pour retrouver les valeurs
   *   de l'hypercube encadrant le point � interpoler 
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
	      v[0] = nan_val; goto fin;
	    }
	  fast_int_search(xx, x[j], dim[j], &(k[j]));
	  if ( k[j] == -1 )   /* le point est a l'exterieur */ 
	    switch (outmode)
	      {
	      case BY_NAN :
		v[0] = nan_val;
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


/**
 * tri_diag_solve:
 * @d: d[0..n-1] is on input the diagonal of A 
 *        (overwritten with the diagonal of the (diagonal) matrix D)
 * @l: l[0..n-2] is on input the sub-diagonal of A (overwritten
 *        with the sub-diagonal of L)
 * @b: b[0..n-1] is on input the rhs on output the solution x 
 * @n: the dimension 
 * 
 * solves a linear system Ax = b where A is symetric tridiagonal 
 *        (and supposed positive definite) using a LDL^t factorization
 * Caution: no zero pivot detection (A is supposed positive definite)
 * 
 **/

static void tri_diag_solve(double *d, double *l, double *b, int n)
{
  int i;
  double temp;

  for ( i = 1 ; i < n ; i++ )
    {
      temp = l[i-1];
      l[i-1] = l[i-1] / d[i-1];
      d[i] = d[i] - temp * l[i-1];
      b[i] = b[i] - l[i-1]*b[i-1];
    }

  b[n-1] = b[n-1] / d[n-1];

  for ( i = n-2 ; i >= 0 ; i-- )
    b[i] = b[i]/d[i] - l[i]*b[i+1];
}

/**
 * nearly_tri_diag_solve:
 * @d: d[0..n-1] is on input the diagonal of A (overwritten with
 *                      the diagonal of the (diagonal) matrix D)
 * @lsd: lsd[0..n-3] ison input the sub-diagonal of A (without  A(n-1,n-2)) 
 *                      (overwritten with the sub-diagonal of L (without  L(n-1,n-2))
 * @lll: lll[0..n-2] is on input the last line of A (without A(n-1,n-1))
 *                      (overwritten with the last line of L (without L(n-1,n-1)))
 * @b: b[0..n-1] is on input the rhs b, on output the solution x
 * @n: is the dimension 
 * 
 *        solve a linear system Ax= b with an LDL^t factorization. A is a symetric matrix 
 *        A "nearly" tridiagonal (supposed to be positive definite) with the form :
 *           
 *          |x x         x|                        |1            |
 *          |x x x       x|                        |x 1          |
 *          |  x x x     x|                        |  x 1        |
 *          |    x x x   x|  and so the L is like  |    x 1      |
 *          |      x x x x|                        |      x 1    |
 *          |        x x x|                        |        x 1  |
 *          |x x x x x x x|                        |x x x x x x 1|
 * 
 *        Caution: A is supposed to be positive definite so no zero pivot detection is done.
 **/

static void nearly_tri_diag_solve(double *d, double *lsd, double *lll, double *b, int n)
{
      
  int i, j;
  double temp1, temp2; 

  for ( i = 0 ; i < n-2 ; i++ )
    {
      temp1 = lsd[i];
      temp2 = lll[i];
      lsd[i] = lsd[i] / d[i];
      lll[i] = lll[i] / d[i];
      d[i+1] = d[i+1] - lsd[i]*temp1;  /* elimination on line i+1 */
      lll[i+1] = lll[i+1] - lll[i]*temp1;  /* elimination on line n */
      d[n-1] = d[n-1]     - lll[i]*temp2; /* elimination on line n  */
    }
  temp2 = lll[n-2];
  lll[n-2] = lll[n-2] / d[n-2];
  d[n-1] = d[n-1] - lll[n-2]*temp2;

  /* solve LDL^t x = b */
  for ( i = 1 ; i < n-1 ; i++ )
    b[i] = b[i] - lsd[i-1]*b[i-1];

  for ( j = 0 ; j < n-1 ; j++ )
    b[n-1] = b[n-1] - lll[j]*b[j];
      
  for ( i = 0 ; i < n ; i++ )
    b[i] = b[i] / d[i];

  b[n-2] = b[n-2] - lll[n-2]*b[n-1];

  for ( i = n-3 ; i >= 0 ; i-- )
    b[i] = b[i] - lsd[i]*b[i+1] - lll[i]*b[n-1];
}


/**
 * nsp_derivd:
 * @x: the n points, @x part must be in strict increasing order
 * @u: the n points, @u part.
 * @du: gives in output the derivatives in each x[i] i = 0..n-1
 * @n:  number of point (n >= 2)
 * @inc:  integer to deal easily with 2d applications, u[i] is in fact
 *        u[inc*i] 
 * @type: integer : FAST (the function is non periodic) or FAST_PERIODIC
 *        (the function is periodic), in this last case u[n-1] must be equal to u[0])
 * 
 *        given functions values u[i] at points x[i],  i = 0, ..., n-1
 *        this subroutine computes approximations du[i] of the derivative
 *        at the points x[i]. 
 * 
 *      METHOD
 *        For i in [1,n-2], the "centered" formula of order 2 is used :
 *            d(i) = derivative at x(i) of the interpolation polynomial
 *                   of the points {(x(j),u(j)), j in [i-1,i+1]}
 *
 *         For i=0 and n-1, if type = FAST_PERIODIC  (in which case u[n-1]=u[0]) then
 *         the previus "centered" formula is also used; else (type = FAST), d[0]
 *         is the derivative at x[0] of the interpolation polynomial of
 *         {(x(j),u(j)), j in [0,2]} and the same method is used for d[n-1]
 *
 * 
 *    NOTES
 *         this routine requires (i)   n >= 2
 *                               (ii)  strict increasing abscissae x[i]
 *                               (iii) u[0]=u[n-1] if type = FAST_PERIODIC
 *         ALL THESE CONDITIONS MUST BE TESTED IN THE CALLER.
 *
 * 
 **/

void nsp_derivd(double *x, double *u, double *du, int n, int inc, int type)
{
  /*
   *     AUTHOR: Bruno Pincon
   */

  double dx_l, du_l, dx_r, du_r, w_l, w_r;
  int i;

  if (n == 2)  /* special case used linear interp */
    {
      du[0] = (u[inc] - u[0])/(x[1]-x[0]);
      du[inc] = du[0];
      return;
    }

  if (type == FAST_PERIODIC)
    {
      dx_r  = x[n-1]-x[n-2];
      du_r  = (u[(n-1)*inc] - u[(n-2)*inc]) / dx_r;
      for ( i = 0 ; i < n-1 ; i++ )
	{
	  dx_l = dx_r;
	  du_l = du_r;
	  dx_r = x[i+1] - x[i];
	  du_r = (u[(i+1)*inc] - u[i*inc])/dx_r;
	  w_l  = dx_r/(dx_l + dx_r);
	  w_r = 1.0 - w_l;
	  du[i*inc] = w_l*du_l + w_r*du_r;
        }
      du[(n-1)*inc] = du[0];
    }

  else  /* type == FAST */
    {
      dx_l = x[1] - x[0];
      du_l =(u[inc] - u[0])/dx_l;
      dx_r = x[2] - x[1];
      du_r = (u[2*inc] - u[inc])/dx_r;
      w_l = dx_r/(dx_l + dx_r);
      w_r = 1.0 - w_l;
      du[0] = (1.0 + w_r)*du_l - w_r*du_r;
      du[inc] = w_l*du_l + w_r*du_r;
      for ( i = 2 ; i < n-1 ; i++ )
	{
	  dx_l = dx_r;
	  du_l  = du_r;
	  dx_r = x[i+1] - x[i];
	  du_r  = (u[(i+1)*inc] - u[i*inc])/dx_r;
	  w_l  = dx_r/(dx_l + dx_r);
	  w_r = 1.0 - w_l;
	  du[i*inc] = w_l*du_l + w_r*du_r;
	}
      du[(n-1)*inc] = (1.0 + w_l)*du_r - w_l*du_l;
    }
}


/**
 * nsp_cubic_spline:
 * @x: the n interpolation points, x must be in strict increasing order
 * @y: the n interpolation points, x must be in strict increasing order
 * @d: on output, the derivatives in each x[i] i = 0..n-1
 * @n: number of interpolation points (n >= 3)
 * @type: type of the spline:  
 *                  NOT_A_KNOT spline with:
 *                                 s'''(x(2)-) = s'''(x(2)+) 
 *                             and s'''(x(n-1)-) = s'''(x(n-1)+)
 *                  NATURAL spline with:
 *                                 s''(x1) = 0
 *                             and s''(xn) = 0
 *                  CLAMPED spline with end point derivative provided 
 *                            (d[0] and d[n-1] given)
 *                  PERIODIC
 * @A_d:  work array, A_d(0..n-1)
 * @A_sd: work array, A_sd(0..n-2)
 * @qdy: work array,qdy(0..n-2)
 * @lll:work array, lll[0..n-2]  used only in the periodic case
 * 
 * computes a cubic spline interpolation function
 * in Hermite form (ie computes the derivatives d(i) of the
 * spline in each interpolation point (x(i), y(i)))
 * 
 *  NOTE that this routine requires (i)   n >= 3 (for natural) n >=4 (for not_a_knot) 
 *      (ii)  strict increasing abscissae x(i)
 *       (iii) y[0] = y[n-1] in the periodic case
 *  THESE CONDITIONS MUST BE TESTED BEFORE CALLING nsp_cubic_spline().
 *
 **/

void nsp_cubic_spline(double *x, double *y, double *d, int n, int type, 
		      double *A_d, double *A_sd, double *qdy, double *lll)
{
  int i;
  double r;

  if (n == 2)
    {
      if (type != CLAMPED)
	{
	  d[0] = (y[1] - y[0])/(x[1] - x[0]);
	  d[1] = d[0];
	}
      return;
    }

  if (n == 3  &&  type == NOT_A_KNOT)
    {
      nsp_derivd(x, y, d, n, 1, FAST);
      return;
    }

  for ( i = 0 ; i < n-1 ; i++ )
    {
      A_sd[i] = 1.0 / (x[i+1] - x[i]);
      qdy[i] = (y[i+1] - y[i]) * A_sd[i]*A_sd[i];
    }

  /*  compute the coef matrix and r.h.s. for rows 1..n-2  */
  /*  (which don't relies on the type) */ 
  
  for ( i = 1 ; i < n-1 ; i++ )
    {
      A_d[i] = 2.0*( A_sd[i-1] + A_sd[i] );
      d[i] = 3.0 * ( qdy[i-1] + qdy[i] );
    }

  /*  compute equ 0 and n-1 in function of the type */
  switch (type)
    {
      case NATURAL:
	A_d[0] =  2.0*A_sd[0];
	d[0] = 3.0*qdy[0];
	A_d[n-1] =  2.0*A_sd[n-2];
	d[n-1] =  3.0*qdy[n-2];
	tri_diag_solve(A_d, A_sd, d, n);
	break;

      case NOT_A_KNOT:
	/*  s'''(x[1]-) = s'''(x[1]+) */
	r = A_sd[1]/A_sd[0];
	A_d[0] = A_sd[0]/(1.0 + r);
	d[0] = ((3.0*r+2.0)*qdy[0]+r*qdy[1])/((1.0+r)*(1.0+r));
	/*  s'''(x[n-2]-) = s'''(x[n-2]+) */
	r = A_sd[n-3]/A_sd[n-2];
	A_d[n-1] = A_sd[n-2]/(1.0 + r);
	d[n-1] = ((3.0*r+2.0)*qdy[n-2]+r*qdy[n-3])/((1.0+r)*(1.0+r));
	tri_diag_solve(A_d, A_sd, d, n);
	break;

      case CLAMPED:
	/* d[0] and d[n-1] are already known  */
	d[1] = d[1] - d[0]*A_sd[0];
	d[n-2] = d[n-2] - d[n-1]*A_sd[n-2];
        tri_diag_solve((A_d+1), (A_sd+1), (d+1), n-2);
	break;

      case PERIODIC:
	A_d[0] = 2.0*( A_sd[0] + A_sd[n-2] );
	d[0] = 3.0 * ( qdy[0] + qdy[n-2] );
	lll[0] = A_sd[n-2];
        for (i=1 ; i<n-1 ; i++) lll[i]=0.0;
	lll[n-3] = A_sd[n-3];
        nearly_tri_diag_solve(A_d, A_sd, lll, d, n-1);
	d[n-1] = d[0];
	break;
    }
}

/**
 * dpchst:
 * @arg1: a double 
 * @arg2: a double 
 * 
 * dpchip sign-testing routine.
 * 
 * Returns: -1. if arg1 and arg2 are of opposite sign.
 *  0. if either argument is zero.
 *  +1. if arg1 and arg2 are of the same sign.
 **/

static int dpchst(double arg1, double arg2)
{
  /* from Slatec */
  /* dpchst:  dpchip sign-testing routine.
   */
  if ( arg1 == 0.0  ||  arg2 == 0.0 )
    return 0;
  else if ( (arg1 > 0.0 && arg2 > 0.0)  ||  (arg1 < 0.0 && arg2 < 0.0) )
    return 1;
  else
    return -1;
}

/**
 * nsp_dpchim:
 * @x: 
 * @u: 
 * @d: 
 * @n: 
 * @inc: 
 * 
 * An adaptation in C of the Slatec dpchim routine.
 * DPCHIM:  Piecewise Cubic Hermite Interpolation to Monotone data 
 **/

void nsp_dpchim(double *x, double *u, double *d, int n, int inc)
{
  int i;
  double del1, del2, dmax, dmin, drat1, drat2, 
    h1, h2, hsum, hsumt3, w1, w2;

  h1 = x[1] - x[0];
  del1 = (u[inc] - u[0])/h1;
  /* dsave = del1; */

  /* special case n=2 -- use linear interpolation */
  if (n == 2)
    {
      d[0] = del1;
      d[inc] = del1;
      return;
    }

  /*  normal case n>=3 */
  h2 =  x[2] - x[1];
  del2 = (u[2*inc] - u[inc])/h2;

  /*  set d[0] via non-centered three-point formula, adjusted to be
      shape-preserving.  */
  hsum = h1 + h2;
  w1 = (h1 + hsum)/hsum;
  w2 = -h1/hsum;
  d[0] = w1*del1 + w2*del2;
  if ( dpchst(d[0],del1) <= 0)
    d[0] = 0.0;
  else if ( dpchst(del1,del2) < 0 )
    {
      /* need do this check only if monotonicity switches. */
      dmax = 3.0*del1;
      if (fabs(d[0]) > fabs(dmax))
	d[0] = dmax;
    }

  /*  loop through interior points. */
  for ( i = 1 ; i < n-1 ; i++ )
    {
      if ( i > 1 )
	{
	  h1 = h2;
	  h2 = x[i+1] - x[i];
	  hsum = h1 + h2;
	  del1 = del2;
	  del2 = (u[(i+1)*inc] - u[i*inc])/h2;
	}
      switch (dpchst(del1,del2)) /*  set d[i]=0 unless data are strictly monotonic. */
	{
	case -1:
	  d[i*inc] = 0.0;
	  /* dsave = del2; */
	  break;
	case 0:
	  d[i*inc] = 0.0;
	  if (del2 != 0.0)
	    {
	      /* dsave = del2; */
	    }
	  break;
	case 1:
	  /* use brodlie modification of butland formula. */
	  hsumt3 = hsum+hsum+hsum;
	  w1 = (hsum + h1)/hsumt3;
	  w2 = (hsum + h2)/hsumt3;
	  dmax = Max( fabs(del1), fabs(del2) );
	  dmin = Min( fabs(del1), fabs(del2) );
	  drat1 = del1/dmax;
	  drat2 = del2/dmax;
	  d[i*inc] = dmin/(w1*drat1 + w2*drat2);
	}
    }

  /*  set d[n-1] via non-centered three-point formula, 
      adjusted to be shape-preserving. */
  w1 = -h2/hsum;
  w2 = (h2 + hsum)/hsum;
  d[(n-1)*inc] = w1*del1 + w2*del2;
  if ( dpchst(d[(n-1)*inc],del2) <= 0.0 )
    d[(n-1)*inc] = 0.0;
  else if ( dpchst(del1,del2) < 0.0 )
    {
      /* need do this check only if monotonicity switches. */
      dmax = 3.0*del2;
      if (fabs(d[(n-1)*inc]) > fabs(dmax))  
	d[(n-1)*inc] = dmax;
    }
}

/**
 * newton_form_for_hermite:
 * @xa: 
 * @xb: 
 * @ya: 
 * @yb: 
 * @da: 
 * @db: 
 * @c2: 
 * @c3: 
 * 
 * 
 **/
static void newton_form_for_hermite(double xa, double xb, double ya, double yb, 
				    double da, double db, double *c2, double *c3)
{
  /*   compute the following Newton form:                           */
  /*      h(t) = ya + da*(t-xa) + c2*(t-xa)^2 + c3*(t-xa)^2*(t-xb)  */
  double dx, p;
  dx = xb - xa;
  p = (yb - ya)/dx;
  *c2 = (p - da)/dx;
  *c3 = ((db - p)+(da - p))/(dx*dx);
}
/**
 * eval_hermite:
 * @t: 
 * @xa: 
 * @xb: 
 * @ya: 
 * @da: 
 * @c2: 
 * @c3: 
 * @h: 
 * @dh: 
 * @ddh: 
 * @dddh: 
 * 
 * 
 * 
 * Returns: 
 **/
 
static void eval_hermite(double t, double xa, double xb, double ya, double da, double c2, 
			 double c3, double *h, double *dh, double *ddh, double *dddh)
{
  double tmxa;
  /*  eval h(t), h'(t), h"(t) and h"'(t), by a generalised Horner 's scheme */
  /*  h(t) = ya + da*(t-xa) + c2*(t-xa)^2 + c3*(t-xa)^2*(t-xb)   */
  tmxa = t - xa;
  *h = c2 + c3*(t - xb);
  *dh = *h + c3*tmxa;
  *ddh = 2.0*( *dh + c3*tmxa );
  *dddh = 6.0*c3;
  *h = da + (*h)*tmxa;
  *dh = *h + (*dh)*tmxa;
  *h = ya + (*h)*tmxa;
}

/**
 * nsp_eval_piecewise_hermite:
 * @t: (input) vector of abscissae where the spline or subspline must be evaluated
 * @st: (output) values of the spline or subspline at @t
 * @dst: (output) values of first derivative of the spline or subspline at @t
 * @d2st: (output) values of second derivative of the spline or subspline at @t
 * @d3st: (output) values of the third derivative of the spline or subspline at @t
 * @m: (input) length of @t
 * @x: (input) breakpoints of the spline or subspline
 * @y: (input) values of the spline or subspline at the breakpoint @x
 * @d: (input) values of the derivative of the spline or subpline at the breakpoint @x
 * @n: (input) number of breakpoints
 * @outmode: (input) defines the behavior to evaluate the spline or subspline outside [x[0], x[n-1]]
 * 
 * evaluation at the abscissae t[0..m-1] of the piecewise hermite function
 * defined by x[0..n-1], y[0..n-1], d[0..n-1] (d being the derivatives at the
 * x[i]) together with its derivative, second derivative and third derivative
 * outmode defines what return in case t[j] not in [x[0], x[n-1]]
 * 
 **/

void nsp_eval_piecewise_hermite(double *t, double *st, double *dst, double *d2st, 
				double *d3st, int m, double *x, double *y, double *d, 
				int n, int outmode)
{
  int i, j, i_old=-1;;
  double tt, c2=0, c3=0;
  double Nan = 0.0/0.0;

  i = -1;
  for ( j = 0 ; j < m ; j++ )
    {
      tt = t[j];
      fast_int_search(tt, x, n, &i);  /* recompute i only if necessary */
      if ( i != -1 )
	{
	  if ( i != i_old )
	    {
	      newton_form_for_hermite(x[i], x[i+1], y[i], y[i+1], d[i], d[i+1], &c2, &c3);
	      i_old = i;
	    }
	  eval_hermite(tt, x[i], x[i+1], y[i], d[i], c2, c3 , st+j, dst+j, d2st+j, d3st+j);
	}
      else   
	/* t(j) is outside  [x[0], x[n-1]] evaluation depend upon outmode */ 
	if (outmode == BY_NAN  ||  ISNAN(tt) )
	  {
	    st[j] = Nan; dst[j] = Nan; d2st[j] = Nan; d3st[j] = Nan;
	  }
	else if (outmode == BY_ZERO)
	  {
	    st[j] = 0.0; dst[j] = 0.0; d2st[j] = 0.0; d3st[j] = 0.0;
	  }
	else if (outmode == C0)
	  {
	    dst[j] = 0.0; d2st[j] = 0.0; d3st[j] = 0.0;
	    if ( tt < x[0] ) st[j] = y[0]; else st[j] = y[n-1];
	  }
	else if (outmode == LINEAR)
	  {
	    d2st[j] = 0.0; d3st[j] = 0.0;
	    if ( tt < x[0] )
	      {
		dst[j] = d[0]; st[j] = y[0] + (tt - x[0])*d[0];
	      }
	    else
	      {
		dst[j] = d[n-1]; st[j] = y[n-1] + (tt - x[n-1])*d[n-1];
	      }
	  }
	else
	  {
	    if (outmode == NATURAL)
	      if (tt < x[0]) i = 0; else i=n-2;
	    else if (outmode == PERIODIC)
	      coord_by_periodicity(&tt, x, n, &i);
	    if ( i != i_old )
	      {
		newton_form_for_hermite(x[i], x[i+1], y[i], y[i+1], d[i], d[i+1], &c2, &c3);
		i_old = i;
	      }
	    eval_hermite(tt, x[i], x[i+1], y[i], d[i], c2, c3 , st+j, dst+j, d2st+j, d3st+j);
	  }
    }
}

/**
 * coef_bicubic:
 * @u: (input) array of size nx x ny, u(i,j)=u[i+nx*j] value u(x,y) at the grid point (@x[i],@y[j])
 * @p: (input) array of size nx x ny, p(i,j)=p[i+nx*j] value of du/dx at the grid point (@x[i],@y[j])
 * @q: (input) array of size nx x ny, q(i,j)=q[i+nx*j] value of du/dy at the grid point (@x[i],@y[j])
 * @r: (input) array of size nx x ny, r(i,j)=r[i+nx*j] value of ddu/dxdy at the grid point (@x[i],@y[j])
 * @x: (input) first coordinate of the grid points
 * @y: (input) second coordinate of the grid points
 * @nx: (input) size of the grid in x
 * @ny: (input) size of the grid in y
 * @C: (output) array of size 4 x 4 x (nx-1) x (ny-1)
 * 
 * compute for each bicubic polynomial (i,j)-patch (defined on 
 * [x(i),x(i+1)]x[y(i),y(i+1)]) the following base representation:
 *
 *           i,j        _3_  _3_   i,j       k        l
 *          u   (x,y) = >__  >__  C   (x-x(i)) (y-y(j)) 
 *                      k=0  l=0   k,l
 *
 * from the "Hermite" representation (values of u, p = du/dx,
 * q = du/dy, r = ddu/dxdy at the 4 vertices (x(i),y(j)),
 * (x(i+1),y(j)), (x(i+1),y(j+1)), (x(i),y(j+1)). 
 *
 * the coef C(k,l,i,j) defined below is stored in C[ k+4*l + 16*(i + (nx-1)j) ]
 * 
 **/
static void coef_bicubic(double *u, double *p, double *q, double *r, double *x, double *y, int nx, int ny, double *C)
{
  int i, j, k;
  double a, b, c, d, dx, dy;

  for (j = 0, k = 0 ; j < ny-1 ; j++ )
    {
      dy = 1.0/(y[j+1] - y[j]);

      for ( i = 0 ; i < nx-1 ; i++, k+=16 )
	{
	  dx = 1.0/(x[i+1] - x[i]);

	  C[k] = u[i + nx*j];
	  C[k+1] = p[i + nx*j];
	  C[k+4] = q[i + nx*j];
	  C[k+5] = r[i + nx*j];

	  a = (u[i+1 + nx*j] - u[i + nx*j])*dx;
	  C[k+2] = (3.0*a -2.0*p[i + nx*j] - p[i+1 + nx*j])*dx;
          C[k+3] = (p[i+1 + nx*j] + p[i + nx*j] - 2.0*a)*(dx*dx);

	  a = (u[i + nx*(j+1)] - u[i + nx*j])*dy;
	  C[k+8] = (3.0*a -2.0*q[i + nx*j] - q[i + nx*(j+1)])*dy;
	  C[k+12] = (q[i + nx*(j+1)] + q[i + nx*j] - 2.0*a)*(dy*dy);
            
	  a = (q[i+1 + nx*j] - q[i + nx*j])*dx;
	  C[k+6] = (3.0*a - r[i+1 + nx*j] - 2.0*r[i+nx*j])*dx;
	  C[k+7] = (r[i+1 + nx*j] + r[i + nx*j] - 2.0*a)*(dx*dx);

	  a = (p[i + nx*(j+1)] - p[i + nx*j])*dy;
	  C[k+9] = (3.0*a - r[i + nx*(j+1)] - 2.0*r[i + nx*j])*dy;
	  C[k+13] = (r[i + nx*(j+1)] + r[i + nx*j] - 2.0*a)*(dy*dy);

	  a = (u[i+1 + nx*(j+1)] + u[i + nx*j] - u[i+1 + nx*j] - u[i + nx*(j+1)])*(dx*dx*dy*dy)
	    - (p[i + nx*(j+1)] - p[i + nx*j])*(dx*dy*dy)
	    - (q[i+1 + nx*j] - q[i + nx*j])*(dx*dx*dy)
	    + r[i + nx*j]*(dx*dy);

	  b = (p[i+1 + nx*(j+1)] + p[i + nx*j] - p[i+1 + nx*j] - p[i + nx*(j+1)])*(dx*dy*dy)
	    - (r[i+1 + nx*j] - r[i + nx*j])*(dx*dy);
	  c = (q[i+1 + nx*(j+1)] + q[i + nx*j] - q[i+1 + nx*j] - q[i + nx*(j+1)])*(dx*dx*dy)
	    - (r[i + nx*(j+1)] - r[i + nx*j])*(dx*dy);
	  d = (r[i+1 + nx*(j+1)] + r[i+nx*j] - r[i+1 + nx*j] - r[i + nx*(j+1)])*(dx*dy);

	  C[k+10] =  9.0*a - 3.0*b - 3.0*c + d;
	  C[k+14] =(-6.0*a + 2.0*b + 3.0*c - d)*dy;
	  C[k+11] =(-6.0*a + 3.0*b + 2.0*c - d)*dx;
	  C[k+15] =( 4.0*a - 2.0*b - 2.0*c + d)*dx*dy;
	}
    }
}

/**
 * nsp_bicubic_subspline:
 * @x: (input) first coordinates of the grid points
 * @y: (input) second coordinates of the grid points
 * @u: (input) array of size nx x ny, u(i,j)=u[i+nx*j] value u(x,y) at the grid point (@x[i],@y[j])
 * @nx: (input) size of the grid in x
 * @ny: (input) size of the grid in y
 * @C: (output) array of size 4 x 4 x (nx-1) x (ny-1) (must be preallocated)
 * @type: (input) type of bicubic subspline to compute
 * 
 * compute a bicubic subspline s (s is only one time continuously differentiable) 
 * which interpolates the @u values on the grid defined by @x and @y. (s(x[i],y[j]) 
 * must be equal to u(i,j)=u[i*nx*j]). The subspline is completly defined with
 * the triplet (@x,@y,@C) and could be evaluated at some points with #nsp_eval_bicubic.
 * See #coef_bicubic for detail about the bicubic patch coef @C.
 * 
 * return %OK or %FAIL (%FAIL when allocation of works arrays fail or when type is
 * not good)  
 **/
int nsp_bicubic_subspline(double *x, double *y, double *u, int nx, int ny, double *C, int type)
{
  double *p=NULL, *q=NULL, *r=NULL;
  int i, j;

  p = nsp_alloc_doubles(nx*ny);
  q = nsp_alloc_doubles(nx*ny);
  r = nsp_alloc_doubles(nx*ny);

  if ( p == NULL || q == NULL || r == NULL )
    {
      FREE(p); FREE(q); FREE(r);
      return FAIL;
    }

  if (type == MONOTONE)   /*  approximate the derivatives with nsp_dpchim */
    {
      /* p = du/dx */
      for ( j = 0 ; j < ny ; j++ )
	nsp_dpchim(x, &u[nx*j], &p[nx*j], nx, 1);

      /* q = du/dy */
      for ( i = 0 ; i < nx ; i++ )
	nsp_dpchim(y, &u[i], &q[i], ny, nx);

      /* r = d2 u/ dx dy  approchee via  dq / dx */
      for ( j = 0 ; j < ny ; j++ )
	nsp_dpchim(x, &q[nx*j], &r[nx*j], nx, 1);
    }

  else if (type == FAST  ||  type == FAST_PERIODIC) /*  approximate the derivatives with nsp_derivd */
    {
      /* p = du/dx  */
      for ( j = 0 ; j < ny ; j++ )
	nsp_derivd(x, &u[nx*j], &p[nx*j], nx, 1, type);

      /* q = du/dy */
      for ( i = 0 ; i < nx ; i++ )
	nsp_derivd(y, &u[i], &q[i], ny, nx, type);

      /* r = d2 u/ dx dy  approchee via  dq / dx */
      for ( j = 0 ; j < ny ; j++ )
	nsp_derivd(x, &q[nx*j], &r[nx*j], nx, 1, type);
    }

  else  /* not the good type */
    {
      FREE(p); FREE(q); FREE(r);
      return FAIL;
    }

  /* compute polynomial coefficients in the basis (x-x(i))^k (y-y(j))^l  0<= k,l <= 3
   * for fast evaluation with Horner 's scheme */
  coef_bicubic(u, p, q, r, x, y, nx, ny, C);

  return OK;
}

/**
 * nsp_bicubic_spline:
 * @x: (input) first coordinates of the grid points
 * @y: (input) second coordinates of the grid points
 * @u: (input) array of size nx x ny, u(i,j)=u[i+nx*j] value u(x,y) at the grid point (@x[i],@y[j])
 * @nx: (input) size of the grid in x
 * @ny: (input) size of the grid in y
 * @C: (output) array of size 4 x 4 x (nx-1) x (ny-1) (must be preallocated)
 * @type: (input) type of bicubic spline to compute
 * 
 * compute a bicubic spline s (s is two time continuously differentiable) 
 * which interpolates the @u values on the grid defined by @x and @y. (s(x[i],y[j]) 
 * must be equal to u(i,j)=u[i*nx*j]). The spline is completly defined with
 * the triplet (@x,@y,@C) and could be evaluated at some points with #nsp_eval_bicubic.
 * See #coef_bicubic for detail about the bicubic patch coef @C.
 * 
 * return %OK or %FAIL (%FAIL when allocation of works arrays fail)
 *
 **/
int nsp_bicubic_spline(double *x, double *y, double *u, int nx, int ny, double *C, int type)
{
  double *p=NULL, *q=NULL, *r=NULL, *u_temp=NULL, *d=NULL, *Ad=NULL, *Asd=NULL, *qdu=NULL, *ll=NULL;
  int i, j, k, n = Max(nx,ny);

  p = nsp_alloc_doubles(nx*ny);
  q = nsp_alloc_doubles(nx*ny);
  r = nsp_alloc_doubles(nx*ny);
  u_temp = nsp_alloc_doubles(ny);
  d = nsp_alloc_doubles(ny);
  Ad = nsp_alloc_doubles(n);
  Asd = nsp_alloc_doubles(n-1);
  qdu = nsp_alloc_doubles(n-1);
  if (type == PERIODIC)
    ll = nsp_alloc_doubles(n-1);

  if ( p == NULL || q == NULL || r == NULL || u_temp == NULL || d == NULL || Ad == NULL || Asd == NULL || qdu == NULL || (type==PERIODIC && ll==NULL) )
    {
      FREE(p); FREE(q); FREE(r); FREE(u_temp); FREE(d); FREE(Ad); FREE(Asd); FREE(qdu); FREE(ll);
      return FAIL;
    }

  /* compute du/dx */
  for ( j = 0 ; j < ny ; j++ )
    nsp_cubic_spline(x, &(u[nx*j]), &(p[nx*j]), nx, type, Ad, Asd, qdu, ll);

  /* compute du/dy */
  for ( i = 0 ; i < nx ; i++ )
    {
      for ( j = 0, k = i; j < ny ; j++, k+=nx ) u_temp[j] = u[k];
      nsp_cubic_spline(y, u_temp, d, ny, type, Ad, Asd, qdu, ll);
      for ( j = 0, k = i ; j < ny ; j++, k+=nx ) q[k] = d[j];
    }

  /*compute ddu/dxdy */
  nsp_cubic_spline(x, q,  r,  nx, type, Ad, Asd, qdu, ll);
  nsp_cubic_spline(x, &(q[nx*(ny-1)]), &(r[nx*(ny-1)]), nx, type, Ad, Asd, qdu, ll);
  for ( i = 0 ; i < nx ; i++ )
    {
      for ( j = 0, k = i ; j < ny ; j++, k+=nx ) u_temp[j] = p[k];
      d[0] = r[i]; d[ny-1] = r[i + nx*(ny-1)];
      nsp_cubic_spline(y, u_temp, d, ny, CLAMPED, Ad, Asd, qdu, ll);
      for ( j = 1, k = i+nx ; j < ny-1 ; j++, k+=nx ) r[k] = d[j];
    }

  /* compute polynomial coefficients in the basis (x-x(i))^k (y-y(j))^l  0<= k,l <= 3
   * for fast evaluation with Horner 's scheme */
  coef_bicubic(u, p, q, r, x, y, nx, ny, C);

  return OK;
}

static double nsp_eval_bicubic_patch(double xx, double yy, double xk, double yk, double *Ck)
{
  double dx, dy, u;
  int i;

  dx = xx - xk;
  dy = yy - yk;

  u = 0.0;
  for ( i = 12 ; i > -1 ; i-=4 )
    u = Ck[i] + dx*(Ck[i+1] + dx*(Ck[i+2] + dx*Ck[i+3])) + u*dy;

  return u;
}

static void nsp_eval_bicubic_patch_with_grad(double xx, double yy, double xk, double yk, double *Ck, 
					     double *U, double *DUDX, double *DUDY)
{
  double dx, dy, u, dudx, dudy;
  int i, j;

  dx = xx - xk;
  dy = yy - yk;

  u = 0.0; dudx = 0.0; dudy = 0.0;
  for ( j = 3, i = 12 ; j >= 0 ; j--, i-=4 )
    {
      u = Ck[i] + dx*(Ck[i+1] + dx*(Ck[i+2] + dx*Ck[i+3])) + u*dy;
      dudx = Ck[i+1] + dx*(2.0*Ck[i+2] + dx*3.0*Ck[i+3]) + dudx*dy;
      dudy = Ck[j+4] + dy*(2.0*Ck[j+8] + dy*3.0*Ck[j+12]) + dudy*dx;
    }

  *U = u; *DUDX = dudx; *DUDY = dudy;
}
        
     
/**
 * nsp_eval_bicubic:
 * @x: (input) first coordinates of the grid points
 * @y: (input) second coordinates of the grid points
 * @C: (input) array of size 4 x 4 x (nx-1) x (ny-1)
 * @nx: (input) size of the grid in x
 * @ny: (input) size of the grid in y
 * @x_eval: (input) array of size @m  
 * @y_eval: (input) array of size @m
 * @z_eval: (output) array of size @m, z_eval[k] will be the value of the bicubic at the point (x_eval[k],y_eval[k]).
 * @dzdx_eval: (output) if not NULL must be an array of size @m
 * @dzdy_eval: (output) if not NULL must be an array of size @m
 * @m: (input) length of arrays @x_eval and @y_eval, number of points where the bicubic must be evaluated
 * @outmode: (input) set the behavior to evaluate the bicubic outside the grid
 *
 * evaluates the bicubic defined by the triplet  (@x,@y,@C) at the points (x_eval,y_eval). If @dzdx_eval is
 * not NULL it is assumed that @dzdx_eval and @dzdy_eval are arrays of size @m and the first derivatives
 * at the (x_eval,y_eval) points are also computed. The bicubic (@x,@y,@C) could be build to interpolate
 * grid values with either #nsp_bicubic_spline or  #nsp_bicubic_subspline.
 *
 **/
void nsp_eval_bicubic(double *x, double *y, double *C, int nx, int ny, double *x_eval, double *y_eval, 
		      double *z_eval, double *dzdx_eval, double *dzdy_eval, int m, int outmode)
{
  double xx, yy, Nan=0.0/0.0;
  int k, i, j, change_dzdx, change_dzdy, without_grad = dzdx_eval==NULL;
  int stride = nx-1;

  i = -1;
  j = -1;
  for ( k = 0 ; k < m ; k++ )
    {
      xx = x_eval[k];
      fast_int_search(xx, x, nx, &i);  /* recompute i only if necessary */ 
      yy = y_eval[k];
      fast_int_search(yy, y, ny, &j);  /* recompute j only if necessary */ 
 
      if (i != -1  &&  j != -1)
	{
	  if ( without_grad )
	    z_eval[k] = nsp_eval_bicubic_patch(xx, yy, x[i], y[j], &C[16*(i+stride*j)]);
	  else
	    nsp_eval_bicubic_patch_with_grad(xx, yy, x[i], y[j], &C[16*(i+stride*j)],
					     &z_eval[k],&dzdx_eval[k], &dzdy_eval[k]);
	}
      else if ( outmode == BY_NAN  ||  ISNAN(xx) || ISNAN(yy) )
	{
	  z_eval[k] = Nan;
          if ( ! without_grad ) { dzdx_eval[k] = Nan; dzdy_eval[k] = Nan; }
	}
      else if (outmode == BY_ZERO )
	{
	  z_eval[k] = 0.0 ; 
	  if ( ! without_grad ) { dzdx_eval[k] = 0.0; dzdy_eval[k] = 0.0; }
	}
      else if (outmode == PERIODIC)
	{
	  if (i == -1) coord_by_periodicity(&xx, x, nx, &i);
	  if (j == -1) coord_by_periodicity(&yy, y, ny, &j);
	  if ( without_grad )
	    z_eval[k] = nsp_eval_bicubic_patch(xx, yy, x[i], y[j], &C[16*(i+stride*j)]);
	  else
	    nsp_eval_bicubic_patch_with_grad(xx, yy, x[i], y[j], &C[16*(i+stride*j)],
					     &z_eval[k],&dzdx_eval[k], &dzdy_eval[k]);
	}
      else if (outmode == C0)
	{
	  change_dzdx = 0; change_dzdy = 0;
	  if (i == -1) 
	    { 
	      if ( xx < x[0] ) { xx = x[0]; i = 0;} else { xx = x[nx-1]; i = nx-2;}
	      change_dzdx = 1;
	    }
	  if (j == -1)
	    { 
	      if ( yy < y[0] ) { yy = y[0]; j = 0;} else { yy = y[ny-1]; j = ny-2;}
	      change_dzdy = 1;
	    }

	  if ( without_grad )
	    z_eval[k] = nsp_eval_bicubic_patch(xx, yy, x[i], y[j], &C[16*(i+stride*j)]);
	  else
	    {
	      nsp_eval_bicubic_patch_with_grad(xx, yy, x[i], y[j], &C[16*(i+stride*j)],
					       &z_eval[k],&dzdx_eval[k], &dzdy_eval[k]);
	      if (change_dzdx) dzdx_eval[k] = 0.0;
	      if (change_dzdy) dzdy_eval[k] = 0.0;
	    }
	}
      else if (outmode == NATURAL)
	{
	  if (i == -1) { if ( xx < x[0] ) i = 0; else i = nx-2; }
	  if (j == -1) { if ( yy < y[0] ) j = 0; else j = ny-2; }
	  if ( without_grad )
	    z_eval[k] = nsp_eval_bicubic_patch(xx, yy, x[i], y[j], &C[16*(i+stride*j)]);
	  else
	    nsp_eval_bicubic_patch_with_grad(xx, yy, x[i], y[j], &C[16*(i+stride*j)],
					     &z_eval[k],&dzdx_eval[k], &dzdy_eval[k]);
	}
    }
}

/* an utility routine for nsp_intg_spline */
static double intg_eval_hermite(double t, double *x, double *y, double *d)
{
  double dx, Ll, Lr, Ll2, Lr2, Hl, Hr, Kl, Kr;
  dx = x[1] - x[0];
  Ll = (x[1]-t)/dx; Lr = (t-x[0])/dx;
  Ll2 = Ll*Ll; Lr2 = Lr*Lr;
  Hl = (1.0+2.0*Lr)*Ll2; Hr = (1.0+2.0*Ll)*Lr2;
  Kl = dx*Lr*Ll2       ; Kr = -dx*Ll*Lr2;
  return y[0]*Hl + y[1]*Hr + d[0]*Kl + d[1]*Kr;
}

/* an utility routine for nsp_intg_spline */
static double intg_spline_with_gauss(double a, double b, int k, double *x, double *y, double *d)
{
  double xi = 1.0/sqrt(3.0),t1, t2;
  t1 = 0.5*((1.0+xi)*a + (1.0-xi)*b);
  t2 = 0.5*((1.0-xi)*a + (1.0+xi)*b);
  return 0.5*(b-a)*(  intg_eval_hermite(t1, &(x[k]), &(y[k]),  &(d[k]))
		    + intg_eval_hermite(t2, &(x[k]), &(y[k]),  &(d[k])) );
}

/* an utility routine for nsp_intg_spline */
static double intg_spline_outside(double a, double b, double *x, double *y, double *d, int n, int outmode)
{
  double I, T, delta_x, K;
  switch ( outmode )
    {
    case BY_ZERO:
      I = 0.0;
      break;

    case BY_NAN:
      I = 0.0/0.0;
      break;

    case C0:
      if ( b <= x[0] )
	I = (b-a)*y[0];
      else
	I = (b-a)*y[n-1];
      break;

    case LINEAR:
      if ( b <= x[0] )
	I = (b-a)*(y[0] + 0.5*d[0]*(a+b-2.0*x[0]));
      else
	I = (b-a)*(y[n-1] + 0.5*d[n-1]*(a+b-2.0*x[n-1]));
      break;

    case NATURAL:
      if ( b <= x[0] )
	I = intg_spline_with_gauss(a, b, 0, x, y, d);
      else
	I = intg_spline_with_gauss(a, b, n-2, x, y, d);
      break;

    case PERIODIC:
      T = x[n-1] - x[0];
      K = floor((b-a)/T);
      if ( K >= 1.0 )
	I = K*nsp_intg_spline(x[0],x[n-1],x, y, d, n, 0);
      else
	I = 0.0;
      delta_x = (b-a) - K*T;
      if ( delta_x > 0.0 )
	{
	  if ( a < x[0] )
	    {
	      a += T*ceil((x[0]-a)/T); b = a + delta_x;
	      if ( b > x[n-1] )
		I +=    nsp_intg_spline(a,x[n-1],x,y,d,n,0) 
		     +  nsp_intg_spline(x[0],x[0]+(b-x[n-1]),x,y,d,n,0); 
	      else
		I += nsp_intg_spline(a,b,x,y,d,n,0);
	    }
	  else
	    {
	      b -= T*ceil((b-x[n-1])/T); a = b - delta_x;
	      if ( a < x[0] )
		I +=    nsp_intg_spline(x[n-1]-(x[0]-a),x[n-1],x,y,d,n,0) 
		     +  nsp_intg_spline(x[0],b,x,y,d,n,0); 
	      else
		I += nsp_intg_spline(a,b,x,y,d,n,0);
	    }
	}
      break;

    default:
      I = 0.0;
      break;
    }
  return I;
}

/**
 * nsp_intg_spline:
 * @a: (input) double scalar (should be finite) a and b define the integration interval
 * @b: (input) double scalar (should be finite)  
 * @x: (input) breakpoints of the spline or subspline
 * @y: (input) values of the spline or subspline at the breakpoint @x
 * @d: (input) values of the derivative of the spline or subpline at the breakpoint @x
 * @n: (input) number of breakpoints
 * @outmode: (input) defines the spline or subspline outside [x[0], x[n-1]]
 * 
 * compute  \int_a^b s(t) dt, where s is a cubic spline or sub-spline defined
 * by the triplet (x, y, d) (the Hermite form). 
 * a and b could be taken outside the spline domain [x[0],x[n-1]], in this case
 * the definition of the spline depends upon the parameter outmode.
 * 
 **/
double nsp_intg_spline(double a, double b, double *x, double *y, double *d, int n, int outmode) 
{
  double I = 0.0, s = 1.0;
  int k;
  
  if ( a == b) return 0.0;

  if ( b < a ) 
    {
      double temp = a; 
      a = b; b = temp; s = -1.0;
    }

  if ( a < x[0] )
    {
      if ( b <= x[0] )
	return s*intg_spline_outside(a, b, x, y, d, n, outmode);
      else
	{
	  I = intg_spline_outside(a, x[0], x, y, d, n, outmode);
	  k = 0;
	}
    }
  else if ( a >= x[n-1] )
    {
      return s*intg_spline_outside(a, b, x, y, d, n, outmode);
    }
  else
    {
      k = isearch(a, x, n);
      if ( b <= x[k+1] )
	return s*intg_spline_with_gauss(a, b, k, x, y, d);
      else
	{
	  I = intg_spline_with_gauss(a, x[k+1], k, x, y, d);
	  k++;
	}
    }

  while ( k <= n-2 && b >= x[k+1] )
    {
      double dx = x[k+1] - x[k];
      I += dx*( 0.5*(y[k]+y[k+1]) + dx*(d[k]-d[k+1])/12.0 );
      k++;
    }

  if ( k <= n-2 )
    {
      if ( b > x[k] )
	I += intg_spline_with_gauss(x[k], b, k, x, y, d);
    }
  else if ( b > x[n-1] )
    I += intg_spline_outside(x[n-1], b, x, y, d, n, outmode);

  return s*I;
}
