/* 
 * some interpolation routines for Nsp 
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
#include <nsp/math.h>
#include "nsp/approx.h"

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


static void tri_diag_solve(double *d, double *l, double *b, int n)
{
  /*
   *     PURPOSE           
   *        solve a linear system Ax = b where A is symetric tridiagonal 
   *        (and supposed positive definite) using a LDL^t factorization
   *        
   *     PARAMETERS
   *        d[0..n-1] : on input the diagonal of A (overwritten with
   *                    the diagonal of the (diagonal) matrix D)
   *        l[0..n-2] : on input the sub-diagonal of A (overwritten
   *                    with the sub-diagonal of L)
   *        b[0..n-1] : on input the rhs
   *                    on output the solution x   
   *        n         : the dimension 
   *
   *     CAUTION 
   *        no zero pivot detection (A is supposed positive definite)
   */
  
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

static void nearly_tri_diag_solve(double *d, double *lsd, double *lll, double *b, int n)
{
  /*
   *     PURPOSE
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
   *     PARAMETERS
   *        d[0..n-1]   : on input the diagonal of A (overwritten with
   *                      the diagonal of the (diagonal) matrix D)
   *        lsd[0..n-3] : on input the sub-diagonal of A (without  A(n-1,n-2)) 
   *                      (overwritten with the sub-diagonal of L (without  L(n-1,n-2))
   *        lll[0..n-2] : on input the last line of A (without A(n-1,n-1))
   *                      (overwritten with the last line of L (without L(n-1,n-1)))
   *        b[0..n-1]   : on input the rhs b, on output the solution x
   *        n           : the dimension 
   *
   *     CAUTION 
   *        A is supposed to be positive definite so no zero pivot detection is done.
   */
      
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


void derivd(double *x, double *u, double *du, int n, int inc, int type)
{
  /*
   *     PURPOSE
   *        given functions values u[i] at points x[i],  i = 0, ..., n-1
   *        this subroutine computes approximations du[i] of the derivative
   *        at the points x[i]. 
   *
   *     METHOD
   *        For i in [1,n-2], the "centered" formula of order 2 is used :
   *            d(i) = derivative at x(i) of the interpolation polynomial
   *                   of the points {(x(j),u(j)), j in [i-1,i+1]}
   *
   *         For i=0 and n-1, if type = FAST_PERIODIC  (in which case u[n-1]=u[0]) then
   *         the previus "centered" formula is also used; else (type = FAST), d[0]
   *         is the derivative at x[0] of the interpolation polynomial of
   *         {(x(j),u(j)), j in [0,2]} and the same method is used for d[n-1]
   *
   *     ARGUMENTS
   *      inputs :
   *         n       integer : number of point (n >= 2)
   *         x, u    the n points, x must be in strict increasing order
   *         type    integer : FAST (the function is non periodic) or FAST_PERIODIC
   *                 (the function is periodic), in this last case u[n-1] must be equal to u[0])
   *         inc     integer : to deal easily with 2d applications, u[i] is in fact
   *                 u[inc*i] 
   *      outputs :
   *         du      the derivatives in each x[i] i = 0..n-1
   *
   *    NOTES
   *         this routine requires (i)   n >= 2
   *                               (ii)  strict increasing abscissae x[i]
   *                               (iii) u[0]=u[n-1] if type = FAST_PERIODIC
   *         ALL THESE CONDITIONS MUST BE TESTED IN THE CALLING CODE
   *
   *     AUTHOR
   *        Bruno Pincon
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
	  dx_r = x[(i+1)*inc] - x[i*inc];
	  du_r = (u[(i+1)*inc] - u[i*inc])/dx_r;
	  w_l  = dx_r/(dx_l + dx_r);
	  w_r = 1.0 - w_l;
	  du[i*inc] = w_l*du_l + w_r*du_r;
        }
      du[(n-1)*inc] = du[0];
    }

  else  /* type == FAST */
    {
      dx_l = x[inc] - x[0];
      du_l =(u[inc] - u[0])/dx_l;
      dx_r = x[2*inc] - x[inc];
      du_r = (u[2*inc] - u[inc])/dx_r;
      w_l = dx_r/(dx_l + dx_r);
      w_r = 1.0 - w_l;
      du[0] = (1.0 + w_r)*du_l - w_r*du_r;
      du[inc] = w_l*du_l + w_r*du_r;
      for ( i = 2 ; i < n-1 ; i++ )
	{
	  dx_l = dx_r;
	  du_l  = du_r;
	  dx_r = x[(i+1)*inc] - x[i*inc];
	  du_r  = (u[(i+1)*inc] - u[i*inc])/dx_r;
	  w_l  = dx_r/(dx_l + dx_r);
	  w_r = 1.0 - w_l;
	  du[i*inc] = w_l*du_l + w_r*du_r;
	}
      du[(n-1)*inc] = (1.0 + w_l)*du_r - w_l*du_l;
    }
}


void cubic_spline(double *x, double *y, double *d, int n, int type, 
		  double *A_d, double *A_sd, double *qdy, double *lll)
{
  /*
   *     PURPOSE
   *        computes a cubic spline interpolation function
   *        in Hermite form (ie computes the derivatives d(i) of the
   *        spline in each interpolation point (x(i), y(i)))
   *
   *     ARGUMENTS
   *      inputs :
   *         n       number of interpolation points (n >= 3)
   *         x, y    the n interpolation points, x must be in strict increasing order
   *         type    type of the spline:  
   *                 * NOT_A_KNOT spline with:
   *                                 s'''(x(2)-) = s'''(x(2)+) 
   *                             and s'''(x(n-1)-) = s'''(x(n-1)+)
   *                 * NATURAL spline with:
   *                                 s''(x1) = 0
   *                             and s''(xn) = 0
   *                 * CLAMPED spline with end point derivative provided 
   *                            (d[0] and d[n-1] given)
   *
   *                 * PERIODIC
   *      outputs :
   *         d    the derivatives in each x[i] i = 0..n-1
   *
   *      work arrays :
   *         A_d(0..n-1), A_sd(0..n-2), qdy(0..n-2)
   *         lll[0..n-2]  used only in the periodic case
   *
   *    NOTES
   *         this routine requires (i)   n >= 3 (for natural) n >=4 (for not_a_knot) 
   *                               (ii)  strict increasing abscissae x(i)
   *                               (iii) y[0] = y[n-1] in the periodic case
   *         THESE CONDITIONS MUST BE TESTED IN THE CALLING CODE
   */

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
      derivd(x, y, d, n, 1, FAST);
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

static int dpchst(double arg1, double arg2)
{
  /* from Slatec */
  /* dpchst:  dpchip sign-testing routine.
      returns:
        -1. if arg1 and arg2 are of opposite sign.
         0. if either argument is zero.
        +1. if arg1 and arg2 are of the same sign.
  */
  if ( arg1 == 0.0  ||  arg2 == 0.0 )
    return 0;
  else if ( (arg1 > 0.0 && arg2 > 0.0)  ||  (arg1 < 0.0 && arg2 < 0.0) )
    return 1;
  else
    return -1;
}

void dpchim(double *x, double *u, double *d, int n, int inc)
{

  /*  an adaptation in C of the Slatec dpchim routine */
  /*  DPCHIM:  Piecewise Cubic Hermite Interpolation to Monotone data */

  int i;
  double del1, del2, dmax, dmin, drat1, drat2, dsave,
    h1, h2, hsum, hsumt3, w1, w2;

  h1 = x[1] - x[0];
  del1 = (u[inc] - u[0])/h1;
  dsave = del1;

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
	  dsave = del2;
	  break;
	case 0:
	  d[i*inc] = 0.0;
	  if (del2 != 0.0)
	    dsave = del2;
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

void eval_piecewise_hermite(double *t, double *st, double *dst, double *d2st, 
			    double *d3st, int m, double *x, double *y, double *d, 
			    int n, int outmode)
{
  /*
   *     PURPOSE
   *        evaluation at the abscissae t[0..m-1] of the piecewise hermite function
   *        defined by x[0..n-1], y[0..n-1], d[0..n-1] (d being the derivatives at the
   *        x[i]) together with its derivative, second derivative and third derivative
   *        
   *        outmode defines what return in case t[j] not in [x[0], x[n-1]]
   */

  int i, j, i_old=-1;;
  double tt, c2, c3;
  double Nan = (2*DBL_MAX)*0.0;

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

