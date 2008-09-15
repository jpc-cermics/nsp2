/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2008-     Bruno Pinçon Esial/Iecn
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

#include "nsp/object.h"
#include "nsp/cnumeric.h"
#include "nsp/spmf.h"

#define NEW 1

/*
 * a set of C functions which can be used if res =x 
 * ex nsp_cos_c(&x,&x);
 */

/**
 * nsp_abs_c:
 * @x: a double complex (#doubleC) 
 * 
 * absolute value of a complex number x=a+ib sqrt(a^2+b^2).
 * 
 * Returns: a double
 */

double nsp_abs_c(const doubleC *x)
{
  return hypot (x->r, x->i);
}

/**
 * nsp_arg_c:
 * @x: a double complex (#doubleC) 
 * 
 * calculates the arc tangent of @x->i and @x->r
 * using atan2 
 * 
 * Returns: a double 
 **/

double nsp_arg_c(const doubleC *x)
{
  return atan2 (x->i, x->r);
}

/**
 * nsp_polar_c:
 * @r: a double 
 * @t: a double 
 * @res: a pointer to a double complex 
 * 
 * res a double complex built from its polar coordinates 
 * (r,t)
 **/

void nsp_polar_c(double r, double t, doubleC *res)
{
  res->r=r * cos (t);
  res->i=r * sin (t);
}

/**
 * nsp_conj_c:
 * @x: a  double complex 
 * @res: a pointer to a double complex 
 * 
 * res is the complex conjugate of x
 * 
 **/

void nsp_conj_c(const doubleC *x, doubleC *res)
{
  res->r = x->r; res->i = - x->i;
}

/**
 * nsp_norm_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 * 
 * Return value: 
 **/
double nsp_norm_c(const doubleC *x, doubleC *res)
{
  return x->r * x->r + x->i * x->i;
}

/**
 * nsp_cos_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * The complex cosine of x returned in res 
 *
 **/

void nsp_cos_c(const doubleC *x, doubleC *res)
{
  double loc;
  loc = x->r;
  res->r=cos (x->r) * cosh (x->i);
  res->i= - sin (loc) * sinh (x->i);
}

/**
 * nsp_cosh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/

void nsp_cosh_c(const doubleC *x, doubleC *res)
{
  double loc;
  loc = x->r;
  res->r=cosh (x->r) * cos (x->i);
  res->i=sinh (loc) * sin (x->i);
}

/**
 * nsp_exp_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/

void nsp_exp_c(const doubleC *x, doubleC *res)
{
  nsp_polar_c(exp (x->r), x->i,res);
}


#ifdef NEW

/**
 * nsp_log_c:
 * @x: a pointer to a double complex  
 * @y: a pointer to a double complex  
 * 
 * computes the logarithm of a complex number
 * @y = yr + i yi = log(@x), @x = xr + i xi 
 * adapted with some modifications from Hull, Fairgrieve, Tang, 
 *  "Implementing Complex Elementary Functions Using Exception Handling", 
 *  ACM TOMS, Vol. 20 (1994), pp 215-244
 * yr = log(|@x|) = various formulae depending on @x value. 
 * yi = Arg(@x) = atan2(xi, xr)
 * Author: Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr>.
 *
 */

void nsp_log_c(const doubleC *x, doubleC *y)
{
  double a = x->r, b = x->i, t;
  double const R2 = 1.41421356237309504,  /* sqrt(2) */ 
    LSUP = 9.48075190810917589e+153, /* sqrt(0.5*DBL_MAX) */
    LINF = 1.49166814624004135e-154; /* sqrt(dlamch('U')) */  

  /* compute imaginary part */
  y->i = atan2(b,a);

  /* compute real part */
  a = fabs(a); b = fabs(b);
  /* order a and b such that 0 <= b <= a */
  if ( b > a ) { t = b; b = a; a = t; }

  if ( 0.5 <= a  &&  a <= R2 )
    y->r = 0.5*nsp_log1p( (a-1.0)*(a+1.0) + b*b );
  else if ( LINF <= b  &&  a <= LSUP ) 
    /*no overflow or underflow can occur in computing a*a + b*b */
    y->r = 0.5*log(a*a + b*b);
  else if ( a > DBL_MAX ) 
    /* overflow */
    y->r = a;
  else
    {
      t = nsp_hypot(a,b);
      if ( t <= DBL_MAX )
	y->r = log(t);
      else
	{
	  /*handle rare spurious overflow with: */
	  t = b/a;
	  y->r = log(a) + 0.5*nsp_log1p(t*t);
	}
    }
}
#else

/**
 * nsp_log_c:
 * @x: a pointer double complex  
 * @res: a pointer to a double complex  
 * 
 **/

void nsp_log_c(const doubleC *x, doubleC *res)
{
  double loc;
  loc=log (nsp_abs_c(x));
  res->i=nsp_arg_c(x);
  res->r=loc;
}
#endif

/**
 * nsp_pow_cc:
 * @x: a  double complex  
 * @y: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes in @res the value of @x raised to the power of @y.
 **/

void nsp_pow_cc(const doubleC *x,const  doubleC *y, doubleC *res)
{
  double logr = log (nsp_abs_c(x));
  double t =nsp_arg_c(x);
  nsp_polar_c( (exp (logr * y->r - y->i * t)),
	       (y->i * logr + y->r * t), res);
}

/**
 * nsp_pow_cd:
 * @x: a  double complex  
 * @y: a double 
 * @res: a pointer to a double complex  
 * 
 * computes in @res the value of @x raised to the power of @y. 
 *
 **/
void nsp_pow_cd(const  doubleC *x, double y, doubleC *res)
{
  nsp_log_c(x,res);
  res->r=res->r*y;
  res->i=res->i*y;
  nsp_exp_c(res,res);
}

/**
 * nsp_pow_cd_or_ci:
 * @x: a  double complex  
 * @y: a double 
 * @res: a pointer to a double complex  
 * 
 * computes in @res the value of @x raised to the power of @y.
 **/

void nsp_pow_cd_or_ci(const  doubleC *x, double y, doubleC *res)
{
  if ( floor(y) == y && fabs(y) <= 65536.0 ) 
    nsp_pow_ci(x, (int) y, res);
  else
    nsp_pow_cd(x, y, res);
}

/**
 * nsp_pow_dc:
 * @x: a double
 * @y: a  double complex 
 * @res: a pointer to a double complex  
 * 
 * computes in @res the value of @x raised to the power of @y.
 **/

void nsp_pow_dc(double x, const doubleC *y, doubleC *res)
{
  double loc;
  loc=log(x);
  res->r=loc*y->r;
  res->i=loc*y->i;
  nsp_exp_c(res,res);
}

/**
 * nsp_pow_di:
 * @x: a  double
 * @p: an int 
 * 
 * computes in @res the value of @x raised to the power of @y.
 * 
 **/

double nsp_pow_di(double x, int p)
{
  if ( p < 0 )
    {
      if ( p > INT_MIN ) /* p > -2^31 with 32 bits arithmetic */
	return 1.0/nsp_pow_di(x, -p);
      else 
	{   /*  1.0/nsp_pow_di(x, -p) don't work because -(-2^31)   */
            /*  = 2^31 mathematically but as MAXINT = 2^31-1 we get */
            /*  in fact -2^31 with the usual int arithmetic         */
	  int pp = p + 1;
	  return 1.0/(nsp_pow_di(x,-pp)*x);
	}
    }
  else if ( p == 0 )
    return isnan(x) ? x : 1.0;
  else
    {
      double z = 1.0;
      while ( p > 1 )
	{
	  if ( p % 2 == 1 )
	    z *= x;
	  x *= x;
	  p = p/2;
	}
      return z*x;
    }
} 

/**
 * nsp_pow_ii:
 * @p: an int 
 * @n: an int 
 * 
 * computes value of @p raised to the power of @n.
 * 
 * Returns: an int 
 **/

/* from slatec */

int nsp_pow_ii(int x, int p)
{
  if ( p < 0 )
    {
      if ( p > INT_MIN ) /* p > -2^31 with 32 bits arithmetic */
	return 1.0/nsp_pow_ii(x, -p);
      else 
	{   
	  /*  1.0/nsp_pow_di(x, -p) don't work because -(-2^31)   */
	  /*  = 2^31 mathematically but as MAXINT = 2^31-1 we get */
	  /*  in fact -2^31 with the usual int arithmetic         */
	  int pp = p + 1;
	  return 1.0/(nsp_pow_di(x,-pp)*x);
	}
    }
  else if ( p == 0 )
    return 1.0;
  else
    {
      int z = 1;
      while ( p > 1 )
	{
	  if ( p % 2 == 1 )
	    z *= x;
	  x *= x;
	  p = p/2;
	}
      return z*x;
    }
} 


/**
 * nsp_pow_ci:
 * @x: a #doubleC
 * @p: an integer 
 * @y: a #doubleC
 *
 * computes in @y the value of @x raised to the power of @p (can be used with x=x^p).
 *  
 **/

void nsp_pow_ci(const doubleC *x, int p, doubleC *y)
{
  if ( p < 0 )
    {
      nsp_pow_ci(x, -p, y);
      nsp_div_dc(1.0, y, y);
    }
  else
    {
      doubleC z = {1.0,0.0};
      if ( p == 0 )
	*y = z;
      else
	{
	  *y = *x;
	  while ( p > 1 )
	    {
	      if ( p & 1 )  /*  p % 2 == 1 */
		nsp_prod_c(&z, y);
	      nsp_prod_c(y, y);
	      p >>= 1;      /*  p /= 2;    */
	    }
	  nsp_prod_c(y, &z);
	}
    }
}



/**
 * nsp_sin_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes the sin of a complex @x.
 * 
 **/

void nsp_sin_c(const doubleC *x, doubleC *res)
{
  double loc=x->r;
  res->r=sin (x->r) * cosh (x->i);
  res->i=cos (loc) * sinh (x->i);
}

/**
 * nsp_sinh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes the hyperbolic sin of a complex @x.
 **/

void nsp_sinh_c(const doubleC *x, doubleC *res)
{
  double loc=x->r;
  res->r=sinh (x->r) * cos (x->i);
  res->i=cosh (loc) * sin (x->i);
}

/**
 * nsp_div_cc:
 * @x: a  double complex  
 * @y: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes in @res the value of @x divided by @y.
 **/

void nsp_div_cc(const doubleC *x,const  doubleC *y, doubleC *res)
{
  double t,d,loc;
  if ( Abs (y->r) <= Abs (y->i) )
    {
      loc=x->r;
      t = y->r / y->i;
      d = y->i * (1 + t*t);
      res->r = (x->r * t + x->i) / d;
      res->i = (x->i * t - loc) / d;
    }
  else
    {
      loc=x->r;
      t = y->i / y->r;
      d = y->r * (1 + t*t);
      res->r = (x->r + x->i * t) / d;
      res->i = (x->i - loc * t) / d;
    }
}

/**
 * nsp_div_dc:
 * @x: a double
 * @y: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes in @res the value of @x divided by @y.
 **/

void nsp_div_dc(double x, const doubleC *y, doubleC *res)
{
  double t,d;
  if ( Abs (y->r) <= Abs (y->i) )
    {
      t = y->r / y->i;
      d = y->i * (1 + t*t);
      res->r = x * t / d;
      res->i = -x / d;
    }
  else
    {
      t = y->i / y->r;
      d = y->r * (1 + t*t);
      res->r = x / d;
      res->i = -x * t / d;
    }
}

#ifdef NEW
/**
 * nsp_sqrt_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes in @res the square root of @x.
 **/
void nsp_sqrt_c(const doubleC *x, doubleC *res)
{

/*     ALGORITHM
 *        essentially the classic one which consists in
 *        choosing the good formula such as avoid cancellation ; 
 *        Also rare spurious overflow are treated with a
 *        "manual" method. For some more "automated" methods
 *        (but currently difficult to implement in a portable
 *        way) see :
 *
 *          Hull, Fairgrieve, Tang,
 *          "Implementing Complex Elementary Functions Using
 *          Exception Handling", ACM TOMS, Vol. 20 (1994), pp 215-244
 *
 *        Noting y = sqrt(x)
 *
 *        for xr > 0 :                            
 *          yr = sqrt(2( xr + sqrt( xr^2 + xi^2)) )/ 2
 *          yi = xi / sqrt(2(xr + sqrt(xr^2 + xi^2)))
 *
 *        and for xr < 0 :                           
 *          yr = |xi| / sqrt( 2( -xr + sqrt( xr^2 + xi^2 )) )
 *          yi = sign(xi) sqrt(2(-xr + sqrt( xr^2 + xi^2))) / 2
 *     
 *        for xr = 0 use          
 *          yr = sqrt(0.5)*sqrt(|xi|)  when |xi| is such that 0.5*|xi| may underflow
 *             = sqrt(0.5*|xi|)        else
 *          yi = sign(xi) yr
 *
 *        Noting t = sqrt( 2( |xr| + sqrt( xr^2 + yr^2)) ) 
 *                 = sqrt( 2( |xr| + nsp_hypot(xr,xi) ) )
 *        it comes :
 *
 *          for xr > 0   |  for xr < 0
 *         --------------+---------------------
 *           yr = 0.5*t  |  yr =  |xi| / t
 *           yi = xi / t |  yi = sign(xi)*0.5* t
 *    
 *        as the function nsp_hypot must not underflow (and overflow only 
 *        if sqrt(x^2+y^2) > DBL_MAX) only spurious (rare) case of overflow 
 *        occurs in which case a scaling is done.
 */
  double a = x->r, b = x->i, t;
  double const brmin = 4.450147717014402766180465435e-308; /* 2*dlamch('u') */

  if ( a == 0.0 )   /*pure imaginary case */
    {
      if ( fabs(b) >= brmin )
	res->r = sqrt(0.5*fabs(b));
      else
	res->r = sqrt(0.5)*sqrt(fabs(b));
      res->i = b >= 0 ? res->r : -res->r;
    }
  else if ( fabs(a) <= DBL_MAX  &&  fabs(b) <= DBL_MAX ) /* standard case */
    {
      t = sqrt(2.0*(fabs(a) + nsp_hypot(a,b)));
      if ( t <= DBL_MAX )
	{
	  /* classic switch to get the stable formulae */
	  if ( a >= 0.0 )
	    {
	      res->r = 0.5*t; res->i = b/t;
	    }
	  else
	    {
	      res->r = fabs(b)/t;
	      res->i = b >= 0 ? 0.5*t : -0.5*t;
	    }
	}
      else
	{
	  /* handle spurious overflow by scaling a and b */
	  a /= 16.0; b /= 16.0;
	  t = sqrt(2.0*(fabs(a) + nsp_hypot(a,b)));
	  if ( a <= 0 )
	    {
	      res->r = 2.0*t; res->i = 4.0*b/t;
	    }
	  else
	    {
	      res->r = 4*fabs(b)/t;
	      res->i = b >= 0 ? 2.0*t : -2.0*t;
	    }
	}
    }
  else  /* special cases where a or b are +-oo or Nan */
    {
      /*
       *  The following is the treatment recommended by the C99 standard
       *  with the simplification of returning NaN + i NaN if the
       *  the real part or the imaginary part is NaN (C99 recommends
       *  something more complicated)
       */
      if ( isnan(a) || isnan(b) )
	{
	  /* got Nan + i Nan */
	  res->r = a + b; res->i = res->r;
	}
      else if ( fabs(b) > DBL_MAX )  
	/*  case x = a +- i oo -> result must be +oo +- i oo  for all a (finite or not) */
	{
	  res->r = fabs(b); res->i = b;
	}
      else if ( a < -DBL_MAX )
	/* here a is -Inf and b is finite */
	{
	  res->r = 0.0;
	  res->i = b >= 0 ? fabs(a) : -fabs(a);
	}
      else
	/* here a is +Inf and b is finite */
	{
	  res->r = a; res->i = 0.0;
	}
    }
}
#else
/**
 * nsp_sqrt_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes in @res the square root of @x.
 **/
void nsp_sqrt_c(const doubleC *x, doubleC *res)
{
  double r;
  r =nsp_abs_c(x);
  if (r == 0.0)
    {
      res->r=res->i = r;
    }
  else if (x->r > 0)
    {
      res->r = sqrt (0.5 * (r + x->r));
      res->i = x->i / res->r / 2;
    }
  else
    {
      double loc=x->i;
      res->i = sqrt (0.5 * (r - x->r));
      if (loc < 0) res->i = - res->i;
      res->r = loc / res->i / 2;
    }
}
#endif

/**
 * nsp_prod_c:
 * @x: a  double complex  
 * @y: a  double complex  
 *
 * computes in @res the value of @x multiplied by @y.
 **/

/* x= x*y : can be used with x=x **/

void nsp_prod_c( doubleC *x,const doubleC *y)
{
  double loc,loc1;
  loc=x->r;loc1=y->r;
  x->r = x->r*y->r - x->i*y->i;
  x->i = loc*y->i + x->i*loc1;
}


/*
 * Complex versions of math.h functions 
 * function of two arguments can be used with res=x 
 * ex nsp_acos_c(&x,&x) 
 */

/**
 * nsp_isnan_c:
 * @x: a  double complex  
 *
 * Return value: true or false in a double 
 **/

double nsp_isnan_c(const doubleC *x)
{
  return (double) (isnan (x->r) || isnan (x->i));
}

/**
 * nsp_isinf_c:
 * @x: a  double complex  
 * 
 * Return value: true or false in a double 
 **/

double nsp_isinf_c(const doubleC *x)
{
  return (double) ( ((int) isinf (x->r) || (int) isinf (x->i)));
}

/**
 * nsp_finite_c:
 * @x: a  double complex  
 * 
 * 
 * 
 * Return value: 
 **/
double nsp_finite_c(const doubleC *x)
{
  return (double) (! (int)nsp_isinf_c(x));
}

#ifdef NEW
/**
 * nsp_acos_c:
 * @z: 
 * @res:
 * 
 * computes in @res the  arccos of a complex number @z.
 *
 * 
 * Author: Bruno Pincon
 * Thanks to Tom Fairgrieve
 **/
void nsp_acos_c(const doubleC *z, doubleC *res)
{
  /*
   *  This is a C translation of an algorithm by 
   *  T.E. Hull, T. F. Fairgrieve and P.T.P. Tang which 
   *  appears in their paper:
   *  "Implementing the Complex Arcsine and Arccosine 
   *   Functions Using Exception Handling", ACM, TOMS, 
   *   Vol 23, No. 3, Sept 1997, p. 299-335
   *
   *  with some modifications so as don't rely on ieee handle
   *  trap functions (initial version was coded in fortran77
   *  for scilab).
   */
  double const LN2 = 0.6931471805599453094172321,
    HALFPI = 1.5707963267948966192313216,
    PI = 3.1415926535897932384626433,
    Across = 1.5,
    Bcross = 0.6417,
    LSUP = 1.675975991242824544390291997e153,   /* sqrt(DBL_MAX)/8.0 */
    LINF = 5.966672584960165394645868037e-154,  /* 4sqrt(dlamch('u'))*/
    EPSM = 1.053671212772350794674224201e-8;    /* sqrt(epsm)        */
  double x = fabs(z->r), y = fabs(z->i), R, S, A, B, xp1, xm1, szr, szi;

  szr = z->r >= 0.0 ? 1.0 : -1.0;
  szi = z->i >= 0.0 ? 1.0 : -1.0;
  xp1 = x + 1.0; xm1 = x - 1.0; 

  if ( LINF <= Min(x,y)  &&  Max(x,y) <= LSUP )  /* we are in the safe region */
    {
      R = sqrt(xp1*xp1 + y*y);
      S = sqrt(xm1*xm1 + y*y);
      A = 0.5*(R + S);
      B = x/A;

      /* compute real part */
      if ( B <= Bcross )
	res->r = acos(B);
      else if ( x <= 1.0 )
	res->r = atan( sqrt(0.5*(A+x)*((y*y)/(R+xp1)+(S-xm1))) / x );
      else
	res->r = atan( (y*sqrt(0.5*((A+x)/(R+xp1)+(A+x)/(S+xm1)))) / x );

      /* compute imaginary part */
      if ( A <= Across )
	{
	  double Am1;
	  if ( x < 1.0 )
	    Am1 = 0.5*((y*y)/(R+xp1)+(y*y)/(S-xm1));
	  else
	    Am1 = 0.5*((y*y)/(R+xp1)+(S+xm1));
	  res->i = nsp_log1p(Am1 + sqrt(Am1*(A+1.0)));
	}
      else
	res->i = log(A + sqrt(A*A - 1.0));
    }

  else  /* HANDLE BLOC : evaluation in the special regions  */
    {
      if ( y <= EPSM*fabs(xm1) )
	{
	  if ( x < 1.0 )
	    {
	      res->r = acos(x);
	      res->i = y/sqrt(-xm1*xp1);
	    }
	  else
	    {
	      res->r = 0.0;
	      if ( x <= LSUP )
		res->i = nsp_log1p(xm1 + sqrt(xm1*xp1));
	      else
		res->i = LN2 + log(x);
	    }
	}
      else if ( y < LINF )
	{
	  res->r = sqrt(y);
	  res->i = res->r;
	}
      else if ( EPSM*y - 1.0 >= x )
	{
	  res->r = HALFPI;
	  res->i = LN2 + log(y);
	}
      else if ( x > 1.0 )
	{
	  res->r = atan(y/x);
	  res->i = LN2 + log(y) + 0.5*nsp_log1p((x/y)*(x/y));
	}
      else
	{
	  res->r = HALFPI;
	  A = sqrt(1.0 + y*y);
	  res->i = 0.5*nsp_log1p(2.0*y*(y+A));
	}
    }

  /* recover the signs */
  if ( szr < 0.0 )
    res->r = PI - res->r;

  if ( y != 0.0  ||  szr < 0.0 )
    res->i = - szi * res->i;
}
#else
/**
 * nsp_acos_c:
 * @x: 
 * @res: 
 * 
 * -i * log (x + sqrt (x*x - 1.0)); 
 **/
void nsp_acos_c(const doubleC *x, doubleC *res)
{
  doubleC zloc;
  zloc.r=x->r ; zloc.i=x->i;
  nsp_prod_c(&zloc,&zloc);
  zloc.r -= 1.00;
  nsp_sqrt_c(&zloc,&zloc);
  zloc.r += x->r ; 
  zloc.i += x->i;
  nsp_log_c(&zloc,&zloc);
  res->r = zloc.i;
  res->i = - zloc.r;
}
#endif

#ifdef NEW
/**
 * nsp_acosh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes the acosh of the complex number x
 * uses the formula
 * 
 *   acosh(x) = sign(-imag(acos(x)) i acos(x)
 *
 *   (with sign(0) = 1) 
 **/
void nsp_acosh_c(const doubleC *x, doubleC *res)
{
  doubleC xloc;
  int sign;

  /* xloc = x */
  xloc.r = x->r; xloc.i = x->i;
  /* xloc <- acos(xloc) */
  nsp_acos_c(&xloc, &xloc);
  /* compute sign(-imag(xloc)) */
  sign = -xloc.i >= 0.0 ? 1 : -1;
  /* res = i * xloc */
  res->r = -xloc.i; res->i = xloc.r;
  if ( sign < 0 )
    {
      res->r = - res->r; res->i = - res->i;
    } 
}
#else
/**
 * nsp_acosh_c:
 * @x: 
 * @res: 
 * 
 * log (x + sqrt (x*x - 1.0));
 **/
void nsp_acosh_c(const doubleC *x, doubleC *res)
{
  doubleC zloc;
  zloc.r=x->r ; zloc.i=x->i;
  nsp_prod_c(&zloc,&zloc);
  zloc.r -= 1.00;
  nsp_sqrt_c(&zloc,&zloc);
  zloc.r += x->r ; 
  zloc.i += x->i;
  nsp_log_c(&zloc,res);
}
#endif

#ifdef NEW
/**
 * nsp_asin_c:
 * @z: 
 * @res:
 * 
 * computes in @res the  arcsin of a complex number @z.
 *
 *       res = -i * log (i*z + sqrt (1.0 - z*z));
 * 
 * Author: Bruno Pincon
 * Thanks to Tom Fairgrieve
 **/
void nsp_asin_c(const doubleC *z, doubleC *res)
{
  /*
   *  This is a C translation of an algorithm by 
   *  T.E. Hull, T. F. Fairgrieve and P.T.P. Tang which 
   *  appears in their paper:
   *  "Implementing the Complex Arcsine and Arccosine 
   *   Functions Using Exception Handling", ACM, TOMS, 
   *   Vol 23, No. 3, Sept 1997, p. 299-335
   *
   *  with some modifications so as don't rely on ieee handle
   *  trap functions (initial version was coded in fortran77
   *  for scilab).
   */
  double const LN2 = 0.6931471805599453094172321,
    HALFPI = 1.5707963267948966192313216,
    Across = 1.5,
    Bcross = 0.6417,
    LSUP = 1.675975991242824544390291997e153,   /* sqrt(DBL_MAX)/8.0 */
    LINF = 5.966672584960165394645868037e-154,  /* 4sqrt(dlamch('u'))*/
    EPSM = 1.053671212772350794674224201e-8;    /* sqrt(epsm)        */
  double x = fabs(z->r), y = fabs(z->i), R, S, A, B, xp1, xm1, szr, szi;

  szr = z->r >= 0.0 ? 1.0 : -1.0;
  szi = z->i >= 0.0 ? 1.0 : -1.0;
  xp1 = x + 1.0; xm1 = x - 1.0; 

  if ( LINF <= Min(x,y)  &&  Max(x,y) <= LSUP )  /* we are in the safe region */
    {
      R = sqrt(xp1*xp1 + y*y);
      S = sqrt(xm1*xm1 + y*y);
      A = 0.5*(R + S);
      B = x/A;

      /* compute real part */
      if ( B <= Bcross )
	res->r = asin(B);
      else if ( x <= 1.0 )
	res->r = atan( x / sqrt(0.5*(A+x)*((y*y)/(R+xp1)+(S-xm1))) );
      else
	res->r = atan( x / (y*sqrt(0.5*((A+x)/(R+xp1)+(A+x)/(S+xm1)))) );

      /* compute imaginary part */
      if ( A <= Across )
	{
	  double Am1;
	  if ( x < 1.0 )
	    Am1 = 0.5*((y*y)/(R+xp1)+(y*y)/(S-xm1));
	  else
	    Am1 = 0.5*((y*y)/(R+xp1)+(S+xm1));
	  res->i = nsp_log1p(Am1 + sqrt(Am1*(A+1.0)));
	}
      else
	res->i = log(A + sqrt(A*A - 1.0));
    }

  else  /* HANDLE BLOC : evaluation in the special regions  */
    {
      if ( y <= EPSM*fabs(xm1) )
	{
	  if ( x < 1.0 )
	    {
	      res->r = asin(x);
	      res->i = y/sqrt(-xm1*xp1);
	    }
	  else
	    {
	      res->r = HALFPI;
	      if ( x <= LSUP )
		res->i = nsp_log1p(xm1 + sqrt(xm1*xp1));
	      else
		res->i = LN2 + log(x);
	    }
	}
      else if ( y < LINF )
	{
	  res->r = HALFPI - sqrt(y);
	  res->i = sqrt(y);
	}
      else if ( EPSM*y - 1.0 >= x )
	{
	  res->r = x/y;
	  res->i = LN2 + log(y);
	}
      else if ( x > 1.0 )
	{
	  res->r = atan(x/y);
	  res->i = LN2 + log(y) + 0.5*nsp_log1p((x/y)*(x/y));
	}
      else
	{
	  A = sqrt(1.0 + y*y);
	  res->r = x/A;
	  res->i = 0.5*nsp_log1p(2.0*y*(y+A));
	}
    }

  /* recover the signs */
  res->r = szr * res->r;
  res->i = szi * res->i;
}
#else
/**
 * nsp_asin_c:
 * @x: 
 * @res: 
 * 
 * 
 * -i * log (i*x + sqrt (1.0 - x*x)); 
 **/
void nsp_asin_c(const doubleC *x, doubleC *res)
{
  doubleC zloc;
  zloc.r=x->r ; zloc.i=x->i;
  nsp_prod_c(&zloc,&zloc);
  zloc.r -= 1.00;
  zloc.r = -zloc.r ;
  zloc.i = -zloc.i ;
  nsp_sqrt_c(&zloc,&zloc);
  zloc.r -= x->i ; 
  zloc.i += x->r;
  nsp_log_c(&zloc,&zloc);
  res->r = zloc.i;
  res->i = - zloc.r;
}
#endif

#ifdef NEW
/**
 * nsp_asinh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes the arcsin of the complex number x
 * uses the formula  asinh(z) = -i asin(i z)
 * 
 **/
void nsp_asinh_c(const doubleC *x, doubleC *res)
{
  doubleC xloc;

  /* xloc = i x */
  xloc.r = -x->i; xloc.i = x->r;
  /* xloc <- asin(xloc) */
  nsp_asin_c(&xloc, &xloc);
  /* res = -i * xloc */
  res->r = xloc.i; res->i = -xloc.r;
}
#else
/**
 * nsp_asinh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * log (x + sqrt (x*x + 1.0)); 
 * 
 **/
void nsp_asinh_c(const doubleC *x, doubleC *res)
{
  doubleC zloc;
  zloc.r=x->r ; zloc.i=x->i;
  nsp_prod_c(&zloc,&zloc);
  zloc.r += 1.00;
  nsp_sqrt_c(&zloc,&zloc);
  zloc.r += x->r ; 
  zloc.i += x->i;
  nsp_log_c(&zloc,res);
}
#endif

#ifdef NEW

/**
 * nsp_atan_c:
 * @x: a pointer to a double complex  
 * @y: a pointer to a double complex  
 * 
 * computes the arctangent of a complex number
 * @y = yr + i yi = atan(@x), @x = xr + i xi
 * @y = (i/2) * log( (i+x)/(i-x) )
 * 
 * Author: Bruno Pincon and Lydia van Dijk (2001).
 * (Written by Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr>, 
 *  Polished by Lydia van Dijk).
**/

void nsp_atan_c(const doubleC *x, doubleC *y)
{
  /* Notes: 
   * CHANGES : - (Bruno on 2001 May 22) for ysptrk use a 
   *                 minimax polynome to enlarge the special
   *                 evaluation zone |s| < SLIM. Also rename
   *                 this function as lnp1m1.
   *               - (Bruno on 2001 June 7) better handling
   *                 of spurious over/underflow ; remove
   *                 the call to pythag ; better accuracy
   *                 in the real part for x near +-i
   *               - (Bruno on 2008 August 18) convert in C
   *                 for nsp
   *
   * ALGORITHM : noting x = a + i*b, we have:
   *     y = yr + i*yi = arctan(x) = (i/2) * log( (i+x)/(i-x) )
   *              
   *     This function has two branch points at +i and -i and the
   *     chosen  branch cuts are the two half-straight lines
   *     D1 = [i, i*oo) and D2 = (-i*oo, i].  The function is then
   *     analytic in C \ (D1 U D2)). 
   *
   *     From the definition it follows that:
   *
   *        yr = 0.5 Arg ( (i+x)/(i-x) )                   (1)
   *        yi = 0.5 log (|(i+x)/(i-x)|)                   (2)
   *
   *     so lim (x -> +- i) yr = undefined (and Nan is logical)
   *        lim (x -> +i)   yi = +oo
   *        lim (x -> -i)   yi = -oo
   *
   *     The real part of arctan(x) is discontinuous across D1 and D2
   *     and we impose the following definitions:
   *         if imag(x) > 1 then
   *             Arg(arctan(x)) =  pi/2 (=lim real(x) -> 0+)
   *         if imag(x) < 1 then
   *             Arg(arctan(x)) = -pi/2 (=lim real(x) -> 0-)
   *                 
   *
   *     Basic evaluation: if we write (i+x)/(i-x) using
   *     x = a + i*b, we get:
   *
   *     i+x    1-(a**2+b**2) + i*(2a)
   *     --- =  ---------------------- 
   *     i-x       a**2 + (1-b)**2
   *
   *     then, with r2 = |x|^2 = a**2 + b**2 :
   *  
   *     yr = 0.5 * Arg(1-r2 + (2*a)*i)
   *        = 0.5 * atan2(2a, (1-r2))                      (3)
   *
   *     This formula is changed when r2 > DBL_MAX (max pos float)
   *     and also when |1-r2| and |a| are near 0 (see comments
   *     in the code).
   *
   *     After some math:
   *             
   *     yi = 0.25 * log( (a**2 + (b + 1)**2) /
   *                      (a**2 + (b - 1)**2) )            (4)     
   *               
   *     Evaluation for "big" |x|
   *     ------------------------
   *
   *     If |x| is "big", the direct evaluation of yi by (4) may
   *     suffer of innaccuracies and of spurious overflow.  Noting 
   *     that  s = 2 b / (1 + |x|**2), we have:
   *
   *     yi = 0.25 log ( (1 + s)/(1 - s) )                 (5)
   *
   *                                3        5    
   *     yi = 0.25*( 2 * ( s + 1/3 s  + 1/5 s  + ... ))
   *
   *     yi = 0.25 * lnp1m1(s)    if  |s| < SLIM
   *
   *     So if |s| is less than SLIM we switch to a special
   *     evaluation done by the function lnp1m1. The 
   *     threshold value SLIM is choosen by experiment 
   *     (with the Pari-gp software). For |s| 
   *     "very small" we used a truncated taylor dvp, 
   *     else a minimax polynome (see lnp1m1).
   *
   *     To avoid spurious overflows (which result in spurious
   *     underflows for s) in computing s with s= 2 b / (1 + |x|**2)
   *     when |x|^2 > RMAX (max positive float) we use :
   *
   *            s = 2 / ( (a/b)*a + b )
   *     
   *     but if |b| = Inf  this formula leads to NaN when
   *     |a| is also Inf. As we have :
   *
   *            |s| <= 2 / |b|
   *
   *     we impose simply : s = 0  when |b| = Inf
   *
   *     Evaluation for x very near to i or -i:
   *     --------------------------------------
   *     Floating point numbers of the form a+i or a-i with 0 <
   *     a**2 < tiny (approximately 1d-308) may lead to underflow
   *     (i.e., a**2 = 0) and the logarithm will break formula (4).
   *     So we switch to the following formulas:
   *
   *     If b = +-1 and |a| < sqrt(tiny) approximately 1d-150 (say)
   *     then (by using that a**2 + 4 = 4 in machine for such a):
   *         
   *         yi = 0.5 * log( 2/|a| )   for b=1
   * 
   *         yi = 0.5 * log( |a|/2 )   for b=-1
   *
   *     finally: yi = 0.5 * sign(b) * log( 2/|a| )     
   *              yi = 0.5 * sign(b) * (log(2) - log(|a|)) (6)
   *
   *     The last trick is to avoid overflow for |a|=tiny!  In fact
   *     this formula may be used until a**2 + 4 = 4 so that the
   *     threshold value may be larger.
   */

  double const SLIM=0.2, ALIM=1e-150, TOL=0.3, LN2=0.69314718055994531,
    HALFPI=1.570796326794896619231321692;
  double r2, s, a, b;
  
  /*   Avoid problems due to sharing the same memory locations by
   *   x->r, y->r and x->i, y->i.
   */
  a = x->r;
  b = x->i;

  if (b == 0.0) /* x is real */
    {       
      y->r = atan(a);
      y->i = 0.0;
    }
  else          /* x is complex */
    {
      /* (1) Compute the imaginary part of arctan(x) */
      r2 = a*a + b*b;
      if ( r2  > DBL_MAX )
	{
	  if ( fabs(b) > DBL_MAX ) /* |b| is Inf => s = 0 */
	    s = 0.0;
	  else
            /*  try to avoid the spurious underflow in s when |b| is not
             *  negligible with respect to |a|
             */
	    s = 1.0 / ( ((0.5*a)/b)*a + 0.5*b );
	}
      else
	s = 2.0*b / (1.0 + r2);

      if ( fabs(s) < SLIM )
	/*  s is small: |s| < SLIM  <=>  |x| outside the following disks:
         *  D+ = D(center = [0;  1/slim], radius = sqrt(1/slim**2 - 1)) if b > 0
         *  D- = D(center = [0; -1/slim], radius = sqrt(1/slim**2 - 1)) if b < 0
         *  use the special evaluation of log((1+s)/(1-s)) (5)
         */
	y->i = 0.25*lnp1m1(s); /* cette fct est en static pour le moment */

      else   /* |s| >= SLIM  => |x| is inside D+ or D-  */
	{
	  if ( (fabs(b) == 1.0) && (fabs(a) <= ALIM) )
	    /*  x is very near +- i : use formula (6)  */
	    y->i = b > 0.0 ? 0.5*(LN2 - log(fabs(a))) : -0.5*(LN2 - log(fabs(a)));
	  else
            /*  use formula (4) */
	    y->i = 0.25 * log( (a*a + (b + 1.0)*(b + 1.0)) /
                               (a*a + (b - 1.0)*(b - 1.0)) );
	}

      /* (2) Compute the real part of arctan(x) */
      if (a == 0.0)  /* x is purely imaginary */
	{
	  if ( fabs(b) > 1.0 )
            /* got sign(b) * pi/2 */
	    y->r = b > 0.0 ? HALFPI : -HALFPI; 
	  else if ( fabs(b) == 1.0 )
            /* got a Nan with 0/0 */
	    y->r = (a - a) / (a - a);
	  else
	    y->r = 0.0;
	}
      else if ( r2 > DBL_MAX )
	/* yr is necessarily very near sign(a)* pi/2 */
	y->r = a > 0.0 ? HALFPI : -HALFPI;
      else if ( fabs(1.0 - r2) + fabs(a) <= TOL )
	/*  |b| is very near 1 (and a is near 0)  some
         *  cancellation occur in the (next) generic formula 
         */
	y->r = 0.5 * atan2(2.0*a, (1.0-b)*(1.0+b) - a*a);
      else
	/*  generic formula */
	y->r = 0.5 * atan2(2.0*a, 1.0 - r2);
    }
}

#else

/**
 * nsp_atan_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * i * log ((i + x) / (i - x)) / 2.0; 
 * 
 **/

void nsp_atan_c(const doubleC *x, doubleC *res)
{
  doubleC zloc,zloc1;
  zloc.r=x->r ; zloc.i= x->i + 1.00;
  zloc1.r= - x->r ; zloc1.i = - x->i + 1.00;
  nsp_div_cc(&zloc,&zloc1,&zloc);
  nsp_log_c(&zloc,&zloc);
  res->r = - zloc.i/2.00; 
  res->i = zloc.r/2.00;
}
#endif

#ifdef NEW

/**
 * nsp_atanh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * based on the formula  atanh(x) = i atan(-i x)
 * 
 **/

void nsp_atanh_c(const doubleC *x, doubleC *res)
{
  doubleC xloc;

  /* xloc = -i x */
  xloc.r = x->i; xloc.i = -x->r;
  /* xloc <- atan(xloc) */
  nsp_atan_c(&xloc, &xloc);
  /* res = i * xloc */
  res->r = -xloc.i; res->i = xloc.r;
}

#else

/**
 * nsp_atanh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 *  based on the formula  log ((1 + x) / (1 - x)) / 2.0; 
 * 
 **/

void nsp_atanh_c(const doubleC *x, doubleC *res)
{
  doubleC zloc,zloc1;
  zloc.r=x->r +1.00 ; zloc.i= x->i ;
  zloc1.r= 1.00 - x->r ; zloc1.i= - x->i;
  nsp_div_cc(&zloc,&zloc1,&zloc);
  nsp_log_c(&zloc,res);
  res->r /= 2.00; 
  res->i /= 2.00;
}
#endif

/**
 * nsp_ceil_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_ceil_c(const doubleC *x, doubleC *res)
{
  res->r=ceil (x->r); res->i=ceil (x->i);
}

/**
 * nsp_aint_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_aint_c(const doubleC *x, doubleC *res)
{
  res->r=aint(x->r);res->i= aint(x->i);
}

/**
 * nsp_floor_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_floor_c(const doubleC *x, doubleC *res)
{
  res->r=floor (x->r); res->i=floor (x->i);
}

/**
 * nsp_log10_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_log10_c(const doubleC *x, doubleC *res)
{
  nsp_log_c(x,res);
  res->r *=  M_LOG10E ;
  res->i *=  M_LOG10E ;
}

/**
 * nsp_round_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_round_c(const doubleC *x, doubleC *res)
{
  res->r=anint (x->r); res->i=anint (x->i);

}

/**
 * nsp_signum_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_signum_c(const doubleC *x, doubleC *res)
{
  double loc;
  loc =nsp_abs_c(x);
  if ( loc == 0.00) 
    { res->r = res->i = 0.00 ;}
  else 
    {
      res->r = x->r/loc;
      res->i = x->i/loc;
    }
}

#ifdef NEW
/**
 * nsp_tan_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * computes the tangent of the complex number x
 *
 * Author: Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr>.
 **/
void nsp_tan_c(const doubleC *x, doubleC *res)
{
  /*
   *     ALGORITHM
   *        based on the formula :
   *
   *                         0.5 sin(2 xr) +  i 0.5 sinh(2 xi)
   *        tan(xr + i xi) = ---------------------------------
   *                             cos(xr)^2  + sinh(xi)^2
   *        
   *        noting  d = cos(xr)^2 + sinh(xi)^2, we have :
   *
   *                yr = 0.5 * sin(2 * xr) / d       (1)
   *
   *                yi = 0.5 * sinh(2 * xi) / d      (2)
   *
   *        to avoid spurious overflows in computing yi with 
   *        formula (2) (which results in NaN for yi)
   *        we use also the following formula :
   *
   *                yi = sign(xi)   when |xi| > LIM  (3)
   *
   *        Explanations for (3) :
   *
   *        we have d = sinh(xi)^2 ( 1 + (cos(xr)/sinh(xi))^2 ), 
   *        so when : 
   *
   *           (cos(xr)/sinh(xi))^2 < epsm   ( epsm = max relative error
   *                                          for coding a real in a f.p.
   *                                          number set F(b,p,emin,emax) 
   *                                             epsm = 0.5 b^(1-p) )
   *        which is forced  when :
   *
   *            1/sinh(xi)^2 < epsm   (4) 
   *        <=> |xi| > asinh(1/sqrt(epsm)) (= 19.06... in ieee 754 double)
   *
   *        sinh(xi)^2 is a good approximation for d (relative to the f.p. 
   *        arithmetic used) and then yr may be approximate with :
   *
   *         yr = cosh(xi)/sinh(xi) 
   *            = sign(xi) (1 + exp(-2 |xi|))/(1 - exp(-2|xi|)) 
   *            = sign(xi) (1 + 2 u + 2 u^2 + 2 u^3 + ...)
   *
   *        with u = exp(-2 |xi|)). Now when :
   *
   *            2 exp(-2|xi|) < epsm  (2)  
   *        <=> |xi| > 0.5 * log(2/epsm) (= 18.71... in ieee 754 double)
   * 
   *        sign(xi)  is a good approximation for yr.
   *
   *        Constraint (1) is stronger than (2) and we take finaly 
   *
   *        LIM = 1 + log(2/sqrt(epsm)) 
   *
   *        (log(2/sqrt(epsm)) being very near asinh(1/sqrt(epsm))
   *
   */
  double const LIM = 20.06154746539849600897388334;
  double c, s, d, xr = x->r, xi = x->i;

  if ( xi == 0.0 )
    {
      res->r = tan(x->r);
      res->i = 0.0;
    }
  else if ( xr == 0.0 )
    {
      res->r = 0.0;
      res->i = tanh(x->i);
    }
  else
    {
      c = cos(xr); s = sinh(xi);
      d = c*c + s*s;
      res->r = 0.5*sin(2.0*xr)/d;
      if ( fabs(xi) < LIM )
	res->i = 0.5*sinh(2.0*xi)/d;
      else
	res->i = xi >= 0.0 ? 1.0 : -1.0;
    }
}
#else
/**
 * nsp_tan_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_tan_c(const doubleC *x, doubleC *res)
{
  doubleC loc1,loc2;
  nsp_sin_c(x,&loc1);
  nsp_cos_c(x,&loc2);
  nsp_div_cc(&loc1,&loc2,res);
}
#endif

#ifdef NEW
/**
 * nsp_tanh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 *
 * computes the tanh of a complex number
 * based on the formula  tanh(x) = -i tan(i x)
 * 
 **/
void nsp_tanh_c(const doubleC *x, doubleC *res)
{
  doubleC ix;
  ix.r = -x->i; ix.i = x->r;
  nsp_tan_c(&ix, &ix);
  res->r = ix.i; res->i = -ix.r;
}
#else
/**
 * nsp_tanh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_tanh_c(const doubleC *x, doubleC *res)
{
  doubleC loc1,loc2;
  nsp_sinh_c(x,&loc1);
  nsp_cosh_c(x,&loc2);
  nsp_div_cc(&loc1,&loc2,res);
}
#endif


