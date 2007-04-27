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
#include <stdlib.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/cnumeric.h"

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
 * Returns a double
 */

double nsp_abs_c(const doubleC *x)
{
  return hypot (x->r, x->i);
}

/**
 * nsp_arg_c:
 * @x: a double complex (#doubleC) 
 * 
 * 
 * 
 * Return value: 
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

/**
 * nsp_log_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_log_c(const doubleC *x, doubleC *res)
{
  double loc;
  loc=log (nsp_abs_c(x));
  res->i=nsp_arg_c(x);
  res->r=loc;
}

/**
 * nsp_pow_cc:
 * @x: a  double complex  
 * @y: 
 * @res: a pointer to a double complex  
 * 
 * 
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
 * @y: 
 * @res: a pointer to a double complex  
 * 
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
 * @y: 
 * @res: a pointer to a double complex  
 * 
 * 
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
 * @x: a  double complex  
 * @y: 
 * @res: a pointer to a double complex  
 * 
 * 
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
 * return x^p
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
 * nsp_pow_ci:
 * @x: a #doubleC
 * @p: an integer 
 * @y: a #doubleC
 *
 * computes y = x^p (can be used with x=x^p)
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
 * 
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
 * @y: 
 * @res: a pointer to a double complex  
 * 
 * 
 **/
void nsp_div_cc(const doubleC *x, doubleC *y, doubleC *res)
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
 * @x: a  double complex  
 * @y: 
 * @res: a pointer to a double complex  
 * 
 * 
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



/**
 * nsp_sqrt_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * 
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

/**
 * nsp_prod_c:
 * @x: a  double complex  
 * @y: a  double complex  
 *
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

/**
 * nsp_atanh_c:
 * @x: a  double complex  
 * @res: a pointer to a double complex  
 * 
 * log ((1 + x) / (1 - x)) / 2.0; 
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



