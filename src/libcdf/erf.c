/* Nsp
 * Copyright (C) 2007 Jean-Philippe Chancelier Enpc/Cermics
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

#include "cdf.h"

/* A union which permits us to convert between a double and two 32 bit ints.  
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * 
 * This union and SET_LOW_WORD are used to imporve the computation of exp(x*x) 
 * as in libc (See below).
 */

#define WIN32 

#ifndef WIN32 
#include <endian.h>
#include <sys/types.h>

typedef union
{
  double value;
  struct
  {
#if __FLOAT_WORD_ORDER == BIG_ENDIAN
    u_int32_t msw;
    u_int32_t lsw;
#else 
    u_int32_t lsw;
    u_int32_t msw;
#endif 
  } parts;
} ieee_double_shape_type;


#define SET_LOW_WORD(d,v)					\
  do {								\
    ieee_double_shape_type sl_u;				\
    sl_u.value = (d);						\
    sl_u.parts.lsw = (v);					\
    (d) = sl_u.value;						\
  } while (0)
#endif 

static double erf_approx(double x);
static double erf_approx1(double x);
static double erf_approx2(double x);

/**
 * cdf_erf:
 * @x: a double 
 * 
 * cdf_erf function returns the error function of x; defined as
 * erf(x) = 2/sqrt(pi)* integral from 0 to x of exp(-t*t) dt
 * max relative error 2.01e-16 on Linux. 
 * 
 * Returns: a double 
 **/

double cdf_erf (double x)
{
  const double c = .564189583547756; /* 1/sqrt(%pi) */
  double ret_val, t, x2, ax;

  ax = Abs (x);
  if (ax <= 0.85)
    {
      /* f_approx is a pade approximation [10,10] */
      return x*erf_approx(x*x)/2.0;
    }
  if (ax > 4.)
    {
      if (ax >= 7) return ( x > 0) ? 1 : -1 ;
      /* asymptotic series 
       *
       * erf(x) = 1 - (exp(-x^2)/sqrt(Pi))*(1/x)* 
       *           (1 - (1/2)*x^-2 +(3/4)*x^-4 -(15/8) x^(-6)+ (105/16)*x^(-8)) 
       * the asymptotic serie is approximated by pade approximation using 
       * Maple. 
       */
      x2 = x * x;
      t = 1. / x2;
      ret_val =  1 - exp(-x2)*c*(1/ax)*erf_approx2(t);
      return  (x < 0.) ?  -ret_val : ret_val ;
    }
  
  ret_val= 0.5 - exp (-(ax) * ax)*erf_approx1(ax)+0.5;
  return ( x < 0) ?  -ret_val : ret_val ;
}

/*
 * Using Maple code for approximation of erf 
 *
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) (erf(x)-erf(-x))/x ;end proc;
  g:= proc(x) f(sqrt(x));end proc; 

  Digits:=200;
  ggp:=chebpade(g(x),x=0.01..0.5,[10,10]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

*/


static double erf_approx(double x)
{
  {
    return((0.1991182222397213E1+(0.2919538530000317+(0.974098062698039E-1+(
0.6589093818965865E-2+(0.927131273455712E-3+(0.3417290206883588E-4+(
0.2580933874996821E-5+(0.4906587858994353E-7+(0.1928837863588316E-8+(
0.12412816944941E-10+0.1208762589108909E-12*x)*x)*x)*x)*x)*x)*x)*x)*x)*x)/(
0.8823196494856359+(0.4234752325876976+(0.9609004247372082E-1+(
0.1360981777952708E-1+(0.1336356469097111E-2+(0.9536149819709116E-4+(
0.5026788743980206E-5+(0.1940848549814423E-6+(0.5278156679389076E-8+(
0.9180481849414321E-10+0.7806230977595437E-12*x)*x)*x)*x)*x)*x)*x)*x)*x)*x));
  }
}

static double erf_approx1(double x)
{
  return((0.3568217177627678E-2+(0.5444297501411179E-2+(0.4135716726130396E-2
+(0.1888516179006951E-2+(0.5404695127508915E-3+(0.9187011004034143E-4+(
0.7350874797709247E-5-0.1128729588597506E-11*x)*x)*x)*x)*x)*x)*x)/(
0.356821717529337E-2+(0.9470599459420979E-2+(0.112539264524628E-1+(
0.7800815126334948E-2+(0.3428993607308159E-2+(0.9644447087996436E-3+(
0.1628374909101921E-3+0.130289958082267E-4*x)*x)*x)*x)*x)*x)*x));
}

/*
 * Using Maple code for approximation of erf 
 *
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) erfc(x)*exp(x*x)*x*sqrt(Pi) ;end proc;
  g:= proc(x) f(1/sqrt(x));end proc; 
  
  Digits:=200;
  ggp:=chebpade(g(x),x=(1/49)..(1/16),[10,10]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

*/

static double erf_approx2(double x)
{
  {
    return((0.2599098159436489+(0.9403918995875176E1+(0.1243729660185006E3+(
0.7558123106477017E3+(0.2174289444886517E4+(0.273052355666007E4+(
0.1169641949711052E4+0.7358165519735863E2*x)*x)*x)*x)*x)*x)*x)/(
0.2599098159436489+(0.9533873903846993E1+(0.1289449706084679E3+(
0.8136217214287283E3+(0.2500561933068374E4+(0.3557469464700427E4+(
0.1991626855607897E4+0.2842440147288589E3*x)*x)*x)*x)*x)*x)*x));
  }
}

/**
 * cdf_erfc:
 * @ind: an integer 
 * @x: a double 
 * 
 * complementary error function 
 *     cdf_erfc(0,x) function returns the complementary error function of  x,
 *       that is 1.0 - erf(x).
 *     cdf_erfc(1,x) function returns 
 *        y = exp(x^2 ) * erfc(x) 
 * 
 * Returns: a double 
 **/

double cdf_erfc1(double x);

double cdf_erfc(int ind, double x)
{
  const double c = .564189583547756;
  double ret_val, d1;
  double t,  ax;
#ifndef WIN32 
  double z=x;
#endif 
  
  if ( ind == 1) return cdf_erfc1(x);

  ax = Abs (x);
  if ( ax <= 0.8 )
    {
      /* Abs(x) <= 0.8 
       * here we use 1 - the same approximation for erf(x) 
       */
      return  0.5*(1 - x*erf_approx(x*x) +1) ;
    }

  if (ax > 4.)
    {
      if (x <= - 6)
	{
	  return 2;
	}
      if ( x > 100. ) return 0.0;
      if ( x * x > -cdf_exparg (1)) return 0.0;
      d1 = 1. / x;
      t = d1 * d1;
#ifndef WIN32 
      SET_LOW_WORD(z,0);
      ret_val  =  exp(-z*z)*exp((z-x)*(z+x))*  c*(1/ax)*erf_approx2(t);
#else 
      ret_val  =  exp(-x*x)*c*(1/ax)*erf_approx2(t);
#endif 
      return  (x < 0.) ? 2. - ret_val : ret_val;
    }
  else 
    {
      /* 0.5 <= abs(X) <= 4 */
#ifndef WIN32 
      SET_LOW_WORD(z,0);
      ret_val= exp(-z*z)*exp((z-x)*(z+x))*erf_approx1(ax);
#else 
      ret_val= exp(-x*x)*erf_approx1(ax);
#endif 
      return  (x < 0.) ? 2. - ret_val : ret_val;
    }
}

/**
 * cdf_erfc1:
 * @x: a double 
 * 
 * computes exp(x^2)*erfc(x);
 * 
 * Returns: a double 
 **/

double cdf_erfc1(double x)
{
  static const double c = .564189583547756;
  double ret_val, d1, t,  ax, expx2;
#ifndef WIN32 
  double z;
#endif 
  ax = Abs (x);
  if ( ax <= 0.8 )
    {
      /* Abs(x) <= 0.8 
       * here we use 1 - the same approximation for erf(x) 
       */
      double t = x*x;
      return exp (t)* 0.5*(1 - x*erf_approx(t) +1);
    }
  /* if ( x  > 2.5 ) return c*erfc_cf(x); */
#ifndef WIN32 
  z=x;
  SET_LOW_WORD(z,0);
  expx2= exp(z*z)*exp(-(z-x)*(z+x));
#else 
  expx2= exp(x*x);
#endif 
  if ( ax > 4.)
    {
      if (x <= -6 )
	{
	  return expx2 * 2. ;
	}
      d1 = 1. / x;
      t = d1 * d1;
      ret_val =  c*(1/ax)*erf_approx2(t);
      return  (x < 0.) ? expx2* 2 - ret_val : ret_val ;
    }
  else 
    {
      /* 0.5 <= abs(X) <= 4 */
      ret_val= erf_approx1(ax);
    }
  return ( x< 0) ? expx2 * 2. - ret_val : ret_val;
}	


/* sqrt(pi)*erfc(x)*exp(x^2) approximated with continued fractions for positive x. 
 * erfc(x) = 2/sqrt(pi)*integral(exp(-t^2),t,x,inf)
 *         = exp(-x^2)/sqrt(pi) * [1/x+ (1/2)/x+ (2/2)/x+ (3/2)/x+ (4/2)/x+ ...]
 */

#if 0
static double erfc_cf(double x)
{
  double rel_error = 1.e-15;
  double a=1, b=x, c=x, d=x*x+0.5;  
  double q1,q2, n= 1.0, t;
  do {
    t= a*n+b*x;
    a= b;
    b= t;
    t= c*n+d*x;
    c= d;
    d= t;
    n+= 0.5;
    q1= q2;
    q2= b/d;
  } while ( Abs(q1-q2)/q2 > rel_error);
  return q2;
}
#endif 

