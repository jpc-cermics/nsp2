/* Nsp
 * Copyright (C) 2007-2010 Jean-Philippe Chancelier Enpc/Cermics
 *                         Bruno Pincon Esial/Iecn
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
 * a version of psi(x) approximated with Mapple 
 *
 */

#include <nsp/spmf.h>
#include "cdf.h"

static double f_approx_ls(double x);
static double f_approx_rs(double x);
static double f_approx_gt_3(double x);

/**
 * cdf_psi1:
 * @xx: a double 
 * 
 *     evaluation of the digamma function psi(x) 
 *     this code was written following the code of psi1 
 *     (funpack (Cody and all) + modification by A.H. Morris (nswc)) with 
 *     various modifications:
 *       * the rationnal approximation for evaluation in [0.5,3] was replaced by
 *         2 rationnal approximations (one for x < dx0, the other for x >=dx0, 
 *         dx0 = 1.4616321... the positive zero of psi). These rationnal approximations were
 *         computed with Maple (the Maple code is given at the end).
 *       * use of nsp_cotanpi in the reflection formula for x < 0.5
 *       * use a kind of quad approximation of the zero of psi
 *         (relative error stays small for floats near  1.4616321... the positive zero).
 *       * change the value of xmax constant. 
 *       * change evaluation at 0
 * 
 *   Returns: a double
 **/

double cdf_psi1 (double x)
{
  double aug = 0.0;
  /* machine dependent constants (here they are set for double ieee) */ 
  double const xmax   = 1e16,  /* switch value beyond which psi may be represented as log(x) */ 
               xsmall = 1e-9;  /* absolute argument below which pi*cotan(pi*x) may be represented by 1/x */ 

  if (x < 0.5)
    {
      /* for x < 0.5, we use  psi(x) = psi(1-x) - pi * cotan(pi*x)  
       * first compute aug = - pi*cotan(pi*x)
       */
      if ( fabs(x) > xsmall )
	aug = - M_PI * nsp_cotanpi(x);
      else 
	{
	  /*  | x | <= xsmall. use 1/x as a substitute for  pi*cotan(pi*x) 
	   *  (moreover this provides the good behavior at x = 0 (-oo for x=+0 and +oo for x=-0))
           */
	  aug = -1.0 / x;
	}
      x = 1.0 - x;
    }
  
  if (x > 3.0)
    {
      if (x >= xmax)
	return aug + log (x);
      else 
	return aug + f_approx_gt_3(x) + log(x) - 1.0/(2.0*x);
    }
  else           /* 0.5 <= x <= 3.0  */
    {
      /*  use 2 rational approximations of  psi(x) / (x - dx0), computed with Maple 
       *  one for x < dx0 the other for x >= dx0. dx0 positive zero of psi.
       */
      const double dx0 = 1.461632144968362341262659542325721325;
      /* the 3 following constants improve the computation of x-dx0 using (x - xn/xd) - xr
       * xn/xd is the closest (under) approximation of (exact) dx0 in ieee double precision
       * and xd/xd + xr is an approximation of dx0 in quad precision. xd/xn could be written as a double
       * but we use this form to avoid possible simplification by the compiler with -Ox flag 
       */
      const double xn = 3291302991716127.0, xd = 2251799813685248.0, xr = 3.17544559224688270016686752e-16;
      if ( x >= dx0) 
	return f_approx_rs(x)*((x - xn/xd) - xr) + aug ;
      else	
	return f_approx_ls(x)*((x - xn/xd) - xr) + aug ;
    }
} 

/*  approximations of  psi(x) / (x - dx0) on [dx0, 3] computed with Maple */
static double f_approx_rs(double x)
{
  return((0.1944202116321335E-1+(0.6023197220165417E-1+(0.5013507837380874E-1
+(0.1497783719529072E-1+(0.1580892247487796E-2+(0.4474696461772995E-4+
0.6664740431685916E-7*x)*x)*x)*x)*x)*x)/(0.1935895616227E-7+(
0.2841689855839281E-1+(0.5219307314640644E-1+(0.2966216682460071E-1+(
0.6335518082535423E-2+(0.4777767743644465E-3+0.9029486537581591E-5*x)*x)*x)*x)*
x)*x));

}

/*  approximations of  psi(x) / (x - dx0) on  [0.5, dx0] computed with Maple */
static double f_approx_ls(double x)
{
    return((0.1320643016823474+(0.4253192778241912+(0.3875433094640546+(
0.1348672821673454+(0.1788325396971957E-1+(0.6938011374594729E-3+
0.1628066660999515E-5*x)*x)*x)*x)*x)*x)/(0.13188548555E-10+(0.1930294282906748+
(0.3781764189968253+(0.2403577987227719+(0.608877209974987E-1+(
0.5829374028059031E-2+0.1504027028852691E-3*x)*x)*x)*x)*x)*x));
}


/* approximations of  psi(x) - log(x) + 1.0/(2.0*x) for  x > 3 */

/* the following one was computed by Maple but is not enough accurate */
/* static double f_approx_gt_3(double x) */
/* { */
/*   return((-0.1166030795749449E-5+(-0.4013284555660655E-5+( */
/* -0.3142071959165251E-5+(-0.2344315052738454E-5+(-0.139772915509597E-13+ */
/* 0.2043488018737909E-15*x)*x)*x)*x)*x)/(-0.2505852419838E-6+( */
/* 0.381347621436607E-5+(0.17753395150168E-4+(0.5097372579550912E-4+( */
/* 0.3770476944594263E-4+0.2813178578851754E-4*x)*x)*x)*x)*x)); */
/* } */


/* this one involves rational chebyshev  approximations published 
 * in math. comp. 27, 123-127(1973) by cody, strecok and thacher. 
 */
static double f_approx_gt_3(double x)
{
  double w = 1.0/(x*x);
  return (  (w*(-0.648157123766197 + w*(-4.48616543918019 + w*(-7.01677227766759 - w*2.12940445131011))))
	    / (7.77788548522962 + w*(54.6117738103215 + w*(89.2920700481861 + w*(32.2703493791143+w)))) );
}


/*
 * Using Maple code for approximation of Psi(x)/(x-dx0) in [0.5,3] 
 * Approximation of dx0 
 Digits:= 50;
 dx0:= 1.461632144968362341262659542325721328468196204;
 Psi(dx0);
 * gives  -.62379552335332861548617858697593025567122107441233 1O^(-47) 

  with(numapprox);
  with(orthopoly);
  f:= proc(x) Psi(x)/(x-dx0);end proc;

  Digits:=30;
  ggp:=chebpade(f(x),x=1.5..3,[7,7]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

  Digits:=60;
  ggp:=chebpade(f(x),x=0.5..1.45,[7,7]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

  Digits:=60;
  ggp:=chebpade(Psi(x) - ln(x) + 1 / (2*x),x=3..10,[5,5]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

*/


/**
 * cdf_psi1_old:
 * @xx: a double 
 * 
 *     evaluation of the digamma function 
 *
 *     psi1(xx) is assigned the value 0 when the digamma function cannot 
 *     be computed. 
 *     the main computation involves evaluation of rational chebyshev 
 *     approximations published in math. comp. 27, 123-127(1973) by 
 *     cody, strecok and thacher. 
 * 
 *     psi1 was written at argonne national laboratory for the funpack 
 *     package of special function subroutines. 
 *     psi1 was modified by a.h. Morris (nswc). 
 *     this version was C-translated and modified. 
 * 
 * Returns: a double
 **/

double cdf_psi1_old (double xx)
{
  /*     pivo4 = PI/4 */
  const double piov4 = .785398163397448;

  double den, aug, sgn, xmx0; 
  double w, x, z__, upper,  xmax1, xmax2,  xsmall;
  int  i , m, n ,  nq;
  /*     machine dependent constants ... 
   *        xmax1  = the smallest positive floating point constant 
   *                 with entirely int representation.  also used 
   *                 as negative of lower bound on acceptable negative 
   *                 arguments and as the positive argument beyond which 
   *                 psi1 may be represented as alog(x). 
   *        xsmall = absolute argument below which pi*cotan(pi*x) 
   *                 may be represented by 1/x. 
   */
  xmax1 = (double) cdf_ipmpar (3);
  xmax2 =  1. / cdf_spmpar(1);
  xmax1 = Min (xmax1, xmax2);
  xsmall = 1e-9;

  x = xx;
  aug = 0.;
 
  if (x < 0.5)
    {
      /* X < 0.5, we use :  PSI1(1-X) = PSI1(X) + PI * COTAN(PI*X)  
       * i.e compute psi1(x) with psi1(1-x) - pi*cotan(pi*x) 
       * first compute aug = - pi*cotan(pi*x)
       */
      if (Abs (x) > xsmall)
	{
	  /*     reduction of argument for cotan  */
	  w = -x;
	  sgn = piov4;
	  if (w <= 0.)
	    {
	      w = -w;
	      sgn = -sgn;
	    }
	  
	  if (w >= xmax1)
	    {
	      /*     make an error exit if x .le. -xmax1  */
	      goto L100;
	    }
	  nq = (int) w;
	  w -= (double) nq;
	  nq = (int) (w * 4.);
	  w = (w - (double) nq * .25) * 4.;
	  /* 
	   *     w is now related to the fractional part of  4.0 * x. 
	   *     adjust argument to correspond to values in first 
	   *     quadrant and determine sign 
	   */
	  n = nq / 2;
	  if (n + n != nq)
	    {
	      w = 1. - w;
	    }
	  z__ = piov4 * w;
	  m = n / 2;
	  if (m + m != n)
	    {
	      sgn = -sgn;
	    }
	  /* 
	   *     determine final value for  -pi*cotan(pi*x) 
	   */
	  n = (nq + 1) / 2;
	  m = n / 2;
	  m += m;
	  if (m != n)
	    {
	      aug = sgn * (sin (z__) / cos (z__) * 4.);
	    }
	  else 
	    {
	      if (z__ == 0.)
		{
		  goto L100;
		}
	      /* 
	       *     use cos/sin as a substitute for cotan, and 
	       *     sin/cos as a substitute for tan 
	       */
	      aug = sgn * (cos (z__) / sin (z__) * 4.);
	    }
	}
      else 
	{
	  /* Abs (x) < xsmal */
	  if (x == 0.)
	    {
	      goto L100;
	    }
	  /*  0 < abs(x) <= xsmall. use 1/x as a substitute 
	   *  for  pi*cotan(pi*x) 
	   */
	  aug = -1. / x;
	}
      x = 1. - x;
    }
  
  /* in the range [0.5,xmax1,3,...]  */
  if (x > 3.)
    {
      if (x >= xmax1)
	{
	  return  aug + log (x);
	}
      else 
	{
	  /*     coefficients for rational approximation of 
	   *     psi1(x) - ln(x) + 1 / (2*x),  x .gt. 3.0 
	   */
	  const double p2[4] =  { -2.12940445131011, -7.01677227766759, -4.48616543918019,
				  -.648157123766197 };
	  const double q2[4] =  { 32.2703493791143, 89.2920700481861, 54.6117738103215,
				  7.77788548522962 };

	  w = 1. / (x * x);
	  den = w;
	  upper = p2[0] * w;
	  for (i = 0; i < 3; ++i)
	    {
	      den = (den + q2[i]) * w;
	      upper = (upper + p2[i+1]) * w;
	    }
	  aug = upper / (den + q2[3]) - .5 / x + aug;
	  return  aug + log (x);
	}
    }
  else 
    {
      /*  0.5 <= X <= 3.0  
       *  coefficients for rational approximation of 
       *     psi1(x) / (x - dx0),  0.5 .le. x .le. 3.0 
       *  dx0 = zero of psi1 to extended precision 
       */

      const double dx0 = 1.461632144968362341262659542325721325;

      const double p1[7] = { .0089538502298197, 4.77762828042627, 142.441585084029, 
			     1186.45200713425,
			     3633.51846806499, 4138.10161269013, 1305.60269827897 };
      const double q1[6] = { 44.8452573429826, 520.752771467162, 2210.0079924783, 
			     3641.27349079381, 1908.310765963, 6.91091682714533e-6 };
      den = x;
      upper = p1[0] * x;
      for (i = 0; i < 5; ++i)
	{
	  den = (den + q1[i]) * x;
	  upper = (upper + p1[i+1]) * x;
	}
      den = (upper + p1[6]) / (den + q1[5]);
      xmx0 = x - dx0;
      return den * xmx0 + aug;
    }
  
 L100:
  return 0;
} 
