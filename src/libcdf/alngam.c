/* Nsp
 * Copyright (C) 2007-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * a version of log(gamma(x)) approximated with Mapple 
 *
 */

#include "cdf.h"


/**
 * cdf_alngam:
 * @x: pointer to double 
 * 
 * computes the natural logarithm of gamma(x). 
 * or 0 when gamma(x) is negative. pade approximations 
 * are computed with Maple.
 * 
 * Returns: a double 
 **/

/* approche log(gamma(x))/(x-2) dans [2,3] */

inline double f_approx_2_3(double x)
{
    return((-0.2794541810411736E-2+(-0.1750088178423485E-1+(
-0.6758249491763692E-2+(0.1888720066674089E-1+(0.7889156408246519E-2+(
0.2888937267292737E-3+(-0.121317216947609E-4+(0.5753967990547954E-6+(
-0.2115429901368066E-7+0.4123840082903748E-9*x)*x)*x)*x)*x)*x)*x)*x)*x)/(
0.1389552237743223E-2+(0.202448732795327E-1+(0.4752910037707498E-1+(
0.2828438434068512E-1+0.4033042504760442E-2*x)*x)*x)*x));
}

/* approche log(gamma(x))/(x-2) dans [1.5,2] */

inline double f_approx_15_2(double x)
{
    return((-0.3131749855875889E-2+(-0.3083317481667811E-1+(
-0.3246981457626206E-1+(0.3683597860831141E-1+(0.2802282302892632E-1+(
0.1667696708820676E-2+(-0.9798138151122817E-4+(0.6554108188827196E-5+(
-0.3412681424406199E-6+0.9446264568714881E-8*x)*x)*x)*x)*x)*x)*x)*x)*x)/(
0.1431904787560423E-2+(0.298047281489312E-1+(0.1005584471583368+(
0.844861749564532E-1+0.1694223676885694E-1*x)*x)*x)*x));
}

inline double f_approx_1_15(double x)
{
  return((-0.5652013301775986E-2+(-0.917908961418217E-1+(-0.2354081919178484+
(-0.5713581783385663E-1+(0.8136406261691052E-1+(0.1046225196882071E-1+(
-0.9138733441960895E-3+(0.8904336071089001E-4+(-0.6674994095911582E-5+
0.2640496384435728E-6*x)*x)*x)*x)*x)*x)*x)*x)*x)/(0.1187377777326479E-2+(
0.3643944985695971E-1+(0.1824310922229121+(0.2292621661592006+
0.6866974641221182E-1*x)*x)*x)*x));
}

double cdf_alngam(double x)
{
  const double hln2pi=0.91893853320467274178E0;
  double prod, xx;

  if ( x > 12 ) 
    {
      return cdf_stirling_series(x) 
	+  hln2pi  + (x - .5) * log (x) - x;
    }
  else 
    {
      if ( x>= 0 && x <= 0.5 ) 
	{
	  return f_approx_1_15(x+1)*x - log (x);
	}
      else if ( x>= 0 && x <=  1.0 ) 
	{
	  return f_approx_15_2(x+1)*(x-1)-log(x);
	}
      else if ( x >= 0 && x <= 1.5 ) 
	{
	  return f_approx_1_15(x)*(x-1);
	}
      else if ( x >= 1.5 && x <= 2.0) 
	{
	  return f_approx_15_2(x)*(x-2);
	}
      prod = 1.;
      xx = x;
      /* bring back x <= 3 */
      while ( xx > 3 ) 
	{
	  xx -= 1;
	  prod *=xx;
	}
      /* now bring x >=2 */
      while ( xx < 2 ) 
	{
	  prod /= xx;
	  xx += 1.;
	}
      /* here we have gamma(x)= prod*gamma(xx) 
       * and xx is in the range [2,3] 
       */
      return ( prod < 0 ) ? 0 : log(prod) + f_approx_2_3(xx)*(xx-2);
    }
}


/*
 * Using Maple code for approximation of log(GAMMA(x)) in [2,3] 
 *
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) log(GAMMA(x))/(x-2);end proc;

  Digits:=60;
  ggp:=chebpade(f(x),x=2..3,[9,4]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

  f:= proc(x) log(GAMMA(x))/(x-2);end proc;
  Digits:=60;
  ggp:=chebpade(f(x),x=1.5..2,[9,4]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);

  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);
  f:= proc(x) log(GAMMA(x))/(x-1);end proc;
  Digits:=60;
  ggp:=chebpade(f(x),x=1..1.5,[9,4]);
  Digits:=20;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(gg,x);
  codegen[C](f_approx,optimized);

*/

/**
 * cdf_alngam_old:
 * @x: pointer to double 
 * 
 * computes the natural logarithm of gamma(x). 
 * if x <= 6.0, then use recursion to get x below 3 
 * then apply rational approximation number 5236 of 
 * hart et al, computer approximations, John Wiley and sons, ny, 1968. 
 * if x > 6.0, then use recursion to get x to at least 12 and 
 * then use formula 5423 of the same source. 
 * 
 * Returns: a double 
 **/

double cdf_alngam_old (double x)
{
  const int c9 = 9;
  const int c4 = 4;
  const int c5 = 5;

  const double hln2pi=0.91893853320467274178E0;
  const double scoefn[9] =
    { 62.003838007127258804, 36.036772530024836321, 20.782472531792126786,
      6.338067999387272343, 2.15994312846059073, .3980671310203570498, 
      .1093115956710439502, .0092381945590275995, .0029737866448101651 };
  const double scoefd[4] =
    { 62.003838007126989331, 9.822521104713994894, -8.906016659497461257,1. };
  const double coef[5] =
    { .083333333333333023564, -.0027777777768818808, 7.9365006754279e-4,
      -5.94997310889e-4, 8.065880899e-4 };

  int i, n;
  double ret_val, d1, d2, prod, xx, offset;

  if ( x > 6 ) 
    {
      offset = hln2pi;
      if (x > 12.)
	{
	  xx = x;
	}
      else
	{
	  n = 1 + (int) (12. - x);  /* correction bruno */
	  prod = 1.;
	  for (i = 0; i < n; ++i)
	    {
	      prod *= x + ((double) (i));
	    }
	  offset -= log (prod);
	  xx = x + (double) n;
	}
      /* now xx is in the range >= 12  */
      d2 = xx;
      d1 = 1. / (d2 * d2);
      ret_val = cdf_devlpl (coef, c5, d1) / xx;
      ret_val = ret_val + offset + (xx - .5) * log (xx) - xx;
      return ret_val;
    }
  else 
    {
      /* here x <= 6 */
      prod = 1.;
      xx = x;
      /* bring back x <= 3 */
      while ( xx > 3 ) 
	{
	  xx -= 1;
	  prod *=xx;
	}
      /* now bring x >=2 */
      while ( xx < 2 ) 
	{
	  prod /= xx;
	  xx += 1.;
	}
      /* here we have gamma(x)= prod*gamma(xx) 
       * and xx is in the range [2,3] 
       */
      /* rational approximation of GAMMA(X) in [0,1] */
      d1 = xx - 2.;
      d2 = xx - 2.;
      ret_val =  cdf_devlpl (scoefn, c9, d1) / cdf_devlpl (scoefd, c4, d2);
      ret_val *= prod;
      /* return 0 when gamma is < 0 */
      return ( prod < 0 ) ? 0 : log(ret_val);
    }
}
