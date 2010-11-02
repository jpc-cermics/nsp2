/* Nsp
 * Copyright (C) 2007-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 * a version of exp(x) - 1 approximated with Mapple 
 *
 */

#include "cdf.h"

/** 
 * cdf_rexp:
 * @x: a double 
 * 
 * exp(x)-1
 * 
 * Returns: a double 
 **/

extern int nsp_use_extended_fpu();

double cdf_rexp(double x)
{
  double y;
  if ( x < -0.015 ||  x > 0.015 )
    {
      y = exp(x)-1;
    }
  else 
    {
      /* pade approximation computed with 
       * Maple (see code below).
       */
      y= x*(0.9999879465639932
		   +(0.1681565684988593E-17
		     +0.2380922406793938E-1*x)*x)
	/(0.9999879465639932
	  +(-0.4999939732819966
	    +(0.1071415529482722
	      +(-0.1190461203396969E-1+0.5952298576585598E-3*x)*x)*x)*x);
      /* 
       * improve the result: only usefull when 
       * pure 64bits is used 
       */
    }
#ifdef __linux__ 
  if ( nsp_use_extended_fpu() == FALSE && Abs(x) > 0.01  ) 
    {
      /* improve by newton step : 
       *     Copyright (C) 2002 The R Development Core Team
       */
      y -= (1.0 + y) * (nsp_log1p(y) - x);
    }
#endif 
  return y;
} 

/* 
 * Using Maple code for approximation 
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) exp(x)-1;end proc;
  g:= proc(x) f(x)/x;end proc;
  Digits:=50;
  am:=-15;ap:=15;ad:=1000;
  ggp:=chebpade(g(x),x=(am/ad)..(ap/ad),[2,4]);
  Digits:=17;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(x*gg,x);
  infnorm(f(x)-f_approx(x),x=(am/ad)..(ap/ad));
  codegen[C](f_approx,optimized);
  codegen[C](confracform(x*gg));
  
  f_err:=proc(x) local u1,u2; u1:=evalf(f(x),70); u2:=evalf(f_approx(x),17);
    evalf((u1-u2)/u1,70);
  end proc;
  
  m_err:=proc(am,ap,b,nn)
    mvm:=am*nn;mvp:=ap*nn;mvd:= b*nn;
    s:=[seq(i/mvd,i=mvm..-1),seq(i/mvd,i=1..mvp)];
    serr:= map(f_err,s);
    convert(max(op(serr)),float);
  end proc;
  
  m_err(am,ap,ad,1);

  # in [0.15,0.30] 

  f_1:= unapply(x*gg,x);
  f_approx := proc(x) (f_1(x/2))*(f_1(x/2)+2);end proc;

  m_err1:=proc(am,ap,b,nn)
    mvm:=am*nn;mvp:=ap*nn;mvd:= b*nn;
    s:=[seq(i/mvd,i=mvm..mvp)];
    serr:= map(f_err,s);
    convert(max(op(serr)),float);
  end proc;

  m_err(15,30,100,1);
  m_err(-30,-15,100,10);

*/


/* test original code with Maple 
 *
 *

p1 := 9.14041914819518e-10;
p2 := .0238082361044469;
q1 := -.499999999085958;
q2 := .107141568980644;
q3 := -.0119041179760821;
q4 := 5.95130811860248e-4;

f_approx:= unapply(x *(((p2*x + p1)*x + 1.) /((((q4*x + q3)*x + q2)*x + q1)*x + 1.)),x);
infnorm(f(x) - f_approx(x),x=-0.15..0.15);


*/

/**
 * cdf_rexp_old:
 * @x: a double 
 * 
 * exp(x)-1
 * 
 * Returns: a double 
 **/

/* unused ACM code just here for test comparisons. 
 *
 */

double cdf_rexp_old (double x)
{
  const double p1 = 9.14041914819518e-10;
  const double p2 = .0238082361044469;
  const double q1 = -.499999999085958;
  const double q2 = .107141568980644;
  const double q3 = -.0119041179760821;
  const double q4 = 5.95130811860248e-4;

  double w;
  if (Abs (x) > .15)
    {
      w = exp (x);
      if (x > 0.)
	{
	  return w * (.5 - 1. / w + .5);
	}
      else 
	{
	  return  w - .5 - .5;
	}
    }
  else 
    {
      return   x *(((p2*x + p1)*x + 1.) /
		   ((((q4*x + q3)*x + q2)*x + q1)*x + 1.));
    }
}
