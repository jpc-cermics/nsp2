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

static double cdf_rexp_(double x);

double cdf_rexp(double x)
{
  double w;
  if ( x > 0.15 ) 
    {
      if ( x > 0.30 ) return exp(x)-1;
      /* exp(x)-1= (exp(x/2)-1)*(exp(x/2)+1) */
      w = cdf_rexp_(x/2); 
      return w*(w+2);
    }
  else 
    {
      if ( x < -0.30) return exp(x)-1;
      if ( x > -0.15) return cdf_rexp_(x);
      /* exp(x)-1= (exp(x/2)-1)*(exp(x/2)+1) */
      w = cdf_rexp_(x/2); 
      return w*(w+2);
    }
}

static double cdf_rexp_(double x)
    {
#ifdef HORNER 
      return (x*(0.9987959956059006
		 +(0.1680067132994839E-11
		   +0.2377958241890412E-1*x)*x)
	      /(0.9987959956059007
		+(-0.4993979978012702
		  +(0.107012582051777
		    +(-0.1188979120931212E-1
		      +0.5944152325789097E-3*x)*x)*x)*x));
#else 
      /* continued fraction form */
      return ( 0.4000500174892026E2
	       / (x-0.2000250087429536E2+0.1380277617472468E3
		  /(x+0.1003149949888885E-9+0.298286123203475E2
		    /(x+0.1127717937405408E-10+0.121736389391988E2
		      /(x-0.4094050759274607E-10)))));
#endif 
}


/* 
 * Using Maple code for approximation 
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) exp(x)-1;end proc;
  g:= proc(x) f(x)/x;end proc;
  Digits:=50;
  am:=-15;ap:=15;ad:=100;
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
