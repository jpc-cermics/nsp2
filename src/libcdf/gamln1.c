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
 *
 * a version of LN(GAMMA(1 + A)) for -0.2 <= A <= 1.25  approximated with Mapple 
 *
 */

#include "cdf.h"
/**
 * cdf_gamln1:
 * @a: 
 * 
 * computes LN(GAMMA(1 + A)) for -0.2 <= A <= 1.25 
 * chebyshev pade approximation using Maple are used used 
 * 
 * 
 * Returns: a double 
 **/

static double cdf_gamln1px(double x);
static double cdf_gamln2px(double x);

double cdf_gamln1 (double a)
{
  return (a <= .7) ? cdf_gamln1px(a) : cdf_gamln2px(a-1);
}

/* 
 * Using Maple code for approximation of log(GAMMA(1+x))/x in [-0.2,0.6]
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) log(GAMMA(1+x));end proc;
  g:= proc(x) f(x)/x;end proc;
  Digits:=60;
  ggp:=chebpade(g(x),x=(-0.2)..(0.65),[7,7]);
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(x*gg,x);
  codegen[C](f_approx,optimized);

*/
double cdf_gamln1px(double x)
{
  {
    return(x*(-0.2198906661819617
	      +(-0.4233198418459166
		+(-0.7013596901756501E-1
		  +(0.3408062242957307
		    +(0.280483198889643
		      +(0.8269644814016514E-1
			+(0.9108065849360645E-2
			  +0.2525398702821731E-3*x)*x)*x)*x)*x)*x)*x)
	   /(0.3809506213236137
	     +(0.1276194001721497E1
	       +(0.1675495107148691E1
		 +(0.108964227437159E1
		   +(0.3649845163183046
		     +(0.5920285367936551E-1
		       +(0.3837461924420116E-2
			 +0.576004574077233E-4*x)*x)*x)*x)*x)*x)*x));
  }
}

/*
 * Using Maple code for approximation of log(GAMMA(2+x))/x in [-0.4,0.25]

  with(numapprox);
  with(orthopoly);
  f:= proc(x) log(GAMMA(2+x));end proc;
  g:= proc(x) f(x)/x;end proc;
  Digits:=90;
  ggp:=chebpade(g(x),x=(-0.4)..(0.25),[7,7]);
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(x*gg,x);
  codegen[C](f_approx,optimized);
  
*/

static double cdf_gamln2px(double x)
{
  {
    return(x*(0.4522844244709163
	      +(0.1133932736059987E1
		+(0.1073636543983591E1
		  +(0.4990574663122923
		    +(0.1210741834314334
		      +(0.1477101621206509E-1
			+(0.7774143441506743E-3
			  +0.1134764538089259E-4*x)*x)*x)*x)*x)*x)*x)
	   /(0.1069775738889613E1
	     +(0.186611769077824E1
	       +(0.1286535286877397E1
		 +(0.4443464535670653
		   +(0.802605075180054E-1
		     +(0.7165776126953171E-2
		       +(0.2628459447187425E-3+0.2319923649253971E-5*x)*x)*x)*x)*x)*x)*x));
  }
}

/**
 * cdf_gamln1_old:
 * @a: 
 * 
 * computes LN(GAMMA(1 + A)) for -0.2 <= A <= 1.25 
 * 
 * Returns: a double 
 **/

/* unused ACM code just here for test comparisons. 
 *
 */

double cdf_gamln1_old (double a)
{
  /* a= -0.2:0.01:1.25; norm(log(gamma(1+a))- cdf_gamln1(a)) */
  const double p0 = .577215664901533;
  const double q3 = 1.56875193295039;
  const double q4 = .361951990101499;
  const double q5 = .0325038868253937;
  const double q6 = 6.67465618796164e-4;
  const double r0 = .422784335098467;
  const double r1 = .848044614534529;
  const double r2 = .565221050691933;
  const double r3 = .156513060486551;
  const double r4 = .017050248402265;
  const double r5 = 4.97958207639485e-4;
  const double p1 = .844203922187225;
  const double s1 = 1.24313399877507;
  const double s2 = .548042109832463;
  const double s3 = .10155218743983;
  const double s4 = .00713309612391;
  const double s5 = 1.16165475989616e-4;
  const double p2 = -.168860593646662;
  const double p3 = -.780427615533591;
  const double p4 = -.402055799310489;
  const double p5 = -.0673562214325671;
  const double p6 = -.00271935708322958;
  const double q1 = 2.88743195473681;
  const double q2 = 3.12755088914843;
  double w, x;
  if (a >= .6)
    {
      x = (a) - .5 - .5;
      w =  (((((r5*x + r4)*x + r3)*x + r2)*x + r1)*x + r0) 
	/ (((((s5*x + s4)*x + s3)*x + s2)*x + s1)*x + 1.);
      return  x*w;
    }
  else 
    {
      w = ((((((p6*(a) + p5)*(a) + p4)*(a) + p3)*(a) + p2)*(a) + p1)*(a) + p0) 
	/ ((((((q6*(a) + q5)*(a) + q4)*(a) + q3)*(a) + q2)*(a) +  q1)*(a) + 1.);
      return  -((a)) * w;
    }
}




