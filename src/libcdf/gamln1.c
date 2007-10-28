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
  return (a <= .6) ? cdf_gamln1px(a) : cdf_gamln2px(a-1);
  /* 
   * better result with : 
   * (a <= .6) ? cdf_gamln1px(a) : (( a <= 0.8) ? lgamma(1+a) : cdf_gamln2px(a-1));
   */
}

/* 
 * Using Maple code for approximation of log(GAMMA(1+x))/x in [-0.2,0.6]
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) log(GAMMA(1+x));end proc;
  g:= proc(x) f(x)/x;end proc;
  Digits:=50;
  am:=-20;ap:=60;ad:=100;
  ggp:=chebpade(g(x),x=(am/ad)..(ap/ad),[8,8]);
  Digits:=16;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(x*gg,x);
  infnorm(f(x)-f_approx(x),x=(am/ad)..(ap/ad));
  codegen[C](f_approx,optimized);
  codegen[C](g_cheb,optimized);
  codegen[C](confracform(n/d));
  
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

*/

static double cdf_gamln1px(double x)
{
  return (x*(-0.2817006634536722
	     +(-0.4105736402455481
	       +(0.8378423180533141E-1
		 +(0.379859104805501
		   +(0.1948356322068602
		     +(0.3249105389946038E-1
		       +0.1304280947267097E-2*x)*x)*x)*x)*x)*x)
	  /(0.4880336425064407
	    +(0.1406692977654196E1
	      +(0.1520447819039188E1
		+(0.7606688426616652
		  +(0.1749446847735022
		    +(0.1564737968074626E-1
		      +0.3196847805872678E-3*x)*x)*x)*x)*x)*x));
}

/*
 * Using Maple code for approximation of log(GAMMA(2+x))/x in [-0.4,0.25]

  with(numapprox);
  with(orthopoly);
  f:= proc(x) log(GAMMA(2+x));end proc;
  g:= proc(x) f(x)/x;end proc;
  Digits:=50;
  am:=-40;ap:=25;ad:=100;
  ggp:=chebpade(g(x),x=(am/ad)..(ap/ad),[8,8]);
  Digits:=16;
  gg:= convert(ggp,float);
  gg:= convert(gg,horner);
  f_approx:= unapply(x*gg,x);
  infnorm(f(x)-f_approx(x),x=(am/ad)..(ap/ad));
  codegen[C](f_approx,optimized);
  codegen[C](g_cheb,optimized);
  codegen[C](confracform(n/d));
  
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
  
*/

static double cdf_gamln2px(double x)
{
  return(x*(0.4509088339066821
	    +(0.9035354359359669
	      +(0.6012062835551742
		+(0.1660902456536165
		  +(0.1803476058196925E-1
		    +0.5241953151972274E-3*x)*x)*x)*x)*x)
	 / (0.1066522092881385E1
	    +(0.1323647008900873E1
	      +(0.5823453139920647
		+(0.107629022644866+
		  (0.7534275524800251E-2
		   +0.122129001350747E-3*x)*x)*x)*x)*x));
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




