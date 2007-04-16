#include "cdf.h"


/**
 * cdf_dln1px:
 * @a: a double, value for which ln(1+a) is desired.
 * 
 * Returns ln(1+ @a) for small @a (good accuracy if @a .le. 0.1). 
 * Note that the obvious code log(1.0 + @a) won't work for small @a
 * because 1.0+ @a loses accuracy 
 * 
 *     Renames ALNREL from: 
 *     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant 
 *     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM 
 *     Trans. Math.  Softw. 18 (1993), 360-373. 
 * 
 * Returns: a double 
 **/

double cdf_dln1px_old (double a)
{
  const double p1 = -1.29418923021993;
  const double p2 = .405303492862024;
  const double p3 = -.0178874546012214;
  const double q1 = -1.62752256355323;
  const double q2 = .747811014037616;
  const double q3 = -.0845104217945565;
  double t, w,  t2;

  if (Abs(a) > 0.375)  return log(1+a);
  t = a / (a + 2.);
  t2 = t * t;
  w = (((p3 * t2 + p2) * t2 + p1) * t2 + 1.) 
    / (((q3 * t2 + q2) * t2 + q1) * t2 + 1.);
  return  t * 2. * w;
}	

/* test the original values in the ACM file. 
   
p1 := -1.29418923021993;
p2 := .405303492862024;
p3 := -.0178874546012214;
q1 := -1.62752256355323;
q2 := .747811014037616;
q3 := -.0845104217945565;

g_acm := unapply((((p3 * t2 + p2) * t2 + p1) * t2 + 1.) 
  / (((q3 * t2 + q2) * t2 + q1) * t2 + 1.),t2);
f_approx:= proc(x) local u; u:= x/(2+x);2*u*g_acm(u*u);end proc;
infnorm(f(x)-f_approx(x),x=-0.18..0.18);

m_err(18,100,10);

*/

/* GPL Version, Jean-Philippe Chancelier 2007 */

/** 
 * cdf_dln1px:
 * @a: a double, value for which ln(1+a) is desired.
 * 
 * Returns ln(1+ @a). 
 * Note that the obvious code log(1.0 + @a) won't work for small @a
 * because 1.0+ @a loses accuracy. We obtain here around 6.e-16 relative error.
 * Rational chebishev pade approximation is obtained with Maple
 * with ideas from alnrel and rlog1 from: 
 *     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant 
 *     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM 
 *     Trans. Math.  Softw. 18 (1993), 360-373. 
 * 
 * Returns: a double 
 **/


/* 
 * Using Maple code for approximation in [-0.1,0.1] of 
 * (log(1+x)-log(1-x))/2x giving an approximation 
 * of log(1+x) in [-0.18,0.18].
 * 
 
  with(numapprox);
  with(orthopoly);
  f:= proc(x) log(1+x);end proc;
  g:=proc(x) (f(x)-f(-x))/(2*x);end proc;
  Digits:=50;
  gg:=chebpade(g(x),x=-0.1..0.1,[8,8]);
  Digits:=16;
  gg:= convert(gg,float);
  n:= numer(gg);
  xx:=coeffs(n,x)[1];
  n:=n/xx;
  d:= denom(gg)/xx;
  g_cheb:= unapply(n/d,x);
  f_approx:= proc(x) local u; u:= x/(2+x); 2*u*(g_cheb(u)); end proc;
  infnorm(f(x)-f_approx(x),x=-0.18..0.18);
  codegen[C](f_approx,optimized);
  codegen[C](g_cheb,optimized);
  codegen[C](confracform(n/d));
  
  f_err:=proc(x) local u1,u2; u1:=evalf(f(x),70); u2:=evalf(f_approx(x),17);
    evalf((u1-u2)/u1,70);
  end proc;
  
  m_err:=proc(a,b,nn)
    mv:=a*nn; mvp:=b*nn;
    s:=[seq(i/mvp,i=-mv..-1),seq(i/mvp,i=1..mv)];
    serr:= map(f_err,s);
    convert(max(op(serr)),float);
  end proc;
  
  m_err(18,100,10);
  
*/

double cdf_dln1px (double x)
{
  /* basic test: x=linspace(-0.38,0.56,10000);max(abs(cdf_dln1px(x)-log(1+x)))
   * 
   */
  double res = 0.0,u,u2;
  if (x < -.18)
    {
      if ( x < -0.39 ) return  log (1+x) ;
      /* x is in  [-0.39,-0.18] 
       * log(1+x) = log(1+ (10x+3)/7) - log(10/7);
       */
      x = (x + .3)/0.7;
      res -= 0.35667494393873240;
    }
  else 
    {
      if ( x > 0.57) return log (1+x);
      if ( x > 0.18) 
	{
	  /* [0.18,0.57] 
	   * log(1+x)= log(1 + (3x-1)/4) - log(3/4);
	   */
	  x *= 0.75; x -= 0.25;
	  res -= -0.28768207245178093;
	}
    }
  /* now x is in the range [-0.18,0.18] */
  u = x/(2.0+x);
  u2 = u*u;
  res += 2*u*(1 + ( -0.7793882156769878+0.6815528913297467E-1*u2)*u2)
    /(1 + (-0.1112721549008866E1+0.2390624709752837*u2)*u2);
  return res;
}
