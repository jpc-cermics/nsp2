/* Nsp
 * Copyright (C) 2007-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * ln(gamma(b)/gamma(a+b)) when b >= 8 a+b too. 
 *
 */

#include <nsp/spmf.h>
#include "cdf.h"

/**
 * cdf_algdiv:
 * @a: a double 
 * @b: a double
 * 
 * computation of ln(gamma(b)/gamma(a+b)) when b >= 8 
 * in this algorithm, del(x) is the function defined by 
 * ln(gamma(x)) = (x - 0.5)*ln(x) - x + 0.5*ln(2*pi) + del(x). 
 *     
 * Note: this approximation is used for b >= 8 and a+b>=8. 
 *      
 * Returns: a double 
 **/


double cdf_algdiv(double a, double b)
{
  double comp=cdf_stirling_series_diff(b,a),u,v;
  /* compute g(b) - g(b+a), where 
   * g:= proc(z) (z-1/2)*log(z) -z;end proc;
   * h:= proc(a,b) a - a*log(b)+ (0.5 -a-b)*log(1+a/b);end proc;
   * 
   * g(b) - g(a+b) == h(a,b);
   */
  /* 
   * return  a - a*log(b)+ (0.5 -a-b)*nsp_log1p(a/b) + comp;
   */
  u = ((a <= b) ? b + (a - .5) : a + (b - .5)) * nsp_log1p (a/b);
  v = a * (log (b) - 1.);
  return (u <= v) ? comp - u - v : comp - v - u;

}




/**
 * cdf_stirling_series_diff:
 * @z: a double 
 * @y: a double 
 * 
 * compute f(z) - f(z+y) where f(x) is the stirling Serie:
 * i.e sum_{n=1}^{infty} b[n-1]*(1/x)^{2*n-1} 
 * Supposed to be used for z large enough (z>=8) 
 * We take care of small y. 
 * 
 * 
 * Returns: a double 
 **/

/*
 * Maple code for log gamma: 
 * f:=proc(z) apply(`+`,seq(bernoulli(2*n)*(1/z)^(2*n-1)/(2*n*(2*n-1)),n=1..6));end proc;
 * g:=proc(z) (z-1/2)*log(z) -z + (1/2)*log(2*Pi) + f(z);end proc;
 * gg:=proc(x) local u1,u2; u1:=evalf(log(GAMMA(x)),70);u2:=evalf(g(x),16);evalf((u1-u2)/u1,70) end proc;
 * g1:= proc(x) - log(x) -log(x+1) +g(x+2);end proc;
 * gg1:=proc(x) local u1,u2; u1:=evalf(log(GAMMA(x)),70);u2:=evalf(g1(x),16);evalf((u1-u2)/u1,70) end proc;
 */

double cdf_stirling_series_diff(double z, double y) 
{
  /* precompute the sequence [seq(bernoulli(2*n)/(2*n*(2*n-1)),n=1..6)] 
   * Maple code: 
   * Digits:=17;nm:=6;convert([seq(bernoulli(2*n)/(2*n*(2*n-1)),n=1..nm)],float);
   */
  #define NN 10
  static const double b[]={.083333333333333333, -.0027777777777777778, 
			   .00079365079365079365, -.00059523809523809524, 
			   .00084175084175084175, -.0019175269175269175,
			   .0064102564102564103, -.029550653594771242, 
			   .17964437236883057, -1.3924322169059011};
  
  /*
   * now we need to compute f(z) - f(z+y) where 
   * f(x) is the stirling Serie sum_{n=1}^{infty} b[n-1]*(1/x)^{2*n-1} 
   * we assume here that z >= y we truncate the serie using 6 terms.
   * 
   * f6(z)-f6(z+y) = sum_{n=1}^{6} b[n-1]*((1/z)^{2*n-1} - (1/(z+y))^{2*n-1})
   *             = sum_{n=1}^{6} b[n-1]*(1/z)^{2*n-1}( 1 - (z/(z+y))^{2*n-1}) 
   *             = sum_{n=1}^{6} b[n-1]*(sn)*(1/z)^{2*n-1}*(1-(z/(z+y)))
   * were sn = ( 1 - (z/(y+z))^{2*n-1})/(1-(z/(z+y))) 
   * s1= 1; s(n+1)= (1+ (z/(y+z))+ (z/(y+z))^2*s(n));
   * 
   */
  double sn[NN]={1.0,0},x,x0,x1,x2,w,w2,res=0.0;
  int i,nn=NN;
  /* x = z/(z+y); */
  x0=(z/y); x0= x0/(1.0 + x0);
  x1=1.0 /(1.0 + (y/z)); 
  x=( y >= z) ? x0 : x1;
  x2=x*x;
  w=(1/z);
  w2=w*w;
  for ( i= 1 ; i < nn; i++) sn[i]= (1+x+x2*sn[i-1]);
  /* horner evaluation of sum_{n=1}^{6} b2n*(1/z)^{2*n-1}(sn) */
  for ( i= nn-1 ; i>=0; i--) 
    res = ( res + sn[i]*b[i])*w2;
  res *= (y*x);
  return  res;
}

/**
 * cdf_stirling_series:
 * @a: a double 
 * 
 * compute stirling series 
 * Returns: a double 
 **/

double cdf_stirling_series(double a) 
{
  /* precompute the sequence [seq(bernoulli(2*n)/(2*n*(2*n-1)),n=1..6)] 
   * Maple code: 
   * Digits:=17;nm:=6;convert([seq(bernoulli(2*n)/(2*n*(2*n-1)),n=1..nm)],float);
   */

  #define NN1 10
  static const double b[]={.083333333333333333, -.0027777777777777778, 
			   .00079365079365079365, -.00059523809523809524, 
			   .00084175084175084175, -.0019175269175269175,
			   .0064102564102564103, -.029550653594771242, 
			   .17964437236883057, -1.3924322169059011};

  /* same as above but sn replaced by 1 */
  double w,w2,res=0.0;
  int i,nn=NN1;
  w=(1/a);
  w2=w*w;
  /* horner evaluation of sum_{n=1}^{6} b2n*(1/z)^{2*n-1}(sn) */
  for ( i= nn-1 ; i>=0; i--) 
    res = ( res + b[i])*w2;
  res *= a ;
  return  res;
}


/**
 * cdf_algdiv:
 * @a: a double 
 * @b: a double
 * 
 * computation of ln(gamma(b)/gamma(a+b)) when b >= 8 
 * in this algorithm, del(x) is the function defined by 
 * ln(gamma(x)) = (x - 0.5)*ln(x) - x + 0.5*ln(2*pi) + del(x). 
 *     
 * Note: this approximation is used for b >= 8 but certainly also 
 * a+b>=8. 
 *      
 * Returns: a double 
 **/

/* acm copyrighted code used for testing only 
 * 
 */

double cdf_algdiv_old (double a, double b)
{
  static const double c0 = .0833333333333333;
  static const double c1 = -.00277777777760991;
  static const double c2 = 7.9365066682539e-4;
  static const double c3 = -5.9520293135187e-4;
  static const double c4 = 8.37308034031215e-4;
  static const double c5 = -.00165322962780713;
  double d1, c, d, h, t, u, v, w, x, s3, s5, s7, x2, s9, s11;

  if (a <= b)
    {
      h = a / b;
      c = h / (h + 1.);
      x = 1. / (h + 1.);
      d = b + (a - .5);
    }
  else 
    {
      h = b / a;
      c = 1. / (h + 1.);
      x = h / (h + 1.);
      d = a + (b - .5);
    }
  x2 = x * x;
  s3 = x + x2 + 1.;
  s5 = x + x2 * s3 + 1.;
  s7 = x + x2 * s5 + 1.;
  s9 = x + x2 * s7 + 1.;
  s11 = x + x2 * s9 + 1.;
  /* SET W = DEL(B) - DEL(A + B) */
  /* Computing 2nd power */
  d1 = 1. / b;
  t = d1 * d1;
  w = ((((c5 * s11 * t + c4 * s9) * t + c3 * s7) * t + c2 * s5) * t + c1 * s3) * t + c0;
  w *= c / b;
  /* COMBINE THE RESULTS */
  d1 = a / b;
  u = d * nsp_log1p (d1);
  v = a * (log (b) - 1.);
  if (u <= v)
    {
      return  w - u - v;
    }
  return  w - v - u;
}
