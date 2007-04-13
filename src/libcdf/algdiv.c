#include "cdf.h"


/**
 * cdf_algdiv:
 * @a: a double 
 * @b: a double
 * 
 *     computation of ln(gamma(b)/gamma(a+b)) when b >= 8 
 *     in this algorithm, del(x) is the function defined by 
 *     ln(gamma(x)) = (x - 0.5)*ln(x) - x + 0.5*ln(2*pi) + del(x). 
 *     
 * Cette approximation est utilisée pour b >= 8 mais il faut sans doute 
 * aussi que a+b>=8 i.e 
 *      
 * Returns: a double 
 **/

double cdf_algdiv (double a, double b)
{
  /* test
     bs=9;a=-(bs-2):0.1:bs;
     b=bs*ones(a);norm(cdf_algdiv(a,bs*ones(a))  - log(gamma(b)./gamma(a+b)),'inf')
  */
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
  u = d * cdf_alnrel (d1);
  v = a * (log (b) - 1.);
  if (u <= v)
    {
      return  w - u - v;
    }
  return  w - v - u;
}


double cdf_algdiv_new (double a, double b)
{
  double comp=cdf_stirling_series_diff(b,a);
  /* compute g(b) - g(b+a), where 
   * g:= proc(z) (z-1/2)*log(z) -z;end proc;
   * h:= proc(a,b) a - a*log(b)+ (0.5 -a-b)*log(1+a/b);end proc;
   * 
   * g(b) - g(a+b) == h(a,b);
   */
  return  a - a*log(b)+ (0.5 -a-b)*cdf_alnrel(a/b) + comp;
}


/* GPL Copyright Chancelier Jean-Philippe 
 * 
 * compute f(z) - f(z+y) where f(x) is the stirling Serie:
 * i.e sum_{n=1}^{infty} b[n-1]*(1/x)^{2*n-1} 
 * Supposed to be used for z large enough (z>=8) 
 * We take care of small y. 
 *
 * f:=proc(z) apply(`+`,seq(bernoulli(2*n)*(1/z)^(2*n-1)/(2*n*(2*n-1)),n=1..6));end proc;
 * 
 * Maple code for log gamma: 
 * f:=proc(z) apply(`+`,seq(bernoulli(2*n)*(1/z)^(2*n-1)/(2*n*(2*n-1)),n=1..6));end proc;
 * g:=proc(z) (z-1/2)*log(z) -z + (1/2)*log(2*Pi) + f(z);end proc;
 * gg:=proc(n); evalf(g(n)-log((n-1)!),40); end proc;
 */

double cdf_stirling_series_diff(double z, double y) 
{
  /* precompute the sequence [seq(bernoulli(2*n)/(2*n*(2*n-1)),n=1..6)] 
   * Maple code: 
   * Digits:=17;nm:=6;convert([seq(bernoulli(2*n)/(2*n*(2*n-1)),n=1..nm)],float);
   */
  #define NN 6
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
  double sn[NN]={1.0,0},x=(z/(z+y)),x2=x*x,w=(1/z),w2=w*w,res;
  int i,nn=NN;
  for ( i= 1 ; i < nn; i++) sn[i]= (1+x+x2*sn[i-1]);
  /* horner evaluation of sum_{n=1}^{6} b2n*(1/z)^{2*n-1}(sn) */
  res = sn[nn-1]*b[nn-1]*w2;
  for ( i= nn-2 ; i>=0; i--) res = ( res + sn[i]*b[i])*w2;
  res *=( y*z/(z+y));
  return  res;
}

