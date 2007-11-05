#include "cdf.h"



/**
 * cdf_grat1:
 * @a: 
 * @x: 
 * @r: 
 * @p: 
 * @q: 
 * @eps: 
 * 
 * evaluation of the incomplete gamma ratio functions 
 * p(a,x) and q(a,x) 
 * it is assumed that a <= 1. 
 * eps is the tolerance to be used. 
 * the input argument r has the value e**(-x)*x**a/gamma(a). 
 * 
 * 
 * Returns: an integer.
 **/

int cdf_grat1_old (double *a, double *x, double *r, double *p, double *q, double *eps)
{
  const int c0 = 0;
  double d1;
  double a2nm1, b2nm1;
  double c, g, h, j, l, t, w, z, an, am0, an0, a2n, b2n, cma;
  double tol, sum;

  if (*a * *x == 0.)
    {
      if (*x <= *a)
	{
	  *p = 0.;
	  *q = 1.;
	  return 0;
	}
      else
	{
	  *p = 1.;
	  *q = 0.;
	  return 0;
	}
    }
  if (*a == .5)
    {
      if (*x >= .25)
	{
	  d1 = sqrt (*x);
	  *q = cdf_erfc (c0, d1);
	  *p = .5 - *q + .5;
	  return 0;
	}
      else 
	{
	  d1 = sqrt (*x);
	  *p = cdf_erf (d1);
	  *q = .5 - *p + .5;
	  return 0;
	}
    }

  if (*x >= 1.1)
    {
      /*              CONTINUED FRACTION EXPANSION */
      a2nm1 = 1.;
      a2n = 1.;
      b2nm1 = *x;
      b2n = *x + (1. - *a);
      c = 1.;
      while (1) 
	{
	  a2nm1 = *x * a2n + c * a2nm1;
	  b2nm1 = *x * b2n + c * b2nm1;
	  am0 = a2nm1 / b2nm1;
	  c += 1.;
	  cma = c - *a;
	  a2n = a2nm1 + cma * a2n;
	  b2n = b2nm1 + cma * b2n;
	  an0 = a2n / b2n;
	  if (!((d1 = an0 - am0, Abs (d1)) >= *eps * an0)) break;
	}
      *q = *r * an0;
      *p = .5 - *q + .5;
      return 0;
    }
  /*             TAYLOR SERIES FOR P(A,X)/X**A */

  an = 3.;
  c = *x;
  sum = *x / (*a + 3.);
  tol = *eps * .1 / (*a + 1.);
  while (1) 
    {
      an += 1.;
      c = -c * (*x / an);
      t = c / (*a + an);
      sum += t;
      if (! (Abs (t) > tol)) break;
    }

  j = *a * *x * ((sum / 6. - .5 / (*a + 2.)) * *x + 1. / (*a + 1.));

  z = *a * log (*x);
  h = cdf_gam1 (*a);
  g = h + 1.;
  if (*x < .25)
    {
      goto L30;
    }
  if (*a < *x / 2.59)
    {
      goto L50;
    }
  goto L40;
 L30:
  if (z > -.13394)
    {
      goto L50;
    }
 L40:
  w = exp (z);
  *p = w * g * (.5 - j + .5);
  *q = .5 - *p + .5;
  return 0;
 L50:
  l = cdf_rexp (z);
  w = l + .5 + .5;
  *q = (w * j - l) * g - h;
  if (*q < 0.)
    {
      *p = 1.;
      *q = 0.;
      return 0;
    }
  *p = .5 - *q + .5;
  return 0;


}			


/**
 * cdf_grat1:
 * @a: 
 * @x: 
 * @r: 
 * @p: 
 * @q: 
 * @eps: 
 * 
 * evaluation of the incomplete gamma ratio functions 
 * p(a,x) and q(a,x) 
 * it is assumed that a <= 1. 
 * eps is the tolerance to be used. 
 * the input argument r has the value e**(-x)*x**a/gamma(a). 
 * 
 * 
 * Returns: an integer.
 **/

static double igamma_continued_fraction(double a, double z, double eps);
static double igamma_taylor(double a, double z,double tol);

int cdf_grat1 (double *a, double *x, double *r, double *p, double *q, double *eps)
{
  double g, h, j, l, w, z, an0,  tol; 

  if (*a * *x == 0.)
    {
      if (*x <= *a)
	{
	  *p = 0.;
	  *q = 1.;
	  return 0;
	}
      else
	{
	  *p = 1.;
	  *q = 0.;
	  return 0;
	}
    }
  if (*a == .5)
    {
      if (*x >= .25)
	{
	  *q = cdf_erfc (0,sqrt (*x));
	  *p = .5 - *q + .5;
	  return 0;
	}
      else 
	{
	  *p = cdf_erf (sqrt (*x));
	  *q = .5 - *p + .5;
	  return 0;
	}
    }
  
  if (*x >= 2.0 )
    {
      an0= igamma_continued_fraction(*a,*x,*eps);
      *q = *r * an0;
      *p = .5 - *q + .5;  
      return 0;
    }
  /* taylor expansion */
  tol = *eps * .1 / (*a + 1.);
#if 1
  *p = exp((*a * log (*x)))* igamma_taylor( *a, *x, tol);
  /* gamma(x)= (*a)*(cdf_gam1(*a)+1) */
  *p = (*a)*(cdf_gam1(*a)+1)*(*p);
  *q = .5 - *p + .5;
  return 0;
#endif
  j = (*a)* igamma_taylor( *a, *x, tol); 
  z = *a * log (*x);
  h = cdf_gam1 (*a);
  g = h + 1.;
  if (*x < .25)
    {
      goto L30;
    }
  if (*a < *x / 2.59)
    {
      goto L50;
    }
  goto L40;
 L30:
  if (z > -.13394)
    {
      goto L50;
    }
 L40:
  w = exp (z);
  *p = w * g * (.5 - j + .5);
  *q = .5 - *p + .5;
  return 0;
 L50:
  l = cdf_rexp (z);
  w = l + .5 + .5;
  *q = (w * j - l) * g - h;
  if (*q < 0.)
    {
      *p = 1.;
      *q = 0.;
      return 0;
    }
  *p = .5 - *q + .5;
  return 0;
}			


/*
 * \gamma(a,x) = \int_0^x t ^(a-1) exp(-t) dt 
 * \gamma(1/2,x) = sqrt(%pi)*erfc(sqrt(x)) 
 *
 * regularized gamma: P(a,x) = \gamma(a,x)/\Gamma(a) 
 * Abramovitz Stegun gives 
 * 1/taylor expansion of x^(-a) \gamma(a,x)= \sum_{n=0}^\infty (-x)^n/((a+n)n!);
 * 2/Continued fraction:
 * \Gamma(a,x)= \Gamma(a) - \gamma(a,x) 
 *
 * exp(x)*x^(-a)\Gamma(a,x) =  b_0 + K_{i=1}^\infty a_i/b_i 
 * and the sequences 
 * b= 0,[ x, 1, x,1,x,1,x,1,....]
 * a=   [ 1, 1-a,1, 2-a, 2, 3-a , 3,....
 *
 * By A_n/B_n with the recursion 
 * A_{n} = (b_{n}A_{n-1} + a_{n} A_{n-2}
 * B_{n} = (b_{n}B_{n-1} + a_{n} B_{n-2}
 * 
 * with A_{-1}=1, A_0 = b_0
 *      B_{-1}=0, B_0 = 1
 * 

 */

static double igamma_continued_fraction(double a, double x, double eps)
{
  double b[]= {0,x};
  double aa[]={0,1};
  double A0=b[0],B0=1;
  double A1 = b[1]*A0 + aa[1];
  double B1 = b[1]*B0;
  double an = (1-a);  
  double anp1 = 1; 
  double Anm1=A1,Anm2=A0,Bnm1=B1,Bnm2=B0;
  double An,Bn,Anp1,Bnp1,xn,xnp1;
  while (1) 
    {
      An = Anm1 + an*Anm2;
      Bn = Bnm1 + an*Bnm2;
      xn = An/Bn;
      Anp1 = x*An + anp1*Anm1;
      Bnp1 = x*Bn + anp1*Bnm1;
      xnp1 = Anp1/Bnp1;
      if ( !( Abs(xn-xnp1) >= eps*xn ) ) break;
      Anm2=An; Bnm2=Bn;
      Anm1=Anp1; Bnm1=Bnp1;
      an += 1;
      anp1 +=1;
    }
  return  xnp1;
}

/* taylor expansion of  x^(-a) \gamma(a,x) 
 *   = \sum_{n=0}^\infty (-x)^n/((a+n)n!);
 *
 */

static double igamma_taylor(double a, double z,double tol)
{
  double res = 1/a ,an=a+1, bn=1, x=-z,n=1, res1;
  while (1)
    {
      res1 = x/(an*bn);
      res += res1;
      if ( Abs(res1) < res*tol) break;
      x *= -z;
      an += 1;
      n  += 1;
      bn *= n;
    }
  return res;
}


