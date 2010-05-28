/* Nsp
 * Copyright (C) 2006-2009 Bruno Pincon Esial/Iecn
 * nsp_expm1 code: Copyright (C) 2002 The R Development Core Team
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
 */

#include <limits.h>
#include <float.h>
#include <nsp/math.h> 
#include <nsp/sciio.h>
#include <nsp/spmf.h>
#include <string.h>
#include "../libcdf/cdf.h"

/**
 * SECTION:spmf
 * @Title: Libspmf
 * @Short_Description: special mathematical functions
 * @Stability_Level: 
 * @See_Also: 
 *
 * 
 */


/*
 *     PURPOSE :  Compute   v = log ( (1 + s)/(1 - s) ) 
 *        for small s, this is for |s| < 0.334
 *     ALGORITHM : 
 *     1/ if |s| is "very small" we use a truncated 
 *        taylor dvp (by keeping 4 terms) from : 
 *
 *        t = 2 * s * ( 1 + 1/3 s^2  + 1/5 s^4  +  1/7 s^6   [+  1/9 s^8 .... ]) 
 *
 *        the relative error er is about 1/9 s^8, er <= epsm leads to
 *         |s| <= (9 epsm)^(1/8) which is near 0.0133. We take 0.01
 *        as limit for this formula.
 *
 *     2/ for 0.01 < |s| <= 0.2  we used a minimax polynome : 
 *        yi = s * (2  + d3 s^3 + d5 s^5 .... + d13 s^13 + d15 s^15) 
 *        computed (by some remes algorithm) following 
 *        (*) the sin(x) example (p 39) of the book : 
 *         "ELEMENTARY FUNCTIONS" 
 *         "Algorithms and implementation" 
 *         J.M. Muller (Birkhauser) 
 *        (*) without the additionnal raffinement to get the first coefs 
 *         very near floating point numbers) 
 *
 *     3/ for 0.2 < |s| <= 0.334 we used another minimax polynome
 *        yi = s * (2  + e3 s^2 + e5 s^4 + .... + e15 s^15 + e17 s^17) 
 *        got less or more empiricaly with the pari-gp software.
 *        
 */

double lnp1m1(double s)
{
  double  s2;
  const double E = 1.e-2, C3  = 2.0/3.0, C5  = 2.0/5.0, C7 = 2.0/7.0;
  const double 
    D3 = 0.66666666666672679472E0, D5 = 0.39999999996176889299E0,
    D7 = 0.28571429392829380980E0, D9 = 0.22222138684562683797E0,
    D11= 0.18186349187499222459E0, D13= 0.15250315884469364710E0,
    D15= 0.15367270224757008114E0, E1 = 2.000000000009506159978903118, 
    E3 = 0.6666666653952079503982298174, E5 = 0.4000000741877232817059141633,
    E7 = 0.2857118089055873685381586584, E9 = 0.2222743062829225981951620178,
    E11 =0.1811042136269037116837648205, E13 =0.1601985962777479638905952230,
    E15 =0.0983334292284553038580498852, E17 =0.2223610247590649232825929055;

  s2 = s * s;
  if (fabs(s) <=  E) 
    return  s * (2E0 + s2*(C3 + s2*(C5 + s2*C7)));
  else if ( fabs(s) <= 0.2 )
    return s * (2.E0 + s2*(D3 + s2*(D5 + s2*( D7 + s2*(D9 + s2*(D11 + s2*(D13 + s2*D15)))))));
  else
    return s * (E1 + s2*(E3 + s2*(E5 + s2*( E7 + s2*(E9 + s2*(E11 + s2*(E13 + s2*(E15 + s2*E17))))))));
}

/**
 * nsp_log1p:
 * @x:  a double 
 * 
 * computes log(1+x) with accuracy near x = 0
 * 
 * Returns: a double
 **/
double nsp_log1p(double x)
{
  if ( -0.5 <= x && x <= 1.0) 
    {
      /* use the function log((1+g)/(1-g)) with g = x/(x + 2) */
      return lnp1m1( x / (x + 2.0));
    } 
  else 
    {
      /* use the standard formula */
      return log(x + 1.0);
    }
} 


/**
 * nsp_expm1:
 * @x:  a double 
 * 
 * computes exp(x)-1 with accuracy near x = 0
 * code origin:
 *     Mathlib : A C Library of Special Functions
 *     Copyright (C) 2002 The R Development Core Team
 * 
 * Returns: a double
 **/
double nsp_expm1(double x)
{
    double y, a = fabs(x);

    if (a < DBL_EPSILON) return x;
    if (a > 0.697) return exp(x) - 1.0;  /* negligible cancellation */

    /* initial value for the Newton step: */
    if (a > 1e-8)
	y = exp(x) - 1.0;
    else /* Taylor expansion, more accurate in this range */
	y = (0.5*x + 1.0) * x;

    /* Newton step for solving   log(1 + y) = x   for y : */
    y -= (1.0 + y) * (nsp_log1p(y) - x);
    return y;
}


/**
 * nsp_sinpi:
 * @x: a double 
 * 
 * computes sin(pi x) with accuracy even for |x| big
 * 
 * Returns: a double
 **/
double nsp_sinpi(double x)
{
  unsigned int n;
  int odd;
  double a, t, res;
  static double
    /* sinus polynomial approx (computed with pari-gp) */
    A1 =  3.141592653589793238462643383,  /* Pi in fact */
    A3 = -5.167712780049960279993726290,
    A5 =  2.550164039874133978126524173,
    A7 = -0.5992645289803753056280813802,
    A9 =  0.08214587022916833811426663266,
    A11= -0.007370034512922470592483313766,
    A13=  0.0004615931209034139599168587169,
    /* cosinus polynomial approx (computed with pari-gp) */
    B2 = -4.934802200544630057169486610,
    B4 =  4.058712126400859221869361610,
    B6 = -1.335262767189179924892238186,
    B8 =  0.2353305509220838948431767241,
    B10 =-0.02580498183722295980687186645,
    B12 = 0.001907004403665643602371240441;

  a = fabs(x);

  if ( a <= 2.0 )
    {
      if ( a <= 1.0 )
	odd = 0;
      else
	{
	  a -= 1.0; odd = 1;
	}
    }
  else
    {
      t = floor(a);
      if ( t <= UINT_MAX )
	{
	  n = (unsigned int) t;
	  odd = n%2;
	}
      else
	{
	  odd = 0.0 != t - 2.0*floor(0.5*t);
	}
      a -= t;
    }

  if ( 0.25 <= a  &&  a <= 0.75 )
    {
      a = 0.5 - a;
      t = a*a;
      res = 1.0 + t*(B2 + t*(B4 + t*(B6 + t*(B8 + t*(B10 + t*B12)))));
    }
  else  /* 0 <= a < 0.25  or  0.75 < a < 1.0 */
    {
      if ( a > 0.75 ) a = 1.0 - a;
      t = a*a;
      res = a*(A1 + t*(A3 + t*(A5 + t*(A7 + t*(A9 + t*(A11 + t*A13))))));
    }

  if ( x < 0.0 )
    res = -res;
  if ( odd )
    res = -res;

  return res;
}


/**
 * nsp_stirling_error:
 * @x: a double 
 * 
 *  computes the error w(x) = ln(gamma(x)) - {0.5*log(2*Pi) + (x-0.5)*log(x) - x} 
 *  using Cody 's approximation (specfun lib available at Netlib) 
 * 
 * Returns: a double
 **/
double  nsp_stirling_error(double x)
{
  static const double C0=-1.910444077728e-03,          C1=8.4171387781295e-04,
    C2=-5.952379913043012e-04,       C3=7.93650793500350248e-04,
    C4=-2.777777777777681622553e-03, C5=8.333333333333333331554247e-02,
    C6=5.7083835261e-03;
  double t;
  t = 1.0/x; t = t*t;
  return ((((((C6*t + C0)*t + C1)*t + C2)*t + C3)*t + C4)*t + C5)/x;
}

/* 
 *  Evaluate gamma for x in [1,2]. This uses the rationnal approximation
 *  of the Cody 's gamma fortran code (specfun lib available at Netlib) 
 */
static double gamma_in_1_2(double x)
{
  static const double P[] = {-1.71618513886549492533811    ,  2.47656508055759199108314e+1,
			     -3.79804256470945635097577e+2 ,  6.29331155312818442661052e+2,
			     8.66966202790413211295064e+2 , -3.14512729688483675254357e+4,
			     -3.61444134186911729807069e+4 ,  6.64561438202405440627855e+4 };

  static const double Q[] = {-3.08402300119738975254353e+1 ,  3.15350626979604161529144e+2,
			     -1.01515636749021914166146e+3 , -3.10777167157231109440444e+3,
			     2.25381184209801510330112e+4 ,  4.75584627752788110767815e+3,
			     -1.34659959864969306392456e+5 , -1.15132259675553483497211e+5 };

  double xnum = 0.0, xden = 1.0;
  int i;
  x -= 1.0;
  for ( i = 0 ; i < 8 ; i++ )
    {
      xnum = (xnum + P[i])*x;
      xden = xden*x + Q[i];
    }
  return (xnum/xden + 1.0);
}

static double gamma_for_x_big(double x)
{
  static double const sqrt_twopi = 2.506628274631000502415765284;
  double w, q, res;

  w = nsp_stirling_error(x);
  /*   we have to compute sqrt(2Pi) x^(x-0.5) exp(-x) exp(w(x)) */
  /*   the "factorisation" by exp: exp( 0.5*log(2Pi) + (x-0.5)log(x) - x + w(x) ) is bad */
  /*   use the gsl method: the trick is that pow seems quite accurate in fact */
  q = pow(x,0.5*x);
  res = (q*exp(-x))*q;
  res = (sqrt_twopi * (res/sqrt(x))) * exp(w);
  return res;
}

/**
 * nsp_gamma:
 * @x: a double 
 * 
 * computes gamma(x) with accuracy and normally with good special values
 * (Nan for x a negative integer, -Inf for x = -0, + Inf for x = +0)
 * overflow limit is near 171.624.
 *
 * Returns: a double
 **/
double nsp_gamma(double x)
{
 static double const cst1 = DBL_EPSILON, cst2 = 15;
 double abs_x, xx;
 double prodxx;   /* long double prodxx; */

 abs_x = fabs(x);

 if ( x < 0.0  &&  floor(x) == x )
   return 0.0/0.0;   /* NaN */
   
 else if ( abs_x <= cst1 )
   return 1.0/x;
    
  else if ( abs_x <= cst2 )
    {
      if ( x > 2.0 )
	{
	  xx = x-1; prodxx = xx;
	  while ( xx > 2.0 )
	    {
	      xx -= 1.0;
	      prodxx *= xx;
	    }
	  return prodxx*gamma_in_1_2(xx);
	}
      else if ( x < 1.0 )
	{
	  prodxx = x; xx = x+1.0;
	  while ( xx < 1.0 )
	    {
	      prodxx *= xx;
	      xx += 1.0;
	    }
	  return gamma_in_1_2(xx)/prodxx;
	}
      else
	return gamma_in_1_2(x);
    }
  else
    {
      if ( x > 0.0 )
	return gamma_for_x_big(x);
      else
	return M_PI/(nsp_sinpi(x)*gamma_for_x_big(1.0-x));
    }     
}

/* approximation for lngamma in [4,12] translation in C of the Cody 's code */
static double lngamma_in_4_12(double x)
{
  double const 
    D = 1.791759469228055000094023,
    P[] = {1.474502166059939948905062e4, 2.426813369486704502836312e6,
	   1.214755574045093227939592e8, 2.663432449630976949898078e9,
	   2.940378956634553899906876e10,1.702665737765398868392998e11,
	   4.926125793377430887588120e11,5.606251856223951465078242e11},
    Q[] = {2.690530175870899333379843e3, 6.393885654300092398984238e5,
           4.135599930241388052042842e7, 1.120872109616147941376570e9,
	   1.488613728678813811542398e10,1.016803586272438228077304e11,
	   3.417476345507377132798597e11,4.463158187419713286462081e11};
    double xm4, xnum, xden;
    int i;
    xm4 = x - 4.0;
    xden = -1.0;
    xnum = 0.0;
    for ( i = 0 ; i < 8 ; i++ )
      {
	xnum = xnum*xm4 + P[i];
	xden = xden*xm4 + Q[i];
      }
    return D + xm4*(xnum/xden);
}

/* approximation for lngamma in [1.5,4] translation in C of the Cody 's code */
static double lngamma_in_1p5_4(double x)
{
  double const 
    D = 4.227843350984671393993777e-1,
    P[] = {4.974607845568932035012064,  5.424138599891070494101986e2,
	   1.550693864978364947665077e4,1.847932904445632425417223e5,
	   1.088204769468828767498470e6,3.338152967987029735917223e6,
	   5.106661678927352456275255e6,3.074109054850539556250927e6},
    Q[] = {1.830328399370592604055942e2,7.765049321445005871323047e3,
	   1.331903827966074194402448e5,1.136705821321969608938755e6,
	   5.267964117437946917577538e6,1.346701454311101692290052e7,
	   1.782736530353274213975932E7,9.533095591844353613395747e6};
    double xm2, xnum, xden;
    int i;
    xm2 = x - 2.0;
    xden = 1.0;
    xnum = 0.0;
    for ( i = 0 ; i < 8 ; i++ )
      {
	xnum = xnum*xm2 + P[i];
	xden = xden*xm2 + Q[i];
      }
    return xm2*(D + xm2*(xnum/xden));
}

/* approximation for lngamma in [0.68,1.5] translation in C of the Cody 's code */
static double lngamma_in_0p68_1p5(double x)
{
  double const 
    D = -5.772156649015328605195174e-1,
    P[] = {4.945235359296727046734888e0, 2.018112620856775083915565e2,
	   2.290838373831346393026739e3, 1.131967205903380828685045e4,
	   2.855724635671635335736389e4, 3.848496228443793359990269e4,
	   2.637748787624195437963534e4, 7.225813979700288197698961e3},
    Q[] = {6.748212550303777196073036e1, 1.113332393857199323513008e3,
	   7.738757056935398733233834e3,2.763987074403340708898585e4,
	   5.499310206226157329794414e4,6.161122180066002127833352e4,
	   3.635127591501940507276287e4,8.785536302431013170870835e3};
    double xm1, xnum, xden;
    int i;
    xm1 = x - 1.0;
    xden = 1.0;
    xnum = 0.0;
    for ( i = 0 ; i < 8 ; i++ )
      {
	xnum = xnum*xm1 + P[i];
	xden = xden*xm1 + Q[i];
      }
    return xm1*(D + xm1*(xnum/xden));
}


/**
 * nsp_lngamma:
 * @x: a double 
 * 
 * compute log(|gamma(x)|). This code follow essentially the Cody's specfun 
 * algama fortran Code
 *
 * Returns: a double
 **/
double nsp_lngamma(double x)
{
  double const log_sqr_2pi = 0.9189385332046727417803297364, lim = 0.709; /* 0.6796875 in the Cody's code */
  if ( x > 12.0 )
    return log_sqr_2pi + nsp_stirling_error(x) + (x-0.5)*log(x) - x;
  else if ( x > 4.0 )
    return lngamma_in_4_12(x);
  else if ( x > 1.5 )
    return lngamma_in_1p5_4(x);
  else if ( x > lim )
    return lngamma_in_0p68_1p5(x);
  else if ( x > 0.5 )
    return lngamma_in_1p5_4(x+1.0) - log(x);
  else if ( x >= DBL_EPSILON )
    return lngamma_in_0p68_1p5(x+1.0) - log(x);
  else if ( x >= -DBL_EPSILON )
    return -log(fabs(x));
  else
    {
      if ( x == floor(x) ) 
	return 1.0/0.0;  /* Inf */
      else
	return log(fabs(nsp_gamma(x)));
    }
}


extern int C2F(dgemm) (char *transa, char *transb, int *m, int *n, int *k, double *alpha, double *a, int *lda, double *b, int *ldb, double *beta, double *c__, int *ldc, int transa_len, int transb_len);

/* an utility for nsp_kcdf: this routine computes A^p with some scaling to avoid overflows... */
static int matpow_for_kcdf(double *A, int m, int p, int *e, double lim_factor, double scale_factor)
{
  double alpha=1.0,beta=0.0;
  double *temp = NULL, *oddmat = NULL;
  int i, oddflag=0, sizeA = m*m, k_k = sizeA/2, e1 = 0, e2 = 0;

  if ( (temp = malloc(sizeA*sizeof(double))) == NULL ) return FAIL;
  while ( p > 1 )
    {
      if ( p % 2 == 1 )
	{
	  if ( ! oddflag )
	    {
	      if ( (oddmat = malloc(sizeA*sizeof(double))) == NULL ) { free(temp); return FAIL; }
	      oddflag = 1;
	      memcpy(oddmat, A, sizeA*sizeof(double));
	    }
	  else
	    {
	      C2F(dgemm)("N","N",&m,&m,&m,&alpha,A,&m,oddmat,&m, &beta,temp,&m,1,1); 
	      memcpy(oddmat, temp, sizeA*sizeof(double));
	    }
	  e2 += e1;
	}
      C2F(dgemm)("N","N",&m,&m,&m,&alpha,A,&m,A,&m,&beta,temp,&m,1,1);
      memcpy(A, temp, sizeA*sizeof(double));
      e1 *= 2; 
      if ( A[k_k] >= lim_factor )
	{
	  for ( i = 0 ; i < sizeA ; i++ ) A[i] *= scale_factor; 
	  e1 += 465;
	}  
      p = p/2;
    }
  if ( oddflag )
    {
      C2F(dgemm)("N","N",&m,&m,&m,&alpha,A,&m,oddmat,&m, &beta,temp,&m,1,1); 
      memcpy(A, temp, sizeA*sizeof(double));
      free(oddmat);
    }
  free(temp);
  *e = e1 + e2;
  return OK;
}

/**
 * nsp_kcdf:
 * @x: (input) a double 
 * @res (output) a pointer to double which hold the final result
 * @n: (input) an int
 *
 * computes P( K_n <= x ) where K_n is the Kolmogorov distribution
 * of a "n-sample": 
 *
 *   K_n = sqrt(n) D_n 
 *
 *   D_n = max( x_1 - 0/n, x_2 - 1/n, ...., x_n - (n-1)/n ,
 *              1/n - x_1, 2/n - x_2, ...., n/n - x_n )
 *
 *   x_1 < x_2 < x_3 < ..... an ordered set of uniform [0,1)
 *
 *   In general x_i is got from x_i = F_Y(y_i) where y_1,y_2,...,y_n
 *   is supposed to be a n independant sample from an Y distribution 
 *   and we want to test if this hypothesis is realistic or not.
 *
 *  The code follows the algorithm and code described in:
 * "Evaluation Kolmogorov's Distribution, G. Marsaglia
 *  W. W. Tsang and J. Wang, Journal of Statistic Software,
 *  Vol. 8, Issue 18, Nov 2003" (the paper could be downloaded
 *  at http://www.jstatsoft.org/v08/i18), with some little 
 *  changes, it uses :
 *   - an iterative matrix power algorithm (in place of a recursive one) 
 *     (see the routine matpow_for_kcdf just before this one)
 *   - the blas dgemm routine inside the matrix power algorithm 
 *   - a scaling with a power of 2 in place of a scaling with 
 *     a power of 10 (trying to avoid supplementary floating point
 *     errors).
 * 
 * Returns: %OK or %FAIL
 **/
int nsp_kcdf(double x, double *res, int n)
{
  double d, sqrt_n = sqrt(n), s, h, *H=NULL, fact;
  int k, m, i, j, diag, e;
  double lim_factor = pow(2.0,465);
  double scale_factor = 1.0/lim_factor;

  if ( x <= 0.0 )
    {
      *res = 0.0; return OK;
    }

  if ( n < 1 ) return FAIL;

  d = x/sqrt_n;
  s = x*x;

  if ( s > 7.25  ||  (s > 3.76 && n > 99) )
    {
      *res = 1.0 - 2.0*exp(-(2.000071 + 0.331/sqrt_n + 1.409/n)*s);
      return OK;
    }

  k = (int) (x*sqrt_n) + 1;
  h = (double) k -  x*sqrt_n;
  m = 2*k - 1;

  if ( (H = malloc(m*m*sizeof(double))) == NULL ) return FAIL;

  for ( j = 1 ; j < m ; j++ )
    for ( i = 0 ; i < m-1 ; i++ )
      H[m*j + i] = (j-i > 1) ? 0.0 : 1.0;
  
  for ( i = 0, j = m*m-1 ; i < m-1 ; i++, j-=m )
    {
      H[i] = (1.0 - pow(h,i+1));
      H[j] = H[i];
    }

  H[m-1] = 1.0 - 2.0*pow(h,m);
  if (h > 0.5) H[m-1] += pow(2*h-1,m);

  fact = 1.0;
  for ( diag = 1 ; diag < m ; diag++ )
    {
      fact *= (diag+1);
      for ( i = diag, j = 1 ; j <= m - diag ; j++, i+=m+1 )
	H[i] /= fact;
    }

  if ( matpow_for_kcdf(H, m, n, &e, lim_factor, scale_factor) == FAIL )
    {
      free(H); return FAIL;
    }

  s = H[m*m/2];

  for ( i = 1 ; i < n ; i++ )
    {
      s = (s*i)/n;
      if ( s < scale_factor ) { s *= lim_factor; e -= 465; };
    } 
  *res = s * pow(2.0,e);
  free(H);
  return OK;
}
/**
 * nsp_kcdflim:
 * @x: (input) a double 
 * @q (output) a pointer to double which hold 1 - the final "returned" result
 *
 * computes p = P( K <= x ) and q = 1 - p, where K is the limit 
 * Kolmogorov distribution when n the size sample -> + infinity 
 * (see #nsp_kcdf).
 *
 * It is known that:
 *
 *  P( K <=  x ) = 1 - 2 sum_{j=1}^{+oo} (-1)^(j-1) exp(-2 j^2 x^2 )
 *
 *      or       = sqrt(2pi)/x  sum_{j=1}^{+oo} exp(-(2j-1)^2 pi^2 / 8 x^2 )
 *
 *  (see the reference at #nsp_kcdf)
 *
 *  For x <= 0.75 we use the second serie. Noting u = exp(-(1/8)(pi/x)^2)
 *  this one could be expressed as:
 *
 *      p =  sqrt(2pi)/x ( u + u^9 + u^25 + u^49 + u^81 + ...)
 *        =  sqrt(2pi)/x * u * ( 1 + u^8 + u^24 + u^80 + ....)
 *     
 *     the max value for u is got for x=0.75 and is near 0.1115.
 *     u^24 is near 1.38e-23 so, up to double precision machine
 *     accuracy (2^(-53) = 1.11e-16), we need only 2 terms of the serie.
 *
 *  For  x > 0.75 we use the first serie. Noting u = exp(-2 x^2) we
 *  have:
 *
 *      p = 1 - q and q = 2 ( u - u^4 + u^9 - u^16 + u^25 - u^36....)
 *      q = 2 u * ( 1 - u^3 + u^8 - u^15 + u^24 - u^35.....)
 *
 * the max value for u is near 0.324 and we need to use terms until
 * u^24 (= 1.879e-12), u^35 being less than 2^(-53). If x >= 2.5 
 * we need only one term. The computation of:
 *
 *    q = 2 u ( 1 - u^3 + u^8 - u^15 + u^24 )  
 *
 *  is made using an Horner scheme:
 *  
 *      = 2 u ( 1 - u^3 ( 1 - u^5 ( 1 - u^7 ( 1 - u^9 ) ) ) )  
 *
 *  The switch limit (0.75) between the 2 formulae have been set up
 *  using the pari-gp software.
 *
 * Returns: a double
 **/

double nsp_kcdflim(double x, double *q)
{
  double const pi2d8 = 1.233700550136169827354311375, /* pi^2/8 */
               sqrt_2pi = 2.506628274631000502415765285;
  double u, u2, u3, u5, u7, u9, p;

  if ( x <= 0.0 )
    {  
      p = 0.0;
      *q = 1.0;
    }
  else if ( x <= 0.75 ) 
    {
      u = exp(-pi2d8/(x*x));
      if ( u == 0.0 ) { *q = 1.0 ; return 0.0;}
      p = sqrt_2pi * u*(1.0 + pow(u,8))/x;
      *q = 1.0 - p;
    }
  else
    {
      u = exp(-2*x*x);
      if ( x > 2.5 )
	{
	  *q = 2.0*u;
	  p = 1.0 - *q;
	}
      else
	{
	  u2 = u*u;
	  u3 = u2*u;
	  u5 = u3*u2;
	  u7 = u5*u2;
	  u9 = u7*u2;
	  *q = 2.0*u*(1.0 - u3*(1.0 - u5*(1.0 - u7*(1.0 - u9))));
	  p = 1.0 - *q;
	}
    }
  return p;
}


int nsp_invkcdflim(double p, double q, double *x)
{
  int pq_flag = p <= 0.3728 ? 1 : 0, status = 0, qleft, qhi;
  double pp,qq, fx, xinf = 0.04, xsup = 20, atol=1e-50, tol=1e-14, step=0.4, rstep=0.0, step_inc=2.0; 
  int iter=0;

  /* extreme values */
  if ( p == 0.0 )
    {
      *x = 0.0; return OK;
    }
  if ( q == 0.0 )
    {
      *x = 2.0*DBL_MAX; /* for Inf */ return OK;
    }

  /* init for inversion routine */
  cdf_dstinv (&xinf, &xsup, &step, &rstep, &step_inc, &atol, &tol);

  *x = 0.828; /* this is approximately the median */
 
  cdf_dinvr (&status, x, &fx, &qleft, &qhi);

  while ( status == 1 )
    {
      pp = nsp_kcdflim(*x, &qq); 
      iter++;
      if ( pq_flag )
	fx = pp - p;
      else
	fx = qq - q;
      cdf_dinvr (&status, x, &fx, &qleft, &qhi);
    }

  Sciprintf(" iter = %d\n",iter);

  if ( status == 0 )
    return OK;
  else
    return FAIL;
}


/* Marsaglia, Tang, K cdf for test comparizon and test purpose only 
 * (this will be removed after)
 */
static void mMultiply(double *A,double *B,double *C,int m)
{
  int i,j,k;
  double s;
  for(i=0;i<m;i++)
    for(j=0;j<m;j++)
      {
	s=0.;
	for(k=0;k<m;k++) s+=A[i*m+k]*B[k*m+j];
	C[i*m+j]=s;
      }
}

static void mPower(double *A,int eA,double *V,int *eV,int m,int n)
{
  double *B;int eB,i;
  if(n==1)
    {
      for(i=0;i<m*m;i++) V[i]=A[i];
      *eV=eA;
      return;
    }
  mPower(A,eA,V,eV,m,n/2);
  B=(double*)malloc((m*m)*sizeof(double));
  mMultiply(V,V,B,m);
  eB=2*(*eV);
  if(n%2==0) {for(i=0;i<m*m;i++) V[i]=B[i]; *eV=eB;}
  else   {mMultiply(A,B,V,m);*eV=eA+eB;}
  if(V[(m/2)*m+(m/2)]>1e140)
    {for(i=0;i<m*m;i++) V[i]=V[i]*1e-140; *eV+=140;}
  free(B);
}

double marsaglia_K(double x, int n)
{
  double d = x/sqrt(n);
  int k,m,i,j,g,eH,eQ;
  double h,s,*H,*Q;
  /* OMIT NEXT TWO LINES IF YOU REQUIRE >7 DIGIT ACCURACY IN THE RIGHT TAIL*/
  s=d*d*n;
  if(s>7.24||(s>3.76&&n>99)) return 1-2*exp(-(2.000071+.331/sqrt(n)+1.409/n)*s);
  k=(int)(n*d)+1;
  m=2*k-1;
  h=k-n*d;
  H=(double*)malloc((m*m)*sizeof(double));
  Q=(double*)malloc((m*m)*sizeof(double));
  for(i=0;i<m;i++)
    for(j=0;j<m;j++)
      if(i-j+1<0) H[i*m+j]=0;
      else     H[i*m+j]=1;
  for(i=0;i<m;i++)
    {
      H[i*m]-=pow(h,i+1);
      H[(m-1)*m+i]-=pow(h,(m-i));
    }
  H[(m-1)*m]+=(2*h-1>0?pow(2*h-1,m):0);
  for(i=0;i<m;i++)
    for(j=0;j<m;j++)
      if(i-j+1>0)
        for(g=1;g<=i-j+1;g++) H[i*m+j]/=g;
  eH=0;
  mPower(H,eH,Q,&eQ,m,n);
  s=Q[(k-1)*m+k-1];
  for(i=1;i<=n;i++)
    {
      s=s*i/n;
      if(s<1e-140){s*=1e140; eQ-=140;}
    }
  s*=pow(10.,eQ);
  free(H);
  free(Q);
  return s;
}

/**
 * nsp_hypot:
 * @x: a double 
 * @y: a double 
 * 
 *  computes sqrt(x^2 + y^2) with accuracy and
 *  without spurious underflow / overflow problems
 *
 *  algorithm by William Kahan,					                                             
 *  which appears in his article "Branch cuts for complex elementary    
 *  functions, or much ado about nothing's sign bit", Editors: Iserles, 
 *  A. and Powell, M. J. D. in "States of the Art in Numerical Analysis"
 *  Oxford, Clarendon Press, 1987 ISBN 0-19-853614-3                          
 *       
 *  Returns: a double     
 *       
 **/
double nsp_hypot(double x, double y)
{
  const double   
    r2 = 1.41421356237309504,         /* sqrt(2) to machine prec */
    r2p1 = 2.41421356237309504,       /* sqrt(2)+1 to machine prec */
    t2p1 = 1.25371671790502177e-16;   /* 1+sqrt(2)-r2p1 to machine prec */

  double s, t, temp;

  if ( ISNAN(x) )
    return x;
  if ( ISNAN(y) )
    return y;

  x = fabs(x); y = fabs(y);
  /*  Order x and y such that 0 <= y <= x  */
  if ( x < y )
    {
      temp = x ; x = y ; y = temp; 
    }

  /* test for overflowing x */
  if ( x > DBL_MAX )
    return x;
  
  /* handle generic case */
  t = x - y;
  if ( t != x )
    {
      if ( t > y )
	{
	  /*  2 < x/y < 2/epsm */
	  s = x / y;
	  s += sqrt(1.0 + s*s);
	}
      else
	{
	  /*  1 <= x/y <= 2   */
	  s = t / y;
	  t = (2.0 + s) * s;
	  s = ( ( t2p1 + t/(r2 + sqrt(2.0 + t)) ) + s ) + r2p1;
        }
          
      return x + y/s;
    }
  else
    return x;
}


/**
 * nsp_primefactor:
 * @n: unsigned int number to be factorized 
 * @factors: array (of size 9) with the factors on output 
 * @powers: array (of size 9) with the powers of the factors 
 * @nb_factors: number of factors (normaly <= 9) 
 *
 * computes the prime factors of an unsigned int.
 *
 **/
void nsp_primefactors(unsigned int n, unsigned int *factors, int *powers, int *nb_factors)
{
  int k=0;
  unsigned int inc, d, lim;

  if ( n <= 1 )
    {
      factors[0] = n;
      powers[0] = 1;
      *nb_factors = 1;
      return;
    }

  /* test divisibility by 2 */
  if ( n % 2 == 0 )
    {
      factors[k] = 2; powers[k] = 1; n = n / 2;
      while ( n % 2 == 0 )
	{
	  powers[k]++; n = n / 2;
	}
      k++;
    }

  /* test divisibility by 3 */
  if ( n % 3 == 0 )
    {
      factors[k] = 3; powers[k] = 1; n = n / 3;
      while ( n % 3 == 0 )
	{
	  powers[k]++; n = n / 3;
	}
      k++;
    }

  /* general loop avoiding multiples of 2 and 3 */
  d = 5; inc = 4; lim = (unsigned int) sqrt((double) n);

  while ( n > 1 )
    {
      if ( d > lim )
	{
	  factors[k] = n; powers[k] = 1; k++; break;
	}

      if ( n % d == 0 )
	{
	  factors[k] = d; powers[k] = 1; n = n / d;
	  while ( n % d == 0 )
	    {
	      powers[k]++; n = n / d;
	    }
	  k++;
	  lim = (unsigned int) sqrt((double) n);
	}
      
      inc = inc == 2 ? 4 : 2;
      d += inc;
    }

  *nb_factors = k;
  return;
}


/**
 * nsp_isprime:
 * @n: unsigned int
 *
 * tests if n is a prime number
 *
 * Returns: %TRUE or %FALSE
 *       
 **/
int nsp_isprime(unsigned int n)
{
  unsigned int inc, d, lim;

  if ( n <= 1 )
    return FALSE;

  if ( n == 2 || n == 3 )
    return TRUE;

  if ( n % 2 == 0 || n % 3 == 0 )
    return FALSE;

  /* general loop avoiding multiples of 2 and 3 */
  d = 5; inc = 4; lim = (unsigned int) sqrt((double) n);

  while ( d <= lim )
    {
      if ( n % d == 0 )
	return FALSE;
      inc = inc == 2 ? 4 : 2;
      d += inc;
    }

  return TRUE;
}


/**
 * nsp_primes:
 * @n: int
 * @Primes: int array fill with the prime numbers
 * @nb_primes: size of Primes
 *
 * computes all primes less or equal than n with a basic 
 * Erathostene 's sieve (just avoiding even numbers)
 *
 * Returns: %OK or %FAIL
 *       
 **/
int nsp_primes(int n, int **Primes, int *nb_primes)
{
  int *primes, i, j, k, m;

  if ( n <= 1 )
    {
      *nb_primes = 0;
      return OK;
    }

  if ( n == 2 )
    {
      if ( (primes = malloc(sizeof(int))) == NULL )
	return FAIL;
      *nb_primes = 1;
      primes[0] = 2;
    }
  else  
    {
      char *sieve;
      if ( n % 2 == 0 ) n--;
      m = (n-1)/2;
      if ( (sieve = malloc((m+1)*sizeof(char))) == NULL )
	return FAIL;

      *nb_primes = 1;
      memset(sieve,1,m+1);
      for ( i = 3, k = 1 ; i <= (int) sqrt((double) n) ; i+=2, k++ )
	{
	  if ( sieve[k] )
	    for ( j = (i*i)/2 ; j <= m ; j+=i )
	      sieve[j] = 0;
	}

      /* count the number of primes */
      *nb_primes = 1;
      for ( k = 1 ; k <= m ; k++ )   
	*nb_primes = *nb_primes + sieve[k];  /* this is a little faster than if (sieve[k]) (*nb_primes)++ */

      /* allocate then fill the array with the prime numbers */
      if ( (primes = malloc((*nb_primes)*sizeof(int))) == NULL )
	{
	  FREE(sieve);
	  return FAIL;
	}
      primes[0] = 2;
      i = 1;
      for ( k = 1 ; k <= m ; k++ )
	if ( sieve[k] )
	  primes[i++] = 2*k+1;
      free(sieve);
    }

  *Primes = primes;
  return OK;
}

/**
 * nsp_pdf_normal
 * @x: double
 * @mu: double
 * @sigma: double
 *
 * compute the gaussian density N(mu,sigma^2) at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_normal(double x, double mu, double sigma)
{
  double const sqrt_two_pi = 2.506628274631000502415765285;
  double r = (x-mu)/sigma;
  return  exp(-0.5*r*r)/(sigma*sqrt_two_pi);
}

/**
 * nsp_pdf_lognormal
 * @x: double
 * @mu: double
 * @sigma: double
 *
 * compute the lognormal density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_lognormal(double x, double mu, double sigma)
{
  double const sqrt_two_pi = 2.506628274631000502415765285;
  if ( x <= 0.0 )
    return 0.0;
  else
    {
      double r = (log(x)-mu)/sigma;
      return  exp(-0.5*r*r)/(sigma*sqrt_two_pi*x);
    }
}


/**
 * nsp_pdf_chi2
 * @x: double
 * @nu: double
 *
 * compute the chi2 density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_chi2(double x, double nu)
{
  return nsp_pdf_gamma(x, 0.5*nu, 0.5, 0);
}

/**
 * nsp_pdf_nchi2
 * @x: double
 * @nu: double  (dof)
 * @lambda: double  (non centrality parameter)
 *
 * compute the non central chi2 density at x
 * use the direct formula
 *
 * Returns: a double
 *
 **/
/* extern double cephes_iv(double v, double x); */
double nsp_pdf_nchi2(double x, double nu, double lambda)
{
  double d = 0.0;
/*   if ( ~( x <= 0.0) ) */
/*     d = 0.5*exp(-0.5*(x+lambda))*pow((x/lambda),0.25*nu-0.5)*cephes_iv(0.5*nu-1.0,sqrt(lambda*x)); */
  return d;
}


/**
 * nsp_pdf_exp
 * @x: double
 * @Av: double
 *
 * compute the exponential E(Av) density at x
 * the parametre is the mean f the distribution
 * (generally it is lambda = 1/Av)
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_exp(double x, double tau)
{
  if ( x < 0.0 )
    return 0.0;
  else
    return exp(-x/tau)/tau;
}

/**
 * nsp_pdf_cauchy
 * @x: double
 * @sigma: double
 *
 * compute the Cauchy density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_cauchy(double x, double sigma)
{
  return  sigma/(M_PI*(x*x + sigma*sigma)); 
}


/**
 * nsp_pdf_pareto
 * @x: double
 * @a: double
 * @b: double
 *
 * compute the Pareto density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_pareto(double x, double a, double b)
{
  if ( x < b )
    return 0.0;
  else
    return  (a/b) / pow(x/b,a+1.0);
}

/**
 * nsp_pdf_laplace
 * @x: double
 * @a: double
 *
 * compute the Laplace density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_laplace(double x, double a)
{
  return exp(-fabs(x/a))/(2.0*a);
}

/**
 * nsp_pdf_logistic
 * @x: double
 * @a: double
 * @b: double
 *
 * compute the logistic density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_logistic(double x, double a, double b)
{
  double y = exp(-fabs((x-a)/b));
  return y / (b* (1.0+y)*(1.0+y));
}

/**
 * nsp_pdf_weibull
 * @x: double
 * @a: double
 * @b: double
 *
 * compute the Weibull density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_weibull(double x, double a, double b)
{
  if ( x < 0.0 )
    return 0.0;
  else if ( b == 1.0 )
    return exp(-(x/a))/a;
  else
    {
      double y = x/a;
      return (b/a)*exp( (b-1.0)*log(y) - pow(y,b) );
    }
}

/**
 * nsp_pdf_rayleigh
 * @x: double
 * @sigma: double
 *
 * compute the Rayleigh density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_rayleigh(double x, double sigma)
{
  if ( x < 0.0 )
    return 0.0;
  else
    {
      double r = x / sigma;
      return (r/sigma)*exp(-0.5*r*r);
    }
}

/**
 * nsp_pdf_tailrayleigh
 * @x: double
 * @sigma: double
 * @a: double
 *
 * compute the tail Rayleigh density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_tailrayleigh(double x, double sigma, double a)
{
  if ( x < a )
    return 0.0;
  else
    {
      double r = x / sigma, s = a / sigma;
      return (r/sigma)*exp(-0.5*(r-s)*(r+s));
    }
}

/**
 * nsp_pdf_geometric
 * @x: double
 * @p: double
 *
 * compute the geometric density at x
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_geometric(double x, double p)
{
  if ( x < 1.0 || floor(x) != x  )
    return 0.0;
  else
    return p * pow(1.0-p,x-1.0);
}


/**
 * nsp_pdf_uin
 * @x: double
 * @n1: double
 * @n2: double
 *
 * compute the uin (uniform integer distribution) mass function at x
 * n1 and n2 should be integre (stored as double) with n1 <= n2
 * This should be verified at the calling level.
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_uin(double x, double n1, double n2)
{
  if ( x < n1 || x > n2 || floor(x) != x  )
    return 0.0;
  else if ( isnan(x) )
    return x;
  else
    return 1.0/(n2 - n1 + 1.0);
}

/**
 * nsp_pdf_unf
 * @x: double
 * @a: double
 * @b: double
 *
 * compute the U(a,b) density function at x
 * a and b should be such that a < b
 * This should be verified at the calling level.
 *
 * Returns: a double
 *       
 **/
double nsp_pdf_unf(double x, double a, double b)
{
  if ( x < a || x > b )
    return 0.0;
  else if ( isnan(x) )
    return x;
  else
    return 1.0/(b - a);
}

/**
 * nsp_binomial_coef
 * @n: double
 * @k: double
 * @res: hold the result
 *
 *           / n \ 
 * compute   |   |
 *           \ k /
 *
 * Returns: FAIL if n or k are not integer or 
 *                if k is not between 0 and n
 *          otherwise OK
 **/
int nsp_binomial_coef(double n, double k, double *res)
{
  if ( ! (floor(n) == n && n >= 0.0 && floor(k) == k && 0 <= k && k <= n) )
    return FAIL;
  else
    {
      double d, r=1.0;
      if ( k > n - k ) k = n - k;
      for ( d=1.0 ; d <= k ; d++, n-- )
	{
	  r *= n; r /= d;
	}
      *res = floor(r+0.5);
      return OK;
    }
}
