#include "cdf.h"


/* ********************************************************************** */
/*     SUBROUINE CUMNOR(X,RESULT,CCUM) */
/*                              Function */
/*     Computes the cumulative  of    the  normal   distribution,   i.e., */
/*     the integral from -infinity to x of */
/*          (1/sqrt(2*pi)) exp(-u*u/2) du */
/*     X --> Upper limit of integration. */
/*                                        X is DOUBLE PRECISION */
/*     RESULT <-- Cumulative normal distribution. */
/*                                        RESULT is DOUBLE PRECISION */
/*     CCUM <-- Compliment of Cumulative normal distribution. */
/*                                        CCUM is DOUBLE PRECISION */
/*     Renaming of function ANORM from: */
/*     Cody, W.D. (1993). "ALGORITHM 715: SPECFUN - A Portabel FORTRAN */
/*     Package of Special Function Routines and Test Drivers" */
/*     acm Transactions on Mathematical Software. 19, 22-32. */
/*     with slight modifications to return ccum and to deal with */
/*     machine constants. */
/* ********************************************************************** */


int cdf_cumnor (double *arg, double *result, double *ccum)
{
  const int c__1 = 1;
  const int c__2 = 2;


  const  double one = 1.;
  const  double c__[9] =
    { .39894151208813466764, 8.8831497943883759412, 93.506656132177855979,
      597.27027639480026226, 2494.5375852903726711, 6848.1904505362823326, 11602.651437647350124,
      9842.7148383839780218, 1.0765576773720192317e-8 };
  const  double d__[8] =
    { 22.266688044328115691, 235.38790178262499861, 1519.377599407554805,
      6485.558298266760755, 18615.571640885098091, 34900.952721145977266, 38912.003286093271411,
      19685.429676859990727 };
  const  double p[6] =
    { .21589853405795699, .1274011611602473639, .022235277870649807,
      .001421619193227893466, 2.9112874951168792e-5, .02307344176494017303 };
  const  double q[5] =
    { 1.28426009614491121, .468238212480865118, .0659881378689285515,
      .00378239633202758244, 7.29751555083966205e-5 };
  const  double half = .5;
  const  double zero = 0.;
  const  double sixten = 1.6;
  const  double sqrpi = .39894228040143267794;
  const  double thrsh = .66291;
  const  double root32 = 5.656854248;
  const  double a[5] =
    { 2.2352520354606839287, 161.02823106855587881, 1067.6894854603709582,
      18154.981253343561249, .065682337918207449113 };
  const  double b[4] =
    { 47.20258190468824187, 976.09855173777669322, 10260.932208618978205,
      45507.789335026729956 };

  double d__1;
  double xden, temp, xnum;
  int i__;
  double x, y;
  double del, min__, eps, xsq;



/* Original Comments: */
/* ------------------------------------------------------------------ */

/* This function evaluates the normal distribution function: */

/*                              / x */
/*                     1       |       -t*t/2 */
/*          P(x) = ----------- |      e       dt */
/*                 sqrt(2 pi)  | */
/*                             /-oo */

/*   The main computation evaluates near-minimax approximations */
/*   derived from those in "Rational Chebyshev approximations for */
/*   the error function" by W. J. Cody, Math. Comp., 1969, 631-637. */
/*   This transportable program uses rational functions that */
/*   theoretically approximate the normal distribution function to */
/*   at least 18 significant decimal digits.  The accuracy achieved */
/*   depends on the arithmetic system, the compiler, the intrinsic */
/*   functions, and proper selection of the machine-dependent */
/*   constants. */

/* ******************************************************************* */
/* ******************************************************************* */

/* Explanation of machine-dependent constants. */

/*   MIN   = smallest machine representable number. */

/*   EPS   = argument below which anorm(x) may be represented by */
/*           0.5  and above which  x*x  will not underflow. */
/*           A conservative value is the largest machine number X */
/*           such that   1.0 + X = 1.0   to machine precision. */
/* ******************************************************************* */
/* ******************************************************************* */

/* Error returns */

/*  The program returns  ANORM = 0     for  ARG .LE. XLOW. */


/* Intrinsic functions required are: */

/*     ABS, AINT, EXP */


/*  Author: W. J. Cody */
/*          Mathematics and Computer Science Division */
/*          Argonne National Laboratory */
/*          Argonne, IL 60439 */

/*  Latest modification: March 15, 1992 */

/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
/*  External Function */
/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
/*  Mathematical constants */

/*  SQRPI = 1 / sqrt(2*pi), ROOT32 = sqrt(32), and */
/*  THRSH is the argument for which anorm = 0.75. */
/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
/*  Coefficients for approximation in first interval */
/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
/*  Coefficients for approximation in second interval */
/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
/*  Coefficients for approximation in third interval */
/* ------------------------------------------------------------------ */
/* ------------------------------------------------------------------ */
/*  Machine dependent constants */
/* ------------------------------------------------------------------ */
  eps = cdf_spmpar (c__1) * .5;
  min__ = cdf_spmpar (c__2);
/* ------------------------------------------------------------------ */
  x = *arg;
  y = Abs (x);
  if (y <= thrsh)
    {
/* ------------------------------------------------------------------ */
/*  Evaluate  anorm  for  |X| <= 0.66291 */
/* ------------------------------------------------------------------ */
      xsq = zero;
      if (y > eps)
	{
	  xsq = x * x;
	}
      xnum = a[4] * xsq;
      xden = xsq;
      for (i__ = 1; i__ <= 3; ++i__)
	{
	  xnum = (xnum + a[i__ - 1]) * xsq;
	  xden = (xden + b[i__ - 1]) * xsq;
/* L10: */
	}
      *result = x * (xnum + a[3]) / (xden + b[3]);
      temp = *result;
      *result = half + temp;
      *ccum = half - temp;
/* ------------------------------------------------------------------ */
/*  Evaluate  anorm  for 0.66291 <= |X| <= sqrt(32) */
/* ------------------------------------------------------------------ */
    }
  else if (y <= root32)
    {
      xnum = c__[8] * y;
      xden = y;
      for (i__ = 1; i__ <= 7; ++i__)
	{
	  xnum = (xnum + c__[i__ - 1]) * y;
	  xden = (xden + d__[i__ - 1]) * y;
/* L20: */
	}
      *result = (xnum + c__[7]) / (xden + d__[7]);
      d__1 = y * sixten;
      xsq = D_INT (d__1) / sixten;
      del = (y - xsq) * (y + xsq);
      *result = exp (-xsq * xsq * half) * exp (-del * half) * *result;
      *ccum = one - *result;
      if (x > zero)
	{
	  temp = *result;
	  *result = *ccum;
	  *ccum = temp;
	}
/* ------------------------------------------------------------------ */
/*  Evaluate  anorm  for |X| > sqrt(32) */
/* ------------------------------------------------------------------ */
    }
  else
    {
      *result = zero;
      xsq = one / (x * x);
      xnum = p[5] * xsq;
      xden = xsq;
      for (i__ = 1; i__ <= 4; ++i__)
	{
	  xnum = (xnum + p[i__ - 1]) * xsq;
	  xden = (xden + q[i__ - 1]) * xsq;
/* L30: */
	}
      *result = xsq * (xnum + p[4]) / (xden + q[4]);
      *result = (sqrpi - *result) / y;
      d__1 = x * sixten;
      xsq = D_INT (d__1) / sixten;
      del = (x - xsq) * (x + xsq);
      *result = exp (-xsq * xsq * half) * exp (-del * half) * *result;
      *ccum = one - *result;
      if (x > zero)
	{
	  temp = *result;
	  *result = *ccum;
	  *ccum = temp;
	}
    }
  if (*result < min__)
    {
      *result = 0.;
    }
  if (*ccum < min__)
    {
      *ccum = 0.;
    }
/* ------------------------------------------------------------------ */
/*  Fix up for negative argument, erf, etc. */
/* ------------------------------------------------------------------ */
/* ----------Last card of ANORM ---------- */
  return 0;
}				/* cumnor_ */
