/* wasin.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/* Table of constant values */

static double c_b5 = 1.;

int nsp_calpack_wasin (double *zr, double *zi, double *ar, double *ai)
{
  /* Initialized data */

  static int first = TRUE;

  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  static double linf, epsm, lsup;
  double a, b, r__, s, x, y;
  double am1, szi, szr;

  /* 
   *    PURPOSE 
   *       Compute the arcsin of a complex number 
   *        a = ar + i ai = asin(z), z = zr + i zi 
   * 
   *    CALLING LIST / PARAMETERS 
   *       subroutine wasin(zr,zi,ar,ai) 
   *       double precision zr,zi,ar,ai 
   * 
   *       zr,zi: real and imaginary parts of the complex number 
   *       ar,ai: real and imaginary parts of the result 
   *              ar,ai may have the same memory cases than zr et zi 
   * 
   *    REFERENCE 
   *       This is a Fortran-77 translation of an algorithm by 
   *       T.E. Hull, T. F. Fairgrieve and P.T.P. Tang which 
   *       appears in their article : 
   *         "Implementing the Complex Arcsine and Arccosine 
   *          Functions Using Exception Handling", ACM, TOMS, 
   *          Vol 23, No. 3, Sept 1997, p. 299-335 
   * 
   *       with some modifications so as don't rely on ieee handle 
   *       trap functions. 
   * 
   *    AUTHOR 
   *       Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> 
   *       Thanks to Tom Fairgrieve 
   * 
   *    PARAMETERS 
   *    EXTERNAL FUNCTIONS 
   *    CONSTANTS 
   *    LOCAL VARIABLES 
   *    STATIC VARIABLES 
   */
  /*    TEXT 
   *    got f.p. parameters used by the algorithm 
   */
  if (first)
    {
      lsup = sqrt (C2F (dlamch) ("o", 1L)) / 8.;
      linf = sqrt (C2F (dlamch) ("u", 1L)) * 4.;
      epsm = sqrt (C2F (dlamch) ("e", 1L));
      first = FALSE;
    }
  /*    avoid memory pb ... 
   */
  x = Abs (*zr);
  y = Abs (*zi);
  szr = d_sign (&c_b5, zr);
  szi = d_sign (&c_b5, zi);
  if (linf <= Min (x, y) && Max (x, y) <= lsup)
    {
      /*       we are in the safe region 
       *Computing 2nd power 
       */
      d__1 = x + 1.;
      /*Computing 2nd power 
       */
      d__2 = y;
      r__ = sqrt (d__1 * d__1 + d__2 * d__2);
      /*Computing 2nd power 
       */
      d__1 = x - 1.;
      /*Computing 2nd power 
       */
      d__2 = y;
      s = sqrt (d__1 * d__1 + d__2 * d__2);
      a = (r__ + s) * .5;
      b = x / a;
      /*       compute the real part 
       */
      if (b <= .6417)
	{
	  *ar = asin (b);
	}
      else if (x <= 1.)
	{
	  /*Computing 2nd power 
	   */
	  d__1 = y;
	  *ar =
	    atan (x /
		  sqrt ((a + x) * .5 * (d__1 * d__1 / (r__ + (x + 1.)) +
					(s + (1. - x)))));
	}
      else
	{
	  *ar =
	    atan (x /
		  (y *
		   sqrt (((a + x) / (r__ + (x + 1.)) +
			  (a + x) / (s + (x - 1.))) * .5)));
	}
      /*       compute the imaginary part 
       */
      if (a <= 1.5)
	{
	  if (x < 1.)
	    {
	      /*Computing 2nd power 
	       */
	      d__1 = y;
	      /*Computing 2nd power 
	       */
	      d__2 = y;
	      am1 =
		(d__1 * d__1 / (r__ + (x + 1.)) +
		 d__2 * d__2 / (s + (1. - x))) * .5;
	    }
	  else
	    {
	      /*Computing 2nd power 
	       */
	      d__1 = y;
	      am1 = (d__1 * d__1 / (r__ + (x + 1.)) + (s + (x - 1.))) * .5;
	    }
	  d__1 = am1 + sqrt (am1 * (a + 1.));
	  *ai = nsp_calpack_logp1 (&d__1);
	}
      else
	{
	  /*Computing 2nd power 
	   */
	  d__1 = a;
	  *ai = log (a + sqrt (d__1 * d__1 - 1.));
	}
    }
  else
    {
      /*       HANDLE BLOC : evaluation in the special regions ... 
       */
      if (y <= epsm * (d__1 = x - 1., Abs (d__1)))
	{
	  if (x < 1.)
	    {
	      *ar = asin (x);
	      *ai = y / sqrt ((x + 1.) * (1. - x));
	    }
	  else
	    {
	      *ar = 1.5707963267948966192313216;
	      if (x <= lsup)
		{
		  d__1 = x - 1. + sqrt ((x - 1.) * (x + 1.));
		  *ai = nsp_calpack_logp1 (&d__1);
		}
	      else
		{
		  *ai = log (x) + .6931471805599453094172321;
		}
	    }
	}
      else if (y < linf)
	{
	  *ar = 1.5707963267948966192313216 - sqrt (y);
	  *ai = sqrt (y);
	}
      else if (epsm * y - 1. >= x)
	{
	  *ar = x / y;
	  *ai = log (y) + .6931471805599453094172321;
	}
      else if (x > 1.)
	{
	  *ar = atan (x / y);
	  /*Computing 2nd power 
	   */
	  d__2 = x / y;
	  d__1 = d__2 * d__2;
	  *ai =
	    log (y) + .6931471805599453094172321 +
	    nsp_calpack_logp1 (&d__1) * .5;
	}
      else
	{
	  /*Computing 2nd power 
	   */
	  d__1 = y;
	  a = sqrt (d__1 * d__1 + 1);
	  *ar = x / a;
	  d__1 = y * 2. * (y + a);
	  *ai = nsp_calpack_logp1 (&d__1) * .5;
	}
    }
  /*    recover the signs 
   */
  *ar = szr * *ar;
  *ai = szi * *ai;
  return 0;
}				/* wasin_ */
