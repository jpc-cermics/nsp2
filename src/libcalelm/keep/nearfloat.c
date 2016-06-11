/* nearfloat.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static double c_b6 = 1.;

double nsp_calpack_nearfloat (double *x, double *dir)
{
  /* Initialized data */

  static int first = TRUE;

  /* System generated locals */
  int i__1;
  double ret_val;

  /* Local variables */
  static double base, rmin, rmax, tiny;
  double d__;
  int e, i__;
  double m;
  int p;
  double ep;
  double xa;
  static int denorm;
  double sign_x__;
  static double lnb, ulp;

  /* 
   *    PURPOSE 
   *       Compute the near (double) float from x in 
   *       the direction dir 
   *       dir >= 0 => toward +oo 
   *       dir < 0  => toward -oo 
   * 
   *    AUTHOR 
   *       Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> 
   * 
   *    REMARK 
   *       This code may be shorter if we assume that the 
   *       radix base of floating point numbers is 2 : one 
   *       could use the frexp C function to extract the 
   *       mantissa and exponent part in place of dealing 
   *       with a call to the log function with corrections 
   *       to avoid possible floating point error... 
   * 
   *    PARAMETERS 
   *    EXTERNAL FUNCTIONS 
   *    LOCAL VARIABLES 
   *    STATIC VARIABLES 
   */
  /*    TEXT 
   *    got f.p. parameters used by the algorithm 
   */
  if (first)
    {
      rmax = nsp_dlamch ("o");
      rmin = nsp_dlamch ("u");
      base = C2F (dlamch) ("b", 1L);
      p = (int) C2F (dlamch) ("n", 1L);
      lnb = log (base);
      /*        computation of 1 ulp : 1 ulp = base^(1-p) 
       *        p = number of digits for the mantissa = dlamch('n') 
       */
      i__1 = 1 - p;
      ulp = pow_di_ref (&base, &i__1);
      /*        query if denormalised numbers are used : if yes 
       *        compute TINY the smallest denormalised number > 0 : 
       *        TINY is also the increment between 2 neighbooring 
       *        denormalised numbers 
       */
      if (rmin / base != 0.)
	{
	  denorm = TRUE;
	  tiny = rmin;
	  i__1 = p - 1;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      tiny /= base;
	    }
	}
      else
	{
	  denorm = FALSE;
	}
      first = FALSE;
    }
  d__ = d_sign (&c_b6, dir);
  sign_x__ = d_sign (&c_b6, x);
  xa = Abs (*x);
  if (nsp_calpack_isanan (x) == 1)
    {
      /*    nan 
       */
      ret_val = *x;
    }
  else if (xa > rmax)
    {
      /*    +-inf 
       */
      if (d__ * sign_x__ < 0.)
	{
	  ret_val = sign_x__ * rmax;
	}
      else
	{
	  ret_val = *x;
	}
    }
  else if (xa >= rmin)
    {
      /*    usual case : x is a normalised floating point number 
       *       1/ got the exponent e and the exponent part ep = base^e 
       */
      e = (int) (log (xa) / lnb);
      ep = pow_di_ref (&base, &e);
      /*       in case of xa very near RMAX an error in e (of + 1) 
       *       result in an overflow in ep 
       */
      if (ep > rmax)
	{
	  --e;
	  ep = pow_di_ref (&base, &e);
	}
      /*       also in case of xa very near RMIN and when denormalised 
       *       number are not used, an error in e (of -1) results in a 
       *       flush to 0 for ep. 
       */
      if (ep == 0.)
	{
	  ++e;
	  ep = pow_di_ref (&base, &e);
	}
      /*       2/ got the mantissa 
       */
      m = xa / ep;
      /*       3/ verify that 1 <= m < BASE 
       */
      if (m < 1.)
	{
	  /*          multiply m by BASE 
	   */
	  while (m < 1.)
	    {
	      m *= base;
	      ep /= base;
	    }
	}
      else if (m >= base)
	{
	  /*          divide m by BASE 
	   */
	  while (m >= 1.)
	    {
	      m /= base;
	      ep *= base;
	    }
	}
      /*       4/ now compute the near float 
       */
      if (d__ * sign_x__ < 0.)
	{
	  /*          retrieve one ULP to m but there is a special case 
	   */
	  if (m == 1.)
	    {
	      /*             this is the special case : we must retrieve ULP / BASE 
	       */
	      ret_val = sign_x__ * (m - ulp / base) * ep;
	    }
	  else
	    {
	      ret_val = sign_x__ * (m - ulp) * ep;
	    }
	}
      else
	{
	  ret_val = sign_x__ * (m + ulp) * ep;
	}
    }
  else if (*x == 0.)
    {
      /*    case x = 0  nearfloat depends if denormalised numbers are used 
       */
      if (denorm)
	{
	  ret_val = d__ * tiny;
	}
      else
	{
	  ret_val = d__ * rmin;
	}
    }
  else
    {
      /*    x is a denormalised number 
       */
      ret_val = *x + d__ * tiny;
    }
  return ret_val;
}				/* nearfloat_ */
#endif
