/* 
 * Minpack Copyright Notice (1999) University of Chicago.  All rights reserved
 * 
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the
 * following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above
 * copyright notice, this list of conditions and the following
 * disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials
 * provided with the distribution.
 * 
 * 3. The end-user documentation included with the
 * redistribution, if any, must include the following
 * acknowledgment:
 * 
 *    "This product includes software developed by the
 *    University of Chicago, as Operator of Argonne National
 *    Laboratory.
 * 
 * Alternately, this acknowledgment may appear in the software
 * itself, if and wherever such third-party acknowledgments
 * normally appear.
 * 
 * 4. WARRANTY DISCLAIMER. THE SOFTWARE IS SUPPLIED "AS IS"
 * WITHOUT WARRANTY OF ANY KIND. THE COPYRIGHT HOLDER, THE
 * UNITED STATES, THE UNITED STATES DEPARTMENT OF ENERGY, AND
 * THEIR EMPLOYEES: (1) DISCLAIM ANY WARRANTIES, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE
 * OR NON-INFRINGEMENT, (2) DO NOT ASSUME ANY LEGAL LIABILITY
 * OR RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR
 * USEFULNESS OF THE SOFTWARE, (3) DO NOT REPRESENT THAT USE OF
 * THE SOFTWARE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS, (4)
 * DO NOT WARRANT THAT THE SOFTWARE WILL FUNCTION
 * UNINTERRUPTED, THAT IT IS ERROR-FREE OR THAT ANY ERRORS WILL
 * BE CORRECTED.
 * 
 * 5. LIMITATION OF LIABILITY. IN NO EVENT WILL THE COPYRIGHT
 * HOLDER, THE UNITED STATES, THE UNITED STATES DEPARTMENT OF
 * ENERGY, OR THEIR EMPLOYEES: BE LIABLE FOR ANY INDIRECT,
 * INCIDENTAL, CONSEQUENTIAL, SPECIAL OR PUNITIVE DAMAGES OF
 * ANY KIND OR NATURE, INCLUDING BUT NOT LIMITED TO LOSS OF
 * PROFITS OR LOSS OF DATA, FOR ANY REASON WHATSOEVER, WHETHER
 * SUCH LIABILITY IS ASSERTED ON THE BASIS OF CONTRACT, TORT
 * (INCLUDING NEGLIGENCE OR STRICT LIABILITY), OR OTHERWISE,
 * EVEN IF ANY OF SAID PARTIES HAS BEEN WARNED OF THE
 * POSSIBILITY OF SUCH LOSS OR DAMAGES.
 */

/*     subroutine chkder 
 *
 *     this subroutine checks the gradients of m nonlinear functions 
 *     in n variables, evaluated at a point x, for consistency with 
 *     the functions themselves. the user must call chkder twice, 
 *     first with mode = 1 and then with mode = 2. 
 *
 *     mode = 1. on input, x must contain the point of evaluation. 
 *               on output, xp is set to a neighboring point. 
 *
 *     mode = 2. on input, fvec must contain the functions and the 
 *                         rows of fjac must contain the gradients 
 *                         of the respective functions each evaluated 
 *                         at x, and fvecp must contain the functions 
 *                         evaluated at xp. 
 *               on output, err contains measures of correctness of 
 *                          the respective gradients. 
 *
 *     the subroutine does not perform reliably if cancellation or 
 *     rounding errors cause a severe loss of significance in the 
 *     evaluation of a function. therefore, none of the components 
 *     of x should be unusually small (in particular, zero) or any 
 *     other value which may cause loss of significance. 
 *
 *     the subroutine statement is 
 *
 *       subroutine chkder(m,n,x,fvec,fjac,ldfjac,xp,fvecp,mode,err) 
 *
 *     where 
 *
 *       - m is a positive int input variable set to the number of functions. 
 *       - n is a positive int input variable set to the number of variables. 
 *
 *       - x is an input array of length n. 
 *
 *       - fvec is an array of length m. on input when mode = 2, 
 *         fvec must contain the functions evaluated at x. 
 *
 *       - fjac is an m by n array. on input when mode = 2, 
 *         the rows of fjac must contain the gradients of 
 *         the respective functions evaluated at x. 
 *
 *       - ldfjac is a positive int input parameter not less than m 
 *         which specifies the leading dimension of the array fjac. 
 *
 *       - xp is an array of length n. on output when mode = 1, 
 *         xp is set to a neighboring point of x. 
 *
 *       - fvecp is an array of length m. on input when mode = 2, 
 *         fvecp must contain the functions evaluated at xp. 
 *
 *       - mode is an int input variable set to 1 on the first call 
 *         and 2 on the second. other values of mode are equivalent 
 *         to mode = 1. 
 *
 *       - err is an array of length m. on output when mode = 2, 
 *         err contains measures of correctness of the respective 
 *         gradients. if there is no severe loss of significance, 
 *         then if err(i) is 1.0 the i-th gradient is correct, 
 *         while if err(i) is 0.0 the i-th gradient is incorrect. 
 *         for values of err between 0.0 and 1.0, the categorization 
 *         is less certain. in general, a value of err(i) greater 
 *         than 0.5 indicates that the i-th gradient is probably 
 *         correct, while a value of err(i) less than 0.5 indicates 
 *         that the i-th gradient is probably incorrect. 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */


#include "minpack.h"

static double d_lg10 (double *x)
{
  const double log10e=0.43429448190325182765 ;
  return( log10e * log(*x) );
}


int minpack_chkder (int *m, int *n, double *x, double *fvec, double *fjac,
		    int *ldfjac, double *xp, double *fvecp, int *mode,
		    double *err)
{
  int c__1 = 1;
  const double factor = 100.;
  const double one = 1.;
  const double zero = 0.;
  /* System generated locals */
  int fjac_dim1, fjac_offset, i__1, i__2;
  double d__1, d__2, d__3, d__4, d__5;

  /* Builtin functions */
  double epsf, temp;
  int i__, j;
  double epsmch;
  double epslog, eps;

  /* Parameter adjustments */
  --err;
  --fvecp;
  --fvec;
  --xp;
  --x;
  fjac_dim1 = *ldfjac;
  fjac_offset = fjac_dim1 + 1;
  fjac -= fjac_offset;

  /* Function Body */

  /*     epsmch is the machine precision. */

  epsmch = minpack_dpmpar (&c__1);

  eps = sqrt (epsmch);

  if (*mode == 2)
    {
      goto L20;
    }

  /*        mode = 1. */

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      temp = eps * (d__1 = x[j], Abs (d__1));
      if (temp == zero)
	{
	  temp = eps;
	}
      xp[j] = x[j] + temp;
      /* L10: */
    }
  goto L70;
 L20:

  /*        mode = 2. */

  epsf = factor * epsmch;
  epslog = d_lg10 (&eps);
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      err[i__] = zero;
      /* L30: */
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      temp = (d__1 = x[j], Abs (d__1));
      if (temp == zero)
	{
	  temp = one;
	}
      i__2 = *m;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  err[i__] += temp * fjac[i__ + j * fjac_dim1];
	  /* L40: */
	}
      /* L50: */
    }
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      temp = one;
      if (fvec[i__] != zero && fvecp[i__] != zero
	  && (d__1 = fvecp[i__] - fvec[i__], Abs (d__1)) >= epsf * (d__2 =
								    fvec[i__],
								    Abs
								    (d__2)))
	{
	  temp = eps * (d__3 =
			(fvecp[i__] - fvec[i__]) / eps - err[i__],
			Abs (d__3)) / ((d__4 =
					fvec[i__], Abs (d__4)) + (d__5 =
								  fvecp[i__],
								  Abs
								  (d__5)));
	}
      err[i__] = one;
      if (temp > epsmch && temp < eps)
	{
	  err[i__] = (d_lg10 (&temp) - epslog) / epslog;
	}
      if (temp >= eps)
	{
	  err[i__] = zero;
	}
      /* L60: */
    }
 L70:

  return 0;
}

