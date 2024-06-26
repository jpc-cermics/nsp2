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

	


/*     subroutine qform 
 *
 *     this subroutine proceeds from the computed qr factorization of 
 *     an m by n matrix a to accumulate the m by m orthogonal matrix 
 *     q from its factored form. 
 *
 *     the subroutine statement is 
 *
 *       subroutine qform(m,n,q,ldq,wa) 
 *
 *     where 
 *
 *       m is a positive int input variable set to the number 
 *         of rows of a and the order of q. 
 *
 *       n is a positive int input variable set to the number 
 *         of columns of a. 
 *
 *       q is an m by m array. on input the full lower trapezoid in 
 *         the first Min(m,n) columns of q contains the factored form. 
 *         on output q has been accumulated into a square matrix. 
 *
 *       ldq is a positive int input variable not less than m 
 *         which specifies the leading dimension of the array q. 
 *
 *       wa is a work array of length m. 
 *
 *     subprograms called 
 *
 *       fortran-supplied ... min0 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

#include "minpack.h"

int minpack_qform (int *m, int *n, double *q, int *ldq, double *wa)
{
  /* Initialized data */

  const double one = 1.;
  const double zero = 0.;

  /* System generated locals */
  int q_dim1, q_offset, i__1, i__2, i__3;

  /* Local variables */
  double temp;
  int i__, j, k, l, minmn, jm1, np1;
  double sum;

  /* Parameter adjustments */
  --wa;
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;

  /* Function Body */

  /*     zero out upper triangle of q in the first Min(m,n) columns. */

  minmn = Min (*m, *n);
  if (minmn < 2)
    {
      goto L30;
    }
  i__1 = minmn;
  for (j = 2; j <= i__1; ++j)
    {
      jm1 = j - 1;
      i__2 = jm1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  q[i__ + j * q_dim1] = zero;
	  /* L10: */
	}
      /* L20: */
    }
 L30:

  /*     initialize remaining columns to those of the identity matrix. */

  np1 = *n + 1;
  if (*m < np1)
    {
      goto L60;
    }
  i__1 = *m;
  for (j = np1; j <= i__1; ++j)
    {
      i__2 = *m;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  q[i__ + j * q_dim1] = zero;
	  /* L40: */
	}
      q[j + j * q_dim1] = one;
      /* L50: */
    }
 L60:

  /*     accumulate q from its factored form. */

  i__1 = minmn;
  for (l = 1; l <= i__1; ++l)
    {
      k = minmn - l + 1;
      i__2 = *m;
      for (i__ = k; i__ <= i__2; ++i__)
	{
	  wa[i__] = q[i__ + k * q_dim1];
	  q[i__ + k * q_dim1] = zero;
	  /* L70: */
	}
      q[k + k * q_dim1] = one;
      if (wa[k] == zero)
	{
	  goto L110;
	}
      i__2 = *m;
      for (j = k; j <= i__2; ++j)
	{
	  sum = zero;
	  i__3 = *m;
	  for (i__ = k; i__ <= i__3; ++i__)
	    {
	      sum += q[i__ + j * q_dim1] * wa[i__];
	      /* L80: */
	    }
	  temp = sum / wa[k];
	  i__3 = *m;
	  for (i__ = k; i__ <= i__3; ++i__)
	    {
	      q[i__ + j * q_dim1] -= temp * wa[i__];
	      /* L90: */
	    }
	  /* L100: */
	}
    L110:
      /* L120: */
      ;
    }
  return 0;
}
