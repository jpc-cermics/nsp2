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

	


/*     subroutine dogleg 
 *     given an m by n matrix a, an n by n nonsingular diagonal 
 *     matrix d, an m-vector b, and a positive number delta, the 
 *     problem is to determine the convex combination x of the 
 *     gauss-newton and scaled gradient directions that minimizes 
 *     (a*x - b) in the least squares sense, subject to the 
 *     restriction that the euclidean norm of d*x be at most delta. 
 *
 *     this subroutine completes the solution of the problem 
 *     if it is provided with the necessary information from the 
 *     qr factorization of a. that is, if a = q*r, where q has 
 *     orthogonal columns and r is an upper triangular matrix, 
 *     then dogleg expects the full upper triangle of r and 
 *     the first n components of (q transpose)*b. 
 *
 *     the subroutine statement is 
 *       subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2) 
 *     where 
 *       n is a positive int input variable set to the order of r. 
 *       r is an input array of length lr which must contain the upper 
 *         triangular matrix r stored by rows. 
 *       lr is a positive int input variable not less than 
 *         (n*(n+1))/2. 
 *       diag is an input array of length n which must contain the 
 *         diagonal elements of the matrix d. 
 *       qtb is an input array of length n which must contain the first 
 *         n elements of the vector (q transpose)*b. 
 *       delta is a positive input variable which specifies an upper 
 *         bound on the euclidean norm of d*x. 
 *       x is an output array of length n which contains the desired 
 *         convex combination of the gauss-newton direction and the 
 *         scaled gradient direction. 
 *       wa1 and wa2 are work arrays of length n. 
 *     subprograms called 
 *       minpack-supplied ... dpmpar,enorm 
 *       fortran-supplied ... dabs,dmax1,dmin1,dsqrt 
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

#include "minpack.h"

int minpack_dogleg (int n,const double *r, int lr, const double *diag,const  double *qtb,
		    double delta, double *x, double *wa1, double *wa2)
{
  const double one = 1. ,  zero = 0.;
  double d1, d2, d3, d4,  temp, alpha, bnorm, gnorm, qnorm, epsmch, sgnorm, sum;
  int i, j, k, l;  int jj;  int jp1;

  /*     epsmch is the machine precision. */
  epsmch = minpack_dpmpar (1);
  /*     first, calculate the gauss-newton direction. */

  jj = n * (n + 1) / 2 + 1;

  for (k = 1; k <= n; ++k)
    {
      j = n - k + 1;
      jp1 = j + 1;
      jj -= k;
      l = jj + 1;
      sum = zero;
      if (n < jp1)
	{
	  goto L20;
	}

      for (i = jp1; i <= n ; ++i)
	{
	  sum += r[l-1] * x[i-1];
	  ++l;
	}
    L20:
      temp = r[jj-1];
      if (temp != zero)
	{
	  goto L40;
	}
      l = j;

      for (i = 1; i <= j ; ++i)
	{
	  d2 = temp, d3 = (d1 = r[l-1], Abs (d1));
	  temp = Max (d2, d3);
	  l = l + n - i;
	}
      temp = epsmch * temp;
      if (temp == zero)
	{
	  temp = epsmch;
	}
    L40:
      x[j-1] = (qtb[j-1] - sum) / temp;
    }

  /*     test whether the gauss-newton direction is acceptable. */


  for (j = 1; j <= n ; ++j)
    {
      wa1[j-1] = zero;
      wa2[j-1] = diag[j-1] * x[j-1];
    }
  qnorm = minpack_enorm (n, wa2);
  if (qnorm <= delta)
    {
      goto L140;
    }

  /*     the gauss-newton direction is not acceptable. 
   *     next, calculate the scaled gradient direction.
   */

  l = 1;

  for (j = 1; j <= n; ++j)
    {
      temp = qtb[j-1];

      for (i = j; i <= n; ++i)
	{
	  wa1[i-1] += r[l-1] * temp;
	  ++l;
	}
      wa1[j-1] /= diag[j-1];
    }

  /*     calculate the norm of the scaled gradient and test for 
   *     the special case in which the scaled gradient is zero. 
   */

  gnorm = minpack_enorm (n, wa1);
  sgnorm = zero;
  alpha = delta / qnorm;
  if (gnorm == zero)
    {
      goto L120;
    }

  /*     calculate the point along the scaled gradient 
   *     at which the quadratic is minimized. 
   */


  for (j = 1; j <= n; ++j)
    {
      wa1[j-1] = wa1[j-1] / gnorm / diag[j-1];
    }
  l = 1;

  for (j = 1; j <= n; ++j)
    {
      sum = zero;

      for (i = j; i <= n ; ++i)
	{
	  sum += r[l-1] * wa1[i-1];
	  ++l;
	}
      wa2[j-1] = sum;
    }
  temp = minpack_enorm (n, wa2);
  sgnorm = gnorm / temp / temp;

  /*     test whether the scaled gradient direction is acceptable. */

  alpha = zero;
  if (sgnorm >= delta)
    {
      goto L120;
    }

  /*     the scaled gradient direction is not acceptable.
   *     finally, calculate the point along the dogleg 
   *     at which the quadratic is minimized. 
   */

  bnorm = minpack_enorm (n, qtb);
  temp = bnorm / gnorm * (bnorm / qnorm) * (sgnorm / delta);
  d1 = sgnorm / delta;
  d2 = temp - delta / qnorm;
  d3 = delta / qnorm;
  d4 = sgnorm / delta;
  temp = temp - delta / qnorm * (d1 * d1) 
    + sqrt (d2 * d2 +  (one - d3 * d3) * (one - d4 * d4));
  d1 = sgnorm / delta;
  alpha = delta / qnorm * (one - d1 * d1) / temp;
 L120:

  /*     form appropriate convex combination of the gauss-newton 
   *     direction and the scaled gradient direction. 
   */

  temp = (one - alpha) * Min (sgnorm, delta);

  for (j = 1; j <= n ; ++j)
    {
      x[j-1] = temp * wa1[j-1] + alpha * x[j-1];
    }
 L140:
  return 0;
}
