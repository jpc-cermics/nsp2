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

	


/*     subroutine lmpar 
 *
 *     given an m by n matrix a, an n by n nonsingular diagonal 
 *     matrix d, an m-vector b, and a positive number delta, 
 *     the problem is to determine a value for the parameter 
 *     par such that if x solves the system 
 *
 *           a*x = b ,     sqrt(par)*d*x = 0 , 
 *
 *     in the least squares sense, and dxnorm is the euclidean 
 *     norm of d*x, then either par is zero and 
 *
 *           (dxnorm-delta) .le. 0.1*delta , 
 *
 *     or par is positive and 
 *
 *           Abs(dxnorm-delta) .le. 0.1*delta . 
 *
 *     this subroutine completes the solution of the problem 
 *     if it is provided with the necessary information from the 
 *     qr factorization, with column pivoting, of a. that is, if 
 *     a*p = q*r, where p is a permutation matrix, q has orthogonal 
 *     columns, and r is an upper triangular matrix with diagonal 
 *     elements of nonincreasing magnitude, then lmpar expects 
 *     the full upper triangle of r, the permutation matrix p, 
 *     and the first n components of (q transpose)*b. on output 
 *     lmpar also provides an upper triangular matrix s such that 
 *
 *            t   t                   t 
 *           p *(a *a + par*d*d)*p = s *s . 
 *
 *     s is employed within lmpar and may be of separate interest. 
 *
 *     only a few iterations are generally needed for convergence 
 *     of the algorithm. if, however, the limit of 10 iterations 
 *     is reached, then the output par will contain the best 
 *     value obtained so far. 
 *
 *     the subroutine statement is 
 *
 *       subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag, 
 *                        wa1,wa2) 
 *
 *     where 
 *
 *       n is a positive int input variable set to the order of r. 
 *
 *       r is an n by n array. on input the full upper triangle 
 *         must contain the full upper triangle of the matrix r. 
 *         on output the full upper triangle is unaltered, and the 
 *         strict lower triangle contains the strict upper triangle 
 *         (transposed) of the upper triangular matrix s. 
 *
 *       ldr is a positive int input variable not less than n 
 *         which specifies the leading dimension of the array r. 
 *
 *       ipvt is an int input array of length n which defines the 
 *         permutation matrix p such that a*p = q*r. column j of p 
 *         is column ipvt(j) of the identity matrix. 
 *
 *       diag is an input array of length n which must contain the 
 *         diagonal elements of the matrix d. 
 *
 *       qtb is an input array of length n which must contain the first 
 *         n elements of the vector (q transpose)*b. 
 *
 *       delta is a positive input variable which specifies an upper 
 *         bound on the euclidean norm of d*x. 
 *
 *       par is a nonnegative variable. on input par contains an 
 *         initial estimate of the levenberg-marquardt parameter. 
 *         on output par contains the final estimate. 
 *
 *       x is an output array of length n which contains the least 
 *         squares solution of the system a*x = b, sqrt(par)*d*x = 0, 
 *         for the output par. 
 *
 *       sdiag is an output array of length n which contains the 
 *         diagonal elements of the upper triangular matrix s. 
 *
 *       wa1 and wa2 are work arrays of length n. 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

#include "minpack.h"


int minpack_lmpar (int *n, double *r__, int *ldr, int *ipvt, double *diag,
		   double *qtb, double *delta, double *par, double *x,
		   double *sdiag, double *wa1, double *wa2)
{
  const double p1 = .1;
  const double p001 = .001;
  const double zero = 0.;

  /* System generated locals */
  int r_dim1, r_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  double parc, parl;
  int iter;
  double temp, paru;
  int i__, j, k, l;
  double dwarf;
  int nsing;
  double gnorm, fp;
  double dxnorm;
  int jm1, jp1;
  double sum;

  --wa2;
  --wa1;
  --sdiag;
  --x;
  --qtb;
  --diag;
  --ipvt;
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;

  /* Function Body */

  /*     dwarf is the smallest positive magnitude. */

  dwarf = minpack_dpmpar (2);

  /*     compute and store in x the gauss-newton direction. if the */
  /*     jacobian is rank-deficient, obtain a least squares solution. */

  nsing = *n;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa1[j] = qtb[j];
      if (r__[j + j * r_dim1] == zero && nsing == *n)
	{
	  nsing = j - 1;
	}
      if (nsing < *n)
	{
	  wa1[j] = zero;
	}
      /* L10: */
    }
  if (nsing < 1)
    {
      goto L50;
    }
  i__1 = nsing;
  for (k = 1; k <= i__1; ++k)
    {
      j = nsing - k + 1;
      wa1[j] /= r__[j + j * r_dim1];
      temp = wa1[j];
      jm1 = j - 1;
      if (jm1 < 1)
	{
	  goto L30;
	}
      i__2 = jm1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  wa1[i__] -= r__[i__ + j * r_dim1] * temp;
	  /* L20: */
	}
    L30:
      /* L40: */
      ;
    }
 L50:
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      l = ipvt[j];
      x[l] = wa1[j];
      /* L60: */
    }

  /*     initialize the iteration counter. */
  /*     evaluate the function at the origin, and test */
  /*     for acceptance of the gauss-newton direction. */

  iter = 0;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa2[j] = diag[j] * x[j];
      /* L70: */
    }
  dxnorm = minpack_enorm (*n, &wa2[1]);
  fp = dxnorm - *delta;
  if (fp <= p1 * *delta)
    {
      goto L220;
    }

  /*     if the jacobian is not rank deficient, the newton */
  /*     step provides a lower bound, parl, for the zero of */
  /*     the function. otherwise set this bound to zero. */

  parl = zero;
  if (nsing < *n)
    {
      goto L120;
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      l = ipvt[j];
      wa1[j] = diag[l] * (wa2[l] / dxnorm);
      /* L80: */
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      sum = zero;
      jm1 = j - 1;
      if (jm1 < 1)
	{
	  goto L100;
	}
      i__2 = jm1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  sum += r__[i__ + j * r_dim1] * wa1[i__];
	  /* L90: */
	}
    L100:
      wa1[j] = (wa1[j] - sum) / r__[j + j * r_dim1];
      /* L110: */
    }
  temp = minpack_enorm (*n, &wa1[1]);
  parl = fp / *delta / temp / temp;
 L120:

  /*     calculate an upper bound, paru, for the zero of the function. */

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      sum = zero;
      i__2 = j;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  sum += r__[i__ + j * r_dim1] * qtb[i__];
	  /* L130: */
	}
      l = ipvt[j];
      wa1[j] = sum / diag[l];
      /* L140: */
    }
  gnorm = minpack_enorm (*n, &wa1[1]);
  paru = gnorm / *delta;
  if (paru == zero)
    {
      paru = dwarf / Min (*delta, p1);
    }

  /*     if the input par lies outside of the interval (parl,paru), */
  /*     set par to the closer endpoint. */

  *par = Max (*par, parl);
  *par = Min (*par, paru);
  if (*par == zero)
    {
      *par = gnorm / dxnorm;
    }

  /*     beginning of an iteration. */

 L150:
  ++iter;

  /*        evaluate the function at the current value of par. */

  if (*par == zero)
    {
      /* Computing MAX */
      d__1 = dwarf, d__2 = p001 * paru;
      *par = Max (d__1, d__2);
    }
  temp = sqrt (*par);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa1[j] = temp * diag[j];
      /* L160: */
    }
  minpack_qrsolv (n, &r__[r_offset], ldr, &ipvt[1], &wa1[1], &qtb[1], &x[1],
		  &sdiag[1], &wa2[1]);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa2[j] = diag[j] * x[j];
      /* L170: */
    }
  dxnorm = minpack_enorm (*n, &wa2[1]);
  temp = fp;
  fp = dxnorm - *delta;

  /*        if the function is small enough, accept the current value */
  /*        of par. also test for the exceptional cases where parl */
  /*        is zero or the number of iterations has reached 10. */

  if (Abs (fp) <= p1 * *delta 
      || (parl == zero && fp <= temp && temp < zero)
      || iter == 10)
    {
      goto L220;
    }

  /*        compute the newton correction. */

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      l = ipvt[j];
      wa1[j] = diag[l] * (wa2[l] / dxnorm);
      /* L180: */
    }
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      wa1[j] /= sdiag[j];
      temp = wa1[j];
      jp1 = j + 1;
      if (*n < jp1)
	{
	  goto L200;
	}
      i__2 = *n;
      for (i__ = jp1; i__ <= i__2; ++i__)
	{
	  wa1[i__] -= r__[i__ + j * r_dim1] * temp;
	  /* L190: */
	}
    L200:
      /* L210: */
      ;
    }
  temp = minpack_enorm (*n, &wa1[1]);
  parc = fp / *delta / temp / temp;

  /*        depending on the sign of the function, update parl or paru. */

  if (fp > zero)
    {
      parl = Max (parl, *par);
    }
  if (fp < zero)
    {
      paru = Min (paru, *par);
    }

  /*        compute an improved estimate for par. */

  /* Computing MAX */
  d__1 = parl, d__2 = *par + parc;
  *par = Max (d__1, d__2);

  /*        end of an iteration. */

  goto L150;
 L220:

  /*     termination. */

  if (iter == 0)
    {
      *par = zero;
    }
  return 0;
}
