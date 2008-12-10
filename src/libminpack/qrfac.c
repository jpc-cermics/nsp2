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

	


/*     subroutine qrfac 
 *
 *     this subroutine uses householder transformations with column 
 *     pivoting (optional) to compute a qr factorization of the 
 *     m by n matrix a. that is, qrfac determines an orthogonal 
 *     matrix q, a permutation matrix p, and an upper trapezoidal 
 *     matrix r with diagonal elements of nonincreasing magnitude, 
 *     such that a*p = q*r. the householder transformation for 
 *     column k, k = 1,2,...,min(m,n), is of the form 
 *
 *                           t 
 *           i - (1/u(k))*u*u 
 *
 *     where u has zeros in the first k-1 positions. the form of 
 *     this transformation and the method of pivoting first 
 *     appeared in the corresponding linpack subroutine. 
 *
 *     the subroutine statement is 
 *
 *       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa) 
 *
 *     where 
 *
 *       m is a positive int input variable set to the number 
 *         of rows of a. 
 *
 *       n is a positive int input variable set to the number 
 *         of columns of a. 
 *
 *       a is an m by n array. on input a contains the matrix for 
 *         which the qr factorization is to be computed. on output 
 *         the strict upper trapezoidal part of a contains the strict 
 *         upper trapezoidal part of r, and the lower trapezoidal 
 *         part of a contains a factored form of q (the non-trivial 
 *         elements of the u vectors described above). 
 *
 *       lda is a positive int input variable not less than m 
 *         which specifies the leading dimension of the array a. 
 *
 *       pivot is a int input variable. if pivot is set true, 
 *         then column pivoting is enforced. if pivot is set false, 
 *         then no column pivoting is done. 
 *
 *       ipvt is an int output array of length lipvt. ipvt 
 *         defines the permutation matrix p such that a*p = q*r. 
 *         column j of p is column ipvt(j) of the identity matrix. 
 *         if pivot is false, ipvt is not referenced. 
 *
 *       lipvt is a positive int input variable. if pivot is false, 
 *         then lipvt may be as small as 1. if pivot is true, then 
 *         lipvt must be at least n. 
 *
 *       rdiag is an output array of length n which contains the 
 *         diagonal elements of r. 
 *
 *       acnorm is an output array of length n which contains the 
 *         norms of the corresponding columns of the input matrix a. 
 *         if this information is not needed, then acnorm can coincide 
 *         with rdiag. 
 *
 *       wa is a work array of length n. if pivot is false, then wa 
 *         can coincide with rdiag. 
 *
 *     subprograms called 
 *
 *       minpack-supplied ... dpmpar,enorm 
 *
 *       fortran-supplied ... dmax1,dsqrt,min0 
 *
 *     argonne national laboratory. minpack project. march 1980. 
 *     burton s. Garbow, kenneth e. Hillstrom, jorge j. More 
 */

#include "minpack.h"

int minpack_qrfac (int *m, int *n, double *a, int *lda, int *pivot, int *ipvt,
		   int *lipvt, double *rdiag, double *acnorm, double *wa)
{
  const double one = 1.0,  p05 = .05, zero = 0.;
  int c__1 = 1, a_dim1, a_offset, i2, i3, kmax, jp1, i1 , j, k, minmn;
  double epsmch, ajnorm, temp, sum, d__1, d__2, d__3;

  /* Parameter adjustments */
  --wa;
  --acnorm;
  --rdiag;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipvt;

  /*     epsmch is the machine precision. */
  epsmch = minpack_dpmpar (&c__1);

  /*     compute the initial column norms and initialize several arrays. */

  for (j = 1; j <= *n; ++j)
    {
      acnorm[j] = minpack_enorm (m, &a[j * a_dim1 + 1]);
      rdiag[j] = acnorm[j];
      wa[j] = rdiag[j];
      if (*pivot)   ipvt[j] = j;
    }

  /*     reduce a to r with householder transformations. */

  minmn = Min (*m, *n);
  for (j = 1; j <= minmn ; ++j)
    {
      if (!(*pivot))
	{
	  goto L40;
	}

      /*        bring the column of largest norm into the pivot position. */

      kmax = j;
      for (k = j; k <= *n ; ++k)
	{
	  if (rdiag[k] > rdiag[kmax])
	    {
	      kmax = k;
	    }
	}
      if (kmax == j)
	{
	  goto L40;
	}

      for (i1 = 1; i1 <= *m; ++i1)
	{
	  temp = a[i1 + j * a_dim1];
	  a[i1 + j * a_dim1] = a[i1 + kmax * a_dim1];
	  a[i1 + kmax * a_dim1] = temp;
	}
      rdiag[kmax] = rdiag[j];
      wa[kmax] = wa[j];
      k = ipvt[j];
      ipvt[j] = ipvt[kmax];
      ipvt[kmax] = k;
    L40:

      /*        compute the householder transformation to reduce the */
      /*        j-th column of a to a multiple of the j-th unit vector. */

      i2 = *m - j + 1;
      ajnorm = minpack_enorm (&i2, &a[j + j * a_dim1]);
      if (ajnorm == zero)
	{
	  goto L100;
	}
      if (a[j + j * a_dim1] < zero)
	{
	  ajnorm = -ajnorm;
	}

      for (i1 = j; i1 <= *m; ++i1)
	{
	  a[i1 + j * a_dim1] /= ajnorm;
	}
      a[j + j * a_dim1] += one;

      /*        apply the transformation to the remaining columns */
      /*        and update the norms. */

      jp1 = j + 1;
      if (*n < jp1)
	{
	  goto L100;
	}

      for (k = jp1; k <= *n; ++k)
	{
	  sum = zero;

	  for (i1 = j; i1 <= *m; ++i1)
	    {
	      sum += a[i1 + j * a_dim1] * a[i1 + k * a_dim1];
	      /* L60: */
	    }
	  temp = sum / a[j + j * a_dim1];

	  for (i1 = j; i1 <= *m; ++i1)
	    {
	      a[i1 + k * a_dim1] -= temp * a[i1 + j * a_dim1];
	    }
	  if (!(*pivot) || rdiag[k] == zero)
	    {
	      goto L80;
	    }
	  temp = a[j + k * a_dim1] / rdiag[k];
	  /* Computing MAX */
	  /* Computing 2nd power */
	  d__3 = temp;
	  d__1 = zero, d__2 = one - d__3 * d__3;
	  rdiag[k] *= sqrt ((Max (d__1, d__2)));
	  /* Computing 2nd power */
	  d__1 = rdiag[k] / wa[k];
	  if (p05 * (d__1 * d__1) > epsmch)
	    {
	      goto L80;
	    }
	  i3 = *m - j;
	  rdiag[k] = minpack_enorm (&i3, &a[jp1 + k * a_dim1]);
	  wa[k] = rdiag[k];
	L80:
	  ;
	}
    L100:
      rdiag[j] = -ajnorm;
    }
  return 0;
}
