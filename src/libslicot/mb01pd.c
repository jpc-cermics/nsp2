/* MB01PD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Subroutine */ int
nsp_slicot_mb01pd (char *scun, char *type__, int *m, int *n, int *kl,
		   int *ku, double *anrm, int *nbl, int *nrows,
		   double *a, int *lda, int *info, long int scun_len,
		   long int type_len)
{
  /* Initialized data */

  static int first = TRUE;

  /* System generated locals */
  int a_dim1, a_offset, i__1;

  /* Local variables */
  int isum, i__;
  int itype;
  int mn;
  int lscale;
  static double bignum, smlnum;

  /* 
   *    SLICOT RELEASE 5.0. 
   * 
   *    Copyright (c) 2002-2009 NICONET e.V. 
   * 
   *    This program is free software: you can redistribute it and/or 
   *    modify it under the terms of the GNU General Public License as 
   *    published by the Free Software Foundation, either version 2 of 
   *    the License, or (at your option) any later version. 
   * 
   *    This program is distributed in the hope that it will be useful, 
   *    but WITHOUT ANY WARRANTY; without even the implied warranty of 
   *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
   *    GNU General Public License for more details. 
   * 
   *    You should have received a copy of the GNU General Public License 
   *    along with this program.  If not, see 
   *    <http://www.gnu.org/licenses/>. 
   * 
   *    PURPOSE 
   * 
   *    To scale a matrix or undo scaling.  Scaling is performed, if 
   *    necessary, so that the matrix norm will be in a safe range of 
   *    representable numbers. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    SCUN    CHARACTER*1 
   *            SCUN indicates the operation to be performed. 
   *            = 'S':  scale the matrix. 
   *            = 'U':  undo scaling of the matrix. 
   * 
   *    TYPE    CHARACTER*1 
   *            TYPE indicates the storage type of the input matrix. 
   *            = 'G':  A is a full matrix. 
   *            = 'L':  A is a (block) lower triangular matrix. 
   *            = 'U':  A is an (block) upper triangular matrix. 
   *            = 'H':  A is an (block) upper Hessenberg matrix. 
   *            = 'B':  A is a symmetric band matrix with lower bandwidth 
   *                    KL and upper bandwidth KU and with the only the 
   *                    lower half stored. 
   *            = 'Q':  A is a symmetric band matrix with lower bandwidth 
   *                    KL and upper bandwidth KU and with the only the 
   *                    upper half stored. 
   *            = 'Z':  A is a band matrix with lower bandwidth KL and 
   *                    upper bandwidth KU. 
   * 
   *    Input/Output Parameters 
   * 
   *    M       (input) INT 
   *            The number of rows of the matrix A. M >= 0. 
   * 
   *    N       (input) INT 
   *            The number of columns of the matrix A. N >= 0. 
   * 
   *    KL      (input) INT 
   *            The lower bandwidth of A.  Referenced only if TYPE = 'B', 
   *            'Q' or 'Z'. 
   * 
   *    KU      (input) INT 
   *            The upper bandwidth of A.  Referenced only if TYPE = 'B', 
   *            'Q' or 'Z'. 
   * 
   *    ANRM    (input) DOUBLE PRECISION 
   *            The norm of the initial matrix A.  ANRM >= 0. 
   *            When  ANRM = 0  then an immediate return is effected. 
   *            ANRM should be preserved between the call of the routine 
   *            with SCUN = 'S' and the corresponding one with SCUN = 'U'. 
   * 
   *    NBL     (input) INT 
   *            The number of diagonal blocks of the matrix A, if it has a 
   *            block structure.  To specify that matrix A has no block 
   *            structure, set NBL = 0.  NBL >= 0. 
   * 
   *    NROWS   (input) INT array, dimension Max(1,NBL) 
   *            NROWS(i) contains the number of rows and columns of the 
   *            i-th diagonal block of matrix A.  The sum of the values 
   *            NROWS(i),  for  i = 1: NBL,  should be equal to Min(M,N). 
   *            The elements of the array  NROWS  are not referenced if 
   *            NBL = 0. 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) 
   *            On entry, the leading M by N part of this array must 
   *            contain the matrix to be scaled/unscaled. 
   *            On exit, the leading M by N part of A will contain 
   *            the modified matrix. 
   *            The storage mode of A is specified by TYPE. 
   * 
   *    LDA     (input) INT 
   *            The leading dimension of the array A.  LDA  >= Max(1,M). 
   * 
   *    Error Indicator 
   * 
   *    INFO    (output) INT 
   *            = 0:  successful exit 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value. 
   * 
   *    METHOD 
   * 
   *    Denote by ANRM the norm of the matrix, and by SMLNUM and BIGNUM, 
   *    two positive numbers near the smallest and largest safely 
   *    representable numbers, respectively.  The matrix is scaled, if 
   *    needed, such that the norm of the result is in the range 
   *    [SMLNUM, BIGNUM].  The scaling factor is represented as a ratio 
   *    of two numbers, one of them being ANRM, and the other one either 
   *    SMLNUM or BIGNUM, depending on ANRM being less than SMLNUM or 
   *    larger than BIGNUM, respectively.  For undoing the scaling, the 
   *    norm is again compared with SMLNUM or BIGNUM, and the reciprocal 
   *    of the previous scaling factor is used. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, Nov. 1996. 
   * 
   *    REVISIONS 
   * 
   *    Oct. 2001, V. Sima, Research Institute for Informatics, Bucharest. 
   * 
   *   ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
   *    .. External Functions .. 
   *    .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
   *    .. Save statement .. 
   *    .. Data statements .. 
   */
  /* Parameter adjustments */
  --nrows;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  /*    .. 
   *    .. Executable Statements .. 
   * 
   *    Test the input scalar arguments. 
   * 
   */
  *info = 0;
  lscale = C2F (lsame) (scun, "S", 1L, 1L);
  if (C2F (lsame) (type__, "G", 1L, 1L))
    {
      itype = 0;
    }
  else if (C2F (lsame) (type__, "L", 1L, 1L))
    {
      itype = 1;
    }
  else if (C2F (lsame) (type__, "U", 1L, 1L))
    {
      itype = 2;
    }
  else if (C2F (lsame) (type__, "H", 1L, 1L))
    {
      itype = 3;
    }
  else if (C2F (lsame) (type__, "B", 1L, 1L))
    {
      itype = 4;
    }
  else if (C2F (lsame) (type__, "Q", 1L, 1L))
    {
      itype = 5;
    }
  else if (C2F (lsame) (type__, "Z", 1L, 1L))
    {
      itype = 6;
    }
  else
    {
      itype = -1;
    }
  /* 
   */
  mn = Min (*m, *n);
  /* 
   */
  isum = 0;
  if (*nbl > 0)
    {
      i__1 = *nbl;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  isum += nrows[i__];
	  /* L10: */
	}
    }
  /* 
   */
  if (!lscale && !C2F (lsame) (scun, "U", 1L, 1L))
    {
      *info = -1;
    }
  else if (itype == -1)
    {
      *info = -2;
    }
  else if (*m < 0)
    {
      *info = -3;
    }
  else if (*n < 0 || (itype == 4 || itype == 5) && *n != *m)
    {
      *info = -4;
    }
  else if (*anrm < 0.)
    {
      *info = -7;
    }
  else if (*nbl < 0)
    {
      *info = -8;
    }
  else if (*nbl > 0 && isum != mn)
    {
      *info = -9;
    }
  else if (itype <= 3 && *lda < Max (1, *m))
    {
      *info = -11;
    }
  else if (itype >= 4)
    {
      /*Computing MAX 
       */
      i__1 = *m - 1;
      if (*kl < 0 || *kl > Max (i__1, 0))
	{
	  *info = -5;
	}
      else			/* if(complicated condition) */
	{
	  /*Computing MAX 
	   */
	  i__1 = *n - 1;
	  if (*ku < 0 || *ku > Max (i__1, 0) || (itype == 4 || itype == 5)
	      && *kl != *ku)
	    {
	      *info = -6;
	    }
	  else if (itype == 4 && *lda < *kl + 1 || itype == 5
		   && *lda < *ku + 1 || itype == 6
		   && *lda < (*kl << 1) + *ku + 1)
	    {
	      *info = -11;
	    }
	}
    }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("MB01PD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (mn == 0 || *anrm == 0.)
    {
      return 0;
    }
  /* 
   */
  if (first)
    {
      /* 
       *       Get machine parameters. 
       * 
       */
      smlnum = C2F (dlamch) ("S", 1L) / C2F (dlamch) ("P", 1L);
      bignum = 1. / smlnum;
      C2F (dlabad) (&smlnum, &bignum);
      first = FALSE;
    }
  /* 
   */
  if (lscale)
    {
      /* 
       *       Scale A, if its norm is outside range [SMLNUM,BIGNUM]. 
       * 
       */
      if (*anrm < smlnum)
	{
	  /* 
	   *          Scale matrix norm up to SMLNUM. 
	   * 
	   */
	  nsp_slicot_mb01qd (type__, m, n, kl, ku, anrm, &smlnum, nbl,
			     &nrows[1], &a[a_offset], lda, info, 1L);
	}
      else if (*anrm > bignum)
	{
	  /* 
	   *          Scale matrix norm down to BIGNUM. 
	   * 
	   */
	  nsp_slicot_mb01qd (type__, m, n, kl, ku, anrm, &bignum, nbl,
			     &nrows[1], &a[a_offset], lda, info, 1L);
	}
      /* 
       */
    }
  else
    {
      /* 
       *       Undo scaling. 
       * 
       */
      if (*anrm < smlnum)
	{
	  nsp_slicot_mb01qd (type__, m, n, kl, ku, &smlnum, anrm, nbl,
			     &nrows[1], &a[a_offset], lda, info, 1L);
	}
      else if (*anrm > bignum)
	{
	  nsp_slicot_mb01qd (type__, m, n, kl, ku, &bignum, anrm, nbl,
			     &nrows[1], &a[a_offset], lda, info, 1L);
	}
    }
  /* 
   */
  return 0;
  /**** Last line of MB01PD *** 
   */
}				/* nsp_slicot_mb01pd */
