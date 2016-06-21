/* MB03OD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__2 = 2;
static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_mb03od (char *jobqr, int *m, int *n, double *a, int *lda,
		   int *jpvt, double *rcond, double *svlmax,
		   double *tau, int *rank, double *sval,
		   double *dwork, int *ldwork, int *info, long int jobqr_len)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;
  double d__1;

  /* Local variables */
  double smin, smax;
  int i__;
  int ismin, ismax;
  double c1, c2;
  double s1, s2;
  int mn;
  int ljobqr;
  int minwrk;
  double sminpr;
  int maxwrk;
  double smaxpr;

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
   *    To compute (optionally) a rank-revealing QR factorization of a 
   *    real general M-by-N matrix  A,  which may be rank-deficient, 
   *    and estimate its effective rank using incremental condition 
   *    estimation. 
   * 
   *    The routine uses a QR factorization with column pivoting: 
   *       A * P = Q * R,  where  R = [ R11 R12 ], 
   *                                  [  0  R22 ] 
   *    with R11 defined as the largest leading submatrix whose estimated 
   *    condition number is less than 1/RCOND.  The order of R11, RANK, 
   *    is the effective rank of A. 
   * 
   *    MB03OD  does not perform any scaling of the matrix A. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    JOBQR   CHARACTER*1 
   *            = 'Q':  Perform a QR factorization with column pivoting; 
   *            = 'N':  Do not perform the QR factorization (but assume 
   *                    that it has been done outside). 
   * 
   *    Input/Output Parameters 
   * 
   *    M       (input) INT 
   *            The number of rows of the matrix A.  M >= 0. 
   * 
   *    N       (input) INT 
   *            The number of columns of the matrix A.  N >= 0. 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension 
   *            ( LDA, N ) 
   *            On entry with JOBQR = 'Q', the leading M by N part of this 
   *            array must contain the given matrix A. 
   *            On exit with JOBQR = 'Q', the leading Min(M,N) by N upper 
   *            triangular part of A contains the triangular factor R, 
   *            and the elements below the diagonal, with the array TAU, 
   *            represent the orthogonal matrix Q as a product of 
   *            Min(M,N) elementary reflectors. 
   *            On entry and on exit with JOBQR = 'N', the leading 
   *            Min(M,N) by N upper triangular part of A contains the 
   *            triangular factor R, as determined by the QR factorization 
   *            with pivoting.  The elements below the diagonal of A are 
   *            not referenced. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= Max(1,M). 
   * 
   *    JPVT    (input/output) INT array, dimension ( N ) 
   *            On entry with JOBQR = 'Q', if JPVT(i) <> 0, the i-th 
   *            column of A is an initial column, otherwise it is a free 
   *            column. Before the QR factorization of A, all initial 
   *            columns are permuted to the leading positions; only the 
   *            remaining free columns are moved as a result of column 
   *            pivoting during the factorization.  For rank determination 
   *            it is preferable that all columns be free. 
   *            On exit with JOBQR = 'Q', if JPVT(i) = k, then the i-th 
   *            column of A*P was the k-th column of A. 
   *            Array JPVT is not referenced when JOBQR = 'N'. 
   * 
   *    RCOND   (input) DOUBLE PRECISION 
   *            RCOND is used to determine the effective rank of A, which 
   *            is defined as the order of the largest leading triangular 
   *            submatrix R11 in the QR factorization with pivoting of A, 
   *            whose estimated condition number is less than 1/RCOND. 
   *            RCOND >= 0. 
   *            NOTE that when SVLMAX > 0, the estimated rank could be 
   *            less than that defined above (see SVLMAX). 
   * 
   *    SVLMAX  (input) DOUBLE PRECISION 
   *            If A is a submatrix of another matrix B, and the rank 
   *            decision should be related to that matrix, then SVLMAX 
   *            should be an estimate of the largest singular value of B 
   *            (for instance, the Frobenius norm of B).  If this is not 
   *            the case, the input value SVLMAX = 0 should work. 
   *            SVLMAX >= 0. 
   * 
   *    TAU     (output) DOUBLE PRECISION array, dimension ( MIN( M, N ) ) 
   *            On exit with JOBQR = 'Q', the leading Min(M,N) elements of 
   *            TAU contain the scalar factors of the elementary 
   *            reflectors. 
   *            Array TAU is not referenced when JOBQR = 'N'. 
   * 
   *    RANK    (output) INT 
   *            The effective (estimated) rank of A, i.e. the order of 
   *            the submatrix R11. 
   * 
   *    SVAL    (output) DOUBLE PRECISION array, dimension ( 3 ) 
   *            The estimates of some of the singular values of the 
   *            triangular factor R: 
   *            SVAL(1): largest singular value of R(1:RANK,1:RANK); 
   *            SVAL(2): smallest singular value of R(1:RANK,1:RANK); 
   *            SVAL(3): smallest singular value of R(1:RANK+1,1:RANK+1), 
   *                     if RANK < MIN( M, N ), or of R(1:RANK,1:RANK), 
   *                     otherwise. 
   *            If the triangular factorization is a rank-revealing one 
   *            (which will be the case if the leading columns were well- 
   *            conditioned), then SVAL(1) will also be an estimate for 
   *            the largest singular value of A, and SVAL(2) and SVAL(3) 
   *            will be estimates for the RANK-th and (RANK+1)-st singular 
   *            values of A, respectively. 
   *            By examining these values, one can confirm that the rank 
   *            is well defined with respect to the chosen value of RCOND. 
   *            The ratio SVAL(1)/SVAL(2) is an estimate of the condition 
   *            number of R(1:RANK,1:RANK). 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension ( LDWORK ) 
   *            On exit, if  INFO = 0,  DWORK(1) returns the optimal value 
   *            of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= 3*N + 1,                 if JOBQR = 'Q'; 
   *            LDWORK >= Max( 1, 2*min( M, N ) ), if JOBQR = 'N'. 
   *            For good performance when JOBQR = 'Q', LDWORK should be 
   *            larger. Specifically, LDWORK >= 2*N + ( N + 1 )*NB, where 
   *            NB is the optimal block size for the LAPACK Library 
   *            routine DGEQP3. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value. 
   * 
   *    METHOD 
   * 
   *    The routine computes or uses a QR factorization with column 
   *    pivoting of A,  A * P = Q * R,  with  R  defined above, and then 
   *    finds the largest leading submatrix whose estimated condition 
   *    number is less than 1/RCOND, taking the possible positive value of 
   *    SVLMAX into account.  This is performed using the LAPACK 
   *    incremental condition estimation scheme and a slightly modified 
   *    rank decision test. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, Nov. 1996. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Mar. 2005. 
   * 
   *   ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
   *    .. 
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
   *    .. 
   *    .. Executable Statements .. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --jpvt;
  --tau;
  --sval;
  --dwork;

  /* Function Body */
  ljobqr = C2F (lsame) (jobqr, "Q", 1L, 1L);
  mn = Min (*m, *n);
  ismin = 1;
  ismax = mn + 1;
  if (ljobqr)
    {
      minwrk = *n * 3 + 1;
    }
  else
    {
      /*Computing MAX 
       */
      i__1 = 1, i__2 = mn << 1;
      minwrk = Max (i__1, i__2);
    }
  maxwrk = minwrk;
  /* 
   *    Test the input scalar arguments. 
   * 
   */
  *info = 0;
  if (!ljobqr && !C2F (lsame) (jobqr, "N", 1L, 1L))
    {
      *info = -1;
    }
  else if (*m < 0)
    {
      *info = -2;
    }
  else if (*n < 0)
    {
      *info = -3;
    }
  else if (*lda < Max (1, *m))
    {
      *info = -5;
    }
  else if (*rcond < 0.)
    {
      *info = -7;
    }
  else if (*svlmax < 0.)
    {
      *info = -8;
    }
  else if (*ldwork < minwrk)
    {
      *info = -13;
    }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("MB03OD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible 
   * 
   */
  if (mn == 0)
    {
      *rank = 0;
      sval[1] = 0.;
      sval[2] = 0.;
      sval[3] = 0.;
      dwork[1] = 1.;
      return 0;
    }
  /* 
   */
  if (ljobqr)
    {
      /* 
       *       Compute QR factorization with column pivoting of A: 
       *          A * P = Q * R 
       *       Workspace need   3*N + 1; 
       *                 prefer 2*N + (N+1)*NB. 
       *       Details of Householder rotations stored in TAU. 
       * 
       */
      C2F (dgeqp3) (m, n, &a[a_offset], lda, &jpvt[1], &tau[1], &dwork[1],
		    ldwork, info);
      /*Computing MAX 
       */
      i__1 = maxwrk, i__2 = (int) dwork[1];
      maxwrk = Max (i__1, i__2);
    }
  /* 
   *    Determine RANK using incremental condition estimation 
   * 
   */
  dwork[ismin] = 1.;
  dwork[ismax] = 1.;
  smax = (d__1 = a[a_dim1 + 1], Abs (d__1));
  smin = smax;
  if (smax == 0. || *svlmax * *rcond > smax)
    {
      *rank = 0;
      sval[1] = smax;
      sval[2] = 0.;
      sval[3] = 0.;
    }
  else
    {
      *rank = 1;
      sminpr = smin;
      /* 
       */
    L10:
      if (*rank < mn)
	{
	  i__ = *rank + 1;
	  C2F (dlaic1) (&c__2, rank, &dwork[ismin], &smin,
			&a[i__ * a_dim1 + 1], &a[i__ + i__ * a_dim1], &sminpr,
			&s1, &c1);
	  C2F (dlaic1) (&c__1, rank, &dwork[ismax], &smax,
			&a[i__ * a_dim1 + 1], &a[i__ + i__ * a_dim1], &smaxpr,
			&s2, &c2);
	  /* 
	   */
	  if (*svlmax * *rcond <= smaxpr)
	    {
	      if (*svlmax * *rcond <= sminpr)
		{
		  if (smaxpr * *rcond <= sminpr)
		    {
		      i__1 = *rank;
		      for (i__ = 1; i__ <= i__1; ++i__)
			{
			  dwork[ismin + i__ - 1] =
			    s1 * dwork[ismin + i__ - 1];
			  dwork[ismax + i__ - 1] =
			    s2 * dwork[ismax + i__ - 1];
			  /* L20: */
			}
		      dwork[ismin + *rank] = c1;
		      dwork[ismax + *rank] = c2;
		      smin = sminpr;
		      smax = smaxpr;
		      ++(*rank);
		      goto L10;
		    }
		}
	    }
	}
      sval[1] = smax;
      sval[2] = smin;
      sval[3] = sminpr;
    }
  /* 
   */
  dwork[1] = (double) maxwrk;
  return 0;
  /**** Last line of MB03OD *** 
   */
}				/* nsp_slicot_mb03od */
