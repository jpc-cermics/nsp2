/* MB02QY.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__0 = 0;
static double c_b15 = 0.;
static double c_b28 = 1.;
static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_mb02qy (int *m, int *n, int *nrhs, int *rank,
		   double *a, int *lda, int *jpvt, double *b,
		   int *ldb, double *tau, double *dwork, int *ldwork,
		   int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, i__1, i__2;

  /* Local variables */
  double anrm, bnrm;
  int i__, j, iascl, ibscl;
  int mn;
  double bignum;
  double maxwrk, smlnum;

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
   *    To determine the minimum-norm solution to a real linear least 
   *    squares problem: 
   * 
   *        minimize || A * X - B ||, 
   * 
   *    using the rank-revealing QR factorization of a real general 
   *    M-by-N matrix  A,  computed by SLICOT Library routine  MB03OD. 
   * 
   *    ARGUMENTS 
   * 
   *    Input/Output Parameters 
   * 
   *    M       (input) INT 
   *            The number of rows of the matrices A and B.  M >= 0. 
   * 
   *    N       (input) INT 
   *            The number of columns of the matrix A.  N >= 0. 
   * 
   *    NRHS    (input) INT 
   *            The number of columns of the matrix B.  NRHS >= 0. 
   * 
   *    RANK    (input) INT 
   *            The effective rank of  A,  as returned by SLICOT Library 
   *            routine  MB03OD.  Min(M,N) >= RANK >= 0. 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension 
   *            ( LDA, N ) 
   *            On entry, the leading Min(M,N)-by-N upper trapezoidal 
   *            part of this array contains the triangular factor  R,  as 
   *            returned by SLICOT Library routine  MB03OD.  The strict 
   *            lower trapezoidal part of  A  is not referenced. 
   *            On exit, if  RANK < N,  the leading  RANK-by-RANK  upper 
   *            triangular part of this array contains the upper 
   *            triangular matrix  R  of the complete orthogonal 
   *            factorization of  A,  and the submatrix  (1:RANK,RANK+1:N) 
   *            of this array, with the array  TAU,  represent the 
   *            orthogonal matrix  Z  (of the complete orthogonal 
   *            factorization of  A),  as a product of  RANK  elementary 
   *            reflectors. 
   *            On exit, if  RANK = N,  this array is unchanged. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= Max(1,M). 
   * 
   *    JPVT    (input) INT array, dimension ( N ) 
   *            The recorded permutations performed by SLICOT Library 
   *            routine  MB03OD;  if  JPVT(i) = k,  then the i-th column 
   *            of  A*P  was the k-th column of the original matrix  A. 
   * 
   *    B       (input/output) DOUBLE PRECISION array, dimension 
   *            ( LDB, NRHS ) 
   *            On entry, if  NRHS > 0,  the leading M-by-NRHS part of 
   *            this array must contain the matrix  B  (corresponding to 
   *            the transformed matrix  A,  returned by SLICOT Library 
   *            routine  MB03OD). 
   *            On exit, if  NRHS > 0,  the leading N-by-NRHS part of this 
   *            array contains the solution matrix X. 
   *            If  M >= N  and  RANK = N,  the residual sum-of-squares 
   *            for the solution in the i-th column is given by the sum 
   *            of squares of elements  N+1:M  in that column. 
   *            If  NRHS = 0,  the array  B  is not referenced. 
   * 
   *    LDB     INT 
   *            The leading dimension of the array B. 
   *            LDB >= Max(1,M,N),  if  NRHS > 0. 
   *            LDB >= 1,           if  NRHS = 0. 
   * 
   *    TAU     (output) DOUBLE PRECISION array, dimension ( Min(M,N) ) 
   *            The scalar factors of the elementary reflectors. 
   *            If  RANK = N,  the array  TAU  is not referenced. 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension ( LDWORK ) 
   *            On exit, if  INFO = 0,  DWORK(1) returns the optimal value 
   *            of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= Max( 1, N, NRHS ). 
   *            For good performance,  LDWORK  should sometimes be larger. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value. 
   * 
   *    METHOD 
   * 
   *    The routine uses a QR factorization with column pivoting: 
   * 
   *       A * P = Q * R = Q * [ R11 R12 ], 
   *                           [  0  R22 ] 
   * 
   *    where  R11  is an upper triangular submatrix of estimated rank 
   *    RANK,  the effective rank of  A.  The submatrix  R22  can be 
   *    considered as negligible. 
   * 
   *    If  RANK < N,  then  R12  is annihilated by orthogonal 
   *    transformations from the right, arriving at the complete 
   *    orthogonal factorization: 
   * 
   *       A * P = Q * [ T11 0 ] * Z. 
   *                   [  0  0 ] 
   * 
   *    The minimum-norm solution is then 
   * 
   *       X = P * Z' [ inv(T11)*Q1'*B ], 
   *                  [        0       ] 
   * 
   *    where Q1 consists of the first  RANK  columns of Q. 
   * 
   *    The input data for  MB02QY  are the transformed matrices  Q' * A 
   *    (returned by SLICOT Library routine  MB03OD)  and  Q' * B. 
   *    Matrix  Q  is not needed. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The implemented method is numerically stable. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Aug. 1999. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Least squares solutions; QR decomposition. 
   * 
   *   ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
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
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  --tau;
  --dwork;

  /* Function Body */
  mn = Min (*m, *n);
  /* 
   *    Test the input scalar arguments. 
   * 
   */
  *info = 0;
  if (*m < 0)
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*nrhs < 0)
    {
      *info = -3;
    }
  else if (*rank < 0 || *rank > mn)
    {
      *info = -4;
    }
  else if (*lda < Max (1, *m))
    {
      *info = -6;
    }
  else if (*ldb < 1 || *nrhs > 0 && *ldb < Max (*m, *n))
    {
      *info = -9;
    }
  else				/* if(complicated condition) */
    {
      /*Computing MAX 
       */
      i__1 = Max (1, *n);
      if (*ldwork < Max (i__1, *nrhs))
	{
	  *info = -12;
	}
    }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("MB02QY", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (Min (mn, *nrhs) == 0)
    {
      dwork[1] = 1.;
      return 0;
    }
  /* 
   *    Intly partition R = [ R11 R12 ], 
   *                            [  0  R22 ] 
   * 
   *    where R11 = R(1:RANK,1:RANK).  If  RANK = N,  let  T11 = R11. 
   * 
   */
  maxwrk = (double) (*n);
  if (*rank < *n)
    {
      /* 
       *       Get machine parameters. 
       * 
       */
      smlnum =
	C2F (dlamch) ("Safe minimum", 12L) / C2F (dlamch) ("Precision", 9L);
      bignum = 1. / smlnum;
      C2F (dlabad) (&smlnum, &bignum);
      /* 
       *       Scale A, B if max entries outside range [SMLNUM,BIGNUM]. 
       * 
       */
      anrm =
	C2F (dlantr) ("MaxNorm", "Upper", "Non-unit", rank, n, &a[a_offset],
		      lda, &dwork[1], 7L, 5L, 8L);
      iascl = 0;
      if (anrm > 0. && anrm < smlnum)
	{
	  /* 
	   *          Scale matrix norm up to SMLNUM. 
	   * 
	   */
	  C2F (dlascl) ("Upper", &c__0, &c__0, &anrm, &smlnum, rank, n,
			&a[a_offset], lda, info, 5L);
	  iascl = 1;
	}
      else if (anrm > bignum)
	{
	  /* 
	   *          Scale matrix norm down to BIGNUM. 
	   * 
	   */
	  C2F (dlascl) ("Upper", &c__0, &c__0, &anrm, &bignum, rank, n,
			&a[a_offset], lda, info, 5L);
	  iascl = 2;
	}
      else if (anrm == 0.)
	{
	  /* 
	   *          Matrix all zero. Return zero solution. 
	   * 
	   */
	  C2F (dlaset) ("Full", n, nrhs, &c_b15, &c_b15, &b[b_offset], ldb,
			4L);
	  dwork[1] = 1.;
	  return 0;
	}
      /* 
       */
      bnrm =
	C2F (dlange) ("MaxNorm", m, nrhs, &b[b_offset], ldb, &dwork[1], 7L);
      ibscl = 0;
      if (bnrm > 0. && bnrm < smlnum)
	{
	  /* 
	   *          Scale matrix norm up to SMLNUM. 
	   * 
	   */
	  C2F (dlascl) ("General", &c__0, &c__0, &bnrm, &smlnum, m, nrhs,
			&b[b_offset], ldb, info, 7L);
	  ibscl = 1;
	}
      else if (bnrm > bignum)
	{
	  /* 
	   *          Scale matrix norm down to BIGNUM. 
	   * 
	   */
	  C2F (dlascl) ("General", &c__0, &c__0, &bnrm, &bignum, m, nrhs,
			&b[b_offset], ldb, info, 7L);
	  ibscl = 2;
	}
      /* 
       *       [R11,R12] = [ T11, 0 ] * Z. 
       *       Details of Householder rotations are stored in TAU. 
       *       Workspace need RANK, prefer RANK*NB. 
       * 
       */
      C2F (dtzrzf) (rank, n, &a[a_offset], lda, &tau[1], &dwork[1], ldwork,
		    info);
      maxwrk = Max (maxwrk, dwork[1]);
    }
  /* 
   *    B(1:RANK,1:NRHS) := inv(T11) * B(1:RANK,1:NRHS). 
   * 
   */
  C2F (dtrsm) ("Left", "Upper", "No transpose", "Non-unit", rank, nrhs,
	       &c_b28, &a[a_offset], lda, &b[b_offset], ldb, 4L, 5L, 12L, 8L);
  /* 
   */
  if (*rank < *n)
    {
      /* 
       */
      i__1 = *n - *rank;
      C2F (dlaset) ("Full", &i__1, nrhs, &c_b15, &c_b15,
		    &b[*rank + 1 + b_dim1], ldb, 4L);
      /* 
       *       B(1:N,1:NRHS) := Z' * B(1:N,1:NRHS). 
       *       Workspace need NRHS, prefer NRHS*NB. 
       * 
       */
      i__1 = *n - *rank;
      C2F (dormrz) ("Left", "Transpose", n, nrhs, rank, &i__1, &a[a_offset],
		    lda, &tau[1], &b[b_offset], ldb, &dwork[1], ldwork, info,
		    4L, 9L);
      maxwrk = Max (maxwrk, dwork[1]);
      /* 
       *       Undo scaling. 
       * 
       */
      if (iascl == 1)
	{
	  C2F (dlascl) ("General", &c__0, &c__0, &anrm, &smlnum, n, nrhs,
			&b[b_offset], ldb, info, 7L);
	  C2F (dlascl) ("Upper", &c__0, &c__0, &smlnum, &anrm, rank, rank,
			&a[a_offset], lda, info, 5L);
	}
      else if (iascl == 2)
	{
	  C2F (dlascl) ("General", &c__0, &c__0, &anrm, &bignum, n, nrhs,
			&b[b_offset], ldb, info, 7L);
	  C2F (dlascl) ("Upper", &c__0, &c__0, &bignum, &anrm, rank, rank,
			&a[a_offset], lda, info, 5L);
	}
      if (ibscl == 1)
	{
	  C2F (dlascl) ("General", &c__0, &c__0, &smlnum, &bnrm, n, nrhs,
			&b[b_offset], ldb, info, 7L);
	}
      else if (ibscl == 2)
	{
	  C2F (dlascl) ("General", &c__0, &c__0, &bignum, &bnrm, n, nrhs,
			&b[b_offset], ldb, info, 7L);
	}
    }
  /* 
   *    B(1:N,1:NRHS) := P * B(1:N,1:NRHS). 
   *    Workspace N. 
   * 
   */
  i__1 = *nrhs;
  for (j = 1; j <= i__1; ++j)
    {
      /* 
       */
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  dwork[jpvt[i__]] = b[i__ + j * b_dim1];
	  /* L10: */
	}
      /* 
       */
      C2F (dcopy) (n, &dwork[1], &c__1, &b[j * b_dim1 + 1], &c__1);
      /* L20: */
    }
  /* 
   */
  dwork[1] = maxwrk;
  return 0;
  /* 
**** Last line of MB02QY *** 
*/
}				/* nsp_slicot_mb02qy */
