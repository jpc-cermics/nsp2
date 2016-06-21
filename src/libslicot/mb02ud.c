/* MB02UD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static int c_n1 = -1;
static double c_b35 = 1.;
static double c_b36 = 0.;

/* Subroutine */ int
nsp_slicot_mb02ud (char *fact, char *side, char *trans, char *jobp, int *m,
		   int *n, double *alpha, double *rcond, int *rank,
		   double *r__, int *ldr, double *q, int *ldq,
		   double *sv, double *b, int *ldb, double *rp,
		   int *ldrp, double *dwork, int *ldwork, int *info,
		   long int fact_len, long int side_len, long int trans_len,
		   long int jobp_len)
{
  /* System generated locals */
  int b_dim1, b_offset, q_dim1, q_offset, r_dim1, r_offset, rp_dim1,
    rp_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int left, nfct, tran;
  double toll;
  int pinv;
  int i__, l;
  char ntran[1];
  int mn;
  int minwrk, maxwrk;

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
   *    To compute the minimum norm least squares solution of one of the 
   *    following linear systems 
   * 
   *       op(R)*X = alpha*B,                                          (1) 
   *       X*op(R) = alpha*B,                                          (2) 
   * 
   *    where alpha is a real scalar, op(R) is either R or its transpose, 
   *    R', R is an L-by-L real upper triangular matrix, B is an M-by-N 
   *    real matrix, and L = M for (1), or L = N for (2). Singular value 
   *    decomposition, R = Q*S*P', is used, assuming that R is rank 
   *    deficient. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    FACT    CHARACTER*1 
   *            Specifies whether R has been previously factored or not, 
   *            as follows: 
   *            = 'F':  R has been factored and its rank and singular 
   *                    value decomposition, R = Q*S*P', are available; 
   *            = 'N':  R has not been factored and its singular value 
   *                    decomposition, R = Q*S*P', should be computed. 
   * 
   *    SIDE    CHARACTER*1 
   *            Specifies whether op(R) appears on the left or right 
   *            of X as follows: 
   *            = 'L':  Solve op(R)*X = alpha*B  (op(R) is on the left); 
   *            = 'R':  Solve X*op(R) = alpha*B  (op(R) is on the right). 
   * 
   *    TRANS   CHARACTER*1 
   *            Specifies the form of op(R) to be used as follows: 
   *            = 'N':  op(R) = R; 
   *            = 'T':  op(R) = R'; 
   *            = 'C':  op(R) = R'. 
   * 
   *    JOBP    CHARACTER*1 
   *            Specifies whether or not the pseudoinverse of R is to be 
   *            computed or it is available as follows: 
   *            = 'P':  Compute pinv(R), if FACT = 'N', or 
   *                    use pinv(R),     if FACT = 'F'; 
   *            = 'N':  Do not compute or use pinv(R). 
   * 
   *    Input/Output Parameters 
   * 
   *    M       (input) INT 
   *            The number of rows of the matrix B.  M >= 0. 
   * 
   *    N       (input) INT 
   *            The number of columns of the matrix B.  N >= 0. 
   * 
   *    ALPHA   (input) DOUBLE PRECISION 
   *            The scalar alpha. When alpha is zero then B need not be 
   *            set before entry. 
   * 
   *    RCOND   (input) DOUBLE PRECISION 
   *            RCOND is used to determine the effective rank of R. 
   *            Singular values of R satisfying Sv(i) <= RCOND*Sv(1) are 
   *            treated as zero. If RCOND <= 0, then EPS is used instead, 
   *            where EPS is the relative machine precision (see LAPACK 
   *            Library routine DLAMCH).  RCOND <= 1. 
   *            RCOND is not used if FACT = 'F'. 
   * 
   *    RANK    (input or output) INT 
   *            The rank of matrix R. 
   *            RANK is an input parameter when FACT = 'F', and an output 
   *            parameter when FACT = 'N'.  L >= RANK >= 0. 
   * 
   *    R       (input/output) DOUBLE PRECISION array, dimension (LDR,L) 
   *            On entry, if FACT = 'F', the leading L-by-L part of this 
   *            array must contain the L-by-L orthogonal matrix P' from 
   *            singular value decomposition, R = Q*S*P', of the matrix R; 
   *            if JOBP = 'P', the first RANK rows of P' are assumed to be 
   *            scaled by inv(S(1:RANK,1:RANK)). 
   *            On entry, if FACT = 'N', the leading L-by-L upper 
   *            triangular part of this array must contain the upper 
   *            triangular matrix R. 
   *            On exit, if INFO = 0, the leading L-by-L part of this 
   *            array contains the L-by-L orthogonal matrix P', with its 
   *            first RANK rows scaled by inv(S(1:RANK,1:RANK)), when 
   *            JOBP = 'P'. 
   * 
   *    LDR     INT 
   *            The leading dimension of array R.  LDR >= MAX(1,L). 
   * 
   *    Q       (input or output) DOUBLE PRECISION array, dimension 
   *            (LDQ,L) 
   *            On entry, if FACT = 'F', the leading L-by-L part of this 
   *            array must contain the L-by-L orthogonal matrix Q from 
   *            singular value decomposition, R = Q*S*P', of the matrix R. 
   *            If FACT = 'N', this array need not be set on entry, and 
   *            on exit, if INFO = 0, the leading L-by-L part of this 
   *            array contains the orthogonal matrix Q. 
   * 
   *    LDQ     INT 
   *            The leading dimension of array Q.  LDQ >= MAX(1,L). 
   * 
   *    SV      (input or output) DOUBLE PRECISION array, dimension (L) 
   *            On entry, if FACT = 'F', the first RANK entries of this 
   *            array must contain the reciprocal of the largest RANK 
   *            singular values of the matrix R, and the last L-RANK 
   *            entries of this array must contain the remaining singular 
   *            values of R sorted in descending order. 
   *            If FACT = 'N', this array need not be set on input, and 
   *            on exit, if INFO = 0, the first RANK entries of this array 
   *            contain the reciprocal of the largest RANK singular values 
   *            of the matrix R, and the last L-RANK entries of this array 
   *            contain the remaining singular values of R sorted in 
   *            descending order. 
   * 
   *    B       (input/output) DOUBLE PRECISION array, dimension (LDB,N) 
   *            On entry, if ALPHA <> 0, the leading M-by-N part of this 
   *            array must contain the matrix B. 
   *            On exit, if INFO = 0 and RANK > 0, the leading M-by-N part 
   *            of this array contains the M-by-N solution matrix X. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,M). 
   * 
   *    RP      (input or output) DOUBLE PRECISION array, dimension 
   *            (LDRP,L) 
   *            On entry, if FACT = 'F', JOBP = 'P', and RANK > 0, the 
   *            leading L-by-L part of this array must contain the L-by-L 
   *            matrix pinv(R), the Moore-Penrose pseudoinverse of R. 
   *            On exit, if FACT = 'N', JOBP = 'P', and RANK > 0, the 
   *            leading L-by-L part of this array contains the L-by-L 
   *            matrix pinv(R), the Moore-Penrose pseudoinverse of R. 
   *            If JOBP = 'N', this array is not referenced. 
   * 
   *    LDRP    INT 
   *            The leading dimension of array RP. 
   *            LDRP >= MAX(1,L), if JOBP = 'P'. 
   *            LDRP >= 1,        if JOBP = 'N'. 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) returns the optimal LDWORK; 
   *            if INFO = i, 1 <= i <= L, then DWORK(2:L) contain the 
   *            unconverged superdiagonal elements of an upper bidiagonal 
   *            matrix D whose diagonal is in SV (not necessarily sorted). 
   *            D satisfies R = Q*D*P', so it has the same singular 
   *            values as R, and singular vectors related by Q and P'. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= MAX(1,L),   if FACT = 'F'; 
   *            LDWORK >= MAX(1,5*L), if FACT = 'N'. 
   *            For optimum performance LDWORK should be larger than 
   *            MAX(1,L,M*N),   if FACT = 'F'; 
   *            MAX(1,5*L,M*N), if FACT = 'N'. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            > 0:  if INFO = i, i = 1:L, the SVD algorithm has failed 
   *                  to converge. In this case INFO specifies how many 
   *                  superdiagonals did not converge (see the description 
   *                  of DWORK); this failure is not likely to occur. 
   * 
   *    METHOD 
   * 
   *    The L-by-L upper triangular matrix R is factored as  R = Q*S*P', 
   *    if FACT = 'N', using SLICOT Library routine MB03UD, where Q and P 
   *    are L-by-L orthogonal matrices and S is an L-by-L diagonal matrix 
   *    with non-negative diagonal elements, SV(1), SV(2), ..., SV(L), 
   *    ordered decreasingly. Then, the effective rank of R is estimated, 
   *    and matrix (or matrix-vector) products and scalings are used to 
   *    compute X. If FACT = 'F', only matrix (or matrix-vector) products 
   *    and scalings are performed. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    Option JOBP = 'P' should be used only if the pseudoinverse is 
   *    really needed. Usually, it is possible to avoid the use of 
   *    pseudoinverse, by computing least squares solutions. 
   *    The routine uses BLAS 3 calculations if LDWORK >= M*N, and BLAS 2 
   *    calculations, otherwise. No advantage of any additional workspace 
   *    larger than L is taken for matrix products, but the routine can 
   *    be called repeatedly for chunks of columns of B, if LDWORK < M*N. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute of Informatics, Bucharest, Oct. 1999. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Feb. 2000. 
   * 
   *    KEYWORDS 
   * 
   *    Bidiagonalization, orthogonal transformation, singular value 
   *    decomposition, singular values, triangular form. 
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
   *    .. Executable Statements .. 
   * 
   *    Check the input scalar arguments. 
   * 
   */
  /* Parameter adjustments */
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  --sv;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  rp_dim1 = *ldrp;
  rp_offset = rp_dim1 + 1;
  rp -= rp_offset;
  --dwork;

  /* Function Body */
  *info = 0;
  nfct = C2F (lsame) (fact, "N", 1L, 1L);
  left = C2F (lsame) (side, "L", 1L, 1L);
  pinv = C2F (lsame) (jobp, "P", 1L, 1L);
  tran = C2F (lsame) (trans, "T", 1L, 1L) || C2F (lsame) (trans, "C", 1L, 1L);
  if (left)
    {
      l = *m;
    }
  else
    {
      l = *n;
    }
  mn = *m * *n;
  if (!nfct && !C2F (lsame) (fact, "F", 1L, 1L))
    {
      *info = -1;
    }
  else if (!left && !C2F (lsame) (side, "R", 1L, 1L))
    {
      *info = -2;
    }
  else if (!tran && !C2F (lsame) (trans, "N", 1L, 1L))
    {
      *info = -3;
    }
  else if (!pinv && !C2F (lsame) (jobp, "N", 1L, 1L))
    {
      *info = -4;
    }
  else if (*m < 0)
    {
      *info = -5;
    }
  else if (*n < 0)
    {
      *info = -6;
    }
  else if (nfct && *rcond > 1.)
    {
      *info = -8;
    }
  else if (!nfct && ((double) (*rank) < 0. || *rank > l))
    {
      *info = -9;
    }
  else if (*ldr < Max (1, l))
    {
      *info = -11;
    }
  else if (*ldq < Max (1, l))
    {
      *info = -13;
    }
  else if (*ldb < Max (1, *m))
    {
      *info = -16;
    }
  else if (*ldrp < 1 || pinv && *ldrp < l)
    {
      *info = -18;
    }
  /* 
   *    Compute workspace 
   *    (Note: Comments in the code beginning "Workspace:" describe the 
   *    minimal amount of workspace needed at that point in the code, 
   *    as well as the preferred amount for good performance. 
   *    NB refers to the optimal block size for the immediately following 
   *    subroutine, as returned by ILAENV.) 
   * 
   */
  minwrk = 1;
  if (*info == 0 && *ldwork >= 1 && l > 0)
    {
      minwrk = Max (1, l);
      maxwrk = Max (minwrk, mn);
      if (nfct)
	{
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 =
	    l * 3 + (l << 1) * C2F (ilaenv) (&c__1, "DGEBRD", " ", &l, &l,
					     &c_n1, &c_n1, 6L, 1L);
	  maxwrk = Max (i__1, i__2);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 =
	    l * 3 + l * C2F (ilaenv) (&c__1, "DORGBR", "Q", &l, &l, &l, &c_n1,
				      6L, 1L);
	  maxwrk = Max (i__1, i__2);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 =
	    l * 3 + l * C2F (ilaenv) (&c__1, "DORGBR", "P", &l, &l, &l, &c_n1,
				      6L, 1L);
	  maxwrk = Max (i__1, i__2);
	  /*Computing MAX 
	   */
	  i__1 = 1, i__2 = l * 5;
	  minwrk = Max (i__1, i__2);
	  maxwrk = Max (maxwrk, minwrk);
	}
    }
  /* 
   */
  if (*ldwork < minwrk)
    {
      *info = -20;
    }
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("MB02UD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (l == 0)
    {
      if (nfct)
	{
	  *rank = 0;
	}
      dwork[1] = 1.;
      return 0;
    }
  /* 
   */
  if (nfct)
    {
      /* 
       *       Compute the SVD of R, R = Q*S*P'. 
       *       Matrix Q is computed in the array Q, and P' overwrites R. 
       *       Workspace: need   5*L; 
       *                  prefer larger. 
       * 
       */
      nsp_slicot_mb03ud ("Vectors", "Vectors", &l, &r__[r_offset], ldr,
			 &q[q_offset], ldq, &sv[1], &dwork[1], ldwork, info,
			 7L, 7L);
      if (*info != 0)
	{
	  return 0;
	}
      /* 
       *       Use the default tolerance, if required. 
       * 
       */
      toll = *rcond;
      if (toll <= 0.)
	{
	  toll = C2F (dlamch) ("Precision", 9L);
	}
      /*Computing MAX 
       */
      d__1 = toll * sv[1], d__2 = C2F (dlamch) ("Safe minimum", 12L);
      toll = Max (d__1, d__2);
      /* 
       *       Estimate the rank of R. 
       * 
       */
      i__1 = l;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (toll > sv[i__])
	    {
	      goto L20;
	    }
	  /* L10: */
	}
      /* 
       */
      i__ = l + 1;
    L20:
      *rank = i__ - 1;
      /* 
       */
      i__1 = *rank;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  sv[i__] = 1. / sv[i__];
	  /* L30: */
	}
      /* 
       */
      if (pinv && *rank > 0)
	{
	  /* 
	   *          Compute  pinv(S)'*P'  in R. 
	   * 
	   */
	  nsp_slicot_mb01sd ("Row scaling", rank, &l, &r__[r_offset], ldr,
			     &sv[1], &sv[1], 11L);
	  /* 
	   *          Compute  pinv(R) = P*pinv(S)*Q'  in  RP. 
	   * 
	   */
	  C2F (dgemm) ("Transpose", "Transpose", &l, &l, rank, &c_b35,
		       &r__[r_offset], ldr, &q[q_offset], ldq, &c_b36,
		       &rp[rp_offset], ldrp, 9L, 9L);
	}
    }
  /* 
   *    Return if Min(M,N) = 0 or RANK = 0. 
   * 
   */
  if (Min (*m, *n) == 0 || *rank == 0)
    {
      dwork[1] = (double) maxwrk;
      return 0;
    }
  /* 
   *    Set X = 0 if alpha = 0. 
   * 
   */
  if (*alpha == 0.)
    {
      C2F (dlaset) ("Full", m, n, &c_b36, &c_b36, &b[b_offset], ldb, 4L);
      dwork[1] = (double) maxwrk;
      return 0;
    }
  /* 
   */
  if (pinv)
    {
      /* 
       */
      if (left)
	{
	  /* 
	   *          Compute  alpha*op(pinv(R))*B  in workspace and save it in B. 
	   *          Workspace:  need   M   (BLAS 2); 
	   *                      prefer M*N (BLAS 3). 
	   * 
	   */
	  if (*ldwork >= mn)
	    {
	      C2F (dgemm) (trans, "NoTranspose", m, n, m, alpha,
			   &rp[rp_offset], ldrp, &b[b_offset], ldb, &c_b36,
			   &dwork[1], m, 1L, 11L);
	      C2F (dlacpy) ("Full", m, n, &dwork[1], m, &b[b_offset], ldb,
			    4L);
	    }
	  else
	    {
	      /* 
	       */
	      i__1 = *n;
	      for (i__ = 1; i__ <= i__1; ++i__)
		{
		  C2F (dgemv) (trans, m, m, alpha, &rp[rp_offset], ldrp,
			       &b[i__ * b_dim1 + 1], &c__1, &c_b36, &dwork[1],
			       &c__1, 1L);
		  C2F (dcopy) (m, &dwork[1], &c__1, &b[i__ * b_dim1 + 1],
			       &c__1);
		  /* L40: */
		}
	      /* 
	       */
	    }
	}
      else
	{
	  /* 
	   *          Compute  alpha*B*op(pinv(R))  in workspace and save it in B. 
	   *          Workspace:  need   N   (BLAS 2); 
	   *                      prefer M*N (BLAS 3). 
	   * 
	   */
	  if (*ldwork >= mn)
	    {
	      C2F (dgemm) ("NoTranspose", trans, m, n, n, alpha, &b[b_offset],
			   ldb, &rp[rp_offset], ldrp, &c_b36, &dwork[1], m,
			   11L, 1L);
	      C2F (dlacpy) ("Full", m, n, &dwork[1], m, &b[b_offset], ldb,
			    4L);
	    }
	  else
	    {
	      /* 
	       */
	      if (tran)
		{
		  *(unsigned char *) ntran = 'N';
		}
	      else
		{
		  *(unsigned char *) ntran = 'T';
		}
	      /* 
	       */
	      i__1 = *m;
	      for (i__ = 1; i__ <= i__1; ++i__)
		{
		  C2F (dgemv) (ntran, n, n, alpha, &rp[rp_offset], ldrp,
			       &b[i__ + b_dim1], ldb, &c_b36, &dwork[1],
			       &c__1, 1L);
		  C2F (dcopy) (n, &dwork[1], &c__1, &b[i__ + b_dim1], ldb);
		  /* L50: */
		}
	      /* 
	       */
	    }
	}
      /* 
       */
    }
  else
    {
      /* 
       */
      if (left)
	{
	  /* 
	   *          Compute  alpha*P*pinv(S)*Q'*B  or  alpha*Q*pinv(S)'*P'*B. 
	   *          Workspace:  need   M   (BLAS 2); 
	   *                      prefer M*N (BLAS 3). 
	   * 
	   */
	  if (*ldwork >= mn)
	    {
	      if (tran)
		{
		  /* 
		   *                Compute  alpha*P'*B  in workspace. 
		   * 
		   */
		  C2F (dgemm) ("NoTranspose", "NoTranspose", m, n, m, alpha,
			       &r__[r_offset], ldr, &b[b_offset], ldb, &c_b36,
			       &dwork[1], m, 11L, 11L);
		  /* 
		   *                Compute  alpha*pinv(S)'*P'*B. 
		   * 
		   */
		  nsp_slicot_mb01sd ("Row scaling", rank, n, &dwork[1], m,
				     &sv[1], &sv[1], 11L);
		  /* 
		   *                Compute  alpha*Q*pinv(S)'*P'*B. 
		   * 
		   */
		  C2F (dgemm) ("NoTranspose", "NoTranspose", m, n, rank,
			       &c_b35, &q[q_offset], ldq, &dwork[1], m,
			       &c_b36, &b[b_offset], ldb, 11L, 11L);
		}
	      else
		{
		  /* 
		   *                Compute  alpha*Q'*B  in workspace. 
		   * 
		   */
		  C2F (dgemm) ("Transpose", "NoTranspose", m, n, m, alpha,
			       &q[q_offset], ldq, &b[b_offset], ldb, &c_b36,
			       &dwork[1], m, 9L, 11L);
		  /* 
		   *                Compute  alpha*pinv(S)*Q'*B. 
		   * 
		   */
		  nsp_slicot_mb01sd ("Row scaling", rank, n, &dwork[1], m,
				     &sv[1], &sv[1], 11L);
		  /* 
		   *                Compute  alpha*P*pinv(S)*Q'*B. 
		   * 
		   */
		  C2F (dgemm) ("Transpose", "NoTranspose", m, n, rank, &c_b35,
			       &r__[r_offset], ldr, &dwork[1], m, &c_b36,
			       &b[b_offset], ldb, 9L, 11L);
		}
	    }
	  else
	    {
	      if (tran)
		{
		  /* 
		   *                Compute  alpha*P'*B  in B using workspace. 
		   * 
		   */
		  i__1 = *n;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      C2F (dgemv) ("NoTranspose", m, m, alpha, &r__[r_offset],
				   ldr, &b[i__ * b_dim1 + 1], &c__1, &c_b36,
				   &dwork[1], &c__1, 11L);
		      C2F (dcopy) (m, &dwork[1], &c__1, &b[i__ * b_dim1 + 1],
				   &c__1);
		      /* L60: */
		    }
		  /* 
		   *                Compute  alpha*pinv(S)'*P'*B. 
		   * 
		   */
		  nsp_slicot_mb01sd ("Row scaling", rank, n, &b[b_offset],
				     ldb, &sv[1], &sv[1], 11L);
		  /* 
		   *                Compute  alpha*Q*pinv(S)'*P'*B  in B using workspace. 
		   * 
		   */
		  i__1 = *n;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      C2F (dgemv) ("NoTranspose", m, rank, &c_b35,
				   &q[q_offset], ldq, &b[i__ * b_dim1 + 1],
				   &c__1, &c_b36, &dwork[1], &c__1, 11L);
		      C2F (dcopy) (m, &dwork[1], &c__1, &b[i__ * b_dim1 + 1],
				   &c__1);
		      /* L70: */
		    }
		}
	      else
		{
		  /* 
		   *                Compute  alpha*Q'*B  in B using workspace. 
		   * 
		   */
		  i__1 = *n;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      C2F (dgemv) ("Transpose", m, m, alpha, &q[q_offset],
				   ldq, &b[i__ * b_dim1 + 1], &c__1, &c_b36,
				   &dwork[1], &c__1, 9L);
		      C2F (dcopy) (m, &dwork[1], &c__1, &b[i__ * b_dim1 + 1],
				   &c__1);
		      /* L80: */
		    }
		  /* 
		   *                Compute  alpha*pinv(S)*Q'*B. 
		   * 
		   */
		  nsp_slicot_mb01sd ("Row scaling", rank, n, &b[b_offset],
				     ldb, &sv[1], &sv[1], 11L);
		  /* 
		   *                Compute  alpha*P*pinv(S)*Q'*B  in B using workspace. 
		   * 
		   */
		  i__1 = *n;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      C2F (dgemv) ("Transpose", rank, m, &c_b35,
				   &r__[r_offset], ldr, &b[i__ * b_dim1 + 1],
				   &c__1, &c_b36, &dwork[1], &c__1, 9L);
		      C2F (dcopy) (m, &dwork[1], &c__1, &b[i__ * b_dim1 + 1],
				   &c__1);
		      /* L90: */
		    }
		}
	    }
	}
      else
	{
	  /* 
	   *          Compute  alpha*B*P*pinv(S)*Q'  or  alpha*B*Q*pinv(S)'*P'. 
	   *          Workspace:  need   N   (BLAS 2); 
	   *                      prefer M*N (BLAS 3). 
	   * 
	   */
	  if (*ldwork >= mn)
	    {
	      if (tran)
		{
		  /* 
		   *                Compute  alpha*B*Q  in workspace. 
		   * 
		   */
		  C2F (dgemm) ("NoTranspose", "NoTranspose", m, n, n, alpha,
			       &b[b_offset], ldb, &q[q_offset], ldq, &c_b36,
			       &dwork[1], m, 11L, 11L);
		  /* 
		   *                Compute  alpha*B*Q*pinv(S)'. 
		   * 
		   */
		  nsp_slicot_mb01sd ("Column scaling", m, rank, &dwork[1], m,
				     &sv[1], &sv[1], 14L);
		  /* 
		   *                Compute  alpha*B*Q*pinv(S)'*P' in B. 
		   * 
		   */
		  C2F (dgemm) ("NoTranspose", "NoTranspose", m, n, rank,
			       &c_b35, &dwork[1], m, &r__[r_offset], ldr,
			       &c_b36, &b[b_offset], ldb, 11L, 11L);
		}
	      else
		{
		  /* 
		   *                Compute  alpha*B*P  in workspace. 
		   * 
		   */
		  C2F (dgemm) ("NoTranspose", "Transpose", m, n, n, alpha,
			       &b[b_offset], ldb, &r__[r_offset], ldr, &c_b36,
			       &dwork[1], m, 11L, 9L);
		  /* 
		   *                Compute  alpha*B*P*pinv(S). 
		   * 
		   */
		  nsp_slicot_mb01sd ("Column scaling", m, rank, &dwork[1], m,
				     &sv[1], &sv[1], 14L);
		  /* 
		   *                Compute  alpha*B*P*pinv(S)*Q' in B. 
		   * 
		   */
		  C2F (dgemm) ("NoTranspose", "Transpose", m, n, rank, &c_b35,
			       &dwork[1], m, &q[q_offset], ldq, &c_b36,
			       &b[b_offset], ldb, 11L, 9L);
		}
	    }
	  else
	    {
	      if (tran)
		{
		  /* 
		   *                Compute  alpha*B*Q  in B using workspace. 
		   * 
		   */
		  i__1 = *m;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      C2F (dgemv) ("Transpose", n, n, alpha, &q[q_offset],
				   ldq, &b[i__ + b_dim1], ldb, &c_b36,
				   &dwork[1], &c__1, 9L);
		      C2F (dcopy) (n, &dwork[1], &c__1, &b[i__ + b_dim1],
				   ldb);
		      /* L100: */
		    }
		  /* 
		   *                Compute  alpha*B*Q*pinv(S)'. 
		   * 
		   */
		  nsp_slicot_mb01sd ("Column scaling", m, rank, &b[b_offset],
				     ldb, &sv[1], &sv[1], 14L);
		  /* 
		   *                Compute  alpha*B*Q*pinv(S)'*P' in B using workspace. 
		   * 
		   */
		  i__1 = *m;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      C2F (dgemv) ("Transpose", rank, n, &c_b35,
				   &r__[r_offset], ldr, &b[i__ + b_dim1], ldb,
				   &c_b36, &dwork[1], &c__1, 9L);
		      C2F (dcopy) (n, &dwork[1], &c__1, &b[i__ + b_dim1],
				   ldb);
		      /* L110: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Compute  alpha*B*P  in B using workspace. 
		   * 
		   */
		  i__1 = *m;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      C2F (dgemv) ("NoTranspose", n, n, alpha, &r__[r_offset],
				   ldr, &b[i__ + b_dim1], ldb, &c_b36,
				   &dwork[1], &c__1, 11L);
		      C2F (dcopy) (n, &dwork[1], &c__1, &b[i__ + b_dim1],
				   ldb);
		      /* L120: */
		    }
		  /* 
		   *                Compute  alpha*B*P*pinv(S). 
		   * 
		   */
		  nsp_slicot_mb01sd ("Column scaling", m, rank, &b[b_offset],
				     ldb, &sv[1], &sv[1], 14L);
		  /* 
		   *                Compute  alpha*B*P*pinv(S)*Q' in B using workspace. 
		   * 
		   */
		  i__1 = *m;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      C2F (dgemv) ("NoTranspose", n, rank, &c_b35,
				   &q[q_offset], ldq, &b[i__ + b_dim1], ldb,
				   &c_b36, &dwork[1], &c__1, 11L);
		      C2F (dcopy) (n, &dwork[1], &c__1, &b[i__ + b_dim1],
				   ldb);
		      /* L130: */
		    }
		}
	    }
	}
    }
  /* 
   *    Return optimal workspace in DWORK(1). 
   * 
   */
  dwork[1] = (double) maxwrk;
  /* 
   */
  return 0;
  /**** Last line of MB02UD *** 
   */
}				/* nsp_slicot_mb02ud */
