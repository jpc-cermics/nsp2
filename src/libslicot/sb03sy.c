/* SB03SY.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b21 = 0.;
static double c_b22 = 1.;
static double c_b23 = .5;

/* Subroutine */ int
nsp_slicot_sb03sy (char *job, char *trana, char *lyapun, int *n, double *t,
		   int *ldt, double *u, int *ldu, double *xa,
		   int *ldxa, double *sepd, double *thnorm,
		   int *iwork, double *dwork, int *ldwork,
		   int *info, long int job_len, long int trana_len,
		   long int lyapun_len)
{
  /* System generated locals */
  int t_dim1, t_offset, u_dim1, u_offset, xa_dim1, xa_offset, i__1, i__2;

  /* Local variables */
  int kase, itmp;
  char uplo[1];
  int info2;
  double scale;
  int wants, wantt;
  int nn;
  double bignum;
  int update;
  char tranat[1];
  int notrna;
  double est;

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
   *    To estimate the "separation" between the matrices op(A) and 
   *    op(A)', 
   * 
   *    sepd(op(A),op(A)') = min norm(op(A)'*X*op(A) - X)/norm(X) 
   *                       = 1 / norm(inv(Omega)) 
   * 
   *    and/or the 1-norm of Theta, where op(A) = A or A' (A**T), and 
   *    Omega and Theta are linear operators associated to the real 
   *    discrete-time Lyapunov matrix equation 
   * 
   *           op(A)'*X*op(A) - X = C, 
   * 
   *    defined by 
   * 
   *    Omega(W) = op(A)'*W*op(A) - W, 
   *    Theta(W) = inv(Omega(op(W)'*X*op(A) + op(A)'*X*op(W))). 
   * 
   *    The 1-norm condition estimators are used. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    JOB     CHARACTER*1 
   *            Specifies the computation to be performed, as follows: 
   *            = 'S':  Compute the separation only; 
   *            = 'T':  Compute the norm of Theta only; 
   *            = 'B':  Compute both the separation and the norm of Theta. 
   * 
   *    TRANA   CHARACTER*1 
   *            Specifies the form of op(A) to be used, as follows: 
   *            = 'N':  op(A) = A    (No transpose); 
   *            = 'T':  op(A) = A**T (Transpose); 
   *            = 'C':  op(A) = A**T (Conjugate transpose = Transpose). 
   * 
   *    LYAPUN  CHARACTER*1 
   *            Specifies whether or not the original Lyapunov equations 
   *            should be solved, as follows: 
   *            = 'O':  Solve the original Lyapunov equations, updating 
   *                    the right-hand sides and solutions with the 
   *                    matrix U, e.g., X <-- U'*X*U; 
   *            = 'R':  Solve reduced Lyapunov equations only, without 
   *                    updating the right-hand sides and solutions. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrices A and X.  N >= 0. 
   * 
   *    T       (input) DOUBLE PRECISION array, dimension (LDT,N) 
   *            The leading N-by-N upper Hessenberg part of this array 
   *            must contain the upper quasi-triangular matrix T in Schur 
   *            canonical form from a Schur factorization of A. 
   * 
   *    LDT     INT 
   *            The leading dimension of array T.  LDT >= MAX(1,N). 
   * 
   *    U       (input) DOUBLE PRECISION array, dimension (LDU,N) 
   *            The leading N-by-N part of this array must contain the 
   *            orthogonal matrix U from a real Schur factorization of A. 
   *            If LYAPUN = 'R', the array U is not referenced. 
   * 
   *    LDU     INT 
   *            The leading dimension of array U. 
   *            LDU >= 1,        if LYAPUN = 'R'; 
   *            LDU >= MAX(1,N), if LYAPUN = 'O'. 
   * 
   *    XA      (input) DOUBLE PRECISION array, dimension (LDXA,N) 
   *            The leading N-by-N part of this array must contain the 
   *            matrix product X*op(A), if LYAPUN = 'O', or U'*X*U*op(T), 
   *            if LYAPUN = 'R', in the Lyapunov equation. 
   *            If JOB = 'S', the array XA is not referenced. 
   * 
   *    LDXA    INT 
   *            The leading dimension of array XA. 
   *            LDXA >= 1,        if JOB = 'S'; 
   *            LDXA >= MAX(1,N), if JOB = 'T' or 'B'. 
   * 
   *    SEPD    (output) DOUBLE PRECISION 
   *            If JOB = 'S' or JOB = 'B', and INFO >= 0, SEPD contains 
   *            the estimated quantity sepd(op(A),op(A)'). 
   *            If JOB = 'T' or N = 0, SEPD is not referenced. 
   * 
   *    THNORM  (output) DOUBLE PRECISION 
   *            If JOB = 'T' or JOB = 'B', and INFO >= 0, THNORM contains 
   *            the estimated 1-norm of operator Theta. 
   *            If JOB = 'S' or N = 0, THNORM is not referenced. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (N*N) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= 0,            if N = 0; 
   *            LDWORK >= MAX(3,2*N*N), if N > 0. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = N+1:  if T has (almost) reciprocal eigenvalues; 
   *                  perturbed values were used to solve Lyapunov 
   *                  equations (but the matrix T is unchanged). 
   * 
   *    METHOD 
   * 
   *    SEPD is defined as 
   * 
   *           sepd( op(A), op(A)' ) = sigma_min( K ) 
   * 
   *    where sigma_min(K) is the smallest singular value of the 
   *    N*N-by-N*N matrix 
   * 
   *       K = kprod( op(A)', op(A)' ) - I(N**2). 
   * 
   *    I(N**2) is an N*N-by-N*N identity matrix, and kprod denotes the 
   *    Kronecker product. The routine estimates sigma_min(K) by the 
   *    reciprocal of an estimate of the 1-norm of inverse(K), computed as 
   *    suggested in [1]. This involves the solution of several discrete- 
   *    time Lyapunov equations, either direct or transposed. The true 
   *    reciprocal 1-norm of inverse(K) cannot differ from sigma_min(K) by 
   *    more than a factor of N. 
   *    The 1-norm of Theta is estimated similarly. 
   * 
   *    REFERENCES 
   * 
   *    [1] Higham, N.J. 
   *        FORTRAN codes for estimating the one-norm of a real or 
   *        complex matrix, with applications to condition estimation. 
   *        ACM Trans. Math. Softw., 14, pp. 381-396, 1988. 
   * 
   *    NUMERICAL ASPECTS 
   *                              3 
   *    The algorithm requires 0(N ) operations. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    When SEPD is zero, the routine returns immediately, with THNORM 
   *    (if requested) not set. In this case, the equation is singular. 
   *    The option LYAPUN = 'R' may occasionally produce slightly worse 
   *    or better estimates, and it is much faster than the option 'O'. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Romania, 
   *    Oct. 1998. Partly based on DDLSVX (and then SB03SD) by P. Petkov, 
   *    Tech. University of Sofia, March 1998 (and December 1998). 
   * 
   *    REVISIONS 
   * 
   *    February 6, 1999, V. Sima, Katholieke Univ. Leuven, Belgium. 
   *    V. Sima, Research Institute for Informatics, Bucharest, Oct. 2004. 
   * 
   *    KEYWORDS 
   * 
   *    Lyapunov equation, orthogonal transformation, real Schur form. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. 
   *    .. Scalar Arguments .. 
   *    .. 
   *    .. Array Arguments .. 
   *    .. 
   *    .. Local Scalars .. 
   *    .. 
   *    .. External Functions .. 
   *    .. 
   *    .. External Subroutines .. 
   *    .. 
   *    .. Intrinsic Functions .. 
   *    .. 
   *    .. Executable Statements .. 
   * 
   *    Decode and Test input parameters. 
   * 
   */
  /* Parameter adjustments */
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  xa_dim1 = *ldxa;
  xa_offset = xa_dim1 + 1;
  xa -= xa_offset;
  --iwork;
  --dwork;

  /* Function Body */
  wants = C2F (lsame) (job, "S", 1L, 1L);
  wantt = C2F (lsame) (job, "T", 1L, 1L);
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  update = C2F (lsame) (lyapun, "O", 1L, 1L);
  /* 
   */
  nn = *n * *n;
  *info = 0;
  if (!(wants || wantt || C2F (lsame) (job, "B", 1L, 1L)))
    {
      *info = -1;
    }
  else
    if (!
	(notrna || C2F (lsame) (trana, "T", 1L, 1L)
	 || C2F (lsame) (trana, "C", 1L, 1L)))
      {
	*info = -2;
      }
    else if (!(update || C2F (lsame) (lyapun, "R", 1L, 1L)))
      {
	*info = -3;
      }
    else if (*n < 0)
      {
	*info = -4;
      }
    else if (*ldt < Max (1, *n))
      {
	*info = -6;
      }
    else if (*ldu < 1 || update && *ldu < *n)
      {
	*info = -8;
      }
    else if (*ldxa < 1 || !wants && *ldxa < *n)
      {
	*info = -10;
      }
    else				/* if(complicated condition) */
      {
	/*Computing MAX 
	 */
	i__1 = 3, i__2 = nn << 1;
	if (*ldwork < 0 || *ldwork < Max (i__1, i__2) && *n > 0)
	  {
	    *info = -15;
	  }
      }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("SB03SY", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   */
  itmp = nn + 1;
  /* 
   */
  if (notrna)
    {
      *(unsigned char *) tranat = 'T';
    }
  else
    {
      *(unsigned char *) tranat = 'N';
    }
  /* 
   */
  if (!wantt)
    {
      /* 
       *       Estimate sepd(op(A),op(A)'). 
       *       Workspace:  Max(3,2*N*N). 
       * 
       */
      kase = 0;
      /* 
       *       REPEAT 
       */
    L10:
      C2F (dlacon) (&nn, &dwork[itmp], &dwork[1], &iwork[1], &est, &kase);
      if (kase != 0)
	{
	  /* 
	   *          Select the triangular part of symmetric matrix to be used. 
	   * 
	   */
	  if (C2F (dlansy)
	      ("1-norm", "Upper", n, &dwork[1], n, &dwork[itmp], 6L,
	       5L) >= C2F (dlansy) ("1-norm", "Lower", n, &dwork[1], n,
				    &dwork[itmp], 6L, 5L))
	    {
	      *(unsigned char *) uplo = 'U';
	    }
	  else
	    {
	      *(unsigned char *) uplo = 'L';
	    }
	  /* 
	   */
	  if (update)
	    {
	      /* 
	       *             Transform the right-hand side: RHS := U'*RHS*U. 
	       * 
	       */
	      nsp_slicot_mb01ru (uplo, "Transpose", n, n, &c_b21, &c_b22,
				 &dwork[1], n, &u[u_offset], ldu, &dwork[1],
				 n, &dwork[itmp], &nn, &info2, 1L, 9L);
	      i__1 = *n + 1;
	      C2F (dscal) (n, &c_b23, &dwork[1], &i__1);
	    }
	  nsp_slicot_ma02ed (uplo, n, &dwork[1], n, 1L);
	  /* 
	   */
	  if (kase == 1)
	    {
	      /* 
	       *             Solve op(T)'*Y*op(T) - Y = scale*RHS. 
	       * 
	       */
	      nsp_slicot_sb03mx (trana, n, &t[t_offset], ldt, &dwork[1], n,
				 &scale, &dwork[itmp], &info2, 1L);
	    }
	  else
	    {
	      /* 
	       *             Solve op(T)*W*op(T)' - W = scale*RHS. 
	       * 
	       */
	      nsp_slicot_sb03mx (tranat, n, &t[t_offset], ldt, &dwork[1], n,
				 &scale, &dwork[itmp], &info2, 1L);
	    }
	  /* 
	   */
	  if (info2 > 0)
	    {
	      *info = *n + 1;
	    }
	  /* 
	   */
	  if (update)
	    {
	      /* 
	       *             Transform back to obtain the solution: Z := U*Z*U', with 
	       *             Z = Y or Z = W. 
	       * 
	       */
	      nsp_slicot_mb01ru (uplo, "No transpose", n, n, &c_b21, &c_b22,
				 &dwork[1], n, &u[u_offset], ldu, &dwork[1],
				 n, &dwork[itmp], &nn, &info2, 1L, 12L);
	      i__1 = *n + 1;
	      C2F (dscal) (n, &c_b23, &dwork[1], &i__1);
	      /* 
	       *             Fill in the remaining triangle of the symmetric matrix. 
	       * 
	       */
	      nsp_slicot_ma02ed (uplo, n, &dwork[1], n, 1L);
	    }
	  /* 
	   */
	  goto L10;
	}
      /*       UNTIL KASE = 0 
       * 
       */
      if (est > scale)
	{
	  *sepd = scale / est;
	}
      else
	{
	  bignum = 1. / C2F (dlamch) ("Safe minimum", 12L);
	  if (scale < est * bignum)
	    {
	      *sepd = scale / est;
	    }
	  else
	    {
	      *sepd = bignum;
	    }
	}
      /* 
       *       Return if the equation is singular. 
       * 
       */
      if (*sepd == 0.)
	{
	  return 0;
	}
    }
  /* 
   */
  if (!wants)
    {
      /* 
       *       Estimate norm(Theta). 
       *       Workspace:  Max(3,2*N*N). 
       * 
       */
      kase = 0;
      /* 
       *       REPEAT 
       */
    L20:
      C2F (dlacon) (&nn, &dwork[itmp], &dwork[1], &iwork[1], &est, &kase);
      if (kase != 0)
	{
	  /* 
	   *          Select the triangular part of symmetric matrix to be used. 
	   * 
	   */
	  if (C2F (dlansy)
	      ("1-norm", "Upper", n, &dwork[1], n, &dwork[itmp], 6L,
	       5L) >= C2F (dlansy) ("1-norm", "Lower", n, &dwork[1], n,
				    &dwork[itmp], 6L, 5L))
	    {
	      *(unsigned char *) uplo = 'U';
	    }
	  else
	    {
	      *(unsigned char *) uplo = 'L';
	    }
	  /* 
	   *          Fill in the remaining triangle of the symmetric matrix. 
	   * 
	   */
	  nsp_slicot_ma02ed (uplo, n, &dwork[1], n, 1L);
	  /* 
	   *          Compute RHS = op(W)'*X*op(A) + op(A)'*X*op(W). 
	   * 
	   */
	  C2F (dsyr2k) (uplo, tranat, n, n, &c_b22, &dwork[1], n,
			&xa[xa_offset], ldxa, &c_b21, &dwork[itmp], n, 1L,
			1L);
	  C2F (dlacpy) (uplo, n, n, &dwork[itmp], n, &dwork[1], n, 1L);
	  /* 
	   */
	  if (update)
	    {
	      /* 
	       *             Transform the right-hand side: RHS := U'*RHS*U. 
	       * 
	       */
	      nsp_slicot_mb01ru (uplo, "Transpose", n, n, &c_b21, &c_b22,
				 &dwork[1], n, &u[u_offset], ldu, &dwork[1],
				 n, &dwork[itmp], &nn, &info2, 1L, 9L);
	      i__1 = *n + 1;
	      C2F (dscal) (n, &c_b23, &dwork[1], &i__1);
	    }
	  nsp_slicot_ma02ed (uplo, n, &dwork[1], n, 1L);
	  /* 
	   */
	  if (kase == 1)
	    {
	      /* 
	       *             Solve op(T)'*Y*op(T) - Y = scale*RHS. 
	       * 
	       */
	      nsp_slicot_sb03mx (trana, n, &t[t_offset], ldt, &dwork[1], n,
				 &scale, &dwork[itmp], &info2, 1L);
	    }
	  else
	    {
	      /* 
	       *             Solve op(T)*W*op(T)' - W = scale*RHS. 
	       * 
	       */
	      nsp_slicot_sb03mx (tranat, n, &t[t_offset], ldt, &dwork[1], n,
				 &scale, &dwork[itmp], &info2, 1L);
	    }
	  /* 
	   */
	  if (info2 > 0)
	    {
	      *info = *n + 1;
	    }
	  /* 
	   */
	  if (update)
	    {
	      /* 
	       *             Transform back to obtain the solution: Z := U*Z*U', with 
	       *             Z = Y or Z = W. 
	       * 
	       */
	      nsp_slicot_mb01ru (uplo, "No transpose", n, n, &c_b21, &c_b22,
				 &dwork[1], n, &u[u_offset], ldu, &dwork[1],
				 n, &dwork[itmp], &nn, &info2, 1L, 12L);
	      i__1 = *n + 1;
	      C2F (dscal) (n, &c_b23, &dwork[1], &i__1);
	      /* 
	       *             Fill in the remaining triangle of the symmetric matrix. 
	       * 
	       */
	      nsp_slicot_ma02ed (uplo, n, &dwork[1], n, 1L);
	    }
	  /* 
	   */
	  goto L20;
	}
      /*       UNTIL KASE = 0 
       * 
       */
      if (est < scale)
	{
	  *thnorm = est / scale;
	}
      else
	{
	  bignum = 1. / C2F (dlamch) ("Safe minimum", 12L);
	  if (est < scale * bignum)
	    {
	      *thnorm = est / scale;
	    }
	  else
	    {
	      *thnorm = bignum;
	    }
	}
    }
  /* 
   */
  return 0;
  /**** Last line of SB03SY *** 
   */
}				/* nsp_slicot_sb03sy */
