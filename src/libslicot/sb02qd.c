/* SB02QD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b18 = -1.;
static double c_b19 = 1.;
static int c__1 = 1;
static double c_b41 = 0.;
static double c_b43 = .5;

/* Subroutine */ int
nsp_slicot_sb02qd (char *job, char *fact, char *trana, char *uplo,
		   char *lyapun, int *n, double *a, int *lda, double *t,
		   int *ldt, double *u, int *ldu, double *g, int *ldg,
		   double *q, int *ldq, double *x, int *ldx, double *sep,
		   double *rcond, double *ferr, int *iwork, double *dwork,
		   int *ldwork, int *info, long int job_len,
		   long int fact_len, long int trana_len, long int uplo_len,
		   long int lyapun_len)
{
  /* System generated locals */
  int a_dim1, a_offset, g_dim1, g_offset, q_dim1, q_offset, t_dim1,
    t_offset, u_dim1, u_offset, x_dim1, x_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Local variables */
  int jobb, jobc, jobe;
  int iabs, kase, sdim;
  char sjob[1];
  int ires, ixbs;
  double epsn, temp;
  int itmp;
  double tmax;
  char loup[1];
  int info2;
  int i__, j;
  double scale;
  double denom;
  double anorm;
  double gnorm;
  int bwork[1];
  int lower;
  double qnorm, xnorm;
  int needac;
  int jj;
  int nn;
  int jx;
  int nofact;
  double bignum;
  int update;
  char tranat[1];
  int notrna;
  double pinorm, xanorm, thnorm;
  int wrkopt;
  double sig;
  int lwa, ldw;
  double eps, est;

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
   *    To estimate the conditioning and compute an error bound on the 
   *    solution of the real continuous-time matrix algebraic Riccati 
   *    equation 
   * 
   *        op(A)'*X + X*op(A) + Q - X*G*X = 0,                        (1) 
   * 
   *    where op(A) = A or A' (A**T) and Q, G are symmetric (Q = Q**T, 
   *    G = G**T). The matrices A, Q and G are N-by-N and the solution X 
   *    is N-by-N. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    JOB     CHARACTER*1 
   *            Specifies the computation to be performed, as follows: 
   *            = 'C':  Compute the reciprocal condition number only; 
   *            = 'E':  Compute the error bound only; 
   *            = 'B':  Compute both the reciprocal condition number and 
   *                    the error bound. 
   * 
   *    FACT    CHARACTER*1 
   *            Specifies whether or not the real Schur factorization of 
   *            the matrix Ac = A - G*X (if TRANA = 'N') or Ac = A - X*G 
   *            (if TRANA = 'T' or 'C') is supplied on entry, as follows: 
   *            = 'F':  On entry, T and U (if LYAPUN = 'O') contain the 
   *                    factors from the real Schur factorization of the 
   *                    matrix Ac; 
   *            = 'N':  The Schur factorization of Ac will be computed 
   *                    and the factors will be stored in T and U (if 
   *                    LYAPUN = 'O'). 
   * 
   *    TRANA   CHARACTER*1 
   *            Specifies the form of op(A) to be used, as follows: 
   *            = 'N':  op(A) = A    (No transpose); 
   *            = 'T':  op(A) = A**T (Transpose); 
   *            = 'C':  op(A) = A**T (Conjugate transpose = Transpose). 
   * 
   *    UPLO    CHARACTER*1 
   *            Specifies which part of the symmetric matrices Q and G is 
   *            to be used, as follows: 
   *            = 'U':  Upper triangular part; 
   *            = 'L':  Lower triangular part. 
   * 
   *    LYAPUN  CHARACTER*1 
   *            Specifies whether or not the original Lyapunov equations 
   *            should be solved in the iterative estimation process, 
   *            as follows: 
   *            = 'O':  Solve the original Lyapunov equations, updating 
   *                    the right-hand sides and solutions with the 
   *                    matrix U, e.g., RHS <-- U'*RHS*U; 
   *            = 'R':  Solve reduced Lyapunov equations only, without 
   *                    updating the right-hand sides and solutions. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrices A, X, Q, and G.  N >= 0. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *            If FACT = 'N' or LYAPUN = 'O', the leading N-by-N part of 
   *            this array must contain the matrix A. 
   *            If FACT = 'F' and LYAPUN = 'R', A is not referenced. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A. 
   *            LDA >= Max(1,N), if FACT = 'N' or  LYAPUN = 'O'; 
   *            LDA >= 1,        if FACT = 'F' and LYAPUN = 'R'. 
   * 
   *    T       (input or output) DOUBLE PRECISION array, dimension 
   *            (LDT,N) 
   *            If FACT = 'F', then T is an input argument and on entry, 
   *            the leading N-by-N upper Hessenberg part of this array 
   *            must contain the upper quasi-triangular matrix T in Schur 
   *            canonical form from a Schur factorization of Ac (see 
   *            argument FACT). 
   *            If FACT = 'N', then T is an output argument and on exit, 
   *            if INFO = 0 or INFO = N+1, the leading N-by-N upper 
   *            Hessenberg part of this array contains the upper quasi- 
   *            triangular matrix T in Schur canonical form from a Schur 
   *            factorization of Ac (see argument FACT). 
   * 
   *    LDT     INT 
   *            The leading dimension of the array T.  LDT >= Max(1,N). 
   * 
   *    U       (input or output) DOUBLE PRECISION array, dimension 
   *            (LDU,N) 
   *            If LYAPUN = 'O' and FACT = 'F', then U is an input 
   *            argument and on entry, the leading N-by-N part of this 
   *            array must contain the orthogonal matrix U from a real 
   *            Schur factorization of Ac (see argument FACT). 
   *            If LYAPUN = 'O' and FACT = 'N', then U is an output 
   *            argument and on exit, if INFO = 0 or INFO = N+1, it 
   *            contains the orthogonal N-by-N matrix from a real Schur 
   *            factorization of Ac (see argument FACT). 
   *            If LYAPUN = 'R', the array U is not referenced. 
   * 
   *    LDU     INT 
   *            The leading dimension of the array U. 
   *            LDU >= 1,        if LYAPUN = 'R'; 
   *            LDU >= MAX(1,N), if LYAPUN = 'O'. 
   * 
   *    G       (input) DOUBLE PRECISION array, dimension (LDG,N) 
   *            If UPLO = 'U', the leading N-by-N upper triangular part of 
   *            this array must contain the upper triangular part of the 
   *            matrix G. 
   *            If UPLO = 'L', the leading N-by-N lower triangular part of 
   *            this array must contain the lower triangular part of the 
   *            matrix G.                     _ 
   *            Matrix G should correspond to G in the "reduced" Riccati 
   *            equation (with matrix T, instead of A), if LYAPUN = 'R'. 
   *            See METHOD. 
   * 
   *    LDG     INT 
   *            The leading dimension of the array G.  LDG >= Max(1,N). 
   * 
   *    Q       (input) DOUBLE PRECISION array, dimension (LDQ,N) 
   *            If UPLO = 'U', the leading N-by-N upper triangular part of 
   *            this array must contain the upper triangular part of the 
   *            matrix Q. 
   *            If UPLO = 'L', the leading N-by-N lower triangular part of 
   *            this array must contain the lower triangular part of the 
   *            matrix Q.                     _ 
   *            Matrix Q should correspond to Q in the "reduced" Riccati 
   *            equation (with matrix T, instead of A), if LYAPUN = 'R'. 
   *            See METHOD. 
   * 
   *    LDQ     INT 
   *            The leading dimension of the array Q.  LDQ >= Max(1,N). 
   * 
   *    X       (input) DOUBLE PRECISION array, dimension (LDX,N) 
   *            The leading N-by-N part of this array must contain the 
   *            symmetric solution matrix of the original Riccati 
   *            equation (with matrix A), if LYAPUN = 'O', or of the 
   *            "reduced" Riccati equation (with matrix T), if 
   *            LYAPUN = 'R'. See METHOD. 
   * 
   *    LDX     INT 
   *            The leading dimension of the array X.  LDX >= Max(1,N). 
   * 
   *    SEP     (output) DOUBLE PRECISION 
   *            If JOB = 'C' or JOB = 'B', the estimated quantity 
   *            sep(op(Ac),-op(Ac)'). 
   *            If N = 0, or X = 0, or JOB = 'E', SEP is not referenced. 
   * 
   *    RCOND   (output) DOUBLE PRECISION 
   *            If JOB = 'C' or JOB = 'B', an estimate of the reciprocal 
   *            condition number of the continuous-time Riccati equation. 
   *            If N = 0 or X = 0, RCOND is set to 1 or 0, respectively. 
   *            If JOB = 'E', RCOND is not referenced. 
   * 
   *    FERR    (output) DOUBLE PRECISION 
   *            If JOB = 'E' or JOB = 'B', an estimated forward error 
   *            bound for the solution X. If XTRUE is the true solution, 
   *            FERR bounds the magnitude of the largest entry in 
   *            (X - XTRUE) divided by the magnitude of the largest entry 
   *            in X. 
   *            If N = 0 or X = 0, FERR is set to 0. 
   *            If JOB = 'C', FERR is not referenced. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (N*N) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0 or INFO = N+1, DWORK(1) returns the 
   *            optimal value of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The dimension of the array DWORK. 
   *            Let LWA = N*N, if LYAPUN = 'O' and JOB = 'E' or 'B'; 
   *                LWA = 0,   otherwise. 
   *            If FACT = 'N', then 
   *               LDWORK  = MAX(1, 5*N, 2*N*N),        if JOB = 'C'; 
   *               LDWORK  = MAX(1, LWA + 5*N, 4*N*N ), if JOB = 'E', 'B'. 
   *            If FACT = 'F', then 
   *               LDWORK  = MAX(1, 2*N*N),  if JOB = 'C'; 
   *               LDWORK  = MAX(1, 4*N*N ), if JOB = 'E' or 'B'. 
   *            For good performance, LDWORK must generally be larger. 
   * 
   *    Error indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            > 0:  if INFO = i, i <= N, the QR algorithm failed to 
   *                  complete the reduction of the matrix Ac to Schur 
   *                  canonical form (see LAPACK Library routine DGEES); 
   *                  on exit, the matrix T(i+1:N,i+1:N) contains the 
   *                  partially converged Schur form, and DWORK(i+1:N) and 
   *                  DWORK(N+i+1:2*N) contain the real and imaginary 
   *                  parts, respectively, of the converged eigenvalues; 
   *                  this error is unlikely to appear; 
   *            = N+1:  if the matrices T and -T' have common or very 
   *                  close eigenvalues; perturbed values were used to 
   *                  solve Lyapunov equations, but the matrix T, if given 
   *                  (for FACT = 'F'), is unchanged. 
   * 
   *    METHOD 
   * 
   *    The condition number of the Riccati equation is estimated as 
   * 
   *    cond = ( norm(Theta)*norm(A) + norm(inv(Omega))*norm(Q) + 
   *                norm(Pi)*norm(G) ) / norm(X), 
   * 
   *    where Omega, Theta and Pi are linear operators defined by 
   * 
   *    Omega(W) = op(Ac)'*W + W*op(Ac), 
   *    Theta(W) = inv(Omega(op(W)'*X + X*op(W))), 
   *       Pi(W) = inv(Omega(X*W*X)), 
   * 
   *    and Ac = A - G*X (if TRANA = 'N') or Ac = A - X*G (if TRANA = 'T' 
   *    or 'C'). Note that the Riccati equation (1) is equivalent to 
   *               _   _         _   _ _ _ 
   *        op(T)'*X + X*op(T) + Q + X*G*X = 0,                        (2) 
   *          _           _               _ 
   *    where X = U'*X*U, Q = U'*Q*U, and G = U'*G*U, with U the 
   *    orthogonal matrix reducing Ac to a real Schur form, T = U'*Ac*U. 
   * 
   *    The routine estimates the quantities 
   * 
   *    sep(op(Ac),-op(Ac)') = 1 / norm(inv(Omega)), 
   * 
   *    norm(Theta) and norm(Pi) using 1-norm condition estimator. 
   * 
   *    The forward error bound is estimated using a practical error bound 
   *    similar to the one proposed in [2]. 
   * 
   *    REFERENCES 
   * 
   *    [1] Ghavimi, A.R. and Laub, A.J. 
   *        Backward error, sensitivity, and refinement of computed 
   *        solutions of algebraic Riccati equations. 
   *        Numerical Linear Algebra with Applications, vol. 2, pp. 29-49, 
   *        1995. 
   * 
   *    [2] Higham, N.J. 
   *        Perturbation theory and backward error for AX-XB=C. 
   *        BIT, vol. 33, pp. 124-136, 1993. 
   * 
   *    [3] Petkov, P.Hr., Konstantinov, M.M., and Mehrmann, V. 
   *        DGRSVX and DMSRIC: Fortran 77 subroutines for solving 
   *        continuous-time matrix algebraic Riccati equations with 
   *        condition and accuracy estimates. 
   *        Preprint SFB393/98-16, Fak. f. Mathematik, Tech. Univ. 
   *        Chemnitz, May 1998. 
   * 
   *    NUMERICAL ASPECTS 
   *                              3 
   *    The algorithm requires 0(N ) operations. 
   *    The accuracy of the estimates obtained depends on the solution 
   *    accuracy and on the properties of the 1-norm estimator. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    The option LYAPUN = 'R' may occasionally produce slightly worse 
   *    or better estimates, and it is much faster than the option 'O'. 
   *    When SEP is computed and it is zero, the routine returns 
   *    immediately, with RCOND and FERR (if requested) set to 0 and 1, 
   *    respectively. In this case, the equation is singular. 
   * 
   *    CONTRIBUTOR 
   * 
   *    P.Hr. Petkov, Technical University of Sofia, December 1998. 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, February 1999. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Oct. 2004. 
   * 
   *    KEYWORDS 
   * 
   *    Conditioning, error estimates, orthogonal transformation, 
   *    real Schur form, Riccati equation. 
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
   *    .. Local Arrays .. 
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
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  t_dim1 = *ldt;
  t_offset = t_dim1 + 1;
  t -= t_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  g_dim1 = *ldg;
  g_offset = g_dim1 + 1;
  g -= g_offset;
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --iwork;
  --dwork;

  /* Function Body */
  jobc = C2F (lsame) (job, "C", 1L, 1L);
  jobe = C2F (lsame) (job, "E", 1L, 1L);
  jobb = C2F (lsame) (job, "B", 1L, 1L);
  nofact = C2F (lsame) (fact, "N", 1L, 1L);
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  lower = C2F (lsame) (uplo, "L", 1L, 1L);
  update = C2F (lsame) (lyapun, "O", 1L, 1L);
  /* 
   */
  needac = update && !jobc;
  /* 
   */
  nn = *n * *n;
  if (needac)
    {
      lwa = nn;
    }
  else
    {
      lwa = 0;
    }
  /* 
   */
  if (nofact)
    {
      if (jobc)
	{
	  /*Computing MAX 
	   */
	  i__1 = *n * 5, i__2 = nn << 1;
	  ldw = Max (i__1, i__2);
	}
      else
	{
	  /*Computing MAX 
	   */
	  i__1 = lwa + *n * 5, i__2 = nn << 2;
	  ldw = Max (i__1, i__2);
	}
    }
  else
    {
      if (jobc)
	{
	  ldw = nn << 1;
	}
      else
	{
	  ldw = nn << 2;
	}
    }
  /* 
   */
  *info = 0;
  if (!(jobb || jobc || jobe))
    {
      *info = -1;
    }
  else if (!(nofact || C2F (lsame) (fact, "F", 1L, 1L)))
    {
      *info = -2;
    }
  else
    if (!
	(notrna || C2F (lsame) (trana, "T", 1L, 1L)
	 || C2F (lsame) (trana, "C", 1L, 1L)))
      {
	*info = -3;
      }
    else if (!(lower || C2F (lsame) (uplo, "U", 1L, 1L)))
      {
	*info = -4;
      }
    else if (!(update || C2F (lsame) (lyapun, "R", 1L, 1L)))
      {
	*info = -5;
      }
    else if (*n < 0)
      {
	*info = -6;
      }
    else if (*lda < 1 || *lda < *n && (update || nofact))
      {
	*info = -8;
      }
    else if (*ldt < Max (1, *n))
      {
	*info = -10;
      }
    else if (*ldu < 1 || *ldu < *n && update)
      {
	*info = -12;
      }
    else if (*ldg < Max (1, *n))
      {
	*info = -14;
      }
    else if (*ldq < Max (1, *n))
      {
	*info = -16;
      }
    else if (*ldx < Max (1, *n))
      {
	*info = -18;
      }
    else if (*ldwork < Max (1, ldw))
      {
	*info = -24;
      }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("SB02QD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0)
    {
      if (!jobe)
	{
	  *rcond = 1.;
	}
      if (!jobc)
	{
	  *ferr = 0.;
	}
      dwork[1] = 1.;
      return 0;
    }
  /* 
   *    Compute the 1-norm of the matrix X. 
   * 
   */
  xnorm =
    C2F (dlansy) ("1-norm", uplo, n, &x[x_offset], ldx, &dwork[1], 6L, 1L);
  if (xnorm == 0.)
    {
      /* 
       *       The solution is zero. 
       * 
       */
      if (!jobe)
	{
	  *rcond = 0.;
	}
      if (!jobc)
	{
	  *ferr = 0.;
	}
      dwork[1] = (double) (*n);
      return 0;
    }
  /* 
   *    Workspace usage. 
   * 
   */
  ixbs = 0;
  itmp = ixbs + nn;
  iabs = itmp + nn;
  ires = iabs + nn;
  /* 
   *    Workspace:  LWR, where 
   *                LWR = N*N, if LYAPUN = 'O' and JOB = 'E' or 'B', or 
   *                              FACT = 'N', 
   *                LWR = 0,   otherwise. 
   * 
   */
  if (needac || nofact)
    {
      /* 
       */
      C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &dwork[1], n, 4L);
      if (notrna)
	{
	  /* 
	   *          Compute Ac = A - G*X. 
	   * 
	   */
	  C2F (dsymm) ("Left", uplo, n, n, &c_b18, &g[g_offset], ldg,
		       &x[x_offset], ldx, &c_b19, &dwork[1], n, 4L, 1L);
	}
      else
	{
	  /* 
	   *          Compute Ac = A - X*G. 
	   * 
	   */
	  C2F (dsymm) ("Right", uplo, n, n, &c_b18, &g[g_offset], ldg,
		       &x[x_offset], ldx, &c_b19, &dwork[1], n, 5L, 1L);
	}
      /* 
       */
      wrkopt = (int) ((double) nn);
      if (nofact)
	{
	  C2F (dlacpy) ("Full", n, n, &dwork[1], n, &t[t_offset], ldt, 4L);
	}
    }
  else
    {
      wrkopt = (int) ((double) (*n));
    }
  /* 
   */
  if (nofact)
    {
      /* 
       *       Compute the Schur factorization of Ac, Ac = U*T*U'. 
       *       Workspace:  need   LWA + 5*N; 
       *                   prefer larger; 
       *                   LWA = N*N, if LYAPUN = 'O' and JOB = 'E' or 'B'; 
       *                   LWA = 0,   otherwise. 
       *       (Note: Comments in the code beginning "Workspace:" describe the 
       *       minimal amount of real workspace needed at that point in the 
       *       code, as well as the preferred amount for good performance.) 
       * 
       */
      if (update)
	{
	  *(unsigned char *) sjob = 'V';
	}
      else
	{
	  *(unsigned char *) sjob = 'N';
	}
      i__1 = *ldwork - lwa - (*n << 1);
      C2F (dgees) (sjob, "Not ordered", (L_fp) nsp_slicot_select, n,
		   &t[t_offset], ldt, &sdim, &dwork[lwa + 1],
		   &dwork[lwa + *n + 1], &u[u_offset], ldu,
		   &dwork[lwa + (*n << 1) + 1], &i__1, bwork, info, 1L, 11L);
      if (*info > 0)
	{
	  if (lwa > 0)
	    {
	      i__1 = *n << 1;
	      C2F (dcopy) (&i__1, &dwork[lwa + 1], &c__1, &dwork[1], &c__1);
	    }
	  return 0;
	}
      /* 
       *Computing MAX 
       */
      i__1 = wrkopt, i__2 =
	(int) dwork[lwa + (*n << 1) + 1] + lwa + (*n << 1);
      wrkopt = Max (i__1, i__2);
    }
  if (needac)
    {
      C2F (dlacpy) ("Full", n, n, &dwork[1], n, &dwork[iabs + 1], n, 4L);
    }
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
  if (!jobe)
    {
      /* 
       *       Estimate sep(op(Ac),-op(Ac)') = sep(op(T),-op(T)') and 
       *       norm(Theta). 
       *       Workspace LWA + 2*N*N. 
       * 
       */
      nsp_slicot_sb03qy ("Both", trana, lyapun, n, &t[t_offset], ldt,
			 &u[u_offset], ldu, &x[x_offset], ldx, sep, &thnorm,
			 &iwork[1], &dwork[1], ldwork, info, 4L, 1L, 1L);
      /* 
       *Computing MAX 
       */
      i__1 = wrkopt, i__2 = lwa + (nn << 1);
      wrkopt = Max (i__1, i__2);
      /* 
       *       Return if the equation is singular. 
       * 
       */
      if (*sep == 0.)
	{
	  *rcond = 0.;
	  if (jobb)
	    {
	      *ferr = 1.;
	    }
	  dwork[1] = (double) wrkopt;
	  return 0;
	}
      /* 
       *       Estimate norm(Pi). 
       *       Workspace LWA + 2*N*N. 
       * 
       */
      kase = 0;
      /* 
       *       REPEAT 
       */
    L10:
      C2F (dlacon) (&nn, &dwork[itmp + 1], &dwork[1], &iwork[1], &est, &kase);
      if (kase != 0)
	{
	  /* 
	   *          Select the triangular part of symmetric matrix to be used. 
	   * 
	   */
	  if (C2F (dlansy)
	      ("1-norm", "Upper", n, &dwork[1], n, &dwork[itmp + 1], 6L,
	       5L) >= C2F (dlansy) ("1-norm", "Lower", n, &dwork[1], n,
				    &dwork[itmp + 1], 6L, 5L))
	    {
	      *(unsigned char *) loup = 'U';
	    }
	  else
	    {
	      *(unsigned char *) loup = 'L';
	    }
	  /* 
	   *          Compute RHS = X*W*X. 
	   * 
	   */
	  nsp_slicot_mb01ru (loup, "No Transpose", n, n, &c_b41, &c_b19,
			     &dwork[1], n, &x[x_offset], ldx, &dwork[1], n,
			     &dwork[itmp + 1], &nn, &info2, 1L, 12L);
	  i__1 = *n + 1;
	  C2F (dscal) (n, &c_b43, &dwork[1], &i__1);
	  /* 
	   */
	  if (update)
	    {
	      /* 
	       *             Transform the right-hand side: RHS := U'*RHS*U. 
	       * 
	       */
	      nsp_slicot_mb01ru (loup, "Transpose", n, n, &c_b41, &c_b19,
				 &dwork[1], n, &u[u_offset], ldu, &dwork[1],
				 n, &dwork[itmp + 1], &nn, &info2, 1L, 9L);
	      i__1 = *n + 1;
	      C2F (dscal) (n, &c_b43, &dwork[1], &i__1);
	    }
	  /* 
	   *          Fill in the remaining triangle of the symmetric matrix. 
	   * 
	   */
	  nsp_slicot_ma02ed (loup, n, &dwork[1], n, 1L);
	  /* 
	   */
	  if (kase == 1)
	    {
	      /* 
	       *             Solve op(T)'*Y + Y*op(T) = scale*RHS. 
	       * 
	       */
	      nsp_slicot_sb03my (trana, n, &t[t_offset], ldt, &dwork[1], n,
				 &scale, &info2, 1L);
	    }
	  else
	    {
	      /* 
	       *             Solve op(T)*W + W*op(T)' = scale*RHS. 
	       * 
	       */
	      nsp_slicot_sb03my (tranat, n, &t[t_offset], ldt, &dwork[1], n,
				 &scale, &info2, 1L);
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
	      nsp_slicot_mb01ru (loup, "No transpose", n, n, &c_b41, &c_b19,
				 &dwork[1], n, &u[u_offset], ldu, &dwork[1],
				 n, &dwork[itmp + 1], &nn, &info2, 1L, 12L);
	      i__1 = *n + 1;
	      C2F (dscal) (n, &c_b43, &dwork[1], &i__1);
	      /* 
	       *             Fill in the remaining triangle of the symmetric matrix. 
	       * 
	       */
	      nsp_slicot_ma02ed (loup, n, &dwork[1], n, 1L);
	    }
	  goto L10;
	}
      /*       UNTIL KASE = 0 
       * 
       */
      if (est < scale)
	{
	  pinorm = est / scale;
	}
      else
	{
	  bignum = 1. / C2F (dlamch) ("Safe minimum", 12L);
	  if (est < scale * bignum)
	    {
	      pinorm = est / scale;
	    }
	  else
	    {
	      pinorm = bignum;
	    }
	}
      /* 
       *       Compute the 1-norm of A or T. 
       * 
       */
      if (update)
	{
	  anorm =
	    C2F (dlange) ("1-norm", n, n, &a[a_offset], lda, &dwork[1], 6L);
	}
      else
	{
	  anorm =
	    C2F (dlanhs) ("1-norm", n, &t[t_offset], ldt, &dwork[1], 6L);
	}
      /* 
       *       Compute the 1-norms of the matrices Q and G. 
       * 
       */
      qnorm =
	C2F (dlansy) ("1-norm", uplo, n, &q[q_offset], ldq, &dwork[1], 6L,
		      1L);
      gnorm =
	C2F (dlansy) ("1-norm", uplo, n, &g[g_offset], ldg, &dwork[1], 6L,
		      1L);
      /* 
       *       Estimate the reciprocal condition number. 
       * 
       *Computing MAX 
       */
      d__1 = Max (*sep, xnorm), d__1 = Max (d__1, anorm);
      tmax = Max (d__1, gnorm);
      if (tmax <= 1.)
	{
	  temp = *sep * xnorm;
	  denom = qnorm + *sep * anorm * thnorm + *sep * gnorm * pinorm;
	}
      else
	{
	  temp = *sep / tmax * (xnorm / tmax);
	  denom =
	    1. / tmax * (qnorm / tmax) +
	    *sep / tmax * (anorm / tmax) * thnorm +
	    *sep / tmax * (gnorm / tmax) * pinorm;
	}
      if (temp >= denom)
	{
	  *rcond = 1.;
	}
      else
	{
	  *rcond = temp / denom;
	}
    }
  /* 
   */
  if (!jobc)
    {
      /* 
       *       Form a triangle of the residual matrix 
       *         R = op(A)'*X + X*op(A) + Q - X*G*X, 
       *       or           _   _         _   _ _ _ 
       *         R = op(T)'*X + X*op(T) + Q + X*G*X, 
       *       exploiting the symmetry. 
       *       Workspace 4*N*N. 
       * 
       */
      if (update)
	{
	  C2F (dlacpy) (uplo, n, n, &q[q_offset], ldq, &dwork[ires + 1], n,
			1L);
	  C2F (dsyr2k) (uplo, tranat, n, n, &c_b19, &a[a_offset], lda,
			&x[x_offset], ldx, &c_b19, &dwork[ires + 1], n, 1L,
			1L);
	  sig = -1.;
	}
      else
	{
	  nsp_slicot_mb01ud ("Right", trana, n, n, &c_b19, &t[t_offset], ldt,
			     &x[x_offset], ldx, &dwork[ires + 1], n, &info2,
			     5L, 1L);
	  jj = ires + 1;
	  if (lower)
	    {
	      i__1 = *n;
	      for (j = 1; j <= i__1; ++j)
		{
		  i__2 = *n - j + 1;
		  C2F (daxpy) (&i__2, &c_b19, &dwork[jj], n, &dwork[jj],
			       &c__1);
		  i__2 = *n - j + 1;
		  C2F (daxpy) (&i__2, &c_b19, &q[j + j * q_dim1], &c__1,
			       &dwork[jj], &c__1);
		  jj = jj + *n + 1;
		  /* L20: */
		}
	    }
	  else
	    {
	      i__1 = *n;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (daxpy) (&j, &c_b19, &dwork[ires + j], n, &dwork[jj],
			       &c__1);
		  C2F (daxpy) (&j, &c_b19, &q[j * q_dim1 + 1], &c__1,
			       &dwork[jj], &c__1);
		  jj += *n;
		  /* L30: */
		}
	    }
	  sig = 1.;
	}
      nsp_slicot_mb01ru (uplo, tranat, n, n, &c_b19, &sig, &dwork[ires + 1],
			 n, &x[x_offset], ldx, &g[g_offset], ldg,
			 &dwork[itmp + 1], &nn, &info2, 1L, 1L);
      /* 
       *       Get the machine precision. 
       * 
       */
      eps = C2F (dlamch) ("Epsilon", 7L);
      epsn = eps * (double) (*n + 4);
      temp = eps * 4.;
      /* 
       *       Add to Abs(R) a term that takes account of rounding errors in 
       *       forming R: 
       *        Abs(R) := Abs(R) + EPS*(4*abs(Q) + (n+4)*(Abs(op(Ac))'*abs(X) 
       *                + Abs(X)*abs(op(Ac))) + 2*(n+1)*abs(X)*abs(G)*abs(X)), 
       *       or                             _                           _ 
       *        Abs(R) := Abs(R) + EPS*(4*abs(Q) + (n+4)*(Abs(op(T))'*abs(X) 
       *                      _                            _      _      _ 
       *                + Abs(X)*abs(op(T))) + 2*(n+1)*abs(X)*abs(G)*abs(X)), 
       *       where EPS is the machine precision. 
       * 
       */
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      dwork[ixbs + (j - 1) * *n + i__] = (d__1 =
						  x[i__ + j * x_dim1],
						  Abs (d__1));
	      /* L40: */
	    }
	  /* L50: */
	}
      /* 
       */
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  dwork[ires + (j - 1) * *n + i__] = temp * (d__1 =
							     q[i__ +
							       j * q_dim1],
							     Abs (d__1)) +
		    (d__2 = dwork[ires + (j - 1) * *n + i__], Abs (d__2));
		  /* L60: */
		}
	      /* L70: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  dwork[ires + (j - 1) * *n + i__] = temp * (d__1 =
							     q[i__ +
							       j * q_dim1],
							     Abs (d__1)) +
		    (d__2 = dwork[ires + (j - 1) * *n + i__], Abs (d__2));
		  /* L80: */
		}
	      /* L90: */
	    }
	}
      /* 
       */
      if (update)
	{
	  /* 
	   */
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  dwork[iabs + (j - 1) * *n + i__] = (d__1 =
						      dwork[iabs +
							    (j - 1) * *n +
							    i__], Abs (d__1));
		  /* L100: */
		}
	      /* L110: */
	    }
	  /* 
	   */
	  C2F (dsyr2k) (uplo, tranat, n, n, &epsn, &dwork[iabs + 1], n,
			&dwork[ixbs + 1], n, &c_b19, &dwork[ires + 1], n, 1L,
			1L);
	}
      else
	{
	  /* 
	   */
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      /*Computing MIN 
	       */
	      i__3 = j + 1;
	      i__2 = Min (i__3, *n);
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  dwork[iabs + (j - 1) * *n + i__] = (d__1 =
						      t[i__ + j * t_dim1],
						      Abs (d__1));
		  /* L120: */
		}
	      /* L130: */
	    }
	  /* 
	   */
	  nsp_slicot_mb01ud ("Left", tranat, n, n, &epsn, &dwork[iabs + 1], n,
			     &dwork[ixbs + 1], n, &dwork[itmp + 1], n, &info2,
			     4L, 1L);
	  jj = ires + 1;
	  jx = itmp + 1;
	  if (lower)
	    {
	      i__1 = *n;
	      for (j = 1; j <= i__1; ++j)
		{
		  i__2 = *n - j + 1;
		  C2F (daxpy) (&i__2, &c_b19, &dwork[jx], n, &dwork[jx],
			       &c__1);
		  i__2 = *n - j + 1;
		  C2F (daxpy) (&i__2, &c_b19, &dwork[jx], &c__1, &dwork[jj],
			       &c__1);
		  jj = jj + *n + 1;
		  jx = jx + *n + 1;
		  /* L140: */
		}
	    }
	  else
	    {
	      i__1 = *n;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (daxpy) (&j, &c_b19, &dwork[itmp + j], n, &dwork[jx],
			       &c__1);
		  C2F (daxpy) (&j, &c_b19, &dwork[jx], &c__1, &dwork[jj],
			       &c__1);
		  jj += *n;
		  jx += *n;
		  /* L150: */
		}
	    }
	}
      /* 
       */
      if (lower)
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  dwork[iabs + (j - 1) * *n + i__] = (d__1 =
						      g[i__ + j * g_dim1],
						      Abs (d__1));
		  /* L160: */
		}
	      /* L170: */
	    }
	}
      else
	{
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  dwork[iabs + (j - 1) * *n + i__] = (d__1 =
						      g[i__ + j * g_dim1],
						      Abs (d__1));
		  /* L180: */
		}
	      /* L190: */
	    }
	}
      /* 
       */
      d__1 = eps * (double) (*n + 1 << 1);
      nsp_slicot_mb01ru (uplo, trana, n, n, &c_b19, &d__1, &dwork[ires + 1],
			 n, &dwork[ixbs + 1], n, &dwork[iabs + 1], n,
			 &dwork[itmp + 1], &nn, &info2, 1L, 1L);
      /* 
       *Computing MAX 
       */
      i__1 = wrkopt, i__2 = nn << 2;
      wrkopt = Max (i__1, i__2);
      /* 
       *       Compute forward error bound, using matrix norm estimator. 
       *       Workspace 4*N*N. 
       * 
       */
      xanorm =
	C2F (dlansy) ("Max", uplo, n, &x[x_offset], ldx, &dwork[1], 3L, 1L);
      /* 
       */
      nsp_slicot_sb03qx (trana, uplo, lyapun, n, &xanorm, &t[t_offset], ldt,
			 &u[u_offset], ldu, &dwork[ires + 1], n, ferr,
			 &iwork[1], &dwork[1], &ires, info, 1L, 1L, 1L);
    }
  /* 
   */
  dwork[1] = (double) wrkopt;
  return 0;
  /* 
**** Last line of SB02QD *** 
*/
}				/* nsp_slicot_sb02qd */
