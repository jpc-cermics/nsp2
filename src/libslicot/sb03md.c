/* SB03MD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b18 = 0.;
static double c_b19 = 1.;
static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_sb03md (char *dico, char *job, char *fact, char *trana, int *n,
		   double *a, int *lda, double *u, int *ldu,
		   double *c__, int *ldc, double *scale,
		   double *sep, double *ferr, double *wr,
		   double *wi, int *iwork, double *dwork,
		   int *ldwork, int *info, long int dico_len,
		   long int job_len, long int fact_len, long int trana_len)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, u_dim1, u_offset, i__1, i__2;
  double d__1;

  /* Local variables */
  int kase, sdim;
  int nota;
  int ierr;
  int cont;
  char uplo[1];
  int i__;
  char notra[1];
  int bwork[1], wantx;
  int nn;
  double scalef;
  int nofact;
  int wantbh;
  int minwrk;
  char transt[1];
  int wantsp;
  int nn2;
  char ntrnst[1];
  int lwa;
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
   *    To solve for X either the real continuous-time Lyapunov equation 
   * 
   *       op(A)'*X + X*op(A) = scale*C                             (1) 
   * 
   *    or the real discrete-time Lyapunov equation 
   * 
   *       op(A)'*X*op(A) - X = scale*C                             (2) 
   * 
   *    and/or estimate an associated condition number, called separation, 
   *    where op(A) = A or A' (A**T) and C is symmetric (C = C'). 
   *    (A' denotes the transpose of the matrix A.) A is N-by-N, the right 
   *    hand side C and the solution X are N-by-N, and scale is an output 
   *    scale factor, set less than or equal to 1 to avoid overflow in X. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    DICO    CHARACTER*1 
   *            Specifies the equation from which X is to be determined 
   *            as follows: 
   *            = 'C':  Equation (1), continuous-time case; 
   *            = 'D':  Equation (2), discrete-time case. 
   * 
   *    JOB     CHARACTER*1 
   *            Specifies the computation to be performed, as follows: 
   *            = 'X':  Compute the solution only; 
   *            = 'S':  Compute the separation only; 
   *            = 'B':  Compute both the solution and the separation. 
   * 
   *    FACT    CHARACTER*1 
   *            Specifies whether or not the real Schur factorization 
   *            of the matrix A is supplied on entry, as follows: 
   *            = 'F':  On entry, A and U contain the factors from the 
   *                    real Schur factorization of the matrix A; 
   *            = 'N':  The Schur factorization of A will be computed 
   *                    and the factors will be stored in A and U. 
   * 
   *    TRANA   CHARACTER*1 
   *            Specifies the form of op(A) to be used, as follows: 
   *            = 'N':  op(A) = A    (No transpose); 
   *            = 'T':  op(A) = A**T (Transpose); 
   *            = 'C':  op(A) = A**T (Conjugate transpose = Transpose). 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrices A, X, and C.  N >= 0. 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) 
   *            On entry, the leading N-by-N part of this array must 
   *            contain the matrix A. If FACT = 'F', then A contains 
   *            an upper quasi-triangular matrix in Schur canonical form; 
   *            the elements below the upper Hessenberg part of the 
   *            array A are not referenced. 
   *            On exit, if INFO = 0 or INFO = N+1, the leading N-by-N 
   *            upper Hessenberg part of this array contains the upper 
   *            quasi-triangular matrix in Schur canonical form from the 
   *            Schur factorization of A. The contents of array A is not 
   *            modified if FACT = 'F'. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,N). 
   * 
   *    U       (input or output) DOUBLE PRECISION array, dimension 
   *            (LDU,N) 
   *            If FACT = 'F', then U is an input argument and on entry 
   *            the leading N-by-N part of this array must contain the 
   *            orthogonal matrix U of the real Schur factorization of A. 
   *            If FACT = 'N', then U is an output argument and on exit, 
   *            if INFO = 0 or INFO = N+1, it contains the orthogonal 
   *            N-by-N matrix from the real Schur factorization of A. 
   * 
   *    LDU     INT 
   *            The leading dimension of array U.  LDU >= MAX(1,N). 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,N) 
   *            On entry with JOB = 'X' or 'B', the leading N-by-N part of 
   *            this array must contain the symmetric matrix C. 
   *            On exit with JOB = 'X' or 'B', if INFO = 0 or INFO = N+1, 
   *            the leading N-by-N part of C has been overwritten by the 
   *            symmetric solution matrix X. 
   *            If JOB = 'S', C is not referenced. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C. 
   *            LDC >= 1,        if JOB = 'S'; 
   *            LDC >= MAX(1,N), otherwise. 
   * 
   *    SCALE   (output) DOUBLE PRECISION 
   *            The scale factor, scale, set less than or equal to 1 to 
   *            prevent the solution overflowing. 
   * 
   *    SEP     (output) DOUBLE PRECISION 
   *            If JOB = 'S' or JOB = 'B', and INFO = 0 or INFO = N+1, SEP 
   *            contains the estimated separation of the matrices op(A) 
   *            and -op(A)', if DICO = 'C' or of op(A) and op(A)', if 
   *            DICO = 'D'. 
   *            If JOB = 'X' or N = 0, SEP is not referenced. 
   * 
   *    FERR    (output) DOUBLE PRECISION 
   *            If JOB = 'B', and INFO = 0 or INFO = N+1, FERR contains an 
   *            estimated forward error bound for the solution X. 
   *            If XTRUE is the true solution, FERR bounds the relative 
   *            error in the computed solution, measured in the Frobenius 
   *            norm:  norm(X - XTRUE)/norm(XTRUE). 
   *            If JOB = 'X' or JOB = 'S', FERR is not referenced. 
   * 
   *    WR      (output) DOUBLE PRECISION array, dimension (N) 
   *    WI      (output) DOUBLE PRECISION array, dimension (N) 
   *            If FACT = 'N', and INFO = 0 or INFO = N+1, WR and WI 
   *            contain the real and imaginary parts, respectively, of 
   *            the eigenvalues of A. 
   *            If FACT = 'F', WR and WI are not referenced. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (N*N) 
   *            This array is not referenced if JOB = 'X'. 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0 or INFO = N+1, DWORK(1) returns the 
   *            optimal value of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK.  LDWORK >= 1, and 
   *            If JOB = 'X' then 
   *               If FACT = 'F', LDWORK >= N*N,           for DICO = 'C'; 
   *                              LDWORK >= MAX(N*N, 2*N), for DICO = 'D'; 
   *               If FACT = 'N', LDWORK >= MAX(N*N, 3*N). 
   *            If JOB = 'S' or JOB = 'B' then 
   *               If FACT = 'F', LDWORK >= 2*N*N,       for DICO = 'C'; 
   *                              LDWORK >= 2*N*N + 2*N, for DICO = 'D'. 
   *               If FACT = 'N', LDWORK >= MAX(2*N*N, 3*N), DICO = 'C'; 
   *                              LDWORK >= 2*N*N + 2*N, for DICO = 'D'. 
   *            For optimum performance LDWORK should be larger. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            > 0:  if INFO = i, the QR algorithm failed to compute all 
   *                  the eigenvalues (see LAPACK Library routine DGEES); 
   *                  elements i+1:n of WR and WI contain eigenvalues 
   *                  which have converged, and A contains the partially 
   *                  converged Schur form; 
   *            = N+1:  if DICO = 'C', and the matrices A and -A' have 
   *                  common or very close eigenvalues, or 
   *                  if DICO = 'D', and matrix A has almost reciprocal 
   *                  eigenvalues (that is, lambda(i) = 1/lambda(j) for 
   *                  some i and j, where lambda(i) and lambda(j) are 
   *                  eigenvalues of A and i <> j); perturbed values were 
   *                  used to solve the equation (but the matrix A is 
   *                  unchanged). 
   * 
   *    METHOD 
   * 
   *    The Schur factorization of a square matrix  A  is given by 
   * 
   *       A = U*S*U' 
   * 
   *    where U is orthogonal and S is block upper triangular with 1-by-1 
   *    and 2-by-2 blocks on its diagonal, these blocks corresponding to 
   *    the eigenvalues of A, the 2-by-2 blocks being complex conjugate 
   *    pairs. This factorization is obtained by numerically stable 
   *    methods: first A is reduced to upper Hessenberg form (if FACT = 
   *    'N') by means of Householder transformations and then the 
   *    QR Algorithm is applied to reduce the Hessenberg form to S, the 
   *    transformation matrices being accumulated at each step to give U. 
   *    If A has already been factorized prior to calling the routine 
   *    however, then the factors U and S may be supplied and the initial 
   *    factorization omitted. 
   *                  _            _ 
   *    If we now put C = U'CU and X = UXU' equations (1) and (2) (see 
   *    PURPOSE) become (for TRANS = 'N') 
   *         _   _    _ 
   *       S'X + XS = C,                                               (3) 
   *    and 
   *         _    _   _ 
   *       S'XS - X = C,                                               (4) 
   * 
   *    respectively. Partition S, C and X as 
   *                           _   _         _   _ 
   *           (s    s')      (c   c')      (x   x') 
   *           ( 11    )  _   ( 11   )  _   ( 11   ) 
   *       S = (       ), C = (      ), X = (      ) 
   *           (       )      ( _    )      ( _    ) 
   *           ( 0   S )      ( c  C )      ( x  X ) 
   *                  1             1             1 
   *               _      _ 
   *    where s  , c  and x  are either scalars or 2-by-2 matrices and s, 
   *           11   11     11 
   *    _     _ 
   *    c and x are either (N-1) element vectors or matrices with two 
   *    columns. Equations (3) and (4) can then be re-written as 
   *          _     _        _ 
   *       s' x   + x  s   = c                                       (3.1) 
   *        11 11    11 11    11 
   * 
   *         _   _           _    _ 
   *       S'x + xs        = c - sx                                  (3.2) 
   *        1      11              11 
   * 
   *                               _    _ 
   *       S'X + X S       = C - (sx' + xs')                         (3.3) 
   *        1 1   1 1         1 
   *    and 
   *          _       _       _ 
   *       s' x  s  - x     = c                                      (4.1) 
   *        11 11 11   11      11 
   * 
   *         _     _          _    _ 
   *       S'xs  - x        = c - sx  s                              (4.2) 
   *        1  11                   11 11 
   * 
   *                               _            _        _ 
   *       S'X S - X        = C - sx  s' - [s(S'x)' + (S'x)s']       (4.3) 
   *        1 1 1   1          1    11         1        1 
   *                                                 _ 
   *    respectively. If DICO = 'C' ['D'], then once x   has been 
   *                                                  11 
   *    found from equation (3.1) [(4.1)], equation (3.2) [(4.2)] can be 
   *                                       _ 
   *    solved by forward substitution for x and then equation (3.3) 
   *    [(4.3)] is of the same form as (3) [(4)] but of the order (N-1) or 
   *    (N-2) depending upon whether s   is 1-by-1 or 2-by-2. 
   *                                  11 
   *                            _      _ 
   *    When s   is 2-by-2 then x  and c   will be 1-by-2 matrices and s, 
   *          11                 11     11 
   *    _     _ 
   *    x and c are matrices with two columns. In this case, equation 
   *    (3.1) [(4.1)] defines the three equations in the unknown elements 
   *       _ 
   *    of x   and equation (3.2) [(4.2)] can then be solved by forward 
   *        11                 _ 
   *    substitution, a row of x being found at each step. 
   * 
   *    REFERENCES 
   * 
   *    [1] Barraud, A.Y.                   T 
   *        A numerical algorithm to solve A XA - X = Q. 
   *        IEEE Trans. Auto. Contr., AC-22, pp. 883-885, 1977. 
   * 
   *    [2] Bartels, R.H. and Stewart, G.W.  T 
   *        Solution of the matrix equation A X + XB = C. 
   *        Comm. A.C.M., 15, pp. 820-826, 1972. 
   * 
   *    [3] Hammarling, S.J. 
   *        Numerical solution of the stable, non-negative definite 
   *        Lyapunov equation. 
   *        IMA J. Num. Anal., 2, pp. 303-325, 1982. 
   * 
   *    NUMERICAL ASPECTS 
   *                              3 
   *    The algorithm requires 0(N ) operations and is backward stable. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    If DICO = 'C', SEP is defined as the separation of op(A) and 
   *    -op(A)': 
   * 
   *           sep( op(A), -op(A)' ) = sigma_min( T ) 
   * 
   *    and if DICO = 'D', SEP is defined as 
   * 
   *           sep( op(A), op(A)' ) = sigma_min( T ) 
   * 
   *    where sigma_min(T) is the smallest singular value of the 
   *    N*N-by-N*N matrix 
   * 
   *      T = kprod( I(N), op(A)' ) + kprod( op(A)', I(N) )  (DICO = 'C'), 
   * 
   *      T = kprod( op(A)', op(A)' ) - I(N**2)              (DICO = 'D'). 
   * 
   *    I(x) is an x-by-x identity matrix, and kprod denotes the Kronecker 
   *    product. The program estimates sigma_min(T) by the reciprocal of 
   *    an estimate of the 1-norm of inverse(T). The true reciprocal 
   *    1-norm of inverse(T) cannot differ from sigma_min(T) by more 
   *    than a factor of N. 
   * 
   *    When SEP is small, small changes in A, C can cause large changes 
   *    in the solution of the equation. An approximate bound on the 
   *    maximum relative error in the computed solution is 
   * 
   *                     EPS * norm(A) / SEP      (DICO = 'C'), 
   * 
   *                     EPS * norm(A)**2 / SEP   (DICO = 'D'), 
   * 
   *    where EPS is the machine precision. 
   * 
   *    CONTRIBUTOR 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, July 1997. 
   *    Supersedes Release 2.0 routine SB03AD by Control Systems Research 
   *    Group, Kingston Polytechnic, United Kingdom. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, May 1999. 
   * 
   *    KEYWORDS 
   * 
   *    Lyapunov equation, orthogonal transformation, real Schur form, 
   *    Sylvester equation. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
   *    .. Local Arrays .. 
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
   *    .. Executable Statements .. 
   * 
   *    Decode and Test input parameters. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  u_dim1 = *ldu;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  --wr;
  --wi;
  --iwork;
  --dwork;

  /* Function Body */
  cont = C2F (lsame) (dico, "C", 1L, 1L);
  wantx = C2F (lsame) (job, "X", 1L, 1L);
  wantsp = C2F (lsame) (job, "S", 1L, 1L);
  wantbh = C2F (lsame) (job, "B", 1L, 1L);
  nofact = C2F (lsame) (fact, "N", 1L, 1L);
  nota = C2F (lsame) (trana, "N", 1L, 1L);
  nn = *n * *n;
  nn2 = nn << 1;
  /* 
   */
  *info = 0;
  if (!cont && !C2F (lsame) (dico, "D", 1L, 1L))
    {
      *info = -1;
    }
  else if (!wantbh && !wantsp && !wantx)
    {
      *info = -2;
    }
  else if (!nofact && !C2F (lsame) (fact, "F", 1L, 1L))
    {
      *info = -3;
    }
  else if (!nota && !C2F (lsame) (trana, "T", 1L, 1L)
	   && !C2F (lsame) (trana, "C", 1L, 1L))
    {
      *info = -4;
    }
  else if (*n < 0)
    {
      *info = -5;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldu < Max (1, *n))
    {
      *info = -9;
    }
  else if (wantsp && *ldc < 1 || !wantsp && *ldc < Max (1, *n))
    {
      *info = -11;
    }
  else
    {
      if (wantx)
	{
	  if (nofact)
	    {
	      /*Computing MAX 
	       */
	      i__1 = nn, i__2 = *n * 3;
	      minwrk = Max (i__1, i__2);
	    }
	  else if (cont)
	    {
	      minwrk = nn;
	    }
	  else
	    {
	      /*Computing MAX 
	       */
	      i__1 = nn, i__2 = *n << 1;
	      minwrk = Max (i__1, i__2);
	    }
	}
      else
	{
	  if (cont)
	    {
	      if (nofact)
		{
		  /*Computing MAX 
		   */
		  i__1 = nn2, i__2 = *n * 3;
		  minwrk = Max (i__1, i__2);
		}
	      else
		{
		  minwrk = nn2;
		}
	    }
	  else
	    {
	      minwrk = nn2 + (*n << 1);
	    }
	}
      if (*ldwork < Max (1, minwrk))
	{
	  *info = -19;
	}
    }
  /* 
   */
  if (*info != 0)
    {
      /* 
       *       Error return. 
       * 
       */
      i__1 = -(*info);
      C2F (xerbla) ("SB03MD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0)
    {
      *scale = 1.;
      if (wantbh)
	{
	  *ferr = 0.;
	}
      dwork[1] = 1.;
      return 0;
    }
  /* 
   */
  lwa = 0;
  /* 
   */
  if (nofact)
    {
      /* 
       *       Compute the Schur factorization of A. 
       *       Workspace:  need   3*N; 
       *                   prefer larger. 
       *       (Note: Comments in the code beginning "Workspace:" describe the 
       *       minimal amount of real workspace needed at that point in the 
       *       code, as well as the preferred amount for good performance. 
       *       NB refers to the optimal block size for the immediately 
       *       following subroutine, as returned by ILAENV.) 
       * 
       */
      C2F (dgees) ("Vectors", "Not ordered", (L_fp) nsp_slicot_select, n,
		   &a[a_offset], lda, &sdim, &wr[1], &wi[1], &u[u_offset],
		   ldu, &dwork[1], ldwork, bwork, info, 7L, 11L);
      if (*info > 0)
	{
	  return 0;
	}
      lwa = (int) dwork[1];
    }
  /* 
   */
  if (!wantsp)
    {
      /* 
       *       Transform the right-hand side. 
       *       Workspace:  N*N. 
       * 
       */
      *(unsigned char *) ntrnst = 'N';
      *(unsigned char *) transt = 'T';
      *(unsigned char *) uplo = 'U';
      nsp_slicot_mb01rd (uplo, transt, n, n, &c_b18, &c_b19, &c__[c_offset],
			 ldc, &u[u_offset], ldu, &c__[c_offset], ldc,
			 &dwork[1], ldwork, info, 1L, 1L);
      /* 
       */
      i__1 = *n;
      for (i__ = 2; i__ <= i__1; ++i__)
	{
	  i__2 = i__ - 1;
	  C2F (dcopy) (&i__2, &c__[i__ * c_dim1 + 1], &c__1,
		       &c__[i__ + c_dim1], ldc);
	  /* L10: */
	}
      /* 
       */
      lwa = Max (lwa, nn);
      /* 
       *       Solve the transformed equation. 
       *       Workspace for DICO = 'D':  2*N. 
       * 
       */
      if (cont)
	{
	  nsp_slicot_sb03my (trana, n, &a[a_offset], lda, &c__[c_offset], ldc,
			     scale, info, 1L);
	}
      else
	{
	  nsp_slicot_sb03mx (trana, n, &a[a_offset], lda, &c__[c_offset], ldc,
			     scale, &dwork[1], info, 1L);
	}
      if (*info > 0)
	{
	  *info = *n + 1;
	}
      /* 
       *       Transform back the solution. 
       *       Workspace:  N*N. 
       * 
       */
      nsp_slicot_mb01rd (uplo, ntrnst, n, n, &c_b18, &c_b19, &c__[c_offset],
			 ldc, &u[u_offset], ldu, &c__[c_offset], ldc,
			 &dwork[1], ldwork, &ierr, 1L, 1L);
      /* 
       */
      i__1 = *n;
      for (i__ = 2; i__ <= i__1; ++i__)
	{
	  i__2 = i__ - 1;
	  C2F (dcopy) (&i__2, &c__[i__ * c_dim1 + 1], &c__1,
		       &c__[i__ + c_dim1], ldc);
	  /* L20: */
	}
      /* 
       */
    }
  /* 
   */
  if (!wantx)
    {
      /* 
       *       Estimate the separation. 
       *       Workspace:  2*N*N       for DICO = 'C'; 
       *                   2*N*N + 2*N for DICO = 'D'. 
       * 
       */
      if (nota)
	{
	  *(unsigned char *) notra = 'T';
	}
      else
	{
	  *(unsigned char *) notra = 'N';
	}
      /* 
       */
      est = 0.;
      kase = 0;
      /*       REPEAT 
       */
    L30:
      C2F (dlacon) (&nn, &dwork[nn + 1], &dwork[1], &iwork[1], &est, &kase);
      if (kase != 0)
	{
	  if (kase == 1)
	    {
	      if (cont)
		{
		  nsp_slicot_sb03my (trana, n, &a[a_offset], lda, &dwork[1],
				     n, &scalef, &ierr, 1L);
		}
	      else
		{
		  nsp_slicot_sb03mx (trana, n, &a[a_offset], lda, &dwork[1],
				     n, &scalef, &dwork[nn2 + 1], &ierr, 1L);
		}
	    }
	  else
	    {
	      if (cont)
		{
		  nsp_slicot_sb03my (notra, n, &a[a_offset], lda, &dwork[1],
				     n, &scalef, &ierr, 1L);
		}
	      else
		{
		  nsp_slicot_sb03mx (notra, n, &a[a_offset], lda, &dwork[1],
				     n, &scalef, &dwork[nn2 + 1], &ierr, 1L);
		}
	    }
	  goto L30;
	}
      /*       UNTIL KASE = 0 
       * 
       */
      *sep = scalef / est;
      /* 
       */
      if (wantbh)
	{
	  /* 
	   *          Get the machine precision. 
	   * 
	   */
	  eps = C2F (dlamch) ("P", 1L);
	  /* 
	   *          Compute the estimate of the relative error. 
	   * 
	   */
	  if (cont)
	    {
	      *ferr =
		eps * C2F (dlanhs) ("Frobenius", n, &a[a_offset], lda,
				    &dwork[1], 9L) / *sep;
	    }
	  else
	    {
	      /*Computing 2nd power 
	       */
	      d__1 =
		C2F (dlanhs) ("Frobenius", n, &a[a_offset], lda, &dwork[1],
			      9L);
	      *ferr = eps * (d__1 * d__1) / *sep;
	    }
	}
    }
  /* 
   */
  dwork[1] = (double) Max (lwa, minwrk);
  return 0;
  /**** Last line of SB03MD *** 
   */
}				/* nsp_slicot_sb03md */
