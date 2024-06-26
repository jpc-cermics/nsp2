/* SB02ND.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b16 = 1.;
static double c_b17 = 0.;
static int c__1 = 1;
static int c__0 = 0;

/* Subroutine */ int
nsp_slicot_sb02nd (char *dico, char *fact, char *uplo, char *jobl, int *n,
		   int *m, int *p, double *a, int *lda,
		   double *b, int *ldb, double *r__, int *ldr,
		   int *ipiv, double *l, int *ldl, double *x,
		   int *ldx, double *rnorm, double *f, int *ldf,
		   int *oufact, int *iwork, double *dwork,
		   int *ldwork, int *info, long int dico_len,
		   long int fact_len, long int uplo_len, long int jobl_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, f_dim1, f_offset, l_dim1,
    l_offset, r_dim1, r_offset, x_dim1, x_offset, i__1, i__2, i__3, i__4,
    i__5, i__6, i__7, i__8;
  double d__1, d__2;

  /* Builtin functions */
  

  /* Local variables */
  int itau;
  double temp;
  int i__, j;
  int ifail;
  int discr;
  double rcond;
  int withl;
  int jwork;
  double dummy[1];
  int lfacta, lfactc, lfactd;
  int jw, jz;
  int lfactu;
  double rnormp;
  int luplou;
  int wrkopt;
  double eps;

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
   *    To compute the optimal feedback matrix F for the problem of 
   *    optimal control given by 
   * 
   *                       -1 
   *         F = (R + B'XB)  (B'XA + L')                           (1) 
   * 
   *    in the discrete-time case and 
   * 
   *              -1 
   *         F = R  (B'X + L')                                     (2) 
   * 
   *    in the continuous-time case, where A, B and L are N-by-N, N-by-M 
   *    and N-by-M matrices respectively; R and X are M-by-M and N-by-N 
   *    symmetric matrices respectively. 
   * 
   *    Optionally, matrix R may be specified in a factored form, and L 
   *    may be zero. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    DICO    CHARACTER*1 
   *            Specifies the equation from which F is to be determined, 
   *            as follows: 
   *            = 'D':  Equation (1), discrete-time case; 
   *            = 'C':  Equation (2), continuous-time case. 
   * 
   *    FACT    CHARACTER*1 
   *            Specifies how the matrix R is given (factored or not), as 
   *            follows: 
   *            = 'N':  Array R contains the matrix R; 
   *            = 'D':  Array R contains a P-by-M matrix D, where R = D'D; 
   *            = 'C':  Array R contains the Cholesky factor of R; 
   *            = 'U':  Array R contains the symmetric indefinite UdU' or 
   *                    LdL' factorization of R. This option is not 
   *                    available for DICO = 'D'. 
   * 
   *    UPLO    CHARACTER*1 
   *            Specifies which triangle of the possibly factored matrix R 
   *            (or R + B'XB, on exit) is or should be stored, as follows: 
   *            = 'U':  Upper triangle is stored; 
   *            = 'L':  Lower triangle is stored. 
   * 
   *    JOBL    CHARACTER*1 
   *            Specifies whether or not the matrix L is zero, as follows: 
   *            = 'Z':  L is zero; 
   *            = 'N':  L is nonzero. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrices A and X.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The number of system inputs.  M >= 0. 
   * 
   *    P       (input) INT 
   *            The number of system outputs.  P >= 0. 
   *            This parameter must be specified only for FACT = 'D'. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *            If DICO = 'D', the leading N-by-N part of this array must 
   *            contain the state matrix A of the system. 
   *            If DICO = 'C', this array is not referenced. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A. 
   *            LDA >= MAX(1,N) if DICO = 'D'; 
   *            LDA >= 1        if DICO = 'C'. 
   * 
   *    B       (input) DOUBLE PRECISION array, dimension (LDB,M) 
   *            The leading N-by-M part of this array must contain the 
   *            input matrix B of the system. 
   *            If DICO = 'D' and FACT = 'D' or 'C', the contents of this 
   *            array is destroyed. 
   *            Otherwise, B is unchanged on exit. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,N). 
   * 
   *    R       (input/output) DOUBLE PRECISION array, dimension (LDR,M) 
   *            On entry, if FACT = 'N', the leading M-by-M upper 
   *            triangular part (if UPLO = 'U') or lower triangular part 
   *            (if UPLO = 'L') of this array must contain the upper 
   *            triangular part or lower triangular part, respectively, 
   *            of the symmetric input weighting matrix R. 
   *            On entry, if FACT = 'D', the leading P-by-M part of this 
   *            array must contain the direct transmission matrix D of the 
   *            system. 
   *            On entry, if FACT = 'C', the leading M-by-M upper 
   *            triangular part (if UPLO = 'U') or lower triangular part 
   *            (if UPLO = 'L') of this array must contain the Cholesky 
   *            factor of the positive definite input weighting matrix R 
   *            (as produced by LAPACK routine DPOTRF). 
   *            On entry, if DICO = 'C' and FACT = 'U', the leading M-by-M 
   *            upper triangular part (if UPLO = 'U') or lower triangular 
   *            part (if UPLO = 'L') of this array must contain the 
   *            factors of the UdU' or LdL' factorization, respectively, 
   *            of the symmetric indefinite input weighting matrix R (as 
   *            produced by LAPACK routine DSYTRF). 
   *            The stricly lower triangular part (if UPLO = 'U') or 
   *            stricly upper triangular part (if UPLO = 'L') of this 
   *            array is used as workspace. 
   *            On exit, if OUFACT(1) = 1, and INFO = 0 (or INFO = M+1), 
   *            the leading M-by-M upper triangular part (if UPLO = 'U') 
   *            or lower triangular part (if UPLO = 'L') of this array 
   *            contains the Cholesky factor of the given input weighting 
   *            matrix (for DICO = 'C'), or that of the matrix R + B'XB 
   *            (for DICO = 'D'). 
   *            On exit, if OUFACT(1) = 2, and INFO = 0 (or INFO = M+1), 
   *            the leading M-by-M upper triangular part (if UPLO = 'U') 
   *            or lower triangular part (if UPLO = 'L') of this array 
   *            contains the factors of the UdU' or LdL' factorization, 
   *            respectively, of the given input weighting matrix 
   *            (for DICO = 'C'), or that of the matrix R + B'XB 
   *            (for DICO = 'D'). 
   *            On exit R is unchanged if FACT = 'U'. 
   * 
   *    LDR     INT. 
   *            The leading dimension of the array R. 
   *            LDR >= MAX(1,M)   if FACT <> 'D'; 
   *            LDR >= MAX(1,M,P) if FACT =  'D'. 
   * 
   *    IPIV    (input/output) INT array, dimension (M) 
   *            On entry, if FACT = 'U', this array must contain details 
   *            of the interchanges performed and the block structure of 
   *            the d factor in the UdU' or LdL' factorization of matrix R 
   *            (as produced by LAPACK routine DSYTRF). 
   *            On exit, if OUFACT(1) = 2, this array contains details of 
   *            the interchanges performed and the block structure of the 
   *            d factor in the UdU' or LdL' factorization of matrix R (or 
   *            D'D) or R + B'XB (or D'D + B'XB), as produced by LAPACK 
   *            routine DSYTRF. 
   *            This array is not referenced for DICO = 'D' or FACT = 'D', 
   *            or 'C'. 
   * 
   *    L       (input) DOUBLE PRECISION array, dimension (LDL,M) 
   *            If JOBL = 'N', the leading N-by-M part of this array must 
   *            contain the cross weighting matrix L. 
   *            If JOBL = 'Z', this array is not referenced. 
   * 
   *    LDL     INT 
   *            The leading dimension of array L. 
   *            LDL >= MAX(1,N) if JOBL = 'N'; 
   *            LDL >= 1        if JOBL = 'Z'. 
   * 
   *    X       (input/output) DOUBLE PRECISION array, dimension (LDX,N) 
   *            On entry, the leading N-by-N part of this array must 
   *            contain the solution matrix X of the algebraic Riccati 
   *            equation as produced by SLICOT Library routines SB02MD or 
   *            SB02OD. Matrix X is assumed non-negative definite. 
   *            On exit, if DICO = 'D', FACT = 'D' or 'C', OUFACT(2) = 1, 
   *            and INFO = 0, the N-by-N upper triangular part of this 
   *            array contains the Cholesky factor of the given matrix X, 
   *            which is found to be positive definite. 
   *            On exit, if DICO = 'D', FACT = 'D' or 'C', OUFACT(2) = 2, 
   *            and INFO = 0, the leading N-by-N part of this array 
   *            contains the matrix of orthonormal eigenvectors of X. 
   *            On exit X is unchanged if DICO = 'C' or FACT = 'N'. 
   * 
   *    LDX     INT 
   *            The leading dimension of array X.  LDX >= MAX(1,N). 
   * 
   *    RNORM   (input) DOUBLE PRECISION 
   *            If FACT = 'U', this parameter must contain the 1-norm of 
   *            the original matrix R (before factoring it). 
   *            Otherwise, this parameter is not used. 
   * 
   *    F       (output) DOUBLE PRECISION array, dimension (LDF,N) 
   *            The leading M-by-N part of this array contains the 
   *            optimal feedback matrix F. 
   * 
   *    LDF     INT 
   *            The leading dimension of array F.  LDF >= MAX(1,M). 
   * 
   *    OUFACT  (output) INT array, dimension (2) 
   *            Information about the factorization finally used. 
   *            OUFACT(1) = 1:  Cholesky factorization of R (or R + B'XB) 
   *                            has been used; 
   *            OUFACT(1) = 2:  UdU' (if UPLO = 'U') or LdL' (if UPLO = 
   *                            'L') factorization of R (or R + B'XB) 
   *                            has been used; 
   *            OUFACT(2) = 1:  Cholesky factorization of X has been used; 
   *            OUFACT(2) = 2:  Spectral factorization of X has been used. 
   *            The value of OUFACT(2) is not set for DICO = 'C' or for 
   *            DICO = 'D' and FACT = 'N'. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (M) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) returns the optimal value 
   *            of LDWORK, and DWORK(2) contains the reciprocal condition 
   *            number of the matrix R (for DICO = 'C') or of R + B'XB 
   *            (for DICO = 'D'). 
   *            If on exit INFO = 0, and OUFACT(2) = 2, then DWORK(3),..., 
   *            DWORK(N+2) contain the eigenvalues of X, in ascending 
   *            order. 
   * 
   *    LDWORK  INT 
   *            Dimension of working array DWORK. 
   *            LDWORK >= Max(2,3*M)         if FACT = 'N'; 
   *            LDWORK >= Max(2,2*M)         if FACT = 'U'; 
   *            LDWORK >= Max(2,3*M)         if FACT = 'C', DICO = 'C'; 
   *            LDWORK >= N+3*M+2            if FACT = 'C', DICO = 'D'; 
   *            LDWORK >= Max(2,min(P,M)+M)  if FACT = 'D', DICO = 'C'; 
   *            LDWORK >= Max(N+3*M+2,4*N+1) if FACT = 'D', DICO = 'D'. 
   *            For optimum performance LDWORK should be larger. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = i:  if the i-th element of the d factor is exactly zero; 
   *                  the UdU' (or LdL') factorization has been completed, 
   *                  but the block diagonal matrix d is exactly singular; 
   *            = M+1:  if the matrix R (if DICO = 'C'), or R + B'XB 
   *                  (if DICO = 'D') is numerically singular (to working 
   *                  precision); 
   *            = M+2:  if one or more of the eigenvalues of X has not 
   *                  converged. 
   * 
   *    METHOD 
   * 
   *    The optimal feedback matrix F is obtained as the solution to the 
   *    system of linear equations 
   * 
   *       (R + B'XB) * F = B'XA + L' 
   * 
   *    in the discrete-time case and 
   * 
   *       R * F = B'X + L' 
   * 
   *    in the continuous-time case, with R replaced by D'D if FACT = 'D'. 
   *    The factored form of R, specified by FACT <> 'N', is taken into 
   *    account. If FACT = 'N', Cholesky factorization is tried first, but 
   *    if the coefficient matrix is not positive definite, then UdU' (or 
   *    LdL') factorization is used. The discrete-time case involves 
   *    updating of a triangular factorization of R (or D'D); Cholesky or 
   *    symmetric spectral factorization of X is employed to avoid 
   *    squaring of the condition number of the matrix. When D is given, 
   *    its QR factorization is determined, and the triangular factor is 
   *    used as described above. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The algorithm consists of numerically stable steps. 
   *                                   3     2 
   *    For DICO = 'C', it requires O(m  + mn ) floating point operations 
   *                          2 
   *    if FACT = 'N' and O(mn ) floating point operations, otherwise. 
   *    For DICO = 'D', the operation counts are similar, but additional 
   *       3 
   *    O(n ) floating point operations may be needed in the worst case. 
   * 
   *    CONTRIBUTORS 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Sep. 1997. 
   *    Supersedes Release 2.0 routine SB02BD by M. Vanbegin, and 
   *    P. Van Dooren, Philips Research Laboratory, Brussels, Belgium. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Algebraic Riccati equation, closed loop system, continuous-time 
   *    system, discrete-time system, matrix algebra, optimal control, 
   *    optimal regulator. 
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
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  --ipiv;
  l_dim1 = *ldl;
  l_offset = l_dim1 + 1;
  l -= l_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  f_dim1 = *ldf;
  f_offset = f_dim1 + 1;
  f -= f_offset;
  --oufact;
  --iwork;
  --dwork;

  /* Function Body */
  *info = 0;
  discr = C2F (lsame) (dico, "D", 1L, 1L);
  lfactc = C2F (lsame) (fact, "C", 1L, 1L);
  lfactd = C2F (lsame) (fact, "D", 1L, 1L);
  lfactu = C2F (lsame) (fact, "U", 1L, 1L);
  luplou = C2F (lsame) (uplo, "U", 1L, 1L);
  withl = C2F (lsame) (jobl, "N", 1L, 1L);
  lfacta = lfactc || lfactd || lfactu;
  /* 
   *    Test the input scalar arguments. 
   * 
   */
  if (!discr && !C2F (lsame) (dico, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (!lfacta && !C2F (lsame) (fact, "N", 1L, 1L) || discr && lfactu)
    {
      *info = -2;
    }
  else if (!luplou && !C2F (lsame) (uplo, "L", 1L, 1L))
    {
      *info = -3;
    }
  else if (!withl && !C2F (lsame) (jobl, "Z", 1L, 1L))
    {
      *info = -4;
    }
  else if (*n < 0)
    {
      *info = -5;
    }
  else if (*m < 0)
    {
      *info = -6;
    }
  else if (*p < 0)
    {
      *info = -7;
    }
  else if (!discr && *lda < 1 || discr && *lda < Max (1, *n))
    {
      *info = -9;
    }
  else if (*ldb < Max (1, *n))
    {
      *info = -11;
    }
  else if (*ldr < Max (1, *m) || lfactd && *ldr < Max (1, *p))
    {
      *info = -13;
    }
  else if (!withl && *ldl < 1 || withl && *ldl < Max (1, *n))
    {
      *info = -16;
    }
  else if (*ldx < Max (1, *n))
    {
      *info = -18;
    }
  else if (lfactu)
    {
      if (*rnorm < 0.)
	{
	  *info = -19;
	}
    }
  if (*ldf < Max (1, *m))
    {
      *info = -21;
    }
  else				/* if(complicated condition) */
    {
      /*Computing MAX 
       */
      i__1 = 2, i__2 = *m * 3;
      /*Computing MAX 
       */
      i__3 = 2, i__4 = *m << 1;
      /*Computing MAX 
       */
      i__5 = 2, i__6 = Min (*p, *m) + *m;
      /*Computing MAX 
       */
      i__7 = *n + *m * 3 + 2, i__8 = (*n << 2) + 1;
      if ((!lfacta || lfactc && !discr) && *ldwork < Max (i__1, i__2)
	  || lfactu && *ldwork < Max (i__3, i__4) || discr && lfactc
	  && *ldwork < *n + *m * 3 + 2 || !discr && lfactd
	  && *ldwork < Max (i__5, i__6) || discr && lfactd
	  && *ldwork < Max (i__7, i__8))
	{
	  *info = -25;
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
      C2F (xerbla) ("SB02ND", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0 || *m == 0 || lfactd && *p == 0)
    {
      dwork[1] = 1.;
      dwork[2] = 1.;
      return 0;
    }
  /* 
   */
  wrkopt = 1;
  eps = C2F (dlamch) ("Epsilon", 7L);
  /* 
   *    Determine the right-hand side of the matrix equation. 
   *    Compute  B'X  in F. 
   * 
   *    (Note: Comments in the code beginning "Workspace:" describe the 
   *    minimal amount of real workspace needed at that point in the 
   *    code, as well as the preferred amount for good performance. 
   *    NB refers to the optimal block size for the immediately 
   *    following subroutine, as returned by ILAENV.) 
   * 
   */
  C2F (dgemm) ("Transpose", "No transpose", m, n, n, &c_b16, &b[b_offset],
	       ldb, &x[x_offset], ldx, &c_b17, &f[f_offset], ldf, 9L, 12L);
  /* 
   */
  if (!lfacta)
    {
      if (discr)
	{
	  /* 
	   *          Discrete-time case with R not factored. Compute R + B'XB. 
	   * 
	   */
	  if (luplou)
	    {
	      /* 
	       */
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (dgemv) ("No transpose", &j, n, &c_b16, &f[f_offset],
			       ldf, &b[j * b_dim1 + 1], &c__1, &c_b16,
			       &r__[j * r_dim1 + 1], &c__1, 12L);
		  /* L10: */
		}
	      /* 
	       */
	    }
	  else
	    {
	      /* 
	       */
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (dgemv) ("Transpose", n, &j, &c_b16, &b[b_offset], ldb,
			       &f[j + f_dim1], ldf, &c_b16, &r__[j + r_dim1],
			       ldr, 9L);
		  /* L20: */
		}
	      /* 
	       */
	    }
	}
      /* 
       *       Compute the 1-norm of the matrix  R  or  R + B'XB. 
       *       Workspace: need M. 
       * 
       */
      rnormp =
	C2F (dlansy) ("1-norm", uplo, m, &r__[r_offset], ldr, &dwork[1], 6L,
		      1L);
      wrkopt = Max (wrkopt, *m);
    }
  /* 
   */
  if (discr)
    {
      /* 
       *       For discrete-time case, postmultiply B'X by A. 
       *       Workspace: need N. 
       * 
       */
      i__1 = *m;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  C2F (dcopy) (n, &f[i__ + f_dim1], ldf, &dwork[1], &c__1);
	  C2F (dgemv) ("Transpose", n, n, &c_b16, &a[a_offset], lda,
		       &dwork[1], &c__1, &c_b17, &f[i__ + f_dim1], ldf, 9L);
	  /* L30: */
	}
      /* 
       */
      wrkopt = Max (wrkopt, *n);
    }
  /* 
   */
  if (withl)
    {
      /* 
       *       Add L'. 
       * 
       */
      i__1 = *m;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  /* 
	   */
	  i__2 = *n;
	  for (j = 1; j <= i__2; ++j)
	    {
	      f[i__ + j * f_dim1] += l[j + i__ * l_dim1];
	      /* L40: */
	    }
	  /* 
	   */
	  /* L50: */
	}
      /* 
       */
    }
  /* 
   *    Solve the matrix equation. 
   * 
   */
  if (lfacta)
    {
      /* 
       *       Case 1: Matrix R is given in a factored form. 
       * 
       */
      if (lfactd)
	{
	  /* 
	   *          Use QR factorization of D. 
	   *          Workspace: need   Min(P,M) + M, 
	   *                     prefer Min(P,M) + M*NB. 
	   * 
	   */
	  itau = 1;
	  jwork = itau + Min (*p, *m);
	  i__1 = *ldwork - jwork + 1;
	  C2F (dgeqrf) (p, m, &r__[r_offset], ldr, &dwork[itau],
			&dwork[jwork], &i__1, &ifail);
	  /*Computing MAX 
	   */
	  i__1 = wrkopt, i__2 = (int) dwork[jwork] + jwork - 1;
	  wrkopt = Max (i__1, i__2);
	  /* 
	   *          Make positive the diagonal elements of the triangular 
	   *          factor. Construct the strictly lower triangle, if requested. 
	   * 
	   */
	  i__1 = *m;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      if (r__[i__ + i__ * r_dim1] < 0.)
		{
		  /* 
		   */
		  i__2 = *m;
		  for (j = i__; j <= i__2; ++j)
		    {
		      r__[i__ + j * r_dim1] = -r__[i__ + j * r_dim1];
		      /* L60: */
		    }
		  /* 
		   */
		}
	      if (!luplou)
		{
		  i__2 = i__ - 1;
		  C2F (dcopy) (&i__2, &r__[i__ * r_dim1 + 1], &c__1,
			       &r__[i__ + r_dim1], ldr);
		}
	      /* L70: */
	    }
	  /* 
	   */
	  if (*p < *m)
	    {
	      i__1 = *m - *p;
	      C2F (dlaset) ("Full", &i__1, m, &c_b17, &c_b17,
			    &r__[*p + 1 + r_dim1], ldr, 4L);
	      if (!discr)
		{
		  dwork[2] = 0.;
		  *info = *m + 1;
		  return 0;
		}
	    }
	}
      /* 
       */
      jw = 1;
      if (discr)
	{
	  /* 
	   *          Discrete-time case. Update the factorization for B'XB. 
	   *          Try first the Cholesky factorization of X, saving the 
	   *          diagonal of X, in order to recover it, if X is not positive 
	   *          definite. In the later case, use spectral factorization. 
	   *          Workspace: need N. 
	   *          Define     JW = 1   for Cholesky factorization of X, 
	   *                     JW = N+3 for spectral factorization of X. 
	   * 
	   */
	  i__1 = *ldx + 1;
	  C2F (dcopy) (n, &x[x_offset], &i__1, &dwork[1], &c__1);
	  C2F (dpotrf) ("Upper", n, &x[x_offset], ldx, &ifail, 5L);
	  if (ifail == 0)
	    {
	      /* 
	       *             Use Cholesky factorization of X to compute chol(X)*B. 
	       * 
	       */
	      oufact[2] = 1;
	      C2F (dtrmm) ("Left", "Upper", "No transpose", "Non unit", n, m,
			   &c_b16, &x[x_offset], ldx, &b[b_offset], ldb, 4L,
			   5L, 12L, 8L);
	    }
	  else
	    {
	      /* 
	       *             Use spectral factorization of X, X = UVU'. 
	       *             Workspace: need   4*N+1, 
	       *                        prefer N*(NB+2)+N+2. 
	       * 
	       */
	      jw = *n + 3;
	      oufact[2] = 2;
	      i__1 = *ldx + 1;
	      C2F (dcopy) (n, &dwork[1], &c__1, &x[x_offset], &i__1);
	      i__1 = *ldwork - jw + 1;
	      C2F (dsyev) ("Vectors", "Lower", n, &x[x_offset], ldx,
			   &dwork[3], &dwork[jw], &i__1, &ifail, 7L, 5L);
	      if (ifail > 0)
		{
		  *info = *m + 2;
		  return 0;
		}
	      /*Computing MAX 
	       */
	      i__1 = wrkopt, i__2 = (int) dwork[jw] + jw - 1;
	      wrkopt = Max (i__1, i__2);
	      temp = (d__1 = dwork[*n + 2], Abs (d__1)) * eps;
	      /* 
	       *             Count the negligible eigenvalues and compute sqrt(V)U'B. 
	       *             Workspace: need 2*N+2. 
	       * 
	       */
	      jz = 0;
	      /* 
	       */
	    L80:
	      if ((d__1 = dwork[jz + 3], Abs (d__1)) <= temp)
		{
		  ++jz;
		  if (jz < *n)
		    {
		      goto L80;
		    }
		}
	      /* 
	       */
	      i__1 = *m;
	      for (j = 1; j <= i__1; ++j)
		{
		  C2F (dcopy) (n, &b[j * b_dim1 + 1], &c__1, &dwork[jw],
			       &c__1);
		  C2F (dgemv) ("Transpose", n, n, &c_b16, &x[x_offset], ldx,
			       &dwork[jw], &c__1, &c_b17, &b[j * b_dim1 + 1],
			       &c__1, 9L);
		  /* L90: */
		}
	      /* 
	       */
	      i__1 = *n;
	      for (i__ = jz + 1; i__ <= i__1; ++i__)
		{
		  d__2 = sqrt ((d__1 = dwork[i__ + 2], Abs (d__1)));
		  C2F (dscal) (m, &d__2, &b[i__ + b_dim1], ldb);
		  /* L100: */
		}
	      /* 
	       */
	      if (jz > 0)
		{
		  C2F (dlaset) ("Full", &jz, m, &c_b17, &c_b17, &b[b_offset],
				ldb, 4L);
		}
	    }
	  /* 
	   *          Update the triangular factorization. 
	   * 
	   */
	  if (!luplou)
	    {
	      /* 
	       *             For efficiency, use the transposed of the lower triangle. 
	       * 
	       */
	      i__1 = *m;
	      for (i__ = 2; i__ <= i__1; ++i__)
		{
		  i__2 = i__ - 1;
		  C2F (dcopy) (&i__2, &r__[i__ + r_dim1], ldr,
			       &r__[i__ * r_dim1 + 1], &c__1);
		  /* L110: */
		}
	      /* 
	       */
	    }
	  /* 
	   *          Workspace: need JW+2*M-1. 
	   * 
	   */
	  nsp_slicot_mb04kd ("Full", m, &c__0, n, &r__[r_offset], ldr,
			     &b[b_offset], ldb, dummy, n, dummy, m,
			     &dwork[jw], &dwork[jw + *n], 4L);
	  /*Computing MAX 
	   */
	  i__1 = wrkopt, i__2 = jw + (*m << 1) - 1;
	  wrkopt = Max (i__1, i__2);
	  /* 
	   *          Make positive the diagonal elements of the triangular 
	   *          factor. 
	   * 
	   */
	  i__1 = *m;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      if (r__[i__ + i__ * r_dim1] < 0.)
		{
		  /* 
		   */
		  i__2 = *m;
		  for (j = i__; j <= i__2; ++j)
		    {
		      r__[i__ + j * r_dim1] = -r__[i__ + j * r_dim1];
		      /* L120: */
		    }
		  /* 
		   */
		}
	      /* L130: */
	    }
	  /* 
	   */
	  if (!luplou)
	    {
	      /* 
	       *             Construct the lower triangle. 
	       * 
	       */
	      i__1 = *m;
	      for (i__ = 2; i__ <= i__1; ++i__)
		{
		  i__2 = i__ - 1;
		  C2F (dcopy) (&i__2, &r__[i__ * r_dim1 + 1], &c__1,
			       &r__[i__ + r_dim1], ldr);
		  /* L140: */
		}
	      /* 
	       */
	    }
	}
      /* 
       *       Compute the condition number of the coefficient matrix. 
       * 
       */
      if (!lfactu)
	{
	  /* 
	   *          Workspace: need JW+3*M-1. 
	   * 
	   */
	  C2F (dtrcon) ("1-norm", uplo, "Non unit", m, &r__[r_offset], ldr,
			&rcond, &dwork[jw], &iwork[1], &ifail, 6L, 1L, 8L);
	  oufact[1] = 1;
	  /*Computing MAX 
	   */
	  i__1 = wrkopt, i__2 = jw + *m * 3 - 1;
	  wrkopt = Max (i__1, i__2);
	}
      else
	{
	  /* 
	   *          Workspace: need 2*M. 
	   * 
	   */
	  C2F (dsycon) (uplo, m, &r__[r_offset], ldr, &ipiv[1], rnorm, &rcond,
			&dwork[1], &iwork[1], info, 1L);
	  oufact[1] = 2;
	  /*Computing MAX 
	   */
	  i__1 = wrkopt, i__2 = *m << 1;
	  wrkopt = Max (i__1, i__2);
	}
      dwork[2] = rcond;
      if (rcond < eps)
	{
	  *info = *m + 1;
	  return 0;
	}
      /* 
       */
    }
  else
    {
      /* 
       *       Case 2: Matrix R is given in an unfactored form. 
       * 
       *       Save the given triangle of  R  or  R + B'XB  in the other 
       *       strict triangle and the diagonal in the workspace, and try 
       *       Cholesky factorization. 
       *       Workspace: need M. 
       * 
       */
      i__1 = *ldr + 1;
      C2F (dcopy) (m, &r__[r_offset], &i__1, &dwork[1], &c__1);
      if (luplou)
	{
	  /* 
	   */
	  i__1 = *m;
	  for (j = 2; j <= i__1; ++j)
	    {
	      i__2 = j - 1;
	      C2F (dcopy) (&i__2, &r__[j * r_dim1 + 1], &c__1,
			   &r__[j + r_dim1], ldr);
	      /* L150: */
	    }
	  /* 
	   */
	}
      else
	{
	  /* 
	   */
	  i__1 = *m;
	  for (j = 2; j <= i__1; ++j)
	    {
	      i__2 = j - 1;
	      C2F (dcopy) (&i__2, &r__[j + r_dim1], ldr, &r__[j * r_dim1 + 1],
			   &c__1);
	      /* L160: */
	    }
	  /* 
	   */
	}
      C2F (dpotrf) (uplo, m, &r__[r_offset], ldr, info, 1L);
      oufact[1] = 1;
      if (*info == 0)
	{
	  /* 
	   *          Compute the reciprocal of the condition number of R. 
	   *          Workspace: need 3*M. 
	   * 
	   */
	  C2F (dpocon) (uplo, m, &r__[r_offset], ldr, &rnormp, &rcond,
			&dwork[1], &iwork[1], info, 1L);
	  /* 
	   *          Return if the matrix is singular to working precision. 
	   * 
	   */
	  dwork[2] = rcond;
	  if (rcond < eps)
	    {
	      *info = *m + 1;
	      return 0;
	    }
	  /*Computing MAX 
	   */
	  i__1 = wrkopt, i__2 = *m * 3;
	  wrkopt = Max (i__1, i__2);
	}
      else
	{
	  /* 
	   *          Use UdU' or LdL' factorization, first restoring the saved 
	   *          triangle. 
	   * 
	   */
	  i__1 = *ldr + 1;
	  C2F (dcopy) (m, &dwork[1], &c__1, &r__[r_offset], &i__1);
	  if (luplou)
	    {
	      /* 
	       */
	      i__1 = *m;
	      for (j = 2; j <= i__1; ++j)
		{
		  i__2 = j - 1;
		  C2F (dcopy) (&i__2, &r__[j + r_dim1], ldr,
			       &r__[j * r_dim1 + 1], &c__1);
		  /* L170: */
		}
	      /* 
	       */
	    }
	  else
	    {
	      /* 
	       */
	      i__1 = *m;
	      for (j = 2; j <= i__1; ++j)
		{
		  i__2 = j - 1;
		  C2F (dcopy) (&i__2, &r__[j * r_dim1 + 1], &c__1,
			       &r__[j + r_dim1], ldr);
		  /* L180: */
		}
	      /* 
	       */
	    }
	  /* 
	   *          Workspace: need   1, 
	   *                     prefer M*NB. 
	   * 
	   */
	  C2F (dsytrf) (uplo, m, &r__[r_offset], ldr, &ipiv[1], &dwork[1],
			ldwork, info, 1L);
	  oufact[1] = 2;
	  if (*info > 0)
	    {
	      return 0;
	    }
	  /*Computing MAX 
	   */
	  i__1 = wrkopt, i__2 = (int) dwork[1];
	  wrkopt = Max (i__1, i__2);
	  /* 
	   *          Compute the reciprocal of the condition number of R. 
	   *          Workspace: need   2*M. 
	   * 
	   */
	  C2F (dsycon) (uplo, m, &r__[r_offset], ldr, &ipiv[1], &rnormp,
			&rcond, &dwork[1], &iwork[1], info, 1L);
	  /* 
	   *          Return if the matrix is singular to working precision. 
	   * 
	   */
	  dwork[2] = rcond;
	  if (rcond < eps)
	    {
	      *info = *m + 1;
	      return 0;
	    }
	}
    }
  /* 
   */
  if (oufact[1] == 1)
    {
      /* 
       *       Solve the positive definite linear system. 
       * 
       */
      C2F (dpotrs) (uplo, m, n, &r__[r_offset], ldr, &f[f_offset], ldf, info,
		    1L);
    }
  else
    {
      /* 
       *       Solve the indefinite linear system. 
       * 
       */
      C2F (dsytrs) (uplo, m, n, &r__[r_offset], ldr, &ipiv[1], &f[f_offset],
		    ldf, info, 1L);
    }
  /* 
   *    Set the optimal workspace. 
   * 
   */
  dwork[1] = (double) wrkopt;
  /* 
   */
  return 0;
  /**** Last line of SB02ND *** 
   */
}				/* nsp_slicot_sb02nd */
