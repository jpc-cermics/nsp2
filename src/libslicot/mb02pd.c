/* MB02PD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Subroutine */ int
nsp_slicot_mb02pd (char *fact, char *trans, int *n, int *nrhs, double *a,
		   int *lda, double *af, int *ldaf, int *ipiv,
		   char *equed, double *r__, double *c__, double *b,
		   int *ldb, double *x, int *ldx, double *rcond,
		   double *ferr, double *berr, int *iwork,
		   double *dwork, int *info, long int fact_len,
		   long int trans_len, long int equed_len)
{
  /* System generated locals */
  int a_dim1, a_offset, af_dim1, af_offset, b_dim1, b_offset, x_dim1,
    x_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  double amax;
  char norm[1];
  int i__, j;
  double rcmin, rcmax, anorm;
  int equil;
  double colcnd;
  int nofact;
  double bignum;
  int infequ;
  int colequ;
  double rowcnd;
  int notran;
  double smlnum;
  int rowequ;
  static double rpvgrw;

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
   *    To solve (if well-conditioned) the matrix equations 
   * 
   *       op( A )*X = B, 
   * 
   *    where X and B are N-by-NRHS matrices, A is an N-by-N matrix and 
   *    op( A ) is one of 
   * 
   *       op( A ) = A   or   op( A ) = A'. 
   * 
   *    Error bounds on the solution and a condition estimate are also 
   *    provided. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    FACT    CHARACTER*1 
   *            Specifies whether or not the factored form of the matrix A 
   *            is supplied on entry, and if not, whether the matrix A 
   *            should be equilibrated before it is factored. 
   *            = 'F':  On entry, AF and IPIV contain the factored form 
   *                    of A. If EQUED is not 'N', the matrix A has been 
   *                    equilibrated with scaling factors given by R 
   *                    and C. A, AF, and IPIV are not modified. 
   *            = 'N':  The matrix A will be copied to AF and factored. 
   *            = 'E':  The matrix A will be equilibrated if necessary, 
   *                    then copied to AF and factored. 
   * 
   *    TRANS   CHARACTER*1 
   *            Specifies the form of the system of equations as follows: 
   *            = 'N':  A * X = B     (No transpose); 
   *            = 'T':  A**T * X = B  (Transpose); 
   *            = 'C':  A**H * X = B  (Transpose). 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The number of linear equations, i.e., the order of the 
   *            matrix A.  N >= 0. 
   * 
   *    NRHS    (input) INT 
   *            The number of right hand sides, i.e., the number of 
   *            columns of the matrices B and X.  NRHS >= 0. 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) 
   *            On entry, the leading N-by-N part of this array must 
   *            contain the matrix A.  If FACT = 'F' and EQUED is not 'N', 
   *            then A must have been equilibrated by the scaling factors 
   *            in R and/or C.  A is not modified if FACT = 'F' or 'N', 
   *            or if FACT = 'E' and EQUED = 'N' on exit. 
   *            On exit, if EQUED .NE. 'N', the leading N-by-N part of 
   *            this array contains the matrix A scaled as follows: 
   *            EQUED = 'R':  A := diag(R) * A; 
   *            EQUED = 'C':  A := A * diag(C); 
   *            EQUED = 'B':  A := diag(R) * A * diag(C). 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= Max(1,N). 
   * 
   *    AF      (input or output) DOUBLE PRECISION array, dimension 
   *            (LDAF,N) 
   *            If FACT = 'F', then AF is an input argument and on entry 
   *            the leading N-by-N part of this array must contain the 
   *            factors L and U from the factorization A = P*L*U as 
   *            computed by DGETRF.  If EQUED .NE. 'N', then AF is the 
   *            factored form of the equilibrated matrix A. 
   *            If FACT = 'N', then AF is an output argument and on exit 
   *            the leading N-by-N part of this array contains the factors 
   *            L and U from the factorization A = P*L*U of the original 
   *            matrix A. 
   *            If FACT = 'E', then AF is an output argument and on exit 
   *            the leading N-by-N part of this array contains the factors 
   *            L and U from the factorization A = P*L*U of the 
   *            equilibrated matrix A (see the description of A for the 
   *            form of the equilibrated matrix). 
   * 
   *    LDAF    (input) INT 
   *            The leading dimension of the array AF.  LDAF >= Max(1,N). 
   * 
   *    IPIV    (input or output) INT array, dimension (N) 
   *            If FACT = 'F', then IPIV is an input argument and on entry 
   *            it must contain the pivot indices from the factorization 
   *            A = P*L*U as computed by DGETRF; row i of the matrix was 
   *            interchanged with row IPIV(i). 
   *            If FACT = 'N', then IPIV is an output argument and on exit 
   *            it contains the pivot indices from the factorization 
   *            A = P*L*U of the original matrix A. 
   *            If FACT = 'E', then IPIV is an output argument and on exit 
   *            it contains the pivot indices from the factorization 
   *            A = P*L*U of the equilibrated matrix A. 
   * 
   *    EQUED   (input or output) CHARACTER*1 
   *            Specifies the form of equilibration that was done as 
   *            follows: 
   *            = 'N':  No equilibration (always true if FACT = 'N'); 
   *            = 'R':  Row equilibration, i.e., A has been premultiplied 
   *                    by diag(R); 
   *            = 'C':  Column equilibration, i.e., A has been 
   *                    postmultiplied by diag(C); 
   *            = 'B':  Both row and column equilibration, i.e., A has 
   *                    been replaced by diag(R) * A * diag(C). 
   *            EQUED is an input argument if FACT = 'F'; otherwise, it is 
   *            an output argument. 
   * 
   *    R       (input or output) DOUBLE PRECISION array, dimension (N) 
   *            The row scale factors for A.  If EQUED = 'R' or 'B', A is 
   *            multiplied on the left by diag(R); if EQUED = 'N' or 'C', 
   *            R is not accessed.  R is an input argument if FACT = 'F'; 
   *            otherwise, R is an output argument.  If FACT = 'F' and 
   *            EQUED = 'R' or 'B', each element of R must be positive. 
   * 
   *    C       (input or output) DOUBLE PRECISION array, dimension (N) 
   *            The column scale factors for A.  If EQUED = 'C' or 'B', 
   *            A is multiplied on the right by diag(C); if EQUED = 'N' 
   *            or 'R', C is not accessed.  C is an input argument if 
   *            FACT = 'F'; otherwise, C is an output argument.  If 
   *            FACT = 'F' and EQUED = 'C' or 'B', each element of C must 
   *            be positive. 
   * 
   *    B       (input/output) DOUBLE PRECISION array, dimension 
   *            (LDB,NRHS) 
   *            On entry, the leading N-by-NRHS part of this array must 
   *            contain the right-hand side matrix B. 
   *            On exit, 
   *            if EQUED = 'N', B is not modified; 
   *            if TRANS = 'N' and EQUED = 'R' or 'B', the leading 
   *            N-by-NRHS part of this array contains diag(R)*B; 
   *            if TRANS = 'T' or 'C' and EQUED = 'C' or 'B', the leading 
   *            N-by-NRHS part of this array contains diag(C)*B. 
   * 
   *    LDB     INT 
   *            The leading dimension of the array B.  LDB >= Max(1,N). 
   * 
   *    X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS) 
   *            If INFO = 0 or INFO = N+1, the leading N-by-NRHS part of 
   *            this array contains the solution matrix X to the original 
   *            system of equations.  Note that A and B are modified on 
   *            exit if EQUED .NE. 'N', and the solution to the 
   *            equilibrated system is inv(diag(C))*X if TRANS = 'N' and 
   *            EQUED = 'C' or 'B', or inv(diag(R))*X if TRANS = 'T' or 
   *            'C' and EQUED = 'R' or 'B'. 
   * 
   *    LDX     (input) INT 
   *            The leading dimension of the array X.  LDX >= Max(1,N). 
   * 
   *    RCOND   (output) DOUBLE PRECISION 
   *            The estimate of the reciprocal condition number of the 
   *            matrix A after equilibration (if done).  If RCOND is less 
   *            than the machine precision (in particular, if RCOND = 0), 
   *            the matrix is singular to working precision.  This 
   *            condition is indicated by a return code of INFO > 0. 
   *            For efficiency reasons, RCOND is computed only when the 
   *            matrix A is factored, i.e., for FACT = 'N' or 'E'.  For 
   *            FACT = 'F', RCOND is not used, but it is assumed that it 
   *            has been computed and checked before the routine call. 
   * 
   *    FERR    (output) DOUBLE PRECISION array, dimension (NRHS) 
   *            The estimated forward error bound for each solution vector 
   *            X(j) (the j-th column of the solution matrix X). 
   *            If XTRUE is the true solution corresponding to X(j), 
   *            FERR(j) is an estimated upper bound for the magnitude of 
   *            the largest element in (X(j) - XTRUE) divided by the 
   *            magnitude of the largest element in X(j).  The estimate 
   *            is as reliable as the estimate for RCOND, and is almost 
   *            always a slight overestimate of the true error. 
   * 
   *    BERR    (output) DOUBLE PRECISION array, dimension (NRHS) 
   *            The componentwise relative backward error of each solution 
   *            vector X(j) (i.e., the smallest relative change in 
   *            any element of A or B that makes X(j) an exact solution). 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (N) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (4*N) 
   *            On exit, DWORK(1) contains the reciprocal pivot growth 
   *            factor norm(A)/norm(U). The "max absolute element" norm is 
   *            used. If DWORK(1) is much less than 1, then the stability 
   *            of the LU factorization of the (equilibrated) matrix A 
   *            could be poor. This also means that the solution X, 
   *            condition estimator RCOND, and forward error bound FERR 
   *            could be unreliable. If factorization fails with 
   *            0 < INFO <= N, then DWORK(1) contains the reciprocal pivot 
   *            growth factor for the leading INFO columns of A. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            > 0:  if INFO = i, and i is 
   *                  <= N:  U(i,i) is exactly zero.  The factorization 
   *                         has been completed, but the factor U is 
   *                         exactly singular, so the solution and error 
   *                         bounds could not be computed. RCOND = 0 is 
   *                         returned. 
   *                  = N+1: U is nonsingular, but RCOND is less than 
   *                         machine precision, meaning that the matrix is 
   *                         singular to working precision.  Nevertheless, 
   *                         the solution and error bounds are computed 
   *                         because there are a number of situations 
   *                         where the computed solution can be more 
   *                         accurate than the value of RCOND would 
   *                         suggest. 
   *            The positive values for INFO are set only when the 
   *            matrix A is factored, i.e., for FACT = 'N' or 'E'. 
   * 
   *    METHOD 
   * 
   *    The following steps are performed: 
   * 
   *    1. If FACT = 'E', real scaling factors are computed to equilibrate 
   *       the system: 
   * 
   *       TRANS = 'N':  diag(R)*A*diag(C)     *inv(diag(C))*X = diag(R)*B 
   *       TRANS = 'T': (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B 
   *       TRANS = 'C': (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B 
   * 
   *       Whether or not the system will be equilibrated depends on the 
   *       scaling of the matrix A, but if equilibration is used, A is 
   *       overwritten by diag(R)*A*diag(C) and B by diag(R)*B 
   *       (if TRANS='N') or diag(C)*B (if TRANS = 'T' or 'C'). 
   * 
   *    2. If FACT = 'N' or 'E', the LU decomposition is used to factor 
   *       the matrix A (after equilibration if FACT = 'E') as 
   *          A = P * L * U, 
   *       where P is a permutation matrix, L is a unit lower triangular 
   *       matrix, and U is upper triangular. 
   * 
   *    3. If some U(i,i)=0, so that U is exactly singular, then the 
   *       routine returns with INFO = i. Otherwise, the factored form 
   *       of A is used to estimate the condition number of the matrix A. 
   *       If the reciprocal of the condition number is less than machine 
   *       precision, INFO = N+1 is returned as a warning, but the routine 
   *       still goes on to solve for X and compute error bounds as 
   *       described below. 
   * 
   *    4. The system of equations is solved for X using the factored form 
   *       of A. 
   * 
   *    5. Iterative refinement is applied to improve the computed 
   *       solution matrix and calculate error bounds and backward error 
   *       estimates for it. 
   * 
   *    6. If equilibration was used, the matrix X is premultiplied by 
   *       diag(C) (if TRANS = 'N') or diag(R) (if TRANS = 'T' or 'C') so 
   *       that it solves the original system before equilibration. 
   * 
   *    REFERENCES 
   * 
   *    [1] Anderson, E., Bai, Z., Bischof, C., Demmel, J., Dongarra, J., 
   *        Du Croz, J., Greenbaum, A., Hammarling, S., McKenney, A., 
   *        Ostrouchov, S., Sorensen, D. 
   *        LAPACK Users' Guide: Second Edition, SIAM, Philadelphia, 1995. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    This is a simplified version of the LAPACK Library routine DGESVX, 
   *    useful when several sets of matrix equations with the same 
   *    coefficient matrix  A and/or A'  should be solved. 
   * 
   *    NUMERICAL ASPECTS 
   *                              3 
   *    The algorithm requires 0(N ) operations. 
   * 
   *    CONTRIBUTORS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Apr. 1999. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Condition number, matrix algebra, matrix operations. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
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
   *    .. Save Statement .. 
   *    .. 
   *    .. Executable Statements .. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  af_dim1 = *ldaf;
  af_offset = af_dim1 + 1;
  af -= af_offset;
  --ipiv;
  --r__;
  --c__;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  x_dim1 = *ldx;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  --ferr;
  --berr;
  --iwork;
  --dwork;

  /* Function Body */
  *info = 0;
  nofact = C2F (lsame) (fact, "N", 1L, 1L);
  equil = C2F (lsame) (fact, "E", 1L, 1L);
  notran = C2F (lsame) (trans, "N", 1L, 1L);
  if (nofact || equil)
    {
      *(unsigned char *) equed = 'N';
      rowequ = FALSE;
      colequ = FALSE;
    }
  else
    {
      rowequ = C2F (lsame) (equed, "R", 1L, 1L)
	|| C2F (lsame) (equed, "B", 1L, 1L);
      colequ = C2F (lsame) (equed, "C", 1L, 1L)
	|| C2F (lsame) (equed, "B", 1L, 1L);
      smlnum = C2F (dlamch) ("Safe minimum", 12L);
      bignum = 1. / smlnum;
    }
  /* 
   *    Test the input parameters. 
   * 
   */
  if (!nofact && !equil && !C2F (lsame) (fact, "F", 1L, 1L))
    {
      *info = -1;
    }
  else if (!notran && !C2F (lsame) (trans, "T", 1L, 1L)
	   && !C2F (lsame) (trans, "C", 1L, 1L))
    {
      *info = -2;
    }
  else if (*n < 0)
    {
      *info = -3;
    }
  else if (*nrhs < 0)
    {
      *info = -4;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -6;
    }
  else if (*ldaf < Max (1, *n))
    {
      *info = -8;
    }
  else if (C2F (lsame) (fact, "F", 1L, 1L)
	   && !(rowequ || colequ || C2F (lsame) (equed, "N", 1L, 1L)))
    {
      *info = -10;
    }
  else
    {
      if (rowequ)
	{
	  rcmin = bignum;
	  rcmax = 0.;
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      /*Computing MIN 
	       */
	      d__1 = rcmin, d__2 = r__[j];
	      rcmin = Min (d__1, d__2);
	      /*Computing MAX 
	       */
	      d__1 = rcmax, d__2 = r__[j];
	      rcmax = Max (d__1, d__2);
	      /* L10: */
	    }
	  if (rcmin <= 0.)
	    {
	      *info = -11;
	    }
	  else if (*n > 0)
	    {
	      rowcnd = Max (rcmin, smlnum) / Min (rcmax, bignum);
	    }
	  else
	    {
	      rowcnd = 1.;
	    }
	}
      if (colequ && *info == 0)
	{
	  rcmin = bignum;
	  rcmax = 0.;
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      /*Computing MIN 
	       */
	      d__1 = rcmin, d__2 = c__[j];
	      rcmin = Min (d__1, d__2);
	      /*Computing MAX 
	       */
	      d__1 = rcmax, d__2 = c__[j];
	      rcmax = Max (d__1, d__2);
	      /* L20: */
	    }
	  if (rcmin <= 0.)
	    {
	      *info = -12;
	    }
	  else if (*n > 0)
	    {
	      colcnd = Max (rcmin, smlnum) / Min (rcmax, bignum);
	    }
	  else
	    {
	      colcnd = 1.;
	    }
	}
      if (*info == 0)
	{
	  if (*ldb < Max (1, *n))
	    {
	      *info = -14;
	    }
	  else if (*ldx < Max (1, *n))
	    {
	      *info = -16;
	    }
	}
    }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("MB02PD", &i__1, 6L);
      return 0;
    }
  /* 
   */
  if (equil)
    {
      /* 
       *       Compute row and column scalings to equilibrate the matrix A. 
       * 
       */
      C2F (dgeequ) (n, n, &a[a_offset], lda, &r__[1], &c__[1], &rowcnd,
		    &colcnd, &amax, &infequ);
      if (infequ == 0)
	{
	  /* 
	   *          Equilibrate the matrix. 
	   * 
	   */
	  C2F (dlaqge) (n, n, &a[a_offset], lda, &r__[1], &c__[1], &rowcnd,
			&colcnd, &amax, equed, 1L);
	  rowequ = C2F (lsame) (equed, "R", 1L, 1L)
	    || C2F (lsame) (equed, "B", 1L, 1L);
	  colequ = C2F (lsame) (equed, "C", 1L, 1L)
	    || C2F (lsame) (equed, "B", 1L, 1L);
	}
    }
  /* 
   *    Scale the right hand side. 
   * 
   */
  if (notran)
    {
      if (rowequ)
	{
	  i__1 = *nrhs;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  b[i__ + j * b_dim1] = r__[i__] * b[i__ + j * b_dim1];
		  /* L30: */
		}
	      /* L40: */
	    }
	}
    }
  else if (colequ)
    {
      i__1 = *nrhs;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      b[i__ + j * b_dim1] = c__[i__] * b[i__ + j * b_dim1];
	      /* L50: */
	    }
	  /* L60: */
	}
    }
  /* 
   */
  if (nofact || equil)
    {
      /* 
       *       Compute the LU factorization of A. 
       * 
       */
      C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &af[af_offset], ldaf,
		    4L);
      C2F (dgetrf) (n, n, &af[af_offset], ldaf, &ipiv[1], info);
      /* 
       *       Return if INFO is non-zero. 
       * 
       */
      if (*info != 0)
	{
	  if (*info > 0)
	    {
	      /* 
	       *             Compute the reciprocal pivot growth factor of the 
	       *             leading rank-deficient INFO columns of A. 
	       * 
	       */
	      rpvgrw =
		C2F (dlantr) ("M", "U", "N", info, info, &af[af_offset], ldaf,
			      &dwork[1], 1L, 1L, 1L);
	      if (rpvgrw == 0.)
		{
		  rpvgrw = 1.;
		}
	      else
		{
		  rpvgrw =
		    C2F (dlange) ("M", n, info, &a[a_offset], lda, &dwork[1],
				  1L) / rpvgrw;
		}
	      dwork[1] = rpvgrw;
	      *rcond = 0.;
	    }
	  return 0;
	}
      /* 
       *       Compute the norm of the matrix A and the 
       *       reciprocal pivot growth factor RPVGRW. 
       * 
       */
      if (notran)
	{
	  *(unsigned char *) norm = '1';
	}
      else
	{
	  *(unsigned char *) norm = 'I';
	}
      anorm = C2F (dlange) (norm, n, n, &a[a_offset], lda, &dwork[1], 1L);
      rpvgrw =
	C2F (dlantr) ("M", "U", "N", n, n, &af[af_offset], ldaf, &dwork[1],
		      1L, 1L, 1L);
      if (rpvgrw == 0.)
	{
	  rpvgrw = 1.;
	}
      else
	{
	  rpvgrw =
	    C2F (dlange) ("M", n, n, &a[a_offset], lda, &dwork[1],
			  1L) / rpvgrw;
	}
      /* 
       *       Compute the reciprocal of the condition number of A. 
       * 
       */
      C2F (dgecon) (norm, n, &af[af_offset], ldaf, &anorm, rcond, &dwork[1],
		    &iwork[1], info, 1L);
      /* 
       *       Set INFO = N+1 if the matrix is singular to working precision. 
       * 
       */
      if (*rcond < C2F (dlamch) ("Epsilon", 7L))
	{
	  *info = *n + 1;
	}
    }
  /* 
   *    Compute the solution matrix X. 
   * 
   */
  C2F (dlacpy) ("Full", n, nrhs, &b[b_offset], ldb, &x[x_offset], ldx, 4L);
  C2F (dgetrs) (trans, n, nrhs, &af[af_offset], ldaf, &ipiv[1], &x[x_offset],
		ldx, info, 1L);
  /* 
   *    Use iterative refinement to improve the computed solution and 
   *    compute error bounds and backward error estimates for it. 
   * 
   */
  C2F (dgerfs) (trans, n, nrhs, &a[a_offset], lda, &af[af_offset], ldaf,
		&ipiv[1], &b[b_offset], ldb, &x[x_offset], ldx, &ferr[1],
		&berr[1], &dwork[1], &iwork[1], info, 1L);
  /* 
   *    Transform the solution matrix X to a solution of the original 
   *    system. 
   * 
   */
  if (notran)
    {
      if (colequ)
	{
	  i__1 = *nrhs;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  x[i__ + j * x_dim1] = c__[i__] * x[i__ + j * x_dim1];
		  /* L70: */
		}
	      /* L80: */
	    }
	  i__1 = *nrhs;
	  for (j = 1; j <= i__1; ++j)
	    {
	      ferr[j] /= colcnd;
	      /* L90: */
	    }
	}
    }
  else if (rowequ)
    {
      i__1 = *nrhs;
      for (j = 1; j <= i__1; ++j)
	{
	  i__2 = *n;
	  for (i__ = 1; i__ <= i__2; ++i__)
	    {
	      x[i__ + j * x_dim1] = r__[i__] * x[i__ + j * x_dim1];
	      /* L100: */
	    }
	  /* L110: */
	}
      i__1 = *nrhs;
      for (j = 1; j <= i__1; ++j)
	{
	  ferr[j] /= rowcnd;
	  /* L120: */
	}
    }
  /* 
   */
  dwork[1] = rpvgrw;
  return 0;
  /* 
**** Last line of MB02PD *** 
*/
}				/* nsp_slicot_mb02pd */
