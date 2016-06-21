/* MB03OY.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static int c__2 = 2;

/* Subroutine */ int
nsp_slicot_mb03oy (int *m, int *n, double *a, int *lda,
		   double *rcond, double *svlmax, int *rank,
		   double *sval, int *jpvt, double *tau, double *dwork,
		   int *info)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;
  double d__1, d__2;

  /* Builtin functions */
  

  /* Local variables */
  double smin, temp, smax;
  double temp2;
  int i__, j;
  int itemp, ismin;
  int ismax;
  double c1, c2;
  double s1, s2;
  int mn;
  double sminpr, smaxpr, aii;
  int pvt;

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
   *    To compute a rank-revealing QR factorization of a real general 
   *    M-by-N matrix  A,  which may be rank-deficient, and estimate its 
   *    effective rank using incremental condition estimation. 
   * 
   *    The routine uses a truncated QR factorization with column pivoting 
   *                                  [ R11 R12 ] 
   *       A * P = Q * R,  where  R = [         ], 
   *                                  [  0  R22 ] 
   *    with R11 defined as the largest leading upper triangular submatrix 
   *    whose estimated condition number is less than 1/RCOND.  The order 
   *    of R11, RANK, is the effective rank of A.  Condition estimation is 
   *    performed during the QR factorization process.  Matrix R22 is full 
   *    (but of small norm), or empty. 
   * 
   *    MB03OY  does not perform any scaling of the matrix A. 
   * 
   *    ARGUMENTS 
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
   *            On entry, the leading M-by-N part of this array must 
   *            contain the given matrix A. 
   *            On exit, the leading RANK-by-RANK upper triangular part 
   *            of A contains the triangular factor R11, and the elements 
   *            below the diagonal in the first  RANK  columns, with the 
   *            array TAU, represent the orthogonal matrix Q as a product 
   *            of  RANK  elementary reflectors. 
   *            The remaining  N-RANK  columns contain the result of the 
   *            QR factorization process used. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= Max(1,M). 
   * 
   *    RCOND   (input) DOUBLE PRECISION 
   *            RCOND is used to determine the effective rank of A, which 
   *            is defined as the order of the largest leading triangular 
   *            submatrix R11 in the QR factorization with pivoting of A, 
   *            whose estimated condition number is less than 1/RCOND. 
   *            0 <= RCOND <= 1. 
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
   *    RANK    (output) INT 
   *            The effective (estimated) rank of A, i.e., the order of 
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
   *    JPVT    (output) INT array, dimension ( N ) 
   *            If JPVT(i) = k, then the i-th column of A*P was the k-th 
   *            column of A. 
   * 
   *    TAU     (output) DOUBLE PRECISION array, dimension ( MIN( M, N ) ) 
   *            The leading  RANK  elements of TAU contain the scalar 
   *            factors of the elementary reflectors. 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension ( 3*N-1 ) 
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
   *    The routine computes a truncated QR factorization with column 
   *    pivoting of A,  A * P = Q * R,  with  R  defined above, and, 
   *    during this process, finds the largest leading submatrix whose 
   *    estimated condition number is less than 1/RCOND, taking the 
   *    possible positive value of SVLMAX into account.  This is performed 
   *    using the LAPACK incremental condition estimation scheme and a 
   *    slightly modified rank decision test.  The factorization process 
   *    stops when  RANK  has been determined. 
   * 
   *    The matrix Q is represented as a product of elementary reflectors 
   * 
   *       Q = H(1) H(2) . . . H(k), where k = rank <= Min(m,n). 
   * 
   *    Each H(i) has the form 
   * 
   *       H = I - tau * v * v' 
   * 
   *    where tau is a real scalar, and v is a real vector with 
   *    v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in 
   *    A(i+1:m,i), and tau in TAU(i). 
   * 
   *    The matrix P is represented in jpvt as follows: If 
   *       jpvt(j) = i 
   *    then the jth column of P is the ith canonical unit vector. 
   * 
   *    REFERENCES 
   * 
   *    [1] Bischof, C.H. and P. Tang. 
   *        Generalizing Incremental Condition Estimation. 
   *        LAPACK Working Notes 32, Mathematics and Computer Science 
   *        Division, Argonne National Laboratory, UT, CS-91-132, 
   *        May 1991. 
   * 
   *    [2] Bischof, C.H. and P. Tang. 
   *        Robust Incremental Condition Estimation. 
   *        LAPACK Working Notes 33, Mathematics and Computer Science 
   *        Division, Argonne National Laboratory, UT, CS-91-133, 
   *        May 1991. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The algorithm is backward stable. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, Feb. 1998. 
   * 
   *    REVISIONS 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Jan. 2009. 
   * 
   *    KEYWORDS 
   * 
   *    Eigenvalue problem, matrix operations, orthogonal transformation, 
   *    singular values. 
   * 
   *   ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. 
   *    .. Local Scalars .. 
   *    .. 
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
   *    .. 
   *    .. Executable Statements .. 
   * 
   *    Test the input scalar arguments. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --sval;
  --jpvt;
  --tau;
  --dwork;

  /* Function Body */
  *info = 0;
  if (*m < 0)
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*lda < Max (1, *m))
    {
      *info = -4;
    }
  else if (*rcond < 0. || *rcond > 1.)
    {
      *info = -5;
    }
  else if (*svlmax < 0.)
    {
      *info = -6;
    }
  /* 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("MB03OY", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  mn = Min (*m, *n);
  if (mn == 0)
    {
      *rank = 0;
      sval[1] = 0.;
      sval[2] = 0.;
      sval[3] = 0.;
      return 0;
    }
  /* 
   */
  ismin = 1;
  ismax = ismin + *n;
  /* 
   *    Initialize partial column norms and pivoting vector. The first n 
   *    elements of DWORK store the exact column norms. The already used 
   *    leading part is then overwritten by the condition estimator. 
   * 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      dwork[i__] = C2F (dnrm2) (m, &a[i__ * a_dim1 + 1], &c__1);
      dwork[*n + i__] = dwork[i__];
      jpvt[i__] = i__;
      /* L10: */
    }
  /* 
   *    Compute factorization and determine RANK using incremental 
   *    condition estimation. 
   * 
   */
  *rank = 0;
  /* 
   */
 L20:
  if (*rank < mn)
    {
      i__ = *rank + 1;
      /* 
       *       Determine ith pivot column and swap if necessary. 
       * 
       */
      i__1 = *n - i__ + 1;
      pvt = i__ - 1 + C2F (idamax) (&i__1, &dwork[i__], &c__1);
      /* 
       */
      if (pvt != i__)
	{
	  C2F (dswap) (m, &a[pvt * a_dim1 + 1], &c__1, &a[i__ * a_dim1 + 1],
		       &c__1);
	  itemp = jpvt[pvt];
	  jpvt[pvt] = jpvt[i__];
	  jpvt[i__] = itemp;
	  dwork[pvt] = dwork[i__];
	  dwork[*n + pvt] = dwork[*n + i__];
	}
      /* 
       *       Save A(I,I) and generate elementary reflector H(i). 
       * 
       */
      if (i__ < *m)
	{
	  aii = a[i__ + i__ * a_dim1];
	  i__1 = *m - i__ + 1;
	  C2F (dlarfg) (&i__1, &a[i__ + i__ * a_dim1],
			&a[i__ + 1 + i__ * a_dim1], &c__1, &tau[i__]);
	}
      else
	{
	  tau[*m] = 0.;
	}
      /* 
       */
      if (*rank == 0)
	{
	  /* 
	   *          Initialize; exit if matrix is zero (RANK = 0). 
	   * 
	   */
	  smax = (d__1 = a[a_dim1 + 1], Abs (d__1));
	  if (smax == 0.)
	    {
	      sval[1] = 0.;
	      sval[2] = 0.;
	      sval[3] = 0.;
	      return 0;
	    }
	  smin = smax;
	  smaxpr = smax;
	  sminpr = smin;
	  c1 = 1.;
	  c2 = 1.;
	}
      else
	{
	  /* 
	   *          One step of incremental condition estimation. 
	   * 
	   */
	  C2F (dlaic1) (&c__2, rank, &dwork[ismin], &smin,
			&a[i__ * a_dim1 + 1], &a[i__ + i__ * a_dim1], &sminpr,
			&s1, &c1);
	  C2F (dlaic1) (&c__1, rank, &dwork[ismax], &smax,
			&a[i__ * a_dim1 + 1], &a[i__ + i__ * a_dim1], &smaxpr,
			&s2, &c2);
	}
      /* 
       */
      if (*svlmax * *rcond <= smaxpr)
	{
	  if (*svlmax * *rcond <= sminpr)
	    {
	      if (smaxpr * *rcond <= sminpr)
		{
		  /* 
		   *                Continue factorization, as rank is at least RANK. 
		   * 
		   */
		  if (i__ < *n)
		    {
		      /* 
		       *                   Apply H(i) to A(i:m,i+1:n) from the left. 
		       * 
		       */
		      aii = a[i__ + i__ * a_dim1];
		      a[i__ + i__ * a_dim1] = 1.;
		      i__1 = *m - i__ + 1;
		      i__2 = *n - i__;
		      C2F (dlarf) ("Left", &i__1, &i__2,
				   &a[i__ + i__ * a_dim1], &c__1, &tau[i__],
				   &a[i__ + (i__ + 1) * a_dim1], lda,
				   &dwork[(*n << 1) + 1], 4L);
		      a[i__ + i__ * a_dim1] = aii;
		    }
		  /* 
		   *                Update partial column norms. 
		   * 
		   */
		  i__1 = *n;
		  for (j = i__ + 1; j <= i__1; ++j)
		    {
		      if (dwork[j] != 0.)
			{
			  /*Computing 2nd power 
			   */
			  d__2 = (d__1 =
				  a[i__ + j * a_dim1], Abs (d__1)) / dwork[j];
			  temp = 1. - d__2 * d__2;
			  temp = Max (temp, 0.);
			  /*Computing 2nd power 
			   */
			  d__1 = dwork[j] / dwork[*n + j];
			  temp2 = temp * .05 * (d__1 * d__1) + 1.;
			  if (temp2 == 1.)
			    {
			      if (*m - i__ > 0)
				{
				  i__2 = *m - i__;
				  dwork[j] =
				    C2F (dnrm2) (&i__2,
						 &a[i__ + 1 + j * a_dim1],
						 &c__1);
				  dwork[*n + j] = dwork[j];
				}
			      else
				{
				  dwork[j] = 0.;
				  dwork[*n + j] = 0.;
				}
			    }
			  else
			    {
			      dwork[j] *= sqrt (temp);
			    }
			}
		      /* L30: */
		    }
		  /* 
		   */
		  i__1 = *rank;
		  for (i__ = 1; i__ <= i__1; ++i__)
		    {
		      dwork[ismin + i__ - 1] = s1 * dwork[ismin + i__ - 1];
		      dwork[ismax + i__ - 1] = s2 * dwork[ismax + i__ - 1];
		      /* L40: */
		    }
		  /* 
		   */
		  dwork[ismin + *rank] = c1;
		  dwork[ismax + *rank] = c2;
		  smin = sminpr;
		  smax = smaxpr;
		  ++(*rank);
		  goto L20;
		}
	    }
	}
    }
  /* 
   *    Restore the changed part of the (RANK+1)-th column and set SVAL. 
   * 
   */
  if (*rank < *n)
    {
      if (i__ < *m)
	{
	  i__1 = *m - i__;
	  d__1 = -a[i__ + i__ * a_dim1] * tau[i__];
	  C2F (dscal) (&i__1, &d__1, &a[i__ + 1 + i__ * a_dim1], &c__1);
	  a[i__ + i__ * a_dim1] = aii;
	}
    }
  if (*rank == 0)
    {
      smin = 0.;
      sminpr = 0.;
    }
  sval[1] = smax;
  sval[2] = smin;
  sval[3] = sminpr;
  /* 
   */
  return 0;
  /**** Last line of MB03OY *** 
   */
}				/* nsp_slicot_mb03oy */
