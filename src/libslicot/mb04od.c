/* MB04OD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;

/* Subroutine */ int
nsp_slicot_mb04od (char *uplo, int *n, int *m, int *p, double *r__,
		   int *ldr, double *a, int *lda, double *b,
		   int *ldb, double *c__, int *ldc, double *tau,
		   double *dwork, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, r_dim1,
    r_offset, i__1, i__2;

  /* Local variables */
  int i__;
  int luplo;
  int im;

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
   *    To calculate a QR factorization of the first block column and 
   *    apply the orthogonal transformations (from the left) also to the 
   *    second block column of a structured matrix, as follows 
   *                         _   _ 
   *           [ R   B ]   [ R   B ] 
   *      Q' * [       ] = [     _ ] 
   *           [ A   C ]   [ 0   C ] 
   *                _ 
   *    where R and R are upper triangular. The matrix A can be full or 
   *    upper trapezoidal/triangular. The problem structure is exploited. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    UPLO    CHARACTER*1 
   *            Indicates if the matrix A is or not triangular as follows: 
   *            = 'U':  Matrix A is upper trapezoidal/triangular; 
   *            = 'F':  Matrix A is full. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT                 _ 
   *            The order of the matrices R and R.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The number of columns of the matrices B and C.  M >= 0. 
   * 
   *    P       (input) INT 
   *            The number of rows of the matrices A and C.  P >= 0. 
   * 
   *    R       (input/output) DOUBLE PRECISION array, dimension (LDR,N) 
   *            On entry, the leading N-by-N upper triangular part of this 
   *            array must contain the upper triangular matrix R. 
   *            On exit, the leading N-by-N upper triangular part of this 
   *                                                       _ 
   *            array contains the upper triangular matrix R. 
   *            The strict lower triangular part of this array is not 
   *            referenced. 
   * 
   *    LDR     INT 
   *            The leading dimension of array R.  LDR >= MAX(1,N). 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) 
   *            On entry, if UPLO = 'F', the leading P-by-N part of this 
   *            array must contain the matrix A. If UPLO = 'U', the 
   *            leading MIN(P,N)-by-N part of this array must contain the 
   *            upper trapezoidal (upper triangular if P >= N) matrix A, 
   *            and the elements below the diagonal are not referenced. 
   *            On exit, the leading P-by-N part (upper trapezoidal or 
   *            triangular, if UPLO = 'U') of this array contains the 
   *            trailing components (the vectors v, see Method) of the 
   *            elementary reflectors used in the factorization. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,P). 
   * 
   *    B       (input/output) DOUBLE PRECISION array, dimension (LDB,M) 
   *            On entry, the leading N-by-M part of this array must 
   *            contain the matrix B. 
   *            On exit, the leading N-by-M part of this array contains 
   *                                _ 
   *            the computed matrix B. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,N). 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,M) 
   *            On entry, the leading P-by-M part of this array must 
   *            contain the matrix C. 
   *            On exit, the leading P-by-M part of this array contains 
   *                                _ 
   *            the computed matrix C. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C.  LDC >= MAX(1,P). 
   * 
   *    TAU     (output) DOUBLE PRECISION array, dimension (N) 
   *            The scalar factors of the elementary reflectors used. 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (MAX(N-1,M)) 
   * 
   *    METHOD 
   * 
   *    The routine uses N Householder transformations exploiting the zero 
   *    pattern of the block matrix.  A Householder matrix has the form 
   * 
   *                                    ( 1 ) 
   *       H  = I - tau *u *u',    u  = ( v ), 
   *        i          i  i  i      i   (  i) 
   * 
   *    where v  is a P-vector, if UPLO = 'F', or a Min(i,P)-vector, if 
   *           i 
   *    UPLO = 'U'.  The components of v  are stored in the i-th column 
   *                                    i 
   *    of A, and tau  is stored in TAU(i). 
   *                 i 
   *    In-line code for applying Householder transformations is used 
   *    whenever possible (see MB04OY routine). 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The algorithm is backward stable. 
   * 
   *    CONTRIBUTORS 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, Feb. 1997. 
   * 
   *    REVISIONS 
   * 
   *    Dec. 1997. 
   * 
   *    KEYWORDS 
   * 
   *    Elementary reflector, QR factorization, orthogonal transformation. 
   * 
   *    ****************************************************************** 
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
   *    For efficiency reasons, the parameters are not checked. 
   * 
   */
  /* Parameter adjustments */
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  --tau;
  --dwork;

  /* Function Body */
  if (Min (*n, *p) == 0)
    {
      return 0;
    }
  /* 
   */
  luplo = C2F (lsame) (uplo, "U", 1L, 1L);
  if (luplo)
    {
      /* 
       */
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  /* 
	   *          Annihilate the I-th column of A and apply the 
	   *          transformations to the entire block matrix, exploiting 
	   *          its structure. 
	   * 
	   */
	  im = Min (i__, *p);
	  i__2 = im + 1;
	  C2F (dlarfg) (&i__2, &r__[i__ + i__ * r_dim1], &a[i__ * a_dim1 + 1],
			&c__1, &tau[i__]);
	  /* 
	   *          Compute 
	   *                          [ R(I,I+1:N)    ] 
	   *          w := [ 1 v' ] * [               ], 
	   *                          [ A(1:IM,I+1:N) ] 
	   * 
	   *          [ R(I,I+1:N)    ]    [ R(I,I+1:N)    ]         [ 1 ] 
	   *          [               ] := [               ] - tau * [   ] * w . 
	   *          [ A(1:IM,I+1:N) ]    [ A(1:IM,I+1:N) ]         [ v ] 
	   * 
	   */
	  if (*n - i__ > 0)
	    {
	      i__2 = *n - i__;
	      nsp_slicot_mb04oy (&im, &i__2, &a[i__ * a_dim1 + 1], &tau[i__],
				 &r__[i__ + (i__ + 1) * r_dim1], ldr,
				 &a[(i__ + 1) * a_dim1 + 1], lda, &dwork[1]);
	    }
	  /* 
	   *          Compute 
	   *                          [  B(I,:)   ] 
	   *          w := [ 1 v' ] * [           ], 
	   *                          [ C(1:IM,:) ] 
	   * 
	   *          [   B(I,:)  ]    [  B(I,:)   ]         [ 1 ] 
	   *          [           ] := [           ] - tau * [   ] * w. 
	   *          [ C(1:IM,:) ]    [ C(1:IM,:) ]         [ v ] 
	   * 
	   * 
	   */
	  if (*m > 0)
	    {
	      nsp_slicot_mb04oy (&im, m, &a[i__ * a_dim1 + 1], &tau[i__],
				 &b[i__ + b_dim1], ldb, &c__[c_offset], ldc,
				 &dwork[1]);
	    }
	  /* L10: */
	}
      /* 
       */
    }
  else
    {
      /* 
       */
      i__1 = *n - 1;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  /* 
	   *          Annihilate the I-th column of A and apply the 
	   *          transformations to the first block column, exploiting its 
	   *          structure. 
	   * 
	   */
	  i__2 = *p + 1;
	  C2F (dlarfg) (&i__2, &r__[i__ + i__ * r_dim1], &a[i__ * a_dim1 + 1],
			&c__1, &tau[i__]);
	  /* 
	   *          Compute 
	   *                          [ R(I,I+1:N) ] 
	   *          w := [ 1 v' ] * [            ], 
	   *                          [ A(:,I+1:N) ] 
	   * 
	   *          [ R(I,I+1:N) ]    [ R(I,I+1:N) ]         [ 1 ] 
	   *          [            ] := [            ] - tau * [   ] * w . 
	   *          [ A(:,I+1:N) ]    [ A(:,I+1:N) ]         [ v ] 
	   * 
	   */
	  i__2 = *n - i__;
	  nsp_slicot_mb04oy (p, &i__2, &a[i__ * a_dim1 + 1], &tau[i__],
			     &r__[i__ + (i__ + 1) * r_dim1], ldr,
			     &a[(i__ + 1) * a_dim1 + 1], lda, &dwork[1]);
	  /* L20: */
	}
      /* 
       */
      i__1 = *p + 1;
      C2F (dlarfg) (&i__1, &r__[*n + *n * r_dim1], &a[*n * a_dim1 + 1], &c__1,
		    &tau[*n]);
      if (*m > 0)
	{
	  /* 
	   *          Apply the transformations to the second block column. 
	   * 
	   */
	  i__1 = *n;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      /* 
	       *             Compute 
	       *                             [ B(I,:) ] 
	       *             w := [ 1 v' ] * [        ], 
	       *                             [   C    ] 
	       * 
	       *             [ B(I,:) ]    [ B(I,:) ]         [ 1 ] 
	       *             [        ] := [        ] - tau * [   ] * w. 
	       *             [   C    ]    [   C    ]         [ v ] 
	       * 
	       */
	      nsp_slicot_mb04oy (p, m, &a[i__ * a_dim1 + 1], &tau[i__],
				 &b[i__ + b_dim1], ldb, &c__[c_offset], ldc,
				 &dwork[1]);
	      /* L30: */
	    }
	  /* 
	   */
	}
    }
  return 0;
  /**** Last line of MB04OD *** 
   */
}				/* nsp_slicot_mb04od */
