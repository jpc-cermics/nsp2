/* MB04ND.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Subroutine */ int
nsp_slicot_mb04nd (char *uplo, int *n, int *m, int *p, double *r__,
		   int *ldr, double *a, int *lda, double *b,
		   int *ldb, double *c__, int *ldc, double *tau,
		   double *dwork, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, r_dim1,
    r_offset, i__1;

  /* Local variables */
  int i__;
  int luplo;
  int im, ip;

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
   *    To calculate an RQ factorization of the first block row and 
   *    apply the orthogonal transformations (from the right) also to the 
   *    second block row of a structured matrix, as follows 
   *                             _ 
   *      [ A   R ]        [ 0   R ] 
   *      [       ] * Q' = [ _   _ ] 
   *      [ C   B ]        [ C   B ] 
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
   *            The number of rows of the matrices B and C.  M >= 0. 
   * 
   *    P       (input) INT 
   *            The number of columns of the matrices A and C.  P >= 0. 
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
   *    A       (input/output) DOUBLE PRECISION array, dimension (LDA,P) 
   *            On entry, if UPLO = 'F', the leading N-by-P part of this 
   *            array must contain the matrix A. For UPLO = 'U', if 
   *            N <= P, the upper triangle of the subarray A(1:N,P-N+1:P) 
   *            must contain the N-by-N upper triangular matrix A, and if 
   *            N >= P, the elements on and above the (N-P)-th subdiagonal 
   *            must contain the N-by-P upper trapezoidal matrix A. 
   *            On exit, if UPLO = 'F', the leading N-by-P part of this 
   *            array contains the trailing components (the vectors v, see 
   *            METHOD) of the elementary reflectors used in the 
   *            factorization. If UPLO = 'U', the upper triangle of the 
   *            subarray A(1:N,P-N+1:P) (if N <= P), or the elements on 
   *            and above the (N-P)-th subdiagonal (if N >= P), contain 
   *            the trailing components (the vectors v, see METHOD) of the 
   *            elementary reflectors used in the factorization. 
   *            The remaining elements are not referenced. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,N). 
   * 
   *    B       (input/output) DOUBLE PRECISION array, dimension (LDB,N) 
   *            On entry, the leading M-by-N part of this array must 
   *            contain the matrix B. 
   *            On exit, the leading M-by-N part of this array contains 
   *                                _ 
   *            the computed matrix B. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,M). 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,P) 
   *            On entry, the leading M-by-P part of this array must 
   *            contain the matrix C. 
   *            On exit, the leading M-by-P part of this array contains 
   *                                _ 
   *            the computed matrix C. 
   * 
   *    LDC     INT 
   *            The leading dimension of array C.  LDC >= MAX(1,M). 
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
   *    where v  is a P-vector, if UPLO = 'F', or a Min(N-i+1,P)-vector, 
   *           i 
   *    if UPLO = 'U'.  The components of v  are stored in the i-th row 
   *                                       i 
   *    of A, and tau  is stored in TAU(i), i = N,N-1,...,1. 
   *                 i 
   *    In-line code for applying Householder transformations is used 
   *    whenever possible (see MB04NY routine). 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The algorithm is backward stable. 
   * 
   *    CONTRIBUTORS 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, Apr. 1998. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Elementary reflector, RQ factorization, orthogonal transformation. 
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
      for (i__ = *n; i__ >= 1; --i__)
	{
	  /* 
	   *          Annihilate the I-th row of A and apply the transformations 
	   *          to the entire block matrix, exploiting its structure. 
	   * 
	   *Computing MIN 
	   */
	  i__1 = *n - i__ + 1;
	  im = Min (i__1, *p);
	  /*Computing MAX 
	   */
	  i__1 = *p - *n + i__;
	  ip = Max (i__1, 1);
	  i__1 = im + 1;
	  C2F (dlarfg) (&i__1, &r__[i__ + i__ * r_dim1],
			&a[i__ + ip * a_dim1], lda, &tau[i__]);
	  /* 
	   *          Compute 
	   *                                               [ 1 ] 
	   *          w := [ R(1:I-1,I)  A(1:I-1,IP:P) ] * [   ], 
	   *                                               [ v ] 
	   * 
	   *          [ R(1:I-1,I)  A(1:I-1,IP:P) ] = 
	   *          [ R(1:I-1,I)  A(1:I-1,IP:P) ] - tau * w * [ 1 v' ]. 
	   * 
	   */
	  if (i__ > 0)
	    {
	      i__1 = i__ - 1;
	      nsp_slicot_mb04ny (&i__1, &im, &a[i__ + ip * a_dim1], lda,
				 &tau[i__], &r__[i__ * r_dim1 + 1], ldr,
				 &a[ip * a_dim1 + 1], lda, &dwork[1]);
	    }
	  /* 
	   * 
	   *          Compute 
	   *                                       [ 1 ] 
	   *          w := [ B(:,I)  C(:,IP:P) ] * [   ], 
	   *                                       [ v ] 
	   * 
	   *          [ B(:,I)  C(:,IP:P) ] = [ B(:,I)  C(:,IP:P) ] - 
	   *                                  tau * w * [ 1 v' ]. 
	   * 
	   */
	  if (*m > 0)
	    {
	      nsp_slicot_mb04ny (m, &im, &a[i__ + ip * a_dim1], lda,
				 &tau[i__], &b[i__ * b_dim1 + 1], ldb,
				 &c__[ip * c_dim1 + 1], ldc, &dwork[1]);
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
      for (i__ = *n; i__ >= 2; --i__)
	{
	  /* 
	   *          Annihilate the I-th row of A and apply the transformations 
	   *          to the first block row, exploiting its structure. 
	   * 
	   */
	  i__1 = *p + 1;
	  C2F (dlarfg) (&i__1, &r__[i__ + i__ * r_dim1], &a[i__ + a_dim1],
			lda, &tau[i__]);
	  /* 
	   *          Compute 
	   *                                            [ 1 ] 
	   *          w := [ R(1:I-1,I)  A(1:I-1,:) ] * [   ], 
	   *                                            [ v ] 
	   * 
	   *          [ R(1:I-1,I)  A(1:I-1,:) ] = [ R(1:I-1,I)  A(1:I-1,:) ] - 
	   *                                       tau * w * [ 1 v' ]. 
	   * 
	   */
	  i__1 = i__ - 1;
	  nsp_slicot_mb04ny (&i__1, p, &a[i__ + a_dim1], lda, &tau[i__],
			     &r__[i__ * r_dim1 + 1], ldr, &a[a_offset], lda,
			     &dwork[1]);
	  /* L20: */
	}
      /* 
       */
      i__1 = *p + 1;
      C2F (dlarfg) (&i__1, &r__[r_dim1 + 1], &a[a_dim1 + 1], lda, &tau[1]);
      if (*m > 0)
	{
	  /* 
	   *          Apply the transformations to the second block row. 
	   * 
	   */
	  for (i__ = *n; i__ >= 1; --i__)
	    {
	      /* 
	       *             Compute 
	       *                                  [ 1 ] 
	       *             w := [ B(:,I)  C ] * [   ], 
	       *                                  [ v ] 
	       * 
	       *             [ B(:,I)  C ] = [ B(:,I)  C ] - tau * w * [ 1 v' ]. 
	       * 
	       */
	      nsp_slicot_mb04ny (m, p, &a[i__ + a_dim1], lda, &tau[i__],
				 &b[i__ * b_dim1 + 1], ldb, &c__[c_offset],
				 ldc, &dwork[1]);
	      /* L30: */
	    }
	  /* 
	   */
	}
    }
  return 0;
  /**** Last line of MB04ND *** 
   */
}				/* nsp_slicot_mb04nd */
