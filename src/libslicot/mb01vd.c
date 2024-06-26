/* MB01VD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b10 = 0.;
static int c__1 = 1;
static int c__0 = 0;

/* Subroutine */ int
nsp_slicot_mb01vd (char *trana, char *tranb, int *ma, int *na, int *mb,
		   int *nb, double *alpha, double *beta, double *a,
		   int *lda, double *b, int *ldb, double *c__,
		   int *ldc, int *mc, int *nc, int *info,
		   long int trana_len, long int tranb_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2,
    i__3, i__4;

  /* Local variables */
  int i__, j, k, l;
  int ic, jc, lc, nz;
  int transa, transb, sparse;
  double aij, dum[1];

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
   *    To perform the following matrix operation 
   * 
   *       C = alpha*kron( op(A), op(B) ) + beta*C, 
   * 
   *    where alpha and beta are real scalars, op(M) is either matrix M or 
   *    its transpose, M', and kron( X, Y ) denotes the Kronecker product 
   *    of the matrices X and Y. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    TRANA   CHARACTER*1 
   *            Specifies the form of op(A) to be used as follows: 
   *            = 'N':  op(A) = A; 
   *            = 'T':  op(A) = A'; 
   *            = 'C':  op(A) = A'. 
   * 
   *    TRANB   CHARACTER*1 
   *            Specifies the form of op(B) to be used as follows: 
   *            = 'N':  op(B) = B; 
   *            = 'T':  op(B) = B'; 
   *            = 'C':  op(B) = B'. 
   * 
   *    Input/Output Parameters 
   * 
   *    MA      (input) INT 
   *            The number of rows of the matrix op(A).  MA >= 0. 
   * 
   *    NA      (input) INT 
   *            The number of columns of the matrix op(A).  NA >= 0. 
   * 
   *    MB      (input) INT 
   *            The number of rows of the matrix op(B).  MB >= 0. 
   * 
   *    NB      (input) INT 
   *            The number of columns of the matrix op(B).  NB >= 0. 
   * 
   *    ALPHA   (input) DOUBLE PRECISION 
   *            The scalar alpha. When alpha is zero then A and B need not 
   *            be set before entry. 
   * 
   *    BETA    (input) DOUBLE PRECISION 
   *            The scalar beta. When beta is zero then C need not be 
   *            set before entry. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,ka), 
   *            where ka is NA when TRANA = 'N', and is MA otherwise. 
   *            If TRANA = 'N', the leading MA-by-NA part of this array 
   *            must contain the matrix A; otherwise, the leading NA-by-MA 
   *            part of this array must contain the matrix A. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A. 
   *            LDA >= Max(1,MA), if TRANA = 'N'; 
   *            LDA >= Max(1,NA), if TRANA = 'T' or 'C'. 
   * 
   *    B       (input) DOUBLE PRECISION array, dimension (LDB,kb) 
   *            where kb is NB when TRANB = 'N', and is MB otherwise. 
   *            If TRANB = 'N', the leading MB-by-NB part of this array 
   *            must contain the matrix B; otherwise, the leading NB-by-MB 
   *            part of this array must contain the matrix B. 
   * 
   *    LDB     INT 
   *            The leading dimension of the array B. 
   *            LDB >= Max(1,MB), if TRANB = 'N'; 
   *            LDB >= Max(1,NB), if TRANB = 'T' or 'C'. 
   * 
   *    C       (input/output) DOUBLE PRECISION array, dimension (LDC,NC) 
   *            On entry, if beta is nonzero, the leading MC-by-NC part of 
   *            this array must contain the given matric C, where 
   *            MC = MA*MB and NC = NA*NB. 
   *            On exit, the leading MC-by-NC part of this array contains 
   *            the computed matrix expression 
   *            C = alpha*kron( op(A), op(B) ) + beta*C. 
   * 
   *    LDC     INT 
   *            The leading dimension of the array C. 
   *            LDC >= Max(1,MC). 
   * 
   *    MC      (output) INT 
   *            The number of rows of the matrix C.  MC = MA*MB. 
   * 
   *    NC      (output) INT 
   *            The number of columns of the matrix C.  NC = NA*NB. 
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
   *    The Kronecker product of the matrices op(A) and op(B) is computed 
   *    column by column. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    The multiplications by zero elements in A are avoided, if the 
   *    matrix A is considered to be sparse, i.e., if 
   *    (number of zeros in A)/(MA*NA) >= SPARST = 0.8. The code makes 
   *    NB+1 passes through the matrix A, and MA*NA passes through the 
   *    matrix B. If LDA and/or LDB are very large, and op(A) = A' and/or 
   *    op(B) = B', it could be more efficient to transpose A and/or B 
   *    before calling this routine, and use the 'N' values for TRANA 
   *    and/or TRANB. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Katholieke Univ. Leuven, Belgium, February 2000. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Elementary matrix operations, matrix operations. 
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
   * 
   *    .. Executable Statements .. 
   * 
   *    Test the input scalar arguments. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;

  /* Function Body */
  transa = C2F (lsame) (trana, "T", 1L, 1L)
    || C2F (lsame) (trana, "C", 1L, 1L);
  transb = C2F (lsame) (tranb, "T", 1L, 1L)
    || C2F (lsame) (tranb, "C", 1L, 1L);
  *mc = *ma * *mb;
  *info = 0;
  if (!(transa || C2F (lsame) (trana, "N", 1L, 1L)))
    {
      *info = -1;
    }
  else if (!(transb || C2F (lsame) (tranb, "N", 1L, 1L)))
    {
      *info = -2;
    }
  else if (*ma < 0)
    {
      *info = -3;
    }
  else if (*na < 0)
    {
      *info = -4;
    }
  else if (*mb < 0)
    {
      *info = -5;
    }
  else if (*nb < 0)
    {
      *info = -6;
    }
  else if (transa && *lda < *na || *lda < 1 || !transa && *lda < *ma)
    {
      *info = -10;
    }
  else if (transb && *ldb < *nb || *ldb < 1 || !transb && *ldb < *mb)
    {
      *info = -12;
    }
  else if (*ldc < Max (1, *mc))
    {
      *info = -14;
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
      C2F (xerbla) ("MB01VD", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return, if possible. 
   * 
   */
  *nc = *na * *nb;
  if (*mc == 0 || *nc == 0)
    {
      return 0;
    }
  /* 
   */
  if (*alpha == 0.)
    {
      if (*beta == 0.)
	{
	  C2F (dlaset) ("Full", mc, nc, &c_b10, &c_b10, &c__[c_offset], ldc,
			4L);
	}
      else if (*beta != 1.)
	{
	  /* 
	   */
	  i__1 = *nc;
	  for (j = 1; j <= i__1; ++j)
	    {
	      C2F (dscal) (mc, beta, &c__[j * c_dim1 + 1], &c__1);
	      /* L10: */
	    }
	  /* 
	   */
	}
      return 0;
    }
  /* 
   */
  dum[0] = 0.;
  jc = 1;
  nz = 0;
  /* 
   *    Compute the Kronecker product of the matrices op(A) and op(B), 
   *       C = alpha*kron( op(A), op(B) ) + beta*C. 
   *    First, check if A is sparse. Here, A is considered as being sparse 
   *    if (number of zeros in A)/(MA*NA) >= SPARST. 
   * 
   */
  i__1 = *na;
  for (j = 1; j <= i__1; ++j)
    {
      /* 
       */
      i__2 = *ma;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  if (transa)
	    {
	      if (a[j + i__ * a_dim1] == 0.)
		{
		  ++nz;
		}
	    }
	  else
	    {
	      if (a[i__ + j * a_dim1] == 0.)
		{
		  ++nz;
		}
	    }
	  /* L20: */
	}
      /* 
       */
      /* L30: */
    }
  /* 
   */
  sparse = (double) nz / (double) (*ma * *na) >= .8;
  /* 
   */
  if (!transa && !transb)
    {
      /* 
       *       Case op(A) = A and op(B) = B. 
       * 
       */
      if (*beta == 0.)
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 0, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      if (aij == 0.)
				{
				  C2F (dcopy) (mb, dum, &c__0,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else if (aij == 1.)
				{
				  C2F (dcopy) (mb, &b[k * b_dim1 + 1], &c__1,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L50: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L60: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L70: */
			}
		      /* 
		       */
		      /* L80: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 0, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L90: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L100: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L110: */
			}
		      /* 
		       */
		      /* L120: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 0, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      if (aij == 0.)
				{
				  C2F (dcopy) (mb, dum, &c__0,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L130: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L140: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L150: */
			}
		      /* 
		       */
		      /* L160: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 0, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L170: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L180: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L190: */
			}
		      /* 
		       */
		      /* L200: */
		    }
		  /* 
		   */
		}
	    }
	}
      else if (*beta == 1.)
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 1, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      if (aij != 0.)
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] +=
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L210: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L220: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L230: */
			}
		      /* 
		       */
		      /* L240: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 1, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] +=
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L250: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L260: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L270: */
			}
		      /* 
		       */
		      /* L280: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 1, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      if (aij != 0.)
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] +=
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L290: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L300: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L310: */
			}
		      /* 
		       */
		      /* L320: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 1, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] +=
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L330: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L340: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L350: */
			}
		      /* 
		       */
		      /* L360: */
		    }
		  /* 
		   */
		}
	    }
	}
      else
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      /* 
			       */
			      if (aij == 0.)
				{
				  C2F (dscal) (mb, beta,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					*beta * c__[lc + jc * c_dim1] +
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L370: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L380: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L390: */
			}
		      /* 
		       */
		      /* L400: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    *beta * c__[lc + jc * c_dim1] +
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L410: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L420: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L430: */
			}
		      /* 
		       */
		      /* L440: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      /* 
			       */
			      if (aij == 0.)
				{
				  C2F (dscal) (mb, beta,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					*beta * c__[lc + jc * c_dim1] +
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L450: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L460: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L470: */
			}
		      /* 
		       */
		      /* L480: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    *beta * c__[lc + jc * c_dim1] +
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L490: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L500: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L510: */
			}
		      /* 
		       */
		      /* L520: */
		    }
		  /* 
		   */
		}
	    }
	}
    }
  else if (transa && !transb)
    {
      /* 
       *       Case op(A) = A' and op(B) = B. 
       * 
       */
      if (*beta == 0.)
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 0, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      if (aij == 0.)
				{
				  C2F (dcopy) (mb, dum, &c__0,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else if (aij == 1.)
				{
				  C2F (dcopy) (mb, &b[k * b_dim1 + 1], &c__1,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L530: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L540: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L550: */
			}
		      /* 
		       */
		      /* L560: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 0, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L570: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L580: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L590: */
			}
		      /* 
		       */
		      /* L600: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 0, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      if (aij == 0.)
				{
				  C2F (dcopy) (mb, dum, &c__0,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L610: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L620: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L630: */
			}
		      /* 
		       */
		      /* L640: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 0, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L650: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L660: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L670: */
			}
		      /* 
		       */
		      /* L680: */
		    }
		  /* 
		   */
		}
	    }
	}
      else if (*beta == 1.)
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 1, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      if (aij != 0.)
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] +=
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L690: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L700: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L710: */
			}
		      /* 
		       */
		      /* L720: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 1, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] +=
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L730: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L740: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L750: */
			}
		      /* 
		       */
		      /* L760: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 1, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      if (aij != 0.)
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] +=
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L770: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L780: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L790: */
			}
		      /* 
		       */
		      /* L800: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 1, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] +=
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L810: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L820: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L830: */
			}
		      /* 
		       */
		      /* L840: */
		    }
		  /* 
		   */
		}
	    }
	}
      else
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      /* 
			       */
			      if (aij == 0.)
				{
				  C2F (dscal) (mb, beta,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					*beta * c__[lc + jc * c_dim1] +
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L850: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L860: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L870: */
			}
		      /* 
		       */
		      /* L880: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    *beta * c__[lc + jc * c_dim1] +
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L890: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L900: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L910: */
			}
		      /* 
		       */
		      /* L920: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      /* 
			       */
			      if (aij == 0.)
				{
				  C2F (dscal) (mb, beta,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					*beta * c__[lc + jc * c_dim1] +
					aij * b[l + k * b_dim1];
				      ++lc;
				      /* L930: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L940: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L950: */
			}
		      /* 
		       */
		      /* L960: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    *beta * c__[lc + jc * c_dim1] +
				    aij * b[l + k * b_dim1];
				  ++lc;
				  /* L970: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L980: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L990: */
			}
		      /* 
		       */
		      /* L1000: */
		    }
		  /* 
		   */
		}
	    }
	}
    }
  else if (transb && !transa)
    {
      /* 
       *       Case op(A) = A and op(B) = B'. 
       * 
       */
      if (*beta == 0.)
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 0, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      if (aij == 0.)
				{
				  C2F (dcopy) (mb, dum, &c__0,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else if (aij == 1.)
				{
				  C2F (dcopy) (mb, &b[k + b_dim1], ldb,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1050: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1060: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1070: */
			}
		      /* 
		       */
		      /* L1080: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 0, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1090: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1100: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1110: */
			}
		      /* 
		       */
		      /* L1120: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 0, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      if (aij == 0.)
				{
				  C2F (dcopy) (mb, dum, &c__0,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1130: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1140: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1150: */
			}
		      /* 
		       */
		      /* L1160: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 0, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1170: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1180: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1190: */
			}
		      /* 
		       */
		      /* L1200: */
		    }
		  /* 
		   */
		}
	    }
	}
      else if (*beta == 1.)
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 1, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      if (aij != 0.)
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] +=
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1210: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1220: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1230: */
			}
		      /* 
		       */
		      /* L1240: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 1, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] +=
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1250: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1260: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1270: */
			}
		      /* 
		       */
		      /* L1280: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 1, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      if (aij != 0.)
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] +=
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1290: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1300: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1310: */
			}
		      /* 
		       */
		      /* L1320: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 1, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] +=
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1330: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1340: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1350: */
			}
		      /* 
		       */
		      /* L1360: */
		    }
		  /* 
		   */
		}
	    }
	}
      else
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      /* 
			       */
			      if (aij == 0.)
				{
				  C2F (dscal) (mb, beta,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					*beta * c__[lc + jc * c_dim1] +
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1370: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1380: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1390: */
			}
		      /* 
		       */
		      /* L1400: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    *beta * c__[lc + jc * c_dim1] +
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1410: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1420: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1430: */
			}
		      /* 
		       */
		      /* L1440: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      /* 
			       */
			      if (aij == 0.)
				{
				  C2F (dscal) (mb, beta,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					*beta * c__[lc + jc * c_dim1] +
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1450: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1460: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1470: */
			}
		      /* 
		       */
		      /* L1480: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[i__ + j * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    *beta * c__[lc + jc * c_dim1] +
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1490: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1500: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1510: */
			}
		      /* 
		       */
		      /* L1520: */
		    }
		  /* 
		   */
		}
	    }
	}
    }
  else
    {
      /* 
       *       Case op(A) = A' and op(B) = B'. 
       * 
       */
      if (*beta == 0.)
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 0, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      if (aij == 0.)
				{
				  C2F (dcopy) (mb, dum, &c__0,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else if (aij == 1.)
				{
				  C2F (dcopy) (mb, &b[k + b_dim1], ldb,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1550: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1560: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1570: */
			}
		      /* 
		       */
		      /* L1580: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 0, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1590: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1600: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1610: */
			}
		      /* 
		       */
		      /* L1620: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 0, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      if (aij == 0.)
				{
				  C2F (dcopy) (mb, dum, &c__0,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1630: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1640: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1650: */
			}
		      /* 
		       */
		      /* L1660: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 0, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1670: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1680: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1690: */
			}
		      /* 
		       */
		      /* L1700: */
		    }
		  /* 
		   */
		}
	    }
	}
      else if (*beta == 1.)
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 1, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      if (aij != 0.)
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] +=
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1710: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1720: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1730: */
			}
		      /* 
		       */
		      /* L1740: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 1, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] +=
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1750: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1760: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1770: */
			}
		      /* 
		       */
		      /* L1780: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta = 1, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      if (aij != 0.)
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] +=
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1790: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1800: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1810: */
			}
		      /* 
		       */
		      /* L1820: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta = 1, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] +=
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1830: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1840: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1850: */
			}
		      /* 
		       */
		      /* L1860: */
		    }
		  /* 
		   */
		}
	    }
	}
      else
	{
	  if (*alpha == 1.)
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha = 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      /* 
			       */
			      if (aij == 0.)
				{
				  C2F (dscal) (mb, beta,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					*beta * c__[lc + jc * c_dim1] +
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1870: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1880: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1890: */
			}
		      /* 
		       */
		      /* L1900: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha = 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    *beta * c__[lc + jc * c_dim1] +
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1910: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L1920: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1930: */
			}
		      /* 
		       */
		      /* L1940: */
		    }
		  /* 
		   */
		}
	    }
	  else
	    {
	      if (sparse)
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha <> 1, A sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      /* 
			       */
			      if (aij == 0.)
				{
				  C2F (dscal) (mb, beta,
					       &c__[ic + jc * c_dim1], &c__1);
				}
			      else
				{
				  lc = ic;
				  /* 
				   */
				  i__4 = *mb;
				  for (l = 1; l <= i__4; ++l)
				    {
				      c__[lc + jc * c_dim1] =
					*beta * c__[lc + jc * c_dim1] +
					aij * b[k + l * b_dim1];
				      ++lc;
				      /* L1950: */
				    }
				  /* 
				   */
				}
			      ic += *mb;
			      /* L1960: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L1970: */
			}
		      /* 
		       */
		      /* L1980: */
		    }
		  /* 
		   */
		}
	      else
		{
		  /* 
		   *                Case beta <> 0 or 1, alpha <> 1, A not sparse. 
		   * 
		   */
		  i__1 = *na;
		  for (j = 1; j <= i__1; ++j)
		    {
		      /* 
		       */
		      i__2 = *nb;
		      for (k = 1; k <= i__2; ++k)
			{
			  ic = 1;
			  /* 
			   */
			  i__3 = *ma;
			  for (i__ = 1; i__ <= i__3; ++i__)
			    {
			      aij = *alpha * a[j + i__ * a_dim1];
			      lc = ic;
			      /* 
			       */
			      i__4 = *mb;
			      for (l = 1; l <= i__4; ++l)
				{
				  c__[lc + jc * c_dim1] =
				    *beta * c__[lc + jc * c_dim1] +
				    aij * b[k + l * b_dim1];
				  ++lc;
				  /* L1990: */
				}
			      /* 
			       */
			      ic += *mb;
			      /* L2000: */
			    }
			  /* 
			   */
			  ++jc;
			  /* L2010: */
			}
		      /* 
		       */
		      /* L2020: */
		    }
		  /* 
		   */
		}
	    }
	}
    }
  return 0;
  /**** Last line of MB01VD *** 
   */
}				/* nsp_slicot_mb01vd */
