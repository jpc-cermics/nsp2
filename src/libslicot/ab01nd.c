/* AB01ND.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b7 = 0.;
static double c_b8 = 1.;
static int c__1 = 1;
static int c__0 = 0;
static int c_false = FALSE;

/* Subroutine */ int
nsp_slicot_ab01nd (char *jobz, int *n, int *m, double *a, int *lda,
		   double *b, int *ldb, int *ncont, int *indcon,
		   int *nblk, double *z__, int *ldz, double *tau,
		   double *tol, int *iwork, double *dwork,
		   int *ldwork, int *info, long int jobz_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, i__1, i__2,
    i__3, i__4;
  double d__1, d__2;

  /* Local variables */
  int rank, itau;
  double fnrm;
  int mcrt, ncrt;
  double sval[3];
  int j;
  int ljobf, ljobi;
  double anorm, bnorm;
  int ljobz;
  int ni, nj;
  double toldef;
  int wrkopt, nbl, iqr;

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
   *    To find a controllable realization for the linear time-invariant 
   *    multi-input system 
   * 
   *            dX/dt = A * X + B * U, 
   * 
   *    where A and B are N-by-N and N-by-M matrices, respectively, 
   *    which are reduced by this routine to orthogonal canonical form 
   *    using (and optionally accumulating) orthogonal similarity 
   *    transformations.  Specifically, the pair (A, B) is reduced to 
   *    the pair (Ac, Bc),  Ac = Z' * A * Z,  Bc = Z' * B,  given by 
   * 
   *            [ Acont     *    ]         [ Bcont ] 
   *       Ac = [                ],   Bc = [       ], 
   *            [   0    Auncont ]         [   0   ] 
   * 
   *       and 
   * 
   *               [ A11 A12  . . .  A1,p-1 A1p ]         [ B1 ] 
   *               [ A21 A22  . . .  A2,p-1 A2p ]         [ 0  ] 
   *               [  0  A32  . . .  A3,p-1 A3p ]         [ 0  ] 
   *       Acont = [  .   .   . . .    .     .  ],   Bc = [ .  ], 
   *               [  .   .     . .    .     .  ]         [ .  ] 
   *               [  .   .       .    .     .  ]         [ .  ] 
   *               [  0   0   . . .  Ap,p-1 App ]         [ 0  ] 
   * 
   *    where the blocks  B1, A21, ..., Ap,p-1  have full row ranks and 
   *    p is the controllability index of the pair.  The size of the 
   *    block  Auncont is equal to the dimension of the uncontrollable 
   *    subspace of the pair (A, B). 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    JOBZ    CHARACTER*1 
   *            Indicates whether the user wishes to accumulate in a 
   *            matrix Z the orthogonal similarity transformations for 
   *            reducing the system, as follows: 
   *            = 'N':  Do not form Z and do not store the orthogonal 
   *                    transformations; 
   *            = 'F':  Do not form Z, but store the orthogonal 
   *                    transformations in the factored form; 
   *            = 'I':  Z is initialized to the unit matrix and the 
   *                    orthogonal transformation matrix Z is returned. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the original state-space representation, 
   *            i.e. the order of the matrix A.  N >= 0. 
   * 
   *    M       (input) INT 
   *            The number of system inputs, or of columns of B.  M >= 0. 
   * 
   *    A       (input/output) DOUBLE PRECISION array, dimension (LDA,N) 
   *            On entry, the leading N-by-N part of this array must 
   *            contain the original state dynamics matrix A. 
   *            On exit, the leading NCONT-by-NCONT part contains the 
   *            upper block Hessenberg state dynamics matrix Acont in Ac, 
   *            given by Z' * A * Z, of a controllable realization for 
   *            the original system. The elements below the first block- 
   *            subdiagonal are set to zero. 
   * 
   *    LDA     INT 
   *            The leading dimension of array A.  LDA >= MAX(1,N). 
   * 
   *    B       (input/output) DOUBLE PRECISION array, dimension (LDB,M) 
   *            On entry, the leading N-by-M part of this array must 
   *            contain the input matrix B. 
   *            On exit, the leading NCONT-by-M part of this array 
   *            contains the transformed input matrix Bcont in Bc, given 
   *            by Z' * B, with all elements but the first block set to 
   *            zero. 
   * 
   *    LDB     INT 
   *            The leading dimension of array B.  LDB >= MAX(1,N). 
   * 
   *    NCONT   (output) INT 
   *            The order of the controllable state-space representation. 
   * 
   *    INDCON  (output) INT 
   *            The controllability index of the controllable part of the 
   *            system representation. 
   * 
   *    NBLK    (output) INT array, dimension (N) 
   *            The leading INDCON elements of this array contain the 
   *            the orders of the diagonal blocks of Acont. 
   * 
   *    Z       (output) DOUBLE PRECISION array, dimension (LDZ,N) 
   *            If JOBZ = 'I', then the leading N-by-N part of this 
   *            array contains the matrix of accumulated orthogonal 
   *            similarity transformations which reduces the given system 
   *            to orthogonal canonical form. 
   *            If JOBZ = 'F', the elements below the diagonal, with the 
   *            array TAU, represent the orthogonal transformation matrix 
   *            as a product of elementary reflectors. The transformation 
   *            matrix can then be obtained by calling the LAPACK Library 
   *            routine DORGQR. 
   *            If JOBZ = 'N', the array Z is not referenced and can be 
   *            supplied as a dummy array (i.e. set parameter LDZ = 1 and 
   *            declare this array to be Z(1,1) in the calling program). 
   * 
   *    LDZ     INT 
   *            The leading dimension of array Z. If JOBZ = 'I' or 
   *            JOBZ = 'F', LDZ >= MAX(1,N); if JOBZ = 'N', LDZ >= 1. 
   * 
   *    TAU     (output) DOUBLE PRECISION array, dimension (N) 
   *            The elements of TAU contain the scalar factors of the 
   *            elementary reflectors used in the reduction of B and A. 
   * 
   *    Tolerances 
   * 
   *    TOL     DOUBLE PRECISION 
   *            The tolerance to be used in rank determination when 
   *            transforming (A, B). If the user sets TOL > 0, then 
   *            the given value of TOL is used as a lower bound for the 
   *            reciprocal condition number (see the description of the 
   *            argument RCOND in the SLICOT routine MB03OD);  a 
   *            (sub)matrix whose estimated condition number is less than 
   *            1/TOL is considered to be of full rank.  If the user sets 
   *            TOL <= 0, then an implicitly computed, default tolerance, 
   *            defined by  TOLDEF = N*N*EPS,  is used instead, where EPS 
   *            is the machine precision (see LAPACK Library routine 
   *            DLAMCH). 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (M) 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if INFO = 0, DWORK(1) returns the optimal value 
   *            of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= MAX(1, N, 3*M). 
   *            For optimum performance LDWORK should be larger. 
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
   *    Matrix B is first QR-decomposed and the appropriate orthogonal 
   *    similarity transformation applied to the matrix A. Leaving the 
   *    first rank(B) states unchanged, the remaining lower left block 
   *    of A is then QR-decomposed and the new orthogonal matrix, Q1, 
   *    is also applied to the right of A to complete the similarity 
   *    transformation. By continuing in this manner, a completely 
   *    controllable state-space pair (Acont, Bcont) is found for the 
   *    given (A, B), where Acont is upper block Hessenberg with each 
   *    subdiagonal block of full row rank, and Bcont is zero apart from 
   *    its (independent) first rank(B) rows. 
   *    NOTE that the system controllability indices are easily 
   *    calculated from the dimensions of the blocks of Acont. 
   * 
   *    REFERENCES 
   * 
   *    [1] Konstantinov, M.M., Petkov, P.Hr. and Christov, N.D. 
   *        Orthogonal Invariants and Canonical Forms for Linear 
   *        Controllable Systems. 
   *        Proc. 8th IFAC World Congress, Kyoto, 1, pp. 49-54, 1981. 
   * 
   *    [2] Paige, C.C. 
   *        Properties of numerical algorithms related to computing 
   *        controllablity. 
   *        IEEE Trans. Auto. Contr., AC-26, pp. 130-138, 1981. 
   * 
   *    [3] Petkov, P.Hr., Konstantinov, M.M., Gu, D.W. and 
   *        Postlethwaite, I. 
   *        Optimal Pole Assignment Design of Linear Multi-Input Systems. 
   *        Leicester University, Report 99-11, May 1996. 
   * 
   *    NUMERICAL ASPECTS 
   *                              3 
   *    The algorithm requires 0(N ) operations and is backward stable. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    If the system matrices A and B are badly scaled, it would be 
   *    useful to scale them with SLICOT routine TB01ID, before calling 
   *    the routine. 
   * 
   *    CONTRIBUTOR 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Nov. 1996. 
   *    Supersedes Release 2.0 routine AB01BD by P.Hr. Petkov. 
   * 
   *    REVISIONS 
   * 
   *    January 14, 1997, June 4, 1997, February 13, 1998, 
   *    September 22, 2003, February 29, 2004. 
   * 
   *    KEYWORDS 
   * 
   *    Controllability, minimal realization, orthogonal canonical form, 
   *    orthogonal transformation. 
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
   *    .. 
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
  --nblk;
  z_dim1 = *ldz;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  --tau;
  --iwork;
  --dwork;

  /* Function Body */
  *info = 0;
  ljobf = C2F (lsame) (jobz, "F", 1L, 1L);
  ljobi = C2F (lsame) (jobz, "I", 1L, 1L);
  ljobz = ljobf || ljobi;
  /* 
   *    Test the input scalar arguments. 
   * 
   */
  if (!ljobz && !C2F (lsame) (jobz, "N", 1L, 1L))
    {
      *info = -1;
    }
  else if (*n < 0)
    {
      *info = -2;
    }
  else if (*m < 0)
    {
      *info = -3;
    }
  else if (*lda < Max (1, *n))
    {
      *info = -5;
    }
  else if (*ldb < Max (1, *n))
    {
      *info = -7;
    }
  else if (*ldz < 1 || ljobz && *ldz < *n)
    {
      *info = -12;
    }
  else				/* if(complicated condition) */
    {
      /*Computing MAX 
       */
      i__1 = Max (1, *n), i__2 = *m * 3;
      if (*ldwork < Max (i__1, i__2))
	{
	  *info = -17;
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
      C2F (xerbla) ("AB01ND", &i__1, 6L);
      return 0;
    }
  /* 
   */
  *ncont = 0;
  *indcon = 0;
  /* 
   *    Quick return if possible. 
   * 
   */
  if (Min (*n, *m) == 0)
    {
      if (*n > 0)
	{
	  if (ljobi)
	    {
	      C2F (dlaset) ("Full", n, n, &c_b7, &c_b8, &z__[z_offset], ldz,
			    4L);
	    }
	  else if (ljobf)
	    {
	      C2F (dlaset) ("Full", n, n, &c_b7, &c_b7, &z__[z_offset], ldz,
			    4L);
	      C2F (dlaset) ("Full", n, &c__1, &c_b7, &c_b7, &tau[1], n, 4L);
	    }
	}
      dwork[1] = 1.;
      return 0;
    }
  /* 
   *    Calculate the absolute norms of A and B (used for scaling). 
   * 
   */
  anorm = C2F (dlange) ("M", n, n, &a[a_offset], lda, &dwork[1], 1L);
  bnorm = C2F (dlange) ("M", n, m, &b[b_offset], ldb, &dwork[1], 1L);
  /* 
   *    Return if matrix B is zero. 
   * 
   */
  if (bnorm == 0.)
    {
      if (ljobi)
	{
	  C2F (dlaset) ("Full", n, n, &c_b7, &c_b8, &z__[z_offset], ldz, 4L);
	}
      else if (ljobf)
	{
	  C2F (dlaset) ("Full", n, n, &c_b7, &c_b7, &z__[z_offset], ldz, 4L);
	  C2F (dlaset) ("Full", n, &c__1, &c_b7, &c_b7, &tau[1], n, 4L);
	}
      dwork[1] = 1.;
      return 0;
    }
  /* 
   *    Scale (if needed) the matrices A and B. 
   * 
   */
  nsp_slicot_mb01pd ("Scale", "G", n, n, &c__0, &c__0, &anorm, &c__0,
		     &nblk[1], &a[a_offset], lda, info, 5L, 1L);
  nsp_slicot_mb01pd ("Scale", "G", n, m, &c__0, &c__0, &bnorm, &c__0,
		     &nblk[1], &b[b_offset], ldb, info, 5L, 1L);
  /* 
   *    Compute the Frobenius norm of [ B  A ] (used for rank estimation). 
   * 
   */
  d__1 = C2F (dlange) ("F", n, m, &b[b_offset], ldb, &dwork[1], 1L);
  d__2 = C2F (dlange) ("F", n, n, &a[a_offset], lda, &dwork[1], 1L);
  fnrm = C2F (dlapy2) (&d__1, &d__2);
  /* 
   */
  toldef = *tol;
  if (toldef <= 0.)
    {
      /* 
       *       Use the default tolerance in controllability determination. 
       * 
       */
      toldef = (double) (*n * *n) * C2F (dlamch) ("EPSILON", 7L);
    }
  /* 
   */
  wrkopt = 1;
  ni = 0;
  itau = 1;
  ncrt = *n;
  mcrt = *m;
  iqr = 1;
  /* 
   *    (Note: Comments in the code beginning "Workspace:" describe the 
   *    minimal amount of real workspace needed at that point in the 
   *    code, as well as the preferred amount for good performance. 
   *    NB refers to the optimal block size for the immediately 
   *    following subroutine, as returned by ILAENV.) 
   * 
   */
 L10:
  /* 
   *       Rank-revealing QR decomposition with column pivoting. 
   *       The calculation is performed in NCRT rows of B starting from 
   *       the row IQR (initialized to 1 and then set to rank(B)+1). 
   *       Workspace: 3*MCRT. 
   * 
   */
  nsp_slicot_mb03oy (&ncrt, &mcrt, &b[iqr + b_dim1], ldb, &toldef, &fnrm,
		     &rank, sval, &iwork[1], &tau[itau], &dwork[1], info);
  /* 
   */
  if (rank != 0)
    {
      nj = ni;
      ni = *ncont;
      *ncont += rank;
      ++(*indcon);
      nblk[*indcon] = rank;
      /* 
       *          Premultiply and postmultiply the appropriate block row 
       *          and block column of A by Q' and Q, respectively. 
       *          Workspace: need   NCRT; 
       *                     prefer NCRT*NB. 
       * 
       */
      C2F (dormqr) ("Left", "Transpose", &ncrt, &ncrt, &rank,
		    &b[iqr + b_dim1], ldb, &tau[itau],
		    &a[ni + 1 + (ni + 1) * a_dim1], lda, &dwork[1], ldwork,
		    info, 4L, 9L);
      /*Computing MAX 
       */
      i__1 = wrkopt, i__2 = (int) dwork[1];
      wrkopt = Max (i__1, i__2);
      /* 
       *          Workspace: need   N; 
       *                     prefer N*NB. 
       * 
       */
      C2F (dormqr) ("Right", "No transpose", n, &ncrt, &rank,
		    &b[iqr + b_dim1], ldb, &tau[itau],
		    &a[(ni + 1) * a_dim1 + 1], lda, &dwork[1], ldwork, info,
		    5L, 12L);
      /*Computing MAX 
       */
      i__1 = wrkopt, i__2 = (int) dwork[1];
      wrkopt = Max (i__1, i__2);
      /* 
       *          If required, save transformations. 
       * 
       */
      if (ljobz && ncrt > 1)
	{
	  i__1 = ncrt - 1;
	  /*Computing MIN 
	   */
	  i__3 = rank, i__4 = ncrt - 1;
	  i__2 = Min (i__3, i__4);
	  C2F (dlacpy) ("L", &i__1, &i__2, &b[iqr + 1 + b_dim1], ldb,
			&z__[ni + 2 + itau * z_dim1], ldz, 1L);
	}
      /* 
       *          Zero the subdiagonal elements of the current matrix. 
       * 
       */
      if (rank > 1)
	{
	  i__1 = rank - 1;
	  i__2 = rank - 1;
	  C2F (dlaset) ("L", &i__1, &i__2, &c_b7, &c_b7, &b[iqr + 1 + b_dim1],
			ldb, 1L);
	}
      /* 
       *          Backward permutation of the columns of B or A. 
       * 
       */
      if (*indcon == 1)
	{
	  C2F (dlapmt) (&c_false, &rank, m, &b[iqr + b_dim1], ldb, &iwork[1]);
	  iqr = rank + 1;
	}
      else
	{
	  i__1 = mcrt;
	  for (j = 1; j <= i__1; ++j)
	    {
	      C2F (dcopy) (&rank, &b[iqr + j * b_dim1], &c__1,
			   &a[ni + 1 + (nj + iwork[j]) * a_dim1], &c__1);
	      /* L20: */
	    }
	}
      /* 
       */
      itau += rank;
      if (rank != ncrt)
	{
	  mcrt = rank;
	  ncrt -= rank;
	  C2F (dlacpy) ("G", &ncrt, &mcrt, &a[*ncont + 1 + (ni + 1) * a_dim1],
			lda, &b[iqr + b_dim1], ldb, 1L);
	  C2F (dlaset) ("G", &ncrt, &mcrt, &c_b7, &c_b7,
			&a[*ncont + 1 + (ni + 1) * a_dim1], lda, 1L);
	  goto L10;
	}
    }
  /* 
   *    If required, accumulate transformations. 
   *    Workspace: need N;  prefer N*NB. 
   * 
   */
  if (ljobi)
    {
      /*Computing MAX 
       */
      i__2 = 1, i__3 = itau - 1;
      i__1 = Max (i__2, i__3);
      C2F (dorgqr) (n, n, &i__1, &z__[z_offset], ldz, &tau[1], &dwork[1],
		    ldwork, info);
      /*Computing MAX 
       */
      i__1 = wrkopt, i__2 = (int) dwork[1];
      wrkopt = Max (i__1, i__2);
    }
  /* 
   *    Annihilate the trailing blocks of B. 
   * 
   */
  if (*n >= iqr)
    {
      i__1 = *n - iqr + 1;
      C2F (dlaset) ("G", &i__1, m, &c_b7, &c_b7, &b[iqr + b_dim1], ldb, 1L);
    }
  /* 
   *    Annihilate the trailing elements of TAU, if JOBZ = 'F'. 
   * 
   */
  if (ljobf)
    {
      i__1 = *n;
      for (j = itau; j <= i__1; ++j)
	{
	  tau[j] = 0.;
	  /* L30: */
	}
    }
  /* 
   *    Undo scaling of A and B. 
   * 
   */
  if (*indcon < *n)
    {
      nbl = *indcon + 1;
      nblk[nbl] = *n - *ncont;
    }
  else
    {
      nbl = 0;
    }
  nsp_slicot_mb01pd ("Undo", "H", n, n, &c__0, &c__0, &anorm, &nbl, &nblk[1],
		     &a[a_offset], lda, info, 4L, 1L);
  nsp_slicot_mb01pd ("Undo", "G", &nblk[1], m, &c__0, &c__0, &bnorm, &c__0,
		     &nblk[1], &b[b_offset], ldb, info, 4L, 1L);
  /* 
   *    Set optimal workspace dimension. 
   * 
   */
  dwork[1] = (double) wrkopt;
  return 0;
  /**** Last line of AB01ND *** 
   */
}				/* nsp_slicot_ab01nd */
