/* SB03OT.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__2 = 2;
static int c__1 = 1;
static double c_b19 = -1.;
static double c_b47 = 1.;
static int c__3 = 3;
static int c__0 = 0;

/* Subroutine */ int
nsp_slicot_sb03ot (int *discr, int *ltrans, int *n, double *s,
		   int *lds, double *r__, int *ldr, double *scale,
		   double *dwork, int *info)
{
  /* System generated locals */
  int r_dim1, r_offset, s_dim1, s_offset, i__1;
  double d__1, d__2;

  /* Builtin functions */
  double sqrt (double), d_sign (double *, double *);

  /* Local variables */
  int isgn;
  int cont;
  double temp, smin;
  int tbyt;
  double a[4] /* was [2][2] */ , b[4] /* was [2][2] */ ;
  int j, k;
  double alpha, u[4] /* was [2][2] */ ;
  int infom;
  int ksize;
  double d1, d2;
  int j1, j2, j3, k1, k2, k3, kount;
  double t1, t2, t3, t4, v1, v2, v3, v4;
  double dr;
  double scaloc;
  double absskk, bignum, smlnum, eps, sum, tau1, tau2;

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
   *    To solve for X = op(U)'*op(U) either the stable non-negative 
   *    definite continuous-time Lyapunov equation 
   *                                  2 
   *       op(S)'*X + X*op(S) = -scale *op(R)'*op(R)                   (1) 
   * 
   *    or the convergent non-negative definite discrete-time Lyapunov 
   *    equation 
   *                                  2 
   *       op(S)'*X*op(S) - X = -scale *op(R)'*op(R)                   (2) 
   * 
   *    where op(K) = K or K' (i.e., the transpose of the matrix K), S is 
   *    an N-by-N block upper triangular matrix with one-by-one or 
   *    two-by-two blocks on the diagonal, R is an N-by-N upper triangular 
   *    matrix, and scale is an output scale factor, set less than or 
   *    equal to 1 to avoid overflow in X. 
   * 
   *    In the case of equation (1) the matrix S must be stable (that 
   *    is, all the eigenvalues of S must have negative real parts), 
   *    and for equation (2) the matrix S must be convergent (that is, 
   *    all the eigenvalues of S must lie inside the unit circle). 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    DISCR   INT 
   *            Specifies the type of Lyapunov equation to be solved as 
   *            follows: 
   *            = .TRUE. :  Equation (2), discrete-time case; 
   *            = .FALSE.:  Equation (1), continuous-time case. 
   * 
   *    LTRANS  INT 
   *            Specifies the form of op(K) to be used, as follows: 
   *            = .FALSE.:  op(K) = K    (No transpose); 
   *            = .TRUE. :  op(K) = K**T (Transpose). 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrices S and R.  N >= 0. 
   * 
   *    S       (input) DOUBLE PRECISION array of dimension (LDS,N) 
   *            The leading N-by-N upper Hessenberg part of this array 
   *            must contain the block upper triangular matrix. 
   *            The elements below the upper Hessenberg part of the array 
   *            S are not referenced. The 2-by-2 blocks must only 
   *            correspond to complex conjugate pairs of eigenvalues (not 
   *            to real eigenvalues). 
   * 
   *    LDS     INT 
   *            The leading dimension of array S.  LDS >= MAX(1,N). 
   * 
   *    R       (input/output) DOUBLE PRECISION array of dimension (LDR,N) 
   *            On entry, the leading N-by-N upper triangular part of this 
   *            array must contain the upper triangular matrix R. 
   *            On exit, the leading N-by-N upper triangular part of this 
   *            array contains the upper triangular matrix U. 
   *            The strict lower triangle of R is not referenced. 
   * 
   *    LDR     INT 
   *            The leading dimension of array R.  LDR >= MAX(1,N). 
   * 
   *    SCALE   (output) DOUBLE PRECISION 
   *            The scale factor, scale, set less than or equal to 1 to 
   *            prevent the solution overflowing. 
   * 
   *    Workspace 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (4*N) 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 1:  if the Lyapunov equation is (nearly) singular 
   *                  (warning indicator); 
   *                  if DISCR = .FALSE., this means that while the 
   *                  matrix S has computed eigenvalues with negative real 
   *                  parts, it is only just stable in the sense that 
   *                  small perturbations in S can make one or more of the 
   *                  eigenvalues have a non-negative real part; 
   *                  if DISCR = .TRUE., this means that while the 
   *                  matrix S has computed eigenvalues inside the unit 
   *                  circle, it is nevertheless only just convergent, in 
   *                  the sense that small perturbations in S can make one 
   *                  or more of the eigenvalues lie outside the unit 
   *                  circle; 
   *                  perturbed values were used to solve the equation 
   *                  (but the matrix S is unchanged); 
   *            = 2:  if the matrix S is not stable (that is, one or more 
   *                  of the eigenvalues of S has a non-negative real 
   *                  part), if DISCR = .FALSE., or not convergent (that 
   *                  is, one or more of the eigenvalues of S lies outside 
   *                  the unit circle), if DISCR = .TRUE.; 
   *            = 3:  if the matrix S has two or more consecutive non-zero 
   *                  elements on the first sub-diagonal, so that there is 
   *                  a block larger than 2-by-2 on the diagonal; 
   *            = 4:  if the matrix S has a 2-by-2 diagonal block with 
   *                  real eigenvalues instead of a complex conjugate 
   *                  pair. 
   * 
   *    METHOD 
   * 
   *    The method used by the routine is based on a variant of the 
   *    Bartels and Stewart backward substitution method [1], that finds 
   *    the Cholesky factor op(U) directly without first finding X and 
   *    without the need to form the normal matrix op(R)'*op(R) [2]. 
   * 
   *    The continuous-time Lyapunov equation in the canonical form 
   *                                                       2 
   *      op(S)'*op(U)'*op(U) + op(U)'*op(U)*op(S) = -scale *op(R)'*op(R), 
   * 
   *    or the discrete-time Lyapunov equation in the canonical form 
   *                                                       2 
   *      op(S)'*op(U)'*op(U)*op(S) - op(U)'*op(U) = -scale *op(R)'*op(R), 
   * 
   *    where U and R are upper triangular, is solved for U. 
   * 
   *    REFERENCES 
   * 
   *    [1] Bartels, R.H. and Stewart, G.W. 
   *        Solution of the matrix equation  A'X + XB = C. 
   *        Comm. A.C.M., 15, pp. 820-826, 1972. 
   * 
   *    [2] Hammarling, S.J. 
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
   *    The Lyapunov equation may be very ill-conditioned. In particular 
   *    if S is only just stable (or convergent) then the Lyapunov 
   *    equation will be ill-conditioned. "Large" elements in U relative 
   *    to those of S and R, or a "small" value for scale, is a symptom 
   *    of ill-conditioning. A condition estimate can be computed using 
   *    SLICOT Library routine SB03MD. 
   * 
   *    CONTRIBUTOR 
   * 
   *    Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, May 1997. 
   *    Supersedes Release 2.0 routine SB03CZ by Sven Hammarling, 
   *    NAG Ltd, United Kingdom, Oct. 1986. 
   *    Partly based on SB03CZ and PLYAP1 by A. Varga, University of 
   *    Bochum, May 1992. 
   * 
   *    REVISIONS 
   * 
   *    Dec. 1997, April 1998, May 1999, Feb. 2004. 
   * 
   *    KEYWORDS 
   * 
   *    Lyapunov equation, orthogonal transformation, real Schur form. 
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
  s_dim1 = *lds;
  s_offset = s_dim1 + 1;
  s -= s_offset;
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  --dwork;

  /* Function Body */
  *info = 0;
  /* 
   *    Test the input scalar arguments. 
   * 
   */
  if (*n < 0)
    {
      *info = -3;
    }
  else if (*lds < Max (1, *n))
    {
      *info = -5;
    }
  else if (*ldr < Max (1, *n))
    {
      *info = -7;
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
      C2F (xerbla) ("SB03OT", &i__1, 6L);
      return 0;
    }
  /* 
   */
  *scale = 1.;
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0)
    {
      return 0;
    }
  /* 
   *    Set constants to control overflow. 
   * 
   */
  eps = C2F (dlamch) ("P", 1L);
  smlnum = C2F (dlamch) ("S", 1L);
  bignum = 1. / smlnum;
  C2F (dlabad) (&smlnum, &bignum);
  smlnum = smlnum * (double) (*n * *n) / eps;
  bignum = 1. / smlnum;
  /* 
   *Computing MAX 
   */
  d__1 = smlnum, d__2 =
    eps * C2F (dlanhs) ("Max", n, &s[s_offset], lds, &dwork[1], 3L);
  smin = Max (d__1, d__2);
  infom = 0;
  /* 
   *    Start the solution. Most of the comments refer to notation and 
   *    equations in sections 5 and 10 of the second reference above. 
   * 
   *    Determine whether or not the current block is two-by-two. 
   *    K gives the position of the start of the current block and 
   *    TBYT is true if the block is two-by-two. 
   * 
   */
  cont = !(*discr);
  isgn = 1;
  if (!(*ltrans))
    {
      /* 
       *       Case op(M) = M. 
       * 
       */
      kount = 1;
      /* 
       */
    L10:
      /*       WHILE( KOUNT.LE.N )LOOP 
       */
      if (kount <= *n)
	{
	  k = kount;
	  if (kount >= *n)
	    {
	      tbyt = FALSE;
	      ++kount;
	    }
	  else if (s[k + 1 + k * s_dim1] == 0.)
	    {
	      tbyt = FALSE;
	      ++kount;
	    }
	  else
	    {
	      tbyt = TRUE;
	      if (k + 1 < *n)
		{
		  if (s[k + 2 + (k + 1) * s_dim1] != 0.)
		    {
		      *info = 3;
		      return 0;
		    }
		}
	      kount += 2;
	    }
	  if (tbyt)
	    {
	      /* 
	       *             Solve the two-by-two Lyapunov equation (6.1) or (10.19), 
	       *             using the routine SB03OY. 
	       * 
	       */
	      b[0] = s[k + k * s_dim1];
	      b[1] = s[k + 1 + k * s_dim1];
	      b[2] = s[k + (k + 1) * s_dim1];
	      b[3] = s[k + 1 + (k + 1) * s_dim1];
	      u[0] = r__[k + k * r_dim1];
	      u[2] = r__[k + (k + 1) * r_dim1];
	      u[3] = r__[k + 1 + (k + 1) * r_dim1];
	      /* 
	       */
	      nsp_slicot_sb03oy (discr, ltrans, &isgn, b, &c__2, u, &c__2, a,
				 &c__2, &scaloc, info);
	      if (*info > 1)
		{
		  return 0;
		}
	      infom = Max (*info, infom);
	      if (scaloc != 1.)
		{
		  /* 
		   */
		  i__1 = *n;
		  for (j = 1; j <= i__1; ++j)
		    {
		      C2F (dscal) (&j, &scaloc, &r__[j * r_dim1 + 1], &c__1);
		      /* L20: */
		    }
		  /* 
		   */
		  *scale *= scaloc;
		}
	      r__[k + k * r_dim1] = u[0];
	      r__[k + (k + 1) * r_dim1] = u[2];
	      r__[k + 1 + (k + 1) * r_dim1] = u[3];
	      /* 
	       *             If we are not at the end of S then set up and solve 
	       *             equation (6.2) or (10.20). 
	       * 
	       *             Note that  SB03OY  returns  ( u11*s11*inv( u11 ) ) in  B 
	       *             and returns scaled alpha in  A.  ksize is the order of 
	       *             the remainder of  S.  k1, k2 and k3  point to the start 
	       *             of vectors in  DWORK. 
	       * 
	       */
	      if (kount <= *n)
		{
		  ksize = *n - k - 1;
		  k1 = ksize + 1;
		  k2 = ksize + k1;
		  k3 = ksize + k2;
		  /* 
		   *                Form the right-hand side of (6.2) or (10.20), the 
		   *                first column in DWORK( 1 ) ,..., DWORK( n - k - 1 ) 
		   *                the second in DWORK( n - k ) ,..., 
		   *                DWORK( 2*( n - k - 1 ) ). 
		   * 
		   */
		  C2F (dcopy) (&ksize, &r__[k + (k + 2) * r_dim1], ldr,
			       &dwork[1], &c__1);
		  C2F (dcopy) (&ksize, &r__[k + 1 + (k + 2) * r_dim1], ldr,
			       &dwork[k1], &c__1);
		  C2F (dtrmm) ("Right", "Upper", "No transpose", "Non-unit",
			       &ksize, &c__2, &c_b19, a, &c__2, &dwork[1],
			       &ksize, 5L, 5L, 12L, 8L);
		  if (cont)
		    {
		      d__1 = -r__[k + k * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k + (k + 2) * s_dim1],
				   lds, &dwork[1], &c__1);
		      d__1 = -r__[k + (k + 1) * r_dim1];
		      C2F (daxpy) (&ksize, &d__1,
				   &s[k + 1 + (k + 2) * s_dim1], lds,
				   &dwork[1], &c__1);
		      d__1 = -r__[k + 1 + (k + 1) * r_dim1];
		      C2F (daxpy) (&ksize, &d__1,
				   &s[k + 1 + (k + 2) * s_dim1], lds,
				   &dwork[k1], &c__1);
		    }
		  else
		    {
		      d__1 = -r__[k + k * r_dim1] * b[0];
		      C2F (daxpy) (&ksize, &d__1, &s[k + (k + 2) * s_dim1],
				   lds, &dwork[1], &c__1);
		      d__1 =
			-(r__[k + (k + 1) * r_dim1] * b[0] +
			  r__[k + 1 + (k + 1) * r_dim1] * b[1]);
		      C2F (daxpy) (&ksize, &d__1,
				   &s[k + 1 + (k + 2) * s_dim1], lds,
				   &dwork[1], &c__1);
		      d__1 = -r__[k + k * r_dim1] * b[2];
		      C2F (daxpy) (&ksize, &d__1, &s[k + (k + 2) * s_dim1],
				   lds, &dwork[k1], &c__1);
		      d__1 =
			-(r__[k + (k + 1) * r_dim1] * b[2] +
			  r__[k + 1 + (k + 1) * r_dim1] * b[3]);
		      C2F (daxpy) (&ksize, &d__1,
				   &s[k + 1 + (k + 2) * s_dim1], lds,
				   &dwork[k1], &c__1);
		    }
		  /* 
		   *                SB03OR  solves the Sylvester equations. The solution 
		   *                is overwritten on DWORK. 
		   * 
		   */
		  nsp_slicot_sb03or (discr, ltrans, &ksize, &c__2,
				     &s[k + 2 + (k + 2) * s_dim1], lds, b,
				     &c__2, &dwork[1], &ksize, &scaloc, info);
		  infom = Max (*info, infom);
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (&j, &scaloc, &r__[j * r_dim1 + 1],
				       &c__1);
			  /* L30: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  /* 
		   *                Copy the solution into the next  2*( n - k - 1 ) 
		   *                elements of  DWORK. 
		   * 
		   */
		  i__1 = ksize << 1;
		  C2F (dcopy) (&i__1, &dwork[1], &c__1, &dwork[k2], &c__1);
		  /* 
		   *                Now form the matrix  Rhat  of equation (6.4) or 
		   *                (10.22). Note that (10.22) is incorrect, so here we 
		   *                implement a corrected version of (10.22). 
		   * 
		   */
		  if (cont)
		    {
		      /* 
		       *                   Swap the two rows of R with DWORK. 
		       * 
		       */
		      C2F (dswap) (&ksize, &dwork[1], &c__1,
				   &r__[k + (k + 2) * r_dim1], ldr);
		      C2F (dswap) (&ksize, &dwork[k1], &c__1,
				   &r__[k + 1 + (k + 2) * r_dim1], ldr);
		      /* 
		       *                   1st column: 
		       * 
		       */
		      d__1 = -a[0];
		      C2F (daxpy) (&ksize, &d__1, &dwork[k2], &c__1,
				   &dwork[1], &c__1);
		      d__1 = -a[2];
		      C2F (daxpy) (&ksize, &d__1, &dwork[k3], &c__1,
				   &dwork[1], &c__1);
		      /* 
		       *                   2nd column: 
		       * 
		       */
		      d__1 = -a[3];
		      C2F (daxpy) (&ksize, &d__1, &dwork[k3], &c__1,
				   &dwork[k1], &c__1);
		    }
		  else
		    {
		      /* 
		       *                   Form  v = S1'*u + s*u11', overwriting  v  on DWORK. 
		       * 
		       *                   Compute  S1'*u,  first multiplying by the 
		       *                   triangular part of  S1. 
		       * 
		       */
		      C2F (dtrmm) ("Left", "Upper", "Transpose", "Non-unit",
				   &ksize, &c__2, &c_b47,
				   &s[k + 2 + (k + 2) * s_dim1], lds,
				   &dwork[1], &ksize, 4L, 5L, 9L, 8L);
		      /* 
		       *                   Then multiply by the subdiagonal of  S1  and add in 
		       *                   to the above result. 
		       * 
		       */
		      j1 = k1;
		      j2 = k + 2;
		      /* 
		       */
		      i__1 = ksize - 1;
		      for (j = 1; j <= i__1; ++j)
			{
			  if (s[j2 + 1 + j2 * s_dim1] != 0.)
			    {
			      dwork[j] =
				s[j2 + 1 + j2 * s_dim1] * dwork[k2 + j] +
				dwork[j];
			      dwork[j1] =
				s[j2 + 1 + j2 * s_dim1] * dwork[k3 + j] +
				dwork[j1];
			    }
			  ++j1;
			  ++j2;
			  /* L40: */
			}
		      /* 
		       *                   Add in s*u11'. 
		       * 
		       */
		      C2F (daxpy) (&ksize, &r__[k + k * r_dim1],
				   &s[k + (k + 2) * s_dim1], lds, &dwork[1],
				   &c__1);
		      C2F (daxpy) (&ksize, &r__[k + (k + 1) * r_dim1],
				   &s[k + 1 + (k + 2) * s_dim1], lds,
				   &dwork[1], &c__1);
		      C2F (daxpy) (&ksize, &r__[k + 1 + (k + 1) * r_dim1],
				   &s[k + 1 + (k + 2) * s_dim1], lds,
				   &dwork[k1], &c__1);
		      /* 
		       *                   Next recover r from R, swapping r with u. 
		       * 
		       */
		      C2F (dswap) (&ksize, &dwork[k2], &c__1,
				   &r__[k + (k + 2) * r_dim1], ldr);
		      C2F (dswap) (&ksize, &dwork[k3], &c__1,
				   &r__[k + 1 + (k + 2) * r_dim1], ldr);
		      /* 
		       *                   Now we perform the QR factorization. 
		       * 
		       *                   ( a ) = Q*( t ), 
		       *                   ( b ) 
		       * 
		       *                   and form 
		       * 
		       *                   ( p' ) = Q'*( r' ). 
		       *                   ( y' )      ( v' ) 
		       * 
		       *                   y  is then the correct vector to use in (10.22). 
		       *                   Note that  a  is upper triangular and that  t  and 
		       *                   p  are not required. 
		       * 
		       */
		      C2F (dlarfg) (&c__3, a, b, &c__1, &tau1);
		      v1 = b[0];
		      t1 = tau1 * v1;
		      v2 = b[1];
		      t2 = tau1 * v2;
		      sum = a[2] + v1 * b[2] + v2 * b[3];
		      b[2] -= sum * t1;
		      b[3] -= sum * t2;
		      C2F (dlarfg) (&c__3, &a[3], &b[2], &c__1, &tau2);
		      v3 = b[2];
		      t3 = tau2 * v3;
		      v4 = b[3];
		      t4 = tau2 * v4;
		      j1 = k1;
		      j2 = k2;
		      j3 = k3;
		      /* 
		       */
		      i__1 = ksize;
		      for (j = 1; j <= i__1; ++j)
			{
			  sum = dwork[j2] + v1 * dwork[j] + v2 * dwork[j1];
			  d1 = dwork[j] - sum * t1;
			  d2 = dwork[j1] - sum * t2;
			  sum = dwork[j3] + v3 * d1 + v4 * d2;
			  dwork[j] = d1 - sum * t3;
			  dwork[j1] = d2 - sum * t4;
			  ++j1;
			  ++j2;
			  ++j3;
			  /* L50: */
			}
		      /* 
		       */
		    }
		  /* 
		   *                Now update  R1  to give  Rhat. 
		   * 
		   */
		  C2F (dcopy) (&ksize, &dwork[1], &c__1, &dwork[k2], &c__1);
		  C2F (dcopy) (&ksize, &dwork[k1], &c__1, &dwork[k3], &c__1);
		  C2F (dcopy) (&ksize, &dwork[k3], &c__1, &dwork[2], &c__2);
		  C2F (dcopy) (&ksize, &dwork[k2], &c__1, &dwork[1], &c__2);
		  nsp_slicot_mb04od ("Full", &ksize, &c__0, &c__2,
				     &r__[k + 2 + (k + 2) * r_dim1], ldr,
				     &dwork[1], &c__2, &dwork[1], &c__1,
				     &dwork[1], &c__1, &dwork[k2], &dwork[k3],
				     4L);
		}
	    }
	  else
	    {
	      /* 
	       *             1-by-1 block. 
	       * 
	       *             Make sure S is stable or convergent and find u11 in 
	       *             equation (5.13) or (10.15). 
	       * 
	       */
	      if (*discr)
		{
		  absskk = (d__1 = s[k + k * s_dim1], Abs (d__1));
		  if (absskk - 1. >= 0.)
		    {
		      *info = 2;
		      return 0;
		    }
		  temp = sqrt ((1. - absskk) * (absskk + 1.));
		}
	      else
		{
		  if (s[k + k * s_dim1] >= 0.)
		    {
		      *info = 2;
		      return 0;
		    }
		  temp = sqrt ((d__1 = s[k + k * s_dim1] * 2., Abs (d__1)));
		}
	      /* 
	       */
	      scaloc = 1.;
	      if (temp < smin)
		{
		  temp = smin;
		  infom = 1;
		}
	      dr = (d__1 = r__[k + k * r_dim1], Abs (d__1));
	      if (temp < 1. && dr > 1.)
		{
		  if (dr > bignum * temp)
		    {
		      scaloc = 1. / dr;
		    }
		}
	      alpha = d_sign (&temp, &r__[k + k * r_dim1]);
	      r__[k + k * r_dim1] /= alpha;
	      if (scaloc != 1.)
		{
		  /* 
		   */
		  i__1 = *n;
		  for (j = 1; j <= i__1; ++j)
		    {
		      C2F (dscal) (&j, &scaloc, &r__[j * r_dim1 + 1], &c__1);
		      /* L60: */
		    }
		  /* 
		   */
		  *scale *= scaloc;
		}
	      /* 
	       *             If we are not at the end of  S  then set up and solve 
	       *             equation (5.14) or (10.16).  ksize is the order of the 
	       *             remainder of  S.  k1 and k2 point to the start of vectors 
	       *             in  DWORK. 
	       * 
	       */
	      if (kount <= *n)
		{
		  ksize = *n - k;
		  k1 = ksize + 1;
		  k2 = ksize + k1;
		  /* 
		   *                Form the right-hand side in DWORK( 1 ),..., 
		   *                DWORK( n - k ). 
		   * 
		   */
		  C2F (dcopy) (&ksize, &r__[k + (k + 1) * r_dim1], ldr,
			       &dwork[1], &c__1);
		  d__1 = -alpha;
		  C2F (dscal) (&ksize, &d__1, &dwork[1], &c__1);
		  if (cont)
		    {
		      d__1 = -r__[k + k * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k + (k + 1) * s_dim1],
				   lds, &dwork[1], &c__1);
		    }
		  else
		    {
		      d__1 = -s[k + k * s_dim1] * r__[k + k * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k + (k + 1) * s_dim1],
				   lds, &dwork[1], &c__1);
		    }
		  /* 
		   *                SB03OR solves the Sylvester equations. The solution is 
		   *                overwritten on  DWORK. 
		   * 
		   */
		  nsp_slicot_sb03or (discr, ltrans, &ksize, &c__1,
				     &s[k + 1 + (k + 1) * s_dim1], lds,
				     &s[k + k * s_dim1], &c__1, &dwork[1],
				     &ksize, &scaloc, info);
		  infom = Max (*info, infom);
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (&j, &scaloc, &r__[j * r_dim1 + 1],
				       &c__1);
			  /* L70: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  /* 
		   *                Copy the solution into the next  ( n - k ) elements 
		   *                of  DWORK,  copy the solution back into  R  and copy 
		   *                the row of  R  back into  DWORK. 
		   * 
		   */
		  C2F (dcopy) (&ksize, &dwork[1], &c__1, &dwork[k1], &c__1);
		  C2F (dswap) (&ksize, &dwork[1], &c__1,
			       &r__[k + (k + 1) * r_dim1], ldr);
		  /* 
		   *                Now form the matrix  Rhat  of equation (5.15) or 
		   *                (10.17), first computing  y  in  DWORK,  and then 
		   *                updating  R1. 
		   * 
		   */
		  if (cont)
		    {
		      d__1 = -alpha;
		      C2F (daxpy) (&ksize, &d__1, &dwork[k1], &c__1,
				   &dwork[1], &c__1);
		    }
		  else
		    {
		      /* 
		       *                   First form  lambda( 1 )*r  and then add in 
		       *                   alpha*u11*s. 
		       * 
		       */
		      d__1 = -s[k + k * s_dim1];
		      C2F (dscal) (&ksize, &d__1, &dwork[1], &c__1);
		      d__1 = alpha * r__[k + k * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k + (k + 1) * s_dim1],
				   lds, &dwork[1], &c__1);
		      /* 
		       *                   Now form  alpha*S1'*u,  first multiplying by the 
		       *                   sub-diagonal of  S1  and then the triangular part 
		       *                   of  S1,  and add the result in DWORK. 
		       * 
		       */
		      j1 = k + 1;
		      /* 
		       */
		      i__1 = ksize - 1;
		      for (j = 1; j <= i__1; ++j)
			{
			  if (s[j1 + 1 + j1 * s_dim1] != 0.)
			    {
			      dwork[j] =
				alpha * s[j1 + 1 + j1 * s_dim1] * dwork[k1 +
									j] +
				dwork[j];
			    }
			  ++j1;
			  /* L80: */
			}
		      /* 
		       */
		      C2F (dtrmv) ("Upper", "Transpose", "Non-unit", &ksize,
				   &s[k + 1 + (k + 1) * s_dim1], lds,
				   &dwork[k1], &c__1, 5L, 9L, 8L);
		      C2F (daxpy) (&ksize, &alpha, &dwork[k1], &c__1,
				   &dwork[1], &c__1);
		    }
		  nsp_slicot_mb04od ("Full", &ksize, &c__0, &c__1,
				     &r__[k + 1 + (k + 1) * r_dim1], ldr,
				     &dwork[1], &c__1, &dwork[1], &c__1,
				     &dwork[1], &c__1, &dwork[k2], &dwork[k1],
				     4L);
		}
	    }
	  goto L10;
	}
      /*       END WHILE 10 
       * 
       */
    }
  else
    {
      /* 
       *       Case op(M) = M'. 
       * 
       */
      kount = *n;
      /* 
       */
    L90:
      /*       WHILE( KOUNT.GE.1 )LOOP 
       */
      if (kount >= 1)
	{
	  k = kount;
	  if (kount == 1)
	    {
	      tbyt = FALSE;
	      --kount;
	    }
	  else if (s[k + (k - 1) * s_dim1] == 0.)
	    {
	      tbyt = FALSE;
	      --kount;
	    }
	  else
	    {
	      tbyt = TRUE;
	      --k;
	      if (k > 1)
		{
		  if (s[k + (k - 1) * s_dim1] != 0.)
		    {
		      *info = 3;
		      return 0;
		    }
		}
	      kount += -2;
	    }
	  if (tbyt)
	    {
	      /* 
	       *             Solve the two-by-two Lyapunov equation corresponding to 
	       *             (6.1) or (10.19), using the routine SB03OY. 
	       * 
	       */
	      b[0] = s[k + k * s_dim1];
	      b[1] = s[k + 1 + k * s_dim1];
	      b[2] = s[k + (k + 1) * s_dim1];
	      b[3] = s[k + 1 + (k + 1) * s_dim1];
	      u[0] = r__[k + k * r_dim1];
	      u[2] = r__[k + (k + 1) * r_dim1];
	      u[3] = r__[k + 1 + (k + 1) * r_dim1];
	      /* 
	       */
	      nsp_slicot_sb03oy (discr, ltrans, &isgn, b, &c__2, u, &c__2, a,
				 &c__2, &scaloc, info);
	      if (*info > 1)
		{
		  return 0;
		}
	      infom = Max (*info, infom);
	      if (scaloc != 1.)
		{
		  /* 
		   */
		  i__1 = *n;
		  for (j = 1; j <= i__1; ++j)
		    {
		      C2F (dscal) (&j, &scaloc, &r__[j * r_dim1 + 1], &c__1);
		      /* L100: */
		    }
		  /* 
		   */
		  *scale *= scaloc;
		}
	      r__[k + k * r_dim1] = u[0];
	      r__[k + (k + 1) * r_dim1] = u[2];
	      r__[k + 1 + (k + 1) * r_dim1] = u[3];
	      /* 
	       *             If we are not at the front of S then set up and solve 
	       *             equation corresponding to (6.2) or (10.20). 
	       * 
	       *             Note that  SB03OY  returns  ( inv( u11 )*s11*u11 ) in  B 
	       *             and returns scaled alpha, alpha = inv( u11 )*r11, in  A. 
	       *             ksize is the order of the remainder leading part of  S. 
	       *             k1, k2 and k3 point to the start of vectors in  DWORK. 
	       * 
	       */
	      if (kount >= 1)
		{
		  ksize = k - 1;
		  k1 = ksize + 1;
		  k2 = ksize + k1;
		  k3 = ksize + k2;
		  /* 
		   *                Form the right-hand side of equations corresponding to 
		   *                (6.2) or (10.20), the first column in DWORK( 1 ) ,..., 
		   *                DWORK( k - 1 ) the second in DWORK( k ) ,..., 
		   *                DWORK( 2*( k - 1 ) ). 
		   * 
		   */
		  C2F (dcopy) (&ksize, &r__[k * r_dim1 + 1], &c__1, &dwork[1],
			       &c__1);
		  C2F (dcopy) (&ksize, &r__[(k + 1) * r_dim1 + 1], &c__1,
			       &dwork[k1], &c__1);
		  C2F (dtrmm) ("Right", "Upper", "Transpose", "Non-unit",
			       &ksize, &c__2, &c_b19, a, &c__2, &dwork[1],
			       &ksize, 5L, 5L, 9L, 8L);
		  if (cont)
		    {
		      d__1 = -r__[k + k * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k * s_dim1 + 1], &c__1,
				   &dwork[1], &c__1);
		      d__1 = -r__[k + (k + 1) * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k * s_dim1 + 1], &c__1,
				   &dwork[k1], &c__1);
		      d__1 = -r__[k + 1 + (k + 1) * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[(k + 1) * s_dim1 + 1],
				   &c__1, &dwork[k1], &c__1);
		    }
		  else
		    {
		      d__1 =
			-(r__[k + k * r_dim1] * b[0] +
			  r__[k + (k + 1) * r_dim1] * b[2]);
		      C2F (daxpy) (&ksize, &d__1, &s[k * s_dim1 + 1], &c__1,
				   &dwork[1], &c__1);
		      d__1 = -r__[k + 1 + (k + 1) * r_dim1] * b[2];
		      C2F (daxpy) (&ksize, &d__1, &s[(k + 1) * s_dim1 + 1],
				   &c__1, &dwork[1], &c__1);
		      d__1 =
			-(r__[k + k * r_dim1] * b[1] +
			  r__[k + (k + 1) * r_dim1] * b[3]);
		      C2F (daxpy) (&ksize, &d__1, &s[k * s_dim1 + 1], &c__1,
				   &dwork[k1], &c__1);
		      d__1 = -r__[k + 1 + (k + 1) * r_dim1] * b[3];
		      C2F (daxpy) (&ksize, &d__1, &s[(k + 1) * s_dim1 + 1],
				   &c__1, &dwork[k1], &c__1);
		    }
		  /* 
		   *                SB03OR  solves the Sylvester equations. The solution 
		   *                is overwritten on DWORK. 
		   * 
		   */
		  nsp_slicot_sb03or (discr, ltrans, &ksize, &c__2,
				     &s[s_offset], lds, b, &c__2, &dwork[1],
				     &ksize, &scaloc, info);
		  infom = Max (*info, infom);
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (&j, &scaloc, &r__[j * r_dim1 + 1],
				       &c__1);
			  /* L110: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  /* 
		   *                Copy the solution into the next  2*( k - 1 ) elements 
		   *                of  DWORK. 
		   * 
		   */
		  i__1 = ksize << 1;
		  C2F (dcopy) (&i__1, &dwork[1], &c__1, &dwork[k2], &c__1);
		  /* 
		   *                Now form the matrix  Rhat  of equation corresponding 
		   *                to (6.4) or (10.22) (corrected version). 
		   * 
		   */
		  if (cont)
		    {
		      /* 
		       *                   Swap the two columns of R with DWORK. 
		       * 
		       */
		      C2F (dswap) (&ksize, &dwork[1], &c__1,
				   &r__[k * r_dim1 + 1], &c__1);
		      C2F (dswap) (&ksize, &dwork[k1], &c__1,
				   &r__[(k + 1) * r_dim1 + 1], &c__1);
		      /* 
		       *                   1st column: 
		       * 
		       */
		      d__1 = -a[0];
		      C2F (daxpy) (&ksize, &d__1, &dwork[k2], &c__1,
				   &dwork[1], &c__1);
		      /* 
		       *                   2nd column: 
		       * 
		       */
		      d__1 = -a[2];
		      C2F (daxpy) (&ksize, &d__1, &dwork[k2], &c__1,
				   &dwork[k1], &c__1);
		      d__1 = -a[3];
		      C2F (daxpy) (&ksize, &d__1, &dwork[k3], &c__1,
				   &dwork[k1], &c__1);
		    }
		  else
		    {
		      /* 
		       *                   Form  v = S1*u + s*u11, overwriting  v  on DWORK. 
		       * 
		       *                   Compute  S1*u,  first multiplying by the triangular 
		       *                   part of  S1. 
		       * 
		       */
		      C2F (dtrmm) ("Left", "Upper", "No transpose",
				   "Non-unit", &ksize, &c__2, &c_b47,
				   &s[s_offset], lds, &dwork[1], &ksize, 4L,
				   5L, 12L, 8L);
		      /* 
		       *                   Then multiply by the subdiagonal of  S1  and add in 
		       *                   to the above result. 
		       * 
		       */
		      j1 = k1;
		      /* 
		       */
		      i__1 = ksize;
		      for (j = 2; j <= i__1; ++j)
			{
			  ++j1;
			  if (s[j + (j - 1) * s_dim1] != 0.)
			    {
			      dwork[j] =
				s[j + (j - 1) * s_dim1] * dwork[k2 + j - 2] +
				dwork[j];
			      dwork[j1] =
				s[j + (j - 1) * s_dim1] * dwork[k3 + j - 2] +
				dwork[j1];
			    }
			  /* L120: */
			}
		      /* 
		       *                   Add in s*u11. 
		       * 
		       */
		      C2F (daxpy) (&ksize, &r__[k + k * r_dim1],
				   &s[k * s_dim1 + 1], &c__1, &dwork[1],
				   &c__1);
		      C2F (daxpy) (&ksize, &r__[k + (k + 1) * r_dim1],
				   &s[k * s_dim1 + 1], &c__1, &dwork[k1],
				   &c__1);
		      C2F (daxpy) (&ksize, &r__[k + 1 + (k + 1) * r_dim1],
				   &s[(k + 1) * s_dim1 + 1], &c__1,
				   &dwork[k1], &c__1);
		      /* 
		       *                   Next recover r from R, swapping r with u. 
		       * 
		       */
		      C2F (dswap) (&ksize, &dwork[k2], &c__1,
				   &r__[k * r_dim1 + 1], &c__1);
		      C2F (dswap) (&ksize, &dwork[k3], &c__1,
				   &r__[(k + 1) * r_dim1 + 1], &c__1);
		      /* 
		       *                   Now we perform the QL factorization. 
		       * 
		       *                   ( a' ) = Q*( t ), 
		       *                   ( b' ) 
		       * 
		       *                   and form 
		       * 
		       *                   ( p' ) = Q'*( r' ). 
		       *                   ( y' )      ( v' ) 
		       * 
		       *                   y  is then the correct vector to use in the 
		       *                   relation corresponding to (10.22). 
		       *                   Note that  a  is upper triangular and that  t  and 
		       *                   p  are not required. 
		       * 
		       */
		      C2F (dlarfg) (&c__3, &a[3], &b[1], &c__2, &tau1);
		      v1 = b[1];
		      t1 = tau1 * v1;
		      v2 = b[3];
		      t2 = tau1 * v2;
		      sum = a[2] + v1 * b[0] + v2 * b[2];
		      b[0] -= sum * t1;
		      b[2] -= sum * t2;
		      C2F (dlarfg) (&c__3, a, b, &c__2, &tau2);
		      v3 = b[0];
		      t3 = tau2 * v3;
		      v4 = b[2];
		      t4 = tau2 * v4;
		      j1 = k1;
		      j2 = k2;
		      j3 = k3;
		      /* 
		       */
		      i__1 = ksize;
		      for (j = 1; j <= i__1; ++j)
			{
			  sum = dwork[j3] + v1 * dwork[j] + v2 * dwork[j1];
			  d1 = dwork[j] - sum * t1;
			  d2 = dwork[j1] - sum * t2;
			  sum = dwork[j2] + v3 * d1 + v4 * d2;
			  dwork[j] = d1 - sum * t3;
			  dwork[j1] = d2 - sum * t4;
			  ++j1;
			  ++j2;
			  ++j3;
			  /* L130: */
			}
		      /* 
		       */
		    }
		  /* 
		   *                Now update  R1  to give  Rhat. 
		   * 
		   */
		  nsp_slicot_mb04nd ("Full", &ksize, &c__0, &c__2,
				     &r__[r_offset], ldr, &dwork[1], &ksize,
				     &dwork[1], &c__1, &dwork[1], &c__1,
				     &dwork[k2], &dwork[k3], 4L);
		}
	    }
	  else
	    {
	      /* 
	       *             1-by-1 block. 
	       * 
	       *             Make sure S is stable or convergent and find u11 in 
	       *             equation corresponding to (5.13) or (10.15). 
	       * 
	       */
	      if (*discr)
		{
		  absskk = (d__1 = s[k + k * s_dim1], Abs (d__1));
		  if (absskk - 1. >= 0.)
		    {
		      *info = 2;
		      return 0;
		    }
		  temp = sqrt ((1. - absskk) * (absskk + 1.));
		}
	      else
		{
		  if (s[k + k * s_dim1] >= 0.)
		    {
		      *info = 2;
		      return 0;
		    }
		  temp = sqrt ((d__1 = s[k + k * s_dim1] * 2., Abs (d__1)));
		}
	      /* 
	       */
	      scaloc = 1.;
	      if (temp < smin)
		{
		  temp = smin;
		  infom = 1;
		}
	      dr = (d__1 = r__[k + k * r_dim1], Abs (d__1));
	      if (temp < 1. && dr > 1.)
		{
		  if (dr > bignum * temp)
		    {
		      scaloc = 1. / dr;
		    }
		}
	      alpha = d_sign (&temp, &r__[k + k * r_dim1]);
	      r__[k + k * r_dim1] /= alpha;
	      if (scaloc != 1.)
		{
		  /* 
		   */
		  i__1 = *n;
		  for (j = 1; j <= i__1; ++j)
		    {
		      C2F (dscal) (&j, &scaloc, &r__[j * r_dim1 + 1], &c__1);
		      /* L140: */
		    }
		  /* 
		   */
		  *scale *= scaloc;
		}
	      /* 
	       *             If we are not at the front of  S  then set up and solve 
	       *             equation corresponding to (5.14) or (10.16).  ksize is 
	       *             the order of the remainder leading part of  S.  k1 and k2 
	       *             point to the start of vectors in  DWORK. 
	       * 
	       */
	      if (kount >= 1)
		{
		  ksize = k - 1;
		  k1 = ksize + 1;
		  k2 = ksize + k1;
		  /* 
		   *                Form the right-hand side in DWORK( 1 ),..., 
		   *                DWORK( k - 1 ). 
		   * 
		   */
		  C2F (dcopy) (&ksize, &r__[k * r_dim1 + 1], &c__1, &dwork[1],
			       &c__1);
		  d__1 = -alpha;
		  C2F (dscal) (&ksize, &d__1, &dwork[1], &c__1);
		  if (cont)
		    {
		      d__1 = -r__[k + k * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k * s_dim1 + 1], &c__1,
				   &dwork[1], &c__1);
		    }
		  else
		    {
		      d__1 = -s[k + k * s_dim1] * r__[k + k * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k * s_dim1 + 1], &c__1,
				   &dwork[1], &c__1);
		    }
		  /* 
		   *                SB03OR solves the Sylvester equations. The solution is 
		   *                overwritten on  DWORK. 
		   * 
		   */
		  nsp_slicot_sb03or (discr, ltrans, &ksize, &c__1,
				     &s[s_offset], lds, &s[k + k * s_dim1],
				     &c__1, &dwork[1], &ksize, &scaloc, info);
		  infom = Max (*info, infom);
		  if (scaloc != 1.)
		    {
		      /* 
		       */
		      i__1 = *n;
		      for (j = 1; j <= i__1; ++j)
			{
			  C2F (dscal) (&j, &scaloc, &r__[j * r_dim1 + 1],
				       &c__1);
			  /* L150: */
			}
		      /* 
		       */
		      *scale *= scaloc;
		    }
		  /* 
		   *                Copy the solution into the next  ( k - 1 ) elements 
		   *                of  DWORK,  copy the solution back into  R  and copy 
		   *                the column of  R  back into  DWORK. 
		   * 
		   */
		  C2F (dcopy) (&ksize, &dwork[1], &c__1, &dwork[k1], &c__1);
		  C2F (dswap) (&ksize, &dwork[1], &c__1, &r__[k * r_dim1 + 1],
			       &c__1);
		  /* 
		   *                Now form the matrix  Rhat  of equation corresponding 
		   *                to (5.15) or (10.17), first computing  y  in  DWORK, 
		   *                and then updating  R1. 
		   * 
		   */
		  if (cont)
		    {
		      d__1 = -alpha;
		      C2F (daxpy) (&ksize, &d__1, &dwork[k1], &c__1,
				   &dwork[1], &c__1);
		    }
		  else
		    {
		      /* 
		       *                   First form  lambda( 1 )*r  and then add in 
		       *                   alpha*u11*s. 
		       * 
		       */
		      d__1 = -s[k + k * s_dim1];
		      C2F (dscal) (&ksize, &d__1, &dwork[1], &c__1);
		      d__1 = alpha * r__[k + k * r_dim1];
		      C2F (daxpy) (&ksize, &d__1, &s[k * s_dim1 + 1], &c__1,
				   &dwork[1], &c__1);
		      /* 
		       *                   Now form  alpha*S1*u,  first multiplying by the 
		       *                   sub-diagonal of  S1  and then the triangular part 
		       *                   of  S1,  and add the result in DWORK. 
		       * 
		       */
		      i__1 = ksize;
		      for (j = 2; j <= i__1; ++j)
			{
			  if (s[j + (j - 1) * s_dim1] != 0.)
			    {
			      dwork[j] =
				alpha * s[j + (j - 1) * s_dim1] * dwork[k1 +
									j -
									2] +
				dwork[j];
			    }
			  /* L160: */
			}
		      /* 
		       */
		      C2F (dtrmv) ("Upper", "No transpose", "Non-unit",
				   &ksize, &s[s_offset], lds, &dwork[k1],
				   &c__1, 5L, 12L, 8L);
		      C2F (daxpy) (&ksize, &alpha, &dwork[k1], &c__1,
				   &dwork[1], &c__1);
		    }
		  nsp_slicot_mb04nd ("Full", &ksize, &c__0, &c__1,
				     &r__[r_offset], ldr, &dwork[1], &ksize,
				     &dwork[1], &c__1, &dwork[1], &c__1,
				     &dwork[k2], &dwork[k1], 4L);
		}
	    }
	  goto L90;
	}
      /*       END WHILE 90 
       * 
       */
    }
  *info = infom;
  return 0;
  /**** Last line of SB03OT *** 
   */
}				/* nsp_slicot_sb03ot */
