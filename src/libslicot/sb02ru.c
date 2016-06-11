/* SB02RU.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static double c_b33 = 0.;
static double c_b34 = 1.;
static double c_b57 = -1.;

/* Subroutine */ int
nsp_slicot_sb02ru (char *dico, char *hinv, char *trana, char *uplo, int *n,
		   double *a, int *lda, double *g, int *ldg,
		   double *q, int *ldq, double *s, int *lds,
		   int *iwork, double *dwork, int *ldwork,
		   int *info, long int dico_len, long int hinv_len,
		   long int trana_len, long int uplo_len)
{
  /* System generated locals */
  int a_dim1, a_offset, g_dim1, g_offset, q_dim1, q_offset, s_dim1,
    s_offset, i__1, i__2;

  /* Local variables */
  double temp;
  int i__, j;
  char equed[1];
  int discr;
  double rcond;
  int lhinv, luplo;
  int n2, nj;
  double rconda;
  char tranat[1];
  int notrna;
  double pivotg;
  int np1;

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
   *    To construct the 2n-by-2n Hamiltonian or symplectic matrix S 
   *    associated to the linear-quadratic optimization problem, used to 
   *    solve the continuous- or discrete-time algebraic Riccati equation, 
   *    respectively. 
   * 
   *    For a continuous-time problem, S is defined by 
   * 
   *            ( op(A)   -G    ) 
   *        S = (               ),                                     (1) 
   *            (  -Q   -op(A)' ) 
   * 
   *    and for a discrete-time problem by 
   * 
   *                    -1              -1 
   *            (  op(A)           op(A)  *G       ) 
   *        S = (        -1                   -1   ),                  (2) 
   *            ( Q*op(A)     op(A)' + Q*op(A)  *G ) 
   * 
   *    or 
   *                             -T             -T 
   *            ( op(A) + G*op(A)  *Q   -G*op(A)   ) 
   *        S = (           -T                 -T  ),                  (3) 
   *            (     -op(A)  *Q          op(A)    ) 
   * 
   *    where op(A) = A or A' (A**T), A, G, and Q are n-by-n matrices, 
   *    with G and Q symmetric. Matrix A must be nonsingular in the 
   *    discrete-time case. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    DICO    CHARACTER*1 
   *            Specifies the type of the system as follows: 
   *            = 'C':  Continuous-time system; 
   *            = 'D':  Discrete-time system. 
   * 
   *    HINV    CHARACTER*1 
   *            If DICO = 'D', specifies which of the matrices (2) or (3) 
   *            is constructed, as follows: 
   *            = 'D':  The matrix S in (2) is constructed; 
   *            = 'I':  The (inverse) matrix S in (3) is constructed. 
   *            HINV is not referenced if DICO = 'C'. 
   * 
   *    TRANA   CHARACTER*1 
   *            Specifies the form of op(A) to be used, as follows: 
   *            = 'N':  op(A) = A    (No transpose); 
   *            = 'T':  op(A) = A**T (Transpose); 
   *            = 'C':  op(A) = A**T (Conjugate transpose = Transpose). 
   * 
   *    UPLO    CHARACTER*1 
   *            Specifies which triangle of the matrices G and Q is 
   *            stored, as follows: 
   *            = 'U':  Upper triangle is stored; 
   *            = 'L':  Lower triangle is stored. 
   * 
   *    Input/Output Parameters 
   * 
   *    N       (input) INT 
   *            The order of the matrices A, G, and Q.  N >= 0. 
   * 
   *    A       (input) DOUBLE PRECISION array, dimension (LDA,N) 
   *            The leading N-by-N part of this array must contain the 
   *            matrix A. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A.  LDA >= MAX(1,N). 
   * 
   *    G       (input/output) DOUBLE PRECISION array, dimension (LDG,N) 
   *            On entry, the leading N-by-N upper triangular part (if 
   *            UPLO = 'U') or lower triangular part (if UPLO = 'L') of 
   *            this array must contain the upper triangular part or lower 
   *            triangular part, respectively, of the symmetric matrix G. 
   *            On exit, if DICO = 'D', the leading N-by-N part of this 
   *            array contains the symmetric matrix G fully stored. 
   *            If DICO = 'C', this array is not modified on exit, and the 
   *            strictly lower triangular part (if UPLO = 'U') or strictly 
   *            upper triangular part (if UPLO = 'L') is not referenced. 
   * 
   *    LDG     INT 
   *            The leading dimension of the array G.  LDG >= MAX(1,N). 
   * 
   *    Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N) 
   *            On entry, the leading N-by-N upper triangular part (if 
   *            UPLO = 'U') or lower triangular part (if UPLO = 'L') of 
   *            this array must contain the upper triangular part or lower 
   *            triangular part, respectively, of the symmetric matrix Q. 
   *            On exit, if DICO = 'D', the leading N-by-N part of this 
   *            array contains the symmetric matrix Q fully stored. 
   *            If DICO = 'C', this array is not modified on exit, and the 
   *            strictly lower triangular part (if UPLO = 'U') or strictly 
   *            upper triangular part (if UPLO = 'L') is not referenced. 
   * 
   *    LDQ     INT 
   *            The leading dimension of the array Q.  LDQ >= MAX(1,N). 
   * 
   *    S       (output) DOUBLE PRECISION array, dimension (LDS,2*N) 
   *            If INFO = 0, the leading 2N-by-2N part of this array 
   *            contains the Hamiltonian or symplectic matrix of the 
   *            problem. 
   * 
   *    LDS     INT 
   *            The leading dimension of the array S.  LDS >= MAX(1,2*N). 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (LIWORK), where 
   *            LIWORK >= 0,   if DICO = 'C'; 
   *            LIWORK >= 2*N, if DICO = 'D'. 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if DICO = 'D', DWORK(1) returns the reciprocal 
   *            condition number  RCOND  of the given matrix  A,  and 
   *            DWORK(2) returns the reciprocal pivot growth factor 
   *            norm(A)/norm(U) (see SLICOT Library routine MB02PD). 
   *            If DWORK(2) is much less than 1, then the computed  S 
   *            and  RCOND  could be unreliable. If 0 < INFO <= N, then 
   *            DWORK(2) contains the reciprocal pivot growth factor for 
   *            the leading INFO columns of  A. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= 0,          if DICO = 'C'; 
   *            LDWORK >= MAX(2,6*N), if DICO = 'D'. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = i:  if the leading i-by-i (1 <= i <= N) upper triangular 
   *                  submatrix of A is singular in discrete-time case; 
   *            = N+1:  if matrix A is numerically singular in discrete- 
   *                  time case. 
   * 
   *    METHOD 
   * 
   *    For a continuous-time problem, the 2n-by-2n Hamiltonian matrix (1) 
   *    is constructed. 
   *    For a discrete-time problem, the 2n-by-2n symplectic matrix (2) or 
   *    (3) - the inverse of the matrix in (2) - is constructed. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The discrete-time case needs the inverse of the matrix A, hence 
   *    the routine should not be used when A is ill-conditioned. 
   *                              3 
   *    The algorithm requires 0(n ) floating point operations in the 
   *    discrete-time case. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    This routine is a functionally extended and with improved accuracy 
   *    version of the SLICOT Library routine SB02MU. Transposed problems 
   *    can be dealt with as well. The LU factorization of  op(A)  (with 
   *    no equilibration) and iterative refinement are used for solving 
   *    the various linear algebraic systems involved. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Apr. 1999. 
   * 
   *    REVISIONS 
   * 
   *    - 
   * 
   *    KEYWORDS 
   * 
   *    Algebraic Riccati equation, closed loop system, continuous-time 
   *    system, discrete-time system, optimal regulator, Schur form. 
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
   */
  /* Parameter adjustments */
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  g_dim1 = *ldg;
  g_offset = g_dim1 + 1;
  g -= g_offset;
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  s_dim1 = *lds;
  s_offset = s_dim1 + 1;
  s -= s_offset;
  --iwork;
  --dwork;

  /* Function Body */
  n2 = *n + *n;
  *info = 0;
  discr = C2F (lsame) (dico, "D", 1L, 1L);
  luplo = C2F (lsame) (uplo, "U", 1L, 1L);
  notrna = C2F (lsame) (trana, "N", 1L, 1L);
  if (discr)
    {
      lhinv = C2F (lsame) (hinv, "D", 1L, 1L);
    }
  /* 
   *    Test the input scalar arguments. 
   * 
   */
  if (!discr && !C2F (lsame) (dico, "C", 1L, 1L))
    {
      *info = -1;
    }
  else if (discr)
    {
      if (!lhinv && !C2F (lsame) (hinv, "I", 1L, 1L))
	{
	  *info = -2;
	}
    }
  else if (*info == 0)
    {
      if (!notrna && !C2F (lsame) (trana, "T", 1L, 1L)
	  && !C2F (lsame) (trana, "C", 1L, 1L))
	{
	  *info = -3;
	}
      else if (!luplo && !C2F (lsame) (uplo, "L", 1L, 1L))
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
      else if (*ldg < Max (1, *n))
	{
	  *info = -9;
	}
      else if (*ldq < Max (1, *n))
	{
	  *info = -11;
	}
      else if (*lds < Max (1, n2))
	{
	  *info = -13;
	}
      else			/* if(complicated condition) */
	{
	  /*Computing MAX 
	   */
	  i__1 = 2, i__2 = *n * 6;
	  if (*ldwork < 0 || discr && *ldwork < Max (i__1, i__2))
	    {
	      *info = -16;
	    }
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
      C2F (xerbla) ("SB02RU", &i__1, 6L);
      return 0;
    }
  /* 
   *    Quick return if possible. 
   * 
   */
  if (*n == 0)
    {
      if (discr)
	{
	  dwork[1] = 1.;
	  dwork[2] = 1.;
	}
      return 0;
    }
  /* 
   *    The code tries to exploit data locality as much as possible, 
   *    assuming that LDS is greater than LDA, LDQ, and/or LDG. 
   * 
   */
  if (!discr)
    {
      /* 
       *       Continuous-time case: Construct Hamiltonian matrix column-wise. 
       * 
       *       Copy op(A) in S(1:N,1:N), and construct full Q 
       *       in S(N+1:2*N,1:N) and change the sign. 
       * 
       */
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  if (notrna)
	    {
	      C2F (dcopy) (n, &a[j * a_dim1 + 1], &c__1, &s[j * s_dim1 + 1],
			   &c__1);
	    }
	  else
	    {
	      C2F (dcopy) (n, &a[j + a_dim1], lda, &s[j * s_dim1 + 1], &c__1);
	    }
	  /* 
	   */
	  if (luplo)
	    {
	      /* 
	       */
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  s[*n + i__ + j * s_dim1] = -q[i__ + j * q_dim1];
		  /* L20: */
		}
	      /* 
	       */
	      i__2 = *n;
	      for (i__ = j + 1; i__ <= i__2; ++i__)
		{
		  s[*n + i__ + j * s_dim1] = -q[j + i__ * q_dim1];
		  /* L40: */
		}
	      /* 
	       */
	    }
	  else
	    {
	      /* 
	       */
	      i__2 = j - 1;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  s[*n + i__ + j * s_dim1] = -q[j + i__ * q_dim1];
		  /* L60: */
		}
	      /* 
	       */
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  s[*n + i__ + j * s_dim1] = -q[i__ + j * q_dim1];
		  /* L80: */
		}
	      /* 
	       */
	    }
	  /* L100: */
	}
      /* 
       *       Construct full G in S(1:N,N+1:2*N) and change the sign, and 
       *       construct -op(A)' in S(N+1:2*N,N+1:2*N). 
       * 
       */
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  nj = *n + j;
	  if (luplo)
	    {
	      /* 
	       */
	      i__2 = j;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  s[i__ + nj * s_dim1] = -g[i__ + j * g_dim1];
		  /* L120: */
		}
	      /* 
	       */
	      i__2 = *n;
	      for (i__ = j + 1; i__ <= i__2; ++i__)
		{
		  s[i__ + nj * s_dim1] = -g[j + i__ * g_dim1];
		  /* L140: */
		}
	      /* 
	       */
	    }
	  else
	    {
	      /* 
	       */
	      i__2 = j - 1;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  s[i__ + nj * s_dim1] = -g[j + i__ * g_dim1];
		  /* L160: */
		}
	      /* 
	       */
	      i__2 = *n;
	      for (i__ = j; i__ <= i__2; ++i__)
		{
		  s[i__ + nj * s_dim1] = -g[i__ + j * g_dim1];
		  /* L180: */
		}
	      /* 
	       */
	    }
	  /* 
	   */
	  if (notrna)
	    {
	      /* 
	       */
	      i__2 = *n;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  s[*n + i__ + nj * s_dim1] = -a[j + i__ * a_dim1];
		  /* L200: */
		}
	      /* 
	       */
	    }
	  else
	    {
	      /* 
	       */
	      i__2 = *n;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  s[*n + i__ + nj * s_dim1] = -a[i__ + j * a_dim1];
		  /* L220: */
		}
	      /* 
	       */
	    }
	  /* L240: */
	}
      /* 
       */
    }
  else
    {
      /* 
       *       Discrete-time case: Construct the symplectic matrix (2) or (3). 
       * 
       *       Fill in the remaining triangles of the symmetric matrices Q 
       *       and G. 
       * 
       */
      nsp_slicot_ma02ed (uplo, n, &q[q_offset], ldq, 1L);
      nsp_slicot_ma02ed (uplo, n, &g[g_offset], ldg, 1L);
      /* 
       *       Prepare the construction of S in (2) or (3). 
       * 
       */
      np1 = *n + 1;
      if (notrna)
	{
	  *(unsigned char *) tranat = 'T';
	}
      else
	{
	  *(unsigned char *) tranat = 'N';
	}
      /* 
       *       Solve  op(A)'*X = Q  in  S(N+1:2*N,1:N),  using the LU 
       *       factorization of  op(A),  obtained in  S(1:N,1:N),  and 
       *       iterative refinement. No equilibration of  A  is used. 
       *       Workspace:  6*N. 
       * 
       */
      nsp_slicot_mb02pd ("No equilibration", tranat, n, n, &a[a_offset], lda,
			 &s[s_offset], lds, &iwork[1], equed, &dwork[1],
			 &dwork[1], &q[q_offset], ldq, &s[np1 + s_dim1], lds,
			 &rcond, &dwork[1], &dwork[np1], &iwork[np1],
			 &dwork[n2 + 1], info, 16L, 1L, 1L);
      /* 
       *       Return if the matrix is exactly singular or singular to 
       *       working precision. 
       * 
       */
      if (*info > 0)
	{
	  dwork[1] = rcond;
	  dwork[2] = dwork[n2 + 1];
	  return 0;
	}
      /* 
       */
      rconda = rcond;
      pivotg = dwork[n2 + 1];
      /* 
       */
      if (lhinv)
	{
	  /* 
	   *          Complete the construction of S in (2). 
	   * 
	   *          Transpose  X  in-situ. 
	   * 
	   */
	  i__1 = *n - 1;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = *n - j;
	      C2F (dswap) (&i__2, &s[np1 + j + j * s_dim1], &c__1,
			   &s[*n + j + (j + 1) * s_dim1], lds);
	      /* L260: */
	    }
	  /* 
	   *          Solve  op(A)*X = I_n  in  S(N+1:2*N,N+1:2*N),  using the LU 
	   *          factorization of  op(A),  computed in  S(1:N,1:N),  and 
	   *          iterative refinement. 
	   * 
	   */
	  C2F (dlaset) ("Full", n, n, &c_b33, &c_b34, &s[np1 * s_dim1 + 1],
			lds, 4L);
	  nsp_slicot_mb02pd ("Factored", trana, n, n, &a[a_offset], lda,
			     &s[s_offset], lds, &iwork[1], equed, &dwork[1],
			     &dwork[1], &s[np1 * s_dim1 + 1], lds,
			     &s[np1 + np1 * s_dim1], lds, &rcond, &dwork[1],
			     &dwork[np1], &iwork[np1], &dwork[n2 + 1], info,
			     8L, 1L, 1L);
	  /* 
	   *          Solve  op(A)*X = G  in  S(1:N,N+1:2*N),  using the LU 
	   *          factorization of  op(A),  computed in  S(1:N,1:N),  and 
	   *          iterative refinement. 
	   * 
	   */
	  nsp_slicot_mb02pd ("Factored", trana, n, n, &a[a_offset], lda,
			     &s[s_offset], lds, &iwork[1], equed, &dwork[1],
			     &dwork[1], &g[g_offset], ldg,
			     &s[np1 * s_dim1 + 1], lds, &rcond, &dwork[1],
			     &dwork[np1], &iwork[np1], &dwork[n2 + 1], info,
			     8L, 1L, 1L);
	  /* 
	   *                     -1 
	   *          Copy  op(A)    from  S(N+1:2*N,N+1:2*N)  in  S(1:N,1:N). 
	   * 
	   */
	  C2F (dlacpy) ("Full", n, n, &s[np1 + np1 * s_dim1], lds,
			&s[s_offset], lds, 4L);
	  /* 
	   *                                   -1 
	   *          Compute  op(A)' + Q*op(A)  *G  in  S(N+1:2*N,N+1:2*N). 
	   * 
	   */
	  if (notrna)
	    {
	      nsp_slicot_ma02ad ("Full", n, n, &a[a_offset], lda,
				 &s[np1 + np1 * s_dim1], lds, 4L);
	    }
	  else
	    {
	      C2F (dlacpy) ("Full", n, n, &a[a_offset], lda,
			    &s[np1 + np1 * s_dim1], lds, 4L);
	    }
	  C2F (dgemm) ("No transpose", "No transpose", n, n, n, &c_b34,
		       &q[q_offset], ldq, &s[np1 * s_dim1 + 1], lds, &c_b34,
		       &s[np1 + np1 * s_dim1], lds, 12L, 12L);
	  /* 
	   */
	}
      else
	{
	  /* 
	   *          Complete the construction of S in (3). 
	   * 
	   *          Change the sign of  X. 
	   * 
	   */
	  i__1 = *n;
	  for (j = 1; j <= i__1; ++j)
	    {
	      /* 
	       */
	      i__2 = n2;
	      for (i__ = np1; i__ <= i__2; ++i__)
		{
		  s[i__ + j * s_dim1] = -s[i__ + j * s_dim1];
		  /* L280: */
		}
	      /* 
	       */
	      /* L300: */
	    }
	  /* 
	   *          Solve  op(A)'*X = I_n  in  S(N+1:2*N,N+1:2*N),  using the LU 
	   *          factorization of  op(A),  computed in  S(1:N,1:N),  and 
	   *          iterative refinement. 
	   * 
	   */
	  C2F (dlaset) ("Full", n, n, &c_b33, &c_b34, &s[np1 * s_dim1 + 1],
			lds, 4L);
	  nsp_slicot_mb02pd ("Factored", tranat, n, n, &a[a_offset], lda,
			     &s[s_offset], lds, &iwork[1], equed, &dwork[1],
			     &dwork[1], &s[np1 * s_dim1 + 1], lds,
			     &s[np1 + np1 * s_dim1], lds, &rcond, &dwork[1],
			     &dwork[np1], &iwork[np1], &dwork[n2 + 1], info,
			     8L, 1L, 1L);
	  /* 
	   *          Solve  op(A)*X' = -G  in  S(1:N,N+1:2*N),  using the LU 
	   *          factorization of  op(A),  obtained in  S(1:N,1:N),  and 
	   *          iterative refinement. 
	   * 
	   */
	  nsp_slicot_mb02pd ("Factored", trana, n, n, &a[a_offset], lda,
			     &s[s_offset], lds, &iwork[1], equed, &dwork[1],
			     &dwork[1], &g[g_offset], ldg,
			     &s[np1 * s_dim1 + 1], lds, &rcond, &dwork[1],
			     &dwork[np1], &iwork[np1], &dwork[n2 + 1], info,
			     8L, 1L, 1L);
	  /* 
	   *          Change the sign of  X  and transpose it in-situ. 
	   * 
	   */
	  i__1 = n2;
	  for (j = np1; j <= i__1; ++j)
	    {
	      /* 
	       */
	      i__2 = *n;
	      for (i__ = 1; i__ <= i__2; ++i__)
		{
		  temp = -s[i__ + j * s_dim1];
		  s[i__ + j * s_dim1] = -s[j - *n + (i__ + *n) * s_dim1];
		  s[j - *n + (i__ + *n) * s_dim1] = temp;
		  /* L320: */
		}
	      /* 
	       */
	      /* L340: */
	    }
	  /*                                  -T 
	   *          Compute  op(A) + G*op(A)  *Q  in  S(1:N,1:N). 
	   * 
	   */
	  if (notrna)
	    {
	      C2F (dlacpy) ("Full", n, n, &a[a_offset], lda, &s[s_offset],
			    lds, 4L);
	    }
	  else
	    {
	      nsp_slicot_ma02ad ("Full", n, n, &a[a_offset], lda,
				 &s[s_offset], lds, 4L);
	    }
	  C2F (dgemm) ("No transpose", "No transpose", n, n, n, &c_b57,
		       &g[g_offset], ldg, &s[np1 + s_dim1], lds, &c_b34,
		       &s[s_offset], lds, 12L, 12L);
	  /* 
	   */
	}
      dwork[1] = rconda;
      dwork[2] = pivotg;
    }
  return 0;
  /* 
**** Last line of SB02RU *** 
*/
}				/* nsp_slicot_sb02ru */
