/* IB01PY.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static int c_n1 = -1;
static double c_b45 = .66666666666666663;
static double c_b53 = 1.;
static double c_b56 = 0.;

/* Subroutine */ int
nsp_slicot_ib01py (char *meth, char *job, int *nobr, int *n, int *m,
		   int *l, int *rankr1, double *ul, int *ldul,
		   double *r1, int *ldr1, double *tau1,
		   double *pgal, int *ldpgal, double *k, int *ldk,
		   double *r__, int *ldr, double *h__, int *ldh,
		   double *b, int *ldb, double *d__, int *ldd,
		   double *tol, int *iwork, double *dwork,
		   int *ldwork, int *iwarn, int *info, long int meth_len,
		   long int job_len)
{
  /* System generated locals */
  int b_dim1, b_offset, d_dim1, d_offset, h_dim1, h_offset, k_dim1,
    k_offset, pgal_dim1, pgal_offset, r_dim1, r_offset, r1_dim1, r1_offset,
    ul_dim1, ul_offset, i__1, i__2, i__3, i__4;

  /* Builtin functions */

  /* Local variables */
  int rank, ierr, itau;
  double sval[3], toll;
  int nrow;
  int n4sid;
  int ldun2;
  int i__, j;
  double rcond;
  int nobrh;
  int lnobr, mnobr;
  int withb, withd;
  int moesp;
  int jwork, ji, jl, jm;
  double thresh;
  int minwrk, maxwrk;
  double svlmax;
  int lp1, nrowml;
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
   *    1. To compute the triangular  (QR)  factor of the  p-by-L*s 
   *    structured matrix  Q, 
   * 
   *        [ Q_1s  Q_1,s-1  Q_1,s-2  ...  Q_12  Q_11 ] 
   *        [  0      Q_1s   Q_1,s-1  ...  Q_13  Q_12 ] 
   *    Q = [  0       0       Q_1s   ...  Q_14  Q_13 ], 
   *        [  :       :        :           :     :   ] 
   *        [  0       0        0     ...   0    Q_1s ] 
   * 
   *    and apply the transformations to the p-by-m matrix  Kexpand, 
   * 
   *              [ K_1 ] 
   *              [ K_2 ] 
   *    Kexpand = [ K_3 ], 
   *              [  :  ] 
   *              [ K_s ] 
   * 
   *    where, for MOESP approach (METH = 'M'), p = s*(L*s-n), and 
   *    Q_1i = u2(L*(i-1)+1:L*i,:)'  is  (Ls-n)-by-L,  for  i = 1:s, 
   *    u2 = Un(1:L*s,n+1:L*s),  K_i = K(:,(i-1)*m+1:i*m)  (i = 1:s) 
   *    is  (Ls-n)-by-m, and for N4SID approach (METH = 'N'), p = s*(n+L), 
   *    and 
   * 
   *              [   -L_1|1    ]          [ M_i-1 - L_1|i ] 
   *       Q_11 = [             ],  Q_1i = [               ],  i = 2:s, 
   *              [ I_L - L_2|1 ]          [     -L_2|i    ] 
   * 
   *    are  (n+L)-by-L  matrices, and 
   *    K_i = K(:,(i-1)*m+1:i*m),  i = 1:s,  is  (n+L)-by-m. 
   *    The given matrices are: 
   *    For  METH = 'M',  u2 = Un(1:L*s,n+1:L*s), 
   *                      K(1:Ls-n,1:m*s); 
   * 
   *                          [ L_1|1  ...  L_1|s ] 
   *    For  METH = 'N',  L = [                   ],   (n+L)-by-L*s, 
   *                          [ L_2|1  ...  L_2|s ] 
   * 
   *                      M = [ M_1  ...  M_s-1 ],  n-by-L*(s-1),  and 
   *                      K,                        (n+L)-by-m*s. 
   *                      Matrix M is the pseudoinverse of the matrix GaL, 
   *                      built from the first  n  relevant singular 
   *                      vectors,  GaL = Un(1:L(s-1),1:n),  and computed 
   *                      by SLICOT Library routine IB01PD for METH = 'N'. 
   * 
   *    Matrix  Q  is triangularized  (in  R),  exploiting its structure, 
   *    and the transformations are applied from the left to  Kexpand. 
   * 
   *    2. To estimate the matrices B and D of a linear time-invariant 
   *    (LTI) state space model, using the factor  R,  transformed matrix 
   *    Kexpand, and the singular value decomposition information provided 
   *    by other routines. 
   * 
   *    IB01PY  routine is intended for speed and efficient use of the 
   *    memory space. It is generally not recommended for  METH = 'N',  as 
   *    IB01PX  routine can produce more accurate results. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    METH    CHARACTER*1 
   *            Specifies the subspace identification method to be used, 
   *            as follows: 
   *            = 'M':  MOESP  algorithm with past inputs and outputs; 
   *            = 'N':  N4SID  algorithm. 
   * 
   *    JOB     CHARACTER*1 
   *            Specifies whether or not the matrices B and D should be 
   *            computed, as follows: 
   *            = 'B':  compute the matrix B, but not the matrix D; 
   *            = 'D':  compute both matrices B and D; 
   *            = 'N':  do not compute the matrices B and D, but only the 
   *                    R  factor of  Q  and the transformed Kexpand. 
   * 
   *    Input/Output Parameters 
   * 
   *    NOBR    (input) INT 
   *            The number of block rows,  s,  in the input and output 
   *            Hankel matrices processed by other routines.  NOBR > 1. 
   * 
   *    N       (input) INT 
   *            The order of the system.  NOBR > N > 0. 
   * 
   *    M       (input) INT 
   *            The number of system inputs.  M >= 0. 
   * 
   *    L       (input) INT 
   *            The number of system outputs.  L > 0. 
   * 
   *    RANKR1  (input) INT 
   *            The effective rank of the upper triangular matrix  r1, 
   *            i.e., the triangular QR factor of the matrix  GaL, 
   *            computed by SLICOT Library routine IB01PD. It is also 
   *            the effective rank of the matrix  GaL.  0 <= RANKR1 <= N. 
   *            If  JOB = 'N',  or  M = 0,  or  METH = 'N',  this 
   *            parameter is not used. 
   * 
   *    UL      (input/workspace) DOUBLE PRECISION array, dimension 
   *            ( LDUL,L*NOBR ) 
   *            On entry, if  METH = 'M',  the leading  L*NOBR-by-L*NOBR 
   *            part of this array must contain the matrix  Un  of 
   *            relevant singular vectors. The first  N  columns of  UN 
   *            need not be specified for this routine. 
   *            On entry, if  METH = 'N',  the leading  (N+L)-by-L*NOBR 
   *            part of this array must contain the given matrix  L. 
   *            On exit, the leading  LDF-by-L*(NOBR-1) part of this array 
   *            is overwritten by the matrix  F  of the algorithm in [4], 
   *            where  LDF = MAX( 1, L*NOBR-N-L ), if  METH = 'M'; 
   *                   LDF = N,                    if  METH = 'N'. 
   * 
   *    LDUL    INT 
   *            The leading dimension of the array  UL. 
   *            LDUL >= L*NOBR, if  METH = 'M'; 
   *            LDUL >= N+L,    if  METH = 'N'. 
   * 
   *    R1      (input) DOUBLE PRECISION array, dimension ( LDR1,N ) 
   *            If  JOB <> 'N',  M > 0,  METH = 'M',  and  RANKR1 = N, 
   *            the leading  L*(NOBR-1)-by-N  part of this array must 
   *            contain details of the QR factorization of the matrix 
   *            GaL,  as computed by SLICOT Library routine IB01PD. 
   *            Specifically, the leading N-by-N upper triangular part 
   *            must contain the upper triangular factor  r1  of  GaL, 
   *            and the lower  L*(NOBR-1)-by-N  trapezoidal part, together 
   *            with array TAU1, must contain the factored form of the 
   *            orthogonal matrix  Q1  in the QR factorization of  GaL. 
   *            If  JOB = 'N',  or  M = 0,  or  METH = 'N', or  METH = 'M' 
   *            and  RANKR1 < N,  this array is not referenced. 
   * 
   *    LDR1    INT 
   *            The leading dimension of the array  R1. 
   *            LDR1 >= L*(NOBR-1), if  JOB <> 'N',  M > 0,  METH = 'M', 
   *                                and  RANKR1 = N; 
   *            LDR1 >= 1,          otherwise. 
   * 
   *    TAU1    (input) DOUBLE PRECISION array, dimension ( N ) 
   *            If  JOB <> 'N',  M > 0,  METH = 'M',  and  RANKR1 = N, 
   *            this array must contain the scalar factors of the 
   *            elementary reflectors used in the QR factorization of the 
   *            matrix  GaL,  computed by SLICOT Library routine IB01PD. 
   *            If  JOB = 'N',  or  M = 0,  or  METH = 'N', or  METH = 'M' 
   *            and  RANKR1 < N,  this array is not referenced. 
   * 
   *    PGAL    (input) DOUBLE PRECISION array, dimension 
   *            ( LDPGAL,L*(NOBR-1) ) 
   *            If  METH = 'N',  or  JOB <> 'N',  M > 0,  METH = 'M'  and 
   *            RANKR1 < N,  the leading  N-by-L*(NOBR-1)  part of this 
   *            array must contain the pseudoinverse of the matrix  GaL, 
   *            as computed by SLICOT Library routine IB01PD. 
   *            If  METH = 'M'  and  JOB = 'N',  or  M = 0,  or 
   *            RANKR1 = N,  this array is not referenced. 
   * 
   *    LDPGAL  INT 
   *            The leading dimension of the array  PGAL. 
   *            LDPGAL >= N,  if   METH = 'N',  or  JOB <> 'N',  M > 0, 
   *                          and  METH = 'M'  and RANKR1 < N; 
   *            LDPGAL >= 1,  otherwise. 
   * 
   *    K       (input/output) DOUBLE PRECISION array, dimension 
   *            ( LDK,M*NOBR ) 
   *            On entry, the leading  (p/s)-by-M*NOBR  part of this array 
   *            must contain the given matrix  K  defined above. 
   *            On exit, the leading  (p/s)-by-M*NOBR  part of this array 
   *            contains the transformed matrix  K. 
   * 
   *    LDK     INT 
   *            The leading dimension of the array  K.  LDK >= p/s. 
   * 
   *    R       (output) DOUBLE PRECISION array, dimension ( LDR,L*NOBR ) 
   *            If  JOB = 'N',  or  M = 0,  or  Q  has full rank, the 
   *            leading  L*NOBR-by-L*NOBR  upper triangular part of this 
   *            array contains the  R  factor of the QR factorization of 
   *            the matrix  Q. 
   *            If  JOB <> 'N',  M > 0,  and  Q  has not a full rank, the 
   *            leading  L*NOBR-by-L*NOBR  upper trapezoidal part of this 
   *            array contains details of the complete orhogonal 
   *            factorization of the matrix  Q,  as constructed by SLICOT 
   *            Library routines MB03OD and MB02QY. 
   * 
   *    LDR     INT 
   *            The leading dimension of the array  R.  LDR >= L*NOBR. 
   * 
   *    H       (output) DOUBLE PRECISION array, dimension ( LDH,M ) 
   *            If  JOB = 'N'  or  M = 0,  the leading  L*NOBR-by-M  part 
   *            of this array contains the updated part of the matrix 
   *            Kexpand  corresponding to the upper triangular factor  R 
   *            in the QR factorization of the matrix  Q. 
   *            If  JOB <> 'N',  M > 0,  and  METH = 'N'  or  METH = 'M' 
   *            and  RANKR1 < N,  the leading  L*NOBR-by-M  part of this 
   *            array contains the minimum norm least squares solution of 
   *            the linear system  Q*X = Kexpand,  from which the matrices 
   *            B  and  D  are found. The first  NOBR-1  row blocks of  X 
   *            appear in the reverse order in  H. 
   *            If  JOB <> 'N',  M > 0,  METH = 'M'  and  RANKR1 = N,  the 
   *            leading  L*(NOBR-1)-by-M  part of this array contains the 
   *            matrix product  Q1'*X,  and the subarray 
   *            L*(NOBR-1)+1:L*NOBR-by-M  contains the  corresponding 
   *            submatrix of  X,  with  X  defined in the phrase above. 
   * 
   *    LDH     INT 
   *            The leading dimension of the array  H.  LDH >= L*NOBR. 
   * 
   *    B       (output) DOUBLE PRECISION array, dimension ( LDB,M ) 
   *            If  M > 0,  JOB = 'B' or 'D'  and  INFO = 0,  the leading 
   *            N-by-M part of this array contains the system input 
   *            matrix. 
   *            If  M = 0  or  JOB = 'N',  this array is not referenced. 
   * 
   *    LDB     INT 
   *            The leading dimension of the array B. 
   *            LDB >= N, if  M > 0 and JOB = 'B' or 'D'; 
   *            LDB >= 1, if  M = 0 or  JOB = 'N'. 
   * 
   *    D       (output) DOUBLE PRECISION array, dimension ( LDD,M ) 
   *            If  M > 0,  JOB = 'D'  and  INFO = 0,  the leading 
   *            L-by-M part of this array contains the system input-output 
   *            matrix. 
   *            If  M = 0  or  JOB = 'B'  or  'N',  this array is not 
   *            referenced. 
   * 
   *    LDD     INT 
   *            The leading dimension of the array D. 
   *            LDD >= L, if  M > 0 and JOB = 'D'; 
   *            LDD >= 1, if  M = 0 or  JOB = 'B' or 'N'. 
   * 
   *    Tolerances 
   * 
   *    TOL     DOUBLE PRECISION 
   *            The tolerance to be used for estimating the rank of 
   *            matrices. If the user sets  TOL > 0,  then the given value 
   *            of  TOL  is used as a lower bound for the reciprocal 
   *            condition number;  an m-by-n matrix whose estimated 
   *            condition number is less than  1/TOL  is considered to 
   *            be of full rank.  If the user sets  TOL <= 0,  then an 
   *            implicitly computed, default tolerance, defined by 
   *            TOLDEF = m*n*EPS,  is used instead, where  EPS  is the 
   *            relative machine precision (see LAPACK Library routine 
   *            DLAMCH). 
   *            This parameter is not used if  M = 0  or  JOB = 'N'. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension ( LIWORK ) 
   *            where  LIWORK >= 0,       if  JOB =  'N',  or   M = 0; 
   *                   LIWORK >= L*NOBR,  if  JOB <> 'N',  and  M > 0. 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension ( LDWORK ) 
   *            On exit, if  INFO = 0,  DWORK(1) returns the optimal value 
   *            of  LDWORK,  and, if  JOB <> 'N',  and  M > 0,  DWORK(2) 
   *            contains the reciprocal condition number of the triangular 
   *            factor of the matrix  R. 
   *            On exit, if  INFO = -28,  DWORK(1)  returns the minimum 
   *            value of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= MAX( 2*L, L*NOBR, L+M*NOBR ), 
   *                                        if  JOB = 'N',  or  M = 0; 
   *            LDWORK >= MAX( L+M*NOBR, L*NOBR + MAX( 3*L*NOBR+1, M ) ), 
   *                                        if  JOB <> 'N',  and  M > 0. 
   *            For good performance,  LDWORK  should be larger. 
   * 
   *    Warning Indicator 
   * 
   *    IWARN   INT 
   *            = 0:  no warning; 
   *            = 4:  the least squares problem to be solved has a 
   *                  rank-deficient coefficient matrix. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 3:  a singular upper triangular matrix was found. 
   * 
   *    METHOD 
   * 
   *    The QR factorization is computed exploiting the structure, 
   *    as described in [4]. 
   *    The matrices  B  and  D  are then obtained by solving certain 
   *    linear systems in a least squares sense. 
   * 
   *    REFERENCES 
   * 
   *    [1] Verhaegen M., and Dewilde, P. 
   *        Subspace Model Identification. Part 1: The output-error 
   *        state-space model identification class of algorithms. 
   *        Int. J. Control, 56, pp. 1187-1210, 1992. 
   * 
   *    [2] Van Overschee, P., and De Moor, B. 
   *        N4SID: Two Subspace Algorithms for the Identification 
   *        of Combined Deterministic-Stochastic Systems. 
   *        Automatica, Vol.30, No.1, pp. 75-93, 1994. 
   * 
   *    [3] Van Overschee, P. 
   *        Subspace Identification : Theory - Implementation - 
   *        Applications. 
   *        Ph. D. Thesis, Department of Electrical Engineering, 
   *        Katholieke Universiteit Leuven, Belgium, Feb. 1995. 
   * 
   *    [4] Sima, V. 
   *        Subspace-based Algorithms for Multivariable System 
   *        Identification. 
   *        Studies in Informatics and Control, 5, pp. 335-344, 1996. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The implemented method for computing the triangular factor and 
   *    updating Kexpand is numerically stable. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    The computed matrices B and D are not the least squares solutions 
   *    delivered by either MOESP or N4SID algorithms, except for the 
   *    special case n = s - 1, L = 1. However, the computed B and D are 
   *    frequently good enough estimates, especially for  METH = 'M'. 
   *    Better estimates could be obtained by calling SLICOT Library 
   *    routine IB01PX, but it is less efficient, and requires much more 
   *    workspace. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Oct. 1999. 
   * 
   *    REVISIONS 
   * 
   *    Feb. 2000, Sep. 2001, March 2005. 
   * 
   *    KEYWORDS 
   * 
   *    Identification methods; least squares solutions; multivariable 
   *    systems; QR decomposition; singular value decomposition. 
   * 
   *    ****************************************************************** 
   * 
   *    .. Parameters .. 
   *    .. Scalar Arguments .. 
   *    .. Array Arguments .. 
   *    .. Local Scalars .. 
   *    .. Local Array .. 
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
   *    .. Executable Statements .. 
   * 
   *    Decode the scalar input parameters. 
   * 
   */
  /* Parameter adjustments */
  ul_dim1 = *ldul;
  ul_offset = ul_dim1 + 1;
  ul -= ul_offset;
  r1_dim1 = *ldr1;
  r1_offset = r1_dim1 + 1;
  r1 -= r1_offset;
  --tau1;
  pgal_dim1 = *ldpgal;
  pgal_offset = pgal_dim1 + 1;
  pgal -= pgal_offset;
  k_dim1 = *ldk;
  k_offset = k_dim1 + 1;
  k -= k_offset;
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  h_dim1 = *ldh;
  h_offset = h_dim1 + 1;
  h__ -= h_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  --iwork;
  --dwork;

  /* Function Body */
  moesp = C2F (lsame) (meth, "M", 1L, 1L);
  n4sid = C2F (lsame) (meth, "N", 1L, 1L);
  withd = C2F (lsame) (job, "D", 1L, 1L);
  withb = C2F (lsame) (job, "B", 1L, 1L) || withd;
  mnobr = *m * *nobr;
  lnobr = *l * *nobr;
  ldun2 = lnobr - *l;
  lp1 = *l + 1;
  if (moesp)
    {
      nrow = lnobr - *n;
    }
  else
    {
      nrow = *n + *l;
    }
  nrowml = nrow - *l;
  *iwarn = 0;
  *info = 0;
  /* 
   *    Check the scalar input parameters. 
   * 
   */
  if (!(moesp || n4sid))
    {
      *info = -1;
    }
  else if (!(withb || C2F (lsame) (job, "N", 1L, 1L)))
    {
      *info = -2;
    }
  else if (*nobr <= 1)
    {
      *info = -3;
    }
  else if (*n >= *nobr || *n <= 0)
    {
      *info = -4;
    }
  else if (*m < 0)
    {
      *info = -5;
    }
  else if (*l <= 0)
    {
      *info = -6;
    }
  else if (moesp && withb && *m > 0
	   && ((double) (*rankr1) < 0. || *rankr1 > *n))
    {
      *info = -7;
    }
  else if (moesp && *ldul < lnobr || n4sid && *ldul < nrow)
    {
      *info = -9;
    }
  else if (*ldr1 < 1 || *m > 0 && withb && moesp && *ldr1 < ldun2
	   && *rankr1 == *n)
    {
      *info = -11;
    }
  else if (*ldpgal < 1 || *ldpgal < *n
	   && (n4sid || withb && *m > 0 && (moesp && *rankr1 < *n)))
    {
      *info = -14;
    }
  else if (*ldk < nrow)
    {
      *info = -16;
    }
  else if (*ldr < lnobr)
    {
      *info = -18;
    }
  else if (*ldh < lnobr)
    {
      *info = -20;
    }
  else if (*ldb < 1 || *m > 0 && withb && *ldb < *n)
    {
      *info = -22;
    }
  else if (*ldd < 1 || *m > 0 && withd && *ldd < *l)
    {
      *info = -24;
    }
  else
    {
      /* 
       *       Compute workspace. 
       *       (Note: Comments in the code beginning "Workspace:" describe the 
       *        minimal amount of workspace needed at that point in the code, 
       *        as well as the preferred amount for good performance. 
       *        NB refers to the optimal block size for the immediately 
       *        following subroutine, as returned by ILAENV.) 
       * 
       *Computing MAX 
       */
      i__1 = *l << 1, i__1 = Max (i__1, lnobr), i__2 = *l + mnobr;
      minwrk = Max (i__1, i__2);
      maxwrk = minwrk;
      /*Computing MAX 
       */
      i__1 = maxwrk, i__2 =
	*l + *l * C2F (ilaenv) (&c__1, "DGEQRF", " ", &nrow, l, &c_n1, &c_n1,
				6L, 1L);
      maxwrk = Max (i__1, i__2);
      /*Computing MAX 
       */
      i__1 = maxwrk, i__2 =
	*l + ldun2 * C2F (ilaenv) (&c__1, "DORMQR", "LT", &nrow, &ldun2, l,
				   &c_n1, 6L, 2L);
      maxwrk = Max (i__1, i__2);
      /*Computing MAX 
       */
      i__1 = maxwrk, i__2 =
	*l + mnobr * C2F (ilaenv) (&c__1, "DORMQR", "LT", &nrow, &mnobr, l,
				   &c_n1, 6L, 2L);
      maxwrk = Max (i__1, i__2);
      /* 
       */
      if (*m > 0 && withb)
	{
	  /*Computing MAX 
	   */
	  i__1 = minwrk, i__2 = (lnobr << 2) + 1, i__1 =
	    Max (i__1, i__2), i__2 = lnobr + *m;
	  minwrk = Max (i__1, i__2);
	  /*Computing MAX 
	   */
	  i__1 = Max (minwrk, maxwrk), i__2 =
	    lnobr + *m * C2F (ilaenv) (&c__1, "DORMQR", "LT", &lnobr, m,
				       &lnobr, &c_n1, 6L, 2L);
	  maxwrk = Max (i__1, i__2);
	}
      /* 
       */
      if (*ldwork < minwrk)
	{
	  *info = -28;
	  dwork[1] = (double) minwrk;
	}
    }
  /* 
   *    Return if there are illegal arguments. 
   * 
   */
  if (*info != 0)
    {
      i__1 = -(*info);
      C2F (xerbla) ("IB01PY", &i__1, 6L);
      return 0;
    }
  /* 
   *    Construct in  R  the first block-row of  Q,  i.e., the 
   *    (p/s)-by-L*s  matrix  [ Q_1s  ...  Q_12  Q_11  ],  where 
   *    Q_1i,  defined above, is  (p/s)-by-L,  for  i = 1:s. 
   * 
   */
  if (moesp)
    {
      /* 
       */
      i__1 = *nobr;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_slicot_ma02ad ("Full", l, &nrow,
			     &ul[*l * (i__ - 1) + 1 + (*n + 1) * ul_dim1],
			     ldul,
			     &r__[(*l * (*nobr - i__) + 1) * r_dim1 + 1], ldr,
			     4L);
	  /* L10: */
	}
      /* 
       */
    }
  else
    {
      jl = lnobr;
      jm = ldun2;
      /* 
       */
      i__1 = ldun2;
      i__2 = *l;
      for (ji = 1; i__2 < 0 ? ji >= i__1 : ji <= i__1; ji += i__2)
	{
	  /* 
	   */
	  i__3 = ji;
	  for (j = ji + *l - 1; j >= i__3; --j)
	    {
	      /* 
	       */
	      i__4 = *n;
	      for (i__ = 1; i__ <= i__4; ++i__)
		{
		  r__[i__ + j * r_dim1] =
		    pgal[i__ + jm * pgal_dim1] - ul[i__ + jl * ul_dim1];
		  /* L20: */
		}
	      /* 
	       */
	      i__4 = nrow;
	      for (i__ = *n + 1; i__ <= i__4; ++i__)
		{
		  r__[i__ + j * r_dim1] = -ul[i__ + jl * ul_dim1];
		  /* L30: */
		}
	      /* 
	       */
	      --jl;
	      --jm;
	      /* L40: */
	    }
	  /* 
	   */
	  /* L50: */
	}
      /* 
       */
      i__2 = ldun2 + 1;
      for (j = lnobr; j >= i__2; --j)
	{
	  /* 
	   */
	  i__1 = nrow;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      r__[i__ + j * r_dim1] = -ul[i__ + jl * ul_dim1];
	      /* L60: */
	    }
	  /* 
	   */
	  --jl;
	  r__[*n + j - ldun2 + j * r_dim1] += 1.;
	  /* L70: */
	}
    }
  /* 
   *    Triangularize the submatrix  Q_1s  using an orthogonal matrix  S. 
   *    Workspace: need 2*L, prefer L+L*NB. 
   * 
   */
  itau = 1;
  jwork = itau + *l;
  /* 
   */
  i__2 = *ldwork - jwork + 1;
  C2F (dgeqrf) (&nrow, l, &r__[r_offset], ldr, &dwork[itau], &dwork[jwork],
		&i__2, &ierr);
  /* 
   *    Apply the transformation  S'  to the matrix 
   *    [ Q_1,s-1  ...  Q_11 ].  Therefore, 
   * 
   *                             [ R  P_s-1  P_s-2  ...  P_2  P_1 ] 
   *    S'[ Q_1,s  ...  Q_11 ] = [                                ]. 
   *                             [ 0  F_s-1  F_s-2  ...  F_2  F_1 ] 
   * 
   *    Workspace: need L*NOBR, prefer L+(L*NOBR-L)*NB. 
   * 
   */
  i__2 = *ldwork - jwork + 1;
  C2F (dormqr) ("Left", "Transpose", &nrow, &ldun2, l, &r__[r_offset], ldr,
		&dwork[itau], &r__[lp1 * r_dim1 + 1], ldr, &dwork[jwork],
		&i__2, &ierr, 4L, 9L);
  /* 
   *    Apply the transformation  S'  to each of the submatrices  K_i  of 
   *    Kexpand = [ K_1'  K_2'  ...  K_s' ]',  K_i = K(:,(i-1)*m+1:i*m) 
   *    (i = 1:s)  being  (p/s)-by-m.  Denote  ( H_i'  G_i' )' = S'K_i 
   *    (i = 1:s),  where  H_i  has  L  rows. 
   *    Finally,  H_i  is saved in  H(L*(i-1)+1:L*i,1:m), i = 1:s. 
   *    (G_i  is in  K(L+1:p/s,(i-1)*m+1:i*m),  i = 1:s.) 
   *    Workspace: need L+M*NOBR, prefer L+M*NOBR*NB. 
   * 
   */
  i__2 = *ldwork - jwork + 1;
  C2F (dormqr) ("Left", "Transpose", &nrow, &mnobr, l, &r__[r_offset], ldr,
		&dwork[itau], &k[k_offset], ldk, &dwork[jwork], &i__2, &ierr,
		4L, 9L);
  /* 
   *    Put the rows to be annihilated (matrix F) in  UL(1:p/s-L,1:L*s-L). 
   * 
   */
  C2F (dlacpy) ("Full", &nrowml, &ldun2, &r__[lp1 + lp1 * r_dim1], ldr,
		&ul[ul_offset], ldul, 4L);
  /* 
   *    Now, the structure of the transformed matrices is: 
   * 
   *        [  R   P_s-1  P_s-2  ...  P_2  P_1  ]             [  H_1  ] 
   *        [  0     R    P_s-1  ...  P_3  P_2  ]             [  H_2  ] 
   *        [  0     0      R    ...  P_4  P_3  ]             [  H_3  ] 
   *        [  :     :      :          :    :   ]             [   :   ] 
   *        [  0     0      0    ...   R  P_s-1 ]             [ H_s-1 ] 
   *    Q = [  0     0      0     ...  0    R   ],  Kexpand = [  H_s  ], 
   *        [  0   F_s-1  F_s-2  ...  F_2  F_1  ]             [  G_1  ] 
   *        [  0     0    F_s-1  ...  F_3  F_2  ]             [  G_2  ] 
   *        [  :     :      :          :    :   ]             [   :   ] 
   *        [  0     0      0     ...  0  F_s-1 ]             [ G_s-1 ] 
   *        [  0     0      0     ...  0    0   ]             [  G_s  ] 
   * 
   *    where the block-rows have been permuted, to better exploit the 
   *    structure. The block-rows having  R  on the diagonal are dealt 
   *    with successively in the array  R. 
   *    The  F  submatrices are stored in the array  UL,  as a block-row. 
   * 
   *    Copy  H_1  in  H(1:L,1:m). 
   * 
   */
  C2F (dlacpy) ("Full", l, m, &k[k_offset], ldk, &h__[h_offset], ldh, 4L);
  /* 
   *    Triangularize the transformed matrix exploiting its structure. 
   *    Workspace: need L+MAX(L-1,L*NOBR-2*L,M*(NOBR-1)). 
   * 
   */
  i__2 = *nobr - 1;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      /* 
       *       Copy part of the preceding block-row and then annihilate the 
       *       current submatrix  F_s-i  using an orthogonal matrix modifying 
       *       the corresponding submatrix  R.  Simultaneously, apply the 
       *       transformation to the corresponding block-rows of the matrices 
       *       R  and  F. 
       * 
       */
      i__1 = lnobr - *l * i__;
      C2F (dlacpy) ("Upper", l, &i__1,
		    &r__[*l * (i__ - 1) + 1 + (*l * (i__ - 1) + 1) * r_dim1],
		    ldr, &r__[*l * i__ + 1 + (*l * i__ + 1) * r_dim1], ldr,
		    5L);
      i__1 = lnobr - *l * (i__ + 1);
      nsp_slicot_mb04od ("Full", l, &i__1, &nrowml,
			 &r__[*l * i__ + 1 + (*l * i__ + 1) * r_dim1], ldr,
			 &ul[(*l * (i__ - 1) + 1) * ul_dim1 + 1], ldul,
			 &r__[*l * i__ + 1 + (*l * (i__ + 1) + 1) * r_dim1],
			 ldr, &ul[(*l * i__ + 1) * ul_dim1 + 1], ldul,
			 &dwork[itau], &dwork[jwork], 4L);
      /* 
       *       Apply the transformation to the corresponding block-rows of 
       *       the matrix  G  and copy  quapro_H(i+1)  in  H(L*i+1:L*(i+1),1:m). 
       * 
       */
      i__1 = *l;
      for (j = 1; j <= i__1; ++j)
	{
	  i__3 = *m * (*nobr - i__);
	  nsp_slicot_mb04oy (&nrowml, &i__3,
			     &ul[(*l * (i__ - 1) + j) * ul_dim1 + 1],
			     &dwork[j], &k[j + (*m * i__ + 1) * k_dim1], ldk,
			     &k[lp1 + k_dim1], ldk, &dwork[jwork]);
	  /* L80: */
	}
      /* 
       */
      C2F (dlacpy) ("Full", l, m, &k[(*m * i__ + 1) * k_dim1 + 1], ldk,
		    &h__[*l * i__ + 1 + h_dim1], ldh, 4L);
      /* L90: */
    }
  /* 
   *    Return if only the factorization is needed. 
   * 
   */
  if (*m == 0 || !withb)
    {
      dwork[1] = (double) maxwrk;
      return 0;
    }
  /* 
   *    Set the precision parameters. A threshold value  EPS**(2/3)  is 
   *    used for deciding to use pivoting or not, where  EPS  is the 
   *    relative machine precision (see LAPACK Library routine DLAMCH). 
   * 
   */
  eps = C2F (dlamch) ("Precision", 9L);
  thresh = pow_dd (&eps, &c_b45);
  toll = *tol;
  if (toll <= 0.)
    {
      toll = lnobr * lnobr * eps;
    }
  svlmax = 0.;
  /* 
   *    Compute the reciprocal of the condition number of the triangular 
   *    factor  R  of  Q. 
   *    Workspace: need 3*L*NOBR. 
   * 
   */
  C2F (dtrcon) ("1-norm", "Upper", "NonUnit", &lnobr, &r__[r_offset], ldr,
		&rcond, &dwork[1], &iwork[1], &ierr, 6L, 5L, 7L);
  /* 
   */
  if (rcond > Max (toll, thresh))
    {
      /* 
       *       The triangular factor  R  is considered to be of full rank. 
       *       Solve for  X,  R*X = H. 
       * 
       */
      C2F (dtrsm) ("Left", "Upper", "NoTranspose", "Non-unit", &lnobr, m,
		   &c_b53, &r__[r_offset], ldr, &h__[h_offset], ldh, 4L, 5L,
		   11L, 8L);
    }
  else
    {
      /* 
       *       Rank-deficient triangular factor  R.  Compute the 
       *       minimum-norm least squares solution of  R*X = H  using 
       *       the complete orthogonal factorization of  R. 
       * 
       */
      i__2 = lnobr;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  iwork[i__] = 0;
	  /* L100: */
	}
      /* 
       *       Workspace: need   4*L*NOBR+1; 
       *                  prefer 3*L*NOBR+(L*NOBR+1)*NB. 
       * 
       */
      jwork = itau + lnobr;
      i__2 = lnobr - 1;
      C2F (dlaset) ("Lower", &i__2, &lnobr, &c_b56, &c_b56, &r__[r_dim1 + 2],
		    ldr, 5L);
      i__2 = *ldwork - jwork + 1;
      nsp_slicot_mb03od ("QR", &lnobr, &lnobr, &r__[r_offset], ldr, &iwork[1],
			 &toll, &svlmax, &dwork[itau], &rank, sval,
			 &dwork[jwork], &i__2, &ierr, 2L);
      /*Computing MAX 
       */
      i__2 = maxwrk, i__1 = (int) dwork[jwork] + jwork - 1;
      maxwrk = Max (i__2, i__1);
      /* 
       *       Workspace: need L*NOBR+M; prefer L*NOBR+M*NB. 
       * 
       */
      i__2 = *ldwork - jwork + 1;
      C2F (dormqr) ("Left", "Transpose", &lnobr, m, &lnobr, &r__[r_offset],
		    ldr, &dwork[itau], &h__[h_offset], ldh, &dwork[jwork],
		    &i__2, &ierr, 4L, 9L);
      if (rank < lnobr)
	{
	  /* 
	   *          The least squares problem is rank-deficient. 
	   * 
	   */
	  *iwarn = 4;
	}
      /* 
       *       Workspace: need L*NOBR+max(L*NOBR,M); prefer larger. 
       * 
       */
      i__2 = *ldwork - jwork + 1;
      nsp_slicot_mb02qy (&lnobr, &lnobr, m, &rank, &r__[r_offset], ldr,
			 &iwork[1], &h__[h_offset], ldh, &dwork[itau],
			 &dwork[jwork], &i__2, &ierr);
      /*Computing MAX 
       */
      i__2 = maxwrk, i__1 = (int) dwork[jwork] + jwork - 1;
      maxwrk = Max (i__2, i__1);
    }
  /* 
   *    Construct the matrix  D,  if needed. 
   * 
   */
  if (withd)
    {
      C2F (dlacpy) ("Full", l, m, &h__[ldun2 + 1 + h_dim1], ldh,
		    &d__[d_offset], ldd, 4L);
    }
  /* 
   *    Compute  B  by solving another linear system (possibly in 
   *    a least squares sense). 
   * 
   *    Make a block-permutation of the rows of the right-hand side,  H, 
   *    to construct the matrix 
   * 
   *       [ H(L*(s-2)+1:L*(s-1),:); ... H(L+1:L*2,:); H(1:L),:) ] 
   * 
   *    in  H(1:L*s-L,1:n). 
   * 
   */
  nobrh = *nobr / 2 + *nobr % 2 - 1;
  /* 
   */
  i__2 = *m;
  for (j = 1; j <= i__2; ++j)
    {
      /* 
       */
      i__1 = nobrh;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  C2F (dswap) (l, &h__[*l * (i__ - 1) + 1 + j * h_dim1], &c__1,
		       &h__[*l * (*nobr - i__ - 1) + 1 + j * h_dim1], &c__1);
	  /* L110: */
	}
      /* 
       */
      /* L120: */
    }
  /* 
   *    Solve for  B  the matrix equation  GaL*B = H(1:L*s-L,:),  using 
   *    the available QR factorization of  GaL,  if  METH = 'M'  and 
   *    rank(GaL) = n, or the available pseudoinverse of  GaL,  otherwise. 
   * 
   */
  if (moesp && *rankr1 == *n)
    {
      /* 
       *       The triangular factor  r1  of  GaL  is considered to be of 
       *       full rank. Compute  Q1'*H  in  H  and then solve for  B, 
       *       r1*B = H(1:n,:)  in  B,  where  Q1  is the orthogonal matrix 
       *       in the QR factorization of  GaL. 
       *       Workspace: need M; prefer M*NB. 
       * 
       */
      C2F (dormqr) ("Left", "Transpose", &ldun2, m, n, &r1[r1_offset], ldr1,
		    &tau1[1], &h__[h_offset], ldh, &dwork[1], ldwork, &ierr,
		    4L, 9L);
      /*Computing MAX 
       */
      i__2 = maxwrk, i__1 = (int) dwork[1];
      maxwrk = Max (i__2, i__1);
      /* 
       *       Compute the solution in  B. 
       * 
       */
      C2F (dlacpy) ("Full", n, m, &h__[h_offset], ldh, &b[b_offset], ldb, 4L);
      /* 
       */
      C2F (dtrtrs) ("Upper", "NoTranspose", "NonUnit", n, m, &r1[r1_offset],
		    ldr1, &b[b_offset], ldb, &ierr, 5L, 11L, 7L);
      if (ierr > 0)
	{
	  *info = 3;
	  return 0;
	}
    }
  else
    {
      /* 
       *       Rank-deficient triangular factor  r1.  Use the available 
       *       pseudoinverse of  GaL  for computing  B  from  GaL*B = H. 
       * 
       */
      C2F (dgemm) ("NoTranspose", "NoTranspose", n, m, &ldun2, &c_b53,
		   &pgal[pgal_offset], ldpgal, &h__[h_offset], ldh, &c_b56,
		   &b[b_offset], ldb, 11L, 11L);
    }
  /* 
   *    Return optimal workspace in  DWORK(1)  and reciprocal condition 
   *    number in  DWORK(2). 
   * 
   */
  dwork[1] = (double) maxwrk;
  dwork[2] = rcond;
  /* 
   */
  return 0;
  /* 
**** Last line of IB01PY *** 
*/
}				/* nsp_slicot_ib01py */
