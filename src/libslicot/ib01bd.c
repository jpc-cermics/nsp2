/* IB01BD.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static double c_b21 = 0.;
static int c__0 = 0;

/* Subroutine */ int
nsp_slicot_ib01bd (char *meth, char *job, char *jobck, int *nobr, int *n,
		   int *m, int *l, int *nsmpl, double *r__,
		   int *ldr, double *a, int *lda, double *c__,
		   int *ldc, double *b, int *ldb, double *d__,
		   int *ldd, double *q, int *ldq, double *ry,
		   int *ldry, double *s, int *lds, double *k,
		   int *ldk, double *tol, int *iwork, double *dwork,
		   int *ldwork, int *bwork, int *iwarn, int *info,
		   long int meth_len, long int job_len, long int jobck_len)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1,
    d_offset, k_dim1, k_offset, q_dim1, q_offset, r_dim1, r_offset, ry_dim1,
    ry_offset, s_dim1, s_offset, i__1, i__2, i__3, i__4, i__5;

  /* Local variables */
  double rcnd[8], ferr;
  int ierr;
  int n4sid;
  int i__;
  char jobbd[1];
  int ifact;
  char jobcv[1];
  double rcond;
  int lnobr, mnobr;
  int withb, withc;
  int ldunn;
  int withd, moesp, withk;
  int jwork;
  double rnorm;
  int n2, ia, ic, id, ig, ik, io, ll, iq, ir, is, it, nl, iv, nn, ix, nr;
  int combin;
  int oufact[2];
  char jobcov[1];
  double rcondr;
  int withal;
  int lmnobr, mnobrn, iwarnl;
  int withco;
  int lmmnol, minwrk, maxwrk, iaw;
  double sep;
  int iwi, npl, iwr;

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
   *    To estimate the system matrices A, C, B, and D, the noise 
   *    covariance matrices Q, Ry, and S, and the Kalman gain matrix K 
   *    of a linear time-invariant state space model, using the 
   *    processed triangular factor R of the concatenated block Hankel 
   *    matrices, provided by SLICOT Library routine IB01AD. 
   * 
   *    ARGUMENTS 
   * 
   *    Mode Parameters 
   * 
   *    METH    CHARACTER*1 
   *            Specifies the subspace identification method to be used, 
   *            as follows: 
   *            = 'M':  MOESP  algorithm with past inputs and outputs; 
   *            = 'N':  N4SID  algorithm; 
   *            = 'C':  combined method:  MOESP  algorithm for finding the 
   *                    matrices A and C, and  N4SID  algorithm for 
   *                    finding the matrices B and D. 
   * 
   *    JOB     CHARACTER*1 
   *            Specifies which matrices should be computed, as follows: 
   *            = 'A':  compute all system matrices, A, B, C, and D; 
   *            = 'C':  compute the matrices A and C only; 
   *            = 'B':  compute the matrix B only; 
   *            = 'D':  compute the matrices B and D only. 
   * 
   *    JOBCK   CHARACTER*1 
   *            Specifies whether or not the covariance matrices and the 
   *            Kalman gain matrix are to be computed, as follows: 
   *            = 'C':  the covariance matrices only should be computed; 
   *            = 'K':  the covariance matrices and the Kalman gain 
   *                    matrix should be computed; 
   *            = 'N':  the covariance matrices and the Kalman gain matrix 
   *                    should not be computed. 
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
   *    NSMPL   (input) INT 
   *            If  JOBCK = 'C' or 'K',  the total number of samples used 
   *            for calculating the covariance matrices. 
   *            NSMPL >= 2*(M+L)*NOBR. 
   *            This parameter is not meaningful if  JOBCK = 'N'. 
   * 
   *    R       (input/workspace) DOUBLE PRECISION array, dimension 
   *            ( LDR,2*(M+L)*NOBR ) 
   *            On entry, the leading  2*(M+L)*NOBR-by-2*(M+L)*NOBR  part 
   *            of this array must contain the relevant data for the MOESP 
   *            or N4SID algorithms, as constructed by SLICOT Library 
   *            routine IB01AD. Let  R_ij,  i,j = 1:4,  be the 
   *            ij submatrix of  R  (denoted  S  in IB01AD),  partitioned 
   *            by  M*NOBR,  L*NOBR,  M*NOBR,  and  L*NOBR  rows and 
   *            columns. The submatrix  R_22  contains the matrix of left 
   *            singular vectors used. Also needed, for  METH = 'N'  or 
   *            JOBCK <> 'N',  are the submatrices  R_11,  R_14 : R_44, 
   *            and, for  METH = 'M' or 'C'  and  JOB <> 'C', the 
   *            submatrices  R_31  and  R_12,  containing the processed 
   *            matrices  R_1c  and  R_2c,  respectively, as returned by 
   *            SLICOT Library routine IB01AD. 
   *            Moreover, if  METH = 'N'  and  JOB = 'A' or 'C',  the 
   *            block-row  R_41 : R_43  must contain the transpose of the 
   *            block-column  R_14 : R_34  as returned by SLICOT Library 
   *            routine IB01AD. 
   *            The remaining part of  R  is used as workspace. 
   *            On exit, part of this array is overwritten. Specifically, 
   *            if  METH = 'M',  R_22  and  R_31  are overwritten if 
   *                JOB = 'B' or 'D',  and  R_12,  R_22,  R_14 : R_34, 
   *                and possibly  R_11  are overwritten if  JOBCK <> 'N'; 
   *            if  METH = 'N',  all needed submatrices are overwritten. 
   *            The details of the contents of  R  need not be known if 
   *            this routine is called once just after calling the SLICOT 
   *            Library routine IB01AD. 
   * 
   *    LDR     INT 
   *            The leading dimension of the array  R. 
   *            LDR >= 2*(M+L)*NOBR. 
   * 
   *    A       (input or output) DOUBLE PRECISION array, dimension 
   *            (LDA,N) 
   *            On entry, if  METH = 'N' or 'C'  and  JOB = 'B' or 'D', 
   *            the leading N-by-N part of this array must contain the 
   *            system state matrix. 
   *            If  METH = 'M'  or  (METH = 'N' or 'C'  and JOB = 'A' 
   *            or 'C'),  this array need not be set on input. 
   *            On exit, if  JOB = 'A' or 'C'  and  INFO = 0,  the 
   *            leading N-by-N part of this array contains the system 
   *            state matrix. 
   * 
   *    LDA     INT 
   *            The leading dimension of the array A. 
   *            LDA >= N,  if  JOB = 'A' or 'C',  or  METH = 'N' or 'C' 
   *                           and  JOB = 'B' or 'D'; 
   *            LDA >= 1,  otherwise. 
   * 
   *    C       (input or output) DOUBLE PRECISION array, dimension 
   *            (LDC,N) 
   *            On entry, if  METH = 'N' or 'C'  and  JOB = 'B' or 'D', 
   *            the leading L-by-N part of this array must contain the 
   *            system output matrix. 
   *            If  METH = 'M'  or  (METH = 'N' or 'C'  and JOB = 'A' 
   *            or 'C'),  this array need not be set on input. 
   *            On exit, if  JOB = 'A' or 'C'  and  INFO = 0,  or 
   *            INFO = 3  (or  INFO >= 0,  for  METH = 'M'),  the leading 
   *            L-by-N part of this array contains the system output 
   *            matrix. 
   * 
   *    LDC     INT 
   *            The leading dimension of the array C. 
   *            LDC >= L,  if  JOB = 'A' or 'C',  or  METH = 'N' or 'C' 
   *                           and  JOB = 'B' or 'D'; 
   *            LDC >= 1,  otherwise. 
   * 
   *    B       (output) DOUBLE PRECISION array, dimension (LDB,M) 
   *            If  M > 0,  JOB = 'A', 'B', or 'D'  and  INFO = 0,  the 
   *            leading N-by-M part of this array contains the system 
   *            input matrix. If  M = 0  or  JOB = 'C',  this array is 
   *            not referenced. 
   * 
   *    LDB     INT 
   *            The leading dimension of the array B. 
   *            LDB >= N,  if M > 0 and JOB = 'A', 'B', or 'D'; 
   *            LDB >= 1,  if M = 0 or  JOB = 'C'. 
   * 
   *    D       (output) DOUBLE PRECISION array, dimension (LDD,M) 
   *            If  M > 0,  JOB = 'A' or 'D'  and  INFO = 0,  the leading 
   *            L-by-M part of this array contains the system input-output 
   *            matrix. If  M = 0  or  JOB = 'C' or 'B',  this array is 
   *            not referenced. 
   * 
   *    LDD     INT 
   *            The leading dimension of the array D. 
   *            LDD >= L,  if M > 0 and JOB = 'A' or 'D'; 
   *            LDD >= 1,  if M = 0 or  JOB = 'C' or 'B'. 
   * 
   *    Q       (output) DOUBLE PRECISION array, dimension (LDQ,N) 
   *            If  JOBCK = 'C' or 'K',  the leading N-by-N part of this 
   *            array contains the positive semidefinite state covariance 
   *            matrix. If  JOBCK = 'K',  this matrix has been used as 
   *            state weighting matrix for computing the Kalman gain. 
   *            This parameter is not referenced if JOBCK = 'N'. 
   * 
   *    LDQ     INT 
   *            The leading dimension of the array Q. 
   *            LDQ >= N,  if JOBCK = 'C' or 'K'; 
   *            LDQ >= 1,  if JOBCK = 'N'. 
   * 
   *    RY      (output) DOUBLE PRECISION array, dimension (LDRY,L) 
   *            If  JOBCK = 'C' or 'K',  the leading L-by-L part of this 
   *            array contains the positive (semi)definite output 
   *            covariance matrix. If  JOBCK = 'K',  this matrix has been 
   *            used as output weighting matrix for computing the Kalman 
   *            gain. 
   *            This parameter is not referenced if JOBCK = 'N'. 
   * 
   *    LDRY    INT 
   *            The leading dimension of the array RY. 
   *            LDRY >= L,  if JOBCK = 'C' or 'K'; 
   *            LDRY >= 1,  if JOBCK = 'N'. 
   * 
   *    S       (output) DOUBLE PRECISION array, dimension (LDS,L) 
   *            If  JOBCK = 'C' or 'K',  the leading N-by-L part of this 
   *            array contains the state-output cross-covariance matrix. 
   *            If  JOBCK = 'K',  this matrix has been used as state- 
   *            output weighting matrix for computing the Kalman gain. 
   *            This parameter is not referenced if JOBCK = 'N'. 
   * 
   *    LDS     INT 
   *            The leading dimension of the array S. 
   *            LDS >= N,  if JOBCK = 'C' or 'K'; 
   *            LDS >= 1,  if JOBCK = 'N'. 
   * 
   *    K       (output) DOUBLE PRECISION array, dimension ( LDK,L ) 
   *            If  JOBCK = 'K',  the leading  N-by-L  part of this array 
   *            contains the estimated Kalman gain matrix. 
   *            If  JOBCK = 'C' or 'N',  this array is not referenced. 
   * 
   *    LDK     INT 
   *            The leading dimension of the array  K. 
   *            LDK >= N,  if JOBCK = 'K'; 
   *            LDK >= 1,  if JOBCK = 'C' or 'N'. 
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
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension (LIWORK) 
   *            LIWORK >= Max(LIW1,LIW2), where 
   *            LIW1 = N,                     if METH <> 'N' and M = 0 
   *                                       or JOB = 'C' and JOBCK = 'N'; 
   *            LIW1 = M*NOBR+N,              if METH <> 'N', JOB = 'C', 
   *                                          and JOBCK <> 'N'; 
   *            LIW1 = Max(L*NOBR,M*NOBR),    if METH = 'M', JOB <> 'C', 
   *                                          and JOBCK = 'N'; 
   *            LIW1 = Max(L*NOBR,M*NOBR+N),  if METH = 'M', JOB <> 'C', 
   *                                          and JOBCK = 'C' or 'K'; 
   *            LIW1 = Max(M*NOBR+N,M*(N+L)), if METH = 'N', or METH = 'C' 
   *                                          and JOB  <> 'C'; 
   *            LIW2 = 0,                     if JOBCK <> 'K'; 
   *            LIW2 = N*N,                   if JOBCK =  'K'. 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if  INFO = 0,  DWORK(1) returns the optimal value 
   *            of LDWORK,  and  DWORK(2),  DWORK(3),  DWORK(4),  and 
   *            DWORK(5)  contain the reciprocal condition numbers of the 
   *            triangular factors of the following matrices (defined in 
   *            SLICOT Library routine IB01PD and in the lower level 
   *            routines): 
   *               GaL  (GaL = Un(1:(s-1)*L,1:n)), 
   *               R_1c (if  METH = 'M' or 'C'), 
   *               M    (if  JOBCK = 'C' or 'K'  or  METH = 'N'),  and 
   *               Q or T  (see SLICOT Library routine IB01PY or IB01PX), 
   *            respectively. 
   *            If  METH = 'N',  DWORK(3)  is set to one without any 
   *            calculations. Similarly, if  METH = 'M'  and  JOBCK = 'N', 
   *            DWORK(4)  is set to one. If  M = 0  or  JOB = 'C', 
   *            DWORK(3)  and  DWORK(5)  are set to one. 
   *            If  JOBCK = 'K'  and  INFO = 0,  DWORK(6)  to  DWORK(13) 
   *            contain information about the accuracy of the results when 
   *            computing the Kalman gain matrix, as follows: 
   *               DWORK(6)  - reciprocal condition number of the matrix 
   *                           U11  of the Nth order system of algebraic 
   *                           equations from which the solution matrix  X 
   *                           of the Riccati equation is obtained; 
   *               DWORK(7)  - reciprocal pivot growth factor for the LU 
   *                           factorization of the matrix  U11; 
   *               DWORK(8)  - reciprocal condition number of the matrix 
   *                           As = A - S*inv(Ry)*C,  which is inverted by 
   *                           the standard Riccati solver; 
   *               DWORK(9)  - reciprocal pivot growth factor for the LU 
   *                           factorization of the matrix  As; 
   *               DWORK(10) - reciprocal condition number of the matrix 
   *                           Ry; 
   *               DWORK(11) - reciprocal condition number of the matrix 
   *                           Ry + C*X*C'; 
   *               DWORK(12) - reciprocal condition number for the Riccati 
   *                           equation solution; 
   *               DWORK(13) - forward error bound for the Riccati 
   *                           equation solution. 
   *            On exit, if  INFO = -30,  DWORK(1)  returns the minimum 
   *            value of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= Max( LDW1,LDW2,LDW3 ), where, if METH = 'M', 
   *            LDW1 >= Max( 2*(L*NOBR-L)*N+2*N, (L*NOBR-L)*N+N*N+7*N ), 
   *                    if JOB = 'C' or JOB = 'A' and M = 0; 
   *            LDW1 >= Max( 2*(L*NOBR-L)*N+N*N+7*N, 
   *                         (L*NOBR-L)*N+N+6*M*NOBR, (L*NOBR-L)*N+N+ 
   *                         Max( L+M*NOBR, L*NOBR + 
   *                                        Max( 3*L*NOBR+1, M ) ) ), 
   *                    if M > 0 and JOB = 'A', 'B', or 'D'; 
   *            LDW2 >= 0,                          if JOBCK = 'N'; 
   *            LDW2 >= L*NOBR*N+ 
   *                    Max( (L*NOBR-L)*N+Aw+2*N+max(5*N,(2*M+L)*NOBR+L), 
   *                         4*(M*NOBR+N)+1, M*NOBR+2*N+L ), 
   *                                                if JOBCK = 'C' or 'K', 
   *            where Aw = N+N*N, if M = 0 or JOB = 'C'; 
   *                  Aw = 0,     otherwise; 
   *            if METH = 'N', 
   *            LDW1 >= L*NOBR*N+max( (L*NOBR-L)*N+2*N+(2*M+L)*NOBR+L, 
   *                                  2*(L*NOBR-L)*N+N*N+8*N, 
   *                                  N+4*(M*NOBR+N)+1, M*NOBR+3*N+L ); 
   *            LDW2 >= 0, if M = 0 or JOB = 'C'; 
   *            LDW2 >= L*NOBR*N+M*NOBR*(N+L)*(M*(N+L)+1)+ 
   *                               Max( (N+L)**2, 4*M*(N+L)+1 ), 
   *                    if M > 0 and JOB = 'A', 'B', or 'D'; 
   *            and, if METH = 'C', LDW1 as 
   *            Max( LDW1 for METH = 'M', JOB = 'C', LDW1 for METH = 'N'), 
   *            and LDW2 for METH = 'N' are used; 
   *            LDW3 >= 0,                     if JOBCK <> 'K'; 
   *            LDW3 >= Max(  4*N*N+2*N*L+L*L+max( 3*L,N*L ), 
   *                         14*N*N+12*N+5 ),  if JOBCK =  'K'. 
   *            For good performance,  LDWORK  should be larger. 
   * 
   *    BWORK   INT array, dimension (LBWORK) 
   *            LBWORK = 2*N, if JOBCK =  'K'; 
   *            LBWORK = 0,   if JOBCK <> 'K'. 
   * 
   *    Warning Indicator 
   * 
   *    IWARN   INT 
   *            = 0:  no warning; 
   *            = 4:  a least squares problem to be solved has a 
   *                  rank-deficient coefficient matrix; 
   *            = 5:  the computed covariance matrices are too small. 
   *                  The problem seems to be a deterministic one; the 
   *                  gain matrix is set to zero. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 2:  the singular value decomposition (SVD) algorithm did 
   *                  not converge; 
   *            = 3:  a singular upper triangular matrix was found; 
   *            = 3+i:  if  JOBCK = 'K'  and the associated Riccati 
   *                  equation could not be solved, where i = 1,...,6; 
   *                  (see the description of the parameter INFO for the 
   *                  SLICOT Library routine SB02RD for the meaning of 
   *                  the i values); 
   *            = 10: the QR algorithm did not converge. 
   * 
   *    METHOD 
   * 
   *    In the MOESP approach, the matrices  A  and  C  are first 
   *    computed from an estimated extended observability matrix [1], 
   *    and then, the matrices  B  and  D  are obtained by solving an 
   *    extended linear system in a least squares sense. 
   *    In the N4SID approach, besides the estimated extended 
   *    observability matrix, the solutions of two least squares problems 
   *    are used to build another least squares problem, whose solution 
   *    is needed to compute the system matrices  A,  C,  B,  and  D.  The 
   *    solutions of the two least squares problems are also optionally 
   *    used by both approaches to find the covariance matrices. 
   *    The Kalman gain matrix is obtained by solving a discrete-time 
   *    algebraic Riccati equation. 
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
   *    The implemented method consists in numerically stable steps. 
   * 
   *    FURTHER COMMENTS 
   * 
   *    The covariance matrices are computed using the N4SID approach. 
   *    Therefore, for efficiency reasons, it is advisable to set 
   *    METH = 'N',  if the Kalman gain matrix or covariance matrices 
   *    are needed  (JOBCK = 'K', or 'C').  When  JOBCK = 'N',  it could 
   *    be more efficient to use the combined method,  METH = 'C'. 
   *    Often, this combination will also provide better accuracy than 
   *    MOESP algorithm. 
   *    In some applications, it is useful to compute the system matrices 
   *    using two calls to this routine, the first one with  JOB = 'C', 
   *    and the second one with  JOB = 'B' or 'D'.  This is slightly less 
   *    efficient than using a single call with  JOB = 'A',  because some 
   *    calculations are repeated. If  METH = 'N',  all the calculations 
   *    at the first call are performed again at the second call; 
   *    moreover, it is required to save the needed submatrices of  R 
   *    before the first call and restore them before the second call. 
   *    If the covariance matrices and/or the Kalman gain are desired, 
   *    JOBCK  should be set to  'C'  or  'K'  at the second call. 
   *    If  B  and  D  are both needed, they should be computed at once. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Dec. 1999. 
   * 
   *    REVISIONS 
   * 
   *    March 2000, August 2000, Sept. 2001, March 2005. 
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
   *    .. Local Arrays .. 
   *    .. External Functions .. 
   *    .. External Subroutines .. 
   *    .. Intrinsic Functions .. 
   *    .. Executable Statements .. 
   * 
   *    Decode the scalar input parameters. 
   * 
   */
  /* Parameter adjustments */
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *ldc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  b_dim1 = *ldb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  d_dim1 = *ldd;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  ry_dim1 = *ldry;
  ry_offset = ry_dim1 + 1;
  ry -= ry_offset;
  s_dim1 = *lds;
  s_offset = s_dim1 + 1;
  s -= s_offset;
  k_dim1 = *ldk;
  k_offset = k_dim1 + 1;
  k -= k_offset;
  --iwork;
  --dwork;
  --bwork;

  /* Function Body */
  moesp = C2F (lsame) (meth, "M", 1L, 1L);
  n4sid = C2F (lsame) (meth, "N", 1L, 1L);
  combin = C2F (lsame) (meth, "C", 1L, 1L);
  withal = C2F (lsame) (job, "A", 1L, 1L);
  withc = C2F (lsame) (job, "C", 1L, 1L) || withal;
  withd = C2F (lsame) (job, "D", 1L, 1L) || withal;
  withb = C2F (lsame) (job, "B", 1L, 1L) || withd;
  withk = C2F (lsame) (jobck, "K", 1L, 1L);
  withco = C2F (lsame) (jobck, "C", 1L, 1L) || withk;
  mnobr = *m * *nobr;
  lnobr = *l * *nobr;
  lmnobr = lnobr + mnobr;
  mnobrn = mnobr + *n;
  ldunn = (lnobr - *l) * *n;
  lmmnol = lnobr + (mnobr << 1) + *l;
  nr = lmnobr + lmnobr;
  npl = *n + *l;
  n2 = *n + *n;
  nn = *n * *n;
  nl = *n * *l;
  ll = *l * *l;
  minwrk = 1;
  *iwarn = 0;
  *info = 0;
  /* 
   *    Check the scalar input parameters. 
   * 
   */
  if (!(moesp || n4sid || combin))
    {
      *info = -1;
    }
  else if (!(withb || withc))
    {
      *info = -2;
    }
  else if (!(withco || C2F (lsame) (jobck, "N", 1L, 1L)))
    {
      *info = -3;
    }
  else if (*nobr <= 1)
    {
      *info = -4;
    }
  else if (*n <= 0 || *n >= *nobr)
    {
      *info = -5;
    }
  else if (*m < 0)
    {
      *info = -6;
    }
  else if (*l <= 0)
    {
      *info = -7;
    }
  else if (withco && *nsmpl < nr)
    {
      *info = -8;
    }
  else if (*ldr < nr)
    {
      *info = -10;
    }
  else if (*lda < 1 || (withc || withb && !moesp) && *lda < *n)
    {
      *info = -12;
    }
  else if (*ldc < 1 || (withc || withb && !moesp) && *ldc < *l)
    {
      *info = -14;
    }
  else if (*ldb < 1 || withb && *ldb < *n && *m > 0)
    {
      *info = -16;
    }
  else if (*ldd < 1 || withd && *ldd < *l && *m > 0)
    {
      *info = -18;
    }
  else if (*ldq < 1 || withco && *ldq < *n)
    {
      *info = -20;
    }
  else if (*ldry < 1 || withco && *ldry < *l)
    {
      *info = -22;
    }
  else if (*lds < 1 || withco && *lds < *n)
    {
      *info = -24;
    }
  else if (*ldk < 1 || withk && *ldk < *n)
    {
      *info = -26;
    }
  else
    {
      /* 
       *       Compute workspace. 
       *       (Note: Comments in the code beginning "Workspace:" describe the 
       *        minimal amount of workspace needed at that point in the code, 
       *        as well as the preferred amount for good performance.) 
       * 
       */
      iaw = 0;
      minwrk = ldunn + (*n << 2);
      if (!n4sid)
	{
	  id = 0;
	  if (withc)
	    {
	      /*Computing MAX 
	       */
	      i__1 = minwrk, i__2 = (ldunn << 1) + n2, i__1 =
		Max(i__1, i__2), i__2 = ldunn + nn + *n * 7;
	      minwrk = Max (i__1, i__2);
	    }
	}
      else
	{
	  id = *n;
	}
      /* 
       */
      if (*m > 0 && withb || !moesp)
	{
	  /*Computing MAX 
	   */
	  i__1 = minwrk, i__2 = (ldunn << 1) + nn + id + *n * 7;
	  minwrk = Max (i__1, i__2);
	  if (moesp)
	    {
	      /*Computing MAX 
	       *Computing MAX 
	       *Computing MAX 
	       */
	      i__5 = lnobr * 3 + 1;
	      i__3 = *l + mnobr, i__4 = lnobr + Max (i__5, *m);
	      i__1 = minwrk, i__2 = ldunn + *n + mnobr * 6, i__1 =
		Max(i__1, i__2), i__2 = ldunn + *n + Max (i__3, i__4);
	      minwrk = Max (i__1, i__2);
	    }
	}
      else
	{
	  if (!n4sid)
	    {
	      iaw = *n + nn;
	    }
	}
      /* 
       */
      if (!moesp || withco)
	{
	  /*Computing MAX 
	   *Computing MAX 
	   */
	  i__3 = *n * 5;
	  i__1 = minwrk, i__2 = ldunn + iaw + n2 + Max (i__3, lmmnol), i__1 =
	    Max (i__1, i__2), i__2 = id + (mnobrn << 2) + 1, i__1 =
	    Max (i__1, i__2), i__2 = id + mnobrn + npl;
	  minwrk = Max (i__1, i__2);
	  if (!moesp && *m > 0 && withb)
	    {
	      /*Computing MAX 
	       *Computing MAX 
	       *Computing 2nd power 
	       */
	      i__5 = npl;
	      i__3 = i__5 * i__5, i__4 = (*m << 2) * npl + 1;
	      i__1 = minwrk, i__2 =
		mnobr * npl * (*m * npl + 1) + Max (i__3, i__4);
	      minwrk = Max (i__1, i__2);
	    }
	  minwrk = lnobr * *n + minwrk;
	}
      /* 
       */
      if (withk)
	{
	  /*Computing MAX 
	   *Computing MAX 
	   */
	  i__3 = *l * 3;
	  i__1 = minwrk, i__2 =
	    (nn << 2) + (nl << 1) + ll + Max (i__3, nl), i__1 =
	    Max (i__1, i__2), i__2 = nn * 14 + *n * 12 + 5;
	  minwrk = Max (i__1, i__2);
	}
      /* 
       */
      if (*ldwork < minwrk)
	{
	  *info = -30;
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
      C2F (xerbla) ("IB01BD", &i__1, 6L);
      return 0;
    }
  /* 
   */
  if (!withk)
    {
      *(unsigned char *) jobcv = *(unsigned char *) jobck;
    }
  else
    {
      *(unsigned char *) jobcv = 'C';
    }
  /* 
   */
  io = 1;
  if (!moesp || withco)
    {
      jwork = io + lnobr * *n;
    }
  else
    {
      jwork = io;
    }
  maxwrk = minwrk;
  /* 
   *    Call the computational routine for estimating system matrices. 
   * 
   */
  if (!combin)
    {
      i__1 = *ldwork - jwork + 1;
      nsp_slicot_ib01pd (meth, job, jobcv, nobr, n, m, l, nsmpl,
			 &r__[r_offset], ldr, &a[a_offset], lda,
			 &c__[c_offset], ldc, &b[b_offset], ldb,
			 &d__[d_offset], ldd, &q[q_offset], ldq,
			 &ry[ry_offset], ldry, &s[s_offset], lds, &dwork[io],
			 &lnobr, tol, &iwork[1], &dwork[jwork], &i__1, iwarn,
			 info, 1L, 1L, 1L);
      /* 
       */
    }
  else
    {
      /* 
       */
      if (withc)
	{
	  if (withal)
	    {
	      *(unsigned char *) jobcov = 'N';
	    }
	  else
	    {
	      *(unsigned char *) jobcov = *(unsigned char *) jobcv;
	    }
	  i__1 = *ldwork - jwork + 1;
	  nsp_slicot_ib01pd ("MOESP", "C and A", jobcov, nobr, n, m, l, nsmpl,
			     &r__[r_offset], ldr, &a[a_offset], lda,
			     &c__[c_offset], ldc, &b[b_offset], ldb,
			     &d__[d_offset], ldd, &q[q_offset], ldq,
			     &ry[ry_offset], ldry, &s[s_offset], lds,
			     &dwork[io], &lnobr, tol, &iwork[1],
			     &dwork[jwork], &i__1, &iwarnl, info, 5L, 7L, 1L);
	  if (*info != 0)
	    {
	      return 0;
	    }
	  *iwarn = Max (*iwarn, iwarnl);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	}
      /* 
       */
      if (withb)
	{
	  if (!withal)
	    {
	      *(unsigned char *) jobbd = *(unsigned char *) job;
	    }
	  else
	    {
	      *(unsigned char *) jobbd = 'D';
	    }
	  i__1 = *ldwork - jwork + 1;
	  nsp_slicot_ib01pd ("N4SID", jobbd, jobcv, nobr, n, m, l, nsmpl,
			     &r__[r_offset], ldr, &a[a_offset], lda,
			     &c__[c_offset], ldc, &b[b_offset], ldb,
			     &d__[d_offset], ldd, &q[q_offset], ldq,
			     &ry[ry_offset], ldry, &s[s_offset], lds,
			     &dwork[io], &lnobr, tol, &iwork[1],
			     &dwork[jwork], &i__1, &iwarnl, info, 5L, 1L, 1L);
	  *iwarn = Max (*iwarn, iwarnl);
	}
    }
  /* 
   */
  if (*info != 0)
    {
      return 0;
    }
  /*Computing MAX 
   */
  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
  maxwrk = Max (i__1, i__2);
  /* 
   */
  for (i__ = 1; i__ <= 4; ++i__)
    {
      rcnd[i__ - 1] = dwork[jwork + i__];
      /* L10: */
    }
  /* 
   */
  if (withk)
    {
      if (*iwarn == 5)
	{
	  /* 
	   *          The problem seems to be a deterministic one. Set the Kalman 
	   *          gain to zero, set accuracy parameters and return. 
	   * 
	   */
	  C2F (dlaset) ("Full", n, l, &c_b21, &c_b21, &k[k_offset], ldk, 4L);
	  /* 
	   */
	  for (i__ = 6; i__ <= 12; ++i__)
	    {
	      dwork[i__] = 1.;
	      /* L20: */
	    }
	  /* 
	   */
	  dwork[13] = 0.;
	}
      else
	{
	  /* 
	   *          Compute the Kalman gain matrix. 
	   * 
	   *          Convert the optimal problem with coupling weighting terms 
	   *          to a standard problem. 
	   *          Workspace:  need   4*N*N+2*N*L+L*L+max( 3*L,N*L ); 
	   *                      prefer larger. 
	   * 
	   */
	  ix = 1;
	  iq = ix + nn;
	  ia = iq + nn;
	  ig = ia + nn;
	  ic = ig + nn;
	  ir = ic + nl;
	  is = ir + ll;
	  jwork = is + nl;
	  /* 
	   */
	  nsp_slicot_ma02ad ("Full", n, n, &a[a_offset], lda, &dwork[ia], n,
			     4L);
	  nsp_slicot_ma02ad ("Full", l, n, &c__[c_offset], ldc, &dwork[ic], n,
			     4L);
	  C2F (dlacpy) ("Upper", n, n, &q[q_offset], ldq, &dwork[iq], n, 5L);
	  C2F (dlacpy) ("Upper", l, l, &ry[ry_offset], ldry, &dwork[ir], l,
			5L);
	  C2F (dlacpy) ("Full", n, l, &s[s_offset], lds, &dwork[is], n, 4L);
	  /* 
	   */
	  i__1 = *ldwork - jwork + 1;
	  nsp_slicot_sb02mt ("G needed", "Nonzero S", "Not factored", "Upper",
			     n, l, &dwork[ia], n, &dwork[ic], n, &dwork[iq],
			     n, &dwork[ir], l, &dwork[is], n, &iwork[1],
			     &ifact, &dwork[ig], n, &iwork[*l + 1],
			     &dwork[jwork], &i__1, &ierr, 8L, 9L, 12L, 5L);
	  if (ierr != 0)
	    {
	      *info = 3;
	      return 0;
	    }
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  rcondr = dwork[jwork + 1];
	  /* 
	   *          Solve the Riccati equation. 
	   *          Workspace:  need   14*N*N+12*N+5; 
	   *                      prefer larger. 
	   * 
	   */
	  it = ic;
	  iv = it + nn;
	  iwr = iv + nn;
	  iwi = iwr + n2;
	  is = iwi + n2;
	  jwork = is + n2 * n2;
	  /* 
	   */
	  i__1 = *ldwork - jwork + 1;
	  nsp_slicot_sb02rd ("All", "Discrete", "Direct", "NoTranspose",
			     "Upper", "General scaling", "Unstable first",
			     "Not factored", "Reduced", n, &dwork[ia], n,
			     &dwork[it], n, &dwork[iv], n, &dwork[ig], n,
			     &dwork[iq], n, &dwork[ix], n, &sep, &rcond,
			     &ferr, &dwork[iwr], &dwork[iwi], &dwork[is], &n2,
			     &iwork[1], &dwork[jwork], &i__1, &bwork[1],
			     &ierr, 3L, 8L, 6L, 11L, 5L, 15L, 14L, 12L, 7L);
	  /* 
	   */
	  if (ierr != 0 && ierr < 7)
	    {
	      *info = ierr + 3;
	      return 0;
	    }
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  /* 
	   */
	  for (i__ = 1; i__ <= 4; ++i__)
	    {
	      rcnd[i__ + 3] = dwork[jwork + i__];
	      /* L30: */
	    }
	  /* 
	   *          Compute the gain matrix. 
	   *          Workspace:  need   2*N*N+2*N*L+L*L+3*L; 
	   *                      prefer larger. 
	   * 
	   */
	  ia = ix + nn;
	  ic = ia + nn;
	  ir = ic + nl;
	  ik = ir + ll;
	  jwork = ik + nl;
	  /* 
	   */
	  nsp_slicot_ma02ad ("Full", n, n, &a[a_offset], lda, &dwork[ia], n,
			     4L);
	  nsp_slicot_ma02ad ("Full", l, n, &c__[c_offset], ldc, &dwork[ic], n,
			     4L);
	  C2F (dlacpy) ("Upper", l, l, &ry[ry_offset], ldry, &dwork[ir], l,
			5L);
	  /* 
	   */
	  i__1 = *ldwork - jwork + 1;
	  nsp_slicot_sb02nd ("Discrete", "NotFactored", "Upper", "Nonzero S",
			     n, l, &c__0, &dwork[ia], n, &dwork[ic], n,
			     &dwork[ir], l, &iwork[1], &s[s_offset], lds,
			     &dwork[ix], n, &rnorm, &dwork[ik], l, oufact,
			     &iwork[*l + 1], &dwork[jwork], &i__1, &ierr, 8L,
			     11L, 5L, 9L);
	  /* 
	   */
	  if (ierr != 0)
	    {
	      if (ierr <= *l + 1)
		{
		  *info = 3;
		}
	      else if (ierr == *l + 2)
		{
		  *info = 10;
		}
	      return 0;
	    }
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  /* 
	   */
	  nsp_slicot_ma02ad ("Full", l, n, &dwork[ik], l, &k[k_offset], ldk,
			     4L);
	  /* 
	   *          Set the accuracy parameters. 
	   * 
	   */
	  dwork[11] = dwork[jwork + 1];
	  /* 
	   */
	  for (i__ = 6; i__ <= 9; ++i__)
	    {
	      dwork[i__] = rcnd[i__ - 2];
	      /* L40: */
	    }
	  /* 
	   */
	  dwork[10] = rcondr;
	  dwork[12] = rcond;
	  dwork[13] = ferr;
	}
    }
  /* 
   *    Return optimal workspace in  DWORK(1)  and the remaining 
   *    reciprocal condition numbers in the next locations. 
   * 
   */
  dwork[1] = (double) maxwrk;
  /* 
   */
  for (i__ = 2; i__ <= 5; ++i__)
    {
      dwork[i__] = rcnd[i__ - 2];
      /* L50: */
    }
  /* 
   */
  return 0;
  /* 
**** Last line of IB01BD *** 
*/
}				/* nsp_slicot_ib01bd */
