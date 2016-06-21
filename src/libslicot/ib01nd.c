/* IB01ND.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "slicot.h"

/* Table of constant values */

static int c__1 = 1;
static int c_n1 = -1;
static double c_b37 = .66666666666666663;
static int c__0 = 0;
static double c_b50 = 0.;

/* Subroutine */ int
nsp_slicot_ib01nd (char *meth, char *jobd, int *nobr, int *m, int *l,
		   double *r__, int *ldr, double *sv, double *tol,
		   int *iwork, double *dwork, int *ldwork,
		   int *iwarn, int *info, long int meth_len,
		   long int jobd_len)
{
  /* System generated locals */
  int r_dim1, r_offset, i__1, i__2;

  /* Builtin functions */

  /* Local variables */
  int rank, ierr, itau;
  double sval[3], toll;
  int rank1;
  int n4sid;
  int itau2, itau3;
  int i__, j;
  int jobdm;
  int lnobr, mnobr;
  int moesp;
  int jwork;
  double rcond1, rcond2;
  int nr;
  int llmnob, lmmnob, llnobr, lmnobr, mmnobr;
  double thresh;
  int nrsave;
  int minwrk, maxwrk;
  double svlmax;
  int nr2, nr3, nr4;
  double dum[1], eps;

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
   *    To find the singular value decomposition (SVD) giving the system 
   *    order, using the triangular factor of the concatenated block 
   *    Hankel matrices. Related preliminary calculations needed for 
   *    computing the system matrices are also performed. 
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
   *    JOBD    CHARACTER*1 
   *            Specifies whether or not the matrices B and D should later 
   *            be computed using the MOESP approach, as follows: 
   *            = 'M':  the matrices B and D should later be computed 
   *                    using the MOESP approach; 
   *            = 'N':  the matrices B and D should not be computed using 
   *                    the MOESP approach. 
   *            This parameter is not relevant for METH = 'N'. 
   * 
   *    Input/Output Parameters 
   * 
   *    NOBR    (input) INT 
   *            The number of block rows,  s,  in the input and output 
   *            block Hankel matrices.  NOBR > 0. 
   * 
   *    M       (input) INT 
   *            The number of system inputs.  M >= 0. 
   * 
   *    L       (input) INT 
   *            The number of system outputs.  L > 0. 
   * 
   *    R       (input/output) DOUBLE PRECISION array, dimension 
   *            ( LDR,2*(M+L)*NOBR ) 
   *            On entry, the leading 2*(M+L)*NOBR-by-2*(M+L)*NOBR upper 
   *            triangular part of this array must contain the upper 
   *            triangular factor R from the QR factorization of the 
   *            concatenated block Hankel matrices. Denote  R_ij, 
   *            i,j = 1:4,  the ij submatrix of  R,  partitioned by 
   *            M*NOBR,  M*NOBR,  L*NOBR,  and  L*NOBR  rows and columns. 
   *            On exit, if INFO = 0, the leading 
   *            2*(M+L)*NOBR-by-2*(M+L)*NOBR upper triangular part of this 
   *            array contains the matrix S, the processed upper 
   *            triangular factor R, as required by other subroutines. 
   *            Specifically, let  S_ij, i,j = 1:4,  be the ij submatrix 
   *            of  S,  partitioned by  M*NOBR,  L*NOBR,  M*NOBR,  and 
   *            L*NOBR  rows and columns. The submatrix  S_22  contains 
   *            the matrix of left singular vectors needed subsequently. 
   *            Useful information is stored in  S_11  and in the 
   *            block-column  S_14 : S_44.  For METH = 'M' and JOBD = 'M', 
   *            the upper triangular part of  S_31  contains the upper 
   *            triangular factor in the QR factorization of the matrix 
   *            R_1c = [ R_12'  R_22'  R_11' ]',  and  S_12  contains the 
   *            corresponding leading part of the transformed matrix 
   *            R_2c = [ R_13'  R_23'  R_14' ]'.  For  METH = 'N',  the 
   *            subarray  S_41 : S_43  contains the transpose of the 
   *            matrix contained in  S_14 : S_34. 
   * 
   *    LDR     INT 
   *            The leading dimension of the array  R. 
   *            LDR >= MAX( 2*(M+L)*NOBR, 3*M*NOBR ), 
   *                                 for METH = 'M' and JOBD = 'M'; 
   *            LDR >= 2*(M+L)*NOBR, for METH = 'M' and JOBD = 'N' or 
   *                                 for METH = 'N'. 
   * 
   *    SV      (output) DOUBLE PRECISION array, dimension ( L*NOBR ) 
   *            The singular values of the relevant part of the triangular 
   *            factor from the QR factorization of the concatenated block 
   *            Hankel matrices. 
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
   *            This parameter is not used for  METH = 'M'. 
   * 
   *    Workspace 
   * 
   *    IWORK   INT array, dimension ((M+L)*NOBR) 
   *            This parameter is not referenced for METH = 'M'. 
   * 
   *    DWORK   DOUBLE PRECISION array, dimension (LDWORK) 
   *            On exit, if  INFO = 0,  DWORK(1) returns the optimal value 
   *            of LDWORK,  and, for  METH = 'N',  DWORK(2)  and  DWORK(3) 
   *            contain the reciprocal condition numbers of the 
   *            triangular factors of the matrices  U_f  and  r_1  [6]. 
   *            On exit, if  INFO = -12,  DWORK(1)  returns the minimum 
   *            value of LDWORK. 
   * 
   *    LDWORK  INT 
   *            The length of the array DWORK. 
   *            LDWORK >= Max( (2*M-1)*NOBR, (M+L)*NOBR, 5*L*NOBR ), 
   *                                        if METH = 'M' and JOBD = 'M'; 
   *            LDWORK >=  5*L*NOBR,        if METH = 'M' and JOBD = 'N'; 
   *            LDWORK >=  5*(M+L)*NOBR+1,  if METH = 'N'. 
   *            For good performance,  LDWORK  should be larger. 
   * 
   *    Warning Indicator 
   * 
   *    IWARN   INT 
   *            = 0:  no warning; 
   *            = 4:  the least squares problems with coefficient matrix 
   *                  U_f,  used for computing the weighted oblique 
   *                  projection (for METH = 'N'), have a rank-deficient 
   *                  coefficient matrix; 
   *            = 5:  the least squares problem with coefficient matrix 
   *                  r_1  [6], used for computing the weighted oblique 
   *                  projection (for METH = 'N'), has a rank-deficient 
   *                  coefficient matrix. 
   * 
   *    Error Indicator 
   * 
   *    INFO    INT 
   *            = 0:  successful exit; 
   *            < 0:  if INFO = -i, the i-th argument had an illegal 
   *                  value; 
   *            = 2:  the singular value decomposition (SVD) algorithm did 
   *                  not converge. 
   * 
   *    METHOD 
   * 
   *    A singular value decomposition (SVD) of a certain matrix is 
   *    computed, which reveals the order  n  of the system as the number 
   *    of "non-zero" singular values. For the MOESP approach, this matrix 
   *    is  [ R_24'  R_34' ]' := R(ms+1:(2m+l)s,(2m+l)s+1:2(m+l)s), 
   *    where  R  is the upper triangular factor  R  constructed by SLICOT 
   *    Library routine  IB01MD.  For the N4SID approach, a weighted 
   *    oblique projection is computed from the upper triangular factor  R 
   *    and its SVD is then found. 
   * 
   *    REFERENCES 
   * 
   *    [1] Verhaegen M., and Dewilde, P. 
   *        Subspace Model Identification. Part 1: The output-error 
   *        state-space model identification class of algorithms. 
   *        Int. J. Control, 56, pp. 1187-1210, 1992. 
   * 
   *    [2] Verhaegen M. 
   *        Subspace Model Identification. Part 3: Analysis of the 
   *        ordinary output-error state-space model identification 
   *        algorithm. 
   *        Int. J. Control, 58, pp. 555-586, 1993. 
   * 
   *    [3] Verhaegen M. 
   *        Identification of the deterministic part of MIMO state space 
   *        models given in innovations form from input-output data. 
   *        Automatica, Vol.30, No.1, pp.61-74, 1994. 
   * 
   *    [4] Van Overschee, P., and De Moor, B. 
   *        N4SID: Subspace Algorithms for the Identification of 
   *        Combined Deterministic-Stochastic Systems. 
   *        Automatica, Vol.30, No.1, pp. 75-93, 1994. 
   * 
   *    [5] Van Overschee, P., and De Moor, B. 
   *        Subspace Identification for Linear Systems: Theory - 
   *        Implementation - Applications. 
   *        Kluwer Academic Publishers, Boston/London/Dordrecht, 1996. 
   * 
   *    [6] Sima, V. 
   *        Subspace-based Algorithms for Multivariable System 
   *        Identification. 
   *        Studies in Informatics and Control, 5, pp. 335-344, 1996. 
   * 
   *    NUMERICAL ASPECTS 
   * 
   *    The implemented method is numerically stable. 
   *                                     3 
   *    The algorithm requires 0(((m+l)s) ) floating point operations. 
   * 
   *    CONTRIBUTOR 
   * 
   *    V. Sima, Research Institute for Informatics, Bucharest, Aug. 1999. 
   * 
   *    REVISIONS 
   * 
   *    Feb. 2000, Feb. 2001, Feb. 2004, March 2005. 
   * 
   *    KEYWORDS 
   * 
   *    Identification methods, multivariable systems, QR decomposition, 
   *    singular value decomposition. 
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
   *    Decode the scalar input parameters. 
   * 
   */
  /* Parameter adjustments */
  r_dim1 = *ldr;
  r_offset = r_dim1 + 1;
  r__ -= r_offset;
  --sv;
  --iwork;
  --dwork;

  /* Function Body */
  moesp = C2F (lsame) (meth, "M", 1L, 1L);
  n4sid = C2F (lsame) (meth, "N", 1L, 1L);
  jobdm = C2F (lsame) (jobd, "M", 1L, 1L);
  mnobr = *m * *nobr;
  lnobr = *l * *nobr;
  llnobr = lnobr + lnobr;
  lmnobr = lnobr + mnobr;
  mmnobr = mnobr + mnobr;
  lmmnob = mmnobr + lnobr;
  nr = lmnobr + lmnobr;
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
  else if (moesp && !(jobdm || C2F (lsame) (jobd, "N", 1L, 1L)))
    {
      *info = -2;
    }
  else if (*nobr <= 0)
    {
      *info = -3;
    }
  else if (*m < 0)
    {
      *info = -4;
    }
  else if (*l <= 0)
    {
      *info = -5;
    }
  else if (*ldr < nr || moesp && jobdm && *ldr < mnobr * 3)
    {
      *info = -7;
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
       */
      minwrk = 1;
      if (*ldwork >= 1)
	{
	  if (moesp)
	    {
	      minwrk = lnobr * 5;
	      if (jobdm)
		{
		  /*Computing MAX 
		   */
		  i__1 = mmnobr - *nobr, i__1 = Max (i__1, lmnobr);
		  minwrk = Max (i__1, minwrk);
		}
	      maxwrk =
		lnobr + lnobr * C2F (ilaenv) (&c__1, "DGEQRF", " ", &lmnobr,
					      &lnobr, &c_n1, &c_n1, 6L, 1L);
	    }
	  else
	    {
	      /* 
	       *Computing MAX 
	       */
	      i__1 = minwrk, i__2 = lmnobr * 5 + 1;
	      minwrk = Max (i__1, i__2);
	      /*Computing MAX 
	       */
	      i__1 =
		mnobr + mnobr * C2F (ilaenv) (&c__1, "DGEQRF", " ", &mmnobr,
					      &mnobr, &c_n1, &c_n1, 6L, 1L),
		i__2 =
		mnobr + llnobr * C2F (ilaenv) (&c__1, "DORMQR", "LT", &mmnobr,
					       &llnobr, &mnobr, &c_n1, 6L,
					       2L);
	      maxwrk = Max (i__1, i__2);
	      /*Computing MAX 
	       */
	      i__1 = maxwrk, i__2 =
		mnobr + lnobr * C2F (ilaenv) (&c__1, "DORMQR", "LN", &mmnobr,
					      &lnobr, &mnobr, &c_n1, 6L, 2L);
	      maxwrk = Max (i__1, i__2);
	      /*Computing MAX 
	       */
	      i__1 = maxwrk, i__2 =
		lnobr + lnobr * C2F (ilaenv) (&c__1, "DGEQRF", " ", &lmmnob,
					      &lnobr, &c_n1, &c_n1, 6L, 1L);
	      maxwrk = Max (i__1, i__2);
	    }
	  maxwrk = Max (minwrk, maxwrk);
	}
      /* 
       */
      if (*ldwork < minwrk)
	{
	  *info = -12;
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
      C2F (xerbla) ("IB01ND", &i__1, 6L);
      return 0;
    }
  /* 
   *    Compute pointers to the needed blocks of  R. 
   * 
   */
  nr2 = mnobr + 1;
  nr3 = mmnobr + 1;
  nr4 = lmmnob + 1;
  itau = 1;
  jwork = itau + mnobr;
  /* 
   */
  if (moesp)
    {
      /* 
       *       MOESP approach. 
       * 
       */
      if (*m > 0 && jobdm)
	{
	  /* 
	   *          Rearrange the blocks of  R: 
	   *          Copy the (1,1) block into the position (3,2) and 
	   *          copy the (1,4) block into (3,3). 
	   * 
	   */
	  C2F (dlacpy) ("Upper", &mnobr, &mnobr, &r__[r_offset], ldr,
			&r__[nr3 + nr2 * r_dim1], ldr, 5L);
	  C2F (dlacpy) ("Full", &mnobr, &lnobr, &r__[nr4 * r_dim1 + 1], ldr,
			&r__[nr3 + nr3 * r_dim1], ldr, 4L);
	  /* 
	   *          Using structure, triangularize the matrix 
	   *             R_1c = [ R_12'  R_22'  R_11' ]' 
	   *          and then apply the transformations to the matrix 
	   *             R_2c = [ R_13'  R_23'  R_14' ]'. 
	   *          Workspace: need M*NOBR + MAX(M-1,L)*NOBR. 
	   * 
	   */
	  nsp_slicot_mb04od ("Upper", &mnobr, &lnobr, &mnobr,
			     &r__[nr2 + nr2 * r_dim1], ldr,
			     &r__[nr3 + nr2 * r_dim1], ldr,
			     &r__[nr2 + nr3 * r_dim1], ldr,
			     &r__[nr3 + nr3 * r_dim1], ldr, &dwork[itau],
			     &dwork[jwork], 5L);
	  i__1 = mnobr - 1;
	  i__2 = *ldwork - jwork + 1;
	  nsp_slicot_mb04id (&mmnobr, &mnobr, &i__1, &lnobr,
			     &r__[nr2 * r_dim1 + 1], ldr,
			     &r__[nr3 * r_dim1 + 1], ldr, &dwork[itau],
			     &dwork[jwork], &i__2, &ierr);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  /* 
	   *          Copy the leading  M*NOBR x M*NOBR  and  M*NOBR x L*NOBR 
	   *          submatrices of  R_1c  and  R_2c,  respectively, into their 
	   *          final positions, required by SLICOT Library routine  IB01PD. 
	   * 
	   */
	  C2F (dlacpy) ("Upper", &mnobr, &mnobr, &r__[nr2 * r_dim1 + 1], ldr,
			&r__[lmnobr + 1 + r_dim1], ldr, 5L);
	  C2F (dlacpy) ("Full", &mnobr, &lnobr, &r__[nr3 * r_dim1 + 1], ldr,
			&r__[nr2 * r_dim1 + 1], ldr, 4L);
	}
      /* 
       *       Copy [ R_24'  R_34' ]'  in  [ R_22'  R_32' ]'. 
       * 
       */
      C2F (dlacpy) ("Full", &lmnobr, &lnobr, &r__[nr2 + nr4 * r_dim1], ldr,
		    &r__[nr2 + nr2 * r_dim1], ldr, 4L);
      /* 
       *       Triangularize the matrix in  [ R_22'  R_32' ]'. 
       *       Workspace: need 2*L*NOBR; prefer L*NOBR + L*NOBR*NB. 
       * 
       */
      jwork = itau + lnobr;
      i__1 = *ldwork - jwork + 1;
      C2F (dgeqrf) (&lmnobr, &lnobr, &r__[nr2 + nr2 * r_dim1], ldr,
		    &dwork[itau], &dwork[jwork], &i__1, &ierr);
      /* 
       */
    }
  else
    {
      /* 
       *       N4SID approach. 
       * 
       */
      dum[0] = 0.;
      llmnob = llnobr + mnobr;
      /* 
       *       Set the precision parameters. A threshold value  EPS**(2/3)  is 
       *       used for deciding to use pivoting or not, where  EPS  is the 
       *       relative machine precision (see LAPACK Library routine DLAMCH). 
       * 
       */
      toll = *tol;
      eps = C2F (dlamch) ("Precision", 9L);
      thresh = pow_dd (&eps, &c_b37);
      /* 
       */
      if (*m > 0)
	{
	  /* 
	   *          For efficiency of later calculations, interchange the first 
	   *          two block-columns. The corresponding submatrices are 
	   *          redefined according to their new position. 
	   * 
	   */
	  i__1 = mnobr;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      C2F (dswap) (&i__, &r__[i__ * r_dim1 + 1], &c__1,
			   &r__[(mnobr + i__) * r_dim1 + 1], &c__1);
	      C2F (dcopy) (&mnobr, &r__[i__ + 1 + (mnobr + i__) * r_dim1],
			   &c__1, &r__[i__ + 1 + i__ * r_dim1], &c__1);
	      i__2 = mmnobr - i__;
	      C2F (dcopy) (&i__2, dum, &c__0,
			   &r__[i__ + 1 + (mnobr + i__) * r_dim1], &c__1);
	      /* L10: */
	    }
	  /* 
	   *          Now, 
	   * 
	   *          U_f = [ R_11'  R_21'    0      0   ]', 
	   *          U_p = [ R_12'    0      0      0   ]', 
	   *          Y_p = [ R_13'  R_23'  R_33'    0   ]',  and 
	   *          Y_f = [ R_14'  R_24'  R_34'  R_44' ]', 
	   * 
	   *          where  R_21,  R_12,  R_33,  and  R_44  are upper triangular. 
	   *          Define  W_p := [ U_p  Y_p ]. 
	   * 
	   *          Prepare the computation of residuals of the two least 
	   *          squares problems giving the weighted oblique projection P: 
	   * 
	   *          r_1 = W_p - U_f X_1,   X_1 = arg min || U_f X - W_p ||, 
	   *          r_2 = Y_f - U_f X_2,   X_2 = arg min || U_f X - Y_f ||, 
	   * 
	   *          P = (arg min || r_1 X - r_2 ||)' r_1'.                   (1) 
	   * 
	   *          Alternately,  P'  is given by the projection 
	   *             P' = Q_1 (Q_1)' r_2, 
	   *          where  Q_1  contains the first  k  columns of the orthogonal 
	   *          matrix in the  QR  factorization of  r_1,  k := rank(r_1). 
	   * 
	   *          Triangularize the matrix  U_f = q r  (using structure), and 
	   *          apply the transformation  q'  to the corresponding part of 
	   *          the matrices  W_p,  and  Y_f. 
	   *          Workspace: need 2*(M+L)*NOBR. 
	   * 
	   */
	  i__1 = mnobr - 1;
	  i__2 = *ldwork - jwork + 1;
	  nsp_slicot_mb04id (&mmnobr, &mnobr, &i__1, &llmnob, &r__[r_offset],
			     ldr, &r__[nr2 * r_dim1 + 1], ldr, &dwork[itau],
			     &dwork[jwork], &i__2, &ierr);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  /* 
	   *          Save updated  Y_f  (transposed) in the last block-row of  R. 
	   * 
	   */
	  nsp_slicot_ma02ad ("Full", &lmmnob, &lnobr, &r__[nr4 * r_dim1 + 1],
			     ldr, &r__[nr4 + r_dim1], ldr, 4L);
	  /* 
	   *          Check the condition of the triangular factor  r  and decide 
	   *          to use pivoting or not. 
	   *          Workspace: need 4*M*NOBR. 
	   * 
	   */
	  C2F (dtrcon) ("1-norm", "Upper", "NonUnit", &mnobr, &r__[r_offset],
			ldr, &rcond1, &dwork[jwork], &iwork[1], &ierr, 6L, 5L,
			7L);
	  /* 
	   */
	  if (toll <= 0.)
	    {
	      toll = mnobr * mnobr * eps;
	    }
	  if (rcond1 > Max (toll, thresh))
	    {
	      /* 
	       *             U_f is considered full rank and no pivoting is used. 
	       * 
	       */
	      C2F (dlaset) ("Full", &mnobr, &llmnob, &c_b50, &c_b50,
			    &r__[nr2 * r_dim1 + 1], ldr, 4L);
	    }
	  else
	    {
	      /* 
	       *             Save information about  q  in the (2,1) block of  R. 
	       *             Use QR factorization with column pivoting,  r P = Q R. 
	       *             Information on  Q  is stored in the strict lower triangle 
	       *             of R_11  and in  DWORK(ITAU2). 
	       * 
	       */
	      i__1 = mnobr - 1;
	      for (i__ = 1; i__ <= i__1; ++i__)
		{
		  i__2 = nr2;
		  for (j = mmnobr; j >= i__2; --j)
		    {
		      r__[j + i__ * r_dim1] =
			r__[j - mnobr + i__ + i__ * r_dim1];
		      /* L15: */
		    }
		  i__2 = mnobr - i__;
		  C2F (dcopy) (&i__2, dum, &c__0,
			       &r__[i__ + 1 + i__ * r_dim1], &c__1);
		  iwork[i__] = 0;
		  /* L20: */
		}
	      /* 
	       */
	      iwork[mnobr] = 0;
	      /* 
	       *             Workspace: need   5*M*NOBR+1. 
	       *                        prefer 4*M*NOBR + (M*NOBR+1)*NB. 
	       * 
	       */
	      itau2 = jwork;
	      jwork = itau2 + mnobr;
	      svlmax = 0.;
	      i__1 = *ldwork - jwork + 1;
	      nsp_slicot_mb03od ("QR", &mnobr, &mnobr, &r__[r_offset], ldr,
				 &iwork[1], &toll, &svlmax, &dwork[itau2],
				 &rank, sval, &dwork[jwork], &i__1, &ierr,
				 2L);
	      /*Computing MAX 
	       */
	      i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	      maxwrk = Max (i__1, i__2);
	      /* 
	       *             Workspace: need   2*M*NOBR + (M+2*L)*NOBR; 
	       *                        prefer 2*M*NOBR + (M+2*L)*NOBR*NB. 
	       * 
	       */
	      i__1 = *ldwork - jwork + 1;
	      C2F (dormqr) ("Left", "Transpose", &mnobr, &llmnob, &mnobr,
			    &r__[r_offset], ldr, &dwork[itau2],
			    &r__[nr2 * r_dim1 + 1], ldr, &dwork[jwork], &i__1,
			    &ierr, 4L, 9L);
	      /*Computing MAX 
	       */
	      i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	      maxwrk = Max (i__1, i__2);
	      if (rank < mnobr)
		{
		  /* 
		   *                The least squares problem is rank-deficient. 
		   * 
		   */
		  *iwarn = 4;
		}
	      /* 
	       *             Determine residuals r_1 and r_2: premultiply by  Q  and 
	       *             then by  q. 
	       *             Workspace: need   2*M*NOBR + (M+2*L)*NOBR); 
	       *                        prefer 2*M*NOBR + (M+2*L)*NOBR*NB. 
	       * 
	       */
	      C2F (dlaset) ("Full", &rank, &llmnob, &c_b50, &c_b50,
			    &r__[nr2 * r_dim1 + 1], ldr, 4L);
	      i__1 = *ldwork - jwork + 1;
	      C2F (dormqr) ("Left", "NoTranspose", &mnobr, &llmnob, &mnobr,
			    &r__[r_offset], ldr, &dwork[itau2],
			    &r__[nr2 * r_dim1 + 1], ldr, &dwork[jwork], &i__1,
			    &ierr, 4L, 11L);
	      /*Computing MAX 
	       */
	      i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	      maxwrk = Max (i__1, i__2);
	      jwork = itau2;
	      /* 
	       *             Restore the transformation  q. 
	       * 
	       */
	      i__1 = mnobr - 1;
	      for (i__ = 1; i__ <= i__1; ++i__)
		{
		  i__2 = mmnobr;
		  for (j = nr2; j <= i__2; ++j)
		    {
		      r__[j - mnobr + i__ + i__ * r_dim1] =
			r__[j + i__ * r_dim1];
		      /* L25: */
		    }
		  /* L30: */
		}
	      /* 
	       */
	    }
	  /* 
	   *          Premultiply by the transformation  q  (apply transformations 
	   *          in backward order). 
	   *          Workspace: need   M*NOBR + (M+2*L)*NOBR; 
	   *                     prefer larger. 
	   * 
	   */
	  i__1 = mnobr - 1;
	  i__2 = *ldwork - jwork + 1;
	  nsp_slicot_mb04iy ("Left", "NoTranspose", &mmnobr, &llmnob, &mnobr,
			     &i__1, &r__[r_offset], ldr, &dwork[itau],
			     &r__[nr2 * r_dim1 + 1], ldr, &dwork[jwork],
			     &i__2, &ierr, 4L, 11L);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  /* 
	   */
	}
      else
	{
	  /* 
	   *          Save  Y_f  (transposed) in the last block-row of  R. 
	   * 
	   */
	  nsp_slicot_ma02ad ("Full", &lmmnob, &lnobr, &r__[nr4 * r_dim1 + 1],
			     ldr, &r__[nr4 + r_dim1], ldr, 4L);
	  rcond1 = 1.;
	}
      /* 
       *       Triangularize the matrix  r_1  for determining the oblique 
       *       projection  P  in least squares problem in (1).  Exploit the 
       *       fact that the third block-row of r_1  has the structure 
       *       [ 0  T ],  where  T  is an upper triangular matrix.  Then apply 
       *       the corresponding transformations  Q'  to the matrix  r_2. 
       *       Workspace: need   2*M*NOBR; 
       *                  prefer   M*NOBR + M*NOBR*NB. 
       * 
       */
      i__1 = *ldwork - jwork + 1;
      C2F (dgeqrf) (&mmnobr, &mnobr, &r__[nr2 * r_dim1 + 1], ldr,
		    &dwork[itau], &dwork[jwork], &i__1, &ierr);
      /* 
       *       Workspace: need   M*NOBR + 2*L*NOBR; 
       *                  prefer M*NOBR + 2*L*NOBR*NB. 
       * 
       */
      i__1 = *ldwork - jwork + 1;
      C2F (dormqr) ("Left", "Transpose", &mmnobr, &llnobr, &mnobr,
		    &r__[nr2 * r_dim1 + 1], ldr, &dwork[itau],
		    &r__[nr3 * r_dim1 + 1], ldr, &dwork[jwork], &i__1, &ierr,
		    4L, 9L);
      nrsave = nr2;
      /* 
       */
      itau2 = jwork;
      jwork = itau2 + lnobr;
      i__1 = lnobr - 1;
      i__2 = *ldwork - jwork + 1;
      nsp_slicot_mb04id (&lmnobr, &lnobr, &i__1, &lnobr,
			 &r__[nr2 + nr3 * r_dim1], ldr,
			 &r__[nr2 + nr4 * r_dim1], ldr, &dwork[itau2],
			 &dwork[jwork], &i__2, &ierr);
      /*Computing MAX 
       */
      i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
      maxwrk = Max (i__1, i__2);
      /* 
       *       Check the condition of the triangular matrix of order  (m+l)*s 
       *       just determined, and decide to use pivoting or not. 
       *       Workspace: need 4*(M+L)*NOBR. 
       * 
       */
      C2F (dtrcon) ("1-norm", "Upper", "NonUnit", &lmnobr,
		    &r__[nr2 * r_dim1 + 1], ldr, &rcond2, &dwork[jwork],
		    &iwork[1], &ierr, 6L, 5L, 7L);
      /* 
       */
      if (*tol <= 0.)
	{
	  toll = lmnobr * lmnobr * eps;
	}
      if (rcond2 <= Max (toll, thresh))
	{
	  if (*m > 0)
	    {
	      /* 
	       *             Save information about  Q  in  R_11  (in the strict lower 
	       *             triangle),  R_21  and  R_31  (transposed information). 
	       * 
	       */
	      i__1 = mmnobr - 1;
	      C2F (dlacpy) ("Lower", &i__1, &mnobr, &r__[nr2 * r_dim1 + 2],
			    ldr, &r__[r_dim1 + 2], ldr, 5L);
	      nrsave = 1;
	      /* 
	       */
	      i__1 = lmnobr;
	      for (i__ = nr2; i__ <= i__1; ++i__)
		{
		  C2F (dcopy) (&mnobr, &r__[i__ + 1 + (mnobr + i__) * r_dim1],
			       &c__1, &r__[mnobr + i__ + r_dim1], ldr);
		  /* L40: */
		}
	      /* 
	       */
	    }
	  /* 
	   */
	  i__1 = lmnobr - 1;
	  i__2 = lmnobr - 1;
	  C2F (dlaset) ("Lower", &i__1, &i__2, &c_b50, &c_b50,
			&r__[nr2 * r_dim1 + 2], ldr, 5L);
	  /* 
	   *          Use QR factorization with column pivoting. 
	   *          Workspace: need   5*(M+L)*NOBR+1. 
	   *                     prefer 4*(M+L)*NOBR + ((M+L)*NOBR+1)*NB. 
	   * 
	   */
	  i__1 = lmnobr;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      iwork[i__] = 0;
	      /* L50: */
	    }
	  /* 
	   */
	  itau3 = jwork;
	  jwork = itau3 + lmnobr;
	  svlmax = 0.;
	  i__1 = *ldwork - jwork + 1;
	  nsp_slicot_mb03od ("QR", &lmnobr, &lmnobr, &r__[nr2 * r_dim1 + 1],
			     ldr, &iwork[1], &toll, &svlmax, &dwork[itau3],
			     &rank1, sval, &dwork[jwork], &i__1, &ierr, 2L);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  /* 
	   *          Workspace: need   2*(M+L)*NOBR + L*NOBR; 
	   *                     prefer 2*(M+L)*NOBR + L*NOBR*NB. 
	   * 
	   */
	  i__1 = *ldwork - jwork + 1;
	  C2F (dormqr) ("Left", "Transpose", &lmnobr, &lnobr, &lmnobr,
			&r__[nr2 * r_dim1 + 1], ldr, &dwork[itau3],
			&r__[nr4 * r_dim1 + 1], ldr, &dwork[jwork], &i__1,
			&ierr, 4L, 9L);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  if (rank1 < lmnobr)
	    {
	      /* 
	       *             The least squares problem is rank-deficient. 
	       * 
	       */
	      *iwarn = 5;
	    }
	  /* 
	   *          Apply the orthogonal transformations, in backward order, to 
	   *          [r_2(1:rank(r_1),:)' 0]',  to obtain  P'. 
	   *          Workspace: need   2*(M+L)*NOBR + L*NOBR; 
	   *                     prefer 2*(M+L)*NOBR + L*NOBR*NB. 
	   * 
	   */
	  i__1 = lmnobr - rank1;
	  C2F (dlaset) ("Full", &i__1, &lnobr, &c_b50, &c_b50,
			&r__[rank1 + 1 + nr4 * r_dim1], ldr, 4L);
	  i__1 = *ldwork - jwork + 1;
	  C2F (dormqr) ("Left", "NoTranspose", &lmnobr, &lnobr, &lmnobr,
			&r__[nr2 * r_dim1 + 1], ldr, &dwork[itau3],
			&r__[nr4 * r_dim1 + 1], ldr, &dwork[jwork], &i__1,
			&ierr, 4L, 11L);
	  /*Computing MAX 
	   */
	  i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
	  maxwrk = Max (i__1, i__2);
	  jwork = itau3;
	  /* 
	   */
	  if (*m > 0)
	    {
	      /* 
	       *             Restore the saved transpose matrix from  R_31. 
	       * 
	       */
	      i__1 = lmnobr;
	      for (i__ = nr2; i__ <= i__1; ++i__)
		{
		  C2F (dcopy) (&mnobr, &r__[mnobr + i__ + r_dim1], ldr,
			       &r__[i__ + 1 + (mnobr + i__) * r_dim1], &c__1);
		  /* L60: */
		}
	      /* 
	       */
	    }
	  /* 
	   */
	}
      /* 
       *       Workspace: need   M*NOBR + L*NOBR; 
       *                  prefer larger. 
       * 
       */
      i__1 = lnobr - 1;
      i__2 = *ldwork - jwork + 1;
      nsp_slicot_mb04iy ("Left", "NoTranspose", &lmnobr, &lnobr, &lnobr,
			 &i__1, &r__[nr2 + nr3 * r_dim1], ldr, &dwork[itau2],
			 &r__[nr2 + nr4 * r_dim1], ldr, &dwork[jwork], &i__2,
			 &ierr, 4L, 11L);
      /*Computing MAX 
       */
      i__1 = maxwrk, i__2 = (int) dwork[jwork] + jwork - 1;
      maxwrk = Max (i__1, i__2);
      /* 
       *       Workspace: need   M*NOBR + L*NOBR; 
       *                  prefer M*NOBR + L*NOBR*NB. 
       * 
       */
      jwork = itau2;
      i__1 = *ldwork - jwork + 1;
      C2F (dormqr) ("Left", "NoTranspose", &mmnobr, &lnobr, &mnobr,
		    &r__[nrsave * r_dim1 + 1], ldr, &dwork[itau],
		    &r__[nr4 * r_dim1 + 1], ldr, &dwork[jwork], &i__1, &ierr,
		    4L, 11L);
      /* 
       *       Now, the matrix  P'  is available in  R_14 : R_34. 
       *       Triangularize the matrix  P'. 
       *       Workspace: need   2*L*NOBR; 
       *                  prefer   L*NOBR + L*NOBR*NB. 
       * 
       */
      jwork = itau + lnobr;
      i__1 = *ldwork - jwork + 1;
      C2F (dgeqrf) (&lmmnob, &lnobr, &r__[nr4 * r_dim1 + 1], ldr,
		    &dwork[itau], &dwork[jwork], &i__1, &ierr);
      /* 
       *       Copy the triangular factor to its final position,  R_22. 
       * 
       */
      C2F (dlacpy) ("Upper", &lnobr, &lnobr, &r__[nr4 * r_dim1 + 1], ldr,
		    &r__[nr2 + nr2 * r_dim1], ldr, 5L);
      /* 
       *       Restore  Y_f. 
       * 
       */
      nsp_slicot_ma02ad ("Full", &lnobr, &lmmnob, &r__[nr4 + r_dim1], ldr,
			 &r__[nr4 * r_dim1 + 1], ldr, 4L);
    }
  /* 
   *    Find the singular value decomposition of  R_22. 
   *    Workspace: need 5*L*NOBR. 
   * 
   */
  nsp_slicot_mb03ud ("NoVectors", "Vectors", &lnobr, &r__[nr2 + nr2 * r_dim1],
		     ldr, dum, &c__1, &sv[1], &dwork[1], ldwork, &ierr, 9L,
		     7L);
  if (ierr != 0)
    {
      *info = 2;
      return 0;
    }
  /*Computing MAX 
   */
  i__1 = maxwrk, i__2 = (int) dwork[1];
  maxwrk = Max (i__1, i__2);
  /* 
   *    Transpose  R(m*s+1:(m+L)*s,m*s+1:(m+L)*s)  in-situ; its 
   *    columns will then be the singular vectors needed subsequently. 
   * 
   */
  i__1 = lmnobr;
  for (i__ = nr2 + 1; i__ <= i__1; ++i__)
    {
      i__2 = lmnobr - i__ + 1;
      C2F (dswap) (&i__2, &r__[i__ + (i__ - 1) * r_dim1], &c__1,
		   &r__[i__ - 1 + i__ * r_dim1], ldr);
      /* L70: */
    }
  /* 
   *    Return optimal workspace in  DWORK(1)  and reciprocal condition 
   *    numbers, if  METH = 'N'. 
   * 
   */
  dwork[1] = (double) maxwrk;
  if (n4sid)
    {
      dwork[2] = rcond1;
      dwork[3] = rcond2;
    }
  return 0;
  /* 
**** Last line of IB01ND *** 
*/
}				/* nsp_slicot_ib01nd */
