/* dxlegf.f -- translated by f2c (version 19961017).
 *
 *
 */

#if 0
#include "calpack.h"

/* Common Block Declarations */

struct
{
  int nbitsf;
} dxblk1_;

#define dxblk1_1 dxblk1_

struct
{
  double radix, radixl, rad2l, dlg10r;
  int l, l2, kmax;
} dxblk2_;

#define dxblk2_1 dxblk2_

struct
{
  int nlg102, mlg102, lg102[21];
} dxblk3_;

#define dxblk3_1 dxblk3_

/* Table of constant values */

static int c__0 = 0;
static double c_b4 = 0.;
static double c_b9 = 1.;
static int c_n1 = -1;
static int c__2 = 2;

/* 
 * original code from the Slatec library 
 * 
 * slight modifications by Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> : 
 * 
 *    1/ some (minor modifications) so that the "enter" is 
 *       now X and not THETA (X=cos(THETA)). This leads to 
 *       better accuracy for x near 0 and seems more 
 *       natural but may be there is some drawback ? 
 *    2/ remove parts which send warning messages to the 
 *       Slatec XERMSG routine (nevertheless all the errors 
 *       flags are communicated throw IERROR). 
 *       Normaly the scilab interface verify the validity of the 
 *       input arguments but the verifications in this code are 
 *       still here. 
 *    3/ substitute calls to I1MACH by calls to dlamch 
 *       (Scilab uses dlamch and not I1MACH to get machine 
 *       parameter so it seems more int). 
 * 
 *   IERROR values : 
 *         210 :  DNU1, NUDIFF, MU1, MU2, or ID not valid 
 *         211 :  X out of range (must be in [0,1) 
 *         201, 202, 203, 204 : invalid input was provided to DXSET 
 *                      (should not occured in IEEE floating point) 
 *         205, 206 : internal consistency error occurred in DXSET 
 *                    (probably due to a software malfunction in the 
 *                     library routine I1MACH) Should not occured 
 *                     in IEEE floating point, if dlamch works well. 
 *         207 : an overflow or underflow of an extended-range number 
 *               was detected in DXADJ. 
 *         208 : an error which may occur in DXC210 but this one is not 
 *               call from DXLEGF (don't know why it is given below). 
 * 
 *    Normally on the return to scilab, only 207 may be present. 
 */

/* Compute normalized Legendre polynomials and associated Legendre functions. 
 * Slatec Library 
 * Author: Smith, John M., (NBS and George Mason University) 
 * 
 *  DXLEGF: Extended-range Double-precision Legendre Functions 
 * 
 *  A feature of the DXLEGF subroutine for Legendre functions is 
 *the use of extended-range arithmetic, a software extension of 
 *ordinary floating-point arithmetic that greatly increases the 
 *exponent range of the representable numbers. This avoids the 
 *need for scaling the solutions to lie within the exponent range 
 *of the most restrictive manufacturer's hardware. The increased 
 *exponent range is achieved by allocating an int storage 
 *location together with each floating-point storage location. 
 * 
 *  The interpretation of the pair (X,I) where X is floating-point 
 *and I is int is X*(IR**I) where IR is the internal radix of 
 *the computer arithmetic. 
 * 
 *  This subroutine computes one of the following vectors: 
 * 
 *1. Legendre function of the first kind of negative order, either 
 *   a. P(-MU1,NU,X), P(-MU1-1,NU,X), ..., P(-MU2,NU,X) or 
 *   b. P(-MU,NU1,X), P(-MU,NU1+1,X), ..., P(-MU,NU2,X) 
 *2. Legendre function of the second kind, either 
 *   a. Q(MU1,NU,X), Q(MU1+1,NU,X), ..., Q(MU2,NU,X) or 
 *   b. Q(MU,NU1,X), Q(MU,NU1+1,X), ..., Q(MU,NU2,X) 
 *3. Legendre function of the first kind of positive order, either 
 *   a. P(MU1,NU,X), P(MU1+1,NU,X), ..., P(MU2,NU,X) or 
 *   b. P(MU,NU1,X), P(MU,NU1+1,X), ..., P(MU,NU2,X) 
 *4. Normalized Legendre polynomials, either 
 *   a. PN(MU1,NU,X), PN(MU1+1,NU,X), ..., PN(MU2,NU,X) or 
 *   b. PN(MU,NU1,X), PN(MU,NU1+1,X), ..., PN(MU,NU2,X) 
 * 
 *where X = COS(THETA). 
 * 
 *  The input values to DXLEGF are DNU1, NUDIFF, MU1, MU2, THETA (now X), 
 *and ID. These must satisfy 
 * 
 *   DNU1 is DOUBLE PRECISION and greater than or equal to -0.5; 
 *   NUDIFF is INT and non-negative; 
 *   MU1 is INT and non-negative; 
 *   MU2 is INT and greater than or equal to MU1; 
 *   THETA is DOUBLE PRECISION and in the half-open interval (0,PI/2]; 
 *   modification :  X is given (and not THETA) X must be in [0,1) 
 *   ID is INT and equal to 1, 2, 3 or 4; 
 * 
 *and  additionally either NUDIFF = 0 or MU2 = MU1. 
 * 
 *  If ID=1 and NUDIFF=0, a vector of type 1a above is computed 
 *with NU=DNU1. 
 * 
 *  If ID=1 and MU1=MU2, a vector of type 1b above is computed 
 *with NU1=DNU1, NU2=DNU1+NUDIFF and MU=MU1. 
 * 
 *  If ID=2 and NUDIFF=0, a vector of type 2a above is computed 
 *with NU=DNU1. 
 * 
 *  If ID=2 and MU1=MU2, a vector of type 2b above is computed 
 *with NU1=DNU1, NU2=DNU1+NUDIFF and MU=MU1. 
 * 
 *  If ID=3 and NUDIFF=0, a vector of type 3a above is computed 
 *with NU=DNU1. 
 * 
 *  If ID=3 and MU1=MU2, a vector of type 3b above is computed 
 *with NU1=DNU1, NU2=DNU1+NUDIFF and MU=MU1. 
 * 
 *  If ID=4 and NUDIFF=0, a vector of type 4a above is computed 
 *with NU=DNU1. 
 * 
 *  If ID=4 and MU1=MU2, a vector of type 4b above is computed 
 *with NU1=DNU1, NU2=DNU1+NUDIFF and MU=MU1. 
 * 
 *  In each case the vector of computed Legendre function values 
 *is returned in the extended-range vector (PQA(I),IPQA(I)). The 
 *length of this vector is either MU2-MU1+1 or NUDIFF+1. 
 * 
 *  Where possible, DXLEGF returns IPQA(I) as zero. In this case the 
 *value of the Legendre function is contained entirely in PQA(I), 
 *so it can be used in subsequent computations without further 
 *consideration of extended-range arithmetic. If IPQA(I) is nonzero, 
 *then the value of the Legendre function is not representable in 
 *floating-point because of underflow or overflow. The program that 
 *calls DXLEGF must test IPQA(I) to ensure correct usage. 
 * 
 *  IERROR is an error indicator. If no errors are detected, IERROR=0 
 *when control returns to the calling routine. If an error is detected, 
 *IERROR is returned as nonzero. The calling routine must check the 
 *value of IERROR. 
 * 
 *  If IERROR=210 or 211, invalid input was provided to DXLEGF. 
 *  If IERROR=201,202,203, or 204, invalid input was provided to DXSET. 
 *  If IERROR=205 or 206, an internal consistency error occurred in 
 *DXSET (probably due to a software malfunction in the library routine 
 *I1MACH). 
 *  If IERROR=207, an overflow or underflow of an extended-range number 
 *was detected in DXADJ. 
 *  If IERROR=208, an overflow or underflow of an extended-range number 
 *was detected in DXC210. 
 * 
 * References:  Olver and Smith, Associated Legendre Functions on the 
 *                Cut, J Comp Phys, v 51, n 3, Sept 1983, pp 502--518. 
 *              Smith, Olver and Lozier, Extended-Range Arithmetic and 
 *                Normalized Legendre Polynomials, ACM Trans on Math 
 *                Softw, v 7, n 1, March 1981, pp 93--105. 
 *  820728  DATE WRITTEN 
 *  890126  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
 *  901019  Revisions to prologue.  (DWL and WRB) 
 *  901106  Changed all specific intrinsics to generic.  (WRB) 
 *          Corrected order of sections in prologue and added TYPE 
 *          section.  (WRB) 
 *          CALLs to XERROR changed to CALLs to XERMSG.  (WRB) 
 *  920127  Revised PURPOSE section of prologue.  (DWL) 
 */

int
nsp_calpack_dxlegf (double *dnu1, int *nudiff, int *mu1, int *mu2, double *x,
		    int *id, double *pqa, int *ipqa, int *ierror)
{
  int i__1;
  int i__, l;
  double sx;
  double pi2;
  double dnu2;

  --ipqa;
  --pqa;

  /* Function Body */
  *ierror = 0;
  nsp_calpack_dxset (&c__0, &c__0, &c_b4, &c__0, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  pi2 = atan (1.) * 2.;
  /* 
   *       ZERO OUTPUT ARRAYS 
   * 
   */
  l = *mu2 - *mu1 + *nudiff + 1;
  i__1 = l;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      pqa[i__] = 0.;
      /* L290: */
      ipqa[i__] = 0;
    }
  /* 
   *       CHECK FOR VALID INPUT VALUES 
   * 
   *** normally all these are verified by the scilab interface 
   */
  if (*nudiff < 0)
    {
      goto L400;
    }
  if (*dnu1 < -.5)
    {
      goto L400;
    }
  if (*mu2 < *mu1)
    {
      goto L400;
    }
  if (*mu1 < 0)
    {
      goto L400;
    }
  /*     IF(THETA.LE.0.D0.OR.THETA.GT.PI2) GO TO 420 
   */
  if (*x < 0. || *x >= 1.)
    {
      goto L420;
    }
  if (*id < 1 || *id > 4)
    {
      goto L400;
    }
  if (*mu1 != *mu2 && *nudiff > 0)
    {
      goto L400;
    }
  /* 
   *       IF DNU1 IS NOT AN INT, NORMALIZED P(MU,DNU,X) 
   *       CANNOT BE CALCULATED.  IF DNU1 IS AN INT AND 
   *       MU1.GT.DNU2 THEN ALL VALUES OF P(+MU,DNU,X) AND 
   *       NORMALIZED P(MU,NU,X) WILL BE ZERO. 
   * 
   */
  dnu2 = *dnu1 + *nudiff;
  if (*id == 3 && d_mod (dnu1, &c_b9) != 0.)
    {
      goto L295;
    }
  if (*id == 4 && d_mod (dnu1, &c_b9) != 0.)
    {
      goto L400;
    }
  if ((*id == 3 || *id == 4) && (double) (*mu1) > dnu2)
    {
      return 0;
    }
 L295:
  /* 
   *     X=COS(THETA) 
   *     SX=1.D0/SIN(THETA) 
   */
  sx = 1. / sqrt ((1. - *x) * (*x + 1.));
  if (*id == 2)
    {
      goto L300;
    }
  if (*mu2 - *mu1 <= 0)
    {
      goto L360;
    }
  /* 
   *       FIXED NU, VARIABLE MU 
   *       CALL DXPMU TO CALCULATE P(-MU1,NU,X),....,P(-MU2,NU,X) 
   * 
   */
  nsp_calpack_dxpmu (dnu1, &dnu2, mu1, mu2, x, &sx, id, &pqa[1], &ipqa[1],
		     ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  goto L380;
  /* 
   */
 L300:
  if (*mu2 == *mu1)
    {
      goto L320;
    }
  /* 
   *       FIXED NU, VARIABLE MU 
   *       CALL DXQMU TO CALCULATE Q(MU1,NU,X),....,Q(MU2,NU,X) 
   * 
   */
  nsp_calpack_dxqmu (dnu1, &dnu2, mu1, mu2, x, &sx, id, &pqa[1], &ipqa[1],
		     ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  goto L390;
  /* 
   *       FIXED MU, VARIABLE NU 
   *       CALL DXQNU TO CALCULATE Q(MU,DNU1,X),....,Q(MU,DNU2,X) 
   * 
   */
 L320:
  nsp_calpack_dxqnu (dnu1, &dnu2, mu1, x, &sx, id, &pqa[1], &ipqa[1], ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  goto L390;
  /* 
   *       FIXED MU, VARIABLE NU 
   *       CALL DXPQNU TO CALCULATE P(-MU,DNU1,X),....,P(-MU,DNU2,X) 
   * 
   */
 L360:
  nsp_calpack_dxpqnu (dnu1, &dnu2, mu1, x, &sx, id, &pqa[1], &ipqa[1],
		      ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  /* 
   *       IF ID = 3, TRANSFORM P(-MU,NU,X) VECTOR INTO 
   *       P(MU,NU,X) VECTOR. 
   * 
   */
 L380:
  if (*id == 3)
    {
      nsp_calpack_dxpmup (dnu1, &dnu2, mu1, mu2, &pqa[1], &ipqa[1], ierror);
    }
  if (*ierror != 0)
    {
      return 0;
    }
  /* 
   *       IF ID = 4, TRANSFORM P(-MU,NU,X) VECTOR INTO 
   *       NORMALIZED P(MU,NU,X) VECTOR. 
   * 
   */
  if (*id == 4)
    {
      nsp_calpack_dxpnrm (dnu1, &dnu2, mu1, mu2, &pqa[1], &ipqa[1], ierror);
    }
  if (*ierror != 0)
    {
      return 0;
    }
  /* 
   *       PLACE RESULTS IN REDUCED FORM IF POSSIBLE 
   *       AND RETURN TO MAIN PROGRAM. 
   * 
   */
 L390:
  i__1 = l;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nsp_calpack_dxred (&pqa[i__], &ipqa[i__], ierror);
      if (*ierror != 0)
	{
	  return 0;
	}
      /* L395: */
    }
  return 0;
  /* 
   *       *****     ERROR TERMINATION     ***** 
   * 
   * 400 CALL XERMSG ('SLATEC', 'DXLEGF', 
   *    +             'DNU1, NUDIFF, MU1, MU2, or ID not valid', 210, 1) 
   */
 L400:
  *ierror = 210;
  return 0;
  /* 420 CALL XERMSG ('SLATEC', 'DXLEGF', 'THETA out of range', 211, 1) 
   */
 L420:
  *ierror = 211;
  return 0;
}

/*
 * To compute the values of Legendre functions for DXLEGF. 
 *           Method: backward mu-wise recurrence for P(-MU,NU,X) for 
 *           fixed nu to obtain P(-MU2,NU1,X), P(-(MU2-1),NU1,X), ..., 
 *           P(-MU1,NU1,X) and store in ascending mu order. 
 *SLATEC 
 *
 * Author:  Smith, John M., (NBS and George Mason University) 
 *
 *  820728  DATE WRITTEN 
 *  890126  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
 *  901019  Revisions to prologue.  (DWL and WRB) 
 *  901106  Changed all specific intrinsics to generic.  (WRB) 
 *          Corrected order of sections in prologue and added TYPE 
 *          section.  (WRB) 
 *  920127  Revised PURPOSE section of prologue.  (DWL) 
 * 
 *       CALL DXPQNU TO OBTAIN P(-MU2,NU,X) 
 */

int
nsp_calpack_dxpmu (double *nu1, double *nu2, int *mu1, int *mu2, double *x,
		   double *sx, int *id, double *pqa, int *ipqa, int *ierror)
{
  int j, n;
  double p0, x1, x2;
  int mu, ip0;

  /* Parameter adjustments */
  --ipqa;
  --pqa;

  /* Function Body */
  *ierror = 0;
  nsp_calpack_dxpqnu (nu1, nu2, mu2, x, sx, id, &pqa[1], &ipqa[1], ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  p0 = pqa[1];
  ip0 = ipqa[1];
  mu = *mu2 - 1;
  /* 
   *       CALL DXPQNU TO OBTAIN P(-MU2-1,NU,X) 
   * 
   */
  nsp_calpack_dxpqnu (nu1, nu2, &mu, x, sx, id, &pqa[1], &ipqa[1], ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  n = *mu2 - *mu1 + 1;
  pqa[n] = p0;
  ipqa[n] = ip0;
  if (n == 1)
    {
      goto L300;
    }
  pqa[n - 1] = pqa[1];
  ipqa[n - 1] = ipqa[1];
  if (n == 2)
    {
      goto L300;
    }
  j = n - 2;
 L290:
  /* 
   *       BACKWARD RECURRENCE IN MU TO OBTAIN 
   *             P(-MU2,NU1,X),P(-(MU2-1),NU1,X),....P(-MU1,NU1,X) 
   *             USING 
   *             (NU-MU)*(NU+MU+1.)*P(-(MU+1),NU,X)= 
   *               2.*MU*X*SQRT((1./(1.-X**2))*P(-MU,NU,X)-P(-(MU-1),NU,X) 
   * 
   */
  x1 = mu * 2. * *x * *sx * pqa[j + 1];
  x2 = -(*nu1 - mu) * (*nu1 + mu + 1.) * pqa[j + 2];
  nsp_calpack_dxadd (&x1, &ipqa[j + 1], &x2, &ipqa[j + 2], &pqa[j], &ipqa[j],
		     ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  nsp_calpack_dxadj (&pqa[j], &ipqa[j], ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  if (j == 1)
    {
      goto L300;
    }
  --j;
  --mu;
  goto L290;
 L300:
  return 0;
}				/* dxpmu_ */


/* DXPMUP 
****SUBSIDIARY 
* To compute the values of Legendre functions for DXLEGF. 
*           This subroutine transforms an array of Legendre functions 
*           of the first kind of negative order stored in array PQA 
*           into Legendre functions of the first kind of positive 
*           order stored in array PQA. The original array is destroyed. 
*Slatec Library 
*
* Author:  Smith, John M., (NBS and George Mason University) 
****ROUTINES CALLED  DXADJ 
****REVISION HISTORY  (YYMMDD) 
*  820728  DATE WRITTEN 
*  890126  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
*  901019  Revisions to prologue.  (DWL and WRB) 
*  901106  Changed all specific intrinsics to generic.  (WRB) 
*          Corrected order of sections in prologue and added TYPE 
*          section.  (WRB) 
*  920127  Revised PURPOSE section of prologue.  (DWL) 
*/

int
nsp_calpack_dxpmup (double *nu1, double *nu2, int *mu1, int *mu2,
		    double *pqa, int *ipqa, int *ierror)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double prod;
  int i__, j, k, l, n;
  int iprod, mu;
  double nu, dmu;

  /* Parameter adjustments */
  --ipqa;
  --pqa;

  /* Function Body */
  *ierror = 0;
  nu = *nu1;
  mu = *mu1;
  dmu = (double) mu;
  n = (int) (*nu2 - *nu1 + .1) + (*mu2 - *mu1) + 1;
  j = 1;
  /*     IF(MOD(REAL(NU),1.).NE.0.) GO TO 210 
   */
  if (d_mod (&nu, &c_b9) != 0.)
    {
      goto L210;
    }
 L200:
  if (dmu < nu + 1.)
    {
      goto L210;
    }
  pqa[j] = 0.;
  ipqa[j] = 0;
  ++j;
  if (j > n)
    {
      return 0;
    }
  /*       INCREMENT EITHER MU OR NU AS APPROPRIATE. 
   */
  if (*nu2 - *nu1 > .5)
    {
      nu += 1.;
    }
  if (*mu2 > *mu1)
    {
      ++mu;
    }
  goto L200;
  /* 
   *       TRANSFORM P(-MU,NU,X) TO P(MU,NU,X) USING 
   *       P(MU,NU,X)=(NU-MU+1)*(NU-MU+2)*...*(NU+MU)*P(-MU,NU,X)*(-1)**MU 
   * 
   */
 L210:
  prod = 1.;
  iprod = 0;
  k = mu << 1;
  if (k == 0)
    {
      goto L222;
    }
  i__1 = k;
  for (l = 1; l <= i__1; ++l)
    {
      prod *= dmu - nu - l;
      /* L220: */
      nsp_calpack_dxadj (&prod, &iprod, ierror);
    }
  if (*ierror != 0)
    {
      return 0;
    }
 L222:
  i__1 = n;
  for (i__ = j; i__ <= i__1; ++i__)
    {
      if (mu == 0)
	{
	  goto L225;
	}
      pqa[i__] = pqa[i__] * prod * pow_ii (&c_n1, &mu);
      ipqa[i__] += iprod;
      nsp_calpack_dxadj (&pqa[i__], &ipqa[i__], ierror);
      if (*ierror != 0)
	{
	  return 0;
	}
    L225:
      if (*nu2 - *nu1 > .5)
	{
	  goto L230;
	}
      prod = (dmu - nu) * prod * (-dmu - nu - 1.);
      nsp_calpack_dxadj (&prod, &iprod, ierror);
      if (*ierror != 0)
	{
	  return 0;
	}
      ++mu;
      dmu += 1.;
      goto L240;
    L230:
      prod = prod * (-dmu - nu - 1.) / (dmu - nu - 1.);
      nsp_calpack_dxadj (&prod, &iprod, ierror);
      if (*ierror != 0)
	{
	  return 0;
	}
      nu += 1.;
    L240:
      ;
    }
  return 0;
}				/* dxpmup_ */

/* DXPNRM 
****SUBSIDIARY 
* To compute the values of Legendre functions for DXLEGF. 
*           This subroutine transforms an array of Legendre functions 
*           of the first kind of negative order stored in array PQA 
*           into normalized Legendre polynomials stored in array PQA. 
*           The original array is destroyed. 
*SLATEC 
*
* Author:  Smith, John M., (NBS and George Mason University) 
****ROUTINES CALLED  DXADJ 
****REVISION HISTORY  (YYMMDD) 
*  820728  DATE WRITTEN 
*  890126  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
*  901019  Revisions to prologue.  (DWL and WRB) 
*  901106  Changed all specific intrinsics to generic.  (WRB) 
*          Corrected order of sections in prologue and added TYPE 
*          section.  (WRB) 
*  920127  Revised PURPOSE section of prologue.  (DWL) 
*/

int
nsp_calpack_dxpnrm (double *nu1, double *nu2, int *mu1, int *mu2,
		    double *pqa, int *ipqa, int *ierror)
{
  int i__1;

  double prod;
  int i__, j, k, l;
  int iprod;
  double c1;
  int mu;
  double nu, dmu;

  /* Parameter adjustments */
  --ipqa;
  --pqa;

  /* Function Body */
  *ierror = 0;
  l = (int) (*mu2 - *mu1 + (*nu2 - *nu1 + 1.5));
  mu = *mu1;
  dmu = (double) (*mu1);
  nu = *nu1;
  /* 
   *        IF MU .GT.NU, NORM P =0. 
   * 
   */
  j = 1;
 L500:
  if (dmu <= nu)
    {
      goto L505;
    }
  pqa[j] = 0.;
  ipqa[j] = 0;
  ++j;
  if (j > l)
    {
      return 0;
    }
  /* 
   *       INCREMENT EITHER MU OR NU AS APPROPRIATE. 
   * 
   */
  if (*mu2 > *mu1)
    {
      dmu += 1.;
    }
  if (*nu2 - *nu1 > .5)
    {
      nu += 1.;
    }
  goto L500;
  /* 
   *        TRANSFORM P(-MU,NU,X) INTO NORMALIZED P(MU,NU,X) USING 
   *             NORM P(MU,NU,X)= 
   *                SQRT((NU+.5)*FACTORIAL(NU+MU)/FACTORIAL(NU-MU)) 
   *                             *P(-MU,NU,X) 
   * 
   */
 L505:
  prod = 1.;
  iprod = 0;
  k = mu << 1;
  if (k <= 0)
    {
      goto L520;
    }
  i__1 = k;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      prod *= sqrt (nu + dmu + 1. - i__);
      /* L510: */
      nsp_calpack_dxadj (&prod, &iprod, ierror);
    }
  if (*ierror != 0)
    {
      return 0;
    }
 L520:
  i__1 = l;
  for (i__ = j; i__ <= i__1; ++i__)
    {
      c1 = prod * sqrt (nu + .5);
      pqa[i__] *= c1;
      ipqa[i__] += iprod;
      nsp_calpack_dxadj (&pqa[i__], &ipqa[i__], ierror);
      if (*ierror != 0)
	{
	  return 0;
	}
      if (*nu2 - *nu1 > .5)
	{
	  goto L530;
	}
      if (dmu >= nu)
	{
	  goto L525;
	}
      prod = sqrt (nu + dmu + 1.) * prod;
      if (nu > dmu)
	{
	  prod *= sqrt (nu - dmu);
	}
      nsp_calpack_dxadj (&prod, &iprod, ierror);
      if (*ierror != 0)
	{
	  return 0;
	}
      ++mu;
      dmu += 1.;
      goto L540;
    L525:
      prod = 0.;
      iprod = 0;
      ++mu;
      dmu += 1.;
      goto L540;
    L530:
      prod = sqrt (nu + dmu + 1.) * prod;
      if (nu != dmu - 1.)
	{
	  prod /= sqrt (nu - dmu + 1.);
	}
      nsp_calpack_dxadj (&prod, &iprod, ierror);
      if (*ierror != 0)
	{
	  return 0;
	}
      nu += 1.;
    L540:
      ;
    }
  return 0;
}				/* dxpnrm_ */

/* DXPQNU 
****SUBSIDIARY 
* To compute the values of Legendre functions for DXLEGF. 
*           This subroutine calculates initial values of P or Q using 
*           power series, then performs forward nu-wise recurrence to 
*           obtain P(-MU,NU,X), Q(0,NU,X), or Q(1,NU,X). The nu-wise 
*           recurrence is stable for P for all mu and for Q for mu=0,1. 
*SLATEC 
*
* Author:  Smith, John M., (NBS and George Mason University) 
****ROUTINES CALLED  DXADD, DXADJ, DXPSI 
****COMMON BLOCKS    DXBLK1 
****REVISION HISTORY  (YYMMDD) 
*  820728  DATE WRITTEN 
*  890126  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
*  901019  Revisions to prologue.  (DWL and WRB) 
*  901106  Changed all specific intrinsics to generic.  (WRB) 
*          Corrected order of sections in prologue and added TYPE 
*          section.  (WRB) 
*  920127  Revised PURPOSE section of prologue.  (DWL) 
****END PROLOGUE  DXPQNU 
* 
*       J0, IPSIK, AND IPSIX ARE INITIALIZED IN THIS SUBROUTINE. 
*       J0 IS THE NUMBER OF TERMS USED IN SERIES EXPANSION 
*       IN SUBROUTINE DXPQNU. 
*       IPSIK, IPSIX ARE VALUES OF K AND X RESPECTIVELY 
*       USED IN THE CALCULATION OF THE DXPSI FUNCTION. 
* 
*/


int
nsp_calpack_dxpqnu (double *nu1, double *nu2, int *mu, double *x, double *sx,
		    int *id, double *pqa, int *ipqa, int *ierror)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  double flok, a;
  int i__, j, k;
  double r__;
  double w, y, z__;
  int ipsik;
  int j0, ipsix;
  double x1, x2;
  int ia;
  double di;
  int if__;
  double pq, nu, xs, factmu, pq1, pq2;
  int ix1;
  double dmu;
  int ipq, ixs, ipq1, ipq2;

  /* Parameter adjustments */
  --ipqa;
  --pqa;

  /* Function Body */
  *ierror = 0;
  j0 = dxblk1_1.nbitsf;
  ipsik = dxblk1_1.nbitsf / 10 + 1;
  ipsix = ipsik * 5;
  ipq = 0;
  /*       FIND NU IN INTERVAL [-.5,.5) IF ID=2  ( CALCULATION OF Q ) 
   */
  nu = d_mod (nu1, &c_b9);
  if (nu >= .5)
    {
      nu += -1.;
    }
  /*       FIND NU IN INTERVAL (-1.5,-.5] IF ID=1,3, OR 4  ( CALC. OF P ) 
   */
  if (*id != 2 && nu > -.5)
    {
      nu += -1.;
    }
  /*       CALCULATE MU FACTORIAL 
   */
  k = *mu;
  dmu = (double) (*mu);
  if (*mu <= 0)
    {
      goto L60;
    }
  factmu = 1.;
  if__ = 0;
  i__1 = k;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      factmu *= i__;
      /* L50: */
      nsp_calpack_dxadj (&factmu, &if__, ierror);
    }
  if (*ierror != 0)
    {
      return 0;
    }
 L60:
  if (k == 0)
    {
      factmu = 1.;
    }
  if (k == 0)
    {
      if__ = 0;
    }
  /* 
   *       X=COS(THETA) 
   *       Y=SIN(THETA/2)**2=(1-X)/2=.5-.5*X 
   *       R=TAN(THETA/2)=SQRT((1-X)/(1+X) 
   * 
   */
  y = (1. - *x) * .5;
  r__ = sqrt ((1. - *x) / (*x + 1.));
  /* 
   *       USE ASCENDING SERIES TO CALCULATE TWO VALUES OF P OR Q 
   *       FOR USE AS STARTING VALUES IN RECURRENCE RELATION. 
   * 
   */
  pq2 = 0.;
  for (j = 1; j <= 2; ++j)
    {
      ipq1 = 0;
      if (*id == 2)
	{
	  goto L80;
	}
      /* 
       *       SERIES FOR P ( ID = 1, 3, OR 4 ) 
       *       P(-MU,NU,X)=1./FACTORIAL(MU)*SQRT(((1.-X)/(1.+X))**MU) 
       *               *SUM(FROM 0 TO J0-1)A(J)*(.5-.5*X)**J 
       * 
       */
      ipq = 0;
      pq = 1.;
      a = 1.;
      ia = 0;
      i__1 = j0;
      for (i__ = 2; i__ <= i__1; ++i__)
	{
	  di = (double) i__;
	  a =
	    a * y * (di - 2. - nu) * (di - 1. +
				      nu) / ((di - 1. + dmu) * (di - 1.));
	  nsp_calpack_dxadj (&a, &ia, ierror);
	  if (*ierror != 0)
	    {
	      return 0;
	    }
	  if (a == 0.)
	    {
	      goto L66;
	    }
	  nsp_calpack_dxadd (&pq, &ipq, &a, &ia, &pq, &ipq, ierror);
	  if (*ierror != 0)
	    {
	      return 0;
	    }
	  /* L65: */
	}
    L66:
      if (*mu <= 0)
	{
	  goto L90;
	}
      x2 = r__;
      x1 = pq;
      k = *mu;
      i__1 = k;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  x1 *= x2;
	  /* L77: */
	  nsp_calpack_dxadj (&x1, &ipq, ierror);
	}
      if (*ierror != 0)
	{
	  return 0;
	}
      pq = x1 / factmu;
      ipq -= if__;
      nsp_calpack_dxadj (&pq, &ipq, ierror);
      if (*ierror != 0)
	{
	  return 0;
	}
      goto L90;
      /* 
       *       Z=-LN(R)=.5*LN((1+X)/(1-X)) 
       * 
       */
    L80:
      z__ = -log (r__);
      d__1 = nu + 1.;
      w = nsp_calpack_dxpsi (&d__1, &ipsik, &ipsix);
      xs = *sx;
      /*     XS=1.D0/SIN(THETA) 
       * 
       *       SERIES SUMMATION FOR Q ( ID = 2 ) 
       *       Q(0,NU,X)=SUM(FROM 0 TO J0-1)((.5*LN((1+X)/(1-X)) 
       *   +DXPSI(J+1,IPSIK,IPSIX)-DXPSI(NU+1,IPSIK,IPSIX)))*A(J)*(.5-.5*X)**J 
       * 
       *       Q(1,NU,X)=-SQRT(1./(1.-X**2))+SQRT((1-X)/(1+X)) 
       *            *SUM(FROM 0 T0 J0-1)(-NU*(NU+1)/2*LN((1+X)/(1-X)) 
       *                +(J-NU)*(J+NU+1)/(2*(J+1))+NU*(NU+1)* 
       *    (DXPSI(NU+1,IPSIK,IPSIX)-DXPSI(J+1,IPSIK,IPSIX))*A(J)*(.5-.5*X)**J 
       * 
       *       NOTE, IN THIS LOOP K=J+1 
       * 
       *pour le cas ou XS serait modifie par la suite 
       */
      pq = 0.;
      ipq = 0;
      ia = 0;
      a = 1.;
      i__1 = j0;
      for (k = 1; k <= i__1; ++k)
	{
	  flok = (double) k;
	  if (k == 1)
	    {
	      goto L81;
	    }
	  a =
	    a * y * (flok - 2. - nu) * (flok - 1. +
					nu) / ((flok - 1. + dmu) * (flok -
								    1.));
	  nsp_calpack_dxadj (&a, &ia, ierror);
	  if (*ierror != 0)
	    {
	      return 0;
	    }
	L81:
	  if (*mu >= 1)
	    {
	      goto L83;
	    }
	  x1 = (nsp_calpack_dxpsi (&flok, &ipsik, &ipsix) - w + z__) * a;
	  ix1 = ia;
	  nsp_calpack_dxadd (&pq, &ipq, &x1, &ix1, &pq, &ipq, ierror);
	  if (*ierror != 0)
	    {
	      return 0;
	    }
	  goto L85;
	L83:
	  x1 =
	    (nu * (nu + 1.) *
	     (z__ - w + nsp_calpack_dxpsi (&flok, &ipsik, &ipsix)) + (nu -
								      flok +
								      1.) *
	     (nu + flok) / (flok * 2.)) * a;
	  ix1 = ia;
	  nsp_calpack_dxadd (&pq, &ipq, &x1, &ix1, &pq, &ipq, ierror);
	  if (*ierror != 0)
	    {
	      return 0;
	    }
	L85:
	  ;
	}
      if (*mu >= 1)
	{
	  pq = -r__ * pq;
	}
      ixs = 0;
      if (*mu >= 1)
	{
	  d__1 = -xs;
	  nsp_calpack_dxadd (&pq, &ipq, &d__1, &ixs, &pq, &ipq, ierror);
	}
      if (*ierror != 0)
	{
	  return 0;
	}
      if (j == 2)
	{
	  *mu = -(*mu);
	}
      if (j == 2)
	{
	  dmu = -dmu;
	}
    L90:
      if (j == 1)
	{
	  pq2 = pq;
	}
      if (j == 1)
	{
	  ipq2 = ipq;
	}
      nu += 1.;
      /* L100: */
    }
  k = 0;
  if (nu - 1.5 < *nu1)
    {
      goto L120;
    }
  ++k;
  pqa[k] = pq2;
  ipqa[k] = ipq2;
  if (nu > *nu2 + .5)
    {
      return 0;
    }
 L120:
  pq1 = pq;
  ipq1 = ipq;
  if (nu < *nu1 + .5)
    {
      goto L130;
    }
  ++k;
  pqa[k] = pq;
  ipqa[k] = ipq;
  if (nu > *nu2 + .5)
    {
      return 0;
    }
  /* 
   *       FORWARD NU-WISE RECURRENCE FOR F(MU,NU,X) FOR FIXED MU 
   *       USING 
   *       (NU+MU+1)*F(MU,NU,X)=(2.*NU+1)*F(MU,NU,X)-(NU-MU)*F(MU,NU-1,X) 
   *       WHERE F(MU,NU,X) MAY BE P(-MU,NU,X) OR IF MU IS REPLACED 
   *       BY -MU THEN F(MU,NU,X) MAY BE Q(MU,NU,X). 
   *       NOTE, IN THIS LOOP, NU=NU+1 
   * 
   */
 L130:
  x1 = (nu * 2. - 1.) / (nu + dmu) * *x * pq1;
  x2 = (nu - 1. - dmu) / (nu + dmu) * pq2;
  d__1 = -x2;
  nsp_calpack_dxadd (&x1, &ipq1, &d__1, &ipq2, &pq, &ipq, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  nsp_calpack_dxadj (&pq, &ipq, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  nu += 1.;
  pq2 = pq1;
  ipq2 = ipq1;
  goto L120;
  /* 
   */
}

/* DXPSI 
****SUBSIDIARY 
* To compute values of the Psi function for DXLEGF. 
*Slatec Library 
*
****KEYWORDS  PSI FUNCTION 
* Author:  Smith, John M., (NBS and George Mason University) 
****ROUTINES CALLED  (NONE) 
****REVISION HISTORY  (YYMMDD) 
*  820728  DATE WRITTEN 
*  890126  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
*  901019  Revisions to prologue.  (DWL and WRB) 
*  901106  Changed all specific intrinsics to generic.  (WRB) 
*          Corrected order of sections in prologue and added TYPE 
*          section.  (WRB) 
*  920127  Revised PURPOSE section of prologue.  (DWL) 
****END PROLOGUE  DXPSI 
* 
*       CNUM(I) AND CDENOM(I) ARE THE ( REDUCED ) NUMERATOR 
*       AND 2*I*DENOMINATOR RESPECTIVELY OF THE 2*I TH BERNOULLI 
*       NUMBER. 
*/


double nsp_calpack_dxpsi (double *a, int *ipsik, int *ipsix)
{
  /* Initialized data */

  static double cnum[12] =
    { 1., -1., 1., -1., 1., -691., 1., -3617., 43867., -174611., 77683.,
      -236364091.
    };
  static double cdenom[12] =
    { 12., 120., 252., 240., 132., 32760., 12., 8160., 14364., 6600., 276.,
      65520.
    };

  /* System generated locals */
  int i__1, i__2;
  double ret_val, d__1;

  /* Local variables */
  double b, c__;
  int i__, k, m, n, k1;


  i__1 = 0, i__2 = *ipsix - (int) (*a);
  n = Max (i__1, i__2);
  b = n + *a;
  k1 = *ipsik - 1;
  /* 
   *       SERIES EXPANSION FOR A .GT. IPSIX USING IPSIK-1 TERMS. 
   * 
   */
  c__ = 0.;
  i__1 = k1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      k = *ipsik - i__;
      /* L12: */
      /*Computing 2nd power 
       */
      d__1 = b;
      c__ = (c__ + cnum[k - 1] / cdenom[k - 1]) / (d__1 * d__1);
    }
  ret_val = log (b) - (c__ + .5 / b);
  if (n == 0)
    {
      goto L20;
    }
  b = 0.;
  /* 
   *       RECURRENCE FOR A .LE. IPSIX. 
   * 
   */
  i__1 = n;
  for (m = 1; m <= i__1; ++m)
    {
      /* L15: */
      b += 1. / (n - m + *a);
    }
  ret_val -= b;
 L20:
  return ret_val;
}				/* dxpsi_ */

/* DXQMU 
****SUBSIDIARY 
* To compute the values of Legendre functions for DXLEGF. 
*           Method: forward mu-wise recurrence for Q(MU,NU,X) for fixed 
*           nu to obtain Q(MU1,NU,X), Q(MU1+1,NU,X), ..., Q(MU2,NU,X). 
*Slatec Library 
*
*
* Author:  Smith, John M., (NBS and George Mason University) 
****ROUTINES CALLED  DXADD, DXADJ, DXPQNU 
****REVISION HISTORY  (YYMMDD) 
*  820728  DATE WRITTEN 
*  890126  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
*  901019  Revisions to prologue.  (DWL and WRB) 
*  901106  Corrected order of sections in prologue and added TYPE 
*          section.  (WRB) 
*  920127  Revised PURPOSE section of prologue.  (DWL) 
*/


int
nsp_calpack_dxqmu (double *nu1, double *nu2, int *mu1, int *mu2, double *x,
		   double *sx, int *id, double *pqa, int *ipqa, int *ierror)
{
  /* System generated locals */
  double d__1;

  /* Local variables */
  int k;
  double x1, x2, pq;
  int mu;
  double nu;
  double pq1, pq2, dmu;
  int ipq, ipq1, ipq2;

  /* Parameter adjustments */
  --ipqa;
  --pqa;

  /* Function Body */
  *ierror = 0;
  mu = 0;
  /* 
   *       CALL DXPQNU TO OBTAIN Q(0.,NU1,X) 
   * 
   */
  nsp_calpack_dxpqnu (nu1, nu2, &mu, x, sx, id, &pqa[1], &ipqa[1], ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  pq2 = pqa[1];
  ipq2 = ipqa[1];
  mu = 1;
  /* 
   *       CALL DXPQNU TO OBTAIN Q(1.,NU1,X) 
   * 
   */
  nsp_calpack_dxpqnu (nu1, nu2, &mu, x, sx, id, &pqa[1], &ipqa[1], ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  nu = *nu1;
  k = 0;
  mu = 1;
  dmu = 1.;
  pq1 = pqa[1];
  ipq1 = ipqa[1];
  if (*mu1 > 0)
    {
      goto L310;
    }
  ++k;
  pqa[k] = pq2;
  ipqa[k] = ipq2;
  if (*mu2 < 1)
    {
      goto L330;
    }
 L310:
  if (*mu1 > 1)
    {
      goto L320;
    }
  ++k;
  pqa[k] = pq1;
  ipqa[k] = ipq1;
  if (*mu2 <= 1)
    {
      goto L330;
    }
 L320:
  /* 
   *       FORWARD RECURRENCE IN MU TO OBTAIN 
   *                 Q(MU1,NU,X),Q(MU1+1,NU,X),....,Q(MU2,NU,X) USING 
   *            Q(MU+1,NU,X)=-2.*MU*X*SQRT(1./(1.-X**2))*Q(MU,NU,X) 
   *                              -(NU+MU)*(NU-MU+1.)*Q(MU-1,NU,X) 
   * 
   */
  x1 = dmu * -2. * *x * *sx * pq1;
  x2 = (nu + dmu) * (nu - dmu + 1.) * pq2;
  d__1 = -x2;
  nsp_calpack_dxadd (&x1, &ipq1, &d__1, &ipq2, &pq, &ipq, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  nsp_calpack_dxadj (&pq, &ipq, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  pq2 = pq1;
  ipq2 = ipq1;
  pq1 = pq;
  ipq1 = ipq;
  ++mu;
  dmu += 1.;
  if (mu < *mu1)
    {
      goto L320;
    }
  ++k;
  pqa[k] = pq;
  ipqa[k] = ipq;
  if (*mu2 > mu)
    {
      goto L320;
    }
 L330:
  return 0;
}				/* dxqmu_ */

/* DXQNU 
****SUBSIDIARY 
* To compute the values of Legendre functions for DXLEGF. 
*           Method: backward nu-wise recurrence for Q(MU,NU,X) for 
*           fixed mu to obtain Q(MU1,NU1,X), Q(MU1,NU1+1,X), ..., 
*           Q(MU1,NU2,X). 
*Slatec Library 
*
*
* Author:  Smith, John M., (NBS and George Mason University) 
****ROUTINES CALLED  DXADD, DXADJ, DXPQNU 
****REVISION HISTORY  (YYMMDD) 
*  820728  DATE WRITTEN 
*  890126  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
*  901019  Revisions to prologue.  (DWL and WRB) 
*  901106  Corrected order of sections in prologue and added TYPE 
*          section.  (WRB) 
*  920127  Revised PURPOSE section of prologue.  (DWL) 
*/

int
nsp_calpack_dxqnu (double *nu1, double *nu2, int *mu1, double *x, double *sx,
		   int *id, double *pqa, int *ipqa, int *ierror)
{
  /* System generated locals */
  double d__1;

  /* Local variables */
  int ipql1, ipql2, k;
  double x1, x2, pq;
  int mu;
  double nu;
  double pq1, pq2, dmu;
  int ipq, ipq1, ipq2;
  double pql1, pql2;

  /* Parameter adjustments */
  --ipqa;
  --pqa;

  /* Function Body */
  *ierror = 0;
  k = 0;
  pq2 = 0.;
  ipq2 = 0;
  pql2 = 0.;
  ipql2 = 0;
  if (*mu1 == 1)
    {
      goto L290;
    }
  mu = 0;
  /* 
   *       CALL DXPQNU TO OBTAIN Q(0.,NU2,X) AND Q(0.,NU2-1,X) 
   * 
   */
  nsp_calpack_dxpqnu (nu1, nu2, &mu, x, sx, id, &pqa[1], &ipqa[1], ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  if (*mu1 == 0)
    {
      return 0;
    }
  k = (int) (*nu2 - *nu1 + 1.5);
  pq2 = pqa[k];
  ipq2 = ipqa[k];
  pql2 = pqa[k - 1];
  ipql2 = ipqa[k - 1];
 L290:
  mu = 1;
  /* 
   *       CALL DXPQNU TO OBTAIN Q(1.,NU2,X) AND Q(1.,NU2-1,X) 
   * 
   */
  nsp_calpack_dxpqnu (nu1, nu2, &mu, x, sx, id, &pqa[1], &ipqa[1], ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  if (*mu1 == 1)
    {
      return 0;
    }
  nu = *nu2;
  pq1 = pqa[k];
  ipq1 = ipqa[k];
  pql1 = pqa[k - 1];
  ipql1 = ipqa[k - 1];
 L300:
  mu = 1;
  dmu = 1.;
 L320:
  /* 
   *       FORWARD RECURRENCE IN MU TO OBTAIN Q(MU1,NU2,X) AND 
   *             Q(MU1,NU2-1,X) USING 
   *             Q(MU+1,NU,X)=-2.*MU*X*SQRT(1./(1.-X**2))*Q(MU,NU,X) 
   *                  -(NU+MU)*(NU-MU+1.)*Q(MU-1,NU,X) 
   * 
   *             FIRST FOR NU=NU2 
   * 
   */
  x1 = dmu * -2. * *x * *sx * pq1;
  x2 = (nu + dmu) * (nu - dmu + 1.) * pq2;
  d__1 = -x2;
  nsp_calpack_dxadd (&x1, &ipq1, &d__1, &ipq2, &pq, &ipq, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  nsp_calpack_dxadj (&pq, &ipq, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  pq2 = pq1;
  ipq2 = ipq1;
  pq1 = pq;
  ipq1 = ipq;
  ++mu;
  dmu += 1.;
  if (mu < *mu1)
    {
      goto L320;
    }
  pqa[k] = pq;
  ipqa[k] = ipq;
  if (k == 1)
    {
      return 0;
    }
  if (nu < *nu2)
    {
      goto L340;
    }
  /* 
   *             THEN FOR NU=NU2-1 
   * 
   */
  nu += -1.;
  pq2 = pql2;
  ipq2 = ipql2;
  pq1 = pql1;
  ipq1 = ipql1;
  --k;
  goto L300;
  /* 
   *        BACKWARD RECURRENCE IN NU TO OBTAIN 
   *             Q(MU1,NU1,X),Q(MU1,NU1+1,X),....,Q(MU1,NU2,X) 
   *             USING 
   *             (NU-MU+1.)*Q(MU,NU+1,X)= 
   *                      (2.*NU+1.)*X*Q(MU,NU,X)-(NU+MU)*Q(MU,NU-1,X) 
   * 
   */
 L340:
  pq1 = pqa[k];
  ipq1 = ipqa[k];
  pq2 = pqa[k + 1];
  ipq2 = ipqa[k + 1];
 L350:
  if (nu <= *nu1)
    {
      return 0;
    }
  --k;
  x1 = (nu * 2. + 1.) * *x * pq1 / (nu + dmu);
  x2 = -(nu - dmu + 1.) * pq2 / (nu + dmu);
  nsp_calpack_dxadd (&x1, &ipq1, &x2, &ipq2, &pq, &ipq, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  nsp_calpack_dxadj (&pq, &ipq, ierror);
  if (*ierror != 0)
    {
      return 0;
    }
  pq2 = pq1;
  ipq2 = ipq1;
  pq1 = pq;
  ipq1 = ipq;
  pqa[k] = pq;
  ipqa[k] = ipq;
  nu += -1.;
  goto L350;
}				/* dxqnu_ */

/* DXRED 
 * To provide double-precision floating-point arithmetic 
 *           with an extended exponent range. 
 *Slatec Library 
 ****TYPE      DOUBLE PRECISION (XRED-S, DXRED-D) 
 *
 * Author:  Lozier, Daniel W., (National Bureau of Standards) 
 *          Smith, John M., (NBS and George Mason University) 
 ****DESCRIPTION 
 *    DOUBLE PRECISION X 
 *    INT IX 
 * 
 *                 IF 
 *                 RADIX**(-2L) .LE. (ABS(X),IX) .LE. RADIX**(2L) 
 *                 THEN DXRED TRANSFORMS (X,IX) SO THAT IX=0. 
 *                 IF (X,IX) IS OUTSIDE THE ABOVE RANGE, 
 *                 THEN DXRED TAKES NO ACTION. 
 *                 THIS SUBROUTINE IS USEFUL IF THE 
 *                 RESULTS OF EXTENDED-RANGE CALCULATIONS 
 *                 ARE TO BE USED IN SUBSEQUENT ORDINARY 
 *                 DOUBLE-PRECISION CALCULATIONS. 
 * 
 ****SEE ALSO  DXSET 
 ****REFERENCES  (NONE) 
 ****ROUTINES CALLED  (NONE) 
 ****COMMON BLOCKS    DXBLK2 
 ****REVISION HISTORY  (YYMMDD) 
 *  820712  DATE WRITTEN 
 *  881020  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
 *  901019  Revisions to prologue.  (DWL and WRB) 
 *  901106  Changed all specific intrinsics to generic.  (WRB) 
 *          Corrected order of sections in prologue and added TYPE 
 *          section.  (WRB) 
 *  920127  Revised PURPOSE section of prologue.  (DWL) 
 */

int nsp_calpack_dxred (double *x, int *ix, int *ierror)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;
  double xa;
  int ixa, ixa1, ixa2;

  *ierror = 0;
  if (*x == 0.)
    {
      goto L90;
    }
  xa = Abs (*x);
  if (*ix == 0)
    {
      goto L70;
    }
  ixa = Abs (*ix);
  ixa1 = ixa / dxblk2_1.l2;
  ixa2 = ixa % dxblk2_1.l2;
  if (*ix > 0)
    {
      goto L40;
    }
 L10:
  if (xa > 1.)
    {
      goto L20;
    }
  xa *= dxblk2_1.rad2l;
  ++ixa1;
  goto L10;
 L20:
  xa /= pow_di_ref (&dxblk2_1.radix, &ixa2);
  if (ixa1 == 0)
    {
      goto L70;
    }
  i__1 = ixa1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (xa < 1.)
	{
	  goto L100;
	}
      xa /= dxblk2_1.rad2l;
      /* L30: */
    }
  goto L70;
  /* 
   */
 L40:
  if (xa < 1.)
    {
      goto L50;
    }
  xa /= dxblk2_1.rad2l;
  ++ixa1;
  goto L40;
 L50:
  xa *= pow_di_ref (&dxblk2_1.radix, &ixa2);
  if (ixa1 == 0)
    {
      goto L70;
    }
  i__1 = ixa1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (xa > 1.)
	{
	  goto L100;
	}
      xa *= dxblk2_1.rad2l;
      /* L60: */
    }
 L70:
  if (xa > dxblk2_1.rad2l)
    {
      goto L100;
    }
  if (xa > 1.)
    {
      goto L80;
    }
  if (dxblk2_1.rad2l * xa < 1.)
    {
      goto L100;
    }
 L80:
  *x = d_sign (&xa, x);
 L90:
  *ix = 0;
 L100:
  return 0;
}				/* dxred_ */


/* DXSET 
 *                (X,IX)*(Y,IY) + (U,IU)*(V,IV) 
 * 
 *CAN BE COMPUTED AND STORED IN ADJUSTED FORM WITH NO EXPLICIT 
 *CALLS TO DXADJ. 
 * 
 *  WHEN AN EXTENDED-RANGE NUMBER IS TO BE PRINTED, IT MUST BE 
 *CONVERTED TO AN EXTENDED-RANGE FORM WITH DECIMAL RADIX.  SUBROUTINE 
 *DXCON IS PROVIDED FOR THIS PURPOSE. 
 * 
 *  THE SUBROUTINES CONTAINED IN THIS PACKAGE ARE 
 * 
 *    SUBROUTINE DXADD 
 *USAGE 
 *                 CALL DXADD(X,IX,Y,IY,Z,IZ,IERROR) 
 *                 IF (IERROR.NE.0) RETURN 
 *DESCRIPTION 
 *                 FORMS THE EXTENDED-RANGE SUM  (Z,IZ) = 
 *                 (X,IX) + (Y,IY).  (Z,IZ) IS ADJUSTED 
 *                 BEFORE RETURNING. THE INPUT OPERANDS 
 *                 NEED NOT BE IN ADJUSTED FORM, BUT THEIR 
 *                 PRINCIPAL PARTS MUST SATISFY 
 *                 RADIX**(-2L).LE.ABS(X).LE.RADIX**(2L), 
 *                 RADIX**(-2L).LE.ABS(Y).LE.RADIX**(2L). 
 * 
 *    SUBROUTINE DXADJ 
 *USAGE 
 *                 CALL DXADJ(X,IX,IERROR) 
 *                 IF (IERROR.NE.0) RETURN 
 *DESCRIPTION 
 *                 TRANSFORMS (X,IX) SO THAT 
 *                 RADIX**(-L) .LE. ABS(X) .LT. RADIX**L. 
 *                 ON MOST COMPUTERS THIS TRANSFORMATION DOES 
 *                 NOT CHANGE THE MANTISSA OF X PROVIDED RADIX IS 
 *                 THE NUMBER BASE OF DOUBLE-PRECISION ARITHMETIC. 
 * 
 *    SUBROUTINE DXC210 
 *USAGE 
 *                 CALL DXC210(K,Z,J,IERROR) 
 *                 IF (IERROR.NE.0) RETURN 
 *DESCRIPTION 
 *                 GIVEN K THIS SUBROUTINE COMPUTES J AND Z 
 *                 SUCH THAT  RADIX**K = Z*10**J, WHERE Z IS IN 
 *                 THE RANGE 1/10 .LE. Z .LT. 1. 
 *                 THE VALUE OF Z WILL BE ACCURATE TO FULL 
 *                 DOUBLE-PRECISION PROVIDED THE NUMBER 
 *                 OF DECIMAL PLACES IN THE LARGEST 
 *                 INT PLUS THE NUMBER OF DECIMAL 
 *                 PLACES CARRIED IN DOUBLE-PRECISION DOES NOT 
 *                 EXCEED 60. DXC210 IS CALLED BY SUBROUTINE 
 *                 DXCON WHEN NECESSARY. THE USER SHOULD 
 *                 NEVER NEED TO CALL DXC210 DIRECTLY. 
 * 
 *    SUBROUTINE DXCON 
 *USAGE 
 *                 CALL DXCON(X,IX,IERROR) 
 *                 IF (IERROR.NE.0) RETURN 
 *DESCRIPTION 
 *                 CONVERTS (X,IX) = X*RADIX**IX 
 *                 TO DECIMAL FORM IN PREPARATION FOR 
 *                 PRINTING, SO THAT (X,IX) = X*10**IX 
 *                 WHERE 1/10 .LE. ABS(X) .LT. 1 
 *                 IS RETURNED, EXCEPT THAT IF 
 *                 (ABS(X),IX) IS BETWEEN RADIX**(-2L) 
 *                 AND RADIX**(2L) THEN THE REDUCED 
 *                 FORM WITH IX = 0 IS RETURNED. 
 * 
 *    SUBROUTINE DXRED 
 *USAGE 
 *                 CALL DXRED(X,IX,IERROR) 
 *                 IF (IERROR.NE.0) RETURN 
 *DESCRIPTION 
 *                 IF 
 *                 RADIX**(-2L) .LE. (ABS(X),IX) .LE. RADIX**(2L) 
 *                 THEN DXRED TRANSFORMS (X,IX) SO THAT IX=0. 
 *                 IF (X,IX) IS OUTSIDE THE ABOVE RANGE, 
 *                 THEN DXRED TAKES NO ACTION. 
 *                 THIS SUBROUTINE IS USEFUL IF THE 
 *                 RESULTS OF EXTENDED-RANGE CALCULATIONS 
 *                 ARE TO BE USED IN SUBSEQUENT ORDINARY 
 *                 DOUBLE-PRECISION CALCULATIONS. 
 * 
 ****REFERENCES  Smith, Olver and Lozier, Extended-Range Arithmetic and 
 *                Normalized Legendre Polynomials, ACM Trans on Math 
 *                Softw, v 7, n 1, March 1981, pp 93--105. 
 ****ROUTINES CALLED  I1MACH, XERMSG 
 ****COMMON BLOCKS    DXBLK1, DXBLK2, DXBLK3 
 ****REVISION HISTORY  (YYMMDD) 
 *  820712  DATE WRITTEN 
 *  881020  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
 *  901019  Revisions to prologue.  (DWL and WRB) 
 *  901106  Changed all specific intrinsics to generic.  (WRB) 
 *          Corrected order of sections in prologue and added TYPE 
 *          section.  (WRB) 
 *          CALLs to XERROR changed to CALLs to XERMSG.  (WRB) 
 *  920127  Revised PURPOSE section of prologue.  (DWL) 
 ****END PROLOGUE  DXSET 
 *    dlamch is used in place of i1mach : 
 * 
 * 
 *  LOG102 CONTAINS THE FIRST 60 DIGITS OF LOG10(2) FOR USE IN 
 *CONVERSION OF EXTENDED-RANGE NUMBERS TO BASE 10 . 
 */

int
nsp_calpack_dxset (int *irad, int *nradpl, double *dzero, int *nbits,
		   int *ierror)
{
  /* Initialized data */

  static int log102[20] =
    { 301, 29, 995, 663, 981, 195, 213, 738, 894, 724, 493, 26, 768, 189, 881,
      462, 108, 541, 310, 428
    };
  static int iflag = 0;

  /* System generated locals */
  int i__1, i__2;
  double d__1;
  
  /* Local variables */
  int lg102x, log2r, i__, j, k, iradx, ic, nb, ii, kk;
  int it, lx, nrdplc, lgtemp[20], iminex, imaxex, nbitsx;
  double dzerox;
  int np1;

  /* 
   *FOLLOWING CODING PREVENTS DXSET FROM BEING EXECUTED MORE THAN ONCE. 
   *THIS IS IMPORTANT BECAUSE SOME SUBROUTINES (SUCH AS DXNRMP AND 
   *DXLEGF) CALL DXSET TO MAKE SURE EXTENDED-RANGE ARITHMETIC HAS 
   *BEEN INITIALIZED. THE USER MAY WANT TO PRE-EMPT THIS CALL, FOR 
   *EXAMPLE WHEN I1MACH IS NOT AVAILABLE. SEE CODING BELOW. 
   */
  *ierror = 0;
  if (iflag != 0)
    {
      return 0;
    }
  iradx = *irad;
  nrdplc = *nradpl;
  dzerox = *dzero;
  iminex = 0;
  imaxex = 0;
  nbitsx = *nbits;
  /*FOLLOWING 5 STATEMENTS SHOULD BE DELETED IF I1MACH IS 
   *NOT AVAILABLE OR NOT CONFIGURED TO RETURN THE CORRECT 
   *MACHINE-DEPENDENT VALUES. 
   * 
   *modif : use a call to dlamch in place of I1MACH 
   */
  if (iradx == 0)
    {
      iradx = (int) C2F (dlamch) ("b", 1L);
    }
  /*I1MACH (10) 
   */
  if (nrdplc == 0)
    {
      nrdplc = (int) C2F (dlamch) ("n", 1L);
    }
  /*I1MACH (14) 
   */
  if (dzerox == 0.)
    {
      iminex = (int) C2F (dlamch) ("m", 1L);
    }
  /*I1MACH (15) 
   */
  if (dzerox == 0.)
    {
      imaxex = (int) C2F (dlamch) ("l", 1L);
    }
  /*I1MACH (16) 
   */
  if (nbitsx == 0)
    {
      nbitsx = 31;
    }
  /*I1MACH (8) 
   */
  if (iradx == 2)
    {
      goto L10;
    }
  if (iradx == 4)
    {
      goto L10;
    }
  if (iradx == 8)
    {
      goto L10;
    }
  if (iradx == 16)
    {
      goto L10;
    }
  /*     CALL XERMSG ('SLATEC', 'DXSET', 'IMPROPER VALUE OF IRAD', 201, 1) 
   */
  *ierror = 201;
  return 0;
 L10:
  log2r = 0;
  if (iradx == 2)
    {
      log2r = 1;
    }
  if (iradx == 4)
    {
      log2r = 2;
    }
  if (iradx == 8)
    {
      log2r = 3;
    }
  if (iradx == 16)
    {
      log2r = 4;
    }
  dxblk1_1.nbitsf = log2r * nrdplc;
  dxblk2_1.radix = (double) iradx;
  dxblk2_1.dlg10r = d_lg10 (&dxblk2_1.radix);
  if (dzerox != 0.)
    {
      goto L14;
    }
  /*Computing MIN 
   */
  i__1 = (1 - iminex) / 2, i__2 = (imaxex - 1) / 2;
  lx = Min (i__1, i__2);
  goto L16;
 L14:
  lx = (int) (d_lg10 (&dzerox) * .5 / dxblk2_1.dlg10r);
  /*RADIX**(2*L) SHOULD NOT OVERFLOW, BUT REDUCE L BY 1 FOR FURTHER 
   *PROTECTION. 
   */
  --lx;
 L16:
  dxblk2_1.l2 = lx << 1;
  if (lx >= 4)
    {
      goto L20;
    }
  /*     CALL XERMSG ('SLATEC', 'DXSET', 'IMPROPER VALUE OF DZERO', 202, 1) 
   */
  *ierror = 202;
  return 0;
 L20:
  dxblk2_1.l = lx;
  dxblk2_1.radixl = pow_di_ref (&dxblk2_1.radix, &dxblk2_1.l);
  /*Computing 2nd power 
   */
  d__1 = dxblk2_1.radixl;
  dxblk2_1.rad2l = d__1 * d__1;
  /*   IT IS NECESSARY TO RESTRICT NBITS (OR NBITSX) TO BE LESS THAN SOME 
   *UPPER LIMIT BECAUSE OF BINARY-TO-DECIMAL CONVERSION. SUCH CONVERSION 
   *IS DONE BY DXC210 AND REQUIRES A CONSTANT THAT IS STORED TO SOME FIXED 
   *PRECISION. THE STORED CONSTANT (LOG102 IN THIS ROUTINE) PROVIDES 
   *FOR CONVERSIONS ACCURATE TO THE LAST DECIMAL DIGIT WHEN THE INT 
   *WORD LENGTH DOES NOT EXCEED 63. A LOWER LIMIT OF 15 BITS IS IMPOSED 
   *BECAUSE THE SOFTWARE IS DESIGNED TO RUN ON COMPUTERS WITH INT WORD 
   *LENGTH OF AT LEAST 16 BITS. 
   */
  if (15 <= nbitsx && nbitsx <= 63)
    {
      goto L30;
    }
  /*     CALL XERMSG ('SLATEC', 'DXSET', 'IMPROPER VALUE OF NBITS', 203, 1) 
   */
  *ierror = 203;
  return 0;
 L30:
  i__1 = nbitsx - 1;
  dxblk2_1.kmax = pow_ii (&c__2, &i__1) - dxblk2_1.l2;
  nb = (nbitsx - 1) / 2;
  dxblk3_1.mlg102 = pow_ii (&c__2, &nb);
  if (1 <= nrdplc * log2r && nrdplc * log2r <= 120)
    {
      goto L40;
    }
  /*     CALL XERMSG ('SLATEC', 'DXSET', 'IMPROPER VALUE OF NRADPL', 204, 
   *    +             1) 
   */
  *ierror = 204;
  return 0;
 L40:
  dxblk3_1.nlg102 = nrdplc * log2r / nb + 3;
  np1 = dxblk3_1.nlg102 + 1;
  /* 
   *  AFTER COMPLETION OF THE FOLLOWING LOOP, IC CONTAINS 
   *THE INT PART AND LGTEMP CONTAINS THE FRACTIONAL PART 
   *OF LOG10(IRADX) IN RADIX 1000. 
   */
  ic = 0;
  for (ii = 1; ii <= 20; ++ii)
    {
      i__ = 21 - ii;
      it = log2r * log102[i__ - 1] + ic;
      ic = it / 1000;
      lgtemp[i__ - 1] = it % 1000;
      /* L50: */
    }
  /* 
   *  AFTER COMPLETION OF THE FOLLOWING LOOP, LG102 CONTAINS 
   *LOG10(IRADX) IN RADIX MLG102. THE RADIX POINT IS 
   *BETWEEN LG102(1) AND LG102(2). 
   */
  dxblk3_1.lg102[0] = ic;
  i__1 = np1;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      lg102x = 0;
      i__2 = nb;
      for (j = 1; j <= i__2; ++j)
	{
	  ic = 0;
	  for (kk = 1; kk <= 20; ++kk)
	    {
	      k = 21 - kk;
	      it = (lgtemp[k - 1] << 1) + ic;
	      ic = it / 1000;
	      lgtemp[k - 1] = it % 1000;
	      /* L60: */
	    }
	  lg102x = (lg102x << 1) + ic;
	  /* L70: */
	}
      dxblk3_1.lg102[i__ - 1] = lg102x;
      /* L80: */
    }
  /* 
   *CHECK SPECIAL CONDITIONS REQUIRED BY SUBROUTINES... 
   */
  if (nrdplc < dxblk2_1.l)
    {
      goto L90;
    }
  /*     CALL XERMSG ('SLATEC', 'DXSET', 'NRADPL .GE. L', 205, 1) 
   */
  *ierror = 205;
  return 0;
 L90:
  if (dxblk2_1.l * 6 <= dxblk2_1.kmax)
    {
      goto L100;
    }
  /*     CALL XERMSG ('SLATEC', 'DXSET', '6*L .GT. KMAX', 206, 1) 
   */
  *ierror = 206;
  return 0;
 L100:
  iflag = 1;
  return 0;
}

/* DXADD 
 * To provide double-precision floating-point arithmetic 
 *           with an extended exponent range. 
 *Slatec Library 
 ****TYPE      DOUBLE PRECISION (XADD-S, DXADD-D) 
 *
 * Author:  Lozier, Daniel W., (National Bureau of Standards) 
 *          Smith, John M., (NBS and George Mason University) 
 ****DESCRIPTION 
 *    DOUBLE PRECISION X, Y, Z 
 *    INT IX, IY, IZ 
 * 
 *                 FORMS THE EXTENDED-RANGE SUM  (Z,IZ) = 
 *                 (X,IX) + (Y,IY).  (Z,IZ) IS ADJUSTED 
 *                 BEFORE RETURNING. THE INPUT OPERANDS 
 *                 NEED NOT BE IN ADJUSTED FORM, BUT THEIR 
 *                 PRINCIPAL PARTS MUST SATISFY 
 *                 RADIX**(-2L).LE.ABS(X).LE.RADIX**(2L), 
 *                 RADIX**(-2L).LE.ABS(Y).LE.RADIX**(2L). 
 * 
 ****SEE ALSO  DXSET 
 ****REFERENCES  (NONE) 
 ****ROUTINES CALLED  DXADJ 
 ****COMMON BLOCKS    DXBLK2 
 ****REVISION HISTORY  (YYMMDD) 
 *  820712  DATE WRITTEN 
 *  881020  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
 *  901019  Revisions to prologue.  (DWL and WRB) 
 *  901106  Changed all specific intrinsics to generic.  (WRB) 
 *          Corrected order of sections in prologue and added TYPE 
 *          section.  (WRB) 
 *  920127  Revised PURPOSE section of prologue.  (DWL) 
 ****END PROLOGUE  DXADD 
 * 
 *  THE CONDITIONS IMPOSED ON L AND KMAX BY THIS SUBROUTINE 
 *ARE 
 *    (1) 1 .LT. L .LE. 0.5D0*LOGR(0.5D0*DZERO) 
 * 
 *    (2) NRADPL .LT. L .LE. KMAX/6 
 * 
 *    (3) KMAX .LE. (2**NBITS - 4*L - 1)/2 
 * 
 *THESE CONDITIONS MUST BE MET BY APPROPRIATE CODING 
 *IN SUBROUTINE DXSET. 
 */

int
nsp_calpack_dxadd (double *x, int *ix, double *y, int *iy, double *z__,
		   int *iz, int *ierror)
{
  int i__1;

  int i__, j;
  double s, t;
  int i1, i2, is;

  *ierror = 0;
  if (*x != 0.)
    {
      goto L10;
    }
  *z__ = *y;
  *iz = *iy;
  goto L220;
 L10:
  if (*y != 0.)
    {
      goto L20;
    }
  *z__ = *x;
  *iz = *ix;
  goto L220;
 L20:
  if (*ix >= 0 && *iy >= 0)
    {
      goto L40;
    }
  if (*ix < 0 && *iy < 0)
    {
      goto L40;
    }
  if (Abs (*ix) <= dxblk2_1.l * 6 && Abs (*iy) <= dxblk2_1.l * 6)
    {
      goto L40;
    }
  if (*ix >= 0)
    {
      goto L30;
    }
  *z__ = *y;
  *iz = *iy;
  goto L220;
 L30:
  *z__ = *x;
  *iz = *ix;
  goto L220;
 L40:
  i__ = *ix - *iy;
  if (i__ < 0)
    {
      goto L80;
    }
  else if (i__ == 0)
    {
      goto L50;
    }
  else
    {
      goto L90;
    }
 L50:
  if (Abs (*x) > 1. && Abs (*y) > 1.)
    {
      goto L60;
    }
  if (Abs (*x) < 1. && Abs (*y) < 1.)
    {
      goto L70;
    }
  *z__ = *x + *y;
  *iz = *ix;
  goto L220;
 L60:
  s = *x / dxblk2_1.radixl;
  t = *y / dxblk2_1.radixl;
  *z__ = s + t;
  *iz = *ix + dxblk2_1.l;
  goto L220;
 L70:
  s = *x * dxblk2_1.radixl;
  t = *y * dxblk2_1.radixl;
  *z__ = s + t;
  *iz = *ix - dxblk2_1.l;
  goto L220;
 L80:
  s = *y;
  is = *iy;
  t = *x;
  goto L100;
 L90:
  s = *x;
  is = *ix;
  t = *y;
 L100:
  /* 
   * AT THIS POINT, THE ONE OF (X,IX) OR (Y,IY) THAT HAS THE 
   *LARGER AUXILIARY INDEX IS STORED IN (S,IS). THE PRINCIPAL 
   *PART OF THE OTHER INPUT IS STORED IN T. 
   * 
   */
  i1 = Abs (i__) / dxblk2_1.l;
  i2 = Abs (i__) % dxblk2_1.l;
  if (Abs (t) >= dxblk2_1.radixl)
    {
      goto L130;
    }
  if (Abs (t) >= 1.)
    {
      goto L120;
    }
  if (dxblk2_1.radixl * Abs (t) >= 1.)
    {
      goto L110;
    }
  j = i1 + 1;
  i__1 = dxblk2_1.l - i2;
  t *= pow_di_ref (&dxblk2_1.radix, &i__1);
  goto L140;
 L110:
  j = i1;
  i__1 = -i2;
  t *= pow_di_ref (&dxblk2_1.radix, &i__1);
  goto L140;
 L120:
  j = i1 - 1;
  if (j < 0)
    {
      goto L110;
    }
  i__1 = -i2;
  t = t * pow_di_ref (&dxblk2_1.radix, &i__1) / dxblk2_1.radixl;
  goto L140;
 L130:
  j = i1 - 2;
  if (j < 0)
    {
      goto L120;
    }
  i__1 = -i2;
  t = t * pow_di_ref (&dxblk2_1.radix, &i__1) / dxblk2_1.rad2l;
 L140:
  /* 
   * AT THIS POINT, SOME OR ALL OF THE DIFFERENCE IN THE 
   *AUXILIARY INDICES HAS BEEN USED TO EFFECT A LEFT SHIFT 
   *OF T.  THE SHIFTED VALUE OF T SATISFIES 
   * 
   *      RADIX**(-2*L) .LE. ABS(T) .LE. 1.0D0 
   * 
   *AND, IF J=0, NO FURTHER SHIFTING REMAINS TO BE DONE. 
   * 
   */
  if (j == 0)
    {
      goto L190;
    }
  if (Abs (s) >= dxblk2_1.radixl || j > 3)
    {
      goto L150;
    }
  if (Abs (s) >= 1.)
    {
      switch (j)
	{
	case 1:
	  goto L180;
	case 2:
	  goto L150;
	case 3:
	  goto L150;
	}
    }
  if (dxblk2_1.radixl * Abs (s) >= 1.)
    {
      switch (j)
	{
	case 1:
	  goto L180;
	case 2:
	  goto L170;
	case 3:
	  goto L150;
	}
    }
  switch (j)
    {
    case 1:
      goto L180;
    case 2:
      goto L170;
    case 3:
      goto L160;
    }
 L150:
  *z__ = s;
  *iz = is;
  goto L220;
 L160:
  s *= dxblk2_1.radixl;
 L170:
  s *= dxblk2_1.radixl;
 L180:
  s *= dxblk2_1.radixl;
 L190:
  /* 
   *  AT THIS POINT, THE REMAINING DIFFERENCE IN THE 
   *AUXILIARY INDICES HAS BEEN USED TO EFFECT A RIGHT SHIFT 
   *OF S.  IF THE SHIFTED VALUE OF S WOULD HAVE EXCEEDED 
   *RADIX**L, THEN (S,IS) IS RETURNED AS THE VALUE OF THE 
   *SUM. 
   * 
   */
  if (Abs (s) > 1. && Abs (t) > 1.)
    {
      goto L200;
    }
  if (Abs (s) < 1. && Abs (t) < 1.)
    {
      goto L210;
    }
  *z__ = s + t;
  *iz = is - j * dxblk2_1.l;
  goto L220;
 L200:
  s /= dxblk2_1.radixl;
  t /= dxblk2_1.radixl;
  *z__ = s + t;
  *iz = is - j * dxblk2_1.l + dxblk2_1.l;
  goto L220;
 L210:
  s *= dxblk2_1.radixl;
  t *= dxblk2_1.radixl;
  *z__ = s + t;
  *iz = is - j * dxblk2_1.l - dxblk2_1.l;
 L220:
  nsp_calpack_dxadj (z__, iz, ierror);
  return 0;
}				/* dxadd_ */

/* DXADJ 
 * To provide double-precision floating-point arithmetic 
 *           with an extended exponent range. 
 * Slatec Library 
 *
 *
 * Author:  Lozier, Daniel W., (National Bureau of Standards) 
 *          Smith, John M., (NBS and George Mason University) 
 ****DESCRIPTION 
 *    DOUBLE PRECISION X 
 *    INT IX 
 * 
 *                 TRANSFORMS (X,IX) SO THAT 
 *                 RADIX**(-L) .LE. ABS(X) .LT. RADIX**L. 
 *                 ON MOST COMPUTERS THIS TRANSFORMATION DOES 
 *                 NOT CHANGE THE MANTISSA OF X PROVIDED RADIX IS 
 *                 THE NUMBER BASE OF DOUBLE-PRECISION ARITHMETIC. 
 * 
 ****SEE ALSO  DXSET 
 ****REFERENCES  (NONE) 
 ****ROUTINES CALLED  XERMSG 
 ****COMMON BLOCKS    DXBLK2 
 ****REVISION HISTORY  (YYMMDD) 
 *  820712  DATE WRITTEN 
 *  881020  Revised to meet SLATEC CML recommendations.  (DWL and JMS) 
 *  901019  Revisions to prologue.  (DWL and WRB) 
 *  901106  Changed all specific intrinsics to generic.  (WRB) 
 *          Corrected order of sections in prologue and added TYPE 
 *          section.  (WRB) 
 *          CALLs to XERROR changed to CALLs to XERMSG.  (WRB) 
 *  920127  Revised PURPOSE section of prologue.  (DWL) 
 ****END PROLOGUE  DXADJ 
 * 
 *  THE CONDITION IMPOSED ON L AND KMAX BY THIS SUBROUTINE 
 *IS 
 *    2*L .LE. KMAX 
 * 
 *THIS CONDITION MUST BE MET BY APPROPRIATE CODING 
 *IN SUBROUTINE DXSET. 
 * 
 */

int nsp_calpack_dxadj (double *x, int *ix, int *ierror)
{
  *ierror = 0;
  if (*x == 0.)
    {
      goto L50;
    }
  if (Abs (*x) >= 1.)
    {
      goto L20;
    }
  if (dxblk2_1.radixl * Abs (*x) >= 1.)
    {
      goto L60;
    }
  *x *= dxblk2_1.rad2l;
  if (*ix < 0)
    {
      goto L10;
    }
  *ix -= dxblk2_1.l2;
  goto L70;
 L10:
  if (*ix < -dxblk2_1.kmax + dxblk2_1.l2)
    {
      goto L40;
    }
  *ix -= dxblk2_1.l2;
  goto L70;
 L20:
  if (Abs (*x) < dxblk2_1.radixl)
    {
      goto L60;
    }
  *x /= dxblk2_1.rad2l;
  if (*ix > 0)
    {
      goto L30;
    }
  *ix += dxblk2_1.l2;
  goto L70;
 L30:
  if (*ix > dxblk2_1.kmax - dxblk2_1.l2)
    {
      goto L40;
    }
  *ix += dxblk2_1.l2;
  goto L70;
 L40:
  /*  40 CALL XERMSG ('SLATEC', 'DXADJ', 'overflow in auxiliary index', 
   *    +             207, 1) 
   */
  *ierror = 207;
  return 0;
 L50:
  *ix = 0;
 L60:
  if (Abs (*ix) > dxblk2_1.kmax)
    {
      goto L40;
    }
 L70:
  return 0;
}			
#endif
