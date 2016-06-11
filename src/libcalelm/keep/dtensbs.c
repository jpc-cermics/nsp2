/* dtensbs.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/* Table of constant values */

#if 0 
static int c__9 = 9;
static int c__1 = 1;

double
nsp_calpack_dbvalu (double *t, double *a, int *n, int *k, int *ideriv,
		    double *x, int *inbv, double *work)
{
  /* System generated locals */
  int i__1, i__2;
  double ret_val;

  /* Builtin functions */
  int s_wsle (cilist *), do_lio (int *, int *, char *, long int),
    e_wsle (void);

  /* Local variables */
  double fkmj;
  int ip1mj, i__, j, mflag, imkpj, j1, j2, iderp1, jj, kmider, ihmkmj;
  int km1, ip1, ihi, imk, kmj, ipj, ilo, kpk;

  /* Fortran I/O blocks */
  static cilist io___21 = { 0, 6, 0, 0, 0 };
  static cilist io___22 = { 0, 6, 0, 0, 0 };
  static cilist io___23 = { 0, 6, 0, 0, 0 };
  static cilist io___24 = { 0, 6, 0, 0, 0 };
  static cilist io___25 = { 0, 6, 0, 0, 0 };
  static cilist io___26 = { 0, 6, 0, 0, 0 };


  /****BEGIN PROLOGUE  DBVALU 
   ****DATE WRITTEN   800901   (YYMMDD) 
   ****REVISION DATE  820801   (YYMMDD) 
   ****REVISION HISTORY  (YYMMDD) 
   *  000330  Modified array declarations.  (JEC) 
   * 
   ****CATEGORY NO.  E3,K6 
   ****KEYWORDS  B-SPLINE,DATA FITTING,DOUBLE PRECISION,INTERPOLATION, 
   *            SPLINE 
   ****AUTHOR  AMOS, D. E., (SNLA) 
   ****PURPOSE  Evaluates the B-representation of a B-spline at X for the 
   *           function value or any of its derivatives. 
   ****DESCRIPTION 
   * 
   *    Written by Carl de Boor and modified by D. E. Amos 
   * 
   *    Reference 
   *        SIAM J. Numerical Analysis, 14, No. 3, June, 1977, pp.441-472. 
   * 
   *    Abstract   **** a double precision routine **** 
   *        DBVALU is the BVALUE function of the reference. 
   * 
   *        DBVALU evaluates the B-representation (T,A,N,K) of a B-spline 
   *        at X for the function value on IDERIV=0 or any of its 
   *        derivatives on IDERIV=1,2,...,K-1.  Right limiting values 
   *        (right derivatives) are returned except at the right end 
   *        point X=T(N+1) where left limiting values are computed.  The 
   *        spline is defined on T(K) .LE. X .LE. T(N+1).  DBVALU returns 
   *        a fatal error message when X is outside of this interval. 
   * 
   *        To compute left derivatives or left limiting values at a 
   *        knot T(I), replace N by I-1 and set X=T(I), I=K+1,N+1. 
   * 
   *        DBVALU calls DINTRV 
   * 
   *    Description of Arguments 
   * 
   *        Input      T,A,X are double precision 
   *         T       - knot vector of length N+K 
   *         A       - B-spline coefficient vector of length N 
   *         N       - number of B-spline coefficients 
   *                   N = sum of knot multiplicities-K 
   *         K       - order of the B-spline, K .GE. 1 
   *         IDERIV  - order of the derivative, 0 .LE. IDERIV .LE. K-1 
   *                   IDERIV = 0 returns the B-spline value 
   *         X       - argument, T(K) .LE. X .LE. T(N+1) 
   *         INBV    - an initialization parameter which must be set 
   *                   to 1 the first time DBVALU is called. 
   * 
   *        Output     WORK,DBVALU are double precision 
   *         INBV    - INBV contains information for efficient process- 
   *                   ing after the initial call and INBV must not 
   *                   be changed by the user.  Distinct splines require 
   *                   distinct INBV parameters. 
   *         WORK    - work vector of length 3*K. 
   *         DBVALU  - value of the IDERIV-th derivative at X 
   * 
   *    Error Conditions 
   *        An improper input is a fatal error 
   ****REFERENCES  C. DE BOOR, *PACKAGE FOR CALCULATING WITH B-SPLINES*, 
   *                SIAM JOURNAL ON NUMERICAL ANALYSIS, VOLUME 14, NO. 3, 
   *                JUNE 1977, PP. 441-472. 
   ****ROUTINES CALLED  DINTRV,XERROR 
   ****END PROLOGUE  DBVALU 
   * 
   * 
   ****FIRST EXECUTABLE STATEMENT  DBVALU 
   */
  /* Parameter adjustments */
  --t;
  --a;
  --work;

  /* Function Body */
  ret_val = 0.;
  if (*k < 1)
    {
      goto L102;
    }
  if (*n < *k)
    {
      goto L101;
    }
  if (*ideriv < 0 || *ideriv >= *k)
    {
      goto L110;
    }
  kmider = *k - *ideriv;
  /* 
   **** FIND *I* IN (K,N) SUCH THAT T(I) .LE. X .LT. T(I+1) 
   *    (OR, .LE. T(I+1) IF T(I) .LT. T(I+1) = T(N+1)). 
   */
  km1 = *k - 1;
  i__1 = *n + 1;
  nsp_calpack_dintrv (&t[1], &i__1, x, inbv, &i__, &mflag);
  if (*x < t[*k])
    {
      goto L120;
    }
  if (mflag == 0)
    {
      goto L20;
    }
  if (*x > t[i__])
    {
      goto L130;
    }
L10:
  if (i__ == *k)
    {
      goto L140;
    }
  --i__;
  if (*x == t[i__])
    {
      goto L10;
    }
  /* 
   **** DIFFERENCE THE COEFFICIENTS *IDERIV* TIMES 
   *    WORK(I) = AJ(I), WORK(K+I) = DP(I), WORK(K+K+I) = DM(I), I=1.K 
   * 
   */
L20:
  imk = i__ - *k;
  i__1 = *k;
  for (j = 1; j <= i__1; ++j)
    {
      imkpj = imk + j;
      work[j] = a[imkpj];
      /* L30: */
    }
  if (*ideriv == 0)
    {
      goto L60;
    }
  i__1 = *ideriv;
  for (j = 1; j <= i__1; ++j)
    {
      kmj = *k - j;
      fkmj = (double) kmj;
      i__2 = kmj;
      for (jj = 1; jj <= i__2; ++jj)
	{
	  ihi = i__ + jj;
	  ihmkmj = ihi - kmj;
	  work[jj] = (work[jj + 1] - work[jj]) / (t[ihi] - t[ihmkmj]) * fkmj;
	  /* L40: */
	}
      /* L50: */
    }
  /* 
   **** COMPUTE VALUE AT *X* IN (T(I),(T(I+1)) OF IDERIV-TH DERIVATIVE, 
   *    GIVEN ITS RELEVANT B-SPLINE COEFF. IN AJ(1),...,AJ(K-IDERIV). 
   */
L60:
  if (*ideriv == km1)
    {
      goto L100;
    }
  ip1 = i__ + 1;
  kpk = *k + *k;
  j1 = *k + 1;
  j2 = kpk + 1;
  i__1 = kmider;
  for (j = 1; j <= i__1; ++j)
    {
      ipj = i__ + j;
      work[j1] = t[ipj] - *x;
      ip1mj = ip1 - j;
      work[j2] = *x - t[ip1mj];
      ++j1;
      ++j2;
      /* L70: */
    }
  iderp1 = *ideriv + 1;
  i__1 = km1;
  for (j = iderp1; j <= i__1; ++j)
    {
      kmj = *k - j;
      ilo = kmj;
      i__2 = kmj;
      for (jj = 1; jj <= i__2; ++jj)
	{
	  work[jj] =
	    (work[jj + 1] * work[kpk + ilo] +
	     work[jj] * work[*k + jj]) / (work[kpk + ilo] + work[*k + jj]);
	  --ilo;
	  /* L80: */
	}
      /* L90: */
    }
L100:
  ret_val = work[1];
  return ret_val;
  /* 
   * 
   */
L101:
  /*     CALL XERROR( ' DBVALU,  N DOES NOT SATISFY N.GE.K',35,2,1) 
   */
  s_wsle (&io___21);
  do_lio (&c__9, &c__1, " DBVALU,  N DOES NOT SATISFY N.GE.K", 35L);
  e_wsle ();
  return ret_val;
L102:
  /*     CALL XERROR( ' DBVALU,  K DOES NOT SATISFY K.GE.1',35,2,1) 
   */
  s_wsle (&io___22);
  do_lio (&c__9, &c__1, " DBVALU,  K DOES NOT SATISFY K.GE.1", 35L);
  e_wsle ();
  return ret_val;
L110:
  /*     CALL XERROR( ' DBVALU,  IDERIV DOES NOT SATISFY 0.LE.IDERIV.LT.K', 
   */
  s_wsle (&io___23);
  do_lio (&c__9, &c__1, " DBVALU,  IDERIV DOES NOT SATISFY 0.LE.IDERIV.LT.K",
	  50L);
  e_wsle ();
  return ret_val;
L120:
  /*     CALL XERROR( ' DBVALU,  X IS N0T GREATER THAN OR EQUAL TO T(K)' 
   */
  s_wsle (&io___24);
  do_lio (&c__9, &c__1, " DBVALU,  X IS N0T GREATER THAN OR EQUAL TO T(K)",
	  48L);
  e_wsle ();
  return ret_val;
L130:
  /*     CALL XERROR( ' DBVALU,  X IS NOT LESS THAN OR EQUAL TO T(N+1)', 
   *    1 47, 2, 1) 
   */
  s_wsle (&io___25);
  do_lio (&c__9, &c__1, " DBVALU,  X IS NOT LESS THAN OR EQUAL TO T(N+1)",
	  47L);
  e_wsle ();
  return ret_val;
L140:
  /*     CALL XERROR( ' DBVALU,  A LEFT LIMITING VALUE CANN0T BE OBTAINED A 
   *    1T T(K)',    58, 2, 1) 
   */
  s_wsle (&io___26);
  do_lio (&c__9, &c__1,
	  " DBVALU, A LEFT LIMITING VALUE CANT BE OBTAINED AT T(K)", 55L);
  e_wsle ();
  return ret_val;
}				/* dbvalu_ */

int
nsp_calpack_dintrv (double *xt, int *lxt, double *x, int *ilo, int *ileft,
		    int *mflag)
{
  int istep, middle, ihi;

  /****BEGIN PROLOGUE  DINTRV 
   ****DATE WRITTEN   800901   (YYMMDD) 
   ****REVISION DATE  820801   (YYMMDD) 
   ****CATEGORY NO.  E3,K6 
   ****KEYWORDS  B-SPLINE,DATA FITTING,DOUBLE PRECISION,INTERPOLATION, 
   *            SPLINE 
   ****AUTHOR  AMOS, D. E., (SNLA) 
   ****PURPOSE  Computes the largest int ILEFT in 1.LE.ILEFT.LE.LXT 
   *           such that XT(ILEFT).LE.X where XT(*) is a subdivision of 
   *           the X interval. 
   ****DESCRIPTION 
   * 
   *    Written by Carl de Boor and modified by D. E. Amos 
   * 
   *    Reference 
   *        SIAM J.  Numerical Analysis, 14, No. 3, June 1977, pp.441-472. 
   * 
   *    Abstract    **** a double precision routine **** 
   *        DINTRV is the INTERV routine of the reference. 
   * 
   *        DINTRV computes the largest int ILEFT in 1 .LE. ILEFT .LE. 
   *        LXT such that XT(ILEFT) .LE. X where XT(*) is a subdivision of 
   *        the X interval.  Precisely, 
   * 
   *                     X .LT. XT(1)                1         -1 
   *        if  XT(I) .LE. X .LT. XT(I+1)  then  ILEFT=I  , MFLAG=0 
   *          XT(LXT) .LE. X                         LXT        1, 
   * 
   *        That is, when multiplicities are present in the break point 
   *        to the left of X, the largest index is taken for ILEFT. 
   * 
   *    Description of Arguments 
   * 
   *        Input      XT,X are double precision 
   *         XT      - XT is a knot or break point vector of length LXT 
   *         LXT     - length of the XT vector 
   *         X       - argument 
   *         ILO     - an initialization parameter which must be set 
   *                   to 1 the first time the spline array XT is 
   *                   processed by DINTRV. 
   * 
   *        Output 
   *         ILO     - ILO contains information for efficient process- 
   *                   ing after the initial call and ILO must not be 
   *                   changed by the user.  Distinct splines require 
   *                   distinct ILO parameters. 
   *         ILEFT   - largest int satisfying XT(ILEFT) .LE. X 
   *         MFLAG   - signals when X lies out of bounds 
   * 
   *    Error Conditions 
   *        None 
   ****REFERENCES  C. DE BOOR, *PACKAGE FOR CALCULATING WITH B-SPLINES*, 
   *                SIAM JOURNAL ON NUMERICAL ANALYSIS, VOLUME 14, NO. 3, 
   *                JUNE 1977, PP. 441-472. 
   ****ROUTINES CALLED  (NONE) 
   ****END PROLOGUE  DINTRV 
   * 
   * 
   ****FIRST EXECUTABLE STATEMENT  DINTRV 
   */
  /* Parameter adjustments */
  --xt;

  /* Function Body */
  ihi = *ilo + 1;
  if (ihi < *lxt)
    {
      goto L10;
    }
  if (*x >= xt[*lxt])
    {
      goto L110;
    }
  if (*lxt <= 1)
    {
      goto L90;
    }
  *ilo = *lxt - 1;
  ihi = *lxt;
  /* 
   */
L10:
  if (*x >= xt[ihi])
    {
      goto L40;
    }
  if (*x >= xt[*ilo])
    {
      goto L100;
    }
  /* 
   **** NOW X .LT. XT(IHI) . FIND LOWER BOUND 
   */
  istep = 1;
L20:
  ihi = *ilo;
  *ilo = ihi - istep;
  if (*ilo <= 1)
    {
      goto L30;
    }
  if (*x >= xt[*ilo])
    {
      goto L70;
    }
  istep <<= 1;
  goto L20;
L30:
  *ilo = 1;
  if (*x < xt[1])
    {
      goto L90;
    }
  goto L70;
  /**** NOW X .GE. XT(ILO) . FIND UPPER BOUND 
   */
L40:
  istep = 1;
L50:
  *ilo = ihi;
  ihi = *ilo + istep;
  if (ihi >= *lxt)
    {
      goto L60;
    }
  if (*x < xt[ihi])
    {
      goto L70;
    }
  istep <<= 1;
  goto L50;
L60:
  if (*x >= xt[*lxt])
    {
      goto L110;
    }
  ihi = *lxt;
  /* 
   **** NOW XT(ILO) .LE. X .LT. XT(IHI) . NARROW THE INTERVAL 
   */
L70:
  middle = (*ilo + ihi) / 2;
  if (middle == *ilo)
    {
      goto L100;
    }
  /*    NOTE. IT IS ASSUMED THAT MIDDLE = ILO IN CASE IHI = ILO+1 
   */
  if (*x < xt[middle])
    {
      goto L80;
    }
  *ilo = middle;
  goto L70;
L80:
  ihi = middle;
  goto L70;
  /**** SET OUTPUT AND RETURN 
   */
L90:
  *mflag = -1;
  *ileft = 1;
  return 0;
L100:
  *mflag = 0;
  *ileft = *ilo;
  return 0;
L110:
  *mflag = 1;
  *ileft = *lxt;
  return 0;
}				/* dintrv_ */

int nsp_calpack_dbknot (double *x, int *n, int *k, double *t)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double rnot;
  int i__, j, jstrt, ip1, ipj, npj;

  /****BEGIN PROLOGUE  DBKNOT 
   ****REFER TO  DB2INK,DB3INK 
   ****ROUTINES CALLED  (NONE) 
   ****REVISION HISTORY  (YYMMDD) 
   *  000330  Modified array declarations.  (JEC) 
   * 
   ****END PROLOGUE  DBKNOT 
   * 
   * -------------------------------------------------------------------- 
   * DBKNOT CHOOSES A KNOT SEQUENCE FOR INTERPOLATION OF ORDER K AT THE 
   * DATA POINTS X(I), I=1,..,N.  THE N+K KNOTS ARE PLACED IN THE ARRAY 
   * T.  K KNOTS ARE PLACED AT EACH ENDPOINT AND NOT-A-KNOT END 
   * CONDITIONS ARE USED.  THE REMAINING KNOTS ARE PLACED AT DATA POINTS 
   * IF N IS EVEN AND BETWEEN DATA POINTS IF N IS ODD.  THE RIGHTMOST 
   * KNOT IS SHIFTED SLIGHTLY TO THE RIGHT TO INSURE PROPER INTERPOLATION 
   * AT X(N) (SEE PAGE 350 OF THE REFERENCE). 
   * DOUBLE PRECISION VERSION OF BKNOT. 
   * -------------------------------------------------------------------- 
   * 
   * ------------ 
   * DECLARATIONS 
   * ------------ 
   * 
   * PARAMETERS 
   * 
   * 
   * LOCAL VARIABLES 
   * 
   * 
   * 
   * ---------------------------- 
   * PUT K KNOTS AT EACH ENDPOINT 
   * ---------------------------- 
   * 
   *    (SHIFT RIGHT ENPOINTS SLIGHTLY -- SEE PG 350 OF REFERENCE) 
   */
  /* Parameter adjustments */
  --x;
  --t;

  /* Function Body */
  rnot = x[*n] + (x[*n] - x[*n - 1]) * .1;
  i__1 = *k;
  for (j = 1; j <= i__1; ++j)
    {
      t[j] = x[1];
      npj = *n + j;
      t[npj] = rnot;
      /* L110: */
    }
  /* 
   * -------------------------- 
   * DISTRIBUTE REMAINING KNOTS 
   * -------------------------- 
   * 
   */
  if (*k % 2 == 1)
    {
      goto L150;
    }
  /* 
   *    CASE OF EVEN K --  KNOTS AT DATA POINTS 
   * 
   */
  i__ = *k / 2 - *k;
  jstrt = *k + 1;
  i__1 = *n;
  for (j = jstrt; j <= i__1; ++j)
    {
      ipj = i__ + j;
      t[j] = x[ipj];
      /* L120: */
    }
  goto L200;
  /* 
   *    CASE OF ODD K --  KNOTS BETWEEN DATA POINTS 
   * 
   */
L150:
  i__ = (*k - 1) / 2 - *k;
  ip1 = i__ + 1;
  jstrt = *k + 1;
  i__1 = *n;
  for (j = jstrt; j <= i__1; ++j)
    {
      ipj = i__ + j;
      t[j] = (x[ipj] + x[ipj + 1]) * .5;
      /* L160: */
    }
L200:
  /* 
   */
  return 0;
}				/* dbknot_ */

int
nsp_calpack_dbtpcf (double *x, int *n, double *fcn, int *ldf, int *nf,
		    double *t, int *k, double *bcoef, double *work)
{
  /* System generated locals */
  int fcn_dim1, fcn_offset, bcoef_dim1, bcoef_offset, i__1, i__2;

  /* Local variables */
  int i__, j, k1, k2, iq, iw;

  /****BEGIN PROLOGUE  DBTPCF 
   ****REFER TO  DB2INK,DB3INK 
   ****ROUTINES CALLED  DBINTK,DBNSLV 
   ****REVISION HISTORY  (YYMMDD) 
   *  000330  Modified array declarations.  (JEC) 
   * 
   ****END PROLOGUE  DBTPCF 
   * 
   * ----------------------------------------------------------------- 
   * DBTPCF COMPUTES B-SPLINE INTERPOLATION COEFFICIENTS FOR NF SETS 
   * OF DATA STORED IN THE COLUMNS OF THE ARRAY FCN. THE B-SPLINE 
   * COEFFICIENTS ARE STORED IN THE ROWS OF BCOEF HOWEVER. 
   * EACH INTERPOLATION IS BASED ON THE N ABCISSA STORED IN THE 
   * ARRAY X, AND THE N+K KNOTS STORED IN THE ARRAY T. THE ORDER 
   * OF EACH INTERPOLATION IS K. THE WORK ARRAY MUST BE OF LENGTH 
   * AT LEAST 2*K*(N+1). 
   * DOUBLE PRECISION VERSION OF BTPCF. 
   * ----------------------------------------------------------------- 
   * 
   * ------------ 
   * DECLARATIONS 
   * ------------ 
   * 
   * PARAMETERS 
   * 
   * 
   * LOCAL VARIABLES 
   * 
   * 
   * --------------------------------------------- 
   * CHECK FOR NULL INPUT AND PARTITION WORK ARRAY 
   * --------------------------------------------- 
   * 
   ****FIRST EXECUTABLE STATEMENT 
   */
  /* Parameter adjustments */
  --x;
  bcoef_dim1 = *nf;
  bcoef_offset = bcoef_dim1 + 1;
  bcoef -= bcoef_offset;
  fcn_dim1 = *ldf;
  fcn_offset = fcn_dim1 + 1;
  fcn -= fcn_offset;
  --t;
  --work;

  /* Function Body */
  if (*nf <= 0)
    {
      goto L500;
    }
  k1 = *k - 1;
  k2 = k1 + *k;
  iq = *n + 1;
  iw = iq + k2 * *n + 1;
  /* 
   * ----------------------------- 
   * COMPUTE B-SPLINE COEFFICIENTS 
   * ----------------------------- 
   * 
   * 
   *  FIRST DATA SET 
   * 
   */
  nsp_calpack_dbintk (&x[1], &fcn[fcn_offset], &t[1], n, k, &work[1],
		      &work[iq], &work[iw]);
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      bcoef[i__ * bcoef_dim1 + 1] = work[i__];
      /* L20: */
    }
  /* 
   * ALL REMAINING DATA SETS BY BACK-SUBSTITUTION 
   * 
   */
  if (*nf == 1)
    {
      goto L500;
    }
  i__1 = *nf;
  for (j = 2; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  work[i__] = fcn[i__ + j * fcn_dim1];
	  /* L50: */
	}
      nsp_calpack_dbnslv (&work[iq], &k2, n, &k1, &k1, &work[1]);
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  bcoef[j + i__ * bcoef_dim1] = work[i__];
	  /* L60: */
	}
      /* L100: */
    }
  /* 
   * ---- 
   * EXIT 
   * ---- 
   * 
   */
L500:
  return 0;
}				/* dbtpcf_ */

int
nsp_calpack_dbintk (double *x, double *y, double *t, int *n, int *k,
		    double *bcoef, double *q, double *work)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int left, lenq, kpkm2, i__, j, iflag, iwork, ilp1mx;
  int jj;
  double xi;
  int km1, np1;

  /****BEGIN PROLOGUE  DBINTK 
   ****DATE WRITTEN   800901   (YYMMDD) 
   ****REVISION DATE  820801   (YYMMDD) 
   ****REVISION HISTORY  (YYMMDD) 
   *  000330  Modified array declarations.  (JEC) 
   * 
   ****CATEGORY NO.  E1A 
   ****KEYWORDS  B-SPLINE,DATA FITTING,DOUBLE PRECISION,INTERPOLATION, 
   *            SPLINE 
   ****AUTHOR  AMOS, D. E., (SNLA) 
   ****PURPOSE  Produces the B-spline coefficients, BCOEF, of the 
   *           B-spline of order K with knots T(I), I=1,...,N+K, which 
   *           takes on the value Y(I) at X(I), I=1,...,N. 
   ****DESCRIPTION 
   * 
   *    Written by Carl de Boor and modified by D. E. Amos 
   * 
   *    References 
   * 
   *        A Practical Guide to Splines by C. de Boor, Applied 
   *        Mathematics Series 27, Springer, 1979. 
   * 
   *    Abstract    **** a double precision routine **** 
   * 
   *        DBINTK is the SPLINT routine of the reference. 
   * 
   *        DBINTK produces the B-spline coefficients, BCOEF, of the 
   *        B-spline of order K with knots T(I), I=1,...,N+K, which 
   *        takes on the value Y(I) at X(I), I=1,...,N.  The spline or 
   *        any of its derivatives can be evaluated by calls to DBVALU. 
   * 
   *        The I-th equation of the linear system A*BCOEF = B for the 
   *        coefficients of the interpolant enforces interpolation at 
   *        X(I), I=1,...,N.  Hence, B(I) = Y(I), for all I, and A is 
   *        a band matrix with 2K-1 bands if A is invertible.  The matrix 
   *        A is generated row by row and stored, diagonal by diagonal, 
   *        in the rows of Q, with the main diagonal going into row K. 
   *        The banded system is then solved by a call to DBNFAC (which 
   *        constructs the triangular factorization for A and stores it 
   *        again in Q), followed by a call to DBNSLV (which then 
   *        obtains the solution BCOEF by substitution).  DBNFAC does no 
   *        pivoting, since the total positivity of the matrix A makes 
   *        this unnecessary.  The linear system to be solved is 
   *        (theoretically) invertible if and only if 
   *                T(I) .LT. X(I) .LT. T(I+K),        for all I. 
   *        Equality is permitted on the left for I=1 and on the right 
   *        for I=N when K knots are used at X(1) or X(N).  Otherwise, 
   *        violation of this condition is certain to lead to an error. 
   * 
   *        DBINTK calls DBSPVN, DBNFAC, DBNSLV, XERROR 
   * 
   *    Description of Arguments 
   * 
   *        Input       X,Y,T are double precision 
   *          X       - vector of length N containing data point abscissa 
   *                    in strictly increasing order. 
   *          Y       - corresponding vector of length N containing data 
   *                    point ordinates. 
   *          T       - knot vector of length N+K 
   *                    Since T(1),..,T(K) .LE. X(1) and T(N+1),..,T(N+K) 
   *                    .GE. X(N), this leaves only N-K knots (not nec- 
   *                    essarily X(I) values) interior to (X(1),X(N)) 
   *          N       - number of data points, N .GE. K 
   *          K       - order of the spline, K .GE. 1 
   * 
   *        Output      BCOEF,Q,WORK are double precision 
   *          BCOEF   - a vector of length N containing the B-spline 
   *                    coefficients 
   *          Q       - a work vector of length (2*K-1)*N, containing 
   *                    the triangular factorization of the coefficient 
   *                    matrix of the linear system being solved.  The 
   *                    coefficients for the interpolant of an 
   *                    additional data set (X(I),yY(I)), I=1,...,N 
   *                    with the same abscissa can be obtained by loading 
   *                    YY into BCOEF and then executing 
   *                        CALL DBNSLV(Q,2K-1,N,K-1,K-1,BCOEF) 
   *          WORK    - work vector of length 2*K 
   * 
   *    Error Conditions 
   *        Improper input is a fatal error 
   *        Singular system of equations is a fatal error 
   ****REFERENCES  C. DE BOOR, *A PRACTICAL GUIDE TO SPLINES*, APPLIED 
   *                MATHEMATICS SERIES 27, SPRINGER, 1979. 
   *              D.E. AMOS, *COMPUTATION WITH SPLINES AND B-SPLINES*, 
   *                SAND78-1968,SANDIA LABORATORIES,MARCH,1979. 
   ****ROUTINES CALLED  DBNFAC,DBNSLV,DBSPVN,XERROR 
   ****END PROLOGUE  DBINTK 
   * 
   * 
   *    DIMENSION Q(2*K-1,N), T(N+K) 
   ****FIRST EXECUTABLE STATEMENT  DBINTK 
   */
  /* Parameter adjustments */
  --t;
  --bcoef;
  --y;
  --x;
  --q;
  --work;

  /* Function Body */
  if (*k < 1)
    {
      goto L100;
    }
  if (*n < *k)
    {
      goto L105;
    }
  jj = *n - 1;
  if (jj == 0)
    {
      goto L6;
    }
  i__1 = jj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (x[i__] >= x[i__ + 1])
	{
	  goto L110;
	}
      /* L5: */
    }
L6:
  np1 = *n + 1;
  km1 = *k - 1;
  kpkm2 = km1 << 1;
  left = *k;
  /*               ZERO OUT ALL ENTRIES OF Q 
   */
  lenq = *n * (*k + km1);
  i__1 = lenq;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      q[i__] = 0.;
      /* L10: */
    }
  /* 
   * ***   LOOP OVER I TO CONSTRUCT THE  N  INTERPOLATION EQUATIONS 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      xi = x[i__];
      /*Computing MIN 
       */
      i__2 = i__ + *k;
      ilp1mx = Min (i__2, np1);
      /*       *** FIND  LEFT  IN THE CLOSED INTERVAL (I,I+K-1) SUCH THAT 
       *               T(LEFT) .LE. X(I) .LT. T(LEFT+1) 
       *       MATRIX IS SINGULAR IF THIS IS NOT POSSIBLE 
       */
      left = Max (left, i__);
      if (xi < t[left])
	{
	  goto L80;
	}
    L20:
      if (xi < t[left + 1])
	{
	  goto L30;
	}
      ++left;
      if (left < ilp1mx)
	{
	  goto L20;
	}
      --left;
      if (xi > t[left + 1])
	{
	  goto L80;
	}
      /*       *** THE I-TH EQUATION ENFORCES INTERPOLATION AT XI, HENCE 
       *       A(I,J) = B(J,K,T)(XI), ALL J. ONLY THE  K  ENTRIES WITH  J = 
       *       LEFT-K+1,...,LEFT ACTUALLY MIGHT BE NONZERO. THESE  K  NUMBERS 
       *       ARE RETURNED, IN  BCOEF (USED FOR TEMP.STORAGE HERE), BY THE 
       *       FOLLOWING 
       */
    L30:
      nsp_calpack_dbspvn (&t[1], k, k, &c__1, &xi, &left, &bcoef[1],
			  &work[1], &iwork);
      /*       WE THEREFORE WANT  BCOEF(J) = B(LEFT-K+J)(XI) TO GO INTO 
       *       A(I,LEFT-K+J), I.E., INTO  Q(I-(LEFT+J)+2*K,(LEFT+J)-K) SINCE 
       *       A(I+J,J)  IS TO GO INTO  Q(I+K,J), ALL I,J,  IF WE CONSIDER  Q 
       *       AS A TWO-DIM. ARRAY , WITH  2*K-1  ROWS (SEE COMMENTS IN 
       *       DBNFAC). IN THE PRESENT PROGRAM, WE TREAT  Q  AS AN EQUIVALENT 
       *       ONE-DIMENSIONAL ARRAY (BECAUSE OF FORTRAN RESTRICTIONS ON 
       *       DIMENSION STATEMENTS) . WE THEREFORE WANT  BCOEF(J) TO GO INTO 
       *       ENTRY 
       *           I -(LEFT+J) + 2*K + ((LEFT+J) - K-1)*(2*K-1) 
       *                  =  I-LEFT+1 + (LEFT -K)*(2*K-1) + (2*K-2)*J 
       *       OF  Q . 
       */
      jj = i__ - left + 1 + (left - *k) * (*k + km1);
      i__2 = *k;
      for (j = 1; j <= i__2; ++j)
	{
	  jj += kpkm2;
	  q[jj] = bcoef[j];
	  /* L40: */
	}
      /* L50: */
    }
  /* 
   *    ***OBTAIN FACTORIZATION OF  A  , STORED AGAIN IN  Q. 
   */
  i__1 = *k + km1;
  nsp_calpack_dbnfac (&q[1], &i__1, n, &km1, &km1, &iflag);
  switch (iflag)
    {
    case 1:
      goto L60;
    case 2:
      goto L90;
    }
  /*    *** SOLVE  A*BCOEF = Y  BY BACKSUBSTITUTION 
   */
L60:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      bcoef[i__] = y[i__];
      /* L70: */
    }
  i__1 = *k + km1;
  nsp_calpack_dbnslv (&q[1], &i__1, n, &km1, &km1, &bcoef[1]);
  return 0;
  /* 
   * 
   */
L80:
  /*     CALL XERROR( ' DBINTK,  SOME ABSCISSA WAS NOT IN THE SUPPORT OF TH 
   *    1E CORRESPONDING BASIS FUNCTION AND THE SYSTEM IS SINGULAR.',109,2, 
   *    21) 
   */
  return 0;
L90:
  /*     CALL XERROR( ' DBINTK,  THE SYSTEM OF SOLVER DETECTS A SINGULAR SY 
   *    1STEM ALTHOUGH THE THEORETICAL CONDITIONS FOR A SOLUTION WERE SATIS 
   *    2FIED.',123,8,1) 
   */
  return 0;
L100:
  /*     CALL XERROR( ' DBINTK,  K DOES NOT SATISFY K.GE.1', 35, 2, 1) 
   */
  return 0;
L105:
  /*     CALL XERROR( ' DBINTK,  N DOES NOT SATISFY N.GE.K', 35, 2, 1) 
   */
  return 0;
L110:
  /*     CALL XERROR( ' DBINTK,  X(I) DOES NOT SATISFY X(I).LT.X(I+1) FOR S 
   *    1OME I', 57, 2, 1) 
   */
  return 0;
}				/* dbintk_ */

int
nsp_calpack_dbnfac (double *w, int *nroww, int *nrow, int *nbandl,
		    int *nbandu, int *iflag)
{
  /* System generated locals */
  int w_dim1, w_offset, i__1, i__2, i__3;

  /* Local variables */
  int jmax, kmax, i__, j, k, midmk;
  double pivot;
  int nrowm1, middle;
  double factor;
  int ipk;

  /****BEGIN PROLOGUE  DBNFAC 
   ****REFER TO  DBINT4,DBINTK 
   * 
   * DBNFAC is the BANFAC routine from 
   *       * A Practical Guide to Splines *  by C. de Boor 
   * 
   * DBNFAC is a double precision routine 
   * 
   * Returns in  W  the LU-factorization (without pivoting) of the banded 
   * matrix  A  of order  NROW  with  (NBANDL + 1 + NBANDU) bands or diag- 
   * onals in the work array  W . 
   * 
   ******  I N P U T  ****** W is double precision 
   * W.....Work array of size  (NROWW,NROW)  containing the interesting 
   *       part of a banded matrix  A , with the diagonals or bands of  A 
   *       stored in the rows of  W , while columns of  A  correspond to 
   *       columns of  W . This is the storage mode used in  LINPACK  and 
   *       results in efficient innermost loops. 
   *          Explicitly,  A  has  NBANDL  bands below the diagonal 
   *                           +     1     (main) diagonal 
   *                           +   NBANDU  bands above the diagonal 
   *       and thus, with    MIDDLE = NBANDU + 1, 
   *         A(I+J,J)  is in  W(I+MIDDLE,J)  for I=-NBANDU,...,NBANDL 
   *                                             J=1,...,NROW . 
   *       For example, the interesting entries of A (1,2)-banded matrix 
   *       of order  9  would appear in the first  1+1+2 = 4  rows of  W 
   *       as follows. 
   *                         13 24 35 46 57 68 79 
   *                      12 23 34 45 56 67 78 89 
   *                   11 22 33 44 55 66 77 88 99 
   *                   21 32 43 54 65 76 87 98 
   * 
   *       All other entries of  W  not identified in this way with an en- 
   *       try of  A  are never referenced . 
   * NROWW.....Row dimension of the work array  W . 
   *       must be  .GE.  NBANDL + 1 + NBANDU  . 
   * NBANDL.....Number of bands of  A  below the main diagonal 
   * NBANDU.....Number of bands of  A  above the main diagonal . 
   * 
   ******  O U T P U T  ****** W is double precision 
   * IFLAG.....Int indicating success( = 1) or failure ( = 2) . 
   *    If  IFLAG = 1, then 
   * W.....contains the LU-factorization of  A  into a unit lower triangu- 
   *       lar matrix  L  and an upper triangular matrix  U (both banded) 
   *       and stored in customary fashion over the corresponding entries 
   *       of  A . This makes it possible to solve any particular linear 
   *       system  A*X = B  for  X  by a 
   *             CALL DBNSLV ( W, NROWW, NROW, NBANDL, NBANDU, B ) 
   *       with the solution X  contained in  B  on return . 
   *    If  IFLAG = 2, then 
   *       one of  NROW-1, NBANDL,NBANDU failed to be nonnegative, or else 
   *       one of the potential pivots was found to be zero indicating 
   *       that  A  does not have an LU-factorization. This implies that 
   *       A  is singular in case it is totally positive . 
   * 
   ******  M E T H O D  ****** 
   *    Gauss elimination  W I T H O U T  pivoting is used. The routine is 
   * intended for use with matrices  A  which do not require row inter- 
   * changes during factorization, especially for the  T O T A L L Y 
   * P O S I T I V E  matrices which occur in spline calculations. 
   *    The routine should NOT be used for an arbitrary banded matrix. 
   ****ROUTINES CALLED  (NONE) 
   ****END PROLOGUE  DBNFAC 
   * 
   * 
   ****FIRST EXECUTABLE STATEMENT  DBNFAC 
   */
  /* Parameter adjustments */
  w_dim1 = *nroww;
  w_offset = w_dim1 + 1;
  w -= w_offset;

  /* Function Body */
  *iflag = 1;
  middle = *nbandu + 1;
  /*                        W(MIDDLE,.) CONTAINS THE MAIN DIAGONAL OF  A . 
   */
  nrowm1 = *nrow - 1;
  if (nrowm1 < 0)
    {
      goto L120;
    }
  else if (nrowm1 == 0)
    {
      goto L110;
    }
  else
    {
      goto L10;
    }
L10:
  if (*nbandl > 0)
    {
      goto L30;
    }
  /*               A IS UPPER TRIANGULAR. CHECK THAT DIAGONAL IS NONZERO . 
   */
  i__1 = nrowm1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (w[middle + i__ * w_dim1] == 0.)
	{
	  goto L120;
	}
      /* L20: */
    }
  goto L110;
L30:
  if (*nbandu > 0)
    {
      goto L60;
    }
  /*             A IS LOWER TRIANGULAR. CHECK THAT DIAGONAL IS NONZERO AND 
   *                DIVIDE EACH COLUMN BY ITS DIAGONAL . 
   */
  i__1 = nrowm1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      pivot = w[middle + i__ * w_dim1];
      if (pivot == 0.)
	{
	  goto L120;
	}
      /*Computing MIN 
       */
      i__2 = *nbandl, i__3 = *nrow - i__;
      jmax = Min (i__2, i__3);
      i__2 = jmax;
      for (j = 1; j <= i__2; ++j)
	{
	  w[middle + j + i__ * w_dim1] /= pivot;
	  /* L40: */
	}
      /* L50: */
    }
  return 0;
  /* 
   *       A  IS NOT JUST A TRIANGULAR MATRIX. CONSTRUCT LU FACTORIZATION 
   */
L60:
  i__1 = nrowm1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /*                                 W(MIDDLE,I)  IS PIVOT FOR I-TH STEP . 
       */
      pivot = w[middle + i__ * w_dim1];
      if (pivot == 0.)
	{
	  goto L120;
	}
      /*                JMAX  IS THE NUMBER OF (NONZERO) ENTRIES IN COLUMN  I 
       *                    BELOW THE DIAGONAL . 
       *Computing MIN 
       */
      i__2 = *nbandl, i__3 = *nrow - i__;
      jmax = Min (i__2, i__3);
      /*             DIVIDE EACH ENTRY IN COLUMN  I  BELOW DIAGONAL BY PIVOT . 
       */
      i__2 = jmax;
      for (j = 1; j <= i__2; ++j)
	{
	  w[middle + j + i__ * w_dim1] /= pivot;
	  /* L70: */
	}
      /*                KMAX  IS THE NUMBER OF (NONZERO) ENTRIES IN ROW  I  TO 
       *                    THE RIGHT OF THE DIAGONAL . 
       *Computing MIN 
       */
      i__2 = *nbandu, i__3 = *nrow - i__;
      kmax = Min (i__2, i__3);
      /*                 SUBTRACT  A(I,I+K)*(I-TH COLUMN) FROM (I+K)-TH COLUMN 
       *                 (BELOW ROW  I ) . 
       */
      i__2 = kmax;
      for (k = 1; k <= i__2; ++k)
	{
	  ipk = i__ + k;
	  midmk = middle - k;
	  factor = w[midmk + ipk * w_dim1];
	  i__3 = jmax;
	  for (j = 1; j <= i__3; ++j)
	    {
	      w[midmk + j + ipk * w_dim1] -=
		w[middle + j + i__ * w_dim1] * factor;
	      /* L80: */
	    }
	  /* L90: */
	}
      /* L100: */
    }
  /*                                      CHECK THE LAST DIAGONAL ENTRY . 
   */
L110:
  if (w[middle + *nrow * w_dim1] != 0.)
    {
      return 0;
    }
L120:
  *iflag = 2;
  return 0;
}				/* dbnfac_ */

int
nsp_calpack_dbnslv (double *w, int *nroww, int *nrow, int *nbandl,
		    int *nbandu, double *b)
{
  /* System generated locals */
  int w_dim1, w_offset, i__1, i__2, i__3;

  /* Local variables */
  int jmax, i__, j, nrowm1, middle;

  /****BEGIN PROLOGUE  DBNSLV 
   ****REFER TO  DBINT4,DBINTK 
   * 
   * DBNSLV is the BANSLV routine from 
   *       * A Practical Guide to Splines *  by C. de Boor 
   * 
   * DBNSLV is a double precision routine 
   * 
   * Companion routine to  DBNFAC . It returns the solution  X  of the 
   * linear system  A*X = B  in place of  B , given the LU-factorization 
   * for  A  in the work array  W from DBNFAC. 
   * 
   ******  I N P U T  ****** W,B are DOUBLE PRECISION 
   * W, NROWW,NROW,NBANDL,NBANDU.....Describe the LU-factorization of a 
   *       banded matrix  A  of order  NROW  as constructed in  DBNFAC . 
   *       For details, see  DBNFAC . 
   * B.....Right side of the system to be solved . 
   * 
   ******  O U T P U T  ****** B is DOUBLE PRECISION 
   * B.....Contains the solution  X , of order  NROW . 
   * 
   ******  M E T H O D  ****** 
   *    (With  A = L*U, as stored in  W,) the unit lower triangular system 
   * L(U*X) = B  is solved for  Y = U*X, and  Y  stored in  B . Then the 
   * upper triangular system  U*X = Y  is solved for  X  . The calcul- 
   * ations are so arranged that the innermost loops stay within columns. 
   ****ROUTINES CALLED  (NONE) 
   ****END PROLOGUE  DBNSLV 
   * 
   ****FIRST EXECUTABLE STATEMENT  DBNSLV 
   */
  /* Parameter adjustments */
  --b;
  w_dim1 = *nroww;
  w_offset = w_dim1 + 1;
  w -= w_offset;

  /* Function Body */
  middle = *nbandu + 1;
  if (*nrow == 1)
    {
      goto L80;
    }
  nrowm1 = *nrow - 1;
  if (*nbandl == 0)
    {
      goto L30;
    }
  /*                                FORWARD PASS 
   *           FOR I=1,2,...,NROW-1, SUBTRACT  RIGHT SIDE(I)*(I-TH COLUMN 
   *           OF  L )  FROM RIGHT SIDE  (BELOW I-TH ROW) . 
   */
  i__1 = nrowm1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /*Computing MIN 
       */
      i__2 = *nbandl, i__3 = *nrow - i__;
      jmax = Min (i__2, i__3);
      i__2 = jmax;
      for (j = 1; j <= i__2; ++j)
	{
	  b[i__ + j] -= b[i__] * w[middle + j + i__ * w_dim1];
	  /* L10: */
	}
      /* L20: */
    }
  /*                                BACKWARD PASS 
   *           FOR I=NROW,NROW-1,...,1, DIVIDE RIGHT SIDE(I) BY I-TH DIAG- 
   *           ONAL ENTRY OF  U, THEN SUBTRACT  RIGHT SIDE(I)*(I-TH COLUMN 
   *           OF  U)  FROM RIGHT SIDE  (ABOVE I-TH ROW). 
   */
L30:
  if (*nbandu > 0)
    {
      goto L50;
    }
  /*                               A  IS LOWER TRIANGULAR . 
   */
  i__1 = *nrow;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      b[i__] /= w[i__ * w_dim1 + 1];
      /* L40: */
    }
  return 0;
L50:
  i__ = *nrow;
L60:
  b[i__] /= w[middle + i__ * w_dim1];
  /*Computing MIN 
   */
  i__1 = *nbandu, i__2 = i__ - 1;
  jmax = Min (i__1, i__2);
  i__1 = jmax;
  for (j = 1; j <= i__1; ++j)
    {
      b[i__ - j] -= b[i__] * w[middle - j + i__ * w_dim1];
      /* L70: */
    }
  --i__;
  if (i__ > 1)
    {
      goto L60;
    }
L80:
  b[1] /= w[middle + w_dim1];
  return 0;
}				/* dbnslv_ */

int
nsp_calpack_dbspvn (double *t, int *jhigh, int *k, int *index, double *x,
		    int *ileft, double *vnikx, double *work, int *iwork)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int imjp1, jp1ml, l;
  double vm;
  int jp1;
  double vmprev;
  int ipj;

  /****BEGIN PROLOGUE  DBSPVN 
   ****DATE WRITTEN   800901   (YYMMDD) 
   ****REVISION DATE  820801   (YYMMDD) 
   ****REVISION HISTORY  (YYMMDD) 
   *  000330  Modified array declarations.  (JEC) 
   * 
   ****CATEGORY NO.  E3,K6 
   ****KEYWORDS  B-SPLINE,DATA FITTING,DOUBLE PRECISION,INTERPOLATION, 
   *            SPLINE 
   ****AUTHOR  AMOS, D. E., (SNLA) 
   ****PURPOSE  Calculates the value of all (possibly) nonzero basis 
   *           functions at X. 
   ****DESCRIPTION 
   * 
   *    Written by Carl de Boor and modified by D. E. Amos 
   * 
   *    Reference 
   *        SIAM J. Numerical Analysis, 14, No. 3, June, 1977, pp.441-472. 
   * 
   *    Abstract    **** a double precision routine **** 
   *        DBSPVN is the BSPLVN routine of the reference. 
   * 
   *        DBSPVN calculates the value of all (possibly) nonzero basis 
   *        functions at X of order MAX(JHIGH,(J+1)*(INDEX-1)), where T(K) 
   *        .LE. X .LE. T(N+1) and J=IWORK is set inside the routine on 
   *        the first call when INDEX=1.  ILEFT is such that T(ILEFT) .LE. 
   *        X .LT. T(ILEFT+1).  A call to DINTRV(T,N+1,X,ILO,ILEFT,MFLAG) 
   *        produces the proper ILEFT.  DBSPVN calculates using the basic 
   *        algorithm needed in DBSPVD.  If only basis functions are 
   *        desired, setting JHIGH=K and INDEX=1 can be faster than 
   *        calling DBSPVD, but extra coding is required for derivatives 
   *        (INDEX=2) and DBSPVD is set up for this purpose. 
   * 
   *        Left limiting values are set up as described in DBSPVD. 
   * 
   *    Description of Arguments 
   * 
   *        Input      T,X are double precision 
   *         T       - knot vector of length N+K, where 
   *                   N = number of B-spline basis functions 
   *                   N = sum of knot multiplicities-K 
   *         JHIGH   - order of B-spline, 1 .LE. JHIGH .LE. K 
   *         K       - highest possible order 
   *         INDEX   - INDEX = 1 gives basis functions of order JHIGH 
   *                         = 2 denotes previous entry with work, IWORK 
   *                             values saved for subsequent calls to 
   *                             DBSPVN. 
   *         X       - argument of basis functions, 
   *                   T(K) .LE. X .LE. T(N+1) 
   *         ILEFT   - largest int such that 
   *                   T(ILEFT) .LE. X .LT.  T(ILEFT+1) 
   * 
   *        Output     VNIKX, WORK are double precision 
   *         VNIKX   - vector of length K for spline values. 
   *         WORK    - a work vector of length 2*K 
   *         IWORK   - a work parameter.  Both WORK and IWORK contain 
   *                   information necessary to continue for INDEX = 2. 
   *                   When INDEX = 1 exclusively, these are scratch 
   *                   variables and can be used for other purposes. 
   * 
   *    Error Conditions 
   *        Improper input is a fatal error. 
   ****REFERENCES  C. DE BOOR, *PACKAGE FOR CALCULATING WITH B-SPLINES*, 
   *                SIAM JOURNAL ON NUMERICAL ANALYSIS, VOLUME 14, NO. 3, 
   *                JUNE 1977, PP. 441-472. 
   ****ROUTINES CALLED  XERROR 
   ****END PROLOGUE  DBSPVN 
   * 
   * 
   *    DIMENSION T(ILEFT+JHIGH) 
   *    CONTENT OF J, DELTAM, DELTAP IS EXPECTED UNCHANGED BETWEEN CALLS. 
   *    WORK(I) = DELTAP(I), WORK(K+I) = DELTAM(I), I = 1,K 
   ****FIRST EXECUTABLE STATEMENT  DBSPVN 
   */
  /* Parameter adjustments */
  --t;
  --vnikx;
  --work;

  /* Function Body */
  if (*k < 1)
    {
      goto L90;
    }
  if (*jhigh > *k || *jhigh < 1)
    {
      goto L100;
    }
  if (*index < 1 || *index > 2)
    {
      goto L105;
    }
  if (*x < t[*ileft] || *x > t[*ileft + 1])
    {
      goto L110;
    }
  switch (*index)
    {
    case 1:
      goto L10;
    case 2:
      goto L20;
    }
L10:
  *iwork = 1;
  vnikx[1] = 1.;
  if (*iwork >= *jhigh)
    {
      goto L40;
    }
  /* 
   */
L20:
  ipj = *ileft + *iwork;
  work[*iwork] = t[ipj] - *x;
  imjp1 = *ileft - *iwork + 1;
  work[*k + *iwork] = *x - t[imjp1];
  vmprev = 0.;
  jp1 = *iwork + 1;
  i__1 = *iwork;
  for (l = 1; l <= i__1; ++l)
    {
      jp1ml = jp1 - l;
      vm = vnikx[l] / (work[l] + work[*k + jp1ml]);
      vnikx[l] = vm * work[l] + vmprev;
      vmprev = vm * work[*k + jp1ml];
      /* L30: */
    }
  vnikx[jp1] = vmprev;
  *iwork = jp1;
  if (*iwork < *jhigh)
    {
      goto L20;
    }
  /* 
   */
L40:
  return 0;
  /* 
   * 
   */
L90:
  /*     CALL XERROR( ' DBSPVN,  K DOES NOT SATISFY K.GE.1', 35, 2, 1) 
   */
  return 0;
L100:
  /*     CALL XERROR( ' DBSPVN,  JHIGH DOES NOT SATISFY 1.LE.JHIGH.LE.K', 
   *    1 48, 2, 1) 
   */
  return 0;
L105:
  /*     CALL XERROR( ' DBSPVN,  INDEX IS NOT 1 OR 2',29,2,1) 
   */
  return 0;
L110:
  /*     CALL XERROR( ' DBSPVN,  X DOES NOT SATISFY T(ILEFT).LE.X.LE.T(ILEF 
   *    1T+1)', 56, 2, 1) 
   */
  return 0;
}				/* dbspvn_ */

double
nsp_calpack_db3val (double *xval, double *yval, double *zval, int *idx,
		    int *idy, int *idz, double *tx, double *ty, double *tz,
		    int *nx, int *ny, int *nz, int *kx, int *ky, int *kz,
		    double *bcoef, double *work)
{
  /* Initialized data */

  static int iloy = 1;
  static int iloz = 1;
  static int inbvx = 1;

  /* System generated locals */
  int bcoef_dim1, bcoef_dim2, bcoef_offset, i__1, i__2;
  double ret_val;

  /* Local variables */
  int inbv1, inbv2, i__, j, k, mflag, kcoly, kcolz, lefty, leftz, iw, iz;
  int izm1;

  /****BEGIN PROLOGUE  DB3VAL 
   ****DATE WRITTEN   25 MAY 1982 
   ****REVISION DATE  25 MAY 1982 
   ****REVISION HISTORY  (YYMMDD) 
   *  R.F. BOISVERT, NIST 
   *  22 FEB 00 
   * 
   *<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> 
   * ------------ 
   * DECLARATIONS 
   * ------------ 
   * 
   * PARAMETERS 
   * 
   * 
   * LOCAL VARIABLES 
   * 
   * 
   */
  /* Parameter adjustments */
  --tx;
  --ty;
  --tz;
  bcoef_dim1 = *nx;
  bcoef_dim2 = *ny;
  bcoef_offset = bcoef_dim1 * (bcoef_dim2 + 1) + 1;
  bcoef -= bcoef_offset;
  --work;

  /* Function Body */
  /*    SAVE ILOY    ,  ILOZ    ,  INBVX 
   * 
   * 
   ****FIRST EXECUTABLE STATEMENT 
   */
  ret_val = 0.;
  /* NEXT STATEMENT - RFB MOD 
   */
  if (*xval < tx[1] || *xval > tx[*nx + *kx] || *yval < ty[1]
      || *yval > ty[*ny + *ky] || *zval < tz[1] || *zval > tz[*nz + *kz])
    {
      goto L100;
    }
  i__1 = *ny + *ky;
  nsp_calpack_dintrv (&ty[1], &i__1, yval, &iloy, &lefty, &mflag);
  if (mflag != 0)
    {
      goto L100;
    }
  i__1 = *nz + *kz;
  nsp_calpack_dintrv (&tz[1], &i__1, zval, &iloz, &leftz, &mflag);
  if (mflag != 0)
    {
      goto L100;
    }
  iz = *ky * *kz + 1;
  iw = iz + *kz;
  kcolz = leftz - *kz;
  i__ = 0;
  i__1 = *kz;
  for (k = 1; k <= i__1; ++k)
    {
      ++kcolz;
      kcoly = lefty - *ky;
      i__2 = *ky;
      for (j = 1; j <= i__2; ++j)
	{
	  ++i__;
	  ++kcoly;
	  work[i__] =
	    nsp_calpack_dbvalu (&tx[1],
				&bcoef[(kcoly +
					kcolz * bcoef_dim2) * bcoef_dim1 +
				       1], nx, kx, idx, xval, &inbvx,
				&work[iw]);
	  /* L50: */
	}
    }
  inbv1 = 1;
  izm1 = iz - 1;
  kcoly = lefty - *ky + 1;
  i__2 = *kz;
  for (k = 1; k <= i__2; ++k)
    {
      i__ = (k - 1) * *ky + 1;
      j = izm1 + k;
      work[j] =
	nsp_calpack_dbvalu (&ty[kcoly], &work[i__], ky, ky, idy, yval,
			    &inbv1, &work[iw]);
      /* L60: */
    }
  inbv2 = 1;
  kcolz = leftz - *kz + 1;
  ret_val =
    nsp_calpack_dbvalu (&tz[kcolz], &work[iz], kz, kz, idz, zval, &inbv2,
			&work[iw]);
L100:
  return ret_val;
}				/* db3val_ */

int
nsp_calpack_db3ink (double *x, int *nx, double *y, int *ny, double *z__,
		    int *nz, double *fcn, int *ldf1, int *ldf2, int *kx,
		    int *ky, int *kz, double *tx, double *ty, double *tz,
		    double *bcoef, double *work, int *iflag)
{
  /* System generated locals */
  int fcn_dim1, fcn_dim2, fcn_offset, bcoef_dim1, bcoef_dim2, bcoef_offset,
    i__1, i__2, i__3;

  /* Local variables */
  int i__, j, k, iw;
  int loc, npk;

  /****BEGIN PROLOGUE  DB3INK 
   ****DATE WRITTEN   25 MAY 1982 
   *          The actual first dimension of FCN used in the 
   *          calling program. 
   * 
   *  LDF2    Int scalar (.GE. NY) 
   *          The actual second dimension of FCN used in the calling 
   *          program. 
   * 
   *  KX      Int scalar (.GE. 2, .LT. NX) 
   *          The order of spline pieces in x. 
   *          (Order = polynomial degree + 1) 
   * 
   *  KY      Int scalar (.GE. 2, .LT. NY) 
   *          The order of spline pieces in y. 
   *          (Order = polynomial degree + 1) 
   * 
   *  KZ      Int scalar (.GE. 2, .LT. NZ) 
   *          The order of spline pieces in z. 
   *          (Order = polynomial degree + 1) 
   * 
   * 
   *  I N P U T   O R   O U T P U T 
   *  ----------------------------- 
   * 
   *  TX      Double precision 1D array (size NX+KX) 
   *          The knots in the x direction for the spline interpolant. 
   *          If IFLAG=0 these are chosen by DB3INK. 
   *          If IFLAG=1 these are specified by the user. 
   *                     (Must be non-decreasing.) 
   * 
   *  TY      Double precision 1D array (size NY+KY) 
   *          The knots in the y direction for the spline interpolant. 
   *          If IFLAG=0 these are chosen by DB3INK. 
   *          If IFLAG=1 these are specified by the user. 
   *                     (Must be non-decreasing.) 
   * 
   *  TZ      Double precision 1D array (size NZ+KZ) 
   *          The knots in the z direction for the spline interpolant. 
   *          If IFLAG=0 these are chosen by DB3INK. 
   *          If IFLAG=1 these are specified by the user. 
   *                     (Must be non-decreasing.) 
   * 
   * 
   *  O U T P U T 
   *  ----------- 
   * 
   *  BCOEF   Double precision 3D array (size NX by NY by NZ) 
   *          Array of coefficients of the B-spline interpolant. 
   *          This may be the same array as FCN. 
   * 
   * 
   *  M I S C E L L A N E O U S 
   *  ------------------------- 
   * 
   *  WORK    Double precision 1D array (size NX*NY*NZ + Max( 2*KX*(NX+1), 
   *                            2*KY*(NY+1), 2*KZ*(NZ+1) ) 
   *          Array of working storage. 
   * 
   *  IFLAG   Int scalar. 
   *          On input:  0 == knot sequence chosen by B2INK 
   *                     1 == knot sequence chosen by user. 
   *          On output: 1 == successful execution 
   *                     2 == IFLAG out of range 
   *                     3 == NX out of range 
   *                     4 == KX out of range 
   *                     5 == X not strictly increasing 
   *                     6 == TX not non-decreasing 
   *                     7 == NY out of range 
   *                     8 == KY out of range 
   *                     9 == Y not strictly increasing 
   *                    10 == TY not non-decreasing 
   *                    11 == NZ out of range 
   *                    12 == KZ out of range 
   *                    13 == Z not strictly increasing 
   *                    14 == TY not non-decreasing 
   * 
   ****REFERENCES  CARL DE BOOR, A PRACTICAL GUIDE TO SPLINES, 
   *                SPRINGER-VERLAG, NEW YORK, 1978. 
   *              CARL DE BOOR, EFFICIENT COMPUTER MANIPULATION OF TENSOR 
   *                PRODUCTS, ACM TRANSACTIONS ON MATHEMATICAL SOFTWARE, 
   *                VOL. 5 (1979), PP. 173-182. 
   ****ROUTINES CALLED  DBTPCF,DBKNOT 
   ****END PROLOGUE  DB3INK 
   * 
   * ------------ 
   * DECLARATIONS 
   * ------------ 
   * 
   * PARAMETERS 
   * 
   * 
   * LOCAL VARIABLES 
   * 
   * 
   * ----------------------- 
   * CHECK VALIDITY OF INPUT 
   * ----------------------- 
   * 
   ****FIRST EXECUTABLE STATEMENT 
   */
  /* Parameter adjustments */
  --x;
  --y;
  bcoef_dim1 = *nx;
  bcoef_dim2 = *ny;
  bcoef_offset = bcoef_dim1 * (bcoef_dim2 + 1) + 1;
  bcoef -= bcoef_offset;
  --z__;
  fcn_dim1 = *ldf1;
  fcn_dim2 = *ldf2;
  fcn_offset = fcn_dim1 * (fcn_dim2 + 1) + 1;
  fcn -= fcn_offset;
  --tx;
  --ty;
  --tz;
  --work;

  /* Function Body */
  if (*iflag < 0 || *iflag > 1)
    {
      goto L920;
    }
  if (*nx < 3)
    {
      goto L930;
    }
  if (*ny < 3)
    {
      goto L970;
    }
  if (*nz < 3)
    {
      goto L1010;
    }
  if (*kx < 2 || *kx >= *nx)
    {
      goto L940;
    }
  if (*ky < 2 || *ky >= *ny)
    {
      goto L980;
    }
  if (*kz < 2 || *kz >= *nz)
    {
      goto L1020;
    }
  i__1 = *nx;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (x[i__] <= x[i__ - 1])
	{
	  goto L950;
	}
      /* L10: */
    }
  i__1 = *ny;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (y[i__] <= y[i__ - 1])
	{
	  goto L990;
	}
      /* L20: */
    }
  i__1 = *nz;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (z__[i__] <= z__[i__ - 1])
	{
	  goto L1030;
	}
      /* L30: */
    }
  if (*iflag == 0)
    {
      goto L70;
    }
  npk = *nx + *kx;
  i__1 = npk;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (tx[i__] < tx[i__ - 1])
	{
	  goto L960;
	}
      /* L40: */
    }
  npk = *ny + *ky;
  i__1 = npk;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (ty[i__] < ty[i__ - 1])
	{
	  goto L1000;
	}
      /* L50: */
    }
  npk = *nz + *kz;
  i__1 = npk;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      if (tz[i__] < tz[i__ - 1])
	{
	  goto L1040;
	}
      /* L60: */
    }
L70:
  /* 
   * ------------ 
   * CHOOSE KNOTS 
   * ------------ 
   * 
   */
  if (*iflag != 0)
    {
      goto L100;
    }
  nsp_calpack_dbknot (&x[1], nx, kx, &tx[1]);
  nsp_calpack_dbknot (&y[1], ny, ky, &ty[1]);
  nsp_calpack_dbknot (&z__[1], nz, kz, &tz[1]);
L100:
  /* 
   * ------------------------------- 
   * CONSTRUCT B-SPLINE COEFFICIENTS 
   * ------------------------------- 
   * 
   */
  *iflag = 1;
  iw = *nx * *ny * *nz + 1;
  /* 
   *    COPY FCN TO WORK IN PACKED FOR DBTPCF 
   */
  loc = 0;
  i__1 = *nz;
  for (k = 1; k <= i__1; ++k)
    {
      i__2 = *ny;
      for (j = 1; j <= i__2; ++j)
	{
	  i__3 = *nx;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      ++loc;
	      work[loc] = fcn[i__ + (j + k * fcn_dim2) * fcn_dim1];
	      /* L510: */
	    }
	}
    }
  /* 
   */
  i__3 = *ny * *nz;
  nsp_calpack_dbtpcf (&x[1], nx, &work[1], nx, &i__3, &tx[1], kx,
		      &bcoef[bcoef_offset], &work[iw]);
  i__3 = *nx * *nz;
  nsp_calpack_dbtpcf (&y[1], ny, &bcoef[bcoef_offset], ny, &i__3, &ty[1], ky,
		      &work[1], &work[iw]);
  i__3 = *nx * *ny;
  nsp_calpack_dbtpcf (&z__[1], nz, &work[1], nz, &i__3, &tz[1], kz,
		      &bcoef[bcoef_offset], &work[iw]);
  goto L9999;
  /* 
   * ----- 
   * EXITS 
   * ----- 
   * 
   */
L920:
  /*     CALL XERRWV('DB3INK -  IFLAG=I1 IS OUT OF RANGE.', 
   *    *            35,2,1,1,IFLAG,I2,0,R1,R2) 
   */
  *iflag = 2;
  goto L9999;
  /* 
   */
L930:
  *iflag = 3;
  /*     CALL XERRWV('DB3INK -  NX=I1 IS OUT OF RANGE.', 
   *    *            32,IFLAG,1,1,NX,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L940:
  *iflag = 4;
  /*     CALL XERRWV('DB3INK -  KX=I1 IS OUT OF RANGE.', 
   *    *            32,IFLAG,1,1,KX,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L950:
  *iflag = 5;
  /*     CALL XERRWV('DB3INK -  X ARRAY MUST BE STRICTLY INCREASING.', 
   *    *            46,IFLAG,1,0,I1,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L960:
  *iflag = 6;
  /*     CALL XERRWV('DB3INK -  TX ARRAY MUST BE NON-DECREASING.', 
   *    *            42,IFLAG,1,0,I1,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L970:
  *iflag = 7;
  /*     CALL XERRWV('DB3INK -  NY=I1 IS OUT OF RANGE.', 
   *    *            32,IFLAG,1,1,NY,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L980:
  *iflag = 8;
  /*     CALL XERRWV('DB3INK -  KY=I1 IS OUT OF RANGE.', 
   *    *            32,IFLAG,1,1,KY,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L990:
  *iflag = 9;
  /*     CALL XERRWV('DB3INK -  Y ARRAY MUST BE STRICTLY INCREASING.', 
   *    *            46,IFLAG,1,0,I1,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L1000:
  *iflag = 10;
  /*     CALL XERRWV('DB3INK -  TY ARRAY MUST BE NON-DECREASING.', 
   *    *            42,IFLAG,1,0,I1,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L1010:
  *iflag = 11;
  /*     CALL XERRWV('DB3INK -  NZ=I1 IS OUT OF RANGE.', 
   *    *            32,IFLAG,1,1,NZ,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L1020:
  *iflag = 12;
  /*     CALL XERRWV('DB3INK -  KZ=I1 IS OUT OF RANGE.', 
   *    *            32,IFLAG,1,1,KZ,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L1030:
  *iflag = 13;
  /*     CALL XERRWV('DB3INK -  Z ARRAY MUST BE STRICTLY INCREASING.', 
   *    *            46,IFLAG,1,0,I1,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L1040:
  *iflag = 14;
  /*     CALL XERRWV('DB3INK -  TZ ARRAY MUST BE NON-DECREASING.', 
   *    *            42,IFLAG,1,0,I1,I2,0,R1,R2) 
   */
  goto L9999;
  /* 
   */
L9999:
  return 0;
}				/* db3ink_ */
#endif 
