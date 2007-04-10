#include "cdf.h"

static int
dinvr_cdf_0 (int n__, int *status, double *x, double *fx, int *qleft,
	     int *qhi,const double *zsmall,const double *zbig,const double *zabsst,
	     const double *zrelst,const double *zstpmu,const double *zabsto,const double *zrelto);


/* ********************************************************************** */
/*      SUBROUTINE DSTINV( SMALL, BIG, ABSSTP, RELSTP, STPMUL, */
/*     +                   ABSTOL, RELTOL ) */
/*      Double Precision - SeT INverse finder - Reverse Communication */
/*                              Function */
/*     Concise Description - Given a monotone function F finds X */
/*     such that F(X) = Y.  Uses Reverse communication -- see invr. */
/*     This routine sets quantities needed by INVR. */
/*          More Precise Description of INVR - */
/*     F must be a monotone function, the results of QMFINV are */
/*     otherwise undefined.  QINCR must be .TRUE. if F is non- */
/*     decreasing and .FALSE. if F is non-increasing. */
/*     QMFINV will return .TRUE. if and only if F(SMALL) and */
/*     F(BIG) bracket Y, i. e., */
/*          QINCR is .TRUE. and F(SMALL).LE.Y.LE.F(BIG) or */
/*          QINCR is .FALSE. and F(BIG).LE.Y.LE.F(SMALL) */
/*     if QMFINV returns .TRUE., then the X returned satisfies */
/*     the following condition.  let */
/*               TOL(X) = MAX(ABSTOL,RELTOL*ABS(X)) */
/*     then if QINCR is .TRUE., */
/*          F(X-TOL(X)) .LE. Y .LE. F(X+TOL(X)) */
/*     and if QINCR is .FALSE. */
/*          F(X-TOL(X)) .GE. Y .GE. F(X+TOL(X)) */
/*                              Arguments */
/*     SMALL --> The left endpoint of the interval to be */
/*          searched for a solution. */
/*                    SMALL is DOUBLE PRECISION */
/*     BIG --> The right endpoint of the interval to be */
/*          searched for a solution. */
/*                    BIG is DOUBLE PRECISION */
/*     ABSSTP, RELSTP --> The initial step size in the search */
/*          is MAX(ABSSTP,RELSTP*ABS(X)). See algorithm. */
/*                    ABSSTP is DOUBLE PRECISION */
/*                    RELSTP is DOUBLE PRECISION */
/*     STPMUL --> When a step doesn't bound the zero, the step */
/*                size is multiplied by STPMUL and another step */
/*                taken.  A popular value is 2.0 */
/*                    DOUBLE PRECISION STPMUL */
/*     ABSTOL, RELTOL --> Two numbers that determine the accuracy */
/*          of the solution.  See function for a precise definition. */
/*                    ABSTOL is DOUBLE PRECISION */
/*                    RELTOL is DOUBLE PRECISION */
/*                              Method */
/*     Compares F(X) with Y for the input value of X then uses QINCR */
/*     to determine whether to step left or right to bound the */
/*     desired x.  the initial step size is */
/*          MAX(ABSSTP,RELSTP*ABS(S)) for the input value of X. */
/*     Iteratively steps right or left until it bounds X. */
/*     At each step which doesn't bound X, the step size is doubled. */
/*     The routine is careful never to step beyond SMALL or BIG.  If */
/*     it hasn't bounded X at SMALL or BIG, QMFINV returns .FALSE. */
/*     after setting QLEFT and QHI. */
/*     If X is successfully bounded then Algorithm R of the paper */
/*     'Two Efficient Algorithms with Guaranteed Convergence for */
/*     Finding a Zero of a Function' by J. C. P. Bus and */
/*     T. J. Dekker in ACM Transactions on Mathematical */
/*     Software, Volume 1, No. 4 page 330 (DEC. '75) is employed */
/*     to find the zero of the function F(X)-Y. This is routine */
/*     QRZERO. */
/* ********************************************************************** */


/* Subroutine */ int
cdf_dinvr (int *status, double *x, double *fx, int *qleft, int *qhi)
{
  return dinvr_cdf_0 (0, status, x, fx, qleft, qhi, (double *) 0,
		      (double *) 0, (double *) 0, (double *) 0, (double *) 0,
		      (double *) 0, (double *) 0);
}

/* Subroutine */ int
cdf_dstinv (const double *zsmall,const double *zbig,const double *zabsst,const double *zrelst,
	    const double *zstpmu,const double *zabsto,const  double *zrelto)
{
  return dinvr_cdf_0 (1, (int *) 0, (double *) 0, (double *) 0, (int *) 0,
		      (int *) 0, zsmall, zbig, zabsst, zrelst, zstpmu, zabsto,
		      zrelto);
}


static int
dinvr_cdf_0 (int n__, int *status, double *x, double *fx, int *qleft,
	     int *qhi,const double *zsmall,const double *zbig,const double *zabsst,
	     const double *zrelst,const double *zstpmu,const double *zabsto,const double *zrelto)
{
  double d__1, d__2;
  static double fbig;
  static int qbdd, qlim;
  static double step;
  static int qdum1, qdum2, qcond;
  static double small;
  static int qincr;
  static double xsave;
  static double fsmall, yy, abstol, absstp;
  static double reltol, relstp, stpmul, big, xlb, xhi;
  static int qok;
  static double xub, xlo;
  static int qup;
  static int i99999;

/* ********************************************************************** */
/*     SUBROUTINE DINVR(STATUS, X, FX, QLEFT, QHI) */
/*          Double precision */
/*          bounds the zero of the function and invokes zror */
/*                    Reverse Communication */
/*                              Function */
/*     Bounds the    function  and  invokes  ZROR   to perform the   zero */
/*     finding.  STINVR  must  have   been  called  before this   routine */
/*     in order to set its parameters. */
/*                              Arguments */
/*     STATUS <--> At the beginning of a zero finding problem, STATUS */
/*                 should be set to 0 and INVR invoked.  (The value */
/*                 of parameters other than X will be ignored on this cal */
/*                 When INVR needs the function evaluated, it will set */
/*                 STATUS to 1 and return.  The value of the function */
/*                 should be set in FX and INVR again called without */
/*                 changing any of its other parameters. */
/*                 When INVR has finished without error, it will return */
/*                 with STATUS 0.  In that case X is approximately a root */
/*                 of F(X). */
/*                 If INVR cannot bound the function, it returns status */
/*                 -1 and sets QLEFT and QHI. */
/*                         INT STATUS */
/*     X <-- The value of X at which F(X) is to be evaluated. */
/*                         DOUBLE PRECISION X */
/*     FX --> The value of F(X) calculated when INVR returns with */
/*            STATUS = 1. */
/*                         DOUBLE PRECISION FX */
/*     QLEFT <-- Defined only if QMFINV returns .FALSE.  In that */
/*          case it is .TRUE. If the stepping search terminated */
/*          unsucessfully at SMALL.  If it is .FALSE. the search */
/*          terminated unsucessfully at BIG. */
/*                    QLEFT is INT */
/*     QHI <-- Defined only if QMFINV returns .FALSE.  In that */
/*          case it is .TRUE. if F(X) .GT. Y at the termination */
/*          of the search and .FALSE. if F(X) .LT. Y at the */
/*          termination of the search. */
/*                    QHI is INT */
/* ********************************************************************** */
/*     Modified by S. Steer INRIA 1998,to replace ASSIGN instruction by */
/*     Computed GOTO */
/* ********************************************************************** */

  switch (n__)
    {
    case 1:
      goto L_dstinv;
    }

  if (*status > 0)
    {
      goto L310;
    }
  qcond = !(small <= *x && *x <= big);
  if (qcond)
    {
      Scierror(" SMALL, X, BIG not monotone in INVR\n");
      *status = -100;
      return 0;
    }
  xsave = *x;

/*     See that SMALL and BIG bound the zero and set QINCR */

  *x = small;
/*     GET-FUNCTION-VALUE */
/*      ASSIGN 10 TO i99999 */
  i99999 = 1;
  goto L300;
L10:
  fsmall = *fx;
  *x = big;
/*     GET-FUNCTION-VALUE */
/*      ASSIGN 20 TO i99999 */
  i99999 = 2;
  goto L300;
L20:
  fbig = *fx;
  qincr = fbig > fsmall;
  if (!qincr)
    {
      goto L50;
    }
  if (fsmall <= 0.)
    {
      goto L30;
    }
  *status = -1;
  *qleft = TRUE;
  *qhi = TRUE;
  return 0;
L30:
  if (fbig >= 0.)
    {
      goto L40;
    }
  *status = -1;
  *qleft = FALSE;
  *qhi = FALSE;
  return 0;
L40:
  goto L80;
L50:
  if (fsmall >= 0.)
    {
      goto L60;
    }
  *status = -1;
  *qleft = TRUE;
  *qhi = FALSE;
  return 0;
L60:
  if (fbig <= 0.)
    {
      goto L70;
    }
  *status = -1;
  *qleft = FALSE;
  *qhi = TRUE;
  return 0;
L70:
L80:
  *x = xsave;
/* Computing MAX */
  d__1 = absstp, d__2 = relstp * Abs (*x);
  step = Max (d__1, d__2);
/*      YY = F(X) - Y */
/*     GET-FUNCTION-VALUE */
/*      ASSIGN 90 TO i99999 */
  i99999 = 3;
  goto L300;
L90:
  yy = *fx;
  if (!(yy == 0.))
    {
      goto L100;
    }
  *status = 0;
  qok = TRUE;
  return 0;
L100:
  qup = (qincr && yy < 0.) || (!qincr && yy > 0.);
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     HANDLE CASE IN WHICH WE MUST STEP HIGHER */

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  if (!qup)
    {
      goto L170;
    }
  xlb = xsave;
/* Computing MIN */
  d__1 = xlb + step;
  xub = Min (d__1, big);
  goto L120;
L110:
  if (qcond)
    {
      goto L150;
    }
/*      YY = F(XUB) - Y */
L120:
  *x = xub;
/*     GET-FUNCTION-VALUE */
/*      ASSIGN 130 TO i99999 */
  i99999 = 4;
  goto L300;
L130:
  yy = *fx;
  qbdd = (qincr && yy >= 0.) || (!qincr && yy <= 0.);
  qlim = xub >= big;
  qcond = qbdd || qlim;
  if (qcond)
    {
      goto L140;
    }
  step = stpmul * step;
  xlb = xub;
/* Computing MIN */
  d__1 = xlb + step;
  xub = Min (d__1, big);
L140:
  goto L110;
L150:
  if (!(qlim && !qbdd))
    {
      goto L160;
    }
  *status = -1;
  *qleft = FALSE;
  *qhi = !qincr;
  *x = big;
  return 0;
L160:
  goto L240;
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     HANDLE CASE IN WHICH WE MUST STEP LOWER */

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
L170:
  xub = xsave;
/* Computing MAX */
  d__1 = xub - step;
  xlb = Max (d__1, small);
  goto L190;
L180:
  if (qcond)
    {
      goto L220;
    }
/*      YY = F(XLB) - Y */
L190:
  *x = xlb;
/*     GET-FUNCTION-VALUE */
/*      ASSIGN 200 TO i99999 */
  i99999 = 5;
  goto L300;
L200:
  yy = *fx;
  qbdd = (qincr && yy <= 0.) || (!qincr && yy >= 0.);
  qlim = xlb <= small;
  qcond = qbdd || qlim;
  if (qcond)
    {
      goto L210;
    }
  step = stpmul * step;
  xub = xlb;
/* Computing MAX */
  d__1 = xub - step;
  xlb = Max (d__1, small);
L210:
  goto L180;
L220:
  if (!(qlim && !qbdd))
    {
      goto L230;
    }
  *status = -1;
  *qleft = TRUE;
  *qhi = qincr;
  *x = small;
  return 0;
L230:
L240:
  cdf_dstzr (&xlb, &xub, &abstol, &reltol);
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

/*     IF WE REACH HERE, XLB AND XUB BOUND THE ZERO OF F. */

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
  *status = 0;
  goto L260;
L250:
  if (!(*status == 1))
    {
      goto L290;
    }
L260:
  cdf_dzror (status, x, fx, &xlo, &xhi, &qdum1, &qdum2);
  if (!(*status == 1))
    {
      goto L280;
    }
/*     GET-FUNCTION-VALUE */
/*      ASSIGN 270 TO i99999 */
  i99999 = 6;
  goto L300;
L270:
L280:
  goto L250;
L290:
  *x = xlo;
  *status = 0;
  return 0;

L_dstinv:
  small = *zsmall;
  big = *zbig;
  absstp = *zabsst;
  relstp = *zrelst;
  stpmul = *zstpmu;
  abstol = *zabsto;
  reltol = *zrelto;
  return 0;
/* (jpc)      STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***' */
/*     TO GET-FUNCTION-VALUE */
L300:
  *status = 1;
  return 0;
L310:
  switch (i99999)
    {
    case 1:
      goto L10;
    case 2:
      goto L20;
    case 3:
      goto L90;
    case 4:
      goto L130;
    case 5:
      goto L200;
    case 6:
      goto L270;
    }
/*      GO TO i99999 */
  return 0;
}				/* dinvr_ */


