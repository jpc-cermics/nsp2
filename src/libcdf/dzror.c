#include "cdf.h"


/* ********************************************************************** */
/*     SUBROUTINE DZROR(STATUS, X, FX, XLO, XHI, QLEFT, QHI) */
/*     Double precision ZeRo of a function -- Reverse Communication */
/*                              Function */
/*     Performs the zero finding.  STZROR must have been called before */
/*     this routine in order to set its parameters. */
/*                              Arguments */
/*     STATUS <--> At the beginning of a zero finding problem, STATUS */
/*                 should be set to 0 and ZROR invoked.  (The value */
/*                 of other parameters will be ignored on this call.) */
/*                                                                     */
/*                 When ZROR needs the function evaluated, it will set */
/*                 STATUS to 1 and return.  The value of the function */
/*                 should be set in FX and ZROR again called without */
/*                 changing any of its other parameters. */
/*                                                                     */
/*                 When ZROR has finished without error, it will return */
/*                 with STATUS 0.  In that case (XLO,XHI) bound the answe */
/*                                                                     */
/*                 If ZROR finds an error (which implies that F(XLO)-Y an */
/*                 F(XHI)-Y have the same sign, it returns STATUS -1.  In */
/*                 this case, XLO and XHI are undefined. */
/*                         INT STATUS */
/*     X <-- The value of X at which F(X) is to be evaluated. */
/*                         DOUBLE PRECISION X */
/*     FX --> The value of F(X) calculated when ZROR returns with */
/*            STATUS = 1. */
/*                         DOUBLE PRECISION FX */
/*     XLO <-- When ZROR returns with STATUS = 0, XLO bounds the */
/*             inverval in X containing the solution below. */
/*                         DOUBLE PRECISION XLO */
/*     XHI <-- When ZROR returns with STATUS = 0, XHI bounds the */
/*             inverval in X containing the solution above. */
/*                         DOUBLE PRECISION XHI */
/*     QLEFT <-- .TRUE. if the stepping search terminated unsucessfully */
/*                at XLO.  If it is .FALSE. the search terminated */
/*                unsucessfully at XHI. */
/*                    QLEFT is INT */
/*     QHI <-- .TRUE. if F(X) .GT. Y at the termination of the */
/*              search and .FALSE. if F(X) .LT. Y at the */
/*              termination of the search. */
/*                    QHI is INT */

/* ********************************************************************** */
/*     Modified by S. Steer INRIA 1998,to replace ASSIGN instruction by */
/*     Computed GOTO */
/* ********************************************************************** */

static int dzror_cdf_0 (int n__, int *status, double *x, double *fx, double *xlo,
			double *xhi, int *qleft, int *qhi, double *zxlo, double *zxhi,
			const double *zabstl,const double *zreltl);

int cdf_dzror (int *status, double *x, double *fx, double *xlo, double *xhi,
	       int *qleft, int *qhi)
{
  return dzror_cdf_0 (0, status, x, fx, xlo, xhi, qleft, qhi, (double *) 0,
		      (double *) 0, (const double *) 0, (const double *) 0);
}

int cdf_dstzr (double *zxlo, double *zxhi,const double *zabstl,const double *zreltl)
{
  return dzror_cdf_0 (1, (int *) 0, (double *) 0, (double *) 0, (double *) 0,
		      (double *) 0, (int *) 0, (int *) 0, zxlo, zxhi, zabstl,
		      zreltl);
}


/* ********************************************************************** */
/*     SUBROUTINE DSTZR( XLO, XHI, ABSTOL, RELTOL ) */
/*     Double precision SeT ZeRo finder - Reverse communication version */
/*                              Function */
/*     Sets quantities needed by ZROR.  The function of ZROR */
/*     and the quantities set is given here. */
/*     Concise Description - Given a function F */
/*     find XLO such that F(XLO) = 0. */
/*          More Precise Description - */
/*     Input condition. F is a double precision function of a single */
/*     double precision argument and XLO and XHI are such that */
/*          F(XLO)*F(XHI)  .LE.  0.0 */
/*     If the input condition is met, QRZERO returns .TRUE. */
/*     and output values of XLO and XHI satisfy the following */
/*          F(XLO)*F(XHI)  .LE. 0. */
/*          ABS(F(XLO)  .LE. ABS(F(XHI) */
/*          ABS(XLO-XHI)  .LE. TOL(X) */
/*     where */
/*          TOL(X) = MAX(ABSTOL,RELTOL*ABS(X)) */
/*     If this algorithm does not find XLO and XHI satisfying */
/*     these conditions then QRZERO returns .FALSE.  This */
/*     implies that the input condition was not met. */
/*                              Arguments */
/*     XLO --> The left endpoint of the interval to be */
/*           searched for a solution. */
/*                    XLO is DOUBLE PRECISION */
/*     XHI --> The right endpoint of the interval to be */
/*           for a solution. */
/*                    XHI is DOUBLE PRECISION */
/*     ABSTOL, RELTOL --> Two numbers that determine the accuracy */
/*                      of the solution.  See function for a */
/*                      precise definition. */
/*                    ABSTOL is DOUBLE PRECISION */
/*                    RELTOL is DOUBLE PRECISION */
/*                              Method */
/*     Algorithm R of the paper 'Two Efficient Algorithms with */
/*     Guaranteed Convergence for Finding a Zero of a Function' */
/*     by J. C. P. Bus and T. J. Dekker in ACM Transactions on */
/*     Mathematical Software, Volume 1, no. 4 page 330 */
/*     (Dec. '75) is employed to find the zero of F(X)-Y. */
/* ********************************************************************** */

static int dzror_cdf_0 (int n__, int *status, double *x, double *fx, double *xlo,
			double *xhi, int *qleft, int *qhi, double *zxlo, double *zxhi,
			const double *zabstl,const double *zreltl)
{
  double d__1, d__2;
  static double xxhi, xxlo, a, b, c__, d__, m, p, q, w;
  static int first;
  static double fa, fb, fc, fd, mb, abstol, reltol;
  static int qrzero;
  static double fda, fdb, tol;
  static int ext, i99999;

  switch (n__)
    {
    case 1:
      goto L_dstzr;
    }

  if (*status > 0)
    {
      switch (i99999)
	{
	case 1:
	  goto L10;
	case 2:
	  goto L20;
	case 3:
	  goto L200;
	}
      return 0;
    }

  *xlo = xxlo;
  *xhi = xxhi;
  b = *xlo;
  *x = *xlo;
  /*     GET-FUNCTION-VALUE */
  /*      ASSIGN 10 TO i99999 */
  i99999 = 1;
  goto L270;
 L10:
  fb = *fx;
  *xlo = *xhi;
  a = *xlo;
  *x = *xlo;
  /*     GET-FUNCTION-VALUE */
  /*      ASSIGN 20 TO i99999 */
  i99999 = 2;
  goto L270;
  /*     Check that F(ZXLO) < 0 < F(ZXHI)  or */
  /*                F(ZXLO) > 0 > F(ZXHI) */
 L20:
  if (!(fb < 0.))
    {
      goto L40;
    }
  if (!(*fx < 0.))
    {
      goto L30;
    }
  *status = -1;
  *qleft = *fx < fb;
  *qhi = FALSE;
  return 0;
L30:
L40:
  if (!(fb > 0.))
    {
      goto L60;
    }
  if (!(*fx > 0.))
    {
      goto L50;
    }
  *status = -1;
  *qleft = *fx > fb;
  *qhi = TRUE;
  return 0;
L50:
L60:
  fa = *fx;

  first = TRUE;
L70:
  c__ = a;
  fc = fa;
  ext = 0;
L80:
  if (!(Abs (fc) < Abs (fb)))
    {
      goto L100;
    }
  if (!(c__ != a))
    {
      goto L90;
    }
  d__ = a;
  fd = fa;
L90:
  a = b;
  fa = fb;
  *xlo = c__;
  b = *xlo;
  fb = fc;
  c__ = a;
  fc = fa;
L100:
/* Computing MAX */
  d__1 = abstol, d__2 = reltol * Abs (*xlo);
  tol = Max (d__1, d__2) * .5;
  m = (c__ + b) * .5;
  mb = m - b;
  if (!(Abs (mb) > tol))
    {
      goto L240;
    }
  if (!(ext > 3))
    {
      goto L110;
    }
  w = mb;
  goto L190;
L110:
  tol = D_SIGN (tol,mb);
  p = (b - a) * fb;
  if (!first)
    {
      goto L120;
    }
  q = fa - fb;
  first = FALSE;
  goto L130;
L120:
  fdb = (fd - fb) / (d__ - b);
  fda = (fd - fa) / (d__ - a);
  p = fda * p;
  q = fdb * fa - fda * fb;
L130:
  if (!(p < 0.))
    {
      goto L140;
    }
  p = -p;
  q = -q;
L140:
  if (ext == 3)
    {
      p *= 2.;
    }
  if (!(p * 1. == 0. || p <= q * tol))
    {
      goto L150;
    }
  w = tol;
  goto L180;
L150:
  if (!(p < mb * q))
    {
      goto L160;
    }
  w = p / q;
  goto L170;
L160:
  w = mb;
L170:
L180:
L190:
  d__ = a;
  fd = fa;
  a = b;
  fa = fb;
  b += w;
  *xlo = b;
  *x = *xlo;
/*     GET-FUNCTION-VALUE */
/*      ASSIGN 200 TO i99999 */
  i99999 = 3;
  goto L270;
L200:
  fb = *fx;
  if (!(fc * fb >= 0.))
    {
      goto L210;
    }
  goto L70;
L210:
  if (!(w == mb))
    {
      goto L220;
    }
  ext = 0;
  goto L230;
L220:
  ++ext;
L230:
  goto L80;
L240:
  *xhi = c__;
  qrzero = (fc >= 0. && fb <= 0.) || (fc < 0. && fb >= 0.);
  if (!qrzero)
    {
      goto L250;
    }
  *status = 0;
  goto L260;
L250:
  *status = -1;
L260:
  return 0;
L_dstzr:
  xxlo = *zxlo;
  xxhi = *zxhi;
  abstol = *zabstl;
  reltol = *zreltl;
  return 0;
  /* (jpc)      STOP '*** EXECUTION FLOWING INTO FLECS PROCEDURES ***' */
  /*     TO GET-FUNCTION-VALUE */
L270:
  *status = 1;
  return 0;
}



