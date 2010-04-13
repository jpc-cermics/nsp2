#include "cdf.h"

/* ********************************************************************** */
/*      SUBROUTINE CDFPOI( WHICH, P, Q, S, XLAM, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*               POIsson distribution */
/*                              Function */
/*     Calculates any one parameter of the Poisson */
/*     distribution given values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which  argument */
/*               value is to be calculated from the others. */
/*               Legal range: 1..3 */
/*               iwhich = 1 : Calculate P and Q from S and XLAM */
/*               iwhich = 2 : Calculate A from P,Q and XLAM */
/*               iwhich = 3 : Calculate XLAM from P,Q and S */
/*                    INT WHICH */
/*        P <--> The cumulation from 0 to S of the poisson density. */
/*               Input range: [0,1]. */
/*                    DOUBLE PRECISION P */
/*        Q <--> 1-P. */
/*               Input range: (0, 1]. */
/*               P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*        S <--> Upper limit of cumulation of the Poisson. */
/*               Input range: [0, +infinity). */
/*               Search range: [0,1E300] */
/*                    DOUBLE PRECISION S */
/*     XLAM <--> Mean of the Poisson distribution. */
/*               Input range: [0, +infinity). */
/*               Search range: [0,1E300] */
/*                    DOUBLE PRECISION XLAM */
/*     STATUS <-- 0 if calculation completed correctly */
/*               -I if input parameter number I is out of range */
/*                1 if answer appears to be lower than lowest */
/*                  search bound */
/*                2 if answer appears to be higher than greatest */
/*                  search bound */
/*                3 if P + Q .ne. 1 */
/*                    INT STATUS */
/*     BOUND <-- Undefined if STATUS is 0 */
/*               Bound exceeded by parameter number I if STATUS */
/*               is negative. */
/*               Lower search bound if STATUS is 1. */
/*               Upper search bound if STATUS is 2. */
/*                              Method */
/*     Formula   26.4.21  of   Abramowitz  and   Stegun,   Handbook  of */
/*     Mathematical Functions (1966) is used  to reduce the computation */
/*     of  the cumulative distribution function to that  of computing a */
/*     chi-square, hence an incomplete gamma function. */
/*     Cumulative  distribution function  (P) is  calculated  directly. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired value of  P.   The  search relies  on  the */
/*     monotinicity of P with the other parameter. */
/* ********************************************************************** */

int
cdf_cdfpoi (int *which, double *p, double *q, double *s, double *xlam,
	    int *status, double *bound)
{
static int c__1 = 1;
static double c_b22 = 0.;
static double c_b23 = .5;
static double c_b25 = 5.;

  const double tol=1.0E-14, atol=1.0E-50, inf=1.0E300;
  double d__1;
  double ccum;
  int qleft;
  int qporq;
  double fx, pq;
  int qhi;
  double cum;

  if (!(*which < 1 || *which > 3))
    {
      goto L30;
    }
  if (!(*which < 1))
    {
      goto L10;
    }
  *bound = 1.;
  goto L20;
L10:
  *bound = 3.;
L20:
  *status = -1;
  return 0;
L30:
  if (*which == 1)
    {
      goto L70;
    }

/*     P */

  if (!(*p < 0. || *p > 1.))
    {
      goto L60;
    }
  if (!(*p < 0.))
    {
      goto L40;
    }
  *bound = 0.;
  goto L50;
L40:
  *bound = 1.;
L50:
  *status = -2;
  return 0;
L60:
L70:
  if (*which == 1)
    {
      goto L110;
    }

/*     Q */

  if (!(*q <= 0. || *q > 1.))
    {
      goto L100;
    }
  if (!(*q <= 0.))
    {
      goto L80;
    }
  *bound = 0.;
  goto L90;
L80:
  *bound = 1.;
L90:
  *status = -3;
  return 0;
L100:
L110:
  if (*which == 2)
    {
      goto L130;
    }

/*     S */

  if (!(*s < 0.) || *which==1 )   /* add *which==1 to compute cdfpoi with *s < 0 (bruno april,12,2010)) */
    {
      goto L120;
    }
  *bound = 0.;
  *status = -4;
  return 0;
L120:
L130:
  if (*which == 3)
    {
      goto L150;
    }

/*     XLAM */

  if (!(*xlam < 0.))
    {
      goto L140;
    }
  *bound = 0.;
  *status = -5;
  return 0;
L140:
L150:
  if (*which == 1)
    {
      goto L190;
    }

/*     P + Q */

  pq = *p + *q;
  if (!((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L180;
    }
  if (!(pq < 0.))
    {
      goto L160;
    }
  *bound = 0.;
  goto L170;
L160:
  *bound = 1.;
L170:
  *status = 3;
  return 0;
L180:
L190:
  if (!(*which == 1))
    {
      qporq = *p <= *q;
    }
  /*     Select the minimum of P or Q */
  /*     Calculate ANSWERS */
  if (1 == *which)
    {
      /*     Calculating P */
      double sf = floor(*s);  /* add floor to compute the real cdfpoi (bruno april 2010)) */
      cdf_cumpoi (&sf, xlam, p, q);
      *status = 0;
    }
  else if (2 == *which)       /*     Calculating S */
    {
      if ( *p <= exp(-*xlam) ) /* bruno april 2010 (using the jpc 's workaround of cdfbin) */
	{
	  *status = 0; 
	  *s =0.0;
	  return 0;
	}
      *s = 5.;
      cdf_dstinv (&c_b22, &inf, &c_b23, &c_b23, &c_b25, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, s, &fx, &qleft, &qhi);
    L200:
      if (!(*status == 1))
	{
	  goto L230;
	}
      cdf_cumpoi (s, xlam, &cum, &ccum);
      if (!qporq)
	{
	  goto L210;
	}
      fx = cum - *p;
      goto L220;
    L210:
      fx = ccum - *q;
    L220:
      cdf_dinvr (status, s, &fx, &qleft, &qhi);
      goto L200;
    L230:
      if (!(*status == -1))
	{
	  goto L260;
	}
      if (!qleft)
	{
	  goto L240;
	}
      *status = 1;
      *bound = 0.;
      goto L250;
    L240:
      *status = 2;
      *bound = inf;
    L250:
    L260:
      ;
    }
  else if (3 == *which)
    {
      /*     Calculating XLAM */
      *xlam = 5.;
      cdf_dstinv (&c_b22, &inf, &c_b23, &c_b23, &c_b25, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, xlam, &fx, &qleft, &qhi);
    L270:
      if (!(*status == 1))
	{
	  goto L300;
	}
      cdf_cumpoi (s, xlam, &cum, &ccum);
      if (!qporq)
	{
	  goto L280;
	}
      fx = cum - *p;
      goto L290;
    L280:
      fx = ccum - *q;
    L290:
      cdf_dinvr (status, xlam, &fx, &qleft, &qhi);
      goto L270;
    L300:
      if (!(*status == -1))
	{
	  goto L330;
	}
      if (!qleft)
	{
	  goto L310;
	}
      *status = 1;
      *bound = 0.;
      goto L320;
    L310:
      *status = 2;
      *bound = inf;
    L320:
    L330:
      ;
    }
  return 0;
}		

