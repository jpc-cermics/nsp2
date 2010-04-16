#include "cdf.h"

/* ********************************************************************** */
/*      SUBROUTINE CDFCHI( WHICH, P, Q, X, DF, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*               CHI-Square distribution */
/*                              Function */
/*     Calculates any one parameter of the chi-square */
/*     distribution given values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next three argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..3 */
/*               iwhich = 1 : Calculate P and Q from X and DF */
/*               iwhich = 2 : Calculate X from P,Q and DF */
/*               iwhich = 3 : Calculate DF from P,Q and X */
/*                    INT WHICH */
/*     P <--> The integral from 0 to X of the chi-square */
/*            distribution. */
/*            Input range: [0, 1]. */
/*                    DOUBLE PRECISION P */
/*     Q <--> 1-P. */
/*            Input range: (0, 1]. */
/*            P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*     X <--> Upper limit of integration of the non-central */
/*            chi-square distribution. */
/*            Input range: [0, +infinity). */
/*            Search range: [0,1E300] */
/*                    DOUBLE PRECISION X */
/*     DF <--> Degrees of freedom of the */
/*             chi-square distribution. */
/*             Input range: (0, +infinity). */
/*             Search range: [ 1E-300, 1E300] */
/*                    DOUBLE PRECISION DF */
/*     STATUS <-- 0 if calculation completed correctly */
/*               -I if input parameter number I is out of range */
/*                1 if answer appears to be lower than lowest */
/*                  search bound */
/*                2 if answer appears to be higher than greatest */
/*                  search bound */
/*                3 if P + Q .ne. 1 */
/*               10 indicates error returned from cumgam.  See */
/*                  references in cdfgam */
/*                    INT STATUS */
/*     BOUND <-- Undefined if STATUS is 0 */
/*               Bound exceeded by parameter number I if STATUS */
/*               is negative. */
/*               is negative. */
/*               Lower search bound if STATUS is 1. */
/*               Upper search bound if STATUS is 2. */
/*                              Method */
/*     Formula    26.4.19   of Abramowitz  and     Stegun, Handbook  of */
/*     Mathematical Functions   (1966) is used   to reduce the chisqure */
/*     distribution to the incomplete distribution. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */



int cdf_cdfchi (int *which, double *p, double *q, double *x, double *df,
		int *status, double *bound)
{
  const int c__1 = 1;
  const double c_b25 = 0.;
  const double c_b26 = .5;
  const double c_b28 = 5.;
  const double tol=1.0E-14,atol=1.0E-50,zero=1.0E-300,inf=1.0E300;
  double d__1;
  double ccum, porq;
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

/*     X */

  if (!(*x < 0.) || *which==1 )   /* add *which==1 to compute cdfchi with *x < 0 (bruno march,22,2010)) */
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

/*     DF */

  if (!(*df <= 0.))
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
  if (*which == 1)
    {
      goto L220;
    }

/*     Select the minimum of P or Q */

  qporq = *p <= *q;
  if (!qporq)
    {
      goto L200;
    }
  porq = *p;
  goto L210;
L200:
  porq = *q;
L210:

/*     Calculate ANSWERS */

L220:
  if (1 == *which)
    {

/*     Calculating P and Q */

      *status = 0;
      cdf_cumchi (x, df, p, q);
/*     jpc 2000 : porq must be computed ? */
/*     ---> IF (porq.GT.1.5D0) THEN */
      if (*p > 1.5 || *q > 1.5)
	{
	  *status = 10;
	  return 0;
	}
    }
  else if (2 == *which)
    {

/*     Calculating X */

      *x = *df;  /* start from the mean instead of 5, bruno april 2010 */
      cdf_dstinv (&c_b25, &inf, &c_b26, &c_b26, &c_b28, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, x, &fx, &qleft, &qhi);
    L230:
      if (!(*status == 1))
	{
	  goto L270;
	}
      cdf_cumchi (x, df, &cum, &ccum);
      if (!qporq)
	{
	  goto L240;
	}
      fx = cum - *p;
      goto L250;
    L240:
      fx = ccum - *q;
    L250:
      if (!(fx + porq > 1.5))
	{
	  goto L260;
	}
      *status = 10;
      return 0;
    L260:
      cdf_dinvr (status, x, &fx, &qleft, &qhi);
      goto L230;
    L270:
      if (!(*status == -1))
	{
	  goto L300;
	}
      if (!qleft)
	{
	  goto L280;
	}
      *status = 1;
      *bound = 0.;
      goto L290;
    L280:
      *status = 2;
      *bound = inf;
    L290:
    L300:
      ;
    }
  else if (3 == *which)
    {

/*     Calculating DF */

      *df = 5.;
      cdf_dstinv (&zero, &inf, &c_b26, &c_b26, &c_b28, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
    L310:
      if (!(*status == 1))
	{
	  goto L350;
	}
      cdf_cumchi (x, df, &cum, &ccum);
      if (!qporq)
	{
	  goto L320;
	}
      fx = cum - *p;
      goto L330;
    L320:
      fx = ccum - *q;
    L330:
      if (!(fx + porq > 1.5))
	{
	  goto L340;
	}
      *status = 10;
      return 0;
    L340:
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
      goto L310;
    L350:
      if (!(*status == -1))
	{
	  goto L380;
	}
      if (!qleft)
	{
	  goto L360;
	}
      *status = 1;
      *bound = zero;
      goto L370;
    L360:
      *status = 2;
      *bound = inf;
    L370:
    L380:
      ;
    }
  return 0;
}				/* cdfchi_ */
