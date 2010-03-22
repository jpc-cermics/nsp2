#include "cdf.h"


/* ********************************************************************** */
/*      SUBROUTINE CDFBET( WHICH, P, Q, X, Y, A, B, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*                         BETa Distribution */
/*                              Function */
/*     Calculates any one parameter of the beta distribution given */
/*     values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next four argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..4 */
/*               iwhich = 1 : Calculate P and Q from X,Y,A and B */
/*               iwhich = 2 : Calculate X and Y from P,Q,A and B */
/*               iwhich = 3 : Calculate A from P,Q,X,Y and B */
/*               iwhich = 4 : Calculate B from P,Q,X,Y and A */
/*                    INT WHICH */
/*     P <--> The integral from 0 to X of the chi-square */
/*            distribution. */
/*            Input range: [0, 1]. */
/*                    DOUBLE PRECISION P */
/*     Q <--> 1-P. */
/*            Input range: [0, 1]. */
/*            P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*     X <--> Upper limit of integration of beta density. */
/*            Input range: [0,1]. */
/*            Search range: [0,1] */
/*                    DOUBLE PRECISION X */
/*     Y <--> 1-X. */
/*            Input range: [0,1]. */
/*            Search range: [0,1] */
/*            X + Y = 1.0. */
/*                    DOUBLE PRECISION Y */
/*     A <--> The first parameter of the beta density. */
/*            Input range: (0, +infinity). */
/*            Search range: [1E-300,1D300] */
/*                    DOUBLE PRECISION A */
/*     B <--> The second parameter of the beta density. */
/*            Input range: (0, +infinity). */
/*            Search range: [1E-300,1D300] */
/*                    DOUBLE PRECISION B */
/*     STATUS <-- 0 if calculation completed correctly */
/*               -I if input parameter number I is out of range */
/*                1 if answer appears to be lower than lowest */
/*                  search bound */
/*                2 if answer appears to be higher than greatest */
/*                  search bound */
/*                3 if P + Q .ne. 1 */
/*                4 if X + Y .ne. 1 */
/*                    INT STATUS */
/*     BOUND <-- Undefined if STATUS is 0 */
/*               Bound exceeded by parameter number I if STATUS */
/*               is negative. */
/*               Lower search bound if STATUS is 1. */
/*               Upper search bound if STATUS is 2. */
/*                              Method */
/*     Cumulative distribution function  (P)  is calculated directly by */
/*     code associated with the following reference. */
/*     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant */
/*     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM */
/*     Trans. Math.  Softw. 18 (1993), 360-373. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */
/*                              Note */
/*     The beta density is proportional to */
/*               t^(A-1) * (1-t)^(B-1) */


int cdf_cdfbet (int *which, double *p, double *q, double *x, double *y, double *a,
		double *b, int *status, double *bound)
{
  const double c_b46 = .5;
  const double c_b48 = 5.;
  const double tol=1.0E-8, atol=1.0E-50,zero=1.0E-300,inf=1.0E300,one=1.0E0;
  const int c__1 = 1;
  double c_b35 = 0. , c_b36 = 1.;
  double ccum,  cum, xhi, xlo,  d__1, fx, pq,  xy;
  int qhi, qleft, qporq;
  
  if (!(*which < 1 || *which > 4))
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
  *bound = 4.;
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
  if (!(*q < 0. || *q > 1.))
    {
      goto L100;
    }
  if (!(*q < 0.))
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
      goto L150;
    }
  /*     X */
  if (!(*x < 0. || *x > 1.) || *which==1 )   /* add *which==1 to compute cdfbet for every *x (bruno march,22,2010)) */
    {
      goto L140;
    }
  if (!(*x < 0.))
    {
      goto L120;
    }
  *bound = 0.;
  goto L130;
L120:
  *bound = 1.;
L130:
  *status = -4;
  return 0;
L140:
L150:
  if (*which == 2)
    {
      goto L190;
    }
  /*     Y */
  if (!(*y < 0. || *y > 1.) || *which==1 )   /* add *which==1 to compute cdfbet for every *y (y=1-x) (bruno march,22,2010)) */
    {
      goto L180;
    }
  if (!(*y < 0.))
    {
      goto L160;
    }
  *bound = 0.;
  goto L170;
L160:
  *bound = 1.;
L170:
  *status = -5;
  return 0;
L180:
L190:
  if (*which == 3)
    {
      goto L210;
    }
  /*     A */
  if (!(*a <= 0.))
    {
      goto L200;
    }
  *bound = 0.;
  *status = -6;
  return 0;
L200:
L210:
  if (*which == 4)
    {
      goto L230;
    }
  /*     B */
  if (!(*b <= 0.))
    {
      goto L220;
    }
  *bound = 0.;
  *status = -7;
  return 0;
L220:
L230:
  if (*which == 1)
    {
      goto L270;
    }
  /*     P + Q */
  pq = *p + *q;
  if (!((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L260;
    }
  if (!(pq < 0.))
    {
      goto L240;
    }
  *bound = 0.;
  goto L250;
L240:
  *bound = 1.;
L250:
  *status = 3;
  return 0;
L260:
L270:
  if (*which == 2)
    {
      goto L310;
    }
  /*     X + Y */
  xy = *x + *y;
  if (!((d__1 = xy - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L300;
    }
  if (!(xy < 0.))
    {
      goto L280;
    }
  *bound = 0.;
  goto L290;
L280:
  *bound = 1.;
L290:
  *status = 4;
  return 0;
L300:
L310:
  if (!(*which == 1))
    {
      qporq = *p <= *q;
    }
  /*     Select the minimum of P or Q */
  /*     Calculate ANSWERS */
  if (1 == *which)
    {
      /*     Calculating P and Q */
      cdf_cumbet (x, y, a, b, p, q);
      *status = 0;
    }
  else if (2 == *which)
    {
      /*     Calculating X and Y */
      cdf_dstzr (&c_b35, &c_b36, &atol, &tol);
      if (!qporq)
	{
	  goto L340;
	}
      *status = 0;
      cdf_dzror (status, x, &fx, &xlo, &xhi, &qleft, &qhi);
      *y = one - *x;
    L320:
      if (!(*status == 1))
	{
	  goto L330;
	}
      cdf_cumbet (x, y, a, b, &cum, &ccum);
      fx = cum - *p;
      cdf_dzror (status, x, &fx, &xlo, &xhi, &qleft, &qhi);
      *y = one - *x;
      /*          write(6,'(''x'',e10.3,''y='',e10.3,''sta='',i3)') x,y,status */
      goto L320;
    L330:
      goto L370;
    L340:
      *status = 0;
      cdf_dzror (status, y, &fx, &xlo, &xhi, &qleft, &qhi);
      *x = one - *y;
    L350:
      if (!(*status == 1))
	{
	  goto L360;
	}
      cdf_cumbet (x, y, a, b, &cum, &ccum);
      fx = ccum - *q;
      cdf_dzror (status, y, &fx, &xlo, &xhi, &qleft, &qhi);
      *x = one - *y;
      goto L350;
    L360:
    L370:
      if (!(*status == -1))
	{
	  goto L400;
	}
      if (!qleft)
	{
	  goto L380;
	}
      *status = 1;
      *bound = 0.;
      goto L390;
    L380:
      *status = 2;
      *bound = 1.;
    L390:
    L400:
      ;
    }
  else if (3 == *which)
    {
      /*     Computing A */
      *a = 5.;
      cdf_dstinv (&zero, &inf, &c_b46, &c_b46, &c_b48, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, a, &fx, &qleft, &qhi);
    L410:
      if (!(*status == 1))
	{
	  goto L440;
	}
      cdf_cumbet (x, y, a, b, &cum, &ccum);
      if (!qporq)
	{
	  goto L420;
	}
      fx = cum - *p;
      goto L430;
    L420:
      fx = ccum - *q;
    L430:
      cdf_dinvr (status, a, &fx, &qleft, &qhi);
      goto L410;
    L440:
      if (!(*status == -1))
	{
	  goto L470;
	}
      if (!qleft)
	{
	  goto L450;
	}
      *status = 1;
      *bound = zero;
      goto L460;
    L450:
      *status = 2;
      *bound = inf;
    L460:
    L470:
      ;
    }
  else if (4 == *which)
    {
      /*     Computing B */
      *b = 5.;
      cdf_dstinv (&zero, &inf, &c_b46, &c_b46, &c_b48, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, b, &fx, &qleft, &qhi);
    L480:
      if (!(*status == 1))
	{
	  goto L510;
	}
      cdf_cumbet (x, y, a, b, &cum, &ccum);
      if (!qporq)
	{
	  goto L490;
	}
      fx = cum - *p;
      goto L500;
    L490:
      fx = ccum - *q;
    L500:
      cdf_dinvr (status, b, &fx, &qleft, &qhi);
      goto L480;
    L510:
      if (!(*status == -1))
	{
	  goto L540;
	}
      if (!qleft)
	{
	  goto L520;
	}
      *status = 1;
      *bound = zero;
      goto L530;
    L520:
      *status = 2;
      *bound = inf;
    L530:
    L540:
      ;
    }
  return 0;
}	
