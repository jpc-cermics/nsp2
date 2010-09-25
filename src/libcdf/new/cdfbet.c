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
		double *b, int *status, double *bound, double *boundbis)
{
  const double c_b46 = .5;
  const double c_b48 = 5.;
  const double tol=1.0E-14, atol=1.0E-120,zero=1.0E-300,inf=1.0E300,one=1.0E0;
  const int c__1 = 1;
  double c_b35 = 0. , c_b36 = 1.;
  double ccum,  cum, xhi, xlo,  d__1, fx, pq,  xy;
  int qhi, qleft, qporq;
  
  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );
 
  if (*which != 1)
    {
      /*     P */
      CDF_CHECK_ARG(*p < 0 , 0 , -2 );
      CDF_CHECK_ARG(*p > 1 , 1 , -2 );
      /*     Q */
      CDF_CHECK_ARG(*q < 0 , 0 , -3 );
      CDF_CHECK_ARG(*q > 1 , 1 , -3 );
    }

  if (*which != 2 && *which != 1)
    {
      /* add *which==1 to compute cdfbet for every *x (bruno march,22,2010)) */
      /*     X */
      CDF_CHECK_ARG(*x < 0 , 0 , -4 );
      CDF_CHECK_ARG(*x > 1 , 1 , -4 );
      /*     Y */
      CDF_CHECK_ARG(*y < 0 , 0 , -5 );
      CDF_CHECK_ARG(*y > 1 , 1 , -5 );
    }

  if (*which != 3)
    {
      /*     A */
      CDF_CHECK_ARG(*a <=0 , 0 , -6 );
    }

  if (*which != 4)
    {
      /*     B */
      CDF_CHECK_ARG(*b <=0 , 0 , -7 );
    }

  if (*which != 1)
    {
      /*     P + Q */
      pq = *p + *q;
      if (((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
	{
	  *bound = (!(pq < 0.)) ?  1.: 0.0;
	  *status = 3;
	  return 0;
	}
    }

  if (*which != 2)
    {
      /*     X + Y */
      xy = *x + *y;
      if (((d__1 = xy - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
	{
	  *bound = (xy < 0.) ? 0 : 1 ;
	  *status = 4;
	  return 0;
	}
    }

  if ( *which != 1)
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
