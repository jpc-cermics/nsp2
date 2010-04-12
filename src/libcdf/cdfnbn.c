#include "cdf.h"

/* ********************************************************************** */
/*      SUBROUTINE CDFNBN ( WHICH, P,Q, S, XN, PR, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*               Negative BiNomial distribution */
/*                              Function */
/*     Calculates any one parameter of the negative binomial */
/*     distribution given values for the others. */
/*     The  cumulative  negative   binomial  distribution  returns  the */
/*     probability that there  will be  F or fewer failures before  the */
/*     XNth success in binomial trials each of which has probability of */
/*     success PR. */
/*     The individual term of the negative binomial is the probability of */
/*     S failures before XN successes and is */
/*          Choose( S, XN+S-1 ) * PR^(XN) * (1-PR)^S */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next four argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..4 */
/*               iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR */
/*               iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR */
/*               iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR */
/*               iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN */
/*                    INT WHICH */
/*     P <--> The cumulation from 0 to S of the  negative */
/*            binomial distribution. */
/*            Input range: [0,1]. */
/*                    DOUBLE PRECISION P */
/*     Q <--> 1-P. */
/*            Input range: (0, 1]. */
/*            P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*     S <--> The upper limit of cumulation of the binomial distribution. */
/*            There are F or fewer failures before the XNth success. */
/*            Input range: [0, +infinity). */
/*            Search range: [0, 1E300] */
/*                    DOUBLE PRECISION S */
/*     XN  <--> The number of successes. */
/*              Input range: [0, +infinity). */
/*              Search range: [0, 1E300] */
/*                    DOUBLE PRECISION XN */
/*     PR  <--> The probability of success in each binomial trial. */
/*              Input range: [0,1]. */
/*              Search range: [0,1]. */
/*                    DOUBLE PRECISION PR */
/*     OMPR  <--> 1-PR */
/*              Input range: [0,1]. */
/*              Search range: [0,1] */
/*              PR + OMPR = 1.0 */
/*                    DOUBLE PRECISION OMPR */
/*     STATUS <-- 0 if calculation completed correctly */
/*               -I if input parameter number I is out of range */
/*                1 if answer appears to be lower than lowest */
/*                  search bound */
/*                2 if answer appears to be higher than greatest */
/*                  search bound */
/*                3 if P + Q .ne. 1 */
/*                4 if PR + OMPR .ne. 1 */
/*                    INT STATUS */
/*     BOUND <-- Undefined if STATUS is 0 */
/*               Bound exceeded by parameter number I if STATUS */
/*               is negative. */
/*               Lower search bound if STATUS is 1. */
/*               Upper search bound if STATUS is 2. */
/*                              Method */
/*     Formula   26.5.26   of   Abramowitz  and  Stegun,  Handbook   of */
/*     Mathematical Functions (1966) is used  to  reduce calculation of */
/*     the cumulative distribution  function to that of  an  incomplete */
/*     beta. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */
/* ********************************************************************** */

int
cdf_cdfnbn (int *which, double *p, double *q, double *s, double *xn,
	    double *pr, double *ompr, int *status, double *bound)
{
static int c__1 = 1;
static double c_b35 = 0.;
static double c_b36 = .5;
static double c_b38 = 5.;
static double c_b58 = 1.;

  const double tol=1.0E-14, atol=1.0E-50,inf=1.0E300, one=1.0E0;
  /* System generated locals */
  double d__1;

  /* Local variables */
  double ccum;
  int qleft;
  int qporq;
  double fx, pq;
  double prompr;
  int qhi;
  double cum, xhi, xlo;

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

  if (!(*s < 0.) || *which==1 )   /* add *which==1 to compute cdfnnbn with *s < 0 (bruno march,22,2010)) */ 
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

/*     XN */

  if (!(*xn < 0.))
    {
      goto L140;
    }
  *bound = 0.;
  *status = -5;
  return 0;
L140:
L150:
  if (*which == 4)
    {
      goto L190;
    }

/*     PR */

  if (!(*pr < 0. || *pr > 1.))
    {
      goto L180;
    }
  if (!(*pr < 0.))
    {
      goto L160;
    }
  *bound = 0.;
  goto L170;
L160:
  *bound = 1.;
L170:
  *status = -6;
  return 0;
L180:
L190:
  if (*which == 4)
    {
      goto L230;
    }

/*     OMPR */

  if (!(*ompr < 0. || *ompr > 1.))
    {
      goto L220;
    }
  if (!(*ompr < 0.))
    {
      goto L200;
    }
  *bound = 0.;
  goto L210;
L200:
  *bound = 1.;
L210:
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
  if (*which == 4)
    {
      goto L310;
    }

/*     PR + OMPR */

  prompr = *pr + *ompr;
  if (!((d__1 = prompr - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L300;
    }
  if (!(prompr < 0.))
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

  if (1 == *which)            /* Calculating P */
    {
      double sf = floor(*s);  /* add floor to compute the real cdfnbn (bruno march,22,2010)) */
      cdf_cumnbn (&sf, xn, pr, ompr, p, q);
      *status = 0;
    }

  else if (2 == *which)       /* Calculating S */
    {
      /* bruno april 2010 (using the jpc 's workaround of cdfbin) */
      double p0, q0, s0=0.0;
      cdf_cumnbn(&s0, xn, pr, ompr, &p0, &q0);
      if ( *p <= p0 )
	{
	  *status = 0; 
	  *s =0.0;
	  return 0;
	}

      *s = 5.;
      cdf_dstinv (&c_b35, &inf, &c_b36, &c_b36, &c_b38, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, s, &fx, &qleft, &qhi);
    L320:
      if (!(*status == 1))
	{
	  goto L350;
	}
      cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
      if (!qporq)
	{
	  goto L330;
	}
      fx = cum - *p;
      goto L340;
    L330:
      fx = ccum - *q;
    L340:
      cdf_dinvr (status, s, &fx, &qleft, &qhi);
      goto L320;
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
      *bound = 0.;
      goto L370;
    L360:
      *status = 2;
      *bound = inf;
    L370:
    L380:
      ;
    }
  else if (3 == *which)
    {

/*     Calculating XN */

      *xn = 5.;
      cdf_dstinv (&c_b35, &inf, &c_b36, &c_b36, &c_b38, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, xn, &fx, &qleft, &qhi);
    L390:
      if (!(*status == 1))
	{
	  goto L420;
	}
      cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
      if (!qporq)
	{
	  goto L400;
	}
      fx = cum - *p;
      goto L410;
    L400:
      fx = ccum - *q;
    L410:
      cdf_dinvr (status, xn, &fx, &qleft, &qhi);
      goto L390;
    L420:
      if (!(*status == -1))
	{
	  goto L450;
	}
      if (!qleft)
	{
	  goto L430;
	}
      *status = 1;
      *bound = 0.;
      goto L440;
    L430:
      *status = 2;
      *bound = inf;
    L440:
    L450:
      ;
    }
  else if (4 == *which)
    {

/*     Calculating PR and OMPR */

      cdf_dstzr (&c_b35, &c_b58, &atol, &tol);
      if (!qporq)
	{
	  goto L480;
	}
      *status = 0;
      cdf_dzror (status, pr, &fx, &xlo, &xhi, &qleft, &qhi);
      *ompr = one - *pr;
    L460:
      if (!(*status == 1))
	{
	  goto L470;
	}
      cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
      fx = cum - *p;
      cdf_dzror (status, pr, &fx, &xlo, &xhi, &qleft, &qhi);
      *ompr = one - *pr;
      goto L460;
    L470:
      goto L510;
    L480:
      *status = 0;
      cdf_dzror (status, ompr, &fx, &xlo, &xhi, &qleft, &qhi);
      *pr = one - *ompr;
    L490:
      if (!(*status == 1))
	{
	  goto L500;
	}
      cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
      fx = ccum - *q;
      cdf_dzror (status, ompr, &fx, &xlo, &xhi, &qleft, &qhi);
      *pr = one - *ompr;
      goto L490;
    L500:
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
      *bound = 0.;
      goto L530;
    L520:
      *status = 2;
      *bound = 1.;
    L530:
    L540:
      ;
    }
  return 0;
}				/* cdfnbn_ */
