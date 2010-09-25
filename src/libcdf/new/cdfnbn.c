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
	    double *pr, double *ompr, int *status, double *bound, double *boundbis)
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

  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );
 
  if (*which != 1)
    {
      /*     P */
      CDF_CHECK_ARG(*p < 0 , 0 , -2 );
      CDF_CHECK_ARG(*p > 1 , 1 , -2 );
      /*     Q */
      CDF_CHECK_ARG(*q <= 0 , 0 , -3 );
      CDF_CHECK_ARG(*q > 1 , 1 , -3 );
    }

  if (*which != 2 && *which != 1 )
    {
      /* add *which==1 to compute cdfnnbn with *s < 0 (bruno march,22,2010)) */ 
      /*     S */
      CDF_CHECK_ARG(*s  < 0 , 0 , -4 );
    }

  if (*which != 3)
    {
      /*     XN */
      CDF_CHECK_ARG(*xn  < 0 , 0 , -5 );
    }

  if (*which != 4)
    {
      /*     PR */
      CDF_CHECK_ARG(*pr  < 0 , 0 , -6 );
      CDF_CHECK_ARG(*pr  > 1 , 1 , -6 );
      /*     OMPR */
      CDF_CHECK_ARG(*ompr < 0 , 0 , -7 );
      CDF_CHECK_ARG(*ompr > 1 , 1 , -7 );
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

  if (*which != 4)
    {
      /*     PR + OMPR */
      prompr = *pr + *ompr;
      if (((d__1 = prompr - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
	{
	  *bound = (prompr < 0.) ?  0. : 1.0 ;
	  *status = 4;
	  return 0;
	}
    }

  if (*which != 1)
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

      *s = *xn *(*pr/(1.0 - *pr));  /* start from the mean instead of 5, bruno april 2010 */
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
