#include "cdf.h"

/**
 * cdf_cdfpoi:
 * @which: integer indicating which  argument value is to be calculated from the others
 * @p: 
 * @q: 
 * @s: 
 * @xlam: 
 * @status: 
 * @bound: 
 * 
 * Cumulative Distribution Function os the Poisson distribution.
 * Calculates any one parameter of the Poisson distribution given 
 * values for the others. 
 * iwhich = 1 : Calculate P and Q from S and XLAM 
 * iwhich = 2 : Calculate A from P,Q and XLAM 
 * iwhich = 3 : Calculate XLAM from P,Q and S 
 * 
 * P <--> The cumulation from 0 to S of the poisson density. 
 *               Input range: [0,1]. 
 * Q <--> 1-P. Input range: (0, 1].    P + Q = 1.0. 
 * S <--> Upper limit of cumulation of the Poisson. 
 *               Input range: [0, +infinity). 
 *               Search range: [0,1E300] 
 * XLAM <--> Mean of the Poisson distribution. 
 *               Input range: [0, +infinity). 
 *               Search range: [0,1E300] 
 * STATUS <-- 0 if calculation completed correctly 
 *           -I if input parameter number I is out of range 
 *            1 if answer appears to be lower than lowest search bound 
 *            2 if answer appears to be higher than greatest search bound 
 *            3 if P + Q .ne. 1 
 * BOUND <-- Undefined if STATUS is 0 
 *           Bound exceeded by parameter number I if STATUS 
 *               is negative. 
 *               Lower search bound if STATUS is 1. 
 *               Upper search bound if STATUS is 2. 

 *     Formula   26.4.21  of   Abramowitz  and   Stegun,   Handbook  of 
 *     Mathematical Functions (1966) is used  to reduce the computation 
 *     of  the cumulative distribution function to that  of computing a 
 *     chi-square, hence an incomplete gamma function. 
 *     Cumulative  distribution function  (P) is  calculated  directly. 
 *     Computation of other parameters involve a seach for a value that 
 *     produces  the desired value of  P.   The  search relies  on  the 
 *     monotinicity of P with the other parameter. 
 *
 * Returns: 0
 **/

int
cdf_cdfpoi (int *which, double *p, double *q, double *s, double *xlam,
	    int *status, double *bound, double *boundbis)
{
  const int c__1 = 1;
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

  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 3 , 3 , -1 );
 
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
      /* add *which==1 to compute cdfpoi with *s < 0 (bruno april,12,2010)) */
      /*     S */
      CDF_CHECK_ARG(*s < 0 , 0 , -4 );
    }

  if (*which != 3)
    {
      /*     XLAM */
      CDF_CHECK_ARG(*xlam < 0 , 0 , -5 );
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

  if (*which != 1)
    {
      /*     P + Q */
      pq = *p + *q;
      if (((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
	{
	  * bound = (pq < 0.) ? 0: 1;
	  *status = 3;
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
      /*     Calculating P */
      double sf = floor(*s);  /* add floor to compute the real cdfpoi (bruno april 2010)) */
      cdf_cumpoi (&sf, xlam, p, q);
      *status = 0;
    }
  else if (2 == *which)       /*     Calculating S */
    {
      /*       if ( *p <= exp(-*xlam) ) bruno april 2010 (using the jpc 's workaround of cdfbin) */
      /* 	{ */
      /* 	  *status = 0; */
      /* 	  *s =0.0; */
      /* 	  return 0; */
      /* 	} */
      /*       *s = *xlam;  start from the mean instead of 5, bruno april 2010 */
      /*       cdf_dstinv (&c_b22, &inf, &c_b23, &c_b23, &c_b25, &atol, &tol); */
      /*       *status = 0; */
      /*       cdf_dinvr (status, s, &fx, &qleft, &qhi); */
      /*     L200: */
      /*       if (!(*status == 1)) */
      /* 	{ */
      /* 	  goto L230; */
      /* 	} */
      /*       cdf_cumpoi (s, xlam, &cum, &ccum); */
      /*       if (!qporq) */
      /* 	{ */
      /* 	  goto L210; */
      /* 	} */
      /*       fx = cum - *p; */
      /*       goto L220; */
      /*     L210: */
      /*       fx = ccum - *q; */
      /*     L220: */
      /*       cdf_dinvr (status, s, &fx, &qleft, &qhi); */
      /*       goto L200; */
      /*     L230: */
      /*       if (!(*status == -1)) */
      /* 	{ */
      /* 	  goto L260; */
      /* 	} */
      /*       if (!qleft) */
      /* 	{ */
      /* 	  goto L240; */
      /* 	} */
      /*       *status = 1; */
      /*       *bound = 0.; */
      /*       goto L250; */
      /*     L240: */
      /*       *status = 2; */
      /*       *bound = inf; */
      /*     L250: */
      /*     L260: */
      /*       ; */

      if ( *p <= exp(-*xlam) ) /* bruno april 2010 (using the jpc 's workaround of cdfbin) */
	{
	  *status = 0;
	  *s =0.0;
	  return 0;
	}
      else
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;
	  zsearch_monotonicity monotonicity = qporq ? INCREASING : DECREASING;
	  double step = sqrt(*xlam); /* initial step = std */
	  *s = *xlam + 0.333;  /* start near the median instead of 5, bruno september 2010 */
	  nsp_zsearch_init(*s, 0.0, inf, step, 0.0, 2.0, atol, tol, monotonicity, &S);

	  do
	    {
	      cdf_cumpoi (s, xlam, &cum, &ccum);
	      if ( qporq )
		fx = cum - *p;
	      else
		fx = ccum - *q;
	      ret_val = nsp_zsearch(s, fx, &S);
	    }
	  while ( ret_val == EVAL_FX );

	  switch ( ret_val )
	    {
	    case SUCCESS:
	      *status = 0; break;
	    case LEFT_BOUND_EXCEEDED: /* we do a special thing : for some p slightly
                                         upper exp(-*xlam) the inversion process fails
                                         because cumpoi is not computed enough accuratly
				      */
	      *status = 0; *s = 0.0; break;
	    case RIGHT_BOUND_EXCEEDED:
	      *status = 2; *bound = inf; break;
	    default:
	      *status = 4;
	    }
	}

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

