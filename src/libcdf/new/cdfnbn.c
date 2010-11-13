#include "cdf.h"

/**
 * Cumulative Distribution Function of Negative BiNomial distribution 
 *
 * Calculates any one parameter of the negative binomial 
 * distribution given values for the others. 
 * The  cumulative  negative   binomial  distribution  returns  the 
 * probability that there  will be  F or fewer failures before  the 
 * XNth success in binomial trials each of which has probability of 
 * success PR. 
 * The individual term of the negative binomial is the probability of 
 * S failures before XN successes and is 
 *      Choose( S, XN+S-1 ) * PR^(XN) * (1-PR)^S 
 * 
 * WHICH --> Int indicating which of the next four argument 
 *           values is to be calculated from the others. 
 *           Legal range: 1..4 
 *           iwhich = 1 : Calculate P and Q from S,XN,PR and OMPR 
 *           iwhich = 2 : Calculate S from P,Q,XN,PR and OMPR 
 *           iwhich = 3 : Calculate XN from P,Q,S,PR and OMPR 
 *           iwhich = 4 : Calculate PR and OMPR from P,Q,S and XN 
 * P <--> The cumulation from 0 to S of the  negative 
 *        binomial distribution. 
 *        Input range: [0,1]. 
 * Q <--> 1-P. 
 *        Input range: (0, 1]. 
 *        P + Q = 1.0. 
 * S <--> The upper limit of cumulation of the binomial distribution. 
 *        There are F or fewer failures before the XNth success. 
 *        Input range: [0, +infinity). 
 *        Search range: [0, 1E300] 
 * XN  <--> The number of successes. 
 *          Input range: [0, +infinity). 
 *          Search range: [0, 1E300] 
 * PR  <--> The probability of success in each binomial trial. 
 *          Input range: [0,1]. 
 *          Search range: [0,1]. 
 * OMPR  <--> 1-PR 
 *          Input range: [0,1]. 
 *          Search range: [0,1] 
 *          PR + OMPR = 1.0 
 * STATUS <-- 0 if calculation completed correctly 
 *           -I if input parameter number I is out of range 
 *            1 if answer appears to be lower than lowest 
 *              search bound 
 *            2 if answer appears to be higher than greatest 
 *              search bound 
 *            3 if P + Q .ne. 1 
 *            4 if PR + OMPR .ne. 1 
 * BOUND <-- Undefined if STATUS is 0 
 *           Bound exceeded by parameter number I if STATUS 
 *           is negative. 
 *           Lower search bound if STATUS is 1. 
 *           Upper search bound if STATUS is 2. 
 * 
 * Formula   26.5.26   of   Abramowitz  and  Stegun,  Handbook   of 
 * Mathematical Functions (1966) is used  to  reduce calculation of 
 * the cumulative distribution  function to that of  an  incomplete 
 * beta. 
 * Computation of other parameters involve a seach for a value that 
 * produces  the desired  value  of P.   The search relies  on  the 
 * monotinicity of P with the other parameter. 
 **/

/* rewritten and modified by Bruno Pincon and Jean-Philippe Chancelier (sept-nov 2010) */

int
cdf_cdfnbn (int *which, double *p, double *q, double *s, double *xn,
	    double *pr, double *ompr, int *status, double *bound, double *boundbis)
{
  const  double tol = 1e-14, atol=1e-50, zero=1.0E-300, inf=1.0E300;
  double cum, ccum, fx;
  int pq_flag=1;

  /*** Check parameter ***/

  /* check which */
  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );
 
  if (*which != 1)   /* check p and q */
    {
      if ( *which == 2 )   /* p=0 or q=0 are possible */
	{
	  CDF_CHECK_PQ( 0.0 <= , <= 1.0 );
	}
      else
	{
	  CDF_CHECK_PQ( 0.0 < , <= 1.0 );
	}
       pq_flag = *p <= *q;
    }


  if (*which != 2 && *which != 1 ) /* check s : range for s is extented for which==1 (bruno march,22,2010)) */ 
    {
      CDF_CHECK_ARG( !(*s >= 0.0) , 0 , -4 );
    }

  if (*which != 3)   /* check xn */
    {
      CDF_CHECK_ARG( !(*xn  >= 0) , 0 , -5 );
    }

  if (*which != 4)   /* check pr and ompr: the case ompr=0 (that is pr = 1 in exact arithmetic) 
                        corresponds to a constant random variable equal to 0 */
    {
      CDF_CHECK_ARG( !(*pr > 0) , 0 , -6 );
      CDF_CHECK_ARG( !(*ompr >= 0) , 0 , -7 );
      CDF_CHECK_ARG( !(*pr <= 1) , 1 , -6 );
      CDF_CHECK_ARG( !(*ompr <= 1) , 1 , -7 );
      CDF_CHECK_ARG( fabs((*pr+*ompr) -1.0) >= 0.5*DBL_EPSILON , 1.0, 4);
    }


  /****  Calculate ANSWERS  ****/
  
  if (1 == *which)               /* compute (P,Q) */      
    {
      double sf = floor(*s);  /* add floor to compute the real cdfnbn (bruno march,22,2010)) */
      cdf_cumnbn (&sf, xn, pr, ompr, p, q);
      *status = 0;
    }

  else if (2 == *which)         /* compute S */
    {
      /* bruno april 2010 (use jpc 's workaround of cdfbin) */
      double p0, q0, s0=0.0;
      cdf_cumnbn(&s0, xn, pr, ompr, &p0, &q0);
      if ( *p <= p0 )
	{
	  *status = 0; *s =0.0;
	}
      else if ( *q == 0.0 )
	{
	  *status = 0; *s = 2.0*DBL_MAX;  /* Inf */
	}
      else
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;
	  double step;
	  *s = *xn *(*pr/(*ompr));  /* start from the mean instead of 5, bruno april 2010 */
	  step = sqrt( *s/(*ompr) );  /* initial step = std */

	  nsp_zsearch_init(*s, 0.0, inf, step, 0.0, 2.0, atol, tol, pq_flag ?  INCREASING : DECREASING, &S);
	  do
	    {
	      cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
	      if ( pq_flag )
		fx = cum - *p;
	      else
		fx = ccum - *q;
	    }
	  while ( (ret_val = nsp_zsearch(s, fx, &S)) == EVAL_FX );

	  switch ( ret_val )
	    {
	    case SUCCESS:
	      *status = 0; 
	      break;
	    case LEFT_BOUND_EXCEEDED:
	      *status = 1; *bound = 0.0; break;
	    case RIGHT_BOUND_EXCEEDED:
	      *status = 2; *bound = inf; break;
	    default:
	      *status = 5;
	    }
	}
    }

  else if (3 == *which)         /* compute Xn */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;

      *xn = Max ( 1.0 , Min( (*s)*(*ompr)/(*pr), inf));   /* start from s*(1-pr)/pr instead of 5 (bruno, nov 2010) */

      nsp_zsearch_init(*xn, zero, inf, 2.0, 0.0, 2.0, atol, tol, pq_flag ? DECREASING : INCREASING, &S);
      do
	{
	  cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(xn, fx, &S)) == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0; break;
	case LEFT_BOUND_EXCEEDED:
	  *status = 1; *bound = zero; break;
	case RIGHT_BOUND_EXCEEDED:
	  *status = 2; *bound = inf; break;
	default:
	  *status = 5;
	}
    }

  else if (4 == *which)        /* compute pr and ompr */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      /* decide if we compute pr or ompr */
      /*(this is important for pr << 1 (ompr near 1) or ompr << 1 (ompr near 0)) */
      /* for that purpose we use the function pr -> cum_probability (with S and Xn fixed) which is increasing */
      /* caution: this is due to the definition used for negative binomial distribution (and there are others definitions) */
      /* So if cum_probability(0.5) >= p then pr <= 0.5 and we compute pr otherwise we compute ompr */
      *pr = *ompr = 0.5;
      cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
      
      if ( cum >= *p ) /* compute pr */
	{
	  *pr = 0.4;
	  nsp_zsearch_init(*pr, zero, 1.0, 0.1, 0.0, 1.0, atol, tol,  pq_flag ?  INCREASING : DECREASING, &S);
	  do
	    {
	      *ompr = 1.0 - *pr;
	      cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
	      if ( pq_flag )
		fx = cum - *p;
	      else
		fx = ccum - *q;
	    }
	  while ( (ret_val = nsp_zsearch(pr, fx, &S)) == EVAL_FX );
	  *ompr = *pr;
	  
	  switch ( ret_val )
	    {
	    case SUCCESS:
	      *status = 0; break;
	    case LEFT_BOUND_EXCEEDED:   /* this should not occur (or may be with Nan in the computation) */
	      *status = 1; *bound = zero; break;
	    case RIGHT_BOUND_EXCEEDED:  /* this should not occur (or may be with Nan in the computation) */
	      *status = 2; *bound = 1.0; break;
	    default:                    /* this should not occur (or may be with Nan in the computation) */
	      *status = 5;
	    }
	}
      else             /* compute ompr */
	{
	  *ompr = 0.4;
	  nsp_zsearch_init(*ompr, zero, 1.0, 0.1, 0.0, 1.0, atol, tol,  pq_flag ? DECREASING : INCREASING, &S);
	  do
	    {
	      *pr = 1.0 - *ompr;
	      cdf_cumnbn (s, xn, pr, ompr, &cum, &ccum);
	      if ( pq_flag )
		fx = cum - *p;
	      else
		fx = ccum - *q;
	    }
	  while ( (ret_val = nsp_zsearch(ompr, fx, &S)) == EVAL_FX );
	  *pr = 1.0 - *ompr;
	  
	  switch ( ret_val )
	    {
	    case SUCCESS:
	      *status = 0; 
	      break;
	    case LEFT_BOUND_EXCEEDED:   /* this should not occur (or may be with Nan in the computation) */
	      *status = 1; *bound = zero; break;
	    case RIGHT_BOUND_EXCEEDED:  /* this should not occur (or may be with Nan in the computation) */
	      *status = 2; *bound = 1.0; break;
	    default:                    /* this should not occur (or may be with Nan in the computation) */
	      *status = 5;
	    }
	}
     }
  return 0;
}	
