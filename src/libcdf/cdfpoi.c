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

/* rewritten and modified by Bruno Pincon and Jean-Philippe Chancelier (sept 2010) */

int
cdf_cdfpoi (int *which, double *p, double *q, double *s, double *xlam,
	    int *status, double *bound, double *boundbis)
{
  const double tol=1.0E-14, atol=1.0E-50, inf=1.0E300, zero=1e-300;
  int pq_flag = 1;
  double fx, cum, ccum;

  /* check parameters */
  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 3 , 3 , -1 );
 
  if (*which != 1)       /* check p and q */
    {
      if ( *which == 2 ) /* p=0 and q = 0 are possible */
	{
	  CDF_CHECK_PQ( 0.0 <= , <= 1.0 );
	}
      else
	{
	  CDF_CHECK_PQ( 0.0 < , <= 1.0 );
	}
      pq_flag = *p <= *q;
    }

  if (*which != 2 && *which != 1 ) /* add *which==1 to compute cdfpoi with *s < 0 (bruno april,12,2010)) */
    {
      CDF_CHECK_ARG( !(*s >= 0) , 0 , -4 );
    }

  if (*which != 3)       /* check xlam */
    {
      CDF_CHECK_ARG(!(*xlam >= 0) , 0 , -5 );  /* note: xlam=0 is a very special case  */
    }



  /*  Calculate ANSWERS */

  if (1 == *which)       /* compute (p,q) */
    {
      double sf = floor(*s);  /* add floor to compute the real cdfpoi (bruno april 2010)) */
      cdf_cumpoi (&sf, xlam, p, q);
      if ( *q > 1.5 ) /* this comes from gratio (when it is unable to compute the result the result is 2) */
	*status = 10;
      else
	*status = 0;
    }

  else if (2 == *which)  /* compute s */
    {
      /* first we take care of some special cases */
      if ( *xlam == 0.0 )   
	{
	  *status = 0; *s = 0.0;
	}
      else if ( *q == 0.0 )
	{
	  *status = 0; *s = 2.0*DBL_MAX; /* Inf (valid only if xlam > 0 but the case xlam=0 is set just before) */
	}
      else if ( *p <= exp(-*xlam) ) /* bruno april 2010 (using the jpc 's workaround of cdfbin) */
	{
	  *status = 0; *s = 0.0;
	}
      else /* inversion process for usual cases */
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;
	  double step = sqrt(*xlam); /* initial step = std */
	  *s = *xlam + 0.333;  /* start near the median instead of 5, bruno september 2010 */
	  nsp_zsearch_init(*s, 0.0, inf, step, 0.0, 2.0, atol, tol, pq_flag ?  INCREASING : DECREASING, &S);

	  do
	    {
	      cdf_cumpoi (s, xlam, &cum, &ccum);
	      if ( ccum > 1.5 ) /* this comes from gratio (when it is unable to compute the result the result is 2) */
		{
		  *status = 10; return 0;
		}
	      if ( pq_flag )
		fx = cum - *p;
	      else
		fx = ccum - *q;
	    }
	  while ( (ret_val = nsp_zsearch(s, fx, &S)) == EVAL_FX );

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
	      *status = 5;
	    }
	}
    }

  else if (3 == *which)   /* compute xlam */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *xlam = 5.;
      nsp_zsearch_init(*xlam, 0.0, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
	
      do
	{
	  cdf_cumpoi (s, xlam, &cum, &ccum);
	  if ( ccum > 1.5 ) /* this comes from gratio (when it is unable to compute the result it output 2) */
	    {
	      *status = 10; return 0;
	    }
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(xlam, fx, &S)) == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0; break;
	case BOTH_BOUND_EXCEEDED:
	  *status = 4; *bound = zero; *boundbis = inf; break;
	default:
	  *status = 5;
	}
    }
  return 0;
}		

