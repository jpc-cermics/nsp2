#include "cdf.h"

/************************************************************************ 
 * 
 *     SUBROUTINE CDFTNC( WHICH, P, Q, T, DF, PNONC, STATUS, BOUND ) 
 *              Cumulative Distribution Function 
 *                 Non-Central T distribution 
 * 
 *                              Function 
 * 
 *    Calculates any one parameter of the noncentral t distribution give 
 *    values for the others. 
 * 
 *                              Arguments 
 * 
 *    WHICH --> Int indicating which  argument 
 *              values is to be calculated from the others. 
 *              Legal range: 1..3 
 *              iwhich = 1 : Calculate P and Q from T,DF,PNONC 
 *              iwhich = 2 : Calculate T from P,Q,DF,PNONC 
 *              iwhich = 3 : Calculate DF from P,Q,T 
 *              iwhich = 4 : Calculate PNONC from P,Q,DF,T 
 *                   INT WHICH 
 * 
 *       P <--> The integral from -infinity to t of the noncentral t-den 
 *             Input range: (0,1]. 
 *                   DOUBLE PRECISION P 
 * 
 *    Q <--> 1-P. 
 *           Input range: (0, 1]. 
 *           P + Q = 1.0. 
 *                   DOUBLE PRECISION Q 
 * 
 *       T <--> Upper limit of integration of the noncentral t-density. 
 *              Input range: ( -infinity, +infinity). 
 *              Search range: [ -1E100, 1E100 ] 
 *                   DOUBLE PRECISION T 
 * 
 *       DF <--> Degrees of freedom of the noncentral t-distribution. 
 *               Input range: (0 , +infinity). 
 *               Search range: [1e-100, 1E10] 
 *                   DOUBLE PRECISION DF 
 * 
 *    PNONC <--> Noncentrality parameter of the noncentral t-distributio 
 *               Input range: [-infinity , +infinity). 
 *               Search range: [-1e4, 1E4] 
 * 
 *    STATUS <-- 0 if calculation completed correctly 
 *              -I if input parameter number I is out of range 
 *               1 if answer appears to be lower than lowest 
 *                 search bound 
 *               2 if answer appears to be higher than greatest 
 *                 search bound 
 *               3 if P + Q .ne. 1 
 *                   INT STATUS 
 * 
 *    BOUND <-- Undefined if STATUS is 0 
 * 
 *              Bound exceeded by parameter number I if STATUS 
 *              is negative. 
 * 
 *              Lower search bound if STATUS is 1. 
 * 
 *              Upper search bound if STATUS is 2. 
 * 
 *                               Method 
 * 
 *    Upper tail    of  the  cumulative  noncentral t is calculated usin 
 *    formulae  from page 532  of Johnson, Kotz,  Balakrishnan, Coninuou 
 *    Univariate Distributions, Vol 2, 2nd Edition.  Wiley (1995) 
 * 
 *    Computation of other parameters involve a seach for a value that 
 *    produces  the desired  value  of P.   The search relies  on  the 
 *    monotinicity of P with the other parameter. 
 * 
 ************************************************************************ 
 */

/* rewritten by Bruno Pincon and Jean-Philippe Chancelier (sept 2010) */

int cdf_cdftnc (int *which, double *p, double *q, double *t, double *df,
		double *pnonc, int *status, double *bound, double *boundbis)
{
  double cum, ccum, fx;
  const double tol=1.0E-14, atol=1.0E-50;
  int pq_flag=1;   


  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );

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

  if (*which != 3)       /* check df */
    {
      CDF_CHECK_ARG( !(*df > 0.0) , 0 , -5 );
    }

  /* we could verify that pnonc is not Nan (for the moment output Nan in this case) */
  if (*which != 4  &&  isnan(*pnonc) )
    {
      if ( *which == 1 )
	{
	  *p = *q = *pnonc;
	}
      else if ( *which == 2 )
	{
	  *t = *pnonc;
	}
      else /* which == 3 */
	*df = *pnonc;
    }

    

  if (1 == *which)        /* Compute P and Q */
    {
      cdf_cumtnc (t, df, pnonc, p, q);
      *status = 0;
    }

  else if (2 == *which)    /* Compute T */
    {
      if ( *p == 0.0 )
	*t = -2.0*DBL_MAX;   /* -Inf */
      else if ( *q == 0.0 )
	*t =  2.0*DBL_MAX;   /* Inf */
      else
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;
	  double tinf = -1e100, tsup = 1e100; 

	  *t = *pnonc;    /* start near the mode */
	  nsp_zsearch_init(*t, tinf, tsup, 2.0, 0.1, 2.0, atol, tol, pq_flag ? INCREASING : DECREASING, &S);
	  do
	    {
	      cdf_cumtnc (t, df, pnonc, &cum, &ccum);
	      if ( pq_flag )
		fx = cum - *p;
	      else
		fx = ccum - *q;
	    }
	  while ( (ret_val = nsp_zsearch(t, fx, &S)) == EVAL_FX );
	  
	  switch ( ret_val )
	    {
	    case SUCCESS:
	      *status = 0; break;
	    case LEFT_BOUND_EXCEEDED:
	      *status = 1; *bound = tinf; break;
	    case RIGHT_BOUND_EXCEEDED:
	      *status = 2; *bound = tsup; break;
	    default:
	      *status = 5;
	    }
	}
    }

  else if (3 == *which)    /*  Compute DF */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      double dfinf = 1e-100, dfsup = 1e10;
      *df = 5.0;
      nsp_zsearch_init(*df, dfinf, dfsup, 2.0, 0.1, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumtnc (t, df, pnonc, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(df, fx, &S)) == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0; break;
	case BOTH_BOUND_EXCEEDED:
	  *status = 4; *bound = dfinf; *boundbis = dfsup; break;
	default:
	  *status = 5;
	}
 
    }

  else if (4 == *which)   /*  compute pnonc */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      double pninf = 1e-4, pnsup = 1e4;
      *pnonc = 5.0;
      nsp_zsearch_init(*pnonc, pninf, pnsup, 2.0, 0.1, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumtnc (t, df, pnonc, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(pnonc, fx, &S)) == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0; break;
	case BOTH_BOUND_EXCEEDED:
	  *status = 4; *bound = pninf; *boundbis = pnsup; break;
	default:
	  *status = 5;
	}
    }

  return 0;
}
