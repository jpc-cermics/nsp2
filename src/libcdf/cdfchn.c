#include <limits.h>
#include "cdf.h"


/*------------------------------------------------------------------------
 *      SUBROUTINE CDFCHN( WHICH, P, Q, X, DF, PNONC, STATUS, BOUND ) 
 *               Cumulative Distribution Function 
 *               Non-central Chi-Square 
 *                              Function 
 *     Calculates any one parameter of the non-central chi-square 
 *     distribution given values for the others. 
 *                              Arguments 
 *     WHICH --> Int indicating which of the next three argument 
 *               values is to be calculated from the others. 
 *               Input range: 1..4 
 *               iwhich = 1 : Calculate P and Q from X and DF 
 *               iwhich = 2 : Calculate X from P,DF and PNONC 
 *               iwhich = 3 : Calculate DF from P,X and PNONC 
 *               iwhich = 3 : Calculate PNONC from P,X and DF 
 *                    INT WHICH 
 *     P <--> The integral from 0 to X of the non-central chi-square 
 *            distribution. 
 *            Input range: [0, 1-1E-16). 
 *                    DOUBLE PRECISION P 
 *     Q <--> 1-P. 
 *            Q is not used by this subroutine and is only included 
 *            for similarity with other cdf* routines. 
 *            Note (bruno april 2010): try to use Q now as cumchn_new is accurate for Q
 *                    DOUBLE PRECISION Q 
 *     X <--> Upper limit of integration of the non-central 
 *            chi-square distribution. 
 *            Input range: [0, +infinity). 
 *            Search range: [0,1E300] 
 *                    DOUBLE PRECISION X 
 *     DF <--> Degrees of freedom of the non-central 
 *             chi-square distribution. 
 *             Input range: (0, +infinity). 
 *             Search range: [ 1E-300, 1E300] 
 *                    DOUBLE PRECISION DF 
 *     PNONC <--> Non-centrality parameter of the non-central 
 *                chi-square distribution. 
 *                Input range: [0, +infinity). 
 *                Search range: [0,1E4] 
 *                    DOUBLE PRECISION PNONC 
 *     STATUS <-- 0 if calculation completed correctly 
 *               -I if input parameter number I is out of range 
 *                1 if answer appears to be lower than lowest 
 *                  search bound 
 *                2 if answer appears to be higher than greatest 
 *                  search bound 
 *                    INT STATUS 
 *     BOUND <-- Undefined if STATUS is 0 
 *               Bound exceeded by parameter number I if STATUS 
 *               is negative. 
 *               Lower search bound if STATUS is 1. 
 *               Upper search bound if STATUS is 2. 
 *                              Method 
 *     Formula  26.4.25   of   Abramowitz   and   Stegun,  Handbook  of 
 *     Mathematical  Functions (1966) is used to compute the cumulative 
 *     distribution function. 
 *     Computation of other parameters involve a seach for a value that 
 *     produces  the desired  value  of P.   The search relies  on  the 
 *     monotinicity of P with the other parameter. 
 *                            WARNING 
 *     The computation time  required for this  routine is proportional 
 *     to the noncentrality  parameter  (PNONC).  Very large  values of 
 *     this parameter can consume immense  computer resources.  This is 
 *     why the search range is bounded by 10,000. 
 *----------------------------------------------------------------------*/


/*   some changes by Bruno Pincon (april 2010):
 *   - use cdf_cumchn_new (instead of cdf_cumchn)
 *   - rewrite the function in a non spaghetti coding style 
 *   - use the fact that q is computed accurately with cumchn_new in
 *     the inversion processes (which == 2|3|4)  
 *
 */


int
cdf_cdfchn (int *which, double *p, double *q, double *x, double *df,
	    double *pnonc, int *status, double *bound, double *boundbis)
{
  const double tol=1.0e-14, atol=1.0e-50, zero=1.0e-300, inf=1.0e300;
  double ccum, fx, cum;
  int pq_flag=1;  

  /*  Verify parameters */

  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );


  if (*which != 1)  /* test p and q */
    {
      if ( *which == 2 ) /* p=0 or q = 0 are possible */
	{
	  CDF_CHECK_PQ( 0.0 <= , <= 1.0 );
	}
      else
	{
	  CDF_CHECK_PQ( 0.0 < , <= 1.0 );
	}
       pq_flag = *p <= *q;
    }


  if ( *which != 2  &&  *which != 1 )   /* test *x (but also not for which == 1 as this case handles all values for x) */
    {
      CDF_CHECK_ARG(!(*x >= 0.0), 0.0, -4);
    }

  if ( *which != 3 )    /* test df */
    {
      CDF_CHECK_ARG(!(*df > 0.0), 0.0, -5);
    }

  if ( *which != 4 )   /* test pnonc */
    {
      CDF_CHECK_ARG(!(*pnonc >= 0.0), 0.0, -6);
    }


  /*  Calculate ANSWERS  */

  if (1 == *which)             /* Calculating P and Q */
    {
      nsp_cdf_cumchn (*x, *df, *pnonc, p, q);
      *status = 0;
    }
  else if (2 == *which)        /* Calculating X */
    {
      if ( *p == 0.0 )
	*x = 0.0;
      else if ( *q == 0.0 )
	*x =  2.0*DBL_MAX;   /* Inf */
      else
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;
	  double step = sqrt(*df+2**pnonc);  /* a step proportionnal to the std */
	  *x = *df + *pnonc;       /* start from the mean instead of 5  (bruno, april 2010) */ 

	  nsp_zsearch_init(*x, 0, inf, step, 0.0, 2.0, atol, tol, pq_flag ? INCREASING : DECREASING, &S);

	  do
	    {
	      nsp_cdf_cumchn (*x, *df, *pnonc, &cum, &ccum);
	      if ( pq_flag )
		fx = cum - *p;
	      else
		fx = ccum - *q;
	    }
	  while ( (ret_val = nsp_zsearch(x, fx, &S)) == EVAL_FX );
	  
	  switch ( ret_val )
	    {
	    case SUCCESS:
	      *status = 0; break;
	    case LEFT_BOUND_EXCEEDED:
	      *status = 1; *bound = zero; break;
	    case RIGHT_BOUND_EXCEEDED:
	      *status = 2; *bound = inf; break;
	    default:
	      *status = 4;
	    }
	}
    }

  else if (3 == *which)        /* Calculating DF */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *df = 5.;
      nsp_zsearch_init(*df, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  nsp_cdf_cumchn (*x, *df, *pnonc, &cum, &ccum);
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
	  *status = 4; *bound = zero; *boundbis = inf; break;
	default:
	  *status = 5;
	}
    }
  
  else if (4 == *which)        /* Calculating PNONC */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      double pnoncmax = 1e5;
      *pnonc = 5.;
      nsp_zsearch_init(*pnonc, 0.0, pnoncmax, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  nsp_cdf_cumchn (*x, *df, *pnonc, &cum, &ccum);
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
	  *status = 4; *bound = 0.0; *boundbis = pnoncmax; break;
	default:
	  *status = 5;
	}
    }

  return 0;
}

