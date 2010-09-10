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
 */


int
cdf_cdfchn (int *which, double *p, double *q, double *x, double *df,
	    double *pnonc, int *status, double *bound)
{
  static double c_b15 = 0., c_b16 = .5, c_b18 = 5.;
  const double tent4=1.0E5, tol=1.0E-14, atol=1.0E-50;
  const double zero=1.0E-300, inf=1.0E300;
  double ccum, fx, cum;
  int qleft, qhi;

  /* 
   *  part 1: verify parameters 
   *  ------
   */

  if ( *which < 1 || *which > 4 )
    {
      *status = -1;
      *bound = (*which < 1) ? 1 : 4;
      return 0;
    }

  if (*which != 1)  /* test p and q */
    {
      double pq = *p + *q;

      if ( ! ( 0.0 < *p  &&  *p <= 1.0 ) )
	{
	  *status = -2;
	  if ( *p <= 0.0 )
	    *bound = 0.0;
	  else            /* rmk: Nan will be tracted here (so not the good message) */
	    *bound = 1.0; 
	  return 0;
	}

      if ( ! ( 0.0 < *q  &&  *q <= 1.0 ) )
	{
	  *status = -3;
	  if ( *q <= 0.0 )
	    *bound = 0.0;
	  else            /* rmk: Nan will be threaded here (so not the good message) */
	    *bound = 1.0;
	  return 0;   
	}

      if ( fabs(pq - 1.0) >= 0.5*DBL_EPSILON )
	{
	  *status = 3;
	  *bound = 1.0;
	  return 0;
	}
    }

  if ( *which != 2  &&  *which != 1 )   /* test *x (but also not for which == 1 as this case handles all values for x) */
    {
      if ( ! (*x >= 0.0) )
	{
	  *status = -4;
	  *bound = 0.0;
	  return 0;
	}
    }

  if ( *which != 3 )    /* test df */
    {
      if ( ! (*df > 0.0) )
	{
	  *bound = 0.0;
	  *status = -5;
	  return 0;
	}
    }

  if ( *which != 4 )   /* test pnonc */
    {
      if ( ! (*pnonc >= 0.0) )
	{
	  *bound = 0.;
	  *status = -6;
	  return 0;
	}
    }


  /* 
   *  part 2: calculate  answers
   *  ------
   */

  if (1 == *which)             /* Calculating P and Q */
    {
      cdf_cumchn_new (x, df, pnonc, p, q);
      *status = 0;
    }
  else if (2 == *which)        /* Calculating X */
    {
      int pq_flag = *p <= *q ? 1 : 0;
      ZsearchStruct S;
      zsearch_ret ret_val;
      zsearch_monotonicity monotonicity = pq_flag ? INCREASING : DECREASING;
      double step = sqrt(*df+2**pnonc);  /* a step proportionnal to the std */
      double zero = 0.0;
      double inc_step = 2;
      *x = *df + *pnonc;       /* start from the mean instead of 5  (bruno, april 2010) */ 
      nsp_zsearch_init(*x, 0, inf, step, zero, inc_step, atol, tol, monotonicity, &S);

      do
	{
	  cdf_cumchn_new (x, df, pnonc, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	  ret_val = nsp_zsearch(x, fx, &S);
	}
      while ( ret_val == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0;
	  break;
	case LEFT_BOUND_EXCEEDED:
	  *status = 1;
	  *bound = zero;
	  break;
	case RIGHT_BOUND_EXCEEDED:
	  *status = 2;
	  *bound = inf;
	  break;
	default:
	  *status = 4;
	}
    }

  else if (3 == *which)        /* Calculating DF */
    {
      int pq_flag = *p <= *q ? 1 : 0;
      *df = 5.;
      cdf_dstinv (&zero, &inf, &c_b16, &c_b16, &c_b18, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
      while ( *status == 1 )
	{
	  cdf_cumchn_new (x, df, pnonc, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	  cdf_dinvr (status, df, &fx, &qleft, &qhi);
	}

      if ( *status == -1 )
	{
	  if ( qleft )
	    {
	      *status = 1; *bound = zero;
	    }
	  else
	    {
	      *status = 2; *bound = inf;
	    }
	}
    }

  else if (4 == *which)        /* Calculating PNONC */
    {
      int pq_flag = *p <= *q ? 1 : 0;
      *pnonc = 5.;
      cdf_dstinv (&c_b15, &tent4, &c_b16, &c_b16, &c_b18, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, pnonc, &fx, &qleft, &qhi);
      while ( *status == 1 )
	{
	  cdf_cumchn_new (x, df, pnonc, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	  cdf_dinvr (status, pnonc, &fx, &qleft, &qhi);
	}

      if ( *status == -1 )
	{
	  if ( qleft )
	    {
	      *status = 1; *bound = zero;
	    }
	  else
	    {
	      *status = 2;
	      *bound = tent4;
	    }
	}
    }

  return 0;
}

