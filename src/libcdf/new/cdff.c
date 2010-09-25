/* ********************************************************************** */
/*      SUBROUTINE CDFF( WHICH, P, Q, F, DFN, DFD, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*               F distribution */
/*                              Function */
/*     Calculates any one parameter of the F distribution */
/*     given values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next four argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..4 */
/*               iwhich = 1 : Calculate P and Q from F,DFN and DFD */
/*               iwhich = 2 : Calculate F from P,Q,DFN and DFD */
/*               iwhich = 3 : Calculate DFN from P,Q,F and DFD */
/*               iwhich = 4 : Calculate DFD from P,Q,F and DFN */
/*                    INT WHICH */
/*       P <--> The integral from 0 to F of the f-density. */
/*              Input range: [0,1]. */
/*                    DOUBLE PRECISION P */
/*       Q <--> 1-P. */
/*              Input range: (0, 1]. */
/*              P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*       F <--> Upper limit of integration of the f-density. */
/*              Input range: [0, +infinity). */
/*              Search range: [0,1E300] */
/*                    DOUBLE PRECISION F */
/*     DFN < --> Degrees of freedom of the numerator sum of squares. */
/*               Input range: (0, +infinity). */
/*               Search range: [ 1E-300, 1E300] */
/*                    DOUBLE PRECISION DFN */
/*     DFD < --> Degrees of freedom of the denominator sum of squares. */
/*               Input range: (0, +infinity). */
/*               Search range: [ 1E-300, 1E300] */
/*                    DOUBLE PRECISION DFD */
/*     STATUS <-- 0 if calculation completed correctly */
/*               -I if input parameter number I is out of range */
/*                1 if answer appears to be lower than lowest */
/*                  search bound */
/*                2 if answer appears to be higher than greatest */
/*                  search bound */
/*                3 if P + Q .ne. 1 */
/*                    INT STATUS */
/*     BOUND <-- Undefined if STATUS is 0 */
/*               Bound exceeded by parameter number I if STATUS */
/*               is negative. */
/*               Lower search bound if STATUS is 1. */
/*               Upper search bound if STATUS is 2. */
/*                              Method */
/*     Formula   26.6.2   of   Abramowitz   and   Stegun,  Handbook  of */
/*     Mathematical  Functions (1966) is used to reduce the computation */
/*     of the  cumulative  distribution function for the  F  variate to */
/*     that of an incomplete beta. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */
/*                              WARNING */
/*     The value of the  cumulative  F distribution is  not necessarily */
/*     monotone in  either degrees of freedom.  There  thus may  be two */
/*     values  that  provide a given CDF  value.   This routine assumes */
/*     monotonicity and will find an arbitrary one of the two values. */
/* ********************************************************************** */

/*   some changes by Bruno Pincon (sept. 2010):
 *   - rewrite the function in a non spaghetti coding style 
 *   - use nsp_zsearch in place of dinvr
 *
 */


#include "cdf.h"


int
cdf_cdff (int *which, double *p, double *q, double *f, double *dfn,
	  double *dfd, int *status, double *bound, double *boundbis)
{
  const double tol=1.0e-14, atol=1.0e-50, zero=1.0e-300, inf=1.0e300;
  double cum, ccum;
  int pq_flag=1;   
  double fx;

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
      
      pq_flag = *p <= *q;
    }

  if ( *which != 2  &&  *which != 1 )   /* test f (but also not for which == 1 as this case handles all values for f) */
    {
      if ( ! (*f >= 0.0) )
	{
	  *status = -4;
	  *bound = 0.0;
	  return 0;
	}
    }

  if ( *which != 3 )    /* test dfn */
    {
      if ( ! (*dfn > 0.0) )
	{
	  *bound = 0.0;
	  *status = -5;
	  return 0;
	}
    }

  if ( *which != 4 )    /* test dfd */
    {
      if ( ! (*dfd > 0.0) )
	{
	  *bound = 0.0;
	  *status = -6;
	  return 0;
	}
    }


  /*     Calculate ANSWERS */

  if (1 == *which)        /* Calculating P and Q */
    {
      cdf_cumf (f, dfn, dfd, p, q);
      *status = 0;
    }

  else if (2 == *which)   /* Calculating F */
    { 
      ZsearchStruct S;
      zsearch_ret ret_val;
      double step;
      if ( *dfd > 4.0 )    /* in this case start from the mean and use step_init = std */
	{
	  *f = *dfd / (*dfd - 2.0 );
	  step = *f*sqrt( 2.0*(*dfn+ *dfd -2.0)/(*dfn*(*dfd - 4.0)) );
	}
      else
	{
	  *f = 5.; step = 2.0;
	}

      nsp_zsearch_init(*f, zero, inf, step, 0.0, 2.0, atol, tol, pq_flag ? INCREASING : DECREASING, &S);
      do
	{
	  cdf_cumf (f, dfn, dfd, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(f, fx, &S)) == EVAL_FX );

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

  else if (3 == *which)   /* Calculating DFN */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *dfn = 5.0;
      nsp_zsearch_init(*dfn, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumf (f, dfn, dfd, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(dfn, fx, &S)) == EVAL_FX );

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

  else if (4 == *which)    /* Calculating DFD */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *dfd = 5.0;
      nsp_zsearch_init(*dfd, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumf (f, dfn, dfd, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(dfd, fx, &S)) == EVAL_FX );

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
}				/* cdff_ */
