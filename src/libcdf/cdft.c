/* ********************************************************************** */
/*      SUBROUTINE CDFT( WHICH, P, Q, T, DF, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*                         T distribution */
/*                              Function */
/*     Calculates any one parameter of the t distribution given */
/*     values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which  argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..3 */
/*               iwhich = 1 : Calculate P and Q from T and DF */
/*               iwhich = 2 : Calculate T from P,Q and DF */
/*               iwhich = 3 : Calculate DF from P,Q and T */
/*                    INT WHICH */
/*        P <--> The integral from -infinity to t of the t-density. */
/*              Input range: (0,1]. */
/*                    DOUBLE PRECISION P */
/*     Q <--> 1-P. */
/*            Input range: (0, 1]. */
/*            P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*        T <--> Upper limit of integration of the t-density. */
/*               Input range: ( -infinity, +infinity). */
/*               Search range: [ -1E150, 1E150 ] */
/*                    DOUBLE PRECISION T */
/*        DF <--> Degrees of freedom of the t-distribution. */
/*                Input range: (0 , +infinity). */
/*                Search range: [1e-300, 1E10] */
/*                    DOUBLE PRECISION DF */
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
/*     Formula  26.5.27  of   Abramowitz   and  Stegun,   Handbook   of */
/*     Mathematical Functions  (1966) is used to reduce the computation */
/*     of the cumulative distribution function to that of an incomplete */
/*     beta. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */
/* ********************************************************************** */

/* rewritten by Bruno Pincon and Jean-Philippe Chancelier (sept 2010) */

#include "cdf.h"

int
cdf_cdft (int *which, double *p, double *q, double *t, double *df,
	  int *status, double *bound, double *boundbis)
{
  const double tol=1.0E-14, atol=1.0E-50, zero=1.0E-300,rtinf=1.0E150, maxdf=1.0E10;
  double cum, ccum, fx;
  int pq_flag=1;   


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

  if (*which != 3)       /* check df */
    {
      CDF_CHECK_ARG( !(*df > 0.0) , 0 , -5 );
    }


  /* Calculate ANSWERS */
  if (1 == *which)         /* Compute P and Q */
    {
      cdf_cumt (t, df, p, q);
      *status = 0;
    }

  else if (2 == *which)    /* Compute T */
    {
      if ( *p == 0.0 )
	*t = -2.0*DBL_MAX;   /* -Inf */
      else if ( *q == 0.0 )
	*t =  2.0*DBL_MAX;   /* Inf */
      else if ( *p == 0.5 || *q == 0.5 ) /* 0 */
	*t = 0.0;
      else
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;

	  *t = cdf_dt1 (p, q, df);  /* Get initial approximation for T */
	  nsp_zsearch_init(*t, -rtinf, rtinf, 0.1, 0.1, 2.0, atol, tol, pq_flag ? INCREASING : DECREASING, &S);
	  do
	    {
	      cdf_cumt (t, df, &cum, &ccum);
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
	      *status = 1; *bound = -rtinf; break;
	    case RIGHT_BOUND_EXCEEDED:
	      *status = 2; *bound = rtinf; break;
	    default:
	      *status = 5;
	    }
	}
    }

  else if (3 == *which)    /*  Compute DF */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;

      *df = 5.;
      nsp_zsearch_init(*df, zero, maxdf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumt (t, df, &cum, &ccum);
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
	  *status = 4; *bound = zero; *boundbis = maxdf; break;
	default:
	  *status = 5;
	}
    }

  return 0;
}		

