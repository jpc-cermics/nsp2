#include "cdf.h"

/* ********************************************************************** */
/*      SUBROUTINE CDFCHI( WHICH, P, Q, X, DF, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*               CHI-Square distribution */
/*                              Function */
/*     Calculates any one parameter of the chi-square */
/*     distribution given values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next three argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..3 */
/*               iwhich = 1 : Calculate P and Q from X and DF */
/*               iwhich = 2 : Calculate X from P,Q and DF */
/*               iwhich = 3 : Calculate DF from P,Q and X */
/*                    INT WHICH */
/*     P <--> The integral from 0 to X of the chi-square */
/*            distribution. */
/*            Input range: [0, 1]. */
/*                    DOUBLE PRECISION P */
/*     Q <--> 1-P. */
/*            Input range: (0, 1]. */
/*            P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*     X <--> Upper limit of integration of the non-central */
/*            chi-square distribution. */
/*            Input range: [0, +infinity). */
/*            Search range: [0,1E300] */
/*                    DOUBLE PRECISION X */
/*     DF <--> Degrees of freedom of the */
/*             chi-square distribution. */
/*             Input range: (0, +infinity). */
/*             Search range: [ 1E-300, 1E300] */
/*                    DOUBLE PRECISION DF */
/*     STATUS <-- 0 if calculation completed correctly */
/*               -I if input parameter number I is out of range */
/*                1 if answer appears to be lower than lowest */
/*                  search bound */
/*                2 if answer appears to be higher than greatest */
/*                  search bound */
/*                3 if P + Q .ne. 1 */
/*               10 indicates error returned from cumgam.  See */
/*                  references in cdfgam */
/*                    INT STATUS */
/*     BOUND <-- Undefined if STATUS is 0 */
/*               Bound exceeded by parameter number I if STATUS */
/*               is negative. */
/*               is negative. */
/*               Lower search bound if STATUS is 1. */
/*               Upper search bound if STATUS is 2. */
/*                              Method */
/*     Formula    26.4.19   of Abramowitz  and     Stegun, Handbook  of */
/*     Mathematical Functions   (1966) is used   to reduce the chisqure */
/*     distribution to the incomplete distribution. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */


/* rewritten by Bruno Pincon and Jean-Philippe Chancelier (sept/oct 2010) */


int cdf_cdfchi (int *which, double *p, double *q, double *x, double *df,
		int *status, double *bound, double *boundbis)
{
  const double tol=1.0E-14,atol=1.0E-50,zero=1.0E-300,inf=1.0E300;
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
      CDF_CHECK_ARG(!(*df > 0.0) , 0 , -5 );
    }

  /*     Calculate ANSWERS */

  if (1 == *which)         /* Compute P and Q */
    {
      cdf_cumchi (x, df, p, q);
      if (*p > 1.5 )  /* this comes from gratio (cumchi calls cumgam which calls gratio) */
	              /* when gratio is unable to compute the result the probability p */
                      /* is set to 2 */
	*status = 10;
      else
	*status = 0;
    }

  else if (2 == *which)    /* Compute X */
    {
      if ( *p == 0.0 )
	*x = 0.0;
      else if ( *q == 0.0 )
	*x =  2.0*DBL_MAX;   /* Inf */
      else
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;
	  double step = sqrt(2*(*df));  /* use std as initial step */
	  *x = *df;  /* start from the mean instead of 5, bruno april 2010 */
	  nsp_zsearch_init(*x, 0, inf, step, 0.0, 2.0, atol, tol, pq_flag ? INCREASING : DECREASING, &S);
	  do
	    {
	      cdf_cumchi (x, df, &cum, &ccum);
	      if (cum > 1.5 )  /* gratio unable to compute (see comment before) */
		{
		  *status = 10; return 0;
		}
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
	      *status = 1; *bound = 0.0; break;
	    case RIGHT_BOUND_EXCEEDED:
	      *status = 2; *bound = inf; break;
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
      nsp_zsearch_init(*df, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumchi (x, df, &cum, &ccum);
	  if (cum > 1.5 )  /* gratio unable to compute (see comment before) */
	    {
	      *status = 10; return 0;
	    }
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

  return 0;
}				/* cdfchi_ */
