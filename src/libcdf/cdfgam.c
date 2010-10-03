#include "cdf.h"

/* ********************************************************************** */
/*      SUBROUTINE CDFGAM( WHICH, P, Q, X, SHAPE, RATE, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*                         GAMma Distribution */
/*                              Function */
/*     Calculates any one parameter of the gamma */
/*     distribution given values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next four argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..4 */
/*               iwhich = 1 : Calculate P and Q from X,SHAPE and RATE */
/*               iwhich = 2 : Calculate X from P,Q,SHAPE and RATE */
/*               iwhich = 3 : Calculate SHAPE from P,Q,X and RATE */
/*               iwhich = 4 : Calculate RATE from P,Q,X and SHAPE */
/*                    INT WHICH */
/*     P <--> The integral from 0 to X of the gamma density. */
/*            Input range: [0,1]. */
/*                    DOUBLE PRECISION P */
/*     Q <--> 1-P. */
/*            Input range: (0, 1]. */
/*            P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*     X <--> The upper limit of integration of the gamma density. */
/*            Input range: [0, +infinity). */
/*            Search range: [0,1E300] */
/*                    DOUBLE PRECISION X */
/*     SHAPE <--> The shape parameter of the gamma density. */
/*                Input range: (0, +infinity). */
/*                Search range: [1E-300,1E300] */
/*                  DOUBLE PRECISION SHAPE */
/*     RATE <--> The rate parameter of the gamma density. */
/*                Input range: (0, +infinity). */
/*                Search range: (1E-300,1E300] */
/*                   DOUBLE PRECISION RATE */
/*     STATUS <-- 0 if calculation completed correctly */
/*               -I if input parameter number I is out of range */
/*                1 if answer appears to be lower than lowest */
/*                  search bound */
/*                2 if answer appears to be higher than greatest */
/*                  search bound */
/*                3 if P + Q .ne. 1 */
/*                10 if the gamma or inverse gamma routine cannot */
/*                   compute the answer.  Usually happens only for */
/*                   X and SHAPE very large (gt 1E10 or more) */
/*                    INT STATUS */
/*     BOUND <-- Undefined if STATUS is 0 */
/*               Bound exceeded by parameter number I if STATUS is negative. */
/*               Lower search bound if STATUS is 1. */
/*               Upper search bound if STATUS is 2. */
/*                              Method */
/*     Cumulative distribution function (P) is calculated directly by */
/*     the code associated with: */
/*     DiDinato, A. R. and Morris, A. H. Computation of the  incomplete */
/*     gamma function  ratios  and their  inverse.   ACM  Trans.  Math. */
/*     Softw. 12 (1986), 377-393. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */
/*                              Note */
/*     The gamma density is proportional to */
/*       T**(SHAPE - 1) * EXP(- RATE * T) */
/*                              History */

/* rewritten by Bruno Pincon and Jean-Philippe Chancelier (sept/oct 2010) */

int
cdf_cdfgam (int *which, double *p, double *q, double *x, double *shape,
	    double *rate, int *status, double *bound, double *boundbis)
{
  const double tol=1.0E-14, atol=1.0E-50, zero=1.0E-300,inf=1.0E300;
  double xrate, cum, ccum, fx, x0=0.0;
  int pq_flag=1, ierr;   

  /***  Check parameters  ***/

  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );
 
  if (*which != 1)   /* test p and q */
    {
      if ( *which == 2 ) /* p=0 or q=0 are possible */
	{
	  CDF_CHECK_PQ( 0.0 <= , <= 1.0 );
	}
      else
	{
	  CDF_CHECK_PQ( 0.0 < , <= 1.0 );
	}
       pq_flag = *p <= *q;
    }

  if (*which != 2 && *which != 1 )  /* test x */
    {
      /* add *which==1 to compute cdfgam with *x < 0 (bruno march,22,2010)) */
      CDF_CHECK_ARG( !(*x  > 0.0) , 0 , -4 );
    }

  if (*which != 3)         /* test shape */
    {
      CDF_CHECK_ARG( !(*shape > 0.0) , 0 , -5 );
    }

  if (*which != 4)         /* test rate */
    {
      CDF_CHECK_ARG( !(*rate > 0.0) , 0 , -6 );
    }



  /***  Compute answers ***/

  if (1 == *which)        /* compute P and Q */
    {
      xrate = *x * *rate;
      cdf_cumgam (&xrate, shape, p, q);
      if (*p > 1.5)   /* this comes from gratio (cumgam calls gratio) when gratio */
	              /* is unable to compute the result the probability p is set to 2 */
	*status = 10;
      else
	*status = 0;
    }

  else if (2 == *which)   /* compute X */
    {
      double x0 = 0.0;
      cdf_gaminv (shape, &xrate, &x0, p, q, &ierr);
      if ( ierr < 0 )
	*status = 10;
      else
	{
	  *x = xrate / *rate; *status = 0;
	}
    }

  else if (3 == *which)   /* compute shape */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *shape = 5.0;
      xrate = *x * *rate;
      nsp_zsearch_init(*shape, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumgam (&xrate, shape, &cum, &ccum);
	  if ( cum > 1.5 ) /* error code from gratio */
	    {
	      *status = 10; return 0;
	    }
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(shape, fx, &S)) == EVAL_FX );

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

  else if (4 == *which)   /* compute rate */
    {
      cdf_gaminv (shape, &xrate, &x0, p, q, &ierr);
      if ( ierr < 0 )
	*status = 10;
      else
	{
	  *rate = xrate / *x; *status = 0;
	}
    }
  return 0;
}				/* cdfgam_ */
