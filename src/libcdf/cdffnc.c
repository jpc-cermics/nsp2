/* ********************************************************************** */
/*      SUBROUTINE CDFFNC( WHICH, P, Q, F, DFN, DFD, PNONC, STATUS, BOUND */
/*               Cumulative Distribution Function */
/*               Non-central F distribution */
/*                              Function */
/*     Calculates any one parameter of the Non-central F */
/*     distribution given values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next five argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..5 */
/*               iwhich = 1 : Calculate P and Q from F,DFN,DFD and PNONC */
/*               iwhich = 2 : Calculate F from P,Q,DFN,DFD and PNONC */
/*               iwhich = 3 : Calculate DFN from P,Q,F,DFD and PNONC */
/*               iwhich = 4 : Calculate DFD from P,Q,F,DFN and PNONC */
/*               iwhich = 5 : Calculate PNONC from P,Q,F,DFN and DFD */
/*                    INT WHICH */
/*       P <--> The integral from 0 to F of the non-central f-density. */
/*              Input range: [0,1-1E-16). */
/*                    DOUBLE PRECISION P */
/*       Q <--> 1-P. */
/*            Q is not used by this subroutine and is only included */
/*            for similarity with other cdf* routines. */
/*                    DOUBLE PRECISION Q */
/*       F <--> Upper limit of integration of the non-central f-density. */
/*              Input range: [0, +infinity). */
/*              Search range: [0,1E300] */
/*                    DOUBLE PRECISION F */
/*     DFN < --> Degrees of freedom of the numerator sum of squares. */
/*               Input range: (0, +infinity). */
/*               Search range: [ 1E-300, 1E300] */
/*                    DOUBLE PRECISION DFN */
/*     DFD < --> Degrees of freedom of the denominator sum of squares. */
/*               Must be in range: (0, +infinity). */
/*               Input range: (0, +infinity). */
/*               Search range: [ 1E-300, 1E300] */
/*                    DOUBLE PRECISION DFD */
/*     PNONC <-> The non-centrality parameter */
/*               Input range: [0,infinity) */
/*               Search range: [0,1E4] */
/*                    DOUBLE PRECISION PHONC */
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
/*     Formula  26.6.20   of   Abramowitz   and   Stegun,  Handbook  of */
/*     Mathematical  Functions (1966) is used to compute the cumulative */
/*     distribution function. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */
/*                            WARNING */
/*     The computation time  required for this  routine is proportional */
/*     to the noncentrality  parameter  (PNONC).  Very large  values of */
/*     this parameter can consume immense  computer resources.  This is */
/*     why the search range is bounded by 10,000. */
/*                              WARNING */
/*     The  value  of the  cumulative  noncentral F distribution is not */
/*     necessarily monotone in either degrees  of freedom.  There  thus */
/*     may be two values that provide a given  CDF value.  This routine */
/*     assumes monotonicity  and will find  an arbitrary one of the two */
/*     values. */
/* ********************************************************************** */

#include "cdf.h"

/*   some changes by Bruno Pincon and Jpc (oct. 2010):
 *   - rewrite the function in a non spaghetti coding style 
 *   - use nsp_zsearch in place of dinvr
 *   - use some macros to simplify argument checking
 *   - use new cumulative distribution function which is also accurate for small q
 */


int
cdf_cdffnc (int *which, double *p, double *q, double *f, double *dfn,
	    double *dfd, double *phonc, int *status, double *bound, double *boundbis)
{
  const double tent4=1.0E4,tol=1.0E-14, atol=1.0E-50, zero=1.0E-300,inf=1.0E300;
  double cum, ccum, fx;
  int pq_flag = 1;

  /*  Verify parameters */

  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 5 , 5 , -1 );


  if (*which != 1)  /* test p and q */
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


  if ( *which != 2  &&  *which != 1 )   /* test f (but also not for which == 1 as this case handles all values for f) */
    {
      CDF_CHECK_ARG(!(*f >= 0.0), 0.0, -4);
    }

  if ( *which != 3 )    /* test dfn */
    {
      CDF_CHECK_ARG(!(*dfn > 0.0), 0.0, -5);
    }

  if ( *which != 4 )    /* test dfd */
    {
      CDF_CHECK_ARG(!(*dfd > 0.0), 0.0, -6);
    }

  if (*which != 5)      /* test phonc */
    {
      CDF_CHECK_ARG(!(*phonc >= 0.0) , 0 , -7 );
    }


  /*     Calculate ANSWERS */

  if (1 == *which)         /* compute p */
    {
      nsp_cdf_cumfnc (*f, *dfn, *dfd, *phonc, p, q);
      *status = 0;
    }

  else if (2 == *which)    /* compute f */
    {
      if ( *p == 0.0 )
	*f = 0.0;
      else if ( *q == 0.0 )
	*f =  2.0*DBL_MAX;   /* Inf */
      else
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;
	  *f = 5.0;
	  nsp_zsearch_init(*f, 0.0, inf, 2.0, 0.0, 2.0, atol, tol, pq_flag ? INCREASING : DECREASING, &S);
	  do
	    {
	      nsp_cdf_cumfnc (*f, *dfn, *dfd, *phonc, &cum, &ccum);
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
	      *status = 1; *bound = 0.0; break;
	    case RIGHT_BOUND_EXCEEDED:
	      *status = 2; *bound = inf; break;
	    default:
	      *status = 5;
	    }
	}
    }

  else if (3 == *which)    /* compute dfn */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *dfn = 5.0;
      nsp_zsearch_init(*dfn, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  nsp_cdf_cumfnc (*f, *dfn, *dfd, *phonc, &cum, &ccum);
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

  else if (4 == *which)    /* compute dfd */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *dfd = 5.0;
      nsp_zsearch_init(*dfd, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  nsp_cdf_cumfnc (*f, *dfn, *dfd, *phonc, &cum, &ccum);
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

  else if (5 == *which)    /* compute phonc */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *phonc = 5.0;
      nsp_zsearch_init(*phonc, 0.0, tent4, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  nsp_cdf_cumfnc (*f, *dfn, *dfd, *phonc, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(phonc, fx, &S)) == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0; break;
	case BOTH_BOUND_EXCEEDED:
	  *status = 4; *bound = zero; *boundbis = tent4; break;
	default:
	  *status = 5;
	}
    }

  return 0;
}				/* cdffnc_ */
