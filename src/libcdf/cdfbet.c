#include "cdf.h"


/* ********************************************************************** */
/*      SUBROUTINE CDFBET( WHICH, P, Q, X, Y, A, B, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*                         BETa Distribution */
/*                              Function */
/*     Calculates any one parameter of the beta distribution given */
/*     values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next four argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..4 */
/*               iwhich = 1 : Calculate P and Q from X,Y,A and B */
/*               iwhich = 2 : Calculate X and Y from P,Q,A and B */
/*               iwhich = 3 : Calculate A from P,Q,X,Y and B */
/*               iwhich = 4 : Calculate B from P,Q,X,Y and A */
/*                    INT WHICH */
/*     P <--> The integral from 0 to X of the chi-square */
/*            distribution. */
/*            Input range: [0, 1]. */
/*                    DOUBLE PRECISION P */
/*     Q <--> 1-P. */
/*            Input range: [0, 1]. */
/*            P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*     X <--> Upper limit of integration of beta density. */
/*            Input range: [0,1]. */
/*            Search range: [0,1] */
/*                    DOUBLE PRECISION X */
/*     Y <--> 1-X. */
/*            Input range: [0,1]. */
/*            Search range: [0,1] */
/*            X + Y = 1.0. */
/*                    DOUBLE PRECISION Y */
/*     A <--> The first parameter of the beta density. */
/*            Input range: (0, +infinity). */
/*            Search range: [1E-300,1D300] */
/*                    DOUBLE PRECISION A */
/*     B <--> The second parameter of the beta density. */
/*            Input range: (0, +infinity). */
/*            Search range: [1E-300,1D300] */
/*                    DOUBLE PRECISION B */
/*     STATUS <-- 0 if calculation completed correctly */
/*               -I if input parameter number I is out of range */
/*                1 if answer appears to be lower than lowest */
/*                  search bound */
/*                2 if answer appears to be higher than greatest */
/*                  search bound */
/*                3 if P + Q .ne. 1 */
/*                4 if X + Y .ne. 1 */
/*                    INT STATUS */
/*     BOUND <-- Undefined if STATUS is 0 */
/*               Bound exceeded by parameter number I if STATUS */
/*               is negative. */
/*               Lower search bound if STATUS is 1. */
/*               Upper search bound if STATUS is 2. */
/*                              Method */
/*     Cumulative distribution function  (P)  is calculated directly by */
/*     code associated with the following reference. */
/*     DiDinato, A. R. and Morris,  A.   H.  Algorithm 708: Significant */
/*     Digit Computation of the Incomplete  Beta  Function Ratios.  ACM */
/*     Trans. Math.  Softw. 18 (1993), 360-373. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */
/*                              Note */
/*     The beta density is proportional to */
/*               t^(A-1) * (1-t)^(B-1) */

/* rewritten by Bruno Pincon and Jean-Philippe Chancelier (sept/oct 2010) */

int cdf_cdfbet (int *which, double *p, double *q, double *x, double *y, double *a,
		double *b, int *status, double *bound, double *boundbis)
{
  const double tol=1.0E-14, atol=1.0E-120,zero=1.0E-300,inf=1.0E300;
  double fx, ccum, cum;
  int pq_flag=1;

  /*** Check parameter ***/

  /* check which */
  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );
 
  if (*which != 1)   /* check p and q */
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

  if ( *which != 2  &&  *which != 1 )  /* check x and y */
    {
      CDF_CHECK_ARG( !(*x > 0.0) , 0 , -4 );
      CDF_CHECK_ARG( !(*x <= 1.0) , 1 , -4 );

      CDF_CHECK_ARG( !(*y > 0.0) , 0 , -5 );
      CDF_CHECK_ARG( !(*y <= 1.0) , 1 , -5 );

      /* X+Y = 1 */
      CDF_CHECK_ARG( fabs((*x+*y) -1.0) >= 0.5*DBL_EPSILON , 1.0, 4);
    }

  if (*which != 3)      /* check a */
    {
      CDF_CHECK_ARG( !(*a > 0.0) , 0 , -6 );
    }

  if (*which != 4)      /* check b */
    {
      CDF_CHECK_ARG( !(*b > 0.0) , 0 , -7 );
    }



  /***  Calculate ANSWERS ***/

  if (1 == *which)       /* compute (p,q) */
    {
      if ( finite(*x) && finite(*y) )
	{
	  CDF_CHECK_ARG( fabs((*x+*y) -1.0) >= 0.5*DBL_EPSILON , 1.0, 4);
	  cdf_cumbet (x, y, a, b, p, q);
	  *status = 0;
	}
      else  /* take care of special cases where we don't check that x+y=1 */
	{
	  if ( isnan(*x) )
	    *p = *q = *x;
	  else if ( isnan(*y) )
	    *p = *q = *y;
	  else if ( *x > DBL_MAX && *y < - DBL_MAX )
	    {
	      *p = 1.0; *q = 0.0;
	    }
	  else if ( *y > DBL_MAX &&  *x < - DBL_MAX )
	    {
	      *q = 1.0; *p = 0.0;
	    }
	  else
	    {
	      *status = 4;
	    }
	}
    }

  else if (2 == *which)  /* compute (x,y) */
    {
      /* special cases */
      if ( *p == 0.0 )
	{
	  *x = 0.0; *y = 1.0;
	}
      else if ( *q == 0.0 )
	{
	  *x = 1.0; *y = 0.0;
	}
      else
	{
	  ZsearchStruct S;
	  zsearch_ret ret_val;
	  double xinit = 1.0/(1.0+(*b)/(*a));  /* this is the mean */
	  double step = *a >= *b ? sqrt(*b/(1.0 + (*b+1)/(*a)))/(*a+*b) : sqrt(*a/(1.0 + (*a+1)/(*b)))/(*a+*b);
	  
	  /* force xinit to be in (0,1) and step to be > 0 */
	  xinit = Max ( DBL_EPSILON , xinit );
	  xinit = Min ( 1.0-DBL_EPSILON , xinit );
	  step = Max ( DBL_EPSILON, step );

	  if ( pq_flag )   /* compute x */
	    {
	      *x = xinit;
	      nsp_zsearch_init(*x, 0.0, 1.0, step, 0.0, 2.0, atol, tol, INCREASING, &S);
	      do
		{
		  *y = 1.0 - *x;
		  cdf_cumbet (x, y, a, b, &cum, &ccum);
		  fx = cum - *p;
		}
	      while ( (ret_val = nsp_zsearch(x, fx, &S)) == EVAL_FX );
	      switch ( ret_val )
		{
		case 
		  SUCCESS:
		  *status = 0; *y = 1.0 - *x; break;
		case LEFT_BOUND_EXCEEDED:
		  *status = 0; *x = 0.0; *y = 1.0; break;  /* display a warning ? */
		case RIGHT_BOUND_EXCEEDED:
		  *status = 0; *x = 1.0; *y = 0.0; break;  /* display a warning ? */
		default:
		  *status = 6;
		}
	    }
	  else            /* compute y */
	    {
	      *y = 1.0 - xinit;;
	      nsp_zsearch_init(*y, 0.0, 1.0, step, 0.0, 2.0, atol, tol, INCREASING, &S);
	      do
		{
		  *x = 1.0 - *y;
		  cdf_cumbet (x, y, a, b, &cum, &ccum);
		  fx = ccum - *q;
		}
	      while ( (ret_val = nsp_zsearch(y, fx, &S)) == EVAL_FX );
	      switch ( ret_val )
		{
		case 
		  SUCCESS:
		  *status = 0; *x = 1.0 - *y; break;
		case LEFT_BOUND_EXCEEDED:
		  *status = 0; *y = 0.0; *x = 1.0; break;  /* display a warning ? */
		case RIGHT_BOUND_EXCEEDED:
		  *status = 0; *y = 1.0; *x = 0.0; break;  /* display a warning ? */
		default:
		  *status = 6;
		}
	    }
	}
    }

  else if (3 == *which)         /* compute a */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *a = 5.;
      nsp_zsearch_init(*a, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumbet (x, y, a, b, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(a, fx, &S)) == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0; break;
	case BOTH_BOUND_EXCEEDED:
	  *status = 5; *bound = zero; *boundbis = inf; break;
	default:
	  *status = 6;
	}
    }

  else if (4 == *which)         /* compute b */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *b = 5.;
      nsp_zsearch_init(*b, zero, inf, 2.0, 0.0, 2.0, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumbet (x, y, a, b, &cum, &ccum);
	  if ( pq_flag )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	}
      while ( (ret_val = nsp_zsearch(b, fx, &S)) == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0; break;
	case BOTH_BOUND_EXCEEDED:
	  *status = 5; *bound = zero; *boundbis = inf; break;
	default:
	  *status = 6;
	}
    }

  return 0;
}	
