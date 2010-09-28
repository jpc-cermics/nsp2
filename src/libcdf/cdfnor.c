/* ********************************************************************** */
/*      SUBROUTINE CDFNOR( WHICH, P, Q, X, MEAN, SD, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*               NORmal distribution */
/*                              Function */
/*     Calculates any one parameter of the normal */
/*     distribution given values for the others. */
/*                              Arguments */
/*     WHICH  --> Int indicating  which of the  next  parameter */
/*     values is to be calculated using values  of the others. */
/*     Legal range: 1..4 */
/*               iwhich = 1 : Calculate P and Q from X,MEAN and SD */
/*               iwhich = 2 : Calculate X from P,Q,MEAN and SD */
/*               iwhich = 3 : Calculate MEAN from P,Q,X and SD */
/*               iwhich = 4 : Calculate SD from P,Q,X and MEAN */
/*                    INT WHICH */
/*     P <--> The integral from -infinity to X of the normal density. */
/*            Input range: (0,1]. */
/*                    DOUBLE PRECISION P */
/*     Q <--> 1-P. */
/*            Input range: (0, 1]. */
/*            P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*     X < --> Upper limit of integration of the normal-density. */
/*             Input range: ( -infinity, +infinity) */
/*                    DOUBLE PRECISION X */
/*     MEAN <--> The mean of the normal density. */
/*               Input range: (-infinity, +infinity) */
/*                    DOUBLE PRECISION MEAN */
/*     SD <--> Standard Deviation of the normal density. */
/*             Input range: (0, +infinity). */
/*                    DOUBLE PRECISION SD */
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
/*     A slightly modified version of ANORM from */
/*     Cody, W.D. (1993). "ALGORITHM 715: SPECFUN - A Portabel FORTRAN */
/*     Package of Special Function Routines and Test Drivers" */
/*     acm Transactions on Mathematical Software. 19, 22-32. */
/*     is used to calulate the  cumulative standard normal distribution. */
/*     The rational functions from pages  90-95  of Kennedy and Gentle, */
/*     Statistical  Computing,  Marcel  Dekker, NY,  1980 are  used  as */
/*     starting values to Newton's Iterations which compute the inverse */
/*     standard normal.  Therefore no  searches  are necessary for  any */
/*     parameter. */
/*     For X < -15, the asymptotic expansion for the normal is used  as */
/*     the starting value in finding the inverse standard normal. */
/*     This is formula 26.2.12 of Abramowitz and Stegun. */
/*                              Note */
/*      The normal density is proportional to */
/*      exp( - 0.5 * (( X - MEAN)/SD)**2) */
/* ********************************************************************** */

/* rewritten by Bruno Pincon and Jean-Philippe Chancelier (sept 2010) */

#include "cdf.h"


int cdf_cdfnor (int *which, double *p, double *q, double *x, double *mean,
		double *sd, int *status, double *bound, double *boundbis)
{
  double z;
  *status = 0;


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
    }

  if (*which != 4)    /* test sd */
    {
      CDF_CHECK_ARG( !(*sd > 0.0) , 0 , -6 );
    }

  /* Note that when p=q=0.5 and x,mean are given (to compute sd) */
  /* then x should be equal to mean (this is not verified) */



  /*   Calculate ANSWERS */

  if (1 == *which)          /* Compute (P,Q) */
    {
      z = (*x - *mean) / *sd;
      cdf_cumnor (&z, p, q);
    }

  else if (2 == *which)    /* Compute X */
    {
      z = cdf_dinvnr(p, q);
      *x = *sd * z + *mean;
    }

  else if (3 == *which)    /* Compute Mean */
    {
      z = cdf_dinvnr (p, q);
      *mean = *x - *sd * z;
    }

  else if (4 == *which)    /* Compute Sd: Note that this is not defined for p=q=0.5 */
    {                      /* in this case return Nan (thanks to a change in cdf_dinvnr */
                           /* which return exactly 0 in this case) */
      z = cdf_dinvnr (p, q);
      *sd = (*x - *mean) / z;
    }

  return 0;
}	

