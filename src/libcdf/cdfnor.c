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

#include "cdf.h"


int
cdf_cdfnor (int *which, double *p, double *q, double *x, double *mean,
	    double *sd, int *status, double *bound)
{
  static int c__1 = 1;
  double d__1;
  double z__, pq;
  *status = 0;
  if (!(*which < 1 || *which > 4))
    {
      goto L30;
    }
  if (!(*which < 1))
    {
      goto L10;
    }
  *bound = 1.;
  goto L20;
L10:
  *bound = 4.;
L20:
  *status = -1;
  return 0;
L30:
  if (*which == 1)
    {
      goto L70;
    }

/*     P */

  if (!(*p <= 0. || *p > 1.))
    {
      goto L60;
    }
  if (!(*p <= 0.))
    {
      goto L40;
    }
  *bound = 0.;
  goto L50;
L40:
  *bound = 1.;
L50:
  *status = -2;
  return 0;
L60:
L70:
  if (*which == 1)
    {
      goto L110;
    }

/*     Q */

  if (!(*q <= 0. || *q > 1.))
    {
      goto L100;
    }
  if (!(*q <= 0.))
    {
      goto L80;
    }
  *bound = 0.;
  goto L90;
L80:
  *bound = 1.;
L90:
  *status = -3;
  return 0;
L100:
L110:
  if (*which == 1)
    {
      goto L150;
    }

/*     P + Q */

  pq = *p + *q;
  if (!((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L140;
    }
  if (!(pq < 0.))
    {
      goto L120;
    }
  *bound = 0.;
  goto L130;
L120:
  *bound = 1.;
L130:
  *status = 3;
  return 0;
L140:
L150:
  if (*which == 4)
    {
      goto L170;
    }

/*     SD */

  if (!(*sd <= 0.))
    {
      goto L160;
    }
  *bound = 0.;
  *status = -6;
  return 0;
L160:

/*     Calculate ANSWERS */

L170:
  if (1 == *which)
    {

/*     Computing P */

      z__ = (*x - *mean) / *sd;
      cdf_cumnor (&z__, p, q);
    }
  else if (2 == *which)
    {

/*     Computing X */

      z__ = cdf_dinvnr (p, q);
      *x = *sd * z__ + *mean;
    }
  else if (3 == *which)
    {

/*     Computing the MEAN */

      z__ = cdf_dinvnr (p, q);
      *mean = *x - *sd * z__;
    }
  else if (4 == *which)
    {

/*     Computing SD */

      z__ = cdf_dinvnr (p, q);
      *sd = (*x - *mean) / z__;
    }
  return 0;
}				/* cdfnor_ */
