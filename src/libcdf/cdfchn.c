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

int
cdf_cdfchn (int *which, double *p, double *q, double *x, double *df,
	    double *pnonc, int *status, double *bound)
{
  static double c_b15 = 0., c_b16 = .5, c_b18 = 5.;
  const double tent4=1.0E4, tol=1.0E-14, atol=1.0E-50;
  const double zero=1.0E-300,one=1.0E0-1.0E-16,inf=1.0E300;
  double ccum, fx, cum;
  int qleft, qhi;

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
  /* P */
  if ( ! (*p < 0. || *p > one))
    {
      goto L60;
    }
  if (!(*p < 0.))
    {
      goto L40;
    }
  *bound = 0.;
  goto L50;
 L40:
  *bound = one;
 L50:
  *status = -2;
  return 0;
 L60:
 L70:
  if (*which == 2)
    {
      goto L90;
    }
  /*     X */
  if ( !(*x < 0.) || *which==1 )   /* add *which==1 to compute cdfchn with *x < 0 (bruno march,22,2010)) */
    {
      goto L80;
    }
  *bound = 0.;
  *status = -4;
  return 0;
 L80:
 L90:
  if (*which == 3)
    {
      goto L110;
    }
  /*     DF */
  if (!(*df <= 0.))
    {
      goto L100;
    }
  *bound = 0.;
  *status = -5;
  return 0;
 L100:
 L110:
  if (*which == 4)
    {
      goto L130;
    }

  /*     PNONC */

  if (!(*pnonc < 0.))
    {
      goto L120;
    }
  *bound = 0.;
  *status = -6;
  return 0;
 L120:
  /*     Calculate ANSWERS */
 L130:
  if (1 == *which)
    {
      /* Calculating P and Q */
      cdf_cumchn_new (x, df, pnonc, p, q);
      *status = 0;
    }
  else if (2 == *which)
    {
      /*     Calculating X */
      *x = *df + *pnonc;    /* start from the mean instead of 5, bruno april 2010 */
      cdf_dstinv (&c_b15, &inf, &c_b16, &c_b16, &c_b18, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, x, &fx, &qleft, &qhi);
    L140:
      if (!(*status == 1))
	{
	  goto L150;
	}
      cdf_cumchn_new (x, df, pnonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, x, &fx, &qleft, &qhi);
      goto L140;
    L150:
      if (!(*status == -1))
	{
	  goto L180;
	}
      if (!qleft)
	{
	  goto L160;
	}
      *status = 1;
      *bound = 0.;
      goto L170;
    L160:
      *status = 2;
      *bound = inf;
    L170:
    L180:
      ;
    }
  else if (3 == *which)
    {
      /*     Calculating DF */
      *df = 5.;
      cdf_dstinv (&zero, &inf, &c_b16, &c_b16, &c_b18, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
    L190:
      if (!(*status == 1))
	{
	  goto L200;
	}
      cdf_cumchn_new (x, df, pnonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
      goto L190;
    L200:
      if (!(*status == -1))
	{
	  goto L230;
	}
      if (!qleft)
	{
	  goto L210;
	}
      *status = 1;
      *bound = zero;
      goto L220;
    L210:
      *status = 2;
      *bound = inf;
    L220:
    L230:
      ;
    }
  else if (4 == *which)
    {
      /*     Calculating PNONC */
      *pnonc = 5.;
      cdf_dstinv (&c_b15, &tent4, &c_b16, &c_b16, &c_b18, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, pnonc, &fx, &qleft, &qhi);
    L240:
      if (!(*status == 1))
	{
	  goto L250;
	}
      cdf_cumchn_new (x, df, pnonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, pnonc, &fx, &qleft, &qhi);
      goto L240;
    L250:
      if (!(*status == -1))
	{
	  goto L280;
	}
      if (!qleft)
	{goto L260;
	}
      *status = 1;
      *bound = zero;
      goto L270;
    L260:
      *status = 2;
      *bound = tent4;
    L270:
    L280:
      ;
    }
  return 0;
}

