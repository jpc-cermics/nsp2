#include "cdf.h"

/* Table of constant values */

static double c_b12 = -1e100;
static double c_b13 = 1e100;
static double c_b14 = .5;
static double c_b16 = 5.;
static double c_b17 = 1e-50;
static double c_b18 = 1e-14;  /* rtol parameter */   
static double c_b24 = 1e-100;
static double c_b25 = 1e4;
static double c_b36 = -1e4;

/************************************************************************ 
 * 
 *     SUBROUTINE CDFTNC( WHICH, P, Q, T, DF, PNONC, STATUS, BOUND ) 
 *              Cumulative Distribution Function 
 *                 Non-Central T distribution 
 * 
 *                              Function 
 * 
 *    Calculates any one parameter of the noncentral t distribution give 
 *    values for the others. 
 * 
 *                              Arguments 
 * 
 *    WHICH --> Int indicating which  argument 
 *              values is to be calculated from the others. 
 *              Legal range: 1..3 
 *              iwhich = 1 : Calculate P and Q from T,DF,PNONC 
 *              iwhich = 2 : Calculate T from P,Q,DF,PNONC 
 *              iwhich = 3 : Calculate DF from P,Q,T 
 *              iwhich = 4 : Calculate PNONC from P,Q,DF,T 
 *                   INT WHICH 
 * 
 *       P <--> The integral from -infinity to t of the noncentral t-den 
 *             Input range: (0,1]. 
 *                   DOUBLE PRECISION P 
 * 
 *    Q <--> 1-P. 
 *           Input range: (0, 1]. 
 *           P + Q = 1.0. 
 *                   DOUBLE PRECISION Q 
 * 
 *       T <--> Upper limit of integration of the noncentral t-density. 
 *              Input range: ( -infinity, +infinity). 
 *              Search range: [ -1E100, 1E100 ] 
 *                   DOUBLE PRECISION T 
 * 
 *       DF <--> Degrees of freedom of the noncentral t-distribution. 
 *               Input range: (0 , +infinity). 
 *               Search range: [1e-100, 1E10] 
 *                   DOUBLE PRECISION DF 
 * 
 *    PNONC <--> Noncentrality parameter of the noncentral t-distributio 
 *               Input range: [-infinity , +infinity). 
 *               Search range: [-1e4, 1E4] 
 * 
 *    STATUS <-- 0 if calculation completed correctly 
 *              -I if input parameter number I is out of range 
 *               1 if answer appears to be lower than lowest 
 *                 search bound 
 *               2 if answer appears to be higher than greatest 
 *                 search bound 
 *               3 if P + Q .ne. 1 
 *                   INT STATUS 
 * 
 *    BOUND <-- Undefined if STATUS is 0 
 * 
 *              Bound exceeded by parameter number I if STATUS 
 *              is negative. 
 * 
 *              Lower search bound if STATUS is 1. 
 * 
 *              Upper search bound if STATUS is 2. 
 * 
 *                               Method 
 * 
 *    Upper tail    of  the  cumulative  noncentral t is calculated usin 
 *    formulae  from page 532  of Johnson, Kotz,  Balakrishnan, Coninuou 
 *    Univariate Distributions, Vol 2, 2nd Edition.  Wiley (1995) 
 * 
 *    Computation of other parameters involve a seach for a value that 
 *    produces  the desired  value  of P.   The search relies  on  the 
 *    monotinicity of P with the other parameter. 
 * 
 ************************************************************************ 
 */

int cdf_cdftnc (int *which, double *p, double *q, double *t, double *df,
		double *pnonc, int *status, double *bound, double *boundbis)
{
  double ccum;
  int qleft;
  double fx;
  int qhi;
  double cum;

  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );

  if (*which != 1)
    {
      CDF_CHECK_ARG(*p < 0 , 0 , -2 );
      CDF_CHECK_ARG(*p >  .99999999999999989, .99999999999999989 , -2 );
    }

  if (*which != 3)
    {
      CDF_CHECK_ARG(*df <= 0 , 0 , -5 );
    }

  if (1 == *which)
    {
      cdf_cumtnc (t, df, pnonc, p, q);
      *status = 0;
    }
  else if (2 == *which)
    {
      *t = *pnonc;    /* start near the mode */
      cdf_dstinv (&c_b12, &c_b13, &c_b14, &c_b14, &c_b16, &c_b17, &c_b18);
      *status = 0;
      cdf_dinvr (status, t, &fx, &qleft, &qhi);
    L110:
      if (!(*status == 1))
	{
	  goto L120;
	}
      cdf_cumtnc (t, df, pnonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, t, &fx, &qleft, &qhi);
      goto L110;
    L120:
      if (!(*status == -1))
	{
	  goto L150;
	}
      if (!qleft)
	{
	  goto L130;
	}
      *status = 1;
      *bound = -1e100;
      goto L140;
    L130:
      *status = 2;
      *bound = 1e100;
    L140:
    L150:
      ;
    }
  else if (3 == *which)
    {
      *df = 5.;
      cdf_dstinv (&c_b24, &c_b25, &c_b14, &c_b14, &c_b16, &c_b17, &c_b18);
      *status = 0;
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
    L160:
      if (!(*status == 1))
	{
	  goto L170;
	}
      cdf_cumtnc (t, df, pnonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
      goto L160;
    L170:
      if (!(*status == -1))
	{
	  goto L200;
	}
      if (!qleft)
	{
	  goto L180;
	}
      *status = 1;
      *bound = 1e-100;
      goto L190;
    L180:
      *status = 2;
      *bound = 1e100;
    L190:
    L200:
      ;
    }
  else if (4 == *which)
    {
      *pnonc = 5.;
      cdf_dstinv (&c_b36, &c_b25, &c_b14, &c_b14, &c_b16, &c_b17, &c_b18);
      *status = 0;
      cdf_dinvr (status, pnonc, &fx, &qleft, &qhi);
    L210:
      if (!(*status == 1))
	{
	  goto L220;
	}
      cdf_cumtnc (t, df, pnonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, pnonc, &fx, &qleft, &qhi);
      goto L210;
    L220:
      if (!(*status == -1))
	{
	  goto L250;
	}
      if (!qleft)
	{
	  goto L230;
	}
      *status = 1;
      *bound = 0.;
      goto L240;
    L230:
      *status = 2;
      *bound = 1e4;
    L240:
    L250:
      ;
    }
  return 0;
}
