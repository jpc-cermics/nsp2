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

#include "cdf.h"

int
cdf_cdft (int *which, double *p, double *q, double *t, double *df,
	  int *status, double *bound)
{
  static int c__1 = 1;
  static double c_b20 = .5;
  static double c_b22 = 5.;
  /* const double inf = 1.0D300;*/
  const double tol=1.0E-8, atol=1.0E-50, zero=1.0E-300,rtinf=1.0E150, maxdf=1.0E10;
  double d__1;
  double ccum;
  int qleft;
  int qporq;
  double fx, pq;
  int qhi;
  double cum;

  if (!(*which < 1 || *which > 3))
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
  *bound = 3.;
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
  if (*which == 3)
    {
      goto L130;
    }

/*     DF */

  if (!(*df <= 0.))
    {
      goto L120;
    }
  *bound = 0.;
  *status = -5;
  return 0;
L120:
L130:
  if (*which == 1)
    {
      goto L170;
    }

/*     P + Q */

  pq = *p + *q;
  if (!((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L160;
    }
  if (!(pq < 0.))
    {
      goto L140;
    }
  *bound = 0.;
  goto L150;
L140:
  *bound = 1.;
L150:
  *status = 3;
  return 0;
L160:
L170:
  if (!(*which == 1))
    {
      qporq = *p <= *q;
    }

/*     Select the minimum of P or Q */


/*     Calculate ANSWERS */

  if (1 == *which)
    {

/*     Computing P and Q */

      cdf_cumt (t, df, p, q);
      *status = 0;
    }
  else if (2 == *which)
    {

/*     Computing T */

/*     .. Get initial approximation for T */

      *t = cdf_dt1 (p, q, df);
      d__1 = -rtinf;
      cdf_dstinv (&d__1, &rtinf, &c_b20, &c_b20, &c_b22, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, t, &fx, &qleft, &qhi);
    L180:
      if (!(*status == 1))
	{
	  goto L210;
	}
      cdf_cumt (t, df, &cum, &ccum);
      if (!qporq)
	{
	  goto L190;
	}
      fx = cum - *p;
      goto L200;
    L190:
      fx = ccum - *q;
    L200:
      cdf_dinvr (status, t, &fx, &qleft, &qhi);
      goto L180;
    L210:
      if (!(*status == -1))
	{
	  goto L240;
	}
      if (!qleft)
	{
	  goto L220;
	}
      *status = 1;
      *bound = -rtinf;
      goto L230;
    L220:
      *status = 2;
      *bound = rtinf;
    L230:
    L240:
      ;
    }
  else if (3 == *which)
    {

/*     Computing DF */

      *df = 5.;
      cdf_dstinv (&zero, &maxdf, &c_b20, &c_b20, &c_b22, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
    L250:
      if (!(*status == 1))
	{
	  goto L280;
	}
      cdf_cumt (t, df, &cum, &ccum);
      if (!qporq)
	{
	  goto L260;
	}
      fx = cum - *p;
      goto L270;
    L260:
      fx = ccum - *q;
    L270:
      cdf_dinvr (status, df, &fx, &qleft, &qhi);
      goto L250;
    L280:
      if (!(*status == -1))
	{
	  goto L310;
	}
      if (!qleft)
	{
	  goto L290;
	}
      *status = 1;
      *bound = zero;
      goto L300;
    L290:
      *status = 2;
      *bound = maxdf;
    L300:
    L310:
      ;
    }
  return 0;
}				/* cdft_ */
