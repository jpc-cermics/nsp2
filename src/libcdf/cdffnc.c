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


int
cdf_cdffnc (int *which, double *p, double *q, double *f, double *dfn,
	    double *dfd, double *phonc, int *status, double *bound)
{
static double c_b17 = 0.;
static double c_b18 = .5;
static double c_b20 = 5.;

  const double tent4=1.0E4,tol=1.0E-8, atol=1.0E-50, zero=1.0E-300,one=1.0E0-1.0E-16,inf=1.0E300;

  double ccum;
  int qleft;
  double fx;
  int qhi;
  double cum;

  if (!(*which < 1 || *which > 5))
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
  *bound = 5.;
L20:
  *status = -1;
  return 0;
L30:
  if (*which == 1)
    {
      goto L70;
    }

/*     P */

  if (!(*p < 0. || *p > one))
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

/*     F */

  if (!(*f < 0.))
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

/*     DFN */

  if (!(*dfn <= 0.))
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

/*     DFD */

  if (!(*dfd <= 0.))
    {
      goto L120;
    }
  *bound = 0.;
  *status = -6;
  return 0;
L120:
L130:
  if (*which == 5)
    {
      goto L150;
    }

/*     PHONC */

  if (!(*phonc < 0.))
    {
      goto L140;
    }
  *bound = 0.;
  *status = -7;
  return 0;
L140:

/*     Calculate ANSWERS */

L150:
  if (1 == *which)
    {

/*     Calculating P */

      cdf_cumfnc (f, dfn, dfd, phonc, p, q);
      *status = 0;
    }
  else if (2 == *which)
    {

/*     Calculating F */

      *f = 5.;
      cdf_dstinv (&c_b17, &inf, &c_b18, &c_b18, &c_b20, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, f, &fx, &qleft, &qhi);
    L160:
      if (!(*status == 1))
	{
	  goto L170;
	}
      cdf_cumfnc (f, dfn, dfd, phonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, f, &fx, &qleft, &qhi);
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
      *bound = 0.;
      goto L190;
    L180:
      *status = 2;
      *bound = inf;
    L190:
    L200:
      ;
    }
  else if (3 == *which)
    {

/*     Calculating DFN */

      *dfn = 5.;
      cdf_dstinv (&zero, &inf, &c_b18, &c_b18, &c_b20, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, dfn, &fx, &qleft, &qhi);
    L210:
      if (!(*status == 1))
	{
	  goto L220;
	}
      cdf_cumfnc (f, dfn, dfd, phonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, dfn, &fx, &qleft, &qhi);
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
      *bound = zero;
      goto L240;
    L230:
      *status = 2;
      *bound = inf;
    L240:
    L250:
      ;
    }
  else if (4 == *which)
    {

/*     Calculating DFD */

      *dfd = 5.;
      cdf_dstinv (&zero, &inf, &c_b18, &c_b18, &c_b20, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, dfd, &fx, &qleft, &qhi);
    L260:
      if (!(*status == 1))
	{
	  goto L270;
	}
      cdf_cumfnc (f, dfn, dfd, phonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, dfd, &fx, &qleft, &qhi);
      goto L260;
    L270:
      if (!(*status == -1))
	{
	  goto L300;
	}
      if (!qleft)
	{
	  goto L280;
	}
      *status = 1;
      *bound = zero;
      goto L290;
    L280:
      *status = 2;
      *bound = inf;
    L290:
    L300:
      ;
    }
  else if (5 == *which)
    {

/*     Calculating PHONC */

      *phonc = 5.;
      cdf_dstinv (&c_b17, &tent4, &c_b18, &c_b18, &c_b20, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, phonc, &fx, &qleft, &qhi);
    L310:
      if (!(*status == 1))
	{
	  goto L320;
	}
      cdf_cumfnc (f, dfn, dfd, phonc, &cum, &ccum);
      fx = cum - *p;
      cdf_dinvr (status, phonc, &fx, &qleft, &qhi);
      goto L310;
    L320:
      if (!(*status == -1))
	{
	  goto L350;
	}
      if (!qleft)
	{
	  goto L330;
	}
      *status = 1;
      *bound = 0.;
      goto L340;
    L330:
      *status = 2;
      *bound = tent4;
    L340:
    L350:
      ;
    }
  return 0;
}				/* cdffnc_ */
