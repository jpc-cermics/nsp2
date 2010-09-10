/* ********************************************************************** */
/*      SUBROUTINE CDFF( WHICH, P, Q, F, DFN, DFD, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*               F distribution */
/*                              Function */
/*     Calculates any one parameter of the F distribution */
/*     given values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next four argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..4 */
/*               iwhich = 1 : Calculate P and Q from F,DFN and DFD */
/*               iwhich = 2 : Calculate F from P,Q,DFN and DFD */
/*               iwhich = 3 : Calculate DFN from P,Q,F and DFD */
/*               iwhich = 4 : Calculate DFD from P,Q,F and DFN */
/*                    INT WHICH */
/*       P <--> The integral from 0 to F of the f-density. */
/*              Input range: [0,1]. */
/*                    DOUBLE PRECISION P */
/*       Q <--> 1-P. */
/*              Input range: (0, 1]. */
/*              P + Q = 1.0. */
/*                    DOUBLE PRECISION Q */
/*       F <--> Upper limit of integration of the f-density. */
/*              Input range: [0, +infinity). */
/*              Search range: [0,1E300] */
/*                    DOUBLE PRECISION F */
/*     DFN < --> Degrees of freedom of the numerator sum of squares. */
/*               Input range: (0, +infinity). */
/*               Search range: [ 1E-300, 1E300] */
/*                    DOUBLE PRECISION DFN */
/*     DFD < --> Degrees of freedom of the denominator sum of squares. */
/*               Input range: (0, +infinity). */
/*               Search range: [ 1E-300, 1E300] */
/*                    DOUBLE PRECISION DFD */
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
/*     Formula   26.6.2   of   Abramowitz   and   Stegun,  Handbook  of */
/*     Mathematical  Functions (1966) is used to reduce the computation */
/*     of the  cumulative  distribution function for the  F  variate to */
/*     that of an incomplete beta. */
/*     Computation of other parameters involve a seach for a value that */
/*     produces  the desired  value  of P.   The search relies  on  the */
/*     monotinicity of P with the other parameter. */
/*                              WARNING */
/*     The value of the  cumulative  F distribution is  not necessarily */
/*     monotone in  either degrees of freedom.  There  thus may  be two */
/*     values  that  provide a given CDF  value.   This routine assumes */
/*     monotonicity and will find an arbitrary one of the two values. */
/* ********************************************************************** */


#include "cdf.h"


int
cdf_cdff (int *which, double *p, double *q, double *f, double *dfn,
	  double *dfd, int *status, double *bound)
{
static int c__1 = 1;
static double c_b24 = 0.;
static double c_b25 = .5;
static double c_b27 = 5.;

  const double tol=1.0E-14,atol=1.0E-50, zero=1.0E-300,inf=1.0E300;

  double d__1;

  double ccum;
  int qleft;
  int qporq;
  double fx, pq;
  int qhi;
  double cum;

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

  if (!(*p < 0. || *p > 1.))
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
  if (*which == 2)
    {
      goto L130;
    }

/*     F */

  if (!(*f < 0.) || *which==1 )   /* add *which==1 to compute cdff with *x < 0 (bruno march,22,2010)) */
    {
      goto L120;
    }
  *bound = 0.;
  *status = -4;
  return 0;
L120:
L130:
  if (*which == 3)
    {
      goto L150;
    }

/*     DFN */

  if (!(*dfn <= 0.))
    {
      goto L140;
    }
  *bound = 0.;
  *status = -5;
  return 0;
L140:
L150:
  if (*which == 4)
    {
      goto L170;
    }

/*     DFD */

  if (!(*dfd <= 0.))
    {
      goto L160;
    }
  *bound = 0.;
  *status = -6;
  return 0;
L160:
L170:
  if (*which == 1)
    {
      goto L210;
    }

/*     P + Q */

  pq = *p + *q;
  if (!((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
    {
      goto L200;
    }
  if (!(pq < 0.))
    {
      goto L180;
    }
  *bound = 0.;
  goto L190;
L180:
  *bound = 1.;
L190:
  *status = 3;
  return 0;
L200:
L210:
  if (!(*which == 1))
    {
      qporq = *p <= *q;
    }

/*     Select the minimum of P or Q */


/*     Calculate ANSWERS */

  if (1 == *which)
    {

/*     Calculating P */

      cdf_cumf (f, dfn, dfd, p, q);
      *status = 0;
    }
  else if (2 == *which)
    {

/*     Calculating F */

      *f = 5.;
      cdf_dstinv (&c_b24, &inf, &c_b25, &c_b25, &c_b27, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, f, &fx, &qleft, &qhi);
    L220:
      if (!(*status == 1))
	{
	  goto L250;
	}
      cdf_cumf (f, dfn, dfd, &cum, &ccum);
      if (!qporq)
	{
	  goto L230;
	}
      fx = cum - *p;
      goto L240;
    L230:
      fx = ccum - *q;
    L240:
      cdf_dinvr (status, f, &fx, &qleft, &qhi);
      goto L220;
    L250:
      if (!(*status == -1))
	{
	  goto L280;
	}
      if (!qleft)
	{
	  goto L260;
	}
      *status = 1;
      *bound = 0.;
      goto L270;
    L260:
      *status = 2;
      *bound = inf;
    L270:
    L280:
      ;
    }
  else if (3 == *which)   /* Calculating DFN */
    {
      ZsearchStruct S;
      zsearch_ret ret_val;
      *dfn = 5.;
      nsp_zsearch_init(*dfn, zero, inf, c_b25, c_b25, c_b27, atol, tol, UNKNOWN, &S);
      do
	{
	  cdf_cumf (f, dfn, dfd, &cum, &ccum);
	  if ( qporq )
	    fx = cum - *p;
	  else
	    fx = ccum - *q;
	  ret_val = nsp_zsearch(dfn, fx, &S);
	}
      while ( ret_val == EVAL_FX );

      switch ( ret_val )
	{
	case SUCCESS:
	  *status = 0;
	  break;
	case LEFT_BOUND_EXCEEDED:
	  *status = 1;
	  *bound = zero;
	  break;
	case RIGHT_BOUND_EXCEEDED:
	  *status = 2;
	  *bound = inf;
	  break;
	default:
	  *status = 4;
	}
    }
  else if (4 == *which)
    {

/*     Calculating DFD */

      *dfd = 5.;
      cdf_dstinv (&zero, &inf, &c_b25, &c_b25, &c_b27, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, dfd, &fx, &qleft, &qhi);
    L360:
      if (!(*status == 1))
	{
	  goto L390;
	}
      cdf_cumf (f, dfn, dfd, &cum, &ccum);
      if (!qporq)
	{
	  goto L370;
	}
      fx = cum - *p;
      goto L380;
    L370:
      fx = ccum - *q;
    L380:
      cdf_dinvr (status, dfd, &fx, &qleft, &qhi);
      goto L360;
    L390:
      if (!(*status == -1))
	{
	  goto L420;
	}
      if (!qleft)
	{
	  goto L400;
	}
      *status = 1;
      *bound = zero;
      goto L410;
    L400:
      *status = 2;
      *bound = inf;
    L410:
    L420:
      ;
    }
  return 0;
}				/* cdff_ */
