#include "cdf.h"

/* ********************************************************************** */
/*      SUBROUTINE CDFGAM( WHICH, P, Q, X, SHAPE, SCALE, STATUS, BOUND ) */
/*               Cumulative Distribution Function */
/*                         GAMma Distribution */
/*                              Function */
/*     Calculates any one parameter of the gamma */
/*     distribution given values for the others. */
/*                              Arguments */
/*     WHICH --> Int indicating which of the next four argument */
/*               values is to be calculated from the others. */
/*               Legal range: 1..4 */
/*               iwhich = 1 : Calculate P and Q from X,SHAPE and SCALE */
/*               iwhich = 2 : Calculate X from P,Q,SHAPE and SCALE */
/*               iwhich = 3 : Calculate SHAPE from P,Q,X and SCALE */
/*               iwhich = 4 : Calculate SCALE from P,Q,X and SHAPE */
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
/*     SCALE <--> The scale parameter of the gamma density. */
/*                Input range: (0, +infinity). */
/*                Search range: (1E-300,1E300] */
/*                   DOUBLE PRECISION SCALE */
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
/*       T**(SHAPE - 1) * EXP(- SCALE * T) */
/*                              History */
/*     Routine modified by Scilab group 1998, because of an undefined */
/*     variable. See below comments "CSS". */


int
cdf_cdfgam (int *which, double *p, double *q, double *x, double *shape,
	    double *scale, int *status, double *bound, double *boundbis)
{
  static int c__1 = 1;
  static double c_b27 = -1.;
  static double c_b28 = .5;
  static double c_b30 = 5.;
  const double tol=1.0E-14, atol=1.0E-50, zero=1.0E-300,inf=1.0E300;
  /* System generated locals */
  double d__1;
  /* Local variables */
  double ccum;
  int ierr;
  double porq;
  int qleft;
  int qporq;
  double fx, pq;
  double xscale, xx;
  int qhi;
  double cum;

  /* ********************************************************************** */
  /*     .. Parameters .. */
  /*      CONVERTPARAMETER( (tol=1.0E-8) */
  /*      CONVERTPARAMETER( (atol=1.0E-50) */
  /*      CONVERTPARAMETER( (zero=1.0E-300,inf=1.0D300) */

  CDF_CHECK_ARG(*which < 1 , 1 , -1 );
  CDF_CHECK_ARG(*which > 4 , 4 , -1 );
 
  if (*which != 1)
    {
      /*     P */
      CDF_CHECK_ARG(*p < 0 , 0 , -2 );
      CDF_CHECK_ARG(*p > 1 , 1 , -2 );
      /*     Q */
      CDF_CHECK_ARG(*q <= 0 , 0 , -3 );
      CDF_CHECK_ARG(*q > 1 , 1 , -3 );
    }

  if (*which != 2 && *which != 1 )
    {
      /* add *which==1 to compute cdfgam with *x < 0 (bruno march,22,2010)) */
      /*     X */
      CDF_CHECK_ARG(*x  < 0 , 0 , -4 );
    }
  if (*which != 3)
    {
      /*     SHAPE */
      CDF_CHECK_ARG(*shape  <= 0 , 0 , -5 );
    }

  if (*which != 4)
    {
      /*     SCALE */
      CDF_CHECK_ARG(*scale <= 0 , 0 , -6 );
    }

  if (*which != 1)
    {
      /*     P + Q */
      pq = *p + *q;
      if (((d__1 = pq - .5 - .5, Abs (d__1)) > cdf_spmpar (c__1) * 3.))
	{
	  *bound = (!(pq < 0.)) ?  1.: 0.0;
	  *status = 3;
	  return 0;
	}
    }

  if (*which != 1)
    {
      /*     Select the minimum of P or Q */
      qporq = *p <= *q;
      porq = Min(*p,*q); 
    }

  /*     Calculate ANSWERS */

  if (1 == *which)
    {
      /*     Calculating P */

      *status = 0;
      xscale = *x * *scale;
      cdf_cumgam (&xscale, shape, p, q);
      /* SS   Next line changed by Scilab group. porq undefined here */
      /* SS       IF (porq.GT.1.5E0) status = 10 */
      if (*p > 1.5)
	{
	  *status = 10;
	}
    }
  else if (2 == *which)
    {

      /*     Computing X */

      cdf_gaminv (shape, &xx, &c_b27, p, q, &ierr);
      if ((double) ierr < 0.)
	{
	  *status = 10;
	  return 0;
	}
      else
	{
	  *x = xx / *scale;
	  *status = 0;
	}
    }
  else if (3 == *which)
    {

      /*     Computing SHAPE */

      *shape = 5.;
      xscale = *x * *scale;
      cdf_dstinv (&zero, &inf, &c_b28, &c_b28, &c_b30, &atol, &tol);
      *status = 0;
      cdf_dinvr (status, shape, &fx, &qleft, &qhi);
    L250:
      if (!(*status == 1))
	{
	  goto L290;
	}
      cdf_cumgam (&xscale, shape, &cum, &ccum);
      if (!qporq)
	{
	  goto L260;
	}
      fx = cum - *p;
      goto L270;
    L260:
      fx = ccum - *q;
    L270:
      if (!((qporq && cum > 1.5) || (!qporq && ccum > 1.5)))
	{
	  goto L280;
	}
      *status = 10;
      return 0;
    L280:
      cdf_dinvr (status, shape, &fx, &qleft, &qhi);
      goto L250;
    L290:
      if (!(*status == -1))
	{
	  goto L320;
	}
      if (!qleft)
	{
	  goto L300;
	}
      *status = 1;
      *bound = zero;
      goto L310;
    L300:
      *status = 2;
      *bound = inf;
    L310:
    L320:
      ;
    }
  else if (4 == *which)
    {

      /*     Computing SCALE */

      cdf_gaminv (shape, &xx, &c_b27, p, q, &ierr);
      if ((double) ierr < 0.)
	{
	  *status = 10;
	  return 0;
	}
      else
	{
	  *scale = xx / *x;
	  *status = 0;
	}
    }
  return 0;
}				/* cdfgam_ */
