#include "cdf.h"

/* ********************************************************************** */
/*     SUBROUTINE CUMT(T,DF,CUM,CCUM) */
/*                    CUMulative T-distribution */
/*                              Function */
/*     Computes the integral from -infinity to T of the t-density. */
/*                              Arguments */
/*     T --> Upper limit of integration of the t-density. */
/*                                                  T is DOUBLE PRECISION */
/*     DF --> Degrees of freedom of the t-distribution. */
/*                                                  DF is DOUBLE PRECISIO */
/*     CUM <-- Cumulative t-distribution. */
/*                                                  CCUM is DOUBLE PRECIS */
/*     CCUM <-- Compliment of Cumulative t-distribution. */
/*                                                  CCUM is DOUBLE PRECIS */
/*                              Method */
/*     Formula 26.5.27   of     Abramowitz  and   Stegun,    Handbook  of */
/*     Mathematical Functions  is   used   to  reduce the  t-distribution */
/*     to an incomplete beta. */
/* ********************************************************************** */



int cdf_cumt (double *t, double *df, double *cum, double *ccum)
{
  double c_b2 = .5;
  double d__1;
  double a, dfptt, tt;
  double xx, yy, oma;
  tt = *t * *t;
  dfptt = *df + tt;
  xx = *df / dfptt;
  yy = tt / dfptt;
  d__1 = *df * .5;
  cdf_cumbet (&xx, &yy, &d__1, &c_b2, &a, &oma);
  if (!(*t <= 0.))
    {
      goto L10;
    }
  *cum = a * .5;
  *ccum = oma + *cum;
  goto L20;
L10:
  *ccum = a * .5;
  *cum = oma + *ccum;
L20:
  return 0;
}				/* cumt_ */
