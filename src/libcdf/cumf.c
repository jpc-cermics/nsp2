#include "cdf.h"

/* ********************************************************************** */

/*     SUBROUTINE CUMF(F,DFN,DFD,CUM,CCUM) */
/*                    CUMulative F distribution */


/*                              Function */


/*     Computes  the  integral from  0  to  F of  the f-density  with DFN */
/*     and DFD degrees of freedom. */


/*                              Arguments */


/*     F --> Upper limit of integration of the f-density. */
/*                                                  F is DOUBLE PRECISION */

/*     DFN --> Degrees of freedom of the numerator sum of squares. */
/*                                                  DFN is DOUBLE PRECISI */

/*     DFD --> Degrees of freedom of the denominator sum of squares. */
/*                                                  DFD is DOUBLE PRECISI */

/*     CUM <-- Cumulative f distribution. */
/*                                                  CUM is DOUBLE PRECISI */

/*     CCUM <-- Compliment of Cumulative f distribution. */
/*                                                  CCUM is DOUBLE PRECIS */


/*                              Method */


/*     Formula  26.5.28 of  Abramowitz and   Stegun   is  used to  reduce */
/*     the cumulative F to a cumulative beta distribution. */


/*                              Note */


/*     If F is less than or equal to 0, 0 is returned. */


int cdf_cumf (double *f, double *dfn, double *dfd, double *cum, double *ccum)
{
  const double half=0.5E0, done=1.0E0;
  double d__1, d__2;
  int ierr;
  double prod, dsum, xx;
  double yy;

  if (!(*f <= 0.))
    {
      goto L10;
    }
  *cum = 0.;
  *ccum = 1.;
  return 0;
L10:
  prod = *dfn * *f;

/*     XX is such that the incomplete beta with parameters */
/*     DFD/2 and DFN/2 evaluated at XX is 1 - CUM or CCUM */

/*     YY is 1 - XX */

/*     Calculate the smaller of XX and YY accurately */

  dsum = *dfd + prod;
  xx = *dfd / dsum;
  if (xx > half)
    {
      yy = prod / dsum;
      xx = done - yy;
    }
  else
    {
      yy = done - xx;
    }
  d__1 = *dfd * half;
  d__2 = *dfn * half;
  cdf_bratio (&d__1, &d__2, &xx, &yy, ccum, cum, &ierr);
  return 0;
}				/* cumf_ */
