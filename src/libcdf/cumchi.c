#include "cdf.h"

/* ********************************************************************** */

/*     SUBROUTINE FUNCTION CUMCHI(X,DF,CUM,CCUM) */
/*             CUMulative of the CHi-square distribution */


/*                              Function */


/*     Calculates the cumulative chi-square distribution. */


/*                              Arguments */


/*     X       --> Upper limit of integration of the */
/*                 chi-square distribution. */
/*                                                 X is DOUBLE PRECISION */

/*     DF      --> Degrees of freedom of the */
/*                 chi-square distribution. */
/*                                                 DF is DOUBLE PRECISION */

/*     CUM <-- Cumulative chi-square distribution. */
/*                                                 CUM is DOUBLE PRECISIO */

/*     CCUM <-- Compliment of Cumulative chi-square distribution. */
/*                                                 CCUM is DOUBLE PRECISI */


/*                              Method */


/*     Calls incomplete gamma function (CUMGAM) */

/* ********************************************************************** */


int cdf_cumchi (double *x, double *df, double *cum, double *ccum)
{
  double a;
  double xx;
  a = *df * .5;
  xx = *x * .5;
  cdf_cumgam (&xx, &a, cum, ccum);
  return 0;
}

