#include "cdf.h"

/* ********************************************************************** */
/*     SUBROUTINE CUMPOI(S,XLAM,CUM,CCUM) */
/*                    CUMulative POIsson distribution */
/*                              Function */
/*     Returns the  probability  of  S   or  fewer events in  a   Poisson */
/*     distribution with mean XLAM. */
/*                              Arguments */
/*     S --> Upper limit of cumulation of the Poisson. */
/*                                                  S is DOUBLE PRECISION */
/*     XLAM --> Mean of the Poisson distribution. */
/*                                                  XLAM is DOUBLE PRECIS */
/*     CUM <-- Cumulative poisson distribution. */
/*                                        CUM is DOUBLE PRECISION */
/*     CCUM <-- Compliment of Cumulative poisson distribution. */
/*                                                  CCUM is DOUBLE PRECIS */
/*                              Method */
/*     Uses formula  26.4.21   of   Abramowitz and  Stegun,  Handbook  of */
/*     Mathematical   Functions  to reduce   the   cumulative Poisson  to */
/*     the cumulative chi-square distribution. */
/* ********************************************************************** */


int cdf_cumpoi (double *s, double *xlam, double *cum, double *ccum)
{
  double df;
  double chi;
  df = (*s + 1.) * 2.;
  chi = *xlam * 2.;
  cdf_cumchi (&chi, &df, ccum, cum);
  return 0;
}				/* cumpoi_ */
