#include "cdf.h"

/* ********************************************************************** */
/*     SUBROUTINE CUMGAM(X,A,CUM,CCUM) */
/*           Double precision cUMulative incomplete GAMma distribution */
/*                              Function */
/*     Computes   the  cumulative        of    the     incomplete   gamma */
/*     distribution, i.e., the integral from 0 to X of */
/*          (1/GAM(A))*EXP(-T)*T**(A-1) DT */
/*     where GAM(A) is the complete gamma function of A, i.e., */
/*          GAM(A) = integral from 0 to infinity of */
/*                    EXP(-T)*T**(A-1) DT */
/*                              Arguments */
/*     X --> The upper limit of integration of the incomplete gamma. */
/*                                                X is DOUBLE PRECISION */
/*     A --> The shape parameter of the incomplete gamma. */
/*                                                A is DOUBLE PRECISION */
/*     CUM <-- Cumulative incomplete gamma distribution. */
/*                                        CUM is DOUBLE PRECISION */
/*     CCUM <-- Compliment of Cumulative incomplete gamma distribution. */
/*                                                CCUM is DOUBLE PRECISIO */
/*                              Method */
/*     Calls the routine GRATIO. */
/* ********************************************************************** */




int cdf_cumgam (double *x, double *a, double *cum, double *ccum)
{
  const int c__0 = 0;
  if (!(*x <= 0.))
    {
      goto L10;
    }
  *cum = 0.;
  *ccum = 1.;
  return 0;
 L10:
  cdf_gratio (a, x, cum, ccum, &c__0);
  return 0;
} 

