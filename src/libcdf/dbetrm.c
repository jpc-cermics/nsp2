#include "cdf.h"

/* ********************************************************************** */

/*     DOUBLE PRECISION FUNCTION DBETRM( A, B ) */
/*          Double Precision Sterling Remainder for Complete */
/*                    Beta Function */


/*                              Function */


/*     Log(Beta(A,B)) = Lgamma(A) + Lgamma(B) - Lgamma(A+B) */
/*     where Lgamma is the log of the (complete) gamma function */

/*     Let ZZ be approximation obtained if each log gamma is approximated */
/*     by Sterling's formula, i.e., */
/*     Sterling(Z) = LOG( SQRT( 2*PI ) ) + ( Z-0.5 ) * LOG( Z ) - Z */

/*     Returns Log(Beta(A,B)) - ZZ */


/*                              Arguments */


/*     A --> One argument of the Beta */
/*                    DOUBLE PRECISION A */

/*     B --> The other argument of the Beta */
/*                    DOUBLE PRECISION B */

/* ********************************************************************** */

double cdf_dbetrm (double *a, double *b)
{
  double ret_val, d__1;
  /*     Try to sum from smallest to largest */
  d__1 = *a + *b;
  ret_val = -cdf_dstrem (&d__1);
  d__1 = Max (*a, *b);
  ret_val += cdf_dstrem (&d__1);
  d__1 = Min (*a, *b);
  ret_val += cdf_dstrem (&d__1);
  return ret_val;
}				/* dbetrm_ */
