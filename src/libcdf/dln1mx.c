#include "cdf.h"

/* ********************************************************************** */

/*     DOUBLE PRECISION FUNCTION DLN1MX(X) */
/*               Double precision LN(1-X) */


/*                              Function */


/*     Returns ln(1-x) for small x (good accuracy if x .le. 0.1). */
/*     Note that the obvious code of */
/*               LOG(1.0-X) */
/*     won't work for small X because 1.0-X loses accuracy */


/*                              Arguments */


/*     X --> Value for which ln(1-x) is desired. */
/*                                        X is DOUBLE PRECISION */


/*                              Method */


/*     If X > 0.1, the obvious code above is used ELSE */
/*     The Taylor series for 1-x is expanded to 20 terms. */

/* ********************************************************************** */


double cdf_dln1mx (double *x)
{
  double ret_val, d__1;
  d__1 = -(*x);
  ret_val = cdf_dln1px (&d__1);
  return ret_val;
}
