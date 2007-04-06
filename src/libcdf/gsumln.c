#include "cdf.h"

/* ----------------------------------------------------------------------- */
/*          EVALUATION OF THE FUNCTION LN(GAMMA(A + B)) */
/*          FOR 1 .LE. A .LE. 2  AND  1 .LE. B .LE. 2 */
/* ----------------------------------------------------------------------- */

double cdf_gsumln (double *a, double *b)
{
  double ret_val, d__1;
  double x;

  x = *a + *b - 2.;
  if (x > .25)
    {
      goto L10;
    }
  d__1 = x + 1.;
  ret_val = cdf_gamln1 (d__1);
  return ret_val;
L10:
  if (x > 1.25)
    {
      goto L20;
    }
  ret_val = cdf_gamln1 (x) + cdf_alnrel (x);
  return ret_val;
L20:
  d__1 = x - 1.;
  ret_val = cdf_gamln1 (d__1) + log (x * (x + 1.));
  return ret_val;
}				/* gsumln_ */
