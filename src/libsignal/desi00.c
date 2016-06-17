#include "signal.h"

/*!purpose 
 *transform tolerance scheme 
 *! 
 * 
 * 
 */


int
signal_desi00 (int *ityp, double *om, int *norma, double *edeg, int *ndeg,
	       double *adeg, double *vsn, double *vd, double *a)
{

  /* Parameter adjustments */
  --om;

  /* Function Body */
  if (*ityp >= 3)
    {
      *ndeg = (*ndeg + 1) / 2;
    }
  if (*ndeg != 0)
    {
      *adeg = (double) (*ndeg) / (*edeg + 1.);
    }
  signal_transn (ityp, &om[1], norma, vsn, vd, a);
  return 0;
}				/* desi00_ */
