/* wdegre.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*/MEMBR ADD NAME=WDEGRE,SSI=0 
 *    Copyright INRIA 
 */
int nsp_ctrlpack_wdegre (double *ar, double *ai, int *majo, int *nvrai)
{
  /* System generated locals */
  int i__1;
  double d__1, d__2;

  /* Local variables */
  double test;
  int k, kk;

  /*   calcul du degre d un polynome a coefficients complexes 
   *   a=ar+i*ai=coeffs par ordre croissant 
   *   majo=majorant du degre 
   *   nvrai=degre calcule 
   */
  /* Parameter adjustments */
  --ai;
  --ar;

  /* Function Body */
  if (*majo == 0)
    {
      goto L20;
    }
  i__1 = *majo + 1;
  for (k = 1; k <= i__1; ++k)
    {
      kk = *majo + 2 - k;
      test = (d__1 = ar[kk], Abs (d__1)) + (d__2 = ai[kk], Abs (d__2));
      if (test + 1. != 1.)
	{
	  *nvrai = kk - 1;
	  return 0;
	}
      /* L10: */
    }
 L20:
  *nvrai = 0;
  return 0;
}				/* wdegre_ */
