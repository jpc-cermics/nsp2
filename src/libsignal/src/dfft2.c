/* dfft2.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/* Subroutine */ int
signal_dfft2 (double *a, double *b, int *nseg, int *n, int *nspn, int *isn,
	      int *ierr, int *iw, int *lw)
{
  double *r;
  /*! 
   *fft avec tableau de travail iw 
   * 
   *    voir la routine fft de singleton pour l'appel 
   *    iw est ici un tableau de travail a dimensionner a lw 
   *    dans le pgm appelant.Si lw est trop petit on sort avec 
   *    ierr < 0 et -ierr est le manque de place necessaire pour 
   *    poursuivre le calcul 
   *! 
   */
  /* Parameter adjustments */
  --iw;
  --b;
  --a;

  /* Function Body */
  iw[1] = 0;
  iw[2] = 10;
  iw[3] = 10;
  iw[4] = *lw;
  iw[5] = 10;
  r = (double *) &iw[1];
  signal_dfftbi (&a[1], &b[1], nseg, n, nspn, isn, ierr, &iw[1], &iw[2],
		 &iw[3], &iw[4], &iw[5], r, &iw[1]);
  return 0;
}
