/* poles.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*/MEMBR ADD NAME=POLES,SSI=0 
 */
/* Subroutine */ int
signal_poles (int *ordre, double *fc, double *poler, double *polei)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;
  double pi;

  /*! 
   *sous-programme : poles 
   *calcul des poles de la fonction de transfert 
   *calcul en double precision 
   *ecrit par gregoire de nanteuil le 08-10-1985 
   * 
   * 
   *                          parametres entrants 
   *      ordre : ordre du filtre 
   * 
   *         fc : frquence de coupure 
   * 
   *                          parametres sortants 
   *     poler  : tableau des parties reelles des poles 
   *               de la fonction de transfert. 
   * 
   *     polei  : tableau des parties imaginaires des poles 
   *               de la fonction de transfert. 
   * 
   *                          variables internes 
   *     i 
   *! 
   * 
   */
  /* Parameter adjustments */
  --polei;
  --poler;

  /* Function Body */
  pi = 3.141592653589793;
  /* 
   */
  i__1 = *ordre - 1;
  for (i__ = 0; i__ <= i__1; ++i__)
    {
      poler[i__ + 1] = -(*fc) * sin (pi * ((i__ << 1) + 1) / (*ordre << 1));
      polei[i__ + 1] = *fc * cos (pi * ((i__ << 1) + 1) / (*ordre << 1));
      /* L30: */
    }
  return 0;
}				/* poles_ */
