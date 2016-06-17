/* coeft.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*/MEMBR ADD NAME=COEFT,SSI=0 
 */
/* Subroutine */ int
signal_coeft (int *ordre, double *poler, double *polei, double *gain)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;
  double zi, zr, pim, pre;

  /*! 
   *sous-programme : coeft 
   *calcul du gain de la fonction de transfert 
   *et des coefficients de la decomposition en elements simples 
   *en double precision 
   *ecrit par gregoire de nanteuil le 08-10-1985 
   * 
   * 
   *                          parametres entrants 
   *     poler  : tableau des parties reelles des poles 
   *               de la fonction de transfert. 
   * 
   *     polei  : tableau des parties imaginaires des poles 
   *               de la fonction de transfert. 
   * 
   *                          parametres sortants 
   * 
   *     gain : coefficient de la fonction de transfert (gain) 
   * 
   *                          variables internes 
   *     zr,zi,pre,pim,i,j,ordre 
   *! 
   * 
   */
  /* Parameter adjustments */
  --polei;
  --poler;

  /* Function Body */
  zr = 1.;
  zi = 0.;
  i__1 = *ordre;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      pre = zr * (-poler[i__]) - zi * (-polei[i__]);
      pim = zi * (-poler[i__]) + zr * (-polei[i__]);
      zr = pre;
      zi = pim;
      /* L40: */
    }
  *gain = zr;
  /* L50: */
  return 0;
}				/* coeft_ */
