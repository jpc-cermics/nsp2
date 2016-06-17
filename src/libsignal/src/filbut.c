/* filbut.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*/MEMBR ADD NAME=FILBUT,SSI=0 
 */
/* Subroutine */ int
signal_filbut (double *fmin, double *fmax, int *atmin, int *atmax, int *ordre,
	       double *fc, double *gain, double *poler, double *polei,
	       int *err)
{

  /*! 
   *sous-programme : filbut 
   *filtre a reponse impulsionnelle infinie 
   *calcul en double precision 
   *ecrit par gregoire de nanteuil le 08-10-1985 
   * 
   * 
   *                          parametres entrants 
   *   par l"utilisateur : 
   * 
   *     fmin : frequence de fin de bande passante. 
   * 
   *     fmax : frequence de debut de bande affaiblie. 
   * 
   *     atmin : amplitude crete a crete des ondulations 
   *           en bande passante en db . 
   * 
   *     atmax : amplitude des ondulations en bande affaiblie en db . 
   * 
   * 
   *                          parametres sortants 
   *     ordre : ordre du filtre. 
   * 
   *     fc    : frequence de coupure du filtre. 
   * 
   *     gain  : gain de la fonction de transfert. 
   * 
   *     poler  : tableau des parties reelles des poles 
   *               de la fonction de transfert. 
   * 
   *     polei  : tableau des parties imaginaires des poles 
   *               de la fonction de transfert. 
   * 
   *     err    : variable contenant le type d"erreur rencontree 
   * 
   *     subroutines appelees frequen, poles, coeft 
   *! 
   * 
   */
  /* Parameter adjustments */
  --polei;
  --poler;

  /* Function Body */
  /* L1: */
  if (*fmin >= *fmax)
    {
      *err = 36;
    }
  if (*err > 0)
    {
      return 0;
    }
  if (*fmin <= 0. || *fmax >= 1.)
    {
      *err = 36;
    }
  if (*err > 0)
    {
      return 0;
    }
  if (*atmin >= *atmax)
    {
      *err = 36;
    }
  if (*err > 0)
    {
      return 0;
    }
  if (*atmin <= 0)
    {
      *err = 36;
    }
  if (*err > 0)
    {
      return 0;
    }
  /* L10: */
  /* 
   */
  signal_freque (fmin, fmax, atmin, atmax, ordre, fc, err);
  signal_poles (ordre, fc, &poler[1], &polei[1]);
  signal_coeft (ordre, &poler[1], &polei[1], gain);
  /* L100: */
  return 0;
}				/* filbut_ */
