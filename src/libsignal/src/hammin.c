/* hammin.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*/MEMBR ADD NAME=HAMMIN,SSI=0 
 */
/* Subroutine */ int
signal_hammin (int *ordr, int *demi, int *ieo, double *alph, double *win)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double beta;
  int compt;
  double twopi, pi, xcompt, ycompt;

  /*! 
   *sous-programme:  hammin 
   *fenetre de hamming generalisee 
   *en double precision 
   *acheve le 05/12/85 
   *ecrit par philippe touron 
   * 
   * 
   *                 parametres entrants 
   *                ------------------- 
   *demi, ordre de la demi-fenetre (entier) 
   *ordr, ordre du filtre (entier) 
   *ieo, indicateur de parite (entier) 
   *alph, 1ere constante de la fenetre de hamming (reelle) 
   * 
   *                 parametres sortants 
   *                ------------------- 
   *win, les valeurs de la demi-fenetre (tableau de reels qui doit 
   *   dans un programme appelant etre dimensionne a ordr) 
   * 
   *                 variables internes 
   *                ------------------ 
   *xcompt,ycompt,compt, compteur de boucle et indice de tableau en reel 
   *        (resp en entier) 
   * 
   *sous programmes appeles: aucun 
   *! 
   * 
   */
  /* Parameter adjustments */
  --win;

  /* Function Body */
  pi = acos (-1.);
  twopi = pi * 2.;
  beta = 1. - *alph;
  ycompt = (double) (*ordr) - 1.;
  i__1 = *demi;
  for (compt = 1; compt <= i__1; ++compt)
    {
      xcompt = (double) compt - 1.;
      if (*ieo == 0)
	{
	  xcompt += .5;
	}
      win[compt] = *alph + beta * cos (twopi * xcompt / ycompt);
      /* L10: */
    }
  return 0;
}				/* hammin_ */
