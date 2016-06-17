/* cheby.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*/MEMBR ADD NAME=CHEBY,SSI=0 
 */
/* Subroutine */ int
signal_cheby (int *ordr, int *demi, int *ieo, double *dp, double *x0,
	      double *tam, double *win)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  double gama;
  int indi;
  double cres, teta, freq, somm, xindi;
  int compt;
  double c0, c1, c2, xordr, twopi, pi;
  int icompt, jcompt;
  double xcompt, twn;

  /*! 
   *sous-programme: cheby 
   *fenetre de dolph chebyshev 
   *en double precision 
   *acheve le 05/12/85 
   *ecrit par philippe touron 
   * 
   * 
   *                 parametres entrants 
   *                ------------------- 
   *ordr, l'ordre du filtre (entier) 
   *demi, l'ordre de la demi fenetre (entier) 
   *ieo, indicateur de parite vaut 0 si ordr pair et 1 sinon (entier) 
   *dp, attenuation en absolu (reelle) 
   *x0, constante de la fenetre de chebyshev fonction de 
   *     la largeur du lobe principale (reelle) 
   *tam, tableau de travail (reels) qui doit etre dans un programme 
   *   appelant dimensionne a 3 fois ordr) 
   * 
   *                 parametres sortants 
   *                ------------------- 
   *win, tableau de valeurs de la fenetre (reelles) qui doit dans 
   *   un programme appelant etre dimensionne a ordr 
   * 
   *                 variables internes 
   *                ------------------ 
   *c0,c1,c2, des buffers de calcul (reels) 
   *compt,indi,xcompt,xindi, compteurs de boucles (entiers et reels) 
   *teta,gama, coefficients de la fenetres de chebyshev (reels) 
   * 
   *sous programmes appele: coshin 
   * 
   *! 
   * 
   */
  /* Parameter adjustments */
  --win;
  --tam;

  /* Function Body */
  xordr = (double) (*ordr);
  teta = (*x0 + 1.) / 2.;
  gama = (*x0 - 1.) / 2.;
  pi = acos (-1.);
  twopi = pi * 2.;
  c0 = (xordr - 1.) / 2.;
  i__1 = *ordr;
  for (compt = 1; compt <= i__1; ++compt)
    {
      icompt = *ordr + compt;
      jcompt = icompt + *ordr;
      xcompt = (double) compt - 1.;
      c1 = xcompt / xordr;
      freq = teta * cos (twopi * c1) + gama;
      cres = Abs (freq) - 1.;
      if (cres <= 0.)
	{
	  goto L10;
	}
      else
	{
	  goto L20;
	}
    L10:
      tam[icompt] = *dp * cos (c0 * acos (freq));
      goto L30;
    L20:
      tam[icompt] = *dp * cosh (c0 * nsp_calpack_coshin (&freq));
    L30:
      tam[jcompt] = 0.;
      /* 
       *modification si filtre d'ordr pair 
       * 
       */
      if (*ieo == 1)
	{
	  goto L40;
	}
      tam[jcompt] = -tam[icompt] * sin (pi * c1);
      tam[icompt] *= cos (pi * c1);
      if (compt > *ordr / 2 + 1)
	{
	  tam[icompt] = -tam[icompt];
	  tam[jcompt] = -tam[jcompt];
	}
    L40:
      ;
    }
  /* 
   *transformee de fourrier inverse 
   * 
   */
  twn = twopi / xordr;
  i__1 = *demi;
  for (compt = 1; compt <= i__1; ++compt)
    {
      xcompt = (double) compt - 1.;
      somm = 0.;
      i__2 = *ordr;
      for (indi = 1; indi <= i__2; ++indi)
	{
	  icompt = indi + *ordr;
	  jcompt = icompt + *ordr;
	  xindi = (double) indi - 1.;
	  somm =
	    somm + tam[icompt] * cos (twn * xindi * xcompt) +
	    tam[jcompt] * sin (twn * xindi * xcompt);
	  /* L50: */
	}
      win[compt] = somm;
      /* L60: */
    }
  c2 = win[1];
  i__1 = *demi;
  for (compt = 1; compt <= i__1; ++compt)
    {
      win[compt] /= c2;
      /* L70: */
    }
  return 0;
}				/* cheby_ */
