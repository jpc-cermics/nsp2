/* ino.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*/MEMBR ADD NAME=INO,SSI=0 
 */
double
signal_ino (double *x)
{
  /* System generated locals */
  double ret_val;

  /* Local variables */
  double prec, incr, term, somm, y;
  int compt;
  double xcompt;

  /*! 
   *fonction:  ino 
   *fonction de bessel pour la fenetre de kaiser 
   *en double precision 
   *acheve 05/12/85 
   *ecrit par philippe touron 
   * 
   * 
   *                 paramatres entrants 
   *                ------------------- 
   *x, variable reelle 
   * 
   *                 parametres sortants 
   *                ------------------- 
   *ino(x), valeur de la fonction de bessel (reelle) 
   * 
   *                 variables internes 
   *                ------------------ 
   *y, variable reelle egale a x/2 
   *incr, increment reel de la fonction de bessel 
   *term, valeur reelle du terme general de la serie de bessel 
   *somm, fonction de bessel a la n-ieme iteration (reel) 
   *prec, si le rapport (term/somm) < 1.0**-8, le calcul est stoppe !! 
   *compt, compteur de boucle (entier) 
   *xcompt, valeur de compt en double precision 
   * 
   *sous programmes appele: aucun 
   *! 
   * 
   */
  y = *x / 2.;
  prec = 1e-8;
  somm = 1.;
  incr = 1.;
  for (compt = 1; compt <= 25; ++compt)
    {
      xcompt = (double) compt;
      incr = incr * y / xcompt;
      term = incr * incr;
      somm += term;
      if (somm * prec - term <= 0.)
	{
	  goto L10;
	}
      else
	{
	  goto L20;
	}
    L10:
      ;
    }
 L20:
  ret_val = somm;
  return ret_val;
}				/* ino_ */
