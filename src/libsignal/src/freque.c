/* freque.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/* Table of constant values */

int
i_dnnt (double *x)
{
  return (int) (*x >= 0. ? floor (*x + .5) : -floor (.5 - *x));
}

static double c_b2 = 10.;

/*/MEMBR ADD NAME=FREQUE,SSI=0 
 */
/* Subroutine */ int
signal_freque (double *fmin, double *fmax, int *atmin, int *atmax, int *ordre,
	       double *fc, int *err)
{
  /* System generated locals */
  double d__1, d__2, d__3, d__4, d__5;

  /*! 
   *sous-programme : frequen 
   *calcul de l'ordre et de la frequence de coupure du filtre 
   *calcul en double precision 
   *ecrit par gregoire de nanteuil le 08-10-1985 
   * 
   * 
   *                          parametres entrants 
   *      fmin : frequence de fin de bande passante [0.;0.5] 
   * 
   *      fmax : frequence de debut de bande affaiblie [0.;0.5] 
   * 
   *     atmin : amplitude crete a crete des ondulations 
   *                        en bande passante en db 
   * 
   *     atmax : amplitudes des ondulations en bande affaiblie en db 
   * 
   *                          parametres sortants 
   *     ordre : ordre du filtre 
   * 
   *        fc : frequence de coupure 
   * 
   *     err   : variable contenant le type d"erreur rencontree 
   *! 
   * 
   */
  d__3 = *atmax / 10.;
  d__4 = *atmin / 10.;
  d__2 = (pow_dd (&c_b2, &d__3) - 1) / (pow_dd (&c_b2, &d__4) - 1);
  d__5 = *fmax / *fmin;
  d__1 = log10 (d__2) / (log10 (d__5) * 2.);
  *ordre = i_dnnt (&d__1);
  if (*ordre <= 0)
    {
      *err = 36;
    }
  if (*err > 0)
    {
      return 0;
    }
  d__3 = *atmax / 10.;
  d__2 = pow_dd (&c_b2, &d__3) - 1;
  d__1 = log10 (d__2) / (*ordre << 1);
  *fc = *fmax / pow_dd (&c_b2, &d__1);
  if (*fc <= 0.)
    {
      *err = 36;
    }
  if (*err > 0)
    {
      return 0;
    }
  /* L100: */
  return 0;
}				/* freque_ */
