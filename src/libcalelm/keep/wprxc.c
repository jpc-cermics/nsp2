/* wprxc.f -- translated by f2c (version 19961017).
  *
 *
 */

#include "calpack.h"

/* Table of constant values */

static double c_b2 = 0.;
static int c__1 = 1;

/*/MEMBR ADD NAME=WPRXC,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_calpack_wprxc (int *n, double *rootr, double *rooti, double *coeffr,
		   double *coeffi)
{
  /* System generated locals */
  int i__1;
  double d__1, d__2;

  /* Local variables */
  int j;
  int nj;

/*!but 
 *    wprxc calcule les coefficients d'un polynome defini par ses 
 *    racines (complexes) et dont le coefficient de degre maximum est 1 
 * 
 *!liste d'appel 
 *     subroutine wprxc(n,rootr,rooti,coeffr,coeffi) 
 *    double precision rootr(n),rooti(n),coeffr(n+1),coeffi(n+1) 
 *    int n 
 * 
 *    n     : degre du polynome 
 *    root : tableau contenant les racines 
 *    coeff : tableau contenant les coefficients du polynome, ranges 
 *            par odre croissant 
 *!sous programmes appeles 
 *    dset waxpy (blas) 
 *!origine 
 *    serge Steer INRIA 
 *! 
 * 
 * 
 */
  /* Parameter adjustments */
  --rooti;
  --rootr;
  --coeffr;
  --coeffi;

  /* Function Body */
  nsp_dset (n, &c_b2, &coeffr[1], &c__1);
  i__1 = *n + 1;
  nsp_dset (&i__1, &c_b2, &coeffi[1], &c__1);
  coeffr[*n + 1] = 1.;
/* 
 */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      nj = *n + 1 - j;
      d__1 = -rootr[j];
      d__2 = -rooti[j];
      nsp_calpack_waxpy (&j, &d__1, &d__2, &coeffr[nj + 1], &coeffi[nj + 1],
			 &c__1, &coeffr[nj], &coeffi[nj], &c__1);
/* L10: */
    }
/* 
 */
  return 0;
}				/* wprxc_ */
