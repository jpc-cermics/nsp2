#include "calpack.h"

/* Table of constant values */

static double c_b2 = 0.;
static int c__1 = 1;

/*   Copyright INRIA  */

int nsp_calpack_dprxc (int *n, double *roots, double *coeff)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  int j;
  int nj;

/*!but 
 *    dpxrc calcule les coefficients d'un polynome defini par ses 
 *    racines (reelles) et dont le coefficient de degre maximum est 1 
 * 
 *!liste d'appel 
 *     subroutine dpxrc(n,roots,coeff) 
 *    double precision roots(n),coeff(n+1) 
 *    int n 
 * 
 *    n     : degre du polynome 
 *    roots : tableau contenant les racines 
 *    coeff : tableau contenant les coefficients du polynome, ranges 
 *            par odre croissant 
 *!sous programmes appeles 
 *    dset daxpy (blas) 
 *!origine 
 *    serge Steer INRIA 
 *! 
 * 
 * 
 */
  /* Parameter adjustments */
  --roots;
  --coeff;

  /* Function Body */
  nsp_dset (n, &c_b2, &coeff[1], &c__1);
  coeff[*n + 1] = 1.;
/* 
 */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      nj = *n + 1 - j;
      d__1 = -roots[j];
      C2F (daxpy) (&j, &d__1, &coeff[nj + 1], &c__1, &coeff[nj], &c__1);
      /* L10: */
    }
  /* 
   */
  return 0;
}				/* dprxc_ */
