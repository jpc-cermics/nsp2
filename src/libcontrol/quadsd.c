/* quadsd.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*/MEMBR ADD NAME=QUADSD,SSI=0 
 */
int
nsp_ctrlpack_quadsd (int *nn, double *u, double *v, double *p, double *q,
		     double *a, double *b)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double c__;
  int i__;

  /*divides p by the quadratic  1,u,v placing the 
   *quotient in q and the remainder in a,b 
   */
  /* Parameter adjustments */
  --q;
  --p;

  /* Function Body */
  *b = p[1];
  q[1] = *b;
  *a = p[2] - *u * *b;
  q[2] = *a;
  i__1 = *nn;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      c__ = p[i__] - *u * *a - *v * *b;
      q[i__] = c__;
      *b = *a;
      *a = c__;
      /* L10: */
    }
  return 0;
}				/* quadsd_ */
