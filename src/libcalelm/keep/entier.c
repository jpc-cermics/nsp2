/* entier.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=ENTIER,SSI=0 
 *    Copyright INRIA 
 */
int nsp_calpack_entier (int *n, double *d__, int *s)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;

  /*! 
   */
  /* Parameter adjustments */
  --s;
  --d__;

  /* Function Body */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s[i__] = (int) d__[i__];
      /* L10: */
    }
  return 0;
}				/* entier_ */
