/* ivimp.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int nsp_calpack_ivimp (int *i1, int *i2, int *pas, int *iv)
{
  /* System generated locals */
  int i__1, i__2;

  /* Local variables */
  int i__, k;

  /*    generate iv=i1:pas:i2 
   *    Copyright INRIA 
   */
  /* Parameter adjustments */
  --iv;

  /* Function Body */
  k = 0;
  i__1 = *i2;
  i__2 = *pas;
  for (i__ = *i1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2)
    {
      ++k;
      iv[k] = i__;
      /* L10: */
    }
  return 0;
}				/* ivimp_ */
