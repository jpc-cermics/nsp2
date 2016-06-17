/* nstabl.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*Scilab ( http://www.scilab.org/ ) - This file is part of Scilab 
 *Copyright (C) INRIA 
 * 
 *This file must be used under the terms of the CeCILL. 
 *This source file is licensed as described in the file COPYING, which 
 *you should have received as part of this distribution.  The terms 
 *are also available at 
 *http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.txt 
 */

int
signal_nstabl (double *a, int *n, double *w, int *ist)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, j, k, n1;
  double al;
  int nk, nk1;

  /*    test de stabilite 
   */
  /* Parameter adjustments */
  --w;
  --a;

  /* Function Body */
  *ist = 1;
  n1 = *n + 1;
  i__1 = n1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      w[i__] = a[i__];
      w[n1 + i__] = 0.;
      /* L1: */
    }
  k = 0;
  if (*n == 0)
    {
      goto L99;
    }
 L10:
  nk1 = *n - k + 1;
  i__1 = nk1;
  for (j = 1; j <= i__1; ++j)
    {
      /* L11: */
      w[n1 + j] = w[nk1 - j + 1];
    }
  if (w[n1 + nk1] == 0.)
    {
      return 0;
    }
  al = w[nk1] / w[n1 + nk1];
  if (Abs (al) >= 1.)
    {
      return 0;
    }
  nk = *n - k;
  i__1 = nk;
  for (j = 1; j <= i__1; ++j)
    {
      /* L12: */
      w[j] -= al * w[n1 + j];
    }
  ++k;
  if (k < *n)
    {
      goto L10;
    }
 L99:
  *ist = 0;
  return 0;
}				/* nstabl_ */
