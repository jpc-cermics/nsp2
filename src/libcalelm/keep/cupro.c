/* cupro.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*     Utility fct: cumulated product 
 *     Copyright INRIA 
 */

int nsp_calpack_cupro (const int *n, double *w)
{
  int k;
  double t = 1.0;
  for (k = 0 ; k < *n ; k++)
    {
      w[k] = t * w[k];
      t = w[k];
    }
  return 0;
}


