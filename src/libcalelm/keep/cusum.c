/* cusum.f -- translated by f2c (version 19961017). */

/*    Utility fct: cumulated sum 
 *    Copyright INRIA 
 */

#include "calpack.h"

int nsp_calpack_cusum (const int *n, double *w)
{
  int k;
  double t = 0.;
  for (k = 0; k < *n ; k++)
    {
      w[k] = t + w[k];
      t = w[k];
    }
  return 0;
}

