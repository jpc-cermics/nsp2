/* cuproi.f -- translated by f2c (version 19961017). */

#include "calpack.h"

/*    Utility fct: cumulated product, complex argument 
 *    Copyright INRIA 
 */

int nsp_calpack_cuproi (const int *n, double *wr, double *wi)
{
  int k;
  double tr = 1., ti = 0.,  wwr;
  for (k = 0 ; k < *n ; k++) 
    {
      /*    w(k)=t*w(k)  */
      wwr = wr[k];
      wr[k] = tr * wwr - ti * wi[k];
      wi[k] = tr * wi[k] + ti * wwr;
      tr = wr[k];
      ti = wi[k];
    }
  return 0;
}

