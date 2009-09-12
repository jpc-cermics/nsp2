#include "integ.h"

/*
 *this function routine computes the weighted max-norm 
 *of the vector of length n contained in the array v, with weights 
 *contained in the array w of length n.. 
 *  vmnorm = Max(i=1,...,n) Abs(v(i))*w(i) 
 */

double nsp_ode_vmnorm (const int *n, const double *v, const double *w)
{
  int i;
  double vm = 0.;
  for (i = 0; i < *n; ++i)
    {
      vm = Max (vm, Abs (v[i]) * w[i]);
    }
  return vm;
}
