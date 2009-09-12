#include "integ.h"

/*
 *this function routine computes the weighted root-mean-square norm 
 *of the vector of length n contained in the array v, with weights 
 *contained in the array w of length n.. 
 *  vnorm = sqrt( (1/n) * sum( v(i)*w(i) )**2 ) 
 */

double nsp_ode_vnorm (const int *n, const double *v, const double *w)
{
  int i;
  double sum = 0.;
  for (i = 0; i < *n; ++i)
    {
      double d1 = v[i] * w[i];
      sum += d1 * d1;
    }
  return sqrt (sum / (double) (*n));
}
