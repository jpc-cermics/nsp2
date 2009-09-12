#include "integ.h"

/*
 * this function computes the norm of a full n by n matrix, 
 * stored in the array a, that is consistent with the weighted max-norm 
 * on vectors, with weights stored in the array w.. 
 * fnorm = Max(i=1,...,n) ( w(i) * sum(j=1,...,n) Abs(a(i,j))/w(j) ) 
 */

double nsp_ode_fnorm (int *n, double *a, double *w)
{
  int i, j;
  double an, sum;
  an = 0.;
  for (i = 0; i < *n; ++i)
    {
      sum = 0.;
      for (j = 0; j < *n; ++j)
	{
	  sum += Abs (a[i + (*n) * j]) / w[j];
	}
      an = Max (an, sum * w[i]);
    }
  return an;
}
