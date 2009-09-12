#include "integ.h"

/*
 *this function computes the norm of a banded n by n matrix, 
 *stored in the array a, that is consistent with the weighted max-norm 
 *on vectors, with weights stored in the array w. 
 *ml and mu are the lower and upper half-bandwidths of the matrix. 
 *nra is the first dimension of the a array, nra .ge. ml+mu+1. 
 *in terms of the matrix elements a(i,j), the norm is given by.. 
 *  bnorm = Max(i=1,...,n) ( w(i) * sum(j=1,...,n) Abs(a(i,j))/w(j) ) 
 */

double
nsp_ode_bnorm (const int *n, const double *a, const int *nra,
		  const int *ml, const int *mu, const double *w)
{
  int a_dim1 = *nra, a_offset = a_dim1 + 1;
  int i, j, i3, jhi, jlo;
  double an, sum;

  --w;
  a -= a_offset;

  an = 0.;
  for (i = 1; i <= *n; ++i)
    {
      sum = 0.;
      i3 = i + *mu + 1;
      jlo = Max (i - *ml, 1);
      jhi = Min (i + *mu, *n);
      for (j = jlo; j <= jhi; ++j)
	{
	  sum += Abs (a[i3 - j + j * a_dim1]) / w[j];
	}
      an = Max (an, sum * w[i]);
    }
  return an;
}
