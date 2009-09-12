#include "integ.h"

/*
 * this subroutine sets the error weight vector ewt according to 
 *    ewt(i) = rtol(i)*abs(ycur(i)) + atol(i),  i = 1,...,n, 
 * with the subscript on rtol and/or atol possibly replaced by 1 above, 
 * depending on the value of itol. 
 */

int
nsp_ode_ewset (const int *n,const int *itol, double *rtol,const double *atol,
	       const double *ycur, double *ewt)
{
  int i;
  double atoli = atol[0], rtoli = rtol[0];
  for (i = 0; i < *n; ++i)
    {
      if (*itol >= 3)
	{
	  rtoli = rtol[i];
	}
      if (*itol == 2 || *itol == 4)
	{
	  atoli = atol[i];
	}
      ewt[i] = rtoli * Abs (ycur[i]) + atoli;
    }
  return 0;
}
