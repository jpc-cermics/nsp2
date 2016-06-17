#include "signal.h"

/*
 *    purposes  :  function tscccf computes lag crosscovariance cxy 
 *                 n-dimensioned time series x and y, x leading y, 
 *                 starting at lag 0. 
 * 
 *    parameters       on entry             on return 
 *    ----------       --------             --------- 
 *         x    :      time series x        unchanged 
 *         y    :      time series y        unchanged 
 *         n    :      series length        unchanged 
 *       cxy    :      unusedcrosscovariance function 
 *       lag    :      number of lags       unchanged 
 * 
 *    remarks   :  lag must be strictly positive, inferior to n. 
 * 
 *    error flag tscccf is set to the following values: 
 *          ierr =  0 :  no errors. 
 *          ierr = -1 :  wrong input parameter n or lag. 
 */

int signal_tscccf (const double *x,const double *y,const int *n, double *cxy, double *xymean,const int *lag, int *ierr)
{
  int j, k, m;
  double exx = 0.0 , eyy= 0.0;

  if (*lag <= 0 || *lag > *n || *n <= 0)
    {
      *ierr = -1;
      return 0;
    }

  for (k = 0 ; k < *n; ++k)
    {
      exx += x[k];
      eyy += y[k];
    }
  exx /= ((double) (*n));  xymean[0] = exx;
  eyy /= ((double) (*n));  xymean[1] = eyy;
  
  for (m = 0 ; m < *lag ; ++m)
    {
      cxy[m]=0.0;
      for (j = 0 ; j < *n - m ; ++j)
	{
	  cxy[m] += (x[j] - exx) * (y[j + m] - eyy);
	}
      cxy[m] /= (double) *n;
    }
  *ierr = 0;
  return 0;
}


