#include <math.h>
#include "calpack.h"

double nsp_calpack_exp (double x)
{
  double Limit = 16;
  if (x < Limit)
    {
      return exp (x);
    }
  else
    {
      return exp (Limit) * (x + 1 - Limit);
    };
}

double nsp_calpack_log (double x)
{
  double eps = 1e-10;
  if (Abs (x) > eps)
    {
      return log (Abs (x));
    }
  else
    {
      return (Abs (x) / eps) + log (eps) - 1;
    };

}

double nsp_calpack_pow (double x, double y)
{
  return nsp_calpack_exp (y * nsp_calpack_log (x));
}
