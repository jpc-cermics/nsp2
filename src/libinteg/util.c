#include "integ.h"

double d_sign (const double *a, const double *b)
{
  double x = (*a >= 0 ? *a : -*a);
  return (*b >= 0 ? x : -x);
}

int i_sign (const int *a, const int *b)
{
  int x = (*a >= 0 ? *a : -*a);
  return (*b >= 0 ? x : -x);
}
