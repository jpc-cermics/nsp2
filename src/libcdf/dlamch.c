#include <strings.h>
#include <nsp/math.h>
#include "cdf.h"

double cdf_dlamch (char *cmach, long int cmach_len)
{
  return nsp_dlamch (cmach);
}

