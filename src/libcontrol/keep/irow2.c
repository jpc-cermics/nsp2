/* irow2.f -- translated by f2c (version 19961017).
 *
 *
 */


/*     int function irow2(i,m) 
 *     this routine is only to be called from syhsc 
 */

#include "ctrlpack.h"

int nsp_ctrlpack_irow2 (int *i__, int *m)
{
  int ret_val;
  int k;
  ret_val = ((*i__ - 1) << 1) * *m;
  k = (*i__ - 4) * (*i__ - 3) / 2;
  if (*i__ <= 2)
    {
      k = 0;
    }
  ret_val -= k;
  return ret_val;
}

