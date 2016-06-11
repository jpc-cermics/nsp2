/* lrow2.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

int nsp_ctrlpack_lrow2 (int *i__, int *m)
{
  /* System generated locals */
  int ret_val;

  /* Local variables */
  int k;

  /*%calling sequence 
   *       int function lrow2(i,m) 
   *% purpose 
   *     this routine is only to be called from syhsc 
   *% 
   */
  k = *i__ - 3;
  if (*i__ <= 2)
    {
      k = 0;
    }
  ret_val = (*m << 1) - k;
  if (*i__ <= 2)
    {
      ret_val = *m << 1;
    }
  return ret_val;
}				/* lrow2_ */
