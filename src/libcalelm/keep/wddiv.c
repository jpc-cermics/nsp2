/* wddiv.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int
nsp_calpack_wddiv (double *ar, double *ai, double *br, double *cr,
		   double *ci, int *ierr)
{
  /*!but 
   * 
   *    This subroutine wddiv computes c=a/b where a is a complex number 
   *     and b a real number 
   * 
   *!Calling sequence 
   * 
   *    subroutine wddiv(ar,ai,br,bi,cr,ci,ierr) 
   * 
   *    ar, ai: double precision, a real and complex parts. 
   * 
   *    br, bi: double precision, b real and complex parts. 
   * 
   *    cr, ci: double precision, c real and complex parts. 
   * 
   *!author 
   * 
   *    Serge Steer 
   * 
   *! 
   *    Copyright INRIA 
   * 
   */
  *ierr = 0;
  if (*br == 0.)
    {
      *ierr = 1;
      /*        return 
       */
    }
  *cr = *ar / *br;
  *ci = *ai / *br;
  return 0;
}				/* wddiv_ */
