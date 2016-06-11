/* dwdiv.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int
nsp_calpack_dwdiv (double *ar, double *br, double *bi, double *cr,
		   double *ci, int *ierr)
{
  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double d__, s, bis, ars, brs;

  /*!but 
   * 
   *    This subroutine dwdiv computes c=a/b where a is a real number and 
   *     b a complex number 
   * 
   *!Calling sequence 
   * 
   *    subroutine dwdiv(ar,br,bi,cr,ci,ierr) 
   * 
   *    ar    : double precision. 
   * 
   *    br, bi: double precision, b real and complex parts. 
   * 
   *    cr, ci: double precision, c real and complex parts. 
   * 
   *!author 
   * 
   *    Serge Steer INRIA 
   * 
   *! 
   *    Copyright INRIA 
   *    c = a/b 
   * 
   */
  *ierr = 0;
  if (*bi == 0.)
    {
      *cr = *ar / *br;
      *ci = 0.;
    }
  else if (*br == 0.)
    {
      *ci = -(*ar) / *bi;
      *cr = 0.;
    }
  else
    {
      s = Abs (*br) + Abs (*bi);
      if (s == 0.)
	{
	  *ierr = 1;
	  *cr = *ar / s;
	  *ci = 0.;
	  return 0;
	}
      ars = *ar / s;
      brs = *br / s;
      bis = *bi / s;
      /*Computing 2nd power 
       */
      d__1 = brs;
      /*Computing 2nd power 
       */
      d__2 = bis;
      d__ = d__1 * d__1 + d__2 * d__2;
      *cr = ars * brs / d__;
      *ci = -ars * bis / d__;
    }
  return 0;
}				/* dwdiv_ */
