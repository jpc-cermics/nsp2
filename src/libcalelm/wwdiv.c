#include "calpack.h"

  /* 
   *       complex division algorithm : compute c := a / b 
   *       where : 
   * 
   *       a = ar + i ai 
   *       b = br + i bi 
   *       c = cr + i ci 
   * 
   *       inputs  : ar, ai, br, bi  (double precision) 
   *       outputs : cr, ci          (double precision) 
   *                 ierr  (int) ierr = 1 if b = 0 (else 0) 
   * 
   *    IMPLEMENTATION NOTES 
   *       1/ Use scaling with ||b||_oo; the original wwdiv.f used a scaling 
   *          with ||b||_1.  It results fewer operations.  From the famous 
   *          Golberg paper.  This is known as Smith's method. 
   *       2/ Currently set c = NaN + i NaN in case of a division by 0 ; 
   *          is that the good choice ? 
   * 
   *    AUTHOR 
   *       Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr> 
   */

int
nsp_calpack_wwdiv (double *ar, double *ai, double *br, double *bi,
		   double *cr, double *ci, int *ierr)
{
  double d, r;

  *ierr = 0;
  /*    Treat special cases */
  if (*bi == 0.)
    {
      if (*br == 0.)
	{
	  *ierr = 1;
	  /*  got NaN + i NaN  */
	  *cr = *bi / *br;
	  *ci = *cr;
	}
      else
	{
	  *cr = *ar / *br;
	  *ci = *ai / *br;
	}
    }
  else if (*br == 0.)
    {
      *cr = *ai / *bi;
      *ci = -(*ar) / *bi;
    }
  else
    {
      /*    Generic division algorithm */
      if (Abs (*br) >= Abs (*bi))
	{
	  r = *bi / *br;
	  d = *br + r * *bi;
	  *cr = (*ar + *ai * r) / d;
	  *ci = (*ai - *ar * r) / d;
	}
      else
	{
	  r = *br / *bi;
	  d = *bi + r * *br;
	  *cr = (*ar * r + *ai) / d;
	  *ci = (*ai * r - *ar) / d;
	}
    }
  return 0;
}

