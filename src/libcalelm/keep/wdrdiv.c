/* wdrdiv.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int
nsp_calpack_wdrdiv (double *ar, double *ai, int *ia, double *br, int *ib,
		    double *rr, double *ri, int *ir, int *n, int *ierr)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int ierr1, k;
  int jb, ja, jr;
  double wi, wr;

  /*!    purpose 
   *    computes r=a./b with a real vector and b complex vector 
   * 
   *    ia,ib,ir : increment between two consecutive element of vectors a 
   *               b and r 
   *    ar,ai    : arrays containing a real and imaginary parts 
   *    br       : array  containing b vector 
   *    rr,ri    : arrays containing r real and imaginary parts 
   *    n        : vectors length 
   *    ierr     : returned error flag: 
   *               o   : ok 
   *               <>0 : b(ierr)=0 
   *! 
   *    Copyright INRIA 
   *    wr, wi used because rr, ri may share same mem as ar,ai or br,bi 
   */
  /* Parameter adjustments */
  --ri;
  --rr;
  --br;
  --ai;
  --ar;

  /* Function Body */
  jr = 1;
  jb = 1;
  ja = 1;
  *ierr = 0;
  if (*ia == 0)
    {
      i__1 = *n;
      for (k = 1; k <= i__1; ++k)
	{
	  nsp_calpack_wddiv (&ar[ja], &ai[ja], &br[jb], &wr, &wi, &ierr1);
	  rr[jr] = wr;
	  ri[jr] = wi;
	  if (ierr1 != 0)
	    {
	      *ierr = k;
	      /*              return 
	       */
	    }
	  jr += *ir;
	  jb += *ib;
	  /* L10: */
	}
    }
  else if (*ib == 0)
    {
      if (br[jb] == 0.)
	{
	  *ierr = 1;
	  /*           return 
	   */
	}
      i__1 = *n;
      for (k = 1; k <= i__1; ++k)
	{
	  nsp_calpack_wddiv (&ar[ja], &ai[ja], &br[jb], &wr, &wi, &ierr1);
	  rr[jr] = wr;
	  ri[jr] = wi;
	  jr += *ir;
	  ja += *ia;
	  /* L11: */
	}
    }
  else
    {
      i__1 = *n;
      for (k = 1; k <= i__1; ++k)
	{
	  nsp_calpack_wddiv (&ar[ja], &ai[ja], &br[jb], &wr, &wi, &ierr1);
	  rr[jr] = wr;
	  ri[jr] = wi;
	  if (ierr1 != 0)
	    {
	      *ierr = k;
	      /*              return 
	       */
	    }
	  jr += *ir;
	  jb += *ib;
	  ja += *ia;
	  /* L12: */
	}
    }
  return 0;
}				/* wdrdiv_ */
