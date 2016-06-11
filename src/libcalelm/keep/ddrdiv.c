/* ddrdiv.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int
nsp_calpack_ddrdiv (double *a, int *ia, double *b, int *ib, double *r__,
		    int *ir, int *n, int *ierr)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int k, jb, ja, jr;

  /*!    purpose 
   *    computes r=a./b with a and b real 
   * 
   *    ia,ib,ir : increment between two consecutive element of vectors a 
   *               b and r 
   *    a        : array  containing vector a elements 
   *    b        : array  containing vector b elements 
   *    r        : array  containing vector r elements 
   *    n        : vectors length 
   *    ierr     : returned error flag: 
   *               o   : ok 
   *               <>0 : b(ierr)=0 
   *! 
   *    Copyright INRIA 
   */
  /* Parameter adjustments */
  --r__;
  --b;
  --a;

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
	  if (b[jb] == 0.)
	    {
	      *ierr = k;
	      /*              return 
	       */
	    }
	  r__[jr] = a[ja] / b[jb];
	  jr += *ir;
	  jb += *ib;
	  /* L10: */
	}
    }
  else if (*ib == 0)
    {
      if (b[jb] == 0.)
	{
	  *ierr = 1;
	  /*           return 
	   */
	}
      i__1 = *n;
      for (k = 1; k <= i__1; ++k)
	{
	  r__[jr] = a[ja] / b[jb];
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
	  if (b[jb] == 0.)
	    {
	      *ierr = k;
	      /*              return 
	       */
	    }
	  r__[jr] = a[ja] / b[jb];
	  jr += *ir;
	  jb += *ib;
	  ja += *ia;
	  /* L12: */
	}
    }
  return 0;
}				/* ddrdiv_ */
