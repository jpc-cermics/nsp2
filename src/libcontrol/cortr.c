/* cortr.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *    cortr accumulate the  unitary similarities performed by corth 
 *
 *     subroutine cortr(nm,n,low,igh,hr,hi,ortr,orti,zr,zi) 
 *    on input. 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement. 
 *       n is the order of the matrix. 
 *       low and igh are ints determined by the balancing 
 *         subroutine  cbal.  if  cbal  has not been used, 
 *         set low=1, igh=n. 
 *       hr and hi contain the real and imaginary parts, 
 *         respectively, of the complex upper hessenberg matrix. 
 *         their lower triangles below the subdiagonal contain further 
 *         information about the transformations which were used in the 
 *         reduction by  corth, if performed.  if the eigenvectors of 
 *         the hessenberg matrix are desired, these elements may be 
 *         arbitrary. 
 *    on output. 
 *       zr and zi contain the real and imaginary parts, 
 *         respectivelyof the tranformations performed 
 */

int
nsp_ctrlpack_cortr (int *nm, int *n, int *low, int *igh, double *hr,
		    double *hi, double *ortr, double *orti, double *zr,
		    double *zi)
{
  /* System generated locals */
  int hr_dim1, hr_offset, hi_dim1, hi_offset, zr_dim1, zr_offset, zi_dim1,
    zi_offset, i__1, i__2, i__3;

  /* Local variables */
  int iend;
  double norm;
  int i__, j, k, ii;
  double si, sr;
  int ip1;

  /* Parameter adjustments */
  zi_dim1 = *nm;
  zi_offset = zi_dim1 + 1;
  zi -= zi_offset;
  zr_dim1 = *nm;
  zr_offset = zr_dim1 + 1;
  zr -= zr_offset;
  hi_dim1 = *nm;
  hi_offset = hi_dim1 + 1;
  hi -= hi_offset;
  hr_dim1 = *nm;
  hr_offset = hr_dim1 + 1;
  hr -= hr_offset;
  --orti;
  --ortr;

  /* Function Body */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  zr[i__ + j * zr_dim1] = 0.;
	  zi[i__ + j * zi_dim1] = 0.;
	  if (i__ == j)
	    {
	      zr[i__ + j * zr_dim1] = 1.;
	    }
	  /* L100: */
	}
    }
  /*    .......... form the matrix of accumulated transformations 
   *               from the information left by corth .......... 
   */
  iend = *igh - *low - 1;
  if (iend <= 0)
    {
      goto L150;
    }
  else
    {
      goto L105;
    }
  /*    .......... for i=igh-1 step -1 until low+1 do -- .......... 
   */
 L105:
  i__2 = iend;
  for (ii = 1; ii <= i__2; ++ii)
    {
      i__ = *igh - ii;
      /*x         if (ortr(i) .eq. 0.0d+0 .and. orti(i) .eq. 0.0d+0) go to 140 
       *x         if (hr(i,i-1).eq.0.0d+0 .and. hi(i,i-1).eq.0.0d+0) go to 140 
       *    .......... norm below is negative of h formed in corth .......... 
       */
      norm =
	hr[i__ + (i__ - 1) * hr_dim1] * ortr[i__] + hi[i__ +
						       (i__ -
							1) * hi_dim1] *
	orti[i__];
      if (norm == 0.)
	{
	  goto L140;
	}
      ip1 = i__ + 1;
      /* 
       */
      i__1 = *igh;
      for (k = ip1; k <= i__1; ++k)
	{
	  ortr[k] = hr[k + (i__ - 1) * hr_dim1];
	  orti[k] = hi[k + (i__ - 1) * hi_dim1];
	  /* L110: */
	}
      /* 
       */
      i__1 = *igh;
      for (j = i__; j <= i__1; ++j)
	{
	  sr = 0.;
	  si = 0.;
	  /* 
	   */
	  i__3 = *igh;
	  for (k = i__; k <= i__3; ++k)
	    {
	      sr =
		sr + ortr[k] * zr[k + j * zr_dim1] + orti[k] * zi[k +
								  j *
								  zi_dim1];
	      si =
		si + ortr[k] * zi[k + j * zi_dim1] - orti[k] * zr[k +
								  j *
								  zr_dim1];
	      /* L115: */
	    }
	  /* 
	   */
	  sr /= norm;
	  si /= norm;
	  /* 
	   */
	  i__3 = *igh;
	  for (k = i__; k <= i__3; ++k)
	    {
	      zr[k + j * zr_dim1] =
		zr[k + j * zr_dim1] + sr * ortr[k] - si * orti[k];
	      zi[k + j * zi_dim1] =
		zi[k + j * zi_dim1] + sr * orti[k] + si * ortr[k];
	      /* L120: */
	    }
	  /* 
	   */
	  /* L130: */
	}
      /* 
       */
    L140:
      ;
    }
  /****** 
   */
 L150:
  return 0;
}				/* cortr_ */
