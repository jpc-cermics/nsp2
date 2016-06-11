/* comqr3.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* 
 * 
 *    this subroutine is a translation of a unitary analogue of the 
 *    algol procedure  comlr2, num. math. 16, 181-204(1970) by peters 
 *    and wilkinson. 
 *    handbook for auto. comp., vol.ii-linear algebra, 372-395(1971). 
 *    the unitary analogue substitutes the qr algorithm of francis 
 *    (comp. jour. 4, 332-345(1962)) for the lr algorithm. 
 * 
 *    modified by  c. moler 
 *
 *    this subroutine finds the eigenvalues of a complex upper 
 *    hessenberg matrix by the qr method. The unitary transformation 
 *    can also be accumulated if  corth  has been used to reduce 
 *    this general matrix to hessenberg form. 
 * 
 *    MODIFICATION OF EISPACK COMQR+COMQR2 
 *       1. job parameter added 
 *       2. code concerning eigenvector computation deleted 
 *     subroutine comqr3(nm,n,low,igh,hr,hi,wr,wi,zr,zi,ierr,job) 
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
 *         reduction by  corth, if performed. 
 *       zr and zi contain the real and imaginary parts,respectively 
 *          of the unitary similarity used to put h on hessenberg form 
 *          or a unitary matrix ,if vectors are desired 
 *      job indicate the job to be performed: job=xy 
 *          if y=0 no accumulation of the unitary transformation 
 *          if y=1 transformation  accumulated in z 
 *    on output. 
 *    the upper hessenberg portions of hr and hi have been destroyed 
 *       wr and wi contain the real and imaginary parts, 
 *         respectively, of the eigenvalues.  if an error 
 *         exit is made, the eigenvalues should be correct 
 *         for indices ierr+1,...,n. 
 *       zr and zi contain the real and imaginary parts, 
 *         respectively, of the eigenvectors.  the eigenvectors 
 *         are unnormalized.  if an error exit is made, none of 
 *         the eigenvectors has been found. 
 *       ierr is set to 
 *         zero       for normal return, 
 *         j          if the j-th eigenvalue has not been 
 *                    determined after a total of 30*n iterations. 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 */


int
nsp_ctrlpack_comqr3 (int *nm, int *n, int *low, int *igh, double *hr,
		     double *hi, double *wr, double *wi, double *zr,
		     double *zi, int *ierr, int *job)
{
  /* System generated locals */
  int hr_dim1, hr_offset, hi_dim1, hi_offset, zr_dim1, zr_offset, zi_dim1,
    zi_offset, i__1, i__2;
  double d__1, d__2, d__3, d__4;

  /* Local variables */
  int iend;
  double norm;
  int i__, j, l=0;
  int en, ll;
  double si, ti, xi, yi;
  int jx;
  double sr, tr;
  int jy;
  double xr, yr;
  int ip1, lp1, itn, its;
  double zzi, zzr;
  int enm1;

  /* Parameter adjustments */
  zi_dim1 = *nm;
  zi_offset = zi_dim1 + 1;
  zi -= zi_offset;
  zr_dim1 = *nm;
  zr_offset = zr_dim1 + 1;
  zr -= zr_offset;
  --wi;
  --wr;
  hi_dim1 = *nm;
  hi_offset = hi_dim1 + 1;
  hi -= hi_offset;
  hr_dim1 = *nm;
  hr_offset = hr_dim1 + 1;
  hr -= hr_offset;

  /* Function Body */
  *ierr = 0;
  /****** 
   */
  jx = *job / 10;
  jy = *job - jx * 10;
  /* 
   *    .......... create real subdiagonal elements .......... 
   */
  iend = *igh - *low - 1;
  if (iend < 0)
    {
      goto L180;
    }
  /* L150: */
  l = *low + 1;
  /* 
   */
  i__1 = *igh;
  for (i__ = l; i__ <= i__1; ++i__)
    {
      /*Computing MIN 
       */
      i__2 = i__ + 1;
      ll = Min (i__2, *igh);
      if (hi[i__ + (i__ - 1) * hi_dim1] == 0.)
	{
	  goto L170;
	}
      norm =
	nsp_calpack_pythag (&hr[i__ + (i__ - 1) * hr_dim1],
			    &hi[i__ + (i__ - 1) * hi_dim1]);
      yr = hr[i__ + (i__ - 1) * hr_dim1] / norm;
      yi = hi[i__ + (i__ - 1) * hi_dim1] / norm;
      hr[i__ + (i__ - 1) * hr_dim1] = norm;
      hi[i__ + (i__ - 1) * hi_dim1] = 0.;
      /* 
       */
      i__2 = *n;
      for (j = i__; j <= i__2; ++j)
	{
	  si = yr * hi[i__ + j * hi_dim1] - yi * hr[i__ + j * hr_dim1];
	  hr[i__ + j * hr_dim1] =
	    yr * hr[i__ + j * hr_dim1] + yi * hi[i__ + j * hi_dim1];
	  hi[i__ + j * hi_dim1] = si;
	  /* L155: */
	}
      /* 
       */
      i__2 = ll;
      for (j = 1; j <= i__2; ++j)
	{
	  si = yr * hi[j + i__ * hi_dim1] + yi * hr[j + i__ * hr_dim1];
	  hr[j + i__ * hr_dim1] =
	    yr * hr[j + i__ * hr_dim1] - yi * hi[j + i__ * hi_dim1];
	  hi[j + i__ * hi_dim1] = si;
	  /* L160: */
	}
      /****** 
       */
      if (jy == 0)
	{
	  goto L170;
	}
      /****** 
       */
      i__2 = *igh;
      for (j = *low; j <= i__2; ++j)
	{
	  si = yr * zi[j + i__ * zi_dim1] + yi * zr[j + i__ * zr_dim1];
	  zr[j + i__ * zr_dim1] =
	    yr * zr[j + i__ * zr_dim1] - yi * zi[j + i__ * zi_dim1];
	  zi[j + i__ * zi_dim1] = si;
	  /* L165: */
	}
      /* 
       */
    L170:
      ;
    }
  /*    .......... store roots isolated by cbal .......... 
   * 
   */
 L180:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (i__ >= *low && i__ <= *igh)
	{
	  goto L200;
	}
      wr[i__] = hr[i__ + i__ * hr_dim1];
      wi[i__] = hi[i__ + i__ * hi_dim1];
    L200:
      ;
    }
  /* 
   */
  /* L210: */
  en = *igh;
  tr = 0.;
  ti = 0.;
  itn = *n * 30;
  /*    .......... search for next eigenvalue .......... 
   */
 L220:
  if (en < *low)
    {
      goto L1001;
    }
  its = 0;
  enm1 = en - 1;
  /*    .......... look for single small sub-diagonal element 
   *               for l=en step -1 until low do -- .......... 
   */
 L240:
  i__1 = en;
  for (ll = *low; ll <= i__1; ++ll)
    {
      l = en + *low - ll;
      if (l == *low)
	{
	  goto L300;
	}
      /****** 
       */
      xr = (d__1 = hr[l - 1 + (l - 1) * hr_dim1], Abs (d__1)) + (d__4 =
								 hi[l - 1 +
								    (l -
								     1) *
								    hi_dim1] +
								 (d__2 =
								  hr[l +
								     l *
								     hr_dim1],
								  Abs (d__2))
								 + (d__3 =
								    hi[l +
								       l *
								       hi_dim1],
								    Abs
								    (d__3)),
								 Abs (d__4));
      yr = xr + (d__1 = hr[l + (l - 1) * hr_dim1], Abs (d__1));
      if (xr == yr)
	{
	  goto L300;
	}
      /****** 
       */
      /* L260: */
    }
  /*    .......... form shift .......... 
   */
 L300:
  if (l == en)
    {
      goto L660;
    }
  if (itn == 0)
    {
      goto L1000;
    }
  if (its == 10 || its == 20)
    {
      goto L320;
    }
  sr = hr[en + en * hr_dim1];
  si = hi[en + en * hi_dim1];
  xr = hr[enm1 + en * hr_dim1] * hr[en + enm1 * hr_dim1];
  xi = hi[enm1 + en * hi_dim1] * hr[en + enm1 * hr_dim1];
  if (xr == 0. && xi == 0.)
    {
      goto L340;
    }
  yr = (hr[enm1 + enm1 * hr_dim1] - sr) / 2.;
  yi = (hi[enm1 + enm1 * hi_dim1] - si) / 2.;
  /*Computing 2nd power 
   */
  d__2 = yr;
  /*Computing 2nd power 
   */
  d__3 = yi;
  d__1 = d__2 * d__2 - d__3 * d__3 + xr;
  d__4 = yr * 2. * yi + xi;
  nsp_calpack_wsqrt (&d__1, &d__4, &zzr, &zzi);
  if (yr * zzr + yi * zzi >= 0.)
    {
      goto L310;
    }
  zzr = -zzr;
  zzi = -zzi;
 L310:
  d__1 = yr + zzr;
  d__2 = yi + zzi;
  nsp_ctrlpack_cdiv (&xr, &xi, &d__1, &d__2, &zzr, &zzi);
  sr -= zzr;
  si -= zzi;
  goto L340;
  /*    .......... form exceptional shift .......... 
   */
 L320:
  sr = (d__1 = hr[en + enm1 * hr_dim1], Abs (d__1)) + (d__2 =
						       hr[enm1 +
							  (en - 2) * hr_dim1],
						       Abs (d__2));
  si = 0.;
  /* 
   */
 L340:
  i__1 = en;
  for (i__ = *low; i__ <= i__1; ++i__)
    {
      hr[i__ + i__ * hr_dim1] -= sr;
      hi[i__ + i__ * hi_dim1] -= si;
      /* L360: */
    }
  /* 
   */
  tr += sr;
  ti += si;
  ++its;
  --itn;
  /*    .......... reduce to triangle (rows) .......... 
   */
  lp1 = l + 1;
  /* 
   */
  i__1 = en;
  for (i__ = lp1; i__ <= i__1; ++i__)
    {
      sr = hr[i__ + (i__ - 1) * hr_dim1];
      hr[i__ + (i__ - 1) * hr_dim1] = 0.;
      d__1 =
	nsp_calpack_pythag (&hr[i__ - 1 + (i__ - 1) * hr_dim1],
			    &hi[i__ - 1 + (i__ - 1) * hi_dim1]);
      norm = nsp_calpack_pythag (&d__1, &sr);
      xr = hr[i__ - 1 + (i__ - 1) * hr_dim1] / norm;
      wr[i__ - 1] = xr;
      xi = hi[i__ - 1 + (i__ - 1) * hi_dim1] / norm;
      wi[i__ - 1] = xi;
      hr[i__ - 1 + (i__ - 1) * hr_dim1] = norm;
      hi[i__ - 1 + (i__ - 1) * hi_dim1] = 0.;
      hi[i__ + (i__ - 1) * hi_dim1] = sr / norm;
      /* 
       */
      i__2 = *n;
      for (j = i__; j <= i__2; ++j)
	{
	  yr = hr[i__ - 1 + j * hr_dim1];
	  yi = hi[i__ - 1 + j * hi_dim1];
	  zzr = hr[i__ + j * hr_dim1];
	  zzi = hi[i__ + j * hi_dim1];
	  hr[i__ - 1 + j * hr_dim1] =
	    xr * yr + xi * yi + hi[i__ + (i__ - 1) * hi_dim1] * zzr;
	  hi[i__ - 1 + j * hi_dim1] =
	    xr * yi - xi * yr + hi[i__ + (i__ - 1) * hi_dim1] * zzi;
	  hr[i__ + j * hr_dim1] =
	    xr * zzr - xi * zzi - hi[i__ + (i__ - 1) * hi_dim1] * yr;
	  hi[i__ + j * hi_dim1] =
	    xr * zzi + xi * zzr - hi[i__ + (i__ - 1) * hi_dim1] * yi;
	  /* L490: */
	}
      /* 
       */
      /* L500: */
    }
  /* 
   */
  si = hi[en + en * hi_dim1];
  if (si == 0.)
    {
      goto L540;
    }
  norm = nsp_calpack_pythag (&hr[en + en * hr_dim1], &si);
  sr = hr[en + en * hr_dim1] / norm;
  si /= norm;
  hr[en + en * hr_dim1] = norm;
  hi[en + en * hi_dim1] = 0.;
  if (en == *n)
    {
      goto L540;
    }
  ip1 = en + 1;
  /* 
   */
  i__1 = *n;
  for (j = ip1; j <= i__1; ++j)
    {
      yr = hr[en + j * hr_dim1];
      yi = hi[en + j * hi_dim1];
      hr[en + j * hr_dim1] = sr * yr + si * yi;
      hi[en + j * hi_dim1] = sr * yi - si * yr;
      /* L520: */
    }
  /*    .......... inverse operation (columns) .......... 
   */
 L540:
  i__1 = en;
  for (j = lp1; j <= i__1; ++j)
    {
      xr = wr[j - 1];
      xi = wi[j - 1];
      /* 
       */
      i__2 = j;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  yr = hr[i__ + (j - 1) * hr_dim1];
	  yi = 0.;
	  zzr = hr[i__ + j * hr_dim1];
	  zzi = hi[i__ + j * hi_dim1];
	  if (i__ == j)
	    {
	      goto L560;
	    }
	  yi = hi[i__ + (j - 1) * hi_dim1];
	  hi[i__ + (j - 1) * hi_dim1] =
	    xr * yi + xi * yr + hi[j + (j - 1) * hi_dim1] * zzi;
	L560:
	  hr[i__ + (j - 1) * hr_dim1] =
	    xr * yr - xi * yi + hi[j + (j - 1) * hi_dim1] * zzr;
	  hr[i__ + j * hr_dim1] =
	    xr * zzr + xi * zzi - hi[j + (j - 1) * hi_dim1] * yr;
	  hi[i__ + j * hi_dim1] =
	    xr * zzi - xi * zzr - hi[j + (j - 1) * hi_dim1] * yi;
	  /* L580: */
	}
      /****** 
       */
      if (jy == 0)
	{
	  goto L600;
	}
      /****** 
       */
      i__2 = *igh;
      for (i__ = *low; i__ <= i__2; ++i__)
	{
	  yr = zr[i__ + (j - 1) * zr_dim1];
	  yi = zi[i__ + (j - 1) * zi_dim1];
	  zzr = zr[i__ + j * zr_dim1];
	  zzi = zi[i__ + j * zi_dim1];
	  zr[i__ + (j - 1) * zr_dim1] =
	    xr * yr - xi * yi + hi[j + (j - 1) * hi_dim1] * zzr;
	  zi[i__ + (j - 1) * zi_dim1] =
	    xr * yi + xi * yr + hi[j + (j - 1) * hi_dim1] * zzi;
	  zr[i__ + j * zr_dim1] =
	    xr * zzr + xi * zzi - hi[j + (j - 1) * hi_dim1] * yr;
	  zi[i__ + j * zi_dim1] =
	    xr * zzi - xi * zzr - hi[j + (j - 1) * hi_dim1] * yi;
	  /* L590: */
	}
      /* 
       */
    L600:
      ;
    }
  /* 
   */
  if (si == 0.)
    {
      goto L240;
    }
  /* 
   */
  i__1 = en;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      yr = hr[i__ + en * hr_dim1];
      yi = hi[i__ + en * hi_dim1];
      hr[i__ + en * hr_dim1] = sr * yr - si * yi;
      hi[i__ + en * hi_dim1] = sr * yi + si * yr;
      /* L630: */
    }
  /****** 
   */
  if (jy == 0)
    {
      goto L240;
    }
  /****** 
   */
  i__1 = *igh;
  for (i__ = *low; i__ <= i__1; ++i__)
    {
      yr = zr[i__ + en * zr_dim1];
      yi = zi[i__ + en * zi_dim1];
      zr[i__ + en * zr_dim1] = sr * yr - si * yi;
      zi[i__ + en * zi_dim1] = sr * yi + si * yr;
      /* L640: */
    }
  /* 
   */
  goto L240;
  /*    .......... a root found .......... 
   */
 L660:
  hr[en + en * hr_dim1] += tr;
  wr[en] = hr[en + en * hr_dim1];
  hi[en + en * hi_dim1] += ti;
  wi[en] = hi[en + en * hi_dim1];
  en = enm1;
  goto L220;
  /*    .......... all roots found.  .......... 
   *     go to 1001 
   * 
   *    .......... set error -- no convergence to an 
   *               eigenvalue after 30 iterations .......... 
   */
 L1000:
  *ierr = en;
 L1001:
  return 0;
}				/* comqr3_ */
