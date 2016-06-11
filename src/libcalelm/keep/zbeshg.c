/* zbeshg.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c_n1 = -1;
static double c_b21 = -1.;
static int c__2 = 2;

int
nsp_calpack_zbeshg (double *x1r, double *x1i, double *alpha, int *kode,
		    int *k, int *n, double *yr, double *yi, int *nz,
		    double *wr, double *wi, int *ierr)
{
  /* Initialized data */

  static double pi = 3.14159265358979324;

  /* System generated locals */
  int i__1, i__2;
  double d__1;

  /* Local variables */
  double a, b;
  double a1;
  int i0;
  int nn;
  double xi, xr;
  int nz1, nz2;
  double inf;
  int ier, ier2;

  /*    Author Serge Steer, Copyright INRIA, 2005 
   *    extends cbesi for the case where alpha is negative 
   * 
   */
  /* Parameter adjustments */
  --wi;
  --wr;
  --yi;
  --yr;

  /* Function Body */
  inf = C2F (dlamch) ("o", 1L) * 2.;
  xr = *x1r;
  xi = *x1i;
  ier2 = 0;
  if (xr != xr || xi != xi || *alpha != *alpha)
    {
      /*    .  NaN case 
       */
      d__1 = inf - inf;
      nsp_calpack_dset (n, &d__1, &yr[1], &c__1);
      d__1 = inf - inf;
      nsp_calpack_dset (n, &d__1, &yi[1], &c__1);
      *ierr = 4;
    }
  else if (*alpha >= 0.)
    {
      nsp_calpack_zbesh (&xr, &xi, alpha, kode, k, n, &yr[1], &yi[1], nz,
			 ierr);
      if (*ierr == 1 || *ierr == 2 || *ierr >= 4)
	{
	  d__1 = inf - inf;
	  nsp_calpack_dset (n, &d__1, &yr[1], &c__1);
	  d__1 = inf - inf;
	  nsp_calpack_dset (n, &d__1, &yi[1], &c__1);
	}
    }
  else if (*alpha == d_int (alpha))
    {
      /*    .  alpha <0 and int, 
       *    .  transform to positive value of alpha 
       */
      if (*alpha - 1 + *n >= 0.)
	{
	  /*    .     0 is between alpha and alpha+n 
	   */
	  a1 = 0.;
	  /*Computing MIN 
	   */
	  i__1 = *n, i__2 = (int) (-(*alpha));
	  nn = Min (i__1, i__2);
	}
      else
	{
	  a1 = -(*alpha - 1 + *n);
	  nn = *n;
	}
      nsp_calpack_zbesh (&xr, &xi, &a1, kode, k, n, &wr[1], &wi[1], nz, ierr);
      if (*ierr == 1 || *ierr == 2 || *ierr >= 4)
	{
	  d__1 = inf - inf;
	  nsp_calpack_dset (n, &d__1, &yr[1], &c__1);
	  d__1 = inf - inf;
	  nsp_calpack_dset (n, &d__1, &yi[1], &c__1);
	}
      else
	{
	  if (*n > nn)
	    {
	      /*    .        0 is between alpha and alpha+n 
	       */
	      i__1 = *n - nn;
	      C2F (dcopy) (&i__1, &wr[1], &c__1, &yr[nn + 1], &c__1);
	      i__1 = *n - nn;
	      C2F (dcopy) (&i__1, &wi[1], &c__1, &yi[nn + 1], &c__1);
	      C2F (dcopy) (&nn, &wr[2], &c_n1, &yr[1], &c__1);
	      C2F (dcopy) (&nn, &wi[2], &c_n1, &yi[1], &c__1);
	    }
	  else
	    {
	      /*    .        alpha and alpha+n are negative 
	       */
	      C2F (dcopy) (n, &wr[1], &c_n1, &yr[1], &c__1);
	      C2F (dcopy) (n, &wi[1], &c_n1, &yi[1], &c__1);
	    }
	}
      i0 = ((int) Abs (*alpha) + 1) % 2;
      i__1 = (nn - i0 + 1) / 2;
      C2F (dscal) (&i__1, &c_b21, &yr[i0 + 1], &c__2);
      i__1 = (nn - i0 + 1) / 2;
      C2F (dscal) (&i__1, &c_b21, &yi[i0 + 1], &c__2);
    }
  else
    {
      /*    .  first alpha is negative non int, 
       *    .  transform to positive value of alpha 
       */
      if (*alpha - 1. + *n >= 0.)
	{
	  /*    .     0 is between alpha and alpha+n 
	   */
	  nn = (int) (-(*alpha)) + 1;
	}
      else
	{
	  nn = *n;
	}
      /*    .  compute for negative value of alpha+k, transform problem for 
       *    .  a1:a1+(nn-1) with a1 positive  a1+k =abs(alpha+nn-k) 
       */
      a1 = -(*alpha - 1. + nn);
      nsp_calpack_zbesh (&xr, &xi, &a1, kode, k, n, &wr[1], &wi[1], &nz1,
			 ierr);
      *nz = Max (nz1, nz2);
      if (*ierr == 0)
	{
	  a = cos (a1 * pi);
	  b = sin (a1 * pi);
	  if (*k == 1)
	    {
	      nsp_calpack_wscal (&nn, &a, &b, &wr[1], &wi[1], &c__1);
	    }
	  else
	    {
	      d__1 = -b;
	      nsp_calpack_wscal (&nn, &a, &d__1, &wr[1], &wi[1], &c__1);
	    }
	  /*    .     change sign to take into account that sin((a1+k)*pi) and cos((a1+k)*pi) 
	   *    .     changes sign with k 
	   */
	  if (nn >= 2)
	    {
	      i__1 = nn / 2;
	      C2F (dscal) (&i__1, &c_b21, &wr[2], &c__2);
	      i__1 = nn / 2;
	      C2F (dscal) (&i__1, &c_b21, &wi[2], &c__2);
	    }
	}
      else if (*ierr == 1 || *ierr == 2 || *ierr >= 4)
	{
	  d__1 = inf - inf;
	  nsp_calpack_dset (&nn, &d__1, &wr[1], &c__1);
	  d__1 = inf - inf;
	  nsp_calpack_dset (&nn, &d__1, &wi[1], &c__1);
	}
      /*    .  store the result in the correct order 
       */
      C2F (dcopy) (&nn, &wr[1], &c_n1, &yr[1], &c__1);
      C2F (dcopy) (&nn, &wi[1], &c_n1, &yi[1], &c__1);
      /*    .  compute for positive value of alpha+k is any 
       */
      if (*n > nn)
	{
	  a1 = 1. - a1;
	  i__1 = *n - nn;
	  nsp_calpack_zbesh (&xr, &xi, &a1, kode, k, &i__1, &yr[nn + 1],
			     &yi[nn + 1], nz, &ier);
	  if (ier == 1 || ier == 2 || ier >= 4)
	    {
	      i__1 = *n - nn;
	      d__1 = inf - inf;
	      nsp_calpack_dset (&i__1, &d__1, &yr[nn + 1], &c__1);
	      i__1 = *n - nn;
	      d__1 = inf - inf;
	      nsp_calpack_dset (&i__1, &d__1, &yi[nn + 1], &c__1);
	    }
	  *ierr = Max (*ierr, ier);
	}
    }
  return 0;
}				/* zbeshg_ */

int
nsp_calpack_zbeshv (double *xr, double *xi, int *nx, double *alpha, int *na,
		    int *kode, int *k, double *yr, double *yi, double *wr,
		    double *wi, int *ierr)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  int i__, j, n;
  int j0;
  int nz;
  int ier;
  double eps;

  /*    Author Serge Steer, Copyright INRIA, 2005 
   *    compute besseli function for x and alpha given by vectors 
   *    w : working array of size 2*na (used only if nz>0 and alpha 
   *    contains negative 
   *    values 
   */
  /* Parameter adjustments */
  --xi;
  --xr;
  --alpha;
  --yr;
  --yi;
  --wr;
  --wi;

  /* Function Body */
  *ierr = 0;
  eps = nsp_dlamch ("p");
  if (*na < 0)
    {
      /*    .  element wise case x and alpha are supposed to have the same 
       *    size 
       */
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_zbeshg (&xr[i__], &xi[i__], &alpha[i__], kode, k,
			      &c__1, &yr[i__], &yi[i__], &nz, &wr[1], &wi[1],
			      &ier);
	  *ierr = Max (*ierr, ier);
	}
    }
  else if (*na == 1)
    {
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_zbeshg (&xr[i__], &xi[i__], &alpha[1], kode, k, &c__1,
			      &yr[i__], &yi[i__], &nz, &wr[1], &wi[1], &ier);
	  *ierr = Max (*ierr, ier);
	}
    }
  else
    {
      /*    .  compute besselh(x(i),y(j)), i=1,nx,j=1,na 
       */
      j0 = 1;
    L5:
      n = 0;
    L10:
      ++n;
      j = j0 + n;
      if (j <= *na && (d__1 = alpha[j - 1] + 1 - alpha[j], Abs (d__1)) <= eps)
	{
	  goto L10;
	}
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_zbeshg (&xr[i__], &xi[i__], &alpha[j0], kode, k, &n,
			      &wr[1], &wi[1], &nz, &wr[*na + 1],
			      &wi[*na + 1], &ier);
	  *ierr = Max (*ierr, ier);
	  C2F (dcopy) (&n, &wr[1], &c__1, &yr[i__ + (j0 - 1) * *nx], nx);
	  C2F (dcopy) (&n, &wi[1], &c__1, &yi[i__ + (j0 - 1) * *nx], nx);
	}
      j0 = j;
      if (j0 <= *na)
	{
	  goto L5;
	}
    }
  return 0;
}				/* zbeshv_ */
#endif
