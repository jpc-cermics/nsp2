/* zbeskg.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;

int
nsp_calpack_zbeskg (double *x1r, double *x1i, double *alpha, int *kode,
		    int *n, double *yr, double *yi, int *nz, int *ierr)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  double temp;
  int i__;
  double a1;
  int nn;
  double xi, xr, inf;
  int ier;

  /*    Author Serge Steer, Copyright INRIA, 2005 
   *    extends cbesk for the case where alpha is negative 
   * 
   */
  /* Parameter adjustments */
  --yi;
  --yr;

  /* Function Body */
  inf = C2F (dlamch) ("o", 1L) * 2.;
  xr = *x1r;
  xi = *x1i;
  *ierr = 0;
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
      d__1 = Abs (*alpha);
      nsp_calpack_zbesk (&xr, &xi, &d__1, kode, n, &yr[1], &yi[1], nz, ierr);
      if (*ierr == 2)
	{
	  nsp_calpack_dset (n, &inf, &yr[1], &c__1);
	  nsp_calpack_dset (n, &inf, &yi[1], &c__1);
	}
      else if (*ierr >= 4)
	{
	  d__1 = inf - inf;
	  nsp_calpack_dset (n, &d__1, &yr[1], &c__1);
	  d__1 = inf - inf;
	  nsp_calpack_dset (n, &d__1, &yi[1], &c__1);
	}
    }
  else
    {
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
      a1 = -(*alpha - 1. + nn);
      nsp_calpack_zbesk (&xr, &xi, &a1, kode, &nn, &yr[1], &yi[1], nz, ierr);
      if (*ierr == 0)
	{
	  /*    .      swap the result to have it in correct order 
	   */
	  if (nn >= 2)
	    {
	      i__1 = nn / 2;
	      for (i__ = 1; i__ <= i__1; ++i__)
		{
		  temp = yr[i__];
		  yr[i__] = yr[nn + 1 - i__];
		  yr[nn + 1 - i__] = temp;
		  temp = yi[i__];
		  yi[i__] = yi[nn + 1 - i__];
		  yi[nn + 1 - i__] = temp;
		}
	    }
	}
      else if (*ierr == 2)
	{
	  nsp_calpack_dset (n, &inf, &yr[1], &c__1);
	  nsp_calpack_dset (n, &inf, &yi[1], &c__1);
	}
      else if (*ierr >= 4)
	{
	  d__1 = inf - inf;
	  nsp_calpack_dset (n, &d__1, &yr[1], &c__1);
	  d__1 = inf - inf;
	  nsp_calpack_dset (n, &d__1, &yi[1], &c__1);
	}
      if (*n > nn)
	{
	  a1 = 1. - a1;
	  i__1 = *n - nn;
	  nsp_calpack_zbesk (&xr, &xi, &a1, kode, &i__1, &yr[nn + 1],
			     &yi[nn + 1], nz, &ier);
	  if (ier == 2)
	    {
	      i__1 = *n - nn;
	      nsp_calpack_dset (&i__1, &inf, &yr[nn + 1], &c__1);
	      i__1 = *n - nn;
	      nsp_calpack_dset (&i__1, &inf, &yi[nn + 1], &c__1);
	    }
	  else if (*ierr >= 4)
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
}				/* zbeskg_ */

int
nsp_calpack_zbeskv (double *xr, double *xi, int *nx, double *alpha, int *na,
		    int *kode, double *yr, double *yi, double *wr,
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
   *    w : working array of size 2*na (used only if nz>0 and alpha contains negative 
   *        values 
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
  eps = nsp_dlamch ("p");
  *ierr = 0;
  if (*na < 0)
    {
      /*    .  element wise case x and alpha are supposed to have the same size 
       */
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_zbeskg (&xr[i__], &xi[i__], &alpha[i__], kode, &c__1,
			      &yr[i__], &yi[i__], &nz, &ier);
	  *ierr = Max (*ierr, ier);
	}
    }
  else if (*na == 1)
    {
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_zbeskg (&xr[i__], &xi[i__], &alpha[1], kode, &c__1,
			      &yr[i__], &yi[i__], &nz, &ier);
	  *ierr = Max (*ierr, ier);
	}
    }
  else
    {
      /*    .  compute besseli(x(i),y(j)), i=1,nx,j=1,na 
       */
      j0 = 1;
    L5:
      n = 0;
    L10:
      ++n;
      j = j0 + n;
      if (j <= *na
	  && (d__1 = alpha[j - 1] + 1. - alpha[j], Abs (d__1)) <= eps)
	{
	  goto L10;
	}
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_zbeskg (&xr[i__], &xi[i__], &alpha[j0], kode, &n,
			      &wr[1], &wi[1], &nz, &ier);
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
}				/* zbeskv_ */
#endif
