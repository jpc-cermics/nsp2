/* dbeskg.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;

int
nsp_calpack_dbeskg (double *x1, double *alpha, int *kode, int *n, double *y,
		    int *nz, int *ierr)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  double temp;
  int i__;
  double x;
  double a1;
  int nn;
  double inf;
  int ier;

  /*    Author Serge Steer, Copyright INRIA, 2005 
   *    extends dbesk for the case where alpha is negative 
   *    x is supposed to be positive (besselk,with x<0 is complex) 
   * 
   */
  /* Parameter adjustments */
  --y;

  /* Function Body */
  inf = C2F (dlamch) ("o", 1L) * 2.;
  x = *x1;
  *ierr = 0;
  if (x != x || *alpha != *alpha)
    {
      /*    .  NaN case 
       */
      d__1 = inf - inf;
      nsp_calpack_dset (n, &d__1, &y[1], &c__1);
      *ierr = 4;
    }
  else if (x == 0.)
    {
      d__1 = -inf;
      nsp_calpack_dset (n, &d__1, &y[1], &c__1);
      *ierr = 2;
    }
  else if (*alpha >= 0.)
    {
      nsp_calpack_dbesk (&x, alpha, kode, n, &y[1], nz, ierr);
      if (*ierr == 2)
	{
	  nsp_calpack_dset (n, &inf, &y[1], &c__1);
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
      nsp_calpack_dbesk (&x, &a1, kode, &nn, &y[1], nz, ierr);
      if (*ierr == 2)
	{
	  nsp_calpack_dset (&nn, &inf, &y[1], &c__1);
	}
      /*    .  swap the result to have it in correct order 
       */
      if (nn >= 2)
	{
	  i__1 = nn / 2;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      temp = y[i__];
	      y[i__] = y[nn + 1 - i__];
	      y[nn + 1 - i__] = temp;
	    }
	}
      if (*n > nn)
	{
	  d__1 = 1. - a1;
	  i__1 = *n - nn;
	  nsp_calpack_dbesk (&x, &d__1, kode, &i__1, &y[nn + 1], nz, &ier);
	  if (ier == 2)
	    {
	      i__1 = *n - nn;
	      nsp_calpack_dset (&i__1, &inf, &y[nn + 1], &c__1);
	    }
	  *ierr = Max (*ierr, ier);
	}
    }
  return 0;
}				/* dbeskg_ */

int
nsp_calpack_dbeskv (double *x, int *nx, double *alpha, int *na, int *kode,
		    double *y, double *w, int *ierr)
{
  /* System generated locals */
  int i__1;
  double d__1, d__2;

  /* Local variables */
  int i__, j, n;
  int j0;
  int nz, ier;
  double eps;

  /*    Author Serge Steer, Copyright INRIA, 2005 
   *    compute besseli function for x and alpha given by vectors 
   *    w : working array of size 2*na (used only if nz>0 and alpha contains negative 
   *        values 
   */
  /* Parameter adjustments */
  --x;
  --alpha;
  --y;
  --w;

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
	  d__2 = (d__1 = x[i__], Abs (d__1));
	  nsp_calpack_dbeskg (&d__2, &alpha[i__], kode, &c__1, &y[i__], &nz,
			      &ier);
	  *ierr = Max (*ierr, ier);
	}
    }
  else if (*na == 1)
    {
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  d__2 = (d__1 = x[i__], Abs (d__1));
	  nsp_calpack_dbeskg (&d__2, &alpha[1], kode, &c__1, &y[i__], &nz,
			      &ier);
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
      if (j <= *na && (d__1 = alpha[j - 1] + 1 - alpha[j], Abs (d__1)) <= eps)
	{
	  goto L10;
	}
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  d__2 = (d__1 = x[i__], Abs (d__1));
	  nsp_calpack_dbeskg (&d__2, &alpha[j0], kode, &n, &w[1], &nz, &ier);
	  *ierr = Max (*ierr, ier);
	  C2F (dcopy) (&n, &w[1], &c__1, &y[i__ + (j0 - 1) * *nx], nx);
	}
      j0 = j;
      if (j0 <= *na)
	{
	  goto L5;
	}
    }
  return 0;
}				/* dbeskv_ */
#endif
