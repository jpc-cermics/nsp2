/* dbesjg.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;
static double c_b5 = -1.;
static int c__2 = 2;
static int c_n1 = -1;
static double c_b19 = 0.;

int
nsp_calpack_dbesjg (double *x1, double *alpha, int *n, double *y, int *nz,
		    double *w, int *ierr)
{
  /* Initialized data */

  static double pi = 3.14159265358979324;

  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  double a, b;
  double x;
  double a1;
  int i0;
  int nn, nz1;
  double inf;
  int ier;
  double eps;
  int ier2;

  /*    Author Serge Steer, Copyright INRIA, 2005 
   *    extends dbesj for the case where alpha is negative 
   *    if alpha is negative and non int and x1 is negative:error 
   * 
   */
  /* Parameter adjustments */
  --w;
  --y;

  /* Function Body */
  inf = C2F (dlamch) ("o", 1L) * 2.;
  x = *x1;
  *ierr = 0;
  ier2 = 0;
  if (x != x || *alpha != *alpha)
    {
      /*    .  NaN case 
       */
      d__1 = inf - inf;
      nsp_calpack_dset (n, &d__1, &y[1], &c__1);
      *ierr = 4;
    }
  else if (*alpha >= 0.)
    {
      /*    .  alpha <0 and x>=0 or x<0 
       */
      d__1 = Abs (x);
      nsp_calpack_dbesj (&d__1, alpha, n, &y[1], nz, ierr);
      if (*ierr == 2)
	{
	  nsp_calpack_dset (n, &inf, &y[1], &c__1);
	}
      if (x < 0.)
	{
	  i0 = ((int) (*alpha) + 1) % 2;
	  i__1 = (*n - i0 + 1) / 2;
	  C2F (dscal) (&i__1, &c_b5, &y[i0 + 1], &c__2);
	}
    }
  else if (*alpha == d_int (alpha))
    {
      /*    .  alpha <0 and int,   CHECKED 
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
      d__1 = Abs (x);
      nsp_calpack_dbesj (&d__1, &a1, n, &w[1], nz, ierr);
      if (*ierr == 2)
	{
	  nsp_calpack_dset (n, &inf, &y[1], &c__1);
	}
      else
	{
	  if (*n > nn)
	    {
	      /*    .        0 is between alpha and alpha+n 
	       */
	      i__1 = *n - nn;
	      C2F (dcopy) (&i__1, &w[1], &c__1, &y[nn + 1], &c__1);
	      C2F (dcopy) (&nn, &w[2], &c_n1, &y[1], &c__1);
	    }
	  else
	    {
	      /*    .        alpha and alpha+n are negative 
	       */
	      C2F (dcopy) (&nn, &w[1], &c_n1, &y[1], &c__1);
	    }
	}
      /*    .  apply parity 
       */
      i0 = ((int) Abs (*alpha) + 1) % 2;
      if (x > 0.)
	{
	  i__1 = (nn - i0 + 1) / 2;
	  C2F (dscal) (&i__1, &c_b5, &y[i0 + 1], &c__2);
	}
      else
	{
	  i__1 = (*n - nn) / 2;
	  C2F (dscal) (&i__1, &c_b5, &y[nn + 2], &c__2);
	}
    }
  else if (x == 0.)
    {
      /*    .  alpha <0 and x==0 CHECKED 
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
      *ierr = 2;
      d__1 = -inf;
      nsp_calpack_dset (&nn, &d__1, &y[1], &c__1);
      if (*n > nn)
	{
	  i__1 = *n - nn;
	  nsp_calpack_dset (&i__1, &c_b19, &y[nn + 1], &c__1);
	}
    }
  else
    {
      /*    .  first alpha is negative non int, x should be positive (with 
       *    .  x negative the result is complex. CHECKED 
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
      nsp_calpack_dbesj (&x, &a1, &nn, &y[1], &nz1, ierr);
      nsp_calpack_dbesy (&x, &a1, &nn, &w[1], &ier);
      *ierr = Max (*ierr, ier);
      if (*ierr == 0)
	{
	  a = cos (a1 * pi);
	  b = -sin (a1 * pi);
	  /*    .     to avoid numerical errors if a is near 0 (or b near 0) 
	   *    .     and y is big (or w is big) 
	   */
	  if ((d__1 = Abs (a) - 1., Abs (d__1)) < eps)
	    {
	      b = 0.;
	    }
	  if ((d__1 = Abs (b) - 1., Abs (d__1)) < eps)
	    {
	      a = 0.;
	    }
	  C2F (dscal) (&nn, &b, &w[1], &c__1);
	  C2F (daxpy) (&nn, &a, &y[1], &c__1, &w[1], &c__1);
	}
      else if (*ierr == 2)
	{
	  nsp_calpack_dset (&nn, &inf, &w[1], &c__1);
	}
      else if (*ierr == 4)
	{
	  d__1 = inf - inf;
	  nsp_calpack_dset (&nn, &d__1, &w[1], &c__1);
	}
      /*    .  change sign to take into account that cos((a1+k)*pi) and 
       *    .  sin((a1+k)*pi) changes sign with k 
       */
      if (nn >= 2)
	{
	  i__1 = nn / 2;
	  C2F (dscal) (&i__1, &c_b5, &w[2], &c__2);
	}
      /*    .  store the result in the correct order 
       */
      C2F (dcopy) (&nn, &w[1], &c_n1, &y[1], &c__1);
      /*    .  compute for positive value of alpha+k is any (note that x>0) 
       */
      if (*n > nn)
	{
	  /*    .     this code is taken from the alpha>0 case above 
	   */
	  d__1 = Abs (x);
	  d__2 = 1. - a1;
	  i__1 = *n - nn;
	  nsp_calpack_dbesj (&d__1, &d__2, &i__1, &y[nn + 1], &nz1, &ier);
	  if (ier == 2)
	    {
	      i__1 = *n - nn;
	      nsp_calpack_dset (&i__1, &inf, &y[nn + 1], &c__1);
	    }
	  *ierr = Max (*ierr, ier);
	}
    }
  return 0;
}				/* dbesjg_ */

int
nsp_calpack_dbesjv (double *x, int *nx, double *alpha, int *na, int *kode,
		    double *y, double *w, int *ierr)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  int i__, j, n;
  int j0;
  double w1;
  int nz, ier;
  double eps;

  /*    Author Serge Steer, Copyright INRIA, 2005 
   *    compute besselj function for x and alpha given by vectors 
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
	  nsp_calpack_dbesjg (&x[i__], &alpha[i__], &c__1, &y[i__], &nz, &w1,
			      &ier);
	  *ierr = Max (*ierr, ier);
	}
    }
  else if (*na == 1)
    {
      /*    .  element wise case x and alpha are supposed to have the same size 
       */
      i__1 = *nx;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  nsp_calpack_dbesjg (&x[i__], &alpha[1], &c__1, &y[i__], &nz, &w1,
			      &ier);
	  *ierr = Max (*ierr, ier);
	}
    }
  else
    {
      /*    .  compute besselj(x(i),y(j)), i=1,nx,j=1,na 
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
	  nsp_calpack_dbesjg (&x[i__], &alpha[j0], &n, &w[1], &nz,
			      &w[*na + 1], &ier);
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
}				/* dbesjv_ */
#endif
