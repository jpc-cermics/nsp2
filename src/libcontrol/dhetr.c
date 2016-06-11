/* dhetr.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* 
 *    given a real general matrix a, shetr reduces a submatrix 
 *    of a in rows and columns low through igh to upper hessenberg 
 *    form by orthogonal similarity transformations.  these 
 *    orthogonal transformations are further accumulated into rows 
 *    low through igh of an n x m matrix b and columns low 
 *    through igh of an l x n matrix c by premultiplication and 
 *    postmultiplication, respectively. 
 *
 *       b        double precision(nb,m) 
 *                an n x m double precision matrix 
 *       c        double precision(nc,n) 
 *                an l x n double precision matrix. 
 *    on return: 
 *       a        an upper hessenberg matric similar to (via an 
 *                orthogonal matrix consisting of a sequence of 
 *                householder transformations) the original matrix a; 
 *                further information concerning the orthogonal 
 *                transformations used in the reduction is contained 
 *                in the elements below the first subdiagonal; see 
 *                orthes documentation for details. 
 *       b        the original b matrix premultiplied by the transpose 
 *                of the orthogonal transformation used to reduce a. 
 *       c        the original c matrix postmultiplied by the orthogonal 
 *                transformation used to reduce a. 
 *       ort      double precision(n) 
 *                a work vector containing information about the 
 *                orthogonal transformations; see orthes documentation 
 *                for details. 
 *    this version dated july 1980. 
 *    alan j. laub, university of southern california. 
 */

int
nsp_ctrlpack_dhetr (int *na, int *nb, int *nc, int *l, int *m, int *n,
		    int *low, int *igh, double *a, double *b, double *c__,
		    double *ort)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2, i__3;
  double d__1;

  /* Builtin functions */
  double sqrt (double), d_sign (double *, double *);

  /* Local variables */
  double f, g, h__;
  int i__, j, k;
  double scale;
  int la, ii, jj, kp1, kpn;

  /* Parameter adjustments */
  b_dim1 = *nb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  --ort;
  c_dim1 = *nc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  la = *igh - 1;
  kp1 = *low + 1;
  if (la < kp1)
    {
      goto L170;
    }
  i__1 = la;
  for (k = kp1; k <= i__1; ++k)
    {
      h__ = 0.;
      ort[k] = 0.;
      scale = 0.;
      /* 
       *       scale column 
       * 
       */
      i__2 = *igh;
      for (i__ = k; i__ <= i__2; ++i__)
	{
	  scale += (d__1 = a[i__ + (k - 1) * a_dim1], Abs (d__1));
	  /* L10: */
	}
      if (scale == 0.)
	{
	  goto L150;
	}
      kpn = k + *igh;
      i__2 = *igh;
      for (ii = k; ii <= i__2; ++ii)
	{
	  i__ = kpn - ii;
	  ort[i__] = a[i__ + (k - 1) * a_dim1] / scale;
	  h__ += ort[i__] * ort[i__];
	  /* L20: */
	}
      d__1 = sqrt (h__);
      g = -d_sign (&d__1, &ort[k]);
      h__ -= ort[k] * g;
      ort[k] -= g;
      /* 
       *       form  (i-(u*transpose(u))/h) *a 
       * 
       */
      i__2 = *n;
      for (j = k; j <= i__2; ++j)
	{
	  f = 0.;
	  i__3 = *igh;
	  for (ii = k; ii <= i__3; ++ii)
	    {
	      i__ = kpn - ii;
	      f += ort[i__] * a[i__ + j * a_dim1];
	      /* L30: */
	    }
	  f /= h__;
	  i__3 = *igh;
	  for (i__ = k; i__ <= i__3; ++i__)
	    {
	      a[i__ + j * a_dim1] -= f * ort[i__];
	      /* L40: */
	    }
	  /* L50: */
	}
      /* 
       *       form  (i-(u*transpose(u))/h) *b 
       * 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  f = 0.;
	  i__3 = *igh;
	  for (ii = k; ii <= i__3; ++ii)
	    {
	      i__ = kpn - ii;
	      f += ort[i__] * b[i__ + j * b_dim1];
	      /* L60: */
	    }
	  f /= h__;
	  i__3 = *igh;
	  for (i__ = k; i__ <= i__3; ++i__)
	    {
	      b[i__ + j * b_dim1] -= f * ort[i__];
	      /* L70: */
	    }
	  /* L80: */
	}
      /* 
       *       form  (i-(u*transpose(u))/h) *a*(i-(u*transpose(u))/h) 
       * 
       */
      i__2 = *igh;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  f = 0.;
	  i__3 = *igh;
	  for (jj = k; jj <= i__3; ++jj)
	    {
	      j = kpn - jj;
	      f += ort[j] * a[i__ + j * a_dim1];
	      /* L90: */
	    }
	  f /= h__;
	  i__3 = *igh;
	  for (j = k; j <= i__3; ++j)
	    {
	      a[i__ + j * a_dim1] -= f * ort[j];
	      /* L100: */
	    }
	  /* L110: */
	}
      /* 
       *       form  c*(i-(u*transpose(u))/h) 
       * 
       */
      i__2 = *l;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  f = 0.;
	  i__3 = *igh;
	  for (jj = k; jj <= i__3; ++jj)
	    {
	      j = kpn - jj;
	      f += ort[j] * c__[i__ + j * c_dim1];
	      /* L120: */
	    }
	  f /= h__;
	  i__3 = *igh;
	  for (j = k; j <= i__3; ++j)
	    {
	      c__[i__ + j * c_dim1] -= f * ort[j];
	      /* L130: */
	    }
	  /* L140: */
	}
      ort[k] = scale * ort[k];
      a[k + (k - 1) * a_dim1] = scale * g;
    L150:
      /* L160: */
      ;
    }
 L170:
  return 0;
}				/* dhetr_ */
