/* sydsr.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;

int
nsp_ctrlpack_sydsr (int *n, int *m, double *a, int *na, double *b, int *nb,
		    double *c__, int *nc, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, b_dim1, b_offset, i__1, i__2, i__3;

  /* Local variables */
  int ldim;
  int info, ipvt[4], nsys;
  int i__, j, k, l;
  double p[4], t[16] /* was [4][4] */ ;
  int l1, dk, dl, ii, ik, jj, kk, ll, km1, job;

  /*%But 
   * 
   *    this routine solves the discrete sylvester equation for the 
   *    case where the matrices  a and b  has been transformed to 
   *    quasi-triangular form. 
   * 
   * 
   * warning   -this routine is intended to be called only from 
   *            slice  routine  sybad . 
   * Copyright SLICOT 
   *% 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *nb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  c_dim1 = *nc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;

  /* Function Body */
  *ierr = 0;
  ldim = 4;
  job = 0;
  l = 1;
 L10:
  dl = 1;
  if (l == *m)
    {
      goto L20;
    }
  if (b[l + 1 + l * b_dim1] != 0.)
    {
      dl = 2;
    }
 L20:
  ll = l + dl - 1;
  k = 1;
 L30:
  km1 = k - 1;
  dk = 1;
  if (k == *n)
    {
      goto L40;
    }
  if (a[k + 1 + k * a_dim1] != 0.)
    {
      dk = 2;
    }
 L40:
  kk = k + dk - 1;
  l1 = l - 1;
  if (l1 == 0)
    {
      goto L70;
    }
  ii = 0;
  /* 
   */
  i__1 = kk;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      ++ii;
      jj = 0;
      /* 
       */
      i__2 = ll;
      for (j = l; j <= i__2; ++j)
	{
	  ++jj;
	  t[ii + (jj << 2) - 5] =
	    C2F (ddot) (&l1, &c__[i__ + c_dim1], nc,
			&b[j * b_dim1 + 1], &c__1);
	  /* L50: */
	}
    }
  /* 
   */
  i__2 = kk;
  for (i__ = k; i__ <= i__2; ++i__)
    {
      jj = 0;
      /* 
       */
      i__1 = ll;
      for (j = l; j <= i__1; ++j)
	{
	  ++jj;
	  i__3 = kk - k + 1;
	  c__[i__ + j * c_dim1] -=
	    C2F (ddot) (&i__3, &a[k + i__ * a_dim1], &c__1,
			&t[(jj << 2) - 4], &c__1);
	  /* L60: */
	}
    }
  /* 
   */
 L70:
  if (km1 == 0)
    {
      goto L100;
    }
  /* 
   */
  i__1 = ll;
  for (j = l; j <= i__1; ++j)
    {
      /* 
       */
      i__2 = km1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* 
	   */
	  i__3 = kk;
	  for (ik = k; ik <= i__3; ++ik)
	    {
	      c__[ik + j * c_dim1] -=
		a[i__ + ik * a_dim1] * C2F (ddot) (&ll,
						   &c__[i__ + c_dim1],
						   nc,
						   &b[j * b_dim1 + 1], &c__1);
	      /* L90: */
	    }
	}
    }
  /* 
   */
 L100:
  if (dl == 2)
    {
      goto L120;
    }
  if (dk == 2)
    {
      goto L110;
    }
  t[0] = a[k + k * a_dim1] * b[l + l * b_dim1] - 1.;
  if (t[0] == 0.)
    {
      goto L180;
    }
  c__[k + l * c_dim1] /= t[0];
  goto L150;
 L110:
  t[0] = a[k + k * a_dim1] * b[l + l * b_dim1] - 1.;
  t[4] = b[l + l * b_dim1] * a[kk + k * a_dim1];
  t[1] = b[l + l * b_dim1] * a[k + kk * a_dim1];
  t[5] = a[kk + kk * a_dim1] * b[l + l * b_dim1] - 1.;
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[kk + l * c_dim1];
  nsys = 2;
  nsp_ctrlpack_dgefa (t, &ldim, &nsys, ipvt, &info);
  if (info != 0)
    {
      goto L180;
    }
  nsp_ctrlpack_dgesl (t, &ldim, &nsys, ipvt, p, &job);
  c__[k + l * c_dim1] = p[0];
  c__[kk + l * c_dim1] = p[1];
  goto L150;
 L120:
  if (dk == 2)
    {
      goto L140;
    }
  t[0] = b[l + l * b_dim1] * a[k + k * a_dim1] - 1.;
  t[1] = b[l + ll * b_dim1] * a[k + k * a_dim1];
  t[4] = b[ll + l * b_dim1] * a[k + k * a_dim1];
  t[5] = b[ll + ll * b_dim1] * a[k + k * a_dim1] - 1.;
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[k + ll * c_dim1];
  nsys = 2;
  nsp_ctrlpack_dgefa (t, &ldim, &nsys, ipvt, &info);
  if (info != 0)
    {
      goto L180;
    }
  nsp_ctrlpack_dgesl (t, &ldim, &nsys, ipvt, p, &job);
  c__[k + l * c_dim1] = p[0];
  c__[k + ll * c_dim1] = p[1];
  goto L150;
 L140:
  t[0] = b[l + l * b_dim1] * a[k + k * a_dim1] - 1.;
  t[4] = b[l + l * b_dim1] * a[kk + k * a_dim1];
  t[8] = b[ll + l * b_dim1] * a[k + k * a_dim1];
  t[12] = b[ll + l * b_dim1] * a[kk + k * a_dim1];
  t[1] = b[l + l * b_dim1] * a[k + kk * a_dim1];
  t[5] = b[l + l * b_dim1] * a[kk + kk * a_dim1] - 1.;
  t[9] = b[ll + l * b_dim1] * a[k + kk * a_dim1];
  t[13] = b[ll + l * b_dim1] * a[kk + kk * a_dim1];
  t[2] = b[l + ll * b_dim1] * a[k + k * a_dim1];
  t[6] = b[l + ll * b_dim1] * a[kk + k * a_dim1];
  t[10] = b[ll + ll * b_dim1] * a[k + k * a_dim1] - 1.;
  t[14] = b[ll + ll * b_dim1] * a[kk + k * a_dim1];
  t[3] = b[l + ll * b_dim1] * a[k + kk * a_dim1];
  t[7] = b[l + ll * b_dim1] * a[kk + kk * a_dim1];
  t[11] = b[ll + ll * b_dim1] * a[k + kk * a_dim1];
  t[15] = b[ll + ll * b_dim1] * a[kk + kk * a_dim1] - 1.;
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[kk + l * c_dim1];
  p[2] = c__[k + ll * c_dim1];
  p[3] = c__[kk + ll * c_dim1];
  nsys = 4;
  nsp_ctrlpack_dgefa (t, &ldim, &nsys, ipvt, &info);
  if (info != 0)
    {
      goto L180;
    }
  nsp_ctrlpack_dgesl (t, &ldim, &nsys, ipvt, p, &job);
  c__[k + l * c_dim1] = p[0];
  c__[kk + l * c_dim1] = p[1];
  c__[k + ll * c_dim1] = p[2];
  c__[kk + ll * c_dim1] = p[3];
  /* 
   */
 L150:
  k += dk;
  if (k <= *n)
    {
      goto L30;
    }
  l += dl;
  if (l <= *m)
    {
      goto L10;
    }
  goto L190;
 L180:
  *ierr = 1;
 L190:
  return 0;
}				/* sydsr_ */
