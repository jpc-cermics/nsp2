/* lybsc.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* 
 *       subroutine lybsc(n,a,na,c,x,u,wrk,mode,ierr) 
 * 
 *       int n,na,mode,ierr 
 *       double precision a(na,n),c(na,n),x(na,n),u(na,n),wrk(n) 
 * 
 *arguments in 
 *      n        int 
 *               -the dimension of a 
 *      a        double precision(n,n) 
 *               -coefficient matrix of the equation 
 *               *** n.b. in this routine  a  is overwritten with 
 *               its transformed upper triangular form (see comments) 
 *      c        double precision(n,n) 
 *               -coefficient matrix of the equation 
 *      na       int 
 *               -the declared first dimension of a,c,x and u 
 *      mode     int 
 *               - mode = 0  if  a  has not already been reduced to 
 *                               upper triangular form 
 *               - mode = 1  if  a  has been reduced to triangular form 
 *                               by (e.g) a previous call to lybsc 
 * 
 *arguments out 
 *      a        double precision(n,n) 
 *               -on exit, a  contains the transformed upper triangular 
 *               form of a 
 *      x        double precision(n,n) 
 *               -the solution matrix 
 *      u        double precision(n,n) 
 *               - u  contains the orthogonal matrix which was used 
 *               to reduce  a  to upper triangular form 
 *      ierr     int 
 *               -error indicator 
 *               ierr = 0     successful return 
 *               ierr = 1     a  has a degenerate pair of eigenvalues 
 *               ierr = 2     a  cannot be reduced to triangular form 
 *       wrk     double precision(n) 
 * 
 *       to solve the double precision matrix equation 
 * 
 *              trans(a)*x + x*a = c 
 * 
 *       where  c  is symmetric, and  trans(a)  denotes 
 *       the transpose of  a . 
 * 
 *       this routine is a modification of the subroutine  atxxac, 
 *       written and discussed by  r.h.bartels & g.w.stewart. 
 * 
 *        r.h. bartels & g.w. stewart 
 *           "solution of the matrix equation  a'x + xb = c  ", 
 *           commun. a.c.m., vol 15, 1972, pp. 820-826 . 
 * 
 *               control systems research group, dept eecs, kingston 
 *               polytechnic, penrhyn rd.,kingston-upon-thames, england. 
 * 
 */

static int c__1 = 1;
static int c__11 = 11;

int
nsp_ctrlpack_lybsc (int *n, double *a, int *na, double *c__, double *x,
		    double *u, double *eps, double *wrk, int *mode, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, x_dim1, x_offset, u_dim1, u_offset,
    i__1, i__2, i__3;

  /* Local variables */
  int i__, j, k;
  double dprec;
  double tt[1];

  /* Parameter adjustments */
  --wrk;
  u_dim1 = *na;
  u_offset = u_dim1 + 1;
  u -= u_offset;
  x_dim1 = *na;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  c_dim1 = *na;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  if (*mode == 1)
    {
      goto L10;
    }
  nsp_ctrlpack_orthes (na, n, &c__1, n, &a[a_offset], &wrk[1]);
  nsp_ctrlpack_ortran (na, n, &c__1, n, &a[a_offset], &wrk[1], &u[u_offset]);
  nsp_ctrlpack_hqror2 (na, n, &c__1, n, &a[a_offset], tt, tt, &u[u_offset],
		       ierr, &c__11);
  if (*ierr != 0)
    {
      goto L140;
    }
 L10:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  x[i__ + j * x_dim1] = c__[i__ + j * c_dim1];
	  /* L15: */
	}
      x[i__ + i__ * x_dim1] *= .5;
      /* L20: */
    }
  /* 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  dprec = 0.;
	  i__3 = *n;
	  for (k = i__; k <= i__3; ++k)
	    {
	      dprec += x[i__ + k * x_dim1] * u[k + j * u_dim1];
	      /* L25: */
	    }
	  wrk[j] = dprec;
	  /* L30: */
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  x[i__ + j * x_dim1] = wrk[j];
	  /* L40: */
	}
    }
  i__2 = *n;
  for (j = 1; j <= i__2; ++j)
    {
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  dprec = 0.;
	  i__3 = *n;
	  for (k = 1; k <= i__3; ++k)
	    {
	      dprec += u[k + i__ * u_dim1] * x[k + j * x_dim1];
	      /* L45: */
	    }
	  wrk[i__] = dprec;
	  /* L50: */
	}
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  x[i__ + j * x_dim1] = wrk[i__];
	  /* L60: */
	}
    }
  /* 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *n;
      for (j = i__; j <= i__2; ++j)
	{
	  x[i__ + j * x_dim1] += x[j + i__ * x_dim1];
	  x[j + i__ * x_dim1] = x[i__ + j * x_dim1];
	  /* L70: */
	}
    }
  /* 
   *    call shrslv (c,a,x,n,n,na,na,na,0.0d+0,1.0d+20,fail) 
   */
  nsp_ctrlpack_lycsr (n, &a[a_offset], na, &x[x_offset], ierr);
  if (*ierr != 0)
    {
      return 0;
    }
  /* 
   */
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      x[i__ + i__ * x_dim1] *= .5;
      /* L80: */
    }
  /* 
   */
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  dprec = 0.;
	  i__3 = *n;
	  for (k = i__; k <= i__3; ++k)
	    {
	      dprec += x[i__ + k * x_dim1] * u[j + k * u_dim1];
	      /* L85: */
	    }
	  wrk[j] = dprec;
	  /* L90: */
	}
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  x[i__ + j * x_dim1] = wrk[j];
	  /* L100: */
	}
    }
  /* 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  dprec = 0.;
	  i__3 = *n;
	  for (k = 1; k <= i__3; ++k)
	    {
	      dprec += u[i__ + k * u_dim1] * x[k + j * x_dim1];
	      /* L105: */
	    }
	  wrk[i__] = dprec;
	  /* L110: */
	}
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  x[i__ + j * x_dim1] = wrk[i__];
	  /* L120: */
	}
    }
  /* 
   */
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      i__1 = *n;
      for (j = i__; j <= i__1; ++j)
	{
	  x[i__ + j * x_dim1] += x[j + i__ * x_dim1];
	  x[j + i__ * x_dim1] = x[i__ + j * x_dim1];
	  /* L130: */
	}
    }
  /* 
   */
  goto L150;
 L140:
  *ierr = 2;
 L150:
  return 0;
}				/* lybsc_ */

int nsp_ctrlpack_lycsr (int *n, double *a, int *na, double *c__, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, c_dim1, c_offset, i__1, i__2, i__3;

  /* Local variables */
  int ldim, info, ipvt[4], nsys;
  int i__, j, k, l;
  double p[4], t[16] /* was [4][4] */ , dprec;
  int ia, dk, dl, kk, ll, km1, job, ldl;

  /*%calling sequence 
   *     subroutine lycsr(n,a,na,c,ierr) 
   *    int n,na,ierr 
   *    double precision a(na,n),c(na,n) 
   *%purpose 
   * 
   * 
   *    this routine solves the continuous lyapunov equation where 
   *    the matrix  a  has been transformed to quasi-triangular form. 
   * 
   *    this routine is intended to be called only from 
   *           slice   routine  lybsc . 
   *% 
   */
  /* Parameter adjustments */
  c_dim1 = *na;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  *ierr = 0;
  ldim = 4;
  job = 0;
  l = 1;
 L10:
  dl = 1;
  if (l == *n)
    {
      goto L20;
    }
  if (a[l + 1 + l * a_dim1] != 0.)
    {
      dl = 2;
    }
 L20:
  ll = l + dl - 1;
  k = l;
 L30:
  km1 = k - 1;
  dk = 1;
  if (k == *n)
    {
      goto L34;
    }
  if (a[k + 1 + k * a_dim1] != 0.)
    {
      dk = 2;
    }
 L34:
  kk = k + dk - 1;
  if (k == l)
    {
      goto L45;
    }
  /* 
   */
  i__1 = kk;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = ll;
      for (j = l; j <= i__2; ++j)
	{
	  dprec = 0.;
	  i__3 = km1;
	  for (ia = l; ia <= i__3; ++ia)
	    {
	      dprec += a[ia + i__ * a_dim1] * c__[ia + j * c_dim1];
	      /* L35: */
	    }
	  c__[i__ + j * c_dim1] -= dprec;
	  /* L40: */
	}
    }
  /* 
   */
 L45:
  if (dl == 2)
    {
      goto L60;
    }
  if (dk == 2)
    {
      goto L50;
    }
  t[0] = a[k + k * a_dim1] + a[l + l * a_dim1];
  if (t[0] == 0.)
    {
      goto L130;
    }
  c__[k + l * c_dim1] /= t[0];
  goto L90;
 L50:
  t[0] = a[k + k * a_dim1] + a[l + l * a_dim1];
  t[4] = a[kk + k * a_dim1];
  t[1] = a[k + kk * a_dim1];
  t[5] = a[kk + kk * a_dim1] + a[l + l * a_dim1];
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[kk + l * c_dim1];
  nsys = 2;
  nsp_ctrlpack_dgefa (t, &ldim, &nsys, ipvt, &info);
  if (info != 0)
    {
      goto L130;
    }
  nsp_ctrlpack_dgesl (t, &ldim, &nsys, ipvt, p, &job);
  c__[k + l * c_dim1] = p[0];
  c__[kk + l * c_dim1] = p[1];
  goto L90;
 L60:
  if (dk == 2)
    {
      goto L70;
    }
  t[0] = a[k + k * a_dim1] + a[l + l * a_dim1];
  t[4] = a[ll + l * a_dim1];
  t[1] = a[l + ll * a_dim1];
  t[5] = a[k + k * a_dim1] + a[ll + ll * a_dim1];
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[k + ll * c_dim1];
  nsys = 2;
  nsp_ctrlpack_dgefa (t, &ldim, &nsys, ipvt, &info);
  if (info != 0)
    {
      goto L130;
    }
  nsp_ctrlpack_dgesl (t, &ldim, &nsys, ipvt, p, &job);
  c__[k + l * c_dim1] = p[0];
  c__[k + ll * c_dim1] = p[1];
  goto L90;
 L70:
  if (k != l)
    {
      goto L80;
    }
  t[0] = a[l + l * a_dim1];
  t[4] = a[ll + l * a_dim1];
  t[8] = 0.;
  t[1] = a[l + ll * a_dim1];
  t[5] = a[l + l * a_dim1] + a[ll + ll * a_dim1];
  t[9] = t[4];
  t[2] = 0.;
  t[6] = t[1];
  t[10] = a[ll + ll * a_dim1];
  p[0] = c__[l + l * c_dim1] * .5;
  p[1] = c__[ll + l * c_dim1];
  p[2] = c__[ll + ll * c_dim1] * .5;
  nsys = 3;
  nsp_ctrlpack_dgefa (t, &ldim, &nsys, ipvt, &info);
  if (info != 0)
    {
      goto L130;
    }
  nsp_ctrlpack_dgesl (t, &ldim, &nsys, ipvt, p, &job);
  c__[l + l * c_dim1] = p[0];
  c__[ll + l * c_dim1] = p[1];
  c__[l + ll * c_dim1] = p[1];
  c__[ll + ll * c_dim1] = p[2];
  goto L90;
 L80:
  t[0] = a[k + k * a_dim1] + a[l + l * a_dim1];
  t[4] = a[kk + k * a_dim1];
  t[8] = a[ll + l * a_dim1];
  t[12] = 0.;
  t[1] = a[k + kk * a_dim1];
  t[5] = a[kk + kk * a_dim1] + a[l + l * a_dim1];
  t[9] = 0.;
  t[13] = t[8];
  t[2] = a[l + ll * a_dim1];
  t[6] = 0.;
  t[10] = a[k + k * a_dim1] + a[ll + ll * a_dim1];
  t[14] = t[4];
  t[3] = 0.;
  t[7] = t[2];
  t[11] = t[1];
  t[15] = a[kk + kk * a_dim1] + a[ll + ll * a_dim1];
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[kk + l * c_dim1];
  p[2] = c__[k + ll * c_dim1];
  p[3] = c__[kk + ll * c_dim1];
  nsys = 4;
  nsp_ctrlpack_dgefa (t, &ldim, &nsys, ipvt, &info);
  if (info != 0)
    {
      goto L130;
    }
  nsp_ctrlpack_dgesl (t, &ldim, &nsys, ipvt, p, &job);
  c__[k + l * c_dim1] = p[0];
  c__[kk + l * c_dim1] = p[1];
  c__[k + ll * c_dim1] = p[2];
  c__[kk + ll * c_dim1] = p[3];
 L90:
  k += dk;
  if (k <= *n)
    {
      goto L30;
    }
  ldl = l + dl;
  if (ldl > *n)
    {
      return 0;
    }
  /* 
   */
  i__2 = *n;
  for (j = ldl; j <= i__2; ++j)
    {
      /* 
       */
      i__1 = ll;
      for (i__ = l; i__ <= i__1; ++i__)
	{
	  c__[i__ + j * c_dim1] = c__[j + i__ * c_dim1];
	  /* L100: */
	}
      /* 
       */
      i__1 = *n;
      for (i__ = j; i__ <= i__1; ++i__)
	{
	  dprec = 0.;
	  i__3 = ll;
	  for (k = l; k <= i__3; ++k)
	    {
	      dprec += c__[i__ + k * c_dim1] * a[k + j * a_dim1];
	      dprec += a[k + i__ * a_dim1] * c__[k + j * c_dim1];
	      /* L110: */
	    }
	  c__[i__ + j * c_dim1] -= dprec;
	  c__[j + i__ * c_dim1] = c__[i__ + j * c_dim1];
	  /* L120: */
	}
    }
  /* 
   */
  l = ldl;
  goto L10;
  /* 
   */
 L130:
  *ierr = 1;
  return 0;
}				/* lycsr_ */
