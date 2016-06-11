/* shrslv.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__4 = 4;
static int c__0 = 0;

int
nsp_ctrlpack_shrslv (double *a, double *b, double *c__, int *m, int *n,
		     int *na, int *nb, int *nc, double *eps, double *cond,
		     double *rmax, int *fail)
{
  /* Initialized data */

  static double zero = 0.;

  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2;
  double d__1;

  /* Builtin functions */
  double sqrt (double), d_sign (double *, double *);

  /* Local variables */
  int info, ipvt[4], nsys;
  int i__, j, k, l;
  double p[4];
  double t[16] /* was [4][4] */ , z__[4];
  double rcond, const__=0;
  int dk, dl, kk, ll, km1, lm1;

  /* 
   *!purpose 
   *  shrslv is a fortran iv subroutine to solve the real matrix 
   *  equation ax + xb = c, where a is in lower real schur form 
   *  and b is in upper real schur form, 
   * 
   *!calling sequence 
   * 
   *      subroutine shrslv(a,b,c,m,n,na,nb,nc,eps,cond,rmax,fail) 
   *  a      a doubly subscripted array containg the matrix a in 
   *         lower schur form 
   * 
   *  b      a doubly subscripted array containing tbe matrix b 
   *         in upper real schur form 
   * 
   *  c      a doubly subscripted array containing the matrix c. 
   * 
   *  m      the order of the matrix a 
   * 
   *  n      the order of the matrix b 
   * 
   *  na     the first dimension of the array a 
   * 
   *  nb     the first dimension of the array b 
   * 
   *  nc     the first dimension of the array c 
   * 
   *  eps    tolerance on a(k,k)+b(l,l) 
   *         if |a(k,k)+b(l,l)|<eps algorithm suppose that |a(k,k)+b(l,l)|=eps 
   * 
   *  cond    minimum allowed conditionnement for linear systems 
   *         if cond .le. 0 no estimation of conditionnement is done 
   * 
   *  rmax   maximum allowed size of any element of the transformation 
   * 
   *  fail   indicates if shrslv failed 
   * 
   *!auxiliary routines 
   *    ddot (blas) 
   *    dgeco dgefa dgesl (linpack) 
   *    dbas sqrt (fortran) 
   *!originator 
   *     Bartels and Stewart 
   *! 
   * 
   *internal variables 
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
  if (*cond > zero)
    {
      const__ = sqrt (sqrt (*cond));
    }
  /* 
   */
  info = 0;
  *fail = TRUE;
  l = 1;
 L10:
  lm1 = l - 1;
  dl = 1;
  if (l == *n)
    {
      goto L20;
    }
  if (b[l + 1 + l * b_dim1] != zero)
    {
      dl = 2;
    }
 L20:
  ll = l + dl - 1;
  if (l == 1)
    {
      goto L60;
    }
  i__1 = ll;
  for (j = l; j <= i__1; ++j)
    {
      i__2 = *m;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  c__[i__ + j * c_dim1] -=
	    C2F (ddot) (&lm1, &c__[i__ + c_dim1], nc,
			&b[j * b_dim1 + 1], &c__1);
	  /* L40: */
	}
      /* L50: */
    }
 L60:
  k = 1;
 L70:
  km1 = k - 1;
  dk = 1;
  if (k == *m)
    {
      goto L80;
    }
  if (a[k + (k + 1) * a_dim1] != zero)
    {
      dk = 2;
    }
 L80:
  kk = k + dk - 1;
  if (k == 1)
    {
      goto L120;
    }
  i__1 = kk;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      i__2 = ll;
      for (j = l; j <= i__2; ++j)
	{
	  c__[i__ + j * c_dim1] -=
	    C2F (ddot) (&km1, &a[i__ + a_dim1], na,
			&c__[j * c_dim1 + 1], &c__1);
	  /* L100: */
	}
      /* L110: */
    }
 L120:
  /*     write(6,'(''dl='',i1,'' dk='',i1)') dl,dk 
   */
  if (dl == 2)
    {
      goto L160;
    }
  if (dk == 2)
    {
      goto L130;
    }
  t[0] = a[k + k * a_dim1] + b[l + l * b_dim1];
  /*     write(6,'(e10.3,3x,e10.3)') t(1,1),c(k,l) 
   */
  if (Abs (t[0]) < *eps)
    {
      t[0] = d_sign (eps, t);
    }
  c__[k + l * c_dim1] /= t[0];
  /*     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(k,l),rmax 
   */
  if ((d__1 = c__[k + l * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
  goto L220;
 L130:
  t[0] = a[k + k * a_dim1] + b[l + l * b_dim1];
  t[4] = a[k + kk * a_dim1];
  t[1] = a[kk + k * a_dim1];
  t[5] = a[kk + kk * a_dim1] + b[l + l * b_dim1];
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[kk + l * c_dim1];
  /*     write(6,'(e10.3,3x,e10.3,3x,e10.3)') t(1,1),t(1,2),p(1) 
   *     write(6,'(e10.3,3x,e10.3,3x,e10.3)') t(2,1),t(2,2),p(2) 
   */
  nsys = 2;
  if (*cond > zero)
    {
      goto L140;
    }
  nsp_ctrlpack_dgefa (t, &c__4, &nsys, ipvt, &info);
  if (info > 0)
    {
      return 0;
    }
  goto L150;
 L140:
  nsp_ctrlpack_dgeco (t, &c__4, &nsys, ipvt, &rcond, z__);
  if (rcond < const__)
    {
      return 0;
    }
 L150:
  nsp_ctrlpack_dgesl (t, &c__4, &nsys, ipvt, p, &c__0);
  /*     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(k,l),rmax 
   *     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(kk,l),rmax 
   */
  c__[k + l * c_dim1] = p[0];
  if ((d__1 = c__[k + l * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
  c__[kk + l * c_dim1] = p[1];
  if ((d__1 = c__[kk + l * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
  goto L220;
 L160:
  if (dk == 2)
    {
      goto L190;
    }
  t[0] = a[k + k * a_dim1] + b[l + l * b_dim1];
  t[4] = b[ll + l * b_dim1];
  t[1] = b[l + ll * b_dim1];
  t[5] = a[k + k * a_dim1] + b[ll + ll * b_dim1];
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[k + ll * c_dim1];
  /*     write(6,'(e10.3,3x,e10.3,3x,e10.3)') t(1,1),t(1,2),p(1) 
   *     write(6,'(e10.3,3x,e10.3,3x,e10.3)') t(2,1),t(2,2),p(2) 
   */
  nsys = 2;
  if (*cond > zero)
    {
      goto L170;
    }
  nsp_ctrlpack_dgefa (t, &c__4, &nsys, ipvt, &info);
  if (info > 0)
    {
      return 0;
    }
  goto L180;
 L170:
  nsp_ctrlpack_dgeco (t, &c__4, &nsys, ipvt, &rcond, z__);
  if (rcond < const__)
    {
      return 0;
    }
 L180:
  nsp_ctrlpack_dgesl (t, &c__4, &nsys, ipvt, p, &c__0);
  /*     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(k,l),rmax 
   *     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(kk,l),rmax 
   */
  c__[k + l * c_dim1] = p[0];
  if ((d__1 = c__[k + l * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
  c__[k + ll * c_dim1] = p[1];
  if ((d__1 = c__[k + ll * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
  goto L220;
 L190:
  t[0] = a[k + k * a_dim1] + b[l + l * b_dim1];
  t[4] = a[k + kk * a_dim1];
  t[8] = b[ll + l * b_dim1];
  t[12] = zero;
  t[1] = a[kk + k * a_dim1];
  t[5] = a[kk + kk * a_dim1] + b[l + l * b_dim1];
  t[9] = zero;
  t[13] = t[8];
  t[2] = b[l + ll * b_dim1];
  t[6] = zero;
  t[10] = a[k + k * a_dim1] + b[ll + ll * b_dim1];
  t[14] = t[4];
  t[3] = zero;
  t[7] = t[2];
  t[11] = t[1];
  t[15] = a[kk + kk * a_dim1] + b[ll + ll * b_dim1];
  p[0] = c__[k + l * c_dim1];
  p[1] = c__[kk + l * c_dim1];
  p[2] = c__[k + ll * c_dim1];
  p[3] = c__[kk + ll * c_dim1];
  for (j = 1; j <= 4; ++j)
    {
      /*     write(6,'(5(e10.3,3x))') (t(j,i),i=1,4),p(j) 
       */
      /* L191: */
    }
  nsys = 4;
  if (*cond > zero)
    {
      goto L200;
    }
  nsp_ctrlpack_dgefa (t, &c__4, &nsys, ipvt, &info);
  if (info > 0)
    {
      return 0;
    }
  goto L210;
 L200:
  nsp_ctrlpack_dgeco (t, &c__4, &nsys, ipvt, &rcond, z__);
  if (rcond < const__)
    {
      return 0;
    }
 L210:
  nsp_ctrlpack_dgesl (t, &c__4, &nsys, ipvt, p, &c__0);
  /*     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(k,l),rmax 
   *     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(kk,l),rmax 
   *     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(k,ll),rmax 
   *     write(6,'(''c='',e10.3,'' rmax='',e10.3)') c(kk,ll),rmax 
   */
  c__[k + l * c_dim1] = p[0];
  if ((d__1 = c__[k + l * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
  c__[kk + l * c_dim1] = p[1];
  if ((d__1 = c__[kk + l * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
  c__[k + ll * c_dim1] = p[2];
  if ((d__1 = c__[k + ll * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
  c__[kk + ll * c_dim1] = p[3];
  if ((d__1 = c__[kk + ll * c_dim1], Abs (d__1)) >= *rmax)
    {
      return 0;
    }
 L220:
  k += dk;
  if (k <= *m)
    {
      goto L70;
    }
  l += dl;
  if (l <= *n)
    {
      goto L10;
    }
  *fail = FALSE;
  return 0;
}				/* shrslv_ */
