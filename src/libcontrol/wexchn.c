/* wexchn.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

int
nsp_ctrlpack_wexchn (double *ar, double *ai, double *vr, double *vi, int *n,
		     int *l, int *fail, int *na, int *nv)
{
  /* Initialized data */

  static double zero = 0.;

  /* System generated locals */
  int ar_dim1, ar_offset, ai_dim1, ai_offset, vr_dim1, vr_offset, vi_dim1,
    vi_offset, i__1;
  double d__1, d__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int i__, j;
  double r__;
  int l1;
  double pi, qi, si, ti, pr, qr, sr, tr;

  /*!purpose 
   * 
   *    given the upper triangular complex matrix a ,wexchn produce a 
   *    unitary transformation which exchange the two consecutive blocks 
   *    starting at a(l,l),along with their eigenvalues. 
   *    the transformation is accumulated in v. 
   *!calling sequence 
   * 
   *    subroutine exchng(ar, ai, vr, vi, n, l, fail, na, nv) 
   * 
   *    int l, na, nv 
   *    double precision ar, ai, vr, vi 
   *    dimension ar(na,n) , ai(na,n) ,vr(nv,n) ,vi(nv,n) 
   *    int fail 
   * 
   *    starred parameters are altered by the subroutine 
   * 
   *       *ar,ai  the matrix whose blocks are to be 
   *               interchanged. 
   *       *vr,vi  the array into which the transformations 
   *               are to re accumulated. 
   *        n      the order of the matrix a. 
   *        l      the position of the blocks. 
   *      *fail    a int variable which is false on a 
   *               normal return.  if thirty iterations were 
   *               performed without convergence, fail is set 
   *               to true and the element 
   *               a(l+b2,l+b2-1) cannot be assumed zero. 
   *        na     the first dimension of the array a. 
   *        nv     the first dimension of the array v. 
   * 
   *!auxiliary routines 
   *    max sqrt Abs(fortran) 
   *!originator 
   *    steer i.n.r.i.a  from routine exchng 
   *! 
   * 
   * 
   *    Copyright INRIA 
   * 
   *    internal variables. 
   * 
   */
  /* Parameter adjustments */
  ai_dim1 = *na;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *na;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  vi_dim1 = *nv;
  vi_offset = vi_dim1 + 1;
  vi -= vi_offset;
  vr_dim1 = *nv;
  vr_offset = vr_dim1 + 1;
  vr -= vr_offset;

  /* Function Body */
  l1 = *l + 1;
  /* 
   */
  *fail = FALSE;
  /* 
   *        interchange 1x1 and 1x1 blocks. 
   * 
   */
  qr = ar[l1 + l1 * ar_dim1] - ar[*l + *l * ar_dim1];
  pr = ar[*l + l1 * ar_dim1];
  qi = ai[l1 + l1 * ai_dim1] - ai[*l + *l * ai_dim1];
  pi = ai[*l + l1 * ai_dim1];
  /*Computing MAX 
   */
  d__1 = Abs (pr), d__2 = Abs (pi), d__1 = Max (d__1, d__2), d__2 =
    Abs (qr), d__1 = Max (d__1, d__2), d__2 = Abs (qi);
  r__ = Max (d__1, d__2);
  if (r__ == zero)
    {
      return 0;
    }
  pr /= r__;
  qr /= r__;
  pi /= r__;
  qi /= r__;
  r__ = sqrt (pr * pr + pi * pi + qr * qr + qi * qi);
  pr /= r__;
  qr /= r__;
  pi /= r__;
  qi /= r__;
  i__1 = *n;
  for (j = *l; j <= i__1; ++j)
    {
      sr = ar[*l + j * ar_dim1];
      si = ai[*l + j * ai_dim1];
      tr = ar[l1 + j * ar_dim1];
      ti = ai[l1 + j * ai_dim1];
      ar[*l + j * ar_dim1] = pr * sr + pi * si + qr * tr + qi * ti;
      ai[*l + j * ai_dim1] = pr * si - pi * sr + qr * ti - qi * tr;
      ar[l1 + j * ar_dim1] = pr * tr - pi * ti - qr * sr + qi * si;
      ai[l1 + j * ai_dim1] = pr * ti + pi * tr - qr * si - qi * sr;
      /* L10: */
    }
  i__1 = l1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      sr = ar[i__ + *l * ar_dim1];
      si = ai[i__ + *l * ai_dim1];
      tr = ar[i__ + l1 * ar_dim1];
      ti = ai[i__ + l1 * ai_dim1];
      ar[i__ + *l * ar_dim1] = pr * sr + qr * tr - pi * si - qi * ti;
      ai[i__ + *l * ai_dim1] = pi * sr + qi * tr + pr * si + qr * ti;
      ar[i__ + l1 * ar_dim1] = pr * tr + pi * ti - qr * sr - qi * si;
      ai[i__ + l1 * ai_dim1] = pr * ti - pi * tr - qr * si + qi * sr;
      /* L20: */
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      sr = vr[i__ + *l * vr_dim1];
      si = vi[i__ + *l * vi_dim1];
      tr = vr[i__ + l1 * vr_dim1];
      ti = vi[i__ + l1 * vi_dim1];
      vr[i__ + *l * vr_dim1] = pr * sr + qr * tr - pi * si - qi * ti;
      vi[i__ + *l * vi_dim1] = pi * sr + qi * tr + pr * si + qr * ti;
      vr[i__ + l1 * vr_dim1] = pr * tr + pi * ti - qr * sr - qi * si;
      vi[i__ + l1 * vi_dim1] = pr * ti - pi * tr - qr * si + qi * sr;
      /* L30: */
    }
  ar[l1 + *l * ar_dim1] = zero;
  ai[l1 + *l * ai_dim1] = zero;
  return 0;
}				/* wexchn_ */
