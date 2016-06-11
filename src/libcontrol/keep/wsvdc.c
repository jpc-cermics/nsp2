/* wsvdc.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"

/* Table of constant values */

static int c__1 = 1;
static double c_b8 = 1.;
static double c_b9 = 0.;
static double c_b57 = -1.;

int
nsp_ctrlpack_wsvdc (double *xr, double *xi, int *ldx, int *n, int *p,
		    double *sr, double *si, double *er, double *ei,
		    double *ur, double *ui, int *ldu, double *vr, double *vi,
		    int *ldv, double *workr, double *worki, int *job,
		    int *info)
{
  /* System generated locals */
  int xr_dim1, xr_offset, xi_dim1, xi_offset, ur_dim1, ur_offset, ui_dim1,
    ui_offset, vr_dim1, vr_offset, vi_dim1, vi_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int kase, jobu, iter;
  double test;
  int nctp1;
  double b, c__;
  int nrtp1;
  double f, g;
  int i__, j, k, l, m;
  double scale;
  double shift;
  int maxit;
  int wantu, wantv;
  double t1;
  double ztest, el;
  int kk, ll;
  double cs;
  int mm;
  double ri, ti;
  int ls, lu;
  double sl, rr, tr, sm, sn;
  int lm1, mm1, lp1, mp1, nct, ncu, lls, nrt;
  double emm1, smm1;

  /*!purpose 
   * 
   * 
   *    wsvdc is a subroutine to reduce a double-complex nxp matrix x by 
   *    unitary transformations u and v to diagonal form.  the 
   *    diagonal elements s(i) are the singular values of x.  the 
   *    columns of u are the corresponding left singular vectors, 
   *    and the columns of v the right singular vectors. 
   * 
   *!calling sequence 
   * 
   *     subroutine wsvdc(xr,xi,ldx,n,p,sr,si,er,ei,ur,ui,ldu,vr,vi,ldv, 
   *    on entry 
   * 
   *        x         double-complex(ldx,p), where ldx.ge.n. 
   *                  x contains the matrix whose singular value 
   *                  decomposition is to be computed.  x is 
   *                  destroyed by wsvdc. 
   * 
   *        n         int. 
   *                  n is the number of rows of the matrix x. 
   * 
   *        p         int. 
   *                  p is the number of columns of the matrix x. 
   * 
   *        ldx       int. 
   *                  ldx is the leading dimension of the array x. 
   * 
   *        ldu       int. 
   *                  ldu is the leading dimension of the array u 
   *                  (see below). 
   * 
   *        ldv       int. 
   *                  ldv is the leading dimension of the array v 
   *                  (see below). 
   * 
   *        work      double-complex(n). 
   *                  work is a scratch array. 
   * 
   *        job       int. 
   *                  job controls the computation of the singular 
   *                  vectors.  it has the decimal expansion ab 
   *                  with the following meaning 
   * 
   *                       a.eq.0    do not compute the left singular 
   *                                 vectors. 
   *                       a.eq.1    return the n left singular vectors 
   *                                 in u. 
   *                       a.ge.2    returns the first Min(n,p) 
   *                                 left singular vectors in u. 
   *                       b.eq.0    do not compute the right singular 
   *                                 vectors. 
   *                       b.eq.1    return the right singular vectors 
   *                                 in v. 
   * 
   *    on return 
   * 
   *        s         double-complex(mm), where mm=min(n+1,p). 
   *                  the first Min(n,p) entries of s contain the 
   *                  singular values of x arranged in descending 
   *                  order of magnitude. 
   * 
   *        e         double-complex(p). 
   *                  e ordinarily contains zeros.  however see the 
   *                  discussion of info for exceptions. 
   * 
   *        u         double-complex(ldu,k), where ldu.ge.n. 
   *                  if joba.eq.1 then k.eq.n, 
   *                  if joba.eq.2 then k.eq.min(n,p). 
   *                  u contains the matrix of right singular vectors. 
   *                  u is not referenced if joba.eq.0.  if n.le.p 
   *                  or if joba.gt.2, then u may be identified with x 
   *                  in the subroutine call. 
   * 
   *        v         double-complex(ldv,p), where ldv.ge.p. 
   *                  v contains the matrix of right singular vectors. 
   *                  v is not referenced if jobb.eq.0.  if p.le.n, 
   *                  then v may be identified whth x in the 
   *                  subroutine call. 
   * 
   *        info      int. 
   *                  the singular values (and their corresponding 
   *                  singular vectors) s(info+1),s(info+2),...,s(m) 
   *                  are correct (here m=min(n,p)).  thus if 
   *                  info.eq.0, all the singular values and their 
   *                  vectors are correct.  in any event, the matrix 
   *                  b = ctrans(u)*x*v is the bidiagonal matrix 
   *                  with the elements of s on its diagonal and the 
   *                  elements of e on its super-diagonal (ctrans(u) 
   *                  is the conjugate-transpose of u).  thus the 
   *                  singular values of x and b are the same. 
   * 
   *!originator 
   *    linpack. this version dated 07/03/79 . 
   *    g.w. stewart, university of maryland, argonne national lab. 
   * 
   *!auxiliary routines 
   * 
   *    blas waxpy,pythag,wdotcr,wdotci,wscal,wswap,wnrm2,drotg 
   *    fortran abs,dimag,max 
   *    fortran max,min,mod,sqrt 
   * 
   *! 
   *    Copyright INRIA 
   *    internal variables 
   * 
   * 
   * 
   *    set the maximum number of iterations. 
   *    MODIFIED ACCORDING TO EISPACK HQR2 
   * 
   */
  /* Parameter adjustments */
  xi_dim1 = *ldx;
  xi_offset = xi_dim1 + 1;
  xi -= xi_offset;
  xr_dim1 = *ldx;
  xr_offset = xr_dim1 + 1;
  xr -= xr_offset;
  --sr;
  --si;
  --er;
  --ei;
  ui_dim1 = *ldu;
  ui_offset = ui_dim1 + 1;
  ui -= ui_offset;
  ur_dim1 = *ldu;
  ur_offset = ur_dim1 + 1;
  ur -= ur_offset;
  vi_dim1 = *ldv;
  vi_offset = vi_dim1 + 1;
  vi -= vi_offset;
  vr_dim1 = *ldv;
  vr_offset = vr_dim1 + 1;
  vr -= vr_offset;
  --workr;
  --worki;

  /* Function Body */
  maxit = Min (*n, *p) * 30;
  /* 
   * 
   *    determine what is to be computed. 
   * 
   */
  wantu = FALSE;
  wantv = FALSE;
  jobu = *job % 100 / 10;
  ncu = *n;
  if (jobu > 1)
    {
      ncu = Min (*n, *p);
    }
  if (jobu != 0)
    {
      wantu = TRUE;
    }
  if (*job % 10 != 0)
    {
      wantv = TRUE;
    }
  /* 
   *    reduce x to bidiagonal form, storing the diagonal elements 
   *    in s and the super-diagonal elements in e. 
   * 
   */
  *info = 0;
  /*Computing MIN 
   */
  i__1 = *n - 1;
  nct = Min (i__1, *p);
  /*Computing MAX 
   *Computing MIN 
   */
  i__3 = *p - 2;
  i__1 = 0, i__2 = Min (i__3, *n);
  nrt = Max (i__1, i__2);
  lu = Max (nct, nrt);
  if (lu < 1)
    {
      goto L190;
    }
  i__1 = lu;
  for (l = 1; l <= i__1; ++l)
    {
      lp1 = l + 1;
      if (l > nct)
	{
	  goto L30;
	}
      /* 
       *          compute the transformation for the l-th column and 
       *          place the l-th diagonal in s(l). 
       * 
       */
      i__2 = *n - l + 1;
      sr[l] =
	nsp_calpack_wnrm2 (&i__2, &xr[l + l * xr_dim1], &xi[l + l * xi_dim1],
			   &c__1);
      si[l] = 0.;
      if ((d__1 = sr[l], Abs (d__1)) + (d__2 = si[l], Abs (d__2)) == 0.)
	{
	  goto L20;
	}
      if ((d__1 = xr[l + l * xr_dim1], Abs (d__1)) + (d__2 =
						      xi[l + l * xi_dim1],
						      Abs (d__2)) == 0.)
	{
	  goto L10;
	}
      nsp_calpack_wsign (&sr[l], &si[l], &xr[l + l * xr_dim1],
			 &xi[l + l * xi_dim1], &sr[l], &si[l]);
    L10:
      nsp_calpack_wdiv (&c_b8, &c_b9, &sr[l], &si[l], &tr, &ti);
      i__2 = *n - l + 1;
      nsp_calpack_wscal (&i__2, &tr, &ti, &xr[l + l * xr_dim1],
			 &xi[l + l * xi_dim1], &c__1);
      xr[l + l * xr_dim1] += 1.;
    L20:
      sr[l] = -sr[l];
      si[l] = -si[l];
    L30:
      if (*p < lp1)
	{
	  goto L60;
	}
      i__2 = *p;
      for (j = lp1; j <= i__2; ++j)
	{
	  if (l > nct)
	    {
	      goto L40;
	    }
	  if ((d__1 = sr[l], Abs (d__1)) + (d__2 = si[l], Abs (d__2)) == 0.)
	    {
	      goto L40;
	    }
	  /* 
	   *             apply the transformation. 
	   * 
	   */
	  i__3 = *n - l + 1;
	  tr =
	    -nsp_calpack_wdotcr (&i__3, &xr[l + l * xr_dim1],
				 &xi[l + l * xi_dim1], &c__1,
				 &xr[l + j * xr_dim1], &xi[l + j * xi_dim1],
				 &c__1);
	  i__3 = *n - l + 1;
	  ti =
	    -nsp_calpack_wdotci (&i__3, &xr[l + l * xr_dim1],
				 &xi[l + l * xi_dim1], &c__1,
				 &xr[l + j * xr_dim1], &xi[l + j * xi_dim1],
				 &c__1);
	  nsp_calpack_wdiv (&tr, &ti, &xr[l + l * xr_dim1],
			    &xi[l + l * xi_dim1], &tr, &ti);
	  i__3 = *n - l + 1;
	  nsp_calpack_waxpy (&i__3, &tr, &ti, &xr[l + l * xr_dim1],
			     &xi[l + l * xi_dim1], &c__1,
			     &xr[l + j * xr_dim1], &xi[l + j * xi_dim1],
			     &c__1);
	L40:
	  /* 
	   *          place the l-th row of x into  e for the 
	   *          subsequent calculation of the row transformation. 
	   * 
	   */
	  er[j] = xr[l + j * xr_dim1];
	  ei[j] = -xi[l + j * xi_dim1];
	  /* L50: */
	}
    L60:
      if (!wantu || l > nct)
	{
	  goto L80;
	}
      /* 
       *          place the transformation in u for subsequent back 
       *          multiplication. 
       * 
       */
      i__2 = *n;
      for (i__ = l; i__ <= i__2; ++i__)
	{
	  ur[i__ + l * ur_dim1] = xr[i__ + l * xr_dim1];
	  ui[i__ + l * ui_dim1] = xi[i__ + l * xi_dim1];
	  /* L70: */
	}
    L80:
      if (l > nrt)
	{
	  goto L170;
	}
      /* 
       *          compute the l-th row transformation and place the 
       *          l-th super-diagonal in e(l). 
       * 
       */
      i__2 = *p - l;
      er[l] = nsp_calpack_wnrm2 (&i__2, &er[lp1], &ei[lp1], &c__1);
      ei[l] = 0.;
      if ((d__1 = er[l], Abs (d__1)) + (d__2 = ei[l], Abs (d__2)) == 0.)
	{
	  goto L100;
	}
      if ((d__1 = er[lp1], Abs (d__1)) + (d__2 = ei[lp1], Abs (d__2)) == 0.)
	{
	  goto L90;
	}
      nsp_calpack_wsign (&er[l], &ei[l], &er[lp1], &ei[lp1], &er[l], &ei[l]);
    L90:
      nsp_calpack_wdiv (&c_b8, &c_b9, &er[l], &ei[l], &tr, &ti);
      i__2 = *p - l;
      nsp_calpack_wscal (&i__2, &tr, &ti, &er[lp1], &ei[lp1], &c__1);
      er[lp1] += 1.;
    L100:
      er[l] = -er[l];
      ei[l] = ei[l];
      if (lp1 > *n
	  || (d__1 = er[l], Abs (d__1)) + (d__2 = ei[l], Abs (d__2)) == 0.)
	{
	  goto L140;
	}
      /* 
       *             apply the transformation. 
       * 
       */
      i__2 = *n;
      for (i__ = lp1; i__ <= i__2; ++i__)
	{
	  workr[i__] = 0.;
	  worki[i__] = 0.;
	  /* L110: */
	}
      i__2 = *p;
      for (j = lp1; j <= i__2; ++j)
	{
	  i__3 = *n - l;
	  nsp_calpack_waxpy (&i__3, &er[j], &ei[j], &xr[lp1 + j * xr_dim1],
			     &xi[lp1 + j * xi_dim1], &c__1, &workr[lp1],
			     &worki[lp1], &c__1);
	  /* L120: */
	}
      i__2 = *p;
      for (j = lp1; j <= i__2; ++j)
	{
	  d__1 = -er[j];
	  d__2 = -ei[j];
	  nsp_calpack_wdiv (&d__1, &d__2, &er[lp1], &ei[lp1], &tr, &ti);
	  i__3 = *n - l;
	  d__1 = -ti;
	  nsp_calpack_waxpy (&i__3, &tr, &d__1, &workr[lp1], &worki[lp1],
			     &c__1, &xr[lp1 + j * xr_dim1],
			     &xi[lp1 + j * xi_dim1], &c__1);
	  /* L130: */
	}
    L140:
      if (!wantv)
	{
	  goto L160;
	}
      /* 
       *             place the transformation in v for subsequent 
       *             back multiplication. 
       * 
       */
      i__2 = *p;
      for (i__ = lp1; i__ <= i__2; ++i__)
	{
	  vr[i__ + l * vr_dim1] = er[i__];
	  vi[i__ + l * vi_dim1] = ei[i__];
	  /* L150: */
	}
    L160:
    L170:
      /* L180: */
      ;
    }
 L190:
  /* 
   *    set up the final bidiagonal matrix or order m. 
   * 
   *Computing MIN 
   */
  i__1 = *p, i__2 = *n + 1;
  m = Min (i__1, i__2);
  nctp1 = nct + 1;
  nrtp1 = nrt + 1;
  if (nct >= *p)
    {
      goto L200;
    }
  sr[nctp1] = xr[nctp1 + nctp1 * xr_dim1];
  si[nctp1] = xi[nctp1 + nctp1 * xi_dim1];
 L200:
  if (*n >= m)
    {
      goto L210;
    }
  sr[m] = 0.;
  si[m] = 0.;
 L210:
  if (nrtp1 >= m)
    {
      goto L220;
    }
  er[nrtp1] = xr[nrtp1 + m * xr_dim1];
  ei[nrtp1] = xi[nrtp1 + m * xi_dim1];
 L220:
  er[m] = 0.;
  ei[m] = 0.;
  /* 
   *    if required, generate u. 
   * 
   */
  if (!wantu)
    {
      goto L350;
    }
  if (ncu < nctp1)
    {
      goto L250;
    }
  i__1 = ncu;
  for (j = nctp1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ur[i__ + j * ur_dim1] = 0.;
	  ui[i__ + j * ui_dim1] = 0.;
	  /* L230: */
	}
      ur[j + j * ur_dim1] = 1.;
      ui[j + j * ui_dim1] = 0.;
      /* L240: */
    }
 L250:
  if (nct < 1)
    {
      goto L340;
    }
  i__1 = nct;
  for (ll = 1; ll <= i__1; ++ll)
    {
      l = nct - ll + 1;
      if ((d__1 = sr[l], Abs (d__1)) + (d__2 = si[l], Abs (d__2)) == 0.)
	{
	  goto L300;
	}
      lp1 = l + 1;
      if (ncu < lp1)
	{
	  goto L270;
	}
      i__2 = ncu;
      for (j = lp1; j <= i__2; ++j)
	{
	  i__3 = *n - l + 1;
	  tr =
	    -nsp_calpack_wdotcr (&i__3, &ur[l + l * ur_dim1],
				 &ui[l + l * ui_dim1], &c__1,
				 &ur[l + j * ur_dim1], &ui[l + j * ui_dim1],
				 &c__1);
	  i__3 = *n - l + 1;
	  ti =
	    -nsp_calpack_wdotci (&i__3, &ur[l + l * ur_dim1],
				 &ui[l + l * ui_dim1], &c__1,
				 &ur[l + j * ur_dim1], &ui[l + j * ui_dim1],
				 &c__1);
	  nsp_calpack_wdiv (&tr, &ti, &ur[l + l * ur_dim1],
			    &ui[l + l * ui_dim1], &tr, &ti);
	  i__3 = *n - l + 1;
	  nsp_calpack_waxpy (&i__3, &tr, &ti, &ur[l + l * ur_dim1],
			     &ui[l + l * ui_dim1], &c__1,
			     &ur[l + j * ur_dim1], &ui[l + j * ui_dim1],
			     &c__1);
	  /* L260: */
	}
    L270:
      i__2 = *n - l + 1;
      nsp_calpack_wrscal (&i__2, &c_b57, &ur[l + l * ur_dim1],
			  &ui[l + l * ui_dim1], &c__1);
      ur[l + l * ur_dim1] += 1.;
      lm1 = l - 1;
      if (lm1 < 1)
	{
	  goto L290;
	}
      i__2 = lm1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ur[i__ + l * ur_dim1] = 0.;
	  ui[i__ + l * ui_dim1] = 0.;
	  /* L280: */
	}
    L290:
      goto L320;
    L300:
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ur[i__ + l * ur_dim1] = 0.;
	  ui[i__ + l * ui_dim1] = 0.;
	  /* L310: */
	}
      ur[l + l * ur_dim1] = 1.;
      ui[l + l * ui_dim1] = 0.;
    L320:
      /* L330: */
      ;
    }
 L340:
 L350:
  /* 
   *    if it is required, generate v. 
   * 
   */
  if (!wantv)
    {
      goto L400;
    }
  i__1 = *p;
  for (ll = 1; ll <= i__1; ++ll)
    {
      l = *p - ll + 1;
      lp1 = l + 1;
      if (l > nrt)
	{
	  goto L370;
	}
      if ((d__1 = er[l], Abs (d__1)) + (d__2 = ei[l], Abs (d__2)) == 0.)
	{
	  goto L370;
	}
      i__2 = *p;
      for (j = lp1; j <= i__2; ++j)
	{
	  i__3 = *p - l;
	  tr =
	    -nsp_calpack_wdotcr (&i__3, &vr[lp1 + l * vr_dim1],
				 &vi[lp1 + l * vi_dim1], &c__1,
				 &vr[lp1 + j * vr_dim1],
				 &vi[lp1 + j * vi_dim1], &c__1);
	  i__3 = *p - l;
	  ti =
	    -nsp_calpack_wdotci (&i__3, &vr[lp1 + l * vr_dim1],
				 &vi[lp1 + l * vi_dim1], &c__1,
				 &vr[lp1 + j * vr_dim1],
				 &vi[lp1 + j * vi_dim1], &c__1);
	  nsp_calpack_wdiv (&tr, &ti, &vr[lp1 + l * vr_dim1],
			    &vi[lp1 + l * vi_dim1], &tr, &ti);
	  i__3 = *p - l;
	  nsp_calpack_waxpy (&i__3, &tr, &ti, &vr[lp1 + l * vr_dim1],
			     &vi[lp1 + l * vi_dim1], &c__1,
			     &vr[lp1 + j * vr_dim1], &vi[lp1 + j * vi_dim1],
			     &c__1);
	  /* L360: */
	}
    L370:
      i__2 = *p;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  vr[i__ + l * vr_dim1] = 0.;
	  vi[i__ + l * vi_dim1] = 0.;
	  /* L380: */
	}
      vr[l + l * vr_dim1] = 1.;
      vi[l + l * vi_dim1] = 0.;
      /* L390: */
    }
 L400:
  /* 
   *    transform s and e so that they are real. 
   * 
   */
  i__1 = m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      tr = nsp_calpack_pythag (&sr[i__], &si[i__]);
      if (tr == 0.)
	{
	  goto L405;
	}
      rr = sr[i__] / tr;
      ri = si[i__] / tr;
      sr[i__] = tr;
      si[i__] = 0.;
      if (i__ < m)
	{
	  nsp_calpack_wdiv (&er[i__], &ei[i__], &rr, &ri, &er[i__], &ei[i__]);
	}
      if (wantu)
	{
	  nsp_calpack_wscal (n, &rr, &ri, &ur[i__ * ur_dim1 + 1],
			     &ui[i__ * ui_dim1 + 1], &c__1);
	}
    L405:
      /*    ...exit 
       */
      if (i__ == m)
	{
	  goto L430;
	}
      tr = nsp_calpack_pythag (&er[i__], &ei[i__]);
      if (tr == 0.)
	{
	  goto L410;
	}
      nsp_calpack_wdiv (&tr, &c_b9, &er[i__], &ei[i__], &rr, &ri);
      er[i__] = tr;
      ei[i__] = 0.;
      nsp_calpack_wmul (&sr[i__ + 1], &si[i__ + 1], &rr, &ri, &sr[i__ + 1],
			&si[i__ + 1]);
      if (wantv)
	{
	  nsp_calpack_wscal (p, &rr, &ri, &vr[(i__ + 1) * vr_dim1 + 1],
			     &vi[(i__ + 1) * vi_dim1 + 1], &c__1);
	}
    L410:
      /* L420: */
      ;
    }
 L430:
  /* 
   *    main iteration loop for the singular values. 
   * 
   */
  mm = m;
  iter = 0;
 L440:
  /* 
   *       quit if all the singular values have been found. 
   * 
   *    ...exit 
   */
  if (m == 0)
    {
      goto L700;
    }
  /* 
   *       if too many iterations have been performed, set 
   *       flag and return. 
   * 
   */
  if (iter < maxit)
    {
      goto L450;
    }
  *info = m;
  /*    ......exit 
   */
  goto L700;
 L450:
  /* 
   *       this section of the program inspects for 
   *       negligible elements in the s and e arrays.  on 
   *       completion the variable kase is set as follows. 
   * 
   *          kase = 1     if sr(m) and er(l-1) are negligible and l.lt.m 
   *          kase = 2     if sr(l) is negligible and l.lt.m 
   *          kase = 3     if er(l-1) is negligible, l.lt.m, and 
   *                       sr(l), ..., sr(m) are not negligible (qr step). 
   *          kase = 4     if er(m-1) is negligible (convergence). 
   * 
   */
  i__1 = m;
  for (ll = 1; ll <= i__1; ++ll)
    {
      l = m - ll;
      /*       ...exit 
       */
      if (l == 0)
	{
	  goto L480;
	}
      test =
	nsp_calpack_pythag (&sr[l],
			    &si[l]) + nsp_calpack_pythag (&sr[l + 1],
							  &si[l + 1]);
      ztest = test + nsp_calpack_pythag (&er[l], &ei[l]);
      if (ztest != test)
	{
	  goto L460;
	}
      er[l] = 0.;
      ei[l] = 0.;
      /*       ......exit 
       */
      goto L480;
    L460:
      /* L470: */
      ;
    }
 L480:
  if (l != m - 1)
    {
      goto L490;
    }
  kase = 4;
  goto L560;
 L490:
  lp1 = l + 1;
  mp1 = m + 1;
  i__1 = mp1;
  for (lls = lp1; lls <= i__1; ++lls)
    {
      ls = m - lls + lp1;
      /*          ...exit 
       */
      if (ls == l)
	{
	  goto L520;
	}
      test = 0.;
      if (ls != m)
	{
	  test += nsp_calpack_pythag (&er[ls], &ei[ls]);
	}
      if (ls != l + 1)
	{
	  test += nsp_calpack_pythag (&er[ls - 1], &ei[ls - 1]);
	}
      ztest = test + nsp_calpack_pythag (&sr[ls], &si[ls]);
      if (ztest != test)
	{
	  goto L500;
	}
      sr[ls] = 0.;
      si[ls] = 0.;
      /*          ......exit 
       */
      goto L520;
    L500:
      /* L510: */
      ;
    }
 L520:
  if (ls != l)
    {
      goto L530;
    }
  kase = 3;
  goto L550;
 L530:
  if (ls != m)
    {
      goto L540;
    }
  kase = 1;
  goto L550;
 L540:
  kase = 2;
  l = ls;
 L550:
 L560:
  ++l;
  /* 
   *       perform the task indicated by kase. 
   * 
   */
  switch (kase)
    {
    case 1:
      goto L570;
    case 2:
      goto L600;
    case 3:
      goto L620;
    case 4:
      goto L650;
    }
  /* 
   *       deflate negligible s(m). 
   * 
   */
 L570:
  mm1 = m - 1;
  f = er[m - 1];
  er[m - 1] = 0.;
  ei[m - 1] = 0.;
  i__1 = mm1;
  for (kk = l; kk <= i__1; ++kk)
    {
      k = mm1 - kk + l;
      t1 = sr[k];
      C2F (drotg) (&t1, &f, &cs, &sn);
      sr[k] = t1;
      si[k] = 0.;
      if (k == l)
	{
	  goto L580;
	}
      f = -sn * er[k - 1];
      er[k - 1] = cs * er[k - 1];
      ei[k - 1] = cs * ei[k - 1];
    L580:
      if (wantv)
	{
	  C2F (drot) (p, &vr[k * vr_dim1 + 1], &c__1,
		      &vr[m * vr_dim1 + 1], &c__1, &cs, &sn);
	}
      if (wantv)
	{
	  C2F (drot) (p, &vi[k * vi_dim1 + 1], &c__1,
		      &vi[m * vi_dim1 + 1], &c__1, &cs, &sn);
	}
      /* L590: */
    }
  goto L690;
  /* 
   *       split at negligible s(l). 
   * 
   */
 L600:
  f = er[l - 1];
  er[l - 1] = 0.;
  ei[l - 1] = 0.;
  i__1 = m;
  for (k = l; k <= i__1; ++k)
    {
      t1 = sr[k];
      C2F (drotg) (&t1, &f, &cs, &sn);
      sr[k] = t1;
      si[k] = 0.;
      f = -sn * er[k];
      er[k] = cs * er[k];
      ei[k] = cs * ei[k];
      if (wantu)
	{
	  C2F (drot) (n, &ur[k * ur_dim1 + 1], &c__1,
		      &ur[(l - 1) * ur_dim1 + 1], &c__1, &cs, &sn);
	}
      if (wantu)
	{
	  C2F (drot) (n, &ui[k * ui_dim1 + 1], &c__1,
		      &ui[(l - 1) * ui_dim1 + 1], &c__1, &cs, &sn);
	}
      /* L610: */
    }
  goto L690;
  /* 
   *       perform one qr step. 
   * 
   */
 L620:
  /* 
   *          calculate the shift. 
   * 
   *Computing MAX 
   */
  d__1 = nsp_calpack_pythag (&sr[m], &si[m]), d__2 =
    nsp_calpack_pythag (&sr[m - 1], &si[m - 1]), d__1 =
    Max (d__1, d__2), d__2 =
    nsp_calpack_pythag (&er[m - 1], &ei[m - 1]), d__1 =
    Max (d__1, d__2), d__2 = nsp_calpack_pythag (&sr[l], &si[l]), d__1 =
    Max (d__1, d__2), d__2 = nsp_calpack_pythag (&er[l], &ei[l]);
  scale = Max (d__1, d__2);
  sm = sr[m] / scale;
  smm1 = sr[m - 1] / scale;
  emm1 = er[m - 1] / scale;
  sl = sr[l] / scale;
  el = er[l] / scale;
  /*Computing 2nd power 
   */
  d__1 = emm1;
  b = ((smm1 + sm) * (smm1 - sm) + d__1 * d__1) / 2.;
  /*Computing 2nd power 
   */
  d__1 = sm * emm1;
  c__ = d__1 * d__1;
  shift = 0.;
  if (b == 0. && c__ == 0.)
    {
      goto L630;
    }
  /*Computing 2nd power 
   */
  d__1 = b;
  shift = sqrt (d__1 * d__1 + c__);
  if (b < 0.)
    {
      shift = -shift;
    }
  shift = c__ / (b + shift);
 L630:
  /*           f = (sl + sm)*(sl - sm) - shift 
   */
  f = (sl + sm) * (sl - sm) + shift;
  g = sl * el;
  /* 
   *          chase zeros. 
   * 
   */
  mm1 = m - 1;
  i__1 = mm1;
  for (k = l; k <= i__1; ++k)
    {
      C2F (drotg) (&f, &g, &cs, &sn);
      if (k != l)
	{
	  er[k - 1] = f;
	  ei[k - 1] = 0.;
	}
      f = cs * sr[k] + sn * er[k];
      er[k] = cs * er[k] - sn * sr[k];
      ei[k] = cs * ei[k] - sn * si[k];
      g = sn * sr[k + 1];
      sr[k + 1] = cs * sr[k + 1];
      si[k + 1] = cs * si[k + 1];
      if (wantv)
	{
	  C2F (drot) (p, &vr[k * vr_dim1 + 1], &c__1,
		      &vr[(k + 1) * vr_dim1 + 1], &c__1, &cs, &sn);
	}
      if (wantv)
	{
	  C2F (drot) (p, &vi[k * vi_dim1 + 1], &c__1,
		      &vi[(k + 1) * vi_dim1 + 1], &c__1, &cs, &sn);
	}
      C2F (drotg) (&f, &g, &cs, &sn);
      sr[k] = f;
      si[k] = 0.;
      f = cs * er[k] + sn * sr[k + 1];
      sr[k + 1] = -sn * er[k] + cs * sr[k + 1];
      si[k + 1] = -sn * ei[k] + cs * si[k + 1];
      g = sn * er[k + 1];
      er[k + 1] = cs * er[k + 1];
      ei[k + 1] = cs * ei[k + 1];
      if (wantu && k < *n)
	{
	  C2F (drot) (n, &ur[k * ur_dim1 + 1], &c__1,
		      &ur[(k + 1) * ur_dim1 + 1], &c__1, &cs, &sn);
	}
      if (wantu && k < *n)
	{
	  C2F (drot) (n, &ui[k * ui_dim1 + 1], &c__1,
		      &ui[(k + 1) * ui_dim1 + 1], &c__1, &cs, &sn);
	}
      /* L640: */
    }
  er[m - 1] = f;
  ei[m - 1] = 0.;
  ++iter;
  goto L690;
  /* 
   *       convergence 
   * 
   */
 L650:
  /* 
   *          make the singular value  positive 
   * 
   */
  if (sr[l] >= 0.)
    {
      goto L660;
    }
  sr[l] = -sr[l];
  si[l] = -si[l];
  if (wantv)
    {
      nsp_calpack_wrscal (p, &c_b57, &vr[l * vr_dim1 + 1],
			  &vi[l * vi_dim1 + 1], &c__1);
    }
 L660:
  /* 
   *          order the singular value. 
   * 
   */
 L670:
  if (l == mm)
    {
      goto L680;
    }
  /*          ...exit 
   */
  if (sr[l] >= sr[l + 1])
    {
      goto L680;
    }
  tr = sr[l];
  sr[l] = sr[l + 1];
  sr[l + 1] = tr;
  tr = si[l];
  si[l] = si[l + 1];
  si[l + 1] = tr;
  if (wantv && l < *p)
    {
      nsp_calpack_wswap (p, &vr[l * vr_dim1 + 1], &vi[l * vi_dim1 + 1],
			 &c__1, &vr[(l + 1) * vr_dim1 + 1],
			 &vi[(l + 1) * vi_dim1 + 1], &c__1);
    }
  if (wantu && l < *n)
    {
      nsp_calpack_wswap (n, &ur[l * ur_dim1 + 1], &ui[l * ui_dim1 + 1],
			 &c__1, &ur[(l + 1) * ur_dim1 + 1],
			 &ui[(l + 1) * ui_dim1 + 1], &c__1);
    }
  ++l;
  goto L670;
 L680:
  iter = 0;
  --m;
 L690:
  goto L440;
 L700:
  return 0;
}				/* wsvdc_ */
