/* wqrsl.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"


/* Table of constant values */

static int c__1 = 1;

int
nsp_ctrlpack_wqrsl (double *xr, double *xi, int *ldx, int *n, int *k,
		    double *qrauxr, double *qrauxi, double *yr, double *yi,
		    double *qyr, double *qyi, double *qtyr, double *qtyi,
		    double *br, double *bi, double *rsdr, double *rsdi,
		    double *xbr, double *xbi, int *job, int *info)
{
  /* System generated locals */
  int xr_dim1, xr_offset, xi_dim1, xi_offset, i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int cqty;
  int i__, j;
  double tempi, tempr;
  int cb;
  int jj;
  int cr;
  double ti;
  int ju;
  double tr;
  int kp1;
  int cxb, cqy;

  /*!purpose 
   * 
   *    wqrsl applies the output of wqrdc to compute coordinate 
   *    transformations, projections, and least squares solutions. 
   *    for k .le. Min(n,p), let xk be the matrix 
   * 
   *           xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k))) 
   * 
   *    formed from columnns jpvt(1), ... ,jpvt(k) of the original 
   *    n x p matrix x that was input to wqrdc (if no pivoting was 
   *    done, xk consists of the first k columns of x in their 
   *    original order).  wqrdc produces a factored unitary matrix q 
   *    and an upper triangular matrix r such that 
   * 
   *             xk = q * (r) 
   *                      (0) 
   * 
   *    this information is contained in coded form in the arrays 
   *    x and qraux. 
   * 
   *!calling sequence 
   * 
   *     subroutine wqrsl(xr,xi,ldx,n,k,qrauxr,qrauxi,yr,yi,qyr,qyi,qtyr, 
   *    on entry 
   * 
   *       x      double-complex(ldx,p). 
   *              x contains the output of wqrdc. 
   * 
   *       ldx    int. 
   *              ldx is the leading dimension of the array x. 
   * 
   *       n      int. 
   *              n is the number of rows of the matrix xk.  it must 
   *              have the same value as n in wqrdc. 
   * 
   *       k      int. 
   *              k is the number of columns of the matrix xk.  k 
   *              must nnot be greater than Min(n,p), where p is the 
   *              same as in the calling sequence to wqrdc. 
   * 
   *       qraux  double-complex(p). 
   *              qraux contains the auxiliary output from wqrdc. 
   * 
   *       y      double-complex(n) 
   *              y contains an n-vector that is to be manipulated 
   *              by wqrsl. 
   * 
   *       job    int. 
   *              job specifies what is to be computed.  job has 
   *              the decimal expansion abcde, with the following 
   *              meaning. 
   * 
   *                   if a.ne.0, compute qy. 
   *                   if b,c,d, or e .ne. 0, compute qty. 
   *                   if c.ne.0, compute b. 
   *                   if d.ne.0, compute rsd. 
   *                   if e.ne.0, compute xb. 
   * 
   *              note that a request to compute b, rsd, or xb 
   *              automatically triggers the computation of qty, for 
   *              which an array must be provided in the calling 
   *              sequence. 
   * 
   *    on return 
   * 
   *       qy     double-complex(n). 
   *              qy conntains q*y, if its computation has been 
   *              requested. 
   * 
   *       qty    double-complex(n). 
   *              qty contains ctrans(q)*y, if its computation has 
   *              been requested.  here ctrans(q) is the conjugate 
   *              transpose of the matrix q. 
   * 
   *       b      double-complex(k) 
   *              b contains the solution of the least squares problem 
   * 
   *                   minimize norm2(y - xk*b), 
   * 
   *              if its computation has been requested.  (note that 
   *              if pivoting was requested in wqrdc, the j-th 
   *              component of b will be associated with column jpvt(j) 
   *              of the original matrix x that was input into wqrdc.) 
   * 
   *       rsd    double-complex(n). 
   *              rsd contains the least squares residual y - xk*b, 
   *              if its computation has been requested.  rsd is 
   *              also the orthogonal projection of y onto the 
   *              orthogonal complement of the column space of xk. 
   * 
   *       xb     double-complex(n). 
   *              xb contains the least squares approximation xk*b, 
   *              if its computation has been requested.  xb is also 
   *              the orthogonal projection of y onto the column space 
   *              of x. 
   * 
   *       info   int. 
   *              info is zero unless the computation of b has 
   *              been requested and r is exactly singular.  in 
   *              this case, info is the index of the first zero 
   *              diagonal element of r and b is left unaltered. 
   * 
   *    the parameters qy, qty, b, rsd, and xb are not referenced 
   *    if their computation is not requested and in this case 
   *    can be replaced by dummy variables in the calling program. 
   *    to save storage, the user may in some cases use the same 
   *    array for different parameters in the calling sequence.  a 
   *    frequently occuring example is when one wishes to compute 
   *    any of b, rsd, or xb and does not need y or qty.  in this 
   *    case one may identify y, qty, and one of b, rsd, or xb, while 
   *    providing separate arrays for anything else that is to be 
   *    computed.  thus the calling sequence 
   * 
   *         call wqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info) 
   * 
   *    will result in the computation of b and rsd, with rsd 
   *    overwriting y.  more generally, each item in the following 
   *    list contains groups of permissible identifications for 
   *    a single callinng sequence. 
   * 
   *         1. (y,qty,b) (rsd) (xb) (qy) 
   * 
   *         2. (y,qty,rsd) (b) (xb) (qy) 
   * 
   *         3. (y,qty,xb) (b) (rsd) (qy) 
   * 
   *         4. (y,qy) (qty,b) (rsd) (xb) 
   * 
   *         5. (y,qy) (qty,rsd) (b) (xb) 
   * 
   *         6. (y,qy) (qty,xb) (b) (rsd) 
   * 
   *    in any group the value returned in the array allocated to 
   *    the group corresponds to the last member of the group. 
   * 
   *!originator 
   *    linpack. this version dated 07/03/79 . 
   *    g.w. stewart, university of maryland, argonne national lab. 
   * 
   *!auxiliary routines 
   * 
   *    blas waxpy,wcopy,wdotcr,wdotci 
   *    fortran abs,dimag,min,mod 
   * 
   *    Copyright INRIA 
   *! 
   *    internal variables 
   * 
   * 
   * 
   *    set info flag. 
   * 
   */
  /* Parameter adjustments */
  xi_dim1 = *ldx;
  xi_offset = xi_dim1 + 1;
  xi -= xi_offset;
  xr_dim1 = *ldx;
  xr_offset = xr_dim1 + 1;
  xr -= xr_offset;
  --qrauxr;
  --qrauxi;
  --yr;
  --yi;
  --qyr;
  --qyi;
  --qtyr;
  --qtyi;
  --br;
  --bi;
  --rsdr;
  --rsdi;
  --xbr;
  --xbi;

  /* Function Body */
  *info = 0;
  /* 
   *    determine what is to be computed. 
   * 
   */
  cqy = *job / 10000 != 0;
  cqty = *job % 10000 != 0;
  cb = *job % 1000 / 100 != 0;
  cr = *job % 100 / 10 != 0;
  cxb = *job % 10 != 0;
  /*Computing MIN 
   */
  i__1 = *k, i__2 = *n - 1;
  ju = Min (i__1, i__2);
  /* 
   *    special action when n=1. 
   * 
   */
  if (ju != 0)
    {
      goto L80;
    }
  if (!cqy)
    {
      goto L10;
    }
  qyr[1] = yr[1];
  qyi[1] = yi[1];
 L10:
  if (!cqty)
    {
      goto L20;
    }
  qtyr[1] = yr[1];
  qtyi[1] = yi[1];
 L20:
  if (!cxb)
    {
      goto L30;
    }
  xbr[1] = yr[1];
  xbi[1] = yi[1];
 L30:
  if (!cb)
    {
      goto L60;
    }
  if ((d__1 = xr[xr_dim1 + 1], Abs (d__1)) + (d__2 =
					      xi[xi_dim1 + 1],
					      Abs (d__2)) != 0.)
    {
      goto L40;
    }
  *info = 1;
  goto L50;
 L40:
  nsp_calpack_wdiv (&yr[1], &yi[1], &xr[xr_dim1 + 1], &xi[xi_dim1 + 1],
		    &br[1], &bi[1]);
 L50:
 L60:
  if (!cr)
    {
      goto L70;
    }
  rsdr[1] = 0.;
  rsdi[1] = 0.;
 L70:
  goto L290;
 L80:
  /* 
   *       set up to compute qy or qty. 
   * 
   */
  if (cqy)
    {
      nsp_calpack_wcopy (n, &yr[1], &yi[1], &c__1, &qyr[1], &qyi[1], &c__1);
    }
  if (cqty)
    {
      nsp_calpack_wcopy (n, &yr[1], &yi[1], &c__1, &qtyr[1], &qtyi[1], &c__1);
    }
  if (!cqy)
    {
      goto L110;
    }
  /* 
   *          compute qy. 
   * 
   */
  i__1 = ju;
  for (jj = 1; jj <= i__1; ++jj)
    {
      j = ju - jj + 1;
      if ((d__1 = qrauxr[j], Abs (d__1)) + (d__2 =
					    qrauxi[j], Abs (d__2)) == 0.)
	{
	  goto L90;
	}
      tempr = xr[j + j * xr_dim1];
      tempi = xi[j + j * xi_dim1];
      xr[j + j * xr_dim1] = qrauxr[j];
      xi[j + j * xi_dim1] = qrauxi[j];
      i__2 = *n - j + 1;
      tr =
	-nsp_calpack_wdotcr (&i__2, &xr[j + j * xr_dim1],
			     &xi[j + j * xi_dim1], &c__1, &qyr[j], &qyi[j],
			     &c__1);
      i__2 = *n - j + 1;
      ti =
	-nsp_calpack_wdotci (&i__2, &xr[j + j * xr_dim1],
			     &xi[j + j * xi_dim1], &c__1, &qyr[j], &qyi[j],
			     &c__1);
      nsp_calpack_wdiv (&tr, &ti, &xr[j + j * xr_dim1], &xi[j + j * xi_dim1],
			&tr, &ti);
      i__2 = *n - j + 1;
      nsp_calpack_waxpy (&i__2, &tr, &ti, &xr[j + j * xr_dim1],
			 &xi[j + j * xi_dim1], &c__1, &qyr[j], &qyi[j],
			 &c__1);
      xr[j + j * xr_dim1] = tempr;
      xi[j + j * xi_dim1] = tempi;
    L90:
      /* L100: */
      ;
    }
 L110:
  if (!cqty)
    {
      goto L140;
    }
  /* 
   *          compute ctrans(q)*y. 
   * 
   */
  i__1 = ju;
  for (j = 1; j <= i__1; ++j)
    {
      if ((d__1 = qrauxr[j], Abs (d__1)) + (d__2 =
					    qrauxi[j], Abs (d__2)) == 0.)
	{
	  goto L120;
	}
      tempr = xr[j + j * xr_dim1];
      tempi = xi[j + j * xi_dim1];
      xr[j + j * xr_dim1] = qrauxr[j];
      xi[j + j * xi_dim1] = qrauxi[j];
      i__2 = *n - j + 1;
      tr =
	-nsp_calpack_wdotcr (&i__2, &xr[j + j * xr_dim1],
			     &xi[j + j * xi_dim1], &c__1, &qtyr[j], &qtyi[j],
			     &c__1);
      i__2 = *n - j + 1;
      ti =
	-nsp_calpack_wdotci (&i__2, &xr[j + j * xr_dim1],
			     &xi[j + j * xi_dim1], &c__1, &qtyr[j], &qtyi[j],
			     &c__1);
      nsp_calpack_wdiv (&tr, &ti, &xr[j + j * xr_dim1], &xi[j + j * xi_dim1],
			&tr, &ti);
      i__2 = *n - j + 1;
      nsp_calpack_waxpy (&i__2, &tr, &ti, &xr[j + j * xr_dim1],
			 &xi[j + j * xi_dim1], &c__1, &qtyr[j], &qtyi[j],
			 &c__1);
      xr[j + j * xr_dim1] = tempr;
      xi[j + j * xi_dim1] = tempi;
    L120:
      /* L130: */
      ;
    }
 L140:
  /* 
   *       set up to compute b, rsd, or xb. 
   * 
   */
  if (cb)
    {
      nsp_calpack_wcopy (k, &qtyr[1], &qtyi[1], &c__1, &br[1], &bi[1], &c__1);
    }
  kp1 = *k + 1;
  if (cxb)
    {
      nsp_calpack_wcopy (k, &qtyr[1], &qtyi[1], &c__1, &xbr[1], &xbi[1],
			 &c__1);
    }
  if (cr && *k < *n)
    {
      i__1 = *n - *k;
      nsp_calpack_wcopy (&i__1, &qtyr[kp1], &qtyi[kp1], &c__1, &rsdr[kp1],
			 &rsdi[kp1], &c__1);
    }
  if (!cxb || kp1 > *n)
    {
      goto L160;
    }
  i__1 = *n;
  for (i__ = kp1; i__ <= i__1; ++i__)
    {
      xbr[i__] = 0.;
      xbi[i__] = 0.;
      /* L150: */
    }
 L160:
  if (!cr)
    {
      goto L180;
    }
  i__1 = *k;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      rsdr[i__] = 0.;
      rsdi[i__] = 0.;
      /* L170: */
    }
 L180:
  if (!cb)
    {
      goto L230;
    }
  /* 
   *          compute b. 
   * 
   */
  i__1 = *k;
  for (jj = 1; jj <= i__1; ++jj)
    {
      j = *k - jj + 1;
      if ((d__1 = xr[j + j * xr_dim1], Abs (d__1)) + (d__2 =
						      xi[j + j * xi_dim1],
						      Abs (d__2)) != 0.)
	{
	  goto L190;
	}
      *info = j;
      /*                ......exit 
       *          ......exit 
       */
      goto L220;
    L190:
      nsp_calpack_wdiv (&br[j], &bi[j], &xr[j + j * xr_dim1],
			&xi[j + j * xi_dim1], &br[j], &bi[j]);
      if (j == 1)
	{
	  goto L200;
	}
      tr = -br[j];
      ti = -bi[j];
      i__2 = j - 1;
      nsp_calpack_waxpy (&i__2, &tr, &ti, &xr[j * xr_dim1 + 1],
			 &xi[j * xi_dim1 + 1], &c__1, &br[1], &bi[1], &c__1);
    L200:
      /* L210: */
      ;
    }
 L220:
 L230:
  if (!cr && !cxb)
    {
      goto L280;
    }
  /* 
   *          compute rsd or xb as required. 
   * 
   */
  i__1 = ju;
  for (jj = 1; jj <= i__1; ++jj)
    {
      j = ju - jj + 1;
      if ((d__1 = qrauxr[j], Abs (d__1)) + (d__2 =
					    qrauxi[j], Abs (d__2)) == 0.)
	{
	  goto L260;
	}
      tempr = xr[j + j * xr_dim1];
      tempi = xi[j + j * xi_dim1];
      xr[j + j * xr_dim1] = qrauxr[j];
      xi[j + j * xi_dim1] = qrauxi[j];
      if (!cr)
	{
	  goto L240;
	}
      i__2 = *n - j + 1;
      tr =
	-nsp_calpack_wdotcr (&i__2, &xr[j + j * xr_dim1],
			     &xi[j + j * xi_dim1], &c__1, &rsdr[j], &rsdi[j],
			     &c__1);
      i__2 = *n - j + 1;
      ti =
	-nsp_calpack_wdotci (&i__2, &xr[j + j * xr_dim1],
			     &xi[j + j * xi_dim1], &c__1, &rsdr[j], &rsdi[j],
			     &c__1);
      nsp_calpack_wdiv (&tr, &ti, &xr[j + j * xr_dim1], &xi[j + j * xi_dim1],
			&tr, &ti);
      i__2 = *n - j + 1;
      nsp_calpack_waxpy (&i__2, &tr, &ti, &xr[j + j * xr_dim1],
			 &xi[j + j * xi_dim1], &c__1, &rsdr[j], &rsdi[j],
			 &c__1);
    L240:
      if (!cxb)
	{
	  goto L250;
	}
      i__2 = *n - j + 1;
      tr =
	-nsp_calpack_wdotcr (&i__2, &xr[j + j * xr_dim1],
			     &xi[j + j * xi_dim1], &c__1, &xbr[j], &xbi[j],
			     &c__1);
      i__2 = *n - j + 1;
      ti =
	-nsp_calpack_wdotci (&i__2, &xr[j + j * xr_dim1],
			     &xi[j + j * xi_dim1], &c__1, &xbr[j], &xbi[j],
			     &c__1);
      nsp_calpack_wdiv (&tr, &ti, &xr[j + j * xr_dim1], &xi[j + j * xi_dim1],
			&tr, &ti);
      i__2 = *n - j + 1;
      nsp_calpack_waxpy (&i__2, &tr, &ti, &xr[j + j * xr_dim1],
			 &xi[j + j * xi_dim1], &c__1, &xbr[j], &xbi[j],
			 &c__1);
    L250:
      xr[j + j * xr_dim1] = tempr;
      xi[j + j * xi_dim1] = tempi;
    L260:
      /* L270: */
      ;
    }
 L280:
 L290:
  return 0;
}				/* wqrsl_ */
