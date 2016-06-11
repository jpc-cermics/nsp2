/* wqrdc.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"
#include "../libcalelm/calpack.h"


/* Table of constant values */

static int c__1 = 1;
static double c_b26 = 1.;
static double c_b27 = 0.;

/*/MEMBR ADD NAME=WQRDC,SSI=0 
 *    Copyright INRIA 
 */
int
nsp_ctrlpack_wqrdc (double *xr, double *xi, int *ldx, int *n, int *p,
		    double *qrauxr, double *qrauxi, int *jpvt, double *workr,
		    double *worki, int *job)
{
  /* System generated locals */
  int xr_dim1, xr_offset, xi_dim1, xi_offset, i__1, i__2, i__3;
  double d__1, d__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int negj;
  int maxj;
  int j, l;
  int swapj;
  int jj, jp, pl;
  double ti;
  int pu;
  double tr, tt;
  double maxnrm;
  double nrmxli;
  int lp1;
  double nrmxlr;
  int lup;

  /*!purpose 
   * 
   *    wqrdc uses householder transformations to compute the qr 
   *    factorization of an n by p matrix x.  column pivoting 
   *    based on the 2-norms of the reduced columns may be 
   *    performed at the users option. 
   * 
   *!calling sequence 
   * 
   *     subroutine wqrdc(xr,xi,ldx,n,p,qrauxr,qrauxi,jpvt,workr,worki, 
   *    on entry 
   * 
   *       x       double-complex(ldx,p), where ldx .ge. n. 
   *               x contains the matrix whose decomposition is to be 
   *               computed. 
   * 
   *       ldx     int. 
   *               ldx is the leading dimension of the array x. 
   * 
   *       n       int. 
   *               n is the number of rows of the matrix x. 
   * 
   *       p       int. 
   *               p is the number of columns of the matrix x. 
   * 
   *       jpvt    int(p). 
   *               jpvt contains ints that control the selection 
   *               of the pivot columns.  the k-th column x(k) of x 
   *               is placed in one of three classes according to the 
   *               value of jpvt(k). 
   * 
   *                  if jpvt(k) .gt. 0, then x(k) is an initial 
   *                                     column. 
   * 
   *                  if jpvt(k) .eq. 0, then x(k) is a free column. 
   * 
   *                  if jpvt(k) .lt. 0, then x(k) is a final column. 
   * 
   *               before the decomposition is computed, initial columns 
   *               are moved to the beginning of the array x and final 
   *               columns to the end.  both initial and final columns 
   *               are frozen in place during the computation and only 
   *               free columns are moved.  at the k-th stage of the 
   *               reduction, if x(k) is occupied by a free column 
   *               it is interchanged with the free column of largest 
   *               reduced norm.  jpvt is not referenced if 
   *               job .eq. 0. 
   * 
   *       work    double-complex(p). 
   *               work is a work array.  work is not referenced if 
   *               job .eq. 0. 
   * 
   *       job     int. 
   *               job is an int that initiates column pivoting. 
   *               if job .eq. 0, no pivoting is done. 
   *               if job .ne. 0, pivoting is done. 
   * 
   *    on return 
   * 
   *       x       x contains in its upper triangle the upper 
   *               triangular matrix r of the qr factorization. 
   *               below its diagonal x contains information from 
   *               which the unitary part of the decomposition 
   *               can be recovered.  note that if pivoting has 
   *               been requested, the decomposition is not that 
   *               of the original matrix x but that of x 
   *               with its columns permuted as described by jpvt. 
   * 
   *       qraux   double-complex(p). 
   *               qraux contains further information required to recover 
   *               the unitary part of the decomposition. 
   * 
   *       jpvt    jpvt(k) contains the index of the column of the 
   *               original matrix that has been interchanged into 
   *               the k-th column, if pivoting was requested. 
   * 
   *!originator 
   *    linpack. this version dated 07/03/79 . 
   *    g.w. stewart, university of maryland, argonne national lab. 
   * 
   *!auxiliary routines 
   * 
   *    blas waxpy,pythag,wdotcr,wdotci,wscal,wswap,wnrm2 
   *    fortran abs,dimag,max,min 
   * 
   *! 
   *    internal variables 
   * 
   * 
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
  --jpvt;
  --workr;
  --worki;

  /* Function Body */
  pl = 1;
  pu = 0;
  if (*job == 0)
    {
      goto L60;
    }
  /* 
   *       pivoting has been requested.  rearrange the columns 
   *       according to jpvt. 
   * 
   */
  i__1 = *p;
  for (j = 1; j <= i__1; ++j)
    {
      swapj = jpvt[j] > 0;
      negj = jpvt[j] < 0;
      jpvt[j] = j;
      if (negj)
	{
	  jpvt[j] = -j;
	}
      if (!swapj)
	{
	  goto L10;
	}
      if (j != pl)
	{
	  nsp_calpack_wswap (n, &xr[pl * xr_dim1 + 1], &xi[pl * xi_dim1 + 1],
			     &c__1, &xr[j * xr_dim1 + 1],
			     &xi[j * xi_dim1 + 1], &c__1);
	}
      jpvt[j] = jpvt[pl];
      jpvt[pl] = j;
      ++pl;
    L10:
      /* L20: */
      ;
    }
  pu = *p;
  i__1 = *p;
  for (jj = 1; jj <= i__1; ++jj)
    {
      j = *p - jj + 1;
      if (jpvt[j] >= 0)
	{
	  goto L40;
	}
      jpvt[j] = -jpvt[j];
      if (j == pu)
	{
	  goto L30;
	}
      nsp_calpack_wswap (n, &xr[pu * xr_dim1 + 1], &xi[pu * xi_dim1 + 1],
			 &c__1, &xr[j * xr_dim1 + 1], &xi[j * xi_dim1 + 1],
			 &c__1);
      jp = jpvt[pu];
      jpvt[pu] = jpvt[j];
      jpvt[j] = jp;
    L30:
      --pu;
    L40:
      /* L50: */
      ;
    }
 L60:
  /* 
   *    compute the norms of the free columns. 
   * 
   */
  if (pu < pl)
    {
      goto L80;
    }
  i__1 = pu;
  for (j = pl; j <= i__1; ++j)
    {
      qrauxr[j] =
	nsp_calpack_wnrm2 (n, &xr[j * xr_dim1 + 1], &xi[j * xi_dim1 + 1],
			   &c__1);
      qrauxi[j] = 0.;
      workr[j] = qrauxr[j];
      worki[j] = qrauxi[j];
      /* L70: */
    }
 L80:
  /* 
   *    perform the householder reduction of x. 
   * 
   */
  lup = Min (*n, *p);
  i__1 = lup;
  for (l = 1; l <= i__1; ++l)
    {
      if (l < pl || l >= pu)
	{
	  goto L120;
	}
      /* 
       *          locate the column of largest norm and bring it 
       *          into the pivot position. 
       * 
       */
      maxnrm = 0.;
      maxj = l;
      i__2 = pu;
      for (j = l; j <= i__2; ++j)
	{
	  if (qrauxr[j] <= maxnrm)
	    {
	      goto L90;
	    }
	  maxnrm = qrauxr[j];
	  maxj = j;
	L90:
	  /* L100: */
	  ;
	}
      if (maxj == l)
	{
	  goto L110;
	}
      nsp_calpack_wswap (n, &xr[l * xr_dim1 + 1], &xi[l * xi_dim1 + 1],
			 &c__1, &xr[maxj * xr_dim1 + 1],
			 &xi[maxj * xi_dim1 + 1], &c__1);
      qrauxr[maxj] = qrauxr[l];
      qrauxi[maxj] = qrauxi[l];
      workr[maxj] = workr[l];
      worki[maxj] = worki[l];
      jp = jpvt[maxj];
      jpvt[maxj] = jpvt[l];
      jpvt[l] = jp;
    L110:
    L120:
      qrauxr[l] = 0.;
      qrauxi[l] = 0.;
      if (l == *n)
	{
	  goto L200;
	}
      /* 
       *          compute the householder transformation for column l. 
       * 
       */
      i__2 = *n - l + 1;
      nrmxlr =
	nsp_calpack_wnrm2 (&i__2, &xr[l + l * xr_dim1], &xi[l + l * xi_dim1],
			   &c__1);
      nrmxli = 0.;
      if (Abs (nrmxlr) + Abs (nrmxli) == 0.)
	{
	  goto L190;
	}
      if ((d__1 = xr[l + l * xr_dim1], Abs (d__1)) + (d__2 =
						      xi[l + l * xi_dim1],
						      Abs (d__2)) == 0.)
	{
	  goto L130;
	}
      nsp_calpack_wsign (&nrmxlr, &nrmxli, &xr[l + l * xr_dim1],
			 &xi[l + l * xi_dim1], &nrmxlr, &nrmxli);
    L130:
      nsp_calpack_wdiv (&c_b26, &c_b27, &nrmxlr, &nrmxli, &tr, &ti);
      i__2 = *n - l + 1;
      nsp_calpack_wscal (&i__2, &tr, &ti, &xr[l + l * xr_dim1],
			 &xi[l + l * xi_dim1], &c__1);
      xr[l + l * xr_dim1] += 1.;
      /* 
       *             apply the transformation to the remaining columns, 
       *             updating the norms. 
       * 
       */
      lp1 = l + 1;
      if (*p < lp1)
	{
	  goto L180;
	}
      i__2 = *p;
      for (j = lp1; j <= i__2; ++j)
	{
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
	  if (j < pl || j > pu)
	    {
	      goto L160;
	    }
	  if ((d__1 = qrauxr[j], Abs (d__1)) + (d__2 =
						qrauxi[j], Abs (d__2)) == 0.)
	    {
	      goto L160;
	    }
	  /*Computing 2nd power 
	   */
	  d__1 =
	    nsp_calpack_pythag (&xr[l + j * xr_dim1],
				&xi[l + j * xi_dim1]) / qrauxr[j];
	  tt = 1. - d__1 * d__1;
	  tt = Max (tt, 0.);
	  tr = tt;
	  /*Computing 2nd power 
	   */
	  d__1 = qrauxr[j] / workr[j];
	  tt = tt * .05 * (d__1 * d__1) + 1.;
	  if (tt == 1.)
	    {
	      goto L140;
	    }
	  qrauxr[j] *= sqrt (tr);
	  qrauxi[j] *= sqrt (tr);
	  goto L150;
	L140:
	  i__3 = *n - l;
	  qrauxr[j] =
	    nsp_calpack_wnrm2 (&i__3, &xr[l + 1 + j * xr_dim1],
			       &xi[l + 1 + j * xi_dim1], &c__1);
	  qrauxi[j] = 0.;
	  workr[j] = qrauxr[j];
	  worki[j] = qrauxi[j];
	L150:
	L160:
	  /* L170: */
	  ;
	}
    L180:
      /* 
       *             save the transformation. 
       * 
       */
      qrauxr[l] = xr[l + l * xr_dim1];
      qrauxi[l] = xi[l + l * xi_dim1];
      xr[l + l * xr_dim1] = -nrmxlr;
      xi[l + l * xi_dim1] = -nrmxli;
    L190:
    L200:
      /* L210: */
      ;
    }
  return 0;
}				/* wqrdc_ */
