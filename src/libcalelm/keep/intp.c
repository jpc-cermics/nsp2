/* intp.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;

int
nsp_calpack_intp (double *x, double *xd, double *yd, int *n, int *nc,
		  double *y)
{
  /* System generated locals */
  int yd_dim1, yd_offset, i__1;

  /* Local variables */
  int i__;
  int inter;

  /*!purpose 
   *    linear interpolation computes y=F(x) for f a tabulated function 
   *     from R to R^n 
   *!parameters 
   *    x    : x given point 
   *    xd   : vector (nc) of abscissae mesh points (xd(i+1)>=xd(i)) 
   *    yd   : matrix (nc x n): yd(i,j)=Fj(x(i)) 
   *    n    : dimension of F image 
   *    returned values 
   *    y    : vector (n) :interpolated value of F(x) 
   *!remarks 
   *    if x<=xd(1) y=yd(1,:) 
   *     if x>=xd(nc) y=yd(nc,:) 
   *!origin 
   *    Pejman GOHARI 1996 
   *! 
   *    Copyright INRIA 
   * 
   */
  /* Parameter adjustments */
  --xd;
  yd_dim1 = *nc;
  yd_offset = yd_dim1 + 1;
  yd -= yd_offset;
  --y;

  /* Function Body */
  if (*nc == 1)
    {
      C2F (dcopy) (n, &yd[yd_dim1 + 1], nc, &y[1], &c__1);
    }
  else if (*x >= xd[*nc])
    {
      C2F (dcopy) (n, &yd[*nc + yd_dim1], nc, &y[1], &c__1);
    }
  else if (*x <= xd[1])
    {
      C2F (dcopy) (n, &yd[yd_dim1 + 1], nc, &y[1], &c__1);
    }
  else
    {
      /*    find x interval 
       */
      i__1 = *nc;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  if (*x < xd[i__])
	    {
	      inter = i__ - 1;
	      goto L20;
	    }
	  /* L10: */
	}
    L20:
      /* 
       *    compute interpolated y 
       * 
       */
      if (xd[inter + 1] == xd[inter])
	{
	  C2F (dcopy) (n, &yd[inter + yd_dim1], nc, &y[1], &c__1);
	}
      else
	{
	  i__1 = *n;
	  for (i__ = 1; i__ <= i__1; ++i__)
	    {
	      y[i__] =
		yd[inter + i__ * yd_dim1] + (*x -
					     xd[inter]) *
		((yd[inter + 1 + i__ * yd_dim1] -
		  yd[inter + i__ * yd_dim1]) / (xd[inter + 1] - xd[inter]));
	      /* L40: */
	    }
	}
    }
  return 0;
}				/* intp_ */
