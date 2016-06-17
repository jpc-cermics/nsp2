#include "signal.h"

/*!purpose 
 *     calculates an approximate value for the elliptic integral of 
 *     the first kind, rf(x,y,z), as defined by b.c.carlson. 
 *     -special functions of applied maths-academic press(1977). 
 *     res=integral(zero to x) of 
 *           1/sqrt((1-t*t)(1-ck*ck*t*t)) 
 *     for       0<=t<=1 
 * 
 *!calling sequence 
 *    subroutine deli2(nn,resv,xxv,ck) 
 *    double precision ck, xxv(nn),resv(nn) 
 *! 
 *    .. scalar arguments .. 
 *    .. 
 *    .. local scalars .. 
 *    .. function references .. 
 */

int
signal_deli2 (const int *nn, double *resv,const double *xxv,const double *ck)
{
  /* Initialized data */

  static double acc = 8.5e-4;

  int i__1;
  double d__1, d__2;

  /* Local variables */
  double lamda, x, y, z__, lolim, c1, c2, c3, e2, e3, uplim;
  int kk;
  double mu, xn, yn, zn, rscale, xx;

  double cxn, res, cyn, czn, rtx, rty, rtz;

  /* Parameter adjustments */
  --xxv;
  --resv;

  /* Function Body */
  /* 
   *    order x,y,z into xn,yn,zn  st. xn.le.yn.le.zn 
   *     write(6,*) nn,resv(1),xxv(1),ck 
   */
  i__1 = *nn;
  for (kk = 1; kk <= i__1; ++kk)
    {
      xx = xxv[kk];
      x = 1. - xx * xx;
      y = 1. - *ck * *ck * xx * xx;
      if (x <= y)
	{
	  goto L20;
	}
      xn = y;
      yn = x;
      goto L40;
    L20:
      xn = x;
      yn = y;
      z__ = 1.;
    L40:
      zn = z__;
      if (yn <= zn)
	{
	  goto L60;
	}
      zn = yn;
      yn = z__;
      if (xn <= yn)
	{
	  goto L60;
	}
      yn = xn;
      xn = z__;
      /* 
       *    test for valid arguments 
       */
    L60:
      if (xn < 0.)
	{
	  xn = 0.;
	}
      if (yn <= 0.)
	{
	  goto L180;
	}
      /* 
       *    valid call 
       */
      rscale = 1.;
      lolim = nsp_dlamch ("u") * 16.;
      uplim = nsp_dlamch ("o") * .0625;
      /* 
       *    for extreme arguments scale to avoid under and overflows 
       */
      if (zn <= uplim)
	{
	  goto L120;
	}
      rscale = .25;
      zn *= .0625;
      if (yn <= lolim)
	{
	  goto L80;
	}
      yn *= .0625;
      if (xn <= lolim)
	{
	  goto L100;
	}
      xn *= .0625;
      goto L140;
    L80:
      lamda = (sqrt (xn) + sqrt (yn)) * (sqrt (zn) * .25);
      xn = lamda * .25;
      yn = xn;
      zn = (zn + lamda) * .25;
      goto L140;
    L100:
      rtz = sqrt (zn);
      rty = sqrt (yn);
      lamda = rtz * rty + (rtz + rty) * .25 * sqrt (xn);
      xn = lamda * .25;
      yn = (yn + lamda) * .25;
      zn = (zn + lamda) * .25;
      goto L140;
    L120:
      if (zn > lolim)
	{
	  goto L140;
	}
      rscale = 4.;
      xn *= 16.;
      yn *= 16.;
      zn *= 16.;
      /* 
       *    main recursion 
       */
    L140:
      mu = (xn + yn + zn) / 3.;
      czn = 2. - (zn + mu) / mu;
      cxn = 2. - (xn + mu) / mu;
      /*Computing MAX 
       */
      d__1 = cxn, d__2 = -czn;
      if (Max (d__1, d__2) <= acc)
	{
	  goto L160;
	}
      rtx = sqrt (xn);
      rty = sqrt (yn);
      rtz = sqrt (zn);
      lamda = rtz * (rtx + rty) + rtx * rty;
      xn = (xn + lamda) * .25;
      yn = (yn + lamda) * .25;
      zn = (zn + lamda) * .25;
      goto L140;
      /* 
       *    power series expansion 
       */
    L160:
      c1 = .041666666666666664;
      c2 = .068181818181818177;
      c3 = .071428571428571425;
      cyn = -cxn - czn;
      e2 = cxn * cyn - czn * czn;
      e3 = cxn * czn * cyn;
      res =
	rscale * ((c1 * e2 - .1 - c2 * e3) * e2 + 1. + c3 * e3) / sqrt (mu);
      res *= xx;
      goto L200;
      /* 
       *    failure exits 
       */
    L180:
      res = 0.;
    L200:
      resv[kk] = res;
      /* L201: */
    }
  return 0;
}				/* deli2_ */
