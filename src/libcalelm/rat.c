#include "calpack.h"

/*!but 
 *   Approximate a real by a rational 
 *    double precision x,eps 
 *    int n,d,fail 
 *    x: real to be approximated 
 *    eps: required accuracy (Abs(x-n/d))<eps 
 *    n,d: integers giving n/d 
 *
 * Author: Serge Steer (Inria)
 * Copyright INRIA 
 */

int nsp_calpack_rat (double x, double eps, int *n, int *d)
{
  int nmax= INT_MAX;
  double z= Abs (x), ax=z, xd, dz, xn, err;
  int d0=1, d1=0, n0=0, n1=1, bm;
  while (1)
    {
      err = Abs((d1 * ax - n1));
      if (err <= d1 * eps)
	{
	  goto stop;
	}
      if (z > (double) nmax)
	{
	  return FAIL;
	}
      bm = (int) z;
      dz = z - bm;
      if (dz == 0.)
	{
	  goto L15;
	}
      z  = 1. / dz;
    L15:
      xn = n0 + (double) bm *n1;
      xd = d0 + (double) bm *d1;
      if (xn > (double) nmax || xd > (double) nmax)
	{
	  return FAIL;
	}
      n0 = n1;
      d0 = d1;
      n1 = (int) xn;
      d1 = (int) xd;
      if (dz == 0.)
	{
	  goto stop;
	}
    }
 
 stop:
  *n = n1;
  *d = d1;
  if (x < 0.) *n = -(*n);
  return OK;
}
