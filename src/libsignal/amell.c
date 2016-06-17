#include "signal.h"

/* Scilab ( http://www.scilab.org/ ) - This file is part of Scilab 
 * Copyright (C) INRIA 
 */

/*
 *calculation of the jacobi's elliptic function am(u,k) 
 * 
 *external calculation of the parameter necessary 
 *ddk = k($dk) 
 *dq = exp(-pi*k'/k) ... (jacobi's nome) 
 *!calling sequence 
 *    subroutine amell(du, dk, dsn2 ,n) 
 *    double precision du(n),dsn2(n),dk 
 *    int n 
 */


static double d_mod (double *x, double *y);

int
signal_amell (double *du, double *dk, double *dsn2, int *n)
{
  /* Initialized data */

  static double de = 1.;
  static double dz = 2.;

  int i__1;
  double d__1;

  /* Local variables */
  double domi, dsn22;
  int i__;
  double dc, dh, dm, dq;
  double dkprim, dq1, dq2, du1, ddk;
  int neg;
  double dpi;
  int ijk;
  double dqq, dpi2;

  /* Parameter adjustments */
  --dsn2;
  --du;

  /* Function Body */
  /* 
   */
  domi = nsp_dlamch ("p") * 2.;
  dpi = atan (1.) * 4.;
  signal_compel (dk, &ddk);
  d__1 = sqrt (1. - *dk * *dk);
  signal_compel (&d__1, &dkprim);
  i__1 = *n;
  for (ijk = 1; ijk <= i__1; ++ijk)
    {
      neg = FALSE;
      du1 = du[ijk];
      if (du1 < 0.)
	{
	  neg = TRUE;
	  du1 = -du1;
	}
      d__1 = ddk * 4.;
      du1 = d_mod (&du1, &d__1);
      dq = exp (-dpi * dkprim / ddk);
      dpi2 = dpi / dz;
      if (Abs (dq) >= de)
	{
	  goto L30;
	}
      /* 
       */
      dm = dpi2 * du1 / ddk;
      dc = dz * dm;
      dc = cos (dc);
      /* 
       */
      dm = sin (dm) * ddk / dpi2;
      dqq = dq * dq;
      dq1 = dq;
      dq2 = dqq;
      /* 
       */
      for (i__ = 1; i__ <= 100; ++i__)
	{
	  dh = (de - dq1) / (de - dq2);
	  dh *= dh;
	  dh *= de - dz * dq2 * dc + dq2 * dq2;
	  dh /= de - dz * dq1 * dc + dq1 * dq1;
	  dm *= dh;
	  /* 
	   */
	  dh = (d__1 = de - dh, Abs (d__1));
	  if (dh < domi)
	    {
	      goto L20;
	    }
	  /* 
	   */
	  dq1 *= dqq;
	  dq2 *= dqq;
	  /* L10: */
	}
      /* 
       */
      goto L30;
      /* 
       */
    L20:
      if (dm < -1.)
	{
	  dm = -1.;
	}
      if (dm > 1.)
	{
	  dm = 1.;
	}
      dsn22 = asin (dm);
      if (dsn22 < 0.)
	{
	  dsn22 += dpi * 2.;
	}
      if (du1 >= ddk && du1 <= ddk * 2.)
	{
	  dsn22 = dpi - dsn22;
	}
      if (du1 >= ddk * 2. && du1 <= ddk * 3.)
	{
	  dsn22 = dpi * 3. - dsn22;
	}
      goto L2;
      /* 
       */
    L30:
      dsn22 = 0.;
    L2:
      if (neg)
	{
	  dsn22 = -dsn22;
	}
      dsn2[ijk] = dsn22;
      /* L1: */
    }
  return 0;
}				/* amell_ */

static double
d_mod (double *x, double *y)
{
  double quotient;
  if ((quotient = *x / *y) >= 0)
    quotient = floor (quotient);
  else
    quotient = -floor (-quotient);
  return (*x - (*y) * quotient);
}
