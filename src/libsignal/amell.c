#include "signal.h"

/* Scilab ( http://www.scilab.org/ ) - This file is part of Scilab 
 * Copyright (C) INRIA 
 */

/*
 *  Jacobi's elliptic function am(u,k) 
 * 
 *  dsn2[i]=am(u[i],k) for i=1:n 
 */

static double d_mod (double x, double y);

void signal_amell (const double *u, double dk, double *am, int n)
{
  const double de = 1.;
  const double dz = 2.;
  const double domi = nsp_dlamch ("p") * 2.;
  const double dpi = atan (1.) * 4.;
  const double ddk = signal_compel (dk);
  const double dkprim = signal_compel (sqrt (1. - dk * dk));
  
  double dc, dh, dm, dq, dq1, dq2, ui, dqq, dpi2, dsn22;
  int i, j;
  
  for (i = 0; i < n; ++i)
    {
      int neg = FALSE;
      ui = u[i];
      if (ui < 0.)
	{
	  neg = TRUE;
	  ui = -ui;
	}
      ui = d_mod (ui,ddk * 4.);
      dq = exp (-dpi * dkprim / ddk);
      dpi2 = dpi / dz;
      if (Abs (dq) >= de)
	{
	  goto L30;
	}
      dm = dpi2 * ui / ddk;
      dc = dz * dm;
      dc = cos (dc);

      dm = sin (dm) * ddk / dpi2;
      dqq = dq * dq;
      dq1 = dq;
      dq2 = dqq;
      
      for (j = 1; j <= 100; ++j)
	{
	  dh = (de - dq1) / (de - dq2);
	  dh *= dh;
	  dh *= de - dz * dq2 * dc + dq2 * dq2;
	  dh /= de - dz * dq1 * dc + dq1 * dq1;
	  dm *= dh;

	  dh = Abs(de - dh);
	  if (dh < domi)
	    {
	      goto L20;
	    }
	  dq1 *= dqq;
	  dq2 *= dqq;
	}
      goto L30;
      
    L20:
      if (dm < -1.) dm = -1.;
      if (dm > 1.)  dm = 1.;
      dsn22 = asin (dm);
      if (dsn22 < 0.)
	{
	  dsn22 += dpi * 2.;
	}
      if (ui >= ddk && ui <= ddk * 2.)
	{
	  dsn22 = dpi - dsn22;
	}
      if (ui >= ddk * 2. && ui <= ddk * 3.)
	{
	  dsn22 = dpi * 3. - dsn22;
	}
      am[i] = (neg) ? -dsn22: dsn22;
      continue;

    L30:
      dsn22 = 0.;
      am[i] = (neg) ? -dsn22: dsn22;
      continue;

    }
}

static double d_mod (double x, double y)
{
  double quotient;
  if ((quotient = x / y) >= 0)
    quotient = floor (quotient);
  else
    quotient = -floor (-quotient);
  return (x - y * quotient);
}
