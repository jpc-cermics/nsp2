#include "signal.h"

/*
 * function:     dsn2 
 * calculation of the jacobi's elliptic function sn(u,k) 
 * 
 * external calculation of the parameter necessary 
 * dk = k($k) 
 * dq = exp(-pi*k'/k) ... (jacobi's nome) 
 */


double signal_dsn2 (const double *du,const double *dk,const double *dq)
{
  int i;
  double d1, dc, dh, dm, dq1, dq2, dqq;

  const double de = 1., dz = 2., dpi = atan (1.) * 4.;
  const double domi = nsp_dlamch ("p") * 2.;
  const double dpi2 = dpi / dz;
  if (Abs (*dq) >= de)
    {
      return 0;
    }
  dm = dpi2 * *du / *dk;
  dc = dz * dm;
  dc = cos (dc);
  dm = sin (dm) * *dk / dpi2;
  dqq = *dq * *dq;
  dq1 = *dq;
  dq2 = dqq;
  for (i = 1; i <= 100; ++i)
    {
      dh = (de - dq1) / (de - dq2);
      dh *= dh;
      dh *= de - dz * dq2 * dc + dq2 * dq2;
      dh /= de - dz * dq1 * dc + dq1 * dq1;
      dm *= dh;
      dh = (d1 = de - dh, Abs (d1));
      if (dh < domi)
	{
	  return dm;
	}
      dq1 *= dqq;
      dq2 *= dqq;
    }
  return 0;
}

