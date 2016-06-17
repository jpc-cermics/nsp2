/* snell.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/*/MEMBR ADD NAME=SNELL,SSI=0 
 */
/* Subroutine */ int
signal_snell (double *dsn2, double *du, double *dk, double *dq)
{
  /* Initialized data */

  static double de = 1.;
  static double dz = 2.;

  /* System generated locals */
  double d__1;

  /* Local variables */
  double domi;
  int i__;
  double dc, dh, dm;
  double dq1, dq2, dpi, dqq, dpi2;

  /*!purpose 
   *calculation of the jacobi's elliptic function sn(u,k) 
   * 
   *external calculation of the parameter necessary 
   *dk = k($k) 
   *dq = exp(-pi*k'/k) ... (jacobi's nome) 
   *! 
   * 
   * 
   */
  domi = nsp_dlamch ("p") * 2.;
  dpi = atan (1.) * 4.;
  /* 
   * 
   */
  dpi2 = dpi / dz;
  if (Abs (*dq) >= de)
    {
      goto L30;
    }
  /* 
   */
  dm = dpi2 * *du / *dk;
  dc = dz * dm;
  dc = cos (dc);
  /* 
   */
  dm = sin (dm) * *dk / dpi2;
  dqq = *dq * *dq;
  dq1 = *dq;
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
  *dsn2 = dm;
  return 0;
  /* 
   */
 L30:
  *dsn2 = 0.;
  return 0;
}				/* snell_ */
