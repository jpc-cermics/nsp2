#include "signal.h"

/*
 *computation of the parameters of the normalized lowpass 
 *  Nomenclature Rabiner-Gold (page 241) 
 *  Si ityp = < 2 
 *  vsn=1/k 
 */

int signal_transn (int *ityp, double *om, int *norma,
		   double *vsn, double *vd, double *a)
{
  int j;
  double q, a1 = 0, v1, v2, v3, v4;
  double vdq = 0, vdq1 = 0, vsn1 =0 ;

  --om;

  v1 = sin (om[1] / 2.) / cos (om[1] / 2.);
  v2 = sin (om[2] / 2.) / cos (om[2] / 2.);
  if (*ityp <= 2)
    {
      goto L210;
    }
  v3 = sin (om[3] / 2.) / cos (om[3] / 2.);
  v4 = sin (om[4] / 2.) / cos (om[4] / 2.);
  if (*ityp == 3)
    {
      goto L10;
    }
  q = v1;
  v1 = -v4;
  v4 = -q;
  q = v2;
  v2 = -v3;
  v3 = -q;
  /* 
   */
 L10:
  j = *norma + 1;
  switch (j)
    {
    case 1:
      goto L30;
    case 2:
      goto L30;
    case 3:
      goto L40;
    case 4:
      goto L70;
    }
 L30:
  vdq1 = v2 * v3;
  vsn1 = vdq1 / v1 - v1;
  q = v4 - vdq1 / v4;
  if (q < vsn1)
    {
      vsn1 = q;
    }
  a1 = 1. / (v3 - v2);
  vsn1 *= a1;
  switch (j)
    {
    case 1:
      goto L40;
    case 2:
      goto L50;
    case 3:
      goto L40;
    }
 L40:
  vdq = v1 * v4;
  *a = v2 / (vdq - v2 * v2);
  q = v3 / (v3 * v3 - vdq);
  if (q < *a)
    {
      *a = q;
    }
  *vsn = *a * (v4 - v1);
  if (*norma == 2)
    {
      goto L200;
    }
  if (*vsn >= vsn1)
    {
      goto L200;
    }
 L50:
  vdq = vdq1;
 L60:
  *vsn = vsn1;
  *a = a1;
  goto L200;
  /* 
   */
 L70:
  vdq = sqrt (v1 * v2 * v3 * v4);
  a1 = v3 / (v3 * v3 - vdq);
  vsn1 = (v4 - vdq / v4) * a1;
  *a = v2 / (vdq - v2 * v2);
  *vsn = (vdq / v1 - v1) * *a;
  if (*vsn >= vsn1)
    {
      goto L200;
    }
  goto L60;
  /* 
   *      BUG:       NO PATH TO HERE !!!!! 
   *ccp      vdq = v2*v3 
   *ccp      vsn = v4 - vdq/v4 
   * 
   *ccp      a = 1.0d+0/(v3-v2) 
   *ccp      vsn = vsn*a 
   * 
   */
 L200:
  *vd = sqrt (vdq);
  *a *= *vd;
  if (*ityp <= 3)
    {
      goto L270;
    }
  *a /= *vsn;
  goto L270;
  /* 
   */
 L210:
  j = *ityp;
  switch (j)
    {
    case 1:
      goto L220;
    case 2:
      goto L220;
    case 3:
      goto L230;
    case 4:
      goto L240;
    case 5:
      goto L250;
    case 6:
      goto L260;
    }
 L220:
  *vsn = v2 / v1;
  switch (j)
    {
    case 1:
      goto L250;
    case 2:
      goto L240;
    }
 L230:
  *vd = v2 / *vsn;
  goto L270;
 L240:
  *vd = v2;
  goto L270;
 L250:
  *vd = v1;
  goto L270;
 L260:
  *vd = v1 * *vsn;
  /* 
   */
 L270:
  return 0;
}	
