/* zsqrt.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*DECK ZSQRT 
 */
int nsp_calpack_zsqrt (double *ar, double *ai, double *br, double *bi)
{
  /* Initialized data */

  static double drt = .7071067811865475244008443621;
  static double dpi = 3.141592653589793238462643383;

  /* Local variables */
  double zm, dtheta;

  /****BEGIN PROLOGUE  ZSQRT 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and 
   *           ZBIRY 
   ****LIBRARY   SLATEC 
   ****TYPE      ALL (ZSQRT-A) 
   ****AUTHOR  Amos, D. E., (SNL) 
   ****DESCRIPTION 
   * 
   *    DOUBLE PRECISION COMPLEX SQUARE ROOT, B=CSQRT(A) 
   * 
   ****SEE ALSO  ZAIRY, ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZBIRY 
   ****ROUTINES CALLED  ZABS 
   ****REVISION HISTORY  (YYMMDD) 
   *  830501  DATE WRITTEN 
   *  910415  Prologue converted to Version 4.0 format.  (BAB) 
   ****END PROLOGUE  ZSQRT 
   */
  /****FIRST EXECUTABLE STATEMENT  ZSQRT 
   */
  zm = nsp_calpack_zabs (ar, ai);
  zm = sqrt (zm);
  if (*ar == 0.)
    {
      goto L10;
    }
  if (*ai == 0.)
    {
      goto L20;
    }
  dtheta = atan (*ai / *ar);
  if (dtheta <= 0.)
    {
      goto L40;
    }
  if (*ar < 0.)
    {
      dtheta -= dpi;
    }
  goto L50;
L10:
  if (*ai > 0.)
    {
      goto L60;
    }
  if (*ai < 0.)
    {
      goto L70;
    }
  *br = 0.;
  *bi = 0.;
  return 0;
L20:
  if (*ar > 0.)
    {
      goto L30;
    }
  *br = 0.;
  *bi = sqrt ((Abs (*ar)));
  return 0;
L30:
  *br = sqrt (*ar);
  *bi = 0.;
  return 0;
L40:
  if (*ar < 0.)
    {
      dtheta += dpi;
    }
L50:
  dtheta *= .5;
  *br = zm * cos (dtheta);
  *bi = zm * sin (dtheta);
  return 0;
L60:
  *br = zm * drt;
  *bi = zm * drt;
  return 0;
L70:
  *br = zm * drt;
  *bi = -zm * drt;
  return 0;
}				/* zsqrt_ */
