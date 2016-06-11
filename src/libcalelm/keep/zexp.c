/* zexp.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*DECK ZEXP 
 */
int nsp_calpack_zexp (double *ar, double *ai, double *br, double *bi)
{
  /* Local variables */
  double ca, cb, zm;

  /****BEGIN PROLOGUE  ZEXP 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and 
   *           ZBIRY 
   ****LIBRARY   SLATEC 
   ****TYPE      ALL (ZEXP-A) 
   ****AUTHOR  Amos, D. E., (SNL) 
   ****DESCRIPTION 
   * 
   *    DOUBLE PRECISION COMPLEX EXPONENTIAL FUNCTION B=EXP(A) 
   * 
   ****SEE ALSO  ZAIRY, ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZBIRY 
   ****ROUTINES CALLED  (NONE) 
   ****REVISION HISTORY  (YYMMDD) 
   *  830501  DATE WRITTEN 
   *  910415  Prologue converted to Version 4.0 format.  (BAB) 
   ****END PROLOGUE  ZEXP 
   ****FIRST EXECUTABLE STATEMENT  ZEXP 
   */
  zm = exp (*ar);
  ca = zm * cos (*ai);
  cb = zm * sin (*ai);
  *br = ca;
  *bi = cb;
  return 0;
}				/* zexp_ */
