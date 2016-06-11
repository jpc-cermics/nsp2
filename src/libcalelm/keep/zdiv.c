/* zdiv.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*DECK ZDIV 
 */
int
nsp_calpack_zdiv (double *ar, double *ai, double *br, double *bi, double *cr,
		  double *ci)
{
  double ca, cb, cc, cd, bm;

  /****BEGIN PROLOGUE  ZDIV 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZAIRY and 
   *           ZBIRY 
   ****LIBRARY   SLATEC 
   ****TYPE      ALL (ZDIV-A) 
   ****AUTHOR  Amos, D. E., (SNL) 
   ****DESCRIPTION 
   * 
   *    DOUBLE PRECISION COMPLEX DIVIDE C=A/B. 
   * 
   ****SEE ALSO  ZAIRY, ZBESH, ZBESI, ZBESJ, ZBESK, ZBESY, ZBIRY 
   ****ROUTINES CALLED  ZABS 
   ****REVISION HISTORY  (YYMMDD) 
   *  830501  DATE WRITTEN 
   *  910415  Prologue converted to Version 4.0 format.  (BAB) 
   ****END PROLOGUE  ZDIV 
   ****FIRST EXECUTABLE STATEMENT  ZDIV 
   */
  bm = 1. / nsp_calpack_zabs (br, bi);
  cc = *br * bm;
  cd = *bi * bm;
  ca = (*ar * cc + *ai * cd) * bm;
  cb = (*ai * cc - *ar * cd) * bm;
  *cr = ca;
  *ci = cb;
  return 0;
}				/* zdiv_ */
