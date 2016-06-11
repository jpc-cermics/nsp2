/* zuchk.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*DECK ZUCHK 
 */
int
nsp_calpack_zuchk (double *yr, double *yi, int *nz, double *ascle,
		   double *tol)
{
  double wi, ss, st, wr;

  /****BEGIN PROLOGUE  ZUCHK 
   ****SUBSIDIARY 
   ****PURPOSE  Subsidiary to SERI, ZUOIK, ZUNK1, ZUNK2, ZUNI1, ZUNI2 and 
   *           ZKSCL 
   ****LIBRARY   SLATEC 
   ****TYPE      ALL (CUCHK-A, ZUCHK-A) 
   ****AUTHOR  Amos, D. E., (SNL) 
   ****DESCRIPTION 
   * 
   *     Y ENTERS AS A SCALED QUANTITY WHOSE MAGNITUDE IS GREATER THAN 
   *     EXP(-ALIM)=ASCLE=1.0E+3*D1MACH(1)/TOL. THE TEST IS MADE TO SEE 
   *     IF THE MAGNITUDE OF THE REAL OR IMAGINARY PART WOULD UNDERFLOW 
   *     WHEN Y IS SCALED (BY TOL) TO ITS PROPER VALUE. Y IS ACCEPTED 
   *     IF THE UNDERFLOW IS AT LEAST ONE PRECISION BELOW THE MAGNITUDE 
   *     OF THE LARGEST COMPONENT; OTHERWISE THE PHASE ANGLE DOES NOT HAVE 
   *     ABSOLUTE ACCURACY AND AN UNDERFLOW IS ASSUMED. 
   * 
   ****SEE ALSO  SERI, ZKSCL, ZUNI1, ZUNI2, ZUNK1, ZUNK2, ZUOIK 
   ****ROUTINES CALLED  (NONE) 
   ****REVISION HISTORY  (YYMMDD) 
   *  ??????  DATE WRITTEN 
   *  910415  Prologue converted to Version 4.0 format.  (BAB) 
   ****END PROLOGUE  ZUCHK 
   * 
   *    COMPLEX Y 
   ****FIRST EXECUTABLE STATEMENT  ZUCHK 
   */
  *nz = 0;
  wr = Abs (*yr);
  wi = Abs (*yi);
  st = Min (wr, wi);
  if (st > *ascle)
    {
      return 0;
    }
  ss = Max (wr, wi);
  st /= *tol;
  if (ss < st)
    {
      *nz = 1;
    }
  return 0;
}				/* zuchk_ */
