/* xgetua.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/* Table of constant values */

static int c__5 = 5;
static int c__0 = 0;
static int c_false = FALSE;

/*DECK XGETUA 
 */
int nsp_calpack_xgetua (int *iunita, int *n)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__, index;

  /****BEGIN PROLOGUE  XGETUA 
   ****PURPOSE  Return unit number(s) to which error messages are being 
   *           sent. 
   ****LIBRARY   SLATEC (XERROR) 
   ****CATEGORY  R3C 
   ****TYPE      ALL (XGETUA-A) 
   ****KEYWORDS  ERROR, XERROR 
   ****AUTHOR  Jones, R. E., (SNLA) 
   ****DESCRIPTION 
   * 
   *    Abstract 
   *       XGETUA may be called to determine the unit number or numbers 
   *       to which error messages are being sent. 
   *       These unit numbers may have been set by a call to XSETUN, 
   *       or a call to XSETUA, or may be a default value. 
   * 
   *    Description of Parameters 
   *     --Output-- 
   *       IUNIT - an array of one to five unit numbers, depending 
   *               on the value of N.  A value of zero refers to the 
   *               default unit, as defined by the I1MACH machine 
   *               constant routine.  Only IUNIT(1),...,IUNIT(N) are 
   *               defined by XGETUA.  The values of IUNIT(N+1),..., 
   *               IUNIT(5) are not defined (for N .LT. 5) or altered 
   *               in any way by XGETUA. 
   *       N     - the number of units to which copies of the 
   *               error messages are being sent.  N will be in the 
   *               range from 1 to 5. 
   * 
   ****REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC 
   *                Error-handling Package, SAND82-0800, Sandia 
   *                Laboratories, 1982. 
   ****ROUTINES CALLED  J4SAVE 
   ****REVISION HISTORY  (YYMMDD) 
   *  790801  DATE WRITTEN 
   *  861211  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  920501  Reformatted the REFERENCES section.  (WRB) 
   ****END PROLOGUE  XGETUA 
   ****FIRST EXECUTABLE STATEMENT  XGETUA 
   */
  /* Parameter adjustments */
  --iunita;

  /* Function Body */
  *n = nsp_calpack_j4save (&c__5, &c__0, &c_false);
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      index = i__ + 4;
      if (i__ == 1)
	{
	  index = 3;
	}
      iunita[i__] = nsp_calpack_j4save (&index, &c__0, &c_false);
      /* L30: */
    }
  return 0;
}				/* xgetua_ */
