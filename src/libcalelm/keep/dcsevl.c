/* dcsevl.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__4 = 4;
static int c__2 = 2;
static int c__3 = 3;
static int c__1 = 1;

/*DECK DCSEVL 
 */
double nsp_calpack_dcsevl (double *x, double *cs, int *n)
{
  /* Initialized data */

  static int first = TRUE;

  /* System generated locals */
  int i__1;
  double ret_val;

  /* Local variables */
  double twox;
  int i__;
  static double onepl;
  double b0, b1, b2;
  int ni;

  /****BEGIN PROLOGUE  DCSEVL 
   ****PURPOSE  Evaluate a Chebyshev series. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C3A2 
   ****TYPE      DOUBLE PRECISION (CSEVL-S, DCSEVL-D) 
   ****KEYWORDS  CHEBYSHEV SERIES, FNLIB, SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   * Evaluate the N-term Chebyshev series CS at X.  Adapted from 
   * a method presented in the paper by Broucke referenced below. 
   * 
   *      Input Arguments -- 
   * X    value at which the series is to be evaluated. 
   * CS   array of N terms of a Chebyshev series.  In evaluating 
   *      CS, only half the first coefficient is summed. 
   * N    number of terms in array CS. 
   * 
   ****REFERENCES  R. Broucke, Ten subroutines for the manipulation of 
   *                Chebyshev series, Algorithm 446, Communications of 
   *                the A.C.M. 16, (1973) pp. 254-256. 
   *              L. Fox and I. B. Parker, Chebyshev Polynomials in 
   *                Numerical Analysis, Oxford University Press, 1968, 
   *                page 56. 
   ****ROUTINES CALLED  D1MACH, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770401  DATE WRITTEN 
   *  890831  Modified array declarations.  (WRB) 
   *  890831  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  900329  Prologued revised extensively and code rewritten to allow 
   *          X to be slightly outside interval (-1,+1).  (WRB) 
   *  920501  Reformatted the REFERENCES section.  (WRB) 
   ****END PROLOGUE  DCSEVL 
   */
  /* Parameter adjustments */
  --cs;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DCSEVL 
   */
  if (first)
    {
      onepl = nsp_calpack_d1mach (&c__4) + 1.;
    }
  first = FALSE;
  if (*n < 1)
    {
      nsp_calpack_xermsg ("SLATEC", "DCSEVL", "NUMBER OF TERMS .LE. 0",
			  &c__2, &c__2, 6L, 6L, 22L);
    }
  if (*n > 1000)
    {
      nsp_calpack_xermsg ("SLATEC", "DCSEVL", "NUMBER OF TERMS .GT. 1000",
			  &c__3, &c__2, 6L, 6L, 25L);
    }
  if (Abs (*x) > onepl)
    {
      nsp_calpack_xermsg ("SLATEC", "DCSEVL",
			  "X OUTSIDE THE INTERVAL (-1,+1)", &c__1, &c__1, 6L,
			  6L, 30L);
    }
  /* 
   */
  b1 = 0.;
  b0 = 0.;
  twox = *x * 2.;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      b2 = b1;
      b1 = b0;
      ni = *n + 1 - i__;
      b0 = twox * b1 - b2 + cs[ni];
      /* L10: */
    }
  /* 
   */
  ret_val = (b0 - b2) * .5;
  /* 
   */
  return ret_val;
}				/* dcsevl_ */
#endif
