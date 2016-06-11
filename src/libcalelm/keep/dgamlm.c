/* dgamlm.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__2 = 2;

/*DECK DGAMLM 
 */
int nsp_calpack_dgamlm (double *xmin, double *xmax)
{
  /* System generated locals */
  double d__1, d__2;

  /* Local variables */
  double xold;
  int i__;
  double alnbig, alnsml;
  double xln;

  /****BEGIN PROLOGUE  DGAMLM 
   ****PURPOSE  Compute the minimum and maximum bounds for the argument in 
   *           the Gamma function. 
   ****LIBRARY   SLATEC (FNLIB) 
   ****CATEGORY  C7A, R2 
   ****TYPE      DOUBLE PRECISION (GAMLIM-S, DGAMLM-D) 
   ****KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, LIMITS, SPECIAL FUNCTIONS 
   ****AUTHOR  Fullerton, W., (LANL) 
   ****DESCRIPTION 
   * 
   *Calculate the minimum and maximum legal bounds for X in gamma(X). 
   *XMIN and XMAX are not the only bounds, but they are the only non- 
   *trivial ones to calculate. 
   * 
   *            Output Arguments -- 
   *XMIN   double precision minimum legal value of X in gamma(X).  Any 
   *       smaller value of X might result in underflow. 
   *XMAX   double precision maximum legal value of X in gamma(X).  Any 
   *       larger value of X might cause overflow. 
   * 
   ****REFERENCES  (NONE) 
   ****ROUTINES CALLED  D1MACH, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  770601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890531  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   ****END PROLOGUE  DGAMLM 
   ****FIRST EXECUTABLE STATEMENT  DGAMLM 
   */
  alnsml = log (nsp_calpack_d1mach (&c__1));
  *xmin = -alnsml;
  for (i__ = 1; i__ <= 10; ++i__)
    {
      xold = *xmin;
      xln = log (*xmin);
      *xmin -=
	*xmin * ((*xmin + .5) * xln - *xmin - .2258 + alnsml) / (*xmin * xln +
								 .5);
      if ((d__1 = *xmin - xold, Abs (d__1)) < .005)
	{
	  goto L20;
	}
      /* L10: */
    }
  nsp_calpack_xermsg ("SLATEC", "DGAMLM", "UNABLE TO FIND XMIN", &c__1,
		      &c__2, 6L, 6L, 19L);
  /* 
   */
L20:
  *xmin = -(*xmin) + .01;
  /* 
   */
  alnbig = log (nsp_calpack_d1mach (&c__2));
  *xmax = alnbig;
  for (i__ = 1; i__ <= 10; ++i__)
    {
      xold = *xmax;
      xln = log (*xmax);
      *xmax -=
	*xmax * ((*xmax - .5) * xln - *xmax + .9189 - alnbig) / (*xmax * xln -
								 .5);
      if ((d__1 = *xmax - xold, Abs (d__1)) < .005)
	{
	  goto L40;
	}
      /* L30: */
    }
  nsp_calpack_xermsg ("SLATEC", "DGAMLM", "UNABLE TO FIND XMAX", &c__2,
		      &c__2, 6L, 6L, 19L);
  /* 
   */
L40:
  *xmax += -.01;
  /*Computing MAX 
   */
  d__1 = *xmin, d__2 = -(*xmax) + 1.;
  *xmin = Max (d__1, d__2);
  /* 
   */
  return 0;
}				/* dgamlm_ */
#endif
