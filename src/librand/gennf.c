#include <math.h>
#include <nsp/machine.h> 

#include "grand.h" 

/* ********************************************************************** */
/*     DOUBLE PRECISION FUNCTION GENNF( DFN, DFD, XNONC ) */
/*           GENerate random deviate from the Noncentral F distribution */
/*                              Function */
/*     Generates a random deviate from the  noncentral F (variance ratio) */
/*     distribution with DFN degrees of freedom in the numerator, and DFD */
/*     degrees of freedom in the denominator, and noncentrality parameter */
/*     XNONC. */
/*                              Arguments */
/*     DFN --> Numerator degrees of freedom */
/*             (Must be >= 1.0) */
/*                              DOUBLE PRECISION DFN */
/*      DFD --> Denominator degrees of freedom */
/*             (Must be positive) */
/*                              DOUBLE PRECISION DFD */
/*     XNONC --> Noncentrality parameter */
/*               (Must be nonnegative) */
/*                              DOUBLE PRECISION XNONC */
/*                              Method */
/*     Directly generates ratio of noncentral numerator chisquare variate */
/*     to central denominator chisquare variate. */


double rand_gennf(double *dfn, double *dfd, double *xnonc)
{
  double  d1, d2;
  double xden, xnum;
  /*     JJV changed the code to call SGAMMA and SNORM directly */
  /*     JJV changed the argument checker to allow DFN = 1.0 */
  /*     JJV in the same way as GENNCH was changed. */
  /*     JJV changed this to call SGAMMA and SNORM directly */
  if (*dfn >= 1.000001f) 
    {
      d1 = (*dfn - 1.f) / 2.f;
      /* Computing 2nd power */
      d2 = rand_snorm() + sqrt(*xnonc);
      xnum = (rand_sgamma(&d1) * 2.f + d2 * d2 ) / *dfn;
    }
  else 
    {
      /*     JJV case dfn = 1.0 - here I am treating dfn as exactly 1.0 */
      /* Computing 2nd power */
      d1 = rand_snorm() + sqrt(*xnonc);
      xnum = d1 * d1;
    }
    d1 = *dfd / 2.f;
    xden = rand_sgamma(&d1 ) * 2.f / *dfd;
    /*     JJV changed constant so that it will not underflow at compile time */
    /*     JJV while not slowing generator by using double precision or logs. */
    /*      IF (.NOT. (xden.LE. (1.0E-38*xnum))) GO TO 40 */
    if (! (xden <= xnum * 1e-37f)) return  xnum / xden;

    Sciprintf("Warning: F generated numbers would cause overflow returning 1.0E37");
    /*     JJV next 2 lines changed to maintain truncation of large deviates. */
    /*      genf = 1.0E38 */
    return  1e37;
} 

