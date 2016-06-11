/* d1mach.f -- translated by f2c (version 19961017).
 *
 *
 */

#if 0
#include "calpack.h"

double nsp_calpack_d1mach (int *i__)
{
  /* System generated locals */
  double ret_val, d__1;

  /* Builtin functions */
  double d_lg10 (double *);

  /* Local variables */

  /* 
   * Double-precision machine constants. 
   * This implementation for use in MATLAB Fortran Mex-files. 
   * 
   * D1MACH(1) = realmin = B**(EMIN-1), the smallest positive magnitude. 
   * D1MACH(2) = realmax = B**EMAX*(1 - B**(-T)), the largest magnitude. 
   * D1MACH(3) = eps/2 = B**(-T), the smallest relative spacing. 
   * D1MACH(4) = eps = B**(1-T), the largest relative spacing. 
   * D1MACH(5) = LOG10(B) 
   * 
   * DLAMCH 
   *         = 'E' or 'e',   DLAMCH := eps 
   *         = 'S' or 's ,   DLAMCH := sfmin 
   *         = 'B' or 'b',   DLAMCH := base 
   *         = 'P' or 'p',   DLAMCH := eps*base 
   *         = 'N' or 'n',   DLAMCH := t 
   *         = 'R' or 'r',   DLAMCH := rnd 
   *         = 'M' or 'm',   DLAMCH := emin 
   *         = 'U' or 'u',   DLAMCH := rmin 
   *         = 'L' or 'l',   DLAMCH := emax 
   *         = 'O' or 'o',   DLAMCH := rmax 
   * 
   *         where 
   * 
   *         eps   = relative machine precision 
   *         sfmin = safe minimum, such that 1/sfmin does not overflow 
   *         base  = base of the machine 
   *         prec  = eps*base 
   *         t     = number of (base) digits in the mantissa 
   *         rnd   = 1.0 when rounding occurs in addition, 0.0 otherwise 
   *         emin  = minimum exponent before (gradual) underflow 
   *         rmin  = underflow threshold - base**(emin-1) 
   *         emax  = largest exponent before overflow 
   *         rmax  = overflow threshold  - (base**emax)*(1-eps) 
   * 
   */
  if (*i__ == 1)
    {
      ret_val = C2F (dlamch) ("U", 1L);
    }
  if (*i__ == 2)
    {
      ret_val = C2F (dlamch) ("O", 1L);
    }
  if (*i__ == 3)
    {
      ret_val = C2F (dlamch) ("E", 1L);
    }
  if (*i__ == 4)
    {
      ret_val = C2F (dlamch) ("P", 1L);
    }
  if (*i__ == 5)
    {
      d__1 = C2F (dlamch) ("B", 1L);
      ret_val = d_lg10 (&d__1);
    }
  return ret_val;
}				/* d1mach_ */
#endif
