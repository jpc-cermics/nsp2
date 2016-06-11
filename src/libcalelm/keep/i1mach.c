/* i1mach.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

int nsp_calpack_i1mach (int *i__)
{
  /* Initialized data */

  static int imach[16] =
    { 5, 6, 0, 6, 32, 4, 2, 31, 2147483647, 2, 0, 0, 0, 0, 0, 0 };

  /* System generated locals */
  int ret_val;

  /* Local variables */

  /*    Int machine dependent constants 
   * I/O unit numbers. 
   *   I1MACH( 1) = the standard input unit. 
   *   I1MACH( 2) = the standard output unit. 
   *   I1MACH( 3) = the standard punch unit. 
   *   I1MACH( 4) = the standard error message unit. 
   * 
   * Words. 
   *   I1MACH( 5) = the number of bits per int storage unit. 
   *   I1MACH( 6) = the number of characters per int storage unit. 
   * 
   * Ints. 
   *   assume ints are represented in the S-digit, base-A form 
   * 
   *              sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) ) 
   * 
   *              where 0 .LE. X(I) .LT. A for I=0,...,S-1. 
   *   I1MACH( 7) = A, the base. 
   *   I1MACH( 8) = S, the number of base-A digits. 
   *   I1MACH( 9) = A**S - 1, the largest magnitude. 
   * 
   * Floating-Point Numbers. 
   *   Assume floating-point numbers are represented in the T-digit, 
   *   base-B form 
   *              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) ) 
   * 
   *              where 0 .LE. X(I) .LT. B for I=1,...,T, 
   *              0 .LT. X(1), and EMIN .LE. E .LE. EMAX. 
   *   I1MACH(10) = B, the base. 
   * 
   * Single-Precision 
   *   I1MACH(11) = T, the number of base-B digits. 
   *   I1MACH(12) = EMIN, the smallest exponent E. 
   *   I1MACH(13) = EMAX, the largest exponent E. 
   * 
   * Double-Precision 
   *   I1MACH(14) = T, the number of base-B digits. 
   *   I1MACH(15) = EMIN, the smallest exponent E. 
   *   I1MACH(16) = EMAX, the largest exponent E. 
   * 
   * Reference:  Fox P.A., Hall A.D., Schryer N.L.,"Framework for a 
   *             Portable Library", ACM Transactions on Mathematical 
   *             Software, Vol. 4, no. 2, June 1978, PP. 177-188. 
   * 
   */
  /* 
   *    Get double precision values from DLAMCH 
   */
  if (imach[15] == 0)
    {
      imach[13] = (int) C2F (dlamch) ("N", 1L);
      imach[14] = (int) C2F (dlamch) ("M", 1L);
      imach[15] = (int) C2F (dlamch) ("L", 1L);
    }
  /* 
   */
  ret_val = imach[*i__ - 1];
  return ret_val;
}				/* i1mach_ */
