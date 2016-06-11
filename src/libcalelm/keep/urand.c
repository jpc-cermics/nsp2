/* urand.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/*/MEMBR ADD NAME=URAND,SSI=0 
 */
double nsp_calpack_urand (int *iy)
{
  /* Initialized data */

  static int m2 = 0;
  static int itwo = 2;

  /* System generated locals */
  double ret_val, d__1;

  /* Local variables */
  static int m;
  static double s, halfm;
  static int ia, ic, mic;

  /*!purpose 
   * 
   *     urand is a uniform random number generator based  on  theory  and 
   * suggestions  given  in  d.e. knuth (1969),  vol  2.   the int  iy 
   * should be initialized to an arbitrary int prior to the first call 
   * to urand.  the calling program should  not  alter  the  value  of  iy 
   * between  subsequent calls to urand.  values of urand will be returned 
   * in the interval (0,1). 
   * 
   *!calling sequence 
   *    double precision function urand(iy) 
   *    int iy 
   *! 
   *c symbolics version 
   *     double precision function urand(iy) 
   *     int iy 
   *     lispfunction random 'cl-user::random' (int) int 
   *     urand=dble(real(random(2**31)))/(dble(real(2**31))-1.0d+0) 
   *     return 
   *     end 
   *c end 
   * 
   */
  if (m2 != 0)
    {
      goto L20;
    }
  /* 
   * if first entry, compute machine int word length 
   * 
   */
  m = 1;
L10:
  m2 = m;
  m = itwo * m2;
  if (m > m2)
    {
      goto L10;
    }
  halfm = (double) m2;
  /* 
   * compute multiplier and increment for linear congruential method 
   * 
   */
  d__1 = halfm * atan (1.) / 8.;
  ia = (i_dnnt (&d__1) << 3) + 5;
  d__1 = halfm * (.5 - sqrt (3.) / 6.);
  ic = (i_dnnt (&d__1) << 1) + 1;
  mic = m2 - ic + m2;
  /* 
   * s is the scale factor for converting to floating point 
   * 
   */
  s = .5 / halfm;
  /* 
   * compute next random number 
   * 
   */
L20:
  *iy *= ia;
  /* 
   * the following statement is for computers which do not allow 
   * int overflow on addition 
   * 
   */
  if (*iy > mic)
    {
      *iy = *iy - m2 - m2;
    }
  /* 
   */
  *iy += ic;
  /* 
   * the following statement is for computers where the 
   * word length for addition is greater than for multiplication 
   * 
   */
  if (*iy / 2 > m2)
    {
      *iy = *iy - m2 - m2;
    }
  /* 
   * the following statement is for computers where int 
   * overflow affects the sign bit 
   * 
   */
  if (*iy < 0)
    {
      *iy = *iy + m2 + m2;
    }
  ret_val = (double) (*iy) * s;
  return ret_val;
}				/* urand_ */
#endif
