#include <math.h> 

/*
 * hand conversion of urand.f (scilab) to C language. 
 * This routines could be replaced by grand. 
 */

/**
 * nsp_urand:
 * @iy: an int pointer which value it points to is not to be changed between calls.
 * 
 *  urand is a uniform random number generator based  on  theory  and 
 *  suggestions  given  in  d.e. knuth (1969),  vol  2.   the integer  iy 
 *  should be initialized to an arbitrary integer prior to the first call 
 *  to urand.  the calling program should  not  alter  the  value  of  iy 
 *  between  subsequent calls to urand.  values of urand will be returned 
 *  in the interval (0,1). 
 * 
 *
 * Return value: a double 
 **/


#define ININT(x) ((x)>=0 ? floor(x + .5) : -floor(.5 - x))

double nsp_urand(int *iy)
{
  static int m2 = 0;
  static int itwo = 2;
  static double s, halfm;
  static int ia, ic, mic;

  double ret_val, z;
  int m;

  if ( m2 == 0) 
    {
      /*  if first entry, compute machine int word length */
      m = 1;
      while (1) 
	{
	  m2 = m;
	  m = itwo * m2;
	  if (m <= m2) break;
	}
      halfm = (double) m2;
  
      /*  compute multiplier and increment for linear congruential method */
      z = halfm * atan(1.0) / 8.;
      ia = (((int) ININT(z)) << 3) + 5;
      z = halfm * (.5 - sqrt(3.) / 6.);
      ic = (((int) ININT(z)) << 1) + 1;
      mic = m2 - ic + m2;

      /*  s is the scale factor for converting to floating point */

      s = .5 / halfm;

      /*  compute next random number */
    }

  *iy *= ia;
  /*  the following statement is for computers which do not allow */
  /*  int overflow on addition */
  if (*iy > mic) {
    *iy = *iy - m2 - m2;
  }
  *iy += ic;

  /*  the following statement is for computers where the */
  /*  word length for addition is greater than for multiplication */

  if (*iy / 2 > m2) {
    *iy = *iy - m2 - m2;
  }
  /*  the following statement is for computers where int */
  /*  overflow affects the sign bit */
  if (*iy < 0) {
    *iy = *iy + m2 + m2;
  }
  ret_val = (double) (*iy) * s;
  return ret_val;
} 


