#include "cdf.h"



/**
 * cdf_exparg:
 * @l: an integer 
 * 
 * 
 * if l = 0 then  exparg(l) is the largest positive w for which exp(w) can be computed. 
 * if l is nonzero then  exparg(l) is the largest negative w for 
 *     which the computed value of exp(w) is nonzero. 
 * note... only an approximate value for exparg(l) is needed. 
 * 
 * Returns: 
 **/

double cdf_exparg (const int l)
{
  const int c__4 = 4;
  const int c__9 = 9;
  const int c__10 = 10;
  int b, m;
  double lnb;
  b = cdf_ipmpar (c__4);
  switch (b ) 
    {
    case 2: 
      lnb = .69314718055995;
      break;
    case 8:
      lnb = 2.0794415416798;
      break;
    case 16 :
      lnb = 2.7725887222398;
      break;
    default: 
      lnb = log ((double) b);
    }
  if (l == 0)
    {
      m = cdf_ipmpar (c__10);
      return m * lnb * .99999;
    }
  m = cdf_ipmpar (c__9) - 1;
  return  m * lnb * .99999;
}
  
