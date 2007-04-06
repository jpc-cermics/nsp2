#include "cdf.h"

/**
 * cdf_gamln:
 * @a: 
 * 
 * computes ln(gamma(a)) for positive a 
 * written by alfred h. morris 
 * naval surface warfare center dahlgren, virginia
 * 
 * Returns: a double 
 **/

double cdf_gamln (double a)
{
  /*test: a=0.01:0.01:14; norm(log(gamma(a))- cdf_gamln(a)) */
  /*     D = 0.5*(LN(2*PI) - 1) */
  const double d = .418938533204673;
  const double c0 = .0833333333333333;
  const double c1 = -.00277777777760991;
  const double c2 = 7.9365066682539e-4;
  const double c3 = -5.9520293135187e-4;
  const double c4 = 8.37308034031215e-4;
  const double c5 = -.00165322962780713;
  int i1,  i, n;
  double d1,  t, w;
  
  if ( a <= 0.8 ) 
    {
      return cdf_gamln1 (a) - log (a);
    }
  else if ( a <= 2.25 ) 
    {
      t = a - .5 - .5;
      return cdf_gamln1 (t);
    }
  else if (a >= 10.)
    {
      /* Computing 2nd power */
      d1 = 1. / a;
      t = d1 * d1;
      w = (((((c5 * t + c4) * t + c3) * t + c2) * t + c1) * t + c0) / a;
      return  d + w + (a - .5) * (log (a) - 1.);
    }
  else
    {
      n = (int) (a - 1.25);
      t = a;
      w = 1.;
      i1 = n;
      for (i = 1; i <= i1; ++i)
	{
	  t += -1.;
	  w = t * w;
	}
      d1 = t - 1.;
      return cdf_gamln1 (d1) + log (w);
    }
}	

