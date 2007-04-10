#include "cdf.h"



/**
 * cdf_dlngam:
 * @a: value at which scaled log gamma is to be returned 
 * 
 * returns the natural logarithm of gamma(@a)  for positive @a.
 * renames gamln from: 
 *     Didinato, A. R. and Morris, A.H.  algorithm 708: significant 
 *     digit computation of the incomplete  beta  function ratios.  acm 
 *     trans. math.  softw. 18 (1993), 360-373. 
 * 
 * written by A.H. Morris, naval surface warfare center, 
 * Dahlgren, virginia 
 * 
 * Returns: a double 
 **/


double cdf_dlngam (double a)
{
  /*     d = 0.5*(ln(2*pi) - 1) */
  const double d = .418938533204673;
  const double c0 = .0833333333333333;
  const double c1 = -.00277777777760991;
  const double c2 = 7.9365066682539e-4;
  const double c3 = -5.9520293135187e-4;
  const double c4 = 8.37308034031215e-4;
  const double c5 = -.00165322962780713;

  double d1,  t, w;
  int i, n, i1;

  if (a > .8)
    {
      if (a > 2.25)
	{
	  if (a >= 10.)
	    {
	      d1 = 1. / a;
	      t = d1*d1;
	      w = (((((c5*t + c4)*t + c3)*t + c2)*t + c1)*t + c0) / a;
	      return  d + w + (a - .5)*(log (a) - 1.);
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
	      return cdf_gamln1 (t-1) + log (w);
	    }
	}
      else
	{
	  return cdf_gamln1 ( a - .5 - .5);
	}
    }
  else 
    {
      return cdf_gamln1 (a) - log (a);
    }

}

