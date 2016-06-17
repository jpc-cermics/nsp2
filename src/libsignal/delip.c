#include "signal.h"

/*
 * vector valued elliptic integral : 
 * r = integral from 0 to x of : 
 * dt/sqrt((1-t^2)*(1-ck^2*t^2)) for x real positive 
 * x=argument r=resr+%i*resi=result 
 * ck = parameter 
 * n size of x and r 
 * Author F.D. 
 */

static int c__1 = 1;

int
signal_delip (const int *n, double *resr, double *resi, const double *x, const double *ck)
{
  int i__1;
  int k;
  double m, r__, compk, m1, compk1, xxxxx;
  double xx, yy, ck1;

  --x;
  --resi;
  --resr;

  m = *ck * *ck;
  m1 = 1. - m;
  ck1 = sqrt (m1);
  signal_compel (ck, &compk);
  signal_compel (&ck1, &compk1);
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      xx = x[k];
      if (xx >= 0. && xx <= 1.)
	{
	  signal_deli2 (&c__1, &r__, &xx, ck);
	  resr[k] = r__;
	  resi[k] = 0.;
	}
      else if (xx > 1. && xx <= 1. / *ck)
	{
	  xxxxx = 1. / *ck - xx;
	  /*    .     change of variable : integral from 1 to xx of 
	   *    .     dt/sqrt((1-mt^2)(t^2-1)) = integral from o to yy of 
	   *    .     dt/sqrt((1-t^2)(1-m1t^2))  with m1=1-m and 
	   *    .     yy = (1/sqrt(m1))*sqrt((x^2-1)/x^2) 
	   */
	  yy = 1. / ck1 * sqrt ((xx * xx - 1.) / (xx * xx));
	  signal_deli2 (&c__1, &r__, &yy, &ck1);
	  resi[k] = r__;
	  resr[k] = compk;
	}
      else if (xx >= 1. / *ck)
	{
	  xxxxx = 1. / (*ck * xx);
	  signal_deli2 (&c__1, &r__, &xxxxx, ck);
	  resi[k] = compk1;
	  resr[k] = r__;
	}
      /* L1: */
    }
  return 0;
}				/* delip_ */
