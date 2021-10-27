#include "signal.h"

/*
 * calculate complete elliptic integral of first kind 
 */


#ifdef TEST_COMPEL
double signal_compel_old (double dk)
#else
  double signal_compel (double dk)
#endif
{
  const double c_b3 = 2.;
  const double de = 1.;
  int i1;
  double dgeo, dari, flma, domi, dtest;
  double dpi, dri, dellk;
  
  i1 = (int) C2F(slamch) ("l", 1L) - 2;
  flma = nsp_pow_di (c_b3, i1);
  dpi = atan (1.) * 4.;
  domi = nsp_dlamch ("p") * 2.;
  dgeo = de - dk * dk;
  if (dgeo <= 0.)
    {
      goto L10;
    }
  else
    {
      goto L20;
    }
 L10:
  dellk = flma;
  return dellk;;
 L20:
  dgeo = sqrt (dgeo);
  dri = de;
 L30:
  dari = dri;
  dtest = dari * domi;
  dri = dgeo + dri;
  if (dari - dgeo - dtest <= 0.)
    {
      goto L50;
    }
  dgeo = sqrt (dari * dgeo);
  dri *= .5;
  goto L30;
 L50:
  dellk = dpi / dri;
  return dellk;
}

double signal_compel_new (const double dk)
{
  double dgeo = 1. -  dk * dk;
  const double domi = nsp_dlamch ("p") * 2.;
  if (dgeo <= 0.)
    {
      const int i1 = (int) C2F(slamch) ("l", 1L) - 2;
      return nsp_pow_di (2.0, i1);
    }
  else
    {
      double dari, dri= 1.;
      dgeo = sqrt (dgeo);
      while (1)
	{
	  dari = dri;
	  dri += dgeo;
	  if (dari - dgeo - (dari * domi) <= 0.)
	    {
	      return (atan (1.) * 4) / dri;
	    }
	  dgeo = sqrt (dari * dgeo);
	  dri *= .5;
	}
    }	
}

#ifdef TEST_COMPEL
double signal_compel (const double dk)
{
  double res = signal_compel_new(dk);
  double dellk = signal_compel_old(dk);
  printf("in compel\n");
  if ( Abs(res - dellk ) >= 1.e-12 )
    {
      printf("calling compel bug\n");
    }
  return dellk;
}
#endif
