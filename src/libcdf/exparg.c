#include "cdf.h"

/* -------------------------------------------------------------------- */
/*     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH */
/*     EXP(W) CAN BE COMPUTED. */
/*     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR */
/*     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO. */
/*     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED. */
/*  jpc: translated from fortran */
/* -------------------------------------------------------------------- */


double cdf_exparg (const int l)
{
  const int c__4 = 4;
  const int c__9 = 9;
  const int c__10 = 10;
  int b, m;
  double lnb;
  b = cdf_ipmpar (&c__4);
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
      m = cdf_ipmpar (&c__10);
      return m * lnb * .99999;
    }
  m = cdf_ipmpar (&c__9) - 1;
  return  m * lnb * .99999;
}
  
