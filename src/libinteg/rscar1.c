#include "integ.h"

#define lsa001_1 lsa001_._3
#define lsr001_1 lsr001_._3

extern struct
{
  int mesgflag;
  int lunit;
} integ_eh0001;

/* 
 *this routine restores from rsav and isav the contents of common 
 *blocks ls0001, lsa001, and eh0001, which are used internally in the 
 *lsodar package.  this presumes that rsav and isav were loaded by means 
 *of subroutine svcma or the equivalent. 
 */

int nsp_ode_rscar1 (double *rsav, double *isav)
{
  int lenrls = 219;
  int lenils = 39;
  int lenrla = 22;
  int lenila = 9;
  int lenrlr = 5;
  int lenilr = 9;
  int i, l;
  int c1 = 1;

  l = 0;
  C2F (dcopy) (&lenrls, &rsav[l], &c1, ls0001_._3.rls, &c1);
  l += lenrls;
  C2F (dcopy) (&lenrla, &rsav[l], &c1, lsa001_1.rlsa, &c1);
  l += lenrla;
  C2F (dcopy) (&lenrlr, &rsav[l], &c1, lsr001_1.rlsr, &c1);

  l = 0;
  for (i = 0; i < lenils ; ++i)
    {
      ls0001_._3.ils[i] = (int) isav[l + i];
    }
  l += lenils;
  for (i = 0; i < lenila; ++i)
    {
      lsa001_1.ilsa[i] = (int) isav[l + i];
    }
  l += lenila;
  for (i = 0; i < lenilr; ++i)
    {
      lsr001_1.ilsr[i] = (int) isav[l + i];
    }
  l += lenilr;
  integ_eh0001.mesgflag = (int) isav[l];
  integ_eh0001.lunit = (int) isav[l+1];
  return 0;
}		

