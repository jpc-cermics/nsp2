#include "integ.h"

/* Common Block Declarations */

#define lsa001_1 lsa001_._3
#define lsr001_1 lsr001_._3

struct
{
  int ieh[2];
} eh0001_;

#define eh0001_1 eh0001_

/*
 *this routine restores from rsav and isav the contents of common 
 *blocks ls0001, lsa001, and eh0001, which are used internally in the 
 *lsodar package.  this presumes that rsav and isav were loaded by means 
 *of subroutine svcma or the equivalent. 
 */

int nsp_ode_svcar1 (double *rsav, double *isav)
{
  int c1 = 1;
  ls0001 *Ls = (ls0001*) &ls0001_;
  int lenrls = 219;
  int lenils = 39;
  int lenrla = 22;
  int lenila = 9;
  int lenrlr = 5;
  int lenilr = 9;
  int i, l;

  l = 0;
  C2F (dcopy) (&lenrls, Ls->rls, &c1, &rsav[l], &c1);
  l += lenrls;
  C2F (dcopy) (&lenrla, lsa001_1.rlsa, &c1, &rsav[l], &c1);
  l += lenrla;
  C2F (dcopy) (&lenrlr, lsr001_1.rlsr, &c1, &rsav[l], &c1);

  l = 0;
  for (i = 0; i < lenils; ++i)
    {
      isav[l + i] = (double) Ls->ils[i];
    }
  l += lenils;
  for (i = 0 ; i < lenila; ++i)
    {
      isav[l + i] = (double) lsa001_1.ilsa[i];
    }
  l += lenila;
  for (i = 0; i < lenilr; ++i)
    {
      isav[l + i] = (double) lsr001_1.ilsr[i];
    }
  l += lenilr;
  isav[l] = (double) eh0001_1.ieh[0];
  isav[l+1] = (double) eh0001_1.ieh[1];
  return 0;
}

