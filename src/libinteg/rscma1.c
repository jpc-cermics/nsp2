#include "integ.h"

/* 
 *
 */
 
#define lsa001_1 lsa001_._3

struct
{
  int ieh[2];
} eh0001_;

#define eh0001_1 eh0001_

/*
 *this routine restores from rsav and isav the contents of common 
 *blocks ls0001, lsa001, and eh0001, which are used internally in the 
 *lsoda package.  this presumes that rsav and isav were loaded by means 
 *of subroutine svcma or the equivalent. 
 */

int nsp_ode_pipo (double *rsav, double *isav)
{
  int i;
  for (i = 0 ; i < 7; ++i)
    {
      ls0001_._3.rls[i] = rsav[i];
    }
  return 0;
}



int nsp_ode_rscma1 (double *rsav, double *isav)
{
  int lenrls = 219;
  int lenils = 39;
  int lenrla = 22;
  int lenila = 9;
  int i;

  for (i = 0 ; i < lenrls; ++i)
    {
      ls0001_._3.rls[i] = rsav[i];
    }
  for (i = 0 ; i < lenrla; ++i)
    {
      lsa001_1.rlsa[i] = rsav[lenrls + i];
    }
  for (i = 0 ; i < lenils; ++i)
    {
      ls0001_._3.ils[i] = (int) isav[i];
    }
  for (i = 0; i < lenila; ++i)
    {
      lsa001_1.ilsa[i] = (int) isav[lenils + i];
    }
  eh0001_1.ieh[0] = (int) isav[lenils + lenila];
  eh0001_1.ieh[1] = (int) isav[lenils + lenila + 1];
  return 0;
}	

