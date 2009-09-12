#include "integ.h"


#define lsa001_1 lsa001_._3

struct
{
  int ieh[2];
} eh0001_;

#define eh0001_1 eh0001_


/* 
 * this routine stores in rsav and isav the contents of common blocks 
 * ls0001, lsa001, and eh0001, which are used internally in the 
 * lsoda package. 
 *
 *
 *rsav = real array of length 241 or more. 
 *isav = int array of length 50 or more. 
 */


int nsp_ode_svcma1 (double *rsav, double *isav)
{
  ls0001 *Ls = (ls0001*) &ls0001_;

  int lenrls = 219;
  int lenils = 39;
  int lenrla = 22;
  int lenila = 9;
  int i1;
  int i;

  i1 = lenrls;
  for (i = 0; i < i1 ; ++i)
    {
      rsav[i] = Ls->rls[i];
    }
  i1 = lenrla;
  for (i = 0; i < i1; ++i)
    {
      rsav[lenrls + i] = lsa001_1.rlsa[i];
    }

  i1 = lenils;
  for (i = 0; i < i1; ++i)
    {
      isav[i] = (double) Ls->ils[i];
    }
  i1 = lenila;
  for (i = 0; i < i1; ++i)
    {
      isav[lenils + i] = (double) lsa001_1.ilsa[i];
    }

  isav[lenils + lenila] = (double) eh0001_1.ieh[0];
  isav[lenils + lenila + 1] = (double) eh0001_1.ieh[1];
  return 0;
}	

