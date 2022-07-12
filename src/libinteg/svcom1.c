#include "integ.h"



extern struct
{
  int mesgflag;
  int lunit;
} integ_eh0001;

/*
 *this routine stores in rsav and isav the contents of common blocks 
 *ls0001 and eh0001, which are used internally in the lsode package. 
 * 
 *%calling sequence 
 *rsav = real array of length 219 or more. 
 *isav = int array of length 41 or more. 
 */

int nsp_ode_svcom1 (double *rsav, double *isav)
{
  int lenrls = 219;
  int lenils = 39;
  int i;

  for (i = 0; i < lenrls ; ++i)
    {
      rsav[i] = ls0001_._3.rls[i];
    }

  for (i = 0; i < lenils; ++i)
    {
      isav[i] = (double) ls0001_._3.ils[i];
    }

  isav[lenils] = (double) integ_eh0001.mesgflag;
  isav[lenils+1] = (double) integ_eh0001.lunit;
  return 0;
}		

