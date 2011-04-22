#include "integ.h"



struct
{
  int ieh[2];
} eh0001_;

#define eh0001_1 eh0001_

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

  isav[lenils] = (double) eh0001_1.ieh[0];
  isav[lenils+1] = (double) eh0001_1.ieh[1];
  return 0;
}		

