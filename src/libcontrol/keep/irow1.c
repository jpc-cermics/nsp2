/* irow1.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *   int function irow1(i,m) 
 *   this routine is only to be call from syhsc 
 */

int nsp_ctrlpack_irow1 (int *i, int *m)
{
  if (*i == 1)
    {
      return  0;
    }
  return (*i - 1) * *m - (*i - 2) * (*i - 3) / 2;
}

