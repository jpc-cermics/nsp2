#include "integ.h"

/* Common Block Declarations */


struct
{
  int ieh[2];
} eh0001_;


/*
 * this routine restores from rsav and isav the contents of common 
 * blocks ls0001 and eh0001, which are used internally in the lsode 
 * package.  this presumes that rsav and isav were loaded by means 
 * of subroutine svcom or the equivalent. 
 */

int nsp_ode_rscom1 (double *rsav, double *isav)
{
  ls0001 *Ls = (ls0001*) &ls0001_;

  int lenrls = 219;
  int lenils = 39;
  int i;

  for (i = 0 ; i < lenrls ; ++i)
    {
      Ls->rls[i] = rsav[i];
    }
  for (i = 0;  i < lenils; ++i)
    {
      Ls->ils[i] = (int) isav[i];
    }
  eh0001_.ieh[0] = (int) isav[lenils];
  eh0001_.ieh[1] = (int) isav[lenils + 1];
  return 0;
}
