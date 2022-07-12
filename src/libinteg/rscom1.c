#include "integ.h"

/* Common Block Declarations */


extern struct
{
  int mesgflag;
  int lunit;
} integ_eh0001;


/*
 * this routine restores from rsav and isav the contents of common 
 * blocks ls0001 and eh0001, which are used internally in the lsode 
 * package.  this presumes that rsav and isav were loaded by means 
 * of subroutine svcom or the equivalent. 
 */

int nsp_ode_rscom1 (double *rsav, double *isav)
{
  int lenrls = 219;
  int lenils = 39;
  int i;

  for (i = 0 ; i < lenrls ; ++i)
    {
      ls0001_._3.rls[i] = rsav[i];
    }
  for (i = 0;  i < lenils; ++i)
    {
      ls0001_._3.ils[i] = (int) isav[i];
    }
  integ_eh0001.mesgflag = (int) isav[lenils];
  integ_eh0001.lunit = (int) isav[lenils + 1];
  return 0;
}
