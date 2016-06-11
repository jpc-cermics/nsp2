/* fout.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*    Copyright INRIA 
 *     this function checks if 
 *      the real root alpha/beta lies outside the unit disc 
 *      (if lsize=1) 
 *      the complex conjugate roots with sum s and product p lie 
 *      outside the unit disc (if lsize=2). 
 *     if so, fout=1, otherwise, fout=-1 
 *     in this function the parameter s is not referenced 
 * 
 *    int function fout(lsize,alpha,beta,s,p) 
 *    int lsize 
 *    double precision alpha,beta,s,p 
 */

int
nsp_ctrlpack_fout (int *lsize, double *alpha, double *beta, double *s,
		   double *p)
{
  int ret_val = -1;
  if (*lsize == 2)
    {
      goto L2;
    }
  if (Abs (*alpha) >= Abs (*beta))
    {
      ret_val = 1;
    }
  return ret_val;
 L2:
  if (Abs (*p) >= 1.)
    {
      ret_val = 1;
    }
  return ret_val;
}
