/* folhp.f -- translated by f2c (version 19961017).
 *
 *
 */

/*  Copyright INRIA 
 *
 *     this routine checks if 
 *   the real root alpha/beta lies in the open left half plane 
 *      (if ls=1) 
 *    the complex conjugate roots with sum s and product p lie 
 *    in the open left half plane (if ls=2) 
 *    if so, folhp=1, otherwise, folhp=-1 
 *     in this function the parameter p is not referenced 
 * 
 *    int function folhp(ls,alpha,beta,s,p) 
 *    int ls 
 *    double precision alpha,beta,s,p 
 */

#include "ctrlpack.h"

int
nsp_ctrlpack_folhp (int *ls, double *alpha, double *beta, double *s,
		    double *p)
{
  /* System generated locals */
  int ret_val;

  ret_val = -1;
  if (*ls == 2)
    {
      goto L2;
    }
  if (*alpha * *beta < 0.)
    {
      ret_val = 1;
    }
  return ret_val;
 L2:
  if (*s < 0.)
    {
      ret_val = 1;
    }
  return ret_val;
}
