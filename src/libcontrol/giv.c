/* giv.f -- translated by f2c (version 19961017).
 *
 *
 */

/*    Copyright INRIA 
 *     this routine constructs the givens transformation 
 * 
 *               ( sc  ss ) 
 *           g = (        ),   sc**2+ss**2 = 1. , 
 *               (-ss  sc ) 
 * 
 *     which zeros the second entry of the 2-vector (sa,sb)**t 
 *     this routine is a modification of the blas routine srotg 
 *     (algorithm 539) in order to leave the arguments sa and sb 
 *     unchanged 
 * 
 *    subroutine giv(sa,sb,sc,ss) 
 *    double precision sa,sb,sc,ss 
 */

#include "ctrlpack.h"

int nsp_ctrlpack_giv (double *sa, double *sb, double *sc, double *ss)
{
  double r__, u, v;

  if (Abs (*sa) <= Abs (*sb))
    {
      goto L10;
    }
  /* here Abs(sa) .gt. Abs(sb) */
  u = *sa + *sa;
  v = *sb / u;
  r__ = sqrt (v * v + .25) * u;
  *sc = *sa / r__;
  *ss = v * (*sc + *sc);
  return 0;
  /* here Abs(sa) .le. Abs(sb)  */
 L10:
  if (*sb == 0.)
    {
      goto L20;
    }
  u = *sb + *sb;
  v = *sa / u;
  r__ = sqrt (v * v + .25) * u;
  *ss = *sb / r__;
  *sc = v * (*ss + *ss);
  return 0;
  /** here sa = sb = 0. 
   */
 L20:
  *sc = 1.;
  *ss = 0.;
  return 0;
}

