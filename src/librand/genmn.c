#include "grand.h"

/*     SUBROUTINE GENMN(PARM,X,WORK) 
 *              GENerate Multivariate Normal random deviate 
 *                              Arguments 
 *     PARM --> Parameters needed to generate multivariate normal 
 *               deviates (MEANV and Cholesky decomposition of 
 *               COVM). Set by a previous call to SETGMN. 
 *               1 : 1                - size of deviate, P 
 *               2 : P + 1            - mean vector 
 *               P+2 : P*(P+3)/2 + 1  - upper half of cholesky 
 *                                       decomposition of cov matrix 
 *                                             DOUBLE PRECISION PARM(*) 
 *     X    <-- Vector deviate generated. 
 *                                             DOUBLE PRECISION X(P) 

 *     WORK <--> Scratch array 
 *                                             DOUBLE PRECISION WORK(P) 
 *                              Method 
 *     1) Generate P independent standard normal deviates - Ei ~ N(0,1) 
 *     2) Using Cholesky decomposition find A s.t. trans(A)*A = COVM 
 *     3) trans(A)E + MEANV ~ N(MEANV,COVM) 
 */

int rand_genmn (double *parm, double *x, double *work)
{
  int i__1, i__2;
  int i__, j, p;
  double ae;
  int icount;

  --work;
  --x;
  --parm;

  /* Function Body */
  p = (int) parm[1];
  /*     Generate P independent normal deviates - WORK ~ N(0,1) */
  i__1 = p;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      work[i__] = rand_snorm ();
      /* L10: */
    }
  i__1 = p;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /*     PARM (P+2 : P*(P+3)/2 + 1) contains A, the Cholesky */
      /*      decomposition of the desired covariance matrix. */
      /*          trans(A)(1,1) = PARM(P+2) */
      /*          trans(A)(2,1) = PARM(P+3) */
      /*          trans(A)(2,2) = PARM(P+2+P) */
      /*          trans(A)(3,1) = PARM(P+4) */
      /*          trans(A)(3,2) = PARM(P+3+P) */
      /*          trans(A)(3,3) = PARM(P+2-1+2P)  ... */
      /*     trans(A)*WORK + MEANV ~ N(MEANV,COVM) */
      icount = 0;
      ae = 0.;
      i__2 = i__;
      for (j = 1; j <= i__2; ++j)
	{
	  icount = icount + j - 1;
	  ae += parm[i__ + (j - 1) * p - icount + p + 1] * work[j];
	  /* L20: */
	}
      x[i__] = ae + parm[i__ + 1];
    }
  return 0;
}

