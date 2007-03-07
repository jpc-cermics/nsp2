#include <math.h>
#include <nsp/machine.h> 
#include "grand.h" 


static int rand_spofa (double *a, int *lda, int *n, int *info);

/*     SUBROUTINE SETGMN( MEANV, COVM, LDCOVM, P, PARM) 
 *            SET Generate Multivariate Normal random deviate 
 *                              Function 
 *      Places P, MEANV, and the Cholesky factoriztion of COVM 
 *      in PARM for GENMN. 
 *                              Arguments 
 *     MEANV --> Mean vector of multivariate normal distribution. 
 *                                        DOUBLE PRECISION MEANV(P) 
 *     COVM   <--> (Input) Covariance   matrix    of  the  multivariate 
 *                 normal distribution.  This routine uses only the 
 *                 (1:P,1:P) slice of COVM, but needs to know LDCOVM. 
 *                 (Output) Destroyed on output 
 *                                        DOUBLE PRECISION COVM(LDCOVM,P) 
 *     LDCOVM --> Leading actual dimension of COVM. 
 *                                        INTEGER LDCOVM 
 *     P     --> Dimension of the normal, or length of MEANV. 
 *                                        INTEGER P 
 *     PARM <-- Array of parameters needed to generate multivariate 
 *                normal deviates (P, MEANV and Cholesky decomposition 
 *                of COVM). 
 *                1 : 1                - P 
 *                2 : P + 1            - MEANV 
 *                P+2 : P*(P+3)/2 + 1  - Cholesky decomposition of COVM 
 *                                             DOUBLE PRECISION PARM(P*(P+3)/2 + 1) 
 */

int rand_setgmn(double *meanv, double *covm, int *ldcovm, int *p, double *parm, int *ierr)
{
  /*     JJV changed this routine to take leading dimension of COVM */
  /*     JJV argument and pass it to SPOFA, making it easier to use */
  /*     JJV if the COVM which is used is contained in a larger matrix */
  /*     JJV and to make the routine more consistent with LINPACK. */
  /*     JJV Changes are in comments, declarations, and the call to SPOFA. */

  int covm_dim1, covm_offset, i1, i2;
  int info, i, j;
  int  icount;
  /*     TEST THE INPUT */
  /* Parameter adjustments */
  --parm;
  covm_dim1 = *ldcovm;
  covm_offset = covm_dim1 + 1;
  covm -= covm_offset;
  --meanv;
  /* Function Body */
  parm[1] = (double) (*p);
  /*     PUT P AND MEANV INTO PARM */
  i1 = *p + 1;
  for (i = 2; i <= i1; ++i) {
    parm[i] = meanv[i - 1];
  }
  /*      Cholesky decomposition to find A s.t. trans(A)*(A) = COVM */
  /*      CALL spofa(covm,p,p,info) */
  rand_spofa(&covm[covm_offset], ldcovm, p, &info);
  *ierr = 0;
  if ( (info != 0)) 
    {
      Sciprintf("Rand: COV not positive definite\n");
      *ierr = 1;
      return 0;
    }
  icount = *p + 1;
  /*     PUT UPPER HALF OF A, WHICH IS NOW THE CHOLESKY FACTOR, INTO PARM */
  /*          COVM(1,1) = PARM(P+2) */
  /*          COVM(1,2) = PARM(P+3) */
  /*                    : */
  /*          COVM(1,P) = PARM(2P+1) */
  /*          COVM(2,2) = PARM(2P+2)  ... */
  i1 = *p;
  for (i = 1; i <= i1 ; ++i) {
    i2 = *p;
    for (j = i; j <= i2; ++j) {
      ++icount;
      parm[icount] = covm[i + j * covm_dim1];
    }
  }
  return 0;
} 



/*     SPOFA FACTORS A DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE MATRIX. 
 *     SPOFA IS USUALLY CALLED BY SPOCO, BUT IT CAN BE CALLED 
 *     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED. 
 *     (TIME FOR SPOCO) = (1 + 18/N)*(TIME FOR SPOFA) . 
 *     ON ENTRY 
 *        A       DOUBLE PRECISION(LDA, N) 
 *                THE SYMMETRIC MATRIX TO BE FACTORED.  ONLY THE 
 *                DIAGONAL AND UPPER TRIANGLE ARE USED. 
 *        LDA     INT 
 *                THE LEADING DIMENSION OF THE ARRAY  A . 
 *        N       INT 
 *                THE ORDER OF THE MATRIX  A . 
 *     ON RETURN 
 *        A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A = TRANS(R)*R 
 *                WHERE  TRANS(R)  IS THE TRANSPOSE. 
 *                THE STRICT LOWER TRIANGLE IS UNALTERED. 
 *                IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE. 
 *        INFO    INT 
 *                = 0  FOR NORMAL RETURN. 
 *                = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR 
 *                     OF ORDER  K  IS NOT POSITIVE DEFINITE. 
 *     LINPACK.  THIS VERSION DATED 08/14/78 . 
 *     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB. 
 *     SUBROUTINES AND FUNCTIONS 
 */

static int rand_spofa (double *a, int *lda, int *n, int *info)
{
  int c__1 = 1;
  int a_dim1, a_offset, i__1, i__2, i__3;
  int j, k;
  double s, t;
  int jm1;

  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      *info = j;
      s = 0.;
      jm1 = j - 1;
      if (jm1 < 1)
	{
	  goto L20;
	}
      i__2 = jm1;
      for (k = 1; k <= i__2; ++k)
	{
	  i__3 = k - 1;
	  t =
	    a[k + j * a_dim1] - rand_sdot (&i__3, &a[k * a_dim1 + 1], &c__1,
					   &a[j * a_dim1 + 1], &c__1);
	  t /= a[k + k * a_dim1];
	  a[k + j * a_dim1] = t;
	  s += t * t;
	  /* L10: */
	}
    L20:
      s = a[j + j * a_dim1] - s;
      /*     ......EXIT */
      if (s <= 0.)
	{
	  goto L40;
	}
      a[j + j * a_dim1] = sqrt (s);
      /* L30: */
    }
  *info = 0;
 L40:
  return 0;
}	



