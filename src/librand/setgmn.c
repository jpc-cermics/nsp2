#include <math.h>
#include <nsp/machine.h> 
#include "grand.h" 

/*      SUBROUTINE setgmn(meanv,covm,p,parm) */
/*     JJV changed this routine to take leading dimension of COVM */
/*     JJV argument and pass it to SPOFA, making it easier to use */
/*     JJV if the COVM which is used is contained in a larger matrix */
/*     JJV and to make the routine more consistent with LINPACK. */
/*     JJV Changes are in comments, declarations, and the call to SPOFA. */
/* ********************************************************************** */

/*     SUBROUTINE SETGMN( MEANV, COVM, LDCOVM, P, PARM) */
/*            SET Generate Multivariate Normal random deviate */
/*                              Function */
/*      Places P, MEANV, and the Cholesky factoriztion of COVM */
/*      in PARM for GENMN. */
/*                              Arguments */
/*     MEANV --> Mean vector of multivariate normal distribution. */
/*                                        DOUBLE PRECISION MEANV(P) */
/*     COVM   <--> (Input) Covariance   matrix    of  the  multivariate */
/*                 normal distribution.  This routine uses only the */
/*                 (1:P,1:P) slice of COVM, but needs to know LDCOVM. */
/*                 (Output) Destroyed on output */
/*                                        DOUBLE PRECISION COVM(LDCOVM,P) */
/*     LDCOVM --> Leading actual dimension of COVM. */
/*                                        INTEGER LDCOVM */
/*     P     --> Dimension of the normal, or length of MEANV. */
/*                                        INTEGER P */
/*     PARM <-- Array of parameters needed to generate multivariate */
/*                normal deviates (P, MEANV and Cholesky decomposition */
/*                of COVM). */
/*                1 : 1                - P */
/*                2 : P + 1            - MEANV */
/*                P+2 : P*(P+3)/2 + 1  - Cholesky decomposition of COVM */
/*                                             DOUBLE PRECISION PARM(P*(P+3)/2 + 1) */

int rand_setgmn(double *meanv, double *covm, int *ldcovm, int *p, double *parm, int *ierr)
{
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

