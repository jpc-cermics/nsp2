#include "cdf.h"

/* ********************************************************************** */
/*     DOUBLE PRECISION FUNCTION DEVLPL(A,N,X) */
/*              Double precision EVALuate a PoLynomial at X */
/*                              Function */
/*     returns */
/*          A(1) + A(2)*X + ... + A(N)*X**(N-1) */
/*                              Arguments */
/*     A --> Array of coefficients of the polynomial. */
/*                                        A is DOUBLE PRECISION(N) */
/*     N --> Length of A, also degree of polynomial - 1. */
/*                                        N is INT */
/*     X --> Point at which the polynomial is to be evaluated. */
/*                                        X is DOUBLE PRECISION */
/* ********************************************************************** */

double cdf_devlpl (const double *a,const int *n, double *x)
{
  double term;
  int i;
  term = a[*n -1];
  for (i = *n - 2; i >= 0; --i)
    {
      term = a[i] + term * *x;
    }
  return  term;
}

