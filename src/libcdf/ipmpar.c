#include <nsp/machine.h>
#include <nsp/numeric.h>
#include "cdf.h" 

static int largestint();
extern double cdf_dlamch(char *,long int);

/* ----------------------------------------------------------------------- */
/*  INTEGERS. */
/*     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM */
/*               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) ) */
/*               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1. */
/*     IPMPAR(1) = A, THE BASE. */
/*     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS. */
/*     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE. */
/*  FLOATING-POINT NUMBERS. */
/*     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING */
/*     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE */
/*     NONZERO NUMBERS ARE REPRESENTED IN THE FORM */
/*               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M) */
/*               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M, */
/*               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX. */
/*     IPMPAR(4) = B, THE BASE. */
/*  SINGLE-PRECISION */
/*     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS. */
/*     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E. */
/*     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E. */
/*  DOUBLE-PRECISION */
/*     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS. */
/*     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E. */
/*     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E. */
/* ----------------------------------------------------------------------- */
/*     RWRITTEN BY JPC to use lapack dlamch + a small c program */
/*     for ipmpar(3) */
/* ----------------------------------------------------------------------- */

int cdf_ipmpar(const int *i)
{
  switch (*i) 
    {
    case 3: return largestint();
    case 4: return (int) cdf_dlamch("b", 1L);
    case 9: return (int) cdf_dlamch("m", 1L);
    case 10: return (int) cdf_dlamch("l", 1L);
    default :
      Scierror("ipmpar called with wrong argument\n");
      return 0;
    }
}


/*
 *  MACHINE CONSTANTS
 *  These numbers must be updated when the program is ported to a new machine.
 *  Using spConfig.h to get the largest int 
 */

static int largestint()
{
  static int first=0,large;
  if ( first == 0) 
    {
      if (sizeof(int)==sizeof(long))
	large = LARGEST_LONG_INTEGER ;
      else if (sizeof(int)==sizeof(short))
	large = LARGEST_SHORT_INTEGER;
      else 
	large = 2147483647 ; /** using default value **/
      first++;
      return large ;
    }
  else 
    return large;
}

