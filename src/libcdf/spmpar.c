#include "cdf.h"

/* ----------------------------------------------------------------------- */
/*     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR */
/*     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT */
/*     I IS AN INT HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE */
/*     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND */
/*     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN */
/*        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION, */
/*        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE, */
/*        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE. */
/*     rewriten  BY JPC to use lapack dlamch */
/* ----------------------------------------------------------------------- */

double cdf_spmpar (const int i)
{
  switch (i)
    {
    case 1 : return  cdf_dlamch ("p", 1L);
    case 2 : return  cdf_dlamch ("u", 1L);
    case 3 : return  cdf_dlamch ("o", 1L);
    }
  return 0.0;
}

