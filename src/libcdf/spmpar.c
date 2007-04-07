#include "cdf.h"

/*
 *     spmpar provides the single precision machine constants for 
 *     the computer being used. it is assumed that the argument 
 *     i is an int having one of the values 1, 2, or 3. if the 
 *     single precision arithmetic being used has m base b digits and 
 *     its smallest and largest exponents are emin and emax, then 
 *        spmpar(1) = b**(1 - m), the machine precision, 
 *        spmpar(2) = b**(emin - 1), the smallest magnitude, 
 *        spmpar(3) = b**emax*(1 - b**(-m)), the largest magnitude. 
 *     rewriten  by jpc to use lapack dlamch 
 */

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

