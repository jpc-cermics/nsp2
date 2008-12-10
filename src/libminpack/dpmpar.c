#include "minpack.h"

/*     Function dpmpar 
 *     This function provides double precision machine parameters 
 *     when the appropriate set of data statements is activated (by 
 *     removing the c from column 1) and all other data statements are 
 *     rendered inactive. Most of the parameter values were obtained 
 *     from the corresponding Bell Laboratories Port Library function. 
 *
 *     The function statement is 
 *
 *       double precision function dpmpar(i) 
 *
 *     where 
 *
 *       i is an int input variable set to 1, 2, or 3 which 
 *         selects the desired machine parameter. If the machine has 
 *         t base b digits and its smallest and largest exponents are 
 *         emin and emax, respectively, then these parameters are 
 *
 *         dpmpar(1) = b**(1 - t), the machine precision, 
 *
 *         dpmpar(2) = b**(emin - 1), the smallest magnitude, 
 *
 *         dpmpar(3) = b**emax*(1 - b**(-t)), the largest magnitude. 
 *
 *     rewriten with dlamch (jpc).
 *     Argonne National Laboratory. MINPACK Project. November 1996. 
 *     Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More' 
 */

double minpack_dpmpar (int i)
{
  switch (i)
    {
    case 1 :  return nsp_dlamch ("p");  break;
    case 2 :  return nsp_dlamch ("u");  break;
    case 3 :  return nsp_dlamch ("o");  break;
    }
  return 0.0;
}
