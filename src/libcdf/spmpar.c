/* Nsp
 * Copyright (C) 2007 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 *
 */

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

