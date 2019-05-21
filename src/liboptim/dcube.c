#include "optim.h"

/* m1qn3
 * Copyright (C) 1998-2019 Jean Charles Gilbert, Claude Lemarechal, INRIA. 
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
 * Using f and fp at t and ta, computes new t by cubic formula 
 * safeguarded inside [tlower,tupper]. 
 */

int optim_dcube (double *t, double *f, double *fp, double *ta, double *fa,
		 double *fpa, double *tlower, double *tupper)
{
  double d__1;
  double sign, anum, b, z1, discri, den;

  z1 = *fp + *fpa - (*fa - *f) * 3. / (*ta - *t);
  b = z1 + *fp;
  /* 
   *             first compute the discriminant (without overflow) 
   * 
   */
  if (Abs (z1) <= 1.)
    {
      discri = z1 * z1 - *fp * *fpa;
    }
  else
    {
      discri = *fp / z1;
      discri *= *fpa;
      discri = z1 - discri;
      if (z1 >= 0. && discri >= 0.)
	{
	  discri = sqrt (z1) * sqrt (discri);
	  goto L120;
	}
      if (z1 <= 0. && discri <= 0.)
	{
	  discri = sqrt (-z1) * sqrt (-discri);
	  goto L120;
	}
      discri = -1.;
    }
  if (discri < 0.)
    {
      if (*fp < 0.)
	{
	  *t = *tupper;
	}
      if (*fp >= 0.)
	{
	  *t = *tlower;
	}
      goto L900;
    }
  /* 
   * discriminant nonnegative, compute solution (without overflow) 
   * 
   */
  discri = sqrt (discri);
 L120:
  if (*t - *ta < 0.)
    {
      discri = -discri;
    }
  sign = (*t - *ta) / (d__1 = *t - *ta, Abs (d__1));
  if (b * sign > 0.)
    {
      *t += *fp * (*ta - *t) / (b + discri);
    }
  else
    {
      den = z1 + b + *fpa;
      anum = b - discri;
      if ((d__1 =
	   (*t - *ta) * anum, Abs (d__1)) < (*tupper - *tlower) * Abs (den))
	{
	  *t += anum * (*ta - *t) / den;
	}
      else
	{
	  *t = *tupper;
	}
    }
 L900:
  *t = Max (*t, *tlower);
  *t = Min (*t, *tupper);
  return 0;
}

