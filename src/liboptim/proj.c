/* 
 * Copyright (C) 1985-2008 Frederic Bonnans, INRIA.
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
 *--------------------------------------------------------------------------*/

#include "optim.h"

int optim_proj (int *n, double *binf, double *bsup, double *x)
{
  int i;
  for (i = 0; i < *n; ++i)
    {
      x[i] = Max (binf[i],Min (x[i],bsup[i]));
    }
  return 0;
}	

