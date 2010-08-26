/* 
 * Copyright (C) 1987-2010 Frederic Bonnans, Geneviève Launay INRIA.
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

/* 
 *    calcul de l'etat initial dans ICSE : cas standard : 
 *    controle par l'etat initial 
 *     
 *! 
 * 
 */

int
optim_icsei (int *indi, int *nui, double *u, double *y0, double *y0u,
	     int *itu, double *dtu, double *t0, double *tf, double *dti,
	     double *dtf, double *ermx, int *iu, int *nuc, int *nuv,
	     int *ilin, int *nti, int *ntf, int *ny, int *nea, int *itmx,
	     int *nex, int *nob, int *ntob, int *ntobi, int *nitu, int *ndtu)
{
  double c_b3 = 0.;
  int c__1 = 1;
  /* System generated locals */
  int y0u_dim1, y0u_offset, i__1;

  /* Local variables */
  int i__;

  /* Parameter adjustments */
  --u;
  --iu;
  y0u_dim1 = *ny;
  y0u_offset = y0u_dim1 + 1;
  y0u -= y0u_offset;
  --y0;
  --itu;
  --dtu;

  /* Function Body */
  if (*indi == 1)
    {
      i__1 = *ny;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y0[i__] = u[i__];
	}
    }
  /* 
   */
  if (*indi == 2)
    {
      /*      cas ou y0u est l identite 
       */
      i__1 = *ny * *nui;
      nsp_dset (&i__1, &c_b3, &y0u[y0u_offset], &c__1);
      i__1 = *ny;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  y0u[i__ + i__ * y0u_dim1] = 1.;
	}
    }
  return 0;
}
