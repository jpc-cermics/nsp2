/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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
 * Graphic library
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <nsp/figure.h> 
#include <nsp/axes.h> 

/* just kept for compatibility since 
 * it must be kept in drivers to match 
 * old drivers.
 */

void tape_replay_undo_scale(BCG *Xgc)
{ 
  Sciprintf("This function should not be called\n");
}

void tape_replay_new_angles(BCG *Xgc)
{ 
  Sciprintf("This function should not be called\n");
}

void tape_clean_plots(BCG *Xgc,int winnumber)
{
  Sciprintf("This function should not be called\n");
}

void tape_replay(BCG *Xgc,const GdkRectangle *rect)
{ 
  Sciprintf("This function should not be called\n");
}


void tape_replay_new_scale(BCG *Xgc,int winnumber, int *flag,
			   int *aaint,double *bbox, 
			   int *ibbox)
{ 
  Sciprintf("This function should not be called\n");
}

/**
 * tape_check_recorded_3D:
 * @Xgc: 
 * @winnumber: 
 * 
 * 
 * checks if recorded list contains 3d graphics 
 * 
 * Returns: 
 **/

int tape_check_recorded_3D(BCG *Xgc,int winnumber)
{
  /* A revoir 
   *
   */
  return OK;
}
