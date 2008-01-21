/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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
 */

/*
 * Only used to store fully implicit <<:>> vector up to now  
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nsp/object.h"

/**
 * nsp_ivect_create:
 * @name: 
 * @first: 
 * @step: 
 * @last: 
 * @flag: 
 * 
 * Creates an implicit vector 
 * Return value: a #NspIVect or %NULL
 **/

NspIVect *nsp_ivect_create(char *name, int first, int step, int last, int flag)
{
  NspIVect *IV = new_ivect();
  if ( IV == NULLIVECT) 
    {
      Scierror("No more space\n");
      return(NULLIVECT);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(IV),name) == NULL)
    return NULLIVECT;
  NSP_OBJECT(IV)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
    IV->otype = IVECT;
    IV->ftype = IVect_Type;
  */
  IV->first=first;
  IV->step=step;
  IV->last=last;
  IV->flag=flag;
  return(IV);
}


/**
 * nsp_ivect_copy:
 * @A: 
 * 
 * Returns a copy of A
 * 
 * Return value: a #NspIVect or %NULL
 **/

NspIVect *nsp_ivect_copy(NspIVect *A)
{
  return (nsp_ivect_create(NVOID,A->first,A->step,A->last,A->flag));
}


/**
 * nsp_ivect_destroy:
 * @IV: 
 * 
 * Deletes  IV 
 **/
void nsp_ivect_destroy(NspIVect *IV)
{
  if ( IV != NULLIVECT)
    {
      nsp_object_destroy_name(NSP_OBJECT(IV));
      FREE(IV) ;
    };
}

/**
 * nsp_ivect_info:
 * @IV: 
 * @indent: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * Displays Info on IV
 **/

int nsp_ivect_info(NspIVect *IV, int indent,char *name,int rec_level)
{
  int i;
  if ( IV == NULLIVECT) 
    {
      Sciprintf("Null Pointer IVect \n");
      return TRUE;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("IVect %s %d:%d:%d %d\n",NSP_OBJECT(IV)->name,
	    IV->first,IV->step,IV->last,IV->flag);
  return TRUE;
}

/**
 * nsp_ivect_print:
 * @IV: 
 * @indent: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * writes IVect Objet 
 **/

int nsp_ivect_print(NspIVect *IV, int indent,char *name, int rec_level)
{
  nsp_ivect_info(IV,indent,NULL,0);
  return TRUE;
}


int nsp_ivect_count(NspIVect *iv)
{
  if ( iv->step == 1 )
    return Max(0, iv->last - iv->first + 1);

  else if ( iv->step == -1 )
    return Max(0, iv->first - iv->last + 1);

  else if ( iv->step > 0 )
    {
      int delta = iv->last - iv->first;
      return delta >= 0 ? delta/iv->step + 1 : 0;
    }
  else if ( iv->step < 0 )
    {
      int delta = iv->first - iv->last;
      return delta >= 0 ? delta/(-iv->step) + 1 : 0;
    }
  else
    return 0;
}

int nsp_ivect_count_with_min_max(NspIVect *iv, int *imin, int *imax)
{
  int count, delta;
  if ( iv->step == 1 )
    {
      count = Max(0, iv->last - iv->first + 1);
      *imin = iv->first;
      *imax = iv->last;
    }
  else if ( iv->step == -1 )
    {
      count = Max(0, iv->first - iv->last + 1);
      *imin = iv->last;
      *imax = iv->first;
    }
  else if ( iv->step > 0 )
    {
      delta = iv->last - iv->first;
      count = delta >= 0 ? delta/iv->step + 1 : 0;
      *imin = iv->first;
      *imax = iv->first + (count-1)*iv->step;
    }
  else if ( iv->step < 0 )
    {
      delta = iv->first - iv->last;
      count = delta >= 0 ? delta/(-iv->step) + 1 : 0;
      *imax = iv->first;
      *imin = iv->first + (count-1)*iv->step;
    }
  else
    {
      count = 0;
      *imin = 0;
      *imax = 0;
    }
  
  return count;
}


/**
 * nsp_ivect_2_mat:
 * @IV: 
 * 
 * Expand an implicit vector to a #NspMatrix
 * Return value: a #NspMatrix or %NULL
 **/

NspMatrix *nsp_ivect_2_mat(NspIVect *IV)
{
  int i, count;
  NspMatrix *Loc;

  count = nsp_ivect_count(IV);
  
  if ( (Loc = nsp_matrix_create(NVOID, 'r', 1, count)) == NULLMAT )
    return NULLMAT;

  if ( count > 0 )
    {
      Loc->R[0] = (double) IV->first;
      for ( i = 1 ; i < count ; i++ )
	Loc->R[i] = Loc->R[i-1] + (double) IV->step;
    }

  return Loc;
}



