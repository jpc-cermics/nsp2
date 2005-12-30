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

NspIVect *nsp_ivect_create(char *name, double first, double step, double last, int flag)
{
  NspIVect *IV = new_ivect();
  if ( IV == NULLIVECT) 
    {
      Scierror("No more space\n");
      return(NULLIVECT);
    }
  if ((NSP_OBJECT(IV)->name =new_nsp_string(name))== NULLSTRING) return NULLIVECT;
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
      FREE(NSP_OBJECT(IV)->name);
      FREE(IV) ;
    };
}

/**
 * nsp_ivect_info:
 * @IV: 
 * @indent: 
 * 
 * Displays Info on IV
 **/

void nsp_ivect_info(NspIVect *IV, int indent,char *name,int rec_level)
{
  int i;
  if ( IV == NULLIVECT) 
    {
      Sciprintf("Null Pointer IVect \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("IVect %s %f:%f:%f %d\n",NSP_OBJECT(IV)->name,
	    IV->first,IV->step,IV->last,IV->flag);
}

/**
 * nsp_ivect_print:
 * @IV: 
 * @indent: 
 * 
 * writes IVect Objet 
 **/

void nsp_ivect_print(NspIVect *IV, int indent,char *name, int rec_level)
{
  nsp_ivect_info(IV,indent,NULL,LONG_MAX);
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
  int i;
  NspMatrix *Loc;
  double vals = IV->first;
  int count=0;
  if ( ( IV->first < IV->last && IV->step < 0 ) 
       || ( IV->first >  IV->last && IV->step > 0 ) 
       || IV->step == 0.00)
    {
      Loc = nsp_matrix_create(NVOID,'r',(int) 0,(int) 0);
      return(Loc);
    }
  /* counting **/
  while ( vals <= IV->last ) { vals += IV->step ; count++;}
  Loc = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
  if ( Loc == NULLMAT) return(NULLMAT);
  for ( i=0 ; i < count; i++) 
    {
      Loc->R[i] = IV->first + ((double) i)*IV->step;
    }
  return(Loc);
}




