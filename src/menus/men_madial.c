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
 *
 * menu matrix dialog 
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"

int  nsp_matrix_dialog(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
		       NspSMatrix *Init_matrix,int *cancel)
{
  int rep,ierr=0;
  char *labels =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  if ( labels == NULL) return FAIL;
  rep =  nsp_matrix_dialog_(labels,Labels_v->S,Labels_h->S, Init_matrix->S,
			    Labels_v->mn, Labels_h->mn,&ierr);
  StringDestroy(&labels);
  if ( ierr == 0) 
    {
      *cancel = ( rep == FALSE) ? 1 : 0;
      return OK;
    }
  return FAIL;
}
