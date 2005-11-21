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
 * menu ;
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"

static char *button_def[]={"gtk-cancel",NULL};

int nsp_choose(NspSMatrix *Items,NspSMatrix *Title,NspSMatrix *button,int *nrep)
{
  int Rep,choice=0 ;
  char **but_names; 
  nsp_string descr =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  but_names = (button == NULL) ?  button_def : button->S  ; 
  Rep = nsp_choose_(descr,Items->S,Items->mn,but_names,1,&choice);
  *nrep= ( Rep == TRUE ) ? (1+ choice) : 0;
  nsp_string_destroy(&descr);
  return OK;
}




