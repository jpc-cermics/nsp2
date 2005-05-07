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

int nsp_message(NspSMatrix *Message,NspSMatrix *Buttons,int *rep)
{
  static char* buttons_def[] = { "Ok", NULL };
  String *message =nsp_smatrix_elts_concat(Message,"\n",1,"\n",1);
  if ( message == NULL) return FAIL;
  if ( Buttons == NULLSMAT) 
    {
      *rep= nsp_message_(message,buttons_def,1);
    }
  else 
    {
      *rep= nsp_message_(message,Buttons->S,Buttons->mn);
    }
  StringDestroy(&message);
  return OK;
}

/*
 * Interface  for modeless message
 */

int nsp_message_modeless(NspSMatrix *Message,NspSMatrix *Buttons)
{
  String *message =nsp_smatrix_elts_concat(Message,"\n",1,"\n",1);
  if ( message == NULL) return FAIL;
  nsp_message_modeless_(message);
  return OK;
}


