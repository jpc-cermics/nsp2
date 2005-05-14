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
 * menu dialog 
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"

/*
 * dialog 
 * returns FAIL if the dialog was canceled 
 * returns OK   if the dialog was done 
 *     The the answer is in Rep 
 *     except if lack of memory happens, then Rep == NULLOBJ;
 */

int nsp_dialog(NspSMatrix *Title,NspSMatrix *Init,NspObject **Rep)
{
  NspSMatrix *S;
  char *answer = NULL;
  static char *buttons[]={"OK","Cancel",NULL};
  int rep,ierr=0 ;
  nsp_string title =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  nsp_string init =nsp_smatrix_elts_concat(Init,"\n",1,"\n",1);
  rep = nsp_dialog_(title,init, buttons,&ierr,&answer );
 nsp_string_destroy(&init);
 nsp_string_destroy(&title);
  if ( rep == FALSE)   return FAIL;
  /* \n must be converted */ 
  S =nsp_smatrix_split(answer,"\n");
  if ( S != NULLSMAT ) { S->m = S->n ; S->n=1;} /* column vector */
  *Rep = (NspObject *) S;
  FREE(answer);/** allocated in DialogOK **/
  return OK;
}

/*
 * to open a dialog in a procedure 
 */

void nsp_dialog1(char *title, char *init, char **buttons,char *value,int *ok)
{
  char *answer = NULL;
  int rep,ierr=0;
  rep = nsp_dialog_(title,init,buttons,&ierr,&answer );
  if (rep == FALSE )
    *ok=0;
  else {
    *ok=1;
    strcpy(value,answer);
    FREE(answer);/** allocated in DialogOK **/
  }
}

