/*-------------------------------------------------------------------
 * This Software is (Copyright ENPC 1998-2003) 
 * Jean-Philippe Chancelier Enpc/Cermics
 *-------------------------------------------------------------------*/

#include "nsp/menus.h"

/**********************************************************
 * Scilab Interface 
 **********************************************************/

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

/*************************************************     
 * Scilab Interface  for modeless message
 **********************************************************/

int nsp_message_modeless(NspSMatrix *Message,NspSMatrix *Buttons)
{
  String *message =nsp_smatrix_elts_concat(Message,"\n",1,"\n",1);
  if ( message == NULL) return FAIL;
  nsp_message_modeless_(message);
  return OK;
}


