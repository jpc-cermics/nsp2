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
 * menu message
 *
 * XXX: just one button is enough 
 *      if modeless buttons are useless ? 
 *      should be able to change the icon 
 * 
 * 
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/gtksci.h"


int nsp_message(NspSMatrix *Message,NspSMatrix *Buttons,int *rep)
{
  static char* buttons_def[] = { "Ok", NULL };
  nsp_string message =nsp_smatrix_elts_concat(Message,"\n",1,"\n",1);
  if ( message == NULL) return FAIL;
  if ( Buttons == NULLSMAT) 
    {
      *rep= nsp_message_(message,buttons_def,1);
    }
  else 
    {
      *rep= nsp_message_(message,Buttons->S,Buttons->mn);
    }
  nsp_string_destroy(&message);
  return OK;
}

/*
 * Interface  for modeless message
 */

int nsp_message_modeless(NspSMatrix *Message,NspSMatrix *Buttons)
{
  nsp_string message =nsp_smatrix_elts_concat(Message,"\n",1,"\n",1);
  if ( message == NULL) return FAIL;
  nsp_message_modeless_(message);
  return OK;
}

/*  
 * message with just an OK button 
 * but modeless.
 */  

int nsp_message_modeless_(char *message)
{
  GtkWidget *dialog, *window=NULL;
  char *msg_utf8;

  start_sci_gtk(); /* be sure that gtk is started */
  
  if ((msg_utf8= nsp_string_to_utf8(message)) == NULL) 
    return -1;

  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
				   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
				   GTK_MESSAGE_INFO,
				   GTK_BUTTONS_OK,
				   message);
  g_signal_connect (dialog, "response",  G_CALLBACK (gtk_widget_destroy),  NULL);
  gtk_widget_show (dialog);
  if ( msg_utf8 != message ) g_free (msg_utf8);
  return 1;
}



int nsp_message_(char *message,char **buttons,int n_buttons)
{
  GtkWidget *dialog;
  GtkWidget *hbox;
  GtkWidget *stock;
  GtkWidget *label;
  GtkWidget *window=NULL;
  gint response;
  char *ok_mess, *cancel_mess, *msg_utf8;

  start_sci_gtk(); /* be sure that gtk is started */
  if ((msg_utf8= nsp_string_to_utf8(message)) == NULL) 
    return -1;

  ok_mess = buttons[0];
  if ( strcasecmp(ok_mess,"Ok")==0 ) ok_mess = GTK_STOCK_OK; 

  switch ( n_buttons ) 
    {
    case 0: return 1 ; break;
    case 1 : 
      dialog = gtk_dialog_new_with_buttons ("Scilab Dialog", GTK_WINDOW (window),
					    GTK_DIALOG_MODAL| GTK_DIALOG_DESTROY_WITH_PARENT,
					    ok_mess, GTK_RESPONSE_OK,
					    NULL);
      break;
    case 2:
    default: 
      cancel_mess = buttons[1];
      if ( strcasecmp(cancel_mess,"Cancel")==0 ) cancel_mess = GTK_STOCK_CANCEL; 
      dialog = gtk_dialog_new_with_buttons ("Scilab Dialog",
					    GTK_WINDOW (window),
					    GTK_DIALOG_MODAL| GTK_DIALOG_DESTROY_WITH_PARENT,
					    ok_mess, GTK_RESPONSE_OK,
					    cancel_mess,  GTK_RESPONSE_CANCEL,
					    NULL);
      break;
    }

  hbox = gtk_hbox_new (FALSE, 8);
  gtk_container_set_border_width (GTK_CONTAINER (hbox), 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox, FALSE, FALSE, 0);
  gtk_widget_show (hbox);

  if ( n_buttons >= 2) 
    stock = gtk_image_new_from_stock (GTK_STOCK_DIALOG_QUESTION, GTK_ICON_SIZE_DIALOG);
  else
    stock = gtk_image_new_from_stock (GTK_STOCK_DIALOG_INFO, GTK_ICON_SIZE_DIALOG);
  gtk_box_pack_start (GTK_BOX (hbox), stock, FALSE, FALSE, 0);
  gtk_widget_show (stock);
  label = gtk_label_new (msg_utf8);
  gtk_box_pack_start (GTK_BOX (hbox),label, TRUE, TRUE, 0);
  gtk_widget_show (label);
  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
  if ( msg_utf8 != message ) g_free (msg_utf8);
  if (response == GTK_RESPONSE_OK)
    return 1; 
  else 
    return 2; 
}
