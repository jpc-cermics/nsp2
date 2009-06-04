/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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

menu_answer nsp_message(NspSMatrix *Message,NspSMatrix *Buttons,int *rep)
{
  menu_answer ans;
  char* buttons_def[] = { "gtk-close", NULL };
  nsp_string message =nsp_smatrix_elts_concat(Message,"\n",1,"\n",1);
  if ( message == NULL) return menu_fail;
  if ( Buttons == NULLSMAT) 
    ans = nsp_message_(message, buttons_def,1,rep);
  else 
    ans = nsp_message_(message,Buttons->S,Buttons->mn,rep);
  nsp_string_destroy(&message);
  return ans;
}

/*
 * Interface  for modeless message
 */

menu_answer nsp_message_modeless(NspSMatrix *Message,NspSMatrix *Buttons)
{
  menu_answer rep;
  nsp_string message =nsp_smatrix_elts_concat(Message,"\n",1,"\n",1);
  if ( message == NULL) return menu_fail;
  rep=nsp_message_modeless_(message);
  nsp_string_destroy(&message);
  return rep;
}

/*  
 * modeless message with just a close button 
 */  

menu_answer nsp_message_modeless_(const char *message)
{
  GtkWidget *dialog, *window=NULL;

  start_sci_gtk(); /* be sure that gtk is started */
  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
				   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
				   GTK_MESSAGE_INFO,
				   GTK_BUTTONS_CLOSE,
				   "%s",message);
  g_signal_connect (dialog, "response",  G_CALLBACK (gtk_widget_destroy),  NULL);
  gtk_widget_show (dialog);
  return menu_ok;
}

menu_answer nsp_message_(const char *message,char **buttons,int n_buttons,int *rep)
{
  int i;
  GtkWidget *dialog;
  GtkWidget *hbox;
  GtkWidget *stock;
  GtkWidget *label;
  GtkWidget *window=NULL;
  gint response;

  start_sci_gtk(); /* be sure that gtk is started */

  dialog = gtk_dialog_new_with_buttons ("Nsp Dialog",GTK_WINDOW (window),
					GTK_DIALOG_MODAL| GTK_DIALOG_DESTROY_WITH_PARENT,
					NULL);
  for ( i= 0 ; i <  n_buttons ; i++) 
    gtk_dialog_add_button(GTK_DIALOG (dialog),buttons[i],i);

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
  label = gtk_label_new (message);
  gtk_box_pack_start (GTK_BOX (hbox),label, TRUE, TRUE, 0);
  gtk_widget_show (label);
  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
  if ( response >= 0) 
    {
      *rep = response +1 ;
      return menu_ok;
    }
  else 
    {
      *rep = -1;
      return menu_cancel;
    }
}
