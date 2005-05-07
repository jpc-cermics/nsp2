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
 * gtk menus.
 * simple dialog with a gtk_tex_view
 *--------------------------------------------------------------------------*/

#include <gtk/gtk.h>
#include "nsp/menus.h"
#include "nsp/gtksci.h"
 
typedef enum { DIAL_OK, CANCEL , RESET } state; 

typedef struct scigtk_dialog_ { 
  char *txt;
  state st;
  GtkWidget *text;
  GtkWidget *window;
} scigtk_dialog;

/* ok handler */

static void sci_dialog_ok(GtkWidget *widget,scigtk_dialog *answer)
{
  GtkTextBuffer *buffer;
  GtkTextIter start, end;
  
  buffer = g_object_get_data (G_OBJECT (answer->window), "buffer");
  if ( buffer != NULL )
    gtk_text_buffer_get_bounds (buffer, &start, &end);
  answer->txt  = gtk_text_iter_get_text (&start, &end);
  if ( answer->txt != NULL ) 
    {
      int ind = strlen(answer->txt) - 1 ;
      if ( answer->txt[ind] == '\n') answer->txt[ind] = '\0' ;
    }
  gtk_widget_destroy(answer->window);
  /* this must be here since gtk_widget_destroy will also change answer->st */
  answer->st =  ( answer->txt != NULL) ? DIAL_OK : CANCEL;
  gtk_main_quit();
}

/* cancel and destroy handlers  */

static void sci_dialog_cancel(GtkWidget *widget,scigtk_dialog *answer)
{
  answer->st = CANCEL;
  gtk_widget_destroy(answer->window);
  gtk_main_quit();
}

/* the main function */

int nsp_dialog_(char *Title, char * init_value, char **button_name , int * ierr ,char **dialog_str )
{
  GtkTextIter iter;
  GtkWidget *text;
  GtkTextBuffer *buffer;
  GtkWidget *window = NULL;
  GtkWidget *vbox;
  GtkWidget *hbbox;
  GtkWidget *button_ok,*button_cancel;
  GtkWidget *separator;
  GtkWidget *scrolled_window;
  GtkWidget *label;

  static scigtk_dialog answer = {NULL, RESET , NULL,NULL};

  start_sci_gtk(); /* be sure that gtk is started */

  answer.window = window  = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window),"Scilab dialog");
  gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
  gtk_window_set_wmclass (GTK_WINDOW (window), "dialog", "Scilab");

  gtk_signal_connect (GTK_OBJECT (window), "destroy",
		      GTK_SIGNAL_FUNC(sci_dialog_cancel),
		      &answer);

  gtk_container_set_border_width (GTK_CONTAINER (window), 0);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 10);
  gtk_widget_show (vbox);

  /* label widget description of the dialog */
  label = gtk_label_new (Title);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_widget_show (label);

  /* A scrolled window which will contain the dialog text to be edited */
  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolled_window);
  
  buffer = gtk_text_buffer_new (NULL);
  gtk_text_buffer_get_iter_at_offset (buffer, &iter, 0);
  gtk_text_buffer_insert (buffer, &iter,init_value, -1);

  answer.text = text = gtk_text_view_new_with_buffer (buffer);
  g_object_unref (buffer);

  gtk_container_add (GTK_CONTAINER (scrolled_window), text);
  /* attach the buffer to window */
  g_object_set_data (G_OBJECT (window), "buffer", buffer);
  gtk_widget_grab_focus (text);
  gtk_widget_show (text);
  /* separator */

  separator = gtk_hseparator_new ();
  gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, FALSE, 0);
  gtk_widget_show (separator);

  /* ok and cancel buttons at the bottom */

  hbbox = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (vbox), hbbox, FALSE, FALSE , 2);
  gtk_widget_show (hbbox);

  if ( strcmp(button_name[0],"OK")==0)
    {
#ifdef USE_GNOME
      button_ok = gnome_stock_button (GNOME_STOCK_BUTTON_OK);
#else
      button_ok = gtk_button_new_with_label ("OK");
#endif
    }
  else 
    button_ok = gtk_button_new_with_label (button_name[0]);
  gtk_container_add (GTK_CONTAINER (hbbox), button_ok);
  gtk_signal_connect (GTK_OBJECT (button_ok), "clicked",
				 GTK_SIGNAL_FUNC(sci_dialog_ok),
				 &answer);
  GTK_WIDGET_SET_FLAGS (button_ok, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (button_ok);
  gtk_widget_show (button_ok);

  if ( strcmp(button_name[1],"Cancel")==0)
    {
#ifdef USE_GNOME
      button_cancel = gnome_stock_button (GNOME_STOCK_BUTTON_CANCEL);
#else
      button_cancel = gtk_button_new_with_label ("Cancel");
#endif
    }
  else 
    button_cancel = gtk_button_new_with_label (button_name[1]);
  gtk_container_add (GTK_CONTAINER (hbbox), button_cancel);
  GTK_WIDGET_SET_FLAGS (button_cancel, GTK_CAN_DEFAULT);
  gtk_signal_connect (GTK_OBJECT (button_cancel), "clicked",
				  GTK_SIGNAL_FUNC(sci_dialog_cancel),
				  &answer);
  gtk_widget_show (button_cancel);
  gtk_widget_show (window);
  while ( 1) 
    {
      gtk_main();
      /* want to quit the gtk_main only when this 
       * list menu is achieved 
       */
      if ( answer.st != RESET ) break;
    }
  *dialog_str = answer.txt ;
  return (answer.st == DIAL_OK ) ? TRUE : FALSE ;
}




