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
 * menu choose
 *--------------------------------------------------------------------------*/

#include <gtk/gtk.h>
#include "nsp/menus.h"
#include "nsp/gtksci.h"

static int num_item_selected = -1 ;
static GtkWidget *window = NULL; 

static void item_selected(GtkWidget *widget,  int selected)
{
  gtk_widget_destroy(window); 
  /* must be there since gtk_widget_destroy will change num_item_selected */
  num_item_selected = selected ;
  gtk_main_quit();
}

int nsp_choose_(char *choose_title,char **Items,int nItems,char **but_names, int n_but,int *choice)
{
  guint destroy_id;
  int i,maxl;
  GtkWidget *cbox;
  GtkWidget *vbox;
  GtkWidget *label;
  GtkWidget *scrolled_win;
  GtkWidget *list;
  GtkWidget *button;
  GtkWidget *separator;
  start_sci_gtk(); /* be sure that gtk is started */

  /* do not accept a reenter mode: FIXME */ 
  if ( window != NULL) return FALSE ; 
  /* initialize */
  num_item_selected = -1; 
  
  /* the window */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  destroy_id = gtk_signal_connect (GTK_OBJECT (window), "destroy",
				   GTK_SIGNAL_FUNC(item_selected),
				   GINT_TO_POINTER(-3));
  gtk_window_set_title (GTK_WINDOW (window), "Scilab Choose");
  gtk_container_set_border_width (GTK_CONTAINER (window),5);
  
  /* a vbox */
  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);

  /* label */

  cbox = gtk_hbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (cbox), 10);
  gtk_box_pack_start (GTK_BOX (vbox), cbox, FALSE, TRUE, 0);

  label = gtk_label_new (choose_title);
  gtk_box_pack_start (GTK_BOX (cbox), label, FALSE, FALSE, 0);

  /* the list */
  
  maxl = strlen(Items[0]);
  for (i = 0; i < nItems ; i++) maxl = Max(maxl,strlen(Items[i]));
  
  if ( maxl > 50 || nItems > 30) 
    {
      /* here we need a scrolled window */ 
      scrolled_win = gtk_scrolled_window_new (NULL, NULL);
      gtk_container_set_border_width (GTK_CONTAINER (scrolled_win), 1);
      gtk_widget_set_usize (scrolled_win,200,300);
      gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				      GTK_POLICY_AUTOMATIC,
				      GTK_POLICY_AUTOMATIC);
      list = gtk_list_new ();
      gtk_list_set_selection_mode (GTK_LIST (list), GTK_SELECTION_SINGLE);
      gtk_scrolled_window_add_with_viewport
	(GTK_SCROLLED_WINDOW (scrolled_win), list);
      gtk_container_set_focus_vadjustment
	(GTK_CONTAINER (list),
	 gtk_scrolled_window_get_vadjustment
	 (GTK_SCROLLED_WINDOW (scrolled_win)));
      gtk_container_set_focus_hadjustment
	(GTK_CONTAINER (list),
	 gtk_scrolled_window_get_hadjustment
	 (GTK_SCROLLED_WINDOW (scrolled_win)));
      gtk_widget_show(scrolled_win);
    }
  else 
    { 
      /* no need to add a viewport */
      GtkWidget *frame = gtk_frame_new(NULL);
      GtkWidget *fvbox  = gtk_vbox_new (FALSE, 0);
      gtk_box_pack_start (GTK_BOX (vbox),frame, TRUE, TRUE, 0);
      gtk_container_set_border_width (GTK_CONTAINER (frame),2);
      gtk_widget_show(frame);

      gtk_container_set_border_width (GTK_CONTAINER(fvbox),2);
      gtk_container_add (GTK_CONTAINER (frame),fvbox);
      gtk_widget_show(fvbox);
      list = gtk_list_new ();
      gtk_container_add (GTK_CONTAINER (fvbox),list);
      gtk_widget_show(list);
    }

  for (i = 0; i < nItems ; i++)
    {
      GtkWidget *item = gtk_list_item_new_with_label(Items[i]);
      gtk_signal_connect (GTK_OBJECT (item), "select",
			  GTK_SIGNAL_FUNC(item_selected),
			  GINT_TO_POINTER(i));
      gtk_container_add (GTK_CONTAINER (list), item);
    }
  
  separator = gtk_hseparator_new ();
  gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, TRUE, 0);
  cbox = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), cbox, FALSE, TRUE, 0);
  if ( strcmp(but_names[0],"OK")==0 || strcmp(but_names[0],"Ok")==0) 
    {
      button = gtk_button_new_from_stock (GTK_STOCK_OK);
    }
  else 
    button = gtk_button_new_with_label (but_names[0]);

  gtk_container_set_border_width (GTK_CONTAINER (button), 10);
  gtk_box_pack_start (GTK_BOX (cbox), button, TRUE, TRUE, 0);
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      GTK_SIGNAL_FUNC(item_selected),
		      GINT_TO_POINTER(-2));
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (button);
  gtk_widget_show_all (window);
  while ( 1) 
    {
      gtk_main();
      /* want to quit the gtk_main only when this 
       * list menu is achieved 
       */
      if ( num_item_selected != -1 ) break; 
    }
  window = NULL;
  *choice = num_item_selected;
  return (  *choice >= 0) ? TRUE : FALSE;
}
