/* Nsp
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 * about
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gtk/gtk.h>

#include "nsp-logo.xpm" 
#include "nsp-gw.xpm" 

void create_nsp_about(void)
{
  GtkWidget *window = NULL;
  GtkWidget *box1;
  GtkWidget *label;
  GtkWidget *pixmapwid;
  GdkPixmap *pixmap;
  GdkBitmap *mask;

  window = gtk_dialog_new_with_buttons ("Nsp about",NULL, 0,
					GTK_STOCK_CLOSE, GTK_RESPONSE_ACCEPT,
					NULL);
  gtk_widget_realize(window);
  box1 = GTK_DIALOG(window)->vbox;
  pixmap = gdk_pixmap_create_from_xpm_d (window->window, &mask, NULL,
					 (gchar **) nsp_logo_xpm);
  pixmapwid =gtk_image_new_from_pixmap(pixmap, mask);
  g_object_unref(G_OBJECT(pixmap));
  g_object_unref(G_OBJECT(mask));
  /* XXXX : A garder ds un coin */ 

  pixmap = gdk_pixmap_create_from_xpm_d (window->window, &mask, NULL,
					 nsp_gw_xpm);
  gdk_window_set_icon (window->window, NULL,pixmap,pixmap);

  gtk_container_add (GTK_CONTAINER (box1), pixmapwid);
  
  label = gtk_label_new ("Nsp \n  GPL Copyright 2004/2010");
  gtk_box_pack_start (GTK_BOX (box1), label , FALSE, TRUE, 5);

  gtk_widget_show_all (window);
  gtk_dialog_run(GTK_DIALOG(window));
  gtk_widget_destroy(window);
}


