/* -*-   Encoding: utf-8  -*-  */
/* Nsp
 * Copyright (C) 1998-2011 Jean-Philippe Chancelier Enpc/Cermics
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
#define KEEP_PACKAGE
#include <nsp/config.h>
#include "nsp-logo.xpm" 
#include "nsp-gw.xpm" 


void create_nsp_about_old(void)
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
  
  label = gtk_label_new ("Nsp \n  GPL Copyright 2004/2011");
  gtk_box_pack_start (GTK_BOX (box1), label , FALSE, TRUE, 5);

  gtk_widget_show_all (window);
  gtk_dialog_run(GTK_DIALOG(window));
  gtk_widget_destroy(window);
}

void create_nsp_about(void)
{
  GdkPixbuf *pixbuf, *transparent;
  
  const gchar *authors[] = {
    "J.Ph Chancelier",
    "Bruno Pinçon",
    "and many more for",
    "all library and interfaced tools",
    NULL
  };

  const gchar *documentors[] = {
    "J.Ph Chancelier",
    "F. Delebecque",
    "Bruno Pinçon",
    "and many more...",
    NULL
  };

  const gchar *license =
    "This library is free software; you can redistribute it and/or\n"
    "modify it under the terms of the GNU Library General Public License as\n"
    "published by the Free Software Foundation; either version 2 of the\n"
    "License, or (at your option) any later version.\n" 
    "\n"
    "This library is distributed in the hope that it will be useful,\n"
    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
    "Library General Public License for more details.\n"
    "\n"
    "You should have received a copy of the GNU Library General Public\n"
    "License along with the Gnome Library; see the file COPYING.LIB.  If not,\n"
    "write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,\n"
    "Boston, MA 02111-1307, USA.\n";
  
  pixbuf = NULL;
  transparent = NULL;
  pixbuf =gdk_pixbuf_new_from_xpm_data (nsp_logo_xpm);
  transparent = gdk_pixbuf_add_alpha (pixbuf, TRUE, 0xff, 0xff, 0xff);
  g_object_unref (pixbuf);
  /* gtk_about_dialog_set_email_hook (activate_email, NULL, NULL); */
  /* gtk_about_dialog_set_url_hook (activate_url, NULL, NULL);*/
  gtk_show_about_dialog (NULL,/*GTK_WINDOW (window),*/
			 "program-name", "Nsp",
			 "version", PACKAGE_VERSION,
			 "copyright", "(C) 2004-2011 The Nsp Team",
			 "license", license,
			 "website", "http://cermics.enpc.fr/~jpc/nsp-tiddly/",
			 "authors", authors,
			 "documenters", documentors,
			 "logo", transparent,
                         "title", "About Nsp",
			 NULL);

  g_object_unref (transparent);
}
