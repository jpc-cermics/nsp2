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
					 nsp_logo_xpm);
  pixmapwid = gtk_pixmap_new (pixmap, mask);
  gdk_pixmap_unref (pixmap);
  gdk_pixmap_unref (mask);

  /* XXXX : A garder ds un coin */ 

  pixmap = gdk_pixmap_create_from_xpm_d (window->window, &mask, NULL,
					 nsp_gw_xpm);
  gdk_window_set_icon (window->window, NULL,pixmap,pixmap);

  gtk_container_add (GTK_CONTAINER (box1), pixmapwid);
  
  label = gtk_label_new ("Nsp \n  GPL Copyright 2004 Cermics/Enpc");
  gtk_box_pack_start (GTK_BOX (box1), label , FALSE, TRUE, 5);

  gtk_widget_show_all (window);
  gtk_dialog_run(GTK_DIALOG(window));
  gtk_widget_destroy(window);
}


