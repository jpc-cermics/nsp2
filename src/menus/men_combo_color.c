/* Nsp
 * Copyright (C) 2001-2005 Jean-Philippe Chancelier Enpc/Cermics
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
 * set of added function which use gtk widgets 
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h" 

/*
 * A combo box for choosing a color 
 * 
 * 
 * code extracted and modified from the gtk tests file 
 * testcombo.c
 * Copyright (C) 2003  Kristian Rietveld
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 */

/* create a pixbuf using color id color_id 
 * color from 1 to the number of colors of the colormap 
 * of Xgc;
 */

static GdkPixbuf *create_color_pixbuf (BCG *Xgc,int color_id)
{
  double val[3];
  GdkPixbuf *pixbuf;
  int x,i, num, rowstride,n_colors;
  guchar *pixels, *p,pcol[3];
  
  Xgc->graphic_engine->xget_colormap(Xgc,&n_colors,val,Max(color_id,0));
  for (i = 0; i < 3; i++) pcol[i]=val[i]*255;

  pixbuf = gdk_pixbuf_new (GDK_COLORSPACE_RGB,
			   FALSE, 8,
			   16, 16);

  rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  p = pixels = gdk_pixbuf_get_pixels (pixbuf);

  num = gdk_pixbuf_get_width (pixbuf) * gdk_pixbuf_get_height (pixbuf);

  for (x = 0; x < num; x++) 
    {
      for (i = 0; i < 3; i++) p[i]=pcol[i];
      p += 3;
    }
  return pixbuf;
}

GtkWidget *nsp_gtkcombo_colormap_new( BCG *Xgc)
{
  GtkWidget *combo;
  GtkTreeIter iter;
  GtkCellRenderer *cell = gtk_cell_renderer_pixbuf_new ();
  GtkListStore *store;
  int n_colors,i;

  store = gtk_list_store_new (1, GDK_TYPE_PIXBUF);

  combo = gtk_combo_box_new_with_model (GTK_TREE_MODEL (store));
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo),
			      cell, TRUE);
  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo),
				  cell, "pixbuf", 0, NULL);
  gtk_combo_box_set_wrap_width (GTK_COMBO_BOX (combo),
				10);
  /* get number of colors */
  Xgc->graphic_engine->xget_colormap(Xgc,&n_colors,NULL,0);
  
  for ( i = 0 ; i < n_colors ; i++) 
    {
      gtk_list_store_append (store, &iter);
      gtk_list_store_set (store, &iter,
			  0, create_color_pixbuf (Xgc,i+1),
			  -1);
    }

  g_object_unref (store);

  gtk_combo_box_set_active (GTK_COMBO_BOX (combo), 0);

  return combo;
}

typedef enum { COMBO_OK, COMBO_RESET } state;

int gtkcombo_select_color(BCG *Xgc) 
{
  GtkWidget *window,*mainbox;
  GtkWidget *comboboxgrid;
  GtkWidget *tmp, *boom;
  int answer = COMBO_RESET ;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_container_set_border_width (GTK_CONTAINER (window), 5);
  
  g_signal_connect (window, "destroy", gtk_main_quit, NULL);

  mainbox = gtk_vbox_new (FALSE, 2);
  gtk_container_add (GTK_CONTAINER (window), mainbox);

  /* GtkComboBox (grid mode) */
  tmp = gtk_frame_new ("GtkComboBox (grid mode)");
  gtk_box_pack_start (GTK_BOX (mainbox), tmp, FALSE, FALSE, 0);

  boom = gtk_vbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (boom), 5);
  gtk_container_add (GTK_CONTAINER (tmp), boom);

  comboboxgrid = nsp_gtkcombo_colormap_new(Xgc);

  gtk_box_pack_start (GTK_BOX (boom), comboboxgrid, FALSE, FALSE, 0);

  gtk_widget_show_all (window);
  gtk_main();

  gtk_widget_destroy(window);
  return answer ;
}

