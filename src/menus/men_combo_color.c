/* Nsp
 * Copyright (C) 2001-2010 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/gtksci.h"

static GdkPixbuf *create_color_pixbuf_from_rgb (double pcol[]);
static GdkPixbuf *create_color_pixbuf (BCG *Xgc,int color_id);

/*
 * A combo box for choosing a color in a table 
 * i.e from a nsp graphic window colormap.
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


/**
 * nsp_gtkcombobox_colormap_new:
 * @Xgc: 
 * @init_color: 
 * 
 * returns a GtkComboBox which can be used to select 
 * a color in a grid. The colors are the colors of 
 * the colormap associated to the graphic context @Xgc.
 * If @init_color is equal to -1 then the current color 
 * of @Xgc is used as initial default value. If not then 
 * @init_color is the indice in the colormap of the color to use 
 * as default initial value (first indice is zero).
 * 
 * Return value: 
 **/

GtkWidget *nsp_gtkcombobox_colormap_new( BCG *Xgc,int init_color)
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
  if (init_color == -1 ) 
    init_color=  Xgc->graphic_engine->xget_pattern(Xgc);
  gtk_combo_box_set_active (GTK_COMBO_BOX (combo),Min(Max(init_color,0),n_colors-1));
  return combo;
}

/**
 * nsp_gtkcombobox_colormap_new_from_colormap:
 * @table: 
 * @init_color: 
 * 
 * returns a GtkComboBox which can be used to select 
 * a color in a grid. The colors are the colors of 
 * the colormap associated to the graphic context @Xgc.
 * If @init_color is equal to -1 then the current color 
 * of @Xgc is used as initial default value. If not then 
 * @init_color is the indice in the colormap of the color to use 
 * as default initial value. (first indice is zero).
 * 
 * Return value: 
 **/

GtkWidget *nsp_gtkcombobox_colormap_new_from_colormap(NspMatrix *table,int init_color)
{
  GtkWidget *combo;
  GtkTreeIter iter;
  GtkCellRenderer *cell = gtk_cell_renderer_pixbuf_new ();
  GtkListStore *store;
  int i;

  store = gtk_list_store_new (1, GDK_TYPE_PIXBUF);

  combo = gtk_combo_box_new_with_model (GTK_TREE_MODEL (store));
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combo),
			      cell, TRUE);
  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (combo),
				  cell, "pixbuf", 0, NULL);
  gtk_combo_box_set_wrap_width (GTK_COMBO_BOX (combo),
				10);
  
  for ( i = 0 ; i < table->m ; i++) 
    {
      double val[3]={ table->R[i],table->R[i+table->m],table->R[i+2*table->m]};
      GdkPixbuf *pixbuf = create_color_pixbuf_from_rgb(val);
      gtk_list_store_append (store, &iter);
      gtk_list_store_set (store, &iter, 0,pixbuf, -1);
    }

  g_object_unref (store);
  if (init_color != -1 ) 
    gtk_combo_box_set_active (GTK_COMBO_BOX (combo),Min(Max(init_color,0),table->m-1));
  return combo;
}


/**
 * gtkcombobox_select_color:
 * @Xgc: 
 * @init_color: 
 * 
 * creates and run a GtkDialog for selection of a color in a table.
 * The colors are the colors of the colormap associated to the 
 * graphic context @Xgc.
 * If @init_color is equal to -1 then the current color 
 * of @Xgc is used as initial default value. If not then 
 * @init_color is the indice in the colormap of the color to use 
 * as default initial value.
 * 
 * 
 * Return value: the color id or 0 if cancelled.
 **/

int gtkcombobox_select_color(BCG *Xgc,int init_color) 
{
  GtkWidget *window,*dvbox;
  GtkWidget *comboboxgrid;
  GtkWidget *frame, *vbox;
  gint result;
  int answer;

  start_sci_gtk(); /* be sure that gtk is started */

  window = gtk_dialog_new_with_buttons ("Choose color",
					NULL, 0,
					GTK_STOCK_OK, GTK_RESPONSE_OK,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					NULL);

  dvbox = GTK_DIALOG(window)->vbox;

  frame = gtk_frame_new ("Choose a color");
  gtk_box_pack_start (GTK_BOX(dvbox),frame, FALSE, FALSE, 0);
  
  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
  gtk_container_add (GTK_CONTAINER (frame), vbox);
  comboboxgrid = nsp_gtkcombobox_colormap_new(Xgc,init_color);
  gtk_box_pack_start (GTK_BOX (vbox), comboboxgrid, FALSE, FALSE, 0);
  
  gtk_widget_show_all (window);
  
  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	answer = gtk_combo_box_get_active (GTK_COMBO_BOX(comboboxgrid))+1;
	break;
      default:
	answer = 0;
	break;
    }
  gtk_widget_destroy (window);
  return answer;
}


/**
 * gtkcombobox_select_color_in_table:
 * @table: 
 * @init_color: 
 * 
 * creates and run a GtkDialog for selection of a color in a table.
 * The colors are the colors of the colormap associated to the 
 * graphic context @Xgc.
 * If @init_color is equal to -1 then the current color 
 * of @Xgc is used as initial default value. If not then 
 * @init_color is the indice in the colormap of the color to use 
 * as default initial value.
 * 
 * 
 * Return value: the color id or 0 if cancelled.
 **/

int gtkcombobox_select_color_in_table(NspMatrix *table,int init_color) 
{
  GtkWidget *window,*dvbox;
  GtkWidget *comboboxgrid;
  GtkWidget *frame, *vbox;
  gint result;
  int answer;

  start_sci_gtk(); /* be sure that gtk is started */

  window = gtk_dialog_new_with_buttons ("Choose color",
					NULL, 0,
					GTK_STOCK_OK, GTK_RESPONSE_OK,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					NULL);

  dvbox = GTK_DIALOG(window)->vbox;

  frame = gtk_frame_new ("Choose a color");
  gtk_box_pack_start (GTK_BOX(dvbox),frame, FALSE, FALSE, 0);
  
  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 5);
  gtk_container_add (GTK_CONTAINER (frame), vbox);
  comboboxgrid = nsp_gtkcombobox_colormap_new_from_colormap(table,init_color);
  gtk_box_pack_start (GTK_BOX (vbox), comboboxgrid, FALSE, FALSE, 0);
  
  gtk_widget_show_all (window);
  
  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	answer = gtk_combo_box_get_active (GTK_COMBO_BOX(comboboxgrid))+1;
	break;
      default:
	answer = 0;
	break;
    }
  gtk_widget_destroy (window);
  return answer;
}




/* create a pixbuf using color id color_id 
 * color from 1 to the number of colors of the colormap 
 * of Xgc;
 */

static GdkPixbuf *create_color_pixbuf (BCG *Xgc,int color_id)
{
  double val[3];
  int n_colors;
  Xgc->graphic_engine->xget_colormap(Xgc,&n_colors,val,Max(color_id,0));
  return create_color_pixbuf_from_rgb(val);
}

/* create a pixbuf using color id color_id 
 * color from 1 to the number of colors of the colormap 
 * of Xgc;
 */

static GdkPixbuf *create_color_pixbuf_from_rgb (double val[])
{
  guchar pcol[3];
  GdkPixbuf *pixbuf;
  int x,i, num, rowstride;
  guchar *pixels, *p;
  
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
