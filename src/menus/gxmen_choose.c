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
 * menu choose
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/gtksci.h"

static GtkWidget * nsp_choose_create_tree_view(char **Items,int nItems);
static gboolean button_press_event (GtkWidget *widget, GdkEventButton *event,GtkWidget *dialog);

menu_answer nsp_choose(NspSMatrix *Items,NspSMatrix *Title,NspSMatrix *button,int *nrep)
{
  char *button_def[]={"gtk-cancel",NULL}, **but_names; 
  int choice=0 ;
  menu_answer rep;
  nsp_string descr =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  but_names = (button == NULL) ?  button_def : button->S  ; 
  rep = nsp_choose_(descr,Items->S,Items->mn,but_names,1,&choice);
  *nrep= ( rep == menu_ok ) ? (1+ choice) : 0;
  nsp_string_destroy(&descr);
  return rep;
}

menu_answer nsp_choose_(const char *title,char **Items,int nItems,char **but_names, 
			int n_but,int *choice)
{
  int result, i,maxl;
  GtkWidget *window;
  GtkWidget *vbox;
  GtkWidget *scrolled_win;
  GtkWidget *list;

  start_sci_gtk(); /* be sure that gtk is started */

  if ( n_but != 0 )
    {
      window = gtk_dialog_new_with_buttons ("Nsp choose",NULL, 0, NULL);
      gtk_dialog_add_button(GTK_DIALOG(window),but_names[0],GTK_RESPONSE_CANCEL);
    }
  else
    {
      window = gtk_dialog_new_with_buttons ("Nsp choose",
					    NULL, 0,
					    GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					    NULL);
    }

  vbox = GTK_DIALOG(window)->vbox;
  
  nsp_dialogs_insert_title(title,vbox);
  
  /* initialize */
  
  maxl = (nItems != 0) ? strlen(Items[0]) : 10;
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
      list = nsp_choose_create_tree_view(Items,nItems);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW (scrolled_win), list);
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
      list = nsp_choose_create_tree_view(Items,nItems);
      gtk_container_add (GTK_CONTAINER (fvbox),list);
      gtk_widget_show(list);
    }
  *choice = -1;
  /* choice is set up in button_press_event through the 
   * "chooose" field of G_OBJECT(window)
   */
  g_object_set_data(G_OBJECT(window),"choose",choice);
  g_signal_connect (list, "button_press_event", 
		    G_CALLBACK (button_press_event),window);
  
  gtk_widget_show_all (window);
  result = gtk_dialog_run(GTK_DIALOG(window));
  gtk_widget_destroy(window);
  return (  *choice >= 0) ? menu_ok : menu_cancel ;
}

static GtkTreeModel*create_list_model (char **Items,int nItems)
{
  GtkListStore *store;
  GtkTreeIter iter;
  gint i=0;
  store= gtk_list_store_new (1,G_TYPE_STRING);
  for ( i = 0 ; i < nItems ; i++)
    {
      gtk_list_store_append (store, &iter);
      gtk_list_store_set (store, &iter, 0,Items[i],-1);
    }
  return GTK_TREE_MODEL (store);
}

static gboolean
button_press_event (GtkWidget *widget, GdkEventButton *event,GtkWidget *dialog)
{
  int *data  = g_object_get_data(G_OBJECT(dialog),"choose");
  /* Deselect if people click outside any row. */
  if (event->window == gtk_tree_view_get_bin_window (GTK_TREE_VIEW (widget)))
    {
      GtkTreePath *path;
      int rep =gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (widget),
					      event->x, event->y,&path,NULL,
					      NULL, NULL);
      if ( rep == TRUE )
	{
	  gint*indi= gtk_tree_path_get_indices(path);
	  *data=*indi;
	  gtk_dialog_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
	  gtk_tree_path_free(path);
	}
    }
  /* Let the default code run in any case; it won't reselect anything. */
  return FALSE;
}

static GtkWidget * nsp_choose_create_tree_view(char **Items,int nItems)
{
  GtkWidget *tv;
  GtkTreeModel *model;
  GtkTreeViewColumn *col;
  GtkCellRenderer *rend;

  /* list model */
  model = create_list_model (Items,nItems);
  tv = gtk_tree_view_new_with_model (model);
  /* pour ne pas voir les headers */
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (tv),FALSE);
  rend = gtk_cell_renderer_text_new ();
  col = gtk_tree_view_column_new_with_attributes ("Column 1", rend, "text", 0,  NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (tv), col);
  return tv;
}












