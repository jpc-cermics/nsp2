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
 * menu madial
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/gtksci.h"

int nsp_matrix_dialog_(const char *title,char **Labels_v,char **Labels_h,char **Init, int nl,int nc, int *ierr);

int  nsp_matrix_dialog(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
			   NspSMatrix *Init_matrix,int *cancel)
{
  int rep,ierr=0;
  char *labels =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  if ( labels == NULL) return FAIL;
  rep =  nsp_matrix_dialog_(labels,Labels_v->S,Labels_h->S, Init_matrix->S,
			       Labels_v->mn, Labels_h->mn,&ierr);
  nsp_string_destroy(&labels);
  if ( ierr == 0) 
    {
      *cancel = ( rep == FALSE) ? 1 : 0;
      return OK;
    }
  return FAIL;
}

int nsp_matrix_dialog_(const char *title,char **Labels_v,char **Labels_h,char **Init, int nl,int nc, int *ierr)
{
  int i,j,result,answer = FALSE;
  GtkWidget *window, **entries, *table, *vbox; 

  start_sci_gtk(); /* be sure that gtk is started */
  window = gtk_dialog_new_with_buttons ("Nsp mdialog",NULL, 0,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OK, GTK_RESPONSE_OK,
					NULL);
  /*
   *    gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
   *    gtk_window_set_wmclass (GTK_WINDOW (window), "mdialog", "Nsp");
   */
  vbox = GTK_DIALOG(window)->vbox;

  nsp_dialogs_insert_title(title,vbox);

  /* Allocation of table of widgets */
  entries=(GtkWidget **)MALLOC(  (nc*nl)*sizeof(GtkWidget *));
  if ( entries == NULL )
    {
      *ierr=1; 
      return FALSE;
    } 

  /* XXXX faire un label a viewport */

  table = gtk_table_new (nl+1,nc+1, FALSE);
  gtk_table_set_homogeneous(GTK_TABLE(table),FALSE);
  gtk_widget_show (table);
  gtk_container_set_border_width (GTK_CONTAINER (table), 5);
  gtk_box_pack_start (GTK_BOX (vbox), table ,TRUE,TRUE,0);

  /* The first column : a set of labels */

  for (j=0 ; j < nl ; j++)
    {
      GtkWidget *label;
      label = gtk_label_new (Labels_v[j]);
      gtk_widget_show (label);
      gtk_table_attach (GTK_TABLE (table),label,0,1,j+1,j+2,0,0,5,0);
    }
  
  /* The other rows */
  
  for (i=0 ; i< nc ; i++) 
    {
      /* first a label */ 
      GtkWidget *label;
      label = gtk_label_new (Labels_h[i]);
      gtk_widget_show (label);
      gtk_table_attach (GTK_TABLE (table),label,i+1,i+2,0,1,0,0,0,2);
      for (j=0 ; j<nl ; j++)
	{
	  GtkWidget *entry;
	  entries[j + i*(nl)] = entry =  gtk_entry_new() ;
	  /* could be passed as parameter 0 for no restriction;*/
	  gtk_entry_set_max_length(GTK_ENTRY(entry),0);
	  gtk_entry_set_text (GTK_ENTRY(entry),Init[j+i*(nl)]);
	  gtk_widget_show (entry);
	  gtk_table_attach (GTK_TABLE (table),entry,i+1,i+2,j+1,j+2,GTK_EXPAND | GTK_FILL, GTK_FILL,0,0);
	}
    }

  gtk_widget_show_all (window);
  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	for (i=0; i < nc*nl  ; i++) 
	  {
	    char *loc;
	    char * text = gtk_editable_get_chars(GTK_EDITABLE(entries[i]),0,
						 GTK_ENTRY(entries[i])->text_length);
	    if ( text == NULL ||  (loc =new_nsp_string(text)) == NULLSTRING)
	      {
		*ierr=1;
		break;
	      }
	    /* XXXXX free Init before replacing */
	    Init[i] = loc ;
	  }
	answer=TRUE;
	break;
    }
  FREE(entries);
  gtk_widget_destroy(window);
  return answer;
}



/* new one with treeview 
 *
 */


static GtkWidget * nsp_matrix_create_tree_view(NspSMatrix *S); 
static int nsp_smatrix_from_model(  GtkTreeModel *model);

int nsp_matrix_dialog_i(const char *title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
			NspSMatrix *Init_matrix);

int  nsp_matrix_dialog_new(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
		       NspSMatrix *Init_matrix,int *cancel)
{
  int Rep;
  nsp_string descr =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  Rep = nsp_matrix_dialog_i(descr,Labels_v,Labels_h,Init_matrix);
  nsp_string_destroy(&descr);
  return OK;
}

int nsp_matrix_dialog_i(const char *title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
			NspSMatrix *M)
{
  GtkTreeModel *model;
  int result;
  int i,maxl;
  GtkWidget *window;
  GtkWidget *vbox;
  GtkWidget *scrolled_win;
  GtkWidget *list;

  start_sci_gtk(); /* be sure that gtk is started */

  window = gtk_dialog_new_with_buttons ("Nsp matrix", NULL, 0,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					NULL);

  vbox = GTK_DIALOG(window)->vbox;
  
  nsp_dialogs_insert_title(title,vbox);
  
  /* initialize */
  
  maxl = 0;
  for (i = 0; i < M->m ; i++) 
    {
      int maxj=0,j;
      for ( j=0 ; j < M->n ; j++)
	maxj = Max(maxj,strlen(M->S[i+M->m*j]));
      maxl = Max(maxl,maxj);
    }

  if ( maxl > 50 || M->n > 30 || M->m > 20 ) 
    {
      /* here we need a scrolled window */ 
      scrolled_win = gtk_scrolled_window_new (NULL, NULL);
      gtk_container_set_border_width (GTK_CONTAINER (scrolled_win), 1);
      gtk_widget_set_usize (scrolled_win,200,300);
      gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				      GTK_POLICY_AUTOMATIC,
				      GTK_POLICY_AUTOMATIC);
      list = nsp_matrix_create_tree_view(M);
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
      list = nsp_matrix_create_tree_view(M);
      gtk_container_add (GTK_CONTAINER (fvbox),list);
      gtk_widget_show(list);
    }
  gtk_widget_show_all (window);
  result = gtk_dialog_run(GTK_DIALOG(window));
  model  = gtk_tree_view_get_model (GTK_TREE_VIEW(list));
  nsp_smatrix_from_model( model);
  gtk_widget_destroy(window);
  return TRUE;
}

static GtkListStore* create_list_model(NspSMatrix *M)
{
  GType *types; 
  GtkListStore *store;
  GtkTreeIter iter;
  gint i=0;
  types = malloc(M->n*sizeof(GType *));
  if ( types == NULL) return NULL;
  for ( i= 0 ; i < M->n ; i++) types[i] = G_TYPE_STRING;
  store = gtk_list_store_newv(M->n,types);
  FREE(types);
  for ( i = 0 ; i < M->m ; i++)
    {
      int j;
      gtk_list_store_append (store, &iter);
      for ( j = 0 ; j < M->n ; j++)
	{
	  gtk_list_store_set (store, &iter,j,M->S[i+M->m*j],-1);
	}
    }
  return store;
}

static void
edited (GtkCellRendererText *cell,
	gchar               *path_string,
	gchar               *new_text,
	gpointer             data)
{
  GtkListStore *model = GTK_LIST_STORE (data);
  GtkTreeIter iter;
  GtkTreePath *path = gtk_tree_path_new_from_string (path_string);
  gint col=0;
  /* get the column with cell */
  gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, path);
  col = GPOINTER_TO_INT(g_object_get_data(G_OBJECT(cell),"column"));
  /* store in column col */
  gtk_list_store_set (GTK_LIST_STORE(model), &iter,col, new_text, -1);
  gtk_tree_path_free (path);
}

static GtkWidget * nsp_matrix_create_tree_view(NspSMatrix *S)
{
  int j;
  GValue value = { 0, };
  GtkWidget *tv;
  GtkTreeModel *model;
  GtkTreeViewColumn *col;
  GtkCellRenderer *rend;
  g_value_init (&value, G_TYPE_BOOLEAN);
  g_value_set_boolean (&value, 1);
  /* list model */
  model = GTK_TREE_MODEL(create_list_model (S));
  tv = gtk_tree_view_new_with_model (model);
  /* pour ne pas voir les headers */
  gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (tv),FALSE);
  for ( j = 0 ; j < S->n ; j++ )
    {
      rend = gtk_cell_renderer_text_new ();
      g_object_set_data(G_OBJECT(rend),"column",GINT_TO_POINTER(j));
      /* set editable to true and use edited callback */
      g_object_set_property (G_OBJECT(rend), "editable", &value);
      g_signal_connect (rend, "edited", G_CALLBACK (edited), model);
      col = gtk_tree_view_column_new_with_attributes ("Column 1", rend, "text", j,  NULL);
      gtk_tree_view_append_column (GTK_TREE_VIEW (tv), col);
    }
  g_value_unset(&value);
  return tv;
}

static int nsp_smatrix_from_model(  GtkTreeModel *model)
{
  GValue value = { 0, };
  GtkTreeIter iter;
  int ncol=gtk_tree_model_get_n_columns (GTK_TREE_MODEL (model));
  gboolean g = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (model),&iter);
  while ( g == TRUE)
    {
      int j;
      for ( j= 0 ; j < ncol ; j++)
	{
	  const gchar *str;
	  gtk_tree_model_get_value(GTK_TREE_MODEL (model),&iter,j,&value);
	  str = g_value_get_string(&value);
	  Sciprintf("(%d)=%s\n",j+1,str);
	  g_value_unset(&value);

	}
      g = gtk_tree_model_iter_next(GTK_TREE_MODEL (model),&iter);
    }
  return OK;
}


