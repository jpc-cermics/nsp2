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
 * menu madial
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/gtksci.h"

static GtkWidget *nsp_matrix_table(NspSMatrix *Labels_v,NspSMatrix *Labels_h, NspSMatrix *M,int entry_size);
static int nsp_matrix_table_get_values(GtkWidget *table,NspSMatrix *S);
static GtkWidget * nsp_matrix_create_tree_view(NspSMatrix *colnames,NspSMatrix *rownames,NspSMatrix *S);
static int nsp_smatrix_from_model(NspSMatrix *M, GtkTreeModel *model);
static menu_answer nsp_matrix_dialog_i(const char *title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
				       NspSMatrix *M,int entry_style,int entry_size);



menu_answer nsp_matrix_dialog(NspSMatrix *Title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
			      NspSMatrix *M,int menu_type,int entry_size)
{
  int rep;
  nsp_string descr =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1);
  rep = nsp_matrix_dialog_i(descr,Labels_v,Labels_h,M,menu_type,entry_size);
  nsp_string_destroy(&descr);
  return rep;
}

menu_answer nsp_matrix_dialog_i(const char *title,NspSMatrix *Labels_v,NspSMatrix *Labels_h,
				NspSMatrix *M,int entry_style,int entry_size)
{
  int answer = menu_ok;
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
					GTK_STOCK_OK, GTK_RESPONSE_OK,
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

  if ( maxl > 40 || M->n > 10 || M->m > 10 ) 
    {
      /* here we need a scrolled window */ 
      scrolled_win = gtk_scrolled_window_new (NULL, NULL);
      gtk_container_set_border_width (GTK_CONTAINER (scrolled_win), 1);
      gtk_widget_set_size_request (scrolled_win,400,300);
      gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				      GTK_POLICY_AUTOMATIC,
				      GTK_POLICY_AUTOMATIC);
      if ( entry_style ) 
	list = nsp_matrix_table(Labels_v,Labels_h,M,entry_size);
      else 
	list = nsp_matrix_create_tree_view(Labels_v,Labels_h,M);
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
      if ( entry_style == TRUE) 
	list = nsp_matrix_table(Labels_v,Labels_h,M,entry_size);
      else 
	list = nsp_matrix_create_tree_view(Labels_v,Labels_h,M);
      gtk_container_add (GTK_CONTAINER (fvbox),list);
      gtk_widget_show(list);
    }
  gtk_widget_show_all (window);
  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	answer = menu_ok;
	if ( entry_style == TRUE) 
	  {
	    if ( nsp_matrix_table_get_values(list,M)== FAIL) answer = menu_fail;
	  }
	else 
	  {
	    model  = gtk_tree_view_get_model (GTK_TREE_VIEW(list));
	    if (nsp_smatrix_from_model(M, model)== FAIL)  answer = menu_fail;
	  }
	break;
      default:
	answer = menu_cancel;
	break;
    }
  gtk_widget_destroy(window);
  return answer;
}


static GtkListStore* create_list_model(NspSMatrix *rownames,NspSMatrix *M)
{
  GType *types; 
  GtkListStore *store;
  GtkTreeIter iter;
  gint i=0, ncol = ( rownames != NULL) ? M->n + 1 :  M->n;
  types = malloc(ncol*sizeof(GType *));
  if ( types == NULL) return NULL;
  for ( i= 0 ; i < ncol ; i++) types[i] = G_TYPE_STRING;
  store = gtk_list_store_newv(ncol,types);
  FREE(types);
  for ( i = 0 ; i < M->m ; i++)
    {
      int j,jinit=0;
      gtk_list_store_append (store, &iter);
      if ( rownames != NULL )
	{
	  gtk_list_store_set (store, &iter,0,rownames->S[i],-1);
	  jinit=1;
	}
      for ( j = 0 ; j < M->n ; j++)
	{
	  gtk_list_store_set (store, &iter,j+jinit,M->S[i+M->m*j],-1);
	}
    }
  return store;
}

static void edited (GtkCellRendererText *cell,gchar *path_string,gchar *new_text,
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

static GtkWidget * nsp_matrix_create_tree_view(NspSMatrix *colnames,NspSMatrix *rownames,NspSMatrix *S)
{
  int j=0,jinit=0;
  GValue value = { 0, };
  GtkWidget *tv;
  GtkTreeModel *model;
  GtkTreeViewColumn *col;
  GtkCellRenderer *rend;
  /* list model */
  model = GTK_TREE_MODEL(create_list_model(rownames,S));
  tv = gtk_tree_view_new_with_model (model);

  if ( colnames == NULL) 
    {
      /* pour ne pas voir les headers */
      gtk_tree_view_set_headers_visible (GTK_TREE_VIEW (tv),FALSE);
    }
  /* a cell renderer for row names */
  if ( rownames != NULL) 
    {
      GValue value_c = { 0, };
      rend = gtk_cell_renderer_text_new ();
      g_object_set_data(G_OBJECT(rend),"column",GINT_TO_POINTER(j));
      g_value_init (&value_c, G_TYPE_STRING);
      g_value_set_string (&value_c,"gray");
      /* g_object_set_property (G_OBJECT(rend), "background",&value_c); */
      g_value_unset (&value_c);
      g_value_init (&value, G_TYPE_BOOLEAN);
      g_value_set_boolean (&value, 0);
      g_object_set_property (G_OBJECT(rend), "sensitive",&value);
      g_value_unset (&value);
      col = gtk_tree_view_column_new_with_attributes (NULL, rend, "text",0,  NULL);
      gtk_tree_view_append_column (GTK_TREE_VIEW (tv), col);
      jinit=1;
    }
  g_value_init (&value, G_TYPE_BOOLEAN);
  g_value_set_boolean (&value, 1);
  for ( j = 0 ; j < S->n ; j++ )
    {
      char *cname =( colnames != NULL) ? colnames->S[j] : "Col";
      rend = gtk_cell_renderer_text_new ();
      g_object_set_data(G_OBJECT(rend),"column",GINT_TO_POINTER(j+jinit));
      /* set editable to true and use edited callback */
      g_object_set_property (G_OBJECT(rend), "editable", &value);
      g_signal_connect (rend, "edited", G_CALLBACK (edited), model);
      col = gtk_tree_view_column_new_with_attributes (cname, rend, "text", j+jinit,  NULL);
      gtk_tree_view_append_column (GTK_TREE_VIEW (tv), col);
    }
  g_value_unset(&value);
  return tv;
}

static int nsp_smatrix_from_model(NspSMatrix *M, GtkTreeModel *model)
{
  int row=0;
  GValue value = { 0, };
  GtkTreeIter iter;
  int ncol=gtk_tree_model_get_n_columns (GTK_TREE_MODEL (model));
  gboolean g = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (model),&iter);
  while ( g == TRUE)
    {
      int j,j1=0;
      if (M->n < ncol ) 
	{
	  /* rownames were inserted in model */
	  j1=1;
	}
      if ( row >= M->m ) break; /* should not get there */
      for ( j= 0 ; j < M->n ; j++)
	{
	  const gchar *str;
	  gtk_tree_model_get_value(GTK_TREE_MODEL (model),&iter,j+j1,&value);
	  str = g_value_get_string(&value);
	  if ( strcmp(str,M->S[row+M->m*j]) != 0) 
	    {
	      char *s;
	      if ((s =nsp_string_copy(str)) == (nsp_string) 0) return FAIL;
	      nsp_string_destroy(&(M->S[row+M->m*j]));
	      M->S[row+M->m*j]= s;
	    }
	  g_value_unset(&value);
	}
      g = gtk_tree_model_iter_next(GTK_TREE_MODEL (model),&iter);
      row++;
    }
  return OK;
}


GtkWidget *nsp_matrix_table(NspSMatrix *Labels_v,NspSMatrix *Labels_h, NspSMatrix *M,  int entry_size)
{
  int i,j;
  GtkWidget *table;
  GtkWidget **entries;

  /* Allocation of table of widgets */
  if ((entries=(GtkWidget **)MALLOC(  (M->mn)*sizeof(GtkWidget *)))== NULL)
    return NULL;
  
  table = gtk_table_new (M->m+1,M->n+1, FALSE);
  g_object_set_data_full (G_OBJECT(table),"entries",entries, g_free);

  gtk_table_set_homogeneous(GTK_TABLE(table),FALSE);
  gtk_widget_show(table);
  gtk_container_set_border_width (GTK_CONTAINER (table), 5);
  
  if ( Labels_v != NULL) 
    {
      /* The first column : a set of labels */
      for (j=0 ; j < M->m ; j++)
	{
	  GtkWidget *label;
	  label = gtk_label_new (Labels_v->S[j]);
	  gtk_widget_show (label);
	  gtk_table_attach (GTK_TABLE (table),label,0,1,j+1,j+2,0,0,5,0);
	}
    }
  
  for (i=0 ; i< M->n ; i++) 
    {
      /* The first row */
      if ( Labels_h != NULL) 
	{
	  /* first a label */ 
	  GtkWidget *label;
	  label = gtk_label_new (Labels_h->S[i]);
	  gtk_widget_show (label);
	  gtk_table_attach (GTK_TABLE (table),label,i+1,i+2,0,1,0,0,0,2);
	}
      for (j=0 ; j< M->m ; j++)
	{
	  GtkWidget *entry;
	  entries[j + i*M->m] = entry =  gtk_entry_new() ;
	  /* could be passed as parameter 0 for no restriction;*/
	  gtk_entry_set_max_length(GTK_ENTRY(entry),0);
	  gtk_widget_set_size_request (entry,entry_size,-1);
	  gtk_entry_set_text (GTK_ENTRY(entry),M->S[j+i*M->m]);
	  gtk_widget_show (entry);
	  gtk_table_attach (GTK_TABLE (table),entry,i+1,i+2,j+1,j+2,GTK_EXPAND | GTK_FILL, GTK_FILL,0,0);
	}
    }
  return table;
}

static int nsp_matrix_table_get_values(GtkWidget *table,NspSMatrix *S)
{
  int i;
  GtkWidget **entries=  g_object_get_data(G_OBJECT(table),"entries");
  if ( entries == NULL) return FAIL;
  for (i=0; i < S->mn  ; i++) 
    {
      char *loc;
      char * text = gtk_editable_get_chars(GTK_EDITABLE(entries[i]),0,
					   GTK_ENTRY(entries[i])->text_length);
      if ( text == NULL ||  (loc =new_nsp_string(text)) == NULLSTRING)
	{
	  return FAIL;
	  break;
	}
      nsp_string_destroy(&(S->S[i]));
      S->S[i]= loc;
    }
  return OK;
}
