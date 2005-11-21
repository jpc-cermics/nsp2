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

  if ( title[0] != '\0' )
    {
      GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
      gtk_box_pack_start (GTK_BOX (vbox),hbox, FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (hbox),
			  gtk_image_new_from_stock (GTK_STOCK_DIALOG_QUESTION,
						    GTK_ICON_SIZE_DIALOG),
			  TRUE, TRUE, 0);  
      gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new (title), FALSE, FALSE, 0);
    }


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
