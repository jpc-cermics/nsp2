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
 * menu mdial
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/gtksci.h"

menu_answer nsp_multi_dialog(NspSMatrix *Title,NspSMatrix *Labels,NspSMatrix *Init_values)
{
  menu_answer rep;
  char *labels  =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1); 
  int nv =  Init_values->mn;
  rep =  nsp_multi_dialog_(labels,Labels->S,Init_values->S, nv);
  nsp_string_destroy(&labels); /* works even if labels is null */
  return rep;
}

/* XXX : pszName[i] must be freed before replacement 
 * pszTitle and pszName must have the same size and be NULL terminated arrays
 *
 */

menu_answer nsp_multi_dialog_(const char *title,char **pszTitle, char **pszName,int nv)
{
  int use_scrolled=0, i , result;
  menu_answer answer = FALSE;
  GtkWidget *window = NULL;
  GtkWidget **entries; 
  GtkWidget *table;
  GtkWidget *label;
  GtkWidget *vbox;
  GtkWidget *scrolled_win=NULL;

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

  if (( entries = MALLOC( nv*sizeof(GtkWidget *))) == NULL)  
    {
      return menu_fail;
    }

  /* table widget  of the mdialog */

  if (  nv > 15 ) use_scrolled = 1;

  if ( use_scrolled == 1) 
    {
      scrolled_win = gtk_scrolled_window_new (NULL, NULL);
      gtk_container_set_border_width (GTK_CONTAINER (scrolled_win), 1);
      gtk_widget_set_usize (scrolled_win,300,300);
      gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				      GTK_POLICY_AUTOMATIC,
				      GTK_POLICY_AUTOMATIC);
    }

  table = gtk_table_new (nv, 2, TRUE);
  gtk_table_set_homogeneous(GTK_TABLE(table),FALSE);
  gtk_widget_show (table);

  if ( use_scrolled == 1) 
    {
      gtk_scrolled_window_add_with_viewport
	(GTK_SCROLLED_WINDOW (scrolled_win), table);
      gtk_widget_show(scrolled_win);  
    }
  else 
    gtk_box_pack_start (GTK_BOX (vbox), table , TRUE, TRUE , 0);

  gtk_container_set_border_width (GTK_CONTAINER (table), 5);
  
  for ( i = 0 ; i < nv ; i++) 
    {
      label = gtk_label_new (pszTitle[i]);
      gtk_widget_show (label);
      gtk_table_attach(GTK_TABLE (table),label,0,1,i,i+1,0,0,5,5);
      entries[i] = gtk_entry_new() ; 
      gtk_entry_set_text (GTK_ENTRY(entries[i]),  pszName[i]);
      gtk_widget_show (entries[i]);
      gtk_table_attach (GTK_TABLE (table), entries[i],1,2,i,i+1,
			GTK_EXPAND | GTK_FILL, GTK_FILL,0,0);
    }

  gtk_widget_show_all (window);
  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	answer = TRUE;	
	for (i=0; i < nv  ; i++) {
	  char *loc;
	  char * text = gtk_editable_get_chars(GTK_EDITABLE(entries[i]),0,
					       GTK_ENTRY(entries[i])->text_length);
	  if ( text == NULL) { return menu_fail;}
	  if ( (loc =new_nsp_string(text)) == NULLSTRING) { return menu_fail;}
	  pszName[i] = loc ;
	}
	answer = menu_ok;
	break;
      default:
	answer = menu_cancel;
	break;
    }
  FREE(entries);
  gtk_widget_destroy(window);
  return answer;
}


