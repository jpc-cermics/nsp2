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

#include <stdio.h>
#include <gtk/gtk.h>
#include "nsp/menus.h"
#include "nsp/gtksci.h"

/*---------------------------------------------------------------
 * data and callbacks for print and export menu  
 *---------------------------------------------------------------*/

typedef enum { pOK, pCANCEL , RESET } state; 

static void sci_mdialog_ok (GtkButton       *button, state * rep) 
{
  *rep = pOK;  gtk_main_quit();
} 

static void sci_mdialog_cancel (GtkButton       *button, state * rep) 
{
  *rep = pCANCEL;  gtk_main_quit();
}

/*---------------------------------------------------------------
 * mdialog window 
 * FIXME: pszName est reaffecté a la fin mais pas libéré 
 *        manque un free 
 *---------------------------------------------------------------*/ 

int nsp_multi_dialog_(const char *labels,char **pszTitle, char **pszName, int  nv, int  *ierr)
{
  int use_scrolled=0;
  int i;
  guint signals[3];
  GtkWidget *window = NULL;
  GtkWidget **entries; 
  state rep = RESET ;
  
  GtkWidget *table;
  GtkWidget *label;
  GtkWidget *button_ok;
  GtkWidget *button_cancel;
  GtkWidget *vbox;
  GtkWidget *hbbox;
  GtkWidget *scrolled_win=NULL;

  start_sci_gtk(); /* be sure that gtk is started */
  *ierr=0;
  rep =RESET;
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Scilab mdialog");

  gtk_window_set_title   (GTK_WINDOW (window),"Scilab dialog");
  gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
  gtk_window_set_wmclass  (GTK_WINDOW (window), "mdialog", "Scilab");

  signals[0]=gtk_signal_connect (GTK_OBJECT (window), "destroy",
				 GTK_SIGNAL_FUNC(sci_mdialog_cancel),
				 &rep);

  gtk_container_set_border_width (GTK_CONTAINER (window), 0);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 10);
  gtk_widget_show (vbox);

  if (( entries = MALLOC( nv*sizeof(GtkWidget *))) == NULL) 
    {
      *ierr=1;
      return(FALSE);
    }

  /* label widget description of the mdialog */
  label = gtk_label_new ( labels);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_widget_show (label);

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
      gtk_table_attach (GTK_TABLE (table),label,0,1,i,i+1,
			GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
			0,0);
      entries[i] = gtk_entry_new() ; 
      gtk_entry_set_text (GTK_ENTRY(entries[i]),  pszName[i]);
      gtk_widget_show (entries[i]);
      gtk_table_attach (GTK_TABLE (table), entries[i],1,2,i,i+1,
			GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
			0,0);
    }

  /* ok */ 

  hbbox = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (vbox), hbbox, FALSE, FALSE , 2);
  gtk_widget_show (hbbox);

  button_ok = gtk_button_new_with_label ("OK");
  gtk_container_add (GTK_CONTAINER (hbbox), button_ok);

  signals[1]=gtk_signal_connect (GTK_OBJECT (button_ok), "clicked",
				 GTK_SIGNAL_FUNC (sci_mdialog_ok),
				 &rep);

  GTK_WIDGET_SET_FLAGS (button_ok, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (button_ok);
  gtk_widget_show (button_ok);

  /* cancel */

  button_cancel = gtk_button_new_with_label ("Cancel");
  gtk_container_add (GTK_CONTAINER (hbbox), button_cancel);
  signals[2]=gtk_signal_connect (GTK_OBJECT (button_cancel), "clicked",
				 GTK_SIGNAL_FUNC (sci_mdialog_cancel),
				 &rep);
  GTK_WIDGET_SET_FLAGS (button_cancel, GTK_CAN_DEFAULT);
  gtk_widget_show (button_cancel);

  gtk_widget_show (window);

  while (1) 
    {
      /* here we only want to quit gtk_main after a selection in 
       * this menu XXXXX attention rajouter un test sur destroy 
       */
      gtk_main();
      if ( rep != RESET ) break;
    }

  if ( rep == pOK ) 
    {
      for (i=0; i < nv  ; i++) {
	char *loc;
	char * text = gtk_editable_get_chars(GTK_EDITABLE(entries[i]),0,
					     GTK_ENTRY(entries[i])->text_length);
	if ( text == NULL) { *ierr=1; return FALSE;}
	if ( (loc =new_nsp_string(text)) == NULLSTRING) { *ierr=1; return FALSE;}
	pszName[i] = loc ;
      }
    }
  FREE(entries);
  gtk_signal_disconnect(GTK_OBJECT (window),signals[0]);
  gtk_signal_disconnect(GTK_OBJECT (button_ok),signals[1]);
  gtk_signal_disconnect(GTK_OBJECT (button_cancel),signals[2]);
  gtk_widget_destroy(window);
  return (rep == pOK) ? TRUE : FALSE  ;
}

