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
 * menus getfile 
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/gtksci.h"

/**
 * nsp_get_file_window:
 * @title: 
 * @dirname: 
 * @action: 
 * @file: 
 * @ierr: 
 * 
 * 
 * 
 * Return value: 
 **/

menu_answer nsp_get_file_window(const char *title,const char *dirname,int action,char **file)
{
  G_CONST_RETURN char *loc;
  char *file_p;
  int result, n_actions = 3, action_length=0,active=0;
  menu_answer rep = menu_fail;
  GtkWidget *combo=NULL,  *window;
  char *actions[]={"exec","load","chdir",NULL };

  start_sci_gtk(); /* be sure that gtk is started */
  window = gtk_file_selection_new (title);

  if ( dirname != NULL ) 
    {
      if ( strcmp(dirname,".") == 0) 
	gtk_file_selection_set_filename (GTK_FILE_SELECTION (window),"./");
      else 
	gtk_file_selection_set_filename (GTK_FILE_SELECTION (window),dirname);
    }
  gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);

  if ( action == TRUE ) 
    {
      /* add a menu with default actions ... 
       * this should be passed as arguments 
       */
      int i;
      combo = gtk_combo_box_new_text ();
      for (i = 0 ; i < n_actions ; i++) 
	gtk_combo_box_append_text (GTK_COMBO_BOX (combo),actions[i]);
      gtk_combo_box_set_active (GTK_COMBO_BOX(combo),0);
      gtk_box_pack_start (GTK_BOX(GTK_FILE_SELECTION(window)->action_area), 
			  combo, FALSE, FALSE, 0);
      gtk_widget_show (combo);
    }

  gtk_widget_show (window);
  result = gtk_dialog_run (GTK_DIALOG (window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK :
	loc = gtk_file_selection_get_filename(GTK_FILE_SELECTION(window));
	if ( action == TRUE ) 
	  {
	    active = gtk_combo_box_get_active(GTK_COMBO_BOX(combo));
	    action_length = strlen(actions[active])+3;
	  }
	/* useful for win32 \ in pathnames  */
	if ((file_p = nsp_string_protect(loc)) == NULL) break;
    	/* take care to keep synchronised with "%s('%s');" */
	if (( *file = new_nsp_string_n(strlen(file_p)+6+action_length)) == NULL) break;
	if ( action == TRUE ) 
	  sprintf(*file,"%s('%s');",actions[active],file_p);
	else 
	  strcpy(*file,file_p);
	nsp_string_destroy(&file_p);
    	rep = menu_ok;
	break;
      default:
	rep = menu_cancel;
	break;
    }
  gtk_widget_destroy (window);
  return rep;
}

/* specific widget for save or open 
 */

/**
 * nsp_get_filename_save:
 * @title: 
 * @dirname: 
 * 
 * returned value should be freed by g_free 
 * 
 * Return value: 
 **/

char * nsp_get_filename_save(const char *title,const char *dirname)
{
  char *filename = NULL;
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (title,
					NULL,
					GTK_FILE_CHOOSER_ACTION_SAVE,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					NULL);
  if ( dirname ) gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER (dialog),dirname);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    }
  gtk_widget_destroy (dialog);
  return filename ;
}

/**
 * nsp_get_filename_open:
 * @title: 
 * @dirname: 
 * @filters: 
 * @err: 
 * 
 * 
 * returned value should be freed by g_free 
 * 
 * Return value: 
 **/

char * nsp_get_filename_open(const char *title,const char *dirname,char **filters)
{
  char *filename=NULL;
  GtkWidget *dialog;

  dialog = gtk_file_chooser_dialog_new (title,
					NULL,
					GTK_FILE_CHOOSER_ACTION_OPEN,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
					NULL);
  if ( dirname ) gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER (dialog),dirname);
  if ( filters != NULL  ) 
    {
      GtkFileFilter* filter ;
      while ( *filters != NULL) 
	{
	  filter = gtk_file_filter_new();
	  gtk_file_filter_set_name (GTK_FILE_FILTER(filter),*filters);filters++;
	  gtk_file_filter_add_pattern(filter,*filters);filters++;
	  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog),filter);
	}
      filter = gtk_file_filter_new();
      gtk_file_filter_set_name (GTK_FILE_FILTER(filter),"all files");
      gtk_file_filter_add_pattern(filter,"*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog),filter);
    }
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    }
  gtk_widget_destroy (dialog);
  return filename;
}
