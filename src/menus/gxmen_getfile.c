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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <gtk/gtk.h>

#include "nsp/menus.h"
#include "nsp/gtksci.h"

char * nsp_get_filename_save(const char *title,const char *dirname);
char * nsp_get_filename_open(const char *title,const char *dirname,char **filters);

/*--------------------------------------------------------------
 * Gtk version for file selection 
 *--------------------------------------------------------------*/

/* FIXME */
extern char *sci_convert_to_utf8(char *str, int *alloc);

typedef enum { GETF_OK, CANCEL , DESTROY, RESET } state; 

static void file_selection_ok (GtkWidget  *w,  state *rep)
{
  *rep = GETF_OK;
  gtk_main_quit();
}

static void file_selection_destroy (GtkWidget  *w,  state *rep)
{
  *rep = DESTROY;
  gtk_main_quit();
}

static void file_selection_cancel (GtkWidget *w,  state *rep)
{
  *rep = CANCEL;
  gtk_main_quit();
}


/* XXXX reste a rajouter un bouton home et un bouton SCI 
 * 
 */

int  nsp_get_file_window(char *filemask,char **file,char *dirname,
			 int flag,int action,int *ierr,char *title)
{
  char *title_utf8;
  int title_alloc;
  int last_choice = 0;
  GList *cbitems = NULL;
  GtkWidget *combo=NULL;
  static int n_actions = 3 ;
  static char *actions[]={"exec","load","chdir",NULL };
  guint signals[3];
  static state rep;
  GtkWidget *window;
  rep =RESET ;

  start_sci_gtk(); /* be sure that gtk is started */

  title_utf8= sci_convert_to_utf8(title,&title_alloc);
  window = gtk_file_selection_new (title_utf8);
  if ( title_alloc == TRUE) g_free (title_utf8);
  if ( flag == 1) 
    {
      if ( strcmp(dirname,".") == 0) 
	gtk_file_selection_set_filename (GTK_FILE_SELECTION (window),"./");
      else 
	gtk_file_selection_set_filename (GTK_FILE_SELECTION (window),dirname);
    }
  gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);

  if ( action == 1 ) 
    {
      int j;
      for (j = 0; j < n_actions  ; ++j) 
	cbitems = g_list_append(cbitems, actions[j]);
      combo =  gtk_combo_new ();
      gtk_combo_set_popdown_strings (GTK_COMBO (combo), cbitems);
      gtk_entry_set_text (GTK_ENTRY (GTK_COMBO(combo)->entry),
			  actions[last_choice]);
      gtk_entry_set_editable(GTK_ENTRY (GTK_COMBO(combo)->entry),FALSE);
      
      gtk_box_pack_start (GTK_BOX (GTK_FILE_SELECTION (window)->action_area), 
			  combo, FALSE, FALSE, 0);
      gtk_widget_show (combo);
    }

  signals[0]=gtk_signal_connect (GTK_OBJECT (window), "destroy",
				 GTK_SIGNAL_FUNC(file_selection_destroy),
				 &rep);

  signals[1]=gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (window)->ok_button),
				 "clicked", GTK_SIGNAL_FUNC(file_selection_ok),
				 &rep);
  
  signals[2]=gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (window)->cancel_button),
				 "clicked", GTK_SIGNAL_FUNC(file_selection_cancel),
				 &rep);
  gtk_widget_show (window);
  while (1) 
    {
      gtk_main();
      /* want to quit the gtk_main only when this getfile is achieved 
       */
      if ( rep != RESET ) break;
    }
  if ( rep == GETF_OK ) 
    {
      int action_length=0;
      G_CONST_RETURN char *loc = gtk_file_selection_get_filename(GTK_FILE_SELECTION(window));
      if ( action == 1 ) 
	{
	  int j;
	  G_CONST_RETURN gchar *entry_text;
	  entry_text = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(combo)->entry));
	  for (j = 0; j < n_actions ; ++j) 
	    { 
	      if ( strcmp(entry_text, actions[j]) == 0) 
		{
		  last_choice = j ;
		  action_length = strlen(actions[j])+3;
		  break;
		}
	    }
	}
      /* take care to keep synchronised with "%s('%s');" */
      if (( *file = (char *) MALLOC((strlen(loc)+6+action_length)*sizeof(char))) == NULL) 
	{
	  Scierror("Malloc: running out of memory");
	  *ierr = 1;
	}
      else 
	{ 
	  if ( action == 1) 
	    sprintf(*file,"%s('%s');",actions[last_choice],loc);
	  else 
	    strcpy(*file,loc);
	  *ierr= 0;
	}

    }
  /* since here we are no more in a gtk_main we must disconnect signals 
   * before destroying widget window 
   */
  if ( rep != DESTROY ) 
    {
      gtk_signal_disconnect(GTK_OBJECT(window),signals[0]);
      gtk_signal_disconnect(GTK_OBJECT (GTK_FILE_SELECTION (window)->ok_button),signals[1]);
      gtk_signal_disconnect(GTK_OBJECT (GTK_FILE_SELECTION (window)->cancel_button),signals[2]);
      gtk_widget_destroy(window);
    }
#if 0
  {
    char *res ; 
    char *filters[]={"eps","*.eps","pdf","*.pdf","both","*.pdf,*.eps",NULL};
    res= nsp_get_filename_save("Save file","/tmp",filters);
    Sciprintf("filename = %s\n",res);g_free(res);
    res = nsp_get_filename_open("Open","/tmp",filters);
    Sciprintf("filename = %s\n",res);g_free(res);
  }
#endif

  return (rep == GETF_OK) ? TRUE : FALSE ; 
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
