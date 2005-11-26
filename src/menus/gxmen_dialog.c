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
 * gtk menus.
 * simple dialog with a gtk_tex_view
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/gtksci.h"

static menu_answer nsp_dialog_(const char *title,const char *init_value,char **answer);

/**
 * nsp_dialog:
 * @title: 
 * @init: 
 * @answer: 
 * 
 * returns %menu_cancel if the dialog was canceled, %menu_fail if an internal 
 * fail was raised or  %menu_ok if the dialog was done.
 * If the return value is %menu_ok, the entered string is in object Rep (Rep may be NULL 
 * in case of memory failure).
 * 
 * Return value: %menu_ok, %menu_cancel, or %menu_fail
 **/

menu_answer nsp_dialog(NspSMatrix *title,NspSMatrix *init,NspObject **answer)
{
  NspSMatrix *S;
  char *text_answer = NULL;
  int rep =menu_cancel ;
  nsp_string str_title =nsp_smatrix_elts_concat(title,"\n",1,"\n",1);
  nsp_string str_init =nsp_smatrix_elts_concat(init,"\n",1,"\n",1);
  /* answer is allocated and must be freed here  */
  rep = nsp_dialog_(str_title,str_init,&text_answer);
  nsp_string_destroy(&str_init);
  nsp_string_destroy(&str_title);
  switch (rep ) 
    {
    case menu_fail : return rep;
    case menu_cancel : return rep;
    case menu_ok: 
      /* \n must be converted */
      S = nsp_smatrix_split(text_answer,"\n",0);
      FREE(text_answer);
      if ( S != NULLSMAT ) { S->m = S->n ; S->n=1;} /* column vector */
      *answer = (NspObject *) S; 
      return rep;
    }
  return rep;
}

/* useless */

menu_answer nsp_dialog1(const char *title,const char *init,char **answer)
{
  /* answer is allocated and must be freed here  */
  return nsp_dialog_(title,init,answer);
}


/**
 * nsp_dialog_:
 * @title: 
 * @init_value: 
 * @answer: 
 * 
 * reprendre but_names qui est lilité a deux ? 
 * attention answer est allouée 
 * 
 * Return value: 
 **/

static menu_answer nsp_dialog_(const char *title,const char *init_value,char **answer)
{
  char *text_answer;
  GtkWidget *window, *vbox, *scrolled_window, *text;
  GtkTextIter iter, start, end;
  GtkTextBuffer *buffer;
  menu_answer result = menu_cancel;

  start_sci_gtk(); /* be sure that gtk is started */

  window = gtk_dialog_new_with_buttons ("Nsp dialog",NULL, 0,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OK, GTK_RESPONSE_OK,
					NULL);
  /*
   *    gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
   *    gtk_window_set_wmclass (GTK_WINDOW (window), "dialog", "Nsp");
   */
  vbox = GTK_DIALOG(window)->vbox;

  nsp_dialogs_insert_title(title,vbox);

  /* A scrolled window which will contain the dialog text to be edited */
  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolled_window);
  
  buffer = gtk_text_buffer_new (NULL);
  gtk_text_buffer_get_iter_at_offset (buffer, &iter, 0);
  gtk_text_buffer_insert (buffer, &iter,init_value, -1);
  text = gtk_text_view_new_with_buffer (buffer);
  gtk_container_add (GTK_CONTAINER (scrolled_window), text);

  gtk_widget_grab_focus (text);
  gtk_widget_show_all (window);
  result = gtk_dialog_run(GTK_DIALOG(window));
  *answer = NULL;
  switch (result)
    {
    case GTK_RESPONSE_ACCEPT:
    case GTK_RESPONSE_OK:
      gtk_text_buffer_get_bounds (buffer, &start, &end);
      text_answer  = gtk_text_iter_get_text (&start, &end);
      if ( text_answer != NULL ) 
	{
	  int ind = strlen(text_answer) - 1 ;
	  if (  text_answer[ind] == '\n')  text_answer[ind] = '\0' ;
	  if (( *answer =new_nsp_string(text_answer)) == NULLSTRING)  
	    result= menu_fail;
	  else 
	    result = menu_ok;	
	}
      break;
    default: 
      result = menu_cancel;
      break;
    }
  gtk_widget_destroy(window);
  return result;
}





