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
 * menu choice
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/gtksci.h"

typedef enum { choice_combo, choice_chooser_save, choice_chooser_open,choice_button_save} nsp_choice_value;
typedef struct _nsp_choice_array 
{
  GtkWidget *widget;
  nsp_choice_value type;
} nsp_choice_array;


static void nsp_combo_update_choices(NspList *L,nsp_choice_array *array);
static GtkWidget * nsp_setup_combo_box_text(char **Ms,int Msmn,int active);
static void nsp_setup_framed_combo(nsp_choice_array *ca,GtkWidget *box,char *title,char **Ms,int Msmn,int active);
static void nsp_setup_table_combo(nsp_choice_array *ca,GtkWidget *table,int row,char *title,char **Ms,int Msmn,int active);
static void nsp_setup_combo_from_list(nsp_choice_array *ca,GtkWidget *box,NspList *L,int row);

/*
    l1=list('choice 1',1,['toggle c1','toggle c2','toggle c3']);
    l2=list('choice 2',2,['toggle d1','toggle d2','toggle d3']);
    l3=list('choice 3',3,['toggle e1','toggle e2']);
    l4=list('colors choice 4',29,['']);
    l5=list('filename_save file save',1,['foo.sav']); // initial value 
    l6=list('filename_open file open',1,['foo.rep','*.eps','*.pdf']); // answer, filter 
    rep=x_choices('Toggle Menu',list(l1,l2,l3,l4,l5,l6));
 */

/**
 * nsp_choices_with_combobox:
 * @title: 
 * @L: 
 * 
 * choose answers in combobox
 * in case of succes L is updated with selected items (in l(i)(2))
 *
 * if the first name starts with "colors" a special combo is 
 * used to get a color in colormap 
 * if the first name starts with "filename" a combo box is 
 * used to get a filename.
 *
 * Return value:  %OK or %FAIL
 **/

static int nsp_myprint_dialog(char *title);


int nsp_choices_with_combobox(char *title,NspList *L,int use_table)
{
  GtkWidget *window,*mainbox,*table,*hbox;
  int i,  n = nsp_list_length(L);
  int answer, result ;
  Cell *Loc= L->first;
  nsp_choice_array *combo_entry_array;

  start_sci_gtk(); /* be sure that gtk is started */

  /* nsp_myprint_dialog("test"); */

  window = gtk_dialog_new_with_buttons ("Nsp choices",
					NULL, 0,
					GTK_STOCK_OK, GTK_RESPONSE_OK,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					NULL);

  /* 
   * gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
   *  gtk_window_set_wmclass  (GTK_WINDOW (window), "choices", "Nsp");
   */

  if ((combo_entry_array = malloc(n*sizeof(nsp_choice_array)))== NULL) return FAIL;

  mainbox = GTK_DIALOG(window)->vbox;

  /* title */
  
  if ( title[0] != '\0' )
    {
      hbox = gtk_hbox_new (FALSE, 0);
      gtk_box_pack_start (GTK_BOX (mainbox),hbox, FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (hbox),gtk_image_new_from_stock (GTK_STOCK_DIALOG_QUESTION,
								   GTK_ICON_SIZE_DIALOG),
			  TRUE, TRUE, 0);  
      gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new (title), FALSE, FALSE, 0);
    }

  /* set the combo : arranged in frames 
   * or in a table 
   */

  if ( use_table == TRUE  ) 
    {
      /* label in a frame 
	 labelw = gtk_frame_new (title);
	 gtk_box_pack_start (GTK_BOX (mainbox), labelw, FALSE, FALSE, 0);
	 gtk_widget_show (labelw);
      */
      table = gtk_table_new (n, 2, FALSE);    
      gtk_container_set_border_width (GTK_CONTAINER (table),5);
      gtk_table_set_col_spacing (GTK_TABLE (table), 0, 10);
      gtk_table_set_row_spacings (GTK_TABLE (table), 3);
      for ( i = 0 ; i < n ; i++) 
	{
	  nsp_setup_combo_from_list(&combo_entry_array[i],table,(NspList *) Loc->O,i);
	  Loc = Loc->next;
	}
      /* 
	 gtk_container_add (GTK_CONTAINER (labelw), table);
      */
      gtk_container_add (GTK_CONTAINER(mainbox), table);
    }
  else 
    {
      /* label widget description of the choices 
      labelw = gtk_label_new (title);
      gtk_box_pack_start (GTK_BOX (mainbox), labelw, FALSE, FALSE, 0);
      gtk_widget_show (labelw);
      */
      for ( i = 0 ; i < n ; i++) 
	{
	  nsp_setup_combo_from_list(&combo_entry_array[i],mainbox,(NspList *) Loc->O,-1);
	  Loc = Loc->next;
	}
    }


  gtk_widget_show_all (window);

  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	nsp_combo_update_choices(L,combo_entry_array);
	FREE(combo_entry_array);
	answer = OK;	
	break;
      default:
	answer = FAIL;
	break;
    }
  gtk_widget_destroy(window);
  return answer;
}

/**
 * nsp_setup_combo_box_text:
 * @Ms: 
 * @active: 
 * 
 * returns a GtkComboBoxEntry filled with text from.
 * @active is used to set the active entry.
 * 
 * Return value: 
 **/

static GtkWidget * nsp_setup_combo_box_text(char **Ms,int Msmn,int active)
{
  int i;
  GtkWidget *entry_box = gtk_combo_box_new_text ();
  for (i = 0 ; i < Msmn ; i++) 
    gtk_combo_box_append_text (GTK_COMBO_BOX (entry_box),Ms[i]);
  gtk_combo_box_set_active (GTK_COMBO_BOX (entry_box),Max(0,Min(Msmn-1,active)) );
  return  entry_box;
}

/**
 * nsp_setup_framed_combo:
 * @box: 
 * @title: 
 * @Ms: 
 * @active: 
 * 
 * 
 * 
 * Return value: 
 **/

static void nsp_button_filename_save(GtkWidget *widget,char *title);

static void nsp_setup_framed_combo(nsp_choice_array *ca,GtkWidget *box,char *title,char **Ms,int Msmn,int active)
{
  GtkWidget *tmp;
  GtkWidget *boom,*cbox;
  if ( strncmp(title,"colors",6)==0 &&  strlen(title) > 7 )
    {
      BCG *Xgc;
      Xgc=check_graphic_window();
      cbox = nsp_gtkcombobox_colormap_new(Xgc,active+1);
      tmp = gtk_frame_new (title+6);
      ca->type = choice_combo;
    }
  else   if ( strncmp(title,"filename_open",13)==0 &&  strlen(title) > 14 )
    {
      cbox = gtk_file_chooser_button_new (title+13,GTK_FILE_CHOOSER_ACTION_OPEN);
      if (  Msmn > 2 ) 
	{
	  int i;
	  GtkFileFilter* filter ;
	  for ( i=1; i < Msmn ; i++) 
	    {
	      filter = gtk_file_filter_new();
	      gtk_file_filter_set_name (GTK_FILE_FILTER(filter),Ms[i]);
	      gtk_file_filter_add_pattern(filter,Ms[i]);
	      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(cbox),filter);
	    }
	  filter = gtk_file_filter_new();
	  gtk_file_filter_set_name (GTK_FILE_FILTER(filter),"all files");
	  gtk_file_filter_add_pattern(filter,"*");
	  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(cbox),filter);
	}
      tmp = gtk_frame_new (title+14);
      ca->type = choice_chooser_open;
    }
  else   if ( strncmp(title,"filename_save",13)==0 &&  strlen(title) > 14 )
    {
      /* pb with save which does not support GTK_FILE_CHOOSER_ACTION_SAVE
       * in my version.
       */
#if 1
      if ( active == 1 && Msmn >= 1) 
	cbox = gtk_button_new_with_label(Ms[0]);
      else
	cbox = gtk_button_new_with_label("");
      gtk_signal_connect (GTK_OBJECT (cbox), "clicked",
			  GTK_SIGNAL_FUNC(nsp_button_filename_save),
			  title+14);
      ca->type = choice_button_save;
#else 
      /* cbox = gtk_file_chooser_button_new_with_dialog(dialog); */
      cbox = gtk_file_chooser_button_new (title+13,GTK_FILE_CHOOSER_ACTION_SAVE );
      ca->type = choice_chooser_save;
      if ( active == 1 && Msmn > 1 ) 
	{
	  gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(cbox),Ms[0]);
	}
#endif
      tmp = gtk_frame_new (title+14);
    }
  else
    {
      cbox = nsp_setup_combo_box_text(Ms,Msmn, active);
      tmp = gtk_frame_new (title);
      ca->type = choice_combo;
    }
  ca->widget = cbox;
  gtk_box_pack_start (GTK_BOX (box), tmp, FALSE, FALSE, 0);
  boom = gtk_vbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (boom), 5);
  gtk_container_add (GTK_CONTAINER (tmp), boom);
  gtk_container_add (GTK_CONTAINER (boom),cbox);
}

/* if gtk_file_chooser_button_new (title+13,GTK_FILE_CHOOSER_ACTION_SAVE );
 * does not work we use our own button.
 */

static void nsp_button_filename_save(GtkWidget *widget,char *title)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (title,
					NULL,
					GTK_FILE_CHOOSER_ACTION_SAVE,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
					NULL);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      char *filename;
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      gtk_button_set_label(GTK_BUTTON(widget),filename);
      g_free (filename);
    }
  gtk_widget_destroy (dialog);
}


/* the same but we store in a table */

static void nsp_setup_table_combo(nsp_choice_array *ca,GtkWidget *table,int row,char *title,char **Ms,int Msmn,int active)
{
  GtkWidget *tmp;
  GtkWidget *cbox;
  if ( strncmp(title,"colors",6)==0 &&  strlen(title) > 7 )
    {
      BCG *Xgc;
      Xgc=check_graphic_window();
      cbox = nsp_gtkcombobox_colormap_new(Xgc,active+1);
      tmp = gtk_label_new (title+6);
      ca->type = choice_combo;
    }
  else   if ( strncmp(title,"filename_open",13)==0 &&  strlen(title) > 14 )
    {
      cbox = gtk_file_chooser_button_new (title+13,GTK_FILE_CHOOSER_ACTION_OPEN);
      tmp = gtk_label_new (title+14);
      if (  Msmn > 2 ) 
	{
	  int i;
	  GtkFileFilter* filter ;
	  for ( i=1; i < Msmn ; i++) 
	    {
	      filter = gtk_file_filter_new();
	      gtk_file_filter_set_name (GTK_FILE_FILTER(filter),Ms[i]);
	      gtk_file_filter_add_pattern(filter,Ms[i]);
	      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(cbox),filter);
	    }
	  filter = gtk_file_filter_new();
	  gtk_file_filter_set_name (GTK_FILE_FILTER(filter),"all files");
	  gtk_file_filter_add_pattern(filter,"*");
	  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(cbox),filter);
	}
      ca->type = choice_chooser_open;
    }
  else   if ( strncmp(title,"filename_save",13)==0 &&  strlen(title) > 14 )
    {
      /* pb with save which does not support GTK_FILE_CHOOSER_ACTION_SAVE
       * in my version.
       */
#if 1 
      if ( active == 1 && Msmn >= 1) 
	cbox = gtk_button_new_with_label(Ms[0]);
      else
	cbox = gtk_button_new_with_label("");
      gtk_signal_connect (GTK_OBJECT (cbox), "clicked",
			  GTK_SIGNAL_FUNC(nsp_button_filename_save),
			  title+14);
      ca->type = choice_button_save;
#else 
      /* cbox = gtk_file_chooser_button_new_with_dialog(dialog); */
      cbox = gtk_file_chooser_button_new (title+13,GTK_FILE_CHOOSER_ACTION_SAVE );
      ca->type = choice_chooser_save;
      if ( active == 1 && Msmn > 1 ) 
	{
	  gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(cbox),Ms[0]);
	}
#endif
      tmp = gtk_label_new (title+14);
    }
  else 
    {
      cbox = nsp_setup_combo_box_text(Ms,Msmn, active);
      ca->type = choice_combo;
      tmp = gtk_label_new (title);
    }
  ca->widget = cbox;
  gtk_misc_set_alignment (GTK_MISC (tmp), 0.0, 0.5);
  gtk_table_attach_defaults (GTK_TABLE (table), tmp, 0, 1, row,row+1);
  gtk_table_attach_defaults (GTK_TABLE (table), cbox, 1, 2, row,row+1);
}

/**
 * nsp_setup_combo_from_list:
 * @L: 
 * 
 * we do not check here that the list is properly filled
 * this is done in the interface.
 * 
 * Return value: 
 **/

static void nsp_setup_combo_from_list(nsp_choice_array *ca,GtkWidget *box,NspList *L,int use_row)
{
  int active ;
  char *title;
  NspSMatrix *entries;
  Cell *Loc= L->first;
  title = ((NspSMatrix *) Loc->O)->S[0]; Loc= Loc->next;
  active = ((NspMatrix *) Loc->O)->R[0]; Loc= Loc->next;
  entries = ((NspSMatrix *) Loc->O);
  if ( use_row == -1 )
    {
      /* here box is a box */
      nsp_setup_framed_combo(ca,box,title,entries->S,entries->mn,active);
    }
  else 
    {
      /* here box is a table */
      nsp_setup_table_combo(ca,box,use_row,title,entries->S,entries->mn,active);
    }
}


static void nsp_combo_update_choices(NspList *L,nsp_choice_array *array)
{
  gchar *fname;
  const gchar *fname1;
  int i=0;
  Cell *Loc= L->first;
  while (Loc != NULL) 
    {
      NspMatrix *active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->O);
      switch (  array[i].type )
	{
	case  choice_combo :
	  active_field->R[0] = gtk_combo_box_get_active(GTK_COMBO_BOX (array[i].widget));
	  break;
	case choice_chooser_save:
	case choice_chooser_open:
	  fname = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(array[i].widget));
	  if ( fname != NULL) 
	    {
	      Sciprintf("save: %s\n",fname);
	      g_free(fname);
	      active_field->R[0]  = 1;
	    }
	  else 
	    {
	      Sciprintf("save: none\n");
	      active_field->R[0]  =  0;
	    }
	  break;
	case choice_button_save:
	  fname1 = gtk_button_get_label(GTK_BUTTON(array[i].widget));
	  if ( fname1 != NULL) 
	    {
	      Sciprintf("save: %s\n",fname1);
	      active_field->R[0]  = 1;
	    }
	  else 
	    {
	      Sciprintf("save: none\n");
	      active_field->R[0]  =  0;
	    }
	  break;
	}
      Loc= Loc->next;
      i++;
    }
}

/*
 * old version 
 */

typedef enum { COMBO_OK,COMBO_CANCEL,COMBO_DESTROYED, COMBO_RESET } combo_state;

static void nsp_ok(GtkWidget *widget,int *answer)
{
  *answer = COMBO_OK;
  gtk_main_quit();
}

static void nsp_cancel(GtkWidget *widget,int *answer)
{
  *answer = COMBO_CANCEL;
  gtk_main_quit();
}

static void nsp_destroyed(GtkWidget *widget,int *answer)
{
  if ( *answer == COMBO_RESET ) 
    {
      /* we get there when we destroy the window after an OK 
       * or a Cancel and we do not want to quit gtk_main twice 
       */

      *answer = COMBO_DESTROYED;
      gtk_main_quit();
    }
  else 
    *answer = COMBO_DESTROYED;
}

int nsp_choices_with_combobox_old(char *title,NspList *L)
{
  GtkWidget *window,*mainbox,*button_ok,*button_cancel, *hbbox,*labelw,*table;
  int i,  n = nsp_list_length(L), use_table = 0;
  int answer = COMBO_RESET ;
  Cell *Loc= L->first;
  nsp_choice_array *combo_entry_array;

  start_sci_gtk(); /* be sure that gtk is started */

  if ((combo_entry_array = malloc(n*sizeof(nsp_choice_array)))== NULL) return FAIL;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Nsp choices");
  gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
  gtk_window_set_wmclass  (GTK_WINDOW (window), "choices", "Nsp");


  gtk_signal_connect (GTK_OBJECT (window), "destroy",
		      GTK_SIGNAL_FUNC(nsp_destroyed),
		      &answer);

  gtk_container_set_border_width (GTK_CONTAINER (window), 5);
  /* g_signal_connect (window, "destroy", gtk_main_quit, NULL); */

  mainbox = gtk_vbox_new (FALSE, 2);
  gtk_container_add (GTK_CONTAINER (window), mainbox);


  /* set the combo : arranged in frames 
   * or in a table 
   */

  if ( use_table ) 
    {
      labelw = gtk_frame_new (title);
      gtk_box_pack_start (GTK_BOX (mainbox), labelw, FALSE, FALSE, 0);
      gtk_widget_show (labelw);

      table = gtk_table_new (n, 2, FALSE);    
      gtk_container_set_border_width (GTK_CONTAINER (table),5);
      gtk_table_set_col_spacing (GTK_TABLE (table), 0, 10);
      gtk_table_set_row_spacings (GTK_TABLE (table), 3);
      for ( i = 0 ; i < n ; i++) 
	{
	  nsp_setup_combo_from_list(&combo_entry_array[i],table,(NspList *) Loc->O,i);
	  Loc = Loc->next;
	}
      gtk_container_add (GTK_CONTAINER (labelw), table);
    }
  else 
    {
      /* label widget description of the choices */
      labelw = gtk_label_new (title);
      gtk_box_pack_start (GTK_BOX (mainbox), labelw, FALSE, FALSE, 0);
      gtk_widget_show (labelw);

      for ( i = 0 ; i < n ; i++) 
	{
	  nsp_setup_combo_from_list(&combo_entry_array[i],mainbox,(NspList *) Loc->O,-1);
	  Loc = Loc->next;
	}
    }

  /* hbox for buttons */

  hbbox = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (mainbox), hbbox, FALSE, FALSE , 2);
  gtk_widget_show (hbbox);

  /* Ok and Cancel buttons */

  button_ok = gtk_button_new_from_stock (GTK_STOCK_OK);
  gtk_container_add (GTK_CONTAINER (hbbox), button_ok);
  gtk_signal_connect (GTK_OBJECT (button_ok), "clicked",
		      GTK_SIGNAL_FUNC(nsp_ok),
		      &answer);
  GTK_WIDGET_SET_FLAGS (button_ok, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (button_ok);

  button_cancel = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  gtk_container_add (GTK_CONTAINER (hbbox), button_cancel);
  GTK_WIDGET_SET_FLAGS (button_cancel, GTK_CAN_DEFAULT);
  gtk_signal_connect (GTK_OBJECT (button_cancel), "clicked",
		      GTK_SIGNAL_FUNC(nsp_cancel),
		      &answer);

  gtk_widget_show_all (window);

  while (1) 
    {
      gtk_main();
      if (answer != COMBO_RESET ) break;
    }
  switch (answer) 
    {
    case COMBO_OK :
      /* fill L with results 
       */
      nsp_combo_update_choices(L,combo_entry_array);
      FREE(combo_entry_array);
      gtk_widget_destroy(window);
      answer = OK;
      break;
    case COMBO_CANCEL :
      answer = FAIL;
      gtk_widget_destroy(window);
      break;
    case COMBO_DESTROYED:
      answer = FAIL;
      break;
    }
  return answer;
}



int nsp_myprint_dialog(char *title)
{
  char *file_name[]={ "undefined.eps",NULL};
  nsp_choice_array ca[5];
  char *export_formats[] = {
    "Postscript",
    "Postscript No Preamble",
    "Postscript-Latex",
    "Xfig",
    "Gif",
    "PPM",
    NULL
  };
  char *printers_name[]={ "hp3", "hp4",NULL};
  char *color_items_name[]={ "color", "black and white",NULL};
  char *portrait_items_name[]={"landscape", "portrait", "keep size",NULL };

  GtkWidget *window,*mainbox,*table,*hbox;
  int answer, result ;

  start_sci_gtk(); /* be sure that gtk is started */

  window = gtk_dialog_new_with_buttons (title,
					NULL, 0,
					GTK_STOCK_OK, GTK_RESPONSE_OK,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					NULL);

  mainbox = GTK_DIALOG(window)->vbox;

  /* title */
  
  if ( title[0] != '\0' )
    {
      hbox = gtk_hbox_new (FALSE, 0);
      gtk_box_pack_start (GTK_BOX (mainbox),hbox, FALSE, FALSE, 0);
      gtk_box_pack_start (GTK_BOX (hbox),gtk_image_new_from_stock (GTK_STOCK_DIALOG_QUESTION,
								   GTK_ICON_SIZE_DIALOG),
			  TRUE, TRUE, 0);  
      gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new (title), FALSE, FALSE, 0);
    }

  table = gtk_table_new (3, 2, FALSE);    
  gtk_container_set_border_width (GTK_CONTAINER (table),5);
  gtk_table_set_col_spacing (GTK_TABLE (table), 0, 10);
  gtk_table_set_row_spacings (GTK_TABLE (table), 3);

  /* printer list */
  nsp_setup_table_combo(&ca[0],table,0,"Printers",printers_name,2,0);
  nsp_setup_table_combo(&ca[1],table,1,"Color ?",color_items_name,2,0);
  nsp_setup_table_combo(&ca[2],table,2,"mode", portrait_items_name,3,1);
  nsp_setup_table_combo(&ca[3],table,3,"format", export_formats,5,0);
  nsp_setup_table_combo(&ca[4],table,4,"filename_save filename", file_name,1,1);

  gtk_container_add (GTK_CONTAINER(mainbox), table);

  gtk_widget_show_all (window);

  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	/* nsp_combo_update_choices(L,ca); */
	answer = OK;	
	break;
      default:
	answer = FAIL;
	break;
    }
  gtk_widget_destroy(window);
  return answer;
}
