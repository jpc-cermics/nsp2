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

typedef enum { choice_combo,choice_color,choice_chooser_save, choice_chooser_open,choice_button_save,choice_button_open} 
  nsp_choice_value;
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
static GtkWidget *nsp_setup_choice(nsp_choice_value type,char *title,char **Ms,int Msmn,int active);

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


#ifdef OPEN26
static GtkWidget * nsp_file_chooser_button_open_2_6(char *title,char **Ms,int Msmn);
#else 
static GtkWidget * nsp_file_chooser_button_open_2_4(char *title,char **Ms,int Msmn);
#endif 

static GtkWidget * nsp_file_chooser_button_save(char *title,char **Ms,int Msmn,int active);

static void nsp_setup_framed_combo(nsp_choice_array *ca,GtkWidget *box,char *title,char **Ms,int Msmn,int active)
{
  GtkWidget *tmp,*boom;
  ca->widget = nsp_setup_choice(ca->type,title,Ms,Msmn,active);
  tmp = gtk_frame_new (title);
  gtk_box_pack_start (GTK_BOX (box), tmp, FALSE, FALSE, 0);
  boom = gtk_vbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (boom), 5);
  gtk_container_add (GTK_CONTAINER (tmp), boom);
  gtk_container_add (GTK_CONTAINER (boom),ca->widget);
}

/* the same but we store in a table */

static void nsp_setup_table_combo(nsp_choice_array *ca,GtkWidget *table,int row,char *title,char **Ms,int Msmn,int active)
{
  GtkWidget *tmp;
  ca->widget = nsp_setup_choice(ca->type,title,Ms,Msmn,active);
  tmp = gtk_label_new (title);
  gtk_misc_set_alignment (GTK_MISC (tmp), 0.0, 0.5);
  gtk_table_attach_defaults (GTK_TABLE (table), tmp, 0, 1, row,row+1);
  gtk_table_attach_defaults (GTK_TABLE (table), ca->widget, 1, 2, row,row+1);
}

/* creates a widget according to type  */

static GtkWidget *nsp_setup_choice(nsp_choice_value type,char *title,char **Ms,int Msmn,int active)
{
  BCG *Xgc;
  switch (type) 
    {
    case choice_combo: 
      return nsp_setup_combo_box_text(Ms,Msmn, active);
      break;
    case choice_color: 
      Xgc=check_graphic_window();
      return  nsp_gtkcombobox_colormap_new(Xgc,active+1);
      break;
    case choice_chooser_save:
      break;
    case choice_chooser_open:
#ifdef OPEN26
      return nsp_file_chooser_button_open_2_6(title,Ms,Msmn);
#endif 
      break;
    case choice_button_save:
      return nsp_file_chooser_button_save(title,Ms,Msmn,active);
      break;
    case choice_button_open: 
#ifndef OPEN26
      return nsp_file_chooser_button_open_2_4(title,Ms,Msmn);
#endif 
      break;
    }
  return NULL;
}


static char * nsp_get_choice_from_title(char *title,nsp_choice_value *type)
{
  if ( strncmp(title,"colors",6)==0 &&  strlen(title) > 7 )
    {
      *type = choice_color;
      return title+6;
    }
  else   if ( strncmp(title,"filename_open",13)==0 &&  strlen(title) > 14 )
    {
#ifdef OPEN26
      *type = choice_chooser_open;
#else 
      *type = choice_button_open;
#endif
      return title+13;
    }
  else   if ( strncmp(title,"filename_save",13)==0 &&  strlen(title) > 14 )
    {
      *type = choice_button_save;
      return title+13;
    }
  else 
    {
      *type = choice_combo;
      return title;
    }
  return NULL;
}



/* 
 * a button which activate a gtk_file_chooser_dialog 
 * for saving a file
 * even in gtk 2.8  gtk_file_chooser_button_new do not work 
 * for saving. 
 */

static void nsp_button_filename_save(GtkWidget *widget,char *title);


static GtkWidget * nsp_file_chooser_button_save(char *title,char **Ms,int Msmn,int active)
{
  GtkWidget *cbox;
  if ( active == 1 && Msmn >= 1) 
    {
      cbox = gtk_button_new_with_label(Ms[0]);
      g_object_set_data_full (G_OBJECT(cbox),"filename",g_strdup(Ms[0]), g_free);
    }
  else
    {
      cbox = gtk_button_new_with_label("");
    }
  gtk_signal_connect (GTK_OBJECT (cbox), "clicked",
		      GTK_SIGNAL_FUNC(nsp_button_filename_save),
		      title);
  return cbox;
}

/* associated handler 
 *
 */

static void nsp_button_filename_save(GtkWidget *widget,char *title)
{
  GtkWidget *dialog;
  dialog = gtk_file_chooser_dialog_new (title,
					NULL,
					GTK_FILE_CHOOSER_ACTION_SAVE,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					NULL);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      char *filename,*base;
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      base = g_path_get_basename(filename);
      gtk_button_set_label(GTK_BUTTON(widget),base);
      g_object_set_data_full (G_OBJECT(widget),"filename",filename, g_free);
      g_free (base); 
    }
  gtk_widget_destroy (dialog);
}


#ifdef OPEN26

/* used when gtk_file_chooser_button_new exists 
 * version 2.6 
 */

static GtkWidget * nsp_file_chooser_button_open_2_6(char *title,char **Ms,int Msmn);
{
  GtkWidget *cbox;
  cbox = gtk_file_chooser_button_new (title,GTK_FILE_CHOOSER_ACTION_OPEN);
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
  return cbox;
}

#else 

/* version for gtk 2.4 
 *
 */

typedef struct _nsp_open_filename_open_data nsp_button_filename_open_data;

struct _nsp_open_filename_open_data {
  int Msmn;
  char **Ms;
  char *title;
};

static void nsp_button_filename_open(GtkWidget *widget,void *args);


static GtkWidget * nsp_file_chooser_button_open_2_4(char *title,char **Ms,int Msmn)
{
  nsp_button_filename_open_data *data= NULL;
  GtkWidget *cbox;
  cbox = gtk_button_new_with_label("");
  data = malloc( sizeof(nsp_button_filename_open_data));
  if ( data != NULL) 
    {
      data->Msmn = Msmn;
      data->Ms = Ms;
      data->title= title;
    }
  /* attach data to cbox */
  g_object_set_data_full (G_OBJECT(cbox),"button_open",data, g_free);

  /* the handler will have to free data */
  gtk_signal_connect (GTK_OBJECT (cbox), "clicked",
		      GTK_SIGNAL_FUNC(nsp_button_filename_open),
		      NULL);
  return  cbox;
}

/* handler for button 
 *
 */

static void nsp_button_filename_open(GtkWidget *widget,void *args)
{
  nsp_button_filename_open_data *data;
  GtkWidget *dialog;
  data  = g_object_get_data(G_OBJECT(widget),"button_open");

  dialog = gtk_file_chooser_dialog_new (data->title,
					NULL,
					GTK_FILE_CHOOSER_ACTION_OPEN,
					GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
					NULL);
  if (  data->Msmn > 2 ) 
    {
      int i;
      GtkFileFilter* filter ;
      for ( i=1; i < data->Msmn ; i++) 
	{
	  filter = gtk_file_filter_new();
	  gtk_file_filter_set_name (GTK_FILE_FILTER(filter),data->Ms[i]);
	  gtk_file_filter_add_pattern(filter,data->Ms[i]);
	  gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog),filter);
	}
      filter = gtk_file_filter_new();
      gtk_file_filter_set_name (GTK_FILE_FILTER(filter),"all files");
      gtk_file_filter_add_pattern(filter,"*");
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog),filter);
    }
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      char *filename,*base;
      filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
      base = g_path_get_basename(filename);
      gtk_button_set_label(GTK_BUTTON(widget),base);
      g_object_set_data_full (G_OBJECT(widget),"filename",filename, g_free);
      g_free (base); 
    }
  gtk_widget_destroy (dialog);
}

#endif /* OPEN26*/

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
  nsp_choice_value type;
  int active ;
  char *title,*title_and_type;
  NspSMatrix *entries;
  Cell *Loc= L->first;
  title_and_type = ((NspSMatrix *) Loc->O)->S[0]; Loc= Loc->next;
  active = ((NspMatrix *) Loc->O)->R[0]; Loc= Loc->next;
  entries = ((NspSMatrix *) Loc->O);
 
  title= nsp_get_choice_from_title(title_and_type,&type);
  ca->type = type;
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
	case  choice_color :
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
	case choice_button_open:
	  fname1 = g_object_get_data (G_OBJECT(array[i].widget),"filename");
	  /* fname1 = gtk_button_get_label(GTK_BUTTON(array[i].widget)); */
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
