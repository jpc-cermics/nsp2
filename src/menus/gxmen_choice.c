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

typedef enum { choice_combo,choice_color,choice_chooser_save, 
	       choice_chooser_open,choice_button_save,choice_button_open,choice_entry,
	       choice_unknown,choice_matrix} 
  nsp_choice_value;

typedef struct _nsp_choice_array 
{
  GtkWidget *widget;
  nsp_choice_value type;
} nsp_choice_array;

static int  nsp_combo_update_choices(NspList *L,nsp_choice_array *array);
static GtkWidget * nsp_setup_combo_box_text(char **Ms,int Msmn,int active);
static void nsp_setup_framed_combo(nsp_choice_array *ca,GtkWidget *box,char *title,char **Ms,int Msmn,int active);
static void nsp_setup_table_combo(nsp_choice_array *ca,GtkWidget *table,int row,char *title,char **Ms,int Msmn,int active);
static void nsp_setup_combo_from_list(nsp_choice_array *ca,GtkWidget *box,NspList *L,int row);
static GtkWidget *nsp_setup_choice(nsp_choice_value type,char *title,char **Ms,int Msmn,int active);
static GtkWidget * nsp_setup_matrix_entry(char **Ms,int m,int n,int entry_size);

/*
  l1=list('combo','combo title',1,['choice 1','choice 2','choice 3']);
  l2=list('entry','entry title',1,['initial']); // 1 is unused 
  l3=list('matrix','enter matrix',30,string(rand(6,2)));// l(3) is for entry size
  l4=list('colors','colors choice 4',29,['']);
  l5=list('save','file save',1,['foo.sav']); // initial value 
  l6=list('open','file open',1,['foo.rep','*.eps','*.pdf']); // answer, filter 
  rep=x_choices('Toggle Menu',list(l1,l2,l3,l4,l5,l6));
*/

/**
 * nsp_choices_with_combobox:
 * @title: 
 * @L: 
 * 
 * choose answers in combobox
 * in case of succes L is updated with selected items.
 *
 * Return value:  %OK or %FAIL
 **/


int nsp_choices_with_combobox(char *title,NspList *L,int use_table)
{
  GdkGeometry geometry;
  GtkWidget *window,*mainbox,*table;
  int i,  n = nsp_list_length(L);
  int answer, result ;
  Cell *Loc= L->first;
  nsp_choice_array *combo_entry_array;

  start_sci_gtk(); /* be sure that gtk is started */

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
  
  nsp_dialogs_insert_title(title,mainbox);

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
  
  /* gtk_widget_set_size_request (window,500,-1); */
  geometry.max_width = 300;
  geometry.max_height = 0;
  gtk_window_set_geometry_hints (GTK_WINDOW (window),window,
				 &geometry,
				 GDK_HINT_MAX_SIZE );
  
  gtk_widget_show_all (window);

  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	answer = OK;	
	if ( nsp_combo_update_choices(L,combo_entry_array)==FAIL) answer=FAIL;
	FREE(combo_entry_array);
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
 * returns a GtkComboBox filled with text from.
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
  GtkWidget *w;
  BCG *Xgc;
  switch (type) 
    {
    case choice_matrix :
      return nsp_setup_matrix_entry(Ms,2,Msmn/2,active);/* XXXXXXX */
      break;
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
    case choice_entry:
      w = gtk_entry_new() ; 
      if ( Msmn >= 1 ) gtk_entry_set_text (GTK_ENTRY(w),Ms[0]);
      return w;
      break;
    case choice_unknown: 
      return NULL;
      break;
    }
  return NULL;
}


static int nsp_get_choice_from_title(char *type)
{
  char *table[]= {"combo","entry","colors","save","open","matrix",NULL};
  int rep = is_string_in_array(type,table,1);
  if ( rep < 0 ) return choice_unknown;
  switch (rep) 
    {
    case 0:  return choice_combo; break;
    case 1:  return choice_entry; break;
    case 2:  return choice_color; break;
    case 3:  return choice_button_save;break;
    case 4:     
#ifdef OPEN26
      return choice_chooser_open;
#else 
      return choice_button_open;
#endif
      break;
    case 5:  return choice_matrix;break;
      break;
    }
  return choice_unknown;
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
  int active ;
  char *title,*type;
  NspSMatrix *entries;
  Cell *Loc= L->first;
  type = ((NspSMatrix *) Loc->O)->S[0]; Loc= Loc->next;
  title = ((NspSMatrix *) Loc->O)->S[0]; Loc= Loc->next;
  active = ((NspMatrix *) Loc->O)->R[0]; Loc= Loc->next;
  entries = ((NspSMatrix *) Loc->O);
 
  ca->type = nsp_get_choice_from_title(type);
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


/**
 * nsp_combo_update_choices:
 * @L: 
 * @array: 
 * 
 * inserts in @L the results of selected choices.
 *
 **/

static int nsp_smatrix_set_first(NspSMatrix *A,const char *str);

static int nsp_combo_update_choices(NspList *L,nsp_choice_array *array)
{
  gchar *fname;
  const gchar *fname1;
  int i=0;
  Cell *Loc= L->first;
  while (Loc != NULL) 
    {
      NspMatrix *active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
      NspSMatrix *Ms = ((NspSMatrix *) ((NspList *) Loc->O)->first->next->next->next->O);
      switch (  array[i].type )
	{
	case choice_matrix: 
	  break;
	case choice_unknown : 
	  break;
	case  choice_combo :
	case  choice_color :
	  active_field->R[0] = gtk_combo_box_get_active(GTK_COMBO_BOX (array[i].widget));
	  break;
	case choice_chooser_save:
	case choice_chooser_open:
	  /* fname is to be freed Free with g_free().*/
	  fname = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(array[i].widget));
	  if ( fname != NULL) 
	    {
	      int rep= nsp_smatrix_set_first(Ms,fname);
	      g_free(fname);
	      if ( rep == FAIL) return FAIL;
	    }
	  else 
	    {
	      int rep= nsp_smatrix_set_first(Ms,"");
	      if ( rep == FAIL) return FAIL;
	    }
	  break;
	case choice_button_save:
	case choice_button_open:
	  fname1 = g_object_get_data (G_OBJECT(array[i].widget),"filename");
	  /* fname1 = gtk_button_get_label(GTK_BUTTON(array[i].widget)); */
	  if ( fname1 != NULL) 
	    {
	      if  ( nsp_smatrix_set_first(Ms,fname1) == FAIL) return FAIL;
	    }
	  else 
	    {
	      if  ( nsp_smatrix_set_first(Ms,"") == FAIL) return FAIL;
	    }
	  break;
	case choice_entry :
	  {
	    /* text is to be freed Free with g_free().*/
	    char * text = gtk_editable_get_chars(GTK_EDITABLE(array[i].widget),0,
						 GTK_ENTRY(array[i].widget)->text_length);
	    if ( text == NULL)
	      {
		if  ( nsp_smatrix_set_first(Ms,"") == FAIL) return FAIL;
	      }
	    else
	      {
		if  ( nsp_smatrix_set_first(Ms,text) == FAIL) return FAIL;
	      }
	  }
	  break;
	}
      Loc= Loc->next;
      i++;
    }
  return OK;
}

/*
 *
 */

static int nsp_smatrix_set_first(NspSMatrix *A,const char *str)
{
  nsp_string s;
  if ( A->mn == 0) 
    {
      if ( nsp_smatrix_resize(A,1,1) == FAIL) return FAIL;
    }
  if ((s =nsp_string_copy(str)) == (nsp_string) 0) return FAIL;
  nsp_string_destroy(&(A->S[0]));
  A->S[0]= s;
  return OK;
}



/**
 * nsp_setup_matrix_entry:
 * @Ms: 
 * @active: unused 
 * 
 * returns a table filled with entries one for each component 
 * of Ms 
 * 
 * Return value: 
 **/

static GtkWidget * nsp_setup_matrix_entry(char **Ms,int m,int n,int entry_size)
{
  int i,j;
  GtkWidget *table = gtk_table_new (m,n, FALSE);
  GtkWidget *box;
  void *data = malloc(sizeof(GtkWidget *)*m*n);
  if ( data == NULL) return NULL;
  /* attach data to table 
   * may be useless since the entries are children of table 
   * and we can get them this way 
   */
  g_object_set_data_full(G_OBJECT(table),"entries",data, g_free);
  gtk_container_set_border_width (GTK_CONTAINER (table), 5);
  /* gtk_widget_set_size_request (table,entry_size*n,-1);
     gtk_window_set_default_size(GTK_WINDOW(table), entry_size*n,-1);
  */
  gtk_widget_show(table);
  box= gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (box),table);
  gtk_table_set_homogeneous(GTK_TABLE(table),TRUE);
  for (i= 0 ; i < n ; i++) 
    for ( j = 0 ; j < m ; j++)
      {
	GtkWidget *entry =  gtk_entry_new() ;
	gtk_widget_set_size_request (entry,entry_size,-1);
	gtk_table_attach (GTK_TABLE (table),entry,i,i+1,j,j+1,0,0,0,0);
	gtk_entry_set_text (GTK_ENTRY(entry),Ms[i+m*j]);
	/* gtk_entry_set_max_length (GTK_ENTRY(entry),entry_size);*/
      }
  return box;
}

