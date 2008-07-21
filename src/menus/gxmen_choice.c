/* Nsp
 * Copyright (C) 1998-2006 Jean-Philippe Chancelier Enpc/Cermics
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

#if GTK_MAJOR_VERSION==2 &&  GTK_MINOR_VERSION>=6
#define OPEN26
#endif 

typedef enum { choice_combo,choice_color,choice_chooser_save, 
	       choice_chooser_open_file,choice_button_save,choice_button_open_file,choice_entry,
	       choice_unknown,choice_matrix,choice_chooser_open_folder,choice_button_open_folder,
	       choice_spin_button,choice_range_button,choice_button,choice_ignore }
  nsp_choice_value;

typedef struct _nsp_choice_array 
{
  GtkWidget *widget;
  nsp_choice_value type;
} nsp_choice_array;

static int  nsp_combo_update_choices(NspList *L,nsp_choice_array *array);
static GtkWidget * nsp_setup_combo_box_text(char **Ms,int Mm,int Mn,int active);
static void nsp_setup_framed_combo(nsp_choice_array *ca,GtkWidget *box,char *title,void *Obj,char **Ms,int Mm,int Mn,int active);
static void nsp_setup_table_combo(nsp_choice_array *ca,GtkWidget *table,int row,char *title,void *Obj,
				  char **Ms,int Mm,int Mn,int active);
static int nsp_setup_combo_from_list(nsp_choice_array *ca,GtkWidget *box,NspList *L,int row);
static GtkWidget *nsp_setup_choice(nsp_choice_value type,char *title,void *Obj,char **Ms,int Mm,int Mn,int active);
static GtkWidget * nsp_setup_matrix_entry(GtkWidget *w,char **Ms,int m,int n,int entry_size);
static int nsp_matrix_entry_get_values(GtkWidget *table,NspSMatrix *S);
static GtkWidget *nsp_setup_matrix_wraper(char **Ms,int m,int n,int entry_size);
static GtkWidget *nsp_setup_spin_button_wraper(double *val,int entry_size);
static int nsp_spin_button_get_value(GtkWidget *spin,NspMatrix *M);
static GtkWidget *nsp_setup_scale_wraper(double *val,int entry_size);
static int nsp_scale_get_value(GtkWidget *scale,NspMatrix *M);

static GtkWidget *nsp_setup_choice_button(void *Obj,int entry_size);
static NspList *nsp_combo_extract_choices(NspList *L);

/*
  l1=list('combo','combo title',1,['choice 1','choice 2','choice 3']);
  l2=list('entry','entry title',1,['initial']); // 1 is unused 
  l3=list('matrix','enter matrix',10,string(rand(6,2)));// l(3) is for entry size
  l4=list('colors','colors choice 4',29,['']);
  l5=list('save','file save',1,['foo.sav']); // initial value 
  l6=list('open','file open',1,['foo.rep','*.eps','*.pdf']); // answer, filter 
  l7=list('folder','choose a folder',1,[""]); // answer, filter 
  L= list(l1,l2,l3,l4,l5,l6,l7);
  [rep,L]=x_choices('Toggle Menu',L);
*/

/**
 * nsp_choices_with_combobox:
 * @title: 
 * @L: 
 * 
 * choose answers in combobox
 * in case of succes (menu_ok or menu_cancel) L is updated with selected items and Res contains 
 * the answers extracted from L. 
 *
 * Return value:  menu_ok , menu_cancel, menu_fail, menu_bad_argument.
 **/


menu_answer nsp_choices_with_combobox(char *title,NspList *L,NspList **Res,int use_table)
{
  /* GdkGeometry geometry; */
  GtkWidget *window,*mainbox,*table;
  int i,  n = nsp_list_length(L);
  int result ;
  menu_answer answer;
  Cell *Loc= L->first;
  nsp_choice_array *combo_entry_array=NULL;

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

  if ((combo_entry_array = malloc(n*sizeof(nsp_choice_array)))== NULL) return menu_fail;

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
	  if ( 	  nsp_setup_combo_from_list(&combo_entry_array[i],table,(NspList *) Loc->O,i)==
		  choice_unknown )
	    {
	      FREE(combo_entry_array);
	      return menu_bad_argument;
	    }
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
	  if ( nsp_setup_combo_from_list(&combo_entry_array[i],mainbox,(NspList *) Loc->O,-1) == 
	       choice_unknown )
	    {
	      FREE(combo_entry_array);
	      return menu_bad_argument;
	    }
	  Loc = Loc->next;
	}
    }
  
  /* gtk_widget_set_size_request (window,500,-1); */
  /* 
     geometry.max_width = 300;
     geometry.max_height = 0;
     gtk_window_set_geometry_hints (GTK_WINDOW (window),window,
     &geometry,
     GDK_HINT_MAX_SIZE );
  */

  gtk_widget_show_all (window);

  result = gtk_dialog_run(GTK_DIALOG(window));
  switch (result)
    {
      case GTK_RESPONSE_ACCEPT:
      case GTK_RESPONSE_OK:
	answer = menu_ok;	
	if ( nsp_combo_update_choices(L,combo_entry_array)==FAIL) answer=menu_fail;
	if ( Res != NULL) 
	  {
	    if ((*Res = nsp_combo_extract_choices(L))==NULLLIST) answer=menu_fail;
	  }
	break;
      default:
	answer = menu_cancel;
	if ( Res != NULL ) 
	  {
	    if ((*Res= nsp_list_create(NVOID)) == NULLLIST ) answer=menu_fail;
	  }
	break;
    }
  FREE(combo_entry_array);
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

static GtkWidget * nsp_setup_combo_box_text(char **Ms,int Mm,int Mn,int active)
{
  int i;
  GtkWidget *entry_box = gtk_combo_box_new_text ();
  for (i = 0 ; i < Mm*Mn ; i++) 
    gtk_combo_box_append_text (GTK_COMBO_BOX (entry_box),Ms[i]);
  gtk_combo_box_set_active (GTK_COMBO_BOX (entry_box),Max(0,Min(Mm*Mn-1,active)) );
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
static GtkWidget * nsp_file_chooser_button_open_2_6(char *title,char **Ms,int Mm,int Mn,int folder);
#else 
static GtkWidget * nsp_file_chooser_button_open_2_4(char *title,char **Ms,int Mm,int Mn,int folder);
#endif 

static GtkWidget * nsp_file_chooser_button_save(char *title,char **Ms,int Mm,int Mn,int active);

static void nsp_setup_framed_combo(nsp_choice_array *ca,GtkWidget *box,char *title,void *Obj,char **Ms,int Mm,int Mn,int active)
{
  GtkWidget *tmp,*boom;
  ca->widget = nsp_setup_choice(ca->type,title,Obj,Ms,Mm,Mn,active);
  tmp = gtk_frame_new (title);
  gtk_box_pack_start (GTK_BOX (box), tmp, FALSE, FALSE, 0);
  boom = gtk_vbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (boom), 5);
  gtk_container_add (GTK_CONTAINER (tmp), boom);
  gtk_container_add (GTK_CONTAINER (boom),ca->widget);
}

/* the same but we store in a table */

static void nsp_setup_table_combo(nsp_choice_array *ca,GtkWidget *table,int row,char *title,void *Obj,
				  char **Ms,int Mm,int Mn,int active)
{
  GtkWidget *tmp;
  ca->widget = nsp_setup_choice(ca->type,title,Obj,Ms,Mm,Mn,active);
  tmp = gtk_label_new (title);
  gtk_misc_set_alignment (GTK_MISC (tmp), 0.0, 0.5);
  gtk_table_attach_defaults (GTK_TABLE (table), tmp, 0, 1, row,row+1);
  gtk_table_attach_defaults (GTK_TABLE (table), ca->widget, 1, 2, row,row+1);
}

/* creates a widget according to type  */

static GtkWidget *nsp_setup_choice(nsp_choice_value type,char *title,void *Obj,char **Ms,int Mm,int Mn,int active)
{
  GtkWidget *w;
  BCG *Xgc;
  switch (type) 
    {
    case choice_matrix :
      /* with or without scroll vindow Attention au 2 XXXXXX à corriger */
      if ( 0 ) 
	return nsp_setup_matrix_entry(NULL,Ms,Mm,Mn,active);
      else
	return  nsp_setup_matrix_wraper(Ms,Mm,Mn,active); 
      break;
    case choice_combo: 
      return nsp_setup_combo_box_text(Ms,Mm,Mn, active-1);
      break;
    case choice_color: 
      Xgc=check_graphic_window();
      return  nsp_gtkcombobox_colormap_new(Xgc,active-1);
      break;
    case choice_chooser_save:
      break;
    case choice_chooser_open_file:
#ifdef OPEN26
      return nsp_file_chooser_button_open_2_6(title,Ms,Mm,Mn,FALSE);
#endif 
      break;
    case choice_chooser_open_folder:
#ifdef OPEN26
      return nsp_file_chooser_button_open_2_6(title,Ms,Mm,Mn,TRUE);
#endif 
      break;
    case choice_button_save:
      return nsp_file_chooser_button_save(title,Ms,Mm,Mn,active);
      break;
    case choice_button_open_file: 
#ifndef OPEN26
      return nsp_file_chooser_button_open_2_4(title,Ms,Mm,Mn,FALSE);
#endif 
    case choice_button_open_folder: 
#ifndef OPEN26
      return nsp_file_chooser_button_open_2_4(title,Ms,Mm,Mn,TRUE);
#endif 
      break;
    case choice_entry:
      w = gtk_entry_new() ; 
      if ( Mm*Mn >= 1 ) gtk_entry_set_text (GTK_ENTRY(w),Ms[0]);
      return w;
      break;
    case choice_unknown: 
      return NULL;
      break;
    case choice_spin_button:
      return nsp_setup_spin_button_wraper((double *)Ms,active);
    case choice_ignore : 
      return NULL;
    case choice_range_button:
      return nsp_setup_scale_wraper((double *)Ms,active);
    case choice_button :
      return nsp_setup_choice_button(Obj,active);
    }
  return NULL;
}

/* take care that the order of strings in this function 
 * must match the enum type 
 *
 */


static int nsp_get_choice_from_title(char *type)
{
  char *table[]= {"combo","entry","colors","save","open","matrix","folder","spin","range","button","ignore",NULL};
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
      return choice_chooser_open_file;
#else 
      return choice_button_open_file;
#endif
      break;
    case 5:  return choice_matrix;break;
    case 6:
#ifdef OPEN26
      return choice_chooser_open_folder;
#else 
      return choice_button_open_folder;
#endif
      break;
    case 7: return choice_spin_button;
    case 8: return choice_range_button;
    case 9: return choice_button;
    case 10: return choice_ignore;
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


static GtkWidget * nsp_file_chooser_button_save(char *title,char **Ms,int Mm,int Mn,int active)
{
  GtkWidget *cbox;
  if ( active == 1 && Mm*Mn >= 1) 
    {
      cbox = gtk_button_new_with_label(Ms[0]);
      g_object_set_data_full (G_OBJECT(cbox),"filename",g_strdup(Ms[0]), g_free);
    }
  else
    {
      cbox = gtk_button_new_with_label("");
    }
  g_signal_connect (GTK_OBJECT (cbox), "clicked",
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

static GtkWidget * nsp_file_chooser_button_open_2_6(char *title,char **Ms,int Mm,int Mn,int folder)
{
  GtkWidget *cbox;
  if ( folder ) 
    {
      /* choose a folder */
      cbox = gtk_file_chooser_button_new (title,GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
    }
  else 
    {
      cbox = gtk_file_chooser_button_new (title,GTK_FILE_CHOOSER_ACTION_OPEN);
      if (  Mm*Mn > 2 ) 
	{
	  /* install filters if present 
	   *
	   */
	  int i;
	  GtkFileFilter* filter ;
	  for ( i=1; i < Mm*Mn ; i++) 
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
      if ( Mm*Mn > 1 && Ms[0] != '\0' ) 
	{
	  /* set default choice if given  
	   * Ms[0] must be a name in current folder 
	   * or a fullpathnanme 
	   */
	  gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(cbox),Ms[0]); 
	}
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
  int folder;
};

static void nsp_button_filename_open(GtkWidget *widget,void *args);


static GtkWidget * nsp_file_chooser_button_open_2_4(char *title,char **Ms,int Mm,int Mn,int folder)
{
  nsp_button_filename_open_data *data= NULL;
  GtkWidget *cbox;
  cbox = gtk_button_new_with_label("");
  data = malloc( sizeof(nsp_button_filename_open_data));
  if ( data != NULL) 
    {
      data->Msmn = Mm*Mn;
      data->Ms = Ms;
      data->title= title;
      data->folder = folder;
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
  GtkFileChooserAction action;
  nsp_button_filename_open_data *data;
  GtkWidget *dialog;
  data  = g_object_get_data(G_OBJECT(widget),"button_open");

  action = ( data->folder == TRUE ) ? GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER :
    GTK_FILE_CHOOSER_ACTION_OPEN;
  
  dialog = gtk_file_chooser_dialog_new (data->title,
					NULL,
					action,
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

static int nsp_setup_combo_from_list(nsp_choice_array *ca,GtkWidget *box,NspList *L,int use_row)
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
  switch ( ca->type )
    {
    case choice_unknown: 
      Scierror("Error: unrecognized key-work \"%s\" \n",type);
      return ca->type;
    case choice_ignore: 
      return ca->type;
    default:
      if ( use_row == -1 )
	{
	  /* here box is a box */
	  nsp_setup_framed_combo(ca,box,title,entries,entries->S,entries->m,entries->n,active);
	}
      else 
	{
	  /* here box is a table */
	  nsp_setup_table_combo(ca,box,use_row,title,entries,entries->S,entries->m,entries->n,active);
	}
    }

  return ca->type;
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
  int i=0,rep;
  Cell *Loc= L->first;
  while (Loc != NULL) 
    {
      NspMatrix *active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
      NspSMatrix *Ms = ((NspSMatrix *) ((NspList *) Loc->O)->first->next->next->next->O);
      switch (  array[i].type )
	{
	case choice_matrix: 
	  if ( nsp_matrix_entry_get_values(array[i].widget,Ms)== FAIL) return FAIL;
	  break;
	case choice_unknown : 
	  break;
	case  choice_combo :
	case choice_color:
	  active_field->R[0] = 1+ gtk_combo_box_get_active(GTK_COMBO_BOX (array[i].widget));
	  break;
	case choice_chooser_save:
	case choice_chooser_open_file:
	case choice_chooser_open_folder:
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
	case choice_button_open_file:
	case choice_button_open_folder:
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
	case choice_spin_button:
	  rep = nsp_spin_button_get_value(array[i].widget,(NspMatrix *) Ms);
	  active_field->R[0] = ((NspMatrix *) Ms)->R[0];
	  break;
	case choice_range_button:
	  rep = nsp_scale_get_value(array[i].widget,(NspMatrix *) Ms);
	  active_field->R[0] = ((NspMatrix *) Ms)->R[0];
	  break;
	case choice_button:
	  break;
	case choice_ignore: 
	  break;
	}
      Loc= Loc->next;
      i++;
    }
  return OK;
}

/* extract answers from L 
 */


static NspList *nsp_combo_extract_choices(NspList *L)
{
  NspObject *Ob;
  NspList *Res;
  gchar *fname=NULL;
  const gchar *fname1;
  int i=0;
  Cell *Loc= L->first;
  
  if ((Res= nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;
  while (Loc != NULL) 
    {
      NspSMatrix *title = ((NspSMatrix *) ((NspList *) Loc->O)->first->O);
      NspMatrix *active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
      NspSMatrix *Ms = ((NspSMatrix *) ((NspList *) Loc->O)->first->next->next->next->O);
      nsp_choice_value type = nsp_get_choice_from_title(title->S[0]);
      switch ( type )
	{
	case choice_matrix: 
	  /* store updated in Res */  
	  if ((Ob =nsp_object_copy_and_name("le",NSP_OBJECT(Ms)))== NULLOBJ) return NULLLIST;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL) return NULLLIST;
	  break;
	case choice_unknown : 
	  break;
	case choice_combo :
	case choice_color:
	  if (( Ob =nsp_create_object_from_double("le",active_field->R[0])) == NULLOBJ ) 
	    return NULLLIST;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL) return NULLLIST;
	  break;
	case choice_chooser_save:
	case choice_chooser_open_file:
	case choice_chooser_open_folder:
	  fname = Ms->S[0];
	  if (( Ob = nsp_create_object_from_str("X",fname)) == NULLOBJ ) goto err;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL) goto err;
	  break;
	err: 
	  return NULLLIST;
	  break;
	case choice_button_save:
	case choice_button_open_file:
	case choice_button_open_folder:
	  fname1 = Ms->S[0];
	  if (( Ob = nsp_create_object_from_str("X",fname1)) == NULLOBJ ) return NULLLIST;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL) return NULLLIST;
	  break;
	case choice_entry :
	  if (( Ob = nsp_create_object_from_str("X",Ms->S[0])) == NULLOBJ ) return NULLLIST;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL) return NULLLIST;
	  break;
	case choice_spin_button:
	  if (( Ob =nsp_create_object_from_double("le",active_field->R[0])) == NULLOBJ ) 
	    return NULLLIST;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL)return NULLLIST;
	  break;
	case choice_range_button:
	  if (( Ob =nsp_create_object_from_double("le",active_field->R[0])) == NULLOBJ ) 
	    return NULLLIST;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL)return NULLLIST;
	  break;
	case choice_button:
	  if (( Ob = (NspObject *) nsp_combo_extract_choices((NspList *)Ms)) == NULLOBJ )
	    return NULLLIST;
	  if (nsp_object_set_name(Ob,"le") == FAIL) return NULLLIST;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL) return NULLLIST;
	  break;
	case choice_ignore:
	  if (( Ob =nsp_create_object_from_double("le",active_field->R[0])) == NULLOBJ )
	    return NULLLIST;
	  if ( nsp_list_end_insert(Res,Ob) == FAIL)return NULLLIST;
	  break;
	}
      Loc= Loc->next;
      i++;
    }
  return Res;
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
 * returns a #GtkWidget used to edit matrix entries. 
 * This widget is a matrix of #GtkEntry.
 * If @w is NULL, the returned widget contains a data field named "entries" 
 * which can be used to get back the edited values through function 
 * #nsp_matrix_entry_get_values. If @w is non null then the entries 
 * property is attached to @w.
 * 
 * Return value: 
 **/

static GtkWidget * nsp_setup_matrix_entry(GtkWidget *w,char **Ms,int m,int n,int entry_size)
{
  int i,j;
  GtkWidget *table = gtk_table_new (m,n, FALSE);
  /* GtkWidget *box; */
  GtkWidget **data = malloc(sizeof(GtkWidget *)*m*n);
  if ( data == NULL) return NULL;
  /* attach data to table 
   * may be useless since the entries are children of table 
   * and we can get them this way 
   */
  if ( w == NULL) 
    g_object_set_data_full(G_OBJECT(table),"entries",data, g_free);
  else 
    g_object_set_data_full(G_OBJECT(w),"entries",data, g_free);
  gtk_container_set_border_width (GTK_CONTAINER (table), 5);
  /*  gtk_widget_set_size_request (table,entry_size*n,-1);
   *  gtk_window_set_default_size(GTK_WINDOW(table), entry_size*n,-1);
   */
  gtk_widget_show(table);
  /* box= gtk_vbox_new (FALSE, 0); */
  /* gtk_container_add (GTK_CONTAINER (box),table); */
  gtk_table_set_homogeneous(GTK_TABLE(table),FALSE);
  for (i= 0 ; i < n ; i++) 
    for ( j = 0 ; j < m ; j++)
      {
	GtkWidget *entry =  gtk_entry_new() ;
	data[j+m*i]=entry;
	gtk_entry_set_max_length(GTK_ENTRY(entry),0);
	gtk_widget_set_size_request (entry,entry_size,-1);
	gtk_table_attach (GTK_TABLE (table),entry,i,i+1,j,j+1,GTK_EXPAND | GTK_FILL, GTK_FILL,0,0);
	gtk_entry_set_text (GTK_ENTRY(entry),Ms[j+m*i]);
	/* gtk_entry_set_max_length (GTK_ENTRY(entry),entry_size);*/
      }
  return table;
}


/**
 * nsp_setup_matrix_wraper:
 * @w: 
 * @Ms: 
 * @m: 
 * @n: 
 * @entry_size: 
 * 
 * same as #nsp_setup_matrix_entry but the matrix table 
 * is inserted in a scrolled window if @m or @n are greater than @10.
 * 
 * Return value: the widget which contains data acces to matrix edited values.
 **/


static GtkWidget *nsp_setup_matrix_wraper(char **Ms,int m,int n,int entry_size)
{
  GtkWidget *res;
  if ( m > 10 || n > 10 ) 
    {
      GtkWidget *scrolled_win,*table;
      /* here we need a scrolled window */ 
      res = scrolled_win= gtk_scrolled_window_new (NULL, NULL);
      gtk_container_set_border_width (GTK_CONTAINER (scrolled_win), 1);
      gtk_widget_set_size_request (scrolled_win,Min(400,40*n),Min(300,30*(m+1)));
      gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				      GTK_POLICY_AUTOMATIC,
				      GTK_POLICY_AUTOMATIC);
      table = nsp_setup_matrix_entry(scrolled_win,Ms,m,n,entry_size);
      gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW (scrolled_win),table);
    }
  else 
    { 
      /* no need to add a viewport */
      GtkWidget *frame,*fvbox,*table;
      res = frame = gtk_frame_new(NULL);
      fvbox  = gtk_vbox_new (FALSE, 0);
      gtk_container_set_border_width (GTK_CONTAINER (frame),2);
      gtk_container_set_border_width (GTK_CONTAINER(fvbox),2);
      gtk_container_add (GTK_CONTAINER (frame),fvbox);
      table = nsp_setup_matrix_entry(frame,Ms,m,n,entry_size);
      gtk_container_add (GTK_CONTAINER (fvbox),table);
    }
  return res;
}

static int nsp_matrix_entry_get_values(GtkWidget *table,NspSMatrix *S)
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

/*
 * using spin button
 **/

static GtkWidget *nsp_setup_spin_button_wraper(double *val,int entry_size)
{
  /* value, lower, upper, step_increment, page_increment, page_size, climb_rate, digits*/
  GtkAdjustment *adj;
  adj = GTK_ADJUSTMENT (gtk_adjustment_new (*val,*(val+1),*(val+2),*(val+3),*(val+4),*(val+5)));
  return  gtk_spin_button_new (adj,*(val+6),*(val+7));
}

static int nsp_spin_button_get_value(GtkWidget *spin,NspMatrix *M)
{
  M->R[0] = gtk_spin_button_get_value(GTK_SPIN_BUTTON(spin));
  return OK;
}

/*
 * using range button: 
 */

static GtkWidget *nsp_setup_scale_wraper(double *val,int entry_size)
{
  GtkWidget *scale;
  /* value, lower, upper, step_increment, page_increment, page_size, climb_rate, digits*/
  GtkAdjustment *adj;
  adj = GTK_ADJUSTMENT (gtk_adjustment_new (*val,*(val+1),*(val+2),*(val+3),*(val+4),*(val+5)));
  scale= gtk_hscale_new(adj);
  gtk_scale_set_digits (GTK_SCALE(scale),*(val+7));
  gtk_scale_set_draw_value (GTK_SCALE(scale),TRUE);
  return scale;
}


static int nsp_scale_get_value(GtkWidget *scale,NspMatrix *M)
{
  GtkAdjustment *adj;
  adj= gtk_range_get_adjustment(GTK_RANGE(scale));
  M->R[0] = gtk_adjustment_get_value(adj);
  return OK;
}

/* A button used to recursively open a new 
 * nsp_choices_with_combobox.
 */

static void button_clicked(GtkWidget *widget, void *Obj)
{
  menu_answer ans;
  ans=nsp_choices_with_combobox("Test",Obj,NULL,FALSE);
  switch (ans) 
    {
    case menu_ok:
    case menu_cancel:
    case menu_fail: 
    default:
      break;
    }
}

#ifndef GTK_STOCK_EDIT 
#define GTK_STOCK_EDIT GTK_STOCK_OK
#endif 



static GtkWidget *nsp_setup_choice_button(void *Obj,int entry_size)
{
  GtkWidget *button;
  button = gtk_button_new_from_stock(GTK_STOCK_EDIT);
  g_object_set_data(G_OBJECT(button),"listarg",Obj);
  g_signal_connect (button,"clicked",G_CALLBACK (button_clicked),Obj);
  return button;
}
  
