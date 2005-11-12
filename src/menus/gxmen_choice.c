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


/*-----------------------------------------------------------
 * new fresh version 
 *-----------------------------------------------------------*/

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

static void nsp_combo_update_choices(NspList *L,GtkWidget **array);
static GtkWidget *nsp_setup_combo_entry_text(NspSMatrix *Ms,int active);
static GtkWidget *nsp_setup_framed_combo(GtkWidget *box,char *title,NspSMatrix *Ms,int active);
static GtkWidget *nsp_setup_table_combo(GtkWidget *table,int row,char *title,NspSMatrix *Ms,int active);
static GtkWidget *nsp_setup_combo_from_list(GtkWidget *box,NspList *L,int row);

/**
 * nsp_choices_with_combobox:
 * @title: 
 * @L: 
 * 
 * choose answers in combo
 *  l1=list('choice 1',1,['toggle c1','toggle c2','toggle c3']);
 *  l2=list('choice 2',2,['toggle d1','toggle d2','toggle d3']);
 *  l3=list('choice 3',3,['toggle e1','toggle e2']);
 *  l4=list('colors choice 4',29,['']);
 *  rep=x_choices('Toggle Menu',list(l1,l2,l3,l4));
 * in case of succes L is updated with selected items (in l(i)(2))
 *
 * XXXXX : use_table is to be added and used in the interface 
 *
 * Return value:  %OK or %FAIL
 **/

int nsp_choices_with_combobox(char *title,NspList *L)
{
  GtkWidget *window,*mainbox,*button_ok,*button_cancel, *hbbox,*labelw,*table;
  int i,  n = nsp_list_length(L), use_table = 0;
  int answer = COMBO_RESET ;
  Cell *Loc= L->first;
  GtkWidget **combo_entry_array;

  start_sci_gtk(); /* be sure that gtk is started */

  if ((combo_entry_array = malloc(n*sizeof(GtkWidget *)))== NULL) return FAIL;

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
	  combo_entry_array[i]= nsp_setup_combo_from_list(table,(NspList *) Loc->O,i);
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
	  combo_entry_array[i]= nsp_setup_combo_from_list(mainbox,(NspList *) Loc->O,-1);
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

/**
 * nsp_setup_combo_entry_text:
 * @Ms: 
 * @active: 
 * 
 * returns a GtkComboBoxEntry filled with text from.
 * @active is used to set the active entry.
 * 
 * Return value: 
 **/

static GtkWidget *nsp_setup_combo_entry_text(NspSMatrix *Ms,int active)
{
  int i;
  GtkWidget *entry_box = gtk_combo_box_new_text ();
  for (i = 0 ; i < Ms->mn ; i++) 
    gtk_combo_box_append_text (GTK_COMBO_BOX (entry_box),Ms->S[i]);
  gtk_combo_box_set_active (GTK_COMBO_BOX (entry_box),Max(0,Min(Ms->mn-1,active)) );
  return entry_box;
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

static GtkWidget *nsp_setup_framed_combo(GtkWidget *box,char *title,NspSMatrix *Ms,int active)
{
  GtkWidget *tmp;
  GtkWidget *boom,*entrybox;
  if ( strncmp(title,"colors",6)==0 &&  strlen(title) > 7 )
    {
      BCG *Xgc;
      Xgc=check_graphic_window();
      entrybox = nsp_gtkcombobox_colormap_new(Xgc,active+1);
      tmp = gtk_frame_new (title+6);
    }
  else 
    {
      entrybox = nsp_setup_combo_entry_text(Ms, active);
      tmp = gtk_frame_new (title);
    }
  gtk_box_pack_start (GTK_BOX (box), tmp, FALSE, FALSE, 0);
  boom = gtk_vbox_new (FALSE, 0);
  gtk_container_set_border_width (GTK_CONTAINER (boom), 5);
  gtk_container_add (GTK_CONTAINER (tmp), boom);
  gtk_container_add (GTK_CONTAINER (boom),entrybox);
  return entrybox;
}

/* the same but we store in a table */

static GtkWidget *nsp_setup_table_combo(GtkWidget *table,int row,char *title,NspSMatrix *Ms,int active)
{
  GtkWidget *tmp;
  GtkWidget *entrybox;
  if ( strncmp(title,"colors",6)==0 &&  strlen(title) > 7 )
    {
      BCG *Xgc;
      Xgc=check_graphic_window();
      entrybox = nsp_gtkcombobox_colormap_new(Xgc,active+1);
      tmp = gtk_label_new (title+6);
    }
  else 
    {
      entrybox = nsp_setup_combo_entry_text(Ms, active);
      tmp = gtk_label_new (title);
    }
  gtk_misc_set_alignment (GTK_MISC (tmp), 0.0, 0.5);
  gtk_table_attach_defaults (GTK_TABLE (table), tmp, 0, 1, row,row+1);
  gtk_table_attach_defaults (GTK_TABLE (table), entrybox, 1, 2, row,row+1);
  return entrybox;
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

static GtkWidget *nsp_setup_combo_from_list(GtkWidget *box,NspList *L,int use_row)
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
      return nsp_setup_framed_combo(box,title,entries,active);
    }
  else 
    {
      /* here box is a table */
      return nsp_setup_table_combo(box,use_row,title,entries,active);
    }
}


static void nsp_combo_update_choices(NspList *L,GtkWidget **array)
{
  int i=0;
  Cell *Loc= L->first;
  while (Loc != NULL) 
    {
      NspMatrix *active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->O);
      active_field->R[0]  =  gtk_combo_box_get_active(GTK_COMBO_BOX (array[i]));
      Loc= Loc->next;
      i++;
    }
}


/* XXXXXX old version deprecated 
 *
 */

#define NSP_ENABLE_DEPRECATED 
#ifdef NSP_ENABLE_DEPRECATED 

static int nsp_menus_choices_alloc(char **strh1, char *str2);
/* static int choices_cmap(void); */
static int nsp_menus_choices_I(char *label, int *defval, int nitems);
static void nsp_menus_choices_Free(int nitems) ;
static int  nsp_menus_choices_Create( char **items,int defval[],int nitems);

static GtkWidget *window = NULL; 

/* Data structure to deal with a set of choices */

typedef struct {
  struct {
    char *name;          /* name of combo box */
    int  num_toggles;    /* number of choice in the combo */
    int  default_toggle; /* initial value for combo choice */ 
    GtkWidget *label;
    GtkWidget *combo;
  } choice;
  char **name;         /* table with the combo box list description */
} SciComboData;

static SciComboData ** choices_data;

/*---------------------------------------------------------------
 * data and callbacks for print and export menu  
 *---------------------------------------------------------------*/

typedef enum { pOK, pCANCEL , RESET } state; 

static void sci_choices_ok (GtkButton  *button, state * rep) 
{  
  int i = 0, j;
  G_CONST_RETURN gchar *entry_text;
  /* Loop on the combo boxes */
  while ( choices_data[i] != NULL) 
    {
      SciComboData *info = choices_data[i];
      if ( strncmp( info->choice.name,"colors",6)==0 &&  strlen(info->choice.name) > 7 )
	{
	  info->choice.default_toggle = gtk_combo_box_get_active (GTK_COMBO_BOX(info->choice.combo));
	}
      else
	{
	  entry_text = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(info->choice.combo)->entry));
	  for (j = 0; j < info->choice.num_toggles; ++j) 
	    { 
	      if ( strcmp(entry_text,info->name[j]) == 0) 
		{
		  info->choice.default_toggle = j;
		  break;
		}
	    }
	}
      i++;
    }
  gtk_widget_destroy(window); 
  *rep = pOK;  
  gtk_main_quit();
} 

static void sci_choices_cancel (GtkButton       *button, state * rep) 
{
  gtk_widget_destroy(window); 
  *rep = pCANCEL;  
  gtk_main_quit();
}

/* 
 */


int nsp_choices(char *label, char **items, int *defval, int nitems)
{
  int rep;
  start_sci_gtk(); /* be sure that gtk is started */
  if ( nsp_menus_choices_Create(items,defval,nitems) == FAIL) return FAIL;
  rep=nsp_menus_choices_I(label,defval,nitems);
  nsp_menus_choices_Free(nitems);
  return(rep);
}

static int nsp_menus_choices_I(char *label, int *defval, int nitems)
{
  int Nchoices=0, use_scrolled=0, i;
  static state rep = RESET ;
  
  GtkWidget *table;
  GtkWidget *labelw;
  GtkWidget *button_ok;
  GtkWidget *button_cancel;
  GtkWidget *vbox;
  GtkWidget *hbbox;
  GtkWidget *scrolled_win=NULL;

  rep =RESET;
  /* do not accept a reenter mode */ 
  if ( window != NULL) return FAIL ; 

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), "Scilab choices");
  gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
  gtk_window_set_wmclass  (GTK_WINDOW (window), "choices", "Scilab");

  gtk_signal_connect (GTK_OBJECT (window), "destroy",
		      GTK_SIGNAL_FUNC(sci_choices_cancel),
		      &rep);

  gtk_container_set_border_width (GTK_CONTAINER (window), 0);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 10);
  gtk_widget_show (vbox);

  /* label widget description of the choices */
  labelw = gtk_label_new (label);
  gtk_box_pack_start (GTK_BOX (vbox), labelw, FALSE, FALSE, 0);
  gtk_widget_show (labelw);

  /* table widget  of the choices */

  while ( choices_data[Nchoices] != (SciComboData *) NULL ) Nchoices++;

  if ( Nchoices  > 15 ) use_scrolled = 1;

  if ( use_scrolled ==1 ) {
    scrolled_win = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_set_border_width (GTK_CONTAINER (scrolled_win), 1);
    gtk_widget_set_usize (scrolled_win,300,300);
    gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
				    GTK_POLICY_AUTOMATIC,
				    GTK_POLICY_AUTOMATIC);
  }

  table = gtk_table_new ( Nchoices , 2, TRUE);
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
  
  for ( i = 0 ; i <  Nchoices ; i++) 
    {
      int j;
      SciComboData *info = choices_data[i];
      GList *cbitems = NULL;
      GtkWidget *combo;
      if ( strncmp( info->choice.name,"colors",6)==0 &&  strlen(info->choice.name) > 7 )
	{
	  /* a gtk box to choose a color */
	  BCG *Xgc;
	  info->choice.label = gtk_label_new (&info->choice.name[7]);
	  Xgc=check_graphic_window();
	  info->choice.combo = combo = nsp_gtkcombobox_colormap_new(Xgc,info->choice.default_toggle+1);
	  gtk_widget_show (combo);
	  gtk_widget_show (info->choice.label);
	  gtk_table_attach (GTK_TABLE (table),info->choice.label,0,1,i,i+1,
			    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
			    0,0);
	  gtk_table_attach (GTK_TABLE (table), combo,1,2,i,i+1,
			    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
			    0,0);
	}
      else 
	{
	  info->choice.label = gtk_label_new (info->choice.name);
	  /* set up the toggle widgets */
	  /* qui doit detruire cette liste ? XXXXXXX */ 
	  for (j = 0; j < info->choice.num_toggles; ++j) 
	    cbitems = g_list_append(cbitems, info->name[j]);
	  info->choice.combo = combo =  gtk_combo_new ();
	  gtk_combo_set_popdown_strings (GTK_COMBO (combo), cbitems);
	  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO(combo)->entry),
			      info->name[info->choice.default_toggle]);
	  gtk_entry_set_editable(GTK_ENTRY (GTK_COMBO(combo)->entry),FALSE);
	  gtk_widget_show (combo);
	  gtk_widget_show (info->choice.label);
	  gtk_table_attach (GTK_TABLE (table),info->choice.label,0,1,i,i+1,
			    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
			    0,0);
	  gtk_table_attach (GTK_TABLE (table), combo,1,2,i,i+1,
			    GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL,
			    0,0);
	}
    }

  /* ok */ 

  hbbox = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (vbox), hbbox, FALSE, FALSE , 2);
  gtk_widget_show (hbbox);

  button_ok = gtk_button_new_from_stock (GTK_STOCK_OK);
  gtk_container_add (GTK_CONTAINER (hbbox), button_ok);

  gtk_signal_connect (GTK_OBJECT (button_ok), "clicked",
		      GTK_SIGNAL_FUNC (sci_choices_ok),
		      &rep);

  GTK_WIDGET_SET_FLAGS (button_ok, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (button_ok);
  gtk_widget_show (button_ok);

  /* cancel */
  button_cancel = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  gtk_container_add (GTK_CONTAINER (hbbox), button_cancel);
  gtk_signal_connect (GTK_OBJECT (button_cancel), "clicked",
		      GTK_SIGNAL_FUNC (sci_choices_cancel),
		      &rep);
  GTK_WIDGET_SET_FLAGS (button_cancel, GTK_CAN_DEFAULT);
  gtk_widget_show (button_cancel);

  gtk_widget_show (window);

  while (1) 
    {
      /* here we only want to quit gtk_main after a selection in 
       */
      gtk_main();
      if ( rep != RESET ) break;
    }
  window = NULL;
  if ( rep == pOK ) 
    {
      for ( i=0 ; i < nitems ; i++) 
	{
	  defval[i]= choices_data[i]->choice.default_toggle +1;
	}
    }
  else 
    {
      /* rep == pCANCEL */
      defval[0] = -1;
    }
  return OK;
}

/*
 *   nsp_menus_choices_Create(items,defval,nitems) 
 *   This fuction is used to create the required SciComboData  
 *   Object in order to call create_choices  
 *   from a simpler data structure in order to be able  
 *   to communicate with Scilab  
 *   char*  items[] = {     "Label1",      "choice1",      "choice2",...,NULL, 
 *                          "Label2",      "choice1",      "choice2",...,NULL} 
 *   Default choices are stored in defval (numbering start at indice 1) 
 *   int  defval[]={....} 
 *   nitems : number of labels 	 
 */

static int  nsp_menus_choices_Create( char **items,int defval[],int nitems)
{
  int i,j;
  if ( choices_data != (SciComboData **) NULL) 
    {
      /** someone is using toggles at the same time */
      Scierror("x_choices: is not re-entrant\n");
      return FAIL;
    }
  choices_data= (SciComboData **) MALLOC( (nitems+1)*sizeof(SciComboData *));
  if ( choices_data == (SciComboData **) NULL) return FAIL;
  choices_data[nitems]= (SciComboData *) NULL;
  for ( i=0 ; i < nitems ; i++) 
    {
      char **loc= items ;
      int numch=0;
      while ( *loc != (char *) NULL) { loc++;numch++; };
      numch--;
      if ( numch == 0) 
	{
	  Scierror("x_choices: There's no choice to item number %d\n",i);
	  return FAIL;
	};
      choices_data[i]= (SciComboData *) MALLOC( sizeof(SciComboData));
      if ( choices_data[i] == (SciComboData *) NULL) 
	{
	  return FAIL;
	}
      if ( nsp_menus_choices_alloc(&(choices_data[i]->choice.name),items[0]) == FAIL) 
	{
	  return FAIL;
	}
      choices_data[i]->choice.num_toggles= numch;
      if ( strncmp( choices_data[i]->choice.name,"colors",6)==0 && strlen( choices_data[i]->choice.name) > 7 )
	choices_data[i]->choice.default_toggle = defval[i]-1;
      else
	choices_data[i]->choice.default_toggle = Min(Max(0,defval[i]-1),numch-1);
      choices_data[i]->choice.label = NULL;
      choices_data[i]->choice.combo = NULL;
      choices_data[i]->name = (char **) MALLOC( numch*sizeof(char *));
      if ( choices_data[i]->name == NULL)  return FAIL;
      for ( j = 0 ; j < numch ; j++) 
	{
	  if ( nsp_menus_choices_alloc(& choices_data[i]->name[j] ,items[j+1]) == FAIL ) 
	    {
	      return FAIL;
	    }
	}
      items = items + numch+2;
    }
  return OK;
}

static int nsp_menus_choices_alloc( char **strh1,char *str2)
{
  *strh1= (char *) MALLOC((strlen(str2)+1)*sizeof(char));
  if ( *strh1 == (char *) NULL) 
    {
      Scierror("x_choices: running out of space\n");
      return FAIL;
    }
  strcpy(*strh1,str2);
  return OK;
}

static void nsp_menus_choices_Free(int nitems) 
{
  int i,j;
  for ( i=0 ; i < nitems ; i++) 
    {
      for (j = 0 ; j < choices_data[i]->choice.num_toggles ; j++) 
	FREE(choices_data[i]->name[j]);
      FREE(choices_data[i]->name) ;
      FREE(choices_data[i]->choice.name);
    }
  FREE(choices_data);
  choices_data = NULL;
}

#endif 
