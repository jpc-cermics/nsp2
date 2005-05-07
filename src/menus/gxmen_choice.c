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

#define GTK_ENABLE_BROKEN
#include <stdio.h>
#include <gtk/gtk.h>
#include "nsp/menus.h"
#include "nsp/gtksci.h"

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
      entry_text = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(info->choice.combo)->entry));
      for (j = 0; j < info->choice.num_toggles; ++j) 
	{ 
	  if ( strcmp(entry_text,info->name[j]) == 0) 
	    {
	      info->choice.default_toggle = j;
	      break;
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

/*---------------------------------------------------------------
 * The x_choice interaction window 
 * returns OK or FAIL 
 * when OK results are stored in defval 
 *    if defval[0]= -1 it was a cancel 
 *    else defval contains the user choices 
 *---------------------------------------------------------------*/

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

  gtk_window_set_title   (GTK_WINDOW (window),"Scilab dialog");
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
	info->choice.label = gtk_label_new (&info->choice.name[7]);
      else 
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

  /* ok */ 

  hbbox = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (vbox), hbbox, FALSE, FALSE , 2);
  gtk_widget_show (hbbox);

  button_ok = gtk_button_new_with_label ("OK");
  gtk_container_add (GTK_CONTAINER (hbbox), button_ok);

  gtk_signal_connect (GTK_OBJECT (button_ok), "clicked",
		      GTK_SIGNAL_FUNC (sci_choices_ok),
		      &rep);

  GTK_WIDGET_SET_FLAGS (button_ok, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (button_ok);
  gtk_widget_show (button_ok);

  /* cancel */

  button_cancel = gtk_button_new_with_label ("Cancel");
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


/*
 * checks for color status 
 * checks if we have in our toggle list 
 *   a list with colored toggles buttons 
 */

/* 
static int choices_cmap(void)
{
  int Nchoices=0,i,flag=0 ;			
  while ( choices_data[Nchoices] != (SciComboData *) NULL ) Nchoices++;
  for (i=0 ; i < Nchoices ; ++i) 
    { 
      if ( strncmp(choices_data[i]->choice.name,"colors",6)==0)
	flag=1;
  }
  return(flag);
}

*/

