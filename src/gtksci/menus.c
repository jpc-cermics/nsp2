/* Nsp
 * Copyright (C) 2001-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * Nsp menu functions
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <math.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <gtk/gtk.h>

#include "nsp/command.h" 
#include "nsp/graphics-new/periGtk.h" 
#include "nsp/menus.h" 
#include "nsp/math.h"
#include "nsp/config.h"
#include "../system/files.h" 
#include "nsp/sciio.h" 
#include "nsp/gtksci.h"
#include "nsp/nsptcl.h"

extern void nsp_pa_stop(void);
extern void create_nsp_about(void); 
extern char GetDriver();

static void *nsp_window_create_initial_menu(void) ;
static void nsp_menu_delete_menuitem(menu_entry **m,const char *name) ;
static int sci_menu_add(menu_entry **m,int winid,const char *name,char** entries,int ne,int action_type,char *fname);
static int nsp_call_predefined_callbacks(BCG *Xgc, const char *name, int winid);
static void sci_menubar_add_menu_entry(BCG *Xgc,GtkWidget *menubar,menu_entry *m);
static void sci_menubar_add_last_menu_entry(BCG *Xgc, GtkWidget *menubar,menu_entry *m);
static GtkWidget *sci_menu_to_gtkmenubar(BCG *Xgc,menu_entry *m,GtkAccelGroup *accel_group);
static int is_menu_name(const char *name,const char *name1) ;
/*--------------------------------------------------------------
 * main menu. i.e the menu of the main scilab window 
 *            this menu is attached to a zterm through a plug widget
 *--------------------------------------------------------------*/

static menu_entry *main_menu_entries = NULL;
static GtkWidget  *main_menu_menubar = NULL;

/*
 * used when the menu is plugged 
 */

void create_plugged_main_menu(void)
{
  static int first = 0; 
  static GtkWidget *Plug;
  GtkAccelGroup *accel_group = NULL ; 
  char * plug_info = nsp_getenv("SCIWIN");

  if ( plug_info == NULL) return ;
  if ( first == 0 ) {
    Plug = gtk_plug_new(atoi(nsp_getenv("SCIWIN")));
    main_menu_entries = nsp_window_create_initial_menu();
    if ( main_menu_entries == NULL) return;
    first = 1;
  }

  accel_group = gtk_accel_group_new ();
  /* This function generates the menu items from scilab description */
  main_menu_menubar= sci_menu_to_gtkmenubar(NULL,main_menu_entries,accel_group);
  /* Attach the new accelerator group to the window. */
  /* gtk_window_add_accel_group (GTK_WINDOW (window), accel_group); */ 
  gtk_container_add(GTK_CONTAINER(Plug),main_menu_menubar);
  gtk_window_add_accel_group (GTK_WINDOW (Plug), accel_group);
  gtk_widget_show_all(Plug);
}

/*
 * used when zterm is plugged 
 */

GtkWidget *create_main_menu( GtkWidget  *window)
{
  static int first = 0;
  GtkAccelGroup *accel_group = NULL ; 
  
  /* Make an accelerator group (shortcut keys) */
  if ( window != NULL)  accel_group = gtk_accel_group_new ();

  if ( first == 0 ) {
    main_menu_entries = nsp_window_create_initial_menu();
    if ( main_menu_entries == NULL) return NULL;
    first = 1;
  }
  /* This function generates the menu items from scilab description */
  main_menu_menubar= sci_menu_to_gtkmenubar(NULL,main_menu_entries,accel_group);
  /* Finally, return the actual menu bar created by the item factory. */ 
  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);
  return main_menu_menubar;
}

/*--------------------------------------------------------------
 * Change the Graphic Window menu when graphic window ID changes  
 *--------------------------------------------------------------*/

static integer lab_count = 0;
static char gwin_name[100];

void MenuFixCurrentWin(int ivalue)
{
  int w=-1;
  char *graphic_entries[] = { "Create or Select||$gwselect",
			      "Raise||$gwraise", 
			      "Delete||$gwdelete",
			      "+||$gwplus" ,
			      "-||$gwminus" } ;
  if ( ivalue == lab_count ) return ; 
  if ( main_menu_menubar == NULL ) return;
  sprintf( gwin_name, "Graphic Window %d", (int) lab_count );
  nsp_menus_delete_button(w, gwin_name);
  sprintf( gwin_name, "Graphic Window %d", (int) ivalue );
  lab_count = ivalue;
  sci_menu_add(&main_menu_entries,-1,gwin_name,
	       graphic_entries,5,0,"$graphic_window");
  sci_menubar_add_last_menu_entry(NULL,main_menu_menubar,main_menu_entries);
}


/*--------------------------------------------------------------
 * Graphic window menu 
 *--------------------------------------------------------------*/

void create_graphic_window_menu(BCG *dd)
{
  GtkWidget *menubar;
  GtkAccelGroup *accel_group=  gtk_accel_group_new ();
  /* Attach the new accelerator group to the window. */
  gtk_window_add_accel_group (GTK_WINDOW (dd->private->window), accel_group);
  /* This function generates the menu items from scilab description */
  menubar= sci_menu_to_gtkmenubar(dd,dd->private->menu_entries,accel_group);
  dd->private->menubar = GTK_WIDGET(menubar);
  gtk_box_pack_start (GTK_BOX (dd->private->vbox),dd->private->menubar, FALSE, TRUE, 0);
  gtk_widget_show (dd->private->menubar);
  return ;
}


/*
 * General routines for dynamic menu item creation and deletion
 */

/*---------------------------------------------------
 * Delete the button named button_name in the menu of window 
 * number win_num 
 *---------------------------------------------------*/

int nsp_menus_delete_button(int win_num,const char *button_name)
{
  static char btn[64];
  char *p;
  const char *but= button_name;
  p = btn ; 
  *(p++) = '/';
  while ( *but != '\0' ) {
    if ( *but == '/') break ; 
    else if ( *but == '_') but++ ; 
    else { *(p++)= *(but++);}
  }
  *p = '\0';
  if ( win_num == -1 ) 
    {
      nsp_menu_delete_menuitem(&main_menu_entries,button_name);
    }
  else 
    {
      BCG *dd = window_list_search(win_num);
      if ( dd == NULL) return 0;
      nsp_menu_delete_menuitem(&dd->private->menu_entries,button_name);
    }
  return 0;
}

/*---------------------------------------------------
 * Rename a menuitem name in a menubar 
 *---------------------------------------------------*/

#if 0 
static void nsp_menus_rename_button(int win_num,const char *button_name, const char *new_name)
{
  menu_entry *m;
  static char btn[64];
  char *p;
  const char *but= button_name;
  p = btn ; 
  *(p++) = '/';
  while ( *but != '\0' ) {
    if ( *but == '/') break ; 
    else if ( *but == '_') but++ ; 
    else { *(p++)= *(but++);}
  }
  *p = '\0';
  if ( win_num == -1 ) 
    {
      m = main_menu_entries; 
    }
  else 
    {
      BCG *dd = window_list_search(win_num);
      if ( dd == NULL) return ;
      m = dd->private->menu_entries;
    }
  while ( m != NULL) 
    {
      if ( is_menu_name(m->name,btn) ==0) 
	{
	  /* rename the label or accel_label contained 
	   * inside 
	   */ 
	  return;
	}
      m = m->next;
    }
}
#endif 

/*---------------------------------------------------
 * Add a menu in  window  number wun_num or in Main window
 *  win_num     : graphic window number or -1 for main scilab window
 *  button_name : label of button
 *  entries     : labels of submenus if any
 *  ne          : number of submenus
 *  typ         : Action mode
 *                typ==0 : interpreted (execution of scilab instruction
 *  typ!=0 : hard coded a routine is called
 *  fname;      : name of the action function  
 *---------------------------------------------------*/

int nsp_menus_add(int win_num,const char * button_name,char ** entries,int ne,int typ,char *fname)
{ 
  if ( win_num == -1 ) 
    {
      /* Scilab main menu */ 
      if ( main_menu_menubar == NULL ) return OK;
      if ( sci_menu_add(&main_menu_entries,win_num,button_name,entries,ne,typ,fname) == 1 ) 
	{
	  return FAIL;
	}
      sci_menubar_add_last_menu_entry(NULL,main_menu_menubar,main_menu_entries);
    }
  else 
    {
      BCG *dd = window_list_search(win_num);
      if ( dd == NULL ) return OK;
      if ( sci_menu_add(&dd->private->menu_entries,win_num,button_name,entries,
			ne,typ,fname) == 1 ) 
	{
	  return FAIL;
	}
      sci_menubar_add_last_menu_entry(dd,dd->private->menubar,dd->private->menu_entries);
    }
  return OK;
}

/*--------------------------------------------------
 * Activate or deactivate a menu 
 *---------------------------------------------------*/

static void nsp_menus_set_unset(int win_num,const char *name,int subid,int status)
{ 
  menu_entry *entries;
  if ( win_num == -1 ) 
    {
      entries =main_menu_entries;
    }
  else 
    {
      BCG *dd = window_list_search(win_num);
      if ( dd == NULL ) return ;
      entries = dd->private->menu_entries;
    }
  /* find name in the entries */
  while ( entries != NULL) 
    {
      if ( is_menu_name(entries->name,name)==0) 
	{
	  if ( subid == 0) 
	    {
	      entries->status = status ;
	      gtk_widget_set_sensitive (entries->widget,status);
	      return;
	    }
	  else 
	    {
	      int count;
	      /* walk to find submenu number subid */
	      entries = entries->subs; 
	      for ( count = 0 ; count < subid -1 ; count++) 
		entries = (entries == NULL) ? NULL : entries->next ;
	      if ( entries == NULL) return;
	      entries->status = status;
	      gtk_widget_set_sensitive (entries->widget,status);
	      return;
	    }
	}
      entries = entries->next ;
    }
}

/* activate a menu */

void nsp_menus_set(int win_num,const char *button_name,int ne)
{
  nsp_menus_set_unset(win_num,button_name,ne,TRUE);
}

void nsp_menus_unset(int win_num,const char *button_name,int ne)
{
  nsp_menus_set_unset(win_num,button_name,ne,FALSE);
}

/*--------------------------------------------------------
 * A Set of functions to make addmenu/delmenu/set-unsetmenu 
 * work with itemfactory menus of gtk 
 * It seams that toplevel menu buttons cannot 
 * be deleted in itemfactory. 
 * thus we recreate a new menubar when menus are deleted 
 *--------------------------------------------------------*/

static void submenu_entry_set_menu(menu_entry *subs,menu_entry *father);

/* checks that name and name1 are the same taking care of _ */

static int is_menu_name(const char *name,const char *name1) 
{
  while ( *name != 0) 
    {
      if ( *name == '_' ) 
	{
	  name++;
	  if ( *name1 == '_' )  name1++;
	}
      else if ( *name != *name1 ) {
	return 1;
      }
      else {
	name++; name1++;
      }
    }
  if ( *name1 != 0) return 1;
  return 0;
}

void menu_entry_show(menu_entry *m)
{
  if ( m== NULL) return ;
  fprintf(stdout,"name = %s ",m->name);
  fprintf(stdout,"status %d nsub=%d win=%d action=%d fname=%s\n",
	  m->status,m->nsub,m->winid,m->action_type,m->fname);
  if ( m->accel != NULL) 
    fprintf(stdout,"accel %s \n",m->accel);
  if ( m->subs != NULL) 
    {
      fprintf(stdout,"{\n");
      menu_entry_show(m->subs);
      fprintf(stdout,"}\n");
    }
  menu_entry_show(m->next);
}

static menu_entry *new_menu_entry(const char *name,const char *accel,const char *stock_name,int status,int nsub,
				  menu_entry *subs,int winid, int action_type,const char *fname)
{
  menu_entry *loc; 
  if ((loc = MALLOC(sizeof(menu_entry)))== NULL) 
    return NULL;
  if ((loc->name = MALLOC((strlen(name)+1)*sizeof(char)))==NULL) 
    return NULL;
  strcpy(loc->name,name);
  if (accel != NULL) 
    {
      if ((loc->accel = MALLOC((strlen(accel)+1)*sizeof(accel)))==NULL) 
	return NULL;
      strcpy(loc->accel,accel);
    }
  else loc->accel = NULL;
  
  if ( stock_name  != NULL) 
    {
      if ((loc->stock_name = MALLOC((strlen(stock_name)+1)*sizeof(stock_name)))==NULL) 
	return NULL;
      strcpy(loc->stock_name,stock_name);
    }
  else loc->stock_name = NULL;

  if (fname != NULL) 
    {
      if ((loc->fname = MALLOC((strlen(fname)+1)*sizeof(fname)))==NULL) 
	return NULL;
      strcpy(loc->fname,fname);
    }
  else loc->fname= NULL;

  loc->widget = NULL;
  loc->status = status;
  loc->nsub = nsub;
  loc->subs = subs;
  loc->winid = winid;
  loc->action_type = action_type;
  loc->menu= NULL;
  loc->next = NULL;
  submenu_entry_set_menu(loc->subs,loc);
  return loc;
}

static void submenu_entry_set_menu(menu_entry *subs,menu_entry *father)
{
  if ( subs == NULL) return ;
  subs->menu = father ;
  submenu_entry_set_menu(subs->next,father);
}

/* recursively delete menu_entries.
 */


void menu_entry_delete(menu_entry *me)
{
  if ( me == NULL) return ;
  /* recursive free  */
  FREE(me->name); 
  FREE(me->fname); 
  FREE(me->accel); 
  FREE(me->stock_name); 
  menu_entry_delete(me->subs);
  menu_entry_delete(me->next);
  FREE(me);
}

/*
 * decode a name = "entry|accel|action|stock-name";
 * by returning pointers to entry accel and action 
 * the pointers give position in a new allocated string 
 * which is returned in entry. entry is to be freed 
 * when entry, accel and action are no more used 
 */

static void nsp_menu_decode_name(const char *name,char **entry,char **accel,char **action,char **stock_name)
{
  *accel = NULL,*action=NULL,*stock_name = NULL;
  if ((*entry=strdup(name))== NULL) return;
  *accel = strchr(*entry,'|');
  if ( *accel != NULL) 
    {
      **accel = '\0';
      (*accel)++;
      *action = strchr(*accel,'|');
      if ( *action != NULL ) 
	{ 
	  **action = '\0';
	  (*action)++;
	  if ( *action == *accel + 1) *accel = NULL;
	  *stock_name = strchr(*action,'|');
	  if ( *stock_name != NULL ) 
	    { 
	      **stock_name = '\0';
	      (*stock_name)++;
	      if ( *stock_name == *action +  1) *action = NULL;
	    }
	}
    }
}


/*----------------------------------------------------------------
 * Add a menu in a menu_item list
 *  win_num     : graphic window number or -1 for main scilab window
 *  name        : label of menu button
 *  entries     : labels of submenus if any. each entry is a string 
 *                "entry_name" or "entry_name|accelerator|specific_action"
 * 
 *  ne          : number of submenus
 *  action_type : Action mode
 *                action_type==0 : interpreted (execution of scilab instruction
 *                action_type!=0 : hard coded a routine is called
 *  fname;      : name of the action function  
 *----------------------------------------------------------------*/

static int sci_menu_add(menu_entry **m,int winid,const char *name,char** entries,int ne, 
			int action_type,char *fname)
{  
  char *e_entry,*e_accel,*e_action,*e_stock_name,*action;
  int i;
  /* here we must find the menu_entry associated to win_num */
  menu_entry *me1=NULL,*me2,*top,*subs=NULL;
  /* first build the sub_menus */
  for (i=0 ; i < ne ;i++) 
    {
      nsp_menu_decode_name(entries[i],&e_entry,&e_accel,&e_action,&e_stock_name);
      action = (e_action != NULL) ? e_action : fname ;
      me2 = new_menu_entry(e_entry,e_accel,e_stock_name,1,i+1,NULL,winid, action_type,action);
      if(e_entry != NULL) free(e_entry);
      if ( me2 == NULL) 
	{
	  return 1;
	}
      if ( i != 0) me1->next = me2;
      else { subs = me2;}
      me1=me2;
    }
  /* now the menu entry */
  nsp_menu_decode_name(name,&e_entry,&e_accel,&e_action,&e_stock_name);
  action = (e_action != NULL) ? e_action : fname ;
  top = new_menu_entry(e_entry,e_accel,e_stock_name,1,1,subs,winid,action_type,action);
  if(e_entry != NULL) free(e_entry);
  if ( top == NULL) 
    {
      return 1;
    }
  if ( *m == NULL) *m = top ;
  else 
    { 
      menu_entry *loc= *m ; 
      while (loc->next != NULL) loc=loc->next;
      loc->next = top;
    }
  return 0;
}


/*----------------------------------------------------------------
 *Delete the menu name in menu_entry list 
 *----------------------------------------------------------------*/

static void nsp_menu_delete_menuitem(menu_entry **m,const char *name) 
{ 
  menu_entry *loc,*nloc;
  if ( *m == NULL ) return ;
  /* we want to delete a toplevel menu */
  loc = *m ;
  if ( is_menu_name((*m)->name,name) ==0) 
    {
      /* we delete the first entry of m */
      *m = (*m)->next ; 
      /* change loc->next to prevent menu_entry_delete to recursively delete */
      loc->next = NULL;
      /* 
       * if ( loc->subs != NULL) gtk_menu_item_set_submenu (GTK_MENU_ITEM(loc->widget),NULL);
       */
      gtk_widget_destroy(loc->widget);
      menu_entry_delete(loc);
      return ;
    }
  nloc = loc->next;
  while ( nloc != NULL) 
    {
      if ( is_menu_name(nloc->name,name)==0) 
	{
	  loc->next = nloc->next ;
	  /* change nloc->next to prevent menu_entry_delete to recursively delete */
	  nloc->next = NULL;
	  /* 
	   * if ( nloc->subs != NULL) gtk_menu_item_set_submenu (GTK_MENU_ITEM(nloc->widget),NULL);
	   */
	  gtk_widget_destroy(nloc->widget);
	  menu_entry_delete(nloc);
	  return ;
	}
      loc = nloc;
      nloc= nloc->next ;
    }
}


/*------------------------------------------------------
 * fill an item factory with a menu_entry description 
 *------------------------------------------------------*/

static GtkWidget *sci_menu_to_gtkmenubar(BCG *Xgc,menu_entry *m,GtkAccelGroup *accel_group)
{
  GtkWidget *menubar= gtk_menu_bar_new ();
  g_object_set_data (G_OBJECT (menubar), "user_data", accel_group);
  while ( m != NULL) 
    {
      sci_menubar_add_menu_entry(Xgc,menubar,m);
      m= m->next;
    }
  return menubar;
}

/*-------------------------------------------------------------------
 * build items associated to the last menu_entry contained in m 
 * and add them in the factory ifactory 
 *-------------------------------------------------------------------*/

static void sci_menubar_add_last_menu_entry(BCG *Xgc, GtkWidget *menubar,menu_entry *m)
{
  if ( m == NULL ) return ;
  while ( m->next != NULL) m = m->next ; 
  sci_menubar_add_menu_entry(Xgc,menubar,m);
}

/*-------------------------------------------------------------------
 * build items associated to the first menu_entry contained in m 
 * and add them in the factory ifactory 
 *-------------------------------------------------------------------*/

static void nsp_menu_default_callback (GtkWidget *widget, gpointer   func_data);

static void sci_menubar_add_menu_entry(BCG *Xgc, GtkWidget *menubar,menu_entry *m)
{
  GtkAccelGroup *accel_group = g_object_get_data (G_OBJECT (menubar), "user_data");
  GtkWidget *menuitem;
  if ( m == NULL ) return ;
  if ( m->stock_name != NULL ) 
    menuitem = gtk_image_menu_item_new_from_stock (m->stock_name, NULL);
  else 
    menuitem = gtk_menu_item_new_with_mnemonic (m->name);
  if ( m->accel != NULL )
    {
      guint keyval;
      GdkModifierType mods;
      gtk_accelerator_parse (m->accel, &keyval, &mods);
      gtk_widget_add_accelerator (menuitem,"activate",accel_group,keyval,mods, GTK_ACCEL_VISIBLE);
    }
  /* keep ref in m */
  m->widget = menuitem;
  m->gc = Xgc ;
  if ( m->subs != NULL) 
    {
      menu_entry *loc;
      GtkWidget *menu;
      GtkWidget *menuitem1;
      menu = gtk_menu_new ();
      gtk_menu_set_accel_group (GTK_MENU (menu), accel_group);
      /* gtk_item_factory_create_item(ifactory,&entry,(void *)m,1); */
      loc =  m->subs ; 
      while ( loc != NULL) 
	{
	  if ( loc->stock_name != NULL ) 
	    menuitem1 = gtk_image_menu_item_new_from_stock(loc->stock_name, NULL);
	  else 
	    menuitem1 = gtk_menu_item_new_with_mnemonic(loc->name);
	  gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem1);
	  /* entry.callback = sci_menu_default_callback; */
	  if ( loc->accel != NULL ) 
	    {
	      guint keyval;
	      GdkModifierType mods;
	      gtk_accelerator_parse (loc->accel, &keyval, &mods);
	      gtk_widget_add_accelerator (menuitem1,"activate",accel_group,keyval,mods, GTK_ACCEL_VISIBLE);
	    }
	  g_signal_connect(menuitem1,  "activate",G_CALLBACK ( nsp_menu_default_callback),loc);
	  loc->widget = menuitem1;
	  loc->gc = Xgc;
	  loc = loc->next;
	}
      gtk_menu_item_set_submenu (GTK_MENU_ITEM (menuitem), menu);
    }
  else 
    {
      g_signal_connect(menuitem,  "activate",G_CALLBACK ( nsp_menu_default_callback),m); 
      /*
	g_signal_connect(menuitem,  "activate-item",G_CALLBACK ( nsp_menu_default_callback_item),m);
      */
    }
  gtk_menu_shell_append (GTK_MENU_SHELL (menubar), menuitem);
  gtk_widget_show_all(menuitem);
}


static void nsp_menu_default_callback (GtkWidget *widget, gpointer   func_data)
{
  static char buf[256];
  menu_entry *m = (menu_entry *) func_data;
  if ( m== NULL) return ;

  if ( nsp_call_predefined_callbacks(m->gc, m->fname, m->winid)==1) return ;
  if (m->action_type == 0) 
    { 
      /* Interpreted mode : we store the action on a queue */
      if ( m->winid < 0 ) 
	sprintf(buf,"execstr(%s(%d))",m->fname,m->nsub);
      else 
	sprintf(buf,"execstr(%s_%d(%d))",m->fname,m->winid,m->nsub);
      enqueue_nsp_command(buf);
    }
  else if (m->action_type == 2) 
    { 
      /* Interpreted mode : we store the action on a queue */
      if ( m->winid < 0 ) 
	sprintf(buf,"%s(%d)",m->fname,m->nsub);
      else 
	sprintf(buf,"%s(%d,%d)",m->fname,m->nsub,m->winid);
      enqueue_nsp_command(buf);
    }
  else 
    { 
      /* hard coded mode XXXX */
      Sciprintf("Hardcoded button: to be done \n");
      /*
	int rep ;
	C2F(setfbutn)(m->fname,&rep);
	if ( rep == 0) 
	F2C(fbutn)((m->fname),&(m->winid),&(m->nsub));
      */
    }
}


void * graphic_initial_menu(int winid) 
{
  menu_entry *m = NULL;
  char *file_entries[] = { "_Clear||$clear|gtk-clear",
			   "_Select||$select",
			   "_Print|<control>P|$print|gtk-print",
			   "_Export|<control>E|$export",
			   "S_ave|<control>S|$save|gtk-save",
			   "L_oad|<control>L|$load|gtk-open",
			   "C_lose||$close|gtk-close" };
  sci_menu_add(&m,winid,"_File",file_entries,7,0,"$file");
  sci_menu_add(&m,winid,"_Zoom",NULL,0,0,"$zoom");
  sci_menu_add(&m,winid,"_UnZoom",NULL,0,0,"$unzoom");
  sci_menu_add(&m,winid,"3D _Rot.",NULL,0,0,"$rot3d");
  return m;
}

static void * nsp_window_create_initial_menu(void)
{
  int n_control_entries;
  menu_entry *m = NULL;
  int winid = -1;
  char *file_entries[] = { "File _Operations||$fileops",
			   "_Edit||$editor",
			   "_Kill||$kill",
			   "_Quit|<control>Q|$quit|gtk-quit" };

  char *control_entries[] = { "Quit||$quit",
			      "Abort||$abort",
			      "Restart||$restart",
			      "Stop||$stop",
#ifdef WITH_PORTAUDIO
			      "Stop audio||$stop_audio",
#endif
 } ;

  char *graphic_entries[] = { "Create or Select||$gwselect",
			      "Raise||$gwraise", 
			      "Delete||$gwdelete",
			      "+||$gwplus" ,
			      "-||$gwminus" } ;
	
  char *help_entries[] = { "Nsp Help|F1|$help|gtk-help",
			   "About||$about|gtk-about"};
#ifdef WITH_PORTAUDIO
  n_control_entries=5;
#else 
  n_control_entries=4;
#endif 

  sci_menu_add(&m,winid,"_File",file_entries,4,0,"$file");
  sci_menu_add(&m,winid,"_Control",control_entries,n_control_entries,0,"$zoom");
  sci_menu_add(&m,winid,"_Demos",NULL,0,0,"$demos");
  sci_menu_add(&m,winid,"Graphic Window 0",graphic_entries,5,0,"$graphic_window");
  sci_menu_add(&m,winid,"_Help",help_entries,2,0,"$help");
  return m;
}

/**************************************************************
 * predefined actions for menus 
 **************************************************************/

/*-----------------------------------------------------------------
 * To clear the graphic window and clear the recorded graphics 
 * w and client_data are unused 
 *-----------------------------------------------------------------*/

static void nspg_menu_erase(BCG *Xgc, int winid) 
{
  if ( Xgc != NULL )  Xgc->actions->erase(Xgc);

}

/*----------------------------------------------
 * To select the graphic window 
 -----------------------------------------------*/

static void nspg_menu_select(BCG *Xgc, int winid)
{
  if ( Xgc != NULL )  Xgc->actions->select(Xgc);
}

/*----------------------------------------------
 * To delete the graphic window 
 -----------------------------------------------*/

static void nspg_menu_delete(BCG *Xgc, int winid) 
{
  if ( Xgc != NULL )  Xgc->actions->delete(Xgc);
}

/*-----------------------------------------------------------------*
 * Replot in Postscript style and send to printer 
 *-----------------------------------------------------------------*/

/* for use inside menus */
#if TEST_GTK_PRINT
extern  void do_print (GtkAction *action,int winid);
#endif 

static void nspg_menu_print(BCG *Xgc, int winid)
{
#if TEST_GTK_PRINT
  do_print (NULL,winid);
#else 
  char *printer,*p1;
  int colored,orientation,type;
  if ( nsp_print_dialog(&printer,&colored,&orientation,&type)== FAIL) return;
  if ( ( p1 = nsp_getenv("NSP_TMPDIR"))  == (char *) 0 )
    {
      sciprint("Cannot find environment variable NSP_TMPDIR\r\n");
    }
  Sciprintf("To be done: %s %d %d %d\n",printer,colored,orientation,type);
  nsp_string_destroy(&printer);
  /* 
     sprintf(bufname,"%s/scilab-%d",p1,(int)winid);
     scig_tops(winid,colored,bufname,"Pos",'n');
     sprintf(bufname,"$SCI/bin/scilab -%s %s/scilab-%d %s",
     (orientation == 1) ? "print_l" : "print_p",
     p1,(int)winid,printer);
     system(bufname);
  */
#endif 

}


void nspg_print(int winid) 
{
  nspg_menu_print(NULL, winid);
}

/* activate a gtk dialog fro graphic export 
 */

static void nspg_menu_export(BCG *Xgc, int winid)
{
  char *fname;
  integer colored,orientation,type;
  if ( Xgc == NULL ) return;
  if ( nsp_export_dialog(&fname,&colored,&orientation,&type)== FAIL) return;
  switch (type ) 
    {
    case 1 : /* "Postscript" */
    case 3 : /* "Postscript LaTeX" */
      switch ( orientation ) 
	{
	case 0: Xgc->actions->tops(Xgc,colored,fname,"Pos",'l', TRUE );break;
	case 1: Xgc->actions->tops(Xgc,colored,fname,"Pos",'p', TRUE);break;
	case 2: Xgc->actions->tops(Xgc,colored,fname,"Pos",'k', TRUE);break;
	}    
      break;
    case 2 : /* "Postscript No Preamble" */
      Xgc->actions->tops(Xgc,colored,fname,"Pos",'n', TRUE);
      break;
    case 4 : /* Xfig */
      Xgc->actions->tops(Xgc,colored,fname,"Fig",'n', TRUE);
      break;
    case 5 : /* Gif */
      Xgc->actions->tops(Xgc,colored,fname,"GIF",'n', TRUE);
      break;
    case 6 : /* PPM */
      Xgc->actions->tops(Xgc,colored,fname,"PPM",'n', TRUE);
      break;
    case 7 : /* cairo-pdf */
      Xgc->actions->tops(Xgc,colored,fname,"cairo-pdf",'n', TRUE);
      break;
    case 8: /* cairo-svg */
      Xgc->actions->tops(Xgc,colored,fname,"cairo-svg",'n', TRUE);
      break;
    case 9: /* cairo-ps */
      Xgc->actions->tops(Xgc,colored,fname,"cairo-ps",'n', TRUE);
      break;
    case 10: /* cairo-png */
      Xgc->actions->tops(Xgc,colored,fname,"cairo-png",'n', TRUE);
      break;
    }
  nsp_string_destroy(&fname);
}

/*-----------------------------------------------------------------*
 * Binary File save 
 *-----------------------------------------------------------------*/

static void nspg_menu_save(BCG *Xgc, int winid) 
{
  char *filename;
  if ( Xgc == NULL ) return;
  if ((filename = nsp_get_filename_save("Save Graphic File",NULL)) != NULL) 
    {
      /* nsp_gr_savesg(filename,winid); */
      Xgc->actions->savesg(Xgc,filename);
    }
}

/*-----------------------------------------------------------------*
 * Binary File load 
 *-----------------------------------------------------------------*/

static void nspg_menu_load(BCG *Xgc, int winid) 
{
  char *filename;
  static char *filters[] ={"Nsp binary graphics","*.scg",NULL};
  if ( Xgc == NULL ) return;
  if ((filename = nsp_get_filename_open("Load Graphic File",NULL,filters)) != NULL) 
    {
      Xgc->actions->loadsg(Xgc, filename);
    }
}

/*-----------------------------------------------------------------*
 * file operations 
 *-----------------------------------------------------------------*/

static void nsp_menu_fileops(void)
{
  nsp_string loc;
  char * file = NULL;
  if ((file = nsp_get_filename_open("Exec file",NULL,NULL)) == NULL) 
    return; 
  if ((loc =new_nsp_string_n(strlen(file)+strlen("exec('');"))) == (nsp_string) 0)
    return; 
  sprintf(loc,"exec('%s');",file);
  enqueue_nsp_command(loc);
  FREE(loc);
  FREE(file);
}

/*-----------------------------------------------------------------**
 * 2D Zoom calback 
 *-----------------------------------------------------------------*/

static void nspg_menu_zoom(BCG *Xgc, int winid) 
{
  int ne=0;
  if ( Xgc == NULL ) return;
  nsp_menus_set_unset(winid,"Zoom",ne,FALSE);
  nsp_menus_set_unset(winid,"3D Rot.",ne,FALSE);
  nsp_menus_set_unset(winid,"UnZoom",ne,FALSE);
  nsp_menus_set_unset(winid,"File",ne,FALSE);
  /* nsp_gr_2dzoom(winid); */
  Xgc->actions->zoom(Xgc);
  nsp_menus_set_unset(winid,"Zoom",ne,TRUE);
  nsp_menus_set_unset(winid,"3D Rot.",ne,TRUE);
  nsp_menus_set_unset(winid,"UnZoom",ne,TRUE);
  nsp_menus_set_unset(winid,"File",ne,TRUE);
}


/*-----------------------------------------------------------------
 * Unzoom Callback 
 *-----------------------------------------------------------------*/

static void nspg_menu_unzoom(BCG *Xgc, int winid) 
{
  int ne=0;
  if ( Xgc == NULL ) return;
  nsp_menus_set_unset(winid,"UnZoom",ne,FALSE);
  /* nsp_gr_unzoom(winid); */
  Xgc->actions->unzoom(Xgc);
  nsp_menus_set_unset(winid,"UnZoom",ne,TRUE);
}


/*-----------------------------------------------------------------
 * 3D Rotation callback 
 *-----------------------------------------------------------------*/

static void nspg_menu_rot3d(BCG *Xgc, int winid) 
{
  int ne=0;
  if ( Xgc == NULL ) return;
  nsp_menus_set_unset(winid,"3D Rot.",ne,FALSE);
  nsp_menus_set_unset(winid,"UnZoom",ne,FALSE);
  nsp_menus_set_unset(winid,"Zoom",ne,FALSE);
  nsp_menus_set_unset(winid,"File",ne,FALSE);
  /* nsp_gr_3drot(winid); */
  Xgc->actions->rotation(Xgc);
  nsp_menus_set_unset(winid,"3D Rot.",ne,TRUE);
  nsp_menus_set_unset(winid,"UnZoom",ne,TRUE);
  nsp_menus_set_unset(winid,"Zoom",ne,TRUE);
  nsp_menus_set_unset(winid,"File",ne,TRUE);
}


/*-----------------------------------------------------------------
 * kill scilab 
 *-----------------------------------------------------------------*/

static void nsp_menu_kill(void)
{
  sci_clear_and_exit(1);
}

/*-----------------------------------------------------------------
 * make a stop 
 *-----------------------------------------------------------------*/

static void nsp_menu_stop (void)
{
  /* int j = SIGINT; */ 
  Sciprintf("sci_menu_stop: to be done \n");
  /* XXXXX C2F(sigbas)(&j);*/
  /* if (get_is_reading()) nsp_input_feed(""); */
}

/* stop a portaudio function 
 *
 */

static void nsp_menu_stop_audio (void)
{
#ifdef WITH_PORTAUDIO
  nsp_pa_stop();
#endif 
}



/*-----------------------------------------------------------------
 * make a stop for scicos 
 *-----------------------------------------------------------------*/

extern void scicos_send_halt(void);

static void nsp_menu_scicos_stop (void)
{
  scicos_send_halt();
}

/*-----------------------------------------------------------------
 * run the help 
 *-----------------------------------------------------------------*/

static void nsp_menu_help(void)
{
#ifdef WITH_GTKHTML
  Sci_Help(NULL,NULL,NULL);
#else 
#ifdef HAVE_WEBKIT
  Sci_Help(NULL,NULL,NULL);
#else 
  Sciprintf("help is not available\n");
#endif 
#endif 
}

/* start text_view 
 *
 */

#define USE_TEXT_VIEW_EDITOR

extern void nsp_edit(char *filename,int read_only);

static void nsp_menu_start_editor(void)
{
#ifdef USE_TEXT_VIEW_EDITOR
  nsp_edit(NULL,FALSE);
#else 
  /* we could start emacs here */
  Sciprintf("no editor\n");
#endif 
}

/*-----------------------------------------------------------------
 * run the demos 
 *-----------------------------------------------------------------*/

static void nsp_menu_demos(void)
{
  enqueue_nsp_command( get_sci_data_strings(2));
}

/*-----------------------------------------------------------------
 * Callbacks for the Graphic Window main menu 
 *-----------------------------------------------------------------*/

static void nsp_menu_gwplus(void)
{
  MenuFixCurrentWin(lab_count+1); 
}

static void nsp_menu_gwminus(void)
{
  MenuFixCurrentWin(lab_count-1); 
}

static void nsp_menu_gwcreate_or_select(void)
{
  set_graphic_window(Max(lab_count,0)) ;
}

static void nsp_menu_gwraise(void)
{
  nsp_gr_raise(lab_count);
}

static void nsp_menu_gwdelete(void)
{
  nspg_menu_delete(NULL,lab_count);
}




/*-----------------------------------------------------------------
 * Execute predefined callbacks 
 *-----------------------------------------------------------------*/


static int nsp_call_predefined_callbacks(BCG *Xgc, const char *name, int winid)
{
  if      (strcmp(name,"$clear")== 0)  nspg_menu_erase(Xgc,winid);
  else if (strcmp(name,"$select")== 0) nspg_menu_select(Xgc,winid) ;
  else if (strcmp(name,"$print")== 0)  nspg_menu_print(Xgc,winid); 
  else if (strcmp(name,"$export")== 0) nspg_menu_export(Xgc,winid);
  else if (strcmp(name,"$save")== 0)   nspg_menu_save(Xgc,winid);
  else if (strcmp(name,"$load")== 0)   nspg_menu_load(Xgc,winid);
  else if (strcmp(name,"$close")== 0)  nspg_menu_delete(Xgc,winid);
  else if (strcmp(name,"$zoom")== 0)   nspg_menu_zoom(Xgc,winid);
  else if (strcmp(name,"$unzoom")== 0) nspg_menu_unzoom(Xgc,winid);
  else if (strcmp(name,"$rot3d")== 0)  nspg_menu_rot3d(Xgc,winid);
  else if (strcmp(name,"$help")== 0)   nsp_menu_help();
  else if (strcmp(name,"$stop")== 0)   nsp_menu_stop();
  else if (strcmp(name,"$stop_audio")== 0) nsp_menu_stop_audio();
  else if (strcmp(name,"$kill")== 0)   nsp_menu_kill();
  else if (strcmp(name,"$demos")== 0)  nsp_menu_demos();
  else if (strcmp(name,"$fileops")== 0) nsp_menu_fileops();
  else if (strcmp(name,"$gwselect")== 0) nsp_menu_gwcreate_or_select();
  else if (strcmp(name,"$gwraise")== 0) nsp_menu_gwraise();
  else if (strcmp(name,"$gwdelete")== 0) nsp_menu_gwdelete();
  else if (strcmp(name,"$gwplus")== 0)  nsp_menu_gwplus();
  else if (strcmp(name,"$gwminus")== 0)  nsp_menu_gwminus();
  else if (strcmp(name,"$about")== 0)  create_nsp_about ();
  else if (strcmp(name,"$resume")== 0)  enqueue_nsp_command("resume");
  else if (strcmp(name,"$abort")== 0)   enqueue_nsp_command("abort");
  else if (strcmp(name,"$restart")== 0) enqueue_nsp_command("clear();");
  else if (strcmp(name,"$quit")== 0) enqueue_nsp_command("quit;");
  else if (strcmp(name,"$scicos_stop")== 0) nsp_menu_scicos_stop ();
  else if (strcmp(name,"$editor")== 0) nsp_menu_start_editor();
  else return 0;
  return 1;
}


