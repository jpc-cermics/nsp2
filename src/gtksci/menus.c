/* Nsp
 * Copyright (C) 2001-2005 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/graphics/periGtk.h" 
#include "nsp/menus.h" 
#include "nsp/math.h"
#include "../system/files.h" 
#include "nsp/sciio.h" 
#include "nsp/gtksci.h"

extern void create_nsp_about(void); 
extern char GetDriver();

static void *sci_window_initial_menu(void) ;
static void sci_menu_to_item_factory(GtkItemFactory *ifactory,menu_entry *m);
static void sci_menu_delete(menu_entry **m,const char *name) ;
static int sci_menu_add(menu_entry **m,int winid,const char *name,char** entries,int ne,int action_type,char *fname);
static menu_entry * sci_menu_set_status(menu_entry *m,int winid,const char *name,int subid,int status);
static int call_predefined_callbacks(char *name, int winid);
static void sci_factory_add_menu_entry(GtkItemFactory *ifactory,menu_entry *m);
static void sci_factory_add_last_menu_entry(GtkItemFactory *ifactory,menu_entry *m);

/*--------------------------------------------------------------
 * main menu. i.e the menu of the main scilab window 
 *            this menu is attached to a zterm through a plug widget
 *--------------------------------------------------------------*/

static menu_entry *main_menu_entries = NULL;
static GtkItemFactory  *main_item_factory= NULL;

/*
 * used when the menu is plugged 
 */

void create_plugged_main_menu(void)
{
  static GtkWidget *menubar = NULL; 
  static int first = 0; 
  static GtkWidget *Plug;
  static GtkItemFactory *item_factory;
  GtkAccelGroup *accel_group = NULL ; 
  char * plug_info = getenv("SCIWIN");

  if ( plug_info == NULL) return ;

  main_item_factory= item_factory = gtk_item_factory_new (GTK_TYPE_MENU_BAR, "<main>", 
							  accel_group);
  
  if ( first == 0 ) {
    Plug = gtk_plug_new(atoi(getenv("SCIWIN")));
    main_menu_entries = sci_window_initial_menu();
    if ( main_menu_entries == NULL) return;
    first = 1;
  }

  /* This function generates the menu items from scilab description */
  /* Attention il faut aussi gerer les menu en set unset XXXXX */

  sci_menu_to_item_factory(item_factory, main_menu_entries);
  
  /* Attach the new accelerator group to the window. */
  /* gtk_window_add_accel_group (GTK_WINDOW (window), accel_group); */ 
  
  /* Finally, return the actual menu bar created by the item factory. */ 

  if ( menubar != NULL) gtk_widget_destroy(menubar);
  menubar = gtk_item_factory_get_widget (item_factory, "<main>");
  gtk_container_add(GTK_CONTAINER(Plug),menubar);
  if ( accel_group != NULL ) 
    gtk_window_add_accel_group (GTK_WINDOW (Plug), accel_group);
  gtk_widget_show_all(Plug);

}

/*
 * used when zterm is plugged 
 */

GtkWidget *create_main_menu( GtkWidget  *window)
{
  static int first = 0;
  static GtkWidget *menubar = NULL; 
  static GtkItemFactory *item_factory;
  GtkAccelGroup *accel_group = NULL ; 
  
  /* Make an accelerator group (shortcut keys) */
  if ( window != NULL)  accel_group = gtk_accel_group_new ();

  main_item_factory= item_factory = gtk_item_factory_new (GTK_TYPE_MENU_BAR, "<main>", 
							  accel_group);
  if ( first == 0 ) {
    main_menu_entries = sci_window_initial_menu();
    if ( main_menu_entries == NULL) return NULL;
    first = 1;
  }

  /* This function generates the menu items from scilab description */
  /* Attention il faut aussi gerer les menu en set unset XXXXX */

  sci_menu_to_item_factory(item_factory, main_menu_entries);
  
  /* Finally, return the actual menu bar created by the item factory. */ 
  if ( menubar != NULL) gtk_widget_destroy(menubar);
  menubar = gtk_item_factory_get_widget (item_factory, "<main>");
  if ( window != NULL )  gtk_window_add_accel_group (GTK_WINDOW (window), accel_group);
  return menubar;

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
  if ( main_item_factory == NULL ) return;
  sprintf( gwin_name, "Graphic Window %d", (int) lab_count );
  nsp_menus_delete_button(w, gwin_name);
  sprintf( gwin_name, "Graphic Window %d", (int) ivalue );
  lab_count = ivalue;
  sci_menu_add(&main_menu_entries,-1,gwin_name,
	       graphic_entries,5,0,"$graphic_window");
  sci_factory_add_last_menu_entry(main_item_factory,main_menu_entries);
}


/*--------------------------------------------------------------
 * Graphic window menu 
 *--------------------------------------------------------------*/

void create_graphic_window_menu(BCG *dd)
{
  GtkAccelGroup *accel_group=  gtk_accel_group_new ();

  /* Attach the new accelerator group to the window. */
  gtk_window_add_accel_group (GTK_WINDOW (dd->private->window), accel_group);

  dd->private->item_factory = gtk_item_factory_new (GTK_TYPE_MENU_BAR, "<main>", 
					   accel_group);
  
  /* This function generates the menu items from scilab description */
  /* Attention il faut aussi gerer les menu en set unest XXXXX */
  
  sci_menu_to_item_factory(dd->private->item_factory,dd->private->menu_entries);
  
  /* Finally, return the actual menu bar created by the item factory. */ 

  dd->private->menubar = gtk_item_factory_get_widget (dd->private->item_factory, "<main>");
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
  GtkItemFactory  *item_factory;
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
      item_factory = main_item_factory; 
      sci_menu_delete(&main_menu_entries,button_name);
    }
  else 
    {
      BCG *dd = window_list_search(win_num);
      if ( dd == NULL || dd->private->item_factory == NULL) return 0;
      item_factory = dd->private->item_factory;
      sci_menu_delete(&dd->private->menu_entries,button_name);
    }
  gtk_item_factory_delete_item (item_factory,btn);
  return 0;
}

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
      if ( main_item_factory == NULL ) return OK;
      if ( sci_menu_add(&main_menu_entries,win_num,button_name,entries,ne,typ,fname) == 1 ) 
	{
	  return FAIL;
	}
      sci_factory_add_last_menu_entry(main_item_factory,main_menu_entries);
    }
  else 
    {
      BCG *dd = window_list_search(win_num);
      if ( dd == NULL || dd->private->item_factory == NULL ) return OK;
      if ( sci_menu_add(&dd->private->menu_entries,win_num,button_name,entries,
			ne,typ,fname) == 1 ) 
	{
	  return FAIL;
	}
      sci_factory_add_last_menu_entry(dd->private->item_factory,dd->private->menu_entries);
    }
  return OK;
}

/*--------------------------------------------------
 * Activate or deactivate a menu 
 *---------------------------------------------------*/

static void nsp_menus_set_unset(int win_num,const char *button_name,int ne,int flag)
{ 
  menu_entry *e,*entries;
  GtkItemFactory  *item_factory;
  if ( win_num == -1 ) 
    {
      item_factory = main_item_factory; 
      entries =main_menu_entries;
    }
  else 
    {
      BCG *dd = window_list_search(win_num);
      if ( dd == NULL || dd->private->item_factory == NULL) return ;
      item_factory = dd->private->item_factory;
      entries = dd->private->menu_entries;
    }
  
  e = sci_menu_set_status(entries,win_num,button_name,ne,flag);
  if ( e != NULL) 
    {
      GtkWidget *w;
      char buf[128];
      if ( ne == 0)
	{ 
	  /* top menu */ 
	  char *loc = e->name, *pbuf; 
	  strcpy(buf,"<main>/");
	  pbuf = buf + strlen(buf);
	  while ( *loc != '\0' ) 
	    { 
	      if ( *loc != '_' ) { *pbuf = *loc ; pbuf++; loc++;} 
	      else loc++;
	    }
	  *pbuf = '\0';
	}
      else 
	{
	  /* sub_menu */
	  char *loc = e->menu->name, *pbuf; 
	  strcpy(buf,"<main>/");
	  pbuf = buf + strlen(buf);
	  while ( *loc != '\0' ) 
	    { 
	      if ( *loc != '_' ) { *pbuf = *loc ; pbuf++; loc++;} 
	      else loc++;
	    }
	  *pbuf = '/';pbuf++;
	  loc = e->name ; 
	  while ( *loc != '\0' ) 
	    { 
	      if ( *loc != '_' ) { *pbuf = *loc ; pbuf++; loc++;} 
	      else loc++;
	    }
	  *pbuf = '\0';
	}
      w = gtk_item_factory_get_widget (item_factory,buf);
      /* rend le menu non sensitif */
      if ( w != NULL) 
	{
	  if ( flag == TRUE ) 
	    gtk_widget_set_sensitive (w, TRUE);
	  else 
	    gtk_widget_set_sensitive (w, FALSE);
	}
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

static void sci_menu_delete(menu_entry **m,const char *name) 
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
	  menu_entry_delete(nloc);
	  return ;
	}
      loc = nloc;
      nloc= nloc->next ;
    }
}

/*----------------------------------------------------------------
 * Set the status of a menu 
 *----------------------------------------------------------------*/

static menu_entry * sci_menu_set_status(menu_entry *m,int winid,const char *name,int subid,int status)
{  
  menu_entry *loc = m ;
  while ( loc != NULL) 
    {
      if ( is_menu_name(loc->name,name)==0) 
	{
	  if ( subid == 0) 
	    {
	      loc->status = status ;
	      return loc;
	    }
	  else 
	    {
	      int count;
	      /* walk to find submenu number subid */
	      loc = loc->subs; 
	      for ( count = 0 ; count < subid -1 ; count++) 
		loc = (loc == NULL) ? NULL : loc->next ;
	      if ( loc == NULL) return NULL ;
	      loc->status = status;
	      return loc;
	    }
	}
      loc = loc->next ;
    }
  return NULL;
}

/*------------------------------------------------------
 * menu default callback 
 *------------------------------------------------------*/

static void sci_menu_default_callback (gpointer  callback_data, guint callback_action,GtkWidget  *widget)
{
  static char buf[256];
  menu_entry *m = (menu_entry *) callback_data;
#ifdef DEBUG
  fprintf(stdout,"menu activated \"%s\"", gtk_item_factory_path_from_widget (widget));
#endif 
  if ( m== NULL) return ;
  /* 
     fprintf(stdout,"name = %s ",m->name);
     fprintf(stdout,"status %d nsub=%d win=%d action=%d fname=%s\n",
     m->status,m->nsub,m->winid,m->action_type,m->fname);
  */

  if ( call_predefined_callbacks(m->fname, m->winid)==1) return ;

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

/*------------------------------------------------------
 * fill an item factory with a menu_entry description 
 *------------------------------------------------------*/

static void sci_menu_to_item_factory(GtkItemFactory *ifactory,menu_entry *m)
{
  while ( m != NULL) 
    {
      sci_factory_add_menu_entry(ifactory,m);
      m= m->next;
    }
}

/*-------------------------------------------------------------------
 * build items associated to the last menu_entry contained in m 
 * and add them in the factory ifactory 
 *-------------------------------------------------------------------*/

static void sci_factory_add_last_menu_entry(GtkItemFactory *ifactory,menu_entry *m)
{
  if ( m == NULL ) return ;
  while ( m->next != NULL) m = m->next ; 
  sci_factory_add_menu_entry(ifactory,m);
}

/*-------------------------------------------------------------------
 * build items associated to the first menu_entry contained in m 
 * and add them in the factory ifactory 
 *-------------------------------------------------------------------*/

static void sci_factory_add_menu_entry(GtkItemFactory *ifactory,menu_entry *m)
{
  char buf_path[128];
  GtkItemFactoryEntry entry = { NULL,NULL, sci_menu_default_callback,0,NULL};
  if ( m == NULL ) return ;
  sprintf(buf_path,"/%s",m->name);
  entry.path = buf_path;
  entry.accelerator = m->accel;
  entry.extra_data = m->stock_name;
  if ( m->subs == NULL) 
    {
      if ( entry.extra_data == NULL ) 
	{
	  entry.item_type = "<Item>";
	  gtk_item_factory_create_item(ifactory,&entry,(void *)m,1);
	}
      else 
	{
	  entry.item_type = "<StockItem>";
	  gtk_item_factory_create_item(ifactory,&entry,(void *)m,1);
	}
    }
  else 
    {
      menu_entry *loc;
      if ( is_menu_name(m->name,"Help")==0) 
	entry.item_type = "<LastBranch>";
      else 
	entry.item_type = "<Branch>";
      entry.callback = NULL;
      gtk_item_factory_create_item(ifactory,&entry,(void *)m,1);
      loc =  m->subs ; 
      while ( loc != NULL) 
	{
	  entry.extra_data = loc->stock_name;
	  entry.item_type =  ( entry.extra_data == NULL ) ?  "<Item>" :  "<StockItem>";
	  sprintf(buf_path,"/%s/%s",m->name,loc->name);
	  entry.path = buf_path;
	  entry.accelerator = loc->accel;
	  entry.callback = sci_menu_default_callback;
	  gtk_item_factory_create_item(ifactory,&entry,(void*)loc,1);
	  loc = loc->next;
	}
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

static void * sci_window_initial_menu(void)
{
  menu_entry *m = NULL;
  int winid = -1;
  char *file_entries[] = { "File _Operations||$fileops",
			   "_Kill||$kill",
			   "_Quit|<control>Q|$quit|gtk-quit" } ;
  char *control_entries[] = { "Resume||$resume",
			      "Abort||$abort",
			      "Restart||$restart",
			      "Stop||$stop" } ;
  char *graphic_entries[] = { "Create or Select||$gwselect",
			      "Raise||$gwraise", 
			      "Delete||$gwdelete",
			      "+||$gwplus" ,
			      "-||$gwminus" } ;
	
  char *help_entries[] = { "Nsp Help|F1|$help|gtk-help",
			   "About||$about|gtk-about"};
  sci_menu_add(&m,winid,"_File",file_entries,3,0,"$file");
  sci_menu_add(&m,winid,"_Control",control_entries,4,0,"$zoom");
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

static void nspg_menu_erase(int winid) 
{
  scig_erase(winid);
}

/*----------------------------------------------
 * To select the graphic window 
 -----------------------------------------------*/

static void nspg_menu_select(int winid)
{
  scig_sel(winid);
}

/*----------------------------------------------
 * To delete the graphic window 
 -----------------------------------------------*/

static void nspg_menu_delete(int winid) 
{
  scig_delete(winid);
}

/*-----------------------------------------------------------------*
 * Replot in Postscript style and send to printer 
 *-----------------------------------------------------------------*/

static void nspg_menu_print(int winid)
{
  char *printer,*p1;
  int colored,orientation,type;
  if ( nsp_print_dialog(&printer,&colored,&orientation,&type)== FAIL) return;
  if ( ( p1 = getenv("TMPDIR"))  == (char *) 0 )
    {
      sciprint("Cannot find environment variable TMPDIR\r\n");
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
}

/* for use inside menus */

void nspg_print(int winid) 
{
  nspg_menu_print( winid);
}

/*-----------------------------------------------------------------
 * Replot in Postscript or Xfig style and save 
 *-----------------------------------------------------------------*/

static void nspg_menu_saveps(int winid) 
{
  char *fname;
  integer colored,orientation,type;
  if ( nsp_export_dialog(&fname,&colored,&orientation,&type)== FAIL) return;
  switch (type ) 
    {
    case 0 : /* "Postscript" */
    case 2 : /* "Postscript LaTeX" */
      switch ( orientation ) 
	{
	case 0: scig_tops(winid,colored,fname,"Pos",'l');break;
	case 1: scig_tops(winid,colored,fname,"Pos",'p');break;
	case 2: scig_tops(winid,colored,fname,"Pos",'k');break;
	}    
      break;
    case 1 : /* "Postscript No Preamble" */
      scig_tops(winid,colored,fname,"Pos",'n');
      break;
    case 3 : /* Xfig */
      scig_tops(winid,colored,fname,"Fig",'n');
      break;
    case 4 : /* Gif */
      scig_tops(winid,colored,fname,"GIF",'n');
      break;
    case 5 : /* PPM */
      scig_tops(winid,colored,fname,"PPM",'n');
      break;
    }
  nsp_string_destroy(&fname);
}

/* for use inside menus */

static void nspg_menu_export(int winid)
{
  nspg_menu_saveps(winid) ;
}

/*-----------------------------------------------------------------*
 * Binary File save 
 *-----------------------------------------------------------------*/

static void nspg_menu_save(int winid) 
{
  char *filename;
  if ((filename = nsp_get_filename_save("Save Graphic File",NULL)) != NULL) 
    {
      /* faie un scig_save XXXXX */
      scig_savesg(filename,winid);
    }
}

/*-----------------------------------------------------------------*
 * Binary File load 
 *-----------------------------------------------------------------*/

static void nspg_menu_load(int winid) 
{
  char *filename;
  static char *filters[] ={"Nsp binary graphics","*.scg",NULL};
  if ((filename = nsp_get_filename_open("Load Graphic File",NULL,filters)) != NULL) 
    {
      scig_loadsg(winid, filename);
    }
}

/*-----------------------------------------------------------------*
 * file operations 
 *-----------------------------------------------------------------*/

static void nsp_menu_fileops(void)
{
  char * file = NULL ;
  int rep,ierr;
  rep=nsp_get_file_window("File operations",NULL,1,&file,&ierr);
  if ( ierr == 0 && rep == TRUE ) 
    { 
      enqueue_nsp_command(file);
      FREE(file); 
    }
}

/*-----------------------------------------------------------------**
 * 2D Zoom calback 
 *-----------------------------------------------------------------*/

static void nspg_menu_zoom(int winid) 
{
  int ne=0;
  nsp_menus_set_unset(winid,"Zoom",ne,FALSE);
  nsp_menus_set_unset(winid,"3D Rot.",ne,FALSE);
  nsp_menus_set_unset(winid,"UnZoom",ne,FALSE);
  nsp_menus_set_unset(winid,"File",ne,FALSE);
  scig_2dzoom(winid);
  nsp_menus_set_unset(winid,"Zoom",ne,TRUE);
  nsp_menus_set_unset(winid,"3D Rot.",ne,TRUE);
  nsp_menus_set_unset(winid,"UnZoom",ne,TRUE);
  nsp_menus_set_unset(winid,"File",ne,TRUE);
}


/*-----------------------------------------------------------------
 * Unzoom Callback 
 *-----------------------------------------------------------------*/

static void nspg_menu_unzoom(int winid) 
{
  integer ne=0;
  nsp_menus_set_unset(winid,"UnZoom",ne,FALSE);
  scig_unzoom(winid);
  nsp_menus_set_unset(winid,"UnZoom",ne,TRUE);
}


/*-----------------------------------------------------------------
 * 3D Rotation callback 
 *-----------------------------------------------------------------*/

static void nspg_menu_rot3d(int winid) 
{
  integer ne=0;
  nsp_menus_set_unset(winid,"3D Rot.",ne,FALSE);
  nsp_menus_set_unset(winid,"UnZoom",ne,FALSE);
  nsp_menus_set_unset(winid,"Zoom",ne,FALSE);
  nsp_menus_set_unset(winid,"File",ne,FALSE);
  scig_3drot(winid);
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
  /* if (get_is_reading()) write_scilab(""); */
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
  enqueue_nsp_command("help();");
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
  scig_sel(lab_count);
}

static void nsp_menu_gwraise(void)
{
  scig_raise(lab_count);
}

static void nsp_menu_gwdelete(void)
{
  nspg_menu_delete(lab_count);
}


/*-----------------------------------------------------------------
 * Execute predefined callbacks 
 *-----------------------------------------------------------------*/


static int call_predefined_callbacks(char *name, int winid)
{
  if      (strcmp(name,"$clear")== 0)  nspg_menu_erase(winid);
  else if (strcmp(name,"$select")== 0) nspg_menu_select(winid) ;
  else if (strcmp(name,"$print")== 0)  nspg_menu_print(winid); 
  else if (strcmp(name,"$export")== 0) nspg_menu_export(winid);
  else if (strcmp(name,"$save")== 0)   nspg_menu_save(winid);
  else if (strcmp(name,"$load")== 0)   nspg_menu_load(winid);
  else if (strcmp(name,"$close")== 0)  nspg_menu_delete(winid);
  else if (strcmp(name,"$zoom")== 0)   nspg_menu_zoom(winid);
  else if (strcmp(name,"$unzoom")== 0) nspg_menu_unzoom(winid);
  else if (strcmp(name,"$rot3d")== 0)  nspg_menu_rot3d(winid);
  else if (strcmp(name,"$help")== 0)   nsp_menu_help();
  else if (strcmp(name,"$stop")== 0)   nsp_menu_stop();
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
  else if (strcmp(name,"$restart")== 0) enqueue_nsp_command("exec SCI/scilab.star;");
  else if (strcmp(name,"$quit")== 0) enqueue_nsp_command("quit;");
  else if (strcmp(name,"$scicos_stop")== 0) nsp_menu_scicos_stop ();
  else return 0;
  return 1;
}


