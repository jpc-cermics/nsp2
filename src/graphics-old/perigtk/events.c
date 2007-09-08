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
 * Graphic library
 * jpc@cermics.enpc.fr
 * 
 *--------------------------------------------------------------------------*/

/*
 * Changes the graphic window popupname 
 */

static void nsp_event_pause(int number) ;

/* FIXME */
extern char * nsp_string_to_utf8( char *str);

static void Setpopupname(BCG *Xgc,char *string)
{ 
  char *string_utf8=  nsp_string_to_utf8(string);
  gtk_window_set_title(GTK_WINDOW(Xgc->private->window),string_utf8);
  if ( string_utf8 != string ) g_free(string_utf8);
}

static void setpopupname(BCG *Xgc,char *name)
{
  Setpopupname(Xgc,name);
}

/* event handlers 
 *
 */

typedef struct _GTK_locator_info GTK_locator_info;

struct _GTK_locator_info {
  guint win, x,y, ok;
  int getrelease,getmotion,getmen,getkey, button, mask ;
  int sci_click_activated; /* TRUE when we are in a xclick or xclick_any function */
  guint timer;
  char *str;
  int  lstr;
};

extern GTK_locator_info nsp_event_info;

/* event handler for mouse pressed 
 */

static gboolean locator_button_press(GtkWidget *widget,
				     GdkEventButton *event,
				     BCG *gc)
{
  int id=0;
  switch (event->type) 
    {
    case GDK_BUTTON_PRESS : id= event->button-1 ;break;
    case GDK_2BUTTON_PRESS : id= event->button-1 +3;break;
    case GDK_3BUTTON_PRESS : id= event->button-1 +6;break;
    default: 
      break;
    }

  if ( event->state & GDK_SHIFT_MASK) 
    {
      Sciprintf("A Shift-press\n");
    }

  if ( nsp_event_info.sci_click_activated == FALSE ) 
    {
      nsp_gwin_event ev={ gc->CurWindow,event->x, event->y,id,event->state,0,0};
      nsp_enqueue(&gc->queue,&ev);
    }
  else 
    {
      nsp_event_info.ok = 1; nsp_event_info.win=  gc->CurWindow; nsp_event_info.x = event->x; nsp_event_info.y = event->y; 
      nsp_event_info.button = id;
      nsp_event_info.mask = event->state ;
      gtk_main_quit();
    }
  return TRUE;
}

/* event handler for mouse released 
 */

static gboolean locator_button_release(GtkWidget *widget,
				       GdkEventButton *event,
				       BCG *gc)
{
  if ( nsp_event_info.sci_click_activated == FALSE || nsp_event_info.getrelease == 0 ) 
    {
      nsp_gwin_event ev={ gc->CurWindow,event->x, event->y,event->button-6 ,event->state,0,1};
      nsp_enqueue(&gc->queue,&ev);
    }
  else 
    {
      nsp_event_info.ok =1 ; nsp_event_info.win=  gc->CurWindow; nsp_event_info.x = event->x;  nsp_event_info.y = event->y;
      nsp_event_info.button = event->button -6;
      nsp_event_info.mask = event->state;
      gtk_main_quit();
    }
  return TRUE;
}

/* event handler for mouse motion 
 */

static gboolean locator_button_motion(GtkWidget *widget,
				      GdkEventMotion *event,
				      BCG *gc)
{
  gint x,y; 
  GdkModifierType state;
  if (event->is_hint)
    { 
      gdk_window_get_pointer (event->window, &x, &y, &state);
    }
  else 
    {
      x= event->x; y = event->y;
    }
  if ( nsp_event_info.sci_click_activated == FALSE || nsp_event_info.getmotion == 0 ) 
    {
      nsp_gwin_event ev={ gc->CurWindow,x, y,-1 ,event->state,1,0},evlast;
      if ( nsp_queue_empty(&gc->queue)== FALSE ) 
	{
	  /* to not keep multi motion events */
	  evlast = nsp_peekqueue(&gc->queue);
	  if (evlast.motion == TRUE )
	    {
	      evlast = nsp_dequeue(&gc->queue);
	    }
	}
      nsp_enqueue(&gc->queue,&ev);
    }
  else 
    {
      nsp_event_info.ok =1 ;  nsp_event_info.win=  gc->CurWindow; nsp_event_info.x = x;  nsp_event_info.y = y;
      nsp_event_info.button = -1;
      nsp_event_info.mask = event->state;
      gtk_main_quit();
    }
  return TRUE;
}

/* event handler for key pressed */

static gint key_press_event (GtkWidget *widget, GdkEventKey *event, BCG *gc)
{
  gint x,y; 
  GdkModifierType state;
  if (nsp_event_info.getkey == TRUE && (event->keyval >= 0x20) && (event->keyval <= 0xFF))
    {
      /* since Alt-keys and Ctrl-keys are stored in menus I want to ignore them here */
      if ( event->state != GDK_CONTROL_MASK && event->state != GDK_MOD1_MASK ) 
	{
	  gdk_window_get_pointer (gc->private->drawing->window, &x, &y, &state);
	  nsp_event_info.x=x ; nsp_event_info.y=y;
	  nsp_event_info.ok =1 ;  nsp_event_info.win=  gc->CurWindow; 
	  nsp_event_info.button = event->keyval;
	  nsp_event_info.mask = event->state;
	  gtk_main_quit();
	}
    }
  else 
    {
      gdk_window_get_pointer (gc->private->drawing->window, &x, &y, &state);
      nsp_gwin_event ev={ gc->CurWindow,x, y,event->keyval ,event->state,0,1};
      nsp_enqueue(&gc->queue,&ev);
    }
  return FALSE; /* also want other handlers to be activated */
}


/*
 * a time out to check for menu activation 
 * XXX : info.win is not correct this is to be done 
 */

static gint timeout_test (BCG *gc)
{
  if ( dequeue_nsp_command(nsp_event_info.str,nsp_event_info.lstr) == OK)
    {
      nsp_event_info.ok = 1 ; nsp_event_info.x = 0 ; nsp_event_info.y =0 ; nsp_event_info.button =  -2;
      nsp_event_info.mask = 0;
      nsp_event_info.win = (gc == NULL) ? 0 : gc->CurWindow;
      gtk_main_quit();
    }
  return TRUE;
}
  
#ifdef WITH_TK

extern int flushTKEvents();

static gint timeout_tk (void *v)
{
  flushTKEvents();
  return TRUE;
}

#endif

/* synchronously wait for an event : the three following function are 
 * to be merged to just one function 
 */

void xclick_any(BCG *Xgc,char *str, int *ibutton,int *imask, int *x1,int *yy1, int *iwin, 
		int iflag,int getmotion,int getrelease,int getkey,int lstr)
{
  int change_cursor = TRUE;
  SciClick(Xgc,ibutton,imask,x1,yy1,iwin,iflag,getmotion,getrelease,getkey,str,lstr,change_cursor);
}

void xclick(BCG * Xgc,char *str, int *ibutton,int *imask, int *x1,int *yy1,int iflag,int motion,
	    int release,int key, int istr)
{
  int change_cursor = TRUE;
  int win = ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) ? 0 : Xgc->CurWindow;
  SciClick(Xgc,ibutton,imask,x1, yy1,&win,iflag,motion,release,key,str,istr,change_cursor);
}

void xgetmouse(BCG *Xgc,char *str, int *ibutton,int *imask, int *x1, int *yy1, int usequeue,
	       int motion,int release,int key)
{
  int change_cursor = TRUE;
  int win = ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) ? 0 : Xgc->CurWindow;
  SciClick(Xgc,ibutton,imask,x1, yy1,&win,usequeue,motion,release,key,(char *) 0,0,change_cursor);
}


static void nsp_change_cursor(BCG *Xgc, int win,int wincount, int flag )
{
  GdkCursor *cursor;
  if ( win == -1 ) 
    {
      int i;
      for (i=0; i < wincount ; i++ ) 
	{
	  BCG *bcg =  window_list_search(i);
	  if ( bcg  != NULL)
	    {
	      cursor =  ( flag == 0 ) ? bcg->private->ccursor : bcg->private->gcursor;
	      gdk_window_set_cursor(bcg->private->drawing->window,cursor);
	    }
	}
    }
  else
    {
      if ( Xgc != (BCG *) 0 && Xgc->private != NULL &&  Xgc->private->drawing != NULL ) {
	cursor =  ( flag == 0 ) ? Xgc->private->ccursor : Xgc->private->gcursor;
	gdk_window_set_cursor (Xgc->private->drawing->window,cursor);
      }
    }
}

/*
 * wait for events: mouse motion and mouse press and release 
 *                  and dynamic menu activation through a timeout 
 * 
 * if iflag = 0 : clear previous mouse click 
 * if iflag = 1 : don't 
 * if getmotion = 1 : check also mouse move 
 * if getrelease=1 : check also mouse release 
 * if dyn_men = 1 ; check also dynamic menus (returns the menu code in str )
 * return value : 0,1,2 if button pressed 
 *                -5,-4,-3: if button release
 *                -100 : error or window destroyed 
 *                -2   : menu activated 
 * FIXME:  must add support for Ctrl-C ? 
 */

static void SciClick(BCG *Xgc,int *ibutton,int *imask, int *x1, int *yy1,int *iwin, int iflag, int getmotion,
		     int getrelease,int getkey, char *str, int lstr,int change_cursor)
{
#ifdef WITH_TK
  guint timer_tk;
#endif 
  GTK_locator_info rec_info ; 
  int win=*iwin,wincount=0,win1;
  if ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) {
    *ibutton = -100; *imask=0;     return;
  }

  if ( win == -1 ) 
    {
      /* we will check all the graphic windows */
      wincount = window_list_get_max_id()+1;
    }
  else 
    {
      /* just work on current win */
      win = Xgc->CurWindow;
    }
  win1= win; /* CheckClickQueue change its first argument if -1 */
  /* check for already stored event */
  if ( iflag == TRUE )
    { 
      nsp_gwin_event ev;
      if ( window_list_check_queue((win == -1 ) ? NULL: Xgc,&ev) == OK) 
	{
	  *iwin = ev.win; *x1 = ev.x ; *yy1 = ev.y ; 
	  *ibutton= ev.ibutton, *imask= ev.mask;
	  return;
	}
    }

  if ( iflag == FALSE ) window_list_clear_queue((win == -1 ) ? NULL: Xgc);

  if ( change_cursor ) nsp_change_cursor(Xgc,win,wincount,1);
  
  /* save info in local variable  */
  rec_info = nsp_event_info;
  /* set info */ 
  nsp_event_info.ok = 0 ; 
  nsp_event_info.getrelease = getrelease ; 
  nsp_event_info.getmotion   = getmotion ;
  nsp_event_info.getmen     = (lstr == 0) ? FALSE : TRUE; 
  nsp_event_info.getkey     = getkey;
  nsp_event_info.sci_click_activated = TRUE ;

  if ( nsp_event_info.getmen == TRUE ) 
    {
      /*  Check soft menu activation during xclick */ 
      nsp_event_info.timer = gtk_timeout_add(100, (GtkFunction) timeout_test, Xgc);
      nsp_event_info.str   = str;
      nsp_event_info.lstr  = lstr; /* on entry it gives the size of str buffer */
    }
  
#ifdef WITH_TK
  timer_tk=  gtk_timeout_add(100,  (GtkFunction) timeout_tk , NULL);
#endif
  
  while (1) 
    {
      gtk_main();
      /* be sure that gtk_main_quit was activated by proper event */
      if ( nsp_event_info.ok == 1 ) 
	{
	  if ( win == -1 ) break;
	  if ( nsp_event_info.win == win  ) break;
	}
    }

#ifdef WITH_TK
  gtk_timeout_remove(timer_tk);
#endif

  *x1 = nsp_event_info.x;
  *yy1 = nsp_event_info.y;
  *ibutton = nsp_event_info.button;
  *imask = nsp_event_info.mask;
  *iwin = nsp_event_info.win;
  
  /* remove timer if it was set by us */ 
  if ( nsp_event_info.getmen == TRUE )  gtk_timeout_remove (nsp_event_info.timer);

  /* take care of recursive calls i.e restore info  */
  nsp_event_info = rec_info ; 

  if ( change_cursor ) nsp_change_cursor(Xgc,win,wincount,0);
}


/* generates a pause, in micro-seconds */

#if defined(__STDC__) || defined(_IBMR2)
#include <unistd.h>  /* for usleep */
#endif 


#ifdef HAVE_USLEEP
#define USLEEP(x) usleep(x)
#else
#ifdef HAVE_SLEEP
#define USLEEP(x) sleep(x/1000000)
#else
#define USLEEP(x) x
#endif
#endif

static void xpause(int sec_time,int events)
{ 
  unsigned useconds = (unsigned) sec_time;
  if ( events == TRUE ) 
    {
      /* nsp_event_pause need milliseconds */
      nsp_event_pause(useconds/1000) ;
    }
  else 
    {
      USLEEP(useconds);
    }
}

/* set a flag for enabling or diabling 
 * window destruction from delete or destroy ?
 */

static void xset_win_protect( BCG *gc, int val) { gc->private->protect=val;}

/* ici normalement on peut pas arreter la destruction */

static void sci_destroy_window (GtkWidget *widget,  BCG *gc)
{
  if ( gc->private->protect == TRUE ) 
    {
      xinfo(gc,"Cannot destroy window while acquiring zoom rectangle ");
    }
  if ( nsp_event_info.sci_click_activated == TRUE ) 
    {
      nsp_event_info.ok =1 ;  nsp_event_info.win=  gc->CurWindow; nsp_event_info.x = 0 ;  nsp_event_info.y = 0;
      nsp_event_info.button = -100;
      delete_window(gc,gc->CurWindow);
      gtk_main_quit();
    }
  else 
    {
      delete_window(gc,gc->CurWindow);
    }
}

/* ici avec la valeur renvoyée on peut décider de detruire ou pas */

static gboolean sci_delete_window (GtkWidget *widget, GdkEventKey *event,  BCG *gc)
{
  if ( gc->private->protect == TRUE ) 
    {
      xinfo(gc,"Cannot destroy window while acquiring zoom rectangle ");
      return TRUE;
    }
  if ( nsp_event_info.sci_click_activated == TRUE ) 
    {
      nsp_event_info.ok =1 ;  nsp_event_info.win=  gc->CurWindow; nsp_event_info.x = 0 ;  nsp_event_info.y = 0;
      nsp_event_info.button = -100;
      delete_window(gc,gc->CurWindow);
      gtk_main_quit();
    }
  else 
    delete_window(gc,gc->CurWindow);
  return FALSE;
}

/*
 * writes a message in the info widget associated to the current scilab window 
 */

#define MAXPRINTF 512

static void xinfo(BCG *Xgc,char *format,...) 
{
  /* Extended call for C calling */
  /* Arg args[1];*/
  va_list ap;
  char buf[MAXPRINTF];
  va_start(ap,format);
  (void ) vsprintf(buf, format, ap );
  va_end(ap);
  if ( Xgc != (BCG *) 0 && Xgc->private->CinfoW != NULL)
    {
      gtk_statusbar_pop ((GtkStatusbar *) Xgc->private->CinfoW, 1);
      gtk_statusbar_push ((GtkStatusbar *) Xgc->private->CinfoW, 1,buf);
    }
}


#ifndef PERIGL
int window_list_check_top(BCG *dd,void *win) 
{
  return dd->private->window == (GtkWidget *) win ;
}
#endif 

/* delete the graphic window 
 *
 */

static void delete_window(BCG *dd,int intnum)
{ 
  BCG *winxgc= dd; 
  int top_count;
  if ( dd == NULL) 
    {
      if ((winxgc = window_list_search(intnum)) == NULL) return;
    }
  /* be sure to clear the recorded graphics */
  scig_erase(intnum);

  /* I delete the pixmap and the widget */
  if ( winxgc->CurPixmapStatus == 1 ) 
    {
      /* switch to non extra pixmap mode */
      gdk_pixmap_unref(winxgc->private->extra_pixmap);
      winxgc->private->extra_pixmap = NULL;
      winxgc->private->drawable = NULL;
      winxgc->CurPixmapStatus = 0; 
    }
  if ( winxgc->private->extra_pixmap != NULL) 
    {
      /* we can have a non null extra_pixmap */
      gdk_pixmap_unref(winxgc->private->extra_pixmap);
    }
  /* deconnect handlers */
  scig_deconnect_handlers(winxgc);
  /* backing store private->pixmap */
  if (winxgc->private->pixmap != NULL)  gdk_pixmap_unref(winxgc->private->pixmap);
  /* destroy top level window if it is not shared by other graphics  */
  top_count = window_list_search_toplevel(winxgc->private->window); 
  if ( top_count <= 1) 
    {
      gtk_widget_destroy(winxgc->private->window);
    }
  else 
    {
      GtkWidget *father; 
      gtk_widget_hide(GTK_WIDGET(winxgc->private->drawing)); 
      gtk_widget_hide(GTK_WIDGET(winxgc->private->scrolled)); 
      gtk_widget_hide(GTK_WIDGET(winxgc->private->CinfoW)); 
      gtk_widget_hide(GTK_WIDGET(winxgc->private->vbox));      
      father = gtk_widget_get_parent(GTK_WIDGET(winxgc->private->vbox));
      gtk_container_remove(GTK_CONTAINER(father),GTK_WIDGET(winxgc->private->vbox));
    }
  /* free gui private area */
  FREE(winxgc->private->colors);
  /* free data associated to menus */
  menu_entry_delete(winxgc->private->menu_entries);
  if (winxgc->private->gcursor != NULL) gdk_cursor_unref (winxgc->private->gcursor);
  if (winxgc->private->ccursor != NULL)gdk_cursor_unref (winxgc->private->ccursor);
  if (winxgc->private->stdgc != NULL)g_object_unref(winxgc->private->stdgc);
  if (winxgc->private->wgc != NULL)g_object_unref(winxgc->private->wgc);
  if (winxgc->private->item_factory != NULL) g_object_unref(winxgc->private->item_factory);
  nsp_fonts_finalize(winxgc);
  FREE(winxgc->private);
  /* remove current window from window list */
  window_list_remove(intnum);
}


static void scig_deconnect_handlers(BCG *winxgc)
{
  int n=0;
  n+=g_signal_handlers_disconnect_by_func(GTK_OBJECT(winxgc->private->drawing),
					  (GtkSignalFunc) configure_event, (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func(GTK_OBJECT(winxgc->private->drawing),
					  (GtkSignalFunc) expose_event, (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func(GTK_OBJECT(winxgc->private->window),
					  (GtkSignalFunc)  sci_destroy_window, (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func (GTK_OBJECT (winxgc->private->window),
					   (GtkSignalFunc) key_press_event, (gpointer) winxgc);

  n+=g_signal_handlers_disconnect_by_func(GTK_OBJECT(winxgc->private->drawing),
					  (GtkSignalFunc) locator_button_press, (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func(GTK_OBJECT(winxgc->private->drawing),
					  (GtkSignalFunc) locator_button_release, (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func(GTK_OBJECT(winxgc->private->drawing),
					  (GtkSignalFunc) locator_button_motion, (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func(GTK_OBJECT(winxgc->private->drawing),
					  (GtkSignalFunc) realize_event, (gpointer) winxgc);
}





/**
 * nsp_event_pause:
 * @number: 
 * 
 * make a pause of @number milliseconds and if flag is true 
 * gtk_events are activated during the pause. 
 *
 **/

static gint timeout_pause (int *stop)
{
  *stop = TRUE;
  gtk_main_quit();
  return TRUE;
}

static void nsp_event_pause(int number) 
{
  int stop = FALSE;
  guint tid= gtk_timeout_add(number,(GtkFunction) timeout_pause, &stop);
  while (1) 
    {
      gtk_main();
      if ( stop == TRUE)
	{
	  g_source_remove (tid);
	  break;
	}
    }
}
