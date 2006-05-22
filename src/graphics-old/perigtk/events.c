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
  int getrelease,getmotion,getmen,getkey, button;
  int sci_click_activated; /* TRUE when we are in a xclick or xclick_any function */
  guint timer;
  char *str;
  int  lstr;
};

static GTK_locator_info info = { -1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0};

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

  if ( info.sci_click_activated == FALSE ) 
    {
      nsp_gwin_event ev={ gc->CurWindow,event->x, event->y,id,0,0};
      nsp_enqueue(&gc->queue,&ev);
    }
  else 
    {
      info.ok = 1; info.win=  gc->CurWindow; info.x = event->x; info.y = event->y; 
      info.button = id;
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
  if ( info.sci_click_activated == FALSE || info.getrelease == 0 ) 
    {
      nsp_gwin_event ev={ gc->CurWindow,event->x, event->y,event->button-6 ,0,1};
      nsp_enqueue(&gc->queue,&ev);
    }
  else 
    {
      info.ok =1 ; info.win=  gc->CurWindow; info.x = event->x;  info.y = event->y;
      info.button = event->button -6;
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
  if ( info.sci_click_activated == FALSE || info.getmotion == 0 ) 
    {
      nsp_gwin_event ev={ gc->CurWindow,x, y,-1 ,1,0},evlast;
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
      info.ok =1 ;  info.win=  gc->CurWindow; info.x = x;  info.y = y;
      info.button = -1;
      gtk_main_quit();
    }
  return TRUE;
}

/* event handler for key pressed */

static gint key_press_event (GtkWidget *widget, GdkEventKey *event, BCG *gc)
{
  gint x,y; 
  GdkModifierType state;
  if (info.getkey == TRUE && (event->keyval >= 0x20) && (event->keyval <= 0xFF))
    {
      /* since Alt-keys and Ctrl-keys are stored in menus I want to ignore them here */
      if ( event->state != GDK_CONTROL_MASK && event->state != GDK_MOD1_MASK ) 
	{
	  gdk_window_get_pointer (gc->private->drawing->window, &x, &y, &state);
	  info.x=x ; info.y=y;
	  info.ok =1 ;  info.win=  gc->CurWindow; 
	  info.button = event->keyval;
	  gtk_main_quit();
	}
    }
  else {
    gdk_window_get_pointer (gc->private->drawing->window, &x, &y, &state);
    nsp_gwin_event ev={ gc->CurWindow,x, y,event->keyval ,0,1};
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
  if ( dequeue_nsp_command(info.str,info.lstr) == OK)
    {
      info.ok = 1 ; info.x = 0 ; info.y =0 ; info.button =  -2;
      info.win = (gc == NULL) ? 0 : gc->CurWindow;
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

void xclick_any(BCG *Xgc,char *str, int *ibutton, int *x1,int *yy1, int *iwin, 
		int iflag,int getmotion,int getrelease,int getkey,int lstr)
{
  int change_cursor = TRUE;
  SciClick(Xgc,ibutton,x1,yy1,iwin,iflag,getmotion,getrelease,getkey,str,lstr,change_cursor);
}

void xclick(BCG * Xgc,char *str, int *ibutton, int *x1,int *yy1,int iflag,int motion,
	    int release,int key, int istr)
{
  int change_cursor = TRUE;
  int win = ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) ? 0 : Xgc->CurWindow;
  SciClick(Xgc,ibutton,x1, yy1,&win,iflag,motion,release,key,str,istr,change_cursor);
}

void xgetmouse(BCG *Xgc,char *str, int *ibutton, int *x1, int *yy1, int usequeue,
	       int motion,int release,int key)
{
  int change_cursor = TRUE;
  int win = ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) ? 0 : Xgc->CurWindow;
  SciClick(Xgc,ibutton,x1, yy1,&win,usequeue,motion,release,key,(char *) 0,0,change_cursor);
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

static void SciClick(BCG *Xgc,int *ibutton, int *x1, int *yy1,int *iwin, int iflag, int getmotion,
		     int getrelease,int getkey, char *str, int lstr,int change_cursor)
{
#ifdef WITH_TK
  guint timer_tk;
#endif 
  GTK_locator_info rec_info ; 
  int win=*iwin,wincount=0,win1;
  if ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) {
    *ibutton = -100;     return;
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
	  *iwin = ev.win; *x1 = ev.x ; *yy1 = ev.y ; *ibutton= ev.ibutton;
	  return;
	}
    }

  if ( iflag == FALSE ) window_list_clear_queue((win == -1 ) ? NULL: Xgc);

  if ( change_cursor ) nsp_change_cursor(Xgc,win,wincount,1);
  
  /* save info in local variable  */
  rec_info = info;
  /* set info */ 
  info.ok = 0 ; 
  info.getrelease = getrelease ; 
  info.getmotion   = getmotion ;
  info.getmen     = (lstr == 0) ? FALSE : TRUE; 
  info.getkey     = getkey;
  info.sci_click_activated = TRUE ;

  if ( info.getmen == TRUE ) 
    {
      /*  Check soft menu activation during xclick */ 
      info.timer = gtk_timeout_add(100, (GtkFunction) timeout_test, Xgc);
      info.str   = str;
      info.lstr  = lstr; /* on entry it gives the size of str buffer */
    }
  
#ifdef WITH_TK
  timer_tk=  gtk_timeout_add(100,  (GtkFunction) timeout_tk , NULL);
#endif
  
  while (1) 
    {
      gtk_main();
      /* be sure that gtk_main_quit was activated by proper event */
      if ( info.ok == 1 ) 
	{
	  if ( win == -1 ) break;
	  if ( info.win == win  ) break;
	}
    }

#ifdef WITH_TK
  gtk_timeout_remove(timer_tk);
#endif

  *x1 = info.x;
  *yy1 = info.y;
  *ibutton = info.button;
  *iwin = info.win;
  
  /* remove timer if it was set by us */ 
  if ( info.getmen == TRUE )  gtk_timeout_remove (info.timer);

  /* take care of recursive calls i.e restore info  */
  info = rec_info ; 

  if ( change_cursor ) nsp_change_cursor(Xgc,win,wincount,0);
}


/* generates a pause, in seconds */

#if defined(__STDC__) || defined(_IBMR2)
#include <unistd.h>  /* for usleep */
#endif 

static void xpause(int sec_time)
{ 
  unsigned useconds = (unsigned) sec_time;
  if (useconds != 0)  
#ifdef HAVE_USLEEP
    { usleep(useconds); }
#else
#ifdef HAVE_SLEEP
  {  sleep(useconds/1000000); }
#else
  return;
#endif
#endif
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
  if ( info.sci_click_activated == TRUE ) 
    {
      info.ok =1 ;  info.win=  gc->CurWindow; info.x = 0 ;  info.y = 0;
      info.button = -100;
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
  if ( info.sci_click_activated == TRUE ) 
    {
      info.ok =1 ;  info.win=  gc->CurWindow; info.x = 0 ;  info.y = 0;
      info.button = -100;
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

