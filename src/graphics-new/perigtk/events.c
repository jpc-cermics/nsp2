/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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


#if GTK_CHECK_VERSION(3,0,0)
#define MY_THREADS_LEAVE  /* GDK_THREADS_LEAVE (); */
#define MY_THREADS_ENTER  /* GDK_THREADS_ENTER (); */
#else
#define MY_THREADS_LEAVE  gdk_threads_leave();
#define MY_THREADS_ENTER  gdk_threads_enter();
#endif

/* FIXME */
extern char * nsp_string_to_utf8( char *str);
static void nsp_event_pause(int number) ;

/**
 * setpopupname:
 * @Xgc: a #BCG
 * @string: name to be given to popup window.
 *
 * sets the graphic window popupname to @string.
 * The graphic window is obtained from its @Xgc.
 *
 **/

static void setpopupname(BCG *Xgc,char *string)
{
  char *string_utf8=  nsp_string_to_utf8(string);
  gtk_window_set_title(GTK_WINDOW(Xgc->private->window),string_utf8);
  if ( string_utf8 != string ) g_free(string_utf8);
}


extern GTK_locator_info nsp_event_info;

/* set of static variable(s)/function(s) used here
 * to do not run some gtk functions when calling
 * gtk_main at gtk_main_loop_level 0
 *
 * alan/jpc, 19/09/2012
 */

static GMainLoop *nsp_loop=NULL;

/**
 * nsp_gtk_main:
 * @void:
 *
 * enters a gtk_main_loop. This is prefered to
 * entering a gtk_main since entering top level gtk_main
 * can be time costly.
 **/

static void nsp_gtk_main(void)
{
  if (nsp_loop==NULL) {
    nsp_loop = g_main_loop_new (NULL, TRUE);
  }
  MY_THREADS_LEAVE;
  g_main_loop_run (nsp_loop);
  MY_THREADS_ENTER;
}

/**
 * nsp_gtk_main_quit:
 * @void:
 *
 * used instead of gtk_main_quit
 **/

static void nsp_gtk_main_quit(void)
{
  g_main_loop_quit (nsp_loop);
}

/**
 * locator_button_press:
 * @widget:
 * @event:
 * @gc:
 *
 * event handler for "button-press-event".
 *
 * Returns: %TRUE
 **/

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

  if ( event->state & GDK_SHIFT_MASK )
    {
      if ( event->state & GDK_CONTROL_MASK )
	id +=3000;
      else
	id +=2000;
    }
  else if ( event->state & GDK_CONTROL_MASK ) id +=1000;

  if ( nsp_event_info.sci_click_activated == FALSE )
    {
      nsp_gwin_event ev={ gc->CurWindow,event->x, event->y,id,event->state,0,0};
      nsp_enqueue(&gc->queue,&ev);
    }
  else
    {
      nsp_event_info.ok = 1;
      nsp_event_info.win=  gc->CurWindow;
      nsp_event_info.x = event->x;
      nsp_event_info.y = event->y;
      nsp_event_info.button = id;
      nsp_event_info.mask = event->state ;
      nsp_gtk_main_quit();
    }
  return TRUE;
}

/**
 * locator_button_release:
 * @widget:
 * @event:
 * @gc:
 *
 * event handler for "button-release-event",
 *
 * Returns: %TRUE
 **/

static gboolean locator_button_release(GtkWidget *widget,
				       GdkEventButton *event,
				       BCG *gc)
{
  if ( nsp_event_info.sci_click_activated == FALSE )
    {
      /* here we are not in an xclick or xgetmouse
       * thus we have to store events in queue.
       */
      nsp_gwin_event ev={ gc->CurWindow,event->x, event->y,event->button-6 ,event->state,0,1};
      nsp_enqueue(&gc->queue,&ev);
    }
  else
    {
      if ( nsp_event_info.getrelease == TRUE )
	{
	  nsp_event_info.ok =1 ;
	  nsp_event_info.win=  gc->CurWindow;
	  nsp_event_info.x = event->x;  nsp_event_info.y = event->y;
	  nsp_event_info.button = event->button -6;
	  nsp_event_info.mask = event->state;
	  nsp_gtk_main_quit();
	}
    }
  return TRUE;
}

/**
 * locator_button_motion:
 * @widget:
 * @event:
 * @gc:
 *
 * event handler for "motion-notify-event",
 *
 * Returns: %TRUE
 **/

static gboolean locator_button_motion(GtkWidget *widget,
				      GdkEventMotion *event,
				      BCG *gc)
{
  gint x,y;
  if (event->is_hint)
    {
#ifdef XXX
      /* should check the gdk version to use this */
      x= event->x;y=event->y;
      gdk_event_request_motions(event);
#else
      GdkModifierType state;
#if GTK_CHECK_VERSION(3,0,0)
      gdk_window_get_device_position(event->window,event->device, &x, &y, &state);
#else
      gdk_window_get_pointer (event->window, &x, &y, &state);
#endif

#endif
    }
  else
    {
      x= event->x; y = event->y;
    }
  if ( nsp_event_info.sci_click_activated == FALSE )
    {
      /* here we are not in an xclick or xgetmouse
       * thus we have to store events in queue.
       */
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
      /* here we are inside a xclick or xgetmouse */
      if ( nsp_event_info.getmotion == TRUE )
	{
	  nsp_event_info.ok =1 ;
	  nsp_event_info.win=  gc->CurWindow;
	  nsp_event_info.x = x;  nsp_event_info.y = y;
	  nsp_event_info.button = -1;
	  nsp_event_info.mask = event->state;
	  nsp_gtk_main_quit();
	}
    }
  return TRUE;
}

/**
 * key_press_event_new:
 * @widget:
 * @event:
 * @gc:
 *
 * event handler for "key_press_event"
 *
 * Returns: %TRUE or %FALSE
 **/

static gint key_press_event_new (GtkWidget *widget, GdkEventKey *event, BCG *gc)
{
  gint x,y;
  GdkModifierType state;
#if GTK_CHECK_VERSION(3,0,0)
  static GdkDevice *device= NULL;
  if ( device == NULL ) 
    {
      GdkDisplay *display=gdk_display_get_default ();
      GdkDeviceManager *device_manager=gdk_display_get_device_manager(display);
      device = gdk_device_manager_get_client_pointer(device_manager);
    }
#endif
  if (nsp_event_info.getkey == TRUE && (event->keyval >= 0x20) && (event->keyval <= 0xFF))
    {
      /* since Alt-keys and Ctrl-keys are stored in menus I want to ignore them here */
      if ( (event->state & GDK_CONTROL_MASK) || (event->state & GDK_MOD1_MASK ))
	return FALSE;

      if ( nsp_event_info.sci_click_activated == FALSE )
	{
	  /* here we are not in an xclick or xgetmouse
	   * thus we have to store events in queue.
	   */
#if GTK_CHECK_VERSION(3,0,0)
	  gdk_window_get_device_position(gtk_widget_get_window(gc->private->drawing), device, &x, &y, &state);
#else
	  gdk_window_get_pointer (gtk_widget_get_window(gc->private->drawing), &x, &y, &state);
#endif
	  nsp_gwin_event ev={ gc->CurWindow,x, y,event->keyval ,event->state,0,1};
	  nsp_enqueue(&gc->queue,&ev);
	}
      else
	{
#if GTK_CHECK_VERSION(3,0,0)
	  gdk_window_get_device_position(gtk_widget_get_window(gc->private->drawing), device, &x, &y, &state);
#else
	  gdk_window_get_pointer (gtk_widget_get_window(gc->private->drawing), &x, &y, &state);
#endif
	  nsp_event_info.x=x ; nsp_event_info.y=y;
	  nsp_event_info.ok =1 ;  nsp_event_info.win=  gc->CurWindow;
	  nsp_event_info.button = event->keyval;
	  nsp_event_info.mask = event->state;
	  nsp_gtk_main_quit();
	}
    }
  return FALSE; /* also want other handlers to be activated */
}

#if 0
/* test a drag initiated in graphic window */
static void test_drag_begin(GtkWidget *widget, GdkEvent *event)
{
  static GtkTargetEntry xtarget_table[] = {
    { "STRING",     1, 0  },
    { "text/plain", 1, 0  }
  };
  static guint xn_targets = sizeof(xtarget_table) / sizeof(xtarget_table[0]);
  GtkTargetList *tl= gtk_target_list_new(xtarget_table,xn_targets);
  gtk_drag_begin (widget,tl,GDK_ACTION_COPY, 0 , event);
}
#endif

/**
 * timeout_menu_check:
 * @gc: a #BCG
 *
 * a time out to check for menu activation
 * Note that info.win is not correct this is to be done XXX
 *
 * Returns: %TRUE
 **/

/* #define DEBUG_T */
#ifdef  DEBUG_T
static int ktmc=0;
#endif

static gint timeout_menu_check (BCG *gc)
{
#ifdef  DEBUG_T
  ktmc++;
  Sciprintf("timeout_menu_check with ktmc %d\n",ktmc);
  if (ktmc==100 ||  dequeue_nsp_command(nsp_event_info.str,nsp_event_info.lstr) == OK)
#else
    if (  dequeue_nsp_command(nsp_event_info.str,nsp_event_info.lstr) == OK)
#endif
      {
	MY_THREADS_ENTER;
	nsp_event_info.ok = 1 ; nsp_event_info.x = 0 ; nsp_event_info.y =0 ; nsp_event_info.button =  -2;
	nsp_event_info.mask = 0;
	nsp_event_info.win = (gc == NULL) ? 0 : gc->CurWindow;
	nsp_gtk_main_quit();
	MY_THREADS_LEAVE ;
#ifdef  DEBUG_T
	Sciprintf("quit the menu_check with a message\n");
#endif
      }
  return TRUE;
}

#ifdef WITH_TK
/* be sur that tk events are taken into account */

extern void nsp_flush_tkevents(void);

static gint timeout_tk (void *v)
{
  MY_THREADS_ENTER;
  nsp_flush_tkevents();
  MY_THREADS_LEAVE;
  return TRUE;
}

#endif

/* synchronously wait for an event : the three following function are
 * to be merged to just one function
 */

void xclick_any(BCG *Xgc,char *str, int *ibutton,int *imask, int *x1,int *yy1, int *iwin,
		int iflag,int getmotion,int getrelease,int getkey,int lstr)
{
  nsp_event_wait(Xgc,ibutton,imask,x1,yy1,iwin,iflag,getmotion,getrelease,getkey,str,lstr);
}

void xclick(BCG * Xgc,char *str, int *ibutton,int *imask, int *x1,int *yy1,int iflag,int motion,
	    int release,int key, int istr)
{
  int win = ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) ? 0 : Xgc->CurWindow;
  nsp_event_wait(Xgc,ibutton,imask,x1, yy1,&win,iflag,motion,release,key,str,istr);
}

void xgetmouse(BCG *Xgc,char *str, int *ibutton,int *imask, int *x1, int *yy1, int usequeue,
	       int motion,int release,int key)
{
  int win = ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) ? 0 : Xgc->CurWindow;
  nsp_event_wait(Xgc,ibutton,imask,x1, yy1,&win,usequeue,motion,release,key,(char *) 0,0);
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
	      gdk_window_set_cursor(gtk_widget_get_window(bcg->private->drawing),cursor);
	    }
	}
    }
  else
    {
      if ( Xgc != (BCG *) 0 && Xgc->private != NULL &&  Xgc->private->drawing != NULL ) {
	cursor =  ( flag == 0 ) ? Xgc->private->ccursor : Xgc->private->gcursor;
	gdk_window_set_cursor (gtk_widget_get_window(Xgc->private->drawing),cursor);
      }
    }
}

/*
 * wait for events: mouse motion and mouse press and release
 *                  and dynamic menu activation through a timeout
 *
 * if iflag = 0 : clear previous mouse click else if iflag = 1 : don't
 * if getmotion = 1 : add  mouse move to checked events.
 * if getrelease=1 :  add  mouse release to checked events.
 * if dyn_men = 1 ;   add  dynamic menus activation to checked events
 *                    dyn_menu is activated if str != NULL (returns the menu code in str )
 * if getkey = 1;     add key-pressed to checked events.
 *
 * return value : 0,1,2 if button pressed
 *                -5,-4,-3: if button release
 *                -100 : error or window destroyed
 *                -2   : menu activated
 * FIXME:  must add support for Ctrl-C ?
 */

static void nsp_event_wait(BCG *Xgc,int *ibutton,int *imask, int *x1, int *yy1,int *iwin,
			   int iflag, int getmotion,
			   int getrelease,int getkey, char *str, int lstr)
{
#ifdef WITH_TK
  guint timer_tk;
#endif
  GTK_locator_info rec_info ;
  int win=*iwin,wincount=0,/* win1,*/ change_cursor;

  if ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL )
    {
      /* graphic window does not exists
       */
      *ibutton = -100; *imask=0;
      goto end;
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
  /*win1= win;*/ /* CheckClickQueue change its first argument if -1 */

  /* decode iflag */
  change_cursor = iflag & (1<<2);
  iflag = iflag & 1;

  if ( iflag == TRUE )
    {
      /* check for already stored event */
      int ok = FALSE;
      nsp_gwin_event ev ={0};
      while (1)
	{
	  if ( window_list_check_queue((win == -1 ) ? NULL: Xgc,&ev) == OK) ok=TRUE;
	  if ( ok == FALSE ) break;
	  *iwin = ev.win; *x1 = ev.x ; *yy1 = ev.y ;
	  *ibutton= ev.ibutton, *imask= ev.mask;
	  if ( getmotion  == FALSE && ev.motion  == TRUE ) ok = FALSE;
	  if ( getrelease == FALSE && ev.release == TRUE ) ok = FALSE;
	  if ( ok == FALSE ) break;
	  while ( gtk_events_pending()) gtk_main_iteration();
	  goto end;
	}
    }
  else
    {
      /* clear the queue */
      window_list_clear_queue((win == -1 ) ? NULL: Xgc);
    }

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
#ifdef  DEBUG_T
      ktmc=0;
#endif
      nsp_event_info.timer = g_timeout_add(100, (GSourceFunc) timeout_menu_check, Xgc);
      nsp_event_info.str   = str;
      nsp_event_info.lstr  = lstr; /* on entry it gives the size of str buffer */
    }

#ifdef WITH_TK
  timer_tk=  g_timeout_add(100,  (GSourceFunc) timeout_tk , NULL);
#endif
#ifdef  DEBUG_T
  Sciprintf("enter the while \n");
#endif
  while (1)
    {
      /* enter the gtk_main */
      nsp_gtk_main();
#ifdef  DEBUG_T
      Sciprintf("quit the main \n");
#endif
      /* be sure that gtk_main_quit was activated by proper event */
      if ( nsp_event_info.ok == 1 )
	{
	  if ( win == -1 || nsp_event_info.win == win  )
	    {
#ifdef  DEBUG_T
	      Sciprintf("quit the while loop\n");
#endif
	      break;
	    }
	}
    }

#ifdef WITH_TK
  g_source_remove(timer_tk);
#endif

  *x1 = nsp_event_info.x;
  *yy1 = nsp_event_info.y;
  *ibutton = nsp_event_info.button;
  *imask = nsp_event_info.mask;
  *iwin = nsp_event_info.win;

  /* Sciprintf("xfound %d %d %d\n",nsp_event_info.x,nsp_event_info.y,nsp_event_info.button);   */
  /* remove timer if it was set by us */
  if ( nsp_event_info.getmen == TRUE )  g_source_remove (nsp_event_info.timer);

  /* take care of recursive calls i.e restore info  */
  nsp_event_info = rec_info ;

  if ( change_cursor ) nsp_change_cursor(Xgc,win,wincount,0);

 end:
  return;

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

extern int nsp_check_events_activated(void);

static void xpause(int sec_time,int events)
{
  unsigned useconds = (unsigned) sec_time;
  if ( events == TRUE && nsp_check_events_activated() == TRUE )
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
      nsp_gtk_main_quit();
    }
  else
    {
      delete_window(gc,gc->CurWindow);
    }
}

/* ici avec la valeur renvoy�e on peut d�cider de detruire ou pas */

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
      nsp_gtk_main_quit();
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

/* delete the graphic window given a graphic
 * context or the window id @intnum.
 *
 */

static void delete_window(BCG *dd,int intnum)
{
  NspFigure *F;
  BCG *winxgc= dd;
  int top_count;
  if ( dd == NULL)
    {
      if ((winxgc = window_list_search(intnum)) == NULL) return;
    }

  /* delete the associated figure */
  F =  dd->figure;
  if ( F != NULL)
    {
      NspFigure *cF;
      /* Note that because of ref_count the figure may not
       * be destroyed, thus we need to clear Figure Xgc
       */
      F->obj->Xgc = NULL;
      cF = nsp_get_current_figure();
      if ( cF != NULL && cF->obj ==  F->obj )
	nsp_unset_current_figure();
      nsp_figure_destroy(F);
    }

  /* I delete the pixmap and the widget */
  if ( winxgc->CurPixmapStatus == 1 )
    {
      /* switch to non extra pixmap mode */
      cairo_surface_destroy(winxgc->private->extra_pixmap);
      winxgc->private->extra_pixmap = NULL;
      winxgc->private->drawable = NULL;
      winxgc->CurPixmapStatus = 0;
    }
  if ( winxgc->private->extra_pixmap != NULL)
    {
      /* we can have a non null extra_pixmap */
      cairo_surface_destroy(winxgc->private->extra_pixmap);
    }
  /* deconnect handlers */
  scig_deconnect_handlers(winxgc);
  /* backing store private->pixmap */
  if (winxgc->private->pixmap != NULL)
    {
      cairo_surface_destroy(winxgc->private->pixmap);
    }
  /* destroy top level window if it is not shared by other graphics  */
  top_count = window_list_search_toplevel(winxgc->private->window);
  if ( top_count <= 1)
    {
      if ( winxgc->private->window != NULL)
	{
	  gtk_widget_destroy(winxgc->private->window);
	}
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
  if ( winxgc->private->q_colors != NULL)
    {
      if (g_queue_is_empty (winxgc->private->q_colors))
	{
	  /* When q_colors is empty then colors can be freed they
	   * correspond to colors initialy allocated and not to
	   * colors from a colormap
	   */
	  if ( winxgc->private->a_colors != NULL)
	    {
	      nsp_matrix_destroy(winxgc->private->a_colors);
	      winxgc->private->a_colors= NULL;
	    }
	}
      else
	{
	  /* Sciprintf("The q_colors is not empty \n"); */
	}
      g_queue_free( winxgc->private->q_colors);
    }
  else
    {
      if ( winxgc->private->a_colors != NULL)
	nsp_matrix_destroy(winxgc->private->a_colors);
      winxgc->private->a_colors= NULL;
    }
  /* free data associated to menus */
  menu_entry_delete(winxgc->private->menu_entries);
  if (winxgc->private->gcursor != NULL)
    {
#if GTK_CHECK_VERSION(3,0,0)
      g_object_unref (G_OBJECT(winxgc->private->gcursor));
#else
      gdk_cursor_unref (winxgc->private->gcursor);
#endif
      winxgc->private->gcursor= NULL;
    }
  if (winxgc->private->ccursor != NULL)
    {
#if GTK_CHECK_VERSION(3,0,0)
      g_object_unref (G_OBJECT(winxgc->private->ccursor));
#else
      gdk_cursor_unref (winxgc->private->ccursor);
#endif
      winxgc->private->ccursor= NULL;
    }
  if (winxgc->private->extra_cursor != NULL)
    {
#if GTK_CHECK_VERSION(3,0,0)
      g_object_unref (G_OBJECT(winxgc->private->extra_cursor));
#else
      gdk_cursor_unref(winxgc->private->extra_cursor);
#endif
      winxgc->private->extra_cursor= NULL;
    }
  /*
  if (winxgc->private->stdgc != NULL)g_object_unref(winxgc->private->stdgc);
  if (winxgc->private->wgc != NULL)g_object_unref(winxgc->private->wgc);
  */
  nsp_fonts_finalize(winxgc);
  FREE(winxgc->private);
  /* remove current window from window list */
  window_list_remove(intnum);
}


static void scig_deconnect_handlers(BCG *winxgc)
{
  int n=0;
  if ( winxgc->private->window == NULL ) return;

  n+=g_signal_handlers_disconnect_by_func(winxgc->private->drawing,
					  G_CALLBACK( configure_event), (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func((winxgc->private->drawing),
					  G_CALLBACK( expose_event_new), (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func((winxgc->private->window),
					  G_CALLBACK(  sci_destroy_window ), (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func((winxgc->private->window),
					  G_CALLBACK(sci_delete_window), (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func ((winxgc->private->window),
					   G_CALLBACK( key_press_event_new), (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func((winxgc->private->drawing),
					  G_CALLBACK( locator_button_press), (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func((winxgc->private->drawing),
					  G_CALLBACK( locator_button_release), (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func((winxgc->private->drawing),
					  G_CALLBACK( locator_button_motion), (gpointer) winxgc);
  n+=g_signal_handlers_disconnect_by_func((winxgc->private->drawing),
					  G_CALLBACK( realize_event), (gpointer) winxgc);
}




/**
 * nsp_event_pause:
 * @number:
 *
 * make a pause of @number milliseconds and if flag is true
 * gtk_events are activated during the pause.
 *
 **/

static gint timeout_pause (void *data)
{
  MY_THREADS_ENTER;
  nsp_gtk_main_quit();
  MY_THREADS_LEAVE;
  return TRUE;
}

extern void controlC_handler (int sig);
static void controlC_handler_pause(int sig)
{
  timeout_pause (NULL);
}

static void nsp_event_pause(int number)
{
  guint tid;
  signal(SIGINT,controlC_handler_pause);
  tid=g_timeout_add(number,(GSourceFunc) timeout_pause, NULL);
  nsp_gtk_main();
  g_source_remove(tid);
  signal(SIGINT,controlC_handler);
}

static void
target_drag_data_received  (GtkWidget          *widget,
			    GdkDragContext     *context,
			    gint                x,
			    gint                y,
			    GtkSelectionData   *data,
			    guint               info,
			    guint               time)
{
#if GTK_CHECK_VERSION(3,0,0)
  GdkAtom data_target = gtk_selection_data_get_target(data);
#else
  GdkAtom data_target = data->target;
#endif
  /* Sciprintf("In target_drag_data_received\n"); */
  if (gtk_drag_get_source_widget (context) == widget)
    {
      /* we stop if the drag was initiated by us */
      g_signal_stop_emission_by_name (widget, "drag-data-received");
      return;
    }

  /* if (data->target == gdk_atom_intern_static_string ("GTK_TREE_MODEL_ROW")) */
  if (data_target == gdk_atom_intern ("GTK_TREE_MODEL_ROW",FALSE ))
    {
      /* used when draging an icon from a scicos palette
       * the associated model row is transmited through drag and drop
       */
      GtkTreeModel     *tree_model;
      GtkTreePath      *path;
      if ( gtk_tree_get_row_drag_data (data,&tree_model,&path))
	{
	  int ncols;
	  GtkTreeIter iter;
	  ncols= gtk_tree_model_get_n_columns(tree_model);
	  if ( ncols != 4 )
	    {
	      gtk_drag_finish (context, FALSE, FALSE, time);
	      return;
	    }
	  if (gtk_tree_model_get_iter(tree_model, &iter, path))
	    {
	      gint x1,y1;
	      GdkModifierType state;
	      BCG *Xgc;
	      static char buf[512];
	      const gchar *bname;
	      gchar name[256];
	      int ids[2],winnum;
	      double pt[2];
	      GType mtype;
	      GValue value = { 0, };
	      int col=1;
	      /* column one is the block name */
	      gtk_tree_model_get_value(tree_model,&iter ,col, &value);
	      mtype = G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(&value));
	      if ( mtype != G_TYPE_STRING )
		{
		  gtk_drag_finish (context, FALSE, FALSE, time);
		  return;
		}
	      bname = g_value_get_string(&value);
	      strcpy(name,bname);
	      g_value_unset(&value);
	      for (col = 2 ; col < 4; col++)
		{
		  gtk_tree_model_get_value(tree_model,&iter ,col, &value);
		  mtype = G_TYPE_FUNDAMENTAL(G_VALUE_TYPE(&value));
		  if ( mtype != G_TYPE_DOUBLE )
		    {
		      gtk_drag_finish (context, FALSE, FALSE, time);
		      return;
		    }
		  ids[col-2] = g_value_get_double(&value);
		  g_value_unset(&value);
		}
	      winnum = window_list_search_from_drawing(widget);
	      if ( winnum == -1 )
		{
		  gtk_drag_finish (context, FALSE, FALSE, time);
		  return;
		}
	      /* Sciprintf("You have draged a block from palette [%d,%d] to window %d\n",
	       *  ids[0],ids[1],winnum);
	       */
	      Xgc = window_list_search(winnum);
#if GTK_CHECK_VERSION(3,0,0)
	      gdk_window_get_device_position(gtk_widget_get_window(Xgc->private->drawing),
					     gdk_drag_context_get_device (context),
					     &x1, &y1, &state);
#else
	      gdk_window_get_pointer (gtk_widget_get_window(Xgc->private->drawing), &x1, &y1, &state);
#endif

	      if ( nsp_new_graphics() == TRUE)
		nsp_get_point_axes(Xgc,x1,y1,pt);
	      else
		scale_i2f(Xgc->scales,pt,pt+1,&x1,&y1,1);
	      /* Sciprintf("PlaceDropped_info([%5.3f,%5.3f],[%d,%d],[%d,%d],%d,%d,%d)\n",
	       * pt[0],pt[1],x,y,x1,y1,ids[0],ids[1],winnum);
	       */
	      sprintf(buf,"PlaceDropped_info([%5.3f,%5.3f],%d,%d,%d,'%s')\n",
		      pt[0],pt[1],ids[0],ids[1],winnum,name);
	      enqueue_nsp_command(buf);
	      gtk_drag_finish (context, TRUE, FALSE, time);
	    }
	}
    }
  gtk_drag_finish (context, FALSE, FALSE, time);
}

#if 0
static gboolean
target_drag_drop(GtkWidget *widget, GdkDragContext *context,
		      gint x, gint y, guint time)
{
  Sciprintf("drop received\n");
  if (gtk_drag_get_source_widget(context) != NULL) {
    /* we only accept drops from the same instance of the application,
     * as the drag data is a pointer in our address space */
    return TRUE;
  }
  gtk_drag_finish (context, FALSE, FALSE, time);
  return FALSE;
}
#endif

#if 0
static void
target_drag_data_get  (GtkWidget        *widget,
		       GdkDragContext   *context,
		       GtkSelectionData *selection_data,
		       guint             info,
		       guint             time,
		       void              *args)
{
  Sciprintf("xx drag_data_get\n");
  /*
  if ((data->length >= 0) && (data->format == 8))
    {
      g_print ("Received \"%s\" in trashcan\n", (gchar *)data->data);
      gtk_drag_finish (context, TRUE, FALSE, time);
      return;
    }
  gtk_drag_finish (context, FALSE, FALSE, time);
  */
}
#endif
