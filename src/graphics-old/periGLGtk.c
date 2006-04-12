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
 * Graphic library
 * jpc@cermics.enpc.fr
 * Gtk driver 
 *
 * A variation on the gtk driver whith Opengl capabilities.
 * what follows is a generic driver which gives both X11 and GL 
 * driver according to preprocessor flags. 
 * Note that when it is used as an Opengl driver we can mix 
 * Opengl and gdk drawings but they have to be wrapped with calls 
 * to gdk_gl_drawable_wait_gdk and gdk_gl_drawable_wait_gl
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <gtk/gtk.h>

#define PERI_PRIVATE 1
#include "nsp/sciio.h" 
#include "nsp/math.h"
#ifdef PERIGTK
#include "nsp/graphics/periGtk.h"
#else 
#include "nsp/graphics/periGL.h"
#endif 
#include "nsp/version.h"
#include "nsp/graphics/color.h"
#include "nsp/command.h"

/* add : periGtk with OpenGl rendering 
 * the first three variables are to be moved 
 * in periGL.h 
 */
static int nsp_set_gldrawable(GdkPixmap *pixmap);

#ifdef PERIGL 
static GdkGLConfig *glconfig = NULL;
static GdkGLContext *glcontext = NULL;
static GdkGLDrawable *gldrawable=NULL;
static void drawpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag);
static void fillpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag);
static void realize_event_ogl();
static void clip_rectangle(BCG *Xgc, GdkRectangle clip_rect);
static void unclip_rectangle(GdkRectangle clip_rect);
#endif 

/*
 * 
 *  Xgc->record_flag == TRUE if we are recording graphics 
 *  Xgc->private->in_expose == TRUE if the call is from an expose_event 
 *  Xgc->CurPixmapStatus == 0 if we are not using an extra pixmap 
 *  Xgc->private->draw = TRUE we have something to draw 
 * 
 */ 

/*
 * we always draw in a drawable which is a pixmap but the expose event is asynchronous
 */

#ifdef PERIGTK 
#define DRAW_CHECK  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 ) nsp_gtk_invalidate(Xgc);
#define DRAW_CHECK_GDK  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 ) nsp_gtk_invalidate(Xgc);
#else 
#define DRAW_CHECK  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 ) nsp_gtk_invalidate(Xgc);\
        if ( Xgc->private->gdk_only == TRUE ) return ;

#define DRAW_CHECK_GDK  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 ) \
 nsp_gtk_invalidate(Xgc); if ( Xgc->private->gl_only == TRUE ) return ;
#endif 





/* Global variables to deal with X11 **/

static unsigned long maxcol; /* FIXME: incorect value to be fixed  */
static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data);
static void nsp_gtk_invalidate(BCG *Xgc);

/*------------------------------------------------------------------
 * the current graphic data structure 
 *------------------------------------------------------------------*/

/* functions **/

static void nsp_gtk_set_color(BCG *Xgc,int col);
static void LoadFonts(void), LoadSymbFonts(void);
static void loadfamily_n(char *name, int *j);
static void pixmap_clear_rect   (BCG *Xgc,int x,int y,int w,int h);
static void SciClick(BCG *Xgc,int *ibutton, int *x1, int *yy1,int *iwin,int iflag,int getmotion, int getrelease,int getkey,char *str, int lstr, int change_cursor);
static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos);
static void scig_deconnect_handlers(BCG *winxgc);
static void draw_mark(BCG *Xgc,int *x, int *y);

static void gdk_draw_text_rot(GdkDrawable *drawable, GdkFont *font,  GdkGC *gc,
			      int x, int y, int maxx, int maxy, const gchar *text,
			      gint text_length, double angle);

static void force_affichage(BCG *Xgc);

/* utility for points allocations */
#ifdef PERIGTK 
static GdkPoint *gtk_get_xpoints(void);
static int GtkReallocVector (int n);
static int gtk_store_points (int n, int *vx,int *vy,int  onemore);
#endif 

void create_graphic_window_menu( BCG *dd);
extern void start_sci_gtk();

/*---------------------------------------------------------
 * nsp_gengine : default graphic engine to use 
 * a global variable. 
 * FIXME: maybe to be moved 
 *---------------------------------------------------------*/

#ifdef PERIGTK 
Gengine * nsp_gengine = &Gtk_gengine ;
#endif 

/* 
 * force expose events to be executed 
 */

static void force_affichage(BCG *Xgc)
{
  /* Xgc->private->resize = 1; */
  gdk_window_process_updates (Xgc->private->drawing->window, FALSE);
}

/* 
 * force an expose_event with draw set to TRUE
 */

static void force_redraw(BCG *Xgc)
{
  nsp_gtk_invalidate(Xgc);
  Xgc->private->draw = TRUE;
  gdk_window_process_updates (Xgc->private->drawing->window, FALSE);
}


/*
 * Next routine are used to deal with the extra_pixmap 
 * which is used when xset('pixmap',1) is activated.
 * FIXME: this is unfinished for OpenGl since we have 
 * to attach a gldrawable to this pixmap.
 * Note also that it is not so usefull since 
 *  nsp graphics are asynchronous and double buffered 
 *  between a pixmap and the graphic windows. 
 */

static void xset_pixmapclear(BCG *Xgc)
{
  if ( Xgc->CurPixmapStatus == 1) 
    {
      pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
    }
}

static void xset_show(BCG *Xgc)
{
  if ( Xgc->CurPixmapStatus == 1) 
    {
      /* we copy the extra_pixmap to the window and to the backing store pixmap */
      /* gdk_gc_set_background(Xgc->private->stdgc, &Xgc->private->gcol_bg); */
      /* drawing to the window and to the backing store pixmap */
      gdk_draw_pixmap(Xgc->private->drawing->window,Xgc->private->stdgc, Xgc->private->extra_pixmap,
		      0,0,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
      gdk_draw_pixmap(Xgc->private->pixmap, Xgc->private->stdgc, Xgc->private->extra_pixmap,
		      0,0,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
    }
  else
    {
      /* see the comments at the begining */
      force_affichage(Xgc);
    }
}

/*
 * Pixmap clear: clear the extra private->pixmap associated to the window 
 * using the background color.
 */

static void pixmap_clear_rect(BCG *Xgc,int x, int y, int w, int h)
{
  if ( Xgc->CurPixmapStatus == 1) 
    {
      gdk_gc_set_background(Xgc->private->stdgc, &Xgc->private->gcol_bg);
      gdk_draw_rectangle(Xgc->private->extra_pixmap,Xgc->private->stdgc, TRUE,
			 0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
    }
}

/* 
 * Resize the Pixmap according to window size change 
 * But only if there's a private->pixmap 
 */

static void pixmap_resize(BCG *Xgc)
{
  if ( Xgc->CurPixmapStatus == 1) 
    {
      int x= Xgc->CWindowWidth; 
      int y= Xgc->CWindowHeight;
      /* create a new pixmap */
      GdkDrawable *temp = (GdkDrawable *) gdk_pixmap_new(Xgc->private->drawing->window,x,y,-1);
      if ( temp  == NULL ) 
	{
	  xinfo(Xgc,"No more space to create Pixmaps");
	  return;
	}
      gdk_pixmap_unref((GdkPixmap *) Xgc->private->extra_pixmap);
      Xgc->private->drawable = Xgc->private->extra_pixmap = temp;
      pixmap_clear_rect(Xgc,0,0,x,y);
    }
} 


/*
 * General routines callable from nsp 
 */

/* xselect() or xselect(win)
 * raise the graphic window associated to Xgc
 * If there's no graphic window then select creates one 
 */

static void xselgraphic(BCG *Xgc)
{ 
  /* Test not really usefull: see sciwin in matdes.f */
  if ( Xgc == (BCG *)0 || Xgc->private->window ==  NULL) initgraphic("",NULL,NULL,NULL,NULL,NULL,'e');
  gdk_window_show(Xgc->private->window->window);
  gdk_flush();
}

/* End of graphic (do nothing) */

static void xend(BCG *Xgc)
{
  /* Must destroy everything  **/
}

/* Clear the current graphic window */

static void clearwindow(BCG *Xgc)
{
  /* we use the private->stdgc graphic context */
  DRAW_CHECK;
#ifdef PERIGTK
  gdk_gc_set_foreground(Xgc->private->stdgc, &Xgc->private->gcol_bg);
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->stdgc, TRUE, 0, 0,
		     Xgc->CWindowWidth, Xgc->CWindowHeight);
#else 
  glClearColor(Xgc->private->gcol_bg.red /255.0,
	       Xgc->private->gcol_bg.green /255.0,
	       Xgc->private->gcol_bg.blue /255.0,0.0);
  glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
#endif 
}

/* generates a pause, 
 * sleep some number of microseconds 
 * FIXME: this function should be recorded 
 * and usleep should be masked in machine.h
 */

#if defined(__STDC__) || defined(_IBMR2)
/** for usleep **/
#include <unistd.h> 
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


/*
 * Changes the graphic window popupname 
 */

/*FIXME: this should be in a .h */
extern char * nsp_string_to_utf8( char *str);

/*
 * FIXME: do we really need two names !!
 */

static void Setpopupname(BCG *Xgc,char *string)
{ 
  char *string_utf8=  nsp_string_to_utf8(string);
  gtk_window_set_title(GTK_WINDOW(Xgc->private->window),string_utf8);
  if ( string_utf8 != string ) g_free(string_utf8);
}

/* appelle ds Xcall.c */

static void setpopupname(BCG *Xgc,char *name)
{
  Setpopupname(Xgc,name);
}

/*
 * Wait for mouse events in graphic window 
 *   send back mouse location  (x1,y1)  and button number  {0,1,2}
 *   and the window number 
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

static gboolean locator_button_press(GtkWidget *widget,
				     GdkEventButton *event,
				     BCG *gc)
{
  int id;
  switch (event->type) 
    {
    case GDK_BUTTON_PRESS : id= event->button-1 ;break;
    case GDK_2BUTTON_PRESS : id= event->button-1 +3;break;
    case GDK_3BUTTON_PRESS : id= event->button-1 +6;break;
    default: 
      break;
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


static gboolean locator_button_release(GtkWidget *widget,
				       GdkEventButton *event,
				       BCG *gc)
{
  /* fprintf(stderr,"release event %d\n",event->type);*/
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


static gint key_press_event (GtkWidget *widget, GdkEventKey *event, BCG *gc)
{
  /* modified 30/10/02 to get cursor location and register  key_press_event in queue' SS */
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
    delete_window(gc,gc->CurWindow);
}

/* ici avec la valeur renvoy�e on peut d�cider de detruire ou pas */

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
 * A time out to check for menu activation 
 *   for menus added with addmenu etc....
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

/*
 * A timeout to flush TK Events while in xclick 
 * Note that WITH_TK is not supposed to be used in nsp.
 */

  
#ifdef WITH_TK
extern int flushTKEvents();

static gint timeout_tk (void *v)
{
  flushTKEvents();
  return TRUE;
}
#endif

void xclick_any(BCG *Xgc,char *str, int *ibutton, int *x1,int *yy1, int *iwin, int iflag,int getmotion,int getrelease,int getkey,int lstr)
{
  int change_cursor = TRUE;
  SciClick(Xgc,ibutton,x1,yy1,iwin,iflag,getmotion,getrelease,getkey,str,lstr,change_cursor);
}

void xclick(BCG * Xgc,char *str, int *ibutton, int *x1,int *yy1,int iflag,int motion,int release,int key, int istr)
{
  int change_cursor = TRUE;
  int win = ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) ? 0 : Xgc->CurWindow;
  SciClick(Xgc,ibutton,x1, yy1,&win,iflag,motion,release,key,str,istr,change_cursor);
}

void xgetmouse(BCG *Xgc,char *str, int *ibutton, int *x1, int *yy1, int usequeue, int motion,int release,int key)
{
  int change_cursor = TRUE;
  int win = ( Xgc == (BCG *) 0 || Xgc->private->drawing == NULL ) ? 0 : Xgc->CurWindow;
  SciClick(Xgc,ibutton,x1, yy1,&win,usequeue,motion,release,key,(char *) 0,0,change_cursor);
}

/*------------------------------------------------------------------------------
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
 *------------------------------------------------------------------------------*/

/* 
 * A finir pour tenir compte des control C de l'utilisateur  
 */


static void nsp_change_cursor(BCG *Xgc, int win,int wincount, int flag );

static void SciClick(BCG *Xgc,int *ibutton, int *x1, int *yy1,int *iwin, int iflag, int getmotion, int getrelease,int getkey, char *str, int lstr,int change_cursor)
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

  /* check for already stored event */
  if ( iflag == TRUE ) 
    { 
      nsp_event_queue ev;
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
 * clear a rectangle zone 
 */

#ifdef PERIGTK 
static void cleararea(BCG *Xgc, int x, int y, int w, int h)
{
  int clipflag = 0;
  /* switch to a clear gc */
  int cur_alu = Xgc->CurDrawFunction;
  int clear = 0 ; /* 0 is the Xclear alufunction */;
  DRAW_CHECK;

  if ( cur_alu != clear ) xset_alufunction1(Xgc,clear);
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
      gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
    }
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, TRUE,x,y,w,h);
  if ( cur_alu != clear )
    xset_alufunction1(Xgc,cur_alu);   /* back to current value */ 
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      /* restore clip */
      GdkRectangle clip_rect = { Xgc->CurClipRegion[0],
				 Xgc->CurClipRegion[1],
				 Xgc->CurClipRegion[2],
				 Xgc->CurClipRegion[3]};
      gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
    }
}

#else 

/*
 * could be merged with previous one !!
 */

typedef void (*r_c) (BCG *Xgc,int x,int y,int w,int h);
static void RectangleClear   (BCG *Xgc,int x,int y,int w,int h,int clipflag,r_c f );
static void R_clear  (BCG *Xgc,int x,int y,int w,int h);

static void R_clear(BCG *Xgc,int x, int y, int w, int h)
{
  int tab[4];

  tab[0] = x;      tab[1] = y;      tab[2] = w;      tab[3] = h;
  fillrectangle(Xgc, tab);
  /*
    gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, TRUE,x,y,w,h);
    if ( Xgc->private->drawable == Xgc->private->drawing->window) 
    gdk_draw_rectangle(Xgc->private->pixmap, Xgc->private->wgc, TRUE,x,y,w,h);
  */
}

static void RectangleClear(BCG *Xgc,int x, int y, int w, int h, int clipflag, r_c F)
{
  /* switch to a clear gc */
  int cur_alu = Xgc->CurDrawFunction;
  int clear = 0 ; /* 0 is the Xclear alufunction */;
  if ( cur_alu != clear ) xset_alufunction1(Xgc,clear);
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
      /* gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect); */
      clip_rectangle(Xgc, clip_rect);
    }
  (*F)(Xgc,x,y,w,h);
  if ( cur_alu != clear )
    xset_alufunction1(Xgc,cur_alu);   /* back to current value */ 
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      /* restore clip */
      GdkRectangle clip_rect = { Xgc->CurClipRegion[0],
				 Xgc->CurClipRegion[1],
				 Xgc->CurClipRegion[2],
				 Xgc->CurClipRegion[3]};
      /* gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect); */
      clip_rectangle(Xgc, clip_rect);
    }
}

static void cleararea(BCG *Xgc, int x, int y, int w, int h)
{
  DRAW_CHECK;
  RectangleClear(Xgc,x,y,w,h,0,R_clear);
}
#endif 


/*
 * graphic context modifications 
 */

/* record or not the graphic commands */

static int xget_recording(BCG *Xgc)
{
  return Xgc->record_flag;
}

static void xset_recording(BCG *Xgc, int val)
{
  Xgc->record_flag = (val == 0 ) ? FALSE : TRUE;
}
/** to get the window upper-left point coordinates on the screen  **/

static void xget_windowpos(BCG *Xgc,int *x,int *y)
{
  gint xx,yy;
  gdk_window_get_position (Xgc->private->window->window,&xx,&yy);
  *x = xx; *y =yy;
}

/** to set the window upper-left point position on the screen **/

static void xset_windowpos(BCG *Xgc, int x, int y)
{
  if (Xgc == NULL || Xgc->private->window ==  NULL) initgraphic("",NULL,NULL,NULL,NULL,NULL,'e');
  gdk_window_move (Xgc->private->window->window, x,y);
}

/** To get the drawbox  window size **/

static void xget_windowdim(BCG *Xgc,int *x, int *y)
{     
  /* the two dimensions are always updated */
  *x =  Xgc->CWindowWidth;
  *y =  Xgc->CWindowHeight;
} 

/*
 * To change the drawbox window size. 
 * Here this function set the min size of the graphic window 
 * which means that if the scrolled window 
 * is smaller than the min size scrollbar will be drawn 
 * but if the scrolled window is greater then drawbox will follow 
 * gtk_widget_set_size_request. 
 */

static void xset_windowdim(BCG *Xgc,int x, int y)
{
  /* XXXX: not so easy !!! */
  if (Xgc == NULL || Xgc->private->window ==  NULL) return ;
  if ( Xgc->CurResizeStatus == 1) 
    {
      /* here drawing and scrolled move together */
      gint pw,ph,w,h;
      gdk_window_get_size (Xgc->private->window->window,&pw,&ph);
      gdk_window_get_size (Xgc->private->drawing->window,&w,&h);
      /* resize the graphic window */
      gdk_window_resize(Xgc->private->drawing->window,x,y);
      /* resize the main window at init time */
      gdk_window_resize(Xgc->private->window->window,x+Max((pw-w),0),y+Max((ph-h),0));
      /* want the expose event to resize pixmap and redraw */
      Xgc->private->resize = 1; 
    }
  else
    {
      /* here drawing and scrolled do not move together */
      /* gint sc_w,sc_h;*/
      GdkGeometry geometry;
      GdkWindowHints geometry_mask;
      /* resize the graphic window */
      gdk_window_resize(Xgc->private->drawing->window,x,y);
      /* want the scrolled window to be aware */
      gtk_widget_set_size_request(Xgc->private->drawing, x,y);
      /* Limit the scolled window size  */
      /* gdk_window_get_size (Xgc->private->scrolled,&sc_w,&sc_h); */
      geometry.max_width = x+15;
      geometry.max_height = y+15;
      geometry_mask = GDK_HINT_MAX_SIZE ; 
      gtk_window_set_geometry_hints (GTK_WINDOW (Xgc->private->window), Xgc->private->scrolled,
				     &geometry, geometry_mask);
      /* here we will only generate a configure event and an expose event 
       * if the size is schrinked 
       * thus we activate the redraw by calling appropriate function 
       */
      if ( (Xgc->CWindowWidth > x ) || (Xgc->CWindowHeight > y )) 
	{
	  Xgc->CWindowWidth = x;
	  Xgc->CWindowHeight = y;
	  Xgc->private->resize = 1;/* be sure to put this */
	}
      else 
	{
	  Xgc->CWindowWidth = x;
	  Xgc->CWindowHeight = y;
	  Xgc->private->resize = 1;/* be sure to put this */
	  /* FIXME: NULL to be changed */
	  expose_event( Xgc->private->drawing,NULL, Xgc);
	}
    }
  gdk_flush();
}

/* To get the popup  window size */

static void xget_popupdim(BCG *Xgc,int *x, int *y)
{ 
  gint xx,yy;
  gdk_window_get_size (Xgc->private->window->window,&xx,&yy);
  *x = xx ;  *y = yy ; 
} 

/* To change the popup window size  */

static void xset_popupdim(BCG *Xgc,int x, int y)
{
  gdk_window_resize(Xgc->private->window->window,x,y);
}

/* To get the viewport Upper/Left point Position */

static void xget_viewport(BCG *Xgc,int *x, int *y)
{     
  GtkAdjustment * H, *V;
  if ( Xgc->CurResizeStatus == 0) 
    {
      /* get the horizontal and vertival adjustments of the ? */
      H = gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (Xgc->private->scrolled));
      V = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (Xgc->private->scrolled));
      *x = (int) H->value;
      *y = (int) V->value;
    }
  else 
    { 
      *x = *y =0;
    }
} 

/* To change the window size  */

static void xset_viewport(BCG *Xgc,int x, int y)
{
  if ( Xgc->CurResizeStatus == 0) 
    {
      gtk_adjustment_set_value( gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (Xgc->private->scrolled)),
				(gfloat) x);
      gtk_adjustment_set_value( gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (Xgc->private->scrolled)),
				(gfloat) y);      
    }
}

/* xset('window',x)
 * select window intnum as the current window 
 * window is created if necessary 
 * return the value of the previous current window 
 * 
 */

static int xset_curwin(int intnum,int set_menu)
{
  /* the current graphic context */
  int old;
  BCG *bcgk= window_list_get_first(),*new=NULL;
  if ( bcgk == (BCG *) 0 ) 
    {
      /** First entry or no more graphic window **/
      initgraphic("",&intnum,NULL,NULL,NULL,NULL,'e');
      /** send info to menu **/
      new = window_list_get_first();
      old = -1;
    }
  else 
    {
      if ( bcgk->CurWindow != intnum )
	{
	  BCG *new= window_list_win_to_front(intnum);
	  if ( new == NULL) 
	    {
	      initgraphic("",&intnum,NULL,NULL,NULL,NULL,'e');
	      new = window_list_get_first();
	    }
	  old =  bcgk->CurWindow ;
	}
      else
	{
	  /* nothing to do */
	  return intnum ;
	}
    }

  /* be sure that the graphic window is realized */
  if ( new != NULL ) 
    {
      if (! GTK_WIDGET_REALIZED(new->private->drawing))
	gtk_widget_realize(new->private->drawing);
      if ( set_menu == TRUE) MenuFixCurrentWin(intnum);
    }
  return old;
}


/*
 * Get the id number of the Current Graphic Window 
 * In all the other functions we are sure that Xgc exists 
 * when we call them ( see sciwin in matdes.f ) 
 * exept for this function which is called in sciwin and the previous one 
 * returns -1 when there's no current window 
 */
 
static int xget_curwin(void)
{
  BCG *Xgc= window_list_get_first();
  return  ( Xgc == NULL) ? -1 : Xgc->CurWindow;
}

/* Set a clip zone (rectangle ) **/

#ifdef PERIGTK 
static void xset_clip(BCG *Xgc,int x[])
{
  int i;
  GdkRectangle clip_rect ={x[0],x[1],x[2],x[3]};
  Xgc->ClipRegionSet = 1;
  for (i=0 ; i < 4 ; i++)   Xgc->CurClipRegion[i]= x[i];
  gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
}

#else 
/* Note that for Opengl we do both */
static void xset_clip(BCG *Xgc,int x[])
{
  int i;
  GdkRectangle clip_rect ={x[0],x[1],x[2],x[3]};
  Xgc->ClipRegionSet = 1;
  for (i=0 ; i < 4 ; i++)   Xgc->CurClipRegion[i]= x[i];
  clip_rectangle(Xgc, clip_rect);
  gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
}

#endif 


/* unset clip zone */

#ifdef PERIGTK 
static void xset_unclip(BCG *Xgc)
{
  static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
  Xgc->ClipRegionSet = 0;
  gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
}
#else 
/* Note that for Opengl we do both */
static void xset_unclip(BCG *Xgc)
{
  static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
  Xgc->ClipRegionSet = 0;
  gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
  unclip_rectangle(clip_rect);
}
#endif 

/* Get the boundaries of the current clip zone */

static void xget_clip(BCG *Xgc,int *x)
{
  x[0] = Xgc->ClipRegionSet;
  if ( x[0] == 1)
    {
      x[1] =Xgc->CurClipRegion[0];
      x[2] =Xgc->CurClipRegion[1];
      x[3] =Xgc->CurClipRegion[2];
      x[4] =Xgc->CurClipRegion[3];
    }
}


/*
 * For the private->drawing functions dealing with vectors of 
 * points, the following routine is used to select the mode 
 * absolute or relative 
 * Absolute mode if *num==0, relative mode if *num != 0
 */

/* to set absolute or relative mode */

static void xset_absourel(BCG *Xgc,int flag)
{
  if (flag == 0 )
    Xgc->CurVectorStyle =  CoordModeOrigin;
  else 
    Xgc->CurVectorStyle =  CoordModePrevious ;
}

/* to get information on absolute or relative mode */

static int xget_absourel(BCG *Xgc)
{
  return  Xgc->CurVectorStyle  ;
}

/* The alu function for private->drawing : Works only with X11
 * Not in Postscript, Read The X11 manual to get more informations 
 */

typedef struct alinfo_ { 
  char *name;
  char id;
  char *info;} alinfo;

static alinfo AluStruc_gtk [] =
  { 
    {"GXclear" , GDK_CLEAR," 0 "},
    {"GXand" , GDK_AND," src AND dst "},
    {"GXandReverse" , GDK_AND_REVERSE," src AND NOT dst "},
    {"GXcopy" , GDK_COPY," src "},
    {"GXandInverted" , GDK_AND_INVERT," NOT src AND dst "},
    {"GXnoop" , GDK_NOOP," dst "},
    {"GXxor" , GDK_XOR," src XOR dst "},
    {"GXor" , GDK_OR," src OR dst "},
    {"GXnor" , GDK_OR," NOT src AND NOT dst "}, /*  GDK_NOR:  XXX missing in gdk */
    {"GXequiv" , GDK_EQUIV," NOT src XOR dst "},
    {"GXinvert" , GDK_INVERT," NOT dst "},
    {"GXorReverse" , GDK_OR_REVERSE," src OR NOT dst "},
    {"GXcopyInverted" , GDK_COPY_INVERT," NOT src "},
    {"GXorInverted" , GDK_OR_INVERT," NOT src OR dst "},
    {"GXnand" , GDK_NAND," NOT src OR NOT dst "},
    {"GXset" , GDK_SET," 1 "}
  };

#ifdef PERIGL 
static struct alinfo_gl { 
  char *name;
  GLenum id;
  char *info;} AluStruc_[] =  { 
    {"GXclear" , GL_CLEAR," 0 "},
    {"GXand" , GL_AND," src AND dst "},
    {"GXandReverse" , GL_AND_REVERSE," src AND NOT dst "},
    {"GXcopy" , GL_COPY," src "},
    {"GXandInverted" , GL_AND_INVERTED," NOT src AND dst "},
    {"GXnoop" , GL_NOOP," dst "},
    {"GXxor" , GL_XOR," src XOR dst "},
    {"GXor" , GL_OR," src OR dst "},
    {"GXnor" , GL_NOR," NOT src AND NOT dst "}, /*  GDK_NOR:  XXX missing in gdk */
    {"GXequiv" , GL_EQUIV," NOT src XOR dst "},
    {"GXinvert" , GL_INVERT," NOT dst "},
    {"GXorReverse" , GL_OR_REVERSE," src OR NOT dst "},
    {"GXcopyInverted" , GL_COPY_INVERTED," NOT src "},
    {"GXorInverted" , GL_OR_INVERTED," NOT src OR dst "},
    {"GXnand" , GL_NAND," NOT src OR NOT dst "},
    {"GXset" , GL_SET," 1 "}
  };
#endif 


static void xset_alufunction1_gtk(BCG *Xgc,int num)
{   
  int value ; 
  GdkColor temp = {0,0,0,0};
  Xgc->CurDrawFunction = Min(15,Max(0,num));
  value = AluStruc_gtk[Xgc->CurDrawFunction].id;
  switch (value) 
    {
    case GDK_CLEAR : 
      gdk_gc_set_foreground(Xgc->private->wgc, &Xgc->private->gcol_bg);
      gdk_gc_set_background(Xgc->private->wgc, &Xgc->private->gcol_bg);
      gdk_gc_set_function(Xgc->private->wgc,GDK_COPY);
      break;
    case GDK_XOR   : 
      temp.pixel = Xgc->private->gcol_fg.pixel ^ Xgc->private->gcol_bg.pixel ;
      gdk_gc_set_foreground(Xgc->private->wgc, &temp);
      gdk_gc_set_background(Xgc->private->wgc, &Xgc->private->gcol_bg);
      gdk_gc_set_function(Xgc->private->wgc,GDK_XOR);
      break;
    default :
      gdk_gc_set_foreground(Xgc->private->wgc, &Xgc->private->gcol_fg);
      gdk_gc_set_background(Xgc->private->wgc, &Xgc->private->gcol_bg);
      gdk_gc_set_function(Xgc->private->wgc,value);
      break;
    }
  if ( value == GDK_XOR  && Xgc->CurColorStatus == 1 )
    {
      /* the way colors are computed changes if we are in Xor mode 
       * so we force here the computation of current color  
       */
      nsp_gtk_set_color(Xgc,Xgc->CurColor);
    }
}

#ifdef PERIGTK 
static void xset_alufunction1(BCG *Xgc,int num)
{
  xset_alufunction1_gtk(Xgc,num);
}
#endif 

#ifdef PERIGL
/* we call both functions */

static void xset_alufunction1(BCG *Xgc,int num)
{   
  GLenum value ; 
  Xgc->CurDrawFunction = Min(15,Max(0,num));
  value = AluStruc_[Xgc->CurDrawFunction].id;
  /* FIXME: is it a good choice ? 
   * to disable by default for GL_COPY
   */
  if ( value == GL_COPY ) 
    {
      glDisable(GL_COLOR_LOGIC_OP);
    }
  else 
    {
      glEnable(GL_COLOR_LOGIC_OP);
      glLogicOp(value);
    }
  xset_alufunction1_gtk(Xgc,num);
}
#endif 

static int xget_alufunction(BCG *Xgc)
{ 
  return  Xgc->CurDrawFunction ;
}

/*
 *  to set the thickness of lines : 0 is a possible value 
 *  it gives the thinest line (0 and 1 are the same for X11 but
 *  with diferent algorithms 
 *  defaut value is 1 
 */

static void xset_thickness(BCG *Xgc,int value)
{ 
  int val =  Xgc->CurDashStyle + 1;
  Xgc->CurLineWidth =Max(0, value);
  /* when line thickness changes we must change the dash style */
  xset_dash(Xgc,val);
}

/** to get the thickness value **/

static int xget_thickness(BCG *Xgc)
{
  return Xgc->CurLineWidth ;
}

/* This was used for b& white printers 
 * To set grey level for filing areas 
 * from black (*num =0 ) to white    
 * Pixmap  Tabpix_[GREYNUMBER]; 

 static char grey0[GREYNUMBER][8]={
 {(char)0x00, (char)0x00, (char)0x00, (char)0x00, (char)0x00, (char)0x00, (char)0x00, (char)0x00},
 {(char)0x00, (char)0x00, (char)0x44, (char)0x00, (char)0x00, (char)0x00, (char)0x44, (char)0x00},
 {(char)0x00, (char)0x44, (char)0x00, (char)0x22, (char)0x08, (char)0x40, (char)0x01, (char)0x20},
 {(char)0x00, (char)0x92, (char)0x00, (char)0x25, (char)0x00, (char)0x92, (char)0x00, (char)0xa4},
 {(char)0x55, (char)0x00, (char)0xaa, (char)0x00, (char)0x55, (char)0x00, (char)0xaa, (char)0x00},
 {(char)0xad, (char)0x00, (char)0x5b, (char)0x00, (char)0xda, (char)0x00, (char)0x6d, (char)0x00},
 {(char)0x6d, (char)0x02, (char)0xda, (char)0x08, (char)0x6b, (char)0x10, (char)0xb6, (char)0x20},
 {(char)0x6d, (char)0x22, (char)0xda, (char)0x0c, (char)0x6b, (char)0x18, (char)0xb6, (char)0x24},
 {(char)0x55, (char)0xaa, (char)0x55, (char)0xaa, (char)0x55, (char)0xaa, (char)0x55, (char)0xaa},
 {(char)0x92, (char)0xdd, (char)0x25, (char)0xf3, (char)0x94, (char)0xe7, (char)0x49, (char)0xdb},
 {(char)0x92, (char)0xfd, (char)0x25, (char)0xf7, (char)0x94, (char)0xef, (char)0x49, (char)0xdf},
 {(char)0x52, (char)0xff, (char)0xa4, (char)0xff, (char)0x25, (char)0xff, (char)0x92, (char)0xff},
 {(char)0xaa, (char)0xff, (char)0x55, (char)0xff, (char)0xaa, (char)0xff, (char)0x55, (char)0xff},
 {(char)0xff, (char)0x6d, (char)0xff, (char)0xda, (char)0xff, (char)0x6d, (char)0xff, (char)0x5b},
 {(char)0xff, (char)0xbb, (char)0xff, (char)0xdd, (char)0xf7, (char)0xbf, (char)0xfe, (char)0xdf},
 {(char)0xff, (char)0xff, (char)0xbb, (char)0xff, (char)0xff, (char)0xff, (char)0xbb, (char)0xff},
 {(char)0xff, (char)0xff, (char)0xff, (char)0xff, (char)0xff, (char)0xff, (char)0xff, (char)0xff},
 };

 void CreatePatterns(whitepixel, blackpixel)
 Pixel whitepixel;
 Pixel blackpixel;
 { 
  
 int i ;
 for ( i=0 ; i < GREYNUMBER ; i++)
 Tabpix_[i] =XCreatePixmapFromBitmapData(dpy, root,grey0[i] ,8,8,whitepixel
 ,blackpixel,XDefaultDepth (dpy,DefaultScreen(dpy)));
 
 }

*/

static int  xset_pattern(BCG *Xgc,int num)
{ 
  int old = xget_pattern(Xgc);
  if (Xgc->CurColorStatus == 1) 
    {
      nsp_gtk_set_color(Xgc,num-1);
    }
  else 
    {
      /* 
	 int i ; 
	 i= Max(0,Min(*num - 1,GREYNUMBER - 1));
	 Xgc->CurPattern = i;
	 XSetTile (dpy, gc, Tabpix_[i]); 
	 if (i ==0)
	 XSetFillStyle(dpy,gc,FillSolid);
	 else 
	 XSetFillStyle(dpy,gc,FillTiled);
      */
    }
  return old;
}

/* To get the id of the current pattern  */

static int xget_pattern(BCG *Xgc)
{ 
  if ( Xgc->CurColorStatus == 1 ) 
    return Xgc->CurColor + 1;
  else 
    return Xgc->CurPattern + 1;
}

/* To get the id of the last pattern in colormap */

static int xget_last(BCG *Xgc)
{
  if ( Xgc->CurColorStatus == 1 ) 
    {
      return Xgc->IDLastPattern + 1;
    }
  else 
    {
      return Xgc->IDLastPattern + 1;
    }
}

/* Line style 
 * use a table of dashes and set default X11-dash style to 
 *  one of the possible value. value points 
 *  to a strictly positive int 
 *   if *value == 0 -> Solid line  
 * else Dashed Line 
 */

#define MAXDASH 6

static int DashTab[MAXDASH][4] = {
  {2,5,2,5}, {5,2,5,2},  {5,3,2,3}, {8,3,2,3}, {11,3,2,3}, {11,3,5,3}};

static int  xset_dash(BCG *Xgc,int value)
{
  int old = xget_dash(Xgc);
  int  l2 = 4, l3;
  l3 = Max(0,Min(MAXDASH - 1,value - 1));
  xset_dashstyle(Xgc,l3,DashTab[l3],&l2);
  Xgc->CurDashStyle = l3;
  return old;
}


static int xget_dash(BCG *Xgc)
{
  return  Xgc->CurDashStyle + 1;
}

/* old version of xset_dash retained for compatibility */

static void xset_dash_and_color(BCG *Xgc,int dash,int color)
{
  xset_dash(Xgc,dash);
  xset_pattern(Xgc,color);
}

static void xset_line_style(BCG *Xgc,int value)
{
  if (Xgc->CurColorStatus == 0) 
    xset_dash(Xgc,value);
  else {
    xset_dash(Xgc,Xgc->CurDashStyle + 1);
    xset_pattern(Xgc,value);
  }
}

/*
 *  To change The X11-default dash style
 * if *value == 0, use a solid line, if *value != 0 
 * the dash style is specified by the xx vector of n values 
 * xx[3]={5,3,7} and *n == 3 means :  5white 3 void 7 white \ldots 
 */

static void xset_dashstyle_gtk(BCG *Xgc,int value, int *xx, int *n)
{
  if ( value == 0) 
    {
      gdk_gc_set_line_attributes(Xgc->private->wgc,
				 (Xgc->CurLineWidth <= 1) ? 0 : Xgc->CurLineWidth,
				 GDK_LINE_SOLID,GDK_CAP_BUTT, GDK_JOIN_ROUND);
    }
  else 
    {
      gint8 buffdash[18];
      int i;
      for ( i =0 ; i < *n ; i++) buffdash[i]=xx[i];
      gdk_gc_set_dashes(Xgc->private->wgc, 0, buffdash, *n);
      gdk_gc_set_line_attributes(Xgc->private->wgc, 
				 (Xgc->CurLineWidth == 0 ) ? 1 : Xgc->CurLineWidth,
				 GDK_LINE_ON_OFF_DASH, GDK_CAP_BUTT, GDK_JOIN_ROUND);
    }
}

#ifdef PERIGTK 
static void xset_dashstyle(BCG *Xgc,int value, int *xx, int *n)
{
  xset_dashstyle_gtk(Xgc,value,xx,n);
}
#endif 

#ifdef PERIGL
static void xset_dashstyle(BCG *Xgc,int value, int *xx, int *n)
{
  if ( value == 0) 
    {
      /* FIXME : */
      glLineWidth( ((Xgc->CurLineWidth <= 1) ? 1 : Xgc->CurLineWidth)*0.5);
    }
  else 
    {
#if 0 
      gint8 buffdash[18];
      int i;
      for ( i =0 ; i < *n ; i++) buffdash[i]=xx[i];
      gdk_gc_set_dashes(Xgc->private->wgc, 0, buffdash, *n);
      gdk_gc_set_line_attributes(Xgc->private->wgc, 
				 (Xgc->CurLineWidth == 0 ) ? 1 : Xgc->CurLineWidth,
				 GDK_LINE_ON_OFF_DASH, GDK_CAP_BUTT, GDK_JOIN_ROUND);
#endif 
    }
  xset_dashstyle_gtk(Xgc,value,xx,n);
}
#endif 

/* 
   static void xget_dashstyle(BCG *Xgc,int *n,int *value)
   {
   int i ;
   *n =1 ;
   *value = Xgc->CurDashStyle + 1;
   if (*value != 1) 
   {
   value[1]=4;
   *n = value[1]+2;
   for (i = 0 ; i < value[1]; i++) value[i+2]=DashTab[*value-2][i];
   }
   }
*/

/* to get the current dash-style 
 * old version of xget_dash retained for compatibility 
 */

static void xget_dash_and_color(BCG *Xgc,int *dash,int *color)
{
  *dash = xget_dash(Xgc);
  *color = xget_pattern(Xgc);
}

/* used to switch from color to b&w and reverse */

static void xset_usecolor(BCG *Xgc,int num)
{
  int i;
  i =  Min(Max(num,0),1);
  if ( Xgc->CurColorStatus != (int) i) 
    {
      if (Xgc->CurColorStatus == 1) 
	{
	  /* from color to b&w */
	  Xgc->CurColorStatus = 1;
	  xset_pattern(Xgc,1);
	  /* go to b&w */
	  Xgc->CurColorStatus = 0;
	  i= Xgc->CurPattern + 1;
	  xset_pattern(Xgc,i);
	  i= Xgc->CurDashStyle + 1;
	  xset_dash(Xgc,i);
	  Xgc->IDLastPattern = GREYNUMBER - 1;
	}
      else 
	{
	  /* switching to color mode */
	  /* patterns and dashes reinitialization */
	  /* colors too */
	  Xgc->CurColorStatus = 0;
	  xset_pattern(Xgc,1);
	  xset_dash(Xgc,1);
	  /* switching to color mode */
	  Xgc->CurColorStatus = 1;
	  i= Xgc->CurColor + 1;
	  xset_pattern(Xgc,i);
	  xset_pattern(Xgc,i);
	  Xgc->IDLastPattern = Xgc->Numcolors - 1;
	}
    }
}

static int xget_usecolor(BCG *Xgc)
{
  return  Xgc->CurColorStatus;
}

/* Change the private->pixmap status of a Graphic Window. 
 * adding or removing a Background Pixmap to it 
 * FIXME: to be finished for opengl since we have 
 * to add a gldrawable to the extra pixmap
 */

static void xset_pixmapOn(BCG *Xgc,int num)
{ 
  int num1= Min(Max(num,0),1);
  if ( Xgc->CurPixmapStatus == num1 ) return;
  if ( num1 == 1 )
    {
      GdkDrawable *temp ;
      /** create a new pixmap **/
      temp = (GdkDrawable *) gdk_pixmap_new(Xgc->private->drawing->window,
					    Xgc->CWindowWidth, Xgc->CWindowHeight,
					    -1);
      if ( temp  == NULL ) 
	{
	  xinfo(Xgc, "Not enough space to switch to Animation mode");
	}
      else 
	{
	  xinfo(Xgc,"Animation mode is on,( xset('pixmap',0) to leave)");
	  Xgc->private->drawable = Xgc->private->extra_pixmap = temp;
	  Xgc->CurPixmapStatus = 1;
	  pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
	}
    }
  else 
    {
      /** I remove the extra pixmap to the window **/
      xinfo(Xgc," ");
      gdk_pixmap_unref((GdkPixmap *) Xgc->private->extra_pixmap);
      Xgc->private->extra_pixmap = NULL;
      Xgc->private->drawable = (GdkDrawable *)Xgc->private->pixmap;
      Xgc->CurPixmapStatus = 0; 
    }
}

static int xget_pixmapOn(BCG *Xgc)
{
  return Xgc->CurPixmapStatus;
}

/* Change the status of a Graphic Window
 * i.e follows or dont follow the viewport resize
 * Here the behaviour is different 
 * If we want the graphic window to follow the viewport resize 
 * (i.e we dont want to see scrollbars) then we fix the minimum 
 * size of the grahic window to very small values 
 */

static void xset_wresize(BCG *Xgc,int num)
{
  GdkGeometry geometry;
  GdkWindowHints geometry_mask;
  int num1= Min(Max(num,0),1);
  if ( num1 != Xgc->CurResizeStatus && num1 == 1) 
    {
      /* we want here that the graphic window follows the viewport resize 
       * remove the scrolled window size hints 
       */
      geometry.max_width = G_MAXSHORT;
      geometry.max_height = G_MAXSHORT;
      geometry_mask = GDK_HINT_MAX_SIZE ; 
      gtk_window_set_geometry_hints (GTK_WINDOW (Xgc->private->window), Xgc->private->scrolled,
				     &geometry, geometry_mask);
      /* remove the min size request */
      gtk_widget_set_size_request(Xgc->private->drawing,0,0);
      Xgc->CurResizeStatus = num1 ;
    }
  else 
    {
      int w,h;
      gdk_window_get_size (Xgc->private->drawing->window,&w,&h);
      Xgc->CurResizeStatus = num1 ;
      xset_windowdim(Xgc,w,h);
    }
}

static int xget_wresize(BCG *Xgc)
{
  return Xgc->CurResizeStatus;
}

/* XXXX setting the default colormap with colors defined in color.h */

static int set_default_colormap_flag = 1;

static void sedeco(int flag)
{
  set_default_colormap_flag = flag;
}

/* set_default_colormap is called when raising a window for the first 
 * time by xset('window',...) or by getting back to default by 
 * xset('default',...) 
 */

static int XgcAllocColors( BCG *xgc, int m);
static void set_colormap_constants(BCG *Xgc,int m);

static void xset_default_colormap(BCG *Xgc)
{
  int i,m ;
  GdkColor *colors_old;
  /*  we don't want to set the default colormap at window creation 
   *  if the scilab command was xset("colormap"); 
   */
  if ( Xgc->CmapFlag == 1 ) return ; /* default colormap already */
  if (set_default_colormap_flag == 0) return;
  if (DEFAULTNUMCOLORS > maxcol) {
    Sciprintf("Not enough colors for default colormap. Maximum is %d\n", maxcol);
    return;
  }
  m = DEFAULTNUMCOLORS;
  /* Save old color vectors */
  colors_old = Xgc->private->colors;
  if (!XgcAllocColors(Xgc,m)) {
    Xgc->private->colors =     colors_old ;
    return;
  }

  /* get pixel values for the colors we let gtk do the job 
   * an other way to fix a pixel value but which could only work for X 
   * if rgb are coded as guchar
   * gdk_rgb_xpixel_from_rgb((r << 16)|(g << 8)|(b))
   */

  if ( Xgc->private->drawing == NULL ) return;
  if ( Xgc->private->colormap == NULL ) 
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);
  for (i = 0; i < m; i++) {
    Xgc->private->colors[i].red = (default_colors[3*i] << 8);
    Xgc->private->colors[i].green = (default_colors[3*i+1] << 8);
    Xgc->private->colors[i].blue = (default_colors[3*i+2] << 8);
    gdk_rgb_find_color (Xgc->private->colormap,&Xgc->private->colors[i]);      
  }
  set_colormap_constants(Xgc,m);
  FREE(colors_old);
}

/* Setting the colormap 
 *   a must be a m x 3 double RGB matrix: 
 *   a[i] = RED
 *   a[i+m] = GREEN
 *   a[i+2*m] = BLUE
 * v2 gives the value of m and *v3 must be equal to 3 
 */

static void xset_colormap(BCG *Xgc,int m,int n,double *a)
{
  int i ;
  GdkColor *colors_old;
  /* 2 colors reserved for black and white */
  if ( n != 3 || m  < 0 || m > maxcol - 2) {
    Scierror("Colormap must be a m x 3 array with m <= %ld\n", maxcol-2);
    return;
  }

  colors_old = Xgc->private->colors;
  if (!XgcAllocColors(Xgc,m)) {
    Xgc->private->colors =     colors_old ;
    return;
  }

  if ( Xgc->private->drawing == NULL ) return;
  if ( Xgc->private->colormap == NULL ) 
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);

  for (i = 0; i < m; i++) {
  }

  /* Checking RGB values */
  for (i = 0; i < m; i++) 
    {
      if (a[i] < 0 || a[i] > 1 || a[i+m] < 0 || a[i+m] > 1 ||
	  a[i+2*m] < 0 || a[i+2*m]> 1) 
	{
	  Sciprintf("RGB values must be between 0 and 1\n");
	  FREE(Xgc->private->colors);
	  Xgc->private->colors = colors_old ;
	  return;
	}
      Xgc->private->colors[i].red = (guint16)  (a[i]*65535);
      Xgc->private->colors[i].green = (guint16)(a[i+m]*65535);
      Xgc->private->colors[i].blue = (guint16) (a[i+2*m]*65535);
      gdk_rgb_find_color (Xgc->private->colormap,&Xgc->private->colors[i]);      
    }
  set_colormap_constants(Xgc,m);
  FREE(colors_old);
}

/* utility function */

static int XgcAllocColors( BCG *xgc, int m)
{
  /* don't forget black and white */
  int mm = m + 2;
  if ( ! ( xgc->private->colors = MALLOC(mm*sizeof(GdkColor))))
    {
      Sciprintf("memory exhausted: unable to alloc an array of GdkColors\n");
      FREE(xgc->private->colors);
      return 0;
    }
  return 1;
}

static void set_colormap_constants(BCG *Xgc,int m)
{
  /* Black */
  if ( Xgc->private->drawing == NULL ) return;
  if ( Xgc->private->colormap == NULL ) 
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);
  Xgc->private->colors[m].red = 0; 
  Xgc->private->colors[m].green = 0;
  Xgc->private->colors[m].blue = 0;
  gdk_rgb_find_color (Xgc->private->colormap,&Xgc->private->colors[m]);      
  /* White */
  Xgc->private->colors[m+1].red = 65535; 
  Xgc->private->colors[m+1].green = 65535; 
  Xgc->private->colors[m+1].blue = 65535; 
  gdk_rgb_find_color (Xgc->private->colormap,&Xgc->private->colors[m+1]);      
  /* constants */
  Xgc->Numcolors = m;
  Xgc->IDLastPattern = m - 1;
  Xgc->CmapFlag = 0;
  Xgc->NumForeground = m;
  Xgc->NumBackground = m + 1;
  xset_usecolor(Xgc,1);
  xset_alufunction1(Xgc,Xgc->CurDrawFunction);
  xset_pattern(Xgc,Xgc->NumForeground+1);
  xset_foreground(Xgc,Xgc->NumForeground+1);
  xset_background(Xgc,Xgc->NumForeground+2);
}

/* getting the colormap */


static void xget_colormap(BCG *Xgc, int *num,  double *val,int color_id)
{
  int m = Xgc->Numcolors;
  int i;
  *num = m;
  if ( val != NULL )
    {
      if ( color_id != 0 ) 
	{
	  /* just return one color */
	  if ( color_id >= 1 && color_id <= m )
	    {
	      int i=color_id-1;
	      val[0] = (double)Xgc->private->colors[i].red/65535.0;
	      val[1] = (double)Xgc->private->colors[i].green/65535.0;
	      val[2] = (double)Xgc->private->colors[i].blue/65535.0;
	    }
	  else 
	    {
	      val[0]= val[1] =val[2]=0;
	    }
	}
      else 
	{
	  for (i = 0; i < m; i++) {
	    val[i] = (double)Xgc->private->colors[i].red/65535.0;
	    val[i+m] = (double)Xgc->private->colors[i].green/65535.0;
	    val[i+2*m] = (double)Xgc->private->colors[i].blue/65535.0;
	  }
      }
    }
}

/* set and get the number of the background or foreground */

static void xset_background_gtk(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      int bg = Xgc->NumBackground =  Max(0,Min(num - 1,Xgc->Numcolors + 1));
      if (Xgc->private->colors != NULL )
	{
	  /* we fix the default background in Xgc->private->gcol_bg */
	  Xgc->private->gcol_bg = Xgc->private->colors[bg];
	}
      /* 
       * if we change the background of the window we must change 
       * the gc ( with alufunction ) and the window background 
       */
      xset_alufunction1(Xgc,Xgc->CurDrawFunction);
      gdk_window_set_background(Xgc->private->drawing->window, &Xgc->private->gcol_bg);
    }
}

#ifdef PERIGTK 
static void xset_background(BCG *Xgc,int num)
{
  xset_background_gtk(Xgc,num);
}
#endif 

#ifdef PERIGL 

static void xset_background(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      int bg = Xgc->NumBackground =  Max(0,Min(num - 1,Xgc->Numcolors + 1));
      if (Xgc->private->colors != NULL )
	{
	  /* we fix the default background in Xgc->private->gcol_bg */
	  Xgc->private->gcol_bg = Xgc->private->colors[bg];
	}

      /* 
       * if we change the background of the window we must change 
       * the gc ( with alufunction ) and the window background 
       */
      /*  xset_alufunction1(Xgc,Xgc->CurDrawFunction);
       *  gdk_window_set_background(Xgc->private->drawing->window, &Xgc->private->gcol_bg);
       */
      /* FIXME 
	 glClearColor(Xgc->private->gcol_bg.red /255.0,
	 Xgc->private->gcol_bg.green /255.0,
	 Xgc->private->gcol_bg.blue /255.0,0.0);
      */
    }
  xset_background_gtk(Xgc,num);
}
#endif 

 
static int  xget_background(BCG *Xgc)
{ 
  return ( Xgc->CurColorStatus == 1 ) ? Xgc->NumBackground + 1 : 1;
}

/* set and get the number of the background or foreground */

static void xset_foreground(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      int fg = Xgc->NumForeground = Max(0,Min(num - 1,Xgc->Numcolors + 1));
      if (Xgc->private->colors != NULL )
	{
	  Xgc->private->gcol_fg =  Xgc->private->colors[fg];
	  xset_alufunction1(Xgc,Xgc->CurDrawFunction);
	}
    }
}

static int xget_foreground(BCG *Xgc)
{ 
  if ( Xgc->CurColorStatus == 1 ) 
    {
      return  Xgc->NumForeground + 1;
    }
  else 
    {
      return 1 ;
    }
}

/* set and get the number of the hidden3d color 
 * which is used for backface drawing in 3d plot
 * > 0 then a color is used
 * 0 : no painting 
 * < 0 same as face color
 */

static void xset_hidden3d(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      Xgc->NumHidden3d = Max(-2,Min(num - 1,Xgc->Numcolors + 1));
    }
}

static int xget_hidden3d(BCG *Xgc)
{ 
  if ( Xgc->CurColorStatus == 1 ) 
    {
      return  Xgc->NumHidden3d + 1;
    }
  else 
    {
      return  1; /* the hidden3d is a solid line style in b&w */
    }
}

/*
 * 
 */

static void xset_autoclear(BCG *Xgc,int num)
{ 
  Xgc->Autoclear = Max(0,Min(1,num));
}

static void xset_autoclear_def(BCG *Xgc) 
{
  Xgc->Autoclear = 0;
}

static int xget_autoclear(BCG *Xgc)
{ 
  return  Xgc->Autoclear;
}

static char *xget_fpf(BCG *Xgc)
{
  return( Xgc->fp_format);
}

static void xset_fpf(BCG *Xgc,char *fmt) 
{
  strncpy(Xgc->fp_format,fmt,32);
}

static void xset_fpf_def(BCG *Xgc) 
{
  Xgc->fp_format[0]='\0';
}


/*-----------------------------------------------------------
 * Functions for private->drawing 
 *-----------------------------------------------------------*/

/*
 *  display of a string
 *  at (x,y) position whith slope angle alpha in degree . 
 * Angle are given clockwise. 
 * If *flag ==1 and angle is z\'ero a framed box is added 
 * around the string}.
 * 
 * (x,y) defines the lower left point of the bounding box 
 * of the string ( we do not separate asc and desc 
 */

static void displaystring(BCG *Xgc,char *string, int x, int y,  int flag, double angle) 
{ 
  gint lbearing, rbearing, iascent, idescent, iwidth;
  DRAW_CHECK_GDK;
  gdk_string_extents(Xgc->private->font,"X", &lbearing, &rbearing,
		     &iwidth, &iascent, &idescent);
  if ( Abs(angle) <= 0.1) 
    {
      gdk_draw_text(Xgc->private->drawable,Xgc->private->font,Xgc->private->wgc, 
		    x, y - idescent , string, strlen(string));
      if ( flag == 1) 
	{
	  int rect[] = { x , y- iascent - idescent, 
			 gdk_string_width(Xgc->private->font, string),
			 iascent+idescent};
	  drawrectangle(Xgc,rect);
	}
    }
  else 
    {
      gdk_draw_text_rot(Xgc->private->drawable,Xgc->private->font,Xgc->private->wgc, 
			x, y - idescent ,0,0, string, strlen(string),-angle * M_PI/180.0);
    }
}

/* To get the string extents of a string */

static void boundingbox(BCG *Xgc,char *string, int x, int y, int *rect)
{ 
  gint lbearing, rbearing, iascent, idescent, iwidth;
  gdk_string_extents(Xgc->private->font,"X", &lbearing, &rbearing, &iwidth, &iascent, &idescent);
  rect[0]= x ;
  rect[1]= y - iascent - idescent;
  rect[2]= gdk_string_width(Xgc->private->font, string);
  rect[3]= iascent + idescent;
}

/*------------------------------------------------
 * line segments arrows 
 *-------------------------------------------------*/

static void drawline(BCG *Xgc,int x1, int y1, int x2, int y2)
{
  DRAW_CHECK;
#ifdef PERIGTK 
  gdk_draw_line(Xgc->private->drawable,Xgc->private->wgc, x1, y1, x2, y2);
#else 
  glBegin(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glEnd();
#endif
}

#ifdef PERIGL
static void drawline3D(BCG *Xgc,double x1,double y1, double z1, double x2,double y2, double z2)
{
  DRAW_CHECK;
  glBegin(GL_LINES);
  glVertex3d(x1, y1, z1);
  glVertex3d(x2, y2, z2);
  glEnd();
}
#endif 


/* Draw a set of segments 
 * segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) 
 * for i=0 step 2 
 * n is the size of vx and vy 
 */

static void drawsegments(BCG *Xgc, int *vx, int *vy, int n, int *style, int iflag)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->drawsegments(Xgc,vx,vy,n,style,iflag);
}

#ifdef PERIGL 
void drawsegments3D(BCG *Xgc,double *x,double *y,double *z, int n, int *style, int iflag)
{
  int dash,color,i;
  DRAW_CHECK;
  xget_dash_and_color(Xgc,&dash,&color);
  if ( iflag == 1) { /* one style per segment */
    for (i=0 ; i < n/2 ; i++) {
      xset_line_style(Xgc,style[i]);
      drawline3D(Xgc,x[2*i],y[2*i],z[2*i],x[2*i+1],y[2*i+1],z[2*i+1]);
    }
  }
  else {
    if (*style >= 1) xset_line_style(Xgc,*style);
    /* une fonction gtk existe ici FIXME */
    for (i=0 ; i < n/2 ; i++) 
      drawline3D(Xgc,x[2*i],y[2*i],z[2*i],x[2*i+1],y[2*i+1],z[2*i+1]);
  }
  xset_dash_and_color(Xgc,dash,color);
}
#endif 



/* Draw a set of arrows 
 * arrows are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) 
 * for i=0 step 2 
 * n is the size of vx and vy 
 * as is 10*arsize (arsize) the size of the arrow head in pixels 
 */

static void drawarrows(BCG *Xgc, int *vx, int *vy, int n, int as, int *style, int iflag)
{ 
  DRAW_CHECK;
  Xgc->graphic_engine->generic->drawarrows(Xgc,vx,vy,n,as,style,iflag);
}

/*
 * Rectangles
 * Draw or fill a set of rectangle 
 * rectangle i is specified by (vect[i],vect[i+1],vect[i+2],vect[i+3]) 
 * for x,y,width,height 
 * for i=0 step 4 
 * (*n) : number of rectangles 
 * fillvect[*n] : specify the action  
 * if fillvect[i] is > 0 then fill the rectangle i 
 * if fillvect[i] is == 0  then only draw the rectangle i 
 *                         with the current private->drawing style 
 * if fillvect[i] is < 0 then draw the  rectangle with -fillvect[i] 
 */

static void drawrectangles(BCG *Xgc,const int *vects,const int *fillvect, int n)
{
  int i,dash,color;
  DRAW_CHECK;
  xget_dash_and_color(Xgc,&dash,&color);
  for (i = 0 ; i < n ; i++)
    {
      if ( fillvect[i] < 0 )
	{
	  int dash = - fillvect[i];
	  xset_line_style(Xgc,dash);
	  drawrectangle(Xgc,vects+4*i);
	}
      else if ( fillvect[i] == 0 ) 
	{
	  /* xset_line_style(cd,PI0,PI0,PI0);*/
	  drawrectangle(Xgc,vects+4*i);
	}
      else
	{
	  xset_pattern(Xgc,fillvect[i]);
	  fillrectangle(Xgc,vects+4*i);
	}
    }
  xset_dash_and_color(Xgc,dash,color);
}

/* Draw one rectangle with current line style 
 */


static void drawrectangle(BCG *Xgc,const int rect[])
{ 
  DRAW_CHECK
#ifdef PERIGTK 
    gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, FALSE,
		       rect[0],rect[1],rect[2],rect[3]);
#else 
  glBegin(GL_LINE_LOOP);
  glVertex2i(rect[0]        ,rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]+rect[3]);
  glVertex2i(rect[0]        ,rect[1]+rect[3]);
  glEnd();
#endif
}

/* fill one rectangle, with current pattern */


static void fillrectangle(BCG *Xgc,const int rect[])
{ 
  DRAW_CHECK;
#ifdef PERIGTK 
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, TRUE,rect[0],rect[1],rect[2],rect[3]);
#else 
  glBegin(GL_QUADS);
  glVertex2i(rect[0]        ,rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]+rect[3]);
  glVertex2i(rect[0]        ,rect[1]+rect[3]);
  glEnd();
#endif 
}

/*
 * draw a set of rectangles, provided here to accelerate GraySquare for X11 device 
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : is the value of a function on the grid defined by x,y 
 *  on each rectangle the average value of z is computed 
 */

static  void fill_grid_rectangles(BCG *Xgc,const int x[],const int y[],const double z[], int nx, int ny,
				  int remap,const int *colminmax,const double *zminmax)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->fill_grid_rectangles(Xgc,x,y,z,nx,ny,remap,colminmax,zminmax);
}

/*
 * draw a set of rectangles, provided here to accelerate GraySquare1 for X11 device 
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : of size (n1-1)*(n2-1)  gives the f-values on the middle 
 *  of each rectangle. 
 *  z[i,j] is the value on the middle of rectangle 
 *        P1= x[i],y[j] x[i+1],y[j+1]
 */

static void fill_grid_rectangles1(BCG *Xgc,const int x[],const int y[],const double z[], int nr, int nc,
				  int remap,const int *colminmax,const double *zminmax)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->fill_grid_rectangles1(Xgc,x,y,z,nr,nc,remap,colminmax,zminmax);
}


/*
 * Circles and Ellipsis 
 * Draw or fill a set of ellipsis or part of ellipsis 
 * Each is defined by 6-parameters, 
 * ellipsis i is specified by $vect[6*i+k]_{k=0,5}= x,y,width,height,angle1,angle2$ 
 * <x,y,width,height> is the bounding box 
 * angle1,angle2 specifies the portion of the ellipsis 
 * caution : angle=degreangle*64          
 * if fillvect[i] is in [1,lastpattern] then  fill the ellipsis i 
 * with pattern fillvect[i] 
 * if fillvect[i] is > lastpattern  then only draw the ellipsis i 
 * The private->drawing style is the current private->drawing 
 */

static void fillarcs(BCG *Xgc,int *vects, int *fillvect, int n) 
{
  int i,cpat,verb;
  DRAW_CHECK;
  verb=0;
  cpat = xget_pattern(Xgc);
  for (i=0 ; i< n ; i++)
    {
      if (fillvect[i] > Xgc->IDLastPattern + 1)
	{
	  xset_pattern(Xgc,cpat);
	  drawarc(Xgc,vects+6*i);
	}
      else
	{
	  xset_pattern(Xgc,fillvect[i]);
	  fillarc(Xgc,vects+6*i);
	}
    }
  xset_pattern(Xgc,cpat);
}

/*
 * Draw a set of ellipsis or part of ellipsis 
 * Each is defined by 6-parameters, 
 * ellipsis i is specified by $vect[6*i+k]_{k=0,5}= x,y,width,height,angle1,angle2$ 
 * <x,y,width,height> is the bounding box 
 * angle1,angle2 specifies the portion of the ellipsis 
 * caution : angle=degreangle*64          
 */


static void drawarcs(BCG *Xgc, int *vects, int *style, int n)
{
  int dash,color,i;
  DRAW_CHECK;
  /* store the current values */
  xget_dash_and_color(Xgc,&dash,&color);
  for (i=0 ; i< n ; i++)
    {
      xset_line_style(Xgc,style[i]);
      drawarc(Xgc,vects+6*i);
    }
  xset_dash_and_color(Xgc,dash,color);
}

/* Draw a single ellipsis or part of it 
 */

static void drawarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK;
  /* FIXME : to be done for OpenGl */
  gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,FALSE,
	       arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
}

/** Fill a single elipsis or part of it with current pattern **/

static void fillarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK;
  /* FIXME : to be done for OpenGl */
  gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,TRUE,
	       arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
}

/*
 * Filling or Drawing Polylines and Polygons
 */

/* 
 * Draw a set of (*n) polylines (each of which have (*p) points) 
 * with lines or marks 
 * drawvect[i] <= 0 use a mark for polyline i
 * drawvect[i] >  0 use a line style for polyline i 
 */

static void drawpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *drawvect,int n, int p)
{ 
  int symb[2],dash,color,i,close;
  /* DRAW_CHECK; remove since we want DRAW_CHECK_GDK for drawpolymark */
  /* store the current values */
  xget_mark(Xgc,symb);
  xget_dash_and_color(Xgc,&dash,&color);
  for (i=0 ; i< n ; i++)
    {
      if (drawvect[i] <= 0)
	{ /** we use the markid : drawvect[i] : with current dash **/
	  xset_mark(Xgc,- drawvect[i],symb[1]);
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolymark(Xgc,vectsx+(p)*i,vectsy+(p)*i,p);
	}
      else
	{/** we use the line-style number abs(drawvect[i])  **/
	  xset_line_style(Xgc,*(drawvect+i));
	  close = 0;
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,close);
	}
    }
  /** back to default values **/
  xset_dash_and_color(Xgc,dash,color);
  xset_mark(Xgc,symb[0],symb[1]);
}

/*
 *  fill a set of polygons each of which is defined by 
 * (*p) points (*n) is the number of polygons 
 * the polygon is closed by the routine 
 * fillvect[*n] :         
 * if fillvect[i] == 0 draw the boundaries with current color 
 * if fillvect[i] > 0  draw the boundaries with current color 
 *               then fill with pattern fillvect[i]
 * if fillvect[i] < 0  fill with pattern - fillvect[i]
 */

#ifdef PERIGTK

static void fillpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *fillvect,int n, int p)
{
  int dash,color,i;
  DRAW_CHECK;
  xget_dash_and_color(Xgc,&dash,&color);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{ 
	  /** fill + boundaries **/
	  xset_pattern(Xgc,fillvect[i]);
	  fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else 
	{
	  xset_pattern(Xgc,-fillvect[i]);
	  fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  xset_pattern(Xgc,color);
	}
    }
  xset_dash_and_color(Xgc,dash,color);
}

#else 

/* specific version for OpenGl to add a glEnable(GL_POLYGON_OFFSET_FILL);
 */

static void fillpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *fillvect,int n, int p)
{
  int dash,color,i;
  DRAW_CHECK;
  xget_dash_and_color(Xgc,&dash,&color);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{ 
	  /* fill + boundaries **/
	  xset_pattern(Xgc,fillvect[i]);
	  glEnable(GL_POLYGON_OFFSET_FILL);
	  glPolygonOffset(1.0,1.0);
	  fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  glEnable(GL_POLYGON_OFFSET_FILL);
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else 
	{
	  xset_pattern(Xgc,-fillvect[i]);
	  fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  xset_pattern(Xgc,color);
	}
    }
  xset_dash_and_color(Xgc,dash,color);
}

#endif 

/* 
 * Only draw one polygon  with current line style 
 * according to *closeflag : it's a polyline or a polygon
 * n is the number of points of the polyline 
 */

#ifdef PERIGTK 

static void drawpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag)
{ 
  int n1;
  DRAW_CHECK;
  if (closeflag == 1) n1 =n+1;else n1= n;
  if (n1 >= 2) 
    {
      if ( gtk_store_points(n, vx, vy, closeflag)) 
	{
	  gdk_draw_lines(Xgc->private->drawable,Xgc->private->wgc, gtk_get_xpoints(), n1);
	}
    }
}

#else 

static void drawpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag)
{ 
  gint i;
  if ( n <= 1) return;
  DRAW_CHECK;
  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
  if ( closeflag == 1 ) 
    glBegin(GL_LINE_LOOP);
  else
    glBegin(GL_LINE_STRIP);
  for (i=0; i < n ; i++) glVertex2i(vx[i], vy[i]);
  glEnd();
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
}

#endif 

/* 
 * Fill the polygon or polyline 
 * according to *closeflag : the given vector is a polyline or a polygon 
 */

#ifdef PERIGTK 

static void fillpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag) 
{
  int n1;
  DRAW_CHECK;
  if (closeflag == 1) n1 = n+1;else n1= n;
  if ( gtk_store_points(n, vx, vy, closeflag)) 
    {
      gdk_draw_polygon(Xgc->private->drawable,Xgc->private->wgc,TRUE,gtk_get_xpoints(), n1);
    }
}

#else 

/* 
 * FIXME: Attention �a ne marche que pour un polygone convexe !!!!!!
 * sinon il faut le trianguler 
 */

static void fillpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag) 
{
  gint i;
  if ( n <= 1) return;
  DRAW_CHECK;
  glBegin(GL_POLYGON);
  for ( i=0 ;  i< n ; i++) glVertex2i( vx[i], vy[i]);
  glEnd();
  
}

#endif

#ifdef PERIGL
void fillpolyline2D_shade(BCG *Xgc,int *vx, int *vy, int *colors, int n,int closeflag)
{
  gint i;
  if ( n <= 1) return;
  DRAW_CHECK;
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(1.0,1.0);
  glBegin(GL_POLYGON);
  for ( i=0 ;  i< n ; i++) 
    {
      xset_pattern(Xgc,Abs(colors[i]));
      glVertex2i( vx[i], vy[i]);
    }
  glEnd();
  glDisable(GL_POLYGON_OFFSET_FILL);
}

static void fillpolyline3D_shade(BCG *Xgc, double *vx, double *vy, double *vz,int *colors, int n,int closeflag) ;

void fillpolylines3D_shade(BCG *Xgc,double *vectsx, double *vectsy, 
			   double *vectsz, int *fillvect,int n, int p)
{
  int dash,color,i;
  DRAW_CHECK;
  xget_dash_and_color(Xgc,&dash,&color);
  for (i = 0 ; i< n ; i++)
    {
      /* for each polyline we only take a decision according to the first color */
      if (fillvect[i] > 0 )
	{ 
	  /* fill + boundaries **/
	  glEnable(GL_POLYGON_OFFSET_FILL);
	  glPolygonOffset(1.0,1.0);
	  fillpolyline3D_shade(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,fillvect+(p)*i,p,1);
	  glDisable(GL_POLYGON_OFFSET_FILL);
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else 
	{
	  fillpolyline3D_shade(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,fillvect+(p)*i,p,1);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	}
    }
  xset_dash_and_color(Xgc,dash,color);
}


static void fillpolyline3D_shade(BCG *Xgc, double *vx, double *vy, double *vz,int *colors, int n,int closeflag) 
{
  gint i;
  if ( n <= 1) return;
  DRAW_CHECK;
  glBegin(GL_POLYGON);
  for ( i=0 ;  i< n ; i++) 
    {
      xset_pattern(Xgc,Abs(colors[i]));
      glVertex3d( vx[i], vy[i], vz[i]);
    }
  glEnd();
}

/*
 * fillpolylines3D:
 * the same for 3D vertices 
 * FIXME: a rajouter ds la table et rendre statique 
 */

void fillpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p)
{
  int dash,color,i;
  DRAW_CHECK;
  xget_dash_and_color(Xgc,&dash,&color);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{ 
	  /* fill + boundaries **/
	  Xgc->graphic_engine->xset_pattern(Xgc,fillvect[i]);
	  glEnable(GL_POLYGON_OFFSET_FILL);
	  glPolygonOffset(1.0,1.0);
	  fillpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	  glDisable(GL_POLYGON_OFFSET_FILL);
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else 
	{
	  Xgc->graphic_engine->xset_pattern(Xgc,-fillvect[i]);
	  fillpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	}
    }
  xset_dash_and_color(Xgc,dash,color);
}

static void fillpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag) 
{
  gint i;
  if ( n <= 1) return;
  DRAW_CHECK;
  glBegin(GL_POLYGON);
  for ( i=0 ;  i< n ; i++) glVertex3d( vx[i], vy[i], vz[i]);
  glEnd();
}


static void drawpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag)
{ 
  gint i;
  if ( n <= 1) return;
  DRAW_CHECK;
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  if ( closeflag == 1 ) 
    glBegin(GL_LINE_LOOP);
  else
    glBegin(GL_LINE_STRIP);
  for (i=0; i < n ; i++) glVertex3d(vx[i], vy[i], vz[i]);
  glEnd();
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
}
#endif 


/* 
 * Draw the current mark centred at points defined
 * by vx and vy (vx[i],vy[i]) 
 */

#ifdef PERIGTK 

static void drawpolymark(BCG *Xgc,int *vx, int *vy,int n)
{
  DRAW_CHECK;
  if ( Xgc->CurHardSymb == 0 )
    {
      if (gtk_store_points(n, vx, vy,(int)0L))
	{
	  gdk_draw_points(Xgc->private->drawable,
			  Xgc->private->wgc,gtk_get_xpoints(), n);
	}
    }
  else 
    { 
      int i,keepid,keepsize,hds;
      i=1;
      keepid =  Xgc->fontId;
      keepsize= Xgc->fontSize;
      hds= Xgc->CurHardSymbSize;
      xset_font(Xgc,i,hds);
      for ( i=0; i< n ;i++) draw_mark(Xgc,vx+i,vy+i);
      xset_font(Xgc,keepid,keepsize);
    }
}

#else 

static void drawpolymark(BCG *Xgc,int *vx, int *vy,int n)
{
  DRAW_CHECK_GDK;

  if ( Xgc->CurHardSymb == 0 )
    {
      gint i;
      glBegin(GL_POINTS);
      for (i=0; i< n ; i++) glVertex2i( vx[i], vy[i]);
      glEnd();
    }
  else 
    { 
      int i,keepid,keepsize,hds;
      i=1;
      keepid =  Xgc->fontId;
      keepsize= Xgc->fontSize;
      hds= Xgc->CurHardSymbSize;
      xset_font(Xgc,i,hds);
      for ( i=0; i< n ;i++) draw_mark(Xgc,vx+i,vy+i);
      xset_font(Xgc,keepid,keepsize);
    }
}

#endif

#ifdef PERIGL 
static void drawpolymark3D(BCG *Xgc,double *vx, double *vy, double *vz, int n)
{
  DRAW_CHECK;
  printf("Fuck off : va falloir utiliser le billboarding pour la fct drawpolymark3D !!\n");
}
#endif 

/*
 * FIXME: a rajouter ds la table et rendre statique 
 */

#ifdef PERIGL 
void drawpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *drawvect,int n, int p)
{ 
  int symb[2],dash,color,i,close;
  /* store the current values */
  DRAW_CHECK;
  Xgc->graphic_engine->xget_mark(Xgc,symb);
  xget_dash_and_color(Xgc,&dash,&color);

  for (i=0 ; i< n ; i++)
    {
      if (drawvect[i] <= 0)
	{ /* we use the markid : drawvect[i] : with current dash **/
	  Xgc->graphic_engine->xset_mark(Xgc,- drawvect[i],symb[1]);
	  drawpolymark3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p);
	}
      else
	{/* we use the line-style number abs(drawvect[i])  **/
	  Xgc->graphic_engine->xset_line_style(Xgc,*(drawvect+i));
	  close = 0;
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,close);
	}
    }
  /* back to default values **/
  xset_dash_and_color(Xgc,dash,color);
  Xgc->graphic_engine->xset_mark(Xgc,symb[0],symb[1]);
}
#endif 

/*
 * window_list management 
 */

#ifdef PERIGTK 
int window_list_check_top(BCG *dd,void *win) 
{
  return dd->private->window == (GtkWidget *) win ;
}
#endif 

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
      gdk_pixmap_unref(winxgc->private->extra_pixmap);
      winxgc->private->extra_pixmap = NULL;
      winxgc->private->drawable = NULL; /* (GdkDrawable *)winxgc->private->drawing->window;*/
      winxgc->CurPixmapStatus = 0; 
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
  gdk_cursor_unref (winxgc->private->gcursor);
  gdk_cursor_unref (winxgc->private->ccursor);
  g_object_unref(winxgc->private->stdgc);
  g_object_unref(winxgc->private->wgc);
  g_object_unref(winxgc->private->item_factory);
  FREE(winxgc->private);
  /* remove current window from window list */
  window_list_remove(intnum);
}

/*
 * Routines for initialization : string is a display name 
 */


static void nsp_gtk_set_color_gtk(BCG *Xgc,int col)
{
  int value = AluStruc_gtk[Xgc->CurDrawFunction].id;
  GdkColor temp = {0,0,0,0};
  /* colors from 1 to Xgc->Numcolors */
  Xgc->CurColor = col = Max(0,Min(col,Xgc->Numcolors + 1));
  if (Xgc->private->colors  == NULL) return;
  temp.pixel = Xgc->private->colors[col].pixel;
  switch (value) 
    {
    case GDK_CLEAR : 
      break;
    case GDK_XOR   : 
      temp.pixel = temp.pixel ^ Xgc->private->gcol_bg.pixel ;
      gdk_gc_set_foreground(Xgc->private->wgc, &temp);
      break;
    default :
      gdk_gc_set_foreground(Xgc->private->wgc, &temp);
      break;
    }
}

#ifdef PERIGTK 
static void nsp_gtk_set_color(BCG *Xgc,int col)
{
  nsp_gtk_set_color_gtk(Xgc,col);
}
#endif 

#ifdef PERIGL
static void nsp_gtk_set_color(BCG *Xgc,int col)
{
  GdkColor c; 
  col = Max(0,Min(col,Xgc->Numcolors + 1));
  if (col == Xgc->CurColor) return;
  Xgc->CurColor=col;
  if (Xgc->private->colors  == NULL) return;
  c = Xgc->private->colors[Xgc->CurColor];
  glColor3ub( c.red >> 8 , c.green >> 8,c.blue >> 8 );
  /* also set the color for gdk */
  nsp_gtk_set_color_gtk(Xgc,col);

}
#endif 

/*
 * initgraphic : initialize graphic window
 * If v2 is not a nul pointer *v2 is the window number to create 
 * EntryCounter is used to check for first Entry + to know the next 
 * available window number 
 */


static void nsp_initgraphic(char *string,GtkWidget *win,GtkWidget *box,int *v2,
			    int *wdim,int *wpdim,double *viewport_pos,int *wpos);


static void initgraphic(char *string, int *v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos,char mode)
{ 
  nsp_initgraphic(string,NULL,NULL,v2,wdim,wpdim,viewport_pos,wpos);
}

/* used when a graphic window is to be inserted in a more complex 
 * widget hierarchy 
 */

#ifdef PERIGTK
static int EntryCounter = 0;

int nsp_graphic_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{ 
  nsp_initgraphic("",win,box,&v2,wdim,wpdim,viewport_pos,wpos);
  return  nsp_get_win_counter()-1;
}

int nsp_get_win_counter() { return EntryCounter;};
void nsp_set_win_counter(int n) {  EntryCounter=Max(EntryCounter,n); EntryCounter++;}

#else 

int nsp_graphic_new_gl(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{ 
  nsp_initgraphic("",win,box,&v2,wdim,wpdim,viewport_pos,wpos);
  return  nsp_get_win_counter()-1;
}

#endif 

static void nsp_initgraphic(char *string,GtkWidget *win,GtkWidget *box,int *v2,
			    int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  static int first = 0;
  BCG *NewXgc ;
  int WinNum = ( v2 != (int *) NULL && *v2 != -1 ) ? *v2 : nsp_get_win_counter();
  gui_private *private ; 
  if ( ( private = MALLOC(sizeof(gui_private)))== NULL) 
    {
      Sciprintf("initgraphics: running out of memory \n");
      return;
    }
  /* default values  */
  private->colors=NULL;
  private->colormap=NULL;
  private->window=NULL;		
  private->drawing=NULL;           
  private->scrolled=NULL;          
  private->CinfoW =NULL;           
  private->vbox=NULL;              
  private->menubar=NULL;
  private->item_factory=NULL;
  private->menu_entries=NULL;
  private->pixmap=NULL;       
  private->extra_pixmap=NULL;       
  private->drawable=NULL;  
  private->wgc=NULL;
  private->stdgc=NULL;
  private->gcursor=NULL;      
  private->ccursor=NULL;      
  private->font=NULL;
  private->resize = 0; /* do not remove !! */
  private->in_expose= FALSE;
#ifdef PERIGL
  private->gdk_only= FALSE;
  private->gl_only= FALSE;
#endif 
  private->protect= FALSE;
  private->draw= FALSE;

  if (( NewXgc = window_list_new(private) ) == (BCG *) 0) 
    {
      Sciprintf("initgraphics: unable to alloc\n");
      return ;
    }

  NewXgc->CurWindow = WinNum;
  NewXgc->record_flag = TRUE; /* default mode is to record plots */
  NewXgc->plots = NULL;
  NewXgc->incr_plots = NULL;
#ifdef PERIGTK 
  NewXgc->graphic_engine = &Gtk_gengine ; /* the graphic engine associated to this graphic window */
#else  
  NewXgc->graphic_engine = &GL_gengine ; /* the graphic engine associated to this graphic window */
#endif 
  start_sci_gtk(); /* be sure that gtk is started */

  if (first == 0)
    {
      maxcol = 1 << 16; /* FIXME XXXXX : to be changed */
      LoadFonts();
      first++;
    }

  if ( win != NULL )
    {
      gtk_nsp_graphic_window(FALSE,NewXgc,"unix:0",win,box,wdim,wpdim,viewport_pos,wpos);
    }
  else 
    {
      gtk_nsp_graphic_window(TRUE,NewXgc,"unix:0",NULL,NULL,wdim,wpdim,viewport_pos,wpos);
    }

  /* recheck with valgrind 
   * valgrind detecte des variables non initialisees dans 
   * initialize a cause d'initialisation crois�es 
   * d'ou des valeurs par defaut ...
   * A tester sans pour faire les choses dans l'ordre 
   * dans initialize 
   */

  NewXgc->fontId=0 ;
  NewXgc->fontSize=0 ;
  NewXgc->CurHardSymb=0;
  NewXgc->CurHardSymbSize=0;
  NewXgc->CurLineWidth=0;
  NewXgc->CurPattern=0;
  NewXgc->CurColor=0;
  NewXgc->CurPixmapStatus=0;
  NewXgc->CurVectorStyle=0;
  NewXgc->CurDrawFunction=0;
  NewXgc->ClipRegionSet=0;
  NewXgc->CurDashStyle=0;
  NewXgc->IDLastPattern=0;
  NewXgc->Numcolors=0; 
  NewXgc->NumBackground=0;
  NewXgc->NumForeground=0;
  NewXgc->NumHidden3d=0; 
  NewXgc->Autoclear=0;

  /* next values are to be set since initialize_gc 
   * action depend on the current state defined by these 
   * variables. For pixmap, resizestatus and colorstatus 
   * initialize performs a switch from old value to new value 
   */

  NewXgc->CurPixmapStatus = 0; 
  /* default colormap not instaled */
  NewXgc->CmapFlag = -1; 
  /* default resize not yet defined */
  NewXgc->CurResizeStatus = -1; /* to be sure that next will initialize */
  NewXgc->CurColorStatus = -1;  /* to be sure that next will initialize */

  NewXgc->graphic_engine->scale->initialize_gc(NewXgc);
  /* Attention ce qui est ici doit pas etre rejou� 
   * on l'enleve donc de initialize_gc
   */
  NewXgc->graphic_engine->xset_pixmapOn(NewXgc,0);
  NewXgc->graphic_engine->xset_wresize(NewXgc,1);
  /* now initialize the scale list : already performed in window_list_new */
  /* NewXgc->scales = NULL; xgc_add_default_scale(NewXgc);*/
  nsp_set_win_counter(WinNum);
  gdk_flush();
}



/*---------------------------------------------------------------------------
 * writes a message in the info widget associated to the current scilab window 
 *----------------------------------------------------------------------------*/

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


/*--------------------------------------------------------
 * Initialisation of the graphic context. Used also 
 *  to come back to the default graphic state}
 *---------------------------------------------------------*/

extern void nsp_initialize_gc( BCG *Xgc ) ;

static void xset_default(BCG *Xgc)
{
  nsp_initialize_gc(Xgc);
}

/*
 *   Draw an axis whith a slope of alpha degree (clockwise) 
 *   . Along the axis marks are set in the direction ( alpha + pi/2), in the  
 *   following way : 
 *   \begin{itemize} 
 *   \item   $n=<n1,n2>$, 
 *   \begin{verbatim} 
 *   |            |           | 
 *   |----|---|---|---|---|---| 
 *   <-----n1---->                  
 *   <-------------n2--------> 
 *   \end{verbatim} 
 *   $n1$and $n2$ are int numbers for interval numbers. 
 *   \item $size=<dl,r,coeff>$. $dl$ distance in points between  
 *   two marks, $r$ size in points of small mark, $r*coeff$  
 *   size in points of big marks. (they are doubleing points numbers) 
 *   \item $init$. Initial point $<x,y>$.  
 *   \end{itemize} 
 */

#ifdef PERIGTK 
/* must be turned into a generic function 
 */

static void drawaxis(BCG *Xgc, int alpha, int *nsteps, int *initpoint,double *size)
{
  int i;
  double xi,yi,xf,yf;
  double cosal,sinal;
  DRAW_CHECK;
  cosal= cos( (double)M_PI * (alpha)/180.0);
  sinal= sin( (double)M_PI * (alpha)/180.0);
  for (i=0; i <= nsteps[0]*nsteps[1]; i++)
    {
      if (( i % nsteps[0]) != 0)
	{
	  xi = initpoint[0]+i*size[0]*cosal;
	  yi = initpoint[1]+i*size[0]*sinal;
	  xf = xi - ( size[1]*sinal);
	  yf = yi + ( size[1]*cosal);
	  gdk_draw_line(Xgc->private->drawable,Xgc->private->wgc, xi,yi,xf,yf) ;
	}
    }
  for (i=0; i <= nsteps[1]; i++)
    { 
      xi = initpoint[0]+i*nsteps[0]*size[0]*cosal;
      yi = initpoint[1]+i*nsteps[0]*size[0]*sinal;
      xf = xi - ( size[1]*size[2]*sinal);
      yf = yi + ( size[1]*size[2]*cosal);
      gdk_draw_line(Xgc->private->drawable,Xgc->private->wgc, xi,yi,xf,yf) ;
    }
}

#else 

static void drawaxis(BCG *Xgc, int alpha, int *nsteps, int *initpoint,double *size)
{
  int i;
  double xi,yi,xf,yf;
  double cosal,sinal;
  DRAW_CHECK;
  cosal= cos( (double)M_PI * (alpha)/180.0);
  sinal= sin( (double)M_PI * (alpha)/180.0);
  for (i=0; i <= nsteps[0]*nsteps[1]; i++)
    {
      if (( i % nsteps[0]) != 0)
	{
	  xi = initpoint[0]+i*size[0]*cosal;
	  yi = initpoint[1]+i*size[0]*sinal;
	  xf = xi - ( size[1]*sinal);
	  yf = yi + ( size[1]*cosal);
	  drawline(Xgc,xi,yi,xf,yf);
	}
    }
  for (i=0; i <= nsteps[1]; i++)
    { 
      xi = initpoint[0]+i*nsteps[0]*size[0]*cosal;
      yi = initpoint[1]+i*nsteps[0]*size[0]*sinal;
      xf = xi - ( size[1]*size[2]*sinal);
      yf = yi + ( size[1]*size[2]*cosal);
      drawline(Xgc,xi,yi,xf,yf);
    }
}
#endif


/*
 * Display numbers z[i] at location (x[i],y[i])
 *   with a slope alpha[i] (see displaystring), if flag==1
 *   add a box around the string, only if slope =0}
 */

static void displaynumbers(BCG *Xgc, int *x, int *y, int n, int flag, double *z, double *alpha)
{
  int i ;
  static char buf[56];
  for (i=0 ; i< n ; i++)
    { 
      sprintf(buf,Xgc->CurNumberDispFormat,z[i]);
      displaystring(Xgc,buf,x[i],y[i],flag,alpha[i]);
    }
}

/*
 * Using gtk  Fonts
 */

#define FONTNUMBER 7 
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 10

/* FontsList : storing font informations
 *             the font i with size fsiz is stored at 
 *             FontsList_[i][fsiz]->fid
 */

GdkFont *FontsList_[FONTNUMBER][FONTMAXSIZE];

/* FontInfoTab : information on fonts 
 *  its name and ok is set to one if the font is loaded in the Xserver 
 *  loadfamily is used for font loading 
 */

struct FontInfo { int ok;
  char fname[100];
} FontInfoTab_[FONTNUMBER];

/** Must be of size FONTMAXSIZE **/

static char *size_[] = { "08" ,"10","12","14","18","24"};
/* static int i_size_[] = { 8 ,10,12,14,18,24}; */

/*
 * To set the current font id  and size 
 * load the fonts into X11 if necessary 
 */

typedef  struct  {
  char *alias;
  char *name;
}  FontAlias;

/* ce qui suit marche sur 75dpi ou 100dpi */

static FontAlias fonttab[] ={
  {"CourR", "-adobe-courier-medium-r-normal--*-%s0-*-*-m-*-iso8859-1"},
  {"Symb", "-adobe-symbol-medium-r-normal--*-%s0-*-*-p-*-adobe-fontspecific"},
  {"TimR", "-adobe-times-medium-r-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimI", "-adobe-times-medium-i-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimB", "-adobe-times-bold-r-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimBI", "-adobe-times-bold-i-normal--*-%s0-*-*-p-*-iso8859-1"},
  {(char *) NULL,( char *) NULL}
};

/* 
   static int fontidscale(BCG *Xgc,int fontsize)
   {
   int nnsiz,i;
   int isiz = i_size_[fontsize];
   double d = Min(Xgc->CWindowHeight,Xgc->CWindowWidth);
   nnsiz = (Xgc != NULL) ? inint((isiz*d/400.0)) : isiz; 
   / * fprintf(stderr,"Scaling by -->%d %d \n",isiz,nnsiz); * /
   for ( i=0; i < FONTMAXSIZE ; i++) 
   {
   if (i_size_[i] >= nnsiz ) return Max(i-1,0);
   }
   return FONTMAXSIZE -1;
   }
*/

static void xset_font(BCG *Xgc,int fontid, int fontsize)
{ 
  int i,fsiz,fsiz_sca;
  i = Min(FONTNUMBER-1,Max(fontid,0));
  fsiz = Min(FONTMAXSIZE-1,Max(fontsize,0));
  fsiz_sca = fsiz ;/* XXX fontidscale(fsiz); Scale fonts */
  if ( FontInfoTab_[i].ok !=1 ) 
    { 
      if (i != 6 )
	{
	  loadfamily(fonttab[i].alias,&i);
	}
      else 
	{
	  Sciprintf(" The Font Id %d is not affected \n",(int)i);
	  Sciprintf(" use xlfont to set it \n");
	  return;
	}
    }
  Xgc->fontId = i;
  Xgc->fontSize = fsiz;
  Xgc->private->font = FontsList_[i][fsiz_sca];
  /* 
     XSetFont(dpy,gc,FontsList_[i][fsiz_sca]->fid);
     gdk_flush();
  */
}

/** To get the  id and size of the current font **/

static void  xget_font(BCG *Xgc,int *font)
{
  font[0] = Xgc->fontId ;
  font[1] = Xgc->fontSize ;
}

/** To set the current mark **/

static void xset_mark(BCG *Xgc,int number, int size)
{ 
  Xgc->CurHardSymb = Max(Min(SYMBOLNUMBER-1,number),0);
  Xgc->CurHardSymbSize = Max(Min(FONTMAXSIZE-1,size),0);
}

/** To get the current mark id **/

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}

/* Load in X11 a font at size  08 10 12 14 18 24 
 * TimR08 TimR10 TimR12 TimR14 TimR18 TimR24 
 * name is a string if it's a string containing the char % 
 *   it's suposed to be a format for a generic font in X11 string style 
 *   ex :  "-adobe-times-bold-i-normal--%s-*-75-75-p-*-iso8859-1"
 *   and the font is loaded at size 8,10,12,14,18,24
 *   else it's supposed to be an alias for a font name
 *   Ex : TimR and we shall try to load TimR08 TimR10 TimR12 TimR14 TimR18 TimR24 
 *   we first look in an internal table and transmits the string 
 *   to X11 
 */

static void loadfamily(char *name, int *j)
{ 
  int i,flag=1 ;
  /** generic name with % **/
  if ( strchr(name,'%') != (char *) NULL)
    {
      loadfamily_n(name,j);
      return;
    }
  else 
    {
      /** our table of alias **/
      i=0;
      while ( fonttab[i].alias != (char *) NULL)
	{
	  if (strcmp(fonttab[i].alias,name)==0)
	    {
	      loadfamily_n(fonttab[i].name,j);
	      return ;
	    }
	  i++;
	}
      /** Using X11 Table of aliases **/
      for ( i = 0; i < FONTMAXSIZE ; i++)
	{
	  char name1[200];
	  sprintf(name1,"%s%s",name,size_[i]);
	  FontsList_[*j][i]=  gdk_font_load(name1);
	  if  (FontsList_[*j][i]== NULL)
	    { 
	      flag=0;
	      Sciprintf("\n Unknown font : %s",name1);
	      Sciprintf("\n I'll use font: fixed ");
	      FontsList_[*j][i]=  gdk_font_load(name1);
	      if  (FontsList_[*j][i]== NULL)
		{
		  Sciprintf("\n Unknown font : %s\n","fixed");
		  Sciprintf("Please call an X Wizard !");
		}
	    }
	}
      FontInfoTab_[*j].ok = 1;
      if (flag != 0) 
	strcpy(FontInfoTab_[*j].fname,name);
      else
	strcpy(FontInfoTab_[*j].fname,"fixed");
    }
}

static char *size_n_[] = { "8" ,"10","12","14","18","24"};

static void loadfamily_n(char *name, int *j)
{ 
  char name1[200];
  int i,flag=1 ;
  for ( i = 0; i < FONTMAXSIZE ; i++)
    {
      sprintf(name1,name,size_n_[i]);
      FontsList_[*j][i]=  gdk_font_load(name1);
      if  (FontsList_[*j][i]== NULL)
	{ 
	  flag=0;
	  Sciprintf("\n Unknown font : %s",name1);
	  Sciprintf("\n I'll use font: fixed ");
	  FontsList_[*j][i]= gdk_font_load(name1);
	  if  (FontsList_[*j][i]== NULL)
	    {
	      Sciprintf("\n Unknown font : %s\n","fixed");
	      Sciprintf("  Please call an X Wizard !");
	    }
	}
    }
  FontInfoTab_[*j].ok = 1;
  if (flag != 0) 
    strcpy(FontInfoTab_[*j].fname,name);
  else
    strcpy(FontInfoTab_[*j].fname,"fixed");
}

static void queryfamily(char *name, int *j,int *v3)
{ 
  int i ;
  name[0]='\0';
  for (i=0;i<FONTNUMBER;i++) {
    v3[i]=strlen(FontInfoTab_[i].fname);
    if (v3[i] > 0)
      strcat(name,FontInfoTab_[i].fname);
    else
      if (i < 6) {
	v3[i]=strlen(fonttab[i].name);
	strcat(name,fonttab[i].name);
      }
  }
  *j=FONTNUMBER;
}

static void LoadFonts(void)
{
  int fnum;
  loadfamily("CourR",(fnum=0,&fnum));
  LoadSymbFonts();
  loadfamily("TimR",(fnum=2,&fnum));
  /*  the next fonts are loaded when needed       See xsetfont
      loadfamily("TimI",(fnum=3,&fnum));
      loadfamily("TimB",(fnum=4,&fnum));
      loadfamily("TimBI",(fnum=5,&fnum));
  */
}

/*
 *  We use the Symbol font  for mark plotting
 *  thus we must be able to center a Symbol character at a specified point. 
 *  
 */

typedef  struct { int xoffset[SYMBOLNUMBER];
  int yoffset[SYMBOLNUMBER];} Offset ;

static Offset ListOffset_[FONTMAXSIZE];
static char Marks[] = {
  /*., +,X,*,diamond(filled),diamond,triangle up,triangle down,trefle,circle*/
  (char)0x2e,(char)0x2b,(char)0xb4,(char)0xc5,(char)0xa8,
  (char)0xe0,(char)0x44,(char)0xd1,(char)0xa7,(char)0x4f};

static void LoadSymbFonts(void)
{ 
  int j, i;
  /** Symbol Font is loaded under Id : 1 **/
  loadfamily("Symb",(i=1,&i));

  /* We compute the char offset for several chars of the symbol font
     in order to be able to center them on a specific point 
     we need one offset per symbol
     for the font i 
     n1=FontsList_[i]->min_char_or_byte2
     info on char coded as  oxyy are stored in 
     FontsList_[i]->per_char[(char)0xyy-n1]
     
  */
  /** if symbol font was not found me must stop **/
  if (strcmp(FontInfoTab_[1].fname,fonttab[1].name) != 0) return;
  for (i =0 ; i < FONTMAXSIZE ; i++)
    {    
      if (FontsList_[1][i] != NULL)
	{
	  for (j=0 ; j < SYMBOLNUMBER ; j++)
	    { 
	      gint lbearing, rbearing, iascent, idescent, iwidth;
	      gchar tmp[2] = { (gchar) Marks[j],0};
	      gdk_string_extents(FontsList_[1][i], tmp,
				 &lbearing, &rbearing,
				 &iwidth, &iascent, &idescent);
	      (ListOffset_[i].xoffset)[j] = (rbearing+lbearing)/2;/* ou iwidth/2 ? */
	      (ListOffset_[i].yoffset)[j] = (iascent+idescent)/2;
	    }
	}
    }
}

/*
 * The two next functions send the x and y offsets to center the current
 * symbol at point (x,y) 
 */

static int CurSymbXOffset(BCG *Xgc)
{
  return(-(ListOffset_[Xgc->CurHardSymbSize].xoffset)[Xgc->CurHardSymb]);
}

static int CurSymbYOffset(BCG *Xgc)
{
  return((ListOffset_[Xgc->CurHardSymbSize].yoffset)[Xgc->CurHardSymb]);
}

#ifdef PERIGTK 
static void draw_mark(BCG *Xgc,int *x, int *y)
{ 
  char str[1];
  str[0]=Marks[Xgc->CurHardSymb];
  gdk_draw_text(Xgc->private->drawable,Xgc->private->font,Xgc->private->wgc, 
		*x+CurSymbXOffset(Xgc), *y +CurSymbYOffset(Xgc),str,1);
}


#else 
/* drawing marks with pango 
 * FIXME: some operation could be stored in cache 
 * i.e utf8 translation and offsets used for drawing 
 */

/* drawing marks with pango 
 */

static const int symbols[] = 
  {
    0x22C5, /* lozenge */
    0x002B, /* plus sign */
    0x00D7, /* multiplication sign */
    0x2217, /* asterisk operator */
    0x2666, /* black diamond suit */
    0x25CA, /* lozenge */
    0x0394, /* greek capital letter delta */
    0x2207, /* nabla  */
    0x2663, /* black club suit */
    0x2295, /* circled plus */
    0x2665, /* black heart suit */
    0x2660, /* black spade suit */
    0x2297, /* circled times */
    0x2022, /* bullet */
    0x00B0  /* degree sign */
  };

static void draw_mark(BCG *Xgc,int *x, int *y)
{
  double dx,dy;
  PangoRectangle ink_rect,logical_rect;
  int code = symbols[Xgc->CurHardSymb]; 
  gchar symbol_code[4], *iter = symbol_code;
  DRAW_CHECK;
  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);
  pango_layout_set_text (Xgc->private->mark_layout,symbol_code, -1);
  pango_layout_get_extents(Xgc->private->mark_layout,&ink_rect,&logical_rect);
  dx = ink_rect.x + ink_rect.width/2.0;
  dy = ink_rect.y -logical_rect.height + ink_rect.height/2.0;
  /* gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,*x-dx,*y-dy,Xgc->private->mark_layout); */
  set_c(Xgc,1);
  glRasterPos2i(*x-PANGO_PIXELS(dx),*y + PANGO_PIXELS(-dy));
  gl_pango_ft2_render_layout (Xgc->private->mark_layout,NULL);
  if (0) 
    {
      /* draw the ink_rectangle around the mark */
      int i;
      double rect[]={ink_rect.x -dx ,ink_rect.y -logical_rect.height -dy,ink_rect.width,ink_rect.height};
      int myrect[]={*x,*y,0,0};
      for ( i=0; i < 4 ; i++) myrect[i] += PANGO_PIXELS(rect[i]);
      drawrectangle(Xgc,myrect);
    }
}
#endif

/*
 * Allocation and storing function for vectors of GtkPoints 
 */

#ifdef PERIGTK 
static GdkPoint *gtk_points = NULL;

static GdkPoint *gtk_get_xpoints(void) { return(gtk_points); }

static int gtk_store_points(int n, int *vx, int *vy, int onemore)
{ 
  int i,n1 = ( onemore == 1) ? n+1 : n;
  if (GtkReallocVector(n1) == 1)
    {
      for (i = 0; i < n; i++){
	gtk_points[i].x =(gint16) Min(Max(-int16max,vx[i]),int16max);
	gtk_points[i].y =(gint16) Min(Max(-int16max,vy[i]),int16max);
      }
      if (onemore == 1) {
	gtk_points[n].x=(gint16) gtk_points[0].x;
	gtk_points[n].y=(gint16) gtk_points[0].y;
      }
      return(1);
    }
  else return(0);
}

#define MESSAGE5 "Can't re-allocate point vector" 

static int GtkReallocVector(int n)
{
  if (( gtk_points = graphic_alloc(8,n,sizeof(GdkPoint))) == 0) 
    { 
      Sciprintf(MESSAGE5); return 0;
    }
  return 1;
}

#endif 

/*--------------------------------------------------------------------------
 * Create Graphic widget 
 *--------------------------------------------------------------------------*/

/* Infos 
 *  width = gdk_screen_width();
 *  gdk_screen_width_mm();
 *  height = gdk_screen_height();
 *  heightMM = gdk_screen_height_mm();
 *  gtk_widget_destroy(dd->private->window);
 *  gdk_private->pixmap_unref(dd->private->pixmap);
 *
 */

/* signal functions */

static gint realize_event(GtkWidget *widget, gpointer data)
{
  GdkColor white={0,0,0,0};
  GdkColor black={0,65535,65535,65535};
  BCG *dd = (BCG *) data;

  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  
  /* create gc */
  dd->private->stdgc = gdk_gc_new(dd->private->drawing->window);
  gdk_gc_set_rgb_bg_color(dd->private->stdgc,&black);
  gdk_gc_set_rgb_fg_color(dd->private->stdgc,&white);
  /* standard gc : for private->pixmap copies */
  /* this gc could be shared by all windows */
  dd->private->wgc = gdk_gc_new(dd->private->drawing->window);
  gdk_gc_set_rgb_bg_color(dd->private->wgc,&black);
  gdk_gc_set_rgb_fg_color(dd->private->wgc,&white);

  /* set the cursor */
  dd->private->gcursor = gdk_cursor_new(GDK_CROSSHAIR);
  dd->private->ccursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);
  gdk_window_set_cursor(dd->private->drawing->window, dd->private->ccursor);
  /* set window bg */
  gdk_window_set_background(dd->private->drawing->window, &dd->private->gcol_bg);

  if ( dd->private->pixmap == NULL)
    {
      dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
					   dd->CWindowWidth, dd->CWindowHeight,
					   -1);
      nsp_set_gldrawable(dd->private->pixmap);
      gdk_gc_set_foreground(dd->private->stdgc, &dd->private->gcol_bg);
      gdk_draw_rectangle(dd->private->pixmap, dd->private->stdgc, TRUE, 0, 0,
			 dd->CWindowWidth, dd->CWindowHeight);
    }

  /* default value is to use the background pixmap */
  dd->private->drawable= (GdkDrawable *) dd->private->pixmap;
#ifdef PERIGL 
  realize_event_ogl();
#endif 
  return FALSE;
}

static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data)
{
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);

  /* check for resize */
  if(GTK_WIDGET_REALIZED(dd->private->drawing))
    {
      if ( dd->CurResizeStatus == 1) 
	{
	  if ( (dd->CWindowWidth != event->width) || (dd->CWindowHeight != event->height))
	    {
	      dd->CWindowWidth = event->width;
	      dd->CWindowHeight = event->height;
	      dd->private->resize = 1;
	    }
	}
    }
  return FALSE;
}


static void nsp_gtk_invalidate(BCG *Xgc)
{

  gdk_window_invalidate_rect(Xgc->private->drawing->window,
			     &Xgc->private->drawing->allocation,
			     FALSE);
}

static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  /* {static int count = 0; xinfo(dd,"Expose event %d",count++);} */
  if(dd->private->resize != 0) 
    { 
      dd->private->resize = 0;
      if ( dd->private->pixmap)   gdk_pixmap_unref(dd->private->pixmap);
      dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
					   dd->CWindowWidth, dd->CWindowHeight,
					   -1);
      nsp_set_gldrawable(dd->private->pixmap);
      /* update drawable */
      if ( dd->CurPixmapStatus == 0 ) dd->private->drawable = dd->private->pixmap;
      /* fill private background with background */
      gdk_gc_set_background(dd->private->stdgc, &dd->private->gcol_bg);
      gdk_draw_rectangle(dd->private->pixmap,dd->private->stdgc, TRUE,0,0,
			 dd->CWindowWidth, dd->CWindowHeight);
      /* On lance l'action standard de resize + redessin  */
      /* Sync. */
#ifdef PERIGL 
      if (gdk_gl_drawable_gl_begin (gldrawable, glcontext))
	{	
	  /* Sync. with gdk  */
	  gdk_gl_drawable_wait_gdk (gldrawable);
	  /* first pass for opengl  */
	  glClear(GL_DEPTH_BUFFER_BIT);
	  nsp_ogl_set_view(dd);
	  dd->private->in_expose= TRUE;
	  dd->private->gl_only = TRUE;
	  scig_resize(dd->CurWindow);
	  dd->private->gl_only = FALSE;
	  dd->private->in_expose= FALSE;
	  glFlush ();      
	  gdk_gl_drawable_wait_gl (gldrawable);
	  /* second pass for gdk draw only (strings and marks) */
	  dd->private->in_expose= TRUE;
	  dd->private->gdk_only = TRUE;
	  scig_resize(dd->CurWindow);
	  dd->private->gdk_only = FALSE;
	  dd->private->in_expose= FALSE;
	  gdk_flush();
	  gdk_gl_drawable_wait_gdk (gldrawable);
	  gdk_gl_drawable_gl_end (gldrawable);
	}
      else
#else 
	{
	  /* gdk here */
	  dd->private->in_expose= TRUE;
	  scig_resize(dd->CurWindow);
	  dd->private->in_expose= FALSE;
	}
#endif
      /* synchronize pixmap */
      gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
		      dd->CWindowWidth, dd->CWindowHeight);
    }
  else 
    {
      if ( dd->private->draw == TRUE ) 
	{
	  /* drawing but no resizing */
#ifdef PERIGL 
	  if (gdk_gl_drawable_gl_begin (gldrawable, glcontext))
	    {
	      /* Sync. with gdk  */
	      gdk_gl_drawable_wait_gdk (gldrawable);
	      /* first pass for opengl  */
	      dd->private->draw = FALSE;
	      dd->private->in_expose= TRUE;
	      dd->private->gl_only = TRUE;
	      scig_replay(dd->CurWindow);
	      dd->private->in_expose= FALSE;
	      dd->private->gl_only = FALSE;
	      glFlush ();      
	      gdk_gl_drawable_wait_gl (gldrawable);
	      /* second pass for gdk draw only (strings and marks) */
	      dd->private->draw = FALSE;
	      dd->private->in_expose= TRUE;
	      dd->private->gdk_only = TRUE;
	      scig_resize(dd->CurWindow);
	      dd->private->gdk_only = FALSE;
	      dd->private->in_expose= FALSE;
	      gdk_flush();
	      gdk_gl_drawable_wait_gdk (gldrawable);
	      gdk_gl_drawable_gl_end (gldrawable);
	    }
	  else
#endif
	    {
	      dd->private->draw = FALSE;
	      dd->private->in_expose= TRUE;
	      scig_replay(dd->CurWindow);
	      dd->private->in_expose= FALSE;
	    }
	}
      else 
	{
#ifdef PERIGL
	  gdk_gl_drawable_wait_gl (gldrawable);
#endif
	}
      if (event  != NULL) 
	gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,
			event->area.x, event->area.y, event->area.x, event->area.y,
			event->area.width, event->area.height);
      else 
	gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
			dd->CWindowWidth, dd->CWindowHeight);
    }
  gdk_flush();
  return FALSE;
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

/*---------------------------------------------------------------
 * partial or full creation of a graphic nsp widget 
 * if is_top == FALSE a partial widget (vbox) is created 
 *---------------------------------------------------------------*/

static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  char gwin_name[100];
  gint iw, ih;
  GtkWidget *scrolled_window;
  GtkWidget *vbox;
  /* initialise pointers */
  dd->private->drawing = NULL;
  dd->private->wgc = NULL;
  dd->private->gcursor = NULL;
  dd->private->ccursor = NULL;
  gdk_rgb_init();
  gtk_widget_push_visual(gdk_rgb_get_visual());
  gtk_widget_push_colormap(gdk_rgb_get_cmap());

  /* create window etc */
  if ( wdim != NULL ) 
    {
      dd->CWindowWidth = iw = wdim[0] ; /*  / pixelWidth(); */
      dd->CWindowHeight = ih = wdim[1]; /*  pixelHeight(); */
    }
  else 
    {
      dd->CWindowWidth = iw = 600 ; /*  / pixelWidth(); */
      dd->CWindowHeight = ih = 400; /*  pixelHeight(); */
    }

  if ( is_top == TRUE ) 
    {
      dd->private->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      sprintf( gwin_name, "Graphic Window %d", dd->CurWindow );
      gtk_window_set_title (GTK_WINDOW (dd->private->window),  gwin_name);
      gtk_window_set_policy(GTK_WINDOW(dd->private->window), TRUE, TRUE, FALSE);
      gtk_widget_realize(dd->private->window);
      vbox = gtk_vbox_new (FALSE, 0);
      gtk_container_add (GTK_CONTAINER (dd->private->window), vbox);
    }
  else 
    {
      dd->private->window = win ;
      sprintf( gwin_name, "Graphic Window %d", dd->CurWindow );
      gtk_window_set_title (GTK_WINDOW (dd->private->window),  gwin_name);
      gtk_window_set_policy(GTK_WINDOW(dd->private->window), TRUE, TRUE, FALSE);
      /* gtk_widget_realize(dd->private->window);*/
      vbox = gtk_vbox_new (FALSE, 0);
      gtk_container_add (GTK_CONTAINER(box) , vbox);
    }

  /* gtk_widget_show (vbox); */

  dd->private->vbox =  gtk_vbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), dd->private->vbox, FALSE, TRUE, 0);

  dd->private->menu_entries = graphic_initial_menu(dd->CurWindow );
  dd->private->menubar = NULL;
  create_graphic_window_menu(dd);

  dd->private->CinfoW = gtk_statusbar_new ();
  gtk_box_pack_start (GTK_BOX (vbox), dd->private->CinfoW, FALSE, TRUE, 0);

  /* create a new scrolled window. */
  dd->private->scrolled = scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_set_border_width (GTK_CONTAINER (scrolled_window),0);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  /* fix min size of the scrolled window */
  if ( wpdim != NULL) 
    gtk_widget_set_size_request (scrolled_window,wpdim[0],wpdim[1]);
  else
    gtk_widget_set_size_request (scrolled_window,iw+10,ih+10);

  /* place and realize the scrolled window  */

  gtk_box_pack_start (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);

  if ( is_top == TRUE ) 
    gtk_widget_realize(scrolled_window);
  else
    gtk_widget_show(scrolled_window);

  if ( viewport_pos != NULL )
    {
      gtk_adjustment_set_value( gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (scrolled_window)),
				(gfloat) viewport_pos[0]);
      gtk_adjustment_set_value( gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (scrolled_window)),
				(gfloat) viewport_pos[1]);      
    }

  /* create private->drawingarea */

  dd->private->drawing = gtk_drawing_area_new();
  /* we use our own double buffer */
  gtk_widget_set_double_buffered (dd->private->drawing ,FALSE);

  gtk_signal_connect(GTK_OBJECT(dd->private->drawing), "button-press-event",
		     (GtkSignalFunc) locator_button_press, (gpointer) dd);
  gtk_signal_connect(GTK_OBJECT(dd->private->drawing), "button-release-event",
		     (GtkSignalFunc) locator_button_release, (gpointer) dd);
  gtk_signal_connect(GTK_OBJECT(dd->private->drawing), "motion-notify-event",
		     (GtkSignalFunc) locator_button_motion, (gpointer) dd);
  gtk_signal_connect(GTK_OBJECT(dd->private->drawing), "realize",
		     (GtkSignalFunc) realize_event, (gpointer) dd);

  gtk_widget_set_events(dd->private->drawing, GDK_EXPOSURE_MASK 
			| GDK_BUTTON_PRESS_MASK 
			| GDK_BUTTON_RELEASE_MASK
			| GDK_POINTER_MOTION_MASK
			| GDK_POINTER_MOTION_HINT_MASK
			| GDK_LEAVE_NOTIFY_MASK );

  /* private->drawingarea properties */
  /* min size of the graphic window */
  gtk_widget_set_size_request(dd->private->drawing, iw, ih);

  /* place and realize the private->drawing area */
  gtk_scrolled_window_add_with_viewport ( GTK_SCROLLED_WINDOW (scrolled_window),dd->private->drawing);

  if ( is_top == TRUE )  
    gtk_widget_realize(dd->private->drawing);
  else
    gtk_widget_show(dd->private->drawing);

  /* connect to signal handlers, etc */
  gtk_signal_connect(GTK_OBJECT(dd->private->drawing), "configure_event",
		     (GtkSignalFunc) configure_event, (gpointer) dd);

  gtk_signal_connect(GTK_OBJECT(dd->private->drawing), "expose_event",
		     (GtkSignalFunc) expose_event, (gpointer) dd);
  
  gtk_signal_connect(GTK_OBJECT(dd->private->window), "destroy",
		     (GtkSignalFunc) sci_destroy_window, (gpointer) dd);

  
  gtk_signal_connect(GTK_OBJECT(dd->private->window), "delete_event",
		     (GtkSignalFunc) sci_delete_window, (gpointer) dd);

  gtk_signal_connect (GTK_OBJECT (dd->private->window), "key_press_event",
		      (GtkSignalFunc) key_press_event, (gpointer) dd);

  /* show everything */

  if ( is_top == TRUE ) 
    {
      /* create offscreen drawable : Already done in the realize_event 
       */
      if ( wpos != NULL ) 
	gtk_window_move (GTK_WINDOW(dd->private->window),wpos[0],wpos[1]);
      gtk_widget_realize(dd->private->window);
      gtk_widget_show_all(dd->private->window);
    }
  else 
    {
      /* we need here to realize the dd->private->drawing 
       * this will create offscreen drawable : in realize_event
       * and the initialize_gc 
       */
      gtk_widget_realize(dd->private->drawing);
    }

  /* let other widgets use the default colour settings */
  gtk_widget_pop_visual();
  gtk_widget_pop_colormap();
  
}


 
/*
 *  Next routine is comming from R 
 *  ------------------------------------------------------------------
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1998-1999   Lyndon Drake
 *                            and the R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

void gdk_draw_text_rot(GdkDrawable *drawable,
		       GdkFont *font,
		       GdkGC *gc,
		       int x, int y,
		       int maxx, int maxy,
		       const gchar *text,
		       gint text_length,
		       double angle)
{
  GdkColor black, white;
  GdkPixmap *pixmap;
  GdkGC *rotgc;
  GdkImage *image;

  int lbearing, rbearing, width, ascent, descent, height;
  int dx, dy;
  int i, j, mini, minj, maxi, maxj;

  double sintheta, costheta;

  /* sanity check */
  if((text == NULL) || (*text == '\0'))
    return;

  /* shortcut horizontal text */
  if(angle == 0.0) {
    gdk_draw_text(drawable, font, gc, x, y, text, text_length);
  }
  else {
    /* text metrics */
    gdk_text_extents(font, text, text_length,
		     &lbearing, &rbearing,
		     &width, &ascent, &descent);
	
    height = ascent + descent;
	
    /* draw text into pixmap */
    pixmap = gdk_pixmap_new(drawable, width, height, 1);
    rotgc = gdk_gc_new(pixmap);
    gdk_gc_set_font(rotgc, font);

    white.pixel = gdk_rgb_xpixel_from_rgb(0xffffffff);
    black.pixel = gdk_rgb_xpixel_from_rgb(0);

    gdk_gc_set_foreground(rotgc, &white);
    gdk_draw_rectangle (pixmap, rotgc, 1, 0, 0, width, height);

    gdk_gc_set_foreground(rotgc, &black);
    gdk_draw_text(pixmap, font, rotgc, 0, ascent, text, text_length);
    image = gdk_image_get(pixmap, 0, 0, width, height); 

    /* precalc cos/sin of angle */
    /* the floor(x * 1000.0 + 0.5) / 1000.0 is a hack to round things off */
    costheta = floor(cos(angle) * 1000.0 + 0.5) / 1000.0;
    sintheta = floor(sin(angle) * 1000.0 + 0.5) / 1000.0;

    /* calculate bounding box for i and j iteration */
    mini = maxi = floor((double)(0 - ascent) * sintheta) + x;
    minj = maxj = floor((double)(0 - ascent) * costheta) + y;

    i = floor((double)width * costheta + (double)(height - ascent) * sintheta) + x;
    j = floor(- (double)width * sintheta + (double)(height - ascent) * costheta) + y;
    if(i < mini) mini = i;
    if(i > maxi) maxi = i;
    if(j < minj) minj = j;
    if(j > maxj) maxj = j;

    i = floor((double)(height - ascent) * sintheta) + x;
    j = floor((double)(height - ascent) * costheta) + y;
    if(i < mini) mini = i;
    if(i > maxi) maxi = i;
    if(j < minj) minj = j;
    if(j > maxj) maxj = j;

    i = floor((double)width * costheta + (double)(0 - ascent) * sintheta) + x;
    j = floor(- (double)width * sintheta + (double)(0 - ascent) * costheta) + y;
    if(i < mini) mini = i;
    if(i > maxi) maxi = i;
    if(j < minj) minj = j;
    if(j > maxj) maxj = j;

    maxi++; maxj++;

    if(mini < 0) mini = 0;
    /* jpc : if(maxi > maxx) maxi = maxx; */
    if(minj < 0) minj = 0;
    /* if(maxj > maxy) maxj = maxy; */

    /* copy pixels */
    for(j = minj; j < maxj; j++) {
      for(i = mini; i < maxi; i++) {
	dx = floor((double)(i - x) * costheta - (double)(j - y) * sintheta);
	dy = floor((double)(i - x) * sintheta + (double)(j - y) * costheta) + ascent;
		
	if((dx >= 0) && (dx < width) && (dy >= 0) && (dy < height) &&
	   (gdk_image_get_pixel(image, dx, dy) == black.pixel)) {
	  gdk_draw_point(drawable, gc, i, j);
	}
      }
    }

    /* clean up */
    gdk_pixmap_unref(pixmap);
    gdk_gc_unref(rotgc);
  }
}


/*
 * added for opengl capabilities 
 */


/* this is to be called when the pixmap is recreated */

static int nsp_set_gldrawable(GdkPixmap *pixmap)
{
#ifdef PERIGL 
  if (glconfig == NULL)  
    glconfig = gdk_gl_config_new_by_mode (GDK_GL_MODE_RGB    |
					  GDK_GL_MODE_DEPTH  |
					  GDK_GL_MODE_SINGLE);
  if (glconfig == NULL)  return FALSE;
  gldrawable = GDK_GL_DRAWABLE (gdk_pixmap_set_gl_capability (pixmap,
                                                              glconfig,
                                                              NULL));
  /*
   * Create OpenGL rendering context (not direct).
   */
  if (glcontext == NULL)
    {
      glcontext = gdk_gl_context_new (gldrawable,
                                      NULL,
                                      FALSE,
                                      GDK_GL_RGBA_TYPE);
      if (glcontext == NULL)
        {
          g_print ("Connot create the OpenGL rendering context\n");
	  return FALSE;
        }
      g_print ("The OpenGL rendering context is created\n");
    }
#endif
  return TRUE;
}

#ifdef TESTGL 
static void nsp_draw_gl_sphere (void)
{
  GLUquadricObj *qobj;
  static GLfloat light_diffuse[] = {1.0, 0.0, 0.0, 1.0};
  static GLfloat light_position[] = {1.0, 1.0, 1.0, 0.0};

  qobj = gluNewQuadric ();
  gluQuadricDrawStyle (qobj, GLU_FILL);

  glLightfv (GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv (GL_LIGHT0, GL_POSITION, light_position);
  glEnable (GL_LIGHTING);
  glEnable (GL_LIGHT0);
  glEnable (GL_DEPTH_TEST);

  glClearColor (1.0, 1.0, 1.0, 1.0);
  glClearDepth (1.0);

  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  gluPerspective (40.0, 1.0, 1.0, 10.0);

  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();
  gluLookAt (0.0, 0.0, 3.0,
             0.0, 0.0, 0.0,
             0.0, 1.0, 0.0);
  glTranslatef (0.0, 0.0, -3.0);
  gluSphere (qobj, 1.0, 20, 20);

}
#endif 

#ifdef TOBEUSED 
static int destroy_gl_context (gpointer data)
{
  if (glconfig != NULL)
    g_object_unref (G_OBJECT (glconfig));
  if (glcontext != NULL)
    g_object_unref (G_OBJECT (glcontext));
  return FALSE;
}
#endif 

/*
 * set opengl geometric context 
 */

#ifdef PERIGL 

void nsp_ogl_set_view(BCG *Xgc)
{
  /* xset_background(Xgc,Xgc->NumBackground+1); */
  if ( Xgc->scales->scale_flag3d == 0 ) /* XXX */
    {
      nsp_ogl_set_2dview(Xgc);
    }
  else 
    {
      nsp_ogl_set_3dview(Xgc);
    }
}


void nsp_ogl_set_2dview(BCG *Xgc)
{
  glViewport (0,  0, Xgc->private->drawing->allocation.width, 
	      Xgc->private->drawing->allocation.height);
  /* xset_background(Xgc,Xgc->NumBackground+1); */
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity ();
  gluLookAt (0,0,1,
	     0,0,-1,
	     0,1,0);
  glMatrixMode(GL_PROJECTION); 

  glLoadIdentity();
  glOrtho(0, Xgc->private->drawing->allocation.width,
	  Xgc->private->drawing->allocation.height,
	  0,-4,4);
  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_DEPTH_TEST);
}

void nsp_ogl_set_3dview(BCG *Xgc)
{
  /* double xs,ys; */
  double theta = Xgc->scales->theta;
  double alpha = Xgc->scales->alpha;
  double cost=cos((theta)*M_PI/180.0);
  double sint=sin((theta)*M_PI/180.0);
  double cosa=cos((alpha)*M_PI/180.0);
  double sina=sin((alpha)*M_PI/180.0);
  double cx= Xgc->scales->c[0];
  double cy= Xgc->scales->c[1];
  double cz= Xgc->scales->c[2];
  /* radius and center of the sphere circumscribing the box */
  double dx=Xgc->scales->bbox1[1]-Xgc->scales->bbox1[0]; 
  double dy=Xgc->scales->bbox1[3]-Xgc->scales->bbox1[2]; 
  double dz=Xgc->scales->bbox1[5]-Xgc->scales->bbox1[4];
  double R= (double) sqrt(dx*dx + dy*dy + dz*dz)/2; 

  glViewport (0,  0, Xgc->private->drawing->allocation.width, 
	      Xgc->private->drawing->allocation.height);
  /* 
   * fix the model view using the box center 
   * and a point on the sphere circumscribing the box
   * qui sont important pour l'�limination des parties cach�es
   */
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity ();
  gluLookAt (cx+R*cost*sina,
	     cy+R*sint*sina,
	     cz+R*cosa,
	     cx,cy,cz,
	     0,0,(sina >= 0.0 ) ? 1 : -1);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  /*
   * setting the modelview 
   * we use the computed min max points 
   * FIXME: when we use iso mode we have to change 
   *      the next code 
   * FIXME: ameliorer le zmin,zmax et l'utiliser pour le depth buffer 
   *      i.e donner l'info 
   */

  /* 
     xs=(Xgc->scales->frect[2]-Xgc->scales->frect[0])/
     (1 - Xgc->scales->axis[0] - Xgc->scales->axis[1]);
     ys=(Xgc->scales->frect[3]-Xgc->scales->frect[1])/
     (1 - Xgc->scales->axis[2] - Xgc->scales->axis[3]);
     glOrtho(Xgc->scales->frect[0]-xs*Xgc->scales->axis[0],
     Xgc->scales->frect[2]+xs*Xgc->scales->axis[1],
     Xgc->scales->frect[1]-ys*Xgc->scales->axis[3],
     Xgc->scales->frect[3]+ys*Xgc->scales->axis[2],
     -2*R,2*R);
  */
  glLoadIdentity();
  glOrtho(XPi2R(0),XPi2R(Xgc->scales->wdim[0]),
	  YPi2R(Xgc->scales->wdim[1]),YPi2R(0),
	  -2*R,2*R);
  glMatrixMode(GL_MODELVIEW);
  glEnable(GL_DEPTH_TEST);
}

/*
 * at the end of realize_event
 */

static void realize_event_ogl()
{
  if (!gdk_gl_drawable_gl_begin (gldrawable, glcontext)) return 
							   glClearDepth(1.0);
  glEnable(GL_DEPTH_TEST);
  /*     glDrawBuffer(GL_FRONT_AND_BACK); */
  /*     glEnable(GL_TEXTURE_2D); */
  /*     glEnable (GL_CULL_FACE); */
  /*     glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);  */
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glClearStencil(0x0);
  glEnable(GL_STENCIL_TEST);
  /* glEnable(GL_LINE_SMOOTH); */
  /* glEnable(GL_BLEND); */
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  /*     glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE); */
  /*     glLineWidth(1.5); */
  /*     glHint(GL_LINE_SMOOTH_HINT, GL_NICEST); */
  /*     glLineWidth(0.5); */
  glAlphaFunc(GL_GREATER,0.1f);
  glEnable(GL_ALPHA_TEST);
  glShadeModel(GL_SMOOTH);
  gdk_gl_drawable_gl_end (gldrawable);
}

/* 
 * Gl clipping
 */

static void clip_rectangle(BCG *Xgc, GdkRectangle clip_rect)
{
#if 0
  int bg = Xgc->NumBackground;
  glStencilFunc(GL_ALWAYS, 0x1, 0x1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
  glColor3f(Xgc->private->colors[bg].r/65535.0,
	    Xgc->private->colors[bg].g/65535.0,
	    Xgc->private->colors[bg].b/65535.0);
  glBegin(GL_QUADS);
  glVertex2i(clip_rect.x, clip_rect.y);
  glVertex2i(clip_rect.x+clip_rect.width, clip_rect.y);
  glVertex2i(clip_rect.x+clip_rect.width, clip_rect.y+clip_rect.height);
  glVertex2i(clip_rect.x, clip_rect.y+clip_rect.height);
  glEnd();
#endif
}

static void unclip_rectangle(GdkRectangle clip_rect)
{
#if 0
  glStencilFunc(GL_ALWAYS, 0x0, 0x0);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
  glBegin(GL_QUADS);
  glVertex2i(clip_rect.x, clip_rect.y);
  glVertex2i(clip_rect.x+clip_rect.width, clip_rect.y);
  glVertex2i(clip_rect.x+clip_rect.width, clip_rect.y+clip_rect.height);
  glVertex2i(clip_rect.x, clip_rect.y+clip_rect.height);
  glEnd();
#endif
}

/*
 * rendering text with PangoFT2 
 * from: font-pangoft2.c written by Naofumi Yasufuku  
 * <naofumi@users.sourceforge.net>
 * Note that if the layout uses a transformation matrix 
 * the enclosing rectangle is given by e_rect
 */

static void gl_pango_ft2_render_layout (PangoLayout *layout,      GdkRectangle *e_rect)
{
  int x,y;
  PangoRectangle logical_rect;
  FT_Bitmap bitmap;
  GLvoid *pixels;
  guint32 *p;
  GLfloat color[4];
  guint32 rgb;
  GLfloat a;
  guint8 *row, *row_end;
  int i;

  pango_layout_get_extents (layout, NULL, &logical_rect);
  if (logical_rect.width == 0 || logical_rect.height == 0)
    return;

  if ( e_rect == NULL )
    {
      bitmap.rows = PANGO_PIXELS (logical_rect.height);
      bitmap.width = PANGO_PIXELS (logical_rect.width);
      bitmap.pitch = bitmap.width;
      x= PANGO_PIXELS (-logical_rect.x);
      y= 0;
    }
  else
    {
      /* rotated string: we use the enclosing rectangle */
      bitmap.rows = e_rect->height;
      bitmap.width = e_rect->width;
      bitmap.pitch = bitmap.width;
      /* note that displaystring will pass e_rect here is such that x=y=0 */
      x = - e_rect->x;
      y = - e_rect->y ;
    }
  bitmap.buffer = g_malloc (bitmap.rows * bitmap.width);
  bitmap.num_grays = 256;
  bitmap.pixel_mode = ft_pixel_mode_grays;

  memset (bitmap.buffer, 0, bitmap.rows * bitmap.width);
  
  pango_ft2_render_layout (&bitmap, layout,x,y);

  pixels = g_malloc (bitmap.rows * bitmap.width * 4);
  p = (guint32 *) pixels;

  glGetFloatv (GL_CURRENT_COLOR, color);
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
  rgb =  ((guint32) (color[0] * 255.0))        |
        (((guint32) (color[1] * 255.0)) << 8)  |
        (((guint32) (color[2] * 255.0)) << 16);
#else
  rgb = (((guint32) (color[0] * 255.0)) << 24) |
        (((guint32) (color[1] * 255.0)) << 16) |
        (((guint32) (color[2] * 255.0)) << 8);
#endif
  a = color[3];

  row = bitmap.buffer + bitmap.rows * bitmap.width; /* past-the-end */
  row_end = bitmap.buffer;      /* beginning */

  if (a == 1.0)
    {
      do
        {
          row -= bitmap.width;
          for (i = 0; i < bitmap.width; i++)
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
            *p++ = rgb | (((guint32) row[i]) << 24);
#else
            *p++ = rgb | ((guint32) row[i]);
#endif
        }
      while (row != row_end);
    }
  else
    {
      do
        {
          row -= bitmap.width;
          for (i = 0; i < bitmap.width; i++)
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
            *p++ = rgb | (((guint32) (a * row[i])) << 24);
#else
            *p++ = rgb | ((guint32) (a * row[i]));
#endif
        }
      while (row != row_end);
    }

  glPixelStorei (GL_UNPACK_ALIGNMENT, 4);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

#if !defined(GL_VERSION_1_2)
  glDrawPixels (bitmap.width, bitmap.rows,
                GL_RGBA, GL_UNSIGNED_BYTE,
                pixels);
#else
  glDrawPixels (bitmap.width, bitmap.rows,
                GL_RGBA, GL_UNSIGNED_INT_8_8_8_8,
                pixels);
#endif

  glDisable (GL_BLEND);

  g_free (bitmap.buffer);
  g_free (pixels);
}



#endif /* PERIGL */
