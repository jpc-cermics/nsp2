/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------
 *    Gtk  Driver 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <gtk/gtk.h>

#define PERI_PRIVATE 1
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "nsp/graphics/periGtk.h"
#include "nsp/version.h"
#include "nsp/graphics/color.h"
#include "nsp/command.h"

/** Global variables to deal with X11 **/

static unsigned long maxcol; /* XXXXX : à revoir */

/*------------------------------------------------------------------
 * the current graphic data structure 
 *------------------------------------------------------------------*/

/** functions **/

static void set_c(BCG *Xgc,int col);
static void LoadFonts(void), LoadSymbFonts(void);
static void analyze_points(BCG *Xgc,int n, int *vx, int *vy, int onemore);
static void DrawMark(BCG *Xgc,int *x, int *y), My2draw(BCG *Xgc,int j, int *vx, int *vy);
static void MyDraw(BCG *Xgc,int iib, int iif, int *vx, int *vy), change_points(int i, int x, int y);
static void loadfamily_n(char *name, int *j);
static void pixmap_clear_rect   (BCG *Xgc,int x,int y,int w,int h);
static void SciClick(BCG *Xgc,int *ibutton, int *x1, int *yy1, int iflag,int getmotion, int getrelease,int getkey,char *str, int lstr);
static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp, double w, double h,GtkWidget *c,GtkWidget *d);
static void scig_deconnect_handlers(BCG *winxgc);

/* utility for points allocations */

static GdkPoint *gtk_get_xpoints(void);
static int GtkReallocVector (int n);
static int gtk_store_points (int n, int *vx,int *vy,int  onemore);

void create_graphic_window_menu( BCG *dd);
void start_sci_gtk();

/* Allocating colors in BCG struct */

#define PIXEL_FROM_RGB(r,g,b) gdk_rgb_xpixel_from_rgb((r << 16)|(g << 8)|(b))
#define PIXEL_FROM_CMAP(i) PIXEL_FROM_RGB(Xgc->private->Red[i],Xgc->private->Green[i],Xgc->private->Blue[i])

static void DispStringAngle( BCG *xgc,int x0, int yy0, char *string, double angle);

static int XgcAllocColors( BCG *xgc, int m)
{
  /* don't forget black and white */
  int mm = m + 2;
  if ( (!(xgc->private->Red = (guchar *) MALLOC(mm*sizeof(guchar))))
       || (!(xgc->private->Green = (guchar *) MALLOC(mm*sizeof(guchar))))
       || (!(xgc->private->Blue = (guchar *) MALLOC(mm*sizeof(guchar)))) ) 
    {
      Sciprintf("XgcAllocColors: unable to alloc\n");
      FREE(xgc->private->Red);
      FREE(xgc->private->Green);
      FREE(xgc->private->Blue);
      return 0;
    }
  return 1;
}

/*---------------------------------------------------------
 * Gtk graphic engine 
 * A définir sans doute ailleurs XXXXX
 *---------------------------------------------------------*/

Gengine * nsp_gengine = &Gtk_gengine ;

/*---------------------------------------------------------
 * Pixmap routines: 
 *---------------------------------------------------------*/

static void xset_pixmapclear(BCG *Xgc)
{
  pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
}

static void xset_show(BCG *Xgc)
{
  if ( Xgc->private->Cdrawable != Xgc->private->drawing->window) 
    {
      /* copy pixmap Xgc->private->Cdrawable to  Xgc->private->drawing->window
       * and expose 
       */
      gdk_gc_set_background(Xgc->private->stdgc, &Xgc->private->gcol_bg);
      /* drawing to the window and to the backing store pixmap */
      gdk_draw_pixmap(Xgc->private->drawing->window,Xgc->private->stdgc, Xgc->private->Cdrawable,0,0,
		      0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
      gdk_draw_pixmap(Xgc->private->pixmap, Xgc->private->stdgc, Xgc->private->Cdrawable,0,0,0,0,
		      Xgc->CWindowWidth, Xgc->CWindowHeight);
      /* force expose */
      /* gtk_widget_queue_draw(Xgc->private->drawing);*/
    }
}

/*
 * Pixmap clear: clear the extra private->pixmap associated to the window 
 * using the window background 
 */

static void pixmap_clear_rect(BCG *Xgc,int x, int y, int w, int h)
{
  gdk_gc_set_background(Xgc->private->stdgc, &Xgc->private->gcol_bg);
  gdk_draw_rectangle(Xgc->private->Cdrawable,Xgc->private->stdgc, TRUE,
		     0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
}

/* 
 * Resize the Pixmap according to window size change 
 * But only if there's a private->pixmap 
 */

static void pixmap_resize(BCG *Xgc)
{
  if (Xgc->private->Cdrawable != (GdkDrawable *) Xgc->private->drawing->window)
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
      gdk_pixmap_unref((GdkPixmap *) Xgc->private->Cdrawable);
      Xgc->private->Cdrawable = temp;
      pixmap_clear_rect(Xgc,0,0,x,y);
    }
} 

/*-----------------------------------------------------
 * General routines callable from Scilab 
 -----------------------------------------------------*/

/* 
 * To select (raise on the screen )the current graphic Window
 * If there's no graphic window then select creates one 
 */

void xselgraphic(BCG *Xgc)
{ 
  /* Test not really usefull: see sciwin in matdes.f */
  if ( Xgc == (BCG *)0 || Xgc->private->window ==  NULL) initgraphic("",NULL);
  gdk_window_show(Xgc->private->window->window);
  gdk_flush();
}

/** End of graphic (do nothing)  **/

void xendgraphic(void)
{
} 

void xend(BCG *Xgc)
{
  /** Must destroy everything  **/
}

/** Clear the current graphic window     **/

static void clearwindow(BCG *Xgc)
{
  /* we use the private->stdgc graphic context */
  gdk_gc_set_foreground(Xgc->private->stdgc, &Xgc->private->gcol_bg);
  gdk_draw_rectangle(Xgc->private->Cdrawable, Xgc->private->stdgc, TRUE, 0, 0,
		     Xgc->CWindowWidth, Xgc->CWindowHeight);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_rectangle(Xgc->private->pixmap, Xgc->private->stdgc, TRUE, 0, 0,
		       Xgc->CWindowWidth, Xgc->CWindowHeight);
}

/* generates a pause, in seconds */

#if defined(__STDC__) || defined(_IBMR2)
/** for usleep **/
#include <unistd.h> 
#endif 

void xpause(int sec_time)
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

/*-----------------------------------------------------------------
 * Changes the graphic window popupname 
 *-----------------------------------------------------------------*/

void Setpopupname(BCG *Xgc,char *string)
{ 
  gtk_window_set_title(GTK_WINDOW(Xgc->private->window),string);
}

/* appelle ds Xcall.c */

void setpopupname(BCG *Xgc,char *name)
{
  Setpopupname(Xgc,name);
}

/*-----------------------------------------------------------------
 * Wait for mouse click in graphic window 
 *   send back mouse location  (x1,y1)  and button number  {0,1,2}
 *   and the window number 
 *-----------------------------------------------------------------*/

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
  if ( info.sci_click_activated == FALSE ) 
    {
      PushClickQueue( gc->CurWindow,event->x, event->y,event->button-1 ,0,0);
    }
  else 
    {
      info.ok = 1; info.win=  gc->CurWindow; info.x = event->x; info.y = event->y; 
      info.button = event->button -1;
      gtk_main_quit();
    }
  return TRUE;
}

static gboolean locator_button_release(GtkWidget *widget,
				       GdkEventButton *event,
				       BCG *gc)
{
  if ( info.sci_click_activated == FALSE || info.getrelease == 0 ) 
    {
      PushClickQueue( gc->CurWindow,event->x, event->y,event->button-6 ,0,1);
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
      PushClickQueue( gc->CurWindow,x, y,-1 ,1,0);
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
    PushClickQueue( gc->CurWindow,x, y,event->keyval ,0,1);
  }

  return TRUE;
}

static int sci_graphic_protect = 0;

void   set_delete_win_mode(void) {  sci_graphic_protect = 0 ;}
extern void   set_no_delete_win_mode(void) {  sci_graphic_protect = 1 ;}

/* ici normalement on peut pas arreter la destruction */

static void sci_destroy_window (GtkWidget *widget,  BCG *gc)
{
  if (  sci_graphic_protect == 1 )
    {
      xinfo(gc,"Cannot destroy window while acquiring zoom rectangle ");
    }
  if ( info.sci_click_activated == TRUE ) 
    {
      info.ok =1 ;  info.win=  gc->CurWindow; info.x = 0 ;  info.y = 0;
      info.button = -100;
      DeleteSGWin(gc->CurWindow);
      gtk_main_quit();
    }
  else 
    DeleteSGWin(gc->CurWindow);
}

/* ici avec la valeur renvoyée on peut décider de detruire ou pas */

static gboolean sci_delete_window (GtkWidget *widget, GdkEventKey *event,  BCG *gc)
{
  if (  sci_graphic_protect == 1 )
    {
      xinfo(gc,"Cannot destroy window while acquiring zoom rectangle ");
      return TRUE;
    }
  if ( info.sci_click_activated == TRUE ) 
    {
      info.ok =1 ;  info.win=  gc->CurWindow; info.x = 0 ;  info.y = 0;
      info.button = -100;
      DeleteSGWin(gc->CurWindow);
      gtk_main_quit();
    }
  else 
    DeleteSGWin(gc->CurWindow);
  return FALSE;
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


void xclick_any(char *str, int *ibutton, int *x1,int *yy1, int *iwin, int iflag,int motion,int release,int key,int istr)
{
#ifdef WITH_TK
  guint timer_tk;
#endif 
  GTK_locator_info rec_info ; 
  int win = -1,i;
  int wincount = window_list_get_max_id()+1;
  if (wincount == 0) 
    {
      *x1=0;  *yy1=0;  *iwin=0;  *ibutton = -100;
      return; 
    }
  
  /* checks  if we already have something on the queue **/  

  if ( iflag == TRUE && CheckClickQueue(&win,x1,yy1,ibutton) == 1)  
    {
      *iwin = win ; return;
    }
  if ( iflag == FALSE )   ClearClickQueue(-1);

  /* change the cursors */ 

  for (i=0; i < wincount ; i++ ) 
    {
      BCG *bcg =  window_list_search(i);
      if ( bcg  != NULL)
	gdk_window_set_cursor(bcg->private->drawing->window,bcg->private->gcursor);
    }

  /* save info in local variable  */
  rec_info = info;
  /* set info */ 
  info.ok = 0 ; 
  info.getrelease = release ;
  info.getmotion   = motion ; 
  info.getkey     = key ; 
  info.getmen     = (istr != 0) ? TRUE : FALSE ; 
  info.sci_click_activated = TRUE;

  if ( info.getmen == TRUE  ) 
    {
      /*  Check soft menu activation during xclick_any */ 
      info.timer = gtk_timeout_add(100, (GtkFunction) timeout_test,NULL);/*  Xgc);*/
      info.str   = str;
      info.lstr  = istr; /* on entry it gives the size of str buffer */
    }

  /* take care of tck/tk */
  
#ifdef WITH_TK
  timer_tk=  gtk_timeout_add(100,  (GtkFunction) timeout_tk , NULL);
#endif


  while (1) 
    {
      /* take care of window destroy during this .....XXXXXX */
      gtk_main();
      /* be sure that gtk_main_quit was activated by proper event */
      if ( info.ok == 1 ) break;
    }
  
  *x1 = info.x; 
  *yy1 = info.y;
  *ibutton = info.button;
  *iwin = info.win;

  /* remove timer if it was set by us */ 
  if ( info.getmen == TRUE )  gtk_timeout_remove (info.timer);

#ifdef WITH_TK
  gtk_timeout_remove(timer_tk);
#endif
  /* take care of recursive calls i.e restore info  */
  info = rec_info ; 

  for (i=0; i < wincount ; i++ ) 
    {
      BCG *bcg =  window_list_search(i);
      if ( bcg  != NULL) 
	gdk_window_set_cursor(bcg->private->drawing->window,bcg->private->ccursor);
    }
}


void xclick(BCG * Xgc,char *str, int *ibutton, int *x1,int *yy1,int iflag,int motion,int release,int key, int istr)
{
  SciClick(Xgc,ibutton,x1, yy1,iflag,motion,release,key,str,istr);
}

void xgetmouse(BCG *Xgc,char *str, int *ibutton, int *x1, int *yy1, int usequeue, int motion,int release,int key)
{
  SciClick(Xgc,ibutton,x1, yy1,usequeue,motion,release,key,(char *) 0,0);
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

static void SciClick(BCG *Xgc,int *ibutton, int *x1, int *yy1, int iflag, int getmotion, int getrelease,int getkey, char *str, int lstr)
{
#ifdef WITH_TK
  guint timer_tk;
#endif 
  GTK_locator_info rec_info ; 
  int win;
  if ( Xgc == (BCG *) 0 || Xgc->private->Cdrawable == NULL ) {
    *ibutton = -100;     return;
  }
  win = Xgc->CurWindow;
  if ( iflag == TRUE && CheckClickQueue(&win,x1,yy1,ibutton) == 1) 
    { 
      /* sciprint("ds la queue %f %f \n",(double) *x1,(double) *yy1);*/ /* XXXX */
      return ;
    }
  if ( iflag == FALSE )  ClearClickQueue(Xgc->CurWindow);

  gdk_window_set_cursor (Xgc->private->drawing->window,Xgc->private->gcursor);
  
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
      if ( info.ok == 1 &&  info.win == win  ) break;
    }

#ifdef WITH_TK
  gtk_timeout_remove(timer_tk);
#endif

  *x1 = info.x;
  *yy1 = info.y;
  *ibutton = info.button;
  
  /* remove timer if it was set by us */ 
  if ( info.getmen == TRUE )  gtk_timeout_remove (info.timer);

  /* take care of recursive calls i.e restore info  */
  info = rec_info ; 

  if ( Xgc != (BCG *) 0 && Xgc->private->Cdrawable != NULL ) {
    gdk_window_set_cursor (Xgc->private->drawing->window,Xgc->private->ccursor);
  }

}

/*******************************************************
 * clear a rectangle zone 
 *******************************************************/

typedef void (*r_c) (BCG *Xgc,int x,int y,int w,int h);
static void RectangleClear   (BCG *Xgc,int x,int y,int w,int h,int clipflag,r_c f );
static void R_clear  (BCG *Xgc,int x,int y,int w,int h);

static void R_clear(BCG *Xgc,int x, int y, int w, int h)
{
  gdk_draw_rectangle(Xgc->private->Cdrawable, Xgc->private->wgc, TRUE,x,y,w,h);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_rectangle(Xgc->private->pixmap, Xgc->private->wgc, TRUE,x,y,w,h);
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
      gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
    }
  (*F)(Xgc,x,y,w,h);
  if ( cur_alu != clear )
    xset_alufunction1(Xgc,cur_alu);   /* back to current value */ 
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      /* restor clip */
      GdkRectangle clip_rect = { Xgc->CurClipRegion[0],
				 Xgc->CurClipRegion[1],
				 Xgc->CurClipRegion[2],
				 Xgc->CurClipRegion[3]};
      gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
    }
}

static void cleararea(BCG *Xgc, int x, int y, int w, int h)
{
  RectangleClear(Xgc,x,y,w,h,0,R_clear);
}



/************************************************************************
 * graphic context modifications 
 ************************************************************************/

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
  if (Xgc == NULL || Xgc->private->window ==  NULL) initgraphic("",NULL);
  gdk_window_move (Xgc->private->window->window, x,y);
}

/** To get the drawbox  window size : used by periGif **/

void getwindowdim(BCG *Xgc,int *verbose, int *x, int *narg, double *dummy)
{   
  xget_windowdim(Xgc,x,x+1);
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
 */

static void xset_windowdim(BCG *Xgc,int x, int y)
{
  /* XXXX: not so easy !!! */
  if (Xgc == NULL || Xgc->private->window ==  NULL) return ;
  gtk_widget_set_usize(Xgc->private->drawing,x,y);
  /* XXXXX gdk_window_resize(Xgc->private->drawing->window,*x,*y); */
  gdk_flush();
}

/** To get the popup  window size **/

static void xget_popupdim(BCG *Xgc,int *x, int *y)
{ 
  gint xx,yy;
  gdk_window_get_size (Xgc->private->window->window,&xx,&yy);
  *x = xx ;  *y = yy ; 
} 

/** To change the popup window size  **/

static void xset_popupdim(BCG *Xgc,int x, int y)
{
  gdk_window_resize(Xgc->private->window->window,x,y);
}

/** To get the viewport Upper/Left point Position **/

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

/** To change the window size  **/

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

/********************************************
 * select window intnum as the current window 
 * window is created if necessary 
 * return the value of the previous current window 
 ********************************************/

static int xset_curwin(int intnum,int set_menu)
{
  /* the current graphic context */
  int old;
  BCG *bcgk= window_list_get_first(),*new=NULL;
  if ( bcgk == (BCG *) 0 ) 
    {
      /** First entry or no more graphic window **/
      initgraphic("",&intnum);
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
	      initgraphic("",&intnum);
	    }
	  new = window_list_get_first();
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

/** Set a clip zone (rectangle ) **/

static void xset_clip(BCG *Xgc,int x[])
{
  int i;
  GdkRectangle clip_rect ={x[0],x[1],x[2],x[3]};
  Xgc->ClipRegionSet = 1;
  for (i=0 ; i < 4 ; i++)   Xgc->CurClipRegion[i]= x[i];
  gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
}

/** unset clip zone **/

static void xset_unclip(BCG *Xgc)
{
  static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
  Xgc->ClipRegionSet = 0;
  gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
}

/** Get the boundaries of the current clip zone **/

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

/** to set absolute or relative mode **/

static void xset_absourel(BCG *Xgc,int flag)
{
  if (flag == 0 )
    Xgc->CurVectorStyle =  CoordModeOrigin;
  else 
    Xgc->CurVectorStyle =  CoordModePrevious ;
}

/** to get information on absolute or relative mode **/

static int xget_absourel(BCG *Xgc)
{
  return  Xgc->CurVectorStyle  ;
}

/* The alu function for private->drawing : Works only with X11
 * Not in Postscript, Read The X11 manual to get more informations 
 */

static struct alinfo { 
  char *name;
  char id;
  char *info;} AluStruc_[] =
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

static void idfromname(char *name1, int *num)
{
  int i;
  *num = -1;
  for ( i =0 ; i < 16;i++)
    if (strcmp(AluStruc_[i].name,name1)== 0)  *num=i;
  if (*num == -1 ) 
    {
      Sciprintf("\n Use the following keys (int in scilab");
      for ( i=0 ; i < 16 ; i++)
	Sciprintf("\nkey %s   -> %s\n",AluStruc_[i].name,
		  AluStruc_[i].info);
    }
}

void xset_alufunction(BCG *Xgc,char *string)
{   
  int value;
  idfromname(string,&value);
  if ( value != -1)
    {
      Xgc->CurDrawFunction = value;
      gdk_gc_set_function(Xgc->private->wgc, AluStruc_[value].id);
    }
}

static void xset_alufunction1(BCG *Xgc,int num)
{   
  int value ; 
  GdkColor temp = {0,0,0,0};
  Xgc->CurDrawFunction = Min(15,Max(0,num));
  value = AluStruc_[Xgc->CurDrawFunction].id;
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
      /** the way colors are computed changes if we are in Xor mode **/
      /** so we force here the computation of current color  **/
      set_c(Xgc,Xgc->CurColor);
    }
}


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

/** To set grey level for filing areas **/
/** from black (*num =0 ) to white     **/

/* Pixmap  Tabpix_[GREYNUMBER]; */

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

/*  XXXXX 

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
      set_c(Xgc,num-1);
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

/** To get the id of the current pattern  **/

static int xget_pattern(BCG *Xgc)
{ 
  if ( Xgc->CurColorStatus == 1 ) 
    return Xgc->CurColor + 1;
  else 
    return Xgc->CurPattern + 1;
}

/** To get the id of the last pattern **/

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

static void xset_dash_or_color(BCG *Xgc,int value)
{
  if ( Xgc->CurColorStatus == 1) 
    set_c(Xgc,value-1);
  else
    xset_dash(Xgc,value);
}

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

static void xset_dashstyle(BCG *Xgc,int value, int *xx, int *n)
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


/** to get the current dash-style **/
/* old version of xget_dash retained for compatibility */

static int xget_dash_or_color(BCG *Xgc)
{
  return ( Xgc->CurColorStatus ==1) ?  Xgc->CurColor + 1 :  xget_dash(Xgc);
}

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
	  Xgc->private->Cdrawable = temp;
	  Xgc->CurPixmapStatus = 1;
	  pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
	}
    }
  else 
    {
      /** I remove the extra pixmap to the window **/
      xinfo(Xgc," ");
      gdk_pixmap_unref((GdkPixmap *) Xgc->private->Cdrawable);
      Xgc->private->Cdrawable = (GdkDrawable *)Xgc->private->drawing->window;
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
  int num1= Min(Max(num,0),1);
  if ( num1 != Xgc->CurResizeStatus && num1 == 1) 
    {
      /* we want here that the graphic window follows the viewport resize */
      /* we set the min size of graphic window to the min accepted size */
      gtk_widget_set_usize(Xgc->private->drawing,200,100);
      Xgc->CurResizeStatus = num1 ;
      return ; 
    }
  Xgc->CurResizeStatus = num1 ;
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

#define SETCOLOR(i,r,g,b)  Xgc->private->Red[i]=r;Xgc->private->Green[i]=g;Xgc->private->Blue[i]=b ; 

static void set_colormap_constants(BCG *Xgc,int m)
{
  /* Black */
  SETCOLOR(m, 0,0,0);
  /* White */
  SETCOLOR(m+1, 255,255,255);
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

static void xset_default_colormap(BCG *Xgc)
{
  int i;
  guchar *r, *g, *b;
  int m;
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
  r = Xgc->private->Red;  g = Xgc->private->Green;  b = Xgc->private->Blue;

  if (!XgcAllocColors(Xgc,m)) {
    Xgc->private->Red = r;    Xgc->private->Green = g;    Xgc->private->Blue = b;
    return;
  }
  /* Getting RGB values */
  for (i = 0; i < m; i++) {
    SETCOLOR(i, default_colors[3*i], default_colors[3*i+1], default_colors[3*i+2]);
  }

  set_colormap_constants(Xgc,m);
  FREE(r); FREE(g); FREE(b);
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
  int i;
  guchar *r, *g, *b;
  /* 2 colors reserved for black and white */
  if ( n != 3 || m  < 0 || m > maxcol - 2) {
    Scierror("Colormap must be a m x 3 array with m <= %ld\n", maxcol-2);
    return;
  }
  /* Save old color vectors */
  r = Xgc->private->Red;
  g = Xgc->private->Green;
  b = Xgc->private->Blue;

  if (!XgcAllocColors(Xgc,m)) {
    Xgc->private->Red = r;
    Xgc->private->Green = g;
    Xgc->private->Blue = b;
    return;
  }
  /* Checking RGB values */
  for (i = 0; i < m; i++) {
    if (a[i] < 0 || a[i] > 1 || a[i+m] < 0 || a[i+m] > 1 ||
	a[i+2*m] < 0 || a[i+2*m]> 1) {
      Sciprintf("RGB values must be between 0 and 1\n");
      Xgc->private->Red = r;
      Xgc->private->Green = g;
      Xgc->private->Blue = b;
      return;
    }
    SETCOLOR(i, (guchar)  (a[i]*255), (guchar)(a[i+m]*255),(guchar) (a[i+2*m]*255));
  }
  set_colormap_constants(Xgc,m);
  FREE(r); FREE(g); FREE(b);
}


/* getting the colormap */

static void xget_colormap(BCG *Xgc, int *num,  double *val)
{
  int m = Xgc->Numcolors;
  int i;
  *num = m;
  if ( val != NULL )
    {
      for (i = 0; i < m; i++) {
	val[i] = (double)Xgc->private->Red[i]/255.0;
	val[i+m] = (double)Xgc->private->Green[i]/255.0;
	val[i+2*m] = (double)Xgc->private->Blue[i]/255.0;
      }
    }
}

/** set and get the number of the background or foreground */

static void xset_background(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      int bg = Xgc->NumBackground =  Max(0,Min(num - 1,Xgc->Numcolors + 1));
      if (Xgc->private->Red != NULL )
	{
	  /* we fix the default background in Xgc->private->gcol_bg */
	  Xgc->private->gcol_bg.red = 0;
	  Xgc->private->gcol_bg.green = 0;
	  Xgc->private->gcol_bg.blue = 0;
	  Xgc->private->gcol_bg.pixel = PIXEL_FROM_CMAP(bg);
	}
      /* 
       * if we change the background of the window we must change 
       * the gc ( with alufunction ) and the window background 
       */
      xset_alufunction1(Xgc,Xgc->CurDrawFunction);
      gdk_window_set_background(Xgc->private->drawing->window, &Xgc->private->gcol_bg);
    }
}
 
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
      if (Xgc->private->Red != NULL )
	{
	  Xgc->private->gcol_fg.red = 0;
	  Xgc->private->gcol_fg.green = 0;
	  Xgc->private->gcol_fg.blue = 0;
	  Xgc->private->gcol_fg.pixel = PIXEL_FROM_CMAP(fg);
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

/** set and get the number of the hidden3d color */

static void xset_hidden3d(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      /* e Segre: Max(0,... -> Max(-1,... */
      Xgc->NumHidden3d = Max(-1,Min(num - 1,Xgc->Numcolors + 1));
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
      return  1; /** the hidden3d is a solid line style in b&w */
    }
}

/*-----------------------------------------------------------------------------
 * All the following function xxxx_1 
 * can be called using nsp_engine for a direct call 
 * or using C2F(dr1) using a name table 
 * this is usefull for replaying with the Rec driver (See Rec.c) 
 *-----------------------------------------------------------------------------*/

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


/**********************************************************
 * Used in xsetm()
 *    to see the colormap of current graphic window
 ******************************************************/

int IsPrivateCmap(void) { return 0 ;} 

void set_cmap(void * w)
{
  /* XXX
  if ( Xgc != (BCG *) 0 && Xgc->Cmap != (Colormap)0)
    XSetWindowColormap(dpy,w,Xgc->Cmap);
  */
}

int get_pixel(int i)
{
  /* XXX
  if ( Xgc != (BCG *) 0 && Xgc->Cmap != (Colormap)0)
    return(Xgc->Colors[Max(Min(i,Xgc->Numcolors + 1),0)]);
  else 
  */
  return(0);
}
/* 
Pixmap get_private->pixmap(i) 
     int i;
{
  return(Tabpix_[ Max(0,Min(i - 1,GREYNUMBER - 1))]);
}
*/

/*****************************************************
 * return 1 : if the current window exists 
 *            and its colormap is not the default 
 *            colormap (the number of colors is returned in m
 * else return 0 
 * Only used for periFig which is to be updated XXXXXX 
 *****************************************************/

int CheckColormap(BCG *Xgc,int *m)
{
  if (  Xgc != (BCG *) 0 ) 
    {
      *m =  Xgc->Numcolors;
      if ( Xgc->CmapFlag  != 1) 
	return 1;
      else 
	return 0;
    }
  else 
    { 
      *m=0;
      return(0);
    }
}

/*-----------------------------------------------------------
 * general routines accessing the previous  set<> or get<> 
 *-----------------------------------------------------------*/

/*-----------------------------------------------------------
 * Functions for private->drawing 
 *-----------------------------------------------------------*/

/**************************************************
 *  display of a string
 *  at (x,y) position whith slope angle alpha in degree . 
 * Angle are given clockwise. 
 * If *flag ==1 and angle is z\'ero a framed box is added 
 * around the string}.
 * 
 * (x,y) defines the lower left point of the bounding box 
 * of the string ( we do not separate asc and desc 
 **************************************************/

static void displaystring(BCG *Xgc,char *string, int x, int y,  int flag, double angle) 
{ 
  if ( Abs(angle) <= 0.1) 
    {
      gint lbearing, rbearing, iascent, idescent, iwidth;
      gdk_string_extents(Xgc->private->font,"X", &lbearing, &rbearing,
			 &iwidth, &iascent, &idescent);
      gdk_draw_text(Xgc->private->Cdrawable,Xgc->private->font,Xgc->private->wgc, 
		    x, y - idescent , string, strlen(string));
      if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	gdk_draw_text(Xgc->private->pixmap,Xgc->private->font,Xgc->private->wgc, 
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
      DispStringAngle(Xgc,x,y,string,angle);
    }
}

static void DispStringAngle(BCG *Xgc,int x0, int yy0, char *string, double angle)
{
  int i;
  int x,y, rect[4];
  double sina ,cosa,l;
  char str1[2];
  str1[1]='\0';
  x= x0;
  y= yy0;
  sina= sin(angle * M_PI/180.0);  cosa= cos(angle * M_PI/180.0);
  for ( i = 0 ; i < (int)strlen(string); i++)
    { 
      str1[0]=string[i];
      /* XDrawString(dpy,Xgc->private->Cdrawable,gc,(int) x,(int) y ,str1,1); */
      boundingbox(Xgc,str1,x,y,rect);
      /** drawrectangle(Xgc,string,rect,rect+1,rect+2,rect+3); **/
      if ( cosa <= 0.0 && i < (int)strlen(string)-1)
	{ char str2[2];
	/** si le cosinus est negatif le deplacement est a calculer **/
	/** sur la boite du caractere suivant **/
	str2[1]='\0';str2[0]=string[i+1];
	boundingbox(Xgc,str2,x,y,rect);
	}
      if ( Abs(cosa) >= 1.e-8 )
	{
	  if ( Abs(sina/cosa) <= Abs(((double)rect[3])/((double)rect[2])))
	    l = Abs(rect[2]/cosa);
	  else 
	    l = Abs(rect[3]/sina);
	}
      else 
	l = Abs(rect[3]/sina);
      x +=  cosa*l*1.1;
      y +=  sina*l*1.1;
    }
}

/** To get the bounding rectangle of a string **/

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

static void drawline(BCG *Xgc,int *x1, int *yy1, int *x2, int *y2)
{
  gdk_draw_line(Xgc->private->Cdrawable,Xgc->private->wgc, *x1, *yy1, *x2, *y2);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_line(Xgc->private->pixmap,Xgc->private->wgc, *x1, *yy1, *x2, *y2);
}

/** Draw a set of segments **/
/** segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) **/
/** for i=0 step 2 **/
/** n is the size of vx and vy **/

static void drawsegments(BCG *Xgc, int *vx, int *vy, int n, int *style, int iflag)
{
  int dash,color,i;
  xget_dash_and_color(Xgc,&dash,&color);
  if ( iflag == 1) { /* one style per segment */
    for (i=0 ; i < n/2 ; i++) {
      xset_line_style(Xgc,style[i]);
      gdk_draw_line(Xgc->private->Cdrawable,Xgc->private->wgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
      if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	gdk_draw_line(Xgc->private->pixmap,Xgc->private->wgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
    }
  }
  else {
    if (*style >= 1) xset_line_style(Xgc,*style);
    for (i=0 ; i < n/2 ; i++) {
      if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	gdk_draw_line(Xgc->private->pixmap,Xgc->private->wgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
      gdk_draw_line(Xgc->private->Cdrawable,Xgc->private->wgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
    }
  }
  xset_dash_and_color(Xgc,dash,color);
}

/* Draw a set of arrows 
 * arrows are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) 
 * for i=0 step 2 
 * n is the size of vx and vy 
 * as is 10*arsize (arsize) the size of the arrow head in pixels 
 */

static void drawarrows(BCG *Xgc, int *vx, int *vy, int n, int as, int *style, int iflag)
{ 
  int dash,color,i,lstyle;
  double cos20=cos(20.0*M_PI/180.0);
  double sin20=sin(20.0*M_PI/180.0);
  int polyx[4],polyy[4];
  xget_dash_and_color(Xgc,&dash,&color);
  for (i=0 ; i < n/2 ; i++)
    { 
      double dx,dy,norm;
      lstyle = (iflag == 1) ? style[i] : ( *style < 1 ) ? color : *style; 
      xset_line_style(Xgc,lstyle);
      if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	gdk_draw_line(Xgc->private->pixmap,Xgc->private->wgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
      gdk_draw_line(Xgc->private->Cdrawable,Xgc->private->wgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);

      dx=( vx[2*i+1]-vx[2*i]);
      dy=( vy[2*i+1]-vy[2*i]);
      norm = sqrt(dx*dx+dy*dy);
      if ( Abs(norm) >  SMDOUBLE ) 
	{
	  int nn=1,p=3;
	  dx=(as/10.0)*dx/norm;dy=(as/10.0)*dy/norm;
	  polyx[0]= polyx[3]=vx[2*i+1]; /* +dx*cos20;*/
	  polyx[1]= inint(polyx[0]  - cos20*dx -sin20*dy );
	  polyx[2]= inint(polyx[0]  - cos20*dx + sin20*dy);
	  polyy[0]= polyy[3]=vy[2*i+1]; /* +dy*cos20;*/
	  polyy[1]= inint(polyy[0] + sin20*dx -cos20*dy) ;
	  polyy[2]= inint(polyy[0] - sin20*dx - cos20*dy) ;
	  fillpolylines(Xgc,polyx,polyy,&lstyle,nn,p);
	}
    }
  xset_dash_and_color(Xgc,dash,color);
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

/** Draw one rectangle with current line style **/

static void drawrectangle(BCG *Xgc,const int rect[])
{ 
  gdk_draw_rectangle(Xgc->private->Cdrawable, Xgc->private->wgc, FALSE,rect[0],rect[1],rect[2],rect[3]);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_rectangle(Xgc->private->pixmap,Xgc->private->wgc, FALSE,rect[0],rect[1],rect[2],rect[3]);
}

/** fill one rectangle, with current pattern **/

static void fillrectangle(BCG *Xgc,const int rect[])
{ 
  gdk_draw_rectangle(Xgc->private->Cdrawable, Xgc->private->wgc, TRUE,rect[0],rect[1],rect[2],rect[3]);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_rectangle(Xgc->private->pixmap,Xgc->private->wgc, TRUE,rect[0],rect[1],rect[2],rect[3]);
}

/*----------------------------------------------------------------------------------
 * draw a set of rectangles, provided here to accelerate GraySquare for X11 device 
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : is the value of a function on the grid defined by x,y 
 *  on each rectangle the average value of z is computed 
 *----------------------------------------------------------------------------------*/

static void fill_grid_rectangles(BCG *Xgc,int *x, int *y, double *z, int n1, int n2)
{
  double zmoy,zmax,zmin,zmaxmin;
  int i,j,whiteid,fill[1],cpat,xz[2];
  zmin=Mini(z,(n1)*(n2));
  zmax=Maxi(z,(n1)*(n2));
  zmaxmin=zmax-zmin;
  if (zmaxmin <= SMDOUBLE) zmaxmin=SMDOUBLE;
  
  whiteid = xget_last(Xgc);
  cpat = xget_pattern(Xgc);
  xget_windowdim(Xgc,xz,xz+1);

  for (i = 0 ; i < (n1)-1 ; i++)
    for (j = 0 ; j < (n2)-1 ; j++)
      {
	int w,h;
	zmoy=1/4.0*(z[i+n1*j]+z[i+n1*(j+1)]+z[i+1+n1*j]+z[i+1+n1*(j+1)]);
	fill[0]=1 + inint((whiteid-1)*(zmoy-zmin)/(zmaxmin));
	xset_pattern(Xgc,*fill);
        w=Abs(x[i+1]-x[i]);h=Abs(y[j+1]-y[j]);
	/* We don't trace rectangle which are totally out **/
	if ( w != 0 && h != 0 && x[i] < xz[0] && y[j+1] < xz[1] && x[i]+w > 0 && y[j+1]+h > 0 )
	  if ( Abs(x[i]) < int16max && Abs(y[j+1]) < int16max && w < uns16max && h < uns16max)
	    {
	      gdk_draw_rectangle(Xgc->private->Cdrawable, Xgc->private->wgc, 
				 TRUE,x[i],y[j+1],w,h);
	      if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
		gdk_draw_rectangle(Xgc->private->pixmap,Xgc->private->wgc,
				   TRUE,x[i],y[j+1],w,h);
	    }
      }
  xset_pattern(Xgc,cpat);
}

/*----------------------------------------------------------------------------------
 * draw a set of rectangles, provided here to accelerate GraySquare1 for X11 device 
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : of size (n1-1)*(n2-1)  gives the f-values on the middle 
 *  of each rectangle. 
 *  z[i,j] is the value on the middle of rectangle 
 *        P1= x[i],y[j] x[i+1],y[j+1]
 *----------------------------------------------------------------------------------*/

static void fill_grid_rectangles1(BCG *Xgc,int *x, int *y, double *z, int n1, int n2)
{
  int i,j,fill[1],cpat,xz[2];
  cpat = xget_pattern(Xgc);
  xget_windowdim(Xgc,xz,xz+1);
  for (i = 0 ; i < (n1)-1 ; i++)
    for (j = 0 ; j < (n2)-1 ; j++)
      {
	int w,h;
	fill[0]= z[i+(n1-1)*j];
	xset_pattern(Xgc,*fill);
	w=Abs(x[j+1]-x[j]);
	h=Abs(y[i+1]-y[i]);
	/* We don't trace rectangle which are totally out **/
	if ( w != 0 && h != 0 && x[j] < xz[0] && y[i] < xz[1] && x[j]+w > 0 && y[i]+h > 0 )
	  if ( Abs(x[j]) < int16max && Abs(y[i+1]) < int16max && w < uns16max && h < uns16max)
	    {
	      gdk_draw_rectangle(Xgc->private->Cdrawable, Xgc->private->wgc, 
				 TRUE,x[j],y[i],w,h);
	      if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
		gdk_draw_rectangle(Xgc->private->pixmap,Xgc->private->wgc,
				   TRUE,x[j],y[i],w,h);
	    }
      }
  xset_pattern(Xgc,cpat);
}


/*----------------------------------------------------------------------------------
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
 *----------------------------------------------------------------------------------*/

static void fillarcs(BCG *Xgc,int *vects, int *fillvect, int n) 
{
  int i,cpat,verb;
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
  /* store the current values */
  xget_dash_and_color(Xgc,&dash,&color);
  for (i=0 ; i< n ; i++)
    {
      xset_line_style(Xgc,style[i]);
      drawarc(Xgc,vects+6*i);
    }
  xset_dash_and_color(Xgc,dash,color);
}

/** Draw a single ellipsis or part of it **/

static void drawarc(BCG *Xgc,int arc[])
{ 
  gdk_draw_arc(Xgc->private->Cdrawable, Xgc->private->wgc,FALSE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_arc(Xgc->private->pixmap,Xgc->private->wgc,FALSE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
}

/** Fill a single elipsis or part of it with current pattern **/

static void fillarc(BCG *Xgc,int arc[])
{ 
  gdk_draw_arc(Xgc->private->Cdrawable, Xgc->private->wgc,TRUE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_arc(Xgc->private->pixmap,Xgc->private->wgc,TRUE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
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

/***********************************************************
 *  fill a set of polygons each of which is defined by 
 * (*p) points (*n) is the number of polygons 
 * the polygon is closed by the routine 
 * fillvect[*n] :         
 * if fillvect[i] == 0 draw the boundaries with current color 
 * if fillvect[i] > 0  draw the boundaries with current color 
 *               then fill with pattern fillvect[i]
 * if fillvect[i] < 0  fill with pattern - fillvect[i]
 **************************************************************/

static void fillpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *fillvect,int n, int p)
{
  int dash,color,i;
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

/* 
 * Only draw one polygon  with current line style 
 * according to *closeflag : it's a polyline or a polygon
 * n is the number of points of the polyline 
 */

static void drawpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag)
{ 
  int n1;
  if (closeflag == 1) n1 =n+1;else n1= n;
  if (n1 >= 2) 
    {
      /* 
	 analyze_points(*n, vx, vy,*closeflag); 
	 gdk_flush();
      */
      if ( gtk_store_points(n, vx, vy, closeflag)) 
	{
	  gdk_draw_lines(Xgc->private->Cdrawable,Xgc->private->wgc, gtk_get_xpoints(), n1);
	  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	    gdk_draw_lines(Xgc->private->pixmap, Xgc->private->wgc, gtk_get_xpoints(), n1);
	}
    }
}

/* 
 * Fill the polygon or polyline 
 * according to *closeflag : the given vector is a polyline or a polygon 
 */

static void fillpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag) 
{
  int n1;
  if (closeflag == 1) n1 = n+1;else n1= n;
  /* 
  if (gtk_store_points(*n, vx, vy,*closeflag)){
    XFillPolygon (dpy, Xgc->private->Cdrawable, gc, get_xpoints(), n1,
		  Complex, Xgc->CurVectorStyle);
  }
  gdk_flush();
  */
  if ( gtk_store_points(n, vx, vy, closeflag)) 
    {
      gdk_draw_polygon(Xgc->private->Cdrawable,Xgc->private->wgc,TRUE,gtk_get_xpoints(), n1);
      if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	gdk_draw_polygon(Xgc->private->pixmap, Xgc->private->wgc,TRUE,gtk_get_xpoints(), n1); 
    }
}

/* 
 * Draw the current mark centred at points defined
 * by vx and vy (vx[i],vy[i]) 
 */

static void drawpolymark(BCG *Xgc,int *vx, int *vy,int n)
{
  if ( Xgc->CurHardSymb == 0 )
    {
      if (gtk_store_points(n, vx, vy,(int)0L))
	{
	  gdk_draw_points(Xgc->private->Cdrawable,
			  Xgc->private->wgc,gtk_get_xpoints(), n);
	  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	    gdk_draw_points(Xgc->private->pixmap, 
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
      for ( i=0; i< n ;i++) DrawMark(Xgc,vx+i,vy+i);
      xset_font(Xgc,keepid,keepsize);
    }
}

/*-------------------------------------------------------------------------
 * window_list management 
 *-------------------------------------------------------------------------*/

int window_list_check_top(BCG *dd,void *win) 
{
  return dd->private->window == (GtkWidget *) win ;
}


void DeleteSGWin(int intnum)
{ 
  BCG *winxgc; 
  int top_count;
  if ((winxgc = window_list_search(intnum)) == NULL) return;
  /* be sure to clear the recorded graphics */
  scig_erase(intnum);

  /* I delete the pixmap and the widget */
  if ( winxgc->CurPixmapStatus == 1 ) 
    {
      gdk_pixmap_unref(winxgc->private->Cdrawable);
      winxgc->private->Cdrawable = (GdkDrawable *)winxgc->private->drawing->window;
      winxgc->CurPixmapStatus = 0; 
    }
  /* deconnect handlers */
  scig_deconnect_handlers(winxgc);
  /* backing store private->pixmap */
  gdk_pixmap_unref(winxgc->private->pixmap);
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
  FREE(winxgc->private->Red);
  FREE(winxgc->private->Green);
  FREE(winxgc->private->Blue);
  FREE(winxgc->private);
  /* remove current window from window list */
  window_list_remove(intnum);
}



/********************************************
 * Routines for initialization : string is a display name 
 ********************************************/

static void set_c(BCG *Xgc,int col)
{
  int value = AluStruc_[Xgc->CurDrawFunction].id;
  GdkColor temp = {0,0,0,0};
  /* colors from 1 to Xgc->Numcolors */
  Xgc->CurColor = col = Max(0,Min(col,Xgc->Numcolors + 1));
  if (Xgc->private->Red  == NULL) return;
  temp.pixel = PIXEL_FROM_CMAP(col);
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

/*
 * initgraphic : initialize graphic window
 * If v2 is not a nul pointer *v2 is the window number to create 
 * EntryCounter is used to check for first Entry + to know the next 
 * available window number 
 */

static int EntryCounter = 0;
static void nsp_initgraphic(char *string,GtkWidget *win,GtkWidget *box, int *v2);

static void initgraphic(char *string, int *v2) 
{ 
  nsp_initgraphic(string,NULL,NULL,v2);
}

/* used when a graphic window is to be inserted in a more complex 
 * widget hierarchy 
 */

extern void nsp_graphic_new(GtkWidget *win,GtkWidget *box, int v2)
{ 
  nsp_initgraphic("",win,box,&v2);
}

static void nsp_initgraphic(char *string,GtkWidget *win,GtkWidget *box, int *v2)
{
  static int first = 0;
  BCG *NewXgc ;
  int WinNum = ( v2 != (int *) NULL && *v2 != -1 ) ? *v2 :  EntryCounter;
  gui_private *private ; 
  if ( ( private = MALLOC(sizeof(gui_private)))== NULL) 
    {
      Sciprintf("initgraphics: running out of memory \n");
      return;
    }
  /* default values  */
  private->Red=NULL;  
  private->Green=NULL;
  private->Blue=NULL; 
  private->window=NULL;		
  private->drawing=NULL;           
  private->scrolled=NULL;          
  private->CinfoW =NULL;           
  private->vbox=NULL;              
  private->menubar=NULL;
  private->item_factory=NULL;
  private->menu_entries=NULL;
  private->pixmap=NULL;       
  private->Cdrawable=NULL;  
  private->wgc=NULL;
  private->stdgc=NULL;
  private->gcursor=NULL;      
  private->ccursor=NULL;      
  private->font=NULL;
  private->resize = 0; /* do not remove !! */

  if (( NewXgc = window_list_new(private) ) == (BCG *) 0) 
    {
      Sciprintf("initgraphics: unable to alloc\n");
      return;
    }

  NewXgc->CurWindow = WinNum;
  NewXgc->record_flag = TRUE; /* default mode is to record plots */
  NewXgc->plots = NULL;
  NewXgc->graphic_engine = &Gtk_gengine ; /* the graphic engine associated to this graphic window */

  if (first == 0)
    {
      maxcol = 1 << 16; /* XXXXX : to be changed */
      LoadFonts();
      first++;
    }

  start_sci_gtk(); /* be sure that gtk is started */
  if ( win != NULL )
    {
      gtk_nsp_graphic_window(FALSE,NewXgc,"unix:0",600,400,win,box);
    }
  else 
    {
      gtk_nsp_graphic_window(TRUE,NewXgc,"unix:0",600,400,NULL,NULL);
    }

  /* recheck with valgrind 
   * valgrind detecte des variables non initialisees dans 
   * initialize a cause d'initialisation croisées 
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

  /* Default value is without Pixmap **/
  NewXgc->private->Cdrawable = (GdkDrawable *) NewXgc->private->drawing->window;  NewXgc->CurPixmapStatus = 0; 
  /* default colormap not instaled */
  NewXgc->CmapFlag = -1; 
  /* default resize not yet defined */
  NewXgc->CurResizeStatus = -1; /* to be sure that next will initialize */
  NewXgc->CurColorStatus = -1;  /* to be sure that next will initialize */


  NewXgc->graphic_engine->scale->initialize_gc(NewXgc);

  /* now initialize the scale list */
  NewXgc->scales = NULL;
  xgc_add_default_scale(NewXgc);
  EntryCounter=Max(EntryCounter,WinNum);
  EntryCounter++;
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


/*------------------------------------------------------
  Draw an axis whith a slope of alpha degree (clockwise)
  . Along the axis marks are set in the direction ( alpha + pi/2), in the 
  following way :
  \begin{itemize}
  \item   $n=<n1,n2>$,
  \begin{verbatim}
  |            |           |
  |----|---|---|---|---|---|
  <-----n1---->                 
  <-------------n2-------->
  \end{verbatim}
  $n1$and $n2$ are int numbers for interval numbers.
  \item $size=<dl,r,coeff>$. $dl$ distance in points between 
  two marks, $r$ size in points of small mark, $r*coeff$ 
  size in points of big marks. (they are doubleing points numbers)
  \item $init$. Initial point $<x,y>$. 
  \end{itemize}
  -------------------------------------------------------------*/

static void drawaxis(BCG *Xgc, int alpha, int *nsteps, int *initpoint,double *size)
{
  int i;
  double xi,yi,xf,yf;
  double cosal,sinal;
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
	  gdk_draw_line(Xgc->private->Cdrawable,Xgc->private->wgc, xi,yi,xf,yf) ;
	  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	    gdk_draw_line(Xgc->private->pixmap,Xgc->private->wgc, xi,yi,xf,yf);
	}
    }
  for (i=0; i <= nsteps[1]; i++)
    { 
      xi = initpoint[0]+i*nsteps[0]*size[0]*cosal;
      yi = initpoint[1]+i*nsteps[0]*size[0]*sinal;
      xf = xi - ( size[1]*size[2]*sinal);
      yf = yi + ( size[1]*size[2]*cosal);
      gdk_draw_line(Xgc->private->Cdrawable,Xgc->private->wgc, xi,yi,xf,yf) ;
      if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
	gdk_draw_line(Xgc->private->pixmap,Xgc->private->wgc, xi,yi,xf,yf);
    }
}

/*-----------------------------------------------------
 * Display numbers z[i] at location (x[i],y[i])
 *   with a slope alpha[i] (see displaystring), if flag==1
 *   add a box around the string, only if slope =0}
 *-----------------------------------------------------*/

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

/*-----------------------------------------------------
 * bitmap display 
 *-----------------------------------------------------*/

void bitmap(BCG *Xgc,char *string, int w, int h)
{
  /* 
  static XImage *setimage;
  setimage = XCreateImage (dpy, XDefaultVisual (dpy, DefaultScreen(dpy)),
			   1, XYBitmap, 0, string,w,h, 8, 0);	
  setimage->data = string;
  XPutImage (dpy, Xgc->private->Cdrawable, gc, setimage, 0, 0, 10,10,w,h);
  XDestroyImage(setimage);
  */
}


/*---------------------------------------------------------------------
 * Using X11 Fonts
 *---------------------------------------------------------------------*/

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
static int i_size_[] = { 8 ,10,12,14,18,24};

/*
 * To set the current font id  and size 
 * load the fonts into X11 if necessary 
 */

typedef  struct  {
  char *alias;
  char *name;
}  FontAlias;

/* ce qui suit marche sur 75dpi ou 100dpi */

FontAlias fonttab[] ={
  {"CourR", "-adobe-courier-medium-r-normal--*-%s0-*-*-m-*-iso8859-1"},
  {"Symb", "-adobe-symbol-medium-r-normal--*-%s0-*-*-p-*-adobe-fontspecific"},
  {"TimR", "-adobe-times-medium-r-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimI", "-adobe-times-medium-i-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimB", "-adobe-times-bold-r-normal--*-%s0-*-*-p-*-iso8859-1"},
  {"TimBI", "-adobe-times-bold-i-normal--*-%s0-*-*-p-*-iso8859-1"},
  {(char *) NULL,( char *) NULL}
};

int fontidscale(BCG *Xgc,int fontsize)
{
  int nnsiz,i;
  int isiz = i_size_[fontsize];
  double d = Min(Xgc->CWindowHeight,Xgc->CWindowWidth);
  nnsiz = (Xgc != NULL) ? inint((isiz*d/400.0)) : isiz; 
  /* fprintf(stderr,"Scaling by -->%d %d \n",isiz,nnsiz); */
  for ( i=0; i < FONTMAXSIZE ; i++) 
    {
      if (i_size_[i] >= nnsiz ) return Max(i-1,0);
    }
  return FONTMAXSIZE -1;
}

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
  font[0]= Xgc->fontId ;
  font[1] =Xgc->fontSize ;
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

static void DrawMark(BCG *Xgc,int *x, int *y)
{ 
  char str[1];
  str[0]=Marks[Xgc->CurHardSymb];
  gdk_draw_text(Xgc->private->Cdrawable,Xgc->private->font,Xgc->private->wgc, 
		*x+CurSymbXOffset(Xgc), *y +CurSymbYOffset(Xgc),str,1);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_text(Xgc->private->pixmap,Xgc->private->font,Xgc->private->wgc, 
		  *x+CurSymbXOffset(Xgc), *y +CurSymbYOffset(Xgc),str,1);
}


/*-------------------------------------------------------------------
 * Allocation and storing function for vectors of GtkPoints 
 *------------------------------------------------------------------------*/

static GdkPoint *gtk_points = NULL;

static GdkPoint *gtk_get_xpoints(void) { return(gtk_points); }

static int gtk_store_points(int n, int *vx, int *vy, int onemore)
{ 
  int i,n1 = ( onemore == 1) ? n+1 : n;
  if (GtkReallocVector(n1) == 1)
    {
      for (i = 0; i < n; i++){
	gtk_points[i].x =(gint16) Min(Max(0,vx[i]),int16max);
	gtk_points[i].y =(gint16) Min(Max(0,vy[i]),int16max);
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

/*----------------------------------------------------------
 * Clipping functions 
 *----------------------------------------------------------*/

static void XDroutine(BCG *Xgc,int npts)
{
  gdk_draw_lines(Xgc->private->Cdrawable,Xgc->private->wgc, gtk_get_xpoints(), npts);
  if ( Xgc->private->Cdrawable == Xgc->private->drawing->window) 
    gdk_draw_lines(Xgc->private->pixmap, Xgc->private->wgc, gtk_get_xpoints(), npts);
}

/* My own clipping routines  
 * XDrawlines with clipping on the current graphic window 
 * to avoid trouble on some X servers 
 */

static int xleft,xright,ybot,ytop;

/* Test a single point to be within the xleft,xright,ybot,ytop bbox.
 * Sets the returned ints 4 l.s.b. as follows:
 * bit 0 if to the left of xleft.
 * bit 1 if to the right of xright.
 * bit 2 if below of ybot.
 * bit 3 if above of ytop.
 * 0 is returned if inside.
 */

static int clip_point(int x, int y)
{
  int ret_val = 0;

  if (x < xleft) ret_val |= (char)0x01;
  else if (x > xright) ret_val |= (char)0x02;
  if (y < ybot) ret_val |= (char)0x04;
  else if (y > ytop) ret_val |= (char)0x08;
  return ret_val;
}

/* Clip the given line to private->drawing coords defined as xleft,xright,ybot,ytop.
 *   This routine uses the cohen & sutherland bit mapping for fast clipping -
 * see "Principles of Interactive Computer Graphics" Newman & Sproull page 65.
 * return 0  : segment out 
 *       1  : (x1,y1) changed 
 *	2  : (x2,y2) changed 
 *	3  : (x1,y1) and (x2,y2) changed 
 *	4  : segment in 
 */

void  set_clip_box(int xxleft, int xxright, int yybot, int yytop)
{
  xleft=xxleft;
  xright=xxright;
  ybot=yybot;
  ytop=yytop;
}

void clip_line(int x1, int yy1, int x2, int y2, int *x1n, int *yy1n, int *x2n, int *y2n, int *flag)
{
  int x, y, dx, dy, x_intr[2], y_intr[2], count, pos1, pos2;
  *x1n=x1;*yy1n=yy1;*x2n=x2;*y2n=y2;*flag=4;
  pos1 = clip_point(x1, yy1);
  pos2 = clip_point(x2, y2);
  if (pos1 || pos2) {
    if (pos1 & pos2) { *flag=0;return;}	  
    /* segment is totally out. */

    /* Here part of the segment MAy be inside. test the intersection
     * of this segment with the 4 boundaries for hopefully 2 intersections
     * in. If non found segment is totaly out.
     */
    count = 0;
    dx = x2 - x1;
    dy = y2 - yy1;

    /* Find intersections with the x parallel bbox lines: */
    if (dy != 0) {
      x = (int) (ybot - y2)  * ((double) dx / (double) dy) + x2;
      /* Test for ybot boundary. */
      if (x >= xleft && x <= xright) {
	x_intr[count] = x;
	y_intr[count++] = ybot;
      }
      x = (ytop - y2) * ((double) dx / (double) dy) + x2; 
      /* Test for ytop boundary. */
      if (x >= xleft && x <= xright) {
	x_intr[count] = x;
	y_intr[count++] = ytop;
      }
    }
    if ( count < 2 ) 
      {
	/* Find intersections with the y parallel bbox lines: */
	if (dx != 0) {
	  y = (xleft - x2) * ((double) dy / (double) dx) + y2;   
	  /* Test for xleft boundary. */
	  if (y >= ybot && y <= ytop) {
	    x_intr[count] = xleft;
	    y_intr[count++] = y;
	  }
	  if ( count < 2 ) 
	    {  
	      y = (xright - x2) * ((double) dy / (double) dx) + y2;  
	      /* Test for xright boundary. */
	      if (y >= ybot && y <= ytop) {
		x_intr[count] = xright;
		y_intr[count++] = y;
	      }
	    }
	}
      }


    if (count == 2) {
      if (pos1 && pos2) {	   /* Both were out - update both */
	*x1n = x_intr[0];
	*yy1n = y_intr[0];
	*x2n = x_intr[1];
	*y2n = y_intr[1];
	*flag=3;return;
      }
      else if (pos1) {       /* Only x1/yy1 was out - update only it */
	if (dx * (x2 - x_intr[0]) + dy * (y2 - y_intr[0]) >= 0) {
	  *x1n = x_intr[0];
	  *yy1n = y_intr[0];
	  *flag=1;return;
	}
	else {
	  *x1n = x_intr[1];
	  *yy1n = y_intr[1];
	  *flag=1;return;
	}
      }
      else {	         /* Only x2/y2 was out - update only it */
	if (dx * (x_intr[0] - x1) + dy * (y_intr[0] - yy1) >= 0) {
	  *x2n = x_intr[0];
	  *y2n = y_intr[0];
	  *flag=2;return;
	}
	else {
	  *x2n = x_intr[1];
	  *y2n = y_intr[1];
	  *flag=2;return;
	}
      }
    }
    else 
      {
	/* count != 0 */
	*flag=0;return;
      }
  }
}

static void change_points(int i, int x, int y)
{
  gtk_points[i].x=(gint16)x;   gtk_points[i].y=(gint16)y;
}

static void MyDraw(BCG *Xgc,int iib, int iif, int *vx, int *vy)
{
  int x1n,y1n,x11n,y11n,x2n,y2n,flag2=0,flag1=0;
  int npts;
  npts= ( iib > 0) ? iif-iib+2  : iif-iib+1;
  if ( iib > 0) 
    {
      clip_line(vx[iib-1],vy[iib-1],vx[iib],vy[iib],&x1n,&y1n,&x2n,&y2n,&flag1);
    }
  clip_line(vx[iif-1],vy[iif-1],vx[iif],vy[iif],&x11n,&y11n,&x2n,&y2n,&flag2);
  if (gtk_store_points(npts, &vx[Max(0,iib-1)], &vy[Max(0,iib-1)],(int)0L));
  {
    if (iib > 0 && (flag1==1||flag1==3)) change_points((int)0L,x1n,y1n);
    if (flag2==2 || flag2==3) change_points(npts-1,x2n,y2n);
    XDroutine(Xgc,(int)npts);
  }
}

static void My2draw(BCG *Xgc,int j, int *vx, int *vy)
{
  /** The segment is out but can cross the box **/
  int vxn[2],vyn[2],flag;
  int npts=2;
  clip_line(vx[j-1],vy[j-1],vx[j],vy[j],&vxn[0],&vyn[0],&vxn[1],&vyn[1],&flag);
  if (flag == 3 && gtk_store_points(npts,vxn,vyn,(int)0L))
    {
#ifdef DEBUG
      Sciprintf("segment out mais intersecte en (%d,%d),(%d,%d)\n",
	       vxn[0],vyn[0],vxn[1],vyn[1]);
#endif 
      XDroutine(Xgc,(int)npts);
    }
}

/* 
 *  returns the first (vx[.],vy[.]) point inside 
 *  xleft,xright,ybot,ytop bbox. begining at index ideb
 *  or zero if the whole polyline is out 
 */

int first_in(int n, int ideb, int *vx, int *vy)
{
  int i;
  for (i=ideb  ; i < n ; i++)
    {
      if (vx[i]>= xleft && vx[i] <= xright  && vy[i] >= ybot && vy[i] <= ytop)
	{
#ifdef DEBUG
	  Sciprintf("first in %d->%d=(%d,%d)\n",ideb,i,vx[i],vy[i]);
#endif
	  return(i);
	}
    }
  return(-1);
}

/* 
 *  returns the first (vx[.],vy[.]) point outside
 *  xleft,xright,ybot,ytop bbox.
 *  or zero if the whole polyline is out 
 */

int first_out(int n, int ideb, int *vx, int *vy)
{
  int i;
  for (i=ideb  ; i < n ; i++)
    {
      if ( vx[i]< xleft || vx[i]> xright  || vy[i] < ybot || vy[i] > ytop) 
	{
#ifdef DEBUG
	  Sciprintf("first out %d->%d=(%d,%d)\n",ideb,i,vx[i],vy[i]);
#endif
	  return(i);
	}
    }
  return(-1);
}

static void analyze_points(BCG *Xgc,int n, int *vx, int *vy, int onemore)
{ 
  int iib,iif,ideb=0,vxl[2],vyl[2], wd[2];
  xget_windowdim(Xgc,wd,wd+1);
  xleft=0;xright=wd[0]; ybot=0;ytop=wd[1];
#ifdef DEBUG1
  xleft=100;xright=300;
  ybot=100;ytop=300;
  XDrawRectangle(dpy, Xgc->private->Cdrawable, gc,xleft,ybot,(unsigned)xright-xleft,
		 (unsigned)ytop-ybot);
#endif
#ifdef DEBUG 
  Sciprintf("inside analyze\n");
#endif
  while (1) 
    { int j;
    iib=first_in(n,ideb,vx,vy);
    if (iib == -1) 
      { 
#ifdef DEBUG
	Sciprintf("[%d,end=%d] polyline out\n",(int)ideb,(int)n);
	/* all points are out but segments can cross the box */
#endif 
	for (j=ideb+1; j < n; j++) My2draw(Xgc,j,vx,vy);
	break;
      }
    else 
      if ( iib - ideb > 1) 
	{
	  /* un partie du polygine est totalement out de ideb a iib -1 */
	  /* mais peu couper la zone */
	  for (j=ideb+1; j < iib; j++) My2draw(Xgc,j,vx,vy);
	};
    iif=first_out(n,iib,vx,vy);
    if (iif == -1) {
      /* special case the polyligne is totaly inside */
      if (iib == 0) 
	{
	  if (gtk_store_points(n,vx,vy,onemore))
	    {
	      int n1 ;
	      if (onemore == 1) n1 = n+1;else n1= n;
	      XDroutine(Xgc,n1);
	      return;
	    }
	  else
	    return;
	}
      else 
	MyDraw(Xgc,iib,n-1,vx,vy);
      break;
    }
#ifdef DEBUG
    Sciprintf("Analysed : [%d,%d]\n",(int)iib,(int)iif);
#endif 
    MyDraw(Xgc,iib,iif,vx,vy);
    ideb=iif;
    }
  if (onemore == 1) {
    /* The polyligne is closed we consider the closing segment */
    int x1n,y1n,x2n,y2n,flag1=0;
    vxl[0]=vx[n-1];vxl[1]=vx[0];vyl[0]=vy[n-1];vyl[1]=vy[0];
    clip_line(vxl[0],vyl[0],vxl[1],vyl[1],&x1n,&y1n,&x2n,&y2n,&flag1);
    if ( flag1==0) return ;
    if ( gtk_store_points((int)2L,vxl,vyl,(int)0L))
      {
	if (flag1==1||flag1==3) change_points((int)0L,x1n,y1n);
	if (flag1==2||flag1==3) change_points((int)1L,x2n,y2n);
	XDroutine(Xgc,2);
      }
  }
}

int CheckXgc(BCG *Xgc)
{
  return( Xgc != (BCG *) 0);
}

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

/* a revoir XXXX */

#define R_RED(col)	(((col)	   )&255) 
#define R_GREEN(col)	(((col)>> 8)&255)
#define R_BLUE(col)	(((col)>>16)&255)

/* set the r, g, b, and pixel values of gcol to color */

static void SetRgBColor(BCG *dd,int red,int green,int blue)
{
  GdkColor gcol = { gdk_rgb_xpixel_from_rgb((red << 16)|(green << 8)|(blue)),0,0,0};
  gdk_gc_set_foreground(dd->private->wgc, &gcol);
}

static void SetColor(GdkColor *gcol, int color)
{
  int red, green, blue;
  red = R_RED(color);
  green = R_GREEN(color);
  blue = R_BLUE(color);
  gcol->red = 0;
  gcol->green = 0;
  gcol->blue = 0;
  gcol->pixel = gdk_rgb_xpixel_from_rgb((red << 16)|(green << 8)|(blue));
}


/* signal functions */

static gint realize_event(GtkWidget *widget, gpointer data)
{
  BCG *dd = (BCG *) data;

  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  
  /* create gc */
  dd->private->wgc = gdk_gc_new(dd->private->drawing->window);
  /* standard gc : for private->pixmap copies */
  /* this gc could be shared by all windows */
  dd->private->stdgc = gdk_gc_new(dd->private->drawing->window);
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
      gdk_gc_set_foreground(dd->private->stdgc, &dd->private->gcol_bg);
      gdk_draw_rectangle(dd->private->pixmap, dd->private->stdgc, TRUE, 0, 0,
			 dd->CWindowWidth, dd->CWindowHeight);
    }
  
  if (  dd->private->Cdrawable == NULL ) 
    {
      dd->private->Cdrawable= (GdkDrawable *) dd->private->drawing->window;
    }

  return FALSE;
}

static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data)
{
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);

  /* check for resize */
  if( (GTK_WIDGET_REALIZED(dd->private->drawing)) 
      && ((dd->CWindowWidth != event->width) || (dd->CWindowHeight != event->height))) 
    {
      dd->CWindowWidth = event->width;
      dd->CWindowHeight = event->height;
      dd->private->resize = 1;
    }
  return FALSE;
}

static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  /* xinfo(dd,"Expose event "); */
  if(dd->private->resize != 0) 
    { 
      dd->private->resize = 0;
      if ( dd->private->pixmap)   gdk_pixmap_unref(dd->private->pixmap);
      dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
					   dd->CWindowWidth, dd->CWindowHeight,
					   -1);

      /* fill private background with background */
      gdk_gc_set_background(dd->private->stdgc, &dd->private->gcol_bg);
      gdk_draw_rectangle(dd->private->pixmap,dd->private->stdgc, TRUE,0,0,dd->CWindowWidth, dd->CWindowHeight);
      /* On lance l'action standard de resize + redessin  */
      scig_resize(dd->CurWindow);
      gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
		      dd->CWindowWidth, dd->CWindowHeight);
    }
  else 
    {
      gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,
		      event->area.x, event->area.y, event->area.x, event->area.y,
 		      event->area.width, event->area.height);
    }
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

#define R_RGB(r,g,b)	((r)|((g)<<8)|((b)<<16))


void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp, double w, double h,GtkWidget *win,GtkWidget *box)
{
  static char gwin_name[100];
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

  dd->CWindowWidth = iw = w ; /*  / pixelWidth(); */
  dd->CWindowHeight = ih = h; /*  pixelHeight(); */

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
  gtk_widget_set_usize(scrolled_window,iw,ih);
  
  /* place and realize the scrolled window  */

  gtk_box_pack_start (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);

  if ( is_top == TRUE ) 
    gtk_widget_realize(scrolled_window);
  else
    gtk_widget_show(scrolled_window);

  /* create private->drawingarea */

  dd->private->drawing = gtk_drawing_area_new();

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
  gtk_widget_set_usize(dd->private->drawing, iw, ih);

  /* setup background color */
  dd->private->bg = R_RGB(255, 255, 255);
  SetColor(&dd->private->gcol_bg, dd->private->bg);
  
  /* setup foreground color */
  dd->private->fg =  R_RGB(0,0,0);
  SetColor(&dd->private->gcol_fg , dd->private->fg);

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

/**
 * nsp_set_graphic_eventhandler:
 * @win_num: 
 * @name: 
 * @ierr: 
 * 
 * Used to set the EventHandler field of win_num properties 
 * this is to be changed one day XXXX 
 **/

void nsp_set_graphic_eventhandler(int *win_num,char *name,int *ierr)
{  
  BCG *SciGc;
  /*ButtonPressMask|PointerMotionMask|ButtonReleaseMask|KeyPressMask */
  *ierr = 0;
  SciGc = window_list_search(*win_num);
  if ( SciGc ==  NULL ) {*ierr=1;return;}
  strncpy(SciGc->EventHandler,name,NAME_MAXL);
}


