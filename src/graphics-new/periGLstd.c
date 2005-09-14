/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------
 *    Open GL Driver 
 * FIXME: pour une �limination correcte des parties cach�es il 
 *   faut utiliser glDepthRange 
 *   pour pr�ciser la zone de la scene.
 *   etc.....
 *
 * when a graphic function is activated ex drawrectangle 
 *   the graphic is performed and an expose event is queued 
 *   (using nsp_gtk_invalidate where resize flag is set to 1 to force 
 *    a redraw when the expose is effective)
 *   Proceding this way the graphic is present on the back and front buffer 
 *   and when an expose event is activated without resize a swap_buffers is enough 
 *   to redraw the graphics without replaying the whole stuff.
 *   FIXME: il semble que parfois �a ne marche plus !!!!
 *   et je ne sais pas pourquoi.
 *   
 *   force_affichage(BCG *Xgc) is similar to nsp_gtk_invalidate but the 
 *   expose is done synchronously. 
 *   xset('wshow') when we are not using an extra_pixmap performs 
 *   a force_affichage. 
 *   This can be usefull in 
 *   for i=1:10;xclear();plot3d(x,y,z,angle=i);xset('wshow');end
 *   since if the force_affichage is not used then only the last 
 *   plot3d will be visible. 
 *   
 * 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <gtk/gtk.h>

#define PERI_PRIVATE 1
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "nsp/graphics/periGL.h"
#include "nsp/version.h"
#include "nsp/graphics/color.h"
#include "nsp/command.h"

#define FONTNUMBER 7 
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 10

/* periGL with the new DRAW_CHECK only works in recoding mode since the drawing actions 
 * are to be done in expose_event.
 */ 

#define DRAW_CHECK_XX  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 )  {  nsp_gtk_invalidate(Xgc); Xgc->private->draw = TRUE;  return; }

/* just a test to perform graphic when we are not in recording 
 * mode: this is not supposed to work since graphics are 
 * not performed in an expose_event context.But this 
 * is usefull for acquiring for ex the zoom rectangle 
 */

#define DRAW_CHECK  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 ) \
   {  nsp_gtk_invalidate(Xgc); if (Xgc->record_flag == TRUE) {  Xgc->private->draw = TRUE;  return;} }

static char Marks[] = {
  /*., +,X,*,diamond(filled),diamond,triangle up,triangle down,trefle,circle*/
  (char)0x2e,(char)0x2b,(char)0xb4,(char)0xc5,(char)0xa8,
  (char)0xe0,(char)0x44,(char)0xd1,(char)0xa7,(char)0x4f};

/* Global variables to deal with X11 **/

static unsigned long maxcol; /* XXXXX : � revoir */
static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data);
static void nsp_gtk_invalidate(BCG *Xgc);

/* 
 * OpenGL 
 */

static void force_affichage(BCG *Xgc);
void nsp_ogl_set_view(BCG *Xgc);
static bool LoadTGA(TextureImage *texture, char *filename);
static GLuint BuildFont(GLuint texID,int nb_char,int nb_ligne,int nb_col);
static void glPrint2D(BCG *Xgc, GLfloat x, GLfloat y,  GLfloat scal, GLfloat rot, bool set, const char *string, ...);
#if 0
static void glut_display_string(BCG *Xgc, GLfloat x, GLfloat y, const char *string);
#endif 
static void clip_rectangle(BCG *Xgc, GdkRectangle clip_rect);
static void unclip_rectangle(GdkRectangle clip_rect);

/*------------------------------------------------------------------
 * the current graphic data structure 
 *------------------------------------------------------------------*/

/* functions **/

static void set_c(BCG *Xgc,int col);
static void LoadFonts(BCG *Xgc);
static void DrawMark(BCG *Xgc,int *x, int *y);
static void pixmap_clear_rect   (BCG *Xgc,int x,int y,int w,int h);
static void SciClick(BCG *Xgc,int *ibutton, int *x1, int *yy1, int iflag,int getmotion, int getrelease,int getkey,char *str, int lstr);
static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos);

static void scig_deconnect_handlers(BCG *winxgc);

/* utility for points allocations */

void create_graphic_window_menu( BCG *dd);
void start_sci_gtk();

static void DispStringAngle( BCG *xgc,int x0, int yy0, char *string, double angle);
static void drawpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag);
static void fillpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag);

/* FIXME:*/
extern void change_camera(BCG *Xgc,const double *val); 


/*---------------------------------------------------------
 * Next routine are used to deal with the extra_pixmap 
 * which is used when xset('pixmap',1) is activated at 
 * scilab level. 
 * FIXME: not used for periGL 
 *        since xset('pixmap','on') is inactive 
 *---------------------------------------------------------*/

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
  if ( Xgc == (BCG *)0 || Xgc->private->window ==  NULL) 
    initgraphic("",NULL,NULL,NULL,NULL,NULL,'e');
  gdk_window_show(Xgc->private->window->window);
}

/* End of graphic (do nothing)  **/

void xendgraphic(void)
{
} 


void xend(BCG *Xgc)
{
  /* Must destroy everything  **/
  /* FIXME: a mettre ailleurs */
  glDeleteLists(Xgc->private->tab_base[0],95);
  glDeleteLists(Xgc->private->tab_base[1],95);
}

/* Clear the current graphic window 
 */

static void clearwindow(BCG *Xgc)
{
  DRAW_CHECK;
  glClearColor(Xgc->private->gcol_bg.red /255.0,
	       Xgc->private->gcol_bg.green /255.0,
	       Xgc->private->gcol_bg.blue /255.0,0.0);
  glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

/* generates a pause, in seconds */

#if defined(__STDC__) || defined(_IBMR2)
/* for usleep **/
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

/* FIXME */
extern char * nsp_string_to_utf8( char *str);

static void Setpopupname(BCG *Xgc,char *string)
{ 
  char *string_utf8=  nsp_string_to_utf8(string);
  gtk_window_set_title(GTK_WINDOW(Xgc->private->window),string_utf8);
  if ( string_utf8 != string ) g_free(string_utf8);
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

  return FALSE;
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
  if ( Xgc == (BCG *) 0 || Xgc->private->drawable == NULL ) {
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

  if ( Xgc != (BCG *) 0 && Xgc->private->drawable != NULL ) {
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
      // gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
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
      //gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
      clip_rectangle(Xgc, clip_rect);
    }
}

static void cleararea(BCG *Xgc, int x, int y, int w, int h)
{
  DRAW_CHECK;
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
/* to get the window upper-left point coordinates on the screen  **/

static void xget_windowpos(BCG *Xgc,int *x,int *y)
{
  gint xx,yy;
  gdk_window_get_position (Xgc->private->window->window,&xx,&yy);
  *x = xx; *y =yy;
}

/* to set the window upper-left point position on the screen **/

static void xset_windowpos(BCG *Xgc, int x, int y)
{
  if (Xgc == NULL || Xgc->private->window ==  NULL) initgraphic("",NULL,NULL,NULL,NULL,NULL,'e');
  gdk_window_move (Xgc->private->window->window, x,y);
}

/* To get the drawbox  window size : used by periGif **/

void getwindowdim(BCG *Xgc,int *verbose, int *x, int *narg, double *dummy)
{   
  xget_windowdim(Xgc,x,x+1);
}

/* To get the drawbox  window size **/

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


/* fixe la taille min s'un widget 
   gtk_widget_set_size_request     (GtkWidget *widget,
   gint width,
   gint height
   XXXXXXXXXXXXXXXXX
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
  gdk_flush(); //FIXME invalidate_rectangle
}

/* To get the popup  window size **/

static void xget_popupdim(BCG *Xgc,int *x, int *y)
{ 
  gint xx,yy;
  gdk_window_get_size (Xgc->private->window->window,&xx,&yy);
  *x = xx ;  *y = yy ; 
} 

/* To change the popup window size  **/

static void xset_popupdim(BCG *Xgc,int x, int y)
{
  gdk_window_resize(Xgc->private->window->window,x,y);
}

/* To get the viewport Upper/Left point Position **/

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

/* To change the window size  **/

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
      /* First entry or no more graphic window **/
      initgraphic("",&intnum,NULL,NULL,NULL,NULL,'e');
      /* send info to menu **/
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

static void xset_clip(BCG *Xgc,int x[])
{
  int i;
  GdkRectangle clip_rect ={x[0],x[1],x[2],x[3]};
  Xgc->ClipRegionSet = 1;
  for (i=0 ; i < 4 ; i++)   Xgc->CurClipRegion[i]= x[i];
  //gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
  clip_rectangle(Xgc, clip_rect);
}

/* unset clip zone **/

static void xset_unclip(BCG *Xgc)
{
  static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
  Xgc->ClipRegionSet = 0;
  //gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
  unclip_rectangle(clip_rect);
}

/* Get the boundaries of the current clip zone **/

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

/* to set absolute or relative mode **/

static void xset_absourel(BCG *Xgc,int flag)
{
  if (flag == 0 )
    Xgc->CurVectorStyle =  CoordModeOrigin;
  else 
    Xgc->CurVectorStyle =  CoordModePrevious ;
}

/* to get information on absolute or relative mode **/

static int xget_absourel(BCG *Xgc)
{
  return  Xgc->CurVectorStyle  ;
}

/* The alu function for private->drawing : Works only with X11
 * Not in Postscript, Read The X11 manual to get more informations 
 * default value is GL_COPY for which we disable Xor mode 
 */

static struct alinfo { 
  char *name;
  GLenum id;
  char *info;} AluStruc_[] =
    { 
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

/* 
   void xset_alufunction(BCG *Xgc,char *string)
   { 
   int value;
   idfromname(string,&value);
   if ( value != -1)
   {
   Xgc->CurDrawFunction = value;
   glLogicOp(AluStruc_[value].id);
   }
   }
*/

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
  value = Max(0, value);
  if ( Xgc->CurLineWidth == value ) return;
  Xgc->CurLineWidth =Max(0, value);
  /* when line thickness changes we must change the dash style */
  xset_dash(Xgc,Xgc->CurDashStyle + 1);
}

/* to get the thickness value **/

static int xget_thickness(BCG *Xgc)
{
  return Xgc->CurLineWidth ;
}

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

/* To get the id of the current pattern  **/

static int xget_pattern(BCG *Xgc)
{ 
  if ( Xgc->CurColorStatus == 1 ) 
    return Xgc->CurColor + 1;
  else 
    return Xgc->CurPattern + 1;
}

/* To get the id of the last pattern **/

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

static void xset_dashstyle(BCG *Xgc,int value, int *xx, int *n)
{
  if ( value == 0) 
    {
      /* 
	 gdk_gc_set_line_attributes(Xgc->private->wgc,
	 (Xgc->CurLineWidth <= 1) ? 0 : Xgc->CurLineWidth,
	 GDK_LINE_SOLID,GDK_CAP_BUTT, GDK_JOIN_ROUND);
      */
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
}


/* to get the current dash-style **/

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
  Sciprintf("pixmap on is not implemented in OpenGL driver\n");
  if ( Xgc->CurPixmapStatus == num1 ) return;
  if ( num1 == 1 )
    {
      GdkDrawable *temp ;
      /* create a new pixmap **/
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
	  Xgc->private->drawable = temp;
	  Xgc->CurPixmapStatus = 1;
	  pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
	}
    }
  else 
    {
      /* I remove the extra pixmap to the window **/
      xinfo(Xgc," ");
      gdk_pixmap_unref((GdkPixmap *) Xgc->private->drawable);
      Xgc->private->drawable = (GdkDrawable *)Xgc->private->drawing->window;
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

static void xget_colormap(BCG *Xgc, int *num,  double *val)
{
  int m = Xgc->Numcolors;
  int i;
  *num = m;
  if ( val != NULL )
    {
      for (i = 0; i < m; i++) {
	val[i] = (double)Xgc->private->colors[i].red/65535.0;
	val[i+m] = (double)Xgc->private->colors[i].green/65535.0;
	val[i+2*m] = (double)Xgc->private->colors[i].blue/65535.0;
      }
    }
}

/* set and get the number of the background or foreground */

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

/* set and get the number of the hidden3d color */

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
      return  1; /* the hidden3d is a solid line style in b&w */
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
  DRAW_CHECK;
  /*
   * Une police de taille 12 = une texture-fonte d'homotethie ECHELLE_CHAR (0.6) !!
   */
  if ( Abs(angle) <= 0.1) 
    {
#if 0      
      glut_display_string(Xgc, x,  y, string);
#endif
      glPrint2D(Xgc, x,y, ECHELLE_CHAR, angle, false, string);
      if ( flag == 1) 
	{
	  int rect[4];
	  boundingbox(Xgc,string, x, y, rect);
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
#if 0
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
      /* XDrawString(dpy,Xgc->private->drawable,gc,(int) x,(int) y ,str1,1); */
      boundingbox(Xgc,str1,x,y,rect);
      /* drawrectangle(Xgc,string,rect,rect+1,rect+2,rect+3); **/
      if ( cosa <= 0.0 && i < (int)strlen(string)-1)
	{ char str2[2];
	/* si le cosinus est negatif le deplacement est a calculer **/
	/* sur la boite du caractere suivant **/
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
#endif
}

/* To get the bounding rectangle of a string */

static void boundingbox(BCG *Xgc,char *string, int x, int y, int *rect)
{ 
  rect[0]= x ;
  rect[1]= y - TAILLE_CHAR * ECHELLE_CHAR;
  rect[2]= strlen(string)*TRANSLATE_CHAR * ECHELLE_CHAR;
  rect[3]= TAILLE_CHAR * ECHELLE_CHAR;
}

/*------------------------------------------------
 * line segments arrows 
 *-------------------------------------------------*/

static void drawline(BCG *Xgc,int x1, int y1, int x2, int y2)
{
  DRAW_CHECK;
  glBegin(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glEnd();
}

static void drawline3D(BCG *Xgc,double x1,double y1, double z1, double x2,double y2, double z2)
{
  DRAW_CHECK;
  glBegin(GL_LINES);
  glVertex3d(x1, y1, z1);
  glVertex3d(x2, y2, z2);
  glEnd();
}

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
	  // xset_line_style(cd,PI0,PI0,PI0);
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

/* Draw one rectangle with current line style **/

static void drawrectangle(BCG *Xgc,const int rect[])
{   
  DRAW_CHECK;
  glBegin(GL_LINE_LOOP);
  glVertex2i(rect[0]        ,rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]+rect[3]);
  glVertex2i(rect[0]        ,rect[1]+rect[3]);
  glEnd();
}

/* fill one rectangle, with current pattern **/

static void fillrectangle(BCG *Xgc,const int rect[])
{ 
  DRAW_CHECK;
  glBegin(GL_QUADS);
  glVertex2i(rect[0]        ,rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]+rect[3]);
  glVertex2i(rect[0]        ,rect[1]+rect[3]);
  glEnd();
}

/*----------------------------------------------------------------------------------
 * draw a set of rectangles, provided here to accelerate GraySquare for X11 device 
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : is the value of a function on the grid defined by x,y 
 *  on each rectangle the average value of z is computed 
 *----------------------------------------------------------------------------------*/

static  void fill_grid_rectangles(BCG *Xgc,const int x[],const int y[],const double z[], int nx, int ny,
				  int remap,const int *colminmax,const double *zminmax)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->fill_grid_rectangles(Xgc,x,y,z,nx,ny,remap,colminmax,zminmax);
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

static void fill_grid_rectangles1(BCG *Xgc,const int x[],const int y[],const double z[], int nr, int nc,
				  int remap,const int *colminmax,const double *zminmax)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->fill_grid_rectangles1(Xgc,x,y,z,nr,nc,remap,colminmax,zminmax);
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
  /* store the current values */
  DRAW_CHECK;
  xget_dash_and_color(Xgc,&dash,&color);
  for (i=0 ; i< n ; i++)
    {
      xset_line_style(Xgc,style[i]);
      drawarc(Xgc,vects+6*i);
    }
  xset_dash_and_color(Xgc,dash,color);
}

/* Draw a single ellipsis or part of it **/

static void drawarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK;
  gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,FALSE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
  if ( Xgc->private->drawable == Xgc->private->drawing->window) 
    gdk_draw_arc(Xgc->private->pixmap,Xgc->private->wgc,FALSE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
}

/* Fill a single elipsis or part of it with current pattern **/

static void fillarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK;
  gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,TRUE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
  if ( Xgc->private->drawable == Xgc->private->drawing->window) 
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
  DRAW_CHECK;
  xget_mark(Xgc,symb);
  xget_dash_and_color(Xgc,&dash,&color);


  for (i=0 ; i< n ; i++)
    {
      if (drawvect[i] <= 0)
	{ /* we use the markid : drawvect[i] : with current dash **/
	  xset_mark(Xgc,- drawvect[i],symb[1]);
	  xset_dash_and_color(Xgc,dash,color);
	  drawpolymark(Xgc,vectsx+(p)*i,vectsy+(p)*i,p);
	}
      else
	{/* we use the line-style number abs(drawvect[i])  **/
	  xset_line_style(Xgc,*(drawvect+i));
	  close = 0;
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,close);
	}
    }
  /* back to default values **/
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

/* 
 * Only draw one polygon  with current line style 
 * according to *closeflag : it's a polyline or a polygon
 * n is the number of points of the polyline 
 */

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

/* 
 * Fill the polygon or polyline 
 * according to *closeflag : the given vector is a polyline or a polygon 
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


/**
 * fillpolylines3D_shade: 
 * @Xgc: 
 * @vectsx: 
 * @vectsy: 
 * @vectsz: 
 * @fillvect: 
 * @n: 
 * @p: 
 * 
 * when we have one color for each node 
 **/


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


/**
 * fillpolylines3D:
 * @Xgc: 
 * @vectsx: 
 * @vectsy: 
 * @vectsz: 
 * @fillvect: 
 * @n: 
 * @p: 
 * 
 * the same for 3D vertices 
 * FIXME: a rajouter ds la table et rendre statique 
 **/


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

/* 
 * Draw the current mark centred at points defined
 * by vx and vy (vx[i],vy[i]) 
 */

static void drawpolymark(BCG *Xgc,int *vx, int *vy,int n)
{
  DRAW_CHECK;

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
      for ( i=0; i< n ;i++) DrawMark(Xgc,vx+i,vy+i);
      xset_font(Xgc,keepid,keepsize);
    }
}



/*
** NEW !!
*/
static void drawpolymark3D(BCG *Xgc,double *vx, double *vy, double *vz, int n)
{
  DRAW_CHECK;
  printf("Fuck off : va falloir utiliser le billboarding pour la fct drawpolymark3D !!\n");
}

/*
** FIXME: a rajouter ds la table et rendre statique 
*/

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



/*-------------------------------------------------------------------------
 * window_list management 
 *-------------------------------------------------------------------------*/

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
  /*
    if ( winxgc->CurPixmapStatus == 1 ) 
    {
    gdk_pixmap_unref(winxgc->private->drawable);
    winxgc->private->drawable = (GdkDrawable *)winxgc->private->drawing->window;
    winxgc->CurPixmapStatus = 0; 
    }
  */
  /* deconnect handlers */
  scig_deconnect_handlers(winxgc);
  /* backing store private->pixmap */
  if ( winxgc->private->pixmap != NULL)  gdk_pixmap_unref(winxgc->private->pixmap);
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

  FREE(winxgc->private);
  /* remove current window from window list */
  window_list_remove(intnum);
}



/********************************************
 * Routines for initialization : string is a display name 
 ********************************************/

static void set_c(BCG *Xgc,int col)
{
  /* int value = AluStruc_[Xgc->CurDrawFunction].id; */
  GdkColor c; 
  /* colors from 1 to Xgc->Numcolors */
  col = Max(0,Min(col,Xgc->Numcolors + 1));
  if (col == Xgc->CurColor) return;
  Xgc->CurColor=col;
  if (Xgc->private->colors  == NULL) return;
  c = Xgc->private->colors[Xgc->CurColor];
  glColor3ub( c.red >> 8 , c.green >> 8,c.blue >> 8 );

#if 0
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
#endif
}

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

int nsp_graphic_new_gl(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{ 
  nsp_initgraphic("",win,box,&v2,wdim,wpdim,viewport_pos,wpos);
  return  nsp_get_win_counter()-1;
}


/* 
 * shared between Gtk and GL 
 */

static void nsp_initgraphic(char *string,GtkWidget *win,GtkWidget *box,int *v2,
			    int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  static int first = 0;
  BCG *NewXgc ;
  /* Attention ici on peut faire deux fenetre de meme num�ro � r�gler ? XXXXX 
   */
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
  private->drawable=NULL;  
  private->wgc=NULL;
  private->stdgc=NULL;
  private->gcursor=NULL;      
  private->ccursor=NULL;      
  private->font=NULL;
  private->resize = 0; /* do not remove !! */
  private->in_expose= FALSE;
  private->protect= FALSE;
  private->draw= FALSE;

  if (( NewXgc = window_list_new(private) ) == (BCG *) 0) 
    {
      Sciprintf("initgraphics: unable to alloc\n");
      return;
    }

  {
    /* FIXME: temporary for 2d Tests */
    /* double v[]={0,0,1,0,0,0,0,1,0,-4,4,0,600,400,0};
       change_camera(NewXgc,v);
    */
  }

  NewXgc->CurWindow = WinNum;
  NewXgc->record_flag = TRUE; /* default mode is to record plots */
  NewXgc->plots = NULL;
  NewXgc->graphic_engine = &GL_gengine ; /* the graphic engine associated to this graphic window */

  if (first == 0)
    {
      maxcol = 1 << 16; /* XXXXX : to be changed */
      /* 
      ** NEW !! ATTENTION on ne peut pas appeller LoadFonts() ici parcequ'elle appelle
      ** des fcts OpenGL et OpenGL n'est pas encore initialise. confere xinit(opengl=%t)
      */
      //LoadFonts(); 
      first++;
    }

  start_sci_gtk(); /* be sure that gtk is started */
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

  /* Default value is without Pixmap **/
  NewXgc->private->drawable = (GdkDrawable *) NewXgc->private->drawing->window;  NewXgc->CurPixmapStatus = 0; 
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
  /* now initialize the scale list */
  NewXgc->scales = NULL;
  xgc_add_default_scale(NewXgc);
     
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
  //nsp_initialize_gc(Xgc);
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
  printf("fct bitmap pas encore implementee en OpenGL\n");
  /* 
     static XImage *setimage;
     setimage = XCreateImage (dpy, XDefaultVisual (dpy, DefaultScreen(dpy)),
     1, XYBitmap, 0, string,w,h, 8, 0);	
     setimage->data = string;
     XPutImage (dpy, Xgc->private->drawable, gc, setimage, 0, 0, 10,10,w,h);
     XDestroyImage(setimage);
  */
}


int fontidscale(BCG *Xgc,int fontsize)
{
  printf("fct fontidscale pas encore implementee en OpenGL\n");
  return 0;
}

static void xset_font(BCG *Xgc,int fontid, int fontsize)
{ 
  Xgc->fontId = fontid;
  Xgc->fontSize = fontsize;
}

/* To get the  id and size of the current font **/

static void  xget_font(BCG *Xgc,int *font)
{
  font[0] = Xgc->fontId ;
  font[1] = Xgc->fontSize ;
}

/* To set the current mark **/

static void xset_mark(BCG *Xgc,int number, int size)
{ 
  Xgc->CurHardSymb = Max(Min(SYMBOLNUMBER-1,number),0);
  Xgc->CurHardSymbSize = Max(Min(FONTMAXSIZE-1,size),0);
}

/* To get the current mark id **/

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

static void queryfamily(char *name, int *j,int *v3)
{ 
  printf("fct queryfamily  pas encore implementee en OpenGL\n");
}

static void loadfamily(char *name, int *j)
{ 
  printf("fct loadfamily pas encore implementee en OpenGL\n");
}

/*FIXME */
#define MAX_PATH 1024
static char font_path[MAX_PATH];

static void ogl_path_font()
{
  char *env = getenv("SCI");
  sprintf(font_path,"%s/src/graphics/Font_mini.tga",(env == NULL) ? "/fixme" : env);
}

static void LoadFonts(BCG *Xgc)
{
  /*
   * Construction de fontes avec OpenGL
   */
  static int first = 0 ;
  if (first == 0 ) 
    {
      ogl_path_font();
      first++;
    }

  if (!LoadTGA(&Xgc->private->tab_textures_font[0],font_path) ||
      !LoadTGA(&Xgc->private->tab_textures_font[1],font_path))
    {
      Sciprintf("Texture Font missing or corrupted\n");
    }
  Xgc->private->tab_base[0] = BuildFont(Xgc->private->tab_textures_font[0].texID, 95, 16, 8);
  Xgc->private->tab_base[1] = BuildFont(Xgc->private->tab_textures_font[1].texID, 95, 16, 8);
  /* base[1] = BuildFont(tab_textures_font[1].texID, 256, 16, 16); */
  Xgc->private->base_encours  = Xgc->private->tab_base[0];
  Xgc->private->fonte_encours = Xgc->private->tab_textures_font[0].texID;
}


static void DrawMark(BCG *Xgc,int *x, int *y)
{ 
  char str[2];
  DRAW_CHECK;
  str[0]=Marks[Xgc->CurHardSymb];
  str[1] = '\0';
  glPrint2D(Xgc, *x,*y, ECHELLE_CHAR, 0, false, str);
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

/*
 * FIXME : experimental for tests 
 */
#if 0 
static void init_gl_lights(GLfloat light0_pos[4])
{
  /* GLfloat light0_pos[4]   = { -50.0, 50.0, 50.0, 0.0 }; */
  GLfloat light0_color[4] = { .6, .6, .6, 1.0 }; /* white light */

  /* speedups */
  glEnable(GL_DITHER);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_FASTEST);

  /* light */
  glLightfv(GL_LIGHT0, GL_POSITION, light0_pos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE,  light0_color);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);
  /* logical Ops */
  glEnable(GL_COLOR_LOGIC_OP);
}
#endif 


/* realize handler for 
 * opengl window 
 */

static gint realize_event(GtkWidget *widget, gpointer data)
{
  BCG *Xgc = (BCG *) data;
  GdkGLContext *glcontext;
  GdkGLDrawable *gldrawable;

  g_return_val_if_fail(Xgc != NULL, FALSE);
  g_return_val_if_fail(Xgc->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(Xgc->private->drawing), FALSE);

  glcontext = gtk_widget_get_gl_context (widget);
  gldrawable = gtk_widget_get_gl_drawable (widget);

  if (!gdk_gl_drawable_gl_begin (gldrawable, glcontext))
    return FALSE;
 
#if 0   
  glClear(GL_STENCIL_BUFFER_BIT);
  glStencilFunc(GL_ALWAYS, 0x1, 0x1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
  glBegin(GL_QUADS);
  glVertex2f(0,0);
  glVertex2f(100,0);
  glVertex2f(100,100);
  glVertex2f(0,100);
  glEnd();
#endif

  xset_background(Xgc,Xgc->NumBackground+1);

  /* init_gl_lights(light0_pos); */

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
  LoadFonts(Xgc);
  
  gdk_gl_drawable_gl_end (gldrawable);
  return FALSE;
}

/*
 * 
 */

static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data)
{
  BCG *dd = (BCG *) data;
  /* GdkGLContext *glcontext = gtk_widget_get_gl_context (widget);
     GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (widget);
  */

  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);

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

  return TRUE;  
}



static void nsp_gtk_invalidate(BCG *Xgc)
{
  /* put an expose in the queue as if the window was resized */
  /* Xgc->private->resize = 1;  */
  gdk_window_invalidate_rect(Xgc->private->drawing->window,
			     &Xgc->private->drawing->allocation,
			     FALSE);
}

static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  BCG *dd = (BCG *) data;
  GdkGLContext *glcontext = gtk_widget_get_gl_context (widget);
  GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (widget);

  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  
  /* glLineWidth(1.5);  FIXME */ 

  if ( dd->private->resize != 0)  
    { 
      /* redraw after resize 
       */
      dd->private->resize = 0;
      if (!gdk_gl_drawable_gl_begin (gldrawable, glcontext)) return FALSE;
      /* glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); */
      glClear(GL_DEPTH_BUFFER_BIT);
      nsp_ogl_set_view(dd);
      dd->private->in_expose= TRUE;
      scig_resize(dd->CurWindow);
      dd->private->in_expose= FALSE;
      /* Swap buffers or flush */
      if (gdk_gl_drawable_is_double_buffered (gldrawable))
	gdk_gl_drawable_swap_buffers (gldrawable);
      else
	glFlush ();
      gdk_gl_drawable_gl_end (gldrawable);
    }
  else 
    {
      /* just an expose without resizing we need to redraw  */ 
      if (!gdk_gl_drawable_gl_begin (gldrawable, glcontext)) return FALSE;
      /* we could take care here of  dd->private->draw == TRUE 
       */
      if ( dd->record_flag == TRUE  ) 
	{
	  /* just redraw if we have recorded stuffs */
	  /* glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); */
	  glClear(GL_DEPTH_BUFFER_BIT);
	  nsp_ogl_set_view(dd);
	  dd->private->draw = FALSE;
	  /* need to redraw */
	  dd->private->in_expose= TRUE;
	  scig_replay(dd->CurWindow);
	  dd->private->in_expose= FALSE;
	  if (gdk_gl_drawable_is_double_buffered (gldrawable))
	    gdk_gl_drawable_swap_buffers (gldrawable);
	  else
	    glFlush ();
	}
      else 
	{
	  /* just try a swp buffer ? */
	  if (gdk_gl_drawable_is_double_buffered (gldrawable))
	    gdk_gl_drawable_swap_buffers (gldrawable);
	  else
	    glFlush ();
	}
      gdk_gl_drawable_gl_end (gldrawable);
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


/*
** NEW !!
*/
static void
print_gl_config_attrib (GdkGLConfig *glconfig,
                        const gchar *attrib_str,
                        int          attrib,
                        gboolean     is_boolean)
{
  int value;
     
  g_print ("%s = ", attrib_str);
  if (gdk_gl_config_get_attrib (glconfig, attrib, &value))
    {
      if (is_boolean)
	g_print ("%s\n", value == TRUE ? "TRUE" : "FALSE");
      else
	g_print ("%d\n", value);
    }
  else
    g_print ("*** Cannot get %s attribute value\n", attrib_str);
}

static void
examine_gl_config_attrib (GdkGLConfig *glconfig)
{
  g_print ("\nOpenGL visual configurations :\n\n");

  g_print ("gdk_gl_config_is_rgba (glconfig) = %s\n",
	   gdk_gl_config_is_rgba (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_is_double_buffered (glconfig) = %s\n",
	   gdk_gl_config_is_double_buffered (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_is_stereo (glconfig) = %s\n",
	   gdk_gl_config_is_stereo (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_has_alpha (glconfig) = %s\n",
	   gdk_gl_config_has_alpha (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_has_depth_buffer (glconfig) = %s\n",
	   gdk_gl_config_has_depth_buffer (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_has_stencil_buffer (glconfig) = %s\n",
	   gdk_gl_config_has_stencil_buffer (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_has_accum_buffer (glconfig) = %s\n",
	   gdk_gl_config_has_accum_buffer (glconfig) ? "TRUE" : "FALSE");
     
  g_print ("\n");
     
  print_gl_config_attrib (glconfig, "GDK_GL_USE_GL",           GDK_GL_USE_GL,           TRUE);
  print_gl_config_attrib (glconfig, "GDK_GL_BUFFER_SIZE",      GDK_GL_BUFFER_SIZE,      FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_LEVEL",            GDK_GL_LEVEL,            FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_RGBA",             GDK_GL_RGBA,             TRUE);
  print_gl_config_attrib (glconfig, "GDK_GL_DOUBLEBUFFER",     GDK_GL_DOUBLEBUFFER,     TRUE);
  print_gl_config_attrib (glconfig, "GDK_GL_STEREO",           GDK_GL_STEREO,           TRUE);
  print_gl_config_attrib (glconfig, "GDK_GL_AUX_BUFFERS",      GDK_GL_AUX_BUFFERS,      FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_RED_SIZE",         GDK_GL_RED_SIZE,         FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_GREEN_SIZE",       GDK_GL_GREEN_SIZE,       FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_BLUE_SIZE",        GDK_GL_BLUE_SIZE,        FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ALPHA_SIZE",       GDK_GL_ALPHA_SIZE,       FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_DEPTH_SIZE",       GDK_GL_DEPTH_SIZE,       FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_STENCIL_SIZE",     GDK_GL_STENCIL_SIZE,     FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ACCUM_RED_SIZE",   GDK_GL_ACCUM_RED_SIZE,   FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ACCUM_GREEN_SIZE", GDK_GL_ACCUM_GREEN_SIZE, FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ACCUM_BLUE_SIZE",  GDK_GL_ACCUM_BLUE_SIZE,  FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ACCUM_ALPHA_SIZE", GDK_GL_ACCUM_ALPHA_SIZE, FALSE);
     
  g_print ("\n");
}




static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  char gwin_name[100];
  gint iw, ih;
  guint mode = GDK_GL_MODE_RGB | GDK_GL_MODE_DEPTH | GDK_GL_MODE_STENCIL;
  GtkWidget *scrolled_window;
  GtkWidget *vbox;
  GdkGLConfig *glconfig;
  
  glconfig = gdk_gl_config_new_by_mode (mode | GDK_GL_MODE_DOUBLE) ;
  if (glconfig == NULL)
    {
      glconfig = gdk_gl_config_new_by_mode (mode);
      if (glconfig == NULL)
	{
	  /* FIXME XXX */
	  g_print ("*** No appropriate OpenGL-capable visual found.\n");
	  return;
	}
    }
  /* commenter --- decommenter */
  examine_gl_config_attrib(glconfig);

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

  /*
  ** NEW !! 
  */

  /* Set OpenGL-capability to the widget */
  gtk_widget_set_gl_capability (dd->private->drawing,
				glconfig,
				NULL,
				TRUE,
				GDK_GL_RGBA_TYPE);
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
** #############################################################
**
**                      ROUTINES POUR OPENGL
**
** #############################################################
*/

void afficher_repere(float ox, float oy, float oz)
{
  printf("Afficher_repere\n");
  glBegin(GL_LINES);
  glColor3f(1.0,0.0,0.0);
  glVertex3f(ox, oy, oz);
  glVertex3f(ox+10.0, oy, oz);
  glColor3f(0.0,1.0,0.0);
  glVertex3f(ox, oy, oz);
  glVertex3f(ox, oy+10.0, oz);
  glColor3f(0.0,0.0,1.0);
  glVertex3f(ox, oy, oz);
  glVertex3f(ox, oy, oz+10.0);
  glEnd();
}

int use_camera(BCG *Xgc)
{
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity ();
  gluLookAt( Xgc->private->camera.position.x,
	     Xgc->private->camera.position.y,
	     Xgc->private->camera.position.z,
	     Xgc->private->camera.cible.x,
	     Xgc->private->camera.cible.y,
	     Xgc->private->camera.cible.z,
	     Xgc->private->camera.orientation.x,
	     Xgc->private->camera.orientation.y,
	     Xgc->private->camera.orientation.z);
  glMatrixMode(GL_PROJECTION); 
  glLoadIdentity();
  glOrtho(Xgc->private->camera.xmin ,
	  Xgc->private->camera.xmax ,
	  Xgc->private->camera.ymin ,
	  Xgc->private->camera.ymax ,
	  Xgc->private->camera.near,Xgc->private->camera.far);
  glMatrixMode(GL_MODELVIEW);
  return 0;
}


void change_camera(BCG *Xgc,const double *val)
{
#if 1
  Xgc->private->camera.position.x=*val;val++;
  Xgc->private->camera.position.y=*val;val++;
  Xgc->private->camera.position.z=*val;val++;
  Xgc->private->camera.cible.x=*val;val++;
  Xgc->private->camera.cible.y=*val;val++;
  Xgc->private->camera.cible.z=*val;val++;
  Xgc->private->camera.orientation.x=*val;val++;
  Xgc->private->camera.orientation.y=*val;val++;
  Xgc->private->camera.orientation.z=*val;val++;
  Xgc->private->camera.near=*val;val++;
  Xgc->private->camera.far=*val;val++;
  Xgc->private->camera.xmin=*val;val++;
  Xgc->private->camera.xmax=*val;val++;
  Xgc->private->camera.ymin=*val;val++;
  Xgc->private->camera.ymax=*val;val++;
#endif
  expose_event( Xgc->private->drawing,NULL, Xgc);
}

/* 
 * force expose events to be executed 
 */

static void force_affichage(BCG *Xgc)
{
  Xgc->private->resize = 1;
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
** Commute la vue : mode perspective cavaliere (2D) --- mode perspective)
*/

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
** TGA avec transparence
** goto http://nehe.gamedev.net/    lesson #32
*/
static bool LoadTGA(TextureImage *texture, char *filename)
{    
  GLubyte		TGAheader[12]={0,0,2,0,0,0,0,0,0,0,0,0};
  GLubyte		TGAcompare[12];	
  GLubyte		header[6];
  GLuint		bytesPerPixel;	
  GLuint		imageSize;
  GLuint		temp;
  GLuint		type=GL_RGBA;
  GLuint             i;
  /* printf("XXX je vais charger la texture %s\n", filename);*/
  FILE *file = fopen(filename, "rb");
     
  if ( file==NULL ||
       fread(TGAcompare,1,sizeof(TGAcompare),file)!=sizeof(TGAcompare) ||
       memcmp(TGAheader,TGAcompare,sizeof(TGAheader))!=0 ||
       fread(header,1,sizeof(header),file)!=sizeof(header))
    {
      if (file == NULL)
	{
	  /* printf("%s chemin inexistant\n", filename);*/
	  return false;
	}
      else
	{
	  fclose(file);
	  return false;
	}
    }

  texture->width  = header[1] * 256 + header[0];
  texture->height = header[3] * 256 + header[2];
     
  if (	texture->width	<=0	||
	texture->height	<=0	||
	(header[4]!=24 && header[4]!=32))
    {
      fclose(file);
      printf("%s pas une texture 24 ou 32 bits\n", filename);
      return false;
    }
     
  texture->bpp	= header[4];
  bytesPerPixel	= texture->bpp/8;
  imageSize	= texture->width*texture->height*bytesPerPixel;

  texture->imageData=(GLubyte *)malloc(imageSize);

  if(	texture->imageData==NULL ||
	fread(texture->imageData, 1, imageSize, file)!=imageSize)
    {
      if(texture->imageData!=NULL)
	free(texture->imageData);
	     
      fclose(file);
      return false;
    }
	
  for (i=0; i<(unsigned) imageSize; i+=bytesPerPixel)
    {
      temp=texture->imageData[i];
      texture->imageData[i] = texture->imageData[i + 2];
      texture->imageData[i + 2] = temp;
    }

  fclose (file);

  glGenTextures(1, &texture->texID);

  glBindTexture(GL_TEXTURE_2D, texture->texID);	
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	
  if (texture->bpp==24)
    type=GL_RGB;

  glTexImage2D(GL_TEXTURE_2D, 0, type, texture->width, texture->height, 0, type, GL_UNSIGNED_BYTE, texture->imageData);

  return true;
}

/*
** Construit les listes d'affichages pour stocker les cararcteres
** alphanumeriques sous forme de rectangles textures avec
** transparence.
**
** texID : ID de la texture.
** retourne la liste d'affichage des fontes
** Ne pas oublier de detruire les listes d'affiches a la fin du prog !!
*/

static GLuint BuildFont(GLuint texID,int nb_char,int nb_ligne,int nb_col)			
{
  int    loop;
  GLuint base;

  base=glGenLists(nb_char);
  glBindTexture(GL_TEXTURE_2D, texID);
  for (loop=0; loop<nb_char; loop++)
    {
      float cx = (float) (loop%16) / ((float) nb_ligne);
      float cy = (float) (loop/16) / ((float) nb_col);
      float dx = 1.0 / ((float) nb_ligne);
      float dy = 1.0 / ((float) nb_col);
      glNewList(base+loop,GL_COMPILE);
      glBegin(GL_QUADS);
      glTexCoord2f(cx,    1.0f-cy);    glVertex2i(0,0);
      glTexCoord2f(cx+dx, 1.0f-cy);    glVertex2i(TAILLE_CHAR,0);
      glTexCoord2f(cx+dx, 1.0f-cy-dy); glVertex2i(TAILLE_CHAR,TAILLE_CHAR);
      glTexCoord2f(cx,    1.0f-cy-dy); glVertex2i(0,TAILLE_CHAR);
      glEnd();
      glTranslated(TRANSLATE_CHAR,0,0);
      glEndList();
    }
  return base;
}



/*
** Affiche une chaine de caractere dans l'espace 3D
** les textures sont toujours orientees dans la meme direction
** meme si on tourne la camera
** x,y,z la position du premier caractere.
** scale : echelle du texte
** rot : angle rotation 
** set : if you have a look at the bitmap, you'll notice there 
** are two different character sets. The first character set is
** normal, and the second character set is italicized. If set is 0,
** the first character set is selected. If set is 1 or greater the
** second character set is selected.
*/

static void glPrint2D(BCG *Xgc, GLfloat x, GLfloat y,  GLfloat scal, GLfloat rot, bool set, const char *string, ...)
{
  char		text[256];
  va_list		ap;
     
  if (string == NULL)
    return;
	
  if (set > 1) set = 1;     

  glColor3f(0,0,0);      
  va_start(ap, string);
  vsprintf(text, string, ap);
  va_end(ap);
     
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);

  glBindTexture(GL_TEXTURE_2D, Xgc->private->fonte_encours);
  glPushMatrix();
  glLoadIdentity();
  glTranslatef(x-TRANSLATE_CHAR*ECHELLE_CHAR/2, y-TAILLE_CHAR*ECHELLE_CHAR,0);
  glRotated(rot,1,0,0);
  glScaled(scal,scal,scal);
  glListBase(Xgc->private->base_encours-32+(128*set));
  glCallLists(strlen(text), GL_UNSIGNED_BYTE, text);
  glPopMatrix();
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
}


#if 0
#include <GL/glut.h>

static void glut_display_string(BCG *Xgc, GLfloat x, GLfloat y, const char *string)
{
  int j;
  double height = 10.0f;
  // The Mono Roman font supported by GLUT has characters that are
  // 104.76 units wide, up to 119.05 units high, and descenders can
  // go as low as 33.33 units.
  // We scale the text to make its height that desired by the caller.
  const static float FONT_HEIGHT = 119.05f;
  glPushMatrix();
  glTranslatef( x, y, 0 );
  float s = height / FONT_HEIGHT; // scale factor
  glScalef( s, s, 1 );
  for ( j = 0; string[j] != '\0'; ++j )
    glutStrokeCharacter( GLUT_STROKE_MONO_ROMAN, string[j] );
  glPopMatrix();
}
#endif 


/*
 * Comme glPrint3D mais les textures sont toujours orientees vers la camera
 * quelque soit la position de la camera dans l'espace
 * goto http://www.lighthouse3d.com/opengl/billboarding/index.php3?billInt
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



