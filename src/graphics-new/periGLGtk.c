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
#endif 

#ifdef PERIGL 
#define HAVE_FREETYPE
#ifdef HAVE_FREETYPE
#include <pango/pangoft2.h> 
#endif 
#include "nsp/graphics/periGL.h"
#endif 

#include "nsp/version.h"
#include "nsp/graphics/color.h"
#include "nsp/command.h"

/* add : periGtk with OpenGl rendering 
 * the first three variables are to be moved 
 * in periGL.h 
 */
static int nsp_set_gldrawable(BCG *Xgc,GdkPixmap *pixmap);

#ifdef PERIGL 
static GdkGLConfig *glconfig = NULL;
static void drawpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag);
static void fillpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag);
static void realize_event_ogl();
static void clip_rectangle(BCG *Xgc, GdkRectangle clip_rect);
static void unclip_rectangle(GdkRectangle clip_rect);
static void gl_pango_ft2_render_layout (PangoLayout *layout,      GdkRectangle * rect);
static void nsp_fonts_initialize(BCG *Xgc);
static void nsp_fonts_finalize(BCG *Xgc);
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
static void pixmap_clear_rect   (BCG *Xgc,int x,int y,int w,int h);
static void SciClick(BCG *Xgc,int *ibutton,int *imask, int *x1, int *yy1,int *iwin,int iflag,int getmotion, int getrelease,int getkey,char *str, int lstr, int change_cursor);
static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos);
static void scig_deconnect_handlers(BCG *winxgc);
static void draw_mark(BCG *Xgc,int *x, int *y);
static void force_affichage(BCG *Xgc);

/* utility for points allocations */
#ifdef PERIGTK 
static void gdk_draw_text_rot(GdkDrawable *drawable, GdkFont *font,  GdkGC *gc,
			      int x, int y, int maxx, int maxy, const gchar *text,
			      gint text_length, double angle);
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
      if ( Xgc->private->gldrawable != NULL 
	   &&  GDK_IS_GL_DRAWABLE (Xgc->private->gldrawable))
	gdk_gl_drawable_wait_gl(Xgc->private->gldrawable);
      gdk_draw_pixmap(Xgc->private->drawing->window,Xgc->private->stdgc, Xgc->private->extra_pixmap,
		      0,0,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
      gdk_draw_pixmap(Xgc->private->pixmap, Xgc->private->stdgc, Xgc->private->extra_pixmap,
		      0,0,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
      if ( Xgc->private->gldrawable != NULL 
	   &&  GDK_IS_GL_DRAWABLE (Xgc->private->gldrawable))
	gdk_gl_drawable_wait_gdk(Xgc->private->gldrawable);
    }
  else
    {
      /* see the comments at the begining */
      force_affichage(Xgc);
    }
}

/*
 * clear the current drawable which is always a pixmap 
 */

static void pixmap_clear_rect(BCG *Xgc,int x, int y, int w, int h)
{
  /* if (! gdk_gl_drawable_gl_begin (Xgc->private->gldrawable,Xgc->private->glcontext)) return; */
  glClearColor(Xgc->private->gcol_bg.red /255.0,
	       Xgc->private->gcol_bg.green /255.0,
	       Xgc->private->gcol_bg.blue /255.0,0.0);
  glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  /* gdk_gl_drawable_gl_end (Xgc->private->gldrawable); */
  /* 
     gdk_gc_set_background(Xgc->private->stdgc, &Xgc->private->gcol_bg);
     gdk_draw_rectangle(Xgc->private->extra_pixmap,Xgc->private->stdgc, TRUE,
     0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
  */
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
      gdk_pixmap_unset_gl_capability (Xgc->private->extra_pixmap);
      g_object_unref (G_OBJECT (Xgc->private->extra_pixmap));
      /*  gdk_pixmap_unref((GdkPixmap *) Xgc->private->extra_pixmap);*/
      Xgc->private->drawable = Xgc->private->extra_pixmap = temp;
      nsp_set_gldrawable(Xgc, Xgc->private->extra_pixmap);
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
	  nsp_set_gldrawable(Xgc, Xgc->private->extra_pixmap);
	  pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
	  Xgc->CurPixmapStatus = 1;
	}
    }
  else 
    {
      /** I remove the extra pixmap to the window **/
      xinfo(Xgc," ");
      if ( Xgc->private->gldrawable != NULL) 
	gdk_gl_drawable_gl_end (Xgc->private->gldrawable);
      gdk_pixmap_unset_gl_capability (Xgc->private->extra_pixmap);
      g_object_unref (G_OBJECT (Xgc->private->extra_pixmap));
      /* gdk_pixmap_unref((GdkPixmap *) Xgc->private->extra_pixmap); */
      Xgc->private->extra_pixmap = NULL;
      Xgc->private->drawable = (GdkDrawable *)Xgc->private->pixmap;
      Xgc->CurPixmapStatus = 0; 
      nsp_set_gldrawable(Xgc, Xgc->private->pixmap);
      pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
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

#ifdef PERIGL 
void drawsegments3D(BCG *Xgc,double *x,double *y,double *z, int n, int *style, int iflag)
{
  int dash,color,i;
  DRAW_CHECK;
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
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
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
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
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
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
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else 
	{
	  fillpolyline3D_shade(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,fillvect+(p)*i,p,1);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
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
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
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
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else 
	{
	  Xgc->graphic_engine->xset_pattern(Xgc,-fillvect[i]);
	  fillpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
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
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);

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
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  Xgc->graphic_engine->xset_mark(Xgc,symb[0],symb[1]);
}
#endif 

/*
 * window_list management 
 */


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
 * initgraphic : create initialize graphic windows
 */

static gint realize_event(GtkWidget *widget, gpointer data);
static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data);


#include "perigtk/events.c"  
#include "perigtk/init_gtkgl.c" 

/*--------------------------------------------------------
 * Initialisation of the graphic context. Used also 
 *  to come back to the default graphic state}
 *---------------------------------------------------------*/

extern void nsp_initialize_gc( BCG *Xgc ) ;

static void xset_default(BCG *Xgc)
{
  nsp_initialize_gc(Xgc);
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
    }

  /* default value is to use the background pixmap */
  dd->private->drawable= (GdkDrawable *) dd->private->pixmap;
  nsp_set_gldrawable(dd,dd->private->pixmap);
  pixmap_clear_rect(dd,0,0,dd->CWindowWidth, dd->CWindowHeight);
  /*
  gdk_gc_set_foreground(dd->private->stdgc, &dd->private->gcol_bg);
  gdk_draw_rectangle(dd->private->pixmap, dd->private->stdgc, TRUE, 0, 0,
		     dd->CWindowWidth, dd->CWindowHeight);
  */

#ifdef PERIGL 
  if (!gdk_gl_drawable_gl_begin (dd->private->gldrawable,dd->private->glcontext))
    return FALSE;
  realize_event_ogl(dd);
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
      /* here we execute an expose and it works also when CurPixmapStatus == 1.
       * when CurPixmapStatus == 1 the extra pixmap is resized in scig_resize
       * and the drawing are redirected to the extra_pixmap. If a wshow is 
       * recorded then it will copy the extra_pixmap to both window and pixmap.
       * the last gdk_draw_pixmap found here is not usefull in that case. 
       */
      dd->private->resize = 0;
      if ( dd->private->pixmap ) 
	{
	  /* free old pixmap */
	  gdk_pixmap_unset_gl_capability (dd->private->pixmap);
	  g_object_unref (G_OBJECT (dd->private->pixmap));
	  /* gdk_pixmap_unref(dd->private->pixmap); */
	  if ( dd->CurPixmapStatus == 0 ) dd->private->gldrawable=NULL;
	}
      /* allocate a new pixmap and set its open Gl capabilities */
      dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
					   dd->CWindowWidth, dd->CWindowHeight,
					   -1);
      nsp_set_gldrawable(dd,dd->private->pixmap);
      /* update drawable */
      if ( dd->CurPixmapStatus == 0 ) dd->private->drawable = dd->private->pixmap;
      /* fill private background with background */
      pixmap_clear_rect(dd,0,0,dd->CWindowWidth, dd->CWindowHeight);
      /* 
      gdk_gc_set_background(dd->private->stdgc, &dd->private->gcol_bg);
      gdk_draw_rectangle(dd->private->pixmap,dd->private->stdgc, TRUE,0,0,
			 dd->CWindowWidth, dd->CWindowHeight);
      */
      /* we resize and draw  
       * Note that the gldrawable will be changed in scig_resize if we are 
       * using the extra_pixmap.
       */
      gdk_gl_drawable_wait_gdk(dd->private->gldrawable);
      gdk_gl_drawable_gl_begin(dd->private->gldrawable, dd->private->glcontext);  
      dd->private->in_expose= TRUE;
      dd->private->gl_only = TRUE;
      scig_resize(dd->CurWindow);
      dd->private->in_expose= FALSE;
      gdk_gl_drawable_gl_end (dd->private->gldrawable);
      glFlush ();
      gdk_gl_drawable_wait_gl (dd->private->gldrawable);
      /* synchronize pixmap */
      gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
		      dd->CWindowWidth, dd->CWindowHeight);
      gdk_gl_drawable_wait_gdk(dd->private->gldrawable);
    }
  else 
    {
      if ( dd->private->draw == TRUE ) 
	{
	  gdk_gl_drawable_wait_gdk(dd->private->gldrawable);
	  gdk_gl_drawable_gl_begin (dd->private->gldrawable,dd->private->glcontext);
	  dd->private->draw = FALSE;
	  dd->private->in_expose= TRUE;
	  dd->private->gl_only = TRUE;
	  scig_replay(dd->CurWindow);
	  dd->private->in_expose= FALSE;
	  gdk_gl_drawable_gl_end (dd->private->gldrawable);
	}
      glFlush ();
      gdk_gl_drawable_wait_gl(dd->private->gldrawable);
      if (event  != NULL) 
	gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,
			event->area.x, event->area.y, event->area.x, event->area.y,
			event->area.width, event->area.height);
      else 
	gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
			dd->CWindowWidth, dd->CWindowHeight);
      /* if a zrect exists then add it on graphics  */
      if ( dd->zrect[2] != 0 && dd->zrect[3] != 0) 
	gdk_draw_rectangle(dd->private->drawing->window,dd->private->wgc,FALSE,
			   dd->zrect[0],dd->zrect[1],dd->zrect[2],dd->zrect[3]);
      gdk_gl_drawable_wait_gdk(dd->private->gldrawable);
    }
  gdk_flush();
  return FALSE;
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

#ifdef PERIGTK 
static void gdk_draw_text_rot(GdkDrawable *drawable,
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
#endif 

/*
 * added for opengl capabilities 
 */


/* this is to be called when the pixmap is recreated */

static int nsp_set_gldrawable(BCG *Xgc,GdkPixmap *pixmap)
{
#ifdef PERIGL 
  if (glconfig == NULL)  
    glconfig = gdk_gl_config_new_by_mode (GDK_GL_MODE_RGB    |
					  GDK_GL_MODE_DEPTH  |
					  GDK_GL_MODE_SINGLE);
  if (glconfig == NULL)  
    {
      Xgc->private->gldrawable= NULL;
      Xgc->private->glcontext = NULL;
      return FALSE;
    }
  if ( Xgc->private->gldrawable != NULL 
       &&  GDK_IS_GL_DRAWABLE (Xgc->private->gldrawable))
     gdk_gl_drawable_gl_end (Xgc->private->gldrawable);
  
  Xgc->private->gldrawable = GDK_GL_DRAWABLE (gdk_pixmap_set_gl_capability (pixmap,
									    glconfig,
									    NULL));
  /*
   * Create OpenGL rendering context (not direct or direct: third argument).
   * 
   */
  if (Xgc->private->glcontext == NULL)
    Xgc->private->glcontext = gdk_gl_context_new (Xgc->private->gldrawable,
						  NULL,
						  FALSE,
						  GDK_GL_RGBA_TYPE);
  if (Xgc->private->glcontext == NULL)
    {
      g_print ("Connot create the OpenGL rendering context\n");
      return FALSE;
    }
  
  gdk_gl_drawable_make_current(Xgc->private->gldrawable,Xgc->private->glcontext);
  gdk_gl_drawable_gl_begin(Xgc->private->gldrawable,Xgc->private->glcontext);
  glClear(GL_DEPTH_BUFFER_BIT);
  nsp_ogl_set_view(Xgc);
  /* g_print ("The OpenGL rendering context is created\n"); */
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
   * qui sont important pour l'élimination des parties cachées
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

static void realize_event_ogl(BCG *dd )
{
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
  /* gdk_gl_drawable_gl_end (dd->private->gldrawable); */
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

#endif /* PERIGL */

#ifdef PERIGTK
/* get drawable as an image 
 *
 */

GdkImage* nsp_get_image(BCG *Xgc) 
{
  return gdk_drawable_get_image(Xgc->private->drawable,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
}

GdkPixbuf* nsp_get_pixbuf(BCG *Xgc) 
{
  return gdk_pixbuf_get_from_drawable(NULL,Xgc->private->drawable,
				      gdk_colormap_get_system(),
				      0,0,0,0,
				      Xgc->CWindowWidth,Xgc->CWindowHeight);
}

#endif 

/*
 * include basic operations 
 */


#ifdef PERIGL 
#include "perigtk/fonts_pango_ft2_gl.c"
#include "perigtk/peridraw_gl.c"
#endif 

#ifdef PERIGTK 
#ifdef WITH_PANGO
#include "perigtk/fonts_pango_gdk.c"
#else 
#include "perigtk/fonts_gdk.c" 
#endif 
#include "perigtk/peridraw_gdk.c"
#endif 

