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
 * jpc@cermics.enpc.fr
 * Open GL Driver 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <gtk/gtk.h>
#include <gtk/gtkgl.h>

#ifdef HAVE_FREETYPE
#undef PANGO_DISABLE_DEPRECATED 
#include <pango/pangoft2.h> 
#endif 

#define PERI_PRIVATE 1
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "nsp/graphics/periGL.h"
#include "nsp/version.h"
#include "nsp/graphics/color.h"
#include "nsp/command.h"

extern void nsp_ogl_set_view(BCG *Xgc);
extern void create_graphic_window_menu( BCG *dd);
extern void start_sci_gtk();

/* periGL with the new DRAW_CHECK only works in recording mode 
 * since the drawing actions are to be done in expose_event.
 */ 

#define DRAW_CHECK_XX							\
  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 )	\
    {  nsp_gtk_invalidate(Xgc); Xgc->private->draw = TRUE;  return; }

/* just a test to perform graphic when we are not in recording 
 * mode: this is not supposed to work since graphics are 
 * not performed in an expose_event context.But this 
 * is usefull for acquiring for ex the zoom rectangle 
 */

#define DRAW_CHECK							\
  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 )	\
    {  nsp_gtk_invalidate(Xgc);						\
      if (Xgc->record_flag == TRUE) {Xgc->private->draw = TRUE;return;} \
    }

static unsigned long maxcol; /* XXXXX : � revoir */
static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data);
static void nsp_gtk_invalidate(BCG *Xgc);
static void nsp_pango_initialize_layout(BCG *Xgc);
static void nsp_pango_finalize_layout(BCG *Xgc);
static void gl_pango_ft2_render_layout (PangoLayout *layout,      GdkRectangle * rect);
static void force_affichage(BCG *Xgc);
static void clip_rectangle(BCG *Xgc, GdkRectangle clip_rect);
static void unclip_rectangle(GdkRectangle clip_rect);

/*
 * the current graphic data structure 
 */

static void nsp_gtk_set_color(BCG *Xgc,int col);
static void draw_mark(BCG *Xgc,int *x, int *y);
static void pixmap_clear_rect(BCG *Xgc,int x,int y,int w,int h);
static void SciClick(BCG *Xgc,int *ibutton, int *x1, int *yy1,int *iwin,
		     int iflag,int getmotion, int getrelease,int getkey,
		     char *str, int lstr, int change_cursor);
static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,
				   GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos);

static void scig_deconnect_handlers(BCG *winxgc);
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

/* End of graphic (do nothing)  */

void xendgraphic(void) 
{
} 


void xend(BCG *Xgc)
{
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


#include "perigtk/events.c"  

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

/*
 * select window intnum as the current window 
 * window is created if necessary 
 * return the value of the previous current window 
 */

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

/* Set a clip zone (rectangle ) */

static void xset_clip(BCG *Xgc,int x[])
{
  int i;
  GdkRectangle clip_rect ={x[0],x[1],x[2],x[3]};
  Xgc->ClipRegionSet = 1;
  for (i=0 ; i < 4 ; i++)   Xgc->CurClipRegion[i]= x[i];
  /* gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect); */
  clip_rectangle(Xgc, clip_rect);
}

/* unset clip zone */

static void xset_unclip(BCG *Xgc)
{
  static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
  Xgc->ClipRegionSet = 0;
  /* gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect); */
  unclip_rectangle(clip_rect);
}

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

/* to get the thickness value */

static int xget_thickness(BCG *Xgc)
{
  return Xgc->CurLineWidth ;
}

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

/* To get the id of the last pattern */

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


/* to get the current dash-style */

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

/*
 * All the following function xxxx_1 
 * can be called using nsp_engine for a direct call 
 * or using C2F(dr1) using a name table 
 * this is usefull for replaying with the Rec driver (See Rec.c) 
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



static void drawline3D(BCG *Xgc,double x1,double y1, double z1, double x2,double y2, double z2)
{
  DRAW_CHECK;
  glBegin(GL_LINES);
  glVertex3d(x1, y1, z1);
  glVertex3d(x2, y2, z2);
  glEnd();
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
 *
 */
static void drawpolymark3D(BCG *Xgc,double *vx, double *vy, double *vz, int n)
{
  DRAW_CHECK;
  printf("To be done \n");
}

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

  nsp_pango_finalize_layout(winxgc);

  FREE(winxgc->private);
  /* remove current window from window list */
  window_list_remove(intnum);
}


/*
 * Routines for initialization : string is a display name 
 */

static void  nsp_gtk_set_color(BCG *Xgc,int col)
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

static gint realize_event(GtkWidget *widget, gpointer data);
static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data);

#include "perigtk/init.c" 

/*--------------------------------------------------------
 * Initialisation of the graphic context. Used also 
 *  to come back to the default graphic state}
 *---------------------------------------------------------*/

extern void nsp_initialize_gc( BCG *Xgc ) ;


static void xset_default(BCG *Xgc)
{
  //nsp_initialize_gc(Xgc);
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


/*-----------------------------------------------------------------
 * Text and symbols with pango 
 *----------------------------------------------------------------*/

#define FONTNUMBER 7 
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 12

/* Must be of size FONTMAXSIZE */

static const int pango_size[] = { 8 ,10,12,14,18,24};

static char *pango_fonttab[] ={"Courier", "Standard Symbols L","Sans","Sans","Sans","Sans"};

static void nsp_pango_finalize_layout(BCG *Xgc)
{
  if ( Xgc->private->layout != NULL) 
    {
      g_object_unref ( Xgc->private->layout);Xgc->private->layout=NULL;
      g_object_unref ( Xgc->private->mark_layout);Xgc->private->mark_layout=NULL;
      pango_font_description_free ( Xgc->private->desc); Xgc->private->desc=NULL;
      pango_font_description_free ( Xgc->private->mark_desc);Xgc->private->mark_desc=NULL;
      g_object_unref (G_OBJECT (Xgc->private->ft2_context));
    }
}

static void nsp_pango_initialize_layout(BCG *Xgc)
{
  if ( Xgc->private->layout == NULL) 
    {
      Xgc->private->context = gtk_widget_get_pango_context (Xgc->private->drawing);
      /* a revoir deprecated XXXX */
#ifndef HAVE_FREETYPE
      Xgc->private->ft2_context = pango_ft2_get_context (72, 72);
      Xgc->private->layout = pango_layout_new (Xgc->private->ft2_context);
      Xgc->private->mark_layout = pango_layout_new (Xgc->private->ft2_context);
#else 
      Xgc->private->layout = NULL;
      Xgc->private->mark_layout = NULL;
#endif 
      Xgc->private->desc = pango_font_description_new();
      Xgc->private->mark_desc = pango_font_description_from_string(pango_fonttab[1]);
    }
}


static void xset_font(BCG *Xgc,int fontid, int fontsize)
{ 
  int i,fsiz;
  i = Min(FONTNUMBER-1,Max(fontid,0));
  fsiz = Min(FONTMAXSIZE-1,Max(fontsize,0));
  if ( Xgc->fontId != i || Xgc->fontSize != fsiz )
    {
      Xgc->fontId = i;
      Xgc->fontSize = fsiz;
      pango_font_description_set_family(Xgc->private->desc, pango_fonttab[i]);
      /* pango_font_description_set_size (Xgc->private->desc, pango_size[fsiz] * PANGO_SCALE);*/
      pango_font_description_set_absolute_size (Xgc->private->desc, pango_size[fsiz] * PANGO_SCALE);

      pango_layout_set_font_description (Xgc->private->layout, Xgc->private->desc);
    }
}


static void xset_mark(BCG *Xgc,int number, int size)
{ 
  int n_size;
  Xgc->CurHardSymb = Max(Min(SYMBOLNUMBER-1,number),0);
  n_size  = Max(Min(FONTMAXSIZE-1,size),0);
  if ( Xgc->CurHardSymbSize != n_size )
    {
      Xgc->CurHardSymbSize = n_size;
      /* pango_font_description_set_size (Xgc->private->mark_desc, pango_size[fsiz] * PANGO_SCALE);*/
      pango_font_description_set_absolute_size (Xgc->private->mark_desc, 
						pango_size[Xgc->CurHardSymbSize] * PANGO_SCALE);
      pango_layout_set_font_description (Xgc->private->mark_layout, Xgc->private->mark_desc);
    }
}


/* To get the  id and size of the current font 
 */

static void  xget_font(BCG *Xgc,int *font)
{
  font[0] = Xgc->fontId ;
  font[1] = Xgc->fontSize ;
}

/* To get the current mark id */

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}


static void loadfamily(char *name, int *j)
{ 
}

static void queryfamily(char *name, int *j,int *v3)
{ 
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
 *
 */

#if 0 
static void print_gl_config_attrib (GdkGLConfig *glconfig,
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

static void examine_gl_config_attrib (GdkGLConfig *glconfig)
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
#endif 

/*
 *
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


/* select the view mode from 2d view to 
 * 3d view.
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
 *
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

#ifndef HAVE_FREETYPE

static void gl_pango_ft2_render_layout (PangoLayout *layout,      GdkRectangle *e_rect)
{
  Sciprintf("Cannot render string in OpenGL missing pangoft2\n");
}

#else 

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


#endif 

/*
 * include the OpenGl basic graphic routines 
 */

#include "perigtk/peridraw_gl.c"

