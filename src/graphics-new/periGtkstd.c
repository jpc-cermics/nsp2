/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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
 * Gtk/Cairo/OpenGL driver 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <gtk/gtk.h>

#define PERI_PRIVATE 1
#include "nsp/sciio.h" 
#include "nsp/math.h"
#ifdef PERIGL 
#include "nsp/graphics/periGL.h"
#ifdef PERIGLGTK 
static int nsp_set_gldrawable(BCG *Xgc,GdkPixmap *pixmap);
#endif 
#else 
#include "nsp/graphics/periGtk.h"
#endif 
#include "nsp/version.h"
#include "nsp/graphics/color.h"
#include "nsp/command.h"

/*
 * macro used in each drawing primitive. 
 *  Xgc->record_flag == TRUE if we are recording graphics 
 *  Xgc->private->in_expose == TRUE if the call is from an expose_event 
 *  Xgc->CurPixmapStatus == 0 if we are not using an extra pixmap 
 *  Xgc->private->draw = TRUE we have something to draw 
 */

#ifdef PERIGL 
#ifdef PERIGLGTK 

/* version for pixmap case */ 

#define DRAW_CHECK							\
  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 )	\
    nsp_gtk_invalidate(Xgc);

#else /* PERIGLGTK */

#define DRAW_CHECK							\
  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 )	\
    {  nsp_gtk_invalidate(Xgc);						\
      if (Xgc->record_flag == TRUE) {Xgc->private->draw = TRUE;return;} \
    }
#endif /* PERIGLGTK */

#else /* PERIGL */
/* PERICAIRO and PERIGTK 
 * we always draw in a drawable which is a pixmap but the expose event is asynchronous
 */

#ifdef PERICAIRO
#define DRAW_CHECK							\
  if ( Xgc->private->cairo_cr == NULL) return;				\
  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0	\
       && Xgc->private->drawing != NULL) nsp_gtk_invalidate(Xgc); 
#else 
#define DRAW_CHECK							\
  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 ) nsp_gtk_invalidate(Xgc); 
#endif /* PERICAIRO */

#endif /* PERIGL */

/* Global variables to store graphic drivers 
 */

#if defined(PERIGTK) 
Gengine * nsp_gengine = &Gtk_gengine ;
GTK_locator_info nsp_event_info = { -1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0};
#endif 

/**
 * force_affichage:
 * @Xgc: a #BCG 
 * 
 * force expose events to be executed by calling 
 * gdk_window_process_updates().
 **/

static void force_affichage(BCG *Xgc)
{
#ifndef PERIGLGTK
  Xgc->private->resize = 1;
#endif 
  gdk_window_process_updates (Xgc->private->drawing->window, FALSE);
}

/**
 * force_redraw:
 * @Xgc: a #BCG 
 * 
 * 
 * force an expose_event with draw set to TRUE
 **/

static void force_redraw(BCG *Xgc)
{
  nsp_gtk_invalidate(Xgc);
  Xgc->private->draw = TRUE;
  gdk_window_process_updates (Xgc->private->drawing->window, FALSE);
}

/*---------------------------------------------------------
 * Next routine are used to deal with the extra_pixmap 
 * which is used when xset('pixmap',1) is activated at 
 * nsp level. 
 *---------------------------------------------------------*/

/**
 * xset_pixmapclear:
 * @Xgc: a #BCG 
 * 
 * clear the extra pixmap associated to the graphic window.
 **/

static void xset_pixmapclear(BCG *Xgc)
{
  if ( Xgc->CurPixmapStatus == 1) 
    {
      pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
    }
}

/**
 * xset_show:
 * @Xgc: a #BCG 
 * 
 * make a draw using the extra_pixmap as the source for updating 
 * the graphic window and its standard associated pixmap.
 **/

static void xset_show(BCG *Xgc)
{
  if ( Xgc->CurPixmapStatus == 1) 
    {
#ifdef PERIGLGTK 
      /* we copy the extra_pixmap to the window and to the backing store pixmap */
      /* gdk_gc_set_background(Xgc->private->stdgc, &Xgc->private->gcol_bg); */
      /* drawing to the window and to the backing store pixmap */
      if ( Xgc->private->gldrawable != NULL 
	   &&  GDK_IS_GL_DRAWABLE (Xgc->private->gldrawable))
	gdk_gl_drawable_wait_gl(Xgc->private->gldrawable);
      gdk_draw_drawable(Xgc->private->drawing->window,Xgc->private->stdgc, Xgc->private->extra_pixmap,
		      0,0,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
      gdk_draw_drawable(Xgc->private->pixmap, Xgc->private->stdgc, Xgc->private->extra_pixmap,
		      0,0,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
      if ( Xgc->private->gldrawable != NULL 
	   &&  GDK_IS_GL_DRAWABLE (Xgc->private->gldrawable))
	gdk_gl_drawable_wait_gdk(Xgc->private->gldrawable);
#else 
      /* we copy the extra_pixmap to the window and to the backing store pixmap 
       * except for perigl which draw without a Xgc->private->pixmap.
       */
      gdk_draw_drawable(Xgc->private->drawing->window,Xgc->private->stdgc, Xgc->private->extra_pixmap,
		      0,0,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
#ifndef PERIGK
      gdk_draw_drawable(Xgc->private->pixmap, Xgc->private->stdgc, Xgc->private->extra_pixmap,
		      0,0,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
#endif 
#endif 
    }
  else
    {
      /* see the comments at the begining */
      force_affichage(Xgc);
    }
}

/*
 * General routines callable from nsp
 */

/**
 * xselgraphic:
 * @Xgc: a #BCG 
 * 
 * To select (raise on the screen )the current graphic Window
 * If there's no graphic window then select creates one 
 * 
 **/

static void xselgraphic(BCG *Xgc)
{ 
  /* Test not really usefull: see sciwin in matdes.f */
  if ( Xgc == (BCG *)0 || Xgc->private->window ==  NULL) initgraphic("",NULL,NULL,NULL,NULL,NULL,'e',NULL);
  gdk_window_show(Xgc->private->window->window);
  gdk_flush();
}

/**
 * xend:
 * @Xgc: a #BCG  
 * 
 * End of graphic (do nothing)  
 **/

static void xend(BCG *Xgc)
{
  /* Must destroy everything  */
}

/**
 * clearwindow:
 * @Xgc: a #BCG  
 * 
 * 
 * clears the current graphic window.
 **/

static void clearwindow(BCG *Xgc)
{
#ifdef PERICAIRO
  cairo_t *cr; 
#endif 
  /* we use the private->stdgc graphic context */
  DRAW_CHECK;
#ifdef PERICAIRO
  cr =  Xgc->private->cairo_cr;
  cairo_set_source_rgb(cr,
		       Xgc->private->gcol_bg.red/65535.0,
		       Xgc->private->gcol_bg.green/65535.0,
		       Xgc->private->gcol_bg.blue/65535.0);
  cairo_rectangle (cr,0,0, Xgc->CWindowWidth, Xgc->CWindowHeight);
  cairo_fill (cr);
#endif 
#ifdef PERIGTK  
  gdk_gc_set_foreground(Xgc->private->stdgc, &Xgc->private->gcol_bg);
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->stdgc, TRUE, 0, 0,
		     Xgc->CWindowWidth, Xgc->CWindowHeight);
#endif 
#ifdef PERIGL 
  glClearColor(Xgc->private->gcol_bg.red /255.0,
	       Xgc->private->gcol_bg.green /255.0,
	       Xgc->private->gcol_bg.blue /255.0,0.0);
  glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
#endif 
}

/**
 * xget_recording:
 * @Xgc: a #BCG  
 * 
 * checks if recording is on or off in the graphic window.
 * 
 * Returns: %TRUE or %FALSE.
 **/

static int xget_recording(BCG *Xgc)
{
  return Xgc->record_flag;
}

/**
 * xset_recording:
 * @Xgc: a #BCG  
 * @val: %TRUE or %FALSE
 * 
 * sets the recoding mode to %TRUE or %FALSE 
 **/

static void xset_recording(BCG *Xgc, int val)
{
  Xgc->record_flag = (val == 0 ) ? FALSE : TRUE;
}

/**
 * xget_windowpos:
 * @Xgc: a #BCG  
 * @x: int pointer 
 * @y: int pointer 
 * 
 * to get the window upper-left point position on the 
 * screen in point coordinates. 
 **/

static void xget_windowpos(BCG *Xgc,int *x,int *y)
{
  gint xx,yy;
  gdk_window_get_position (Xgc->private->window->window,&xx,&yy);
  *x = xx; *y =yy;
}

/**
 * xset_windowpos:
 * @Xgc: a #BCG  
 * @x: an integer 
 * @y: an integer 
 * 
 * move the upper-left point position of the graphic window 
 * at position (@x,@y) on the screen.
 *
 **/

static void xset_windowpos(BCG *Xgc, int x, int y)
{
  if (Xgc == NULL || Xgc->private->window ==  NULL) initgraphic("",NULL,NULL,NULL,NULL,NULL,'e',NULL);
  gdk_window_move (Xgc->private->window->window, x,y);
}

/**
 * xget_windowdim:
 * @Xgc: a #BCG  
 * @x: an int pointer 
 * @y: an int pointer
 * 
 * set (@x,@y) to the dimension of the graphic area.
 * 
 **/

static void xget_windowdim(BCG *Xgc,int *x, int *y)
{     
  /* the two dimensions are always updated */
  *x =  Xgc->CWindowWidth;
  *y =  Xgc->CWindowHeight;
} 

/**
 * xset_windowdim:
 * @Xgc: a #BCG  
 * @x: an integer 
 * @y: an integer 
 * 
 * To change the drawbox window size. 
 * Here this function set the min size of the graphic window 
 * which means that if the scrolled window 
 * is smaller than the min size scrollbar will be drawn 
 * 
 **/

static void xset_windowdim(BCG *Xgc,int x, int y)
{
  gint pw,ph,w,h;
  if (Xgc == NULL || Xgc->private->window ==  NULL) return ;
  if ( Xgc->CurResizeStatus == 1) 
    {
      /* here drawing and scrolled move together */
      gdk_drawable_get_size (Xgc->private->window->window,&pw,&ph);
      gdk_drawable_get_size (Xgc->private->scrolled->window,&pw,&ph);
      gdk_drawable_get_size (Xgc->private->drawing->window,&w,&h);
      /* resize the graphic window */
      gdk_window_resize(Xgc->private->drawing->window,x,y);
      /* resize the main window at init time */
      /* gdk_window_resize(Xgc->private->window->window,x+Max((pw-w),0),y+Max((ph-h),0));*/
      /* resize the scrolled */
      gdk_window_resize(Xgc->private->scrolled->window,x+Max((pw-w),0),y+Max((ph-h),0));
      /* want the expose event to resize pixmap and redraw */
      Xgc->private->resize = 1; 
    }
  else
    {
      /* here drawing and scrolled do not move together 
       * the scrolled window should stay larger than the drawing 
       */
      /* inhibit_enlarge: can be changed not to allow the drawing 
       * window to be smaller than its container 
       *
       */
      int inhibit_enlarge = TRUE;
      int schrink = FALSE;
      /* gint sc_w,sc_h;*/
      GdkGeometry geometry;
      GdkWindowHints geometry_mask;
      gdk_drawable_get_size (Xgc->private->window->window,&pw,&ph);
      gdk_drawable_get_size (Xgc->private->drawing->window,&w,&h);
      if ( (Xgc->CWindowWidth > x ) || (Xgc->CWindowHeight > y )) schrink = TRUE;
      /* resize the graphic window */
      gdk_window_resize(Xgc->private->drawing->window,x,y);
      /* want the scrolled window to be aware */
      gtk_widget_set_size_request(Xgc->private->drawing, x,y);
      /* Limit the scolled window size  */
      /* gdk_drawable_get_size (Xgc->private->scrolled,&sc_w,&sc_h); */
      if ( inhibit_enlarge == TRUE ) 
	{
	  geometry.max_width = x+15;
	  geometry.max_height = y+15;
	  geometry_mask = GDK_HINT_MAX_SIZE ;
	  gtk_window_set_geometry_hints (GTK_WINDOW (Xgc->private->window), 
					 Xgc->private->scrolled,
					 &geometry, geometry_mask);
	}
      else 
	{
	  /* if window was schrinked then scrolled must follow */
	  if (  schrink == TRUE ) 
	    {
	      /* resize the main window */
	      gdk_window_resize(Xgc->private->window->window,x+Max((pw-w),0),y+Max((ph-h),0));
	    }
	}
      Xgc->CWindowWidth = x;
      Xgc->CWindowHeight = y;
      Xgc->private->resize = 1;/* be sure to put this */
      /* here we will only generate a configure event and an expose event 
       * if the size is enlarged. 
       */
      if ( schrink == FALSE && inhibit_enlarge == TRUE ) 
	{
	  expose_event( Xgc->private->drawing,NULL, Xgc);
	}
    }
  gdk_flush();
}

/**
 * xget_popupdim:
 * @Xgc: a #BCG  
 * @x: an int pointer 
 * @y: an int pointer
 * 
 * To get the popup  window size 
 **/

static void xget_popupdim(BCG *Xgc,int *x, int *y)
{ 
  gint xx,yy;
  /*   gdk_drawable_get_size (Xgc->private->window->window,&xx,&yy); */
  gdk_drawable_get_size (Xgc->private->scrolled->window,&xx,&yy);
  *x = xx ;  *y = yy ; 
} 

/**
 * xset_popupdim:
 * @Xgc: a #BCG  
 * @x: 
 * @y: 
 * 
 * To change the popup window size
 * 
 **/

static void xset_popupdim(BCG *Xgc,int x, int y)
{
  if ( Xgc->CurResizeStatus == 0) 
    {
      int w,h,pw,ph, xoff, yoff;
      gdk_drawable_get_size (Xgc->private->scrolled->window,&pw,&ph);
      gdk_drawable_get_size (Xgc->private->drawing->window,&w,&h);
      xoff = Max((pw-w),0); 
      yoff= Max((ph-h),0);
      if ( (Xgc->CWindowWidth < x - xoff  ) || (Xgc->CWindowHeight <  y - yoff )) 
	{
	  /* scrolled is larger than child we must enlarge the child 
	   * we use gtk_widget_set_size_request because if 
	   * gdk_window_resize is used the scroll bars of the scrolled window 
	   * are not properly updated.
	   */
	  /*  gdk_window_resize(Xgc->private->drawing->window,x-xoff,y-yoff);  
	   */
	  gtk_widget_set_size_request (Xgc->private->drawing,x-xoff,y-yoff);

	}
      /* for a resize of the scrolled window */
      gdk_window_resize(Xgc->private->scrolled->window,x,y);
    }
  else
    {
      /* now resize the scrolled window */
      gdk_window_resize(Xgc->private->scrolled->window,x,y);
    }
}

/**
 * xget_viewport:
 * @Xgc: a #BCG  
 * @x: an int pointer 
 * @y: an int pointer
 * 
 * To get the viewport Upper/Left point Position 
 **/

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

/**
 * xset_viewport:
 * @Xgc: a #BCG  
 * @x: an integer 
 * @y: an integer 
 * 
 * To change the window size 
 **/


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

/**
 * xset_curwin:
 * @intnum: 
 * @set_menu: 
 * 
 * select window intnum as the current window 
 * window is created if necessary 
 * return the value of the previous current window 
 * 
 * Returns: 
 **/

static int xset_curwin(int intnum,int set_menu)
{
  /* the current graphic context */
  int old;
  BCG *bcgk= window_list_get_first(),*new=NULL;
  if ( bcgk == (BCG *) 0 ) 
    {
      /* First entry or no more graphic window */
      initgraphic("",&intnum,NULL,NULL,NULL,NULL,'e',NULL);
      /* send info to menu */
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
	      initgraphic("",&intnum,NULL,NULL,NULL,NULL,'e',NULL);
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



/**
 * xget_curwin:
 * @void: 
 * 
 * Get the id number of the Current Graphic Window 
 * In all the other functions we are sure that Xgc exists 
 * when we call them ( see sciwin in matdes.f ) 
 * exept for this function which is called in sciwin and the previous one 
 * returns -1 when there's no current window 
 * 
 * Returns: 
 **/
 
static int xget_curwin(void)
{
  BCG *Xgc= window_list_get_first();
  return  ( Xgc == NULL) ? -1 : Xgc->CurWindow;
}

/* clip functions are moved to peridraw_
 *
 */

/* For the private->drawing functions dealing with vectors of 
 * points, the following routine is used to select the mode 
 * absolute or relative 
 * Absolute mode if *num==0, relative mode if *num != 0
 */

/**
 * xset_absourel:
 * @Xgc: a #BCG  
 * @flag: 
 * 
 * to set absolute or relative mode 
 **/

static void xset_absourel(BCG *Xgc,int flag)
{
  if (flag == 0 )
    Xgc->CurVectorStyle =  CoordModeOrigin;
  else 
    Xgc->CurVectorStyle =  CoordModePrevious ;
}

/**
 * xget_absourel:
 * @Xgc: a #BCG  
 * 
 * to get information on absolute or relative mode 
 *
 * Returns: 
 **/

static int xget_absourel(BCG *Xgc)
{
  return  Xgc->CurVectorStyle  ;
}



/**
 * xset_thickness:
 * @Xgc: a #BCG  
 * @value: 
 * 
 *  to set the thickness of lines : 0 is a possible value 
 *  it gives the thinest line (0 and 1 are the same for X11 but
 *  with diferent algorithms 
 *  defaut value is 1 
 **/

static void xset_thickness(BCG *Xgc,int value)
{ 
  value = Max(0, value);
  if ( Xgc->CurLineWidth == value ) return;
  Xgc->CurLineWidth  = value; 
  /* when line thickness changes we must change the dash style */
  xset_dash(Xgc,Xgc->CurDashStyle + 1);
}

/**
 * xget_thickness:
 * @Xgc: a #BCG  
 * 
 * to get the thickness value 
 * 
 * 
 * Returns: 
 **/

static int xget_thickness(BCG *Xgc)
{
  return Xgc->CurLineWidth ;
}

/* To set grey level for filing areas 
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

/**
 * xset_pattern:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * 
 * 
 * Returns: 
 **/

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

/**
 * xget_pattern:
 * @Xgc: a #BCG  
 * 
 *  To get the id of the current pattern
 * 
 * Returns: 
 **/

static int xget_pattern(BCG *Xgc)
{ 
  if ( Xgc->CurColorStatus == 1 ) 
    return Xgc->CurColor + 1;
  else 
    return Xgc->CurPattern + 1;
}

/**
 * xget_last:
 * @Xgc: a #BCG  
 * 
 * To get the id of the last pattern
 * 
 * Returns: 
 **/

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

/**
 * xset_dash:
 * @Xgc: a #BCG  
 * @value: 
 * 
 * 
 * 
 * Returns: 
 **/

static int  xset_dash(BCG *Xgc,int value)
{
  int old = xget_dash(Xgc);
  int  l2 = 4, l3;
  l3 = Max(0,Min(MAXDASH - 1,value - 1));
  xset_dashstyle(Xgc,l3,DashTab[l3],&l2);
  Xgc->CurDashStyle = l3;
  return old;
}

/**
 * xget_dash:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/
static int xget_dash(BCG *Xgc)
{
  return  Xgc->CurDashStyle + 1;
}


/**
 * xset_line_style:
 * @Xgc: a #BCG  
 * @value: 
 * 
 * 
 **/
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

/* dash syle moved to peridraw_ */


/**
 * xset_usecolor:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * 
 * 
 * Returns: 
 **/
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

/**
 * xget_usecolor:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/
static int xget_usecolor(BCG *Xgc)
{
  return  Xgc->CurColorStatus;
}

/**
 * xget_pixmapOn:
 * @Xgc: a #BCG  
 * 
 * Change the private->pixmap status of a Graphic Window. 
 * adding or removing a Background Pixmap to it 
 * 
 * Returns: 
 **/

static int xget_pixmapOn(BCG *Xgc)
{
  return Xgc->CurPixmapStatus;
}

/**
 * xset_wresize:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * Change the status of a Graphic Window
 * i.e follows or dont follow the viewport resize
 * Here the behaviour is different 
 * If we want the graphic window to follow the viewport resize 
 * (i.e we dont want to see scrollbars) then we fix the minimum 
 * size of the grahic window to very small values 
 *
 **/

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
      if ( Xgc->private->window == NULL ) return;
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
      if ( Xgc->private->drawing == NULL ) return;
      gdk_drawable_get_size (Xgc->private->drawing->window,&w,&h);
      Xgc->CurResizeStatus = num1 ;
      xset_windowdim(Xgc,w,h);
    }
}

/**
 * xget_wresize:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/
static int xget_wresize(BCG *Xgc)
{
  return Xgc->CurResizeStatus;
}

/* XXXX setting the default colormap with colors defined in color.h */

/**
 * sedeco:
 * @flag: 
 * 
 * 
 * 
 * Returns: 
 **/
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

/**
 * xset_default_colormap:
 * @Xgc: a #BCG  
 * 
 * 
 **/

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
  if ( Xgc->private->colormap == NULL && Xgc->private->drawing != NULL) 
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);
  for (i = 0; i < m; i++) {
    Xgc->private->colors[i].red = (default_colors[3*i] << 8);
    Xgc->private->colors[i].green = (default_colors[3*i+1] << 8);
    Xgc->private->colors[i].blue = (default_colors[3*i+2] << 8);
    if ( Xgc->private->colormap != NULL) 
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

/**
 * xset_colormap:
 * @Xgc: a #BCG  
 * @m: 
 * @n: 
 * @a: 
 * 
 * 
 **/

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
  if ( Xgc->private->colormap == NULL&& Xgc->private->drawing != NULL ) 
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);
  
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
      if ( Xgc->private->colormap != NULL )
	gdk_rgb_find_color (Xgc->private->colormap,&Xgc->private->colors[i]);      
    }
  set_colormap_constants(Xgc,m);
  FREE(colors_old);
}

/**
 * XgcAllocColors:
 * @xgc: 
 * @m: 
 * 
 * 
 * 
 * Returns: 
 **/

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

/**
 * set_colormap_constants:
 * @Xgc: a #BCG  
 * @m: 
 * 
 * 
 **/
static void set_colormap_constants(BCG *Xgc,int m)
{
  /* Black */
  if ( Xgc->private->colormap == NULL &&  Xgc->private->drawing != NULL ) 
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);
  Xgc->private->colors[m].red = 0; 
  Xgc->private->colors[m].green = 0;
  Xgc->private->colors[m].blue = 0;
  if (  Xgc->private->colormap != NULL ) 
    gdk_rgb_find_color (Xgc->private->colormap,&Xgc->private->colors[m]);      
  /* White */
  Xgc->private->colors[m+1].red = 65535; 
  Xgc->private->colors[m+1].green = 65535; 
  Xgc->private->colors[m+1].blue = 65535; 
  if (  Xgc->private->colormap != NULL ) 
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

/**
 * xget_colormap:
 * @Xgc: a #BCG  
 * @num: 
 * @val: 
 * @color_id: 
 * 
 * 
 * 
 * Returns: 
 **/
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


/**
 * xset_background:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * sets the background color. 
 * 
 * Returns: 
 **/

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
#ifndef PERIGL 
      /* 
       * if we change the background of the window we must change 
       * the gc ( with alufunction ) and the window background 
       * this is only used for xor mode but we are removing it.
       */
      xset_alufunction1(Xgc,Xgc->CurDrawFunction);
      if (Xgc->private->drawing!= NULL) 
	gdk_window_set_background(Xgc->private->drawing->window, &Xgc->private->gcol_bg);
#endif 
    }
}

/**
 * xget_background:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/
 
static int  xget_background(BCG *Xgc)
{ 
  return ( Xgc->CurColorStatus == 1 ) ? Xgc->NumBackground + 1 : 1;
}

/**
 * xset_foreground:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * 
 * 
 * Returns: 
 **/
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

/**
 * xget_foreground:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/
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


/**
 * xset_hidden3d:
 * @Xgc: a #BCG  
 * @num: 
 * 
 *  set and get the number of the hidden3d color 
 * which is used for backface drawing in 3d plot
 * > 0 then a color is used
 * 0 : no painting 
 * < 0 same as face color
 **/

static void xset_hidden3d(BCG *Xgc,int num)
{ 
  if (Xgc->CurColorStatus == 1) 
    {
      Xgc->NumHidden3d = Max(-2,Min(num - 1,Xgc->Numcolors + 1));
    }
}

/**
 * xget_hidden3d:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/
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


/**
 * xset_autoclear:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * 
 **/
static void xset_autoclear(BCG *Xgc,int num)
{ 
  Xgc->Autoclear = Max(0,Min(1,num));
}

/**
 * xset_autoclear_def:
 * @Xgc: a #BCG  
 * 
 * 
 **/
static void xset_autoclear_def(BCG *Xgc) 
{
  Xgc->Autoclear = 0;
}

/**
 * xget_autoclear:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/
static int xget_autoclear(BCG *Xgc)
{ 
  return  Xgc->Autoclear;
}

/**
 * xget_fpf:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/
static char *xget_fpf(BCG *Xgc)
{
  return( Xgc->fp_format);
}

/**
 * xset_fpf:
 * @Xgc: a #BCG  
 * @fmt: 
 * 
 * 
 **/
static void xset_fpf(BCG *Xgc,char *fmt) 
{
  strncpy(Xgc->fp_format,fmt,32);
}

/**
 * xset_fpf_def:
 * @Xgc: a #BCG  
 * 
 * 
 **/
static void xset_fpf_def(BCG *Xgc) 
{
  Xgc->fp_format[0]='\0';
}


/*
 * window_list management 
 */

static gint realize_event(GtkWidget *widget, gpointer data);
static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data);

/* shared by all the drivers */

#include "perigtk/events.c"  
#ifdef PERIGLGTK 
#include "perigtk/init_gtkgl.c" 
#else 
#include "perigtk/init.c" 
#endif 

/*
 * Initialisation of the graphic context. Used also 
 * to come back to the default graphic state
 */

/**
 * nsp_initialize_gc:
 * @Xgc: a #BCG  
 * 
 * 
 **/
extern void nsp_initialize_gc( BCG *Xgc ) ;

static void xset_default(BCG *Xgc)
{
  nsp_initialize_gc(Xgc);
}


/*
 * graphic widget 
 */

/* Infos 
 *  width = gdk_screen_width();
 *  gdk_screen_width_mm();
 *  height = gdk_screen_height();
 *  heightMM = gdk_screen_height_mm();
 *  gtk_widget_destroy(dd->private->window);
 *  gdk_private->pixmap_unref(dd->private->pixmap);
 *
 */

/**
 * realize_event:
 * @widget: 
 * @data: 
 * 
 * 
 * 
 * Returns: 
 **/

#if defined(PERIGTK) || defined(PERICAIRO)
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
  /* note that dd->private->gcol_bg does not exists at that point */
  /* 
   * dd->private->gcol_bg = white;
   * gdk_draw_rectangle(dd->private->drawing->window, dd->private->stdgc, TRUE, 0, 0, 
   * dd->CWindowWidth, dd->CWindowHeight);
   */
  /* gdk_window_set_background(dd->private->drawing->window, &dd->private->gcol_bg); */

  if ( dd->private->pixmap == NULL)
    {
      dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
					   dd->CWindowWidth, dd->CWindowHeight,
					   -1);
      /* 
       *  gdk_gc_set_foreground(dd->private->stdgc, &dd->private->gcol_bg);
       *  gdk_draw_rectangle(dd->private->pixmap, dd->private->stdgc, TRUE, 0, 0,
       * 	 dd->CWindowWidth, dd->CWindowHeight);
       */
    }

  /* default value is to use the background pixmap */
  dd->private->drawable= (GdkDrawable *) dd->private->pixmap;
#ifdef  PERICAIRO 
  dd->private->cairo_cr = gdk_cairo_create (dd->private->pixmap);
#endif 
  return FALSE;
}
#endif 


#ifdef PERIGL 
static void realize_event_ogl(BCG *dd );
#ifndef PERIGLGTK
static gint realize_event(GtkWidget *widget, gpointer data)
{
  BCG *Xgc = (BCG *) data;

  g_return_val_if_fail(Xgc != NULL, FALSE);
  g_return_val_if_fail(Xgc->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(Xgc->private->drawing), FALSE);
  /* we need a gc in xset_wshow */
  Xgc->private->stdgc = gdk_gc_new(Xgc->private->drawing->window);

  Xgc->private->glcontext = gtk_widget_get_gl_context (widget);
  Xgc->private->gldrawable = gtk_widget_get_gl_drawable (widget);

  if (!gdk_gl_drawable_gl_begin (Xgc->private->gldrawable, Xgc->private->glcontext))
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

#ifdef LIGHTS
  init_gl_lights(light0_pos); 
#endif
  realize_event_ogl(Xgc);
  return FALSE;
}

#else /* PERIGLGTK */
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
  if (!gdk_gl_drawable_gl_begin (dd->private->gldrawable,dd->private->glcontext))
    return FALSE;
  realize_event_ogl(dd);
  return FALSE;
}
#endif /* PERIGLGTK */


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
  /* antialiasing on or off, but when changed 
   * the effect of glLineWidth is not the same
   */
  /* glEnable(GL_LINE_SMOOTH); */
  /* glEnable(GL_BLEND); */
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  /*     glHint(GL_LINE_SMOOTH_HINT, GL_DONT_CARE); */
  /*     glLineWidth(1.5); */
  /*     glHint(GL_LINE_SMOOTH_HINT, GL_NICEST); */
  glLineWidth(1.5);
  glAlphaFunc(GL_GREATER,0.1f);
  glEnable(GL_ALPHA_TEST);
  glShadeModel(GL_SMOOTH);
  /* gdk_gl_drawable_gl_end (dd->private->gldrawable); */
}
#endif 



/**
 * configure_event:
 * @widget: 
 * @event: 
 * @data: 
 * 
 * 
 * 
 * Returns: 
 **/

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
      else 
	{
	  /* The following code is useless if 
	   * inhibit_enlarge == TRUE in xset_windowdim
	   */
	  if ( (dd->CWindowWidth < event->width) || (dd->CWindowHeight < event->height))
	    {
	      int w,h;
	      dd->CWindowWidth = event->width;
	      dd->CWindowHeight = event->height;
	      dd->private->resize = 1;
	      gdk_drawable_get_size (dd->private->drawing->window,&w,&h);
	      /* just to give the scrollbar the possibility to be updated */
	      gtk_widget_set_size_request (dd->private->drawing,w,h);
	    }
	}
    }
  return FALSE;
}


/**
 * nsp_gtk_invalidate:
 * @Xgc: a #BCG  
 * 
 * 
 **/
static void nsp_gtk_invalidate(BCG *Xgc)
{
  gdk_window_invalidate_rect(Xgc->private->drawing->window,
			     &Xgc->private->drawing->allocation,
			     FALSE);
}

/**
 * expose_event:
 * @widget: 
 * @event: 
 * @data: 
 * 
 * 
 * 
 * Returns: 
 **/

#if defined(PERIGTK) || defined(PERICAIRO)
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
       * the last gdk_draw_drawable found here is not usefull in that case. 
       */

      dd->private->resize = 0;
      if ( dd->private->pixmap) g_object_unref(G_OBJECT(dd->private->pixmap));
      dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
					   dd->CWindowWidth, dd->CWindowHeight,
					   -1);
      /* update drawable */
      if ( dd->CurPixmapStatus == 0 ) dd->private->drawable = dd->private->pixmap;
      /* fill private background with background */
      gdk_gc_set_background(dd->private->stdgc, &dd->private->gcol_bg);
      gdk_draw_rectangle(dd->private->pixmap,dd->private->stdgc, TRUE,0,0,dd->CWindowWidth, dd->CWindowHeight);
      /* On lance l'action standard de resize + redessin  */
      dd->private->in_expose= TRUE;
#ifdef PERICAIRO
      if ( dd->private->cairo_cr != NULL) cairo_destroy (dd->private->cairo_cr);
      dd->private->cairo_cr = gdk_cairo_create (dd->private->pixmap);
      scig_resize(dd->CurWindow);
      /* cairo_destroy (dd->private->cairo_cr);
	 dd->private->cairo_cr = NULL;
      */
#else 
      scig_resize(dd->CurWindow);
#endif 
      dd->private->in_expose= FALSE;
      gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
		      dd->CWindowWidth, dd->CWindowHeight);
    }
  else 
    {
      if ( dd->private->draw == TRUE ) 
	{
	  /* {static int count = 0; printf("Expose event with draw %d\n",count++);} */
	  /* need to make incremental draw */
	  dd->private->draw = FALSE;
	  dd->private->in_expose= TRUE;
	  scig_replay(dd->CurWindow);
	  dd->private->in_expose= FALSE;
	}
      else 
	{
	  /* static int count = 0; printf("Expose event without draw %d\n",count++); */
	}

      if (event  != NULL) 
	gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,
			event->area.x, event->area.y, event->area.x, event->area.y,
			event->area.width, event->area.height);
      else 
	gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
			dd->CWindowWidth, dd->CWindowHeight);
      /* if a zrect exists then add it on graphics  */
      if ( dd->zrect[2] != 0 && dd->zrect[3] != 0) 
	{
	  gdk_draw_rectangle(dd->private->drawing->window,dd->private->wgc,FALSE,
			     dd->zrect[0],dd->zrect[1],dd->zrect[2],dd->zrect[3]);
	}
    }
  gdk_flush();
  return FALSE;
}
#endif 

#ifdef PERIGL 
#ifndef PERIGLGTK 

/* periGL version */
static gint expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  BCG *dd = (BCG *) data;
  /* 
   *  GdkGLContext *glcontext = gtk_widget_get_gl_context (widget);
   *  GdkGLDrawable *gldrawable = gtk_widget_get_gl_drawable (widget);
   */
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  
  /* glLineWidth(1.5);  FIXME */ 

  if ( dd->private->resize != 0)  
    { 
      /* redraw after resize 
       */
      dd->private->resize = 0;
      if (!gdk_gl_drawable_gl_begin (dd->private->gldrawable, dd->private->glcontext)) return FALSE;
      /* glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); */
      glClear(GL_DEPTH_BUFFER_BIT);
      nsp_ogl_set_view(dd);
      dd->private->in_expose= TRUE;
      scig_resize(dd->CurWindow);
      dd->private->in_expose= FALSE;
      /* Swap buffers or flush */
      if (gdk_gl_drawable_is_double_buffered (dd->private->gldrawable))
	gdk_gl_drawable_swap_buffers (dd->private->gldrawable);
      else
	glFlush ();
      gdk_gl_drawable_gl_end (dd->private->gldrawable);
    }
  else 
    {
      /* just an expose without resizing we need to redraw  */ 
      if (!gdk_gl_drawable_gl_begin (dd->private->gldrawable, dd->private->glcontext)) return FALSE;
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
	  if ( dd->zrect[2] != 0 && dd->zrect[3] != 0) 
	    dd->graphic_engine->drawrectangle(dd,dd->zrect);
	  dd->private->in_expose= FALSE;
	  if (gdk_gl_drawable_is_double_buffered (dd->private->gldrawable))
	    gdk_gl_drawable_swap_buffers (dd->private->gldrawable);
	  else
	    glFlush ();
	}
      else 
	{
	  /* just try a swap buffer ? */
	  if (gdk_gl_drawable_is_double_buffered (dd->private->gldrawable))
	    gdk_gl_drawable_swap_buffers (dd->private->gldrawable);
	  else
	    glFlush ();
	}
      gdk_gl_drawable_gl_end (dd->private->gldrawable);
    }
  return FALSE;
}

#else 

/* periGL version */
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
       * the last gdk_draw_drawable found here is not usefull in that case. 
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
      gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
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
	gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,
			event->area.x, event->area.y, event->area.x, event->area.y,
			event->area.width, event->area.height);
      else 
	gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
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

#endif 
#endif 


/**
 * nsp_get_image:
 * @Xgc: a #BCG  
 * 
 * get drawable as an image 
 * 
 * Returns: 
 **/

#if defined(PERIGTK)
GdkImage* nsp_get_image(BCG *Xgc) 
{
  return gdk_drawable_get_image(Xgc->private->drawable,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
}
#endif 

/**
 * nsp_get_pixbuf:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/

#if defined(PERIGTK)
GdkPixbuf* nsp_get_pixbuf(BCG *Xgc) 
{
  return gdk_pixbuf_get_from_drawable(NULL,Xgc->private->drawable,
				      gdk_drawable_get_colormap(Xgc->private->drawable),
				      0,0,0,0,
				      Xgc->CWindowWidth,Xgc->CWindowHeight);
  /* gdk_colormap_get_system(), */
}
#endif 

/* 
 * basic graphics: we switch here depending on drivers 
 */ 

/*
 * include the cairo basic graphic routines 
 */

#ifdef PERICAIRO
#include "perigtk/fonts_pango_cairo.c"
#include "perigtk/peridraw_cairo_new.c"
#endif /* PERIGTK */

/*
 * include the gdk basic graphic routines 
 */

#ifdef PERIGTK
#ifdef WITH_PANGO
#include "perigtk/fonts_pango_gdk.c"
#else 
#include "perigtk/fonts_gdk.c" 
#endif 
#include "perigtk/peridraw_gdk.c"
/* routines for allocating GdkPoints */
#include "perigtk/points.c" 
#endif /* PERIGTK */

/*
 * include the opengl basic graphic routines 
 */

#ifdef PERIGL 
#include "perigtk/fonts_pango_ft2_gl.c"
#include "perigtk/peridraw_gl.c"
#endif /* PERIGL */

