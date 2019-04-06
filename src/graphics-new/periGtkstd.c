/* Nsp
 * Copyright (C) 1998-2016 Jean-Philippe Chancelier Enpc/Cermics
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
#include <signal.h>
#include <gtk/gtk.h>
#ifdef WITH_EPOXY
#include <epoxy/gl.h>
#endif
#define PERI_PRIVATE 1
#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/opengl.h"
#include "nsp/graphics-new/periGtk.h"
/* seams defined on WIN32 with gtkglext */
#ifdef interface
#undef interface
#endif
#include "nsp/version.h"
#include "nsp/graphics-new/color.h"
#include "nsp/command.h"
#include "nsp/object.h"
#include "nsp/figure.h"
#include "nsp/axes.h"
#include <nsp/gtk/gtkwindow.h>

/* #define DEBUG_GRAPHICS(x) = x */
#define DEBUG_GRAPHICS(x) 

static gint realize_event(GtkWidget *widget, gpointer data);
static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data);
static void nsp_gtk_widget_get_size(GtkWidget *widget, gint *width, gint *height);
static void nsp_set_graphic_geometry_hints(GtkWidget *widget,int x,int y);
static void nsp_configure_wait(BCG *dd);
/* static void size_allocate_event (GtkWidget *widget, GdkRectangle *allocation, gpointer data); */

#ifdef PERICAIRO
static void nsp_drawing_resize(BCG *dd,int width, int height);
#if GTK_CHECK_VERSION(3,0,0)
#ifdef PERIGTK3GL
static void xget_color_rgb(BCG *Xgc,double *rgb);
static gint resize_event(GtkWidget *widget, int width, int height, gpointer data);
static void realize_gtk3gl (GtkWidget *widget);
static void unrealize_gtk3gl (GtkWidget *widget);
static gint render_callback(GtkGLArea    *area, GdkGLContext *context,  gpointer data);
#else
static gint draw_callback(GtkWidget *widget, cairo_t *cr, gpointer data);
#endif 
static gint scrolled_draw_callback(GtkWidget *widget, cairo_t *cr, gpointer data);
static void nsp_drawing_invalidate_handler(GdkWindow *window, cairo_region_t *region);
#endif 
#endif 

#ifdef PERIGL
static void xget_color_rgb(BCG *Xgc,double *rgb);
#endif

static void nsp_get_color_rgb(BCG *Xgc,int color,double *rgb, NspMatrix *colors);

/* defined only in periCairo and shared */

extern double nsp_predef_colors[];

/**
 * invalidate:
 * @Xgc: a #BCG
 *
 * invalidate a rectangle and set the draw mode to TRUE to
 * tell to expose events that the graphic structure is to
 * be used for drawing i.e the backing store pixmap is not
 * up to date.
 *
 **/

static void invalidate(BCG *Xgc,void *rect)
{
  Xgc->private->draw = TRUE;
  if ( Xgc->private->drawing == NULL) return;
  if ( rect == NULL )
    {
      gtk_widget_get_allocation (Xgc->private->drawing,&Xgc->private->invalidated);
      gdk_window_invalidate_rect(gtk_widget_get_window(Xgc->private->drawing),
				 &Xgc->private->invalidated,
				 FALSE);
    }
  else
    {
      GdkRectangle *grect = rect;
      /*
       * keep track of the union of the invalidated rectangles (to be used
       * in expose_event.
       */
      if ( Xgc->private->invalidated.width != 0
	   && Xgc->private->invalidated.height != 0 )
	{
	  gdk_rectangle_union(grect,&Xgc->private->invalidated,&Xgc->private->invalidated);
	}
      else
	{
	  Xgc->private->invalidated = *grect;
	}
      gdk_window_invalidate_rect(gtk_widget_get_window(Xgc->private->drawing),rect, FALSE);
    }
}

/**
 * process_updates:
 * @Xgc: a #BCG
 *
 *
 **/

static void process_updates(BCG *Xgc)
{
  if ( Xgc->private->drawing == NULL) return;
  gdk_window_process_updates (gtk_widget_get_window(Xgc->private->drawing), FALSE);
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
  cairo_t *cr;
  if ( Xgc->CurPixmapStatus == 1)
    {
      /* we copy the extra_pixmap to the window and to the backing store pixmap
       */
      cr = gdk_cairo_create (gtk_widget_get_window(Xgc->private->drawing));
      cairo_set_source_surface(cr,Xgc->private->extra_pixmap,0,0);
      cairo_pattern_set_extend (cairo_get_source (cr), CAIRO_EXTEND_REPEAT);
      cairo_rectangle (cr, 0, 0, Xgc->CWindowWidth, Xgc->CWindowHeight);
      cairo_fill (cr);
      cairo_destroy (cr);
      /* copy extra_pixmap to pixmap */
      cr = cairo_create (Xgc->private->pixmap); /* pixmap is a surface */
      cairo_set_source_surface(cr,Xgc->private->extra_pixmap,0,0);
      cairo_pattern_set_extend (cairo_get_source (cr), CAIRO_EXTEND_REPEAT);
      cairo_rectangle (cr, 0, 0,Xgc->CWindowWidth, Xgc->CWindowHeight);
      cairo_fill (cr);
      cairo_destroy (cr);
    }
  else
    {
      /* if we do not have an extra pixmap
       * we just make a gdk_window_process_updates
       */
      gdk_window_process_updates (gtk_widget_get_window(Xgc->private->drawing), FALSE);
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
  if ( Xgc == (BCG *)0 || Xgc->private->window ==  NULL)
    initgraphic("",NULL,NULL,NULL,NULL,NULL,'e',NULL,NULL);
  gdk_window_show(gtk_widget_get_window(Xgc->private->window));
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
 * xget_recording:
 * @Xgc: a #BCG
 *
 * checks if recording is on or off in the graphic window.
 *
 * Returns: %TRUE or %FALSE.
 **/

static int xget_recording(BCG *Xgc)
{
  return TRUE;
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
  /* Xgc->record_flag = (val == 0 ) ? FALSE : TRUE; */
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
  gdk_window_get_position (gtk_widget_get_window(Xgc->private->window),&xx,&yy);
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
  if (Xgc == NULL || Xgc->private->window ==  NULL)
    initgraphic("",NULL,NULL,NULL,NULL,NULL,'e',NULL,NULL);
  gdk_window_move (gtk_widget_get_window(Xgc->private->window), x,y);
}

/**
 * xget_windowdim:
 * @Xgc: a #BCG
 * @width: an int pointer
 * @height: an int pointer
 *
 * set (@x,@y) to the dimension of the graphic area.
 *
 **/

static void xget_windowdim(BCG *Xgc,int *width, int *height)
{
  /* do not use 
   * nsp_gtk_widget_get_size (Xgc->private->drawing,width,height);
   * since when exporting Xgc->private->drawing may be null 
   */
  *width= Xgc->CWindowWidth;
  *height= Xgc->CWindowHeight;
#if 0
  {
    int w,h;
    nsp_gtk_widget_get_size (Xgc->private->window,&w,&h);
    Sciprintf("window (%d,%d)\n",w,h);
    nsp_gtk_widget_get_size (Xgc->private->vbox,&w,&h);
    Sciprintf("vbox (%d,%d)\n",w,h);
    nsp_gtk_widget_get_size (Xgc->private->scrolled,&w,&h);
    Sciprintf("scrolled (%d,%d)\n",w,h);
    nsp_gtk_widget_get_size (GTK_WIDGET(gtk_scrolled_window_get_hscrollbar(GTK_SCROLLED_WINDOW(Xgc->private->scrolled))),&w,&h);
    Sciprintf("scrolled adjustment (%d,%d)\n",w,h);
    nsp_gtk_widget_get_size (Xgc->private->drawing,&w,&h);
    Sciprintf("drawing (%d,%d)\n",w,h);
  }
#endif
}

/**
 * nsp_gtk_widget_get_size:
 * @widget: a #GtkWidget
 * @width: a gint pointer 
 * @height: a gint pointer 
 * 
 * returns in @width and @height the allocated sizes of the given 
 * widget.
 **/

static void nsp_gtk_widget_get_size(GtkWidget *widget, gint *width, gint *height)
{
#if GTK_CHECK_VERSION(3,0,0)
  *width = gtk_widget_get_allocated_width (widget);
  *height = gtk_widget_get_allocated_height (widget);
#else
  *width = widget->allocation.width;
  *height = widget->allocation.height;
#endif
}

/**
 * nsp_remove_hints:
 * @Xgc: a #BCG
 *
 * remove hints on the graphic widgets associated to @Xgc.
 **/

static void nsp_remove_hints(BCG *Xgc,int width,int height)
{
  DEBUG_GRAPHICS(Sciprintf("nsp_remove_hints size=(%d,%d)",width,height));
  Xgc->private->configured = TRUE;
  gtk_widget_set_size_request (Xgc->private->window,-1,-1);
  gtk_window_set_geometry_hints (GTK_WINDOW (Xgc->private->window),
				 Xgc->private->window,
				 NULL,0);
  if ( Xgc->CurResizeStatus == 1 ) 
    {
      DEBUG_GRAPHICS(Sciprintf(" drawing hints removed "));
      gtk_widget_set_size_request (Xgc->private->drawing,-1,-1);
      /* not a  GTK_WINDOW
      gtk_window_set_geometry_hints (GTK_WINDOW (Xgc->private->drawing),
				     Xgc->private->drawing,
				     NULL,0);
      */
    }
  else
    {
      int w= width;
      int h= height;
      DEBUG_GRAPHICS(Sciprintf(" drawing hints=(%d,%d)", w,h));
      gtk_widget_set_size_request (Xgc->private->drawing,w,h);
      nsp_set_graphic_geometry_hints(Xgc->private->drawing,w,h);
    }
  gtk_widget_set_size_request (Xgc->private->scrolled,-1,-1);
  /* 
  gtk_window_set_geometry_hints (GTK_WINDOW (Xgc->private->scrolled),
				 Xgc->private->scrolled,
				 NULL,0);
  */
  DEBUG_GRAPHICS(Sciprintf(" quit\n"));
}

/**
 * nsp_set_geometry_hints:
 * @Xgc: a #BCG
 * @x: an integer
 * @y: an integer
 *
 * force the size of the main widget to (@x,@y)
 * by setting min and max hints.
 **/

static void _nsp_set_geometry_hints(GtkWidget *widget,GdkWindowHints geometry_mask,int x,int y)
{
  GdkGeometry geometry;
  geometry.max_width = 	geometry.min_width = x;
  geometry.max_height =	geometry.min_height = y;
  gtk_window_set_geometry_hints (GTK_WINDOW(widget),widget,&geometry, geometry_mask);
}

static void nsp_set_geometry_hints(BCG *Xgc,int x,int y)
{
  int w,h;
  nsp_gtk_widget_get_size (Xgc->private->window,&w,&h);
  if ( w != x || h != y)
    {
      /* it is important here to call this function 
       * only if the sizes are to be changed 
       * because the hints will be removed by a configure event 
       * and no configure event will be generated if the size is not changed
       */
      GdkWindowHints geometry_mask= GDK_HINT_MAX_SIZE |GDK_HINT_MIN_SIZE ;
      DEBUG_GRAPHICS(Sciprintf("fix hints on window (%d,%d)->(%d,%d)\n",w,h,x,y));
      _nsp_set_geometry_hints(Xgc->private->window,geometry_mask,x,y);
    }
}
/**
 * nsp_set_graphic_geometry_hints:
 * @Xgc: a #BCG
 * @x: an integer
 * @y: an integer
 *
 * force the size of the graphic widget to min sizes (@x,@y)
 * by setting min hints.
 **/

static void nsp_set_graphic_geometry_hints(GtkWidget *widget,int x,int y)
{
  /* GdkWindowHints geometry_mask= GDK_HINT_MIN_SIZE ;*/
  DEBUG_GRAPHICS(Sciprintf("fix min hints on graphic (%d,%d)\n",x,y));
  /* not a GTK_WINDOW  
  _nsp_set_geometry_hints(widget,geometry_mask,x,y);
  */
  /* 
     gtk_window_resize(GTK_WINDOW(widget),x,y);
     gtk_window_set_resizable (GTK_WINDOW(widget),FALSE);
  */
}

/**
 * xset_windowdim:
 * @Xgc: a #BCG
 * @x: an integer
 * @y: an integer
 *
 * Changes the drawing window size to (@x,@y)
 * Here this function set the min size of the graphic window
 * which means that if the scrolled window
 * is smaller than the min size scrollbar will be drawn.
 *
 **/

static void xset_windowdim(BCG *Xgc,int x, int y)
{
  gint pw,ph,w,h,sw,sh;
  if (Xgc == NULL || Xgc->private->window ==  NULL) return ;
  if ( Xgc->CurResizeStatus == 1 )
    {
      /* here drawing and scrolled have the same size */
      nsp_gtk_widget_get_size (Xgc->private->window,&pw,&ph);
      nsp_gtk_widget_get_size (Xgc->private->scrolled,&w,&h);
      if ( w != x || h != y )
	{
	  gtk_widget_set_size_request(Xgc->private->drawing, x,y);
	  gtk_widget_set_size_request(Xgc->private->scrolled, x,y);
	  nsp_set_geometry_hints(Xgc,x + (pw-w), y+ (ph-h));
	  Xgc->CWindowWidth = x;
	  Xgc->CWindowHeight = y;
	  Xgc->private->resize = 1;
	  nsp_configure_wait(Xgc);
	}
    }
  else
    {
      /* here drawing and scrolled do not move together
       * the scrolled window should stay smaller than the drawing
       */
      int hw,hh,vw,vh;
      GtkWidget *hscrollbar= gtk_scrolled_window_get_hscrollbar(GTK_SCROLLED_WINDOW (Xgc->private->scrolled));
      GtkWidget *vscrollbar= gtk_scrolled_window_get_vscrollbar(GTK_SCROLLED_WINDOW (Xgc->private->scrolled));
      nsp_gtk_widget_get_size (Xgc->private->window,&pw,&ph);
      nsp_gtk_widget_get_size (Xgc->private->scrolled,&sw,&sh);
      nsp_gtk_widget_get_size (hscrollbar,&hw,&hh);
      nsp_gtk_widget_get_size (vscrollbar,&vw,&vh);
      nsp_gtk_widget_get_size (Xgc->private->drawing,&w,&h);
      if ( w != x || h != y )
	{
	  /* resize the graphic window to (x,y) 
	   * even if the size is already correct then we will have a configure
	   */
	  gtk_widget_set_size_request(Xgc->private->drawing, x,y);
	  nsp_set_graphic_geometry_hints(Xgc->private->drawing,x,y);
	  /* the scrolled window should be Min(x+vw,sw)xMin(y+hh,sh) */
	  gtk_widget_set_size_request(Xgc->private->scrolled, Min(x+vw,sw),Min(y+hh,sh));
	  /* set hints on the main window */
	  nsp_set_geometry_hints(Xgc, Min(x+vw,sw) + (pw-sw), Min(y+hh,sh) + (ph-sh));
	  Xgc->CWindowWidth = x;
	  Xgc->CWindowHeight = y;
	  Xgc->private->resize = 1;
	  nsp_configure_wait(Xgc);
	}
    }
}

/**
 * xget_popupdim:
 * @Xgc: a #BCG
 * @x: an int pointer
 * @y: an int pointer
 *
 * To get the popup  window size (In fact the scrolled window)
 **/

static void xget_popupdim(BCG *Xgc,int *x, int *y)
{
  nsp_gtk_widget_get_size (Xgc->private->scrolled,x,y);
}

/**
 * xset_popupdim:
 * @Xgc: a #BCG
 * @x:
 * @y:
 *
 * To change the popup window size in fact the scrolled 
 *
 **/

static void xset_popupdim(BCG *Xgc,int x, int y)
{
  GtkWidget *hscrollbar;
  GtkWidget *vscrollbar;
  int w,h,pw,ph,hw,hh,vw,vh,dw,dh;
  if (Xgc == NULL || Xgc->private->window ==  NULL) return ;
  switch  ( Xgc->CurResizeStatus ) 
    {
    case 1:  /* scrolled and graphics have the same dimensions and no-scrolled bars */
      xset_windowdim(Xgc,x,y);
      break;
    case 0:
    case 2:
      hscrollbar= gtk_scrolled_window_get_hscrollbar(GTK_SCROLLED_WINDOW (Xgc->private->scrolled));
      vscrollbar= gtk_scrolled_window_get_vscrollbar(GTK_SCROLLED_WINDOW (Xgc->private->scrolled));
      nsp_gtk_widget_get_size (Xgc->private->window,&pw,&ph);
      nsp_gtk_widget_get_size (Xgc->private->scrolled,&w,&h);
      nsp_gtk_widget_get_size (Xgc->private->drawing,&dw,&dh);
      nsp_gtk_widget_get_size (hscrollbar,&hw,&hh);
      nsp_gtk_widget_get_size (vscrollbar,&vw,&vh);
      if ( x != w || y != h ) 
	{
	  int dw1 = Max(x-vw,dw), dh1 = Max(y -hh,dh);
/* #define SEND_CONFIGURE  */
#if ! defined(SEND_CONFIGURE)
	  /* be sure that drawing window size changes to generates a configure  */
	  if ( dw1 == dw && dh1 == dh ) {dw1 +=1;dh1 +=1;}
#endif 
	  nsp_set_graphic_geometry_hints(Xgc->private->drawing,dw1, dh1);
	  gtk_widget_set_size_request(Xgc->private->drawing, dw1,dh1);
	  gtk_widget_set_size_request(Xgc->private->scrolled, x,y);
	  /* we force (x,y) sizes for scrolled by giving hints on the main widget  */
	  nsp_set_geometry_hints(Xgc, x + (pw-w), y+(ph-h));
	  DEBUG_GRAPHICS(Sciprintf("fix window to (%d,%d)\n", x + (pw-w), y+(ph-h)));
	  Xgc->CWindowWidth =  dw1;
	  Xgc->CWindowHeight = dh1;
	  Xgc->private->resize = 1;
#if ! defined(SEND_CONFIGURE)
	  nsp_configure_wait(Xgc);
#else 
	  {
	    /* we directly send a configure */
	    GdkEvent *event = gdk_event_new (GDK_CONFIGURE);
	    GtkAllocation allocation;
	    gtk_widget_get_allocation (Xgc->private->drawing, &allocation);
	    event->configure.window = g_object_ref (gtk_widget_get_window(Xgc->private->drawing));
	    event->configure.send_event = TRUE;
	    event->configure.x = allocation.x;
	    event->configure.y = allocation.y;
	    event->configure.width = allocation.width;
	    event->configure.height = allocation.height;
	    gtk_widget_event (Xgc->private->drawing, event);
	    gdk_event_free (event);
	    nsp_check_gtk_events();
	  }
#endif 
	}
      break;
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
  if ( Xgc->CurResizeStatus == 0 || Xgc->CurResizeStatus == 2 )
    {
      /* get the horizontal and vertival adjustments of the ? */
      H = gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (Xgc->private->scrolled));
      V = gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (Xgc->private->scrolled));
      *x = gtk_adjustment_get_value(H);
      *y = gtk_adjustment_get_value(V);
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
 * To change the drawing area position inside the 
 * scrolled window. 
 * If x or y is -1 we center the drawing area 
 **/

static void xset_viewport(BCG *Xgc,int x, int y)
{
  if ( Xgc->CurResizeStatus == 0 || Xgc->CurResizeStatus == 2)
    {
      GtkScrolledWindow *scrolled_window = GTK_SCROLLED_WINDOW (Xgc->private->scrolled);
      GtkAdjustment *hadjustment = gtk_scrolled_window_get_hadjustment(scrolled_window);
      GtkAdjustment *vadjustment = gtk_scrolled_window_get_vadjustment(scrolled_window);
      if ( x == -1 || y == -1 )
	{
	  gdouble hpage_size, vpage_size;
	  hpage_size = gtk_adjustment_get_page_size (hadjustment);
	  vpage_size = gtk_adjustment_get_page_size (vadjustment);
	  gtk_adjustment_set_value( hadjustment,(gfloat) (Xgc->CWindowWidth - hpage_size)/2);
	  gtk_adjustment_set_value( vadjustment,(gfloat) (Xgc->CWindowHeight - vpage_size)/2);
	}
      else 
	{
	  gtk_adjustment_set_value( hadjustment, (gfloat) x);
	  gtk_adjustment_set_value( vadjustment, (gfloat) y);
	}
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
      initgraphic("",&intnum,NULL,NULL,NULL,NULL,'e',NULL,NULL);
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
	      initgraphic("",&intnum,NULL,NULL,NULL,NULL,'e',NULL,NULL);
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
      if (! gtk_widget_get_realized(new->private->drawing))
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

static int xset_thickness(BCG *Xgc,int value)
{
  int old = Xgc->CurLineWidth;
  value = Max(0, value);
  if ( Xgc->CurLineWidth == value ) return value;
  Xgc->CurLineWidth  = value;
  /* when line thickness changes we must change the dash style */
  xset_dash(Xgc,Xgc->CurDashStyle + 1);
  return old ;
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
 * nsp_get_color_rgb:
 * @Xgc:
 * @color:
 * @rgb:
 * @colors:
 *
 *
 **/

static void nsp_get_color_rgb(BCG *Xgc,int color,double *rgb, NspMatrix *colors)
{
  if ( color <= colors->m )
    {
      rgb[0]= colors->R[color-1];
      rgb[1]= colors->R[color-1+colors->m];
      rgb[2]= colors->R[color-1+2*colors->m];
    }
  else
    {
      color -= colors->m +1 ;
      color  = Min(8,color);
      rgb[0] = nsp_predef_colors[3*color];
      rgb[1] = nsp_predef_colors[3*color+1];
      rgb[2] = nsp_predef_colors[3*color+2];
    }
}


/**
 * xget_color:
 * @Xgc: a #BCG
 *
 *  To get the id of the current pattern
 *
 * Returns:
 **/

static int xget_color(BCG *Xgc)
{
  return Xgc->CurColor;
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
      return Xgc->IDLastPattern;
    }
  else
    {
      return Xgc->IDLastPattern;
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
    xset_color(Xgc,value);
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
	  xset_color(Xgc,1);
	  /* go to b&w */
	  Xgc->CurColorStatus = 0;
	  i= Xgc->CurPattern + 1;
	  xset_color(Xgc,i);
	  i= Xgc->CurDashStyle + 1;
	  xset_dash(Xgc,i);
	  Xgc->IDLastPattern = GREYNUMBER;
	}
      else
	{
	  /* switching to color mode */
	  /* patterns and dashes reinitialization */
	  /* colors too */
	  Xgc->CurColorStatus = 0;
	  xset_color(Xgc,1);
	  xset_dash(Xgc,1);
	  /* switching to color mode */
	  Xgc->CurColorStatus = 1;
	  i= Xgc->CurColor + 1;
	  xset_color(Xgc,i);
	  xset_color(Xgc,i);
	  Xgc->IDLastPattern = Xgc->Numcolors;
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
  int w,h,sw,sh;
  int num1= Min(Max(num,0),2);
  switch ( Xgc->CurResizeStatus )
    {
    case 0:
    case 2:
      switch (num1) 
	{
	case 0: break;
	case 1: 
	  nsp_gtk_widget_get_size (Xgc->private->drawing,&w,&h);
	  nsp_gtk_widget_get_size (Xgc->private->scrolled,&sw,&sh);
	  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (Xgc->private->scrolled),
					  GTK_POLICY_NEVER, GTK_POLICY_NEVER);
	  Xgc->CurResizeStatus = 1;
	  /* 
	  gtk_window_set_geometry_hints (GTK_WINDOW (Xgc->private->drawing),
					 Xgc->private->drawing,
					 NULL,0);
	  */
	  /* adding 1 to force a configure event */
	  xset_windowdim(Xgc,w+1,h+1);
	  break;
	case 2: break;
	}
      break;
    case 1:
      switch (num1) 
	{
	case 1: break;
	case 0: 
	case 2: 
	  nsp_gtk_widget_get_size (Xgc->private->drawing,&w,&h);
	  nsp_gtk_widget_get_size (Xgc->private->scrolled,&sw,&sh);
	  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (Xgc->private->scrolled),
					  GTK_POLICY_ALWAYS, GTK_POLICY_ALWAYS);
	  Xgc->CurResizeStatus = num1;
	  /* adding 1 to force a configure event */
	  xset_windowdim(Xgc,w+1,h+1);
	  break;
	}
      break;
    }
  Xgc->CurResizeStatus = num1;
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


/**
 * sedeco:
 * @flag:
 *
 * setting the default colormap with colors defined in color.h
 *
 *
 * Returns:
 **/
static int set_default_colormap_flag = 1;

static void sedeco(int flag)
{
  set_default_colormap_flag = flag;
}


/**
 * xset_default_colormap:
 * @Xgc: a #BCG
 *
 * set_default_colormap is called when raising a window for the first
 * time by xset('window',...) or by getting back to default by
 * xset('default',...)
 *
 **/

static int xset_default_colormap(BCG *Xgc)
{
  int i,m= DEFAULTNUMCOLORS;
  NspMatrix *colors;
  /*  we don't want to set the default colormap at window creation
   *  if the command was xset("colormap");
   */
  if ( Xgc->CmapFlag == 1 ) return OK ; /* default colormap already */
  if (set_default_colormap_flag == 0) return OK;

  /* Allocate a new matrix for storing the default colors
   * don't forget to add three colors at the end black, white, gray
   */

  if ((colors = nsp_matrix_create("colors",'r',m,3))== NULL)
    {
      return FAIL;
    }
  else
    {
      if ( Xgc->private->a_colors != NULL)
	nsp_matrix_destroy(Xgc->private->a_colors);
      Xgc->private->a_colors = colors;
    }
  /* just in case: initialize the colormap */
  /*
  if ( Xgc->private->colormap == NULL && Xgc->private->drawing != NULL)
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);
  */
  for (i = 0; i < m; i++) {
    colors->R[i] = (default_colors[3*i]/(double) 255);
    colors->R[i+ colors->m] = (default_colors[3*i+1]/(double) 255);
    colors->R[i+2*colors->m] = (default_colors[3*i+2]/(double) 255);
  }
  nsp_set_colormap_constants(Xgc,m);
  return OK;
}


/**
 * xset_colormap:
 * @Xgc: a #BCG
 * @a: should be a #NspMatrix
 *
 * a must be a m x 3 double RGB matrix:
 * a[i] = RED
 * a[i+m] = GREEN
 * a[i+2*m] = BLUE
 *
 * Returns: %OK or %FAIL
 **/

static int xset_colormap(BCG *Xgc,void *a)
{
  NspMatrix *colors, *A = (NspMatrix *) a;
  int i ;
  /* 3 colors reserved for black and white and gray */
  if ( A->n != 3 || A->m  < 0 )
    {
      Scierror("Error: a colormap must be a m x 3 array of doubles\n");
      return FAIL;
    }
  /* Checking RGB values */
  for (i = 0; i < A->m; i++)
    {
      if (A->R[i] < 0 || A->R[i] > 1 || A->R[i+A->m] < 0 || A->R[i+A->m] > 1 ||
	  A->R[i+2*A->m] < 0 || A->R[i+2*A->m]> 1)
	{
	  Scierror("Error: RGB values must be between 0 and 1\n");
	  return FAIL;
	}
    }

  /* Allocate a new matrix for storing the default colors */
  if ((colors = nsp_matrix_create("colors",'r',A->m,3))== NULL)
    {
      return FAIL;
    }
  else
    {
      if ( Xgc->private->a_colors != NULL)
	nsp_matrix_destroy(Xgc->private->a_colors);
      Xgc->private->a_colors = colors;
    }
  /* just in case: initialize the colormap */
  /*
  if ( Xgc->private->colormap == NULL && Xgc->private->drawing != NULL)
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);
  */
  for (i = 0; i < A->m; i++) {
    colors->R[i] = A->R[i];
    colors->R[i+ colors->m] =A->R[i+A->m];
    colors->R[i+2*colors->m] =A->R[i+2*A->m];
  }
  nsp_set_colormap_constants(Xgc,A->m);
  return OK;
}

/**
 * xget_colormap:
 * @Xgc: a #BCG
 * @num:
 * @val:
 * @color_id:
 *
 *
 * Returns:
 **/

static void xget_colormap(BCG *Xgc, int *num,  double *val,int color_id)
{
  NspMatrix *colors = Xgc->private->a_colors;
  int m = Xgc->Numcolors,  i;
  NspFigure *F = Xgc->figure;
  NspFigureData *Gc = NULL;
  if ( F != NULL )
    {
      Gc = F->obj->gc;
      if ( Gc->colormap != NULL && Gc->colormap->mn != 0 )
	{
	  colors= Gc->colormap;
	  m = colors->m;
	}
    }
  *num = m;
  if ( val == NULL ) return;
  if ( color_id != 0 )
    {
      /* just return one color: remember that we have
       * extra predef colors at the end and that
       * colors start at 1 for nsp_get_color_rgb
       */
      nsp_get_color_rgb(Xgc,color_id,val,colors);
    }
  else
    {
      /* get all colors */
      for (i = 0; i < m; i++)
	{
	  val[i] = colors->R[i];
	  val[i+m] =  colors->R[i+colors->m];
	  val[i+2*m] = colors->R[i+2*colors->m];
	}
    }
}

/**
 * xpush_colormap:
 * @Xgc: a #BCG
 *
 * Returns:
 **/

static int xpush_colormap(BCG *Xgc,void *colors)
{
  if ( ((NspMatrix *)colors)->mn == 0) return FAIL;
  if ( Xgc->private->q_colors == NULL)
    Xgc->private->q_colors = g_queue_new();
  if ( Xgc->private->q_colors == NULL) return FAIL;
  g_queue_push_head( Xgc->private->q_colors,Xgc->private->a_colors);
  Xgc->private->a_colors = colors ;
  nsp_set_colormap_constants(Xgc,((NspMatrix *) colors)->m);
  return OK;
}

/**
 * xpop_colormap:
 * @Xgc: a #BCG
 *
 * Returns:
 **/

static int xpop_colormap(BCG *Xgc)
{
  NspMatrix *C = g_queue_pop_head(Xgc->private->q_colors);
  if ( C == NULL ) return FAIL;
  Xgc->private->a_colors = C;
  nsp_set_colormap_constants(Xgc, C->m);
  return OK;
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
  const int predef =9; /* see nsp_predef_colors */
  if (Xgc->CurColorStatus == 1)
    {
      double rgb[3];
      Xgc->NumBackground =  Max(1,Min(num,Xgc->Numcolors+predef));
      nsp_get_color_rgb(Xgc,Xgc->NumBackground,rgb,Xgc->private->a_colors);
      Xgc->private->gcol_bg.red   = (guint16)  (rgb[0]*65535);
      Xgc->private->gcol_bg.green = (guint16)  (rgb[1]*65535);
      Xgc->private->gcol_bg.blue  = (guint16)  (rgb[2]*65535);
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
  return ( Xgc->CurColorStatus == 1 ) ? Xgc->NumBackground: 1;
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

static void xset_foreground(BCG *Xgc,int num)
{
  const int predef =9; /* see nsp_predef_colors */
  if (Xgc->CurColorStatus == 1)
    {
      double rgb[3];
      Xgc->NumForeground =  Max(1,Min(num,Xgc->Numcolors+predef));
      nsp_get_color_rgb(Xgc,Xgc->NumForeground,rgb,Xgc->private->a_colors);
      Xgc->private->gcol_fg.red   = (guint16)  (rgb[0]*65535);
      Xgc->private->gcol_fg.green = (guint16)  (rgb[1]*65535);
      Xgc->private->gcol_fg.blue  = (guint16)  (rgb[2]*65535);
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
  return ( Xgc->CurColorStatus == 1 ) ? Xgc->NumForeground: 1;
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
      Xgc->NumHidden3d = Max(-2,Min(num -1,Xgc->Numcolors + 1));
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
  Xgc->fp_format[31]='\0';
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


/* shared by all the drivers */

#include "perigtk/events.c"
#include "perigtk/actions.c"
#include "perigtk/init.c"

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

static gint realize_event_common(GtkWidget *widget, gpointer data)
{
  BCG *dd = (BCG *) data;

  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  dd->private->gcursor = gdk_cursor_new(GDK_CROSSHAIR);
  dd->private->ccursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);
  dd->private->extra_cursor = NULL;
  gdk_window_set_cursor(gtk_widget_get_window(dd->private->drawing), dd->private->ccursor);
  return TRUE;
}

/**
 * realize_event:
 * @widget:
 * @data:
 *
 *
 *
 * Returns:
 **/

#if defined(PERICAIRO)
static gint realize_event(GtkWidget *widget, gpointer data)
{
  BCG *Xgc = (BCG *) data;
  if ( realize_event_common(widget,data) == FALSE ) return FALSE;
  {
    int width, height;
    nsp_gtk_widget_get_size (Xgc->private->drawing,&width,&height);
    DEBUG_GRAPHICS(Sciprintf("In realize (%d,%d) and (%d,%d)\n",
			     width,height, Xgc->CWindowWidth, Xgc->CWindowHeight));
  }
  nsp_drawing_resize(Xgc, Xgc->CWindowWidth, Xgc->CWindowHeight);
  return FALSE;
}
#endif


#ifdef PERIGL
static void realize_event_ogl(BCG *dd );

static gint realize_event(GtkWidget *widget, gpointer data)
{
  BCG *Xgc = (BCG *) data;
  if ( realize_event_common(widget,data) == FALSE ) return FALSE;

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

  xset_background(Xgc,Xgc->NumBackground);

#ifdef LIGHTS
  init_gl_lights(light0_pos);
#endif
  realize_event_ogl(Xgc);

  gdk_gl_drawable_gl_end (Xgc->private->gldrawable);

  return FALSE;
}

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
}
#endif

/*
static void size_allocate_event (GtkWidget *widget, GdkRectangle *allocation, gpointer user_data)
{
  Sciprintf("A size allocate: (%d,%d)\n",allocation->width,allocation->height);
}
*/

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
  int width, height;
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  /* check for resize */
  if( ! gtk_widget_get_realized(dd->private->drawing) ) return FALSE;
  if ( dd->CurResizeStatus == -1)
    {
      /* before initialization */
      return FALSE ;
    }
  nsp_gtk_widget_get_size (dd->private->drawing,&width,&height);
  DEBUG_GRAPHICS(Sciprintf("In drawing configure: wdim (%d,%d), get_size=(%d,%d) event=(%d,%d)\n",
			   dd->CWindowWidth, dd->CWindowHeight,
			   width,height, 
			   event->width,event->height));
  nsp_remove_hints(dd,width,height);
  
  if ( dd->CurResizeStatus == 2 )
    {
      if (0 )
	{
	  /* The next command when activated during a nsp_gtk_main() loop
	   * (for example while in a xclick() or xgetmouse() will activate
	   * the timeout_menu_check function in perigtk/event.c
	   * and thus will produce a call to nsp_gtk_main_quit.
	   * Unfortunately, on windows (at least with the version of gtk we use)
	   * the  nsp_gtk_main_quit call will not produce an exit from nsp_gtk_main
	   * while the graphic window is enlarged. That's why we have the next
	   * hack and do not use the enqueue_nsp_command.
	   */
	  enqueue_nsp_command("redraw_requested");
	}
      else
	{
	  /* This is a hack for scicos that should possibly be moved elsewhere.
	   * when using  dd->CurResizeStatus == 2 we do not want the
	   * graphic to be scaled when enlarging the drawing area. Thus
	   * the scales are changed in order to take care of a larger
	   * drawing area without scaling the graphics.
	   *
	   */
	  if (dd->figure != NULL)
	    {
	      NspObject *Obj;
	      if ( nsp_list_length(((NspFigure *)dd->figure)->obj->children) > 0 
		   && (Obj = nsp_list_get_element(((NspFigure *)dd->figure)->obj->children,1)) !=  NULLOBJ )
		{
		  if ( IsAxes(Obj))
		    {
		      double w1,w2, h1, h2, dw,dh;
		      NspAxes *Axes = (NspAxes *) Obj;
		      scale_i2f(&Axes->obj->scale,&w1,&h1,&dd->CWindowWidth, &dd->CWindowHeight,1);
		      scale_i2f(&Axes->obj->scale,&w2,&h2, &event->width,  &event->height, 1);
		      Axes->obj->arect->R[0] = Axes->obj->arect->R[0] * dd->CWindowWidth/event->width;
		      Axes->obj->arect->R[1] = Axes->obj->arect->R[1] * dd->CWindowWidth/event->width;
		      Axes->obj->arect->R[2] = Axes->obj->arect->R[2] * dd->CWindowHeight/event->height;
		      Axes->obj->arect->R[3] = Axes->obj->arect->R[3] * dd->CWindowHeight/event->height;
		      Axes->obj->frect->R[2] += (dw=w2-w1,dw);
		      Axes->obj->rect->R[2]  += dw;
		      Axes->obj->frect->R[1] += (dh=h2-h1,dh);
		      Axes->obj->rect->R[1]  += dh;
		    }
		}
	    }
	}
    }

  dd->CWindowWidth = event->width;
  dd->CWindowHeight = event->height;
  /* alert draw that the drawing window was resized */
  dd->private->resize = 1;

  return FALSE;
}


#if GTK_CHECK_VERSION(3,0,0)
#ifdef PERIGTK3GL

/* configure_event is not called on gtk_gl_area 
 * we use the resize event 
 */

static gint resize_event(GtkWidget *widget, int width, int height, gpointer data)
{
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  /* g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE); */
  /* check for resize */
  if( ! gtk_widget_get_realized(dd->private->drawing) ) return FALSE;
  if ( dd->CurResizeStatus == -1)
    {
      /* before initialization */
      return FALSE ;
    }
  nsp_gtk_widget_get_size (dd->private->drawing,&width,&height);
  DEBUG_GRAPHICS(Sciprintf("In drawing configure: wdim (%d,%d), get_size=(%d,%d) event=(%d,%d)\n",
			   dd->CWindowWidth, dd->CWindowHeight,
			   width,height, 
			   event->width,event->height));
  nsp_remove_hints(dd,width,height);
  
  if ( dd->CurResizeStatus == 2 )
    {
      if (0 )
	{
	  /* The next command when activated during a nsp_gtk_main() loop
	   * (for example while in a xclick() or xgetmouse() will activate
	   * the timeout_menu_check function in perigtk/event.c
	   * and thus will produce a call to nsp_gtk_main_quit.
	   * Unfortunately, on windows (at least with the version of gtk we use)
	   * the  nsp_gtk_main_quit call will not produce an exit from nsp_gtk_main
	   * while the graphic window is enlarged. That's why we have the next
	   * hack and do not use the enqueue_nsp_command.
	   */
	  enqueue_nsp_command("redraw_requested");
	}
      else
	{
	  /* This is a hack for scicos that should possibly be moved elsewhere.
	   * when using  dd->CurResizeStatus == 2 we do not want the
	   * graphic to be scaled when enlarging the drawing area. Thus
	   * the scales are changed in order to take care of a larger
	   * drawing area without scaling the graphics.
	   *
	   */
	  if (dd->figure != NULL)
	    {
	      NspObject *Obj;
	      if ( nsp_list_length(((NspFigure *)dd->figure)->obj->children) > 0 
		   && (Obj = nsp_list_get_element(((NspFigure *)dd->figure)->obj->children,1)) !=  NULLOBJ )
		{
		  if ( IsAxes(Obj))
		    {
		      double w1,w2, h1, h2, dw,dh;
		      NspAxes *Axes = (NspAxes *) Obj;
		      scale_i2f(&Axes->obj->scale,&w1,&h1,&dd->CWindowWidth, &dd->CWindowHeight,1);
		      scale_i2f(&Axes->obj->scale,&w2,&h2, &width,  &height, 1);
		      Axes->obj->arect->R[0] = Axes->obj->arect->R[0] * dd->CWindowWidth/width;
		      Axes->obj->arect->R[1] = Axes->obj->arect->R[1] * dd->CWindowWidth/width;
		      Axes->obj->arect->R[2] = Axes->obj->arect->R[2] * dd->CWindowHeight/height;
		      Axes->obj->arect->R[3] = Axes->obj->arect->R[3] * dd->CWindowHeight/height;
		      Axes->obj->frect->R[2] += (dw=w2-w1,dw);
		      Axes->obj->rect->R[2]  += dw;
		      Axes->obj->frect->R[1] += (dh=h2-h1,dh);
		      Axes->obj->rect->R[1]  += dh;
		    }
		}
	    }
	}
    }

  dd->CWindowWidth = width;
  dd->CWindowHeight = height;
  /* alert draw that the drawing window was resized */
  dd->private->resize = 1;
  return FALSE;
}
#endif
#endif

/* wait for a configure event in the graphic window */

static gint timeout_configured (void *data)
{
  BCG *dd = (BCG *) data;
  if ( dd->private->configured == TRUE ) 
    {
      DEBUG_GRAPHICS(Sciprintf("TIMEOUT: Configured\n"));
      MY_THREADS_ENTER;
      nsp_gtk_main_quit();
      MY_THREADS_LEAVE;
      return FALSE;
    }
  else
    {
      DEBUG_GRAPHICS(Sciprintf("TIMEOUT: Not configured\n"));
      return TRUE;
    }
}

extern void controlC_handler (int sig);
static void controlC_handler_configured(int sig)
{
  MY_THREADS_ENTER;
  nsp_gtk_main_quit();
  MY_THREADS_LEAVE;
}

static void nsp_configure_wait(BCG *dd)
{
  DEBUG_GRAPHICS(Sciprintf("enter: nsp_configure_wait\n"));
  dd->private->configured = FALSE;
  signal(SIGINT,controlC_handler_configured);
  g_timeout_add(100,(GSourceFunc) timeout_configured, dd);
  nsp_gtk_main();
  /* g_source_remove(tid); this is done when timeout_configured return FALSE */
  signal(SIGINT,controlC_handler);
  DEBUG_GRAPHICS(Sciprintf("quit: nsp_configure_wait\n"));
}

/**
 * expose_event_new:
 * @widget:
 * @event:
 * @data:
 *
 * used with Gtk+2 
 *
 * Returns:
 **/

#if defined(PERICAIRO)

static gint expose_event_new(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GdkRectangle *rect;
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);

  DEBUG_GRAPHICS(Sciprintf("Expose event\n"));
  /*
   * redraw rectangle:
   * here we use dd->private->invalidated and not
   * event because event is clipped to the visible
   * part of the drawing area and we want to keep
   * also correct drawing in hidden part of the
   * drawing area since we use a backing store pixbuf.
   */
  rect = ( event != NULL) ? &dd->private->invalidated : NULL;

  if( dd->private->resize != 0)
    {
      /* we need to resize the pixmap used for drawing
       */
      dd->private->resize = 0;
      if ( dd->private->pixmap) cairo_surface_destroy(dd->private->pixmap);
      
      dd->private->pixmap = gdk_window_create_similar_surface (gtk_widget_get_window(dd->private->drawing),
							       CAIRO_CONTENT_COLOR,
							       dd->CWindowWidth, dd->CWindowHeight);
      /* update drawable */
      if ( dd->CurPixmapStatus == 0 ) dd->private->drawable = dd->private->pixmap;
      if ( dd->private->cairo_pixmap_cr != NULL) cairo_destroy (dd->private->cairo_pixmap_cr);
      dd->private->cairo_pixmap_cr = cairo_create (dd->private->pixmap);
      dd->private->cairo_drawable_cr = dd->private->cairo_pixmap_cr;
      /* if we have an extra pixmap we must resize */
      dd->graphic_engine->pixmap_resize(dd);
      /* we want to redraw all the window */
      dd->private->draw = TRUE;
      rect = NULL;
    }

  if ( dd->private->draw == TRUE )
    {
      dd->private->draw = FALSE;
      if (dd->figure == NULL)
	{
	  dd->graphic_engine->cleararea(dd,rect);
	}
      else
	{
	  /* call a new draw when necessary here */
	  NspGraphic *G = (NspGraphic *) dd->figure ;
	  G->type->draw(dd,G,rect,NULL);
	}
    }

  if (event  != NULL)
    {
      /*
      gdk_draw_drawable(gtk_widget_get_window(dd->private->drawing), dd->private->stdgc,
			dd->private->pixmap,
			event->area.x, event->area.y, event->area.x, event->area.y,
			event->area.width, event->area.height);
      */
      /* uncomment to debug the drawing rectangle which is updated
      gdk_draw_rectangle(gtk_widget_get_window(dd->private->drawing),dd->private->wgc,FALSE,
			 event->area.x, event->area.y,
			 event->area.width, event->area.height);
      */
      /*
      cairo_set_source_surface (cr, src_surface, x_dest - x_src, y_dest - y_src);
      cairo_rectangle (cr, x_dest, y_dest, width, height);
      */
      cairo_t *cr = gdk_cairo_create (gtk_widget_get_window(dd->private->drawing));
      cairo_set_source_surface(cr,dd->private->pixmap,0,0);
      cairo_pattern_set_extend (cairo_get_source (cr), CAIRO_EXTEND_REPEAT);
      cairo_rectangle (cr, event->area.x, event->area.y, event->area.width, event->area.height);
      cairo_fill (cr);
      cairo_destroy (cr);
    }
  else
    {
      /*
      gdk_draw_drawable(gtk_widget_get_window(dd->private->drawing), dd->private->stdgc,
			dd->private->pixmap,
			0,0,0,0,
			dd->CWindowWidth, dd->CWindowHeight);
      */
      cairo_t *cr = gdk_cairo_create (gtk_widget_get_window(dd->private->drawing));
      cairo_set_source_surface(cr,dd->private->pixmap,0,0);
      cairo_pattern_set_extend (cairo_get_source (cr), CAIRO_EXTEND_REPEAT);
      cairo_fill (cr);
      cairo_destroy (cr);
    }
  /* if a zrect exists then add it on graphics  */
  if ( dd->zrect[2] != 0 && dd->zrect[3] != 0)
    {
      cairo_t *cr = gdk_cairo_create (gtk_widget_get_window(dd->private->drawing));
      cairo_rectangle (cr,dd->zrect[0],dd->zrect[1],dd->zrect[2],dd->zrect[3]);
      cairo_stroke (cr);
    }

  dd->private->invalidated.x = 0;
  dd->private->invalidated.y = 0;
  dd->private->invalidated.width = 0;
  dd->private->invalidated.height = 0;

  gdk_flush();
  return FALSE;
}
#endif

#if defined(PERICAIRO)

/* resize/create surfaces associated to the drawing widget */

static void nsp_drawing_surface_resize(BCG *dd,int width,int height,int extra_surface)
{
  cairo_surface_t *surface = (extra_surface) ? dd->private->pixmap : dd->private->extra_pixmap;
  cairo_t *cr = (extra_surface) ? dd->private->cairo_pixmap_cr : dd->private->cairo_extra_pixmap_cr;
  if ( surface ) g_object_unref(G_OBJECT(surface));
  surface = gdk_window_create_similar_surface (gtk_widget_get_window(dd->private->drawing),
					       CAIRO_CONTENT_COLOR,
					       width,height);
  if ( cr != NULL) cairo_destroy (cr);
  cr = cairo_create (surface);
  dd->private->cairo_drawable_cr = cr;
  dd->private->drawable = surface;
  pixmap_clear_rect(dd,0,0,width,height);
  if ( extra_surface) 
    {
      dd->private->extra_pixmap = surface;
      dd->private->cairo_extra_pixmap_cr = cr;
    }
  else
    {
      dd->private->pixmap = surface;
      dd->private->cairo_pixmap_cr = cr;
    }
}

static void nsp_drawing_resize(BCG *dd,int width, int height)
{
  nsp_drawing_surface_resize(dd,width,height, FALSE);
  if ( dd->CurPixmapStatus != 0)
      {
	/* we are using also an extra surface */
	nsp_drawing_surface_resize(dd,width,height, TRUE);
      }
}


#if GTK_CHECK_VERSION(3,0,0)
static void nsp_drawing_invalidate_handler(GdkWindow *window, cairo_region_t *region)
{
  GQuark quark = g_quark_from_string("xgc");
  BCG *dd = g_object_get_qdata(G_OBJECT(window), quark);
  cairo_rectangle_int_t r;
  cairo_region_get_extents (region,&r);
  /* Sciprintf("Inside the nsp_drawing_invalidate_handler [%d,%d,%d,%d]\n",r.x,r.y,r.width,r.height);   */
  if ( dd != NULL) 
    {
      dd->private->invalidated_r = r;
      dd->private->region = region ;
    }
  else
    Sciprintf("Inside the nsp_drawing_invalidate_handler dd is null [%d,%d,%d,%d]\n",r.x,r.y,r.width,r.height);
}

#endif 

#if GTK_CHECK_VERSION(3,0,0)

/* the drawing callback for GTk+3 
 * we use a non-double buffered window 
 * Thus begin/end paint are not called before the draw_callback 
 */

#if ! defined(PERIGTK3GL) 
static gint draw_callback(GtkWidget *widget, cairo_t *cr, gpointer data)
{
  gint width, height;
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);
  
  nsp_gtk_widget_get_size(dd->private->drawing,&width,&height);
  DEBUG_GRAPHICS(Sciprintf("Drawing: the drawing area is of size (%d,%d)\n",width,height));
  /* 
  {
    double x1,x2,x3,x4;
    cairo_clip_extents (cr,&x1,&x2,&x3,&x4);
    DEBUG_GRAPHICS(Sciprintf("Clip extents %f,%f,%f,%f\n",x1,x2,x3,x4));
  }
  */
  
  if( dd->private->draw_init) 
    {
      /* first time we are in draw_callback, we need to initialize 
       * the surface 
       */
      pixmap_clear_rect(dd,0,0,width,height);
      dd->private->draw_init = FALSE;
    }
  
  if( dd->private->resize != 0 ) 
    {
      /* we need to resize the surface used for drawing */
      DEBUG_GRAPHICS(Sciprintf("Drawing: make a resize of pixmap \n"));
      dd->private->resize = 0;
      nsp_drawing_resize(dd,width,height);
      /* we want to redraw all the window */
      dd->private->draw = TRUE;
    }
  
  if ( dd->private->draw == TRUE )
    {
      /* we need to update the surface associated to the graphic 
       * window 
       */
      DEBUG_GRAPHICS(Sciprintf("Drawing: redraw to pixmap \n"));
      
      dd->private->draw = FALSE;
      /* we redraw with a clip rectangle given by the invalidate_handler 
       * we could use dd->private->invalidated ? instead 
       */
      if (dd->figure == NULL)
	{
	  /* nothing to do except clearing the window */
	  dd->graphic_engine->cleararea(dd,NULL);
	}
      else
	{
	  /* call a new draw when necessary here */
	  GdkRectangle rect;
	  /* 
	  Sciprintf("Inside the drawing callback [%d,%d,%d,%d] and [%d,%d,%d,%d]\n",
		    dd->private->invalidated_r.x,
		    dd->private->invalidated_r.y,
		    dd->private->invalidated_r.width,
		    dd->private->invalidated_r.height,
		    dd->private->invalidated.x,
		    dd->private->invalidated.y,
		    dd->private->invalidated.width,
		    dd->private->invalidated.height);
	  */
	  if ( dd->private->invalidated.width == 0 ) 
	    {
	      rect = dd->private->invalidated_r;
	    }
	  else
	    {
	      rect = dd->private->invalidated;
	    }
	  NspGraphic *G = (NspGraphic *) dd->figure ;
	  G->type->draw(dd,G,&rect,NULL);
	}
    }
  
  /* draws dd->private->pixmap to physical window 
   * cr is dealing with clip not to draw everything.
   * is this true when we do not use double buffering ? 
   */
  if ( dd->private->region != NULL) 
    gdk_window_begin_paint_region (gtk_widget_get_window(dd->private->drawing),
				   dd->private->region);
  cairo_set_source_rgb(cr,
		       dd->private->gcol_bg.red/65535.0,
		       dd->private->gcol_bg.green/65535.0,
		       dd->private->gcol_bg.blue/65535.0);
  cairo_set_source_surface(cr,dd->private->pixmap,0,0);
  cairo_rectangle (cr, 0,0, width, height);
  cairo_fill (cr);
  if ( dd->private->region != NULL) 
    {
      gdk_window_end_paint(gtk_widget_get_window(dd->private->drawing));
      dd->private->region = NULL;
    }
  
  

  /* if a zrect exists then add it on graphics  */
  if ( dd->zrect[2] != 0 && dd->zrect[3] != 0)
    {
      /* draw the zoom rectangle */
      cairo_set_source_rgb(cr,0.0,0.0,0.0);
      cairo_rectangle (cr,dd->zrect[0],dd->zrect[1],dd->zrect[2],dd->zrect[3]);
      cairo_stroke (cr);
    }
  
  dd->private->invalidated.x = 0;
  dd->private->invalidated.y = 0;
  dd->private->invalidated.width = 0;
  dd->private->invalidated.height = 0;
  
  return FALSE;
}
#endif

#if defined(PERIGTK3GL) 
static GLuint c_color, c_flag;
static GLuint program;
static GLuint mvp_location;
static GLuint mytexture;
static GLint attribute_coords=0;
static GLint attribute_colors=1;
static GLint attribute_texcoords=2;
static GLuint vbo_triangle_coords, vbo_triangle_colors;
static GLuint vao_triangles;
static GLuint vbo_textriangle_coords, vbo_textriangle_texcoords;
static GLuint vao_textriangles;
static void gen_buffers(void);
static GLuint create_shader (int type, const char *src);
static void array_fill(void);
static void array_texture_fill(void);
static void init_shaders (GLuint *program_out, GLuint *mvp_out);

static void shader_fill_triangle(GLfloat vertex_data[],GLfloat vertex_colors[],int ncolors);
static void shader_fill_quad(GLfloat vertex_data[],GLfloat vertex_colors[],int ncolors);
static void shader_draw_line(GLfloat vertex_data[],int n, int closeflag, GLfloat vertex_colors[],int ncolors);
static void shader_draw_points(GLfloat vertex_data[],int n,  GLfloat vertex_colors[],int ncolors);

static void realize_gtk3gl (GtkWidget *widget)
{
  gtk_gl_area_make_current (GTK_GL_AREA (widget));
  /* Enable depth buffer */
  gtk_gl_area_set_has_depth_buffer(GTK_GL_AREA (widget), TRUE);
  /* render mode */
  /* gtk_gl_area_set_auto_render(GTK_GL_AREA (widget), FALSE); */
  gen_buffers ();
  init_shaders (&program, &mvp_location);
  array_fill();
  array_texture_fill();	
}

static void unrealize_gtk3gl (GtkWidget *widget)
 {
  gtk_gl_area_make_current (GTK_GL_AREA (widget));
  glDeleteBuffers (1, &vbo_triangle_coords);
  glDeleteBuffers (1, &vbo_triangle_colors);
  glDeleteBuffers(1,&vbo_textriangle_coords);
  glDeleteBuffers(1,&vbo_textriangle_texcoords);
  glDeleteProgram (program);
 }

static gint render_callback(GtkGLArea    *area, GdkGLContext *context,  gpointer data)
{
  
  GdkRectangle *rect;
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  
  if (gtk_gl_area_get_error (area) != NULL)
    return FALSE;
  
  /*   rect = ( event != NULL) ? &dd->private->invalidated : NULL; */
  /*
   * with this driver we need to fully redraw.
   */
  rect = NULL;
  
  if (0 && dd->private->resize != 0)
    {
      /* redraw after resize
       */
      dd->private->resize = 0;
      /* glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); */
      glClear(GL_DEPTH_BUFFER_BIT);
      nsp_ogl_set_view(dd);
      /* if we have an extra pixmap we must resize */
      dd->graphic_engine->pixmap_resize(dd);
      /* we want to redraw all the window */
      dd->private->draw = TRUE;
      rect = NULL;
    }
  
  /* with this driver we have to draw all times */

  if ( 1 ||  dd->private->draw == TRUE  )
    {
      /* just redraw if we have recorded stuffs */
      /* glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); */
      glClear(GL_DEPTH_BUFFER_BIT);
      glUseProgram (program);
      nsp_ogl_set_view(dd);
      dd->private->draw = FALSE;
      /* need to redraw */
      if (dd->figure == NULL)
	{
	  dd->graphic_engine->cleararea(dd,rect);
	}
      else
	{
	  NspGraphic *G = (NspGraphic *) dd->figure ;
	  G->type->draw(dd,G,rect,NULL);
	}
      if ( dd->zrect[2] != 0 && dd->zrect[3] != 0)
	{
	  double zrect[4];
	  int i;
	  for ( i = 0 ; i < 4 ; i++) zrect[i]=dd->zrect[i];
	  dd->graphic_engine->drawrectangle(dd,zrect);
	}
      glUseProgram (0);
    }
  /* Flush the contents of the pipeline */
  glFlush ();
  return TRUE;
}
#endif

static gint scrolled_draw_callback(GtkWidget *widget, cairo_t *cr, gpointer data)
{
  return FALSE;
}
#endif
#endif

#ifdef PERIGL
/* periGL version */
static gint expose_event_new(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GdkRectangle *rect;
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);

  /*   rect = ( event != NULL) ? &dd->private->invalidated : NULL; */
  /*
   * with this driver we need to fully redraw.
   */
  rect = NULL;

  if ( dd->private->resize != 0)
    {
      /* redraw after resize
       */
      dd->private->resize = 0;
      if (!gdk_gl_drawable_gl_begin (dd->private->gldrawable, dd->private->glcontext)) return FALSE;
      /* glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); */
      glClear(GL_DEPTH_BUFFER_BIT);
      nsp_ogl_set_view(dd);
      /* if we have an extra pixmap we must resize */
      dd->graphic_engine->pixmap_resize(dd);
      /* we want to redraw all the window */
      dd->private->draw = TRUE;
      rect = NULL;
    }
  else
    {
      if (!gdk_gl_drawable_gl_begin (dd->private->gldrawable, dd->private->glcontext)) return FALSE;
    }

  /* with this driver we have to draw all times */

  if ( 1 ||  dd->private->draw == TRUE  )
    {
      /* just redraw if we have recorded stuffs */
      /* glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); */
      glClear(GL_DEPTH_BUFFER_BIT);
      nsp_ogl_set_view(dd);
      dd->private->draw = FALSE;
      /* need to redraw */
      if (dd->figure == NULL)
	{
	  dd->graphic_engine->cleararea(dd,rect);
	}
      else
	{
	  NspGraphic *G = (NspGraphic *) dd->figure ;
	  G->type->draw(dd,G,rect,NULL);
	}
      if ( dd->zrect[2] != 0 && dd->zrect[3] != 0)
	{
	  double zrect[4];
	  int i;
	  for ( i = 0 ; i < 4 ; i++) zrect[i]=dd->zrect[i];
	  dd->graphic_engine->drawrectangle(dd,zrect);
	}
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
  return FALSE;
}

#endif

/*
 * basic graphics: we switch here depending on drivers
 */

/*
 * include the cairo basic graphic routines
 */

#if defined(PERICAIRO) && !defined(PERIGTK3GL)
#include "perigtk/peridraw_cairo_new.c"
#endif /* PERICAIRO */

/*
 * include the opengl basic graphic routines
 */

#if defined(PERIGL) 
#include "perigtk/peridraw_gl.c"
#endif /* PERIGL */


#if defined(PERIGTK3GL)
#include "perigtk/peridraw_gtk3gl.c"
#endif /* PERIGL */



/* function below are to be defined once they are 
 * shared by all drivers. PERI_COMMON is just defined 
 * in periCairoNew.c
 */

#ifdef PERI_COMMON

void nsp_set_cursor(BCG *Xgc,int id)
{
  GdkCursor *cursor;
  if ( id == -1 )
    {
      gdk_window_set_cursor (gtk_widget_get_window(Xgc->private->drawing),
			     Xgc->private->ccursor);
      if (Xgc->private->extra_cursor != NULL)
	{
#if GTK_CHECK_VERSION(3,0,0)
	  g_object_unref (G_OBJECT(Xgc->private->extra_cursor));
#else
	  gdk_cursor_unref (Xgc->private->extra_cursor);
#endif
	}
      Xgc->private->extra_cursor = NULL;
    }
  else
    {
      cursor = gdk_cursor_new(id);
      if ( cursor != NULL)
	{
	  gdk_window_set_cursor (gtk_widget_get_window(Xgc->private->drawing),
				 cursor);
	  if (Xgc->private->extra_cursor != NULL)
	    {
#if GTK_CHECK_VERSION(3,0,0)
	      g_object_unref (G_OBJECT(Xgc->private->extra_cursor));
#else
	      gdk_cursor_unref (Xgc->private->extra_cursor);
#endif
	    }
	  Xgc->private->extra_cursor = cursor;
	}
    }
}

NspObject *nsp_get_graphic_widget(int wid)
{
  NspObject *Obj;
  BCG *Xgc;
  if ( (Xgc=window_list_search_new(wid)) == NULL) return NULL;
  if ( Xgc->private->window == NULL)  return NULL;
  nsp_type_gtkwindow = new_type_gtkwindow(T_BASE);
  Obj = (NspObject *)
    gobject_create(NVOID,(GObject *) Xgc->private->window,
		   (NspTypeBase *) nsp_type_gtkwindow);
  return Obj;
  return NULL;
}

/**
 * nsp_set_colormap_constants:
 * @Xgc: a #BCG
 * @m:
 *
 *
 **/

void nsp_set_colormap_constants(BCG *Xgc,int m)
{
  Xgc->Numcolors = m;
  Xgc->IDLastPattern = m;
  Xgc->NumForeground = -1 ;
  Xgc->NumBackground = -1;
  Xgc->CurColor = -1;
  Xgc->CmapFlag = 0;
  Xgc->graphic_engine->xset_usecolor(Xgc,1);
  Xgc->graphic_engine->xset_color(Xgc,m+1);
  Xgc->graphic_engine->xset_foreground(Xgc,m+1);
  Xgc->graphic_engine->xset_background(Xgc,m+2);
}

GTK_locator_info nsp_event_info = { -1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0};

/* A set of predefined colors
 */

double nsp_predef_colors[] =
  {
    0,0,0,       /* black */
    1,1,1,       /* white */
    0.8,0.8,0.8, /* gray */
    0,   0, 1,   /* Blue */
    0,   1, 0,   /* Green */
    0,   1, 1,   /* Cyan */
    1,   0, 0,   /* Red */
    1,   0, 1,   /* Magenta */
    1,   1, 0,   /* Yellow */
};

/**
 * nsp_initialize_gc:
 * @Xgc: a #BCG
 *
 * Initialisation of the graphic context. Used also
 * to come back to the default graphic state
 *
 **/

void nsp_initialize_gc( BCG *Xgc )
{
  int i;
  Xgc->graphic_engine->xset_unclip(Xgc);
  Xgc->fontId=0; Xgc->fontSize=0 ;
  Xgc->graphic_engine->xset_font(Xgc,2,1,FALSE);
  Xgc->CurHardSymb=0; Xgc->CurHardSymbSize=0;
  Xgc->graphic_engine->xset_mark(Xgc,1,1);
  /* Absolute coord mode */
  Xgc->graphic_engine->xset_absourel(Xgc,CoordModeOrigin);
  /* initialisation des pattern dash par defaut en n&b */
  Xgc->graphic_engine->xset_default_colormap(Xgc);
  getcolordef(&i); /* preferred color status */
  Xgc->graphic_engine->xset_usecolor(Xgc,i);
  Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->xset_hidden3d(Xgc,1);
  Xgc->graphic_engine->xset_thickness(Xgc,1);
  Xgc->graphic_engine->xset_color(Xgc,1);
  Xgc->graphic_engine->xset_foreground(Xgc,Xgc->NumForeground);
  Xgc->graphic_engine->xset_background(Xgc,Xgc->NumBackground);
  Xgc->graphic_engine->xset_hidden3d(Xgc,4);
  Xgc->graphic_engine->xset_autoclear_def(Xgc) ;
  Xgc->graphic_engine->xset_fpf_def(Xgc) ;
}


/**
 * nsp_get_pixbuf:
 * @Xgc: a #BCG
 *
 *
 *
 * Returns:
 **/

#if GTK_CHECK_VERSION(3,0,0)
GdkPixbuf* nsp_get_pixbuf(BCG *Xgc)
{
  cairo_surface_t *surface;
  GtkWidget *graphic_widget=  Xgc->private->drawing;
  GdkPixbuf *pixbuf;
  int width, height;
  cairo_t *cr;
  if (  Xgc->private->drawing == NULL ) return NULL;
  width = gtk_widget_get_allocated_width (graphic_widget);
  height = gtk_widget_get_allocated_height (graphic_widget);
  
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);

  cr = cairo_create (surface);
  gtk_widget_draw (graphic_widget, cr);
  cairo_destroy (cr);
  
  pixbuf = gdk_pixbuf_get_from_surface (surface, 0, 0,width, height);
  cairo_surface_destroy (surface);
  return pixbuf;
}

#else

GdkPixbuf* nsp_get_pixbuf(BCG *Xgc)
{
  int width, height;
  GdkPixbuf *pixbuf;
  GdkPixmap *pixmap;
  GtkWidget *graphic_widget = Xgc->private->drawing;
  if (  Xgc->private->drawing == NULL ) return NULL;
  pixmap = gtk_widget_get_snapshot (graphic_widget, NULL);
  gdk_drawable_get_size (pixmap, &width, &height);
  pixbuf = gdk_pixbuf_get_from_drawable (NULL, pixmap,
					 gtk_widget_get_colormap (graphic_widget),
					 0, 0, 0, 0, width, height);
  g_object_unref (pixmap);
  return pixbuf;
}

#endif /* GTK_CHECK_VERSION(3,0,0) */

void nsp_pause(int sec_time,int events)
{
  if ( sec_time == 0 )
    {
      /* flush events only */
      while ( gtk_events_pending()) gtk_main_iteration();
    }
  else
    {
      xpause(sec_time,events);
    }
}

int window_list_check_top(BCG *dd,void *win)
{
  return dd->private->window == (GtkWidget *) win ;
}

int window_list_check_drawing(BCG *dd,void *win)
{
  return dd->private->drawing == (GtkWidget *) win ;
}

/* this should be  moved in windows: keep track of window ids
 */

static int EntryCounter = 0;
int nsp_get_win_counter() { return EntryCounter;};
void nsp_set_win_counter(int n) {  EntryCounter=Max(EntryCounter,n); EntryCounter++;}

#endif /* PERI_COMMON */
