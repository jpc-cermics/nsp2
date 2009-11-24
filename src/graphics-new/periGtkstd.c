/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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

#define PERI_PRIVATE 1
#include "nsp/sciio.h" 
#include "nsp/math.h"
#include "nsp/graphics-new/periGtk.h"
#include "nsp/version.h"
#include "nsp/graphics-new/color.h"
#include "nsp/command.h"
#include "nsp/object.h"
#include "nsp/figure.h"

static void nsp_get_color_rgb(BCG *Xgc,int color,double *rgb, NspMatrix *colors);

#ifdef PERIGTK
GTK_locator_info nsp_event_info = { -1 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0};
#endif 

#ifdef PERIGTK
double nsp_predef_colors[] = 
  {
    0,0,0,       /* black */
    1,1,1,       /* white */
    0.9,0.9,0.9, /* gray */
    0,   0, 1,   /* Blue */
    0,   1, 0,   /* Green */
    0,   1, 1,   /* Cyan */
    1,   0, 0,   /* Red */
    1,   0, 1,   /* Magenta */
    1,   1, 0,   /* Yellow */
};
#else 
extern double nsp_predef_colors[];
#endif 


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
      gdk_window_invalidate_rect(Xgc->private->drawing->window,
				 &Xgc->private->drawing->allocation,
				 FALSE);
      Xgc->private->invalidated = Xgc->private->drawing->allocation;
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
      gdk_window_invalidate_rect(Xgc->private->drawing->window,rect, FALSE);
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
      /* if we do not have an extra pixmap 
       * we just make a gdk_window_process_updates
       */
      gdk_window_process_updates (Xgc->private->drawing->window, FALSE);
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
 * use background to paint the current window.
 * this function should only be called by expose_event
 * when necessary.
 * 
 **/

static void clearwindow(BCG *Xgc)
{
#ifdef PERICAIRO
  cairo_t *cr; 
#endif 
  /* we use the private->stdgc graphic context */
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
  gdk_gc_set_rgb_fg_color(Xgc->private->stdgc,&Xgc->private->gcol_bg);
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
	  expose_event_new( Xgc->private->drawing,NULL, Xgc);
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
    return Xgc->CurColor;
  else 
    return Xgc->CurPattern;
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
	  Xgc->IDLastPattern = GREYNUMBER;
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
      if ( Xgc->private->colors != NULL) 
	nsp_matrix_destroy(Xgc->private->colors);
      Xgc->private->colors = colors; 
    }
  /* just in case: initialize the colormap */
  if ( Xgc->private->colormap == NULL && Xgc->private->drawing != NULL) 
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);

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
      if ( Xgc->private->colors != NULL) 
	nsp_matrix_destroy(Xgc->private->colors);
      Xgc->private->colors = colors; 
    }
  /* just in case: initialize the colormap */
  if ( Xgc->private->colormap == NULL && Xgc->private->drawing != NULL) 
    Xgc->private->colormap = gtk_widget_get_colormap( Xgc->private->drawing);
  
  for (i = 0; i < A->m; i++) {
    colors->R[i] = A->R[i];
    colors->R[i+ colors->m] =A->R[i+A->m];
    colors->R[i+2*colors->m] =A->R[i+2*A->m];
  }
  nsp_set_colormap_constants(Xgc,A->m);
  return OK;
}

/**
 * nsp_set_colormap_constants:
 * @Xgc: a #BCG  
 * @m: 
 * 
 * 
 **/

#ifdef  PERIGTK
void nsp_set_colormap_constants(BCG *Xgc,int m)
{
  Xgc->Numcolors = m;
  Xgc->IDLastPattern = m;
  Xgc->NumForeground = -1 ;
  Xgc->NumBackground = -1;
  Xgc->CurColor = -1;
  Xgc->CmapFlag = 0;
  Xgc->graphic_engine->xset_usecolor(Xgc,1);
  Xgc->graphic_engine->xset_pattern(Xgc,m+1);
  Xgc->graphic_engine->xset_foreground(Xgc,m+1);
  Xgc->graphic_engine->xset_background(Xgc,m+2);
}
#endif 

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

static void xget_colormap(BCG *Xgc, int *num,  double *val,int color_id)
{
  NspMatrix *colors = Xgc->private->colors;
  int m = Xgc->Numcolors,  i;
  *num = m;
  if ( val != NULL )
    {
      if ( color_id != 0 ) 
	{
	  /* just return one color */
	  if ( color_id >= 1 && color_id <= m )
	    {
	      int i=color_id-1;
	      val[0] = colors->R[i];
	      val[1] = colors->R[i+colors->m];
	      val[2] = colors->R[i+2*colors->m];
	    }
	  else 
	    {
	      val[0]= val[1] =val[2]=0.0;
	    }
	}
      else 
	{
	  for (i = 0; i < m; i++) 
	    {
	      val[i] = colors->R[i];
	      val[i+m] =  colors->R[i+colors->m];
	      val[i+2*m] = colors->R[i+2*colors->m];
	    }
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
  g_queue_push_head( Xgc->private->q_colors,Xgc->private->colors);
  Xgc->private->colors = colors ;
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
  Xgc->private->colors = C; 
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
      nsp_get_color_rgb(Xgc,Xgc->NumBackground,rgb,Xgc->private->colors);
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
      nsp_get_color_rgb(Xgc,Xgc->NumForeground,rgb,Xgc->private->colors);
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
#include "perigtk/init.c" 

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

static gint realize_event_common(GtkWidget *widget, gpointer data)
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
  dd->private->extra_cursor = NULL;
  gdk_window_set_cursor(dd->private->drawing->window, dd->private->ccursor);
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

#if defined(PERIGTK) || defined(PERICAIRO)
static gint realize_event(GtkWidget *widget, gpointer data)
{
  BCG *dd = (BCG *) data;
  if ( realize_event_common(widget,data) == FALSE ) return FALSE;

  if ( dd->private->pixmap == NULL)
    {
      dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
					   dd->CWindowWidth, dd->CWindowHeight,
					   -1);
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

  return FALSE;
}

#else /* PERIGLGTK */
static gint realize_event(GtkWidget *widget, gpointer data)
{
  BCG *dd = (BCG *) data;

  if ( realize_event_common(widget,data) == FALSE ) return FALSE;

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
      if ( dd->CurResizeStatus == -1)
	{
	  /* before initialization */
	  return FALSE ;
	}
      else if ( dd->CurResizeStatus == 1) 
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
 * expose_event_new:
 * @widget: 
 * @event: 
 * @data: 
 * 
 * 
 * 
 * Returns: 
 **/

#if defined(PERIGTK) || defined(PERICAIRO)
static gint expose_event_new(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GdkRectangle *rect;
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);

  /* 
   * redraw rectangle:
   * here we use dd->private->invalidated and not 
   * event because event is clipped to the visible 
   * part of the drawing area and we want to keep 
   * also correct drawing in hidden part of the 
   * drawing area since we use a backing store pixbuf.
   */
  rect = ( event != NULL) ? &dd->private->invalidated : NULL;
  
  if(dd->private->resize != 0) 
    { 
      /* we need to resize the pixmap used for drawing 
       */
      dd->private->resize = 0;
      if ( dd->private->pixmap) g_object_unref(G_OBJECT(dd->private->pixmap));
      dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
					   dd->CWindowWidth, dd->CWindowHeight,
					   -1);
      /* update drawable */
      if ( dd->CurPixmapStatus == 0 ) dd->private->drawable = dd->private->pixmap;
#ifdef PERICAIRO
      if ( dd->private->cairo_cr != NULL) cairo_destroy (dd->private->cairo_cr);
      dd->private->cairo_cr = gdk_cairo_create (dd->private->pixmap);
#endif 
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
	  NspGraphic *G = (NspGraphic *) dd->figure ;
	  G->type->draw(dd,G,rect,NULL);
	}
    }
  
  if (event  != NULL) 
    {
      gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, 
			dd->private->pixmap,
			event->area.x, event->area.y, event->area.x, event->area.y,
			event->area.width, event->area.height);
      /* debug the drawing rectangle which is updated        
	 gdk_draw_rectangle(dd->private->drawing->window,dd->private->wgc,FALSE,
	 event->area.x, event->area.y, 
	 event->area.width, event->area.height);
      */
    }
  else 
    {
      gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, 
			dd->private->pixmap,
			0,0,0,0,
			dd->CWindowWidth, dd->CWindowHeight);
    }
  /* if a zrect exists then add it on graphics  */
  if ( dd->zrect[2] != 0 && dd->zrect[3] != 0) 
    {
      gdk_draw_rectangle(dd->private->drawing->window,dd->private->wgc,FALSE,
			 dd->zrect[0],dd->zrect[1],dd->zrect[2],dd->zrect[3]);
    }
  
  dd->private->invalidated.x = 0;
  dd->private->invalidated.y = 0;
  dd->private->invalidated.width = 0;
  dd->private->invalidated.height = 0;
  
  gdk_flush();
  return FALSE;
}
#endif 

#ifdef PERIGL 
#ifndef PERIGLGTK 

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
  if (!gdk_gl_drawable_gl_begin (dd->private->gldrawable, dd->private->glcontext)) return FALSE;
  
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
	dd->graphic_engine->drawrectangle(dd,dd->zrect);
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

#else 
/* periGL version with drawing in a pixbuf
 */
static gint expose_event_new(GtkWidget *widget, GdkEventExpose *event, gpointer data)
{
  GdkRectangle *rect;
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_DRAWING_AREA(dd->private->drawing), FALSE);

  /* 
   * redraw rectangle:
   * here we use dd->private->invalidated and not 
   * event because event is clipped to the visible 
   * part of the drawing area and we want to keep 
   * also correct drawing in hidden part of the 
   * drawing area since we use a backing store pixbuf.
   */
  rect =  ( event != NULL) ? &dd->private->invalidated : NULL;


  if(dd->private->resize != 0) 
    { 
      /* we need to resize the pixmap used for drawing 
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
      dd->private->gl_only = TRUE;
      /* if we have an extra pixmap we must resize */
      dd->graphic_engine->pixmap_resize(dd);
      /* we want to redraw all the window */
      dd->private->draw = TRUE;
      rect = NULL;
    }

  if ( dd->private->draw == TRUE ) 
    {
      gdk_gl_drawable_gl_begin (dd->private->gldrawable,dd->private->glcontext);
      dd->private->draw = FALSE;
      dd->private->gl_only = TRUE;
      if (dd->figure == NULL) 
	{
	  dd->graphic_engine->cleararea(dd,rect);
	}
      else
	{
	  NspGraphic *G = (NspGraphic *) dd->figure ;
	  G->type->draw(dd,G,rect,NULL);
	}
      gdk_gl_drawable_gl_end (dd->private->gldrawable);
    }

  glFlush ();
  gdk_gl_drawable_wait_gl(dd->private->gldrawable);
  
  if (event  != NULL) 
    {
      gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,
			event->area.x, event->area.y, event->area.x, event->area.y,
			event->area.width, event->area.height);
      /* debug the drawing rectangle which is updated              */
      gdk_draw_rectangle(dd->private->drawing->window,dd->private->wgc,FALSE,
			 event->area.x, event->area.y, 
			 event->area.width, event->area.height);
    }
  else 
    {
      gdk_draw_drawable(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
			dd->CWindowWidth, dd->CWindowHeight);
    }
  /* if a zrect exists then add it on graphics  */
  if ( dd->zrect[2] != 0 && dd->zrect[3] != 0) 
    {
      gdk_draw_rectangle(dd->private->drawing->window,dd->private->wgc,FALSE,
			 dd->zrect[0],dd->zrect[1],dd->zrect[2],dd->zrect[3]);
      gdk_gl_drawable_wait_gdk(dd->private->gldrawable);
    }
  gdk_gl_drawable_wait_gdk(dd->private->gldrawable);

  dd->private->invalidated.x = 0;
  dd->private->invalidated.y = 0;
  dd->private->invalidated.width = 0;
  dd->private->invalidated.height = 0;

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



#ifdef PERIGTK
/* for all drivers */

void nsp_set_cursor(BCG *Xgc,int id)
{
  GdkCursor *cursor;
  if ( id == -1 ) 
    {
      if (Xgc->private->extra_cursor != NULL)
	gdk_cursor_unref (Xgc->private->extra_cursor);
      Xgc->private->extra_cursor = NULL;
      gdk_window_set_cursor (Xgc->private->drawing->window,
			     Xgc->private->ccursor);
    }
  else 
    {
      cursor = gdk_cursor_new(id);
      if ( cursor != NULL) 
	{
	  if (Xgc->private->extra_cursor != NULL)
	    gdk_cursor_unref (Xgc->private->extra_cursor);
	  Xgc->private->extra_cursor = cursor;
	  gdk_window_set_cursor (Xgc->private->drawing->window,
				 cursor);
	}
    }
}
#endif 


