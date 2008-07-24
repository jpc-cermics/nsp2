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
 * Cairo driver 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <gtk/gtk.h>
#include <gtkcairo.h>

#define PERICAIRO
#define PERI_PRIVATE 1
#define PERI_PRIVATE_CAIRO 1
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "nsp/graphics/periGtk.h"
#include "nsp/version.h"
#include "nsp/graphics/color.h"
#include "nsp/command.h"

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

/*
 * FIXME: Attention pour Cairo on n'a pas encore de pixmap 
 *        on dessine que ds expose 
 * Il faut proteger tous les ordres graphiques avec DRAW_CHECK 
 * pour confiner le fait que dessiner a lieu juste dans paint.
 * Il faut faire de meme pour periGtk.c 
 */

#define DRAW_CHECK  if ( Xgc->private->in_expose == FALSE && Xgc->CurPixmapStatus == 0 ) \
   {  nsp_gtk_invalidate(Xgc); if (Xgc->record_flag == TRUE) {  Xgc->private->draw = TRUE;  return;} }


#define GTK_CAIRO_GET_CAIRO(cairo,gtkcairo,str)				\
  if ((cairo = gtk_cairo_get_cairo(gtkcairo))==NULL)			\
    {									\
      fprintf (stderr, "gtk_cairo_get_cairo_failed in %s\n",str);	\
      return;								\
    } 

#define GTK_CAIRO_GET_CAIRO_RET(cairo,gtkcairo,str,val)			\
  if ((cairo = gtk_cairo_get_cairo(gtkcairo))==NULL)			\
    {									\
      fprintf (stderr, "gtk_cairo_get_cairo_failed in %s\n",str);	\
      return val;							\
    } 


/* Global variables to deal with X11 **/

static unsigned long maxcol; /* FIXME XXXXX : à revoir */
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

static void force_affichage(BCG *Xgc);


void create_graphic_window_menu( BCG *dd);
extern void start_sci_gtk();

/*---------------------------------------------------------
 * Gtk graphic engine 
 * A définir sans doute ailleurs XXXXX
 *---------------------------------------------------------*/

Gengine * nsp_gengine = &Gtk_gengine ;

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

/*---------------------------------------------------------
 * Next routine are used to deal with the extra_pixmap 
 * which is used when xset('pixmap',1) is activated at 
 * scilab level. 
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

static void xselgraphic(BCG *Xgc)
{ 
  /* Test not really usefull: see sciwin in matdes.f */
  if ( Xgc == (BCG *)0 || Xgc->private->window ==  NULL) initgraphic("",NULL,NULL,NULL,NULL,NULL,'e');
  gdk_window_show(Xgc->private->window->window);
  gdk_flush();
}

/* End of graphic (do nothing)  */

static void xend(BCG *Xgc)
{
  /** Must destroy everything  **/
}


/* Clear the current graphic window     */

static void clearwindow(BCG *Xgc)
{
  cairo_t *cairo;
  GtkCairo *gtkcairo;
  /* we use the private->stdgc graphic context */
  DRAW_CHECK;
  gtkcairo = GTK_CAIRO (Xgc->private->cairo_drawing);
  GTK_CAIRO_GET_CAIRO(cairo,gtkcairo,"xset_clearwindow");
  cairo_set_source_rgb(cairo,
		       Xgc->private->gcol_bg.red/65535.0,
		       Xgc->private->gcol_bg.green/65535.0,
		       Xgc->private->gcol_bg.blue/65535.0);
  cairo_rectangle (cairo,0,0, Xgc->CWindowWidth, Xgc->CWindowHeight);
  cairo_fill (cairo);
}


#include "perigtk/events.c"  



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
      gdk_drawable_get_size (Xgc->private->window->window,&pw,&ph);
      gdk_drawable_get_size (Xgc->private->drawing->window,&w,&h);
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
      /* gdk_drawable_get_size (Xgc->private->scrolled,&sc_w,&sc_h); */
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

/** To get the popup  window size **/

static void xget_popupdim(BCG *Xgc,int *x, int *y)
{ 
  gint xx,yy;
  gdk_drawable_get_size (Xgc->private->window->window,&xx,&yy);
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

/** Set a clip zone (rectangle ) **/

static void xset_clip(BCG *Xgc,int x[])
{
  /* 
     GtkCairo *gtkcairo;
     cairo_t *cairo;
     int i;*/
  /* clip_rect ={x[0],x[1],x[2],x[3]}= {x,y,w,h} */
  DRAW_CHECK;
  /* 
  Xgc->ClipRegionSet = 1;
  for (i=0 ; i < 4 ; i++)   Xgc->CurClipRegion[i]= x[i];
  gtkcairo = GTK_CAIRO (Xgc->private->cairo_drawing);
  GTK_CAIRO_GET_CAIRO(cairo,gtkcairo,"xset_clip");
  cairo_new_path (cairo);
  cairo_move_to (cairo, x[0],x[1]);
  cairo_rel_line_to (cairo, x[2],0.0);
  cairo_rel_line_to (cairo, 0.0, - x[3]);
  cairo_rel_line_to (cairo, -x[2],0.0);
  cairo_rel_line_to (cairo, 0.0, x[3]);
  cairo_close_path (cairo);
  cairo_clip (cairo);
  */
  /* gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
   */

}

/** unset clip zone **/

static void xset_unclip(BCG *Xgc)
{
  GtkCairo *gtkcairo;
  cairo_t *cairo;
  double x[]={0,0,int16max,  int16max};
  DRAW_CHECK;
  if ( Xgc->ClipRegionSet == 0 ) return;
  Xgc->ClipRegionSet = 0;
  return;/* XXXX*/
  gtkcairo = GTK_CAIRO (Xgc->private->cairo_drawing);
  GTK_CAIRO_GET_CAIRO(cairo,gtkcairo,"xset_unclip");
  /* 
   * XXXX how to remove a clip region ? FIXME 
   * cairo_init_clip(cairo);  
   */
  cairo_move_to (cairo, x[0],x[1]);
  cairo_rel_line_to (cairo, x[2],0.0);
  cairo_rel_line_to (cairo, 0.0, - x[3]);
  cairo_rel_line_to (cairo, -x[2],0.0);
  cairo_rel_line_to (cairo, 0.0, x[3]);
  cairo_close_path (cairo);
  cairo_clip (cairo);
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

/*
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

  static void xset_alufunction(BCG *Xgc,char *string)
  {   
  int value;
  idfromname(string,&value);
  if ( value != -1)
  {
  Xgc->CurDrawFunction = value;
  gdk_gc_set_function(Xgc->private->wgc, AluStruc_[value].id);
  }
  }
*/

static void xset_alufunction1(BCG *Xgc,int num)
{   
  GtkCairo *gtkcairo;
  cairo_t *cairo;
  int value ; 
  /* GdkColor temp = {0,0,0,0}; */
  Xgc->CurDrawFunction = Min(15,Max(0,num));
  value = AluStruc_[Xgc->CurDrawFunction].id;
  gtkcairo = GTK_CAIRO (Xgc->private->cairo_drawing);
  GTK_CAIRO_GET_CAIRO(cairo,gtkcairo,"xset_dashstyle");
  switch (value) 
    {
    case GDK_CLEAR : 
      /* 
	 gdk_gc_set_foreground(Xgc->private->wgc, &Xgc->private->gcol_bg);
	 gdk_gc_set_background(Xgc->private->wgc, &Xgc->private->gcol_bg);
	 gdk_gc_set_function(Xgc->private->wgc,GDK_COPY);
      */
      break;
    case GDK_XOR   : 
      cairo_set_operator (cairo, CAIRO_OPERATOR_XOR);
      /* temp.pixel = Xgc->private->gcol_fg.pixel ^ Xgc->private->gcol_bg.pixel ;
	 gdk_gc_set_foreground(Xgc->private->wgc, &temp);
	 gdk_gc_set_background(Xgc->private->wgc, &Xgc->private->gcol_bg);
	 gdk_gc_set_function(Xgc->private->wgc,GDK_XOR);
      */
      break;
    default :
      cairo_set_operator (cairo, CAIRO_OPERATOR_OVER);
      /* 
	 gdk_gc_set_foreground(Xgc->private->wgc, &Xgc->private->gcol_fg);
	 gdk_gc_set_background(Xgc->private->wgc, &Xgc->private->gcol_bg);
	 gdk_gc_set_function(Xgc->private->wgc,value);
      */
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
  GtkCairo *gtkcairo;
  cairo_t *cairo;
  gtkcairo = GTK_CAIRO (Xgc->private->cairo_drawing);
  GTK_CAIRO_GET_CAIRO(cairo,gtkcairo,"xset_dashstyle");

  if ( value == 0) 
    {
      /* FIXME: proper width in double ? */
      cairo_set_line_width(cairo,(Xgc->CurLineWidth <= 1) ? 1 : Xgc->CurLineWidth*0.5);
      cairo_set_line_cap(cairo, CAIRO_LINE_CAP_BUTT);
      cairo_set_line_join (cairo, CAIRO_LINE_JOIN_ROUND);
      /* remove dash */
      cairo_set_dash (cairo,NULL,0,0.0);
      /* 
       * gdk_gc_set_line_attributes(Xgc->private->wgc,
       *		 (Xgc->CurLineWidth <= 1) ? 0 : Xgc->CurLineWidth,
       *			 GDK_LINE_SOLID,GDK_CAP_BUTT, GDK_JOIN_ROUND);
       */
    }
  else 
    {
      double buffdash[18];
      int i;
      for ( i =0 ; i < *n ; i++) buffdash[i]=xx[i];
      cairo_set_dash (cairo,buffdash,*n,0.0);
      cairo_set_line_width(cairo,(Xgc->CurLineWidth <= 1) ? 1 : Xgc->CurLineWidth*0.5);
      cairo_set_line_cap(cairo, CAIRO_LINE_CAP_BUTT);
      cairo_set_line_join (cairo, CAIRO_LINE_JOIN_ROUND);
      /* 
	 gdk_gc_set_dashes(Xgc->private->wgc, 0, buffdash, *n);
	 gdk_gc_set_line_attributes(Xgc->private->wgc, 
	 (Xgc->CurLineWidth == 0 ) ? 1 : Xgc->CurLineWidth,
	 GDK_LINE_ON_OFF_DASH, GDK_CAP_BUTT, GDK_JOIN_ROUND);
      */
    }
}
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
      gdk_drawable_get_size (Xgc->private->drawing->window,&w,&h);
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
      xset_alufunction1(Xgc,Xgc->CurDrawFunction);
      /* XXXXX 
	 gdk_window_set_background(Xgc->private->drawing->window, &Xgc->private->gcol_bg);
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


/*-------------------------------------------------------------------------
 * window_list management 
 *-------------------------------------------------------------------------*/

int window_list_check_top(BCG *dd,void *win) 
{
  return dd->private->window == (GtkWidget *) win ;
}

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
      /* XXXX 
	 gtk_widget_hide(GTK_WIDGET(winxgc->private->drawing)); 
      */
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
  /* 
     g_object_unref(winxgc->private->stdgc);
     g_object_unref(winxgc->private->wgc);
  */
  g_object_unref(winxgc->private->item_factory);
  FREE(winxgc->private);
  /* remove current window from window list */
  window_list_remove(intnum);
}

/********************************************
 * Routines for initialization : string is a display name 
 ********************************************/


static void nsp_gtk_set_color(BCG *Xgc,int col)
{
  GtkCairo *gtkcairo;
  cairo_t *cairo;
  /* int value = AluStruc_[Xgc->CurDrawFunction].id;
   * GdkColor temp = {0,0,0,0};
   */
  /* colors from 1 to Xgc->Numcolors */
  Xgc->CurColor = col = Max(0,Min(col,Xgc->Numcolors + 1));
  if (Xgc->private->colors  == NULL) return;

  gtkcairo = GTK_CAIRO (Xgc->private->cairo_drawing);
  GTK_CAIRO_GET_CAIRO(cairo,gtkcairo,"nsp_gtk_set_color");
  cairo_set_source_rgb(cairo,Xgc->private->colors[col].red/((double) 0xffff),
		       Xgc->private->colors[col].green/((double) 0xffff),
		       Xgc->private->colors[col].blue/((double) 0xffff));
  /* 
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
  */
}

/*
 * initgraphic : create initialize graphic windows
 */

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
  nsp_initialize_gc(Xgc);
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

/* 
   cairo_select_font_face (cr, "Bitstream Vera Sans",
                            CAIRO_FONT_SLANT_NORMAL,
                            CAIRO_FONT_WEIGHT_NORMAL);

  CAIRO_FONT_SLANT_NORMAL,
  CAIRO_FONT_SLANT_ITALIC,
  CAIRO_FONT_SLANT_OBLIQUE
  CAIRO_FONT_WEIGHT_NORMAL,
  CAIRO_FONT_WEIGHT_BOLD

*/
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
  /* if symbol font was not found me must stop 
   */
  if (strcmp(FontInfoTab_[1].fname,fonttab[1].name) != 0) return;
  for (i =0 ; i < FONTMAXSIZE ; i++)
    {    
      if (FontsList_[1][i] != NULL)
	{
	  for (j=0 ; j < SYMBOLNUMBER ; j++)
	    { 
	      gint lbearing=0, rbearing=0, iascent=0, idescent=0; /* , iwidth=0; */
	      /* gchar tmp[2] = { (gchar) Marks[j],0}; */
	      /* XXXXX 
		 gdk_string_extents(FontsList_[1][i], tmp,
		 &lbearing, &rbearing,
		 &iwidth, &iascent, &idescent);
	      */
	      (ListOffset_[i].xoffset)[j] = (rbearing+lbearing)/2;/* ou iwidth/2 ? */
	      (ListOffset_[i].yoffset)[j] = (iascent+idescent)/2;
	    }
	}
    }
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
  g_return_val_if_fail(GTK_IS_CAIRO(dd->private->drawing), FALSE);
  /* we create our own cairo surface 
   * note that it should change when the window is resized 
   * we must also remove double buffering when working that way 
   */
  gtk_cairo_set_x11_cr((GtkCairo *)dd->private->drawing,600,400);

  /* create gc */
  /* 
     dd->private->stdgc = gdk_gc_new(dd->private->drawing->window);
     gdk_gc_set_rgb_bg_color(dd->private->stdgc,&black);
     gdk_gc_set_rgb_fg_color(dd->private->stdgc,&white);
  */
  /* standard gc : for private->pixmap copies */
  /* this gc could be shared by all windows */
  /* 
     dd->private->wgc = gdk_gc_new(dd->private->drawing->window);
     gdk_gc_set_rgb_bg_color(dd->private->wgc,&black);
     gdk_gc_set_rgb_fg_color(dd->private->wgc,&white);
  */
  /* set the cursor */
  dd->private->gcursor = gdk_cursor_new(GDK_CROSSHAIR);
  dd->private->ccursor = gdk_cursor_new(GDK_TOP_LEFT_ARROW);
  gdk_window_set_cursor(dd->private->drawing->window, dd->private->ccursor);
  /* set window bg */
  dd->private->gcol_bg = white;
  dd->private->gcol_fg = black;
  /* gdk_window_set_background(dd->private->drawing->window, &dd->private->gcol_bg); */

  if ( dd->private->pixmap == NULL)
    {
      /* XXXXX 
	 dd->private->pixmap = gdk_pixmap_new(dd->private->drawing->window,
	 dd->CWindowWidth, dd->CWindowHeight,
	 -1);
	 gdk_gc_set_foreground(dd->private->stdgc, &dd->private->gcol_bg);
	 gdk_draw_rectangle(dd->private->pixmap, dd->private->stdgc, TRUE, 0, 0,
	 dd->CWindowWidth, dd->CWindowHeight);
      */
    }

  /* default value is to use the background pixmap */
  dd->private->drawable= (GdkDrawable *) dd->private->pixmap;
  return FALSE;
}


static gint configure_event(GtkWidget *widget, GdkEventConfigure *event, gpointer data)
{
  BCG *dd = (BCG *) data;
  g_return_val_if_fail(dd != NULL, FALSE);
  g_return_val_if_fail(dd->private->drawing != NULL, FALSE);
  g_return_val_if_fail(GTK_IS_CAIRO(dd->private->drawing), FALSE);

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
  /* cairo_status_t status; */
  BCG *dd = (BCG *) data;
  GtkCairo *gtkcairo;
  cairo_t *cr;
  gint width , height;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (GTK_IS_CAIRO (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if( ! GTK_WIDGET_REALIZED(dd->private->drawing)) return FALSE;

  gtkcairo = GTK_CAIRO (widget);

  width = widget->allocation.width;
  height = widget->allocation.height;

  /* set a new cairo surface */
  gtk_cairo_set_x11_cr((GtkCairo *)dd->private->drawing,width,height);

  GTK_CAIRO_GET_CAIRO_RET(cr,gtkcairo,"cairo_expose_event",FALSE);

  /* configure_event is not activated for cairo */
  if ( dd->CurResizeStatus == 1) 
    {
      if ( (dd->CWindowWidth != width) || (dd->CWindowHeight != height))
	{
	  dd->CWindowWidth = width;
	  dd->CWindowHeight = height;
	  dd->private->resize = 1;
	}
    }
  dd->private->in_expose= TRUE;
  scig_resize(dd->CurWindow);
  dd->private->in_expose= FALSE;

  GTK_CAIRO_GET_CAIRO_RET(cr,gtkcairo,"cairo_expose_event after draw",FALSE);
  
  return TRUE; /* prevent the standard expose for gtkcairo */
}

#if 0
static gint unused_expose_event(GtkWidget *widget, GdkEventExpose *event, gpointer data)
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
      /* update drawable */
      if ( dd->CurPixmapStatus == 0 ) dd->private->drawable = dd->private->pixmap;
      /* fill private background with background */
      gdk_gc_set_background(dd->private->stdgc, &dd->private->gcol_bg);
      gdk_draw_rectangle(dd->private->pixmap,dd->private->stdgc, TRUE,0,0,dd->CWindowWidth, dd->CWindowHeight);
      /* On lance l'action standard de resize + redessin  */
      dd->private->in_expose= TRUE;
      scig_resize(dd->CurWindow);
      dd->private->in_expose= FALSE;
      gdk_draw_pixmap(dd->private->drawing->window, dd->private->stdgc, dd->private->pixmap,0,0,0,0,
		      dd->CWindowWidth, dd->CWindowHeight);
    }
  else 
    {
      if ( dd->private->draw == TRUE ) 
	{
	  /* need to make incremental draw */
	  dd->private->draw = FALSE;
	  dd->private->in_expose= TRUE;
	  scig_replay(dd->CurWindow);
	  dd->private->in_expose= FALSE;
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
#endif 


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


/*
 * include the cairo basic graphic routines 
 */

#include "perigtk/peridraw_cairo.c"
