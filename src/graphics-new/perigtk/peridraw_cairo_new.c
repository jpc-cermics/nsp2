/* Nsp
 * Copyright (C) 1998-2006 Jean-Philippe Chancelier Enpc/Cermics
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

/*
 * clear a rectangle zone 
 */

static void cleararea(BCG *Xgc, int x, int y, int w, int h)
{
  cairo_t *cr =  Xgc->private->cairo_cr;
  int clipflag = 0;
  /* switch to a clear gc */
  int cur_alu = Xgc->CurDrawFunction;
  int clear = 0 ; /* 0 is the Xclear alufunction */;
  DRAW_CHECK;
  /* unset clip and set drawing mode to clear */
  if ( cur_alu != clear ) xset_alufunction1(Xgc,clear);
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
      gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
    }
  cairo_set_source_rgb(cr,
		       Xgc->private->gcol_bg.red/65535.0,
		       Xgc->private->gcol_bg.green/65535.0,
		       Xgc->private->gcol_bg.blue/65535.0);
  cairo_rectangle (cr,x,y,w,h);
  cairo_fill (cr);
  /* back to current value */ 
  if ( cur_alu != clear )    xset_alufunction1(Xgc,cur_alu); 
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

/* line 
 *
 */ 

static void drawline(BCG *Xgc,int x1, int yy1, int x2, int y2)
{
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  cairo_move_to(cr,x1,yy1);
  cairo_line_to(cr,x2,y2);
  cairo_stroke(cr);
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
  DRAW_CHECK;
  Xgc->graphic_engine->generic->drawrectangles(Xgc,vects,fillvect,n);
}

/* Draw one rectangle with current line style */

static void drawrectangle(BCG *Xgc,const int rect[])
{ 
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  cairo_rectangle (cr,rect[0],rect[1],rect[2],rect[3]);
  cairo_stroke (cr);
}

/* fill one rectangle, with current pattern */

static void fillrectangle(BCG *Xgc,const int rect[])
{
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  cairo_rectangle (cr,rect[0],rect[1],rect[2],rect[3]);
  cairo_fill (cr);
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
  DRAW_CHECK;
  Xgc->graphic_engine->generic->fillarcs(Xgc,vects,fillvect,n);
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
  DRAW_CHECK;
  Xgc->graphic_engine->generic->drawarcs(Xgc,vects,style,n);
}

/* Draw a single ellipsis or part of it **/

static void drawarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK;
  /* gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,FALSE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]); */
}

/* Fill a single elipsis or part of it with current pattern */

static void fillarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK;
  /* gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,TRUE,arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);*/
}



/* 
 * Draw a set of (*n) polylines (each of which have (*p) points) 
 * with lines or marks 
 * drawvect[i] <= 0 use a mark for polyline i
 * drawvect[i] >  0 use a line style for polyline i 
 */

static void drawpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *drawvect,int n, int p)
{ 
  DRAW_CHECK;
  Xgc->graphic_engine->generic->drawpolylines(Xgc,vectsx,vectsy,drawvect,n,p);
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

static void fillpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *fillvect,int n, int p)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->fillpolylines(Xgc,vectsx,vectsy,fillvect,n,p);
}

/* 
 * Only draw one polygon  with current line style 
 * according to *closeflag : it's a polyline or a polygon
 * n is the number of points of the polyline 
 */

static void drawpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag)
{ 
  cairo_t *cr =  Xgc->private->cairo_cr;
  cairo_status_t status;
  int n1,i;
  DRAW_CHECK;
  if (closeflag == 1) n1 =n+1;else n1= n;
  if (n1 >= 2) 
    {
      cairo_new_path(cr);
      cairo_move_to(cr, vx[0],vy[0]);
      for ( i = 1 ; i < n ; i++ ) 
	cairo_line_to(cr,vx[i],vy[i]);
      if ( closeflag == 1) cairo_line_to(cr,vx[0],vy[0]);
      cairo_stroke (cr);
      if ((status=cairo_status (cr)) != CAIRO_STATUS_SUCCESS) 
	{
	  fprintf (stderr, "Cairo is unhappy in drawpolyline: %s\n",
		   cairo_status_to_string(status));
	}
    }
}

/* 
 * Fill the polygon or polyline 
 * according to *closeflag : the given vector is a polyline or a polygon 
 */

static void fillpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag) 
{
  cairo_t *cr =  Xgc->private->cairo_cr;
  cairo_status_t status;
  int n1,i;
  DRAW_CHECK;
  if (closeflag == 1) n1 = n+1;else n1= n;
  cairo_new_path(cr); 
  cairo_move_to(cr, vx[0],vy[0]);
  for ( i = 1 ; i < n ; i++ ) 
    cairo_line_to(cr,vx[i],vy[i]);
  if ( closeflag == 1) cairo_line_to(cr,vx[0],vy[0]);
  cairo_fill(cr);
  if ((status=cairo_status (cr)) != CAIRO_STATUS_SUCCESS) 
    {
      fprintf (stderr, "Cairo is unhappy in fillpolyline : %s\n",
	       cairo_status_to_string(status));
    }
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
      /* XXXX 
	 if (gtk_store_points(n, vx, vy,(int)0L))
	 {
	 gdk_draw_points(Xgc->private->drawable,
	 Xgc->private->wgc,gtk_get_xpoints(), n);
	 }
      */
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

/*
 *   Draw an axis whith a slope of alpha degree (clockwise) 
 *   . Along the axis marks are set in the direction ( alpha + pi/2), in the  
 *   following way : 
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
 */

static void drawaxis(BCG *Xgc, int alpha, int *nsteps, int *initpoint,double *size)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->drawaxis(Xgc,alpha,nsteps,initpoint,size);
}

/*
 * Display numbers z[i] at location (x[i],y[i])
 *   with a slope alpha[i] (see displaystring), if flag==1
 *   add a box around the string, only if slope =0}
 */

static void displaynumbers(BCG *Xgc, int *x, int *y, int n, int flag, double *z, double *alpha)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->displaynumbers(Xgc,x,y,n,flag,z,alpha);
}



static void draw_mark(BCG *Xgc,int *x, int *y)
{ 
  cairo_t *cr =  Xgc->private->cairo_cr;
  cairo_status_t status;
  int code = symbols[Xgc->CurHardSymb]; 
  gchar symbol_code[4], *iter = symbol_code;

  DRAW_CHECK;
  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);

  cairo_select_font_face (cr, "Standard Symbols L ",
			  CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
  if ((status=cairo_status (cr)) != CAIRO_STATUS_SUCCESS) 
    {
      fprintf (stderr, "Cairo is unhappy in draw_mark : %s\n",
	       cairo_status_to_string(status));
    }
  /* we need here to center the symbol XXXX */
  cairo_set_font_size (cr, 10);
  cairo_move_to (cr, *x,*y);
  cairo_show_text (cr,symbol_code);
}

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
  int rect[4];
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  cairo_select_font_face (cr, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size(cr, 20);

  if ( flag == 1) boundingbox(Xgc,string, x,y,rect);

  if ( Abs(angle) <= 0.1) 
    {
      cairo_move_to (cr, x,y);
      cairo_show_text (cr, string);
      if ( flag == 1) 
	{
	  cairo_rectangle (cr,rect[0],rect[1],rect[2],rect[3]);
	  cairo_stroke (cr);
	}
    }
  else 
    {
      double rad_angle = angle * M_PI/180.0;
      /* cairo_text_extents_t extents; */
      cairo_save (cr);
      cairo_identity_matrix (cr);
      cairo_translate (cr, x,y);
      cairo_rotate (cr, rad_angle);
      cairo_move_to (cr, 0,0);
      cairo_show_text (cr, string);
      if ( flag == 1) 
	{
	  cairo_rectangle (cr,0,0,rect[2],rect[3]);
	  cairo_stroke (cr);
	}
      cairo_restore (cr);
    }
}

/*
 * To get the bounding rectangle of a string 
 */

static void boundingbox(BCG *Xgc,char *string, int x, int y, int *rect)
{ 
  cairo_t *cr =  Xgc->private->cairo_cr;
  cairo_text_extents_t extents;
  DRAW_CHECK;
  cairo_text_extents (cr,string, &extents);
  rect[0]= x ;
  rect[3]= extents.height;
  rect[1]= y + extents.y_bearing;
  rect[2]= extents.width +extents.x_bearing ;
}


/*

include <math.h>
include <pango/pangocairo.h>

static void draw_text (cairo_t *cr)
{
#define RADIUS 150
#define N_WORDS 10
#define FONT "Sans Bold 27"
  PangoLayout *layout;
  PangoFontDescription *desc;
  int i;
  cairo_translate (cr, RADIUS, RADIUS);
  layout = pango_cairo_create_layout (cr);
  pango_layout_set_text (layout, "Text", -1);
  desc = pango_font_description_from_string (FONT);
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  for (i = 0; i < N_WORDS; i++)
    {
      int width, height;
      double angle = (360. * i) / N_WORDS;
      double red;
      cairo_save (cr);
      red   = (1 + cos ((angle - 60) * G_PI / 180.)) / 2;
      cairo_set_rgb_color (cr, red, 0, 1.0 - red);
      cairo_rotate (cr, angle * G_PI / 180.);
      pango_cairo_update_layout (cr, layout);
      pango_layout_get_size (layout, &width, &height);
      cairo_move_to (cr, - ((double)width / PANGO_SCALE) / 2, - RADIUS);
      pango_cairo_show_layout (cr, layout);
      cairo_restore (cr);
    }
    g_object_unref (layout);
}

*/


