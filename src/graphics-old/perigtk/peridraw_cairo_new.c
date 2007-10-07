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


/* clear a rectangle by drawing with the background color 
 * plot2d()
 * xclip(1,0,2,2)
 * xset('color',3);xfrect(1,0,2,6);
 * xclea(1,0,2,6)
 */

static void cleararea(BCG *Xgc, int x, int y, int w, int h)
{
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  cairo_set_source_rgb(cr,
		       Xgc->private->gcol_bg.red/65535.0,
		       Xgc->private->gcol_bg.green/65535.0,
		       Xgc->private->gcol_bg.blue/65535.0);
  cairo_rectangle (cr,x,y,w,h);
  cairo_fill (cr);
}

/*
 * line segments arrows 
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

/* Draw or Fill a single ellipsis or part of it */

static void _draw_fill_arc(BCG *Xgc,int arc[], int flag)
{ 
  cairo_t *cr =  Xgc->private->cairo_cr;
  double xc, yc, radius, angle1, angle2; 
  DRAW_CHECK;
  xc = arc[0]+arc[2]/2.0;
  yc = arc[1]+arc[3]/2.0;
  radius = arc[2]/2.0;
  angle1 = arc[4]* (M_PI / (64*180.));
  angle2 = angle1 + arc[5]* (M_PI / (64*180.));
  cairo_new_path (cr);
  cairo_save (cr);
  cairo_translate (cr, xc,yc);
  cairo_scale (cr, arc[2]/2.0 ,- arc[3]/2.0);
  cairo_arc (cr, 0.0, 0.0, 1.0, angle1, angle2);
  if ( flag == TRUE ) 
    {
      cairo_line_to(cr,0,0);
      cairo_close_path(cr);
      cairo_restore (cr);
      cairo_fill(cr);
    }
  else 
    {
      cairo_restore (cr);
      cairo_stroke(cr);
    }
}

/* Draw a single ellipsis or part of it **/


static void drawarc(BCG *Xgc,int arc[])
{ 
  _draw_fill_arc(Xgc,arc,FALSE);
}

/* Fill a single elipsis or part of it with current pattern */

static void fillarc(BCG *Xgc,int arc[])
{ 
  _draw_fill_arc(Xgc,arc,TRUE);
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

static void filldrawpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag, int color );

static void fillpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *fillvect,int n, int p)
{
  DRAW_CHECK;
  /* Xgc->graphic_engine->generic->fillpolylines(Xgc,vectsx,vectsy,fillvect,n,p); */
  int dash,color,i;
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{ 
	  /* fill + draw */
	  Xgc->graphic_engine->xset_pattern(Xgc,fillvect[i]);
	  filldrawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1,color);
	}
      else  if (fillvect[i] == 0 )
	{
	  /* just draw */
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  Xgc->graphic_engine->drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else 
	{
	  /* fill */
	  Xgc->graphic_engine->xset_pattern(Xgc,-fillvect[i]);
	  Xgc->graphic_engine->fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);


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

static void filldrawpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag, int color ) 
{
  cairo_t *cr =  Xgc->private->cairo_cr;
  cairo_status_t status;
  int n1,i;
  /*   DRAW_CHECK; */
  if (closeflag == 1) n1 = n+1;else n1= n;
  cairo_new_path(cr); 
  cairo_move_to(cr, vx[0],vy[0]);
  for ( i = 1 ; i < n ; i++ ) 
    cairo_line_to(cr,vx[i],vy[i]);
  if ( closeflag == 1) cairo_line_to(cr,vx[0],vy[0]);
  cairo_fill_preserve(cr);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  cairo_stroke(cr);
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

/* cairo: we use in fact next routine i.e pango + cairo  */
#if 0 
static void draw_mark_cairo(BCG *Xgc,int *x, int *y)
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
#endif 
 

/* pango layout and cairo 
 *
 */ 

static void draw_mark(BCG *Xgc,int *x, int *y)
{ 
  double dx,dy;
  PangoRectangle ink_rect;
  cairo_t *cr =  Xgc->private->cairo_cr;
  int code = symbols[Xgc->CurHardSymb]; 
  gchar symbol_code[4], *iter = symbol_code;
  DRAW_CHECK;
  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);
  pango_layout_set_text (Xgc->private->mark_layout,symbol_code, -1);
  pango_layout_get_extents(Xgc->private->mark_layout,&ink_rect,NULL);
  dx = PANGO_PIXELS(( ink_rect.x + ink_rect.width/2.0));
  dy = PANGO_PIXELS(( ink_rect.y + ink_rect.height/2.0));
  cairo_move_to (cr, *x-dx,*y-dy);
  pango_cairo_update_layout (cr,Xgc->private->mark_layout);
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

#if 0 
static void displaystring_cairo(BCG *Xgc,char *string, int x, int y,  int flag, double angle) 
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
#endif 

/*
 * To get the bounding rectangle of a string 
 */
#if 0 
static void boundingbox_cairo(BCG *Xgc,char *string, int x, int y, int *rect)
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
#endif 

/* pango layout + cairo */

static void displaystring(BCG *Xgc,char *string, int x, int y,  int flag, double angle) 
{ 
  int width,height;
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  pango_layout_set_text (Xgc->private->layout, string, -1);
  /* used to position the descent of the last line of layout at y */
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
  if ( Abs(angle) <= 0.1) 
    {
      cairo_move_to (cr, x,y-height);
      pango_cairo_update_layout (cr,Xgc->private->layout);
      pango_cairo_show_layout (cr,Xgc->private->layout);
      /* horizontal string */
      if (flag == 1) /*  flag == 1)  */
	{
	  cairo_rectangle (cr,x,y-height,width,height);
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
      cairo_move_to (cr, 0,-height);
      pango_layout_set_text (Xgc->private->layout,string, -1);
      pango_cairo_update_layout (cr,Xgc->private->layout);
      pango_cairo_show_layout (cr,Xgc->private->layout);
      if ( flag == 1)  
	{
	  cairo_rectangle (cr,0,-height,width,height);
	  cairo_stroke (cr);
	}
      cairo_restore (cr);
    }
}


static void boundingbox(BCG *Xgc,char *string, int x, int y, int *rect)
{
  int width, height;
  DRAW_CHECK;
  pango_layout_set_text (Xgc->private->layout, string, -1);
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
  rect[0]=x;rect[1]=y+height;rect[2]=width;rect[3]=height;
}


static void draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  /* here pix must be a GdkPixbuf */
  GdkPixbuf *pixbuf=pix; 
  int w,h;
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  cairo_save (cr);
  w = gdk_pixbuf_get_width (pixbuf);
  h = gdk_pixbuf_get_height (pixbuf);
  /* cairo_rectangle (cr,dest_x,dest_y,width,height);
   * cairo_stroke (cr);
   */
  cairo_translate(cr,  dest_x, dest_y);
  cairo_scale (cr, ((double) width)/w, ((double) height)/h);
  gdk_cairo_set_source_pixbuf (cr,pixbuf,0,0);
  cairo_paint (cr);
  cairo_restore (cr);
}

static void draw_pixbuf_from_file(BCG *Xgc,const char *fname,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  int w,h, status ;
  cairo_surface_t *image;
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  image = cairo_image_surface_create_from_png (fname);
  cairo_save (cr);
  if ((status=cairo_status (cr)) != CAIRO_STATUS_SUCCESS) 
    {
      fprintf (stderr,"Cairo is unhappy in pixbuf_from_file: %s\n",
	       cairo_status_to_string(status));
      cairo_restore (cr);
      return;
    }
  w = cairo_image_surface_get_width (image);
  h = cairo_image_surface_get_height (image);
  /* cairo_rectangle (cr,dest_x,dest_y,width,height);
   *  cairo_stroke (cr);
   */
  cairo_translate(cr,  dest_x, dest_y);
  cairo_scale (cr, ((double) width)/w, ((double) height)/h);
  cairo_set_source_surface (cr, image,0,0);
  cairo_paint (cr);
  cairo_restore (cr);
  cairo_surface_destroy (image);
}





/**
 * xset_clip:
 * @Xgc: a graphic context 
 * @x: rectangle to be used for clipping 
 * 
 * Set a clip zone (rectangle) 
 * 
 **/

static void xset_clip(BCG *Xgc,int x[])
{
  int i;
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  Xgc->ClipRegionSet = 1;
  for (i=0 ; i < 4 ; i++)   Xgc->CurClipRegion[i]= x[i];
  cairo_new_path (cr);
  cairo_rectangle(cr,x[0],x[1],x[2],x[3]);
  cairo_clip (cr);
}

/**
 * xset_unclip:
 * @Xgc: a #BCG  
 * 
 * unset clip zone 
 **/

static void xset_unclip(BCG *Xgc)
{
  cairo_t *cr =  Xgc->private->cairo_cr;
  DRAW_CHECK;
  if ( Xgc->ClipRegionSet == 0 ) return;
  Xgc->ClipRegionSet = 0;
  cairo_reset_clip(cr);
}

/**
 * xget_clip:
 * @Xgc: a #BCG  
 * @x: an int pointer 
 * 
 * Get the boundaries of the current clip zone 
 **/


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

/**
 * xset_alufunction1:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * 
 **/
static void xset_alufunction1(BCG *Xgc,int num) 
{   
  cairo_t *cr =  Xgc->private->cairo_cr; 
  int value ;  
  /* GdkColor temp = {0,0,0,0}; */
  Xgc->CurDrawFunction = Min(15,Max(0,num));
  value = AluStruc_[Xgc->CurDrawFunction].id;
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
      cairo_set_operator (cr, CAIRO_OPERATOR_XOR);
      /* temp.pixel = Xgc->private->gcol_fg.pixel ^ Xgc->private->gcol_bg.pixel ;
	 gdk_gc_set_foreground(Xgc->private->wgc, &temp);
	 gdk_gc_set_background(Xgc->private->wgc, &Xgc->private->gcol_bg);
	 gdk_gc_set_function(Xgc->private->wgc,GDK_XOR);
      */
      break;
    default :
      cairo_set_operator (cr, CAIRO_OPERATOR_OVER);
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

/**
 * xget_alufunction:
 * @Xgc: a #BCG  
 * 
 * 
 * 
 * Returns: 
 **/

static int xget_alufunction(BCG *Xgc)
{ 
  return  Xgc->CurDrawFunction ;
}


/**
 * xset_dashstyle:
 * @Xgc: a #BCG  
 * @value: 
 * @xx: 
 * @n: 
 * 
 * 
 **/

static void xset_dashstyle(BCG *Xgc,int value, int *xx, int *n)
{
  cairo_t *cr =  Xgc->private->cairo_cr; 
  if ( value == 0) 
    {
      /* FIXME: proper width in double ? */
      cairo_set_line_width(cr,(Xgc->CurLineWidth <= 1) ? 1 : Xgc->CurLineWidth*0.5);
      cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT);
      cairo_set_line_join (cr, CAIRO_LINE_JOIN_ROUND);
      /* remove dash */
      cairo_set_dash (cr,NULL,0,0.0);
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
      cairo_set_dash (cr,buffdash,*n,0.0);
      cairo_set_line_width(cr,(Xgc->CurLineWidth <= 1) ? 1 : Xgc->CurLineWidth*0.5);
      cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT);
      cairo_set_line_join (cr, CAIRO_LINE_JOIN_ROUND);
      /* 
	 gdk_gc_set_dashes(Xgc->private->wgc, 0, buffdash, *n);
	 gdk_gc_set_line_attributes(Xgc->private->wgc, 
	 (Xgc->CurLineWidth == 0 ) ? 1 : Xgc->CurLineWidth,
	 GDK_LINE_ON_OFF_DASH, GDK_CAP_BUTT, GDK_JOIN_ROUND);
      */
    }
}

/**
 * pixmap_clear_rect:
 * @Xgc: a #BCG 
 * @x: integer 
 * @y: integer 
 * @w: integer
 * @h: integer
 * 
 * clears a rectangle defined by its upper-left position (x,y) and dimensions (w,h) 
 * in the extra_pixmap using the background color.
 **/

static void pixmap_clear_rect(BCG *Xgc,int x, int y, int w, int h)
{
  cairo_t *cr =  Xgc->private->cairo_cr;
  cairo_set_source_rgb(cr,
		       Xgc->private->gcol_bg.red/65535.0,
		       Xgc->private->gcol_bg.green/65535.0,
		       Xgc->private->gcol_bg.blue/65535.0);
  cairo_rectangle (cr,0,0, Xgc->CWindowWidth, Xgc->CWindowHeight);
  cairo_fill (cr);
}

/**
 * pixmap_resize:
 * @Xgc: a #BCG 
 * 
 * resizes, if present the extra_pixmap according to window size change 
 **/

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
      if ( Xgc->private->cairo_cr != NULL) cairo_destroy (Xgc->private->cairo_cr);
      Xgc->private->cairo_cr = gdk_cairo_create (Xgc->private->extra_pixmap);
      pixmap_clear_rect(Xgc,0,0,x,y);
    }
} 


/**
 * xset_pixmapOn:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * 
 **/

static void xset_pixmapOn(BCG *Xgc,int num)
{ 
  int num1= Min(Max(num,0),2);
  if ( Xgc->CurPixmapStatus == num1 ) return;
  if ( num1 == 1 )
    {
      /* switch to extra pixmap mode */
      if ( Xgc->private->extra_pixmap != NULL) 
	{
	  Xgc->private->drawable = Xgc->private->extra_pixmap;
	  if ( Xgc->private->cairo_cr != NULL) cairo_destroy (Xgc->private->cairo_cr);
	  Xgc->private->cairo_cr = gdk_cairo_create (Xgc->private->extra_pixmap);
	  Xgc->CurPixmapStatus = 1;
	}
      else 
	{
	  GdkDrawable *temp ;
	  /* create a new pixmap */
	  temp = (GdkDrawable *) gdk_pixmap_new(Xgc->private->drawing->window,
						Xgc->CWindowWidth, Xgc->CWindowHeight,
						-1);
	  if ( temp  == NULL ) 
	    {
	      xinfo(Xgc,"Not enough space to switch to Animation mode");
	    }
	  else 
	    {
	      xinfo(Xgc,"Animation mode is on,( xset('pixmap',0) to leave)");
	      Xgc->private->drawable = Xgc->private->extra_pixmap = temp;
	      if ( Xgc->private->cairo_cr != NULL) cairo_destroy (Xgc->private->cairo_cr);
	      Xgc->private->cairo_cr = gdk_cairo_create (Xgc->private->extra_pixmap);
	      pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
	      Xgc->CurPixmapStatus = 1;
	    }
	}
    }
  else if ( num1 == 0 ) 
    {
      /* deleting and removing the extra pixmap as the default drawable */
      xinfo(Xgc," ");
      gdk_pixmap_unref((GdkPixmap *) Xgc->private->extra_pixmap);
      Xgc->private->extra_pixmap = NULL;
      Xgc->private->drawable = (GdkDrawable *)Xgc->private->pixmap;
      if ( Xgc->private->cairo_cr != NULL) cairo_destroy (Xgc->private->cairo_cr);
      Xgc->private->cairo_cr = gdk_cairo_create (Xgc->private->pixmap);
      Xgc->CurPixmapStatus = 0; 
    }
  else
    {
      /* removing the extra pixmap as the default drawable 
       * but extra_pixmap is not destroyed 
       */
      Xgc->private->drawable = (GdkDrawable *)Xgc->private->pixmap;
      if ( Xgc->private->cairo_cr != NULL) cairo_destroy (Xgc->private->cairo_cr);
      Xgc->private->cairo_cr = gdk_cairo_create (Xgc->private->pixmap);
      Xgc->CurPixmapStatus = 0; 
    }
  
}


/**
 * nsp_gtk_set_color:
 * @Xgc: a #BCG  
 * @col: 
 * 
 * 
 **/

static void nsp_gtk_set_color(BCG *Xgc,int col)
{
  cairo_t *cr =  Xgc->private->cairo_cr; 
  col = Max(0,Min(col,Xgc->Numcolors + 1));
  if ( Xgc->CurColor == col ) return; 
  Xgc->CurColor = col;
  if (Xgc->private->colors  == NULL) return;
  cairo_set_source_rgb(cr,Xgc->private->colors[col].red/((double) 0xffff),
		       Xgc->private->colors[col].green/((double) 0xffff),
		       Xgc->private->colors[col].blue/((double) 0xffff));
}


/* export cairo graphics to files in various formats 
 * This cairo facilities should be checked by configure 
 */

#include <cairo-pdf.h>
#include <cairo-ps.h>
#include <cairo-svg.h>

int nsp_cairo_export(BCG *Xgc,int win_num,int colored, const char *bufname,char *driver,char option)
{
  /* default is to follow the window size */
  int width = Xgc->CWindowWidth; 
  int height= Xgc->CWindowHeight;
  int uc;
  cairo_surface_t *surface;
  cairo_t *cr, *cr_current;
  if ( strcmp(driver,"cairo-pdf")==0 ) 
    {
      surface = cairo_pdf_surface_create (bufname,width, height );
    }
  else if ( strcmp(driver,"cairo-svg")==0 )
    {
      surface = cairo_svg_surface_create (bufname,width, height );
    }
  else if ( strcmp(driver,"cairo-ps")==0 )
    {
      surface = cairo_ps_surface_create (bufname,width, height );
    }
  else if ( strcmp(driver,"cairo-png")==0 )
    {
      surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,  width, height );
    }
  else 
    {
      return FAIL;
    }
  if ( surface == NULL) return FAIL; 
  cr = cairo_create (surface);
  if ( cr == NULL) return FAIL; 
  cairo_save (cr);
  /* take care here that Xgc can be a non cairo Xgc */
  if ( Xgc->graphic_engine != &Cairo_gengine ) 
    {
      Sciprintf("cannot export a non cairo graphic\n");
    }
  else 
    {
      cr_current =  Xgc->private->cairo_cr;
      Xgc->private->cairo_cr = cr;
      uc = Xgc->graphic_engine->xget_usecolor(Xgc);
      fprintf(stderr,"colored = %d\n",colored);
      if (colored==1 ) 
	Xgc->graphic_engine->xset_usecolor(Xgc,1);
      else
	Xgc->graphic_engine->xset_usecolor(Xgc,0);
      Xgc->graphic_engine->tape_replay(Xgc,win_num);
      Xgc->private->cairo_cr = cr_current;
      Xgc->graphic_engine->xset_usecolor(Xgc,uc);
      cairo_show_page (cr);
      cairo_restore (cr);
      cairo_destroy (cr); 
      cairo_surface_destroy (surface); 
      return OK;
    }
}

