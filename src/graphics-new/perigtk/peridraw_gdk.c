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
 *
 *--------------------------------------------------------------------------*/

/*
 * clear a rectangle using background 
 */

static void cleararea(BCG *Xgc,const GdkRectangle *r)
{
  int old= xset_pattern(Xgc,Xgc->NumBackground);
  if ( r != NULL) 
    gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, TRUE,
		       r->x,r->y,r->width,r->height);
  else 
    gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, TRUE,
		       0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
  xset_pattern(Xgc,old);
}

/*
 * line segments arrows 
 */

static void drawline(BCG *Xgc,int x1, int yy1, int x2, int y2)
{
  
  gdk_draw_line(Xgc->private->drawable,Xgc->private->wgc, x1, yy1, x2, y2);
}

/* Draw a set of segments 
 * segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) 
 * for i=0 step 2 
 * n is the size of vx and vy 
 */

static void drawsegments(BCG *Xgc, int *vx, int *vy, int n, int *style, int iflag)
{
  
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
  
  Xgc->graphic_engine->generic->drawrectangles(Xgc,vects,fillvect,n);
}

/* Draw one rectangle with current line style */

static void drawrectangle(BCG *Xgc,const int rect[])
{ 
  
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, FALSE,
		     rect[0],rect[1],rect[2],rect[3]);
}

/* fill one rectangle, with current pattern */

static void fillrectangle(BCG *Xgc,const int rect[])
{ 
  
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, TRUE,rect[0],rect[1],rect[2],rect[3]);
}

/*
 * draw a set of rectangles, provided here to accelerate GraySquare for X11 device 
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : is the value of a function on the grid defined by x,y 
 *  on each rectangle the average value of z is computed 
 */

static  void fill_grid_rectangles(BCG *Xgc,const int x[],const int y[],const double z[],
				  int nx, int ny, int remap,const int *colminmax,
				  const double *zminmax,const int *colout)
{
  Xgc->graphic_engine->generic->fill_grid_rectangles(Xgc,x,y,z,nx,ny,remap,colminmax,zminmax,colout);
}

/*
 * draw a set of rectangles, provided here to accelerate GraySquare1 for X11 device 
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : of size (n1-1)*(n2-1)  gives the f-values on the middle 
 *  of each rectangle. 
 *  z[i,j] is the value on the middle of rectangle 
 *        P1= x[i],y[j] x[i+1],y[j+1]
 */

static void fill_grid_rectangles1(BCG *Xgc,const int x[],const int y[],const double z[],
				  int nr, int nc,int remap,const int *colminmax,
				  const double *zminmax)
{
  Xgc->graphic_engine->generic->fill_grid_rectangles1(Xgc,x,y,z,nr,nc,remap,colminmax,zminmax);
}

/*
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
 */

static void fillarcs(BCG *Xgc,int *vects, int *fillvect, int n) 
{
  
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
  
  Xgc->graphic_engine->generic->drawarcs(Xgc,vects,style,n);
}

/* Draw a single ellipsis or part of it */

static void drawarc(BCG *Xgc,int arc[])
{ 
  
  gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,FALSE,
	       arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
}

/* Fill a single elipsis or part of it with current pattern */

static void fillarc(BCG *Xgc,int arc[])
{ 
  
  gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,TRUE,
	       arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
}

/* 
 * Draw a set of (*n) polylines (each of which have (*p) points) 
 * with lines or marks 
 * drawvect[i] <= 0 use a mark for polyline i
 * drawvect[i] >  0 use a line style for polyline i 
 */

static void drawpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *drawvect,int n, int p)
{ 
  
  Xgc->graphic_engine->generic->drawpolylines(Xgc,vectsx,vectsy,drawvect,n,p);
}

/*
 * fills a set of polygons each of which is defined by 
 * (*p) points (*n) is the number of polygons 
 * the polygon is closed by the routine 
 * fillvect[*n] :         
 * if fillvect[i] == 0 draw the boundaries with current color 
 * if fillvect[i] > 0  draw the boundaries with current color 
 *               then fill with pattern fillvect[i]
 * if fillvect[i] < 0  fill with pattern - fillvect[i]
 *
 */

static void fillpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *fillvect,int n, int p)
{
  
  Xgc->graphic_engine->generic->fillpolylines(Xgc,vectsx,vectsy,fillvect,n,p);
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
      if ( gtk_store_points(n, vx, vy, closeflag)) 
	{
	  gdk_draw_lines(Xgc->private->drawable,Xgc->private->wgc, gtk_get_xpoints(), n1);
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
  if ( gtk_store_points(n, vx, vy, closeflag)) 
    {
      gdk_draw_polygon(Xgc->private->drawable,Xgc->private->wgc,TRUE,gtk_get_xpoints(), n1);
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
	  gdk_draw_points(Xgc->private->drawable,
			  Xgc->private->wgc,gtk_get_xpoints(), n);
	}
    }
  else 
    { 
      int i;
      for ( i=0; i< n ;i++) draw_mark(Xgc,vx+i,vy+i);
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
  
  Xgc->graphic_engine->generic->drawaxis(Xgc,alpha,nsteps,initpoint,size);
}

/*
 * Display numbers z[i] at location (x[i],y[i])
 *   with a slope alpha[i] (see displaystring), if flag==1
 *   add a box around the string, only if slope =0}
 */

static void displaynumbers(BCG *Xgc, int *x, int *y, int n, int flag, double *z, double *alpha)
{
  
  Xgc->graphic_engine->generic->displaynumbers(Xgc,x,y,n,flag,z,alpha);
}

/**
 * draw_pixbuf:
 * @Xgc: 
 * @pix: 
 * @src_x: 
 * @src_y: 
 * @dest_x: 
 * @dest_y: 
 * @width: 
 * @height: 
 * 
 * src_x and src_y are unused here. 
 * The pixbuf is scaled in order to be drawn in the destination 
 * rectangle.
 * 
 * 
 **/

static void draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,int dest_y,
			int width,int height)
{
  GdkPixbuf *pixbuf = pix;
  GdkPixbuf *scaled ;
  gint w = gdk_pixbuf_get_width (pixbuf);
  gint h = gdk_pixbuf_get_height (pixbuf);
  if ( w == width && h == height) 
    gdk_draw_pixbuf(Xgc->private->drawable,
		    Xgc->private->wgc,
		    pixbuf,
		    src_x,src_y,
		    dest_x,dest_y,
		    width,height,
		    GDK_RGB_DITHER_NONE,
		    0,0);
  else
    {
      scaled = gdk_pixbuf_scale_simple(pixbuf,width,height, GDK_INTERP_BILINEAR);
      gdk_draw_pixbuf(Xgc->private->drawable,
		      Xgc->private->wgc,
		      scaled,
		      src_x,src_y,
		      dest_x,dest_y,
		      width,height,
		      GDK_RGB_DITHER_NONE,
		      0,0);
      gdk_pixbuf_unref(scaled);
    }

}

static void draw_pixbuf_from_file(BCG *Xgc,const char *pix,int src_x,int src_y,
				  int dest_x,int dest_y,int width,int height)
{
  Xgc->graphic_engine->generic->draw_pixbuf_from_file(Xgc,pix,src_x,src_y,dest_x,dest_y,width,height);
}


/**
 * xset_clip:
 * @Xgc: 
 * @x: 
 * 
 * Set a clip zone (rectangle) 
 * 
 **/

static void xset_clip(BCG *Xgc,const GdkRectangle *r)
{
  Xgc->ClipRegionSet = 1;
  Xgc->CurClipRegion= *r;
  gdk_gc_set_clip_rectangle(Xgc->private->wgc, r);
}

/**
 * xset_unclip:
 * @Xgc: a #BCG  
 * 
 * unset clip zone 
 **/

static void xset_unclip(BCG *Xgc)
{
  static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
  Xgc->ClipRegionSet = 0;
  gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
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
      x[1] =Xgc->CurClipRegion.x;
      x[2] =Xgc->CurClipRegion.y;
      x[3] =Xgc->CurClipRegion.width;
      x[4] =Xgc->CurClipRegion.height;
    }
}

/**
 * xset_alufunction1:
 * @Xgc: a #BCG  
 * @num: 
 * 
 * unused 
 **/

static void xset_alufunction1(BCG *Xgc,int num)
{   
  Xgc->CurDrawFunction = Min(15,Max(0,num));
}


/**
 * xget_alufunction:
 * @Xgc: a #BCG  
 * 
 * unused 
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
 *  To change The X11-default dash style
 * if *value == 0, use a solid line, if *value != 0 
 * the dash style is specified by the xx vector of n values 
 * xx[3]={5,3,7} and *n == 3 means :  5white 3 void 7 white \ldots 
 * 
 **/

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
  if ( Xgc->CurPixmapStatus == 1) 
    {
      gdk_gc_set_rgb_fg_color(Xgc->private->stdgc, &Xgc->private->gcol_bg);
      gdk_draw_rectangle(Xgc->private->extra_pixmap,Xgc->private->stdgc, TRUE,
			 0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
    }
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
	      Xgc->CurPixmapStatus = 1;
	      pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
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
      Xgc->CurPixmapStatus = 0; 
    }
  else
    {
      /* removing the extra pixmap as the default drawable 
       * but extra_pixmap is not destroyed 
       */
      Xgc->private->drawable = (GdkDrawable *)Xgc->private->pixmap;
      Xgc->CurPixmapStatus = 0; 
    }
  
}


/**
 * xset_pattern:
 * @Xgc: 
 * @num: 
 * 
 * 
 * 
 * Returns: 
 **/

static int  xset_pattern(BCG *Xgc,int color)
{
  GdkColor temp;
  double rgb[3];
  int old = xget_pattern(Xgc);
  if ( old == color ) return old;
  if ( Xgc->private->colors == NULL) return 1 ;
  if ( gdk_gc_get_colormap(Xgc->private->wgc) == NULL) 
    {
      gdk_gc_set_colormap(Xgc->private->wgc,Xgc->private->colormap);
    }
  Xgc->CurColor = color = Max(1,color);
  nsp_get_color_rgb(Xgc,color,rgb,Xgc->private->colors);
  temp.red   = (guint16)  (rgb[0]*65535);
  temp.green = (guint16)  (rgb[1]*65535);
  temp.blue  = (guint16)  (rgb[2]*65535);
  gdk_gc_set_rgb_fg_color(Xgc->private->wgc,&temp);
  return old;
}



static  void xset_test(BCG *Xgc)
{
  Xgc->graphic_engine->generic->xset_test(Xgc);
}


/*
 * Text and symbols with pango 
 */

#define FONTNUMBER 6
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 12

/* Must be of size FONTMAXSIZE */

static const int pango_size[] = {8,10,12,14,18,24};
static char *pango_fonttab[] ={"Courier", "Standard Symbols L","Sans","Sans","Sans","Sans"};

static void nsp_fonts_finalize(BCG *Xgc)
{
  if ( Xgc->private->layout != NULL) 
    {
      g_object_unref ( Xgc->private->layout);Xgc->private->layout=NULL;
      g_object_unref ( Xgc->private->mark_layout);Xgc->private->mark_layout=NULL;
      pango_font_description_free ( Xgc->private->desc); Xgc->private->desc=NULL;
      pango_font_description_free ( Xgc->private->mark_desc);Xgc->private->mark_desc=NULL;
    }
}

static void nsp_fonts_initialize(BCG *Xgc)
{
  if ( Xgc->private->layout == NULL) 
    {
      Xgc->private->context = gtk_widget_get_pango_context (Xgc->private->drawing);
      Xgc->private->layout = pango_layout_new (Xgc->private->context);
      Xgc->private->mark_layout = pango_layout_new (Xgc->private->context);
      
      Xgc->private->desc = pango_font_description_new();
      Xgc->private->mark_desc = pango_font_description_from_string(pango_fonttab[1]);
    }
}

static void loadfamily(char *name, int *j)
{
  /* */
}

static void queryfamily(char *name, int *j,int *v3)
{
  /* */
}

/* set up the private layout with proper font 
 * Xgc->fontSize is used to keep track of previous value
 *  if <=0 then previous value was an indice in the pango_size array 
 *  if > 0 then previous value was an absolute value in pixel.
 */

static void xset_font(BCG *Xgc,int fontid, int fontsize,int full)
{ 
  int i,fsiz, changed = TRUE;
  i = Min(FONTNUMBER-1,Max(fontid,0));
  if ( full == TRUE ) 
    {
      fsiz = Max(1,fontsize);
      changed = Xgc->fontId != i || Xgc->fontSize != fsiz ;
      Xgc->fontSize = fsiz;
   }
  else
    {
      fsiz = Min(FONTMAXSIZE-1,Max(fontsize,0));
      changed = Xgc->fontId != i || Xgc->fontSize != - fsiz ;
      Xgc->fontSize = - fsiz ;
      fsiz = pango_size[fsiz];
    }
  if ( changed  ) 
    {
      Xgc->fontId = i;
      pango_font_description_set_family(Xgc->private->desc, pango_fonttab[i]);
      pango_font_description_set_absolute_size (Xgc->private->desc, fsiz * PANGO_SCALE);
      pango_layout_set_font_description (Xgc->private->layout, Xgc->private->desc);
    }
}

/* To get the  id and size of the current font */

static void  xget_font(BCG *Xgc,int *font,int full)
{
  font[0] = Xgc->fontId ;
  if ( full == TRUE )
    {
      font[1] = (Xgc->fontSize <= 0) ?
	pango_size[-Xgc->fontSize]: Xgc->fontSize;
    }
  else
    {
      font[1] = (Xgc->fontSize <= 0) ? -Xgc->fontSize : 0 ;
    }
}

/* set up the private mark_layout with proper font 
 *
 */

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

/* To get the current mark id */

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}


/* drawing marks with pango 
 */

static const int symbols[] = 
  {
    0x22C5, /* lozenge */
    0x002B, /* plus sign */
    0x00D7, /* multiplication sign */
    0x2217, /* asterisk operator */
    0x2666, /* black diamond suit */
    0x25CA, /* lozenge */
    0x0394, /* greek capital letter delta */
    0x2207, /* nabla  */
    0x2663, /* black club suit */
    0x2295, /* circled plus */
    0x2665, /* black heart suit */
    0x2660, /* black spade suit */
    0x2297, /* circled times */
    0x2022, /* bullet */
    0x00B0  /* degree sign */
  };


/* utility to rotate a string */

static void get_rotated_layout_bounds (PangoLayout  *layout, const PangoMatrix *matrix, GdkRectangle *rect)
{
  gdouble x_min = 0, x_max = 0, y_min = 0, y_max = 0;
  PangoRectangle logical_rect;
  gint i, j;
  pango_layout_get_extents (layout, NULL, &logical_rect);
  for (i = 0; i < 2; i++)
    {
      gdouble x = (i == 0) ? logical_rect.x : logical_rect.x + logical_rect.width;
      for (j = 0; j < 2; j++)
	{
	  gdouble y = (j == 0) ? logical_rect.y : logical_rect.y + logical_rect.height;
	  
	  gdouble xt = (x * matrix->xx + y * matrix->xy) / PANGO_SCALE + matrix->x0;
	  gdouble yt = (x * matrix->yx + y * matrix->yy) / PANGO_SCALE + matrix->y0;
	  
	  if (i == 0 && j == 0)
	    {
	      x_min = x_max = xt;
	      y_min = y_max = yt;
	    }
	  else
	    {
	      if (xt < x_min)
		x_min = xt;
	      if (yt < y_min)
		y_min = yt;
	      if (xt > x_max)
		x_max = xt;
	      if (yt > y_max)
		y_max = yt;
	    }
	}
    }
  
  rect->x = floor (x_min);
  rect->width = ceil (x_max) - rect->x;
  rect->y = floor (y_min);
  rect->height = floor (y_max) - rect->y;
}


/* 
 * FIXME: flag is unused since the rectangle is drawn in Graphics-IN.c 
 *        to deal with the case when string can be on multiple lines 
 *        but pango could make this directly. 
 * maybe we should change the display an consider that (x,y) is the baseline 
 * of the string not its lower left boundary.
 * Note that if the string contains \n then the layout will have multiple lines 
 * 
 */

static void displaystring(BCG *Xgc,const char *str, int x, int y, int flag,double angle,
			  gr_str_posx posx, gr_str_posy posy )
{
  PangoRectangle ink_rect,logical_rect;
  int  height,width;
  ;
  pango_layout_set_text (Xgc->private->layout, str, -1);
  /* used to position the descent of the last line of layout at y */
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
  if ( posy == GR_STR_YBASELINE ) 
    {
      /* we want (x,y) to be at the baseline of the first layout line 
       */
      PangoLayoutLine *line;
      line = pango_layout_get_line(Xgc->private->layout,0);
      pango_layout_line_get_pixel_extents(line, &ink_rect,&logical_rect);
    }
  if ( Abs(angle) >= 0.1) 
    {
      double xt,yt;
      GdkRectangle rect;
      PangoMatrix matrix = PANGO_MATRIX_INIT; 
      pango_matrix_rotate (&matrix, - angle );
      pango_context_set_matrix (Xgc->private->context, &matrix);
      pango_layout_context_changed (Xgc->private->layout);
      pango_layout_get_extents(Xgc->private->layout,&ink_rect,&logical_rect);
      /* 
       * in gdk_draw_layout x and y specify the position of the top left corner 
       * of the bounding box (in device space) of the transformed layout. 
       * Here when alpha = 0, (x,y) is the lower left point of the bounding box 
       * of the string we want the string to rotate around this point. 
       * thus we cannot call gdk_draw_layout with (x,y) directly.
       */
      xt = 0 * matrix.xx + -height * matrix.xy + matrix.x0;
      yt = 0 * matrix.yx + -height * matrix.yy + matrix.y0;
      get_rotated_layout_bounds (Xgc->private->layout, &matrix,&rect);
      gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,
		       x+rect.x+xt,y+rect.y+yt,Xgc->private->layout);
      pango_context_set_matrix (Xgc->private->context,NULL);
      pango_layout_context_changed (Xgc->private->layout);
      if (flag == TRUE ) 
	{
	  /* draw the enclosing polyline 
	   */
	  int vx[]={x,x,x,x},vy[]={y,y,y,y};
	  double dx,dy;
	  dx =  0 * matrix.xx + -height * matrix.xy + matrix.x0;
	  dy =  0 * matrix.yx + -height * matrix.yy + matrix.x0;
	  vx[1] += dx; vy[1] += dy;
	  dx =  width * matrix.xx + -height * matrix.xy + matrix.x0;
	  dy =  width * matrix.yx + -height * matrix.yy + matrix.x0;
	  vx[2] += dx; vy[2] += dy;
	  dx =  width * matrix.xx + 0 * matrix.xy + matrix.x0;
	  dy =  width * matrix.yx + 0 * matrix.yy + matrix.x0;
	  vx[3] += dx; vy[3] += dy;
	  drawpolyline(Xgc,vx, vy,4,1);
	}
    }
  else
    {
      int xpos=x,ypos=y;
      /* horizontal string */
      switch( posx ) 
	{
	case GR_STR_XLEFT: xpos = x; break;
	case GR_STR_XCENTER: xpos = x - width/2; break;
	case GR_STR_XRIGHT: xpos = x - width; break;
	}
      switch( posy ) 
	{
	case GR_STR_YBOTTOM: ypos = y -height; break;
	case GR_STR_YCENTER:  ypos = y - height/2; break;
	case GR_STR_YBASELINE: ypos = y + logical_rect.y; break;
	case GR_STR_YUP:  ypos = y ; break;
	} 
      if ( flag == TRUE )
	gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc,FALSE,xpos,ypos,width,height);
      gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,xpos,ypos,Xgc->private->layout);
    }
}

static void boundingbox(BCG *Xgc,const char *string, int x, int y, int *rect)
{
  int width, height;
  pango_layout_set_text (Xgc->private->layout, string, -1);
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
  rect[0]=x;rect[1]=y+height;rect[2]=width;rect[3]=height;
}

/* draw a mark centred at (x,y) using the 
 * symbol font and the mark_layout.
 */

static void draw_mark(BCG *Xgc,int *x, int *y)
{
  double dx,dy;
  PangoRectangle ink_rect;
  int code = symbols[Xgc->CurHardSymb]; 
  gchar symbol_code[4], *iter = symbol_code;
  ;
  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);
  pango_layout_set_text (Xgc->private->mark_layout,symbol_code, -1);
  pango_layout_get_extents(Xgc->private->mark_layout,&ink_rect,NULL);
  dx = PANGO_PIXELS(( ink_rect.x + ink_rect.width/2.0));
  dy = PANGO_PIXELS(( ink_rect.y + ink_rect.height/2.0));
  gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,*x-dx,*y-dy,Xgc->private->mark_layout);
  if (0) 
    {
      /* draw the ink_rectangle aroud the mark */
      int i;
      double rect[]={ink_rect.x,ink_rect.y,ink_rect.width,ink_rect.height};
      int myrect[]={*x-dx,*y-dy,0,0};
      for ( i=0; i < 4 ; i++) myrect[i] += PANGO_PIXELS(rect[i]);
      drawrectangle(Xgc,myrect);
    }
}


void xstring_pango(BCG *Xgc,char *str,int rect[],char *font,int size,int markup,int position)
{
  
}





