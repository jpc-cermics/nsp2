/* Nsp
 * Copyright (C) 1998-2017 Jean-Philippe Chancelier Enpc/Cermics
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
 * Text and symbols with pango and cairo
 */

#define FONTNUMBER 6
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 17

/* Must be of size FONTMAXSIZE */

static const int pango_size[] = {8,10,12,14,18,24};
static char *pango_fonttab[] ={"Courier", "Standard Symbols L","Sans","Sans","Sans","Sans"};

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
    0x002B, /* degree sign */
    0x25A0, /* black square */
    0x25A1  /* white square */
  };

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
  cairo_t *cr = Xgc->private->cairo_drawable_cr;
  /* when exporting, figure background should not
   * be painted when  Xgc->figure_bg_draw is not TRUE
   */
  if ( Xgc->figure_bg_draw == TRUE )
    {
      cairo_set_source_rgb(cr,
			   Xgc->private->gcol_bg.red/65535.0,
			   Xgc->private->gcol_bg.green/65535.0,
			   Xgc->private->gcol_bg.blue/65535.0);
      cairo_rectangle (cr,0,0, Xgc->CWindowWidth, Xgc->CWindowHeight);
      cairo_fill (cr);
    }
}

/* clear a rectangle zone by painting with the background color
 * if r is NULL then entire graphics window is painted.
 */

static void cleararea(BCG *Xgc,const GdkRectangle *r)
{
  cairo_t *cr= Xgc->private->cairo_drawable_cr;
  int old= xset_color(Xgc,Xgc->NumBackground);
  if ( r != NULL)
    cairo_rectangle (cr,r->x,r->y,r->width,r->height);
  else
    cairo_rectangle (cr,0,0,Xgc->CWindowWidth, Xgc->CWindowHeight);
  cairo_fill (cr);
  xset_color(Xgc,old);
}

/*
 * line segments arrows
 */

static void drawline(BCG *Xgc,double x1, double yy1, double x2, double y2)
{
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  cairo_move_to(cr,x1,yy1);
  cairo_line_to(cr,x2,y2);
  cairo_stroke(cr);
}

/* Draw a set of segments
 * segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1])
 * for i=0 step 2
 * n is the size of vx and vy
 */

static void drawsegments(BCG *Xgc, double *vx, double *vy, int n, int *style, int *width)
{
  /* cairo_t *cr =  Xgc->private->cairo_cr; */
  /* test */
  /* cairo_set_antialias(cr,CAIRO_ANTIALIAS_NONE); */
  Xgc->graphic_engine->generic->drawsegments(Xgc,vx,vy,n,style,width);
}

/* Draw a set of arrows
 * arrows are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1])
 * for i=0 step 2
 * n is the size of vx and vy
 * as is 10*arsize (arsize) the size of the arrow head in pixels
 */

static void drawarrows(BCG *Xgc, double *vx, double *vy, int n, int as, int *style, int iflag)
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
#if 0 
static void drawrectangles(BCG *Xgc,const double *vects,const int *fillvect, int n)
{
  Xgc->graphic_engine->generic->drawrectangles(Xgc,vects,fillvect,n);
}
#endif 

/* Draw one rectangle with current line style */

static void drawrectangle(BCG *Xgc,const double rect[])
{
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  cairo_rectangle (cr,rect[0],rect[1],rect[2],rect[3]);
  cairo_stroke (cr);
}

/* fill one rectangle, with current pattern */

static void fillrectangle(BCG *Xgc,const double rect[])
{
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  int cpat = Xgc->graphic_engine->xget_color(Xgc);
#if 0
  {
    cairo_pattern_t *pat1;  
    int length=10, xr = rect[0]+rect[2],yr=rect[1], yd=rect[1]+rect[3];
    /* right */
    pat1 = cairo_pattern_create_linear(xr,rect[1],xr+length,yr);
    cairo_pattern_add_color_stop_rgb(pat1, 0, 0.5, 0.5, 0.5);
    cairo_pattern_add_color_stop_rgb(pat1, 1.0, 1, 1, 1);
    cairo_rectangle (cr,xr,yr,length,rect[3]);
    cairo_set_source(cr, pat1);
    cairo_fill(cr);  
    cairo_pattern_destroy(pat1);
    /* down */
    pat1 = cairo_pattern_create_linear(rect[0],yd,rect[0],yd+length);
    cairo_pattern_add_color_stop_rgb(pat1, 0, 0.5, 0.5, 0.5);
    cairo_pattern_add_color_stop_rgb(pat1, 1.0, 1, 1, 1);
    cairo_rectangle (cr,rect[0],yd,rect[2],length);
    cairo_set_source(cr, pat1);
    cairo_fill(cr);  
    /* corner */
    pat1 = cairo_pattern_create_linear(xr,yd,xr+length/2.0,yd+length/2.0);
    cairo_pattern_add_color_stop_rgb(pat1, 0, 0.5, 0.5, 0.5);
    cairo_pattern_add_color_stop_rgb(pat1, 1.0, 1, 1, 1);
    cairo_rectangle (cr,xr,yd,length,length);
    cairo_set_source(cr, pat1);
    cairo_fill(cr);  
    cairo_pattern_destroy(pat1);
  }
#endif 
  Xgc->graphic_engine->xset_color(Xgc,cpat);
  cairo_rectangle (cr,rect[0],rect[1],rect[2],rect[3]);
  cairo_fill (cr);
}

/*
 * Circles and Ellipsis
 * Draw or fill a set of ellipsis or part of ellipsis
 * Each is defined by 6-parameters,
 * ellipsis i is specified by $vect[6*i+k]_{k=0,5}= x,y,width,height,angle1,angle2$
 * <x,y,width,height> is the bounding box
 * angle1,angle2 specifies the portion of the ellipsis
 * caution : angle=degreangle*64
 */

/* Draw or Fill a single ellipsis or part of it */

static void _draw_fill_arc(BCG *Xgc, double arc[], int flag)
{
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  double xc, yc, /* radius,*/ angle1, angle2;
  xc = arc[0]+arc[2]/2.0;
  yc = arc[1]+arc[3]/2.0;
  /* radius = arc[2]/2.0; */
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

/* Draw a single ellipsis or part of it */

static void drawarc(BCG *Xgc, double arc[])
{
  _draw_fill_arc(Xgc,arc,FALSE);
}

/* Fill a single elipsis or part of it with current pattern */

static void fillarc(BCG *Xgc, double arc[])
{
  _draw_fill_arc(Xgc,arc,TRUE);
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

static void fillpolylines(BCG *Xgc, const double *vectsx, const double *vectsy, int *fillvect,int n, int p)
{
  Xgc->graphic_engine->generic->fillpolylines(Xgc,vectsx,vectsy,fillvect,n,p);
}

/*
 * Only draw one polygon  with current line style
 * according to *closeflag : it's a polyline or a polygon
 * n is the number of points of the polyline
 */

static void drawpolyline(BCG *Xgc, const double *vx, const double *vy, int n,int closeflag)
{
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  cairo_status_t status;
  int n1,i;
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
 * Note that it also draw the polyline 
 */

static void fillpolyline(BCG *Xgc, const double *vx, const double *vy, int n,int closeflag, int color)
{
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  cairo_status_t status;
  int i;
  /* if (closeflag == 1) n1 = n+1;else n1= n; */
  cairo_new_path(cr);
  cairo_move_to(cr, vx[0],vy[0]);
  for ( i = 1 ; i < n ; i++ )
    cairo_line_to(cr,vx[i],vy[i]);
  if ( closeflag == 1) cairo_line_to(cr,vx[0],vy[0]);
  cairo_fill_preserve(cr);
  if ( color >=0 ) 
    Xgc->graphic_engine->xset_color(Xgc,color);
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

static void drawpolymark(BCG *Xgc, double *vx, double *vy,int n)
{
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
      int i, cpat;
      cpat = Xgc->graphic_engine->xget_color(Xgc);
      Xgc->CurColor = -1; /* we want to force xset to change rgb color */
      Xgc->graphic_engine->xset_color(Xgc,cpat);
      for ( i=0; i< n ;i++) draw_mark(Xgc,vx+i,vy+i);
      Xgc->graphic_engine->xset_color(Xgc,cpat);
    }
}

/* pango layout and cairo
 *
 */

static void draw_mark(BCG *Xgc,double *x, double *y)
{
  double dx,dy;
  PangoRectangle ink_rect;
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  int code = symbols[Xgc->CurHardSymb];
  gchar symbol_code[4], *iter = symbol_code;

  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);
  pango_layout_set_text (Xgc->private->mark_layout,symbol_code, -1);
  pango_layout_get_extents(Xgc->private->mark_layout,&ink_rect,NULL);
  dx = PANGO_PIXELS(( ink_rect.x + ink_rect.width/2.0));
  dy = PANGO_PIXELS(( ink_rect.y + ink_rect.height/2.0));
  cairo_move_to (cr, *x-dx,*y-dy);
  pango_cairo_update_layout (cr,Xgc->private->mark_layout);
  pango_cairo_show_layout (cr,Xgc->private->mark_layout);
  if (0)
    {
      cairo_rectangle (cr,*x-dx/2.0,*y-dy/2.0,PANGO_PIXELS(ink_rect.width),PANGO_PIXELS(ink_rect.height));
      cairo_stroke (cr);
    }
}



static void displaystring(BCG *Xgc,const char *str, double x, double y, int flag,double angle,
			  gr_str_posx posx, gr_str_posy posy )
{
  PangoRectangle ink_rect,logical_rect;
  int  height,width;
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
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
      int xpos=0,ypos=0;
      double rad_angle = angle * M_PI/180.0;
      switch( posx )
	{
	case GR_STR_XLEFT: xpos = 0; break;
	case GR_STR_XCENTER: xpos = 0 - width/2; break;
	case GR_STR_XRIGHT: xpos = 0 - width; break;
	}
      switch( posy )
	{
	case GR_STR_YBOTTOM: ypos = 0 -height; break;
	case GR_STR_YCENTER:  ypos = 0 - height/2; break;
	case GR_STR_YBASELINE: ypos = 0 + logical_rect.y; break;
	case GR_STR_YUP:  ypos = 0 ; break;
	}
      /* we have to rotate the string at its center */
      cairo_save (cr);
      cairo_identity_matrix (cr);
      cairo_translate (cr, x,y);
      cairo_rotate (cr, rad_angle);
      /* position the layout according to (posx,posy) */
      cairo_move_to (cr, xpos, ypos);
      pango_layout_set_text (Xgc->private->layout,str, -1);
      pango_cairo_update_layout (cr,Xgc->private->layout);
      pango_cairo_show_layout (cr,Xgc->private->layout);
      if ( flag == 1)
	{
	  cairo_rectangle (cr,0,-height,width,height);
	  cairo_stroke (cr);
	}
      cairo_restore (cr);
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
      cairo_move_to (cr, xpos,ypos);
      pango_cairo_update_layout (cr,Xgc->private->layout);
      pango_cairo_show_layout (cr,Xgc->private->layout);
      /* horizontal string */
      if ( flag == TRUE ) /*  flag == 1)  */
	{
	  cairo_rectangle (cr,xpos,ypos,width,height);
	  cairo_stroke (cr);
	}
    }
}

static void boundingbox(BCG *Xgc,const char *string, int x, int y, double *rect)
{
  int width, height;
  pango_layout_set_text (Xgc->private->layout, string, -1);
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height);
  rect[0]=x;rect[1]=y+height;rect[2]=width;rect[3]=height;
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
 **/

static void draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  GdkPixbuf *pixbuf=pix;
  int w,h;
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  cairo_save (cr);
  w = gdk_pixbuf_get_width (pixbuf);
  h = gdk_pixbuf_get_height (pixbuf);
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
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;

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

static void xset_clip(BCG *Xgc,const GdkRectangle *r)
{
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  if ( Xgc->ClipRegionSet == 1 )
    {
      cairo_reset_clip(cr);
    }
  Xgc->ClipRegionSet = 1;
  Xgc->CurClipRegion = *r;
  cairo_new_path (cr);
  /* Sciprintf("clip in %d %d %d %d\n",r->x,r->y,r->width,r->height); */
  cairo_rectangle(cr,r->x,r->y,r->width,r->height);
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
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
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
      x[1] =Xgc->CurClipRegion.x;
      x[2] =Xgc->CurClipRegion.y;
      x[3] =Xgc->CurClipRegion.width;
      x[4] =Xgc->CurClipRegion.height;
    }
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
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  if ( value == 0)
    {
      /* FIXME: proper width in double ? */
      cairo_set_line_width(cr,(Xgc->CurLineWidth <= 1) ? 0.5 : Xgc->CurLineWidth*0.5);
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
      cairo_set_line_width(cr,(Xgc->CurLineWidth <= 1) ? 0.5 : Xgc->CurLineWidth*0.5);
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
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  cairo_set_source_rgb(cr,
		       Xgc->private->gcol_bg.red/65535.0,
		       Xgc->private->gcol_bg.green/65535.0,
		       Xgc->private->gcol_bg.blue/65535.0);
  cairo_rectangle (cr,0,0, w, h);
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
  if ( Xgc->CurPixmapStatus != 0)
    {
      int width, height;
      cairo_surface_t *temp;
      nsp_gtk_widget_get_size(Xgc->private->drawing,&width,&height);
      temp = gdk_window_create_similar_surface (gtk_widget_get_window(Xgc->private->drawing),
						CAIRO_CONTENT_COLOR,
						width,height);
      if ( temp  == NULL )
	{
	  xinfo(Xgc,"No more space to create cairo surface");
	  return;
	}
      cairo_surface_destroy (Xgc->private->extra_pixmap);
      Xgc->private->drawable = Xgc->private->extra_pixmap = temp;
      if ( Xgc->private->cairo_extra_pixmap_cr != NULL) cairo_destroy (Xgc->private->cairo_extra_pixmap_cr);
      Xgc->private->cairo_extra_pixmap_cr = cairo_create (Xgc->private->extra_pixmap);
      Xgc->private->cairo_drawable_cr = Xgc->private->cairo_extra_pixmap_cr;
      pixmap_clear_rect(Xgc,0,0,width,height);
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
      /* we switch to extra pixmap mode */
      int width, height;
      cairo_surface_t *temp;
      nsp_gtk_widget_get_size(Xgc->private->drawing,&width,&height);
      temp = gdk_window_create_similar_surface (gtk_widget_get_window(Xgc->private->drawing),
						CAIRO_CONTENT_COLOR,
						width,height);
      if ( temp  == NULL )
	{
	  xinfo(Xgc,"Not enough space to switch to Animation mode");
	  return;
	}
      if ( Xgc->private->extra_pixmap != NULL) cairo_surface_destroy (Xgc->private->extra_pixmap);
      xinfo(Xgc,"Animation mode is on,( xset('pixmap',0) to leave)");
      Xgc->private->drawable = Xgc->private->extra_pixmap = temp;
      if ( Xgc->private->cairo_extra_pixmap_cr != NULL) cairo_destroy (Xgc->private->cairo_extra_pixmap_cr);
      Xgc->private->cairo_extra_pixmap_cr = cairo_create (Xgc->private->extra_pixmap);
      Xgc->private->cairo_drawable_cr = Xgc->private->cairo_extra_pixmap_cr;
      pixmap_clear_rect(Xgc,0,0, width,height);
      Xgc->CurPixmapStatus = 1;
    }
  else if ( num1 == 0 )
    {
      /* deleting and removing the extra pixmap as the default drawable */
      xinfo(Xgc," ");
      if ( Xgc->private->extra_pixmap != NULL)  cairo_surface_destroy (Xgc->private->extra_pixmap);
      Xgc->private->extra_pixmap = NULL;
      Xgc->private->drawable = Xgc->private->pixmap;
      if ( Xgc->private->cairo_pixmap_cr != NULL) cairo_destroy (Xgc->private->cairo_pixmap_cr);
      Xgc->private->cairo_pixmap_cr = cairo_create (Xgc->private->pixmap);
      Xgc->private->cairo_drawable_cr = Xgc->private->cairo_pixmap_cr;
      Xgc->CurPixmapStatus = 0;
    }
  else
    {
      /* removing the extra pixmap as the default drawable
       * but extra_pixmap is not destroyed
       */
      Xgc->private->drawable = Xgc->private->pixmap;
      if ( Xgc->private->cairo_pixmap_cr != NULL) cairo_destroy (Xgc->private->cairo_pixmap_cr);
      Xgc->private->cairo_pixmap_cr = cairo_create (Xgc->private->pixmap);
      Xgc->private->cairo_drawable_cr = Xgc->private->cairo_pixmap_cr;
      Xgc->CurPixmapStatus = 0;
    }
}

/**
 * xset_color:
 * @Xgc:
 * @num:
 *
 *
 *
 * Returns:
 **/

static int  xset_color(BCG *Xgc,int color)
{
  cairo_t *cr =  Xgc->private->cairo_drawable_cr;
  int old = Xgc->CurColor;
  double rgb[3];
  color = Max(1,color);
  /* if ( Xgc->CurColor == color ) return old; */
  if ( Xgc->private->a_colors == NULL) return 1;
  Xgc->CurColor = color; 
  nsp_get_color_rgb(Xgc,color,rgb,Xgc->private->a_colors);
  cairo_set_source_rgb(cr, rgb[0], rgb[1], rgb[2]);
  return old;
}

/* export cairo graphics to files in various formats
 * This cairo facilities should be checked by configure
 */

#include <cairo-pdf.h>
#include <cairo-ps.h>
#include <cairo-svg.h>

int nsp_cairo_draw_to_file(const char *fname,const char *driver,BCG *Xgc,int colored,int figure_bg_draw,char option);
int nsp_cairo_draw_to_cr(cairo_t *cr, BCG *Xgc,int colored,char option, int figure_bg_draw, double width, double height);

int nsp_cairo_export(BCG *Xgc,int win_num,int colored, const char *fname,const char *driver,char option,
		     int figure_bg_draw)
{
  return nsp_cairo_draw_to_file(fname,driver,Xgc, colored,figure_bg_draw,option);
}

/* the nex function is used by the print menu
 */

int nsp_cairo_print_deprecated(int win_num,cairo_t *cr, int width,int height)
{
  NspGraphic *G ;
  NspFigure *F;
  BCG *Xgc = window_list_search_new(win_num);
  int v1=-1,cwin;
  BCG *Xgc1=Xgc; /* used for drawing */
  /* we must create a cairo Xgc with a file-surface */
  cwin =  Xgc->graphic_engine->xget_curwin();
  /* create a new graphic with cairo which becomes the current one */
  F = Cairo_gengine.initgraphic("void",&v1,NULL,NULL,NULL,NULL,'k',cr,NULL);
  /* we don't want the cairo graphic to become the current one */
  xset_curwin(cwin,FALSE);
  if ( F == NULL )
    {
      Sciprintf("cannot export a non cairo graphic\n");
      return FAIL;
    }
  Xgc1 = F->obj->Xgc;
  Xgc1->CWindowWidth= width;
  Xgc1->CWindowHeight=  height;

  G = (NspGraphic *) Xgc->figure ;
  G->type->draw(Xgc1,G,NULL,NULL);
  if ( Xgc1 != Xgc )
    {
      /* delete the localy created <<window>>
       * we need here to avoid the fact that during the
       * delete the window is erased
       */
      Xgc1->private->cairo_drawable_cr = NULL;
      /* nsp_gr_delete(win); */
      Xgc1->actions->destroy(Xgc1);
    }
  return OK;
}

/* draws the figure contained in Xgc->figure using a cairo surface
 *
 */

int nsp_cairo_draw_to_file(const char *fname,const char *driver,BCG *Xgc,int colored,int figure_bg_draw,char option)
{
  int width = Xgc->CWindowWidth, height= Xgc->CWindowHeight, rep;
  cairo_surface_t *surface;
  cairo_t *cr;
  if ( strcmp(driver,"cairo-pdf")==0 )
    {
      surface = cairo_pdf_surface_create (fname,width, height );
    }
  else if ( strcmp(driver,"cairo-svg")==0 )
    {
      surface = cairo_svg_surface_create (fname,width, height );
    }
  else if ( strcmp(driver,"cairo-ps")==0 )
    {
      surface = cairo_ps_surface_create (fname,width, height );
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
  rep = nsp_cairo_draw_to_cr(cr,Xgc,colored,option,figure_bg_draw,Xgc->CWindowWidth,Xgc->CWindowHeight);
  cairo_show_page (cr);
  if ( strcmp(driver,"cairo-png")==0 )
    {
      cairo_surface_write_to_png (surface,fname);
    }
  cairo_destroy (cr);
  cairo_surface_destroy (surface);
  return rep;
}

/* draws the figure contained in Xgc->figure using the cairo driver with cairo_t *cr
 */

int nsp_cairo_draw_to_cr(cairo_t *cr, BCG *Xgc,int colored,char option, int figure_bg_draw, double width, double height)
{
  NspFigure *F;
  NspGraphic *G;
  int v1=-1,cwin;
  BCG *Xgc1= NULL;
  if ( cr == NULL) return FAIL;
  /* we must create a cairo Xgc with a file-surface */
  cwin =  Xgc->graphic_engine->xget_curwin();
  /* create a new graphic with cairo not attached to a Widget */
  F = Cairo_gengine.initgraphic("void",&v1,NULL,NULL,NULL,NULL,option,cr,NULL);
  /* we don't want the cairo graphic to become the current one */
  xset_curwin(cwin,FALSE);
  if ( F == NULL )
    {
      Sciprintf("cannot export graphics\n");
      return FAIL;
    }
  Xgc1 = F->obj->Xgc;
  Xgc1->CWindowWidth=  width;
  Xgc1->CWindowHeight= height;
  Xgc1->graphic_engine->xset_usecolor(Xgc1,(colored ==1) ? 1:0);
  Xgc1->figure_bg_draw = figure_bg_draw;
  G = (NspGraphic *) Xgc->figure ;
  G->type->draw(Xgc1,G,NULL,NULL);
  Xgc1->figure_bg_draw = TRUE;
  if ( Xgc != Xgc1 )
    {
      /* Is it possible that Xgc == Xgc1 ? */
      Xgc1->actions->destroy(Xgc1);
    }
  return OK;
}

static void nsp_fonts_finalize(BCG *Xgc)
{
  if ( Xgc->private->layout != NULL)
    {
      Xgc->private->context=NULL; /* unused */
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
      /* cairo is not always used for a widget */
      Xgc->private->context= NULL;
      Xgc->private->layout = pango_cairo_create_layout(Xgc->private->cairo_drawable_cr);
      Xgc->private->mark_layout = pango_cairo_create_layout(Xgc->private->cairo_drawable_cr);
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

static void  xget_font(BCG *Xgc,int *font, int full)
{
  font[0] = Xgc->fontId ;
  if ( full == TRUE )
    {
      /* returns font size in pixel */
      font[1] = (Xgc->fontSize <= 0) ?
	pango_size[-Xgc->fontSize]: Xgc->fontSize;
    }
  else
    {
      /* returns font size as an array indice */
      font[1] = (Xgc->fontSize <= 0) ? -Xgc->fontSize : 0 ;
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

/* To get the current mark id */

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}


/* drawing marks with pango
 */


void xstring_pango(BCG *Xgc,char *str,int rect[],char *font,int size,int markup,int position)
{

}
