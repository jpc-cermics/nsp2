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
 *
 *--------------------------------------------------------------------------*/

/*
 * clear a rectangle zone 
 */

static void cleararea(BCG *Xgc, int x, int y, int w, int h)
{
  int clipflag = 0;
  /* switch to a clear gc */
  int cur_alu = Xgc->CurDrawFunction;
  int clear = 0 ; /* 0 is the Xclear alufunction */;
  DRAW_CHECK;

  if ( cur_alu != clear ) xset_alufunction1(Xgc,clear);
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
      gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect);
    }
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, TRUE,x,y,w,h);
  if ( cur_alu != clear )
    xset_alufunction1(Xgc,cur_alu);   /* back to current value */ 
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

/*
 * line segments arrows 
 */

static void drawline(BCG *Xgc,int x1, int yy1, int x2, int y2)
{
  DRAW_CHECK;
  gdk_draw_line(Xgc->private->drawable,Xgc->private->wgc, x1, yy1, x2, y2);
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
  DRAW_CHECK;
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, FALSE,
		     rect[0],rect[1],rect[2],rect[3]);
}

/* fill one rectangle, with current pattern */

static void fillrectangle(BCG *Xgc,const int rect[])
{ 
  DRAW_CHECK;
  gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, TRUE,rect[0],rect[1],rect[2],rect[3]);
}

/*
 * draw a set of rectangles, provided here to accelerate GraySquare for X11 device 
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : is the value of a function on the grid defined by x,y 
 *  on each rectangle the average value of z is computed 
 */

static  void fill_grid_rectangles(BCG *Xgc,const int x[],const int y[],const double z[], int nx, int ny,
				  int remap,const int *colminmax,const double *zminmax)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->fill_grid_rectangles(Xgc,x,y,z,nx,ny,remap,colminmax,zminmax);
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

static void fill_grid_rectangles1(BCG *Xgc,const int x[],const int y[],const double z[], int nr, int nc,
				  int remap,const int *colminmax,const double *zminmax)
{
  DRAW_CHECK;
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

/* Draw a single ellipsis or part of it */

static void drawarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK;
  gdk_draw_arc(Xgc->private->drawable, Xgc->private->wgc,FALSE,
	       arc[0],arc[1],arc[2],arc[3],arc[4],arc[5]);
}

/* Fill a single elipsis or part of it with current pattern */

static void fillarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK;
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
  DRAW_CHECK;
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
  int n1;
  DRAW_CHECK;
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
  DRAW_CHECK;
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
  DRAW_CHECK;
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

#ifndef WITH_PANGO

static void draw_mark(BCG *Xgc,int *x, int *y)
{ 
  char str[2]={0,0};
  DRAW_CHECK;
  str[0]=Marks[Xgc->CurHardSymb]; 
  gdk_draw_text(Xgc->private->drawable,Xgc->private->font,Xgc->private->wgc, 
		*x+CurSymbXOffset(Xgc), *y +CurSymbYOffset(Xgc),str,1);
}

#endif 

#ifdef WITH_PANGO

static void draw_mark(BCG *Xgc,int *x, int *y)
{
  double dx,dy;
  PangoRectangle ink_rect;
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

#endif 



#ifndef WITH_PANGO 

/*
 * display of a string at (x,y) position whith slope angle alpha 
 * (given in degree clockwise). If *flag ==1 and angle is zero a 
 * framed box is added around the string. 
 * (x,y) defines the lower left point of the bounding box 
 * of the string ( we do not separate asc and desc).
 */

static void displaystring(BCG *Xgc,char *string, int x, int y,  int flag, double angle) 
{ 
  gint lbearing, rbearing, iascent, idescent, iwidth;
  DRAW_CHECK;
  gdk_string_extents(Xgc->private->font,"X", &lbearing, &rbearing,
		     &iwidth, &iascent, &idescent);
  if ( Abs(angle) <= 0.1) 
    {
      gdk_draw_text(Xgc->private->drawable,Xgc->private->font,Xgc->private->wgc, 
		    x, y - idescent , string, strlen(string));
      if ( flag == 1) 
	{
	  int rect[] = { x , y- iascent - idescent, 
			 gdk_string_width(Xgc->private->font, string),
			 iascent+idescent};
	  drawrectangle(Xgc,rect);
	}
    }
  else 
    {
      gdk_draw_text_rot(Xgc->private->drawable,Xgc->private->font,Xgc->private->wgc, 
			x, y - idescent ,0,0, string, strlen(string),-angle * M_PI/180.0);
    }
}

#endif 

/* To get the bounding rectangle of a string 
 */

#ifndef WITH_PANGO 

static void boundingbox(BCG *Xgc,char *string, int x, int y, int *rect)
{ 
  gint lbearing, rbearing, iascent, idescent, iwidth;
  gdk_string_extents(Xgc->private->font,"X", &lbearing, &rbearing, &iwidth, &iascent, &idescent);
  rect[0]= x ;
  rect[1]= y - iascent - idescent;
  rect[2]= gdk_string_width(Xgc->private->font, string);
  rect[3]= iascent + idescent;
}
#endif 


#ifdef WITH_PANGO 

/* 
 * FIXME: flag is unused since the rectangle is drawn in Graphics-IN.c 
 *        to deal with the case when string can be on multiple lines 
 *        but pango could make this directly. 
 * maybe we should change the display an consider that (x,y) is the baseline 
 * of the string not its lower left boundary.
 * Note that if the string contains \n then the layout will have multiple lines 
 * 
 */

static void displaystring(BCG *Xgc,char *str, int x, int y, int flag,double angle)
{
  PangoRectangle ink_rect,logical_rect;
  int  height,width;
  DRAW_CHECK;
  pango_layout_set_text (Xgc->private->layout, str, -1);
  /*  PangoLayoutLine *line;
   *  nline = pango_layout_get_line_count(Xgc->private->layout); 
   *  if ( nline == 1 ) 
   *    {
   *   / * we want (x,y) to be at the baseline of the first string position * /
   *   line = pango_layout_get_line(Xgc->private->layout,0);
   *   pango_layout_line_get_extents(line, &ink_rect,&logical_rect);
   *   height = - logical_rect.y/PANGO_SCALE;
   *   width = logical_rect.width/PANGO_SCALE;
   */
  /* used to position the descent of the last line of layout at y */
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
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
      get_rotated_layout_bounds (Xgc->private->layout,Xgc->private->context, 
				 &matrix,&rect);
      gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,
		       x+rect.x+xt,y+rect.y+yt,Xgc->private->layout);
      if (0) 
	{
	  /* just to test : also draw the enclosing rectangle */
	  int myrect[]={ x,y ,rect.width,rect.height};
	  drawrectangle(Xgc,myrect);
	  fprintf(stderr,"rect = %d %d %d %d\n",rect.x,rect.y,rect.width,rect.height);
	  gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,
			   x,y,Xgc->private->layout);

	}
      pango_context_set_matrix (Xgc->private->context,NULL);
      pango_layout_context_changed (Xgc->private->layout);
      if (0) 
	{
	  /* draw the bounding box */
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
      /* horizontal string */
      if (0)
	gdk_draw_rectangle(Xgc->private->drawable, Xgc->private->wgc, FALSE,x,y - height,width,height);
      gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,x,y - height,Xgc->private->layout);
    }
}
#endif 

#ifdef WITH_PANGO 

static void boundingbox(BCG *Xgc,char *string, int x, int y, int *rect)
{
  int width, height;
  pango_layout_set_text (Xgc->private->layout, string, -1);
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
  rect[0]=x;rect[1]=y+height;rect[2]=width;rect[3]=height;
}

#endif 

