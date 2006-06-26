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
 *--------------------------------------------------------------------------*/

/*
 * cleararea: clear a rectangle zone 
 */

typedef void (*r_c) (BCG *Xgc,int x,int y,int w,int h);
static void RectangleClear   (BCG *Xgc,int x,int y,int w,int h,int clipflag,r_c f );
static void R_clear  (BCG *Xgc,int x,int y,int w,int h);

static void R_clear(BCG *Xgc,int x, int y, int w, int h)
{
  int tab[]={ x,y,w, h};
  fillrectangle(Xgc, tab);
}

static void RectangleClear(BCG *Xgc,int x, int y, int w, int h, int clipflag, r_c F)
{
  /* switch to a clear gc */
  int cur_alu = Xgc->CurDrawFunction;
  int clear = 0 ; /* 0 is the Xclear alufunction */;
  if ( cur_alu != clear ) xset_alufunction1(Xgc,clear);
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      static GdkRectangle clip_rect = { 0,0,int16max,  int16max};
      /* gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect); */
      clip_rectangle(Xgc, clip_rect);
    }
  (*F)(Xgc,x,y,w,h);
  if ( cur_alu != clear )
    xset_alufunction1(Xgc,cur_alu);   /* back to current value */ 
  if ( clipflag == 1 && Xgc->ClipRegionSet == 1) 
    {
      /* restore clip */
      GdkRectangle clip_rect = { Xgc->CurClipRegion[0],
				 Xgc->CurClipRegion[1],
				 Xgc->CurClipRegion[2],
				 Xgc->CurClipRegion[3]};
      /* gdk_gc_set_clip_rectangle(Xgc->private->wgc, &clip_rect); */
      clip_rectangle(Xgc, clip_rect);
    }
}

static void cleararea(BCG *Xgc, int x, int y, int w, int h)
{
  DRAW_CHECK;
  RectangleClear(Xgc,x,y,w,h,0,R_clear);
}


/*
 * line 
 */

static void drawline(BCG *Xgc,int x1, int y1, int x2, int y2)
{
  DRAW_CHECK;
  glBegin(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glEnd();
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
  DRAW_CHECK
  glBegin(GL_LINE_LOOP);
  glVertex2i(rect[0]        ,rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]+rect[3]);
  glVertex2i(rect[0]        ,rect[1]+rect[3]);
  glEnd();
}

/* fill one rectangle, with current pattern */

static void fillrectangle(BCG *Xgc,const int rect[])
{ 
  DRAW_CHECK;
  glBegin(GL_QUADS);
  glVertex2i(rect[0]        ,rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]);
  glVertex2i(rect[0]+rect[2],rect[1]+rect[3]);
  glVertex2i(rect[0]        ,rect[1]+rect[3]);
  glEnd();
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

/* Draw a single ellipsis or part of it 
 */

static void drawarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK; /* FIXME */
  Xgc->graphic_engine->generic->drawarc(Xgc,arc);
}

/* Fill a single elipsis or part of it with current pattern  */

static void fillarc(BCG *Xgc,int arc[])
{ 
  DRAW_CHECK; /* FIXME */
  Xgc->graphic_engine->generic->fillarc(Xgc,arc);
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

/* specific version for OpenGl to add a glEnable(GL_POLYGON_OFFSET_FILL);
 */

static void fillpolylines(BCG *Xgc,int *vectsx, int *vectsy, int *fillvect,int n, int p)
{
  int dash,color,i;
  DRAW_CHECK;
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{ 
	  /* fill + boundaries **/
	  xset_pattern(Xgc,fillvect[i]);
	  glEnable(GL_POLYGON_OFFSET_FILL);
	  glPolygonOffset(1.0,1.0);
	  fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  glEnable(GL_POLYGON_OFFSET_FILL);
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else 
	{
	  xset_pattern(Xgc,-fillvect[i]);
	  fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  xset_pattern(Xgc,color);
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
  gint i;
  if ( n <= 1) return;
  DRAW_CHECK;
  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
  if ( closeflag == 1 ) 
    glBegin(GL_LINE_LOOP);
  else
    glBegin(GL_LINE_STRIP);
  for (i=0; i < n ; i++) glVertex2i(vx[i], vy[i]);
  glEnd();
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
}

/* 
 * Fill the polygon or polyline 
 * according to *closeflag : the given vector is a polyline or a polygon 
 */

/* FIXME: Attention ça ne marche que pour un polygone convexe !!!!!!
 * sinon il faut le trianguler 
 */

static void fillpolyline(BCG *Xgc, int *vx, int *vy, int n,int closeflag) 
{
  gint i;
  if ( n <= 1) return;
  DRAW_CHECK;
  glBegin(GL_POLYGON);
  for ( i=0 ;  i< n ; i++) glVertex2i( vx[i], vy[i]);
  glEnd();
  
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
      gint i;
      glBegin(GL_POINTS);
      for (i=0; i< n ; i++) glVertex2i( vx[i], vy[i]);
      glEnd();
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
 *   \begin{itemize} 
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
 *   \end{itemize} 
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

static void draw_mark(BCG *Xgc,int *x, int *y)
{
  double dx,dy;
  PangoRectangle ink_rect,logical_rect;
  int code = symbols[Xgc->CurHardSymb]; 
  gchar symbol_code[4], *iter = symbol_code;
  DRAW_CHECK;
  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);
  pango_layout_set_text (Xgc->private->mark_layout,symbol_code, -1);
  pango_layout_get_extents(Xgc->private->mark_layout,&ink_rect,&logical_rect);
  dx = ink_rect.x + ink_rect.width/2.0;
  dy = ink_rect.y -logical_rect.height + ink_rect.height/2.0;
  /* gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,*x-dx,*y-dy,Xgc->private->mark_layout); */
  nsp_gtk_set_color(Xgc,1);
  glRasterPos2i(*x-PANGO_PIXELS(dx),*y + PANGO_PIXELS(-dy));
  gl_pango_ft2_render_layout (Xgc->private->mark_layout,NULL);
  if (0) 
    {
      /* draw the ink_rectangle around the mark */
      int i;
      double rect[]={ink_rect.x -dx ,ink_rect.y -logical_rect.height -dy,ink_rect.width,ink_rect.height};
      int myrect[]={*x,*y,0,0};
      for ( i=0; i < 4 ; i++) myrect[i] += PANGO_PIXELS(rect[i]);
      drawrectangle(Xgc,myrect);
    }
}


/* text and rotated text with pango 
 */

static void get_rotated_layout_bounds (PangoLayout  *layout,PangoContext *context,
				       const PangoMatrix *matrix, GdkRectangle *rect);

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
      pango_context_set_matrix (Xgc->private->ft2_context, &matrix);
      pango_layout_context_changed (Xgc->private->layout);
      pango_layout_get_extents(Xgc->private->layout,&ink_rect,&logical_rect);
      get_rotated_layout_bounds (Xgc->private->layout,Xgc->private->ft2_context, 
				 &matrix,&rect);
      /* we want to change the transformation in order to have the upper left 
       * point of the enclosing box after rotation at (0,0) since 
       * gl_pango_ft2_render_layout seams not to work fine if it is not the case 
       */
      matrix.x0= -rect.x; matrix.y0=-rect.y;
      pango_context_set_matrix (Xgc->private->ft2_context, &matrix);
      pango_layout_context_changed (Xgc->private->layout);
      get_rotated_layout_bounds (Xgc->private->layout,Xgc->private->ft2_context, 
				 &matrix,&rect);
      /* the down, left point of the enclosing rectangle is at position (x,y) 
       * we need to translate since we want the down left corner of the rotated rectangle 
       * to be at position (x,y)
       */
      xt = 0 * matrix.xx + height * matrix.xy + matrix.x0;
      yt = 0 * matrix.yx + height * matrix.yy + matrix.y0;
      glRasterPos2i(x-xt,y+rect.height  - yt);
      gl_pango_ft2_render_layout (Xgc->private->layout,&rect);
      if (0) 
	{
	  /* draw the enclosing rectangle */
	  int myrect[]={ x-xt,y-yt ,rect.width,rect.height};
	  drawrectangle(Xgc,myrect);
	  fprintf(stderr,"rect = %d %d %d %d\n",rect.x,rect.y,rect.width,rect.height);
	}
      pango_context_set_matrix (Xgc->private->ft2_context,NULL);
      pango_layout_context_changed (Xgc->private->layout);
      if (0) 
	{
	  /* draw the rotated enclosing rectangle */
	  double xx[]={0,width},yy[]={0,height};
	  int vx[4],vy[4],ik[]={0,1,3,2},i,j,k;
	  double dx,dy;
	  for ( i = 0 ; i < 2 ; i++) 
	    {
	      for ( j=0; j < 2 ; j++)
		{
		  dx =  xx[i]* matrix.xx + yy[j] * matrix.xy + matrix.x0;
		  dy =  xx[i]* matrix.yx + yy[j] * matrix.yy + matrix.y0;
		  k = ik[i+2*j];
		  vx[k]= x-(xt-dx), vy[k]= y-(yt-dy);
		}
	    }
	  drawpolyline(Xgc,vx, vy,4,1);
	}
    }
  else
    {
      /* horizontal string */
      if (0)
	{
	  /* draw enclosing rectangle */
	  int rect[]={ x,y - height,width,height};
	  drawrectangle(Xgc,rect);
	}
      /* gdk_draw_layout (Xgc->private->drawable,Xgc->private->wgc,x,y - height,Xgc->private->layout); */
      glRasterPos2i(x,y);
      gl_pango_ft2_render_layout (Xgc->private->layout,NULL);
    }
}


/* returns the enclosing rectangle of the pango layout transformed 
 * by matrix transformation: 
 */

static void get_rotated_layout_bounds (PangoLayout  *layout,PangoContext *context,
				       const PangoMatrix *matrix, GdkRectangle *rect)
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

/* returns the bounding box for a non rotated string 
 */

static void boundingbox(BCG *Xgc,char *string, int x, int y, int *rect)
{
  int width, height;
  pango_layout_set_text (Xgc->private->layout, string, -1);
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height); 
  rect[0]=x;rect[1]=y+height;rect[2]=width;rect[3]=height;
}



/* pixbuf 
 *
 */

static void draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  DRAW_CHECK;
  Xgc->graphic_engine->generic->draw_pixbuf(Xgc,pix,src_x,src_y,dest_x,dest_y,width,height);
}
