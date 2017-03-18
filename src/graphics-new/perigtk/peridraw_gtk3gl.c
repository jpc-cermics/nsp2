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
 *--------------------------------------------------------------------------*/

#define R_RGB(r,g,b)	((r)|((g)<<8)|((b)<<16))


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
  glClearColor(Xgc->private->gcol_bg.red /255.0,
	       Xgc->private->gcol_bg.green /255.0,
	       Xgc->private->gcol_bg.blue /255.0,0.0);
  glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
}

/*
 * cleararea: clear a rectangle zone
 */

static void cleararea(BCG *Xgc,const GdkRectangle *r)
{
  nsp_ogl_set_2dview(Xgc);
  if ( r == NULL )
    {
      clearwindow(Xgc);
      return ;
    }
  xset_color(Xgc,Xgc->NumBackground);
  glScissor(r->x, r->y,r->x+r->width,r->y+r->height);
  glEnable(GL_SCISSOR_TEST);
  glClear(GL_COLOR_BUFFER_BIT);
  glDisable(GL_SCISSOR_TEST);
}

/*
 * line
 */

static void drawline(BCG *Xgc, double x1, double y1, double x2, double y2)
{
  double rgb[3];
  int j,k;
  GLfloat vertex_colors[2*4];
  GLfloat vertex_data[]={x1,y1,0.f,1.0f,
			 x2,y2,0.f,1.0f};
  xget_color_rgb(Xgc, rgb);
  for ( j = 0 ; j < 2 ; j++)
    {
      for (k=0 ; k < 3; k++)
	vertex_colors[4*j + k ]=rgb[k];
      vertex_colors[4*j +3] = 1.0f;
    }
  shader_draw_line(vertex_data,vertex_colors,2,FALSE);
}

/* Draw a set of segments
 * segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1])
 * for i=0 step 2
 * n is the size of vx and vy
 */

static void drawsegments(BCG *Xgc, double *vx, double *vy, int n, int *style, int *width)
{
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
  double rgb[3];
  int j,k;
  GLfloat vertex_colors[4*4];
  GLfloat vertex_data[4*4];
  j=0;
  vertex_data[j]= rect[0];
  vertex_data[j+1]= rect[1];
  vertex_data[j+2]= 0.f;
  vertex_data[j+3]= 1.0f;
  j=4;
  vertex_data[j]= rect[0]+rect[2];
  vertex_data[j+1]= rect[1];
  vertex_data[j+2]= 0.f;
  vertex_data[j+3]= 1.0f;
  j=8;
  vertex_data[j]= rect[0]+rect[2];
  vertex_data[j+1]= rect[1]+rect[3];
  vertex_data[j+2]= 0.f;
  vertex_data[j+3]= 1.0f;
  j=12;
  vertex_data[j]= rect[0];
  vertex_data[j+1]= rect[1]+rect[3];
  vertex_data[j+2]= 0.f;
  vertex_data[j+3]= 1.0f;
  
  xget_color_rgb(Xgc, rgb);
  for ( j=0 ;  j < 4 ; j++)
    {
      for (k=0 ; k < 3; k++)
	vertex_colors[4*j + k ]=rgb[k];
      vertex_colors[4*j +3]= 1.0f;
    }
  shader_draw_line(vertex_data,vertex_colors,4,TRUE);
}

/* fill one rectangle, with current color */

static void fillrectangle(BCG *Xgc,const double rect[])
{
  double rgb[3];
  int j,k;
  GLfloat vertex_colors[4*4];
  GLfloat vertex_data[4*4];
  j=0;
  vertex_data[j]= rect[0];
  vertex_data[j+1]= rect[1];
  vertex_data[j+2]= 0.f;
  vertex_data[j+3]= 1.0f;
  j=4;
  vertex_data[j]= rect[0]+rect[2];
  vertex_data[j+1]= rect[1];
  vertex_data[j+2]= 0.f;
  vertex_data[j+3]= 1.0f;
  j=8;
  vertex_data[j]= rect[0]+rect[2];
  vertex_data[j+1]= rect[1]+rect[3];
  vertex_data[j+2]= 0.f;
  vertex_data[j+3]= 1.0f;
  j=12;
  vertex_data[j]= rect[0];
  vertex_data[j+1]= rect[1]+rect[3];
  vertex_data[j+2]= 0.f;
  vertex_data[j+3]= 1.0f;

  xget_color_rgb(Xgc, rgb);
  for ( j=0 ;  j < 4 ; j++)
    {
      for (k=0 ; k < 3; k++)
	vertex_colors[4*j + k ]=rgb[k];
      vertex_colors[4*j +3]= 1.0f;
    }
  shader_fill_quad(vertex_data,vertex_colors);
}

/*
 * draw a set of rectangles, provided here to accelerate GraySquare for X11 device
 *  x : of size n1 gives the x-values of the grid
 *  y : of size n2 gives the y-values of the grid
 *  z : is the value of a function on the grid defined by x,y
 *  on each rectangle the average value of z is computed
 */

#if 0
static  void fill_grid_rectangles(BCG *Xgc,const int x[],const int y[],const double z[], int nx, int ny,
				  int remap,const int *colminmax,const double *zminmax,const int *colout)
{

  Xgc->graphic_engine->generic->fill_grid_rectangles(Xgc,x,y,z,nx,ny,remap,colminmax,zminmax,colout);
}
#endif 
/*
 * draw a set of rectangles, provided here to accelerate GraySquare1 for X11 device
 *  x : of size n1 gives the x-values of the grid
 *  y : of size n2 gives the y-values of the grid
 *  z : of size (n1-1)*(n2-1)  gives the f-values on the middle
 *  of each rectangle.
 *  z[i,j] is the value on the middle of rectangle
 *        P1= x[i],y[j] x[i+1],y[j+1]
 */
#if 0
static void fill_grid_rectangles1(BCG *Xgc,const int x[],const int y[],const double z[], int nr, int nc,
				  int remap,const int *colminmax,const double *zminmax)
{

  Xgc->graphic_engine->generic->fill_grid_rectangles1(Xgc,x,y,z,nr,nc,remap,colminmax,zminmax);
}
#endif 

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
#if 0
static void fillarcs(BCG *Xgc, double *vects, int *fillvect, int n)
{
  Xgc->graphic_engine->generic->fillarcs(Xgc,vects,fillvect,n);
}
#endif 
/*
 * Draw a set of ellipsis or part of ellipsis
 * Each is defined by 6-parameters,
 * ellipsis i is specified by $vect[6*i+k]_{k=0,5}= x,y,width,height,angle1,angle2$
 * <x,y,width,height> is the bounding box
 * angle1,angle2 specifies the portion of the ellipsis
 * caution : angle=degreangle*64
 */
#if 0
static void drawarcs(BCG *Xgc, double *vects, int *style, int n)
{
  Xgc->graphic_engine->generic->drawarcs(Xgc,vects,style,n);
}
#endif 
/* Draw a single ellipsis or part of it
 */

static void drawarc(BCG *Xgc, double arc[])
{
  /* FIXME */
  Xgc->graphic_engine->generic->drawarc(Xgc,arc);
}

/* Fill a single elipsis or part of it with current pattern  */

static void fillarc(BCG *Xgc, double arc[])
{
  /* FIXME */
  Xgc->graphic_engine->generic->fillarc(Xgc,arc);
}

/*
 * Draw a set of (*n) polylines (each of which have (*p) points)
 * with lines or marks
 * drawvect[i] <= 0 use a mark for polyline i
 * drawvect[i] >  0 use a line style for polyline i
 */
#if 0
static void drawpolylines(BCG *Xgc, double *vectsx, double *vectsy, int *drawvect,int n, int p)
{
  Xgc->graphic_engine->generic->drawpolylines(Xgc,vectsx,vectsy,drawvect,n,p);
}
#endif

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

static void fillpolylines(BCG *Xgc, const double *vectsx, const double *vectsy, int *fillvect,int n, int p)
{
  int i;
  /* int dash = Xgc->graphic_engine->xget_dash(Xgc); */
  int color = Xgc->graphic_engine->xget_color(Xgc);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{
	  /* fill + boundaries */
	  xset_color(Xgc,fillvect[i]);
	  glPolygonOffset(1.0,1.0);
	  fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1,color);
	  /* drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1,color); */
	}
      else  if (fillvect[i] == 0 )
	{
	  /* Xgc->graphic_engine->xset_dash(Xgc,dash); */
	  Xgc->graphic_engine->xset_color(Xgc,color);
	  drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else
	{
	  xset_color(Xgc,-fillvect[i]);
	  fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1,-1);
	  xset_color(Xgc,color);
	}
    }
  /* Xgc->graphic_engine->xset_dash(Xgc,dash); */
  Xgc->graphic_engine->xset_color(Xgc,color);
}

/*
 * Only draw one polygon  with current line style
 * according to *closeflag : it's a polyline or a polygon
 * n is the number of points of the polyline
 */

static void drawpolyline(BCG *Xgc, const double *vx, const double *vy, int n,int closeflag)
{
  double rgb[3];
  int j,k;
  /* XXXX need an alloc */
  GLfloat vertex_colors[1000*4];
  GLfloat vertex_data[1000*4];
  if ( n <= 1) return;
  xget_color_rgb(Xgc, rgb);
  for ( j = 0 ; j < Min(1000,n) ; j++)
    {
      vertex_data[4*j]= vx[j];
      vertex_data[4*j+1]= vy[j];
      vertex_data[4*j+2]= 0.f;
      vertex_data[4*j+3]= 1.0f;
      for (k=0 ; k < 3; k++)
	vertex_colors[4*j + k ]=rgb[k];
      vertex_colors[4*j +3]= 1.0f;
    }
  shader_draw_line(vertex_data,vertex_colors,Min(n,1000),closeflag);
}

/*
 * Fill the polygon or polyline
 * according to *closeflag : the given vector is a polyline or a polygon
 * Note that the function also draws the polyline 
 * FIXME: only works for convex polyline in opengl
 */

static void fillpolyline(BCG *Xgc, const double *vx, const double *vy, int n,int closeflag, int color)
{
  double rgb[3];
  int j,k,n1 = Min(n,1000);
  /* XXXX need an alloc */
  GLfloat vertex_colors[1000*4];
  GLfloat vertex_data[1000*4];
  if ( n <= 1) return;
  xget_color_rgb(Xgc, rgb);
  for ( j = 0 ; j < n1 ; j++)
    {
      vertex_data[4*j]= vx[j];
      vertex_data[4*j+1]= vy[j];
      vertex_data[4*j+2]= 0.f;
      vertex_data[4*j+3]= 1.0f;
      for (k=0 ; k < 3; k++)
	vertex_colors[4*j + k ]=rgb[k];
      vertex_colors[4*j +3]= 1.0f;
    }
  glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_coords);
  glBufferData (GL_ARRAY_BUFFER, 4*n1*sizeof(GLfloat), vertex_data, GL_STATIC_DRAW);
  glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_colors);
  glBufferData (GL_ARRAY_BUFFER, 4*n1*sizeof(GLfloat), vertex_colors, GL_STATIC_DRAW);
  glBindVertexArray (vao_triangles);
  glDrawArrays (GL_TRIANGLE_FAN, 0, n1);
  glBindVertexArray(0);
  
  if ( color >=0 ) Xgc->graphic_engine->xset_color(Xgc,color);
  xget_color_rgb(Xgc, rgb);
  for ( j = 0 ; j < n1 ; j++)
    {
      for (k=0 ; k < 3; k++)
	vertex_colors[4*j + k ]=rgb[k];
      vertex_colors[4*j +3]= 1.0f;
    }
  glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_colors);
  glBufferData (GL_ARRAY_BUFFER, 4*n1*sizeof(GLfloat), vertex_colors, GL_STATIC_DRAW);
  glBindVertexArray (vao_triangles);
  glDrawArrays ((closeflag) ? GL_LINE_LOOP: GL_LINE_STRIP, 0, n1);
  glBindVertexArray(0);
}

/*
 * Draw the current mark centred at points defined
 * by vx and vy (vx[i],vy[i])
 */

static void drawpolymark(BCG *Xgc, double *vx, double *vy,int n)
{
  if ( Xgc->CurHardSymb == 0 )
    {
      gint i;
      glBegin(GL_POINTS);
      for (i=0; i< n ; i++) glVertex2d( vx[i], vy[i]);
      glEnd();
    }
  else
    {
      int i,keepid,keepsize,hds;
      i=1;
      keepid =  Xgc->fontId;
      keepsize= Xgc->fontSize;
      hds= Xgc->CurHardSymbSize;
      xset_font(Xgc,i,hds, FALSE);
      for ( i=0; i< n ;i++) draw_mark(Xgc,vx+i,vy+i);
      xset_font(Xgc,keepid,keepsize, FALSE);
    }
}

static void draw_mark3D(BCG *Xgc,double x, double y, double z);

void drawpolymark3D(BCG *Xgc,double *vx,double *vy,double *vz,int n)
{
  if ( Xgc->CurHardSymb == 0 )
    {
      gint i;
      glBegin(GL_POINTS);
      for (i=0; i< n ; i++) glVertex3f( vx[i], vy[i],vz[i]);
      glEnd();
    }
  else
    {
      int i,keepid,keepsize,hds;
      i=1;
      keepid =  Xgc->fontId;
      keepsize= Xgc->fontSize;
      hds= Xgc->CurHardSymbSize;
      xset_font(Xgc,i,hds, FALSE);
      for ( i=0; i< n ;i++) draw_mark3D(Xgc,vx[i],vy[i],vz[i]);
      xset_font(Xgc,keepid,keepsize, FALSE);
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
#if 0
static void drawaxis(BCG *Xgc, int alpha, int *nsteps, int *initpoint,double *size)
{
  Xgc->graphic_engine->generic->drawaxis(Xgc,alpha,nsteps,initpoint,size);
}
#endif 
/*
 * Display numbers z[i] at location (x[i],y[i])
 *   with a slope alpha[i] (see displaystring), if flag==1
 *   add a box around the string, only if slope =0}
 */
#if 0
static void displaynumbers(BCG *Xgc, double *x, double *y, int n, int flag, double *z, double *alpha)
{
  Xgc->graphic_engine->generic->displaynumbers(Xgc,x,y,n,flag,z,alpha);
}
#endif 

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
    0x00B0, /* degree sign */
    0x25A0, /* black square */
    0x25A1  /* white square */
  };

static void draw_mark(BCG *Xgc, double *x, double *y)
{
  double dx,dy;
  PangoRectangle ink_rect,logical_rect;
  int code = symbols[Xgc->CurHardSymb];
  gchar symbol_code[4], *iter = symbol_code;

  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);
  pango_layout_set_text (Xgc->private->mark_layout,symbol_code, -1);
  pango_layout_get_extents(Xgc->private->mark_layout,&ink_rect,&logical_rect);
  dx = ink_rect.x + ink_rect.width/2.0;
  dy = ink_rect.y -logical_rect.height + ink_rect.height/2.0;
  Xgc->CurColor=1;
  xset_color(Xgc,Xgc->CurColor);
  glRasterPos2i(*x-PANGO_PIXELS(dx),*y + PANGO_PIXELS(-dy));
  gl_pango_ft2_render_layout (Xgc->private->mark_layout,NULL);
  if (0)
    {
      /* draw the ink_rectangle around the mark */
      int i;
      double rect[]={ink_rect.x -dx ,ink_rect.y -logical_rect.height -dy,ink_rect.width,ink_rect.height};
      double myrect[]={*x,*y,0,0};
      for ( i=0; i < 4 ; i++) myrect[i] += PANGO_PIXELS(rect[i]);
      drawrectangle(Xgc,myrect);
    }
}

static void draw_mark3D(BCG *Xgc,double x, double y, double z)
{
  PangoRectangle ink_rect,logical_rect;
  int code = symbols[Xgc->CurHardSymb];
  gchar symbol_code[4], *iter = symbol_code;
  g_unichar_to_utf8(code, iter);
  iter = g_utf8_next_char(iter);
  g_unichar_to_utf8(0x0, iter);
  pango_layout_set_text (Xgc->private->mark_layout,symbol_code, -1);
  pango_layout_get_extents(Xgc->private->mark_layout,&ink_rect,&logical_rect);
  Xgc->CurColor=1;
  xset_color(Xgc,Xgc->CurColor);
  /* XXX  we need here to move x,y,z to center the mark at (x,y,z)  */
  /* double dx,dy;
  dx = ink_rect.x + ink_rect.width/2.0;
  dy = ink_rect.y -logical_rect.height + ink_rect.height/2.0;
  */
  /* glRasterPos3f(x- PANGO_PIXELS(dx),y + PANGO_PIXELS(-dy),z); */
  glRasterPos3f(x,y,z);
  gl_pango_ft2_render_layout (Xgc->private->mark_layout,NULL);
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

static void displaystring(BCG *Xgc,const char *str, double x, double y, int flag,double angle,
			  gr_str_posx posx, gr_str_posy posy)
{
  PangoRectangle ink_rect,logical_rect;
  int  height,width;

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
	  double myrect[]={ x-xt,y-yt ,rect.width,rect.height};
	  drawrectangle(Xgc,myrect);
	  fprintf(stderr,"rect = %d %d %d %d\n",rect.x,rect.y,rect.width,rect.height);
	}
      pango_context_set_matrix (Xgc->private->ft2_context,NULL);
      pango_layout_context_changed (Xgc->private->layout);
      if (0)
	{
	  /* draw the rotated enclosing rectangle */
	  double xx[]={0,width},yy[]={0,height};
	  double vx[4],vy[4];
	  int ik[]={0,1,3,2},i,j,k;
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
	  double rect[]={ x,y - height,width,height};
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

static void boundingbox(BCG *Xgc,const char *string, int x, int y, double *rect)
{
  int width, height;
  pango_layout_set_text (Xgc->private->layout, string, -1);
  pango_layout_get_pixel_size (Xgc->private->layout, &width, &height);
  rect[0]=x;rect[1]=y+height;rect[2]=width;rect[3]=height;
}

/* pixbuf
 *
 */

static void draw_pixbuf(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,
			int dest_y,int width,int height)
{
  guchar *texpix=NULL;
  GdkPixbuf *pixbuf = pix;
  gint w = gdk_pixbuf_get_width (pixbuf);
  gint h = gdk_pixbuf_get_height (pixbuf);
  int nChannels = gdk_pixbuf_get_n_channels(pixbuf);
  /* int rowstride = gdk_pixbuf_get_rowstride (pixbuf); */
  texpix = gdk_pixbuf_get_pixels (pixbuf);
  glShadeModel(GL_FLAT);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glPixelZoom ( ((double) width)/w,- ((double) height)/h);
  glRasterPos2i(dest_x, dest_y );
  if ( nChannels == 4 )
    glDrawPixels( w,h,GL_RGBA,GL_UNSIGNED_BYTE, texpix);
  else
    glDrawPixels( w,h,GL_RGB,GL_UNSIGNED_BYTE, texpix);
  glPixelZoom (1.0,1.0);
}

static void draw_pixbuf_from_file(BCG *Xgc,const char *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  Xgc->graphic_engine->generic->draw_pixbuf_from_file(Xgc,pix,src_x,src_y,dest_x,dest_y,width,height);
}

/**
 * xset_clip:
 * @Xgc: a graphic context
 * @x: rectangle to be used for clipping
 *
 * Set a clip zone (rectangle)
 *
 **/

static void xset_clip(BCG *Xgc,const  GdkRectangle *r)
{
  Xgc->ClipRegionSet = 1;
  Xgc->CurClipRegion = *r;
  glEnable(GL_SCISSOR_TEST);
  glScissor(r->x, r->y, r->width, r->height);
#if 0
  {
    const double rect[] ={ r->x, r->y, r->width, r->height};
    Sciprintf("[%f,%f,%f,%f]\n", rect[0], rect[1], rect[2], rect[3]);
    xset_color(Xgc,4);
    fillrectangle(Xgc, rect);
  }
#endif
}

/**
 * xset_unclip:
 * @Xgc: a #BCG
 *
 * unset clip zone
 **/

static void xset_unclip(BCG *Xgc)
{
  if ( Xgc->ClipRegionSet == 0 ) return;
  Xgc->ClipRegionSet = 0;
  glDisable(GL_SCISSOR_TEST);
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

static void xset_dashstyle(BCG *Xgc,int value, int *xx, int *n)
{
  if ( value == 0)
    {
      /*
	 gdk_gc_set_line_attributes(Xgc->private->wgc,
	 (Xgc->CurLineWidth <= 1) ? 0 : Xgc->CurLineWidth,
	 GDK_LINE_SOLID,GDK_CAP_BUTT, GDK_JOIN_ROUND);
      */
      /* FIXME : */
      glLineWidth( ((Xgc->CurLineWidth <= 1) ? 2.0 : Xgc->CurLineWidth)*0.5);
    }
  else
    {
#if 0
      gint8 buffdash[18];
      int i;
      for ( i =0 ; i < *n ; i++) buffdash[i]=xx[i];
      gdk_gc_set_dashes(Xgc->private->wgc, 0, buffdash, *n);
      gdk_gc_set_line_attributes(Xgc->private->wgc,
				 (Xgc->CurLineWidth == 0 ) ? 1 : Xgc->CurLineWidth,
				 GDK_LINE_ON_OFF_DASH, GDK_CAP_BUTT, GDK_JOIN_ROUND);
#endif
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
  int status = Xgc->CurPixmapStatus;
  if ( status == 1 )
    {
      glClearColor(Xgc->private->gcol_bg.red /255.0,
		   Xgc->private->gcol_bg.green /255.0,
		   Xgc->private->gcol_bg.blue /255.0,0.0);
      glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
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
#if 0
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
#if 1
      cairo_surface_destroy(Xgc->private->extra_pixmap);
#else
      gdk_pixmap_unset_gl_capability (Xgc->private->extra_pixmap);
      cairo_surface_destroy(Xgc->private->extra_pixmap);
#endif
      Xgc->private->drawable = Xgc->private->extra_pixmap = temp;
      nsp_set_gldrawable(Xgc, Xgc->private->extra_pixmap);
      pixmap_clear_rect(Xgc,0,0,x,y);
    }
#endif
}

/* Change the private->pixmap status of a Graphic Window.
 * adding or removing a Background Pixmap to it
 */

static void xset_pixmapOn(BCG *Xgc,int num)
{
#if 0
  int num1= Min(Max(num,0),1);
  /* Sciprintf("pixmap on is not implemented in OpenGL driver\n"); */
  if ( Xgc->CurPixmapStatus == num1 ) return;
  if ( num1 == 1 )
    {
      GdkDrawable *temp ;
      /* create a new pixmap **/
      temp = (GdkDrawable *) gdk_pixmap_new(Xgc->private->drawing->window,
					    Xgc->CWindowWidth, Xgc->CWindowHeight,
					    -1);
      if ( temp  == NULL )
	{
	  xinfo(Xgc, "Not enough space to switch to Animation mode");
	}
      else
	{
	  int status;
	  xinfo(Xgc,"Animation mode is on,( xset('pixmap',0) to leave)");
	  Xgc->private->drawable = Xgc->private->extra_pixmap = temp;
	  status = nsp_set_gldrawable(Xgc, Xgc->private->extra_pixmap);
	  if ( status == FALSE )
	    {
	      Sciprintf("Gl rendering off-screen not working !\n");
	      cairo_surface_destroy(Xgc->private->extra_pixmap);
	      Xgc->private->extra_pixmap = NULL;
	      Xgc->private->drawable = (GdkDrawable *)Xgc->private->drawing->window;
	      Xgc->private->glcontext = gtk_widget_get_gl_context (Xgc->private->drawing);
	      Xgc->private->gldrawable = gtk_widget_get_gl_drawable (Xgc->private->drawing);
	      return ;
	    }
	  pixmap_clear_rect(Xgc,0,0,Xgc->CWindowWidth,Xgc->CWindowHeight);
	  Xgc->CurPixmapStatus = 1;
	}
    }
  else
    {
      /* I remove the extra pixmap to the window **/
      xinfo(Xgc," ");
      g_object_unref (G_OBJECT (Xgc->private->drawable));
      Xgc->private->drawable = (GdkDrawable *)Xgc->private->drawing->window;
      Xgc->CurPixmapStatus = 0;
      Xgc->private->glcontext = gtk_widget_get_gl_context (Xgc->private->drawing);
      Xgc->private->gldrawable = gtk_widget_get_gl_drawable (Xgc->private->drawing);
    }
#endif
}

/* used to create a gldrawable and glcontex ref when
 * we use a pixmap. Note that this function may not work
 * since many drivers seems not to accept to render GL scenes
 * off-screen. In that case we return FALSE.
 */
#if 0
static int nsp_set_gldrawable(BCG *Xgc,GdkPixmap *pixmap)
{
  static GdkGLConfig *glconfig;
  if (glconfig == NULL)
    glconfig = gdk_gl_config_new_by_mode (GDK_GL_MODE_RGB    |
					  GDK_GL_MODE_DEPTH  |
					  GDK_GL_MODE_SINGLE);
  if (glconfig == NULL)
    {
      Xgc->private->gldrawable= NULL;
      Xgc->private->glcontext = NULL;
      return FALSE;
    }

  if ( Xgc->private->gldrawable != NULL
       &&  GDK_IS_GL_DRAWABLE (Xgc->private->gldrawable))
     gdk_gl_drawable_gl_end (Xgc->private->gldrawable);

  if ( Xgc->private->drawing != NULL && Xgc->private->glcontext != gtk_widget_get_gl_context (Xgc->private->drawing))
    {
      g_object_unref(G_OBJECT(Xgc->private->glcontext));
    }

  Xgc->private->gldrawable = GDK_GL_DRAWABLE (gdk_pixmap_set_gl_capability (pixmap,
									    glconfig,
									    NULL));
  /*
   * Create OpenGL rendering context (not direct).
   */
  Xgc->private->glcontext = gdk_gl_context_new (Xgc->private->gldrawable,
						NULL,
						FALSE,
						GDK_GL_RGBA_TYPE);
  if (Xgc->private->glcontext == NULL)
    {
      g_print ("Cannot create the OpenGL rendering context\n");
      return FALSE;
    }

  gdk_error_trap_push ();
  gdk_gl_drawable_make_current(Xgc->private->gldrawable,Xgc->private->glcontext);
  gdk_flush ();
  if (gdk_error_trap_pop ())
    {
      return FALSE;
    }
  gdk_gl_drawable_gl_begin(Xgc->private->gldrawable,Xgc->private->glcontext);
  glClear(GL_DEPTH_BUFFER_BIT);
  nsp_ogl_set_view(Xgc);
  return TRUE;
}
#endif

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
  int old = Xgc->CurColor;
  double rgb[3];
  color = Max(1,color);
  if ( old == color ) return old;
  if ( Xgc->private->a_colors == NULL) return 1;
  Xgc->CurColor = color;
  nsp_get_color_rgb(Xgc,color,rgb,Xgc->private->a_colors);
  glColor3d(rgb[0], rgb[1], rgb[2]);
  return old;
}

static void xget_color_rgb(BCG *Xgc,double *rgb)
{
  if ( Xgc->private->a_colors == NULL)
    {
      rgb[0]=rgb[1]=rgb[2]=1.0; return;
    }
  nsp_get_color_rgb(Xgc,Xgc->CurColor,rgb,Xgc->private->a_colors);
}

#if 0
static void print_gl_config_attrib (GdkGLConfig *glconfig,
				    const gchar *attrib_str,
				    int          attrib,
				    gboolean     is_boolean)
{
  int value;
  g_print ("%s = ", attrib_str);
  if (gdk_gl_config_get_attrib (glconfig, attrib, &value))
    {
      if (is_boolean)
	g_print ("%s\n", value == TRUE ? "TRUE" : "FALSE");
      else
	g_print ("%d\n", value);
    }
  else
    g_print ("*** Cannot get %s attribute value\n", attrib_str);
}

static void examine_gl_config_attrib (GdkGLConfig *glconfig)
{
  g_print ("\nOpenGL visual configurations :\n\n");

  g_print ("gdk_gl_config_is_rgba (glconfig) = %s\n",
	   gdk_gl_config_is_rgba (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_is_double_buffered (glconfig) = %s\n",
	   gdk_gl_config_is_double_buffered (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_is_stereo (glconfig) = %s\n",
	   gdk_gl_config_is_stereo (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_has_alpha (glconfig) = %s\n",
	   gdk_gl_config_has_alpha (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_has_depth_buffer (glconfig) = %s\n",
	   gdk_gl_config_has_depth_buffer (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_has_stencil_buffer (glconfig) = %s\n",
	   gdk_gl_config_has_stencil_buffer (glconfig) ? "TRUE" : "FALSE");
  g_print ("gdk_gl_config_has_accum_buffer (glconfig) = %s\n",
	   gdk_gl_config_has_accum_buffer (glconfig) ? "TRUE" : "FALSE");

  g_print ("\n");

  print_gl_config_attrib (glconfig, "GDK_GL_USE_GL",           GDK_GL_USE_GL,           TRUE);
  print_gl_config_attrib (glconfig, "GDK_GL_BUFFER_SIZE",      GDK_GL_BUFFER_SIZE,      FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_LEVEL",            GDK_GL_LEVEL,            FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_RGBA",             GDK_GL_RGBA,             TRUE);
  print_gl_config_attrib (glconfig, "GDK_GL_DOUBLEBUFFER",     GDK_GL_DOUBLEBUFFER,     TRUE);
  print_gl_config_attrib (glconfig, "GDK_GL_STEREO",           GDK_GL_STEREO,           TRUE);
  print_gl_config_attrib (glconfig, "GDK_GL_AUX_BUFFERS",      GDK_GL_AUX_BUFFERS,      FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_RED_SIZE",         GDK_GL_RED_SIZE,         FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_GREEN_SIZE",       GDK_GL_GREEN_SIZE,       FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_BLUE_SIZE",        GDK_GL_BLUE_SIZE,        FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ALPHA_SIZE",       GDK_GL_ALPHA_SIZE,       FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_DEPTH_SIZE",       GDK_GL_DEPTH_SIZE,       FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_STENCIL_SIZE",     GDK_GL_STENCIL_SIZE,     FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ACCUM_RED_SIZE",   GDK_GL_ACCUM_RED_SIZE,   FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ACCUM_GREEN_SIZE", GDK_GL_ACCUM_GREEN_SIZE, FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ACCUM_BLUE_SIZE",  GDK_GL_ACCUM_BLUE_SIZE,  FALSE);
  print_gl_config_attrib (glconfig, "GDK_GL_ACCUM_ALPHA_SIZE", GDK_GL_ACCUM_ALPHA_SIZE, FALSE);
  g_print ("\n");
}
#endif

/*
 *
 */

static void  mygluLookAt(GLdouble eyex, GLdouble eyey, GLdouble eyez, GLdouble centerx,
			 GLdouble centery, GLdouble centerz, GLdouble upx, GLdouble upy,
			 GLdouble upz);

void afficher_repere(float ox, float oy, float oz)
{
  printf("Afficher_repere\n");
  glBegin(GL_LINES);
  glColor3f(1.0,0.0,0.0);
  glVertex3f(ox, oy, oz);
  glVertex3f(ox+10.0, oy, oz);
  glColor3f(0.0,1.0,0.0);
  glVertex3f(ox, oy, oz);
  glVertex3f(ox, oy+10.0, oz);
  glColor3f(0.0,0.0,1.0);
  glVertex3f(ox, oy, oz);
  glVertex3f(ox, oy, oz+10.0);
  glEnd();
}

int use_camera(BCG *Xgc)
{
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity ();
  mygluLookAt( Xgc->private->camera.position.x,
	     Xgc->private->camera.position.y,
	     Xgc->private->camera.position.z,
	     Xgc->private->camera.cible.x,
	     Xgc->private->camera.cible.y,
	     Xgc->private->camera.cible.z,
	     Xgc->private->camera.orientation.x,
	     Xgc->private->camera.orientation.y,
	     Xgc->private->camera.orientation.z);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(Xgc->private->camera.xmin ,
	  Xgc->private->camera.xmax ,
	  Xgc->private->camera.ymin ,
	  Xgc->private->camera.ymax ,
	  Xgc->private->camera.pt_near,
	  Xgc->private->camera.pt_far);
  glMatrixMode(GL_MODELVIEW);
  return 0;
}

void change_camera(BCG *Xgc,const double *val)
{
#if 1
  Xgc->private->camera.position.x=*val;val++;
  Xgc->private->camera.position.y=*val;val++;
  Xgc->private->camera.position.z=*val;val++;
  Xgc->private->camera.cible.x=*val;val++;
  Xgc->private->camera.cible.y=*val;val++;
  Xgc->private->camera.cible.z=*val;val++;
  Xgc->private->camera.orientation.x=*val;val++;
  Xgc->private->camera.orientation.y=*val;val++;
  Xgc->private->camera.orientation.z=*val;val++;
  Xgc->private->camera.pt_near=*val;val++;
  Xgc->private->camera.pt_far=*val;val++;
  Xgc->private->camera.xmin=*val;val++;
  Xgc->private->camera.xmax=*val;val++;
  Xgc->private->camera.ymin=*val;val++;
  Xgc->private->camera.ymax=*val;val++;
#endif
  expose_event_new( Xgc->private->drawing,NULL, Xgc);
}

/* select the view mode from 2d view to
 * 3d view.
 */

void nsp_ogl_set_view(BCG *Xgc)
{
  if ( Xgc->scales->scale_flag3d == 0 ) /* XXX */
    {
      nsp_ogl_set_2dview(Xgc);
    }
  else
    {
      nsp_ogl_set_3dview(Xgc);
    }
}

void compute_nsp_mvp2d(BCG *Xgc, float v[])
{
  int width = gtk_widget_get_allocated_width (Xgc->private->drawing);
  int height = gtk_widget_get_allocated_height (Xgc->private->drawing);
  graphene_matrix_t o, mv, mvp;
  graphene_vec3_t eye, center, up;
  /* view */
  graphene_vec3_init (&eye,0.f,0.f, 1.f);
  graphene_vec3_init (&center,0.f,0.f, -1.f);
  graphene_vec3_init (&up,0.f, 1.f,0.f);
  graphene_matrix_init_look_at (&mv, &eye, &center, &up);
  /* projection  */
  // graphene_matrix_init_ortho (&o, -width, width, height, -height, -4.f, 4.f);
  graphene_matrix_init_ortho (&o, 0.f, width, height, 0,-4.f,4.f);
  /* the product: note that compared to sdl all the matrices 
   * we have are the transposed of the sdl matrices 
   * Thus to obtaine sdl p*l*t*r 
   * we have to perform r*t*l*p 
   * to obtain the transpose of mvp
   */
  graphene_matrix_multiply(&mv,&o,&mvp);
  /* last transpose to be like in sdl */
  graphene_matrix_to_float(&mvp, v);
}

void nsp_ogl_set_2dview(BCG *Xgc)
{
  float v[16];
  compute_nsp_mvp2d(Xgc,v);
  glUniformMatrix4fv (mvp_location, 1, GL_FALSE, &v[0]);
  /* GL_DEPTH_TEST is disabled for 2d 
   * the drawing order gives the depth
   */
  glDisable(GL_DEPTH_TEST);
}

void compute_nsp_mvp3d(BCG *Xgc, float v[])
{
  /* double xs,ys; */
  double theta = Xgc->scales->theta;
  double alpha = Xgc->scales->alpha;
  double cost=cos((theta)*M_PI/180.0);
  double sint=sin((theta)*M_PI/180.0);
  double cosa=cos((alpha)*M_PI/180.0);
  double sina=sin((alpha)*M_PI/180.0);
  double cx= Xgc->scales->c[0];
  double cy= Xgc->scales->c[1];
  double cz= Xgc->scales->c[2];
  /* radius and center of the sphere circumscribing the box */
  double dx=Xgc->scales->bbox1[1]-Xgc->scales->bbox1[0];
  double dy=Xgc->scales->bbox1[3]-Xgc->scales->bbox1[2];
  double dz=Xgc->scales->bbox1[5]-Xgc->scales->bbox1[4];
  double R= (double) sqrt(dx*dx + dy*dy + dz*dz)/2;
  graphene_matrix_t o, mv, mvp;
  graphene_vec3_t eye, center, up;
  /* view */
  graphene_vec3_init (&eye,cx+R*cost*sina,cy+R*sint*sina,cz+R*cosa);
  graphene_vec3_init (&center,cx,cy,cz);
  graphene_vec3_init (&up,0,0,(sina >= 0.0 ) ? 1 : -1);
  graphene_matrix_init_look_at (&mv, &eye, &center, &up);
  /* projection  */
  graphene_matrix_init_ortho (&o,
			      XPi2R(Xgc->scales,0),XPi2R(Xgc->scales,Xgc->scales->wdim[0]),
			      YPi2R(Xgc->scales,Xgc->scales->wdim[1]),YPi2R(Xgc->scales,0),
			      -2*R,2*R);
  /* the product: note that compared to sdl all the matrices 
   * we have are the transposed of the sdl matrices 
   * Thus to obtaine sdl p*l*t*r 
   * we have to perform r*t*l*p 
   * to obtain the transpose of mvp
   */
  graphene_matrix_multiply(&mv,&o,&mvp);
  /* last transpose to be like in sdl */
  graphene_matrix_to_float(&mvp, v);
}

void nsp_ogl_set_3dview(BCG *Xgc)
{
  float v[16];
  compute_nsp_mvp3d(Xgc,v);
  glUniformMatrix4fv (mvp_location, 1, GL_FALSE, &v[0]);
  glEnable(GL_DEPTH_TEST);
}

static void drawline3D(BCG *Xgc,double x1,double y1, double z1, double x2,double y2, double z2)
{
  double rgb[3];
  int j,k;
  GLfloat vertex_colors[2*4];
  GLfloat vertex_data[]={x1,y1,z1,1.0f,
			 x2,y2,z2,1.0f};
  xget_color_rgb(Xgc, rgb);
  for ( j = 0 ; j < 2 ; j++)
    {
      for (k=0 ; k < 3; k++)
	vertex_colors[4*j + k ]=rgb[k];
      vertex_colors[4*j +3]= 1.0f;
    }
  shader_draw_line(vertex_data,vertex_colors,2,FALSE);
}

void drawsegments3D(BCG *Xgc,double *x,double *y,double *z, int n, int *style, int iflag)
{
  int dash,color,i;

  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_color(Xgc);
  if ( iflag == 1) { /* one style per segment */
    for (i=0 ; i < n/2 ; i++) {
      xset_line_style(Xgc,style[i]);
      drawline3D(Xgc,x[2*i],y[2*i],z[2*i],x[2*i+1],y[2*i+1],z[2*i+1]);
    }
  }
  else {
    if (*style >= 1) xset_line_style(Xgc,*style);
    /* une fonction gtk existe ici FIXME */
    for (i=0 ; i < n/2 ; i++)
      drawline3D(Xgc,x[2*i],y[2*i],z[2*i],x[2*i+1],y[2*i+1],z[2*i+1]);
  }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_color(Xgc,color);
}

void fillpolyline2D_shade(BCG *Xgc, double *vx, double *vy, int *colors, int n,int closeflag)
{
  double rgb[3];
  GLfloat vertex_colors[4*4];
  GLfloat vertex_data[4*4];
  gint i, k, j=0;
  if (!( n == 3 || n == 4) ) return;
  xget_color_rgb(Xgc, rgb);
  for ( i=0 ;  i < n ; i++)
    {
      vertex_data[j]= vx[i];
      vertex_data[j+1]= vy[i];
      vertex_data[j+2]= 0.0f;
      vertex_data[j+3]= 1.0f;
      for (k=0 ; k < 3; k++)
	vertex_colors[j + k ]=rgb[k];
      vertex_colors[j +3]= 1.0f;
      j += 4;
    }
  if ( n == 3 )
    shader_fill_triangle(vertex_data, vertex_colors);
  else
    shader_fill_quad(vertex_data,vertex_colors);
}

/**
 * fillpolylines3D_shade:
 * @Xgc:
 * @vectsx:
 * @vectsy:
 * @vectsz:
 * @fillvect:
 * @n:
 * @p:
 *
 * when we have one color for each node
 **/

static void fillpolyline3D_shade(BCG *Xgc, double *vx, double *vy, double *vz,int *colors, int n,int closeflag) ;

void fillpolylines3D_shade(BCG *Xgc,double *vectsx, double *vectsy,
			   double *vectsz, int *fillvect,int n, int p)
{
  int dash,color,i;

  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_color(Xgc);

  for (i = 0 ; i< n ; i++)
    {
      /* for each polyline we only take a decision according to the first color */
      if (fillvect[i] > 0 )
	{
	  /* fill + boundaries **/
	  glEnable(GL_POLYGON_OFFSET_FILL);
	  glPolygonOffset(1.0,1.0);
	  fillpolyline3D_shade(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,fillvect+(p)*i,p,1);
	  /* xset_dash_and_color(Xgc,&dash,&color); */
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_color(Xgc,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	  glDisable(GL_POLYGON_OFFSET_FILL);
	}
      else  if (fillvect[i] == 0 )
	{
	  /* xset_dash_and_color(Xgc,&dash,&color); */
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_color(Xgc,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else
	{
	  fillpolyline3D_shade(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,fillvect+(p)*i,p,1);
	  Xgc->graphic_engine->xset_color(Xgc,color);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_color(Xgc,color);
}


static void fillpolyline3D_shade(BCG *Xgc, double *vx, double *vy, double *vz,int *colors, int n,int closeflag)
{
  double rgb[3];
  GLfloat vertex_colors[4*4];
  GLfloat vertex_data[4*4];
  int i, k, j=0;
  if (!( n == 3 || n == 4) ) return;
  for ( i=0 ;  i< n ; i++)
    {
      vertex_data[j]= vx[i];
      vertex_data[j+1]= vy[i];
      vertex_data[j+2]= vz[i];
      vertex_data[j+3]= 1.0f;
      xset_color(Xgc,Abs(colors[i]));
      xget_color_rgb(Xgc, rgb);
      for (k=0 ; k < 3; k++)
	vertex_colors[j + k ]=rgb[k];
      vertex_colors[j +3]= 1.0f;
      j += 4;
    }
  if ( n == 3 )
    shader_fill_triangle(vertex_data,vertex_colors);
  else
    shader_fill_quad(vertex_data,vertex_colors);
}

/**
 * fillpolylines3D:
 * @Xgc:
 * @vectsx:
 * @vectsy:
 * @vectsz:
 * @fillvect:
 * @n:
 * @p:
 *
 * the same for 3D vertices
 * FIXME: a rajouter ds la table et rendre statique
 **/


void fillpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p)
{
  int dash,color,i;

  /* xget_dash_and_color(Xgc,&dash,&color); */
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_color(Xgc);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{
	  /* fill + boundaries **/
	  Xgc->graphic_engine->xset_color(Xgc,fillvect[i]);
	  glEnable(GL_POLYGON_OFFSET_FILL);
	  glPolygonOffset(1.0,1.0);
	  fillpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	  glDisable(GL_POLYGON_OFFSET_FILL);
	  /* xset_dash_and_color(Xgc,&dash,&color); */
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_color(Xgc,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_color(Xgc,color);
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	}
      else
	{
	  Xgc->graphic_engine->xset_color(Xgc,-fillvect[i]);
	  fillpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,1);
	  Xgc->graphic_engine->xset_color(Xgc,color);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_color(Xgc,color);
}

static void fillpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag)
{
  double rgb[3];
  GLfloat vertex_colors[4*4];
  GLfloat vertex_data[4*4];
  gint i, k, j=0;
  if (!( n == 3 || n == 4) ) return;
  xget_color_rgb(Xgc, rgb);
  for ( i=0 ;  i < n ; i++)
    {
      vertex_data[j]= vx[i];
      vertex_data[j+1]= vy[i];
      vertex_data[j+2]= vz[i];
      vertex_data[j+3]= 1.0f;
      for (k=0 ; k < 3; k++)
	vertex_colors[j + k ]=rgb[k];
      vertex_colors[j +3]= 1.0f;
      j += 4;
    }
  if ( n == 3 )
    shader_fill_triangle(vertex_data, vertex_colors);
  else
    shader_fill_quad(vertex_data,vertex_colors);
}

static void drawpolyline3D(BCG *Xgc, double *vx, double *vy, double *vz, int n,int closeflag)
{
  double rgb[3];
  GLfloat vertex_colors[10*4];
  GLfloat vertex_data[10*4];
  gint i=0, k, j = 0;
  if ( n <= 1) return;
  xget_color_rgb(Xgc, rgb);
  for ( i=0 ;  i < Min(10,n) ; i++)
    {
      vertex_data[j]= vx[i];
      vertex_data[j+1]= vy[i];
      vertex_data[j+2]= vz[i];
      vertex_data[j+3]= 1.0f;
      for (k=0 ; k < 3; k++)
	vertex_colors[j + k ]=rgb[k];
      vertex_colors[j +3]= 1.0f;
      j += 4;
    }
  shader_draw_line(vertex_data,vertex_colors,Min(10,n),closeflag);
}

/*
 *
 */

void drawpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *drawvect,int n, int p)
{
  int symb[2],dash,color,i,close;
  /* store the current values */
  Xgc->graphic_engine->xget_mark(Xgc,symb);
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_color(Xgc);

  for (i=0 ; i< n ; i++)
    {
      if (drawvect[i] <= 0)
	{ /* we use the markid : drawvect[i] : with current dash **/
	  Xgc->graphic_engine->xset_mark(Xgc,- drawvect[i],symb[1]);
	  drawpolymark3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p);
	}
      else
	{/* we use the line-style number abs(drawvect[i])  **/
	  Xgc->graphic_engine->xset_line_style(Xgc,*(drawvect+i));
	  close = 0;
	  drawpolyline3D(Xgc,vectsx+(p)*i,vectsy+(p)*i,vectsz+(p)*i,p,close);
	}
    }
  /* back to default values **/
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_color(Xgc,color);
  Xgc->graphic_engine->xset_mark(Xgc,symb[0],symb[1]);
}

/*
 * FIXME : experimental for tests
 */

#ifdef LIGHTS
static void init_gl_lights(GLfloat light0_pos[4])
{
  /* GLfloat light0_pos[4]   = { -50.0, 50.0, 50.0, 0.0 }; */
  GLfloat light0_color[4] = { .6, .6, .6, 1.0 }; /* white light */

  /* speedups */
  glEnable(GL_DITHER);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_FASTEST);

  /* light */
  glLightfv(GL_LIGHT0, GL_POSITION, light0_pos);
  glLightfv(GL_LIGHT0, GL_DIFFUSE,  light0_color);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);
  /* logical Ops */
  glEnable(GL_COLOR_LOGIC_OP);
}
#endif

#define FONTNUMBER 6
#define FONTMAXSIZE 6
#define SYMBOLNUMBER 17

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
#ifdef PANGO_VERSION_CHECK
#if   PANGO_VERSION_CHECK(1,22,0)
      g_object_unref (G_OBJECT (Xgc->private->ft2_context));
#endif
#endif
    }
}

static void nsp_fonts_initialize(BCG *Xgc)
{
  if ( Xgc->private->layout == NULL)
    {
      PangoFT2FontMap* pango_fm;
      Xgc->private->context = gtk_widget_get_pango_context (Xgc->private->drawing);
      /* a revoir deprecated XXXX */
#ifdef HAVE_FREETYPE
      pango_fm = (PangoFT2FontMap *) pango_ft2_font_map_new();
      pango_ft2_font_map_set_resolution (pango_fm,72,72);

#ifdef PANGO_VERSION_CHECK
#if   PANGO_VERSION_CHECK(1,22,0)
      Xgc->private->ft2_context= pango_font_map_create_context((PangoFontMap *)pango_fm);
#else
      Xgc->private->ft2_context= pango_ft2_font_map_create_context(pango_fm);
#endif
#else
      Xgc->private->ft2_context= pango_ft2_font_map_create_context(pango_fm);
#endif
      g_object_unref(pango_fm);
      Xgc->private->layout = pango_layout_new (Xgc->private->ft2_context);
      Xgc->private->mark_layout = pango_layout_new (Xgc->private->ft2_context);
#else
      Xgc->private->layout = NULL;
      Xgc->private->mark_layout = NULL;
#endif
      Xgc->private->desc = pango_font_description_new();
      Xgc->private->mark_desc = pango_font_description_from_string(pango_fonttab[1]);
    }
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


/* To get the  id and size of the current font
 */

static void  xget_font(BCG *Xgc,int *font,int full)
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

/* To get the current mark id */

static void xget_mark(BCG *Xgc,int *symb)
{
  symb[0] = Xgc->CurHardSymb ;
  symb[1] = Xgc->CurHardSymbSize ;
}


static void loadfamily(char *name, int *j)
{
}

static void queryfamily(char *name, int *j,int *v3)
{
}

/*
 * rendering text with PangoFT2
 * from: font-pangoft2.c written by Naofumi Yasufuku
 * <naofumi@users.sourceforge.net>
 * Note that if the layout uses a transformation matrix
 * the enclosing rectangle is given by e_rect
 */

#ifndef HAVE_FREETYPE

static void gl_pango_ft2_render_layout (PangoLayout *layout, GdkRectangle *e_rect)
{
  Sciprintf("Cannot render string in OpenGL missing pangoft2\n");
}

#else

static void gl_pango_ft2_render_layout (PangoLayout *layout, GdkRectangle *e_rect)
{
  int x,y;
  PangoRectangle logical_rect;
  static FT_Bitmap bitmap;
  static GLvoid *pixels=NULL;
  guint32 *p;
  GLfloat color[4];
  guint32 rgb;
  GLfloat a;
  guint8 *row, *row_end;
  int i;
  static int size = 0;
  pango_layout_get_extents (layout, NULL, &logical_rect);
  if (logical_rect.width == 0 || logical_rect.height == 0)
    return;

  if ( e_rect == NULL )
    {
      bitmap.rows = PANGO_PIXELS (logical_rect.height);
      bitmap.width = PANGO_PIXELS (logical_rect.width);
      bitmap.pitch = bitmap.width;
      x= PANGO_PIXELS(-logical_rect.x) ;
      y= 0;
    }
  else
    {
      /* rotated string: we use the enclosing rectangle */
      bitmap.rows = e_rect->height;
      bitmap.width = e_rect->width;
      bitmap.pitch = bitmap.width;
      /* note that displaystring will pass e_rect here is such that x=y=0 */
      x = - e_rect->x;
      y = - e_rect->y ;
    }
  /* if pixels != NULL then bitmap.buffer and pixels have already been initializes */
  if ( pixels != NULL )
    {
      if ( bitmap.rows * bitmap.width > size )
	bitmap.buffer = g_realloc (bitmap.buffer, bitmap.rows * bitmap.width);
    }
  else
    {
      bitmap.buffer = g_malloc (bitmap.rows * bitmap.width);
    }
  bitmap.num_grays = 256;
  bitmap.pixel_mode = ft_pixel_mode_grays;

  memset (bitmap.buffer, 0, bitmap.rows * bitmap.width);

  pango_ft2_render_layout (&bitmap, layout,x,y);

  if ( pixels != NULL)
    {
      if ( bitmap.rows * bitmap.width > size )
	pixels = g_realloc (pixels, bitmap.rows * bitmap.width*4);
    }
  else
    {
      pixels = g_malloc (bitmap.rows * bitmap.width * 4);
    }
  size = bitmap.rows * bitmap.width;
  p = (guint32 *) pixels;

  glGetFloatv (GL_CURRENT_COLOR, color);
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
  rgb =  ((guint32) (color[0] * 255.0))        |
        (((guint32) (color[1] * 255.0)) << 8)  |
        (((guint32) (color[2] * 255.0)) << 16);
#else
  rgb = (((guint32) (color[0] * 255.0)) << 24) |
        (((guint32) (color[1] * 255.0)) << 16) |
        (((guint32) (color[2] * 255.0)) << 8);
#endif
  a = color[3];

  row = bitmap.buffer + bitmap.rows * bitmap.width; /* past-the-end */
  row_end = bitmap.buffer;      /* beginning */

  if (a == 1.0)
    {
      do
        {
          row -= bitmap.width;
          for (i = 0; i < bitmap.width; i++)
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
            *p++ = rgb | (((guint32) row[i]) << 24);
#else
            *p++ = rgb | ((guint32) row[i]);
#endif
        }
      while (row != row_end);
    }
  else
    {
      do
        {
          row -= bitmap.width;
          for (i = 0; i < bitmap.width; i++)
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
            *p++ = rgb | (((guint32) (a * row[i])) << 24);
#else
            *p++ = rgb | ((guint32) (a * row[i]));
#endif
        }
      while (row != row_end);
    }

  glPixelStorei (GL_UNPACK_ALIGNMENT, 4);

  glEnable (GL_BLEND);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

#if !defined(GL_VERSION_1_2)
  glDrawPixels (bitmap.width, bitmap.rows,
                GL_RGBA, GL_UNSIGNED_BYTE,
                pixels);
#else
  glDrawPixels (bitmap.width, bitmap.rows,
                GL_RGBA, GL_UNSIGNED_INT_8_8_8_8,
                pixels);
#endif

  glDisable (GL_BLEND);
  /*
  g_free (bitmap.buffer);
  g_free (pixels);
  */
}

#endif

void xstring_pango(BCG *Xgc,char *str,int rect[],char *font,int size,int markup,int position)
{

}

/*
 * SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
 * Copyright (C) 1991-2000 Silicon Graphics, Inc. All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice including the dates of first publication and
 * either this permission notice or a reference to
 * http://oss.sgi.com/projects/FreeB/
 * shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * SILICON GRAPHICS, INC. BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Except as contained in this notice, the name of Silicon Graphics, Inc.
 * shall not be used in advertising or otherwise to promote the sale, use or
 * other dealings in this Software without prior written authorization from
 * Silicon Graphics, Inc.
 */

static void identityf(GLfloat m[16])
{
    m[0+4*0] = 1; m[0+4*1] = 0; m[0+4*2] = 0; m[0+4*3] = 0;
    m[1+4*0] = 0; m[1+4*1] = 1; m[1+4*2] = 0; m[1+4*3] = 0;
    m[2+4*0] = 0; m[2+4*1] = 0; m[2+4*2] = 1; m[2+4*3] = 0;
    m[3+4*0] = 0; m[3+4*1] = 0; m[3+4*2] = 0; m[3+4*3] = 1;
}

static void normalize(float v[3])
{
  float r =  sqrt( v[0]*v[0] + v[1]*v[1] + v[2]*v[2] );
  if (r == 0.0) return;
  v[0] /= r;
  v[1] /= r;
  v[2] /= r;
}

static void cross(float v1[3], float v2[3], float result[3])
{
  result[0] = v1[1]*v2[2] - v1[2]*v2[1];
  result[1] = v1[2]*v2[0] - v1[0]*v2[2];
  result[2] = v1[0]*v2[1] - v1[1]*v2[0];
}

static void  mygluLookAt(GLdouble eyex, GLdouble eyey, GLdouble eyez, GLdouble centerx,
			 GLdouble centery, GLdouble centerz, GLdouble upx, GLdouble upy,
			 GLdouble upz)
{
    float forward[3], side[3], up[3];
    GLfloat m[4][4];

    forward[0] = centerx - eyex;
    forward[1] = centery - eyey;
    forward[2] = centerz - eyez;

    up[0] = upx;
    up[1] = upy;
    up[2] = upz;

    normalize(forward);

    /* Side = forward x up */
    cross(forward, up, side);
    normalize(side);

    /* Recompute up as: up = side x forward */
    cross(side, forward, up);

    identityf(&m[0][0]);
    m[0][0] = side[0];
    m[1][0] = side[1];
    m[2][0] = side[2];

    m[0][1] = up[0];
    m[1][1] = up[1];
    m[2][1] = up[2];

    m[0][2] = -forward[0];
    m[1][2] = -forward[1];
    m[2][2] = -forward[2];

    glMultMatrixf(&m[0][0]);
    glTranslated(-eyex, -eyey, -eyez);
}

static GLuint create_shader (int type, const char *src);

/* Draw a single triangle using shaders */
/* fixed values of the triangle */

static void gen_buffers()
{
  glGenVertexArrays (1, &vao_triangles);
  glGenBuffers (1, &vbo_triangle_coords);
  glGenBuffers (1, &vbo_triangle_colors);
}

/* initialize a vao */

static void array_fill(void)
{
  GLfloat triangle_coords[] = {
    0.f, 0.5f, 0.f, 1.f,
    0.5f, -0.366f, 0.f, 1.f,
    -0.5f, -0.366f, 0.f, 1.f
  };
  GLfloat triangle_colors[] = {
    1.0, 1.0, 0.0, 1.f,
    0.0, 0.0, 1.0, 1.f,
    1.0, 0.0, 0.0, 1.f
  };

  glBindVertexArray (vao_triangles);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_triangle_coords);
  glBufferData(GL_ARRAY_BUFFER, sizeof(triangle_coords), triangle_coords, GL_STATIC_DRAW);
  glVertexAttribPointer (attribute_coords, 4, GL_FLOAT, GL_FALSE, 0, 0);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_triangle_colors);
  glBufferData(GL_ARRAY_BUFFER, sizeof(triangle_colors),
	       triangle_colors, GL_STATIC_DRAW);
  glVertexAttribPointer (attribute_colors, 4, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(attribute_coords);
  glEnableVertexAttribArray(attribute_colors);
  glBindVertexArray (0);
}

/* we create two shaders */

static const char *vertex_shader_code =
  "#version 330\n"				\
  "\n"						\
  "layout(location = 0) in vec4 position;\n"	\
  "layout(location = 1) in vec4 v_color;\n"	\
  "varying vec4 f_color;\n"			\
  "uniform mat4 mvp;\n"				\
  "void main() {\n"				\
  "  gl_Position = mvp * position;\n"		\
  "  f_color = v_color;\n"			\
  "}";

static const char *fragment_shader_code =
  "#version 330\n"				\
  "\n"						\
  "varying vec4 f_color;\n"			\
  "void main() {\n"				\
  "  gl_FragColor = f_color;"			\
  "}";

static void init_shaders (GLuint *program_out, GLuint *mvp_out)
{
  GLuint vertex, fragment;
  GLuint program = 0;
  GLuint mvp = 0;
  int status;
  
  vertex = create_shader (GL_VERTEX_SHADER, vertex_shader_code);
  if (vertex == 0)
    {
      *program_out = 0;
      return;
    }

  fragment = create_shader (GL_FRAGMENT_SHADER, fragment_shader_code);
  if (fragment == 0)
    {
      glDeleteShader (vertex);
      *program_out = 0;
      return;
    }

  program = glCreateProgram ();
  glAttachShader (program, vertex);
  glAttachShader (program, fragment);

  glLinkProgram (program);

  glGetProgramiv (program, GL_LINK_STATUS, &status);
  if (status == GL_FALSE)
    {
      int log_len;
      char *buffer;

      glGetProgramiv (program, GL_INFO_LOG_LENGTH, &log_len);

      buffer = g_malloc (log_len + 1);
      glGetProgramInfoLog (program, log_len, NULL, buffer);

      g_warning ("Linking failure:\n%s\n", buffer);

      g_free (buffer);

      glDeleteProgram (program);
      program = 0;

      goto out;
    }

  mvp = glGetUniformLocation (program, "mvp");

  glDetachShader (program, vertex);
  glDetachShader (program, fragment);

out:
  glDeleteShader (vertex);
  glDeleteShader (fragment);

  if (program_out != NULL)
    *program_out = program;

  if (mvp_out != NULL)
    *mvp_out = mvp;
}


/* generic utility: create a shader */

static GLuint
create_shader (int type, const char *src)
{
  GLint status = GL_FALSE;
  GLuint shader= glCreateShader (type);
  glShaderSource (shader, 1, &src, NULL);
  glCompileShader (shader);

  glGetShaderiv (shader, GL_COMPILE_STATUS, &status);
  if (status == GL_FALSE)
    {
      int log_len;
      char *buffer;

      glGetShaderiv (shader, GL_INFO_LOG_LENGTH, &log_len);

      buffer = g_malloc (log_len + 1);
      glGetShaderInfoLog (shader, log_len, NULL, buffer);
      g_warning ("Compile failure in %s shader:\n%s\n",
                 type == GL_VERTEX_SHADER ? "vertex" : "fragment",
                 buffer);
      g_free (buffer);
      glDeleteShader (shader);
      return 0;
    }
  return shader;
}

static void shader_fill_triangle(GLfloat vertex_data[],GLfloat vertex_colors[])
{
  int nvertex = 3;
  glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_coords);
  glBufferData (GL_ARRAY_BUFFER, 4*nvertex*sizeof(GLfloat), vertex_data, GL_STATIC_DRAW);
  if ( vertex_colors != NULL)
    {
      glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_colors);
      glBufferData (GL_ARRAY_BUFFER, 4*nvertex*sizeof(GLfloat), vertex_colors, GL_STATIC_DRAW);
    }
  glBindVertexArray (vao_triangles);
  glDrawArrays (GL_TRIANGLES, 0, nvertex);
  glBindVertexArray(0);
}

static void shader_fill_quad(GLfloat vertex_data[],GLfloat vertex_colors[])
{
  int nvertex = 4;
  glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_coords);
  glBufferData (GL_ARRAY_BUFFER, 4*nvertex*sizeof(GLfloat), vertex_data, GL_STATIC_DRAW);
  if ( vertex_colors != NULL)
    {
      glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_colors);
      glBufferData (GL_ARRAY_BUFFER, 4*nvertex*sizeof(GLfloat), vertex_colors, GL_STATIC_DRAW);
    }
  glBindVertexArray (vao_triangles);
  glDrawArrays (GL_TRIANGLE_FAN, 0, nvertex);
  glBindVertexArray(0);
}


static void shader_draw_line(GLfloat vertex_data[],GLfloat vertex_colors[],int n, int closeflag)
{
  glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_coords);
  glBufferData (GL_ARRAY_BUFFER, 4*n*sizeof(GLfloat), vertex_data, GL_STATIC_DRAW);
  if ( vertex_colors != NULL)
    {
      glBindBuffer (GL_ARRAY_BUFFER, vbo_triangle_colors);
      glBufferData (GL_ARRAY_BUFFER, 4*n*sizeof(GLfloat), vertex_colors, GL_STATIC_DRAW);
    }
  glBindVertexArray (vao_triangles);
  glDrawArrays ((closeflag) ? GL_LINE_LOOP: GL_LINE_STRIP, 0, n);
  glBindVertexArray(0);
}


