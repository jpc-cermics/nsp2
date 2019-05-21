/* Nsp
 * Copyright (C) 2004-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 * 
 * A set of generic routines which can be used by all the drivers 
 * if they do not wish to implement an accelerated version.
 * 
 * jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <gtk/gtk.h>
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "nsp/graphics-old/Graphics.h"
#include "nsp/graphics-old/perigen.h"
#include "nsp/version.h"
#include "nsp/graphics-old/color.h"
#include "nsp/command.h"


static driver_fill_grid_rectangles fill_grid_rectangles_gen;
static driver_fill_grid_rectangles1 fill_grid_rectangles1_gen ;
static driver_drawarrows drawarrows_gen;
static driver_drawsegments drawsegments_gen;
static driver_drawrectangles drawrectangles_gen;
static driver_drawarcs drawarcs_gen;
static driver_fillarcs fillarcs_gen;
static driver_drawpolylines drawpolylines_gen;
static driver_fillpolylines fillpolylines_gen;
static driver_displaynumbers displaynumbers_gen;
static driver_drawaxis drawaxis_gen; 
static driver_drawarc drawarc_gen; 
static driver_fillarc fillarc_gen; 
static driver_draw_pixbuf draw_pixbuf_gen; 
static driver_draw_pixbuf_from_file draw_pixbuf_from_file_gen; 
static driver_xset_test xset_test;

nsp_gengine_generic nsp_peri_generic_old = {
  fill_grid_rectangles_gen,
  fill_grid_rectangles1_gen,
  drawarrows_gen,
  drawsegments_gen,
  drawrectangles_gen,
  drawarcs_gen,
  fillarcs_gen,
  drawpolylines_gen,
  fillpolylines_gen,
  displaynumbers_gen,
  drawaxis_gen,
  drawarc_gen,
  fillarc_gen,
  draw_pixbuf_gen,
  draw_pixbuf_from_file_gen,
  xset_test
};

static void nsp_remap_colors(BCG *Xgc,int remap,int *colmin,int *colmax,double *zmin, 
			     double *zmax,double *coeff, const int *colminmax,
			     const double *zminmax,const double z[],int zn);


/**
 * fill_grid_rectangles1_gen:
 * @Xgc: 
 * @x: array of int of size nc+1
 * @y: array of int of size nr+1
 * @z: array of double of size nr*nc
 * 
 * A generic function for drawing a set of rectangles 
 * which is accelerated on Gtk driver (see periGtk.c) 
 * 
 *  x : of size nc+1 gives the x-values of the grid 
 *  y : of size nr+1 gives the y-values of the grid 
 *  z : of size nr*nc  gives the color to be used 
 *      on the rectangle defined by ( x[i],y[j], x[i+1],y[j+1])
 *  if zremap = %f then z values are considered as color id 
 *  if zremap = %t zvalues are remapped to colors 
 *      if colminmax== NULL then zmin,zmax are remapped to the min and max 
 *         values of current colormap
 *      else  the zminmax range is remapped to the colmimax range 
 *         and rectangles outside the range are not drawn 
 * 
 **/

static void fill_grid_rectangles1_gen(BCG *Xgc,const int x[],const int y[],const double z[], 
				      int nr, int nc,
				      int remap,const int *colminmax,const double *zminmax)
{
  int colmin,colmax;
  double zmin,zmax,coeff;
  int i,j,fill[1],cpat,xz[2];
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
  
  nsp_remap_colors(Xgc,remap,&colmin,&colmax,&zmin,&zmax,&coeff,colminmax,zminmax,z,nr*nc);

  for (i = 0 ; i < nr ; i++)
    for (j = 0 ; j < nc ; j++)
      {
	int w,h;
	fill[0]= (remap == FALSE) ? rint(z[i+nr*j]) : rint((colmax-colmin)*(z[i+nr*j] - zmin)*coeff + colmin);
	if ( fill[0] < colmin || fill[0] > colmax )
	  {
	    /* do not draw rectangles which are outside the colormap range 
	     * execpt if colout is non null 
	     */
	    continue;
	  }
	Xgc->graphic_engine->xset_pattern(Xgc,fill[0]);
	w=Abs(x[j+1]-x[j]);
	h=Abs(y[i+1]-y[i]);
	/* We don't trace rectangle which are totally out **/
	if ( w != 0 && h != 0 && x[j] < xz[0] && y[i] < xz[1] && x[j]+w > 0 && y[i]+h > 0 )
	  if ( Abs(x[j]) < int16max && Abs(y[i+1]) < int16max && w < uns16max && h < uns16max)
	    {
	      int rect[]={x[j],y[i],w,h};
	      Xgc->graphic_engine->fillrectangle(Xgc,rect);
	    }
      }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
}


/**
 * fill_grid_rectangles_gen:
 * @Xgc: 
 * @x: 
 * @y: 
 * @z: 
 * @nx: 
 * @ny: 
 * 
 * A generic function for drawing a set of rectangles 
 * which is accelerated on Gtk driver (see periGtk.c) 
 * 
 *  x : of size nx gives the x-values of the grid 
 *  y : of size ny gives the y-values of the grid 
 *  z : of size (nx)*(ny). 
 *  the rectangle ( x[i],y[j], x[i+1],y[j+1]) 
 *  the average value of z is computed on each corner of the rectangle 
 *  defined by ( x[i],y[j], x[i+1],y[j+1]). Then this value is 
 *  converted to a colorvalue using the current colormap.
 * 
 **/

static void fill_grid_rectangles_gen(BCG *Xgc,const int x[],const int y[],const double z[], int nx, int ny,
				     int remap,const int *colminmax,const double *zminmax, const int *colout)
{
  int colmin,colmax;
  double zmax,zmin,coeff,zmoy;
  int i,j,color,cpat,xz[2];
  
  nsp_remap_colors(Xgc,remap,&colmin,&colmax,&zmin,&zmax,&coeff,colminmax,zminmax,z,nx*ny);
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
  for (i = 0 ; i < (nx)-1 ; i++)
    for (j = 0 ; j < (ny)-1 ; j++)
      {
	int w,h;
	zmoy=1/4.0*(z[i+nx*j]+z[i+nx*(j+1)]+z[i+1+nx*j]+z[i+1+nx*(j+1)]);
	color = (remap == FALSE) ? rint(zmoy) : rint((colmax-colmin)*(zmoy - zmin)*coeff + colmin);
	if (color < colmin || color > colmax )
	  {
	    if ( colout == NULL) continue;
	    color = ( color < colmin ) ? colout[0] : colout[1];
	    if ( color <= 0 ) continue;
	  }
	Xgc->graphic_engine->xset_pattern(Xgc,color);
        w=Abs(x[i+1]-x[i]);h=Abs(y[j+1]-y[j]);
	/* We don't trace rectangle which are totally out **/
	if ( w != 0 && h != 0 && x[i] < xz[0] && y[j+1] < xz[1] && x[i]+w > 0 && y[j+1]+h > 0 )
	  {
	    if ( Abs(x[i]) < int16max && Abs(y[j+1]) < int16max && w < uns16max && h < uns16max)
	      {
		int rect[]={x[i],y[j+1],w,h};
		Xgc->graphic_engine->fillrectangle(Xgc,rect);
	      }
	    else 
	      {
		/* fprintf(stderr,"Rectangle too large \n"); */
	      }
	  }
      }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
}


/*
 */

static void nsp_remap_colors(BCG *Xgc,int remap,int *colmin,int *colmax,double *zmin, double *zmax,double *coeff,
		      const int *colminmax,const double *zminmax,const double z[],int zn)
{
  *colmin=1;
  *colmax=Xgc->graphic_engine->xget_last(Xgc);
  if ( remap == TRUE) 
    {
      if ( colminmax != NULL) 
	{
	  *colmax = Min(*colmax,colminmax[1]);
	  *colmin = Max(*colmin,colminmax[0]);
	}
      if ( zminmax == NULL) 
	{
	  *zmin = Mini(z,zn);
	  *zmax = Maxi(z,zn);
	  *coeff = ( *zmin == *zmax ) ? 1.0: 1.0/(*zmax-*zmin);
	}
      else 
	{
	  *zmin = zminmax[0];
	  *zmax = zminmax[1];
	  *coeff = ( *zmin == *zmax ) ? 1.0: 1.0/(*zmax-*zmin);
	}
    }
}


/* Draw a set of segments 
 *
 * segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) 
 * for i=0 step 2  n is the size of vx and vy 
 */

static void drawsegments_gen(BCG *Xgc, int *vx, int *vy, int n, int *style, int iflag)
{
  int dash,color,i;
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
  if ( iflag == 1) 
    { 
      /* one style per segment */
      for (i=0 ; i < n/2 ; i++) 
	{
	  Xgc->graphic_engine->xset_line_style(Xgc,style[i]);
	  Xgc->graphic_engine->drawline(Xgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
	}
    }
  else 
    {
      if (*style >= 1) Xgc->graphic_engine->xset_line_style(Xgc,*style);
      /* une fonction gtk existe ici FIXME */
      for (i=0 ; i < n/2 ; i++) 
	Xgc->graphic_engine->drawline(Xgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
}

/* Draw a set of arrows 
 * arrows are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1]) 
 * for i=0 step 2 
 * n is the size of vx and vy 
 * as is 10*arsize (arsize) the size of the arrow head in pixels 
 */

static void drawarrows_gen(BCG *Xgc, int *vx, int *vy, int n, int as, int *style, int iflag)
{ 
  int dash,color,i,lstyle,polyx[4],polyy[4];
  double cos20=cos(20.0*M_PI/180.0), sin20=sin(20.0*M_PI/180.0);
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
  for (i=0 ; i < n/2 ; i++)
    { 
      double dx,dy,norm;
      lstyle = (iflag == 1) ? style[i] : ( *style < 1 ) ? color : *style; 
      Xgc->graphic_engine->xset_line_style(Xgc,lstyle);
      Xgc->graphic_engine->drawline(Xgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
      dx=( vx[2*i+1]-vx[2*i]);
      dy=( vy[2*i+1]-vy[2*i]);
      norm = sqrt(dx*dx+dy*dy);
      if ( Abs(norm) >  SMDOUBLE ) 
	{
	  int nn=1,p=3;
	  dx=(as/10.0)*dx/norm;dy=(as/10.0)*dy/norm;
	  polyx[0]= polyx[3]=vx[2*i+1];
	  polyy[0]= polyy[3]=vy[2*i+1];
	  polyx[1]= inint(polyx[0] - ( cos20*dx - sin20*dy));
	  polyy[1]= inint(polyy[0] - ( sin20*dx + cos20*dy));
	  polyx[2]= inint(polyx[0] - ( cos20*dx + sin20*dy));
	  polyy[2]= inint(polyy[0] - (-sin20*dx + cos20*dy)) ;
	  Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,&lstyle,nn,p);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
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

static void drawrectangles_gen(BCG *Xgc,const int *vects,const int *fillvect, int n)
{
  int i,dash,color;
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
  for (i = 0 ; i < n ; i++)
    {
      if ( fillvect[i] < 0 )
	{
	  Xgc->graphic_engine->xset_line_style(Xgc,- fillvect[i]);
	  Xgc->graphic_engine->drawrectangle(Xgc,vects+4*i);
	}
      else if ( fillvect[i] == 0 ) 
	{
	  Xgc->graphic_engine->drawrectangle(Xgc,vects+4*i);
	}
      else
	{
	  Xgc->graphic_engine->xset_pattern(Xgc,fillvect[i]);
	  Xgc->graphic_engine->fillrectangle(Xgc,vects+4*i);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
}



/* 
 * Draw a set of (*n) polylines (each of which have (*p) points) 
 * with lines or marks 
 * drawvect[i] <= 0 use a mark for polyline i
 * drawvect[i] >  0 use a line style for polyline i 
 */

static void drawpolylines_gen(BCG *Xgc,int *vectsx, int *vectsy, int *drawvect,int n, int p)
{ 
  const int close =0;
  int symb[2],dash,color,i;
  /* store the current values */
  Xgc->graphic_engine->xget_mark(Xgc,symb);
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
  for (i=0 ; i< n ; i++)
    {
      if (drawvect[i] <= 0)
	{ 
	  /* we use the markid : drawvect[i] : with current dash */
	  Xgc->graphic_engine->xset_mark(Xgc,- drawvect[i],symb[1]);
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  Xgc->graphic_engine->drawpolymark(Xgc,vectsx+(p)*i,vectsy+(p)*i,p);
	}
      else
	{
	  /* we use the line-style number abs(drawvect[i])  */
	  Xgc->graphic_engine->xset_line_style(Xgc, *(drawvect+i));
	  Xgc->graphic_engine->drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,close);
	}
    }
  /* back to default values */
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  Xgc->graphic_engine->xset_mark(Xgc,symb[0],symb[1]);
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
 *
 */

static void fillpolylines_gen(BCG *Xgc,int *vectsx, int *vectsy, int *fillvect,int n, int p)
{
  int dash,color,i;
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{ 
	  /** fill + boundaries **/
	  Xgc->graphic_engine->xset_pattern(Xgc,fillvect[i]);
	  Xgc->graphic_engine->fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  Xgc->graphic_engine->drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else  if (fillvect[i] == 0 )
	{
	  Xgc->graphic_engine->xset_dash(Xgc,dash);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	  Xgc->graphic_engine->drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else 
	{
	  Xgc->graphic_engine->xset_pattern(Xgc,-fillvect[i]);
	  Xgc->graphic_engine->fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	  Xgc->graphic_engine->xset_pattern(Xgc,color);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
}

/*
 * Draw a set of ellipsis or part of ellipsis 
 * Each is defined by 6-parameters, 
 * ellipsis i is specified by $vect[6*i+k]_{k=0,5}= x,y,width,height,angle1,angle2$ 
 * <x,y,width,height> is the bounding box 
 * angle1,angle2 specifies the portion of the ellipsis 
 * caution : angle=degreangle*64          
 */

static void drawarcs_gen(BCG *Xgc, int *vects, int *style, int n)
{
  int dash,color,i;
  /* store the current values */
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_pattern(Xgc);
  for (i=0 ; i< n ; i++)
    {
      Xgc->graphic_engine->xset_line_style(Xgc,style[i]);
      Xgc->graphic_engine->drawarc(Xgc,vects+6*i);
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
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

static void fillarcs_gen(BCG *Xgc,int *vects, int *fillvect, int n) 
{
  int i,cpat;
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  for (i=0 ; i< n ; i++)
    {
      if (fillvect[i] > Xgc->IDLastPattern + 1)
	{
	  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
	  Xgc->graphic_engine->drawarc(Xgc,vects+6*i);
	}
      else
	{
	  Xgc->graphic_engine->xset_pattern(Xgc,fillvect[i]);
	  Xgc->graphic_engine->fillarc(Xgc,vects+6*i);
	}
    }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
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

static void drawaxis_gen(BCG *Xgc, int alpha, int *nsteps, int *initpoint,double *size)
{
  int i;
  double xi,yi,xf,yf;
  double cosal,sinal;
  cosal= cos( (double)M_PI * (alpha)/180.0);
  sinal= sin( (double)M_PI * (alpha)/180.0);
  for (i=0; i <= nsteps[0]*nsteps[1]; i++)
    {
      if (( i % nsteps[0]) != 0)
	{
	  xi = initpoint[0]+i*size[0]*cosal;
	  yi = initpoint[1]+i*size[0]*sinal;
	  xf = xi - ( size[1]*sinal);
	  yf = yi + ( size[1]*cosal);
	  Xgc->graphic_engine->drawline(Xgc, xi,yi,xf,yf) ;
	}
    }
  for (i=0; i <= nsteps[1]; i++)
    { 
      xi = initpoint[0]+i*nsteps[0]*size[0]*cosal;
      yi = initpoint[1]+i*nsteps[0]*size[0]*sinal;
      xf = xi - ( size[1]*size[2]*sinal);
      yf = yi + ( size[1]*size[2]*cosal);
      Xgc->graphic_engine->drawline(Xgc, xi,yi,xf,yf) ;
    }
}

/*
 * Display numbers z[i] at location (x[i],y[i])
 *   with a slope alpha[i] (see displaystring), if flag==1
 *   add a box around the string, only if slope =0}
 */

static void displaynumbers_gen(BCG *Xgc, int *x, int *y, int n, int flag, double *z, double *alpha)
{
  int i ;
  static char buf[56];
  for (i=0 ; i< n ; i++)
    { 
      sprintf(buf,Xgc->CurNumberDispFormat,z[i]);
      Xgc->graphic_engine->displaystring(Xgc,buf,x[i],y[i],flag,alpha[i]);
    }
}



static void drawarc_gen(BCG *Xgc,int arc[])
{ 
  int vx[365],vy[365],k,n;
  double alpha,fact=0.01745329251994330,w,h;
  int close = 0;
  w = arc[2]/2.0;
  h = arc[3]/2.0;
  n = Min((arc[5]/64),360);
  for (k = 0; k < n; ++k) {
    alpha=(( arc[4]/64)+k)*fact;
    vx[k] = arc[0] + w*(cos(alpha)+1.0);
    vy[k] = arc[1] + h*(-sin(alpha)+1.0);
  }
  Xgc->graphic_engine->drawpolyline(Xgc,vx, vy,n, close);
}

static void fillarc_gen( BCG *Xgc,int arc[])
{ 
  int vx[365],vy[365],k,k0,kmax,n;
  double alpha,fact=0.01745329251994330,w,h;
  int close = 1;
  
  /* A revoir XXXX fillarc_gen est faux */
  drawarc_gen(Xgc,arc);
  return;

  w = arc[2]/2.0;
  h = arc[3]/2.0;
  n = Min((arc[5]/64),360);

  k0 = 0;
  kmax = n-1;

  if (n != 360) 
    {
      vx[0] = arc[0] + w;
      vy[0] = arc[1] + h;
      k0 = 1;
      kmax = n;
    }
  for (k = k0; k <= kmax; ++k) {
    alpha=(( arc[4]/64)+k)*fact;
    vx[k] = arc[0] + w*(cos(alpha)+1.0);
    vy[k] = arc[1] * h*(-sin(alpha)+1.0);}
  if (n != 360) 
    {
      n++;
      vx[n] = arc[0] + w;
      vy[n] = arc[1] + h;
      n++;
    }
  Xgc->graphic_engine->fillpolyline(Xgc,vx, vy,n,close);
}

/*
 *
 */

static void   draw_pixbuf_gen(BCG *Xgc,void *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  Sciprintf("draw_pixbuf not implemented in this driver\n");
}

static void draw_pixbuf_from_file_gen(BCG *Xgc,const char *pix,int src_x,int src_y,int dest_x,int dest_y,int width,int height)
{
  Sciprintf("draw_pixbuf not implemented in this driver\n");
}



#define XN2DD 2
#define NCURVES2DD  1

static void   xset_test(BCG *Xgc)
{
  static int count=0;
  char str[56];
  int keep;
  int style[NCURVES2DD],aaint[4],n1,n2;
  double x[NCURVES2DD*XN2DD],y[NCURVES2DD*XN2DD],brect[4];
  int i,j;
  for ( j =0 ; j < NCURVES2DD ; j++)
    {
      i=0;
      x[i+ XN2DD*j]= 2.0;
      y[i+ XN2DD*j]= 9.0;
      i=1;
      x[i+ XN2DD*j]= 3.0;
      y[i+ XN2DD*j]= 1.0;
    }
  for ( i=0 ; i < NCURVES2DD ; i++)
    style[i]= NCURVES2DD+i;
  n1=NCURVES2DD;n2=XN2DD;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  brect[0]=brect[1]=0;brect[2]=brect[3]=10.0;
  keep= Xgc->record_flag;
  Xgc->record_flag = FALSE;
  nsp_plot2d_old(Xgc,x,y,&n1,&n2,style,"011"," ",4,brect,aaint);
  sprintf(str,"count=%d",count);
  count++;
  Xgc->graphic_engine->scale->displaystring(Xgc,str,5,5,0,0);
  Xgc->record_flag= keep;
}

