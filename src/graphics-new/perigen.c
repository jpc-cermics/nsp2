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

#include <nsp/nsp.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/graphics-new/perigen.h>

static driver_drawarrows drawarrows_gen;
static driver_drawsegments drawsegments_gen;
static driver_fillpolylines fillpolylines_gen;
static driver_drawarc drawarc_gen;
static driver_fillarc fillarc_gen;

nsp_gengine_generic nsp_peri_generic = {
  drawarrows_gen,
  drawsegments_gen,
  fillpolylines_gen,
  drawarc_gen,
  fillarc_gen,
};

/* Draw a set of segments
 *
 * segments are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1])
 * for i=0 step 2  n is the size of vx and vy
 * style and thickness can be scalars or vectors
 */

static void drawsegments_gen(BCG *Xgc, double *vx, double *vy, int n, int *color, int *width)
{
  int i;
  if ( color != NULL)
    {
      int cur_dash = Xgc->graphic_engine->xget_dash(Xgc);
      int cur_color = Xgc->graphic_engine->xget_color(Xgc);
      /* one color per segment */
      if ( width != NULL )
	{
	  /* one width per segment */
	  int c_width =  Xgc->graphic_engine->xget_thickness(Xgc);
	  for (i=0 ; i < n/2 ; i++)
	    {
	      Xgc->graphic_engine->xset_thickness(Xgc,width[i]);
	      Xgc->graphic_engine->xset_line_style(Xgc,color[i]);
	      Xgc->graphic_engine->drawline(Xgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
	    }
	  Xgc->graphic_engine->xset_thickness(Xgc,c_width);
	}
      else
	{
	  /* common width */
	  for (i=0 ; i < n/2 ; i++)
	    {
	      Xgc->graphic_engine->xset_line_style(Xgc,color[i]);
	      Xgc->graphic_engine->drawline(Xgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
	    }
	}
      Xgc->graphic_engine->xset_dash(Xgc,cur_dash);
      Xgc->graphic_engine->xset_color(Xgc,cur_color);
    }
  else
    {
      for (i=0 ; i < n/2 ; i++)
	Xgc->graphic_engine->drawline(Xgc,vx[2*i],vy[2*i],vx[2*i+1],vy[2*i+1]);
    }
}

/* Draw a set of arrows
 * arrows are defined by (vx[i],vy[i])->(vx[i+1],vy[i+1])
 * for i=0 step 2
 * n is the size of vx and vy
 * as is 10*arsize (arsize) the size of the arrow head in pixels
 */

static void drawarrows_gen(BCG *Xgc, double *vx, double *vy, int n, int as, int *style, int iflag)
{
  int dash,color,i,lstyle;
  double polyx[4],polyy[4];
  double cos20=cos(20.0*M_PI/180.0), sin20=sin(20.0*M_PI/180.0);
  dash = Xgc->graphic_engine->xget_dash(Xgc);
  color = Xgc->graphic_engine->xget_color(Xgc);
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
	  polyx[1]= polyx[0] - ( cos20*dx - sin20*dy);
	  polyy[1]= polyy[0] - ( sin20*dx + cos20*dy);
	  polyx[2]= polyx[0] - ( cos20*dx + sin20*dy);
	  polyy[2]= polyy[0] - (-sin20*dx + cos20*dy);
	  Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,&lstyle,nn,p);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_color(Xgc,color);
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

static void fillpolylines_gen(BCG *Xgc, const double *vectsx, const double *vectsy, int *fillvect,int n, int p)
{
  int color,i;
  color = Xgc->graphic_engine->xget_color(Xgc);
  for (i = 0 ; i< n ; i++)
    {
      if (fillvect[i] > 0 )
	{
	  /* fill + stroke */
	  Xgc->graphic_engine->xset_color(Xgc,fillvect[i]);
	  Xgc->graphic_engine->fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1,color);
	}
      else  if (fillvect[i] == 0 )
	{
	  Xgc->graphic_engine->xset_color(Xgc,color);
	  Xgc->graphic_engine->drawpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1);
	}
      else
	{
	  Xgc->graphic_engine->xset_color(Xgc,-fillvect[i]);
	  Xgc->graphic_engine->fillpolyline(Xgc,vectsx+(p)*i,vectsy+(p)*i,p,1,-1);
	  Xgc->graphic_engine->xset_color(Xgc,color);
	}
    }
  Xgc->graphic_engine->xset_color(Xgc,color);
}


#if 0
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
#endif 

static void drawarc_gen(BCG *Xgc, double arc[])
{
  double vx[365],vy[365];
  int k,n;
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

static void fillarc_gen( BCG *Xgc, double arc[])
{
  double vx[365],vy[365];
  double alpha,fact=0.01745329251994330;
  double w=arc[2]/2.0,h=arc[3]/2.0;
  int n = Min((arc[5]/64),360), count=0;
  int k,close = 1;
  if (n != 360)
    {
      vx[count] = arc[0] + w;
      vy[count] = arc[1] + h;
      count++;
    }
  for (k = 0; k < n ; ++k)
    {
      alpha=((arc[4]/64)+k)*fact;
      vx[count] = arc[0] + w*(cos(alpha)+1.0);
      vy[count] = arc[1] + h*(-sin(alpha)+1.0);
      count++;
    }
  if (n != 360)
    {
      vx[count] = arc[0] + w;
      vy[count] = arc[1] + h;
      count++;
    }
  Xgc->graphic_engine->fillpolyline(Xgc,vx, vy,count,close,-1);
}
