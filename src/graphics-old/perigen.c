/* Nsp
 * Copyright (C) 2004-2005 Jean-Philippe Chancelier Enpc/Cermics
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
 * A set of generic routines which can be used by all the drivers 
 * if the do not implement an accelerated function
 * jpc@cermics.enpc.fr 
 * 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <gtk/gtk.h>
#include "nsp/sciio.h"
#include "nsp/math.h"
#include "nsp/graphics/perigen.h"
#include "nsp/version.h"
#include "nsp/graphics/color.h"
#include "nsp/command.h"

static void nsp_remap_colors(BCG *Xgc,int remap,int *colmin,int *colmax,double *zmin, 
			     double *zmax,double *coeff,
			     const int *colminmax,const double *zminmax,const double z[],int zn);

static driver_fill_grid_rectangles fill_grid_rectangles_gen;
static driver_fill_grid_rectangles1 fill_grid_rectangles1_gen ;
static driver_drawarrows drawarrows_gen;
static driver_drawsegments drawsegments_gen;



nsp_gengine_generic nsp_peri_generic = {
  fill_grid_rectangles_gen,
  fill_grid_rectangles1_gen,
  drawarrows_gen,
  drawsegments_gen
};

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

static void fill_grid_rectangles1_gen(BCG *Xgc,const int x[],const int y[],const double z[], int nr, int nc,
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
	/* do not draw rectangles which are outside the colormap range */
	if ( fill[0] < colmin || fill[0] > colmax ) continue ;
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
				     int remap,const int *colminmax,const double *zminmax)
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
	if (color < colmin || color > colmax ) continue ;
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
	  polyx[0]= polyx[3]=vx[2*i+1]; /* +dx*cos20;*/
	  polyx[1]= inint(polyx[0]  - cos20*dx -sin20*dy );
	  polyx[2]= inint(polyx[0]  - cos20*dx + sin20*dy);
	  polyy[0]= polyy[3]=vy[2*i+1]; /* +dy*cos20;*/
	  polyy[1]= inint(polyy[0] + sin20*dx -cos20*dy) ;
	  polyy[2]= inint(polyy[0] - sin20*dx - cos20*dy) ;
	  Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,&lstyle,nn,p);
	}
    }
  Xgc->graphic_engine->xset_dash(Xgc,dash);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
}
