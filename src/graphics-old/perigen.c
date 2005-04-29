/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 2004 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
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

nsp_gengine_generic nsp_peri_generic = {
  fill_grid_rectangles_gen,
  fill_grid_rectangles1_gen
};

/*
 * A set of generic routines which can be used 
 * by all the drivers 
 */

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
 * FIXME: 
 *   set of generic functions which are to be moved on 
 *   perigen.h 
 *   they can be accelerated for each periXXXX
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


