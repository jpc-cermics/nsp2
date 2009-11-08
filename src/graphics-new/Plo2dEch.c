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
 *--------------------------------------------------------------------------*/


#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include <math.h>
#include <gdk/gdk.h>
#include "nsp/math.h"
#include "nsp/graphics-new/Graphics.h"

/*--------------------------------------------------------------------
 * converts a rectangle into a wrect specification 
 * (see xsetech) 
 * This can be used to locally change the scales in order 
 * to set the plot region to a specified rectangle 
 * --------------------------------------------------------------------------*/

void scale_f2wrect(nsp_gcscale *scales,const double x[],double x1[])
{
  int i=0;
  if ( scales->logflag[0] == 'n' ) 
    {
      x1[i]=  XScale_d(scales,x[i]);
      x1[i+2]= scales->Wscx1*( x[i+2]);
    }
  else 
    {
      x1[i]= XLogScale_d(scales,x[i]);
      x1[i+2]=scales->Wscx1*(log10((x[i]+x[i+2])/x[i]));
    } 
  if ( scales->logflag[1] == 'n' ) 
    {
      x1[i+1]= YScale_d(scales,x[i+1]);
      x1[i+3]= scales->Wscy1*( x[i+3]);
    }
  else 
    {
      x1[i+1]= YLogScale_d(scales,x[i+1]);
      x1[i+3]= scales->Wscy1*(log10(x[i+1]/(x[i+1]-x[i+3])));
    }
  x1[0] /= (double) scales->wdim[0];
  x1[2] /= (double) scales->wdim[0];
  x1[1] /= (double) scales->wdim[1];
  x1[3] /= (double) scales->wdim[1];
} 


/*--------------------------------------------------------------------
 * Convert pixel<->double using current scale 
 * 
 *  echelle2d(x,y,x1,y1,n1,n2,rect,dir)
 *    x,y,x1,y1 of size [n1*n2] 
 *    rect[4] : the frame boundaries in pixel are returned in rect 
 *    dir     : "f2i" -> double to int (you give x and y and get x1,y1)
 *            : "i2f" -> int to double (you give x1 and y1 and get x,y)
 *    lstr    : unused (Fortran/C) 
 * --------------------------------------------------------------------------*/

void scale_f2i(nsp_gcscale *scales,const double x[],const double y[],int x1[],int y1[],int n)
{
  double xd,yd;
  int i;
  if (scales->logflag[0] == 'n' ) 
    {
      if (scales->logflag[1] == 'n') 
	{
	  for ( i=0 ; i < n  ; i++) 
	    {
	      xd = XScaleR_d(scales,x[i],y[i]);
	      yd = YScaleR_d(scales,x[i],y[i]);
	      x1[i]= Min(Max(-int16max,xd),int16max);
	      y1[i]= Min(Max(-int16max,yd),int16max);
	    }
	}
      else
	{
	  for ( i=0 ; i < n  ; i++) 
	    {
	      xd = XScaleR_d(scales,x[i],log10(y[i]));
	      yd = YScaleR_d(scales,x[i],log10(y[i]));
	      x1[i]= Min(Max(-int16max,xd),int16max);
	      y1[i]= Min(Max(-int16max,yd),int16max);
	    }
	}
    }
  else
    {
      if (scales->logflag[1] == 'n') 
	{
	  for ( i=0 ; i < n  ; i++) 
	    {
	      xd = XScaleR_d(scales,log10(x[i]),y[i]);
	      yd = YScaleR_d(scales,log10(x[i]),y[i]);
	      x1[i]= Min(Max(-int16max,xd),int16max);
	      y1[i]= Min(Max(-int16max,yd),int16max);
	    }
	}
      else
	{
	  for ( i=0 ; i < n  ; i++) 
	    {
	      xd = XScaleR_d(scales,log10(x[i]),log10(y[i]));
	      yd = YScaleR_d(scales,log10(x[i]),log10(y[i]));
	      x1[i]= Min(Max(-int16max,xd),int16max);
	      y1[i]= Min(Max(-int16max,yd),int16max);
	    }
	}
    }
}

void scale_i2f(nsp_gcscale *scales, double x[], double y[],const int x1[],const int y1[],int n)
{
  int i;
  if (scales->logflag[0] == 'n') 
    for ( i=0 ; i < n ; i++) x[i]= XPi2R(scales, x1[i] );
  else 
    for ( i=0 ; i < n ; i++) x[i]= exp10(XPi2R(scales, x1[i]));
  if (scales->logflag[1] == 'n') 
    for ( i=0 ; i < n ; i++)  y[i]= YPi2R(scales, y1[i] );
  else 
    for ( i=0 ; i < n ; i++)  y[i]= exp10(YPi2R(scales, y1[i]));
}

/*--------------------------------------------------------------------
 * void echelle2dl(x, y, x1, yy1, n1, n2,  dir)
 * like echelle2d but for length convertion 
 * Note that it cannot work in logarithmic scale 
 *--------------------------------------------------------------------*/

void length_scale_f2i(nsp_gcscale *scales,const double *x,const double *y, int *x1, int *y1, int n)
{
  int i;
  for ( i=0 ; i < n ; i++)
    {
      x1[i]=inint( scales->Wscx1*( x[i]));
      y1[i]=inint( scales->Wscy1*( y[i]));
    }
}

void length_scale_i2f(nsp_gcscale *scales,double *x, double *y, const int *x1, const int *y1, int n)
{
  int i;
  for ( i=0 ; i < n ; i++)
    {
      x[i]=x1[i]/scales->Wscx1;
      y[i]=y1[i]/scales->Wscy1;
    }
}

/* Utilities
 * scale conversions 
 */

void rect2d_f2i(nsp_gcscale *scales,const double x[],int x1[], int n)
{
  int i;
  /** double to int (pixel) direction **/
  for ( i=0 ; i < n ; i= i+4)
    {
      if ( scales->logflag[0] == 'n' ) 
	{
	  x1[i]=  XScale(scales,x[i]);
	  /* x1[i+2]=inint( scales->Wscx1*( x[i+2])); */
	  x1[i+2]= XScale(scales,x[i]+x[i+2]) -x1[i];
	}
      else 
	{
	  x1[i]= XLogScale(scales,x[i]);
	  x1[i+2]=inint( scales->Wscx1*(log10((x[i]+x[i+2])/x[i])));
	} 
      if ( scales->logflag[1] == 'n' ) 
	{
	  x1[i+1]= YScale(scales,x[i+1]);
	  /* x1[i+3]=inint( scales->Wscy1*( x[i+3]));*/
	  x1[i+3]= YScale(scales,x[i+1]-x[i+3]) - x1[i+1];
	}
      else 
	{
	  x1[i+1]= YLogScale(scales,x[i+1]);
	  x1[i+3]=inint( scales->Wscy1*(log10(x[i+1]/(x[i+1]-x[i+3]))));
	}
    }
} 
  
void rect2d_i2f(nsp_gcscale *scales,double x[],const  int x1[], int n)
{
  int i;
  for ( i=0 ; i < n ; i=i+4)
    {
      if ( scales->logflag[0] == 'n' ) 
	{
	  x[i]= XPi2R(scales,x1[i] );
	  x[i+2]=x1[i+2]/scales->Wscx1;
	}
      else
	{
	  x[i]=exp10( XPi2R(scales,x1[i]));
	  x[i+2]=exp10(XPi2R(scales, x1[i]+x1[i+2] ));
	  x[i+2] -= x[i];
	}
      if ( scales->logflag[1] == 'n' ) 
	{
	  x[i+1]= YPi2R(scales, x1[i+1]);
	  x[i+3]=x1[i+3]/scales->Wscy1;
	}
      else
	{
	  x[i+1]=exp10( YPi2R(scales, x1[i+1]));
	  x[i+3]=exp10( YPi2R(scales, x1[i+3]+x1[i+1])); 
	  x[i+2] -= x[i+1];
	}
    }
}







/* meme chose mais pour transformer des ellipses */

void ellipse2d(nsp_gcscale *scales,double *x, int *x1, int *n, char *dir)
{
  int i;
  if (strcmp("f2i",dir)==0) 
    {
      /** double to int (pixel) direction **/
      for ( i=0 ; i < (*n) ; i=i+6)
	{
	  x1[i  ]= XScale(scales,x[i]);
	  x1[i+1]= YScale(scales,x[i+1]);
	  x1[i+2]= inint( scales->Wscx1*( x[i+2]));
	  x1[i+3]= inint( scales->Wscy1*( x[i+3]));
	  x1[i+4]= inint( x[i+4]);
	  x1[i+5]= inint( x[i+5]);
	}
    }
  else if (strcmp("i2f",dir)==0) 
    {
      for ( i=0 ; i < (*n) ; i=i+6)
	{
	  x[i]=   XPi2R(scales,x1[i]); 
	  x[i+1]= YPi2R(scales, x1[i+1] ); 
	  x[i+2]= x1[i+2]/scales->Wscx1;
	  x[i+3]= x1[i+3]/scales->Wscy1;
	  x[i+4]= x1[i+4];
	  x[i+5]= x1[i+5];
	}
    }
  else 
    sciprint(" Wrong dir %s argument in echelle2d\r\n",dir);
}

 
/* meme chose mais pour axis */

void axis2d(nsp_gcscale *scales,double *alpha, double *initpoint, double *size, int *initpoint1, double *size1)
{
  double sina ,cosa;
  double xx,yy,scl;
  /* pour eviter des problemes numerique quand scales->scx1 ou scales->scy1 sont 
   *  tres petits et cosal ou sinal aussi 
   */
  if ( Abs(*alpha) == 90 ) 
    {
      scl=scales->Wscy1;
    }
  else 
    {
      if (Abs(*alpha) == 0) 
	{
	  scl=scales->Wscx1;
	}
      else 
	{
	  sina= sin(*alpha * M_PI/180.0);
	  cosa= cos(*alpha * M_PI/180.0);
	  xx= cosa*scales->Wscx1; xx *= xx;
	  yy= sina*scales->Wscy1; yy *= yy;
	  scl= sqrt(xx+yy);
	}
    }
  size1[0] = size[0]*scl;
  size1[1]=  size[1]*scl;
  size1[2]=  size[2];
  initpoint1[0]= XScale(scales,initpoint[0]);
  initpoint1[1]= YScale(scales,initpoint[1]);
}

/* Changement interactif d'echelle */

extern int EchCheckSCPlots();


