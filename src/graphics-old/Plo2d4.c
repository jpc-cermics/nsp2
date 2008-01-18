/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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

#include <string.h>
#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
/* #include "nsp/graphics/PloEch.h" */

static void Plo2d4RealToPixel (BCG *Xgc,int *n1, int *n2, double *x, double *y, int *xm, int *ym, char *xf);
/*--------------------------------------------------------------------
  nsp_plot2d_4(xf,x,y,n1,n2,style,strflag,legend,brect,aaint)
  --------------------------------------------------------------------------*/
  

int nsp_plot2d_4(BCG *Xgc,char *xf,double x[],double y[],int *n1,int *n2,int style[],char *strflag,
		 const char *legend,int legend_pos,double brect[],int aaint[])
{
  int n;
  int *xm=NULL,*ym=NULL;
  double arsize1=5.0,arsize2=5.0;
  /** Attention : 2*(*n2) **/
  int nn2=2*(*n2);
  int arsize,j;
  if ( CheckxfParam(xf)== 1) return(0);

  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,xf,x,y,n1,n2,aaint,strflag,brect);

  /* Storing values if using the Record driver */
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Plot4(Xgc,xf,x,y,n1,n2,style,strflag,legend,legend_pos,brect,aaint);

  /** Allocations **/
  n = (*n1)*nn2 ; 
  if ( n != 0 ) 
    {
      xm = graphic_alloc(0,n,sizeof(int));
      ym = graphic_alloc(1,n,sizeof(int));
      if ( xm == 0 || ym == 0) 
	{
	  sciprint("Running out of memory \n");
	  return 0;
	}      
      /** Real to Pixel values **/
      Plo2d4RealToPixel(Xgc,n1,n2,x,y,xm,ym,xf);

      /** Draw Axis or only rectangle **/
    }
  axis_draw(Xgc,strflag);
  if ( n != 0 ) 
    {
      /** Drawing the curves **/
      frame_clip_on(Xgc);
      
      nn2=2*(*n2)-1;
      arsize1= Xgc->scales->WIRect1[2]/70.0;
      arsize2= Xgc->scales->WIRect1[3]/70.0;
      arsize=  (arsize1 < arsize2) ? inint(10*arsize1) : inint(10*arsize2) ;
  
      for ( j = 0 ; j < (*n1) ; j++)
	{
	  int lstyle ;
	  lstyle = (style[j] < 0) ?  -style[j] : style[j];
	  Xgc->graphic_engine->drawarrows(Xgc,&xm[2*(*n2)*j],&ym[2*(*n2)*j],nn2,arsize ,&lstyle,0);
	  
	}
      frame_clip_off(Xgc);
      /** Drawing the Legends **/
      if ((int)strlen(strflag) >=1  && strflag[0] == '1' && legend_pos >= 0 && legend != NULL)
	{
	  nsp_legends(Xgc,legend_pos,*n1,style,legend,"@"); 
	}
    }
  return(0);
}



static void Plo2d4RealToPixel(BCG *Xgc,int *n1, int *n2, double *x, double *y, int *xm, int *ym, char *xf)
{
  int i,j;
  /** Computing y-values **/
  if ((int)strlen(xf) >= 3 && xf[2]=='l')	  
    {
      for ( i=0 ; i < (*n2) ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  ym[2*i+2*(*n2)*j]= YLogScale(y[i+(*n2)*j]);
      for ( i=0 ; i < (*n2)-1 ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  ym[2*i+1+2*(*n2)*j]=	  ym[2*i+2+2*(*n2)*j];
    }
  else 
    {
      for ( i=0 ; i < (*n2) ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  ym[2*i+2*(*n2)*j]= YScale(y[i+(*n2)*j]);
      for ( i=0 ; i < (*n2)-1 ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  ym[2*i+1+2*(*n2)*j]=	  ym[2*i+2+2*(*n2)*j];

    }

  /** Computing x-values **/
  switch (xf[0])
    {
    case 'e' :
      /** No X-value given by the user **/
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	{
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2) ; i++)
	      xm[2*i+2*(*n2)*j]= XLogScale(i+1.0);
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2)-1 ; i++)
	      xm[2*i+1+2*(*n2)*j]=	   xm[2*i+2+2*(*n2)*j];
	}
      else 
	{
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2) ; i++)
	      xm[2*i+2*(*n2)*j]= XScale((i+1.0));
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2)-1 ; i++)
	      xm[2*i+1+2*(*n2)*j]=	   xm[2*i+2+2*(*n2)*j];
	}
      break ;
    case 'o' :
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	{
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2) ; i++)
	      xm[2*i+2*(*n2)*j]= XLogScale(x[i]);
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2)-1 ; i++)
	      xm[2*i+1+2*(*n2)*j]=	   xm[2*i+2+2*(*n2)*j];
	}
      else 
     
	{
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2) ; i++)
	      xm[2*i+2*(*n2)*j]= XScale(x[i]);
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2)-1 ; i++)
	      xm[2*i+1+2*(*n2)*j]=	   xm[2*i+2+2*(*n2)*j];

	}
      break;
    case 'g' :
    default:
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	{
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2) ; i++)
	      xm[2*i+2*(*n2)*j]= XLogScale(x[i+(*n2)*j]);
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2)-1 ; i++)
	      xm[2*i+1+2*(*n2)*j]=	   xm[2*i+2+2*(*n2)*j];

	}
      else 
	{
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2) ; i++)
	      xm[2*i+2*(*n2)*j]= XScale(x[i+(*n2)*j]);
	  for (j=0 ; j< (*n1) ; j++)
	    for ( i=0 ; i < (*n2)-1 ; i++)
	      xm[2*i+1+2*(*n2)*j]=	xm[2*i+2+2*(*n2)*j];

	}
      break;
    }
}
