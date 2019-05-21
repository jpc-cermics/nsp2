/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/graphics-old/Graphics.h"
/* #include "nsp/graphics-old/PloEch.h" */

static void Plo2d1RealToPixel (BCG *Xgc,int *n1, int *n2, double *x, double *y, int *xm, int *ym, char *xf);

/*--------------------------------------------------------------------
  nsp_plot2d_1(xf,x,y,n1,n2,style,strflag,legend,brect,aint)
  
  similar to plot2d plus one additionnal argument xf 
  and a special treatment for x 

  there's a splecial treatment for x 
  if xf[0]='e' for empty : x can point to nothing, the x -values 
  are assumed to be x[i+(*n2)*j]= i 
  if xf[0]='o' for one   : all the curves have the same x values 
  x is of size *n2  xx[i+(*n2)*j] = x[i];
  if xf[0]='g' for general : x is of size (*n2)*(n1);

  xf[1]='l' or 'n' LogAxis or standard on X
  xf[2]='l' or 'n' LogAxis or standard on Y
  --------------------------------------------------------------------------*/

int nsp_plot2d_1(BCG *Xgc,char *xf,double x[],double y[],int *n1,int *n2,int style[],char *strflag,
		 const char *legend,int legend_pos,double brect[],int aaint[])
{
  int n;
  int *xm=NULL,*ym=NULL, nn2=(*n2);
  if ( CheckxfParam(xf)== 1) return(0);

  /* Boundaries of the frame **/
  update_frame_bounds_old(Xgc,0,xf,x,y,n1,n2,aaint,strflag,brect);

  /* Storing values if using the Record driver */
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    nsp_gengine_record_old.store_Plot1(Xgc,xf,x,y,n1,n2,style,strflag,legend,legend_pos,brect,aaint);

  /* Allocation **/

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
      /* Real to Pixel values **/
      Plo2d1RealToPixel(Xgc,n1,n2,x,y,xm,ym,xf);
    }
  /* Drawing axes */
  axis_draw_old(Xgc,(strlen(strflag) >= 3) ? strflag[2] : '1', 
	    (strlen(strflag) >= 2) ? strflag[1] : '6',-1);
  /* Drawing the curves */
  if ( n != 0 ) 
    {
      frame_clip_on_old(Xgc);
      Xgc->graphic_engine->drawpolylines(Xgc,xm,ym,style,*n1,nn2);
      frame_clip_off_old(Xgc);
      /* Drawing the Legends **/
      if ((int)strlen(strflag) >=1  && strflag[0] == '1' && legend_pos >= 0 && legend != NULL)
	{
	  nsp_legends_old(Xgc,legend_pos,*n1,style,legend,"@"); 
	}
    }
  return(0);

}

static void Plo2d1RealToPixel(BCG *Xgc,int *n1, int *n2, double *x, double *y, int *xm, int *ym, char *xf)
{
  int i,j;
  /* Computing y-values **/
  if ((int)strlen(xf) >= 3 && xf[2]=='l')	  
    {
      for ( i=0 ; i < (*n2) ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  ym[i+(*n2)*j]= YLogScale(y[i+(*n2)*j]);
    }
  else 
    {
      for ( i=0 ; i < (*n2) ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  ym[i+(*n2)*j]= YScale(y[i+(*n2)*j]);
    }
  
  /* Computing x-values **/
  switch (xf[0])
    {
    case 'e' :
      /* No X-value given by the user **/
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	for ( i=0 ; i < (*n2) ; i++)
	  for (j=0 ; j< (*n1) ; j++)
	    xm[i+(*n2)*j]= XLogScale(i+1.0);
      else 
	for ( i=0 ; i < (*n2) ; i++)
	  for (j=0 ; j< (*n1) ; j++)
	    xm[i+(*n2)*j]= XScale(i+1.0);
      break ;
    case 'o' :
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	for ( i=0 ; i < (*n2) ; i++)
	  for (j=0 ; j< (*n1) ; j++)
	    xm[i+(*n2)*j]= XLogScale(x[i]);
      else 
	for ( i=0 ; i < (*n2) ; i++)
	  for (j=0 ; j< (*n1) ; j++)
	    xm[i+(*n2)*j]= XScale(x[i]);
      break;
    case 'g' :
    default:
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	for ( i=0 ; i < (*n2) ; i++)
	  for (j=0 ; j< (*n1) ; j++)
	    xm[i+(*n2)*j]= XLogScale(x[i+(*n2)*j]);
      else 
	for ( i=0 ; i < (*n2) ; i++)
	  for (j=0 ; j< (*n1) ; j++)
	    xm[i+(*n2)*j]= XScale(x[i+(*n2)*j]);
      break;
    }
}


int CheckxfParam(char *xf)
{
  if ( strlen(xf) < 3 ) 
    {
      sciprint("Error : first argument must be a string of length 3");
      return(1);
    }
  if ( xf[0] != 'g' && xf[0] != 'e' && xf[0] != 'o' ) 
    {
      sciprint("Error : wrong first character in string \"%s\"\n",xf);
      return(1) ;
    }
  if ( xf[1] != 'l' && xf[1] != 'n' ) 
    {
      sciprint("Error : wrong second character in string \"%s\"\n",xf);
      return(1) ;
    }
  if ( xf[2] != 'l' && xf[2] != 'n' ) 
    {
      sciprint("Error : wrong third character in string \"%s\"\n",xf);
      return(1) ;
    }
  return(0);
}

