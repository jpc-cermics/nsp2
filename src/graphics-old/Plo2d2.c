/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include <string.h>
#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
/* #include "nsp/graphics/PloEch.h" */

static void Plo2d2RealToPixel (BCG *Xgc,int *n1, int *n2, double *x, double *y, int *xm, int *ym, char *xf);

/*--------------------------------------------------------------------
  nsp_plot2d_2(xf,x,y,n1,n2,style,strflag,legend,brect,aint)
  
  Same as plot2d1_ but the curves are plotted in a piece-wise linear 
  style 
  --------------------------------------------------------------------------*/

int nsp_plot2d_2(BCG *Xgc,char *xf,double x[],double y[],int *n1,int *n2,int style[],char *strflag,
		char *legend,double brect[],int aaint[])
{
  int n;
  int *xm,*ym;
  /** Attention : 2*(*n2) **/
  int nn2=2*(*n2);
  if ( CheckxfParam(xf)== 1) return(0);

  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,xf,x,y,n1,n2,aaint,strflag,brect);

  /* Storing values if using the Record driver */
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Plot2(Xgc,xf,x,y,n1,n2,style,strflag,legend,brect,aaint);

  /** Allocation **/
  n = (*n1)*nn2;
  if ( n != 0) 
    {
      xm = graphic_alloc(0,n,sizeof(int));
      ym = graphic_alloc(1,n,sizeof(int));
      if ( xm == 0 || ym == 0) 
	{
	  sciprint("Running out of memory \n");
	  return 0 ;
	}      
      /** Real to Pixel values **/
      Plo2d2RealToPixel(Xgc,n1,n2,x,y,xm,ym,xf);
    }
  /** Draw Axis or only rectangle **/
  axis_draw(Xgc,strflag);
  if ( n != 0 ) 
    {
      /** Drawing the curves **/
      frame_clip_on(Xgc);
      Xgc->graphic_engine->drawpolylines(Xgc,xm,ym,style,*n1,nn2);
      frame_clip_off(Xgc);
      /** Drawing the Legends **/
      if ((int)strlen(strflag) >=1  && strflag[0] == '1')
	nsp_legends(Xgc,style,n1,legend);
    }
  return(0);
}


static void Plo2d2RealToPixel(BCG *Xgc,int *n1, int *n2, double *x, double *y, int *xm, int *ym, char *xf)
{
  int i,j;
  /** Computing y-values **/
  if ((int)strlen(xf) >= 3 && xf[2]=='l')	  
    {
      for ( i=0 ; i < (*n2) ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  ym[2*i+1+2*(*n2)*j]= ym[2*i+2*(*n2)*j]= YLogScale(y[i+(*n2)*j]);
    }
  else 
    {
      for ( i=0 ; i < (*n2) ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  ym[2*i+1+2*(*n2)*j]= ym[2*i+2*(*n2)*j]= YScale(y[i+(*n2)*j]);
    }
  
  /** Computing x-values **/
  switch (xf[0])
    {
    case 'e' :
      /** No X-value given by the user **/
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	for (j=0 ; j< (*n1) ; j++)
	  {
	    for ( i=1 ; i < (*n2) ; i++)
	      {
		xm[2*i+2*(*n2)*j]= XLogScale(i+1.0);
		xm[2*i-1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
		
	      }
	    xm[2*(*n2)*j]= XScale(0);
	    xm[2*(*n2)-1+ 2*(*n2)*j]= xm[2*(*n2-1)+ 2*(*n2)*j];
	  }
      else 
	for (j=0 ; j< (*n1) ; j++)
	  {
	    for ( i=1 ; i < (*n2) ; i++)
	      {
		xm[2*i+2*(*n2)*j]= XScale((i+1.0));
		xm[2*i-1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
		
	      }
	    xm[2*(*n2)*j]= XScale(1.0);
	    xm[2*(*n2)-1+ 2*(*n2)*j]=	 xm[2*(*n2-1)+ 2*(*n2)*j];
	  }
      break ;
    case 'o' :
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	for (j=0 ; j< (*n1) ; j++)
	  {
	    for ( i=1 ; i < (*n2) ; i++)
	      {
		
		xm[2*i+2*(*n2)*j]= XLogScale(x[i]);
		xm[2*i-1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
	      }
	    xm[2*(*n2)*j]= XLogScale(x[0]);
	    xm[2*(*n2)-1+ 2*(*n2)*j]= xm[2*(*n2-1)+ 2*(*n2)*j];
	  }
      else 
	for (j=0 ; j< (*n1) ; j++)
	  {
	    for ( i=1 ; i < (*n2) ; i++)
	      {
		xm[2*i+2*(*n2)*j]= XScale(x[i]);
		xm[2*i-1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
		
	      }
	    xm[2*(*n2)*j]= XScale(x[0]);
	    xm[2*(*n2)-1+ 2*(*n2)*j]= xm[2*(*n2-1)+ 2*(*n2)*j];
	  }
      break;
    case 'g' :
    default:
      if ((int)strlen(xf) >= 2 && xf[1]=='l')
	for (j=0 ; j< (*n1) ; j++)
	  {
	    for ( i=1 ; i < (*n2) ; i++)
	      {
		xm[2*i+2*(*n2)*j]= XLogScale(x[i+(*n2)*j]);
		xm[2*i-1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
		
	      }
	    xm[2*(*n2)*j]= XLogScale(x[(*n2)*j]);
	    xm[2*(*n2)-1+ 2*(*n2)*j]= xm[2*(*n2-1)+ 2*(*n2)*j];
	  }
      else 
	for (j=0 ; j< (*n1) ; j++)
	  {
	    for ( i=1 ; i < (*n2) ; i++)
	      {
		xm[2*i+2*(*n2)*j]= XScale(x[i+(*n2)*j]);
		xm[2*i-1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
		
	      }
	    xm[2*(*n2)*j]= XScale(x[(*n2)*j]);
	    xm[2*(*n2)-1+ 2*(*n2)*j]= xm[2*(*n2-1)+ 2*(*n2)*j];
	  }
      break;
    }
}
