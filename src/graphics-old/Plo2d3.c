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


static void Plo2d3RealToPixel (BCG *Xgc,int *n1, int *n2, double *x, double *y, int *xm, int *ym, char *xf);

/*--------------------------------------------------------------------
  nsp_plot2d_3(xf,x,y,n1,n2,style,strflag,legend,brect,aaint)

  used to plot only vertical bars form (x_i,0) to (x_i,y_i)
  the arguments are similar to those of plot2d 
  the only difference is that style must have positive values 
  which are considered as dash-styles 
--------------------------------------------------------------------------*/

int nsp_plot2d_3(BCG *Xgc,char *xf,double x[],double y[],int *n1,int *n2,int style[],char *strflag,
		const char *legend,double brect[],int aaint[])
{
  int n;
  int *xm,*ym;
  int j;
  /** Attention : 2*(*n2) **/
  int nn2=2*(*n2);
  if ( CheckxfParam(xf)== 1) return(0);

  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,xf,x,y,n1,n2,aaint,strflag,brect);

  /* Storing values if using the Record driver */
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Plot3(Xgc,xf,x,y,n1,n2,style,strflag,legend,brect,aaint);

  /* Allocation */
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
      Plo2d3RealToPixel(Xgc,n1,n2,x,y,xm,ym,xf);
      
      /** Draw Axis or only rectangle **/
    }
  axis_draw(Xgc,strflag);
  if ( n != 0 ) 
    {
      /** Drawing the curves **/
      frame_clip_on(Xgc);
      /** to get the default dash **/
      for ( j = 0 ; j < (*n1) ; j++)
	{
	  int lstyle,iflag=0;
	  /** style must be negative **/
	  lstyle = (style[j] < 0) ?  -style[j] : style[j];
	  Xgc->graphic_engine->drawsegments(Xgc,&xm[2*(*n2)*j],&ym[2*(*n2)*j],nn2,&lstyle,iflag);
	}
      frame_clip_off(Xgc);
      /** Drawing the Legends **/
      if ((int)strlen(strflag) >=1  && strflag[0] == '1')
	nsp_legends(Xgc,legend_ur,*n1,style,legend,"@"); 
    }
  return(0);
}


static void Plo2d3RealToPixel(BCG *Xgc,int *n1, int *n2, double *x, double *y, int *xm, int *ym, char *xf)
{
  int i,j;
  /** Computing y-values **/
  if ((int)strlen(xf) >= 3 && xf[2]=='l')	  
    {
      for ( i=0 ; i < (*n2) ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  {
	    ym[2*i+1+2*(*n2)*j]= YScale(0);
	    ym[2*i+2*(*n2)*j]= YLogScale(y[i+(*n2)*j]);
	  }
    }
  else 
    {
      for ( i=0 ; i < (*n2) ; i++)
	for (j=0 ; j< (*n1) ; j++)
	  {
	    ym[2*i+1+2*(*n2)*j]= YScale(0);
	    ym[2*i+2*(*n2)*j]= YScale(y[i+(*n2)*j]);
	  }
    }

  /** Computing x-values **/
  switch (xf[0])
  {
  case 'e' :
   /** No X-value given by the user **/
   if ((int)strlen(xf) >= 2 && xf[1]=='l')
     for (j=0 ; j< (*n1) ; j++)
       {
	 for ( i=0 ; i < (*n2) ; i++)
	   {
	     xm[2*i+2*(*n2)*j]= XLogScale(i+1.0);
	     xm[2*i+1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];

	   }
       }
   else 
     for (j=0 ; j< (*n1) ; j++)
       {
	 for ( i=0 ; i < (*n2) ; i++)
	   {
	     xm[2*i+2*(*n2)*j]= XScale((i+1.0));
	     xm[2*i+1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];

	   }
       }
   break ;
 case 'o' :
   if ((int)strlen(xf) >= 2 && xf[1]=='l')
     for (j=0 ; j< (*n1) ; j++)
       {
	 for ( i=0 ; i < (*n2) ; i++)
	   {

	     xm[2*i+2*(*n2)*j]= XLogScale(x[i]);
	     xm[2*i+1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
	   }
       }
   else 
     for (j=0 ; j< (*n1) ; j++)
       {
	 for ( i=0 ; i < (*n2) ; i++)
	   {
	     xm[2*i+2*(*n2)*j]= XScale(x[i]);
	     xm[2*i+1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
	     
	   }
       }
   break;
 case 'g' :
 default:
   if ((int)strlen(xf) >= 2 && xf[1]=='l')
     for (j=0 ; j< (*n1) ; j++)
       {
	 for ( i=0 ; i < (*n2) ; i++)
	   {
	     xm[2*i+2*(*n2)*j]= XLogScale(x[i+(*n2)*j]);
	     xm[2*i+1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];

	   }
       }
   else 
     for (j=0 ; j< (*n1) ; j++)
       {
	 for ( i=0 ; i < (*n2) ; i++)
	   {
	     xm[2*i+2*(*n2)*j]= XScale(x[i+(*n2)*j]);
	     xm[2*i+1+2*(*n2)*j]=xm[2*i+2*(*n2)*j];
	   }
       }
   break;
 }
}
