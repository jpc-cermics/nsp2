/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
/* #include "PloEch.h" */

static void GraySquare (BCG *Xgc,int *x,int *y,double *z,int n1,int n2);
static void GraySquare1 (BCG *Xgc,int *x,int *y,double *z, int n1,int n2);

/*------------------------------------------------------------
 * - z is a (n1,n2) matrix 
 * - x is a (1,n1) matrix 
 * - y is a (1,n2) matrix 
 * - x,y,z are stored as one dimensionnal array in C 
 *
 *  z : is the value of a function on the grid defined by x,y 
 *  on each rectangle the average value of z is computed 
 *  and [zmin,zmax] is linearly remapped to the [colormin,colormap]
 *  values of colors in the current colormap 
 *  the color associated to zmoy is used for filling a specific rectangle 
 *---------------------------------------------------------------*/

int C2F(xgray)(BCG *Xgc,double *x, double *y, double *z, int *n1, int *n2, char *strflag, double *brect, int *aaint, long int l1)
{
  int N = Max((*n1),(*n2));
  double xx[2],yy[2];
  int *xm,*ym, j, nn1=1,nn2=2;
  /** If Record is on **/

  xx[0]=Mini(x,*n1);xx[1]=Maxi(x,*n1);
  yy[0]=Mini(y,*n2);yy[1]=Maxi(y,*n2);
  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,"gnn",xx,yy,&nn1,&nn2,aaint,strflag,brect);
  /** Allocation **/

  if (nsp_gengine1.get_driver()=='R') store_Gray(Xgc,x,y,z,n1,n2,strflag,brect,aaint);


  xm = graphic_alloc(0,N,sizeof(int));
  ym = graphic_alloc(1,N,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return 0;
    }      

  /** Draw Axis or only rectangle **/
  axis_draw(Xgc,strflag);
  /** Drawing the curves **/
  frame_clip_on(Xgc);
  for ( j =0 ; j < (*n1) ; j++)	 xm[j]= XScale(x[j]);
  for ( j =0 ; j < (*n2) ; j++)	 ym[j]= YScale(y[j]); 
  GraySquare(Xgc,xm,ym,z,*n1,*n2);
  frame_clip_off(Xgc);
  nsp_gengine->drawrectangle(Xgc,Xgc->scales->WIRect1);
  return(0);
}

static void GraySquare_base(BCG *Xgc,int *x, int *y, double *z, int n1, int n2)
{
  double zmoy,zmax,zmin,zmaxmin;
  int i,j,whiteid,fill[1],cpat,xz[2];
  zmin=Mini(z,(n1)*(n2));
  zmax=Maxi(z,(n1)*(n2));
  zmaxmin=zmax-zmin;
  if (zmaxmin <= SMDOUBLE) zmaxmin=SMDOUBLE;
  whiteid = nsp_gengine->xget_last(Xgc);
  cpat = nsp_gengine->xget_pattern(Xgc);
  nsp_gengine->xget_windowdim(Xgc,xz,xz+1);
  for (i = 0 ; i < (n1)-1 ; i++)
    for (j = 0 ; j < (n2)-1 ; j++)
      {
	int w,h;
	zmoy=1/4.0*(z[i+n1*j]+z[i+n1*(j+1)]+z[i+1+n1*j]+z[i+1+n1*(j+1)]);
	fill[0]=1 + inint((whiteid-1)*(zmoy-zmin)/(zmaxmin));
	nsp_gengine->xset_pattern(Xgc,fill[0]);
        w=Abs(x[i+1]-x[i]);h=Abs(y[j+1]-y[j]);
	/* We don't trace rectangle which are totally out **/
	if ( w != 0 && h != 0 && x[i] < xz[0] && y[j+1] < xz[1] && x[i]+w > 0 && y[j+1]+h > 0 )
	  {
	    if ( Abs(x[i]) < int16max && Abs(y[j+1]) < int16max && w < uns16max && h < uns16max)
	      {
		int rect[]={x[i],y[j+1],w,h};
		nsp_gengine->fillrectangle(Xgc,rect);
	      }
	    else 
	      {
		/* fprintf(stderr,"Rectangle too large \n"); */
	      }
	  }
      }
   nsp_gengine->xset_pattern(Xgc,cpat);
}


static void GraySquare(BCG *Xgc,int *x, int *y, double *z, int n1, int n2)
{
  if ( nsp_gengine1.get_driver_id() == 0 ) 
    /** accelerated version for X11 or Windows **/
    nsp_gengine->fill_grid_rectangles(Xgc,x, y, z, n1, n2);
  else 
    GraySquare_base(Xgc,x, y, z, n1, n2);
}




/*-------------------------------------------------------
 *  z : of size n1*n2 
 *  the z value is interpreted as a color number inside the current colormap
 *  z[i,j] is used as the color of a square [i-0.5,i+0.5] [j-0.5,j+0.5]
 *-------------------------------------------------------*/

int C2F(xgray1)(BCG *Xgc,double *z, int *n1, int *n2, char *strflag, double *brect, int *aaint, long int l1)
{
  int N = Max((*n1+1),(*n2+1));
  double xx[2],yy[2];
  static int *xm,*ym,j, nn1=1,nn2=2;
  xx[0]=0.5;xx[1]= *n2+0.5;
  yy[0]=0.5;yy[1]= *n1+0.5;
  /** Boundaries of the frame **/
  update_frame_bounds(Xgc,0,"gnn",xx,yy,&nn1,&nn2,aaint,strflag,brect);

  /** If Record is on **/
  if (nsp_gengine1.get_driver()=='R') store_Gray1(Xgc,z,n1,n2,strflag,brect,aaint);

  /** Allocation **/
  xm = graphic_alloc(0,N,sizeof(int));
  ym = graphic_alloc(1,N,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return 0;
    }      

  /** Draw Axis or only rectangle **/
  axis_draw(Xgc,strflag);
  /** Drawing the curves **/
  frame_clip_on(Xgc);
  for ( j =0 ; j < (*n2+1) ; j++) xm[j]= XScale(j+0.5);
  for ( j =0 ; j < (*n1+1) ; j++) ym[j]= YScale(((*n1)-j+0.5));
  GraySquare1(Xgc,xm,ym,z,*n1+1,*n2+1);
  frame_clip_off(Xgc);
  nsp_gengine->drawrectangle(Xgc,Xgc->scales->WIRect1);
  return(0);
}

/*-------------------------------------------------------
 * like xgray1 : 
 * but xrect here give the rectangle in which the 
 * grayplot is to be drawn using the current scale
 -------------------------------------------------------*/

int C2F(xgray2)(BCG *Xgc,double *z, int *n1, int *n2, double *xrect)
{
  double xx[2],yy[2];
  int xx1[2],yy1[2];
  int *xm,*ym,  j;
  /** If Record is on **/
  if (nsp_gengine1.get_driver()=='R') store_Gray2(Xgc,z,n1,n2,xrect);
  xx[0]=xrect[0];xx[1]=xrect[2];
  yy[0]=xrect[1];yy[1]=xrect[3];
  /** Boundaries of the frame **/
  scale_f2i(Xgc,xx,yy,xx1,yy1,2);
  xm = graphic_alloc(0,*n2,sizeof(int));
  ym = graphic_alloc(1,*n1,sizeof(int));
  if ( xm == 0 || ym == 0 )
    {
      Scistring("Xgray: running out of memory\n");
      return 0;
    }
  for ( j =0 ; j < (*n2+1) ; j++)	 
    xm[j]= (int) (( xx1[1]*j + xx1[0]*((*n2)-j) )/((double) *n2));
  
  for ( j =0 ; j < (*n1+1) ; j++)	 
    ym[j]= (int) (( yy1[0]*j + yy1[1]*((*n1)-j) )/((double) *n1));
  frame_clip_on(Xgc);
  GraySquare1(Xgc,xm,ym,z,*n1+1,*n2+1);
  frame_clip_off(Xgc);
  return(0);
}


/*-------------------------------------------------------
 *  x : of size n1 gives the x-values of the grid 
 *  y : of size n2 gives the y-values of the grid 
 *  z : of size (n1-1)*(n2-1)  gives the f-values on the middle 
 *  of each rectangle. 
 *  z[i,j] is the value on the middle of rectangle 
 *        P1= x[i],y[j] x[i+1],y[j+1]
 *-------------------------------------------------------*/

static void GraySquare1_base(BCG *Xgc,int *x, int *y, double *z, int n1, int n2)
{
  int i,j,fill[1],cpat,xz[2];
  cpat = nsp_gengine->xget_pattern(Xgc);
  nsp_gengine->xget_windowdim(Xgc,xz,xz+1);
  for (i = 0 ; i < (n1)-1 ; i++)
    for (j = 0 ; j < (n2)-1 ; j++)
      {
	int w,h;
	fill[0]= z[i+(n1-1)*j];
	 nsp_gengine->xset_pattern(Xgc,fill[0]);
	w=Abs(x[j+1]-x[j]);
	h=Abs(y[i+1]-y[i]);
	/* We don't trace rectangle which are totally out **/
	if ( w != 0 && h != 0 && x[j] < xz[0] && y[i] < xz[1] && x[j]+w > 0 && y[i]+h > 0 )
	  if ( Abs(x[j]) < int16max && Abs(y[i+1]) < int16max && w < uns16max && h < uns16max)
	    {
	      int rect[]={x[j],y[i],w,h};
	      nsp_gengine->fillrectangle(Xgc,rect);
	    }
      }
   nsp_gengine->xset_pattern(Xgc,cpat);
}

static void GraySquare1(BCG *Xgc,int *x, int *y, double *z, int n1, int n2)
{
  if ( nsp_gengine1.get_driver_id() == 0 ) 
    /** accelerated version for X11 or Windows **/
    nsp_gengine->fill_grid_rectangles1(Xgc,x, y, z, n1, n2);
  else 
    GraySquare1_base(Xgc,x, y, z, n1, n2);
}







