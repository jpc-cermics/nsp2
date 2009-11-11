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

/*------------------------------------------------------------------------
 * the functions in this file are stored in the global struct Gengine nsp_gengine1
 * they are used when a graphic order is to be recorded or 
 * if it needs scale changes from pixel to double or both. 
 *--------------------------------------------------------------------------*/

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <gdk/gdk.h>
#include "nsp/graphics-new/Graphics.h" 
#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/object.h"
#include "nsp/gtk/gobject.h"
#include "new_graphics.h"

#define PERI_SCALE_PRIVATE 
#include "nsp/graphics-new/scale.h" 

static void GSciString (BCG *Xgc,int,int x,int y,char *StrMat,int *w,int *h);
static void Myalloc1 (int **xm,int n,int *err);
static void Myalloc (int **xm,int **ym, int n, int *err);
static void xstringb (BCG *Xgc, char *string,int x, int y, int w, int h);

static void xstringb_vert(BCG *Xgc,char *string, int x, int y, int w, int h);


Gengine1 nsp_gengine1={
  boundingbox_1,
  cleararea_1,
  displaynumbers_1,
  displaystring_1,
  displaystringa_1,
  draw_pixbuf_1,
  draw_pixbuf_from_file_1,
  drawarc_1,
  drawarcs_1,
  drawarrows_1,
  drawaxis_1,
  drawpolyline_1,
  drawpolyline_clip_1,
  drawpolylines_1,
  drawpolymark_1,
  drawrectangle_1,
  drawrectangles_1,
  drawsegments_1,
  fillarc_1,
  fillarcs_1,
  fillpolyline_1,
  fillpolylines_1,
  fillrectangle_1,
  initialize_gc_1,
  xclick_1,
  xclick_any_1,
  xgetmouse_1,
  xset_clip_1,
  xset_clipgrf_1,
  xset_clipping_p_1,
  xset_default_1,
  xset_font_size_1,
  xset_mark_size_1,
  xstringb_1,
};




/*------------------------------------------------
 * graphic context initialization 
 *------------------------------------------------*/

void nsp_initialize_gc( BCG *Xgc ) 
{ 
  int i;
  Xgc->graphic_engine->xset_unclip(Xgc);
  Xgc->graphic_engine->xset_font(Xgc,2,1);
  Xgc->graphic_engine->xset_mark(Xgc,0,0);
  /** Absolute coord mode  **/
  Xgc->graphic_engine->xset_absourel(Xgc,CoordModeOrigin);
  /* initialisation des pattern dash par defaut en n&b */
  Xgc->graphic_engine->xset_default_colormap(Xgc);
  getcolordef(&i); /* preferred color status */
  Xgc->graphic_engine->xset_alufunction1(Xgc,3);
  Xgc->graphic_engine->xset_usecolor(Xgc,i);
  Xgc->graphic_engine->xset_pattern(Xgc,1);
  Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->xset_hidden3d(Xgc,1);
  Xgc->graphic_engine->xset_thickness(Xgc,1);;
  Xgc->graphic_engine->xset_foreground(Xgc,Xgc->NumForeground+1);
  Xgc->graphic_engine->xset_background(Xgc,Xgc->NumForeground+2);
  Xgc->graphic_engine->xset_hidden3d(Xgc,4);
  Xgc->graphic_engine->xset_autoclear_def(Xgc) ;
  Xgc->graphic_engine->xset_fpf_def(Xgc) ;
}

static void initialize_gc_1(BCG *Xgc)
{
  nsp_initialize_gc(Xgc);
}


/**************************************************
 * Global values which are set at this level and 
 * not redirected to each driver
 **************************************************/

/*-----------------------------------------------------------------------------
 *  xset_1 
 *-----------------------------------------------------------------------------*/

static void xset_clipping_p_1(BCG *Xgc,double x,double y,double w,double h)
{
  int rect[4]={x,y,w,h};
  Xgc->graphic_engine->xset_clip(Xgc,rect);
}

static void xset_clipgrf_1(BCG *Xgc)
{
  frame_clip_on(Xgc);
}

static void xset_clip_1(BCG *Xgc,double x[])
{
  /* and clipping is special its args are floats **/
  int ix[4];
  scale_f2i(Xgc->scales,x,x+1,ix,ix+1,1);
  length_scale_f2i(Xgc->scales,x+2,x+3,ix+2,ix+3,1);
  Xgc->graphic_engine->xset_clip(Xgc,ix);
}

static void xset_default_1(BCG *Xgc) 
{
  nsp_initialize_gc(Xgc);
}

static void xset_font_size_1(BCG *Xgc,int val)
{
  int font[2];
  Xgc->graphic_engine->xget_font(Xgc,font);
  Xgc->graphic_engine->xset_font(Xgc,font[0],val);
}

static void xset_mark_size_1(BCG *Xgc,int val)
{
  int mark[2];
  Xgc->graphic_engine->xget_mark(Xgc,mark);
  mark[1]=val;
  Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark[1]);
}

static void drawarc_1(BCG *Xgc,double arc[])
{ 
  int iarc[6];
  rect2d_f2i(Xgc->scales,arc,iarc,4);
  iarc[4]=(int) arc[4];
  iarc[5]=(int) arc[5];
  Xgc->graphic_engine->drawarc(Xgc,iarc);
}

static void fillarcs_1(BCG *Xgc,double vects[],int fillvect[], int n)
{
  int *xm=NULL,err=0,n2;
  Myalloc1(&xm,6*n,&err);
  if (err  ==   1) return;
  ellipse2d(Xgc->scales,vects,xm,(n2=6*n,&n2),"f2i");
  Xgc->graphic_engine->fillarcs(Xgc,xm,fillvect,n);
}

static void drawarcs_1(BCG *Xgc,double vects[], int style[], int n)
{
  int *xm=NULL,err=0,n2;
  Myalloc1(&xm,6*n,&err);
  if (err  ==   1) return;
  ellipse2d(Xgc->scales,vects,xm,(n2=6*(n),&n2),"f2i");
  Xgc->graphic_engine->drawarcs(Xgc,xm,style,n);
}

static void fillpolyline_1(BCG *Xgc,double *vx, double *vy,int n,int closeflag)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,vx,vy,xm,ym,n);
  Xgc->graphic_engine->fillpolyline(Xgc,xm,ym,n,closeflag);
}


static void drawarrows_1(BCG *Xgc,double vx[],double vy[],int n,double as, int style[], int iflag)
{ 
  int *xm=NULL,*ym=NULL,err=0,ias,ias1;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,vx,vy,xm,ym,n);
  /* is as < 0 --> not set */
  if ( as < 0.0 ) 
    {
      int i;
      double Mnorm=0.0;
      for (i=0 ; i < n/2 ; i++)
	{ 
	  double dx,dy;
	  dx=( vx[2*i+1]-vx[2*i]);
	  dy=( vy[2*i+1]-vy[2*i]);
	  Mnorm += sqrt(dx*dx+dy*dy);
	}
      if ( n != 0) Mnorm /= (n/2);
      as = Mnorm/5.0;
    }
  /* we assume here that ias is given using the x scale */
  length_scale_f2i (Xgc->scales,&as,&as,&ias,&ias1,1);
  ias=10*ias;
  Xgc->graphic_engine->drawarrows(Xgc,xm,ym,n,ias,style,iflag);
}


static void drawaxis_1(BCG *Xgc,double *alpha, int *nsteps, double *initpoint, double *size)
{
  int initpoint1[2],alpha1;
  double size1[3];
  alpha1=inint( *alpha);
  axis2d(Xgc->scales,alpha,initpoint,size,initpoint1,size1);  
  Xgc->graphic_engine->drawaxis(Xgc,alpha1,nsteps,initpoint1,size1);
}


static void cleararea_1(BCG *Xgc,double *rect)
{
  GdkRectangle r;
  r.x = XDouble2Pixel(Xgc->scales,rect[0]);
  r.y = YDouble2Pixel(Xgc->scales,rect[1]);
  length_scale_f2i (Xgc->scales,&rect[2],&rect[3],&r.width,&r.height,1);
  Xgc->graphic_engine->cleararea(Xgc,&r);
}


static void xclick_1(BCG *Xgc,char *str,int *ibutton,int *imask, double *x, double *y, int iflag,int motion,int release,int key, int istr)
{ 
  int x1,yy1,n=1;

  Xgc->graphic_engine->xclick(Xgc,str,ibutton,imask,&x1,&yy1,iflag,motion,release,key,istr);
  scale_i2f(Xgc->scales,x,y,&x1,&yy1,n);
}

static void xclick_any_1(BCG *Xgc,char *str, int *ibutton,int *imask, double *x, double *y, int *iwin,int iflag,int motion,int release,int key,int istr)
{ 
  int x1,y1;
  Xgc->graphic_engine->xclick_any(Xgc,str,ibutton,imask,&x1,&y1,iwin,iflag,motion,release,key,istr);
  if (*ibutton>=0){
    BCG *Xgc_win =window_list_search(*iwin);
    scale_i2f(Xgc_win->scales,x,y,&x1,&y1,1);
  }
}

static void xgetmouse_1(BCG *Xgc,char *str, int *ibutton, int *imask,double *x, double *y, int iflag, int motion,int release,int key)
{ 
  int x1,yy1;
  Xgc->graphic_engine->xgetmouse(Xgc,str,ibutton,imask,&x1,&yy1,iflag,motion,release,key);
  scale_i2f(Xgc->scales,x,y,&x1,&yy1,1);
}

static void fillarc_1(BCG *Xgc, double arc[])
{ 
  int iarc[6],n2=4;
  rect2d_f2i(Xgc->scales,arc,iarc,n2);
  iarc[4]=(int) arc[4];
  iarc[5]=(int) arc[5];
  Xgc->graphic_engine->fillarc(Xgc,iarc);
}


static void fillrectangle_1(BCG *Xgc,double rect[])
{ 
  int irect[4],n2=4;
  rect2d_f2i(Xgc->scales,rect,irect,n2);
  Xgc->graphic_engine->fillrectangle(Xgc,irect);
}


static void drawpolyline_1(BCG *Xgc, double *vx, double *vy ,int n, int closeflag)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,vx,vy,xm,ym,n);
  Xgc->graphic_engine->drawpolyline(Xgc,xm,ym,n,closeflag);
}

/* XXXX*/
extern void nsp_set_clip_box(int xxleft, int xxright, int yybot, int yytop);

static void drawpolyline_clip_1(BCG *Xgc, double *vx, double *vy ,int n,double *clip_rect, int closeflag)
{
  int ix[4],cb[4];
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,vx,vy,xm,ym,n);
  /** and clipping is special its args are floats **/
  scale_f2i(Xgc->scales,clip_rect,clip_rect+1,ix,ix+1,1);
  length_scale_f2i(Xgc->scales,clip_rect+2,clip_rect+3,ix+2,ix+3,1);
  /* xxleft, int xxright, int yybot, int yytop)*/
  cb[0]=ix[0];cb[1]=ix[0]+ix[2];cb[2]=ix[1];cb[3]=ix[1]+ix[3];
  Xgc->graphic_engine->drawpolyline_clip(Xgc,xm,ym,n,cb,closeflag);
}

static int nsp_shade(BCG *Xgc,const int *polyx,const int *polyy,const int *fill, int polysize, int flag);

static void fillpolylines_1(BCG *Xgc, double *vx, double *vy, int *fillvect, int n, int p, int v1)
{
  int *xm=NULL,*ym=NULL,err=0,i;
  Myalloc(&xm,&ym,n*p,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,vx,vy,xm,ym,n*p);
  if (v1 == 2) {
    for (i=0 ; i< (n) ;i++) nsp_shade(Xgc,&xm[(p)*i],&ym[(p)*i],&fillvect[(p)*i],p,0);
  }
  else 
    Xgc->graphic_engine->fillpolylines(Xgc,xm,ym,fillvect,n,p);

  /* end of code modified by polpoth 11/7/2000 */

}


/*---------------------------------------------------------------------------------
 *This function sorts the vertices such that the color value is in decreasing order
 *---------------------------------------------------------------------------------*/

static int  triangleSort(const int *polyxin,const int *polyyin,const int *fillin, int *polyx, int *polyy, int *fill)
{ 
  int tmp,k;
  for (k=0;k<3;k++) {polyx[k]=polyxin[k]; polyy[k]=polyyin[k]; fill[k]=fillin[k];}
      
  if (fill[0]<fill[1]) {  
    tmp=fill[0]; fill[0]=fill[1]; fill[1]=tmp;
    tmp=polyx[0]; polyx[0]=polyx[1]; polyx[1]=tmp;
    tmp=polyy[0]; polyy[0]=polyy[1]; polyy[1]=tmp;
  }
  if (fill[0]<fill[2]) {  
    tmp=fill[0]; fill[0]=fill[2]; fill[2]=tmp;
    tmp=polyx[0]; polyx[0]=polyx[2]; polyx[2]=tmp;
    tmp=polyy[0]; polyy[0]=polyy[2]; polyy[2]=tmp;
  }
  if (fill[1]<fill[2]) {  
    tmp=fill[1]; fill[1]=fill[2]; fill[2]=tmp;
    tmp=polyx[1]; polyx[1]=polyx[2]; polyx[2]=tmp;
    tmp=polyy[1]; polyy[1]=polyy[2]; polyy[2]=tmp;
  }
  return 0;
}


/*-----------------------------------------------------------------------
 * This is the main shading function. When the polygon has 4 vertices, it
 * is splitted in two triangles and shade() is recursively called twice.
 * Author : mottelet 2000 
 * XXXX: remplacer les malloc par graphic_alloc pour uniformiser avec les autres 
 *       routines 
 *-----------------------------------------------------------------------*/

static int nsp_shade(BCG *Xgc,const int *polyx,const int *polyy,const int *fill, int polysize, int flag)
{
  int px[5],py[5],fil[4],is[3],ie[3],n[3];
  int npoly=1,k,col,cols,psize,i,s,e;
  int polyxs[4],polyys[4],fills[4],*x[3],*y[3];
  double dx,dy;

  if (polysize == 3) { /* The triangle case */
 
    triangleSort(polyx,polyy,fill,polyxs,polyys,fills);
  
    is[0]=0; ie[0]=1;
    is[1]=1; ie[1]=2;
    is[2]=0; ie[2]=2;
     
    /* Computation of coordinates of elementary polygons for each side */
     
    for(i=0;i<3;i++) {

      s=is[i];
      e=ie[i];
      n[i]=fills[s]-fills[e];

      if (n[i]) {
	
	x[i]=(int *)malloc((n[i]+2)*sizeof(int));
	y[i]=(int *)malloc((n[i]+2)*sizeof(int)); 
	if (x[i]==NULL || y[i]==NULL) {
	  Scistring("shade : malloc No more Place\n");
	  return 0;
	}
		
	dx=((double)(polyxs[e]-polyxs[s]))/(double)n[i];
	dy=((double)(polyys[e]-polyys[s]))/(double)n[i];

	x[i][0]=polyxs[s];
	y[i][0]=polyys[s];
	   
	for(k=0;k<n[i];k++) {
	  x[i][k+1]=linint((double)polyxs[s] + (0.5+k)*dx);
	  y[i][k+1]=linint((double)polyys[s] + (0.5+k)*dy);
	}
	   
	x[i][n[i]+1]=polyxs[e];
	y[i][n[i]+1]=polyys[e];
      }
    }
     
    /* Fill the whole triangle with color fill[1] if all colors are equal */
         
    if (!n[0] && !n[1]) {

      psize=3;
      col=fills[0];
      Xgc->graphic_engine->fillpolylines(Xgc,polyxs,polyys,(cols=-Abs(col),&cols),npoly,psize);
      return(0);
    }
     
    if (n[0]) {
      psize=4;
      col=fills[0];  
      for(i=0;i<=n[0];i++) {
	px[0]=x[2][i]; px[1]=x[0][i]; px[2]=x[0][i+1]; px[3]=x[2][i+1];
	py[0]=y[2][i]; py[1]=y[0][i]; py[2]=y[0][i+1]; py[3]=y[2][i+1];
	Xgc->graphic_engine->fillpolylines(Xgc,px,py,(cols=-Abs(col),&cols),npoly,psize);
	col--;
      }
      free(x[0]);
      free(y[0]);
    }
     
    if (n[1]) {
      psize=4;
      col=fills[1];
      for(i=0;i<=n[1];i++) {
	px[0]=x[2][n[0]+i]; px[1]=x[1][i]; px[2]=x[1][i+1]; px[3]=x[2][n[0]+i+1];
	py[0]=y[2][n[0]+i]; py[1]=y[1][i]; py[2]=y[1][i+1]; py[3]=y[2][n[0]+i+1];
	Xgc->graphic_engine->fillpolylines(Xgc,px,py,(cols=-Abs(col),&cols),npoly,psize);
	col--;
      }
      free(x[1]);
      free(y[1]);  
    }

    if (n[2]) {
      free(x[2]);
      free(y[2]);
    }

  }
   
  else { /* The 4 vertices case  */
     
    px[0]=polyx[0]; px[1]=polyx[1]; px[2]=polyx[2];
    py[0]=polyy[0]; py[1]=polyy[1]; py[2]=polyy[2];
    fil[0]=fill[0]; fil[1]=fill[1]; fil[2]=fill[2];
    nsp_shade(Xgc,px,py,fil,3,flag);
    px[0]=polyx[0]; px[1]=polyx[2]; px[2]=polyx[3];
    py[0]=polyy[0]; py[1]=polyy[2]; py[2]=polyy[3];
    fil[0]=fill[0]; fil[1]=fill[2]; fil[2]=fill[3];
    nsp_shade(Xgc,px,py,fil,3,flag);
  }
  return 0;
}     





static void drawpolymark_1(BCG *Xgc,double *vx, double *vy,int n)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,vx,vy,xm,ym,n);
  Xgc->graphic_engine->drawpolymark(Xgc,xm,ym,n);

}


static void displaynumbers_1(BCG *Xgc,double *x, double *y,int n, int flag,double *z, double *alpha)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,x,y,xm,ym,n);
  Xgc->graphic_engine->displaynumbers(Xgc,xm,ym,n,flag,z,alpha);
}


static void drawpolylines_1(BCG *Xgc,double *vx, double *vy, int *drawvect,int n, int p)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,(n)*(p),&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,vx,vy,xm,ym,n*p);
  Xgc->graphic_engine->drawpolylines(Xgc,xm,ym,drawvect,n,p);
}


static void drawrectangle_1(BCG *Xgc,double rect[])
{
  int xm[4],n2=4;
  rect2d_f2i(Xgc->scales,rect,xm,n2);
  Xgc->graphic_engine->drawrectangle(Xgc,xm);
}

static void drawrectangles_1(BCG *Xgc,double vects[],int fillvect[], int n)
{
  int *xm=NULL,err=0;
  Myalloc1(&xm,4*(n),&err);
  if (err  ==   1) return;
  rect2d_f2i(Xgc->scales,vects,xm,4*(n));
  Xgc->graphic_engine->drawrectangles(Xgc,xm,fillvect,n);
}

static void drawsegments_1(BCG *Xgc,double *vx, double *vy,int n, int *style, int iflag)
{ 
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc->scales,vx,vy,xm,ym,n);
  Xgc->graphic_engine->drawsegments(Xgc,xm,ym,n,style,iflag);
}


static void displaystring_1(BCG *Xgc,char *string,double x, double y,int flag, double angle)
{
  int w,h,ix1,iy1;;
  double xd1,yd1;
  xd1 = XDouble2Pixel_d(Xgc->scales,x);
  yd1 = YDouble2Pixel_d(Xgc->scales,y);
  Xgc->graphic_engine->xget_windowdim(Xgc,&w,&h);
  ix1 = (xd1 > int16max ) ? int16max :  ((xd1 < - int16max) ? - int16max : inint(xd1));
  iy1 = (yd1 > int16max ) ? int16max :  ((yd1 < - int16max) ? - int16max : inint(yd1));
  /* ignore points outside of window */
  if ( ix1 > w || iy1 > h ) return;
  Xgc->graphic_engine->displaystring(Xgc,string,ix1,iy1,flag,angle);
}


static void displaystringa_1(BCG *Xgc,char *string, int ipos)
{
  switch ( ipos )
    {
    case 1:
      xstringb(Xgc,string,Xgc->scales->WIRect1[0],Xgc->scales->WIRect1[1],Xgc->scales->WIRect1[2],
	       Xgc->scales->WIRect1[1] - Xgc->scales->wdim[1]*Xgc->scales->subwin_rect[1]);
      break;
    case 2:
      xstringb(Xgc,string,Xgc->scales->WIRect1[0],
	       Xgc->scales->wdim[1]*(Xgc->scales->subwin_rect[1]+Xgc->scales->subwin_rect[3]),
	       Xgc->scales->WIRect1[2],
	       (Xgc->scales->wdim[1]*(Xgc->scales->subwin_rect[1]+Xgc->scales->subwin_rect[3])
		- (Xgc->scales->WIRect1[1]+Xgc->scales->WIRect1[3]))*2.0/3.0);
      break;
    case 3:
      xstringb_vert(Xgc,string,Xgc->scales->wdim[0]*Xgc->scales->subwin_rect[0],
		    Xgc->scales->WIRect1[1]+Xgc->scales->WIRect1[3],
		    (Xgc->scales->WIRect1[0]-Xgc->scales->wdim[0]*Xgc->scales->subwin_rect[0]) /3.0,
		    Xgc->scales->WIRect1[3]);
      break;
    }
}



static void xstringb(BCG *Xgc,char *string, int x, int y, int w, int h)
{
  char *loc,*loc1;
  loc= (char *) MALLOC( (strlen(string)+1)*sizeof(char));
  if ( loc != 0)
    {
      int wmax=0,htot=0,x1=0,yy1=0,rect[4];
      strcpy(loc,string);
      loc1=strtok(loc,"@");
      while ( loc1 != ( char * ) 0) 
	{  
	  Xgc->graphic_engine->boundingbox(Xgc,loc1,x1,yy1,rect);
	  if ( rect[2] >= wmax ) wmax=rect[2];
	  htot += (int) (1.2*((double) rect[3]));
	  loc1=strtok((char *) 0,"@");
	}
      x1=x+ (w- wmax)/2;
      yy1=y - h + ( h - htot)/2 + rect[3];
      strcpy(loc,string);
      loc1=strtok(loc,"@");
      while ( loc1 != ( char * ) 0) 
	{  
	  double angle=0.0;
	  int flag=0;
	  Xgc->graphic_engine->displaystring(Xgc,loc1,x1,yy1,flag,angle);
	  yy1 += (int) (1.2*((double)rect[3]));
	  loc1=strtok((char *) 0,"@");
	}
      FREE(loc);
    }
  else
    {
      Scistring("xstring : No more Place  \n");
    }
}

/*
 * string are displayed vertically 
 * in the given box 
 */


static void xstringb_vert(BCG *Xgc,char *string, int x, int y, int w, int h)
{
  char *loc,*loc1;
  int count=0,flag=0;
  double angle=-90.0;
  loc= (char *) MALLOC( (strlen(string)+1)*sizeof(char));
  if ( loc != 0)
    {
      int wmax=0,hl=0,x1=0,y1=0,rect[4];
      strcpy(loc,string);
      loc1=strtok(loc,"@");
      while ( loc1 != ( char * ) 0) 
	{  
	  Xgc->graphic_engine->boundingbox(Xgc,loc1,x1,y1,rect);
	  if ( rect[2] >= wmax ) wmax=rect[2];
	  hl = Max(hl,rect[3]);
	  count++;
	  loc1=strtok((char *) 0,"@");
	}
      y1= y - (h - wmax)/2;
      x1= x + ( w - count*hl*1.5)/2.0 ; 
      strcpy(loc,string);
      loc1=strtok(loc,"@");
      while ( loc1 != ( char * ) 0) 
	{  
	  x1 += hl*(1.25);
	  Xgc->graphic_engine->displaystring(Xgc,loc1,x1,y1,flag,angle);
	  loc1=strtok((char *) 0,"@");
	}
      FREE(loc);
    }
  else
    {
      Scistring("xstring : No more Place  \n");
    }
}



static void boundingbox_1(BCG *Xgc,char *string, double x, double y, double *rect)
{ 
  int x1,yy1,rect1[4];
  x1 = XDouble2Pixel(Xgc->scales,x);
  yy1 = YDouble2Pixel(Xgc->scales,y);
  Xgc->graphic_engine->boundingbox(Xgc,string,x1,yy1,rect1);
  scale_i2f(Xgc->scales,rect,rect+1,rect1,rect1+1,1);
  length_scale_i2f(Xgc->scales,rect+2,rect+3,rect1+2,rect1+3,1);
}

/*-----------------------------------------------------------------------------
 * a string in a bounded box : with font size change to fit into the 
 * specified box (only works with driver which properly estimate string sizes)
 *-----------------------------------------------------------------------------*/

#define FONTMAXSIZE 6

static void xstringb_1(BCG *Xgc,char *str,int *fflag, double *xd, double *yd, double *wd, double *hd)
{
  int x,y,w,h,wbox,hbox,size;
  int fontid[2];
  x = XDouble2Pixel(Xgc->scales,*xd);
  y = YDouble2Pixel(Xgc->scales,*yd);
  length_scale_f2i(Xgc->scales,wd,hd,&wbox,&hbox,1);
  Xgc->graphic_engine->xget_font(Xgc,fontid);
  size = FONTMAXSIZE;
  w = wbox +1;
  if ( *fflag  ==  1 ) 
    {
      while ( (w > wbox || h > hbox) && size >=0  ) 
	{
	  size--;
	  Xgc->graphic_engine->xset_font(Xgc,fontid[0],size);
	  GSciString(Xgc,0,x,y,str,&w,&h);
	}
    }
  else 
    {
      GSciString(Xgc,0,x,y,str,&w,&h);
    }
  x = x +  (wbox - w)/2.0;
  y = y -  (hbox - h)/2.0; 
  GSciString(Xgc,1,x,y,str,&w,&h);
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1]);
}


/*
 * StrMat = 'xxxxZxxxxZxxx....' Z = \n 
 * find the enclosing rectangle for drawing 
 * the string StrMat 
 * and the string is Drawn if Dflag  == 1 ;
 */

static void GSciString(BCG *Xgc,int Dflag, int x, int y, char *StrMat, int *w, int *h)
{
  char *p = StrMat,*p1,*p2,*plast;
  int yi=y;
  int wc =0;
  p1 = plast = p+ strlen(p);
  while (1) 
    {
      int logrect[4];
      double angle=0.0;
      int flag=0;
      p2 =p1 ; *p1 = '\0';
      while ( p1 != p && *p1 != '\n' ) 
	p1--;
      if ( Dflag  ==  1) 
	Xgc->graphic_engine->displaystring(Xgc,( p1  ==  p ) ? p1 : p1 +1, x,yi,flag,angle);
      Xgc->graphic_engine->boundingbox(Xgc, ( p1  ==  p ) ? p1 : p1 +1, x,yi,logrect);
      if ( p2 != plast) 	*p2 = '\n';
      wc = Max( wc , logrect[2]);
      if ( p  ==  p1 ) 
	{
	  yi=yi- logrect[3];
	  break;
	}	
      else 
	{
	  yi=yi-1.2*logrect[3];
	}
    }
  *w = wc ;
  *h = y - yi;
}

/*-----------------------------------------------------------------------------
 * pixbuf 
 *-----------------------------------------------------------------------------*/

static void draw_pixbuf_1(BCG *Xgc,void *pix,int src_x,int src_y,double dest_x,
			  double dest_y,double w,double  h)
{ 
  GdkPixbuf *pixbuf=GDK_PIXBUF(((NspGObject *) pix)->obj);
  int idest_x,idest_y,iw,ih;
  scale_f2i(Xgc->scales,&dest_x,&dest_y,&idest_x,&idest_y,1);
  length_scale_f2i(Xgc->scales,&w,&h,&iw,&ih,1);
  Xgc->graphic_engine->draw_pixbuf(Xgc,pixbuf, src_x, src_y,idest_x,idest_y, iw, ih);
  
}

static void draw_pixbuf_from_file_1(BCG *Xgc,const char *fname,int src_x,int src_y,double dest_x,
				    double dest_y,double w,double  h)
{ 
  int idest_x,idest_y,iw,ih;
  scale_f2i(Xgc->scales,&dest_x,&dest_y,&idest_x,&idest_y,1);
  length_scale_f2i(Xgc->scales,&w,&h,&iw,&ih,1);
  Xgc->graphic_engine->draw_pixbuf_from_file(Xgc,fname, src_x, src_y,idest_x,idest_y, iw, ih);
  
}


/*
 * Utilities : Allocation 
 */

static void Myalloc(int **xm, int **ym, int n, int *err)
{
  if ( n != 0) 
    {
      *xm= graphic_alloc(6,n,sizeof(int));
      *ym= graphic_alloc(7,n,sizeof(int));
      if ( *xm  ==  0 || *ym  ==  0 )
	{
	  Scistring("malloc: Running out of memory\n");
	  *err=1;
	}
    }
}

static void Myalloc1(int **xm, int n, int *err)
{
  if ( n != 0) 
    {
      if (( *xm= graphic_alloc(6,n,sizeof(int)))   ==  0  )
	{
	  Scistring("malloc: Running out of memory\n");
	  *err=1;
	}
    }
}


