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
 *   Graphic library for 2D and 3D plotting 
 *   Copyright (C) 1998-2001 Chancelier Jean-Philippe 
 *   jpc@cereve.enpc.fr 
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
#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/object.h"
#include "nsp/gtk/gobject.h"
#include "nsp/graphics/Graphics.h" 
#include "new_graphics.h"

extern Gengine *nsp_gengine ; /* XXXXX */

static void GSciString (BCG *Xgc,int,int x,int y,char *StrMat,int *w,int *h);
static void Myalloc1 (int **xm,int n,int *err);
static void Myalloc (int **xm,int **ym, int n, int *err);
static void xstringb (BCG *Xgc, char *string,int x, int y, int w, int h);

static void xstringb_vert(BCG *Xgc,char *string, int x, int y, int w, int h);

static  driver_s_drawarc drawarc_1;
static  driver_s_fillarcs fillarcs_1;
static  driver_s_drawarcs drawarcs_1;
static  driver_s_fillpolyline fillpolyline_1;
static  driver_s_drawarrows drawarrows_1;
static  driver_s_drawaxis drawaxis_1;
static  driver_s_cleararea cleararea_1;
static  driver_s_xclick xclick_1;
static  driver_s_xclick_any xclick_any_1;
static  driver_s_xgetmouse xgetmouse_1;
static  driver_s_fillarc fillarc_1;
static  driver_s_fillrectangle fillrectangle_1;
static  driver_s_drawpolyline drawpolyline_1;
static  driver_s_drawpolyline_clip drawpolyline_clip_1;
static  driver_s_fillpolylines fillpolylines_1;
static  driver_s_drawpolymark drawpolymark_1;
static  driver_s_displaynumbers displaynumbers_1;
static  driver_s_drawpolylines drawpolylines_1;
static  driver_s_drawrectangle drawrectangle_1;
static  driver_s_drawrectangles drawrectangles_1;
static  driver_s_drawsegments drawsegments_1;
static  driver_s_displaystring displaystring_1;
static  driver_s_displaystringa displaystringa_1;
static  driver_s_boundingbox boundingbox_1;
static  driver_s_xstringb xstringb_1;
static  driver_s_xset_clipping_p xset_clipping_p_1;
static  driver_s_xset_clipgrf xset_clipgrf_1;
static  driver_s_xset_alufunction1 xset_alufunction1_1;
static  driver_s_xset_background xset_background_1;
static  driver_s_xset_unclip xset_unclip_1;
static  driver_s_xset_test xset_test_1;
static  driver_s_xset_clip xset_clip_1;
static  driver_s_xset_pattern xset_pattern_1;
static  driver_s_xset_colormap xset_colormap_1;
static  driver_s_xset_default_colormap xset_default_colormap_1;
static  driver_s_xset_default xset_default_1;
static  driver_s_xset_font_size xset_font_size_1;
static  driver_s_xset_font xset_font_1;
static  driver_s_xset_foreground xset_foreground_1;
static  driver_s_xset_hidden3d xset_hidden3d_1;
static  driver_s_xset_absourel xset_absourel_1;
static  driver_s_xset_dash xset_dash_1;
static  driver_s_xset_mark_size xset_mark_size_1;
static  driver_s_xset_mark xset_mark_1;
static  driver_s_xset_pixmapOn xset_pixmapOn_1;
static  driver_s_xset_thickness xset_thickness_1;
static  driver_s_xset_usecolor xset_usecolor_1;
static  driver_s_xset_viewport xset_viewport_1;
static  driver_s_xset_windowdim xset_windowdim_1;
static  driver_s_xset_popupdim xset_popupdim_1;
static  driver_s_xset_windowpos xset_windowpos_1;
static  driver_s_xset_wresize xset_wresize_1;
static  driver_s_xset_autoclear xset_autoclear_1;
static  driver_s_xset_autoclear_def xset_autoclear_def_1;
static  driver_s_xset_fpf xset_fpf_1;
static  driver_s_xset_fpf_def xset_fpf_def_1;
static  driver_s_xset_show xset_show_1;
static  driver_s_xset_pixmapclear xset_pixmapclear_1;
static  driver_s_initialize_gc initialize_gc_1;
static  driver_s_draw_pixbuf draw_pixbuf_1;
static  driver_s_draw_pixbuf_from_file draw_pixbuf_from_file_1;


Gengine1 nsp_gengine1={
  drawarc_1,
  fillarcs_1,
  drawarcs_1,
  fillpolyline_1,
  drawarrows_1,
  drawaxis_1,
  cleararea_1,
  xclick_1,
  xclick_any_1,
  xgetmouse_1,
  fillarc_1,
  fillrectangle_1,
  drawpolyline_1,
  drawpolyline_clip_1,
  fillpolylines_1,
  drawpolymark_1,
  displaynumbers_1,
  drawpolylines_1,
  drawrectangle_1,
  drawrectangles_1,
  drawsegments_1,
  displaystring_1,
  displaystringa_1,
  boundingbox_1,
  xstringb_1,
  xset_clipping_p_1,
  xset_clipgrf_1,
  xset_alufunction1_1,
  xset_background_1,
  xset_unclip_1,
  xset_test_1,
  xset_clip_1,
  xset_pattern_1,
  xset_colormap_1,
  xset_default_colormap_1,
  xset_default_1,
  xset_font_size_1,
  xset_font_1,
  xset_foreground_1,
  xset_hidden3d_1,
  xset_absourel_1,
  xset_dash_1,
  xset_mark_size_1,
  xset_mark_1,
  xset_pixmapOn_1,
  xset_thickness_1,
  xset_usecolor_1,
  xset_viewport_1,
  xset_windowdim_1,
  xset_popupdim_1,
  xset_windowpos_1,
  xset_wresize_1,
  xset_autoclear_1,
  xset_autoclear_def_1,
  xset_fpf_1,
  xset_fpf_def_1,
  xset_show_1,
  xset_pixmapclear_1,
  initialize_gc_1,
  draw_pixbuf_1,
  draw_pixbuf_from_file_1
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
  if (Xgc->record_flag == TRUE)  store_initialize_gc(Xgc);
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
  if (Xgc->record_flag == TRUE)  store_clipping_p(Xgc,x,y,w,h);
  Xgc->graphic_engine->xset_clip(Xgc,rect);
}

static void xset_clipgrf_1(BCG *Xgc)
{
  if (Xgc->record_flag == TRUE)  store_clipgrf(Xgc);
  frame_clip_on(Xgc);
}

static void xset_unclip_1(BCG *Xgc)
{
  if (Xgc->record_flag == TRUE)  store_unclip(Xgc);
  Xgc->graphic_engine->xset_unclip(Xgc);
}

static void xset_test_1(BCG *Xgc)
{
  if (Xgc->record_flag == TRUE)  store_test(Xgc);
  Xgc->graphic_engine->xset_test(Xgc);
}


static void xset_clip_1(BCG *Xgc,double x[])
{
  /* and clipping is special its args are floats **/
  int ix[4];
  scale_f2i(Xgc,x,x+1,ix,ix+1,1);
  length_scale_f2i(Xgc,x+2,x+3,ix+2,ix+3,1);
  if (Xgc->record_flag == TRUE) store_clip(Xgc,x);
  Xgc->graphic_engine->xset_clip(Xgc,ix);
}

static void xset_alufunction1_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_alufunction1(Xgc,val);
  Xgc->graphic_engine->xset_alufunction1(Xgc,val);
}

static void xset_background_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_background(Xgc,val);
  Xgc->graphic_engine->xset_background(Xgc,val);
}


static void xset_pattern_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_pattern(Xgc,val);
  Xgc->graphic_engine->xset_pattern(Xgc,val);
}

static void xset_colormap_1(BCG *Xgc,int m, double val[])
{
  if (Xgc->record_flag == TRUE) store_colormap(Xgc,m,3,val);
  Xgc->graphic_engine->xset_colormap(Xgc,m,3,val);
}

static void xset_default_colormap_1(BCG *Xgc)
{
  if (Xgc->record_flag == TRUE) store_default_colormap(Xgc);
  Xgc->graphic_engine->xset_default_colormap(Xgc);
}

/* pas clair XXXX */

static void xset_default_1(BCG *Xgc) 
{
  /* no record XXX A FAIRE */
  nsp_initialize_gc(Xgc);
}

static void xset_font_size_1(BCG *Xgc,int val)
{
  int font[2];
  if (Xgc->record_flag == TRUE)  store_font_size(Xgc,val);
  Xgc->graphic_engine->xget_font(Xgc,font);
  Xgc->graphic_engine->xset_font(Xgc,font[0],val);
}

static void xset_font_1(BCG *Xgc,int val,int val1)
{
  if (Xgc->record_flag == TRUE)  store_font(Xgc,val,val1);
  Xgc->graphic_engine->xset_font(Xgc,val,val1);
}

static void xset_foreground_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_foreground(Xgc,val);
  Xgc->graphic_engine->xset_foreground(Xgc,val);
}

static void xset_hidden3d_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_hidden3d(Xgc,val);
  Xgc->graphic_engine->xset_hidden3d(Xgc,val);
}

static void xset_absourel_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_absourel(Xgc,val);
  Xgc->graphic_engine->xset_absourel(Xgc,val);
}

static void xset_dash_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_dash(Xgc,val);
  Xgc->graphic_engine->xset_dash(Xgc,val);
}

static void xset_mark_size_1(BCG *Xgc,int val)
{
  int mark[2];
  if (Xgc->record_flag == TRUE)  store_mark_size(Xgc,val);
  Xgc->graphic_engine->xget_mark(Xgc,mark);
  mark[1]=val;
  Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark[1]);
}

static void xset_mark_1(BCG *Xgc,int val,int val1)
{
  if (Xgc->record_flag == TRUE)  store_mark(Xgc,val,val1);
  Xgc->graphic_engine->xset_mark(Xgc,val,val1);
}

static void xset_pixmapOn_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_pixmapOn(Xgc,val);
  Xgc->graphic_engine->xset_pixmapOn(Xgc,val);
}

static void xset_thickness_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_thickness(Xgc,val);
  Xgc->graphic_engine->xset_thickness(Xgc,val);
}

static void xset_usecolor_1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_usecolor(Xgc,val);
  Xgc->graphic_engine->xset_usecolor(Xgc,val);
}

static void xset_viewport_1(BCG *Xgc,int val,int val1)
{
  Xgc->graphic_engine->xset_viewport(Xgc,val,val1);
}

static void xset_windowdim_1(BCG *Xgc,int val,int val1)
{
  Xgc->graphic_engine->xset_windowdim(Xgc,val,val1);
}

static void xset_popupdim_1(BCG *Xgc,int val,int val1)
{
  Xgc->graphic_engine->xset_popupdim(Xgc,val,val1);
}

static void xset_windowpos_1(BCG *Xgc,int val,int val1)
{
  Xgc->graphic_engine->xset_windowpos(Xgc,val,val1);
}

static void xset_wresize_1(BCG *Xgc,int val)
{
  Xgc->graphic_engine->xset_wresize(Xgc,val);
}

static void xset_show_1(BCG *Xgc)
{
  /* need to store ? */
  if (Xgc->record_flag == TRUE)  store_show(Xgc);
  Xgc->graphic_engine->xset_show(Xgc);
}

static void xset_pixmapclear_1(BCG *Xgc)
{
  /* need to store ? */
  if (Xgc->record_flag == TRUE)  store_pixmapclear(Xgc);
  Xgc->graphic_engine->xset_pixmapclear(Xgc);
}


static void xset_autoclear_1(BCG *Xgc,int val)
{
  Xgc->graphic_engine->xset_autoclear(Xgc,val);
}

static void xset_autoclear_def_1(BCG *Xgc)
{
  Xgc->graphic_engine->xset_autoclear_def(Xgc);
}


static void xset_fpf_1(BCG *Xgc,char *val)
{
  if (Xgc->record_flag == TRUE)  store_fpf(Xgc,val);
  Xgc->graphic_engine->xset_fpf(Xgc,val);
}


static void xset_fpf_def_1(BCG *Xgc)
{
  if (Xgc->record_flag == TRUE)  store_fpf_def(Xgc);
  Xgc->graphic_engine->xset_fpf_def(Xgc);
}

/*-----------------------------------------------------------------------------
 *  drawarc_1
 *-----------------------------------------------------------------------------*/

static void drawarc_1(BCG *Xgc,double arc[])
{ 
  int iarc[6];
  rect2d_f2i(Xgc,arc,iarc,4);
  iarc[4]=(int) arc[4];
  iarc[5]=(int) arc[5];
  if (Xgc->record_flag == TRUE) store_drawarc_1(Xgc,arc);
  Xgc->graphic_engine->drawarc(Xgc,iarc);
}

/*-----------------------------------------------------------------------------
 * 
 *-----------------------------------------------------------------------------*/

static void fillarcs_1(BCG *Xgc,double vects[],int fillvect[], int n)
{
  int *xm=NULL,err=0,n2;
  Myalloc1(&xm,6*n,&err);
  if (err  ==   1) return;
  ellipse2d(Xgc,vects,xm,(n2=6*n,&n2),"f2i");
  if (Xgc->record_flag == TRUE) store_fillarcs_1(Xgc,vects,fillvect,n);
  Xgc->graphic_engine->fillarcs(Xgc,xm,fillvect,n);
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static void drawarcs_1(BCG *Xgc,double vects[], int style[], int n)
{
  int *xm=NULL,err=0,n2;
  Myalloc1(&xm,6*n,&err);
  if (err  ==   1) return;
  ellipse2d(Xgc,vects,xm,(n2=6*(n),&n2),"f2i");
  if (Xgc->record_flag == TRUE) store_drawarcs_1(Xgc,vects,style,n);
  Xgc->graphic_engine->drawarcs(Xgc,xm,style,n);
}
/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

static void fillpolyline_1(BCG *Xgc,double *vx, double *vy,int n,int closeflag)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n);
  if (Xgc->record_flag == TRUE) store_fillpolyline_1(Xgc,vx,vy,n,closeflag);
  Xgc->graphic_engine->fillpolyline(Xgc,xm,ym,n,closeflag);
}

/*-----------------------------------------------------------------------------
 *  arrows
 *-----------------------------------------------------------------------------*/

static void drawarrows_1(BCG *Xgc,double vx[],double vy[],int n,double as, int style[], int iflag)
{ 
  int *xm=NULL,*ym=NULL,err=0,ias,ias1;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n);
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
  length_scale_f2i (Xgc,&as,&as,&ias,&ias1,1);
  if (Xgc->record_flag == TRUE)  store_drawarrows_1(Xgc,vx,vy,n,as,style,iflag);
  ias=10*ias;
  Xgc->graphic_engine->drawarrows(Xgc,xm,ym,n,ias,style,iflag);
}

/*-----------------------------------------------------------------------------
 * axis 
 *-----------------------------------------------------------------------------*/

static void drawaxis_1(BCG *Xgc,double *alpha, int *nsteps, double *initpoint, double *size)
{
  int initpoint1[2],alpha1;
  double size1[3];
  alpha1=inint( *alpha);
  axis2d(Xgc,alpha,initpoint,size,initpoint1,size1);  
  if (Xgc->record_flag == TRUE) store_drawaxis_1(Xgc,alpha,nsteps,initpoint,size);
  Xgc->graphic_engine->drawaxis(Xgc,alpha1,nsteps,initpoint1,size1);
}
/*-----------------------------------------------------------------------------
 *  cleararea
 *-----------------------------------------------------------------------------*/

static void cleararea_1(BCG *Xgc,double x, double y, double w, double h)
{
  int x1,yy1,w1,h1;
  x1 = XDouble2Pixel(x);
  yy1 = YDouble2Pixel(y);
  length_scale_f2i (Xgc,&w,&h,&w1,&h1,1);
  if (Xgc->record_flag == TRUE) store_cleararea_1(Xgc,x,y,w,h);
  Xgc->graphic_engine->cleararea(Xgc,x1,yy1,w1,h1);
}
/*-----------------------------------------------------------------------------
 * click 
 *-----------------------------------------------------------------------------*/

static void xclick_1(BCG *Xgc,char *str,int *ibutton,int *imask, double *x, double *y, int iflag,int motion,int release,int key, int istr)
{ 
  int x1,yy1,n=1;
  Xgc->graphic_engine->xclick(Xgc,str,ibutton,imask,&x1,&yy1,iflag,motion,release,key,istr);
  scale_i2f(Xgc,x,y,&x1,&yy1,n);
}
/*-----------------------------------------------------------------------------
 *  click_any
 *-----------------------------------------------------------------------------*/

static void xclick_any_1(BCG *Xgc,char *str, int *ibutton,int *imask, double *x, double *y, int *iwin,int iflag,int motion,int release,int key,int istr)
{ 
  int x1,y1;
  Xgc->graphic_engine->xclick_any(Xgc,str,ibutton,imask,&x1,&y1,iwin,iflag,motion,release,key,istr);
  if (*ibutton>=0){
    BCG *Xgc_win =window_list_search(*iwin);
    scale_i2f(Xgc_win,x,y,&x1,&y1,1);
  }
}

/*-----------------------------------------------------------------------------
 *   xgetmouse
 *-----------------------------------------------------------------------------*/

static void xgetmouse_1(BCG *Xgc,char *str, int *ibutton, int *imask,double *x, double *y, int iflag, int motion,int release,int key)
{ 
  int x1,yy1;
  Xgc->graphic_engine->xgetmouse(Xgc,str,ibutton,imask,&x1,&yy1,iflag,motion,release,key);
  scale_i2f(Xgc,x,y,&x1,&yy1,1);
}

/*-----------------------------------------------------------------------------
 *   fillarc
 *-----------------------------------------------------------------------------*/

static void fillarc_1(BCG *Xgc, double arc[])
{ 
  int iarc[6],n2=4;
  rect2d_f2i(Xgc,arc,iarc,n2);
  iarc[4]=(int) arc[4];
  iarc[5]=(int) arc[5];
  if (Xgc->record_flag == TRUE) store_fillarc_1(Xgc,arc);
  Xgc->graphic_engine->fillarc(Xgc,iarc);
}

/*-----------------------------------------------------------------------------
 *  fillrectangle
 *-----------------------------------------------------------------------------*/

static void fillrectangle_1(BCG *Xgc,double rect[])
{ 
  int irect[4],n2=4;
  rect2d_f2i(Xgc,rect,irect,n2);
  if (Xgc->record_flag == TRUE)  store_fillrectangle_1(Xgc,rect);
  Xgc->graphic_engine->fillrectangle(Xgc,irect);
}


/*-----------------------------------------------------------------------------
 *  drawpolyline
 *-----------------------------------------------------------------------------*/

static void drawpolyline_1(BCG *Xgc, double *vx, double *vy ,int n, int closeflag)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n);
  if (Xgc->record_flag == TRUE)  store_drawpolyline_1(Xgc,vx,vy,n,closeflag);
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
  scale_f2i(Xgc,vx,vy,xm,ym,n);
  /** and clipping is special its args are floats **/
  scale_f2i(Xgc,clip_rect,clip_rect+1,ix,ix+1,1);
  length_scale_f2i(Xgc,clip_rect+2,clip_rect+3,ix+2,ix+3,1);
  /* 
     if (Xgc->record_flag == TRUE)  store_drawpolyline_1(Xgc,vx,vy,n,closeflag);
  */
  /* xxleft, int xxright, int yybot, int yytop)*/
  cb[0]=ix[0];cb[1]=ix[0]+ix[2];cb[2]=ix[1];cb[3]=ix[1]+ix[3];
  Xgc->graphic_engine->drawpolyline_clip(Xgc,xm,ym,n,cb,closeflag);
}

/*-----------------------------------------------------------------------------
 *  fillpolylines
 *  if v1 == 2 then interpolated shading is used 
 *  and fillvect is in that case of dimension n*p 
 *-----------------------------------------------------------------------------*/

static void fillpolylines_1(BCG *Xgc, double *vx, double *vy, int *fillvect, int n, int p, int v1)
{
  int *xm=NULL,*ym=NULL,err=0,i;
  Myalloc(&xm,&ym,n*p,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n*p);

  if (Xgc->record_flag == TRUE) store_fillpolylines_1(Xgc,vx,vy,fillvect,n,p,v1);

  if (v1 == 2) {
    for (i=0 ; i< (n) ;i++) nsp_shade(Xgc,&xm[(p)*i],&ym[(p)*i],&fillvect[(p)*i],p,0);
  }
  else 
    Xgc->graphic_engine->fillpolylines(Xgc,xm,ym,fillvect,n,p);

  /* end of code modified by polpoth 11/7/2000 */

}
/*-----------------------------------------------------------------------------
 *  drawpolymark
 *-----------------------------------------------------------------------------*/

static void drawpolymark_1(BCG *Xgc,double *vx, double *vy,int n)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n);
  if (Xgc->record_flag == TRUE)  store_drawpolymark_1(Xgc,vx,vy,n);
  Xgc->graphic_engine->drawpolymark(Xgc,xm,ym,n);

}

/*-----------------------------------------------------------------------------
 *  displaynumbers
 *-----------------------------------------------------------------------------*/

static void displaynumbers_1(BCG *Xgc,double *x, double *y,int n, int flag,double *z, double *alpha)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,x,y,xm,ym,n);
  if (Xgc->record_flag == TRUE) 
    store_displaynumbers_1(Xgc,x,y,n,flag,z,alpha);
  Xgc->graphic_engine->displaynumbers(Xgc,xm,ym,n,flag,z,alpha);
}

/*-----------------------------------------------------------------------------
 *   drawpolylines
 *-----------------------------------------------------------------------------*/

static void drawpolylines_1(BCG *Xgc,double *vx, double *vy, int *drawvect,int n, int p)
{
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,(n)*(p),&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n*p);
  if (Xgc->record_flag == TRUE) 
    store_drawpolylines_1(Xgc,vx,vy,drawvect,n,p);
  Xgc->graphic_engine->drawpolylines(Xgc,xm,ym,drawvect,n,p);
}
/*-----------------------------------------------------------------------------
 *   drawrectangle
 *-----------------------------------------------------------------------------*/

static void drawrectangle_1(BCG *Xgc,double rect[])
{
  int xm[4],n2=4;
  rect2d_f2i(Xgc,rect,xm,n2);
  if (Xgc->record_flag == TRUE) store_drawrectangle_1(Xgc,rect);
  Xgc->graphic_engine->drawrectangle(Xgc,xm);
}
/*-----------------------------------------------------------------------------
 *   drawrectangles
 *-----------------------------------------------------------------------------*/

static void drawrectangles_1(BCG *Xgc,double vects[],int fillvect[], int n)
{
  int *xm=NULL,err=0;
  Myalloc1(&xm,4*(n),&err);
  if (err  ==   1) return;
  rect2d_f2i(Xgc,vects,xm,4*(n));
  if (Xgc->record_flag == TRUE) 
    store_drawrectangles_1(Xgc,vects,fillvect,n);
  Xgc->graphic_engine->drawrectangles(Xgc,xm,fillvect,n);
}

/*-----------------------------------------------------------------------------
 *  drawsegments
 *-----------------------------------------------------------------------------*/

static void drawsegments_1(BCG *Xgc,double *vx, double *vy,int n, int *style, int iflag)
{ 
  int *xm=NULL,*ym=NULL,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n);
  if (Xgc->record_flag == TRUE) 
    store_drawsegments_1(Xgc,vx,vy,n,style,iflag);
  Xgc->graphic_engine->drawsegments(Xgc,xm,ym,n,style,iflag);
}
/*-----------------------------------------------------------------------------
 *  displaystring
 *-----------------------------------------------------------------------------*/

static void displaystring_1(BCG *Xgc,char *string,double x, double y,int flag, double angle)
{
  int w,h,ix1,iy1;;
  double xd1,yd1;
  xd1 = XDouble2Pixel_d(x);
  yd1 = YDouble2Pixel_d(y);
  if (Xgc->record_flag == TRUE) 
    store_displaystring_1(Xgc,string,x,y,flag,angle);
  Xgc->graphic_engine->xget_windowdim(Xgc,&w,&h);
  ix1 = (xd1 > int16max ) ? int16max :  ((xd1 < - int16max) ? - int16max : inint(xd1));
  iy1 = (yd1 > int16max ) ? int16max :  ((yd1 < - int16max) ? - int16max : inint(yd1));
  /* ignore points outside of window */
  if ( ix1 > w || iy1 > h ) return;
  Xgc->graphic_engine->displaystring(Xgc,string,ix1,iy1,flag,angle);
}

/*-----------------------------------------------------------------------------
 *  displaystringa
 *-----------------------------------------------------------------------------*/

static void displaystringa_1(BCG *Xgc,char *string, int ipos)
{
  if (Xgc->record_flag == TRUE) 
    store_displaystringa_1(Xgc,string,ipos);
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


/*-----------------------------------------------------------------------------
 * display a set of lines coded with 'line1@line2@.....@'
 * centred in the rectangle [x,y,w=wide,h=height] (x,y) is the down left position ?
 *-----------------------------------------------------------------------------*/

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


/*-----------------------------------------------------------------------------
 *  boundingbox_1
 *  To get the bounding rectangle of a string
 *-----------------------------------------------------------------------------*/

static void boundingbox_1(BCG *Xgc,char *string, double x, double y, double *rect)
{ 
  int x1,yy1,rect1[4];
  x1 = XDouble2Pixel(x);
  yy1 = YDouble2Pixel(y);
  Xgc->graphic_engine->boundingbox(Xgc,string,x1,yy1,rect1);
  scale_i2f(Xgc,rect,rect+1,rect1,rect1+1,1);
  length_scale_i2f(Xgc,rect+2,rect+3,rect1+2,rect1+3,1);
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
  if (Xgc->record_flag == TRUE) store_xstringb_1(Xgc,str,fflag,xd,yd,wd,hd);
  x = XDouble2Pixel(*xd);
  y = YDouble2Pixel(*yd);
  length_scale_f2i(Xgc,wd,hd,&wbox,&hbox,1);
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


/**********************************
 * StrMat = 'xxxxZxxxxZxxx....' Z = \n 
 * find the enclosing rectangle for drawing 
 * the string StrMat 
 * and the string is Drawn if Dflag  == 1 ;
 **********************************/

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
  scale_f2i(Xgc,&dest_x,&dest_y,&idest_x,&idest_y,1);
  length_scale_f2i(Xgc,&w,&h,&iw,&ih,1);
  if (Xgc->record_flag == TRUE)
    store_pixbuf(Xgc,pix,  src_x, src_y,dest_x,dest_y, w, h);
  Xgc->graphic_engine->draw_pixbuf(Xgc,pixbuf, src_x, src_y,idest_x,idest_y, iw, ih);
  
}

static void draw_pixbuf_from_file_1(BCG *Xgc,const char *fname,int src_x,int src_y,double dest_x,
				    double dest_y,double w,double  h)
{ 
  int idest_x,idest_y,iw,ih;
  scale_f2i(Xgc,&dest_x,&dest_y,&idest_x,&idest_y,1);
  length_scale_f2i(Xgc,&w,&h,&iw,&ih,1);
  if (Xgc->record_flag == TRUE)
    store_pixbuf_from_file(Xgc,fname,  src_x, src_y,dest_x,dest_y, w, h);
  Xgc->graphic_engine->draw_pixbuf_from_file(Xgc,fname, src_x, src_y,idest_x,idest_y, iw, ih);
  
}


/*-----------------------------------------------------------------------------
 * Utilities : Allocation 
 *-----------------------------------------------------------------------------*/

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





