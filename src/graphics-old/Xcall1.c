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
#include "nsp/math.h"
#include "nsp/sciio.h"
#include "nsp/graphics/Graphics.h" 

static void GSciString (BCG *Xgc,int,int x,int y,char *StrMat,int *w,int *h);
static void Myalloc1 (int **xm,int n,int *err);
static void Myalloc (int **xm,int **ym, int n, int *err);
static void xstringb (BCG *Xgc, char *string,int x, int y, int w, int h);

static void drawarc_1(BCG *Xgc,double arc[]);
static void fillarcs_1(BCG *Xgc,double vects[],int fillvect[], int n);
static void drawarcs_1(BCG *Xgc,double vects[], int style[], int n);
static void fillpolyline_1(BCG *Xgc,double *vx, double *vy,int n,int closeflag);
static void drawarrows_1(BCG *Xgc,double vx[],double vy[],int n,double as, int style[], int iflag);
static void drawaxis_1(BCG *Xgc,double *alpha, int *nsteps, double *initpoint, double *size);
static void cleararea_1(BCG *Xgc,double x, double y, double w, double h);
static void xclick_1(BCG *Xgc,char *str,int *ibutton, double *x, double *y, int iflag,int motion,int release,int key, int istr);
static void xclick_any_1(BCG *Xgc,char *str, int *ibutton, double *x, double *y, int *iwin,int iflag,int motion,int release,int key,int istr) ;
static void xgetmouse_1(BCG *Xgc,char *str, int *ibutton, double *x, double *y, int iflag,int motion,int release,int key);
static void fillarc_1(BCG *Xgc, double arc[]);
static void fillrectangle_1(BCG *Xgc,double rect[]);
static void drawpolyline_1(BCG *Xgc,double *vx, double *vy , int n,int closeflag);
static void fillpolylines_1(BCG *Xgc, double *vx, double *vy, int *fillvect, int n, int p,int v1);
static void drawpolymark_1(BCG *Xgc,double *vx, double *vy,int n);
static void displaynumbers_1(BCG *Xgc,double *x, double *y,int n, int flag,double *z, double *alpha);
static void drawpolylines_1(BCG *Xgc,double *vx, double *vy, int *drawvect,int n, int p);
static void drawrectangle_1(BCG *Xgc,double rect[]);
static void drawrectangles_1(BCG *Xgc,double vects[],int fillvect[], int n);
static void drawsegments_1(BCG *Xgc,double *vx, double *vy,int n, int *style, int iflag);
static void displaystring_1(BCG *Xgc,char *string,double x, double y,int flag, double angle);
static void displaystringa_1(BCG *Xgc,char *string, int ipos);
static void boundingbox_1(BCG *Xgc,char *string, double x, double y, double *rect);
static void xstringb_1(BCG *Xgc,char *str,int *fflag, double *xd, double *yd, double *wd, double *hd);

static void set_driver(char *x0) ;
static void get_driver_name(char *str);
static char get_driver(void ) ;
static int get_driver_id(void );

static void xset1_clipping_p(BCG *Xgc,double x,double y,double w,double h);
static void xset1_clipgrf(BCG *Xgc);
static void xset1_alufunction1(BCG *Xgc,int val);
static void xset1_background(BCG *Xgc,int val);
static void xset1_unclip(BCG *Xgc);
static void xset1_clip(BCG *Xgc,double x[]);
static void xset1_pattern(BCG *Xgc,int val);
static void xset1_colormap(BCG *Xgc,int m, double val[]);
static void xset1_default(BCG *Xgc) ;
static void xset1_font_size(BCG *Xgc,int val);
static void xset1_font(BCG *Xgc,int val,int val1);
static void xset1_foreground(BCG *Xgc,int val);
static void xset1_hidden3d(BCG *Xgc,int val);
static void xset1_absourel(BCG *Xgc,int val);
static void xset1_dash(BCG *Xgc,int val);
static void xset1_mark_size(BCG *Xgc,int val);
static void xset1_mark(BCG *Xgc,int val,int val1);
static void xset1_pixmapOn(BCG *Xgc,int val);
static void xset1_thickness(BCG *Xgc,int val);
static void xset1_usecolor(BCG *Xgc,int val);
static void xset1_viewport(BCG *Xgc,int val,int val1);
static void xset1_windowdim(BCG *Xgc,int val,int val1);
static void xset1_popupdim(BCG *Xgc,int val,int val1);
static void xset1_windowpos(BCG *Xgc,int val,int val1);
static void xset1_wresize(BCG *Xgc,int val);

static void xset1_autoclear(BCG *Xgc,int num);
static void xset1_autoclear_def(BCG *Xgc);
static void xset1_fpf(BCG *Xgc,char *fmt) ;
static void xset1_fpf_def(BCG *Xgc) ;

static void xset1_show(BCG *Xgc);
static void xset1_pixmapclear(BCG *Xgc);

Gengine1 nsp_gengine1={
  set_driver,
  get_driver_name,
  get_driver,
  get_driver_id,

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

  xset1_clipping_p,
  xset1_clipgrf,
  xset1_alufunction1,
  xset1_background,
  xset1_unclip,
  xset1_clip,
  xset1_pattern,
  xset1_colormap,
  xset1_default,
  xset1_font_size,
  xset1_font,
  xset1_foreground,
  xset1_hidden3d,
  xset1_absourel,
  xset1_dash,
  xset1_mark_size,
  xset1_mark,
  xset1_pixmapOn,
  xset1_thickness,
  xset1_usecolor,
  xset1_viewport,
  xset1_windowdim,
  xset1_popupdim,
  xset1_windowpos,
  xset1_wresize,

  xset1_autoclear,
  xset1_autoclear_def,
  xset1_fpf,
  xset1_fpf_def,

  xset1_show,
  xset1_pixmapclear,

};


extern Gengine XFig_gengine, Pos_gengine, Gtk_gengine; 

/*---------------------------------------------------------------
 * The basic graphic driver is X11 
 *    The name is X11 due to historical reasons 
 *    but according to architecture X11 can be an Xwindow driver 
 *    a driver for Macintosh 
 *    or a Windows driver 
 * Other drivers are Postscript Fig ( xfig ) and Rec ( Rec= X11 + Recording capabilities) 
 *    xfig is only meaningfull when using Unix machine 
 * ----------------------------------------------------------------*/

/* a enlever plus tard XXXX */

static char DriverName[]= "Rec";
static int  DriverId = 0;

static void set_driver_old(char *x0) 
{
  switch (x0[0])
    {
    case 'I':
      /** special driver for windows : used when hdc is fixed elsewhere */
      strcpy(DriverName,"Int"); /* internal : for Win32 */
      DriverId = 0;
      break;
    case 'G':
      if (x0[1]  ==  'I') {
          strcpy(DriverName,"GIF");
	  DriverId = 3;
          break;
      }
    case 'X':
    case 'W':
      strcpy(DriverName,"X11");
      DriverId = 0;
      break;
    case 'P'  :
      if (x0[1]  ==  'P') {
	strcpy(DriverName,"PPM");
	DriverId = 3;
	break;
      }
      else {
 	strcpy(DriverName,"Pos");
 	DriverId = 1;
      }
      break;
    case 'F'  :
      strcpy(DriverName,"Fig");
      DriverId = 2;
      break;
    case 'R'  :
      strcpy(DriverName,"Rec");
      DriverId = 0;
      break;
    default:
      Scistring("\n Wrong driver name I'll use X11");
      strcpy(DriverName,"X11");
      DriverId = 0;
      break;
    }
}


static void set_driver(char *x0) 
{
  switch (x0[0])
    {
    case 'I':
      /** special driver for windows : used when hdc is fixed elsewhere */
      strcpy(DriverName,"Int"); /* internal : for Win32 */
      DriverId = 0;
      break;
    case 'G':
      if (x0[1]  ==  'I') 
	{
          strcpy(DriverName,"GIF");
	  DriverId = 3;
          break;
	}
      else 
	{
	  strcpy(DriverName,"X11");
	  DriverId = 0;
	  nsp_gengine = &Gtk_gengine;
	  break;
	}
    case 'X':
    case 'W':
      strcpy(DriverName,"X11");
      DriverId = 0;
      break;
    case 'P'  :
      if (x0[1]  ==  'P') {
	strcpy(DriverName,"PPM");
	DriverId = 3;
	break;
      }
      else {
 	strcpy(DriverName,"Pos");
 	DriverId = 1;
	nsp_gengine = &Pos_gengine;
	
      }
      break;
    case 'F'  :
      strcpy(DriverName,"Fig");
      DriverId = 2;
      nsp_gengine = &XFig_gengine;
      break;
    case 'R'  :
      strcpy(DriverName,"Rec");
      DriverId = 0;
      nsp_gengine = &Gtk_gengine;
      break;
    default:
      Scistring("\n Wrong driver name I'll use X11");
      strcpy(DriverName,"X11");
      DriverId = 0;
      break;
    }
}

static void get_driver_name(char *str)
{
  strcpy(str,DriverName);
}

static char get_driver(void ) {return(DriverName[0]);}

static int get_driver_id(void ) { return DriverId;}


/**************************************************
 * Global values which are set at this level and 
 * not redirected to each driver
 **************************************************/

/*-----------------------------------------------------------------------------
 *  xset_1 
 *-----------------------------------------------------------------------------*/

void xset1_clipping_p(BCG *Xgc,double x,double y,double w,double h)
{
  int rect[4]={x,y,w,h};
  if (Xgc->record_flag == TRUE)  store_clipping_p(Xgc,x,y,w,h);
  Xgc->graphic_engine->xset_clip(Xgc,rect);
}

void xset1_clipgrf(BCG *Xgc)
{
  if (Xgc->record_flag == TRUE)  store_clipgrf(Xgc);
  frame_clip_on(Xgc);
}

void xset1_unclip(BCG *Xgc)
{
  if (Xgc->record_flag == TRUE)  store_unclip(Xgc);
  Xgc->graphic_engine->xset_unclip(Xgc);
}

void xset1_clip(BCG *Xgc,double x[])
{
  /** and clipping is special its args are floats **/
  int ix[4];
  scale_f2i(Xgc,x,x+1,ix,ix+1,1);
  length_scale_f2i(Xgc,x+2,x+3,ix+2,ix+3,1);
  if (Xgc->record_flag == TRUE) store_clip(Xgc,x);
  Xgc->graphic_engine->xset_clip(Xgc,ix);
}

void xset1_alufunction1(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_alufunction1(Xgc,val);
  Xgc->graphic_engine->xset_alufunction1(Xgc,val);
}

void xset1_background(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_background(Xgc,val);
  Xgc->graphic_engine->xset_background(Xgc,val);
}


void xset1_pattern(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_pattern(Xgc,val);
  Xgc->graphic_engine->xset_pattern(Xgc,val);
}

void xset1_colormap(BCG *Xgc,int m, double val[])
{
  /* not recorded */ 
  Xgc->graphic_engine->xset_colormap(Xgc,m,3,val);
}

/* pas clair XXXX */

void xset1_default(BCG *Xgc) 
{
  /* no record */
  Xgc->graphic_engine->xset_autoclear_def(Xgc) ;
  Xgc->graphic_engine->xset_fpf_def(Xgc) ;
  Xgc->graphic_engine->sedeco(0);
  Xgc->graphic_engine->sedeco(1);
}

void xset1_font_size(BCG *Xgc,int val)
{
  int font[2];
  if (Xgc->record_flag == TRUE)  store_font_size(Xgc,val);
  Xgc->graphic_engine->xget_font(Xgc,font);
  Xgc->graphic_engine->xset_font(Xgc,font[0],val);
}

void xset1_font(BCG *Xgc,int val,int val1)
{
  if (Xgc->record_flag == TRUE)  store_font(Xgc,val,val1);
  Xgc->graphic_engine->xset_font(Xgc,val,val1);
}

void xset1_foreground(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_foreground(Xgc,val);
  Xgc->graphic_engine->xset_foreground(Xgc,val);
}

void xset1_hidden3d(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_hidden3d(Xgc,val);
  Xgc->graphic_engine->xset_hidden3d(Xgc,val);
}

void xset1_absourel(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_absourel(Xgc,val);
  Xgc->graphic_engine->xset_absourel(Xgc,val);
}

void xset1_dash(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_dash(Xgc,val);
  Xgc->graphic_engine->xset_dash(Xgc,val);
}

void xset1_mark_size(BCG *Xgc,int val)
{
  int mark[2];
  if (Xgc->record_flag == TRUE)  store_mark_size(Xgc,val);
  Xgc->graphic_engine->xget_mark(Xgc,mark);
  mark[1]=val;
  Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark[1]);
}

void xset1_mark(BCG *Xgc,int val,int val1)
{
  if (Xgc->record_flag == TRUE)  store_mark(Xgc,val,val1);
  Xgc->graphic_engine->xset_mark(Xgc,val,val1);
}

void xset1_pixmapOn(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_pixmapOn(Xgc,val);
  Xgc->graphic_engine->xset_pixmapOn(Xgc,val);
}

void xset1_thickness(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_thickness(Xgc,val);
  Xgc->graphic_engine->xset_thickness(Xgc,val);
}

void xset1_usecolor(BCG *Xgc,int val)
{
  if (Xgc->record_flag == TRUE)  store_usecolor(Xgc,val);
  Xgc->graphic_engine->xset_usecolor(Xgc,val);
}

void xset1_viewport(BCG *Xgc,int val,int val1)
{
  Xgc->graphic_engine->xset_viewport(Xgc,val,val1);
}

void xset1_windowdim(BCG *Xgc,int val,int val1)
{
  Xgc->graphic_engine->xset_windowdim(Xgc,val,val1);
}

void xset1_popupdim(BCG *Xgc,int val,int val1)
{
  Xgc->graphic_engine->xset_popupdim(Xgc,val,val1);
}

void xset1_windowpos(BCG *Xgc,int val,int val1)
{
  Xgc->graphic_engine->xset_windowpos(Xgc,val,val1);
}

void xset1_wresize(BCG *Xgc,int val)
{
  Xgc->graphic_engine->xset_wresize(Xgc,val);
}

void xset1_show(BCG *Xgc)
{
  /* need to store ? */
  if (Xgc->record_flag == TRUE)  store_show(Xgc);
  Xgc->graphic_engine->xset_show(Xgc);
}

void xset1_pixmapclear(BCG *Xgc)
{
  /* need to store ? */
  if (Xgc->record_flag == TRUE)  store_pixmapclear(Xgc);
  Xgc->graphic_engine->xset_pixmapclear(Xgc);
}


void xset1_autoclear(BCG *Xgc,int val)
{
  Xgc->graphic_engine->xset_autoclear(Xgc,val);
}

void xset1_autoclear_def(BCG *Xgc)
{
  Xgc->graphic_engine->xset_autoclear_def(Xgc);
}


void xset1_fpf(BCG *Xgc,char *val)
{
  if (Xgc->record_flag == TRUE)  store_fpf(Xgc,val);
  Xgc->graphic_engine->xset_fpf(Xgc,val);
}


void xset1_fpf_def(BCG *Xgc)
{
  if (Xgc->record_flag == TRUE)  store_fpf_def(Xgc);
  Xgc->graphic_engine->xset_fpf_def(Xgc);
}

/*-----------------------------------------------------------------------------
 *  drawarc_1
 *-----------------------------------------------------------------------------*/

void drawarc_1(BCG *Xgc,double arc[])
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

void fillarcs_1(BCG *Xgc,double vects[],int fillvect[], int n)
{
  int *xm,err=0,n2;
  Myalloc1(&xm,6*n,&err);
  if (err  ==   1) return;
  ellipse2d(Xgc,vects,xm,(n2=6*n,&n2),"f2i");
  if (Xgc->record_flag == TRUE) store_fillarcs_1(Xgc,vects,fillvect,n);
  Xgc->graphic_engine->fillarcs(Xgc,xm,fillvect,n);
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

void drawarcs_1(BCG *Xgc,double vects[], int style[], int n)
{
  int *xm,err=0,n2;
  Myalloc1(&xm,6*n,&err);
  if (err  ==   1) return;
  ellipse2d(Xgc,vects,xm,(n2=6*(n),&n2),"f2i");
  if (Xgc->record_flag == TRUE) store_drawarcs_1(Xgc,vects,style,n);
  Xgc->graphic_engine->drawarcs(Xgc,xm,style,n);
}
/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

void fillpolyline_1(BCG *Xgc,double *vx, double *vy,int n,int closeflag)
{
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n);
  if (Xgc->record_flag == TRUE) store_fillpolyline_1(Xgc,vx,vy,n,closeflag);
  Xgc->graphic_engine->fillpolyline(Xgc,xm,ym,n,closeflag);
}

/*-----------------------------------------------------------------------------
 *  arrows
 *-----------------------------------------------------------------------------*/

void drawarrows_1(BCG *Xgc,double vx[],double vy[],int n,double as, int style[], int iflag)
{ 
  int *xm,*ym,err=0,ias,ias1;
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
  scale_f2i(Xgc,&as,&as,&ias,&ias1,1);
  if (Xgc->record_flag == TRUE)  store_drawarrows_1(Xgc,vx,vy,n,as,style,iflag);
  ias=10*ias;
  Xgc->graphic_engine->drawarrows(Xgc,xm,ym,n,ias,style,iflag);
}

/*-----------------------------------------------------------------------------
 * axis 
 *-----------------------------------------------------------------------------*/

void drawaxis_1(BCG *Xgc,double *alpha, int *nsteps, double *initpoint, double *size)
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

void cleararea_1(BCG *Xgc,double x, double y, double w, double h)
{
  int x1,yy1,w1,h1;
  x1 = XDouble2Pixel(x);
  yy1 = YDouble2Pixel(y);
  length_scale_f2i (Xgc,&w,&h,&w1,&h1,1);
  if (Xgc->record_flag == TRUE) store_cleararea_1(Xgc,x1,yy1,w1,h1);
  Xgc->graphic_engine->cleararea(Xgc,x1,yy1,w1,h1);
}
/*-----------------------------------------------------------------------------
 * click 
 *-----------------------------------------------------------------------------*/

void xclick_1(BCG *Xgc,char *str,int *ibutton, double *x, double *y, int iflag,int motion,int release,int key, int istr)
{ 
  int x1,yy1,n=1;
  Xgc->graphic_engine->xclick(Xgc,str,ibutton,&x1,&yy1,iflag,motion,release,key,istr);
  scale_i2f(Xgc,x,y,&x1,&yy1,n);
}
/*-----------------------------------------------------------------------------
 *  click_any
 *-----------------------------------------------------------------------------*/

void xclick_any_1(BCG *Xgc,char *str, int *ibutton, double *x, double *y, int *iwin,int iflag,int motion,int release,int key,int istr)
{ 
  int x1,y1,cur;
  Xgc->graphic_engine->xclick_any(str,ibutton,&x1,&y1,iwin,iflag,motion,release,key,istr);
  if (*ibutton>=0){
    cur = Xgc->graphic_engine->xset_curwin(*iwin,FALSE);
    scale_i2f(Xgc,x,y,&x1,&y1,1);
    cur = Xgc->graphic_engine->xset_curwin(cur,FALSE);
  }
}

/*-----------------------------------------------------------------------------
 *   xgetmouse
 *-----------------------------------------------------------------------------*/

void xgetmouse_1(BCG *Xgc,char *str, int *ibutton, double *x, double *y, int iflag, int motion,int release,int key)
{ 
  int x1,yy1;
  Xgc->graphic_engine->xgetmouse(Xgc,str,ibutton,&x1,&yy1,iflag,motion,release,key);
  scale_i2f(Xgc,x,y,&x1,&yy1,1);
}

/*-----------------------------------------------------------------------------
 *   fillarc
 *-----------------------------------------------------------------------------*/

void fillarc_1(BCG *Xgc, double arc[])
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

void fillrectangle_1(BCG *Xgc,double rect[])
{ 
  int irect[4],n2=4;
  rect2d_f2i(Xgc,rect,irect,n2);
  if (Xgc->record_flag == TRUE)  store_fillrectangle_1(Xgc,rect);
  Xgc->graphic_engine->fillrectangle(Xgc,irect);
}

/*-----------------------------------------------------------------------------
 *  drawpolyline
 *-----------------------------------------------------------------------------*/

void drawpolyline_1(BCG *Xgc, double *vx, double *vy ,int n, int closeflag)
{
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n);
  if (Xgc->record_flag == TRUE)  store_drawpolyline_1(Xgc,vx,vy,n,closeflag);
  Xgc->graphic_engine->drawpolyline(Xgc,xm,ym,n,closeflag);
}

/*-----------------------------------------------------------------------------
 *  fillpolylines
 *  if v1 == 2 then interpolated shading is used 
 *  and fillvect is in that case of dimension n*p 
 *-----------------------------------------------------------------------------*/

void fillpolylines_1(BCG *Xgc, double *vx, double *vy, int *fillvect, int n, int p, int v1)
{
  int *xm,*ym,err=0,i;
  Myalloc(&xm,&ym,n*p,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n*p);

  if (Xgc->record_flag == TRUE) store_fillpolylines_1(Xgc,vx,vy,fillvect,n,p,v1);

  if (v1 == 2) {
    for (i=0 ; i< (n) ;i++) shade(Xgc,&xm[(p)*i],&ym[(p)*i],&fillvect[(p)*i],p,0);
  }
  else 
    Xgc->graphic_engine->fillpolylines(Xgc,xm,ym,fillvect,n,p);

  /* end of code modified by polpoth 11/7/2000 */

}
/*-----------------------------------------------------------------------------
 *  drawpolymark
 *-----------------------------------------------------------------------------*/

void drawpolymark_1(BCG *Xgc,double *vx, double *vy,int n)
{
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err  ==   1) return;
  scale_f2i(Xgc,vx,vy,xm,ym,n);
  if (Xgc->record_flag == TRUE)  store_drawpolymark_1(Xgc,vx,vy,n);
  Xgc->graphic_engine->drawpolymark(Xgc,xm,ym,n);

}

/*-----------------------------------------------------------------------------
 *  displaynumbers
 *-----------------------------------------------------------------------------*/

void displaynumbers_1(BCG *Xgc,double *x, double *y,int n, int flag,double *z, double *alpha)
{
  int *xm,*ym,err=0;
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

void drawpolylines_1(BCG *Xgc,double *vx, double *vy, int *drawvect,int n, int p)
{
  int *xm,*ym,err=0;
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

void drawrectangle_1(BCG *Xgc,double rect[])
{
  int xm[4],n2=4;
  rect2d_f2i(Xgc,rect,xm,n2);
  if (Xgc->record_flag == TRUE) store_drawrectangle_1(Xgc,rect);
  Xgc->graphic_engine->drawrectangle(Xgc,xm);
}
/*-----------------------------------------------------------------------------
 *   drawrectangles
 *-----------------------------------------------------------------------------*/

void drawrectangles_1(BCG *Xgc,double vects[],int fillvect[], int n)
{
  int *xm,err=0;
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

void drawsegments_1(BCG *Xgc,double *vx, double *vy,int n, int *style, int iflag)
{ 
  int *xm,*ym,err=0;
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

void displaystring_1(BCG *Xgc,char *string,double x, double y,int flag, double angle)
{
  int x1,yy1;
  x1 = XDouble2Pixel(x);
  yy1 = YDouble2Pixel(y);
  if (Xgc->record_flag == TRUE) 
    store_displaystring_1(Xgc,string,x,y,flag,angle);
  Xgc->graphic_engine->displaystring(Xgc,string,x1,yy1,flag,angle);
}
/*-----------------------------------------------------------------------------
 *  displaystringa
 *-----------------------------------------------------------------------------*/

void displaystringa_1(BCG *Xgc,char *string, int ipos)
{
  if (Xgc->record_flag == TRUE) 
    store_displaystringa_1(Xgc,string,ipos);
  
  switch ( ipos )
    {
    case 1:
      xstringb(Xgc,string,Xgc->scales->WIRect1[0],Xgc->scales->WIRect1[1],Xgc->scales->WIRect1[2],Xgc->scales->WIRect1[3]/6);
      break;
    case 2:
      xstringb(Xgc,string,Xgc->scales->WIRect1[0]+Xgc->scales->WIRect1[2],Xgc->scales->WIRect1[1]+Xgc->scales->WIRect1[3],Xgc->scales->WIRect1[2]/6,Xgc->scales->WIRect1[3]/6);
      break;
    case 3:
      xstringb(Xgc,string,Xgc->scales->WIRect1[0],Xgc->scales->WIRect1[1],Xgc->scales->WIRect1[2]/6,Xgc->scales->WIRect1[3]/12);
      break;
    }
}


/*-----------------------------------------------------------------------------
 * display a set of lines coded with 'line1@line2@.....@'
 *   centred in the rectangle [x,y,w=wide,h=height] 
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


/*-----------------------------------------------------------------------------
 *  boundingbox_1
 *  To get the bounding rectangle of a string
 *-----------------------------------------------------------------------------*/

void boundingbox_1(BCG *Xgc,char *string, double x, double y, double *rect)
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

void xstringb_1(BCG *Xgc,char *str,int *fflag, double *xd, double *yd, double *wd, double *hd)
{
  int x,y,w,h,wbox,hbox,size;
  int fontid[2];
  if (Xgc->record_flag == TRUE)     store_xstringb_1(Xgc,str,fflag,xd,yd,wd,hd);
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
    GSciString(Xgc,0,x,y,str,&w,&h);
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

void GSciString(BCG *Xgc,int Dflag, int x, int y, char *StrMat, int *w, int *h)
{
  char *p = StrMat,*p1,*p2,*plast;
  int yi=y;
  int wc =0;
  p1 = plast = p+ strlen(p);
  while (1) 
    {
      int logrect[4];
      double angle=0.0;
      int flag=1;
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






