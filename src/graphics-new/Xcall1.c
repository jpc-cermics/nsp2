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
#include "Graphics.h" 

static void GSciString (int,int x,int y,char *StrMat,int *w,int *h);
static void Myalloc1 (int **xm,int n,int *err);
static void Myalloc (int **xm,int **ym, int n, int *err);
static void xstringb ( char *string,int x, int y, int w, int h);

static void drawarc_1(double arc[]);
static void fillarcs_1(double vects[],int fillvect[], int n);
static void drawarcs_1(double vects[], int style[], int n);
static void fillpolyline_1(double *vx, double *vy,int n,int closeflag);
static void drawarrows_1(double vx[],double vy[],int n,double as, int style[], int iflag);
static void drawaxis_1(double *alpha, int *nsteps, double *initpoint, double *size);
static void cleararea_1(double x, double y, double w, double h);
static void xclick_1(char *str,int *ibutton, double *x, double *y, int iflag,int motion,int release,int key, int istr);
static void xclick_any_1(char *str, int *ibutton, double *x, double *y, int *iwin,int iflag,int motion,int release,int key,int istr) ;
static void xgetmouse_1(char *str, int *ibutton, double *x, double *y, int iflag,int motion,int release,int key);
static void fillarc_1( double arc[]);
static void fillrectangle_1(double rect[]);
static void drawpolyline_1(double *vx, double *vy , int n,int closeflag);
static void fillpolylines_1( double *vx, double *vy, int *fillvect, int n, int p,int v1);
static void drawpolymark_1(double *vx, double *vy,int n);
static void displaynumbers_1(double *x, double *y,int n, int flag,double *z, double *alpha);
static void drawpolylines_1(double *vx, double *vy, int *drawvect,int n, int p);
static void drawrectangle_1(double rect[]);
static void drawrectangles_1(double vects[],int fillvect[], int n);
static void drawsegments_1(double *vx, double *vy,int n, int *style, int iflag);
static void displaystring_1(char *string,double x, double y,int flag, double angle);
static void displaystringa_1(char *string, int ipos);
static void boundingbox_1(char *string, double x, double y, double *rect);
static void xstringb_1(char *str,int *fflag, double *xd, double *yd, double *wd, double *hd);

static void set_driver(char *x0) ;
static void get_driver_name(char *str);
static char get_driver(void ) ;
static int get_driver_id(void );

static void xset1_clipping_p(double x,double y,double w,double h);
static void xset1_clipgrf();
static void xset1_alufunction1(int val);
static void xset1_background(int val);
static void xset1_unclip();
static void xset1_clip(double x[]);
static void xset1_pattern(int val);
static void xset1_colormap(int m, double val[]);
static void xset1_default(void) ;
static void xset1_font_size(int val);
static void xset1_font(int val,int val1);
static void xset1_foreground(int val);
static void xset1_hidden3d(int val);
static void xset1_absourel(int val);
static void xset1_dash(int val);
static void xset1_mark_size(int val);
static void xset1_mark(int val,int val1);
static void xset1_pixmapOn(int val);
static void xset1_thickness(int val);
static void xset1_usecolor(int val);
static void xset1_viewport(int val,int val1);
static void xset1_windowdim(int val,int val1);
static void xset1_popupdim(int val,int val1);
static void xset1_windowpos(int val,int val1);
static void xset1_wresize(int val);

static void xset1_autoclear(int num);
static void xset1_autoclear_def(void);
static void xset1_fpf(char *fmt) ;
static void xset1_fpf_def() ;

static void xset1_show(void);
static void xset1_pixmapclear(void);

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
      if (x0[1] == 'I') {
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
      if (x0[1] == 'P') {
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
      if (x0[1] == 'I') 
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
      if (x0[1] == 'P') {
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


/* The following function can be called by fortran so we 
 * use the Sun C-fortran interface conventions 
 *  dr has 2 last extra parametres to deal with the size of
 * XXXXX  keep in mind the SetWinhdc and ReleaseWin for windows 
 */

int dr_XXXX(char x0[],char x1[],integer *x2,integer *x3,integer *x4,integer *x5,integer *x6,
	    integer *x7,double *dx1,double *dx2,double *dx3,double *dx4,
	    integer lx0, integer lx1)
{ 
#ifdef WIN32
  if ( DriverId == 0 && DriverName[0] != 'I' ) 
    {
      SetWinhdc();
      /* call function */ 
      ReleaseWinHdc();
    }
  else 
    {
    }
#else
#endif
  return 0;
}


/**************************************************
 * Global values which are set at this level and 
 * not redirected to each driver
 **************************************************/

/*-----------------------------------------------------------------------------
 *  xset_1 
 *-----------------------------------------------------------------------------*/

void xset1_clipping_p(double x,double y,double w,double h)
{
  int rect[4]={x,y,w,h};
  if (nsp_gengine1.get_driver()=='R')  store_clipping_p(x,y,w,h);
  nsp_gengine->xset_clip(rect);
}

void xset1_clipgrf()
{
  if (nsp_gengine1.get_driver()=='R')  store_clipgrf();
  frame_clip_on();
}

void xset1_unclip()
{
  if (nsp_gengine1.get_driver()=='R')  store_unclip();
  nsp_gengine->xset_unclip();
}

void xset1_clip(double x[])
{
  /** and clipping is special its args are floats **/
  int ix[4];
  scale_f2i(x,x+1,ix,ix+1,1);
  length_scale_f2i(x+2,x+3,ix+2,ix+3,1);
  if (nsp_gengine1.get_driver()=='R') store_clip(x);
  nsp_gengine->xset_clip(ix);
}

void xset1_alufunction1(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_alufunction1(val);
  nsp_gengine->xset_alufunction1(val);
}

void xset1_background(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_background(val);
  nsp_gengine->xset_background(val);
}


void xset1_pattern(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_pattern(val);
  nsp_gengine->xset_pattern(val);
}

void xset1_colormap(int m, double val[])
{
  /* not recorded */ 
  nsp_gengine->xset_colormap(m,3,val);
}


void xset1_default(void) 
{
  /* no record */
  nsp_gengine->xset_autoclear_def() ;
  nsp_gengine->xset_fpf_def() ;
  nsp_gengine->sedeco(0);
  nsp_gengine->sedeco(1);
}

void xset1_font_size(int val)
{
  int font[2];
  if (nsp_gengine1.get_driver()=='R')  store_font_size(val);
  nsp_gengine->xget_font(font);
  nsp_gengine->xset_font(font[0],val);
}

void xset1_font(int val,int val1)
{
  if (nsp_gengine1.get_driver()=='R')  store_font(val,val1);
  nsp_gengine->xset_font(val,val1);
}

void xset1_foreground(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_foreground(val);
  nsp_gengine->xset_foreground(val);
}

void xset1_hidden3d(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_hidden3d(val);
  nsp_gengine->xset_hidden3d(val);
}

void xset1_absourel(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_absourel(val);
  nsp_gengine->xset_absourel(val);
}

void xset1_dash(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_dash(val);
  nsp_gengine->xset_dash(val);
}

void xset1_mark_size(int val)
{
  int mark[2];
  if (nsp_gengine1.get_driver()=='R')  store_mark_size(val);
  nsp_gengine->xget_mark(mark);
  mark[1]=val;
  nsp_gengine->xset_mark(mark[0],mark[1]);
}

void xset1_mark(int val,int val1)
{
  if (nsp_gengine1.get_driver()=='R')  store_mark(val,val1);
  nsp_gengine->xset_mark(val,val1);
}

void xset1_pixmapOn(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_pixmapOn(val);
  nsp_gengine->xset_pixmapOn(val);
}

void xset1_thickness(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_thickness(val);
  nsp_gengine->xset_thickness(val);
}

void xset1_usecolor(int val)
{
  if (nsp_gengine1.get_driver()=='R')  store_usecolor(val);
  nsp_gengine->xset_usecolor(val);
}

void xset1_viewport(int val,int val1)
{
  nsp_gengine->xset_viewport(val,val1);
}

void xset1_windowdim(int val,int val1)
{
  nsp_gengine->xset_windowdim(val,val1);
}

void xset1_popupdim(int val,int val1)
{
  nsp_gengine->xset_popupdim(val,val1);
}

void xset1_windowpos(int val,int val1)
{
  nsp_gengine->xset_windowpos(val,val1);
}

void xset1_wresize(int val)
{
  nsp_gengine->xset_wresize(val);
}

void xset1_show(void)
{
  /* need to store ? */
  if (nsp_gengine1.get_driver()=='R')  store_show();
  nsp_gengine->xset_show();
}

void xset1_pixmapclear(void)
{
  /* need to store ? */
  if (nsp_gengine1.get_driver()=='R')  store_pixmapclear();
  nsp_gengine->xset_pixmapclear();
}


void xset1_autoclear(int val)
{
  nsp_gengine->xset_autoclear(val);
}

void xset1_autoclear_def(void)
{
  nsp_gengine->xset_autoclear_def();
}


void xset1_fpf(char *val)
{
  if (nsp_gengine1.get_driver()=='R')  store_fpf(val);
  nsp_gengine->xset_fpf(val);
}


void xset1_fpf_def()
{
  if (nsp_gengine1.get_driver()=='R')  store_fpf_def();
  nsp_gengine->xset_fpf_def();
}

/*-----------------------------------------------------------------------------
 *  drawarc_1
 *-----------------------------------------------------------------------------*/

void drawarc_1(double arc[])
{ 
  int iarc[6];
  rect2d_f2i(arc,iarc,4);
  iarc[4]=(int) arc[4];
  iarc[5]=(int) arc[5];
  if (nsp_gengine1.get_driver()=='R') store_drawarc_1(arc);
  nsp_gengine->drawarc(iarc);
}

/*-----------------------------------------------------------------------------
 * 
 *-----------------------------------------------------------------------------*/

void fillarcs_1(double vects[],int fillvect[], int n)
{
  int *xm,err=0,n2;
  Myalloc1(&xm,6*n,&err);
  if (err ==  1) return;
  ellipse2d(vects,xm,(n2=6*n,&n2),"f2i");
  if (nsp_gengine1.get_driver()=='R') store_fillarcs_1(vects,fillvect,n);
  nsp_gengine->fillarcs(xm,fillvect,n);
}

/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

void drawarcs_1(double vects[], int style[], int n)
{
  int *xm,err=0,n2;
  Myalloc1(&xm,6*n,&err);
  if (err ==  1) return;
  ellipse2d(vects,xm,(n2=6*(n),&n2),"f2i");
  if (nsp_gengine1.get_driver()=='R') store_drawarcs_1(vects,style,n);
  nsp_gengine->drawarcs(xm,style,n);
}
/*-----------------------------------------------------------------------------
 *  
 *-----------------------------------------------------------------------------*/

void fillpolyline_1(double *vx, double *vy,int n,int closeflag)
{
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err ==  1) return;
  scale_f2i(vx,vy,xm,ym,n);
  if (nsp_gengine1.get_driver()=='R') store_fillpolyline_1(vx,vy,n,closeflag);
  nsp_gengine->fillpolyline(xm,ym,n,closeflag);
}

/*-----------------------------------------------------------------------------
 *  arrows
 *-----------------------------------------------------------------------------*/

void drawarrows_1(double vx[],double vy[],int n,double as, int style[], int iflag)
{ 
  int *xm,*ym,err=0,ias,ias1;
  Myalloc(&xm,&ym,n,&err);
  if (err ==  1) return;
  scale_f2i(vx,vy,xm,ym,n);
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
  scale_f2i(&as,&as,&ias,&ias1,1);
  if (nsp_gengine1.get_driver()=='R')  store_drawarrows_1(vx,vy,n,as,style,iflag);
  ias=10*ias;
  nsp_gengine->drawarrows(xm,ym,n,ias,style,iflag);
}

/*-----------------------------------------------------------------------------
 * axis 
 *-----------------------------------------------------------------------------*/

void drawaxis_1(double *alpha, int *nsteps, double *initpoint, double *size)
{
  int initpoint1[2],alpha1;
  double size1[3];
  alpha1=inint( *alpha);
  axis2d(alpha,initpoint,size,initpoint1,size1);  
  if (nsp_gengine1.get_driver()=='R') store_drawaxis_1(alpha,nsteps,initpoint,size);
  nsp_gengine->drawaxis(alpha1,nsteps,initpoint1,size1);
}
/*-----------------------------------------------------------------------------
 *  cleararea
 *-----------------------------------------------------------------------------*/

void cleararea_1(double x, double y, double w, double h)
{
  int x1,yy1,w1,h1;
  x1 = XDouble2Pixel(x);
  yy1 = YDouble2Pixel(y);
  length_scale_f2i (&w,&h,&w1,&h1,1);
  if (nsp_gengine1.get_driver()=='R') store_cleararea_1(x1,yy1,w1,h1);
  nsp_gengine->cleararea(x1,yy1,w1,h1);
}
/*-----------------------------------------------------------------------------
 * click 
 *-----------------------------------------------------------------------------*/

void xclick_1(char *str,int *ibutton, double *x, double *y, int iflag,int motion,int release,int key, int istr)
{ 
  int x1,yy1,n=1;
  nsp_gengine->xclick(str,ibutton,&x1,&yy1,iflag,motion,release,key,istr);
  scale_i2f(x,y,&x1,&yy1,n);
}
/*-----------------------------------------------------------------------------
 *  click_any
 *-----------------------------------------------------------------------------*/

void xclick_any_1(char *str, int *ibutton, double *x, double *y, int *iwin,int iflag,int motion,int release,int key,int istr)
{ 
  int x1,y1,cur;
  nsp_gengine->xclick_any(str,ibutton,&x1,&y1,iwin,iflag,motion,release,key,istr);
  if (*ibutton>=0){
    cur = nsp_gengine->xset_curwin(*iwin,FALSE);
    scale_i2f(x,y,&x1,&y1,1);
    cur = nsp_gengine->xset_curwin(cur,FALSE);
  }
}

/*-----------------------------------------------------------------------------
 *   xgetmouse
 *-----------------------------------------------------------------------------*/

void xgetmouse_1(char *str, int *ibutton, double *x, double *y, int iflag, int motion,int release,int key)
{ 
  int x1,yy1;
  nsp_gengine->xgetmouse(str,ibutton,&x1,&yy1,iflag,motion,release,key);
  scale_i2f(x,y,&x1,&yy1,1);
}

/*-----------------------------------------------------------------------------
 *   fillarc
 *-----------------------------------------------------------------------------*/

void fillarc_1( double arc[])
{ 
  int iarc[6],n2=4;
  rect2d_f2i(arc,iarc,n2);
  iarc[4]=(int) arc[4];
  iarc[5]=(int) arc[5];
  if (nsp_gengine1.get_driver()=='R') store_fillarc_1(arc);
  nsp_gengine->fillarc(iarc);
}

/*-----------------------------------------------------------------------------
 *  fillrectangle
 *-----------------------------------------------------------------------------*/

void fillrectangle_1(double rect[])
{ 
  int irect[4],n2=4;
  rect2d_f2i(rect,irect,n2);
  if (nsp_gengine1.get_driver()=='R')  store_fillrectangle_1(rect);
  nsp_gengine->fillrectangle(irect);
}

/*-----------------------------------------------------------------------------
 *  drawpolyline
 *-----------------------------------------------------------------------------*/

void drawpolyline_1( double *vx, double *vy ,int n, int closeflag)
{
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err ==  1) return;
  scale_f2i(vx,vy,xm,ym,n);
  if (nsp_gengine1.get_driver()=='R')  store_drawpolyline_1(vx,vy,n,closeflag);
  nsp_gengine->drawpolyline(xm,ym,n,closeflag);
}

/*-----------------------------------------------------------------------------
 *  fillpolylines
 *  if v1==2 then interpolated shading is used 
 *  and fillvect is in that case of dimension n*p 
 *-----------------------------------------------------------------------------*/

void fillpolylines_1( double *vx, double *vy, int *fillvect, int n, int p, int v1)
{
  int *xm,*ym,err=0,i;
  Myalloc(&xm,&ym,n*p,&err);
  if (err ==  1) return;
  scale_f2i(vx,vy,xm,ym,n*p);

  if (nsp_gengine1.get_driver()=='R') store_fillpolylines_1(vx,vy,fillvect,n,p,v1);

  if (v1==2) {
    for (i=0 ; i< (n) ;i++) shade(&xm[(p)*i],&ym[(p)*i],&fillvect[(p)*i],p,0);
  }
  else 
    nsp_gengine->fillpolylines(xm,ym,fillvect,n,p);

  /* end of code modified by polpoth 11/7/2000 */

}
/*-----------------------------------------------------------------------------
 *  drawpolymark
 *-----------------------------------------------------------------------------*/

void drawpolymark_1(double *vx, double *vy,int n)
{
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err ==  1) return;
  scale_f2i(vx,vy,xm,ym,n);
  if (nsp_gengine1.get_driver()=='R')  store_drawpolymark_1(vx,vy,n);
  nsp_gengine->drawpolymark(xm,ym,n);

}

/*-----------------------------------------------------------------------------
 *  displaynumbers
 *-----------------------------------------------------------------------------*/

void displaynumbers_1(double *x, double *y,int n, int flag,double *z, double *alpha)
{
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err ==  1) return;
  scale_f2i(x,y,xm,ym,n);
  if (nsp_gengine1.get_driver()=='R') 
    store_displaynumbers_1(x,y,n,flag,z,alpha);
  nsp_gengine->displaynumbers(xm,ym,n,flag,z,alpha);
}

/*-----------------------------------------------------------------------------
 *   drawpolylines
 *-----------------------------------------------------------------------------*/

void drawpolylines_1(double *vx, double *vy, int *drawvect,int n, int p)
{
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,(n)*(p),&err);
  if (err ==  1) return;
  scale_f2i(vx,vy,xm,ym,n*p);
  if (nsp_gengine1.get_driver()=='R') 
    store_drawpolylines_1(vx,vy,drawvect,n,p);
  nsp_gengine->drawpolylines(xm,ym,drawvect,n,p);
}
/*-----------------------------------------------------------------------------
 *   drawrectangle
 *-----------------------------------------------------------------------------*/

void drawrectangle_1(double rect[])
{
  int xm[4],n2=4;
  rect2d_f2i(rect,xm,n2);
  if (nsp_gengine1.get_driver()=='R') store_drawrectangle_1(rect);
  nsp_gengine->drawrectangle(xm);
}
/*-----------------------------------------------------------------------------
 *   drawrectangles
 *-----------------------------------------------------------------------------*/

void drawrectangles_1(double vects[],int fillvect[], int n)
{
  int *xm,err=0;
  Myalloc1(&xm,4*(n),&err);
  if (err ==  1) return;
  rect2d_f2i(vects,xm,4*(n));
  if (nsp_gengine1.get_driver()=='R') 
    store_drawrectangles_1(vects,fillvect,n);
  nsp_gengine->drawrectangles(xm,fillvect,n);
}

/*-----------------------------------------------------------------------------
 *  drawsegments
 *-----------------------------------------------------------------------------*/

void drawsegments_1(double *vx, double *vy,int n, int *style, int iflag)
{ 
  int *xm,*ym,err=0;
  Myalloc(&xm,&ym,n,&err);
  if (err ==  1) return;
  scale_f2i(vx,vy,xm,ym,n);
  if (nsp_gengine1.get_driver()=='R') 
    store_drawsegments_1(vx,vy,n,style,iflag);
  nsp_gengine->drawsegments(xm,ym,n,style,iflag);
}
/*-----------------------------------------------------------------------------
 *  displaystring
 *-----------------------------------------------------------------------------*/

void displaystring_1(char *string,double x, double y,int flag, double angle)
{
  int x1,yy1;
  x1 = XDouble2Pixel(x);
  yy1 = YDouble2Pixel(y);
  if (nsp_gengine1.get_driver()=='R') 
    store_displaystring_1(string,x,y,flag,angle);
  nsp_gengine->displaystring(string,x1,yy1,flag,angle);
}
/*-----------------------------------------------------------------------------
 *  displaystringa
 *-----------------------------------------------------------------------------*/

void displaystringa_1(char *string, int ipos)
{
  if (nsp_gengine1.get_driver()=='R') 
    store_displaystringa_1(string,ipos);
  
  switch ( ipos )
    {
    case 1:
      xstringb(string,current_scale.WIRect1[0],current_scale.WIRect1[1],current_scale.WIRect1[2],current_scale.WIRect1[3]/6);
      break;
    case 2:
      xstringb(string,current_scale.WIRect1[0]+current_scale.WIRect1[2],current_scale.WIRect1[1]+current_scale.WIRect1[3],current_scale.WIRect1[2]/6,current_scale.WIRect1[3]/6);
      break;
    case 3:
      xstringb(string,current_scale.WIRect1[0],current_scale.WIRect1[1],current_scale.WIRect1[2]/6,current_scale.WIRect1[3]/12);
      break;
    }
}


/*-----------------------------------------------------------------------------
 * display a set of lines coded with 'line1@line2@.....@'
 *   centred in the rectangle [x,y,w=wide,h=height] 
 *-----------------------------------------------------------------------------*/

static void xstringb(char *string, int x, int y, int w, int h)
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
	  nsp_gengine->boundingbox(loc1,x1,yy1,rect);
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
	  nsp_gengine->displaystring(loc1,x1,yy1,flag,angle);
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

void boundingbox_1(char *string, double x, double y, double *rect)
{ 
  int x1,yy1,rect1[4];
  x1 = XDouble2Pixel(x);
  yy1 = YDouble2Pixel(y);
  nsp_gengine->boundingbox(string,x1,yy1,rect1);
  scale_i2f(rect,rect+1,rect1,rect1+1,1);
  length_scale_i2f(rect+2,rect+3,rect1+2,rect1+3,1);
}

/*-----------------------------------------------------------------------------
 * a string in a bounded box : with font size change to fit into the 
 * specified box (only works with driver which properly estimate string sizes)
 *-----------------------------------------------------------------------------*/

#define FONTMAXSIZE 6

void xstringb_1(char *str,int *fflag, double *xd, double *yd, double *wd, double *hd)
{
  int x,y,w,h,wbox,hbox,size;
  int fontid[2];
  if (nsp_gengine1.get_driver()=='R')     store_xstringb_1(str,fflag,xd,yd,wd,hd);
  x = XDouble2Pixel(*xd);
  y = YDouble2Pixel(*yd);
  length_scale_f2i(wd,hd,&wbox,&hbox,1);
  nsp_gengine->xget_font(fontid);
  size = FONTMAXSIZE;
  w = wbox +1;
  if ( *fflag == 1 ) 
    {
      while ( (w > wbox || h > hbox) && size >=0  ) 
	{
	  size--;
	  nsp_gengine->xset_font(fontid[0],size);
	  GSciString(0,x,y,str,&w,&h);
	}
    }
  else 
    GSciString(0,x,y,str,&w,&h);
  x = x +  (wbox - w)/2.0;
  y = y -  (hbox - h)/2.0; 
  GSciString(1,x,y,str,&w,&h);
  nsp_gengine->xset_font(fontid[0],fontid[1]);
}


/**********************************
 * StrMat = 'xxxxZxxxxZxxx....' Z = \n 
 * find the enclosing rectangle for drawing 
 * the string StrMat 
 * and the string is Drawn if Dflag ==1 ;
 **********************************/

void GSciString(int Dflag, int x, int y, char *StrMat, int *w, int *h)
{
  char *p = StrMat,*p1,*p2,*plast;
  int yi=y;
  int wc =0;
  char name[4];
  nsp_gengine1.get_driver_name(name);
  p1 = plast = p+ strlen(p);
  while (1) 
    {
      int logrect[4];
      double angle=0.0;
      int flag=1;
      p2 =p1 ; *p1 = '\0';
      while ( p1 != p && *p1 != '\n' ) 
	p1--;
      if ( Dflag == 1) 
	nsp_gengine->displaystring(( p1 == p ) ? p1 : p1 +1, x,yi,flag,angle);
      nsp_gengine->boundingbox( ( p1 == p ) ? p1 : p1 +1, x,yi,logrect);
      if ( p2 != plast) 	*p2 = '\n';
      wc = Max( wc , logrect[2]);
      if ( p == p1 ) 
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
      if ( *xm == 0 || *ym == 0 )
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
      if (( *xm= graphic_alloc(6,n,sizeof(int)))  == 0  )
	{
	  Scistring("malloc: Running out of memory\n");
	  *err=1;
	}
    }
}






