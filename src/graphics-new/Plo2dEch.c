/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2000 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/


#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"

static void zoom_rect (BCG *Xgc,double,double,double,double);

/*--------------------------------------------------------------------
 * converts a rectangle into a wrect specification 
 * (see xsetech) 
 * This can be used to locally change the scales in order 
 * to set the plot region to a specified rectangle 
 * --------------------------------------------------------------------------*/

void scale_f2wrect(BCG *Xgc,const double x[],double x1[])
{
  int i=0;
  if ( Xgc->scales->logflag[0] == 'n' ) 
    {
      x1[i]=  XScale_d(x[i]);
      x1[i+2]= Xgc->scales->Wscx1*( x[i+2]);
    }
  else 
    {
      x1[i]= XLogScale_d(x[i]);
      x1[i+2]=Xgc->scales->Wscx1*(log10((x[i]+x[i+2])/x[i]));
    } 
  if ( Xgc->scales->logflag[1] == 'n' ) 
    {
      x1[i+1]= YScale_d(x[i+1]);
      x1[i+3]= Xgc->scales->Wscy1*( x[i+3]);
    }
  else 
    {
      x1[i+1]= YLogScale_d(x[i+1]);
      x1[i+3]= Xgc->scales->Wscy1*(log10(x[i+1]/(x[i+1]-x[i+3])));
    }
  x1[0] /= (double) Xgc->scales->wdim[0];
  x1[2] /= (double) Xgc->scales->wdim[0];
  x1[1] /= (double) Xgc->scales->wdim[1];
  x1[3] /= (double) Xgc->scales->wdim[1];
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

void scale_f2i(BCG *Xgc,const double x[],const double y[],int x1[],int y1[],int n)
{
  int i;
  if (Xgc->scales->logflag[0] == 'n') 
    for ( i=0 ; i < n  ; i++) x1[i]= XScale(x[i]);
  else 
    for ( i=0 ; i < n  ; i++) x1[i]= XLogScale(x[i]);
  if (Xgc->scales->logflag[1] == 'n') 
    for ( i=0 ; i < n ; i++)  y1[i]= YScale(y[i]);
  else 
    for ( i=0 ; i < n ; i++)  y1[i]= YLogScale(y[i]);
}


void scale_i2f(BCG *Xgc, double x[], double y[],const int x1[],const int y1[],int n)
{
  int i;
  if (Xgc->scales->logflag[0] == 'n') 
    for ( i=0 ; i < n ; i++) x[i]= XPi2R( x1[i] );
  else 
    for ( i=0 ; i < n ; i++) x[i]= exp10(XPi2R( x1[i]));
  if (Xgc->scales->logflag[1] == 'n') 
    for ( i=0 ; i < n ; i++)  y[i]= YPi2R( y1[i] );
  else 
    for ( i=0 ; i < n ; i++)  y[i]= exp10(YPi2R( y1[i]));
}

/*--------------------------------------------------------------------
 * void echelle2dl(x, y, x1, yy1, n1, n2,  dir)
 * like echelle2d but for length convertion 
 * Note that it cannot work in logarithmic scale 
 *--------------------------------------------------------------------*/

void length_scale_f2i(BCG *Xgc,const double *x,const double *y, int *x1, int *y1, int n)
{
  int i;
  for ( i=0 ; i < n ; i++)
    {
      x1[i]=inint( Xgc->scales->Wscx1*( x[i]));
      y1[i]=inint( Xgc->scales->Wscy1*( y[i]));
    }
}

void length_scale_i2f(BCG *Xgc,double *x, double *y, const int *x1, const int *y1, int n)
{
  int i;
  for ( i=0 ; i < n ; i++)
    {
      x[i]=x1[i]/Xgc->scales->Wscx1;
      y[i]=y1[i]/Xgc->scales->Wscy1;
    }
}



/** meme chose mais pour transformer des ellipses **/

void ellipse2d(BCG *Xgc,double *x, int *x1, int *n, char *dir)
{
  int i;
  if (strcmp("f2i",dir)==0) 
    {
      /** double to int (pixel) direction **/
      for ( i=0 ; i < (*n) ; i=i+6)
	{
	  x1[i  ]= XScale(x[i]);
	  x1[i+1]= YScale(x[i+1]);
	  x1[i+2]= inint( Xgc->scales->Wscx1*( x[i+2]));
	  x1[i+3]= inint( Xgc->scales->Wscy1*( x[i+3]));
	  x1[i+4]= inint( x[i+4]);
	  x1[i+5]= inint( x[i+5]);
	}
    }
  else if (strcmp("i2f",dir)==0) 
    {
      for ( i=0 ; i < (*n) ; i=i+6)
	{
	  x[i]=   XPi2R(x1[i]); 
	  x[i+1]= YPi2R( x1[i+1] ); 
	  x[i+2]= x1[i+2]/Xgc->scales->Wscx1;
	  x[i+3]= x1[i+3]/Xgc->scales->Wscy1;
	  x[i+4]= x1[i+4];
	  x[i+5]= x1[i+5];
	}
    }
  else 
    sciprint(" Wrong dir %s argument in echelle2d\r\n",dir);
}

/** meme chose mais pour transformer des rectangles **/

void rect2d_f2i(BCG *Xgc,const double x[],int x1[], int n)
{
  int i;
  /** double to int (pixel) direction **/
  for ( i=0 ; i < n ; i= i+4)
    {
      if ( Xgc->scales->logflag[0] == 'n' ) 
	{
	  x1[i]=  XScale(x[i]);
	  x1[i+2]=inint( Xgc->scales->Wscx1*( x[i+2]));
	}
      else 
	{
	  x1[i]= XLogScale(x[i]);
	  x1[i+2]=inint( Xgc->scales->Wscx1*(log10((x[i]+x[i+2])/x[i])));
	} 
      if ( Xgc->scales->logflag[1] == 'n' ) 
	{
	  x1[i+1]= YScale(x[i+1]);
	  x1[i+3]=inint( Xgc->scales->Wscy1*( x[i+3]));
	}
      else 
	{
	  x1[i+1]= YLogScale(x[i+1]);
	  x1[i+3]=inint( Xgc->scales->Wscy1*(log10(x[i+1]/(x[i+1]-x[i+3]))));
	}
    }
} 
  
void rect2d_i2f(BCG *Xgc,double x[],const  int x1[], int n)
{
  int i;
  for ( i=0 ; i < n ; i=i+4)
    {
      if ( Xgc->scales->logflag[0] == 'n' ) 
	{
	  x[i]= XPi2R(x1[i] );
	  x[i+2]=x1[i+2]/Xgc->scales->Wscx1;
	}
      else
	{
	  x[i]=exp10( XPi2R(x1[i]));
	  x[i+2]=exp10(XPi2R( x1[i]+x1[i+2] ));
	  x[i+2] -= x[i];
	}
      if ( Xgc->scales->logflag[1] == 'n' ) 
	{
	  x[i+1]= YPi2R( x1[i+1]);
	  x[i+3]=x1[i+3]/Xgc->scales->Wscy1;
	}
      else
	{
	  x[i+1]=exp10( YPi2R( x1[i+1]));
	  x[i+3]=exp10( YPi2R( x1[i+3]+x1[i+1])); 
	  x[i+2] -= x[i+1];
	}
    }
}


 
/** meme chose mais pour axis **/

void axis2d(BCG *Xgc,double *alpha, double *initpoint, double *size, int *initpoint1, double *size1)
{
  double sina ,cosa;
  double xx,yy,scl;
  /* pour eviter des problemes numerique quand Xgc->scales->scx1 ou Xgc->scales->scy1 sont 
   *  tres petits et cosal ou sinal aussi 
   */
  if ( Abs(*alpha) == 90 ) 
    {
      scl=Xgc->scales->Wscy1;
    }
  else 
    {
      if (Abs(*alpha) == 0) 
	{
	  scl=Xgc->scales->Wscx1;
	}
      else 
	{
	  sina= sin(*alpha * M_PI/180.0);
	  cosa= cos(*alpha * M_PI/180.0);
	  xx= cosa*Xgc->scales->Wscx1; xx *= xx;
	  yy= sina*Xgc->scales->Wscy1; yy *= yy;
	  scl= sqrt(xx+yy);
	}
    }
  size1[0] = size[0]*scl;
  size1[1]=  size[1]*scl;
  size1[2]=  size[2];
  initpoint1[0]= XScale(initpoint[0]);
  initpoint1[1]= YScale(initpoint[1]);
}

/** Changement interactif d'echelle **/

extern int EchCheckSCPlots();

/** get a rectangle interactively **/ 

void zoom_get_rectangle(BCG *Xgc,double *bbox)
{
  /* Using the mouse to get the new rectangle to fix boundaries */
  int th,th1=1, pixmode,alumode,color,style,fg;
  int ibutton,iwait=FALSE,istr=0;
  double x0,yy0,x,y,xl,yl;

  pixmode = nsp_gengine->xget_pixmapOn(Xgc);
  alumode = nsp_gengine->xget_alufunction(Xgc);
  th = nsp_gengine->xget_thickness(Xgc);
  color= nsp_gengine->xget_pattern(Xgc);
  style = nsp_gengine->xget_dash(Xgc);
  fg    = nsp_gengine->xget_foreground(Xgc);
  set_no_delete_win_mode();

#ifdef WIN32
  SetWinhdc();
  SciMouseCapture();
#endif 
 nsp_gengine->scale->set_driver("X11");
 nsp_gengine->xset_thickness(Xgc,th1);
 nsp_gengine->xset_dash(Xgc,1);
 nsp_gengine->xset_pattern(Xgc,fg);
 
  /** XXXXXX : a regler pour Win32 in = 6 **/
 nsp_gengine->scale->xset1_alufunction1(Xgc,6);
 nsp_gengine->scale->xclick_1(Xgc,"one",&ibutton,&x0,&yy0,iwait,FALSE,FALSE,FALSE,istr);
 x=x0;y=yy0;
 ibutton=-1;
 while ( ibutton == -1 ) 
   {
     /* dessin d'un rectangle */
     zoom_rect(Xgc,x0,yy0,x,y);
     if ( pixmode == 1) nsp_gengine->scale->xset1_show(Xgc);
     nsp_gengine->scale->xgetmouse_1(Xgc,"one",&ibutton,&xl, &yl,iwait,TRUE,FALSE,FALSE);
     /* effacement du rectangle */
     zoom_rect(Xgc,x0,yy0,x,y);
     if ( pixmode == 1) nsp_gengine->scale->xset1_show(Xgc);
     x=xl;y=yl;
    }
#ifndef WIN32
  /** XXXX */
 nsp_gengine->scale->xset1_alufunction1(Xgc,3);
#endif
  /* Back to the default driver which must be Rec and redraw the recorded
   * graphics with the new scales 
   */
  bbox[0]=Min(x0,x);
  bbox[1]=Min(yy0,y);
  bbox[2]=Max(x0,x);
  bbox[3]=Max(yy0,y);
  nsp_gengine->scale->xset1_alufunction1(Xgc,alumode);
  nsp_gengine->xset_thickness(Xgc,th);
  nsp_gengine->xset_dash(Xgc,style);
  nsp_gengine->xset_pattern(Xgc,color);
  
  set_delete_win_mode();
  nsp_gengine->xinfo(Xgc," ");
#ifdef WIN32
  ReleaseWinHdc();
  SciMouseRelease();
#endif 

}

void zoom(BCG *Xgc)
{
  char driver[4];
  int aaint[4],flag[2]; /* ansi : ={1,0};*/
  int ww;
  flag[0] =1 ; flag[1]=0;
  ww= nsp_gengine->xget_curwin();
  nsp_gengine->scale->get_driver_name(driver);
  if (strcmp("Rec",driver) != 0) 
    {
      Scistring("\n Use the Rec driver to zoom " );
      return;
    }
  else 
    {
      double bbox[4];
      zoom_get_rectangle(Xgc,bbox);
      nsp_gengine->scale->set_driver(driver);
      nsp_gengine->clearwindow(Xgc);    
      tape_replay_new_scale(Xgc,ww,flag,aaint,bbox);
    }
}

void unzoom(BCG *Xgc)
{
  char driver[4];
  int ww;
  nsp_gengine->scale->get_driver_name(driver);
  if (strcmp("Rec",driver) != 0) 
    {
      Scistring("\n Use the Rec driver to unzoom " );
      return;
    }
  else 
    {
      nsp_gengine->clearwindow(Xgc);
      ww = nsp_gengine->xget_curwin();
      tape_replay_undo_scale(Xgc,ww);
    }
}

/**
  Win32, warning when using xor mode
  colors are changed and black is turned to white
  so we must use an other pattern than the black one
  inside dbox
  **/

static void zoom_rect(BCG *Xgc,double x0,double yy0,double  x,double  y)
{
  double rect[4]= {Min(x0,x),Max(yy0,y),Abs(x0-x),Abs(yy0-y)};
#ifdef WIN32
  int pat;
  pat = nsp_gengine->xset_pattern(Xgc,3);
#endif
  nsp_gengine->scale->drawrectangle_1(Xgc,rect);
#ifdef WIN32
   nsp_gengine->xset_pattern(Xgc,pat);
#endif
}

/* 
 *  FRectI=[xmin,ymin,xmax,ymax] est transforme de 
 *  facon a avoir une graduation simple et reguliere 
 *  Xdec,Ydec,xnax,ynax
 *  caracterisant cette graduation 
 *  (voir les fonctions qui suivent )
 */

void Gr_Rescale(char *logf, double *FRectI, int *Xdec, int *Ydec, int *xnax, int *ynax)
{
  double FRectO[4];
  if (logf[0] == 'n') 
    {
      graduate(FRectI,FRectI+2,FRectO,FRectO+2,xnax,xnax+1,Xdec,Xdec+1,Xdec+2);
      FRectI[0]=FRectO[0];FRectI[2]=FRectO[2];
    }
  else
    {
      Xdec[0]=inint(FRectI[0]);
      Xdec[1]=inint(FRectI[2]);
      Xdec[2]=0;
    }
  if (logf[1] == 'n') 
    {
      graduate(FRectI+1,FRectI+3,FRectO+1,FRectO+3,ynax,ynax+1,Ydec,Ydec+1,Ydec+2);
      FRectI[1]=FRectO[1];FRectI[3]=FRectO[3];
    }
  else
    {
      Ydec[0]=inint(FRectI[1]);Ydec[1]=inint(FRectI[3]);Ydec[2]=0;
    }
}

    













