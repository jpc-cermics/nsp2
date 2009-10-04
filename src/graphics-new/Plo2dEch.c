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


#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include <math.h>
#include <gdk/gdk.h>
#include "nsp/math.h"
#include "nsp/graphics-new/Graphics.h"

#if 0 
static void zoom_rect (BCG *Xgc,double,double,double,double);
#endif 

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
  double xd,yd;
  int i;
  if (Xgc->scales->logflag[0] == 'n' ) 
    {
      if (Xgc->scales->logflag[1] == 'n') 
	{
	  for ( i=0 ; i < n  ; i++) 
	    {
	      xd = XScaleR_d(x[i],y[i]);
	      yd = YScaleR_d(x[i],y[i]);
	      x1[i]= Min(Max(-int16max,xd),int16max);
	      y1[i]= Min(Max(-int16max,yd),int16max);
	    }
	}
      else
	{
	  for ( i=0 ; i < n  ; i++) 
	    {
	      xd = XScaleR_d(x[i],log10(y[i]));
	      yd = YScaleR_d(x[i],log10(y[i]));
	      x1[i]= Min(Max(-int16max,xd),int16max);
	      y1[i]= Min(Max(-int16max,yd),int16max);
	    }
	}
    }
  else
    {
      if (Xgc->scales->logflag[1] == 'n') 
	{
	  for ( i=0 ; i < n  ; i++) 
	    {
	      xd = XScaleR_d(log10(x[i]),y[i]);
	      yd = YScaleR_d(log10(x[i]),y[i]);
	      x1[i]= Min(Max(-int16max,xd),int16max);
	      y1[i]= Min(Max(-int16max,yd),int16max);
	    }
	}
      else
	{
	  for ( i=0 ; i < n  ; i++) 
	    {
	      xd = XScaleR_d(log10(x[i]),log10(y[i]));
	      yd = YScaleR_d(log10(x[i]),log10(y[i]));
	      x1[i]= Min(Max(-int16max,xd),int16max);
	      y1[i]= Min(Max(-int16max,yd),int16max);
	    }
	}
    }
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

/* Utilities
 * scale conversions 
 */

void rect2d_f2i(BCG *Xgc,const double x[],int x1[], int n)
{
  int i;
  /** double to int (pixel) direction **/
  for ( i=0 ; i < n ; i= i+4)
    {
      if ( Xgc->scales->logflag[0] == 'n' ) 
	{
	  x1[i]=  XScale(x[i]);
	  /* x1[i+2]=inint( Xgc->scales->Wscx1*( x[i+2])); */
	  x1[i+2]= XScale(x[i]+x[i+2]) -x1[i];
	}
      else 
	{
	  x1[i]= XLogScale(x[i]);
	  x1[i+2]=inint( Xgc->scales->Wscx1*(log10((x[i]+x[i+2])/x[i])));
	} 
      if ( Xgc->scales->logflag[1] == 'n' ) 
	{
	  x1[i+1]= YScale(x[i+1]);
	  /* x1[i+3]=inint( Xgc->scales->Wscy1*( x[i+3]));*/
	  x1[i+3]= YScale(x[i+1]-x[i+3]) - x1[i+1];
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

 
/* meme chose mais pour axis */

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

/* Changement interactif d'echelle */

extern int EchCheckSCPlots();

/* get a rectangle interactively */ 


/* A version for drivers who do not have Xor mode 
 * we have to redraw while acquiring the zoom rectangle 
 * we could also try to keep the graphiv in a backing store 
 * pixmap. 
 */
#if 0
static void zoom_get_rectangle_noxor(BCG *Xgc,double *bbox, int *ibbox)
{
  /* Using the mouse to get the new rectangle to fix boundaries */
  int th,pixmode,alumode,color,style,fg;
  int ibutton,imask,iwait=FALSE,istr=0;
  double x0,y0,x,y,xl,yl;
  int    ix0,iy0,ix,iy,ixl,iyl;
  if ( Xgc == NULL ) return; 
  if ( Xgc->graphic_engine->xget_recording(Xgc) == FALSE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"zoom is only possible when recording is on" );
      return;
    }
  Xgc->graphic_engine->xset_win_protect(Xgc,TRUE); /* protect against window kill */
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  alumode = Xgc->graphic_engine->xget_alufunction(Xgc);
  th = Xgc->graphic_engine->xget_thickness(Xgc);
  color= Xgc->graphic_engine->xget_pattern(Xgc);
  style = Xgc->graphic_engine->xget_dash(Xgc);
  fg    = Xgc->graphic_engine->xget_foreground(Xgc);
  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
  Xgc->graphic_engine->xset_thickness(Xgc,1);
  Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->xset_pattern(Xgc,fg);
  Xgc->graphic_engine->xclick(Xgc,"one",&ibutton,&imask,&ix0,&iy0,iwait,FALSE,FALSE,FALSE,istr);
  scale_i2f(Xgc,&x0,&y0,&ix0,&iy0,1);
  x=x0;y=y0;
  ix=ix0;iy=iy0;
  ibutton=-1;
  while ( ibutton == -1 ) 
    {
      /* */
      Xgc->graphic_engine->xset_recording(Xgc,FALSE);
      Xgc->graphic_engine->clearwindow(Xgc);    
      zoom_rect(Xgc,x0,y0,x,y);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      Xgc->graphic_engine->force_redraw(Xgc);
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ixl, &iyl,iwait,TRUE,FALSE,FALSE);
      scale_i2f(Xgc,&xl,&yl,&ixl,&iyl,1);
      x=xl;y=yl;
      ix=ixl;iy=iyl;
    }
  /* Back to the default driver which must be Rec and redraw the recorded
   * graphics with the new scales 
   */
  bbox[0]=Min(x0,x);
  bbox[1]=Min(y0,y);
  bbox[2]=Max(x0,x);
  bbox[3]=Max(y0,y);
  ibbox[0]=Min(ix0,ix);
  ibbox[1]=Min(iy0,iy);
  ibbox[2]=Max(ix0,ix);
  ibbox[3]=Max(iy0,iy);

  Xgc->graphic_engine->xset_thickness(Xgc,th);
  Xgc->graphic_engine->xset_dash(Xgc,style);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  Xgc->graphic_engine->xset_win_protect(Xgc,FALSE); /* protect against window kill */
  Xgc->graphic_engine->xinfo(Xgc," ");
  Xgc->graphic_engine->xset_recording(Xgc,TRUE);
  Xgc->graphic_engine->force_redraw(Xgc);
}
#endif 

/* A version for drivers who do not have Xor mode 
 * we have to redraw while acquiring the zoom rectangle 
 * we could also try to keep the graphic in a backing store 
 * pixmap. 
 */

static void zoom_get_rectangle(BCG *Xgc,double *bbox, int *ibbox)
{
  /* Using the mouse to get the new rectangle to fix boundaries */
  int th,pixmode,alumode,color,style,fg;
  int ibutton,imask,iwait=FALSE,istr=0;
  double x0,y0,x,y,xl,yl;
  int    ix0,iy0,ix,iy,ixl,iyl;
  if ( Xgc == NULL ) return; 
  Xgc->graphic_engine->xset_win_protect(Xgc,TRUE); /* protect against window kill */
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  alumode = Xgc->graphic_engine->xget_alufunction(Xgc);
  th = Xgc->graphic_engine->xget_thickness(Xgc);
  color= Xgc->graphic_engine->xget_pattern(Xgc);
  style = Xgc->graphic_engine->xget_dash(Xgc);
  fg    = Xgc->graphic_engine->xget_foreground(Xgc);
  Xgc->graphic_engine->xset_thickness(Xgc,1);
  Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->xset_pattern(Xgc,fg);
  nsp_set_cursor(Xgc,GDK_TOP_LEFT_CORNER );
  Xgc->graphic_engine->xclick(Xgc,"one",&ibutton,&imask,&ix0,&iy0,iwait,FALSE,FALSE,FALSE,istr);
  scale_i2f(Xgc,&x0,&y0,&ix0,&iy0,1);
  x=x0;y=y0;
  ix=ix0;iy=iy0;
  ibutton=-1;
  while ( ibutton == -1 ) 
    {
      double rect[4]= {Min(x0,x),Max(y0,y),Abs(x0-x),Abs(y0-y)};
      Xgc->graphic_engine->clearwindow(Xgc);    
      rect2d_f2i(Xgc,rect,Xgc->zrect,1);
      Xgc->graphic_engine->force_redraw(Xgc);
      nsp_set_cursor(Xgc,GDK_BOTTOM_RIGHT_CORNER);
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ixl, &iyl,iwait,TRUE,TRUE,FALSE);
      scale_i2f(Xgc,&xl,&yl,&ixl,&iyl,1);
      x=xl;y=yl;
      ix=ixl;iy=iyl;
    }
  nsp_set_cursor(Xgc,-1);
  /* Back to the default driver which must be Rec and redraw the recorded
   * graphics with the new scales 
   */
  bbox[0]=Min(x0,x);
  bbox[1]=Min(y0,y);
  bbox[2]=Max(x0,x);
  bbox[3]=Max(y0,y);
  ibbox[0]=Min(ix0,ix);
  ibbox[1]=Min(iy0,iy);
  ibbox[2]=Max(ix0,ix);
  ibbox[3]=Max(iy0,iy);

  /* disable zrect */
  Xgc->zrect[2]=   Xgc->zrect[3]=0;
  Xgc->graphic_engine->xset_thickness(Xgc,th);
  Xgc->graphic_engine->xset_dash(Xgc,style);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  Xgc->graphic_engine->xset_win_protect(Xgc,FALSE); /* protect against window kill */
  Xgc->graphic_engine->xinfo(Xgc," ");
  Xgc->graphic_engine->force_redraw(Xgc);
}

/* using the Xor mode facility */
#if 0
static void zoom_get_rectangle_std(BCG *Xgc,double *bbox, int *ibbox)
{
  /* Using the mouse to get the new rectangle to fix boundaries */
  int th,th1=1, pixmode,alumode,color,style,fg;
  int ibutton,imask,iwait=FALSE,istr=0,rf;
  double x0,y0,x,y,xl,yl;
  int    ix0,iy0,ix,iy,ixl,iyl;
  if ( Xgc == NULL ) return; 
  rf = Xgc->record_flag ;
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  alumode = Xgc->graphic_engine->xget_alufunction(Xgc);
  th = Xgc->graphic_engine->xget_thickness(Xgc);
  color= Xgc->graphic_engine->xget_pattern(Xgc);
  style = Xgc->graphic_engine->xget_dash(Xgc);
  fg    = Xgc->graphic_engine->xget_foreground(Xgc);
  Xgc->graphic_engine->xset_win_protect(Xgc,TRUE); /* protect against window kill */
  Xgc->record_flag = FALSE;
  Xgc->graphic_engine->xset_thickness(Xgc,th1);
  Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->xset_pattern(Xgc,fg);
 
  /** XXXXXX : a regler pour Win32 in = 6 **/
  Xgc->graphic_engine->scale->xset_alufunction1(Xgc,6);
  Xgc->graphic_engine->xclick(Xgc,"one",&ibutton,&imask,&ix0,&iy0,iwait,FALSE,FALSE,FALSE,istr);
  scale_i2f(Xgc,&x0,&y0,&ix0,&iy0,1);
  x=x0;y=y0;
  ix=ix0;iy=iy0;
  ibutton=-1;
  while ( ibutton == -1 ) 
    {
      /* dessin d'un rectangle */
      zoom_rect(Xgc,x0,y0,x,y);
      if ( pixmode == 1) Xgc->graphic_engine->scale->xset_show(Xgc);
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&imask,&ixl, &iyl,iwait,TRUE,FALSE,FALSE);
      scale_i2f(Xgc,&xl,&yl,&ixl,&iyl,1);
      /* effacement du rectangle */
      zoom_rect(Xgc,x0,y0,x,y);
      if ( pixmode == 1) Xgc->graphic_engine->scale->xset_show(Xgc);
      x=xl;y=yl;
      ix=ixl;iy=iyl;
    }
#ifndef WIN32
  /** XXXX */
  Xgc->graphic_engine->scale->xset_alufunction1(Xgc,3);
#endif
  /* Back to the default driver which must be Rec and redraw the recorded
   * graphics with the new scales 
   */
  bbox[0]=Min(x0,x);
  bbox[1]=Min(y0,y);
  bbox[2]=Max(x0,x);
  bbox[3]=Max(y0,y);
  ibbox[0]=Min(ix0,ix);
  ibbox[1]=Min(iy0,iy);
  ibbox[2]=Max(ix0,ix);
  ibbox[3]=Max(iy0,iy);

  Xgc->graphic_engine->scale->xset_alufunction1(Xgc,alumode);
  Xgc->graphic_engine->xset_thickness(Xgc,th);
  Xgc->graphic_engine->xset_dash(Xgc,style);
  Xgc->graphic_engine->xset_pattern(Xgc,color);
  Xgc->graphic_engine->xset_win_protect(Xgc,FALSE); /* protect against window kill */
  Xgc->graphic_engine->xinfo(Xgc," ");
  Xgc->record_flag = rf;
}
#endif 

void zoom(BCG *Xgc)
{
  int aaint[]={2,10,2,10},flag[]={1,0,0} ;
  if ( Xgc == NULL) return ;
  if ( Xgc->record_flag != TRUE )
    {
      Scistring("\n Use the Rec driver to zoom " );
      return;
    }
  else 
    {
      double bbox[4];
      int ibbox[4];
      zoom_get_rectangle(Xgc,bbox,ibbox);
      Xgc->graphic_engine->clearwindow(Xgc);    
      tape_replay_new_scale(Xgc,Xgc->CurWindow,flag,aaint,bbox,ibbox);
    }
}

void unzoom(BCG *Xgc)
{
  if ( Xgc == NULL) return ;
  if ( Xgc->record_flag != TRUE )
    {
      Scistring("\n Use the Rec driver to unzoom " );
      return;
    }
  else 
    {
      Xgc->graphic_engine->clearwindow(Xgc);
      tape_replay_undo_scale(Xgc,Xgc->CurWindow);
    }
}

#if 0 
static void zoom_rect(BCG *Xgc,double x0,double yy0,double  x,double  y)
{
  double rect[4]= {Min(x0,x),Max(yy0,y),Abs(x0-x),Abs(yy0-y)};
  /*
   *  Win32, warning when using xor mode
   *  colors are changed and black is turned to white
   *  so we must use an other pattern than the black one
   *  inside dbox
   **/
#ifdef WIN32
  int pat;
  pat = Xgc->graphic_engine->xget_pattern(Xgc);
#endif
  Xgc->graphic_engine->scale->drawrectangle(Xgc,rect);
#ifdef WIN32
  Xgc->graphic_engine->xset_pattern(Xgc,pat);
#endif
}
#endif 


/* 
 * here we compute new axis graduation 
 * and changing FRect (enlarge) to fit the new 
 * graduation. 
 * This function is obsolete 
 */

#if 0 

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

#endif 
