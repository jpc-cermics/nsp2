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
 *
 * Axis drawing for 2d plots 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <string.h>
#include <stdio.h>
#include "nsp/math.h"
#include "nsp/graphics-old/Graphics.h"

static double  x_convert (char xy_type,const double x[] ,int i);
static double  y_convert (char xy_type,const double x[] ,int i);
static void NumberFormat (char *str,int k,int a);
static void aplotv1_new(BCG *Xgc,char mode, int grid_color);
static void aplotv2 (BCG *Xgc,char mode, int grid_color);
static void nsp_draw_frame_rectangle(BCG *Xgc) ;
static void Sci_Axis(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, char **str, int subtics, 
		     char *format, int fontsize, int textcolor, int ticscolor, char logflag, int seg_flag, int grid_color);

/**
 * axis_draw_old:
 * @Xgc: a graphic context 
 * @mode: axis drawing mode
 * @scale: axis scale mode
 * 
 * draws axis or only rectangle in a graphic frame.
 * 
 *
 **/

/* mode = strflag[2] ou '1' 
 * scale = strflag[1] 
 *
 */

void axis_draw_old(BCG *Xgc,char mode, char scale, int grid_color)
{
  /* using foreground to draw axis */
  int old_dash,pat, fg;
  char c = mode ; /* (strlen(strflag) >= 3) ? strflag[2] : '1'; */
  fg = Xgc->graphic_engine->xget_foreground(Xgc);
  old_dash = Xgc->graphic_engine->xset_dash(Xgc,1);
  pat = Xgc->graphic_engine->xset_pattern(Xgc,fg);
  switch ( c) 
    {
    case '0' :
      break ;
    case '2' :
      nsp_draw_frame_rectangle(Xgc);
      break;
    default :
      if ( scale  == '5' || scale =='6' )
	{
	  aplotv1_new(Xgc,mode,grid_color);
	}
      else
	{
	  aplotv2(Xgc,mode,grid_color);
	}
      break;
    }
  Xgc->graphic_engine->xset_dash(Xgc,old_dash);
  Xgc->graphic_engine->xset_pattern(Xgc,pat);
}

/*--------------------------------------------------------------
 *  aplot: used to draw a box + x and y ticks and scales 
 *  xmin,ymin,xmax,ymax : are the boundary values
 *  xnax and ynax gives the ticks numbers ex: nax=[3,7];
 *  will give 8 big ticks (7 intervals) with numbers and 
 *  each big interval will be divided in 3 small intervals.
 *----------------------------------------------------------------*/

static void aplotv2(BCG *Xgc,char mode, int grid_color)
{
  char dir = 'l';
  int nx,ny;
  int fontsize=-1,textcolor=-1,ticscolor=-1 ; /*==> use default values  */
  int seg =0;
  double x[3],y[3],x1,y1;
  char c = mode;
  x[0] = Xgc->scales->frect[0]; x[1] = Xgc->scales->frect[2] ; x[2]=Xgc->scales->Waaint1[1];
  y[0]=  Xgc->scales->frect[1]; y[1] = Xgc->scales->frect[3] ; y[2]=Xgc->scales->Waaint1[3];
  switch ( c ) 
    { 
    case '3' : /* right axis */ 
      x1 = x[1]; y1 = y[0]; dir = 'r';
      break;
    case '4' : /* centred axis */
      seg=1;
      x1 = (x[0]+x[1])/2.0;y1=(y[0]+y[1])/2.0;
      break ;
    case '5': /* centred at (0,0) */
      seg=1;
      x1 = y1 = 0.0;
      break;
    case '1' : /* left axis */
    default :
      x1 = x[0]; y1 = y[0];
      break;
    }
  /* x-axis */
  ny=1,nx=3;
  Sci_Axis(Xgc,'d','r',x,&nx,&y1,&ny,NULL,Xgc->scales->Waaint1[0],NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[0],seg,grid_color);
  /* y-axis */
  ny=3,nx=1;
  Sci_Axis(Xgc,dir,'r',&x1,&nx,y,&ny,NULL,Xgc->scales->Waaint1[2],NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[1],seg,grid_color);
  if ( c != '4' && c != '5' )  
    {
      /* frame rectangle */
      nsp_draw_frame_rectangle(Xgc);
    }

}

/* here we use frect to find the axes position 
 * since given axis are not supposed to fit to frect boundaries 
 *
 */

static void aplotv1_new(BCG *Xgc,char mode,int grid_color)
{
  /* we use */
  char dir = 'l';
  char c = mode ;
  int nx,ny,seg=0;
  int fontsize = -1 ,textcolor = -1 ,ticscolor = -1; /* default values */
  double  x1,y1;
  switch ( c ) 
    { 
    case '3' : /* right axis */ 
      x1= Xgc->scales->frect[2];
      y1= Xgc->scales->frect[1];
      dir = 'r';
      break;
    case '4' : /* centred axis */
      seg=1;
      x1= (Xgc->scales->frect[0]+Xgc->scales->frect[2])/2.0;
      y1= (Xgc->scales->frect[1]+Xgc->scales->frect[3])/2.0;
      break ;
    case '5': /* centred at (0,0) */
      seg=1;
      x1 = y1 = 0.0;
      break;
    case '1' : /* left axis */
    default :
      x1= Xgc->scales->frect[0];
      y1= Xgc->scales->frect[1];
      break;
    }
  if ( c != '4' && c != '5' )  
    {
      /* frame rectangle */
      nsp_draw_frame_rectangle(Xgc);
    }
  /* x-axis */
  ny=1,nx=4;
  Sci_Axis(Xgc,'d','i',Xgc->scales->xtics,&nx,&y1,&ny,NULL,Xgc->scales->Waaint1[0],
	   NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[0],seg,grid_color);
  /* y-axis */
  ny=4,nx=1;
  Sci_Axis(Xgc,dir,'i',&x1,&nx,Xgc->scales->ytics,&ny,NULL,Xgc->scales->Waaint1[2],
	   NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[1],seg,grid_color);
}



/*-------------------------------------------------------------
 * Sci_Axis : 
 *   Drawing axis 
 *   pos = 'r' | 'l' | 'u' | 'd' : gives the tics directions 
 *         'r' and 'l' are used for vertical axis 
 *         'u' and 'd' for horizontal axis 
 *   xy_type = 'v' (for vector) or 'r' (for range) 
 *         'v' means that tics position are given by a vector 
 *         'r' means that tics position are in a range i.e given by a vector of size 3 
 *             [min,max,number_of_intervals] 
 *         'i' means that tics positions are in a range given by four number (ints) 
 *             [k1,k2,e,number_of intervals] -> [k1*10^e,k2*10^e] 
 *   x vector of size nx 
 *   y vector of size ny 
 *         if pos = 'r' or 'l' then x must be of size 1 
 *              the size of y i.e ny must be 3 if xy_type == 'r' or 4 if xy_type == 'i'
 *              str if non null gives the string to be used at ticks marks 
 *              it must be then of size ny or of size y[3]+1 (if xy_type == 'r') 
 *              or y[4]+1 (if xy_type == 'i') 
 *         if pos = 'u' or 'd  ' then y must be of size 1 
 *              the size of x i.e nx must be 3 if xy_type == 'r' or 4 if xy_type == 'i'
 *              str if non null gives the string to be used at ticks marks 
 *              it must be then of size ny or of size y[3]+1 (if xy_type == 'r')
 *              or y[4]+1 (if xy_type == 'i') 
 *   str = char *str[] string vector, see above for size constraints 
 *              Warning str must be null terminated 
 *   subtics or subints : number of sub intervals 
 *   format : format for tick marks.
 *            format is only used if str is NULL
 *            if format is null a format is computed else format is used 
 *   fontsize and textcolor : 
 *            XXXXX : to be done 
 *   seg_flag : 0 or 1, flag which control the drawing of the segment associated to the axis 
 *            if 1 the segment is drawn 
 *   grid_color: -1 or the color of an axis to be added.
 *-------------------------------------------------------------*/


void nsp_axis_old(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, char **str, int subtics, char *format, int fontsize, int textcolor, int ticscolor, char logflag, int seg_flag, int grid_color)
{
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    nsp_gengine_record_old.store_SciAxis(Xgc,pos,xy_type,x,nx,y,ny,str,subtics,format,fontsize,textcolor,ticscolor,logflag,seg_flag);
  Sci_Axis(Xgc,pos,xy_type,x,nx,y,ny,str,subtics,format,fontsize,textcolor,ticscolor,logflag,seg_flag,grid_color);
}


static void nsp_axis_grid(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, 
			  int grid_color, char logflag, int seg_flag, int Nx,int Ny);

static void Sci_Axis(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, char **str, int subtics, 
		     char *format, int fontsize, int textcolor, int ticscolor, char logflag, int seg_flag, int grid_color)
{
  int Nx=0,Ny=0;
  double angle=0.0,vxx,vxx1,xd,yd,d_barlength,str_offset;
  int vx[2],vy[2],xm[2],ym[2];
  char c_format[5];
  int flag=0,xx=0,yy=0,posi[2],rect[4];
  int i,barlength;
  int ns=2,style=0,iflag=0;
  int fontid[2],fontsize_kp,logrect[4],smallersize=0,color_kp=0;
  
  /* Modified by POLPOTH09042001 Mon Apr  9 08:59:10 MET DST 2001
   * If  zero ticks are requested, exit
   */

  if (*nx==3) if (x[2]==0.0) return;
  if (*ny==3) if (y[2]==0.0) return;
      
  Xgc->graphic_engine->xget_font(Xgc,fontid);
  fontsize_kp = fontid[1] ;

  if ( fontsize != -1 ) 
    {
      fontid[1] = fontsize ;
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1]);
    }
  if ( textcolor != -1 || ticscolor != -1 ) 
    {
      color_kp = Xgc->graphic_engine->xget_pattern(Xgc);
    }

  if (logflag == 'l' )
    {
      Xgc->graphic_engine->boundingbox(Xgc,"10",xx,yy,logrect);
      smallersize=fontid[1]-2;
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],smallersize);
    }
  
  /* Real to Pixel values */
  switch ( xy_type ) 
    {
    case 'v' : Nx= *nx; Ny= *ny; break;
    case 'r' :
      switch ( pos ) {
      case 'u' : case 'd' : Nx = (int) x[2]+1; break;
      case 'r' : case 'l' : Ny = (int) y[2]+1; break;
      }
      break;
    case 'i' : 
      switch ( pos ) {
      case 'u' : case 'd' : Nx = (int) x[3]+1; break;
      case 'r' : case 'l' : Ny = (int) y[3]+1; break;
      }
      break;
    default: 
      sciprint("Sci_Axis: wrong type argument xy_type\r\n");
    }

  /* Note that in that case xy_type = 'i' we can possibly 
   * have x[3] or y[3] equal to zero which means that we 
   * cannot distinguish the min and the max on the given interval
   */ 
  
  switch (pos ) 
    {
    case 'u' : 
    case 'd' :
      /* Used for an horizontal axis: 
       * if the sign of d_barlength is changed ticks will
       * go in the direction of the strings 
       */
      barlength = Xgc->scales->WIRect1[3]/40.0;
      d_barlength = barlength/Xgc->scales->Wscy1;
      str_offset = Xgc->scales->WIRect1[3]/60.0/Xgc->scales->Wscy1;
      
      /* compute a format 
       */
      if (str == NULL && format == NULL )  
	switch (xy_type ) {
	case 'v' : nsp_grformat_e1(c_format,x,Nx);break;
	case 'r' : nsp_grformat_e (c_format,x[0],x[1],(x[1]-x[0])/x[2]);break;
	}
      if ( seg_flag == 1) 
	{
	  /* the horizontal segment */
	  xd = x_convert(xy_type, x , 0);
	  vx[0] =  inint(XScaleR_d(xd,y[0]));
	  ym[0] = vy[0] =  inint(YScaleR_d(xd,y[0]));
	  xd = x_convert(xy_type, x , Nx-1);
	  vx[1] =  inint(XScaleR_d(xd,y[0]));
	  vy[1] =  inint(YScaleR_d(xd,y[0]));
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,ticscolor);
	  Xgc->graphic_engine->drawsegments(Xgc,vx, vy, ns,&style,iflag);
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}
      
      /* loop on the ticks */
      for (i=0 ; i < Nx ; i++)
	{ 
	  char foo[100];
	  vxx = x_convert(xy_type,x,i);
	  if ( str != NULL)  
	    {
	      sprintf(foo,"%s",str[i]);
	    }
	  else if ( format == NULL) 
	    {
	      /*defaults format **/
	      if  ( xy_type == 'i') 
		{
		  if ( x[3] == 0 ) 
		    NumberFormat(foo,(int) (x[0]), ((int) x[2]));
		  else
		    NumberFormat(foo,((int) (x[0] + i*(x[1]-x[0])/x[3])), ((int) x[2]));
		}
	      else 
		sprintf(foo,c_format,vxx);
	    }
	  else 
	    sprintf(foo,format,vxx);
	  Xgc->graphic_engine->boundingbox(Xgc,foo,xx,yy,rect);
	  
	  /* tick is computed in vx,vy and string is displayed at posi[0],posi[1] position */
	  vx[0] =  inint(XScaleR_d(vxx,y[0]));
	  vy[0] =  inint(YScaleR_d(vxx,y[0]));
	  if ( pos == 'd' ) 
	    {
	      /* if d_barlength is > 0: ticks are going up and string are displayed 
	       * below 
	       */
	      xd = vxx; 
	      yd = y[0] + d_barlength;
	      vx[1]= inint(XScaleR_d(xd,yd));
	      vy[1]= inint(YScaleR_d(xd,yd));
	      xd = vxx - (rect[2]/2.0/Xgc->scales->Wscx1);
	      if ( d_barlength > 0 ) 
		yd = y[0] - str_offset - rect[3]/Xgc->scales->Wscy1; 
	      else
		yd = y[0] - str_offset + d_barlength - rect[3]/Xgc->scales->Wscy1; 
	      posi[0] = inint(XScaleR_d(xd,yd));
	      posi[1] = inint(YScaleR_d(xd,yd));
	    }
	  else 
	    { 
	      xd = vxx; 
	      yd = y[0] - d_barlength;
	      vx[1]= inint(XScaleR_d(xd,yd));
	      vy[1]= inint(YScaleR_d(xd,yd));
	      xd = vxx - (rect[2]/2.0/Xgc->scales->Wscx1);
	      if ( d_barlength > 0 ) 
		yd = y[0] + str_offset;
	      else
		yd = y[0] + str_offset - d_barlength;
	      posi[0] = inint(XScaleR_d(xd,yd));
	      posi[1] = inint(YScaleR_d(xd,yd));
	    }
	  if ( textcolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,textcolor);
	  Xgc->graphic_engine->displaystring(Xgc,foo,posi[0],posi[1],flag,angle);
	  if ( logflag == 'l' )
	    {
	      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1]);
	      Xgc->graphic_engine->displaystring(Xgc,"10",(posi[0] -= logrect[2],posi[0]),(posi[1] += logrect[3],posi[1]),flag,angle);
	      Xgc->graphic_engine->xset_font(Xgc,fontid[0],smallersize);
	    }
	  if ( textcolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,ticscolor);
	  
	  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,&style,iflag);
	  /* subtics */
	  if ( i < Nx-1 ) 
	    {
	      int j;
	      double dx ; 
	      vxx1= x_convert(xy_type,x,i+1);
	      dx = (vxx1-vxx)/subtics;
	      for ( j = 1 ; j < subtics; j++) 
		{
		  xd = vxx+dx*j;
		  vx[0] = inint(XScaleR_d(xd,y[0]));
		  vy[0] = inint(YScaleR_d(xd,y[0]));
		  yd = (pos == 'd') ? y[0] + d_barlength/2.0 : y[0] - d_barlength/2.0;
		  vx[1]= inint(XScaleR_d(xd,yd));
		  vy[1]= inint(YScaleR_d(xd,yd));
		  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,&style,iflag);
		}
	    }
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}
      break;
    case 'r' : 
    case 'l' :
      /* Vertical axes */
      barlength = Xgc->scales->WIRect1[2]/40.0;
      d_barlength = barlength/Xgc->scales->Wscx1;
      str_offset = Xgc->scales->WIRect1[2]/60.0/Xgc->scales->Wscx1;
      if (str == NULL &&  format == NULL )  
	switch (xy_type ) {
	case 'v' : nsp_grformat_e1(c_format,y,Ny);break;
	case 'r' : nsp_grformat_e(c_format,y[0],y[1],(y[1]-y[0])/y[2]);break;
	}
      if ( seg_flag == 1)
	{
	  /* the vertical segment */
	  yd = y_convert(xy_type, y , 0);
	  vx[0] =  inint(XScaleR_d(x[0],yd));
	  vy[0] =  inint(YScaleR_d(x[0],yd));
	  yd = y_convert(xy_type, y , Ny-1);
	  vx[1] =  inint(XScaleR_d(x[0],yd));
	  vy[1] = xm[0]= inint(YScaleR_d(x[0],yd));
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,ticscolor);
	  Xgc->graphic_engine->drawsegments(Xgc,vx, vy, ns,&style,iflag);
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}
      /* loop on the ticks */
      for (i=0 ; i < Ny ; i++)
	{ 
	  char foo[100];
	  vxx = y_convert(xy_type,y,i);
	  if ( str != NULL)  
	    sprintf(foo,"%s",str[i]);
	  else if ( format == NULL)
	    {
	      if ( xy_type == 'i') 
		{
		  if ( y[3] == 0 ) 
		    NumberFormat(foo,((int) (y[0])), ((int) y[2]));
		  else
		    NumberFormat(foo,((int) (y[0] + i*(y[1]-y[0])/y[3])), ((int) y[2]));
		}
	      else 
		sprintf(foo,c_format,vxx);
	    }
	  else 
	    sprintf(foo,format,vxx);

	  Xgc->graphic_engine->boundingbox(Xgc,foo,xx,yy,rect);

	  /* tick is computed in vx,vy and string is displayed at posi[0],posi[1] position */
	  
	  vx[0] = inint(XScaleR_d(x[0],vxx));
	  vy[0] = inint(YScaleR_d(x[0],vxx));
	  if ( pos == 'r' ) 
	    {
	      xd = x[0] - d_barlength;
	      vx[1] = inint(XScaleR_d(xd,vxx));
	      vy[1] = inint(YScaleR_d(xd,vxx));
	      if ( d_barlength > 0) 
		xd = x[0] + str_offset;
	      else 
		xd = x[0] - d_barlength + str_offset;
	      yd = vxx - rect[3]/2.0/Xgc->scales->Wscy1;
	      posi[0]= inint(XScaleR_d(xd,yd));
	      posi[1]= inint(YScaleR_d(xd,yd));
	    }
	  else 
	    { 
	      xd = x[0] + d_barlength;
	      vx[1] = inint(XScaleR_d(xd,vxx));
	      vy[1] = inint(YScaleR_d(xd,vxx));
	      if ( d_barlength > 0) 
		xd = x[0] - str_offset - rect[2]/Xgc->scales->Wscx1;
	      else 
		xd = x[0] + d_barlength -str_offset - rect[2]/Xgc->scales->Wscx1;
	      yd = vxx - rect[3]/2.0/Xgc->scales->Wscy1;
	      posi[0]= inint(XScaleR_d(xd,yd));
	      posi[1]= inint(YScaleR_d(xd,yd));
	    }

	  if ( textcolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,textcolor);
	  Xgc->graphic_engine->displaystring(Xgc,foo,posi[0],posi[1],flag,angle);
	  if ( logflag == 'l' )
	    {
	      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1]);
	      Xgc->graphic_engine->displaystring(Xgc,"10",(posi[0] -= logrect[2],posi[0]),
						 (posi[1] += logrect[3],posi[1]),flag,angle);
	      Xgc->graphic_engine->xset_font(Xgc,fontid[0],smallersize);
	    }
	  if ( textcolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);

	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,ticscolor);
	  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,&style,iflag);
	  /* subtics */
	  if ( i < Ny-1 ) 
	    {
	      int j;
	      double dy ; 
	      vxx1= y_convert(xy_type,y,i+1);
	      dy = (vxx1-vxx)/subtics;
	      for ( j = 1 ; j < subtics; j++) 
		{
		  yd= vxx+dy*j;
		  xd= x[0];
		  vx[0] = inint(XScaleR_d(xd,yd));
		  vy[0] = inint(YScaleR_d(xd,yd));
		  xd = ( pos == 'r' ) ? x[0] -d_barlength/2.0: x[0] +d_barlength/2.0;
		  vx[1]= inint(XScaleR_d(xd,yd));
		  vy[1]= inint(YScaleR_d(xd,yd));
		  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,&style,iflag);
		}
	    }
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}
      break;
    }
  /* reset font to its current size */ 
  if ( fontsize != -1 || logflag == 'l' )
    {
      fontid[1] = fontsize_kp;
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1]);
    }
  /* reset to current color */
  if ( textcolor != -1 || ticscolor != -1 ) 
    {
      Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
    }
  if ( grid_color != -1 ) 
    nsp_axis_grid(Xgc,pos,xy_type, x,nx, y, ny, grid_color, logflag,seg_flag,Nx,Ny);
  
}

/* draw a grid which follows the main ticks. 
 */

static void nsp_axis_grid(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, 
			  int grid_color, char logflag, int seg_flag, int Nx,int Ny)
{

  double vxx,xd,yd,d_barlength;
  int vx[2],vy[2],i, ns=2,style=0,iflag=0, color_kp=0;

  if (*nx==3) if (x[2]==0.0) return;
  if (*ny==3) if (y[2]==0.0) return;
  
  if ( grid_color != -1 ) 
    {
      color_kp = Xgc->graphic_engine->xget_pattern(Xgc);
    }
  
  if ( grid_color == 0)
    {
      /* last +3 is a light gray */
      grid_color = Xgc->graphic_engine->xget_last(Xgc)+3;
    }

  /* Note that in that case xy_type = 'i' we can possibly 
   * have x[3] or y[3] equal to zero which means that we 
   * cannot distinguish the min and the max on the given interval
   */ 
  
  switch (pos ) 
    {
    case 'u' : 
    case 'd' :
      /* Used for an horizontal axis: 
       * if the sign of d_barlength is changed ticks will
       * go in the direction of the strings 
       */
      d_barlength = Xgc->scales->WIRect1[3]/Xgc->scales->Wscy1;
      
      /* loop on the ticks */
      for (i=0 ; i < Nx ; i++)
	{ 
	  vxx = x_convert(xy_type,x,i);
	  /* tick is computed in vx,vy and string is displayed at posi[0],posi[1] position */
	  vx[0] =  inint(XScaleR_d(vxx,y[0]));
	  vy[0] =  inint(YScaleR_d(vxx,y[0]));
	  xd = vxx; 
	  yd = y[0] + d_barlength;
	  vx[1]= inint(XScaleR_d(xd,yd));
	  vy[1]= inint(YScaleR_d(xd,yd));
	  if ( grid_color != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,grid_color);
	  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,&style,iflag);
	  if ( grid_color != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}
      break;
    case 'r' : 
    case 'l' :
      /* Vertical axes */
      d_barlength = Xgc->scales->WIRect1[2]/Xgc->scales->Wscx1;
      /* loop on the ticks */
      for (i=0 ; i < Ny ; i++)
	{ 
	  vxx = y_convert(xy_type,y,i);
	  vx[0] = inint(XScaleR_d(x[0],vxx));
	  vy[0] = inint(YScaleR_d(x[0],vxx));
	  xd = x[0] + d_barlength;
	  vx[1] = inint(XScaleR_d(xd,vxx));
	  vy[1] = inint(YScaleR_d(xd,vxx));
	  if ( grid_color != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,grid_color);
	  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,&style,iflag);
	  if ( grid_color != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}
      break;
    }
  /* reset to current color */
  if ( grid_color != -1 ) 
    {
      Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
    }
}



/* from double to pixel */ 

static double  x_convert(char xy_type,const double x[], int i)
{
  switch ( xy_type ) { 
  case 'v' :  return x[i];
  case 'r' :  return x[0]+i*(x[1]-x[0])/x[2];
  case 'i' :  return (x[3]==0) ? exp10(x[2])*(x[0])  
      : exp10(x[2])*(x[0] + i*(x[1]-x[0])/x[3]);
  }
  return 0.0;
}

static double y_convert(char xy_type,const double y[], int i)
{
  switch ( xy_type ) { 
  case 'v' :  return y[i]; 
  case 'r' :  return y[0]+i*(y[1]-y[0])/y[2];
  case 'i' :  return (y[3]==0) ? exp10(y[2])*(y[0]) 
      : exp10(y[2])*(y[0] + i*(y[1]-y[0])/y[3]); 
  }
  return 0.0; 
}



/* Format pour imprimer un nombre de la forme k10^a */

static void NumberFormat(char *str, int k, int a)
{
  if ( k==0) 
    {
      sprintf(str,"0");
    }
  else
    {
      switch (a) 
	{
	case -1: sprintf(str,"%.1f",(double)k/10.0);break;
	case -2: sprintf(str,"%.2f",(double)k/100.0);break;
	case 0 : sprintf(str,"%d",(int)k);break;
	case 1 : sprintf(str,"%d0",(int)k);break;
	case 2 : sprintf(str,"%d00",(int)k);break;
	default: sprintf(str,"%de%d",(int)k,(int)a) ;break;
	}
    }
}


static void nsp_draw_frame_rectangle(BCG *Xgc) 
{
  if ( Xgc->scales->cosa == 1.0 ) 
    {
      Xgc->graphic_engine->drawrectangle(Xgc,Xgc->scales->WIRect1);
    }
  else 
    {
      double x[4],y[4];
      x[0]= Xgc->scales->frect[0];
      y[0]= Xgc->scales->frect[1];
      x[1]= Xgc->scales->frect[2];
      y[1]= y[0];
      x[2]= x[1];
      y[2]= Xgc->scales->frect[3];
      x[3]= x[0];
      y[3]= y[2];
      Xgc->graphic_engine->scale->drawpolyline(Xgc,x,y,4,1);
    }
}

