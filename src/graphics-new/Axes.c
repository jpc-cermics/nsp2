/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/
/*------------------------------------------------------------------------
 * Axis drawing for 2d plots 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <string.h>
#include <stdio.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
/* #include "nsp/graphics/PloEch.h" */

static double  x_convert (char xy_type,double x[] ,int i);
static double  y_convert (char xy_type,double x[] ,int i);
static void NumberFormat (char *str,int k,int a);
static void aplotv1 (BCG *Xgc,char*);
static void aplotv1_new(BCG *Xgc,char *strflag);
static void aplotv2 (BCG *Xgc,char*);

/*--------------------------------------------------------------
 * Draw Axis or only rectangle
 *----------------------------------------------------------------*/

void axis_draw(BCG *Xgc,char *strflag)
{
  /* using foreground to draw axis */
  int old_dash,pat, fg;
  char c = (strlen(strflag) >= 3) ? strflag[2] : '1';
  fg = Xgc->graphic_engine->xget_foreground(Xgc);
  old_dash = Xgc->graphic_engine->xset_dash(Xgc,1);
  pat = Xgc->graphic_engine->xset_pattern(Xgc,fg);
  switch ( c) 
    {
    case '0' :
      break ;
    case '2' :
      Xgc->graphic_engine->drawrectangle(Xgc,Xgc->scales->WIRect1);
      break;
    default :
      if ( strflag[1] == '5' || strflag[1] =='6' )
	{
	  aplotv1_new(Xgc,strflag);
	}
      else
	{
	  aplotv2(Xgc,strflag);
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

static void aplotv2(BCG *Xgc,char *strflag)
{
  char dir = 'l';
  int nx,ny;
  int fontsize=-1,textcolor=-1,ticscolor=-1 ; /*==> use default values  */
  int seg =0;
  double x[3],y[3],x1,y1;
  char c = (strlen(strflag) >= 3) ? strflag[2] : '1';
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
  if ( c != '4' && c != '5' )  
    /** frame rectangle **/
    Xgc->graphic_engine->drawrectangle(Xgc,Xgc->scales->WIRect1);
  /** x-axis **/
  ny=1,nx=3;
  Sci_Axis(Xgc,'d','r',x,&nx,&y1,&ny,NULL,Xgc->scales->Waaint1[0],NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[0],seg);
  /** y-axis **/
  ny=3,nx=1;
  Sci_Axis(Xgc,dir,'r',&x1,&nx,y,&ny,NULL,Xgc->scales->Waaint1[2],NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[1],seg);
}

/* unused  */

static void aplotv1(BCG *Xgc,char *strflag)
{
  char dir = 'l';
  char c = (strlen(strflag) >= 3) ? strflag[2] : '1';
  int nx,ny,seg=0;
  int fontsize = -1 ,textcolor = -1 ,ticscolor = -1; /* default values */
  double  x1,y1;
  switch ( c ) 
    { 
    case '3' : /* right axis */ 
      x1= Xgc->scales->xtics[1]*exp10(Xgc->scales->xtics[2]);
      y1= Xgc->scales->ytics[0]*exp10(Xgc->scales->ytics[2]);
      dir = 'r';
      break;
    case '4' : /* centred axis */
      seg=1;
      x1= (Xgc->scales->xtics[0]+Xgc->scales->xtics[1])*exp10(Xgc->scales->xtics[2])/2.0;
      y1= (Xgc->scales->ytics[0]+Xgc->scales->xtics[1])*exp10(Xgc->scales->ytics[2])/2.0;
      break ;
    case '5': /* centred at (0,0) */
      seg=1;
      x1 = y1 = 0.0;
      break;
    case '1' : /* left axis */
    default :
      x1= Xgc->scales->xtics[0]*exp10(Xgc->scales->xtics[2]);
      y1= Xgc->scales->ytics[0]*exp10(Xgc->scales->ytics[2]);
      break;
    }
  if ( c != '4' && c != '5' )  
    /** frame rectangle **/
    Xgc->graphic_engine->drawrectangle(Xgc,Xgc->scales->WIRect1);
  /** x-axis **/
  ny=1,nx=4;
  Sci_Axis(Xgc,'d','i',Xgc->scales->xtics,&nx,&y1,&ny,NULL,Xgc->scales->Waaint1[0],
	   NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[0],seg);
  /** y-axis **/
  ny=4,nx=1;
  Sci_Axis(Xgc,dir,'i',&x1,&nx,Xgc->scales->ytics,&ny,NULL,Xgc->scales->Waaint1[2],
	   NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[1],seg);
}

/* here we use frect to find the axes position 
 * since given axis are not supposed to fit to frect boundaries 
 *
 */

static void aplotv1_new(BCG *Xgc,char *strflag)
{
  /* we use */
  char dir = 'l';
  char c = (strlen(strflag) >= 3) ? strflag[2] : '1';
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
    /** frame rectangle **/
    Xgc->graphic_engine->drawrectangle(Xgc,Xgc->scales->WIRect1);
  /** x-axis **/
  ny=1,nx=4;
  Sci_Axis(Xgc,'d','i',Xgc->scales->xtics,&nx,&y1,&ny,NULL,Xgc->scales->Waaint1[0],
	   NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[0],seg);
  /** y-axis **/
  ny=4,nx=1;
  Sci_Axis(Xgc,dir,'i',&x1,&nx,Xgc->scales->ytics,&ny,NULL,Xgc->scales->Waaint1[2],
	   NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[1],seg);
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
 *             [k1,k2,e,number_of intervale] -> [k1*10^e,k2*10^e] 
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
 *-------------------------------------------------------------*/


void sci_axis(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, char **str, int subtics, char *format, int fontsize, int textcolor, int ticscolor, char logflag, int seg_flag)
{
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_SciAxis(Xgc,pos,xy_type,x,nx,y,ny,str,subtics,format,fontsize,textcolor,ticscolor,logflag,seg_flag);
  Sci_Axis(Xgc,pos,xy_type,x,nx,y,ny,str,subtics,format,fontsize,textcolor,ticscolor,logflag,seg_flag);
}

void Sci_Axis(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, char **str, int subtics, char *format, int fontsize, int textcolor, int ticscolor, char logflag, int seg_flag)
{
  int Nx,Ny;
  double angle=0.0,vxx,vxx1;
  int vx[2],vy[2],xm[2],ym[2];
  char c_format[5];
  int flag=0,xx=0,yy=0,posi[2],rect[4];
  int i,barlength;
  int ns=2,style=0,iflag=0;
  int fontid[2],fontsize_kp,logrect[4],smallersize,color_kp;
  
  /* Modified by POLPOTH09042001 Mon Apr  9 08:59:10 MET DST 2001 */
  /* If  zero ticks are requested, exit */
  
  if (*nx==3) if (x[2]==0.0) return;
  if (*ny==3) if (y[2]==0.0) return;
 
  /* End of modified code */
  
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
  /** Real to Pixel values **/
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
  switch (pos ) 
    {
    case 'u' : 
    case 'd' :
      /** Horizontal axes **/
      barlength = Xgc->scales->WIRect1[3]/50.0;
      /** compute a format **/
      if (str == NULL && format == NULL )  
	switch (xy_type ) {
	case 'v' : ChoixFormatE1(c_format,x,Nx);break;
	case 'r' : ChoixFormatE (c_format,x[0],x[1],(x[1]-x[0])/x[2]);break;
	}
      /** the horizontal segment **/
      vx[0] =  XScale(x_convert(xy_type, x , 0));
      vx[1] =  XScale(x_convert(xy_type, x , Nx-1));
      vy[0]= vy[1] = ym[0] = YScale(y[0]);
      if ( seg_flag == 1) 
	{
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,ticscolor);
	  Xgc->graphic_engine->drawsegments(Xgc,vx, vy, ns,&style,iflag);
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}

      /** loop on the ticks **/
      for (i=0 ; i < Nx ; i++)
	{ 
	  char foo[100];
	  vxx = x_convert(xy_type,x,i);
	  if ( str != NULL)  
	    sprintf(foo,"%s",str[i]);
	  else if ( format == NULL) 
	    {
	      /*defaults format **/
	      if  ( xy_type == 'i') 
		NumberFormat(foo,((int) (x[0] + i*(x[1]-x[0])/x[3])),
			     ((int) x[2]));
	      else 
		sprintf(foo,c_format,vxx);
	    }
	  else 
	    sprintf(foo,format,vxx);
	  Xgc->graphic_engine->boundingbox(Xgc,foo,xx,yy,rect);

	  /* tick is computed in vx,vy and string is displayed at posi[0],posi[1] position */

	  vx[0] = vx[1] = xm[0] =  XScale(vxx);
	  posi[0] = inint( xm[0] -rect[2]/2.0);

	  if ( pos == 'd' ) 
	    {
	      posi[1]=inint( ym[0] + 1.2*barlength + rect[3]);
	      vy[0]= ym[0];vy[1]= ym[0] - barlength ;
	    }
	  else 
	    { 
	      posi[1]=inint( ym[0] - 1.2*barlength);
	      vy[0]= ym[0];vy[1]= ym[0] + barlength;
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
		  vx[0] = vx[1] = XScale(vxx+dx*j);
		  if ( pos == 'd' ) 
		    { vy[0]= ym[0];vy[1]= ym[0] - barlength/2.0 ; }
		  else 
		    { vy[0]= ym[0];vy[1]= ym[0] + barlength/2.0; }
		  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,&style,iflag);
		}
	    }
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}
      break;
    case 'r' : 
    case 'l' :
      /** Vertical axes **/
      barlength = Xgc->scales->WIRect1[2]/75.0;
      if (str == NULL &&  format == NULL )  
	switch (xy_type ) {
	case 'v' : ChoixFormatE1(c_format,y,Ny);break;
	case 'r' : ChoixFormatE(c_format,y[0],y[1],(y[1]-y[0])/y[2]);break;
	}
      /** the vertical segment **/
      vy[0] =  YScale(y_convert(xy_type, y , 0));
      vy[1] =  YScale(y_convert(xy_type, y , Ny-1));
      vx[0]= vx[1] = xm[0]= XScale(x[0]);
      if ( seg_flag == 1) 
	{
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,ticscolor);
	  Xgc->graphic_engine->drawsegments(Xgc,vx, vy, ns,&style,iflag);
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_pattern(Xgc,color_kp);
	}
      /** loop on the ticks **/
      for (i=0 ; i < Ny ; i++)
	{ 
	  char foo[100];
	  vxx = y_convert(xy_type,y,i);
	  if ( str != NULL)  
	    sprintf(foo,"%s",str[i]);
	  else if ( format == NULL)
	    {
	      if ( xy_type == 'i') 
		NumberFormat(foo,((int) (y[0] + i*(y[1]-y[0])/y[3])),
			     ((int) y[2]));
	      else 
		sprintf(foo,c_format,vxx);
	    }
	  else 
	    sprintf(foo,format,vxx);

	  Xgc->graphic_engine->boundingbox(Xgc,foo,xx,yy,rect);

	  /* tick is computed in vx,vy and string is displayed at posi[0],posi[1] position */

	  vy[0]= vy[1] = ym[0] = YScale(vxx);
	  posi[1]=inint( ym[0] +rect[3]/2.0);
	  if ( pos == 'r' ) 
	    {
	      posi[0]=inint( xm[0] + 1.2*barlength);
	      vx[0]= xm[0];vx[1]= xm[0] - barlength;
	    }
	  else 
	    { 
	      posi[0]=inint(xm[0] - 1.2*barlength - rect[2]);
	      vx[0]= xm[0];vx[1]= xm[0] + barlength;
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
		  vy[0] = vy[1] = YScale(vxx+dy*j);
		  if ( pos == 'r' ) 
		    { vx[0]= xm[0];vx[1]= xm[0] - barlength/2.0 ; }
		  else 
		    { vx[0]= xm[0];vx[1]= xm[0] +  barlength/2.0; }
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

}

/* from double to pixel */ 

static double  x_convert(char xy_type, double *x, int i)
{
  switch ( xy_type ) { 
  case 'v' :  return x[i];
  case 'r' :  return x[0]+i*(x[1]-x[0])/x[2];
  case 'i' :  return exp10(x[2])*(x[0] + i*(x[1]-x[0])/x[3]);
  }
  return 0.0;
}

static double y_convert(char xy_type, double *y, int i)
{
  switch ( xy_type ) { 
  case 'v' :  return y[i]; 
  case 'r' :  return y[0]+i*(y[1]-y[0])/y[2];
  case 'i' :  return exp10(y[2])*(y[0] + i*(y[1]-y[0])/y[3]); 
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
