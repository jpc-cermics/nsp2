/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "Graphics.h"
/* #include "PloEch.h" */

#define spINSIDE_SPARSE
#if defined(THINK_C) || defined (__MWERKS__)
#include "::sparse:spConfig.h" 
#else
#include "../sparse/spConfig.h"
#endif

/*--------------------------------------------------------------------
 *  C2F(plot2d)(x,y,n1,n2,style,strflag,legend,brect,aaint,lstr1,lstr2)
 *  
 *  Draw *n1 curves of *n2 points each
 *  (x[i+(*n2)*j] ,y[i+(*n2)*j]) Double values giving the point
 *  position of point i of curve j (i=0,*n2-1 j=0,*n1-1)
 *
 *  style[*n1]-> give the style to use for each curve 
 *     if style is positive --> a mark is used (mark id = style[i])
 *     if style is strictly negative --> a dashed line is used 
 *        (dash id = abs(style[i])
 *     if there's only one curve, style can be of type style[0]=style,
 *     style[1]=pos ( pos in [1,6]) 
 *     pos give the legend position (1 to 6) (this can be iteresting
 *     if you want to superpose curves with different legends by 
 *     calling plot2d more than one time.
 *
 *  strflag[3] is a string
 *  
 *     if strflag[0] == '1' then legends are added 
 *        legend = "leg1@leg2@....@legn"; gives the legend for each curve
 *	else no legend
 *
 *     if strflag[1] == '1' then  the values of brect are used to fix 
 *        the drawing boundaries :  brect[]= <xmin,ymin,xmax,ymax>;
 *	if strflag[1] == '2' then the values  are computed from data
 *	else if strflag[1]=='0' the previous values 
 *                (previous call or defaut values) are used 
 *
 *     if  strflag[2] == '1' ->then an axis is added
 *        the number of intervals 
 *        is specified by the vector aaint[4] of integers 
 *	   <aaint[0],aaint[1]> specifies the x-axis number of  points 
 *	   <aaint[2],aaint[3]> same for y-axis
 *     if  strflag[2] == '2' -> no axis, only a box around the curves
 *     else no box and no axis 
 *
 * lstr* : unused ( but used by Fortran ) 
 *--------------------------------------------------------------------------*/
  
int C2F(plot2d)(double x[],double y[],int *n1,int *n2,int style[],char *strflag,
		char *legend,double brect[],int aaint[])
{
  int n;
  integer *xm,*ym;

  /* Storing values if using the Record driver */
  /* Boundaries of the frame */
  
  update_frame_bounds(0,"gnn",x,y,n1,n2,aaint,strflag,brect);

  if (nsp_gengine1.get_driver()=='R') 
    store_Plot1("gnn",x,y,n1,n2,style,strflag,legend,brect,aaint);

  /* Allocation */
  n = (*n1)*(*n2) ; 
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
      scale_f2i(x,y,xm,ym,n);
      /** Drawing axes **/
    }
  axis_draw(strflag);
  /** Drawing the curves **/
  if ( n != 0 ) 
    {
      frame_clip_on();
      nsp_gengine->drawpolylines(xm,ym,style,*n1,*n2);
      frame_clip_off();
      /** Drawing the Legends **/
      if ((int)strlen(strflag) >=1  && strflag[0] == '1')
	Legends(style,n1,legend); 
    }
  return(0);
}


/*--------------------------------------------------------------------
 * add a grid to a 2D plot
 *--------------------------------------------------------------------*/

int C2F(xgrid)(style)
     integer *style;
{
  integer closeflag=0,n=2,vx[2],vy[2],i,j;
  double pas;
  int pat;
  /* Recording command */
  if (nsp_gengine1.get_driver()=='R') store_Grid(style);
  /* changes dash style if necessary */
  pat = nsp_gengine->xset_pattern(*style);
  /** Get current scale **/
  pas = ((double) current_scale.WIRect1[2]) / ((double) current_scale.Waaint1[1]);
  /** x-axis grid (i.e vertical lines ) */
  for ( i=0 ; i < current_scale.Waaint1[1]; i++)
    {
      vy[0]=current_scale.WIRect1[1];
      vy[1]=current_scale.WIRect1[1]+current_scale.WIRect1[3];
      vx[0]=vx[1]= current_scale.WIRect1[0] + inint( ((double) i)*pas);
      if ( i!=0) nsp_gengine->drawpolyline(vx, vy,n,closeflag);
      if (current_scale.logflag[0] == 'l') 
	{
	  int jinit=1;
	  if ( i== 0 ) jinit=2; /* no grid on plot boundary */
	  for (j= jinit; j < 10 ; j++)
	    {
	      vx[0]=vx[1]= current_scale.WIRect1[0] + inint( ((double) i)*pas)+ inint(log10(((double)j))*pas);
	      nsp_gengine->drawpolyline( vx, vy,n,closeflag);
	    }
	}
    }
  /** y-axis grid (i.e horizontal lines ) **/
  pas = ((double) current_scale.WIRect1[3]) / ((double) current_scale.Waaint1[3]);
  for ( i=0 ; i < current_scale.Waaint1[3]; i++)
    {
      vx[0]=current_scale.WIRect1[0];
      vx[1]=current_scale.WIRect1[0]+current_scale.WIRect1[2];
      vy[0]=vy[1]= current_scale.WIRect1[1] + inint( ((double) i)*pas);
      if (i!=0)  nsp_gengine->drawpolyline( vx, vy,n,closeflag);
      if (current_scale.logflag[1] == 'l') 
	{
	  int jinit=1;
	  if ( i== current_scale.Waaint1[3]-1 ) jinit=2; /* no grid on plot boundary */
	  for (j= jinit; j < 10 ; j++)
	    {
	      vy[0]=vy[1]= current_scale.WIRect1[1] + inint( ((double) i+1)*pas)- inint(log10(((double)j))*pas);
	       nsp_gengine->drawpolyline( vx, vy,n,closeflag);
	    }
	}
    }
  nsp_gengine->xset_pattern(pat);
  return(0);
}


/*---------------------------------------------------------------------
 * update_frame_bounds : 
 * modify according to strflag current_scale using given data 
 * output : FRect,aaint,strflag are modified 
 *----------------------------------------------------*/

void update_frame_bounds(cflag, xf, x, y, n1, n2, aaint, strflag, FRect)
     int cflag;
     char *xf;
     double *x, *y;
     integer *n1, *n2, *aaint;
     char *strflag;
     double FRect[4];
{
  int Xdec[3],Ydec[3],i,redraw=0;
  double xmin=0.0,xmax=10.0,ymin= 0.0,ymax= 10.0;
  double hx,hy,hx1,hy1;
  /* cflag is used when using contour */
  int size_x = (cflag == 1) ? (*n1) : (*n1)*(*n2) ;
  int size_y = (cflag == 1) ? (*n2) : (*n1)*(*n2) ;
  char c;
  if ((int)strlen(strflag) < 2) return ;
  /* 
   * min,max using brect or x,y according to flags 
   */
  switch (strflag[1])
    {
    case '0': return ; 
    case '1' : case '3' : case '5' : case '7':
      xmin=FRect[0];xmax=FRect[2];ymin= FRect[1];ymax= FRect[3];
      break;
    case '2' : case '4' : case '6' : case '8':
      if ( (int)strlen(xf) < 1) c='g' ; else c=xf[0];
      switch ( c )
	{
	case 'e' : xmin= 1.0 ; xmax = (*n2);break;
	case 'o' : xmax= Maxi(x,(*n2)); xmin= Mini(x,(*n2)); break;
	case 'g' :
	default: xmax= Maxi(x, size_x); xmin= Mini(x, size_x); break;
	}
      ymin=  Mini(y, size_y); ymax=  Maxi(y,size_y);
      /* back to default values for  x=[] and y = [] */
      if ( ymin == LARGEST_REAL ) { ymin = 0; ymax = 10.0 ;} 
      if ( xmin == LARGEST_REAL ) { xmin = 0; xmax = 10.0 ;} 
      break;
    }

  /*
   * modify computed min,max if isoview requested 
   */

  if ( strflag[1] == '3' || strflag[1] == '4')
    {
      /* code added by S. Mottelet 11/7/2000 */
      double FRect[4],WRect[4],ARect[4];
      char logscale[4];      
      /* end of added code by S. Mottelet 11/7/2000 */
      
      int wdim[2];
      nsp_gengine->xget_windowdim(wdim,wdim+1);
      hx=xmax-xmin;
      hy=ymax-ymin;

      /* code added by S. Mottelet 11/7/2000 */
      getscale2d(WRect,FRect,logscale,ARect);

      wdim[0]=linint((double)wdim[0] * WRect[2]);
      wdim[1]=linint((double)wdim[1] * WRect[3]);
      /* end of added code by S. Mottelet 11/7/2000 */

      if ( hx/(double)wdim[0]  <hy/(double) wdim[1] ) 
	{
	  hx1=wdim[0]*hy/wdim[1];
	  xmin=xmin-(hx1-hx)/2.0;
	  xmax=xmax+(hx1-hx)/2.0;
	}
      else 
	{
	  hy1=wdim[1]*hx/wdim[0];
	  ymin=ymin-(hy1-hy)/2.0;
	  ymax=ymax+(hy1-hy)/2.0;
	}
    }

  /* Changing min,max and aaint if using log scaling X axis */
  if ((int)strlen(xf) >= 2 && xf[1]=='l' && (int)strlen(strflag) >= 2 && strflag[1] != '0')
    {
      /* xaxis */
      if ( xmin >  0)
	{
	  xmax=ceil(log10(xmax));  xmin=floor(log10(xmin));
	}
      else 
	{
	  Scistring("Warning: Can't use Log on X-axis xmin is negative \n");
	  xmax= 1; xmin= 0;
	}
      aaint[0]=1;aaint[1]=inint(xmax-xmin);
    }

  /* Changing ymin,ymax and aaint if using log scaling Y axis */
  if ((int)strlen(xf) >=3  && xf[2]=='l' && (int)strlen(strflag) >= 2 && strflag[1] != '0')
    {
      /* y axis */
      if ( ymin > 0 ) 
	{
	  ymax= ceil(log10(ymax)); ymin= floor(log10(ymin));
	}
      else 
	{
	  Scistring(" Can't use Log on y-axis ymin is negative \n");
	  ymax= 1; ymin= 0;
	}
      aaint[2]=1;aaint[3]=inint(ymax-ymin);
    }
  
  /* FRect gives the plotting boundaries xmin,ymin,xmax,ymax */

  FRect[0]=xmin;FRect[1]=ymin;FRect[2]=xmax;FRect[3]=ymax;

  /* if strflag[1] == 7 or 8 we compute the max between current scale and the new one  */
  if (strflag[1] == '7' || strflag[1] == '8' )
    {
      if ( current_scale.flag != 0 ) 
	{
	  /* first check that we are not changing from normal<-->log */
	  int xlog = ((int)strlen(xf) >= 2 && xf[1]=='l' ) ? 1: 0;
	  int ylog = ((int)strlen(xf) >=3  && xf[2]=='l' ) ? 2: 0;
	  if ( (xlog == 1 && current_scale.logflag[0] == 'n') 
	       || (xlog == 0 && current_scale.logflag[0] == 'l')
	       || (ylog == 1 && current_scale.logflag[1] == 'n')
	       || (ylog == 0 && current_scale.logflag[1] == 'l') )
	    {
	      Scistring("Warning: you cannot use automatic rescale if you switch from log to normal or normal to log \n");
	    }
	  else 
	    {
	      FRect[0] = Min(FRect[0],current_scale.frect[0]);
	      FRect[1] = Min(FRect[1],current_scale.frect[1]);
	      FRect[2] = Max(FRect[2],current_scale.frect[2]);
	      FRect[3] = Max(FRect[3],current_scale.frect[3]);
	      if ( FRect[0] < current_scale.frect[0] 
		   || FRect[1] < current_scale.frect[1] 
		   || FRect[2] > current_scale.frect[2] 
		   || FRect[3] > current_scale.frect[3] )
		redraw = 1;
	    }
	}
      /* and we force flag back to 5  */
      strflag[1] = '5';
    }
  else 
    {
      /* changes strflag[1] to accelerate next calls (replot) */
      switch (strflag[1]) 
	{
	case '2' : strflag[1]='1';break;
	  /* case '4' : strflag[1]='3';break; */
	case '6' : strflag[1]='5';break;
	}
    }
	  
  if ( (int)strlen(strflag) >=2 && ( strflag[1]=='5' || strflag[1]=='6' ))
    {
      /* recherche automatique des bornes et graduations */
      Gr_Rescale(&xf[1],FRect,Xdec,Ydec,&(aaint[0]),&(aaint[2]));
      Gr_Rescale(&xf[1],FRect,Xdec,Ydec,&(aaint[0]),&(aaint[2]));
    }
  
  /* Update the current scale */

  set_scale("tftttf",NULL,FRect,aaint,xf+1,NULL);

  /* Should be added to set_scale */

  for (i=0; i < 3 ; i++ ) current_scale.xtics[i] = Xdec[i];
  for (i=0; i < 3 ; i++ ) current_scale.ytics[i] = Ydec[i];
  current_scale.xtics[3] = aaint[1];
  current_scale.ytics[3] = aaint[3];

  /* Changing back min,max and aaint if using log scaling X axis */
  if ((int)strlen(xf) >= 2 && xf[1]=='l' && (int)strlen(strflag) >= 2 && strflag[1] != '0')
    {
      FRect[0]=exp10(xmin);FRect[2]=exp10(xmax);
    }
  /* Changing ymin,ymax and aaint if using log scaling Y axis */
  if ((int)strlen(xf) >=3  && xf[2]=='l' && (int)strlen(strflag) >= 2 && strflag[1] != '0')
    {
      FRect[1]= exp10(ymin);FRect[3]= exp10(ymax);
    }
  /* Redraw other graphics */
  if ( redraw )
    {
      static int flag[2]={1,0};
      char driver[4];
      /* Redraw previous graphics with new Scale */
      integer ww;
      nsp_gengine1.get_driver_name(driver);
      if (strcmp("Rec",driver) != 0) 
	{
	  Scistring("Auto rescale only works with the rec driver\n" );
	  return;
	}
      ww = nsp_gengine->xget_curwin();
      nsp_gengine1.set_driver("X11");
      nsp_gengine->clearwindow();
      tape_replay_new_scale_1(ww,flag,aaint,FRect);
     nsp_gengine1.set_driver(driver);
    }
}
 

/*----------------------------------------------------
 *  legend="leg1@leg2@leg3@...."             
 * legend contain legends separated by '@'
 * if nlegend is the number of legends stored in legend
 * then the function Legends draw  Min(*n1,6,nlegends) legends
 *-----------------------------------------------------*/


void Legends(int *style,int * n1,char * legend)
{
  int rect[4],xx,yy;
  char *leg,*loc;
  double xi,xi1,yi,yi1,xoffset,yoffset;  
  int i;
  loc=(char *) MALLOC( (strlen(legend)+1)*sizeof(char));

  nsp_gengine->boundingbox("pl",xx,yy,rect);

  if ( loc != 0)
    {
      integer fg,old_dash,pat;
      fg = nsp_gengine->xget_foreground();
      old_dash = nsp_gengine->xset_dash(1);
      pat = nsp_gengine->xset_pattern(fg);

      strcpy(loc,legend);

      /* length for the tick zone associated to the legend */
      xoffset= (current_scale.wdim[0]*current_scale.subwin_rect[2]*(1- current_scale.axis[0] - current_scale.axis[1]))/12.0;
      /* y offset between legends */
      yoffset= (current_scale.wdim[1]*current_scale.subwin_rect[3]*current_scale.axis[3])/5.0;

      /* x position of the legends in pixel if n <= 3 */ 
      xi = current_scale.wdim[0]*current_scale.subwin_rect[2]*current_scale.axis[0]
	+ current_scale.subwin_rect[0]*current_scale.wdim[0];
      /* x position of the legends in pixel if n > 3 */ 
      xi1 = xi + current_scale.wdim[0]*current_scale.subwin_rect[2]*(1.0 - (current_scale.axis[0]+current_scale.axis[1]))/2.0 ;

      /* y position of x-axis in pixel */
      yi1 = current_scale.wdim[1]*current_scale.subwin_rect[3]*current_scale.axis[2]
	+ current_scale.subwin_rect[1]*current_scale.wdim[1] 
	+ current_scale.subwin_rect[3]*current_scale.wdim[1]*(1.0 - (current_scale.axis[2]+current_scale.axis[3])) ; /* ouf !! */

      for ( i = 0 ; i < *n1 && i < 6 ; i++)
	{  
	  integer xs,ys,flag=0,polyx[2],polyy[2],lstyle[1],ni;
	  double angle=0.0;
	  if (*n1 == 1) ni=Max(Min(5,style[1]-1),0);else ni=i;
	  if (ni >= 3)
	    { 
	      /* down left point for string display */
	      xi= xi1;
	      yi= yi1 +(ni-3)*yoffset+3*yoffset;}
	  else
	    { 
	      yi= yi1 + (ni)*yoffset+3*yoffset;
	    }
	  xs=inint(xi+1.2*xoffset);
	  ys=inint(yi);
	  if ( i==0) leg=strtok(loc,"@"); else leg=strtok((char *)0,"@");
	  if (leg != 0) 
	    {
	      nsp_gengine->xset_pattern(fg);
	      nsp_gengine->displaystring(leg,xs,ys,flag,angle);
	      nsp_gengine->xset_pattern(pat);
	      if (style[i] > 0)
		{ 
		  integer n=1,p=2;
		  polyx[0]=inint(xi);polyx[1]=inint(xi+xoffset);
		  polyy[0]=inint(yi - rect[3]/2);polyy[1]=inint(yi- rect[3]/2.0);
		  lstyle[0]=style[i];
		  nsp_gengine->drawpolylines(polyx,polyy,lstyle,n,p);
		}
	      else
		{ 
		  integer n=1,p=1;
		  polyx[0]=inint(xi+xoffset);
		  polyy[0]=inint(yi- rect[3]/2);
		  lstyle[0]=style[i];
		  nsp_gengine->drawpolylines(polyx,polyy,lstyle,n,p);
		}
	    }
	}
      FREE(loc);
      nsp_gengine->xset_dash(old_dash);
    }
  else
    {
      Scistring("Legends : running out of memory to store legends\n");
    }
}













