/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *--------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"

/* FIXME: ? should be removed */
#define spINSIDE_SPARSE
#if defined(THINK_C) || defined (__MWERKS__)
#include "::sparse:spConfig.h" 
#else
#include "../sparse/spConfig.h"
#endif

/*--------------------------------------------------------------------
 *  nsp_plot2d(x,y,n1,n2,style,strflag,legend,brect,aaint,lstr1,lstr2)
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
 *        is specified by the vector aaint[4] of ints 
 *	   <aaint[0],aaint[1]> specifies the x-axis number of  points 
 *	   <aaint[2],aaint[3]> same for y-axis
 *     if  strflag[2] == '2' -> no axis, only a box around the curves
 *     else no box and no axis 
 *
 * lstr* : unused ( but used by Fortran ) 
 *--------------------------------------------------------------------------*/
  
int nsp_plot2d(BCG *Xgc,double x[],double y[],int *n1,int *n2,int style[],char *strflag,
	       char *legend,double brect[],int aaint[])
{
  int n;
  int *xm,*ym;

  /* Storing values if using the Record driver */
  /* Boundaries of the frame */
  
  update_frame_bounds(Xgc,0,"gnn",x,y,n1,n2,aaint,strflag,brect);

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Plot1(Xgc,"gnn",x,y,n1,n2,style,strflag,legend,brect,aaint);

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
      scale_f2i(Xgc,x,y,xm,ym,n);
      /** Drawing axes **/
    }
  axis_draw(Xgc,strflag);
  /** Drawing the curves **/
  if ( n != 0 ) 
    {
      frame_clip_on(Xgc);
      Xgc->graphic_engine->drawpolylines(Xgc,xm,ym,style,*n1,*n2);
      frame_clip_off(Xgc);
      /** Drawing the Legends **/
      if ((int)strlen(strflag) >=1  && strflag[0] == '1')
	nsp_legends(Xgc,style,n1,legend); 
    }

  /* my_gl_main (0,NULL); */

  return(0);
}


/*--------------------------------------------------------------------
 * add a grid to a 2D plot
 *  FIXME: 
 *  If we are using Gr_Rescale_new xgrid will not work 
 *  since the grid does not start at frect boundaries
 *--------------------------------------------------------------------*/

int nsp_plot_grid_old(BCG *Xgc, int *style)
{
  int closeflag=0,n=2,vx[2],vy[2],i,j;
  double pas;
  int pat;
  /* Recording command */
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) store_Grid(Xgc,style);
  /* changes dash style if necessary */
  pat = Xgc->graphic_engine->xset_pattern(Xgc,*style);
  /** Get current scale **/
  pas = ((double) Xgc->scales->WIRect1[2]) / ((double) Xgc->scales->Waaint1[1]);
  /** x-axis grid (i.e vertical lines ) */
  for ( i=0 ; i < Xgc->scales->Waaint1[1]; i++)
    {
      vy[0]=Xgc->scales->WIRect1[1];
      vy[1]=Xgc->scales->WIRect1[1]+Xgc->scales->WIRect1[3];
      vx[0]=vx[1]= Xgc->scales->WIRect1[0] + inint( ((double) i)*pas);
      if ( i!=0) Xgc->graphic_engine->drawpolyline(Xgc,vx, vy,n,closeflag);
      if (Xgc->scales->logflag[0] == 'l') 
	{
	  int jinit=1;
	  if ( i== 0 ) jinit=2; /* no grid on plot boundary */
	  for (j= jinit; j < 10 ; j++)
	    {
	      vx[0]=vx[1]= Xgc->scales->WIRect1[0] + inint( ((double) i)*pas)+ inint(log10(((double)j))*pas);
	      Xgc->graphic_engine->drawpolyline(Xgc, vx, vy,n,closeflag);
	    }
	}
    }
  /** y-axis grid (i.e horizontal lines ) **/
  pas = ((double) Xgc->scales->WIRect1[3]) / ((double) Xgc->scales->Waaint1[3]);
  for ( i=0 ; i < Xgc->scales->Waaint1[3]; i++)
    {
      vx[0]=Xgc->scales->WIRect1[0];
      vx[1]=Xgc->scales->WIRect1[0]+Xgc->scales->WIRect1[2];
      vy[0]=vy[1]= Xgc->scales->WIRect1[1] + inint( ((double) i)*pas);
      if (i!=0)  Xgc->graphic_engine->drawpolyline(Xgc, vx, vy,n,closeflag);
      if (Xgc->scales->logflag[1] == 'l') 
	{
	  int jinit=1;
	  if ( i== Xgc->scales->Waaint1[3]-1 ) jinit=2; /* no grid on plot boundary */
	  for (j= jinit; j < 10 ; j++)
	    {
	      vy[0]=vy[1]= Xgc->scales->WIRect1[1] + inint( ((double) i+1)*pas)- inint(log10(((double)j))*pas);
	       Xgc->graphic_engine->drawpolyline(Xgc, vx, vy,n,closeflag);
	    }
	}
    }
  Xgc->graphic_engine->xset_pattern(Xgc,pat);
  return(0);
}

int nsp_plot_grid(BCG *Xgc, int *style)
{
  int closeflag=0,n=2,vx[2],vy[2],i,j;
  double vxd[2],vyd[2],pas;
  int pat;
  /* Recording command */
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) store_Grid(Xgc,style);
  /* changes dash style if necessary */
  pat = Xgc->graphic_engine->xset_pattern(Xgc,*style);
  /** x-axis grid (i.e vertical lines ) */
  pas = ((double) Xgc->scales->WIRect1[2]) / ((double) Xgc->scales->Waaint1[1]);
  for ( i=0 ; i <= Xgc->scales->xtics[3]; i++)
    {
      vyd[0]=Xgc->scales->frect[1];
      vyd[1]=Xgc->scales->frect[3];
      vxd[0]=vxd[1]= exp10((double)Xgc->scales->xtics[2]) *
	(Xgc->scales->xtics[0] + i*(Xgc->scales->xtics[1]-Xgc->scales->xtics[0])/Xgc->scales->xtics[3]);
      scale_f2i(Xgc,vxd,vyd,vx,vy,2);
      /* FIXME: detect here if tics and vertical boundaries match ? */
      if ( vxd[0] != Xgc->scales->frect[0] && vxd[0] != Xgc->scales->frect[2]) 
	Xgc->graphic_engine->drawpolyline(Xgc,vx, vy,n,closeflag);
      /* FIXME: strange ? */
      if (Xgc->scales->logflag[0] == 'l') 
	{
	  int jinit=1;
	  if ( i== 0 ) jinit=2; /* no grid on plot boundary */
	  for (j= jinit; j < 10 ; j++)
	    {
	      vx[0]=vx[1]= Xgc->scales->WIRect1[0] + inint( ((double) i)*pas)+ inint(log10(((double)j))*pas);
	      Xgc->graphic_engine->drawpolyline(Xgc, vx, vy,n,closeflag);
	    }
	}
    }
  /** y-axis grid (i.e horizontal lines ) **/
  pas = ((double) Xgc->scales->WIRect1[3]) / ((double) Xgc->scales->Waaint1[3]);
  for ( i=0 ; i <= Xgc->scales->ytics[3]; i++)
    {
      vxd[0]=Xgc->scales->frect[0];
      vxd[1]=Xgc->scales->frect[2];
      vyd[0]=vyd[1]= exp10((double)Xgc->scales->ytics[2]) *
	(Xgc->scales->ytics[0] + i*(Xgc->scales->ytics[1]-Xgc->scales->ytics[0])/Xgc->scales->ytics[3]);
      scale_f2i(Xgc,vxd,vyd,vx,vy,2);
      if ( vyd[0] != Xgc->scales->frect[1] && vyd[0] != Xgc->scales->frect[3]) 
	Xgc->graphic_engine->drawpolyline(Xgc, vx, vy,n,closeflag);
      if (Xgc->scales->logflag[1] == 'l') 
	{
	  int jinit=1;
	  if ( i== Xgc->scales->Waaint1[3]-1 ) jinit=2; /* no grid on plot boundary */
	  for (j= jinit; j < 10 ; j++)
	    {
	      vy[0]=vy[1]= Xgc->scales->WIRect1[1] + inint( ((double) i+1)*pas)- inint(log10(((double)j))*pas);
	       Xgc->graphic_engine->drawpolyline(Xgc, vx, vy,n,closeflag);
	    }
	}
    }
  Xgc->graphic_engine->xset_pattern(Xgc,pat);
  return(0);
}


/*---------------------------------------------------------------------
 * update_frame_bounds : 
 *  modify according to strflag the current scales 
 *  Xgc->scales->using given data 
 *  output : FRect,aaint,strflag are modified 
 * Remark: this function is recalled each time 
 *   a plot needs to be redrawn. Thus changed 
 *   data must be changed only once not each time this 
 *   function is called. strflag must be changed 
 *   after the first call for this purpose.
 *----------------------------------------------------*/

void update_frame_bounds(BCG *Xgc, int cflag, char *xf, double *x,double *y,
			 int *n1, int *n2, int *aaint,char *strflag, double FRect[4])
{
  double FRect1[4];
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
    case '1' : case '3' : case '5' : case '7': case '9' : case 'B':
      xmin=FRect[0];xmax=FRect[2];ymin= FRect[1];ymax= FRect[3];
      break;
    case '2' : case '4' : case '6' : case '8': case 'A' : case 'C':
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
      FRect[0]=xmin;FRect[1]=ymin;FRect[2]=xmax;FRect[3]=ymax;
      break;
    }

  if (strflag[1] == '7' || strflag[1] == '8' )
    {
      /* if strflag[1] == 7 or 8 we compute the max between current scale and the new one  
       * and try to detect if we need to redraw previous plots 
       */
      if ( Xgc->scales->scale_flag != 0 ) 
	{
	  /* first check that we are not changing from normal<-->log */
	  int xlog = ((int)strlen(xf) >= 2 && xf[1]=='l' ) ? 1: 0;
	  int ylog = ((int)strlen(xf) >=3  && xf[2]=='l' ) ? 2: 0;
	  if ( (xlog == 1 && Xgc->scales->logflag[0] == 'n') 
	       || (xlog == 0 && Xgc->scales->logflag[0] == 'l')
	       || (ylog == 1 && Xgc->scales->logflag[1] == 'n')
	       || (ylog == 0 && Xgc->scales->logflag[1] == 'l') )
	    {
	      Scistring("Warning: you cannot use automatic rescale if you switch from log to normal or normal to log \n");
	    }
	  else 
	    {
	      FRect[0] = Min(FRect[0],Xgc->scales->frect[0]);
	      FRect[1] = Min(FRect[1],Xgc->scales->frect[1]);
	      FRect[2] = Max(FRect[2],Xgc->scales->frect[2]);
	      FRect[3] = Max(FRect[3],Xgc->scales->frect[3]);
	      if ( FRect[0] < Xgc->scales->frect[0] 
		   || FRect[1] < Xgc->scales->frect[1] 
		   || FRect[2] > Xgc->scales->frect[2] 
		   || FRect[3] > Xgc->scales->frect[3] )
		{
		  Xgc->graphic_engine->xinfo(Xgc,"need to redraw because of autoscale" );
		  redraw = 1;
		}
	    }
	}
    }


  /* switch strflag so as to use now FRect   */
  plot2d_strf_change('d',strflag);

  /* we also need to redraw if other graphics are not in the same  mode */
  if ( redraw == 0 &&  Xgc->scales->scale_flag ) 
    {
      if ( strflag[1] == '9' ) 
	{ if ( Xgc->scales->strflag != '1'  ) redraw = 1;}
      else if ( strflag[1] == 'B' ) 
	{ if ( Xgc->scales->strflag != '3'  ) redraw = 1;}
      else if ( strflag[1] == '7' ) 
	{ if ( Xgc->scales->strflag != '5'  ) redraw = 1;}
      if ( redraw == 1) Xgc->graphic_engine->xinfo(Xgc,"need to redraw because of mode change" );
    }

  /* we also need to redraw if the mode is the same but frect is changing */
  if ( redraw == 0 &&  Xgc->scales->scale_flag != 0 ) 
    {
      if ( strflag[1]=='7'|| strflag[1] == '9' ||  strflag[1] == 'B' )
	{
	  if ( FRect[0] != Xgc->scales->frect[0] 
	       || FRect[1] != Xgc->scales->frect[1] 
	       || FRect[2] != Xgc->scales->frect[2] 
	       || FRect[3] != Xgc->scales->frect[3] )
	    {
	      Xgc->graphic_engine->xinfo(Xgc,"need to redraw because of scale change in mode" );
	      redraw = 1;
	    }
	}
    }
  
  /* Warning: FRect must not be changed bellow 
   */

  /*
   * modify computed min,max if isoview requested 
   */

  if ( (int)strlen(strflag) >= 2 && (strflag[1] == '3' || strflag[1] == '4'))
    {
      /* code by S. Mottelet 11/7/2000 */
      double WRect[4],ARect[4];
      char logscale[4];      
      int wdim[2];
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      hx=xmax-xmin;
      hy=ymax-ymin;
      getscale2d(Xgc,WRect,FRect1,logscale,ARect);
      wdim[0]=linint((double)wdim[0] * WRect[2]);
      wdim[1]=linint((double)wdim[1] * WRect[3]);
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
  
  /* FRect1 gives the plotting boundaries xmin,ymin,xmax,ymax */
  FRect1[0]=xmin;FRect1[1]=ymin;FRect1[2]=xmax;FRect1[3]=ymax;
  
  /* pretty axes */
  if ( (int)strlen(strflag) >=2 && ( strflag[1]=='5' || strflag[1]== '7') ) 
    {
      double FRect2[4];
      int i;
      for (i=0; i< 4 ;i++) FRect2[i]=FRect1[i];
      /* change graduation */
      Gr_Rescale_new(&xf[1],FRect2,Xdec,Ydec,&(aaint[0]),&(aaint[2]));
    }

  /* we also need to redraw if aaint has changed */
  if ( redraw == 0 &&  Xgc->scales->scale_flag != 0 ) 
    {
      if (strflag[1] == '7' || strflag[1] == '9' ||  strflag[1] == 'B' ) 
	{
	  if ( aaint[0] != Xgc->scales->Waaint1[0] 
	       || aaint[1] != Xgc->scales->Waaint1[1] 
	       || aaint[2] != Xgc->scales->Waaint1[2] 
	       || aaint[3] != Xgc->scales->Waaint1[3] )
	    {
	      Xgc->graphic_engine->xinfo(Xgc,"need to redraw because of aaint change" );
	      redraw = 1;
	    }
	}
    }
  
  /* Update the current scale */

  set_scale(Xgc,"tftttf",NULL,FRect1,aaint,xf+1,NULL);

  /* store information about graduation in xtics */
  
  if ( (int)strlen(strflag) >=2 && strflag[1]=='5' ) 
    {
      for (i=0; i < 3 ; i++ ) Xgc->scales->xtics[i] = Xdec[i];
      for (i=0; i < 3 ; i++ ) Xgc->scales->ytics[i] = Ydec[i];
      Xgc->scales->xtics[3] = aaint[1];
      Xgc->scales->ytics[3] = aaint[3];
    }
  else 
    {
      Xgc->scales->xtics[0] = xmin;
      Xgc->scales->xtics[1] = xmax;
      Xgc->scales->xtics[2] = 0.0;
      Xgc->scales->xtics[3] = aaint[1];

      Xgc->scales->ytics[0] = ymin;
      Xgc->scales->ytics[1] = ymax;
      Xgc->scales->ytics[2] = 0.0;
      Xgc->scales->ytics[3] = aaint[3];
    }

  /* switch back to recorded mode */
  if ( strflag[1] == '9' ) strflag[1]= '1';
  else if ( strflag[1] == 'B' ) strflag[1]= '3';
  else if ( strflag[1] == '7' ) strflag[1]= '5';

  /* store information about strflag */ 
  Xgc->scales->strflag = strflag[1]; 

  /* FIXME: is it necessary ? */
  /* Changing back min,max and aaint if using log scaling X axis */
  if ((int)strlen(xf) >= 2 && xf[1]=='l' && (int)strlen(strflag) >= 2 && strflag[1] != '0')
    {
      FRect1[0]=exp10(xmin);FRect1[2]=exp10(xmax);
    }
  /* Changing ymin,ymax and aaint if using log scaling Y axis */
  if ((int)strlen(xf) >=3  && xf[2]=='l' && (int)strlen(strflag) >= 2 && strflag[1] != '0')
    {
      FRect1[1]= exp10(ymin);FRect1[3]= exp10(ymax);
    }

  /* Redraw other graphics if needed */
  if ( redraw )
    {
      /* we propagate the new strflag[1] to previous plots and the new frect 
       */
      static int flag[3]={1,1,1};
      /* if the last graphic contains axis draw we remove previous ones */
      /* Redraw previous graphics with new Scale */
      if ( Xgc->graphic_engine->xget_recording(Xgc) == FALSE ) 
	{
	  Xgc->graphic_engine->xinfo(Xgc,"Auto rescale only works when recording is on " );
	  return;
	}
      Xgc->graphic_engine->clearwindow(Xgc);    
      tape_replay_new_scale_1(Xgc,Xgc->CurWindow,flag,aaint,FRect,strflag);
    }
}

/*----------------------------------------------------
 * plot2d_brect_from_data:
 *  compute FRect from data if necessary 
 *----------------------------------------------------*/

void plot2d_brect_from_data(BCG *Xgc,int cflag, char *xf, double *x,double *y,
			    int *n1, int *n2, int *aaint, char *strflag, double FRect[4])
{
  double xmin,xmax,ymin,ymax;
  /* cflag is used when using contour */
  int size_x = (cflag == 1) ? (*n1) : (*n1)*(*n2) ;
  int size_y = (cflag == 1) ? (*n2) : (*n1)*(*n2) ;
  char c;
  if ((int)strlen(strflag) < 2) return ;
  switch (strflag[1])
    {
    case '1' : case '3' : case '5' : case '7':
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
      FRect[0]=xmin;FRect[1]=ymin;FRect[2]=xmax;FRect[3]=ymax;
      break;
    }
}
 
/*
 * switch strf flag up or down 
 */

void plot2d_strf_change(char c, char *strf)
{
  if ( c == 'u') 
    {
      /* move up */ 
      switch (strf[1]) 
	{
	case '1' : strf[1]='2';break;
	case '3' : strf[1]='4';break;
	case '5' : strf[1]='6';break;
	case '7' : strf[1]='8';break;
	case '9' : strf[1]='A';break;
	case 'B' : strf[1]='C';break;
	}
    }
  else
    {
      /*move down */
      switch (strf[1]) 
	{
	case '2' : strf[1]='1';break;
	case '4' : strf[1]='3';break;
	case '6' : strf[1]='5';break;
	case '8' : strf[1]='7';break;
	case 'A' : strf[1]='9';break;
	case 'C' : strf[1]='B';break;
	}

    }
}


/*----------------------------------------------------
 *  legend="leg1@leg2@leg3@...."             
 * legend contain legends separated by '@'
 * if nlegend is the number of legends stored in legend
 * then the function Legends draw  Min(*n1,6,nlegends) legends
 *-----------------------------------------------------*/

void nsp_legends_old(BCG *Xgc,int *style,int * n1,char * legend)
{
  int rect[4],xx,yy;
  char *leg,*loc;
  double xi,xi1,yi,yi1,xoffset,yoffset;  
  int i;
  loc=(char *) MALLOC( (strlen(legend)+1)*sizeof(char));

  Xgc->graphic_engine->boundingbox(Xgc,"pl",xx,yy,rect);

  if ( loc != 0)
    {
      int fg,old_dash,pat;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      old_dash = Xgc->graphic_engine->xset_dash(Xgc,1);
      pat = Xgc->graphic_engine->xset_pattern(Xgc,fg);

      strcpy(loc,legend);

      /* length for the tick zone associated to the legend */
      xoffset= (Xgc->scales->wdim[0]*Xgc->scales->subwin_rect[2]*(1- Xgc->scales->axis[0] - Xgc->scales->axis[1]))/12.0;
      /* y offset between legends */
      yoffset= (Xgc->scales->wdim[1]*Xgc->scales->subwin_rect[3]*Xgc->scales->axis[3])/5.0;

      /* x position of the legends in pixel if n <= 3 */ 
      xi = Xgc->scales->wdim[0]*Xgc->scales->subwin_rect[2]*Xgc->scales->axis[0]
	+ Xgc->scales->subwin_rect[0]*Xgc->scales->wdim[0];
      /* x position of the legends in pixel if n > 3 */ 
      xi1 = xi + Xgc->scales->wdim[0]*Xgc->scales->subwin_rect[2]*(1.0 - (Xgc->scales->axis[0]+Xgc->scales->axis[1]))/2.0 ;

      /* y position of x-axis in pixel */
      yi1 = Xgc->scales->wdim[1]*Xgc->scales->subwin_rect[3]*Xgc->scales->axis[2]
	+ Xgc->scales->subwin_rect[1]*Xgc->scales->wdim[1] 
	+ Xgc->scales->subwin_rect[3]*Xgc->scales->wdim[1]*(1.0 - (Xgc->scales->axis[2]+Xgc->scales->axis[3])) ; /* ouf !! */

      for ( i = 0 ; i < *n1 && i < 6 ; i++)
	{  
	  int xs,ys,flag=0,polyx[2],polyy[2],lstyle[1],ni;
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
	      Xgc->graphic_engine->xset_pattern(Xgc,fg);
	      Xgc->graphic_engine->displaystring(Xgc,leg,xs,ys,flag,angle);
	      Xgc->graphic_engine->xset_pattern(Xgc,pat);
	      if (style[i] > 0)
		{ 
		  int n=1,p=2;
		  polyx[0]=inint(xi);polyx[1]=inint(xi+xoffset);
		  polyy[0]=polyy[1]=inint(yi - rect[3]/2.0);
		  lstyle[0]=style[i];
		  Xgc->graphic_engine->drawpolylines(Xgc,polyx,polyy,lstyle,n,p);
		}
	      else
		{ 
		  int n=1,p=1;
		  polyx[0]=inint(xi+xoffset);
		  polyy[0]=inint(yi- rect[3]/2);
		  lstyle[0]=style[i];
		  Xgc->graphic_engine->drawpolylines(Xgc,polyx,polyy,lstyle,n,p);
		}
	    }
	}
      FREE(loc);
      Xgc->graphic_engine->xset_dash(Xgc,old_dash);
    }
  else
    {
      Scistring("Legends : running out of memory to store legends\n");
    }
}

/*----------------------------------------------------
 *  legend="leg1@leg2@leg3@...."             
 *  legend contain legends separated by '@'
 *  if nlegend is the number of legends stored in legend
 *  then the function Legends draw  Min(*n1,6,nlegends) legends
 *-----------------------------------------------------*/

static void nsp_legends_box(BCG *Xgc,int n1,int *style,char * legend,int box[4],int get_box,
			    double xoffset,double yoffset,int pat,int fg);

void nsp_legends(BCG *Xgc,int *style,int * n1,char * legend)
{
  int rect[4],box[4],xx,yy;
  char *loc;
  double xoffset,yoffset;  
  loc=(char *) MALLOC( (strlen(legend)+1)*sizeof(char));

  Xgc->graphic_engine->boundingbox(Xgc,"pl",xx,yy,rect);

  if ( loc != 0)
    {
      int fg,old_dash,pat;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      old_dash = Xgc->graphic_engine->xset_dash(Xgc,1);
      pat = Xgc->graphic_engine->xset_pattern(Xgc,fg);

      strcpy(loc,legend);

      /* length for the tick zone associated to the legend */
      xoffset= (Xgc->scales->WIRect1[2])/20.0;
      /* y offset between legends */
      yoffset= rect[3];
      /* upper right position of the legend box */
      box[0] = Xgc->scales->WIRect1[0] + Xgc->scales->WIRect1[2];
      box[1] = Xgc->scales->WIRect1[1];
      nsp_legends_box(Xgc,*n1,style,loc,box,TRUE,xoffset,yoffset,pat,fg);
      strcpy(loc,legend);
      box[0] -= box[2]+ xoffset/2.0;
      box[1] += xoffset/2.0;
      nsp_legends_box(Xgc,*n1,style,loc,box,FALSE,xoffset,yoffset,pat,fg);
      FREE(loc);
      Xgc->graphic_engine->xset_dash(Xgc,old_dash);
    }
  else
    {
      Scistring("Legends : running out of memory to store legends\n");
    }
}

/* 
 * draw or compute the bounding box 
 */

static void nsp_legends_box(BCG *Xgc,int n1,int *style,char * legend,int box[4],int get_box,
			    double xoffset,double yoffset,int pat,int fg)
{
  int i,xs,ys,flag=0,polyx[2],polyy[2],lstyle[1],ni,rect[4];
  double angle=0.0,yi,xi;
  xi= 1.4*xoffset;
  yi= box[1]+ yoffset*(1.25);
  if ( get_box == TRUE )
    {
      box[3]= n1*1.5*yoffset;
      box[2]= xi;
    }
  for ( i = 0 ; i < n1 ; i++)
    {  
      if (n1 == 1) ni=Max(Min(5,style[1]-1),0);else ni=i;
      xs=inint(box[0]+xi);
      ys=inint(yi);
      if ( i==0) legend=strtok(legend,"@"); else legend=strtok((char *)0,"@");
      if (legend != 0) 
	{
	  if ( get_box == TRUE )
	    {
	      Xgc->graphic_engine->boundingbox(Xgc,legend,xs,ys,rect);
	      box[2]= Max(box[2],xi+rect[2]);
	    }
	  else
	    {
	      Xgc->graphic_engine->xset_pattern(Xgc,fg);
	      Xgc->graphic_engine->displaystring(Xgc,legend,xs,ys,flag,angle);
	      Xgc->graphic_engine->xset_pattern(Xgc,pat);
	      if (style[i] > 0)
		{ 
		  int n=1,p=2;
		  polyx[0]=inint(box[0]);polyx[1]=inint(box[0]+xoffset);
		  polyy[0]=polyy[1]=inint(yi - rect[3]/2.0);
		  lstyle[0]=style[i];
		  Xgc->graphic_engine->drawpolylines(Xgc,polyx,polyy,lstyle,n,p);
		}
	      else
		{ 
		  /* FIXME: should be affected by marksize */
		  int n=1,p=1;
		  polyx[0]=inint(box[0]+xoffset);
		  polyy[0]=inint(yi- rect[3]/2);
		  lstyle[0]=style[i];
		  Xgc->graphic_engine->drawpolylines(Xgc,polyx,polyy,lstyle,n,p);
		}
	    }
	}
      yi += yoffset*(1.5);
    }
  if ( get_box == FALSE )
    {
      Xgc->graphic_engine->drawrectangle(Xgc,box);
    }
}
