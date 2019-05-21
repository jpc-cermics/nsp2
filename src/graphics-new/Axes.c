/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/nsp.h>
#include <nsp/graphics-new/Graphics.h>

static double x_convert (char xy_type,const double x[] ,int i);
static double y_convert (char xy_type,const double x[] ,int i);
static void NumberFormat (char *str,int k,int a);
static void aplotv1_new(BCG *Xgc,char mode, int grid_color,int flag);
static void aplotv2 (BCG *Xgc,char mode, int grid_color,int flag);
static void nsp_draw_frame_rectangle(BCG *Xgc) ;
static void nsp_draw_filled_rectangle(BCG *Xgc,int bg) ;
static void Sci_Axis(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, char **str, int subtics,
		     char *format, int fontsize, int textcolor, int ticscolor, char logflag, int seg_flag);
static void nsp_axis_grid(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny,
                          int grid_color, char logflag, int seg_flag);
static double axes_number2str(char dir,char xy_type,char *res,char **str,const char *format,const char *c_format,const double *x,int i);
static int nsp_fontsize_string_in_box(BCG *Xgc, double iw, double ih, int fsize, const char *str);
static void draw_segment(BCG *Xgc, double *vx, double *vy, int color);

/**
 * nsp_graphic_titles:
 * @Xgc: a graphic context
 *
 **/

void nsp_graphic_titles(BCG *Xgc,char *title,char *x,char *y)
{
  int auto_size=TRUE; /* font are resized automatically */
  int fontid[2],fontsize_kp;

  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
  fontsize_kp = fontid[1] ;
  if ( auto_size )
    {
      const char *str = "O";
      double sw = Xgc->scales->Irect.width/25.0;
      double sh = Xgc->scales->Irect.height/25.0;
      int fid1,fid2;
      fid1= nsp_fontsize_string_in_box(Xgc,sw,sh, -1, str);
      fid2= nsp_fontsize_string_in_box(Xgc,sh,sw, -1, str);
      fontid[1]= Max(fid1,fid2);
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], TRUE);
    }
  if ( title[0] != '\0')
    Xgc->graphic_engine->scale->displaystringa(Xgc,title,1);
  if ( x[0] != '\0')
    Xgc->graphic_engine->scale->displaystringa(Xgc,x,2);
  if ( y[0] != '\0')
    Xgc->graphic_engine->scale->displaystringa(Xgc,y,3);
  Xgc->graphic_engine->xset_font(Xgc,fontid[0], fontsize_kp, FALSE);
}

/**
 * nsp_axis_draw:
 * @Xgc: a graphic context
 * @mode: axis drawing mode
 * @scale: axis scale mode
 *
 * draws axis or only rectangle in a graphic frame.
 * mode = strflag[2] ou '1'
 * scale = strflag[1]
 **/

void nsp_axis_draw(BCG *Xgc,char mode, char scale, int grid_color,int bg, int flag)
{
  /* using foreground to draw axis */
  int old_dash,pat, fg, fontid[2];
  char c = mode ;
  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
  fg = Xgc->graphic_engine->xget_foreground(Xgc);
  old_dash = Xgc->graphic_engine->xset_dash(Xgc,1);
  pat = Xgc->graphic_engine->xset_color(Xgc,fg);

  if ( flag == TRUE && Xgc->figure_bg_draw == TRUE )
    {
      /* draw background filled rectangle */
      nsp_draw_filled_rectangle(Xgc,bg);
    }
  
  switch ( c )
    {
    case '0' :
      break ;
    case '2' :
      if ( flag == FALSE ) nsp_draw_frame_rectangle(Xgc);
      break;
    default :
      if ( scale  == '5' || scale =='6' )
	{
	  aplotv1_new(Xgc,mode,grid_color,flag);
	}
      else
	{
	  aplotv2(Xgc,mode,grid_color,flag);
	}
      break;
    }
  Xgc->graphic_engine->xset_dash(Xgc,old_dash);
  Xgc->graphic_engine->xset_color(Xgc,pat);
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1],FALSE);
}

/*--------------------------------------------------------------
 *  aplot: used to draw a box + x and y ticks and scales
 *  xmin,ymin,xmax,ymax : are the boundary values
 *  xnax and ynax gives the ticks numbers ex: nax=[3,7];
 *  will give 8 big ticks (7 intervals) with numbers and
 *  each big interval will be divided in 3 small intervals.
 *----------------------------------------------------------------*/

static void aplotv2(BCG *Xgc,char mode, int grid_color, int flag)
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

  /* draw grid first */
  if ( flag == TRUE )
    {
      if ( grid_color != -1 ) {
	ny=1,nx=3;
	nsp_axis_grid(Xgc,'d','r', x,&nx, &y1, &ny, grid_color,Xgc->scales->logflag[0],seg);
	ny=3,nx=1;
	nsp_axis_grid(Xgc,dir,'r', &x1,&nx, y, &ny, grid_color,Xgc->scales->logflag[1],seg);
      }
    }
  else
    {
      /* x-axis */
      ny=1,nx=3;
      Sci_Axis(Xgc,'d','r',x,&nx,&y1,&ny,NULL,Xgc->scales->Waaint1[0],
	       NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[0],seg);
      
      /* y-axis */
      ny=3,nx=1;
      Sci_Axis(Xgc,dir,'r',&x1,&nx,y,&ny,NULL,Xgc->scales->Waaint1[2],
	       NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[1],seg);
      
      /* frame rectangle */
      if ( c != '4' && c != '5' )
	nsp_draw_frame_rectangle(Xgc);
    }
}

/* here we use frect to find the axes position
 * since given axis are not supposed to fit to frect boundaries
 *
 */

static void aplotv1_new(BCG *Xgc,char mode,int grid_color, int flag)
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

  /* draw grid first */
  if ( flag == TRUE )
    {
      if ( grid_color != -1 ) {
	ny=1,nx=4;
	nsp_axis_grid(Xgc,'d','i', Xgc->scales->xtics,&nx, &y1, &ny, grid_color,Xgc->scales->logflag[0],seg);
	ny=4,nx=1;
	nsp_axis_grid(Xgc,dir,'i', &x1,&nx, Xgc->scales->ytics, &ny, grid_color,Xgc->scales->logflag[1],seg);
      }
    }
  else
    {
      /* x-axis */
      ny=1,nx=4;
      Sci_Axis(Xgc,'d','i',Xgc->scales->xtics,&nx,&y1,&ny,NULL,Xgc->scales->Waaint1[0],
	       NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[0],seg);
      
      /* y-axis */
      ny=4,nx=1;
      Sci_Axis(Xgc,dir,'i',&x1,&nx,Xgc->scales->ytics,&ny,NULL,Xgc->scales->Waaint1[2],
	       NULL,fontsize,textcolor,ticscolor,Xgc->scales->logflag[1],seg);
      
      /* frame rectangle */
      if ( c != '4' && c != '5' )
	nsp_draw_frame_rectangle(Xgc);
    }
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
 *-------------------------------------------------------------*/

static void Sci_Axis(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny, char **str, int subtics,
		     char *format, int fontsize, int textcolor, int ticscolor, char logflag, int seg_flag)
{
  int auto_size=TRUE; /* font are resized automatically */
  char foo[256];
  char c_format[5];
  int Nx=0,Ny=0;
  double angle=0.0,vxx,vxx1,xd,yd,d_barlength,str_offset;
  double vx[2],vy[2];
  double logrect[4];
  int xm[2];
  int flag=0,xx=0,yy=0,posi[2];
  double rect[4];
  int i,barlength, ns=2;
  int fontid[2],fontsize_kp,smallersize=0,color_kp=0;

  if (*nx==3) if (x[2]==0.0) return;
  if (*ny==3) if (y[2]==0.0) return;

  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
  fontsize_kp = fontid[1] ;

  if ( fontsize != -1 )
    {
      fontid[1] = fontsize ;
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], FALSE);
    }

  if ( textcolor != -1 || ticscolor != -1 )
    {
      color_kp = Xgc->graphic_engine->xget_color(Xgc);
    }

  if ( auto_size)
    {
      const char *str = "O";
      double sw = Xgc->scales->Irect.width/20.0;
      double sh = Xgc->scales->Irect.height/20.0;
      int fid1,fid2;
      fid1= nsp_fontsize_string_in_box(Xgc,sw,sh, -1,str);
      fid2= nsp_fontsize_string_in_box(Xgc,sh,sw, -1,str);
      fontid[1]= Max(fid1,fid2);
      smallersize= fontid[1]-2;
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], TRUE);
    }
  else
    {
      if ( fontsize != -1 )
	{
	  fontid[1] = fontsize ;
	  Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], FALSE);
	}
      smallersize=fontid[1]-2;
    }

  if (logflag == 'l' )
    {
      Xgc->graphic_engine->boundingbox(Xgc,"10",xx,yy,logrect);
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
      Sciprintf("Sci_Axis: wrong type argument xy_type\r\n");
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
      barlength = Xgc->scales->Irect.height/40.0;
      d_barlength = barlength/Xgc->scales->Wscy1;
      str_offset = Xgc->scales->Irect.height/60.0/Xgc->scales->Wscy1;

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
	  vx[0] =  inint(XScaleR_d(Xgc->scales,xd,y[0]));
	  /* ym[0] =*/ vy[0] =  inint(YScaleR_d(Xgc->scales,xd,y[0]));
	  xd = x_convert(xy_type, x , Nx-1);
	  vx[1] =  inint(XScaleR_d(Xgc->scales,xd,y[0]));
	  vy[1] =  inint(YScaleR_d(Xgc->scales,xd,y[0]));
	  draw_segment(Xgc,vx,vy, ticscolor);
	}
      /* loop on the ticks */
      for (i=0 ; i < Nx ; i++)
	{
	  vxx=axes_number2str('x',xy_type,foo,str,format,c_format,x,i);
	  Xgc->graphic_engine->boundingbox(Xgc,foo,xx,yy,rect);

	  /* tick is computed in vx,vy and string is displayed at posi[0],posi[1] position */
	  vx[0] =  inint(XScaleR_d(Xgc->scales,vxx,y[0]));
	  vy[0] =  inint(YScaleR_d(Xgc->scales,vxx,y[0]));
	  if ( pos == 'd' )
	    {
	      /* if d_barlength is > 0: ticks are going up and string are displayed
	       * below
	       */
	      xd = vxx;
	      yd = y[0] + d_barlength;
	      vx[1]= inint(XScaleR_d(Xgc->scales,xd,yd));
	      vy[1]= inint(YScaleR_d(Xgc->scales,xd,yd));
	      xd = vxx - (rect[2]/2.0/Xgc->scales->Wscx1);
	      if ( d_barlength > 0 )
		yd = y[0] - str_offset - rect[3]/Xgc->scales->Wscy1;
	      else
		yd = y[0] - str_offset + d_barlength - rect[3]/Xgc->scales->Wscy1;
	      posi[0] = inint(XScaleR_d(Xgc->scales,xd,yd));
	      posi[1] = inint(YScaleR_d(Xgc->scales,xd,yd));
	    }
	  else
	    {
	      xd = vxx;
	      yd = y[0] - d_barlength;
	      vx[1]= inint(XScaleR_d(Xgc->scales,xd,yd));
	      vy[1]= inint(YScaleR_d(Xgc->scales,xd,yd));
	      xd = vxx - (rect[2]/2.0/Xgc->scales->Wscx1);
	      if ( d_barlength > 0 )
		yd = y[0] + str_offset;
	      else
		yd = y[0] + str_offset - d_barlength;
	      posi[0] = inint(XScaleR_d(Xgc->scales,xd,yd));
	      posi[1] = inint(YScaleR_d(Xgc->scales,xd,yd));
	    }
	  /* to vizualize control the box
	  {
	    int val[]={posi[0],posi[1]- barlength*2,barlength*40,barlength*2};
	    Xgc->graphic_engine->drawrectangle(Xgc,val);
	  }
	  */
	  if ( textcolor != -1 )  Xgc->graphic_engine->xset_color(Xgc,textcolor);
	  if ( logflag == 'l' )
	    {
	      Xgc->graphic_engine->xset_font(Xgc,fontid[0],smallersize, auto_size);
	      Xgc->graphic_engine->displaystring(Xgc,foo,posi[0],posi[1],flag,angle,GR_STR_XLEFT, GR_STR_YBOTTOM);
	      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], auto_size);
	      Xgc->graphic_engine->displaystring(Xgc,"10",posi[0] - logrect[2],
						 posi[1] + logrect[3]/2,flag,angle,GR_STR_XLEFT, GR_STR_YBOTTOM);
	    }
	  else
	    {
	      Xgc->graphic_engine->displaystring(Xgc,foo,posi[0],posi[1],flag,angle,GR_STR_XLEFT, GR_STR_YBOTTOM);
	    }
	  if ( textcolor != -1 || ticscolor != -1)  Xgc->graphic_engine->xset_color(Xgc,color_kp);

	  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,NULL,NULL);
	  /* subtics: in fact its the nulber of sub-intervals i.e subtics -1 intervals */
	  if ( i < Nx-1 )
	    {
	      int j;
	      double dx ;
	      if ( logflag == 'l' )
		{
		  /* always 8 subtics in log mode */
		  int xi,xl;
		  xi    = inint(XScaleR_d(Xgc->scales,vxx,y[0]));
		  vy[0] = inint(YScaleR_d(Xgc->scales,vxx,y[0]));
		  yd = (pos == 'd') ? y[0] + d_barlength/2.0 : y[0] - d_barlength/2.0;
		  vy[1] = inint(YScaleR_d(Xgc->scales,vxx,yd));
		  vxx1= x_convert(xy_type,x,i+1);
		  xl=inint(XScaleR_d(Xgc->scales,vxx1,y[0]));
		  for ( j = 2 ; j < 10; j++)
		    {
		      vx[0]=vx[1]= xi +  (xl-xi)*log(j)/log(10.0);
		      Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,NULL,NULL);
		    }
		}
	      else
		{
		  vxx1= x_convert(xy_type,x,i+1);
		  dx = (vxx1-vxx)/subtics;
		  for ( j = 1 ; j < subtics; j++)
		    {
		      xd = vxx+dx*j;
		      vx[0] = inint(XScaleR_d(Xgc->scales,xd,y[0]));
		      vy[0] = inint(YScaleR_d(Xgc->scales,xd,y[0]));
		      yd = (pos == 'd') ? y[0] + d_barlength/2.0 : y[0] - d_barlength/2.0;
		      vx[1]= inint(XScaleR_d(Xgc->scales,xd,yd));
		      vy[1]= inint(YScaleR_d(Xgc->scales,xd,yd));
		      Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,NULL,NULL);
		    }
		}
	    }
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_color(Xgc,color_kp);
	}
      break;
    case 'r' :
    case 'l' :
      /* Vertical axes */
      barlength = Xgc->scales->Irect.width/40.0;
      d_barlength = barlength/Xgc->scales->Wscx1;
      str_offset = Xgc->scales->Irect.width/60.0/Xgc->scales->Wscx1;
      if (str == NULL &&  format == NULL )
	switch (xy_type ) {
	case 'v' : nsp_grformat_e1(c_format,y,Ny);break;
	case 'r' : nsp_grformat_e(c_format,y[0],y[1],(y[1]-y[0])/y[2]);break;
	}
      if ( seg_flag == 1)
	{
	  /* the vertical segment */
	  yd = y_convert(xy_type, y , 0);
	  vx[0] =  inint(XScaleR_d(Xgc->scales,x[0],yd));
	  vy[0] =  inint(YScaleR_d(Xgc->scales,x[0],yd));
	  yd = y_convert(xy_type, y , Ny-1);
	  vx[1] =  inint(XScaleR_d(Xgc->scales,x[0],yd));
	  vy[1] = xm[0]= inint(YScaleR_d(Xgc->scales,x[0],yd));
	  draw_segment(Xgc,vx,vy, ticscolor);
	}
      /* loop on the ticks */
      for (i=0 ; i < Ny ; i++)
	{
	  vxx=axes_number2str('y',xy_type,foo,str,format,c_format,y,i);
	  Xgc->graphic_engine->boundingbox(Xgc,foo,xx,yy,rect);

	  /* tick is computed in vx,vy and string is displayed at posi[0],posi[1] position */

	  vx[0] = inint(XScaleR_d(Xgc->scales,x[0],vxx));
	  vy[0] = inint(YScaleR_d(Xgc->scales,x[0],vxx));
	  if ( pos == 'r' )
	    {
	      xd = x[0] - d_barlength;
	      vx[1] = inint(XScaleR_d(Xgc->scales,xd,vxx));
	      vy[1] = inint(YScaleR_d(Xgc->scales,xd,vxx));
	      if ( d_barlength > 0)
		xd = x[0] + str_offset;
	      else
		xd = x[0] - d_barlength + str_offset;
	      yd = vxx - rect[3]/2.0/Xgc->scales->Wscy1;
	      posi[0]= inint(XScaleR_d(Xgc->scales,xd,yd));
	      posi[1]= inint(YScaleR_d(Xgc->scales,xd,yd));
	    }
	  else
	    {
	      xd = x[0] + d_barlength;
	      vx[1] = inint(XScaleR_d(Xgc->scales,xd,vxx));
	      vy[1] = inint(YScaleR_d(Xgc->scales,xd,vxx));
	      if ( d_barlength > 0)
		xd = x[0] - str_offset - rect[2]/Xgc->scales->Wscx1;
	      else
		xd = x[0] + d_barlength -str_offset - rect[2]/Xgc->scales->Wscx1;
	      yd = vxx - rect[3]/2.0/Xgc->scales->Wscy1;
	      posi[0]= inint(XScaleR_d(Xgc->scales,xd,yd));
	      posi[1]= inint(YScaleR_d(Xgc->scales,xd,yd));
	    }

	  if ( textcolor != -1 ) color_kp=Xgc->graphic_engine->xset_color(Xgc,textcolor);
	  if ( logflag == 'l' )
	    {
	      Xgc->graphic_engine->xset_font(Xgc,fontid[0],smallersize, auto_size);
	      Xgc->graphic_engine->displaystring(Xgc,foo,posi[0],posi[1]- logrect[3]/2,flag,angle,
						 GR_STR_XLEFT, GR_STR_YBOTTOM);
	      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], auto_size);
	      Xgc->graphic_engine->displaystring(Xgc,"10",posi[0] - logrect[2],posi[1],flag,angle,
						 GR_STR_XLEFT, GR_STR_YBOTTOM);
	    }
	  else
	    {
	      Xgc->graphic_engine->displaystring(Xgc,foo,posi[0],posi[1],flag,angle,
						 GR_STR_XLEFT, GR_STR_YBOTTOM);
	    }
	  if ( textcolor != -1 || ticscolor != -1 )  Xgc->graphic_engine->xset_color(Xgc,color_kp);
	  Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,NULL,NULL);
	  /* subtics: in fact its the number of sub-intervals i.e subtics -1 intervals */
	  if ( i < Ny-1 )
	    {
	      int j;
	      double dy ;
	      if ( logflag == 'l' )
		{
		  /* always 8 subtics in log mode */
		  int yi,yl;
		  yi    = inint(YScaleR_d(Xgc->scales,x[0],vxx));
		  vx[0] = inint(XScaleR_d(Xgc->scales,x[0],vxx));
		  xd = ( pos == 'r' ) ? x[0] -d_barlength/2.0: x[0] +d_barlength/2.0;
		  vx[1] = inint(XScaleR_d(Xgc->scales,xd,vxx));
		  vxx1= y_convert(xy_type,y,i+1);
		  yl=inint(YScaleR_d(Xgc->scales,x[0],vxx1));

		  for ( j = 2 ; j < 10; j++)
		    {
		      vy[0]=vy[1]= yi -  (yi-yl)*log(j)/log(10.0);
		      Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,NULL,NULL);
		    }
		}
	      else
		{
		  vxx1= y_convert(xy_type,y,i+1);
		  dy = (vxx1-vxx)/subtics;
		  for ( j = 1 ; j < subtics; j++)
		    {
		      yd= vxx+dy*j;
		      xd= x[0];
		      vx[0] = inint(XScaleR_d(Xgc->scales,xd,yd));
		      vy[0] = inint(YScaleR_d(Xgc->scales,xd,yd));
		      xd = ( pos == 'r' ) ? x[0] -d_barlength/2.0: x[0] +d_barlength/2.0;
		      vx[1]= inint(XScaleR_d(Xgc->scales,xd,yd));
		      vy[1]= inint(YScaleR_d(Xgc->scales,xd,yd));
		      Xgc->graphic_engine->drawsegments(Xgc, vx, vy, ns,NULL,NULL);
		    }
		}
	    }
	  if ( ticscolor != -1 )  Xgc->graphic_engine->xset_color(Xgc,color_kp);
	}
      break;
    }
  /* reset font to its current size */
  if ( fontsize != -1 || logflag == 'l' )
    {
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontsize_kp, FALSE);
    }
  /* reset to current color */
  if ( textcolor != -1 || ticscolor != -1 )
    {
      Xgc->graphic_engine->xset_color(Xgc,color_kp);
    }
}

/* draw a grid which follows the main ticks.
 */

static void nsp_axis_grid(BCG *Xgc,char pos, char xy_type, double *x, int *nx, double *y, int *ny,
			  int grid_color, char logflag, int seg_flag)
{
  int Nx=0, Ny=0, i;
  double vxx,xd,yd,d_barlength;
  double vx[2],vy[2];

  if (*nx==3) if (x[2]==0.0) return;
  if (*ny==3) if (y[2]==0.0) return;

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
      Sciprintf("nsp_axis_grid: wrong type argument xy_type\r\n");
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
      d_barlength = Xgc->scales->Irect.height/Xgc->scales->Wscy1;

      /* loop on the ticks */
      for (i=0 ; i < Nx ; i++)
	{
	  vxx = x_convert(xy_type,x,i);
	  /* tick is computed in vx,vy and string is displayed at posi[0],posi[1] position */
	  vx[0] =  inint(XScaleR_d(Xgc->scales,vxx,y[0]));
	  vy[0] =  inint(YScaleR_d(Xgc->scales,vxx,y[0]));
	  xd = vxx;
	  yd = y[0] + d_barlength;
	  vx[1]= inint(XScaleR_d(Xgc->scales,xd,yd));
	  vy[1]= inint(YScaleR_d(Xgc->scales,xd,yd));
	  draw_segment(Xgc,vx,vy, grid_color);
	  /* loop on subtics for log only  */
	  if ( i < Nx-1 && logflag == 'l')
	    {
	      int j;
	      double vxx1 ;
	      /* always 8 subtics in log mode */
	      int xi,xl;
	      xi    = inint(XScaleR_d(Xgc->scales,vxx,y[0]));
	      vy[0] = inint(YScaleR_d(Xgc->scales,vxx,y[0]));
	      yd = y[0] + d_barlength;
	      vy[1] = inint(YScaleR_d(Xgc->scales,vxx,yd));
	      vxx1= x_convert(xy_type,x,i+1);
	      xl=inint(XScaleR_d(Xgc->scales,vxx1,y[0]));
	      for ( j = 2 ; j < 10; j++)
		{
		  vx[0]=vx[1]= xi +  (xl-xi)*log(j)/log(10.0);
		  draw_segment(Xgc,vx,vy, grid_color);
		}
	    }
	}
      break;
    case 'r' :
    case 'l' :
      /* Vertical axes */
      d_barlength = Xgc->scales->Irect.width/Xgc->scales->Wscx1;
      /* loop on the ticks */
      for (i=0 ; i < Ny ; i++)
	{
	  vxx = y_convert(xy_type,y,i);
	  vx[0] = inint(XScaleR_d(Xgc->scales,x[0],vxx));
	  vy[0] = inint(YScaleR_d(Xgc->scales,x[0],vxx));
	  xd = x[0] + d_barlength;
	  vx[1] = inint(XScaleR_d(Xgc->scales,xd,vxx));
	  vy[1] = inint(YScaleR_d(Xgc->scales,xd,vxx));
	  draw_segment(Xgc,vx,vy, grid_color);
	  /* subtics: in fact its the number of sub-intervals i.e subtics -1 intervals */
	  if ( i < Ny-1 && logflag == 'l' )
	    {
	      int j;
	      double vxx1 ;
	      /* always 8 subtics in log mode */
	      int yi,yl;
	      yi    = inint(YScaleR_d(Xgc->scales,x[0],vxx));
	      vx[0] = inint(XScaleR_d(Xgc->scales,x[0],vxx));
	      xd = x[0] + d_barlength;
	      vx[1] = inint(XScaleR_d(Xgc->scales,xd,vxx));
	      vxx1= y_convert(xy_type,y,i+1);
	      yl=inint(YScaleR_d(Xgc->scales,x[0],vxx1));
	      for ( j = 2 ; j < 10; j++)
		{
		  vy[0]=vy[1]= yi -  (yi-yl)*log(j)/log(10.0);
		  draw_segment(Xgc,vx,vy, grid_color);
		}
	    }
	}
      break;
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
      double rect[4]= {Xgc->scales->Irect.x,Xgc->scales->Irect.y,
		       Xgc->scales->Irect.width,Xgc->scales->Irect.height};
      Xgc->graphic_engine->drawrectangle(Xgc,rect);
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

static void nsp_draw_filled_rectangle(BCG *Xgc,int bg)
{
  int c_color;
  if (bg <= 0) return;
  c_color= Xgc->graphic_engine->xset_color(Xgc,bg);
  if ( Xgc->scales->cosa == 1.0 )
    {
      double rect[4] = {Xgc->scales->Irect.x,Xgc->scales->Irect.y,
			Xgc->scales->Irect.width,Xgc->scales->Irect.height};
      Xgc->graphic_engine->fillrectangle(Xgc,rect);
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
      Xgc->graphic_engine->scale->fillpolyline(Xgc,x,y,4,1);
      /*Xgc->graphic_engine->scale->drawpolyline(Xgc,x,y,4,1);*/
    }
  Xgc->graphic_engine->xset_color(Xgc,c_color);
}

static int nsp_fontsize_string_in_box(BCG *Xgc, double iw, double ih, int fsize, const char *str)
{
  int size_in= 1, size_out=100,size=-1, count=0;
  int fontid[2], check= TRUE;
  double logrect[4];
  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
  /* try to detect if current value is OK. */
  size = (fsize == -1) ? fontid[1] : fsize;
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],size,TRUE);
  Xgc->graphic_engine->boundingbox(Xgc,str,0,0,logrect);
  if ( logrect[2] > iw || logrect[3] > ih )
    {
      size_out = size;
    }
  else
    {
      size_in = size;
      /* is size_out enough ?
       * we first try size + 1
       */
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],size+1,TRUE);
      Xgc->graphic_engine->boundingbox(Xgc,str,0,0,logrect);
      if ( logrect[2] > iw || logrect[3] > ih )
	{
	  size_out = size+1;
	  check = FALSE;
	}
      /* loop to increase upper bound */
      while (1)
	{
	  Xgc->graphic_engine->xset_font(Xgc,fontid[0],size_out,TRUE);
	  Xgc->graphic_engine->boundingbox(Xgc,str,0,0,logrect);
	  if ( !( logrect[2] > iw || logrect[3] > ih) )
	    {
	      size_out *= 2;
	    }
	  else
	    break;
	}
    }
  size= (size_in + size_out)/2;
  /* dichotomic search */
  while ( check  )
    {
      /* Sciprintf("Search with [%d,%d] \n",size_in,size_out); */
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],size,TRUE);
      Xgc->graphic_engine->boundingbox(Xgc,str,0,0,logrect);
      count++;
      if ( logrect[2] > iw || logrect[3] > ih )
	{
	  size_out = size;
	}
      else
	{
	  size_in = size;
	}

      size = (size_in + size_out)/2;
      if ( size_out - size_in <= 1 ) break;
    }
  /* Sciprintf("We quit with %d\n",size); */
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],size,TRUE);
  Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], FALSE);
  return size;
}

static double axes_number2str(char dir,char xy_type,char *res,char **str,const char *format,const char *c_format,const double *x,int i)
{
  double value = (dir=='x') ? x_convert(xy_type,x,i) : y_convert(xy_type,x,i);
  if ( str != NULL)
    {
      sprintf(res,"%s",str[i]);
    }
  else if ( format == NULL)
    {
      /*defaults format **/
      if  ( xy_type == 'i')
	{
	  if ( x[3] == 0 )
	    NumberFormat(res,(int) (x[0]), ((int) x[2]));
	  else
	    NumberFormat(res,((int) (x[0] + i*(x[1]-x[0])/x[3])), ((int) x[2]));
	}
      else
	sprintf(res,c_format,value);
    }
  else
    sprintf(res,format,value);
  return value;
}


static void nsp_legends_box(BCG *Xgc,int n1,
			    const int *mark,const int *mark_size,
			    const int *mark_color,const int *width,const int *color,
			    char **legends,double box[4],
			    int get_box,double xoffset,double yoffset,int fg,const char *sep);

/*----------------------------------------------------
 *  legend="leg1@leg2@leg3@...."
 *  legend contain legends separated by '@'
 *  if nlegend is the number of legends stored in legend
 *  then the function Legends draw  Min(*n1,6,nlegends) legends
 *-----------------------------------------------------*/

void nsp_legends(BCG *Xgc,legends_pos pos,int n1,
		 const int *mark,const int *mark_size, const int *mark_color,const int *width,const int *color,
		 char **legend,const char *sep)
{
  int fg, c_color;
  double rect[4];
  int xx=0,yy=0;
  double box[4];
  double xoffset,yoffset;
  int auto_size=TRUE; /* font are resized automatically */
  int fontid[2],fontsize_kp;

  Xgc->graphic_engine->xget_font(Xgc,fontid, FALSE);
  fontsize_kp = fontid[1] ;
  if ( auto_size)
    {
      const char *str = "O";
      double sw = Xgc->scales->Irect.width/25.0;
      double sh = Xgc->scales->Irect.height/25.0;
      int fid1,fid2;
      fid1= nsp_fontsize_string_in_box(Xgc,sw,sh, -1,str);
      fid2= nsp_fontsize_string_in_box(Xgc,sh,sw, -1,str);
      fontid[1]= Max(fid1,fid2);
      Xgc->graphic_engine->xset_font(Xgc,fontid[0],fontid[1], TRUE);
    }

  Xgc->graphic_engine->boundingbox(Xgc,"pl",xx,yy,rect);
  fg = Xgc->graphic_engine->xget_foreground(Xgc);
  c_color= Xgc->graphic_engine->xset_color(Xgc,fg);

  /* length for the tick zone associated to the legend */
  xoffset= (Xgc->scales->Irect.width)/20.0;
  /* y offset between legends */
  yoffset= rect[3];
  box[0]=box[1]=0;
  nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,TRUE,xoffset,yoffset,fg,sep);

  switch (pos)
    {
    case legend_ur:
      /* upper right position of the legend box */
      box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
      box[1] = Xgc->scales->Irect.y;
      box[0] -= box[2]+rect[2]/2+ xoffset/2.0;
      box[1] += xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,fg,sep);
      break;
    case legend_urm:
      /* upper right margin */
      box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
      box[1] = Xgc->scales->Irect.y;
      box[0] += xoffset/2.0;
      box[1] += xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,fg,sep);
      break;
    case  legend_ul:
      /* upper left position  of the legend box */
      box[0] = Xgc->scales->Irect.x;
      box[1] = Xgc->scales->Irect.y;
      box[0] += xoffset/2.0;
      box[1] += xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,fg,sep);
      break;
    case legend_dr :
      /* down right position  of the legend box */
      box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
      box[1] = Xgc->scales->Irect.y + Xgc->scales->Irect.height;;
      box[0] -= box[2]+rect[2]/2+ xoffset/2.0;
      box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,fg,sep);
      break;
    case legend_drm:
      /* down right margin */
      box[0] = Xgc->scales->Irect.x + Xgc->scales->Irect.width;
      box[1] = Xgc->scales->Irect.y + Xgc->scales->Irect.height;;
      box[0] += xoffset/2.0;
      box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,fg,sep);
      break;
    case legend_dl:
      /* down left  position  of the legend box */
      box[0] = Xgc->scales->Irect.x;
      box[1] = Xgc->scales->Irect.y  + Xgc->scales->Irect.height;;
      box[0] += xoffset/2.0;
      box[1] -= box[3]+rect[3]/2+ xoffset/2.0;
      nsp_legends_box(Xgc,n1,mark,mark_size,mark_color,width,color,legend,box,FALSE,xoffset,yoffset,fg,sep);
      break;
    default:
      Sciprintf("Error: unknow position\n");
      break;
    }
  Xgc->graphic_engine->xset_color(Xgc,c_color);
  Xgc->graphic_engine->xset_font(Xgc,fontid[0], fontsize_kp, FALSE);
}

/*
 * draw the legends and the box
 * or just compute the bounding box according to get_box parameter
 */

static void nsp_draw_legend_polyline(BCG *Xgc, double *vectsx, double *vectsy, int color, int p);
static void nsp_draw_legend_polymark(BCG *Xgc, double *vectsx, double *vectsy, int mark, int p);

static void nsp_legends_box(BCG *Xgc,int n1,
			    const int *mark,const int *mark_size,
			    const int *mark_color,const int *width,const int *color,
			    char **legends,double box[4],
			    int get_box,double xoffset,double yoffset, int fg,const char *sep)
{
  int c_color;
  int c_width =  Xgc->graphic_engine->xget_thickness(Xgc);
  int bg= Xgc->graphic_engine->xget_background(Xgc);
  int xmark[2]={-1,-1};
  int i,xs,ys,flag=0;
  double polyx[2],polyy[2];
  int n1count=0;
  double angle=0.0;
  double rect[4];
  int xi= 1.4*xoffset;
  int yi= box[1]+ yoffset*(1.25);
  /* get current mark */
  Xgc->graphic_engine->xget_mark(Xgc,xmark);

  /* fill the background */
  if ( get_box == FALSE )
    {
      double nbox[4];
      memcpy(nbox,box,4*sizeof(double));
      Xgc->graphic_engine->boundingbox(Xgc,"pl",0,0,rect);
      nbox[0] -= rect[2]/2;
      nbox[1] -= rect[3]/2;
      nbox[2] += rect[2];
      nbox[3] += rect[3];
      c_color= Xgc->graphic_engine->xset_color(Xgc,bg);
      Xgc->graphic_engine->fillrectangle(Xgc,nbox);
      Xgc->graphic_engine->xset_color(Xgc,c_color);
    }

  if ( get_box == TRUE )
    {
      box[2]= xi;
    }

  for ( i = 0 ; i < n1 ; i++)
    {
      char *legend;
      xs=inint(box[0]+xi);
      ys=inint(yi);
      legend = legends[i];
      if (legend != 0 && strcmp(legend,"") != 0 )
	{
	  n1count++;
	  if ( get_box == TRUE )
	    {
	      Xgc->graphic_engine->boundingbox(Xgc,legend,xs,ys,rect);
	      box[2]= Max(box[2],xi+rect[2]);
	    }
	  else
	    {
	      c_color= Xgc->graphic_engine->xset_color(Xgc,fg);
	      Xgc->graphic_engine->displaystring(Xgc,legend,xs,ys,flag,angle
						 ,GR_STR_XLEFT, GR_STR_YBOTTOM);
	      Xgc->graphic_engine->xset_color(Xgc,c_color);
	      Xgc->graphic_engine->boundingbox(Xgc,legend,xs,ys,rect);

	      if ( width[i] >= 0 ) Xgc->graphic_engine->xset_thickness(Xgc,width[i]);

	      if ( color[i] != -2 )
		{
		  /* we neeed to draw a line */
		  polyx[0]=inint(box[0]);polyx[1]=inint(box[0]+xoffset);
		  polyy[0]=inint(yi - rect[3]/2.0);
		  polyy[1]=polyy[0];
		  nsp_draw_legend_polyline(Xgc,polyx,polyy,color[i],2);
		}
	      if ( mark[i] >= -1 )
		{
		  polyx[0]=inint(box[0]+xoffset);
		  polyy[0]=inint(yi- rect[3]/2);
		  if ( mark_size[i] >= 0 ) Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark_size[i]);
		  if ( mark_color[i] >= 0) c_color= Xgc->graphic_engine->xset_color(Xgc, mark_color[i] );
		  nsp_draw_legend_polymark(Xgc,polyx,polyy,mark[i],1);
		  if ( mark_color[i] >= 0) Xgc->graphic_engine->xset_color(Xgc, c_color);
		  if ( mark_size[i] >= 0 ) Xgc->graphic_engine->xset_mark(Xgc,mark[0],mark[1]);
		}
	      if ( width[i] >= 0 ) Xgc->graphic_engine->xset_thickness(Xgc, c_width);
	    }
	}
      yi += yoffset*(1.5);
    }

  if ( get_box == TRUE )
    {
      box[3]= n1count*1.5*yoffset;
    }
  else
    {
      /* enlarged box which is drawn  */
      Xgc->graphic_engine->boundingbox(Xgc,"pl",0,0,rect);
      box[0] -= rect[2]/2;
      box[1] -= rect[3]/2;
      box[2] += rect[2];
      box[3] += rect[3];
      Xgc->graphic_engine->drawrectangle(Xgc,box);
    }
}

static void nsp_draw_legend_polyline(BCG *Xgc, double *vectsx, double *vectsy, int color, int p)
{
  int c_color = 0;
  if ( color >= 0 ) 
    {
      c_color = Xgc->graphic_engine->xset_color(Xgc, color);
    }
  Xgc->graphic_engine->drawpolyline(Xgc,vectsx,vectsy,p,0);
  if ( color >= 0) 
    {
      Xgc->graphic_engine->xset_color(Xgc, c_color);
    }
}

static void nsp_draw_legend_polymark(BCG *Xgc, double *vectsx, double *vectsy, int mark, int p)
{
  int symb[2];
  Xgc->graphic_engine->xget_mark(Xgc,symb);
  if ( mark >= 0)
    {
      Xgc->graphic_engine->xset_mark(Xgc, mark,symb[1]);
    }
  Xgc->graphic_engine->drawpolymark(Xgc,vectsx,vectsy,p);
  if ( mark >= 0)
    {
      Xgc->graphic_engine->xset_mark(Xgc,symb[0],symb[1]);
    }
}

static void draw_segment(BCG *Xgc, double *vx, double *vy, int color)
{
  int ns= 2, color_kp = 0;
  if ( color != -1 )  color_kp= Xgc->graphic_engine->xset_color(Xgc, color);
  Xgc->graphic_engine->drawsegments(Xgc,vx, vy, ns,NULL,NULL);
  if ( color != -1 )  Xgc->graphic_engine->xset_color(Xgc,color_kp);
}


