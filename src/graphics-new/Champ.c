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

#include <math.h>
#include <stdio.h>
#include <string.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"

static double min_of_doubles (const double *x,int n);

/**
 * nsp_champ:
 * @Xgc: 
 * @x: 
 * @y: 
 * @fx: 
 * @fy: 
 * @n1: 
 * @n2: 
 * @strflag: 
 * @brect: 
 * @arfact: 
 * @lstr: 
 *
 * Used for Drawing 2 dimensional vector fields 
 * (fx[i+(*n1)*j], fy[i+(*n1)*j]) is the value of the vector field 
 * at point X=(x[i],y[j]);
 * - fx and fy are (*n1)*(*n2) matrix of double
 * - arfact : a factor by which to multiply the default arrow size 
 *          use 1.0 by defaut 
 * - strflag : a string of length 3 (see plot2d) 
 * - brect=[xmin,ymin,xmax,ymax]    (see plot2d) 
 * - lstr : (used when called from Fortran code)
 * Return value: unsued 
 **/

int nsp_champ(BCG *Xgc,double *x, double *y, double *fx, double *fy, int *n1, 
	      int *n2, char *strflag, double *brect, double *arfact)
{
  nsp_draw_vfield_generic(Xgc,"champ",0,x,y,fx,fy,*n1,*n2,FALSE,strflag,brect,arfact);
  return(0); 
}

/**
 * nsp_champ1:
 * @Xgc: 
 * @x: 
 * @y: 
 * @fx: 
 * @fy: 
 * @n1: 
 * @n2: 
 * @strflag: 
 * @brect: 
 * @arfact: 
 * @lstr: 
 *
 * Used for Drawing 2 dimensional vector fields with colored 
 * arrows.
 * (fx[i+(*n1)*j], fy[i+(*n1)*j]) is the value of the vector field 
 * at point X=(x[i],y[j]);
 * - fx and fy are (*n1)*(*n2) matrix of double
 * - arfact : a factor by which to multiply the default arrow size 
 *          use 1.0 by defaut 
 * - ng_flag : if %t we are using graphic object next arguments are ignored in that case.
 * - strflag : a string of length 3 (see plot2d) 
 * - brect=[xmin,ymin,xmax,ymax]    (see plot2d) 
 * 
 * Return value: unsued 
 **/

int nsp_champ1(BCG *Xgc,double *x, double *y, double *fx, double *fy, int *n1, 
	       int *n2, char *strflag, double *brect, double *arfact)
{
  nsp_draw_vfield_generic(Xgc,"champ1",1,x,y,fx,fy,*n1,*n2,FALSE,strflag,brect,arfact);
  return(0);
}

void nsp_draw_vfield_generic(BCG *Xgc,char *name, int colored, double *x, double *y, 
			     double *fx, double *fy, int n1, int n2,int ng_flag, char *strflag, 
			     double *brect, double *arfact)
{
  int clip_box[4];
  static int aaint[]={2,10,2,10};
  int *xm,*ym,*zm=NULL,i,j,n,na;
  double  xx[2],yy[2], maxx;
  double  nx,ny,sc,sfx,sfy,sfx2,sfy2;
  double  arsize1=0.5,arsize2=0.5;
  int arsize,nn1=1,nn2=2;
  /* get default dash fo rarrows **/
  int cpat,uc;

  uc = Xgc->graphic_engine->xget_usecolor(Xgc);
  if (uc)
    cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  else
    cpat = Xgc->graphic_engine->xget_dash(Xgc);
  /* The arrowsize acording to the windowsize **/
  n=2*(n1)*(n2);
  xx[0]=x[0];xx[1]=x[n1-1];
  yy[0]=y[0];yy[1]=y[n2-1];
  /* Boundaries of the frame **/
  
  if ( ng_flag == FALSE ) 
    {
      update_frame_bounds(Xgc,0,"gnn",xx,yy,&nn1,&nn2,aaint,strflag,brect);
      /* Storing values if using the Record driver */
      if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
	{
	  if (strcmp(name,"champ")==0)
	    store_Champ(Xgc,x,y,fx,fy,&n1,&n2,strflag,brect,arfact);
	  else 
	    store_Champ1(Xgc,x,y,fx,fy,&n1,&n2,strflag,brect,arfact);
	}
    }

  /* Allocation */
  xm = graphic_alloc(0,n,sizeof(int));
  ym = graphic_alloc(1,n,sizeof(int));
  if ( xm == 0 || ym == 0) 
    {
      sciprint("Running out of memory \n");
      return ;
    }      
  if ( colored != 0) {
    zm = graphic_alloc(2,n/2,sizeof(int));
    if (  zm == 0 ) 
      {
	sciprint("Running out of memory \n");
	return ;
      }      
  }
  /* From double to pixels */
  for ( i = 0 ; i < n1 ; i++)
    for ( j =0 ; j < n2 ; j++)
      {
	xm[2*(i +(n1)*j)]= XScale(x[i]);
	ym[2*(i +(n1)*j)]= YScale(y[j]);
      }
  /* Scaling */
  nx=min_of_doubles(x,n1)*Xgc->scales->Wscx1;
  ny=min_of_doubles(y,n2)*Xgc->scales->Wscy1;
  sfx= Xgc->scales->Wscx1;
  sfy= Xgc->scales->Wscy1;
  sfx2= sfx*sfx;
  sfy2= sfy*sfy;
  maxx = sfx2*fx[0]*fx[0]+sfy2*fy[0]*fy[0];
  for (i = 1;  i < (n1)*(n2) ; i++)
    {
      double maxx1 = sfx2*fx[i]*fx[i]+sfy2*fy[i]*fy[i];
      if ( maxx1 > maxx) maxx=maxx1;
    }
  maxx = ( maxx < SMDOUBLE) ? SMDOUBLE : sqrt(maxx);
  sc=maxx;
  /*sc= Min(nx,ny)/sc;*/
  sc= sqrt(nx*nx+ny*ny)/sc;
  sfx *= sc;
  sfy *= sc;
  /** size of arrow **/
  arsize1= ((double) Xgc->scales->WIRect1[2])/(5*(n1));
  arsize2= ((double) Xgc->scales->WIRect1[3])/(5*(n2));
  arsize=  (arsize1 < arsize2) ? inint(arsize1*10.0) : inint(arsize2*10.0) ;
  arsize = (int)(arsize*(*arfact));

  clip_box[0]=Xgc->scales->WIRect1[0];
  clip_box[1]=Xgc->scales->WIRect1[0]+Xgc->scales->WIRect1[2];
  clip_box[2]=Xgc->scales->WIRect1[1];
  clip_box[3]=Xgc->scales->WIRect1[1]+Xgc->scales->WIRect1[3];

  if ( colored == 0 ) 
    {
      int j=0;

      for ( i = 0 ; i < (n1)*(n2) ; i++)
	{
	  int x1n,y1n,x2n,y2n,flag1=0;
	  xm[1+2*j]= (int)(sfx*fx[i]/2+xm[2*i]);
	  xm[2*j]  = (int)(-sfx*fx[i]/2+xm[2*i]);
	  ym[1+2*j]= (int)(-sfy*fy[i]/2+ym[2*i]);
	  ym[2*j]  = (int)(sfy*fy[i]/2+ym[2*i]);
	  clip_line(xm[2*j],ym[2*j],xm[2*j+1],ym[2*j+1],&x1n,&y1n,&x2n,&y2n,&flag1,
		    clip_box[0],clip_box[1],clip_box[2],clip_box[3]);
	  if (flag1 !=0)
	    {
	      /* do not want to clip since if clipped the arrow haed will
		 be badly placed. just eliminate the totally out segments  
		 if (flag1==1||flag1==3) { xm[2*j]=x1n;ym[2*j]=y1n;};
		 if (flag1==2||flag1==3) { xm[2*j+1]=x2n;ym[2*j+1]=y2n;};
	      */
	      j++;
	    } 
	}
      na=2*j;
    }
  else 
    {
      int x1n,y1n,x2n,y2n,flag1=0, whiteid, j=0;
      whiteid=  Xgc->graphic_engine->xget_last(Xgc);
      for ( i = 0 ; i < (n1)*(n2) ; i++)
	{
	  double nor= sqrt(sfx2*fx[i]*fx[i]+sfy2*fy[i]*fy[i]);
	  zm[j] = inint( ((double) whiteid)*(1.0 - nor/maxx));
	  nor= sqrt(fx[i]*fx[i]+fy[i]*fy[i]);
	  xm[1+2*j]= (int)(sfx*fx[i]/(2*nor)+xm[2*i]);
	  xm[2*j]  = (int)(-sfx*fx[i]/(2*nor)+xm[2*i]);
	  ym[1+2*j]= (int)(-sfy*fy[i]/(2*nor)+ym[2*i]);
	  ym[2*j]  = (int)(sfy*fy[i]/(2*nor)+ym[2*i]);
	  clip_line(xm[2*j],ym[2*j],xm[2*j+1],ym[2*j+1],&x1n,&y1n,&x2n,&y2n,&flag1,
		    clip_box[0],clip_box[1],clip_box[2],clip_box[3]);
	  if (flag1 !=0)
	    {
	      /* do not want to clip since if clipped the arrow head will
		 be badly placed. just eliminate the totally out segments 
		 if (flag1==1||flag1==3) { xm[2*j]=x1n;ym[2*j]=y1n;};
		 if (flag1==2||flag1==3) { xm[2*j+1]=x2n;ym[2*j+1]=y2n;};
	      */
	      j++;
	    }
	}
      na=2*j;
    }
  /* Drawing axes */
  if ( ng_flag == FALSE ) axis_draw(Xgc,(strlen(strflag) >= 3) ? strflag[2] : '1', 
				    (strlen(strflag) >= 2) ? strflag[1] : '6');
  /* Drawing the arrows */
  frame_clip_on(Xgc);
  if ( colored ==0) 
    Xgc->graphic_engine->drawarrows(Xgc,xm,ym,na,arsize,&cpat,0);
  else
    Xgc->graphic_engine->drawarrows(Xgc,xm,ym,na,arsize,zm,1);
  frame_clip_off(Xgc);
}

/*----------------------------------
 * Returns min( abs(x)) excluding null x(i)  values 
 * if x==0 then 1 is returned 
 *----------------------------------*/

static double min_of_doubles(const double *x, int n)
{
  int i;
  double dx=1,mindx=1;
  if ( n < 2 ) return(mindx);
  mindx= Abs(x[1]-x[0]);
  mindx = ( mindx != 0 ) ? mindx : 1;
  for ( i = 2 ; i < n ; i++) 
    {
      dx = Abs(x[i]-x[i-1]);
      if ( dx < mindx && dx != 0 ) mindx=dx;
    }
  return(mindx);
}







