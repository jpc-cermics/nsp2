/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 *
 * FIXME: reporter les modifs de 
 *    plot3dg pour les echelles automatiques ds les autres graphiques.
 *
 *--------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <math.h>
#include <stdio.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"
/* #include "nsp/graphics/PloEch.h" */

extern Gengine GL_gengine;

/* FIXME: */
extern double C2F(dsort)();

/* like GEOX or GEOY in PloEch.h but we keep values in xx1 and yy1 for finite check */ 

static double xx1,yy1;

#define PGEOX(x1,y1,z1) inint(xx1= Xgc->scales->Wscx1*(TRX(x1,y1,z1)-Xgc->scales->frect[0]) +Xgc->scales->Wxofset1);
#define PGEOY(x1,y1,z1) inint(yy1= Xgc->scales->Wscy1*(-TRY(x1,y1,z1)+Xgc->scales->frect[3])+Xgc->scales->Wyofset1);

static void C2F(plot3dg) (BCG *Xgc, char *name,
			  int (*func)(BCG *Xgc,int *polyx, int *polyy, int *fill,
				      int whiteid, double zmin, double zmax, double *x, 
				      double *y, double *z, int i, int j, int jj1,
				      int *p, int dc, int fg),
			  double *x, double *y,double *z,
			  int *p, int *q, double *teta,double *alpha,char *legend,
			  int *flag,double *bbox); 

static void C2F(fac3dg) ( BCG *Xgc,char *name, int iflag, double *x, double *y, double *z, 
			  int *cvect, int *p, int *q, double *teta, double *alpha,
			  char *legend, int *flag, double *bbox);

static void dbox(BCG *Xgc,double theta,double alpha);

static void fac3dg_ogl(BCG *Xgc,char *name, int iflag, double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox);

static int DPoints_ogl(BCG *Xgc,double *polyx,double *polyy,double *polyz, int *fill, int whiteid, double zmin, double zmax, 
		double *x, double *y, double *z, int i, int j, int jj1, int *p, int dc, int fg);

static int DPoints1_ogl(BCG *Xgc,double *polyx,double *polyy,double *polyz, int *fill, int whiteid, double zmin, double zmax, 
		 double *x, double *y, double *z, int i, int j, int jj1, int *p, int dc, int fg);

static void plot3dg_ogl(BCG *Xgc,char *name,
			int (*func)(BCG *Xgc,double *polyx, double *polyy, double *polyz,int *fill,
				    int whiteid, double zmin, double zmax, double *x, 
				    double *y, double *z, int i, int j, int jj1,
				    int *p, int dc, int fg),
			double *x, double *y, double *z, int *p, int *q, 
			double *teta, double *alpha, char *legend, int *flag, double *bbox);

typedef enum {plot3d_t ,facettes_t , param3d_t} nsp_plot3d_type;

static void  nsp_plot3d_update_bounds(BCG *Xgc,char *name, double *x, double *y, double *z, int *p, int *q, 
				      double *teta, double *alpha, char *legend, int *flag, double *bbox,
				      double *zmin,double *zmax, nsp_plot3d_type t);

static void AxesStrings(BCG *Xgc,int axflag,const nsp_box_3d *box, char *legend);

static void draw_3d_tics(BCG *Xgc,const nsp_box_3d *box,int axflag,int i1,int i2,int i3,int i4,int x,int y,int flag, 
			 double ang,char *leg,int axis_flag, int xdir,int ofset);

static void BBoxToval(double *x, double *y, double *z, int ind,const double *bbox);

static int nsp_plot_box3d(BCG *Xgc, nsp_box_3d *box);
static int nsp_plot_box3d_ogl(BCG *Xgc,nsp_box_3d *box);

/* FIXME 
 */

extern void fillpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p);
extern void drawpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *drawvect,int n, int p);
extern void drawsegments3D(BCG *Xgc,double *x,double *y,double *z, int n, int *style, int iflag);
static void DrawAxis_ogl(BCG *Xgc, const nsp_box_3d *box, char flag, int style);


/*-------------------------------------------------------------------------
 *
 *  3D Plotting of surfaces given by z=f(x,y)
 *  -->  Entry :
 *     x : vector of size *p
 *     y : vector of size *q
 *     z : vector of size (*p)*(*q) 
 *         z[i+(*p)j]=f(x[i],y[j])
 *     p,q 
 *     teta,alpha (spherical angle in degree of the observation point 
 *                 at infinity )
 *     flag[0]={ n1, n2 }
 *       n1 ==1  with hidden parts 
 *       n1 >=2 without hidden part, ( if flag > 2  the surface is grey )
 *       n1 <=0 only the shape of the surface is painted (with white if 0)
 *     flag[1]=0  ( the current scale are used, for superpose mode )
 *     flag[1]=1  ( bbox is used to fix the box of plot3d )
 *     flag[1]=3  ( isometric, bbox fixes the box of plot3d )
 *     flag[1]=4  ( isometric, bbox fixed by data )
 *     flag[1]=5  ( expanded isometric, bbox fixes the box of plot3d )
 *     flag[1]=6  ( expanded isometric, bbox fixed by data )
 *     flag[2]=0  ( No box around the plot3d )
 *     flag[2]=1  ( petit triedre ds un coin )
 *     flag[2]=2  ( juste triedre cache )
 *     flag[2]=3  ( toute la boite + legendes )
 *     flag[2]=4  ( toute la boite + legendes + axes )
 *     legend="x-legend@y-legend@z-legend"
 *     
 *  <-- The arguments are not modified 
 *-------------------------------------------------------------------------*/



int nsp_plot3d(BCG *Xgc,double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  if ( Xgc->graphic_engine != &GL_gengine ) 
    C2F(plot3dg)(Xgc,"plot3d",DPoints,x,y,z,p,q,teta,alpha,legend,flag,bbox);
  else
    plot3dg_ogl(Xgc,"plot3d",DPoints_ogl,x,y,z,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}

int nsp_plot3d_1(BCG *Xgc,double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  if ( Xgc->graphic_engine != &GL_gengine ) 
    C2F(plot3dg)(Xgc,"plot3d1",DPoints1,x,y,z,p,q,teta,alpha,legend,flag,bbox);
  else
    plot3dg_ogl(Xgc,"plot3d1",DPoints1_ogl,x,y,z,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}


int nsp_plot_fac3d(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  if ( Xgc->graphic_engine != &GL_gengine ) 
    C2F(fac3dg)(Xgc,"fac3d",0,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  else 
    fac3dg_ogl(Xgc,"fac3d",0,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}

int nsp_plot_fac3d_1(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  if ( Xgc->graphic_engine != &GL_gengine ) 
    C2F(fac3dg)(Xgc,"fac3d1",1,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  else 
    fac3dg_ogl(Xgc,"fac3d1",1,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  return(0);

}

int nsp_plot_fac3d_2(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  if ( Xgc->graphic_engine != &GL_gengine ) 
    C2F(fac3dg)(Xgc,"fac3d2",2,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  else
    fac3dg_ogl(Xgc,"fac3d2",2,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}

int nsp_plot_fac3d_3(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  if ( Xgc->graphic_engine != &GL_gengine ) 
    C2F(fac3dg)(Xgc,"fac3d3",3,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  else
    fac3dg_ogl(Xgc,"fac3d3",3,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}


/* 
 * Current geometric transformation and scales 
 * which are used or set according to the value of flag[1]
 *
 */

static void C2F(plot3dg)(BCG *Xgc,char *name,
			 int (*func)(BCG *Xgc,int *polyx, int *polyy, int *fill,
				     int whiteid, double zmin, double zmax, double *x, 
				     double *y, double *z, int i, int j, int jj1,
				     int *p, int dc, int fg),
			 double *x, double *y, double *z, int *p, int *q, 
			 double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  nsp_box_3d box;
  static int fg,fg1,dc;
  /* solid = color of 3D frame */
  int polysize,npoly,whiteid;
  int *polyx,*polyy,*fill;
  static int cache;
  static double zmin,zmax;
  int i,j;

  fg = Xgc->graphic_engine->xget_foreground(Xgc);

  nsp_plot3d_update_bounds(Xgc,name,x,y,z,p,q, teta, alpha,legend,&flag[1],bbox,&zmin,&zmax,plot3d_t);

  /* record current 3d plot */

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    {
      if (strcmp(name,"plot3d")==0) 
	store_Plot3D(Xgc,x,y,z,p,q,teta,alpha,legend,flag,bbox);
      else 
	store_Plot3D1(Xgc,x,y,z,p,q,teta,alpha,legend,flag,bbox);
    }

  SetEch3d1(Xgc,&box,bbox,*teta,*alpha,(long)(flag[1]+1)/2);
  /* Calcule l' Enveloppe Convex de la boite **/
  /* ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,&box,legend,flag[2]);
  /* Le triedre cach\'e **/
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;
  if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
    {
      cache=box.InsideD[0];
      if (flag[2] >=2 ) DrawAxis(Xgc,&box,'D',fg1);
    }
  else 
    {
      cache=box.InsideU[0]-4;
      if (flag[2] >=2 )DrawAxis(Xgc,&box,'U',fg1);
    }
  polyx = graphic_alloc(0,5*(*q),sizeof(int));
  polyy = graphic_alloc(1,5*(*q),sizeof(int));
  fill  = graphic_alloc(2,(*q),sizeof(int));
  if ( (polyx == NULL) || (polyy == NULL) || (fill  == NULL)) 
    {
      Scistring("plot3dg_ : malloc No more Place\n");
      return;
    }

  /* The 3d plot **/

  whiteid = Xgc->graphic_engine->xget_last(Xgc);
  dc =  flag[0];
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;   
  for ( i =0 ; i < (*q)-1 ; i++)   fill[i]= dc ;
  polysize=5;
  npoly= (*q)-1; 
  /* Choix de l'ordre de parcourt **/
  switch (cache)
    {
    case 0 : 
      for ( i =0 ; i < (*p)-1 ; i++)
	{
	  int npolyok=0;
	  for ( j =0 ; j < (*q)-1 ; j++)
	    {
	     npolyok += (*func)(Xgc,polyx,polyy,fill,whiteid,zmin,zmax,
				x,y,z,i,j,npolyok,p,dc,fg1);
	    }
	  if ( npolyok != 0) 
	    Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,fill,npolyok,polysize);
	}
      break;
    case 1 : 
      for ( i =0 ; i < (*p)-1 ; i++)
	{
	  int npolyok=0;
	  for ( j =0  ; j < (*q)-1  ; j++)
	    {
	      npolyok += (*func)(Xgc,polyx,polyy,fill,whiteid,zmin,zmax,
				 x,y,z,i,(*q)-2-j,npolyok,p,dc,fg1);
	   }
	  if ( npolyok != 0) 
	    Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,fill,npolyok,polysize);
	}
      break;
    case 2 : 
      for ( i =(*p)-2 ; i >=0  ; i--)
	{
	  int npolyok=0;
	  for ( j = 0 ; j < (*q)-1 ; j++)
	    {
	     npolyok +=     (*func)(Xgc,polyx,polyy,fill,whiteid,zmin,zmax,
				    x,y,z,i,(*q)-2-j,npolyok,p,dc,fg1);
	   }
	  if ( npolyok != 0) 
	    Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,fill,npolyok,polysize);
	}
      break;
    case 3 : 
      for ( i =(*p)-2 ; i >=0  ; i--)
	{
	  int npolyok=0;
	  for ( j =0 ; j < (*q)-1 ; j++)
	    {
	     npolyok += (*func)(Xgc,polyx,polyy,fill,whiteid,zmin,zmax,
				x,y,z,i,j,npolyok,p,dc,fg1);
	   }
	  if ( npolyok != 0) 
	    Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,fill,npolyok,polysize);
	}
      break;
    }
  /* jpc   if (flag[1] != 0 && flag[2] >=3 ) */
  if ( flag[2] >=3 )
    {
      /* Le triedre que l'on doit voir **/
      if (box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
	DrawAxis(Xgc,&box,'U',fg);
      else 
	DrawAxis(Xgc,&box,'D',fg);
    }
}


static void nsp_plot3d_update_bounds(BCG *Xgc,char *name, double *x, double *y, double *z, int *p, int *q, 
				     double *teta, double *alpha, char *legend, int *flag, double *bbox,double *zmin,
				     double *zmax,nsp_plot3d_type type3d)
{
  int redraw = FALSE;
  int i;
  if (*flag!=0 && *flag!=1 && *flag!=3 && *flag!=5 && *flag != 7 )
    {
      switch (type3d) 
	{
	case plot3d_t :
	  bbox[0]=x[0];bbox[1]=x[*p-1];
	  bbox[2]=y[0];bbox[3]=y[*q-1];
	  *zmin=bbox[4]=(double) Mini(z,*p*(*q)); 
	  *zmax=bbox[5]=(double) Maxi(z,*p*(*q));
	  break;
	case facettes_t: 
	  bbox[0]=(double) Mini(x,*p*(*q));
	  bbox[1]=(double) Maxi(x,*p*(*q));
	  bbox[2]=(double) Mini(y,*p*(*q)); 
	  bbox[3]=(double) Maxi(y,*p*(*q));
	  *zmin=bbox[4]=(double) Mini(z,*p*(*q)); 
	  *zmax=bbox[5]=(double) Maxi(z,*p*(*q));
	  break;
	case param3d_t: 
	  bbox[0]=(double) Mini(x,*p);bbox[1]=(double) Maxi(x,*p);
	  bbox[2]=(double) Mini(y,*p);bbox[3]=(double) Maxi(y,*p);
	  bbox[4]=(double) Mini(z,*p);bbox[5]=(double) Maxi(z,*p);
	  break;
	}
    }
  if ( *flag==1 || *flag==3 || *flag==5 || *flag == 7 ) 
    {
      *zmin=bbox[4];
      *zmax=bbox[5];
    }

  if ( Xgc->scales->scale_flag3d != 0 ) 
    {
      if (*flag == 7 || *flag == 8 )
	{
	  for ( i= 0 ; i < 6 ; i +=2 ) bbox[i]=Min(Xgc->scales->bbox1[i],bbox[i]);
	  for ( i= 1 ; i < 6 ; i +=2 ) bbox[i]=Max(Xgc->scales->bbox1[i],bbox[i]);
	  *zmin=bbox[4];
	  *zmax=bbox[5];

	  if ( bbox[0] < Xgc->scales->bbox1[0] 
	       || bbox[1] > Xgc->scales->bbox1[1] 
	       || bbox[2] < Xgc->scales->bbox1[2] 
	       || bbox[3] > Xgc->scales->bbox1[3] 
	       || bbox[4] < Xgc->scales->bbox1[4] 
	       || bbox[5] > Xgc->scales->bbox1[5] )
	    redraw = TRUE;
	  /* changing flag to the mode used by other recorded 3d plots */
	  *flag=2*Xgc->scales->metric3d;
	  if ( Xgc->scales->theta != *teta ||  Xgc->scales->alpha != *alpha ) 
	    redraw = TRUE;
	}

    }
  else 
    {
      if (*flag == 7 || *flag == 8 )
	{
	  /* we have used a superpose mode and there's no previous 
	   * 3d graphics, we switch to default 
	   */
	  *flag= 1;
	}
    }
  /* switch to mode with ebox to accelerate replot */
  if ( *flag==2 || *flag==4 || *flag==6 || *flag == 8 ) 
     (*flag)--;

  /* Redraw other graphics */
  if ( redraw == TRUE )
    {
      /* just change bbox not flag */
      static int iflag[]={0,0,0,1};
      if ( Xgc->graphic_engine->xget_recording(Xgc) == FALSE ) 
	{
	  Xgc->graphic_engine->xinfo(Xgc,"Auto rescale only works when recording is on " );
	  return;
	}
      Xgc->graphic_engine->clearwindow(Xgc);    
      /* redraw 3d with new bbox */
      tape_replay_new_angles(Xgc,Xgc->CurWindow,iflag,NULL,teta,alpha,bbox);
    }
}


static void C2F(fac3dg)(BCG *Xgc,char *name, int iflag, double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  nsp_box_3d box;
  static int fg1;
  int polysize,npoly,whiteid;
  int *polyx,*polyy,*locindex,fill[4]; /* Modified by polpoth 4/5/2000 fill[4] instead of fill[1] */
  double *polyz;
  static int cache;
  static double zmin,zmax;
  int i;
  /* If Record is on **/


  nsp_plot3d_update_bounds(Xgc,name,x,y,z,p,q, teta, alpha,legend,&flag[1],bbox,&zmin,&zmax,facettes_t);

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) {
      if (strcmp(name,"fac3d")==0) 	
	store_Fac3D(Xgc,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
      else if (strcmp(name,"fac3d1")==0) 	
	store_Fac3D1(Xgc,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
      else if (strcmp(name,"fac3d2")==0) 	
	store_Fac3D2(Xgc,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
      else 
	store_Fac3D3(Xgc,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  }

  SetEch3d1(Xgc,&box,bbox,*teta,*alpha,(long)(flag[1]+1)/2);
  /* Calcule l' Enveloppe Convex de la boite **/
  /* ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,&box,legend,flag[2]);
  /* Le triedre cach\'e **/
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;  
  if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
    {
      cache=box.InsideD[0];
      if (flag[2] >=2 )DrawAxis(Xgc,&box,'D',fg1);
    }
  else 
    {
      cache=box.InsideU[0]-4;
      if (flag[2] >=2 )DrawAxis(Xgc,&box,'U',fg1);
    }
  polyz = graphic_alloc(5,(*q),sizeof(double));
  if ( (polyz == NULL) && (*q) != 0)
    {
      Scistring("plot3dg_ : malloc No more Place\n");
      return;
    }
  /* Allocation  **/
  polyx = graphic_alloc(0,(*p)+1L,sizeof(int));
  polyy = graphic_alloc(1,(*p)+1L,sizeof(int));
  locindex = graphic_alloc(2,(*q),sizeof(int));
  if ( ( polyx == NULL) ||  ( polyy== NULL) || ( locindex== NULL) )
    {
      Scistring("plot3dg_ : malloc No more Place\n");
      return;
    }

  whiteid  = Xgc->graphic_engine->xget_last(Xgc);
  fill[0]=  flag[0];
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  /* tri **/
  for ( i =0 ; i < *q ; i++)
    {
      double zdmin1, zdmin,xmoy=0.00,ymoy=0.00,zmoy=0.00;
      int j=0 ;
      zdmin1=  TRZ(x[ (*p)*i]  ,y[(*p)*i]  ,z[(*p)*i]);
      for ( j= 0 ; j < *p ; j++) 
	{
	  xmoy += x[ j +(*p)*i];  ymoy += y[ j +(*p)*i];  zmoy += z[ j +(*p)*i];
	  zdmin =  TRZ(x[ j +(*p)*i]  ,y[ j +(*p)*i]  ,z[ j +(*p)*i]);
	  if ( zdmin1 < zdmin ) zdmin1= zdmin;
	}
      /* polyz[i]= zdmin1 + TRZ(xmoy,ymoy,zmoy); */
      polyz[i]=  TRZ(xmoy,ymoy,zmoy);
    }
  C2F(dsort)(polyz,q,locindex); 
  for ( i =0 ; i < (*q) ; i++)
    {
      locindex[i] -= 1;  /* Fortran locindex -> C locindex */
      if ( locindex[i] >= *q) 
	sciprint (" index[%d]=%d\r\n",i,locindex[i]);
      locindex[i] = Min(Max(0,locindex[i]),*q-1);
    }
  polysize=(*p)+1; /* jpc : dec 1999 */
  npoly=1; 
  for ( i = (*q)-1 ; i>= 0 ; i--)
    {
      int j,nok=0;
      for ( j =0 ; j < (*p) ; j++)
	{
	  polyx[j]=PGEOX(x[(*p)*locindex[i]+j]  ,y[(*p)*locindex[i]+j]  ,z[(*p)*locindex[i]+j]);
	  if ( finite(xx1) ==0 ) 
	    {
	      nok=1;break;
	    }
	  polyy[j]=PGEOY(x[(*p)*locindex[i]+j]  ,y[(*p)*locindex[i]+j]  ,z[(*p)*locindex[i]+j]);
	  if ( finite(yy1)==0)
	    {
	      nok=1;break;
	    }
	}
      if ( nok == 0) 
	{
	  polyx[(*p)]=polyx[0];
	  polyy[(*p)]=polyy[0];

	  fill[0]=  flag[0];
	  /* Beginning of modified code by E. Segre 4/5/2000 : the call
	     to Xgc->graphic_engine->("xliness" ... is now done in each if/else if block.
	     The case iflag==3 corresponds to the new case, where cvect points
	     to a (*p) times (*q) matrix, in order to do interpolated shading.
	     
	     The new added function are located at the end of thecurrent file (Plo3d.c) */

	  if ( *p >= 2 && ((polyx[1]-polyx[0])*(polyy[2]-polyy[0])-
			   (polyy[1]-polyy[0])*(polyx[2]-polyx[0])) <  0) 
	    {
	      fill[0] = (flag[0] > 0 ) ? fg1 : -fg1 ;
	      /* 
		 The following test fixes a bug : when flag[0]==0 then only the
		 wire frame has to be drawn, and the "shadow" of the surface does
		 not have to appear. polpoth 4/5/2000
		 */
	      
	      if (flag[0]==0) fill[0]=0;
	      /* modification du to E Segre to avoid drawing of hidden facets */
	      if (fg1>0) 
		{
		  Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,fill,npoly,polysize);
		}
	      /*Xgc->graphic_engine->fillpolylines(Xgc,"str",polyx,polyy,fill,&npoly,&polysize);*/
	    }
	  else if ( iflag == 1) 
	    {
	      /* color according to z-level */
	      double zl=0;
	      int k;
	      for ( k= 0 ; k < *p ; k++) 
		zl+= z[(*p)*locindex[i]+k];
	      fill[0]=inint((whiteid-1)*((zl/(*p))-zmin)/(zmax-zmin))+1;
	      if ( flag[0] < 0 ) fill[0]=-fill[0];
	      Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,fill,npoly,polysize);
	    }
	  else if ( iflag == 2) 
	    {
	      /* colors are given by cvect */
	      fill[0]= cvect[locindex[i]];
	      if ( flag[0] < 0 ) fill[0]=-fill[0];
	      Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,fill,npoly,polysize);
	    }
	  else if ( iflag ==3 ) { /* colors are given by cvect of size (*p) times (*q) */
	      int k;
              
	      if ( (*p) != 3 && (*p) !=4 ) {
                Scistring("plot3d1 : interpolated shading is only allowed for polygons with 3 or 4 vertices\n");
 		return;
	      } else {
       	        for ( k= 0 ; k < *p ; k++) fill[k]= cvect[(*p)*locindex[i]+k];
                shade(Xgc,polyx,polyy,fill,*p,flag[0]);
	      }
	  }
	  else Xgc->graphic_engine->fillpolylines(Xgc,polyx,polyy,fill,npoly,polysize);
	  /* End of modified code by polpoth 4/5/2000 */

	}
    } 
  if ( flag[2] >=3 )
    {
      int fg;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      /* Le triedre que l'on doit voir **/
      if (box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
	DrawAxis(Xgc,&box,'U',fg);
      else 
	DrawAxis(Xgc,&box,'D',fg);
    }
}

/*-------------------------------------------------------------------
 *  returns in (polyx, polyy) the polygon for one facet of the surface 
 *--------------------------------------------------------------------*/

int DPoints1(BCG *Xgc,int *polyx, int *polyy, int *fill, int whiteid, double zmin, double zmax, double *x, double *y, double *z, int i, int j, int jj1, int *p, int dc, int fg)
{
  polyx[  5*jj1] =PGEOX(x[i]  ,y[j]  ,z[i+(*p)*j]);
  if ( finite(xx1)==0 )return(0);
  polyy[  5*jj1] =PGEOY(x[i]  ,y[j]  ,z[i+(*p)*j]);
  if ( finite(yy1)==0)return(0);
  polyx[1 +5*jj1]=PGEOX(x[i]  ,y[j+1],z[i+(*p)*(j+1)]);	
  if ( finite(xx1)==0 )return(0);
  polyy[1 +5*jj1]=PGEOY(x[i]  ,y[j+1],z[i+(*p)*(j+1)]);
  if ( finite(yy1)==0)return(0);
  polyx[2 +5*jj1]=PGEOX(x[i+1],y[j+1],z[(i+1)+(*p)*(j+1)]);
  if ( finite(xx1)==0 )return(0);
  polyy[2 +5*jj1]=PGEOY(x[i+1],y[j+1],z[(i+1)+(*p)*(j+1)]);
  if ( finite(yy1)==0)return(0);
  polyx[3 +5*jj1]=PGEOX(x[i+1],y[j]  ,z[(i+1)+(*p)*j]);
  if ( finite(xx1)==0 )return(0);
  polyy[3 +5*jj1]=PGEOY(x[i+1],y[j]  ,z[(i+1)+(*p)*j]);
  if ( finite(yy1)==0)return(0);
  polyx[4 +5*jj1]=PGEOX(x[i]  ,y[j]  ,z[i+(*p)*j]);
  if ( finite(xx1)==0 )return(0);
  polyy[4 +5*jj1]=PGEOY(x[i]  ,y[j]  ,z[i+(*p)*j]);
  if ( finite(yy1)==0)return(0);
  if (((polyx[1+5*jj1]-polyx[0+5*jj1])*(polyy[2+5*jj1]-polyy[0+5*jj1])-
       (polyy[1+5*jj1]-polyy[0+5*jj1])*(polyx[2+5*jj1]-polyx[0+5*jj1])) <  0 
      )
    {
      fill[jj1]= (dc < 0 ) ? -fg : fg ;
    }
  else
    {
      fill[jj1]=inint((whiteid-1)*((1/4.0*( z[i+(*p)*j]+ z[i+1+(*p)*j]+
					    z[i+(*p)*(j+1)]+ z[i+1+(*p)*(j+1)])-zmin)
				   /(zmax-zmin)))+1;
      if ( dc < 0 ) fill[jj1]= -fill[jj1];
    }
  return(1);
  
}

int DPoints(BCG *Xgc,int *polyx, int *polyy, int *fill, int whiteid, double zmin, double zmax, double *x, double *y, double *z, int i, int j, int jj1, int *p, int dc, int fg)
{
#ifdef lint
  whiteid,fill[0],zmin,zmax;
#endif
  polyx[  5*jj1] =PGEOX(x[i]  ,y[j]  ,z[i+(*p)*j]);
  if ( finite(xx1)==0 )return(0);
  polyy[  5*jj1] =PGEOY(x[i]  ,y[j]  ,z[i+(*p)*j]);
  if ( finite(yy1)==0)return(0);
  polyx[1 +5*jj1]=PGEOX(x[i]  ,y[j+1],z[i+(*p)*(j+1)]);
  if ( finite(xx1)==0 )return(0);
  polyy[1 +5*jj1]=PGEOY(x[i]  ,y[j+1],z[i+(*p)*(j+1)]);
  if ( finite(yy1)==0)return(0);
  polyx[2 +5*jj1]=PGEOX(x[i+1],y[j+1],z[(i+1)+(*p)*(j+1)]);
  if ( finite(xx1)==0 )return(0);
  polyy[2 +5*jj1]=PGEOY(x[i+1],y[j+1],z[(i+1)+(*p)*(j+1)]);
  if ( finite(yy1)==0)return(0);
  polyx[3 +5*jj1]=PGEOX(x[i+1],y[j]  ,z[(i+1)+(*p)*j]);
  if ( finite(xx1)==0 )return(0);
  polyy[3 +5*jj1]=PGEOY(x[i+1],y[j]  ,z[(i+1)+(*p)*j]);
  if ( finite(yy1)==0)return(0);
  polyx[4 +5*jj1]=PGEOX(x[i]  ,y[j]  ,z[i+(*p)*j]);
  if ( finite(xx1)==0 )return(0);
  polyy[4 +5*jj1]=PGEOY(x[i]  ,y[j]  ,z[i+(*p)*j]);
  if ( finite(yy1)==0)return(0);
  if (((polyx[1+5*jj1]-polyx[0+5*jj1])*(polyy[2+5*jj1]-polyy[0+5*jj1])-
       (polyy[1+5*jj1]-polyy[0+5*jj1])*(polyx[2+5*jj1]-polyx[0+5*jj1])) <  0)
    fill[jj1]=  (dc != 0 ) ? fg : dc ;
  else
    fill[jj1]= dc;
  return(1);
}

/*-------------------------------------------------------------------
 * param3d function 
 *-------------------------------------------------------------------*/

int nsp_param3d(BCG *Xgc,double *x, double *y, double *z, int *n, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  nsp_box_3d box;
  int style[1],j;
  static int init;
  static int *xm,*ym;
  int fg1;
  double zmin,zmax; /* unused */
  
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_param3d_ogl(Xgc,x,y,z,n,teta,alpha,legend,flag,bbox);
      return 0;
    }

  nsp_plot3d_update_bounds(Xgc,"param3d",x,y,z,n,NULL, teta, alpha,legend,&flag[0],bbox,&zmin,&zmax,param3d_t);

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Param3D(Xgc,x,y,z,n,teta,alpha,legend,flag,bbox);
  
  style[0] = Xgc->graphic_engine->xget_dash(Xgc);

  /* take care here flag[0] is used */
  SetEch3d1(Xgc,&box,bbox,*teta,*alpha,(long)(flag[0]+1)/2);
  /* Calcule l' Enveloppe Convexe de la boite **/
  /* ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,&box,legend,flag[1]);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  /* Le triedre cache **/
  if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
    {
      /* cache=box.InsideD[0];*/
      if (flag[1] >=2 ) DrawAxis(Xgc,&box,'D',fg1);
    }
  else 
    {
      /* cache=box.InsideU[0]-4; */
      if (flag[1] >=2 ) DrawAxis(Xgc,&box,'U',fg1);
    }

  xm = graphic_alloc(0,(*n),sizeof(int));
  ym = graphic_alloc(1,(*n),sizeof(int));
  if ( ( (xm == NULL) && *n != 0 ) || ((ym == NULL) && *n != 0)) 
    {
      Scistring("Param3d : malloc  No more Place\n");
      return(0);
    }
  init = 0 ; 
  while (1) 
    {
      int nel = 0;
      for ( j =init ; j < (*n) ; j++)	 
	{
	  xm[  nel]=PGEOX(x[j],y[j],z[j]);
	  if ( finite(xx1) ==0 ) 
	    {
	      break;
	    }
	  ym[  nel]=PGEOY(x[j],y[j],z[j]);
	  if ( finite(yy1)==0)
	    {
	      break;
	    }
	  nel++;
	}
      if ( nel > 0 ) 
	Xgc->graphic_engine->drawpolylines(Xgc,xm,ym,style,1,nel);
      init = j+1;
      if ( init >= (*n)) break;
    }
  if (flag[1] >=3 ) 
    {
      int fg;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      /* Le triedre que l'on doit voir **/
      if (box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
	DrawAxis(Xgc,&box,'U',fg);
      else 
	DrawAxis(Xgc,&box,'D',fg);
    }
  return(0);
}


/*-------------------------------------------------------------------
 * param3d1 function 
 *-------------------------------------------------------------------*/

int nsp_param3d_1(BCG *Xgc,double *x, double *y, double *z, int *m, int *n, int *iflag, int *colors, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  nsp_box_3d box;
  int style[1],j;
  static int init;
  static int *xm,*ym;
  int fg1,cur;
  double zmin,zmax; /* unused */

  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      nsp_param3d_1_ogl(Xgc,x,y,z,n,teta,alpha,legend,flag,bbox);
      return 1;
    }


  nsp_plot3d_update_bounds(Xgc,"param3d",x,y,z,n,NULL, teta, alpha,legend,&flag[0],bbox,&zmin,&zmax,param3d_t);

  /* If Record is on **/
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Param3D1(Xgc,x,y,z,m,n,iflag,colors,teta,alpha,legend,flag,bbox);
  style[0] = Xgc->graphic_engine->xget_dash(Xgc);

  /* take care here flag[0] is used */
  SetEch3d1(Xgc,&box,bbox,*teta,*alpha,(long)(flag[0]+1)/2);

  /* Calcule l' Enveloppe Convexe de la boite **/
  /* ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,&box,legend,flag[1]);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  /* Le triedre cache **/
  if (box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
    {
      /* cache=box.InsideD[0];*/
      if (flag[1] >=2 ) DrawAxis(Xgc,&box,'D',fg1);
    }
  else 
    {
      /* cache=box.InsideU[0]-4; */
      if (flag[1] >=2 ) DrawAxis(Xgc,&box,'U',fg1);
    }

  xm = graphic_alloc(0,(*m),sizeof(int));
  ym = graphic_alloc(1,(*m),sizeof(int));
  if ( ( (xm == NULL) && *m != 0 ) || ((ym == NULL) && *m != 0)) 
    {
      Scistring("Param3d : malloc  No more Place\n");
      return(0);
    }
  for ( cur=0 ; cur < *n ; cur++)
    {
      init = 0 ; 
      if ( *iflag != 0 ) style[0]=  colors[cur];
      while (1) 
	{
	  int nel = 0,j1;
	  j1= (*m)*cur;
	  for ( j =init ; j < (*m) ; j++)	 
	    {
	      xm[  nel]=PGEOX(x[j+j1],y[j+j1],z[j+j1]);
	      if ( finite(xx1) ==0 ) 
		{
		  break;
		}
	      ym[  nel]=PGEOY(x[j+j1],y[j+j1],z[j+j1]);
	      if ( finite(yy1)==0)
		{
		  break;
		}
	      nel++;
	    }
	  if ( nel > 0 ) 
	    Xgc->graphic_engine->drawpolylines(Xgc,xm,ym,style,1,nel);
	  init = j+1;
	  if ( init >= (*m)) break;
	}
    }
  if (flag[1] >=3 ) 
    {
      int fg;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      /* Le triedre que l'on doit voir **/
      if (box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
	DrawAxis(Xgc,&box,'U',fg);
      else 
	DrawAxis(Xgc,&box,'D',fg);
    }
  return(0);
}


/*-------------------------------------------------------------------
 * box3d 
 *-------------------------------------------------------------------*/

static int nsp_plot_box3d(BCG *Xgc, nsp_box_3d *box)
{
  static int flag[]={1,1,3},fg,fg1;
  /* Calcule l' Enveloppe Convexe de la boite **/
  /* ainsi que les triedres caches ou non **/
  /* FIXME: Convex_Box(Xgc,xbox,ybox,zbox,box.InsideU,box.InsideD,"X@Y@Z",flag[2],Xgc->scales->bbox1); */
  Convex_Box(Xgc,box,"X@Y@Z",flag[2]);
  /* le triedre vu **/
  fg = Xgc->graphic_engine->xget_foreground(Xgc);
  if ( box->z[box->InsideU[0]] > box->z[box->InsideD[0]])
    DrawAxis(Xgc,box,'U',fg);
  else 
    DrawAxis(Xgc,box,'D',fg);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;
  /* Le triedre cache **/
  if (box->z[box->InsideU[0]] > box->z[box->InsideD[0]])
      DrawAxis(Xgc,box,'D',fg1);
  else 
      DrawAxis(Xgc,box,'U',fg1);
  return(0);
}


/*-------------------------------------------------------------------
 * 3d geometric transformation 
 *-------------------------------------------------------------------*/

int nsp_geom3d(BCG *Xgc,double *x, double *y, double *z, int *n)
{
  int j;
  for ( j =0 ; j < (*n) ; j++)	 
    {
      double x1,y1;
      x1=TRX(x[j],y[j],z[j]);
      y1=TRY(x[j],y[j],z[j]);
      z[j]=TRZ(x[j],y[j],z[j]);
      x[j]=x1;
      y[j]=y1;
    }
  return(0);
}



/*-------------------------------------------------------------------
 * functions for 3D scales 
 *-------------------------------------------------------------------*/

void SetEch3d1(BCG *Xgc, nsp_box_3d *box,const double *bbox, double Teta, double Alpha, int flag)
{
  double xmmin,ymmax,xmmax,ymmin,FRect[4],WRect[4],ARect[4];
  double R,xo,yo,zo,dx,dy,dz,hx,hy,hx1,hy1;
  double cost,sint,cosa,sina;
  int ib, i, aaint[]={2,10,2,10},wdim[2], wmax,hmax;
  char logf[2];

  Xgc->scales->scale_flag3d = 1;
  Xgc->scales->alpha = Alpha;
  Xgc->scales->theta = Teta;
  cost=cos((Teta)*M_PI/180.0);
  sint=sin((Teta)*M_PI/180.0);
  cosa=cos((Alpha)*M_PI/180.0);
  sina=sin((Alpha)*M_PI/180.0);
  Xgc->scales->m[0][0]= -sint    ;    Xgc->scales->m[0][1]= cost      ;    Xgc->scales->m[0][2]= 0;
  Xgc->scales->m[1][0]= -cost*cosa;   Xgc->scales->m[1][1]= -sint*cosa;    Xgc->scales->m[1][2]= sina;
  Xgc->scales->m[2][0]=  cost*sina;   Xgc->scales->m[2][1]= sint*sina;     Xgc->scales->m[2][2]= cosa;

  /* in (xbox[8],ybox[8],zbox[8]) are stored the coordinates of the bounding box 
   * which contains the surface stored clockwise (one level then the other)
   *       Z
   *       |
   *      4 ----- 5        
   *       /    /|         
   *     7----6  |__________ Y         
   *      | 0 | / 1        
   *     3----- 2          
   *    /
   *    X
   */
  for (ib=0;ib<6 ;ib++) 
    { 
      if (flag==0) 
	box->bbox[ib]=Xgc->scales->bbox1[ib];
      else 
	box->bbox[ib]=Xgc->scales->bbox1[ib]=bbox[ib];
    }

  Xgc->scales->c[0]=( box->bbox[0]+box->bbox[1])/2.0;
  Xgc->scales->c[1]=( box->bbox[2]+box->bbox[3])/2.0; 
  Xgc->scales->c[2]=( box->bbox[4]+box->bbox[5])/2.0;

  box->x_r[0]=box->bbox[0]; box->y_r[0]=box->bbox[2];  box->z_r[0]=box->bbox[4];
  box->x_r[1]=box->bbox[0]; box->y_r[1]=box->bbox[3];  box->z_r[1]=box->bbox[4];
  box->x_r[2]=box->bbox[1]; box->y_r[2]=box->bbox[3];  box->z_r[2]=box->bbox[4];
  box->x_r[3]=box->bbox[1]; box->y_r[3]=box->bbox[2];  box->z_r[3]=box->bbox[4];
  box->x_r[4]=box->bbox[0]; box->y_r[4]=box->bbox[2];  box->z_r[4]=box->bbox[5];
  box->x_r[5]=box->bbox[0]; box->y_r[5]=box->bbox[3];  box->z_r[5]=box->bbox[5];
  box->x_r[6]=box->bbox[1]; box->y_r[6]=box->bbox[3];  box->z_r[6]=box->bbox[5];
  box->x_r[7]=box->bbox[1]; box->y_r[7]=box->bbox[2];  box->z_r[7]=box->bbox[5];

  for ( i = 0 ; i < 8 ; i++) 
    {
      box->x[i]=TRX(box->x_r[i],box->y_r[i],box->z_r[i]);
      box->y[i]=TRY(box->x_r[i],box->y_r[i],box->z_r[i]);
      box->z[i]=TRZ(box->x_r[i],box->y_r[i],box->z_r[i]);
    }

  /* Calcul des echelles en fonction de la taille du dessin **/
  if ( flag == 1 || flag == 3 )
    {
      xmmin=  (double) Mini(box->x,8L);xmmax= (double) Maxi(box->x,8L);
      ymmax=  (double) - Mini(box->y,8L);
      ymmin=  (double) - Maxi(box->y,8L);
    }
  
  /* code added by es: isoview scaling */
  if ( flag == 2 || flag == 3 )
    {
      /* get current window size */
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      /* FIXME: getscale2d is useless here just use 
       * WRect[i] <-> Xgc->scales->subwin_rect[i];
       */
      getscale2d(Xgc,WRect,FRect,logf,ARect);
      wmax=linint((double)wdim[0] * WRect[2]);
      hmax=linint((double)wdim[1] * WRect[3]); 
    }
  if ( flag == 2 )
    {
      /* radius and center of the sphere circumscribing the box */
      dx=box->bbox[1]-box->bbox[0]; dy=box->bbox[3]-box->bbox[2]; dz=box->bbox[5]-box->bbox[4];
      R= (double) sqrt(dx*dx + dy*dy + dz*dz)/2;
      xo= (double) (box->x[0]+box->x[6])/2 ;
      yo= (double) (box->y[0]+box->y[6])/2 ;
      zo= (double) (box->z[0]+box->z[6])/2 ;
      xmmin=  (double) xo - R ;
      xmmax=  (double) xo + R ;
      ymmax=  (double) -yo + R ;
      ymmin=  (double) -yo - R ;
    }
  if (flag==2 || flag==3)
    {
      /* adjust limits (code adapted from Plo2d.c & Stephane's patch) */
      hx=xmmax-xmmin;
      hy=ymmax-ymmin;
      if ( hx/(double)wmax  < hy/(double)hmax ) 
        {
          hx1=wmax*hy/hmax;
          xmmin=xmmin-(hx1-hx)/2.0;
          xmmax=xmmax+(hx1-hx)/2.0;
        }
      else 
        {
          hy1=hmax*hx/wmax;
          ymmin=ymmin-(hy1-hy)/2.0;
          ymmax=ymmax+(hy1-hy)/2.0;
        }
     }
  if (flag !=0 )
     {
       FRect[0]=xmmin;FRect[1]= -ymmax;FRect[2]=xmmax;FRect[3]= -ymmin;
       set_scale(Xgc,"tftttf",NULL,FRect,aaint,"nn",NULL);
       Xgc->scales->metric3d=flag; /* the metric mode is stored into the list of Scales */
       /* this is used by opengl for zmin zmax and depth */
       Xgc->scales->zfrect[0]= (double) Mini(box->z,8L);
       Xgc->scales->zfrect[1]= (double) Maxi(box->z,8L);
     }
  /* end of code added by es */
}

/*----------------------------------------------------------------
 * Trace un triedre : Indices[4] donne les indices des points qui 
 * constituent le triedre dans les tableaux xbox et ybox 
 *-----------------------------------------------------------------*/ 

void DrawAxis(BCG *Xgc,const nsp_box_3d *box,char flag, int style)
{
  const int *Indices = ( flag == 'U' ) ? box->InsideU : box->InsideD;
  int ixbox[6],iybox[6],npoly=6,lstyle;
  int i,iflag=0;
  for ( i = 0 ; i <= 4 ; i=i+2)
    {
      ixbox[i]=XScale(box->x[Indices[0]]);iybox[i]=YScale(box->y[Indices[0]]);
    }
  ixbox[1]=XScale(box->x[Indices[1]]);iybox[1]=YScale(box->y[Indices[1]]);
  ixbox[3]=XScale(box->x[Indices[2]]);iybox[3]=YScale(box->y[Indices[2]]);
  ixbox[5]=XScale(box->x[Indices[3]]);iybox[5]=YScale(box->y[Indices[3]]);
  lstyle = Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->drawsegments(Xgc,ixbox,iybox,npoly,&style,iflag);
  Xgc->graphic_engine->xset_dash(Xgc,lstyle);
}


/*---------------------------------------------------------------------
 * Trace l'enveloppe convexe de la boite contenant le dessin 
 * et les axes correspondants 
 * et renvoit dans box.InsideU et box.InsideD les indices des points dans xbox et ybox
 * qui sont sur les 2 tri\`edres a l'interieur de l'enveloppe convexe
 * 
 *---------------------------------------------------------------------*/

void Convex_Box(BCG *Xgc, nsp_box_3d *box, char *legend, int flag)
{
  double xmaxi;
  int ind2,ind3,ind, p,n,dvect[1],dash, pat, i;

  /* in (xbox[8],ybox[8],zbox[8]) are stored the coordinates of the bounding box 
   * which contains the surface stored clock wise (one level then the other)
   *       Z
   *       |
   *      4 ----- 5        
   *       /    /|         
   *     7----6  |__________ Y         
   *      | 0 | / 1        
   *     3----- 2          
   *    /
   *    X
   * We thus know for each segment to which kind of axis i.e X,Y or Z it belongs. 
   * For example the sum of the indices of a segment if it bleongs to an X direction can 
   * can only be 11 or 3 (and the reverse is true) This is used in AxesString
   * 
   * we store in xind[8] the indices of the bounding box points which define 
   * when projected on the graphic plan the convex hull. We start at the upper right point 
   * then follow the boundary in the trigonometric way. 
   * For example using the previous graph we will find in xind={5,4,7,3,2,1}; 
   * we also store in InsideU and InsideD the indices of the points defining the two internals 
   * triedra {0,1,3,4} and {6,2,5,7} in the previous example 
   */

  xmaxi=((double) Maxi(box->x,8L));
  ind= -1;
  for (i =0 ; i < 8 ; i++)
    {
      MaxiInd(box->x,8L,&ind,xmaxi);
      if ( ind > 3)
	  {
	    box->xind[0]=ind;
	    break;
	  }
    }
  if (ind < 0 || ind > 8) 
    {
      Scistring("xind out of bounds");
      box->xind[0]=4;
    }
  UpNext(box->xind[0],&ind2,&ind3);
  if ( box->y[box->xind[0]] > box->y[box->xind[0]-4]) 
    {
      if (box->y[ind2] > box->y[ind3]) 
	{
	  box->xind[1]=ind2;box->InsideU[0]=ind3;
	}
      else 
	{
	  box->xind[1]=ind3;box->InsideU[0]=ind2;
	}
    }
  else 
    {
      if (box->y[ind2] < box->y[ind3]) 
	{
	  box->xind[1]=ind2; box->InsideU[0]=ind3;
	}
      else 
	{
	  box->xind[1]=ind3; box->InsideU[0]=ind2;
	}
    }

  UpNext(ind2,&ind2,&ind3);  box->InsideU[1]=box->xind[0];
  box->InsideU[2]=ind2;  box->InsideU[3]= box->InsideU[0]-4;
  box->xind[2]=ind2;
  /* le point en bas qui correspond */
  box->xind[3]=ind2-4;
  DownNext(box->xind[3],&ind2,&ind3);
  if ( box->y[box->xind[0]] > box->y[box->xind[0]-4]) 
    {
      if (box->y[ind2] < box->y[ind3]) 
	{
	  box->xind[4]=ind2;box->InsideD[0]=ind3;
	}
      else  
	{
	  box->xind[4]=ind3;box->InsideD[0]=ind2;
	}
    }
  else
    {
      if (box->y[ind2] > box->y[ind3]) 
	{
	  box->xind[4]=ind2; box->InsideD[0]=ind3;
	}
      else  
	{
	  box->xind[4]=ind3; box->InsideD[0]=ind2;
	}
    }

  DownNext(ind2,&ind2,&ind3);
  box->InsideD[1]=box->xind[3];
  box->InsideD[2]=ind2;
  box->InsideD[3]= box->InsideD[0]+4;
  box->xind[5]=ind2;

  if ( Xgc->graphic_engine != &GL_gengine ) 
    {
      for (i=0; i < 6 ; i++)
	{
	  box->ix[i]=XScale(box->x[box->xind[i]]);
	  box->iy[i]=YScale(box->y[box->xind[i]]);
	}
      box->ix[6]=box->ix[0];box->iy[6]=box->iy[0];
      p=7,n=1;
      dvect[0]= Xgc->graphic_engine->xget_foreground(Xgc);
      /* On trace l'enveloppe cvxe **/
      dash = Xgc->graphic_engine->xset_dash(Xgc,1);
      
      if (flag >=3){
	Xgc->graphic_engine->drawpolylines(Xgc,box->ix,box->iy,dvect,n,p);
      }
      pat = Xgc->graphic_engine->xset_pattern(Xgc,dvect[0]);
      
      if (flag >=3) AxesStrings(Xgc,flag,box,legend);
      Xgc->graphic_engine->xset_pattern(Xgc,pat);
      Xgc->graphic_engine->xset_dash(Xgc,dash);
    }
  else
    {
      for (i=0; i < 6 ; i++)
	{
	  box->xh[i]=box->x_r[box->xind[i]];
	  box->yh[i]=box->y_r[box->xind[i]];
	  box->zh[i]=box->z_r[box->xind[i]];
	}
      box->xh[6]=box->xh[0];box->yh[6]=box->yh[0];box->zh[6]=box->zh[0];
      p=7,n=1;
      dvect[0]= Xgc->graphic_engine->xget_foreground(Xgc);
      /* On trace l'enveloppe cvxe **/
      dash = Xgc->graphic_engine->xset_dash(Xgc,1);
      
      if (flag >=3){
	drawpolylines3D(Xgc,box->xh,box->yh,box->zh,dvect,n,p);
      }
      pat = Xgc->graphic_engine->xset_pattern(Xgc,dvect[0]);
      /* 
       * FIXME
       * if (flag >=3)AxesStrings(Xgc,flag,ixbox,iybox,box->xind,legend,bbox);
       */
      Xgc->graphic_engine->xset_pattern(Xgc,pat);
      Xgc->graphic_engine->xset_dash(Xgc,dash);
    }
}


/* 
 * this routine is called by Convex_Box to add axes strings and graduations 
 * (box->ix,box->iy) contain in pixel the coordinates of the convex hull
 *  box->xind : contains the indices of the convex hull points in the (xbox,ybox,zbox) 
 *        storage. This can be used to detect for each convex hull segment to which 
 *        kind of axis it belongs 
 */

static void AxesStrings(BCG *Xgc,int axflag,const nsp_box_3d *box, char *legend)
{
  int xz[2];
  int iof;
  char *loc,*legx,*legy,*legz; 
  int rect[4],flag=0,x,y;
  double ang=0.0;
  loc=(char *) MALLOC( (strlen(legend)+1)*sizeof(char));
  if ( loc == 0)    
    {
      Scistring("AxesString : No more Place to store Legends\n");
      return;
    }
  strcpy(loc,legend);
  legx=strtok(loc,"@");legy=strtok((char *)0,"@");legz=strtok((char *)0,"@");

  /* FIXME: the z axis could be unified with draw_3d_tics ? */
  Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
  iof = (xz[0]+xz[1])/50;
  x=box->ix[2]-iof ;
  y= ( box->iy[1] >= box->iy[4] ) ? box->iy[3]-iof :  box->iy[2]-iof ;
  if ( axflag>=4)
    {
      double fx,fy,fz,lx,ly,lz;
      int LPoint[2],FPoint[2],Ticsdir[2],xnax[2];
      xnax[0]=5;xnax[1]=2;
      FPoint[0]=box->ix[2];FPoint[1]=box->iy[2];
      LPoint[0]=box->ix[3];LPoint[1]=box->iy[3];
      Ticsdir[0]= -1;
      Ticsdir[1]=0;
      BBoxToval(&fx,&fy,&fz,box->xind[2],box->bbox);
      BBoxToval(&lx,&ly,&lz,box->xind[3],box->bbox);
      TDAxis(Xgc,1L,fz,lz,xnax,FPoint,LPoint,Ticsdir);
    }
  if (legz != 0)
    {
      Xgc->graphic_engine->boundingbox(Xgc,legz,x,y,rect);
      Xgc->graphic_engine->displaystring(Xgc,legz,(x=x - rect[2],x),y,flag ,ang);
    }

  if ( box->iy[1] < box->iy[4] ) /* are we upside down ? */
    {
      /* down left axis */
      x=inint((box->ix[3]+box->ix[4])/2.0 -iof);
      y=inint((1/3.0)*box->iy[3]+(2/3.0)*box->iy[4]+iof);
      if ( box->xind[3]+box->xind[4] == 3 || box->xind[3]+box->xind[4] == 11) 
	{
	  draw_3d_tics(Xgc,box,axflag,3,4,4,5,x,y,flag,ang,legx,2,TRUE,TRUE);
	}
      else 
	{
	  draw_3d_tics(Xgc,box,axflag,3,4,4,5,x,y,flag,ang,legy,2,FALSE,TRUE);
	}
      /* le cot\'e en bas a droite **/
      x=inint((box->ix[4]+box->ix[5])/2+iof);
      y=inint(((2/3.0)*box->iy[4]+(1/3.0)*box->iy[5])+iof);
      if ( box->xind[4]+box->xind[5] == 3 || box->xind[4]+box->xind[5] == 11) 
	{
	  draw_3d_tics(Xgc,box,axflag,4,5,4,3,x,y,flag,ang,legx,3,TRUE,FALSE);
	}
      else 
	{
	  draw_3d_tics(Xgc,box,axflag,4,5,4,3,x,y,flag,ang,legy,3,FALSE,FALSE);
	}
    }
  else 
    {
      /* down left axis */
      x=inint((box->ix[2]+box->ix[1])/2.0 -iof);
      y=inint((1/3.0)*box->iy[2]+(2/3.0)*box->iy[1]+iof);
      if ( box->xind[2]+box->xind[1] == 3 || box->xind[2]+box->xind[1] == 11) 
	{
	  draw_3d_tics(Xgc,box,axflag,2,1,1,0,x,y,flag,ang,legx,2,TRUE,TRUE);
	}
      else 
	{
	  draw_3d_tics(Xgc,box,axflag,2,1,1,0,x,y,flag,ang,legy,2,FALSE,TRUE);
	}
      /* le cot\'e en bas a droite **/
      x=inint((box->ix[1]+box->ix[0])/2+iof);
      y=inint(((2/3.0)*box->iy[1]+(1/3.0)*box->iy[0])+iof);

      if ( box->xind[1]+box->xind[0] == 3 || box->xind[1]+box->xind[0] == 11) 
	{
	  draw_3d_tics(Xgc,box,axflag,1,0,1,2,x,y,flag,ang,legx,3,TRUE,FALSE);
	}
      else 
	{
	  draw_3d_tics(Xgc,box,axflag,1,0,1,2,x,y,flag,ang,legy,3,FALSE,FALSE);
	}
    }
  FREE(loc);
}


static void draw_3d_tics(BCG *Xgc,const nsp_box_3d *box,int axflag,int i1,int i2,int i3,int i4,
			 int x,int y,int flag,double ang,char *leg,int axis_flag, 
			 int xdir,int ofset)
{
  int rect[4];
  if ( axflag>=4)
    {
      double fx,fy,fz,lx,ly,lz;
      int LPoint[2],FPoint[2],Ticsdir[2],xnax[2];
      xnax[0]=5;xnax[1]=2;
      FPoint[0]=box->ix[i1];FPoint[1]=box->iy[i1];
      LPoint[0]=box->ix[i2];LPoint[1]=box->iy[i2];
      Ticsdir[0]=box->ix[i3]-box->ix[i4];
      Ticsdir[1]=box->iy[i3]-box->iy[i4];
      BBoxToval(&fx,&fy,&fz,box->xind[i1],box->bbox);
      BBoxToval(&lx,&ly,&lz,box->xind[i2],box->bbox);
      if ( xdir== TRUE) 
	TDAxis(Xgc,axis_flag,fx,lx,xnax,FPoint,LPoint,Ticsdir); 
      else 
	TDAxis(Xgc,axis_flag,fy,ly,xnax,FPoint,LPoint,Ticsdir);
    }
  if (leg != 0) 
    {
      if ( ofset==TRUE ) 
	{
	  Xgc->graphic_engine->boundingbox(Xgc,leg,x,y,rect);
	  Xgc->graphic_engine->displaystring(Xgc,leg,(x=x-rect[2],x),y,flag,ang);
	}
      else
	Xgc->graphic_engine->displaystring(Xgc,leg,x,y,flag,ang);
    }
}

void MaxiInd(double *vect, int n, int *ind, double maxi)
{
  int i ;
  if ( *ind+1 < n)
    for (i = *ind+1 ; i < n ; i++)
      if ( vect[i] >= maxi)
	{ *ind=i; break;}
}

/* renvoit les indices des points voisins de ind1 sur la face haute 
   de la boite  */

void UpNext(int ind1, int *ind2, int *ind3)
{
  *ind2 = ind1+1;
  *ind3 = ind1-1;
  if (*ind2 == 8) *ind2 = 4;
  if (*ind3 == 3) *ind3 = 7;
}

void DownNext(int ind1, int *ind2, int *ind3)
{
  *ind2 = ind1+1;
  *ind3 = ind1-1;
  if (*ind2 == 4) *ind2 = 0;
  if (*ind3 == -1) *ind3 = 3;
}


void TDAxis(BCG *Xgc,int flag, double FPval, double LPval, int *nax, int *FPoint, int *LPoint, int *Ticsdir)
{
  char fornum[100];
  int i,barlength;
  double xp, dx,dy,ticsx,ticsy,size;
  int xz[2];
  Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
  size = xz[0]>=xz[1] ? xz[1]/50.0 : xz[0]/50.0;
  C2F(TDdrawaxis)(Xgc,size,FPval,LPval,nax,FPoint,LPoint,Ticsdir) ;
  ChoixFormatE(fornum,Min(FPval,LPval),Max(LPval,FPval),
	       Abs((LPval-FPval))/nax[1]);
  xp= FPval;
  barlength=inint(1.2*size);
  dx= ((double) LPoint[0]-FPoint[0])/((double)nax[1]);
  dy= ((double) LPoint[1]-FPoint[1])/((double)nax[1]);
  if ( Ticsdir[0] == 0 && Ticsdir[1] == 0) 
    {
      ticsx= ticsy = 0;
    }
  else 
    {
      ticsx= barlength*( Ticsdir[0])/
	sqrt((double) Ticsdir[0]*Ticsdir[0]+Ticsdir[1]*Ticsdir[1]);
      ticsy= barlength*( Ticsdir[1])/
	sqrt((double) Ticsdir[0]*Ticsdir[0]+Ticsdir[1]*Ticsdir[1]);
    }
  for (i=0; i <= nax[1];i++)
    { double angle=0.0;
      int flag1=0;
      int xx=0,yy=0, posi[2],rect[4];
      char foo[100];/*** JPC : must be cleared properly **/
      double lp;
      lp = xp + i*(LPval-FPval)/((double)nax[1]);
      sprintf(foo,fornum,lp);
      Xgc->graphic_engine->boundingbox(Xgc,foo,xx,yy,rect);
      posi[0]=inint(FPoint[0]+ i*dx + 2*ticsx );
      posi[1]=inint(FPoint[1]+ i*dy + 2*ticsy +rect[3]/2 );
      switch ( flag)
	{
	case 1: posi[0] -= rect[2];
	  /* pour separer ;e 1er arg de l'axe des z de l'axe voisin **/
	  if ( i== nax[1]) posi[1] -= rect[3]/2;
	  break;
	case 2: posi[0] -= rect[2];break;
	}
      Xgc->graphic_engine->displaystring(Xgc,foo,posi[0],posi[1],flag1,angle);
    }
}


void C2F(TDdrawaxis)(BCG *Xgc,double size, double FPval, double LPval, int *nax, int *FPoint, int *LPoint, int *Ticsdir)
{ 
  int i;
  double dx,dy,ticsx,ticsy;
  dx= ((double) LPoint[0]-FPoint[0])/((double)nax[1]*nax[0]);
  dy= ((double) LPoint[1]-FPoint[1])/((double)nax[1]*nax[0]);
  if ( Ticsdir[0] == 0 && Ticsdir[1] == 0) 
    {
      ticsx= ticsy = 0;
    }
  else 
    {
      ticsx= ( Ticsdir[0])/
	sqrt((double) Ticsdir[0]*Ticsdir[0]+Ticsdir[1]*Ticsdir[1]);
      ticsy= ( Ticsdir[1])/
	sqrt((double) Ticsdir[0]*Ticsdir[0]+Ticsdir[1]*Ticsdir[1]);
    }
  for (i=0; i <= nax[1]*nax[0];i++)
    {       
      int siz=2,x[2],y[2],iflag=0,style=0;
      x[0] =linint(FPoint[0]+ ((double)i)*dx );
      y[0] =linint(FPoint[1]+ ((double)i)*dy );
      x[1] =linint(x[0]+ ticsx*size);
      y[1] =linint(y[0]+ ticsy*size);
      Xgc->graphic_engine->drawsegments(Xgc,x,y,siz,&style,iflag);
    }
}


/* Returns the [x,y,z] values of a point given its xbox or ybox indices **/

static void BBoxToval(double *x, double *y, double *z, int ind,const double *bbox)
{
  switch ( ind)
    {
    case 0:	*x=bbox[0],*y=bbox[2],*z=bbox[4];break;
    case 1:	*x=bbox[0],*y=bbox[3],*z=bbox[4];break;
    case 2:	*x=bbox[1],*y=bbox[3],*z=bbox[4];break;
    case 3:	*x=bbox[1],*y=bbox[2],*z=bbox[4];break;
    case 4:	*x=bbox[0],*y=bbox[2],*z=bbox[5];break;
    case 5:	*x=bbox[0],*y=bbox[3],*z=bbox[5];break;
    case 6:	*x=bbox[1],*y=bbox[3],*z=bbox[5];break;
    case 7:	*x=bbox[1],*y=bbox[2],*z=bbox[5];break;
    }
}

/*-------------------------------------
 *  interactive rotation of a 3d plot 
 *--------------------------------------*/

/*-------------------------------------
 * Interactive change of view angle 
 * with full redraw when the mouse moves 
 * The process is initiated by a click and 
 * stopped when the mouse is released 
 *--------------------------------------*/

void I3dRotation(BCG *Xgc)
{
  int box_only = FALSE;
  double theta,alpha, theta_dir;
  int flag[3],pixmode;
  int iflag[]={0,0,0,0};
  int xc,yc;
  double theta0,alpha0;
  int ibutton,iwait=FALSE,istr=0;
  double x0,y0,x,y,bbox[4];
  /* FIXME */
  if ( tape_check_recorded_3D(Xgc,Xgc->CurWindow) == FAIL) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"No 3d recorded plots in your graphic window");
      return;
    }
  if ( Xgc->graphic_engine->xget_recording(Xgc) == FALSE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"3d rotation is not possible when recording is not on" );
      return;
    }
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  Xgc->graphic_engine->xclick(Xgc,"one",&ibutton,&xc,&yc,iwait,FALSE,FALSE,FALSE,istr);
  theta0 = theta = Xgc->scales->theta ;
  alpha0 = alpha = Xgc->scales->alpha ;
  x0 = x = xc;
  y0 = y = yc;
  ibutton=-1;
  while ( ibutton == -1 ) 
    {
      /* */
      theta_dir= ( sin(M_PI*alpha0/180.0) >= 0.0 ) ? 1.0 : -1.0;
      alpha= alpha0 - (y-y0)/5.0;
      theta= theta0 - theta_dir*(x-x0)/5.0;
      x0=x;y0=y;alpha0=alpha;theta0=theta;
      Xgc->graphic_engine->xinfo(Xgc,"alpha=%.2f,theta=%.2f",alpha,theta); 
      if ( box_only == TRUE) 
	{
	  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
	  Xgc->graphic_engine->clearwindow(Xgc);    
	  dbox(Xgc,theta,alpha);
	  if ( pixmode == 1) Xgc->graphic_engine->scale->xset_show(Xgc);
	  Xgc->graphic_engine->xset_recording(Xgc,TRUE);
	}
      else 
	{
	  /* just changes the angles in recorded plots */
	  new_angles_plots(Xgc,Xgc->CurWindow,&theta,&alpha,iflag,flag,bbox);
	  /* immediate redraw */
	  force_redraw(Xgc);
	}
      Xgc->graphic_engine->xgetmouse(Xgc,"one",&ibutton,&xc, &yc,FALSE,TRUE,TRUE,FALSE);
      x=xc;
      y=yc;
    }
  new_angles_plots(Xgc,Xgc->CurWindow,&theta,&alpha,iflag,flag,bbox);
  force_redraw(Xgc);
}


/*
 * Win32, warning when using xor mode
 * colors are changed and black is turned to white
 * so we must use an other pattern than the black one
 * inside dbox
 */

static void dbox(BCG *Xgc,double theta,double alpha)
{
  nsp_box_3d box;
#ifdef WIN32
  int verbose=0,pat,pat1=3,narg;
  pat = Xgc->graphic_engine->xset_pattern(pat1);
#endif

  SetEch3d1(Xgc,&box,Xgc->scales->bbox1,theta,alpha,Xgc->scales->metric3d);
  if ( Xgc->graphic_engine != &GL_gengine ) 
    nsp_plot_box3d(Xgc,&box);
  else
    nsp_plot_box3d_ogl(Xgc,&box);

#ifdef WIN32
   Xgc->graphic_engine->xset_pattern(pat);
#endif
}


/**************** New functions for interpolated shading **********************
 *
 *  Added by polpoth 4/5/2000
 *
 *******************************************************************************/


/*---------------------------------------------------------------------------------
 *This function sorts the vertices such that the color value is in decreasing order
 *---------------------------------------------------------------------------------*/

int  triangleSort(int *polyxin, int *polyyin, int *fillin, int *polyx, int *polyy, int *fill)
{ 
  int tmp,k;
  for (k=0;k<3;k++) {polyx[k]=polyxin[k]; polyy[k]=polyyin[k]; fill[k]=fillin[k];}
      
  if (fill[0]<fill[1]) {  
    tmp=fill[0]; fill[0]=fill[1]; fill[1]=tmp;
    tmp=polyx[0]; polyx[0]=polyx[1]; polyx[1]=tmp;
    tmp=polyy[0]; polyy[0]=polyy[1]; polyy[1]=tmp;
  }
  if (fill[0]<fill[2]) {  
    tmp=fill[0]; fill[0]=fill[2]; fill[2]=tmp;
    tmp=polyx[0]; polyx[0]=polyx[2]; polyx[2]=tmp;
    tmp=polyy[0]; polyy[0]=polyy[2]; polyy[2]=tmp;
  }
  if (fill[1]<fill[2]) {  
    tmp=fill[1]; fill[1]=fill[2]; fill[2]=tmp;
    tmp=polyx[1]; polyx[1]=polyx[2]; polyx[2]=tmp;
    tmp=polyy[1]; polyy[1]=polyy[2]; polyy[2]=tmp;
  }
  return 0;
}


/*-----------------------------------------------------------------------
 * This is the main shading function. When the polygone has 4 vertices, it
 * is splitted in two triangles and shade() is recursively called twice.
 * Author : mottelet 2000 
 * XXXX: remplacer les malloc par graphic_alloc pour uniformiser avec les autres 
 *       routines 
 * FIXME: semble avoir un Pb avec une orientation non conforme 
 *        avec l'orientation standard 
 * FIXME: si des polygones ont trop de cotés shade ne marche pas.
 *-----------------------------------------------------------------------*/

int shade(BCG *Xgc,int *polyx, int *polyy, int *fill, int polysize, int flag)
{
   int px[5],py[5],fil[4],is[3],ie[3],n[3];
   int npoly=1,k,col,cols,psize,i,s,e;
   int polyxs[4],polyys[4],fills[4],*x[3],*y[3];
   double dx,dy;

   if (polysize == 3) { /* The triangle case */
 
     triangleSort(polyx,polyy,fill,polyxs,polyys,fills);
  
     is[0]=0; ie[0]=1;
     is[1]=1; ie[1]=2;
     is[2]=0; ie[2]=2;
     
     /* Computation of coordinates of elementary polygons for each side */
     
     for(i=0;i<3;i++) {

        s=is[i];
	e=ie[i];
	n[i]=fills[s]-fills[e];

        if (n[i]) {
	
           x[i]=(int *)malloc((n[i]+2)*sizeof(int));
	   y[i]=(int *)malloc((n[i]+2)*sizeof(int)); 
	   if (x[i]==NULL || y[i]==NULL) {
		Scistring("shade : malloc No more Place\n");
		return 0;
	   }
		
           dx=((double)(polyxs[e]-polyxs[s]))/(double)n[i];
	   dy=((double)(polyys[e]-polyys[s]))/(double)n[i];

           x[i][0]=polyxs[s];
	   y[i][0]=polyys[s];
	   
           for(k=0;k<n[i];k++) {
             x[i][k+1]=linint((double)polyxs[s] + (0.5+k)*dx);
	     y[i][k+1]=linint((double)polyys[s] + (0.5+k)*dy);
           }
	   
           x[i][n[i]+1]=polyxs[e];
           y[i][n[i]+1]=polyys[e];
        }
     }
     
     /* Fill the whole triangle with color fill[1] if all colors are equal */
         
     if (!n[0] && !n[1]) {

          psize=3;
          col=fills[0];
          Xgc->graphic_engine->fillpolylines(Xgc,polyxs,polyys,(cols=-col,&cols),npoly,psize);
          return(0);
     }
     
     if (n[0]) {
          psize=4;
	  col=fills[0];  
          for(i=0;i<=n[0];i++) {
	     px[0]=x[2][i]; px[1]=x[0][i]; px[2]=x[0][i+1]; px[3]=x[2][i+1];
	     py[0]=y[2][i]; py[1]=y[0][i]; py[2]=y[0][i+1]; py[3]=y[2][i+1];
	     Xgc->graphic_engine->fillpolylines(Xgc,px,py,(cols=-col,&cols),npoly,psize);
             col--;
	  }
	  free(x[0]);
	  free(y[0]);
     }
     
     if (n[1]) {
          psize=4;
	  col=fills[1];
          for(i=0;i<=n[1];i++) {
	     px[0]=x[2][n[0]+i]; px[1]=x[1][i]; px[2]=x[1][i+1]; px[3]=x[2][n[0]+i+1];
	     py[0]=y[2][n[0]+i]; py[1]=y[1][i]; py[2]=y[1][i+1]; py[3]=y[2][n[0]+i+1];
	     Xgc->graphic_engine->fillpolylines(Xgc,px,py,(cols=-col,&cols),npoly,psize);
             col--;
	  }
          free(x[1]);
	  free(y[1]);  
     }

     if (n[2]) {
        free(x[2]);
	free(y[2]);
     }

   }
   
   else { /* The 4 vertices case  */
     
      px[0]=polyx[0]; px[1]=polyx[1]; px[2]=polyx[2];
      py[0]=polyy[0]; py[1]=polyy[1]; py[2]=polyy[2];
      fil[0]=fill[0]; fil[1]=fill[1]; fil[2]=fill[2];
      shade(Xgc,px,py,fil,3,flag);
      px[0]=polyx[0]; px[1]=polyx[2]; px[2]=polyx[3];
      py[0]=polyy[0]; py[1]=polyy[2]; py[2]=polyy[3];
      fil[0]=fill[0]; fil[1]=fill[2]; fil[2]=fill[3];
      shade(Xgc,px,py,fil,3,flag);
   }
   return 0;
}     


/*
 * OpenGL version 
 */



static void fac3dg_ogl(BCG *Xgc,char *name, int iflag, double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  nsp_box_3d box;
  static int fg1;
  int polysize,npoly,whiteid;
  int *polyx,*polyy,*locindex,fill[4]; /* Modified by polpoth 4/5/2000 fill[4] instead of fill[1] */
  double *polyz;
  static int cache;
  static double zmin,zmax;
  int i;

  nsp_plot3d_update_bounds(Xgc,name,x,y,z,p,q, teta, alpha,legend,&flag[1],bbox,&zmin,&zmax,facettes_t);

  /* If Record is on **/
  
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) {
      if (strcmp(name,"fac3d")==0) 	
	store_Fac3D(Xgc,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
      else if (strcmp(name,"fac3d1")==0) 	
	store_Fac3D1(Xgc,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
      else if (strcmp(name,"fac3d2")==0) 	
	store_Fac3D2(Xgc,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
      else 
	store_Fac3D3(Xgc,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  }

  SetEch3d1(Xgc,&box,bbox,*teta,*alpha,(long)(flag[1]+1)/2);
  /* Calcule l' Enveloppe Convex de la boite **/
  /* ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,&box,legend,flag[2]);
  /* Le triedre cach\'e **/
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;  
  if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
    {
      cache=box.InsideD[0];
      if (flag[2] >=2 )DrawAxis_ogl(Xgc,&box,'D',fg1);
    }
  else 
    {
      cache=box.InsideU[0]-4;
      if (flag[2] >=2 )DrawAxis_ogl(Xgc,&box,'U',fg1);
    }
  polyz = graphic_alloc(5,(*q),sizeof(double));
  if ( (polyz == NULL) && (*q) != 0)
    {
      Scistring("plot3dg_ : malloc No more Place\n");
      return;
    }
  /* Allocation  **/
  polyx = graphic_alloc(0,(*p)+1L,sizeof(int));
  polyy = graphic_alloc(1,(*p)+1L,sizeof(int));
  locindex = graphic_alloc(2,(*q),sizeof(int));
  if ( ( polyx == NULL) ||  ( polyy== NULL) || ( locindex== NULL) )
    {
      Scistring("plot3dg_ : malloc No more Place\n");
      return;
    }

  whiteid  = Xgc->graphic_engine->xget_last(Xgc);
  fill[0]=  flag[0];
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  /* tri **/
  for ( i =0 ; i < *q ; i++)
    {
      double zdmin1, zdmin,xmoy=0.00,ymoy=0.00,zmoy=0.00;
      int j=0 ;
      zdmin1=  TRZ(x[ (*p)*i]  ,y[(*p)*i]  ,z[(*p)*i]);
      for ( j= 0 ; j < *p ; j++) 
	{
	  xmoy += x[ j +(*p)*i];  ymoy += y[ j +(*p)*i];  zmoy += z[ j +(*p)*i];
	  zdmin =  TRZ(x[ j +(*p)*i]  ,y[ j +(*p)*i]  ,z[ j +(*p)*i]);
	  if ( zdmin1 < zdmin ) zdmin1= zdmin;
	}
      /* polyz[i]= zdmin1 + TRZ(xmoy,ymoy,zmoy); */
      polyz[i]=  TRZ(xmoy,ymoy,zmoy);
    }
  C2F(dsort)(polyz,q,locindex); 
  for ( i =0 ; i < (*q) ; i++)
    {
      locindex[i] -= 1;  /* Fortran locindex -> C locindex */
      if ( locindex[i] >= *q) 
	sciprint (" index[%d]=%d\r\n",i,locindex[i]);
      locindex[i] = Min(Max(0,locindex[i]),*q-1);
    }
  polysize=(*p)+1; /* jpc : dec 1999 */
  npoly=1; 
  for ( i = (*q)-1 ; i>= 0 ; i--)
    {
      int j,nok=0;
      for ( j =0 ; j < (*p) ; j++)
	{
	  polyx[j]=PGEOX(x[(*p)*locindex[i]+j]  ,y[(*p)*locindex[i]+j]  ,z[(*p)*locindex[i]+j]);
	  if ( finite(xx1) ==0 ) 
	    {
	      nok=1;break;
	    }
	  polyy[j]=PGEOY(x[(*p)*locindex[i]+j]  ,y[(*p)*locindex[i]+j]  ,z[(*p)*locindex[i]+j]);
	  if ( finite(yy1)==0)
	    {
	      nok=1;break;
	    }
	}
      if ( nok == 0) 
	{
	  polyx[(*p)]=polyx[0];
	  polyy[(*p)]=polyy[0];

	  fill[0]=  flag[0];
	  /* Beginning of modified code by E. Segre 4/5/2000 : the call
	     to Xgc->graphic_engine->("xliness" ... is now done in each if/else if block.
	     The case iflag==3 corresponds to the new case, where cvect points
	     to a (*p) times (*q) matrix, in order to do interpolated shading.
	     
	     The new added function are located at the end of thecurrent file (Plo3d.c) */

	  if ( *p >= 2 && ((polyx[1]-polyx[0])*(polyy[2]-polyy[0])-
			   (polyy[1]-polyy[0])*(polyx[2]-polyx[0])) <  0) 
	    {
	      fill[0] = (flag[0] > 0 ) ? fg1 : -fg1 ;
	      /* 
		 The following test fixes a bug : when flag[0]==0 then only the
		 wire frame has to be drawn, and the "shadow" of the surface does
		 not have to appear. polpoth 4/5/2000
		 */
	      
	      if (flag[0]==0) fill[0]=0;
	      /* modification du to E Segre to avoid drawing of hidden facets */
	      if (fg1>0) 
		{
		  fillpolylines3D(Xgc,x+(*p)*locindex[i],y+(*p)*locindex[i],z+(*p)*locindex[i],fill,npoly,polysize-1);
		}
	      /*Xgc->graphic_engine->fillpolylines(Xgc,"str",polyx,polyy,fill,&npoly,&polysize);*/
	    }
	  else if ( iflag == 1) 
	    {
	      /* color according to z-level */
	      double zl=0;
	      int k;
	      for ( k= 0 ; k < *p ; k++) 
		zl+= z[(*p)*locindex[i]+k];
	      fill[0]=inint((whiteid-1)*((zl/(*p))-zmin)/(zmax-zmin))+1;
	      if ( flag[0] < 0 ) fill[0]=-fill[0];
	      fillpolylines3D(Xgc,x+(*p)*locindex[i],y+(*p)*locindex[i],z+(*p)*locindex[i],fill,npoly,polysize-1);
	    }
	  else if ( iflag == 2) 
	    {
	      /* colors are given by cvect */
	      fill[0]= cvect[locindex[i]];
	      if ( flag[0] < 0 ) fill[0]=-fill[0];
	      fillpolylines3D(Xgc,x+(*p)*locindex[i],y+(*p)*locindex[i],z+(*p)*locindex[i],fill,npoly,polysize-1);
	    }
	  else if (iflag ==3) { /* colors are given by cvect of size (*p) times (*q) */
	      int k;
	      if ( (*p) != 3 && (*p) !=4 ) {
                Scistring("plot3d1 : interpolated shading is only allowed for polygons with 3 or 4 vertices\n");
		return;
	      } else {
       	        for ( k= 0 ; k < *p ; k++) fill[k]= cvect[(*p)*locindex[i]+k];
		/* Sciprintf("shade not implemented for opengl fac3dg\n");*/
                /* FIXME: shade(Xgc,polyx,polyy,fill,*p,flag[0]); */
		fillpolylines3D(Xgc,x+(*p)*locindex[i],y+(*p)*locindex[i],z+(*p)*locindex[i],fill,npoly,polysize-1);
	      }
	  }
	  else
	    {
	      fillpolylines3D(Xgc,x+(*p)*locindex[i],y+(*p)*locindex[i],z+(*p)*locindex[i],fill,npoly,polysize-1);
	    }
	  /* End of modified code by polpoth 4/5/2000 */

	}
    } 
  if ( flag[2] >=3 )
    {
      int fg;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      /* Le triedre que l'on doit voir **/
      if (box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
	DrawAxis_ogl(Xgc,&box,'U',fg);
      else 
	DrawAxis_ogl(Xgc,&box,'D',fg);
    }
  
}


/* 
 * Current geometric transformation and scales 
 * which are used or set according to the value of flag[1]
 *
 */

static void plot3dg_ogl(BCG *Xgc,char *name,
			int (*func)(BCG *Xgc,double *polyx, double *polyy, double *polyz,int *fill,
				    int whiteid, double zmin, double zmax, double *x, 
				    double *y, double *z, int i, int j, int jj1,
				    int *p, int dc, int fg),
			double *x, double *y, double *z, int *p, int *q, 
			double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  nsp_box_3d box;
  static int fg,fg1,dc;
  /* solid = color of 3D frame */
  int polysize,npoly,whiteid;
  double *polyx,*polyy,*polyz;
  int *fill;
  static int cache;
  static double zmin,zmax;
  int i,j;

  nsp_plot3d_update_bounds(Xgc,name,x,y,z,p,q, teta, alpha,legend,&flag[1],bbox,&zmin,&zmax,plot3d_t);

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    {
      if (strcmp(name,"plot3d")==0) 
	store_Plot3D(Xgc,x,y,z,p,q,teta,alpha,legend,flag,bbox);
      else 
	store_Plot3D1(Xgc,x,y,z,p,q,teta,alpha,legend,flag,bbox);
    }

  fg = Xgc->graphic_engine->xget_foreground(Xgc);
 
  SetEch3d1(Xgc,&box,bbox,*teta,*alpha,(long)(flag[1]+1)/2);

  /* Calcule l' Enveloppe Convex de la boite **/
  /* ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,&box,legend,flag[2]);
  /* Le triedre cach\'e **/
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;
  if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
    {
      cache=box.InsideD[0];
      if (flag[2] >=2 )DrawAxis_ogl(Xgc,&box,'D',fg1);
    }
  else 
    {
      cache=box.InsideU[0]-4;
      if (flag[2] >=2 )DrawAxis_ogl(Xgc,&box,'U',fg1);
    }
  polyx = graphic_alloc(0,5*(*q),sizeof(double));
  polyy = graphic_alloc(1,5*(*q),sizeof(double));
  polyz = graphic_alloc(5,5*(*q),sizeof(double));
  fill  = graphic_alloc(2,(*q),sizeof(int));
  if ( (polyx == NULL) || (polyy == NULL) || (polyz == NULL) ||(fill  == NULL)) 
    {
      Scistring("plot3dg_ : malloc No more Place\n");
      return;
    }

  /* The 3d plot **/

  whiteid = Xgc->graphic_engine->xget_last(Xgc);
  dc =  flag[0];
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;   
  for ( i =0 ; i < (*q)-1 ; i++)   fill[i]= dc ;
  polysize=5;
  npoly= (*q)-1; 
  /* Choix de l'ordre de parcourt **/
  switch (cache)
    {
    case 0 : 
      for ( i =0 ; i < (*p)-1 ; i++)
	{
	  int npolyok=0;
	  for ( j =0 ; j < (*q)-1 ; j++)
	    {
	     npolyok += (*func)(Xgc,polyx,polyy,polyz,fill,whiteid,zmin,zmax,
				x,y,z,i,j,npolyok,p,dc,fg1);
	    }
	  if ( npolyok != 0) 
	    fillpolylines3D(Xgc,polyx,polyy,polyz,fill,npolyok,polysize);
	}
      break;
    case 1 : 
      for ( i =0 ; i < (*p)-1 ; i++)
	{
	  int npolyok=0;
	  for ( j =0  ; j < (*q)-1  ; j++)
	    {
	      npolyok += (*func)(Xgc,polyx,polyy,polyz,fill,whiteid,zmin,zmax,
				 x,y,z,i,(*q)-2-j,npolyok,p,dc,fg1);
	   }
	  if ( npolyok != 0) 
	    fillpolylines3D(Xgc,polyx,polyy,polyz,fill,npolyok,polysize);
	}
      break;
    case 2 : 
      for ( i =(*p)-2 ; i >=0  ; i--)
	{
	  int npolyok=0;
	  for ( j = 0 ; j < (*q)-1 ; j++)
	    {
	     npolyok +=     (*func)(Xgc,polyx,polyy,polyz,fill,whiteid,zmin,zmax,
				    x,y,z,i,(*q)-2-j,npolyok,p,dc,fg1);
	   }
	  if ( npolyok != 0) 
	    fillpolylines3D(Xgc,polyx,polyy,polyz,fill,npolyok,polysize);
	}
      break;
    case 3 : 
      for ( i =(*p)-2 ; i >=0  ; i--)
	{
	  int npolyok=0;
	  for ( j =0 ; j < (*q)-1 ; j++)
	    {
	     npolyok += (*func)(Xgc,polyx,polyy,polyz,fill,whiteid,zmin,zmax,
				x,y,z,i,j,npolyok,p,dc,fg1);
	   }
	  if ( npolyok != 0) 
	    fillpolylines3D(Xgc,polyx,polyy,polyz,fill,npolyok,polysize);
	}
      break;
    }
  /* jpc   if (flag[1] != 0 && flag[2] >=3 ) */
  if ( flag[2] >=3 )
    {
      /* Le triedre que l'on doit voir **/
      if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
	DrawAxis_ogl(Xgc,&box,'U',fg);
      else 
	DrawAxis_ogl(Xgc,&box,'D',fg);
    }
}



/*-------------------------------------------------------------------
 *  returns in (polyx, polyy) the polygon for one facet of the surface 
 *--------------------------------------------------------------------*/

/* FIXME orientation is not properly calculated here  */

int DPoints1_ogl(BCG *Xgc,double *polyx,double *polyy,double *polyz, int *fill, int whiteid, double zmin, double zmax, 
		 double *x, double *y, double *z, int i, int j, int jj1, int *p, int dc, int fg)
{
  polyx[  5*jj1] =x[i];
  polyy[  5*jj1] =y[j];
  polyz[  5*jj1] =z[i+(*p)*j];
  if (  finite(z[i+(*p)*j])==0) return 0;
  polyx[1 +5*jj1]=x[i];
  polyy[1 +5*jj1]=y[j+1];
  polyz[1 +5*jj1]=z[i+(*p)*(j+1)];
  if (  finite(z[i+(*p)*(j+1)])==0) return 0;
  polyx[2 +5*jj1]=x[i+1];
  polyy[2 +5*jj1]=y[j+1];
  polyz[2 +5*jj1]=z[(i+1)+(*p)*(j+1)];
  if (  finite(z[i+1+(*p)*(j+1)])==0) return 0;
  polyx[3 +5*jj1]=x[i+1];
  polyy[3 +5*jj1]=y[j];
  polyz[3 +5*jj1]=z[(i+1)+(*p)*j];
  if (  finite(z[i+1+(*p)*(j)])==0) return 0;
  polyx[4 +5*jj1]=x[i];
  polyy[4 +5*jj1]=y[j];
  polyz[4 +5*jj1]=z[i+(*p)*j];
  
  /* 
  if (((polyx[1+5*jj1]-polyx[0+5*jj1])*(polyy[2+5*jj1]-polyy[0+5*jj1])-
       (polyy[1+5*jj1]-polyy[0+5*jj1])*(polyx[2+5*jj1]-polyx[0+5*jj1])) <  0)
    fill[jj1]= (dc < 0 ) ? -fg : fg ;
  else
  */
  {
    fill[jj1]=inint((whiteid-1)*((1/4.0*( z[i+(*p)*j]+ z[i+1+(*p)*j]+
					  z[i+(*p)*(j+1)]+ z[i+1+(*p)*(j+1)])-zmin)
				 /(zmax-zmin)))+1;
    if ( dc < 0 ) fill[jj1]= -fill[jj1];
  }
  return(1);
}

/* FIXME orientation is not properly calculated here  */

int DPoints_ogl(BCG *Xgc,double *polyx,double *polyy,double *polyz, int *fill, int whiteid, double zmin, double zmax, 
		double *x, double *y, double *z, int i, int j, int jj1, int *p, int dc, int fg)
{
#ifdef lint
  whiteid,fill[0],zmin,zmax;
#endif
  polyx[  5*jj1] =x[i];
  polyy[  5*jj1] =y[j];
  polyz[  5*jj1] =z[i+(*p)*j];
  if (  finite(z[i+(*p)*j])==0) return 0;
  polyx[1 +5*jj1]=x[i];
  polyy[1 +5*jj1]=y[j+1];
  polyz[1 +5*jj1]=z[i+(*p)*(j+1)];
  if (  finite(z[i+(*p)*(j+1)])==0) return 0;
  polyx[2 +5*jj1]=x[i+1];
  polyy[2 +5*jj1]=y[j+1];
  polyz[2 +5*jj1]=z[(i+1)+(*p)*(j+1)];
  if (  finite(z[i+1+(*p)*(j+1)])==0) return 0;
  polyx[3 +5*jj1]=x[i+1];
  polyy[3 +5*jj1]=y[j];
  polyz[3 +5*jj1]=z[(i+1)+(*p)*j];
  if (  finite(z[i+1+(*p)*(j)])==0) return 0;
  polyx[4 +5*jj1]=x[i];
  polyy[4 +5*jj1]=y[j];
  polyz[4 +5*jj1]=z[i+(*p)*j];
  /* FIXME: 
     if (((polyx[1+5*jj1]-polyx[0+5*jj1])*(polyy[2+5*jj1]-polyy[0+5*jj1])-
     (polyy[1+5*jj1]-polyy[0+5*jj1])*(polyx[2+5*jj1]-polyx[0+5*jj1])) <  0)
     fill[jj1]=  (dc != 0 ) ? fg : dc ;
     else
  */
  fill[jj1]= dc;
  return(1);
}


/*
 * Trace un triedre : Indices[4] donne les indices des points qui 
 * constituent le triedre dans les tableaux xbox et ybox 
 */

static void DrawAxis_ogl(BCG *Xgc, const nsp_box_3d *box, char flag, int style)
{
  const int *Indices = ( flag == 'U' ) ? box->InsideU : box->InsideD;
  double x[6],y[6],z[6],nsegs=6,lstyle;
  int i,iflag=0;
  for ( i = 0 ; i <= 4 ; i=i+2)
    {
      x[i]=box->x_r[Indices[0]];y[i]=box->y_r[Indices[0]];z[i]=box->z_r[Indices[0]];
    }
  x[1]=box->x_r[Indices[1]];y[1]=box->y_r[Indices[1]];z[1]=box->z_r[Indices[1]];
  x[3]=box->x_r[Indices[2]];y[3]=box->y_r[Indices[2]];z[3]=box->z_r[Indices[2]];
  x[5]=box->x_r[Indices[3]];y[5]=box->y_r[Indices[3]];z[5]=box->z_r[Indices[3]];
  lstyle = Xgc->graphic_engine->xset_dash(Xgc,1);
  drawsegments3D(Xgc,x,y,z,nsegs,&style,iflag);
  Xgc->graphic_engine->xset_dash(Xgc,lstyle);
}


/*-------------------------------------------------------------------
 * param3d function 
 *-------------------------------------------------------------------*/

int nsp_param3d_ogl(BCG *Xgc,double *x, double *y, double *z, int *n, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  nsp_box_3d box;
  int style[1],j;
  static int init;
  int fg1;
  double zmin,zmax; /* unused */
  
  nsp_plot3d_update_bounds(Xgc,"param3d",x,y,z,n,NULL, teta, alpha,legend,&flag[0],bbox,&zmin,&zmax,param3d_t);

  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Param3D(Xgc,x,y,z,n,teta,alpha,legend,flag,bbox);
  
  style[0] = Xgc->graphic_engine->xget_dash(Xgc);

  /* take care here flag[0] is used */
  SetEch3d1(Xgc,&box,bbox,*teta,*alpha,(long)(flag[0]+1)/2);
  /* Calcule l' Enveloppe Convexe de la boite **/
  /* ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,&box,legend,flag[1]);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  /* Le triedre cache **/
  if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
    {
      /* cache=box.InsideD[0];*/
      if (flag[2] >=2 ) DrawAxis_ogl(Xgc,&box,'D',fg1);
    }
  else 
    {
      /* cache=box.InsideU[0]-4; */
      if (flag[2] >=2 ) DrawAxis_ogl(Xgc,&box,'U',fg1);
    }
  init = 0 ; 
  while (1) 
    {
      int nel = 0;
      for ( j =init ; j < (*n) ; j++)	 
	{
	  if ( finite(x[j]) ==0 || finite(y[j])==0 || finite(z[j]) == 0) break;
	  nel++;
	}
      if ( nel > 0 ) 
	drawpolylines3D(Xgc,x+init,y+init,z+init,style,1,nel);
      init = j+1;
      if ( init >= (*n)) break;
    }
  if (flag[2] >=3 ) 
    {
      int fg;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      /* Le triedre que l'on doit voir **/
      if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
	DrawAxis_ogl(Xgc,&box,'U',fg);
      else 
	DrawAxis_ogl(Xgc,&box,'D',fg);
    }
  return(0);
}


/*-------------------------------------------------------------------
 * param3d1 function 
 *-------------------------------------------------------------------*/

int nsp_param3d_1_ogl(BCG *Xgc,double *x, double *y, double *z, int *m, int *n, int *iflag, int *colors, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  nsp_box_3d box;
  int style[1],j;
  static int init;
  int fg1,cur;
  double zmin,zmax; /* unused */

  nsp_plot3d_update_bounds(Xgc,"param3d",x,y,z,n,NULL, teta, alpha,legend,&flag[0],bbox,&zmin,&zmax,param3d_t);

  /* If Record is on **/
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Param3D1(Xgc,x,y,z,m,n,iflag,colors,teta,alpha,legend,flag,bbox);
  style[0] = Xgc->graphic_engine->xget_dash(Xgc);

  /* take care here flag[0] is used */
  SetEch3d1(Xgc,&box,bbox,*teta,*alpha,(long)(flag[0]+1)/2);

  /* Calcule l' Enveloppe Convexe de la boite **/
  /* ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,&box,legend,flag[1]);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  /* Le triedre cache **/
  if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
    {
      /* cache=box.InsideD[0];*/
      if (flag[2] >=2 ) DrawAxis_ogl(Xgc,&box,'D',fg1);
    }
  else 
    {
      /* cache=box.InsideU[0]-4; */
      if (flag[2] >=2 ) DrawAxis_ogl(Xgc,&box,'U',fg1);
    }
  for ( cur=0 ; cur < *n ; cur++)
    {
      init = 0 ; 
      if ( *iflag != 0 ) style[0]=  colors[cur];
      while (1) 
	{
	  int nel = 0,j1;
	  j1= (*m)*cur;
	  for ( j =init ; j < (*m) ; j++)	 
	    {
	      if ( finite(x[j]) ==0 || finite(y[j])==0 || finite(z[j]) == 0) break;
	      nel++;
	    }
	  if ( nel > 0 ) drawpolylines3D(Xgc,x+init,y+init,z+init,style,1,nel);
	  init = j+1;
	  if ( init >= (*m)) break;
	}
    }
  if (flag[2] >=3 ) 
    {
      int fg;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      /* Le triedre que l'on doit voir **/
      if ( box.z[box.InsideU[0]] > box.z[box.InsideD[0]])
	DrawAxis_ogl(Xgc,&box,'U',fg);
      else 
	DrawAxis_ogl(Xgc,&box,'D',fg);
    }
  return(0);
}


static int nsp_plot_box3d_ogl(BCG *Xgc,nsp_box_3d *box)
{
  static int flag[]={1,1,3},fg,fg1;
  /* FIXME  Convex_Box(Xgc,xbox,ybox,zbox,box.InsideU,box.InsideD,"X@Y@Z",flag[2],Xgc->scales->bbox1); */
  Convex_Box(Xgc,box,"X@Y@Z",flag[2]);
  fg = Xgc->graphic_engine->xget_foreground(Xgc);
  if ( box->z[box->InsideU[0]] > box->z[box->InsideD[0]])
    DrawAxis_ogl(Xgc,box,'U',fg);
  else
    DrawAxis_ogl(Xgc,box,'D',fg);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;
  /* Le triedre cache **/
  if ( box->z[box->InsideU[0]] > box->z[box->InsideD[0]])
      DrawAxis_ogl(Xgc,box,'D',fg1);
  else
      DrawAxis_ogl(Xgc,box,'U',fg1);
  return(0);

}

