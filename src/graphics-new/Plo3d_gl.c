/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include <string.h> /* in case of dbmalloc use */
#include <math.h>
#include <stdio.h>
#include "nsp/math.h"
/*
** NEW !!
*/
//#include "nsp/graphics/Graphics.h"

/*
** NEW !!
*/
#include "nsp/graphics/periGL.h"

/* #include "nsp/graphics/PloEch.h" */

#ifdef __STDC__
void wininfo(char *format,...);
#else
/*VARARGS0*/
void wininfo();
#endif    

extern double C2F(dsort)();

/** like GEOX or GEOY in PloEch.h but we keep values in xx1 and yy1 for finite check **/ 

static double xx1,yy1;

#define PGEOX(x1,y1,z1) inint(xx1= Xgc->scales->Wscx1*(TRX(x1,y1,z1)-Xgc->scales->frect[0]) +Xgc->scales->Wxofset1);
#define PGEOY(x1,y1,z1) inint(yy1= Xgc->scales->Wscy1*(-TRY(x1,y1,z1)+Xgc->scales->frect[3])+Xgc->scales->Wyofset1);

void fillpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *fillvect,int n, int p);
void drawpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz, int *drawvect,int n, int p);

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

static void dbox (BCG *Xgc);


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
  C2F(plot3dg)(Xgc,"plot3d",DPoints,x,y,z,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}

int nsp_plot3d_1(BCG *Xgc,double *x, double *y, double *z, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  C2F(plot3dg)(Xgc,"plot3d1",DPoints1,x,y,z,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}

int nsp_plot_fac3d(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  C2F(fac3dg)(Xgc,"fac3d",0,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}

int nsp_plot_fac3d_1(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  C2F(fac3dg)(Xgc,"fac3d1",1,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}

int nsp_plot_fac3d_2(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  C2F(fac3dg)(Xgc,"fac3d2",2,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
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
  static int InsideU[4],InsideD[4],fg,fg1,dc;
  /* solid = color of 3D frame */
  int polysize,npoly,whiteid;
  int *polyx,*polyy,*fill;
  double xbox[8],ybox[8],zbox[8];
  static int cache;
  static double zmin,zmax;
  int i,j;
  /** If Record is on **/
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    {
      if (strcmp(name,"plot3d")==0) 
	store_Plot3D(Xgc,x,y,z,p,q,teta,alpha,legend,flag,bbox);
      else 
	store_Plot3D1(Xgc,x,y,z,p,q,teta,alpha,legend,flag,bbox);
    }

  fg = Xgc->graphic_engine->xget_foreground(Xgc);
 
  if (flag[1]!=0 && flag[1]!=1 && flag[1]!=3 && flag[1]!=5)
    {
      bbox[0]=x[0];bbox[1]=x[*p-1];
      bbox[2]=y[0];bbox[3]=y[*q-1];
      zmin=bbox[4]=(double) Mini(z,*p*(*q)); 
      zmax=bbox[5]=(double) Maxi(z,*p*(*q));
    }
  if ( flag[1]==1 || flag[1]==3 || flag[1]==5) 
    {
      zmin=bbox[4];
      zmax=bbox[5];
    }


  if ( flag[1] ==0)
    SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,0L);
  else
    SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,(long)(flag[1]+1)/2);
  /** Calcule l' Enveloppe Convex de la boite **/
  /** ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,xbox,ybox,InsideU,InsideD,legend,flag,bbox);
  /** Le triedre cach\'e **/
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;
  if (zbox[InsideU[0]] > zbox[InsideD[0]])
    {
      cache=InsideD[0];
      if (flag[2] >=2 )DrawAxis(Xgc,xbox,ybox,InsideD,fg1);
    }
  else 
    {
      cache=InsideU[0]-4;
      if (flag[2] >=2 )DrawAxis(Xgc,xbox,ybox,InsideU,fg1);
    }
  polyx = graphic_alloc(0,5*(*q),sizeof(int));
  polyy = graphic_alloc(1,5*(*q),sizeof(int));
  fill  = graphic_alloc(2,(*q),sizeof(int));
  if ( (polyx == NULL) || (polyy == NULL) || (fill  == NULL)) 
    {
      Scistring("plot3dg_ : malloc No more Place\n");
      return;
    }

  /** The 3d plot **/

  whiteid = Xgc->graphic_engine->xget_last(Xgc);
  dc =  flag[0];
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;   
  for ( i =0 ; i < (*q)-1 ; i++)   fill[i]= dc ;
  polysize=5;
  npoly= (*q)-1; 
  /** Choix de l'ordre de parcourt **/
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
      /** Le triedre que l'on doit voir **/
      if (zbox[InsideU[0]] > zbox[InsideD[0]])
	DrawAxis(Xgc,xbox,ybox,InsideU,fg);
      else 
	DrawAxis(Xgc,xbox,ybox,InsideD,fg);
    }
}

static void C2F(fac3dg)(BCG *Xgc,char *name, int iflag, double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  static int InsideU[4],InsideD[4],fg1;
  int polysize,npoly,whiteid;
  int *polyx,*polyy,*locindex,fill[4]; /* Modified by polpoth 4/5/2000 fill[4] instead of fill[1] */
  double xbox[8],ybox[8],zbox[8],*polyz;
  static int cache;
  static double zmin,zmax;
  int i;

  /** If Record is on **/

  printf(">>>>>>>>>>>>>>\n");
  printf("FAC3DG\n");
  printf(">>>>>>>>>>>>>>\n");

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

  if (flag[1]!=1 && flag[1] != 0 && flag[1]!=3 && flag[1]!=5)
    {
      bbox[0]=(double) Mini(x,*p*(*q));
      bbox[1]=(double) Maxi(x,*p*(*q));
      bbox[2]=(double) Mini(y,*p*(*q)); 
      bbox[3]=(double) Maxi(y,*p*(*q));
      zmin=bbox[4]=(double) Mini(z,*p*(*q)); 
      zmax=bbox[5]=(double) Maxi(z,*p*(*q));
    }
  if ( flag[1]==1 || flag[1]==3|| flag[1]==5) 
    {
      zmin=bbox[4];
      zmax=bbox[5];
    }
  if ( flag[1]==0)
    SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,0L);
  else
    SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,(long)(flag[1]+1)/2);
  /** Calcule l' Enveloppe Convex de la boite **/
  /** ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,xbox,ybox,InsideU,InsideD,legend,flag,bbox);
  /** Le triedre cach\'e **/
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;  
  if (zbox[InsideU[0]] > zbox[InsideD[0]])
    {
      cache=InsideD[0];
      if (flag[2] >=2 )DrawAxis(Xgc,xbox,ybox,InsideD,fg1);
    }
  else 
    {
      cache=InsideU[0]-4;
      if (flag[2] >=2 )DrawAxis(Xgc,xbox,ybox,InsideU,fg1);
    }
  polyz = graphic_alloc(5,(*q),sizeof(double));
  if ( (polyz == NULL) && (*q) != 0)
    {
      Scistring("plot3dg_ : malloc No more Place\n");
      return;
    }
  /** Allocation  **/
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
  /** tri **/
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
                shade(Xgc,polyx,polyy,fill,*p,flag[0]);
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
      /** Le triedre que l'on doit voir **/
      if (zbox[InsideU[0]] > zbox[InsideD[0]])
	DrawAxis(Xgc,xbox,ybox,InsideU,fg);
      else 
	DrawAxis(Xgc,xbox,ybox,InsideD,fg);
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
       (polyy[1+5*jj1]-polyy[0+5*jj1])*(polyx[2+5*jj1]-polyx[0+5*jj1])) <  0)
    fill[jj1]= (dc < 0 ) ? -fg : fg ;
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
  static int InsideU[4],InsideD[4];
  static double xbox[8],ybox[8],zbox[8];
  int style[1],j;
  static int init;
  static int *xm,*ym;
  int fg1;
  /** If Record is on **/
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Param3D(Xgc,x,y,z,n,teta,alpha,legend,flag,bbox);
  style[0] = Xgc->graphic_engine->xget_dash(Xgc);
  if (flag[1]!=0 && flag[1]!=1 && flag[1]!=3 && flag[1]!=5)
    {
      bbox[0]=(double) Mini(x,*n);bbox[1]=(double) Maxi(x,*n);
      bbox[2]=(double) Mini(y,*n);bbox[3]=(double) Maxi(y,*n);
      bbox[4]=(double) Mini(z,*n);bbox[5]=(double) Maxi(z,*n);
    }
  if ( flag[1] !=0)
    SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,(long)(flag[1]+1)/2);
  else 
    SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,0L);
  /** Calcule l' Enveloppe Convexe de la boite **/
  /** ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,xbox,ybox,InsideU,InsideD,legend,flag,bbox);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  /** Le triedre cache **/
  if (zbox[InsideU[0]] > zbox[InsideD[0]])
    {
      /* cache=InsideD[0];*/
      if (flag[2] >=2 ) DrawAxis(Xgc,xbox,ybox,InsideD,fg1);
    }
  else 
    {
      /* cache=InsideU[0]-4; */
      if (flag[2] >=2 ) DrawAxis(Xgc,xbox,ybox,InsideU,fg1);
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
	   drawpolylines3D(Xgc,x,y,z,style,1,nel);
//	   Xgc->graphic_engine->drawpolylines3D(Xgc,xm,ym,style,1,nel);
      init = j+1;
      if ( init >= (*n)) break;
    }
  if (flag[2] >=3 ) 
    {
      int fg;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      /** Le triedre que l'on doit voir **/
      if (zbox[InsideU[0]] > zbox[InsideD[0]])
	DrawAxis(Xgc,xbox,ybox,InsideU,fg);
      else 
	DrawAxis(Xgc,xbox,ybox,InsideD,fg);
    }
  return(0);
}


/*-------------------------------------------------------------------
 * param3d1 function 
 *-------------------------------------------------------------------*/

int nsp_param3d_1(BCG *Xgc,double *x, double *y, double *z, int *m, int *n, int *iflag, int *colors, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  static int InsideU[4],InsideD[4];
  static double xbox[8],ybox[8],zbox[8];
  int style[1],j;
  static int init;
  static int *xm,*ym;
  int fg1,cur;
  /** If Record is on **/
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_Param3D1(Xgc,x,y,z,m,n,iflag,colors,teta,alpha,legend,flag,bbox);
  style[0] = Xgc->graphic_engine->xget_dash(Xgc);
  if (flag[1]!=0 && flag[1]!=1 && flag[1]!=3 && flag[1]!=5)
    {
      int mn=*n*(*m);
      bbox[0]=(double) Mini(x,mn);bbox[1]=(double) Maxi(x,mn);
      bbox[2]=(double) Mini(y,mn);bbox[3]=(double) Maxi(y,mn);
      bbox[4]=(double) Mini(z,mn);bbox[5]=(double) Maxi(z,mn);
    }
  if ( flag[1] !=0)
    SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,(long)(flag[1]+1)/2);
  else 
    SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,0L);
  /** Calcule l' Enveloppe Convexe de la boite **/
  /** ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,xbox,ybox,InsideU,InsideD,legend,flag,bbox);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  /** Le triedre cache **/
  if (zbox[InsideU[0]] > zbox[InsideD[0]])
    {
      /* cache=InsideD[0];*/
      if (flag[2] >=2 ) DrawAxis(Xgc,xbox,ybox,InsideD,fg1);
    }
  else 
    {
      /* cache=InsideU[0]-4; */
      if (flag[2] >=2 ) DrawAxis(Xgc,xbox,ybox,InsideU,fg1);
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
  if (flag[2] >=3 ) 
    {
      int fg;
      fg = Xgc->graphic_engine->xget_foreground(Xgc);
      /** Le triedre que l'on doit voir **/
      if (zbox[InsideU[0]] > zbox[InsideD[0]])
	DrawAxis(Xgc,xbox,ybox,InsideU,fg);
      else 
	DrawAxis(Xgc,xbox,ybox,InsideD,fg);
    }
  return(0);
}


/*-------------------------------------------------------------------
 * box3d 
 *-------------------------------------------------------------------*/

int nsp_plot_box3d(BCG *Xgc,double *xbox, double *ybox, double *zbox)
{
  static int InsideU[4],InsideD[4],flag[]={1,1,3},fg,fg1;
  /** Calcule l' Enveloppe Convexe de la boite **/
  /** ainsi que les triedres caches ou non **/
  Convex_Box(Xgc,xbox,ybox,InsideU,InsideD,"X@Y@Z",flag,Xgc->scales->bbox1);
  /** le triedre vu **/
  fg = Xgc->graphic_engine->xget_foreground(Xgc);
  if (zbox[InsideU[0]] > zbox[InsideD[0]])
    DrawAxis(Xgc,xbox,ybox,InsideU,fg);
  else 
    DrawAxis(Xgc,xbox,ybox,InsideD,fg);
  fg1 = Xgc->graphic_engine->xget_hidden3d(Xgc);
  if (fg1==-1) fg1=0;
  /** Le triedre cache **/
  if (zbox[InsideU[0]] > zbox[InsideD[0]])
      DrawAxis(Xgc,xbox,ybox,InsideD,fg1);
  else 
      DrawAxis(Xgc,xbox,ybox,InsideU,fg1);
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

void SetEch3d(BCG *Xgc,double *xbox, double *ybox, double *zbox, double *bbox, double *teta, double *alpha)
{
  SetEch3d1(Xgc,xbox,ybox,zbox,bbox,teta,alpha,1L);
}

/* 
 * if flag==1,2,3  m and bbox and Xgc->scales->are  recomputed  
 * if flag==0      we only change m without changing scales 
 */

void SetEch3d1(BCG *Xgc,double *xbox, double *ybox, double *zbox, double *bbox, double *teta, double *alpha, int flag)
{
  double xmmin,ymmax,xmmax,ymmin,FRect[4],WRect[4],ARect[4];
  int ib;
  static int aaint[]={2,10,2,10};
  int wdim[2];
  char logf[2];
  double R,xo,yo,zo,dx,dy,dz,hx,hy,hx1,hy1,Teta,Alpha;
  int wmax,hmax;
  static double cost=0.5,sint=0.5,cosa=0.5,sina=0.5;
  Teta=*teta;
  Alpha=*alpha;
  /*  if (flag==0) {
    Alpha=Xgc->scales->alpha;
    Teta=Xgc->scales->theta;
      }
  else {
    Xgc->scales->alpha = Alpha;
    Xgc->scales->theta = Teta;
    }*/
  Xgc->scales->alpha = Alpha;
  Xgc->scales->theta = Teta;
  cost=cos((Teta)*M_PI/180.0);
  sint=sin((Teta)*M_PI/180.0);
  cosa=cos((Alpha)*M_PI/180.0);
  sina=sin((Alpha)*M_PI/180.0);
  Xgc->scales->m[0][0]= -sint    ;    Xgc->scales->m[0][1]= cost      ;    Xgc->scales->m[0][2]= 0;
  Xgc->scales->m[1][0]= -cost*cosa;   Xgc->scales->m[1][1]= -sint*cosa;    Xgc->scales->m[1][2]= sina;
  Xgc->scales->m[2][0]=  cost*sina;   Xgc->scales->m[2][1]= sint*sina;     Xgc->scales->m[2][2]= cosa;
  /* Coordonn\'ees apr\`es transformation g\'eometrique de la
   * boite qui entoure le plot3d                            
   * le plan de projection est defini par x et y            
   */
  for (ib=0;ib<6 ;ib++) 
    { 
      if (flag==0) 
	bbox[ib]=Xgc->scales->bbox1[ib];
      else 
	Xgc->scales->bbox1[ib]=bbox[ib];
    }
  xbox[0]=TRX(bbox[0],bbox[2],bbox[4]);
  ybox[0]=TRY(bbox[0],bbox[2],bbox[4]);
  zbox[0]=TRZ(bbox[0],bbox[2],bbox[4]);
  xbox[1]=TRX(bbox[0],bbox[3],bbox[4]);
  ybox[1]=TRY(bbox[0],bbox[3],bbox[4]);
  zbox[1]=TRZ(bbox[0],bbox[3],bbox[4]);
  xbox[2]=TRX(bbox[1],bbox[3],bbox[4]);
  ybox[2]=TRY(bbox[1],bbox[3],bbox[4]);
  zbox[2]=TRZ(bbox[1],bbox[3],bbox[4]);
  xbox[3]=TRX(bbox[1],bbox[2],bbox[4]);
  ybox[3]=TRY(bbox[1],bbox[2],bbox[4]);
  zbox[3]=TRZ(bbox[1],bbox[2],bbox[4]);
  xbox[4]=TRX(bbox[0],bbox[2],bbox[5]);
  ybox[4]=TRY(bbox[0],bbox[2],bbox[5]);
  zbox[4]=TRZ(bbox[0],bbox[2],bbox[5]);
  xbox[5]=TRX(bbox[0],bbox[3],bbox[5]);
  ybox[5]=TRY(bbox[0],bbox[3],bbox[5]);
  zbox[5]=TRZ(bbox[0],bbox[3],bbox[5]);
  xbox[6]=TRX(bbox[1],bbox[3],bbox[5]);
  ybox[6]=TRY(bbox[1],bbox[3],bbox[5]);
  zbox[6]=TRZ(bbox[1],bbox[3],bbox[5]);
  xbox[7]=TRX(bbox[1],bbox[2],bbox[5]);
  ybox[7]=TRY(bbox[1],bbox[2],bbox[5]);
  zbox[7]=TRZ(bbox[1],bbox[2],bbox[5]);
  /** Calcul des echelles en fonction de la taille du dessin **/
  if ( flag == 1 || flag == 3 )
    {
      xmmin=  (double) Mini(xbox,8L);xmmax= (double) Maxi(xbox,8L);
      ymmax=  (double) - Mini(ybox,8L);
      ymmin=  (double) - Maxi(ybox,8L);
    }
  /* code added by es: isoview scaling */
  if ( flag == 2 || flag == 3 )
    {
      /* get current window size */
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      getscale2d(Xgc,WRect,FRect,logf,ARect);
      wmax=linint((double)wdim[0] * WRect[2]);
      hmax=linint((double)wdim[1] * WRect[3]); 
    }
  if ( flag == 2 )
    {
/* radius and center of the sphere circumscribing the box */
      dx=bbox[1]-bbox[0]; dy=bbox[3]-bbox[2]; dz=bbox[5]-bbox[4];
      R= (double) sqrt(dx*dx + dy*dy + dz*dz)/2;
      xo= (double) (xbox[0]+xbox[6])/2 ;
      yo= (double) (ybox[0]+ybox[6])/2 ;
      zo= (double) (zbox[0]+zbox[6])/2 ;
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
      Xgc->scales->metric3d=flag; /* the metric mode is stored into the
                             * List of Scales */
     }
/* end of code added by es */
}

/*----------------------------------------------------------------
 *Trace un triedre : Indices[4] donne les indices des points qui 
 * constituent le triedre dans les tableaux xbox et ybox 
 *-----------------------------------------------------------------*/ 

void DrawAxis(BCG *Xgc,double *xbox, double *ybox, int *Indices, int style)
{
  int ixbox[6],iybox[6],npoly=6,lstyle;
  int i,iflag=0;
  for ( i = 0 ; i <= 4 ; i=i+2)
    {
      ixbox[i]=XScale(xbox[Indices[0]]);iybox[i]=YScale(ybox[Indices[0]]);
    }
  ixbox[1]=XScale(xbox[Indices[1]]);iybox[1]=YScale(ybox[Indices[1]]);
  ixbox[3]=XScale(xbox[Indices[2]]);iybox[3]=YScale(ybox[Indices[2]]);
  ixbox[5]=XScale(xbox[Indices[3]]);iybox[5]=YScale(ybox[Indices[3]]);
  lstyle = Xgc->graphic_engine->xset_dash(Xgc,1);
  Xgc->graphic_engine->drawsegments(Xgc,ixbox,iybox,npoly,&style,iflag);
  Xgc->graphic_engine->xset_dash(Xgc,lstyle);
}

/*---------------------------------------------------------------------
 *Trace l'enveloppe convexe de la boite contenant le dessin 
 * et renvoit dans InsideU et InsideD les indices des points dans xbox et ybox
 * qui sont sur les 2 tri\`edres a l'interieur de l'enveloppe convexe
 *---------------------------------------------------------------------*/

void Convex_Box(BCG *Xgc,double *xbox, double *ybox, int *InsideU, int *InsideD, char *legend, int *flag, double *bbox)
{
  double xmaxi;
  int ixbox[8],iybox[8];
  int xind[8];
  int ind2,ind3,ind;
  int p,n,dvect[1],dash;
  int pat;
  int i;
  /** dans xbox[8] se trouve l'abscisse des points successifs   **/
  /** de la boite qui continent la surface                      **/
  /** on stocke dans xind[8] les indices des points de la boite **/
  /** qui sont sur l'enveloppe convexe en partant du point en haut **/
  /** a droite et en tournant ds le sens trigonometrique           **/
  /** par exemple avec : **/
  /*      4 ----- 5        */
  /*       /    /|         */
  /*     7----6  |         */
  /*      | 0 | / 1        */
  /*     3----- 2          */
  /** on doit trouver xind={5,4,7,3,2,1}; **/
  /** on en profite pour stocker aussi les points des triedres **/

  xmaxi=((double) Maxi(xbox,8L));
  ind= -1;
  for (i =0 ; i < 8 ; i++)
    {
      MaxiInd(xbox,8L,&ind,xmaxi);
      if ( ind > 3)
	  {
	    xind[0]=ind;
	    break;
	  }
    }
  if (ind < 0 || ind > 8) 
    {
      Scistring("xind out of bounds");
      xind[0]=0;
    }
  UpNext(xind[0],&ind2,&ind3);
  if (ybox[ind2] > ybox[ind3]) 
    {
      xind[1]=ind2;InsideU[0]=ind3;
    }
  else 
    {
      xind[1]=ind3;InsideU[0]=ind2;
    }
  UpNext(ind2,&ind2,&ind3); InsideU[1]=xind[0];
  InsideU[2]=ind2; InsideU[3]=InsideU[0]-4;
  xind[2]=ind2;
  /* le point en bas qui correspond */
  xind[3]=ind2-4;
  DownNext(xind[3],&ind2,&ind3);
  if (ybox[ind2] < ybox[ind3]) 
   {
     xind[4]=ind2;InsideD[0]=ind3;
   }
 else  
   {
     xind[4]=ind3;InsideD[0]=ind2;
   }
  DownNext(ind2,&ind2,&ind3);
  InsideD[1]=xind[3];
  InsideD[2]=ind2;
  InsideD[3]=InsideD[0]+4;
  xind[5]=ind2;
  for (i=0; i < 6 ; i++)
   {
     ixbox[i]=XScale(xbox[xind[i]]);
     iybox[i]=YScale(ybox[xind[i]]);
   }
  ixbox[6]=ixbox[0];iybox[6]=iybox[0];
  p=7,n=1;
  dvect[0]= Xgc->graphic_engine->xget_foreground(Xgc);
  /** On trace l'enveloppe cvxe **/
  dash = Xgc->graphic_engine->xset_dash(Xgc,1);
    
  if (flag[2]>=3){
    Xgc->graphic_engine->drawpolylines(Xgc,ixbox,iybox,dvect,n,p);
  }
  pat = Xgc->graphic_engine->xset_pattern(Xgc,dvect[0]);

  if (flag[2]>=3)AxesStrings(Xgc,flag[2],ixbox,iybox,xind,legend,bbox);
  Xgc->graphic_engine->xset_pattern(Xgc,pat);
  Xgc->graphic_engine->xset_dash(Xgc,dash);

}

/** rajoute des symboles x,y,z : sur les axes     **/
/** et une graduation sur les axes **/
/** (ixbox,iybox) : Coordonnees des points de l'envelloppe cvxe en pixel **/
/** xind : indices des points de l'enveloppe cvxe ds xbox et ybox **/

void AxesStrings(BCG *Xgc,int axflag, int *ixbox, int *iybox, int *xind, char *legend, double *bbox)
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
  /** le cot\'e gauche ( c'est tjrs un axe des Z **/
  Xgc->graphic_engine->xget_windowdim(Xgc,xz,xz+1);
  iof = (xz[0]+xz[1])/50;
  x=ixbox[2]-iof ;y=iybox[2]-iof;
  if ( axflag>=4)
    {
      double fx,fy,fz,lx,ly,lz;
      int LPoint[2],FPoint[2],Ticsdir[2],xnax[2];
      xnax[0]=5;xnax[1]=2;
      FPoint[0]=ixbox[2];FPoint[1]=iybox[2];
      LPoint[0]=ixbox[3];LPoint[1]=iybox[3];
      Ticsdir[0]= -1;
      Ticsdir[1]=0;
      BBoxToval(&fx,&fy,&fz,xind[2],bbox);
      BBoxToval(&lx,&ly,&lz,xind[3],bbox);
      TDAxis(Xgc,1L,fz,lz,xnax,FPoint,LPoint,Ticsdir);
    }
  if (legz != 0)
    {
      Xgc->graphic_engine->boundingbox(Xgc,legz,x,y,rect);
      Xgc->graphic_engine->displaystring(Xgc,legz,(x=x - rect[2],x),y,flag ,ang);
    }
  /** le cot\^e en bas \`a gauche **/
  x=inint((ixbox[3]+ixbox[4])/2.0 -iof);
  y=inint((1/3.0)*iybox[3]+(2/3.0)*iybox[4]+iof);
  if ( xind[3]+xind[4] == 3)
    {
      if ( axflag>=4)
	{
	  double fx,fy,fz,lx,ly,lz;
	  int LPoint[2],FPoint[2],Ticsdir[2],xnax[2];
	  xnax[0]=5;xnax[1]=2;
	  FPoint[0]=ixbox[3];FPoint[1]=iybox[3];
	  LPoint[0]=ixbox[4];LPoint[1]=iybox[4];
	  Ticsdir[0]=ixbox[4]-ixbox[5];
	  Ticsdir[1]=iybox[4]-iybox[5];
	  BBoxToval(&fx,&fy,&fz,xind[3],bbox);
	  BBoxToval(&lx,&ly,&lz,xind[4],bbox);
	  TDAxis(Xgc,2L,fx,lx,xnax,FPoint,LPoint,Ticsdir);
	}
      if (legx != 0)
	{

	  Xgc->graphic_engine->boundingbox(Xgc,legx,x,y,rect);
	  Xgc->graphic_engine->displaystring(Xgc,legx,(x=x-rect[2],x),y,flag,ang);
	}
    }
  else 
    {
      if ( axflag>=4)
	{
	  double fx,fy,fz,lx,ly,lz;
	  int LPoint[2],FPoint[2],Ticsdir[2],xnax[2];
	  xnax[0]=5;xnax[1]=2;
	  FPoint[0]=ixbox[3];FPoint[1]=iybox[3];
	  LPoint[0]=ixbox[4];LPoint[1]=iybox[4];
	  Ticsdir[0]=ixbox[4]-ixbox[5];
	  Ticsdir[1]=iybox[4]-iybox[5];
	  BBoxToval(&fx,&fy,&fz,xind[3],bbox);
	  BBoxToval(&lx,&ly,&lz,xind[4],bbox);
	  TDAxis(Xgc,2L,fy,ly,xnax,FPoint,LPoint,Ticsdir);
	}
      if (legy != 0)
	{

	  Xgc->graphic_engine->boundingbox(Xgc,legy,x,y,rect);
	  Xgc->graphic_engine->displaystring(Xgc,legy,(x=x-rect[2],x),y,flag,ang);
	}
    }
  /** le cot\'e en bas a droite **/
  x=inint((ixbox[4]+ixbox[5])/2+iof);
  y=inint(((2/3.0)*iybox[4]+(1/3.0)*iybox[5])+iof);
  if ( xind[4]+xind[5] == 3)
    {
      if ( axflag>=4)
	{
	  double fx,fy,fz,lx,ly,lz;
	  int LPoint[2],FPoint[2],Ticsdir[2],xnax[2];
	  xnax[0]=5;xnax[1]=2;
	  FPoint[0]=ixbox[4];FPoint[1]=iybox[4];
	  LPoint[0]=ixbox[5];LPoint[1]=iybox[5];
	  Ticsdir[0]=ixbox[4]-ixbox[3];
	  Ticsdir[1]=iybox[4]-iybox[3];
	  BBoxToval(&fx,&fy,&fz,xind[4],bbox);
	  BBoxToval(&lx,&ly,&lz,xind[5],bbox);
	  TDAxis(Xgc,3L,fx,lx,xnax,FPoint,LPoint,Ticsdir); 
	}
      if (legx != 0) 
	{
	  Xgc->graphic_engine->displaystring(Xgc,legx,x,y,flag,ang);
	}
    }
  else 
    {
      if ( axflag>=4)
	{
	  double fx,fy,fz,lx,ly,lz;
	  int LPoint[2],FPoint[2],Ticsdir[2],xnax[2];
	  xnax[0]=5;xnax[1]=2;
	  FPoint[0]=ixbox[4];FPoint[1]=iybox[4];
	  LPoint[0]=ixbox[5];LPoint[1]=iybox[5];
	  Ticsdir[0]=ixbox[4]-ixbox[3];
	  Ticsdir[1]=iybox[4]-iybox[3];
	  BBoxToval(&fx,&fy,&fz,xind[4],bbox);
	  BBoxToval(&lx,&ly,&lz,xind[5],bbox);
	  TDAxis(Xgc,3L,fy,ly,xnax,FPoint,LPoint,Ticsdir); 
	}
      if (legy != 0) 
	{
	  Xgc->graphic_engine->displaystring(Xgc,legy,x,y,flag,ang);
	}
    }
  FREE(loc);
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
	  /** pour separer ;e 1er arg de l'axe des z de l'axe voisin **/
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


/** Returns the [x,y,z] values of a point given its xbox or ybox indices **/

void BBoxToval(double *x, double *y, double *z, int ind, double *bbox)
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

/** Changement interactif de 3d **/
static double theta,alpha;

void I3dRotation(BCG *Xgc)
{
  int flag[3],pixmode,alumode;
  static int iflag[]={0,0,0,0};
  double xx,yy;
  double theta0,alpha0;
  if ( tape_check_recorded_3D(Xgc,Xgc->CurWindow) == FAIL) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"No 3d recorded plots in your graphic window");
      return;
    }
  xx=1.0/Abs(Xgc->scales->frect[0]-Xgc->scales->frect[2]);
  yy=1.0/Abs(Xgc->scales->frect[1]-Xgc->scales->frect[3]);
  pixmode = Xgc->graphic_engine->xget_pixmapOn(Xgc);
  alumode = Xgc->graphic_engine->xget_alufunction(Xgc);
  if ( Xgc->graphic_engine->xget_recording(Xgc) == FALSE ) 
    {
      Xgc->graphic_engine->xinfo(Xgc,"3d rotation is not possible when recording is not on" );
      return;
    }
  else 
    {
      int ibutton,iwait=FALSE,istr=0;
      double x0,yy0,x,y,xl,yl,bbox[4];
#ifdef WIN32
      SetWinhdc();
      SciMouseCapture();
      Xgc->graphic_engine->xset_recording(Xgc,FALSE);
#else
      Xgc->graphic_engine->xset_recording(Xgc,FALSE);
#endif
      if ( pixmode == 0 ) Xgc->graphic_engine->scale->xset_alufunction1(Xgc,6);
      Xgc->graphic_engine->scale->xclick(Xgc,"one",&ibutton,&x0,&yy0,iwait,FALSE,FALSE,FALSE,istr);
      Xgc->graphic_engine->clearwindow(Xgc);
      theta=Xgc->scales->theta ;
      alpha=Xgc->scales->alpha ;
      
      x0=(x0-Xgc->scales->frect[0])*xx;
      yy0=(yy0-Xgc->scales->frect[1])*yy;
      x=x0;y=yy0;
      theta0=theta;
      alpha0=alpha;
      ibutton=-1;
      while ( ibutton == -1 ) 
	{
	  /* dessin d'un rectangle */
	  theta= theta0 - 180.0*(x-x0);alpha=alpha0 + 180.0*(y-yy0);
	  Xgc->graphic_engine->xinfo(Xgc,"alpha=%.1f,theta=%.1f",alpha,theta); 
	  if ( pixmode == 1) Xgc->graphic_engine->scale->xset_pixmapclear(Xgc);
	  dbox(Xgc);
	  if ( pixmode == 1) Xgc->graphic_engine->scale->xset_show(Xgc);
	  Xgc->graphic_engine->scale->xgetmouse(Xgc,"one",&ibutton,&xl, &yl,FALSE,TRUE,FALSE,FALSE);
	  /* effacement du rectangle */
	  dbox(Xgc);
	  xx=1.0/Abs(Xgc->scales->frect[0]-Xgc->scales->frect[2]);
	  yy=1.0/Abs(Xgc->scales->frect[1]-Xgc->scales->frect[3]);
	  x=(xl-Xgc->scales->frect[0])*xx;
	  y=(yl-Xgc->scales->frect[1])*yy;
	}
      if ( pixmode == 0) Xgc->graphic_engine->scale->xset_alufunction1(Xgc,3);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,alumode);
#ifdef WIN32
      ReleaseWinHdc();
      SciMouseRelease();
#endif
      tape_replay_new_angles(Xgc,Xgc->CurWindow,iflag,flag,&theta,&alpha,bbox);
    }
}

/*
 * Win32, warning when using xor mode
 * colors are changed and black is turned to white
 * so we must use an other pattern than the black one
 * inside dbox
 */

static void dbox(BCG *Xgc)
{
  double xbox[8],ybox[8],zbox[8];
#ifdef WIN32
  int verbose=0,pat,pat1=3,narg;
  pat = Xgc->graphic_engine->xset_pattern(pat1);
#endif
  SetEch3d1(Xgc,xbox,ybox,zbox,Xgc->scales->bbox1,&theta,&alpha,Xgc->scales->metric3d);
  nsp_plot_box3d(Xgc,xbox,ybox,zbox);
#ifdef WIN32
   Xgc->graphic_engine->xset_pattern(pat);
#endif
}

/**************** New functions for interpolated shading **********************
 *
 *  Added by polpoth 4/5/2000
 *
 *******************************************************************************/

int nsp_plot_fac3d_3(BCG *Xgc,double *x, double *y, double *z, int *cvect, int *p, int *q, double *teta, double *alpha, char *legend, int *flag, double *bbox)
{
  C2F(fac3dg)(Xgc,"fac3d3",3,x,y,z,cvect,p,q,teta,alpha,legend,flag,bbox);
  return(0);
}

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












