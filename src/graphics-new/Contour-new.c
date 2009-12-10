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
#include "nsp/math.h"
#include "nsp/graphics-new/Graphics.h"

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#define PERIGL
#include "nsp/graphics-new/periGtk.h"
#endif 

extern void drawpolylines3D(BCG *Xgc,double *vectsx, double *vectsy, double *vectsz,
			    int *drawvect,int n, int p);

typedef void (level_f) (BCG *Xgc,int ival, double Cont, double xncont,
			double yncont);
typedef void (*ptr_level_f) (BCG *Xgc,int ival, double Cont, double xncont,
			     double yncont);


static void 
contourI (BCG *Xgc,ptr_level_f,double *, double *, double *,
	  double *, int *, int *, int *);

static void
look (BCG *Xgc,ptr_level_f, int i, int j, int ib,
      int jb, int qq,double Cont, int style);

static int ffnd (BCG *Xgc,ptr_level_f,int,int,int,int,int,
		 int,int,int,int,int,
		 double,int *);

static int Gcont_size = 0;

static void ContourTrace (BCG *Xgc,double Cont, int style);
static level_f Contstore_, Contstore_1, Contstore_2, GContstore_2;

#ifdef WITH_GTKGLEXT 
static void ContourTrace_ogl(BCG *Xgc,double Cont, int style);
static level_f Contstore_ogl,Contstore_1_ogl ;
#endif 
static void GContstore_2Last (void);
static double x_cont (int i);
static double y_cont (int i);
static double phi_cont (int, int); 
static double f_intercept  (double, double, double, double, double );
static int not_same_sign  (double val1, double val2); 
static int get_itg_cont  (int i, int j); 
static void inc_itg_cont  (int i, int j, int val); 
static int oddp  (int i);

/*-----------------------------------------------------------------------
 *  Level curves 
 *  The computer journal vol 15 nul 4 p 382 (1972)
 *  from the Lisp Macsyma source (M.I.T)
 * -------------------------------------------------------------------------*/

#define HIDDENFRAMECOLOR 2L /* default color for hidden frame */

/*---------------------------------------------------------------------------
 * General functions (could be changed in #define or
 *   inline functions to increase speed)
 *---------------------------------------------------------------------------*/

static double *GX,*GY,*GZ;
static int Gn1,Gn2;

static void InitValues(double *x, double *y, double *z, int n1, int n2)
{
  Gn1=n1;  Gn2=n2;  GX = x;  GY = y;  GZ = z;
}

/*--------return the  value of f for a point on the grid-----*/

static double phi_cont(int i, int j)
{
  return(GZ[i+Gn1*j]);
}

/*---------return the coordinates between  [xi,xj] along one axis 
 *  for which the value of f is zCont */ 

static double f_intercept(double zCont, double fi, double xi, double fj, double xj)
{
  return( xi+ (zCont-fi)*(xj-xi)/ (fj-fi));
}

/* check for boundary points */

static int  bdyp(int i, int j)
{
  return (  j == 0 || i == 0 || j == Gn2-1 || i == Gn1-1 );
}

/* store or get flag values */

static  int *itg_cont, *xbd_cont,*ybd_cont;

static int get_itg_cont(int i, int j)
{
  return( itg_cont[i+Gn1*j]);
}

static void inc_itg_cont(int i, int j, int val)
{
  itg_cont[i+Gn1*j] += val;
}

static int not_same_sign(double val1, double val2)
{
  if ( ISNAN(val1) ==1 || ISNAN(val2) == 1) return(0);
  /** 0.0 est consid\'er\'e comme positif **/
  if ( val1 >= 0.0) 
    {
      if (val2 < 0.0) return(1) ; else return(0);}
  else 
    {
      if ( val2 >= 0.0) return(1) ; else return(0);}
}

static int oddp(int i) { return( i == 1 || i ==3 );}

/*---------return the x-value of a grid point--------*/

static double x_cont(int i) {  return GX[i] ;}

/*---------return the y-value of a grid point --------*/

static double y_cont(int i) {  return GY[i] ;}

static double ZC=0.0;
static char   ContNumFormat[100];


int nsp_contour3d_draw_new(BCG *Xgc,double *x, double *y, double *z, int n1, int n2, int nz, double *zz, 
		       int flag, double zlev)
{
  double *zconst;
  int err=0, N[3],i;
  void (*func) (BCG *Xgc,int, double,double,double);
  ZC=zlev;

  switch ( flag)
    {
    default:
    case 0: 
      /* 3D geometry with projection on the surface */
#ifdef WITH_GTKGLEXT 
      func =  ( Xgc->graphic_engine == &GL_gengine ) ? Contstore_ogl : Contstore_; 
      break;
#else 
      func=Contstore_; break;  
#endif 
    case 1: 
      /* 3D geometry with projection on a fixed z level  */
#ifdef WITH_GTKGLEXT 
      func =  ( Xgc->graphic_engine == &GL_gengine ) ? Contstore_1_ogl : Contstore_1; 
      break;
#else 
      func=Contstore_1; break;  
#endif 
    }


  if ( zz == NULL )
    {
      /* nz levels */
      nz = Max(nz,1);
      double zmin,zmax;
      zmin=(double) Mini(z,n1*(n2)); 
      zmax=(double) Maxi(z,n1*(n2));
      if ((zconst = graphic_alloc(6,nz,sizeof(double)))== 0) 
	{
	  sciprint("Running out of memory\r\n");
	  return 0;
	}
      for ( i =0 ; i < nz ; i++) 
	zconst[i]=zmin + (i+1)*(zmax-zmin)/(nz+1);
      N[0]= n1;N[1]= n2;N[2]= nz;
      contourI(Xgc,func,x,y,z,zconst,N,(int *) 0,&err);
    }
  else
    {
      /* levels are given by zz of size nz */
      N[0]= n1;N[1]= n2;N[2]= nz;
      contourI(Xgc,func,x,y,z,zz,N,(int *) 0,&err);
    }
  return(0);
}

/* NEW: function used by the contour object 
 *      for drawing itself.
 */

int nsp_contour2d_draw(BCG *Xgc,double *x, double *y, double *z, int n1, int n2, int nz,  double *zz, int *style)
{
  int err=0,i;
  int N[3]= {n1, n2, nz}; 
  
  if ( zz == NULL )
    {
      double *zconst;
      double zmin,zmax;
      zmin=(double) Mini(z,n1*(n2)); 
      zmax=(double) Maxi(z,n1*(n2));
      /* we draw nz level curves */
      if ((zconst = graphic_alloc(6,nz,sizeof(double)))== 0) 
	{
	  sciprint("Running out of memory\r\n");
	  return 0;
	}
      for ( i =0 ; i < nz ; i++) 
	zconst[i]=zmin + (i+1)*(zmax-zmin)/(nz+1);
      contourI(Xgc,Contstore_2,x,y,z,zconst,N,style,&err);
    }
  else
    {
      N[0]= n1;N[1]= n2;N[2]= nz;
      contourI(Xgc,Contstore_2,x,y,z,zz,N,style,&err);
    }
  return(0);
}

/**
 * nsp_contour_if:
 * @Xgc: 
 * @x: 
 * @y: 
 * @z: 
 * @n1: 
 * @n2: 
 * @flagnz: 
 * @nz: 
 * @zz: 
 * @style: 
 * 
 * Used to compute level curves and store them in global variables
 * instead of drawing them.
 * 
 * Return value: 
 **/

int nsp_contour_if_new(BCG *Xgc,double *x, double *y, double *z, int *n1, int *n2,
		       int *flagnz, int *nz, double *zz, int *style)
{
  int err=0, N[3],i;
  double *zconst, zmin,zmax;

  zmin=(double) Mini(z,*n1*(*n2)); 
  zmax=(double) Maxi(z,*n1*(*n2));

  if (*flagnz==0)
    {
      if ((zconst = graphic_alloc(6,(*nz),sizeof(double)))== 0) 
	{
	  sciprint("Running out of memory\r\n");
	  return 0;
	}
      for ( i =0 ; i < *nz ; i++) 
	zconst[i]=zmin + (i+1)*(zmax-zmin)/(*nz+1);
      N[0]= *n1;N[1]= *n2;N[2]= *nz;
      contourI(Xgc,GContstore_2,x,y,z,zconst,N,style,&err);
    }
  else
    {
      N[0]= *n1;N[1]= *n2;N[2]= *nz;
      contourI(Xgc,GContstore_2,x,y,z,zz,N,style,&err);
    }
  return(0);
}



/*-------------------------------------------------------
 *  The function f is given on a grid and we want the level curves 
 *  for the zCont[N[2]] values 
 *  x : of size N[0] gives the x-values of the grid 
 *  y : of size N[1] gives the y-values of the grid 
 *  z : of size N[0]*N[1]  gives the f-values on the grid 
 *  style: size ncont (=N[2]) or empty int pointer 
 *  gives the dash style for contour i
 *-------------------------------------------------------*/

static void contourI(BCG *Xgc,ptr_level_f func, double *x, double *y, double *z, 
		     double *zCont, int *N, int *style, int *err)
{
  int check = 1;
  char *F;
  int n1,n2,ncont,i,c,j,k,n5;
  int stylec;
  n1=N[0];n2=N[1];ncont=N[2];
  F= Xgc->graphic_engine->xget_fpf(Xgc);
  if ( F[0] == '\0') 
    nsp_grformat_e1(ContNumFormat,zCont,N[2]);
  InitValues(x,y,z,n1,n2);
  n5 =  2*(n1)+2*(n2)-3;
  /* Allocation */
  Gcont_size = 0; /* initialize the array indices for storing contours */
  xbd_cont = graphic_alloc(0,n5,sizeof(int));
  ybd_cont = graphic_alloc(1,n5,sizeof(int));
  itg_cont = graphic_alloc(2,n1*n2,sizeof(int));
  if ( (xbd_cont == NULL) && n5 != 0) check= 0;
  if ( (ybd_cont == NULL) && n5 != 0) check= 0;
  if ( (itg_cont == NULL) && n1*n2 != 0) check= 0;
  if ( check == 0) 
    {
      Scistring("contourI_: Running out of memory\n");
      return;
    }
  /* just a parametrization of the boundary points */
  for ( i = 0 ; i <  n2 ; i++)
    {
      ybd_cont[i] = i ;
      xbd_cont[i] = 0 ;
    }
  for ( i = 1 ; i <  n1 ; i++)
    {
      ybd_cont[n2+i-1] = n2-1 ;
      xbd_cont[n2+i-1] = i  ;
    }
  for ( i = n2-2;  i >= 0  ; i--)
    {
      ybd_cont[2*n2 +n1-3-i] = i ;
      xbd_cont[2*n2 +n1-3-i] = n1-1  ;
    }
  for ( i = n1-2 ; i >= 0 ; i--)
    {
      ybd_cont[2*n2 +2*n1-4-i] = 0 ;
      xbd_cont[2*n2 +2*n1-4-i] = i  ;
    }
  for ( c= 0 ; c < ncont ; c++)
    {
      stylec = ( style != NULL ) ? Max(style[c],1) : c+1;
      /* itg-cont is a flag array to memorize checked parts of the grid */
      for ( i = 0 ; i < n1; i++)
	for ( j =0 ; j < n2 ; j++)
	  itg_cont[i+n1*j]=0 ;
      /* all the boundary segments */
      for ( k = 1 ; k < n5 ; k++)
	{ int ib,jb;
	i = xbd_cont[k] ; j = ybd_cont[k];
	ib = xbd_cont[k-1] ; jb= ybd_cont[k-1];
	if  (not_same_sign (phi_cont(i,j)-zCont[c] , 
			    phi_cont(ib,jb)-zCont[c]))
	  look(Xgc,func,i,j,ib,jb,1L,zCont[c],stylec);
	}
      /* inside segments */
      for ( i = 1 ; i < n1-1; i++)
	for ( j = 1 ; j < n2-1 ; j++)
	  if  (not_same_sign ( phi_cont(i,j)-zCont[c] , 
			       phi_cont(i, j-1)-zCont[c]))
	    look(Xgc,func,i,j,i,j-1,2L,zCont[c],stylec);
    }
}

/*--------------------------------------------------------------------
 *  the level curve is crossing the segment (i,j) (ib,jb)
 *  look store the level curve point and try to find the next segment to look at
 *  Cont: value of f along the contour 
 *  ncont: number of contour 
 *  c: indice of the contour Cont 
 *---------------------------------------------------------------------*/

static void look(BCG *Xgc,ptr_level_f func, int i, int j, int ib, int jb, int qq, double Cont, int style)
{
  int ip,jp,im,jm,zds,ent=0,flag=0,wflag;
  jp= j+1; ip= i+1; jm=j-1;im=i-1;
  /*  on regarde comment est le segment de depart */
  if  ( jb == jm)  flag = 1; 
  else  { 
    if ( ib == im ) flag = 2 ;
    else  {
      if ( jb == jp ) flag = 3 ;
      else  if ( ib == ip ) flag = 4;}}
  switch  (  flag)
    {
    case  1 :
      if  (get_itg_cont(i,jm) > 1) return;
      ent=1 ; /* le segment est vertical vers le bas */
      /* Storing intersection point */
      (*func)(Xgc,0,Cont, x_cont(i), 
	      f_intercept(Cont,phi_cont(i,jm),
			  y_cont(jm),phi_cont(i,j),y_cont(j)));
      break;
    case 2 : 
      if  (get_itg_cont(im,j) == 1 || get_itg_cont(im,j)==3 ) return;
      ent=2 ; /* le segment est horizontal gauche */
      /* Storing intersection point */
      (*func)(Xgc,0,Cont,
	      f_intercept(Cont,phi_cont(im,j),
			  x_cont(im),phi_cont(i,j),x_cont(i)), y_cont(j));
      break ; 
    case 3 :
      if  (get_itg_cont(i,j) > 1 ) return;
      ent=3 ; /* le segment est vertical haut */
      /* Storing intersection point */
      (*func)(Xgc,0,Cont,x_cont(i), f_intercept(Cont,phi_cont(i,j),
						y_cont(j),phi_cont(i,jp),y_cont(jp)));
      break ;
    case 4 :
      if  (get_itg_cont(i,j) == 1 || get_itg_cont(i,j)==3 ) return;
      ent=4 ; /* le segment est horizontal droit */
      /* Storing intersection point */
      (*func)(Xgc,0,Cont,f_intercept(Cont,phi_cont(i,j),
				     x_cont(i),phi_cont(ip,j),x_cont(ip)),
	      y_cont(j));
      break;
    default :
      Scistring(" Error in case wrong value ");
      break;
    }
  wflag=1;
  while ( wflag) 
    { 
      jp= j+1; ip= i+1; jm=j-1;im=i-1;
      switch  ( ent) 
	{
	case 1 :
	  inc_itg_cont(i,jm,2L);
	  ent = ffnd(Xgc,func,i,ip,ip,i,j,j,jm,jm,ent,qq,Cont,&zds);
	  /* on calcule le nouveau point, ent donne la 
	     direction du segment a explorer */
	  switch ( ent)
	    {
	    case -1: wflag=0; break;
	    case 1 : i=ip ; break ;
	    case 2 : i=ip;j=jm; break ;
	    }
	  break ;
	case 2  :
	  inc_itg_cont(im,j,1L);
	  ent = ffnd(Xgc,func,i,i,im,im,j,jm,jm,j,ent,qq,Cont,&zds);
	  switch ( ent)
	    { 
	    case -1: wflag=0; break;
	    case 2 : j = jm ;break ;
	    case  3  : i=im;j=jm; break ;
	    }
	  break ;
	case 3 :
	  inc_itg_cont(i,j,2L);
	  ent = ffnd(Xgc,func,i,im,im,i,j,j,jp,jp,ent,qq,Cont,&zds);
	  switch ( ent)
	    { 
	    case -1: wflag=0; break;
	    case 3 : i=im; break ;
	    case 4 : i=im;j=jp; break ;
	    }
	  break ;
	case 4 :
	  inc_itg_cont(i,j,1L);
	  ent = ffnd(Xgc,func,i,i,ip,ip,j,jp,jp,j,ent,qq,Cont,&zds);
	  switch ( ent)
	    {
	    case -1: wflag=0; break;
	    case 4 :j=jp;break ;
	    case 1 :i=ip;j=jp;break ;
	    }
	  break ;
	}
     
      /* new segment is on the boundary */
      if ( zds == 1) 
	{
	  switch ( ent) 
	    {
	    case 1 : inc_itg_cont(i,(j-1),2L); break ; 
	    case 2 : inc_itg_cont(i-1,j,1L);  break ; 
	    case 3 : inc_itg_cont(i,j,2L); break ; 
	    case 4 : inc_itg_cont(i,j,1L); break ; 
	    }
	  /* we must quit the while loop */
	  wflag = 0 ;
	}
      /*  init point was inside the domain */
      if ( qq == 2) 
	{
	  switch ( ent) 
	    {
	    case 1 : if  ( get_itg_cont (i,j-1)  > 1) wflag = 0 ; break ; 
	    case 2 : if  ( oddp(get_itg_cont(i-1,j))) wflag = 0 ; break ; 
	    case 3 : if  ( get_itg_cont(i,j) > 1)     wflag = 0 ; break ; 
	    case 4 : if  ( oddp(get_itg_cont(i,j)))   wflag = 0 ; break ; 
	    }
	}
    }
  if ( func == GContstore_2 ) 
    {
      GContstore_2Last();
    }
  else 
    {
#ifdef WITH_GTKGLEXT 
      if ( Xgc->graphic_engine == &GL_gengine 
	   && ( func== Contstore_ogl || func==Contstore_1_ogl ))
	ContourTrace_ogl(Xgc,Cont,style);
      else
	ContourTrace(Xgc,Cont,style);
#else 
      ContourTrace(Xgc,Cont,style);
#endif 
    }
}


/*-----------------------------------------------------------------------
 *   ffnd : cette fonction  recoit en entree quatre points 
 *       on sait que la courbe de niveau passe entre le point 1 et le quatre 
 *       on cherche a savoir ou elle resort, 
 *       et on fixe une nouvelle valeur de ent qui indiquera le segment 
 *       suivant a explorer 
 *-----------------------------------------------------------------------*/

static int ffnd (BCG *Xgc,ptr_level_f func, int i1, int i2, int i3, int i4, int jj1, int jj2, int jj3, int jj4, int ent, int qq, double Cont, int *zds)
{
  double phi1,phi2,phi3,phi4,xav,yav,phiav;
  int revflag,i;
  phi1=phi_cont(i1,jj1)-Cont;
  phi2=phi_cont(i2,jj2)-Cont;
  phi3=phi_cont(i3,jj3)-Cont;
  phi4=phi_cont(i4,jj4)-Cont;
  revflag = 0;
  *zds = 0;
  /* le point au centre du rectangle */
  xav = ( x_cont(i1)+ x_cont(i3))/2.0 ; 
  yav = ( y_cont(jj1)+ y_cont(jj3))/2.0 ; 
  phiav = ( phi1+phi2+phi3+phi4) / 4.0;
  if (ISNAN(phiav)==1) 
    {
      return -1;
    }
  if (  not_same_sign( phiav,phi4)) 
    {
      int l1, k1; 
      double phi;
      revflag = 1 ; 
      l1= i4; k1= jj4;
      i4=i1; jj4 = jj1; i1= l1; jj1= k1;
      l1= i3; k1= jj3;
      i3=i2; jj3= jj2; i2=l1; jj2= k1;
      phi = phi1; phi1 = phi4; phi4= phi;
      phi = phi3; phi3 = phi2; phi2= phi;
    }
  /* on stocke un nouveau point  */
  (*func)(Xgc,1,Cont,f_intercept(0.0,phi1,x_cont(i1),phiav,xav),
	  f_intercept(0.0,phi1,y_cont(jj1),phiav,yav));
  /*
   * on parcourt les segments du rectangle pour voir sur quelle face
   * on sort 
   */
  for  ( i = 0 ;  ; i++)
    { int l1,k1;
    double phi;
    if ( not_same_sign ( phi1,phi2))   /* sortir du for */ break ; 
    if  ( phiav != 0.0 ) 
      {
	(*func)(Xgc,1,Cont,f_intercept(0.0,phi2,x_cont(i2),phiav,xav),
		f_intercept(0.0,phi2,y_cont(jj2),phiav,yav));
      } 
    /* on permutte les points du rectangle */
    l1=i1; k1= jj1;
    i1=i2;jj1=jj2;i2=i3;jj2=jj3;i3=i4;jj3=jj4;i4=l1;jj4=k1;
    phi=phi1; phi1=phi2;phi2=phi3;phi3=phi4;phi4=phi;
    }
  (*func)(Xgc,1,Cont,f_intercept(0.0,phi1,x_cont(i1),phi2,x_cont(i2)),
	  f_intercept(0.0,phi1,y_cont(jj1),phi2,y_cont(jj2)));
  if ( qq==1 && bdyp(i1,jj1) && bdyp(i2,jj2)) *zds = 1 ;
  if ( revflag == 1  &&  ! oddp (i) )  i = i+2;
  return ( 1 + (  ( i + ent + 2) % 4));
}

/* A set of function for storing and tracing level curves.
 *
 */

static int *xcont=NULL,*ycont=NULL;
static double *xdcont=NULL,*ydcont=NULL,*zdcont=NULL;
static int cont_size ;

/*
 * store a point in the current level curve if ival == 0 the level 
 * curve is reinitialized 
 * used for a contour in a 3D drawing and with opengl 
 */

#ifdef WITH_GTKGLEXT 

static void
G_Contstore_ogl(int ival, double xncont, double yncont, double zncont)
{
  int n;
  /* nouveau contour */
  if ( ival == 0) cont_size =0 ;
  n= cont_size + 1;
  xdcont = graphic_alloc(3,n,sizeof(double));
  ydcont = graphic_alloc(4,n,sizeof(double));
  zdcont = graphic_alloc(5,n,sizeof(double));
  if ( (xdcont == NULL) && n != 0) return ; 
  if ( (ydcont == NULL) && n != 0) return ;
  if ( (zdcont == NULL) && n != 0) return ;
  xdcont[cont_size]= xncont;
  ydcont[cont_size]= yncont;
  zdcont[cont_size++]= zncont;
}

#endif 

/*
 * store a point in the current level curve if ival == 0 the level 
 * curve is reinitialized 
 * used for a contour in a 3D drawing 
 */

#ifdef WITH_GTKGLEXT 

static void
Contstore_ogl(BCG *Xgc,int ival, double Cont, double xncont, double yncont)
{
  G_Contstore_ogl(ival,xncont,yncont,Cont);
}

static void
Contstore_1_ogl(BCG *Xgc,int ival, double Cont, double xncont, double yncont)
{
  G_Contstore_ogl(ival,xncont,yncont,ZC);
}

#endif 

/*
 * store a point in the current level curve if ival == 0 the level 
 * curve is reinitialized 
 * used for a contour in a 3D drawing 
 */

static void
G_Contstore_(int ival, int xncont, int yncont)
{
  int n;
  /* nouveau contour */
  if ( ival == 0) cont_size =0 ;
  n= cont_size + 1;
  xcont = graphic_alloc(3,n,sizeof(int));
  ycont = graphic_alloc(4,n,sizeof(int));
  if ( (xcont == NULL) && n != 0) return ; 
  if ( (ycont == NULL) && n != 0) return ;
  xcont[cont_size]= xncont;
  ycont[cont_size++]= yncont;
}

/*
 * store a point in the current level curve if ival == 0 the level 
 * curve is reinitialized 
 * used for a contour in a 3D drawing 
 */

static void
Contstore_(BCG *Xgc,int ival, double Cont, double xncont, double yncont)
{
  G_Contstore_(ival,GEOX(Xgc->scales,xncont,yncont,Cont),GEOY(Xgc->scales,xncont,yncont,Cont));
}


/*
 * store a point in the current level curve if ival == 0 the level 
 * curve is reinitialized 
 * used for a contour in a 3D drawing with projection at level ZC 
 */

static void
Contstore_1(BCG *Xgc,int ival, double Cont, double xncont, double yncont)
{
  G_Contstore_(ival,GEOX(Xgc->scales,xncont,yncont,ZC),GEOY(Xgc->scales,xncont,yncont,ZC));
}

/*
 * store a point in the current level curve if ival == 0 the level 
 * curve is reinitialized 
 * used for a contour in a 2D drawing 
 */

static void
Contstore_2(BCG *Xgc,int ival, double Cont, double xncont, double yncont)
{
  G_Contstore_(ival,XScale(Xgc->scales,xncont),YScale(Xgc->scales,yncont));
}

/* 
 * Explicit drawing of the current level curve with a dash style 
 * The curve level is also drawn as a string according to current 
 * floating point format 
 */

static void ContourTrace(BCG *Xgc,double Cont, int style)
{ 
  char *F;
  int pat,old;
  int close=0, flag=0, uc;
  double angle=0.0;
  char str[100];

  uc = Xgc->graphic_engine->xget_usecolor(Xgc);
  if (uc) {
    pat = Xgc->graphic_engine->xset_pattern(Xgc,style);
    Xgc->graphic_engine->drawpolyline(Xgc,xcont,ycont,cont_size,close);
    Xgc->graphic_engine->xset_pattern(Xgc,pat);
  }
  else {
    old = Xgc->graphic_engine->xset_dash(Xgc,style);
    Xgc->graphic_engine->drawpolyline(Xgc,xcont,ycont,cont_size,close);
    Xgc->graphic_engine->xset_dash(Xgc,old);
  }

  F=Xgc->graphic_engine->xget_fpf(Xgc);
  if ( F[0] == '\0') 
    sprintf(str,ContNumFormat,Cont);
  else 
    sprintf(str,F,Cont);
  Xgc->graphic_engine->displaystring(Xgc,str, xcont[cont_size / 2],ycont[cont_size /2],flag,angle,
				     GR_STR_XLEFT, GR_STR_YBOTTOM);
}

#ifdef WITH_GTKGLEXT 

static void ContourTrace_ogl(BCG *Xgc,double Cont, int style)
{ 
  double xd,yd,zd, angle=0.0;
  char *F;
  int old, flag=0, uc, x,y;
  char str[100];
  
  uc = Xgc->graphic_engine->xget_usecolor(Xgc);
  if (uc)
    {
      drawpolylines3D(Xgc,xdcont,ydcont,zdcont,&style,1,cont_size);
    }
  else {
    int st=0;
    old = Xgc->graphic_engine->xset_dash(Xgc,style);
    drawpolylines3D(Xgc,xdcont,ydcont,zdcont,&st,1,cont_size);
    Xgc->graphic_engine->xset_dash(Xgc,old);
  }

  F=Xgc->graphic_engine->xget_fpf(Xgc);
  if ( F[0] == '\0') 
    sprintf(str,ContNumFormat,Cont);
  else 
    sprintf(str,F,Cont);

  /* FIXME:
   * AxesStrings uses 2D graphics routines
   * I use the fact that 2d and 3d scales are coherent 
   * Thus it is possible to make the geometrical transform 
   * here and use 2d graphics 
   * but it should be better to have a 3D function. 
   */
  nsp_ogl_set_2dview(Xgc);
  xd=xdcont[cont_size / 2];
  yd=ydcont[cont_size / 2];
  zd=zdcont[cont_size / 2];
  x = XScale(Xgc->scales,TRX(Xgc->scales,xd,yd,zd));
  y = YScale(Xgc->scales,TRY(Xgc->scales,xd,yd,zd));
  Xgc->graphic_engine->displaystring(Xgc,str, x,y,flag,angle,GR_STR_XLEFT, GR_STR_YBOTTOM);
  nsp_ogl_set_3dview(Xgc);
}

#endif 



/*
 * Following code is used to store the current level curves as 
 * double in order to access to the stored data at Scilab level 
 */

double *Gxcont,*Gycont;
static int last=-1;
static int count=0; 

/**
 * nsp_get_level_curves_new:
 * @x: 
 * @y: 
 * @mm: 
 * @n: 
 * 
 * Used to acces to the level curves computed 
 * by nsp_contour_if
 * 
 * Return value: 
 **/

int nsp_get_level_curves_new(double **x, double **y, int *mm, int *n)
{
  *x = Gxcont;
  *y = Gycont;
  *mm= 1;
  *n= Gcont_size;
  return 0;
}

static void GContstore_2(BCG *Xgc,int ival, double Cont, double xncont, double yncont)
{
  int n;
  if ( ival == 0) 
    {
      /* Here : ival == 0 means stop the current level curve and 
       * store data at the end but do reset Gcont_size to zero 
       */
      n= Gcont_size + 1;
      Gxcont = graphic_alloc(3,n,sizeof(double));
      Gycont = graphic_alloc(4,n,sizeof(double));
      if ( (Gxcont == NULL) && n != 0) return ; 
      if ( (Gycont == NULL) && n != 0) return ;
      Gxcont[Gcont_size] = Cont;
      if ( last != -1 ) Gycont[last]= count;
      last = Gcont_size;
      Gcont_size++;
      count = 0;
    }
  n= Gcont_size + 1;
  Gxcont = graphic_alloc(3,n,sizeof(double));
  Gycont = graphic_alloc(4,n,sizeof(double));
  if ( (Gxcont == NULL) && n != 0) return ; 
  if ( (Gycont == NULL) && n != 0) return ;
  Gxcont[Gcont_size]=xncont;
  Gycont[Gcont_size++]=yncont;
  count++;
}

static void GContstore_2Last(void)
{
  if ( last != -1 ) Gycont[last]= count;
}











