/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/graphics/Graphics.h"

#define XN2D 63
#define NCURVES2D  3

static void corps(BCG *Xgc);
static void fixbounds(BCG *Xgc, double xmin,     double xmax,     double ymin,     double ymax);
static void polycorps(BCG *Xgc);

static void test2DD(BCG *Xgc)
{
  int sec=10;
  int style[NCURVES2D],aaint[4],n1,n2;
  double x[NCURVES2D*XN2D],y[NCURVES2D*XN2D],brect[4],Wrect[4],Frect[4];
  int i,j,k;
  for ( k= 0 ; k < 50 ; k++)
    {
      for ( j =0 ; j < NCURVES2D ; j++)
	for ( i=0 ; i < XN2D ; i++)
	  {
	    x[i+ XN2D*j]= ((double) i)/10.0;
	    y[i+ XN2D*j]= (double) sin((j+1)*x[i+XN2D*j]);
	  }
      for ( i=0 ; i < NCURVES2D ; i++)
	style[i]= -NCURVES2D+i;
      n1=NCURVES2D;n2=XN2D;
      aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
      Xgc->graphic_engine->xpause(sec);
      Wrect[0]=Wrect[1]=Wrect[2]=Wrect[3]=0.5;
      for ( i=0 ; i < 4 ; i++)
	{
	  Frect[i]=Wrect[i];
	}
      Wrect[0]=Wrect[1]=Wrect[2]=Wrect[3]=0.5;
      setscale2d(Xgc,Wrect,Frect,"nn");
      nsp_plot2d(Xgc,x,y,&n1,&n2,style,"021"," ","ur",brect,aaint);
      Wrect[0]=Wrect[1]=0;Wrect[2]=Wrect[3]=1.0;
      setscale2d(Xgc,Wrect,Frect,"nn");
    }
}

#define XN2DD 2
#define NCURVES2DD  1

static void test2D(BCG *Xgc)
{
  int style[NCURVES2DD],aaint[4],n1,n2;
  double x[NCURVES2DD*XN2DD],y[NCURVES2DD*XN2DD],brect[4];
  int i,j;
  for ( j =0 ; j < NCURVES2DD ; j++)
    {
      i=0;
      x[i+ XN2DD*j]= ((double) i)/10.0;
      y[i+ XN2DD*j]= -9.75;
      i=1;
      x[i+ XN2DD*j]= ((double) i)/10.0;
      y[i+ XN2DD*j]= 1.10;
      }
  for ( i=0 ; i < NCURVES2DD ; i++)
    style[i]= -NCURVES2DD+i;
  n1=NCURVES2DD;n2=XN2DD;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  brect[0]=brect[1]=0;brect[2]=brect[3]=1.0;
  nsp_plot2d(Xgc,x,y,&n1,&n2,style,"011"," ","ur",brect,aaint);
}

#define XN2D2 200
#define NCURVES2D2 1

static void test2D2(BCG *Xgc)
{
  int style[NCURVES2D2],aaint[4],n1,n2;
  double x[NCURVES2D2*XN2D2],y[NCURVES2D2*XN2D2],brect[4];
  int i,j,k,on=1;
  Xgc->graphic_engine->scale->xset_pixmapOn(Xgc,on);
  for ( k=0 ; k < 50 ; k++) 
    {
      for ( j =0 ; j < NCURVES2D2 ; j++)
	for ( i=0 ; i < XN2D2 ; i++)
	  {
	    x[i+ XN2D2*j]= 6*((double) i)/((double) XN2D2);
	    y[i+ XN2D2*j]= (double) sin(k/10.0 +(j+1)*3.14*x[i+XN2D2*j]);
	  }
      n1=NCURVES2D2;n2=XN2D2;
      brect[0]=0;brect[1]= -1.2;brect[2]=7.0;brect[3]=1.2;
      aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
      for ( i=0 ; i < NCURVES2D2 ; i++)
	style[i]= -i-1;
      nsp_plot2d_2(Xgc,"gnn",x,y,&n1,&n2,style,"111",
		   " y=sin(x/10)@y=sin(2*x/10)","ur",brect,aaint);
      Xgc->graphic_engine->scale->xset_show(Xgc);
      Xgc->graphic_engine->scale->xset_pixmapclear(Xgc);
    }
  on=0;
  Xgc->graphic_engine->scale->xset_pixmapOn(Xgc,on);
}


#define XN2D3 63
#define NCURVES2D3 2

static void test2D3(BCG *Xgc)
{
  int style[NCURVES2D3],aaint[4],n1,n2;
  double x[NCURVES2D3*XN2D3],y[NCURVES2D3*XN2D3],brect[4];
  int i,j;
  for ( j =0 ; j < NCURVES2D3 ; j++)
    for ( i=0 ; i < XN2D3 ; i++)
      {
	x[i+ XN2D3*j]= ((double) i)/10.0;
	y[i+ XN2D3*j]= (double) sin((j+1)*x[i+XN2D3*j]);
      }
  n1=NCURVES2D3;n2=XN2D3;
  brect[0]=0;brect[1]= -1.2;brect[2]=7.0;brect[3]=1.2;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  for ( i=0 ; i < NCURVES2D3 ; i++)
    style[i]= -i-1;
  nsp_plot2d_3(Xgc,"gnn",x,y,&n1,&n2,style,"111",
	       " y=sin(x/10)@y=sin(2*x/10)","u",brect,aaint);

}


#define XN2D4 63
#define NCURVES2D4 2

static void test2D4(BCG *Xgc)
{
  int style[NCURVES2D4],aaint[4],n1,n2;
  double x[NCURVES2D4*XN2D4],y[NCURVES2D4*XN2D4],brect[4];
  int i,j;
  for ( j =0 ; j < NCURVES2D4 ; j++)
    for ( i=0 ; i < XN2D4 ; i++)
      {
	x[i+ XN2D4*j]= ((double) i)/10.0;
	y[i+ XN2D4*j]= (double) sin((j+1)*x[i+XN2D4*j]);
      }
  n1=NCURVES2D4;n2=XN2D4;
  brect[0]=0;brect[1]= -1.2;brect[2]=7.0;brect[3]=1.2;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  for ( i=0 ; i < NCURVES2D4 ; i++)
    style[i]= -i-1;
  nsp_plot2d_4(Xgc,"gnn",x,y,&n1,&n2,style,"111",
	       " y=sin(x/10)@y=sin(2*x/10)","ur",brect,aaint);
}


#define XNN1 200
#define NCURVESN1 2


#define XDDN1 10
#define NC2DN1 1

static void test2DN1(BCG *Xgc)
{
  int style[NC2DN1],aaint[4],n1,n2;
  double x[NC2DN1*XDDN1],y[NC2DN1*XDDN1],brect[4];
  int i,j;
  for ( j =0 ; j < NC2DN1 ; j++)
    for ( i=0 ; i < XDDN1 ; i++)
      {
	double xx;
	x[i+ XDDN1*j]= xx=10.3+i;
	y[i+ XDDN1*j]= (double) i;
      }
  for ( i=0 ; i < NC2DN1 ; i++) style[i]= -NC2DN1+i;
  n1=NC2DN1;n2=XDDN1;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  brect[3] = 3.8561362479999999;
  brect[2] = 4.8584755555555557;
  brect[1] = 3.856136206;
  brect[0] = 4.8546488888888888;
  nsp_plot2d_1(Xgc,"enn",x,y,&n1,&n2,style,"151",
	       " ","ur",brect,aaint);
}

static void test2DN2(BCG *Xgc)
{
  int style[NCURVESN1],aaint[4],n1,n2;
  double x[NCURVESN1*XNN1],y[NCURVESN1*XNN1],brect[4];
  int i,j;
  for ( j =0 ; j < NCURVESN1 ; j++)
    for ( i=0 ; i < XNN1 ; i++)
      {
	double xx;
	x[i+ XNN1*j]= xx=10.3+i;
	if ( j ==0) 
	  y[i+ XNN1*j]= (double) log10((double) xx);
	else 
	  y[i+ XNN1*j]= (double) exp10((double) xx/2000);

      }
  for ( i=0 ; i < NCURVESN1 ; i++)
    style[i]= -NCURVESN1+i;
  n1=NCURVESN1;n2=XNN1;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  nsp_plot2d_1(Xgc,"gln",x,y,&n1,&n2,style,"121", "C1:y=log10(x)@C2:y=exp10(x/2.e3) ","ur",brect,aaint);
}

static void test2DN3(BCG *Xgc)
{
  int style[NCURVESN1],aaint[4],n1,n2;
  double x[NCURVESN1*XNN1],y[NCURVESN1*XNN1],brect[4];
  int i,j;
  for ( j =0 ; j < NCURVESN1 ; j++)
    for ( i=0 ; i < XNN1 ; i++)
      {
	double xx;
	x[i+ XNN1*j]= xx=10.3+i;
	if ( j ==0) 
	  y[i+ XNN1*j]= (double) log10((double) xx);
	else 
	  y[i+ XNN1*j]= (double) exp10((double) xx/2000);

      }
  for ( i=0 ; i < NCURVESN1 ; i++)
    style[i]= -NCURVESN1+i;
  n1=NCURVESN1;n2=XNN1;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  nsp_plot2d_1(Xgc,"gnl",x,y,&n1,&n2,style,"121","C1:y=log10(x)@C2:y=exp10(x/2.e3) ","u",brect,aaint);
}




#define XN3D 21
#define YN3D 21
#define VX3D 10

static void test3D(BCG *Xgc)
{
  double z[XN3D*YN3D],x[XN3D],y[YN3D],bbox[6];
  int flag[3],p,q;
  double teta,alpha;
  int i ,j ;
  for ( i=0 ; i < XN3D ; i++) x[i]=10*i;
  for ( j=0 ; j < YN3D ; j++) y[j]=10*j;
  for ( i=0 ; i < XN3D ; i++)
    for ( j=0 ; j < YN3D ; j++) z[i+XN3D*j]= (i-VX3D)*(i-VX3D)+(j-VX3D)*(j-VX3D);
  p= XN3D ; q= YN3D;  teta=alpha=35;
  flag[0]=2;flag[1]=2,flag[2]=4;
  p= XN3D ; q= YN3D;  teta=alpha=35;
  nsp_plot3d(Xgc,x,y,z,&p,&q,&teta,&alpha,"X@Y@Z",flag,bbox);
}



#define XN3DA 21
#define YN3DA 21
#define VX3DA 10

static void test3DA(BCG *Xgc)
{
  double z[XN3DA*YN3DA],x[XN3DA],y[YN3DA],bbox[6];
  int flag[3],p,q,k;
  double teta,alpha;
  int i ,j,on=1;
  Xgc->graphic_engine->scale->xset_pixmapOn(Xgc,on);
  Xgc->graphic_engine->scale->xset_usecolor(Xgc,1);
  for ( i=0 ; i < XN3DA ; i++) x[i]=i/((double) XN3DA) - 2.0;
  for ( j=0 ; j < YN3DA ; j++) y[j]=j/((double) YN3DA) - 2.0;
  for ( k = 0 ; k < 20 ; k++)
    {
      for ( i=0 ; i < XN3DA ; i++)
	for ( j=0 ; j < YN3DA ; j++) z[i+XN3DA*j]= cos( 2*3.14*x[i])*sin(3.14*2*y[j]*(k/40.0+1));
      p= XN3DA ; q= YN3DA;  teta=alpha=35;
      flag[0]=2;flag[1]=(k== 0)? 2:0,flag[2]=0;
      p= XN3DA ; q= YN3DA;  teta=alpha=35;
      nsp_plot3d_1(Xgc,x,y,z,&p,&q,&teta,&alpha,"X@Y@Z",flag,bbox);
      Xgc->graphic_engine->scale->xset_show(Xgc);
      Xgc->graphic_engine->scale->xset_pixmapclear(Xgc);
    }
  Xgc->graphic_engine->scale->xset_pixmapOn(Xgc,0);
  Xgc->graphic_engine->scale->xset_usecolor(Xgc,0);
}


#define XN3D1 21
#define YN3D1 21
#define VX 10

static void test3D1(BCG *Xgc)
{
  double z[XN3D1*YN3D1],x[XN3D1],y[YN3D1],bbox[6];
  int flag[3],p,q;
  double teta,alpha;
  int i ,j ;
  for ( i=0 ; i < XN3D1 ; i++) x[i]=10*i;
  for ( j=0 ; j < YN3D1 ; j++) y[j]=10*j;
  for ( i=0 ; i < XN3D1 ; i++)
    for ( j=0 ; j < YN3D1 ; j++) z[i+XN3D1*j]= (i-VX)*(i-VX)+(j-VX)*(j-VX);
  p= XN3D1 ; q= YN3D1;  teta=alpha=35;
  flag[0]=2;flag[1]=2,flag[2]=3;
  nsp_plot3d_1(Xgc,x,y,z,&p,&q,&teta,&alpha,"X@Y@Z",flag,bbox);
  I3dRotation(Xgc) ;
}


#define XN3D2 21
#define YN3D2 21
#define VX3D2 5

static void test3D2(BCG *Xgc)
{
  double z[XN3D2*YN3D2],x[XN3D2],y[YN3D2],bbox[6];
  int flag[3],p,q;
  double teta,alpha;
  int i ,j ;
  for ( i=0 ; i < XN3D2 ; i++) x[i]=10*i;
  for ( j=0 ; j < YN3D2 ; j++) y[j]=10*j;
  for ( i=0 ; i < XN3D2 ; i++)
    for ( j=0 ; j < YN3D2 ; j++) z[i+XN3D2*j]= (i-VX3D2)*(i-VX3D2);
  p= XN3D2 ; q= YN3D2;  teta=alpha=35;
  flag[0]=2;flag[1]=2,flag[2]=3;
  nsp_plot3d_1(Xgc,x,y,z,&p,&q,&teta,&alpha,"X@Y@Z",flag,bbox);
}



static void testArrows(BCG *Xgc)
{
  int narrowx2=20,j,style=0,iflag=0;
  double arsizex10=20.0;
  double polyx[20],polyy[20];
  double dx=100;
  fixbounds(Xgc,0.0,500.0,0.0,500.0);
  for ( j =0 ; j < 10 ; j++)
    {polyx[2*j]=250;polyy[2*j]=250;}
  for ( j =0 ; j < 10 ; j++)
    {
      int alpha;
      alpha=36*j;
      polyx[2*j+1]=250+ cos(3.14116*alpha/180.0)*dx;
      polyy[2*j+1]=250 -sin(3.14116*alpha/180.0)*dx;
    }
  Xgc->graphic_engine->scale->drawarrows(Xgc,polyx,polyy,narrowx2,arsizex10,&style,iflag);
}

#define XNC 21
#define YNC 21
#define VXC 10

static void TestC(BCG *Xgc,     int ii)
{
  double z[XNC*YNC],x[XNC],y[YNC];
  double zz,bbox[6],zlev=10.0;
  int p,q,i,j,nz,flagnz=0;
  double teta=35,alpha=45;
  int flag[3];
  zz=1;
  for ( i=0 ; i < XNC ; i++) x[i]=10*i;
  for ( j=0 ; j < YNC ; j++) y[j]=10*j;
  for ( i=0 ; i < XNC ; i++)
    for ( j=0 ; j < YNC ; j++) 
      z[i+XNC*j]= (i-VXC)*(i-VXC)-(j-VXC)*(j-VXC);
  p= XNC ; q= YNC; nz= 10;
  flag[0]=ii;
  flag[1]=2;
  flag[2]=3;
  nsp_contour(Xgc,x,y,z,&p,&q,&flagnz,&nz,&zz,&teta,&alpha, "X@Y@Z",flag,bbox,&zlev,0L);
}

#define LEVELNUM 10

static void TestC2d(BCG *Xgc)
{
  int aaint[4];
  double brect[4];
  double z[XNC*YNC],x[XNC],y[YNC];
  int style[LEVELNUM];
  double zz;
  int p,q,i,j,nz,flagnz=0;
  zz=1;
  for ( i=0 ; i < XNC ; i++) x[i]=10*i;
  for ( j=0 ; j < YNC ; j++) y[j]=10*j;
  for ( i=0 ; i < XNC ; i++)
    for ( j=0 ; j < YNC ; j++) 
      z[i+XNC*j]= (i-VXC)*(i-VXC)-(j-VXC)*(j-VXC);
  p= XNC ; q= YNC; nz= 10;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  brect[0]=brect[1]=0;brect[2]=brect[3]=200.0;
  for (i=0; i < LEVELNUM; i++) 
    style[i]= i;
  nsp_contour2(Xgc,x,y,z,&p,&q,&flagnz,&nz,&zz,style,"011", "X@Y@Z",brect,aaint);
}

static void testC1(BCG *Xgc) { TestC(Xgc,2);}
static void testC2(BCG *Xgc) { TestC(Xgc,0);}
static void testC3(BCG *Xgc) { TestC(Xgc,1);}


#define XNCh 21
#define YNCh 21

static void testCh(BCG *Xgc)
{
  double x[XNCh],y[YNCh];
  double fx[XNCh*YNCh], fy[XNCh*YNCh], vrect[4],arfact;
  int i ,j;
  static char fax[]="021";
  for ( i=0 ; i < XNCh ; i++)
    x[i]= -1+2*((double) i)/XNCh;
  for ( j=0 ; j < YNCh ; j++)
    y[j]= -1+2*((double) j)/YNCh;
  for ( i=0 ; i < XNCh ; i++)
    for ( j=0 ; j < YNCh ; j++)
      { 
	fx[i+XNCh*j]= y[j];
	fy[i+XNCh*j]= -x[i] +(1-x[i]*x[i])*y[j];
	
      }

  vrect[0]=vrect[1]= -1 ,vrect[2]=vrect[3]=1;
  arfact=1.0;
  nsp_champ(Xgc,x,y,fx,fy,(i=XNCh,&i),(j=YNCh,&j),fax,vrect,&arfact,0L);
}


#define XNG 21
#define YNG 21
#define VXG 10

static void testG(BCG *Xgc)
{
  int aaint[4];
  double brect[4];
  double z[XNG*YNG],x[XNG],y[YNG];
  int p,q,i,j;
  for ( i=0 ; i < XNG ; i++) x[i]=10*i;
  for ( j=0 ; j < YNG ; j++) y[j]=10*j;
  for ( i=0 ; i < XNG ; i++)
    for ( j=0 ; j < YNG ; j++) 
      z[i+XNG*j]= (i-VXG)*(i-VXG)-(j-VXG)*(j-VXG);
  p= XNG ; q= YNG;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  nsp_draw_matrix(Xgc,x,y,z,&p,&q,"021",brect,aaint,0L);
}


#define XNP3D 201

static void testP3D(BCG *Xgc)
{
  double z[XNP3D],x[XNP3D],y[XNP3D],bbox[6];
  int n,flag[3],i;
  double theta,alpha;
  for ( i=0 ; i < XNP3D ; i++)
    {
      x[i]=10.0*sin(i/10.0);
      y[i]=10.0*cos(i/10.0)+i/10.0;
      z[i]= i*i/1000.0;
    }
  theta=alpha=35;
  flag[0]=0;
  flag[1]=2;
  flag[2]=2;
  nsp_param3d(Xgc,x,y,z,(n=XNP3D,&n),&theta,&alpha,"X@Y@Z",flag,bbox);
}



static void testPattern(BCG *Xgc)
{
  double rect[]={10,10,50,50};
  int i=0,j,k;
  fixbounds(Xgc,-0.0,500.0,-50.0,200.0);
  for ( k =0 ; k < 4 ; k++)
    {
      for ( j =0 ; j < 5 ; j++)
	{ int pat;
	  pat=j+5*k;
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,pat);
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,rect);
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,i);
	  Xgc->graphic_engine->scale->drawrectangle(Xgc,rect);
	  rect[0] += rect[2]+5.0;
	}
      rect[1] += rect[3]+5.0;
      rect[0]  =10.0;
    }
}

static void testColor(BCG *Xgc)
{
  double rect[]={10,10,50,50};
  int j,k;
  fixbounds(Xgc,-0.0,500.0,-50.0,200.0);
  for ( k =0 ; k < 4 ; k++)
    {
      for ( j =0 ; j < 5 ; j++)
	{ 
	  int pat;
	  pat=j+5*k;
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,pat);
	  Xgc->graphic_engine->scale->drawrectangle(Xgc,rect);
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,rect);
	  rect[0] += rect[2]+5.0;
	}
      rect[1] += rect[3]+5.0;
      rect[0]  =10.0;
    }
}


#define XMP 1
#define NCURVESP  2
  
static void testPrim(BCG *Xgc)
{
  int style[NCURVESP],aaint[4],n1,n2;
  double x[NCURVESP*XMP],y[NCURVESP*XMP],brect[4];
  x[0]= -100.0;x[1]=500.0;
  y[0]= -100.0;y[1]=600.0;
  style[0]= -1;
  n1=NCURVESP;n2=XMP;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  nsp_plot2d(Xgc,x,y,&n1,&n2,style,"022"," ","ur",brect,aaint);
  corps(Xgc);
}

/**
static void transl(x,n,val)
     double x[];
     int n,val;
{
  int i;
  for (i=0 ; i < n ; i++)    x[i]=x[i]+val;
}
**/

static void corps(BCG *Xgc)
{
  double x[7],y[7],boxes[7*4],arcs[7*6],xpols[7*7],ypols[7*7],arsize;
  int pats[7],n,i,j,iflag;
  int whiteid,style=0,xiflag=0;
  Xgc->graphic_engine->xset_default(Xgc);
  n=7;
  whiteid = Xgc->graphic_engine->xget_last(Xgc);
  for (i=0; i < 7; i++) x[i]=i*40.00;
  for (i=0; i < 7; i++)
    {
      boxes[4*i]=x[i];
      boxes[4*i+1]=10.000;
      boxes[4*i+2]=30.000;
      boxes[4*i+3]=30.000;
      pats[i]=whiteid+1;
    }
  Xgc->graphic_engine->scale->drawrectangles(Xgc, boxes,pats,n);
  for (i=0; i < 7; i++) boxes[4*i+1]=45.000;
  pats[0]=0;pats[1]=4;pats[2]=8;pats[3]=12;
  pats[4]=15;pats[5]=whiteid;pats[6]=0;
  Xgc->graphic_engine->scale->drawrectangles(Xgc,boxes,pats,n);
  for (i=0; i < 7; i++)
    {
      arcs[6*i]=x[i];
      arcs[6*i+1]=90.000;
      arcs[6*i+2]=30.000;
      arcs[6*i+3]=30.000;
      arcs[6*i+4]=0.000;
      arcs[6*i+5]=64.0*180.000;
    }
  Xgc->graphic_engine->scale->drawarcs(Xgc,arcs,pats,n);
  for (i=0; i < 7; i++)
    {
      arcs[6*i+1]=135.000;
      arcs[6*i+5]=64*360.000;
      pats[i]=whiteid+1;
    }
  Xgc->graphic_engine->scale->drawarcs(Xgc,arcs,pats,n);
  x[0]=x[6]=0.0;x[5]=x[1]=10.0,x[4]=x[2]=20.0;x[3]=30.0;
  y[0]=15.0;y[1]=y[2]=30.0;y[3]=15.0;y[4]=y[5]=0.0;y[6]=15.0;
  for (i=0;i< 7 ; i++) y[i]=y[i]+160.0;
  for (j=0;j<7;j++)
    {
      for (i=0;i< 7 ; i++) 
	{
	  xpols[i+j*7]=x[i]+40*j;
	  ypols[i+j*7]=y[i];
	}
    }
  Xgc->graphic_engine->scale->fillpolylines(Xgc,xpols,ypols,pats,n,n,0);
  pats[0]=0;pats[1]=4;pats[2]=8;pats[3]=12;
  pats[4]=15;pats[5]=whiteid;pats[6]=0;
  for (j=0;j<7;j++)
      for (i=0;i< 7 ; i++) 
	  ypols[i+j*7]=ypols[i+j*7]+60;
  Xgc->graphic_engine->scale->fillpolylines(Xgc,xpols,ypols,pats,n,n,0);
  for (j=0;j<7;j++)
      for (i=0;i< 7 ; i++) 
	  ypols[i+j*7]=ypols[i+j*7]+60;
  for (j=0;j<7;j++) pats[j]=j;
  Xgc->graphic_engine->scale->drawpolylines(Xgc,xpols,ypols,pats,n,n);
  for (j=0;j<7;j++)
    for (i=0;i< 7 ; i++) 
      ypols[i+j*7]=ypols[i+j*7]+60;
  for (j=0;j<7;j++) pats[j]= -j;
  Xgc->graphic_engine->scale->drawpolylines(Xgc,xpols,ypols,pats,n,n);
  for (i=0; i < 7; i++)
    {
      xpols[2*i]=40*i;
      xpols[2*i+1]=xpols[2*i]+30.0;
      ypols[2*i]=360.0+40.0;
      ypols[2*i+1]=360.0+70.0;
    }
  n=14;
  Xgc->graphic_engine->scale->drawsegments(Xgc,xpols,ypols,n,&style,xiflag);
  for (i=0; i < 7; i++)
    {
      ypols[2*i]=360.0+70.0;
      ypols[2*i+1]=360.0+100.0;
    }
  arsize=50;
  Xgc->graphic_engine->scale->drawarrows(Xgc,xpols,ypols,n,arsize,&style,xiflag);
  x[0]=0;x[1]=100;x[2]=200;
  for (i=0; i < 3 ; i++) y[i]=500;
  xpols[0]=10.0;xpols[1]=20.0;xpols[2]=35;
  ypols[0]=ypols[1]=ypols[2]=0.0;
  n=3;
  iflag=1;
  Xgc->graphic_engine->scale->displaynumbers(Xgc,x,y,n,iflag,xpols,ypols);
  for (i=0; i < 3 ; i++) y[i]=550;
  iflag=0;
  Xgc->graphic_engine->scale->displaynumbers(Xgc,x,y,n,iflag,xpols,ypols);

  }

static void testString(BCG *Xgc)
{
  int j,v=0,pat=1;
  double x=50.0,y=0.0;
  fixbounds(Xgc,-200.0,200.0,-200.0,200.0);
  Xgc->graphic_engine->xset_font(Xgc,2,10);
  for ( j =0 ; j < 360; j=j+45)
    {
      pat=pat+2;
      Xgc->graphic_engine->scale->xset_pattern(Xgc,pat);
      Xgc->graphic_engine->scale->displaystring(Xgc,"String",x,y,v,j);
    }
  Xgc->graphic_engine->xset_default(Xgc);
}

static void testXormode(BCG *Xgc)
{
  Xgc->graphic_engine->xset_alufunction1(Xgc,6);
  testPattern(Xgc);
  testString(Xgc);
  Xgc->graphic_engine->xset_alufunction1(Xgc,3);
}

static void testXliness(BCG *Xgc)
{
  char info[10];
  int j,k,ii=0;
  double ang=0.0;
  double x=20.0,y=20.0,w=40.0,h=30.0;
  int npoly=1,polysize=5;
  double polyx[5],polyy[5];
  int whiteid;
  fixbounds(Xgc,0.0,500.0,0.0,250.0);
  whiteid = Xgc->graphic_engine->xget_last(Xgc);
  sprintf(info,"white=%d",(int)whiteid);
  Xgc->graphic_engine->scale->displaystring(Xgc,info,x,y,ii,ang);
  x=10;y=40;
  for ( k =0 ; k < 4 ; k++)
    {
      for ( j =0 ; j < 10 ; j++)
	{ int pat;
	  pat=j+10*k;
	  polyx[0]=x;polyx[1]=x+w;polyx[2]=x+w;polyx[3]=x;polyx[4]=x;
	  polyy[0]=y;polyy[1]=y;polyy[2]=y+h;polyy[3]=y+h;polyy[4]=y;
	  Xgc->graphic_engine->scale->fillpolylines(Xgc,polyx,polyy,&pat,npoly,polysize,0);
	  polyy[0]=polyy[0]-10.0;
	  sprintf(info,"gl=%d",(int)pat);
	  Xgc->graphic_engine->scale->displaystring(Xgc,info,*polyx,*polyy,ii,ang);
  	  x=x+w+5;
	}
      y=y+h+20;
      x=10;
    }
}

static void testMarks(BCG *Xgc)
{
  int style[2],aaint[4],n1,n2,nn1;
  static double x[4],y[4],brect[4];
  n1=2;n2=2;
  x[0]=0; x[1] =0;x[2]=-1;x[3]=1;
  y[0]=-1; y[1] =1;y[2]=0;y[3]=0;
  style[0]=1;style[1]=1;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  brect[0]=brect[1]=0;brect[2]=brect[3]=1.0;
  nsp_plot2d(Xgc,x,y,&n1,&n2,style,"021"," ","ur",brect,aaint);
  n1=2,nn1=10;
  Xgc->graphic_engine->scale->xset_mark(Xgc,n1,nn1);
  n1=1,n2=1;
  x[0]=0;y[0]=0;
  style[0] = -4;
  nsp_plot2d(Xgc,x,y,&n1,&n2,style,"001"," ","ur",brect,aaint);
}


static void testXrects(BCG *Xgc)
{
  int j,k,nrect=1;
  double rect[4],xx=10.0,yy=10.0,w=40.0,h=40.0;
  fixbounds(Xgc,-0.0,500.0,-50.0,200.0);
  for ( k =0 ; k < 4 ; k++)
    {
      for ( j =0 ; j < 10 ; j++)
	{ int pat;
	  pat=j+10*k;
	  rect[0]=xx;rect[1]=yy;rect[2]=w;rect[3]=h;
	  Xgc->graphic_engine->scale->drawrectangles(Xgc,rect,&pat,nrect);
  	  xx=xx+w+5.0;
	}
      yy=yy+h+5.0;
      xx=10.0;
    }
}


#define NF 1
#define NCU  2

static void fixbounds(BCG *Xgc, double xmin,     double xmax,     double ymin,     double ymax)
{
  int style[NCU],aaint[4],n1=1,n2=1;
  double x[NCU*NF],y[NCU*NF],brect[4];
  x[0]=xmin;x[1]=xmax;
  y[0]=ymin;y[1]=ymax;
  style[0]= -1;
  n1=NCU;n2=NF;
  aaint[0]=aaint[2]=2;aaint[1]=aaint[3]=10;
  nsp_plot2d(Xgc,x,y,&n1,&n2,style,"022"," ","ur",brect,aaint);
}
  
static void testPoly(BCG *Xgc)
{
  int style[1],aaint[4],n1=1,n2=1;
  double brect[4],x[1],y[1];
  brect[0]= -5.0;brect[2]=35.0;
  brect[1]= -5.0;brect[3]=35.0;
  style[0]= -1;
  nsp_plot2d(Xgc,x,y,&n1,&n2,style,"010"," ","ur",brect,aaint);
  polycorps(Xgc);
}

#define NPC 7

static void polycorps(BCG *Xgc)
{
  double x[NPC],y[NPC];
  int n=NPC,cf=0;
  x[0]=x[6]=0.0;x[5]=x[1]=10.0,x[4]=x[2]=20.0;x[3]=30.0;
  y[0]=15.0;y[1]=y[2]=30.0;y[3]=15.0;y[4]=y[5]=0.0;y[6]=15.0;
  Xgc->graphic_engine->scale->drawpolyline(Xgc,x,y,n,cf);
}

typedef  struct  {
  char *name;
  void  (*fonc)();} TestOpTab ;

static void vide()
 {}

TestOpTab GraphicTestTab[] ={
 {"test2D+xsetech", test2DD},
 {"test2D", test2D},
 {"plot2d with animation", test2D2},
 {"test2D3", test2D3},
 {"test2D4", test2D4},
 {"test2DN 1", test2DN1},
 {"test2DN 2", test2DN2},
 {"test2DN 3", test2DN3},
 {"test3D", test3D},
 {"test3D animation", test3DA},
 {"test3D1", test3D1},
 {"test3D2", test3D2},
 {"testArrows", testArrows},
 {"testC 1", testC1},
 {"testC 2", testC2},
 {"testC 3", testC3},
 {"testC2d", TestC2d},
 {"testCh", testCh},
 {"testG", testG},
 {"testP3D", testP3D},
 {"testPattern", testPattern},
 {"testColor", testColor},
 {"testPrim", testPrim},
 {"testString", testString},
 {"testXor", testXormode},
 {"testXliness", testXliness},
 {"testXrects", testXrects},
 {"testXpoly", testPoly},
 {"testMark", testMarks},
 {(char *) NULL,vide}
};

