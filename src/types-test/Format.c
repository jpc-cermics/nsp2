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
 *--------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Axis drawing for 2d plots (format selection) 
 * 
 * void  nsp_grformat_e(fmt, desres, xmin, xmax, xpas) : find a format 
 * void  nsp_grformat_e1(fmt, desres, xx, nx)          : find a format 
 * int   graduate(xmi,xma,xi,xa,np1,np2,kminr,kmaxr,ar) 
 *                : change [xmi,xmax] for pretty graduation 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <string.h>
#include <stdio.h>
#include "nsp/math.h"
#include "nsp/graphics-new/Graphics.h"

extern double nsp_dlamch (char *cmach);

static void FormatPrec (char *fmt, int *desres, double xmin, double xmax, 
			double xpas);
static void FormatPrec1 (char *fmt, int *desres, double *xx, int nx);
static int Fsepare (char *fmt, int dec, int *l, double xmin, double xmax, 
		    double xpas);
static int Fsepare1 (char *fmt, int dec, int *l, double *xx, int nx);
static void graduate1 (const double *xmi,const double * xma,double * xi,double * xa,
		       int * np1,int * np2,int * kminr,int * kmaxr,int * ar,int count);

static void gradua (const double *xmi,const double *xma,int * kminr,int *kmaxr,int *ar,int *npr,int *b);
static void decompSup (double x,int * xk,int *  xa,int   b);
static void decompInf (double x,int * xk,int *  xa,int   b);

/*----------------------------------------------------
 * nsp_grformat_e returns a format ("%.*f" or "%.*e")
 * in fmt given xmin,xmax,pas. 
 *   fmt : character string 
 * fmt gives a format which can be used to display
 * number in range xmin:step:xmax  
 * Exemple : nsp_grformat_e(format,min,max,step);
 *           fprintf(format,min+k*step);
 * The format is searched so as to give distinct values 
 * for the numeric values xmin + k*xpas in [xmin,xmax] 
 * and give enough precision. 
 *------------------------------------------------*/

void nsp_grformat_e(char *fmt, double xmin, double xmax, double xpas)
{
  char c;
  int des,len;
  /* format f minimal  */
  for ( des = 0 ; des < 5 ; des++)
    {
      if (Fsepare("%.*f",des,&len,xmin,xmax,xpas)) break;
    }
  if ( des < 5 && len <= 6)
    {
      c='f';
      strcpy(fmt,"%.*f");
    }
  else 
    {
      for ( des = 0 ; des < 5 ; des++)
	{
	  if (Fsepare("%.*e",des,&len,xmin,xmax,xpas)) break;
	}
      c='e';
      strcpy(fmt,"%.*e");
    }
  FormatPrec(fmt,&des,xmin,xmax,xpas);
  sprintf(fmt,"%%.%d%c",des,c);
}

/*
 *  checks if given format gives enough precision 
 *  if not increase it (i.e increase desres) 
 */

static void FormatPrec(char *fmt, int *desres, double xmin, double xmax, double xpas)
{
  char buf1[100],buf2[100];
  int i=0;
  while ( xmin+((double)i)*xpas < xmax && *desres  < 10 )
    {
      double x1,x2,yy1;
      yy1=xmin+((double) i)*xpas;
      sprintf(buf1,fmt,*desres,yy1);
      sprintf(buf2,fmt,*desres,yy1+xpas );
      sscanf(buf1,"%lf",&x1);
      sscanf(buf2,"%lf",&x2);
      if (  Abs((x2-x1 -xpas) /xpas) >= 0.1)  *desres += 1;
      if (  Abs((x1- yy1)/xpas) >= 0.01) *desres +=1;
      i++;
    }
}

/*
 *  checks if format fmt gives different values for numbers 
 *  from xmin to xmax with step xpas. It also returns in variable l
 *  the string length that will result in using the format 
 */

static int Fsepare(char *fmt, int dec, int *l, double xmin, double xmax, double xpas)
{
  double x=xmin;
  char buf1[100],buf2[100];
  *l = 0;
  /**  Take care of : sprintf(buf1,"%.*f",0,1.d230) which overflow in buf1 **/
  /**  we don't use %.*f format if numbers are two big **/
  if (strcmp("%.*f",fmt)==0 && (Abs(xmax)> 1.e+10 || Abs(xmin) > 1.e+10))
    return(0);
  sprintf(buf1,fmt,dec,xmin);
  while ( x < xmax ) 
    { x += xpas;
    strcpy(buf2,buf1);
    sprintf(buf1,fmt,dec,x);
    *l = (((int)strlen(buf1) >= *l) ? strlen(buf1) : *l) ;
    if ( strcmp(buf1,buf2) == 0) return(0);
    }
  return(1);
}

/*--------------------------------------------
 * same as nsp_grformat_e when numbers are given through an 
 * array xx[0:nx-1];
 *------------------------------------------------*/

void nsp_grformat_e1(char *fmt, double *xx, int nx)
{
  char c;
  int des,len;
  /* format f minimal  */
  for ( des = 0 ; des < 5 ; des++)
    {
      if (Fsepare1("%.*f",des,&len,xx,nx)) break;
    }
  if ( des < 5 && len <= 6)
    {
      c='f';
      strcpy(fmt,"%.*f");
    }
  else 
    {
      for ( des = 0 ; des < 5 ; des++)
	{
	  if (Fsepare1("%.*e",des,&len,xx,nx)) break;
	}
      c='e';
      strcpy(fmt,"%.*e");
    }
  FormatPrec1(fmt,&des,xx,nx);
  sprintf(fmt,"%%.%d%c",des,c);
}


/*----------------------------------------------------------
 *  checks if format fmt gives different values for numbers 
 *  from xmin to xmax with step xpas. It also returns in variable l
 *  the string length that will result in using the format 
 *------------------------------------------------------*/

static void FormatPrec1(char *fmt, int *desres, double *xx, int nx)
{
  char buf1[100],buf2[100];
  double xpas;
  int i=0;
  while ( i < nx-1 && *desres  < 10 )
    {
      double x1,x2;
      sprintf(buf1,fmt,*desres,xx[i]);
      sprintf(buf2,fmt,*desres,xx[i+1]);
      sscanf(buf1,"%lf",&x1);
      sscanf(buf2,"%lf",&x2);
      xpas = xx[i+1]-xx[i];
      if ( xpas != 0.0)
	{
	  if (Abs((x2-x1 - xpas) /xpas) >= 0.1)  *desres += 1;
	  if (Abs((x1-xx[i])/xpas) >= 0.1) *desres +=1;
	}
      i++;
    }
}

static int Fsepare1(char *fmt, int dec, int *l, double *xx, int nx)
{
  char buf1[100],buf2[100];
  int i=0;
  *l = 0;
  /**  Take care of : sprintf(buf1,"%.*f",0,1.d230) which overflow in buf1 **/
  /**  we don't use %.*f format if numbers are two big **/
  if (strcmp("%.*f",fmt)==0 && (Abs(xx[nx-1])> 1.e+10 || Abs(xx[0]) > 1.e+10))
    return(0);
  sprintf(buf1,fmt,dec,xx[0]);
  for ( i=1 ; i < nx ; i++)
    { strcpy(buf2,buf1);
    sprintf(buf1,fmt,dec,xx[i]);
    *l = (((int)strlen(buf1) >= *l) ? strlen(buf1) : *l) ;
    if ( strcmp(buf1,buf2) == 0) return(0);
    }
  return(1);
}

/*----------------------------------------------------
 * int graduate(xmi,xma,xi,xa,np1,np2,kminr,kmaxr,ar)
 * (xgraduate at Scilab level)
 * Rescale an interval so as to find a pretty graduation 
 * for [xmi,xma] given seeks (xi,xa,np1,np2)
 * such that  xi <= xmi <= xmax <= xa 
 * with xi et xa  numbers of type  kminr 10^ar and kmaxr 10^ar.
 * then the interval [xi,xa] can be splited in *np2 sub-intervals
 *  ( kminr-kmaxr can be divided by *np2 ) 
 *  x_i= (kminr + i*(kmaxr-kminr)/ (*np2))*10^ar;
 * i=0:(*np2)
 * ecah of the  np2 intervals can in turn be splited in np1 ungraduated 
 * subintervals 
 * [np1,np2] follow the nax parameters of plot2d.
 *  
 *  We also want to keep np2 small ( *np2 <=10 ) 
 *  and we want [xi,xa] to be as close as possible to the interval  
 *  [xmi,xma]
 *---------------------------------------------------- */

int graduate(const double *xmi,const double *xma, double *xi, double *xa,
	     int *np1, int *np2, int *kminr, int *kmaxr, int *ar)
{
  if ( *xmi > *xma) 
    {
      double xma1=*xmi, xmi1=*xma;
      graduate1(&xmi1,&xma1,xi,xa,np1,np2,kminr,kmaxr,ar,0);
    }
  else 
    graduate1(xmi,xma,xi,xa,np1,np2,kminr,kmaxr,ar,0);
  return(0);
}

static void graduate1(const double *xmi,const double *xma, double *xi, double *xa, int *np1, int *np2,
		      int *kminr, int *kmaxr, int *ar, int count)
{
  int npr,b,i,dx,dxmi,dxma;
  /* fprintf(stderr,"[%20.10f,%20.10f]\n",*xmi,*xma); */
  /* 
   * 
   */
  dx   = ( (*xma) != (*xmi) ) ? (int) ceil(log10(Abs((*xma)-(*xmi)))) :0;
  dxmi = (*xmi != 0 ) ? (int) ceil(log10(Abs((*xmi)))) : 0;
  dxma = (*xma != 0 ) ? (int) ceil(log10(Abs((*xma)))) : 0;
  dx=Max(dx-dxmi,dx-dxma);
  /* il faut limiter b de sorte que dans la decomposition */
  /* avec b les nombres entiers manipules ne deviennent pas trop grands */
  /* on choisit donc b < 10 en considerant que le plus grand entier est */
  /* 0x7FFFFFFF */
  /* on prends aussi un b minimum de 3 : pour avoir des intervalles */
  /* plus serr'es  : ce nombre est 'eventuellement a affiner      */
  b=Max(-dx+2,3);
  /* fprintf(stderr,"choix de b=%d",b); */
  if ( b >= 10 )
    {
      double xmi1,xma1;
      int iexp ;
      /* fprintf(stderr,"je ne peux decomposer les 2 nombres sont identiques\n"); */
      /* 
	 a la precision donnee les deux nombre ne peuvent etre decomposer 
	 kmin,kmax devrait sinon depasser maxint
	 on les ecarte de ce qu'il faut pour pouvoir 
	 les separer. 
	 Attention : il faut faire attention de bien choisir iexp
	 pour ne pas boucler la dedans 
      */
      iexp = 9 - dxmi -1; 
      xmi1 = *xmi-exp10((double) - iexp);
      xma1 = *xmi+exp10((double) - iexp);
      if ( count > 1 ) 
	{
	  Sciprintf("Internal Error: Loop in graduate1 \r\n");
	  Sciprintf("Please send a Bug report to scilab@inria.fr\r\n");
	}
      graduate1(&xmi1,&xma1,xi,xa,np1,np2,kminr,kmaxr,ar,count+1);
      return;
    }
  while ( b >= 1 ) 
    {
      /* fprintf(stderr,"\tAppel avec b=%d\n",b); */
      gradua(xmi,xma,kminr,kmaxr,ar,&npr,&b) ;
      *xi= (*kminr)*exp10((double) *ar);
      *xa= (*kmaxr)*exp10((double) *ar);
      /* fprintf(stderr,"\tRes=[%20.10f,%20.10f]-->[%d,%d,10^%d,%d]\n",*xi,*xa ,*kminr,*kmaxr,*ar,npr); */
      *np2= npr;
      if ( *np2 <= 20 ) 
	{
	  /* try to have 0 in the graduation if present */
	  break;
	  /* wrong test if ( ~( *xmi == -*xma  && *np2 % 2 == 0 ))  { break; } */
	}
      else
	b--;
    }
  /* 
     on veut essayer de ne pas depasser 10 intervalles ( *np2 <= 10) 
     pour les intervalle ou on ecrit un nombre,
     or on peut en avoir jusqu'a 20. On regarde si le nombre d'intervalle 
     est divisible. on aura alors une graduation en np2 pour l'ecriture 
     des nombres et une sous graduation np1 juste avec des tirets.
  */
  *np1= 2 ;
  if ( *np2 <= 10 ) return ;
  /* le nombre est > 10 : s'il est impair on rajoute 1 
     pour diviser par deux */
  if ( *np2 % 2 == 1 ) 
    {
      int step ; 
      step = (*kmaxr-*kminr)/(*np2);
      (*np2)++;
      *kmaxr += step;
      *xa =  (*kmaxr)*exp10((double) *ar);
    }
  /* On recherche des diviseurs a nouveaux pour diminuer le nombre 
     d'intervalles */
  for ( i=2 ; i <=10 ; i++)
    {
      if ( *np2 % i == 0)       
	{
	  *np1=i,*np2 /= i;
	  return;
	}
    }
  *np1=*np2;*np2=1;
}

/*
 *  renvoit kminr,kmaxr et ar tels que 
 *  [kminr*10^ar,kmaxr*10^ar] contient [xmi,xma] 
 *  b est un parametre de decompSup,decompInf 
 *  on doit avoir a l'appel xmi < xma.
 *  le choix se fait entre deux intervalles possibles 
 *  on choisit celui qui est le plus proche de [xmi,xma] 
 *  a condition que (kmaxr-kminr) <= 20 
 *  pour b=1 on sait que (kmaxr-kminr ) <= 20 
 *  20 intervalles au plus ( que l'on obtient si xmin et xmax sont 
 *  de signe opposes sinon c'est 10 )
 */

/* np2, and np1 must be smaller than maxint */

#define DMAX 0xFFFFFFF

static void gradua(const double *xmi,const double *xma, int *kminr, int *kmaxr, int *ar, int *npr, int *b)
{
  double x0=*xmi,x1=*xma,loc;
  int x0k,x0a;
  int x1k,x1a;
  int kmin1,kmax1,a1,np1,kmin2,kmax2,a2,np2,kmin,kmax,a,np;
  decompInf(x0,&x0k,&x0a,*b);
  decompSup(x1,&x1k,&x1a,*b);
  /** special cases **/
  if ( x1 == 0.0 )     {      x1a= x0a;}
  if ( x0 == 0.0 )     {      x0a= x1a;}
  loc = Min( floor(x0*exp10((double) -x1a)),((double)DMAX));
  if ( loc < 0) loc = Max( loc, -((double) DMAX));
  kmin1=(int) loc;
  kmax1=x1k;
  np1= Abs(kmax1-kmin1);
  np1= ( np1 < 0 ) ? DMAX : np1;
  if ( np1 > 10 )
    {
      if  ((np1 % 2) == 0) 
	np1 /=2;
      else 
	{
	  np1++; np1 /= 2;
	  kmax1++;
	}
    }
  a1=x1a;
  /* fprintf(stderr,"\t\tsols : [%d,%d].10^%d,n=%d\t",kmin1,kmax1,a1,np1);  */
  kmin2=x0k;
  loc = Min( ceil( x1*exp10((double) -x0a)),((double)DMAX));
  kmax2=(int) loc ;
  np2 = Abs(kmax2-kmin2);
  np2= ( np2 < 0 ) ? DMAX : np2;
  if ( np2 > 10 ) 
    {
      if ( np2 % 2 == 0)
	np2 /=2;
      else 
	{
	  np2++;
	  kmin2--;
	}
    }
  a2=x0a;
  /* fprintf(stderr,"[%d,%d].10^%d=%d\n",kmin2,kmax2,a2,np2);  */
  if ( np1*exp10((double)a1) < np2*exp10((double) a2) )
    {
      if ( np1 <= 20 ) 
	{
	  kmin=kmin1;	  kmax=kmax1;	  np=np1;	  a=a1;
	}
      else 
	{
	  kmin=kmin2;	  kmax=kmax2;	  np=np2;	  a=a2;
	}
    }
  else 
    {
      if ( np2 <= 20 ) 
	{
	  kmin=kmin2;	  kmax=kmax2;	  np=np2;	  a=a2;
	}
      else 
	{
	  kmin=kmin1;	  kmax=kmax1;	  np=np1;	  a=a1;
	}
    }
  *kminr=kmin;
  *kmaxr=kmax;
  *ar=a;
  *npr=np;
  if ( kmin==kmax ) 
    {
      /* 
       * a la precision demandee les deux [xi,xa] est reduit a un point
       * on elargit l'intervalle
       */
      /* fprintf(stderr,"Arg : kmin=kmax=%d",kmin) ; */
      /* fprintf(stderr," a=%d, x0=%f,x1=%f\n",a,x0,x1); */
      (*kminr)-- ; (*kmaxr)++;*npr=2;
    };
}

/*
 * soit x > 0 reel fixe et b entier fixe : alors il existe un unique couple 
 * (k,a) dans NxZ avec k dans [10^(b-1)+1,10^b] tel que 
 * (k-1)*10^a < x <= k 10^a 
 * donne par  a = ceil(log10(x))-b et k=ceil(x/10^a) 
 * decompSup renvoit xk=k et xa=a
 * si x < 0 alors decompSup(x,xk,xa,b) 
 *    s'obtient par decompInf(-x,xk,xa,b) et xk=-xk 
 * Remarque : la taille de l'entier k obtenu est controle par b 
 * il faut choisir b < 10 pour ne pas depasser dans k l'entier maximum
 */

static void decompSup(double x, int *xk, int *xa, int b)
{
  if ( x == 0.0 || isnan(x) ) 
    { 
      *xk=0 ; *xa= 1; /* jpc */
    }
  else 
    {
      if ( x > 0 ) 
	{
	  double xd;
	  static double epsilon; 
	  static int first=0; 
	  if ( first == 0) { epsilon = 10.0*nsp_dlamch("e"); first++ ;}
	  /* if x is very near (k+1)10^a (epsilon machine) 
	   * we increment xk
	   */
	  *xa = (int) ceil(log10(x)) - b ;
	  *xk = (int) ceil(x/exp10((double) *xa));
	  xd = (*xk-1)*exp10((double) *xa);
	  if ( Abs((x-xd)/x) < epsilon ) *xk -= 1;
	}
      else 
	{
	  decompInf(-x,xk,xa,b);
	  *xk = -(*xk);
	}
    }
}
 

/*
 * soit x > 0 alors il existe un unique couple 
 * (k,a) dans NxZ avec k in [10^(b-1),10^b-1] tel que 
 * (k)*10^a <= x < (k+1) 10^a 
 * donne par 
 * a = floor(log10(x))-b+1 et k = floor(x/10^a) 
 * decompInf renvoit xk=k et xa=a
 * si x < 0 alors decompInf(x,xk,xa,b) 
 *    s'obtient par decompSup(-x,xk,xa,b) et xk=-xk 
 */

static void decompInf(double x, int *xk, int *xa, int b)
{
  if ( x == 0.0 || isnan(x) ) 
    { 
      *xk=0 ; *xa= 1; /* jpc */
    }
  else 
    {
      if ( x > 0 ) 
	{
	  double xup;
	  static double epsilon; 
	  static int first=0; 
	  if ( first == 0) { epsilon = 10.0*nsp_dlamch("e"); first++ ;}
	  *xa = (int) floor(log10(x)) -b +1 ;
	  *xk = (int) floor(x/exp10((double) *xa));
	  /* if x is very near (k+1)10^a (epsilon machine) 
	   * we increment xk
	   */
	  xup = (*xk+1)*exp10((double) *xa);
	  if ( Abs((x-xup)/x) < epsilon ) *xk += 1;
	}
      else 
	{
	  decompSup(-x,xk,xa,b);
	  *xk = -(*xk);
	}
    }
}
 

#ifdef TEST 


main() 
{
  double xmin=1.0,xmax=1.0,eps=1.e-10,xi,xa;
  int n1,n2,n,i;
  int kminr,kmaxr,ar;
  xmin=0.0;
  xmax=1.006;
  graduate(&xmin,&xmax,&xi,&xa,&n1,&n2,&kminr,&kmaxr,&ar) ;
  fprintf(stderr,"test, [%f,%f,%d,%d]\n",xi,xa,n1,n2); 
}



#endif


/* 
 * Graduations:  by Francois Delebecque Inria 
 */


/* Add those lines for FD algo on Theticks */
#define ROUND(x) (x<0?ceil((x)-0.5):floor((x)+0.5))

static double spans[18] = {10,12,14,15,16,18,20,25,30,35,40,45,50,60,70,80,90,100};
static int ticks[18] = {11,7,8,4,9,10,11,6,7,8,9,10,11,7,8,9,10,11};
static double width[18] = {1,2,2,5,2,2,2,5,5,5,5,5,5,10,10,10,10,10};
/* end here */

static void flexpo1(double *x, double *f, double *sg, double *scale)
{
  /*    x = sg*f*scale, sg=+-1; scale=10^n; 10 <= f < 100  */
  double xa, k, un=1;
  *sg=un;  xa=*x;
  if (xa<0) {xa=-xa;*sg=-1;}
  *f=xa;*scale=un;
  if (xa<10) 
    {
      for (k=0;++k;)
	{
	  *scale=*scale/10;
	  *f=*f*10;
	  if (*f >= 10) break;
	}
      return;
    }
  if (xa>100) 
    {
      for (k=0;++k;)
	{
	  *scale=*scale*10;
	  *f=*f/10;
	  if (*f <= 100) break;
	}
      return;
    }
}


static void newbnds(double *xminv,double *xmaxv,double *xmin, double *xmax, double *scale)
{
  double fmin, fmax, sgmin, sgmax, sclmax,sclmin, arguc, arguf, scl;
  flexpo1(xminv,&fmin,&sgmin,&sclmin);
  flexpo1(xmaxv,&fmax,&sgmax,&sclmax);
  if ( Abs(*xmaxv) > Abs(*xminv)) 
    {scl=sclmax;}
  else
    {scl=sclmin;}
  arguc=*xmaxv/scl;arguf=*xminv/scl;
  *xmax=ceil(arguc); *xmin=floor(arguf); *scale=scl;
}

static int gradu(double *xmin, double *xmax, double *grads,int * nticks,double * thewidth,int * tst0,double * scal)
{
  int i1,k, w;
  double f,x,sg,scale;

  *tst0 = *xmin == 0;
  x = *xmax - *xmin;
  flexpo1(&x, &f, &sg, &scale);
  for (k = 1; k <= 18; ++k) {
    w = k;
    if (f <= spans[k - 1]) break;
  }
  *nticks = ticks[w - 1];
  *thewidth = width[w - 1];
  *scal=scale;

  grads[0] = *xmin;
  i1 = *nticks - 1;
  for (k = 0; k < i1; ++k) {
    grads[k + 1] = grads[k] + *thewidth * scale;
    if (grads[k + 1] == 0) *tst0=1; 
  }
  return 0;
}


static void gradu2(double *xmax,double *thewidth,double *scal)
{
  double f,x,sg,scale;
  int k, w;

  x = *xmax;
  flexpo1(&x, &f, &sg, &scale);
  for (k = 1; k <= 18; ++k) {
    w = k;
    if (f <= spans[k - 1]) break;
  }
  *thewidth = width[w - 1];
  *scal=scale;
}


static void grds(double *xminv, double *xmaxv,double *gr,int * nticks, double *thewidth,int * tst0,double *scal)
{
  double span,width,low,up;
  double nup,nlow;
  int k;
  span=*xmaxv-*xminv;
  gradu2(&span, thewidth, scal);
  width=*thewidth* *scal;

  nlow= ROUND(*xminv/ width);low=nlow* width;
  nup = ROUND(*xmaxv/ width);up = nup* width;

  if (low>*xminv) {nlow=floor(*xminv/width);low=nlow*width;}
  /* printf("%e %e %e %e %e\n", *xmaxv-up, *scal, nup, width, *thewidth); */
  if (up<*xmaxv) {nup=ceil(*xmaxv/width);up=nup*width;}
  
  *nticks=(int) (nup-nlow+1);
  gr[0]=low;gr[*nticks-1]=up;
  for (k=1; k<*nticks-1; ++k)
    {
      gr[k]=gr[k-1]+width;
    }
}

static int agrandir(double *xmin,double *xmax,double *xlow,double *xup)
{
  int i1;
  static double work[20], thewidth, scal;
  static int i, j, s, nticks, tst0;

  for (s = 0; s <= 100; ++s) {
    i1 = s;
    for (i = 0; i <= i1; ++i) {
      j = s - i;
      *xup = *xmax + (double) i;
      *xlow = *xmin - (double) j;
      gradu(xlow, xup, work, &nticks, &thewidth, &tst0, &scal);
      if (tst0) {
	return 0;
      }
    }
  }
  return 0;
}



/*   Function used to calculate ticks locations for plotting    
 *   real values located between xminv and xmaxv.               
 *   grads is a vector with at most 20 components such that     
 *   grads(1:ngrads) = linspace(xl, xu, ngrads)                 
 *   xl <= xminv, xu >= xmaxv;                                  
 *   If xminv<0 and xmaxv>0 one of the ticks is at zero.        
 *   Author/Copyright 1998-2019 Fran�ois Delebecque.
 */

int gr_compute_ticks(double *xminv,double *xmaxv,double *grads, int *ngrads)
{
  double d1, d2;  int i1;
  double xmin, xmax, work[20], xlow=0, thewidth, xup=0, scale, scal;
  int k, tst0;
  if (*xminv != *xminv) {
    *ngrads=1;grads[0]=*xminv; return 1; 
  }
  if (*xmaxv != *xmaxv) {
    *ngrads=1;grads[0]=*xmaxv; return 1; 
  }

  if (*xminv == *xmaxv) {
    xmin=floor(*xminv);xmax=ceil(*xmaxv);
    if (xmin==xmax) {
      xmax=xmax+1;
      xmin=xmin-1;
      *ngrads=3;grads[0]=xmin;grads[1]=xmin+1;grads[2]=xmax;return 1;
    }
    gr_compute_ticks(&xmin,&xmax,grads,ngrads);
    return 0;
  }
  if (*xminv >= 0 && *xmaxv > 0) {
    if (*xminv > *xmaxv) {
      xmin=*xmaxv;xmax=*xminv;
      grds(&xmin,&xmax,grads,ngrads, &thewidth, &tst0, &scal);
      return 0;
    }
    grds(xminv, xmaxv, grads, ngrads, &thewidth, &tst0, &scal);
    return 0;
  }
  if (*xminv < 0 && *xmaxv <= 0) {
    d1 = -(*xmaxv);	d2 = -(*xminv);
    if (*xmaxv < *xminv) {d1= -(*xminv); d2 = -(*xmaxv);}
    grds(&d1, &d2, work, ngrads, &thewidth, &tst0, &scal);
    i1 = *ngrads;
    for (k = 0; k < i1; ++k) {
      grads[k] = -work[*ngrads - k -1];
    }
    return 0;
  }
  if (*xminv > 0 && *xmaxv <0)
    {
      /*  should never happen ...   */
      d1=*xmaxv; d2=*xminv;
      gr_compute_ticks(&d1, &d2, grads, ngrads);
      return 0;
    }
  newbnds(xminv, xmaxv, &xmin, &xmax, &scale);
  agrandir(&xmin, &xmax, &xlow, &xup);
  gradu(&xlow, &xup, grads, ngrads, &thewidth, &tst0, &scal);
  i1 = *ngrads;
  for (k = 0; k < i1; ++k) {
    grads[k] = scale * grads[k];
  }
  return 0;
} 


