 /*------------------------------------------------------------------
  * Copyright ENPC 2003 
  * Jean-Philippe Chancelier Enpc/Cermics
  * jpc@cermics.enpc.fr 
  *------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "../Parse/Eval.h"

#include "nsp/graphics/Graphics.h"
#include "nsp/gsort-p.h"


static int sci_demo (char *fname,char *code,int flag) ;
static void  nsp_gwin_clear(BCG *Xgc);
static BCG *nsp_check_graphic_context(void);

/*-----------------------------------------------------------
 * Check optional style argument 
 * XXXXXX pb de ménage pas fait 
 *-----------------------------------------------------------*/

static int * check_style(Stack stack,char *fname,char *varname,NspMatrix *var,int size) 
{
  NspMatrix *loc_var;
  int i,*ival;
  if ( var == NULLMAT) 
    {
      size = Max(size,2);
      /* provide a default value */ 
      if (( loc_var = nsp_matrix_create(NVOID,'r',1,size))== NULLMAT) return NULL;
      ival = (int *) loc_var->R;
      for (i = 0 ; i < size ; ++i) ival[i]= i+1;
      if ( size == 1) ival[1]= 1;
    }
  else
    {
      /* check size */ 
      if ( var->mn < size ) 
	{
	  Scierror("%s:optional argument %s is too small (%d<%d)\n",fname,varname,var->mn,size);
	  return NULL;
	}
      ival = (int *) var->R;
      if ( size == 1 && var->mn != 2) 
	{
	  if ((loc_var = nsp_matrix_create(NVOID,'r',1,2))== NULLMAT) return NULL;
	  ival = (int *) loc_var->R;
	  ival[0]= ((int *) var->R)[0];
	  ival[1]=1;
	}
    }
  return ival;
}

/*-----------------------------------------------------------
 * Check optional iflag argument 
 * 3D options 
 *-----------------------------------------------------------*/

static int iflag_def[]={2,2,4};

static int * check_iflag(Stack stack,char *fname,char *varname,NspMatrix *var,int size) 
{
  if ( var == NULLMAT) 
    {
      /* provide a default value */ 
      iflag_def[0]=iflag_def[1]=2;iflag_def[2]=4;
      return iflag_def;
    }
  else
    {
      /* check size */ 
      if ( var->mn < size ) 
	{
	  Scierror("%s:optional argument %s is too small (%d<%d)\n",fname,varname,var->mn,size);
	  return NULL;
	}
      return (int *) var->R;
    }
}

/*-----------------------------------------------------------
 * Check optional iflag argument for param3d 
 *-----------------------------------------------------------*/

static int param_iflag_def[]={2,2,4};

static int * check_param_iflag(Stack stack,char *fname,char *varname,NspMatrix *var)
{
  if ( var == NULLMAT) 
    {
      /* provide a default value */ 
      param_iflag_def[0]=param_iflag_def[1]=2;param_iflag_def[2]=4;
      return param_iflag_def;
    }
  else
    {
      /* check size */ 
      if ( var->mn != 2 ) 
	{
	  Scierror("%s:optional argument %s is too small (%d<2)\n",fname,varname,var->mn);
	  return NULL;
	}
      param_iflag_def[1]= ((int *) var->R)[0];
      param_iflag_def[2]= ((int *) var->R)[1];
      return param_iflag_def;
    }
}

/*-----------------------------------------------------------
 * Check optional argument rect
 *-----------------------------------------------------------*/


static double  ebox_def[]= { 0,1,0,1,0,1};

static double * check_ebox(Stack stack,char *fname,char *varname,NspMatrix *var)
{
  if ( var == NULLMAT) 
    {
      ebox_def[0]=ebox_def[2]=ebox_def[4]=0.0;
      ebox_def[1]=ebox_def[3]=ebox_def[5]=1.0;
      return ebox_def;
    }
  else 
    {
      /* check size */ 
      if ( var->mn != 6 ) 
	{
	  Scierror("%s:optional argument %s should be of size 8\n",fname,varname);
	  return NULL;
	}
      else 
	{
	  return var->R;
	}
    }
}



/*-----------------------------------------------------------
 * Check optional argument rect
 *-----------------------------------------------------------*/

static double def_rect[4]  = {0.,0.,10.0,10.0}; 

static double * check_rect(Stack stack,char *fname,char *varname,NspMatrix *var)
{
  if ( var == NULLMAT) 
    {
      def_rect[0]=def_rect[1]=0.0;
      def_rect[2]=def_rect[3]=10.0;
      return def_rect;
    }
  else 
    {
      /* check size */ 
      if ( var->mn != 4 ) 
	{
	  Scierror("%s:optional argument %s should be of size 4\n",fname,varname);
	  return NULL;
	}
      else 
	{
	  return var->R;
	}
    }
}

/*-----------------------------------------------------------
 * Check optional argument strf
 *-----------------------------------------------------------*/

#define DEFSTRF "081" 
static char def_strf[]  = DEFSTRF;

static char * check_strf(Stack stack,char *fname,char *varname,char *strf)
{

  if ( strf == NULL ) 
    {
      strcpy(def_strf,DEFSTRF);
      return def_strf;
    }
  else 
    {
      if ( strlen(strf) != 3) 
	{
	  Scierror("%s: optional argument strf has wrong size (%d), 3 expected\n",fname,strlen(strf)); 
	  return NULL;
	}
      else 
	return strf;
    }
  
}


/*-----------------------------------------------------------
 * Check optional argument legend 
 *-----------------------------------------------------------*/

#define DEFLEGEND "X@Y@Z"
static char def_legend[]  = DEFLEGEND;

static char * check_legend(Stack stack,char *fname,char *varname,char *legend)
{
  if ( legend == NULL ) 
    {
      strcpy(def_legend,DEFLEGEND);
      return def_legend;
    }
  else 
    {
      return legend;
    }
}

/*-----------------------------------------------------------
 * Check optional argument nax 
 * note that var can be changed by this function 
 *-----------------------------------------------------------*/

static int def_nax[]={2,10,2,10};

static int * check_nax(Stack stack,char *fname,char *varname,NspMatrix *var)
{
  if ( var == NULLMAT) 
    {
      def_nax[0]=def_nax[2]=2;
      def_nax[1]=def_nax[3]=10;
      return def_nax;
    }
  else 
    {
      /* check size */ 
      if ( var->mn != 4 ) 
	{
	  Scierror("%s:optional argument %s should be of size 4\n",fname,varname);
	  return NULL;
	}
      else 
	{
	  int *ivar  = (int *) var->R,i;
	  for (i = 0 ; i < 4; ++i) ivar[i]=Max(ivar[i],0);
	  return ivar;
	}
    }
}

/*-----------------------------------------------------------
 * Check optional argument zminmax
 * note that var can be changed by this function 
 * (Bruno)
 *-----------------------------------------------------------*/

static double def_zminmax[2]  = {0.,0.};

static double * check_zminmax (Stack stack,char *fname,char *varname,NspMatrix *var)
{
  if ( var == NULLMAT) 
    {
      def_zminmax[0]=def_zminmax[1]=0.0;
      return def_zminmax;
    }
  else 
    {
      /* check size */ 
      if ( var->mn != 2 ) 
	{
	  Scierror("%s:optional argument %s should be of size 2\n",fname,varname);
	  return NULL;
	}
      else 
	{
	  return var->R;
	}
    }
}

/*-----------------------------------------------------------
 * Check optional argument colminmax
 * note that var can be changed by this function 
 * (Bruno)
 *-----------------------------------------------------------*/

static int def_colminmax[2]  = {1,1};

static int * check_colminmax (Stack stack,char *fname,char *varname,NspMatrix *var)
{
  if ( var == NULLMAT) 
    {
      def_colminmax[0]=def_colminmax[1]=0.0;
      return def_colminmax;
    }
  else 
    {
      /* check size */ 
      if ( var->mn != 2 ) 
	{
	  Scierror("%s:optional argument %s should be of size 2\n",fname,varname);
	  return NULL;
	}
      else 
	{
	  return (int *)  var->R;
	}
    }
}

/*-----------------------------------------------------------
 * Check optional argument for logflags 
 * note that var can be changed by this function 
 * (Bruno)
 *-----------------------------------------------------------*/

#define DEFLOGFLAGS "gnn" 
static char def_logflags[]  = DEFLOGFLAGS;

static char * check_logflags(Stack stack,char *fname,char *varname,char *logflags)
{
  if ( logflags == NULL ) 
    {
      strcpy(def_logflags,DEFLOGFLAGS);
      return def_logflags;
    }
  else 
    {
      if ( strlen(logflags) == 2 ) 
	{
	  sprintf(def_logflags,"g%c%c",logflags[0],logflags[1]);
	  return def_logflags;
	}
      if ( strlen(logflags) != 3) 
	{
	  Scierror("%s: optional argument %s has wrong size (%d), 3 expected\n",fname,varname,strlen(logflags)); 
	  return NULL;
	}
      else 
	return logflags;
    }
}


/*-------------------------------------------------------------------
 * champ 
 * champ(x,y,fx,fy,[arfact=1.0,rect=[xMin,yMin,xMax,yMax],flag])
 * champ1(x,y,fx,fy,[arfact=1.0,rect=[xMin,yMin,xMax,yMax],flag])
 *-------------------------------------------------------------------*/

static int int_champ_G(Stack stack, int rhs, int opt, int lhs,
		       int func(BCG *Xgc,double *x, double *y, double *fx, double *fy, int *n1, int *n2, char *strflag, 
				double *brect, double *arfact, int lstr))
{
  BCG *Xgc;
  NspMatrix *x,*y,*fx,*fy,*rect=NULL;
  double arfact =1.0,*R;
  char *strf=NULL;
  int_types T[] = {realmat,realmat,realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "arfact",s_double,NULLOBJ,-1},
		      { "rect",realmat,NULLOBJ,-1},
		      { "strf",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fx,&fy,&opts,&arfact,&rect,&strf) == FAIL) return RET_BUG;

  CheckSameDims(stack.fname,3,4,fx,fy);
  CheckDimProp(stack.fname,2,3, y->mn != fx->n);
  CheckDimProp(stack.fname,1,3, x->mn != fx->m);

  if ( fx->mn == 0) { return 0;} 

  if (( R = check_rect(stack,stack.fname,"rect",rect)) == NULL)  return RET_BUG;
  if (( strf = check_strf(stack,stack.fname,"strf",strf))==NULL) return RET_BUG;
  if ( R == def_rect ) strf[1]='5';

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  (*func)(Xgc,x->R,y->R,fx->R,fy->R,&fx->m,&fx->n,strf,R, &arfact, 4L);
  return 0;
}


int int_champ( Stack stack, int rhs, int opt, int lhs)
{
  if (rhs <= 0) return sci_demo(stack.fname,"champ(1:10,1:10,rand(10,10),rand(10,10));",1);
  return int_champ_G( stack, rhs,opt,lhs, nsp_champ);
}

int int_champ1( Stack stack, int rhs, int opt, int lhs)
{
  if (rhs <= 0) return sci_demo(stack.fname,"champ1(1:10,1:10,rand(10,10),rand(10,10));",1);
  return int_champ_G( stack, rhs, opt, lhs,nsp_champ1);
}


/*-----------------------------------------------------------
 *  contour(x,y,z,nz,[theta,alpha,leg,flag,ebox,zlev])
 *-----------------------------------------------------------*/

int int_contour( Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int *iflag,flagx,nnz=10;
  NspMatrix *x,*y,*z,*nz,*Mebox=NULL,*Mflag=NULL;
  double alpha=35.0,theta=45.0,zlev=0.0,*ebox ;
  char *leg=NULL;
  int_types T[] = {realmat,realmat,realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "alpha",s_double,NULLOBJ,-1},
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",realmat,NULLOBJ,-1},
		      { "leg",string,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { "zlevel",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  
  if (rhs <= 0) { return sci_demo(stack.fname,"contour(1:5,1:10,rand(5,10),5);",1); }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&z,&nz,&opts,&alpha,&Mebox,&Mflag,&leg,&theta,&zlev) == FAIL) return RET_BUG;
  
  CheckVector(stack.fname,1,x);
  CheckVector(stack.fname,2,y);
  if ( z->mn == 0) return 0;
  if ( z->m == 1 || z->n == 1) {
    Scierror("%s: third argument is a vector, expecting a matrix \r\n",stack.fname);
    return RET_BUG;
  }

  CheckDimProp(stack.fname,1,3, x->mn != z->m); 
  CheckDimProp(stack.fname,2,3, y->mn != z->n); 

  if ( nz->mn == 0 ) return 0;
  if ( nz->mn == 1 ) {
    flagx = 0;  nnz = Max(1,(int) nz->R[0]); /* number of levels */
  } else {
    flagx = 1;  nnz = nz->mn ; /*levels given */
  }

  if ( leg == NULL) leg = "";

  if (( iflag=check_iflag(stack,stack.fname,"flag",Mflag,3)) == NULL) return RET_BUG;
  if (( ebox=check_ebox(stack,stack.fname,"ebox",Mebox)) == NULL) return RET_BUG;

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  nsp_contour(Xgc,x->R,y->R,z->R,&z->m,&z->n, &flagx, &nnz,nz->R, &theta, &alpha,
	       leg, iflag, ebox, &zlev,strlen(leg));
  return 0;
}


/*-----------------------------------------------------------
 * standard 2d optional arguments 
 *-----------------------------------------------------------*/

static   nsp_option opts_2d[] ={{ "axesflag",s_int,NULLOBJ,-1},
				{ "frameflag",s_int,NULLOBJ,-1},
				{ "leg",string,NULLOBJ,-1},
				{ "logflag",string,NULLOBJ,-1}, 
				{ "nax",mat_int,NULLOBJ,-1},
				{ "rect",realmat,NULLOBJ,-1},
				{ "strf",string,NULLOBJ,-1},
				{ "style",mat_int,NULLOBJ,-1},
				{ NULL,t_end,NULLOBJ,-1}};

int int_check2d(Stack stack,NspMatrix *Mstyle,int **istyle,int ns,
		char **strf,char **leg,
		NspMatrix *Mrect,double **rect,
		NspMatrix *Mnax,int **nax,
		int frame,int axes,char **logflags)
{
  if (( *istyle = check_style(stack,stack.fname,"style",Mstyle,ns))== NULL) return RET_BUG;
  if (( *strf = check_strf(stack,stack.fname,"strf",*strf))==NULL) return RET_BUG;
  if (( *leg = check_legend(stack,stack.fname,"leg",*leg))==NULL) return RET_BUG;
  if (( *rect = check_rect(stack,stack.fname,"rect",Mrect))==NULL) return RET_BUG;
  if (( *nax = check_nax(stack,stack.fname,"nax",Mnax))==NULL) return RET_BUG;
  if (( *logflags= check_logflags(stack,stack.fname,"logflag",*logflags))==NULL) return RET_BUG;

  if ( frame < -1 || frame > 8 ) 
    {
      Scierror("%s: frame must be in the range [0,8]\r\n",stack.fname);
      return RET_BUG;
    }   
  if ( axes < -1 || axes > 5 ) 
    {
      Scierror("%s: axes must be in the range [0,4]\r\n",stack.fname);
      return RET_BUG;
    }   
  if ( *strf == def_strf) {
    if ( *rect != def_rect) *strf[1] = '7';
    if ( *leg != def_legend) *strf[0] = '1';
    if ( *nax != def_nax) *strf[1] = '1';
    if ( frame != -1 )  *strf[1] = (char)(frame+48);
    if ( axes != -1 )   *strf[2] = (char)(axes+48);
  }
  return 0;
}


/*-----------------------------------------------------------
 * contour2d(x,y,z,nz,[style,strf,leg,rect,nax])
 * Attention trop de var optionnelles ici XXXX 
 *-----------------------------------------------------------*/

typedef int (*fc) (BCG *Xgc,double *,double *,double *,int *,int *,int *,int *,double *,int *,char *,
		   char *,double *,int *); 

int int_contour2d_G( Stack stack, int rhs, int opt, int lhs,fc func)
{
  BCG *Xgc;
  int flagx=0,nnz= 10; /* default number of level curves : 10 */
  int frame= -1, axes=-1;
  NspMatrix *x,*y,*z,*nz;

  /* for 2d optional arguments; */
  int *istyle,*nax;
  NspMatrix *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL;

  int_types T[] = {realmat,realmat,realmat,realmat,new_opts, t_end} ;

  if (rhs <= 0) {return  sci_demo(stack.fname,"contour2d(1:5,1:10,rand(5,10),5);",1); }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&z,&nz,&opts_2d,&axes,&frame,&leg,&logflags,&Mnax,&Mrect,&strf,&Mstyle) == FAIL) return RET_BUG;

  CheckVector(stack.fname,1,x);
  CheckVector(stack.fname,2,y);
  if ( z->mn == 0) return 0;
  if ( z->m == 1 || z->n == 1) {
    Scierror("%s: third argument is a vector, expecting a matrix \r\n",stack.fname);
    return RET_BUG;
  }

  CheckDimProp(stack.fname,1,3, x->mn != z->m); 
  CheckDimProp(stack.fname,2,3, y->mn != z->n); 

  /* XXX could be among the optional args */ 

  if ( nz->mn == 0 ) return 0;
  if ( nz->mn == 1 ) {
    flagx = 0;  nnz = Max(1,(int) nz->R[0]);
  } else {
    flagx = 1;  nnz = nz->mn ;
  }

  if ( int_check2d(stack,Mstyle,&istyle,nnz,&strf,&leg,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;
  
  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  (*func)(Xgc,x->R,y->R,z->R,&z->m,&z->n,&flagx,&nnz,nz->R,istyle,strf,leg,rect,nax); /*,strlen(strf),strlen(leg));*/
  return 0;
}


int int_contour2d( Stack stack, int rhs, int opt, int lhs)
{
  return int_contour2d_G(stack,rhs,opt,lhs, nsp_contour2);
}

int int_contour2d1( Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *x,*y,*z, *M,*M1;
  int flagx=0,nz=10; /* default number of level curves : 10 */
  int m,n;
  double  *hl1, *hl2;
  double *znz= NULL;
  int ix4, i;

  CheckRhs(3,4);
  CheckLhs(2,2);

  if ((x = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((y = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((z = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;

  CheckVector(stack.fname,1,x);
  CheckVector(stack.fname,2,y);
  if ( z->mn == 0) return 0;
  if ( z->m == 1 || z->n == 1) {
    Scierror("%s: third argument is a vector, expecting a matrix \r\n",stack.fname);
    return RET_BUG;
  }

  CheckDimProp(stack.fname,1,3, x->mn != z->m); 
  CheckDimProp(stack.fname,2,3, y->mn != z->n); 

  /*     number of level curves */
  if ( rhs == 4 ) 
    {
      if (( M = GetRealMat(stack,4)) == NULLMAT) return RET_BUG;
      if ( M->mn == 1) {
	flagx = 0;  nz = Max(1,(int)M->R[0] ),znz= M->R;
      } else {
	flagx = 1;  nz = M->mn ; znz= M->R;
      }
    }

  ix4 = Max(nz,2);
  
  if ((M = nsp_matrix_create(NVOID,'r',1,ix4))== NULLMAT) return RET_BUG;
  for (i =0 ; i < ix4 ; ++i) ((int *) M->R)[i] = i+1;
  if  (nz == 1) ((int *) M->R)[1] =1;
  Xgc=nsp_check_graphic_context();
  nsp_contour_if(Xgc,x->R,y->R,z->R,&z->m,&z->n,&flagx,&nz,znz,(int *) M->R);
  nsp_get_level_curves(&hl1, &hl2, &x->m, &x->n);
  n=x->n;m=x->m;
  if ( n== 0 ) m=0;

  if ((M1 = nsp_matrix_create(NVOID,'r',m,n))== NULLMAT) return RET_BUG;
  NSP_OBJECT(M1)->ret_pos = 1;
  StackStore(stack,(NspObject *) M1,rhs+1);
  memcpy(M1->R,hl1,(M1->mn)*sizeof(double)) ;
  if ((M1 = nsp_matrix_create(NVOID,'r',m,n))== NULLMAT) return RET_BUG;
  NSP_OBJECT(M1)->ret_pos = 2;
  memcpy(M1->R,hl2,(M1->mn)*sizeof(double)) ;
  StackStore(stack,(NspObject *) M1,rhs+2);

  nsp_matrix_destroy(M);
  return 2;
}


/*-----------------------------------------------------------
 *  param3d(x,y,z,[theta,alpha,leg,flag,ebox,style])
 *  style is used to give style to each curve ( color or mark) 
 *  and param3d1 is no used any more 
 * 
 *  param3d1(x,y,z,[theta,alpha,leg,flag,ebox])
 *  param3d1(x,y,list(z,colors),[theta,alpha,leg,flag,ebox])
 *                XXXX : to be done for backward compatibility ? 
 *-----------------------------------------------------------*/

int int_param3d( Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int *iflag;
  NspMatrix *x,*y,*z,*Mebox=NULL,*flag=NULL,*Mstyle=NULL;
  double alpha=35.0,theta=45.0,*ebox ;
  char *leg=NULL;
  int_types T[] = {realmat,realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "alpha",s_double,NULLOBJ,-1},
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",realmat,NULLOBJ,-1},
		      { "leg",string,NULLOBJ,-1}, /* XXXX string matrix ? */
		      { "style",mat_int,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( rhs <= 0) {return  sci_demo(stack.fname,"t=0:0.1:5*%pi;param3d(sin(t),cos(t),t*0.1);",1); }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&z,&opts,&alpha,&Mebox,&flag,&leg,&Mstyle,&theta) == FAIL) return RET_BUG;

  if ( x->mn == 0 ) return 0;

  CheckSameDims(stack.fname,1,2,x,y);
  CheckSameDims(stack.fname,1,3,x,z);

  if (( iflag = check_param_iflag(stack,stack.fname,"flag",flag))==NULL) return RET_BUG;
  if (( ebox = check_ebox(stack,stack.fname,"ebox",Mebox)) == NULL) return RET_BUG;
  if (( leg = check_legend(stack,stack.fname,"leg",leg)) == NULL) return RET_BUG;

  if ( Mstyle != NULLMAT ) 
    { 
      if ( Mstyle->mn != z->n) 
	{
	  Scierror("%s: style argument is too small (%d), %d values expected \n",stack.fname,Mstyle->mn, z->n);
	  return RET_BUG;
	}
    }
  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);

  if ( Mstyle != NULLMAT ) 
    {
      int izcol=1;
      nsp_param3d_1(Xgc,x->R,y->R,z->R,&z->m,&z->n,&izcol,(int *) Mstyle->R,
		    &theta,&alpha,leg,iflag,ebox);
    }
  else 
    {
      nsp_param3d(Xgc,x->R,y->R,z->R,&z->mn,&theta,&alpha,leg,iflag,ebox);
    }
  return 0;
} 

/*-----------------------------------------------------------
 * used in contourf, to extract contour points 
 *-----------------------------------------------------------*/

int int_c2dex( Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *rep,*rep1;
  int m1,n1;
  double  *hl1, *hl2;

  CheckRhs(-1,0);
  CheckLhs(1,2);

  nsp_get_level_curves(&hl1, &hl2, &m1, &n1);
  if ( n1 == 0) m1=0;
  switch ( lhs  ) 
    {
    case 0 :
    default : 
      break;
    case 1 : 
      if ((rep= nsp_matrix_create(NVOID,'r',m1,n1))==NULLMAT) return RET_BUG;
      NSP_OBJECT(rep)->ret_pos = 1;
      StackStore(stack,(NspObject *) rep,1);
      memcpy(rep->R,hl1,(rep->mn)*sizeof(double)) ;
      return 1;
      break;
    case 2 :
      if ((rep= nsp_matrix_create(NVOID,'r',m1,n1))==NULLMAT) return RET_BUG;
      NSP_OBJECT(rep)->ret_pos = 1;
      StackStore(stack,(NspObject *) rep,1);
      if ((rep1= nsp_matrix_create(NVOID,'r',m1,n1))==NULLMAT) return RET_BUG;
      NSP_OBJECT(rep1)->ret_pos = 2;
      StackStore(stack,(NspObject *) rep1,2);
      memcpy(rep->R,hl1,(rep->mn)*sizeof(double)) ;
      memcpy(rep1->R,hl2,(rep1->mn)*sizeof(double)) ;
      return 2;
    }
  return 0;
}

/*-----------------------------------------------------------
 *   [x,y]=geom3d(x1,y1,z1)
 *-----------------------------------------------------------*/

int int_geom3d( Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *x1,*y1,*z1;

  if (rhs <= 0) { return sci_demo(stack.fname, "t=0:0.1:5*%pi,[x,y]=geom3d(sin(t),cos(t),t/10);",1);}

  CheckRhs(3,3);
  CheckLhs(2,3);

  if ((x1 = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  if ((y1 = GetRealMatCopy(stack,2)) == NULLMAT) return RET_BUG;
  if ((z1 = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;

  CheckSameDims(stack.fname,1,2,x1,y1);
  CheckSameDims(stack.fname,2,3,y1,z1);

  if ( x1->mn  == 0)  {  return 0;}
  Xgc=nsp_check_graphic_context();
  nsp_geom3d(Xgc,x1->R,y1->R,z1->R,&x1->mn);
  NSP_OBJECT(x1)->ret_pos= 1;
  NSP_OBJECT(y1)->ret_pos= 2;
  return 2;
}

/*-----------------------------------------------------------
 * plot3dXXX(x,y,z,opts)
 *-----------------------------------------------------------*/

typedef int (*f3d) (BCG *Xgc,double *,double *,double *,int *p,int *q,double *,double *,char *,int *,double *); 
typedef int (*f3d1)(BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *,double *,char *,int *,double *); 
typedef int (*f3d2)(BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *,double *,char *,int *,double *); 
typedef int (*f3d3)(BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *,double *,char *,int *,double *);

static int plot3d_build_z(Stack stack,NspMatrix *x,NspMatrix *y,NspMatrix *z,NspObject *f, NspObject *fargs);

int int_plot3d_G( Stack stack, int rhs, int opt, int lhs,f3d func,f3d1 func1,f3d2 func2,f3d3 func3)
{
  BCG *Xgc;
  NspObject  *args = NULL,*fobj;/* when z is a function */
  double alpha=35.0,theta=45.0,*ebox ;
  char *leg=NULL;
  NspMatrix *x,*y,*z,*Mcolors=NULL,*Mflag=NULL,*Mebox=NULL;
  int izcol=0, *zcol=NULL,*iflag;
  
  int_types T[] = {realmat,realmat,obj,new_opts, t_end} ;

  nsp_option opts[] ={{ "args",list,NULLOBJ,-1},
		      { "alpha",s_double,NULLOBJ,-1},
		      { "colors",mat_int,NULLOBJ,-1},
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",mat_int,NULLOBJ,-1},
		      { "leg", string,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fobj,&opts,&args,&alpha,&Mcolors,&Mebox,&Mflag,&leg,&theta) == FAIL) return RET_BUG;

  if (x->mn == 0) { return 0;}

  if ( IsNspPList(fobj) )
    {
      /* third argument can be a macro */
      if ((z = nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULL) return RET_BUG;
      if ( plot3d_build_z(stack,x,y,z,fobj,args)== FAIL) 
	{
	nsp_object_destroy((NspObject **) &z);
	  return RET_BUG;
	}
    }
  else if (IsMat(fobj) && ((NspMatrix *) fobj)->rc_type == 'r')
    {
      z =(NspMatrix *) fobj;
    }
  else
    {
      /* here we could accept list(z,colors) to emulate scilab code */
      Scierror("%s: third argument should be a real matrix or a function\n",stack.fname);
      return RET_BUG;
    }

  if (Mcolors == NULLMAT)
    {
      izcol=0;
    } 
  else 
    {
      izcol = 1;
      zcol  = (int *) Mcolors->R;
      CheckDimProp(stack.fname,3,opts[2].position, Mcolors->mn != z->mn  && Mcolors->mn != z->n );
      /* 
       *   Added by E Segre 4/5/4000. In the case where zcol is a 
       *   matrix of the same size as z, we set izcol to 2. This
       *   value is later transmitted to the C2F(fac3dg) routine,
       *   which has been modified to do the interpolated shading 
       *    (see the file SCI/routines/graphics/Plo3d.c 
       */
      if ( Mcolors->mn == z->mn ) izcol=2  ;
    }

  if (( iflag = check_iflag(stack,stack.fname,"flag",Mflag,3))==NULL) return RET_BUG;
  if (( ebox = check_ebox(stack,stack.fname,"ebox",Mebox)) == NULL) return RET_BUG;
  if (( leg = check_legend(stack,stack.fname,"leg",leg)) == NULL) return RET_BUG;


  if ( x->mn == z->mn && x->mn == z->mn && x->mn != 1) 
    {
      if (! ( x->m == y->m && y->m == z->m && x->n == y->n && y->n == z->n)) {
	Scierror("%s: The three first arguments have incompatible length\n",stack.fname);
	return RET_BUG;
      }
    } 
  else 
    {
      CheckDimProp(stack.fname,1,3, x->mn != z->m); 
      CheckDimProp(stack.fname,2,3, y->mn != z->n); 
      if ( x->mn  <= 1 || y->mn <= 1 ) 
	{
	  Scierror("%s: first and second arguments should be of size >= 2\n",stack.fname);
	  return RET_BUG;
	}
    }

  iflag[1]=Max(Min(iflag[1],6),0);
  /* check that iflag[2] and ebox are compatible */
  if ( Mebox != NULLMAT) 
    {
      /* ebox is given then iflag[1] must be 1 or 3 or 5 */
      if ( iflag[1] == 2 ||  iflag[1] == 4 ||  iflag[1] == 6 ) iflag[1]--;
    }
  else
    {
      /* ebox is not given then iflag[1] cannot be 1 or 3 or 5 */
      if ( iflag[1] == 1 ||  iflag[1] == 3 ||  iflag[1] == 5 ) iflag[1]++;
    }
	

  if ( x->mn == 0 || y->mn == 0 || z->mn == 0) { return 0;} 

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);

  if ( x->mn == y->mn && x->mn == z->mn && x->mn != 1) 
    { 
      /*  Here we are in the case where x,y and z specify some polygons */
      if (izcol == 0) 
	{
	  (*func1)(Xgc,x->R,y->R,z->R,zcol,&z->m,&z->n,&theta,&alpha,leg,iflag,ebox);
	} 
      else if (izcol == 2) 
	{
	  /*  New case for the fac3d3 call (interpolated shadig)  */
	  (*func3)(Xgc,x->R,y->R,z->R,zcol,&z->m,&z->n,&theta,&alpha,leg,iflag,ebox);
	}
      else 
	{
	  (*func2)(Xgc,x->R,y->R,z->R,zcol,&z->m,&z->n,&theta,&alpha,leg,iflag,ebox);
	}
    } 
  else 
    {
      /*  Here we are in the standard case  */
      (*func)(Xgc,x->R,y->R,z->R,&z->m,&z->n,&theta,&alpha,leg,iflag,ebox);
    }
  return 0;
}

/* 
 * build z from f(x,y,fargs)
 */

static int plot3d_build_z(Stack stack,NspMatrix *x,NspMatrix *y,NspMatrix *z,NspObject *f, NspObject *fargs)
{
  NspObject *targs[4];
  NspObject *nsp_ret;
  int nret = 1,nargs = 2;
  NspMatrix *xi,*yj;
  NspObject *func, *args = NULL;
  int ret = FAIL,i,j;
  if (( func =nsp_object_copy(f)) == NULL) return RET_BUG;
  if ((nsp_object_set_name(func,"plot3d_build")== FAIL)) return RET_BUG;
  /** extra arguments **/
  if ( fargs != NULL ) 
    {
      if (( args =nsp_object_copy(fargs)) == NULL ) return RET_BUG;
      if ((nsp_object_set_name(args,"arg")== FAIL)) return RET_BUG;
    }
  if ((xi = nsp_matrix_create("xi",'r',1,1))== NULL) return RET_BUG;
  if ((yj = nsp_matrix_create("yj",'r',1,1))== NULL) return RET_BUG;

  if (fargs != NULL ) 
    {
      targs[2]=(NspObject *) args;
      nargs= 3;
    }

  for ( i= 0 ; i < x->mn ; i++) 
    for ( j= 0 ; j < y->mn; j++) 
      {
	xi->R[0]= x->R[i];
	yj->R[0]= y->R[j];
	targs[0] =(NspObject *) xi;
	targs[1] = (NspObject *) yj;
	if ( targs[0]== NULL ||targs[1]== NULL )  goto end;
	/* XXXX a changer pour metre une fonction eval standard */
	if ( nsp_gtk_eval_function((NspPList *)func ,targs,nargs,&nsp_ret,&nret)== FAIL) 
	  goto end;
	if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' && ((NspMatrix *) nsp_ret)->mn==1 )
	  {
	    z->R[i+z->m*j]= ((NspMatrix *) nsp_ret)->R[0];
	  }
	else 
	  {
	    Scierror("%s: evaluation failed for z(%d,%d)\n",stack.fname,i+1,j+1);
	    goto end; 
	  }
      }
  ret = OK;
 end:
  {
    if ( fargs != NULL)nsp_object_destroy(&args);
    nsp_object_destroy(&func);
    nsp_object_destroy((NspObject **)&xi);
    nsp_object_destroy((NspObject **)&yj);
    return ret;
  }
}



int int_plot3d( Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs <= 0) return sci_demo(stack.fname,"t=-%pi:0.3:%pi;plot3d(t,t,sin(t)'*cos(t))",1);
  return int_plot3d_G(stack,rhs,opt,lhs,nsp_plot3d,nsp_plot_fac3d,nsp_plot_fac3d_2,nsp_plot_fac3d_3);
}

int int_plot3d1( Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs <= 0) return sci_demo(stack.fname,"t=-%pi:0.3:%pi;plot3d1(t,t,sin(t)'*cos(t));",1);
  return int_plot3d_G(stack,rhs,opt,lhs,nsp_plot3d_1,nsp_plot_fac3d_1,nsp_plot_fac3d_2,nsp_plot_fac3d_3);
}


/*-----------------------------------------------------------
 *   plot2d(x,y,[style,strf,leg,rect,nax]) 
 *   plot2dxx(x,y,[style,strf,leg,rect,nax])
 *-----------------------------------------------------------*/

int int_plot2d_G( Stack stack, int rhs, int opt, int lhs,int force2d,
		  int (*func)(BCG *Xgc,char *,double *,double *,int *,int *,
			      int *,char *,char *,double *,int *))
{
  BCG *Xgc;
  /* for 2d optional arguments; */
  int *istyle,*nax, frame= -1, axes=-1,ncurves,lcurve;
  NspMatrix *x,*y, *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL, tflag='g';
  
  int_types T[] = {realmat,realmat,new_opts, t_end} ;
  
  /* XXXX : autoriser les appels spéciaux : plot2d(y,opts) plot2d(logf,x,y,opts) plot2d(logf,x,y,opts)  */
  
  if ( GetArgs(stack,rhs,opt,T,&x,&y,&opts_2d,&axes,&frame,&leg,&logflags,&Mnax,&Mrect,&strf,&Mstyle) == FAIL) return RET_BUG;

  /* decide what to do according to (x,y) dimensions */ 

  if ( x->mn == 0 ) {
    /* must assume that logflags="e..." XXXX */
    tflag = 'e';
    if ( y->m == 1) 
      {
	ncurves = 1;
	lcurve = y->n;
      }
    else 
      {
	ncurves = y->n;
	lcurve = y->m;
      }
  }
  else if ( x->m == 1) 
    {
      if ( y->m == 1 ) 
	{
	  /* x= 1xn and y = 1xn */ 
	  if ( x->n != y->n ) 
	    {
	      Scierror("%s: x and y have incompatible length\n",stack.fname);
	      return RET_BUG;
	    }
	  else
	    {
	      ncurves = 1;
	      lcurve  = x->n;
	      tflag = 'g';
	    }
	} 
      else if ( y->n == 1 ) 
	{
	  /* x= 1xn and y = nx1 */ 
	  if ( x->n != y->m ) 
	    {
	      Scierror("%s: x and y have incompatible length\n",stack.fname);
	      return RET_BUG;
	    }
	  else
	    {
	      ncurves = 1;
	      lcurve  = x->n;
	      tflag = 'g';
	    }
	}
      else 
	{
	  /* x= 1xn and y = pxn */ 
	  if ( x->n != y->m ) 
	    {
	      Scierror("%s: x and y have incompatible length\n",stack.fname);
	      return RET_BUG;
	    }
	  else 
	    {
	      ncurves = y->n;
	      lcurve  = y->m;
	      tflag = 'o';
	    }
	}
    }
  
  else if ( x->n == 1) 
    {
      if ( y->m == 1 ) 
	{
	  /* x= nx1 and y = 1xn */ 
	  if ( x->m != y->n ) 
	    {
	      Scierror("%s: x and y have incompatible length\n",stack.fname);
	      return RET_BUG;
	    }
	  else
	    {
	      ncurves = 1;
	      lcurve  = x->m;
	      tflag = 'g';
	    }
	} 
      else if ( y->n == 1 ) 
	{
	  /* x= nx1 and y = nx1 */ 
	  if ( x->m != y->m ) 
	    {
	      Scierror("%s: x and y have incompatible length\n",stack.fname);
	      return RET_BUG;
	    }
	  else
	    {
	      ncurves = 1;
	      lcurve  = x->m;
	      tflag = 'g';
	    }
	}
      else 
	{
	  /* x= nx1 and y = nxq */ 
	  if ( x->m != y->m ) 
	    {
	      Scierror("%s: x and y have incompatible length\n",stack.fname);
	      return RET_BUG;
	    }
	  else 
	    {
	      /* Set the logflag to "o..." */
	      ncurves = y->n;
	      lcurve  = y->m;
	      tflag = 'o';
	    }
	}
    }
  else 
    {
      if ( x->m != y->m ||  x->n != y->n) 
	{
	  Scierror("%s: x and y have incompatible length\n",stack.fname);
	  return RET_BUG;
	}
      else 
	{
	  ncurves = x->n;
	  lcurve  = x->m;
	  tflag = 'g';
	}
    }

  if ( int_check2d(stack,Mstyle,&istyle,ncurves,&strf,&leg,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;
  
  /* logflags */ 
  logflags[0]= tflag;
  
  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  if ( strcmp(logflags,"gnn")==0 && force2d == 0) 
    {
      nsp_plot2d(Xgc,x->R,y->R,&ncurves, &lcurve,istyle,strf,leg,rect,nax);
    }
  else
    {
      (*func)(Xgc,logflags,x->R,y->R,&ncurves, &lcurve,istyle,strf,leg,rect,nax);
    }
  return 0;
}


static int int_plot2d( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(stack.fname,str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,0,nsp_plot2d_1);
}

static int int_plot2d1_1( Stack stack, int rhs, int opt, int lhs) 
{
  static char str[]="x=0:0.1:2*%pi;plot2d([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(stack.fname,str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,nsp_plot2d_1);
}

static int int_plot2d1_2( Stack stack, int rhs, int opt, int lhs) 
{
  static char str[]="x=0:0.1:2*%pi;plot2d2([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(stack.fname,str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,nsp_plot2d_2);
}

static int int_plot2d1_3( Stack stack, int rhs, int opt, int lhs) 
{
  static char str[]="x=0:0.1:2*%pi;plot2d3([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(stack.fname,str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,nsp_plot2d_3);
}

static int int_plot2d1_4( Stack stack, int rhs, int opt, int lhs) 
{
  static char str[]="x=0:0.1:2*%pi;plot2d4([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(stack.fname,str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,nsp_plot2d_4);
}

/*-----------------------------------------------------------
 *  grayplot(x,y,z,[strf,rect,nax])
 *  Attention trop d'args optionnels XXXXX 
 *-----------------------------------------------------------*/

int int_grayplot( Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int frame= -1, axes=-1;
  NspMatrix *x,*y,*z; 

  /* for 2d optional arguments; */
  int *istyle,*nax;
  NspMatrix *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL;

  int_types T[] = {realmat,realmat,realmat,new_opts, t_end} ;

  if ( rhs <= 0) {return sci_demo(stack.fname, "t=-%pi:0.1:%pi;m=sin(t)'*cos(t);grayplot(t,t,m);",1);}

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&z,&opts_2d,&axes,&frame,&leg,&logflags,&Mnax,&Mrect,&strf,&Mstyle) == FAIL) return RET_BUG;

  CheckVector(stack.fname,1,x);
  CheckVector(stack.fname,2,y);
  if ( z->mn == 0) return 0;
  if ( z->m == 1 || z->n == 1) {
    Scierror("%s: third argument is a vector, expecting a matrix \r\n",stack.fname);
    return RET_BUG;
  }

  CheckDimProp(stack.fname,1,3, x->mn != z->m); 
  CheckDimProp(stack.fname,2,3, y->mn != z->n); 

  if ( int_check2d(stack,Mstyle,&istyle,z->mn,&strf,&leg,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;
  
  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  nsp_draw_matrix(Xgc,x->R,y->R,z->R,&z->m,&z->n,strf,rect,nax,strlen(strf));
  return 0;
}

/*-----------------------------------------------------------
 * scimatplot
 * idem optional arguments ....
 *-----------------------------------------------------------*/

int int_matplot(Stack stack, int rhs, int opt, int lhs) 
{
  BCG *Xgc;
  NspMatrix *z; 
  /* for 2d optional arguments; */
  int *istyle,*nax;
  NspMatrix *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  int frame= -1, axes=-1;
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL;

  int_types T[] = {realmat,new_opts, t_end} ;

  if ( rhs <= 0) {return   sci_demo(stack.fname,"m=[1,2;3,4];Matplot(m);",1);}

  if ( GetArgs(stack,rhs,opt,T,&z,&opts_2d,&axes,&frame,&leg,&logflags,&Mnax,&Mrect,&strf,&Mstyle) == FAIL) return RET_BUG;

  if ( z->mn == 0) return 0;

  if ( int_check2d(stack,Mstyle,&istyle,z->mn,&strf,&leg,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;
  
  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  nsp_draw_matrix_1(Xgc,z->R,&z->m,&z->n,strf,rect,nax,strlen(strf));
  return 0;
} 

/*-----------------------------------------------------------
 * Matplot1 
 *-----------------------------------------------------------*/

int int_gray2plot(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *M,*Rect;
  int_types T[] = {realmat, realmat, t_end} ;

  if ( rhs <= 0) return sci_demo(stack.fname,"plot2d([0,10],[0,10],style=0);a=ones(50,50);a= 3*tril(a)+2*a;Matplot1(a,[4,4,9,9]);",1);

  if ( GetArgs(stack,rhs,opt,T,&M,&Rect) == FAIL) return RET_BUG;

  if (M->mn == 0) { return 0;} 

  if ( Rect->mn != 4) 
    {
      Scierror("%s: second argument should be of length 4\n",stack.fname);
      return RET_BUG;
    }

  Xgc=nsp_check_graphic_context();
  nsp_draw_matrix_2(Xgc,M->R, &M->m,&M->n,Rect->R);
  return 0;
} 

/*-----------------------------------------------------------
 * driver(driver_name) or  current_driver=driver()
 * change the default driver used at scilab level 
 * 
 *-----------------------------------------------------------*/

#ifdef WITH_GTKGLEXT 
extern Gengine GL_gengine; 
#endif 

extern Gengine XFig_gengine, Pos_gengine, Gtk_gengine; 
extern BCG  ScilabGCPos, ScilabGCXfig; 

typedef enum { X11_driver, Win_driver, Gtk_driver,  Pos_driver , Fig_driver, Rec_driver } nsp_driver; 

static char *drivers_name[]={ "Gtk", "Win", "X11", "Pos", "Fig", "Rec" , NULL };
static int drivers_id[]={ Gtk_driver, Win_driver, X11_driver,  Pos_driver , Fig_driver, Rec_driver};
static int nsp_current_driver = 0;
static BCG *nsp_current_bcg= NULL ; 

int int_driver(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  int rep; 

  switch (rhs) 
    {
    case 0: 
      if (( S=nsp_smatrix_create_with_length(NVOID,1,1,strlen(drivers_name[nsp_current_driver])))== NULLSMAT)
	return RET_BUG;
      strcpy(S->S[0],drivers_name[nsp_current_driver]);
      StackStore(stack,(NspObject *) S,1);
      NSP_OBJECT(S)->ret_pos = 1;
      return 1;
    case 1 :
      if ((rep= GetStringInArray(stack,1,drivers_name,1)) == -1) return RET_BUG; 
      if ( drivers_id[rep] == Rec_driver ) 
	{
	  Scierror("%s: Rec driver does not exists, use xset('recording',...) to set recording mode on or off \n",stack.fname);
	}
      nsp_current_driver = rep;
      switch ( rep ) 
	{
	case X11_driver: 
	case Win_driver:
	case Gtk_driver: 
	case Rec_driver: 
	  nsp_current_bcg = NULL;break; 
	case  Pos_driver: 
	  nsp_current_bcg = &ScilabGCPos ; 
	  ScilabGCPos.graphic_engine = &Pos_gengine;
	  /* XXX : attention il faut initialiser */ 
	  break;
	case  Fig_driver: 
	  nsp_current_bcg = &ScilabGCXfig ; 
	  ScilabGCXfig.graphic_engine = &XFig_gengine;
	  break;
	}
      return 0;
    default: 
      Scierror("%s: expecting zero or one argument\n",stack.fname);
      return RET_BUG;
    }
}




/*-----------------------------------------------------------
 * erase a graphic window if necessary 
 *-----------------------------------------------------------*/

static void  nsp_gwin_clear(BCG *Xgc)
{
  if ( Xgc != NULL) 
    if ( Xgc->graphic_engine->xget_autoclear(Xgc) == 1 ) 
      {
	Xgc->graphic_engine->clearwindow(Xgc);
	Xgc->graphic_engine->tape_clean_plots(Xgc,Xgc->CurWindow);
      }
}

/**
 * nsp_check_graphic_context:
 * @void: 
 * 
 * If the current driver is Pos or Fig, this function returns the 
 * associated BCG structure else the BCG structure associated to 
 * the current graphic window is returned (If no current graphic 
 * window exists, one is created).
 * 
 * Return value: #NULL or the current BCG to be used 
 **/

static BCG *nsp_check_graphic_context(void)
{
  if ( nsp_current_bcg != NULL ) 
    return nsp_current_bcg; /* Postscript or Xfig */
  else
    return check_graphic_window(); /* a graphic window */
} 


/*-----------------------------------------------------------
 * xarc(...)
 *-----------------------------------------------------------*/

static int int_xarc_G(Stack stack, int rhs, int opt, int lhs,char *name, void (*f)(BCG *Xgc,double arc[]))
{
  BCG *Xgc;
  double arc[6],*val;
  NspMatrix *M1;
  int i;
  switch ( rhs -opt ) 
    {
    case 1 :
      if ((M1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      CheckLength(stack.fname,1,M1,6);
      val = M1->R;
      break;
    case 6 :
      for ( i = 1 ; i <= 6 ; i++) 
	{
	  if (GetScalarDouble(stack,i,arc+i-1) == FAIL) return RET_BUG;
	}
      val = arc;
      break;
    default :
      Scierror("%s: wrong number of rhs argumens (%d), rhs must be 1 or 6\r\n",stack.fname,rhs);
      return RET_BUG;
    }
  Xgc=nsp_check_graphic_context();
  f(Xgc,val);
  return 0;
} 

int int_xarc(Stack stack, int rhs, int opt, int lhs) 
{
  BCG *Xgc=nsp_check_graphic_context();
  return int_xarc_G(stack,rhs,opt,lhs,"xarc",Xgc->graphic_engine->scale->drawarc);
}

int int_xfarc(Stack stack, int rhs, int opt, int lhs) 
{
  BCG *Xgc=nsp_check_graphic_context();
  return int_xarc_G(stack,rhs,opt,lhs,"xfarc",Xgc->graphic_engine->scale->fillarc);
}



/*-----------------------------------------------------------
 * xarcs(arcs,[style])
 *-----------------------------------------------------------*/

typedef  void (*f_xarcs)(BCG *Xgc,double vects[],int fillvect[], int n);


int int_xarcs_G(Stack stack, int rhs, int opt, int lhs,int row,int flag,char *name,f_xarcs f)
{
  BCG *Xgc;
  NspMatrix *arcs,*styles;
  CheckRhs(1,2);
  if ((arcs = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  CheckRows(stack.fname,1,arcs,row) ;

  if (rhs == 2) 
    {
      if ((styles= GetRealMatInt(stack,2))  == NULLMAT) return RET_BUG;
      CheckVector(stack,2,styles);
      if ( styles->mn != arcs->n ) {
	Scierror("%s: first and second arguments have incompatible length\r\n",stack.fname);
	return RET_BUG;
      }
    }
  else 
    {
      switch (flag) 
	{
	case 1: 
	  if (( styles = nsp_matrix_create_impl(1.0,1.0,arcs->n) ) == NULLMAT) return RET_BUG;
	  break;
	default:
	  if (( styles =nsp_mat_zeros(1,arcs->n))  == NULLMAT) return RET_BUG;
	  break;
	  
	}
      styles= Mat2int(styles);
      StackStore(stack,(NspObject *) styles,2); /* to be sure that styles will be cleaned */
    }  

  Xgc=nsp_check_graphic_context();
  f(Xgc,arcs->R,(int *)styles->R,arcs->n);
  return 0;
} 


int int_xarcs(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc=nsp_check_graphic_context();
  return int_xarcs_G(stack,rhs,opt,lhs,6,0,"xarcs",Xgc->graphic_engine->scale->drawarcs);
}

/*-----------------------------------------------------------
 * xrects(rects,[style])
 *-----------------------------------------------------------*/

int int_xrects(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc=nsp_check_graphic_context();
  return int_xarcs_G(stack,rhs,opt,lhs,4,0,"xrects",Xgc->graphic_engine->scale->drawrectangles);
}

/*-----------------------------------------------------------
 *  xfarcs(arcs,[style])
 *-----------------------------------------------------------*/

int int_xfarcs(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc=nsp_check_graphic_context();
  return int_xarcs_G(stack,rhs,opt,lhs,6,1,"xfarcs",Xgc->graphic_engine->scale->fillarcs);
}

/*-----------------------------------------------------------
 *   xarrows(nx,ny,[arsize=,style=])
 *-----------------------------------------------------------*/

int int_xarrows(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int dstyle = -1;
  double arsize=-1.0 ;
  NspMatrix *nx,*ny,*Mstyle=NULL;

  int_types T[] = {realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "arsize",s_double,NULLOBJ,-1},
		      { "style",mat_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  /* N.n =  4 ; N.names= Names, N.types = Topt, N.objs = Tab; */

  if ( GetArgs(stack,rhs,opt,T,&nx,&ny,&opts,&arsize,&Mstyle) == FAIL) return RET_BUG;

  CheckSameDims(stack.fname,1,2,nx,ny);
  if ( nx->mn == 0) { return 0;} 

  Xgc=nsp_check_graphic_context();
  
  if ( Mstyle != NULLMAT) 
    {
      if ( Mstyle->mn == 1) 
	{
	  dstyle = ((int*) Mstyle->R)[0];
	  Xgc->graphic_engine->scale->drawarrows(Xgc,nx->R,ny->R,nx->mn,arsize,&dstyle,0);
	}
      else 
	{
	  if ( Mstyle->mn != nx->mn/2 ) {
	    Scierror("%s: style has a wrong size (%d), expecting (%d)\r\n",stack.fname,Mstyle->mn, nx->mn/2  );
	    return RET_BUG;
	  }
	  Xgc->graphic_engine->scale->drawarrows(Xgc,nx->R,ny->R,nx->mn,arsize,(int*) Mstyle->R,1);
	}
    }
  else 
    {
      Xgc->graphic_engine->scale->drawarrows(Xgc,nx->R,ny->R,nx->mn,arsize,&dstyle,0);
    }
  return 0;
}
 
/*-----------------------------------------------------------
 *   xsegs(xv,yv,style=)
 *-----------------------------------------------------------*/

int int_xsegs(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int dstyle = -1;
  NspMatrix *nx,*ny,*Mstyle=NULL;

  int_types T[] = {realmat,realmat,opts, t_end} ;
  /* names of optional arguments: must be NULL terminated*/
  char *Names[]={"style",NULL};
  /* types of optional arguments */
  int_types Topt[]={mat_int, t_end} ;
  /* table to store optional arguments */ 
  NspObject *Tab[1]; 
  /* table to store optional arguments position */ 
  int posi[1];
  /* structure for optional arguments */
  named_opts N = { 1 , Names, Topt,Tab, posi};
  /* N.n =  4 ; N.names= Names, N.types = Topt, N.objs = Tab; */
  if ( GetArgs(stack,rhs,opt,T,&nx,&ny,&N,&Mstyle) == FAIL) return RET_BUG;

  CheckSameDims(stack.fname,1,2,nx,ny);
  if ( nx->mn == 0) { return 0;} 

  Xgc=nsp_check_graphic_context();

  if ( Mstyle != NULLMAT) 
    {
      if ( Mstyle->mn == 1) 
	{
	  dstyle = ((int*) Mstyle->R)[0];
	  Xgc->graphic_engine->scale->drawsegments(Xgc,nx->R,ny->R,nx->mn,&dstyle,0);
	}
      else 
	{
	  if ( Mstyle->mn != nx->mn/2 ) {
	    Scierror("%s: style has a wrong size (%d), expecting (%d)\r\n",stack.fname,Mstyle->mn, nx->mn/2  );
	    return RET_BUG;
	  }
	  Xgc->graphic_engine->scale->drawsegments(Xgc,nx->R,ny->R,nx->mn,(int*) Mstyle->R,1);
	}
    }
  else 
    {
      Xgc->graphic_engine->scale->drawsegments(Xgc,nx->R,ny->R,nx->mn,&dstyle,0);
    }
  return 0;
} 

/*-----------------------------------------------------------
 * old version : kept for backward compatibility 
 *-----------------------------------------------------------*/
int int_xaxis(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  double l1;
  NspMatrix *l2,*l3,*l4;

  CheckRhs(4,4);
  if (GetScalarDouble(stack,1,&l1) == FAIL) return RET_BUG;
  if ((l2=GetRealMatInt(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,2,l2,2);
  if ((l3=GetRealMat(stack,3)) ==  NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,3,l3,3);
  if ((l4=GetRealMat(stack,4)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,4,l4,2);

  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->scale->drawaxis(Xgc,&l1,(int *)l2->R,l3->R,l4->R);
  return 0;
}

/*-----------------------------------------------------------
 *   [x1,y1,rect]=xchange(x,y,dir)
 *-----------------------------------------------------------*/

int int_xchange(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int i;
  char *dir;
  NspMatrix *l1,*l2,*l3,*l4,*l5;

  CheckRhs(3,3);
  CheckLhs(2,3);
  
  if ((dir = GetString(stack,3)) == (char*)0) return RET_BUG;

  Xgc=nsp_check_graphic_context();
  
  if ( strncmp(dir,"i2f",3) == 0) 
    {
      if ((l1=GetRealMatInt(stack,1)) == NULLMAT ) return RET_BUG;
      if ((l2=GetRealMatInt(stack,2)) == NULLMAT ) return RET_BUG;
      CheckSameDims(stack.fname,1,2,l1,l2);
      if ((l3 = nsp_matrix_create(NVOID,'r',l1->m,l1->n))== NULLMAT ) return RET_BUG;
      if ((l4 = nsp_matrix_create(NVOID,'r',l1->m,l1->n))== NULLMAT ) return RET_BUG;
      scale_i2f(Xgc,l3->R,l4->R,(int *) l1->R,(int *) l2->R,l1->m*l1->n);
    }
  else   if ( strncmp(dir,"f2i",3) == 0) 
    {
      if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
      CheckSameDims(stack.fname,1,2,l1,l2);
      if ((l3 = nsp_matrix_create(NVOID,'r',l1->m,l1->n)) == NULLMAT ) return RET_BUG;
      if ((l4 = nsp_matrix_create(NVOID,'r',l1->m,l1->n)) == NULLMAT ) return RET_BUG;
      l3->convert='i';
      l4->convert='i';
      scale_f2i(Xgc,l1->R,l2->R,(int *)l3->R,(int *)l4->R,l1->m*l1->n);
    }
  else if ( strncmp(dir,"f2s",3) == 0) 
    {
      /* XXXX temporarire */
      if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      CheckLength(stack.fname,1,l1,4);
      if ((l3 = nsp_matrix_create(NVOID,'r',l1->m,l1->n)) == NULLMAT ) return RET_BUG;
      if ((l4 =nsp_mat_zeros(l1->m,l1->n)) == NULLMAT ) return RET_BUG;
      scale_f2wrect(Xgc,l1->R,l3->R);
    }
  else
    {
      Scierror("%s: dir=%s is wrong \r\n",stack.fname,dir);
      return RET_BUG;
    }
  
  if ((l5 = nsp_matrix_create(NVOID,'r',1,4)) == NULLMAT ) return RET_BUG;
  for (i=0; i < 4 ; i++) l5->R[i] =  Xgc->scales->WIRect1[i];
  NSP_OBJECT(l3)->ret_pos = 1;     StackStore(stack,(NspObject *) l3,rhs+1);
  NSP_OBJECT(l4)->ret_pos = 2;     StackStore(stack,(NspObject *) l4,rhs+2);
  if ( lhs >= 3 )  {   NSP_OBJECT(l5)->ret_pos = 3;     StackStore(stack,(NspObject *) l5,rhs+3);}
  return Max(lhs,2);
}

/*-----------------------------------------------------------
 *     convertion d'entier vers double 
 *     d et s peuvent en fait pointer sur le meme tableau 
 *     car la recopie est fait de n,1,-1 
 *      implicit undefined (a-z) 
 *-----------------------------------------------------------*/

int C2F(entier2dXXX)(int *n, double *dx,  int *s)
{
  int ix;
  for (ix = *n -1 ; ix >= 0; --ix) dx[ix] = (double) s[ix];
  return 0;
} 

/*-----------------------------------------------------------
 *     convertion de float vers double 
 *     d et s peuvent en fait pointer sur le meme tableau 
 *     car la recopie est fait de n,1,-1 
 * Parameter adjustments 
 *-----------------------------------------------------------*/

int C2F(simple2dXXX)(int *n,double *dx, float *s)
{
  int ix;
  for (ix = *n-1 ; ix >= 0; --ix)  dx[ix] = (double) s[ix];
  return 0;
} 


/*-----------------------------------------------------------
 *   xclea(x,y,w,h) etendu a xclea([x,y,w,h])
 *-----------------------------------------------------------*/

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val);

int int_xclea(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  double *val;
  CheckRhs(1,4);
  if ( get_rect(stack,rhs,opt,lhs,&val) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->scale->cleararea(Xgc,*val,*(val+1),*(val+2),*(val+3));
  return 0;
} 

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val)
{
  NspMatrix *M1;
  int i;
  static double l[4];
  switch ( rhs -opt ) 
    {
    case 1 :
      if ((M1=GetRealMat(stack,1)) == NULLMAT ) return FAIL;
      CheckLength(stack.fname,1,M1,4);
      *val = M1->R;
      break;
    case 4 :
      for ( i = 1 ; i <= 4 ; i++) 
	{
	  if (GetScalarDouble(stack,i,l+i-1) == FAIL) return FAIL;
	}
      *val = l;
      break;
    default :
      Scierror("%s: wrong number of rhs argumens (%d), rhs must be 1 or 4\r\n",stack.fname,rhs);
      return FAIL;
    }
  return OK;
}


/*-----------------------------------------------------------
 *   xrect(x,y,w,h,opts) etendu a xrect([x,y,w,h],opts)
 *   opts: color =       ( line color )
 *         thickness =       ( line width )
 *         background=   ( also fill the rectangle)
 *-----------------------------------------------------------*/

int int_xrect(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  double *val;
  int cpat,cwidth,back,color,width;

  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,7);

  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&width) == FAIL) return RET_BUG;

  Xgc=nsp_check_graphic_context();

  if ( opt != 0 ) 
    {
      cpat = Xgc->graphic_engine->xget_pattern(Xgc);
      cwidth = Xgc->graphic_engine->xget_thickness(Xgc);
      if ( opts[0].obj != NULLOBJ) 
	{
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,back);
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,val);
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,cpat);
	}
      if ( opts[1].obj != NULLOBJ) 
	Xgc->graphic_engine->scale->xset_pattern(Xgc,color);
      if ( opts[2].obj != NULLOBJ) 
	Xgc->graphic_engine->scale->xset_thickness(Xgc,width);
    }

  Xgc->graphic_engine->scale->drawrectangle(Xgc,val);

  if ( opt != 0 ) 
    {
      Xgc->graphic_engine->scale->xset_pattern(Xgc,cpat);
      Xgc->graphic_engine->scale->xset_thickness(Xgc,cwidth);
    }
  return 0;
} 

/*-----------------------------------------------------------
 *   xfrect(x,y,w,h,opts) etendu a xfrect([x,y,w,h],opts)
 *   opts: color =       ( fill color )
 *-----------------------------------------------------------*/

int int_xfrect(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  double *val;
  int cpat,color;

  nsp_option opts[] ={{ "color",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,5);

  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&color) == FAIL) return RET_BUG;

  Xgc=nsp_check_graphic_context();
  if ( opt != 0 ) 
    {
      cpat = Xgc->graphic_engine->xget_pattern(Xgc);
      if ( opts[0].obj != NULLOBJ) 
	{
	  Xgc->graphic_engine->scale->xset_pattern(Xgc, color);
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,val);
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,cpat);
	}
      else 
	Xgc->graphic_engine->scale->fillrectangle(Xgc,val);
    }
  else 
    {
      Xgc->graphic_engine->scale->fillrectangle(Xgc,val);
    }
  return 0;
} 


/*-----------------------------------------------------------
 *   xclear(window-ids,[tape_clean])
 *   the default value for tape_clean is TRUE 
 *   if tape_clean  is true then 
 *        a tape_clean_plots is also performed 
 *        and scales are reset to default values
 *-----------------------------------------------------------*/

int int_xclear(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int val =TRUE,  ix;
  NspMatrix *l1;

  CheckRhs(0,2) ;
  if ( rhs == 2 )
    {
      if ( GetScalarBool (stack,2,&val) == FAIL) return RET_BUG;
    }
  
  if (rhs >= 1) 
    {
      if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      for (ix = 0 ; ix < l1->mn ; ++ix) 
	{
	  int wid = l1->R[ix];
	  if (( Xgc=window_list_search(wid)) != NULL) 
	    {
	      Xgc->graphic_engine->clearwindow(Xgc);
	      if ( val == TRUE ) Xgc->graphic_engine->tape_clean_plots(Xgc,wid);
	    }
	}
    } 
  else 
    {
      if ((Xgc = window_list_get_first()) != NULL) 
	{
	  Xgc->graphic_engine->clearwindow(Xgc);
	  if ( val == TRUE ) Xgc->graphic_engine->tape_clean_plots(Xgc,Xgc->CurWindow);
	}
    }
  return 0;
} 

/*-----------------------------------------------------------
 *   
 *   x = xclick(clearq=bool,getmotion=bool,getrelease=bool,win=%d,winall=%t)
 *   [but,x,y,win,str]=xclick(clearq=bool,getmotion=bool,getrelease=bool,win=%d,winall=%t)
 *-----------------------------------------------------------*/

int int_xclick(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int clearq=FALSE,motion=FALSE,release=FALSE,winall=FALSE,istr=0,key=FALSE,win, iflag, button,iw,i;
  char buf[128];
  int buf_len=128;
  NspSMatrix *S;
  NspMatrix *rep[5];
  double drep[4];

  nsp_option opts[] ={
    { "clearq",s_bool,NULLOBJ,-1},
    { "getmotion",s_bool,NULLOBJ,-1},
    { "getrelease",s_bool,NULLOBJ,-1},
    { "getkey",s_bool,NULLOBJ,-1},
    { "win",s_int,NULLOBJ,-1},
    { "winall",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};
  
  int_types T[] = {new_opts, t_end} ;
  
  CheckRhs(0,5);
  CheckLhs(1,5);
  
  if ( GetArgs(stack,rhs,opt,T,&opts,&clearq,&motion,&release,&key,&win,&winall) == FAIL) return RET_BUG;

  if ( opts != 0 && winall != TRUE &&  opts[4].obj != NULLOBJ) 
    {
      win = Max(win,0);
      Xgc = window_list_search(win);
      if ( Xgc == NULL ) 
	{
	  Scierror("%s:window %d does not exists\n",stack.fname,win);
	  return RET_BUG;
	}
    }
  else 
    {
      Xgc = window_list_get_first();
      if ( Xgc == NULL ) 
	{
	  Scierror("%s: No graphic window \n",stack.fname,win);
	  return RET_BUG;
	}
      win = Xgc->CurWindow;
    }

  iflag = (clearq == TRUE) ? FALSE : TRUE;

  switch (lhs) {
  case 4 : winall=TRUE;  break; 
  case 5 : 
    if ( opts[4].obj == NULLOBJ) winall=TRUE;
    istr=buf_len;  /* also get menu */
    break;
  }

  if ( winall ) 
    {
      Xgc->graphic_engine->scale->xclick_any(Xgc,buf,&button,&drep[1],&drep[2],&iw,iflag,motion,release,key,istr);
    }
  else 
    {
      Xgc->graphic_engine->scale->xclick(Xgc,buf,&button,&drep[1],&drep[2],iflag,motion,release,key,istr);
      iw=win;
    }

  drep[0]=(double) button;
  drep[3]=(double) iw;
  
  if ( lhs <= 1 ) 
    {
      if (( rep[0] = nsp_matrix_create(NVOID,'r',1,3))== NULLMAT) return RET_BUG;
      StackStore(stack,(NspObject *) rep[0],rhs+1);
      NSP_OBJECT(rep[0])->ret_pos = 1;
      for ( i = 0 ; i < 3 ; i++) rep[0]->R[i] = drep[i];
      return 1;
    }

  for ( i = 1 ; i <= Min(lhs,4) ; i++) 
    {
      if (( rep[i-1] = (NspMatrix *)nsp_create_object_from_double(NVOID,drep[i-1])) == NULLMAT ) return RET_BUG;
      StackStore(stack,(NspObject *) rep[i-1],rhs+i);
      NSP_OBJECT(rep[i-1])->ret_pos = i;
    }

  if ( lhs >= 5) 
    {
      if ( button != -2) { istr = 4; strcpy(buf,"void");}
      if (( S=nsp_smatrix_create_with_length(NVOID,1,1,strlen(buf)))== NULLSMAT) return RET_BUG;
      strcpy(S->S[0],buf);
      StackStore(stack,(NspObject *) S,rhs+5);
      NSP_OBJECT(S)->ret_pos = 5;
    }
  return Max(lhs,1);
}


/*-----------------------------------------------------------
 * 
 *-----------------------------------------------------------*/

int int_xend(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  CheckRhs(-1,0);
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->xend(Xgc);
  return 0;
}

/*-----------------------------------------------------------
 *   xgrid([style])
 *-----------------------------------------------------------*/

int int_xgrid(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int style = 1;
  CheckRhs(-1,1);
  if ( rhs == 1) {
    if (GetScalarInt(stack,1,&style) == FAIL) return RET_BUG;
  }
  Xgc=nsp_check_graphic_context();
  nsp_plot_grid(Xgc,&style);
  return 0;
} 

/*-----------------------------------------------------------
 *   xfpoly(xv,yv,[close])
 *-----------------------------------------------------------*/

int int_xfpoly(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *l1,*l2;
  int close=0;

  CheckRhs(2,3);

  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(stack.fname,1,2,l1,l2);

  if (rhs == 3) {
    if (GetScalarInt(stack,3,&close) == FAIL) return RET_BUG;
  } 

  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->scale->fillpolyline(Xgc,l1->R,l2->R,l1->mn,close);
  return 0;
}

/*-----------------------------------------------------------
 *  xfpolys(xpols,ypols,[fill])
 *  interpolated shading added by polpoth 7/7/2000
 *-----------------------------------------------------------*/

int int_xfpolys(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *l1,*l2,*l3;
  int v1 = 0;

  CheckRhs(2,3);

  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(stack.fname,1,2,l1,l2);

  if (rhs == 3) 
    {
      if ((l3=GetRealMatInt(stack,3)) == NULLMAT ) return RET_BUG;
      if ( l3->mn == l1->mn ) 
	{ 
	  CheckSameDims(stack.fname,1,3,l1,l3);
	  v1=2; /* interpolated shading */
	  if ( l3->m != 3 && l3->m != 4 ) 
	    {
	      Scierror("%s: interpolated shading only works for polygons of size 3 or 4\r\n",stack.fname);
	      return RET_BUG;
	    }
	} 
      else
	{
	  CheckVector(stack.fname,3,l3);
	  CheckDimProp(stack.fname,2,3, l3->mn != l2->n);
	  v1=1; /* flat shading */
	}
    }
  else 
    {
      v1=1; /* flat shading */ 
      if (( l3 =nsp_mat_zeros(1,l2->n))  == NULLMAT) return RET_BUG;
      l3= Mat2int(l3);
    }

  Xgc=nsp_check_graphic_context();    
  
  Xgc->graphic_engine->scale->fillpolylines(Xgc,l1->R,l2->R,(int *)l3->R,l2->n,l2->m,v1);
  
  /* end of Code modified by polpoth 7/7/2000 */
  return 0;
} 

/*-----------------------------------------------------------
 * 
 *-----------------------------------------------------------*/


static char *xget_Table[] = {  "alufunction", "background", "clipoff",  "clipping",  "color",  "colormap",
			       "dashes",    "font",   "font size",    "foreground",  "hidden3d",
			       "lastpattern",  "line mode",   "line style",   "mark",   "mark size", "pattern",
			       "pixmap",   "thickness",  "use color",  "viewport", "wdim",   "white",   "window",
			       "wpdim",   "wpos",  "wresize", "fpf","auto clear",
			       NULL
};


int int_xget(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *M;
  int rep, flagx=0,x1[10], val,i, cl[5],m3,vals[2];

  if ( rhs <= 0) { return sci_demo(stack.fname,"xsetm();",0);}

  CheckRhs(1,2);
  CheckLhs(0,1);

  if ((rep= GetStringInArray(stack,1,xget_Table,1)) == -1) return RET_BUG; 

  if (rhs == 2) { if (GetScalarInt(stack,2,&flagx) == FAIL) return RET_BUG;}

  Xgc=nsp_check_graphic_context();

  switch (rep) 
    {
    case 0: /* {"alufunction",xset_alufunction1,xget_alufunction}, */
      val = Xgc->graphic_engine->xget_alufunction(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 1: /*{"background",xset_background,xget_background},*/
      val = Xgc->graphic_engine->xget_background(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 2: /*{"clipoff",xset_unclip,xget_clip},*/
    case 3: /*{"clipping",xset_clip,xget_clip},*/
      Xgc->graphic_engine->xget_clip(Xgc,cl);
      if ((M = nsp_matrix_create(NVOID,'r',1,5))== NULLMAT) return RET_BUG;
      for ( i = 0 ; i < 5 ; i++) M->R[i]= cl[i];
      StackStore(stack,(NspObject *) M,rhs+1);
      NSP_OBJECT(M)->ret_pos = 1;
      return 1;
      break;
    case 4: /*{"color",xset_pattern,xget_pattern},*/
      val = Xgc->graphic_engine->xget_pattern(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 5: /*{"colormap",xset_colormap,xget_colormap},*/
      Xgc->graphic_engine->xget_colormap(Xgc,&m3,NULL); /* just to get m3 */
      if ((M = nsp_matrix_create(NVOID,'r',m3,3))== NULLMAT) return RET_BUG;
      Xgc->graphic_engine->xget_colormap(Xgc,&m3,M->R);
      StackStore(stack,(NspObject *) M,rhs+1);
      NSP_OBJECT(M)->ret_pos = 1;
      return 1;
      break;
    case 6: /*{"dashes",xset_dash_or_color,xget_dash_or_color},  */
      val = Xgc->graphic_engine->xget_dash(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 7: /*{"font",xset_font,xget_font},*/
      Xgc->graphic_engine->xget_font(Xgc,vals);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case 8: /*{"font size",xset_font,xget_font},*/
      Xgc->graphic_engine->xget_font(Xgc,x1);
      if ( nsp_move_double(stack,1,(double) x1[1]) == FAIL) return RET_BUG;
      return 1;
      break;
    case 9: /*{"foreground",xset_foreground,xget_foreground},*/
      val = Xgc->graphic_engine->xget_foreground(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 10: /*{"hidden3d",xset_hidden3d,xget_hidden3d},*/
      val = Xgc->graphic_engine->xget_hidden3d(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 11: /*{"lastpattern",xset_empty,xget_last},*/
      val = Xgc->graphic_engine->xget_last(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 12: /*{"line mode",xset_absourel,xget_absourel},*/
      val = Xgc->graphic_engine->xget_absourel(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 13: /*{"line style",xset_dash,xget_dash},*/
      val = Xgc->graphic_engine->xget_dash(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 14: /*{"mark",xset_mark,xget_mark},*/
      Xgc->graphic_engine->xget_mark(Xgc,vals);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case 15: /*{"mark size",xset_mark,xget_mark},*/
      Xgc->graphic_engine->xget_mark(Xgc,x1);
      if ( nsp_move_double(stack,1,(double) x1[1]) == FAIL) return RET_BUG;
      return 1;
      break;
    case 16: /*{"pattern",xset_pattern,xget_pattern},*/
      val = Xgc->graphic_engine->xget_pattern(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 17: /*{"pixmap",xset_pixmapOn,xget_pixmapOn},*/
      val = Xgc->graphic_engine->xget_pixmapOn(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 18: /*{"thickness",xset_thickness,xget_thickness},*/
      val = Xgc->graphic_engine->xget_thickness(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 19: /*{"use color",xset_usecolor,xget_usecolor},*/
      val = Xgc->graphic_engine->xget_usecolor(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 20: /*{"viewport",xset_viewport,xget_viewport},*/
      Xgc->graphic_engine->xget_viewport(Xgc,vals,vals+1);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case 21: /*{"wdim",xset_windowdim,xget_windowdim},*/
      Xgc->graphic_engine->xget_windowdim(Xgc,vals,vals+1);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case 22: /*{"white",xset_empty,xget_last},*/
      val = Xgc->graphic_engine->xget_last(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 23: /*{"window",xset_curwin,xget_curwin},*/
      val = Xgc->graphic_engine->xget_curwin();
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 24: /*{"wpdim",xset_popupdim,xget_popupdim},*/
      Xgc->graphic_engine->xget_popupdim(Xgc,vals,vals+1);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case 25: /*{"wpos",xset_windowpos,xget_windowpos},*/
      Xgc->graphic_engine->xget_windowpos(Xgc,vals,vals+1);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case 26: /*{"wresize",xset_wresize,xget_wresize},*/
      val = Xgc->graphic_engine->xget_wresize(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case 27 : /* fpf */
      {
	NspSMatrix *S;
	if (( S=nsp_smatrix_create(NVOID,1,1,Xgc->graphic_engine->xget_fpf(Xgc),1))== NULLSMAT) return RET_BUG;
	MoveObj(stack,1,(NspObject *) S);
	return 1;
      }
      break;
    case 28:  /* auto clear */
      {
	NspSMatrix *S;
	int val = Xgc->graphic_engine->xget_autoclear(Xgc);
	if ( val == 1) 
	  {
	    if (( S=nsp_smatrix_create(NVOID,1,1,"on",1))== NULLSMAT) return RET_BUG;
	  }
	else 
	  {
	    if (( S=nsp_smatrix_create(NVOID,1,1,"off",1))== NULLSMAT) return RET_BUG;
	  }
	MoveObj(stack,1,(NspObject *) S);
	return 1;
      }
      break;
    }
  return RET_BUG;
}

/*-----------------------------------------------------------
 * xinit(name=,wdim=,wpdim=,wresize=,viewport=,file=,opengl=)
 * name: window name 
 * wdim: window dimensions 
 * wpdim: popupdimension
 * wresize: wresize status 
 * viewport: viewport position 
 *-----------------------------------------------------------*/

int int_xinit(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int v1=-1,opengl=FALSE;
  NspMatrix *wdim=NULL,*wpdim=NULL,*viewport=NULL,*wpos=NULL;
  char *name=NULL, *file=NULL;

  /* just optionals arguments */
  int_types T[] = {new_opts, t_end} ;

  nsp_option opts[] ={{ "dim",mat_int,NULLOBJ,-1},
		      { "file",string,NULLOBJ,-1},
		      { "name",string,NULLOBJ,-1},
		      { "opengl",s_bool,NULLOBJ,-1},
		      { "popup_dim",mat_int,NULLOBJ,-1},
		      { "popup_pos",mat_int,NULLOBJ,-1},
		      { "viewport_pos",realmat,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&opts,&wdim,&file,&name,&opengl,&wpdim,&wpos,&viewport) == FAIL) return RET_BUG;

  if (wdim != NULL && wdim->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",stack.fname,"dim");
      return RET_BUG;
    }
  if (wpdim != NULL &&  wpdim->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",stack.fname,"popup_dim");
      return RET_BUG;
    }

  if (viewport != NULL && viewport->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",stack.fname,"viewport_pos");
      return RET_BUG;
    }

  if (wpos != NULL && wpos->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",stack.fname,"popup_pos");
      return RET_BUG;
    }

  if ( nsp_current_bcg != NULL) 
    {
      nsp_current_bcg->graphic_engine->initgraphic("",&v1, 
						   (wdim) ? (int*) wdim->R: NULL ,
						   (wpdim) ? (int*)wpdim->R: NULL,
						   (viewport) ? viewport->R : NULL,
						   (wpos) ? (int*)wpos->R : NULL);
    }
  else 
    {
      if ( opengl == FALSE ) 
	Gtk_gengine.initgraphic("",&v1,
				(wdim) ? (int*)wdim->R: NULL ,
				(wpdim) ? (int*)wpdim->R: NULL,
				(viewport) ? viewport->R : NULL,
				(wpos) ? (int*)wpos->R : NULL);
      else 
	{
#ifdef WITH_GTKGLEXT 
	  GL_gengine.initgraphic("",&v1,
				 (wdim) ? (int*)wdim->R: NULL ,
				 (wpdim) ? (int*)wpdim->R: NULL,
				 (viewport) ? viewport->R : NULL,
				 (wpos) ? (int*)wpos->R : NULL);
#endif 
	}
    }
  /* we should have an other way here to detect that 
   * initgraphic was fine 
   * Il faut aussi retarder le expose dans ce cas la 
   * noter que les dimensions devraient etre donnee plutot a 
   * initgraphic
   */
  Xgc =  window_list_get_first();
  if ( wpdim != NULL )  
    {
      Xgc->graphic_engine->scale->xset_wresize(Xgc,0);
    }
  if ( name != NULL )   Xgc->graphic_engine->setpopupname(Xgc,name);
  return 0;
}

/*-----------------------------------------------------------
 * xlfont(font-name,font-id)
 * fonts=xlfont()
 * Warning sz dimensions must be compatible with periX11.c FONTNUMBER 
 *-----------------------------------------------------------*/

int int_xlfont(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char *str;
  int num;
  Xgc=nsp_check_graphic_context();
  if (rhs  <= 0) 
    {
      Scierror("%s: xlfont to be done  \r\n",stack.fname);
      return RET_BUG;
      /* XXXXX 
      char **S;
      int m = 0;
      C2F(dr1)("xgfont",C2F(cha1).buf,&m,sz,&v,&v,&v,&v,&dv,&dv,&dv,&dv);
      if (m == 0) { LhsVar(1)=0; return 0;}
      if (( S= (char **) MALLOC( (m+1)*sizeof(char*))) == NULL) 
	{
	  Scierror(999,"%s: running out of memory \r\n",fname);
	  return 0;
	}
      count =0;
      for ( i = 0 ; i < m ; i++) {
	if ((S[i]= (char *) MALLOC((sz[i]+1)*sizeof(char))) == NULL) 
	{
	  Scierror(999,"%s: running out of memory \r\n",fname);
	  return 0;
	}
	strncpy(S[i],C2F(cha1).buf+count,sz[i]);
	count += sz[i]; 
	S[i][sz[i]]='\0';
      } 
      S[m]= (char *) 0;
      CreateVarFromPtr(1,"S",&one,&m,S);
      FreeRhsSVar(S);
      LhsVar(1)=1;
      return 0;
      */
      return 0;
    }
  
  CheckRhs(2,2);

  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
  
  Xgc->graphic_engine->loadfamily(str,&num);
  return 0;
}

/*-----------------------------------------------------------
 * xnumb(x,y,nums,[box,angles]) : 
 *-----------------------------------------------------------*/

int int_xnumb(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *l1,*l2,*l3,*l5;
  int flagx=0;

  CheckRhs(3,5);
  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  if ((l3=GetRealMat(stack,3)) == NULLMAT ) return RET_BUG;

  CheckSameDims(stack.fname,1,2,l1,l2);
  CheckSameDims(stack.fname,2,3,l2,l3);
  
  if ( l3->mn == 0) { return 0;}

  if (rhs >= 4) {   if (GetScalarInt(stack,4,&flagx) == FAIL) return RET_BUG;}
  if (rhs >= 5) 
    {   
      if ((l5=GetRealMat(stack,5)) == NULLMAT ) return RET_BUG;
      CheckSameDims(stack.fname,1,5,l1,l5);
    }
  else 
    {
      if ((l5 =nsp_mat_zeros(1,l3->mn)) == NULLMAT) return RET_BUG;
    }

  Xgc=nsp_check_graphic_context();
  
  Xgc->graphic_engine->scale->displaynumbers(Xgc,l1->R,l2->R,l3->mn,flagx,l3->R,l5->R);
  if ( rhs < 5) nsp_matrix_destroy(l5);
  return 0;
} 

/*-----------------------------------------------------------
 *  xpause(microsecs)
 *-----------------------------------------------------------*/

int int_xpause(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int sec=0;
  CheckRhs(-1,1);
  if (rhs == 1){ if (GetScalarInt(stack,1,&sec) == FAIL) return RET_BUG;}
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->xpause(sec);
  return 0;
} 

/*-----------------------------------------------------------
 *  xpoly(xv,yv, [dtype,close])
 *-----------------------------------------------------------*/

int int_xpoly(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int close=0,color,mark,thick;
  int xmark[2],cmark,cthick,ccolor;
  char *type;

  NspMatrix *l1,*l2;
  static char xlines[]="xlines", xmarks[]="xmarks";
  char *dtype=xlines;

  nsp_option opts[] ={
    { "close",s_bool,NULLOBJ,-1},
    { "color",s_int,NULLOBJ,-1},
    { "mark",s_int,NULLOBJ,-1},
    { "thickness",s_int,NULLOBJ,-1},
    { "type",string,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(2,7);
  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(stack.fname,1,2,l1,l2);
  if ( l1->mn == 0 ) return 0;

  if ( get_optional_args(stack,rhs,opt,opts,&close,&color,&mark,&thick,&type) == FAIL) return RET_BUG;

  Xgc=nsp_check_graphic_context();

  if ( opt != 0 ) 
    {
      if ( opts[1].obj != NULLOBJ) 
	{
	  ccolor = Xgc->graphic_engine->xget_pattern(Xgc); 
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,color);
	}
      if ( opts[2].obj != NULLOBJ) 
	{
	  Xgc->graphic_engine->xget_mark(Xgc,xmark); 
	  Xgc->graphic_engine->scale->xset_mark(Xgc,mark,xmark[1]);
	}
      if ( opts[3].obj != NULLOBJ) 
	{
	  cthick = Xgc->graphic_engine->xget_thickness(Xgc); 
	  Xgc->graphic_engine->scale->xset_thickness(Xgc,thick);
	}
      if ( opts[4].obj != NULLOBJ) 
	{
	  if ( strncmp(type,"lines",5) == 0) dtype = xlines;
	  else if (strncmp(type,"marks",5) == 0) dtype = xmarks;
	  else {
	    Scierror("%s: type must be \"lines\" or \"marks\"\r\n",stack.fname);
	    return RET_BUG;
	}
	}
    }


  if ( dtype == xmarks ) 
    Xgc->graphic_engine->scale->drawpolymark(Xgc,l1->R,l2->R,l2->mn);
  else
    Xgc->graphic_engine->scale->drawpolyline(Xgc,l1->R,l2->R,l2->mn,close);

  if ( opt != 0 ) 
    {
      /* reset to default values */
      if ( opts[1].obj != NULLOBJ) Xgc->graphic_engine->scale->xset_pattern(Xgc,ccolor);
      if ( opts[2].obj != NULLOBJ) 
	{
	  xmark[0]= cmark;  Xgc->graphic_engine->scale->xset_mark(Xgc,xmark[0],xmark[1]);
	}
      if ( opts[3].obj != NULLOBJ) Xgc->graphic_engine->scale->xset_thickness(Xgc,cthick);
    }

  return 0;
}

/*-----------------------------------------------------------
 * xpoly_clip(xv,yv,clip_rect,opts)
 * test interface for xpoly with clipping 
 *-----------------------------------------------------------*/

int int_xpoly_clip(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int close=0,color,mark,thick;
  int cthick,ccolor;
  char *type;
  NspMatrix *l1,*l2,*l3;

  nsp_option opts[] ={
    { "close",s_bool,NULLOBJ,-1},
    { "color",s_int,NULLOBJ,-1},
    { "thickness",s_int,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(2,7);
  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  if ((l3=GetRealMat(stack,3)) == NULLMAT ) return RET_BUG;
  CheckSameDims(stack.fname,1,2,l1,l2);

  CheckLength(stack.fname,3,l3,4);
  if ( l1->mn == 0 ) return 0;
  if ( get_optional_args(stack,rhs,opt,opts,&close,&color,&mark,&thick,&type) == FAIL) return RET_BUG;

  Xgc=nsp_check_graphic_context();

  if ( opt != 0 ) 
    {
      if ( opts[1].obj != NULLOBJ) 
	{
	  ccolor = Xgc->graphic_engine->xget_pattern(Xgc); 
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,color);
	}
      if ( opts[2].obj != NULLOBJ) 
	{
	  cthick = Xgc->graphic_engine->xget_thickness(Xgc); 
	  Xgc->graphic_engine->scale->xset_thickness(Xgc,thick);
	}
    }
  
  Xgc->graphic_engine->scale->drawpolyline_clip(Xgc,l1->R,l2->R,l2->mn,l3->R,close);

  if ( opt != 0 ) 
    {
      /* reset to default values */
      if ( opts[1].obj != NULLOBJ) Xgc->graphic_engine->scale->xset_pattern(Xgc,ccolor);
      if ( opts[2].obj != NULLOBJ) Xgc->graphic_engine->scale->xset_thickness(Xgc,cthick);
    }

  return 0;
}


/*-----------------------------------------------------------
 *   xpolys(xpols,ypols,[draw])
 *-----------------------------------------------------------*/

int int_xpolys(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *l1,*l2,*l3;

  CheckRhs(2,3);

  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(stack.fname,1,2,l1,l2);

  if (rhs == 3) 
    {
      if ((l3=GetRealMatInt(stack,3)) == NULLMAT ) return RET_BUG;
      CheckVector(stack.fname,3,l3); 
      CheckDimProp(stack.fname,1,3, l3->mn < l1->n);
    }
  else
    {
      if (( l3 =nsp_mat_ones(1,l1->n))  == NULLMAT) return RET_BUG;
      l3= Mat2int(l3);
      StackStore(stack,(NspObject *) l3,3);
    }

  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->scale->drawpolylines(Xgc,l1->R,l2->R,(int *)l3->R,l2->n,l2->m);
  return 0;
}

/*-----------------------------------------------------------
 * xselect([winid])
 * raise the current graphics window  or window with id winid 
 * which becomes the new current window. 
 * windows are created if necessary.
 *-----------------------------------------------------------*/

int int_xselect(Stack stack, int rhs, int opt, int lhs)
{
  int win_id;
  BCG *Xgc;
  CheckRhs(0,1);
  if (rhs >= 1) 
    {
      if (GetScalarInt(stack,1,&win_id) == FAIL) return RET_BUG;
      win_id = Max(0,win_id);
      if ((Xgc=window_list_search(win_id)) != NULL) 
	{
	  Xgc->graphic_engine->xset_curwin(win_id,TRUE);
	  Xgc->graphic_engine->xselgraphic(Xgc);
	}
      else 
	{
	  /* create a graphic window */
	  Xgc= set_graphic_window(win_id);
	  Xgc->graphic_engine->xselgraphic(Xgc);
	}
    }
  else 
    {
      /* raise current window with creation if none */
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->xselgraphic(Xgc);
    }
  return 0;


}

/*-----------------------------------------------------------
 * xset(choice-name,x1,x2,x3,x4,x5)
 * or   xset()
 *-----------------------------------------------------------*/

/* XXXXXXXXXXXXXXXXXXXXXX Attention il faut des xset_1 ici */ 


static char *xset_Table[] = { "alufunction", "background", "clipoff",  "clipping",  "color",  "colormap",
			      "dashes",     "default",  "default_colormap",
			      "font",   "font size",    "foreground",  "hidden3d",
			      "lastpattern",  "line mode",   "line style",   "mark",   "mark size", "pattern",
			      "pixmap",   "thickness",  "use color",  "viewport", "wdim",   "white",   "window",
			      "wpdim",   "wpos",  "wresize",  "wshow",  "wwpc", "fpf","auto clear", "clipgrf",
			      NULL
};

int int_xset(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc = NULL;
  static char *auto_clear_values[]= {"off","on",NULL};
  char *info;
  int rep,val,val1, i, mark[2], font[2];
  double cl[4];
  NspMatrix *M;

  if (rhs <= 0) {return sci_demo(stack.fname,"xsetm();",0);}

  CheckRhs(1,6);
  CheckLhs(0,1);

  if ((rep= GetStringInArray(stack,1,xset_Table,1)) == -1) return RET_BUG; 

  switch (rep) 
    {
    case 0: /* {"alufunction",xset_alufunction1,xget_alufunction}, */
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,val);
      break;
    case 1: /*{"background",xset_background,xget_background},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_background(Xgc,val);
      break;
    case 2: /*{"clipoff",xset_unclip,xget_clip},*/
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_unclip(Xgc);
      break;
    case 3: /*{"clipping",xset_clip,xget_clip},*/
      if ( rhs == 2 ) 
	{
	  if (( M = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
	  CheckLength(stack.fname,2,M,4);
	  Xgc=nsp_check_graphic_context();
	  Xgc->graphic_engine->scale->xset_clip(Xgc,M->R);
	}
      else
	{
	  CheckRhs(2,5);
	  for ( i = 0; i < 4 ; i++) 
	    {
	      if (GetScalarDouble(stack,i+2,cl+i) == FAIL) return RET_BUG; 
	    }
	  Xgc=nsp_check_graphic_context();
	  Xgc->graphic_engine->scale->xset_clip(Xgc,cl);
	}
      break;
    case 4: /*{"color",xset_pattern,xget_pattern},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_pattern(Xgc,val);
      break;
    case 5: /*{"colormap",xset_colormap,xget_colormap},*/
      CheckRhs(2,2);
      if ( (M = GetRealMat(stack,2)) == NULLMAT) return RET_BUG; 
      CheckCols(stack.fname,2,M,3);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_colormap(Xgc,M->m,M->R);
      break;
    case 6: /*{"dashes",xset_dash_or_color,xget_dash_or_color},  */
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_dash(Xgc,val);
      break;
    case 7: /*{"default",xset_default*/
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_default(Xgc);
      break;
    case 8: /*{"default_colormap",xset_default_colormap*/
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_default_colormap(Xgc);
      break;
    case 9: /*{"font",xset_font,xget_font},*/
      CheckRhs(2,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if ( rhs == 3 ) 
	{
	  if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
	}
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->xget_font(Xgc,font);
      font[0]=val;
      if ( rhs == 3 ) font[1]=val1;
      Xgc->graphic_engine->scale->xset_font(Xgc,font[0],font[1]);
      break;
    case 10: /*{"font size",xset_font,xget_font},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->xget_font(Xgc,font);
      font[1]=val;
      Xgc->graphic_engine->scale->xset_font(Xgc,font[0],font[1]);
      break;
    case 11: /*{"foreground",xset_foreground,xget_foreground},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_foreground(Xgc,val);
      break;
    case 12: /*{"hidden3d",xset_hidden3d,xget_hidden3d},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_hidden3d(Xgc,val);
      break;
    case 13: /*{"lastpattern",xset_empty,xget_last},*/
      CheckRhs(1,1);
      Scierror("lastpattern cannot be set \n");
      return RET_BUG;
      break;
    case 14: /*{"line mode",xset_absourel,xget_absourel},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_absourel(Xgc,val);
      break;
    case 15: /*{"line style",xset_dash,xget_dash},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_dash(Xgc,val);
      break;
    case 16: /*{"mark",xset_mark,xget_mark},*/
      CheckRhs(2,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if ( rhs == 3 ) 
	{
	  if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
	}
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->xget_mark(Xgc,mark);
      mark[0]=val;
      if ( rhs == 3 ) mark[1]=val1;
      Xgc->graphic_engine->scale->xset_mark(Xgc,mark[0],mark[1]);
      break;
    case 17: /*{"mark size",xset_mark,xget_mark},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->xget_mark(Xgc,mark);
      mark[1]=val;
      Xgc->graphic_engine->scale->xset_mark(Xgc,mark[0],mark[1]);
      break;
    case 18: /*{"pattern",xset_pattern,xget_pattern},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_pattern(Xgc,val);
      break;
    case 19: /*{"pixmap",xset_pixmapOn,xget_pixmapOn},*/
      CheckRhs(2,2);
      Xgc=nsp_check_graphic_context();
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc->graphic_engine->scale->xset_pixmapOn(Xgc,val);
      break;
    case 20: /*{"thickness",xset_thickness,xget_thickness},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_thickness(Xgc,val);
      break;
    case 21: /*{"use color",xset_usecolor,xget_usecolor},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,val);
      break;
    case 22: /*{"viewport",xset_viewport,xget_viewport},*/
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_viewport(Xgc,val,val1);
      break;
    case 23: /*{"wdim",xset_windowdim,xget_windowdim},*/
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_windowdim(Xgc,val,val1);
      break;
    case 24: /*{"white",xset_empty,xget_last},*/
      CheckRhs(1,1);
      Scierror("white cannot be set \n");
      return RET_BUG;
      break;
    case 25: /*{"window",xset_curwin,xget_curwin},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if ((Xgc = window_list_get_first()) != NULL) 
	Xgc->graphic_engine->xset_curwin(Max(val,0),TRUE);
      else 
	Xgc= set_graphic_window(Max(val,0));
      break;
    case 26: /*{"wpdim",xset_popupdim,xget_popupdim},*/
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_popupdim(Xgc,val,val1);
      break;
    case 27: /*{"wpos",xset_windowpos,xget_windowpos},*/
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_windowpos(Xgc,val,val1);
      break;
    case 28: /*{"wresize",xset_wresize,xget_wresize},*/
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_wresize(Xgc,val);
      break;
    case 29: /*{"wshow",xset_show,xget_empty},*/
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_show(Xgc);
      break;
    case 30: /*{"wwpc",xset_pixmapclear,xget_empty}*/
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_pixmapclear(Xgc);
      break;
    case 31: /*{"fpf"}*/
      CheckRhs(2,2);
      if ((info = GetString(stack,2)) == (char*)0) return RET_BUG;
      Xgc=nsp_check_graphic_context();
      if ( strlen(info)== 0) 
	Xgc->graphic_engine->scale->xset_fpf_def(Xgc);
      else 
	Xgc->graphic_engine->scale->xset_fpf(Xgc,info);
      return 0;
      break;
    case 32: /*{"auto clear"}*/
      CheckRhs(2,2);
      Xgc=nsp_check_graphic_context();
      if ( (rep = GetStringInArray(stack,2,auto_clear_values,1)) == -1 ) return RET_BUG;
      Xgc->graphic_engine->scale->xset_autoclear(Xgc,rep);
      return 0;
      break;
    case 33 : /*{"clipgrf"},*/
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
      break;
    }
  return 0;
}


/*-----------------------------------------------------------
 * xstring(x,y,str,[angle,box])
 *-----------------------------------------------------------*/

int int_xstring(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int remove=0;
  NspSMatrix *S;
  double rect[4],wc,x,y,yi,angle=0.0;
  int i,flagx=0;

  CheckRhs(3,5);
  
  if (GetScalarDouble(stack,1,&x) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,2,&y) == FAIL) return RET_BUG;
  yi=y;

  if (( S = GetSMat(stack,3)) == NULLSMAT) return RET_BUG;

  if ( S->mn == 0 ) {  return 0;} 

  if (rhs >= 4) {if (GetScalarDouble(stack,4,&angle) == FAIL) return RET_BUG;};
  if (rhs >= 5) {if (GetScalarInt(stack,5,&flagx) == FAIL) return RET_BUG;}; 

  Xgc=nsp_check_graphic_context();

  /*     to keep the size of the largest line */
  wc = 0.;

  if ( S->n != 1 ) 
    {
      remove=1;
      if (( S =nsp_smatrix_column_concat(S," ",1)) == NULLSMAT) return RET_BUG;
    }

  for (i = S->m -1 ; i >= 0; --i) 
    {
      Xgc->graphic_engine->scale->displaystring(Xgc,S->S[i],x,y,0,angle);
      Xgc->graphic_engine->scale->boundingbox(Xgc,S->S[i],x,y,rect);
      wc = Max(wc,rect[2]);
      if (i != 0 ) 
	y += rect[3] * 1.2;
      else 
	y += rect[3];
    }
  if (flagx == 1) {
    double rect[]={x,y,wc, y - yi};
    Xgc->graphic_engine->scale->drawrectangle(Xgc,rect);
  }
  if ( remove == 1) nsp_smatrix_destroy(S);
  return 0;
} 

/*-----------------------------------------------------------
 * xtitle(tit,x,y)
 *-----------------------------------------------------------*/

int int_xtitle(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspSMatrix *S;
  int narg;

  if ( rhs <= 0) {  return sci_demo(stack.fname,"x=(1:10)';plot2d(x,x);xtitle(['Titre';'Principal'],'x','y');",1);  }

  CheckRhs(1,3);
  Xgc=nsp_check_graphic_context();

  for ( narg = 1 ; narg <= rhs ; narg++) 
    {
      String *str;
      if (( S = GetSMat(stack,narg)) == NULLSMAT) return RET_BUG;
      if ( S->mn == 0 ) continue;
      if (( str =nsp_smatrix_elts_concat(S,"@",1," ",1))== NULL) return RET_BUG;
      Xgc->graphic_engine->scale->displaystringa(Xgc,str,narg);
      FREE(str);
    }
  return 0;
}

/*-----------------------------------------------------------
 * xstringb 
 *-----------------------------------------------------------*/

int int_xstringb(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char * info;
  int fill =0;
  double x,y,w,hx;
  String *str;
  NspSMatrix *S;

  CheckRhs(5,6);
  if (GetScalarDouble(stack,1,&x) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,2,&y) == FAIL) return RET_BUG;
  if ((S = GetSMat(stack,3)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) return 0; 
  if (GetScalarDouble(stack,4,&w) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,5,&hx) == FAIL) return RET_BUG;

  if (rhs == 6) {
    if ((info = GetString(stack,6)) == (char*)0) return RET_BUG;
    if ( strncmp(info,"fill",4) == 0) 
      fill =1;
    else 
      {
	Scierror("%s: optional argument has a wrong value 'fill' expected\r\n",stack.fname);
	return RET_BUG;
      }
  }
  if (( str =nsp_smatrix_elts_concat(S,"\n",1," ",1))== NULL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  
  Xgc->graphic_engine->scale->xstringb(Xgc,str,&fill,&x,&y,&w,&hx);
  FREE(str);
  return 0;
} 


/*-----------------------------------------------------------
 * xstringc 
 * bool= xstringc(rect,str [,opts])
 * opts:  fill=%t (font is chosen as big as possible 
 *        color=text-color 
 *        frame=frame-color ( rect is drawn too)
 *        background=fill-color 
 *-----------------------------------------------------------*/

int int_xstringc(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int cpat,cwidth;
  NspMatrix *M;
  double y;
  int fill=0,frame=-1,color=-1,back=-1,thickness=-1;
  String *str;
  NspSMatrix *S;
  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "fill",s_bool,NULLOBJ,-1},
		      { "frame",s_int,NULLOBJ,-1},
		      { "frame_width",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(2,7);

  if (( M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,1,M,4);
  if ((S = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) return 0; 
  if (( str =nsp_smatrix_elts_concat(S,"\n",1," ",1))== NULL) return RET_BUG;
 
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&fill,&frame,&thickness) == FAIL) return RET_BUG;

  Xgc=nsp_check_graphic_context();

  if ( opt != 0 ) 
    {
      cpat = Xgc->graphic_engine->xget_pattern(Xgc);
      cwidth = Xgc->graphic_engine->xget_thickness(Xgc);
      if ( back != -1 )
	{
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,back);
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,M->R);
	  Xgc->graphic_engine->scale->xset_pattern(Xgc,cpat);
	}
      if ( thickness != -1 ) 
	Xgc->graphic_engine->scale->xset_thickness(Xgc,thickness);
      if ( frame != -1 ) 
	Xgc->graphic_engine->scale->xset_pattern(Xgc,frame);
      else
	Xgc->graphic_engine->scale->xset_pattern(Xgc,cpat);
      Xgc->graphic_engine->scale->drawrectangle(Xgc,M->R);
      if ( thickness != -1 ) 
	Xgc->graphic_engine->scale->xset_thickness(Xgc,cwidth);
      if ( color != -1) 
	Xgc->graphic_engine->scale->xset_pattern(Xgc,color);
      else
	Xgc->graphic_engine->scale->xset_pattern(Xgc,cpat);
      y = M->R[1]-M->R[3];
      Xgc->graphic_engine->scale->xstringb(Xgc,str,&fill,M->R,&y,M->R+2,M->R+3);
      if ( color != -1) 
	Xgc->graphic_engine->scale->xset_pattern(Xgc,cpat);
    }
  else 
    {
      y = M->R[1]-M->R[3];
      Xgc->graphic_engine->scale->xstringb(Xgc,str,&fill,M->R,&y,M->R+2,M->R+3);
    }
  FREE(str);
  return 0;
} 



/*-----------------------------------------------------------
 *  rect=xstringl(x,y,str)
 *-----------------------------------------------------------*/

int int_xstringl(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspSMatrix *S;
  NspMatrix *M;
  double rect[4],wc=0,x,y,yi;
  int i,remove=0;
  CheckRhs(3,3);
  CheckLhs(0,1);
  
  if (GetScalarDouble(stack,1,&x) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,2,&y) == FAIL) return RET_BUG;
  yi=y;
  if ((S = GetSMat(stack,3)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) return 0; 

  if ((M = nsp_matrix_create(NVOID,'r',1,4))== NULLMAT) return RET_BUG;
  NSP_OBJECT(M)->ret_pos = 1;
  StackStore(stack,(NspObject *) M,rhs+1);

  if ( S->n != 1 ) 
    {
      remove=1;
      if (( S =nsp_smatrix_column_concat(S," ",1)) == NULLSMAT) return RET_BUG;
    }

  Xgc=nsp_check_graphic_context();

  for (i = S->m -1 ; i >= 0; --i) 
    {
      Xgc->graphic_engine->scale->boundingbox(Xgc,S->S[i],x,y,rect);
      wc = Max(wc,rect[2]);
      if (i != 0 ) 
	y += rect[3] * 1.2;
      else 
	y += rect[3];
    }
  if ( remove == 1) nsp_smatrix_destroy(S);
  M->R[0]=x;  M->R[1]=y;  M->R[2]=wc;  M->R[3]=y-yi;
  return 1;
}

/*-----------------------------------------------------------
 * xtape: update manual XXX
 *-----------------------------------------------------------*/

int int_xtape(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int rep;
  NspMatrix *M;
  static double  rect_def[4] = { 0,0,10,10}, ebox_def[6] = {0,1,0,1,0,1};
  static int iflag_def[4] = { 0,0,0,0 };
  static int aint_def[4] = { 0,0,0,0 };
  static int iscflag_def[2] = { 1,0 };
  static int flagx_def[3] = { 1,1,1} ;
  int *iflag = iflag_def,*aint = aint_def,*iscflag = iscflag_def, *flagx= flagx_def,num;
  double alpha = 35.0 ,theta = 45.0,  *rect = rect_def ,*ebox = ebox_def ;

  static char *xtape_Table[] = {  "on","clear","replay","replaysc","replayna",  NULL };

  CheckRhs(1,7);
  Xgc=nsp_check_graphic_context();

  /* first argument is a string in xtape_table */

  if ((rep= GetStringInArray(stack,1,xtape_Table,1)) == -1) return RET_BUG; 

  switch (rep) 
    {
    case 0 : /* on */ 
      CheckRhs(1,1);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      break;
    case 1 : /* clear */
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
      Xgc->graphic_engine->tape_clean_plots(Xgc,num);
      break;
    case 2 : /* replay */
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
      Xgc->graphic_engine->tape_replay(Xgc,num);
      break;
    case 3 : /* replaysc */
      CheckRhs(2,5);
      if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
      /*     s'il n'y a que trois argument le 3ieme est rect[4] */
      if (rhs == 3) { 
	if ((M= GetRealMat(stack,3))  == NULLMAT) return RET_BUG;
	CheckLength(stack.fname,3,M,4); rect =  M->R;
      }
      else if ( rhs > 3 ) 
	{
	  if ((M= GetRealMatInt(stack,3))  == NULLMAT) return RET_BUG;
	  CheckLength(stack.fname,3,M,2); iscflag = (int*) M->R;
	  if ( rhs >=4 ) { 
	    if ((M= GetRealMat(stack,4))  == NULLMAT) return RET_BUG;
	    CheckLength(stack.fname,4,M,4); rect =  M->R;
	  }
	  if ( rhs >=5 ) { 
	    if ((M= GetRealMatInt(stack,5))  == NULLMAT) return RET_BUG;
	    CheckLength(stack.fname,5,M,4); aint = (int*) M->R;
	  }
	}
      Xgc->graphic_engine->tape_replay_new_scale(Xgc,num,iscflag,aint,rect);
      break;
    case 4: /* replayna */
      CheckRhs(2,5);
      if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
      if ( rhs >= 3 ) { if (GetScalarDouble(stack,3,&theta) == FAIL) return RET_BUG;}
      if ( rhs >= 4 ) { if (GetScalarDouble(stack,4,&alpha) == FAIL) return RET_BUG;}
      if ( rhs >= 5 ) { 
	if ((M= GetRealMatInt(stack,5))  == NULLMAT) return RET_BUG;
	CheckLength(stack.fname,5,M,4); iflag = (int*) M->R;
      }
      if ( rhs >= 6 ) { 
	if ((M= GetRealMatInt(stack,6))  == NULLMAT) return RET_BUG;
	CheckLength(stack.fname,6,M,3); flagx = (int*) M->R;
      }
      if ( rhs >= 7 ) { 
	if ((M= GetRealMat(stack,6))  == NULLMAT) return RET_BUG;
	CheckLength(stack.fname,7,M,6); ebox =  M->R;
      }
      Xgc->graphic_engine->tape_replay_new_angles(Xgc,num,iflag,flagx,&theta,&alpha,ebox); /*  */
      break;
    }
  return 0;
}

/*-----------------------------------------------------------
 * xinfo(string)
 *-----------------------------------------------------------*/

int int_xinfo(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char *info;
  CheckRhs(1,1);
  if ((info = GetString(stack,1)) == (char*)0) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->xinfo(Xgc,info);
  return 0;
}


/*------------------------------------------------------------
 * xsetech(wrect=[...],frect=[..],logflag="..", arect=[...]) 
 * or 
 * xsetech(wrect,[frect,logflag])
 * or 
 * xsetech()
 *------------------------------------------------------------*/

int int_xsetech(Stack stack, int rhs, int opt, int lhs)
{

  BCG *Xgc;
  double *wrect =NULL,*frect=NULL,*arect=NULL;
  static char logflag_def[]="nn";
  char *logflag = logflag_def;
  NspMatrix *M;
  Xgc=nsp_check_graphic_context();

  if ( opt == 0) 
    {
      /** compatibility with old version **/
      CheckRhs(-1,3);
      CheckLhs(0,1);
      if ( rhs <= 0) { show_scales(Xgc); return 0;	}

      if ((M= GetRealMat(stack,1))  == NULLMAT) return RET_BUG;
      CheckLength(stack.fname,1,M,4);
      wrect = M->R;

      if (rhs >= 2) {
	if ((M= GetRealMat(stack,2))  == NULLMAT) return RET_BUG;
	CheckLength(stack.fname,2,M,4);
	frect = M->R;
      }
      if (rhs >= 3) { 
	if ((logflag = GetString(stack,3)) == (char*)0) return RET_BUG;
	if ( strlen(logflag) != 2 ) {
	  Scierror("%s: third argument has a wrong length %d expecting (%d)\r\n",stack.fname,strlen(logflag),2 );
	  return RET_BUG;
	}
      }
    }
  else 
    {
      NspMatrix *Marect=NULL,*Mwrect=NULL,*Mfrect=NULL;
      int_types T[] = {opts, t_end} ;
      /* 4 named optional arguments */
      /* names of optional arguments: must be NULL terminated*/
      char *Names[]={"arect","frect","logflag","wrect",NULL};
      /* types of optional arguments */
      int_types Topt[]={ realmat,realmat,string,realmat, t_end} ;
      /* table to store optional arguments */ 
      NspObject *Tab[4]; 
      /* table to store optional arguments position */ 
      int posi[4];
      /* structure for optional arguments */
      named_opts N = { 4, Names, Topt,Tab, posi};
      /* N.n =  4 ; N.names= Names, N.types = Topt, N.objs = Tab; */
      if ( GetArgs(stack,rhs,opt,T,&N,&Marect,&Mfrect,&logflag,&Mwrect) == FAIL) return RET_BUG;

      if ( Marect != NULL) {
	arect = Marect->R;CheckLength(stack.fname,posi[0],Marect,4);
      }

      if ( Mfrect != NULL) {
	frect = Mfrect->R;CheckLength(stack.fname,posi[1],Mfrect,4);
      }

      if ( Mwrect != NULL) {
	wrect = Mwrect->R;CheckLength(stack.fname,posi[3],Mwrect,4);
      }
      
      if ( logflag != logflag_def ) {
	if ( strlen(logflag) != 2 ) {
	  Scierror("%s: logflag argument has a wrong length %d expecting (%d)\r\n",stack.fname,strlen(logflag),2 );
	  return RET_BUG;
	} 
      }
    }

  Nsetscale2d(Xgc,wrect,arect,frect,logflag);
  return 0;
}

/*-----------------------------------------------------------
 * [wrect,frect,logflag,arect]=xgetech()
 *-----------------------------------------------------------*/

int int_xgetech(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int i;
  double WRect[4],FRect[4],ARect[4];
  double *W= WRect, *F = FRect, *A= ARect ;
  char logf[2], *L=logf;
  NspObject *l[5];

  CheckRhs(0,0);
  CheckLhs(1,4);

  if ( lhs >=1 ) { if ((l[1]=(NspObject *) nsp_matrix_create(NVOID,'r',1,4))==NULLOBJ) return RET_BUG;   W= ((NspMatrix *) l[1])->R;}
  if ( lhs >=2 ) { if ((l[2]=(NspObject *) nsp_matrix_create(NVOID,'r',1,4))==NULLOBJ) return RET_BUG; F=((NspMatrix *) l[2])->R;}
  if ( lhs >=3 ) 
    { 
      if ((l[3]=(NspObject *)nsp_smatrix_create_with_length(NVOID,1,1,2))== NULLOBJ) return RET_BUG; 
      L=((NspSMatrix *) l[3])->S[0];
      L[2]='\0';
    }

  if ( lhs >=4 ) { if ((l[4]=(NspObject *) nsp_matrix_create(NVOID,'r',1,4))==NULLOBJ) return RET_BUG; A= ((NspMatrix *) l[4])->R;}
  Xgc=nsp_check_graphic_context();
  getscale2d(Xgc,W,F,L,A);
  for ( i = 1 ; i <= lhs ; i++) 
    {
      NSP_OBJECT(l[i])->ret_pos = i;
      StackStore(stack,(NspObject *) l[i],rhs+i);
    }
  return Max(lhs,0);
} 

/*-----------------------------------------------------------
 * fec(x,y,triangles,func,[strf,leg,rect,nax,zminmax,colminmax]);
 * modified version by Bruno 1/2/2001
 *-----------------------------------------------------------*/

int int_fec(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *x,*y,*Tr,*F,*Mrect=NULL,*Mnax=NULL,*Mzminmax=NULL,*Mcolminmax=NULL;
  double *rect,*zminmax;
  int *colminmax,*nax;
  char *strf=NULL,*leg=NULL;
  int_types T[] = {realmat,realmat,realmat,realmat,opts, t_end} ;
  /* 3 named optional arguments */
  /* names of optional arguments: must be NULL terminated*/
  char *Names[]={"leg","nax","rect","strf","zminmax","colminmax",NULL};
  /* types of optional arguments */
  int_types Topt[]={ string,realmat,realmat,string,realmat,mat_int, t_end} ;
  /* table to store optional arguments */ 
  NspObject *Tab[6]; 
  /* table to store optional arguments position */ 
  int posi[6];
  /* structure for optional arguments */
  named_opts N = { 6, Names, Topt,Tab, posi};
  /* N.n =  4 ; N.names= Names, N.types = Topt, N.objs = Tab; */
  if ( GetArgs(stack,rhs,opt,T,&x,&y,&Tr,&F,&N,&leg,&Mnax,&Mrect,&strf,&Mzminmax,&Mcolminmax) == FAIL) return RET_BUG;

  CheckSameDims(stack.fname,1,2,x,y);
  CheckSameDims(stack.fname,1,4,x,F);

  if ( rhs <= 0) { return sci_demo (stack.fname," exec(\"SCI/demos/fec/fec.ex1\");",1);}

  if ( Tr->n != 5) {
    Scierror("%s: triangles have %d columns,expecting 5\r\n",stack.fname,Tr->n);
    return RET_BUG;
  }

  if ( x->mn == 0 || Tr->m == 0) { return 0;} 

  if (( strf = check_strf(stack,stack.fname,"strf",strf))==NULL) return RET_BUG;
  if (( leg = check_legend(stack,stack.fname,"leg",leg))==NULL) return RET_BUG;
  if (( rect = check_rect(stack,stack.fname,"rect",Mrect))==NULL) return RET_BUG;
  if (( nax = check_nax(stack,stack.fname,"nax",Mnax))==NULL) return RET_BUG;

  if (( zminmax = check_zminmax(stack,stack.fname,"zminmax",Mzminmax))== NULL) return RET_BUG;
  if (( colminmax = check_colminmax(stack,stack.fname,"colminmax",Mcolminmax))== NULL) return RET_BUG;

  if ( strf == def_strf) {
    if ( rect != def_rect) strf[1] = '7';
    if ( leg != def_legend) strf[0] = '1';
    if ( nax != def_nax) strf[1] = '1';
  }

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  nsp_fec(Xgc,x->R,y->R,Tr->R,F->R,&x->mn,&Tr->m,strf,leg,rect,nax,zminmax,colminmax);
  return 0;
}

/*--------------------------------------------------------------
 * rep = xgetmouse(clearq=bool,getmotion=bool,getrelease=bool)
 *--------------------------------------------------------------*/

int int_xgetmouse(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *M;
  int clearq=FALSE,motion=TRUE,release=FALSE,key=FALSE, iflag;
  int button;
  double x,y;

  nsp_option opts[] ={
    { "clearq",s_bool,NULLOBJ,-1},
    { "getkey",s_bool,NULLOBJ,-1},
    { "getmotion",s_bool,NULLOBJ,-1},
    { "getrelease",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};
  
  int_types T[] = {new_opts, t_end} ;
  
  if ( GetArgs(stack,rhs,opt,T,&opts,&clearq,&key,&motion,&release) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  iflag = (clearq == TRUE) ? FALSE : TRUE;
  Xgc->graphic_engine->scale->xgetmouse(Xgc,"xv",&button,&x,&y,iflag,motion,release,key);
  if ((M = nsp_matrix_create(NVOID,'r',1,3))== NULLMAT) return RET_BUG;
  M->R[0] = x;  M->R[1] = y;  M->R[2] = (double) button;
  NSP_OBJECT(M)->ret_pos=1;
  StackStore(stack,(NspObject *) M,rhs+1);
  return 1;
} 

/*-----------------------------------------------------------
 * xsave('fname' [, wid]) 
 *-----------------------------------------------------------*/

int int_xsave(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int wid;
  char *str;
  CheckRhs(1,2);

  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if (rhs == 2) 
    { 
      if (GetScalarInt(stack,2,&wid) == FAIL) return RET_BUG;
      if (( Xgc=window_list_search(wid)) == NULL) return 0;
    }
  else 
    {
      Xgc = check_graphic_window();
      wid = Xgc->CurWindow;
    }
  tape_save(Xgc,str,wid);
  return 0;
}

/*-----------------------------------------------------------
 * xload('fname' [, wid]) 
 *-----------------------------------------------------------*/

int int_xload(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int wid;
  char *str;
  CheckRhs(1,2);

  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if (rhs == 2) 
    { 
      if (GetScalarInt(stack,2,&wid) == FAIL) return RET_BUG;
      Xgc = set_graphic_window(Max(0,wid));
    }
  Xgc=check_graphic_window();
  tape_load(Xgc,str);
  return 0;
}

/*-----------------------------------------------------------
 * xdel([win-ids]) 
 *-----------------------------------------------------------*/

int int_xdel(Stack stack, int rhs, int opt, int lhs)
{

  NspMatrix *l1;
  CheckRhs(0,1) ;
  
  if (rhs == 1) 
    {
      int i;
      if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      for (i = 0 ; i < l1->mn ; ++i) 
	{
	  scig_delete((int) l1->R[i]);
	}
    } 
  else 
    {
      BCG *loc =  window_list_get_first();
      if ( loc != NULL) scig_delete(loc->CurWindow);
    }
  return 0;
}

/*-----------------------------------------------------------
 * used to print or export a graphic window 
 *-----------------------------------------------------------*/

static int int_export_G(Stack stack, int rhs, int opt, int lhs,char * export_format)
{
  char *filename;
  int color = -1,iwin;
  CheckRhs(2,3);
  if (GetScalarInt(stack,1,&iwin) == FAIL) return RET_BUG;
  if ((filename = GetString(stack,2)) == (char*)0) return RET_BUG;
  /* color or n & b */ 
  if ( rhs >= 3) {   if (GetScalarInt(stack,3,&color) == FAIL) return RET_BUG;}
  scig_export(filename,iwin,color,export_format);
  return 0;
}

int int_xs2ps(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"Pos");
}

/* backward compatibility */

int int_xg2ps(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"Pos");
}

int int_xs2fig(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"Fig");
}

int int_xs2gif(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"GIF");
}

int int_xs2ppm(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"PPM");
}

/*-----------------------------------------------------------
 * [s,v]= gsort(x,[str1,str2]) 
 *       str1 = 'g','r','c','lc','lr',
 *       str2 = 'i' | 'd' 
 *-----------------------------------------------------------*/

/* Attention il faut copier les arguments **/ 

int int_gsort(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S=NULL;
  NspMatrix *M=NULL,*rep2=NULL;
  int iflag = 0;
  char iord[2] ; /* = { 'd','\0' }; */
  char typex[10]; /* = { 'g' ,'\0'} ; */
  double dv;
  int iv,v;
  iord[0] = 'd'; iord[1]='\0';
  typex[0] = 'g'; typex[1] = '\0';

  CheckRhs(1,3);

  if ( IsMatObj(stack,1)) 
    {
      if ((M=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
    }
  else if ( IsSMatObj(stack,1)) 
    {
      if (( S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
    }
  else 
    {
      Scierror("%s: first argument has a wrong type, expecting scalar or string matrix\r\n",stack.fname);
      return RET_BUG;
    }
  
  if ( rhs >= 2) 
    {
      char *str,c;
      if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;
      if ( strlen(str) == 0 ) {
	Scierror("%s: second argument is an empty string\n",stack.fname);
	return RET_BUG;
      }
      c = str[0];
      if (c != 'r' && c != 'c' && c != 'g' && c != 'l') {
	Scierror("%s: second argument has a wrong value %s should be in r,c,g,lr,lc\r\n",stack.fname,str);
	return RET_BUG;
      }
      strcpy(typex,str);
    }

  if (rhs >= 3) 
    {
      char *str;
      if ((str = GetString(stack,3)) == (char*)0) return RET_BUG;
      if ( str[0] != 'i' && str[0] != 'd') 
	{
	  Scierror("%s: third argument must be \"i\" or \"d\"\r\n",stack.fname);
	  return RET_BUG;
	}
      iord[0] = str[0];
    }

  if ( M != NULL) 
    {
      /** Scalar matrix **/
      if (lhs  == 2) 
	{
	  iflag = 1;
	  if ( typex[0] == 'l') 
	    {
	      if (typex[1] == 'r') {
		if ((rep2=nsp_matrix_create(NVOID,'r',M->m,1))==NULLMAT) return RET_BUG;
	      } else  {
		if ((rep2=nsp_matrix_create(NVOID,'r',1,M->n))==NULLMAT) return RET_BUG;
	      }
	      rep2->convert = 'i';
	      M =  Mat2int(M);
	      C2F(gsort)((int *)M->R,&dv,(int *)rep2->R,&iflag,&M->m,&M->n,typex,iord);
	    }
	  else 
	    {
	      if ((rep2=nsp_matrix_create(NVOID,'r',M->m,M->n))==NULLMAT) return RET_BUG;
	      rep2->convert = 'i';
	      C2F(gsort)(&iv,M->R,(int *) rep2->R,&iflag,&M->m,&M->n,typex,iord);
	    }
	}
      else 
	{
	  /* lhs == 1 */ 
	  iflag = 0;
	  if ( typex[0] == 'l') 
	    {
	      M =  Mat2int(M);
	      C2F(gsort)((int *)M->R,&dv,&iv,&iflag,&M->m,&M->n,typex,iord);
	    } 
	  else 
	    {
	      C2F(gsort)(&iv,M->R,&iv,&iflag,&M->m,&M->n,typex,iord );
	    }
	}
      NSP_OBJECT(M)->ret_pos = 1;
      if ( lhs == 2 ) {
	NSP_OBJECT(rep2)->ret_pos = 2;
	StackStore(stack,(NspObject *) rep2,rhs+1);
      }
    }
  else 
    {
      /** String matrix **/
      if ( lhs == 2) {
	iflag = 1;
	if ( typex[0] == 'l') 
	  {
	    if (typex[1] == 'r') {
	      if ((rep2=nsp_matrix_create(NVOID,'r',S->m,1))==NULLMAT) return RET_BUG;
	    } else  {
	      if ((rep2=nsp_matrix_create(NVOID,'r',1,S->n))==NULLMAT) return RET_BUG;
	    }
	    rep2->convert = 'i';
	    C2F(gsorts)(S->S,(int *)rep2->R,&iflag,&S->m,&S->n,typex,iord);
	  } 
	else 
	  {
	    if ((rep2=nsp_matrix_create(NVOID,'r',S->m,S->n))==NULLMAT) return RET_BUG;
	    rep2->convert = 'i';
	    C2F(gsorts)(S->S,(int *)rep2->R,&iflag,&S->m,&S->n,typex,iord);
	  }
      } else {
	iflag = 0;
	C2F(gsorts)(S->S,&v,&iflag,&S->m,&S->n,typex,iord);
      }
      NSP_OBJECT(S)->ret_pos = 1;
      if ( lhs == 2 ) {
	NSP_OBJECT(rep2)->ret_pos = 2;
	StackStore(stack,(NspObject *) rep2,rhs+1);
      }
    }
  return lhs;
} 


/*-----------------------------------------------------------
 *   x=winsid()
 *-----------------------------------------------------------*/

int int_winsid(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc =  window_list_get_first();
  NspMatrix *M;
  int ids,num;
  CheckRhs(-1,0) ;
  /* first pass to get num */
  if ( Xgc == NULL ) 
    {
      if ((M=nsp_matrix_create(NVOID,'r',0,0))==NULLMAT) return RET_BUG;
    }
  else
    {
      Xgc->graphic_engine->window_list_get_ids(&num,&ids ,0);
      if ((M=nsp_matrix_create(NVOID,'r',1,num))==NULLMAT) return RET_BUG;
      /* second pass to fill M */
      Xgc->graphic_engine->window_list_get_ids(&num,(int *) M->R,1);
    }
  NSP_OBJECT(M)->ret_pos = 1; M->convert='i';
  StackStore(stack,(NspObject *)M,1);
  return 1;
}

/*-----------------------------------------------------------
 * [xi,xa,np1,np2,kMinr,kMaxr,ar]=xgraduate(xmi,xma)
 * rajouter ds le man XXXX 
 *-----------------------------------------------------------*/

int int_xgraduate(Stack stack, int rhs, int opt, int lhs)
{
  double l1,l2,val[7];
  int i, ival[7]; /* &xi,&xa,&np1,&np2,&kMinr,&kMaxr,&ar */

  CheckRhs(2,2);
  CheckLhs(2,7);

  if (GetScalarDouble(stack,1,&l1) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,2,&l2) == FAIL) return RET_BUG;
  
  graduate(&l1,&l2,val,val+1,ival+2,ival+3,ival+4,ival+5,ival+6);
  for ( i= 2 ; i < 7 ; i++) val[i]=ival[i];

  for ( i= 1 ; i <= lhs ; i++) 
    {
      NspObject *O;
      if (( O =nsp_create_object_from_double(NVOID,val[i-1])) == NULLOBJ ) return RET_BUG;
      NSP_OBJECT(O)->ret_pos=i;
      StackStore(stack,O,rhs+i);
    }
  return lhs;
}

/*-----------------------------------------------------------
 * xname('name') : give a name to current graphic window 
 *-----------------------------------------------------------*/

int int_xname(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char *str;
  CheckRhs(1,1);
  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->setpopupname(Xgc,str);
  return 0;
}

/*------------------------------------------------------------
 * dir = 'u','r','d','l'  [default -> 'l' ] 
 * fontsize =             [default -> -1   ] 
 * format_n = format to use for numbers (unused if strings) 
 * seg = flag 1 or 0 draw the base segment of the axis (default 1)
 * sub_int  = number of sub tics (default 2) 
 * textcolor =            [default -> -1 ]
 * ticscolor =            [default -> -1 ]
 * tics = 'v' 'r' 'i'     [default -> 'v' ] 
 *        gives tics type : vector | range | irange (v,r,i) 
 * val  = string matrix 
 * x = scalar | vecteur | range | irange 
 * y = scalar | vecteur | range | irange 
 * 
 * constraints : 
 * ------------
 *   dir = 'u' | 'd' ==> y= scalar | [] 
 *                       x= vecteur | range | irange 
 *   dir = 'r' | 'l' ==> x= scalar | [] 
 *                       y= vecteur | range | irange 
 *   tics = 'r'          ==> x or y is of size 3 (according to dir)
 *   tics = 'i'          ==> x or y is of size 4
 *   val  =              ==> must be of size compatible with x or y 
 *                       according to dir 
 *-------------------------------------------------------------*/


static int check_xy(char *fname,char dir,int mn,int xpos,NspMatrix *Mx,int ypos,NspMatrix *My,int *ntics);

int int_nxaxis(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char dir = 'l', *format = NULL, tics = 'v', **val = NULL;
  int fontsize = -1, sub_int=2, seg_flag = 1,textcolor = -1,ticscolor=-1;
  double *x = NULL,*y = NULL;
  int nx=0,ny=0,ntics;
  NspSMatrix *S;
  NspMatrix *Mx=NULLMAT,*My= NULLMAT;
  char *sdir=NULL,*stics=NULL;
  int_types T[] = {realmat,realmat,realmat,realmat,opts, t_end} ;
  /* names of optional arguments: must be NULL terminated*/
  char *Names[]={"dir","fontsize","format_n","seg","sub_int","textcolor","tics","ticscolor","val","x","y",NULL};
  /* types of optional arguments */
  int_types Topt[]={string,s_int,string,s_int,s_int,s_int,string,s_int,smat,realmat,realmat,
		t_end} ;
  /* table to store optional arguments */ 
  NspObject *Tab[11]; 
  /* table to store optional arguments position */ 
  int posi[11];
  /* structure for optional arguments */
  named_opts N = { 11, Names, Topt,Tab, posi};
  /* N.n =  4 ; N.names= Names, N.types = Topt, N.objs = Tab; */
  if ( GetArgs(stack,rhs,opt,T,&N,&sdir,&fontsize,&format,&seg_flag,
	       &sub_int,&textcolor,&stics,&ticscolor,&S,&Mx,&My) == FAIL) return RET_BUG;

  Xgc=nsp_check_graphic_context();

  if ( sdir != NULL  ) 
    { 
      if ( strlen(sdir) != 1 ) {
	Scierror("%s: optional string dir must contain one character\n",stack.fname);
	return RET_BUG;
      }
      dir = sdir[0];
    } 

  if ( stics != NULL ) 
    { 
      if ( strlen(stics) != 1 ) {
	Scierror("%s: optional string tics must contain one character\n",stack.fname);
	return RET_BUG;
      }
      tics = stics[0];
    } 

  if ( Mx != NULLMAT )
    { 
      x = Mx->R;
      nx = Mx->mn;
    }
  else 
    {
      static double x_def[1];
      nx = 1;
      x = x_def ;
      if ( dir == 'l' ) 
	x_def[0] = Xgc->scales->frect[0];
      else if ( dir == 'r' ) 
	x_def[0] = Xgc->scales->frect[2];
    }

  if ( My != NULLMAT )
    { 
      y = My->R;
      ny = My->mn;
    }
  else 
    {
      static double y_def[1];
      ny = 1;
      y = y_def ;
      if ( dir == 'd' ) 
	y_def[0] = Xgc->scales->frect[1];
      else if ( dir == 'u' ) 
	y_def[0] = Xgc->scales->frect[3];
    }

  /* compatibility test */
  switch (tics ) 
    {
    case 'r' :
      if ( check_xy(stack.fname,dir,3,posi[9],Mx, posi[10],My,&ntics)==0) 
	return 0;
      break;
    case 'i' :
      if ( check_xy(stack.fname,dir,4,posi[9],Mx, posi[10],My,&ntics)==0) 
	return 0;
      break;
    case 'v' :
      if ( check_xy(stack.fname,dir,-1,posi[9],Mx, posi[10],My,&ntics)==0) 
	return 0;
      break;
    default :
      Scierror("%s: tics has a wrong value \"%c\" should be one of \"r\",\"v\" and \"i\" \r\n", 
	       stack.fname,dir);
      return RET_BUG;
    }

  if ( val != 0) 
    {
      /** sciprint("nombre de tics %d\r\n",ntics); **/
      CheckLength(stack.fname, posi[8], S,ntics);
    }

  sci_axis(Xgc,dir,tics,x,&nx,y,&ny,val,sub_int,format,fontsize,textcolor,ticscolor,'n',seg_flag);
  return 0;
}

static int check_xy(char *fname,char dir,int mn,int xpos,NspMatrix *Mx,int ypos,NspMatrix *My,int *ntics)
{
  switch ( dir ) 
    {
    case 'l': case 'r' : 
      /* x must be scalar */
      if ( xpos != -1 ) CheckScalar(fname,xpos,Mx);
      /* y must be of size mn */
      if ( mn != -1 ) CheckLength(fname,ypos,My,mn);
      switch (mn) 
	{
	case 3: 
	  *ntics =  My->R[2]+1;break;
	case 4: 
	  *ntics =  My->R[3]+1;break;
	case -1: 
	  *ntics =  My->mn;break;
	}
      break;
    case 'u' : case 'd' : 
      /* y must be scalar */
      if ( ypos  != -1 ) CheckScalar(fname,ypos,My);
      /* x must be of size mn */
      if (mn != -1 ) CheckLength(fname,xpos,Mx,mn);
      switch (mn) 
	{
	case 3: 
	  *ntics =  Mx->R[2]+1;break;
	case 4: 
	  *ntics =  Mx->R[3]+1;break;
	case -1: 
	  *ntics =  Mx->mn;break;
	}
      break;
    default :
      Scierror("%s: dir has a wrong value \"%c\" should be one of \"u\",\"d\",\"r\" and \"l\"\r\n", 
	       fname,dir);
      return RET_BUG;
    }
  return 1;
}

/*---------------------------------------------------
 * interface for calling the helpbrowser 
 * when scilab is compiled with gtk 
 * not the perfect place to insert this interface XXX ...
 *---------------------------------------------------*/

extern void Sci_Help(char *,char *,char *);

int int_gtkhelp(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *str[3];
  CheckRhs(1,1);
  for (i=0; i < 1 ; i++) {
    if ((str[i] = GetString(stack,i+1)) == (char*)0) return RET_BUG;
  }
  Sci_Help(NULL,NULL,str[0]);
  return 0;
}

/*-----------------------------------------------------------
 * utilities 
 *-----------------------------------------------------------*/

int int_seteventhandler(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char *info;
  int ierr=0,win;
  CheckRhs(1,1);
  CheckLhs(0,1);

  Xgc=nsp_check_graphic_context();
  win = Xgc->graphic_engine->xget_curwin();
  if ( win != -1 ) 
    {
      if (rhs == 1) 
	{
	  if ((info = GetString(stack,1)) == (char*)0) return RET_BUG;
	  nsp_set_graphic_eventhandler(&win,info,&ierr);
	}
      else
	{
	  nsp_set_graphic_eventhandler(&win,"",&ierr);
	}
      return 0;
    }
  return 0;
} 


/*-----------------------------------------------------------
 * moving the camera 
 *-----------------------------------------------------------*/

extern void change_camera(BCG *Xgc,const double *,const double *);


int int_camera(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *c_pos,*cible_pos;

  CheckRhs(2,2);
  CheckLhs(0,0);

  if ((c_pos = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((cible_pos = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;

  CheckVector(stack.fname,1,c_pos);
  CheckVector(stack.fname,2,cible_pos);
  CheckLength(stack.fname,1,c_pos,3);
  CheckLength(stack.fname,2,cible_pos,3);
  Xgc=nsp_check_graphic_context();
  change_camera(Xgc,c_pos->R,cible_pos->R);

  return 0;
}


/*-----------------------------------------------------------
 * Utility function for demo 
 * XXXX
 *-----------------------------------------------------------*/

static int sci_demo (char *fname,char *code,int flag) 
{
  int rep;
  if ( flag == 1) 
    {
      Sciprintf("Demo of %s\r\n",fname);
      Sciprintf("%s\r\n",code);
    }
  rep = ParseEvalFromStr(code,1);
  switch (rep)
    {
    case RET_BUG :
      Scierror("Error:\tBug detected during evaluation of string in %s\n",fname);
      return rep;
    case RET_CTRLC :
      Scierror("Error:\tExecution of string int %s interupted\n",fname);
      return rep;
    }
  return 0;
}

/*************************************************************
 * The Interface for basic matrices operation 
 *************************************************************/

static OpTab Graphics_func[]={
  {"champ",int_champ},
  {"contour",int_contour},
  {"param3d",int_param3d},
  {"plot3d",int_plot3d},
  {"plot3d1",int_plot3d1},
  {"plot2d",int_plot2d},
  {"plot2d1",int_plot2d1_1},
  {"plot2d2",int_plot2d1_2},
  {"plot2d3",int_plot2d1_3},
  {"plot2d4",int_plot2d1_4},
  {"grayplot",int_grayplot},
  {"driver",int_driver},
  {"xfarc",int_xfarc},
  {"xarc",int_xarc},
  {"xarcs",int_xarcs},
  {"xrects",int_xrects},
  {"xarrows",int_xarrows},
  {"xsegs",int_xsegs},
  {"drawaxis",int_nxaxis},
  {"xchange",int_xchange},
  {"xclea",int_xclea},
  {"xrect",int_xrect},
  {"xfrect",int_xfrect},
  {"xclear",int_xclear},
  {"xclick",int_xclick},
  {"xend",int_xend},
  {"xfpoly",int_xfpoly},
  {"xfpolys",int_xfpolys},
  {"xget",int_xget},
  {"xinit",int_xinit},
  {"xlfont",int_xlfont},
  {"xnumb",int_xnumb},
  {"xpause",int_xpause},
  {"xpoly",int_xpoly},
  {"xpoly_clip",int_xpoly_clip},
  {"xpolys",int_xpolys},
  {"xselect",int_xselect},
  {"xset",int_xset},
  {"xstring",int_xstring},
  {"xstringl",int_xstringl},
  {"xtape",int_xtape},
  {"xsetech",int_xsetech},
  {"xgetech",int_xgetech},
  {"geom3d",int_geom3d},
  {"scifec",int_fec},
  {"xgetmouse",int_xgetmouse},
  {"xinfo",int_xinfo},
  {"xtitle",int_xtitle},
  {"xgrid",int_xgrid},
  {"xfarcs",int_xfarcs},
  {"xsave",int_xsave},
  {"xload",int_xload},
  {"champ1",int_champ1},
  {"xdel",int_xdel},
  {"contour2d",int_contour2d},
  {"xg2ps",int_xg2ps},
  {"xs2fig",int_xs2fig},
  {"gsort",int_gsort},
  {"winsid",int_winsid},
  {"param3d1",int_param3d},
  {"xstringb",int_xstringb},
  {"xstringc",int_xstringc},
  {"Matplot",int_matplot},
  {"contour2di",int_contour2d1},
  {"c2dex",int_c2dex},
  {"Matplot1",int_gray2plot}, 
  {"xgraduate",int_xgraduate},
  {"xname",int_xname},
  {"xaxis",int_xaxis},
  {"seteventhandler",int_seteventhandler},
  {"help_gtk",int_gtkhelp},
  {"xs2gif",int_xs2gif},
  {"xs2ppm",int_xs2ppm},
  {"xs2ps",int_xs2ps},
  {"camera",int_camera},
  {(char *) 0, NULL}
};

int Graphics_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Graphics_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Graphics_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Graphics_func[i].name;
  *f = Graphics_func[i].fonc;
}



