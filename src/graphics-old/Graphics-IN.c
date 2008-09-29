/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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
 * interface for nsp 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h> 

#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/parse.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/gsort-p.h"
#include "nsp/gtk/gobject.h" /* FIXME: nsp_gtk_eval_function */
#include "Plo3dObj.h"

/* #define NEW_GRAPHICS  */

#ifdef NEW_GRAPHICS 
#include <gtk/gtk.h>
#include <nsp/figure.h> 
#include <nsp/axes.h> 
#include <nsp/curve.h> 
#include <nsp/polyline.h> 
#include <nsp/vfield.h> 
#include <nsp/grarc.h> 
#include <nsp/grrect.h> 
#include <nsp/arrows.h> 
#include <nsp/segments.h> 

extern void nsp_list_link_figure(NspList *L, NspFigure *F);
extern NspAxes * nsp_check_for_axes(BCG *Xgc) ;
#endif 

/* XXX */
extern NspSMatrix *GetSMatUtf8(Stack stack,int pos); 
extern char *nsp_get_extension(char *name);
extern BCG *nsp_check_graphic_context(void);

static int sci_demo (const char *fname,char *code,int flag) ;
static void  nsp_gwin_clear(BCG *Xgc);
static int plot3d_build_z(Stack stack,NspMatrix *x,NspMatrix *y,NspMatrix *z,NspObject *f, NspObject *fargs);

/**
 * check_style:
 * @stack: calling stack
 * @fname: caller name 
 * @varname: name of variable 
 * @var: value of the variable or NULL
 * @size: requested size 
 * 
 * returns a #NspMatrix containing style values, if the returned 
 * value is different from @var then a new matrix was allocated 
 * and should be freed. 
 * 
 * 
 * Returns: %NULL or a new #NspMatrix 
 **/

static NspMatrix * check_style(Stack stack,const char *fname,char *varname,NspMatrix *var,int size) 
{
  NspMatrix *loc_var = var;
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
      loc_var = var;
      ival = var->I;
      if ( size == 1 && var->mn != 2) 
	{
	  if ((loc_var = nsp_matrix_create(NVOID,'r',1,2))== NULLMAT) return NULL;
	  ival = loc_var->I;
	  ival[0]=  var->I[0];
	  ival[1]=1;
	}
    }
  return loc_var;
}

/*-----------------------------------------------------------
 * Check optional iflag argument 
 * 3D options 
 * default mode is 8 which is a superpose mode 
 *-----------------------------------------------------------*/

static const int iflag_def[]={2,8,4};
static int iflag_loc[] = {2,8,4};

static int * check_iflag(Stack stack,const char *fname,char *varname,NspMatrix *var,int size) 
{
  int i;
  /* provide a default value by copying since the returned array is 
   * changed 
   */
  if ( var == NULLMAT) 
    {
      for ( i= 0 ; i < size ; i++) iflag_loc[i]=iflag_def[i];
    }
  else
    {
      /* check size */ 
      if ( var->mn < size ) 
	{
	  Scierror("%s:optional argument %s is too small (%d<%d)\n",fname,varname,var->mn,size);
	  return NULL;
	}
      for ( i= 0 ; i < size ; i++) iflag_loc[i]=(int) var->R[i];
    }
  return iflag_loc;
}

/*-----------------------------------------------------------
 * Check optional iflag argument for param3d 
 *-----------------------------------------------------------*/

static const int param_iflag_def[]={8,4};
static int param_iflag_loc[] = {8,4};

static int * check_param_iflag(Stack stack,const char *fname,char *varname,NspMatrix *var,int size) 
{
  int i;
  /* provide a default value by copying since the returned array is 
   * changed 
   */
  if ( var == NULLMAT) 
    {
      for ( i= 0 ; i < size ; i++) param_iflag_loc[i]=param_iflag_def[i];
    }
  else
    {
      /* check size */ 
      if ( var->mn < size ) 
	{
	  Scierror("%s:optional argument %s is too small (%d<%d)\n",fname,varname,var->mn,size);
	  return NULL;
	}
      for ( i= 0 ; i < size ; i++) param_iflag_loc[i]=(int) var->R[i];
    }
  return param_iflag_loc;
}

/*-----------------------------------------------------------
 * Check optional argument ebox for 3d plot 
 *-----------------------------------------------------------*/

static const double  ebox_def[]= { 0,1,0,1,0,1};
static double ebox_loc[]=  { 0,1,0,1,0,1};

static double * check_ebox(Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  int i;
  if ( var == NULLMAT) 
    {
      for ( i= 0 ; i < 6 ; i++) ebox_loc[i]=ebox_def[i];
    }
  else 
    {
      /* check size */ 
      if ( var->mn != 6 ) 
	{
	  Scierror("%s:optional argument %s should be of size 6\n",fname,varname);
	  return NULL;
	}
      else 
	{
	  for ( i= 0 ; i < 6 ; i++) ebox_loc[i]= var->R[i];
	}
    }
  return ebox_loc;
}



/*-----------------------------------------------------------
 * Check optional argument rect
 *-----------------------------------------------------------*/

static const double  rect_def[]= {0.,0.,10.0,10.0}; 
static double rect_loc[]=  {0.,0.,10.0,10.0}; 

static double * check_rect(Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  int i;
  if ( var == NULLMAT) 
    {
      for ( i= 0 ; i < 4 ; i++) rect_loc[i]= rect_def[i];
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
	  for ( i= 0 ; i < 4 ; i++) rect_loc[i]= var->R[i];
	}
    }
  return rect_loc;
}

/*-----------------------------------------------------------
 * Check optional argument strf
 *-----------------------------------------------------------*/

#define DEFSTRF "081" 
static char strf_loc[] = DEFSTRF;

static char * check_strf(Stack stack,const char *fname,char *varname,char *strf)
{

  if ( strf == NULL ) 
    {
      strcpy(strf_loc,DEFSTRF);
    }
  else 
    {
      if ( strlen(strf) != 3) 
	{
	  Scierror("%s: optional argument strf has wrong size (%d), 3 expected\n",fname,strlen(strf)); 
	  return NULL;
	}
      else 
	{
	  strcpy(strf_loc,strf);
	}
    }
  return strf_loc;
}

/*-----------------------------------------------------------
 * Check optional argument legend 
 * FIXME should return a const char *
 *-----------------------------------------------------------*/

static char legend_loc[]  = "";

static char * check_legend(Stack stack,const char *fname,char *varname, char *legend)
{
  return ( legend == NULL ) ? legend_loc: legend; 
}

static const char legend_3d_loc[]  = "X@Y@Z";

static const char * check_legend_3d(Stack stack,const char *fname,char *varname,const char *legend)
{
  return ( legend == NULL ) ? legend_3d_loc: legend; 
}

/*-----------------------------------------------------------
 * Check optional argument legend_pos 
 *-----------------------------------------------------------*/
/* 
 * take care to keep the same order in enum legend_pos and in lpos_table;
 * typedef enum { legend_dl, legend_dr ,legend_drm, legend_ur,legend_ur,legend_urm } legends_pos;
 */


static int check_legend_pos(Stack stack,const char *fname,const char *varnam,const char *l_pos)
{
  static char *Table[] = {"dl",  "dr",  "drm", "ul",  "ur", "urm",  NULL};
  char **entry;
  int rep ; 
  if ( l_pos == NULL ) return 4;
  rep = is_string_in_array(l_pos,Table,1);
  if ( rep < 0 ) 
    {
      Scierror("Error:\toptional argument %s of function %s has a wrong value %s\n",varnam,fname,l_pos);
      Scierror("\texpected values are '%s'", *Table);
      for (entry = Table+1 ; *entry != NULL; entry++) {
	if (entry[1] == NULL) {
	  Scierror(", or '%s'\n",*entry);
	} else {
	  Scierror(", '%s'",*entry);
	}
      }
      return -1;
    }
  return rep;
}


/*-----------------------------------------------------------
 * Check optional argument nax 
 * note that var can be changed by this function 
 *-----------------------------------------------------------*/

static const int nax_def[]={2,10,2,10};
static int nax_loc[]={2,10,2,10};

static int * check_nax(Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  int i;
  if ( var == NULLMAT) 
    {
      for (i = 0 ; i < 4; ++i) nax_loc[i]=nax_def[i];
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
	  for (i = 0 ; i < 4; ++i) nax_loc[i]=Max(ivar[i],0);
	  return ivar;
	}
    }
  return nax_loc;
}

/*-----------------------------------------------------------
 * Check optional argument zminmax
 *-----------------------------------------------------------*/

static int check_zminmax (Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  if ( var != NULLMAT &&  var->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",fname,varname);
      return FAIL;
    }
  return OK;
}

/*-----------------------------------------------------------
 * Check optional argument colminmax
 *-----------------------------------------------------------*/

static int check_colminmax (Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  if ( var != NULLMAT && var->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",fname,varname);
      return FAIL;
    }
  return OK;
}

/*-----------------------------------------------------------
 * Check optional argument colout
 *-----------------------------------------------------------*/

static int check_colout (Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  if ( var != NULLMAT &&  var->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",fname,varname);
      return FAIL;
    }
  return OK;
}

/*-----------------------------------------------------------
 * Check optional argument for logflags 
 * note that var can be changed by this function 
 * (Bruno)
 *-----------------------------------------------------------*/

#define DEFLOGFLAGS "gnn" 
static char logflags_loc[]  = DEFLOGFLAGS;

static char * check_logflags(Stack stack,const char *fname,char *varname,char *logflags)
{
  if ( logflags == NULL ) 
    {
      strcpy(logflags_loc,DEFLOGFLAGS);
    }
  else 
    {
      if ( strlen(logflags) == 2 ) 
	{
	  sprintf(logflags_loc,"g%c%c",logflags[0],logflags[1]);
	}
      else if ( strlen(logflags) != 3) 
	{
	  Scierror("%s: optional argument %s has wrong size (%d), 3 expected\n",fname,varname,strlen(logflags)); 
	  return NULL;
	}
      else
	{
	  strcpy(logflags_loc,logflags);
	}
    }
  return logflags_loc;
}


static int get_arc(Stack stack, int rhs, int opt, int lhs,double **val)
{
  NspMatrix *M1;
  int i;
  static double l[6];
  switch ( rhs -opt ) 
    {
    case 1 :
      if ((M1=GetRealMat(stack,1)) == NULLMAT ) return FAIL;
      CheckLength(NspFname(stack),1,M1,6);
      *val = M1->R;
      break;
    case 6 :
      for ( i = 1 ; i <= 6 ; i++) 
	{
	  if (GetScalarDouble(stack,i,l+i-1) == FAIL) return FAIL;
	}
      *val = l;
      break;
    default :
      Scierror("%s: wrong number of standard rhs arguments (%d), rhs must be 1 or 6\n",NspFname(stack),rhs);
      return FAIL;
    }
  return OK;
}


/*-------------------------------------------------------------------
 * champ 
 * champ(x,y,fx,fy,[arfact=1.0,rect=[xMin,yMin,xMax,yMax],flag])
 * champ1(x,y,fx,fy,[arfact=1.0,rect=[xMin,yMin,xMax,yMax],flag])
 *-------------------------------------------------------------------*/

#ifdef NEW_GRAPHICS 

int nsp_champ_obj(BCG *Xgc,NspMatrix *x,NspMatrix *y,NspMatrix *fx,NspMatrix *fy,int colored) 
{
  NspAxes *axe=  nsp_check_for_axes(Xgc);
  if ( axe == NULL) return FAIL;
  /* create a vfield and insert-it in axes */
  if ( ( x = (NspMatrix *)  nsp_object_copy_and_name("x",NSP_OBJECT(x))) == NULLMAT) return FAIL;
  if ( ( y = (NspMatrix *)  nsp_object_copy_and_name("y",NSP_OBJECT(y))) == NULLMAT) return FAIL;
  if ( ( fx = (NspMatrix *)  nsp_object_copy_and_name("fx",NSP_OBJECT(fx))) == NULLMAT) return FAIL;
  if ( ( fy = (NspMatrix *)  nsp_object_copy_and_name("fy",NSP_OBJECT(fy))) == NULLMAT) return FAIL;
  NspVField *vf = nsp_vfield_create("vf",fx,fy,x,y,colored,NULL);
  if ( vf == NULL) return FAIL;
  /* insert the new vfield */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) vf )== FAIL)
    return FAIL;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  return OK;
}

#endif 

static int int_champ_G(Stack stack, int rhs, int opt, int lhs,
		       int func(BCG *Xgc,double *x, double *y, double *fx, double *fy, int *n1, int *n2,
				char *strflag, double *brect, double *arfact))
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

  CheckSameDims(NspFname(stack),3,4,fx,fy);
  CheckDimProp(NspFname(stack),2,3, y->mn != fx->n);
  CheckDimProp(NspFname(stack),1,3, x->mn != fx->m);

  if ( fx->mn == 0) { return 0;} 

  if (( R = check_rect(stack,NspFname(stack),"rect",rect)) == NULL)  return RET_BUG;
  if (( strf = check_strf(stack,NspFname(stack),"strf",strf))==NULL) return RET_BUG;
  if ( R == rect_def ) strf[1]='5';

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
#ifdef NEW_GRAPHICS 
  nsp_champ_obj(Xgc,x,y,fx,fy,( func == nsp_champ ) ? FALSE: TRUE);
#else 
  (*func)(Xgc,x->R,y->R,fx->R,fy->R,&fx->m,&fx->n,strf,R, &arfact);
#endif 
  return 0;
}

int int_champ( Stack stack, int rhs, int opt, int lhs)
{
  if (rhs <= 0) return sci_demo(NspFname(stack),"champ(1:10,1:10,rand(10,10),rand(10,10));",1);
  return int_champ_G( stack, rhs,opt,lhs, nsp_champ);
}

int int_champ1( Stack stack, int rhs, int opt, int lhs)
{
  if (rhs <= 0) return sci_demo(NspFname(stack),"champ1(1:10,1:10,rand(10,10),rand(10,10));",1);
  return int_champ_G( stack, rhs, opt, lhs,nsp_champ1);
}



/*-----------------------------------------------------------
 *  contour(x,y,z,nz,[theta,alpha,leg,flag,ebox,zlev])
 *-----------------------------------------------------------*/

int int_contour( Stack stack, int rhs, int opt, int lhs)
{
  NspObject  *args = NULL,*fobj;/* when z is a function */
  BCG *Xgc;
  int *iflag,flagx,nnz=10;
  NspMatrix *x,*y,*z,*nz,*Mebox=NULL,*Mflag=NULL;
  double alpha=35.0,theta=45.0,zlev=0.0,*ebox ;
  char *leg=NULL;
  int_types T[] = {realmat,realmat,obj,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "alpha",s_double,NULLOBJ,-1},
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",realmat,NULLOBJ,-1},
		      { "leg",string,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { "zlevel",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  
  if (rhs <= 0) { return sci_demo(NspFname(stack),"contour(1:5,1:10,rand(5,10),5);",1); }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fobj,&nz,&opts,&alpha,&Mebox,&Mflag,&leg,&theta,&zlev) == FAIL) return RET_BUG;
  

  CheckVector(NspFname(stack),1,x);
  CheckVector(NspFname(stack),2,y);

  if ( IsNspPList(fobj) )
    {
      /* third argument can be a macro */
      if ((z = nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULL) return RET_BUG;
      if ( plot3d_build_z(stack,x,y,z,fobj,args)== FAIL) 
	{
	  nsp_matrix_destroy(z);
	  return RET_BUG;
	}
    }
  else if (IsMat(fobj) && ((NspMatrix *) fobj)->rc_type == 'r')
    {
      z = Mat2double((NspMatrix *) fobj);
    }
  else
    {
      /* here we could accept list(z,colors) to emulate scilab code */
      Scierror("%s: third argument should be a real matrix or a function\n",NspFname(stack));
      return RET_BUG;
    }

  if ( z->mn == 0) return 0;
  if ( z->m == 1 || z->n == 1) {
    Scierror("%s: third argument is a vector, expecting a matrix \n",NspFname(stack));
    return RET_BUG;
  }

  CheckDimProp(NspFname(stack),1,3, x->mn != z->m); 
  CheckDimProp(NspFname(stack),2,3, y->mn != z->n); 

  if ( nz->mn == 0 ) return 0;
  if ( nz->mn == 1 ) {
    flagx = 0;  nnz = Max(1,(int) nz->R[0]); /* number of levels */
  } else {
    flagx = 1;  nnz = nz->mn ; /*levels given */
  }

  if ( leg == NULL) leg = "";

  if (( iflag=check_iflag(stack,NspFname(stack),"flag",Mflag,3)) == NULL) return RET_BUG;
  if (( ebox=check_ebox(stack,NspFname(stack),"ebox",Mebox)) == NULL) return RET_BUG;

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  nsp_gcontour(Xgc,x->R,y->R,z->R,&z->m,&z->n, &flagx, &nnz,nz->R, &theta, &alpha,
	      leg, iflag, ebox, &zlev,strlen(leg));
  return 0;
}


/*-----------------------------------------------------------
 * standard 2d optional arguments 
 * FIXME: axesflags and frame should be used 
 *        strf kept for compatibility ? 
 *-----------------------------------------------------------*/

static   nsp_option opts_2d[] ={{ "axesflag",s_int,NULLOBJ,-1},
				{ "frameflag",s_int,NULLOBJ,-1},
				{ "leg",string,NULLOBJ,-1},
				{ "leg_pos",string,NULLOBJ,-1},
				{ "logflag",string,NULLOBJ,-1}, 
				{ "nax",mat_int,NULLOBJ,-1},
				{ "rect",realmat,NULLOBJ,-1},
				{ "strf",string,NULLOBJ,-1},
				{ "style",mat_int,NULLOBJ,-1},
				{ NULL,t_end,NULLOBJ,-1}};


int int_check2d(Stack stack,NspMatrix *Mstyle,NspMatrix **Mstyle_new,int ns,
		char **strf,char **leg, char **leg_pos,int *leg_pos_i,
		NspMatrix *Mrect,double **rect,
		NspMatrix *Mnax,int **nax,
		int frameflag,int axesflag,char **logflags)
{
  char *leg1;
  if (( *Mstyle_new = check_style(stack,NspFname(stack),"style",Mstyle,ns))== NULL) return RET_BUG;
  if (( *strf = check_strf(stack,NspFname(stack),"strf",*strf))==NULL) return RET_BUG;
  if (( leg1 = check_legend(stack,NspFname(stack),"leg",*leg))==NULL) return RET_BUG;
  if (( *leg_pos_i = check_legend_pos(stack,NspFname(stack),"leg",*leg_pos))== -1 ) return RET_BUG;
  if (( *rect = check_rect(stack,NspFname(stack),"rect",Mrect))==NULL) return RET_BUG;
  if (( *nax = check_nax(stack,NspFname(stack),"nax",Mnax))==NULL) return RET_BUG;
  if (( *logflags= check_logflags(stack,NspFname(stack),"logflag",*logflags))==NULL) return RET_BUG;

  if ( frameflag < -1 || frameflag > 8 ) 
    {
      Scierror("%s: frame must be in the range [0,8]\n",NspFname(stack));
      return RET_BUG;
    }   
  if ( axesflag < -1 || axesflag > 5 ) 
    {
      Scierror("%s: axes must be in the range [0,5]\n",NspFname(stack));
      return RET_BUG;
    }   

  /* check that Mrect and strf are compatible */
  if ( Mrect == NULL ) 
    { 
      /* if rect is not provided and selected by strf we switch to recompute rect */
      plot2d_strf_change('u',*strf);
    }
  else 
    {
      /* if rect is provided and not selected by strf we force it */
      plot2d_strf_change('d',*strf);
    }
  /* change strf according to other given flags */
  *strf[0] =  ( leg == NULL || strcmp(leg1,"")==0 ) ? '0': '1';
  *leg = leg1;
  if ( frameflag != -1 ) (*strf)[1] = (char)(frameflag+48); 
  if ( axesflag != -1 )  (*strf)[2] = (char)(axesflag+48); 
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
  NspMatrix *Mistyle; 
  int *nax;
  NspMatrix *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;
  int leg_posi;
  int_types T[] = {realmat,realmat,realmat,realmat,new_opts, t_end} ;

  if (rhs <= 0) {return  sci_demo(NspFname(stack),"contour2d(1:5,1:10,rand(5,10),5);",1); }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&z,&nz,&opts_2d,&axes,&frame,&leg,&leg_pos,&logflags,&Mnax,&Mrect,&strf,&Mstyle) == FAIL) return RET_BUG;

  CheckVector(NspFname(stack),1,x);
  CheckVector(NspFname(stack),2,y);
  if ( z->mn == 0) return 0;
  if ( z->m == 1 || z->n == 1) {
    Scierror("%s: third argument is a vector, expecting a matrix \n",NspFname(stack));
    return RET_BUG;
  }

  CheckDimProp(NspFname(stack),1,3, x->mn != z->m); 
  CheckDimProp(NspFname(stack),2,3, y->mn != z->n); 

  /* XXX could be among the optional args */ 

  if ( nz->mn == 0 ) return 0;
  if ( nz->mn == 1 ) {
    flagx = 0;  nnz = Max(1,(int) nz->R[0]);
  } else {
    flagx = 1;  nnz = nz->mn ;
  }

  if ( int_check2d(stack,Mstyle,&Mistyle,nnz,&strf,&leg,&leg_pos,&leg_posi,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;
  
  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  (*func)(Xgc,x->R,y->R,z->R,&z->m,&z->n,&flagx,&nnz,nz->R,Mistyle->I,strf,leg,rect,nax); /*,strlen(strf),strlen(leg));*/
  if ( Mstyle != Mistyle) 
    nsp_matrix_destroy(Mistyle);
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

  CheckVector(NspFname(stack),1,x);
  CheckVector(NspFname(stack),2,y);
  if ( z->mn == 0) return 0;
  if ( z->m == 1 || z->n == 1) {
    Scierror("%s: third argument is a vector, expecting a matrix \n",NspFname(stack));
    return RET_BUG;
  }

  CheckDimProp(NspFname(stack),1,3, x->mn != z->m); 
  CheckDimProp(NspFname(stack),2,3, y->mn != z->n); 

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
  const char *leg=NULL,*leg1=NULL;
  int_types T[] = {realmat,realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "alpha",s_double,NULLOBJ,-1},
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",realmat,NULLOBJ,-1},
		      { "leg",string,NULLOBJ,-1}, /* XXXX string matrix ? */
		      { "style",mat_int,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( rhs <= 0) {return  sci_demo(NspFname(stack),"t=0:0.1:5*%pi;param3d(sin(t),cos(t),t*0.1);",1); }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&z,&opts,&alpha,&Mebox,&flag,&leg,&Mstyle,&theta) == FAIL) return RET_BUG;

  if ( x->mn == 0 ) return 0;

  CheckSameDims(NspFname(stack),1,2,x,y);
  CheckSameDims(NspFname(stack),1,3,x,z);

  if (( iflag = check_param_iflag(stack,NspFname(stack),"flag",flag,2))==NULL) return RET_BUG;
  if (( ebox = check_ebox(stack,NspFname(stack),"ebox",Mebox)) == NULL) return RET_BUG;
  if (( leg1 = check_legend_3d(stack,NspFname(stack),"leg",leg)) == NULL) return RET_BUG;

  if ( Mstyle != NULLMAT ) 
    { 
      if ( Mstyle->mn != z->n) 
	{
	  Scierror("%s: style argument is too small (%d), %d values expected \n",NspFname(stack),Mstyle->mn, z->n);
	  return RET_BUG;
	}
    }
  /*
   * check that iflag[1] and leg are compatible 
   * i.e force visibility of axes names if they are given
   */
  if (leg !=  NULL && strlen(leg) != 0 ) iflag[1]=4;

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);

  if ( Mstyle != NULLMAT ) 
    {
      int izcol=1;
      nsp_param3d_1(Xgc,x->R,y->R,z->R,&z->m,&z->n,&izcol,(int *) Mstyle->R,
		    &theta,&alpha,leg1,iflag,ebox);
    }
  else 
    {
      nsp_param3d(Xgc,x->R,y->R,z->R,&z->mn,&theta,&alpha,leg1,iflag,ebox);
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

  if (rhs <= 0) { return sci_demo(NspFname(stack), "t=0:0.1:5*%pi,[x,y]=geom3d(sin(t),cos(t),t/10);",1);}

  CheckRhs(3,3);
  CheckLhs(2,3);

  if ((x1 = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  if ((y1 = GetRealMatCopy(stack,2)) == NULLMAT) return RET_BUG;
  if ((z1 = GetRealMat(stack,3)) == NULLMAT) return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x1,y1);
  CheckSameDims(NspFname(stack),2,3,y1,z1);

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

typedef int (*f3d) (BCG *Xgc,double *,double *,double *,int *p,int *q,double *,double *,const char *,int *,double *); 
typedef int (*f3d1)(BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *, double *,const char *,int *,double *); 
typedef int (*f3d2)(BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *, double *,const char *,int *,double *); 
typedef int (*f3d3)(BCG *Xgc,double *,double *,double *,int *cvect,int *p,int *q,double *, double *,const char *,int *,double *);

static int plot3d_build_z(Stack stack,NspMatrix *x,NspMatrix *y,NspMatrix *z,NspObject *f, NspObject *fargs);

int int_plot3d_G( Stack stack, int rhs, int opt, int lhs,f3d func,f3d1 func1,f3d2 func2,f3d3 func3)
{
  BCG *Xgc;
  NspObject  *args = NULL,*fobj;/* when z is a function */
  double alpha=35.0,theta=45.0,*ebox ;
  const char *leg=NULL, *leg1;
  NspMatrix *x,*y,*z,*zloc=NULL,*Mcolors=NULL,*Mflag=NULL,*Mebox=NULL;
  int izcol=0, *zcol=NULL,*iflag, ret=0;
  
  int_types T[] = {realmat,realmat,obj,new_opts, t_end} ;

  nsp_option opts[] ={{ "args",list,NULLOBJ,-1},
		      { "alpha",s_double,NULLOBJ,-1},
		      { "colors",mat_int,NULLOBJ,-1},
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",realmat,NULLOBJ,-1},
		      { "leg", string,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fobj,&opts,&args,&alpha,&Mcolors,&Mebox,&Mflag,&leg,&theta) == FAIL) return RET_BUG;

  if (x->mn == 0) { return 0;}

  if ( IsNspPList(fobj) )
    {
      /* third argument can be a macro */
      if ((z = zloc= nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULL) return RET_BUG;
      if ( plot3d_build_z(stack,x,y,z,fobj,args)== FAIL) 
	{
	  ret= RET_BUG;
	  goto end;
	}
    }
  else if (IsMat(fobj) && ((NspMatrix *) fobj)->rc_type == 'r')
    {
      z = Mat2double((NspMatrix *) fobj);
    }
  else
    {
      /* here we could accept list(z,colors) to emulate scilab code */
      Scierror("%s: third argument should be a real matrix or a function\n",NspFname(stack));
      ret= RET_BUG;
      goto end;
    }

  if (Mcolors == NULLMAT)
    {
      izcol=0;
    } 
  else 
    {
      izcol = 1;
      zcol  = (int *) Mcolors->R;
      CheckDimProp(NspFname(stack),3,opts[2].position, Mcolors->mn != z->mn  && Mcolors->mn != z->n );
      /* 
       *   Added by E Segre 4/5/4000. In the case where zcol is a 
       *   matrix of the same size as z, we set izcol to 2. This
       *   value is later transmitted to the C2F(fac3dg) routine,
       *   which has been modified to do the interpolated shading 
       *    (see the file SCI/routines/graphics/Plo3d.c 
       */
      if ( Mcolors->mn == z->mn ) izcol=2  ;
    }

  if (( iflag = check_iflag(stack,NspFname(stack),"flag",Mflag,3))==NULL) 
    {
      ret= RET_BUG; goto end;
    }
  if (( ebox = check_ebox(stack,NspFname(stack),"ebox",Mebox)) == NULL) 
    {
      ret= RET_BUG; goto end;
    }

  if (( leg1 = check_legend_3d(stack,NspFname(stack),"leg",leg)) == NULL) 
    {
      ret= RET_BUG; goto end;
    }


  if ( x->mn == z->mn && x->mn == z->mn && x->mn != 1) 
    {
      if (! ( x->m == y->m && y->m == z->m && x->n == y->n && y->n == z->n)) {
	Scierror("%s: The three first arguments have incompatible length\n",NspFname(stack));
	ret= RET_BUG; goto end;
      }
    } 
  else 
    {
      CheckDimProp(NspFname(stack),1,3, x->mn != z->m); 
      CheckDimProp(NspFname(stack),2,3, y->mn != z->n); 
      if ( x->mn  <= 1 || y->mn <= 1 ) 
	{
	  Scierror("%s: first and second arguments should be of size >= 2\n",NspFname(stack));
	  ret= RET_BUG; goto end;
	}
    }
  /* 7 and 8 are the mode for superposed graphics */
  iflag[1]=Max(Min(iflag[1],8),0);
  /* check that iflag[1] and ebox are compatible */
  if ( Mebox != NULLMAT) 
    {
      /* ebox is given then iflag[1] must be 1 or 3 or 5 */
      if ( iflag[1] == 2 ||  iflag[1] == 4 ||  iflag[1] == 6 || iflag[1] == 8 ) iflag[1]--;
    }
  else
    {
      /* ebox is not given then iflag[1] cannot be 1 or 3 or 5 */
      if ( iflag[1] == 1 ||  iflag[1] == 3 ||  iflag[1] == 5 || iflag[1] == 7 ) iflag[1]++;
    }
  /*
   * check that iflag[2] and leg are compatible 
   * i.e force visibility of axes names if they are given
   */
  if (leg !=  NULL && strlen(leg) != 0 ) iflag[2]=4;

  if ( x->mn == 0 || y->mn == 0 || z->mn == 0) { goto end;}

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  if ( x->mn == y->mn && x->mn == z->mn && x->mn != 1) 
    { 
      /*  Here we are in the case where x,y and z specify some polygons */
      if (izcol == 0) 
	{
	  (*func1)(Xgc,x->R,y->R,z->R,zcol,&z->m,&z->n,&theta,&alpha,leg1,iflag,ebox);
	} 
      else if (izcol == 2) 
	{
	  /*  New case for the fac3d3 call (interpolated shadig)  */
	  (*func3)(Xgc,x->R,y->R,z->R,zcol,&z->m,&z->n,&theta,&alpha,leg1,iflag,ebox);
	}
      else 
	{
	  (*func2)(Xgc,x->R,y->R,z->R,zcol,&z->m,&z->n,&theta,&alpha,leg1,iflag,ebox);
	}
    } 
  else 
    {
      /*  Here we are in the standard case  */
      (*func)(Xgc,x->R,y->R,z->R,&z->m,&z->n,&theta,&alpha,leg1,iflag,ebox);
    }
 end:
  nsp_matrix_destroy(zloc);
  return ret;

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
	/* FIXME : a changer pour metre une fonction eval standard */
	if ( nsp_gtk_eval_function((NspPList *)func ,targs,nargs,&nsp_ret,&nret)== FAIL) 
	  goto end;
	if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' && ((NspMatrix *) nsp_ret)->mn==1 )
	  {
	    Mat2double((NspMatrix *) nsp_ret);
	    z->R[i+z->m*j]= ((NspMatrix *) nsp_ret)->R[0];
	    nsp_matrix_destroy((NspMatrix *) nsp_ret);
	  }
	else 
	  {
	    if ( nret == 1) nsp_object_destroy(&nsp_ret);
	    Scierror("%s: evaluation failed for z(%d,%d)\n",NspFname(stack),i+1,j+1);
	    goto end; 
	  }
      }
  ret = OK;
 end:
  {
    if ( fargs != NULL)nsp_object_destroy(&args);
    nsp_object_destroy(&func);
    nsp_matrix_destroy(xi);
    nsp_matrix_destroy(yj);
    return ret;
  }
}



int int_plot3d( Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs <= 0) return sci_demo(NspFname(stack),"t=-%pi:0.3:%pi;plot3d(t,t,sin(t)'*cos(t))",1);
  return int_plot3d_G(stack,rhs,opt,lhs,nsp_plot3d,nsp_plot_fac3d,nsp_plot_fac3d_2,nsp_plot_fac3d_3);
}

int int_plot3d1( Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs <= 0) return sci_demo(NspFname(stack),"t=-%pi:0.3:%pi;plot3d1(t,t,sin(t)'*cos(t));",1);
  return int_plot3d_G(stack,rhs,opt,lhs,nsp_plot3d_1,nsp_plot_fac3d_1,nsp_plot_fac3d_2,nsp_plot_fac3d_3);
}

/* [] = draw_3d_obj(list(Objs),...)
 */

int int_draw3dobj(Stack stack, int rhs, int opt, int lhs)
{
  int err,*iflag,nf=0,nbObj=0, box_color=-1,box_style=SCILAB,with_mesh=FALSE,with_box=TRUE;
  char *box_style_name=NULL;
  NspList *L;
  BCG *Xgc;
  double alpha=35.0,theta=45.0,*ebox ;
  const char *leg=NULL, *leg1;
  NspMatrix *Mflag=NULL,*Mebox=NULL;
  
  int_types T[] = {list,new_opts, t_end} ;

  nsp_option opts[] ={
    { "alpha",s_double,NULLOBJ,-1},
    { "box_color",s_int,NULLOBJ,-1},
    { "box_style",string,NULLOBJ,-1},
    { "ebox",realmat,NULLOBJ,-1},
    { "flag",realmat,NULLOBJ,-1},
    { "leg", string,NULLOBJ,-1},
    { "theta",s_double,NULLOBJ,-1},
    { "with_box",s_bool,NULLOBJ,-1},
    { "with_mesh",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&L,&opts,&alpha,&box_color,&box_style_name,
	       &Mebox,&Mflag,&leg,&theta,&with_box,&with_mesh) == FAIL) 
    return RET_BUG;
  CheckLhs(1,1);

  if ( box_style_name != NULL) 
    {
      if ( strcmp("matlab",box_style_name) == 0 ) 
	box_style = MATLAB;
      else if ( strcmp("scilab",box_style_name) == 0 ) 
	box_style = SCILAB;
      else
	box_style = OTHER;
    }

  obj3d_from_list(stack,L,FALSE,&err,&nf,&nbObj);

  if (err == TRUE) 
    {
      Scierror("%s: list of Object is wrong\n",NspFname(stack));
      return RET_BUG;
    }

  if (( iflag = check_iflag(stack,NspFname(stack),"flag",Mflag,3))==NULL) return RET_BUG;
  if (( ebox = check_ebox(stack,NspFname(stack),"ebox",Mebox)) == NULL) return RET_BUG;
  if (( leg1 = check_legend_3d(stack,NspFname(stack),"leg",leg)) == NULL) return RET_BUG;

  /* 7 and 8 are the mode for superposed graphics */
  iflag[1]=Max(Min(iflag[1],8),0);
  /* check that iflag[1] and ebox are compatible */
  if ( Mebox != NULLMAT) 
    {
      /* ebox is given then iflag[1] must be 1 or 3 or 5 */
      if ( iflag[1] == 2 ||  iflag[1] == 4 ||  iflag[1] == 6 || iflag[1] == 8 ) iflag[1]--;
    }
  else
    {
      /* ebox is not given then iflag[1] cannot be 1 or 3 or 5 */
      if ( iflag[1] == 1 ||  iflag[1] == 3 ||  iflag[1] == 5 || iflag[1] == 7 ) iflag[1]++;
    }
  /*
   * check that iflag[2] and leg are compatible 
   * i.e force visibility of axes names if they are given
   */
  if (leg !=  NULL && strlen(leg) != 0 ) iflag[2]=4;

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);

  nsp_draw_3d_obj(Xgc,L,&theta,&alpha,leg1,iflag,ebox,with_mesh,with_box,box_color,box_style);
  return 0;
}

/*-----------------------------------------------------------
 *   plot2d(x,y,[style,strf,leg,rect,nax]) 
 *   plot2dxx(x,y,[style,strf,leg,rect,nax])
 *-----------------------------------------------------------*/


static int plot2d_build_y(Stack stack,NspMatrix *x,NspMatrix *y,NspObject *f, NspObject *fargs);

typedef int (*func_2d)(BCG *Xgc,char *,double *,double *,int *,int *,int *,char *,const char *,int,double *,int *);

int int_plot2d_G( Stack stack, int rhs, int opt, int lhs,int force2d,func_2d func)
{
  BCG *Xgc;
  /* for 2d optional arguments; */
  int *nax, frame= -1, axes=-1,ncurves,lcurve;
  NspMatrix *Mistyle, *x,*y, *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  NspObject  *args = NULL,*fobj;/* when z is a function */
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL, tflag='g', *leg_pos = NULL;
  int leg_posi;

  int_types T[] = {realmat,obj,new_opts, t_end} ;
  
  if ( GetArgs(stack,rhs,opt,T,&x,&fobj,&opts_2d,&axes,&frame,&leg,&leg_pos,&logflags,&Mnax,&Mrect,&strf,&Mstyle) == FAIL)
    return RET_BUG;

  if ( IsNspPList(fobj) )
    {
      /* third argument can be a macro */
      if ((y = nsp_matrix_create(NVOID,'r',x->m,x->n))== NULL) return RET_BUG;
      if ( plot2d_build_y(stack,x,y,fobj,args)== FAIL) 
	{
	  nsp_matrix_destroy(y);
	  return RET_BUG;
	}
    }

  else if (IsMat(fobj) && ((NspMatrix *) fobj)->rc_type == 'r')
    {
      /* be sure that y is in a proper state */
      y = Mat2double((NspMatrix *) fobj);
    }
  else
    {
      /* here we could accept list(z,colors) to emulate scilab code */
      Scierror("%s: second argument should be a real matrix or a function\n",NspFname(stack));
      return RET_BUG;
    }


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
	      Scierror("%s: x and y have incompatible length\n",NspFname(stack));
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
	      Scierror("%s: x and y have incompatible length\n",NspFname(stack));
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
	      Scierror("%s: x and y have incompatible length\n",NspFname(stack));
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
	      Scierror("%s: x and y have incompatible length\n",NspFname(stack));
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
	      Scierror("%s: x and y have incompatible length\n",NspFname(stack));
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
	      Scierror("%s: x and y have incompatible length\n",NspFname(stack));
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
	  Scierror("%s: x and y have incompatible length\n",NspFname(stack));
	  return RET_BUG;
	}
      else 
	{
	  ncurves = x->n;
	  lcurve  = x->m;
	  tflag = 'g';
	}
    }

  if ( int_check2d(stack,Mstyle,&Mistyle,ncurves,&strf,&leg,&leg_pos,&leg_posi,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;
  
  /* logflags */ 
  logflags[0]= tflag;

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);

  /* FIXME: make a function with all this and propagate to other primitives */

  if ( Mrect == NULL ) 
    { 
      /* if rect is not provided and selected by strf we switch to recompute rect */
      plot2d_strf_change('u',strf);
    }
  else 
    {
      /* if rect is provided and not selected by strf we force it */
      plot2d_strf_change('d',strf);
    }

#ifdef NEW_GRAPHICS 
  nsp_plot2d_obj(Xgc,x->R,y->R,logflags, &ncurves, &lcurve,Mistyle->I,strf,leg,leg_posi,rect,nax);
#else 
  if ( strcmp(logflags,"gnn")==0 && force2d == 0) 
    {
      nsp_plot2d(Xgc,x->R,y->R,&ncurves, &lcurve,Mistyle->I,strf,leg,leg_posi,rect,nax);
    }
  else
    {
      (*func)(Xgc,logflags,x->R,y->R,&ncurves, &lcurve,Mistyle->I,strf,leg,leg_posi,rect,nax);
    }
#endif 

  if ( Mstyle != Mistyle)     nsp_matrix_destroy(Mistyle);


  return 0;
}



/* 
 * build y from f(x,fargs)
 */

static int plot2d_build_y(Stack stack,NspMatrix *x,NspMatrix *y,NspObject *f, NspObject *fargs)
{
  NspObject *targs[4];
  NspObject *nsp_ret;
  int nret = 1,nargs = 1;
  NspMatrix *xi;
  NspObject *func, *args = NULL;
  int ret = FAIL,i;
  if (( func =nsp_object_copy(f)) == NULL) return RET_BUG;
  if ((nsp_object_set_name(func,"plot2d_build")== FAIL)) return RET_BUG;
  /** extra arguments **/
  if ( fargs != NULL ) 
    {
      if (( args =nsp_object_copy(fargs)) == NULL ) return RET_BUG;
      if ((nsp_object_set_name(args,"arg")== FAIL)) return RET_BUG;
    }
  if ((xi = nsp_matrix_create("xi",'r',1,1))== NULL) return RET_BUG;

  if (fargs != NULL ) 
    {
      targs[1]=(NspObject *) args;
      nargs= 2;
    }

  for ( i= 0 ; i < x->mn ; i++) 
    {
      xi->R[0]= x->R[i];
      targs[0] =(NspObject *) xi;
      if ( targs[0]== NULL )   goto end;
      /* FIXME : a changer pour metre une fonction eval standard */
      if ( nsp_gtk_eval_function((NspPList *)func ,targs,nargs,&nsp_ret,&nret)== FAIL) 
	goto end;
      if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' && ((NspMatrix *) nsp_ret)->mn==1 )
	{
	  Mat2double((NspMatrix *) nsp_ret);
	  y->R[i]= ((NspMatrix *) nsp_ret)->R[0];
	}
      else 
	{
	  Scierror("%s: evaluation failed for y(%d)\n",NspFname(stack),i+1);
	  goto end; 
	}
    }
  ret = OK;
 end:
  {
    if ( fargs != NULL)nsp_object_destroy(&args);
    nsp_object_destroy(&func);
    nsp_matrix_destroy(xi);
    return ret;
  }
}


static int int_plot2d( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,0,nsp_plot2d_1);
}

static int int_plot2d1_1( Stack stack, int rhs, int opt, int lhs) 
{
  static char str[]="x=0:0.1:2*%pi;plot2d([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,nsp_plot2d_1);
}

static int int_plot2d1_2( Stack stack, int rhs, int opt, int lhs) 
{
  static char str[]="x=0:0.1:2*%pi;plot2d2([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,nsp_plot2d_2);
}

static int int_plot2d1_3( Stack stack, int rhs, int opt, int lhs) 
{
  static char str[]="x=0:0.1:2*%pi;plot2d3([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,nsp_plot2d_3);
}

static int int_plot2d1_4( Stack stack, int rhs, int opt, int lhs) 
{
  static char str[]="x=0:0.1:2*%pi;plot2d4([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],strf='151',rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return sci_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,nsp_plot2d_4);
}

/*-----------------------------------------------------------
 *  grayplot(x,y,z,[strf,rect,nax])
 *  Attention trop d'args optionnels XXXXX 
 *-----------------------------------------------------------*/

int int_grayplot( Stack stack, int rhs, int opt, int lhs)
{
  nsp_option opts_mp[] ={{ "axesflag",s_int,NULLOBJ,-1},
			 { "colminmax",mat_int,NULLOBJ,-1},
			 { "colout",mat_int,NULLOBJ,-1},
			 { "frameflag",s_int,NULLOBJ,-1},
			 { "leg",string,NULLOBJ,-1},
			 { "leg_pos",string,NULLOBJ,-1},
			 { "logflag",string,NULLOBJ,-1}, 
			 { "nax",mat_int,NULLOBJ,-1},
			 { "rect",realmat,NULLOBJ,-1},
			 { "remap",s_bool,NULLOBJ,-1},
			 { "shade",s_bool,NULLOBJ,-1},
			 { "strf",string,NULLOBJ,-1},
			 { "style",mat_int,NULLOBJ,-1},
			 { "zminmax",mat,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};
  NspObject  *args = NULL,*fobj;/* when z is a function */
  /* for 2d optional arguments; */
  int *nax, frame= -1, axes=-1, remap=TRUE,shade=FALSE;
  NspMatrix *Mistyle,*Mrect=NULL,*Mnax=NULL,*Mstyle=NULL,*Mzminmax=NULL,*Mcolminmax=NULL,*Mcolout=NULL;
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;
  int leg_posi;
  int_types T[] = {realmat,realmat,obj,new_opts, t_end} ;
  /* */
  BCG *Xgc;
  NspMatrix *x,*y,*z; 

  if ( rhs <= 0) {return sci_demo(NspFname(stack), "t=-%pi:0.1:%pi;m=sin(t)'*cos(t);grayplot(t,t,m);",1);}

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fobj,&opts_mp,&axes,&Mcolminmax,&Mcolout,&frame,&leg,&leg_pos,
	       &logflags,&Mnax,&Mrect,&remap,&shade,&strf,&Mstyle,&Mzminmax) == FAIL) return RET_BUG;

  CheckVector(NspFname(stack),1,x);
  CheckVector(NspFname(stack),2,y);

  if ( IsNspPList(fobj) )
    {
      /* third argument can be a macro */
      if ((z = nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULL) return RET_BUG;
      if ( plot3d_build_z(stack,x,y,z,fobj,args)== FAIL) 
	{
	  nsp_matrix_destroy(z);
	  return RET_BUG;
	}
    }
  else if (IsMat(fobj) && ((NspMatrix *) fobj)->rc_type == 'r')
    {
      z= Mat2double((NspMatrix *) fobj);
    }
  else
    {
      /* here we could accept list(z,colors) to emulate scilab code */
      Scierror("%s: third argument should be a real matrix or a function\n",NspFname(stack));
      return RET_BUG;
    }

  if ( z->mn == 0) return 0;
  if ( z->m == 1 || z->n == 1) {
    Scierror("%s: third argument is a vector, expecting a matrix \n",NspFname(stack));
    return RET_BUG;
  }

  CheckDimProp(NspFname(stack),1,3, x->mn != z->m); 
  CheckDimProp(NspFname(stack),2,3, y->mn != z->n); 

  if ( check_zminmax(stack,NspFname(stack),"zminmax",Mzminmax)== FAIL ) return RET_BUG;
  if ( check_colminmax(stack,NspFname(stack),"colminmax",Mcolminmax)== FAIL) return RET_BUG;
  if ( check_colout(stack,NspFname(stack),"colout",Mcolout)== FAIL) return RET_BUG;

  if ( int_check2d(stack,Mstyle,&Mistyle,z->mn,&strf,&leg,&leg_pos,&leg_posi,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  nsp_draw_matrix(Xgc,x->R,y->R,z->R,z->m,z->n,strf,rect,nax,remap,
		  (Mcolminmax == NULL) ? NULL :(int *) Mcolminmax->R,
		  (Mzminmax == NULL) ? NULL : Mzminmax->R,
		  (Mcolout == NULL) ? NULL :(int *)  Mcolout->R,
		  shade);
  if ( Mstyle != Mistyle)   nsp_matrix_destroy(Mistyle);

  return 0;
}

/*-----------------------------------------------------------
 * scimatplot
 * idem optional arguments ....
 *-----------------------------------------------------------*/

int int_matplot(Stack stack, int rhs, int opt, int lhs) 
{
  nsp_option opts_mp[] ={{ "axesflag",s_int,NULLOBJ,-1},
			 { "colminmax",mat_int,NULLOBJ,-1},
			 { "frameflag",s_int,NULLOBJ,-1},
			 { "leg",string,NULLOBJ,-1},
			 { "leg_pos",string,NULLOBJ,-1},
			 { "logflag",string,NULLOBJ,-1}, 
			 { "nax",mat_int,NULLOBJ,-1},
			 { "rect",realmat,NULLOBJ,-1},
			 { "remap",s_bool,NULLOBJ,-1},
			 { "strf",string,NULLOBJ,-1},
			 { "style",mat_int,NULLOBJ,-1},
			 { "zminmax",mat,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};
  
  BCG *Xgc;
  NspMatrix *z; 
  /* for 2d optional arguments; */
  int *nax, frame= -1, axes=-1, remap=FALSE;
  NspMatrix *Mistyle,*Mrect=NULL,*Mnax=NULL,*Mstyle=NULL,*Mzminmax=NULL,*Mcolminmax=NULL;
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;
  int leg_posi;
  int_types T[] = {realmat,new_opts, t_end} ;

  if ( rhs <= 0) {return   sci_demo(NspFname(stack),"m=[1,2;3,4];Matplot(m);",1);}

  if ( GetArgs(stack,rhs,opt,T,&z,&opts_mp,&axes,&Mcolminmax,&frame,&leg,&leg_pos,
	       &logflags,&Mnax,&Mrect,&remap,&strf,&Mstyle,&Mzminmax) == FAIL) return RET_BUG;

  if ( z->mn == 0) return 0;


  if (Mzminmax != NULL &&  Mzminmax->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",NspFname(stack),"zminmax");
      return RET_BUG;
    }

  if (Mcolminmax != NULL &&  Mcolminmax->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",NspFname(stack),"zminmax");
      return RET_BUG;
    }

  if ( check_zminmax(stack,NspFname(stack),"zminmax",Mzminmax)== FAIL ) return RET_BUG;
  if ( check_colminmax(stack,NspFname(stack),"colminmax",Mcolminmax)== FAIL) return RET_BUG;

  if ( int_check2d(stack,Mstyle,&Mistyle,z->mn,&strf,&leg,&leg_pos,&leg_posi,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  nsp_draw_matrix_1(Xgc,z->R,z->m,z->n,strf,rect,nax,remap,
		    (Mcolminmax == NULL) ? NULL :(int *)  Mcolminmax->R,
		    (Mzminmax == NULL) ? NULL : Mzminmax->R);
  if ( Mstyle != Mistyle)   nsp_matrix_destroy(Mistyle);

  return 0;
} 

/*-----------------------------------------------------------
 * Matplot1 
 *-----------------------------------------------------------*/

int int_matplot1(Stack stack, int rhs, int opt, int lhs)
{
  nsp_option opts_mp[] ={{ "axesflag",s_int,NULLOBJ,-1},
			 { "colminmax",mat_int,NULLOBJ,-1},
			 { "frameflag",s_int,NULLOBJ,-1},
			 { "leg",string,NULLOBJ,-1},
			 { "leg_pos",string,NULLOBJ,-1},
			 { "logflag",string,NULLOBJ,-1}, 
			 { "nax",mat_int,NULLOBJ,-1},
			 { "rect",realmat,NULLOBJ,-1},
			 { "remap",s_bool,NULLOBJ,-1},
			 { "strf",string,NULLOBJ,-1},
			 { "style",mat_int,NULLOBJ,-1},
			 { "zminmax",mat,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};

  /* for 2d optional arguments; */
  int *nax, frame= -1, axes=-1, remap=FALSE,leg_posi;
  NspMatrix *Mistyle,*Mrect=NULL,*Mnax=NULL,*Mstyle=NULL,*Mzminmax=NULL,*Mcolminmax=NULL;
  double *rect ; 
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;

  BCG *Xgc;
  NspMatrix *M,*Rect; 

  int_types T[] = {realmat, realmat, new_opts, t_end} ;

  if ( rhs <= 0) return sci_demo(NspFname(stack),"plot2d([0,10],[0,10],style=0);a=ones(50,50);a= 3*tril(a)+2*a;Matplot1(a,[4,4,9,9]);",1);

  if ( GetArgs(stack,rhs,opt,T,&M,&Rect,&opts_mp,&axes,&Mcolminmax,&frame,&leg,&leg_pos,
	       &logflags,&Mnax,&Mrect,&remap,&strf,&Mstyle,&Mzminmax) == FAIL) return RET_BUG;

  if (M->mn == 0) { return 0;} 

  if ( Rect->mn != 4) 
    {
      Scierror("%s: second argument should be of length 4\n",NspFname(stack));
      return RET_BUG;
    }

  if ( check_zminmax(stack,NspFname(stack),"zminmax",Mzminmax)== FAIL ) return RET_BUG;
  if ( check_colminmax(stack,NspFname(stack),"colminmax",Mcolminmax)== FAIL) return RET_BUG;

  if ( int_check2d(stack,Mstyle,&Mistyle,M->mn,&strf,&leg,&leg_pos,&leg_posi,
		   Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;

  Xgc=nsp_check_graphic_context();
  nsp_draw_matrix_2(Xgc,M->R, M->m,M->n,Rect->R,remap,
		    (Mcolminmax == NULL) ? NULL :(int *)  Mcolminmax->R,
		    (Mzminmax == NULL) ? NULL : Mzminmax->R);

  if ( Mstyle != Mistyle)  nsp_matrix_destroy(Mistyle);
		    
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

#ifdef WITH_CAIRO
extern Gengine Cairo_gengine; 
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
	  Scierror("%s: Rec driver does not exists, use xset('recording',1|0) to set or unset recording mode\n",
		   NspFname(stack));
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
      Scierror("%s: expecting zero or one argument\n",NspFname(stack));
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

BCG *nsp_check_graphic_context(void)
{
  if ( nsp_current_bcg != NULL ) 
    return nsp_current_bcg; /* Postscript or Xfig */
  else
    return check_graphic_window(); /* a graphic window */
} 


#ifdef NEW_GRAPHICS 

/*-----------------------------------------------------------
 *   xrect(x,y,w,h,opts) etendu a xrect([x,y,w,h],opts)
 *   opts: color =       ( line color )
 *         thickness =       ( line width )
 *         background=   ( also fill the rectangle)
 *-----------------------------------------------------------*/

int int_xarc(Stack stack, int rhs, int opt, int lhs)
{
  NspGrArc *arc;
  NspAxes *axe; 
  BCG *Xgc;
  double *val=NULL;
  int back=-1,color=-1,width=-1;
  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,6);
  if ( get_arc(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&width) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  axe=  nsp_check_for_axes(Xgc);
  if ( axe == NULL) return RET_BUG;
  if ((arc = nsp_grarc_create("pl",val[0],val[1],val[2],val[3],val[4],val[5],back,width,color,NULL))== NULL)
    return RET_BUG;
  /* insert the object */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) arc )== FAIL)
    return FAIL;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  return 0;
} 

int int_xfarc(Stack stack, int rhs, int opt, int lhs) 
{
  return int_xarc(stack,rhs,opt,lhs);
}

#else 

/*-----------------------------------------------------------
 * xarc(...)
 *-----------------------------------------------------------*/

static int int_xarc_G(Stack stack, int rhs, int opt, int lhs,char *name, void (*f)(BCG *Xgc,double arc[]))
{
  BCG *Xgc;
  double *val=NULL;
  if ( get_arc(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
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

#endif 

/*-----------------------------------------------------------
 * xarcs(arcs,[style])
 *-----------------------------------------------------------*/

#ifdef NEW_GRAPHICS 

int int_xarcs_G(Stack stack, int rhs, int opt, int lhs,int nrow)
{
  NspGraphic *gobj;
  NspAxes *axe; 
  BCG *Xgc;
  NspMatrix *arcs=NULL;
  NspMatrix *color=NULL;
  NspMatrix *background=NULL;
  NspMatrix *thickness=NULL;
  int i;
  nsp_option opts[] ={{ "background", realmat,NULLOBJ,-1},
		      { "color",realmat,NULLOBJ,-1},
		      { "thickness", realmat,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,1);
  if ((arcs = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  CheckRows(NspFname(stack),1,arcs, nrow) ;
  if ( get_optional_args(stack,rhs,opt,opts,&background,&color,&thickness) == FAIL) return RET_BUG;
  if ( background != NULL) 
    {
      CheckLength(NspFname(stack),opts[0].position, background, arcs->n);
    }
  if ( color != NULL) 
    {
      CheckLength(NspFname(stack),opts[1].position, color, arcs->n);
    }
  if ( thickness != NULL) 
    {
      CheckLength(NspFname(stack),opts[2].position, thickness, arcs->n);
    }
  Xgc=nsp_check_graphic_context();
  axe=  nsp_check_for_axes(Xgc);
  if ( axe == NULL) return RET_BUG;

  for ( i = 0 ; i < arcs->n ; i++)
    {
      double *val = arcs->R + i*(arcs->m);
      int icolor=-1,iback=-1,ithickness=-1;
      if ( color != NULL) icolor = color->R[i];
      if ( background != NULL) iback = background->R[i];
      if ( thickness != NULL)  ithickness = thickness->R[i];
      if ( nrow == 6 ) 
	{
	  if ((gobj =(NspGraphic *) nsp_grarc_create("pl",val[0],val[1],val[2],val[3],val[4],val[5],
						     iback,ithickness,icolor,NULL)) == NULL)
	    return RET_BUG;
	}
      else 
	{
	  if ((gobj =(NspGraphic *) nsp_grrect_create("pl",val[0],val[1],val[2],val[3],
						      iback,ithickness,icolor,NULL))== NULL)
	    return RET_BUG;
	}
      if ( nsp_list_end_insert( axe->obj->children,(NspObject *) gobj )== FAIL)
	return RET_BUG;
    }
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  return 0;
} 

int int_xarcs(Stack stack, int rhs, int opt, int lhs)
{
  return int_xarcs_G(stack,rhs,opt,lhs,6);
}

int int_xrects(Stack stack, int rhs, int opt, int lhs)
{
  return int_xarcs_G(stack,rhs,opt,lhs,4);
}

int int_xfarcs(Stack stack, int rhs, int opt, int lhs)
{
  return int_xarcs_G(stack,rhs,opt,lhs,6);
}


#else 

typedef  void (*f_xarcs)(BCG *Xgc,double vects[],int fillvect[], int n);


int int_xarcs_G(Stack stack, int rhs, int opt, int lhs,int row,int flag,char *name,f_xarcs f)
{
  BCG *Xgc;
  NspMatrix *arcs,*styles;
  CheckRhs(1,2);
  if ((arcs = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  CheckRows(NspFname(stack),1,arcs,row) ;

  if (rhs == 2) 
    {
      if ((styles= GetRealMatInt(stack,2))  == NULLMAT) return RET_BUG;
      CheckVector(NspFname(stack),2,styles);
      if ( styles->mn != arcs->n ) {
	Scierror("%s: first and second arguments have incompatible length\n",NspFname(stack));
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

int int_xrects(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc=nsp_check_graphic_context();
  return int_xarcs_G(stack,rhs,opt,lhs,4,0,"xrects",Xgc->graphic_engine->scale->drawrectangles);
}

int int_xfarcs(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc=nsp_check_graphic_context();
  return int_xarcs_G(stack,rhs,opt,lhs,6,1,"xfarcs",Xgc->graphic_engine->scale->fillarcs);
}


#endif 

/*-----------------------------------------------------------
 *   xarrows(nx,ny,[arsize=,style=])
 *-----------------------------------------------------------*/

#ifdef NEW_GRAPHICS 

int int_xarrows(Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *axe; 
  NspArrows *pl;
  BCG *Xgc;
  double arsize=-1.0 ;
  NspMatrix *x,*y,*Mstyle=NULL,*color;
  
  int_types T[] = {realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "arsize",s_double,NULLOBJ,-1},
		      { "style",mat_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&opts,&arsize,&Mstyle) == FAIL) return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x,y);
  if ( Mstyle != NULL) 
    {
      if ( Mstyle->mn != x->mn/2 ) {
	Scierror("%s: style has a wrong size (%d), expecting (%d)\n",NspFname(stack),Mstyle->mn,x->mn/2);
	return RET_BUG;
      }
    }
    
  Xgc=nsp_check_graphic_context();
  
  axe=  nsp_check_for_axes(Xgc);
  if ( axe == NULL) return RET_BUG;

  if ((x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(x)))== NULL) return RET_BUG;
  if ((y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(y)))== NULL) return RET_BUG;
  if ( color != NULL)
    {
      if ((color = (NspMatrix *) nsp_object_copy_and_name("color",NSP_OBJECT(Mstyle)))== NULL) 
	return RET_BUG;
    }
  /* passer color en entiers ? */
  if ((pl = nsp_arrows_create("ar",x,y,color,arsize,NULL))== NULL)
    return RET_BUG;
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) pl )== FAIL)
    return FAIL;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  return 0;
}

#else 

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

  CheckSameDims(NspFname(stack),1,2,nx,ny);
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
	    Scierror("%s: style has a wrong size (%d), expecting (%d)\n",NspFname(stack),Mstyle->mn, nx->mn/2  );
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

#endif 


#ifdef NEW_GRAPHICS 

int int_xsegs(Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *axe; 
  NspSegments *pl;
  BCG *Xgc;
  NspMatrix *x,*y,*Mstyle=NULL,*color;
  
  int_types T[] = {realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "style",mat_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&opts,&Mstyle) == FAIL) return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x,y);
  if ( Mstyle != NULL) 
    {
      if ( Mstyle->mn != x->mn/2 ) {
	Scierror("%s: style has a wrong size (%d), expecting (%d)\n",NspFname(stack),Mstyle->mn,x->mn/2);
	return RET_BUG;
      }
    }
    
  Xgc=nsp_check_graphic_context();
  
  axe=  nsp_check_for_axes(Xgc);
  if ( axe == NULL) return RET_BUG;

  if ((x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(x)))== NULL) return RET_BUG;
  if ((y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(y)))== NULL) return RET_BUG;
  if ( color != NULL)
    {
      if ((color = (NspMatrix *) nsp_object_copy_and_name("color",NSP_OBJECT(Mstyle)))== NULL) 
	return RET_BUG;
    }
  /* passer color en entiers ? */
  if ((pl = nsp_segments_create("ar",x,y,color,NULL))== NULL)
    return RET_BUG;
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) pl )== FAIL)
    return FAIL;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  return 0;
}

#else 

 
/*-----------------------------------------------------------
 *   xsegs(xv,yv,style=)
 *-----------------------------------------------------------*/

int int_xsegs(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int dstyle = -1;
  NspMatrix *nx,*ny,*Mstyle=NULL;
  int_types T[] = {realmat,realmat,new_opts, t_end} ;
  nsp_option opts[] ={{ "style",mat_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&nx,&ny,&opts,&Mstyle) == FAIL) return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,nx,ny);
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
	    Scierror("%s: style has a wrong size (%d), expecting (%d)\n",NspFname(stack),Mstyle->mn, nx->mn/2  );
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

#endif 

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
  CheckLength(NspFname(stack),2,l2,2);
  if ((l3=GetRealMat(stack,3)) ==  NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),3,l3,3);
  if ((l4=GetRealMat(stack,4)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),4,l4,2);

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
      CheckSameDims(NspFname(stack),1,2,l1,l2);
      if ((l3 = nsp_matrix_create(NVOID,'r',l1->m,l1->n))== NULLMAT ) return RET_BUG;
      if ((l4 = nsp_matrix_create(NVOID,'r',l1->m,l1->n))== NULLMAT ) return RET_BUG;
      scale_i2f(Xgc,l3->R,l4->R,(int *) l1->R,(int *) l2->R,l1->m*l1->n);
    }
  else   if ( strncmp(dir,"f2i",3) == 0) 
    {
      if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
      CheckSameDims(NspFname(stack),1,2,l1,l2);
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
      CheckLength(NspFname(stack),1,l1,4);
      if ((l3 = nsp_matrix_create(NVOID,'r',l1->m,l1->n)) == NULLMAT ) return RET_BUG;
      if ((l4 =nsp_mat_zeros(l1->m,l1->n)) == NULLMAT ) return RET_BUG;
      scale_f2wrect(Xgc,l1->R,l3->R);
    }
  else
    {
      Scierror("%s: dir=%s is wrong \n",NspFname(stack),dir);
      return RET_BUG;
    }
  
  NSP_OBJECT(l3)->ret_pos = 1;     StackStore(stack,(NspObject *) l3,rhs+1);
  NSP_OBJECT(l4)->ret_pos = 2;     StackStore(stack,(NspObject *) l4,rhs+2);
  if ( lhs >= 3 )  
    { 
      if ((l5 = nsp_matrix_create(NVOID,'r',1,4)) == NULLMAT ) return RET_BUG;
      for (i=0; i < 4 ; i++) l5->R[i] =  Xgc->scales->WIRect1[i];
      NSP_OBJECT(l5)->ret_pos = 3;     StackStore(stack,(NspObject *) l5,rhs+3);
    }
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
  double *val=NULL;
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
      CheckLength(NspFname(stack),1,M1,4);
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
      Scierror("%s: wrong number of rhs arguments (%d), rhs must be 1 or 4\n",NspFname(stack),rhs);
      return FAIL;
    }
  return OK;
}


#ifdef NEW_GRAPHICS 

/*-----------------------------------------------------------
 *   xrect(x,y,w,h,opts) etendu a xrect([x,y,w,h],opts)
 *   opts: color =       ( line color )
 *         thickness =       ( line width )
 *         background=   ( also fill the rectangle)
 *-----------------------------------------------------------*/

int int_xrect(Stack stack, int rhs, int opt, int lhs)
{
  NspGrRect *rect;
  NspAxes *axe; 
  BCG *Xgc;
  double *val=NULL;
  int back=-1,color=-1,width=-1;
  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,4);
  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&width) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  axe=  nsp_check_for_axes(Xgc);
  if ( axe == NULL) return RET_BUG;
  if ((rect = nsp_grrect_create("pl",val[0],val[1],val[2],val[3],back,width,color,NULL))== NULL)
    return RET_BUG;
  /* insert the object */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) rect )== FAIL)
    return FAIL;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  return 0;
} 

#else 

/*-----------------------------------------------------------
 *   xrect(x,y,w,h,opts) etendu a xrect([x,y,w,h],opts)
 *   opts: color =       ( line color )
 *         thickness =       ( line width )
 *         background=   ( also fill the rectangle)
 *-----------------------------------------------------------*/

int int_xrect(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  double *val=NULL;
  int cpat=0,cwidth=0,back,color,width;

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

#endif 

/*-----------------------------------------------------------
 *   xfrect(x,y,w,h,opts) etendu a xfrect([x,y,w,h],opts)
 *   opts: color =       ( fill color )
 *-----------------------------------------------------------*/

int int_xfrect(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  double *val=NULL;
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
  int clearq=FALSE,motion=FALSE,release=FALSE,winall=FALSE,istr=0,key=FALSE,win, iflag, button,imask,iw,i;
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

  if ( winall != TRUE &&  opts[4].obj != NULLOBJ) 
    {
      /* win=window_id was given */
      win = Max(win,0);
      Xgc = window_list_search(win);
      if ( Xgc == NULL ) 
	{
	  Scierror("%s:window %d does not exists\n",NspFname(stack),win);
	  return RET_BUG;
	}
    }
  else 
    {
      Xgc = window_list_get_first();
      if ( Xgc == NULL ) 
	{
	  Scierror("%s: No graphic window \n",NspFname(stack),win);
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
      iw=-1;
      Xgc->graphic_engine->scale->xclick_any(Xgc,buf,&button,&imask,&drep[1],&drep[2],&iw,iflag,motion,release,key,istr);
    }
  else 
    {
      Xgc->graphic_engine->scale->xclick(Xgc,buf,&button,&imask,&drep[1],&drep[2],iflag,motion,release,key,istr);
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
  /* FIXME nsp_plot_polar_grid(Xgc,&style); */
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
  CheckSameDims(NspFname(stack),1,2,l1,l2);

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
  CheckSameDims(NspFname(stack),1,2,l1,l2);

  if (rhs == 3) 
    {
      if ((l3=GetRealMatInt(stack,3)) == NULLMAT ) return RET_BUG;
      if ( l3->mn == l1->mn ) 
	{ 
	  CheckSameDims(NspFname(stack),1,3,l1,l3);
	  v1=2; /* interpolated shading */
	  if ( l3->m != 3 && l3->m != 4 ) 
	    {
	      Scierror("%s: interpolated shading only works for polygons of size 3 or 4\n",NspFname(stack));
	      return RET_BUG;
	    }
	} 
      else
	{
	  CheckVector(NspFname(stack),3,l3);
	  CheckDimProp(NspFname(stack),2,3, l3->mn != l2->n);
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

typedef enum {  
  xget_alufunction, xget_background, xget_clipoff, xget_clipping, xget_color, xget_colormap, 
  xget_dashes, xget_font , xget_font_size  , xget_foreground, xget_hidden3d, 
  xget_lastpattern, xget_line_mode , xget_line_style , xget_mark , xget_mark_size, xget_pattern, 
  xget_pixmap,  xget_recording, xget_thickness, xget_use_color, xget_viewport, xget_wdim, 
  xget_white, xget_window, xget_wpdim, xget_wpos, xget_wresize, xget_fpf, xget_auto_clear
} xget_enum;

static char *xget_Table[] = {  
  "alufunction", "background", "clipoff",  "clipping",  "color",  "colormap",
  "dashes",    "font",   "font size",    "foreground",  "hidden3d",
  "lastpattern",  "line mode",   "line style",   "mark",   "mark size", "pattern",
  "pixmap", "recording", "thickness",  "use color",  "viewport", "wdim",   "white",   "window",
  "wpdim",   "wpos",  "wresize", "fpf","auto clear",
  NULL
};

int int_xget(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *M;
  int rep, flagx=0,x1[10], val,i, cl[5],m3,vals[2];

  if ( rhs <= 0) { return sci_demo(NspFname(stack),"xsetm();",0);}

  CheckRhs(1,2);
  CheckLhs(0,1);

  if ((rep= GetStringInArray(stack,1,xget_Table,1)) == -1) return RET_BUG; 

  if (rhs == 2) { if (GetScalarInt(stack,2,&flagx) == FAIL) return RET_BUG;}

  Xgc=nsp_check_graphic_context();

  switch (rep) 
    {
    case xget_alufunction: 
      val = Xgc->graphic_engine->xget_alufunction(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_background:
      val = Xgc->graphic_engine->xget_background(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_clipoff:
    case xget_clipping:
      Xgc->graphic_engine->xget_clip(Xgc,cl);
      if ((M = nsp_matrix_create(NVOID,'r',1,5))== NULLMAT) return RET_BUG;
      for ( i = 0 ; i < 5 ; i++) M->R[i]= cl[i];
      StackStore(stack,(NspObject *) M,rhs+1);
      NSP_OBJECT(M)->ret_pos = 1;
      return 1;
      break;
    case xget_color:
      val = Xgc->graphic_engine->xget_pattern(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_colormap:
      /* flagx can be used if != 0 , to only get color flagx */
      flagx = Max(flagx,0);
      if ( flagx != 0) 
	{
	  /* just get one color */
	  if ((M = nsp_matrix_create(NVOID,'r',1,3))== NULLMAT) return RET_BUG;
	  Xgc->graphic_engine->xget_colormap(Xgc,&m3,M->R,flagx);
	  StackStore(stack,(NspObject *) M,rhs+1);
	  NSP_OBJECT(M)->ret_pos = 1;
	}
      else
	{
	  /* get all colors */
	  Xgc->graphic_engine->xget_colormap(Xgc,&m3,NULL,flagx); /*just to get m3 */
	  if ((M = nsp_matrix_create(NVOID,'r',m3,3))== NULLMAT) return RET_BUG;
	  Xgc->graphic_engine->xget_colormap(Xgc,&m3,M->R,flagx);
	  StackStore(stack,(NspObject *) M,rhs+1);
	  NSP_OBJECT(M)->ret_pos = 1;
	}
      return 1;
      break;
    case xget_dashes:
      val = Xgc->graphic_engine->xget_dash(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_font:
      Xgc->graphic_engine->xget_font(Xgc,vals);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case xget_font_size:
      Xgc->graphic_engine->xget_font(Xgc,x1);
      if ( nsp_move_double(stack,1,(double) x1[1]) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_foreground:
      val = Xgc->graphic_engine->xget_foreground(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_hidden3d:
      val = Xgc->graphic_engine->xget_hidden3d(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_lastpattern:
      val = Xgc->graphic_engine->xget_last(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_line_mode:
      val = Xgc->graphic_engine->xget_absourel(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_line_style:
      val = Xgc->graphic_engine->xget_dash(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_mark:
      Xgc->graphic_engine->xget_mark(Xgc,vals);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case xget_mark_size:
      Xgc->graphic_engine->xget_mark(Xgc,x1);
      if ( nsp_move_double(stack,1,(double) x1[1]) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_pattern:
      val = Xgc->graphic_engine->xget_pattern(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_pixmap:
      val = Xgc->graphic_engine->xget_pixmapOn(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_recording: 
      CheckRhs(1,1);
      val= Xgc->graphic_engine->xget_recording(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_thickness:
      val = Xgc->graphic_engine->xget_thickness(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_use_color:
      val = Xgc->graphic_engine->xget_usecolor(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_viewport:
      Xgc->graphic_engine->xget_viewport(Xgc,vals,vals+1);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case xget_wdim:
      Xgc->graphic_engine->xget_windowdim(Xgc,vals,vals+1);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case xget_white:
      val = Xgc->graphic_engine->xget_last(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_window:
      val = Xgc->graphic_engine->xget_curwin();
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_wpdim:
      Xgc->graphic_engine->xget_popupdim(Xgc,vals,vals+1);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case xget_wpos:
      Xgc->graphic_engine->xget_windowpos(Xgc,vals,vals+1);
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= vals[0]; M->R[1]=vals[1];
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case xget_wresize:
      val = Xgc->graphic_engine->xget_wresize(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_fpf:
      {
	NspSMatrix *S;
	if (( S=nsp_smatrix_create(NVOID,1,1,Xgc->graphic_engine->xget_fpf(Xgc),1))== NULLSMAT) return RET_BUG;
	MoveObj(stack,1,(NspObject *) S);
	return 1;
      }
      break;
    case xget_auto_clear:
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
  int v1=-1,opengl=FALSE,cairo=FALSE;
  NspMatrix *wdim=NULL,*wpdim=NULL,*viewport=NULL,*wpos=NULL;
  char *name=NULL, *file=NULL, *mode = NULL;
  static char *Table[] = {"d", "l", "n", "p", "k", NULL};
  /* just optionals arguments */
  int_types T[] = {new_opts, t_end} ;

  nsp_option opts[] ={{ "cairo",s_bool,NULLOBJ,-1},
		      { "dim",mat_int,NULLOBJ,-1},
		      { "file",string,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { "name",string,NULLOBJ,-1},
		      { "opengl",s_bool,NULLOBJ,-1},
		      { "popup_dim",mat_int,NULLOBJ,-1},
		      { "popup_pos",mat_int,NULLOBJ,-1},
		      { "viewport_pos",realmat,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&opts,&cairo,&wdim,&file,&mode,&name,
	       &opengl,&wpdim,&wpos,&viewport) == FAIL) return RET_BUG;

  if ( mode != NULL) 
    {
      int rep = is_string_in_array(mode,Table,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,mode,Table,"optional argument mode");
	  return RET_BUG;
	}
    }

  if (wdim != NULL && wdim->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",NspFname(stack),"dim");
      return RET_BUG;
    }
  if (wpdim != NULL &&  wpdim->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",NspFname(stack),"popup_dim");
      return RET_BUG;
    }

  if (viewport != NULL && viewport->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",NspFname(stack),"viewport_pos");
      return RET_BUG;
    }

  if (wpos != NULL && wpos->mn != 2 ) 
    {
      Scierror("%s:optional argument %s should be of size 2\n",NspFname(stack),"popup_pos");
      return RET_BUG;
    }

  if ( nsp_current_bcg != NULL) 
    {
      /* Postscript or Fig */
      nsp_current_bcg->graphic_engine->initgraphic(file,&v1, 
						   (wdim) ? (int*) wdim->R: NULL ,
						   (wpdim) ? (int*)wpdim->R: NULL,
						   (viewport) ? viewport->R : NULL,
						   (wpos) ? (int*)wpos->R : NULL,
						   mode[0],NULL);
    }
  else 
    {
      driver_initgraphic *initg = Gtk_gengine.initgraphic;
      if ( opengl == TRUE ) 
	{
#ifdef WITH_GTKGLEXT 
	  initg = GL_gengine.initgraphic;
#else 
	  Sciprintf("No opengl support in this version\n");
#endif 
	}
      if ( cairo  == TRUE ) 
	{
#ifdef WITH_CAIRO 
	  initg = Cairo_gengine.initgraphic;
#else 
	  Sciprintf("No cairo support in this version\n");
#endif 
	}
      initg(file,&v1,
	    (wdim) ? (int*)wdim->R: NULL ,
	    (wpdim) ? (int*)wpdim->R: NULL,
	    (viewport) ? viewport->R : NULL,
	    (wpos) ? (int*)wpos->R : NULL,
	    'e',NULL);
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
      Scierror("%s: xlfont to be done  \n",NspFname(stack));
      return RET_BUG;
      /* XXXXX 
	 char **S;
	 int m = 0;
	 C2F(dr1)("xgfont",C2F(cha1).buf,&m,sz,&v,&v,&v,&v,&dv,&dv,&dv,&dv);
	 if (m == 0) { LhsVar(1)=0; return 0;}
	 if (( S= (char **) MALLOC( (m+1)*sizeof(char*))) == NULL) 
	 {
	 Scierror(999,"%s: running out of memory \n",fname);
	 return 0;
	 }
	 count =0;
	 for ( i = 0 ; i < m ; i++) {
	 if ((S[i]= (char *) MALLOC((sz[i]+1)*sizeof(char))) == NULL) 
	 {
	 Scierror(999,"%s: running out of memory \n",fname);
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

  CheckSameDims(NspFname(stack),1,2,l1,l2);
  CheckSameDims(NspFname(stack),2,3,l2,l3);
  
  if ( l3->mn == 0) { return 0;}

  if (rhs >= 4) {   if (GetScalarInt(stack,4,&flagx) == FAIL) return RET_BUG;}
  if (rhs >= 5) 
    {   
      if ((l5=GetRealMat(stack,5)) == NULLMAT ) return RET_BUG;
      CheckSameDims(NspFname(stack),1,5,l1,l5);
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
 *  xpause(microsecs,events=TRUE or FALSE)
 *  make a pause for given microsecs dealing or not with Gtk events according 
 *  to the flag events.
 *-----------------------------------------------------------*/

extern void nsp_pause(int sec_time,int events);

int int_xpause(Stack stack, int rhs, int opt, int lhs)
{
  /* BCG *Xgc; */
  int sec=0,flag=FALSE;
  CheckRhs(-1,2);
  if (rhs >= 1){ if (GetScalarInt(stack,1,&sec) == FAIL) return RET_BUG;}
  if (rhs >= 2){ if (GetScalarBool(stack,2,&flag) == FAIL) return RET_BUG;}
  /* 
     Xgc=nsp_check_graphic_context();
     Xgc->graphic_engine->xpause(sec,flag);
  */
  nsp_pause(sec,flag);
  return 0;
} 

/*-----------------------------------------------------------
 *  xflush(); flush gtk-event 
 *-----------------------------------------------------------*/

/* FIXME */
extern void nsp_check_gtk_events(void);

int int_xflush(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  nsp_check_gtk_events();
  return 0;
}

/*-----------------------------------------------------------
 *  xpoly(xv,yv, close = %t|%f , color= , thickness= , mark= , 
 *        type = "lines" | "marks") 
 *    if mark is set then type is set to xmarks 
 *    thickness is only active for xlines 
 *    FIXME: mark_size should be added 
 *-----------------------------------------------------------*/

#ifdef NEW_GRAPHICS 

int int_xpoly(Stack stack, int rhs, int opt, int lhs)
{
  NspPolyline *pl;
  NspAxes *axe; 
  BCG *Xgc;
  int close=0,color=-1,mark=-1,mark_size=-1,fill_color=-1,thickness=-1;
  char *type;
  NspMatrix *x,*y;

  nsp_option opts[] ={
    { "close",s_bool,NULLOBJ,-1},
    { "color",s_int,NULLOBJ,-1},
    { "mark",s_int,NULLOBJ,-1},
    { "thickness",s_int,NULLOBJ,-1},
    { "type",string,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};
  
  CheckStdRhs(2,2);
  if ((x=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((y=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,x,y);
  if ( get_optional_args(stack,rhs,opt,opts,&close,&color,&mark,&thickness,&type) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  axe=  nsp_check_for_axes(Xgc);
  if ( axe == NULL) return RET_BUG;

  if ((x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(x)))== NULL) return RET_BUG;
  if ((y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(y)))== NULL) return RET_BUG;

  if ((pl = nsp_polyline_create("pl",x,y,close,color,mark,mark_size,fill_color,thickness,NULL))== NULL)
    return RET_BUG;
  /* insert the polyline */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) pl )== FAIL)
    return FAIL;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  return 0;
}

#else 

int int_xpoly(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int close=0,color,mark,thick;
  int xmark[2],cmark=0,cthick=0,ccolor=0;
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
  
  CheckStdRhs(2,2);
  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,l1,l2);
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
	  cmark=xmark[0];
	  Xgc->graphic_engine->scale->xset_mark(Xgc,mark,xmark[1]);
	  dtype = xmarks; 
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
	    Scierror("%s: type must be \"lines\" or \"marks\"\n",NspFname(stack));
	    return RET_BUG;
	  }
	  if ( opts[2].obj != NULLOBJ && strncmp(type,"lines",5) == 0 )
	    {
	      Sciprintf("type is set to \"marks\" since mark is set \n",NspFname(stack));
	      dtype = xmarks;
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

#endif


/*-----------------------------------------------------------
 * xpoly_clip(xv,yv,clip_rect,opts)
 * test interface for xpoly with clipping 
 *-----------------------------------------------------------*/

int int_xpoly_clip(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int close=0,color,mark,thick;
  int cthick=0,ccolor=0;
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
  CheckSameDims(NspFname(stack),1,2,l1,l2);

  CheckLength(NspFname(stack),3,l3,4);
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

#ifdef NEW_GRAPHICS 

int int_xpolys(Stack stack, int rhs, int opt, int lhs)
{
  int close=0,color=-1,mark=-1,mark_size=-1,fill_color=-1,thickness=-1,i;
  NspMatrix *x,*y,*style=NULL;
  NspPolyline *pl;
  NspAxes *axe; 
  BCG *Xgc;
  CheckRhs(2,3);

  if ((x=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((y=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,x,y);
  
  if (rhs == 3) 
    {
      if ((style=GetRealMatInt(stack,3)) == NULLMAT ) return RET_BUG;
      CheckVector(NspFname(stack),3,style); 
      CheckDimProp(NspFname(stack),1,3, style->mn < x->n);
    }
  Xgc=nsp_check_graphic_context();
  axe=  nsp_check_for_axes(Xgc);
  if ( axe == NULL) return RET_BUG;

  for ( i = 0 ; i < x->n ; i++)
    {
      NspMatrix *xp,*yp;
      if ((xp= nsp_matrix_create_from_array("x",1,x->m,x->R + x->m*i,NULL))== NULL) return RET_BUG;
      if ((yp= nsp_matrix_create_from_array("x",1,y->m,y->R + y->m*i,NULL))== NULL) return RET_BUG;
      if ( style != NULL)
	{
	  if ( style->I[i] <= 0) 
	    mark = - style->I[i];
	  else
	    color = style->I[i];
	}
      if ((pl = nsp_polyline_create("pl",xp,yp,close,color,mark,mark_size,fill_color,thickness,NULL))== NULL)
	return RET_BUG;
      mark = color=-1;
      /* insert the polyline */
      if ( nsp_list_end_insert( axe->obj->children,(NspObject *) pl )== FAIL)
	return RET_BUG;
    }
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig);
  return 0;
}

#else 

int int_xpolys(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *l1,*l2,*l3;

  CheckRhs(2,3);

  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,l1,l2);

  if (rhs == 3) 
    {
      if ((l3=GetRealMatInt(stack,3)) == NULLMAT ) return RET_BUG;
      CheckVector(NspFname(stack),3,l3); 
      CheckDimProp(NspFname(stack),1,3, l3->mn < l1->n);
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

#endif 


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

/* FIXME:  Attention il faut des xset_1 ici */ 

typedef enum  { 
  xset_alufunction, xset_background, xset_clipoff, xset_clipping, xset_color, xset_colormap
  , xset_dashes  , xset_default, xset_default_colormap
  , xset_font , xset_font_size , xset_foreground, xset_hidden3d
  , xset_lastpattern, xset_line_mode , xset_line_style , xset_mark , xset_mark_size, xset_pattern
  , xset_pixmap ,xset_recording, xset_thickness, xset_use_color, xset_viewport, xset_wdim , xset_white , xset_window
  , xset_wpdim , xset_wpos, xset_wresize, xset_wshow, xset_wwpc, xset_fpf, xset_auto_clear, xset_clipgrf
} xset_enum ;

static char *xset_Table[] = { 
  "alufunction", "background", "clipoff", "clipping", "color", "colormap",
  "dashes",     "default",  "default_colormap",
  "font",   "font size",    "foreground",  "hidden3d",
  "lastpattern",  "line mode",   "line style",   "mark",   "mark size", "pattern",
  "pixmap", "recording",  "thickness",  "use color",  "viewport", "wdim",   "white",   "window",
  "wpdim",   "wpos",  "wresize",  "wshow",  "wwpc", "fpf","auto clear", "clipgrf", NULL
};

int int_xset(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc = NULL;
  static char *auto_clear_values[]= {"off","on",NULL};
  char *info;
  int rep,val,val1, i, mark[2], font[2];
  double cl[4];
  NspMatrix *M;

  if (rhs <= 0) {return sci_demo(NspFname(stack),"xsetm();",0);}

  CheckRhs(1,6);
  CheckLhs(0,1);

  if ((rep= GetStringInArray(stack,1,xset_Table,1)) == -1) return RET_BUG; 

  switch (rep) 
    {
    case xset_alufunction:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_alufunction1(Xgc,val);
      break;
    case xset_background:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_background(Xgc,val);
      break;
    case xset_clipoff:
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_unclip(Xgc);
      break;
    case xset_clipping:
      if ( rhs == 2 ) 
	{
	  if (( M = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
	  CheckLength(NspFname(stack),2,M,4);
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
    case xset_color:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_pattern(Xgc,val);
      break;
    case xset_colormap:
      CheckRhs(2,2);
      if ( (M = GetRealMat(stack,2)) == NULLMAT) return RET_BUG; 
      CheckCols(NspFname(stack),2,M,3);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_colormap(Xgc,M->m,M->R);
      break;
    case xset_dashes:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_dash(Xgc,val);
      break;
    case xset_default:
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_default(Xgc);
      break;
    case xset_default_colormap:
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_default_colormap(Xgc);
      break;
    case xset_font:
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
    case xset_font_size:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->xget_font(Xgc,font);
      font[1]=val;
      Xgc->graphic_engine->scale->xset_font(Xgc,font[0],font[1]);
      break;
    case xset_foreground:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_foreground(Xgc,val);
      break;
    case xset_hidden3d:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_hidden3d(Xgc,val);
      break;
    case xset_lastpattern:
      CheckRhs(1,1);
      Scierror("lastpattern cannot be set \n");
      return RET_BUG;
      break;
    case xset_line_mode:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_absourel(Xgc,val);
      break;
    case xset_line_style:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_dash(Xgc,val);
      break;
    case xset_mark:
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
    case xset_mark_size:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->xget_mark(Xgc,mark);
      mark[1]=val;
      Xgc->graphic_engine->scale->xset_mark(Xgc,mark[0],mark[1]);
      break;
    case xset_pattern:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_pattern(Xgc,val);
      break;
    case xset_pixmap:
      CheckRhs(2,2);
      Xgc=nsp_check_graphic_context();
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc->graphic_engine->scale->xset_pixmapOn(Xgc,val);
      break;
    case xset_recording: 
      CheckRhs(2,2);
      Xgc=nsp_check_graphic_context();
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc->graphic_engine->xset_recording(Xgc,(val != 0 ) ? 1 : 0);
      break;
    case xset_thickness:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_thickness(Xgc,val);
      break;
    case xset_use_color:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_usecolor(Xgc,val);
      break;
    case xset_viewport:
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_viewport(Xgc,val,val1);
      break;
    case xset_wdim:
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_windowdim(Xgc,val,val1);
      break;
    case xset_white:
      CheckRhs(1,1);
      Scierror("white cannot be set \n");
      return RET_BUG;
      break;
    case xset_window:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if ((Xgc = window_list_get_first()) != NULL) 
	Xgc->graphic_engine->xset_curwin(Max(val,0),TRUE);
      else 
	Xgc= set_graphic_window(Max(val,0));
      break;
    case xset_wpdim:
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_popupdim(Xgc,val,val1);
      break;
    case xset_wpos:
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_windowpos(Xgc,val,val1);
      break;
    case xset_wresize:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG; 
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_wresize(Xgc,val);
      break;
    case xset_wshow:
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_show(Xgc);
      break;
    case xset_wwpc:
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_pixmapclear(Xgc);
      break;
    case xset_fpf:
      CheckRhs(2,2);
      if ((info = GetString(stack,2)) == (char*)0) return RET_BUG;
      Xgc=nsp_check_graphic_context();
      if ( strlen(info)== 0) 
	Xgc->graphic_engine->scale->xset_fpf_def(Xgc);
      else 
	Xgc->graphic_engine->scale->xset_fpf(Xgc,info);
      return 0;
      break;
    case xset_auto_clear:
      CheckRhs(2,2);
      Xgc=nsp_check_graphic_context();
      if ( (rep = GetStringInArray(stack,2,auto_clear_values,1)) == -1 ) return RET_BUG;
      Xgc->graphic_engine->scale->xset_autoclear(Xgc,rep);
      return 0;
      break;
    case xset_clipgrf:
      CheckRhs(1,1);
      Xgc=nsp_check_graphic_context();
      Xgc->graphic_engine->scale->xset_clipgrf(Xgc);
      break;
    }
  return 0;
}

/*
 *
 * test 
 */

static int int_xtest(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc = NULL;
  CheckRhs(0,0);
  CheckLhs(0,1);
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->scale->xset_test(Xgc);
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

  if (( S = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;

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

  if ( S->m == 1 )
    {
      /* one rotated string */
      Xgc->graphic_engine->scale->displaystring(Xgc,S->S[0],x,y,flagx,angle);
    }
  else 
    {
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

  if ( rhs <= 0) {  return sci_demo(NspFname(stack),"x=(1:10)';plot2d(x,x);xtitle(['Titre';'Principal'],'x legend ','y legend');",1);  }

  CheckRhs(1,3);
  Xgc=nsp_check_graphic_context();

  for ( narg = 1 ; narg <= rhs ; narg++) 
    {
      nsp_string str;
      if (( S = GetSMatUtf8(stack,narg)) == NULLSMAT) return RET_BUG;
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
  nsp_string str;
  NspSMatrix *S;

  CheckRhs(5,6);
  if (GetScalarDouble(stack,1,&x) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,2,&y) == FAIL) return RET_BUG;
  if ((S = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) return 0; 
  if (GetScalarDouble(stack,4,&w) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,5,&hx) == FAIL) return RET_BUG;

  if (rhs == 6) {
    if ((info = GetString(stack,6)) == (char*)0) return RET_BUG;
    if ( strncmp(info,"fill",4) == 0) 
      fill =1;
    else 
      {
	Scierror("%s: optional argument has a wrong value 'fill' expected\n",NspFname(stack));
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
  nsp_string str;
  NspSMatrix *S;
  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "fill",s_bool,NULLOBJ,-1},
		      { "frame",s_int,NULLOBJ,-1},
		      { "frame_width",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(2,7);

  if (( M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,4);

  if ((S = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
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

  if ((S = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;
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
  NspObject  *status;
  BCG *Xgc;
  int rep,rec;
  NspMatrix *M;
  static double  rect_def[4] = { 0,0,10,10}, ebox_def[6] = {0,1,0,1,0,1};
  static int iflag_def[4] = { 0,0,0,0 };
  static int aint_def[4] = { 0,0,0,0 };
  int iscflag[3];
  static int flagx_def[3] = { 1,1,1} ;
  int *iflag = iflag_def,*aint = aint_def, *flagx= flagx_def,num;
  double alpha = 35.0,theta = 45.0,  *rect = rect_def ,*ebox = ebox_def ;

  static char *xtape_Table[] = {  "on","clear","replay","replaysc","replayna","off",  NULL };

  CheckRhs(1,7);
  Xgc=nsp_check_graphic_context();

  /* first argument is a string in xtape_table */

  if ((rep= GetStringInArray(stack,1,xtape_Table,1)) == -1) return RET_BUG; 

  switch (rep) 
    {
    case 0 : /* on */ 
      CheckRhs(1,1);
      rec= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      if ((status= nsp_new_string_obj(NVOID,(rec== TRUE) ? "on": "off",-1))== NULLOBJ)
	return RET_BUG;
      MoveObj(stack,1, status);
      return 1;
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
      CheckRhs(2,4);
      if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
      /*     s'il n'y a que trois argument le 3ieme est rect[4] */
      iscflag[0]=1; iscflag[1]=iscflag[2]=0;
      if ((M= GetRealMat(stack,3))  == NULLMAT) return RET_BUG;
      CheckLength(NspFname(stack),3,M,4); rect =  M->R;
      if ( rhs >= 4 ) 
	{
	  iscflag[1]=1;
	  if ((M= GetRealMatInt(stack,4))  == NULLMAT) return RET_BUG;
	  CheckLength(NspFname(stack),4,M,4); aint = (int*) M->R;
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
	CheckLength(NspFname(stack),5,M,4); iflag = (int*) M->R;
      }
      if ( rhs >= 6 ) { 
	if ((M= GetRealMatInt(stack,6))  == NULLMAT) return RET_BUG;
	CheckLength(NspFname(stack),6,M,3); flagx = (int*) M->R;
      }
      if ( rhs >= 7 ) { 
	if ((M= GetRealMat(stack,6))  == NULLMAT) return RET_BUG;
	CheckLength(NspFname(stack),7,M,6); ebox =  M->R;
      }
      Xgc->graphic_engine->tape_replay_new_angles(Xgc,num,iflag,flagx,&theta,&alpha,ebox); /*  */
      break;
    case 5: /* off */
      CheckRhs(1,1);
      rec= Xgc->graphic_engine->xget_recording(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,FALSE);
      if ((status= nsp_new_string_obj(NVOID,(rec== TRUE) ? "on": "off",-1))== NULLOBJ)
	return RET_BUG;
      MoveObj(stack,1,status);
      return 1;
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
      CheckLength(NspFname(stack),1,M,4);
      wrect = M->R;

      if (rhs >= 2) {
	if ((M= GetRealMat(stack,2))  == NULLMAT) return RET_BUG;
	CheckLength(NspFname(stack),2,M,4);
	frect = M->R;
      }
      if (rhs >= 3) { 
	if ((logflag = GetString(stack,3)) == (char*)0) return RET_BUG;
	if ( strlen(logflag) != 2 ) {
	  Scierror("%s: third argument has a wrong length %d expecting (%d)\n",NspFname(stack),strlen(logflag),2 );
	  return RET_BUG;
	}
      }
    }
  else 
    {
      NspMatrix *Marect=NULL,*Mwrect=NULL,*Mfrect=NULL;
      int_types T[] = {new_opts, t_end} ;

      nsp_option opts[]={{"arect",realmat,NULLOBJ,-1},
			 {"frect",realmat,NULLOBJ,-1},
			 {"logflag",string,NULLOBJ,-1},
			 {"wrect",realmat,NULLOBJ,-1},
			 {NULL,t_end,NULLOBJ,-1},};

      if ( GetArgs(stack,rhs,opt,T,&opts,&Marect,&Mfrect,&logflag,&Mwrect) == FAIL) return RET_BUG;

      if ( Marect != NULL) {
	arect = Marect->R;CheckLength(NspFname(stack),opts[0].position,Marect,4);
      }

      if ( Mfrect != NULL) {
	frect = Mfrect->R;CheckLength(NspFname(stack),opts[1].position,Mfrect,4);
      }

      if ( Mwrect != NULL) {
	wrect = Mwrect->R;CheckLength(NspFname(stack),opts[3].position,Mwrect,4);
      }
      
      if ( logflag != logflag_def ) {
	if ( strlen(logflag) != 2 ) {
	  Scierror("%s: logflag argument has a wrong length %d expecting (%d)\n",NspFname(stack),strlen(logflag),2 );
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
 * fec(x,y,triangles,func,...)
 * from the modified version by Bruno 1/2/2001
 *-----------------------------------------------------------*/


int int_fec(Stack stack, int rhs, int opt, int lhs)
{
  nsp_option opts_fec[] ={{ "axesflag",s_int,NULLOBJ,-1},
			  { "colminmax",mat_int,NULLOBJ,-1},
			  { "colout",mat_int,NULLOBJ,-1},
			  { "frameflag",s_int,NULLOBJ,-1},
			  { "leg",string,NULLOBJ,-1},
			  { "leg_pos",string,NULLOBJ,-1},
			  { "logflag",string,NULLOBJ,-1}, 
			  { "mesh", s_bool,NULLOBJ,-1},
			  { "nax",mat_int,NULLOBJ,-1},
			  { "rect",realmat,NULLOBJ,-1},
			  { "strf",string,NULLOBJ,-1},
			  { "style",mat_int,NULLOBJ,-1},
			  { "zminmax",mat,NULLOBJ,-1},
			  { NULL,t_end,NULLOBJ,-1}};

  BCG *Xgc;
  NspMatrix *Mistyle,*x,*y,*Tr,*F,*Mrect=NULL,*Mnax=NULL,*Mzminmax=NULL,*Mcolminmax=NULL,*Mstyle=NULL,*Mcolout=NULL;
  double *rect;
  int *nax,nnz= 10, frame= -1, axes=-1,mesh = FALSE, leg_posi;
  char *strf=NULL, *leg=NULL, *leg_pos = NULL,*logflags=NULL;
  int_types T[] = {realmat,realmat,realmat,realmat,new_opts, t_end} ;
  /* N.n =  4 ; N.names= Names, N.types = Topt, N.objs = Tab; */

  if ( rhs <= 0) { return sci_demo (NspFname(stack)," exec(\"SCI/demos/fec/fec.ex1\");",1);}
  
  if ( GetArgs(stack,rhs,opt,T,&x,&y,&Tr,&F,&opts_fec,&axes,&Mcolminmax,&Mcolout,&frame,
	       &leg,&leg_pos,&logflags,&mesh,&Mnax,&Mrect,&strf,&Mstyle,&Mzminmax) == FAIL) return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x,y);
  CheckSameDims(NspFname(stack),1,4,x,F);

  if ( Tr->n != 5) {
    Scierror("%s: triangles have %d columns,expecting 5\n",NspFname(stack),Tr->n);
    return RET_BUG;
  }

  if ( x->mn == 0 || Tr->m == 0) { return 0;} 

  if ( check_zminmax(stack,NspFname(stack),"zminmax",Mzminmax)== FAIL ) return RET_BUG;
  if ( check_colminmax(stack,NspFname(stack),"colminmax",Mcolminmax)== FAIL) return RET_BUG;
  if ( check_colout(stack,NspFname(stack),"colout",Mcolout)== FAIL) return RET_BUG;

  if ( int_check2d(stack,Mstyle,&Mistyle,nnz,&strf,&leg,&leg_pos,&leg_posi,Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0) 
    return RET_BUG;

  Xgc=nsp_check_graphic_context();
  nsp_gwin_clear(Xgc);
  nsp_fec(Xgc,x->R,y->R,Tr->R,F->R,&x->mn,&Tr->m,strf,leg,rect,nax,
	  (Mzminmax == NULL) ? NULL : Mzminmax->R,
	  (Mcolminmax == NULL) ? NULL :(int *)  Mcolminmax->R, 
	  (Mcolout == NULL) ? NULL :(int *)  Mcolout->R,
	  mesh);

  if ( Mstyle != Mistyle)     nsp_matrix_destroy(Mistyle);
	  
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
  int button,mask;
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
  Xgc->graphic_engine->scale->xgetmouse(Xgc,"xv",&button,&mask,&x,&y,iflag,motion,release,key);
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
  BCG *Xgc=NULL;
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

static int int_export_G(Stack stack, int rhs, int opt, int lhs,char *export_format)
{
  int win_id,rep=1,color=-1;
  char *filename= NULL, *mode = NULL;
  static char *Table[] = {"d", "l", "n", "p", "k", NULL};
  int_types T[] = {s_int,string, new_opts, t_end} ;
  nsp_option opts[] ={{ "color",s_bool,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&win_id,&filename,&opts,&color,&mode) == FAIL) return RET_BUG;
  if ( mode != NULL) 
    {
      rep = is_string_in_array(mode,Table,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,mode,Table,"optional argument mode");
	  return RET_BUG;
	}
    }
  if ( export_format == NULL )
    {
      char *extension;
      int frep = 0;
      static char *Etable[] = 
	{".svg", ".pdf", ".eps", ".ps", ".fig", ".png", NULL};
      static char *Ftable[] = 
	{"cairo-svg", "cairo-pdf", "cairo-ps", "cairo-ps", "Fig", "cairo-png", NULL};
      extension = nsp_get_extension(filename);
      if ( extension == NULL) 
	{
	  Scierror("Error: no extension given to filename \"%s\" and no format specified in function %s\n",
		   filename,NspFname(stack));
	  return RET_BUG;
	}
      frep = is_string_in_array(extension,Etable,1);
      if ( frep < 0 ) 
	{
	  string_not_in_array(stack,extension,Etable,"extension of filename argument");
	  return RET_BUG;
	}
      export_format = Ftable[frep];
    }
  scig_export(filename,win_id,color,export_format,Table[rep][0]);
  return 0;
}

/* backward compatibility */

int int_xs2ps_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"Pos");
}

int int_xs2fig(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"Fig");
}

int int_xs2pdf(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"cairo-pdf");
}

int int_xs2png(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"cairo-png");
}

int int_xs2svg(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"cairo-svg");
}

int int_xs2ps(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"cairo-ps");
}

int int_xexport(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,NULL);
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


static int check_xy(const char *fname,char dir,int mn,int xpos,NspMatrix *Mx,int ypos,NspMatrix *My,int *ntics);

int int_nxaxis(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char dir = 'l', *format = NULL, tics = 'v';
  char **val = NULL;
  int fontsize = -1, sub_int=2, seg_flag = 1,textcolor = -1,ticscolor=-1;
  double *x = NULL,*y = NULL;
  int nx=0,ny=0,ntics;
  NspSMatrix *S;
  NspMatrix *Mx=NULLMAT,*My= NULLMAT;
  char *sdir=NULL,*stics=NULL;
  int_types T[] = {realmat,realmat,realmat,realmat,new_opts, t_end} ;
  nsp_option opts[]={{"dir",string,NULLOBJ,-1},
		     {"fontsize",s_int,NULLOBJ,-1},
		     {"format_n",string,NULLOBJ,-1},
		     {"seg",s_int,NULLOBJ,-1},
		     {"sub_int",s_int,NULLOBJ,-1},
		     {"textcolor",s_int,NULLOBJ,-1},
		     {"tics",string,NULLOBJ,-1},
		     {"ticscolor",s_int,NULLOBJ,-1},
		     {"val",smat,NULLOBJ,-1},
		     {"x",realmat,NULLOBJ,-1},
		     {"y",realmat,NULLOBJ,-1},
		     {NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&opts,&sdir,&fontsize,&format,&seg_flag,
	       &sub_int,&textcolor,&stics,&ticscolor,&S,&Mx,&My) == FAIL) return RET_BUG;

  Xgc=nsp_check_graphic_context();

  if ( sdir != NULL  ) 
    { 
      if ( strlen(sdir) != 1 ) {
	Scierror("%s: optional string dir must contain one character\n",NspFname(stack));
	return RET_BUG;
      }
      dir = sdir[0];
    } 

  if ( stics != NULL ) 
    { 
      if ( strlen(stics) != 1 ) {
	Scierror("%s: optional string tics must contain one character\n",NspFname(stack));
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
      if ( check_xy(NspFname(stack),dir,3,opts[9].position,Mx, opts[10].position,My,&ntics)==0) 
	return 0;
      break;
    case 'i' :
      if ( check_xy(NspFname(stack),dir,4,opts[9].position,Mx, opts[10].position,My,&ntics)==0) 
	return 0;
      break;
    case 'v' :
      if ( check_xy(NspFname(stack),dir,-1,opts[9].position,Mx, opts[10].position,My,&ntics)==0) 
	return 0;
      break;
    default :
      Scierror("%s: tics has a wrong value \"%c\" should be one of \"r\",\"v\" and \"i\" \n", 
	       NspFname(stack),dir);
      return RET_BUG;
    }

  if ( val != 0) 
    {
      /** sciprint("nombre de tics %d\n",ntics); **/
      CheckLength(NspFname(stack), opts[8].position, S,ntics);
    }

  sci_axis(Xgc,dir,tics,x,&nx,y,&ny,val,sub_int,format,fontsize,textcolor,ticscolor,'n',seg_flag);
  return 0;
}

static int check_xy(const char *fname,char dir,int mn,int xpos,NspMatrix *Mx,int ypos,NspMatrix *My,int *ntics)
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
      Scierror("%s: dir has a wrong value \"%c\" should be one of \"u\",\"d\",\"r\" and \"l\"\n", 
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
#ifdef WITH_GTKHTML
  int i;
  char *str[3]={NULL,NULL,NULL};
  CheckRhs(0,1);
  for (i=0; i < rhs ; i++) {
    if ((str[i] = GetString(stack,i+1)) == (char*)0) return RET_BUG;
  }
  Sci_Help(NULL,NULL,str[0]);
#endif 
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
 * Utility function for demo 
 * XXXX
 *-----------------------------------------------------------*/

static int sci_demo (const char *fname,char *code,int flag) 
{
  int rep;
  if ( flag == 1) 
    {
      Sciprintf("Demo of %s\n",fname);
      Sciprintf("%s\n",code);
    }
  rep =nsp_parse_eval_from_string(code,FALSE,FALSE,FALSE,TRUE);
  if  ( rep < 0 ) 
    {
      Scierror("Error: during evaluation of %s\n",code);
      return RET_BUG;
    }
  return 0;
}

extern int int_bsearch(Stack stack, int rhs, int opt, int lhs);


/*-----------------------------------------------------------
 * feval en preparation 
 *-----------------------------------------------------------*/

typedef struct _feval_data feval_data;
 
struct _feval_data
{
  NspList *args;
  NspMatrix *x,*y;
  NspObject *func;
};

static int feval_prepare(int dim,NspObject *f,NspList *args,feval_data *feval)
{
  if (( feval->func =nsp_object_copy(f)) == NULL) return RET_BUG;
  if (( nsp_object_set_name(feval->func,"feval_f")== FAIL)) return RET_BUG;
  if ( args != NULL ) 
    {
      if (( feval->args = nsp_list_copy(args)) == NULL ) return RET_BUG;
      if (( nsp_object_set_name((NspObject *) feval->args,"arg")== FAIL)) return RET_BUG;
    }
  else 
    {
      feval->args = NULL;
    }
  if ((feval->x = nsp_matrix_create("x",'r',1,1))== NULL) return RET_BUG;
  if ( dim == 2 ) 
    {
      if ((feval->y = nsp_matrix_create("y",'r',1,1))== NULL) return RET_BUG;
    }
  else 
    {
      feval->y = NULL;
    }
  return OK;
}

static void feval_clean(int dim,feval_data *feval)
{
  if ( feval->args != NULL) nsp_list_destroy(feval->args);
  nsp_object_destroy(&feval->func);
  nsp_matrix_destroy(feval->x);
  if ( dim == 2 ) nsp_matrix_destroy(feval->y);
}

static int feval_system(int dim,double x,double y,double * val,feval_data *feval)
{
  NspObject *targs[4];/* arguments to be transmited to feval->func */
  NspObject *nsp_ret;
  int nret = 1,nargs = 0;
  targs[nargs]= NSP_OBJECT(feval->x); nargs++;
  feval->x->R[0] = x;
  if ( dim == 2 ) 
    {
      targs[nargs]= NSP_OBJECT(feval->y); 
      feval->y->R[0]= y;
      nargs++;
    }
  if (feval->args != NULL ) 
    {
      targs[nargs]= NSP_OBJECT(feval->args);
      nargs++;
    }
  /* FIXME : a changer pour metre une fonction eval standard */
  if ( nsp_gtk_eval_function((NspPList *)feval->func ,targs,nargs,&nsp_ret,&nret)== FAIL) 
    return FAIL;
  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' 
      && ((NspMatrix *) nsp_ret)->mn==1 ) 
    {
      Mat2double((NspMatrix *) nsp_ret);
      *val= ((NspMatrix *) nsp_ret)->R[0];
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else 
    {
      Scierror("Error: evaluation failed in feval \n");
      return FAIL;
    }
  return OK;
}


int int_feval( Stack stack, int rhs, int opt, int lhs)
{
  int i,j,dim=1;
  NspMatrix *M;
  feval_data feval;
  NspObject *f= NULL;
  NspList *args=NULL;
  NspMatrix *x,*y;
  int_types T1[] = {realmat,obj,new_opts, t_end} ;
  int_types T2[] = {realmat,realmat,obj,new_opts, t_end} ;

  nsp_option opts[] ={
    { "args",list,  NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };

  if ( rhs - opt == 3 )
    {
      if ( GetArgs(stack,rhs,opt,T2,&x,&y,&f,&opts,&args) == FAIL) return RET_BUG;
      dim=2;
    }
  else if ( rhs - opt == 2 )
    {
      if ( GetArgs(stack,rhs,opt,T1,&x,&f,&opts,&args) == FAIL) return RET_BUG;
      dim=1;
    }
  else 
    {
      Scierror("%s: expecting 2 or 3 non optional arguments found %d\n",NspFname(stack),rhs-opt );
      return RET_BUG; 
    }

  if ( IsNspPList(f) == FALSE  )
    {
      if ( rhs-opt == 2) 
	Scierror("%s: second argument should be a function\n",NspFname(stack));
      else 
	Scierror("%s: third argument should be a function\n",NspFname(stack));
      return RET_BUG;
    }

  if ( feval_prepare(dim,f,args,&feval) == FAIL ) 
    return RET_BUG;
  if ( dim == 1 )
    {
      if ((M = nsp_matrix_create(NVOID,'r',x->m,x->n))== NULLMAT) return RET_BUG;
      for ( i = 0 ; i < x->mn ; i++) 
	{
	  if ( feval_system(dim,x->R[i],0,&M->R[i],&feval)==FAIL) 
	    return RET_BUG;
	}
    }
  else 
    {
      if ((M = nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULLMAT) return RET_BUG;
      for ( i = 0 ; i < x->mn ; i++) 
	for ( j = 0 ; j < y->mn ; j++) 
	  {
	    if ( feval_system(dim,x->R[i],y->R[j],&M->R[i+M->m*j],&feval)==FAIL) 
	      return RET_BUG;
	  }
    }
  feval_clean(1,&feval);
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}


#include "nsp/gtk/gdkimage.h"
#include "nsp/gtk/gdkpixbuf.h"

extern GdkImage* nsp_get_image(BCG *Xgc) ;
extern GdkPixbuf* nsp_get_pixbuf(BCG *Xgc) ;

/* get the content of a graphic window as an image. 
 *
 */

int int_get_image( Stack stack, int rhs, int opt, int lhs)
{
  NspObject *ret1;
  GdkImage *img;
  BCG *Xgc;
  CheckRhs(0,0);
  CheckLhs(0,1);
  Xgc=nsp_check_graphic_context();
  if ( Xgc->private == NULL) 
    {
      Scierror("Error: %s Current graphic driver is not attached to a drawable\n",NspFname(stack));
      return RET_BUG;
    }
  if ((img =  nsp_get_image(Xgc))== NULL) return RET_BUG;
  nsp_type_gdkimage = new_type_gdkimage(T_BASE);
  if ((ret1 = (NspObject *) gobject_create(NVOID,(GObject *)img,(NspTypeBase *) nsp_type_gdkimage))== NULL) 
    return RET_BUG;
  MoveObj(stack,1,ret1);
  return Max(lhs,1);
}

/* get the content of a graphic window as a pixbuf 
 *
 */

int int_get_pixbuf( Stack stack, int rhs, int opt, int lhs)
{
  NspObject *ret1;
  GdkPixbuf *pix;
  BCG *Xgc;
  CheckRhs(0,0);
  CheckLhs(0,1);
  Xgc=nsp_check_graphic_context();
  if ( Xgc->private == NULL) 
    {
      Scierror("Error: %s Current graphic driver is not attached to a drawable\n",NspFname(stack));
      return RET_BUG;
    }
  if ((pix = nsp_get_pixbuf(Xgc))== NULL) return RET_BUG;
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((ret1 = (NspObject *) 
       gobject_create(NVOID,(GObject *)pix,
		      (NspTypeBase *) nsp_type_gdkpixbuf))== NULL) 
    return RET_BUG;
  MoveObj(stack,1,ret1);
  return Max(lhs,1);
}

/* insert a pixbuf given by a file-name in a text  buffer 
 * this can be used for display a pixbuf in text main interaction 
 * window. 
 */

extern int nsp_insert_pixbuf_from_file(char *filename);

int int_show_pixbuf( Stack stack, int rhs, int opt, int lhs)
{
  char *filename;
  CheckRhs(1,1);
  CheckLhs(0,0);
  if ((filename = GetString(stack,1)) == (char*)0) return RET_BUG;
  nsp_insert_pixbuf_from_file(filename);
  return 0;
}

/* experimental: draw a pixbuf in a region of a graphic window. 
 * gtk_logo = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
 * gtk_logo_pixbuf = gdk_pixbuf_new_from_file(gtk_logo);
 * plot2d();
 * xdraw_pixbuf(0,gtk_logo_pixbuf,0,0,2,0,1,1)
 */

int int_draw_pixbuf( Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  /* window , pix,  src_x,src_y, dest_x,dest_y,  width,height */
  int_types T[] = {s_int, obj_check, s_int, s_int, s_double, s_double, s_double, s_double ,t_end};
  int src_x, src_y, win;
  double  dest_x, dest_y, width, height;
  NspGObject *pixbuf;
  Xgc=nsp_check_graphic_context();  
  if ( GetArgs(stack,rhs,opt,T,&win,&nsp_type_gdkpixbuf, &pixbuf, &src_x, &src_y, 
	       &dest_x, &dest_y, &width, &height) == FAIL) return RET_BUG;
  if ( Xgc->private == NULL) 
    {
      Scierror("Error: %s Current graphic driver is not attached to a drawable\n",NspFname(stack));
      return RET_BUG;
    }
  Xgc->graphic_engine->scale->draw_pixbuf(Xgc,pixbuf,
					  src_x, src_y, dest_x, dest_y, 
					  width, height);
  return 0;
}
/* experimental: draw a pixbuf in a region of a graphic window. 
 * gtk_logo = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
 * xdraw_pixbuf_from_file(0,gtk_logo_pixbuf,0,0,2,0,1,1)
 */

int int_draw_pixbuf_from_file( Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  /* window , pix,  src_x,src_y, dest_x,dest_y,  width,height */
  int_types T[] = {s_int, string , s_int, s_int, s_double, s_double, s_double, s_double ,t_end};
  int src_x, src_y, win;
  double  dest_x, dest_y,  width, height;
  char *fname ; 
  Xgc=nsp_check_graphic_context();  
  if ( GetArgs(stack,rhs,opt,T,&win,&fname, &src_x, &src_y, 
	       &dest_x, &dest_y, &width, &height) == FAIL) return RET_BUG;
  if ( Xgc->private == NULL) 
    {
      Scierror("Error: %s Current graphic driver is not attached to a drawable\n",NspFname(stack));
      return RET_BUG;
    }
  Xgc->graphic_engine->scale->draw_pixbuf_from_file(Xgc,fname,
						    src_x, src_y, dest_x, dest_y, 
						    width, height);
  return 0;
}


/* Acceleration of scicos standard draw.
 *
 */

static void scicos_draw_3d(BCG *Xgc,double r[],int color,double size3d)
{
  int cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  int npt=6;
  double x[]={r[0],r[0]     ,r[0]+r[2],r[0]+r[2]-size3d,r[0]-size3d     ,r[0]-size3d};
  double y[]={r[1],r[1]-r[3],r[1]-r[3],r[1]-r[3]-size3d,r[1]-r[3]-size3d,r[1]-size3d};
  if ( color != -1 && color != cpat ) 
    Xgc->graphic_engine->scale->xset_pattern(Xgc,color);
  Xgc->graphic_engine->scale->fillpolyline(Xgc,x,y,npt,TRUE);
  Xgc->graphic_engine->scale->drawpolyline(Xgc,x,y,npt,TRUE);
  Xgc->graphic_engine->scale->drawrectangle(Xgc,r);
  if ( color != -1 && color != cpat ) 
    Xgc->graphic_engine->scale->xset_pattern(Xgc,cpat);
}


static int int_scicos_draw3D(Stack stack, int rhs, int opt, int lhs)
{
  int color=-1;
  double rect[4],e=0.3;
  BCG *Xgc;
  NspMatrix *orig,*size;
  int_types T[] = {realmat,realmat,s_double,s_int, t_end} ;
  CheckRhs(4,4);
  if ( GetArgs(stack,rhs,opt,T,&orig,&size,&e,&color) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,orig,2);
  CheckLength(NspFname(stack),2,size,2);
  Xgc=nsp_check_graphic_context();
  rect[0]=orig->R[0]+e;rect[1]=orig->R[1]+size->R[1];rect[2]=size->R[0]-e;rect[3]=size->R[1]-e;
  scicos_draw_3d(Xgc,rect,color,e);
  return 0;
} 


typedef  enum { SLD_NORTH=0, SLD_SOUTH=1, SLD_EAST=2, SLD_WEST=3, SLD_ANY=4 } 
  slock_dir;
typedef  enum { SL_IN=0  ,SL_OUT=1 ,SL_EVIN=2,SL_EVOUT=3 , SL_SQP=4, SL_SQM=5 } 
  slock_type;

static void lock_draw(BCG *Xgc,const double pt[2],double xf,double yf,slock_dir dir,slock_type typ,int locked)
{
  /* angle according to dir */
  /*  LD_NORTH=0, LD_SOUTH=1, LD_EAST=2, LD_WEST=3, LD_ANY=4 */
  const double alpha[]= {180,0,90,-90,0};
  const double lock_triangle_x[]={- xf/2,xf/2,0};
  const double lock_triangle_yout[]={- yf,-yf,0};
  const double lock_triangle_yin[]={0,0,- yf}; 
  const double lock_square_x[]={- xf/2,xf/2,xf/2,-xf/2};
  const double lock_square_y[]={- yf,-yf,0,0};
  const double *ly=NULL,*lx=NULL;
  double cosa,sina;
  double x[4];
  double y[4];
  int npt=3 , i;
  switch (typ) 
    {
    case SL_SQP:
    case SL_SQM: 
      npt=4;
      lx=  lock_square_x;
      ly=  lock_square_y;
      break;
    case SL_OUT:
    case SL_EVOUT: 
      ly = lock_triangle_yin; 
      lx = lock_triangle_x; 
      break;
    default:
    case SL_IN:
    case SL_EVIN: 
      ly = lock_triangle_yout;
      lx = lock_triangle_x; 
      break;
    }
  cosa= cos(alpha[dir]*M_PI/180);
  sina= sin(alpha[dir]*M_PI/180);
  for ( i = 0 ; i < npt ; i++) 
    {
      x[i] = cosa*lx[i] -sina*ly[i]+pt[0];
      y[i] = sina*lx[i] +cosa*ly[i]+pt[1];
    }
  if ( typ == SL_SQP ) 
    Xgc->graphic_engine->scale->drawpolyline(Xgc,x,y,npt,TRUE);
  else 
    Xgc->graphic_engine->scale->fillpolyline(Xgc,x,y,npt,TRUE);
}


static int int_lock_draw(Stack stack, int rhs, int opt, int lhs)
{
  int dir=0,typ=0;
  double xf,yf;
  BCG *Xgc;
  NspMatrix *Mpt;
  int_types T[] = {realmat,s_double,s_double,s_int,s_int, t_end} ;
  CheckRhs(5,5);
  if ( GetArgs(stack,rhs,opt,T,&Mpt,&xf,&yf,&dir,&typ) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,Mpt,2);
  Xgc=nsp_check_graphic_context();
  lock_draw(Xgc,Mpt->R,xf/9.0,yf/3.5,dir,typ,1);
  return 0;
} 

/* test code for an activaed thread 
 */

#ifdef TEST_EVENT_BOX_THREAD

static void
event_box_label_pressed (GtkWidget        *widget,
			 GdkEventButton   *event,
			 gpointer user_data)
{
  g_print ("clicked on event box\n");
}

static void
event_box_button_clicked (GtkWidget *widget,
			  GtkWidget *button,
			  gpointer user_data)
{
  g_print ("pushed button\n");
}

static void create_event_box (GtkWidget *widget, char *title )
{
  GtkWidget *window = NULL;
  GtkWidget *box1;
  GtkWidget *hbox;
  GtkWidget *vbox;
  GtkWidget *button;
  GtkWidget *event_box;
  GtkWidget *label;

      
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  /* gtk_window_set_screen (GTK_WINDOW (window),
     gtk_widget_get_screen (widget)); */

  g_signal_connect (window, "destroy",
		    G_CALLBACK (gtk_widget_destroyed),
		    &window);

  gtk_window_set_title (GTK_WINDOW (window), title );
  gtk_container_set_border_width (GTK_CONTAINER (window), 0);

  box1 = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), box1);
  
  hbox = gtk_hbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (box1), hbox, TRUE, FALSE, 0);
      
  event_box = gtk_event_box_new ();
  gtk_box_pack_start (GTK_BOX (hbox), event_box, TRUE, FALSE, 0);

  vbox = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (event_box), vbox);
  g_signal_connect (event_box, "button_press_event",
		    G_CALLBACK (event_box_label_pressed),
		    NULL);
      
  label = gtk_label_new ("Click on this label");
  gtk_box_pack_start (GTK_BOX (vbox), label, TRUE, FALSE, 0);
  
  button = gtk_button_new_with_label ("button in eventbox");
  gtk_box_pack_start (GTK_BOX (vbox), button, TRUE, FALSE, 0);
  g_signal_connect (button, "clicked",
		    G_CALLBACK (event_box_button_clicked),
		    NULL);
  gtk_widget_show_all (window);
}


GMainLoop *loop;

static gboolean idle_thingy (gpointer nothing)
{
  static int poo=1;
  /* fprintf(stderr,"Inside the idle\n"); */
  poo++;
  usleep(100);
  if ( poo > 100000) g_main_loop_quit(loop); 
  return TRUE;
}

static gpointer run_thread (gpointer thread)
{
  GSource *s;
  GMainContext* ct;
  ct = g_main_context_new ();
  s = g_idle_source_new ();
  loop = g_main_loop_new (NULL, FALSE);
  g_source_set_callback (s, idle_thingy, NULL, NULL);
  g_source_attach (s, ct);
  g_source_unref (s);
  gdk_threads_enter ();
  create_event_box (NULL,"Thread event Box");
  gdk_threads_leave ();
  g_main_loop_run (loop);
  g_main_loop_unref (loop);
  g_print ("Thread is done\n");
  return NULL;
}

static int int_gtk_loop(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  GThread *thread;
  g_thread_init (NULL);
  gdk_threads_init();
  thread = g_thread_create (run_thread, NULL, TRUE, NULL);
  gdk_threads_enter ();
  nsp_message_modeless_("Quit the thread message");
  gdk_threads_leave ();
  g_print ("make the main thread sleep \n");
  sleep(60);
  g_print ("main thread re-activated \n");
  return 0;
} 
#endif 


/*************************************************************
 * The Interface for graphic functions 
 *************************************************************/

extern int int_ode( Stack stack, int rhs, int opt, int lhs); /* XXX*/
extern int int_intg(Stack stack, int rhs, int opt, int lhs); /* XXX*/
extern int int_int2d(Stack stack, int rhs, int opt, int lhs); /* XXX*/

static OpTab Graphics_func[]={
  {"ode",int_ode}, /* FIXME: en construction */
  {"intg",int_intg}, /* FIXME: en construction */
  {"int2d",int_int2d}, /* FIXME: en construction */
  {"feval",int_feval}, /* FIXME: en construction */
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
  {"fec",int_fec},
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
  {"winsid",int_winsid},
  {"param3d1",int_param3d},
  {"xstringb",int_xstringb},
  {"xstringc",int_xstringc},
  {"Matplot",int_matplot},
  {"contour2di",int_contour2d1},
  {"c2dex",int_c2dex},
  {"Matplot1",int_matplot1}, 
  {"xgraduate",int_xgraduate},
  {"xname",int_xname},
  {"xaxis",int_xaxis},
  {"seteventhandler",int_seteventhandler},
  {"help",int_gtkhelp},
  {"xs2ps",int_xs2ps},
  {"xs2fig",int_xs2fig},
  {"xs2ps_old",int_xs2ps_old},
  {"xs2pdf",int_xs2pdf},
  {"xs2png",int_xs2png},
  {"xs2ps",int_xs2ps},
  {"xs2svg",int_xs2svg},
  {"xexport",int_xexport},
  {"bsearch", int_bsearch},
  {"draw3d_objs", int_draw3dobj},
  {"xget_image",int_get_image},
  {"xget_pixbuf",int_get_pixbuf},
  {"xdraw_pixbuf",int_draw_pixbuf},
  {"xdraw_pixbuf_from_file",int_draw_pixbuf_from_file},
  {"xflush",int_xflush},
  {"show_pixbuf",int_show_pixbuf}, 
  {"scicos_draw3D",int_scicos_draw3D},
  {"scicos_lock_draw",int_lock_draw},
  {"xtest_graphic", int_xtest},
#ifdef TEST_EVENT_BOX_THREAD
  {"gtk_test_loop",int_gtk_loop},
#endif 
  {(char *) 0, NULL}
};

int Graphics_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Graphics_func[i].fonc))(stack,rhs,opt,lhs);
}

/*
 * used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Graphics_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Graphics_func[i].name;
  *f = Graphics_func[i].fonc;
}



