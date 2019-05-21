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
 * interface for nsp
 *--------------------------------------------------------------------------*/

#include <nsp/nsp.h>
#include <nsp/matrix.h>
#include <nsp/bmatrix.h>
#include <nsp/smatrix.h>
#include <nsp/plist.h>
#include <nsp/interf.h>
#include <nsp/command.h>
#include <nsp/gtksci.h>
#include <nsp/system.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/graphics-new/color.h>
#include <nsp/parse.h>
#include <nsp/gsort-p.h>
#include <nsp/gtk/gobject.h> /* FIXME: nsp_gtk_eval_function */
#include "Plo3dObj.h"

#include <gtk/gtk.h>
#include <nsp/figuredata.h>
#include <nsp/figure.h>
#include <nsp/axes.h>
#include <nsp/objs3d.h>
#include <nsp/compound.h>
#include <nsp/curve.h>
#include <nsp/polyline.h>
#include <nsp/vfield.h>
#include <nsp/grarc.h>
#include <nsp/grrect.h>
#include <nsp/grstring.h>
#include <nsp/compound.h>
#include <nsp/arrows.h>
#include <nsp/segments.h>
#include <nsp/polyhedron.h>
#include <nsp/spolyhedron.h>
#include <nsp/polyline3d.h>
#include <nsp/points3d.h>
#include <nsp/objs3d.h>
#include <nsp/grcommon.h>
#include <nsp/gmatrix.h>
#include <nsp/gmatrix1.h>
#include <nsp/grimage.h>
#include <nsp/fec.h>
#include <nsp/contour.h>
#include <nsp/contour3d.h>
#include <nsp/gpixbuf.h>
#include <nsp/nspthreads.h>
#include <nsp/pr-output.h>

/* XXX */
extern NspSMatrix *GetSMatUtf8(Stack stack,int pos);
extern NspSMatrix *GetSMatCopyUtf8(Stack stack,int pos);
extern const char *nsp_get_extension(const char *name);
extern BCG *nsp_check_graphic_context(void);
extern int nsp_call_predefined_callbacks(BCG *Xgc, const char *name, int winid);

extern int nsp_contour_if_new(BCG *Xgc,double *x, double *y, double *z, int *n1, int *n2,
			      int *flagnz, int *nz, double *zz, int *style);
extern int nsp_get_level_curves_new(double **x, double **y, int *mm, int *n);

static int nsp_graphic_demo (const char *fname,const char *code,int flag) ;
static void  nsp_gwin_clear(void);
static int plot3d_build_z(Stack stack,NspMatrix *x,NspMatrix *y,NspMatrix *z,NspObject *f, NspObject *fargs, int std);
static NspMatrix * check_style(Stack stack,const char *fname,char *varname,NspMatrix *var,int size) ;
static int * check_iflag(Stack stack,const char *fname,char *varname,NspMatrix *var,int size) ;
static int * check_param_iflag(Stack stack,const char *fname,char *varname,NspMatrix *var,int size) ;
static double * check_ebox(Stack stack,const char *fname,char *varname,NspMatrix *var);
static char * check_strf(Stack stack,const char *fname,char *varname,char *strf);
static char * check_legend(Stack stack,const char *fname,char *varname, char *legend);
static const char * check_legend_3d(Stack stack,const char *fname,char *varname,const char *legend);
static int check_legend_pos(Stack stack,const char *fname,const char *varnam,const char *l_pos);
static int * check_nax(Stack stack,const char *fname,char *varname,NspMatrix *var);
static int check_zminmax (Stack stack,const char *fname,char *varname,NspMatrix *var);
static int check_colminmax (Stack stack,const char *fname,char *varname,NspMatrix *var);
static int check_colout (Stack stack,const char *fname,char *varname,NspMatrix *var);
static char * check_logflags(Stack stack,const char *fname,char *varname,char *logflags);
static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val);
static int get_arc(Stack stack, int rhs, int opt, int lhs,double **val);
static void plot2d_strf_change(char c, char *strf);
static int int_check2d(Stack stack,NspMatrix *Mstyle,NspMatrix **Mstyle_new,int ns,
		       char **strf,char **leg, char **leg_pos,int *leg_pos_i,
		       NspMatrix *Mrect,double **rect,
		       NspMatrix *Mnax,int **nax,
		       int frameflag,int axesflag,char **logflags);
static void nsp_set_box_parameters(NspObjs3d *objs3d,int flag);

static const double  rect_def[]= {0.,0.,10.0,10.0};
static double rect_loc[]=  {0.,0.,10.0,10.0};
static double * check_rect(Stack stack,const char *fname,char *varname,NspMatrix *var);

/**
 * int_champ_G:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 * @func:
 * @colored:
 *
 * general interface for champ and champ1
 * (XXX: The names should be changed)
 *
 * Returns:
 **/

static int int_champ_G(Stack stack, int rhs, int opt, int lhs,int colored )
{
  int *nax, frame= -1, axes=-1;
  NspMatrix *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  double *rect=NULL ;
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;
  int leg_posi;
  int auto_axis=TRUE,iso=FALSE;

  NspAxes *axe;
  NspVField *vf;
  NspMatrix *x,*y,*fx,*fy;
  double arfact =1.0;

  int_types T[] = {realmat,realmat,realmat,realmat,new_opts, t_end} ;

  nsp_option opts_2d[] ={{ "arfact",s_double,NULLOBJ,-1},
			 { "axesflag",s_int,NULLOBJ,-1},
			 { "frameflag",s_int,NULLOBJ,-1},
			 { "leg",string,NULLOBJ,-1},
			 { "leg_pos",string,NULLOBJ,-1},
			 { "logflag",string,NULLOBJ,-1},
			 { "nax",mat_int,NULLOBJ,-1},
			 { "rect",realmat,NULLOBJ,-1},
			 { "strf",string,NULLOBJ,-1},
			 { "auto_axis",s_bool,NULLOBJ,-1},
			 { "iso",s_bool,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};

  /* keep same order as in opts_2d */
  enum  { arfact_opts, axesflag_opts , frameflag_opts, leg_opts, leg_pos_opts,
	  logflag_opts , nax_opts, rect_opts , strf_opts,
	  auto_axis_opts, iso_opts};

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fx,&fy,&opts_2d,&arfact,
	       &axes,&frame,&leg,&leg_pos,&logflags,
	       &Mnax,&Mrect,&strf,&auto_axis,&iso) == FAIL)
    return RET_BUG;

  CheckSameDims(NspFname(stack),3,4,fx,fy);
  CheckDimProp(NspFname(stack),2,3, y->mn != fx->n);
  CheckDimProp(NspFname(stack),1,3, x->mn != fx->m);

  if ( fx->mn == 0) { return 0;}

  if ( int_check2d(stack,Mstyle,NULL,0,&strf,&leg,&leg_pos,&leg_posi,
		   Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0)
    return RET_BUG;

  nsp_gwin_clear();
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  /* create a vfield and insert-it in axes */
  if ( ( x = (NspMatrix *)  nsp_object_copy_and_name("x",NSP_OBJECT(x))) == NULLMAT) return RET_BUG;
  if ( ( y = (NspMatrix *)  nsp_object_copy_and_name("y",NSP_OBJECT(y))) == NULLMAT) return RET_BUG;
  if ( ( fx = (NspMatrix *)  nsp_object_copy_and_name("fx",NSP_OBJECT(fx))) == NULLMAT) return RET_BUG;
  if ( ( fy = (NspMatrix *)  nsp_object_copy_and_name("fy",NSP_OBJECT(fy))) == NULLMAT) return RET_BUG;
  vf = nsp_vfield_create("vf",fx,fy,x,y,colored,NULL);
  if ( vf == NULL) return RET_BUG;
  /* insert the new vfield */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) vf,FALSE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }

  /* updates the axe information according to optional values */
  if ( opts_2d[strf_opts].obj != NULLOBJ)
    {
      /* strf is given use it to fix other arguments */
      if ( opts_2d[auto_axis_opts].obj == NULLOBJ)
	{
	  /* auto_axis not given */
	  auto_axis = TRUE;
	  if ( strf[1] == '1' || strf[1] == '2' || strf[1] == '3' || strf[1] == '4' ) auto_axis=FALSE;
	}
      if ( opts_2d[iso_opts].obj == NULLOBJ)
	{
	  /* iso not given */
	  iso = FALSE;
	  if ( strf[1] == '3' || strf[1] == '4' || strf[1] == '5' || strf[1] == '6' ) iso=TRUE;
	}
      if ( opts_2d[axesflag_opts].obj == NULLOBJ)
	{
	  /* axes not given */
	  axes = strf[2] - '0';
	}
    }

  if ( opts_2d[iso_opts].obj != NULLOBJ ||  opts_2d[strf_opts].obj ) axe->obj->iso = iso;
  if ( opts_2d[auto_axis_opts].obj != NULLOBJ ||  opts_2d[strf_opts].obj ) axe->obj->auto_axis = auto_axis;
  if ( opts_2d[rect_opts].obj != NULLOBJ )
    {
      memcpy(axe->obj->rect->R,rect,4*sizeof(double));
      memcpy(axe->obj->frect->R,axe->obj->rect->R,4*sizeof(double));
      axe->obj->fixed = TRUE;
    }
  /* update the axesflag if given as options in axesflag or strf */
  if ( opts_2d[axesflag_opts].obj != NULLOBJ  || opts_2d[strf_opts].obj != NULLOBJ)
    {
      axe->obj->axes = axes;
    }
  if ( opts_2d[logflag_opts].obj != NULLOBJ)
    {
      axe->obj->xlog = ( strlen(logflags) >= 1) ? ((logflags[1]=='n') ? FALSE:TRUE) : FALSE;
      axe->obj->ylog=  ( strlen(logflags) >= 2) ? ((logflags[2]=='n') ? FALSE:TRUE) : FALSE;
    }
  /* invalidate the whole axe */
  nsp_axes_invalidate((NspGraphic *)axe);
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(vf));
      return 1;
    }
  return 0;
}

/**
 * int_champ_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_champ_new( Stack stack, int rhs, int opt, int lhs)
{
  if (rhs <= 0) return nsp_graphic_demo(NspFname(stack),"champ(1:10,1:10,rand(10,10),rand(10,10));",1);
  return int_champ_G( stack, rhs,opt,lhs, FALSE);
}

/**
 * int_champ1_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_champ1_new( Stack stack, int rhs, int opt, int lhs)
{
  if (rhs <= 0) return nsp_graphic_demo(NspFname(stack),"champ1(1:10,1:10,rand(10,10),rand(10,10));",1);
  return int_champ_G( stack, rhs, opt, lhs,TRUE);
}

/**
 * int_contour_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_contour_new( Stack stack, int rhs, int opt, int lhs)
{
  NspObject *args = NULL,*fobj;/* when z is a function */
  NspObject *ret = NULL;
  int *iflag,flagx,nnz=10;
  NspMatrix *x,*y,*z,*nz,*Mebox=NULL,*Mflag=NULL;
  double alpha=35.0,theta=45.0,zlev=0.0,*ebox ;
  char *leg=NULL;
  int_types T[] = {realmat,realmat,obj,realmat,new_opts, t_end} ;

  CheckLhs(0,1);

  nsp_option opts[] ={{ "alpha",s_double,NULLOBJ,-1},
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",realmat,NULLOBJ,-1},
		      { "leg",string,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { "zlevel",s_double,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  enum  {alpha_opts, ebox_opts, flag_opts, leg_opts, theta_opts, zlevel_opts };

  if (rhs <= 0) { return nsp_graphic_demo(NspFname(stack),"contour(1:5,1:10,rand(5,10),5);",1); }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fobj,&nz,&opts,&alpha,&Mebox,&Mflag,&leg,
	       &theta,&zlev) == FAIL) return RET_BUG;

  CheckVector(NspFname(stack),1,x);
  CheckVector(NspFname(stack),2,y);

  if ( IsNspPList(fobj) )
    {
      /* Note: here we are always in the case where 
       * x and y are vectors, thus z is of size  (x->mn,y->mn)
       */
      if ((z = nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULL) return RET_BUG;
      if ( plot3d_build_z(stack,x,y,z,fobj,args,TRUE)== FAIL)
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

  nsp_gwin_clear();

  if ( iflag[0] >= 2 )
    {
      /* 2D contour:
       * mode=2:the level curves are drawn on a 2D plot.
       */
      NspMatrix *Mistyle=NULL, *s=NULL;
      NspAxes *axe;
      NspContour *vf;
      if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
      /* create a vfield and insert-it in axes */
      if ( ( x = (NspMatrix *)  nsp_object_copy_and_name("x",NSP_OBJECT(x))) == NULLMAT)
	return RET_BUG;
      if ( ( y = (NspMatrix *)  nsp_object_copy_and_name("y",NSP_OBJECT(y))) == NULLMAT)
	return RET_BUG;
      if ( ( z = (NspMatrix *)  nsp_object_copy_and_name("z",NSP_OBJECT(z))) == NULLMAT)
	return RET_BUG;
      if ( Mistyle != NULL)
	{
	  if ( ( s = (NspMatrix *) nsp_object_copy_and_name("style",NSP_OBJECT(Mistyle))) == NULLMAT)
	    return RET_BUG;
	}
      if ( flagx==1)
	{
	  if ( (nz = (NspMatrix *) nsp_object_copy_and_name("nz",NSP_OBJECT(nz))) == NULLMAT)
	    return RET_BUG;
	}
      vf = nsp_contour_create("c",z,x,y,(flagx==1) ? nz:NULL , nnz,s,NULL);
      if ( vf == NULL) return RET_BUG;
      /* insert the new vfield */
      if ( nsp_axes_insert_child(axe,(NspGraphic *)vf, FALSE )== FAIL)
	{
	  Scierror("Error: failed to insert contour in Figure\n");
	  return RET_BUG;
	}
      /* updates the axes scale information */
      nsp_axes_invalidate(((NspGraphic *) axe));
      ret = NSP_OBJECT(vf);
    }
  else
    {
      NspContour3d *vf;
      NspObjs3d *objs3d;
      if ((objs3d = nsp_check_for_current_objs3d(TRUE)) == NULL) return RET_BUG;

      /* create a conrour3d and insert-it in axes */
      if ( ( x = (NspMatrix *)  nsp_object_copy_and_name("x",NSP_OBJECT(x))) == NULLMAT)
	return RET_BUG;
      if ( ( y = (NspMatrix *)  nsp_object_copy_and_name("y",NSP_OBJECT(y))) == NULLMAT)
	return RET_BUG;
      if ( ( z = (NspMatrix *)  nsp_object_copy_and_name("z",NSP_OBJECT(z))) == NULLMAT)
	return RET_BUG;
      if ( (nz = (NspMatrix *)  nsp_object_copy_and_name("nz",NSP_OBJECT(nz))) == NULLMAT)
	return RET_BUG;

      if ( opts[flag_opts].obj != NULLOBJ)
	{
	  nsp_set_box_parameters(objs3d, iflag[2]);
	}


      vf = nsp_contour3d_create("c",x,y,z,nz,iflag[0],zlev,NULL);
      if ( vf == NULL) return RET_BUG;
      /* insert the new contour3d */
      if ( nsp_objs3d_insert_child(objs3d, (NspGraphic *) vf,FALSE)== FAIL)
	{
	  Scierror("Error: failed to insert contour in Figure\n");
	  return RET_BUG;
	}
      nsp_strf_objs3d( objs3d , ebox , iflag[1]);
      nsp_objs3d_invalidate(((NspGraphic *) objs3d));
      ret = NSP_OBJECT(vf);
    }
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(ret));
      return 1;
    }
  return 0;
}

/**
 * int_contour2d_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * interface for 2d contours
 *
 * Returns:
 **/

static int int_contour2d_new( Stack stack, int rhs, int opt, int lhs)
{
  int auto_axis=TRUE,iso=FALSE, z_allocated=FALSE;
  NspObject *args = NULL,*fobj = NULL;/* when z is a function */
  NspMatrix *s=NULL;
  NspAxes *axe;
  NspContour *vf;
  int flagx=0,nnz= 10, frame= -1, axes=-1;
  NspMatrix *x,*y,*z,*zn,*nz;
  /* for 2d optional arguments; */
  NspMatrix *Mistyle=NULL,*Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  double *rect ;
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;
  int leg_posi;
  int *nax;

  nsp_option opts_2d[] ={{ "axesflag",s_int,NULLOBJ,-1},
			 { "frameflag",s_int,NULLOBJ,-1},
			 { "leg",string,NULLOBJ,-1},
			 { "leg_pos",string,NULLOBJ,-1},
			 { "logflag",string,NULLOBJ,-1},
			 { "nax",mat_int,NULLOBJ,-1},
			 { "rect",realmat,NULLOBJ,-1},
			 { "strf",string,NULLOBJ,-1},
			 { "style",mat,NULLOBJ,-1},
			 { "auto_axis",s_bool,NULLOBJ,-1},
			 { "iso",s_bool,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};

  /* keep same order as in opts_2d */
  enum  { axesflag_opts , frameflag_opts, leg_opts, leg_pos_opts,
	  logflag_opts , nax_opts, rect_opts , strf_opts, style_opts,
	  auto_axis_opts, iso_opts};

  int_types T[] = {realmat,realmat,obj,realmat,new_opts, t_end} ;

  CheckLhs(0,1);

  if (rhs <= 0) {return  nsp_graphic_demo(NspFname(stack),"contour2d(1:5,1:10,rand(5,10),5);",1); }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fobj,&nz,&opts_2d,&axes,&frame,&leg,&leg_pos,
	       &logflags,&Mnax,&Mrect,&strf,&Mstyle,&auto_axis,&iso) == FAIL) return RET_BUG;

  CheckVector(NspFname(stack),1,x);
  CheckVector(NspFname(stack),2,y);

  if ( IsNspPList(fobj) )
    {
      /* Note: here we are always in the case where 
       * x and y are vectors, thus z is of size  (x->mn,y->mn)
       */
      if ((z = nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULL) return RET_BUG;
      z_allocated= TRUE;
      if ( plot3d_build_z(stack,x,y,z,fobj,args,TRUE)== FAIL)
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

  /* XXX could be among the optional args */

  if ( nz->mn == 0 ) return 0;
  if ( nz->mn == 1 ) {
    flagx = 0;  nnz = Max(1,(int) nz->R[0]);
  } else {
    flagx = 1;  nnz = nz->mn ;
  }

  if ( int_check2d(stack,Mstyle,&Mistyle,nnz,&strf,&leg,&leg_pos,&leg_posi,
		   Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0)
    goto bug;

  if ( Mistyle->mn != nnz)
    {
      Scierror("%s: style should be of size %d\n",NspFname(stack),nnz);
      goto bug;
    }

  nsp_gwin_clear();
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) goto bug;

  /* create a vfield and insert-it in axes */
  if ( ( x = (NspMatrix *)  nsp_object_copy_and_name("x",NSP_OBJECT(x))) == NULLMAT) goto bug;
  if ( ( y = (NspMatrix *)  nsp_object_copy_and_name("y",NSP_OBJECT(y))) == NULLMAT) goto bug;
  if ( ( zn = (NspMatrix *)  nsp_object_copy_and_name("z",NSP_OBJECT(z))) == NULLMAT) goto bug;
  if ( Mistyle != NULL)
    {
      if ( ( s = (NspMatrix *) nsp_object_copy_and_name("style",NSP_OBJECT(Mistyle))) == NULLMAT)
	goto bug;
    }
  if ( flagx==1)
    {
      if ( (nz = (NspMatrix *) nsp_object_copy_and_name("nz",NSP_OBJECT(nz))) == NULLMAT)
	goto bug;
    }
  vf = nsp_contour_create("c",zn,x,y,(flagx==1) ? nz:NULL , nnz,s,NULL);
  if ( vf == NULL) goto bug;
  /* insert the new contour */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) vf , FALSE)== FAIL)
    {
      Scierror("Error: failed to insert contour in Figure\n");
      goto bug;
    }

  /* updates the axe information according to optional values */
  if ( opts_2d[strf_opts].obj != NULLOBJ)
    {
      /* strf is given use it to fix other arguments */
      if ( opts_2d[auto_axis_opts].obj == NULLOBJ)
	{
	  /* auto_axis not given */
	  auto_axis = TRUE;
	  if ( strf[1] == '1' || strf[1] == '2' || strf[1] == '3' || strf[1] == '4' ) auto_axis=FALSE;
	}
      if ( opts_2d[iso_opts].obj == NULLOBJ)
	{
	  /* iso not given */
	  iso = FALSE;
	  if ( strf[1] == '3' || strf[1] == '4' || strf[1] == '5' || strf[1] == '6' ) iso=TRUE;
	}
      if ( opts_2d[axesflag_opts].obj == NULLOBJ)
	{
	  /* axes not given */
	  axes = strf[2] - '0';
	}
    }

  if ( opts_2d[iso_opts].obj != NULLOBJ ||  opts_2d[strf_opts].obj ) axe->obj->iso = iso;
  if ( opts_2d[auto_axis_opts].obj != NULLOBJ ||  opts_2d[strf_opts].obj ) axe->obj->auto_axis = auto_axis;
  if ( opts_2d[rect_opts].obj != NULLOBJ )
    {
      memcpy(axe->obj->rect->R,rect,4*sizeof(double));
      memcpy(axe->obj->frect->R,axe->obj->rect->R,4*sizeof(double));
      axe->obj->fixed = TRUE;
    }
  /* update the axesflag if given as options in axesflag or strf */
  if ( opts_2d[axesflag_opts].obj != NULLOBJ  || opts_2d[strf_opts].obj != NULLOBJ)
    {
      axe->obj->axes = axes;
    }
  if ( opts_2d[logflag_opts].obj != NULLOBJ)
    {
      axe->obj->xlog = ( strlen(logflags) >= 1) ? ((logflags[1]=='n') ? FALSE:TRUE) : FALSE;
      axe->obj->ylog=  ( strlen(logflags) >= 2) ? ((logflags[2]=='n') ? FALSE:TRUE) : FALSE;
    }

  nsp_axes_invalidate(((NspGraphic *) axe));
  if ( Mstyle != Mistyle)
    nsp_matrix_destroy(Mistyle);
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(vf));
      return 1;
    }
  if ( z_allocated ) nsp_matrix_destroy(z);
  return 0;
 bug:
  if ( z_allocated ) nsp_matrix_destroy(z);
  return RET_BUG;
}

/**
 * int_contour2di_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 * @func:
 * @colored:
 *
 * Interface to contour2di
 * which is used to get the values of the contours.
 *
 * Returns:
 **/

static int int_contour2di_new( Stack stack, int rhs, int opt, int lhs)
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
  nsp_contour_if_new(Xgc,x->R,y->R,z->R,&z->m,&z->n,&flagx,&nz,znz,NULL);/* (int *) M->R); */
  nsp_get_level_curves_new(&hl1, &hl2, &m, &n);
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

/* deprecated:
 * used in contourf, to extract contour points
 */

#if 0
static int int_c2dex( Stack stack, int rhs, int opt, int lhs)
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
#endif

/**
 * int_param3d_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * XXX should be finished to return a compound
 *
 * Returns:
 **/

static int int_param3d_new( Stack stack, int rhs, int opt, int lhs)
{
  int nb_polylines;
  int iso=FALSE;
  int i,color,nb_poly,psize;
  NspObjs3d *objs3d;
  int *iflag;
  NspMatrix *x,*y,*z,*Mebox=NULL,*flag=NULL,*Mstyle=NULL;
  double alpha=35.0,theta=45.0,*ebox ;
  const char *leg=NULL,*legend=NULL, *box_style_str=NULL;
  int box_style = 2; /* matlab mode by default */
  NspMatrix *Mopts[5]={NULL,NULL,NULL,NULL,NULL};
  char *Mopt_names[]={"mark","mark_size","mark_color", "line_thickness", "line_color" };
  enum { mark_opt, mark_size_opt, mark_color_opt, line_thickness_opt, line_color_opt, };
  int_types T[] = {realmat,realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "alpha",s_double,NULLOBJ,-1},
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",realmat,NULLOBJ,-1},
		      { "leg",string,NULLOBJ,-1}, /* XXXX string matrix ? */
		      { "style",mat_int,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { "box_style",string,NULLOBJ,-1},
		      { "iso",s_bool,NULLOBJ,-1},
		      { "mark",realmat,NULLOBJ,-1},
		      { "mark_size",realmat,NULLOBJ,-1},
		      { "mark_color",realmat,NULLOBJ,-1},
		      { "line_thickness",realmat,NULLOBJ,-1},
		      { "line_color",realmat,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  /* keep same order as in opts */
  enum  {alpha_opts, ebox_opts, flag_opts, leg_opts, style_opts, theta_opts, bow_style_opts, iso_opts };

  if ( rhs <= 0)
    {
      return nsp_graphic_demo(NspFname(stack),"t=0:0.1:5*%pi;param3d(sin(t),cos(t),t*0.1);",1);
    }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&z,&opts,&alpha,&Mebox,&flag,&leg,&Mstyle,
	       &theta,&box_style_str,&iso,&Mopts[0],&Mopts[1],&Mopts[2],&Mopts[3],&Mopts[4])== FAIL) 
    return RET_BUG;
  
  if ( x->mn == 0 ) return 0;

  CheckSameDims(NspFname(stack),1,2,x,y);
  CheckSameDims(NspFname(stack),1,3,x,z);

  if (( iflag = check_param_iflag(stack,NspFname(stack),"flag",flag,2))==NULL) return RET_BUG;

  iflag[0]=Max(Min(iflag[0],8),0);
  if ( Mebox != NULLMAT)
    {
      /* ebox is given then iflag[0] must be 1 or 3 or 5 or 7*/
      if ( iflag[0] == 2 ||  iflag[0] == 4 ||  iflag[0] == 6 || iflag[0] == 8 ) iflag[0]--;
    }
  else
    {
      /* ebox is not given then iflag[1] cannot be 1 or 3 or 5 */
      if ( iflag[0] == 1 ||  iflag[0] == 3 ||  iflag[0] == 5 || iflag[0] == 7 ) iflag[0]++;
    }
  
  if (( ebox = check_ebox(stack,NspFname(stack),"ebox",Mebox)) == NULL) return RET_BUG;
  if (( legend = check_legend_3d(stack,NspFname(stack),"leg",leg)) == NULL) return RET_BUG;

  nb_polylines = (x->m == 1) ? 1 : x->n;
  
  if ( Mstyle != NULLMAT )
    {
      if ( Mstyle->mn !=  nb_polylines && Mstyle->mn != 1 )
	{
	  Scierror("%s: style argument has wrong size (%d), %d values or 1 value expected\n",
		   NspFname(stack),Mstyle->mn, nb_polylines);
	  return RET_BUG;
	}
    }
  for( i = 0 ; i < 4 ; i++)
    {
      if ( Mopts[i] == NULLMAT) continue;
      if ( Mopts[i]->mn !=  nb_polylines &&  Mopts[i]->mn != 1 )
	{
	  Scierror("%s: %s argument has wrong size (%d), %d values or 1 value expected\n",
		   NspFname(stack),Mopt_names[i],Mopts[i]->mn, nb_polylines);
	  return RET_BUG;
	}
    }
  
  /* special case for line_color */
  i = line_color_opt;
  if ( Mopts[i] != NULLMAT)
    {
      if ( Mopts[i]->mn !=  nb_polylines &&  Mopts[i]->mn != 1 &&  Mopts[i]->mn != x->mn )
	{
	  Scierror("%s: %s argument has wrong size (%d), 1 value or %d or %d values expected\n",
		   NspFname(stack),Mopt_names[i],Mopts[i]->mn, nb_polylines, x->mn);
	  return RET_BUG;
	}
    }
  
  /*
   * check that iflag[1] and leg are compatible
   * i.e force visibility of axes names if they are given
   */
  if (leg !=  NULL && strlen(leg) != 0 ) iflag[1]=4;

  nsp_gwin_clear();
  if ((objs3d = nsp_check_for_current_objs3d(TRUE)) == NULL) return RET_BUG;
  /* parameters for box drawing */
  if ( opts[flag_opts].obj != NULLOBJ)
    {
      nsp_set_box_parameters(objs3d, iflag[1]);
    }
  
  if ( opts[alpha_opts].obj != NULLOBJ)
    {
      objs3d->obj->alpha=alpha;
    }

  if ( opts[theta_opts].obj != NULLOBJ)
    {
      objs3d->obj->theta=theta;
    }

  if ( opts[iso_opts].obj != NULLOBJ &&  iso == TRUE)
    {
      iflag[0]= (Mebox == NULL) ? 4: 3;
    }
  
  /* use the box_style parameter if given */
  if ( box_style_str != NULL)
    {
      const char *box_style_table[] = {"none", "scilab", "matlab", NULL};
      box_style = is_string_in_array(box_style_str,box_style_table,1);
      if ( box_style < 0 )
	{
	  string_not_in_array(stack, box_style_str, box_style_table, "optional argument mode");
	  return RET_BUG;
	}
      objs3d->obj->with_box = ( box_style == 0 ) ? FALSE : TRUE;
      objs3d->obj->box_style = (box_style == 1 ) ? SCILAB : MATLAB;
    }
    
  /* Loop on the number of polylines */
  nb_poly = (x->m == 1) ? 1 : x->n;
  psize= (x->m==1) ? x->n : x->m;
  for ( i = 0 ; i < nb_poly ; i++)
    {
      int mark,mark_color,mark_size, use_line, use_mark;
      int line_color, line_thickness;
      NspObject *gobj;
      NspMatrix *M1;
      if ((M1 = nsp_matrix_create("coord",'r',psize,3))== NULLMAT) return RET_BUG;
      memcpy(M1->R,x->R + i*(psize),psize*sizeof(double));
      memcpy(M1->R+psize,y->R + i*(psize),psize*sizeof(double));
      memcpy(M1->R+2*psize,z->R + i*(psize),psize*sizeof(double));
      /* check the color */
      color = ( Mstyle == NULL) ? 1 : (( Mstyle->mn == 1) ? Mstyle->R[0] : Mstyle->R[i]);
      mark = (Mopts[mark_opt]==NULL) ? -2 : Mopts[mark_opt]->R[i];
      mark = (( Mstyle != NULL) && color < 0 ) ? -color : mark;
      mark_size = (Mopts[mark_size_opt]==NULL) ? -1:
	((Mopts[mark_size_opt]->mn == 1 ) ? Mopts[mark_size_opt]->R[0]: Mopts[mark_size_opt]->R[i]);
      mark_color = (Mopts[mark_color_opt]==NULL) ? -1:
	((Mopts[mark_color_opt]->mn == 1 ) ? Mopts[mark_color_opt]->R[0]: Mopts[mark_color_opt]->R[i]);
      line_color =  (Mopts[line_color_opt]==NULL) ? -2 :	
	((Mopts[line_color_opt]->mn == 1 ) ? Mopts[line_color_opt]->R[0]: Mopts[line_color_opt]->R[i]);
      line_color = (( Mstyle != NULL) && color >= 0 ) ? color : line_color;
      line_thickness = (Mopts[line_thickness_opt]==NULL) ? -1 :
	((Mopts[line_thickness_opt]->mn == 1 ) ? Mopts[line_thickness_opt]->R[0] : Mopts[line_thickness_opt]->R[i]);
      use_line = line_color != -2 || ( Mstyle == NULL && mark == -2 );
      use_mark = mark != -2;
      if ( use_mark )
	{
	  NspMatrix *M1c = M1;
	  if ( use_line )
	    {
	      /* we have both mark and line we must copy */
	      if ((M1c =  nsp_matrix_create("coord",'r',psize,3))== NULLMAT) return RET_BUG;
	      memcpy(M1c->R,M1->R,M1c->mn*sizeof(double));
	    }
	  gobj = (NspObject *)nsp_points3d_create("pts",M1c,NULL,mark_color,mark,mark_size,NULL,0,-1,NULL);
	  if ( gobj == NULL)  return RET_BUG;
	  if ( nsp_objs3d_insert_child(objs3d, (NspGraphic *) gobj,FALSE)== FAIL)
	    {
	      Scierror("Error: failed to insert contour in Figure\n");
	      return RET_BUG;
	    }
	}
      if ( use_line )
	{
	  NspMatrix *Mcol;
	  if ( Mopts[line_color_opt] != NULL && Mopts[line_color_opt]->mn == x->mn )
	    {
	      if ((Mcol = nsp_matrix_create("col",'r',1,x->mn))== NULLMAT) return RET_BUG;
	      memcpy(Mcol->R,Mopts[line_color_opt]->R + i*x->m,x->m*sizeof(double));
	    }
	  else
	    {
	      if ((Mcol = nsp_matrix_create("col",'r',1,1))== NULLMAT) return RET_BUG;
	      Mcol->R[0]= line_color;
	    }
	  gobj = (NspObject *)nsp_polyline3d_create("pol",M1,NULL,Mcol,line_thickness,NULL,0,-1,NULL);
	  if ( gobj == NULL)  return RET_BUG;
	  if ( nsp_objs3d_insert_child(objs3d, (NspGraphic *) gobj,FALSE)== FAIL)
	    {
	      Scierror("Error: failed to insert contour in Figure\n");
	      return RET_BUG;
	    }
	}
    }
  if (iflag[0] != 0 ) objs3d->obj->scale_flag = iflag[0];
  nsp_strf_objs3d( objs3d , ebox , iflag[0]);
  nsp_objs3d_invalidate(((NspGraphic *) objs3d));
  return 0;
}

/**
 * int_geom3d:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * XXX a revoir pour le nouveau graphique
 *
 * Returns:
 **/

static int int_geom3d( Stack stack, int rhs, int opt, int lhs)
{
  Scierror("%s: not implemented in new graphics\n",NspFname(stack));
  return RET_BUG;
#if 0
  BCG *Xgc;
  NspMatrix *x1,*y1,*z1;

  if (rhs <= 0) { return nsp_graphic_demo(NspFname(stack), "t=0:0.1:5*%pi,[x,y]=geom3d(sin(t),cos(t),t/10);",1);}

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
#endif

}

/*-----------------------------------------------------------
 * plot3dXXX(x,y,z,opts)
 *-----------------------------------------------------------*/

typedef NspGraphic *(*f3d) (double *,double *,double *,int *p,int *q,double *,
			    double *,const char *,int *,double *,  NspMatrix *,int shade,
			    double *, int ncol, int back_color);
typedef NspGraphic *(*f3d1)(double *,double *,double *,int izcol,int *cvect,int *p,int *q,double *,
			    double *,const char *,int *,double *,NspMatrix *,int shade, int back_color);
typedef NspGraphic *(*f3d2)(double *,double *,double *,int izcol,int *cvect,int *p,int *q,double *,
			    double *,const char *,int *,double *,NspMatrix *,int shade, int back_color);
typedef NspGraphic *(*f3d3)(double *,double *,double *,int izcol,int *cvect,int *p,int *q,double *,
			    double *,const char *,int *,double *,NspMatrix *,int shade, int back_color);

static int plot3d_build_z(Stack stack,NspMatrix *x,NspMatrix *y,NspMatrix *z,NspObject *f, NspObject *fargs, int std);

static int int_plot3d_G( Stack stack, int rhs, int opt, int lhs,f3d func,f3d1 func1,f3d2 func2,f3d3 func3)
{
  int iso=FALSE;
  int mesh=TRUE,mesh_only=FALSE, shade=TRUE;
  NspObject  *args = NULL,*fobj;/* when z is a function */
  double alpha=35.0,theta=45.0,*ebox ;
  const char *leg=NULL, *leg1=NULL,*box_style_str=NULL;
  NspMatrix *x,*y,*z,*zloc=NULL,*Mcolors=NULL,*Mflag=NULL,*Mebox=NULL, *colormap=NULL;
  int izcol=0, *iflag, ret=0;
  NspGraphic *nsp_ret;
  int box_style = 2; /* matlab mode by default */
  int surface_color= -1; /* blue in the default colormap */
  int back_color= 3; /* green in the default colormap */
  int_types T[] = {realmat,realmat,obj,new_opts, t_end} ;


  nsp_option opts[] ={{ "args",list,NULLOBJ,-1},
		      { "alpha",s_double,NULLOBJ,-1},
		      { "colormap",realmat,NULLOBJ,-1},
		      { "colors", matcopy,NULLOBJ,-1},   /* colors per face */
		      { "ebox",realmat,NULLOBJ,-1},
		      { "flag",realmat,NULLOBJ,-1},
		      { "leg", string,NULLOBJ,-1},
		      { "theta",s_double,NULLOBJ,-1},
		      { "mesh", s_bool,NULLOBJ,-1},
		      { "mesh_only", s_bool,NULLOBJ,-1},
		      { "shade", s_bool,NULLOBJ,-1},
		      { "box_style",string,NULLOBJ,-1},
		      { "surface_color",s_int,NULLOBJ,-1}, /* color for plot3d */
		      { "back_color",s_int,NULLOBJ,-1}, /* color for backface -2 means like faces */
		      { "iso",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  /* keep same order as in opts */
  enum { args_opts, alpha_opts, colormap_opts, colors_opts, ebox_opts, flag_opts, 
	 leg_opts, theta_opts, mesh_opts, mesh_only_opts, shade_opts, box_style_opts, 
	 surface_color_opts, back_color_opts, iso_opts};
  
  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fobj,&opts,&args,&alpha,&colormap,
	       &Mcolors,&Mebox,&Mflag,&leg,&theta,&mesh,&mesh_only,&shade,
	       &box_style_str,&surface_color,&back_color, &iso) == FAIL)
    return RET_BUG;

  if (x->mn == 0) { return 0;}

  if ( IsNspPList(fobj) )
    {
      int std=(x->m == 1 || x->n==1) && (y->m == 1 || y->n == 1);
      if (std)
	{
	  if ((z = zloc= nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULL) return RET_BUG;
	  if ( plot3d_build_z(stack,x,y,z,fobj,args, std)== FAIL)
	    {
	      ret= RET_BUG;
	      goto end;
	    }
	}
      else
	{
	  if ((z = zloc= nsp_matrix_create(NVOID,'r',x->m,x->n))== NULL) return RET_BUG;
	  if ( plot3d_build_z(stack,x,y,z,fobj,args,std)== FAIL)
	    {
	      ret= RET_BUG;
	      goto end;
	    }
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

  if ( box_style_str != NULL)
    {
      const char *box_style_table[] = {"none", "scilab", "matlab", NULL};
      box_style = is_string_in_array(box_style_str,box_style_table,1);
      if ( box_style < 0 )
	{
	  string_not_in_array(stack, box_style_str, box_style_table, "optional argument mode");
	  return RET_BUG;
	}
    }

  if (Mcolors == NULLMAT)
    {
      izcol=0;
    }
  else
    {
      izcol = 1;
      CheckDimProp(NspFname(stack),3,opts[3].position, Mcolors->mn != z->mn  && Mcolors->mn != z->n );
      /*
       *   Added by E Segre 4/5/4000. In the case where zcol is a
       *   matrix of the same size as z, we set izcol to 2. This
       *   value is later transmitted to the C2F(fac3dg) routine,
       *   which has been modified to do the interpolated shading
       *    (see the file SCI/routines/graphics-old/Plo3d.c
       */
      if ( Mcolors->mn == z->mn ) izcol=2  ;
    }

  if ( colormap != NULL && colormap->n != 3)
    {
      Scierror("%s: colormap optional argument should be of size nx3 \n",NspFname(stack));
      ret = RET_BUG;
      goto end;
    }


  if (colormap != NULL &&
      (colormap = (NspMatrix *) nsp_object_copy_and_name("cmap",NSP_OBJECT(colormap))) == NULLMAT)
    {
      ret = RET_BUG;
      goto end;
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
      /* ebox is given then iflag[1] must be 1 or 3 or 5 or 7*/
      if ( iflag[1] == 2 ||  iflag[1] == 4 ||  iflag[1] == 6 || iflag[1] == 8 ) iflag[1]--;
    }
  else
    {
      /* ebox is not given then iflag[1] cannot be 1 or 3 or 5 */
      if ( iflag[1] == 1 ||  iflag[1] == 3 ||  iflag[1] == 5 || iflag[1] == 7 ) iflag[1]++;
    }
  
  if ( opts[iso_opts].obj != NULLOBJ &&  iso == TRUE)
    {
      iflag[1]= (Mebox == NULL) ? 4: 3;
    }
  
  /* change iflag according to other options
   * check that iflag[2] and leg are compatible
   * i.e force visibility of axes names if they are given
   */
  if ( surface_color > 0 ) iflag[0] = surface_color;
  if ( leg !=  NULL && strlen(leg) != 0 ) iflag[2]=4;
  if ( mesh == FALSE && iflag[0] > 0) iflag[0]= -iflag[0];
  if ( mesh_only == TRUE ) iflag[0] =0;
  if ( box_style_str != NULL) iflag[2]= box_style;

  if ( x->mn == 0 || y->mn == 0 || z->mn == 0) { goto end;}

  nsp_gwin_clear();
  if ( x->mn == y->mn && x->mn == z->mn && x->mn != 1)
    {
      NspMatrix *Icol = (Mcolors == NULL) ? NULL: Mat2int(Mcolors);
      int *icol = (Icol == NULL) ? NULL : Icol->I;
      /*  Here we are in the case where x,y and z specify some polygons */
      if (izcol == 0)
	{
	  nsp_ret = (*func1)(x->R,y->R,z->R,izcol,icol,&z->m,&z->n,&theta,
			     &alpha,leg1,iflag,ebox ,colormap,shade,back_color);
	}
      else if (izcol == 2)
	{
	  /*  New case for the fac3d3 call (interpolated shadig)  */
	  nsp_ret = (*func3)(x->R,y->R,z->R,izcol,icol,&z->m,&z->n,&theta,&alpha,leg1,iflag,
			     ebox,colormap,shade,back_color);
	}
      else
	{
	  nsp_ret = (*func2)(x->R,y->R,z->R,izcol,icol,&z->m,&z->n,&theta,&alpha,
			     leg1,iflag,ebox,colormap,shade,back_color);
	}
    }
  else
    {
      /*  Here we are in the standard case  */
      if ( izcol== 2)
	nsp_ret = (*func)(x->R,y->R,z->R,&z->m,&z->n,&theta,&alpha,leg1,
			  iflag,ebox,colormap,shade, Mcolors->R, Mcolors->mn,back_color);
      else
	nsp_ret = (*func)(x->R,y->R,z->R,&z->m,&z->n,&theta,&alpha,leg1,
			  iflag,ebox,colormap,shade, NULL, 0,back_color);
    }
  if ( nsp_ret == NULL)
    {
      ret = RET_BUG;
      goto end;
    }
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(nsp_ret));
      ret = 1;
    }
 end:
  /* if ( Mcolors != NULL)  nsp_matrix_destroy(Mcolors); */
  nsp_matrix_destroy(zloc);
  return ret;

}

/*
 * build z from f(x,y,fargs)
 */

static int plot3d_build_z(Stack stack,NspMatrix *x,NspMatrix *y,NspMatrix *z,NspObject *f, NspObject *fargs, int std)
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

  if (std)
    {
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
	    if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r'
		&& ((NspMatrix *) nsp_ret)->mn==1 )
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
    }
  else
    {
      for ( i= 0 ; i < x->m ; i++)
	for ( j= 0 ; j < x->n; j++)
	  {
	    xi->R[0]= x->R[i+x->m*j];
	    yj->R[0]= y->R[i+x->m*j];
	    targs[0] =(NspObject *) xi;
	    targs[1] = (NspObject *) yj;
	    if ( targs[0]== NULL ||targs[1]== NULL )  goto end;
	    /* FIXME : a changer pour metre une fonction eval standard */
	    if ( nsp_gtk_eval_function((NspPList *)func ,targs,nargs,&nsp_ret,&nret)== FAIL)
	      goto end;
	    if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r'
		&& ((NspMatrix *) nsp_ret)->mn==1 )
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

static void nsp_set_box_parameters(NspObjs3d *objs3d,int flag)
{
  objs3d->obj->with_box =( flag == 0 ) ? FALSE : TRUE;
  objs3d->obj->box_style = (flag == 1 ) ? SCILAB : MATLAB;
}

static NspGraphic *nsp_plot3d_new(double *x, double *y, double *z, int *p, int *q,
				  double *teta, double *alpha,const char *legend, int *flag,
				  double *bbox, NspMatrix *colormap, int shade, double *colors,
				  int ncol, int back_color)
{
  NspPolyhedron *pol;
  NspObjs3d *objs3d;
  if ((objs3d = nsp_check_for_current_objs3d(TRUE)) == NULL) return NULL;
  objs3d->obj->alpha=*alpha;
  objs3d->obj->theta=*teta;
  /* parameter for scales */
  if (flag[1] != 0 ) objs3d->obj->scale_flag = flag[1];

  if (colormap != NULL && objs3d->obj->colormap != NULL)
    {
      nsp_matrix_destroy(objs3d->obj->colormap);
      objs3d->obj->colormap=colormap;
    }
  /* create a polyhedron and insert it in objs3d */
  pol = nsp_polyhedron_create_from_triplet("pol",x,y,z,*p,*q);
  if ( pol == NULL) return NULL;
  /* fix the color according to flag
   * If color is null we will only draw mesh
   */
  pol->obj->Mcolor->I[0]=abs(flag[0]);
  if ( flag[0] < 0) pol->obj->mesh = FALSE;
  /* back color 
   */
  pol->obj->Mback_color->I[0]=back_color;
  /* parameters for box drawing */
  nsp_set_box_parameters(objs3d, flag[2]);

  /* insert the new polyhedron */
  if ( nsp_objs3d_insert_child(objs3d, (NspGraphic *) pol,FALSE)== FAIL)
    {
      Scierror("Error: failed to insert contour in Figure\n");
      return NULL;
    }
  /* if bbox exists we consider that it fixes the bounds */
  nsp_strf_objs3d( objs3d , bbox , flag[1] );
  nsp_objs3d_invalidate(((NspGraphic *) objs3d));
  return (NspGraphic *) pol;
}

static NspGraphic * nsp_plot_fac3d_new(double *x, double *y, double *z,int izcol, int *cvect,
				       int *p, int *q, double *teta, double *alpha,const char *legend,
				       int *flag, double *bbox, NspMatrix *colormap,int shade, int back_color)
{
  NspPolyhedron *pol;
  NspObjs3d *objs3d;
  if ((objs3d = nsp_check_for_current_objs3d(TRUE)) == NULL) return NULL;
  objs3d->obj->alpha=*alpha;
  objs3d->obj->theta=*teta;
  /* parameter for scales */
  if (flag[1] != 0 ) objs3d->obj->scale_flag = flag[1];

  if (colormap != NULL && objs3d->obj->colormap != NULL)
    {
      nsp_matrix_destroy(objs3d->obj->colormap);
      objs3d->obj->colormap=colormap;
    }

  /* create a polyhedron and insert it in objs3d */
  pol = nsp_polyhedron_create_from_facets("pol",x,y,z,*p,*q);
  if ( pol == NULL) return NULL;
  /* fix the color according to flag */
  pol->obj->Mcolor->I[0]=abs(flag[0]);
  if ( flag[0] < 0) pol->obj->mesh = FALSE;
  /* back color 
   */
  pol->obj->Mback_color->I[0]=back_color;
  /* parameters for box drawing */
  nsp_set_box_parameters(objs3d, flag[2]);

  /* insert the new polyhedron */
  if ( nsp_objs3d_insert_child(objs3d, (NspGraphic *) pol,FALSE)== FAIL)
    {
      Scierror("Error: failed to insert contour in Figure\n");
      return NULL;
    }
  nsp_strf_objs3d( objs3d , bbox , flag[1]);
  nsp_objs3d_invalidate(((NspGraphic *) objs3d));
  return (NspGraphic *) pol;
}

static NspGraphic *nsp_plot_fac3d1_new(double *x, double *y, double *z,int izcol, int *cvect,
				       int *p, int *q, double *teta, double *alpha,const char *legend,
				       int *flag, double *bbox, NspMatrix *colormap,int shade, int back_color)
{
  NspSPolyhedron *pol;
  NspObjs3d *objs3d;
  if ((objs3d = nsp_check_for_current_objs3d(TRUE)) == NULL) return NULL;
  objs3d->obj->alpha=*alpha;
  objs3d->obj->theta=*teta;
  /* parameter for scales */
  if (flag[1] != 0 ) objs3d->obj->scale_flag = flag[1];

  if (colormap != NULL && objs3d->obj->colormap != NULL)
    {
      nsp_matrix_destroy(objs3d->obj->colormap);
      objs3d->obj->colormap=colormap;
    }

  /* create a polyhedron and insert it in objs3d */

  pol = nsp_spolyhedron_create_from_facets("pol",x,y,z,*p,*q,cvect,
					   (izcol==1) ? *q : (izcol==2) ? *p*(*q) : 0, 1);
  if ( pol == NULL) return NULL;

  if ( flag[0] < 0) pol->obj->mesh = FALSE;
  if ( flag[0] == 0)
    {
      pol->obj->mesh_only = TRUE;
    }
  pol->obj->shade = shade;

  /* parameters for box drawing */
  nsp_set_box_parameters(objs3d, flag[2]);

  pol->obj->back_color= back_color;

  /* insert the new polyhedron */
  if ( nsp_objs3d_insert_child(objs3d, (NspGraphic *) pol,FALSE)== FAIL)
    {
      Scierror("Error: failed to insert contour in Figure\n");
      return NULL;
    }
  nsp_strf_objs3d( objs3d , bbox , flag[1]);
  nsp_objs3d_invalidate(((NspGraphic *) objs3d));
  return (NspGraphic *) pol;
}

static NspGraphic *nsp_plot3d1_new(double *x, double *y, double *z, int *p, int *q,
				   double *teta, double *alpha,const char *legend, int *flag,
				   double *bbox, NspMatrix *colormap,int shade,
				   double *colors, int ncolor, int back_color)
{
  NspSPolyhedron *pol;
  NspObjs3d *objs3d;
  if ((objs3d = nsp_check_for_current_objs3d(TRUE)) == NULL) return NULL;
  objs3d->obj->alpha=*alpha;
  objs3d->obj->theta=*teta;
  /* parameter for scales */
  if (flag[1] != 0 ) objs3d->obj->scale_flag = flag[1];

  if (colormap != NULL && objs3d->obj->colormap != NULL)
    {
      nsp_matrix_destroy(objs3d->obj->colormap);
      objs3d->obj->colormap=colormap;
    }
  /* create a polyhedron and insert it in objs3d */
  pol = nsp_spolyhedron_create_from_triplet("pol",x,y,z,*p,*q,colors,ncolor);
  if ( pol == NULL) return NULL;

  /* fix the mesh according to flag
   * Note that when flg == 0 we should
   * only draw the mesh
   */
  if ( flag[0] < 0) pol->obj->mesh = FALSE;
  if ( flag[0] == 0)
    {
      pol->obj->mesh_only = TRUE;
    }

  pol->obj->shade = shade;

  pol->obj->back_color= back_color;

  /* parameters for box drawing */
  nsp_set_box_parameters(objs3d, flag[2]);

  /* insert the new polyhedron */
  if ( nsp_objs3d_insert_child(objs3d, (NspGraphic *) pol,FALSE)== FAIL)
    {
      Scierror("Error: failed to insert contour in Figure\n");
      return NULL;
    }
  nsp_strf_objs3d( objs3d , bbox , flag[1]);
  nsp_objs3d_invalidate(((NspGraphic *) objs3d));
  return (NspGraphic *) pol;
}

static int int_plot3d_new( Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs <= 0) return nsp_graphic_demo(NspFname(stack),"t=-%pi:0.3:%pi;plot3d(t,t,sin(t)'*cos(t))",1);
  return int_plot3d_G(stack,rhs,opt,lhs,nsp_plot3d_new,nsp_plot_fac3d_new,
		      nsp_plot_fac3d1_new,nsp_plot_fac3d1_new);
}

static int int_plot3d1_new( Stack stack, int rhs, int opt, int lhs)
{
  const char pl3d[]="t=-%pi:0.3:%pi;plot3d1(t,t,sin(t)'*cos(t),colormap=jetcolormap(64));";
  if ( rhs <= 0) return nsp_graphic_demo(NspFname(stack),pl3d,1);
  return int_plot3d_G(stack,rhs,opt,lhs,nsp_plot3d1_new,nsp_plot_fac3d1_new,
		      nsp_plot_fac3d1_new,nsp_plot_fac3d1_new);
}

/**
 * int_plot2d_G:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 * @func:
 * @colored:
 *
 * Interface to plot2d
 *
 * Returns:
 **/

static int plot2d_build_y(Stack stack,NspMatrix *x,NspMatrix *y,NspObject *f, NspObject *fargs);
typedef int (*func_2d)(BCG *Xgc,char *,double *,double *,int *,int *,int *,char *,const char *,int,double *,int *);

static int int_plot2d_G( Stack stack, int rhs, int opt, int lhs,int force2d,int imode,func_2d func)
{
  NspGraphic *ret;
  /* for 2d optional arguments; */
  int *nax, frame= -1, axes=-1,ncurves,lcurve;
  NspMatrix *Mistyle=NULL, *x=NULL,*y=NULL, *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL;
  NspObject  *args = NULL,*fobj;/* when z is a function */
  double *rect ;
  char *leg=NULL, *strf=NULL, *logflags = NULL, tflag='g', *leg_pos = NULL;
  int leg_posi;
  NspMatrix *Mmark=NULL,*Mmark_size=NULL, *Mmark_color=NULL, *Mline_color=NULL, *Mline_thickness=NULL;
  int auto_axis=TRUE,iso=FALSE, mode_opt;
  char *mode= NULL;

  nsp_option opts_2d[] ={{ "axesflag",s_int,NULLOBJ,-1},
			 { "frameflag",s_int,NULLOBJ,-1},
			 { "leg",string,NULLOBJ,-1},
			 { "leg_pos",string,NULLOBJ,-1},
			 { "logflag",string,NULLOBJ,-1},
			 { "nax",mat_int,NULLOBJ,-1},
			 { "rect",realmat,NULLOBJ,-1},
			 { "strf",string,NULLOBJ,-1},
			 { "style",mat,NULLOBJ,-1}, /* old parameter for mark or line */
			 { "mark",realmat,NULLOBJ,-1},
			 { "mark_size",realmat,NULLOBJ,-1},
			 { "mark_color",realmat,NULLOBJ,-1},
			 { "line_color",realmat,NULLOBJ,-1},
			 { "line_thickness",realmat,NULLOBJ,-1},
			 { "auto_axis",s_bool,NULLOBJ,-1},
			 { "iso",s_bool,NULLOBJ,-1},
			 { "mode",string,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};

  /* keep same order as in opts_2d */
  enum  { axesflag_opts , frameflag_opts, leg_opts, leg_pos_opts,
	  logflag_opts , nax_opts, rect_opts , strf_opts, style_opts,
	  mark_opts, mark_size_opts, mark_color_opts, line_color_opts,
	  line_thickness_opts, auto_axis_opts, iso_opts, mode_opts
  };

  int_types T[] = {realmat,obj,new_opts, t_end} ;

  if ( GetArgs(stack,rhs,opt,T,&x,&fobj,&opts_2d,&axes,&frame,&leg,&leg_pos,&logflags,
	       &Mnax,&Mrect,&strf,&Mstyle,&Mmark,&Mmark_size,&Mmark_color,&Mline_color,
	       &Mline_thickness,&auto_axis,&iso,&mode) == FAIL)
    return RET_BUG;

  if ( mode != NULL)
    {
      const char *mode_names[]={ "std", "stairs", "stem", "arrow", "fill","stairs_fill",  NULL };
      if (( mode_opt= is_string_in_array(mode,mode_names,1)) < 0 )
	{
	  string_not_in_array(stack, mode, mode_names, "optional argument mode");
	  return RET_BUG;
	}
      else
	{
	  imode = mode_opt;
	}
    }

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

  if ( int_check2d(stack,Mstyle,&Mistyle,ncurves,&strf,&leg,&leg_pos,&leg_posi,Mrect,
		   &rect,Mnax,&nax,frame,axes,&logflags) != 0)
    return RET_BUG;

  if ( opts_2d[strf_opts].obj != NULLOBJ)
    {
      /* strf is given use it to fix other arguments */
      if ( opts_2d[auto_axis_opts].obj == NULLOBJ)
	{
	  /* auto_axis not given */
	  auto_axis = TRUE;
	  if ( strf[1] == '1' || strf[1] == '2' || strf[1] == '3' || strf[1] == '4' ) auto_axis=FALSE;
	}
      if ( opts_2d[iso_opts].obj == NULLOBJ)
	{
	  /* iso not given */
	  iso = FALSE;
	  if ( strf[1] == '3' || strf[1] == '4' || strf[1] == '5' || strf[1] == '6' ) iso=TRUE;
	}
      if ( opts_2d[axesflag_opts].obj == NULLOBJ)
	{
	  /* axes not given */
	  axes = strf[2] - '0';
	}
    }

#define CheckArg(x,pos)							\
  if ( x != NULL)							\
    {									\
      CheckLength(NspFname(stack),opts_2d[pos].position, x, ncurves);	\
    }

  CheckArg(Mmark,mark_opts);
  CheckArg(Mmark_size,mark_size_opts);
  CheckArg(Mmark_color,mark_color_opts);
  CheckArg(Mline_color,line_color_opts);
  CheckArg(Mline_thickness,line_thickness_opts);

  /* logflags */
  logflags[0]= tflag;

  nsp_gwin_clear();

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

  {
    NspCurve *curve = NULL;
    const char *l_c = leg, *l_n;
    char *curve_l;
    char c;
    double frect[4],xmin,xmax,ymin,ymax;
    int i;
    NspAxes *axe;
    if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

    axe->obj->lpos = leg_posi;

    for (i=0; i < 4; i++) axe->obj->nax->R[i]= nax[i];

    /* compute frect using brect or using data */
    switch (strf[1])
      {
      case '1' : case '3' : case '5' : case '7': case '9' : case 'B':
	/* frect is given by rect */
	frect[0]=rect[0];frect[1]=rect[1];frect[2]=rect[2];frect[3]=rect[3];
	break;
      case '2' : case '4' : case '6' : case '8': case 'A' : case 'C':
	/* logflag[0] can be e, o , g */
	if ( strlen(logflags) < 1) c='g' ; else c=logflags[0];
	switch ( c )
	  {
	  case 'e' : xmin= 1.0 ; xmax = (lcurve);break;
	  case 'o' : xmax= Maxi(x->R,(lcurve)); xmin= Mini(x->R,(lcurve)); break;
	  case 'g' :
	  default: xmax= Maxi(x->R, (ncurves)*(lcurve)); xmin= Mini(x->R, (ncurves)*(lcurve)); break;
	  }
	ymin=  Mini(y->R, (ncurves)*(lcurve)); ymax=  Maxi(y->R, (ncurves)*(lcurve));
	/* back to default values for  x=[] and y = [] */
	if ( ymin == LARGEST_REAL ) { ymin = 0; ymax = 10.0 ;}
	if ( xmin == LARGEST_REAL ) { xmin = 0; xmax = 10.0 ;}
	frect[0]=xmin;frect[1]=ymin;frect[2]=xmax;frect[3]=ymax;
	break;
      }

    if (axe->obj->fixed == TRUE && (strf[1] == '7' || strf[1] == '8' ))
      {
	/* we merge with axes values */
	frect[0] = Min(frect[0], axe->obj->frect->R[0]);
	frect[2] = Max(frect[2], axe->obj->frect->R[2]);
	frect[1] = Min(frect[1], axe->obj->frect->R[1]);
	frect[3] = Max(frect[3], axe->obj->frect->R[3]);
      }

    /* set the axes frect
     * note that this is also performed below
     */

    switch (strf[1])
      {
      case '0': break;
      default: memcpy(axe->obj->frect->R,frect,4*sizeof(double)); break;
      }

    /* create a set of curves and insert them in axe */
    for ( i = 0 ; i < ncurves ; i++)
      {
	int k;
	NspMatrix *Pts = nsp_matrix_create("Pts",'r',lcurve,2);
	if ( Pts == NULL) return RET_BUG;
	/* XXX: we should have to keep the log flags */
	/* get x-values */
	switch ( logflags[0] )
	  {
	  case 'e' : /* No X-value given by the user */
	    for ( k=0 ; k < (lcurve) ; k++)  Pts->R[k] = k+1.0;
	    break ;
	  case 'o' : /* same X for all_curves */
	    memcpy(Pts->R, x->R, (lcurve)*sizeof(double));
	    break;
	  case 'g' :
	  default: /* x are given for each curves */
	    memcpy(Pts->R, x->R +(lcurve)*i, (lcurve)*sizeof(double));
	    break;
	  }
	memcpy(Pts->R+Pts->m,y->R + (lcurve)*i, (lcurve)*sizeof(double));
	/* get legend for curve i*/
	l_n = l_c; while ( *l_n != '@' && *l_n != '\0') l_n++;
	if ( l_n > l_c )
	  {
	    curve_l = new_nsp_string_n(l_n-l_c +1);
	    if ( curve_l != NULL)
	      {
		strncpy(curve_l,l_c,l_n-l_c+1);
		curve_l[l_n-l_c]='\0';
	      }
	  }
	else
	  {
	    curve_l = NULL;
	  }
	l_c = ( *l_n == '@') ? l_n+1: l_n;
	{
	  int style = Mistyle->R[i];
	  int cu_mark = ( style <= 0 ) ? -style : -2 ;
	  int cu_mark_size = -1;
	  int cu_mark_color = -1;
	  int cu_width = -1;
	  int cu_mode = imode;
	  int cu_color= (style > 0 ) ? style : -2 ;
	  if (Mmark != NULL ) { cu_mark = Mmark->R[i];}
	  if (Mmark_color != NULL ) { cu_mark_color = Mmark_color->R[i];}
	  if (Mmark_size != NULL ) { cu_mark_size = Mmark_size->R[i];}
	  if (Mline_color != NULL ) { cu_color = Mline_color->R[i];}
	  if (Mline_thickness != NULL ) { cu_width = Mline_thickness->R[i];}
	  curve= nsp_curve_create("curve",cu_mark,cu_mark_size,cu_mark_color,cu_width,
				  cu_color,cu_mode,Pts,curve_l,NULL);
	}

	/* insert the new curve */
	if ( nsp_axes_insert_child(axe,(NspGraphic *) curve, FALSE)== FAIL)
	  {
	    Scierror("Error: failed to insert rectangle in Figure\n");
	    return RET_BUG;
	  }
      }

    /* updates the axe information according to optional values */
    if ( opts_2d[iso_opts].obj != NULLOBJ ||  opts_2d[strf_opts].obj ) axe->obj->iso = iso;
    if ( opts_2d[auto_axis_opts].obj != NULLOBJ ||  opts_2d[strf_opts].obj ) axe->obj->auto_axis = auto_axis;
    if ( opts_2d[rect_opts].obj != NULLOBJ )
      {
	memcpy(axe->obj->rect->R,frect,4*sizeof(double));
	memcpy(axe->obj->frect->R,axe->obj->rect->R,4*sizeof(double));
	axe->obj->fixed = TRUE;
      }
    /* update the axesflag if given as options in axesflag or strf */
    if ( opts_2d[axesflag_opts].obj != NULLOBJ  || opts_2d[strf_opts].obj != NULLOBJ)
      {
	axe->obj->axes = axes;
      }
    if ( opts_2d[logflag_opts].obj != NULLOBJ)
      {
	axe->obj->xlog = ( strlen(logflags) >= 1) ? ((logflags[1]=='n') ? FALSE:TRUE) : FALSE;
	axe->obj->ylog=  ( strlen(logflags) >= 2) ? ((logflags[2]=='n') ? FALSE:TRUE) : FALSE;
      }
    nsp_axes_invalidate(((NspGraphic *) axe));
    ret = (NspGraphic *) curve ;
  }

  if ( ret == NULL && ncurves != 0 )
    return RET_BUG;
  if ( Mstyle != Mistyle)
    nsp_matrix_destroy(Mistyle);
  if ( lhs == 1 && ncurves != 0 )
    {
      MoveObj(stack,1,NSP_OBJECT(ret));
      return 1;
    }
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


static int int_plot2d_new( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return nsp_graphic_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,0,0,NULL);
}

static int int_plot2d1_1_new( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return nsp_graphic_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,0,NULL);
}

static int int_plot2d1_2_new( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d2([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return nsp_graphic_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,1,NULL);
}

static int int_plot2d1_3_new( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d3([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return nsp_graphic_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,2,NULL);
}

static int int_plot2d1_4_new( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d4([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[-1,-2,3],rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return nsp_graphic_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,3,NULL);
}

static int int_plot2d1_5_new( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d5([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[1,2,3],rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return nsp_graphic_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,4,NULL);
}

static int int_plot2d1_6_new( Stack stack, int rhs, int opt, int lhs)
{
  static char str[]="x=0:0.1:2*%pi;plot2d6([x;x;x]',[sin(x);sin(2*x);sin(3*x)]',style=[1,2,3],rect=[0,-2,2*%pi,2]);";
  if (rhs == 0) {  return nsp_graphic_demo(NspFname(stack),str,1); }
  return int_plot2d_G(stack,rhs,opt,lhs,1,5,NULL);
}


/**
 * int_grayplot:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_grayplot_new( Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *axe;
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
			 { "auto_axis",s_bool,NULLOBJ,-1},
			 { "iso",s_bool,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};

  /* keep same order as in opts_2d */
  enum  { axesflag_opts ,colminmax_opts, colout_opts, frameflag_opts, leg_opts, leg_pos_opts,
	  logflag_opts , nax_opts, rect_opts , remap_opts, shade_opts, strf_opts, style_opts,
	  zminmax_opts, auto_axis_opts, iso_opts};

  int auto_axis=TRUE,iso=FALSE;
  NspObject  *args = NULL,*fobj;/* when z is a function */
  /* for 2d optional arguments; */
  int *nax, frame= -1, axes=-1, remap=TRUE,shade=FALSE;
  NspMatrix *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL,*Mzminmax=NULL,*Mcolminmax=NULL,*Mcolout=NULL;
  double *rect ;
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;
  int leg_posi;
  int_types T[] = {realmat,realmat,obj,new_opts, t_end} ;
  /* */
  NspMatrix *x,*y,*z,*z1;
  NspGMatrix1 *gm;
  if ( rhs <= 0)
    {
      return nsp_graphic_demo(NspFname(stack), "t=-%pi:0.1:%pi;m=sin(t)'*cos(t);grayplot(t,t,m);",1);
    }

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&fobj,&opts_mp,&axes,&Mcolminmax,&Mcolout,&frame,&leg,&leg_pos,
	       &logflags,&Mnax,&Mrect,&remap,&shade,&strf,&Mstyle,&Mzminmax,
	       &auto_axis,&iso) == FAIL) return RET_BUG;

  CheckVector(NspFname(stack),1,x);
  CheckVector(NspFname(stack),2,y);

  if ( IsNspPList(fobj) )
    {
      /* third argument can be a macro */
      if ((z = nsp_matrix_create(NVOID,'r',x->mn,y->mn))== NULL) return RET_BUG;
      if ( plot3d_build_z(stack,x,y,z,fobj,args,TRUE)== FAIL) goto bug;
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

  if ( z->mn == 0) goto end0;
  if ( z->m == 1 || z->n == 1)
    {
      Scierror("%s: third argument is a vector, expecting a matrix \n",
	       NspFname(stack));
      goto bug;
    }

  if (x->mn != z->m )
    {
      Scierror("%s: arguments %d and %d have incompatible size\n",
	       NspFname(stack),1,3);
      goto bug;
    }
  if ( y->mn != z->n)
    {
      Scierror("%s: arguments %d and %d have incompatible size\n",
	       NspFname(stack),2,3);
      goto bug;
    }

  if ( check_zminmax(stack,NspFname(stack),"zminmax",Mzminmax)== FAIL ) goto bug;
  if ( check_colminmax(stack,NspFname(stack),"colminmax",Mcolminmax)== FAIL) goto bug;
  if ( check_colout(stack,NspFname(stack),"colout",Mcolout)== FAIL) goto bug;

  if ( int_check2d(stack,Mstyle,NULL,z->mn,&strf,&leg,&leg_pos,&leg_posi,Mrect,&rect,
		   Mnax,&nax,frame,axes,&logflags) != 0)
    goto bug;

  nsp_gwin_clear();
  /* colout to be added */
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) goto bug;

  /* create a gmatrix and insert-it in axes */
  if ( ( z1 = (NspMatrix *)  nsp_object_copy_and_name("z",NSP_OBJECT(z))) == NULLMAT) goto bug;
  if ( ( y = (NspMatrix *)  nsp_object_copy_and_name("y",NSP_OBJECT(y))) == NULLMAT) goto bug;
  if ( ( x = (NspMatrix *)  nsp_object_copy_and_name("x",NSP_OBJECT(x))) == NULLMAT) goto bug;
  if ( Mcolminmax != NULL )
    {
      if (( Mcolminmax = (NspMatrix *) nsp_object_copy_and_name("cm",NSP_OBJECT(Mcolminmax))) == NULLMAT)
	goto bug;
    }
  if ( Mzminmax != NULL)
    {
      if ( (Mzminmax  = (NspMatrix *)  nsp_object_copy_and_name("zm",NSP_OBJECT(Mzminmax))) == NULLMAT)
	goto bug;
    }
  if ( Mcolout != NULL)
    {
      if ( (Mcolout = (NspMatrix *) nsp_object_copy_and_name("com",NSP_OBJECT(Mcolout))) == NULLMAT)
	goto bug;
    }
  gm = nsp_gmatrix1_create("gm1",z1,remap,shade,Mcolminmax,Mzminmax,Mcolout,x,y,NULL);
  if ( gm == NULL) goto bug;
  /* insert the new matrix */
  if ( nsp_axes_insert_child(axe,(NspGraphic *)gm, FALSE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      goto bug;
    }
  /* updates the axes scale information */

  /* updates the axe information according to optional values */
  if ( opts_mp[strf_opts].obj != NULLOBJ)
    {
      /* strf is given use it to fix other arguments */
      if ( opts_mp[auto_axis_opts].obj == NULLOBJ)
	{
	  /* auto_axis not given */
	  auto_axis = TRUE;
	  if ( strf[1] == '1' || strf[1] == '2' || strf[1] == '3' || strf[1] == '4' ) auto_axis=FALSE;
	}
      if ( opts_mp[iso_opts].obj == NULLOBJ)
	{
	  /* iso not given */
	  iso = FALSE;
	  if ( strf[1] == '3' || strf[1] == '4' || strf[1] == '5' || strf[1] == '6' ) iso=TRUE;
	}
      if ( opts_mp[axesflag_opts].obj == NULLOBJ)
	{
	  /* axes not given */
	  axes = strf[2] - '0';
	}
    }

  if ( opts_mp[iso_opts].obj != NULLOBJ ||  opts_mp[strf_opts].obj ) axe->obj->iso = iso;
  if ( opts_mp[auto_axis_opts].obj != NULLOBJ ||  opts_mp[strf_opts].obj ) axe->obj->auto_axis = auto_axis;
  if ( opts_mp[rect_opts].obj != NULLOBJ )
    {
      memcpy(axe->obj->rect->R,rect,4*sizeof(double));
      memcpy(axe->obj->frect->R,axe->obj->rect->R,4*sizeof(double));
      axe->obj->fixed = TRUE;
    }
  /* update the axesflag if given as options in axesflag or strf */
  if ( opts_mp[axesflag_opts].obj != NULLOBJ  || opts_mp[strf_opts].obj != NULLOBJ)
    {
      axe->obj->axes = axes;
    }
  if ( opts_mp[logflag_opts].obj != NULLOBJ)
    {
      axe->obj->xlog = ( strlen(logflags) >= 1) ? ((logflags[1]=='n') ? FALSE:TRUE) : FALSE;
      axe->obj->ylog=  ( strlen(logflags) >= 2) ? ((logflags[2]=='n') ? FALSE:TRUE) : FALSE;
    }
  nsp_axes_invalidate(((NspGraphic *) axe));
  if ( IsNspPList(fobj) ) nsp_matrix_destroy(z);
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(gm));
      return 1;
    }
  return 0;
 end0:
  if ( IsNspPList(fobj) ) nsp_matrix_destroy(z);
  return 0;
 bug:
  if ( IsNspPList(fobj) ) nsp_matrix_destroy(z);
  return RET_BUG;
}

/**
 * int_matplot_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_matplot_new(Stack stack, int rhs, int opt, int lhs)
{
  nsp_option opts_mp[] ={{ "axesflag",s_int,NULLOBJ,-1},
			 { "colminmax",mat,NULLOBJ,-1},
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
			 { "auto_axis",s_bool,NULLOBJ,-1},
			 { "iso",s_bool,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};

  /* keep same order as in opts_2d */
  enum  { axesflag_opts ,colminmax_opts, frameflag_opts, leg_opts, leg_pos_opts,
	  logflag_opts , nax_opts, rect_opts , remap_opts, strf_opts, style_opts,
	  zminmax_opts, auto_axis_opts, iso_opts};

  NspAxes *axe;
  NspMatrix *z;
  int auto_axis=TRUE,iso=FALSE;
  /* for 2d optional arguments; */
  int *nax, frame= -1, axes=-1, remap=FALSE;
  NspMatrix *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL,*Mzminmax=NULL,*Mcolminmax=NULL;
  double *rect ;
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;
  int leg_posi;
  int_types T[] = {realmat,new_opts, t_end} ;

  if ( rhs <= 0) {return   nsp_graphic_demo(NspFname(stack),"m=[1,2;3,4];Matplot(m);",1);}

  if ( GetArgs(stack,rhs,opt,T,&z,&opts_mp,&axes,&Mcolminmax,&frame,&leg,&leg_pos,
	       &logflags,&Mnax,&Mrect,&remap,&strf,&Mstyle,&Mzminmax,
	       &auto_axis,&iso) == FAIL) return RET_BUG;

  if ( z->mn == 0) return 0;

  if (Mzminmax != NULL &&  Mzminmax->mn != 2 )
    {
      Scierror("%s: optional argument %s should be of size 2\n",NspFname(stack),"zminmax");
      return RET_BUG;
    }

  if (Mcolminmax != NULL &&  Mcolminmax->mn != 2 )
    {
      Scierror("%s: optional argument %s should be of size 2\n",NspFname(stack),"zminmax");
      return RET_BUG;
    }

  if ( check_zminmax(stack,NspFname(stack),"zminmax",Mzminmax)== FAIL ) return RET_BUG;
  if ( check_colminmax(stack,NspFname(stack),"colminmax",Mcolminmax)== FAIL) return RET_BUG;

  if ( int_check2d(stack,Mstyle,NULL,z->mn,&strf,&leg,&leg_pos,&leg_posi,Mrect,
		   &rect,Mnax,&nax,frame,axes,&logflags) != 0)
    return RET_BUG;

  nsp_gwin_clear();
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  /* create a gmatrix and insert-it in axes */
  if ( ( z = (NspMatrix *)  nsp_object_copy_and_name("z",NSP_OBJECT(z))) == NULLMAT) return RET_BUG;
  if ( Mcolminmax != NULL )
    {
      if (( Mcolminmax = (NspMatrix *) nsp_object_copy_and_name("cm",NSP_OBJECT(Mcolminmax))) == NULLMAT)
	return RET_BUG;
    }
  if ( Mzminmax != NULL)
    {
      if ( (Mzminmax  = (NspMatrix *)  nsp_object_copy_and_name("zm",NSP_OBJECT(Mzminmax))) == NULLMAT)
	return RET_BUG;
    }
  if ( Mrect != NULL)
    {
      if (( Mrect = (NspMatrix *)  nsp_object_copy_and_name("rect",NSP_OBJECT(Mrect)))== NULLMAT)
	return RET_BUG;
    }
  NspGMatrix *gm = nsp_gmatrix_create("gm",z,Mrect,remap,Mcolminmax,Mzminmax,NULL);
  if ( gm == NULL) return RET_BUG;
  /* insert the new matrix */
  if ( nsp_axes_insert_child(axe,(NspGraphic *)gm, FALSE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }

  /* updates the axe information according to optional values */
  if ( opts_mp[iso_opts].obj != NULLOBJ ||  opts_mp[strf_opts].obj ) axe->obj->iso = iso;
  if ( opts_mp[auto_axis_opts].obj != NULLOBJ ||  opts_mp[strf_opts].obj ) axe->obj->auto_axis = auto_axis;
  if ( opts_mp[rect_opts].obj != NULLOBJ )
    {
      memcpy(axe->obj->rect->R,rect,4*sizeof(double));
      memcpy(axe->obj->frect->R,axe->obj->rect->R,4*sizeof(double));
      axe->obj->fixed = TRUE;
    }
  /* update the axesflag if given as options in axesflag or strf */
  if ( opts_mp[axesflag_opts].obj != NULLOBJ  || opts_mp[strf_opts].obj != NULLOBJ)
    {
      axe->obj->axes = axes;
    }
  if ( opts_mp[logflag_opts].obj != NULLOBJ)
    {
      axe->obj->xlog = ( strlen(logflags) >= 1) ? ((logflags[1]=='n') ? FALSE:TRUE) : FALSE;
      axe->obj->ylog=  ( strlen(logflags) >= 2) ? ((logflags[2]=='n') ? FALSE:TRUE) : FALSE;
    }
  nsp_axes_invalidate(((NspGraphic *) axe));
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(gm));
      return 1;
    }
  return 0;
}


/**
 * int_matplot1_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_matplot1_new(Stack stack, int rhs, int opt, int lhs)
{
  nsp_option opts_mp[] ={{ "axesflag",s_int,NULLOBJ,-1},
			 { "colminmax",mat,NULLOBJ,-1},
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
			 { "auto_axis",s_bool,NULLOBJ,-1},
			 { "iso",s_bool,NULLOBJ,-1},
			 { NULL,t_end,NULLOBJ,-1}};

  enum  { axesflag_opts , colminmax_opts, frameflag_opts, leg_opts, leg_pos_opts,
	  logflag_opts , nax_opts, rect_opts , remap_opts, strf_opts, style_opts,
	  zminmax_opts, auto_axis_opts, iso_opts};

  /* for 2d optional arguments; */
  int auto_axis=TRUE,iso=FALSE;
  int *nax, frame= -1, axes=-1, remap=FALSE,leg_posi;
  NspMatrix *Mrect=NULL,*Mnax=NULL,*Mstyle=NULL,*Mzminmax=NULL,*Mcolminmax=NULL;
  double *rect ;
  char *leg=NULL, *strf=NULL, *logflags = NULL, *leg_pos = NULL;
  NspMatrix *M,*Rect;
  NspAxes *axe;

  int_types T[] = {realmat, realmat, new_opts, t_end} ;

  if ( rhs <= 0)
    {
      return nsp_graphic_demo(NspFname(stack),
			      "plot2d([0,10],[0,10],style=0);a=ones(50,50);a= 3*tril(a)+2*a;Matplot1(a,[4,4,9,9]);",1);
    }

  if ( GetArgs(stack,rhs,opt,T,&M,&Rect,&opts_mp,&axes,&Mcolminmax,&frame,&leg,&leg_pos,
	       &logflags,&Mnax,&Mrect,&remap,&strf,&Mstyle,&Mzminmax,&auto_axis,&iso) == FAIL)
    return RET_BUG;

  if (M->mn == 0) { return 0;}

  if ( Rect->mn != 4)
    {
      Scierror("%s: second argument should be of length 4\n",NspFname(stack));
      return RET_BUG;
    }

  if ( check_zminmax(stack,NspFname(stack),"zminmax",Mzminmax)== FAIL ) return RET_BUG;
  if ( check_colminmax(stack,NspFname(stack),"colminmax",Mcolminmax)== FAIL) return RET_BUG;

  if ( int_check2d(stack,Mstyle,NULL,M->mn,&strf,&leg,&leg_pos,&leg_posi,
		   Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0)
    return RET_BUG;

  nsp_gwin_clear();
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  /* create a gmatrix and insert-it in axes */
  if ( ( M = (NspMatrix *)  nsp_object_copy_and_name("M",NSP_OBJECT(M))) == NULLMAT) return RET_BUG;
  if ( Mcolminmax != NULL )
    {
      if (( Mcolminmax = (NspMatrix *) nsp_object_copy_and_name("cm",NSP_OBJECT(Mcolminmax))) == NULLMAT)
	return RET_BUG;
    }
  if ( Mzminmax != NULL)
    {
      if ( (Mzminmax  = (NspMatrix *)  nsp_object_copy_and_name("zm",NSP_OBJECT(Mzminmax))) == NULLMAT)
	return RET_BUG;
    }
  if ( Rect != NULL)
    {
      if (( Rect = (NspMatrix *)  nsp_object_copy_and_name("rect",NSP_OBJECT(Rect)))== NULLMAT)
	return RET_BUG;
    }
  NspGMatrix *gm = nsp_gmatrix_create("gm",M,Rect,remap,Mcolminmax,Mzminmax,NULL);
  if ( gm == NULL) return RET_BUG;
  /* insert the new matrix */
  if ( nsp_axes_insert_child(axe,(NspGraphic *)gm,FALSE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }
  /* updates the axe information according to optional values */
  if ( opts_mp[iso_opts].obj != NULLOBJ ||  opts_mp[strf_opts].obj ) axe->obj->iso = iso;
  if ( opts_mp[auto_axis_opts].obj != NULLOBJ ||  opts_mp[strf_opts].obj ) axe->obj->auto_axis = auto_axis;
  if ( opts_mp[rect_opts].obj != NULLOBJ )
    {
      memcpy(axe->obj->rect->R,rect,4*sizeof(double));
      memcpy(axe->obj->frect->R,axe->obj->rect->R,4*sizeof(double));
      axe->obj->fixed = TRUE;
    }
  /* update the axesflag if given as options in axesflag or strf */
  if ( opts_mp[axesflag_opts].obj != NULLOBJ  || opts_mp[strf_opts].obj != NULLOBJ)
    {
      axe->obj->axes = axes;
    }
  if ( opts_mp[logflag_opts].obj != NULLOBJ)
    {
      axe->obj->xlog = ( strlen(logflags) >= 1) ? ((logflags[1]=='n') ? FALSE:TRUE) : FALSE;
      axe->obj->ylog=  ( strlen(logflags) >= 2) ? ((logflags[2]=='n') ? FALSE:TRUE) : FALSE;
    }

  nsp_axes_invalidate(((NspGraphic *) axe));
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(gm));
      return 1;
    }
  return 0;
}

/*-----------------------------------------------------------
 * driver(driver_name) or  current_driver=driver()
 * change the default driver used at scilab level
 *
 *-----------------------------------------------------------*/

#ifdef WITH_OPENGL
extern Gengine GL_gengine;
#endif

#ifdef WITH_CAIRO
extern Gengine Cairo_gengine;
#endif

extern Gengine XFig_gengine, Pos_gengine, Cairo_gengine;
extern BCG  ScilabGCPos, ScilabGCXfig;

typedef enum { X11_driver, Win_driver, Gtk_driver,  Pos_driver , Fig_driver, Rec_driver } nsp_driver;

static const char *drivers_name[]={ "Gtk", "Win", "X11", "Pos", "Fig", "Rec" , NULL };
static int drivers_id[]={ Gtk_driver, Win_driver, X11_driver,  Pos_driver , Fig_driver, Rec_driver};
static int nsp_current_driver = 0;
static BCG *nsp_current_bcg= NULL ;

static int int_driver(Stack stack, int rhs, int opt, int lhs)
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


/**
 * nsp_gwin_clear:
 *
 * remove the children of the current figure
 * when auto_clear mode is active
 **/

static void  nsp_gwin_clear(void)
{
  NspFigure *F = nsp_get_current_figure();
  if ( F == NULL ) return;
  if ( F->obj->gc->auto_clear == TRUE)
    {
#if 1
      NspObject *Obj =nsp_check_for_current_axes_or_objs3d(FALSE);
      if ( Obj != NULL )
	{
	  if ( IsAxes(Obj) )
	    {
	      nsp_axes_remove_children((NspAxes*) Obj);
	    }
	  else
	    {
	      nsp_objs3d_remove_children((NspObjs3d*) Obj);
	    }
	}
      nsp_figure_invalidate((NspGraphic *) F);
#else	
      nsp_figure_remove_children(F);
      nsp_figure_data_reset(F);
      nsp_figure_invalidate((NspGraphic *) F);
#endif
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
    return check_graphic_window_new(); /* a graphic window */
}

/**
 * int_xarc:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xarc_new(Stack stack, int rhs, int opt, int lhs)
{
  NspGrArc *arc;
  NspAxes *axe;
  double *val=NULL;
  int back=-2,color=-1,width=-1;
  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,6);
  if ( get_arc(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&width) == FAIL) return RET_BUG;

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  if ((arc = nsp_grarc_create("pl",val[0],val[1],val[2],val[3],val[4],val[5],
			      back,width,color,0.0,NULL))== NULL)
    return RET_BUG;
  /* insert the object */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) arc,TRUE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(arc));
      return 1;
    }
  return 0;
}

static int int_xfarc_new(Stack stack, int rhs, int opt, int lhs)
{
  NspGrArc *arc;
  NspAxes *axe;
  double *val=NULL;
  int back=-1,color=-2,width=-1;
  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,6);
  if ( get_arc(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&width) == FAIL) return RET_BUG;

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  if ((arc = nsp_grarc_create("pl",val[0],val[1],val[2],val[3],val[4],val[5],
			      back,width,color,0.0,NULL))== NULL)
    return RET_BUG;
  /* insert the object */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) arc,TRUE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(arc));
      return 1;
    }
  return 0;
}

/**
 * int_xarcs_G:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 * @nrow:
 * @flag:
 *
 * generic function for xrects and xarcs and xfarcs
 * with backward compatibility.
 *
 *
 * Returns:
 **/

static int int_xarcs_G_(Stack stack, int rhs, int opt, int lhs,int nrow,int flag)
{
  int compound = TRUE;
  NspList *L;
  NspCompound *C = NULL;
  NspGraphic *gobj = NULL;
  NspAxes *axe;
  NspMatrix *arcs=NULL;
  NspMatrix *color=NULL;
  NspMatrix *color_std=NULL;
  NspMatrix *background=NULL;
  NspMatrix *thickness=NULL;
  int i;
  nsp_option opts[] ={{ "background", realmat,NULLOBJ,-1},
		      { "color",realmat,NULLOBJ,-1},
		      { "thickness", realmat,NULLOBJ,-1},
		      { "compound", s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(1,2);
  if ((arcs = GetRealMat(stack,1)) == NULLMAT) return RET_BUG;
  CheckRows(NspFname(stack),1,arcs, nrow) ;

  if (rhs - opt == 2)
    {
      /* for backward compatibility */
      if ((color_std= GetRealMatInt(stack,2))  == NULLMAT) return RET_BUG;
      CheckLength(NspFname(stack),2, color_std, arcs->n);
    }

  if ( get_optional_args(stack,rhs,opt,opts,&background,&color,&thickness,&compound) == FAIL)
    return RET_BUG;
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

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  if ( compound == TRUE )
    {
      if ((C= nsp_compound_create("c",NULL,NULL,2,-1,10,NULL))== NULL) return RET_BUG;
      L = C->obj->children;
    }
  else
    {
      L = axe->obj->children;
    }
  for ( i = 0 ; i < arcs->n ; i++)
    {
      double *val = arcs->R + i*(arcs->m);
      int icolor=-1,iback=-1,ithickness=-1;
      if  ( color_std != NULL)
	{
	  switch ( flag ){
	  case 0: /* backward compatibility for rects */
	    if ( color_std->I[i] < 0 )
	      {
		icolor = - color_std->I[i];
		iback  = -2; /* no fill*/
	      }
	    else if ( color_std->I[i] > 0 )
	      {
		icolor = -2;
		iback  =  color_std->I[i];
	      }
	    else
	      {
		icolor = -1; /* default*/
		iback  = -2; /* no fill*/
	      }
	    break;
	  case 1: /* backward compatibility for rects xarcs */
	    iback  = -2;
	    icolor = (color_std->I[i] > 0 ) ? color_std->I[i]   : -1;
	    break;
	  case 2: /* backward compatibility for rects xfarcs */
	    icolor = -2;
	    iback = (color_std->I[i] > 0 ) ? color_std->I[i] : -1;
	    break;
	  }
	}
      else if ( opt == 0)
	{
	  switch ( flag ){
	  case 0: /* backward compatibility for rects */
	    iback=-2;
	    break;
	  case 1: /* backward compatibility for rects xarcs */
	    iback=-2;
	    break;
	  case 2: /* backward compatibility for rects xfarcs */
	    icolor = -2;
	    break;
	  }
	}
      else
	{
	  icolor =( color != NULL) ? color->R[i]: -2;
	  iback  =( background != NULL) ? background->R[i]: -2;
	  ithickness = ( thickness != NULL) ? thickness->R[i]:-1;
	  if ( icolor == -2 && ithickness == -2) icolor=-1;
	}
      if ( nrow == 6 )
	{
	  if ((gobj =(NspGraphic *) nsp_grarc_create("arc",val[0],val[1],val[2],val[3],val[4],val[5],
						     iback,ithickness,icolor,0.0,NULL)) == NULL)
	    return RET_BUG;
	}
      else
	{
	  if ((gobj =(NspGraphic *) nsp_grrect_create("rect",val[0],val[1],val[2],val[3],
						      iback,ithickness,icolor,0.0,NULL))== NULL)
	    return RET_BUG;

	}
      if ( compound == TRUE )
	{
	  /* insert in the compound */
	  if ( nsp_list_end_insert(L,(NspObject *) gobj )== FAIL)
	    return RET_BUG;
	}
      else
	{
	  if (  nsp_axes_insert_child(axe,(NspGraphic *) gobj, TRUE)== FAIL)
	    return RET_BUG;
	}
    }
  if ( compound == TRUE )
    {
      /* insert the compound in the axe */
      if ( nsp_axes_insert_child(axe,(NspGraphic *) C, TRUE)== FAIL)
	{
	  Scierror("Error: in %s failed to insert graphic object in Figure\n",NspFname(stack));
	  return RET_BUG;
	}
    }
  if ( lhs == 1 )
    {
      if ( compound == TRUE )
	{
	  MoveObj(stack,1,NSP_OBJECT(C));
	  return 1;
	}
      else if ( gobj != NULL)
	{
	  MoveObj(stack,1,NSP_OBJECT(gobj));
	  return 1;
	}
    }
  return 0;
}

/**
 * int_xrects:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xrects_new(Stack stack, int rhs, int opt, int lhs)
{
  return int_xarcs_G_(stack,rhs,opt,lhs,4,0);
}

/**
 * int_xarcs:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/
static int int_xarcs_new(Stack stack, int rhs, int opt, int lhs)
{
  return int_xarcs_G_(stack,rhs,opt,lhs,6,1);
}

/**
 * int_xfarcs:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/
static int int_xfarcs_new(Stack stack, int rhs, int opt, int lhs)
{
  return int_xarcs_G_(stack,rhs,opt,lhs,6,2);
}

/**
 * int_xarrows:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xarrows_new(Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *axe;
  NspArrows *pl;
  double arsize=-1.0 ;
  NspMatrix *x,*y,*Mstyle=NULL,*color=NULL;

  int_types T[] = {realmat,realmat,new_opts, t_end} ;

  nsp_option opts[] ={{ "arsize",s_double,NULLOBJ,-1},
		      { "style",mat_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&opts,&arsize,&Mstyle) == FAIL) return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x,y);
  if ( Mstyle != NULL)
    {
      if ( Mstyle->mn != x->mn/2 && Mstyle->mn != 1 ) {
	Scierror("%s: style has a wrong size (%d), expecting (%d) or (1)\n",
		 NspFname(stack),Mstyle->mn,x->mn/2);
	return RET_BUG;
      }
    }

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  if ((x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(x)))== NULL) return RET_BUG;
  if ((y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(y)))== NULL) return RET_BUG;

  if ( Mstyle != NULL)
    {
      if ((color = (NspMatrix *) nsp_object_copy_and_name("color",NSP_OBJECT(Mstyle)))== NULL)
	return RET_BUG;
    }
  /* passer color en entiers ? */
  if ((pl = nsp_arrows_create("ar",x,y,color,arsize,NULL))== NULL)
    return RET_BUG;

  if ( nsp_axes_insert_child(axe,(NspGraphic *) pl, FALSE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }
  nsp_axes_invalidate((NspGraphic *)axe);
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(pl));
      return 1;
    }
  return 0;
}


/**
 * int_xsegs:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/
static int int_xsegs_new(Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *axe;
  NspSegments *pl;
  NspMatrix *x,*y,*Mstyle=NULL,*Mcolor=NULL,*color=NULL,*Mthickness=NULL,*thickness=NULL;

  int_types T[] = {realmat,realmat,new_opts, t_end} ;

  /* style is synonym of color: its is maintained for backward compatibility */

  nsp_option opts[] ={{ "style",mat_int,NULLOBJ,-1},
		      { "color",mat_int,NULLOBJ,-1},
		      { "thickness",mat_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&opts,&Mstyle,&Mcolor,&Mthickness) == FAIL)
    return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x,y);

  if ( Mstyle != NULL)
    {
      if ( Mstyle->mn != x->mn/2 && Mstyle->mn != 1 ) {
	Scierror("%s: style has a wrong length (%d), expecting (%d) or (1)\n",
		 NspFname(stack),Mstyle->mn,x->mn/2);
	return RET_BUG;
      }
    }

  if ( Mcolor != NULL)
    {
      if ( Mcolor->mn != x->mn/2 && Mcolor->mn != 1 ) {
	Scierror("%s: color has a wrong length (%d), expecting (%d) or (1)\n",
		 NspFname(stack),Mcolor->mn,x->mn/2);
	return RET_BUG;
      }
    }

  if ( Mthickness != NULL)
    {
      if ( Mthickness->mn != x->mn/2 && Mthickness->mn != 1 ) {
	Scierror("%s: thickness has a wrong length (%d), expecting (%d) or (1)\n",
		 NspFname(stack),Mthickness->mn,x->mn/2);
	return RET_BUG;
      }
    }
  
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  if ((x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(x)))== NULL) return RET_BUG;
  if ((y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(y)))== NULL) return RET_BUG;
  if ( Mcolor != NULL)
    {
      if ((color = (NspMatrix *) nsp_object_copy_and_name("color",NSP_OBJECT(Mcolor)))== NULL)
	return RET_BUG;
    }
  else
    {
      if ( Mstyle != NULL )
	{
	  if ((color = (NspMatrix *) nsp_object_copy_and_name("color",NSP_OBJECT(Mstyle)))== NULL)
	    return RET_BUG;
	}
    }
  if ( Mthickness != NULL )
    {
      if ((thickness = (NspMatrix *) nsp_object_copy_and_name("thickness",NSP_OBJECT(Mthickness)))== NULL)
	return RET_BUG;
    }

  /* passer color en entiers ? */
  if ((pl = nsp_segments_create("ar",x,y,color,thickness,NULL))== NULL)
    return RET_BUG;

  if ( nsp_axes_insert_child(axe,(NspGraphic *) pl, FALSE)== FAIL)
    {
      Scierror("Error: failed to insert segments in Figure\n");
      return RET_BUG;
    }
  nsp_axes_invalidate((NspGraphic *)axe);
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(pl));
      return 1;
    }
  return 0;
}


/*-----------------------------------------------------------
 * old version : kept for backward compatibility
 *-----------------------------------------------------------*/

static int int_xaxis(Stack stack, int rhs, int opt, int lhs)
{
#if 0
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
#endif 
  Scierror("Error: xaxis is deprecated in nsp new graphics\n");
  return RET_BUG;
}

/*-----------------------------------------------------------
 * [x1,y1,rect]=xchange(x,y,dir)
 * should be changed for new graphics 
 * to convert from i2f we can use 
 * F=get_figure(0);
 * [pt,Axe]=F.axes_pt[x,y];
 * pt are the float coordinates using Axes for conversion 
 * the proper Axe is searched in F.
 *-----------------------------------------------------------*/

static int int_xchange_new(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
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
      scale_i2f(Xgc->scales,l3->R,l4->R,(int *) l1->R,(int *) l2->R,l1->m*l1->n);
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
      scale_f2i(Xgc->scales,l1->R,l2->R,(int *)l3->R,(int *)l4->R,l1->m*l1->n);
    }
  else if ( strncmp(dir,"f2s",3) == 0)
    {
      /* XXXX temporarire */
      if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      CheckLength(NspFname(stack),1,l1,4);
      if ((l3 = nsp_matrix_create(NVOID,'r',l1->m,l1->n)) == NULLMAT ) return RET_BUG;
      if ((l4 =nsp_mat_zeros(l1->m,l1->n)) == NULLMAT ) return RET_BUG;
      scale_f2wrect(Xgc->scales,l1->R,l3->R);
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
      l5->R[0] =  Xgc->scales->Irect.x;
      l5->R[1] =  Xgc->scales->Irect.y;
      l5->R[2] =  Xgc->scales->Irect.width;
      l5->R[3] =  Xgc->scales->Irect.height;
      NSP_OBJECT(l5)->ret_pos = 3;     StackStore(stack,(NspObject *) l5,rhs+3);
    }
  return Max(lhs,2);
}


/**
 * int_xclea_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xclea_new(Stack stack, int rhs, int opt, int lhs)
{
  double *val=NULL;
  CheckRhs(1,4);
  if ( get_rect(stack,rhs,opt,lhs,&val) == FAIL) return RET_BUG;
  Sciprintf("Error: function %s should not be used with new graphics\n",
	    NspFname(stack));
  return 0;
}

/**
 * get_rect:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 * @val:
 *
 *
 *
 * Returns:
 **/
static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val)
{
  NspMatrix *M1;
  int i;
  static double l[4];
  switch ( rhs -opt )
    {
    case 1 :
      if ((M1=GetRealMat(stack,1)) == NULLMAT ) return FAIL;
      CheckLength_(NspFname(stack),1,M1,4,FAIL);
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

/**
 * int_xrect_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * Interface for xrect which mixes xrect and xfrect.
 * The values of the optional parameters are used in the following manner
 * color: If color is not given it will be set to the <<current color>>
 *        If color is given a value of -1 the color will be set at run time using
 *                 the figure default color.
 *        If color is given a value of -2 draw is not performed.
 * back: the same except that the default value is -2
 *
 *
 * Returns: 0 or 1 or %RET_BUG;
 **/

static int int_xrect_new(Stack stack, int rhs, int opt, int lhs)
{
  NspGrRect *rect;
  NspAxes *axe;
  double *val=NULL;

  /* XXX: should be changed since colors start at 0 now
   * back : -2 means do not paint the background
   *        -1 paint with default color
   *        >=0 paint with given color.
   * same for color
   * thickness : -1 current else given
   */

  int back=-2,color=-1,width=-1;
  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,4);
  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&width) == FAIL)
    return RET_BUG;

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  /* create the object */
  if ((rect = nsp_grrect_create("pl",val[0],val[1],val[2],val[3],back,width,color,0.0,NULL))== NULL)
    return RET_BUG;
  /* insert the object in the axe */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) rect, TRUE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(rect));
      return 1;
    }
  return 0;
}

/**
 * int_xfrect_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * Interface for xfrect which mixes xrect and xfrect.
 * The values of the optional parameters are used in the following manner
 * color: If color is not given it will be set to the <<current color>>
 *        If color is given a value of -1 the color will be set at run time using
 *                 the figure default color.
 *        If color is given a value of -2 draw is not performed.
 * back: the same except that the default value is -2
 *
 *
 * Returns: 0 or 1 or %RET_BUG;
 **/

static int int_xfrect_new(Stack stack, int rhs, int opt, int lhs)
{
  NspGrRect *rect;
  NspAxes *axe;
  double *val=NULL;
  int color=-3,stroke_color=-2,width=-1;
  nsp_option opts[] ={{ "color",s_int,NULLOBJ,-1},
		      { "stroke_color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(1,4);
  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&color,&stroke_color,&width) == FAIL) return RET_BUG;

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  if ( opts[0].obj == NULLOBJ) color = -1;
  if ((rect = nsp_grrect_create("pl",val[0],val[1],val[2],val[3],color,width,
				stroke_color,0.0,NULL))== NULL)
    return RET_BUG;
  /* insert the object */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) rect )== FAIL)
    return RET_BUG;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig, axe->obj);
  nsp_axes_invalidate(((NspGraphic *) axe));

  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(rect));
      return 1;
    }
  return 0;
}

/**
 * int_xclear_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * xclear(window-ids,[tape_clean])
 * In new graphics the second argument is no
 * more pertinent. This function remove all the axes
 * contained in the graphics window. Then call
 * an invalidate and a show. gc_reset can be used
 * to reset or not the figure graphic context. The
 * default value of gc_reset is TRUE.
 *
 * Returns:
 **/

static int int_xclear_new(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int ix, gc_reset= TRUE;
  NspMatrix *l1;
  CheckStdRhs(0,2);
  nsp_option opts[] ={{ "gc_reset",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( get_optional_args(stack,rhs,opt,opts,&gc_reset) == FAIL)
    return RET_BUG;

  if (rhs >= 1)
    {
      if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      for (ix = 0 ; ix < l1->mn ; ++ix)
	{
	  int wid = l1->R[ix];
	  if (( Xgc=window_list_search_new(wid)) != NULL)
	    {
	      NspFigure *F =  Xgc->figure;
	      if ( F == NULL ) continue;
	      nsp_figure_remove_children(F);
	      if ( gc_reset == TRUE ) nsp_figure_data_reset(F);
	      nsp_figure_invalidate((NspGraphic *) F);
	      Xgc->graphic_engine->process_updates(Xgc);
	    }
	}
    }
  else
    {
      NspFigure *F = nsp_check_for_current_figure();
      if ( F == NULL) return RET_BUG;
      nsp_figure_remove_children(F);
      if ( gc_reset == TRUE ) nsp_figure_data_reset(F);
      nsp_figure_invalidate((NspGraphic *) F);
      if ( F->obj->Xgc != NULL)
	((BCG *) F->obj->Xgc)->graphic_engine->process_updates(F->obj->Xgc);
    }
  return 0;
}

/**
 * int_xclick:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xclick(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int clearq=FALSE,motion=FALSE,release=FALSE,winall=FALSE,istr=0,key=FALSE,win, iflag, button,imask,iw,i;
  char buf[128];
  int buf_len=128;
  NspSMatrix *S;
  NspMatrix *rep[5];
  double drep[4];
  int cursor = TRUE;
  nsp_option opts[] ={
    { "clearq",s_bool,NULLOBJ,-1}, /* clear mouse queue before waiting for event */
    { "cursor",s_bool,NULLOBJ,-1}, /* change the cursor pixmap */
    { "getmotion",s_bool,NULLOBJ,-1},
    { "getrelease",s_bool,NULLOBJ,-1},
    { "getkey",s_bool,NULLOBJ,-1},
    { "win",s_int,NULLOBJ,-1},
    { "winall",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};

  int_types T[] = {new_opts, t_end} ;

  CheckRhs(0,5);
  CheckLhs(1,5);

  if ( GetArgs(stack,rhs,opt,T,&opts,&clearq,&cursor,&motion,&release,&key,&win,&winall) == FAIL)
    return RET_BUG;

  if ( winall != TRUE &&  opts[5].obj != NULLOBJ)
    {
      /* win=window_id was given */
      win = Max(win,0);
      Xgc = window_list_search_new(win);
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

  /* flags coded in iflag
   */

  iflag = (clearq == TRUE) ? FALSE : TRUE;
  if ( cursor == TRUE ) iflag = iflag | (1 <<2);

  /*
   *  printf("cursor = %s",(iflag & (1<<2)) ? "true" : "false");
   *  printf("iflag = %s",(iflag & 1) ? "true" : "false");
   */

  switch (lhs) {
  case 4 : winall=TRUE;  break;
  case 5 :
    if ( opts[5].obj == NULLOBJ) winall=TRUE;
    istr=buf_len;  /* also get menu */
    break;
  }

  if ( winall )
    {
      int ixrep,iyrep;
      iw=-1;
      Xgc->graphic_engine->xclick_any(Xgc,buf,&button,&imask,&ixrep,&iyrep,&iw,iflag,motion,release,key,istr);
      if (button>=-1)
	{
	  /* NspGraphic *G;  */
	  BCG *Xgc_win =window_list_search_new(iw);
	  /* G=*/ nsp_get_point_axes(Xgc_win,ixrep,iyrep,drep+1);
	}
    }
  else
    {
      /* NspGraphic *G;  */
      int ixrep,iyrep;
      Xgc->graphic_engine->xclick(Xgc,buf,&button,&imask,&ixrep,&iyrep,iflag,motion,release,key,istr);
      /* G=*/ nsp_get_point_axes(Xgc,ixrep,iyrep,drep+1);
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


/**
 * int_xend:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xend_new(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  CheckRhs(-1,0);
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->xend(Xgc);
  return 0;
}

/**
 * int_xgrid_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xgrid_new(Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *axe;
  int style = 0;
  CheckRhs(-1,1);
  if ( rhs == 1) {
    if (GetScalarInt(stack,1,&style) == FAIL) return RET_BUG;
  }
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  axe->obj->grid = style;
  nsp_axes_invalidate((NspGraphic *) axe);
  return 0;
}

/**
 * int_xfpoly_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xfpoly_new(Stack stack, int rhs, int opt, int lhs)
{
  NspPolyline *pl;
  NspAxes *axe;
  int close=TRUE,color=-2,mark=-2,mark_size=-1,mark_color=-1,fill_color=-1,thickness=-1;
  NspMatrix *x,*y;

  nsp_option opts[] ={
    { "color",s_int,NULLOBJ,-1},
    { "fill_color",s_int,NULLOBJ,-1},
    { "thickness",s_int,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(2,3);
  if ((x=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((y=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,x,y);
  if ( get_optional_args(stack,rhs,opt,opts,&color,&fill_color,&thickness) == FAIL) return RET_BUG;
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  if ((x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(x)))== NULL) return RET_BUG;
  if ((y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(y)))== NULL) return RET_BUG;
  if ((pl = nsp_polyline_create("pl",x,y,close,color,mark,mark_size,mark_color,fill_color,thickness,NULL))== NULL)
    return RET_BUG;
  /* insert the polyline */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) pl, TRUE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(pl));
      return 1;
    }
  return 0;
}


/**
 * int_xfpolys_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xfpolys_new(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Mcolors = NULL, *Mthickness= NULL, *Mfill_colors= NULL;
  int compound = TRUE;
  NspList *L;
  NspCompound *C=NULL;
  nsp_option opts[] ={{ "compound", s_bool,NULLOBJ,-1},
		      { "thickness", realmat,NULLOBJ,-1}, /* thickness of lines */
		      { "color", realmat ,NULLOBJ,-1},    /* color to use for drawing */
		      { "fill_color",realmat ,NULLOBJ,-1},    /* color to use for filling  */
		      { NULL,t_end,NULLOBJ,-1}};
  int i;
  NspPolyline *pl= NULL;
  NspAxes *axe;
  int color=-2,mark=-2,mark_size=-1,mark_color=-1,fill_color=-1,thickness=-1;
  NspMatrix *x,*y, *l1=NULL,*l2=NULL,*l3=NULL;
  int v1 = 0;

  CheckStdRhs(2,3);

  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,l1,l2);

  if (rhs - opt == 3)
    {
      if ((l3=GetRealMat(stack,3)) == NULLMAT ) return RET_BUG;
      if ( l3->mn == l1->mn )
	{
	  CheckSameDims(NspFname(stack),1,3,l1,l3);
	  v1=2; /* interpolated shading */
	  if ( l3->m != 3 && l3->m != 4 )
	    {
	      Scierror("%s: interpolated shading only works for polygons of size 3 or 4\n",
		       NspFname(stack));
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

  if ( get_optional_args(stack,rhs,opt,opts,&compound,&Mthickness,&Mcolors,&Mfill_colors) == FAIL)
    return RET_BUG;

  if ( Mthickness != NULL )
    {
      CheckDimProp(NspFname(stack),1,opts[1].position, Mthickness->mn != l1->n && Mthickness->mn != 1)
    }

  if ( Mcolors != NULL )
    {
      CheckDimProp(NspFname(stack),1,opts[2].position, Mcolors->mn != l1->n && Mcolors->mn != 1)
    }

  if ( Mfill_colors != NULL )
    {
      CheckDimProp(NspFname(stack),1,opts[3].position, Mfill_colors->mn != l1->n && Mfill_colors->mn != 1)
    }

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  if ( compound == TRUE )
    {
      if ((C= nsp_compound_create("c",NULL,NULL,2,-1,10,NULL))== NULL) return RET_BUG;
      L = C->obj->children;
    }
  else
    {
      L = axe->obj->children;
    }

  /* loop on the polylines */
  for ( i = 0 ; i < l1->n ; i++)
    {
      fill_color=-2;
      if ((x= nsp_matrix_create("x",'r',l1->m,1))== NULLMAT) return RET_BUG;
      if ((y= nsp_matrix_create("y",'r',l1->m,1))== NULLMAT) return RET_BUG;
      memcpy(x->R,l1->R+i*l1->m,l1->m*sizeof(double));
      memcpy(y->R,l2->R+i*l1->m,l1->m*sizeof(double));
      switch (v1 ){
      case 1:
	/* flat shadind */
	if ( l3->R[i] < 0)
	  {
	    color=-2;
	    fill_color= - l3->R[i];
	  }
	else if ( l3->R[i]==0 )
	  {
	    color=-1;
	  }
	else
	  {
	    color=-1;
	    fill_color = l3->R[i];
	  }
	break;
      case 2:
	/* XX interpolated shading: to be done */
	fill_color= l3->R[i*l1->m];
	break;
      default:
	color=-2;
	break;
      }

      color = (Mcolors != NULL) ? ((Mcolors->mn == 1) ?  Mcolors->R[0] : Mcolors->R[i]) : color;
      fill_color =
	(Mfill_colors != NULL) ? ((Mfill_colors->mn == 1) ?  Mfill_colors->R[0] : Mfill_colors->R[i])
	: fill_color;
      thickness = (Mthickness != NULL) ? ((Mthickness->mn == 1) ? Mthickness->R[0] : Mthickness->R[i]) : -1;

      if ((pl = nsp_polyline_create("pl",x,y,TRUE,color,mark,mark_size,mark_color,fill_color,thickness,NULL))== NULL)
	return RET_BUG;
      /* insert the polyline */
      if ( compound == TRUE )
	{
	  /* insert in the compound */
	  if ( nsp_list_end_insert(L,(NspObject *) pl )== FAIL)
	    return RET_BUG;
	}
      else
	{
	  if (  nsp_axes_insert_child(axe,(NspGraphic *) pl, FALSE )== FAIL)
	    return RET_BUG;
	}
    }
  if ( compound == TRUE )
    {
      /* insert the compound in the axe */
      if ( nsp_axes_insert_child(axe,(NspGraphic *) C, TRUE )== FAIL)
	{
	  Scierror("Error: in %s failed to insert graphic object in Figure\n",NspFname(stack));
	  return RET_BUG;
	}
    }
  if ( lhs == 1 )
    {
      if ( compound == TRUE )
	{
	  MoveObj(stack,1,NSP_OBJECT(C));
	  return 1;
	}
      else if ( pl != NULL )
	{
	  MoveObj(stack,1,NSP_OBJECT(pl));
	  return 1;
	}
    }
  return 0;
}


/**
 * int_xget_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

typedef enum {
  xget_alufunction, xget_background, xget_clipoff, xget_clipping, xget_color, xget_colormap,
  xget_dashes, xget_font , xget_font_size  , xget_foreground, xget_hidden3d,
  xget_lastpattern, xget_line_mode , xget_line_style , xget_mark , xget_mark_size, xget_pattern,
  xget_pixmap,  xget_recording, xget_thickness, xget_use_color, xget_viewport, xget_wdim,
  xget_white, xget_window, xget_wpdim, xget_wpos, xget_wresize, xget_fpf, xget_auto_clear
} xget_enum;

static const char *xget_Table[] = {
  "alufunction", "background", "clipoff",  "clipping",  "color",  "colormap",
  "dashes",    "font",   "font size",    "foreground",  "hidden3d",
  "lastpattern",  "line mode",   "line style",   "mark",   "mark size", "pattern",
  "pixmap", "recording", "thickness",  "use color",  "viewport", "wdim",   "white",   "window",
  "wpdim",   "wpos",  "wresize", "fpf","auto clear",
  NULL
};

static void xget_default_colormap(double *val,int color_id);

static int int_xget_new(Stack stack, int rhs, int opt, int lhs)
{
  int color_arg=-1;
  NspFigureData *Gc=NULL;
  NspFigure *F=NULL;
  BCG *Xgc=NULL;
  NspMatrix *M=NULL;
  int rep, flagx=0, val,i, cl[5],m3,vals[2];

  if ( rhs <= 0) { return nsp_graphic_demo(NspFname(stack),"xsetm();",0);}

  CheckRhs(1,2);
  CheckLhs(0,1);

  if ((rep= GetStringInArray(stack,1,xget_Table,1)) == -1) return RET_BUG;

  if (rhs == 2 )
    {
      if ( rep == xget_color )
	{
	  const char *Table[] = {"black","white","gray","blue","green","lightblue",
			   "red","purple","yellow",NULL};
	  if ((color_arg= GetStringInArray(stack,2,Table,1)) == -1)
	    return RET_BUG;
	}
      else
	{
	  if (GetScalarInt(stack,2,&flagx) == FAIL) return RET_BUG;
	}
    }

  /* we can obtain a default colomap even if no figure are found */
  if ( rep != xget_colormap)
    {
      F = nsp_check_for_current_figure();
      Xgc=nsp_check_graphic_context();
      if ( F == NULL) return RET_BUG;
      Gc = F->obj->gc;
    }
  
  switch (rep)
    {
    case xget_alufunction:
      if ( nsp_move_double(stack,1,(double) 0) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_background:
      if ( nsp_move_double(stack,1,(double) Gc->background) == FAIL) return RET_BUG;
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
      if ( rhs == 2 )
	{
	  if ( Gc->colormap != NULL &&Gc->colormap->mn != 0 )
	    val = Gc->colormap->m ;
	  else
	    val = Xgc->graphic_engine->xget_last(Xgc);
	  if ( nsp_move_double(stack,1,(double) val+color_arg+1) == FAIL) return RET_BUG;
	}
      else
	{
	  if ( nsp_move_double(stack,1,(double) Gc->color) == FAIL) return RET_BUG;
	}
      return 1;
      break;
    case xget_colormap:
      if ( Xgc != NULL )
	{
	  /* flagx can be used if != 0 , to only get color flagx */
	  flagx = Max(flagx,0);
	  /* get colors from current figure or current Xgc if no figure
	   * on Xgc
	   */
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
	      /* first call to just get m3 */
	      Xgc->graphic_engine->xget_colormap(Xgc,&m3,NULL,flagx);
	      if ((M = nsp_matrix_create(NVOID,'r',m3,3))== NULLMAT) return RET_BUG;
	      /* second call to get the colors */
	      Xgc->graphic_engine->xget_colormap(Xgc,&m3,M->R,flagx);
	      StackStore(stack,(NspObject *) M,rhs+1);
	      NSP_OBJECT(M)->ret_pos = 1;
	    }
	}
      else
	{
	  /* default colormap */
	  if ((M = nsp_matrix_create(NVOID,'r',(flagx != 0) ? 1:DEFAULTNUMCOLORS ,3))== NULLMAT)
	    return RET_BUG;
	  xget_default_colormap(M->R,flagx);
	  StackStore(stack,(NspObject *) M,rhs+1);
	  NSP_OBJECT(M)->ret_pos = 1;
	}
      return 1;
      break;
    case xget_dashes:
      if ( nsp_move_double(stack,1,(double) Gc->dashes) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_font:
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= Gc->font; M->R[1]=Gc->font_size;
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case xget_font_size:
      if ( nsp_move_double(stack,1,(double)Gc->font_size ) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_foreground:
      if ( nsp_move_double(stack,1,(double)Gc->foreground) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_hidden3d:
      if ( nsp_move_double(stack,1,(double) Gc->hidden3d) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_lastpattern:
      if ( Gc->colormap != NULL &&Gc->colormap->mn != 0 )
	val = Gc->colormap->m ;
      else
	val = Xgc->graphic_engine->xget_last(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_line_mode:
      if ( nsp_move_double(stack,1,(double) Gc->line_mode ) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_line_style:
      if ( nsp_move_double(stack,1,(double) Gc->line_style ) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_mark:
      if ((M= nsp_matrix_create(NVOID,'r',1,2))==NULLMAT) return RET_BUG;
      M->R[0]= Gc->mark; M->R[1]= Gc->mark_size;
      MoveObj(stack,1,(NspObject *) M);
      return 1;
      break;
    case xget_mark_size:
      if ( nsp_move_double(stack,1,(double) Gc->mark_size) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_pattern:
      val = Xgc->graphic_engine->xget_color(Xgc);
      if ( nsp_move_double(stack,1,(double) Gc->pattern) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_pixmap:
      if ( nsp_move_double(stack,1,(double) Gc->pixmap) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_recording:
      CheckRhs(1,1);
      val= Xgc->graphic_engine->xget_recording(Xgc);
      if ( nsp_move_double(stack,1,(double) val) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_thickness:
      if ( nsp_move_double(stack,1,(double) Gc->thickness) == FAIL) return RET_BUG;
      return 1;
      break;
    case xget_use_color:
      if ( nsp_move_double(stack,1,(double) Gc->use_color) == FAIL) return RET_BUG;
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
      if ( Gc->colormap != NULL &&Gc->colormap->mn != 0 )
	val = Gc->colormap->m ;
      else
	val = Xgc->graphic_engine->xget_last(Xgc);
      if ( nsp_move_double(stack,1,(double) val+2) == FAIL) return RET_BUG;
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
	int val = Gc->auto_clear;
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

static void xget_default_colormap(double *val,int color_id)
{
  int i,m= DEFAULTNUMCOLORS;
  if ( val == NULL ) return;
  if ( color_id != 0 )
    {
      i=Min(Min(DEFAULTNUMCOLORS,color_id),1);
      val[0] = (default_colors[3*i]/(double) 255);
      val[1] = (default_colors[3*i+1]/(double) 255);
      val[2] = (default_colors[3*i+2]/(double) 255);
    }
  else
    {
      /* get all colors */
      for (i = 0; i < m; i++) {
	val[i] = (default_colors[3*i]/(double) 255);
	val[i+ m] = (default_colors[3*i+1]/(double) 255);
	val[i+ 2*m] = (default_colors[3*i+2]/(double) 255);
      }
    }
}

/**
 * int_xinit:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * xinit(name=,wdim=,wpdim=,wresize=,viewport=,file=,opengl=)
 * name: window name
 * wdim: window dimensions
 * wpdim: popupdimension
 * wresize: wresize status
 * viewport: viewport position
 *
 *
 * Returns:
 **/

static int int_xinit(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int v1=-1,opengl=FALSE,cairo=FALSE;
  NspMatrix *wdim=NULL,*wpdim=NULL,*viewport=NULL,*wpos=NULL;
  char *name=NULL, *file=NULL, *mode = NULL;
  const char *Table[] = {"d", "l", "n", "p", "k", NULL};
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
      Scierror("%s: optional argument %s should be of size 2\n",NspFname(stack),"dim");
      return RET_BUG;
    }
  if (wpdim != NULL &&  wpdim->mn != 2 )
    {
      Scierror("%s: optional argument %s should be of size 2\n",NspFname(stack),"popup_dim");
      return RET_BUG;
    }

  if (viewport != NULL && viewport->mn != 2 )
    {
      Scierror("%s: optional argument %s should be of size 2\n",NspFname(stack),"viewport_pos");
      return RET_BUG;
    }

  if (wpos != NULL && wpos->mn != 2 )
    {
      Scierror("%s: optional argument %s should be of size 2\n",NspFname(stack),"popup_pos");
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
						   mode[0],NULL,NULL);
    }
  else
    {
      driver_initgraphic *initg = Cairo_gengine.initgraphic;
      if ( opengl == TRUE )
	{
#ifdef WITH_OPENGL
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
	    'e',NULL,NULL);
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
      Xgc->graphic_engine->xset_wresize(Xgc,0);
    }
  if ( name != NULL )   Xgc->graphic_engine->setpopupname(Xgc,name);
  return 0;
}

/*-----------------------------------------------------------
 * xlfont(font-name,font-id)
 * fonts=xlfont()
 * Warning sz dimensions must be compatible with periX11.c FONTNUMBER
 *-----------------------------------------------------------*/

static int int_xlfont(Stack stack, int rhs, int opt, int lhs)
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

static int int_xnumb(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L = NULL;
  NspCompound *C = NULL;
  int i;
  char buf[1024],format[256];
  nsp_num_formats fmt;
  int iposx = 0; /* left */
  int iposy = 0; /* bottom */
  NspGrstring *grs;
  NspAxes *axe;
  double angle=0.0, h = 0.0, w = 0.0;
  int flagx=0, box=FALSE, fill= GR_no_box, fsiz = -1;
  char *posx=NULL,*posy=NULL;

  nsp_option opts[] ={
    { "angle", s_double, NULLOBJ,-1},
    { "box", s_bool,NULLOBJ,-1}, /* draw a box around the string */
    { "fill", s_bool,NULLOBJ,-1}, /* when (w,h) is given the string must fill the
				  * given box i.e change the font size */
    { "h", s_double,NULLOBJ,-1},
    { "w", s_double,NULLOBJ,-1},
    { "posx", string ,NULLOBJ,-1},    /* position in x */
    { "posy", string ,NULLOBJ,-1},    /* position in y */
    { "size", s_int , NULLOBJ,-1},    /* font size in pixel */
    { NULL,t_end,NULLOBJ,-1}};

  NspMatrix *l1,*l2,*l3,*l5=NULL;

  CheckStdRhs(3,5);
  if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((l2=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  if ((l3=GetRealMat(stack,3)) == NULLMAT ) return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,l1,l2);
  CheckSameDims(NspFname(stack),2,3,l2,l3);

  if ( l3->mn == 0) { return 0;}

  if (rhs -opt >= 5)
    {
      if ((l5=GetRealMat(stack,5)) == NULLMAT ) return RET_BUG;
      CheckSameDims(NspFname(stack),1,5,l1,l5);
    }

  if ( get_optional_args(stack,rhs,opt,opts,&angle,&box,&fill,&h,&w,&posx,&posy,&fsiz) == FAIL)
    return RET_BUG;

  if (rhs -opt >= 4)
    {
      /* box given by non optional argument */
      if (GetScalarInt(stack,4,&flagx) == FAIL) return RET_BUG;
      box= (flagx != 0) ? TRUE : FALSE;
    }

  if ( posx != NULL)
    {
      const char *x_table[] = {"left","center", "right", NULL};
      iposx  = is_string_in_array(posx, x_table,1);
      if ( iposx < 0 )
	{
	  string_not_in_array(stack,posx,x_table,"optional argument posx");
	  return RET_BUG;
	}
    }

  if ( posy != NULL)
    {
      const char *y_table[] = {"bottom","center", "baseline","up", NULL};
      iposy = is_string_in_array(posy, y_table,1);
      if ( iposy  < 0 )
	{
	  string_not_in_array(stack,posy,y_table,"optional argument posy");
	  return RET_BUG;
	}
    }

  if ( w != 0.0 && h != 0.0 )
    {
      fill = ( fill == TRUE ) ? GR_fill_box: GR_in_box;
    }

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  if ((C= nsp_compound_create("c",NULL,NULL,2,-1,10,NULL))== NULL) return RET_BUG;
  L = C->obj->children;

  nsp_init_pr_format (&fmt);
  nsp_matrix_set_format(&fmt,l3) ;
  sprintf(format,"%s",fmt.curr_real_fmt);

  for ( i = 0 ; i < l1->mn ; i++)
    {
      NspSMatrix *Loc;
      sprintf(buf,format,l3->R[i]);
      if ((Loc =nsp_smatrix_create_with_length("str",1,1,strlen(buf)+1)) == NULLSMAT)
	return RET_BUG;
      strcpy(Loc->S[0],buf);
      if ( l5 != NULL ) angle = l5->R[i];
      if (( grs = nsp_grstring_create("str",l1->R[i],l2->R[i],NULL,Loc,angle,w,h,fill,iposx,iposy,fsiz,-1,NULL))== NULL)
	return RET_BUG;
      /* insert the new string in the compound */
      if ( nsp_list_end_insert(L,(NspObject *) grs )== FAIL)
	{
	  Scierror("Error: failed to insert a string in Figure\n");
	  return RET_BUG;
	}
    }

  /* insert the compound in the axe */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) C, TRUE)== FAIL)
    {
      Scierror("Error: in %s failed to insert graphic object in Figure\n",NspFname(stack));
      return RET_BUG;
    }

  if ( rhs -opt < 5) nsp_matrix_destroy(l5);
  return 0;
}

/*-----------------------------------------------------------
 *  xpause(microsecs,events=TRUE or FALSE)
 *  make a pause for given microsecs dealing or not with Gtk events according
 *  to the flag events.
 *-----------------------------------------------------------*/

extern void nsp_pause(int sec_time,int events);

static int int_xpause_new(Stack stack, int rhs, int opt, int lhs)
{
  int sec=0,flag=FALSE;
  CheckRhs(-1,2);
  if (rhs >= 1){ if (GetScalarInt(stack,1,&sec) == FAIL) return RET_BUG;}
  if (rhs >= 2){ if (GetScalarBool(stack,2,&flag) == FAIL) return RET_BUG;}
  nsp_pause(sec,flag);
  return 0;
}

/*-----------------------------------------------------------
 *  xflush(); flush gtk-event
 *-----------------------------------------------------------*/

/* FIXME */
extern void nsp_check_gtk_events(void);

static int int_xflush(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  nsp_check_gtk_events();
  return 0;
}

/**
 * int_xpoly_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *  xpoly(xv,yv, close = %t|%f , color= , thickness= , mark= ,
 *        type = "lines" | "marks")
 *    if mark is set then type is set to xmarks
 *    thickness is only active for xlines
 *    FIXME: mark_size should be added
 *
 *
 * Returns:
 **/

static int int_xpoly_new(Stack stack, int rhs, int opt, int lhs)
{
  NspPolyline *pl;
  NspAxes *axe;
  int close=0,color=-1,mark=-2,mark_size=-1,mark_color=-1,fill_color=-2,thickness=-1;
  char *type= NULL; /* "lines"; */
  NspMatrix *x,*y;

  nsp_option opts[] ={
    { "close",s_bool,NULLOBJ,-1}, /* close the polyline */
    { "color",s_int,NULLOBJ,-1},  /* line color */
    { "mark",s_int,NULLOBJ,-1},   /* put a mark */
    { "thickness",s_int,NULLOBJ,-1}, /* thickness of line */
    { "type",string,NULLOBJ,-1},     /* deprecated */
    { "mark_color",s_int,NULLOBJ,-1},/* color for mark  */
    { "mark_size",s_int,NULLOBJ,-1}, /* size of font for mark  */
    { "fill_color",s_int,NULLOBJ,-1},/* fill color */
    { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(2,2);
  if ((x=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((y=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,x,y);
  if ( get_optional_args(stack,rhs,opt,opts,&close,&color,&mark,&thickness,&type,
			 &mark_color,&mark_size,&fill_color) == FAIL) return RET_BUG;

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  if ( type != NULL)
    {
      /* deprecated parameter */
      if (strncmp(type,"marks",5) == 0)  color=-2;
      else if (strncmp(type,"lines",5) == 0) mark=-2;
      else
	{
	  Scierror("Error: types should be equal to \"lines\" or \"marks\" \n");
	  return RET_BUG;
	}
    }

  if ((x = (NspMatrix *) nsp_object_copy_and_name("x",NSP_OBJECT(x)))== NULL) return RET_BUG;
  if ((y = (NspMatrix *) nsp_object_copy_and_name("y",NSP_OBJECT(y)))== NULL) return RET_BUG;

  if ((pl = nsp_polyline_create("pl",x,y,close,color,mark,mark_size,mark_color,fill_color,thickness,NULL))== NULL)
    return RET_BUG;
  /* insert the object in the axe */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) pl, TRUE)== FAIL)
    {
      Scierror("Error: failed to insert a polyline in Figure\n");
      return RET_BUG;
    }
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(pl));
      return 1;
    }
  return 0;
}

/*-----------------------------------------------------------
 * xpoly_clip(xv,yv,clip_rect,opts)
 * test interface for xpoly with clipping
 *-----------------------------------------------------------*/

static int int_xpoly_clip(Stack stack, int rhs, int opt, int lhs)
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
	  ccolor = Xgc->graphic_engine->xget_color(Xgc);
	  Xgc->graphic_engine->xset_color(Xgc,color);
	}
      if ( opts[2].obj != NULLOBJ)
	{
	  cthick = Xgc->graphic_engine->xget_thickness(Xgc);
	  Xgc->graphic_engine->xset_thickness(Xgc,thick);
	}
    }

  Xgc->graphic_engine->scale->drawpolyline_clip(Xgc,l1->R,l2->R,l2->mn,l3->R,close);

  if ( opt != 0 )
    {
      /* reset to default values */
      if ( opts[1].obj != NULLOBJ) Xgc->graphic_engine->xset_color(Xgc,ccolor);
      if ( opts[2].obj != NULLOBJ) Xgc->graphic_engine->xset_thickness(Xgc,cthick);
    }

  return 0;
}


/**
 * int_xpolys_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * Returns:
 **/

static int int_xpolys_new(Stack stack, int rhs, int opt, int lhs)
{
  int compound = TRUE;
  NspList *L;
  NspCompound *C = NULL;
  NspMatrix *Mcolors = NULL, *Mmarks = NULL, *Mmark_sizes= NULL, *Mthickness= NULL, *Mmark_colors= NULL;
  int close=0,mark_size=-1,mark_color=-1,fill_color=-2,thickness=-1,i;
  NspMatrix *x,*y,*style=NULL;
  NspPolyline *pl= NULL;
  NspAxes *axe;
  nsp_option opts[] ={{ "compound", s_bool,NULLOBJ,-1}, /* returns a compound or the last polyline */
		      { "thickness", realmat,NULLOBJ,-1}, /* thickness of lines */
		      { "mark_size", realmat,NULLOBJ,-1}, /* mark size */
		      { "close", s_bool ,NULLOBJ,-1},   /* close the polylines */
		      { "color", realmat ,NULLOBJ,-1},    /* color to use  */
		      { "mark", realmat ,NULLOBJ,-1},     /*  id of mark  */
		      { "mark_color", realmat ,NULLOBJ,-1},/* color to use for marks */
		      { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(2,3);
  if ((x=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  if ((y=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,x,y);

  if (rhs - opt == 3)
    {
      /* third argument is optional and should be deprecated */
      if ((style=GetRealMatInt(stack,3)) == NULLMAT ) return RET_BUG;
      CheckVector(NspFname(stack),3,style);
      CheckDimProp(NspFname(stack),1,3, style->mn < x->n);
    }

  if ( get_optional_args(stack,rhs,opt,opts,&compound,&Mthickness,&Mmark_sizes,
			 &close,&Mcolors,&Mmarks,&Mmark_colors) == FAIL)
    return RET_BUG;

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  if ( compound == TRUE )
    {
      if ((C= nsp_compound_create("c",NULL,NULL,2,-1,10,NULL))== NULL) return RET_BUG;
      L = C->obj->children;
    }
  else
    {
      L = axe->obj->children;
    }

  if ( Mthickness != NULL )
    {
      CheckDimProp(NspFname(stack),1,opts[1].position, Mthickness->mn != x->n && Mthickness->mn != 1)
    }

  if ( Mmark_sizes != NULL )
    {
      CheckDimProp(NspFname(stack),1,opts[2].position, Mmark_sizes->mn != x->n && Mmark_sizes->mn != 1)
    }

  if ( Mcolors != NULL )
    {
      CheckDimProp(NspFname(stack),1,opts[4].position, Mcolors->mn != x->n && Mcolors->mn != 1)
    }

  if ( Mmarks != NULL )
    {
      CheckDimProp(NspFname(stack),1,opts[5].position, Mmarks->mn != x->n && Mmarks->mn != 1)
    }

  if ( Mmark_colors != NULL )
    {
      CheckDimProp(NspFname(stack),1,opts[6].position, Mmark_colors->mn != x->n && Mmark_colors->mn != 1)
    }

  for ( i = 0 ; i < x->n ; i++)
    {
      int lmark,lcolor;
      NspMatrix *xp,*yp;
      /* color to be used */
      lcolor = (Mcolors != NULL) ? ((Mcolors->mn == 1) ?  Mcolors->R[0] : Mcolors->R[i]) : -1;
      lcolor = ( style == NULL) ? lcolor : (( style->I[i] <= 0) ? -2 : style->I[i]);
      /* mark to be used */
      lmark = (Mmarks != NULL) ? ((Mmarks->mn == 1) ?  Mmarks->R[0] : Mmarks->R[i]) : -2;
      lmark = ( style == NULL) ? lmark : (( style->I[i] <= 0) ? - style->I[i] : -2);
      /* mark_size */
      mark_size = (Mmark_sizes != NULL) ? ((Mmark_sizes->mn == 1) ? Mmark_sizes->R[0] : Mmark_sizes->R[i]) : -1;
      /* mark_color */
      mark_color = (Mmark_colors != NULL) ? ((Mmark_colors->mn == 1) ? Mmark_colors->R[0] : Mmark_colors->R[i]) : -1;
      /* thickness */
      thickness = (Mthickness != NULL) ? ((Mthickness->mn == 1) ? Mthickness->R[0] : Mthickness->R[i]) : -1;

      if ((xp= nsp_matrix_create_from_array("x",1,x->m,x->R + x->m*i,NULL))== NULL) return RET_BUG;
      if ((yp= nsp_matrix_create_from_array("x",1,y->m,y->R + y->m*i,NULL))== NULL) return RET_BUG;
      if ((pl = nsp_polyline_create("pl",xp,yp,close,lcolor,lmark,mark_size,mark_color,
				    fill_color,thickness,NULL))== NULL)
	return RET_BUG;
      /* insert the polyline */
      if ( compound == TRUE )
	{
	  /* insert in the compound */
	  if ( nsp_list_end_insert(L,(NspObject *) pl )== FAIL)
	    return RET_BUG;
	}
      else
	{
	  if (  nsp_axes_insert_child(axe,(NspGraphic *) pl, FALSE)== FAIL)
	    return RET_BUG;
	}
    }

  if ( compound == TRUE )
    {
      /* insert the compound in the axe */
      if ( nsp_axes_insert_child(axe,(NspGraphic *) C, TRUE)== FAIL)
	{
	  Scierror("Error: in %s failed to insert graphic object in Figure\n",NspFname(stack));
	  return RET_BUG;
	}
    }
  /* return something */
  if ( lhs == 1 )
    {
      if ( compound == TRUE )
	{
	  MoveObj(stack,1,NSP_OBJECT(C));
	  return 1;
	}
      else if ( pl != NULL)
	{
	  MoveObj(stack,1,NSP_OBJECT(pl));
	  return 1;
	}
    }
  return 0;
}


/* ximage
 *
 */

static int int_ximage_new(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M1;
  char *fname= NULL,*str=NULL;
  char fname_expanded[FSIZE+1];
  NspGrImage *image;
  NspAxes *axe;
  double *rect=NULL;
  int color=-1,thickness=-1,border=FALSE;
  nsp_option opts[] ={{ "border",s_bool,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(2,2);
  if ((fname = GetString(stack,1)) == (char*)0) return RET_BUG;
  nsp_expand_file_with_exec_dir(&stack,fname,fname_expanded);

  if ((M1=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength_(NspFname(stack),2,M1,4,RET_BUG);
  rect = M1->R;
  if ( get_optional_args(stack,rhs,opt,opts,&border,&color,&thickness) == FAIL)
    return RET_BUG;
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  /* create the object */
  if ((str = nsp_string_copy(fname_expanded))== NULL) return RET_BUG;
  if ((image = nsp_grimage_create("img",rect[0],rect[1],rect[2],rect[3],border,thickness,
				  str,NULL,color,NULL))==NULL)
    return RET_BUG;
  /* insert the object in the axe */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) image, TRUE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(rect));
      return 1;
    }
  return 0;
}


/**
 * int_xselect:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * xselect([winid])
 * raise the current graphics window  or window with id winid
 * which becomes the new current window.
 * windows are created if necessary.
 *
 * Returns:
 **/

static int int_xselect(Stack stack, int rhs, int opt, int lhs)
{
  int win_id;
  BCG *Xgc;
  CheckRhs(0,1);
  if (rhs >= 1)
    {
      if (GetScalarInt(stack,1,&win_id) == FAIL) return RET_BUG;
      win_id = Max(0,win_id);
      if ((Xgc=window_list_search_new(win_id)) != NULL)
	{
	  Xgc->graphic_engine->xset_curwin(win_id,TRUE);
	  Xgc->graphic_engine->xselgraphic(Xgc);
	}
      else
	{
	  /* create a graphic window */
	  Xgc= set_graphic_window_new(win_id);
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



/**
 * int_xset_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

typedef enum  {
  xset_alufunction, xset_background, xset_clipoff, xset_clipping, xset_color, xset_colormap
  , xset_dashes  , xset_default, xset_default_colormap
  , xset_font , xset_font_size , xset_foreground, xset_hidden3d
  , xset_lastpattern, xset_line_mode , xset_line_style , xset_mark , xset_mark_size, xset_pattern
  , xset_pixmap ,xset_recording, xset_thickness, xset_use_color, xset_viewport, xset_wdim , xset_white , xset_window
  , xset_wpdim , xset_wpos, xset_wresize, xset_wshow, xset_wwpc, xset_fpf, xset_auto_clear, xset_clipgrf
  , xset_process_updates
} xset_enum ;

static const char *xset_Table[] = {
  "alufunction", "background", "clipoff", "clipping", "color", "colormap",
  "dashes",     "default",  "default_colormap",
  "font",   "font size",    "foreground",  "hidden3d",
  "lastpattern",  "line mode",   "line style",   "mark",   "mark size", "pattern",
  "pixmap", "recording",  "thickness",  "use color",  "viewport", "wdim",   "white",   "window",
  "wpdim",   "wpos",  "wresize",  "wshow",  "wwpc", "fpf","auto clear", "clipgrf",
  "process_updates", NULL
};

static int int_xset_new(Stack stack, int rhs, int opt, int lhs)
{
  NspFigureData *Gc;
  NspFigure *F;
  BCG *Xgc = NULL;
  const char *auto_clear_values[]= {"off","on",NULL};
  char *info;
  int rep,val,val1;
  NspMatrix *M,*Mc;

  if (rhs <= 0) {return nsp_graphic_demo(NspFname(stack),"xsetm();",0);}

  CheckRhs(1,6);
  CheckLhs(0,1);

  if ((rep= GetStringInArray(stack,1,xset_Table,1)) == -1) return RET_BUG;

  if ( rep != xset_window )
    {
      F = nsp_check_for_current_figure();
      if ( F == NULL) return RET_BUG;
      Gc = F->obj->gc;
      Xgc = F->obj->Xgc;
    }

  switch (rep)
    {
    case xset_alufunction:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      /* not valid in new graphics */
      break;
    case xset_background:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->background = val;
      break;
    case xset_clipoff:
      CheckRhs(1,1);
      /*
	Xgc=nsp_check_graphic_context();
	Xgc->graphic_engine->xset_unclip(Xgc);
      */
      break;
    case xset_clipping:
      /* */
      break;
    case xset_color:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->color = val;
      break;
    case xset_colormap:
      {
      	NspObjs3d *objs3d = NULL;
	CheckRhs(2,2);
	if ( (M = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
	CheckCols(NspFname(stack),2,M,3);
	if (( Mc  = nsp_matrix_create("cmap",'r',M->m,M->n))== NULLMAT) return RET_BUG;
	memcpy(Mc->R, M->R, M->mn*sizeof(double));
	objs3d = nsp_check_for_current_objs3d(FALSE);
	if ( objs3d == NULL )
	  nsp_figure_data_set_colormap(F,Mc);
	else
	  {
	    if (objs3d->obj->colormap != NULL ) nsp_matrix_destroy(objs3d->obj->colormap);
	    objs3d->obj->colormap = Mc;
	  }
      }
      nsp_figure_invalidate((NspGraphic *) F);
      break;
    case xset_dashes:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->dashes = val;
      break;
    case xset_default:
      CheckRhs(1,1);
      nsp_figure_data_reset(F);
      break;
    case xset_default_colormap:
      CheckRhs(1,1);
      /*
	 Xgc=nsp_check_graphic_context();
	 Xgc->graphic_engine->xset_default_colormap(Xgc);
      */
      break;
    case xset_font:
      CheckRhs(2,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->font = val;
      if ( rhs == 3 )
	{
	  if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG;
	  Gc->font_size = val;
	}
      break;
    case xset_font_size:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->font_size = val;
      break;
    case xset_foreground:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->foreground = val;
      break;
    case xset_hidden3d:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->hidden3d = val;
      break;
    case xset_lastpattern:
      CheckRhs(1,1);
      Scierror("lastpattern cannot be set \n");
      return RET_BUG;
      break;
    case xset_line_mode:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->line_mode = val;
      break;
    case xset_line_style:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->line_style = val;
      break;
    case xset_mark:
      CheckRhs(2,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->mark = val;
      if ( rhs == 3 )
	{
	  if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG;
	  Gc->mark_size=val1;
	}
      break;
    case xset_mark_size:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->mark_size=val;
      break;
    case xset_pattern:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->pattern=val;
      break;
    case xset_pixmap:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->pixmap=val;
      break;
    case xset_recording:
      CheckRhs(2,2);
      /* ignore */
      break;
    case xset_thickness:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->thickness=val;
      break;
    case xset_use_color:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      Gc->use_color=val;
      break;
    case xset_viewport:
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG;
      if ( F->obj->Xgc != NULL)
	((BCG *) F->obj->Xgc)->graphic_engine->xset_viewport(F->obj->Xgc,val,val1);
      break;
    case xset_wdim:
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG;
      if ( F->obj->Xgc != NULL)
	((BCG *) F->obj->Xgc)->graphic_engine->xset_windowdim(F->obj->Xgc,val,val1);
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
	Xgc= set_graphic_window_new(Max(val,0));
      /* now we have to check that a Figure is associated
       * to Xgc.
       */
      if ((Xgc = window_list_get_first()) != NULL)
	nsp_check_for_figure(Xgc,TRUE);
      break;
    case xset_wpdim:
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG;
      if ( F->obj->Xgc != NULL)
	((BCG *) F->obj->Xgc)->graphic_engine->xset_popupdim(F->obj->Xgc,val,val1);
      break;
    case xset_wpos:
      CheckRhs(3,3);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,3,&val1) == FAIL) return RET_BUG;
      if ( F->obj->Xgc != NULL)
	((BCG *) F->obj->Xgc)->graphic_engine->xset_windowpos(F->obj->Xgc,val,val1);
      break;
    case xset_wresize:
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&val) == FAIL) return RET_BUG;
      if ( F->obj->Xgc != NULL)
	((BCG *) F->obj->Xgc)->graphic_engine->xset_wresize(F->obj->Xgc,val);
      break;
    case xset_wshow:
      CheckRhs(1,1);
      if ( F->obj->Xgc != NULL)
	((BCG *) F->obj->Xgc)->graphic_engine->xset_show(F->obj->Xgc);
      break;
    case xset_wwpc:
      CheckRhs(1,1);
      if ( F->obj->Xgc != NULL)
	((BCG *) F->obj->Xgc)->graphic_engine->xset_pixmapclear(F->obj->Xgc);
      break;
    case xset_fpf:
      CheckRhs(2,2);
      if ((info = GetString(stack,2)) == (char*)0) return RET_BUG;
      if ( F->obj->Xgc != NULL)
	{
	  if ( strlen(info)== 0)
	    ((BCG *) F->obj->Xgc)->graphic_engine->xset_fpf_def(F->obj->Xgc);
	  else
	    ((BCG *) F->obj->Xgc)->graphic_engine->xset_fpf(F->obj->Xgc,info);
	}
      return 0;
      break;
    case xset_auto_clear:
      CheckRhs(2,2);
      if ( (rep = GetStringInArray(stack,2,auto_clear_values,1)) == -1 ) return RET_BUG;
      Gc->auto_clear=rep;
      return 0;
      break;
    case xset_clipgrf:
      /* In new graphics this has no meaning
       * except changing clip for default axe or objs3d.
       */
      break;
    case  xset_process_updates:
      CheckRhs(1,1);
      if ( F->obj->Xgc != NULL )
	((BCG *) F->obj->Xgc)->graphic_engine->process_updates(F->obj->Xgc);
      break;
    }
  return 0;
}


/**
 * int_xstring:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xstring_G(Stack stack, int rhs, int opt, int lhs, int flag )
{
  int iposx = 0; /* left */
  int iposy = 0; /* bottom */
  NspGrstring *grs;
  NspAxes *axe;
  NspSMatrix *S,*Sk;
  double x,y,angle=0.0, h = 0.0, w = 0.0;
  int flagx=0, box=FALSE, str_color=-1, fill= GR_no_box, fsiz = -1;
  char *posx=NULL,*posy=NULL;

  nsp_option opts[] ={
    { "angle", s_double, NULLOBJ,-1},
    { "box",s_bool,NULLOBJ,-1}, /* draw a box around the string: inactive */
    { "fill",s_bool,NULLOBJ,-1}, /* when (w,h) is given the string must fill the
				  * given box i.e change the font size */
    { "h",s_double,NULLOBJ,-1},
    { "w",s_double,NULLOBJ,-1},
    { "posx", string ,NULLOBJ,-1},    /* position in x */
    { "posy", string ,NULLOBJ,-1},    /* position in y */
    { "size", s_int , NULLOBJ,-1},    /* font size in pixel */
    { "color", s_int, NULLOBJ,-1},    /* font color  */
    { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(3,5);
  CheckLhs(0,1);

  if (GetScalarDouble(stack,1,&x) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,2,&y) == FAIL) return RET_BUG;

  if (( S = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) {  return 0;}

  if (rhs -opt >= 4) {if (GetScalarDouble(stack,4,&angle) == FAIL) return RET_BUG;};
  if (rhs -opt >= 5) {if (GetScalarInt(stack,5,&flagx) == FAIL) return RET_BUG;};

  if ( get_optional_args(stack,rhs,opt,opts,&angle,&box,&fill,&h,&w,&posx,
			 &posy,&fsiz,&str_color) == FAIL)
    return RET_BUG;

  if ( posx != NULL)
    {
      const char *x_table[] = {"left","center", "right", NULL};
      iposx  = is_string_in_array(posx, x_table,1);
      if ( iposx < 0 )
	{
	  string_not_in_array(stack,posx,x_table,"optional argument posx");
	  return RET_BUG;
	}
    }

  if ( posy != NULL)
    {
      const char *y_table[] = {"bottom","center", "baseline","up", NULL};
      iposy = is_string_in_array(posy, y_table,1);
      if ( iposy  < 0 )
	{
	  string_not_in_array(stack,posy,y_table,"optional argument posy");
	  return RET_BUG;
	}
    }

  if ( w != 0.0 && h != 0.0 )
    {
      fill = ( fill == TRUE ) ? GR_fill_box : GR_in_box;
    }

  if ( S->n != 1 )
    {
      if (( Sk =nsp_smatrix_column_concat(S," ",1)) == NULLSMAT) return RET_BUG;
      if ( nsp_object_set_name(NSP_OBJECT(Sk),"text") == FAIL ) return RET_BUG;
    }
  else
    {
      if (( Sk = (NspSMatrix *) nsp_object_copy_and_name("text",NSP_OBJECT(S))) == NULL)
	return RET_BUG;
    }

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  if (( grs = nsp_grstring_create("str",x,y,NULL,Sk,angle,w,h,fill,iposx,iposy,fsiz,str_color,NULL))== NULL)
    return RET_BUG;

  if ( flag )
    {
      /* insert the new string in current axe */
      if ( nsp_axes_insert_child(axe,(NspGraphic *) grs, TRUE)== FAIL)
	{
	  Scierror("Error: failed to insert rectangle in Figure\n");
	  return RET_BUG;
	}
      if ( lhs == 1 )
	{
	  MoveObj(stack,1,NSP_OBJECT(grs));
	  return 1;
	}
    }
  else
    {
      double bounds[4];
      NspMatrix *M;
      /* just compute the bounding box */
      NspGraphic *G= (NspGraphic *) grs;
      G->type->link_figure( G,((NspGraphic *) axe)->obj->Fig,axe->obj);
      /* updates the bounds of the axe */
      if ((M = nsp_matrix_create(NVOID,'r',1,4))== NULLMAT) return RET_BUG;
      G->type->bounds(G,bounds);
      M->R[0]=bounds[0];
      M->R[1]=bounds[3];
      M->R[2]=bounds[2]-bounds[0];
      M->R[3]=bounds[3]-bounds[1];
      nsp_grstring_destroy(grs);
      MoveObj(stack,1,NSP_OBJECT(M));
      return 1;
    }
  return 0;
}

static int int_xstring_new(Stack stack, int rhs, int opt, int lhs)
{
  return int_xstring_G(stack,rhs,opt,lhs,TRUE);
}

static int int_xstringl_new(Stack stack, int rhs, int opt, int lhs)
{
  return int_xstring_G(stack,rhs,opt,lhs,FALSE);
}

/**
 * int_xtitle:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xtitle(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  char demo[]="x=(1:10)';plot2d(x,x);xtitle(['Titre';'Principal'],'x legend ','y legend');";
  NspSMatrix *S;
  int narg;
  if ( rhs <= 0) return nsp_graphic_demo(NspFname(stack),demo,1);
  CheckRhs(1,3);
  Obj = nsp_check_for_current_axes_or_objs3d(TRUE);
  if ( Obj == NULL) return RET_BUG;
  /* create a vfield and insert-it in axes */
  if ( IsAxes(Obj))
    for ( narg = 1 ; narg <= rhs ; narg++)
      {
	nsp_string str;
	if (( S = GetSMatUtf8(stack,narg)) == NULLSMAT) return RET_BUG;
	if ( S->mn == 0 ) continue;
	if (( str =nsp_smatrix_elts_concat(S,"@",1," ",1))== NULL) return RET_BUG;
	switch (narg)
	  {
	  case 1: ((NspAxes *) Obj)->obj->title = str;break;
	  case 2: ((NspAxes *) Obj)->obj->x = str;break;
	  case 3: ((NspAxes *) Obj)->obj->y = str;break;
	  }
	nsp_axes_invalidate(((NspGraphic *) Obj));
      }
  else if ( IsObjs3d(Obj))
    for ( narg = 1 ; narg <= rhs ; narg++)
      {
	nsp_string str;
	if (( S = GetSMatUtf8(stack,narg)) == NULLSMAT) return RET_BUG;
	if ( S->mn == 0 ) continue;
	if (( str =nsp_smatrix_elts_concat(S,"@",1," ",1))== NULL) return RET_BUG;
	switch (narg)
	  {
	  case 1: ((NspObjs3d *) Obj)->obj->title = str;break;
	  }
	nsp_objs3d_invalidate(((NspGraphic *) Obj));
      }
  return 0;
}

/*-----------------------------------------------------------
 * xstringb
 *-----------------------------------------------------------*/

static int int_xstringb(Stack stack, int rhs, int opt, int lhs)
{
  NspGrstring *grs;
  NspAxes *axe;
  char * info;
  int fill = FALSE, fsiz=-1;
  double x,y,w,h,angle=0.0;
  NspSMatrix *S,*Sk;

  CheckRhs(5,6);
  CheckLhs(0,1);

  if (GetScalarDouble(stack,1,&x) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,2,&y) == FAIL) return RET_BUG;
  if ((S = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) return 0;
  if (GetScalarDouble(stack,4,&w) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,5,&h) == FAIL) return RET_BUG;

  if (rhs == 6) {
    if ((info = GetString(stack,6)) == (char*)0) return RET_BUG;
    if ( strncmp(info,"fill",4) == 0)
      fill = TRUE;
    else
      {
	Scierror("%s: optional argument has a wrong value 'fill' expected\n",NspFname(stack));
	return RET_BUG;
      }
  }

  if ( S->n != 1 )
    {
      if (( Sk =nsp_smatrix_column_concat(S," ",1)) == NULLSMAT) return RET_BUG;
      if ( nsp_object_set_name(NSP_OBJECT(Sk),"text") == FAIL ) return RET_BUG;
    }
  else
    {
      if (( Sk = (NspSMatrix *) nsp_object_copy_and_name("text",NSP_OBJECT(S))) == NULL)
	return RET_BUG;
    }

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  if (( grs = nsp_grstring_create("str",x,y,NULL,Sk,angle,w,h,fill,
				  GR_STR_XLEFT, GR_STR_YBOTTOM,fsiz,-1,NULL)) == NULL)
    return RET_BUG;
  /* insert the new string */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) grs )== FAIL)
    return RET_BUG;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig, axe->obj);
  nsp_axes_invalidate(((NspGraphic *) axe));

  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(grs));
      return 1;
    }
  return 0;
}

/**
 * int_xstringc:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * interface for xtringc(rect,S,opts)
 * like xrect + xstring with w and h
 *
 * Returns:
 **/

static int int_xstringc(Stack stack, int rhs, int opt, int lhs)
{
  NspGrstring *grs;
  double *val=NULL, x1,y1;
  NspGrRect *rect;
  NspAxes *axe;
  NspMatrix *M;
  int background=-2,color=-1,thickness=-1;
  int fill=TRUE,size=-1,fontcolor=-1;
  double angle=0.0;
  NspSMatrix *S=NULL,*Sk=NULL;
  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1}, /* xrect background */
		      { "color",s_int,NULLOBJ,-1},      /* xrect color */
		      { "thickness",s_int,NULLOBJ,-1},  /* xrect thickness */
		      { "angle", s_double, NULLOBJ,-1},
		      { "fill",s_bool,NULLOBJ,-1},
		      { "size", s_int , NULLOBJ,-1},
		      { "fontcolor", s_int, NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(2,2);
  CheckLhs(0,1);

  if (( M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,M,4);
  val = M->R;
  if ((S = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
  if ( S->mn == 0 ) return 0;

  if ( get_optional_args(stack,rhs,opt,opts,&background,&color,&thickness,
			 &angle,&fill,&size,&fontcolor) == FAIL)
    return RET_BUG;

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  /* create the object */
  if ( angle != 0.0)
    {
      double R[]={ cos( angle*M_PI/180.), sin( angle*M_PI/180.)};
      x1 = R[0]*( val[0] + val[2]/2) -R[1]*(val[1] - val[3]/2);
      y1 = R[1]*( val[0] + val[2]/2) +R[0]*(val[1] - val[3]/2);
      x1 -= val[2]/2;
      y1 += val[3]/2;
    }
  else
    {
      x1 = val[0];
      y1 = val[1];
    }
  if ((rect = nsp_grrect_create("pl",val[0],val[1],val[2],val[3],background,
				thickness,color,angle,NULL)) == NULL)   return RET_BUG;

  /* insert the object in the axe */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) rect, TRUE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }

  if ( S->n != 1 )
    {
      if (( Sk =nsp_smatrix_column_concat(S," ",1)) == NULLSMAT) return RET_BUG;
      if ( nsp_object_set_name(NSP_OBJECT(Sk),"text") == FAIL ) return RET_BUG;
    }
  else
    {
      if (( Sk = (NspSMatrix *) nsp_object_copy_and_name("text",NSP_OBJECT(S))) == NULL)
	return RET_BUG;
    }

  {
    int iposx=0, iposy=0;
    if (( grs = nsp_grstring_create("str",val[0],val[1]-val[3],NULL,Sk,angle,val[2],val[3],
				    fill,iposx,iposy,size,fontcolor,NULL))== NULL)
      return RET_BUG;
  }

  /* insert the object in the axe */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) grs, TRUE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }

  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(rect));
      return 1;
    }
  return 0;
}

/*-----------------------------------------------------------
 *  corners=xstringbox(x,y,str,angle,fontid,fontsize)
 *  ou
 *  corners=xstringbox(txt)
 *  ou
 *  corners=xstringbox(compound)
 *
 *  A revoir:
 *-----------------------------------------------------------*/

static int int_xstringbox(Stack stack, int rhs, int opt, int lhs)
{
  nsp_figure *F;
  NspAxes *axe;
  BCG *Xgc;
  NspSMatrix *S;
  NspMatrix *M;
  double rect[4],wc=0,x,y,yi,angle;
  int fid,fsiz;
  int i,remove=0;
  int font[2];

  CheckRhs(1,6);
  CheckLhs(0,1);

  if ( IsGraphicObj(stack,1))
    {
      double bounds[4], rect[4];
      NspGraphic *G= GetGraphic(stack,1);
      if ( G == NULL) return RET_BUG;
      G->type->bounds(G,bounds);
      rect[0]=bounds[0];
      rect[1]=bounds[1];
      rect[2]=bounds[2]-bounds[0];
      rect[3]=bounds[3]-bounds[1];
      if ((M = nsp_matrix_create(NVOID,'r',2,4))== NULLMAT) return RET_BUG;
      /* corners of the boundibg box */
      M->R[0]=rect[0];M->R[1]=rect[1];
      M->R[2]=rect[0];M->R[3]=rect[1]+rect[3];
      M->R[4]=rect[0]+rect[2];M->R[5]=rect[1]+rect[3];
      M->R[6]=M->R[4];M->R[7]=M->R[1];
      MoveObj(stack,1,NSP_OBJECT(M));
      return 1;
    }
  else
    {
      if (GetScalarDouble(stack,1,&x) == FAIL) return RET_BUG;
      if (GetScalarDouble(stack,2,&y) == FAIL) return RET_BUG;
      if ((S = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;
      if (S->mn == 0) return 0;
      if (S->n != 1) {
	remove=1;
	if (( S =nsp_smatrix_column_concat(S," ",1)) == NULLSMAT) return RET_BUG;
      }
      if (GetScalarDouble(stack,4,&angle) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,5,&fid) == FAIL) return RET_BUG;
      if (GetScalarInt(stack,6,&fsiz) == FAIL) return RET_BUG;
      if ((axe=nsp_check_for_current_axes(TRUE))== NULL) return FAIL;
      if ((M = nsp_matrix_create(NVOID,'r',2,4))== NULLMAT) return RET_BUG;
      NSP_OBJECT(M)->ret_pos=1;
      StackStore(stack,(NspObject *) M,rhs+1);
      F=((NspGraphic *) axe)->obj->Fig;
      Xgc=F->Xgc;
      *Xgc->scales=axe->obj->scale;
      yi=y;
      Xgc->graphic_engine->xget_font(Xgc,font,FALSE);
      Xgc->graphic_engine->xset_font(Xgc,fid,fsiz,TRUE);
      for(i=S->m -1;i>=0;--i) {
	Xgc->graphic_engine->scale->boundingbox(Xgc,S->S[i],x,y,rect);
	wc=Max(wc,rect[2]);
	if (i != 0 )
	  y += rect[3] * 1.2;
	else
	  y += rect[3];
      }
      Xgc->graphic_engine->xset_font(Xgc,font[0],font[1],TRUE);
      if (remove==1) nsp_smatrix_destroy(S);
      M->R[0]=x;M->R[1]=yi;M->R[2]=x;M->R[3]=y;
      M->R[4]=x+wc;M->R[5]=y;M->R[6]=M->R[4];M->R[7]=M->R[1];
      return 1;
    }
}

/*-----------------------------------------------------------
 * xtape: update manual XXX
 *-----------------------------------------------------------*/

static int int_xtape(Stack stack, int rhs, int opt, int lhs)
{
  int bbox1[4];
  NspObject  *status;
  BCG *Xgc;
  int rep;
  NspMatrix *M;
  int iscflag[3];
  static double  rect_def[4] = { 0,0,10,10}; /*  ebox_def[6] = {0,1,0,1,0,1};*/
  /*
  static int iflag_def[4] = { 0,0,0,0 };
  static int aint_def[4] = { 0,0,0,0 };
  static int flagx_def[3] = { 1,1,1} ;
  */
  /* int *iflag = iflag_def,*/ /* *aint = aint_def,*/ /* *flagx= flagx_def,*/
  int num;
  double alpha = 35.0,theta = 45.0,  *rect = rect_def /* ,*ebox = ebox_def*/ ;

  const char *xtape_Table[] = {"on","clear","replay","replaysc","replayna","off",  NULL };
  CheckRhs(1,7);

  /* first argument is a string in xtape_table */

  if ((rep= GetStringInArray(stack,1,xtape_Table,1)) == -1) return RET_BUG;

  switch (rep)
    {
    case 0 : /* on */
      CheckRhs(1,1);
      if ((status= nsp_new_string_obj(NVOID, "on" ,-1))== NULLOBJ)
	return RET_BUG;
      MoveObj(stack,1, status);
      return 1;
    case 1 : /* clear */
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
      if ((Xgc=window_list_search_new(num)) == NULL) return 0;
      Xgc->actions->erase(Xgc);
      break;
    case 2 : /* replay */
      CheckRhs(2,2);
      if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
      if ((Xgc=window_list_search_new(num)) == NULL) return 0;
      Xgc->actions->replay(Xgc);
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
	  CheckLength(NspFname(stack),4,M,4); /* aint = (int*) M->R; */
	}
      if ((Xgc=window_list_search_new(num)) == NULL) return 0;
      /*
       * change the scales XXX
       */
      bbox1[0]= XDouble2Pixel(Xgc->scales,rect[0]);
      bbox1[1]= YDouble2Pixel(Xgc->scales,rect[1]);
      bbox1[2]= XDouble2Pixel(Xgc->scales,rect[2]);
      bbox1[3]= YDouble2Pixel(Xgc->scales,rect[3]);
      nsp_figure_zoom(Xgc,bbox1);
      Xgc->actions->replay(Xgc);
      break;
    case 4: /* replayna */
      CheckRhs(2,5);
      if (GetScalarInt(stack,2,&num) == FAIL) return RET_BUG;
      if ( rhs >= 3 ) { if (GetScalarDouble(stack,3,&theta) == FAIL) return RET_BUG;}
      if ( rhs >= 4 ) { if (GetScalarDouble(stack,4,&alpha) == FAIL) return RET_BUG;}
      if ( rhs >= 5 ) {
	if ((M= GetRealMatInt(stack,5))  == NULLMAT) return RET_BUG;
	CheckLength(NspFname(stack),5,M,4);/* iflag = (int*) M->R; */
      }
      if ( rhs >= 6 ) {
	if ((M= GetRealMatInt(stack,6))  == NULLMAT) return RET_BUG;
	CheckLength(NspFname(stack),6,M,3); /* flagx = (int*) M->R; */
      }
      /*
      if ( rhs >= 7 ) {
	if ((M= GetRealMat(stack,6))  == NULLMAT) return RET_BUG;
	CheckLength(NspFname(stack),7,M,6); ebox =  M->R;
      }
      */

      if ((Xgc=window_list_search_new(num)) == NULL) return 0;
      /*
       * change the angles
       */
      nsp_figure_change3d_orientation(Xgc,theta,alpha,NULL);
      Xgc->actions->replay(Xgc);
      break;
    case 5: /* off */
      CheckRhs(1,1);
      if ((status= nsp_new_string_obj(NVOID,"on",-1))== NULLOBJ)
	return RET_BUG;
      MoveObj(stack,1,status);
      return 1;
    }
  return 0;
}


/**
 * int_xinfo:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xinfo_new(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char *info;
  CheckRhs(1,1);
  if ((info = GetString(stack,1)) == (char*)0) return RET_BUG;
  Xgc=nsp_check_graphic_context();
  Xgc->graphic_engine->xinfo(Xgc,info);
  return 0;
}

/**
 * int_xsetech
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 * xsetech(wrect=[...],frect=[..],logflag="..", arect=[...])
 * or
 * xsetech(wrect,[frect,logflag])
 * or
 * xsetech()
 *
 * Returns:
 **/

static int int_xsetech_new(Stack stack, int rhs, int opt, int lhs)
{
  int axesflag= -1, iso = -1 , clip=-1, fixed = -1 , axe3d=FALSE;
  NspObject *ret;
  double *wrect =NULL,*frect=NULL,*arect=NULL;
  char *logflag = NULL;
  NspMatrix *M;

  CheckLhs(0,1);
  if ( opt == 0)
    {
      /* compatibility with old version */
      CheckRhs(-1,3);
      CheckLhs(0,1);
      if ( rhs <= 0) { return 0;	}

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
	  Scierror("%s: third argument has a wrong length %d expecting (%d)\n",
		   NspFname(stack),strlen(logflag),2 );
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
			 {"fixed",s_bool,NULLOBJ,-1},
			 {"a3d",s_bool,NULLOBJ,-1},
			 {"axesflag",s_int,NULLOBJ,-1},
			 {"iso",s_bool,NULLOBJ,-1},
			 {"clip",s_bool,NULLOBJ,-1},
			 {NULL,t_end,NULLOBJ,-1},};

      if ( GetArgs(stack,rhs,opt,T,&opts,&Marect,&Mfrect,&logflag,&Mwrect,&fixed,&axe3d,
		   &axesflag, &iso,&clip) == FAIL) return RET_BUG;

      if ( Marect != NULL) {
	arect = Marect->R;CheckLength(NspFname(stack),opts[0].position,Marect,4);
      }

      if ( Mfrect != NULL) {
	frect = Mfrect->R;CheckLength(NspFname(stack),opts[1].position,Mfrect,4);
      }

      if ( Mwrect != NULL) {
	wrect = Mwrect->R;CheckLength(NspFname(stack),opts[3].position,Mwrect,4);
      }

      if ( logflag != NULL )
	{
	  if ( strlen(logflag) != 2 ) {
	    Scierror("%s: logflag argument has a wrong length %d expecting (%d)\n",
		     NspFname(stack),strlen(logflag),2 );
	    return RET_BUG;
	  }
	}
    }

  if ( axe3d == TRUE )
    {
      NspObjs3d *objs3d;
      NspFigure *F = nsp_check_for_current_figure();
      if ( F == NULL) return RET_BUG;
      objs3d = nsp_check_for_objs3d_in_figure(F,wrect,TRUE);
      if (objs3d == NULL) return RET_BUG;
      if ( wrect != NULL)   memcpy(objs3d->obj->wrect->R,wrect,4*sizeof(double));
      if ( arect != NULL)   memcpy(objs3d->obj->arect->R,arect,4*sizeof(double));
      if ( frect != NULL)   memcpy(objs3d->obj->frect->R,frect,4*sizeof(double));
      
      nsp_objs3d_invalidate((NspGraphic *) objs3d);
      ret = (NspObject *) objs3d ;
    }
  else
    {
      NspAxes *axe;
      NspFigure *F = nsp_check_for_current_figure();
      if ( F == NULL) return RET_BUG;
      axe = nsp_check_for_axes_in_figure(F,wrect,TRUE);
      if ( axe ==  NULL) return RET_BUG;

      if ( wrect != NULL)
	{
	  memcpy(axe->obj->wrect->R,wrect,4*sizeof(double));
	}
      if ( arect != NULL)
	{
	  memcpy(axe->obj->arect->R,arect,4*sizeof(double));
	}
      if ( frect != NULL)
	{
	  memcpy(axe->obj->frect->R,frect,4*sizeof(double));
	  memcpy(axe->obj->rect->R,frect,4*sizeof(double));
	  axe->obj->fixed = TRUE;
	}
      if ( logflag != NULL )
	{
	  axe->obj->xlog = (logflag[0]=='n') ? FALSE : TRUE;
	  axe->obj->ylog = (logflag[1]=='n') ? FALSE : TRUE;
	}
      if ( clip != -1) axe->obj->clip = clip;
      if ( axesflag != -1) axe->obj->axes = axesflag;
      if ( iso != -1) axe->obj->iso = iso;
      if ( fixed != -1)
	{
	  if ( frect == NULL)
	    {
	      memcpy(axe->obj->rect->R,axe->obj->frect->R,4*sizeof(double));
	    }
	  axe->obj->fixed = fixed;
	}
      nsp_axes_invalidate((NspGraphic *) axe);
      ret =  (NspObject *) axe;
    }

  if ( lhs == 1 )
    {
      if ((ret =nsp_object_copy(ret)) == NULLOBJ)
	return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(ret));
      return 1;
    }
  return 0;
}


/**
 * int_xgetech_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 * [wrect,frect,logflag,arect]=xgetech()
 *
 * Returns:
 **/

static int int_xgetech_new(Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *axe;
  CheckRhs(0,0);
  CheckLhs(1,4);
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;
  if ( lhs >= 1) MoveObj(stack,1,NSP_OBJECT(axe->obj->wrect));
  if ( lhs >= 2) MoveObj(stack,2,NSP_OBJECT(axe->obj->frect));
  if ( lhs >= 3) if ( nsp_move_string(stack,3,"tobedone",-1) ==FAIL) return RET_BUG;
  if ( lhs >= 4) MoveObj(stack,4,NSP_OBJECT(axe->obj->arect));
  return Max(lhs,0);
}

/**
 * int_fec_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * fec(x,y,triangles,func,...)
 * from the modified version by Bruno 1/2/2001
 *
 *
 * Returns:
 **/

static int int_fec_new(Stack stack, int rhs, int opt, int lhs)
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
			  { "colorbar",s_bool,NULLOBJ,-1},
			  { "paint",s_bool,NULLOBJ,-1},
			  { "auto_axis", s_bool,NULLOBJ,-1},
			  { "iso", s_bool,NULLOBJ,-1},
			  { NULL,t_end,NULLOBJ,-1}};

  /* keep same order as in opts_2d */
  enum  { axesflag_opts , colminmax_opts, colout_opts,
	  frameflag_opts, leg_opts, leg_pos_opts,
	  logflag_opts , mesh_opts, nax_opts, rect_opts , strf_opts, style_opts,
	  zminmax_opts, colorbar_opts, paint_opts, auto_axis_opts, iso_opts};

  int auto_axis=TRUE,iso=FALSE;
  NspAxes *axe;
  NspMatrix *x,*y,*Tr,*Tr1,*F,*Mrect=NULL,*Mnax=NULL,*Mzminmax=NULL;
  NspMatrix *Mcolminmax=NULL,*Mstyle=NULL,*Mcolout=NULL;
  double *rect;
  int *nax,nnz= 10, frame= -1, axes=-1,mesh = FALSE, leg_posi,paint = TRUE;
  int colorbar=TRUE;
  char *strf=NULL, *leg=NULL, *leg_pos = NULL,*logflags=NULL;
  int_types T[] = {realmat,realmat,realmat,realmat,new_opts, t_end} ;
  /* N.n =  4 ; N.names= Names, N.types = Topt, N.objs = Tab; */

  CheckLhs(0,1);

  if ( rhs <= 0) { return nsp_graphic_demo (NspFname(stack)," exec(\"NSP/demos/graphics/fec/fec1.sce\");",1);}

  if ( GetArgs(stack,rhs,opt,T,&x,&y,&Tr,&F,&opts_fec,&axes,&Mcolminmax,&Mcolout,&frame,
	       &leg,&leg_pos,&logflags,&mesh,&Mnax,&Mrect,&strf,&Mstyle,&Mzminmax,
	       &colorbar,&paint,&auto_axis,&iso) == FAIL)
    return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x,y);
  CheckSameDims(NspFname(stack),1,4,x,F);

  if ( Tr->n != 5 &&  Tr->n != 3) {
    Scierror("%s: triangles have %d columns,expecting 3 or 5\n",NspFname(stack),Tr->n);
    return RET_BUG;
  }

  if ( x->mn == 0 || Tr->m == 0) { return 0;}

  if ( check_zminmax(stack,NspFname(stack),"zminmax",Mzminmax)== FAIL ) return RET_BUG;
  if ( check_colminmax(stack,NspFname(stack),"colminmax",Mcolminmax)== FAIL) return RET_BUG;
  if ( check_colout(stack,NspFname(stack),"colout",Mcolout)== FAIL) return RET_BUG;

  if ( int_check2d(stack,Mstyle,NULL,nnz,&strf,&leg,&leg_pos,&leg_posi,
		   Mrect,&rect,Mnax,&nax,frame,axes,&logflags) != 0)
    return RET_BUG;

  nsp_gwin_clear();
  /* colout to be added */
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return RET_BUG;

  /* create a gmatrix and insert-it in axes */
  if ( Tr->n == 3 )
    {
      if ( ( Tr1 = (NspMatrix *)  nsp_object_copy_and_name("Tr",NSP_OBJECT(Tr))) == NULLMAT)
	return RET_BUG;
    }
  else
    {
      if ( ( Tr1 = nsp_matrix_create("Tr",'r',Tr->m,3)) == NULLMAT)
	return RET_BUG;
      /* Tr1 = Tr(:,[2:4]); */
      memcpy(Tr1->R,Tr->R+Tr->m,3*(Tr1->m)*sizeof(double));
    }
  if ( ( F = (NspMatrix *)  nsp_object_copy_and_name("F",NSP_OBJECT(F))) == NULLMAT) return RET_BUG;
  if ( ( y = (NspMatrix *)  nsp_object_copy_and_name("y",NSP_OBJECT(y))) == NULLMAT) return RET_BUG;
  if ( ( x = (NspMatrix *)  nsp_object_copy_and_name("x",NSP_OBJECT(x))) == NULLMAT) return RET_BUG;
  if ( Mcolminmax != NULL )
    {
      if (( Mcolminmax = (NspMatrix *) nsp_object_copy_and_name("cm",NSP_OBJECT(Mcolminmax))) == NULLMAT)
	return RET_BUG;
    }
  if ( Mzminmax != NULL)
    {
      if ( (Mzminmax  = (NspMatrix *)  nsp_object_copy_and_name("zm",NSP_OBJECT(Mzminmax))) == NULLMAT)
	return RET_BUG;
    }
  if ( Mcolout != NULL)
    {
      if ( (Mcolout  = (NspMatrix *)  nsp_object_copy_and_name("co",NSP_OBJECT(Mcolout))) == NULLMAT)
	return RET_BUG;
    }
  if ( paint == FALSE ) mesh= TRUE;

  NspFec *fec = nsp_fec_create("fec",x,y,Tr1,F,Mcolminmax,Mzminmax,mesh,
			       paint,Mcolout,colorbar,NULL);
  if ( fec == NULL) return RET_BUG;
  /* insert the new fec */
  if ( nsp_axes_insert_child(axe,(NspGraphic *) fec, FALSE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      return RET_BUG;
    }
  /* updates the axe information according to optional values */
  if ( opts_fec[strf_opts].obj != NULLOBJ)
    {
      /* strf is given use it to fix other arguments */
      if ( opts_fec[auto_axis_opts].obj == NULLOBJ)
	{
	  /* auto_axis not given */
	  auto_axis = TRUE;
	  if ( strf[1] == '1' || strf[1] == '2' || strf[1] == '3' || strf[1] == '4' ) auto_axis=FALSE;
	}
      if ( opts_fec[iso_opts].obj == NULLOBJ)
	{
	  /* iso not given */
	  iso = FALSE;
	  if ( strf[1] == '3' || strf[1] == '4' || strf[1] == '5' || strf[1] == '6' ) iso=TRUE;
	}
      if ( opts_fec[axesflag_opts].obj == NULLOBJ)
	{
	  /* axes not given */
	  axes = strf[2] - '0';
	}
    }
  if ( opts_fec[iso_opts].obj != NULLOBJ ||  opts_fec[strf_opts].obj ) axe->obj->iso = iso;
  if ( opts_fec[auto_axis_opts].obj != NULLOBJ ||  opts_fec[strf_opts].obj ) axe->obj->auto_axis = auto_axis;
  if ( opts_fec[rect_opts].obj != NULLOBJ )
    {
      memcpy(axe->obj->rect->R,rect,4*sizeof(double));
      memcpy(axe->obj->frect->R,axe->obj->rect->R,4*sizeof(double));
      axe->obj->fixed = TRUE;
    }
  /* update the axesflag if given as options in axesflag or strf */
  if ( opts_fec[axesflag_opts].obj != NULLOBJ  || opts_fec[strf_opts].obj != NULLOBJ)
    {
      axe->obj->axes = axes;
    }
  if ( opts_fec[logflag_opts].obj != NULLOBJ)
    {
      axe->obj->xlog = ( strlen(logflags) >= 1) ? ((logflags[1]=='n') ? FALSE:TRUE) : FALSE;
      axe->obj->ylog=  ( strlen(logflags) >= 2) ? ((logflags[2]=='n') ? FALSE:TRUE) : FALSE;
    }
  nsp_axes_invalidate((NspGraphic *)axe);
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(fec));
      return 1;
    }
  return 0;
}

/**
 * int_xgetmouse:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xgetmouse(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspMatrix *M;
  int clearq=FALSE,motion=TRUE,release=FALSE,key=FALSE, iflag;
  int button,mask;
  int cursor = TRUE;
  double drep[2];
  /* NspGraphic *G;  */
  int ix,iy;
  nsp_option opts[] ={
    { "clearq",s_bool,NULLOBJ,-1},
    { "cursor",s_bool,NULLOBJ,-1}, /* change the cursor pixmap */
    { "getkey",s_bool,NULLOBJ,-1},
    { "getmotion",s_bool,NULLOBJ,-1},
    { "getrelease",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}};

  int_types T[] = {new_opts, t_end} ;

  if ( GetArgs(stack,rhs,opt,T,&opts,&clearq,&cursor,&key,&motion,&release) == FAIL) return RET_BUG;
  Xgc=nsp_check_graphic_context();

  iflag = (clearq == TRUE) ? FALSE : TRUE;
  if ( cursor == TRUE ) iflag = iflag | (1 <<2);

  Xgc->graphic_engine->xgetmouse(Xgc,"xv",&button,&mask,&ix,&iy,iflag,motion,release,key);
  /* G= */ nsp_get_point_axes(Xgc,ix,iy,drep);
  if ((M = nsp_matrix_create(NVOID,'r',1,3))== NULLMAT) return RET_BUG;
  M->R[0] = drep[0];  M->R[1] =drep[1];  M->R[2] = (double) button;
  NSP_OBJECT(M)->ret_pos=1;
  StackStore(stack,(NspObject *) M,rhs+1);
  return 1;
}

/**
 * int_xsave:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xsave_new(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  char *str;
  CheckStdRhs(1,2);

  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if (rhs == 2)
    {
      int wid;
      if (GetScalarInt(stack,2,&wid) == FAIL) return RET_BUG;
      if (( Xgc=window_list_search_new(wid)) == NULL) return 0;
    }
  else
    {
      Xgc = check_graphic_window_new();
    }
  /* tape_save(Xgc,str,wid);*/
  Xgc->actions->savesg(Xgc,str);
  return 0;
}

/**
 * int_xload_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xload_new(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc=NULL;
  int wid;
  char *str;
  CheckRhs(1,2);

  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if (rhs == 2)
    {
      if (GetScalarInt(stack,2,&wid) == FAIL) return RET_BUG;
      Xgc = set_graphic_window_new(Max(0,wid));
    }
  Xgc=check_graphic_window_new();
  /* tape_load(Xgc,str);*/
  Xgc->actions->loadsg(Xgc,str);

  return 0;
}



/**
 * int_xdel_new:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xdel_new(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *l1;
  CheckStdRhs(0,1) ;
  if (rhs == 1)
    {
      int i;
      if ((l1=GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
      for (i = 0 ; i < l1->mn ; ++i)
	{
	  nsp_gr_delete((int) l1->R[i]);
	}
    }
  else
    {
      NspFigure *F = nsp_get_current_figure();
      if ( F == NULL) return 0;
      ((BCG *) F->obj->Xgc)->actions->destroy(F->obj->Xgc);
    }
  return 0;
}

/*-----------------------------------------------------------
 * used to print or export a graphic window
 *-----------------------------------------------------------*/

static int int_export_G(Stack stack, int rhs, int opt, int lhs,const char *export_format)
{
  int win_id,rep=1,color=-1;
  int figure_background=TRUE; /* export with figure background drawing*/
  const char *filename= NULL;
  char *mode = NULL;
  const char *Table[] = {"d", "l", "n", "p", "k", NULL};
  int_types T[] = {s_int,string, new_opts, t_end} ;
  nsp_option opts[] ={{ "color",s_bool,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { "figure_background",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&win_id,&filename,&opts,&color,&mode,&figure_background) == FAIL)
    return RET_BUG;
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
      const char *extension;
      int frep = 0;
      const char *Etable[] = {".svg", ".pdf", ".eps", ".ps", ".fig", ".png", ".tikz", NULL};
      const char *Ftable[] =
	{"cairo-svg", "cairo-pdf", "cairo-ps", "cairo-ps", "Fig", "cairo-png", "Tikz", NULL};
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
  nsp_gr_export(filename,win_id,color,export_format,Table[rep][0],figure_background);
  return 0;
}

/* backward compatibility */

static int int_xs2ps_old(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"Pos");
}

static int int_xs2fig(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"Fig");
}

static int int_xs2pdf(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"cairo-pdf");
}

static int int_xs2png(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"cairo-png");
}

static int int_xs2svg(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"cairo-svg");
}

static int int_xs2ps(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,"cairo-ps");
}

static int int_xexport_new(Stack stack, int rhs, int opt, int lhs)
{
  return int_export_G(stack,rhs,opt,lhs,NULL);
}

/**
 * int_winsid:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * returns a vector containing the ids of opened graphic windows
 *
 * Returns: 1 or %RET_BUG;
 **/

static int int_winsid_new(Stack stack, int rhs, int opt, int lhs)
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

/**
 * int_window_exists:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * checks if the given id correspond to an opened graphic window
 * 
 * Returns: 1 or %RET_BUG;
 **/

static int int_window_exists(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int id;
  CheckRhs(1,1);
  if (GetScalarInt(stack,1,&id) == FAIL) return RET_BUG;
  Xgc=window_list_search_new(id);
  if ( nsp_move_boolean(stack,1, Xgc != NULL)==FAIL) 
    {
      return RET_BUG;
    }
  return 1;
}

/**
 * int_window_newid:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 * returns max(winsid())+1;
 * 
 * Returns: 1 or %RET_BUG;
 **/

static int int_window_newid(Stack stack, int rhs, int opt, int lhs)
{
  int id = window_list_get_max_id()+1;
  CheckRhs(0,0);
  if ( nsp_move_double(stack,1, id) ==FAIL) 
    {
      return RET_BUG;
    }
  return 1;
}

static int int_window_debug_updates(Stack stack, int rhs, int opt, int lhs)
{
  int flag=FALSE;
  CheckStdRhs(1,1);
  if (GetScalarBool(stack,1,&flag) == FAIL) return RET_BUG;
  gdk_window_set_debug_updates(flag);
  return 0;
}

/*-----------------------------------------------------------
 * [xi,xa,np1,np2,kMinr,kMaxr,ar]=xgraduate(xmi,xma)
 * rajouter ds le man XXXX
 *-----------------------------------------------------------*/

static int int_xgraduate(Stack stack, int rhs, int opt, int lhs)
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

/**
 * int_xname:
 * @stack:
 * @rhs:
 * @opt:
 * @lhs:
 *
 *
 *
 * Returns:
 **/

static int int_xname(Stack stack, int rhs, int opt, int lhs)
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

static int int_nxaxis(Stack stack, int rhs, int opt, int lhs)
{
  /* BCG *Xgc; */
  char dir = 'l', *format = NULL, tics = 'v';
  char **val = NULL;
  int fontsize = -1, sub_int=2, seg_flag = 1,textcolor = -1,ticscolor=-1;
  /* double *x = NULL,*y = NULL; */
  int /*nx=0,ny=0 */ ntics;
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

  nsp_check_graphic_context();

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
      /* x = Mx->R; */
      /* nx = Mx->mn; */
    }
  else
    {
      /*
      static double x_def[1];
      nx = 1;
      x = x_def ;
      if ( dir == 'l' )
	x_def[0] = Xgc->scales->frect[0];
      else if ( dir == 'r' )
	x_def[0] = Xgc->scales->frect[2];
      */

    }

  if ( My != NULLMAT )
    {
      /* y = My->R;*/
      /* ny = My->mn;*/
    }
  else
    {
      /*
      static double y_def[1];
      ny = 1;
      y = y_def ;
      if ( dir == 'd' )
	y_def[0] = Xgc->scales->frect[1];
      else if ( dir == 'u' )
	y_def[0] = Xgc->scales->frect[3];
      */
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
      /* sciprint("nombre de tics %d\n",ntics); */
      CheckLength(NspFname(stack), opts[8].position, S,ntics);
    }
  Scierror("%s: Nor implemented \n",  NspFname(stack));
  return RET_BUG;
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

static int int_gtkhelp(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *str[3]={NULL,NULL,NULL};
  CheckRhs(0,1);
  for (i=0; i < rhs ; i++) {
    if ((str[i] = GetString(stack,i+1)) == (char*)0) return RET_BUG;
  }
  nsp_help_browser(NULL,NULL,str[0]);
  return 0;
}


/*-----------------------------------------------------------
 * utilities
 *-----------------------------------------------------------*/

/* seteventhandler("name", win=[w1,...,wn] ) : set handler
 * seteventhandler(win=int) : remove hander
 */

static int int_seteventhandler(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Mw;
  nsp_option opts[] ={{ "win",realmat,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  BCG *Xgc;
  char *info = NULL;
  int ierr=0,win=-1;
  CheckStdRhs(0,1);
  CheckLhs(0,1);
  if ( rhs - opt >=1 )
    {
      if ((info = GetString(stack,1)) == (char*)0) return RET_BUG;
    }
  if ( get_optional_args(stack,rhs,opt,opts,&Mw) == FAIL)
    return RET_BUG;

  if ( opts[0].obj == NULLOBJ)
    {
      Xgc=nsp_check_graphic_context();
      win = Xgc->graphic_engine->xget_curwin();
      nsp_gr_set_graphic_eventhandler(&win,(info==NULL) ? "": info,&ierr);
    }
  else
    {
      int i;
      for ( i=0 ; i < Mw->mn ; i++)
	{
	  win = Max(0, ((int) Mw->R[i]));
	  nsp_gr_set_graphic_eventhandler(&win,(info==NULL) ? "": info,&ierr);
	}
    }
  return 0;
}


/**
 * nsp_graphic_demo:
 * @fname:
 * @code:
 * @flag:
 *
 *
 *
 * Returns:
 **/

static int nsp_graphic_demo (const char *fname,const char *code,int flag)
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

/*
 * There is no way to determine if a function returns a real or a complex
 * number before all the iterations are completed. So we assume it is a
 * complex number.
 */
static int feval_system(int dim,double x,double y,doubleC * val,feval_data *feval)
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
      val->r = ((NspMatrix *) nsp_ret)->R[0];
      val->i = 0.;
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'c'
           && ((NspMatrix *) nsp_ret)->mn==1 )
    {
      *val = ((NspMatrix *) nsp_ret)->C[0];
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else
    {
      Scierror("Error: evaluation failed in feval \n");
      return FAIL;
    }
  return OK;
}


static int int_feval( Stack stack, int rhs, int opt, int lhs)
{
  int i,j,dim=1;
  char ret_type;
  NspMatrix *M=NULL, *Mreal=NULL;
  feval_data feval;
  NspObject *f= NULL;
  NspList *args=NULL;
  NspMatrix *x=NULL,*y=NULL;
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
      if ((M = nsp_matrix_create(NVOID,'c',x->m,x->n))== NULLMAT) goto bug;
      for ( i = 0 ; i < x->mn ; i++)
	{
	  if ( feval_system(dim,x->R[i],0,&M->C[i],&feval)==FAIL)
	    goto bug;
	}
    }
  else
    {
      if ((M = nsp_matrix_create(NVOID,'c',x->mn,y->mn))== NULLMAT) goto bug;
      for ( i = 0 ; i < x->mn ; i++)
	for ( j = 0 ; j < y->mn ; j++)
	  {
	    if ( feval_system(dim,x->R[i],y->R[j],&M->C[i+M->m*j],&feval)==FAIL)
	      goto bug;
	  }
    }
  /*
   * Because there is no way to determine whether a function returns a real or
   * complex number, we have done all the iterations assuming the return
   * values are complex. Looping over all of them to check if they weren't
   * actually real
   */
  ret_type = 'r';
  for (i = 0; i<M->mn; i++)
    {
      if (M->C[0].i != 0.)
        {
          ret_type = 'c';
          break;
        }
    }
  if (ret_type == 'r')
    {
      if ((Mreal = nsp_matrix_create(NVOID,'r',M->m, M->n))== NULLMAT) goto bug;
      for (i=0; i<M->mn; i++) Mreal->R[i] = M->C[i].r;
      nsp_matrix_destroy(M);
      M = Mreal;
    }

  feval_clean(dim,&feval);
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
 bug:
  if ( M != NULL )  nsp_matrix_destroy(M);
  feval_clean(dim,&feval);
  return RET_BUG;
}

#include "nsp/gtk/gdkimage.h"
#include "nsp/gtk/gdkpixbuf.h"

extern GdkPixbuf* nsp_get_pixbuf(BCG *Xgc) ;

#ifdef KEEP_GTK2
extern GdkImage* nsp_get_image(BCG *Xgc) ;


/* get the content of a graphic window as an image.
 *
 */

static int int_get_image( Stack stack, int rhs, int opt, int lhs)
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

#endif

/* get the content of a graphic window as a pixbuf
 *
 */

static int int_get_pixbuf( Stack stack, int rhs, int opt, int lhs)
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

static int int_show_pixbuf( Stack stack, int rhs, int opt, int lhs)
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
 * xdraw_pixbuf(gtk_logo_pixbuf,0,0,2,0,1,1)
 */

static int int_draw_pixbuf( Stack stack, int rhs, int opt, int lhs)
{
  NspAxes *axe;
  NspGPixbuf *gp;
  /* window , pix,  src_x,src_y, dest_x,dest_y,  width,height */
  int_types T[] = { obj_check, s_int, s_int, s_double, s_double, s_double, s_double ,t_end};
  int src_x, src_y;
  double  dest_x, dest_y, width, height;
  NspGObject *pixbuf;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &pixbuf, &src_x, &src_y,
	       &dest_x, &dest_y, &width, &height) == FAIL) return RET_BUG;
  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) goto bug;
  nsp_type_gpixbuf = new_type_gpixbuf(T_BASE);
  gp = nsp_gpixbuf_create("pix",src_x,src_y,dest_x,dest_y,width,height,pixbuf,
			  (NspTypeBase *)nsp_type_gpixbuf);
  if ( gp == NULL) goto bug;
  /* insert the new matrix */
  if ( nsp_axes_insert_child(axe,(NspGraphic *)gp, FALSE)== FAIL)
    {
      Scierror("Error: failed to insert rectangle in Figure\n");
      goto bug;
    }
  nsp_graphic_invalidate((NspGraphic *) gp);
  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(gp));
      return 1;
    }
  return 0;
 bug:
  return RET_BUG;
}

/* experimental: draw a pixbuf in a region of a graphic window.
 * gtk_logo = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
 * xdraw_pixbuf_from_file(0,gtk_logo_pixbuf,0,0,2,0,1,1)
 */

static int int_draw_pixbuf_from_file( Stack stack, int rhs, int opt, int lhs)
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

static int scicos_draw_3d_new(double r[],int color,double size3d)
{
  int mark=-2,mark_size=-2,mark_color=-2,thickness=1,fill_color=-2; /* color; */
  NspPolyline *pl;
  NspMatrix *Mx,*My;
  NspObject *gobj;
  int npt=6;
  NspAxes *axe;
  double x[]={r[0],r[0]     ,r[0]+r[2],r[0]+r[2]-size3d,r[0]-size3d     ,r[0]-size3d};
  double y[]={r[1],r[1]-r[3],r[1]-r[3],r[1]-r[3]-size3d,r[1]-r[3]-size3d,r[1]-size3d};

  /* here we need the scicos 3d color */
  fill_color=color;

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return FAIL;
  if ((Mx = nsp_matrix_create("x",'r',6,1))== NULLMAT) return FAIL;
  if ((My = nsp_matrix_create("y",'r',6,1))== NULLMAT) return FAIL;
  memcpy(Mx->R,x,npt*sizeof(double));
  memcpy(My->R,y,npt*sizeof(double));
  if ((pl = nsp_polyline_create("pl",Mx,My,TRUE,color,mark,mark_size,mark_color,fill_color,thickness,NULL))== NULL)
    return FAIL;
  /* insert the polyline */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) pl )== FAIL) return FAIL;
  if ((gobj =(NspObject *) nsp_grrect_create("pl",r[0],r[1],r[2],r[3],
					     -2,thickness,color,0.0,NULL))== NULL)
    return FAIL;
  /* insert the polyline */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) gobj )== FAIL) return FAIL;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig, axe->obj);
  nsp_axes_invalidate(((NspGraphic *) axe));
  return OK;
}

static int int_scicos_draw3D(Stack stack, int rhs, int opt, int lhs)
{
  int color=-1;
  double rect[4],e=0.3;
  NspMatrix *orig,*size;
  int_types T[] = {realmat,realmat,s_double,s_int, t_end} ;
  CheckRhs(4,4);
  if ( GetArgs(stack,rhs,opt,T,&orig,&size,&e,&color) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,orig,2);
  CheckLength(NspFname(stack),2,size,2);
  rect[0]=orig->R[0]+e;
  rect[1]=orig->R[1]+size->R[1];
  rect[2]=size->R[0]-e;
  rect[3]=size->R[1]-e;
  if ( scicos_draw_3d_new(rect,color,e) == FAIL) return RET_BUG;
  return 0;
}


typedef  enum { SLD_NORTH=0, SLD_SOUTH=1, SLD_EAST=2, SLD_WEST=3, SLD_ANY=4 }
  slock_dir;
typedef  enum { SL_IN=0  ,SL_OUT=1 ,SL_EVIN=2,SL_EVOUT=3 , SL_SQP=4, SL_SQM=5 }
  slock_type;

static int lock_draw_new(const double pt[2],double xf,double yf,slock_dir dir,slock_type typ,int locked,int dcolor)
{
  NspPolyline *pl;
  NspAxes *axe;
  int color=-1,mark=-2,mark_size=-1,mark_color=-1,fill_color=-1,thickness=-1;
  NspMatrix *Mx,*My;
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

  if (( axe=  nsp_check_for_current_axes(TRUE))== NULL) return FAIL;

  if ( dcolor != -2 )
    {
      /* color was given as option */
      fill_color= color = dcolor;
    }
  else
    {
      nsp_figure *F = ((NspGraphic *) axe)->obj->Fig;
      fill_color= color = F->gc->color ;
    }


  if ((Mx= nsp_matrix_create("x",'r',npt,1))== NULLMAT) return FAIL;
  if ((My= nsp_matrix_create("y",'r',npt,1))== NULLMAT) return FAIL;
  memcpy(Mx->R,x,npt*sizeof(double));
  memcpy(My->R,y,npt*sizeof(double));
  thickness= -1;
  if ( typ == SL_SQP )
    {
      /* draw only */
      fill_color=-2;
    }
  if ((pl = nsp_polyline_create("pl",Mx,My,TRUE,color,mark,mark_size,mark_color,fill_color,thickness,NULL))== NULL)
    return FAIL;
  /* insert the polyline */
  if ( nsp_list_end_insert( axe->obj->children,(NspObject *) pl )== FAIL)
    return FAIL;
  nsp_list_link_figure(axe->obj->children, ((NspGraphic *) axe)->obj->Fig, axe->obj);
  nsp_axes_invalidate(((NspGraphic *) axe));
  return OK;
}


static int int_lock_draw(Stack stack, int rhs, int opt, int lhs)
{

  nsp_option opts[] ={{ "color",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int dir=0,typ=0,color=-2;
  double xf,yf;
  NspMatrix *Mpt;
  int_types T[] = {realmat,s_double,s_double,s_int,s_int,new_opts, t_end} ;
  CheckStdRhs(5,5);
  if ( GetArgs(stack,rhs,opt,T,&Mpt,&xf,&yf,&dir,&typ,&opts,&color) == FAIL) return RET_BUG;
  CheckLength(NspFname(stack),1,Mpt,2);
  lock_draw_new(Mpt->R,xf/13,yf/5,dir,typ,1,color);
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
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_enter ();
#endif
  create_event_box (NULL,"Thread event Box");
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_leave ();
#endif
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
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_enter ();
#endif
  nsp_message_modeless_("Quit the thread message");
#if GTK_CHECK_VERSION(3,0,0)
#else
  gdk_threads_leave ();
#endif
  g_print ("make the main thread sleep \n");
  sleep(60);
  g_print ("main thread re-activated \n");
  return 0;
}
#endif


static int int_new_graphics(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  if ( nsp_move_boolean(stack,1,nsp_new_graphics())==FAIL)
    return RET_BUG;
  return 1;
}

extern void nsp_set_cursor(  BCG *Xgc,int id);

static int int_xcursor(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int id=-1;
  CheckStdRhs(0,1);
  if ( rhs == 1 )
    {
      if (GetScalarInt(stack,1,&id) == FAIL) return RET_BUG;
    }
  Xgc=nsp_check_graphic_context();
  nsp_set_cursor(Xgc,id);
  return 0;
}


extern void nsp_switch_graphics(void);

static int int_switch_graphics(Stack stack, int rhs, int opt, int lhs)
{
  CheckStdRhs(0,0);
  nsp_switch_graphics();
  return 0;
}

extern NspObject *nsp_get_graphic_widget(int wid);

static int int_nsp_graphic_widget(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *nsp_ret;
  int wid;
  CheckLhs(0,1);
  CheckStdRhs(1,1);
  if (GetScalarInt(stack,1,&wid) == FAIL) return RET_BUG;
  if ((nsp_ret = nsp_get_graphic_widget(wid)) == NULL)
    {
      Scierror("Error: cannot obtain the graphic widget for graphic window %d\n",
	       wid);
      return RET_BUG;
    }
  MoveObj(stack,1,nsp_ret);
  return 1;
}

/* push string to the command queue for graphic window win
 * or if string is a predefined command it is executed directly
 * if only one argument is given it is a command which is enqueued 
 */

static int int_nsp_enqueue_command(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc=NULL;
  char buf[256];
  int wid;
  char *command;
  CheckLhs(0,1);
  CheckStdRhs(1,2);
  if (rhs == 1) 
    {
      if ((command = GetString(stack,1)) == (char*)0) return RET_BUG;
      sprintf(buf,"%s",command);
      enqueue_nsp_command(buf);
    }
  else
    {
      if (GetScalarInt(stack,1,&wid) == FAIL) return RET_BUG;
      if ((command = GetString(stack,2)) == (char*)0) return RET_BUG;
      if (( Xgc=window_list_search_new(wid))== NULL) return 0;
      if ( nsp_call_predefined_callbacks(Xgc, command, wid) == 1)
	return 0;
      sprintf(buf,"%s",command);
      enqueue_nsp_command(buf);
    }
  return 0;
}

/* clear all the event queue
 *
 */

static int int_nsp_clear_queue(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  window_list_clear_queue(NULL);
  return 0;
}

/* be sure that gtk is started 
 *
 */

static int int_start_sci_gtk(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  start_sci_gtk();
  return 0;
}

/*************************************************************
 * The Interface for graphic functions
 *************************************************************/

extern int int_ode( Stack stack, int rhs, int opt, int lhs); /* XXX*/
extern int int_intg(Stack stack, int rhs, int opt, int lhs); /* XXX*/
extern int int_int2d(Stack stack, int rhs, int opt, int lhs); /* XXX*/
extern int int_int3d(Stack stack, int rhs, int opt, int lhs); /* XXX*/
extern int int_tcl_existvar(Stack stack, int rhs, int opt, int lhs); /* XXX*/

#ifdef WITH_TK
extern function int_tcl_DoOneEvent;
extern function   int_tcl_EvalFile;
extern function   int_tcl_EvalStr;
extern function   int_tcl_GetVar;
extern function   int_tcl_InfoVar;
extern function   int_tcl_SetVar;
extern function   int_tcl_OpenTk;
extern function   int_tcl_Close;
extern function   int_tcl_FindObj;
extern function   int_tcl_Set;
extern function   int_tcl_Get;
extern function   int_tcl_Gcf;
extern function   int_tcl_Scf;
extern function   int_tcl_GetVersion;
extern function   int_tcl_UnsetVar;
extern function   int_tcl_ExistVar;
extern function   int_tcl_UpVar;
extern function   int_tcl_DeleteInterp;
extern function   int_tcl_CreateSlave;
extern function   int_tcl_ExistInterp;
#endif

static OpTab GraphicsUtil_func[]={
#ifdef WITH_TK
  {"TCL_DoOneEvent",   int_tcl_DoOneEvent},
  {"TCL_EvalFile",   int_tcl_EvalFile},
  {"TCL_EvalStr",   int_tcl_EvalStr},
  {"TCL_GetVar",   int_tcl_GetVar},
  {"TCL_InfoVar",   int_tcl_InfoVar},
  {"TCL_SetVar",   int_tcl_SetVar},
  {"TCL_UnsetVar",   int_tcl_UnsetVar},
  {"TCL_ExistVar",   int_tcl_ExistVar},
  {"TCL_DeleteInterp",   int_tcl_DeleteInterp},
  {"TCL_CreateSlave", 	int_tcl_CreateSlave},
  {"TCL_ExistInterp", 	int_tcl_ExistInterp},
#endif
  {"ode",int_ode}, /* FIXME: en construction */
  {"intg",int_intg}, /* FIXME: en construction */
  {"int2d",int_int2d}, /* FIXME: en construction */
  {"int3d",int_int3d}, /* FIXME: en construction */
  {"feval",int_feval}, /* FIXME: en construction */
  {"bsearch", int_bsearch},
  {"help" ,int_gtkhelp},
  {"switch_graphics",int_switch_graphics},
  {"new_graphics",int_new_graphics},
#ifdef TEST_EVENT_BOX_THREAD
  {"gtk_test_loop", int_gtk_loop},
#endif
  {"ximage", int_ximage_new},
  {"gtk_start",int_start_sci_gtk},
  {(char *) 0, NULL}
};

int GraphicsUtil_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GraphicsUtil_func[i].fonc))(stack,rhs,opt,lhs);
}

/*
 * used to walk through the interface table
 * (for adding or removing functions)
 */

void GraphicsUtil_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GraphicsUtil_func[i].name;
  *f = GraphicsUtil_func[i].fonc;
}

#define NAMES(a) a, a "_new"

OpGrTab Graphics_func[]={
  {NAMES("Matplot"), int_matplot_new},
  {NAMES("Matplot1"),int_matplot1_new},
  /* {NAMES("c2dex"),int_c2dex}, */
  {NAMES("champ"),int_champ_new},
  {NAMES("champ1"),int_champ1_new},
  {NAMES("contour"),int_contour_new},
  {NAMES("contour2d"),int_contour2d_new},
  {NAMES("contour2di"),int_contour2di_new},
  {NAMES("drawaxis"),int_nxaxis},
  {NAMES("driver"),int_driver},
  {NAMES("fec"),int_fec_new},
  {NAMES("geom3d"),int_geom3d},
  {NAMES("grayplot"),int_grayplot_new},
  {NAMES("param3d"),int_param3d_new},
  {NAMES("param3d1"),int_param3d_new},
  {NAMES("plot2d"),int_plot2d_new},
  {NAMES("plot2d1"),int_plot2d1_1_new},
  {NAMES("plot2d2"),int_plot2d1_2_new},
  {NAMES("plot2d3"),int_plot2d1_3_new},
  {NAMES("plot2d4"),int_plot2d1_4_new},
  {NAMES("plot2d5"),int_plot2d1_5_new},
  {NAMES("plot2d6"),int_plot2d1_6_new},
  {NAMES("plot3d"),int_plot3d_new},
  {NAMES("plot3d1"),int_plot3d1_new},
  {NAMES("seteventhandler"),int_seteventhandler},
  {NAMES("show_pixbuf"),int_show_pixbuf},
  {NAMES("winsid"),int_winsid_new},
  {NAMES("window_exists"),int_window_exists},
  {NAMES("window_newid"),int_window_newid},
  {NAMES("window_debug_updates"), int_window_debug_updates},
  {NAMES("xarc"),int_xarc_new},
  {NAMES("xarcs"),int_xarcs_new},
  {NAMES("xarrows"),int_xarrows_new},
  {NAMES("xaxis"),int_xaxis},
  {NAMES("xchange"),int_xchange_new},
  {NAMES("xclea"),int_xclea_new},
  {NAMES("xclear"),int_xclear_new},
  {NAMES("xbasc"),int_xclear_new},
  {NAMES("xclick"),int_xclick},
  {NAMES("xcursor"), int_xcursor},
  {NAMES("xdel"),int_xdel_new},
  {NAMES("xdraw_pixbuf"),int_draw_pixbuf},
  {NAMES("xdraw_pixbuf_from_file"),int_draw_pixbuf_from_file},
  {NAMES("xend"),int_xend_new},
  {NAMES("xexport"),int_xexport_new},
  {NAMES("xfarc"),int_xfarc_new},
  {NAMES("xfarcs"),int_xfarcs_new},
  {NAMES("xflush"),int_xflush},
  {NAMES("xfpoly"),int_xfpoly_new},
  {NAMES("xfpolys"),int_xfpolys_new},
  {NAMES("xfrect"),int_xfrect_new},
  {NAMES("xget"),int_xget_new},
#ifdef KEEP_GTK2
  {NAMES("xget_image"),int_get_image},
#endif
  {NAMES("xget_pixbuf"),int_get_pixbuf},
  {NAMES("xgetech"),int_xgetech_new},
  {NAMES("xgetmouse"),int_xgetmouse},
  {NAMES("xgraduate"),int_xgraduate},
  {NAMES("xgrid"),int_xgrid_new},
  {NAMES("xinfo"),int_xinfo_new},
  {NAMES("xinit"),int_xinit},
  {NAMES("xlfont"),int_xlfont},
  {NAMES("xload"),int_xload_new},
  {NAMES("xname"),int_xname},
  {NAMES("xnumb"),int_xnumb},
  {NAMES("xpause"),int_xpause_new},
  {NAMES("xpoly"),int_xpoly_new},
  {NAMES("xpoly_clip"),int_xpoly_clip},
  {NAMES("xpolys"),int_xpolys_new},
  {NAMES("xrect"),int_xrect_new},
  {NAMES("xrects"),int_xrects_new},
  {NAMES("xs2fig"),int_xs2fig},
  {NAMES("xs2pdf"),int_xs2pdf},
  {NAMES("xs2png"),int_xs2png},
  {NAMES("xs2ps"),int_xs2ps},
  {NAMES("xs2eps"),int_xs2ps},
  {NAMES("xs2ps_old"),int_xs2ps_old},
  {NAMES("xs2svg"),int_xs2svg},
  {NAMES("xsave"),int_xsave_new},
  {NAMES("xsegs"),int_xsegs_new},
  {NAMES("xselect"),int_xselect},
  {NAMES("xset"),int_xset_new},
  {NAMES("xsetech"),int_xsetech_new},
  {NAMES("xstring"),int_xstring_new},
  {NAMES("xstringb"),int_xstringb},
  {NAMES("xstringc"),int_xstringc},
  {NAMES("xstringl"),int_xstringl_new},
  {NAMES("xstringbox"),int_xstringbox},
  {NAMES("xtape"),int_xtape},
  {NAMES("xtitle"),int_xtitle},
  {NAMES("scicos_draw3D"), int_scicos_draw3D},
  {NAMES("scicos_lock_draw"), int_lock_draw},
  {NAMES("nsp_graphic_widget"), int_nsp_graphic_widget},
  {NAMES("nsp_enqueue_command"), int_nsp_enqueue_command},
  {NAMES("nsp_clear_queue"), int_nsp_clear_queue},
  {(char *) 0, NULL}
};

int Graphics_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,Graphics_func[i].fonc,
					       &stack,rhs,opt,lhs);
#else
  return (*(Graphics_func[i].fonc))(stack,rhs,opt,lhs);
#endif
}

/*
 * used to walk through the interface table
 * (for adding or removing functions)
 */

void Graphics_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Graphics_func[i].name1;
  *f = Graphics_func[i].fonc;
}

/* Utilities  */

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
 * Returns: %NULL or a new #NspMatrix
 **/

static NspMatrix * check_style(Stack stack,const char *fname,char *varname,NspMatrix *var,int size)
{
  NspMatrix *loc_var = var;
  if ( var == NULLMAT)
    {
      /* provide a default value */
      int i;
      if (( loc_var = nsp_matrix_create(NVOID,'r',1,size))== NULLMAT) return NULL;
      for (i = 0 ; i < size ; i++) loc_var->R[i]=i+1;
    }
  else
    {
      /* check size */
      if ( var->mn < size )
	{
	  Scierror("%s: optional argument %s is too small (%d<%d)\n",fname,varname,var->mn,size);
	  return NULL;
	}
      if (( loc_var = nsp_matrix_create(NVOID,'r',1,var->mn))== NULLMAT) return NULL;
      memcpy(loc_var->R,var->R, var->mn*sizeof(double));
    }
  return loc_var;
}

static const int iflag_def[]={2,8,4};
static int iflag_loc[] = {2,8,4};

/**
 * check_iflag:
 * @stack:
 * @fname:
 * @varname:
 * @var:
 * @size:
 *
 * Check optional iflag argument
 * 3D options
 * default mode is 8 which is a superpose mode
 *
 *
 * Returns:
 **/

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
	  Scierror("%s: optional argument %s is too small (%d<%d)\n",fname,varname,var->mn,size);
	  return NULL;
	}
      for ( i= 0 ; i < size ; i++) iflag_loc[i]=(int) var->R[i];
    }
  return iflag_loc;
}


static const int param_iflag_def[]={8,4};
static int param_iflag_loc[] = {8,4};

/**
 * check_param_iflag:
 * @stack:
 * @fname:
 * @varname:
 * @var:
 * @size:
 *
 * Check optional iflag argument for param3d
 *
 * Returns:
 **/

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
	  Scierror("%s: optional argument %s is too small (%d<%d)\n",fname,varname,var->mn,size);
	  return NULL;
	}
      for ( i= 0 ; i < size ; i++) param_iflag_loc[i]=(int) var->R[i];
    }
  return param_iflag_loc;
}


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
	  Scierror("%s: optional argument %s should be of size 6\n",fname,varname);
	  return NULL;
	}
      else
	{
	  for ( i= 0 ; i < 6 ; i++) ebox_loc[i]= var->R[i];
	}
    }
  return ebox_loc;
}


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
	  Scierror("%s: optional argument %s should be of size 4\n",fname,varname);
	  return NULL;
	}
      else
	{
	  for ( i= 0 ; i < 4 ; i++) rect_loc[i]= var->R[i];
	}
    }
  return rect_loc;
}


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


/*
 * take care to keep the same order in enum legend_pos and in lpos_table;
 * typedef enum { legend_dl, legend_dr ,legend_drm, legend_ur,legend_ur,legend_urm } legends_pos;
 */

static int check_legend_pos(Stack stack,const char *fname,const char *varnam,const char *l_pos)
{
  const char *Table[] = {"dl",  "dr",  "drm", "ul",  "ur", "urm",  NULL};
  const nsp_const_string *entry;
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

/*
 * Check optional argument nax
 * note that var can be changed by this function
 */

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
	  Scierror("%s: optional argument %s should be of size 4\n",fname,varname);
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


static int check_zminmax (Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  if ( var != NULLMAT &&  var->mn != 2 )
    {
      Scierror("%s: optional argument %s should be of size 2\n",fname,varname);
      return FAIL;
    }
  return OK;
}

static int check_colminmax (Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  if ( var != NULLMAT && var->mn != 2 )
    {
      Scierror("%s: optional argument %s should be of size 2\n",fname,varname);
      return FAIL;
    }
  return OK;
}

static int check_colout (Stack stack,const char *fname,char *varname,NspMatrix *var)
{
  if ( var != NULLMAT &&  var->mn != 2 )
    {
      Scierror("%s: optional argument %s should be of size 2\n",fname,varname);
      return FAIL;
    }
  return OK;
}

/*
 * Check optional argument for logflags
 * note that var can be changed by this function
 * (Bruno)
 */

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
      CheckLength_(NspFname(stack),1,M1,6,FAIL);
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
      Scierror("%s: wrong number of standard rhs arguments (%d), rhs must be 1 or 6\n",
	       NspFname(stack),rhs);
      return FAIL;
    }
  return OK;
}


static void plot2d_strf_change(char c, char *strf)
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

/*
 * standard 2d optional arguments
 * FIXME: axesflags and frame should be used
 *        strf kept for compatibility ?
 */

static int int_check2d(Stack stack,NspMatrix *Mstyle,NspMatrix **Mstyle_new,int ns,
		       char **strf,char **leg, char **leg_pos,int *leg_pos_i,
		       NspMatrix *Mrect,double **rect,
		       NspMatrix *Mnax,int **nax,
		       int frameflag,int axesflag,char **logflags)
{
  char *leg1;
  if ( Mstyle_new != NULL)
    {
      if (( *Mstyle_new = check_style(stack,NspFname(stack),"style",Mstyle,ns))== NULL) return RET_BUG;
    }
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
  if ( frameflag != -1 ) (*strf)[1] = (char)(frameflag+ 48);
  if ( axesflag != -1 )  (*strf)[2] = (char)(axesflag + 48);
  return 0;
}
