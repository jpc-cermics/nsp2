/* Nsp
 * Copyright (C) 2005 Jean-Philippe Chancelier Enpc/Cermics
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
 */

#include <math.h>
#include <stdio.h>
#include <string.h> 
#include "nsp/interf.h"
#include "nsp/gtk/gobject.h" /* FIXME: nsp_gtk_eval_function */

/*-----------------------------------------------------------
 * ode en préparation 
 * we use a global variable to transmit information to 
 * ode_system. 
 * we could use the possibility to transmit this information 
 * thought y or neq but since lsoda is already non reentrant 
 * this is not really a priority ! 
 *-----------------------------------------------------------*/

typedef int (*ode_f)(int *neq,const double *t,const double y[],double ydot[]);
typedef int (*ode_jac)(int *neq,const double *t,const double y[],int *ml,
		       int *mu,double pd[],int *nrpd);

extern int F2C(lsoda)(ode_f f,int *neq, double *y, double *t, double *tout, int *itol, 
		      double *rtol, const double *atol, int *itask, int *istate, int *iopt, 
		      double *rwork, int *lrw, int *iwork, int *liw, ode_jac jac, int *jt);


typedef struct _ode_data ode_data;
 
struct _ode_data
{
  NspList *args; /* a list to pass extra arguments to the integrator */
  int neq;       /* number of equations if neq <  y->mn y can be used to pass 
		  * extra informations to f
		  */
  NspMatrix *y,*t; /* state of ode y->mn >= neq , t : 1x1 matrix the time */
  NspObject *func; /* equation to integrate */
};

static ode_data ode_d ={ NULL,0,NULL,NULL,NULL}; 

extern struct {
  int mesflg, lunit;
} C2F(eh0001);

extern struct {
  int iero;
} C2F(ierode);

int ode_prepare(int m,int n,NspObject *f,NspList *args,ode_data *obj)
{
  if (( obj->func =nsp_object_copy(f)) == NULL) return RET_BUG;
  if (( nsp_object_set_name(obj->func,"ode_f")== FAIL)) return RET_BUG;
  if ( args != NULL ) 
    {
      if (( obj->args = nsp_list_copy(args)) == NULL ) return RET_BUG;
      if (( nsp_object_set_name((NspObject *) obj->args,"arg")== FAIL)) return RET_BUG;
    }
  else 
    {
      obj->args = NULL;
    }
  if ((obj->y = nsp_matrix_create("y",'r',m,n))== NULL) return RET_BUG;
  if ((obj->t = nsp_matrix_create("t",'r',1,1))== NULL) return RET_BUG;
  return OK;
}


/**
 * ode_clean:
 * @ode: 
 * 
 * clean after integration 
 **/
static void ode_clean(ode_data *obj)
{
  if ( obj->args != NULL) nsp_list_destroy(obj->args);
  nsp_object_destroy(&obj->func);
  nsp_matrix_destroy(obj->y);
  nsp_matrix_destroy(obj->t);
}

/**
 * ode_system:
 * @neq: 
 * @t: 
 * @: 
 * @: 
 * @ode: 
 * 
 * this function is passed to lsoda 
 * 
 * Return value: 
 * 
 **/


static int ode_system(int *neq,const double *t,const double y[],double ydot[])
{
  ode_data *ode = &ode_d;
  NspObject *targs[4];/* arguments to be transmited to ode->func */
  NspObject *nsp_ret;
  int nret = 1,nargs = 2, i;
  targs[0]= NSP_OBJECT(ode->t); 
  ode->t->R[0] = *t;
  targs[1]= NSP_OBJECT(ode->y); 
  for ( i= 0 ; i < ode->y->mn ; i++) ode->y->R[i]= y[i];
  if (ode->args != NULL ) 
    {
      targs[2]= NSP_OBJECT(ode->args);
      nargs= 3;
    }
  /* FIXME : a changer pour metre une fonction eval standard */
  if ( nsp_gtk_eval_function((NspPList *)ode->func ,targs,nargs,&nsp_ret,&nret)== FAIL) 
    {
      C2F(ierode).iero = 1;
      return FAIL;
    }
  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' ) 
    {
      for ( i= 0 ; i < ode->y->mn ; i++) ydot[i]= ((NspMatrix *) nsp_ret)->R[i];
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else 
    {
      Scierror("Error: ode system returned argument is wrong t=%5.3f\n",*t);
      C2F(ierode).iero = 1;
      return FAIL;
    }
  return OK;
}

/**
 * int_ode:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for ode 
 * 
 * Return value: 
 **/

typedef enum {adams,stiff,rk,rkf,fix,discrete,roots} ode_method;

static int int_ode_adams(Stack stack,NspObject *f,int jt,NspObject *jac,NspList *args,NspMatrix *y0,
			 double t0,NspMatrix *time, double rtol, NspMatrix *Matol) ;

int int_ode( Stack stack, int rhs, int opt, int lhs)
{
  ode_method methode= adams;
  int jt=2;
  NspObject *f= NULL, *jac=NULL,*g=NULL;
  NspList *args=NULL, *gargs=NULL;
  double rtol=1.e-7,t0;
  int ng=-1;
  char *type=NULL;
  NspMatrix *y0,*time,*w=NULL,*iw=NULL, *Matol=NULL;
  
  static char *Table[] = {"adams","stiff", "rk", "rkf", "fix", "discrete", "roots", NULL};

  int_types T[] = {realmatcopy,s_double,realmat,obj,new_opts, t_end} ;

  nsp_option opts[] ={
    { "args",list,  NULLOBJ,-1},
    { "atol",realmat,NULLOBJ,-1},
    { "g", obj, NULLOBJ,-1},
    { "gargs",list,  NULLOBJ,-1},
    { "iw",realmatcopy,NULLOBJ,-1},
    { "jac", obj, NULLOBJ,-1},
    { "ng", s_int, NULLOBJ,-1},
    { "rtol",s_double,NULLOBJ,-1},
    { "type",string,NULLOBJ,-1},
    { "w", realmatcopy,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };

  if ( GetArgs(stack,rhs,opt,T,&y0,&t0,&time,&f,&opts,&args,&Matol,&g,&gargs,&iw,&jac,&ng,&rtol,&type,&w) 
       == FAIL) return RET_BUG;

  /* search for given integration method */

  if ( type != NULL) 
    {
      methode = is_string_in_array(type,Table,1);
      if ( methode < 0 ) 
	{
	  string_not_in_array(stack,type,Table,"optional argument type");
	  return RET_BUG;
	}
    }

  if ( IsNspPList(f) == FALSE  )
    {
      Scierror("%s: fourth argument should be a function\n",stack.fname);
      return RET_BUG;
    }

  switch ( methode ) 
    {
    case adams: 
      return int_ode_adams(stack,f,jt,jac,args,y0,t0,time,rtol,Matol);
    case stiff:
    case rk:
    case rkf:
    case fix:
    case discrete:
    case roots:
      Scierror("%s: methode is to be implemented \n",stack.fname);
      return RET_BUG;
    }
  return 0;
}


static int int_ode_adams(Stack stack,NspObject *f,int jt,NspObject *jac,NspList *args,NspMatrix *y0,
			 double t0,NspMatrix *time, double rtol, NspMatrix *Matol) 
{
  int rwork_size,iwork_size, ml=0,mu=0;
  /* atol */
  int itol=1;
  const double defatol=1.e-9;
  const double *atol=&defatol;
  int itask = 1,istate=1,iopt=0,i;
  NspMatrix *rwork,*iwork;
  NspMatrix *res;
  if ( Matol != NULL) 
    {
      /* itol   = 1 or 2 according as atol (below) is a scalar or array. */
      if ( Matol->mn == 1 ) 
	{
	  itol = 1; atol = Matol->R;
	}
      else if ( Matol->mn == y0->mn )
	{
	  itol = 2; atol = Matol->R;
	}
      else 
	{
	  Scierror("%s: size of atol should be equal to state size %d\n",stack.fname,y0->mn);
	  return RET_BUG;
	}
    }

  /* working arrays */

  if ( jt == 1 || jt == 2 ) 
    rwork_size = Max(20 + 16*y0->mn,22 + 9*y0->mn + y0->mn*y0->mn);
  else 
    rwork_size = Max(20 + 16*y0->mn,22 + 10*y0->mn + (2*ml+mu)*y0->mn);
  iwork_size = 20 + y0->mn;
  if (( rwork = nsp_matrix_create(NVOID,'r',1,rwork_size))== NULLMAT) return RET_BUG;
  if (( iwork = nsp_matrix_create(NVOID,'r',1,iwork_size))== NULLMAT) return RET_BUG;

  if ( ode_prepare(y0->m,y0->n,f,args,&ode_d) == FAIL ) 
    return RET_BUG;

  /* output */

  if (( res = nsp_matrix_create(NVOID,'r',y0->mn,time->mn))== NULLMAT) return RET_BUG;

  C2F(ierode).iero = 0;
  C2F(eh0001).mesflg = 1 ;

  for ( i= 0 ; i < time->mn ; i++ )
    {
      double tout = time->R[i];
      C2F(lsoda)(ode_system,&y0->mn,y0->R,&t0,&tout,&itol,&rtol,atol,
		 &itask,&istate,&iopt,rwork->R,&rwork->mn,
		 (int *)iwork->R,&iwork->mn,NULL,&jt);
      t0 = tout;
      if ( istate < 0 ) 
	{
	  Scierror("%s: istate %d\n",stack.fname,istate);
	  return RET_BUG;
	}
      if ( C2F(ierode).iero == 1 ) 
	{
	  break;
	}
      /* FIXME : put a memcpy here */
      memcpy(res->R+i,y0->R,res->m*sizeof(double));
    }

  if ( C2F(ierode).iero == 1 ) 
    {
      /* resize matrix */
      if (nsp_matrix_resize (res,res->m,i-1) != OK) return RET_BUG;
    }

  ode_clean(&ode_d);
  nsp_matrix_destroy(rwork);
  nsp_matrix_destroy(iwork);
  MoveObj(stack,1,(NspObject *) res);
  return 1;
}


