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
  NspObject *jac;  /* jacobian */
};

static ode_data ode_d ={ NULL,0,NULL,NULL,NULL,NULL}; 

extern struct {
  int mesflg, lunit;
} C2F(eh0001);

extern struct {
  int iero;
} C2F(ierode);

int ode_prepare(int m,int n,NspObject *f,NspObject *jac,NspList *args,ode_data *obj)
{
  if (( obj->func =nsp_object_copy(f)) == NULL) return RET_BUG;
  if (( nsp_object_set_name(obj->func,"ode_f")== FAIL)) return RET_BUG;
  if ( jac != NULL ) 
    {
      if (( obj->jac =nsp_object_copy(jac)) == NULL) return RET_BUG;
      if (( nsp_object_set_name(obj->func,"ode_jac")== FAIL)) return RET_BUG;
    }
  else 
    {
      obj->jac = NULL;
    }
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
  if ( obj->jac != NULL)   nsp_object_destroy(&obj->jac);
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
 * this function is passed to lsoda as a ode description 
 * 
 * Return value: %FAIL or %OK 
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
 * ode_jac_system:
 * @neq: 
 * @t: 
 * @: 
 * @: 
 * 
 * this function is passed to lsoda as a jacobian associated 
 * to ode_system
 * 
 * Return value: 
 **/


static int ode_jac_system(int *neq,const double *t,const double y[],
			  int *ml,int *mu,double jac[],int *nrowj)
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
  if ( nsp_gtk_eval_function((NspPList *)ode->jac ,targs,nargs,&nsp_ret,&nret)== FAIL) 
    {
      C2F(ierode).iero = 1;
      return FAIL;
    }
  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' ) 
    {
      if ( *ml == 0 && *mu == 0 )
	{
	  for ( i= 0 ; i < ode->y->mn*ode->y->mn ; i++) 
	    jac[i]= ((NspMatrix *) nsp_ret)->R[i];
	}
      else 
	{
	  Scierror("Error: ode banded jacobian not already implemented \n");
	  C2F(ierode).iero = 1;
	  return FAIL;
	  
	}
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
 * general interface for variations on ode 
 * according to the value of argument methode control is 
 * given to a specific interface. 
 * 
 * Return value: number of returned arguments.
 **/

typedef enum {ode_default,adams,stiff,rk,rkf,fix,discrete,roots} ode_method;

static int int_ode_default(Stack stack,NspObject *f,int jt,NspObject *jac,NspList *args,NspMatrix *y0,
			 double t0,NspMatrix *time, double rtol, NspMatrix *Matol,
			 NspHash *odeoptions) ;

static int int_ode_discrete(Stack stack,NspObject *f,NspList *args,NspMatrix *y0,
			    double t0,NspMatrix *time);

int int_ode( Stack stack, int rhs, int opt, int lhs)
{
  ode_method methode= ode_default;
  int jt=2;
  NspObject *f= NULL, *jac=NULL,*g=NULL;
  NspHash *odeoptions = NULL;
  NspList *args=NULL, *gargs=NULL;
  double rtol=1.e-7,t0;
  int ng=-1;
  char *type=NULL;
  NspMatrix *y0,*time,*w=NULL,*iw=NULL, *Matol=NULL;
  
  static char *Table[] = {"default","adams","stiff","rk","rkf","fix","discrete","roots", NULL};

  int_types T[] = {realmatcopy,s_double,realmat,obj,new_opts, t_end} ;

  nsp_option opts[] ={
    { "args",list,  NULLOBJ,-1},
    { "atol",realmat,NULLOBJ,-1},
    { "g", obj, NULLOBJ,-1},
    { "gargs",list,  NULLOBJ,-1},
    { "iw",realmatcopy,NULLOBJ,-1},
    { "jac", obj, NULLOBJ,-1},
    { "ng", s_int, NULLOBJ,-1},
    { "odeoptions", hash , NULLOBJ,-1},
    { "rtol",s_double,NULLOBJ,-1},
    { "type",string,NULLOBJ,-1},
    { "w", realmatcopy,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };

  if ( GetArgs(stack,rhs,opt,T,&y0,&t0,&time,&f,&opts,&args,&Matol,&g,
	       &gargs,&iw,&jac,&ng,&odeoptions,&rtol,&type,&w) == FAIL) return RET_BUG;

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
      Scierror("%s: fourth argument should be a function\n",NspFname(stack));
      return RET_BUG;
    }


  switch ( methode ) 
    {
    case ode_default: 
      return int_ode_default(stack,f,jt,jac,args,y0,t0,time,rtol,Matol,odeoptions);
    case adams: 
      
    case stiff:
    case rk:
    case rkf:
    case fix:
    case discrete:
      return int_ode_discrete(stack,f,args,y0,t0,time);
    case roots:
      Scierror("%s: methode is to be implemented \n",NspFname(stack));
      return RET_BUG;
    }
  return 0;
}


static int int_ode_default(Stack stack,NspObject *f,int jt,NspObject *jac,NspList *args,
			   NspMatrix *y0, double t0,NspMatrix *time, double rtol, 
			   NspMatrix *Matol, NspHash *odeoptions) 
{
  int op_itask=0,op_jactyp=0,op_mxstep=0,op_mxordn=0,op_mxords=0,op_ixpr=0,op_ml=0,op_mu=0;
  double op_tcrit=0.0,op_h0=0.0,op_hmax=0.0,op_hmin=0.0;
  nsp_option opts[] ={
    { "h0",s_double , NULLOBJ,-1},
    { "hmax",s_double,  NULLOBJ,-1},
    { "hmin",s_double,  NULLOBJ,-1},
    { "itask",s_int,  NULLOBJ,-1},
    { "ixpr",s_int,NULLOBJ,-1},
    { "jactyp",s_int,NULLOBJ,-1},
    { "ml",s_int,NULLOBJ,-1},
    { "mu", s_int,NULLOBJ,-1},
    { "mxordn",s_int, NULLOBJ,-1},
    { "mxords",s_int , NULLOBJ,-1},
    { "mxstep", s_int, NULLOBJ,-1},
    { "tcrit",s_double,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };
  
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
	  Scierror("%s: size of atol should be equal to state size %d\n",NspFname(stack),y0->mn);
	  return RET_BUG;
	}
    }

  /* get options from options */
  
  if ( odeoptions != NULL) 
    {
      if ( get_optional_args_from_hash(stack,odeoptions,opts,&op_h0,&op_hmax,&op_hmin,
				       &op_itask,&op_ixpr,&op_jactyp,&op_ml,&op_mu,&op_mxordn,
				       &op_mxords,&op_mxstep,&op_tcrit) == FAIL) 
	return RET_BUG;
    }

  /* working arrays */

  if ( jt == 1 || jt == 2 ) 
    rwork_size = Max(20 + 16*y0->mn,22 + 9*y0->mn + y0->mn*y0->mn);
  else 
    rwork_size = Max(20 + 16*y0->mn,22 + 10*y0->mn + (2*ml+mu)*y0->mn);
  iwork_size = 20 + y0->mn;
  if (( rwork = nsp_matrix_create(NVOID,'r',1,rwork_size))== NULLMAT) return RET_BUG;
  if (( iwork = nsp_matrix_create(NVOID,'r',1,iwork_size))== NULLMAT) return RET_BUG;

  if ( ode_prepare(y0->m,y0->n,f,jac,args,&ode_d) == FAIL ) 
    return RET_BUG;

  /* output */

  if (( res = nsp_matrix_create(NVOID,'r',y0->mn,time->mn))== NULLMAT) return RET_BUG;

  C2F(ierode).iero = 0;
  C2F(eh0001).mesflg = 1 ;

  if ( jac != NULL ) jt = 1; /* user supplied full jacobian */

  /* loop on time */

  for ( i= 0 ; i < time->mn ; i++ )
    {
      double tout = time->R[i];
      C2F(lsoda)(ode_system,&y0->mn,y0->R,&t0,&tout,&itol,&rtol,atol,
		 &itask,&istate,&iopt,rwork->R,&rwork->mn,
		 (int *)iwork->R,&iwork->mn,ode_jac_system,&jt);
      t0 = tout;
      if ( istate < 0 ) 
	{
	  Scierror("Error: istate=%d in %s\n",istate,NspFname(stack));
	  return RET_BUG;
	}
      if ( C2F(ierode).iero == 1 ) 
	{
	  break;
	}
      /* FIXME : put a memcpy here */
      memcpy(res->R+res->m*i,y0->R,res->m*sizeof(double));
    }

  if ( C2F(ierode).iero == 1 ) 
    {
      /* resize matrix : just returning relevant values */
      if (nsp_matrix_resize (res,res->m,i-1) != OK) return RET_BUG;
    }

  ode_clean(&ode_d);
  nsp_matrix_destroy(rwork);
  nsp_matrix_destroy(iwork);
  MoveObj(stack,1,(NspObject *) res);
  return 1;
}


static int int_ode_discrete(Stack stack,NspObject *f,NspList *args,NspMatrix *y0,
			    double t0,NspMatrix *time)
{
  int job=OK, i , j , tk = (int) t0;
  NspMatrix *res;

  if ( ode_prepare(y0->m,y0->n,f,NULL,args,&ode_d) == FAIL ) 
    return RET_BUG;

  /* output */

  if (( res = nsp_matrix_create(NVOID,'r',y0->mn,time->mn))== NULLMAT) return RET_BUG;

  C2F(ierode).iero = 0;

  for ( i= 0 ; i < time->mn ; i++ )
    {
      /* here we want to compute f(tout,y_tout) */
      int tkp1 = (int) time->R[i];
      if ( tk > tkp1 )
	{
	  Scierror("Error: given times are not increasing, %d followed by t(%d)=%d\n",tk,i+1,tkp1);
	  nsp_matrix_destroy(res);
	  return RET_BUG;
	}
      for ( j = tk  ; j < tkp1 ; j++) 
	{
	  double t=(double) j;
	  /* input output can be the same in ode_system */
	  job= ode_system(&y0->mn,&t,y0->R,y0->R);
	  if ( job == FAIL)  break;
	}
      if ( job == FAIL) break;
      /* now y0 contains y(tkp1) we need to store it */
      memcpy(res->R+res->m*i,y0->R,res->m*sizeof(double));
      tk = tkp1;
    }

  if ( job == FAIL ) 
    {
      /* resize matrix : just returning relevant values */
      if (nsp_matrix_resize(res,res->m,i-1) != OK) return RET_BUG;
    }
  ode_clean(&ode_d);
  MoveObj(stack,1,(NspObject *) res);
  return 1;
}


/*
 * intg interface: [I,er_estim,info] = intg(a, b, f, atol=, rtol=, args=List)
 * Author: Bruno Pincon. 
 * Modelled from the ode interface
 * We use a global variable (intg_date) to transmit information 
 * to intg_system. intg_system is the C func passed to the 
 * integrator and which evaluates f(x [,List]) from a nsp function.
 */ 

typedef double (*intg_f)(const double *t);

extern int C2F(dqags)(intg_f f, double *a, double *b, double *epsabs, double *epsrel, 
		      double *alist, double *blist, double *elist, double *rlist, 
		      int *limit, int *iord, int *liord, double *result, double *abserr, int *ier);

typedef struct _intg_data intg_data;
 
struct _intg_data
{
  NspList *args;   /* a list to pass extra arguments to the integrator */
  NspMatrix *t;    /* current evaluation point, t is a 1x1 matrix */
  NspObject *func; /* function to integrate */
};

static intg_data intg_d ={NULLLIST, NULLMAT, NULLOBJ}; 


extern struct {   /* let the integrator know that the evaluation of the function */
  int iero;       /* by the interpretor has failed */ 
} C2F(ierajf);


static int intg_prepare(NspObject *f, NspList *args, intg_data *obj)
{
  if (( obj->func = nsp_object_copy(f)) == NULL) return FAIL; /* FIXME: why copy it ? */
  if (( nsp_object_set_name(obj->func,"intg_f")== FAIL)) return FAIL;
  if ( args != NULL ) 
    {
      if (( obj->args = nsp_list_copy(args)) == NULL ) return FAIL;
      if (( nsp_object_set_name((NspObject *) obj->args,"arg")== FAIL)) return FAIL;
    }
  else 
    {
      obj->args = NULL;
    }
  if ((obj->t = nsp_matrix_create("t",'r',1,1))== NULL) return FAIL;
  return OK;
}


/**
 * intg_clean:
 * @obj:  intg_data struct
 * 
 * clean after integration 
 **/
static void intg_clean(intg_data *obj)
{
  if ( obj->args != NULL) nsp_list_destroy(obj->args);
  nsp_object_destroy( (NspObject **) &(obj->func));
  nsp_matrix_destroy(obj->t);
}

/**
 * intg_system:
 * @t: 
 * 
 * this function is passed to dqag0 as a function description 
 * 
 * Return value: the value of f(t,args)
 * 
 **/
static double intg_system(const double *t)
{
  intg_data *intg = &intg_d;
  NspObject *targs[2];/* arguments to be transmited to intg->func */
  NspObject *nsp_ret;
  int nret = 1,nargs = 1;
  targs[0]= NSP_OBJECT(intg->t); 
  intg->t->R[0] = *t;
  double val;

  if (intg->args != NULL ) 
    {
      targs[1]= NSP_OBJECT(intg->args);
      nargs= 2;
    }

  /* FIXME : a changer pour mettre une fonction eval standard */
  if ( nsp_gtk_eval_function((NspPList *)intg->func ,targs,nargs,&nsp_ret,&nret)== FAIL) 
    {
      Scierror("Error: intg: failure in function evaluation\n");
      C2F(ierajf).iero = 1;  /* communicate the problem to the integrator */      
      return 0.0;
    }

  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' &&  ((NspMatrix *) nsp_ret)->mn == 1) 
    {
      val = ((NspMatrix *) (nsp_ret))->R[0];
      nsp_object_destroy( ((NspObject **) &nsp_ret));
      return val;
    }
  else 
    {
      Scierror("Error:  intg: a problem occured in function evaluation:\n");
      if ( nret != 1 )
	Scierror("        function return more than one argument\n");
      else if ( !IsMat(nsp_ret) )
	Scierror("        function don't return the good type (must be a Mat)\n");
      else if ( ! (((NspMatrix *) nsp_ret)->rc_type == 'r') )
	Scierror("        function return a complex instead of a real\n");
      else if ( ((NspMatrix *) nsp_ret)->mn != 1 )
	Scierror("        function don't return a real scalar\n");

      nsp_object_destroy((NspObject **) &nsp_ret);
      C2F(ierajf).iero = 1;  /* communicate the problem to the integrator */      
      return 0.0;
    }
}

/**
 * int_intg:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for intg
 * 
 * Return value: number of returned arguments.
 **/
int int_intg(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *res=NULLMAT;
  NspObject *f=NULLOBJ;
  NspList *args=NULLLIST;
  double a, b, rtol=1.e-8, atol=1.e-14, er_estim;
  int limit = 750, lwork, liwork; /* sizes of work arrays (lwork = 4*limit, liwork = limit/2 + 2) */
  double *rwork=NULL;
  int ier, *iwork=NULL;

  int_types T[] = {s_double, s_double, obj, new_opts, t_end} ;

  nsp_option opts[] ={
    { "args",list,  NULLOBJ,-1},
    { "atol",s_double,NULLOBJ,-1},
    { "rtol",s_double,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };

  if ( GetArgs(stack,rhs,opt, T, &a, &b, &f, &opts, &args, &atol, &rtol) == FAIL ) 
    return RET_BUG;

  if ( IsNspPList(f) == FALSE  )
    {
      Scierror("%s: third argument should be a function\n",NspFname(stack));
      return RET_BUG;
    }

  /* allocate working arrays */
  liwork = limit/2 + 2;
  lwork = 4*limit;
  rwork = nsp_alloc_work_doubles(lwork);
  iwork = nsp_alloc_work_int(liwork);
  if ( (rwork == NULL) || (iwork == NULL) )
    {
      FREE(rwork); FREE(iwork);
      return RET_BUG;
    }

  /* set up intg_d global var */
  if ( intg_prepare(f,args,&intg_d) == FAIL ) 
    goto err;

  /* allocate output var */
  if ( (res = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) goto err;

  /* call the integrator  */
  C2F(dqags)(intg_system, &a, &b, &atol, &rtol, rwork, &rwork[limit], &rwork[2*limit],
	     &rwork[3*limit], &limit, iwork, &liwork, res->R, &er_estim, &ier);

  if ( ier == 6 ) goto err;  /* a problem occurs when the interpretor has evaluated */
                             /* the function to integrate at a point */

  intg_clean(&intg_d);
  FREE(rwork); FREE(iwork);
  MoveObj(stack,1,(NspObject *) res);
  if ( lhs > 1 )
    {
      nsp_move_double(stack,2, er_estim);
      if ( lhs > 2 )
	nsp_move_double(stack,3, (double) ier);
    }
  return Max(1,lhs);

 err:
  FREE(rwork); FREE(iwork);
  intg_clean(&intg_d);
  nsp_matrix_destroy(res);
  return RET_BUG;
}



