/* Nsp
 * Copyright (C) 2005-2010 Jean-Philippe Chancelier Enpc/Cermics
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
#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/plist.h> 
#include <nsp/spmf.h>
#include <nsp/interf.h> 

#include "nsp/ode_solvers.h"
#include "nsp/eval.h"
#include "integ.h"


/*
 * ode interface
 * Authors:  Jean-Philippe Chancelier, Bruno Pincon. 
 *
 * we use a global variable to transmit information to 
 * ode_system. 
 * we could use the possibility to transmit this information 
 * thought y or neq but since lsoda is already non reentrant 
 * this is not really a priority ! 
 */

typedef struct _ode_data ode_data;
 
struct _ode_data
{
  NspObject *args; /* extra argument to the integrator (a simple Mat, a List,...) */
  int neq;         /* number of equations if neq <  y->mn y can be used to pass 
		    * extra informations to f
		    */
  NspMatrix *y,*t; /* state of ode y->mn >= neq , t : 1x1 matrix the time */
  NspObject *func; /* (pointer to nsp code of the ode function (if provided as nsp function) */
  NspObject *jac;  /* (pointer to nsp code of its jacobian (if provided as nsp function) */
  ode_f c_func;    /* pointer onto the C (or fortran) code (ode function to integrate)  */ 
  ode_jac c_jac;   /* pointer onto the C (or fortran) code (jacobian of the ode function) */ 
  int errcatch;
  int pausecatch;
};

static ode_data ode_d ={ NULL,0,NULL,NULL,NULL,NULL,NULL,NULL}; 

extern struct {
  int mesflg, lunit;
} C2F(eh0001);

/**
 * ode_clean:
 * @ode: 
 * 
 * clean after integration 
 **/

static void ode_clean(ode_data *obj)
{
  if ( obj->args != NULL) nsp_object_destroy(&obj->args);
  nsp_object_destroy(&obj->func);
  if ( obj->jac != NULL)   nsp_object_destroy(&obj->jac);
  nsp_matrix_destroy(obj->y); obj->y = NULLMAT;
  nsp_matrix_destroy(obj->t); obj->t = NULLMAT;
  obj->c_func = NULL;
  obj->c_jac = NULL;
  obj->neq = 0;
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

static int ode_system(int *neq,const double *t,const double y[],double ydot[], void *param)
{
  ode_data *ode = &ode_d;
  NspObject *targs[4];/* arguments to be transmited to ode->func */
  NspObject *nsp_ret;
  int nret = 1,nargs = 2, i;
  int errcatch =  ode->errcatch;
  int pausecatch = ode->pausecatch;
  targs[0]= NSP_OBJECT(ode->t); 
  ode->t->R[0] = *t;
  targs[1]= NSP_OBJECT(ode->y); 
  for ( i= 0 ; i < ode->y->mn ; i++) ode->y->R[i]= y[i];
  if (ode->args != NULL ) 
    {
      targs[2]= ode->args;
      nargs= 3;
    }
  /* FIXME : a changer pour metre une fonction eval standard */
  if ( nsp_gtk_eval_function_catch((NspPList *)ode->func ,targs,nargs,&nsp_ret,&nret,errcatch,pausecatch)== FAIL) 
    {
      ierode_1.iero = 1;
      return FAIL;
    }
  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' ) 
    {
      for ( i= 0 ; i < ode->y->mn ; i++) ydot[i]= ((NspMatrix *) nsp_ret)->R[i];
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else 
    {
      Scierror("Error: ode: function returned argument is wrong (at t=%g)\n",*t);
      ierode_1.iero = 1;
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
			  int *ml,int *mu,double jac[],int *nrowj, void *param)
{
  ode_data *ode = &ode_d;
  NspObject *targs[4];/* arguments to be transmited to ode->func */
  NspObject *nsp_ret;
  int nret = 1,nargs = 2, i, j, k, dim = ode->y->mn;
  int errcatch =  ode->errcatch;
  int pausecatch = ode->pausecatch;
  targs[0]= NSP_OBJECT(ode->t); 
  ode->t->R[0] = *t;
  targs[1]= NSP_OBJECT(ode->y); 
  for ( i= 0 ; i < dim ; i++) ode->y->R[i]= y[i];
  if (ode->args != NULL ) 
    {
      targs[2]= ode->args;
      nargs= 3;
    }

  /* FIXME : a changer pour metre une fonction eval standard */
  if ( nsp_gtk_eval_function_catch((NspPList *)ode->jac ,targs,nargs,&nsp_ret,&nret,errcatch,pausecatch)== FAIL) 
    {
      ierode_1.iero = 1;
      return FAIL;
    }

  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' ) 
    {
      if ( (*ml == -1 && *mu == -1) )  /* full case */
	{
	  if ( ((NspMatrix *) nsp_ret)->m == dim  && ((NspMatrix *) nsp_ret)->n == dim )
	    {
	      for ( i= 0 ; i < dim*dim ; i++) 
		jac[i]= ((NspMatrix *) nsp_ret)->R[i];
	    }
	  else
	    {
	      Scierror("Error: ode: something wrong with the jacobian dims (should be %d x %d got %d x %d)\n",
		       dim,dim, ((NspMatrix *) nsp_ret)->m,((NspMatrix *) nsp_ret)->n);
	      
	      ierode_1.iero = 1;
	      return FAIL;
	    }
	}
      else   /* this is the banded case: output matrix should be of size (ml+1+mu) x dim */ 
	{
	  if ( ((NspMatrix *) nsp_ret)->m == *ml+1+*mu  && ((NspMatrix *) nsp_ret)->n == dim )
	    {
	      /* take care of the fortran leading dimension of array jac (nrowj which is different from ml+1+mu) ....  */
	      for ( j = 0, k = 0 ; j < dim ; j++)
		for ( i = 0 ; i < *ml+1+*mu ; i++, k++ )
		  jac[i+j*(*nrowj)] = ((NspMatrix *) nsp_ret)->R[k];
	    }
	  else
	    {
	      Scierror("Error: ode: something wrong with the (banded) jacobian dims (should be %d x %d got %d x %d)\n",
		       *ml+1+*mu,dim, ((NspMatrix *) nsp_ret)->m,((NspMatrix *) nsp_ret)->n);
	      ierode_1.iero = 1;
	      return FAIL;
	    }
	}
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else 
    {
      Scierror("Error: ode: jacobian returned argument is wrong (at t=%g)\n",*t);
      ierode_1.iero = 1;
      return FAIL;
    }
  return OK;
}


/* FIXME: should be in a .h */
extern int SearchInDynLinks (char *op, int (**realop)());

int ode_prepare(int m, int n, NspObject *f, NspObject *jac, NspObject *args, ode_data *obj)
{
  if (( obj->func =nsp_object_copy(f)) == NULLOBJ) return FAIL;
  if (( nsp_object_set_name(obj->func,"ode_f")== FAIL)) return FAIL;
  if ( IsNspPList(f) )
    obj->c_func = (ode_f) ode_system;
  else
    {
      char *str = ((NspSMatrix *)f)->S[0];
      int (*func) (void);
      /* search string in the dynamically linked functions */
      if ( SearchInDynLinks(str, &func) == -1 )
	{
	  Scierror("Error: function %s is not dynamically linked in nsp\n",str);
	  return FAIL;
	}
      obj->c_func = (ode_f) func;
    }

  if ( jac != NULL ) 
    {
      if (( obj->jac =nsp_object_copy(jac)) == NULLOBJ) return FAIL;
      if (( nsp_object_set_name(obj->func,"ode_jac")== FAIL)) return FAIL;
      if ( IsNspPList(jac) )
	obj->c_jac = (ode_jac) (ode_jac) ode_jac_system;
      else
	{
	  char *str = ((NspSMatrix *)jac)->S[0];
	  int (*func) (void);
	  /* search string in the dynamically linked functions */
	  if ( SearchInDynLinks(str, &func) == -1 )
	    {
	      Scierror("Error: function %s is not dynamically linked in nsp\n",str);
	      return FAIL;
	    }
	  obj->c_jac = (ode_jac) func;
	}
    }
  else 
    {
      obj->jac = NULL;
    }

  if ( args != NULL ) 
    {
      if (( obj->args = nsp_object_copy(args)) == NULLOBJ ) return FAIL;   /* FIXME: is copy necessary ? */
      if (( nsp_object_set_name(obj->args,"arg")== FAIL)) return FAIL;
    }
  else 
    {
      obj->args = NULL;
    }
  if ((obj->y = nsp_matrix_create("y",'r',m,n))== NULLMAT) return FAIL;
  if ((obj->t = nsp_matrix_create("t",'r',1,1))== NULLMAT) return FAIL;
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

typedef enum {ode_default,adams,stiff,rk,rkd5,fix,discrete,roots} ode_method;

static int int_ode_lsode(Stack stack,NspObject *f, NspObject *jac,NspObject *args,NspMatrix *y0,
			 double t0,NspMatrix *time, double rtol, NspMatrix *Matol, int task,
			 Boolean warn, int lhs, NspHash *odeoptions, ode_method method) ;

static int int_ode_dopri5(Stack stack, NspObject *f, NspObject *args, NspMatrix *y0, 
			  double t0, NspMatrix *time, double rtol, NspMatrix *Matol, 
			  int task, Boolean warn, int lhs, NspHash *odeoptions); 

static int int_ode_discrete(Stack stack,NspObject *f,NspObject *args,NspMatrix *y0,
			    double t0,NspMatrix *time);

static Boolean vector_is_monotone(double *t, int n)
{
  /* verify that t is (strictly) monotone and not Nan or +-Inf */
  int i;
  double dir;

  if ( n >= 1 )
    {
      if ( ! finite(t[0]) ) return FALSE;
      if ( n == 1 ) return TRUE;
      if ( ! finite(t[1]) ) return FALSE;
      dir = t[1] - t[0];

      if ( dir > 0.0 )
	{
	  for ( i = 2 ; i < n ; i++ ) 
	    if ( ! (t[i-1] < t[i]) )   /* form of this test for detecting nan ... */ 
	      return FALSE;
	}
      else if ( dir < 0.0 )
	{
	  for ( i = 2 ; i < n ; i++ ) 
	    if ( ! (t[i-1] > t[i]) )   /* form of this test for detecting nan ... */ 
	      return FALSE;
	}
      else /* t[0]==t[1] */
	return FALSE;

      if ( ! finite(t[n-1]) ) return FALSE;
    }
  return TRUE;
}

int int_ode(Stack stack, int rhs, int opt, int lhs)
{
  ode_method methode= ode_default;
  NspObject *f= NULL, *jac=NULL,*g=NULL, *args=NULL;
  NspHash *odeoptions = NULL;
  NspList *gargs=NULL;
  double t0, rtol=0.0;
  Boolean warn = TRUE;
  int ng=-1, task=1;
  char *type=NULL;
  NspMatrix *y0,*time,*w=NULL,*iw=NULL, *Matol=NULL;
  
  static char *Table[] = {"default","adams","stiff","rk","rkd5","fix","discrete","roots", NULL};

  int_types T[] = {realmatcopy,s_double,realmat,obj,new_opts, t_end} ;

  nsp_option opts[] ={
    { "task",s_int,  NULLOBJ,-1},
    { "warn",s_bool,NULLOBJ,-1},
    { "args",obj,  NULLOBJ,-1},
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

  if ( GetArgs(stack,rhs,opt,T,&y0,&t0,&time,&f,&opts,&task,&warn,&args,&Matol,&g,
	       &gargs,&iw,&jac,&ng,&odeoptions,&rtol,&type,&w) == FAIL) return RET_BUG;

  /* inherits values from stack */
  ode_d.errcatch = stack.val->errcatch ;
  ode_d.pausecatch = stack.val->pause ;


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

  if ( IsNspPList(f) == FALSE  &&  IsString(f) == FALSE )
    {
      Scierror("%s: argument #3 should be a nsp function or a string\n",NspFname(stack));
      return RET_BUG;
    }

  if ( jac != NULLOBJ  &&  (IsNspPList(jac) == FALSE  &&  IsString(jac) == FALSE ) )
    {
      Scierror("%s: optional named argument jac should be a nsp function or a string\n",NspFname(stack));
      return RET_BUG;
    }

  if ( time->mn < 1 )
    {
      Scierror("%s: argument #3 (t) should not be empty\n",NspFname(stack));
      return RET_BUG;
    }
  else if ( ! vector_is_monotone(time->R, time->mn) )
    {
      Scierror("%s: argument #3 (t) is not strictly monotone (or +- Inf detected)\n",NspFname(stack));
      return RET_BUG;
    }
  else  /* verify compatibility between t0 and t */
    {
      if ( (time->R[time->mn-1] > t0 && time->R[0] < t0) || (time->R[time->mn-1] < t0 && time->R[0] > t0) ) 
	{
	  Scierror("%s: argument #3 (t) is not compatible with argument #2 (t0)\n",NspFname(stack));
	  return RET_BUG;
	}
      if ( warn && (task == 2 || task == 3) && time->mn > 1 )
	Sciprintf("%s: Warning for task=2 or 3, t should be a scalar (use the last component)\n",NspFname(stack));
    }

  switch ( methode ) 
    {
    case ode_default: 
    case adams: 
    case stiff:
      return int_ode_lsode(stack, f, jac, args, y0, t0, time, rtol, Matol, task, warn, lhs, odeoptions, methode);
    case discrete:
      return int_ode_discrete(stack,f,args,y0,t0,time);
    case rkd5:
      return int_ode_dopri5(stack, f, args, y0, t0, time, rtol, Matol, task, warn, lhs, odeoptions);
    case fix:
    case rk:
    case roots:
      Scierror("%s: method %s is to be implemented \n",methode, NspFname(stack));
      return RET_BUG;
    }
  return 0;
}


static int int_ode_lsode(Stack stack,NspObject *f, NspObject *jac,NspObject *args,
			 NspMatrix *y0, double t0,NspMatrix *time, double rtol, 
			 NspMatrix *Matol, int task, Boolean warn, int lhs, 
                         NspHash *odeoptions, ode_method method) 
{
  int op_jactyp=0,op_mxstep=0,op_mxordn=0,op_mxords=0,op_ixpr=0,op_ml=-1,op_mu=-1;
  double op_tcrit=2*DBL_MAX, op_h0=0.0, op_hmax=0.0, op_hmin=0.0;
  nsp_option opts[] ={
    { "h0",s_double , NULLOBJ,-1},
    { "hmax",s_double,  NULLOBJ,-1},
    { "hmin",s_double,  NULLOBJ,-1},
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
  
  int jt=0, mf=0, neq = y0->mn, rwork_size,iwork_size, ier;
  Boolean jac_is_banded=FALSE, tcrit_given=FALSE;
  /* atol */
  int itol=1;
  const double defrtol=1e-7;
  const double defatol=1.e-9;
  const double *atol=&defatol;
  int istate=1,iopt=0,i, outsize;
  double *rwork = NULL;
  int *iwork = NULL;
  NspMatrix *res = NULLMAT, *tt = NULLMAT;

  /* inherits values from stack */
  ode_d.errcatch = stack.val->errcatch ;
  ode_d.pausecatch = stack.val->pause ;

  if ( rtol <= 0.0 ) rtol = defrtol;

  if ( Matol != NULL) 
    {
      /* itol   = 1 or 2 according as atol (below) is a scalar or array. */
      if ( Matol->mn == 1 ) 
	{
	  itol = 1; atol = Matol->R;
	}
      else if ( Matol->mn == neq )
	{
	  itol = 2; atol = Matol->R;
	}
      else 
	{
	  Scierror("%s: size of atol should be equal to state size %d\n",NspFname(stack),y0->mn);
	  return RET_BUG;
	}
    }

  /* get options from hashtable */
  if ( odeoptions != NULL) 
    {
      if ( get_optional_args_from_hash(stack,odeoptions,opts,&op_h0,&op_hmax,&op_hmin,
				       &op_ixpr,&op_jactyp,&op_ml,&op_mu,&op_mxordn,
				       &op_mxords,&op_mxstep,&op_tcrit) == FAIL) 
	return RET_BUG;
      iopt = 1;
    }

  if ( op_ml != -1  && op_mu != -1 )  /* bandwith have been provided */
    {
      if ( op_ml < 0 || op_ml >= neq ||  op_mu < 0 || op_mu >= neq )
	{
	  Scierror("%s: lower and upper bandwith ml and mu badly specified\n",NspFname(stack));
	  return RET_BUG;
	}
      jac_is_banded = TRUE;
    }


  /*   for the parameters jt (lsoda) and mf (lsode) 
   *
   *   if method == ode_default then lsoda is used (lsoda is able to switch between adams and stiff (bdf) methods)
   *
   *      jt = 1 full jacobian provided by user (should be detected with jac != NULL  and op_ml < 0 and op_mu < 0)
   *      jt = 2 full jacobian computed by FD (should be detected with jac == NULL and op_ml < 0 and op_mu < 0)      
   *      jt = 4 banded jacobian provided by the user (should be detected with jac != NULL  and op_ml >= 0 and op_mu >= 0)
   *      jt = 5 banded jacobian computed by FD (should be detected with jac != NULL  and op_ml >= 0 and op_mu >= 0)
   *
   *   if method == adams
   *      mf = 10  no jacobian used (simple fonctionnal "fixed point" iteration to solve the non linearity)
   *      mf = 11  full jacobian provided by the user
   *      mf = 12  full jacobian computed by FD (FIXME: should be implemented or not)
   *      mf = 14  banded jacobian provided by the user 
   *      mf = 15  banded jacobian computed by FD
   *
   *   if method == stiff (bdf)
   *      mf = 20 + jt  with jt just as before
   *
   *   size of rwork array:
   *      lsoda max(lrn, lrs)   
   *          lrn = 20 + 16*neq,
   *          lrs = 22 + 9*neq + neq^2           if jt = 1 or 2,
   *          lrs = 22 + 10*neq + (2*ml+mu)*neq  if jt = 4 or 5.
   *
   *      lsode
   *             20 + 16*neq                    for mf = 10,
   *             20 + 16*neq + neq^2            for mf = 11 (or 12) 
   *             22 + 17*neq + (2*ml+mu)*neq    for mf = 14 (or 15)
   *             22 +  9*neq + neq^2            for mf = 21 or 22,
   *             22 + 10*neq + (2*ml + mu)*neq  for mf = 24 or 25.
   *       
   *   size of iwork array: lsoda: 20 + neq
   *                        lsode: 20 if mf = 10
   *                               20 + neq for other values of mf
   *      for mf = 14, 24 or 25 or jt = 4 or 5  iwork[0] = ml and iwork[1] = mu
   *          
   */ 

  if ( method == adams )
    {
      if ( jac != NULL )
	mf = jac_is_banded ? 14 : 11;
      else
	mf = jac_is_banded ? 15 : 10;
      
      switch (mf)
	{
	case 10:
	  rwork_size = 20 + 16*neq; iwork_size = 20; break;
	case 11:
	  rwork_size = 22 + 16*neq + neq*neq; iwork_size = 20 + neq; break;
	case 14:
	case 15:
	  rwork_size = 22 + 17*neq + (2*op_ml+op_mu)*neq; iwork_size = 20 + neq; break;
	}
    }
  else  /* method == ode_default (lsoda) or stiff (bdf) */
    {
      if ( jac != NULL )
	jt = jac_is_banded ? 4 : 1;
      else
	jt = jac_is_banded ? 5 : 2;
      mf = 20 + jt;
      
      if ( jt == 1 || jt == 2 )  /* full jacobian */
	rwork_size = Max(20 + 16*neq,22 + 9*neq + neq*neq);
      else                       /* banded jacobian */
	rwork_size = Max(20 + 16*neq,22 + 10*neq + (2*op_ml+op_mu)*neq);
      iwork_size = 20 + neq;
    }


  if ( (rwork = nsp_alloc_work_doubles(rwork_size)) == NULL ) return RET_BUG;
  if ( (iwork = nsp_alloc_work_int(iwork_size)) == NULL ) 
    {
      FREE(rwork); return RET_BUG;
    }

  if ( iopt == 1 )
    {
      rwork[0] = op_tcrit;
      if ( finite(op_tcrit) ) 
	tcrit_given = TRUE;
      rwork[4] = op_h0; rwork[5] = op_hmax; rwork[6] = op_hmin; 
      rwork[7] = 0.0; rwork[8] = 0.0; rwork[9] = 0.0;  /* these ones are not described in the lsode/lsoda doc but the doc says to set them to 0.0 */ 
      iwork[5] = op_mxstep;  iwork[6] = 0; /* mxhnil parameter which is not currently set using odeoptions */
      iwork[7] = 0; iwork[8] = 0; iwork[9] = 0;  
      if ( method == ode_default )  /* lsoda */
	{
	  iwork[4] = 0; iwork[7] = op_mxordn; iwork[8] = op_mxords;
	}
      else if ( method == stiff )
	iwork[4] = op_mxords;
      else /* method == adams */
	iwork[4] = op_mxordn;
    }

  if ( jac_is_banded )
    {
      iwork[0] = op_ml; iwork[1] = op_mu;
    }

  if ( ode_prepare(y0->m, y0->n, f, jac, args, &ode_d) == FAIL ) 
    goto err;

  /* memory for output variable(s) */
  if ( task == 1 )
    {
      if (( res = nsp_matrix_create(NVOID,'r',neq,time->mn))== NULLMAT) 
	goto err;
    }
  else if ( task == 2 || task == 3 )
    {
      outsize = 1000;
      if (( res = nsp_matrix_create(NVOID,'r',neq,outsize))== NULLMAT) 
	goto err;
      if (( tt = nsp_matrix_create(NVOID,'r',1,outsize))== NULLMAT) 
	goto err;
    }
  else
    {
      Scierror("%s: task optional parameter should be 1, 2 or 3 (got %d)\n",NspFname(stack), task);
      goto err;
    }

  ierode_1.iero = 0;
  C2F(eh0001).mesflg = warn ;

  /* loop on time */
  if ( task == 1 )     /* in this case the solver should output the solution at specified times */
    {
      int itask = tcrit_given ? 4 : 1; 
      for ( i= 0 ; i < time->mn ; i++ )
	{
	  double tout = time->R[i];
	  
	  if ( method == ode_default )
	    C2F(lsoda)(ode_d.c_func, &neq, y0->R, &t0, &tout, &itol, &rtol, atol,
		       &itask, &istate, &iopt, rwork, &rwork_size, iwork, &iwork_size, 
		       ode_d.c_jac, &jt, (void *) ode_d.args);
	  else
	    C2F(lsode)(ode_d.c_func, &neq, y0->R, &t0, &tout, &itol, &rtol, atol,
		       &itask, &istate, &iopt, rwork, &rwork_size, iwork, &iwork_size, 
		       ode_d.c_jac, &mf, (void *) ode_d.args);
	  
	  t0 = tout;
	  
	  if ( ierode_1.iero == 1 )  
	    {
	      /* the interpretor has failed to eval the rhs func or its jacobian
	       * and an error has already been "sent" 
	       * may be something better could be done like the following 
	       */
	      goto err;                    
	    }
	  
	  if ( istate < 0  )
	    {
	      if ( lhs < 2 ) 
		{
		  /* no ier variable at output => generate an error */
		  if ( warn ) /* error messages have been already displayed */
		    {
		      Scierror("Error: %s: integration fails (see previous messages)\n",NspFname(stack));
		      goto err;
		    }
		  else
		    {
		      Scierror("Error: %s: integration fails (ier = %d)\n",NspFname(stack), istate);
		      goto err;
		    }
		}
	      else    
		{
		  /* ier is present => return what have been computed */
		  nsp_matrix_resize (res, res->m, i-1);  
		  break;
		}
	    }
	  memcpy(res->R+neq*i, y0->R, neq*sizeof(double));
	}
    }
  else  
    {
      /* task == 2 or 3: the solution is output at each time step taken
       * by the solver with a slight difference for the last step 
       */
      double tout = time->R[time->mn-1];
      double dir = tout-t0 > 0 ? 1.0 : -1.0;
      int itask = tcrit_given ? 5 : 2; 

      /* copy first initial condition and initial time */
      memcpy(res->R, y0->R, neq*sizeof(double));
      tt->R[0] = t0;
      i = 1;
      while ( dir*(t0 - tout) < 0 )
	{
	  if ( method == ode_default )
	    C2F(lsoda)(ode_d.c_func, &neq, y0->R, &t0, &tout, &itol, &rtol, atol,
		       &itask, &istate, &iopt, rwork, &rwork_size, iwork, &iwork_size, 
		       ode_d.c_jac, &jt, (void *) ode_d.args);
	  else
	    C2F(lsode)(ode_d.c_func, &neq, y0->R, &t0, &tout, &itol, &rtol, atol,
		       &itask, &istate, &iopt, rwork, &rwork_size, iwork, &iwork_size, 
		       ode_d.c_jac, &mf, (void *) ode_d.args);
	  
	  if ( ierode_1.iero == 1 )
	    {
	      /* the interpretor has failed to eval the rhs func or its jacobian 
	       * and an error has already been "sent" 
	       * may be something better could be done like for istate < 0  
	       */
	      goto err;
	    }

	  if ( istate < 0  )
	    {
	      if ( lhs < 3 )
		{
		  /* no ier variable at output => generate an error */
		  if ( warn ) /* error messages have been already displayed */
		    {
		      Scierror("Error: %s: integration fails (see previous messages)\n",NspFname(stack));
		      goto err;
		    }
		  else
		    {
		      Scierror("Error: %s: integration fails (ier = %d)\n",NspFname(stack), istate);
		      goto err;
		    }
		}
	      else
		{
		  /* ier is present => return what have been computed */
		  break;
		}
	    }

	  if ( i >= outsize) 
	    {
	      /* needs to enlarge the arrays tt and res */
	      outsize *=2;
	      if ( nsp_matrix_resize(res, neq, outsize) == FAIL ) goto err;
	      if ( nsp_matrix_resize(tt, 1, outsize) == FAIL ) goto err;
	    }

	  memcpy(res->R+neq*i, y0->R, neq*sizeof(double));
	  tt->R[i] = t0;
	  i++;
	}

      nsp_matrix_resize(res, neq, i); 
      nsp_matrix_resize(tt, 1, i); 

      if ( task == 3  &&  istate >= 0 )
	{
	  /* if itask==3 and if integration is completed (istate >= 0) do an interpolation 
	   *  between tlast-h and tlast to obtain the final output exactly at tout 
	   */
	  itask = tcrit_given ? 4 : 1; 
	  if ( method == ode_default )
	    C2F(lsoda)(ode_d.c_func, &neq, y0->R, &t0, &tout, &itol, &rtol, atol,
		       &itask, &istate, &iopt, rwork, &rwork_size, iwork, &iwork_size, 
		       ode_d.c_jac, &jt, (void *) ode_d.args);
	  else
	    C2F(lsode)(ode_d.c_func, &neq, y0->R, &t0, &tout, &itol, &rtol, atol,
		       &itask, &istate, &iopt, rwork, &rwork_size, iwork, &iwork_size, 
		       ode_d.c_jac, &mf, (void *) ode_d.args);

	  if ( istate < 0 )  
	    {
	      /* this should not occur (?) only an interpolation is needed...  */
	      Scierror("Error: istate=%d in %s\n",istate,NspFname(stack));
	      goto err;
	    }
	  memcpy(res->R+neq*(res->n-1), y0->R, neq*sizeof(double));
	  tt->R[tt->n-1] = tout;
	}
    }

  FREE(rwork); FREE(iwork);
  ode_clean(&ode_d);

  /* FIXME: better control of lhs */
  if ( task == 1 )
    {
      MoveObj(stack,1,(NspObject *) res);
      if ( lhs >= 2 )
	{
	  ier = istate == 2 ? 0 : abs(istate);
	  if ( nsp_move_double(stack,2, (double)ier) == FAIL )	  
	    return RET_BUG;
	}
    }
  else 
    {
      /* for task =2|3, at least 2 output arguments should be returned... */
      MoveObj(stack,1,(NspObject *) res);
      MoveObj(stack,2,(NspObject *) tt);
      if ( lhs >= 3 )
	{
	  if ( nsp_move_double(stack,3, (double) istate) == FAIL )	  
	    return RET_BUG;
	}
    }
  return Max(1,lhs);

 err:
  FREE(rwork); FREE(iwork);
  ode_clean(&ode_d);
  nsp_matrix_destroy(res);
  nsp_matrix_destroy(tt);
  return RET_BUG;
}


static int int_ode_dopri5(Stack stack, NspObject *f, NspObject *args, NspMatrix *y0, 
			  double t0, NspMatrix *time, double rtol, NspMatrix *Matol, 
			  int task, Boolean warn, int lhs, NspHash *odeoptions) 
{
  int op_mxstep=20000, op_nstiff=500, neq = y0->mn, status, noutrel;
  double op_h0=0.0, op_hmax=0.0, tend = time->R[time->mn-1], safe=0.8, beta = 0.04, fac1=0.2, fac2=10.0;
  nsp_option opts[] ={
    { "h0",s_double , NULLOBJ,-1},
    { "hmax",s_double,  NULLOBJ,-1},
    { "mxstep", s_int, NULLOBJ,-1},
    { "nstiff", s_int, NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };
  
  /* atol */
  int itol=0;
  const double defrtol=1.e-4;
  const double defatol=1.e-6;
  const double *atol=&defatol;
  NspMatrix *res = NULLMAT, *tt = NULLMAT;

  /* inherits values from stack */
  ode_d.errcatch = stack.val->errcatch ;
  ode_d.pausecatch = stack.val->pause ;

  if ( rtol <= 0.0 ) rtol = defrtol;

  if ( Matol != NULL) 
    {
      /* itol   = 1 or 2 according as atol (below) is a scalar or array. */
      if ( Matol->mn == 1 ) 
	{
	  itol = 0; atol = Matol->R;
	}
      else if ( Matol->mn == neq )
	{
	  itol = 1; atol = Matol->R;
	}
      else 
	{
	  Scierror("%s: size of atol should be equal to state size %d\n",NspFname(stack), neq);
	  return RET_BUG;
	}
    }

  /* get options from hashtable */
  if ( odeoptions != NULL) 
    {
      if ( get_optional_args_from_hash(stack,odeoptions,opts,&op_h0,&op_hmax,&op_mxstep,&op_nstiff) == FAIL) 
	return RET_BUG;
    }
  if ( op_hmax == 0.0 )
    op_hmax = tend - t0;


  if ( ode_prepare(y0->m, y0->n, f, NULL, args, &ode_d) == FAIL ) 
    goto err;

  /* memory for output variable(s) */
  if ( task == 1 )
    {
      if (( res = nsp_matrix_create(NVOID,'r',neq,time->mn))== NULLMAT) 
	goto err;
    }
  else if ( task == 2 || task == 3 )
    {
      if (( res = nsp_matrix_create(NVOID,'r',neq,op_mxstep+1))== NULLMAT) 
	goto err;
      if (( tt = nsp_matrix_create(NVOID,'r',1,op_mxstep+1))== NULLMAT) 
	goto err;
    }
  else
    {
      Scierror("%s: task optional parameter should be 1, 2 or 3 (got %d)\n",NspFname(stack), task);
      goto err;
    }

  if ( task == 1 )
    status = nsp_dopri5(neq, ode_d.c_func, args, t0, y0->R, tend, op_hmax,
			&op_h0, rtol, atol, itol, op_mxstep, op_nstiff, safe, beta, fac1, fac2,  
			task, warn, res->R, time->R, time->mn, &noutrel);
  else
    status = nsp_dopri5(neq, ode_d.c_func, args, t0, y0->R, tend, op_hmax,
			&op_h0, rtol, atol, itol, op_mxstep, op_nstiff, safe, beta, fac1, fac2,  
			task, warn, res->R, tt->R, op_mxstep+1, &noutrel);

  if ( status != COMPLETED  && ((task == 1  &&  lhs < 2) || (task != 1  &&  lhs < 3 )) ) 
    {
      /* no ier variable at output => generate an error */
      if ( warn ) /* error messages have been already displayed */
	Scierror("Error: %s: integration fails (see previous messages)\n",NspFname(stack));
      else
	Scierror("Error: %s: integration fails (ier = %d)\n",NspFname(stack), status);
      goto err;
    }

  if ( task == 1  &&  status != COMPLETED )
    nsp_matrix_resize(res, neq, noutrel);
  else if ( task != 1 )
    {
      nsp_matrix_resize(res, neq, noutrel);
      nsp_matrix_resize(tt, 1, noutrel); 
    }
 
  ode_clean(&ode_d);

  /* FIXME: better control of lhs */
  if ( task == 1 )
    {
      MoveObj(stack,1,(NspObject *) res);
      if ( lhs >= 2 )
	{
	  if ( nsp_move_double(stack,2, (double) status) == FAIL )	  
	    return RET_BUG;
	}
    }
  else  
    {
      /* for task =2|3, at least 2 output arguments should be returned... */
      MoveObj(stack,1,(NspObject *) res);
      MoveObj(stack,2,(NspObject *) tt);
      if ( lhs >= 3 )
	{
	  if ( nsp_move_double(stack,3, (double) status) == FAIL )	  
	    return RET_BUG;
	}
    }
  return Max(1,lhs);

 err:
  ode_clean(&ode_d);
  nsp_matrix_destroy(res);
  nsp_matrix_destroy(tt);
  return RET_BUG;
}


static int int_ode_discrete(Stack stack,NspObject *f,NspObject *args,NspMatrix *y0,
			    double t0,NspMatrix *time)
{
  int job=OK, i , j , tk = (int) t0;
  NspMatrix *res = NULLMAT;

  if ( ode_prepare(y0->m,y0->n,f,NULL,args, &ode_d) == FAIL ) 
    goto err;

  /* output */
  if (( res = nsp_matrix_create(NVOID,'r',y0->mn,time->mn))== NULLMAT) 
    goto err;

  /* inherits values from stack */
  ode_d.errcatch = stack.val->errcatch ;
  ode_d.pausecatch = stack.val->pause ;

  ierode_1.iero = 0;

  for ( i= 0 ; i < time->mn ; i++ )
    {
      /* here we want to compute f(tout,y_tout) */
      int tkp1 = (int) time->R[i];
      if ( tk > tkp1 )
	{
	  Scierror("Error: given times are not increasing, %d followed by t(%d)=%d\n",tk,i+1,tkp1);
	  goto err;
	}

      for ( j = tk  ; j < tkp1 ; j++) 
	{
	  double t=(double) j;
	  /* input output can be the same in ode_system */
	  job = ode_d.c_func(&y0->mn, &t, y0->R, y0->R, (void *)ode_d.args);
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
      if (nsp_matrix_resize(res,res->m,i-1) != OK) goto err;
    }

  ode_clean(&ode_d);
  MoveObj(stack,1,(NspObject *) res);
  return 1;

 err:
  ode_clean(&ode_d);
  nsp_matrix_destroy(res);
  return RET_BUG;
}


/*
 * intg interface: [I,er_estim,info] = intg(a, b, f, atol=, rtol=, limit=, vect_flag=, args=)
 * Author: Bruno Pincon. 
 * Modelled from the ode interface
 * We use a global variable (intg_data) to transmit information 
 * to intg_func. intg_func is the C func passed to the 
 * integrator and which evaluates f(x [,List]) from a nsp function.
 */ 

typedef int (*intg_f)(const double *t, double *y, int *n);

extern int C2F(nspdqagse)(intg_f f, double *a, double *b, double *epsabs, double *epsrel, 
		          int *limit, double *result, double *abserr, int *neval, int *ier,
                          double *alist, double *blist, double *rlist, double *elist, 
		          int *iord, int *last, int *vectflag, int *stat);
extern int C2F(nspdqagie)(intg_f f, double *bound, int *inf, double *epsabs, double *epsrel, 
		          int *limit, double *result, double *abserr, int *neval, int *ier,
                          double *alist, double *blist, double *rlist, double *elist, 
		          int *iord, int *last, int *vectflag, int *stat);

typedef struct _intg_data intg_data;
 
struct _intg_data
{
  NspObject *args;   /* the extra argument to the integrator (a simple Mat, or a List, ... */
  NspMatrix *x;      /* current evaluation point or vector, x is a 1x1 or 21x1 or 15x1 or 30x1 */
  NspObject *func;   /* function to integrate */
  int errcatch;
  int pausecatch;
};

static intg_data intg_d ={NULLOBJ, NULLMAT, NULLOBJ}; 


static int intg_prepare(NspObject *f, NspObject *args, intg_data *obj, Boolean vect_flag, Boolean use_dqagse, int inf)
{
  if (( obj->func = nsp_object_copy(f)) == NULL) return FAIL;
  if (( nsp_object_set_name(obj->func,"intg_f")== FAIL)) return FAIL;
  if ( args != NULL ) 
    {
      if (( obj->args = nsp_object_copy(args)) == NULL ) return FAIL;
      if (( nsp_object_set_name(obj->args,"arg")== FAIL)) return FAIL;
    }
  else 
    {
      obj->args = NULL;
    }
  if ( vect_flag )
    {
      if ( use_dqagse )  /* integration on [a,b] */
	{
	  /* 	  if ((obj->x = nsp_matrix_create("x",'r',21,1))== NULL) return FAIL; */
	  if ((obj->x = nsp_matrix_create("x",'r',42,1))== NULL) return FAIL;
	}
      else               /* integration on (-oo,a], [a,+oo) or (-oo,+oo) */
	{
	  if ( inf == 2 )  /* integration on (-oo,+oo) 2*15=30 points used */
	    {
	      if ((obj->x = nsp_matrix_create("x",'r',30,1))== NULL) return FAIL;
	    }
	  else             /* integration on  (-oo,a], [a,+oo) 15 points used */
	    {
	      if ((obj->x = nsp_matrix_create("x",'r',15,1))== NULL) return FAIL;
	    }
	}
    }
  else
    {
      if ((obj->x = nsp_matrix_create("x",'r',1,1))== NULL) return FAIL;
    }
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
  if ( obj->args != NULL) nsp_object_destroy(&(obj->args));
  nsp_object_destroy( (NspObject **) &(obj->func));
  nsp_matrix_destroy(obj->x);
}

/**
 * intg_func:
 * @x: 
 * 
 * this function is passed to dqags as a function description 
 * 
 * Return value: 0 (OK) or -1 (FAIL)
 * 
 **/
static int intg_func(const double *x, double *y, int *n)
{
  intg_data *intg = &intg_d;
  NspObject *targs[2];/* arguments to be transmited to intg->func */
  NspObject *nsp_ret;
  int k, nret = 1,nargs = 1;
  int mn_save = intg->x->mn, m_save = intg->x->m;
  int errcatch =  intg->errcatch;
  int pausecatch = intg->pausecatch;
  intg->x->mn = *n;   intg->x->m = *n; 

  targs[0]= NSP_OBJECT(intg->x);
 
  for ( k = 0 ; k < *n ; k++ )
    intg->x->R[k] = x[k];

  if (intg->args != NULL ) 
    {
      targs[1]= intg->args;
      nargs= 2;
    }

  /* FIXME : a changer pour mettre une fonction eval standard */
  if ( nsp_gtk_eval_function_catch((NspPList *)intg->func ,targs,nargs,&nsp_ret,&nret,errcatch,pausecatch)== FAIL) 
    {
      Scierror("Error: intg: failure in function evaluation\n");
      return -1;
    }
  intg->x->mn = mn_save;   intg->x->m = m_save; 

  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' 
      &&  ((NspMatrix *) nsp_ret)->mn == *n) 
    {
      for ( k = 0 ; k < *n ; k++ )
	y[k] = ((NspMatrix *) (nsp_ret))->R[k];
      nsp_object_destroy( ((NspObject **) &nsp_ret));
      return 0;
    }
  else 
    {
      Scierror("Error:  intg: a problem occured in function evaluation:\n");
      if ( nret != 1 )
	Scierror("        function don't return one argument\n");
      else if ( !IsMat(nsp_ret) )
	Scierror("        function don't return the good type (must be a Mat)\n");
      else if ( ! (((NspMatrix *) nsp_ret)->rc_type == 'r') )
	Scierror("        function return a complex instead of a real\n");
      else if ( ((NspMatrix *) nsp_ret)->mn != *n )
	Scierror("        function don't return a vector of good length (wait for %d and got %d) \n",
		 *n,((NspMatrix *) nsp_ret)->mn);

      nsp_object_destroy((NspObject **) &nsp_ret);
      return -1;
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
  NspObject *f=NULLOBJ, *args=NULLOBJ;
  double a, b, rtol=1.e-8, atol=1.e-14, er_estim, bound=0.0;
  int limit = 750, lwork; /* (lwork = 4*limit) */
  double *rwork=NULL;
  int ier, *iwork=NULL, neval, last, inf, sign=1, stat;
  Boolean vect_flag=FALSE, use_dqagse;

  int_types T[] = {s_double, s_double, obj, new_opts, t_end} ;

  nsp_option opts[] ={
    { "args",obj,  NULLOBJ,-1},
    { "atol",s_double,NULLOBJ,-1},
    { "rtol",s_double,NULLOBJ,-1},
    { "limit",s_int,NULLOBJ,-1},
    { "vecteval",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };

  CheckLhs(1,4);

  if ( GetArgs(stack,rhs,opt, T, &a, &b, &f, &opts, &args, &atol, &rtol, &limit, &vect_flag) == FAIL ) 
    return RET_BUG;

  /* inherits values from stack */
  intg_d.errcatch = stack.val->errcatch ;
  intg_d.pausecatch = stack.val->pause ;

  if ( isnan(a) || isnan(b) )
    {
      Scierror("%s: a or b is a Nan\n",NspFname(stack));
      return RET_BUG;
    }

  if ( finite(a) && finite(b) )
    use_dqagse = TRUE;
  else
    {
      use_dqagse = FALSE;
      if ( finite(a) )       /* b is inf or -inf */
	{
	  bound = a;
	  if ( b > 0.0 ) { inf = 1; sign = 1;}
	  else { inf = -1; sign = -1;}
	}
      else if ( finite(b) )  /* a is -inf or inf */
	{
	  bound = b;
	  if ( a < 0.0 ) { inf = -1; sign = 1;}
	  else  { inf = 1; sign = -1;}
	}
      else                   /* a and b are +-inf */
	{
	  inf = 2;
	  if ( a < 0.0 && b > 0.0 ) sign = 1;
	  else  if ( a > 0.0 && b < 0.0 ) sign = -1;
	  else
	    {
	      Scierror("%s: a and b are both %e\n",NspFname(stack),a);
	      return RET_BUG;
	    }
	}
    }
 
  limit = Max(4, limit);

  if ( IsNspPList(f) == FALSE  )
    {
      Scierror("%s: third argument should be a function\n",NspFname(stack));
      return RET_BUG;
    }

  /* allocate working arrays */
  lwork = 4*limit;
  rwork = nsp_alloc_work_doubles(lwork);
  iwork = nsp_alloc_work_int(limit);
  if ( (rwork == NULL) || (iwork == NULL) )
    {
      FREE(rwork); FREE(iwork);
      return RET_BUG;
    }

  /* set up intg_d global var */
  if ( intg_prepare(f,args,&intg_d, vect_flag, use_dqagse, inf) == FAIL ) 
    goto err;

  /* allocate output var */
  if ( (res = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) goto err;

  /* call the integrator  */
  if ( use_dqagse ) 
    C2F(nspdqagse)(intg_func, &a, &b, &atol, &rtol, &limit, res->R, &er_estim, &neval, &ier,
		   rwork, &rwork[limit], &rwork[2*limit], &rwork[3*limit], iwork, &last, 
		   &vect_flag, &stat);
  else
    {
      C2F(nspdqagie)(intg_func, &bound, &inf, &atol, &rtol, &limit, res->R, &er_estim, &neval, &ier,
		     rwork, &rwork[limit], &rwork[2*limit], &rwork[3*limit], iwork, &last, 
		     &vect_flag, &stat);
      if ( sign == -1 ) res->R[0] = -res->R[0];
    }


  if ( stat != 0 ) 
    {
      /* a problem occurs when the interpretor has evaluated 
       * the function to integrate at a point 
       */
      goto err; 
    }
  if ( ier == 6 )
    {
      Scierror("Error:  intg: tolerance too stringent\n");
      goto err;
    }
  else if ( ier != 0 && lhs < 3 )
    {
      Sciprintf("Warning: intg: requested precision not reached (ier = %d)\n",ier);
    }

  intg_clean(&intg_d);
  FREE(rwork); FREE(iwork);
  MoveObj(stack,1,(NspObject *) res);
  if ( lhs > 1 )
    {
      if ( nsp_move_double(stack,2, er_estim) == FAIL )
	return RET_BUG;
      if ( lhs > 2 )
	{
	  if ( nsp_move_double(stack,3, (double) ier) == FAIL )
	    return RET_BUG;
	  if ( lhs > 3 )
	    if ( nsp_move_double(stack,4, (double) neval) == FAIL )
	    return RET_BUG;
	}
    }
  return Max(1,lhs);

 err:
  FREE(rwork); FREE(iwork);
  intg_clean(&intg_d);
  nsp_matrix_destroy(res);
  return RET_BUG;
}



/*
 * int2d interface: [I,er_estim,info] = int2d(x, y, f, tol=, iflag=, iclose=, limit=, vect_flag=, args=List)
 * Author: Bruno Pincon. 
 * Modelled from the intg interface
 * We use a global variable (int2d_data) to transmit information 
 * to int2d_func. int2d_func is the C func passed to the 
 * integrant and which evaluates f(x, y [,arg]) from a nsp function.
 */ 

typedef int (*int2d_f)(const double *x, const double *y, double *z, int *n);

extern int nsp_twodq(int2d_f f, int *n, double *x, double *y, double *tol, int *iclose, 
		      int *maxtri, int *mevals, double *result, double *error, 
		      int *nu, int *nd, int *nevals, int *iflag, double *data, 
		      int *iwork, int *vectflag, int *stat);

typedef struct _int2d_data int2d_data;
 
struct _int2d_data
{
  NspObject *args; /* extra argument of the integrant */
  NspMatrix *x;    /* current evaluation point abscissae, x is a 1x1 or 28x1 or 46x1 */
  NspMatrix *y;    /* current evaluation point ordinates, y is a 1x1 or 28x1 or 46x1 */
  NspObject *func; /* function to integrate */
  int errcatch;
  int pausecatch;
};

static int2d_data int2d_d ={NULLOBJ, NULLMAT, NULLMAT, NULLOBJ}; 


static int int2d_prepare(NspObject *f, NspObject *args, int2d_data *obj, Boolean vect_flag, Boolean iclose)
{
  if (( obj->func = nsp_object_copy(f)) == NULL) return FAIL;
  if (( nsp_object_set_name(obj->func,"int2d_f")== FAIL)) return FAIL;
  if ( args != NULL ) 
    {
      if (( obj->args = nsp_object_copy(args)) == NULL ) return FAIL;
      if (( nsp_object_set_name(obj->args,"arg")== FAIL)) return FAIL;
    }
  else 
    {
      obj->args = NULL;
    }

  if ( vect_flag )
    {
      if ( iclose )  
	{
	  /* lmq1 integration formulae 46/3 points  */
	  if ((obj->x = nsp_matrix_create("x",'r',46,1))== NULL) return FAIL;
	  if ((obj->y = nsp_matrix_create("y",'r',46,1))== NULL) return FAIL;
	}
      else           
	{
	  /* lmq0 integration formulae 28/3 points  */
	  if ((obj->x = nsp_matrix_create("x",'r',28,1))== NULL) return FAIL;
	  if ((obj->y = nsp_matrix_create("y",'r',28,1))== NULL) return FAIL;
	}
    }
  else              
    {
      /* scalar evaluation (so no distinction between lmq0 and lmq1) */
      if ((obj->x = nsp_matrix_create("x",'r',1,1))== NULL) return FAIL;
      if ((obj->y = nsp_matrix_create("y",'r',1,1))== NULL) return FAIL;
    }
  return OK;
}


/**
 * int2d_clean:
 * @obj:  int2d_data struct
 * 
 * clean after integration 
 **/
static void int2d_clean(int2d_data *obj)
{
  if ( obj->args != NULL) nsp_object_destroy(&(obj->args));
  nsp_object_destroy( (NspObject **) &(obj->func));
  nsp_matrix_destroy(obj->x);
  nsp_matrix_destroy(obj->y);
}

/**
 * int2d_func:
 * @x: 
 * @y: 
 * 
 * this function is passed to twodq as a function description 
 * 
 * Return value: 0 (OK) or -1 (FAIL)
 * 
 **/
static int int2d_func(const double *x, const double *y, double *z, int *n)
{
  int2d_data *int2d = &int2d_d;
  NspObject *targs[3];/* arguments to be transmited to intg->func */
  NspObject *nsp_ret;
  int k, nret = 1,nargs = 2;
  int errcatch =  int2d->errcatch;
  int pausecatch = int2d->pausecatch;
  targs[0]= NSP_OBJECT(int2d->x);
  for ( k = 0 ; k < *n ; k++ )
    int2d->x->R[k] = x[k];
  targs[1]= NSP_OBJECT(int2d->y);
  for ( k = 0 ; k < *n ; k++ )
    int2d->y->R[k] = y[k];

  if (int2d->args != NULL ) 
    {
      targs[2]= int2d->args;
      nargs = 3;
    }

  /* FIXME : a changer pour mettre une fonction eval standard */
  if ( nsp_gtk_eval_function_catch((NspPList *)int2d->func,targs,nargs,&nsp_ret,&nret,errcatch,pausecatch)== FAIL) 
    {
      Scierror("Error: int2d: failure in function evaluation\n");
      return -1;
    }

  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' 
      &&  ((NspMatrix *) nsp_ret)->mn == *n) 
    {
      for ( k = 0 ; k < *n ; k++ )
	z[k] = ((NspMatrix *) (nsp_ret))->R[k];
      nsp_object_destroy( ((NspObject **) &nsp_ret));
      return 0;
    }
  else 
    {
      Scierror("Error:  int2d: a problem occured in function evaluation:\n");
      if ( nret != 1 )
	Scierror("        function don't return one argument\n");
      else if ( !IsMat(nsp_ret) )
	Scierror("        function don't return the good type (must be a Mat)\n");
      else if ( ! (((NspMatrix *) nsp_ret)->rc_type == 'r') )
	Scierror("        function return a complex instead of a real\n");
      else if ( ((NspMatrix *) nsp_ret)->mn != *n )
	Scierror("        function don't return a vector of good length\n");

      nsp_object_destroy((NspObject **) &nsp_ret);
      return -1;
    }
}

static int make_triangles(double a, double b, double c, double d, NspMatrix **XX, NspMatrix **YY)
{
  NspMatrix *xx=NULLMAT, *yy=NULLMAT;
  int m, j, nbtris;
  double dx = b-a, dy = d-c, x0, x1, y0, y1;

  if ( ! (finite(a) && finite(b) && finite(c) && finite(d) && a < b && c < d  && finite(dx) && finite(dy)) )
    return FAIL;

  if ( dx > dy )
    m = (int) (dx/dy);
  else
    m = (int) (dy/dx);

  nbtris = 2*m;

  if ( (xx=nsp_matrix_create(NVOID,'r',3,nbtris)) == NULLMAT )
    return FAIL;
  if ( (yy=nsp_matrix_create(NVOID,'r',3,nbtris)) == NULLMAT )
    { nsp_matrix_destroy(xx); return FAIL;}

  if ( dx > dy )
    {
      dx = dx / (double) m;
      x0 = a;
      for ( j = 0 ; j < nbtris ; j+=2 )
	{
	  x1 = j < nbtris-1 ? x0+dx : b; 
	  xx->R[3*j] = x0; xx->R[3*j+1] = x1; xx->R[3*j+2] = x0;
	  yy->R[3*j] = c;  yy->R[3*j+1] = c;  yy->R[3*j+2] = d;
	  xx->R[3*j+3] = x1; xx->R[3*j+4] = x1; xx->R[3*j+5] = x0;
	  yy->R[3*j+3] = c;  yy->R[3*j+4] = d;  yy->R[3*j+5] = d;
	  x0 = x1;
	}
    }
  else
    {
      dy = dy / (double) m;
      y0 = c;
      for ( j = 0 ; j < nbtris ; j+=2 )
	{
	  y1 = j < nbtris-1 ? y0+dy : d; 
	  xx->R[3*j] = a;  xx->R[3*j+1] = b;  xx->R[3*j+2] = a;
	  yy->R[3*j] = y0; yy->R[3*j+1] = y0; yy->R[3*j+2] = y1;
	  xx->R[3*j+3] = b; xx->R[3*j+4] = b; xx->R[3*j+5] = a;
	  yy->R[3*j+3] = y0;  yy->R[3*j+4] = y1;  yy->R[3*j+5] = y1;
	  y0 = y1;
	}
    }
  *XX = xx; *YY = yy;
  return OK;
}
	  
/**
 * int_int2d:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for int2d
 * 
 * Return value: number of returned arguments.
 **/

int int_int2d(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *res=NULLMAT, *x=NULLMAT, *y=NULLMAT, *xx=NULLMAT, *yy=NULLMAT;
  NspObject *f=NULLOBJ, *args=NULLOBJ;
  double tol=1.e-10, er_estim;
  int limit = 100, nu=0, nd=0, meval, neval, stat;
  int lwork, liwork; /* sizes of work arrays (lwork = 9*limit, liwork = 2*limit) */
  double *rwork=NULL;
  int *iwork=NULL;
  Boolean iflag=FALSE, iclose=TRUE, vect_flag=FALSE, on_triangles=TRUE;

  int_types T[] = {realmat, realmat, obj, new_opts, t_end} ;

  nsp_option opts[] ={
    { "args",obj,  NULLOBJ,-1},
    { "tol",s_double,NULLOBJ,-1},
    { "tolflag",s_bool,NULLOBJ,-1},
    { "limit",s_int,NULLOBJ,-1},
    { "iclose",s_bool,NULLOBJ,-1},
    { "vecteval",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };

  CheckLhs(1,4);

  if ( GetArgs(stack,rhs,opt, T, &x, &y, &f, &opts, &args, &tol, &iflag, &limit, &iclose, &vect_flag) == FAIL ) 
    return RET_BUG;
  
  /* inherits values from stack */
  int2d_d.errcatch = stack.val->errcatch ;
  int2d_d.pausecatch = stack.val->pause ;

  CheckSameDims (NspFname(stack), 1, 2, x, y);

  if ( x->mn == 2 )
    {
      on_triangles = FALSE;
      if ( make_triangles(x->R[0],x->R[1], y->R[0], y->R[1], &xx, &yy) == FAIL )
	{
	  Scierror("%s: first and second arguments do not define a rectangle (or a memory problem occured)\n",
		   NspFname(stack));
	  return RET_BUG;
	}
      x = xx; y = yy;
    }
  else if ( x->m != 3 || x->n < 1 )
    {
      Scierror("%s: first and second arguments should be of length 2 or of size 3 x n (n>=1)\n",
	       NspFname(stack));
      return RET_BUG;
    }

  if ( IsNspPList(f) == FALSE  )
    {
      Scierror("%s: third argument should be a function\n",NspFname(stack));
      return RET_BUG;
    }

  /* allocate working arrays */
  limit = Max(x->n, limit);
  liwork = 2*limit;
  lwork = 9*limit;
  rwork = nsp_alloc_work_doubles(lwork);
  iwork = nsp_alloc_work_int(liwork);
  if ( (rwork == NULL) || (iwork == NULL) )
    {
      FREE(rwork); FREE(iwork);
      return RET_BUG;
    }

  /* set up int2d_d global var */
  if ( int2d_prepare(f, args, &int2d_d, vect_flag, iclose) == FAIL ) 
    goto err;

  /* set meval */
  meval = 100*limit;

  /* allocate output var */
  if ( (res = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) goto err;

  /* call the integrator  */
  nsp_twodq(int2d_func, &(x->n), x->R, y->R, &tol, &iclose, &limit, &meval, 
	     res->R, &er_estim, &nu, &nd, &neval, &iflag, rwork, iwork, &vect_flag, &stat);

  if ( stat != 0 ) 
    {
      /* a problem occurs when the interpretor has evaluated 
       * the function to integrate  
       */
      goto err;  
               
    }
  if ( iflag != 0 && lhs < 3 ) 
    {
      Sciprintf("Warning: int2d, requested precision not reached (ier = %d)\n", iflag);
    }

  int2d_clean(&int2d_d);
  FREE(rwork); FREE(iwork);
  if ( ! on_triangles )  /* destroy the triangulation of the rectangle */ 
    { nsp_matrix_destroy(xx);nsp_matrix_destroy(yy); }
  MoveObj(stack,1,(NspObject *) res);
  if ( lhs > 1 )
    {
      if ( nsp_move_double(stack,2, er_estim) == FAIL )
	goto err;
      if ( lhs > 2 )
	{
	  if ( nsp_move_double(stack,3, (double) iflag) == FAIL )
	    goto err;
	  if ( lhs > 3 )
	    if ( nsp_move_double(stack,4, (double) neval) == FAIL )
	      goto err;
	}
    }
  return Max(1,lhs);

 err:
  FREE(rwork); FREE(iwork);
  int2d_clean(&int2d_d);
  nsp_matrix_destroy(res);
  nsp_matrix_destroy(xx); nsp_matrix_destroy(yy);
  return RET_BUG;
}



/*
 * int3d interface: [I,er_estim,info] = int2d(x, y, z, f, rtol=, atol=, limit=, vect_flag=, args=List)
 * Author: Bruno Pincon. 
 * Modelled from the int2d interface
 * We use a global variable (int3d_data) to transmit information 
 * to int3d_func. int3d_func is the C func passed to the 
 * integrant and which evaluates f(x, y, z [,arg]) from a nsp function.
 */ 

typedef int (*int3d_f)(const double *x, const double *y, const double *z, double *values, int *n, void *params);

extern int nsp_int3d(double *X, double *Y, double *Z, int nt, 
		     int3d_f func, void *params, double *I, double *e, 
		     int ntmax, double atol, double rtol, int vecteval,
		     int *ntused, int *nbcalls);

typedef struct _int3d_data int3d_data;
 
struct _int3d_data
{
  NspObject *args; /* extra argument of the integrant */
  NspMatrix *x;    /* current evaluation point abscissae, x is a 1x1 or npx1 */
  NspMatrix *y;    /* current evaluation point ordinates, y is a 1x1 or npx1 */
  NspMatrix *z;    /* current evaluation point elevation, z is a 1x1 or npx1 */
  NspObject *func; /* function to integrate */
  int errcatch;
  int pausecatch;
};

static int3d_data int3d_d ={NULLOBJ, NULLMAT, NULLMAT, NULLMAT, NULLOBJ}; 


static int int3d_prepare(NspObject *f, NspObject *args, int3d_data *obj, Boolean vect_flag)
{
  if (( obj->func = nsp_object_copy(f)) == NULL) return FAIL;
  if (( nsp_object_set_name(obj->func,"int3d_f")== FAIL)) return FAIL;
  if ( args != NULL ) 
    {
      if (( obj->args = nsp_object_copy(args)) == NULL ) return FAIL;
      if (( nsp_object_set_name(obj->args,"arg")== FAIL)) return FAIL;
    }
  else 
    {
      obj->args = NULL;
    }

  if ( vect_flag )
    {
      if ((obj->x = nsp_matrix_create("x",'r',344,1))== NULL) return FAIL;
      if ((obj->y = nsp_matrix_create("y",'r',344,1))== NULL) return FAIL;
      if ((obj->z = nsp_matrix_create("z",'r',344,1))== NULL) return FAIL;
    }
  else              /* scalar evaluation */
    {
      if ((obj->x = nsp_matrix_create("x",'r',1,1))== NULL) return FAIL;
      if ((obj->y = nsp_matrix_create("y",'r',1,1))== NULL) return FAIL;
      if ((obj->z = nsp_matrix_create("z",'r',1,1))== NULL) return FAIL;
    }
  return OK;
}


/**
 * int3d_clean:
 * @obj:  int3d_data struct
 * 
 * clean after integration 
 **/

static void int3d_clean(int3d_data *obj)
{
  if ( obj->args != NULL) nsp_object_destroy(&(obj->args));
  nsp_object_destroy( (NspObject **) &(obj->func));
  nsp_matrix_destroy(obj->x);
  nsp_matrix_destroy(obj->y);
  nsp_matrix_destroy(obj->z);
  /* ici */
}

/**
 * int3d_func:
 * @x: 
 * @y: 
 * @z: 
 * 
 * this function is passed to nsp_int3d as a function description 
 * 
 * Return value: 1 (eval is successuf) or 0 (fail has failed)
 * 
 **/

static int int3d_func(const double *x, const double *y, const double *z, double *val, int *n, void *param)
{
  int3d_data *int3d = &int3d_d;
  NspObject *targs[4];/* arguments to be transmited to intg->func */
  NspObject *nsp_ret;
  int k, nret = 1,nargs = 3;
  int mn_saved = int3d->x->mn, m_saved = int3d->x->m;   /* to be explained... */
  int errcatch =  int3d->errcatch;
  int pausecatch = int3d->pausecatch;

  targs[0]= NSP_OBJECT(int3d->x);
  for ( k = 0 ; k < *n ; k++ )
    int3d->x->R[k] = x[k];
  int3d->x->mn = *n; int3d->x->m = *n;
  targs[1]= NSP_OBJECT(int3d->y);
  for ( k = 0 ; k < *n ; k++ )
    int3d->y->R[k] = y[k];
  int3d->y->mn = *n; int3d->y->m = *n;
  targs[2]= NSP_OBJECT(int3d->z);
  for ( k = 0 ; k < *n ; k++ )
    int3d->z->R[k] = z[k];
  int3d->z->mn = *n; int3d->z->m = *n;

  if (int3d->args != NULL ) 
    {
      targs[3]= int3d->args;
      nargs = 4;
    }

  /* FIXME : a changer pour mettre une fonction eval standard */
  if ( nsp_gtk_eval_function_catch((NspPList *)int3d->func,targs,nargs,&nsp_ret,&nret,errcatch,pausecatch)== FAIL) 
    {
      Scierror("Error: int3d: failure in function evaluation\n");
      return 0;
    }

  int3d->x->mn = mn_saved; int3d->x->m = m_saved;  /* to be explained... */
  int3d->y->mn = mn_saved; int3d->y->m = m_saved;
  int3d->z->mn = mn_saved; int3d->z->m = m_saved;

  if (nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' &&  ((NspMatrix *) nsp_ret)->mn == *n) 
    {
      for ( k = 0 ; k < *n ; k++ )
	val[k] = ((NspMatrix *) (nsp_ret))->R[k];
      nsp_object_destroy( ((NspObject **) &nsp_ret));
      return 1;
    }
  else 
    {
      Scierror("Error:  int3d: a problem occured in function evaluation:\n");
      if ( nret != 1 )
	Scierror("        function don't return one argument\n");
      else if ( !IsMat(nsp_ret) )
	Scierror("        function don't return the good type (must be a Mat)\n");
      else if ( ! (((NspMatrix *) nsp_ret)->rc_type == 'r') )
	Scierror("        function return a complex instead of a real\n");
      else if ( ((NspMatrix *) nsp_ret)->mn != *n )
	Scierror("        function don't return a vector of good length\n");

      nsp_object_destroy((NspObject **) &nsp_ret);
      return 0;
    }
}

void para2tetrahedra(double a, double b, double c, double d, double e, double f,
		     double *Xt, double *Yt, double *Zt);

static int make_tetrahedra(double a, double b, double c, double d, double e, double f, 
			   NspMatrix **XX, NspMatrix **YY, NspMatrix **ZZ)
{
  NspMatrix *xx=NULLMAT, *yy=NULLMAT, *zz=NULLMAT;
  double dx = b-a, dy = d-c, dz = f-e;

  if ( ! (finite(a) && finite(b) && finite(c) && finite(d) && finite(e) && finite(f)
          && a < b && c < d  && e < f && finite(dx) && finite(dy) && finite(dz)) )
    return FAIL;

  if ( (xx=nsp_matrix_create(NVOID,'r',4,5)) == NULLMAT )
    return FAIL;
  if ( (yy=nsp_matrix_create(NVOID,'r',4,5)) == NULLMAT )
    { nsp_matrix_destroy(xx); return FAIL;}
  if ( (zz=nsp_matrix_create(NVOID,'r',4,5)) == NULLMAT )
    { nsp_matrix_destroy(xx); nsp_matrix_destroy(yy); return FAIL;}

  para2tetrahedra(a, b, c, d, e, f, xx->R, yy->R, zz->R);
  *XX = xx; *YY = yy; *ZZ = zz;
  return OK;
}
	  
/**
 * int_int3d:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for int3d
 * 
 * Return value: number of returned arguments.
 **/

int int_int3d(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *res=NULLMAT, *x=NULLMAT, *y=NULLMAT, *z=NULLMAT, *xx=NULLMAT, *yy=NULLMAT, *zz=NULLMAT;
  NspObject *f=NULLOBJ, *args=NULLOBJ;
  double atol=1e-12, rtol=1e-7, Ia, ea;
  int limit = 10000, neval, ntused, stat, ier=0;
  Boolean vecteval=FALSE, on_tetrahedra=TRUE;

  int_types T[] = {realmat, realmat, realmat, obj, new_opts, t_end} ;

  nsp_option opts[] ={
    { "args",obj,  NULLOBJ,-1},
    { "atol",s_double,NULLOBJ,-1},
    { "rtol",s_double,NULLOBJ,-1},
    { "limit",s_int,NULLOBJ,-1},
    { "vecteval",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  };

  CheckLhs(1,4);

  if ( GetArgs(stack,rhs,opt, T, &x, &y, &z, &f, &opts, &args, &atol, &rtol, &limit, &vecteval) == FAIL ) 
    return RET_BUG;
  

  /* inherits values from stack */
  int3d_d.errcatch = stack.val->errcatch ;
  int3d_d.pausecatch = stack.val->pause ;


  CheckSameDims (NspFname(stack), 1, 2, x, y);
  CheckSameDims (NspFname(stack), 1, 3, x, z);
  if ( x->mn == 2 )
    {
      on_tetrahedra = FALSE;
      if ( make_tetrahedra(x->R[0],x->R[1], y->R[0], y->R[1], z->R[0], z->R[1], &xx, &yy, &zz) == FAIL )
	{
	  Scierror("%s: three first arguments don't define a parallelepiped (or a memory problem occured)\n",
		   NspFname(stack));
	  return RET_BUG;
	}
      x = xx; y = yy, z = zz;
    }
  else if ( x->m != 4 || x->n < 1 )
    {
      Scierror("%s:  three first arguments should be of length 2 or of size 4 x n (n>=1)\n",NspFname(stack));
      return RET_BUG;
    }

  if ( IsNspPList(f) == FALSE  )
    {
      Scierror("%s: fourth argument should be a function\n",NspFname(stack));
      return RET_BUG;
    }

  /* set up int3d_d global var */
  if ( int3d_prepare(f, args, &int3d_d, vecteval) == FAIL ) 
    goto err;

  /* allocate output var */
  if ( (res = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) goto err;

  /* call the integrator  */
  limit = Max ( limit, x->n );
  stat = nsp_int3d(x->R, y->R, z->R, x->n, int3d_func, NULL, &Ia, &ea, limit, 
		   atol, rtol, vecteval, &ntused, &neval);

  if ( stat <= 1 ) 
    {
      /* a problem occurs when the interpretor has evaluated the 
       * function to integrate (stat is 0 or 1) or a malloc fails 
       * (stat is 1) likely due to to large limit parameter. 
       * In case stat= 0 or 1 the interpretor should have 
       * already displayed an error message. 
       */
      if ( stat == -1 )
	Scierror("%s: running out of memory (limit parameter is likely to be too large)\n",NspFname(stack));
      goto err;
    } 
  else if ( stat == 2 )
    {
      ier = 1;
      if ( lhs < 3 )
	Sciprintf("\n Warning => int3d: requested precision not reached\n");
    }
  else  /* stat == 3 */
    ier = 0;

  int3d_clean(&int3d_d);
  if ( ! on_tetrahedra ) 
    { 
      /* destroy the tetrahedra associated to the parallelepiped */ 
      nsp_matrix_destroy(xx);nsp_matrix_destroy(yy);nsp_matrix_destroy(zz);
    }

  if ( nsp_move_double(stack,1, Ia) == FAIL )
    goto err;

  if ( lhs > 1 )
    {
      if ( nsp_move_double(stack,2, ea) == FAIL )
	goto err;
      if ( lhs > 2 )
	{
	  if ( nsp_move_double(stack,3, (double) ier) == FAIL )
	    goto err;
	  if ( lhs > 3 )
	    if ( nsp_move_double(stack,4, (double) neval) == FAIL )
	      goto err;
	}
    }
  return Max(1,lhs);

 err:

  int3d_clean(&int3d_d);
  nsp_matrix_destroy(xx); 
  nsp_matrix_destroy(yy);
  nsp_matrix_destroy(zz);
  return RET_BUG;
}

