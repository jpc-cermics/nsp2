/* Nsp
 * Copyright (C) 2008-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * minpack interface.
 *--------------------------------------------------------------------------*/

#include <setjmp.h>
#include "nsp/interf.h"
#include "minpack.h"
#include "nsp/gtk/gobject.h" /* FIXME: nsp_gtk_eval_function */

/* 
 * data used when nsp is used to evaluate an  
 * objective function  
 */

typedef struct _hybr_data hybr_data;

typedef enum { HYBR_fcn, HYBR_jac} HYBR_ftype;

struct _hybr_data
{
  NspObject *args; /* extra arguments to the objective function */
  NspMatrix *x;    /* value of x */
  NspObject *fcn;
  NspObject *jac;
  minpack_fcn1 f_fcn;
  minpack_fcn6 f_jac;
  minpack_fcn2 f_lfcn;
  minpack_fcn7 f_ljac;
};

static void hybr_clean(hybr_data *obj);
static int hybr_prepare(int m,int n,NspObject *fcn,NspObject *jac,NspObject *args,hybr_data *obj);

static int hybr_fcn(const int *n, double *x, double *fvec, int *iflag,void *hybr_obj_d);
static int hybrj_fcn(const int *n,double *x,double *fvec,double *fjac,int *ldfjac,int *iflag,void *data);
static int lmder_fcn(const int *m,const int *n,double *x,double *fvec,double *fjac,int *ldfjac,int *iflag,void *data);

static int hybr_lfcn(const int *m,const int *n, double *x, double *fvec, int *iflag,void *hybr_obj_d);
static int hybr_ljac(const int *m,const int *n,double *x,double *fjac,int *ldfjac,int *iflag,void *data);

static NspObject *get_function(Stack stack, int pos,HYBR_ftype type,hybr_data *data);
static NspObject *get_function_obj(Stack stack, int pos,NspObject *obj,HYBR_ftype type,hybr_data *data);

/*  find a zero of a system of n nonlinear functions 
 *  in n variables. 
 */

static int int_minpack_fsolve (Stack stack, int rhs, int opt, int lhs)
{ 
  int maxfev = -1, warn = TRUE;
  NspMatrix *scale=NULLMAT;
  NspObject *fcn,*jac=NULLOBJ,*args=NULLOBJ;
  nsp_option opts[] ={{"args",obj,NULLOBJ,-1},
		      {"jac", obj,NULLOBJ,-1},
		      {"maxfev",s_int,NULLOBJ,-1},
		      {"xtol",s_double,NULLOBJ,-1},
		      {"ftol",s_double,NULLOBJ,-1},
		      {"scale",realmat,NULLOBJ,-1},
		      {"warn",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspMatrix *X, *work1=NULLMAT,*work2=NULLMAT;
  int nn,info=0,m=-1, nfev, njev=0;
  double xtol, ftol, epsm;
  hybr_data Hybr_data;
  
  CheckStdRhs(2,2);
  CheckLhs(1,5);
    
  epsm = minpack_dpmpar (1);
  xtol = sqrt (epsm); 
  ftol = pow(epsm,2./3.);
  
  if ((X = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  
  if ((fcn = get_function(stack,2,HYBR_fcn,&Hybr_data))==NULL) 
    return RET_BUG;
  
  if ( get_optional_args(stack,rhs,opt,opts,&args,&jac,&maxfev,&xtol,&ftol,&scale,&warn) == FAIL)
    return RET_BUG;

  if ( jac != NULL )
    {
      if ((jac = get_function_obj(stack,opts[1].position,jac,HYBR_jac,&Hybr_data))==NULL) 
	return RET_BUG;
    }

  if ( X->mn == 0 )  
    {
      Scierror("Error: x should not be empty\n");
      goto bug;
    }
  if ( xtol < 0.0 || ftol < 0.0 ) 
    {
      Scierror("Error: xtol and ftol should be non negative\n");
      goto bug;
    }

  if ( hybr_prepare(m,X->mn,fcn,jac,args,&Hybr_data)==FAIL) return RET_BUG;
  
  if ((work1 =nsp_matrix_create(NVOID,'r',X->m,X->n)) == NULLMAT) goto bug;
  if ( jac )
    {
      double *wa;
      int mode, j, lr = X->mn*(X->mn+1)/2, nprint;
      const double factor = 100.;
      nn=(X->mn*(X->mn+13))/2+X->mn*X->mn;
      if ((work2 =nsp_matrix_create(NVOID,'r',nn,1)) == NULLMAT) goto bug;
      /* wa must be of size at least 
       * (n*(n+13))/2. 
       */
      wa = work2->R+X->mn*X->mn; 
      /*     check the input parameters for errors. */
      /* hybrj parameters */
      if ( maxfev < 0 )  maxfev = (X->mn + 1) * 100;
      /* for mode = 1 wa is internally set 
       * if given 
       */ 
      if ( scale != NULLMAT ) 
	{
	  if ( scale->mn != X->mn ) 
	    {
	      Scierror("Error: optional argument scale should be of size %d\n",X->mn);
	      goto bug;
	    }
	  mode = 2; /* use given scaling */
	  for (j = 0; j < X->mn ; ++j) wa[j] = scale->R[j];
	}
      else 
	{
	  mode = 1; /* internal scaling */
	}
      nprint = 0;
      /* here jacobian and f are both given */
      minpack_hybrj (hybrj_fcn,&X->mn,X->R,work1->R,work2->R,&X->mn,&xtol,&ftol,
		     &maxfev, wa, &mode, &factor, &nprint, &info, &nfev,
		     &njev, &wa[X->mn * 6], &lr, &wa[X->mn], &wa[X->mn*2],
		     &wa[X->mn * 3], &wa[X->mn*4], &wa[X->mn * 5],&Hybr_data);

    }
  else
    {
      double *wa;
      const double factor = 100.;
      int nprint, mode, j, ml=X->mn-1,mu=X->mn-1, lr= X->mn*(X->mn+1)/2 ;
      double epsfcn = 0.0;

      nn=(X->mn*(3*X->mn+13))/2;
      if ((work2 =nsp_matrix_create(NVOID,'r',nn,1)) == NULLMAT) goto bug;
      if ( maxfev < 0 )  maxfev = (X->mn + 1) * 200;
      wa = work2->R; 
      if ( scale != NULLMAT ) 
	{
	  if ( scale->mn != X->mn ) 
	    {
	      Scierror("Error: optional argument scale should be of size %d\n",X->mn);
	      goto bug;
	    }
	  mode = 2; /* use given scaling */
	  for (j = 0; j < X->mn ; ++j) wa[j] = scale->R[j];
	}
      else 
	{
	  mode = 1; /* internal scaling */
	}
      
      nprint = 0;
      minpack_hybrd ( Hybr_data.f_fcn,&X->mn, X->R, work1->R, &xtol, &ftol, &maxfev, &ml, &mu,
		      &epsfcn, wa, &mode, &factor, &nprint, &info, &nfev,
		      &wa[X->mn * 6 + lr], &X->mn, &wa[X->mn * 6], &lr, &wa[X->mn],
		      &wa[X->mn*2], &wa[X->mn * 3], &wa[X->mn *4],
		      &wa[X->mn * 5],&Hybr_data);
    }

  if (lhs < 3 ) 
    {
      switch (info )
	{
	case 0 :
	  Scierror("Error: improper input parameters in %s\n",NspFname(stack));
	  goto bug;
	case 2 : 
	  if (warn) Sciprintf("Stop: number of calls to fcn has reached or exceeded maxfev=%d\n", maxfev);
	  break;
	case 3: 
	   if (warn)Sciprintf("Stop: xtol and ftol are too small. no further improvement in the approximate solution x is possible. \n");
	  break;
	case 4: 
	   if (warn)Sciprintf("Stop: iteration is not making good progress, as measured by the improvement from the last" 
		   " five jacobian evaluations\n");
	  break;
	case 5 :
	   if (warn)Sciprintf("Stop: iteration is not making good progress, as measured by the improvement from the last" 
		   " ten jacobian evaluations\n");
	  break;
	default : 
	  if ( info < 0) 
	    {
	      Scierror("Error: execution of %s aborted\n",NspFname(stack));
	      goto bug;
	    }
	}
    }
      
  hybr_clean(&Hybr_data);
  
  NSP_OBJECT(X)->ret_pos=1;
  if ( lhs >= 2) 
    {
      MoveObj(stack,2,NSP_OBJECT(work1));
    }
  else
    {
      if ( work1 != NULL) nsp_matrix_destroy(work1);
    }
  if ( lhs >= 3) 
    {
      if ( nsp_move_double(stack,3,info)== RET_BUG ) goto bug;
      if ( lhs >= 4 )
	{
	  if ( nsp_move_double(stack,4,nfev)== RET_BUG ) goto bug;
	  if ( lhs >= 5 )
	    if ( nsp_move_double(stack,5,njev)== RET_BUG ) goto bug;
	}
    }
  if ( work2 != NULL) nsp_matrix_destroy(work2);
  return Max(lhs,1);
 bug:
  hybr_clean(&Hybr_data);
  if ( work1 != NULL) nsp_matrix_destroy(work1);
  if ( work2 != NULL) nsp_matrix_destroy(work2);
  return RET_BUG;
}

/* returns the evaluation of the function and its jacobian 
 * can be useful when jacobian is not given.
 */

static int int_minpack_eval_jac (Stack stack, int rhs, int opt, int lhs)
{ 
  NspObject *fcn,*jac=NULLOBJ,*args=NULLOBJ;
  nsp_option opts[] ={{ "args",obj,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspMatrix *X,*J=NULLMAT, *work1=NULLMAT,*work2=NULLMAT;
  int info=0,m=-1;
  double tol;
  hybr_data Hybr_data;
  
  CheckStdRhs(2,3);
  CheckLhs(1,2);

  tol = sqrt (minpack_dpmpar (1));

  if ((X = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  
  if ((fcn = get_function(stack,2,HYBR_fcn,&Hybr_data))==NULL) 
    return RET_BUG;
  
  if ( get_optional_args(stack,rhs,opt,opts,&args,&tol) == FAIL)
    return RET_BUG;

  if ( X->mn == 0 )  
    {
      Scierror("Error: x should not be null\n");
      goto bug;
    }

  if ( hybr_prepare(m,X->mn,fcn,jac,args,&Hybr_data)==FAIL) return RET_BUG;
  
  if ((work1 = nsp_matrix_create(NVOID,'r',X->m,X->n)) == NULLMAT) goto bug;

  if ((J =  nsp_matrix_create(NVOID,'r',X->mn,X->mn)) == NULLMAT) goto bug;
  
  {
    double *wa, epsfcn = 0.0;
    int nprint, nfev, ml=X->mn-1,mu=X->mn-1, maxfev = (X->mn + 1) * 200;
    
    if ((work2 =nsp_matrix_create(NVOID,'r',2*X->mn,1)) == NULLMAT) goto bug;
    wa = work2->R; 
    nprint = 0;
      
    J->R[0]=1;
    J->R[1]=2;
    minpack_hybrd_eval ( Hybr_data.f_fcn,X->mn, X->R, work1->R, &maxfev, ml, mu,
			 epsfcn, &info, &nfev, J->R, &X->mn, &wa[X->mn],
			 &wa[X->mn*2],&Hybr_data);
  }
  
  hybr_clean(&Hybr_data);

  if ( info < 0 ) 
    {
      Scierror("Error: execution of %s aborted\n",NspFname(stack));
      goto bug;
    }
  else if ( info == 0) 
    {
      Scierror("Error: improper input in function %s\n",NspFname(stack));
      goto bug;
    }
  MoveObj(stack,1,NSP_OBJECT(work1));
  if ( lhs >= 2) 
    {
      MoveObj(stack,2,NSP_OBJECT(J));
    }
  else 
    {
      nsp_matrix_destroy(J);
    }
  if ( work2 != NULL) nsp_matrix_destroy(work2);
  return Max(lhs,1);
 bug:
  if ( work1 != NULL) nsp_matrix_destroy(work1);
  if ( work2 != NULL) nsp_matrix_destroy(work2);
  if ( J != NULL) nsp_matrix_destroy(J);
  return RET_BUG;
}


/* 
 *  solve in the least-square sense 
 *  Modified (5 dec 2008) by Bruno 
 *  (use lmder and lmdif in place of lmder1 and lmdif1) 
 */
static int int_minpack_lsq (Stack stack, int rhs, int opt, int lhs)
{ 
  int warn = TRUE;
  NspObject *fcn,*jac=NULLOBJ,*args=NULLOBJ;
  NspMatrix *X, *scale=NULLMAT, *fvec=NULLMAT;
  nsp_option opts[] ={{ "args",obj,NULLOBJ,-1},
		      { "jac", obj,NULLOBJ,-1},
		      {"maxfev",s_int,NULLOBJ,-1},
		      { "xtol",s_double,NULLOBJ,-1},
		      { "ftol",s_double,NULLOBJ,-1},
		      { "gtol",s_double,NULLOBJ,-1},
		      {"scale",realmat,NULLOBJ,-1},
		      {"warn",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int j, *ipvt=NULL,n, nwork, info=0, m=-1, maxfev=-1, nfev, njev=0, nprint=0, mode=1;
  double *work=NULL, xtol, ftol, gtol, factor=100, epsfcn=0.0;
  double *fjac, *diag, *qtf, *wa1, *wa2, *wa3, *wa4;
  hybr_data Hybr_data;
  
  CheckStdRhs(3,3);
  CheckLhs(1,5);
  
  xtol = ftol = sqrt(minpack_dpmpar(1));
  gtol = 0.0;

  if ( (X = GetRealMatCopy(stack,1)) == NULLMAT ) return RET_BUG;
  n = X->mn;
  if ( n <= 0  ) 
    {
      Scierror("Error: x0 is empty (the number of unknows must be positive)\n");
      return RET_BUG;
    }

  if ((fcn = get_function(stack,2,HYBR_fcn,&Hybr_data))==NULL) 
    return RET_BUG;
  
  if (GetScalarInt(stack,3,&m) == FAIL) return RET_BUG;
  if ( m < n  ) 
    {
      Scierror("Error: the number of equations (=%d) is inferior to the number of unknows (%d)", m, n);
      return RET_BUG; 
    }
  
  if ( get_optional_args(stack,rhs,opt,opts,&args,&jac,&maxfev,&xtol,&ftol,&gtol,&scale,&warn) == FAIL) return RET_BUG;

  if ( jac != NULL )
    {
      if ((jac = get_function_obj(stack,opts[1].position,jac,HYBR_jac,&Hybr_data))==NULL) 
	return RET_BUG;
    }

  if ( hybr_prepare(m, n, fcn, jac, args, &Hybr_data) == FAIL ) return RET_BUG;
  
  if ((fvec =nsp_matrix_create(NVOID,'r', m, 1)) == NULLMAT) goto bug;


  /* needed double work arrays (either for lmder or for lmdif): 
   *    fjac(m * n) + diag(n) + qtf(n) + wa1(n), wa2(n), wa3(n), wa4(m) 
   */
  nwork = m*n + 5*n + m;
  if ( (work=nsp_alloc_work_doubles(nwork)) == NULL ) goto bug;
  fjac = work; diag = fjac+m*n; qtf = diag+n; wa1 = qtf+n; wa2 = wa1+n; wa3 = wa2+n; wa4 = wa3+n;  

  /* int work array ipvt (n) */
  if ( (ipvt=nsp_alloc_work_int(n)) == NULL ) goto bug;

  if ( scale != NULLMAT ) 
    {
      if ( scale->mn != n ) 
	{
	  Scierror("Error: optional argument scale should be of size %d\n",n);
	  goto bug;
	}
      mode = 2; /* use given scaling */
      for (j = 0; j < X->mn ; ++j) 
	{
	  diag[j] = scale->R[j];
	  if ( diag[j] <= 0.0 )
	    {
	      Scierror("Error: components of the optional argument scale should be positive\n");
	      goto bug;
	    }
	}
    }

  if ( jac ) 
    {
      if ( maxfev <= 0 ) maxfev = 100*(n+1);
      minpack_lmder(lmder_fcn, &m, &n, X->R, fvec->R, fjac, &m, &ftol, &xtol, &gtol,
		    &maxfev, diag, &mode, &factor, &nprint, &info, &nfev, &njev, ipvt,
		    qtf, wa1, wa2, wa3, wa4, &Hybr_data);

    }
  else 
    {
      if ( maxfev <= 0 ) maxfev = 200*(n+1);
      minpack_lmdif(Hybr_data.f_lfcn, &m, &n, X->R, fvec->R, &ftol, &xtol, &gtol,
		    &maxfev, &epsfcn, diag, &mode, &factor, &nprint, &info, &nfev, 
		    fjac, &m, ipvt, qtf, wa1, wa2, wa3, wa4, &Hybr_data);
    }

  if (lhs < 3 ) 
    {
      switch (info )
	{
	case 0 :   /* this case should not arise: all tests in lmder or lmdif leading to info=0 are
                      (normally) trapped in this interface before the call to lmder or lmdif */
	  Scierror("Error: improper input parameters in %s\n",NspFname(stack));
	  goto bug;
	case 1 :
	  if (warn)Sciprintf("%s: successful exit:\n",NspFname(stack));
	  if (warn)Sciprintf("            the relative (estimated) error in the sum of squares is at most ftol\n");
			     
	  break;
	case 2 : 
	  if (warn)Sciprintf("%s: successful exit:\n",NspFname(stack));
          if (warn)Sciprintf("            the relative (estimated) error between x and the solution is at most xtol\n");
			     
	  break;
	case 3: 
	  if (warn)Sciprintf("%s: successful exit:\n",NspFname(stack));
          if (warn)Sciprintf("            the relative (estimated) error in the sum of squares is at most ftol\n");
          if (warn)Sciprintf("            and the relative (estimated) error between x and the solution is at most xtol\n");

	  break;
	case 4:
	  if (warn)Sciprintf("%s: successful exit:\n",NspFname(stack));
          if (warn)Sciprintf("            fvec is orthogonal to the columns of the jacobian up to gtol\n");
	  break;
	case 5 :
	  if (warn)Sciprintf("%s: unsuccessful exit: number of calls to fcn has reached or exceeded maxfev\n", NspFname(stack));
	  break;
	case 6:
	  if (warn)Sciprintf("%s: successful exit: ftol is too small but\n",NspFname(stack));
	  if (warn)Sciprintf("            the relative (estimated) error in the sum of squares is at most epsm\n");
          if (warn)Sciprintf("            (No further reduction in the sum of squares is possible.)\n");
	  break;
	case 7:
	  if (warn)Sciprintf("%s: successful exit: xtol is too small but\n",NspFname(stack));
          if (warn)Sciprintf("            the relative (estimated) error between x and the solution is at most xtol\n");
          if (warn)Sciprintf("            (No further improvement in the approximate solution x is possible).\n");
	  break;
	case 8:
	  if (warn)Sciprintf("%s: successful exit: gtol too small but\n",NspFname(stack));
          if (warn)Sciprintf("            fvec is orthogonal to the columns of the jacobian up to machine precision\n");
	  break;
	default : 
	  if ( info < 0) 
	    {
	      Scierror("Error: execution of %s aborted\n",NspFname(stack));
	      goto bug;
	    }
	}
    }

  hybr_clean(&Hybr_data);
  FREE(work); FREE(ipvt);

  NSP_OBJECT(X)->ret_pos=1;

  if ( lhs < 2 )
    nsp_matrix_destroy(fvec);
  else
    {
      MoveObj(stack,2,NSP_OBJECT(fvec));
      if ( lhs >= 3) 
	{
	  if ( nsp_move_double(stack,3, (double) info)== RET_BUG ) goto bug;
	  if ( lhs >= 4 )
	    {
	      if ( nsp_move_double(stack,4, (double) nfev)== RET_BUG ) goto bug;
	      if ( lhs >= 5 )
		if ( nsp_move_double(stack,5, (double) njev)== RET_BUG ) goto bug;
	    }
	}
    }

  return Max(lhs,1);

 bug:
  hybr_clean(&Hybr_data);
  nsp_matrix_destroy(fvec);
  FREE(work); FREE(ipvt);
  return RET_BUG;
}

/* evaluates f and jacobian only  */

static int int_minpack_lsq_eval_jac (Stack stack, int rhs, int opt, int lhs)
{ 
  NspObject *fcn,*jac=NULLOBJ,*args=NULLOBJ;
  nsp_option opts[] ={{ "args",obj,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspMatrix *X, *work1=NULLMAT,*work2=NULLMAT,*work2bis=NULLMAT,*work3=NULLMAT;
  int n,nn,nnbis,info=0,m=-1;
  double tol;
  hybr_data Hybr_data;
  
  CheckStdRhs(3,3);
  CheckLhs(1,2);

  tol = sqrt (minpack_dpmpar (1));

  if ((X = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  n = X->mn;
  if ( n <= 0  ) 
    {
      Scierror("Error: x is empty\n");
      return RET_BUG;
    }

  if ((fcn = get_function(stack,2,HYBR_fcn,&Hybr_data))==NULL) 
    return RET_BUG;

  if (GetScalarInt(stack,3,&m) == FAIL) return RET_BUG;
  if ( m < n  ) 
    {
      Scierror("Error: m (=%d) is inferior to the number of unknows n (=%d)", m, n);
      return RET_BUG; 
    }
  
  if ( get_optional_args(stack,rhs,opt,opts,&args) == FAIL) return RET_BUG;

  if ( hybr_prepare(m,X->mn,fcn,jac,args,&Hybr_data)==FAIL) return RET_BUG;


  if ((work1 =nsp_matrix_create(NVOID,'r',m,1)) == NULLMAT) goto bug;
  nn=5*X->mn+m;
  if ((work2 =nsp_matrix_create(NVOID,'r',nn,1)) == NULLMAT) goto bug;
  nnbis=m*X->mn;
  if ((work2bis =nsp_matrix_create(NVOID,'r',m,X->mn)) == NULLMAT) goto bug;
  if ((work3 =nsp_matrix_create(NVOID,'r',X->mn,1)) == NULLMAT) goto bug;
  minpack_lmdif2(Hybr_data.f_lfcn,&m,&X->mn,X->R,work1->R,&tol,&info,work3->I,work2->R,&nn,work2bis->R,&nnbis,&Hybr_data);
  
  if ( info == 0) 
    {
      Scierror("Error: improper input parameters in %s\n",NspFname(stack));
      goto bug;
    }
  else if ( info < 0) 
    {
      Scierror("Error: execution of %s aborted\n",NspFname(stack));
      goto bug;
    }

  hybr_clean(&Hybr_data);

  MoveObj(stack,1,NSP_OBJECT(work1));

  if ( lhs >= 2) 
    {
      /* attention on veut work2 mais la fin i.e apres m+ 5*n */
      MoveObj(stack,2,NSP_OBJECT(work2bis));
    }
  if ( work3 != NULL) nsp_matrix_destroy(work3);
  if ( work2 != NULL) nsp_matrix_destroy(work2);
  return Max(lhs,1);
 bug:
  if ( work1 != NULL) nsp_matrix_destroy(work1);
  if ( work2 != NULL) nsp_matrix_destroy(work2);
  if ( work2bis != NULL) nsp_matrix_destroy(work2bis);
  if ( work3 != NULL) nsp_matrix_destroy(work3);
  return RET_BUG;
}




/*--------------------------------------------------------------
 * objective 
 *-------------------------------------------------------------*/

/*
 * hybr_prepare:
 * if m < 0 prepare for hybrd or hybrj 
 * if m >= 0 then for lmdiff or lmderr
 **/

static int hybr_prepare(int m,int n,NspObject *fcn,NspObject *jac,NspObject *args,hybr_data *obj)
{
  if (( obj->fcn =nsp_object_copy(fcn)) == NULL) return FAIL;
  if (( nsp_object_set_name(obj->fcn,"hybr_fcn")== FAIL)) return FAIL;
  if ( jac != NULL ) 
    {
      if (( obj->jac =nsp_object_copy(jac)) == NULL) return FAIL;
      if (( nsp_object_set_name(obj->jac,"hybr_jac")== FAIL)) return FAIL;
    }
  else
    {
      obj->jac = NULL;
    }
  if ( args != NULL ) 
    {
      if (( obj->args = nsp_object_copy(args)) == NULL ) return FAIL;
      if (( nsp_object_set_name((NspObject *) obj->args,"arg")== FAIL)) return FAIL;
    }
  else 
    {
      obj->args = NULL;
    }
  if ((obj->x = nsp_matrix_create("x",'r',n,1))== NULL) return FAIL;
  return OK;
}

/*
 * hybr_clean:
 **/

static void hybr_clean(hybr_data *obj)
{
  if ( obj->args != NULL) nsp_object_destroy(&obj->args);
  nsp_object_destroy(&obj->fcn);
  if ( obj->jac != NULL)   nsp_object_destroy(&obj->jac);
  nsp_matrix_destroy(obj->x);
}

/*
 * soft evaluation of fcn 
 */

static int hybr_lfcn(const int *m,const int *n, double *x, double *fvec, int *iflag,void *hybr_obj_d)
{
  hybr_data *hybr_obj = hybr_obj_d;
  NspObject *targs[2];/* arguments to be transmited to hybr_obj->objective */
  NspObject *nsp_ret;
  int nret = 1,nargs = 1, nres;
  
  targs[0]= NSP_OBJECT(hybr_obj->x); 
  memcpy(hybr_obj->x->R,x,hybr_obj->x->mn*sizeof(double));
  /* for ( i= 0 ; i < hybr_obj->x->mn ; i++) hybr_obj->x->R[i]= x[i];*/
  nres= *m;
  if (hybr_obj->args != NULL ) 
    {
      targs[1]= NSP_OBJECT(hybr_obj->args);
      nargs= 2;
    }
  /* FIXME : a changer pour metre une fonction eval standard */
  if ( nsp_gtk_eval_function((NspPList *)hybr_obj->fcn ,targs,nargs,&nsp_ret,&nret)== FAIL) 
    {
      *iflag= -1;
      return FAIL;
    }
  if ( nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' 
       &&((NspMatrix *) nsp_ret)->mn == nres ) 
    {
      /* for ( i= 0 ; i < nres ; i++) fvec[i] = ((NspMatrix *) nsp_ret)->R[i]; */
      memcpy(fvec,((NspMatrix *) nsp_ret)->R,nres*sizeof(double));
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else 
    {
      Scierror("Error: %s objective function returned argument is wrong\n","fsolve");
      if ( nret != 1 ) Scierror("\t too many arguments returned (%d)\n",nret);
      if ( ! IsMat(nsp_ret) ) Scierror("\t returned argument should be a real matrix \n");
      if ( IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type != 'r') 
	Scierror("\t returned matrix should be real\n");
      if ( IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->mn != nres )
	Scierror("\t returned matrix should be of size %d\n",nres);
      *iflag= -1 ;
      return FAIL;
    }
  return OK;
}

static int hybr_fcn(const int *n, double *x, double *fvec, int *iflag,void *hybr_obj_d)
{
  int m=*n;
  return hybr_lfcn(&m,n,x,fvec,iflag,hybr_obj_d);
}


/*
 * soft evaluation of jacobian. 
 */

static int hybr_ljac(const int *m,const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *hybr_obj_d)
{
  hybr_data *hybr_obj = hybr_obj_d;
  NspObject *targs[2];/* arguments to be transmited to hybr_obj->objective */
  NspObject *nsp_ret;
  int nret = 1, nargs = 1, nres;
  
  targs[0]= NSP_OBJECT(hybr_obj->x); 
  memcpy(hybr_obj->x->R,x,hybr_obj->x->mn*sizeof(double));
  /* for ( i= 0 ; i < hybr_obj->x->mn ; i++) hybr_obj->x->R[i]= x[i]; */
  nres= (*m)*(*n);

  if (hybr_obj->args != NULL ) 
    {
      targs[1]= NSP_OBJECT(hybr_obj->args);
      nargs= 2;
    }
  /* FIXME : a changer pour metre une fonction eval standard */
  if ( nsp_gtk_eval_function((NspPList *)hybr_obj->jac ,targs,nargs,&nsp_ret,&nret)== FAIL) 
    {
      *iflag= -1;
      return FAIL;
    }
  if ( nret ==1 && IsMat(nsp_ret) && ((NspMatrix *) nsp_ret)->rc_type == 'r' 
       &&((NspMatrix *) nsp_ret)->mn == nres ) 
    {
      /* for ( i= 0 ; i < nres ; i++) fjac[i] = ((NspMatrix *) nsp_ret)->R[i]; */
      memcpy(fjac,((NspMatrix *) nsp_ret)->R,nres*sizeof(double));
      nsp_object_destroy((NspObject **) &nsp_ret);
    }
  else 
    {
      Scierror("Error: %s objective function returned argument is wrong\n","fsolve");
      /* NSP_OBJECT(f)->name); */
      *iflag= -1 ;
      return FAIL;
    }
  return OK;
}


static int hybr_jac(const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *hybr_obj_d)
{
  int m=*n;
  return hybr_ljac(&m,n,x,fjac,ldfjac,iflag,hybr_obj_d);
}

/* function for hybrj 
 *
 */

static int hybrj_fcn(const int *n,double *x,double *fvec,double *fjac,int *ldfjac,int *iflag,void *data)
{
  hybr_data *hybr_obj = data;
  if ( *iflag == 1 ) 
    {
      /* calculates fvec */
      return (hybr_obj->f_fcn)(n,x,fvec,iflag,data);
    }
  else if ( *iflag == 2 )
    {
      /* calculates jacobian */
      return (hybr_obj->f_jac)(n,x,fjac,ldfjac,iflag,data);
    }
  else if ( *iflag == 0) 
    {
      /* calculates nothing ? 
       */
      (hybr_obj->f_fcn)(n,x,fvec,iflag,data);
      (hybr_obj->f_jac)(n,x,fjac,ldfjac,iflag,data);
    }
  return OK;
}


/* function for lmder 
 *
 */

static int lmder_fcn(const int *m,const int *n,double *x,double *fvec,double *fjac,int *ldfjac,int *iflag,void *data)
{
  hybr_data *hybr_obj = data;
  if ( *iflag == 1 ) 
    {
      /* calculates fvec */
      return (hybr_obj->f_lfcn)(m,n,x,fvec,iflag,data);
    }
  else if ( *iflag == 2 )
    {
      return (hybr_obj->f_ljac)(m,n,x,fjac,ldfjac,iflag,data);
    }
  else if ( *iflag == 0) 
    {
      (hybr_obj->f_lfcn)(m,n,x,fvec,iflag,data);
      (hybr_obj->f_ljac)(m,n,x,fjac,ldfjac,iflag,data);
    }
  return OK;
}


/* FIXME: should be in a .h */

extern int SearchInDynLinks (char *op, int (**realop)());

static NspObject *get_function_obj(Stack stack, int pos,NspObject *obj,HYBR_ftype type,hybr_data *data)
{
  if ( IsNspPList(obj) )
    {
      /* soft coded function */
      switch (type) 
	{
	case HYBR_fcn: data->f_fcn = hybr_fcn;data->f_lfcn = hybr_lfcn;break;
	case HYBR_jac: data->f_jac = hybr_jac;data->f_ljac = hybr_ljac;break;
	}
      return obj;
    }
  else if ( IsString(obj) )
    {
      char *str = ((NspSMatrix *)obj)->S[0];
      int (*func) (void);
      /* search string in the dynamically linked functions */
      if ( SearchInDynLinks(str, &func) == -1 )
	{
	  Scierror("Error: function %s is not dynamically linked in nsp\n",str);
	  return NULL;
	}
      switch (type) 
	{
	case HYBR_fcn: 
	  data->f_fcn= (minpack_fcn1) func;
	  data->f_lfcn= (minpack_fcn2) func;
	  break;
	case HYBR_jac:
	  data->f_jac= (minpack_fcn6) func ;
	  data->f_ljac= (minpack_fcn7) func ;
	  break;
	}
      return obj;
    }
  else
    {
      Scierror("Error: argument %d should be a function or a string\n",pos,NspFname(stack));
      return NULL;
    }
  return NULL;
}

static NspObject *get_function(Stack stack, int pos,HYBR_ftype type,hybr_data *data)
{
  NspObject *obj;
  if ( (obj=nsp_get_object(stack,pos)) == NULLOBJ ) return NULLOBJ;
  return get_function_obj(stack,pos,obj,type,data);
}



/* interface used to call predefined examples
 * 
 */

static char *test_names[]={ 
  "rosenbrock",
  "powell_singular",
  "powell_badly_scaled",
  "wood",
  "helical_valley",
  "watson",
  "chebyquad",
  "brown",
  "discrete_boundary",
  "discrete_integral",
  "trigonometric",
  "variably_dimensioned",
  "broyden_tridiagonal",
  "broyden_banded",
  "fsol1",
  NULL
};

static minpack_fcn1 test_functions[]={
  minpack_rosenbrock ,
  minpack_powell_singular ,
  minpack_powell_badly_scaled ,
  minpack_wood ,
  minpack_helical_valley ,
  minpack_watson ,
  minpack_chebyquad ,
  minpack_brown ,
  minpack_discrete_boundary ,
  minpack_discrete_integral ,
  minpack_trigonometric ,
  minpack_variably_dimensioned,
  minpack_broyden_tridiagonal ,
  minpack_broyden_banded ,
  minpack_fsol1 ,
  NULL
};


static int int_minpack_hybrd_test (Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *X,*res;
  int rep,iflag=1;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((X = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  if ((rep= GetStringInArray(stack,2,test_names,1)) == -1) return RET_BUG; 
  if ((res =nsp_matrix_create(NVOID,'r',X->m,X->n)) == NULLMAT) return RET_BUG;
  (test_functions[rep])(&X->mn,X->R,res->R,&iflag,NULL);
  if ( iflag < 0 ) 
    {
      Scierror("Error: execution of %s aborted\n",NspFname(stack));
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(res));
  return 1;
}

extern int minpack_jac_rosenbrock (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);
extern int minpack_jac_powell_singular (const int *n, double *x, double *fjac, int *ldfjac, int *iflag, void *data);


static char *test_jac_names[]=
  {
    "jac_rosenbrock",
    "jac_powell_singular",
    "jac_powell_badly_scaled",
    "jac_wood",
    "jac_helical_valley",
    "jac_watson",
    "jac_chebyquad",
    "jac_brown",
    "jac_discrete_boundary",
    "jac_discrete_integral",
    "jac_trigonometric",
    "jac_variably_dimensioned",
    "jac_broyden_tridiagonal",
    "jac_broyden_banded",
    "jac_fsol1",
    NULL 
  };

static minpack_fcn6 test_jac_functions[]=
  {
    minpack_jac_rosenbrock ,
    minpack_jac_powell_singular ,
    minpack_jac_powell_badly_scaled ,
    minpack_jac_wood ,
    minpack_jac_helical_valley ,
    minpack_jac_watson ,
    minpack_jac_chebyquad ,
    minpack_jac_brown ,
    minpack_jac_discrete_boundary ,
    minpack_jac_discrete_integral ,
    minpack_jac_trigonometric ,
    minpack_jac_variably_dimensioned ,
    minpack_jac_broyden_tridiagonal ,
    minpack_jac_broyden_banded ,
    minpack_jac_fsol1 ,
    NULL
  };


static int int_minpack_hybrd_jac_test (Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *X,*res;
  int rep,iflag=1;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((X = GetRealMatCopy(stack,1)) == NULLMAT) return RET_BUG;
  if ((rep= GetStringInArray(stack,2,test_jac_names,1)) == -1) return RET_BUG; 
  if ((res =nsp_matrix_create(NVOID,'r',X->mn,X->mn)) == NULLMAT) return RET_BUG;
  (test_jac_functions[rep])(&X->mn,X->R,res->R,&X->mn,&iflag,NULL);
  if ( iflag < 0 ) 
    {
      Scierror("Error: execution of %s aborted\n",NspFname(stack));
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(res));
  return 1;
}



static OpWrapTab libminpack_func[] = {
  {"fsolve", int_minpack_fsolve,NULL},
  {"fsolve_jac", int_minpack_eval_jac,NULL},
  {"fsolve_lsq", int_minpack_lsq,NULL},
  {"fsolve_lsq_jac", int_minpack_lsq_eval_jac,NULL},
  {"fsolve_test", int_minpack_hybrd_test,NULL},
  {"fsolve_jac_test", int_minpack_hybrd_jac_test,NULL},
  {(char *) 0, NULL, NULL},
};

int libminpack_Interf (int i, Stack stack, int rhs, int opt, int lhs)
{
  if ( libminpack_func[i].wrapper == NULL)
     return (*(libminpack_func[i].fonc)) (stack, rhs, opt, lhs);
  else 
     return (*(libminpack_func[i].wrapper)) (stack, rhs, opt, lhs,libminpack_func[i].fonc);
}
void libminpack_Interf_Info (int i, char **fname, function (**f))
{
 *fname = libminpack_func[i].name;
 *f = libminpack_func[i].fonc;
}
