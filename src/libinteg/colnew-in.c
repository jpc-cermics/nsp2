/*
 *     z=bvode(res,ncomp,m,aleft,aright,zeta,ipar,ltol,tol,fixpnt,...
 *	fsub1,dfsub1,gsub1,dgsub1,guess1)
 *
 *     Interface for the colnew program for boundary values problem.
 */

#include "nsp/interf.h"
#include "colnew-n.h"


/*    Example for colnew 
 *    ------------------ 
 */

const double cn_eps = .01;
const double cn_dmu = .01;
const double cn_gamma = 1.1;


int cn_guess1(double *x, double *z, double *dmval, void *args)
{
  double cons, dcons, d2cons,xt;

  --dmval;
  --z;

  /* Function Body */
  xt = sqrt((cn_gamma - 1.) * 2. / cn_gamma);
  cons = cn_gamma * *x * (1. - *x * .5 * *x);
  dcons = cn_gamma * (1. - *x * 1.5 * *x);
  d2cons = cn_gamma * -3. * *x;
  if (*x > xt) 
    {
      z[1] = 0.;
      z[2] = 0.;
      z[3] = -cons;
      z[4] = -dcons;
      dmval[2] = -d2cons;
    }
  else
    {
      z[1] = *x * 2.;
      z[2] = 2.;
      z[3] = *x * -2. + cons;
      z[4] = dcons - 2.;
      dmval[2] = d2cons;
    }
  dmval[1] = 0.;
  return OK;
}

int cn_f1(double *x, double *z, double *f,void *args)
{
  double d1, eps4mu, xt;
  --f;
  --z;

  d1 = cn_eps, d1 *= d1;
  eps4mu = d1 * d1 / cn_dmu;
  xt = sqrt((cn_gamma - 1.) * 2. / cn_gamma);
  f[1] = z[1] / *x / *x - z[2] / *x 
    + (z[1] - z[3] * (1. - z[1] / *x) 
       - cn_gamma * *x * (1. - *x * *x / 2.)) / eps4mu;
  f[2] = z[3] / *x / *x - z[4] / *x 
    + z[1] * (1. - z[1] / 2. / *x) / cn_dmu;
  return OK;
}


int cn_df1(double *x, double *z, double *df,void *args)
{
  double d1,eps4mu,xt;
  /* Parameter adjustments */
  df -= 3;
  --z;

  /* Function Body */
  d1 = cn_eps, d1 *= d1;
  eps4mu = d1 * d1 / cn_dmu;
  xt = sqrt((cn_gamma - 1.) * 2. / cn_gamma);
  df[3] = 1. / *x / *x + (z[3] / *x + 1.) / eps4mu;
  df[5] = -1. / *x;
  df[7] = -(1. - z[1] / *x) / eps4mu;
  df[9] = 0.;
  df[4] = (1. - z[1] / *x) / cn_dmu;
  df[6] = 0.;
  df[8] = 1. / *x / *x;
  df[10] = -1. / *x;
  return OK;
}


int cn_g1(int *i, double *z, double *g,void *args)
{
  --z;
  switch (*i) {
  case 1: *g = z[1]; break;
  case 2: *g = z[3]; break;
  case 3: *g = z[1]; break;
  case 4: *g = z[4] - z[3] * .3 + .7; break;
  }
  return OK;
} 

int cn_dg1(int *i, double *z, double *dg,void *args)
{
  int j;
  --dg;
  --z;

  for (j = 1; j <= 4; ++j) {
    dg[j] = 0.;
  }
  switch (*i) 
    {
    case 1:  dg[1] = 1.; break;
    case 2:  dg[3] = 1.; break;
    case 3:  dg[1] = 1.; break;
    case 4:  dg[4] = 1.; dg[3] = -.3; break;
    }
  return OK;
} 

/*    Example 2 for colnew 
 */

int cn_guess(double *x, double *z, double *dmval, void *args)
{
  z[0]=0;
  dmval[0]=0;
  return OK;
}

int cn_f(double *x, double *z, double *f,void *args)
{
  double x2=x[0]*x[0], x3 = x2*x[0];
  f[0]=(1 -6*x2*z[3]-6*x[0]*z[2])/ x3;
  return OK;
}


int cn_df(double *x, double *z, double *df,void *args)
{
  df[0] = 0;
  df[1] = 0;
  df[2] = - 6. / (x[0]*x[0]);
  df[3] = - 6. / (x[0]);
  return OK;
}


int cn_g(int *i, double *z, double *g,void *args)
{
  switch (*i) {
  case 1: *g = z[0]; break;
  case 2: *g = z[2]; break;
  case 3: *g = z[0]; break;
  case 4: *g = z[2]; break;
  }
  return OK;
} 

int cn_dg(int *i, double *z, double *dg,void *args)
{
  int j;
  for (j = 0; j < 4; ++j) dg[j] = 0.;
  switch (*i) 
    {
    case 1:  dg[0] = 1.; break;
    case 2:  dg[2] = 1.; break;
    case 3:  dg[0] = 1.; break;
    case 4:  dg[2] = 1.; break;
    }
  return OK;
} 

/* #define FORTRAN 1 */

extern int int_colnew( Stack stack, int rhs, int opt, int lhs)
{
  void *args = NULL;
  NspMatrix *res, *m, *zeta, *ipar,*ltol,*tol,*fixpnt;
  double aleft,aright;
  NspMatrix *ispace,*space,*out;
  int ncomp, mstar,iflag=1,i;
  int ferr=OK;
  Fsub fsub = cn_f;
  DFsub dfsub =cn_df;
  Gsub gsub =cn_g;
  DGsub dgsub = cn_dg;
  Guess dguess = cn_guess;

  if ((res= GetMatCopy(stack,1)) == NULLMAT) return RET_BUG;

  if (GetScalarInt(stack,2,&ncomp) == FAIL) return RET_BUG;
  if(ncomp > 20) 
    {
      /* c     .  bvode: ncomp < 20 requested  */
      Scierror("Error: size should .... ");
      return RET_BUG;
    }

  if ((m = GetMatCopy(stack,3)) == NULLMAT) return RET_BUG;
  Mat2int(m);
  mstar=0; 
  for ( i=0 ; i < m->mn ; i++) 
    mstar += m->I[i];
  if (GetScalarDouble(stack,4,&aleft) == FAIL) return RET_BUG;
  if (GetScalarDouble(stack,5,&aright) == FAIL) return RET_BUG;
  if ((zeta= GetMatCopy(stack,6)) == NULLMAT) return RET_BUG;
  
  if(m->mn != ncomp) 
    {
      /* c     .  bvode: m must be of size ncomp */
      Scierror("Error: size should .... ");
      return RET_BUG;
    }
  
  if(mstar > 40) {
    /* c     .  bvode: sum(m must be less than 40 */
    Scierror("Error: size should .... ");
    return RET_BUG;
  }

  if ((ipar = GetMatCopy(stack,7)) == NULLMAT) return RET_BUG;
  if( ipar->mn < 11)
    {
      Scierror("Error: size should .... ");
      return RET_BUG;
    }
  Mat2int(ipar);

  if ((ltol = GetMatCopy(stack,8)) == NULLMAT) return RET_BUG;
  
  if( ipar->I[3] != ltol->mn) 
    {
      /*  bvode: ltol must be of size ipar(4) */
      Scierror("Error: size should .... ");
      return RET_BUG;
    }
  Mat2int(ltol);

  if ((tol = GetMatCopy(stack,9)) == NULLMAT) return RET_BUG;
  if ((fixpnt = GetMatCopy(stack,10)) == NULLMAT) return RET_BUG;
  if( ipar->I[10] != fixpnt->mn &&  ipar->I[10] != 0)
    {
      /* c     .  bvode: fixpnt must be of size ipar(11) */
      Scierror("Error: size should .... ");
      return RET_BUG;
    }
  
  /*      create working arrays */
  if ((ispace = nsp_matrix_create(NVOID,'r',1,ipar->I[5]))== NULLMAT)
    return RET_BUG;

  if ((space = nsp_matrix_create(NVOID,'r',1,ipar->I[4]))== NULLMAT)
    return RET_BUG;
  nsp_colnew_colnew (&ncomp,m->I,&aleft,&aright,zeta->R,
		     ipar->I,ltol->I, tol->R,fixpnt->R,
		     ispace->I, space->R, &iflag, fsub, 
		     dfsub, gsub, dgsub, dguess,args, &ferr) ;
  if( ferr == FAIL) 
    {
      Scierror("Error: error during external evaluation\n");
      return RET_BUG;
    }
  if ( iflag != 1) 
    {
      switch (iflag) 
	{
	case 1: break;
	case 0: 
	  Scierror("Error: the collocation matrix is singular\n");
	  return RET_BUG;
	case -1:
	  Scierror("Error: the expected no. of subintervals exceeds storage specifications\n");
	  return RET_BUG;
	case -2:
	  Scierror("Error:the nonlinear iteration has not converged.\n");
	  return RET_BUG;
	case -3:  
	  Scierror("Error:there is an input data error.=\n");
	  return RET_BUG;
	}
    }
  if ((out = nsp_matrix_create(NVOID,'r',mstar,res->mn))== NULLMAT)
    return RET_BUG;
  for ( i = 0 ; i < res->mn ; i++)
    {
      nsp_colnew_appsln(&res->R[i],&out->R[i*mstar],space->R, ispace->I);
    }
  MoveObj(stack,1, NSP_OBJECT(out));
  return 1;
}



