/* 
 * interface for approximation/interpolation of Nsp 
 * Copyright (C) 2005-2009  Bruno Pincon
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


#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <strings.h>
#include <nsp/machine.h>
#include <nsp/math.h>
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"
#include "nsp/approx.h"

static int good_order(const double x[], int n)
{
  /*  test if x[i-1] < x[i] */
  int i;

  if (fabs(x[0]) > DBL_MAX  ||  x[n-1] >  DBL_MAX)
    return 0;

  for ( i = 1 ; i < n ; i++ ) 
    if ( ! (x[i-1] < x[i]) )   /* form of this test for detecting nan ... */ 
      return 0;

  return 1;
}

static int get_spline_type(const char *str)
{
  if (strcmp(str,"not_a_knot")==0) 
    return NOT_A_KNOT;
  else if (strcmp(str,"clamped")==0) 
    return CLAMPED;
  else if (strcmp(str,"natural")==0) 
    return NATURAL;
  else if (strcmp(str,"periodic")==0) 
    return PERIODIC;
  else if (strcmp(str,"monotone")==0) 
    return MONOTONE;
  else if (strcmp(str,"fast")==0) 
    return FAST;
  else if (strcmp(str,"fast_periodic")==0) 
    return FAST_PERIODIC;
  else
    return UNDEFINED;
}

static int get_outmode(const char *str)
{
  if (strcmp(str,"C0")==0) 
    return C0;
  else if (strcmp(str,"by_zero")==0) 
    return BY_ZERO;
  else if (strcmp(str,"natural")==0) 
    return NATURAL;
  else if (strcmp(str,"periodic")==0) 
    return PERIODIC;
  else if (strcmp(str,"by_nan")==0) 
    return  BY_NAN;
  else if (strcmp(str,"linear")==0) 
    return  LINEAR;
  else
    return UNDEFINED;
}


int int_nsp_linear_interpn( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  interface on the ndim linear interpolation routine
   *
   *   yp = linear_interpn(xp1, ..., xpn, x1, ..., xn, val, outmode)
   */
  char *str;
  int n = (rhs+1)/2 - 1, m, mxp, nxp, mnxp, i, *ndim, ndim_prod=1, *ad, *k, outmode;
  double **xp, **x, *u, *v, *val;
  NspMatrix *B;

  if ( n < 1 )
    { 
      Scierror("%s: too few arg\n", NspFname(stack));
      return RET_BUG;
    }
  m = 1; for ( i = 1 ; i <= n ; i++) m = 2*m;

  /* allocate some work arrays */
  xp = malloc(n*sizeof(double*));
  x =  malloc(n*sizeof(double*));
  ndim = malloc(n*sizeof(int));
  u = malloc(n*sizeof(double));
  v = malloc(m*sizeof(double));
  ad =  malloc(m*sizeof(int));
  k =  malloc(n*sizeof(int));
  if ( ! (xp && x && ndim && u && v && ad && k) )
    {
      Scierror("%s : running out of memory\n", NspFname(stack));
      goto err;
    }

  /* parse input arg */
  if ((B = GetMat(stack,1)) == NULLMAT) goto err;
  mxp = B->m; nxp = B->n; mnxp = B->mn; xp[0] = B->R;

  for ( i = 2 ; i <= n ; i++ )
    {
      if ((B = GetMat(stack,i)) == NULLMAT) goto err;
      if ( mxp != B->m || nxp != B->n )
	{ 
	  Scierror("%s: bad inputs for xp1, xp2, ...., \n", NspFname(stack));
	  goto err;
	}
      xp[i-1] = B->R;
    }

  for ( i = 1 ; i <= n ; i++ )
    {
      if ((B = GetMat(stack,i+n)) == NULLMAT) goto err;
      CheckVector(NspFname(stack),i+n,B);
      if (B->mn < 2)
	{ 
	  Scierror("%s: bad arg number %d \n", NspFname(stack), n+i);
	  goto err;
	}
      if ( !good_order(B->R, B->mn) )
	{
	  Scierror("%s: grid abscissae of dim %d not in strict increasing order \n", NspFname(stack), n+i);
	  goto err;
	}
      ndim[i-1] = B->mn;
      ndim_prod *= B->mn;
      x[i-1] = B->R;
     }
  
  /* value of the function on the grid (when nsp will support nd arrays this would change) */
  if ((B = GetMat(stack,2*n+1)) == NULLMAT) goto err;
  if ( B->mn != ndim_prod )
    {
      Scierror("%s: bad dimension for the grid values \n", NspFname(stack));
      goto err;
    }
  val = B->R;

  /* get the outmode */
  if ( rhs == 2*n + 2 )
    {
      if ((str = GetString(stack,2*n+2)) == (char*)0) goto err;
      outmode = get_outmode(str);
      if (outmode == UNDEFINED || outmode == LINEAR)
	{
	  Scierror("%s: %s is an unknown or unsupported outmode \n",NspFname(stack), str);
	  goto err;
	}
    }
  else
    outmode = C0;

  /* allocate memory for the output */
  if ((B = nsp_matrix_create(NVOID,'r',mxp,nxp))== NULLMAT) 
    goto err;

  nsp_nlinear_interp(x, val, ndim, n, xp, B->R, mnxp, outmode, u, v, ad, k);

  free(xp); free(x); free(ndim); free(u); free(v); free(ad); free(k);

  MoveObj(stack,1,(NspObject *) B);
  return 1;

 err:
  free(xp); free(x); free(ndim); free(u); free(v); free(ad); free(k);
  return  RET_BUG;
}


static int int_nsp_splin( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  interface on the splin routine
   *
   *   d = splin(x, y [,splin_type, [end_slopes]])
   */
  char *str;
  NspMatrix *x, *y, *s=NULLMAT, *d;
  double *Ad=NULL, *Asd=NULL, *qdy=NULL, *lll=NULL;
  int n, spline_type;

  CheckRhs(2,4);
  CheckLhs(1,4);

  if ((x = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((y = GetMat(stack,2)) == NULLMAT) return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x,y);
  CheckVector(NspFname(stack),1,x);
  n = x->mn;    /* number of interpolation points */
  if ( n < 2 ) 
    { 
      Scierror("%s: the number of interpolation points must be >= 2\n", NspFname(stack));
      return RET_BUG;
    }

  if (! good_order(x->R, n))  /* verify strict increasing abscissae */
    {
      Scierror("%s: elts of arg 1 not (strictly) increasing or +-inf detected\n",  NspFname(stack));
      return RET_BUG;
    }

  if ( rhs >= 3 )   /* get the spline type */
    {
      if ((str = GetString(stack,3)) == (char*)0) return RET_BUG;
      spline_type = get_spline_type(str);
      if ( spline_type == UNDEFINED )
	{
	  Scierror("%s: %s is an unknown or unsupported spline type\n",NspFname(stack), str);
	  return RET_BUG;
	}
    }
  else
    spline_type = NOT_A_KNOT;

  if ( spline_type == CLAMPED ) /* get arg 4 which contains the end point slopes */
    {
      if ( rhs != 4 )
	{
	  Scierror("%s: for a clamped spline you must give the endpoint slopes\n",NspFname(stack));
	  return RET_BUG;
	}
      if ((s = GetMat(stack,4)) == NULLMAT) return RET_BUG;
      CheckLength(NspFname(stack),4,s,2);
    }
  else if ( rhs == 4 )
    {
      Scierror("%s: 4 args are required only for a clamped spline\n",NspFname(stack));
      return RET_BUG;
    }
    
  /*  verify y[0] = y[n-1] for periodic splines */
  if ( (spline_type == PERIODIC || spline_type == FAST_PERIODIC)  &&  y->R[0] != y->R[n-1] )
    {
      Scierror("%s: for a periodic spline y(1) must be equal to y(n)\n",NspFname(stack));
      return RET_BUG;
    }

  if ((d = nsp_matrix_create(NVOID,'r',x->m,x->n))== NULLMAT) return RET_BUG;

  switch(spline_type)
    {
    case(FAST) : case(FAST_PERIODIC) :
      nsp_derivd(x->R, y->R, d->R, n, 1, spline_type);
      break;

    case(MONOTONE) :
      nsp_dpchim(x->R, y->R, d->R, n, 1);
      break;

    case(NOT_A_KNOT) : case(NATURAL) : case(CLAMPED) : case(PERIODIC) :
      Ad = nsp_alloc_work_doubles(n);
      Asd = nsp_alloc_work_doubles(n-1);
      qdy = nsp_alloc_work_doubles(n-1);
      if (spline_type == PERIODIC) lll= nsp_alloc_work_doubles(n-1);

      if ( Ad == NULL || Asd == NULL || qdy == NULL || (spline_type == PERIODIC && lll == NULL) )
	{
	  FREE(Ad); FREE(Asd); FREE(qdy); FREE(lll);
	  return RET_BUG;
	}

      if (spline_type == CLAMPED) 
	{ d->R[0] = s->R[0]; d->R[n-1] = s->R[1]; };

      nsp_cubic_spline(x->R, y->R, d->R, n, spline_type, Ad, Asd, qdy, lll);
      FREE(Ad); FREE(Asd); FREE(qdy); FREE(lll);
      break;
    }

  MoveObj(stack,1,(NspObject *) d);
  return 1;
}


static int int_nsp_interp( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  interface on the interp routine
   *
   *   [st [, dst [, d2st [, d3st]]]] = interp(t, x, y, d [,outmode])
   */
  char *str;
  NspMatrix *t, *x, *y, *d, *st, *dst, *d2st, *d3st;
  int m, n, outmode;

  CheckRhs(4,5);
  CheckLhs(1,4);

  if ((t = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((x = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ((y = GetMat(stack,3)) == NULLMAT) return RET_BUG;
  if ((d = GetMat(stack,4)) == NULLMAT) return RET_BUG;

  CheckSameDims(NspFname(stack),2,3,x,y);
  CheckSameDims(NspFname(stack),2,4,x,d);
  CheckVector(NspFname(stack),2,x);

  /* must we verify also strict increasing abscissae (x) ? */
  m = t->mn;
  n = x->mn;
  if ( x->mn < 2 )
    { 
      Scierror("%s: length of x must be >= 2  \n", NspFname(stack));
      return 0;
    }

  if ( rhs == 5 )   /* get the outmode */
    {
      if ((str = GetString(stack,5)) == (char*)0) return RET_BUG;
      outmode = get_outmode(str);
      if ( outmode == UNDEFINED )
	{
	  Scierror("%s: %s is an unknown or unsupported outmode \n",NspFname(stack), str);
	  return RET_BUG;
	}
    }
  else
    outmode = C0;  /* default outmode */

  /* memory for st, dst, ddst, dddst */
  if ((st = nsp_matrix_create(NVOID,'r',t->m,t->n))== NULLMAT) return RET_BUG;
  if ((dst = nsp_matrix_create(NVOID,'r',t->m,t->n))== NULLMAT) return RET_BUG;
  if ((d2st = nsp_matrix_create(NVOID,'r',t->m,t->n))== NULLMAT) return RET_BUG;
  if ((d3st = nsp_matrix_create(NVOID,'r',t->m,t->n))== NULLMAT) return RET_BUG;

  nsp_eval_piecewise_hermite(t->R, st->R, dst->R, d2st->R, d3st->R, m, x->R, y->R, d->R, n, outmode);

  MoveObj(stack,1,(NspObject *) st);
  if (lhs > 1)
    {
      MoveObj(stack,2,(NspObject *) dst);
      if (lhs > 2)
	{
	  MoveObj(stack,3,(NspObject *) d2st);
	  if (lhs > 3)
	    MoveObj(stack,4,(NspObject *) d3st);
	}
    }
  return lhs;
}


static int int_nsp_splin2d( Stack stack, int rhs, int opt, int lhs)
{ 
  /*  interface on the splin2d routine
   *
   *   C = splin2d(x, y, z [, splin_type])
   */
  char *str;
  NspMatrix *x, *y, *z, *C;
  int spline_type;

  CheckRhs(3,4);
  CheckLhs(1,4);

  if ((x = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),1,x);
  if ((y = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),2,y);

  if ( x->mn < 2 || y->mn < 2 )
    {
      Scierror("%s: first and second arguments must have at least 2 components\n", NspFname(stack));
      return RET_BUG;
    }

  if (! good_order(x->R, x->mn))  /* verify strict increasing abscissae */
    {
      Scierror("%s: elts of arg 1 not (strictly) increasing or +-inf or Nan detected\n",  NspFname(stack));
      return RET_BUG;
    }

  if (! good_order(y->R, y->mn))  /* verify strict increasing abscissae */
    {
      Scierror("%s: elts of arg 2 not (strictly) increasing or +-inf or Nan detected\n",  NspFname(stack));
      return RET_BUG;
    }
      
  if ((z = GetMat(stack,3)) == NULLMAT) return RET_BUG;
  if ( z->m != x->mn || z->n != y->mn )
    {
      Scierror("%s: incompatibility of arg 3 with arg 1 and arg 2\n",  NspFname(stack));
      return RET_BUG;
    }


  if ( rhs == 4)   /* get the spline type */
    {
      if ((str = GetString(stack,4)) == (char*)0) return RET_BUG;
      spline_type = get_spline_type(str);
      if ( spline_type == UNDEFINED ||  spline_type == CLAMPED )
	{
	  Scierror("%s: %s is an unknown or unsupported spline type\n",NspFname(stack), str);
	  return RET_BUG;
	}
    }
  else
    spline_type = NOT_A_KNOT;

  if ( (C = nsp_matrix_create(NVOID,'r',16*(x->mn-1)*(y->mn-1),1)) == NULLMAT ) 
    return RET_BUG;

  switch(spline_type)
    {
    case(MONOTONE) :  case(FAST) : case(FAST_PERIODIC) :
      if ( nsp_bicubic_subspline(x->R, y->R, z->R, x->mn, y->mn, C->R, spline_type) == FAIL )
	{
	  Scierror("%s: running out of memory\n", NspFname(stack));
	  return RET_BUG;
	}
      break;

    case(NOT_A_KNOT) : case(NATURAL) : case(PERIODIC) :
      if ( nsp_bicubic_spline(x->R, y->R, z->R, x->mn, y->mn, C->R, spline_type) == FAIL )
	{
	  Scierror("%s: running out of memory\n", NspFname(stack));
	  return RET_BUG;
	}
      break;
    }

  MoveObj(stack,1,(NspObject *) C);
  return 1;
}

static int int_nsp_interp2d(Stack stack, int rhs, int opt, int lhs)
{ 
  /*  interface on the interp2d routine
   *
   *   [u [, dudx, dudy]] = interp2d(xm, ym, x, y, C [,outmode])
   */
  char *str;
  NspMatrix *xm, *ym, *x, *y, *C, *u, *dudx, *dudy;
  int nx, ny, outmode;

  CheckRhs(5,6);
  CheckLhs(1,3);

  if ((xm = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((ym = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  CheckSameDims(NspFname(stack),1,2,xm,ym);
  if ((x = GetMat(stack,3)) == NULLMAT) return RET_BUG;
  if ((y = GetMat(stack,4)) == NULLMAT) return RET_BUG;
  if ((C = GetMat(stack,5)) == NULLMAT) return RET_BUG;
  if ( x->mn < 2 || y->mn < 2 || C->mn != 16*(x->mn-1)*(y->mn-1) )
    {
      Scierror("%s: incompatible arguments\n", NspFname(stack));
      return RET_BUG;
    }

  nx = x->mn;
  ny = y->mn;

  if ( rhs == 6 )   /* get the outmode */
    {
      if ((str = GetString(stack,6)) == (char*)0) return RET_BUG;
      outmode = get_outmode(str);
      if ( outmode == UNDEFINED || outmode == LINEAR )
	{
	  Scierror("%s: %s is an unknown or unsupported outmode \n",NspFname(stack), str);
	  return RET_BUG;
	}
    }
  else
    outmode = C0;  /* default outmode */

  /* create output arg */
  if ((u = nsp_matrix_create(NVOID,'r',xm->m,xm->n))== NULLMAT) return RET_BUG;

  if ( lhs == 1 )
    nsp_eval_bicubic(x->R, y->R, C->R, nx, ny, xm->R, ym->R, u->R, NULL, NULL, u->mn, outmode);
  else
    {
      if ((dudx = nsp_matrix_create(NVOID,'r',xm->m,xm->n))== NULLMAT) return RET_BUG;
      if ((dudy = nsp_matrix_create(NVOID,'r',xm->m,xm->n))== NULLMAT) return RET_BUG;
      nsp_eval_bicubic(x->R, y->R, C->R, nx, ny, xm->R, ym->R, u->R, dudx->R, dudy->R, u->mn, outmode);
    }

  MoveObj(stack,1,(NspObject *) u);
  if (lhs > 1)
    {
      MoveObj(stack,2,(NspObject *) dudx);
      if (lhs > 2)
	{
	  MoveObj(stack,3,(NspObject *) dudy);
	}
    }
  return lhs;
}


static OpTab Approx_func[]={
    {"linear_interpn", int_nsp_linear_interpn},
    {"splin", int_nsp_splin},
    {"interp", int_nsp_interp},
    {"splin2d", int_nsp_splin2d},
    {"interp2d", int_nsp_interp2d},
    {(char *) 0, NULL}
};

int Approx_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Approx_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Approx_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Approx_func[i].name;
  *f = Approx_func[i].fonc;
}
