/* Nsp
 * Copyright (C) 2007 Bruno Pinçon Esial/Iecn
 * Copyright (C) 2007 Jean-Philippe Chancelier Enpc/Cermics
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
 */

#include <math.h>
#include <string.h>
#include <nsp/machine.h>
#include <nsp/matrix-in.h>
#include <nsp/bmatrix-in.h>
#include <nsp/spmf.h>


static int int_nsp_log1p(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMatCopy (stack, 1)) == NULLMAT )
    return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_log1p(x->R[i]);

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}

static int int_nsp_sinpi(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMatCopy (stack, 1)) == NULLMAT )
    return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_sinpi(x->R[i]);

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}

static int int_nsp_gammabr(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMatCopy (stack, 1)) == NULLMAT )
    return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_gamma(x->R[i]);

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}

static int int_nsp_lngamma(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  int i;
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (x = GetRealMatCopy (stack, 1)) == NULLMAT )
    return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_lngamma(x->R[i]);

  NSP_OBJECT (x)->ret_pos = 1;
  return 1;
}

static int int_nsp_kcdf(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *P;
  int i, n;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (x = GetRealMat(stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( GetScalarInt(stack,2,&n) == FAIL ) return RET_BUG;      

  if ( (P = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT) return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    if ( nsp_kcdf(x->R[i], P->R + i, n) == FAIL )
      {
	nsp_matrix_destroy(P); return RET_BUG;
      }

  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

static int int_nsp_kcdflim(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *P, *Q;
  int i;
  double q;
  CheckRhs (1, 1);
  CheckLhs (1, 2);

  if ( (x = GetRealMat(stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( (P = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT) return RET_BUG;

  if ( lhs == 1 )
    for ( i = 0 ; i < x->mn ; i++ )
      P->R[i] = nsp_kcdflim(x->R[i], &q);
  else  /* lhs==2 need q = 1 - p too */
    {
      if ( (Q = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT)
	{
	  nsp_matrix_destroy(P);
	  return RET_BUG;
	}
      for ( i = 0 ; i < x->mn ; i++ )
	P->R[i] = nsp_kcdflim(x->R[i], &(Q->R[i]));
    }

  MoveObj(stack,1,(NspObject *) P);
  if ( lhs == 2 )
    MoveObj(stack,2,(NspObject *) Q);

  return lhs;
}

static int int_nsp_kcdfbis(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *P;
  int i, n;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (x = GetRealMat(stack, 1)) == NULLMAT )
    return RET_BUG;

  if ( GetScalarInt(stack,2,&n) == FAIL ) return RET_BUG;      

  if ( (P = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT) return RET_BUG;

  for ( i = 0 ; i < x->mn ; i++ )
    P->R[i] = marsaglia_K(x->R[i], n);

  MoveObj(stack,1,(NspObject *) P);
  return 1;
}

static int verify_cstr(double x[], int nb_elt, int *xmin, int *xmax)
{
  /*    1/ verify that the array x is formed by non negative integers
   *       regularly spaced with increment equal to 1 (if yes return 1
   *       if not return 0)
   *    2/ computes the min and the max
   */
  int i;
  if ( ! (floor(x[0]) == x[0]  &&  x[0] >= 0 ))
    return FAIL;
  for ( i = 1 ; i < nb_elt ; i++ )
    if ( x[i] != x[i-1]+1.0 )
      return FAIL;

  *xmin = (int) x[0];
  *xmax = (int) x[nb_elt-1];
  return OK;
} 

static int int_legendre(Stack stack, int rhs, int opt, int lhs)
{
  /*
   *   Interface onto the (Slatec) dxleg.f code. 
   *   nsp calling sequence :
   *
   *   p = legendre(n, m, x [, norm_flag] )
   *
   *      x is a vector with mnx elements (it is better to
   *        have a row vector but this is not forced)
   *
   *      n : a non negative integer scalar (or a vector of such
   *          integer regularly speced with an increment of 1)
   *      m : same constraints than for n
   *
   *      n and m may not be both vectors
   *
   *      norm_flag : optional. When it is present and equal to "norm"
   *                  it is a normalised version which is computed
   *    AUTHOR 
   *       Bruno Pincon <Bruno.Pincon@iecn.u-nancy.fr>
   */
  NspMatrix *N, *M, *x, *y=NULLMAT;
  int n1, n2, m1, m2, id=3, ierror, i, j, nudiff, MNp1;
  Boolean M_is_scalar, N_is_scalar;
  char *str;
  double xx, dnu1, Inf=1.0/0.0;
  int *ipqa=NULL;

  CheckLhs(1,1); 
  CheckRhs(3,4);

  if ( (N = GetRealMat(stack, 1)) == NULLMAT )
    return RET_BUG;
  if ( verify_cstr(N->R, N->mn, &n1, &n2) == FAIL )
    {
      Scierror("%s: bad first argument \n",NspFname(stack));
      return RET_BUG;
    };
  N_is_scalar = N->mn == 1;

  if ( (M = GetRealMat(stack, 2)) == NULLMAT )
    return RET_BUG;
  if ( verify_cstr(M->R, M->mn, &m1, &m2) == FAIL )
    {
      Scierror("%s: bad second argument \n",NspFname(stack));
      return RET_BUG;
    };
  M_is_scalar = M->mn == 1;
  

  if ( ! M_is_scalar  &&  ! N_is_scalar )
    {
      Scierror("%s: only one of arg1 and arg2 may be a vector \n",NspFname(stack));
      return RET_BUG;
    };
       
  if ( (x = GetRealMat(stack, 3)) == NULLMAT )
    return RET_BUG;
  
  if ( rhs == 4 )
    {
      if ((str = GetString(stack,4)) == (char*)0) return RET_BUG;
      if ( strcmp(str,"norm") == 0)
	id = 4;
    }
  
  MNp1 = Max (n2 - n1, m2 - m1) + 1;

  if ( (ipqa =nsp_alloc_work_int(x->mn*MNp1)) == NULL )
    return RET_BUG;

  if ( (y =nsp_matrix_create(NVOID,'r',MNp1, x->mn)) == NULLMAT )
    {
      FREE(ipqa); return RET_BUG;
    };
 
  nudiff = n2-n1;  
  dnu1 = (double) n1;

  for ( i = 0 ; i < x->mn ; i++ )
    {
      xx = fabs(x->R[i]); /* dxleg computes only for x in [0,1) */
      if ( ! ( xx <= 1.0 ) )
	{
	  Scierror("%s: %d th component of the 3th argument not in [-1,1]\r\n",i+1, NspFname(stack));
	  goto err;
	};

      C2F(dxlegf) (&dnu1, &nudiff, &m1, &m2, &xx, &id, &(y->R[i*MNp1]), &(ipqa[i*MNp1]), &ierror);
      if ( ierror != 0 )
	{
	  if ( ierror == 207 )
	    Scierror("%s: overflow or underflow of an extended range number\r\n", NspFname(stack));
	  else
	    Scierror("%s: error number %d\r\n", NspFname(stack), ierror);
	  goto err;
	};
    }

  /*  dxlegf returns the result under a form (pqa,ipqa) (to 
   *  compute internaly with an extended exponent range)
   *  When the "exponent" part (ipqa) is 0 then the number is exactly
   *  given by pqa else it leads to an overflow or an underflow.
   */
  for ( i = 0 ; i < y->mn ; i++ )
    {
      if ( ipqa[i] < 0 ) 
	y->R[i] = 0.0;
      else if ( ipqa[i] > 0 )
	y->R[i] = y->R[i] * Inf; /* pqa[i] * Inf  to get the sign */
    }

  /* complete the result by odd/even symmetry for negative x */
  for ( i = 0 ; i < x->mn ; i++ ) 
    {
      if ( x->R[i] < 0.0 ) 
	{
	  if ( (n1+m1) % 2 == 1 ) 
	    {
	      for ( j = 0 ; j < MNp1 ; j+=2 )
		y->R[i*MNp1 + j] = -y->R[i*MNp1 + j];
	    }
	  else 
	    {
	      for ( j = 1 ; j < MNp1 ; j+=2 )
		y->R[i*MNp1 + j] = -y->R[i*MNp1 + j];
	    }
	}
    }
   MoveObj(stack,1,(NspObject *) y);
   return 1;

 err:
   FREE(ipqa);
   nsp_matrix_destroy(y);
   return RET_BUG;
}

static int int_nsp_hypot(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *y, *z;
  int i, ix, iy, incx, incy;
  CheckRhs (2, 2);
  CheckLhs (1, 1);

  if ( (x = GetRealMat (stack, 1)) == NULLMAT )
    return RET_BUG;
  incx = x->mn == 1 ? 0 : 1;

  if ( (y = GetRealMat (stack, 2)) == NULLMAT )
    return RET_BUG;
  incy = y->mn == 1 ? 0 : 1;

  if ( incx && incy )
    CheckSameDims( NspFname(stack),1,2,x,y);

  if ( incx )
    {
      if ( (z = nsp_matrix_create(NVOID,'r', x->m, x->n)) == NULLMAT )
	return RET_BUG;
    }
  else /* the following works in both cases (incy = 0 or incy = 1) */
    {
      if ( (z = nsp_matrix_create(NVOID,'r', y->m, y->n)) == NULLMAT )
	return RET_BUG;
    }

  for ( i = 0, ix = 0, iy = 0 ; i < Max(x->mn,y->mn) ; i++, ix+=incx, iy+=incy )
    z->R[i] = nsp_hypot(x->R[ix],y->R[iy]);

   MoveObj(stack,1,(NspObject *) z);
   return 1;
}

static OpTab Spmf_func[]={
  {"log1p", int_nsp_log1p},
  {"sinpi", int_nsp_sinpi},
  {"gammabr", int_nsp_gammabr},
  {"lngamma", int_nsp_lngamma},
  {"kcdf", int_nsp_kcdf},
  {"kcdflim", int_nsp_kcdflim},
  {"kcdfbis", int_nsp_kcdfbis},
  {"legendre", int_legendre},
  {"hypot", int_nsp_hypot},
  {(char *) 0, NULL}
};

int Spmf_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Spmf_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Spmf_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Spmf_func[i].name;
  *f = Spmf_func[i].fonc;
}
