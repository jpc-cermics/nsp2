/* Nsp
 * Copyright (C) 2007-2009 Bruno Pinçon Esial/Iecn
 * Copyright (C) 2007-2009 Jean-Philippe Chancelier Enpc/Cermics
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

/*
 *   Interface for the dxlegf function (Slatec code).
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

static int int_legendre(Stack stack, int rhs, int opt, int lhs)
{
  Dxblk2 dxblk2 = {0}; /* iflag should be set to zero for initialization */
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
      xx = fabs(x->R[i]); /* dxleg computes only for x in [0,1] */
      if ( ! ( xx <= 1.0 ) )
	{
	  Scierror("%s: %d th component of the 3th argument not in [-1,1]\n",i+1, NspFname(stack));
	  goto err;
	};

      dxlegf(&dxblk2,&dnu1, &nudiff, &m1, &m2, &xx, &id, &(y->R[i*MNp1]), &(ipqa[i*MNp1]), &ierror);
      if ( ierror != 0 )
	{
	  if ( ierror == 207 )
	    Scierror("%s: overflow or underflow of an extended range number\n", NspFname(stack));
	  else
	    Scierror("%s: error number %d\n", NspFname(stack), ierror);
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
   FREE(ipqa);
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

static int int_nsp_primefactors(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *f, *p;
  double x;
  unsigned int factors[9];
  int k, nb_factors, powers[9], ntot, i, j;
  
  CheckRhs (1, 1);
  CheckLhs (1, 2);

  if ( GetScalarDouble(stack, 1, &x) == FAIL )
    return RET_BUG;
    
  if ( x < 0 || x != floor(x) || x > 4294967296.0 )  /* 2^32 */
    {
      Scierror("%s: input should be a positive integer <= 2^32\n", NspFname(stack));
      return RET_BUG;
    }

  if ( x == 4294967296.0 )
    {
      nb_factors = 1; factors[0] = 2; powers[0] = 32;
    }
  else
    {
      nsp_primefactors((unsigned int) x, factors, powers, &nb_factors);
    }

  if ( lhs == 2 )
    {
      if ( (f = nsp_matrix_create(NVOID,'r', 1, nb_factors)) == NULLMAT )
	return RET_BUG;
      if ( (p = nsp_matrix_create(NVOID,'r', 1, nb_factors)) == NULLMAT )
	return RET_BUG;

      for ( k = 0 ; k < nb_factors; k++ )
	{
	  f->R[k] = (double) factors[k];
	  p->R[k] = (double) powers[k];
	}
      MoveObj(stack,1,(NspObject *) f);
      MoveObj(stack,2,(NspObject *) p);
      return 2;
    }
  else /* lhs == 1  */
    {
      ntot = 0;
      for ( k = 0; k < nb_factors; k++ )
	ntot += powers[k];
      if ( (f = nsp_matrix_create(NVOID,'r', 1, ntot)) == NULLMAT )
	return RET_BUG;
      for ( k = 0, i = 0 ; k < nb_factors; k++ )
	for ( j = 0 ; j < powers[k]; j++, i++ )
	  f->R[i] = factors[k];

      MoveObj(stack,1,(NspObject *) f);
      return 1;
    }
}

static int int_nsp_isprime(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *X;
  NspBMatrix *B;
  double x;
  int k;

  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( (X = GetRealMat(stack, 1)) == NULLMAT )
    return RET_BUG;
    
  if ( (B = nsp_bmatrix_create(NVOID, X->m, X->n)) == NULLBMAT )
    return RET_BUG;

  for ( k = 0 ; k < X->mn ; k++ )
    { 
      x = X->R[k];
      if ( x < 0 || x != floor(x) || x > 4294967296.0 )  /* 2^32 */
	{
	  Scierror("%s: components of input argument must be positive integers <= 2^32\n", NspFname(stack));
	  nsp_bmatrix_destroy(B);
	  return RET_BUG;
	}
      B->B[k] = nsp_isprime((unsigned int) x);  /* rmk: in case of x=2^32, (unsigned int) x should be 0 so it is OK */
    }

  MoveObj(stack,1,(NspObject *) B);
  return 1;
}


static int int_nsp_primes(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *p;
  int k, *primes, nb_primes, n;
  
  CheckRhs (1, 1);
  CheckLhs (1, 1);

  if ( GetScalarInt(stack, 1, &n) == FAIL )
    return RET_BUG;
    
  if ( nsp_primes(n, &primes, &nb_primes) == FAIL )
    {
      Scierror("Error:\tRunning out of memory\n");
      return RET_BUG;
    }
  
  if ( (p = nsp_matrix_create(NVOID,'r', 1, nb_primes)) == NULLMAT )
    return RET_BUG;

  for ( k = 0 ; k < nb_primes ; k++ )
    p->R[k] = (double) primes[k];

  if ( nb_primes > 0 )
    FREE(primes);

  MoveObj(stack,1,(NspObject *) p);
  return 1;
}


static int int_convhull2d(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x, *y, *indices;
  int *ind=NULL, *p=NULL, nhull, i;
  CheckRhs(2,2);
  CheckLhs(1,1);

  if ( (x = GetRealMat(stack,1)) == NULLMAT )  return RET_BUG;
  if ( (y = GetRealMat(stack,2)) == NULLMAT )  return RET_BUG;

  CheckSameDims(NspFname(stack),1,2,x,y);
  CheckVector(NspFname(stack),1,x);

  for ( i = 0 ; i < x->mn ; i++ )
    if ( ! (finite(x->R[i]) && finite(y->R[i])) )
      {
	Scierror("%s: the arguments should not have infinite or nan components \n",NspFname(stack));
	return RET_BUG;
      }
  
  ind = nsp_alloc_work_int(x->mn);
  p =  nsp_alloc_work_int(x->mn);
  if ( ind == NULL || p == NULL ) goto err;

  nsp_convhull2d(x->mn, x->R, y->R, &nhull, ind, p);

  if ( nhull > 2 )   /* not degenerate case */
    {
      if ( (indices = nsp_matrix_create(NVOID,'r',1,nhull+1)) == NULLMAT ) goto err;
      for ( i = 0 ; i < nhull ; i++ )
	indices->R[i] = (double) ind[i];
      indices->R[nhull] = (double) ind[0];  /* matlab and octave */
    }
  else               /* for matlab compat we should raise an error */
    {
      if ( (indices = nsp_matrix_create(NVOID,'r',1,nhull)) == NULLMAT ) goto err;
      for ( i = 0 ; i < nhull ; i++ )
	indices->R[i] = (double) ind[i];
    }

  FREE(ind); FREE(p);
  MoveObj (stack, 1, NSP_OBJECT(indices));
  return 1; 

 err:
  FREE(ind); FREE(p);
  return RET_BUG;
}

static int int_nor_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double mu, sigma;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'nor' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&mu) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&sigma) == FAIL) return RET_BUG;      

  if ( ! (sigma > 0.0) )
    { Scierror("Error: pdf('nor',x,mu,sigma), sigma should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_normal(x->R[i], mu, sigma);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_gam_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double a, b;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'gam' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&a) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&b) == FAIL) return RET_BUG;      

  if ( ! (a > 0.0 && b >= 0.0) )
    { Scierror("Error: pdf('gam',x,a,b), a should be > 0 and b should be >= 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_gamma(x->R[i], a, b, 0);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_chi_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double nu;
  int i;
  if ( rhs != 3 ) 
    { Scierror("Error: 1 parameter is required for 'chi' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&nu) == FAIL) return RET_BUG;      

  if ( ! (nu > 0.0) )
    { Scierror("Error: pdf('chi',x,nu), nu should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_chi2(x->R[i], nu);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_exp_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double tau;
  int i;
  if ( rhs != 3 ) 
    { Scierror("Error: 1 parameter is required for 'exp' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&tau) == FAIL) return RET_BUG;      

  if ( ! (tau > 0.0) )
    { Scierror("Error: pdf('exp',x,tau), tau should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_exp(x->R[i], tau);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}
 
static int int_cau_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double sigma;
  int i;
  if ( rhs != 3 ) 
    { Scierror("Error: 1 parameter is required for 'cau' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&sigma) == FAIL) return RET_BUG;      

  if ( ! (sigma > 0.0) )
    { Scierror("Error: pdf('cau',x,sigma), sigma should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_cauchy(x->R[i], sigma);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_par_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double a, b;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'par' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&a) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&b) == FAIL) return RET_BUG;      

  if ( ! (a > 0.0 && b > 0.0) )
    { Scierror("Error: pdf('par',x,a,b), both a and b should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_pareto(x->R[i], a, b);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}
 
static int int_ray_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double sigma;
  int i;
  if ( rhs != 3 ) 
    { Scierror("Error: 1 parameter is required for 'ray' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&sigma) == FAIL) return RET_BUG;      

  if ( ! (sigma > 0.0) )
    { Scierror("Error: pdf('ray',x,sigma), sigma should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_rayleigh(x->R[i], sigma);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}
 
static int int_tray_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double sigma, a;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameter are required for 'tray' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&sigma) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&a) == FAIL) return RET_BUG;      

  if ( ! (sigma > 0.0 && a >= 0.0) )
    { Scierror("Error: pdf('tray',x,sigma,a), sigma should be > 0 and a should be >= 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_tailrayleigh(x->R[i], sigma, a);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_wei_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double a, b;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'wei' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&a) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&b) == FAIL) return RET_BUG;      

  if ( ! (a > 0.0 && b > 0.0) )
    { Scierror("Error: pdf('wei',x,a,b), both a and b should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_weibull(x->R[i], a, b);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}
   
static int int_lap_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double a;
  int i;
  if ( rhs != 3 ) 
    { Scierror("Error: 1 parameter is required for 'lap' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&a) == FAIL) return RET_BUG;      

  if ( ! (a > 0.0) )
    { Scierror("Error: pdf('lap',x,a), a should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_laplace(x->R[i], a);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_logn_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double mu, sigma;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'logn' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&mu) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&sigma) == FAIL) return RET_BUG;      

  if ( ! (sigma > 0.0) )
    { Scierror("Error: pdf('logn',x,mu,sigma), sigma should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_lognormal(x->R[i], mu, sigma);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_logi_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double a, b;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'logi' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&a) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&b) == FAIL) return RET_BUG;      

  if ( ! (b > 0.0) )
    { Scierror("Error: pdf('logi',x,a,b), b should be > 0\n"); return RET_BUG;}

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_logistic(x->R[i], a, b);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_bin_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double p, n;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'bin' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&n) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&p) == FAIL) return RET_BUG;      

  if ( ! ( n >= 1.0  &&  n == floor(n)  &&  0.0 <= p  && p <= 1.0 ) )
    { 
      Scierror("Error: pdf('bin',x,n,p), invalid parameters: n should be a positive int and p a real in [0,1]\n"); 
      return RET_BUG;
    }
 
  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_binom(x->R[i], n, p, 0);
  
  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_hyp_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double n, r, b;
  int i;
  if ( rhs != 5 ) 
    { Scierror("Error: 3 parameters are required for 'hyp' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&n) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&r) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,5,&b) == FAIL) return RET_BUG;      

  if ( ! ( n >= 1.0  &&  n == floor(n)  &&  r >= 0.0  &&  r == floor(r) 
           && b >= 0.0 && b == floor(b) && n <= r + b) )
    { 
      Scierror("Error: pdf('hyp',x,n,r,b), invalid parameters\n"); 
      return RET_BUG;
    }
 
  for ( i = 0 ; i < x->mn ; i++ )
     x->R[i] = nsp_pdf_hyper(x->R[i], r, b, n, 0);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}
   
static int int_poi_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double mu;
  int i;
  if ( rhs != 3 ) 
    { Scierror("Error: 1 parameter is required for 'poi' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&mu) == FAIL) return RET_BUG;      

  if ( ! ( mu >= 0.0 ) )
    { 
      Scierror("Error: pdf('poi',x,mu), invalid parameter: mu should be >= 0\n"); 
      return RET_BUG;
    }

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_pois(x->R[i], mu, 0);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}
  
static int int_geom_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double p;
  int i;
  if ( rhs != 3 ) 
    { Scierror("Error: 1 parameter is required for 'geom' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&p) == FAIL) return RET_BUG;      

  if ( ! ( 0 < p && p <= 1.0 ) )
    { 
      Scierror("Error: pdf('geom',x,p), invalid parameter: p should be in (0,1]\n"); 
      return RET_BUG;
    }
  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_geometric(x->R[i], p);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_nbn_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double p, r;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'nbn' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&r) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&p) == FAIL) return RET_BUG;      

  if ( !( r > 0.0  &&  0.0 <= p  &&  p <= 1.0) )
    { 
      Scierror("Error: pdf('nbn',x,r,p), invalid parameters: r should be positive and p in [0,1]\n"); 
      return RET_BUG;
    }

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_nbinom(x->R[i], r, p, 0);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_bet_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double a, b;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'bet' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&a) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&b) == FAIL) return RET_BUG;      

  if ( !(a > 0.0  &&  b > 0.0) )
    { 
      Scierror("Error: pdf('bet',x,a,b), invalid parameters: a and b should be positive\n"); 
      return RET_BUG;
    }

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_beta(x->R[i], a, b, 0);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_f_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double nu1, nu2;
  int i;
  if ( rhs != 4 ) 
    { Scierror("Error: 2 parameters are required for 'f' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&nu1) == FAIL) return RET_BUG;      

  if (GetScalarDouble(stack,4,&nu2) == FAIL) return RET_BUG;      

  if ( !(nu1 > 0.0 && nu2 > 0.0) )
    { 
      Scierror("Error: pdf('f',x,nu1,nu2), invalid parameters: nu1 and nu2 should be positive\n"); 
      return RET_BUG;
    }

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_f(x->R[i], nu1, nu2, 0);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}

static int int_t_part(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *x;
  double nu;
  int i;
  if ( rhs != 3 ) 
    { Scierror("Error: 1 parameter is required for 't' option of function %s (got %d)\n",NspFname(stack),rhs-2); return RET_BUG;}
  
  if ( (x = GetRealMatCopy(stack,2)) == NULLMAT ) return RET_BUG;

  if (GetScalarDouble(stack,3,&nu) == FAIL) return RET_BUG;      

  if ( !(nu > 0.0) )
    { 
      Scierror("Error: pdf('t',x,nu), invalid parameter: nu should be positive\n"); 
      return RET_BUG;
    }

  for ( i = 0 ; i < x->mn ; i++ )
    x->R[i] = nsp_pdf_t(x->R[i], nu, 0);

  NSP_OBJECT(x)->ret_pos  = 1;
  return 1;
}


static OpTab pdf_table[]={
  {"nor", int_nor_part},
  {"gam", int_gam_part},
  {"chi", int_chi_part},
  {"exp", int_exp_part},
  {"cau", int_cau_part},
  {"par", int_par_part},
  {"ray", int_ray_part},
  {"tray", int_tray_part},
  {"wei", int_wei_part},
  {"lap", int_lap_part},
  {"logn", int_logn_part},
  {"logi", int_logi_part},
  {"bin", int_bin_part},
  {"poi", int_poi_part},
  {"geom", int_geom_part},
  {"nbn", int_nbn_part},
  {"bet", int_bet_part},
  {"f", int_f_part},
  {"t", int_t_part},
  {"hyp", int_hyp_part},
  {(char *) 0, NULL}
};
         
static int int_nsp_pdf( Stack stack, int rhs, int opt, int lhs)
{ 
  int rep;
  if ((rep= GetStringInStruct(stack,1,pdf_table,sizeof(OpTab),1)) == -1) return RET_BUG; 
  return pdf_table[rep].fonc(stack, rhs, opt, lhs);
}


static OpTab Spmf_func[]={
  {"log1p_m", int_nsp_log1p},
  {"sinpi_m", int_nsp_sinpi},
  {"gammabr_m", int_nsp_gammabr},
  {"lngamma_m", int_nsp_lngamma},
  {"kcdf_m_m", int_nsp_kcdf},
  {"kcdflim_m", int_nsp_kcdflim},
  {"kcdfbis_m", int_nsp_kcdfbis},
  {"legendre_m_m", int_legendre},
  {"hypot_m_m", int_nsp_hypot},
  {"factor_m", int_nsp_primefactors},
  {"isprime_m", int_nsp_isprime},
  {"primes_m", int_nsp_primes},
  {"convhull_m_m", int_convhull2d},
  {"pdf", int_nsp_pdf},
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
