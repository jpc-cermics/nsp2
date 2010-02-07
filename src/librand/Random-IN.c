/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2001-2009 Bruno Pinçon Esial/Iecn
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
#include "nsp/matrix-in.h"
#include "nsp/bmatrix-in.h"

extern int C2F(dpotrf) (char *uplo, int *n, double *a, int *lda, int *info, int uplo_len);

/* external functions to be called through this interface */
#include "grand.h"
#include "basic_generators.h"

static int get_gen_from_name(char *str)
{
  int k;
  for ( k = 0 ; k < NbGenInNsp ; k++ )
    if ( strcmp(str, NspRNG[k]->name_gen) == 0 )
      return k;
  return -1;
}

static void display_gen_names()
{
  int k;
  for ( k = 0 ; k < NbGenInNsp-1 ; k++ )
    Sciprintf("%s, ",NspRNG[k]->name_gen);
  Sciprintf("%s\n",NspRNG[NbGenInNsp-1]->name_gen);
}

static int nsp_verify_probability_vector(double *p, int n)
{
  /* p is a probability vector for a discrete distribution 
   * with n events ; only the probability of the n-1 first events
   * is given, the last one being supposed to be 1- sum_k p_k.
   * The code verify that all probability are positive or null.
   * 
   */
  int k;
  double q, tol=(n-1)*DBL_EPSILON;

  q = 0.0;
  for ( k = 0 ; k < n-1 ; k++ )
    {
      if ( ! (p[k] >= 0.0) )  /* to detect also Nan */
	return FAIL;
      q += p[k];
    }

  /* probabilities must sum up to 1 (up to floating point errors) */
  if ( q >= 1.0 + tol )
    return FAIL;

  return OK;
}

static int nsp_verif_markov_initial_state(double *X0, int mnX0, int n)
{
  int i, j;
  for ( i = 0 ; i < mnX0 ; i++ )
    {
      j = (int) X0[i];
      if ( j < 1 || j > n )
	{
	  Scierror("Error: X0(%d) must be in the range [1,%d]\n", i+1, n);
	  return FAIL;
	}
    }
  return OK;
}

static int nsp_rand_discrete(double *p, double *q, double *Res, int *key, int n, int mn)
{
  int i;

  if ( nsp_guide_table_method(p, 1, q, key, n) == FAIL ) 
    return FAIL;
  
  for ( i = 0 ; i < mn ; i++ )
    Res[i] = 1.0 + (double) nsp_rand_discrete_guide(q, key, n);

  return OK;
}



static int int_markov_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *P,*X0,*Res;
  double *q=NULL;
  int *key=NULL, nn;

  if ( rhs != 4 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'markov',P,X0))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'markov' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&nn) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), nn, 1);

  if ((P = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  if ( ! ( (P->m == P->n  &&  P->m >= 1) || (P->m == 1 &&  P->n >= 2) ) )
    { 
      Scierror("Error: P must be a square matrix\n");return RET_BUG;
    }
  
  if ((X0 = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  if ( nsp_verif_markov_initial_state(X0->R, X0->mn, P->n) == FAIL ) return RET_BUG;
  
  if ((Res = nsp_matrix_create(NVOID,'r', X0->mn, nn))== NULLMAT) return RET_BUG;
  
  if ( P->m == 1  && P->n >= 2 )   /* discrete distribution */
    {
      Sciprintf("Warning: it is better to use directly option 'disc'\n");
      if ( (q =nsp_alloc_work_doubles(P->n+1)) == NULL || (key =nsp_alloc_work_int(P->n)) == NULL )
	goto err;
      if ( nsp_rand_discrete(P->R, q, Res->R, key, P->n, X0->mn*nn) == FAIL )
	{
	  Scierror("Error: incorrect probability vector\n"); goto err;
	}
    }
  else                             /* markov case */
    {
      if ( (q =nsp_alloc_work_doubles(P->n*(P->n+1))) == NULL || (key =nsp_alloc_work_int(P->n*P->n)) == NULL )
	goto err;
  
      if ( nsp_markov_setup(P->R, q, key, P->n) == FAIL )
	{
	  Scierror("Error: incorrect probability matrix\n"); goto err;
	}

      nsp_rand_markov(q, key, X0->R, Res->R, P->n, X0->mn, nn);
    }

  FREE(q); FREE(key);
  MoveObj(stack,1,(NspObject *) Res);
  return 1;

 err:
  FREE(q); FREE(key);
  nsp_matrix_destroy(Res);
  return RET_BUG;
}

static int int_mn_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *Mean,*Cov,*Res;
  int i, m, nn, info;

  if ( rhs != 4 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'mn',Mean,Cov))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'mn' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&nn) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), nn, 1);

  if ((Mean = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  if ( Mean->n != 1) { Scierror("Error: Mean must be column vector\n");return RET_BUG;}
  m = Mean->m;
  if ((Cov = GetRealMatCopy(stack,suite+1)) == NULLMAT) return RET_BUG;
  if ( Cov->m != Cov->n ) { Scierror("Error: Cov must be a square matrix\n");return RET_BUG;}
  if ( Cov->m != m ) { Scierror("Error: Mean and Cov have incompatible dimensions\n");return RET_BUG;}
  
  if ( m <= 0 ) 
    {
      Scierror("Error: Mean and Cov are of null size\n");
      return RET_BUG;
    }
  
  /* Cholesky factorisation of Cov */
  C2F(dpotrf)("L", &m, Cov->R, &m, &info, 1L);
  if ( info != 0 )
    {
      Scierror("Error: Cov not positive definite\n"); return RET_BUG;
    }
  
  if ((Res = nsp_matrix_create(NVOID, 'r', m, nn))== NULLMAT) return RET_BUG;
  
  /* generation */
  for ( i=0 ; i < nn ; i++) 
    nsp_rand_ndgauss(Mean->R, Cov->R, Res->R + m*i, m);
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_sph_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  /* X = grand(1000,"sph",3);
     param3d(X(1,:)',X(2,:)',X(3,:)',style=-5,flag=[4,4])
  */

  NspMatrix *Res;
  int i, nn, dim;

  if ( rhs != 3 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'sph',dim))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'sph' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&nn) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), nn, 1);

  if ( GetScalarInt(stack,suite,&dim) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), dim, suite);
  
  if ((Res = nsp_matrix_create(NVOID, 'r', dim, nn))== NULLMAT) return RET_BUG;
  
  /* generation */
  for ( i=0 ; i < nn ; i++) 
    nsp_rand_sphere(Res->R + dim*i, dim);
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_insph_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *Res;
  int i, nn, dim;

  if ( rhs != 3 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'insph',dim))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'insph' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&nn) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), nn, 1);

  if ( GetScalarInt(stack,suite,&dim) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), dim, suite);
  
  if ((Res = nsp_matrix_create(NVOID, 'r', dim, nn))== NULLMAT) return RET_BUG;
  
  /* generation */
  for ( i=0 ; i < nn ; i++) 
    nsp_rand_in_sphere(Res->R + dim*i, dim);
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_smplx_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  /* X = grand(1000,"simp",3);
  */

  NspMatrix *Res;
  int nn, dim;

  if ( rhs != 3 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'simp',dim))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'simp' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&nn) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), nn, 1);

  if ( GetScalarInt(stack,suite,&dim) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), dim, suite);
  
  if ((Res = nsp_matrix_create(NVOID, 'r', dim, nn))== NULLMAT) return RET_BUG;
  
  /* generation */
  nsp_rand_simplex(Res->R, dim, nn);
  
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

static int int_prm_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *prm;
  double *col_j;
  int nn,j;

  if ( rhs != 3 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'prm',vect))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'prm' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&nn) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), nn, 1);

  if ((prm = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  if ( prm->n != 1) { Scierror("Error: vect must be column vector\n");  return RET_BUG;}
  
  if ((M = nsp_matrix_create(NVOID,'r',prm->m,nn))== NULLMAT) return RET_BUG;
  for ( j = 0 ; j < M->n ; j++) 
    {
      col_j = M->R + M->m*j;
      memcpy(col_j, prm->R, sizeof(double)*M->m);
      rand_genprm(col_j, M->m);
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

static int int_perm_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M;
  int j, n, m, *col_j;

  if ( rhs != 3 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'perm',m))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'perm' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&n) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), n, 1);

  if ( GetScalarInt(stack,suite,&m) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), m, suite);
  
  if ((M = nsp_matrix_create(NVOID,'r',m,n))== NULLMAT) return RET_BUG;

  for ( j = 0, col_j = (int *) M->R ; j < n ; j++, col_j += m) 
    nsp_rand_prm(col_j, m, 1);

  M->convert = 'i';
  Mat2double(M);
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

static int int_smpl_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M;
  int *col_j, j, n, m, p, *next=NULL, *head=NULL;

  if ( rhs != 4 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'smpl',p,m))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'smpl' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&n) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), n, 1);

  if ( GetScalarInt(stack,suite,&p) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), p, suite);

  if ( GetScalarInt(stack,suite+1,&m) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), m, suite+1);

  if ( p > m )
    { 
      Scierror("Error: grand(n,'smpl',p,m), p must be inferior or equal to m\n"); 
      return RET_BUG; 
    }

  /* 
   * here we could choose between 2 algorithms: 
   *               nsp_rand_smpl  for p << m 
   * and otherwise nsp_rand_smpl_bis 
   *
   * 35 is just the cross limit on my machine...
   */
  if ( 35*p <= m )   /* use nsp_rand_smpl */
    { 
      if ((M = nsp_matrix_create(NVOID,'r',p,n))== NULLMAT) return RET_BUG;

      if ( (head = nsp_alloc_work_int(p)) == NULL || (next = nsp_alloc_work_int(p)) == NULL )
	goto err;
      
      for ( j = 0, col_j = (int *) M->R ; j < n ; j++, col_j += p) 
	nsp_rand_smpl(col_j, p, m, 1, head, next);

      FREE(head); FREE(next);
    }
  else                /* use nsp_rand_smpl_bis */
    {
      /*  be careful the array must be of length p*n + m-p, then reduced to p*n 
       *  (in fact this is not always mandatory as we work with half the space:
       *  the array needs really to be enlarged when 4(p*n+m-p) > 8 p*n ...)
      */   
      if ((M = nsp_matrix_create(NVOID,'r',p*n+m-p,1))== NULLMAT) return RET_BUG;

      for ( j = 0, col_j = (int *) M->R ; j < n ; j++, col_j += p) 
	nsp_rand_smpl_bis(col_j, p, m, 1);
      nsp_matrix_resize(M, p, n);
    }

  M->convert = 'i';
  Mat2double(M);
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  FREE(head); FREE(next); nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_mul_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  int N, *I=NULL, i, nn, ncat, *key=NULL;
  NspMatrix *P,*B=NULL;
  double *q=NULL;

  if ( rhs != 4 || suite != 3) 
    { Scierror("Error: bad calling sequence. Correct usage is: grand(n,'mul',N,P))\n"); return RET_BUG;}

  if ( ResL != 1 || ResC != 1 )
    { 
      Scierror("Error: first argument for 'mul' option must be the number of random vectors to generate\n"); 
      return RET_BUG; 
    }

  if ( GetScalarInt(stack,1,&nn) == FAIL ) return RET_BUG;      
  CheckNonNegative(NspFname(stack), nn, 1);

  if (GetScalarInt(stack,3,&N) == FAIL) return RET_BUG;      
  if ((P = GetMat(stack,4)) == NULLMAT) return RET_BUG;
  ncat = P->mn+1;

  if ( N < 0 ) 
    {
      Scierror("Error: bad parameter N (should be non negative)\n"); return RET_BUG;
    }

  if ((B = nsp_matrix_create(NVOID,'r',ncat,nn))== NULLMAT) return RET_BUG;

  I = (int *)  B->R;

  if ( N <= 5*ncat )
    {
      if ( (q =nsp_alloc_work_doubles(ncat+1)) == NULL || (key =nsp_alloc_work_int(ncat)) == NULL )
	goto err;
      
      if ( nsp_guide_table_method_bis(P->R, q, key, ncat) == FAIL )
	{
	  Scierror("Error: bad probability vector \n");
	  goto err;
	}

      for ( i=0 ; i < nn ; i++)  
	nsp_rand_multinomial_bis(q, key, I + ncat*i, ncat, N);

      FREE(q); FREE(key);
    }
  else
    {
      if (  nsp_verify_probability_vector(P->R, ncat) == FAIL )
	{
	  Scierror("Error: bad probability vector \n");
	  goto err;
	}
      for ( i=0 ; i < nn ; i++)  
	nsp_rand_multinomial(P->R, I + ncat*i, ncat, N);
    }

  B->convert = 'i';
  Mat2double(B);
  MoveObj(stack,1,(NspObject *) B);
  return 1;

 err:
  FREE(q); FREE(key);
  nsp_matrix_destroy(B);
  return RET_BUG;
}

static int int_bet_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *A, *B;
  int i, ia, ib, inca, incb;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'bet' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (A = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,A,ResL,ResC);
  
  if ( (B = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,B,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( A->mn == 1  &&  B->mn == 1 )  /* fixed parameter(s) case */
    {
      BetaStruct Bet;
      ia = 0; ib = 0;
      if ( nsp_rand_beta_init(A->R[0], B->R[0], &Bet) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= nsp_rand_beta(&Bet);
    }
  else                               /* varying parameter(s) case */
    {
      inca = A->mn == 1 ? 0 : 1;
      incb = B->mn == 1 ? 0 : 1;
      for ( i=0, ia=0, ib=0 ; i < M->mn ; i++, ia+=inca, ib+=incb) 
	{
	  if ( ! (A->R[ia] > 0.0  &&  B->R[ib] > 0.0) ) goto err;
	  nsp_rand_beta_direct(A->R[ia], B->R[ib]);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'bet',a,b) : a (=%g) or b (=%g) <= 0 \n",A->R[ia], B->R[ib]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_f_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  FStruct F;
  NspMatrix *M, *nu1, *nu2;
  int i, i1, i2, inc1, inc2;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'f' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (nu1 = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,nu1,ResL,ResC);

  if ( (nu2 = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,nu2,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( nu1->mn == 1  &&  nu2->mn == 1 )  /* fixed parameter(s) case */
    {
      i1 = 0; i2 = 0;
      if ( nsp_rand_F_init(nu1->R[0], nu2->R[0], &F) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= nsp_rand_F(&F);
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = nu1->mn == 1 ? 0 : 1;
      inc2 = nu2->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  if ( ! (nu1->R[i1] > 0.0 && nu2->R[i2] > 0.0) ) goto err;
	  M->R[i]= nsp_rand_F_direct(nu1->R[i1],nu2->R[i2]);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'f',nu1,nu2): nu1 (=%g) or nu2 (=%g) <= 0 \n",nu1->R[i1], nu2->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_gam_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *A, *B;
  GammaStruct G;
  int i;
  if ( rhs != suite +1 ) 
    { Scierror("Error: 2 parameters required for 'gam' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (A = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,A,ResL,ResC);
  
  if ( (B = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,A,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( A->mn == 1 )  /* first parameter is fixed */
    {
      i = 0;
      if ( nsp_rand_gamma_init(A->R[0], &G) == FAIL ) goto err;
      if ( B->mn == 1 )
	{
	  double coef = 1.0/B->R[0];
	  for ( i=0 ; i < M->mn ; i++) 
	    M->R[i]= coef * nsp_rand_gamma(&G);
	}
      else
	for ( i=0 ; i < M->mn ; i++) 
	  M->R[i]= nsp_rand_gamma(&G) / B->R[i];
    }
  else               /* first parameter is varying */      
    {
      if ( B->mn == 1 )
	{
	  double  coef = 1.0/B->R[0];
	  for ( i=0 ; i < M->mn ; i++) 
	    {
	      if ( ! (A->R[i] > 0.0 ) ) goto err;
	      M->R[i]= coef * nsp_rand_gamma_direct(A->R[i]);
	    }
	}
      else
	for ( i=0 ; i < M->mn ; i++) 
	  {
	    if ( ! (A->R[i] > 0.0 ) ) goto err;
	    M->R[i]= nsp_rand_gamma_direct(A->R[i]) / B->R[i];
	  }
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'gam',a,b) : a (=%g) <= 0.0 \n",A->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_nor_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *mu, *sigma;
  int i, i1, i2, inc1, inc2;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'nor' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (mu = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,mu,ResL,ResC);

  if ( (sigma = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,sigma,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( mu->mn == 1  &&  sigma->mn == 1 )      /* fixed parameter(s) case */
    {
      double m = mu->R[0], s = sigma->R[0];
      i1 = i2 = 0;
      if ( s < 0.0 ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i] = m + s*nsp_rand_nor_core(); 
    }
  else                                        /* varying parameter(s) case */
    {
      inc1 = mu->mn == 1 ? 0 : 1;
      inc2 = sigma->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  if (  sigma->R[i2] < 0.0 ) goto err;
	  M->R[i] = mu->R[i1] + sigma->R[i2] * nsp_rand_nor_core();
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'nor',mu,sigma) : sigma (=%g) is negative  \n",sigma->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_unf_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *low, *high;
  int i, i1, i2, inc1, inc2;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'unf' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (low = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,low,ResL,ResC);
  
  if ( (high = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,high,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( low->mn == 1  &&  high->mn == 1 )      /* fixed parameter(s) case */
    {
      double a = low->R[0], b = high->R[0], delta;
      i1 = i2 = 0;
      if ( ! (a <= b) ) goto err;
      delta = b - a;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= a + delta* rand_ranf();
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = low->mn == 1 ? 0 : 1;
      inc2 = high->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  if ( ! ( low->R[i1] <= high->R[i2] ) ) goto err;
	  M->R[i]=  low->R[i1] + (high->R[i2] - low->R[i1]) * rand_ranf();
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'unf',low,high) : low (=%g) not inferior or equal to high (=%g)  \n",low->R[i1], high->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_uin_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *low, *high;
  int a, b, i, i1, i2, inc1, inc2;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'uin' option (got %d)\n",rhs-suite+1); return RET_BUG;}

  if ( (low = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,low,ResL,ResC);

  if ( (high = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,high,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( low->mn == 1  &&  high->mn == 1 )      /* fixed parameter(s) case */
    {
      a = (int) low->R[0]; b = (int) high->R[0];
      if ( a > b ) goto err;  /* normally we must have also (b-a+1) <= Min RngMaxInt = 2147483561) */
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i] = rand_ignuin(a, b);
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = low->mn == 1 ? 0 : 1;
      inc2 = high->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  a = (int) low->R[i1]; b = (int) high->R[i2];
	  if ( a > b ) goto err;
	  M->R[i] = rand_ignuin(a, b);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'uin',low,high) : low (=%d) not inferior or equal to high (=%d)  \n",a,b); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_lgi_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M;
  int i;
  if ( rhs != suite -1 ) 
    { Scierror("Error: no parameter required for 'lgi' option (got %d)\n",rhs-suite+1); return RET_BUG;}

  if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
  for ( i=0 ; i < M->mn ; i++) M->R[i]= (double) rand_lgi();
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

static int int_8bits_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M;
  int i;
  if ( rhs != suite -1 ) 
    { Scierror("Error: no parameter required for '8bits' option (got %d)\n",rhs-suite+1); return RET_BUG;}

  if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
  for ( i=0 ; i < M->mn ; i++) M->R[i]= (double) (rand_lgi() & 0xff);
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

static int int_def_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M;
  int i;
  if ( rhs != suite -1 ) 
    { Scierror("Error: no parameter required for 'def' option (got %d)\n",rhs-suite+1); return RET_BUG;}

  if ((M = nsp_matrix_create(NVOID,'r',ResL,ResC))== NULLMAT) return RET_BUG;
  for ( i=0 ; i < M->mn ; i++)   M->R[i]= rand_ranf();
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}

static int int_nch_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *nu, *xnonc;
  int i, i1, i2, inc1, inc2;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'nch' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (nu = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,nu,ResL,ResC);
  
  if ( (xnonc = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,xnonc,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;
  
  if ( nu->mn == 1  &&  xnonc->mn == 1 )      /* fixed parameter(s) case */
    {
      NcChi2Struct C;
      i1 = i2 = 0;
      if ( nsp_rand_ncchi2_init(nu->R[0], xnonc->R[0], &C) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= nsp_rand_ncchi2(&C);
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = nu->mn == 1 ? 0 : 1;
      inc2 = xnonc->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  if ( ! (nu->R[i1] >= 1.0  &&  xnonc->R[i2] >= 0.0) ) goto err;
	  M->R[i] = nsp_rand_ncchi2_direct(nu->R[i1], xnonc->R[i2]);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'nch',nu,xnonc) : nu (=%g) < 1 or xnonc (=%g) < 0 \n",nu->R[i1], xnonc->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_nf_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *nu1, *nu2, *xnonc;
  int i, i1, i2, i3, inc1, inc2, inc3;
  if ( rhs != suite + 2) 
    { Scierror("Error: 3 parameters required for 'nf' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (nu1 = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,nu1,ResL,ResC);
  
  if ( (nu2 = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,nu2,ResL,ResC);
  
  if ( (xnonc = GetRealMat(stack,suite+2)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+2,xnonc,ResL,ResC);
  
  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;
      
  if ( nu1->mn == 1  && nu2->mn == 1  && xnonc->mn == 1 )      /* fixed parameters case */
    {
      NcFStruct E;
      i1 = i2 = i3 = 0;
      if ( nsp_rand_ncF_init(nu1->R[0], nu2->R[0], xnonc->R[0], &E) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= nsp_rand_ncF(&E);
    }
  else                                                         /* varying parameter(s) case */
    {
      inc1 = nu1->mn == 1 ? 0 : 1;
      inc2 = nu2->mn == 1 ? 0 : 1;
      inc3 = xnonc->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0, i3=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2, i3+=inc3) 
	{
	  if ( ! (nu1->R[i1] >= 1.0  &&  nu2->R[i2] > 0.0 && xnonc->R[i3] >= 0.0) ) goto err;
	  M->R[i] = nsp_rand_ncF_direct(nu1->R[i1], nu2->R[i2], xnonc->R[i3]);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'nf',nu1,nu2,xnonc) : nu1 (=%g) < 1 or nu2 (%g) <= 0 or xnonc (=%g) < 0 \n",
	   nu1->R[i1], nu2->R[i2], xnonc->R[i3]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_chi_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *nu;
  int i=0;
  if ( rhs != suite) 
    { Scierror("Error: 1 parameter required for 'chi' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (nu = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,nu,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( nu->mn == 1 )  /* fixed parameter case */
    {
      Chi2Struct C;
      if ( nsp_rand_chi2_init(nu->R[0], &C) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= nsp_rand_chi2(&C);
    }
  else               /* varying parameter case */      
    for ( i=0 ; i < M->mn ; i++) 
      {
	if ( ! (nu->R[i] > 0) ) goto err;
	M->R[i]= nsp_rand_chi2_direct(nu->R[i]);
      }
  
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'chi',nu) : nu (=%g) <= 0.0 \n",nu->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_t_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *nu;
  int i=0;
  if ( rhs != suite) 
    { Scierror("Error: 1 parameter required for 't' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (nu = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,nu,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( nu->mn == 1 )  /* fixed parameter case */
    {
      if ( ! (nu->R[0] > 0) ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= nsp_rand_t(nu->R[0]);
    }
  else               /* varying parameter case */      
    for ( i=0 ; i < M->mn ; i++) 
      {
	if ( ! (nu->R[i] > 0) ) goto err;
	M->R[i]= nsp_rand_t(nu->R[i]);
      }
  
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'t',nu) : nu (=%g) <= 0.0 \n",nu->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_poi_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *mu;
  int i;
  if ( rhs != suite) 
    { Scierror("Error: 1 parameter required for 'poi' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (mu = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,mu,ResL,ResC);
  
  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;
  
  if ( mu->mn == 1 )  /* fixed parameter case */
    {
      PoissonStruct P;
      i = 0;
      if ( nsp_rand_poisson_init(mu->R[0], &P) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= nsp_rand_poisson(&P);
    }
  else               /* varying parameter case */      
    for ( i=0 ; i < M->mn ; i++) 
      {
	if ( mu->R[i] < 0.0 ) goto err;
	M->R[i]= nsp_rand_poisson_direct(mu->R[i]);
      }
  
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'poi',mu) : mu (=%g) < 0.0 \n",mu->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_geom_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *p;
  int i=0;
  if ( rhs != suite ) 
    { Scierror("Error: 1 parameter required for 'geom' option (got %d)\n",rhs-suite+1); return RET_BUG;}

  if ( (p = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,p,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( p->mn == 1 )  /* fixed parameter case */
    {
      GeomStruct G;
      if ( nsp_rand_geom_init(p->R[0], &G) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= (double) nsp_rand_geom(&G);
    }
  else               /* varying parameter case */      
    for ( i=0 ; i < M->mn ; i++) 
      {
	if ( ! (1.3e-307 <= p->R[i]  &&  p->R[i] <= 1) ) goto err;
	M->R[i]= (double) nsp_rand_geom_direct(p->R[i]);
      }
  
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'geom',p) : p (=%g) not in [1.3e-307, 1] \n",p->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_disc_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *Res, *p;
  double *q=NULL;
  int n, *key=NULL;
  if ( rhs != suite ) 
    { Scierror("Error: 1 parameter required for 'disc' option (got %d)\n",rhs-suite+1); return RET_BUG;}

  if ( (p = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckVector(NspFname(stack),suite,p);
  n = p->mn;

  if ( (Res = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( (q = nsp_alloc_work_doubles(n+1)) == NULL  ||  (key = nsp_alloc_work_int(n+1)) == NULL )
    goto err;

  if ( nsp_rand_discrete(p->R, q, Res->R, key, n, Res->mn) == FAIL )
    {
      Scierror("Error: grand(..'disc',p), bad probability vector p\n");
      goto err;
    }

  FREE(q); FREE(key);
  MoveObj(stack,1,(NspObject *) Res);
  return 1;

 err:
  FREE(q); FREE(key);
  nsp_matrix_destroy(Res);
  return RET_BUG;
}

static int int_exp_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *A;
  int i;
  if ( rhs != suite ) 
    { Scierror("Error: 1 parameter required for 'exp' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (A = GetRealMat(stack,suite)) == NULLMAT ) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,A,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT ) return RET_BUG;

  if ( A->mn == 1 )  /* fixed parameter case */
    {
      i = 0;
      if ( ! (A->R[0] >= 0.0) ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= nsp_rand_exp(A->R[0]);
    }
  else               /* varying parameter case */      
    for ( i=0 ; i < M->mn ; i++) 
      {
	if ( ! (A->R[i] >= 0.0) ) goto err;
	M->R[i]= nsp_rand_exp(A->R[i]);
      }

  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'exp',tau) : tau (=%g) < 0.0 \n",A->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_nbn_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *r, *p;
  int i, i1, i2, inc1, inc2;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'nbn' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (r = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,r,ResL,ResC);

  if ( (p = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,p,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( r->mn == 1  &&  p->mn == 1 )      /* fixed parameter(s) case */
    {
      NbnStruct N;
      i1 = i2 = 0;
      if ( nsp_rand_nbn_init( r->R[0], p->R[0], &N) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) M->R[i]= nsp_rand_nbn(&N);
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = r->mn == 1 ? 0 : 1;
      inc2 = p->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  if ( ! ( r->R[i1] > 0.0 && 0 < p->R[i2] && p->R[i2] <= 1.0) ) goto err;
	  M->R[i]= nsp_rand_nbn_direct(r->R[i1], p->R[i2]);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'nbn',r,p) : r (=%d) <= 0 or p (=%g) not in (0,1] \n", r->R[i1], p->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_bin_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *N, *p;
  int i, i1, i2, inc1, inc2;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'bin' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (N = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,N,ResL,ResC);
  
  if ( (p = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,p,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( N->mn == 1  &&  p->mn == 1 )      /* fixed parameter(s) case */
    {
      BinomialStruct B;
      i1 = i2 = 0;
      if ( nsp_rand_binomial_init((int) N->R[0], p->R[0], &B) == FAIL ) goto err;
      for ( i=0 ; i < M->mn ; i++) M->R[i] = (double) nsp_rand_binomial(&B);
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = N->mn == 1 ? 0 : 1;
      inc2 = p->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  if ( ! ((int) N->R[i1] >= 1  &&  0.0 <= p->R[i2] && p->R[i2] <= 1.0) ) goto err;
	  M->R[i] = nsp_rand_binomial_direct((int) N->R[i1], p->R[i2]);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'bin',n,p) : n (=%d) < 1 or p (=%g) not in [0,1] \n",(int) N->R[i1], p->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}  

static int int_cauchy_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *sigma;
  int i=0;
  if ( rhs != suite) 
    { Scierror("Error: 1 parameter required for 'cau' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (sigma = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,sigma,ResL,ResC);
  
  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;
  
  if ( sigma->mn == 1 )  /* fixed parameter case */
    {
      if ( ! (sigma->R[0] > 0.0) ) goto err; 
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= nsp_rand_cauchy(sigma->R[0]);
    }
  else                   /* varying parameter case */      
    for ( i=0 ; i < M->mn ; i++) 
      {
	if ( ! (sigma->R[i] > 0.0) ) goto err;
	M->R[i]= nsp_rand_cauchy(sigma->R[i]);
      }
  
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'cau',sigma) : sigma (=%g) not positive \n",sigma->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_pareto_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *a, *b;
  int i, i1, i2, inc1, inc2;

  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'par' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (a = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,a,ResL,ResC);

  if ( (b = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,b,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( a->mn == 1  &&  b->mn == 1 )      /* fixed parameter(s) case */
    {
      double A = a->R[0], B = b->R[0];
      i1 = i2 = 0;
      if ( ! (A > 0.0 && B > 0.0) ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i] = nsp_rand_pareto(A, B); 
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = a->mn == 1 ? 0 : 1;
      inc2 = b->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  double A = a->R[i1], B = b->R[i2];
	  if ( ! ( A > 0.0 && B > 0.0) ) goto err;
	  M->R[i] = nsp_rand_pareto(A, B);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'par',a,b):  a (=%g) or b (=%g) not positive \n", a->R[i1], b->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_logistic_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *a, *b;
  int i, i1, i2, inc1, inc2;

  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'logi' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (a = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,a,ResL,ResC);

  if ( (b = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,b,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( a->mn == 1  &&  b->mn == 1 )      /* fixed parameter(s) case */
    {
      double A = a->R[0], B = b->R[0];
      i1 = i2 = 0;
      if ( ! (B > 0.0) ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i] = nsp_rand_logistic(A, B); 
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = a->mn == 1 ? 0 : 1;
      inc2 = b->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  double A = a->R[i1], B = b->R[i2];
	  if ( ! (B > 0.0) ) goto err;
	  M->R[i] = nsp_rand_logistic(A, B);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'logi',a,b):  b (=%g) not positive \n", b->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_rayleigh_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *sigma;
  int i=0;
  if ( rhs != suite) 
    { Scierror("Error: 1 parameter required for 'ray' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (sigma = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,sigma,ResL,ResC);
  
  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;
  
  if ( sigma->mn == 1 )  /* fixed parameter case */
    {
      if ( ! (sigma->R[0] > 0.0) ) goto err; 
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= nsp_rand_rayleigh(sigma->R[0]);
    }
  else                   /* varying parameter case */      
    for ( i=0 ; i < M->mn ; i++) 
      {
	if ( ! (sigma->R[i] > 0.0) ) goto err;
	M->R[i]= nsp_rand_rayleigh(sigma->R[i]);
      }
  
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'ray',sigma) : sigma (=%g) not positive \n",sigma->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_tailrayleigh_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *a, *sigma;
  int i, i1, i2, inc1, inc2;

  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'tray' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (sigma = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,sigma,ResL,ResC);

  if ( (a = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,a,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( a->mn == 1  &&  sigma->mn == 1 )      /* fixed parameter(s) case */
    {
      double A = a->R[0], s = sigma->R[0];
      i1 = i2 = 0;
      if ( ! ( A >= 0.0 && s > 0.0) ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i] = nsp_rand_tailrayleigh(s, A); 
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = a->mn == 1 ? 0 : 1;
      inc2 = sigma->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  double A = a->R[i1], s = sigma->R[i2];
	  if ( ! (A >= 0.0 && s > 0.0) ) goto err;
	  M->R[i] = nsp_rand_tailrayleigh(s, A);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'tray',a,sigma): a (=%g) negative or sigma (=%g) not positive \n", a->R[i1], sigma->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_weibull_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *a, *b;
  int i, i1, i2, inc1, inc2;

  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'wei' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (a = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,a,ResL,ResC);

  if ( (b = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,b,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( a->mn == 1  &&  b->mn == 1 )      /* fixed parameter(s) case */
    {
      double A = a->R[0], B = b->R[0];
      i1 = i2 = 0;
      if ( ! (A > 0.0 && B > 0.0) ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i] = nsp_rand_weibull(A, B); 
    }
  else                                   /* varying parameter(s) case */
    {
      inc1 = a->mn == 1 ? 0 : 1;
      inc2 = b->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  double A = a->R[i1], B = b->R[i2];
	  if ( ! ( A > 0.0 && B > 0.0) ) goto err;
	  M->R[i] = nsp_rand_weibull(A, B);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'wei',a,b): a (=%g) or b (=%g) not positive \n", a->R[i1], b->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_laplace_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *a;
  int i=0;
  if ( rhs != suite) 
    { Scierror("Error: 1 parameter required for 'lap' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (a = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,a,ResL,ResC);
  
  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;
  
  if ( a->mn == 1 )  /* fixed parameter case */
    {
      if ( ! (a->R[0] > 0.0) ) goto err; 
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i]= nsp_rand_laplace(a->R[0]);
    }
  else                   /* varying parameter case */      
    for ( i=0 ; i < M->mn ; i++) 
      {
	if ( ! (a->R[i] > 0.0) ) goto err;
	M->R[i]= nsp_rand_laplace(a->R[i]);
      }
  
  MoveObj(stack,1,(NspObject *) M);
  return 1;

 err:
  Scierror("Error: grand(..'lap',a) : a (=%g) not positive \n",a->R[i]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

static int int_lognormal_part(Stack stack, int rhs, int opt, int lhs, int suite, int ResL, int ResC)
{
  NspMatrix *M, *mu, *sigma;
  int i, i1, i2, inc1, inc2;
  if ( rhs != suite + 1) 
    { Scierror("Error: 2 parameters required for 'logn' option (got %d)\n",rhs-suite+1); return RET_BUG;}
  
  if ( (mu = GetRealMat(stack,suite)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite,mu,ResL,ResC);

  if ( (sigma = GetRealMat(stack,suite+1)) == NULLMAT) return RET_BUG;
  CheckScalarOrDims(NspFname(stack),suite+1,sigma,ResL,ResC);

  if ( (M = nsp_matrix_create(NVOID,'r',ResL,ResC)) == NULLMAT) return RET_BUG;

  if ( mu->mn == 1  &&  sigma->mn == 1 )      /* fixed parameter(s) case */
    {
      double m = mu->R[0], s = sigma->R[0];
      i1 = i2 = 0;
      if ( s < 0.0 ) goto err;
      for ( i=0 ; i < M->mn ; i++) 
	M->R[i] = nsp_rand_lognormal(m, s); 
    }
  else                                        /* varying parameter(s) case */
    {
      inc1 = mu->mn == 1 ? 0 : 1;
      inc2 = sigma->mn == 1 ? 0 : 1;
      for ( i=0, i1=0, i2=0 ; i < M->mn ; i++, i1+=inc1, i2+=inc2) 
	{
	  if (  sigma->R[i2] < 0.0 ) goto err;
	  M->R[i] = nsp_rand_lognormal(mu->R[i1],sigma->R[i2]);
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  return 1;
  
 err:
  Scierror("Error: grand(..'logn',mu,sigma) : sigma (=%g) is negative  \n",sigma->R[i2]); 
  nsp_matrix_destroy(M);
  return RET_BUG;
}

/*
 *    interface for grand(string,....) which correspond to
 *    various manipulations onto the bases generators
 *    (change of base generator, change state of the current
 *    base generator, set state of the current base generator,...)
 */
static int int_nsp_grands( Stack stack, int rhs, int opt, int lhs)
{ 
  char *str;
  NspMatrix *M=NULL;

  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;

  if (strcmp("getgen",str)==0) 
    {
      int current_gen = nsp_get_current_gen();
      if ( rhs != 1) 
	{
	  Scierror("Error: rhs should be 1 for 'getgen' option\n");
	  return RET_BUG;
	}
      if ( nsp_move_string(stack,1,NspRNG[current_gen]->name_gen,-1)== FAIL) return RET_BUG;
      return 1;
    }

  else if (strcmp("setgen",str)==0) 
    {
      char *str1; int new_current_gen;
      if ( rhs != 2) 
	{
	  Scierror("Error: rhs should be 2 for 'setgen' option\n");
	  return RET_BUG;
	}
      if ((str1 = GetString(stack,2)) == (char*)0) return RET_BUG;
      
      if ( (new_current_gen =get_gen_from_name(str1)) == -1 )
	{
	  Scierror("Error: unknown generator !\n");
	  Sciprintf("       choose among :");
	  display_gen_names();
	  return RET_BUG;
	}
      nsp_set_current_gen(new_current_gen);
      return 0;
    }

  else if ( strcmp(str,"getsd")==0) 
    {
      int current_gen = nsp_get_current_gen();
      if ( rhs != 1) 
	{
	  Scierror("%s: only one argument expected for grand('getsd')\n",NspFname(stack));
	  return RET_BUG;
	}
      
      if ( (M =nsp_matrix_create(NVOID,'r',NspRNG[current_gen]->dim_state,1)) == NULLMAT) 
	return RET_BUG;
      NspRNG[current_gen]->get_state(M->R);
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
      
  else if ( strcmp(str,"setsd")==0 ) 
    {
      int current_gen = nsp_get_current_gen();
      if ( rhs != 2 ) 
	{
	  Scierror("Error: rhs should be 2 for 'setsd' option\n");
	  return RET_BUG;
	}
      
      if ((M = GetRealMat(stack,2)) == NULLMAT) return RET_BUG;
      if ( M->mn == 1 ) 
	{
	  if ( NspRNG[current_gen]->set_state_simple(M->R[0]) == FAIL ) return RET_BUG;
	}
      else
	{
	  CheckLength(NspFname(stack),2,M, NspRNG[current_gen]->dim_state);
	  if ( NspRNG[current_gen]->set_state(M->R) == FAIL ) return RET_BUG; 
	}
      return 0;
    }
  
  else if (strcmp("phr2sd",str) == 0) 
    {
      char *str1;
      int i1,i2;
      if ( rhs != 2) 
	{
	  Scierror("Error: rhs should be 2 for 'phr2sd' option\n");
	  return RET_BUG;
	}
      if ((str1 = GetString(stack,2)) == (char*)0) return RET_BUG;
      if ((M = nsp_matrix_create(NVOID,'r',1,2))== NULLMAT) return RET_BUG;
      rand_phrtsd(str1,&i1,&i2);
      M->R[0]=i1; 	  M->R[1]=i2; 
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }
  
  /* from now all the next options are only for the clcg4 generator */
  
  else if ( nsp_get_current_gen() == CLCG4 )
    {
      if ( strcmp(str,"setall")==0 ) 
	{
	  if ( rhs != 2 ) 
	    {
	      Scierror("Error: rhs should be 2 for 'setall'  option\n");
	      return RET_BUG;
	    }
	  if ( (M =GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
	  CheckLength(NspFname(stack),2,M, 4);
	  if ( set_initial_seed_clcg4(M->R) == FAIL ) return RET_BUG;
	  return 0;
	}
      
      else if (strcmp("initgn",str)==0) 
	{
	  int i1;
	  SeedType Where;
	  if ( rhs != 2)  
	    {
	      Scierror("Error: rhs should be 2 for 'initgn' option\n");
	      return RET_BUG;
	    }
	  if (GetScalarInt(stack,2,&i1) == FAIL) return RET_BUG;
	  if ( i1 != 0 && i1 != -1 && i1 != 1)
	    {
	      Scierror("Error: for initgn option argument must be -1,0 or 1\n");
	      return RET_BUG;
	    }
	  Where = (SeedType) (i1 + 1);
	  init_generator_clcg4(Where);	  
	  return 0;
	}
      
      else if (strcmp("setcgn",str)==0) 
	{
	  int i1;
	  if ( rhs != 2) 
	    {
	      Scierror("Error: rhs should be 2 for 'setcgn' option\n");
	      return RET_BUG;
	    }
	  if (GetScalarInt(stack,2,&i1) == FAIL) return RET_BUG;
	  if ( set_current_clcg4(i1) == FAIL ) return RET_BUG;
	  return 0;
	}
      else if (strcmp("advnst",str)==0) 
	{
	  int i1;
	  if ( rhs != 2) 
	    {
	      Scierror("Error: rhs should be 2 for 'advnst' option\n");
	      return RET_BUG;
	    }
	  if (GetScalarInt(stack,2,&i1) == FAIL) return RET_BUG;
	  if ( i1 < 1 )
	    {
	      Scierror("Error: parameter K must be > 0 for 'advnst' option\n");
	      return RET_BUG;
	    }
	  advance_state_clcg4(i1);
	  return 0;
	}
      
      else if (strcmp("getcgn",str)==0) 
	{
	  if ( rhs != 1) 
	    {
	      Scierror("Error: rhs should be 1 for 'getcgn' option\n");
	      return RET_BUG;
	    }
	  if ( nsp_move_double(stack,1,(double) get_current_clcg4())== FAIL) return RET_BUG;
	  return 1;
	}
      else 
	{
	  Scierror("Error: %s Wrong first argument %s\n",NspFname(stack),str);
	  return RET_BUG;
	}      
    }
  else 
    {
      Scierror("Error: %s Wrong first argument %s\n",NspFname(stack),str);
      return RET_BUG;
    }
}
      
/*
 *    interface for grand(M,....) or grand(n,...) or grand(m,n,...)
 *    which correspond to generate random numbers
 */

static int int_nsp_grandm( Stack stack, int rhs, int opt, int lhs)
{ 
  char *rand_dist;
  NspMatrix *M=NULL;
  int ResL,ResC,suite;

  if ( rhs >= 2  &&  IsMatObj(stack,2) ) 
    {
      if (GetScalarInt(stack,1,&ResL) == FAIL) return RET_BUG;
      CheckNonNegative(NspFname(stack),ResL,1);
      if (GetScalarInt(stack,2,&ResC) == FAIL) return RET_BUG;
      CheckNonNegative(NspFname(stack),ResC,2);

      if (rhs == 2 )    /* grand(m,n) equivalent to grand(m,n,"def") */ 
	return int_def_part(stack, rhs, opt, lhs, 3, ResL, ResC);

      if ((rand_dist = GetString(stack,3)) == (char*)0) return RET_BUG;
      suite=4;
    }
  else /* if ( rhs >= 1 ) */
    {
      if ((M = GetMat(stack,1)) == NULLMAT) return RET_BUG;
      ResL=M->m; ResC=M->n;
      if (rhs == 1 )    /* grand(Mat) equivalent to grand(Mat,"def") */ 
	return int_def_part(stack, rhs, opt, lhs, 2, ResL, ResC);

      if ((rand_dist = GetString(stack,2)) == (char*)0) return RET_BUG;
      suite=3;
    }
  
  if ( strcmp(rand_dist,"bet")==0 ) 
    return int_bet_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"f")==0) 
    return int_f_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"mul")==0) 
    return int_mul_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"gam")==0) 
    return int_gam_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"nor")==0) 
    return int_nor_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"unf")==0) 
    return int_unf_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"uin")==0) 
    return int_uin_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"lgi")==0) 
    return int_lgi_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"8bits")==0) 
    return int_8bits_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"prm")==0)
    return int_prm_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"perm")==0)
    return int_perm_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"smpl")==0)
    return int_smpl_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"nbn")==0) 
    return int_nbn_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"bin")==0) 
    return int_bin_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"mn")==0) 
    return int_mn_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"markov")==0) 
    return int_markov_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"def")==0) 
    return int_def_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"nch")==0) 
    return int_nch_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"nf")==0) 
    return int_nf_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"chi")==0)
    return int_chi_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"poi")==0) 
    return int_poi_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"geom")==0)
    return int_geom_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"exp")==0)
    return int_exp_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"disc")==0)
    return int_disc_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"sph")==0)
    return int_sph_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"insph")==0)
    return int_insph_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"simp")==0)
    return int_smplx_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"cau")==0)
    return int_cauchy_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"par")==0)
    return int_pareto_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"ray")==0)
    return int_rayleigh_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"tray")==0)
    return int_tailrayleigh_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"wei")==0)
    return int_weibull_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"lap")==0)
    return int_laplace_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"logn")==0)
    return int_lognormal_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"logi")==0)
    return int_logistic_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else if ( strcmp(rand_dist,"t")==0)
    return int_t_part(stack, rhs, opt, lhs, suite, ResL, ResC);

  else 
    {
      Scierror("Error: %s unknown or unsupported random distribution %s\n",NspFname(stack),rand_dist);
      return RET_BUG;
    }      
}

static int int_nsp_randn(Stack stack, int rhs, int opt, int lhs)
{
  int m, n, k;
  NspMatrix *A;
  CheckRhs (0, 2);
  CheckLhs (1, 1);

  switch (rhs)
    {
    case 0:
      m = 1; n = 1;
      break;
    case 1:
      if (! IsMatObj (stack, 1) )
	{
	  Scierror("Error: %s when used with one arg this one must be a matrix\n",NspFname(stack));
	  return RET_BUG;
	}      
      m = nsp_object_get_size (NthObj (1), 1);
      n = nsp_object_get_size (NthObj (1), 2);
      break;
    case 2:
      if (GetScalarInt(stack,1,&m) == FAIL) return RET_BUG;
      CheckNonNegative(NspFname(stack),m,1);
      if (GetScalarInt(stack,2,&n) == FAIL) return RET_BUG;
      CheckNonNegative(NspFname(stack),n,2);
    }

  if ( (A  =nsp_matrix_create(NVOID,'r',m, n)) == NULLMAT )
    return RET_BUG;

  for ( k = 0 ; k < A->mn ; k++ )
    A->R[k] = nsp_rand_nor_core();

  MoveObj(stack,1,(NspObject *) A);
  return 1;
}
  

static int int_nsp_rand_discrete(Stack stack, int rhs, int opt, int lhs)
{ 
  NspMatrix *M=NULLMAT, *p=NULLMAT, *q=NULLMAT;
  NspBMatrix *k=NULLBMAT;
  NspSMatrix *S=NULLSMAT;
  NspList *L;
  Cell *C;
  char *meth=NULL, *default_meth="guide";
  nsp_option opts[] ={{"meth",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckRhs (2, 3);
  CheckLhs (1, 1);

  if ( IsSMatObj(stack,1) )  /* rand_discrete("setup", p, meth=) */
    {
      char *str;
      if ( (str = GetString(stack,1)) == NULL ) 
	return RET_BUG;
      if ( strcmp(str,"setup")==0) 
	{
	  if ( rhs-opt != 2) 
	    {
	      Scierror("%s: two non optional arguments expected\n",NspFname(stack));
	      return RET_BUG;
	    }
	 
	  if ( (p = GetRealMat(stack,2)) == NULLMAT )  
	    return RET_BUG;
	  CheckVector(NspFname(stack), 2, p);
	  
	  if ( get_optional_args(stack, rhs, opt, opts, &meth) == FAIL )
	    return RET_BUG;

	  if ( meth == NULL )
	    meth = default_meth;
	  else
	    if ( strcmp(meth,default_meth)!=0 && strcmp(meth,"alias")!=0 )
	      {
		Scierror("%s: bad optional argument meth (must be alias or guide)\n",NspFname(stack));
		return RET_BUG;
	      }

	  if ( (q  = nsp_matrix_create("lel",'r',p->mn+1,1)) == NULLMAT )
	    return RET_BUG;

	  if ( (k  = nsp_bmatrix_create("lel",p->mn,1)) == NULLBMAT )
	    {
	      nsp_matrix_destroy(q);
	      return RET_BUG;
	    }

	  if ( strcmp(meth,default_meth) == 0 )
	    {
	      if ( nsp_guide_table_method(p->R, 1, q->R, (int *) k->B, p->mn) == FAIL )
		goto err1;
	      if ( (S=nsp_smatrix_create("lel", 1, 1,"rd_guide", 1)) == NULLSMAT )
		goto err2;
	    }
	  else
	    {
	      if ( nsp_alias_method(p->R, q->R, (int *) k->B, p->mn) == FAIL )
		goto err1;
	      if ( (S=nsp_smatrix_create("lel", 1, 1,"rd_alias", 1)) == NULLSMAT )
		goto err2;
	    }

	  /* Put the objects into a list. The first element (a string) lets *
           * to decide between the guide code or the alias code.            */
	  if ((L =nsp_list_create(NVOID))==NULLLIST) 
	    goto err2;
	  
	  if ( nsp_list_end_insert(L, (NspObject *) S) == FAIL )
	    goto err3;
	  if ( nsp_list_end_insert(L, (NspObject *) q) == FAIL )
	    goto err3;
	  if ( nsp_list_end_insert(L, (NspObject *) k) == FAIL )
	    goto err3;

	  MoveObj(stack,1,(NspObject *) L);
	  return 1;
	}
      else
	{
	  Scierror("%s: uncorrect first argument\n",NspFname(stack));
	  return RET_BUG;
	}
    }
  else
    {
      int suite, i, m, n;
      if ( rhs == 2 ) /* rand_discrete(Mat, L) */
	{      
	  if ((M = GetMat(stack,1)) == NULLMAT) return RET_BUG;
	  m=M->m; n=M->n;
	  suite = 2;
	}
      else            /* rand_discrete(m, n, L) */
	{
	  if ( GetScalarInt(stack, 1, &m) == FAIL ) return RET_BUG;      
	  CheckNonNegative(NspFname(stack), m, 1);
	  if ( GetScalarInt(stack, 2, &n) == FAIL ) return RET_BUG;
	  CheckNonNegative(NspFname(stack), n, 2);
	  suite = 3;
	}
      
      if ( (L = GetList(stack, suite)) == NULLLIST ) return RET_BUG;

      if ( nsp_list_length(L) != 3 )
	{
	  Scierror("%s: the list must have 3 elements\n",NspFname(stack));
	  return RET_BUG;
	}

      C = L->first;
      S = (NspSMatrix *) C->O;
      C = C->next;
      q = (NspMatrix *) C->O;
      C = C->next;
      k = (NspBMatrix *) C->O;
	  
      if ( (M  = nsp_matrix_create(NVOID,'r', m, n)) == NULLMAT )
	return RET_BUG;

      if ( strcmp(S->S[0],"rd_guide") == 0 )
	for ( i = 0 ; i < M->mn ; i++ )
	  M->R[i] = 1.0 + (double) nsp_rand_discrete_guide(q->R, (int *)k->B, k->mn);
      else if ( strcmp(S->S[0],"rd_alias") == 0 )
	for ( i = 0 ; i < M->mn ; i++ )
	  M->R[i] = 1.0 + (double) nsp_rand_discrete_alias(q->R, (int *)k->B, k->mn);
      else
	{
	  Scierror("%s: first list element must be 'rd_guide' or 'rd_alias'\n",NspFname(stack));
	  return RET_BUG;
	}

      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

 err1:
  Scierror("%s: uncorrect probability vector \n",NspFname(stack));
 err2:
  nsp_matrix_destroy(q); nsp_bmatrix_destroy(k); nsp_smatrix_destroy(S);
  return RET_BUG;
 err3:
  nsp_matrix_destroy(q); nsp_bmatrix_destroy(k); nsp_smatrix_destroy(S);
  nsp_list_destroy_bis(L);
  return RET_BUG;
}


static OpTab Random_func[]={
  {"grand_s", int_nsp_grands},
  {"grand_m", int_nsp_grandm},
  /*     {"rand", int_nsp_rand}, */
  {"randn", int_nsp_randn},
  {"rand_discrete", int_nsp_rand_discrete},
  {(char *) 0, NULL}
};

int Random_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Random_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void Random_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Random_func[i].name;
  *f = Random_func[i].fonc;
}
