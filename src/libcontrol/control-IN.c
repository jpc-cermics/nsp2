/* Nsp
 * Copyright (C) 2016-2016 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/nsp.h>
#include <nsp/object.h> 
#include <nsp/matrix.h>
#include <nsp/smatrix.h>
#include <nsp/pmatrix.h>
#include <nsp/blas.h> 
#include <nsp/nsp_lapack.h> 
#include <nsp/interf.h>
#include <nsp/matint.h>

#include "ctrlpack.h"

/*
 * [Ab [,X [,bs]]]=bdiag(A [,rMax]) 
 */

int int_bdiag(Stack stack, int rhs, int opt, int lhs)
{
  int k,fail, is_finite = TRUE;
  double rMax, epshr= 1.e-8;
  NspMatrix *A,*er=NULL,*ei=NULL,*bs=NULL,*xr=NULL,*scale=NULL;
  CheckRhs(1,2);
  CheckLhs(1,3);

  if ((A = GetMatCopy (stack, 1)) == NULLMAT)   return RET_BUG;
  CheckSquare(NspFname(stack),1,A);
  
  if ( A->mn == 0)
    {
      MoveObj(stack,1,NSP_OBJECT(A));
      if ( lhs >= 2 )
	{
	  if ((xr = nsp_matrix_create (NVOID, A->rc_type, A->m , A->n)) == NULLMAT)
	    return RET_BUG;
	  MoveObj(stack,2,NSP_OBJECT(xr));
	}
      if ( lhs >= 3 )
	{
	  if ((bs = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT)
	    return RET_BUG;
	  MoveObj(stack,2,NSP_OBJECT(bs));
	}
      return Max(lhs,1);
    }

  if ( A->rc_type == 'r') 
    {
      for ( k = 0 ; k < A->mn ; k++ )
	is_finite = finite(A->R[k]) && is_finite ;
    }
  else 
    {
      for ( k = 0 ; k < A->mn ; k++ )
	is_finite = (finite(A->C[k].r) & finite(A->C[k].i)) && is_finite;
    }
  if ( is_finite == FALSE )
    {
      Scierror("Error: matrix contains non finite elements\n");
      return RET_BUG;
    }
  
  if ( rhs == 2)
    {
      if (GetScalarDouble (stack, 2, &rMax) == FAIL)
	return RET_BUG;
    }
  else
    {
      rMax = nsp_matrix_norm(A,'1');
    }
  
  if ((er = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT) goto err;
  if ((ei = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT) goto err;
  if ((bs = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT) goto err;
  if ((xr = nsp_matrix_create (NVOID, A->rc_type, A->m , A->n)) == NULLMAT) goto err;
  if ((scale = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT) goto err;

  if ( A->rc_type  == 'r' )
    {
      int job=0;
      /*     subroutine bdiag(lda,n,a,epsshr,rMax,er,ei,bs,x,xi,scale,job,fail) */
      nsp_ctrlpack_bdiag(&A->m, &A->n, A->R, &epshr, &rMax, er->R, ei->R,
			 bs->I, xr->R, NULL, scale->R, &job, &fail);
    }
  else
    {
      int job=0;
      nsp_complex2double(A->R,A->mn);
      nsp_ctrlpack_wbdiag(&A->m, &A->n, A->R, A->R+A->mn, &rMax, er->R, ei->R,
			  bs->I, xr->R, xr->R+xr->mn, NULL, NULL , scale->R, &job, &fail);
      nsp_double2complex(A->R,A->mn);
      nsp_double2complex(xr->R,xr->mn);
    }
  
  if (fail)
    {
      Scierror("Error: bdiag failed \n");
      return RET_BUG;
    }

  MoveObj(stack,1,NSP_OBJECT(A));
  if ( lhs >= 1)
    {
      MoveObj(stack,2,NSP_OBJECT(xr));
    }
  if ( lhs >= 2)
    {
      int i=0;
      NspMatrix *Bs;
      /* the blocks size */
      int nbloc = 0;
      for ( k = 0 ;  k < bs->mn ; k++ )  if ( bs->I[k] >= 0)  ++nbloc;
      if ((Bs = nsp_matrix_create (NVOID, 'r' , 1, nbloc )) == NULLMAT)
	return RET_BUG;
      for ( k = 0 ; k < bs->mn  ; k++)
	if ( bs->I[k] >= 0)
	  {
	    Bs->R[i]= bs->I[k];i++;
	  }
      MoveObj(stack,3,NSP_OBJECT(Bs));
    }
  nsp_matrix_destroy (er);
  nsp_matrix_destroy (ei);
  nsp_matrix_destroy (bs);
  nsp_matrix_destroy (scale);
  return Max(lhs,1);
 err:
  if (er != NULL)  nsp_matrix_destroy (er);
  if (ei != NULL)  nsp_matrix_destroy (ei);
  if (bs != NULL)  nsp_matrix_destroy (bs);
  if (xr != NULL)  nsp_matrix_destroy (xr);
  if (scale != NULL)  nsp_matrix_destroy (scale);
  return RET_BUG;
}

/* interface for ereduc  */

int int_ereduc(Stack stack, int rhs, int opt, int lhs)
{
  int rank;
  double tol=1.e-8;
  NspMatrix *E=NULL,*Q=NULL,*Z=NULL,*Istair=NULL;
  CheckStdRhs(2,2);
  CheckLhs(1,5);
  
  if ((E = GetRealMatCopy (stack, 1)) == NULLMAT)   return RET_BUG;

  if ( rhs == 2)
    {
      if (GetScalarDouble (stack, 2, &tol) == FAIL)
	return RET_BUG;
    }
  
  if ((Q = nsp_matrix_create (NVOID, 'r', E->m, E->m)) == NULLMAT) goto err;
  if ((Z = nsp_matrix_create (NVOID, 'r', E->n, E->n)) == NULLMAT) goto err;
  if ((Istair = nsp_matrix_create (NVOID, 'r', 1, E->m)) == NULLMAT) goto err;
  nsp_ctrlpack_ereduc(E->R,&E->m,&E->n,Q->R,Z->R,Istair->I,&rank,&tol);

  MoveObj(stack,1,NSP_OBJECT(E));
  if ( lhs >= 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(Q));
    }
  else
    nsp_matrix_destroy(Q);
  if ( lhs >= 3 )
    {
      MoveObj(stack,3,NSP_OBJECT(Z));
    }
  else
    nsp_matrix_destroy(Z);
  if ( lhs >= 4 )
    {
      Istair->convert = 'i';
      Istair=Mat2double(Istair);
      MoveObj(stack,4,NSP_OBJECT(Istair));
    }
  else
    nsp_matrix_destroy(Istair);
  if ( lhs >= 5 )
    {
      if ( nsp_move_double(stack,5,rank) == FAIL) goto err;
    }
  return Max(lhs,1);
 err:
  if ( E != NULL) nsp_matrix_destroy(E);
  if ( Q != NULL) nsp_matrix_destroy(Q);
  if ( Z != NULL) nsp_matrix_destroy(Z);
  if ( Istair != NULL) nsp_matrix_destroy(Istair);
  return RET_BUG;
}

/* interface for fstair */

int int_fstair(Stack stack, int rhs, int opt, int lhs)
{
  double tol = 1.e-8;
  int nblcks, rank, ierr;
  NspMatrix *A=NULL,*E=NULL,*Q=NULL,*Z=NULL,*Istair=NULL;
  NspMatrix *Imuk =NULL, *Inuk =NULL, *Imuk0 =NULL, *Inuk0 =NULL,
    *Mnei =NULL, *Wrk =NULL, *IWrk =NULL;

  CheckStdRhs(7,7);
  CheckLhs(1,9);
  
  if ((A = GetRealMatCopy (stack, 1)) == NULLMAT)   return RET_BUG;
  if ((E = GetRealMatCopy (stack, 2)) == NULLMAT)   return RET_BUG;
  CheckSameDims (NspFname(stack), 1, 2, A, E);
  if ((Q = GetRealMatCopy (stack, 3)) == NULLMAT)   return RET_BUG;
  if ( Q->m != A->m || Q->m != Q->n )
    {
      Scierror("fstair: argument 3 should be of size %dx%d\n",A->m,A->m);
      return RET_BUG;
    }

  if ((Z = GetRealMatCopy (stack, 4)) == NULLMAT)   return RET_BUG;
  if ( Z->m != A->n || Z->m != Z->n )
    {
      Scierror("fstair: argument 3 should be of size %dx%d\n",A->n,A->n);
      return RET_BUG;
    }

  if ((Istair = GetMatCopyInt (stack, 5)) == NULLMAT)   return RET_BUG;
  if ( Istair->mn != A->m )
    {
      Scierror("fstair: argument 5 should be of size %d\n",A->m);
      return RET_BUG;
    }
  
  if (GetScalarInt (stack, 6, &rank) == FAIL)
    return RET_BUG;
  
  if ( rhs == 7 )
    {
      if (GetScalarDouble (stack, 7, &tol) == FAIL)
	return RET_BUG;
    }
    
  if ((Imuk = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT) goto err;
  if ((Inuk = nsp_matrix_create (NVOID, 'r', 1, A->m+1)) == NULLMAT) goto err;
  if ((Imuk0 = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT) goto err;
  if ((Inuk0 = nsp_matrix_create (NVOID, 'r', 1, A->m+1)) == NULLMAT) goto err;
  if ((Mnei = nsp_matrix_create (NVOID, 'r', 1, 4)) == NULLMAT) goto err;
  if ((Wrk = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT) goto err;
  if ((IWrk = nsp_matrix_create (NVOID, 'r', 1, A->n)) == NULLMAT) goto err;

  nsp_ctrlpack_fstair(A->R,E->R,Q->R,Z->R,&A->m,&A->n,Istair->I,&rank,&tol,
		      &nblcks,Imuk->I,Inuk->I,Imuk0->I,Inuk0->I,Mnei->I,Wrk->R,
		      IWrk->I,&ierr);
  if ( ierr == 1 )
    {
      Scierror("fstair: failed\n") ;
      return RET_BUG;
    }
  
  Imuk->convert = 'i'; Imuk=Mat2double(Imuk); nsp_matrix_resize(Imuk,1,nblcks);
  Inuk->convert = 'i'; Inuk=Mat2double(Inuk); nsp_matrix_resize(Inuk,1,nblcks);
  Imuk0->convert = 'i'; Imuk0=Mat2double(Imuk0); nsp_matrix_resize(Imuk0,1,nblcks);
  Inuk0->convert = 'i'; Inuk0=Mat2double(Inuk0); nsp_matrix_resize(Inuk0,1,nblcks);
  Mnei->convert = 'i'; Mnei=Mat2double(Mnei);
  
  MoveObj(stack,1,NSP_OBJECT(A));
  if ( lhs >= 2 ) MoveObj(stack,2,NSP_OBJECT(E));
  if ( lhs >= 3 ) MoveObj(stack,3,NSP_OBJECT(Q));
  if ( lhs >= 4 ) MoveObj(stack,4,NSP_OBJECT(Z));

#define return_lhs(number,name)			\
  if ( lhs >= number )				\
    {						\
      MoveObj(stack,number,NSP_OBJECT(name));	\
    }						\
  else nsp_matrix_destroy(name);

  /* we already have resized the matrices 
   * Imuk,Inuk etc.. have nblcks size 
   */
  return_lhs(5,Imuk);
  return_lhs(6,Inuk);
  return_lhs(7,Imuk0);
  return_lhs(8,Inuk0);
  return_lhs(9,Mnei);
  
  if ( Wrk   != NULL) nsp_matrix_destroy(Wrk );
  if ( IWrk  != NULL) nsp_matrix_destroy(IWrk);
  return Max(lhs,1);
 err:
  if ( Imuk  != NULL) nsp_matrix_destroy(Imuk);
  if ( Inuk  != NULL) nsp_matrix_destroy(Inuk);
  if ( Imuk0 != NULL) nsp_matrix_destroy(Imuk);
  if ( Inuk0 != NULL) nsp_matrix_destroy(Inuk);
  if ( Mnei  != NULL) nsp_matrix_destroy(Mnei);
  if ( Wrk   != NULL) nsp_matrix_destroy(Wrk );
  if ( IWrk  != NULL) nsp_matrix_destroy(IWrk);
  return RET_BUG;
}


/* interface for ppol */

int int_ppol(Stack stack, int rhs, int opt, int lhs)
{
  int ncont = 0, indcon = 0, one = 1, ierr, sizework;
  double eps = nsp_dlamch("eps");
  double tol = 0.1 * sqrt(eps);
  NspMatrix *A=NULL,*B=NULL,*P=NULL,*Res=NULL,*Z=NULL;
  NspMatrix *Wrk =NULL, *Iwrk =NULL;
    
  CheckStdRhs(3,3);
  CheckLhs(0,1);
  
  if ((A = GetRealMatCopy (stack, 1)) == NULLMAT)   return RET_BUG;
  CheckSquare(NspFname(stack), 1, A);
  
  if ((B = GetRealMatCopy (stack, 2)) == NULLMAT)   return RET_BUG;
  
  if ( A->m != B->m )
    {
      Scierror("ppol: first and second arguments should have same rows\n");
      return RET_BUG;
    }
  
  if ((P = GetMatCopy (stack, 3)) == NULLMAT)   return RET_BUG;
  if ( P->mn != A->m )
    {
      Scierror("ppol: third argument should be of size %d\n",A->m);
      return RET_BUG;
    }
  if ( P->rc_type == 'r' )
    {
      if (nsp_mat_complexify(P,0.0) == FAIL ) return RET_BUG;
    }
  nsp_complex2double(P->R,P->mn);

  if ( A->m == 0 || B->n == 0 )
    {
      if ((Res = nsp_matrix_create (NVOID, 'r', B->n, P->mn)) == NULLMAT) goto err;
      MoveObj(stack,1,NSP_OBJECT(Res));
      return Max(lhs,1);
    }
  
  if ((Res = nsp_matrix_create (NVOID, 'r', B->n, P->mn)) == NULLMAT) goto err;
  if ((Z = nsp_matrix_create (NVOID, 'r', P->mn,P->mn)) == NULLMAT) goto err;
  sizework = Max( P->mn * B->n + 3 *B->n, B->n*B->n + B->n*Max(2,B->n)+  3 *B->n +2*P->mn);
  if ((Wrk = nsp_matrix_create (NVOID, 'r', 1, sizework)) == NULLMAT) goto err;
  if ((Iwrk = nsp_matrix_create (NVOID, 'r', 1, P->mn)) == NULLMAT) goto err;
  
  nsp_ctrlpack_ssxmc(&P->mn, &B->n, A->R, &P->mn, B->R, &ncont, &indcon, Iwrk->I,
		     Z->R,
		     Wrk->R + B->n,
		     Wrk->R + B->n + P->mn*B->n,
		     Wrk->R + B->n + P->mn*B->n + B->n,
		     Wrk->I, &tol,&one);
    
  if ( ncont < P->mn)
    {
      Scierror("%s: Uncontrollable system.\n", "ppol");
    }
    
  nsp_ctrlpack_polmc (&P->mn, &B->n, &P->mn, &B->n, A->R, B->R,
		      Res->R, P->R, P->R+P->mn, Z->R, &indcon, Iwrk->I,
		      &ierr, Wrk->I,
		      Wrk->R + B->n,
		      Wrk->R + B->n + B->n * B->n,
		      Wrk->R + B->n + B->n * B->n + B->n * Max(2, B->n),
		      Wrk->R + B->n + B->n * B->n + B->n * Max(2, B->n) + P->mn,
		      Wrk->R + B->n + B->n * B->n + B->n * Max(2, B->n) + 2*P->mn,
		      Wrk->R + B->n + B->n * B->n + B->n * Max(2, B->n) + 2*P->mn+ B->n);
  if (ierr)
    {
      Scierror("%s: Uncontrollable system.\n", "ppol");
      goto err;
    }
  nsp_matrix_destroy(Z);
  nsp_matrix_destroy(Wrk);
  nsp_matrix_destroy(Iwrk);
  MoveObj(stack,1,NSP_OBJECT(Res));
  return Max(lhs,1);
 err:
  if ( Res != NULL)  nsp_matrix_destroy(Res);
  if ( Z != NULL) nsp_matrix_destroy(Z);
  if ( Wrk != NULL) nsp_matrix_destroy(Wrk);
  if ( Iwrk != NULL) nsp_matrix_destroy(Iwrk);
  return RET_BUG;
}

/* interface to tr_zer function  */

int int_tzer(Stack stack, int rhs, int opt, int lhs)
{
  int i,Nu=0,Rank=0,Err=0, AFm,AFn;
  NspMatrix *Mat[4],*Z=NULL,*AF=NULL,*BF=NULL,*WorkA=NULL,*Work1=NULL,*Work2=NULL;
  double eps = nsp_dlamch("eps");
  CheckRhs(4,4);
  CheckLhs(1,3);

  for ( i = 0 ; i < 4 ; i++ )
    {
      if ((Mat[i] = GetRealMatCopy (stack, i+1 )) == NULLMAT)   return RET_BUG;
    }
  
  if (Mat[1]->m != Mat[0]->n || Mat[2]->n != Mat[0]->n || Mat[1]->n != Mat[3]->n || Mat[2]->m != Mat[3]->m)
    {
      Scierror("tzer: Incompatible arguments, dimensions mismatch.\n");
      return RET_BUG;
    }

  AFm = Mat[0]->n + Mat[2]->m;
  AFn = Mat[0]->n + Mat[1]->n;
    
  if ((Z = nsp_matrix_create (NVOID, 'c', 1, Mat[0]->n)) == NULLMAT) goto err;
  if ((AF = nsp_matrix_create (NVOID, 'r', AFm, AFn)) == NULLMAT) goto err;
  if ((BF = nsp_matrix_create (NVOID, 'r', AFm, AFn)) == NULLMAT) goto err;
  
  if ((WorkA = nsp_matrix_create (NVOID, 'r', Mat[0]->n, Mat[0]->n)) == NULLMAT) goto err;
  if ((Work1 = nsp_matrix_create (NVOID, 'r', 1, Max(Mat[1]->n, Mat[2]->m))) == NULLMAT) goto err;
  if ((Work2 = nsp_matrix_create (NVOID, 'r', 1,  Max(Work1->mn, Mat[0]->n + 1))) == NULLMAT) goto err;
  
  nsp_ctrlpack_sszer(&Mat[0]->n, &Mat[1]->n, &Mat[2]->m,Mat[0]->R,
		     &Mat[0]->n, Mat[1]->R, Mat[2]->R, &Mat[2]->m, Mat[3]->R,
		     &eps, Z->R, Z->R + Z->mn, &Nu, &Rank, AF->R, &AF->m, BF->R, &AF->n,
		     WorkA->R, Work1->R, &Work1->mn, Work2->R, &Work2->mn, &Err);
  
  nsp_matrix_destroy(AF);
  nsp_matrix_destroy(BF);
  nsp_matrix_destroy(WorkA);
  nsp_matrix_destroy(Work1);
  
  if (Err > 0)
    {
      switch ( Err )
	{
	case 1:
	  Scierror("tzer: Incompatible dimensions between arguments.\n");break;
        case 2:
	  Scierror("tzer: divide by zero.\n");break;
        default:
	  Scierror("tzer: ierr %d from qitz (eispack).\n",Err);break;
	}
      nsp_matrix_destroy(Z);
      nsp_matrix_destroy(Work2);
      return RET_BUG;
    }
  
  if (Rank == 0 && Nu > 0)
    {
      memset(Z->R, 0x00, 2*Nu * sizeof(double));
      memset(Work2->R, 0x00, Nu * sizeof(double));
    }

  /* convert Z to nsp complex */
  nsp_double2complex(Z->R, Z->mn);
  /* resize Z */
  nsp_matrix_resize(Z,1, Nu);
  MoveObj(stack,1,NSP_OBJECT(Z));

  if ( lhs >= 2)
    {
      nsp_matrix_resize(Work2,1,Max(Nu,1));
      MoveObj(stack,2,NSP_OBJECT(Work2));
    }
  else
    {
      nsp_matrix_destroy(Work2);
    }
  if (lhs == 3)
    {
      if ( nsp_move_double(stack,3,Rank) == FAIL) goto err;
    }
  return Max(1,lhs);
 err:
  if (Z != NULL) nsp_matrix_destroy(Z );
  if (AF != NULL) nsp_matrix_destroy(AF );
  if (BF != NULL) nsp_matrix_destroy(BF );
  if (WorkA != NULL) nsp_matrix_destroy(WorkA );
  if (Work1 != NULL) nsp_matrix_destroy(Work1 );
  if (Work2 != NULL) nsp_matrix_destroy(Work2 );
  return RET_BUG;
}

/* interface to ricc function (obsolete, replaced by riccati or ric_desc )
 * which use schur and qz 
 */

int int_ricc(Stack stack, int rhs, int opt, int lhs)
{
  double rcond, ferr;
  char *mode = "cont";
  int wantc = 0, wantd = 0, wschur = 0, sign = 0, invf = 0;
  int work_size = 0, info = 0;
  int iSize = 0;

  NspMatrix *X=NULL, *R=NULL, *I=NULL;
  NspMatrix *Iwork=NULL, *Dwork=NULL, *Bwork=NULL;
  NspMatrix *A,*D,*C;
  CheckRhs(4,5);
  CheckLhs(1,3);

  if ((A = GetRealMatCopy (stack, 1 )) == NULLMAT)   return RET_BUG;
  CheckSquare(NspFname(stack), 1, A);
  if ((D = GetRealMatCopy (stack, 2 )) == NULLMAT)   return RET_BUG;
  CheckSquare(NspFname(stack), 2, D);
  if ((C = GetRealMatCopy (stack, 3 )) == NULLMAT)   return RET_BUG;
  CheckSquare(NspFname(stack), 3, C);
  if ( ! ( A->m == D->m && A->m == C->m))
    {
      Scierror("%s: The three first arguments mus have the same number of rows\n",NspFname(stack));
      return RET_BUG;
    }
    
  if (( mode = GetString(stack,4)) == ((char *) 0) ) return RET_BUG;
    
  wantc = (strcmp(mode, "cont") == 0 || strcmp(mode, "CONT") == 0);
  wantd = (strcmp(mode, "disc") == 0 || strcmp(mode, "DISC") == 0);

  if (wantc == FALSE && wantd == FALSE)
    {
      Scierror("%s: Wrong value for input argument 4, should be \"cont\" or \"disc\"\n",NspFname(stack));
      return RET_BUG;
    }

  if ( rhs >= 5 ) 
    {
      char *method = "";
      if (( method = GetString(stack,5)) == ((char *) 0) ) return RET_BUG;
      wschur = (strcmp(method, "schr") == 0 || strcmp(method, "SCHR") == 0);
      if (wantc)
	{
	  sign  = (strcmp(method, "sign") == 0 || strcmp(method, "SIGN") == 0);
	  if (wschur == FALSE && sign == FALSE)
	    {
	      Scierror("%s: Wrong value for input argument 5, must be \"schur\" or \"sign\"\n",NspFname(stack));
	      return RET_BUG;
	    }
	}
      else
	{
	  invf  = (strcmp(method, "invf") == 0 || strcmp(method, "INVF") == 0);
	  if (wschur == FALSE && invf == FALSE)
	    {
	      Scierror("%s: Wrong value for input argument 5, must be \"schur\" or \"invf\"\n",NspFname(stack));
	      return RET_BUG;
	    }
	}
    }


  if (( X = nsp_matrix_create (NVOID, 'r', A->m,A->m ))  == NULLMAT) goto err;
  if (( R = nsp_matrix_create (NVOID, 'r', A->m,1 ))  == NULLMAT) goto err;
  if (( I = nsp_matrix_create (NVOID, 'r', A->m,1 ))  == NULLMAT) goto err;
  iSize = Max(2 * A->m, A->m * A->m);
  if (( Iwork = nsp_matrix_create (NVOID, 'r', 1, iSize ))  == NULLMAT) goto err;
  iSize = 2 * A->m;
  if (( Bwork = nsp_matrix_create (NVOID, 'r', 1, iSize ))  == NULLMAT) goto err;
  if (wantc)
    {
      work_size =(wschur) ?  9 * A->m * A->m + 4 * A->m + Max(1, 6 * A->m)
	:  9 * A->m * A->m + 7 * A->m + 1;
    }
  else
    {
      work_size =(wschur) ?  12 * A->m * A->m + 22 * A->m + Max(16, 4 * A->m)
	:  28 * A->m * A->m + 2 * A->m + Max(1, 2 * A->m);
    }
  if (( Dwork = nsp_matrix_create (NVOID, 'r', 1, work_size ))  == NULLMAT) goto err;

  if (wantc)
    {
      if (wschur)
	{
	  nsp_ctrlpack_riccsl("N", &A->m, A->R, &A->m, "U", C->R, &A->m, D->R,
			      &A->m, X->R, &A->m, R->R, I->R, &rcond,
			      &ferr,Dwork->R, &Dwork->mn, Iwork->I,
			      Bwork->I, &info,0L,0L);
	  if (info != 0)
	    {
	      Scierror("%s: RICCSL exit with info = %d.\n", NspFname(stack), info);
	      return RET_BUG;
	    }
	}
      else if (sign)
	{
	  nsp_ctrlpack_riccms("N", &A->m, A->R, &A->m, "U", C->R, &A->m, D->R,
			      &A->m, X->R, &A->m, R->R, I->R, &rcond,
			      &ferr,Dwork->R, &Dwork->mn, Iwork->I, &info,0L,0L);
	  if (info != 0)
	    {
	      Scierror("%s: RICCMS exit with info = %d.\n", NspFname(stack), info);
	      return RET_BUG;
	    }
	}
    }
  else
    {
      if (wschur)
	{
	  nsp_ctrlpack_ricdsl("N", &A->m, A->R, &A->m, "U", C->R, &A->m, D->R,
			      &A->m, X->R, &A->m, R->R, I->R, &rcond,
			      &ferr,Dwork->R, &Dwork->mn, Iwork->I,
			      Bwork->I, &info,0L,0L);
	  if (info != 0)
	    {
	      Scierror("%s: RICDSL exit with info = %d.\n", NspFname(stack), info);
	      return RET_BUG;
	    }
	}
      else if (invf)
	{
	  nsp_ctrlpack_ricdmf("N", &A->m, A->R, &A->m, "U", C->R, &A->m, D->R,
			      &A->m, X->R, &A->m, R->R, I->R, &rcond,
			      &ferr,Dwork->R, &Dwork->mn, Iwork->I, &info,0L,0L);
	  if (info != 0)
	    {
	      Scierror( "%s: RICDMF exit with info = %d.\n", NspFname(stack), info);
	      return RET_BUG;
	    }
	}
    }

  MoveObj(stack,1,NSP_OBJECT(X));
  if (lhs >= 2 )
    {
      if ( nsp_move_double(stack,2,rcond)== FAIL) goto err;
    }
  if (lhs >= 3) 
    {
      if ( nsp_move_double(stack,3,ferr)== FAIL) goto err;
    }
  return Max(lhs,1);
 err:
  return RET_BUG;
}

/* interface for ltitr */

int int_ltitr(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *X0=NULL, *A=NULL, *B=NULL, *U=NULL, *Xf=NULL, *W  = NULL, *Wgr= NULL;
  int one   = 1, ig = 0, i;

  CheckRhs(3,4);
  CheckLhs(1,2);
  if ((A = GetRealMatCopy (stack,1)) == NULLMAT)   return RET_BUG;      
  CheckSquare(NspFname(stack),1,A);
  if ((B = GetRealMatCopy (stack,2)) == NULLMAT)   return RET_BUG;      
  if ( A->m != B->m)
    {
      Scierror("ltitr: first and second arguments should have same number of rows\n");
      return RET_BUG;
    }
  if ((U = GetRealMatCopy (stack,3)) == NULLMAT)   return RET_BUG;      
  if (rhs == 4)
    {
      if ((X0 = GetRealMatCopy (stack,4)) == NULLMAT)   return RET_BUG;      
      if (X0->m != A->m || X0->n != 1)
        {
	  Scierror("ltitr: fourth argument should be of size %dx%d\n",A->m,X0->n );
	  return RET_BUG;
        }
    }
  if (( W = nsp_matrix_create (NVOID, 'r', 1, A->m ))  == NULLMAT) goto err;
  if (( Wgr = nsp_matrix_create (NVOID, 'r', A->m, U->n+1 ))  == NULLMAT) goto err;
  if (rhs == 4)
    {
      C2F(dcopy)(&A->m, X0->R, &one, Wgr->R, &one);
    }
  else
    {
      memset(Wgr->R, 0, A->m * sizeof(double));
    }

  for (i = 0; i < U->n; i++)
    {
      ig = (i + 1) * A->m;
      nsp_calpack_dmmul(A->R, &A->m, Wgr->R + ig - A->m, &A->m, W->R, &A->m, &A->m, &A->m, &one);
      nsp_calpack_dmmul(B->R, &A->m, U->R + (i * B->n), &B->n, Wgr->R + ig, &A->m, &A->m, &B->n, &one);
      nsp_calpack_dadd(&A->m, W->R, &one, Wgr->R + ig, &one);
    }
  if ( lhs >= 2 ) 
    {
      if (( Xf = nsp_matrix_create (NVOID, 'r',  A->m,1 ))  == NULLMAT) goto err;
      C2F(dcopy)(&A->m, Wgr->R + U->n*A->m, &one, Xf->R, &one);
    }
  nsp_matrix_resize(Wgr,A->m,U->n);

  if ( lhs == 1 )
    {
      MoveObj(stack,1,NSP_OBJECT(Wgr));
    }
  if ( lhs == 2 ) 
    {
      MoveObj(stack,1,NSP_OBJECT(Xf));
      MoveObj(stack,2,NSP_OBJECT(Wgr));
    }
  nsp_matrix_destroy( W);
  return Max(lhs,1);
 err:
  if ( W != NULL) nsp_matrix_destroy( W);
  if ( Wgr != NULL) nsp_matrix_destroy( Wgr);
  if ( Xf != NULL)  nsp_matrix_destroy( Xf);
  return RET_BUG;
}

/* new function to avoid to swap data */

int nsp_ctrlpack_rexpan (const double *a,const int *la, const double *b,const int *lb, double *c, const int *nmax);

/* ldiv should loop on ldivp 
 * should be better to return ldiv as a polynomial matrix ? 
 */ 

int int_ldivp(Stack stack, int rhs, int opt, int lhs)
{
  int k=0,i;
  NspMatrix *Out=NULL;
  nsp_polynom A,B;
  NspMatrix *pA,*pB;
  CheckRhs(3,3);
  CheckLhs(0,1);
  
  if ((A=GetPolynom(stack,1))== NULL) return RET_BUG;
  if ((B=GetPolynom(stack,2))== NULL) return RET_BUG;
  if (GetScalarInt (stack, 3, &k) == FAIL)
    return RET_BUG;
  if (( Out = nsp_matrix_create(NVOID,'r', 1, k)) == NULL)
    goto err;
  pA = (NspMatrix *) A;
  pB = (NspMatrix *) B;
  if (!( pA->rc_type == 'r' &&  pB->rc_type == 'r'))
    {
      Scierror("ldivp: first and second arguments should be real polynomials\n");
      return RET_BUG;
    }
  for (i=0 ; i < Out->mn ; i++) Out->R[i]=0.0;
  nsp_ctrlpack_rexpan( pB->R, &pB->mn, pA->R, &pA->mn, Out->R, &k);
  MoveObj(stack,1,NSP_OBJECT(Out));
  return Max(lhs,1);
 err:
  return RET_BUG;
}

/* freq */

static int int_freq_state(Stack stack, int rhs, int opt, int lhs)
{
  double dRcond;
  int job = 0, one=1, i;
  NspMatrix * A = NULL;
  NspMatrix * B = NULL;
  NspMatrix * C = NULL;
  NspMatrix * D = NULL;
  NspMatrix * F = NULL;
  NspMatrix * Wgr = NULL;
  NspMatrix * W = NULL;
  NspMatrix * W1 = NULL;
  
  if ((A = GetRealMatCopy (stack,1)) == NULLMAT)   return RET_BUG;
  CheckSquare(NspFname(stack),1,A);
  if ((B = GetRealMatCopy (stack,2)) == NULLMAT)   return RET_BUG;
  if ((C = GetRealMatCopy (stack,3)) == NULLMAT)   return RET_BUG;
  
  if ( rhs == 5 )
    {
      if ((D = GetRealMatCopy (stack,4)) == NULLMAT)   return RET_BUG;
      if ((F = GetMatCopy (stack,5)) == NULLMAT)   return RET_BUG;
    }
  else
    {
      if ((F = GetMatCopy (stack,4)) == NULLMAT)   return RET_BUG;
    }
  if (nsp_mat_complexify(F,0.0) == FAIL ) return RET_BUG;
  nsp_complex2double(F->R,F->mn);
      
  if ( A->m != B->m ) 
    {
      Scierror("freq: second and first arguments should have the same number of rows\n");
      return RET_BUG;
    }

  if ( A->m != C->n )
    {
      Scierror("freq: the column number of third argument should equal the number of rows of first argument\n");
      return RET_BUG;
    }
  
  if ( rhs == 5 )
    {
      if (D->m != C->m || D->n != B->n)
	{
	  Scierror("frep: Wrong size for argument: Incompatible dimensions.\n");
	  return RET_BUG;
	}
    }
  
  if (( W1 = nsp_matrix_create(NVOID,'r',1, A->m)) == NULL) goto err;
  if (( W   = nsp_matrix_create(NVOID,'r', 1, 2 * A->m * A->m + 2 * A->m))  == NULL) goto err;

  /* need a complex matrix here */
  
  if (( Wgr = nsp_matrix_create(NVOID,'c', C->m, B->n * F->mn ))  == NULL) goto err;
  for ( i= 0 ; i < Wgr->mn ; i++) {Wgr->C[i].r=0;Wgr->C[i].i=0;}
  
  double *Fp = F->R;
  for (i = 0; i < F->mn; i++)
    {
      static int first = 0;
      int ig = i * B->n * C->m;
      nsp_ctrlpack_dfrmg( &job, &A->m, &A->m, &C->m, &C->m, &B->n, &A->m,
			  A->R, B->R, C->R, Fp, Fp +F->mn, Wgr->R + ig, Wgr->R + Wgr->mn + ig, &dRcond, W->R, W1->I);
      if ( first == 0 && dRcond + 1 == 1)
        {
	  Sciprintf("Warning: matrix is close to singular or badly scaled. rcond = %s\n", dRcond);
	}
      first++;
      if ( rhs == 5)
        {
	  int iSize = B->n * C->m;
	  nsp_calpack_dadd(&iSize, D->R, &one, Wgr->R + ig, &one);
        }
      Fp++;
    }
  
  nsp_double2complex(Wgr->R, Wgr->mn);
  nsp_matrix_destroy( W);
  nsp_matrix_destroy( W1);
  MoveObj(stack,1,NSP_OBJECT(Wgr));
  return Max(lhs,1);
 err:
  if ( W != NULL) nsp_matrix_destroy( W);
  if ( W1 != NULL) nsp_matrix_destroy( W1);
  if ( Wgr != NULL) nsp_matrix_destroy( Wgr);
  return RET_BUG;
}

/* freq */

static int int_freq_tf(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *N=NULL,*D=NULL;
  NspMatrix *F=NULL,*Res=NULL,*Nm,*Dm;
  int i;

  if ( IsMatObj(stack,1))
    {
      if ((N = (NspObject*) GetRealMatCopy (stack,1)) == NULL)   return RET_BUG;
    }
  else if (IsPMatObj(stack,1))
    {
      if ((N= (NspObject*) GetPMat(stack,1))== NULL) return RET_BUG;
    }
  else
    {
      Scierror("freq: first argument should be a matrix or a polynom.\n");
      return RET_BUG;
    }
   
  if ( IsMatObj(stack,2))
    {
      if ((D = (NspObject*) GetRealMatCopy (stack,2)) == NULL)   return RET_BUG;
    }
  else if (IsPMatObj(stack,2))
    {
      if ((D = (NspObject*) GetPMat(stack,2))== NULL) return RET_BUG;
    }
  else
    {
      Scierror("freq: second argument should be a matrix or a polynom.\n");
      return RET_BUG;
    }

  Nm = (NspMatrix *) N;
  Dm = (NspMatrix *) D;
   
  if ((F = GetMatCopy (stack,3)) == NULLMAT)   return RET_BUG;
   
  if ( Nm->m !=  Dm->m || Nm->n !=  Dm->n) 
    {
      Scierror("freq: first and second argument should have the same size\n");
      return RET_BUG;
    }

  if ((Res = nsp_matrix_create(NVOID,F->rc_type, Nm->m,0)) == NULLMAT) goto err;
   
  for ( i = 0; i < F->mn ; i++)
    {
      NspMatrix *Ni,*Di;
      if ( IsPMat(N))
	{
	  if ((Ni= nsp_pmatrix_horner((NspPMatrix *) N,F,i)) == NULLMAT) goto err;
	}
      else
	{
	  if ((Ni = (NspMatrix *) nsp_object_copy(N)) == NULL ) goto err;
	}
      if ( IsPMat(D))
	{
	  if ((Di= nsp_pmatrix_horner((NspPMatrix *)D,F,i)) == NULLMAT) goto err;
	}
      else
	{
	  if ((Di = (NspMatrix *) nsp_object_copy(D)) == NULL ) goto err;
	}
      if ( nsp_mat_div_tt(Ni,Di) == FAIL) goto err;
      if ( nsp_matint_concat_right_bis((NspObject *) Res,(NspObject *) Ni) == FAIL) goto err;
      nsp_matrix_destroy(Ni);
      nsp_matrix_destroy(Di);
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  return Max(lhs,1);
 err:
  return RET_BUG;
}

int int_freq(Stack stack, int rhs, int opt, int lhs)
{
  CheckStdRhs(3,5);
  CheckLhs(0,1);
  return (rhs == 3) ? int_freq_tf(stack,rhs,opt,lhs)
    :  int_freq_state(stack,rhs,opt,lhs);
}


/* arl2 */

extern double no2f_gnrm;
 
int int_arl2(Stack stack, int rhs, int opt, int lhs)
{
  nsp_option opts[] ={{"all",s_bool,NULLOBJ,-1},
		      {"imp",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspPMatrix *PNum = NULL,*PDen = NULL, *PiDen=NULL;
  NspMatrix *Den1=NULL, *Work=NULL, *IWork=NULL, *Y= NULL,*Den = NULL, *Err = NULL;
  int one = 1, N, Ng, all = FALSE, Degree= 0, lunit = 0, info=0,ierr=0;
  double err   = 0;
  
  CheckRhs(3,5);
  CheckLhs(1,3);
  // get Y
  if ( IsMatObj(stack,1)) 
    {
      if ((Y = GetRealMatCopy (stack,1)) == NULLMAT)   return RET_BUG;
    }
  else if (IsPMatObj(stack,1))
    {
      nsp_polynom P;
      if ((P=GetPolynom(stack,1))== NULL) return RET_BUG;
      if (((NspMatrix*) P)->rc_type != 'r') 
	{
	  Scierror("%s: first argument should be a real polynom\n", "arl2_ius");
	  return RET_BUG;
	}
      Y = (NspMatrix*) P;
    }
  else
    {
      Scierror("%s: first argument should be a real matrix or a polynom\n", "arl2_ius");
      return RET_BUG;
    }

  Ng = Y->mn - 1;
  if ((PiDen = GetPMatCopy(stack,2)) == NULLPMAT) return RET_BUG;
  if ( PiDen->mn != 1)
    {
      Scierror("%s: second argument should be a 1x1 real polynom\n", "arl2_ius");
      return RET_BUG;
    }
  if (((NspMatrix*) PiDen->S[0])->rc_type != 'r') 
    {
      Scierror("%s: second argument should be a real polynom\n", "arl2_ius");
      return RET_BUG;
    }
  Den = (NspMatrix*) PiDen->S[0];
  Degree= Max(Den->mn -1,0);
  
  if ( Den->mn == 0 ) goto err;
    
  double dblScal = 1.0 / Den->R[Den->mn -1];
  C2F(dscal)(&Den->mn, &dblScal, Den->R, &one);

  // get n
  if (GetScalarInt (stack, 3, &N) == FAIL) return RET_BUG;
  if ( N < 1)
    {
      Scierror("%s: third argument should be gretear or equal to one\n", "arl2_ius", 3);
      return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &all, &info) == FAIL )
    return RET_BUG;
  if ( info < 0 )
    {
      Scierror("%s: optional argument imp should be a non negative integer\n");
      return RET_BUG;
    }
 
  if (all)
    {
      int i, Nsol = 0, maxsol = 20;
      int WorkSize = 34 + 34 * N + 7 * Ng + N * Ng + N * N * (Ng + 2) + 4 * (N + 1) * maxsol;
      int IWorkSize = 29 + N * N + 4 * N + 2 * maxsol;

      if (( Den1 = nsp_matrix_create(NVOID,'r', 1, maxsol*(N+1)))==  NULLMAT )
	goto err;
      if (( Work = nsp_matrix_create(NVOID,'r', 1, WorkSize)) ==  NULLMAT )
	goto err;
      if (( IWork = nsp_matrix_create(NVOID,'r', 1, IWorkSize)) ==  NULLMAT )
	goto err;
	
      nsp_ctrlpack_arl2a(Y->R, &Y->mn, Den1->R, &maxsol, &Nsol, &N, &info,&ierr,&lunit, Work->R, IWork->I);
      if ( ierr != 0 )
	{
	  switch ( ierr)
	    {
	    case 3 : Sciprintf("%s: Loop on two orders detected.\n", "arl2a"); break;
	    case 4 : Sciprintf("%s: Impossible to reach required order.\n", "arl2a");break;
	    case 5 : Sciprintf("%s: Failure when looking for the intersection with domains bounds.\n", "arl2a"); break;
	    case 7 : Scierror("%s: Too many solutions found.\n", "arl2a"); return RET_BUG;
	    }
	}
      /* return a Nsolx1 matrix of polynoms */
      if ((PDen =nsp_pmatrix_create(NVOID,Nsol,1,NULL,-1, PiDen->var))== NULLPMAT) return RET_BUG;
      for ( i = 0 ; i < PDen->mn ; i++)
	{
	  NspMatrix *loc;
	  if ((loc = nsp_matrix_create("pe",'r',1,N+1)) == NULL) goto err;
	  PDen->S[i]= loc;
	  C2F(dcopy)(&N, Den1->R + i, &maxsol, loc->R, &one);
	  loc->R[N] = 1;
	}
      if ( lhs >= 2 ) 
        {
	  double n2f = sqrt(no2f_gnrm);

	  if ((PNum =nsp_pmatrix_create(NVOID,Nsol,1,NULL,-1,  PiDen->var))== NULLPMAT) return RET_BUG;
	  for ( i = 0 ; i < PNum->mn ; i++)
	    {
	      NspMatrix *loc;
	      if ((loc = nsp_matrix_create("pe",'r',1,N)) == NULL) goto err;
	      PNum->S[i]= loc;
	      nsp_ctrlpack_lq(&N, PDen->S[i]->R, Work->R, Y->R, &Ng);
	      C2F(dscal)(&N, &n2f, Work->R, &one);
	      C2F(dcopy)(&N, Work->R, &one, loc->R, &one);
	    }
	}
      if ( lhs >= 3) 
        {
	  double n2f = sqrt(no2f_gnrm);
	  if ((Err = nsp_matrix_create(NVOID,'r',Nsol, 1))== NULL) goto err;
	  for ( i = 0; i < Err->mn ; i++)
            {
	      double phi=nsp_ctrlpack_phi(PDen->S[i]->R, &N, Y->R, &Ng, Work->R);
	      Err->R[i] = sqrt(phi) * n2f;
            }
        }

      MoveObj(stack,1,NSP_OBJECT(PDen));
      if ( lhs >= 2) MoveObj(stack,2,NSP_OBJECT(PNum));
      if ( lhs >= 3) MoveObj(stack,3,NSP_OBJECT(Err));

      nsp_matrix_destroy( Den1);
      nsp_matrix_destroy( Work);
      nsp_matrix_destroy( IWork);
    }
  else
    {
      NspMatrix *Num;
      int iSizeNum = Max(N, Degree);
      int WorkSize = 32 + 32 * N + 7 * Ng + N * Ng + N * N * (Ng + 2);
      int IWorkSize = 29 + N * N + 4 * N;
      int iSizeTemp = iSizeNum + 1;
      if (( Num = nsp_matrix_create("n",'r', 1, iSizeNum)) ==  NULLMAT )
	goto err;
      if (( Den1 = nsp_matrix_create("d",'r', 1, iSizeTemp)) ==  NULLMAT )
	goto err;
      if (( Work = nsp_matrix_create(NVOID,'r', 1, WorkSize)) ==  NULLMAT )
	goto err;
      if (( IWork = nsp_matrix_create(NVOID,'r', 1, IWorkSize)) ==  NULLMAT )
	goto err;
      memset(Den1->R, 0x00, iSizeTemp * sizeof(double));
      C2F(dcopy)(&Den->mn, Den->R, &one, Den1->R, &one);
      nsp_ctrlpack_arl2(Y->R, &Y->mn, Num->R, Den1->R, &Degree, &N, &err, Work->R, IWork->I,
			&info, &ierr, &lunit);
      nsp_matrix_destroy( Work);
      nsp_matrix_destroy( IWork);

      if ( ierr != 0 )
	{
	  switch ( ierr)
	    {
	    case 3 : Sciprintf("%s: Loop on two orders detected.\n", "arl2a"); break;
	    case 4 : Sciprintf("%s: Impossible to reach required order.\n", "arl2a");break;
	    case 5 : Sciprintf("%s: Failure when looking for the intersection with domains bounds.\n", "arl2a");
	      break;
	    case 7 : Scierror("%s: Too many solutions found.\n", "arl2a");
	      nsp_matrix_destroy( Num);
	      nsp_matrix_destroy( Den1);
	      return RET_BUG;
            }
	}
      
      /* we need here to set up the var name */
      if ((PDen =nsp_pmatrix_create(NVOID,1,1,NULL,-1,  PiDen->var))== NULLPMAT) return RET_BUG;
      nsp_matrix_resize(Den1,1,N+1);
      PDen->S[0]=Den1;
      MoveObj(stack,1,NSP_OBJECT(PDen));
	      
      if (lhs >= 2)
	{
	  /* XXX we need here to set up the var name */
	  if ((PNum =nsp_pmatrix_create(NVOID,1,1,NULL,-1,  PiDen->var))== NULLPMAT) return RET_BUG;
	  nsp_matrix_resize(Num,1,N);
	  PNum->S[0]=Num;
	  MoveObj(stack,2,NSP_OBJECT(PNum));
	}
      if (lhs >= 3)
	{
	  if ( nsp_move_double(stack,3,err)== FAIL) goto err;
	}
    }
  return Max(lhs,1);
 err:
  return RET_BUG;
}
    
/* residu */

int int_residu(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Out=NULL;
  NspPMatrix *P,*Q1,*Q2;
  int i ,j;
  double tol = nsp_dlamch("eps");
  
  CheckRhs(3,3);
  CheckLhs(0,1);

  if ((P = GetPMatCopy(stack,1)) == NULLPMAT) return RET_BUG;
  if ((Q1 = GetPMatCopy(stack,2)) == NULLPMAT) return RET_BUG;
  if ((Q2= GetPMatCopy(stack,3)) == NULLPMAT) return RET_BUG;
  CheckSameDims (NspFname(stack), 1, 2, P, Q1);
  CheckSameDims (NspFname(stack), 1, 3, P, Q2);

  if ((Out =nsp_matrix_create(NVOID,'c',P->m, P->n)) == NULL) return RET_BUG;
  for ( i = 0; i < P->mn; i++)
    {
      NspMatrix *Mat[3]={P->S[i],Q1->S[i],Q2->S[i]};
      int err = 0;
      double v = 0;
      int isreal = TRUE;
      for (j = 0 ; j < 3 ; j++) isreal &= Mat[j]->rc_type == 'r';
      if ( isreal )
	{
	  int deg[3] = { P->S[i]->mn-1,Q1->S[i]->mn-1, Q2->S[i]->mn-1};
	  nsp_ctrlpack_residu(P->S[i]->R,  &deg[0],
			      Q1->S[i]->R, &deg[1],
			      Q2->S[i]->R, &deg[2],
			      &v, &tol, &err);
	  if (err)
	    {
	      Scierror("residu: error %d\n",err);
	      goto err;
	    }
	  Out->C[i].r = v;  Out->C[i].i = 0.0;
	}
      else
	{
	  int deg[3];
	  double real = 0, imag = 0;
	  for (j = 0 ; j < 3 ; j++)
	    {
	      if (nsp_mat_complexify (Mat[j], 0.0) == FAIL ) goto err;
	      nsp_complex2double(Mat[j]->R,Mat[j]->mn);
	      deg[j]=Mat[j]->mn-1;
	    }
	  nsp_ctrlpack_wesidu( Mat[0]->R, Mat[0]->R+Mat[0]->mn, &deg[0],
			       Mat[1]->R, Mat[1]->R+Mat[1]->mn, &deg[1],
			       Mat[2]->R, Mat[2]->R+Mat[2]->mn, &deg[2],
			       &real, &imag, &tol, &err);
	  if (err)
	    {
	      Scierror("residu: error %d\n",err);
	      goto err;
	    }
	  Out->C[i].r = real; Out->C[i].i = imag;
	}
    }
  MoveObj(stack,1,NSP_OBJECT(Out));
  return Max(lhs,1);
 err:
  return RET_BUG;
}



/* 
 * rtitr interface 
 */

static int pmatrix_max_degree(NspPMatrix *P)
{
  int i;
  int max= 0;
  for ( i = 0 ; i < P->mn ; i++ ) 
    {
      int deg = Max(P->S[i]->mn -1,0);
      if ( deg > max ) max = deg;
    }
  return max;
}

static NspMatrix* dmp2pm(NspPMatrix *P, int maxd)
{
  NspMatrix *D=NULL;
  int i= 0, one = 1;
  if ((D = nsp_matrix_create(NVOID,'r',1, P->mn * (maxd + 1))) == NULL) return NULL;
  memset(D->R, 0x00, D->mn* sizeof(double));
  for(i = 0 ;  i < P->mn; i++)
    {
      NspMatrix *Pi = P->S[i];
      C2F(dcopy)(&Pi->mn, Pi->R, &one, D->R + i, &P->mn);
    }
  return D;
}

int int_rtitr(Stack stack, int rhs, int opt, int lhs)
{
  NspPMatrix *Num,*Den;
  NspMatrix *U=NULL, *Up=NULL, *Yp=NULL, *Work=NULL, *IWork=NULL,*Out=NULL;
  NspMatrix *NumMat=NULL, *DenMat=NULL;
  int job=1, iErr=0, maxNumD, maxDenD, NumFree = FALSE, DenFree=FALSE;

  CheckRhs(3,5);
  CheckLhs(0,1);

  if ( IsMatObj(stack,1))
    {
      NspMatrix *NumM;
      if ((NumM = GetMat (stack, 1)) == NULLMAT)   return RET_BUG;
      if ( (Num =nsp_pmatrix_create_m(NVOID,NumM->m,NumM->n,NumM,NULL)) == NULLPMAT)
	return RET_BUG;
      NumFree = TRUE;
    }
  else
    {
      if ((Num = GetPMat(stack,1))== NULL) return RET_BUG;
    }

  if ( IsMatObj(stack,2))
    {
      NspMatrix *DenM;
      if ((DenM = GetMat (stack, 2)) == NULLMAT)   return RET_BUG;
      if ( (Den =nsp_pmatrix_create_m(NVOID,DenM->m,DenM->n,DenM,NULL)) == NULLPMAT)
	return RET_BUG;
      DenFree = TRUE;
    }
  else
    {
      if ((Den = GetPMat(stack,2))== NULL) return RET_BUG;
    }
  CheckSquare(NspFname(stack),2,Den);
  if ( Num->m != Den->m)
    {
      Scierror("Error: first and second arguments have incompatible sizes\n");
      return RET_BUG;
    }

  maxNumD = pmatrix_max_degree(Num);
  maxDenD = pmatrix_max_degree(Den);
    
  if ((U = GetRealMatCopy (stack,3)) == NULLMAT)   return RET_BUG;
  
  if ( Num->n != U->m)
    {
      Scierror("Error: first and thrid arguments have incompatible dimensions.\n");
      goto err;
    }

  if (rhs == 5)
    {
      if ((Up = GetRealMatCopy (stack,4 )) == NULLMAT)   return RET_BUG;
      if ((Yp = GetRealMatCopy (stack,5 )) == NULLMAT)   return RET_BUG;
      job = 2;
      if (Yp->m != Den->m && Yp->m != 0)
	{
	  Scierror("Error: fifth argument has wrong size\n");
	  return RET_BUG;
	}
      if (Yp->n != maxDenD)
	{
	  Scierror("Error: fifth argument has wrong size\n");
	  return RET_BUG;
	}
      if (Up->m != Num->n && Up->m != 0)
	{
	  Scierror("Error: forth argument has wrong size\n");
	  return RET_BUG;
	}
      if (Up->n != maxDenD)
	{
	  Scierror("Error: forth argument has wrong size\n");
	  return RET_BUG;
	}
    }
  if(( Out  = nsp_matrix_create(NVOID,'r',Den->m, U->n + maxDenD - maxNumD))==  NULL) goto err;
  if ((Work = nsp_matrix_create(NVOID,'r',1, Den->m)) == NULL) goto err;
  if ((IWork = nsp_matrix_create(NVOID,'r',1, Den->m)) == NULL) goto err;
      
  // converted a matrix of polynom to a polynomial matrix
  if ((DenMat = dmp2pm(Den, maxDenD))== NULL) goto err;
  if ((NumMat = dmp2pm(Num, maxNumD))== NULL) goto err;
  
  nsp_ctrlpack_rtitr(&U->m, &Den->m, &U->n, NumMat->R, &Den->m, &maxNumD, DenMat->R, &Den->m, &maxDenD,
		     (Up == NULL) ? NULL: Up->R, U->R, &U->m,
		     (Yp == NULL) ? NULL: Yp->R, Out->R, &Den->m, &job, IWork->I, Work->R, &iErr);
  if (iErr)
    {
      if (iErr == 1)
	{
	  char strValue[256];
	  sprintf(strValue, "%lf", Work->R[0]);
	  Sciprintf("Warning: matrix is close to singular or badly scaled. rcond = %s\n", strValue);
	  iErr = 0;
	}
      else if (iErr == 2)
	{
	  Scierror("Problem is singular.\n", "rtitr");
	}
    }
  if (DenFree == TRUE) nsp_pmatrix_destroy(Den);
  if (NumFree == TRUE) nsp_pmatrix_destroy(Num);
  nsp_matrix_destroy(NumMat);
  nsp_matrix_destroy(DenMat);
  nsp_matrix_destroy(Work);
  nsp_matrix_destroy(IWork);
  MoveObj(stack,1,NSP_OBJECT(Out));
  return Max(lhs,1);
 err:
  return RET_BUG;
}

#if 0

/* when we have Polynomial matrices we should loop 
 * on ldivp and return a cell
 */ 

int int_ldiv(Stack stack, int rhs, int opt, int lhs)
{
  int size1_max=0,  size2_max=0;
  NspMatrix *Out=NULL, *temp1=NULL, *temp2=NULL;
  NspMatrix *Coef1  = NULL, * Coef2  = NULL, *piRank1 = NULL, *piRank2= NULL;
  
  int iSize           = 0;
  int iK              = 0;
  int iOne            = 1;
    
  CheckRhs(3,3);
  CheckLhs(0,1);

  if ( IsPMatObj(stack,1))
    {
      NspPMatrix *P1;
      if ((P1 = GetPMat(stack,1)) == NULLPMAT) return RET_BUG;
      iSize = P1->mn;
      iRows = P1->m;
      iCols = P1->n;
    }
  else if ( IsMatObj(stack,1)) 
    {
      NspMatrix *M1;
      if ((M1 = GetRealMatCopy (stack,1)) == NULLMAT)   return RET_BUG;      
      iSize = M1->mn;
      iRows = M1->m;
      iCols = M1->n;
    }
  else
    {
      Scierror("ldiv: first argument should be a real matrix or a polynom expected.\n");
      return RET_BUG;
    }

  if (IsPMatObj(stack,2))
    {
      NspPMatrix *P2;
      if ((P2 = GetPMat(stack,2)) == NULLPMAT) return RET_BUG;
      if (P2->m != iRows || P2->n != iCols)
	{
	  Scierror("%s: Wrong size for input argument #%d: A same size as input argument %d expected.\n", "ldiv", 2, 1);
	  return RET_BUG;
        }
    }
  else if ( IsMatObj(stack,2))
    {
      NspMatrix *M2;
      if ((M2 = GetRealMatCopy (stack,2)) == NULLMAT)
	return RET_BUG;      
      if (M2->m != iRows || M2->n != iCols)
        {
	  Scierror("%s: first and second arguments should be of same size\n", "ldiv");
	  return RET_BUG;
        }
    }
  else
    {
      Scierror("ldiv: second argument should be a matrix or a polynom.\n");
      return RET_BUG;
    }
  if (GetScalarInt (stack, 3, &iK) == FAIL)
    return RET_BUG;
    
  if (( Out = nsp_matrix_create(NVOID,'r',iRows * iK, iCols)) == NULL)
    goto err;

  size1_max=0;
  size2_max=0;
  for (int i = 0; i < iSize; i++)
    {
      size1_max = Max(size1_max,piRank1->I[i] + 1);
      size2_max = Max(size2_max,piRank1->I[i] + 1);
    }
  
  if (( temp1 =  nsp_matrix_create(NVOID,'r',1, size1_max)) == NULL)
    goto err;
  if (( temp2 =  nsp_matrix_create(NVOID,'r',1, size2_max)) == NULL)
    goto err;
  
  for (int i = 0; i < iSize; i++)
    {
      int iSize1 = piRank1->I[i] + 1;
      int iSize2 = piRank2->I[i] + 1;
      C2F(dcopy)(&iSize1, Coef1->R + i, &iOne, temp1->R, &iOne);
      C2F(dcopy)(&iSize2, Coef2->R + i, &iOne, temp2->R, &iOne);
      nsp_calpack_dtild(&iSize1, temp1->R, &iOne);
      nsp_calpack_dtild(&iSize2, temp2->R, &iOne);
      nsp_ctrlpack_expan(temp2->R, &iSize2, temp1->R, &iSize1, Out->R, &iK);
    }
  nsp_matrix_destroy( temp1);
  nsp_matrix_destroy( temp2);
  nsp_matrix_destroy( Coef1);
  nsp_matrix_destroy( Coef2);
  nsp_matrix_destroy( piRank1);
  nsp_matrix_destroy( piRank2);
  MoveObj(stack,1,NSP_OBJECT(Out));
  return Max(lhs,1);
 err:
  return RET_BUG;
}

#endif

#include "../libsignal/signal.h"

int int_bezout(Stack stack, int rhs, int opt, int lhs)
{
  int ipb[6],np,i;
  NspPMatrix *P1,*P2,*U=NULL,*Gcd=NULL;
  NspMatrix *P1m,*P2m,*Work=NULL,*Out=NULL;
  double err= 0.0;
  int P1_degree, P2_degree, max_size, min_size, Work_size, Out_size;

  CheckRhs(2,2);
  CheckLhs(2,3);

  if ((P1 = GetPMat(stack,1))== NULL) return RET_BUG;
  if ((P2 = GetPMat(stack,2))== NULL) return RET_BUG;
  if ( P1->mn != 1 || P2->mn != 1)
    {
      Scierror("Error: expecting 1x1 polynomial matrices \n");
      return RET_BUG;
    }
  P1m = (NspMatrix *) P1->S[0];
  P2m = (NspMatrix *) P2->S[0];
  if ( P1m->rc_type == 'c' || P2m->rc_type == 'c' )
    {
      Scierror("Error: expecting 1x1 real polynomial matrices \n");
      return RET_BUG;
    }
  P1_degree = Max(P1m->mn-1,0);
  P2_degree = Max(P2m->mn-1,0);
  max_size      = Max(P1m->mn,P2m->mn);
  min_size      = Max(P1m->mn,P2m->mn);
  Work_size = 10 * max_size + 3*max_size*max_size;
  Out_size = 2 * ( P1_degree + P2_degree + 2) + min_size + 3;
  if (( Out = nsp_matrix_create(NVOID,'r',1,Out_size))== NULL) goto err;
  if (( Work = nsp_matrix_create(NVOID,'r',1,Work_size))== NULL) goto err;
  signal_recbez(P1m->R, &P1_degree, P2m->R, &P2_degree, Out->R, ipb, Work->R, &err);
  /* the gcd */
  if ((Gcd =nsp_pmatrix_create(NVOID,1,1,NULL,-1, P1->var))== NULLPMAT) goto err;
  np = ipb[1] - ipb[0];
  if ((Gcd->S[0]=nsp_matrix_create("pe",'r',1,np))== NULL) goto err;
  memcpy(((NspMatrix *) Gcd->S[0])->R, Out->R + ipb[0] - 1, np * sizeof(double));
  /* the matrix */
  if ( lhs >= 2 )
    {
      if ((U =nsp_pmatrix_create(NVOID,2,2,NULL,-1, P1->var))== NULLPMAT) goto err;
      for (i = 0; i < U->mn; i++)
	{
	  int size = ipb[(i+1) + 1] - ipb[(i+1)];
	  if ((U->S[i]=nsp_matrix_create("pe",'r',1,size))== NULL) goto err;
	  memcpy(((NspMatrix *) U->S[i])->R, Out->R + ipb[i+1] - 1, size * sizeof(double));
	}
    }
  MoveObj(stack,1,NSP_OBJECT(Gcd));
  if ( lhs >= 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(U));
    }
  if ( lhs >= 3 ) 
    {
      if ( nsp_move_double(stack,3,err)== FAIL) goto err;
    }
  nsp_matrix_destroy(Out);
  nsp_matrix_destroy(Work);
  return Max(lhs,1);
 err:
  if ( Out != NULL) nsp_matrix_destroy(Out);
  if ( Work != NULL) nsp_matrix_destroy(Work);
  if ( Gcd != NULL) nsp_pmatrix_destroy(Gcd);
  if ( U != NULL) nsp_pmatrix_destroy(U);
  return RET_BUG;
}

extern int int_arl2(Stack stack, int rhs, int opt, int lhs);
extern int int_bdiag(Stack stack, int rhs, int opt, int lhs);
extern int int_dhinf(Stack stack, int rhs, int opt, int lhs);
extern int int_ereduc(Stack stack, int rhs, int opt, int lhs);
extern int int_freq(Stack stack, int rhs, int opt, int lhs);
extern int int_fstair(Stack stack, int rhs, int opt, int lhs);
extern int int_hinf(Stack stack, int rhs, int opt, int lhs);
extern int int_ldivp(Stack stack, int rhs, int opt, int lhs);
extern int int_ltitr(Stack stack, int rhs, int opt, int lhs);
extern int int_mucomp(Stack stack, int rhs, int opt, int lhs);
extern int int_sorder(Stack stack, int rhs, int opt, int lhs);
extern int int_sident(Stack stack, int rhs, int opt, int lhs);
extern int int_ppol(Stack stack, int rhs, int opt, int lhs);
extern int int_residu(Stack stack, int rhs, int opt, int lhs);
extern int int_rtitr(Stack stack, int rhs, int opt, int lhs);
extern int int_tzer(Stack stack, int rhs, int opt, int lhs);
extern int int_linmeq(Stack stack, int rhs, int opt, int lhs);
extern int int_findbd(Stack stack, int rhs, int opt, int lhs);


extern int int_corr(Stack stack, int rhs, int opt, int lhs);
extern int int_syredi (Stack stack, int rhs, int opt, int lhs);
extern int int_delip (Stack stack, int rhs, int opt, int lhs);
extern int int_amell (Stack stack, int rhs, int opt, int lhs);
extern int int_remez (Stack stack, int rhs, int opt, int lhs);
extern int int_rpem (Stack stack, int rhs, int opt, int lhs);
extern int int_simp (Stack stack, int rhs, int opt, int lhs);
extern int int_conv2 (Stack stack, int rhs, int opt, int lhs);
extern int int_sfact (Stack stack, int rhs, int opt, int lhs);
extern int int_contr(Stack stack, int rhs, int opt, int lhs);

/*
 * The Interface for basic matrices operation 
 */

static OpTab Control_func[] = {
  {"arl2_ius", int_arl2},
  {"bdiag", int_bdiag},
  {"dhinf", int_dhinf},
  {"ereduc", int_ereduc},
  {"freq", int_freq},
  {"fstair", int_fstair},
  {"hinf", int_hinf},
  {"ldivp", int_ldivp},
  {"ltitr", int_ltitr},
  {"mucomp", int_mucomp},
  {"sorder", int_sorder},
  {"sident", int_sident},
  {"ppol", int_ppol},
  {"residu", int_residu},
  {"rtitr", int_rtitr},
  {"tr_zer", int_tzer},
  {"corr", int_corr},
  {"syredi", int_syredi},
  {"delip", int_delip},
  {"amell", int_amell},
  {"remez", int_remez},
  {"simp", int_simp},
  {"conv2", int_conv2},
  {"sfact", int_sfact},
  {"bezout", int_bezout},
  {"contr", int_contr},
  {"linmeq", int_linmeq},
  {"findBD", int_findbd},
  /*
  {"rpem", int_rpem},
  */
  {(char *) 0, NULL}
};

int
Control_Interf (int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Control_func[i].fonc)) (stack, rhs, opt, lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void
Control_Interf_Info (int i, char **fname, function (**f))
{
  *fname = Control_func[i].name;
  *f = Control_func[i].fonc;
}

