#include <nsp/nsp.h>
#include <nsp/object.h> 
#include <nsp/matrix.h>
#include <nsp/smatrix.h> 
#include <nsp/nsp_lapack.h> 
#include <nsp/interf.h>

#include "slicot.h"

/* WIP : */
  
/* interface for mucomp 
 *     [bound,D,G] = mucomp(Z,K,T)
 *    [bound,D] = mucomp(Z,K,T)
 *    bound = mucomp(Z,K,T)
 * slicot routines 
 * XXXX: Z is a 'z' matrix ? 
 */

int int_mucomp(Stack stack, int rhs, int opt, int lhs)
{
  int  Rwork_size , Zwork_size, info=0;
  NspMatrix *Z=NULL,*Bound=NULL,*D=NULL,*G=NULL,*K=NULL,*T=NULL,
    *X=NULL,*Iwork=NULL,*Rwork=NULL,*Zwork=NULL;
  CheckRhs(3,3);
  CheckLhs(1,3);
  if(( Z=GetMatCopy(stack,1)) ==NULL) return RET_BUG;
  CheckSquare(NspFname(stack),1,Z);
  if( Z->n == 0)
    {
      if((Bound=nsp_matrix_create( NVOID,'r', Z->n, 1)) == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(Bound));
      if(lhs >= 2)
	{
	  if((D=nsp_matrix_create( NVOID,'r', Z->n, 1)) == NULL) return RET_BUG;
	  MoveObj(stack,1,NSP_OBJECT(D));
	}
      if(lhs >= 3)
	{
	  if((G=nsp_matrix_create( NVOID,'r', Z->n, 1)) == NULL) return RET_BUG;
	  MoveObj(stack,1,NSP_OBJECT(G));
	}
      return Max(lhs,1);
    }
  
  if ( Z->rc_type == 'r' )
    {
      if (nsp_mat_complexify(Z,0.0) == FAIL ) return RET_BUG;
    }
  
  if((K=GetMatCopyInt(stack,2)) == NULL) return RET_BUG;
  if((T=GetMatCopyInt(stack,3)) == NULL) return RET_BUG;
  CheckSameDims (NspFname(stack), 2, 3, K, T);
  if ((Bound=nsp_matrix_create( NVOID,'r', 1, 1)) == NULL) goto err;
  if ((D=nsp_matrix_create( NVOID,'r', Z->n, 1)) == NULL)  goto err;
  if((G=nsp_matrix_create( NVOID,'r', Z->n, 1))  == NULL) goto err; 
  if((X=nsp_matrix_create( NVOID,'r', 2*Z->n-1, 1)) == NULL) goto err;
  if((Iwork=nsp_matrix_create( NVOID,'r', 4*Z->n-2, 1)) == NULL) goto err; 
  Rwork_size = 2*Z->n*Z->n*Z->n + 9*Z->n*Z->n +  44*Z->n - 11;
  if((Rwork=nsp_matrix_create( NVOID,'r', Rwork_size, 1)) == NULL) goto err;
  Zwork_size = 6*Z->n*Z->n*Z->n + 12*Z->n*Z->n + 12*Z->n - 3;
  if((Zwork=nsp_matrix_create( NVOID,'c',1,Zwork_size)) == NULL) goto err; 
  
  /* WIP z is a zstk ? */

  nsp_slicot_ab13md( "N", &Z->n, Z->C, &Z->n, &K->mn, K->I, T->I,X->R,Bound->R, D->R, G->R, Iwork->I,
		     Rwork->R, &Rwork_size, Zwork->C, &Zwork_size, &info,1L );
  if(info != 0)
    {
      if ( info < 0 )
	Scierror("mucomp: the %d-th argument had an illegal value\n",-info);
      else
	{
	  switch ( info )
	    {
	    case 1:  Scierror("mucomp: the block sizes must be positive ints; \n");break;
	    case 2:  Scierror("mucomp: the sum of block sizes must be equal to N; \n");break;
	    case 3:  Scierror("mucomp: the size of a real block must be equal to 1; \n");break;
	    case 4:  Scierror("mucomp: the block type must be either 1 or 2; \n");break;
	    case 5:  Scierror("mucomp: errors in solving linear equations or in matrix inversion; \n");break;
	    case 6:  Scierror("mucomp: errors in computing eigenvalues or singular values. \n");break;
	    default:
	      Scierror("mucomp: failed with error %d\n",info);
	    }
	}
      goto err;
    }
  MoveObj(stack,1,NSP_OBJECT(Bound));
  if ( lhs >= 2)
    {
      MoveObj(stack,2,NSP_OBJECT(D));
    }
  else nsp_matrix_destroy(D);
    
  if ( lhs >= 3)
    {
      MoveObj(stack,3,NSP_OBJECT(G));
    }
  else  nsp_matrix_destroy(G);

  if (X!=NULL) nsp_matrix_destroy(X);
  if (Iwork!=NULL) nsp_matrix_destroy(Iwork);
  if (Rwork!=NULL) nsp_matrix_destroy(Rwork);
  if (Zwork!=NULL) nsp_matrix_destroy(Zwork);
  return Max(lhs,1);
 err:

  if (Bound!=NULL) nsp_matrix_destroy(Bound);
  if (D!=NULL) nsp_matrix_destroy(D);
  if (G!=NULL) nsp_matrix_destroy(G);
  if (X!=NULL) nsp_matrix_destroy(X);
  if (Iwork!=NULL) nsp_matrix_destroy(Iwork);
  if (Rwork!=NULL) nsp_matrix_destroy(Rwork);
  if (Zwork!=NULL) nsp_matrix_destroy(Zwork);

  return RET_BUG;
}


/* interface for hinf */
			  
int int_hinf(Stack stack, int rhs, int opt, int lhs)
{
  double eps = nsp_dlamch("eps");
  double tol = sqrt(eps);
  double gamma;
  int N, M, R, Q, info=0;
  NspMatrix *lA,*lB,*lC,*lD;
  NspMatrix *lAK,*lBK,*lCK,*lDK,*lRCOND;
  NspMatrix *Iwork,*lBWORK,*lDWORK;
  int NCON,NMEAS, LINTWORK, LWORK;
  CheckRhs(7,7);
  CheckLhs(4,5);
  
  if((lA=GetMatCopy(stack,1))==NULL) return RET_BUG;
  CheckSquare(NspFname(stack),1,lA);
  if((lB=GetMatCopy(stack,2))==NULL) return RET_BUG;

  if ( lB->m != lA->m )
    {
      Scierror("%s: The two first arguments mus have the same number of rows\n",NspFname(stack));
      return RET_BUG;
    }
  if((lC=GetMatCopy(stack,3))==NULL) return RET_BUG;
  if(  lC->n != lA->n )
    {
      Scierror("%s: The first and third arguments mus have the same number of columns\n",NspFname(stack));
      return RET_BUG;
    }
  if((lD=GetMatCopy(stack,4))==NULL) return RET_BUG;
  if(  lD->n != lB->n )
    {
      Scierror("%s: The second and fourth arguments mus have the same number of columns\n",NspFname(stack));
      return RET_BUG;
    }
  if(  lC->m != lD->m )
    {
      Scierror("%s: The third and fourth arguments mus have the same number of rows\n",NspFname(stack));
      return RET_BUG;
    }
  N = lA->m; M = lB->n; R = lC->m;

  if(N == 0  ||  M == 0  ||  R == 0)
    {
      if((lAK=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(lAK));
      if ( lhs >= 2 )
	{
	  if((lBK=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
	  MoveObj(stack,2,NSP_OBJECT(lBK));
	}
      if ( lhs >= 3 )
	{
	  if((lCK=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
	  MoveObj(stack,3,NSP_OBJECT(lCK));
	}
      if ( lhs >= 4 )
	{
	  if((lDK=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
	  MoveObj(stack,4,NSP_OBJECT(lDK));
	}
      if ( lhs >= 5 )
	{
	  if((lRCOND=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
	  MoveObj(stack,5,NSP_OBJECT(lRCOND));
	}
      return Max(lhs,1);
    }

  if (GetScalarInt (stack, 5, &NCON) == FAIL)  return RET_BUG;
  if (GetScalarInt (stack, 6, &NMEAS) == FAIL)  return RET_BUG;
  if (GetScalarDouble (stack, 7, &gamma) == FAIL)  return RET_BUG;
  
  if((lAK=nsp_matrix_create( NVOID,'r', N, N)) == NULL) return RET_BUG;
  if((lBK=nsp_matrix_create( NVOID,'r', N, NMEAS)) == NULL) return RET_BUG;
  if((lCK=nsp_matrix_create( NVOID,'r', NCON, N)) == NULL) return RET_BUG;
  if((lDK=nsp_matrix_create( NVOID,'r', NCON, NMEAS)) == NULL) return RET_BUG;
  if((lRCOND=nsp_matrix_create( NVOID,'r', 4, 1)) == NULL) return RET_BUG;
  LINTWORK = Max(2 * Max(NCON, N), Max(M, Max(NCON + NMEAS, N * N)));
  if((Iwork=nsp_matrix_create( NVOID,'r', LINTWORK, 1)) == NULL) return RET_BUG;
  if((lBWORK=nsp_matrix_create( NVOID,'r', 2*N, 1)) == NULL) return RET_BUG;
  Q = Max(M - NCON, Max(NCON, Max(R - NMEAS, NMEAS)));
  LWORK = 
    2 * Q * ( 3 * Q + 2 * N ) +
    Max(1,
	Max(( N + Q ) * ( N + Q + 6),
	    Max(Q * ( Q + Max(N, Max(Q, 5)) + 1),
		2 * N * (N + 2 * Q) + Max( 1,
					   Max(4 * Q * Q + Max(2 * Q, 3 * N * N + Max( 2 * N * Q, 10 * N * N + 12 * N + 5 )),
					       Q * ( 3 * N + 3 * Q + Max( 2 * N, 4 * Q + Max(N, Q))))))));
  LWORK *= 2;
  if((lDWORK=nsp_matrix_create( NVOID,'r',1, LWORK)) == NULL) return RET_BUG;
  
  nsp_slicot_sb10fd( &N, &M, &R, &NCON, &NMEAS, &gamma,
		     lA->R, &N, lB->R, &N, lC->R, &R, lD->R, &R, lAK->R, &N,
		     lBK->R, &N, lCK->R, &NCON, lDK->R,  &NCON, lRCOND->R, &tol,
		     Iwork->I, lDWORK->R, &LWORK, lBWORK->I, &info );
  if(info!=0)
    {
      if ( info < 0 )
	Scierror("mucomp: the %d-th argument had an illegal value\n",-info);
      else
	{
	  switch ( info )
	    {
	    default : Scierror("hinf: internal error %d\n",info);break;
	    case  1:  Scierror("hinf: the matrix [ A-j*omega*I, B2 ; C1, D12] had not full column rank in respect to the tolerance EPS; ");break;
	    case  2:  Scierror("hinf: the matrix [ A-j*omega*I, B1 ;C2, D21]  had not full row rank in respect to the tolerance EPS;");break;
	    case  3:  Scierror("hinf: the matrix D12 had not full column rank in respect to the tolerance TOL; ");break;
	    case  4:  Scierror("hinf: the matrix D21 had not full row rank in respect to the tolerance TOL; ");break;
	    case  5:  Scierror("hinf: the singular value decomposition (SVD) algorithm did not converge (when computing the SVD of one of the matrices [A   B2; C1  D12 ], [A   B1; C2  D21 ], D12 or D21).");break;
	    case  6:  Scierror("hinf: the controller is not admissible (too small value of gamma); ");break;
	    case  7:  Scierror("hinf: the X-Riccati equation was not solved successfully (the controller is not admissible or there are numerical difficulties);");break;
	    case  8:  Scierror("hinf: the Y-Riccati equation was not solved successfully (the controller is not admissible or there are numerical difficulties);");break;
	    case  9:  Scierror("hinf: the determinant of Im2 + Tu*D11HAT*Ty*D22 is zero [3]. ");break;
	    }
	}
      return RET_BUG;
    }      

  MoveObj(stack,1,NSP_OBJECT(lAK));
  MoveObj(stack,2,NSP_OBJECT(lBK));
  MoveObj(stack,3,NSP_OBJECT(lCK));
  MoveObj(stack,4,NSP_OBJECT(lDK));
  if(lhs >= 5) MoveObj(stack,5,NSP_OBJECT(lRCOND));
  return Max(lhs,1);
}

/* interface for dhinf 
 *     [Ak,Bk,Ck,Dk,RCOND]=dhinf(A,B,C,D,ncon,nmeas,gamma)
 */

int int_dhinf(Stack stack, int rhs, int opt, int lhs)		     
{
  double eps = nsp_dlamch("eps");
  double tol = sqrt(eps);
  double gamma;
  int N, M, R, Q, info=0;
  NspMatrix *lA,*lB,*lC,*lD;
  NspMatrix *lAK,*lBK,*lCK,*lDK,*lRCOND,*lX,*Z;
  NspMatrix *Iwork,*lBWORK,*lDWORK;
  int NCON,NMEAS, LINTWORK, LWORK;

  CheckRhs(7,7);
  CheckLhs(4,5);
  if((lA=GetMatCopy(stack,1))==NULL) return RET_BUG;
  CheckSquare(NspFname(stack),1,lA);
  if((lB=GetMatCopy(stack,2))==NULL) return RET_BUG;

  if ( lB->m != lA->m )
    {
      Scierror("%s: The two first arguments mus have the same number of rows\n",NspFname(stack));
      return RET_BUG;
    }
  if((lC=GetMatCopy(stack,3))==NULL) return RET_BUG;
  if(  lC->n != lA->n )
    {
      Scierror("%s: The first and third arguments mus have the same number of columns\n",NspFname(stack));
      return RET_BUG;
    }

  if((lD=GetMatCopy(stack,4))==NULL) return RET_BUG;
  if(  lD->n != lB->n )
    {
      Scierror("%s: The second and fourth arguments mus have the same number of columns\n",NspFname(stack));
      return RET_BUG;
    }
  if(  lC->m != lD->m )
    {
      Scierror("%s: The third and fourth arguments mus have the same number of rows\n",NspFname(stack));
      return RET_BUG;
    }

  N = lA->m; M = lB->n; R = lC->m;

  if(N == 0  ||  M == 0  ||  R == 0)
    {
      if((lAK=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
      MoveObj(stack,1,NSP_OBJECT(lAK));
      if ( lhs >= 2 )
	{
	  if((lBK=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
	  MoveObj(stack,2,NSP_OBJECT(lBK));
	}
      if ( lhs >= 3 )
	{
	  if((lCK=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
	  MoveObj(stack,3,NSP_OBJECT(lCK));
	}
      if ( lhs >= 4 )
	{
	  if((lDK=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
	  MoveObj(stack,4,NSP_OBJECT(lDK));
	}
      if ( lhs >= 5 )
	{
	  if((lRCOND=nsp_matrix_create( NVOID,'r', 0, 0)) == NULL) return RET_BUG;
	  MoveObj(stack,5,NSP_OBJECT(lRCOND));
	}
      return Max(lhs,1);
    }

  if (GetScalarInt (stack, 5, &NCON) == FAIL)  return RET_BUG;
  if (GetScalarInt (stack, 6, &NMEAS) == FAIL)  return RET_BUG;
  if (GetScalarDouble (stack, 7, &gamma) == FAIL)  return RET_BUG;

  if((lAK=nsp_matrix_create( NVOID,'r', N, N)) == NULL) return RET_BUG;
  if((lBK=nsp_matrix_create( NVOID,'r', N, NMEAS)) == NULL) return RET_BUG;
  if((lCK=nsp_matrix_create( NVOID,'r', NCON, N)) == NULL) return RET_BUG;
  if((lDK=nsp_matrix_create( NVOID,'r', NCON, NMEAS)) == NULL) return RET_BUG;
  if((lX=nsp_matrix_create( NVOID,'r', N, N)) == NULL) return RET_BUG;
  if((Z=nsp_matrix_create( NVOID,'r', N, N)) == NULL) return RET_BUG;
  if((lRCOND=nsp_matrix_create( NVOID,'r', 8, 1)) == NULL) return RET_BUG;
  LINTWORK = Max(Max(2*Max(NCON,N),M),Max(NCON+NMEAS,N*N));
  if((Iwork=nsp_matrix_create( NVOID,'r', LINTWORK, 1)) == NULL) return RET_BUG;
  if((lBWORK=nsp_matrix_create( NVOID,'r', 2*N, 1)) == NULL) return RET_BUG;
  
  Q = Max( Max(M - NCON, NCON), Max( R - NMEAS, NMEAS));
  LWORK = Max((N+Q)*(N+Q+6),
	      13*N*N + M*M + 2*Q*Q + N*(M+Q)
	      +Max(M*(M+7*N),2*Q*(8*N+M+2*Q)) + 6*N
	      + Max(Max(14*N+23,16*N),Max(2*N+Max(M,2*Q),3*Max(M,2*Q))));
  LWORK *= 2;
  if((lDWORK=nsp_matrix_create( NVOID,'r',1,LWORK)) == NULL) return RET_BUG;

  nsp_slicot_sb10dd( &N, &M, &R, &NCON, &NMEAS, &gamma, lA->R, &N,
		     lB->R, &N, lC->R, &R, lD->R, &R, lAK->R,
		     &N, lBK->R, &N, lCK->R, &NCON, lDK->R,
		     &NCON, lX->R, &N, Z->R, &N, lRCOND->R, &tol,
		     Iwork->I, lDWORK->R, &LWORK, lBWORK->I,
		     &info );
  if(info!=0)
    {
      Scierror("hinf: internal error %d\n",info);
      return RET_BUG;
    }

  MoveObj(stack,1,NSP_OBJECT(lAK));
  MoveObj(stack,2,NSP_OBJECT(lBK));
  MoveObj(stack,3,NSP_OBJECT(lCK));
  MoveObj(stack,4,NSP_OBJECT(lDK));
  if(lhs >= 5) MoveObj(stack,5,NSP_OBJECT(lRCOND));
  return Max(lhs,1);
}
