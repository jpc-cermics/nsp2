#include <nsp/nsp.h>
#include <nsp/object.h> 
#include <nsp/matrix.h>
#include <nsp/smatrix.h> 
#include <nsp/nsp_lapack.h> 
#include <nsp/interf.h>

#include "slicot.h"
/* WIP : */

/* interface for mucomp 
 * [bound,D,G] = mucomp(Z,K,T)
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
      Scierror("%s: The two first arguments must have the same number of rows\n",NspFname(stack));
      return RET_BUG;
    }
  if((lC=GetMatCopy(stack,3))==NULL) return RET_BUG;
  if(  lC->n != lA->n )
    {
      Scierror("%s: The first and third arguments must have the same number of columns\n",NspFname(stack));
      return RET_BUG;
    }
  if((lD=GetMatCopy(stack,4))==NULL) return RET_BUG;
  if(  lD->n != lB->n )
    {
      Scierror("%s: The second and fourth arguments must have the same number of columns\n",NspFname(stack));
      return RET_BUG;
    }
  if(  lC->m != lD->m )
    {
      Scierror("%s: The third and fourth arguments must have the same number of rows\n",NspFname(stack));
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
      Scierror("%s: The two first arguments must have the same number of rows\n",NspFname(stack));
      return RET_BUG;
    }
  if((lC=GetMatCopy(stack,3))==NULL) return RET_BUG;
  if(  lC->n != lA->n )
    {
      Scierror("%s: The first and third arguments must have the same number of columns\n",NspFname(stack));
      return RET_BUG;
    }

  if((lD=GetMatCopy(stack,4))==NULL) return RET_BUG;
  if(  lD->n != lB->n )
    {
      Scierror("%s: The second and fourth arguments must have the same number of columns\n",NspFname(stack));
      return RET_BUG;
    }
  if(  lC->m != lD->m )
    {
      Scierror("%s: The third and fourth arguments must have the same number of rows\n",NspFname(stack));
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

/* contr */

int int_contr(Stack stack, int rhs, int opt, int lhs)
{ 
  int LDA,LDV;
  int INFO, INDCON, Nb_cont;
  char  *JOBU, *JOBV;
  double tol;
  int M,N,work_size;
  NspMatrix *A=NULL,*B=NULL,*Work=NULL,*IWork=NULL,*Kstair=NULL,*U=NULL,*V=NULL,*Ncont=NULL,
    *Ac=NULL,*Bc=NULL;
  
  /*     [NCONT,U,KSTAIR,V,A,B]=ab01od(A,B,[TOL])   */

  CheckRhs(1,3);
  CheckLhs(1,6);

  tol = nsp_dlamch("eps");
  if((A=GetMat(stack,1))==NULL) return RET_BUG;
  CheckSquare(NspFname(stack),1,A);
  if((B=GetMat(stack,2))==NULL) return RET_BUG;
  if ( B->m != A->n )
    {
      Scierror("%s: The second argument should be of size %dxn\n",A->m);
      return RET_BUG;
    }
  N=A->m;
  tol=0.2*sqrt(2*tol)*N;
  M=B->n;
  if ( rhs == 3)
    {
      if (GetScalarDouble (stack, 3, &tol) == FAIL)  return RET_BUG;
      if (tol>1.0||tol<0.0)
	{
	  Scierror("Error: tol must be in [0 1]\r\n");
	  return RET_BUG;
	}
    }
  /*     dimensions...    */
  LDA=Max(1,A->m);
  LDV=Max(1,B->n);
  work_size = Max(1, N*M + Max(N,M) + Max(N,3*M));
  if (( Ac = (NspMatrix *) nsp_object_copy(NSP_OBJECT(A)))== NULL)  return RET_BUG;
  if (( Bc = (NspMatrix *) nsp_object_copy(NSP_OBJECT(B)))== NULL)  return RET_BUG;
  
  JOBU= (lhs >= 2) ? "I": "N";
  JOBV= (lhs >= 4) ? "I": "N"; 
  /*     creating NCONT,U,KSTAIR,V,IWORK,DWORK   */
  if ((Ncont= nsp_matrix_create(NVOID,'r', 1, 1))==NULL) goto err; 
  if ((U=nsp_matrix_create(NVOID,'r', N, N))==NULL) goto err; 
  if ((Kstair = nsp_matrix_create(NVOID,'r', 1, N ))==NULL) goto err;
  if ((V=nsp_matrix_create(NVOID,'r', M, M ))==NULL) goto err;
  if ((IWork= nsp_matrix_create(NVOID,'r', 1, M ))==NULL) goto err;
  if ((Work = nsp_matrix_create(NVOID,'r', 1, work_size)) ==NULL) goto err;
      
  nsp_slicot_ab01od( "A", JOBU, JOBV, &N, &M, Ac->R, &LDA,
		     Bc->R, &LDA, U->R, &LDA, V->R, &LDV, 
		     &Nb_cont, &INDCON, Kstair->I, &tol,  
		     IWork->I,Work->R, &work_size, &INFO,1L, 1L,1L );
  if (INFO != 0)
    {
      Scierror("Error: internal error in %s\n","ab01od");
      return RET_BUG;
    }
  
  Ncont->R[0]= Nb_cont;
  if (lhs >= 3)
    {
      Kstair->convert = 'i'; Kstair=Mat2double(Kstair);
      nsp_matrix_resize(Kstair,1,INDCON);
    }

  MoveObj(stack,1,NSP_OBJECT(Ncont));
  if ( lhs >= 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(U));
    }
  else
    {
      nsp_matrix_destroy(U);
    }
  if ( lhs >= 3 )
    {
      MoveObj(stack,3,NSP_OBJECT(Kstair));
    }
  else
    {
      nsp_matrix_destroy(Kstair);
    }
  if ( lhs >= 4 )
    {
      MoveObj(stack,4,NSP_OBJECT(V));
    }
  else
    {
      nsp_matrix_destroy(V);
    }
  if ( lhs >= 5 )
    {
      MoveObj(stack,5,NSP_OBJECT(Ac));
    }
  else
    {
      nsp_matrix_destroy(Ac);
    }
  if ( lhs >= 6 )
      {
      MoveObj(stack,6,NSP_OBJECT(Bc));
    }
  else
    {
      nsp_matrix_destroy(Bc);
    }

  return Max(1,lhs);
 err:
  return RET_BUG;
}

/*
 *  FINDBD.F - Gateway function for computing the initial state and/or
 *             the matrices B and D of a discrete-time linear system,
 *             given the system matrices A, C, and possibly B, D, and the
 *             input and output trajectories, using SLICOT routine IB01CD.
 */

#define COPY(src,dst,nb) C2F(dcopy)(&nb,src,&one,dst,&one)

int int_findbd(Stack stack, int rhs, int opt, int lhs)
{
  int one =1;
  double TOL ;
  int INFO ,IWARN ,L ,LDA ,LDB ,LDC ,LDD ,LDU ,LDV ,LDWORK , LDY ,M ,N ,NSMP ;
  char *COMUSE ,*JOB , *JOBX0 ;
  NspMatrix *IWORK;
  NspMatrix *A,  *B,  *C,  *D,  *DWORK,  *U,  *V,  *X0,  *Y;
  NspMatrix *Ac,  *Bc,  *Cc,  *Dc, *Uc, *Yc;
  int CUSE ,IC ,IJOB ,INI ,IP ,IPS ,IQ ,ISIZE ,ITMP ,
    LDW1 ,LDW2 ,LDW3 ,LDWMIN ,LIWORK ,MA ,MINSMP ,MINWLS ,NA ,
    NCOL ,NCP1 ,TASK ;
  int PRINTW ;
  const int CSIZE =64000;
  
  CheckRhs(2,11);
  CheckLhs(0,5);
  
  //   jobx0
  if (GetScalarInt (stack, 1, &TASK) == FAIL)  return RET_BUG;
  if (TASK < 0 ||TASK > 1 )
    {
      Scierror ("Error: JOBX0 HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES");
    }
  JOBX0 = (TASK == 1 ) ? "X" : "N";
  // comuse
  if (GetScalarInt (stack, 2, &CUSE) == FAIL)  return RET_BUG;
  if (CUSE < 1 ||CUSE > 3 )
    {
      Scierror ("Error: COMUSE HAS 1, 2, OR 3 THE ONLY ADMISSIBLE VALUES");
    }
  COMUSE = (CUSE == 1 ) ? "C" : ( (CUSE == 2 ) ? "U" : "N");
  
  if (TASK == 1 && CUSE == 3 )
    {
      if (rhs < 5 )
	{
	  Scierror ("Error: findbd requires at least 5 input arguments");
	}
    }
  else
    {
      if (CUSE == 1 )
	{
	  if (rhs < 6 )
	    {
	      Scierror ("Error: findbd requires at least 6 input arguments");
	    }
	}
    }
  //   job
  if ((TASK == 1 && CUSE == 2 )||CUSE == 1 )
    {
      if (GetScalarInt (stack, 3, &IJOB) == FAIL)  return RET_BUG;
      if (IJOB < 1 ||IJOB > 2 ){
	Scierror ("Error: JOB HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES");
      }
      IP =4;
      if (IJOB == 1 ){
	JOB ="B";
      } else {
	JOB ="D";
      }
    }
  else {
    IP =3;
    JOB ="B";
    IJOB =1;
  }

  if (TASK == 1 && CUSE == 2 )
    {
      if (IJOB == 1 && rhs < 7 ){
	Scierror ( "Error: FINDBD REQUIRES AT LEAST 7 INPUT ARGUMENTS");
      }
      else if (IJOB == 2 && rhs < 8 )
	{
	  Scierror ( "Error: FINDBD REQUIRES AT LEAST 8 INPUT ARGUMENTS");
	}
    }
  /* A */
  IPS =IP;
  N =0;
  M =0;
  L =1;
  NSMP =1;
  if (rhs >= IP )
    {
      if(( A=GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
      CheckSquare(NspFname(stack),1,A);
      N = A->m ; NA = A->n;
      IP =IP +1;
    }
  //   B(n,m)
  if (TASK == 1 && CUSE == 2 && rhs >= IP )
    {
      if(( B=GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
      NA = B->m;
      M =  B->n;
      if (NA != N )
	{
	  Scierror("Error: B MUST HAVE %d ROWS",N);
	}
      IP =IP +1;
    }

  //   C(l,n)
  if (rhs >= IP )
    {
      if(( C=GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
      L = C->m ;
      NA = C->n;
      if (NA != N )
	{
	  Scierror("Error: C MUST HAVE %d COLUMNS",N);
	}
      IP =IP +1;
      if (L <= 0 )
	{
	  Scierror ("Error: THE SYSTEM HAS NO OUTPUTS");
	}
    }
  //   D(l,m)
  if (TASK == 1 && CUSE == 2 && IJOB == 2 && rhs >= IP )
    {
      if(( D=GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
      NA = D->m;
      MA = D->n;
      if (NA != L )
	{
	  Scierror("Error: D MUST HAVE %d ROWS", L );
	}
      if (MA != M )
	{
	  Scierror("D MUST HAVE %d COLUMNS", M );
	}
      IP =IP +1;
    }
  //  Y(txp), U(txm)
  if (rhs >= IP )
    {
      if(( Y =GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
      NSMP = Y->m;
      if ( Y->n != L )
	{
	  Scierror(" Y MUST HAVE %d COLUMNS",L);
	}
      IP =IP +1;
    }
  if (((TASK == 1 && CUSE == 2 )||CUSE == 1 ) && rhs >= IP )
    {
      if(( U =GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
      MA = U->n;
      if (CUSE == 2 && MA != M )
	{
	  Scierror("Error: U MUST HAVE %d COLUMNS",M);
	} else {
	M =MA;
      }
      if (M > 0 ){
	if ( U->m != NSMP ){
	  Scierror ("Error: U MUST HAVE THE SAME ROW DIMENSION AS Y");
	}
      }
      IP =IP +1;
    }
  if (CUSE == 1 )
    {
      NCOL =N *M;
      if (TASK == 1 )NCOL =NCOL +N;
      MINSMP =NCOL;
      if (IJOB == 2 )
	{
	  MINSMP =MINSMP +M;
	  IQ =MINSMP;
	}
      else
	{
	  if (TASK == 0 ){
	    IQ =MINSMP;
	    MINSMP =MINSMP +1;
	  } else {
	    IQ =MINSMP;
	  }
	}
    }
  else
    {
      NCOL =N;
      if (TASK == 1 ){
	MINSMP =N;
      } else {
	MINSMP =0;
      }
      IQ =MINSMP;
    }
  if (NSMP < MINSMP ){
    Scierror("Error:  THE NUMBER OF SAMPLES SHOULD BE AT LEAST %d", MINSMP);
  }
  //     tol
  if ( rhs >= IP)
    {
      if (GetScalarDouble (stack, IP, &TOL) == FAIL)  return RET_BUG;
      IP =IP +1;
    }
  //     printw
  PRINTW =FALSE;
  if ( rhs >= IP)
    {
      if (GetScalarInt (stack, IP , &PRINTW) == FAIL)  return RET_BUG;
      if ( PRINTW < 0 || PRINTW > 1 )
	{ 
	  Scierror ("PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES");
	  return RET_BUG;
	} 
      IP =IP +1;
    }
  
  // Determine the lenghts of working arrays.
  // The default value for LDWORK is computed using the formula
  //      LDWORK = Max( minimum value needed, 2*CSIZE/3,
  //                   CSIZE - ( M + L )*NSMP - 2*N*( N + M + L ) - L*M )
  // where CSIZE is the cache size in double precision words.;
  // If LDWORK is specified,
  //        but it is less than the minimum workspace size
  // needed, that minimum value is used instead.

  LDA =Max (1,N );
  LDB =LDA;
  LDC =L;
  LDD =LDC;
  LDV =LDA;
  LDY =Max (1,NSMP );
  if (M > 0 && ((TASK == 1 && CUSE == 2 )||CUSE == 1 )){
    LDU =LDY;
  } else {
    LDU =1;
  }

  LIWORK =NCOL;
  if ((TASK == 0 && CUSE != 1 )||Max (N ,M ) == 0){
    LDWORK =2;
  } else {
    if (IJOB == 2 ) LIWORK =Max (LIWORK ,M );
    IQ =IQ *L;
    NCP1 =NCOL +1;
    ISIZE =NSMP *L *NCP1;
    if (N > 0 && TASK == 1 ){
      if (CUSE == 1 ){
	IC =2*N *N +N;
      } else {
	IC =2*N *N;
      }
    } else {
      IC =0;
    }
    MINWLS =NCOL *NCP1;
    if (CUSE == 1 )
      {
	int IA;
      if (IJOB == 2 )MINWLS =MINWLS +L *M *NCP1;
      if (M > 0 && IJOB == 2 ){
	IA =M +Max (2*NCOL ,M );
      } else {
	IA =2*NCOL;
      }
      LDW1 =N *N *M +Max (IC ,IA );
      if (TASK == 1 )LDW1 =LDW1 +L *N;
      LDW2 =ISIZE +Max (N +Max (IC ,IA ),6*NCOL );
      LDW3 =MINWLS +Max (IQ *NCP1 +LDW1 ,6*NCOL );
      if (M > 0 && IJOB == 2 ){
	LDW2 =Max (LDW2 ,ISIZE +2*M *M +6*M );
	LDW3 =Max (LDW3 ,MINWLS +2*M *M +6*M );
	INI =3;
      } else {
	INI =2;
      }
    } else {
      int ITAU =IC +L *N;
      LDW2 =ISIZE +2*N +Max (IC ,4*N );
      LDW3 =MINWLS +2*N +Max (IQ *NCP1 +ITAU ,4*N );
      INI =2;
    }
    LDWMIN =INI +N *(N +M +L )+ Max(5*N ,Max(INI ,Min (LDW2 ,LDW3 )));
    LDWORK =Max (LDWMIN ,
		 Max(2*CSIZE /3,
		     CSIZE -(M +L )*NSMP -2*N *(N + M +L )-L *M ));
  }
  //   ldwork
  //
  if (rhs >= IP )
    {
      if (GetScalarInt (stack, IP , &ITMP) == FAIL)  return RET_BUG;
      if (ITMP < LDWMIN ){
	LDWORK =LDWMIN;
      } else {
	LDWORK =ITMP;
      }
    }

  // Allocate variable dimension local arrays.
  
  if ((Ac = nsp_matrix_create(NVOID,'r',LDA , N ))==NULL) goto err;
  if ((Bc = nsp_matrix_create(NVOID,'r',LDB , M ))==NULL) goto err;
  if ((Cc = nsp_matrix_create(NVOID,'r',LDC , N ))==NULL) goto err;
  if ((Dc = nsp_matrix_create(NVOID,'r',LDD , M ))==NULL) goto err;
  if ((DWORK = nsp_matrix_create(NVOID,'r',1, LDWORK ))==NULL) goto err;
  if ((IWORK = nsp_matrix_create(NVOID,'r',1, LIWORK))==NULL) goto err;
  if ((Uc = nsp_matrix_create(NVOID,'r',LDU , M ))==NULL) goto err;
  if ((V = nsp_matrix_create(NVOID,'r',LDV , N ))==NULL) goto err;
  if ((X0 = nsp_matrix_create(NVOID,'r',N ,1))==NULL) goto err;
  if ((Yc = nsp_matrix_create(NVOID,'r',LDY , L))==NULL) goto err;
    
  // Copy inputs from MATLAB workspace to locally allocated arrays.
	
  IP =IPS;
  if (rhs >= IP )
    { 
      C2F(dcopy)(&A->mn, A->R, &one, Ac->R, &one);
      if (TASK == 1 && CUSE == 2)
        {
	  if (N > 0)
            {
	      C2F(dcopy)(&B->mn, B->R, &one, Bc->R, &one);
            }
        }
      C2F(dcopy)(&C->mn, C->R, &one, Cc->R, &one);
      if (TASK == 1 && CUSE == 2 && IJOB == 2)
        {
	  C2F(dcopy)(&D->mn, D->R, &one, Dc->R, &one);
        }
      
      C2F(dcopy)(&Y->mn, Y->R, &one, Yc->R, &one);
      if (CUSE == 1 || (TASK == 1 && CUSE == 2))
        {
	  C2F(dcopy)(&U->mn, U->R, &one, Uc->R, &one);
        }
    } 
  // Do the actual computations.
  nsp_slicot_ib01cd (JOBX0 ,COMUSE ,JOB ,&N ,&M ,&L ,&NSMP ,Ac->R,&LDA ,Bc->R,
	  &LDB ,Cc->R,&LDC ,Dc->R,&LDD ,Uc->R,&LDU ,Yc->R,&LDY ,X0->R,
		     V->R,&LDV ,&TOL , IWORK->I,DWORK->R,&LDWORK ,&IWARN ,&INFO ,1L,1L,1L);
  if (IWARN != 0 && PRINTW )
    { 
      Sciprintf("  IWARN = %d ON EXIT FROM IB01CD", IWARN );
    }
  
  if (INFO != 0 )
    { 
      Scierror("INFO = %d ON EXIT FROM IB01CD",INFO);
      return RET_BUG;
    }
  int iPos = 1;

  if (TASK == 1 || (TASK == 0 && CUSE == 2))
    {
      MoveObj(stack,iPos,NSP_OBJECT(X0));
      iPos++;
    }
  
  if (CUSE == 1 && lhs >= iPos)
    {
      MoveObj(stack,iPos,NSP_OBJECT(B));
      iPos++;
      if ( lhs >= iPos)
        {
	  MoveObj(stack,iPos,NSP_OBJECT(D));
	  iPos++;
        }
    }
  
  if ((TASK == 1 || CUSE == 1) && lhs >= iPos)
    {
      MoveObj(stack,iPos,NSP_OBJECT(V));
      iPos++;
    }

  if (lhs >= iPos)
    {
      NspMatrix *X;
      int NO = 1;
      if (CUSE == 1 && M > 0 && IJOB == 2)
        {
	  NO = 2;
        }
      if ((X = nsp_matrix_create(NVOID,'r',NO,1))== NULL) return RET_BUG;
      COPY (DWORK->R+1, X->R, NO );
      MoveObj(stack,iPos,NSP_OBJECT(X));
    }
  return Max(lhs,1);
 err:
  return RET_BUG;
}

// ORDER.F  - Gateway function for computation of the order of a
//            discrete-time system using SLICOT routine IB01AD.
//
// RELEASE 4.0, WGS COPYRIGHT 2000.
//
// Matlab call:
//   [R(,n,sval,rcnd)] = order(meth,alg,jobd,batch,conct,s,Y(,U,tol,
//                             printw,ldwork,R))
// Purpose:
//   To preprocess the input-output data for estimating the matrices 
//   of a linear time-invariant dynamical system, using Cholesky or
//   QR factorization and subspace identification techniques (MOESP 
//   and N4SID), and to find an estimate of the system order. The
//   input-output data can, optionally, be processed sequentially.

int int_sorder(Stack stack, int rhs, int opt, int lhs)
{
  int one=1;
  static const double ZERO =0.0,ONE =1.0;
  // .. Scalar parameters used by SLICOT subroutines ..
  double TOL1 ,TOL2 ;
  int INFO ,IWARN ,L ,LDR ,LDU ,LDWORK ,LDY ,M ,N ,NOBR ,NSMP ;
  char *ALG , *BATCH , *CONCT , *CTRL , *JOBD , *METH ;
  // .. Allocatable arrays ..
  // !Fortran 90/95 (Fixed dimensions should be used with Fortran 77.)
  NspMatrix *IWORK, *DWORK, *R=NULL, *SV, *U, *Y;
  NspMatrix *Rc, *Uc, *Yc;
  // .. Local variables and constant dimension arrays ..
  int IALG ,IBCH ,ICNC ,IJOB ,IMTH ,IP ,ITMP ,
    LDWMIN ,LIWORK ,NCOL ,NR ,NRSAVE ,NSMPMN ;
  int PRINTW ;

  // .. Cache size in double precision words ..;
  static const int CSIZE =64000;

  // For Windows only!
  // This resets the floating point exception to allow divide by zero,
  // overflow and invalid numbers. 
  //     int(2)        CONTROL
  //     GETCONTROLFPQQ( CONTROL )
  //     CONTROL = CONTROL  || FPCWZERODIVIDE
  //     CONTROL = CONTROL  || FPCWINVALID
  //     CONTROL = CONTROL  || FPCWOVERFLOW
  //     SETCONTROLFPQQ( CONTROL )
  
  // Check for proper number of arguments.
  if (rhs < 7 ){ 
    Scierror ("ORDER REQUIRES AT LEAST 7 INPUT ARGUMENTS") ;
  }
  else
    {
      if (lhs < 1 )
	{ 
	  Scierror ("ORDER REQUIRES AT LEAST 1 OUTPUT ARGUMENTS");
	}
    }

  // meth
  if (GetScalarInt (stack, 1, &IMTH) == FAIL)  return RET_BUG;
  if (IMTH < 1 ||IMTH > 2 ){ 
    Scierror ("METH HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES");
  }
  METH = (IMTH == 1 ) ? "M" : "N";
  
  //   alg
  if (GetScalarInt (stack, 2, &IALG) == FAIL)  return RET_BUG;
  if (IALG < 1 ||IALG > 3 ){ 
    Scierror ("ALG HAS 1, 2 OR 3 THE ONLY ADMISSIBLE VALUES");
  } 
  ALG = (IALG == 1 ) ? "C" : ((IALG == 2 ) ? "F" : "Q");
  
  //   jobd
  if (GetScalarInt (stack, 3, &IJOB) == FAIL)  return RET_BUG;
  if (IJOB < 1 ||IJOB > 2 ){ 
    Scierror ("JOBD HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES");
  } 
  JOBD = (IJOB == 1 ) ? "M": "N";
  //   batch
  if (GetScalarInt (stack, 4, &IBCH) == FAIL)  return RET_BUG;
  if (IBCH < 1 ||IBCH > 4 ){ 
    Scierror ("BATCH HAS 1, 2, 3 OR 4 THE ONLY ADMISSIBLE VALUES");
  } 
  BATCH = (IBCH == 1 ) ? "F" : ((IBCH == 2 ) ? "I" : ((IBCH == 3 ) ? "L" : "O"));
  
  //   conct
  if (GetScalarInt (stack, 5, &ICNC) == FAIL)  return RET_BUG;
  if (ICNC < 1 ||ICNC > 2 ){ 
    Scierror ("CONCT HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES");
  } 
  CONCT = (ICNC == 1 ) ? "C": "N";

  //   ctrl is set to "N"
  CTRL ="N";
  //   s
  if (GetScalarInt (stack, 6, &NOBR) == FAIL)  return RET_BUG;
  if (NOBR < 1 ){ 
    Scierror ("S MUST BE A POSITIVE int");
  } 
  //   Y(txp), (U(txm))
  if(( Y =GetRealMatCopy(stack,7)) ==NULL) return RET_BUG;
  NSMP = Y->m;
  L = Y->n;
  if (L <= 0 ){ 
    Scierror ("THE SYSTEM HAS NO OUTPUTS");
  } 
  if (rhs >= 8 ){
    if(( U =GetRealMatCopy(stack,8)) ==NULL) return RET_BUG;
    M = U->n; 
  }
  else
    { 
      M =0;
    } 
  NR =2*(M +L )*NOBR ;
  if (IBCH <= 2 )
    { 
      NSMPMN =2*NOBR ;
    } else { 
    NSMPMN =NR +2*NOBR -1;
  } 
  if (NSMP < NSMPMN ){ 
    Scierror(" THE NUMBER OF SAMPLES SHOULD BE AT LEAST %d",NSMPMN);
  } 
  if (M > 0 )
    { 
      if ( U->m != NSMP )
	{ 
	  Scierror ("U MUST HAVE THE SAME ROW DIMENSION AS Y");
	} 
    } 
  //     tol
  //   
  TOL1 =ZERO ;
  TOL2 =-ONE; 
  if (rhs >= 9 )
    {
      NspMatrix *Tol;
      if(( Tol = GetRealMat(stack,9)) ==NULL) return RET_BUG;
      if ( Tol->mn > 2 )
	{
	  Scierror ("TOL MUST BE A VECTOR WITH AT MOST 2 ELEMENTS");
	}
      TOL1 = (Tol->mn > 0) ? Tol->R[0] : ZERO;
      TOL2 = (Tol->mn > 1) ? Tol->R[1] : -ONE;
    }
  PRINTW =FALSE;
  IP =10;
  NCOL =NR ;
  if (IBCH < 4 )
    { 
      if (ICNC == 1 )NCOL =NCOL +1;
      if (IALG == 2 )NCOL =NCOL +M +L +1;
    } 
  NRSAVE =(NCOL -NR )*NR ;
  //     printw
  //   
  if (rhs >= 10 )
    {
      if (GetScalarInt (stack,10, &PRINTW) == FAIL)  return RET_BUG;
      if ( PRINTW < 0 || PRINTW > 1 )
	{ 
	  Scierror ("PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES");
	  return RET_BUG;
	} 
    }
  
  // Determine the lenghts of working arrays.
  // The quasi-optimal value for LDWORK (assumed block-size 64) is possibly
  // modified, so that the sum of the lenghts of DWORK and other needed
  // arrays does not exceed the cache size. Specifically, the default value
  // for LDWORK is computed using the formulas
  //           nr = 2*( m + l )*s
  //           LDWORK = ( t - 2*s + 3 + 64 )*nr
  //           if ( CSIZE > Max( nr*nr + t*( m + l ) + 16, 2*nr ) ) then
  //              LDWORK = MIN( LDWORK, CSIZE - nr*nr - t*( m + l ) - 16 )
  //           else
  //              LDWORK = MIN( LDWORK, Max( 2*nr, CSIZE/2 ) )
  //           end if
  //           LDWORK = Max( minimum workspace size needed, LDWORK )
  // where CSIZE is the cache size in double precision words.;
  // If LDWORK is specified,
  //        but it is less than the minimum workspace size 
  // needed, that minimum value is used instead.

  LDY =NSMP ;
  if (M > 0 ){ 
    LDU =LDY; 
  } else { 
    LDU =1;
  } 
  LDR =NR ;
  if (IMTH == 1 && IJOB == 1 )LDR =Max (NR ,3*M *NOBR );
  if (IMTH == 2 )
    { 
      LIWORK =(M +L )*NOBR ;
    }
  else
    {
      if (IALG == 2 ){ 
	LIWORK =M +L ;
      } else { 
	LIWORK =1;
      }
    }
  //     The next statement is included in order to allow switching
  //     from Cholesky or fast QR to QR algorithm.

  LDWORK =(NSMP -2*NOBR +3+64)*NR ;
  if (CSIZE  > Max (NR *NR +NSMP *(M +L )+16,2*NR )){ 
    LDWORK =Min (LDWORK ,CSIZE -NR *NR -NSMP *(M +L )-16);
  } else { 
    LDWORK =Min (LDWORK ,Max (2*NR ,CSIZE /2));
  } 
  LDWMIN =2*NR *(NOBR +1);
  if (IALG == 2 )LDWMIN =Max (LDWMIN ,
			      Max(2*NR *(M +L +1)+NR ,NR *(M + L +3)));
  LDWORK =Max (LDWORK ,LDWMIN );
  //   ldwork
  //   
  if (rhs >= 11 )
    {
      if (GetScalarInt (stack, IP , &ITMP) == FAIL)  return RET_BUG;
      if (ITMP < LDWMIN )
	{ 
	  LDWORK =LDWMIN ;
	} else { 
	LDWORK =ITMP ;
      } 
      //     R(2*(m+p)*s,2*(m+p)*s(+c)), where
      //     c = 1,       if conct = 1 and batch < 4;
      //     c = 0,       if conct = 2 or  batch = 4;
      //     c = c+m+l+1, if  alg  = 2 and batch < 4.
      //
  
      if (rhs >= IP && (IBCH == 2 ||IBCH == 3 ))
	{
	  if(( R =GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
	  if ( R->m != NR )
	    { 
	      Scierror("R MUST HAVE %s ROWS",NR);
	    } 
	  if ( R->n  != NCOL ){ 
	    Scierror("R MUST HAVE %d COLUMNS",NCOL);
	  } 
	}
    }
  // Allocate variable dimension local arrays.
  // !Fortran 90/95
  
  if((Rc = nsp_matrix_create(NVOID,'r',LDR,NCOL)) == NULL) goto err;
  if((SV = nsp_matrix_create(NVOID,'r',L*NOBR,1)) == NULL) goto err;
  if((Uc = nsp_matrix_create(NVOID,'r',LDU,M)) == NULL) goto err;
  if((Yc = nsp_matrix_create(NVOID,'r',LDY,L)) == NULL) goto err;
  if((IWORK = nsp_matrix_create(NVOID,'r', 1,LIWORK)) == NULL) goto err;
  if((DWORK = nsp_matrix_create(NVOID,'r', 1,LDWORK)) == NULL) goto err;

  nsp_dset(&Rc->mn,&ZERO,Rc->R,&one);
  
  // Copy inputs from MATLAB workspace to locally allocated arrays.
  //      
  COPY ( Y->R,Yc->R, Y->mn);
  if (M > 0 )
   { 
     COPY ( U->R,Uc->R, U->mn);
   } 

  if ( R != NULL)
    { 
      COPY ( R->R, Rc->R, R->mn);
      if (IALG == 2 ||ICNC == 1 )
	{ 
	  if (LDR == NR )
	    { 
	      C2F(dcopy) (&NRSAVE, R->R+1-1 +(NR+1-1)*LDR,&one,DWORK->R,&one);
	    }
	  else
	    { 
	      C2F(dcopy) (&NRSAVE, R->R+NR*NR+1-1 +(1-1)*LDR,&one,DWORK->R,&one);
	    } 
	} 
    }
  
  // Do the actual computations.
  nsp_slicot_ib01ad (METH ,ALG ,JOBD ,BATCH ,CONCT ,CTRL ,&NOBR ,&M ,&L ,
	  &NSMP ,U->R,&LDU ,Y->R,&LDY ,&N ,Rc->R,&LDR ,SV->R,&TOL1 ,
		     &TOL2 , IWORK->I,DWORK->R,&LDWORK ,&IWARN ,&INFO, 1L,1L,1L, 1L,1L,1L );
  if (IWARN != 0 && PRINTW ){ 
    Scierror("  IWARN = %d ON EXIT FROM IB01AD",  IWARN);
  } 
  if (INFO != 0 ){ 
    Scierror("INFO = %d ON EXIT FROM IB01AD", INFO);
  } 

  // Copy output
  
  if ( LDR > NR ) C2F(dlacpy) ("FULL",&NR ,&NR ,Rc->R,&LDR ,R->R,&NR, 1L );

  if ((IALG == 2 ||ICNC == 1 ) && IBCH <= 2 ){ 
    if (LDR == NR ){ 
      C2F(dcopy) (&NRSAVE ,DWORK->R,&one,R->R+1-1 +(NR+1-1*LDR),&one);
    } else { 
      C2F(dcopy) (&NRSAVE ,DWORK->R,&one,R->R+NR*NR+1-1 +(1-1*LDR),&one);
    } 
  } 
  /* XXXXX */
  MoveObj(stack,1,NSP_OBJECT(Rc));
  if (IBCH > 2 )
    { 
      if (lhs >=2 )
	{
	  if ( nsp_move_double(stack,2,N)== FAIL) goto err;
	}
      if (lhs >= 3 )
	{
	  MoveObj(stack,3,NSP_OBJECT(SV));
	} 
      if (IMTH == 2 && lhs >= 4 )
	{
	  int nc=2;
	  NspMatrix *X;
	  if ((X=nsp_matrix_create(NVOID,'r',nc,1))== NULL) goto err;
	  COPY (DWORK->R+2-1, X->R, nc);
	  MoveObj(stack,4,NSP_OBJECT(X));
	} 
    }
  return Max(lhs,1);
 err:
  return RET_BUG;
}

// SIDENT.F - Gateway function for computation of a discrete-time
//            state-space realization (A,B,C,D) and Kalman gain
//            using SLICOT routine IB01BD.
// RELEASE 4.0, WGS COPYRIGHT 2000.
// Matlab call:  
//   [(A,C)(,B(,D))(,K,Q,Ry,S,rcnd)] = sident(meth,job,s,n,l,R(,tol,t,A,
//                                            C,printw))
// Purpose:
//   To compute a state-space realization (A,B,C,D) and the Kalman
//   predictor gain K of a discrete-time system, given the system
//   order and the relevant part of the R factor of the concatenated 
//   block-Hankel matrices, using subspace identification techniques 
//   (MOESP and N4SID).
// Contributor:
//   V. Sima, Research Institute for Informatics, Bucharest, Oct. 1999.
//   V. Sima, May 2000, July 2000.

int int_sident(Stack stack, int rhs, int opt, int lhs)
{
  int one=1, IAW;
  // .. Scalar parameters used by SLICOT subroutines ..
  double TOL = 0.0 ;
  int INFO ,IWARN ,L ,LDA ,LDB ,LDC ,LDD ,LDK ,LDO ,LDQ ,LDR ,
    LDRY ,LDS ,LDWORK ,M ,N ,NOBR ,NSMPL ;
  char  *JOB ,*JOBCK ,*METH ;
  NspMatrix *IWORK, *BWORK, *A, *B, *C, *D, *DWORK, *K, *Q,* R, *RY, *S;
  NspMatrix *Ac, *Cc, *Rc;
  int ID ,IJOB ,IP,LBWORK ,LDUNN ,LIWORK ,LL ,LNOBR ,MA ,
    MNOBR ,MNOBRN ,N2 ,NA ,NCOL ,NL ,NN ,NPL ,NR ,NRC ,TASK, PRINTW ;
  
  CheckRhs(6,11);
  CheckLhs(0,9);
  
  // meth
  if (GetScalarInt (stack, 1 , &TASK) == FAIL)  return RET_BUG;
  if (TASK < 1 ||TASK > 3 )
    {
      Scierror ("METH HAS 1, 2, OR 3 THE ONLY ADMISSIBLE VALUES");
    }
  METH = (TASK == 1 ) ? "M" : ((TASK == 2 ) ? "N": "C");
  // job
  if (GetScalarInt (stack, 2 , &IJOB) == FAIL)  return RET_BUG;
  if (IJOB < 1 ||IJOB > 4 ){ 
    Scierror ("JOB HAS 1, 2, 3 OR 4 THE ONLY ADMISSIBLE VALUES");
  }
  JOB = (IJOB == 1 ) ? "A": (IJOB == 2 ) ? "C" : (IJOB == 3 ) ? "B": "D";
  //        
  // s
  if (GetScalarInt (stack, 3, &NOBR) == FAIL)  return RET_BUG;
  if (NOBR < 1 ){ 
    Scierror ("S MUST BE A POSITIVE int");
  }
  // n
  if (GetScalarInt (stack, 4, &N) == FAIL)  return RET_BUG;
  if (N < 1 ){ 
    Scierror ("N MUST BE A POSITIVE int");
  } 
  if (N >= NOBR )
    { 
      Scierror("Error: THE ORDER SHOULD BE AT MOST %d\n",NOBR-1);
      return RET_BUG;
    } 
  // l
  if (GetScalarInt (stack, 5, &L) == FAIL)  return RET_BUG;
  if (L < 1 ){ 
    Scierror ("THE SYSTEM HAS NO OUTPUTS");
  } 
  // R(nr,nr)
  if(( R=GetRealMatCopy(stack,6)) ==NULL) return RET_BUG;
  NR = R->m;
  NCOL = R->n;
  if ( NR < 2 *L ){ 
    Scierror("R MUST HAVE AT LEAST %d ROWS", 2*L );
  } 
  if (NCOL < NR ){ 
    Scierror("R MUST HAVE AT LEAST %d COLUMNS", NCOL);
  } 
  //   m
  M =NR /(2*NOBR )-L ;
  //   tol
  if ( rhs >= 7 ){
    if (GetScalarDouble (stack, 7 , &TOL) == FAIL)  return RET_BUG;
  } 
  //   t
  NSMPL =0;
  JOBCK ="N";
  if (rhs >= 8 )
    { 
      JOBCK ="K";
      if (GetScalarInt (stack, 8, &NSMPL) == FAIL)  return RET_BUG;
      if (NSMPL != 0 && NSMPL < NR )
	{
	  Scierror("Error: the number of samples should be at least %d",NR);
	  return RET_BUG;
	}
      else if (NSMPL == 0 ){ 
	JOBCK ="N";
      } 
    }
  // A(n,n)
  if (TASK >= 2 && IJOB >= 3 )
    {
      if ( ! (rhs >= 10))
	{
	  Scierror("Error: rhs should be greater than 10 when meth >= 2 and job >=3");
	  return RET_BUG;
	}
      if(( A=GetRealMatCopy(stack,9)) ==NULL) return RET_BUG;
      CheckSquare(NspFname(stack),1,A);
      MA = A->m;
      NA = A->n;
      if (MA != N ||NA != N ){ 
	Scierror("Error: 9-th arguments should be of size %dx%d",N,N);
      } 
      //   C(l,n)
      if(( C=GetRealMatCopy(stack,10)) ==NULL) return RET_BUG;
      MA = C->m;
      NA = C->n;
      if (MA != L )
	{ 
	  Scierror("Error: 10-th argument must have %d rows",L );
	} 
      if (NA != N ){ 
	Scierror("Error: 10-th argument must have %d columns",N);
      } 
    }
  //     printw
  //   
  PRINTW = FALSE;
  if ( rhs >= 11 )
    {
      if (GetScalarInt (stack, 11 , &PRINTW) == FAIL)  return RET_BUG;
      if ( PRINTW < 0 || PRINTW > 1 )
	{ 
	  Scierror ("PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES");
	  return RET_BUG;
	}
    }
  
  // Determine the lenghts of working arrays.
  // The value for LDWORK is the minimum value needed by IB01BD for each
  // method and algorithm implemented.  Using a larger value could
  // increase the efficiency.
  
  MNOBR =M *NOBR ;
  LNOBR =L *NOBR ;
  MNOBRN =MNOBR +N ;
  LDUNN =(LNOBR -L )*N ;
  NPL =N +L ;
  N2 =N +N ;
  NN =N *N ;
  NL =N *L ;
  LL =L *L ;
  LDA =Max (1,N );
  LDB =LDA ;
  LDC =Max (1,L );
  LDD =LDC ;
  LDO =LNOBR ;
  LDR =NR ;
  if (NSMPL != 0 ){ 
    LDK =LDA ;
    LDQ =LDA ;
    LDS =LDA ;
    LDRY =LDC ;
    LBWORK =N2 ;
  } else { 
    LDK =1;
    LDQ =1;
    LDS =1;
    LDRY =1;
    LBWORK =1;
  } 
  LIWORK =MNOBR +N ;
  if (TASK == 1 )
    { 
      LIWORK =Max (LIWORK ,LNOBR );
    }
  else  if (TASK == 2 )
    { 
      LIWORK =Max (LIWORK ,M *NPL );
    }
  else
    { 
      LIWORK =Max (LIWORK , Max(LNOBR , M *NPL));
    }
  if (NSMPL > 0 )LIWORK =Max (LIWORK ,NN );
  
  IAW =0;
  LDWORK =LDUNN +4*N ;
  if (TASK == 1 ){ 
    ID =0;
  } else { 
    ID =N ;
  } 

  if (TASK != 2 ){ 
    if (IJOB <= 2 ){ 
      LDWORK =Max (LDWORK , Max(2*LDUNN +N2 , LDUNN +NN +7*N ));
    } 
  } 
  
  if ((M > 0 && IJOB != 2 )||TASK >= 2 ){ 
    LDWORK =Max (LDWORK ,2*LDUNN +NN +ID +7*N );
    if (TASK == 1 )LDWORK =Max (LDWORK ,
				Max(LDUNN +N +6*MNOBR ,
				    LDUNN + N +Max (L +MNOBR ,LNOBR +Max (3*LNOBR ,M ))));
  }
  else { 
    if (TASK != 2 ){IAW =N +NN ;
    } 
  }
  if (TASK != 1 ||NSMPL > 0 ){ 
    LDWORK =Max (LDWORK ,
		 Max(LDUNN +IAW +N2 +Max (5*N ,LNOBR +2*MNOBR + L ),
		     Max(ID +4*MNOBRN ,	 ID +MNOBRN +NPL )));
    if (TASK != 1 && M > 0 && IJOB != 2 )
      LDWORK =Max (LDWORK ,
		   MNOBR *NPL *(M *NPL +1)+Max (NPL *NPL,4*M *NPL +1));
    LDWORK =LNOBR *N +LDWORK ;
  } 

  if (NSMPL > 0 )
    LDWORK =Max (LDWORK , Max(4*NN +2*NL +LL +Max (3*L , NL ),14*NN +12*N +5));

  // Allocate variable dimension local arrays.
  // !Fortran 90/95
  
  if((A = nsp_matrix_create(NVOID,'r',LDA,N)) == NULL) goto err;;
  if((B = nsp_matrix_create(NVOID,'r',LDB,M)) == NULL) goto err;;
  if((C = nsp_matrix_create(NVOID,'r',LDC,N)) == NULL) goto err;;
  if((D = nsp_matrix_create(NVOID,'r',LDD,M)) == NULL) goto err;;
  if((DWORK = nsp_matrix_create(NVOID,'r',LDWORK,1)) == NULL) goto err;;
  if((IWORK = nsp_matrix_create(NVOID,'r',LIWORK,1)) == NULL) goto err;;
  if((Q = nsp_matrix_create(NVOID,'r',LDQ,N)) == NULL) goto err;;
  if((Rc = nsp_matrix_create(NVOID,'r',LDR,NCOL)) == NULL) goto err;;
  if((RY = nsp_matrix_create(NVOID,'r',LDRY,L)) == NULL) goto err;;
  if((S = nsp_matrix_create(NVOID,'r',LDS,L)) == NULL) goto err;;
  if((BWORK = nsp_matrix_create(NVOID,'r', LBWORK,1)) == NULL) goto err;;
  if((K = nsp_matrix_create(NVOID,'r',LDK,L)) == NULL) goto err;;

  // Copy inputs from MATLAB workspace to locally allocated arrays.
  //      
  COPY ( R->R, Rc->R, R->mn);
  if (TASK >= 2 && IJOB >= 3 )
    { 
      COPY ( A->R,Ac->R, A->mn );
      COPY ( C->R, Cc->R, C->mn);
    } 
  // Do the actual computations.

  nsp_slicot_ib01bd (METH ,JOB ,JOBCK ,&NOBR ,&N ,&M ,&L ,&NSMPL ,R->R,&LDR ,
	  A->R,&LDA ,C->R,&LDC ,B->R,&LDB ,D->R,&LDD ,Q->R,&LDQ ,
	  RY->R,&LDRY ,S->R,&LDS ,K->R,&LDK ,&TOL , IWORK->I,
		     DWORK->R,&LDWORK , BWORK->I,&IWARN ,&INFO ,1L,1L,1L);
  if (IWARN != 0 && PRINTW ){ 
    Sciprintf("  IWARN = %d ON EXIT FROM IB01BD", IWARN);
  } 
  if (INFO != 0 )
    { 
      Scierror("INFO =  %d ON EXIT FROM IB01BD", INFO );
    }
  // Copy output to MATLAB workspace.
  IP = 0;
  if (IJOB <= 2 )
    {
      MoveObj(stack,1,NSP_OBJECT(A));
      IP =1;
      if (lhs > 1 )
	{
	  MoveObj(stack,2,NSP_OBJECT(C));
	  IP =2;
	}
    }
  
  if (lhs > IP )
    { 
      if (IJOB == 1 ||IJOB >= 3 )
	{
	  IP =IP +1;
	  MoveObj(stack,IP,NSP_OBJECT(B));
	}
    }
  if (lhs > IP )
    { 
      if (IJOB == 1 ||IJOB == 4 )
	{ 
	  IP =IP +1;
	  MoveObj(stack,IP,NSP_OBJECT(D));
	} 
    } 
  if (NSMPL > 0 && lhs > IP )
    { 
      IP =IP +1;
      MoveObj(stack,IP,NSP_OBJECT(K));
  } 
  
  if (NSMPL > 0 && lhs > IP )
    { 
      IP =IP +1;
      MoveObj(stack,IP,NSP_OBJECT(Q));
      IP =IP +1;
      MoveObj(stack,IP,NSP_OBJECT(RY));
      IP =IP +1;
      MoveObj(stack,IP,NSP_OBJECT(S));
    } 
  if (lhs > IP )
    {
      NspMatrix *X;
      IP =IP +1;
      NRC = (NSMPL == 0 ) ? 4 : 12;
      if ((X = nsp_matrix_create(NVOID,'r',NRC,1))== NULL) return RET_BUG;
      COPY (DWORK->R+1, X->R, NRC );
      MoveObj(stack,IP,NSP_OBJECT(X));
    }
  return Max(lhs,1);
 err:
  return RET_BUG;
}

/* This interface is called through sylv 
 * 
 * interface function for solving Sylvester and Lyapunov matrix
 * equations using SLICOT routines 
 *
 * Copyright INRIA 2001
 * Scilab syntax
 *   [X(,sep)] = linmeq(task,A(,B),C,flag,trans(,schur))
 *   task = 1 :      [X] = linmeq(1,A,B,C,flag,trans,schur)
 *   task = 2 :  [X,sep] = linmeq(2,A,C,flag,trans)
 *                   [X] = linmeq(2,A,C,flag,trans)
 *   task = 3 :      [X] = linmeq(3,A,C,flag,trans)
 * Purpose:
 *   To solve the Sylvester and Lyapunov linear matrix equations
 *   task = 1:
 *         op(A)*X + X*op(B) = C,                          (1a)
 *         op(A)*X*op(B) + X = C,                          (1b)
 *   task = 2:
 *         op(A)'*X + X*op(A) = C,                         (2a)
 *         op(A)'*X*op(A) - X = C,                         (2b)
 *   task = 3:
 *         op(A)'*(op(X)'*op(X)) + (op(X)'*op(X))*op(A) =
 *                               -  op(C)'*op(C),          (3a)
 *         op(A)'*(op(X)'*op(X))*op(A) - op(X)'*op(X) =
 *                                     - op(C)'*op(C),     (3b)
 *   where op(M) = M, if trans = 0, and op(M) = M', if trans = 1.
 *
 * Input parameters: 
 *   task  - integer option to determine the equation type:
 *           = 1 : solve the Sylvester equation (1a) or (1b);
 *           = 2 : solve the Lyapunov equation (2a) or (2b);
 *           = 3 : solve for the Cholesky factor op(X) the Lyapunov
 *                 equation (3a) or (3b).
 *   A     - real coefficient N-by-N matrix.
 *           When task = 3, matrix A must be stable.
 *   B     - another real coefficient M-by-M matrix for
 *           equations (1a) or (1b).
 *   C     - right hand side matrix.
 *           task = 1 : C is N-by-M;
 *           task = 2 : C is N-by-N symmetric;
 *           task = 3 : op(C) is P-by-N.
 *   flag  - (optional) integer vector of length 3 or 2 containing
 *           options.
 *           task = 1 : flag has length 3
 *                flag(1) = 0 : solve the continuous-time equation (1a);
 *                              otherwise, solve the discrete-time
 *                              equation (1b).
 *                flag(2) = 1 : A is (quasi) upper triangular;
 *                          2 : A is upper Hessenberg;
 *                              otherwise, A is in general form.
 *                flag(3) = 1 : B is (quasi) upper triangular;
 *                          2 : B is upper Hessenberg;
 *                              otherwise, B is in general form.
 *           task = 2 : flag has length 2
 *                flag(1) = 0 : solve continuous-time equation (2a);
 *                              otherwise, solve discrete-time
 *                              equation (2b).
 *                flag(2) = 1 : A is (quasi) upper triangular;
 *                              otherwise, A is in general form.
 *           task = 3 : flag has length 2
 *                flag(1) = 0 : solve continuous-time equation (3a);
 *                              otherwise, solve discrete-time
 *                              equation (3b).
 *                flag(2) = 1 : A is (quasi) upper triangular;
 *                              otherwise, A is in general form.
 *           Default:    flag(1) = 0, flag(2) = 0 (, flag(3) = 0).
 *   trans - (optional) integer specifying a transposition option.
 *           trans = 0 : solve the equations (1) - (3) with op(M) = M.
 *           trans = 1 : solve the equations (1) - (3) with op(M) = M'.
 *           trans = 2 : solve the equations (1) with op(A) = A',
 *                                                    op(B) = B.
 *           trans = 3 : solve the equations (1) with op(A) = A,
 *                                                    op(B) = B'.
 *           Default:    trans = 0.
 *   schur - (optional) integer specifying whether the Hessenberg-Schur
 *           or Schur method should be used.
 *           Available for task = 1. 
 *           schur = 1 : Hessenberg-Schur method (one matrix is reduced
 *                       to Schur form).
 *           schur = 2 : Schur method (two matrices are reduced to Schur
 *                       form).
 *           Default:    schur = 1.
 *
 * Output parameters:
 *   X     - solution of the equation (or its Cholesky factor for (3)).
 *   sep   - (optional) estimator of Sep(op(A),-op(A)') for (2.a) or
 *           Sepd(A,A') for (2.b).
 *
 * Comments:
 *   1. For equation (1a) or (1b), when schur = 1, the Hessenberg-Schur 
 *      method is used, reducing one matrix to Hessenberg form and the
 *      other one to a real Schur form.
 *      Otherwise, both matrices are reduced to real Schur forms.
 *      If one or both matrices are already reduced to Schur/Hessenberg
 *      forms, this could be specified by flag(2) and flag(3).
 *      For general matrices, the Hessenberg-Schur method could be 
 *      significantly more efficient than the Schur method.
 *   2. For equation (3a) or (3b), the computed matrix X is the Cholesky
 *      factor of the solution, i.e., the real solution is op(X)'*op(X),
 *      where X is an N-by-N upper triangular matrix.
 *
 * References:
 *   This interface is based on the SLICOT routines:
 *   SB04PD, SB04MD, SB04QD, DTRSYL, SB04PY, SB04ND,  SB04RD, SB03MD, SB03OD
 *   
 *
 * Revisions:
 *   Adapted from the Slicot Matlab Mexfile by S. Steer Oct 2001
 */

int int_linmeq(Stack stack, int rhs, int opt, int lhs)
{
  int one=1;
  NspMatrix *A,*B,*C,*Flag;
  NspMatrix *Work=NULL,*U=NULL,*V=NULL,*Iwork=NULL,*WI=NULL,*WR=NULL,*SEP=NULL;
  double ONE=1.0 ,ZERO=0, sep;
  char *DICO, *FACT, *FACTA, *FACTB, *JOB, *SCHU, *TRANA, *TRANB, *ULA, *ULB ;
  int INFO ,ISGN ,LDA ,LDB ,LDC ,LDU ,LDV ,NDWORK ,M ,N ,P;
  double FERR ,SCALE ,TOL ;
  int PERTRB, IB, IP, J, LDW1, LDW2, NIWORK,  MXMN,  NM, NSCHUR=1, TASK, TRANS ;
  int Iflag[4]={0,0,0,0};
  CheckRhs(3,7);
  CheckLhs(1,2);
  
  /* task at position 1 */
  if (GetScalarInt (stack, 1 , &TASK) == FAIL)  return RET_BUG;
  if (TASK < 1 ||TASK > 3 )
    {
      Scierror ("task has 1, 2, or 3 as admissible values");
    }
  if (TASK == 1 )
    {
      if (rhs < 4 )
	{
	  Scierror ("Error: linmeq requires at least 4 input arguments when task equals 1");
	  return RET_BUG;
	}
      IP =6;
    }
  else 
    {
      IP =5;
    }
  TRANS =0;
  /* trans at position 6 or 5 */
  if ( rhs >= IP )
    {
      if (GetScalarInt (stack, IP , &TRANS) == FAIL)  return RET_BUG;
    }
  if (TASK == 1 && (TRANS < 0  || TRANS > 3 ))
    {
      Scierror("Error: admissible values for trans are 0, 1, 2, OR 3 when task == 1\n");
      return RET_BUG;
    }
  else
    {
      if ( TASK != 1  && (TRANS < 0  || TRANS > 1 ))
	{
	  Scierror("Error: admissible values for trans are 0, or 1 when task != 1");
	  return RET_BUG;
	}
    }
  /*  schur */
  if (TASK == 1 )
    {
      NSCHUR =1;
      if (rhs >= IP +1)
	{
	  if (GetScalarInt (stack, IP+1 , &NSCHUR) == FAIL)  return RET_BUG;
	  if (NSCHUR < 1  || NSCHUR > 2 )
	    {
	      Scierror("Error: admissible values for schur are 1, or 2 when task == 1\n");
	      return RET_BUG;
	    }
	}
    }
  /*  A(NxN), (B(MxM),) C(NxM), or C(NxN), or op(C)(PxN) */
  
  if((A=GetRealMatCopy(stack,2))==NULL) return RET_BUG;
  CheckSquare(NspFname(stack),2,A);
  N=A->m;
  if (TASK == 1)
    {
      if((B=GetRealMatCopy(stack,3))==NULL) return RET_BUG;
      CheckSquare(NspFname(stack),3,B);
      M= B->m;
      if((C=GetRealMatCopy(stack,4))==NULL) return RET_BUG;
      if( C->m != A->m || C->n != B->m)
	{
	  Scierror("Error: C should be of size %dx%d\n",A->m,B->m);
	  return RET_BUG;
	}
    }
  else
    {
      if((C=GetRealMatCopy(stack,3))==NULL) return RET_BUG;
      if (TRANS == 0 )
	{
	  P = C->m;
	  if( C->n != A->m )
	    {
	      Scierror("Error: C should have %d columns\n",A->m);
	      return RET_BUG;
	    }
	}
      else 
	{
	  if( C->m != A->m)
	    {
	      Scierror("Error: C should have %d rows\n",A->m);
	      return RET_BUG;
	    }
	  P = C->n;
	}
    }
  if (TASK == 1)
    IP=5;
  else
    IP=4;

  /*    flag */
  if (rhs >= IP )
    {
      int i;
      if((Flag=GetRealMatCopy(stack,IP))==NULL) return RET_BUG;
      if (TASK == 1 )
	{
	  if ( Flag->mn  > 3) {
	    Scierror("Error: flag must be a vector with at most 3 elements\n");
	    return RET_BUG;
	  }
	}
      else 
	{if ( Flag->mn > 2 ) {
	    Scierror("Error: flag must be a vector with at most 2 elements");
	    return RET_BUG;
	  }
	}
      for ( i = 0 ; i < Flag->mn ; i++) Iflag[i+1]= Flag->R[i];
    }
  /* 
   * Determine the lenghts of working arrays.
   * Use a larger value for NDWORK for enabling calls of block algorithms
   * in DGEES, and possibly in DGEHRD, DGEQRF, DGERQF, SB04PD.
   *
   */
  LDA =Max (1,N );
  if (TASK == 1 )
    {
      LDB =Max (1,M );
      if (NSCHUR == 2 )
	{
	  if ( Iflag[2] == 1)
	    {
	      LDW1 =0;
	      LDW2 =0;
	    }
	  else
	    {
	      LDW1 =1+2*N ;
	      LDW2 =3*N;
	    }
	  IB =0;
	  if ( Iflag[3] != 1)
	    {
	      IB =2*M ;
	      if ( Iflag[2] == 1) IB =IB +1;
	      LDW2 =Max (LDW2 ,IB +3*M );
	    }
	  LDW2 =Max (LDW2 ,IB +2*N );
	  NDWORK =Max (1,LDW1 +LDW2 );
	}
      else
	{
	  if ( Iflag[2] * Iflag[3] == 1 ) 
	    {
	      /* */
	      NIWORK =0;
	      if ( Iflag[1] != 0)
		NDWORK =2*N ;
	      else 
		NDWORK =0;
	    }
	  else if ( Iflag[2]*Iflag[3] == 2)
	    {
	      MXMN =Max (M ,N );
	      NIWORK =2*MXMN ;
	      NDWORK =2*MXMN *(4+2*MXMN );
	    }
	  else
	    {
	      NIWORK =4*N ;
	      NDWORK =Max (1, Max(5*M ,N +M ));
	      if ( Iflag[1] == 0)
		NDWORK =Max (NDWORK , 2*N *N +8*N );
	      else 
		NDWORK =Max (NDWORK ,2*N *N +9*N );
	    }
	}
      NM =M ;
    }
  else if (TASK == 2 )
    {
      NDWORK =Max (1, Max(N *N ,3*N ));
      if (lhs == 2 ) {
	NDWORK =Max (NDWORK ,2*N *N );
	if ( Iflag[1] != 0) NDWORK =Max (NDWORK ,2*(N *N +N ));
      }
      NM =N ;
    }
  if (TASK != 3 ) 
    LDC =LDA ;
  else
    {
      if (TRANS == 0 ) 
	LDC =Max (1, Max(N ,P) );
      else 
	LDC =LDA ;
      MXMN =Min (P ,N );
      NDWORK =Max (1,4*N +MXMN );
      NM =N ;
    }
  /* 
   * Allocate variable dimension local arrays.
   */
  if (TASK == 1 )
    {
      if( (Work = nsp_matrix_create(NVOID,'r',NDWORK,1))== NULL) return RET_BUG;
      if (NSCHUR == 2 )
	{
	  if ( Iflag[2] == 1) {
	    FACTA ="S";
	    LDU =1;
	    if( (U = nsp_matrix_create(NVOID,'r',LDU,1))== NULL) return RET_BUG;}
	  else {
	    FACTA ="N";
	    LDU =LDA ;
	    if( (U = nsp_matrix_create(NVOID,'r',LDU,N))== NULL) return RET_BUG;
	  }
	  if ( Iflag[3] == 1){
	    FACTB ="S";
	    LDV =1;
	    if( (V = nsp_matrix_create(NVOID,'r',LDV,1))== NULL) return RET_BUG;
	  }
	  else {
	    FACTB ="N";
	    LDV =LDB ;
	    if( (V = nsp_matrix_create(NVOID,'r',LDV,M))== NULL) return RET_BUG;
	  }
	}
      
      else
	{
	  SCHU ="N";
	  if( (Iwork=nsp_matrix_create(NVOID,'r',NIWORK,1))== NULL) return RET_BUG;
	  if ( Iflag[2] == 1) {
	    if (Iflag[3] == 1) 
	      SCHU ="S";
	    else if (Iflag[3] == 2)
	      SCHU ="A";
	  }
	  else
	    if (Iflag[2] == 2 &&  Iflag[3] == 1) {
	      SCHU ="B";
	    }
	  if ( strcmp(SCHU ,"N") == 0)
	    {
	      LDU =LDB ;
	      if( (U=nsp_matrix_create(NVOID,'r',LDU,M))== NULL) return RET_BUG; 
	    }
	}    
    }
  else
    {
      if (TASK == 2 )
	{
	  LDU =LDA ;
	  if( (U=nsp_matrix_create(NVOID,'r',LDU,N)) == NULL) return RET_BUG;;
	  if( (Work=nsp_matrix_create(NVOID,'r',NDWORK,1)) == NULL) return RET_BUG;;
	  if( (WI=nsp_matrix_create(NVOID,'r',N,1)) == NULL) return RET_BUG;;
	  if( (WR=nsp_matrix_create(NVOID,'r',N,1)) == NULL) return RET_BUG;;
	  if (lhs <= 1 ) 
	    {
	      if((Iwork =nsp_matrix_create(NVOID,'r',1, 1)) == NULL) return RET_BUG;
	    }
	  else 
	    {
	      if((Iwork =nsp_matrix_create(NVOID,'r', N*N, 1)) == NULL) return RET_BUG;
	    }
	}
      else 
	{  LDU =LDA ;
	  if( (U=nsp_matrix_create(NVOID,'r',LDU,N)) == NULL) return RET_BUG;;
	  if( (Work=nsp_matrix_create(NVOID,'r',NDWORK,1)) == NULL) return RET_BUG;;
	  if( (WI=nsp_matrix_create(NVOID,'r',N,1)) == NULL) return RET_BUG;;
	  if( (WR=nsp_matrix_create(NVOID,'r',N,1)) == NULL) return RET_BUG;;
	}
    }
  /*
    C Do the actual computations.
  */
  if (TASK == 1 ) {
    if (NSCHUR == 2 ) {
      if (TRANS == 0 ) {
	TRANA ="N";
	TRANB ="N";}
      else if (TRANS == 1 ){ 
	TRANA ="T";
	TRANB ="T";}
      else if (TRANS == 2 ){ 
	TRANA ="T";
	TRANB ="N";}
      else if (TRANS == 3 ){ 
	TRANA ="N";
	TRANB ="T";}
      if (Iflag[1] != 0) 
	DICO ="D";
      else 
	DICO ="C";
      ISGN =1;
      nsp_slicot_sb04pd (DICO ,FACTA ,FACTB ,TRANA ,TRANB ,&ISGN ,&N ,&M ,
			 A->R, &LDA ,U->R,&LDU ,B->R,&LDB ,V->R,&LDV ,
			 C->R, &LDC ,&SCALE ,Work->R,&NDWORK ,&INFO,1L,1L,1L,1L,1L );
    }
    else
      {
	if (TRANS == 0 ){ 
	  if ( strcmp(SCHU ,"S")== 0)
	    { 
	      TRANA ="N";
	      TRANB ="N";
	    }
	  else {
	    ULA ="U";
	    ULB ="U";
	  }
	}		  
	else if (TRANS == 1 ){ 
	  if ( strcmp(SCHU ,"S")==0 ){ 
	    TRANA ="T";
	    TRANB ="T";}
	  else {
	    ULA ="L";
	    ULB ="L";
	    for ( J =2 ; J <= N ; J++)
	      {
		int j = J-1;
		C2F(dswap)(&j,A->R +1-1 +(J-1)*LDA,&one, (A->R+J- 1 +(1-1)*LDA),&LDA );
	      }
	    for ( J=2 ; J<= M ; J++)
	      {
		int j = J-1;
		C2F(dswap) (&j, (B->R+1-1 +(J-1)*LDB),&one,(B->R+J- 1 +(1-1)*LDB),&LDB );
	      }
	  }
	}
	else if (TRANS == 2 ){ 
	  if ( strcmp(SCHU ,"S")==0){ 
	    TRANA ="T";
	    TRANB ="N";}
	  else {
	    ULA ="L";
	    ULB ="U";
	    for ( J =2 ; J <= N ; J++)
	      {
		int j=J-1;
		C2F(dswap) (&j,(A->R+1-1 +(J-1)*LDA),&one,(A->R+J-1 +(1-1)*LDA),&LDA );
	      }
	  }
	}
	else if (TRANS == 3 ){ 
	  if ( strcmp(SCHU ,"S")==0){ 
	    TRANA ="N";
	    TRANB ="T";}
	  else {
	    ULA ="U";
	    ULB ="L";
	    for ( J =2 ; J <= M ; J++) {
	      int j=J-1;
	      C2F(dswap) (&j,(B->R+1-1 +(J-1)*LDB),&one,(B->R+J- 1 +(1-1)*LDB),&LDB );
	    }
	  }
	}
	if ( strcmp (SCHU ,"N")==0)
	  { 
	    SCALE =ONE ;
	    if (Iflag[1] == 0) 
	      nsp_slicot_sb04md (&N ,&M ,A->R,&LDA ,B->R,&LDB ,C->R,
				 &LDC ,U->R,&LDU ,Iwork->I,Work->R,&NDWORK ,
				 &INFO );
	    else 
	      nsp_slicot_sb04qd (&N ,&M ,A->R,&LDA ,B->R,&LDB ,C->R,
				 &LDC ,U->R,&LDU ,Iwork->I,Work->R,&NDWORK ,
				 &INFO );
	  }
	else if ( strcmp(SCHU ,"S")==0)
	  { 
	    if (Iflag[1] == 0)
	      C2F(dtrsyl) (TRANA ,TRANB ,&one,&N ,&M ,A->R,&LDA , B->R,&LDB,C->R,&LDC,&SCALE,&INFO,1L,1L );
	    else 
	      nsp_slicot_sb04py (TRANA ,TRANB ,&one,&N ,&M ,A->R,&LDA , B->R,&LDB ,C->R,
				 &LDC ,&SCALE ,Work->R,&INFO,1L,1L);
	  }
	else
	  {
	    SCALE =ONE ;
	    TOL =ZERO; 
	    /* C              Default tolerance (epsilon_machine) is used. */
	    if (Iflag[1] == 0)
	      nsp_slicot_sb04nd (SCHU ,ULA ,ULB ,&N ,&M ,A->R,&LDA ,
				 B->R, &LDB ,C->R,&LDC ,&TOL , Iwork->I,
				 Work->R, &NDWORK ,&INFO,1L,1L,1L );
	    else 
	      nsp_slicot_sb04rd (SCHU ,ULA ,ULB ,&N ,&M ,A->R,&LDA ,
				 B->R, &LDB ,C->R,&LDC ,&TOL ,Iwork->I,
				 Work->R, &NDWORK ,&INFO,1L,1L,1L );
	  }
      }
  }
  else
    {
      DICO = (Iflag[1] == 0) ? "C" : "D";
      FACT = (Iflag[2] != 1) ? "N" : "F";
      if ( FACT[0]== 'F') C2F(dlaset) ("FULL",&N ,&N ,&ZERO ,&ONE ,U->R,&LDU,1L );
      TRANA = (TRANS == 0 ) ? "N": "T";

      if (TASK == 2 )
	{ 
	  JOB =  (lhs == 2 ) ? "B": "X";
	  nsp_slicot_sb03md (DICO ,JOB ,FACT ,TRANA ,&N ,A->R, &LDA ,
			     U->R, &LDU ,C->R,&LDC ,&SCALE ,&sep ,&FERR, WR->R, 
			     WI->R, Iwork->I, Work->R, &NDWORK ,&INFO,1L,1L,1L,1L );
	}
      else 
	{
	  nsp_slicot_sb03od (DICO ,FACT ,TRANA ,&N ,&P ,A->R,&LDA ,U->R ,
			     &LDU ,C->R,&LDC ,&SCALE ,WR->R,WI->R,Work->R,
			     &NDWORK ,&INFO,1L,1L,1L );
	}
    }
  
  PERTRB =(TASK == 1  && (INFO == N +M +1 ||  (Iflag[2]*Iflag[3]  == 1 &&  INFO == 1 ))) || 
    (TASK == 2  && INFO == N +1) || (TASK == 3  && INFO == 1 );
  if (INFO == 0  || PERTRB ){ 
    if (lhs >= 1 ){ 
      if (TASK == 3 ){ 
	if (TRANS == 0  && P > N )
	  C2F(dlacpy) ("UPPER",&N ,&N , C->R,&LDC ,C->R,&LDA,1L );
	if (N > 1 ) {
	  int nm1=N-1;
	  C2F(dlaset) ("LOWER",&nm1,&nm1,&ZERO ,&ZERO ,(C->R+2-1 +(1-1)*LDC),&LDA,1L );
	}
      }
    }
    if (TASK == 2 ){ 
      if (lhs >= 2 ){
	double sep;
	if (N == ZERO ) sep =ZERO ;
	if( (SEP=nsp_matrix_create(NVOID,'r',1,1)) == NULL) return RET_BUG;;
	SEP->R[0]=sep;
      }
    }
  }
  /* 
   * Error and warning handling.
   */
  if (INFO > 0 )
    { 
      if (TASK == 1 ){ 
	if (NSCHUR == 2 )
	  { 
	    if(INFO <= M+N) Scierror("Error: Failure when computing eigenvalues\n");
	    else if(INFO > M+N)	Scierror("Error: equation is singular\n");
	    goto err;
	  }
	else
	  {
	    if (strcmp(SCHU ,"N")==0)
	      { 
		if(INFO <= M) Scierror("Error: Failure when computing eigenvalues\n");
		else if(INFO > M) Scierror("Error: equation is singular\n");
	      }
	    else
	      {
		Scierror("Error: equation is singular\n");
	      }
	    goto err;
	  }
      }
      else if (TASK == 2 )
	{ 
	  if(INFO <= N) Scierror("Error: Failure when computing eigenvalues\n");
	  else if(INFO > N) 		Scierror("Error: equation is singular\n");
	  goto err;
	}
      else if (TASK == 3 )
	{ 
	  if (INFO == 1) Scierror("Error: equation is singular\n");
	  else if (INFO == 2 || INFO == 3)
	    {
	      if( strcmp(DICO,"C")==0) Scierror("Error: Matrix is not stable (cont)\n");
	      else Scierror("Error: not a schur form\n");
	    }
	  else if (INFO == 4 || INFO == 5)
	    {
	      Scierror("Error: not a schur form");
	    }
	  else if (INFO == 6) {
	    Scierror("Error: Failure when computing eigenvalues\n");
	  }
	  goto err;
	}
    }
  else if(INFO < 0)
    {
      Scierror("Error: internal error in linmeq");
      goto err;
    }
  if ((INFO == 0  || PERTRB ) && SCALE != ONE )
    {
      double TEMP;
      if (TASK >= 2 ) 
	TEMP =SCALE ;
      else 
	TEMP =SCALE * SCALE;
      printf("warning: the right hand sides were scaled by %f to avoid overflow\n",TEMP);
    }
  if (INFO != 0  &&  !PERTRB )
    {
      Scierror("Error: in linmeq");
      goto err;
    }
  else if (SCALE != ONE )
    { 
      // msgs(1000,0);
    }
  if (PERTRB ){ 
    printf("warning: the equation is (almost) singular; perturbed values have been used\n");
  }
  NSP_OBJECT(C)->ret_pos = 1;
  if ( lhs >= 2 )
    {
      if (TASK == 2 )
	{
	  MoveObj(stack,2,NSP_OBJECT(SEP));
	}
      else
	{
	  Scierror("Error: lhs should be one whan task is not 2\n");
	  goto err;
	}
    }
  if ( Work !=NULL) nsp_matrix_destroy(Work);
  if ( U !=NULL) nsp_matrix_destroy(U);
  if ( V !=NULL) nsp_matrix_destroy(V);
  if ( Iwork !=NULL) nsp_matrix_destroy(Iwork);
  if ( WI !=NULL) nsp_matrix_destroy(WI);
  if ( WR !=NULL) nsp_matrix_destroy(WR);
  return Max(lhs,1);
 err:
  if ( Work !=NULL) nsp_matrix_destroy(Work);
  if ( U !=NULL) nsp_matrix_destroy(U);
  if ( V !=NULL) nsp_matrix_destroy(V);
  if ( Iwork !=NULL) nsp_matrix_destroy(Iwork);
  if ( WI !=NULL) nsp_matrix_destroy(WI);
  if ( WR !=NULL) nsp_matrix_destroy(WR);
  return RET_BUG;
}
