/*
 *  FINDBD.F - Gateway function for computing the initial state and/or
 *             the matrices B and D of a discrete-time linear system,
 *             given the system matrices A, C, and possibly B, D, and the
 *             input and output trajectories, using SLICOT routine IB01CD.
 */

#define COPY(src,dst,nb) C2F(dcopy)(&nb,src,&one,dst,&one)

int int_findbd(Stack stack, int rhs, int opt, int lhs)
{
  static const double ZERO =0.0D0;
  double TOL ;
  int INFO ,IWARN ,L ,LDA ,LDB ,LDC ,LDD ,LDU ,LDV ,LDWORK , LDY ,M ,N ,NSMP ;
  CHARACTER COMUSE ,JOB ,JOBX0 ;
  int IWORK;
  int A, B, C, D, DWORK, U, V, X0, Y;
  double TEMP ;
  int CSIZE ,CUSE ,IC ,IJOB ,INI ,IP ,IPS ,IQ ,ISIZE ,ITMP ,
    LDW1 ,LDW2 ,LDW3 ,LDWMIN ,LIWORK ,MA ,MINSMP ,MINWLS ,NA ,
    NCOL ,NCP1 ,TASK ;
  int PRINTW ;
  static const int CSIZE =64000;
  
  CheckRhs(2,2);
  CheckLhs(0,1);
  
  // Check dimensions of input parameters and read/set scalar parameters.
  //   jobx0
  if (GetScalarInt (stack, 1, &TASK) == FAIL)  return RET_BUG;
  if (TASK < 0 ||TASK > 1 )
    {
      Scierror ("JOBX0 HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES");
    }
  JOBX0 = (TASK == 1 ) ? 'X' : 'N';
  // comuse
  if (GetScalarInt (stack, 2, &CUSE) == FAIL)  return RET_BUG;
  if (CUSE < 1 ||CUSE > 3 )
    {
      Scierror ("COMUSE HAS 1, 2, OR 3 THE ONLY ADMISSIBLE VALUES");
    }
  COMUSE = (CUSE == 1 ) ? 'C' : ( (CUSE == 2 ) ? 'U' : 'N');
    
  if (TASK == 1 && CUSE == 3 )
    {
      if (rhs < 5 )
	{
	  Scierror ("FINDBD REQUIRES AT LEAST 5 INPUT ARGUMENTS");
	}
    }
  else
    {
      if (CUSE == 1 )
	{
	  if (rhs < 6 )
	    {
	      Scierror ("FINDBD REQUIRES AT LEAST 6 INPUT ARGUMENTS");
	    }
	}
    }
  //   job
  if ((TASK == 1 && CUSE == 2 )||CUSE == 1 )
    {
      if (GetScalarInt (stack, 3, &IJOB) == FAIL)  return RET_BUG;
      if (IJOB < 1 ||IJOB > 2 ){
	Scierror ("JOB HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES");
      }
      IP =4;
      if (IJOB == 1 ){
	JOB ='B';
      } else {
	JOB ='D';
      }
    }
  else {
    IP =3;
    JOB ='B';
    IJOB =1;
  }

  if (TASK == 1 && CUSE == 2 )
    {
      if (IJOB == 1 && rhs < 7 ){
	Scierror ( "FINDBD REQUIRES AT LEAST 7 INPUT ARGUMENTS");
      }
      else if (IJOB == 2 && rhs < 8 )
	{
	  Scierror ( "FINDBD REQUIRES AT LEAST 8 INPUT ARGUMENTS");
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
	  Scierror("B MUST HAVE %d ROWS",N);
	}
      IP =IP +1;
    }

  //   C(l,n)
  if (rhs >= IP )
    {
      if(( C=GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
      L = C->m :
      NA = C->n;
      if (NA != N )
	{
	  Scierror("C MUST HAVE %d COLUMNS",N);
	}
      IP =IP +1;
      if (L.LE.0 )
	{
	  Scierror ("THE SYSTEM HAS NO OUTPUTS");
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
	  Scierror("D MUST HAVE %d ROWS" L );
	}
      if (MA != M )
	{
	  Scierror("D MUST HAVE %d COLUMNS'" M );
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
  if (((TASK == 1 && CUSE == 2 )||CUSE == 1 ).AND.rhs >= IP )
    {
      if(( U =GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
      MA = U->n;
      if (CUSE == 2 && MA != M )
	{
	  Scierror("U MUST HAVE %d COLUMNS",M);
	} else {
	M =MA;
      }
      if (M > 0 ){
	if ( U->m != NSMP ){
	  Scierror ("U MUST HAVE THE SAME ROW DIMENSION AS Y");
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
    Scierror(" THE NUMBER OF SAMPLES SHOULD BE AT LEAST %d", MINSMP);
  }
  //     tol
  if ( rhs >= IP)
    {
      if (GetScalarDouble (stack, IP, &TOL) == FAIL)  return RET_BUG;
      IP =IP +1;
    }
  //     printw
  PRINTW =.FALSE.;
  if ( rhs >= IP)
    {
      if (GetScalarInt (stack, IP , &PRINTW) == FAIL)  return RET_BUG;
      if (ITMP < 0 ||ITMP > 1 )
	{ 
	  Scierror ("PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES");
	  return RET_BUG;
	} 
      PRINTW = (ITMP == 1);
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
    if (CUSE == 1 ){
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
      ITAU =IC +L *N;
      LDW2 =ISIZE +2*N +Max (IC ,4*N );
      LDW3 =MINWLS +2*N +Max (IQ *NCP1 +ITAU ,4*N );
      INI =2;
    }
    LDWMIN =INI +N *(N +M +L )+Max (5*N ,INI ,Min (LDW2 ,LDW3 ));
    LDWORK =Max (LDWMIN ,2*CSIZE /3,CSIZE -(M +L )*NSMP -2*N *(N +
							       M +L )-L *M );
  }
  //   ldwork
  //
  if (rhs >= IP )
    {
      if (GetScalarInt (stack, IP , &TEMP) == FAIL)  return RET_BUG;
      ITMP =INT (TEMP );
      if (ITMP < LDWMIN ){
	LDWORK =LDWMIN;
      } else {
	LDWORK =ITMP;
      }
    }

  // Allocate variable dimension local arrays.
  
  if ((A = nsp_matrix_create(NVOID,'r',LDA , N ))==NULL) goto err;
  if ((B = nsp_matrix_create(NVOID,'r',LDB , M ))==NULL) goto err;
  if ((C = nsp_matrix_create(NVOID,'r',LDC , N ))==NULL) goto err;
  if ((D = nsp_matrix_create(NVOID,'r',LDD , M ))==NULL) goto err;
  if ((DWORK = nsp_matrix_create(NVOID,'r',1, LDWORK ))==NULL) goto err;
  if ((IWORK = nsp_matrix_create(NVOID,'r',1, LIWORK))==NULL) goto err;
  if ((U = nsp_matrix_create(NVOID,'r',LDU , M ))==NULL) goto err;
  if ((V = nsp_matrix_create(NVOID,'r',LDV , N ))==NULL) goto err;
  if ((X0 = nsp_matrix_create(NVOID,'r',N ,1))==NULL) goto err;
  if ((Y = nsp_matrix_create(NVOID,'r',LDY , L))==NULL) goto err;
    
  // Copy inputs from MATLAB workspace to locally allocated arrays.
	
  IP =IPS;
  if (rhs >= IP )
    { 
      C2F(dcopy)(&iSizeA, pdblA, &iOne, pA, &iOne);
      if (iTASK == 1 && iCUSE == 2)
        {
	  if (iN > 0)
            {
	      C2F(dcopy)(&iSizeB, pdblB, &iOne, pB, &iOne);
            }
        }
      C2F(dcopy)(&iSizeC, pdblC, &iOne, pC, &iOne);
      if (iTASK == 1 && iCUSE == 2 && iIJOB == 2)
        {
	  C2F(dcopy)(&iSizeD, pdblD, &iOne, pD, &iOne);
        }
      
      C2F(dcopy)(&iSizeY, pdblY, &iOne, pY, &iOne);
      if (iCUSE == 1 || (iTASK == 1 && iCUSE == 2))
        {
	  C2F(dcopy)(&iSizeU, pdblU, &iOne, pU, &iOne);
        }
    } 
  // Do the actual computations.
  IB01CD (JOBX0 ,COMUSE ,JOB ,N ,M ,L ,NSMP ,A->R,LDA ,B->R,
	  ,LDB ,C->R,LDC ,D->R,LDD ,U->R,LDU ,Y->R,LDY ,X0->R,
	  V->R,LDV ,TOL ,iIWORK->R,DWORK->R,LDWORK ,IWARN ,INFO );
  if (IWARN != 0 && PRINTW )
    { 
      Sciprintf("  IWARN = %d ON EXIT FROM IB01CD", IWARN );
    }
  
  if (INFO != 0 )
    { 
      Scierror("INFO = %d ON EXIT FROM IB01CD",INFO);
      return RET_BUG;
    }
  iPos = 1;

  if (iTASK == 1 || (iTASK == 0 && iCUSE == 2))
    {
      MoveObj(stack,iPos,NSP_OBJECT(X0));
      iPos++;
    }
  
  if (iCUSE == 1 && lhs >= iPos)
    {
      MoveObj(stack,iPos,NSP_OBJECT(B));
      iPos++;
      if ( lhs >= iPos)
        {
	  MoveObj(stack,iPos,NSP_OBJECT(D));
	  iPos++;
        }
    }
  
  if ((iTASK == 1 || iCUSE == 1) && lhs >= iPos)
    {
      MoveObj(stack,iPos,NSP_OBJECT(V));
      iPos++;
    }

  if (lhs >= iPos)
    {
      int iNO = 1;
      if (iCUSE == 1 && iM > 0 && iIJOB == 2)
        {
	  iNO = 2;
        }
      ?
      createMatrixOfDouble(pvApiCtx, iCurrentLhs, iNO, 1, pDWORK + 1);
      AssignOutputVariable(pvApiCtx, iPos) = iCurrentLhs;
    }
  return Max(lhs,1);
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
  static const double ZERO =0.0D0,ONE =1.0D0;
  // .. Scalar parameters used by SLICOT subroutines ..
  double TOL1 ,TOL2 ;
  int INFO ,IWARN ,L ,LDR ,LDU ,LDWORK ,LDY ,M ,N ,NOBR ,NSMP ;
  CHARACTER ALG ,BATCH ,CONCT ,CTRL ,JOBD ,METH ;
  // .. Allocatable arrays ..
  // !Fortran 90/95 (Fixed dimensions should be used with Fortran 77.)
  int IWORK;
  int DWORK, R, SV, U, Y;

  // .. Local variables and constant dimension arrays ..
  double TEMP ,TOL (2);
  int CSIZE ,IALG ,IBCH ,ICNC ,IJOB ,IMTH ,IP ,ISIZE ,ITMP ,
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
  METH = (IMTH == 1 ) ? 'M' : 'N';
  
  //   alg
  if (GetScalarInt (stack, 2, &IALG) == FAIL)  return RET_BUG;
  if (IALG < 1 ||IALG > 3 ){ 
    Scierror ("ALG HAS 1, 2 OR 3 THE ONLY ADMISSIBLE VALUES");
  } 
  ALG = (IALG == 1 ) ? 'C' : ((IALG == 2 ) ? 'F' : 'Q');
  
  //   jobd
  if (GetScalarInt (stack, 3, &IJOB) == FAIL)  return RET_BUG;
  if (IJOB < 1 ||IJOB > 2 ){ 
    Scierror ("JOBD HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES");
  } 
  JOBD = (IJOB == 1 ) ? 'M': 'N';
  //   batch
  if (GetScalarInt (stack, 4, &IBCH) == FAIL)  return RET_BUG;
  if (IBCH < 1 ||IBCH > 4 ){ 
    Scierror ("BATCH HAS 1, 2, 3 OR 4 THE ONLY ADMISSIBLE VALUES");
  } 
  BATCH = (IBCH == 1 ) ? 'F' : ((IBCH == 2 ) ? 'I' : ((IBCH == 3 ) ? 'L' : 'O'));
  
  //   conct
  if (GetScalarInt (stack, 5, &ICNC) == FAIL)  return RET_BUG;
  if (ICNC < 1 ||ICNC > 2 ){ 
    Scierror ("CONCT HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES");
  } 
  CONCT = (ICNC == 1 ) ? 'C': 'N';

  //   ctrl is set to 'N'
  CTRL ='N';
  //   s
  if (GetScalarInt (stack, 6, &NOBR) == FAIL)  return RET_BUG;
  if (NOBR < 1 ){ 
    Scierror ("S MUST BE A POSITIVE int");
  } 
  //   Y(txp), (U(txm))
  if(( Y =GetRealMatCopy(stack,7)) ==NULL) return RET_BUG;
  NSMP = Y->m;
  L = Y->n;
  if (L.LE.0 ){ 
    Scierror ("THE SYSTEM HAS NO OUTPUTS");
  } 
  if (rhs >= 8 ){
    if(( U =GetRealMatCopy(stack,8)) ==NULL) return RET_BUG;
    M =->n; 
  }
  else
    { 
      M =0;
    } 
  NR =2*(M +L )*NOBR ;
  if (IBCH.LE.2 )
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
      if(( TOL =GetRealMatCopy(stack,9)) ==NULL) return RET_BUG;
      ISIZE = TOL->mn;
      if (ISIZE > 2 )
	{
	  Scierror ("TOL MUST BE A VECTOR WITH AT MOST 2 ELEMENTS");
	}
      if (ISIZE > 0 ) TOL1 =TOL->R[0];
      if (ISIZE > 1 ) TOL2 =TOL->R[1];
    }
  PRINTW =.FALSE.;
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
      if (ITMP < 0 ||ITMP > 1 )
	{ 
	  Scierror ("PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES");
	  return RET_BUG;
	} 
      PRINTW = ITMP == 1 ;
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
  if (IALG == 2 )LDWMIN =Max (LDWMIN ,2*NR *(M +L +1)+NR ,NR *(M +
							       L +3));
  LDWORK =Max (LDWORK ,LDWMIN );
  //   ldwork
  //   
  if (rhs >= 11 )
    {
      if (GetScalarInt (stack, IP , &ITMP) == FAIL)  return RET_BUG;
      ITMP =INT (TEMP );
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
	  if(( r =GetRealMatCopy(stack,IP)) ==NULL) return RET_BUG;
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
  
  if((R = nsp_matrix_create(NVOID,'r',LDR,NCOL)) == NULL) goto err;
  if((SV = nsp_matrix_create(NVOID,'r',L*NOBR,1)) == NULL) goto err;
  if((U = nsp_matrix_create(NVOID,'r',LDU,M)) == NULL) goto err;
  if((Y = nsp_matrix_create(NVOID,'r',LDY,L)) == NULL) goto err;
  if((IWORK = nsp_matrix_create(NVOID,'r', 1,,LIWORK,1)) == NULL) goto err;
  if((DWORK = nsp_matrix_create(NVOID,'r',LDWORK,1)) == NULL) goto err;

  DSET(LDR*NCOL,0.0d0,R->R,1);

  // Copy inputs from MATLAB workspace to locally allocated arrays.
  //      
 COPY (MXGETPR (PRHS (7)),Y->R,NSMP *L );
  if (M > 0 ){ 
   COPY (MXGETPR (PRHS (8)),U->R,NSMP *M );
  } 
  if (rhs >= IP && (IBCH == 2 ||IBCH == 3 )){ 
   COPY (MXGETPR (PRHS (IP )),R->R,NR *NCOL );
    if (IALG == 2 ||ICNC == 1 ){ 
      if (LDR == NR ){ 
	DCOPY (NRSAVE ,R+1-1 +(NR+1-1->R*LDR),1,DWORK->R,1);
      }
      else
	{ 
	  DCOPY (NRSAVE ,R+NR*NR+1-1 +(1-1->R*LDR),1,DWORK->R,1);
	} 
    } 
  } 
  // Do the actual computations.
  IB01AD (METH ,ALG ,JOBD ,BATCH ,CONCT ,CTRL ,NOBR ,M ,L ,
	  NSMP ,U->R,LDU ,Y->R,LDY ,N ,R->R,LDR ,SV->R,TOL1 ,
	  TOL2 ,iIWORK->R,DWORK->R,LDWORK ,IWARN ,INFO );
  if (IWARN != 0 && PRINTW ){ 
    Scierror("  IWARN = '',I4,'' ON EXIT FROM IB01AD'",  IWARN);
  } 
  if (INFO != 0 ){ 
    Scierror("INFO = '',I4,'' ON EXIT FROM IB01AD'", INFO);
  } 

  // Copy output to MATLAB workspace.

  if (LDR > NR ) DLACPY ('FULL',NR ,NR ,R->R,LDR ,R->R,NR );
  if ((IALG == 2 ||ICNC == 1 ).AND.IBCH.LE.2 ){ 
    if (LDR == NR ){ 
      DCOPY (NRSAVE ,DWORK->R,1,R+1-1 +(NR+1-1->R*LDR),1);
    } else { 
      DCOPY (NRSAVE ,DWORK->R,1,R+NR*NR+1-1 +(1-1->R*LDR),1);
    } 
  } 
  PLHS (1)=MXCREATEFULL (NR ,NCOL ,0);
  COPY (R->R,MXGETPR (PLHS (1)),NR *NCOL );
  if (IBCH > 2 ){ 
    if (lhs > 1 ){ 
      PLHS (2)=MXCREATEFULL (1,1,0);
      TEMP =N ;
      COPY (TEMP ,MXGETPR (PLHS (2)),1);
      if (lhs > 2 )
	{ 
	  PLHS (3)=MXCREATEFULL (L *NOBR ,1,0);
	  COPY (SV->R,MXGETPR (PLHS (3)),L *NOBR );
	} 
      if (IMTH == 2 && lhs > 3 )
	{ 
	  PLHS (4)=MXCREATEFULL (2,1,0);
	  COPY (DWORK+2-1->R,MXGETPR (PLHS (4)), 2);
	} 
    } 
  } 
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
  static const double ZERO =0.0D0;
  // .. Scalar parameters used by SLICOT subroutines ..
  double TOL ;
  int INFO ,IWARN ,L ,LDA ,LDB ,LDC ,LDD ,LDK ,LDO ,LDQ ,LDR ,
    LDRY ,LDS ,LDWORK ,M ,N ,NOBR ,NSMPL ;
  CHARACTER JOB ,JOBCK ,METH ;

  // .. Allocatable arrays ..
  // !Fortran 90/95 (Fixed dimensions should be used with Fortran 77.)
  int IWORK;
  int BWORK;
  int A, B, C, D, DWORK, K, Q, R, RY, S;
  // .. Local variables and constant dimension arrays ..
  double TEMP ;
  int ID ,IJOB ,IP ,ITMP ,LBWORK ,LDUNN ,LIWORK ,LL ,LNOBR ,MA ,
    MNOBR ,MNOBRN ,N2 ,NA ,NCOL ,NL ,NN ,NPL ,NR ,NRC ,TASK ;
  int PRINTW ;
  
  // Check for proper number of arguments.
  CheckRhs(6,11);
  CheckLhs(0,9);
  
  // 1 meth
  if (GetScalarInt (stack, 1 , &TASK) == FAIL)  return RET_BUG;
  if (TASK < 1 ||TASK > 3 )
    {
      Scierror ("METH HAS 1, 2, OR 3 THE ONLY ADMISSIBLE VALUES");
    }
  METH = (TASK == 1 ) ? 'M' : ((TASK == 2 ) ? 'N': 'C');
  // 2 job
  if (GetScalarInt (stack, 2 , &IJOB) == FAIL)  return RET_BUG;
  if (IJOB < 1 ||IJOB > 4 ){ 
    Scierror ("JOB HAS 1, 2, 3 OR 4 THE ONLY ADMISSIBLE VALUES");
  }
  JOB = (IJOB == 1 ) ? 'A': (IJOB == 2 ) ? 'C' : (IJOB == 3 ) ? 'B': 'D';
  //         
  //   s
  if (GetScalarInt (stack, 3, &NOBR) == FAIL)  return RET_BUG;
  if (NOBR < 1 ){ 
    Scierror ("S MUST BE A POSITIVE int");
  }
  //   n
  if (GetScalarInt (stack, 4, &N) == FAIL)  return RET_BUG;
  if (N < 1 ){ 
    Scierror ("N MUST BE A POSITIVE int");
  } 
  if (N >= NOBR )
    { 
      Scierros("Error: THE ORDER SHOULD BE AT MOST %d\n",NOBR-1);
      return RET_BUG;
    } 
  //  l
  if (GetScalarInt (stack, 5, &L) == FAIL)  return RET_BUG;
  if (L < 1 ){ 
    Scierror ("THE SYSTEM HAS NO OUTPUTS");
  } 
  // R(nr,nr)
  if(( R=GetRealMatCopy(stack,6)) ==NULL) return RET_BUG;
  NR = R->m;
  NCOL = R->n;
  if (NR < 2 *L ){ 
    Scierror("R MUST HAVE AT LEAST %d ROWS", 2*L );
  } 
  if (NCOL < NR ){ 
    Scierror("R MUST HAVE AT LEAST %d COLUMNS", NCOL);
  } 
  //   m
  M =NR /(2*NOBR )-L ;
  //   tol
  TOL =ZERO;
  if ( rhs >= 7 ){
    if (GetScalarInt (stack, 7 , &TOL) == FAIL)  return RET_BUG;
  } 
  //   t
  NSMPL =0;
  JOBCK ='N';
  if (rhs >= 8 )
    { 
      JOBCK ='K';
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
  //   A(n,n)
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
	Scierror("A MUST HAVE %d ROWS AND COLUMNS'", N);
      } 
      //   C(l,n)
      if(( C=GetRealMatCopy(stack,10)) ==NULL) return RET_BUG;
      MA = C->m;
      NA =->n;
      if (MA != L )
	{ 
	  Scierror("C MUST HAVE %d ROWS",L );
	} 
      if (NA != N ){ 
	Scierror("C MUST HAVE %d COLUMNS",N);
      } 
    }
  //     printw
  //   
  PRINTW =.FALSE.;
  if ( rhs >= 11 )
    {
      if (GetScalarInt (stack, 11 , &PRINTW) == FAIL)  return RET_BUG;
      if (ITMP < 0 ||ITMP > 1 )
	{ 
	  Scierror ("PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES");
	  return RET_BUG;
	} 
      PRINTW =ITMP == 1 ;
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
      LIWORK =Max (LIWORK ,LNOBR ,M *NPL );
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
    if (IJOB.LE.2 ){ 
      LDWORK =Max (LDWORK ,2*LDUNN +N2 ,LDUNN +NN +7*N );
    } 
  } 
  
  if ((M > 0 && IJOB != 2 )||TASK >= 2 ){ 
    LDWORK =Max (LDWORK ,2*LDUNN +NN +ID +7*N );
    if (TASK == 1 )LDWORK =Max (LDWORK ,LDUNN +N +6*MNOBR ,LDUNN +
				N +Max (L +MNOBR ,LNOBR +Max (3*LNOBR ,M )));
  }
  else { 
    if (TASK != 2 ){IAW =N +NN ;
    } 
  }
  if (TASK != 1 ||NSMPL > 0 ){ 
    LDWORK =Max (LDWORK ,LDUNN +IAW +N2 +Max (5*N ,LNOBR +2*MNOBR +
					      L ),ID +4*MNOBRN ,ID +MNOBRN +NPL );
    if (TASK != 1 && M > 0 && IJOB != 2 )
      LDWORK =Max (LDWORK ,
		   MNOBR *NPL *(M *NPL +1)+Max (NPL **2,4*M *NPL +1));
    LDWORK =LNOBR *N +LDWORK ;
  } 

  if (NSMPL > 0 )
    LDWORK =Max (LDWORK ,4*NN +2*NL +LL +Max (3*L , NL ),14*NN +12*N +5);

  // Allocate variable dimension local arrays.
  // !Fortran 90/95
  
  if((A = nsp_matrix_create(NVOID,'r',LDA,N)) == NULL) goto err;;
  if((B = nsp_matrix_create(NVOID,'r',LDB,M)) == NULL) goto err;;
  if((C = nsp_matrix_create(NVOID,'r',LDC,N)) == NULL) goto err;;
  if((D = nsp_matrix_create(NVOID,'r',LDD,M)) == NULL) goto err;;
  if((DWORK = nsp_matrix_create(NVOID,'r',LDWORK,1)) == NULL) goto err;;
  if((IWORK = nsp_matrix_create(NVOID,'r',LIWORK,1)) == NULL) goto err;;
  if((Q = nsp_matrix_create(NVOID,'r',LDQ,N)) == NULL) goto err;;
  if((R = nsp_matrix_create(NVOID,'r',LDR,NCOL)) == NULL) goto err;;
  if((RY = nsp_matrix_create(NVOID,'r',LDRY,L)) == NULL) goto err;;
  if((S = nsp_matrix_create(NVOID,'r',LDS,L)) == NULL) goto err;;
  if((B = nsp_matrix_create(NVOID,'r', LBWORK,1)) == NULL) goto err;;
  if((K = nsp_matrix_create(NVOID,'r',LDK,L)) == NULL) goto err;;

  // Copy inputs from MATLAB workspace to locally allocated arrays.
  //      
  COPY (MXGETPR (PRHS (6)),R->R,LDR *NCOL );
  if (TASK >= 2 && IJOB >= 3 ){ 
    COPY  (MXGETPR (PRHS (9)),A->R,LDA *N );
    COPY (MXGETPR (PRHS (10)),C->R,LDC *N );
  } 
  // Do the actual computations.

  IB01BD (METH ,JOB ,JOBCK ,NOBR ,N ,M ,L ,NSMPL ,R->R,LDR ,
	  A->R,LDA ,C->R,LDC ,B->R,LDB ,D->R,LDD ,Q->R,LDQ ,
	  RY->R,LDRY ,S->R,LDS ,K->R,LDK ,TOL ,iIWORK->R,
	  DWORK->R,LDWORK ,iBWORK->R,IWARN ,INFO );
  if (IWARN != 0 && PRINTW ){ 
    Sciprintf("  IWARN = %d ON EXIT FROM IB01BD", IWARN);
  } 
  if (INFO != 0 )
    { 
      Scierror("INFO =  %d ON EXIT FROM IB01BD", INFO );
    }
  // Copy output to MATLAB workspace.
  IP = 0;
  if (IJOB.LE.2 )
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
}
