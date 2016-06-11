
#if 0

/* 
  {(Myinterfun) fortran_mex_gateway, C2F(sident),"sident"},
  {(Myinterfun) fortran_mex_gateway, C2F(sorder),"sorder"},
  {(Myinterfun) fortran_mex_gateway, C2F(findbd),"findbd"},
  {(Myinterfun) sci_gateway, intrankqr,"rankqr"},
  {(Myinterfun) sci_gateway, intab01od,"contr"},
  {(Myinterfun) sci_gateway, C2F(intmucomp),"mucomp"},
  {(Myinterfun) sci_gateway, C2F(intricc2),"pet_ricc"},
  {(Myinterfun) sci_gateway, C2F(inthinf),"hinf"},
  {(Myinterfun) sci_gateway, C2F(intdhinf),"dhinf"},
  {(Myinterfun) sci_gateway, C2F(intlinmeq),"linmeq"},
};

extern Gatefunc C2F(sident);
extern Gatefunc C2F(sorder);
extern Gatefunc C2F(findbd);
extern Gatefunc C2F(intmucomp);
extern Gatefunc C2F(intricc2);
extern Gatefunc C2F(inthinf);
extern Gatefunc C2F(intdhinf);
extern Gatefunc C2F(intlinmeq);
extern int C2F(intmb03od) __PARAMS((char *fname, unsigned long fname_len));
extern int C2F(intzb03od) __PARAMS((char *fname, unsigned long fname_len));
extern int C2F(ab01od)();
*/

int intrankqr(fname)
     char* fname;
{
  int *header1;
  int Cmplx;

  header1 = (int *) GetData(1);
  Cmplx=header1[3];
  if (Cmplx==0) {
    C2F(intmb03od)("rankqr",6L);
    return 0; } 
  else
    {
      C2F(intzb03od)("rankqr",6L);
      return 0; } 
}

int intab01od()
{ 
  int mA,nA,ptrA, mB,nB, ptrB;
  int A,B,U,KSTAIR,V, ptrIWORK,ptrU,ptrTOL;
  int ptrKSTAIR,ptrV,ptrDWORK,ptrJUNK,ptrNCONT;
  int LDA, LDB, LDU, LDV, LDWORK;
  int N, M, mtol, ntol, i, j, un; int INFO, INDCON, NCONT;
  char  *JOBU, *JOBV;
  double theTOL;
  int minlhs=1, minrhs=2, maxlhs=6, maxrhs=3;

  /*     [NCONT,U,KSTAIR,V,A,B]=ab01od(A,B,[TOL])   */

  CheckRhs(minrhs,maxrhs);  CheckLhs(minlhs,maxlhs);
  theTOL=(double) C2F(dlamch)("e",1L);
  GetRhsVar(1,"d",&mA,&nA,&ptrA);   A=1;        /*     A */
  N=mA;
  theTOL=0.2*sqrt(2*theTOL)*N;
  GetRhsVar(2,"d",&mB,&nB,&ptrB);   B=2;        /*     B */
  M=nB;
  if (nA != mB || mA != nA )
    { Scierror(999,"Invalid A,B matrices \r\n");  return 0; }
  if (Rhs == 3) {
    /*    TOL is given:   ab01od(A,B,tol)   */
    GetRhsVar(3,"d",&mtol,&ntol,&ptrTOL);  theTOL=*stk(ptrTOL);    /*     TOL */
    if (theTOL>1.0||theTOL<0.0) {
      Scierror(999,"TOL must be in [0 1]\r\n");  return 0; 
    }
  }

  /*     dimensions...    */
  LDA=MAX(1,N);  LDB=LDA;  LDU=LDA; LDV=MAX(1,M);
  LDWORK = MAX(1, N*M + MAX(N,M) + MAX(N,3*M));

  /*     other parameters of AB01OD   */
  JOBU= "N"; if (Lhs >= 2)  JOBU="I";
  JOBV= "N"; if (Lhs >= 4)  JOBV="I";

  /*     creating NCONT,U,KSTAIR,V,IWORK,DWORK   */
  nsp_matrix_create(Rhs+1,"i",(i=1,&i),(j=1,&j),&ptrNCONT);  NCONT=Rhs+1;
  nsp_matrix_create(Rhs+2,"d",&N,&N,&ptrU);  U=Rhs+2;
  nsp_matrix_create(Rhs+3,"i",(un=1,&un),&N,&ptrKSTAIR);  KSTAIR=Rhs+3;
  nsp_matrix_create(Rhs+4,"d",&M,&M,&ptrV);  V=Rhs+4;
  nsp_matrix_create(Rhs+5,"i",(un=1,&un),&M,&ptrIWORK);
  nsp_matrix_create(Rhs+6,"d",(un=1,&un),&LDWORK,&ptrDWORK);

  C2F(ab01od)( "A", JOBU, JOBV, &N, &M, stk(ptrA), &LDA, 
               stk(ptrB), &LDB, stk(ptrU), &LDU, stk(ptrV), &LDV, 
	       istk(ptrNCONT), &INDCON, istk(ptrKSTAIR), &theTOL,  
               istk(ptrIWORK), stk(ptrDWORK), &LDWORK, &INFO );
  if (INFO != 0) {
    C2F(errorinfo)("ab01od", &INFO, 6L);
    return 0;
  }
  if (Lhs >= 3) {
    int ii;
    /*     resizing KSTAIR      */
/*     nsp_matrix_create(Rhs+7,"i",(un=1,&un),&INDCON,&ptrJUNK);  */
/*     KSTAIR=Rhs+7; */
/*     C2F(icopy)(&INDCON,istk(ptrKSTAIR),(un=1,&un),istk(ptrJUNK),(ii=1,&ii)); } */
/*   /\*     lhs variables: [NCONT,U,KSTAIR,V,A,B]=ab01od(A,B)   *\/ */
/*   LhsVar(1)=NCONT; */
/*   LhsVar(2)=U;  */
/*   LhsVar(3)=KSTAIR; */
/*   LhsVar(4)=V;  */
/*   LhsVar(5)=A; */
/*   LhsVar(6)=B; */
/*   return 0; */
/* } */

/* /\* */
/*   C FINDBD.F - Gateway function for computing the initial state and/or */
/*   C            the matrices B and D of a discrete-time linear system,  */
/*   C            given the system matrices A, C, and possibly B, D, and the */
/*   C            input and output trajectories, using SLICOT routine IB01CD. */
/* *\/ */

/* int finddb() */
/* { */
/*   double ZERO ; */
/*   static const int ZERO =0.0D0; */
/*   // .. Scalar parameters used by SLICOT subroutines .. */
/*   double TOL ; */
/*   int INFO ,IWARN ,L ,LDA ,LDB ,LDC ,LDD ,LDU ,LDV ,LDWORK , LDY ,M ,N ,NSMP ; */
/*   CHARACTER COMUSE ,JOB ,JOBX0 ; */

/*   // .. Allocatable arrays .. */
/*   // !Fortran 90/95 (Fixed dimensions should be used with Fortran 77.) */
/*   int IWORK; */
/*   int A, B, C, D, DWORK, U, V, X0, Y; */

/*   // .. Local variables and constant dimension arrays .. */
/*   double TEMP ; */
/*   CHARACTER *120TEXT ; */
/*   int CSIZE ,CUSE ,IC ,IJOB ,INI ,IP ,IPS ,IQ ,ISIZE ,ITMP , */
/*     LDW1 ,LDW2 ,LDW3 ,LDWMIN ,LIWORK ,MA ,MINSMP ,MINWLS ,NA , */
/*     NCOL ,NCP1 ,TASK ; */
/*   LOGICAL PRINTW ; */
  
/*   // .. Cache size in double precision words ..; */
/*   static const int CSIZE =64000; */
/*   // Check for proper number of arguments. */

/*   CheckRhs(2,2); */
/*   CheckLhs(0,1); */
  
/*   // Check dimensions of input parameters and read/set scalar parameters. */

/*   //   jobx0 */
/*   if (GetScalarInt (stack, 1, &TASK) == FAIL)  return RET_BUG; */
/*   if (TASK < 0 ||TASK > 1 ){  */
/*     Scierror ( */
/* 		"JOBX0 HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES") */
/*     }  */
/*   JOBX0 = (TASK == 1 ) ? 'X' : 'N'; */
/*   //   comuse */
/*   if (GetScalarInt (stack, 2, &CUSE) == FAIL)  return RET_BUG; */
/*   if (CUSE < 1 ||CUSE > 3 ) */
/*     {  */
/*       Scierror ("COMUSE HAS 1, 2, OR 3 THE ONLY ADMISSIBLE VALUES"); */
/*     } */
/*   COMUSE = (CUSE == 1 ) ? 'C' : ( (CUSE == 2 ) ? 'U' : 'N'); */
    
/*   if (TASK == 1 && CUSE == 3 ) */
/*     {  */
/*       if (NRHS < 5 ) */
/* 	{  */
/* 	  Scierror ("FINDBD REQUIRES AT LEAST 5 INPUT ARGUMENTS"); */
/* 	}  */
/*     } */
/*   else */
/*     { */
/*       if (CUSE == 1 ) */
/* 	{  */
/* 	  if (NRHS < 6 ) */
/* 	    {  */
/* 	      Scierror ("FINDBD REQUIRES AT LEAST 6 INPUT ARGUMENTS"); */
/* 	    }  */
/* 	}  */
/*       //   job */
/*       if ((TASK == 1 && CUSE == 2 )||CUSE == 1 ) */
/* 	{ */
/* 	  if (GetScalarInt (stack, 3, &IJOB) == FAIL)  return RET_BUG; */
/* 	  if (IJOB < 1 ||IJOB > 2 ){  */
/* 	    Scierror ("JOB HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES"); */
/* 	  }  */
/* 	  IP =4; */
/* 	  if (IJOB == 1 ){  */
/* 	    JOB ='B'; */
/* 	  } else {  */
/* 	    JOB ='D'; */
/* 	  }  */
/* 	} */
/*       else {  */
/* 	IP =3; */
/* 	JOB ='B'; */
/* 	IJOB =1; */
/*       }  */
/*       if (TASK == 1 && CUSE == 2 ) */
/* 	{  */
/* 	  if (IJOB == 1 && NRHS < 7 ){  */
/*             Scierror ( "FINDBD REQUIRES AT LEAST 7 INPUT ARGUMENTS"); */
/* 	  } */
/* 	  else */
/* 	    { */
/* 	      if (IJOB == 2 && NRHS < 8 ) */
/* 		{  */
/* 		  Scierror ( "FINDBD REQUIRES AT LEAST 8 INPUT ARGUMENTS"); */
/* 		}  */
/* 	    }  */
/* 	  c */
/* 	    IPS =IP  */
/* 	    N =0 */
/* 	    M =0 */
/* 	    L =1 */
/* 	    NSMP =1 */
	    
/* //   A(n,n) */

/*       if (NRHS >= IP ){  */
/*          N =MXGETM (PRHS (IP )) */
/*          NA =MXGETN (PRHS (IP )) */
/*          if (NA != N ){  */
/* 	   WRITE (TEXT ,"(''A MUST HAVE '',I6,'' ROWS AND COLUMNS')"); */
/*               N  */
/*             Scierror (TEXT ) */
/*          }  */
/*          if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )) */
/*             == 1){  */
/*             Scierror ("A MUST BE A REAL MATRIX") */
/*          }  */
/*          IP =IP +1 */
/*       }  */

/* //   B(n,m) */

/*       if (TASK == 1 && CUSE == 2 && NRHS >= IP ){  */
/*          NA =MXGETM (PRHS (IP )) */
/*          M =MXGETN (PRHS (IP )) */
/*          if (NA != N ){  */
/*             WRITE (TEXT ,"(''B MUST HAVE '',I6,'' ROWS'')")N  */
/*             Scierror (TEXT ) */
/*          }  */
/*          if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )) */
/*             == 1){  */
/*             Scierror ("B MUST BE A REAL MATRIX") */
/*          }  */
/*          IP =IP +1 */
/*       }  */

/* //   C(l,n) */

/*       if (NRHS >= IP ){  */
/*          L =MXGETM (PRHS (IP )) */
/*          NA =MXGETN (PRHS (IP )) */
/*          if (NA != N ){  */
/*             WRITE (TEXT ,"(''C MUST HAVE '',I6,'' COLUMNS'')")N  */
/*             Scierror (TEXT ) */
/*          }  */
/*          if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )) */
/*             == 1){  */
/*             Scierror ("C MUST BE A REAL MATRIX") */
/*          }  */
/*          IP =IP +1 */

/*          if (L.LE.0 ){  */
/*             Scierror ("THE SYSTEM HAS NO OUTPUTS") */
/*          }  */
/*       }  */

/* //   D(l,m) */
/*       if (TASK == 1 && CUSE == 2 && IJOB == 2 && NRHS >= IP ) */
/* 	{  */
/* 	  NA =MXGETM (PRHS (IP )); */
/* 	  MA =MXGETN (PRHS (IP )); */
/* 	  if (NA != L ) */
/* 	    {  */
/* 	      WRITE (TEXT ,"(''D MUST HAVE '',I6,'' ROWS'')" L ); */
/* 	      Scierror (TEXT ); */
/*          }  */
/*          if (MA != M ){  */
/* 	   WRITE (TEXT ,"(''D MUST HAVE '',I6,'' COLUMNS'" M ); */
/* 	   Scierror (TEXT ); */
/*          }  */
/*          if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )) == 1) */
/* 	   {  */
/* 	     Scierror ("D MUST BE A REAL MATRIX"); */
/* 	   }  */
/*          IP =IP +1; */
/* 	}  */

/* //  Y(txp), U(txm) */

/*       if (NRHS >= IP ) */
/* 	{  */
/* 	  NSMP =MXGETM (PRHS (IP )) */
/* 	    if (MXGETN (PRHS (IP )) != L ){  */
/* 	      WRITE (TEXT ,"('' Y MUST HAVE '',I6,'' COLUMNS'')")L  */
/*             Scierror (TEXT ) */
/*          }  */
/*          if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )) */
/*             == 1){  */
/*             Scierror ("Y MUST BE A REAL MATRIX") */
/*          }  */
/*          IP =IP +1 */
/*       }  */

/*       if (((TASK == 1 && CUSE == 2 )||CUSE == 1 ).AND.NRHS >= IP ) */
/*         {  */
/*          MA =MXGETN (PRHS (IP )) */
/*          if (CUSE == 2 && MA != M ){  */
/*             WRITE (TEXT ,"(''U MUST HAVE '',I6,'' COLUMNS'')")M  */
/*             Scierror (TEXT ) */
/*          } else {  */
/*             M =MA  */
/*          }  */

/*          if (M > 0 ){  */
/*             if (MXGETM (PRHS (IP )) != NSMP ){  */
/*                Scierror ( */
/*                  "U MUST HAVE THE SAME ROW DIMENSION AS Y") */
/*             }  */
/*             if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP ) */
/*               ) == 1){  */
/*                Scierror ("U MUST BE A REAL MATRIX") */
/*             }  */
/*          }  */
/*          IP =IP +1 */
/*       }  */

/*       if (CUSE == 1 ){  */
/*          NCOL =N *M  */
/*          if (TASK == 1 )NCOL =NCOL +N  */
/*          MINSMP =NCOL  */
/*          if (IJOB == 2 ){  */
/*             MINSMP =MINSMP +M  */
/*             IQ =MINSMP  */
/*          } else { if (TASK == 0 ){  */
/*             IQ =MINSMP  */
/*             MINSMP =MINSMP +1 */
/*          } else {  */
/*             IQ =MINSMP  */
/*          }  */
/*       } else {  */
/*          NCOL =N  */
/*          if (TASK == 1 ){  */
/*             MINSMP =N  */
/*          } else {  */
/*             MINSMP =0 */
/*          }  */
/*          IQ =MINSMP  */
/*       }  */

/*       if (NSMP < MINSMP ){  */
/*          WRITE (TEXT ,"('' THE NUMBER OF SAMPLES SHOULD BE AT LEAST '',I10))" MINSMP  */
/*          Scierror (TEXT ) */
/*       }  */

/* //     tol */
/* //    */
/*       TOL =ZERO  */
/*       if (NRHS >= IP ){  */
/*          if (MXGETM (PRHS (IP )) != 1 || MXGETN (PRHS (IP )) != 1){  */
/*             Scierror ("TOL MUST BE A SCALAR") */
/*          }  */
/*          if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )) */
/*             == 1){  */
/*               Scierror ("TOL MUST BE A REAL SCALAR") */
/*          }  */
/*          MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),TOL ,1) */
/*          IP =IP +1 */
/*       }  */

/* //     printw */
/* //    */
/*       PRINTW =.FALSE. */
/*       if (NRHS >= IP ){  */
/*          if (MXGETM (PRHS (IP )) != 1||MXGETN (PRHS (IP )) != 1){  */
/*             Scierror ("PRINTW MUST BE A SCALAR") */
/*          }  */
/*          if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )) */
/*             == 1){  */
/*             Scierror ("PRINTW MUST BE AN int SCALAR") */
/*          }  */
/*          MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),TEMP ,1) */
/*          ITMP =TEMP  */
/*          if (ITMP < 0 ||ITMP > 1 ){  */
/*             Scierror ( */
/*               "PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES") */
/*          }  */
/*          PRINTW =ITMP == 1  */
/*          IP =IP +1 */
/*       }  */

/* // Determine the lenghts of working arrays. */
/* // The default value for LDWORK is computed using the formula */
/* //      LDWORK = MAX( minimum value needed, 2*CSIZE/3, */
/* //                   CSIZE - ( M + L )*NSMP - 2*N*( N + M + L ) - L*M ) */
/* // where CSIZE is the cache size in double precision words.; */
/* // If LDWORK is specified, */
/* //        but it is less than the minimum workspace size  */
/* // needed, that minimum value is used instead. */

/*       LDA =MAX (1,N ) */
/*       LDB =LDA  */
/*       LDC =L  */
/*       LDD =LDC  */
/*       LDV =LDA  */
/*       LDY =MAX (1,NSMP ) */
/*       if (M > 0 && ((TASK == 1 && CUSE == 2 )||CUSE == 1 )){  */
/*          LDU =LDY  */
/*       } else {  */
/*          LDU =1 */
/*       }  */

/*       LIWORK =NCOL  */
/*       if ((TASK == 0 && CUSE != 1 )||MAX (N ,M ) == 0){  */
/*          LDWORK =2 */
/*       } else {  */
/*          if (IJOB == 2 )LIWORK =MAX (LIWORK ,M ) */
/*          IQ =IQ *L  */
/*          NCP1 =NCOL +1 */
/*          ISIZE =NSMP *L *NCP1  */
/*          if (N > 0 && TASK == 1 ){  */
/*             if (CUSE == 1 ){  */
/*                IC =2*N *N +N  */
/*             } else {  */
/*                IC =2*N *N  */
/*             }  */
/*          } else {  */
/*             IC =0 */
/*          }  */
/*          MINWLS =NCOL *NCP1  */
/*          if (CUSE == 1 ){  */
/*             if (IJOB == 2 )MINWLS =MINWLS +L *M *NCP1  */
/*             if (M > 0 && IJOB == 2 ){  */
/*                IA =M +MAX (2*NCOL ,M ) */
/*             } else {  */
/*                IA =2*NCOL  */
/*             }  */
/*             LDW1 =N *N *M +MAX (IC ,IA ) */
/*             if (TASK == 1 )LDW1 =LDW1 +L *N  */
/*             LDW2 =ISIZE +MAX (N +MAX (IC ,IA ),6*NCOL ) */
/*             LDW3 =MINWLS +MAX (IQ *NCP1 +LDW1 ,6*NCOL ) */
/*             if (M > 0 && IJOB == 2 ){  */
/*                LDW2 =MAX (LDW2 ,ISIZE +2*M *M +6*M ) */
/*                LDW3 =MAX (LDW3 ,MINWLS +2*M *M +6*M ) */
/*                INI =3 */
/*             } else {  */
/*                INI =2 */
/*             }  */
/*          } else {  */
/*             ITAU =IC +L *N  */
/*             LDW2 =ISIZE +2*N +MAX (IC ,4*N ) */
/*             LDW3 =MINWLS +2*N +MAX (IQ *NCP1 +ITAU ,4*N ) */
/*             INI =2 */
/*          }  */
/*          LDWMIN =INI +N *(N +M +L )+MAX (5*N ,INI ,MIN (LDW2 ,LDW3 )) */
/*          LDWORK =MAX (LDWMIN ,2*CSIZE /3,CSIZE -(M +L )*NSMP -2*N *(N + */
/*            M +L )-L *M ) */
/*       }  */

/* //   ldwork */
/* //    */
/*       if (NRHS >= IP ){  */
/*          if (MXGETM (PRHS (IP )) != 1||MXGETN (PRHS (IP )) != 1){  */
/*             Scierror ("LDWORK MUST BE A SCALAR") */
/*          }  */
/*          if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )) */
/*             == 1){  */
/*               Scierror ("LDWORK MUST BE A REAL SCALAR") */
/*          }  */
/*          MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),TEMP ,1) */
/*          ITMP =INT (TEMP ) */
/*          if (ITMP < LDWMIN ){  */
/*             LDWORK =LDWMIN  */
/*          } else {  */
/*             LDWORK =ITMP  */
/*          }  */
/*       }  */

/* // Allocate variable dimension local arrays. */
/* // !Fortran 90/95 */

/*       IF(.NOT.CREATEVAR(NBVARS+1,'d',LDA,N,A)) RETURN */
/*       IF(.NOT.CREATEVAR(NBVARS+1,'d',LDB,M,B)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDC,N,C)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDD,M,D)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDWORK,1,DWORK)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'i',LIWORK,1,IWORK)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDU,M,U)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDV,N,V)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',N,1,X0)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDY,L,Y)) RETURN

// Copy inputs from MATLAB workspace to locally allocated arrays.
//      
      IP =IPS 
      if (NRHS >= IP ){ 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),stk(A),LDA *N )
         IP =IP +1
         if (TASK == 1 && CUSE == 2 ){ 
            if (N > 0 ){ 
               MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),stk(B),
                 LDB *M )
            } 
            IP =IP +1
         } 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),stk(C),LDC *N )
         IP =IP +1
         if (TASK == 1 && CUSE == 2 && IJOB == 2 ){ 
            MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),stk(D),LDD *M )
            IP =IP +1
         } 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),stk(Y),NSMP *L )
         IP =IP +1
         if (CUSE == 1 ||(TASK == 1 && CUSE == 2 )){ 
            MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),stk(U),NSMP *M )
         } 
      } 

// Do the actual computations.

      IB01CD (JOBX0 ,COMUSE ,JOB ,N ,M ,L ,NSMP ,stk(A),LDA ,stk(B)
        ,LDB ,stk(C),LDC ,stk(D),LDD ,stk(U),LDU ,stk(Y),LDY ,stk(X0),
        stk(V),LDV ,TOL ,istk(IWORK),stk(DWORK),LDWORK ,IWARN ,INFO )
      if (IWARN != 0 && PRINTW ){ 
         WRITE (TEXT ,"(''  IWARN = '',I4,'' ON EXIT FROM IB01CD')")
           IWARN 
      } 
      if (INFO != 0 ){ 
         WRITE (TEXT ,"(''INFO = '',I4,'' ON EXIT FROM IB01CD')")INFO 
      } else { 

// Copy output to MATLAB workspace.

         IP =1
         if (TASK == 1 ||(TASK == 0 && CUSE == 2 )){ 
            PLHS (IP )=MXCREATEFULL (N ,1,0)
            MXCOPYREAL8TOPTR (stk(X0),MXGETPR (PLHS (IP )),N )
            IP =IP +1
         } 
//         
         if (CUSE == 1 && NLHS >= IP ){ 
            PLHS (IP )=MXCREATEFULL (N ,M ,0)
            MXCOPYREAL8TOPTR (stk(B),MXGETPR (PLHS (IP )),N *M )
            IP =IP +1
            if (NLHS >= IP && IJOB == 2 ){ 
               PLHS (IP )=MXCREATEFULL (L ,M ,0)
               MXCOPYREAL8TOPTR (stk(D),MXGETPR (PLHS (IP )),L *M )
               IP =IP +1
            } 
         } 

         if ((TASK == 1 ||CUSE == 1 ).AND.NLHS >= IP ){ 
            PLHS (IP )=MXCREATEFULL (N ,N ,0)
            MXCOPYREAL8TOPTR (stk(V),MXGETPR (PLHS (IP )),N *N )
            IP =IP +1
         } 
//         
         if (NLHS >= IP ){ 
            NO =1
            if (CUSE == 1 && M > 0 && IJOB == 2 )NO =2
            PLHS (IP )=MXCREATEFULL (NO ,1,0)
            MXCOPYREAL8TOPTR (stk(DWORK+2-1),MXGETPR (PLHS (IP )),
              NO )
         } 
      } 

// Deallocate local arrays.
// !Fortran 90/95

//     DEALLOCATE( A, B, C, D, V, U, Y, IWORK, DWORK, X0 )

// Error and warning handling.

      if (IWARN != 0 && PRINTW ){ 
         MEXPRINTF (TEXT )
      } 

      if (INFO != 0 ){ 
         Scierror (TEXT )
      } 
}

// ORDER.F  - Gateway function for computation of the order of a
//            discrete-time system using SLICOT routine IB01AD.

// RELEASE 4.0, WGS COPYRIGHT 2000.

// Matlab call:
//   [R(,n,sval,rcnd)] = order(meth,alg,jobd,batch,conct,s,Y(,U,tol,
//                             printw,ldwork,R))
// Purpose:
//   To preprocess the input-output data for estimating the matrices 
//   of a linear time-invariant dynamical system, using Cholesky or
//   QR factorization and subspace identification techniques (MOESP 
//   and N4SID), and to find an estimate of the system order. The
//   input-output data can, optionally, be processed sequentially.

int sorder()
{
	      double ZERO ,ONE ;
	      static const int ZERO =0.0D0,ONE =1.0D0;

// .. Scalar parameters used by SLICOT subroutines ..
	double TOL1 ,TOL2 ;
	int INFO ,IWARN ,L ,LDR ,LDU ,LDWORK ,LDY ,M ,N ,NOBR ,NSMP ;
      CHARACTER ALG ,BATCH ,CONCT ,CTRL ,JOBD ,METH 

// .. Allocatable arrays ..
// !Fortran 90/95 (Fixed dimensions should be used with Fortran 77.)
	int IWORK;
      int DWORK, R, SV, U, Y;

// .. Local variables and constant dimension arrays ..
	double TEMP ,TOL (2);
      int CSIZE ,IALG ,IBCH ,ICNC ,IJOB ,IMTH ,IP ,ISIZE ,ITMP ,
        LDWMIN ,LIWORK ,NCOL ,NR ,NRSAVE ,NSMPMN ;
      CHARACTER *120TEXT 
      LOGICAL PRINTW 

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

      if (NRHS < 7 ){ 
         Scierror ("ORDER REQUIRES AT LEAST 7 INPUT ARGUMENTS") 
      } else { if (NLHS < 1 ){ 
         Scierror (
           "ORDER REQUIRES AT LEAST 1 OUTPUT ARGUMENTS")
      } 

// Check dimensions of input parameters and read/set scalar parameters.

//   meth

      if (MXGETM (PRHS (1)) != 1 || MXGETN (PRHS (1)) != 1){ 
         Scierror ("METH MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (1)) == 0||MXISCOMPLEX (PRHS (1)) == 1)
        { 
         Scierror ("METH MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (1)),TEMP ,1)
      IMTH =TEMP 
      if (IMTH < 1 ||IMTH > 2 ){ 
         Scierror (
           "METH HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES")
      } 

      if (IMTH == 1 ){ 
         METH ='M'
      } else { 
         METH ='N'
      } 

//   alg

      if (MXGETM (PRHS (2)) != 1 || MXGETN (PRHS (2)) != 1){ 
         Scierror ("ALG MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (2)) == 0||MXISCOMPLEX (PRHS (2)) == 1)
        { 
         Scierror ("ALG MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (2)),TEMP ,1)
      IALG =TEMP 
      if (IALG < 1 ||IALG > 3 ){ 
         Scierror ("ALG HAS 1, 2 OR 3 THE ONLY ADMISSIBLE VALUES")
      } 

      if (IALG == 1 ){ 
         ALG ='C'
      } else { if (IALG == 2 ){ 
         ALG ='F'
      } else { 
         ALG ='Q'
      } 

//   jobd

      if (MXGETM (PRHS (3)) != 1 || MXGETN (PRHS (3)) != 1){ 
         Scierror ("JOBD MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (3)) == 0||MXISCOMPLEX (PRHS (3)) == 1)
        { 
         Scierror ("JOBD MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (3)),TEMP ,1)
      IJOB =TEMP 
      if (IJOB < 1 ||IJOB > 2 ){ 
         Scierror (
           "JOBD HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES")
      } 

      if (IJOB == 1 ){ 
         JOBD ='M'
      } else { 
         JOBD ='N'
      } 

//   batch

      if (MXGETM (PRHS (4)) != 1 || MXGETN (PRHS (4)) != 1){ 
         Scierror ("BATCH MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (4)) == 0||MXISCOMPLEX (PRHS (4)) == 1)
        { 
         Scierror ("BATCH MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (4)),TEMP ,1)
      IBCH =TEMP 
      if (IBCH < 1 ||IBCH > 4 ){ 
         Scierror ("
           BATCH HAS 1, 2, 3 OR 4 THE ONLY ADMISSIBLE VALUES")
      } 

      if (IBCH == 1 ){ 
         BATCH ='F'
      } else { if (IBCH == 2 ){ 
         BATCH ='I'
      } else { if (IBCH == 3 ){ 
         BATCH ='L'
      } else { 
         BATCH ='O'
      } 

//   conct

      if (MXGETM (PRHS (5)) != 1 || MXGETN (PRHS (5)) != 1){ 
         Scierror ("CONCT MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (5)) == 0||MXISCOMPLEX (PRHS (5)) == 1)
        { 
         Scierror ("CONCT MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (5)),TEMP ,1)
      ICNC =TEMP 
      if (ICNC < 1 ||ICNC > 2 ){ 
         Scierror (
           "CONCT HAS 1 OR 2 THE ONLY ADMISSIBLE VALUES")
      } 

      if (ICNC == 1 ){ 
         CONCT ='C'
      } else { 
         CONCT ='N'
      } 

//   ctrl is set to 'N'

      CTRL ='N'

//   s

      if (MXGETM (PRHS (6)) != 1 || MXGETN (PRHS (6)) != 1){ 
         Scierror ("S MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (6)) == 0||MXISCOMPLEX (PRHS (6)) == 1)
        { 
         Scierror ("S MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (6)),TEMP ,1)
      NOBR =TEMP 
      if (NOBR < 1 ){ 
         Scierror ("S MUST BE A POSITIVE int")
      } 

//   Y(txp), (U(txm))

      NSMP =MXGETM (PRHS (7))
      L =MXGETN (PRHS (7))

      if (L.LE.0 ){ 
         Scierror ("THE SYSTEM HAS NO OUTPUTS")
      } 

      if (NRHS >= 8 ){ 
         M =MXGETN (PRHS (8))
      } else { 
         M =0
      } 
      NR =2*(M +L )*NOBR 
      if (IBCH.LE.2 ){ 
         NSMPMN =2*NOBR 
      } else { 
         NSMPMN =NR +2*NOBR -1
      } 

      if (NSMP < NSMPMN ){ 
         WRITE (TEXT ,"('' THE NUMBER OF SAMPLES SHOULD BE AT LEAST '',I10)"
           )NSMPMN 
         Scierror (TEXT )
      } 

      if (MXISNUMERIC (PRHS (7)) == 0||MXISCOMPLEX (PRHS (7)) == 1)
        { 
         Scierror ("Y MUST BE A REAL MATRIX")
      } 

      if (M > 0 ){ 
         if (MXISNUMERIC (PRHS (8)) == 0||MXISCOMPLEX (PRHS (8)) == 1)
           { 
            Scierror ("U MUST BE A REAL MATRIX")
         } 
         if (MXGETM (PRHS (8)) != NSMP ){ 
            Scierror (
              "U MUST HAVE THE SAME ROW DIMENSION AS Y")
         } 
      } 

//     tol
//   
      TOL1 =ZERO 
      TOL2 =-ONE 
      if (NRHS >= 9 ){ 
         ISIZE =MXGETM (PRHS (9))*MXGETN (PRHS (9))
         if (ISIZE > 2 )Scierror (
           "TOL MUST BE A VECTOR WITH AT MOST 2 ELEMENTS")
         if (MXISNUMERIC (PRHS (9)) == 0||MXISCOMPLEX (PRHS (9)) == 1)
           { 
              Scierror ("TOL MUST BE A REAL VECTOR")
         } 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (9)),TOL ,ISIZE )
         if (ISIZE > 0 )TOL1 =TOL (1)
         if (ISIZE > 1 )TOL2 =TOL (2)
      } 

      PRINTW =.FALSE.
      IP =10
      NCOL =NR 
      if (IBCH < 4 ){ 
         if (ICNC == 1 )NCOL =NCOL +1
         if (IALG == 2 )NCOL =NCOL +M +L +1
      } 
      NRSAVE =(NCOL -NR )*NR 

//     printw
//   
      if (NRHS >= IP ){ 
         if (MXGETM (PRHS (IP )) != 1||MXGETN (PRHS (IP )) != 1){ 
            Scierror ("PRINTW MUST BE A SCALAR")
         } 
         if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP ))
            == 1){ 
            Scierror ("PRINTW MUST BE AN int SCALAR")
         } 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),TEMP ,1)
         ITMP =TEMP 
         if (ITMP < 0 ||ITMP > 1 ){ 
            Scierror (
              "PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES")
         } 
         PRINTW =ITMP == 1 
         IP =IP +1
      } 

// Determine the lenghts of working arrays.
// The quasi-optimal value for LDWORK (assumed block-size 64) is possibly
// modified, so that the sum of the lenghts of DWORK and other needed
// arrays does not exceed the cache size. Specifically, the default value
// for LDWORK is computed using the formulas
//           nr = 2*( m + l )*s
//           LDWORK = ( t - 2*s + 3 + 64 )*nr
//           if ( CSIZE > MAX( nr*nr + t*( m + l ) + 16, 2*nr ) ) then
//              LDWORK = MIN( LDWORK, CSIZE - nr*nr - t*( m + l ) - 16 )
//           else
//              LDWORK = MIN( LDWORK, MAX( 2*nr, CSIZE/2 ) )
//           end if
//           LDWORK = MAX( minimum workspace size needed, LDWORK )
// where CSIZE is the cache size in double precision words.;
// If LDWORK is specified,
//        but it is less than the minimum workspace size 
// needed, that minimum value is used instead.

      LDY =NSMP 
      if (M > 0 ){ 
         LDU =LDY 
      } else { 
         LDU =1
      } 
      LDR =NR 
      if (IMTH == 1 && IJOB == 1 )LDR =MAX (NR ,3*M *NOBR )
      if (IMTH == 2 ){ 
         LIWORK =(M +L )*NOBR 
      } else { if (IALG == 2 ){ 
         LIWORK =M +L 
      } else { 
         LIWORK =1
      } 

//     The next statement is included in order to allow switching
//     from Cholesky or fast QR to QR algorithm.

      LDWORK =(NSMP -2*NOBR +3+64)*NR 
      if (CSIZE  > MAX (NR *NR +NSMP *(M +L )+16,2*NR )){ 
         LDWORK =MIN (LDWORK ,CSIZE -NR *NR -NSMP *(M +L )-16)
      } else { 
         LDWORK =MIN (LDWORK ,MAX (2*NR ,CSIZE /2))
      } 
      LDWMIN =2*NR *(NOBR +1)
      if (IALG == 2 )LDWMIN =MAX (LDWMIN ,2*NR *(M +L +1)+NR ,NR *(M +
        L +3))
      LDWORK =MAX (LDWORK ,LDWMIN )

//   ldwork
//   
      if (NRHS >= IP ){ 
         if (MXGETM (PRHS (IP )) != 1||MXGETN (PRHS (IP )) != 1){ 
            Scierror ("LDWORK MUST BE A SCALAR")
         } 
         if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP ))
            == 1){ 
              Scierror ("LDWORK MUST BE A REAL SCALAR")
         } 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),TEMP ,1)
         ITMP =INT (TEMP )
         if (ITMP < LDWMIN ){ 
            LDWORK =LDWMIN 
         } else { 
            LDWORK =ITMP 
         } 
         IP =IP +1

//     R(2*(m+p)*s,2*(m+p)*s(+c)), where
//     c = 1,       if conct = 1 and batch < 4;
//     c = 0,       if conct = 2 or  batch = 4;
//     c = c+m+l+1, if  alg  = 2 and batch < 4.
//   
         if (NRHS >= IP && (IBCH == 2 ||IBCH == 3 )){ 
            if (MXGETM (PRHS (IP )) != NR ){ 
               WRITE (TEXT ,"(''R MUST HAVE '',I5,'' ROWS'")NR 
               Scierror (TEXT )
            } 
            if (MXGETN (PRHS (IP )) != NCOL ){ 
               WRITE (TEXT ,"(''R MUST HAVE '',I5,'' COLUMNS'")NCOL 
               Scierror (TEXT )
            } 
            if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP )
              ) == 1){ 
               Scierror ("R MUST BE A REAL MATRIX")
            } 
         } 
      } 

// Allocate variable dimension local arrays.
// !Fortran 90/95

      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDR,NCOL,R)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',L*NOBR,1,SV)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDU,M,U)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDY,L,Y)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'i',LIWORK,1,IWORK)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDWORK,1,DWORK)) RETURN
      DSET(LDR*NCOL,0.0d0,stk(R),1)

// Copy inputs from MATLAB workspace to locally allocated arrays.
//      
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (7)),stk(Y),NSMP *L )

      if (M > 0 ){ 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (8)),stk(U),NSMP *M )
      } 
      if (NRHS >= IP && (IBCH == 2 ||IBCH == 3 )){ 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),stk(R),NR *NCOL )
         if (IALG == 2 ||ICNC == 1 ){ 
            if (LDR == NR ){ 
               DCOPY (NRSAVE ,stk(R+1-1 +(NR+1-1)*LDR),1,stk(DWORK)
                 ,1)
            } else { 
               DCOPY (NRSAVE ,stk(R+NR*NR+1-1 +(1-1)*LDR),1,stk(
                 DWORK),1)
            } 
         } 
      } 


// Do the actual computations.

      IB01AD (METH ,ALG ,JOBD ,BATCH ,CONCT ,CTRL ,NOBR ,M ,L ,
        NSMP ,stk(U),LDU ,stk(Y),LDY ,N ,stk(R),LDR ,stk(SV),TOL1 ,
        TOL2 ,istk(IWORK),stk(DWORK),LDWORK ,IWARN ,INFO )
      if (IWARN != 0 && PRINTW ){ 
         WRITE (TEXT ,"(''  IWARN = '',I4,'' ON EXIT FROM IB01AD'")
           IWARN 
      } 
      if (INFO != 0 ){ 
         WRITE (TEXT ,"(''INFO = '',I4,'' ON EXIT FROM IB01AD'")INFO 
         GO TO 20
      } 

// Copy output to MATLAB workspace.

      if (LDR > NR )DLACPY ('FULL',NR ,NR ,stk(R),LDR ,stk(R),NR )
      if ((IALG == 2 ||ICNC == 1 ).AND.IBCH.LE.2 ){ 
         if (LDR == NR ){ 
            DCOPY (NRSAVE ,stk(DWORK),1,stk(R+1-1 +(NR+1-1)*LDR),1)
         } else { 
            DCOPY (NRSAVE ,stk(DWORK),1,stk(R+NR*NR+1-1 +(1-1)*LDR)
              ,1)
         } 
      } 
      PLHS (1)=MXCREATEFULL (NR ,NCOL ,0)
      MXCOPYREAL8TOPTR (stk(R),MXGETPR (PLHS (1)),NR *NCOL )
      if (IBCH > 2 ){ 
         if (NLHS > 1 ){ 
            PLHS (2)=MXCREATEFULL (1,1,0)
            TEMP =N 
            MXCOPYREAL8TOPTR (TEMP ,MXGETPR (PLHS (2)),1)
            if (NLHS > 2 ){ 
               PLHS (3)=MXCREATEFULL (L *NOBR ,1,0)
               MXCOPYREAL8TOPTR (stk(SV),MXGETPR (PLHS (3)),
                 L *NOBR )
            } 
            if (IMTH == 2 && NLHS > 3 ){ 
               PLHS (4)=MXCREATEFULL (2,1,0)
               MXCOPYREAL8TOPTR (stk(DWORK+2-1),MXGETPR (PLHS (4)),
                 2)
            } 
         } 
      } 

// Deallocate local arrays.
// !Fortran 90/95

   20 CONTINUE 
// 
//     DEALLOCATE( R, SV, U, Y, IWORK, DWORK )

// Error and warning handling.

      if (IWARN != 0 && PRINTW ){ 
         MEXPRINTF (TEXT )
      } 

      if (INFO != 0 ){ 
         Scierror (TEXT )
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

      void sident()
      {
	
      double ZERO ;
      static const int ZERO =0.0D0;

      // .. Scalar parameters used by SLICOT subroutines ..
      double TOL ;
      int INFO ,IWARN ,L ,LDA ,LDB ,LDC ,LDD ,LDK ,LDO ,LDQ ,LDR ,
        LDRY ,LDS ,LDWORK ,M ,N ,NOBR ,NSMPL ;
      CHARACTER JOB ,JOBCK ,METH 

// .. Allocatable arrays ..
// !Fortran 90/95 (Fixed dimensions should be used with Fortran 77.)
      int IWORK;
      int BWORK;
      int A, B, C, D, DWORK, K, Q, R, RY, S;

// .. Local variables and constant dimension arrays ..
      double TEMP ;
      CHARACTER *120TEXT 
      int ID ,IJOB ,IP ,ITMP ,LBWORK ,LDUNN ,LIWORK ,LL ,LNOBR ,MA ,
        MNOBR ,MNOBRN ,N2 ,NA ,NCOL ,NL ,NN ,NPL ,NR ,NRC ,TASK ;
      LOGICAL PRINTW 


// Check for proper number of arguments.

      if (NRHS < 6 ){ 
         Scierror (
		   "SIDENT REQUIRES AT LEAST 6 INPUT ARGUMENTS")
      } else { if (NLHS < 1 ){ 
         Scierror (
           "SIDENT REQUIRES AT LEAST 1 OUTPUT ARGUMENTS")
      } 

// Check dimensions of input parameters and read/set scalar parameters.

//   meth

      if (MXGETM (PRHS (1)) != 1 || MXGETN (PRHS (1)) != 1){ 
         Scierror ("METH MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (1)) == 0||MXISCOMPLEX (PRHS (1)) == 1)
        { 
         Scierror ("METH MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (1)),TEMP ,1)
      TASK =TEMP 
      if (TASK < 1 ||TASK > 3 ){ 
         Scierror ("
           METH HAS 1, 2, OR 3 THE ONLY ADMISSIBLE VALUES")
      } 

      if (TASK == 1 ){ 
         METH ='M'
      } else { if (TASK == 2 ){ 
         METH ='N'
      } else { 
         METH ='C'
      } 

//   job

      if (MXGETM (PRHS (2)) != 1 || MXGETN (PRHS (2)) != 1){ 
         Scierror ("JOB MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (2)) == 0||MXISCOMPLEX (PRHS (2)) == 1)
        { 
         Scierror ("JOB MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (2)),TEMP ,1)
      IJOB =TEMP 
      if (IJOB < 1 ||IJOB > 4 ){ 
         Scierror ("
           JOB HAS 1, 2, 3 OR 4 THE ONLY ADMISSIBLE VALUES")
      } 

      if (IJOB == 1 ){ 
         JOB ='A'
      } else { if (IJOB == 2 ){ 
         JOB ='C'
      } else { if (IJOB == 3 ){ 
         JOB ='B'
      } else { 
         JOB ='D'
      } 
//         
//   s

      if (MXGETM (PRHS (3)) != 1 || MXGETN (PRHS (3)) != 1){ 
         Scierror ("S MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (3)) == 0||MXISCOMPLEX (PRHS (3)) == 1)
        { 
         Scierror ("S MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (3)),TEMP ,1)
      NOBR =TEMP 
      if (NOBR < 1 ){ 
         Scierror ("S MUST BE A POSITIVE int")
      } 
//         
//   n

      if (MXGETM (PRHS (4)) != 1 || MXGETN (PRHS (4)) != 1){ 
         Scierror ("N MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (4)) == 0||MXISCOMPLEX (PRHS (4)) == 1)
        { 
         Scierror ("N MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (4)),TEMP ,1)
      N =TEMP 
      if (N < 1 ){ 
         Scierror ("N MUST BE A POSITIVE int")
      } 
      if (N >= NOBR ){ 
         WRITE (TEXT ,"('' THE ORDER SHOULD BE AT MOST '', I6)")NOBR -1
         Scierror (TEXT )
      } 
//         
//   l

      if (MXGETM (PRHS (5)) != 1 || MXGETN (PRHS (5)) != 1){ 
         Scierror ("L MUST BE A SCALAR")
      } 
      if (MXISNUMERIC (PRHS (5)) == 0||MXISCOMPLEX (PRHS (5)) == 1)
        { 
         Scierror ("L MUST BE AN int SCALAR")
      } 
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (5)),TEMP ,1)
      L =TEMP 
      if (L < 1 ){ 
         Scierror ("THE SYSTEM HAS NO OUTPUTS")
      } 

//   R(nr,nr)

      NR =MXGETM (PRHS (6))
      NCOL =MXGETN (PRHS (6))
      if (NR < 2 *L ){ 
         WRITE (TEXT ,"(''R MUST HAVE AT LEAST '',I5,'' ROWS'")2*L 
         Scierror (TEXT )
      } 
      if (NCOL < NR ){ 
         WRITE (TEXT ,"(''R MUST HAVE AT LEAST '',I5,'' COLUMNS'")
           NCOL 
         Scierror (TEXT )
      } 
      if (MXISNUMERIC (PRHS (6)) == 0||MXISCOMPLEX (PRHS (6)) == 1)
        { 
         Scierror ("R MUST BE A REAL MATRIX")
      } 

//   m

      M =NR /(2*NOBR )-L 

//   tol
//   
      TOL =ZERO 
      if (NRHS > 6 ){ 
         if (MXGETM (PRHS (7)) != 1 || MXGETN (PRHS (7)) != 1){ 
            Scierror ("TOL MUST BE A SCALAR")
         } 
         if (MXISNUMERIC (PRHS (7)) == 0||MXISCOMPLEX (PRHS (7)) == 1)
           { 
              Scierror ("TOL MUST BE A REAL SCALAR")
         } 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (7)),TOL ,1)
      } 

//   t
//   
      NSMPL =0
      JOBCK ='N'
      if (NRHS > 7 ){ 
         JOBCK ='K'
         if (MXGETM (PRHS (8)) != 1 || MXGETN (PRHS (8)) != 1){ 
            Scierror ("T MUST BE A SCALAR")
         } 
         if (MXISNUMERIC (PRHS (8)) == 0||MXISCOMPLEX (PRHS (8)) == 1)
           { 
              Scierror ("T MUST BE A REAL SCALAR")
         } 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (8)),TEMP ,1)
         NSMPL =TEMP 

         if (NSMPL != 0 && NSMPL < NR ){ 
            WRITE (TEXT ,'
              ('' THE NUMBER OF SAMPLES SHOULD BE AT LEAST '',I10)")NR 
            Scierror (TEXT )
         } else { if (NSMPL == 0 ){ 
            JOBCK ="N'
         } 
      } 

//   A(n,n)

      if (TASK >= 2 && IJOB >= 3 ){ 
         MA =MXGETM (PRHS (9))
         NA =MXGETN (PRHS (9))
         if (MA != N ||NA != N ){ 
            WRITE (TEXT ,"(''A MUST HAVE '',I5,'' ROWS AND COLUMNS'")
              N 
            Scierror (TEXT )
         } 
         if (MXISNUMERIC (PRHS (9)) == 0||MXISCOMPLEX (PRHS (9)) == 1)
           { 
            Scierror ("A MUST BE A REAL MATRIX")
         } 

//   C(l,n)

         MA =MXGETM (PRHS (10))
         NA =MXGETN (PRHS (10))
         if (MA != L ){ 
            WRITE (TEXT ,"(''C MUST HAVE '',I5,'' ROWS'")L 
            Scierror (TEXT )
         } 
         if (NA != N ){ 
            WRITE (TEXT ,"(''C MUST HAVE '',I5,'' COLUMNS'")N 
            Scierror (TEXT )
         } 
         if (MXISNUMERIC (PRHS (10)) == 0||MXISCOMPLEX (PRHS (10))
            == 1){ 
            Scierror ("C MUST BE A REAL MATRIX")
         } 
      } 

//     printw
//   
      PRINTW =.FALSE.
      IP =11
      if (NRHS >= IP ){ 
         if (MXGETM (PRHS (IP )) != 1||MXGETN (PRHS (IP )) != 1){ 
            Scierror ("PRINTW MUST BE A SCALAR")
         } 
         if (MXISNUMERIC (PRHS (IP )) == 0||MXISCOMPLEX (PRHS (IP ))
            == 1){ 
            Scierror ("PRINTW MUST BE AN int SCALAR")
         } 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (IP )),TEMP ,1)
         ITMP =TEMP 
         if (ITMP < 0 ||ITMP > 1 ){ 
            Scierror (
              "PRINTW HAS 0 OR 1 THE ONLY ADMISSIBLE VALUES")
         } 
         PRINTW =ITMP == 1 
      } 

// Determine the lenghts of working arrays.
// The value for LDWORK is the minimum value needed by IB01BD for each
// method and algorithm implemented.  Using a larger value could
// increase the efficiency.

      MNOBR =M *NOBR 
      LNOBR =L *NOBR 
      MNOBRN =MNOBR +N 
      LDUNN =(LNOBR -L )*N 
      NPL =N +L 
      N2 =N +N 
      NN =N *N 
      NL =N *L 
      LL =L *L 

      LDA =MAX (1,N )
      LDB =LDA 
      LDC =MAX (1,L )
      LDD =LDC 
      LDO =LNOBR 
      LDR =NR 
      if (NSMPL != 0 ){ 
         LDK =LDA 
         LDQ =LDA 
         LDS =LDA 
         LDRY =LDC 
         LBWORK =N2 
      } else { 
         LDK =1
         LDQ =1
         LDS =1
         LDRY =1
         LBWORK =1
      } 

      LIWORK =MNOBR +N 
      if (TASK == 1 ){ 
         LIWORK =MAX (LIWORK ,LNOBR )
      } else { if (TASK == 2 ){ 
         LIWORK =MAX (LIWORK ,M *NPL )
      } else { 
         LIWORK =MAX (LIWORK ,LNOBR ,M *NPL )
      } 
      if (NSMPL > 0 )LIWORK =MAX (LIWORK ,NN )

      IAW =0
      LDWORK =LDUNN +4*N 
      if (TASK == 1 ){ 
         ID =0
      } else { 
         ID =N 
      } 

      if (TASK != 2 ){ 
         if (IJOB.LE.2 ){ 
            LDWORK =MAX (LDWORK ,2*LDUNN +N2 ,LDUNN +NN +7*N )
         } 
      } 

      if ((M > 0 && IJOB != 2 )||TASK >= 2 ){ 
         LDWORK =MAX (LDWORK ,2*LDUNN +NN +ID +7*N )
         if (TASK == 1 )LDWORK =MAX (LDWORK ,LDUNN +N +6*MNOBR ,LDUNN +
           N +MAX (L +MNOBR ,LNOBR +MAX (3*LNOBR ,M )))
      } else { 
         if (TASK != 2 ){IAW =N +NN 
      } 

      if (TASK != 1 ||NSMPL > 0 ){ 
         LDWORK =MAX (LDWORK ,LDUNN +IAW +N2 +MAX (5*N ,LNOBR +2*MNOBR +
           L ),ID +4*MNOBRN ,ID +MNOBRN +NPL )
         if (TASK != 1 && M > 0 && IJOB != 2 )LDWORK =MAX (LDWORK ,
           MNOBR *NPL *(M *NPL +1)+MAX (NPL **2,4*M *NPL +1))
         LDWORK =LNOBR *N +LDWORK 
      } 

      if (NSMPL > 0 )LDWORK =MAX (LDWORK ,4*NN +2*NL +LL +MAX (3*L ,
        NL ),14*NN +12*N +5)

// Allocate variable dimension local arrays.
// !Fortran 90/95

      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDA,N,A)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDB,M,B)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDC,N,C)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDD,M,D)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDWORK,1,DWORK)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'i',LIWORK,1,IWORK)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDQ,N,Q)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDR,NCOL,R)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDRY,L,RY)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDS,L,S)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'b',LBWORK,1,BWORK)) RETURN
      IF(.NOT.CREATEVAR(NBVARS+1,'d',LDK,L,K)) RETURN

// Copy inputs from MATLAB workspace to locally allocated arrays.
//      
      MXCOPYPTRTOREAL8 (MXGETPR (PRHS (6)),stk(R),LDR *NCOL )
      if (TASK >= 2 && IJOB >= 3 ){ 
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (9)),stk(A),LDA *N )
         MXCOPYPTRTOREAL8 (MXGETPR (PRHS (10)),stk(C),LDC *N )
      } 

// Do the actual computations.

      IB01BD (METH ,JOB ,JOBCK ,NOBR ,N ,M ,L ,NSMPL ,stk(R),LDR ,
        stk(A),LDA ,stk(C),LDC ,stk(B),LDB ,stk(D),LDD ,stk(Q),LDQ ,
        stk(RY),LDRY ,stk(S),LDS ,stk(K),LDK ,TOL ,istk(IWORK),stk(
        DWORK),LDWORK ,istk(BWORK),IWARN ,INFO )
      if (IWARN != 0 && PRINTW ){ 
         WRITE (TEXT ,"(''  IWARN = '',I4,'' ON EXIT FROM IB01BD'")
           IWARN 
      } 
      if (INFO != 0 ){ 
         WRITE (TEXT ,"(''INFO = '',I4,'' ON EXIT FROM IB01BD'" ) INFO 
      } else { 

// Copy output to MATLAB workspace.

         if (IJOB.LE.2 ){ 
            PLHS (1)=MXCREATEFULL (N ,N ,0)
            MXCOPYREAL8TOPTR (stk(A),MXGETPR (PLHS (1)),NN )
            IP =1
            if (NLHS > 1 ){ 
               PLHS (2)=MXCREATEFULL (L ,N ,0)
               MXCOPYREAL8TOPTR (stk(C),MXGETPR (PLHS (2)),NL )
               IP =2
            } 
         } else { 
            IP =0
         } 
//         
         if (NLHS > IP ){ 
            if (IJOB == 1 ||IJOB >= 3 ){ 
               IP =IP +1
               PLHS (IP )=MXCREATEFULL (N ,M ,0)
               MXCOPYREAL8TOPTR (stk(B),MXGETPR (PLHS (IP )),N *M )
            } 
            if (NLHS > IP ){ 
               if (IJOB == 1 ||IJOB == 4 ){ 
                  IP =IP +1
                  PLHS (IP )=MXCREATEFULL (L ,M ,0)
                  MXCOPYREAL8TOPTR (stk(D),MXGETPR (PLHS (IP )),
                    L *M )
               } 
            } 
         } 

         if (NSMPL > 0 && NLHS > IP ){ 
            IP =IP +1
            PLHS (IP )=MXCREATEFULL (N ,L ,0)
            MXCOPYREAL8TOPTR (stk(K),MXGETPR (PLHS (IP )),NL )
         } 

         if (NSMPL > 0 && NLHS > IP ){ 
            IP =IP +1
            PLHS (IP )=MXCREATEFULL (N ,N ,0)
            MXCOPYREAL8TOPTR (stk(Q),MXGETPR (PLHS (IP )),NN )
            IP =IP +1
            PLHS (IP )=MXCREATEFULL (L ,L ,0)
            MXCOPYREAL8TOPTR (stk(RY),MXGETPR (PLHS (IP )),LL )
            IP =IP +1
            PLHS (IP )=MXCREATEFULL (N ,L ,0)
            MXCOPYREAL8TOPTR (stk(S),MXGETPR (PLHS (IP )),NL )
         } 

         if (NLHS > IP ){ 
            IP =IP +1
            if (NSMPL == 0 ){ 
               NRC =4
            } else { 
               NRC =12
            } 
            PLHS (IP )=MXCREATEFULL (NRC ,1,0)
            MXCOPYREAL8TOPTR (stk(DWORK+2-1),MXGETPR (PLHS (IP )),
              NRC )
         } 
      } 

// Deallocate local arrays.
// !Fortran 90/95

//     DEALLOCATE( A, B, C, D, R, Q, RY, S, IWORK, DWORK )
//     DEALLOCATE( K, BWORK )

// Error and warning handling.

      if (IWARN != 0 && PRINTW ){ 
         MEXPRINTF (TEXT )
      } 

      if (INFO != 0 ){ 
         Scierror (TEXT )
      } 

}
#endif
