/* fstair.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*    PURPOSE: 
 * 
 *    Given a pencil sE-A where matrix E is in column echelon form the 
 *    subroutine FSTAIR computes according to the wishes of the user a 
 *    unitary transformed pencil Q(sE-A)Z which is more or less similar 
 *    to the generalized Schur form of the pencil sE-A. 
 *    The subroutine yields also part of the Kronecker structure of 
 *    the given pencil. 
 * 
 *    CONTRIBUTOR: Th.G.J. Beelen (Philips Glass Eindhoven). 
 *    Copyright SLICOT 
 * 
 *    REVISIONS: 1988, February 02. 
 * 
 ************************************************************************ 
 *     Philips Glass Eindhoven 
 *     5600 MD Eindhoven, Netherlands 
 ************************************************************************ 
 *         FSTAIR - SLICOT Library Routine Document 
 * 
 *1 PURPOSE: 
 * 
 *  Given a pencil sE-A where matrix E is in column echelon form the 
 *  subroutine FSTAIR computes according to the wishes of the user a 
 *  unitary transformed pencil Q(sE-A)Z which is more or less similar 
 *  to the generalized Schur form of the pencil sE-A. The computed form 
 *  yields part of the Kronecker structure of the given pencil. 
 * 
 *2 SPECIFICATION: 
 * 
 *  SUBROUTINE FSTAIR(A, LDA, E, Q, LDQ, Z, LDZ, M, N, ISTAIR, RANKE, 
 *                    NBLCKS, IMUK, INUK, IMUK0, INUK0, MNEI, 
 *                    WRK, IWRK, TOL, MODE, IERR) 
 *  INT LDA, LDQ, LDZ, M, N, RANKE, NBLCKS, MODE, IERR 
 *  INT ISTAIR(M), IMUK(N), INUK(M+1), IMUK0(N), INUK0(M+1), 
 *  INT MNEI(4), IWRK(N) 
 *  DOUBLE PRECISION TOL 
 *  DOUBLE PRECISION WRK(N) 
 *  DOUBLE PRECISION A(LDA,N), E(LDA,N), Q(LDQ,M), Z(LDZ,N) 
 * 
 *3 ARGUMENT LIST: 
 * 
 *  3.1 ARGUMENTS IN 
 * 
 *      A      - DOUBLE PRECISION array of DIMENSION (LDA,N). 
 *               The leading M x N part of this array contains the M x N 
 *               matrix A that has to be row compressed. 
 *               NOTE: this array is overwritten. 
 * 
 *      LDA    - INT 
 *               LDA is the leading dimension of the arrays A and E. 
 *               (LDA >= M) 
 * 
 *      E      - DOUBLE PRECISION array of DIMENSION (LDA,N). 
 *               The leading M x N part of this array contains the M x N 
 *               matrix E which will be transformed equivalent to matrix 
 *               A. 
 *               On entry, matrix E has to be in column echelon form. 
 *               This may be accomplished by subroutine EREDUC. 
 *               NOTE: this array is overwritten. 
 * 
 *      Q      - DOUBLE PRECISION array of DIMENSION (LDQ,M). 
 *               The leading M x M part of this array contains an M x M 
 *               unitary row transformation matrix corresponding to the 
 *               row transformations of the matrices A and E which are 
 *               needed to transform an arbitrary pencil to a pencil 
 *               where E is in column echelon form. 
 *               NOTE: this array is overwritten. 
 * 
 *      LDQ    - INT 
 *               LDQ is the leading dimension of the array Q. 
 *               (LDQ >= M) 
 * 
 *      Z      - DOUBLE PRECISION array of DIMENSION (LDZ,N). 
 *               The leading N x N part of this array contains an N x N 
 *               unitary column transformation matrix corresponding to 
 *               the column transformations of the matrices A and E 
 *               which are needed to transform an arbitrary pencil to 
 *               a pencil where E is in column echelon form. 
 *               NOTE: this array is overwritten. 
 * 
 *      LDZ    - INT 
 *               LDZ is the leading dimension of the array Z. 
 *               (LDZ >= N) 
 * 
 *      M      - INT 
 *     M is the number of rows of the matrices A, E and Q. 
 * 
 *      N      - INT 
 *     N is the number of columns of the matrices A, E and Z. 
 * 
 *      ISTAIR - INT array of DIMENSION (M). 
 *     ISTAIR contains the information on the column echelon 
 *     form of the input matrix E. This may be accomplished 
 *     by subroutine EREDUC. 
 *     ISTAIR(i) = + j   if the boundary element E(i,j) is a 
 *   corner point. 
 *       - j   if the boundary element E(i,j) is not 
 *   a corner point. 
 *     (i=1,...,M) 
 *     NOTE: this array is destroyed. 
 * 
 *      RANKE  - INT 
 *     RANKE is the rank of the input matrix E being in column 
 *     echelon form. 
 * 
 *  3.2 ARGUMENTS OUT 
 * 
 *      A      - DOUBLE PRECISION array of DIMENSION (LDA,N). 
 *     The leading M x N part of this array contains the M x N 
 *     matrix A that has been row compressed while keeping E 
 *     in column echelon form. 
 * 
 *      E      - DOUBLE PRECISION array of DIMENSION (LDA,N). 
 *     The leading M x N part of this array contains the M x N 
 *     matrix E that has been transformed equivalent to matrix 
 *     A. 
 * 
 *      Q      - DOUBLE PRECISION array of DIMENSION (LDQ,M). 
 *     The leading M x M part of this array contains the M x M 
 *     unitary matrix Q which is the product of the input 
 *     matrix Q and the row transformation matrix which has 
 *     transformed the rows of the matrices A and E. 
 * 
 *      Z      - DOUBLE PRECISION array of DIMENSION (LDZ,N). 
 *     The leading N x N part of this array contains the N x N 
 *     unitary matrix Z which is the product of the input 
 *     matrix Z and the column transformation matrix which has 
 *     transformed the columns of the matrices A and E. 
 * 
 *      NBLCKS - INT 
 *     NBLCKS is the number of submatrices having 
 *     full row rank >= 0  detected in matrix A. 
 * 
 *      IMUK   - INT array of DIMENSION (N). 
 *     Array IMUK contains the column dimensions mu(k) 
 *     (k=1,...,NBLCKS) of the submatrices having full column 
 *     rank in the pencil sE(x)-A(x) 
 *     where  x = eps,inf  if MODE = 1 or 2 
 *      eps         MODE = 3 . 
 * 
 *      INUK   - INT array of DIMENSION (M+1). 
 *     Array INUK contains the row dimensions nu(k) 
 *     (k=1,...,NBLCKS) of the submatrices having full row 
 *     rank in the pencil sE(x)-A(x) 
 *     where  x = eps,inf  if MODE = 1 or 2 
 *      eps         MODE = 3 . 
 * 
 *      IMUK0  - INT array of DIMENSION (N). 
 *     Array IMUK0 contains the column dimensions mu(k) 
 *     (k=1,...,NBLCKS) of the submatrices having full column 
 *     rank in the pencil sE(eps,inf)-A(eps,inf). 
 * 
 *      INUK0  - INT array of DIMENSION (M+1). 
 *     Array INUK0 contains the row dimensions nu(k) 
 *     (k=1,...,NBLCKS) of the submatrices having full row 
 *     rank in the pencil sE(eps,inf)-A(eps,inf). 
 * 
 *      MNEI   - INT array of DIMENSION (4). 
 *     If MODE = 3 then 
 *     MNEI(1) = row    dimension of sE(eps)-A(eps) 
 *2  = column dimension of sE(eps)-A(eps) 
 *3  = row    dimension of sE(inf)-A(inf) 
 *4  = column dimension of sE(inf)-A(inf) 
 *     If MODE = 1 or 2 then the array MNEI is empty. 
 * 
 *  3.3 WORK SPACE 
 * 
 *      WRK    - DOUBLE PRECISION array of DIMENSION (N). 
 * 
 *      IWRK   - INT array of DIMENSION (N). 
 * 
 *  3.4 TOLERANCES 
 * 
 *      TOL    - DOUBLE PRECISION 
 *     TOL is the tolerance used when considering matrix 
 *     elements to be zero. TOL should be set to 
 *     TOL = RE * Max( ||A|| , ||E|| ) + AE , where 
 *     ||.|| is the Frobenius norm. AE and RE are the absolute 
 *     and relative accuracy. 
 *     A recommanded choice is AE = EPS and RE = 100*EPS, 
 *     where EPS is the machine precision. 
 * 
 *  3.5 MODE PARAMETERS 
 * 
 *      MODE   - INT 
 *     According to the value of MODE the subroutine FSTAIR 
 *     computes a generalized Schur form of the pencil sE-A, 
 *     where the structure of the generalized Schur form is 
 *     specified more the higher the value of MODE is. 
 *     (See also 6 DESCRIPTION). 
 * 
 *  3.6 ERROR INDICATORS 
 * 
 *      IERR   - INT 
 *     On return, IERR contains 0 unless the subroutine 
 *     has failed. 
 * 
 *4 ERROR INDICATORS and WARNINGS: 
 * 
 *  IERR = -1: the value of MODE is not 1, 2 or 3. 
 *  IERR =  0: succesfull completion. 
 *  IERR =  1: the algorithm has failed. 
 * 
 *5 AUXILARY ROUTINES and COMMON BLOCKS: 
 * 
 *  BAE, SQUAEK, TRIRED from SLICOT. 
 * 
 *6 DESCRIPTION: 
 * 
 *  On entry, matrix E is assumed to be in column echelon form. 
 *  Depending on the value of the parameter MODE, submatrices of A 
 *  and E will be reduced to a specific form. The higher the value of 
 *  MODE, the more the submatrices are transformed. 
 * 
 *  Step 1 of the algorithm. 
 *  If MODE = 1 then subroutine FSTAIR transforms the matrices A and 
 *  E to the following generalized Schur form by unitary transformations 
 *  Q1 and Z1, using subroutine BAE. (See also Algorithm 3.2.1 in [1]). 
 * 
 *                   | sE(eps,inf)-A(eps,inf) |      X     | 
 *      Q1(sE-A)Z1 = |------------------------|------------| 
 *                   |            O           | sE(r)-A(r) | 
 * 
 *  Here the pencil sE(eps,inf)-A(eps,inf) is in staircase form. 
 *  This pencil contains all Kronecker column indices and infinite 
 *  elementary divisors of the pencil sE-A. 
 *  The pencil sE(r)-A(r) contains all Kronecker row indices and 
 *  elementary divisors of sE-A. 
 *  NOTE: X is a pencil. 
 * 
 *  Step 2 of the algorithm. 
 *  If MODE = 2 then furthermore the submatrices having full row and 
 *  column rank in the pencil sE(eps,inf)-A(eps,inf) are triangularized 
 *  by applying unitary transformations Q2 and Z2 to Q1*(sE-A)*Z1. This 
 *  is done by subroutine TRIRED. (see also Algorithm 3.3.1 [1]). 
 * 
 *  Step 3 of the algorithm. 
 *  If MODE = 3 then moreover the pencils sE(eps)-A(eps) and 
 *  sE(inf)-A(inf) are separated in sE(eps,inf)-A(eps,inf) by applying 
 *  unitary transformations Q3 and Z3 to Q2*Q1*(sE-A)*Z1*Z2. This is 
 *  done by subroutine SQUAEK. (See also Algorithm 3.3.3 in [1]). 
 *  We then obtain 
 * 
 *             | sE(eps)-A(eps) |        X       |      X     | 
 *             |----------------|----------------|------------| 
 *             |        O       | sE(inf)-A(inf) |      X     | 
 *  Q(sE-A)Z = |=================================|============|  (1) 
 *             |             |            | 
 *             |                O                | sE(r)-A(r) | 
 * 
 *  where Q = Q3*Q2*Q1 and Z = Z1*Z2*Z3. 
 *  The accumulated row and column transformations are multiplied on 
 *  the left and right respectively with the contents of the arrays Q 
 *  and Z on entry and the results are stored in Q and Z, respectively. 
 *  NOTE: the pencil sE(r)-A(r) is not reduced furthermore. 
 * 
 *  Now let sE-A be an arbitrary pencil. This pencil has to be 
 *  transformed into a pencil with E in column echelon form before 
 *  calling FSTAIR. This may be accomplished by the subroutine EREDUC. 
 *  Let the therefore needed unitary row and column transformations be 
 *  Q0 and Z0, respectively. 
 *  Let, on entry, the arrays Q and Z contain Q0 and Z0, and let ISTAIR 
 *  contain the appropiate information. Then, on return with MODE = 3, 
 *  the contents of the arrays Q and Z are Q3*Q2*Q1*Q0 and Z0*Z1*Z2*Z3 
 *  which are the transformation matrices that transform the arbitrary 
 *  pencil sE-A into the form (1). 
 * 
 *7 REFERENCES: 
 * 
 *  [1] Th.G.J. Beelen, New Algorithms for Computing the Kronecker 
 *      structure of a Pencil with Applications to Systems and Control 
 *      Theory, Ph.D.Thesis, Eindhoven University of Technology, 
 *      The Netherlands, 1987. 
 * 
 *8 NUMERICAL ASPECTS: 
 * 
 *  It is shown in [1] that the algorithm is numerically backward 
 *  stable. The operations count is proportional to (Max(M,N))**3 . 
 * 
 *9 FURTHER REMARKS: 
 * 
 *  - The difference mu(k)-nu(k) = # Kronecker blocks of size kx(k+1). 
 *    The number of these blocks is given by NBLCKS. 
 *  - The difference nu(k)-mu(k+1) = # infinite elementary divisors of 
 *    degree k  (here mu(NBLCKS+1) := 0). 
 *  - MNEI(3) = MNEI(4) since pencil sE(inf)-A(inf) is regular. 
 *  - In the pencil sE(r)-A(r) the pencils sE(f)-A(f) and sE(eta)-A(eta) 
 *    can be separated by pertransposing the pencil sE(r)-A(r) and 
 *    using the last part of subroutine FSTAIR. The result has got to be 
 *    pertransposed again. (For more details see section 3.3.1 in [1]). 
 * 
 */

static int droti(int *n, double *x, int *incx, double *y, int *incy, double *c__, double *s);
static int c__1 = 1;

int
nsp_ctrlpack_fstair (double *a, double *e, double *q, double *z__, int *m,
		     int *n, int *istair, int *ranke, double *tol,
		     int *nblcks, int *imuk, int *inuk, int *imuk0,
		     int *inuk0, int *mnei, double *wrk, int *iwrk, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, e_dim1, e_offset, q_dim1, q_offset, z_dim1, z_offset,
    i__1;

  /* Local variables */
  int mode, i__, ifica, k, ifira, ranka, ismuk, isnuk, jk;
  int lda, nca, /* lde,*/ /* mei, nei,*/ ldq, /* nra, */ ldz;

  /* Parameter adjustments */
  --inuk0;
  --inuk;
  --istair;
  q_dim1 = *m;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  --iwrk;
  --wrk;
  --imuk0;
  --imuk;
  z_dim1 = *n;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  e_dim1 = *m;
  e_offset = e_dim1 + 1;
  e -= e_offset;
  a_dim1 = *m;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --mnei;

  /* Function Body */
  lda = *m;
  /* lde = *m; */
  ldq = *m;
  ldz = *n;
  mode = 3;
  *ierr = 0;
  /* 
   *    A(k) is the submatrix in A that will be row compressed. 
   * 
   *    ISMUK= sum(i=1,..,k) MU(i), ISNUK= sum(i=1,...,k) NU(i), 
   *    IFIRA, IFICA: first row and first column index of A(k) in A. 
   *    NRA, NCA: number of rows and columns in A(k). 
   * 
   */
  ifira = 1;
  ifica = 1;
  /* nra = *m; */
  nca = *n - *ranke;
  isnuk = 0;
  ismuk = 0;
  /* 
   *    NBLCKS = # blocks detected in A with full row rank >= 0. 
   * 
   */
  *nblcks = 0;
  k = 0;
  /* 
   *    Initialization of the arrays INUK and IMUK. 
   * 
   */
  i__1 = *m + 1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      inuk[i__] = -1;
      /* L10: */
    }
  /* 
   *    Note: it is necessary that array INUK has dimension M+1 since it 
   *          is possible that M = 1 and NBLCKS = 2. 
   *          Example sE-A = (0 0 s -1). 
   * 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      imuk[i__] = -1;
      /* L20: */
    }
  /* 
   *    Compress the rows of A while keeping E in column echelon form. 
   * 
   *    REPEAT 
   * 
   */
 L30:
  ++k;
  nsp_ctrlpack_bae (&a[a_offset], &lda, &e[e_offset], &q[q_offset], &ldq,
		    &z__[z_offset], &ldz, m, n, &istair[1], &ifira, &ifica,
		    &nca, &ranka, &wrk[1], &iwrk[1], tol);
  imuk[k] = nca;
  ismuk += nca;
  inuk[k] = ranka;
  isnuk += ranka;
  ++(*nblcks);
  /* 
   *       If rank of A(k) = nrb then A has full row rank ; 
   *       JK = first column index (in A) after right most column of 
   *       matrix A(k+1). 
   *       (in case A(k+1) is empty, then JK = N+1). 
   * 
   */
  ifira = isnuk + 1;
  ifica = ismuk + 1;
  if (ifira > *m)
    {
      jk = *n + 1;
    }
  else
    {
      jk = (i__1 = istair[ifira], Abs (i__1));
    }
  /* nra = *m - isnuk; */
  nca = jk - 1 - ismuk;
  /* 
   *       If NCA > 0 then there can be done some more row compression 
   *       of matrix A while keeping matrix E in column echelon form. 
   * 
   */
  if (nca > 0)
    {
      goto L30;
    }
  /*    UNTIL NCA <= 0 
   * 
   *    Matrix E(k+1) has full column rank since NCA = 0. 
   *    Reduce A and E by ignoring all rows and columns corresponding 
   *    to E(k+1). 
   *    Ignoring these columns in E changes the ranks of the 
   *    submatrices E(i), (i=1,...,k-1). 
   * 
   *    MEI and NEI are the dimensions of the pencil 
   *    sE(eps,inf)-A(eps,inf), i.e., the pencil that contains only 
   *    Kronecker column indices and infinity elementary divisors. 
   * 
   */
  /* mei = isnuk;
     nei = ismuk;
  */
  /* 
   *    Save dimensions of the submatrices having full row or column rank 
   *    in pencil sE(eps,inf)-A(eps,inf), i.e., copy the arrays 
   *    IMUK and INUK to IMUK0 and INUK0, respectively. 
   * 
   */
  i__1 = *m + 1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      inuk0[i__] = inuk[i__];
      /* L40: */
    }
  /* 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      imuk0[i__] = imuk[i__];
      /* L50: */
    }
  /* 
   */
  if (mode == 1)
    {
      return 0;
    }
  /* 
   *    Triangularization of the submatrices in A and E. 
   * 
   */
  nsp_ctrlpack_trired (&a[a_offset], &lda, &e[e_offset], &q[q_offset], &ldq,
		       &z__[z_offset], &ldz, m, n, nblcks, &inuk[1], &imuk[1],
		       ierr);
  /* 
   */
  if (*ierr != 0)
    {
      /*     write(6,*) 'error: fstair failed!' 
       */
      return 0;
    }
  /* 
   */
  if (mode == 2)
    {
      return 0;
    }
  /* 
   *    Reduction to square submatrices E(k)'s in E. 
   * 
   */
  nsp_ctrlpack_squaek (&a[a_offset], &lda, &e[e_offset], &q[q_offset], &ldq,
		       &z__[z_offset], &ldz, m, n, nblcks, &inuk[1], &imuk[1],
		       &mnei[1]);
  /* 
   */
  return 0;
  /**** Last line of FSTAIR ********************************************* 
   */
}				/* fstair_ */

int
nsp_ctrlpack_squaek (double *a, int *lda, double *e, double *q, int *ldq,
		     double *z__, int *ldz, int *m, int *n, int *nblcks,
		     int *inuk, int *imuk, int *mnei)
{
  /* System generated locals */
  int a_dim1, a_offset, e_dim1, e_offset, q_dim1, q_offset, z_dim1, z_offset,
    i__1, i__2;

  /* Local variables */
  int minf, ninf, nelm;
  int sk1p1, tk1p1, meps, neps, mukp1, j, k;
  int ismuk, isnuk, ca, ce, ra;
  double sc;
  int re, ip, tp;
  double ss;
  int tp1, cja, cje, rje, muk, nuk, mup, nup, mup1;

  /* 
   *    PURPOSE: 
   * 
   *    On entry, it is assumed that the M by N matrices A and E have 
   *        Control Theory, Ph.D.Thesis, Eindhoven University of 
   *        Technology, The Netherlands, 1987. 
   * 
   *    CONTRIBUTOR: Th.G.J. Beelen (Philips Glas Eindhoven) 
   * 
   *    REVISIONS: 1988, February 02. 
   * 
   *    Specification of the parameters. 
   * 
   *    .. Scalar arguments .. 
   * 
   * 
   *    .. Array arguments .. 
   * 
   * 
   *    EXTERNAL SUBROUTINES: 
   * 
   *      DGIV, DROTI from SLICOT. 
   * 
   *    Local variables. 
   * 
   * 
   *    Initialisation. 
   * 
   */
  /* Parameter adjustments */
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  z_dim1 = *ldz;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  e_dim1 = *lda;
  e_offset = e_dim1 + 1;
  e -= e_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --imuk;
  --inuk;
  --mnei;

  /* Function Body */
  ismuk = 0;
  isnuk = 0;
  i__1 = *nblcks;
  for (k = 1; k <= i__1; ++k)
    {
      ismuk += imuk[k];
      isnuk += inuk[k];
      /* L10: */
    }
  /* 
   *    MEPS, NEPS are the dimensions of the pencil s*E(eps)-A(eps). 
   *    MEPS = Sum(k=1,...,nblcks) NU(k), 
   *    NEPS = Sum(k=1,...,nblcks) MU(k). 
   *    MINF, NINF are the dimensions of the pencil s*E(inf)-A(inf). 
   * 
   */
  meps = isnuk;
  neps = ismuk;
  minf = 0;
  ninf = 0;
  /* 
   *    MUKP1 = mu(k+1).  N.B. It is assumed that mu(NBLCKS + 1) = 0. 
   * 
   */
  mukp1 = 0;
  /* 
   */
  for (k = *nblcks; k >= 1; --k)
    {
      nuk = inuk[k];
      muk = imuk[k];
      /* 
       *       Reduce submatrix E(k,k+1) to square matrix. 
       *       NOTE that always NU(k) >= MU(k+1) >= 0. 
       * 
       *       WHILE ( NU(k) >  MU(k+1) ) DO 
       */
    L20:
      if (nuk > mukp1)
	{
	  /* 
	   *          sk1p1 = sum(i=k+1,...,p-1) NU(i) 
	   *          tk1p1 = sum(i=k+1,...,p-1) MU(i) 
	   *          ismuk = sum(i=1,...,k) MU(i) 
	   *          tp1 = sum(i=1,...,p-1) MU(i) = ismuk + tk1p1. 
	   * 
	   */
	  sk1p1 = 0;
	  tk1p1 = 0;
	  i__1 = *nblcks;
	  for (ip = k + 1; ip <= i__1; ++ip)
	    {
	      /* 
	       *             Annihilate the elements originally present in the last 
	       *             row of E(k,p+1) and A(k,p). 
	       *             Start annihilating the first MU(p) - MU(p+1) elements by 
	       *             applying column Givens rotations plus interchanging 
	       *             elements. 
	       *             Use original bottom diagonal element of A(k,k) as pivot. 
	       *             Start position pivot in A = (ra,ca). 
	       * 
	       */
	      tp1 = ismuk + tk1p1;
	      ra = isnuk + sk1p1;
	      ca = tp1;
	      /* 
	       */
	      mup = imuk[ip];
	      mup1 = inuk[ip];
	      nup = mup1;
	      /* 
	       */
	      i__2 = mup - nup;
	      for (j = 1; j <= i__2; ++j)
		{
		  /* 
		   *                CJA = current column index of pivot in A. 
		   * 
		   */
		  cja = ca + j - 1;
		  nsp_ctrlpack_dgiv (&a[ra + cja * a_dim1],
				     &a[ra + (cja + 1) * a_dim1], &sc, &ss);
		  /* 
		   *                Apply transformations to A- and E-matrix. 
		   *                Interchange columns simultaneously. 
		   *                Update column transformation matrix Z. 
		   * 
		   */
		  nelm = ra;
		  droti (&nelm, &a[cja * a_dim1 + 1], &c__1,
			 &a[(cja + 1) * a_dim1 + 1], &c__1, &sc, &ss);
		  a[ra + cja * a_dim1] = 0.;
		  droti (&nelm, &e[cja * e_dim1 + 1], &c__1,
			 &e[(cja + 1) * e_dim1 + 1], &c__1, &sc, &ss);
		  droti (n, &z__[cja * z_dim1 + 1], &c__1,
			 &z__[(cja + 1) * z_dim1 + 1], &c__1, &sc, &ss);
		  /* L30: */
		}
	      /* 
	       *             Annihilate the remaining elements originally present in 
	       *             the last row of E(k,p+1) and A(k,p) by alternatingly 
	       *             applying row and column rotations plus interchanging 
	       *             elements. 
	       *             Use diagonal elements of E(p,p+1) and original bottom 
	       *             diagonal element of A(k,k) as pivots, respectively. 
	       *             (re,ce) and (ra,ca) are the starting positions of the 
	       *             pivots in E and A. 
	       * 
	       */
	      re = ra + 1;
	      tp = tp1 + mup;
	      ce = tp + 1;
	      ca = tp - mup1;
	      /* 
	       */
	      i__2 = mup1;
	      for (j = 1; j <= i__2; ++j)
		{
		  /* 
		   *                (RJE,CJE) = current position pivot in E. 
		   * 
		   */
		  rje = re + j - 1;
		  cje = ce + j - 1;
		  cja = ca + j - 1;
		  /* 
		   *                Determine the row transformations. 
		   *                Apply these transformations to E- and A-matrix . 
		   *                Interchange the rows simultaneously. 
		   *                Update row transformation matrix Q. 
		   * 
		   */
		  nsp_ctrlpack_dgiv (&e[rje + cje * e_dim1],
				     &e[rje - 1 + cje * e_dim1], &sc, &ss);
		  nelm = *n - cje + 1;
		  droti (&nelm, &e[rje + cje * e_dim1], lda,
			 &e[rje - 1 + cje * e_dim1], lda, &sc, &ss);
		  e[rje + cje * e_dim1] = 0.;
		  nelm = *n - cja + 1;
		  droti (&nelm, &a[rje + cja * a_dim1], lda,
			 &a[rje - 1 + cja * a_dim1], lda, &sc, &ss);
		  droti (m, &q[rje + q_dim1], ldq,
			 &q[rje - 1 + q_dim1], ldq, &sc, &ss);
		  /* 
		   *                Determine the column transformations. 
		   *                Apply these transformations to A- and E-matrix. 
		   *                Interchange the columns simultaneously. 
		   *                Update column transformation matrix Z. 
		   * 
		   */
		  nsp_ctrlpack_dgiv (&a[rje + cja * a_dim1],
				     &a[rje + (cja + 1) * a_dim1], &sc, &ss);
		  nelm = rje;
		  droti (&nelm, &a[cja * a_dim1 + 1], &c__1,
			 &a[(cja + 1) * a_dim1 + 1], &c__1, &sc, &ss);
		  a[rje + cja * a_dim1] = 0.;
		  droti (&nelm, &e[cja * e_dim1 + 1], &c__1,
			 &e[(cja + 1) * e_dim1 + 1], &c__1, &sc, &ss);
		  droti (n, &z__[cja * z_dim1 + 1], &c__1,
			 &z__[(cja + 1) * z_dim1 + 1], &c__1, &sc, &ss);
		  /* L40: */
		}
	      /* 
	       */
	      sk1p1 += nup;
	      tk1p1 += mup;
	      /* 
	       */
	      /* L50: */
	    }
	  /* 
	   *          Reduce A=A(eps,inf) and E=E(eps,inf) by ignoring their last 
	   *          row and right most column. The row and column ignored 
	   *          belong to the pencil s*E(inf)-A(inf). 
	   *          Redefine blocks in new A and E. 
	   * 
	   */
	  --muk;
	  --nuk;
	  imuk[k] = muk;
	  inuk[k] = nuk;
	  --ismuk;
	  --isnuk;
	  --meps;
	  --neps;
	  ++minf;
	  ++ninf;
	  /* 
	   */
	  goto L20;
	}
      /*       END WHILE 20 
       * 
       *       Now submatrix E(k,k+1) is square. 
       * 
       *       Consider next submatrix (k:=k-1). 
       * 
       */
      isnuk -= nuk;
      ismuk -= muk;
      mukp1 = muk;
      /* L60: */
    }
  /* 
   *    If mu(NBLCKS) = 0, then the last submatrix counted in NBLCKS is 
   *    a 0 by 0 (empty) matrix. This "matrix" must be removed. 
   * 
   */
  if (imuk[*nblcks] == 0)
    {
      --(*nblcks);
    }
  /* 
   *    Store dimensions of the pencils s*E(eps)-A(eps) and 
   *    s*E(inf)-A(inf) in array MNEI. 
   * 
   */
  mnei[1] = meps;
  mnei[2] = neps;
  mnei[3] = minf;
  mnei[4] = ninf;
  /* 
   */
  return 0;
  /**** Last line of SQUAEK ********************************************* 
   */
}				/* squaek_ */

/** END OF SQUAEKTEXT 
 */
int
nsp_ctrlpack_triaak (double *a, int *lda, double *e, double *z__, int *ldz,
		     int *n, int *nra, int *nca, int *ifira, int *ifica)
{
  /* System generated locals */
  int a_dim1, a_offset, e_dim1, e_offset, z_dim1, z_offset;

  /* Local variables */
  int nelm;
  int i__, j, ifica1, jjpvt, ifira1, ii, jj;
  double sc, ss;
  int mni;

  /* 
   *    PURPOSE: 
   * 
   *    Subroutine TRIAAK reduces a submatrix A(k) of A to upper triangu- 
   *    lar form by column Givens rotations only. 
   *    Here A(k) = A(IFIRA:ma,IFICA:na) where ma = IFIRA - 1 + NRA, 
   *    na = IFICA - 1 + NCA. 
   *    Matrix A(k) is assumed to have full row rank on entry. Hence, no 
   *    pivoting is done during the reduction process. See Algorithm 2.3.1 
   *    and Remark 2.3.4 in [1]. 
   *    The constructed column transformations are also applied to matrix 
   *    E(k) = E(1:IFIRA-1,IFICA:na). 
   *    Note that in E columns are transformed with the same column 
   *    indices as in A, but with row indices different from those in A. 
   *    REMARK: This routine is intended to be called only from the 
   *            SLICOT auxiliary routine TRIRED. 
   * 
   *    PARAMETERS: 
   * 
   *    A - DOUBLE PRECISION array of dimension (LDA,N). 
   *        On entry, it contains the submatrix A(k) of full row rank 
   *        to be reduced to upper triangular form. 
   *        On return, it contains the transformed matrix A(k). 
   *    E - DOUBLE PRECISION array of dimension (LDA,N). 
   *        On entry, it contains the submatrix E(k). 
   *        On return, it contains the transformed matrix E(k). 
   *    Z - DOUBLE PRECISION array of dimension (LDZ,N). 
   *        On entry, Z contains the column transformations corresponding 
   *        to the input matrices A and E. 
   *        On return, it contains the product of the input matrix Z and 
   *        the column transformation matrix that has transformed the 
   *        columns of the matrices A and E. 
   *    N - INT. 
   *        Number of columns of A and E. (N >= 1). 
   *    NRA - INT. 
   *        Number of rows in A(k) to be transformed (1 <= NRA <= LDA). 
   *    NCA - INT. 
   *        Number of columns in A(k) to be transformed (1 <= NCA <= N). 
   *    IFIRA - INT. 
   *        Number of first row in A(k) to be transformed. 
   *    IFICA - INT. 
   *        Number of first column in A(k) to be transformed. 
   * 
   *    REFERENCES: 
   * 
   *    [1] Th.G.J. Beelen, New Algorithms for Computing the Kronecker 
   *        structure of a Pencil with Applications to Systems and 
   *        Control Theory, Ph.D.Thesis, Eindhoven University of 
   *        Technology, The Netherlands, 1987. 
   * 
   *    CONTRIBUTOR: Th.G.J. Beelen (Philips Glas Eindhoven) 
   * 
   *    REVISIONS: 1988, January 29. 
   * 
   *    Specification of the parameters. 
   * 
   *    .. Scalar arguments .. 
   * 
   * 
   *    .. Array arguments .. 
   * 
   * 
   *    EXTERNAL SUBROUTINES: 
   * 
   *      DROT from BLAS 
   *      DGIV from SLICOT. 
   * 
   *    Local variables. 
   * 
   * 
   */
  /* Parameter adjustments */
  z_dim1 = *ldz;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  e_dim1 = *lda;
  e_offset = e_dim1 + 1;
  e -= e_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  ifira1 = *ifira - 1;
  ifica1 = *ifica - 1;
  /* 
   */
  for (i__ = *nra; i__ >= 1; --i__)
    {
      ii = ifira1 + i__;
      mni = *nca - *nra + i__;
      jjpvt = ifica1 + mni;
      nelm = ifira1 + i__;
      for (j = mni - 1; j >= 1; --j)
	{
	  jj = ifica1 + j;
	  /* 
	   *          Determine the Givens transformation on columns jj and jjpvt. 
	   *          Apply the transformation to these columns to annihilate 
	   *          A(ii,jj) (from rows 1 up to ifira1+i). 
	   *          Apply the transformation also to the E-matrix 
	   *          (from rows 1 up to ifira1). 
	   *          Update column transformation matrix Z. 
	   * 
	   */
	  nsp_ctrlpack_dgiv (&a[ii + jjpvt * a_dim1], &a[ii + jj * a_dim1],
			     &sc, &ss);
	  C2F (drot) (&nelm, &a[jjpvt * a_dim1 + 1], &c__1,
		      &a[jj * a_dim1 + 1], &c__1, &sc, &ss);
	  a[ii + jj * a_dim1] = 0.;
	  C2F (drot) (&ifira1, &e[jjpvt * e_dim1 + 1], &c__1,
		      &e[jj * e_dim1 + 1], &c__1, &sc, &ss);
	  C2F (drot) (n, &z__[jjpvt * z_dim1 + 1], &c__1,
		      &z__[jj * z_dim1 + 1], &c__1, &sc, &ss);
	  /* L10: */
	}
      /* L20: */
    }
  /* 
   */
  return 0;
  /**** Last line of TRIAAK ********************************************* 
   */
}				/* triaak_ */

/** END OF TRIAAKTEXT 
 *UPTODATE TRIAEKTEXT 
 */
int
nsp_ctrlpack_triaek (double *a, int *lda, double *e, double *q, int *ldq,
		     int *m, int *n, int *nre, int *nce, int *ifire,
		     int *ifice, int *ifica)
{
  /* System generated locals */
  int a_dim1, a_offset, e_dim1, e_offset, q_dim1, q_offset, i__1, i__2;

  /* Local variables */
  int nelm;
  int i__, j, iipvt, ifice1, ifire1, ii, jj;
  double sc, ss;

  /* 
   *    PURPOSE: 
   * 
   *    Subroutine TRIAEK reduces a submatrix E(k) of E to upper triangu- 
   *    lar form by row Givens rotations only. 
   *    Here E(k) = E(IFIRE:me,IFICE:ne), where me = IFIRE - 1 + NRE, 
   *    ne = IFICE - 1 + NCE. 
   *    Matrix E(k) is assumed to have full column rank on entry. Hence, 
   *    no pivoting is done during the reduction process. See Algorithm 
   *    2.3.1 and Remark 2.3.4 in [1]. 
   *    The constructed row transformations are also applied to matrix 
   *    A(k) = A(IFIRE:me,IFICA:N). 
   *    Note that in A(k) rows are transformed with the same row indices 
   *    as in E but with column indices different from those in E. 
   *    REMARK: This routine is intended to be called only from the 
   *            SLICOT auxiliary routine TRIRED. 
   * 
   *    PARAMETERS: 
   * 
   *    A - DOUBLE PRECISION array of dimension (LDA,N). 
   *        On entry, it contains the submatrix A(k). 
   *        On return, it contains the transformed matrix A(k). 
   *    E - DOUBLE PRECISION array of dimension (LDA,N). 
   *        On entry, it contains the submatrix E(k) of full column 
   *        rank to be reduced to upper triangular form. 
   *        On return, it contains the transformed matrix E(k). 
   *    Q - DOUBLE PRECISION array of dimension (LDQ,M). 
   *        On entry, Q contains the row transformations corresponding 
   *        to the input matrices A and E. 
   *        On return, it contains the product of the input matrix Q and 
   *        the row transformation matrix that has transformed the rows 
   *        of the matrices A and E. 
   *    M - INT. 
   *        Number of rows of A and E. (1 <= M <= LDA). 
   *    N - INT. 
   *        Number of columns of A and E. (N >= 1). 
   *    NRE - INT 
   *        Number of rows in E to be transformed (1 <= NRE <= M). 
   *    NCE - INT. 
   *        Number of columns in E to be transformed (1 <= NCE <= N). 
   *    IFIRE - INT. 
   *        Index of first row in E to be transformed. 
   *    IFICE - INT. 
   *        Index of first column in E to be transformed. 
   *    IFICA - INT. 
   *        Index of first column in A to be transformed. 
   * 
   *    REFERENCES: 
   * 
   *    [1] Th.G.J. Beelen, New Algorithms for Computing the Kronecker 
   *        structure of a Pencil with Applications to Systems and 
   *        Control Theory, Ph.D.Thesis, Eindhoven University of 
   *        Technology, The Netherlands, 1987. 
   * 
   *    CONTRIBUTOR: Th.G.J. Beelen (Philips Glas Eindhoven) 
   * 
   *    REVISIONS: 1988, January 29. 
   * 
   *    Specification of the parameters. 
   * 
   *    .. Scalar arguments .. 
   * 
   * 
   *    .. Array arguments .. 
   * 
   * 
   *    EXTERNAL SUBROUTINES: 
   * 
   *      DROT from BLAS 
   *      DGIV from SLICOT. 
   * 
   *    Local variables. 
   * 
   * 
   */
  /* Parameter adjustments */
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  e_dim1 = *lda;
  e_offset = e_dim1 + 1;
  e -= e_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  ifire1 = *ifire - 1;
  ifice1 = *ifice - 1;
  /* 
   */
  i__1 = *nce;
  for (j = 1; j <= i__1; ++j)
    {
      jj = ifice1 + j;
      iipvt = ifire1 + j;
      i__2 = *nre;
      for (i__ = j + 1; i__ <= i__2; ++i__)
	{
	  ii = ifire1 + i__;
	  /* 
	   *          Determine the Givens transformation on rows ii and iipvt. 
	   *          Apply the transformation to these rows (in whole E-matrix) 
	   *          to annihilate E(ii,jj)  (from columns jj up to n). 
	   *          Apply the transformations also to the A-matrix 
	   *          (from columns ifica up to n). 
	   *          Update the row transformation matrix Q. 
	   * 
	   */
	  nsp_ctrlpack_dgiv (&e[iipvt + jj * e_dim1], &e[ii + jj * e_dim1],
			     &sc, &ss);
	  nelm = *n - jj + 1;
	  C2F (drot) (&nelm, &e[iipvt + jj * e_dim1], lda,
		      &e[ii + jj * e_dim1], lda, &sc, &ss);
	  e[ii + jj * e_dim1] = 0.;
	  nelm = *n - *ifica + 1;
	  C2F (drot) (&nelm, &a[iipvt + *ifica * a_dim1], lda,
		      &a[ii + *ifica * a_dim1], lda, &sc, &ss);
	  C2F (drot) (m, &q[iipvt + q_dim1], ldq, &q[ii + q_dim1], ldq,
		      &sc, &ss);
	  /* L10: */
	}
      /* L20: */
    }
  /* 
   */
  return 0;
  /**** Last line of TRIAEK ********************************************* 
   */
}				/* triaek_ */

/** END OF TRIAEKTEXT 
 *UPTODATE TRIREDTEXT 
 */
int
nsp_ctrlpack_trired (double *a, int *lda, double *e, double *q, int *ldq,
		     double *z__, int *ldz, int *m, int *n, int *nblcks,
		     int *inuk, int *imuk, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, e_dim1, e_offset, q_dim1, q_offset, z_dim1, z_offset,
    i__1;

  /* Local variables */
  int mukp1, i__, ifica, k, ifice, ifira, ifire, ismuk, isnuk1;
  int muk, nuk;

  /* 
   *    PURPOSE: 
   * 
   *    On entry, it is assumed that the M by N matrices A and E have 
   *    been transformed to generalized Schur form by unitary trans- 
   *    formations (see Algorithm 3.2.1 in [1]), i.e., 
   * 
   *                   | s*E(eps,inf)-A(eps,inf) |     X       | 
   *         s*E - A = |-------------------------|-------------| . 
   *                   |            0            | s*E(r)-A(r) | 
   * 
   *    Here the pencil s*E(eps,inf)-A(eps,inf) is in staircase form. 
   *    This pencil contains all Kronecker column indices and infinite 
   *    elementary divisors of the pencil s*E - A. 
   *    The pencil s*E(r)-A(r) contains all Kronecker row indices and 
   *    finite elementary divisors of s*E - A. 
   *    Subroutine TRIRED performs the triangularization of the sub- 
   *    matrices having full row and column rank in the pencil 
   *    s*E(eps,inf)-A(eps,inf) using Algorithm 3.3.1 in [1]. 
   *    REMARK: This routine is intended to be called only from the 
   *            SLICOT routine FSTAIR. 
   * 
   *    PARAMETERS: 
   * 
   *    A - DOUBLE PRECISION array of dimension (LDA,N). 
   *        On entry, it contains the matrix A to be reduced. 
   *        On return, it contains the transformed matrix A. 
   *    E - DOUBLE PRECISION array of dimension (LDA,N). 
   *        On entry, it contains the matrix E to be reduced. 
   *        On return, it contains the transformed matrix E. 
   *    Q - DOUBLE PRECISION array of dimension (LDQ,M). 
   *        On entry, Q contains the row transformations corresponding 
   *        to the input matrices A and E. 
   *        On return, it contains the product of the input matrix Q and 
   *        the row transformation matrix that has transformed the rows 
   *        of the matrices A and E. 
   *    Z - DOUBLE PRECISION array of dimension (LDZ,N). 
   *        On entry, Z contains the column transformations corresponding 
   *        to the input matrices A and E. 
   *        On return, it contains the product of the input matrix Z and 
   *        the column transformation matrix that has transformed the 
   *        columns of the matrices A and E. 
   *    M - INT. 
   *        Number of rows in A and E, 1 <= M <= LDA. 
   *    N - INT. 
   *        Number of columns in A and E, N >= 1. 
   *    NBLCKS - INT. 
   *        Number of submatrices having full row rank >=0 in A(eps,inf). 
   *    INUK - INT array of dimension (NBLCKS). 
   *        Array containing the row dimensions nu(k) (k=1,..,NBLCKS) 
   *        of the submatrices having full row rank in the pencil 
   *        s*E(eps,inf)-A(eps,inf). 
   *    IMUK - INT array of dimension (NBLCKS). 
   *        Array containing the column dimensions mu(k) (k=1,..,NBLCKS) 
   *        of the submatrices having full column rank in the pencil. 
   *    IERR - INT. 
   *        IERR = 0, successful completion, 
   *               1, incorrect dimensions of a full row rank submatrix, 
   *               2, incorrect dimensions of a full column rank submatrix 
   * 
   *    REFERENCES: 
   * 
   *    [1] Th.G.J. Beelen, New Algorithms for Computing the Kronecker 
   *        structure of a Pencil with Applications to Systems and 
   *        Control Theory, Ph.D.Thesis, Eindhoven University of 
   *        Technology, The Netherlands, 1987. 
   * 
   *    CONTRIBUTOR: Th.G.J. Beelen (Philips Glas Eindhoven) 
   * 
   *    REVISIONS: 1988, January 29. 
   * 
   *    Specification of the parameters. 
   * 
   *    .. Scalar arguments .. 
   * 
   * 
   *    .. Array arguments .. 
   * 
   * 
   *    EXTERNAL SUBROUTINES: 
   * 
   *      TRIAAK, TRIAEK from SLICOT. 
   * 
   *    Local variables. 
   * 
   * 
   *    ISMUK  = sum(i=1,...,k) MU(i), 
   *    ISNUK1 = sum(i=1,...,k-1) NU(i). 
   * 
   */
  /* Parameter adjustments */
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  z_dim1 = *ldz;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  e_dim1 = *lda;
  e_offset = e_dim1 + 1;
  e -= e_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --imuk;
  --inuk;

  /* Function Body */
  ismuk = 0;
  isnuk1 = 0;
  i__1 = *nblcks;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ismuk += imuk[i__];
      isnuk1 += inuk[i__];
      /* L10: */
    }
  /* 
   *    NOTE:  ISNUK1 has not yet the correct value. 
   * 
   */
  *ierr = 0;
  mukp1 = 0;
  for (k = *nblcks; k >= 1; --k)
    {
      muk = imuk[k];
      nuk = inuk[k];
      isnuk1 -= nuk;
      /* 
       *       Determine left upper absolute coordinates of E(k) in E-matrix. 
       * 
       */
      ifire = isnuk1 + 1;
      ifice = ismuk + 1;
      /* 
       *       Determine left upper absolute coordinates of A(k) in A-matrix. 
       * 
       */
      ifira = ifire;
      ifica = ifice - muk;
      /* 
       *       Reduce E(k) to upper triangular form using Givens 
       *       transformations on rows only. Apply the same transformations 
       *       to the rows of A(k). 
       * 
       */
      if (mukp1 > nuk)
	{
	  *ierr = 1;
	  return 0;
	}
      /* 
       */
      nsp_ctrlpack_triaek (&a[a_offset], lda, &e[e_offset], &q[q_offset], ldq,
			   m, n, &nuk, &mukp1, &ifire, &ifice, &ifica);
      /* 
       *       Reduce A(k) to upper triangular form using Givens 
       *       transformations on columns only. Apply the same transformations 
       *       to the columns in the E-matrix. 
       * 
       */
      if (nuk > muk)
	{
	  *ierr = 2;
	  return 0;
	}
      /* 
       */
      nsp_ctrlpack_triaak (&a[a_offset], lda, &e[e_offset], &z__[z_offset],
			   ldz, n, &nuk, &muk, &ifira, &ifica);
      /* 
       */
      ismuk -= muk;
      mukp1 = muk;
      /* L20: */
    }
  /* 
   */
  return 0;
  /**** Last line of TRIRED ********************************************* 
   */
}				/* trired_ */

int
nsp_ctrlpack_bae (double *a, int *lda, double *e, double *q, int *ldq,
		  double *z__, int *ldz, int *m, int *n, int *istair,
		  int *ifira, int *ifica, int *nca, int *rank, double *wrk,
		  int *iwrk, double *tol)
{
  /* System generated locals */
  int a_dim1, a_offset, e_dim1, e_offset, q_dim1, q_offset, z_dim1, z_offset,
    i__1, i__2;
  double d__1;

  /* Local variables */
  int nelm, lsav;
  int jpvt, i__, k, l;
  int itype;
  int lzero;
  int ifica1, k1, nrows, ifira1, ii, kk, mj, ll, ip, nj, ir;
  double sc;
  double ss;
  int jc1, jc2, mxrank;
  double eijpvt, bmxnrm;
  int mk1, istpvt;
  double bmx;
  int imx, ist1, ist2;

  /* 
   *    LIBRARY INDEX: 
   * 
   * 
   * 
   *    PURPOSE: 
   * 
   *    Let A and E be M x N matrices with E in column echelon form. 
   *        is the machine precision. 
   * 
   *    REFERENCES: 
   * 
   *    [1] Th.G.J. Beelen, New Algorithms for Computing the Kronecker 
   *        structure of a Pencil with Applications to Systems and 
   *        Control Theory, Ph.D.Thesis, Eindhoven University of 
   *        Technology, The Netherlands, 1987. 
   * 
   *    CONTRIBUTOR: Th.G.J. Beelen (Philips Glass Eindhoven). 
   * 
   *    REVISIONS: 1988, January 29. 
   * 
   *    Specification of the parameters. 
   * 
   *    .. Scalar arguments .. 
   * 
   * 
   *    .. Array arguments .. 
   * 
   * 
   *    EXTERNAL SUBROUTINES/FUNCTIONS: 
   * 
   *      IDAMAX, DROT, DSWAP from BLAS. 
   *      DGIV from SLICOT. 
   * 
   *    Local variables. 
   * 
   * 
   *    Initialisation. 
   * 
   *    NJ = number of columns in submatrix Aj, 
   *    MJ = number of rows in submatrices Aj and Ej. 
   * 
   */
  /* Parameter adjustments */
  --istair;
  q_dim1 = *ldq;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  --iwrk;
  --wrk;
  z_dim1 = *ldz;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  e_dim1 = *lda;
  e_offset = e_dim1 + 1;
  e -= e_offset;
  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  nj = *nca;
  mj = *m + 1 - *ifira;
  ifira1 = *ifira - 1;
  ifica1 = *ifica - 1;
  i__1 = nj;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      iwrk[i__] = i__;
      /* L10: */
    }
  k = 1;
  lzero = FALSE;
  *rank = Min (nj, mj);
  mxrank = *rank;
  /* 
   *    WHILE (K <= MXRANK) and (LZERO = FALSE) DO 
   */
 L20:
  if (k <= mxrank && !lzero)
    {
      /* 
       *       Determine column in Aj with largest max-norm. 
       * 
       */
      bmxnrm = 0.;
      lsav = k;
      i__1 = nj;
      for (l = k; l <= i__1; ++l)
	{
	  /* 
	   *          IMX is relative index in column L of Aj where max el. is 
	   *          found. 
	   *          NOTE: the first el. in column L is in row K of matrix Aj. 
	   * 
	   */
	  kk = ifira1 + k;
	  ll = ifica1 + l;
	  i__2 = mj - k + 1;
	  imx = C2F (idamax) (&i__2, &a[kk + ll * a_dim1], &c__1);
	  bmx = (d__1 = a[imx + kk - 1 + ll * a_dim1], Abs (d__1));
	  if (bmx > bmxnrm)
	    {
	      bmxnrm = bmx;
	      lsav = l;
	    }
	  /* L30: */
	}
      /* 
       */
      if (bmxnrm < *tol)
	{
	  /* 
	   *          Set submatrix of Aj to zero. 
	   * 
	   */
	  i__1 = nj;
	  for (l = k; l <= i__1; ++l)
	    {
	      ll = ifica1 + l;
	      i__2 = mj;
	      for (i__ = k; i__ <= i__2; ++i__)
		{
		  ii = ifira1 + i__;
		  a[ii + ll * a_dim1] = 0.;
		  /* L40: */
		}
	      /* L50: */
	    }
	  lzero = TRUE;
	  *rank = k - 1;
	}
      else
	{
	  /* 
	   *          Check whether columns have to be interchanged. 
	   * 
	   */
	  if (lsav != k)
	    {
	      /* 
	       *             Interchange the columns in A which correspond to the 
	       *             columns lsav and k in Aj. Store the permutation in IWRK. 
	       * 
	       */
	      C2F (dswap) (m, &a[(ifica1 + k) * a_dim1 + 1], &c__1,
			   &a[(ifica1 + lsav) * a_dim1 + 1], &c__1);
	      ip = iwrk[lsav];
	      iwrk[lsav] = iwrk[k];
	      iwrk[k] = ip;
	    }
	  /* 
	   */
	  k1 = k + 1;
	  mk1 = nj - k + 1 + (*n - *nca - ifica1);
	  kk = ifica1 + k;
	  /* 
	   */
	  i__1 = mj;
	  for (ir = k1; ir <= i__1; ++ir)
	    {
	      /* 
	       */
	      i__ = mj - ir + k1;
	      /* 
	       *             II = absolute row number in A corresponding to row i in 
	       *                  Aj. 
	       * 
	       */
	      ii = ifira1 + i__;
	      /* 
	       *             Construct Givens transformation to annihilate Aj(i,k). 
	       *             Apply the row transformation to whole matrix A. 
	       *             (NOT only to Aj). 
	       *             Update row transformation matrix Q. 
	       * 
	       */
	      nsp_ctrlpack_dgiv (&a[ii - 1 + kk * a_dim1],
				 &a[ii + kk * a_dim1], &sc, &ss);
	      C2F (drot) (&mk1, &a[ii - 1 + kk * a_dim1], lda,
			  &a[ii + kk * a_dim1], lda, &sc, &ss);
	      a[ii + kk * a_dim1] = 0.;
	      C2F (drot) (m, &q[ii - 1 + q_dim1], ldq, &q[ii + q_dim1],
			  ldq, &sc, &ss);
	      /* 
	       *             Determine boundary type of matrix E at rows II-1 and II. 
	       * 
	       */
	      ist1 = istair[ii - 1];
	      ist2 = istair[ii];
	      if (ist1 * ist2 > 0)
		{
		  if (ist1 > 0)
		    {
		      /* 
		       *                   boundary form = (* x) 
		       *                                   (0 *) 
		       * 
		       */
		      itype = 1;
		    }
		  else
		    {
		      /* 
		       *                   boundary form = (x x) 
		       *                                   (x x) 
		       * 
		       */
		      itype = 3;
		    }
		}
	      else
		{
		  if (ist1 < 0)
		    {
		      /* 
		       *                   boundary form = (x x) 
		       *                                   (* x) 
		       * 
		       */
		      itype = 2;
		    }
		  else
		    {
		      /* 
		       *                   boundary form = (* x) 
		       *                                   (0 x) 
		       * 
		       */
		      itype = 4;
		    }
		}
	      /* 
	       *             Apply row transformation also to matrix E. 
	       * 
	       *             JC1 = absolute number of the column in E in which stair 
	       *                   element of row i-1 of Ej is present. 
	       *             JC2 = absolute number of the column in E in which stair 
	       *                   element of row i of Ej is present. 
	       * 
	       *             NOTE: JC1 < JC2   if ITYPE = 1. 
	       *                   JC1 = JC2   if ITYPE = 2, 3 or 4. 
	       * 
	       */
	      jc1 = Abs (ist1);
	      jc2 = Abs (ist2);
	      jpvt = Min (jc1, jc2);
	      nelm = *n - jpvt + 1;
	      /* 
	       */
	      C2F (drot) (&nelm, &e[ii - 1 + jpvt * e_dim1], lda,
			  &e[ii + jpvt * e_dim1], lda, &sc, &ss);
	      eijpvt = e[ii + jpvt * e_dim1];
	      /* 
	       */
	      switch (itype)
		{
		case 1:
		  goto L80;
		case 2:
		  goto L60;
		case 3:
		  goto L90;
		case 4:
		  goto L70;
		}
	      /* 
	       */
	    L60:
	      if (Abs (eijpvt) < *tol)
		{
		  /*                                                    (x x)    (* x) 
		   *                Boundary form has been changed from (* x) to (0 x) 
		   * 
		   */
		  istpvt = istair[ii];
		  istair[ii - 1] = istpvt;
		  istair[ii] = -(istpvt + 1);
		  e[ii + jpvt * e_dim1] = 0.;
		}
	      goto L90;
	      /* 
	       */
	    L70:
	      if (Abs (eijpvt) >= *tol)
		{
		  /* 
		   *                                                    (* x)    (x x) 
		   *                Boundary form has been changed from (0 x) to (* x) 
		   * 
		   */
		  istpvt = istair[ii - 1];
		  istair[ii - 1] = -istpvt;
		  istair[ii] = istpvt;
		}
	      goto L90;
	      /* 
	       *             Construct column Givens transformation to annihilate 
	       *             E(ii,jpvt). 
	       *             Apply column Givens transformation to matrix E. 
	       *             (NOT only to Ej). 
	       * 
	       */
	    L80:
	      nsp_ctrlpack_dgiv (&e[ii + (jpvt + 1) * e_dim1],
				 &e[ii + jpvt * e_dim1], &sc, &ss);
	      C2F (drot) (&ii, &e[(jpvt + 1) * e_dim1 + 1], &c__1,
			  &e[jpvt * e_dim1 + 1], &c__1, &sc, &ss);
	      e[ii + jpvt * e_dim1] = 0.;
	      /* 
	       *             Apply this transformation also to matrix A. 
	       *             (NOT only to Aj). 
	       *             Update column transformation matrix Z. 
	       * 
	       */
	      C2F (drot) (m, &a[(jpvt + 1) * a_dim1 + 1], &c__1,
			  &a[jpvt * a_dim1 + 1], &c__1, &sc, &ss);
	      C2F (drot) (n, &z__[(jpvt + 1) * z_dim1 + 1], &c__1,
			  &z__[jpvt * z_dim1 + 1], &c__1, &sc, &ss);
	      /* 
	       */
	    L90:
	      ;
	    }
	  /* 
	   */
	  ++k;
	}
      goto L20;
    }
  /*    END WHILE 20 
   * 
   *    Permute columns of Aj to original order. 
   * 
   */
  nrows = ifira1 + *rank;
  i__1 = nrows;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = nj;
      for (k = 1; k <= i__2; ++k)
	{
	  kk = ifica1 + k;
	  wrk[iwrk[k]] = a[i__ + kk * a_dim1];
	  /* L100: */
	}
      i__2 = nj;
      for (k = 1; k <= i__2; ++k)
	{
	  kk = ifica1 + k;
	  a[i__ + kk * a_dim1] = wrk[k];
	  /* L110: */
	}
      /* L120: */
    }
  /* 
   */
  return 0;
  /**** Last line of BAE ************************************************ 
   */
}				/* bae_ */

/** END OF BAETEXT 
 *UPTODATE DGIVTEXT 
 */
int nsp_ctrlpack_dgiv (double *da, double *db, double *dc, double *ds)
{
  /* System generated locals */
  double d__1;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  double r__, u, v;

  /* 
   *    LIBRARY INDEX: 
   * 
   *    2.1.4  Decompositions and transformations. 
   * 
   *    PURPOSE: 
   * 
   *    This routine constructs the Givens transformation 
   * 
   *           ( DC  DS ) 
   *       G = (        ),   DC**2 + DS**2 = 1.0D0 , 
   *           (-DS  DC ) 
   *                                T                          T 
   *    such that the vector (DA,DB)  is transformed into (R,0), i.e., 
   * 
   *           ( DC  DS ) ( DA )   ( R ) 
   *           (        ) (    ) = (   ) 
   *           (-DS  DC ) ( DB )   ( 0 ) . 
   * 
   *    This routine is a modification of the BLAS routine DROTG 
   *    (Algorithm 539) in order to leave the arguments DA and DB 
   *    unchanged. The value or R is not returned. 
   * 
   *    CONTRIBUTOR: P. Van Dooren (PRLB). 
   * 
   *    REVISIONS: 1987, November 24. 
   * 
   *    Specification of parameters. 
   * 
   *    .. Scalar Arguments .. 
   * 
   * 
   *    Local variables. 
   * 
   * 
   */
  if (Abs (*da) > Abs (*db))
    {
      u = *da + *da;
      v = *db / u;
      /*Computing 2nd power 
       */
      d__1 = v;
      r__ = sqrt (d__1 * d__1 + .25) * u;
      *dc = *da / r__;
      *ds = v * (*dc + *dc);
    }
  else
    {
      if (*db != 0.)
	{
	  u = *db + *db;
	  v = *da / u;
	  /*Computing 2nd power 
	   */
	  d__1 = v;
	  r__ = sqrt (d__1 * d__1 + .25) * u;
	  *ds = *db / r__;
	  *dc = v * (*ds + *ds);
	}
      else
	{
	  *dc = 1.;
	  *ds = 0.;
	}
    }
  return 0;
  /**** Last line of DGIV *********************************************** 
   */
}				/* dgiv_ */

/** END OF DGIVTEXT 
 *UPTODATE DROTITEXT 
 */

static int droti(int *n, double *x, int *incx, double *y, int *incy, double *c__, double *s)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  int i__;
  double dtemp;
  int ix, iy;

  /* 
   *    LIBRARY INDEX: 
   * 
   *    2.1.4 Decompositions and transfromations. 
   * 
   *    PURPOSE: 
   * 
   *    The subroutine DROTI performs the Givens transformation, defined 
   *    by C (cos) and S (sin), and interchanges the vectors involved, 
   *    i.e., 
   * 
   *       |X(i)|    | 0   1 |   | C   S |   |X(i)| 
   *       |    | := |       | x |       | x |    |, i = 1,...N. 
   *       |Y(i)|    | 1   0 |   |-S   C |   |Y(i)| 
   * 
   *    REMARK. This routine is a modification of DROT from BLAS. 
   * 
   *    CONTRIBUTOR: Th.G.J. Beelen (Philips Glass Eindhoven) 
   * 
   *    REVISIONS: 1988, February 02. 
   * 
   *    Specification of the parameters. 
   * 
   *    .. Scalar argumants .. 
   * 
   * 
   *    .. Array arguments .. 
   * 
   * 
   *    Local variables. 
   * 
   * 
   */
  /* Parameter adjustments */
  --y;
  --x;

  /* Function Body */
  if (*n <= 0)
    {
      return 0;
    }
  if (*incx != 1 || *incy != 1)
    {
      /* 
       *       Code for unequal increments or equal increments not equal to 1. 
       * 
       */
      ix = 1;
      iy = 1;
      if (*incx < 0)
	{
	  ix = (-(*n) + 1) * *incx + 1;
	}
      if (*incy < 0)
	{
	  iy = (-(*n) + 1) * *incy + 1;
	}
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  dtemp = *c__ * y[iy] - *s * x[ix];
	  y[iy] = *c__ * x[ix] + *s * y[iy];
	  x[ix] = dtemp;
	  ix += *incx;
	  iy += *incy;
	  /* L10: */
	}
    }
  else
    {
      /* 
       *       Code for both increments equal to 1. 
       * 
       */
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  dtemp = *c__ * y[i__] - *s * x[i__];
	  y[i__] = *c__ * x[i__] + *s * y[i__];
	  x[i__] = dtemp;
	  /* L20: */
	}
    }
  return 0;
}
