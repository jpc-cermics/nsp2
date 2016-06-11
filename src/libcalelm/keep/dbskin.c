/* dbskin.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__15 = 15;
static int c__5 = 5;
static int c__4 = 4;
static int c__14 = 14;
static int c__2 = 2;

/*DECK DBSKIN 
 */
int
nsp_calpack_dbskin (double *x, int *n, int *kode, int *m, double *y, int *nz,
		    int *ierr)
{
  /* Initialized data */

  static double a[50] =
    { 1., .5, .375, .3125, .2734375, .24609375, .2255859375, .20947265625,
    .196380615234375, .1854705810546875, .176197052001953125,
    .168188095092773438,
    .161180257797241211, .154981017112731934, .149445980787277222,
    .144464448094367981,
    .139949934091418982, .135833759559318423, .132060599571559578,
    .128585320635465905,
    .125370687619579257, .122385671247684513, .119604178719328047,
    .117004087877603524,
    .114566502713486784, .112275172659217048, .110116034723462874,
    .108076848895250599,
    .106146905164978267, .104316786110409676, .102578173008569515,
    .100923686347140974,
    .0993467537479668965, .0978414999033007314, .0964026543164874854,
    .0950254735405376642,
    .0937056752969190855, .09243938238750126, .0912230747245078224,
    .0900535481254756708,
    .0889278787739072249, .0878433924473961612, .0867976377754033498,
    .0857883629175498224,
    .0848134951571231199, .0838711229887106408, .0829594803475290034,
    .0820769326842574183,
    .0812219646354630702, .0803931690779583449
  };
  static double hrtpi = .886226925452758014;

  /* System generated locals */
  int i__1, i__2;
  double d__1;


  /* Local variables */
  int nflg, nlim;
  double xlim, h__[31];
  int i__, k, icase;
  double w, enlim, xnlim;
  int ktrms, m3;
  double t1, t2;
  int ne;
  double fn;
  int il, kk;
  double hn, gr;
  int nl, nn;
  int np, ns, nt;
  double ss, xp;
  double ys[3];
  int i1m;
  double exi[102], tol, yss[3];

  /****BEGIN PROLOGUE  DBSKIN 
   ****PURPOSE  Compute repeated integrals of the K-zero Bessel function. 
   ****LIBRARY   SLATEC 
   ****CATEGORY  C10F 
   ****TYPE      DOUBLE PRECISION (BSKIN-S, DBSKIN-D) 
   ****KEYWORDS  BICKLEY FUNCTIONS, EXPONENTIAL INTEGRAL, 
   *            INTEGRALS OF BESSEL FUNCTIONS, K-ZERO BESSEL FUNCTION 
   ****AUTHOR  Amos, D. E., (SNLA) 
   ****DESCRIPTION 
   * 
   *        The following definitions are used in DBSKIN: 
   * 
   *    Definition 1 
   *        KI(0,X) = K-zero Bessel function. 
   * 
   *    Definition 2 
   *        KI(N,X) = Bickley Function 
   *                =  integral from X to infinity of KI(N-1,t)dt 
   *                    for X .ge. 0 and N = 1,2,... 
   * _____________________________________________________________________ 
   *   DBSKIN computes a sequence of Bickley functions (repeated integrals 
   *   of the K0 Bessel function); i.e. for fixed X and N and for K=1,..., 
   *   DBSKIN computes the sequence 
   * 
   *                    Y(K) =         KI(N+K-1,X) for KODE=1 
   *         or 
   *                    Y(K) = EXP(X)*KI(N+K-1,X) for KODE=2, 
   * 
   *        for N.ge.0 and X.ge.0 (N and X cannot be zero simultaneously). 
   * 
   *     INPUT      X is DOUBLE PRECISION 
   *       X      - Argument, X .ge. 0.0D0 
   *       N      - Order of first member of the sequence N .ge. 0 
   *       KODE   - Selection parameter 
   *            KODE = 1 returns Y(K)=        KI(N+K-1,X), K=1,M 
   *                 = 2 returns Y(K)=EXP(X)*KI(N+K-1,X), K=1,M 
   *       M      - Number of members in the sequence, M.ge.1 
   * 
   *      OUTPUT     Y is a DOUBLE PRECISION VECTOR 
   *        Y      - A vector of dimension at least M containing the 
   *                 sequence selected by KODE. 
   *        NZ     - Underflow flag 
   *                 NZ = 0 means computation completed 
   *                    = 1 means an exponential underflow occurred on 
   *                        KODE=1.  Y(K)=0.0D0, K=1,...,M is returned 
   *                        KODE=1 AND Y(K)=0.0E0, K=1,...,M IS RETURNED 
   *        IERR   - Error flag 
   *                   IERR=0, Normal return, computation completed 
   *                   IERR=1, Input error,   no computation 
   *                   IERR=2, Error,         no computation 
   *                           Algorithm termination condition not met 
   * 
   *        The nominal computational accuracy is the maximum of unit 
   *        roundoff (=D1MACH(4)) and 1.0D-18 since critical constants 
   *        are given to only 18 digits. 
   * 
   *        BSKIN is the single precision version of DBSKIN. 
   * 
   **Long Description: 
   * 
   *        Numerical recurrence on 
   * 
   *     (L-1)*KI(L,X) = X(KI(L-3,X) - KI(L-1,X)) + (L-2)*KI(L-2,X) 
   * 
   *        is stable where recurrence is carried forward or backward 
   *        away from INT(X+0.5).  The power series for indices 0,1 and 2 
   *        on 0.le.X.le.2 starts a stable recurrence for indices 
   *        greater than 2.  If N is sufficiently large (N.gt.NLIM), the 
   *        uniform asymptotic expansion for N to INFINITY is more 
   *        economical.  On X.gt.2 the recursion is started by evaluating 
   *        the uniform expansion for the three members whose indices are 
   *        closest to INT(X+0.5) within the set N,...,N+M-1.  Forward 
   *        recurrence, backward recurrence or both complete the 
   *        sequence depending on the relation of INT(X+0.5) to the 
   *        indices N,...,N+M-1. 
   * 
   ****REFERENCES  D. E. Amos, Uniform asymptotic expansions for 
   *                exponential integrals E(N,X) and Bickley functions 
   *                KI(N,X), ACM Transactions on Mathematical Software, 
   *                1983. 
   *              D. E. Amos, A portable Fortran subroutine for the 
   *                Bickley functions KI(N,X), Algorithm 609, ACM 
   *                Transactions on Mathematical Software, 1983. 
   ****ROUTINES CALLED  D1MACH, DBKIAS, DBKISR, DEXINT, DGAMRN, I1MACH 
   ****REVISION HISTORY  (YYMMDD) 
   *  820601  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  891006  Cosmetic changes to prologue.  (WRB) 
   *  891009  Removed unreferenced statement label.  (WRB) 
   *  891009  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  920501  Reformatted the REFERENCES section.  (WRB) 
   ****END PROLOGUE  DBSKIN 
   *----------------------------------------------------------------------- 
   *            COEFFICIENTS IN SERIES OF EXPONENTIAL INTEGRALS 
   *----------------------------------------------------------------------- 
   */
  /* Parameter adjustments */
  --y;

  /* Function Body */
  /*----------------------------------------------------------------------- 
   *            SQRT(PI)/2 
   *----------------------------------------------------------------------- 
   * 
   ****FIRST EXECUTABLE STATEMENT  DBSKIN 
   */
  *ierr = 0;
  *nz = 0;
  if (*x < 0.)
    {
      *ierr = 1;
    }
  if (*n < 0)
    {
      *ierr = 1;
    }
  if (*kode < 1 || *kode > 2)
    {
      *ierr = 1;
    }
  if (*m < 1)
    {
      *ierr = 1;
    }
  if (*x == 0. && *n == 0)
    {
      *ierr = 1;
    }
  if (*ierr != 0)
    {
      return 0;
    }
  if (*x == 0.)
    {
      goto L300;
    }
  i1m = -nsp_calpack_i1mach (&c__15);
  t1 = nsp_calpack_d1mach (&c__5) * 2.3026 * i1m;
  xlim = t1 - 3.228086;
  t2 = t1 + (*n + *m - 1);
  if (t2 > 1e3)
    {
      xlim = t1 - (log (t2) - .451583) * .5;
    }
  if (*x > xlim && *kode == 1)
    {
      goto L320;
    }
  /*Computing MAX 
   */
  d__1 = nsp_calpack_d1mach (&c__4);
  tol = Max (d__1, 1e-18);
  i1m = nsp_calpack_i1mach (&c__14);
  /*----------------------------------------------------------------------- 
   *    LN(NLIM) = 0.125*LN(EPS),   NLIM = 2*KTRMS+N 
   *----------------------------------------------------------------------- 
   */
  xnlim = (i1m - 1) * .287823 * nsp_calpack_d1mach (&c__5);
  enlim = exp (xnlim);
  nlim = (int) enlim + 2;
  nlim = Min (100, nlim);
  nlim = Max (20, nlim);
  m3 = Min (*m, 3);
  nl = *n + *m - 1;
  if (*x > 2.)
    {
      goto L130;
    }
  if (*n > nlim)
    {
      goto L280;
    }
  /*----------------------------------------------------------------------- 
   *    COMPUTATION BY SERIES FOR 0.LE.X.LE.2 
   *----------------------------------------------------------------------- 
   */
  nflg = 0;
  nn = *n;
  if (nl <= 2)
    {
      goto L60;
    }
  m3 = 3;
  nn = 0;
  nflg = 1;
L60:
  xp = 1.;
  if (*kode == 2)
    {
      xp = exp (*x);
    }
  i__1 = m3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      nsp_calpack_dbkisr (x, &nn, &w, ierr);
      if (*ierr != 0)
	{
	  return 0;
	}
      w *= xp;
      if (nn < *n)
	{
	  goto L70;
	}
      kk = nn - *n + 1;
      y[kk] = w;
    L70:
      ys[i__ - 1] = w;
      ++nn;
      /* L80: */
    }
  if (nflg == 0)
    {
      return 0;
    }
  ns = nn;
  xp = 1.;
L90:
  /*----------------------------------------------------------------------- 
   *    FORWARD RECURSION SCALED BY EXP(X) ON ICASE=0,1,2 
   *----------------------------------------------------------------------- 
   */
  fn = (double) (ns - 1);
  il = nl - ns + 1;
  if (il <= 0)
    {
      return 0;
    }
  i__1 = il;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      t1 = ys[1];
      t2 = ys[2];
      ys[2] = (*x * (ys[0] - ys[2]) + (fn - 1.) * ys[1]) / fn;
      ys[1] = t2;
      ys[0] = t1;
      fn += 1.;
      if (ns < *n)
	{
	  goto L100;
	}
      kk = ns - *n + 1;
      y[kk] = ys[2] * xp;
    L100:
      ++ns;
      /* L110: */
    }
  return 0;
  /*----------------------------------------------------------------------- 
   *    COMPUTATION BY ASYMPTOTIC EXPANSION FOR X.GT.2 
   *----------------------------------------------------------------------- 
   */
L130:
  w = *x + .5;
  nt = (int) w;
  if (nl > nt)
    {
      goto L270;
    }
  /*----------------------------------------------------------------------- 
   *    CASE NL.LE.NT, ICASE=0 
   *----------------------------------------------------------------------- 
   */
  icase = 0;
  nn = nl;
  /*Computing MIN 
   */
  i__1 = *m - m3;
  nflg = Min (i__1, 1);
L140:
  kk = (nlim - nn) / 2;
  ktrms = Max (0, kk);
  ns = nn + 1;
  np = nn - m3 + 1;
  xp = 1.;
  if (*kode == 1)
    {
      xp = exp (-(*x));
    }
  i__1 = m3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      kk = i__;
      nsp_calpack_dbkias (x, &np, &ktrms, a, &w, &kk, &ne, &gr, h__, ierr);
      if (*ierr != 0)
	{
	  return 0;
	}
      ys[i__ - 1] = w;
      ++np;
      /* L150: */
    }
  /*----------------------------------------------------------------------- 
   *    SUM SERIES OF EXPONENTIAL INTEGRALS BACKWARD 
   *----------------------------------------------------------------------- 
   */
  if (ktrms == 0)
    {
      goto L160;
    }
  ne = ktrms + ktrms + 1;
  np = nn - m3 + 2;
  nsp_calpack_dexint (x, &np, &c__2, &ne, &tol, exi, nz, ierr);
  if (*nz != 0)
    {
      goto L320;
    }
L160:
  i__1 = m3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ss = 0.;
      if (ktrms == 0)
	{
	  goto L180;
	}
      kk = i__ + ktrms + ktrms - 2;
      il = ktrms;
      i__2 = ktrms;
      for (k = 1; k <= i__2; ++k)
	{
	  ss += a[il - 1] * exi[kk - 1];
	  kk += -2;
	  --il;
	  /* L170: */
	}
    L180:
      ys[i__ - 1] += ss;
      /* L190: */
    }
  if (icase == 1)
    {
      goto L200;
    }
  if (nflg != 0)
    {
      goto L220;
    }
L200:
  i__1 = m3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__] = ys[i__ - 1] * xp;
      /* L210: */
    }
  if (icase == 1 && nflg == 1)
    {
      goto L90;
    }
  return 0;
L220:
  /*----------------------------------------------------------------------- 
   *    BACKWARD RECURSION SCALED BY EXP(X) ICASE=0,2 
   *----------------------------------------------------------------------- 
   */
  kk = nn - *n + 1;
  k = m3;
  i__1 = m3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[kk] = ys[k - 1] * xp;
      yss[i__ - 1] = ys[i__ - 1];
      --kk;
      --k;
      /* L230: */
    }
  il = kk;
  if (il <= 0)
    {
      goto L250;
    }
  fn = (double) (nn - 3);
  i__1 = il;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      t1 = ys[1];
      t2 = ys[0];
      ys[0] = ys[1] + ((fn + 2.) * ys[2] - (fn + 1.) * ys[0]) / *x;
      ys[1] = t2;
      ys[2] = t1;
      y[kk] = ys[0] * xp;
      --kk;
      fn += -1.;
      /* L240: */
    }
L250:
  if (icase != 2)
    {
      return 0;
    }
  i__1 = m3;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ys[i__ - 1] = yss[i__ - 1];
      /* L260: */
    }
  goto L90;
L270:
  if (*n < nt)
    {
      goto L290;
    }
  /*----------------------------------------------------------------------- 
   *    ICASE=1, NT.LE.N.LE.NL WITH FORWARD RECURSION 
   *----------------------------------------------------------------------- 
   */
L280:
  nn = *n + m3 - 1;
  /*Computing MIN 
   */
  i__1 = *m - m3;
  nflg = Min (i__1, 1);
  icase = 1;
  goto L140;
  /*----------------------------------------------------------------------- 
   *    ICASE=2, N.LT.NT.LT.NL WITH BOTH FORWARD AND BACKWARD RECURSION 
   *----------------------------------------------------------------------- 
   */
L290:
  nn = nt + 1;
  /*Computing MIN 
   */
  i__1 = *m - m3;
  nflg = Min (i__1, 1);
  icase = 2;
  goto L140;
  /*----------------------------------------------------------------------- 
   *    X=0 CASE 
   *----------------------------------------------------------------------- 
   */
L300:
  fn = (double) (*n);
  hn = fn * .5;
  gr = nsp_calpack_dgamrn (&hn);
  y[1] = hrtpi * gr;
  if (*m == 1)
    {
      return 0;
    }
  y[2] = hrtpi / (hn * gr);
  if (*m == 2)
    {
      return 0;
    }
  i__1 = *m;
  for (k = 3; k <= i__1; ++k)
    {
      y[k] = fn * y[k - 2] / (fn + 1.);
      fn += 1.;
      /* L310: */
    }
  return 0;
  /*----------------------------------------------------------------------- 
   *    UNDERFLOW ON KODE=1, X.GT.XLIM 
   *----------------------------------------------------------------------- 
   */
L320:
  *nz = *m;
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__] = 0.;
      /* L330: */
    }
  return 0;
}				/* dbskin_ */
#endif
