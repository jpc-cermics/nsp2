/* dbesk.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__15 = 15;
static int c__5 = 5;
static int c__1 = 1;

/*DECK DBESK 
 */
int
nsp_calpack_dbesk (double *x, double *fnu, int *kode, int *n, double *y,
		   int *nz, int *ierr)
{
  /* Initialized data */

  static int nulim[2] = { 35, 70 };

  /* System generated locals */
  int i__1;

  /* Local variables */
  double elim, xlim;
  int i__, j, k;
  double s, t, w[2], flgik;
  double s1, s2;
  int nb;
  double cn;
  int nd;
  double fn;
  int nn;
  double tm;
  int mz;
  double zn;
  double gln, fnn;
  int nud;
  double dnu, gnu, etx, trx, rtz;

  /****BEGIN PROLOGUE  DBESK 
   ****PURPOSE  Implement forward recursion on the three term recursion 
   *           relation for a sequence of non-negative order Bessel 
   *           functions K/SUB(FNU+I-1)/(X), or scaled Bessel functions 
   *           EXP(X)*K/SUB(FNU+I-1)/(X), I=1,...,N for real, positive 
   *           X and non-negative orders FNU. 
   ****LIBRARY   SLATEC 
   ****CATEGORY  C10B3 
   ****TYPE      DOUBLE PRECISION (BESK-S, DBESK-D) 
   ****KEYWORDS  K BESSEL FUNCTION, SPECIAL FUNCTIONS 
   ****AUTHOR  Amos, D. E., (SNLA) 
   ****DESCRIPTION 
   * 
   *    Abstract  **** a double precision routine **** 
   *        DBESK implements forward recursion on the three term 
   *        recursion relation for a sequence of non-negative order Bessel 
   *        functions K/sub(FNU+I-1)/(X), or scaled Bessel functions 
   *        EXP(X)*K/sub(FNU+I-1)/(X), I=1,..,N for real X .GT. 0.0D0 and 
   *        non-negative orders FNU.  If FNU .LT. NULIM, orders FNU and 
   *        FNU+1 are obtained from DBSKNU to start the recursion.  If 
   *        FNU .GE. NULIM, the uniform asymptotic expansion is used for 
   *        orders FNU and FNU+1 to start the recursion.  NULIM is 35 or 
   *        70 depending on whether N=1 or N .GE. 2.  Under and overflow 
   *        tests are made on the leading term of the asymptotic expansion 
   *        before any extensive computation is done. 
   * 
   *        The maximum number of significant digits obtainable 
   *        is the smaller of 14 and the number of digits carried in 
   *        double precision arithmetic. 
   * 
   *    Description of Arguments 
   * 
   *        Input      X,FNU are double precision 
   *          X      - X .GT. 0.0D0 
   *          FNU    - order of the initial K function, FNU .GE. 0.0D0 
   *          KODE   - a parameter to indicate the scaling option 
   *                   KODE=1 returns Y(I)=       K/sub(FNU+I-1)/(X), 
   *                                       I=1,...,N 
   *                   KODE=2 returns Y(I)=EXP(X)*K/sub(FNU+I-1)/(X), 
   *                                       I=1,...,N 
   *          N      - number of members in the sequence, N .GE. 1 
   * 
   *        Output     Y is double precision 
   *          Y      - a vector whose first N components contain values 
   *                   for the sequence 
   *                   Y(I)=       k/sub(FNU+I-1)/(X), I=1,...,N  or 
   *                   Y(I)=EXP(X)*K/sub(FNU+I-1)/(X), I=1,...,N 
   *                   depending on KODE 
   *          NZ     - number of components of Y set to zero due to 
   *                   underflow with KODE=1, 
   *                   NZ=0   , normal return, computation completed 
   *                   NZ .NE. 0, first NZ components of Y set to zero 
   *                            due to underflow, Y(I)=0.0D0, I=1,...,NZ 
   * 
   *    Error Conditions 
   *        Improper input arguments - a fatal error 
   *        Overflow - a fatal error 
   *        Underflow with KODE=1 -  a non-fatal error (NZ .NE. 0) 
   * 
   ****REFERENCES  F. W. J. Olver, Tables of Bessel Functions of Moderate 
   *                or Large Orders, NPL Mathematical Tables 6, Her 
   *                Majesty's Stationery Office, London, 1962. 
   *              N. M. Temme, On the numerical evaluation of the modified 
   *                Bessel function of the third kind, Journal of 
   *                Computational Physics 19, (1975), pp. 324-337. 
   ****ROUTINES CALLED  D1MACH, DASYIK, DBESK0, DBESK1, DBSK0E, DBSK1E, 
   *                   DBSKNU, I1MACH, XERMSG 
   ****REVISION HISTORY  (YYMMDD) 
   *  790201  DATE WRITTEN 
   *  890531  Changed all specific intrinsics to generic.  (WRB) 
   *  890911  Removed unnecessary intrinsics.  (WRB) 
   *  890911  REVISION DATE from Version 3.2 
   *  891214  Prologue converted to Version 4.0 format.  (BAB) 
   *  900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ) 
   *  920501  Reformatted the REFERENCES section.  (WRB) 
   ****END PROLOGUE  DBESK 
   * 
   */
  /* Parameter adjustments */
  --y;

  /* Function Body */
  /****FIRST EXECUTABLE STATEMENT  DBESK 
   */
  *ierr = 0;
  nn = -nsp_calpack_i1mach (&c__15);
  elim = (nn * nsp_calpack_d1mach (&c__5) - 3.) * 2.303;
  xlim = nsp_calpack_d1mach (&c__1) * 1e3;
  if (*kode < 1 || *kode > 2)
    {
      goto L280;
    }
  if (*fnu < 0.)
    {
      goto L290;
    }
  if (*x <= 0.)
    {
      goto L300;
    }
  if (*x < xlim)
    {
      goto L320;
    }
  if (*n < 1)
    {
      goto L310;
    }
  etx = (double) (*kode - 1);
  /* 
   *    ND IS A DUMMY VARIABLE FOR N 
   *    GNU IS A DUMMY VARIABLE FOR FNU 
   *    NZ = NUMBER OF UNDERFLOWS ON KODE=1 
   * 
   */
  nd = *n;
  *nz = 0;
  nud = (int) (*fnu);
  dnu = *fnu - nud;
  gnu = *fnu;
  nn = Min (2, nd);
  fn = *fnu + *n - 1;
  fnn = fn;
  if (fn < 2.)
    {
      goto L150;
    }
  /* 
   *    OVERFLOW TEST  (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION) 
   *    FOR THE LAST ORDER, FNU+N-1.GE.NULIM 
   * 
   */
  zn = *x / fn;
  if (zn == 0.)
    {
      goto L320;
    }
  rtz = sqrt (zn * zn + 1.);
  gln = log ((rtz + 1.) / zn);
  t = rtz * (1. - etx) + etx / (zn + rtz);
  cn = -fn * (t - gln);
  if (cn > elim)
    {
      goto L320;
    }
  if (nud < nulim[nn - 1])
    {
      goto L30;
    }
  if (nn == 1)
    {
      goto L20;
    }
L10:
  /* 
   *    UNDERFLOW TEST (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION) 
   *    FOR THE FIRST ORDER, FNU.GE.NULIM 
   * 
   */
  fn = gnu;
  zn = *x / fn;
  rtz = sqrt (zn * zn + 1.);
  gln = log ((rtz + 1.) / zn);
  t = rtz * (1. - etx) + etx / (zn + rtz);
  cn = -fn * (t - gln);
L20:
  if (cn < -elim)
    {
      goto L230;
    }
  /* 
   *    ASYMPTOTIC EXPANSION FOR ORDERS FNU AND FNU+1.GE.NULIM 
   * 
   */
  flgik = -1.;
  nsp_calpack_dasyik (x, &gnu, kode, &flgik, &rtz, &cn, &nn, &y[1]);
  if (nn == 1)
    {
      goto L240;
    }
  trx = 2. / *x;
  tm = (gnu + gnu + 2.) / *x;
  goto L130;
  /* 
   */
L30:
  if (*kode == 2)
    {
      goto L40;
    }
  /* 
   *    UNDERFLOW TEST (LEADING EXPONENTIAL OF ASYMPTOTIC EXPANSION IN X) 
   *    FOR ORDER DNU 
   * 
   */
  if (*x > elim)
    {
      goto L230;
    }
L40:
  if (dnu != 0.)
    {
      goto L80;
    }
  if (*kode == 2)
    {
      goto L50;
    }
  s1 = nsp_calpack_dbesk0 (x);
  goto L60;
L50:
  s1 = nsp_calpack_dbsk0e (x);
L60:
  if (nud == 0 && nd == 1)
    {
      goto L120;
    }
  if (*kode == 2)
    {
      goto L70;
    }
  s2 = nsp_calpack_dbesk1 (x);
  goto L90;
L70:
  s2 = nsp_calpack_dbsk1e (x);
  goto L90;
L80:
  nb = 2;
  if (nud == 0 && nd == 1)
    {
      nb = 1;
    }
  nsp_calpack_dbsknu (x, &dnu, kode, &nb, w, nz);
  s1 = w[0];
  if (nb == 1)
    {
      goto L120;
    }
  s2 = w[1];
L90:
  trx = 2. / *x;
  tm = (dnu + dnu + 2.) / *x;
  /*    FORWARD RECUR FROM DNU TO FNU+1 TO GET Y(1) AND Y(2) 
   */
  if (nd == 1)
    {
      --nud;
    }
  if (nud > 0)
    {
      goto L100;
    }
  if (nd > 1)
    {
      goto L120;
    }
  s1 = s2;
  goto L120;
L100:
  i__1 = nud;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      s = s2;
      s2 = tm * s2 + s1;
      s1 = s;
      tm += trx;
      /* L110: */
    }
  if (nd == 1)
    {
      s1 = s2;
    }
L120:
  y[1] = s1;
  if (nd == 1)
    {
      goto L240;
    }
  y[2] = s2;
L130:
  if (nd == 2)
    {
      goto L240;
    }
  /*    FORWARD RECUR FROM FNU+2 TO FNU+N-1 
   */
  i__1 = nd;
  for (i__ = 3; i__ <= i__1; ++i__)
    {
      y[i__] = tm * y[i__ - 1] + y[i__ - 2];
      tm += trx;
      /* L140: */
    }
  goto L240;
  /* 
   */
L150:
  /*    UNDERFLOW TEST FOR KODE=1 
   */
  if (*kode == 2)
    {
      goto L160;
    }
  if (*x > elim)
    {
      goto L230;
    }
L160:
  /*    OVERFLOW TEST 
   */
  if (fn <= 1.)
    {
      goto L170;
    }
  if (-fn * (log (*x) - .693) > elim)
    {
      goto L320;
    }
L170:
  if (dnu == 0.)
    {
      goto L180;
    }
  nsp_calpack_dbsknu (x, fnu, kode, &nd, &y[1], &mz);
  goto L240;
L180:
  j = nud;
  if (j == 1)
    {
      goto L210;
    }
  ++j;
  if (*kode == 2)
    {
      goto L190;
    }
  y[j] = nsp_calpack_dbesk0 (x);
  goto L200;
L190:
  y[j] = nsp_calpack_dbsk0e (x);
L200:
  if (nd == 1)
    {
      goto L240;
    }
  ++j;
L210:
  if (*kode == 2)
    {
      goto L220;
    }
  y[j] = nsp_calpack_dbesk1 (x);
  goto L240;
L220:
  y[j] = nsp_calpack_dbsk1e (x);
  goto L240;
  /* 
   *    UPDATE PARAMETERS ON UNDERFLOW 
   * 
   */
L230:
  ++nud;
  --nd;
  if (nd == 0)
    {
      goto L240;
    }
  nn = Min (2, nd);
  gnu += 1.;
  if (fnn < 2.)
    {
      goto L230;
    }
  if (nud < nulim[nn - 1])
    {
      goto L230;
    }
  goto L10;
L240:
  *nz = *n - nd;
  if (*nz == 0)
    {
      return 0;
    }
  if (nd == 0)
    {
      goto L260;
    }
  i__1 = nd;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      j = *n - i__ + 1;
      k = nd - i__ + 1;
      y[j] = y[k];
      /* L250: */
    }
L260:
  i__1 = *nz;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      y[i__] = 0.;
      /* L270: */
    }
  return 0;
  /* 
   * 
   * 
   */
L280:
  /*     CALL XERMSG ('SLATEC', 'DBESK', 
   *    +   'SCALING OPTION, KODE, NOT 1 OR 2', 2, 1) 
   */
  *ierr = 1;
  return 0;
L290:
  /*     CALL XERMSG ('SLATEC', 'DBESK', 'ORDER, FNU, LESS THAN ZERO', 2, 
   *    +   1) 
   */
  *ierr = 1;
  return 0;
L300:
  /*     CALL XERMSG ('SLATEC', 'DBESK', 'X LESS THAN OR EQUAL TO ZERO', 
   *    +   2, 1) 
   */
  *ierr = 1;
  return 0;
L310:
  /*     CALL XERMSG ('SLATEC', 'DBESK', 'N LESS THAN ONE', 2, 1) 
   */
  *ierr = 1;
  return 0;
L320:
  /*     CALL XERMSG ('SLATEC', 'DBESK', 
   *    +   'OVERFLOW, FNU OR N TOO LARGE OR X TOO SMALL', 6, 1) 
   */
  *ierr = 2;
  return 0;
}				/* dbesk_ */
#endif
