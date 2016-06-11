/* dspfit.f -- translated by f2c (version 19961017).
 *
 *
 */
#if 0
#include "calpack.h"

/* Table of constant values */

static int c__4 = 4;

/* 
 *  This code comes from the NSWC fortran library with slight 
 *  modifications from Bruno Pincon 
 * 
 */
int
nsp_calpack_spfit (double *x, double *y, double *wgt, int *m,
		   double *break__, int *l, double *z__, double *a,
		   double *wk, int *ierr)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double temp[20], b, c__;
  int j, k, n;
  int la;
  double dx;
  int lq, lw, lm1;

  /*----------------------------------------------------------------------- 
   *           WEIGHTED LEAST SQUARES CUBIC SPLINE FITTING 
   *----------------------------------------------------------------------- 
   *--------------------- 
   */
  /* Parameter adjustments */
  --wgt;
  --y;
  --x;
  --break__;
  --z__;
  --a;
  --wk;

  /* Function Body */
  n = *l + 2;
  /* 
   *               DEFINE THE KNOTS FOR THE B-SPLINES 
   * 
   */
  wk[1] = break__[1];
  wk[2] = break__[1];
  wk[3] = break__[1];
  wk[4] = break__[1];
  i__1 = *l;
  for (j = 2; j <= i__1; ++j)
    {
      /*the conditions break(k) < break(k+1) ar 
       */
      wk[j + 3] = break__[j];
      /*verified in the interface 
       */
    }
  wk[*l + 4] = break__[*l];
  wk[*l + 5] = break__[*l];
  wk[*l + 6] = break__[*l];
  /* 
   *    OBTAIN THE B-SPLINE COEFFICIENTS OF THE LEAST SQUARES FIT 
   * 
   */
  la = n + 5;
  /*start indices (in wk) for the others working area 
   */
  lw = la + n;
  /* 
   */
  lq = lw + n;
  /* 
   */
  nsp_calpack_bslsq (&x[1], &y[1], &wgt[1], m, &wk[1], &n, &c__4, &wk[la],
		     &wk[lw], &wk[lq], ierr);
  /* 
   *    pour BSLSQ : IERR =-1  not enought points for the fit 
   *                 IERR = 0  OK 
   *                 IERR = 1  non uniqness of the solution (but a solution is computed) 
   * 
   */
  if (*ierr >= 0)
    {
      /*        OBTAIN THE COEFFICIENTS OF THE FIT IN TAYLOR SERIES FORM 
       */
      nsp_calpack_bspp (&wk[1], &wk[la], &n, &c__4, &break__[1], &wk[lq],
			&lm1, temp);
      k = lq;
      i__1 = lm1;
      for (j = 1; j <= i__1; ++j)
	{
	  z__[j] = wk[k];
	  a[j] = wk[k + 1];
	  k += 4;
	}
      /*a trick to get the spline value (Z(L)) and first derivative 
       *(A(L)) on the last breakpoint : the last polynomial piece 
       *has the form  Z(LM1) + A(LM1)(x- break(l-1)) + B(LM1)(...) 
       */
      dx = break__[*l] - break__[*l - 1];
      b = wk[lq + (*l - 2 << 2) + 2];
      c__ = wk[lq + (*l - 2 << 2) + 3];
      z__[*l] = z__[lm1] + dx * (a[lm1] + dx * (b + dx * c__));
      a[*l] = a[lm1] + dx * (b * 2. + dx * 3. * c__);
    }
  return 0;
}				/* spfit_ */

int
nsp_calpack_bslsq (double *tau, double *gtau, double *wgt, int *ntau,
		   double *t, int *n, int *k, double *a, double *wk,
		   double *q, int *ierr)
{
  /* System generated locals */
  int q_dim1, q_offset, i__1, i__2, i__3;

  /* Local variables */
  int left, i__, j, l;
  int jj, mm;
  double dw;
  int leftmk, ntau_count__;

  /*----------------------------------------------------------------------- 
   * 
   *       BSLSQ PRODUCES THE B-SPLINE COEFFICIENTS OF A PIECEWISE 
   *             POLYNOMIAL P(X) OF ORDER K WHICH MINIMIZES 
   * 
   *               SUM (WGT(J)*(P(TAU(J)) - GTAU(J))**2). 
   * 
   * 
   *    INPUT ... 
   * 
   *      TAU   ARRAY OF LENGTH NTAU CONTAINING DATA POINT ABSCISSAE. 
   *      GTAU  ARRAY OF LENGTH NTAU CONTAINING DATA POINT ORDINATES. 
   *      WGT   ARRAY OF LENGTH NTAU CONTAINING THE WEIGHTS. 
   *      NTAU  NUMBER OF DATA POINTS TO BE FITTED. 
   *      T     KNOT SEQUENCE OF LENGTH N + K. 
   *      N     DIMENSION OF THE PIECEWISE POLYNOMIAL SPACE. 
   *      K     ORDER OF THE B-SPLINES. 
   * 
   *    OUTPUT ... 
   * 
   *      A     ARRAY OF LENGTH N CONTAINING THE B-SPLINE COEFFICIENTS 
   *            OF THE L2 APPROXIMATION. 
   * 
   *      IERR  INT REPORTING THE STATUS OF THE RESULTS ... 
   * 
   *            0  THE COEFFICIENT MATRIX IS NONSIGULAR. THE 
   *               UNIQUE LEAST SQUARES SOLUTION WAS OBTAINED. 
   *            1  THE COEFFICIENT MATRIX IS SINGULAR. A 
   *               LEAST SQUARES SOLUTION WAS OBTAINED. 
   *           -1  INPUT ERRORS WERE DETECTED. 
   * 
   *----------------------------------------------------------------------- 
   * 
   * some modifs : 
   *   1/ to avoid the sort on the datas points use a dicho search to get 
   *      the interval LEFT 
   *   2/ all the datas points outside the interval definition of the spline 
   *      ([T(K),T(N+1)]) or with a non positive weight are not taken into acount 
   *      in the fit 
   * 
   * 
   *  Note: the breakpoints goes to T(K) until T(N+1)  (N+1-K+1 points) 
   *      T(K) is the first break point  (T(K) = X(1), ..., T(I) = X(I-K+1) 
   *      T(N+1) = T(L+K-1) = X(L)  is the last break point 
   * 
   */
  /* Parameter adjustments */
  --wgt;
  --gtau;
  --tau;
  --t;
  --wk;
  --a;
  q_dim1 = *k;
  q_offset = q_dim1 + 1;
  q -= q_offset;

  /* Function Body */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      a[j] = 0.;
      i__2 = *k;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  q[i__ + j * q_dim1] = 0.;
	}
    }
  /* 
   */
  ntau_count__ = 0;
  left = *k;
  i__1 = *ntau;
  for (l = 1; l <= i__1; ++l)
    {
      if (tau[l] >= t[*k] && tau[l] <= t[*n + 1] && wgt[l] > 0.)
	{
	  /*added by B 
	   */
	  ++ntau_count__;
	  /*          find the index left such that  T(LEFT) <= TAU(L) <= T(LEFT+1) (modified by Bruno) 
	   */
	  i__2 = *n - *k + 2;
	  left = nsp_calpack_isearch (&tau[l], &t[*k], &i__2) + 3;
	  jj = 0;
	  nsp_calpack_bspvb (&t[1], k, k, &jj, &tau[l], &left, &wk[1]);
	  leftmk = left - *k;
	  i__2 = *k;
	  for (mm = 1; mm <= i__2; ++mm)
	    {
	      dw = wk[mm] * wgt[l];
	      j = leftmk + mm;
	      a[j] = dw * gtau[l] + a[j];
	      i__ = 1;
	      i__3 = *k;
	      for (jj = mm; jj <= i__3; ++jj)
		{
		  q[i__ + j * q_dim1] = wk[jj] * dw + q[i__ + j * q_dim1];
		  ++i__;
		}
	    }
	}
    }
  if (ntau_count__ >= Max (2, *k))
    {
      /*       SOLVE THE NORMAL EQUATIONS 
       */
      nsp_calpack_bchfac (&q[q_offset], k, n, &wk[1], ierr);
      nsp_calpack_bchslv (&q[q_offset], k, n, &a[1]);
    }
  else
    {
      *ierr = -1;
    }
  return 0;
}				/* bslsq_ */

int nsp_calpack_bchfac (double *w, int *nb, int *n, double *diag, int *iflag)
{
  /* System generated locals */
  int w_dim1, w_offset, i__1, i__2, i__3;

  /* Local variables */
  int imax, jmax, i__, j, k;
  double t, ratio;
  int ipj, kpi;

  /*----------------------------------------------------------------------- 
   * FROM  * A PRACTICAL GUIDE TO SPLINES *  BY C. DE BOOR 
   * CONSTRUCTS CHOLESKY FACTORIZATION 
   *                    C  =  L * D * L-TRANSPOSE 
   * WITH L UNIT LOWER TRIANGULAR AND D DIAGONAL, FOR GIVEN MATRIX C OF 
   * ORDER  N , IN CASE  C  IS (SYMMETRIC) POSITIVE SEMIDEFINITE 
   * AND BANDED, HAVING NB DIAGONALS AT AND BELOW THE MAIN DIAGONAL. 
   * 
   *******  INPUT  ****** 
   * 
   *    N      THE ORDER OF THE MATRIX C. 
   * 
   *    NB     THE BANDWIDTH OF C, I.E., 
   *              C(I,J) = 0 FOR ABS(I-J) .GT. NB . 
   * 
   *    W      WORK ARRAY OF SIZE NB BY N CONTAINING THE NB DIAGONALS 
   *           IN ITS ROWS, WITH THE MAIN DIAGONAL IN ROW 1. PRECISELY, 
   *           W(I,J)  CONTAINS  C(I+J-1,J), I=1,...,NB, J=1,...,N. 
   *           FOR EXAMPLE, THE INTERESTING ENTRIES OF A SEVEN DIAGONAL 
   *           SYMMETRIC MATRIX C OF ORDER 9 WOULD BE STORED IN W AS 
   * 
   *                      11 22 33 44 55 66 77 88 99 
   *                      21 32 43 54 65 76 87 98 
   *                      31 42 53 64 75 86 97 
   *                      41 52 63 74 85 96 
   * 
   *           ALL OTHER ENTRIES OF W NOT IDENTIFIED WITH AN ENTRY OF C 
   *           ARE NEVER REFERENCED. 
   * 
   *    DIAG   WORK ARRAY OF LENGTH N. 
   * 
   *******  O U T P U T  ****** 
   *                                                        T 
   *    W      CONTAINS THE CHOLESKY FACTORIZATION C = L*D*L   WHERE 
   *           W(1,I) = 1/D(I,I) AND W(I,J) = L(I-1+J,J) (I=2,...,NB). 
   * 
   *    IFLAG  0 IF C IS NONSINGULAR AND 1 IF C IS SINGULAR. 
   * 
   *******  M E T H O D  ****** 
   * 
   *  GAUSS ELIMINATION, ADAPTED TO THE SYMMETRY AND BANDEDNESS OF  C , IS 
   *  USED . 
   *    NEAR ZERO PIVOTS ARE HANDLED IN A SPECIAL WAY. THE DIAGONAL ELE- 
   * MENT C(K,K) = W(1,K) IS SAVED INITIALLY IN  DIAG(K), ALL K. AT THE K- 
   * TH ELIMINATION STEP, THE CURRENT PIVOT ELEMENT, VIZ.  W(1,K), IS COM- 
   * PARED WITH ITS ORIGINAL VALUE, DIAG(K). IF, AS THE RESULT OF PRIOR 
   * ELIMINATION STEPS, THIS ELEMENT HAS BEEN REDUCED BY ABOUT A WORD 
   * LENGTH, (I.E., IF W(1,K)+DIAG(K) .LE. DIAG(K)), THEN THE PIVOT IS DE- 
   * CLARED TO BE ZERO, AND THE ENTIRE K-TH ROW IS DECLARED TO BE LINEARLY 
   * DEPENDENT ON THE PRECEDING ROWS. THIS HAS THE EFFECT OF PRODUCING 
   *  X(K) = 0  WHEN SOLVING  C*X = B  FOR  X, REGARDLESS OF  B. JUSTIFIC- 
   * ATION FOR THIS IS AS FOLLOWS. IN CONTEMPLATED APPLICATIONS OF THIS 
   * PROGRAM, THE GIVEN EQUATIONS ARE THE NORMAL EQUATIONS FOR SOME LEAST- 
   * SQUARES APPROXIMATION PROBLEM, DIAG(K) = C(K,K) GIVES THE NORM-SQUARE 
   * OF THE K-TH BASIS FUNCTION, AND, AT THIS POINT,  W(1,K)  CONTAINS THE 
   * NORM-SQUARE OF THE ERROR IN THE LEAST-SQUARES APPROXIMATION TO THE K- 
   * TH BASIS FUNCTION BY LINEAR COMBINATIONS OF THE FIRST K-1 . HAVING 
   * W(1,K)+DIAG(K) .LE. DIAG(K) SIGNIFIES THAT THE K-TH FUNCTION IS LIN- 
   * EARLY DEPENDENT TO MACHINE ACCURACY ON THE FIRST K-1 FUNCTIONS, THERE 
   * FORE CAN SAFELY BE LEFT OUT FROM THE BASIS OF APPROXIMATING FUNCTIONS 
   *    THE SOLUTION OF A LINEAR SYSTEM 
   *                      C*X = B 
   *  IS EFFECTED BY THE SUCCESSION OF THE FOLLOWING  T W O  CALLS ... 
   *    CALL BCHFAC (W, NB, N, DIAG, IFLAG)   , TO GET FACTORIZATION 
   *    CALL BCHSLV (W, NB, N, B, X )            , TO SOLVE FOR X. 
   *----------------------------------------------------------------------- 
   * 
   */
  /* Parameter adjustments */
  --diag;
  w_dim1 = *nb;
  w_offset = w_dim1 + 1;
  w -= w_offset;

  /* Function Body */
  if (*n > 1)
    {
      goto L10;
    }
  *iflag = 1;
  if (w[w_dim1 + 1] == 0.)
    {
      return 0;
    }
  *iflag = 0;
  w[w_dim1 + 1] = 1. / w[w_dim1 + 1];
  return 0;
  /* 
   *    STORE THE DIAGONAL OF C IN DIAG 
   * 
   */
L10:
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      diag[k] = w[k * w_dim1 + 1];
      /* L11: */
    }
  /* 
   *    FACTORIZATION 
   * 
   */
  *iflag = 0;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      t = w[k * w_dim1 + 1] + diag[k];
      if (t != diag[k])
	{
	  goto L30;
	}
      *iflag = 1;
      i__2 = *nb;
      for (j = 1; j <= i__2; ++j)
	{
	  w[j + k * w_dim1] = 0.;
	  /* L20: */
	}
      goto L60;
      /* 
       */
    L30:
      t = 1. / w[k * w_dim1 + 1];
      w[k * w_dim1 + 1] = t;
      /*Computing MIN 
       */
      i__2 = *nb - 1, i__3 = *n - k;
      imax = Min (i__2, i__3);
      if (imax < 1)
	{
	  goto L60;
	}
      jmax = imax;
      i__2 = imax;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  ratio = t * w[i__ + 1 + k * w_dim1];
	  kpi = k + i__;
	  i__3 = jmax;
	  for (j = 1; j <= i__3; ++j)
	    {
	      ipj = i__ + j;
	      w[j + kpi * w_dim1] -= w[ipj + k * w_dim1] * ratio;
	      /* L40: */
	    }
	  --jmax;
	  w[i__ + 1 + k * w_dim1] = ratio;
	  /* L50: */
	}
    L60:
      ;
    }
  return 0;
}				/* bchfac_ */

int nsp_calpack_bchslv (double *w, int *nb, int *n, double *b)
{
  /* System generated locals */
  int w_dim1, w_offset, i__1, i__2, i__3;

  /* Local variables */
  int jmax, j, k, jpk, nbm1;

  /*----------------------------------------------------------------------- 
   * 
   *    BCHSLV SOLVES THE LINEAR SYSTEM C*X = B FOR X WHEN W CONTAINS 
   *    THE CHOLESKY FACTORIZATION OBTAINED BY THE SUBROUTINE BCHFAC 
   *    FOR THE BANDED SYMMETRIC POSITIVE DEFINITE MATRIX C. 
   * 
   *    INPUT ... 
   * 
   *       N   THE ORDER OF THE MATRIX C 
   *       NB  THE BANDWIDTH OF C 
   *       W   THE CHOLESKY FACTORIZATION OF C 
   *       B   VECTOR OF LENGTH N CONTAINING THE RIGHT SIDE 
   * 
   *    OUTPUT ... 
   * 
   *       B   SOLUTION X OF THE LINEAR SYSTEM C*X = B 
   * 
   *                                      T 
   *    NOTE.  THE FACTORIZATION C = L*D*L  IS USED, WHERE L IS A 
   *    UNIT LOWER TRIANGULAR MATRIX AND D A DIAGONAL MATRIX. 
   * 
   *----------------------------------------------------------------------- 
   * 
   */
  /* Parameter adjustments */
  --b;
  w_dim1 = *nb;
  w_offset = w_dim1 + 1;
  w -= w_offset;

  /* Function Body */
  if (*n > 1)
    {
      goto L10;
    }
  b[1] *= w[w_dim1 + 1];
  return 0;
  /* 
   *    FORWARD SUBSTITUTION. SOLVE L*Y = B FOR Y AND STORE Y IN B. 
   * 
   */
L10:
  nbm1 = *nb - 1;
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      /*Computing MIN 
       */
      i__2 = nbm1, i__3 = *n - k;
      jmax = Min (i__2, i__3);
      if (jmax < 1)
	{
	  goto L30;
	}
      i__2 = jmax;
      for (j = 1; j <= i__2; ++j)
	{
	  jpk = j + k;
	  b[jpk] -= w[j + 1 + k * w_dim1] * b[k];
	  /* L20: */
	}
    L30:
      ;
    }
  /*                             T     -1 
   *    BACKSUBSTITUTION. SOLVE L X = D  Y  FOR X AND STORE X IN B. 
   * 
   */
  k = *n;
L40:
  b[k] *= w[k * w_dim1 + 1];
  /*Computing MIN 
   */
  i__1 = nbm1, i__2 = *n - k;
  jmax = Min (i__1, i__2);
  if (jmax < 1)
    {
      goto L60;
    }
  i__1 = jmax;
  for (j = 1; j <= i__1; ++j)
    {
      jpk = j + k;
      b[k] -= w[j + 1 + k * w_dim1] * b[jpk];
      /* L50: */
    }
L60:
  --k;
  if (k > 0)
    {
      goto L40;
    }
  return 0;
}				/* bchslv_ */

int
nsp_calpack_bspvb (double *t, int *k, int *jhigh, int *j, double *x,
		   int *left, double *blist)
{
  /* System generated locals */
  int i__1;

  /* Local variables */
  double timj, term;
  int i__, l;
  double s, ti;
  int imj;

  /*----------------------------------------------------------------------- 
   * 
   *    BSPVB CALCULATES THE VALUE OF ALL POSSIBLY NONZERO B-SPLINES 
   *    AT X OF ORDER MAX(JHIGH,J + 1) WHERE T(K) .LE. X .LT. T(N+1). 
   * 
   *    DESCRIPTION OF ARGUMENTS 
   * 
   *        INPUT 
   * 
   *         T       - KNOT VECTOR OF LENGTH N + K. 
   *         K       - HIGHEST POSSIBLE ORDER OF THE B-SPLINES. 
   *         JHIGH   - ORDER OF B-SPLINES (1 .LE. JHIGH .LE. K). 
   *         J       - J .LE. 0  GIVES B-SPLINES OF ORDER JHIGH. 
   *                   J .GE. 1  ON A PREVIOUS CALL TO BSPVB THE 
   *                             B-SPLINES OF ORDER J WERE COM- 
   *                             PUTED AND STORED IN BLIST. IT IS 
   *                             ASSUMED THAT WORK HAS NOT BEEN 
   *                             MODIFIED AND THAT J .LT. K. 
   *         X       - ARGUMENT OF THE B-SPLINES. 
   *         LEFT    - LARGEST INT SUCH THAT 
   *                   T(LEFT) .LE. X .LT. T(LEFT+1) 
   * 
   *        OUTPUT 
   * 
   *         BLIST   - VECTOR OF LENGTH K FOR SPLINE VALUES. 
   *         J       - B-SPLINES OF ORDER J HAVE BEEN COMPUTED 
   *                   AND STORED IN BLIST. 
   * 
   *----------------------------------------------------------------------- 
   *    WRITTEN BY CARL DE BOOR (UNIVERSITY OF WISCONSIN) AND MODIFIED 
   *        BY A.H. MORRIS (NSWC). 
   *----------------------------------------------------------------------- 
   * 
   */
  /* Parameter adjustments */
  --t;
  --blist;

  /* Function Body */
  if (*j > 0)
    {
      goto L10;
    }
  *j = 1;
  blist[1] = 1.;
  if (*j >= *jhigh)
    {
      return 0;
    }
  /* 
   */
L10:
  s = 0.;
  i__1 = *j;
  for (l = 1; l <= i__1; ++l)
    {
      i__ = *left + l;
      imj = i__ - *j;
      timj = t[imj];
      ti = t[i__];
      term = blist[l] / (ti - timj);
      blist[l] = s + (ti - *x) * term;
      s = (*x - timj) * term;
      /* L20: */
    }
  ++(*j);
  blist[*j] = s;
  if (*j < *jhigh)
    {
      goto L10;
    }
  /* 
   */
  return 0;
}				/* bspvb_ */

int
nsp_calpack_bspp (double *t, double *a, int *n, int *k, double *break__,
		  double *c__, int *l, double *wk)
{
  /* System generated locals */
  int c_dim1, c_offset, wk_dim1, wk_offset, i__1, i__2, i__3;

  /* Local variables */
  double diff;
  int ilkj, left;
  double term;
  int i__, j;
  double r__, s, x;
  int jj, il, km1, jp1, kp1, ilj, kmj;

  /*----------------------------------------------------------------------- 
   * 
   *             CONVERSION FROM B-SPLINE REPRESENTATION 
   *             TO PIECEWISE POLYNOMIAL REPRESENTATION 
   * 
   * 
   *    INPUT ... 
   * 
   *      T     KNOT SEQUENCE OF LENGTH N+K 
   *      A     B-SPLINE COEFFICIENT SEQUENCE OF LENGTH N 
   *      N     LENGTH OF A 
   *      K     ORDER OF THE B-SPLINES 
   * 
   *    OUTPUT ... 
   * 
   *      BREAK BREAKPOINT SEQUENCE, OF LENGTH L+1, CONTAINING 
   *            (IN INCREASING ORDER) THE DISTINCT POINTS OF THE 
   *            SEQUENCE T(K),...,T(N+1). 
   *      C     KXL MATRIX WHERE C(I,J) = (I-1)ST RIGHT DERIVATIVE 
   *            OF THE PP AT BREAK(J) DIVIDED BY FACTORIAL(I-1). 
   *      L     NUMBER OF POLYNOMIALS WHICH FORM THE PP 
   * 
   *    WORK AREA ... 
   * 
   *      WK    2-DIMENSIONAL ARRAY OF DIMENSION (K,K+1) 
   * 
   *----------------------------------------------------------------------- 
   * 
   */
  /* Parameter adjustments */
  --t;
  --a;
  wk_dim1 = *k;
  wk_offset = wk_dim1 + 1;
  wk -= wk_offset;
  c_dim1 = *k;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  --break__;

  /* Function Body */
  *l = 0;
  break__[1] = t[*k];
  if (*k == 1)
    {
      goto L100;
    }
  km1 = *k - 1;
  kp1 = *k + 1;
  /* 
   *         GENERAL K-TH ORDER CASE 
   * 
   */
  i__1 = *n;
  for (left = *k; left <= i__1; ++left)
    {
      if (t[left] == t[left + 1])
	{
	  goto L60;
	}
      ++(*l);
      break__[*l + 1] = t[left + 1];
      i__2 = *k;
      for (j = 1; j <= i__2; ++j)
	{
	  jj = left - *k + j;
	  wk[j + wk_dim1] = a[jj];
	  /* L10: */
	}
      /* 
       */
      i__2 = km1;
      for (j = 1; j <= i__2; ++j)
	{
	  jp1 = j + 1;
	  kmj = *k - j;
	  i__3 = kmj;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      il = i__ + left;
	      ilkj = il - kmj;
	      diff = t[il] - t[ilkj];
	      wk[i__ + jp1 * wk_dim1] =
		(wk[i__ + 1 + j * wk_dim1] - wk[i__ + j * wk_dim1]) / diff;
	      /* L20: */
	    }
	  /* L21: */
	}
      /* 
       */
      wk[kp1 * wk_dim1 + 1] = 1.;
      x = t[left];
      c__[*k + *l * c_dim1] = wk[*k * wk_dim1 + 1];
      r__ = 1.;
      i__2 = km1;
      for (j = 1; j <= i__2; ++j)
	{
	  jp1 = j + 1;
	  s = 0.;
	  i__3 = j;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      il = i__ + left;
	      ilj = il - j;
	      term = wk[i__ + kp1 * wk_dim1] / (t[il] - t[ilj]);
	      wk[i__ + kp1 * wk_dim1] = s + (t[il] - x) * term;
	      s = (x - t[ilj]) * term;
	      /* L30: */
	    }
	  wk[jp1 + kp1 * wk_dim1] = s;
	  /* 
	   */
	  s = 0.;
	  kmj = *k - j;
	  i__3 = jp1;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      s += wk[i__ + kmj * wk_dim1] * wk[i__ + kp1 * wk_dim1];
	      /* L40: */
	    }
	  r__ = r__ * (double) kmj / (double) j;
	  c__[kmj + *l * c_dim1] = r__ * s;
	  /* L50: */
	}
    L60:
      ;
    }
  return 0;
  /* 
   *         PIECEWISE CONSTANT CASE 
   * 
   */
L100:
  i__1 = *n;
  for (left = *k; left <= i__1; ++left)
    {
      if (t[left] == t[left + 1])
	{
	  goto L110;
	}
      ++(*l);
      break__[*l + 1] = t[left + 1];
      c__[*l * c_dim1 + 1] = a[left];
    L110:
      ;
    }
  return 0;
}				/* bspp_ */
#endif
