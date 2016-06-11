/* sszer.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

static int
nsp_ctrlpack_house (double *wrk2, int *k, int *j, double *heps, int *zero, double *s);

static int
nsp_ctrlpack_pivot (const double *vec, double *vmax, int *ibar,const int *i1, const int *i2);

static int
nsp_ctrlpack_preduc (double *abf,const int *naf, const int *mplusn, const int *m,const int *n,
		     const int *p, double *heps, int *iro, int *isigma, int *mu,
		     int *nu, double *wrk1,const int *nwrk1, double *wrk2,
		     const int *nwrk2);

static int
nsp_ctrlpack_tr2 (double *a, const int *na,const int *n, double *u, double *s,const int *i1,
		  const int *i2,const int *j1,const  int *j2);

static int
nsp_ctrlpack_tr1 (double *a,const int *na,const int *n, double *u, double *s,const int *i1,
		  const int *i2, const int *j1,const int *j2);

/* Table of constant values */

static int c__1 = 1;
static int c__0 = 0;

/* 
 *  computes the invariant zeros of a linear multivariable 
 *  system given in state space form. 
 * 
 *!method 
 * 
 *       this routine extracts from the system matrix of a state-space 
 *       system  a,b,c,d  a regular pencil   lambda * bf  -  af 
 *       which has the invariant zeros of the system as generalized 
 *       eigenvalues. 
 * 
 *!reference 
 * 
 *       emami-naeini, a. and van dooren, p. 
 *       'computation of zeros of linear multivariable systems' 
 *       report na-80-03, computer science department, stanford univ. 
 * 
 *!originator 
 * 
 *    a.emami-naeini, computer science department, stanford university. 
 *    Copyrigth SLICE 
 * 
 *       double precision a(na,n),b(na,m),c(nc,n),d(nc,m),wrka(na,n) 
 *       double precision af(naf,mplusn),bf(naf,mplusn) 
 *       double precision wrk1(nwrk1),wrk2(nwrk2) 
 *       double precision zeror(n),zeroi(n) 
 * 
 *arguments in 
 *       n       int 
 *               -the number of state variables in the system 
 *       m       int 
 *               -the number of inputs to the system 
 *       p       int 
 *               -the number of outputs from the system 
 *       a       double precision (n,n) 
 *               -the state dynamics matrix of the system 
 *       na      int 
 *               -the declared first dimension of matrices a and b 
 *       b       double precision (n,m) 
 *               -the  input/state  matrix of the system 
 *       c       double precision (p,n) 
 *               -the  state/output  matrix of the system 
 *       nc      int 
 *               -the declared first dimension of matrices  c and d 
 *       d       double precision (p,m) 
 *               -the  input/output  matrix of the system 
 *       naf     int 
 *               -the declared first dimension of matrices  af and bf 
 *                naf must be at least  n + p 
 *       mplusn  int 
 *               -the second dimension of  af and bf.  mplusn  must be 
 *               at least  m + n . 
 *       nwrk1   int 
 *               -the length of work vector wrk1. 
 *               nwrk1  must be at least  Max(m,p) 
 *       nwrk2   int 
 *               -the length of work vector  wrk2. 
 *               nwrk2  must be at least  Max(n,m,p)+1 
 *arguments out 
 *       nu      int 
 *               -the number of (finite) invariant zeros 
 *       irank   int 
 *               -the normal rank of the transfer function 
 *       zeror   double precision (n) 
 *       zeroi   double precision (n) 
 *               -the real  and imaginary parts of the zeros 
 *       af      double precision ( n+p , m+n ) 
 *       bf      double precision ( n+p , m+n ) 
 *               -the coefficient matrices of the reduced pencil 
 *       ierr    int 
 *               -error indicator 
 *               ierr = 0        successful return 
 *               ierr = 1        incorrect dimensions of matrices 
 *               ierr = 2        attempt to divide by zero 
 *               ierr = i > 2    ierr value i-2 from qitz (eispack) 
 * 
 *!working space 
 *       wrka    double precision (na,n) 
 *       wrk1    double precision (nwrk1) 
 *       wrk2    double precision (nwrk2) 
 */

int
nsp_ctrlpack_sszer (const int *n,const int *m,const int *p, double *a,const int *na, double *b,
		    double *c__, const int *nc, double *d__, const double *deps,
		    double *zeror, double *zeroi, int *nu, int *irank,
		    double *af, const int *naf, double *bf, const int *mplusn,
		    double *wrka, double *wrk1, const int *nwrk1, double *wrk2,
		    const int *nwrk2, int *ierr)
{
  double eps = *deps;
  
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, d_dim1, d_offset,
    wrka_dim1, wrka_offset, af_dim1, af_offset, bf_dim1, bf_offset, i__1,
    i__2;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  double heps;
  int matq, matz, zero;
  int numu;
  int numu1, i__, j;
  double s;
  int j1;
  int mj, ni, mm, nn, pp, mu, isigma;
  int nu1;
  int iro, mnu;
  double sum, xxx[1] /* was [1][1] */ ;
  int mnu1;

  /* Parameter adjustments */
  --zeroi;
  --zeror;
  wrka_dim1 = *na;
  wrka_offset = wrka_dim1 + 1;
  wrka -= wrka_offset;
  b_dim1 = *na;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  d_dim1 = *nc;
  d_offset = d_dim1 + 1;
  d__ -= d_offset;
  c_dim1 = *nc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  bf_dim1 = *naf;
  bf_offset = bf_dim1 + 1;
  bf -= bf_offset;
  af_dim1 = *naf;
  af_offset = af_dim1 + 1;
  af -= af_offset;
  --wrk1;
  --wrk2;

  /* Function Body */
  *ierr = 1;
  if (*na < *n)
    {
      return 0;
    }
  if (*nc < *p)
    {
      return 0;
    }
  if (*naf < *n + *p)
    {
      return 0;
    }
  if (*nwrk1 < *m)
    {
      return 0;
    }
  if (*nwrk1 < *p)
    {
      return 0;
    }
  if (*nwrk2 < *n)
    {
      return 0;
    }
  if (*nwrk2 < *m)
    {
      return 0;
    }
  if (*nwrk2 < *p)
    {
      return 0;
    }
  if (*mplusn < *m + *n)
    {
      return 0;
    }
  *ierr = 0;
  /*      construct the compound matrix (b      a) of dimension 
   *                                    (d      c) 
   *      (n + p) * (m + n) 
   * 
   */
  sum = 0.;
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  bf[i__ + j * bf_dim1] = b[i__ + j * b_dim1];
	  sum += b[i__ + j * b_dim1] * b[i__ + j * b_dim1];
	  /* L10: */
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  mj = *m + j;
	  bf[i__ + mj * bf_dim1] = a[i__ + j * a_dim1];
	  sum += a[i__ + j * a_dim1] * a[i__ + j * a_dim1];
	  /* L30: */
	}
    }
  /* 
   */
  i__2 = *p;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      ni = *n + i__;
      i__1 = *m;
      for (j = 1; j <= i__1; ++j)
	{
	  bf[ni + j * bf_dim1] = d__[i__ + j * d_dim1];
	  sum += d__[i__ + j * d_dim1] * d__[i__ + j * d_dim1];
	  /* L40: */
	}
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  mj = *m + j;
	  bf[ni + mj * bf_dim1] = c__[i__ + j * c_dim1];
	  sum += c__[i__ + j * c_dim1] * c__[i__ + j * c_dim1];
	  /* L60: */
	}
    }
  /* 
   */
  heps = eps * sqrt (sum);
  /* 
   *      reduce this system to one with the same invariant zeros and with 
   *      d full row rank mu (the normal rank of the original system) 
   * 
   */
  iro = *p;
  isigma = 0;
  /* 
   */
  nsp_ctrlpack_preduc (&bf[bf_offset], naf, mplusn, m, n, p, &heps, &iro,
		       &isigma, &mu, nu, &wrk1[1], nwrk1, &wrk2[1], nwrk2);
  /* 
   */
  *irank = mu;
  if (*nu == 0)
    {
      return 0;
    }
  /* 
   *      pertranspose the system 
   * 
   */
  numu = *nu + mu;
  mnu = *m + *nu;
  numu1 = numu + 1;
  mnu1 = mnu + 1;
  i__1 = numu;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ni = numu1 - i__;
      i__2 = mnu;
      for (j = 1; j <= i__2; ++j)
	{
	  mj = mnu1 - j;
	  af[mj + ni * af_dim1] = bf[i__ + j * bf_dim1];
	  /* L70: */
	}
    }
  /* 
   */
  mm = *m;
  nn = *n;
  pp = *p;
  if (mu == mm)
    {
      goto L80;
    }
  pp = mm;
  nn = *nu;
  mm = mu;
  /* 
   *      reduce the system to one with the same invariant zeros and with 
   *      d square and of full rank 
   * 
   */
  iro = pp - mm;
  isigma = mm;
  /* 
   */
  nsp_ctrlpack_preduc (&af[af_offset], naf, mplusn, &mm, &nn, &pp, &heps,
		       &iro, &isigma, &mu, nu, &wrk1[1], nwrk1, &wrk2[1],
		       nwrk2);
  /* 
   */
  if (*nu == 0)
    {
      return 0;
    }
  mnu = mm + *nu;
 L80:
  i__2 = *nu;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      ni = mm + i__;
      i__1 = mnu;
      for (j = 1; j <= i__1; ++j)
	{
	  bf[i__ + j * bf_dim1] = 0.;
	  /* L90: */
	}
      bf[i__ + ni * bf_dim1] = 1.;
      /* L100: */
    }
  /* 
   */
  if (*irank == 0)
    {
      return 0;
    }
  nu1 = *nu + 1;
  numu = *nu + mu;
  j1 = mm;
  i__2 = mm;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      --j1;
      i__1 = nu1;
      for (j = 1; j <= i__1; ++j)
	{
	  mj = j1 + j;
	  wrk2[j] = af[numu + mj * af_dim1];
	  /* L110: */
	}
      /* 
       */
      nsp_ctrlpack_house (&wrk2[1], &nu1, &nu1, &heps, &zero, &s);
      nsp_ctrlpack_tr2 (&af[af_offset], naf, mplusn, &wrk2[1], &s, &c__1,
			&numu, &j1, &nu1);
      nsp_ctrlpack_tr2 (&bf[bf_offset], naf, mplusn, &wrk2[1], &s, &c__1, nu,
			&j1, &nu1);
      /* 
       */
      --numu;
      /* L120: */
    }
  matz = FALSE;
  matq = FALSE;
  /*c 
   */
  nsp_ctrlpack_qhesz (naf, nu, &af[af_offset], &bf[bf_offset], &matq, xxx,
		      &matz, &wrka[wrka_offset]);
  nsp_ctrlpack_qitz (naf, nu, &af[af_offset], &bf[bf_offset], &eps, &matq, xxx,
		     &matz, &wrka[wrka_offset], ierr);
  if (*ierr != 0)
    {
      goto L150;
    }
  /*c 
   */
  nsp_ctrlpack_qvalz (naf, nu, &af[af_offset], &bf[bf_offset], &eps, &zeror[1],
		      &zeroi[1], &wrk2[1], &matq, xxx, &matz,
		      &wrka[wrka_offset]);
  /*c 
   *        do 130 i = 1,nu 
   *           if (wrk2(i) .eq. 0.0d+0) go to 140 
   *           zeror(i) = zeror(i)/wrk2(i) 
   *           zeroi(i) = zeroi(i)/wrk2(i) 
   * 130       continue 
   *c 
   *c       successful completion 
   *c 
   */
  *ierr = 0;
  return 0;
  /*c 
   *c       attempt to divide by zero 
   *c 
   * 140    ierr = 2 
   *        return 
   *c 
   *c       failure in subroutine qzit 
   *c 
   */
 L150:
  *ierr += 2;
  return 0;
}				/* sszer_ */

static int
nsp_ctrlpack_preduc (double *abf,const int *naf, const int *mplusn, const int *m,const int *n,
		     const int *p, double *heps, int *iro, int *isigma, int *mu,
		     int *nu, double *wrk1,const int *nwrk1, double *wrk2,
		     const int *nwrk2)
{
  /* System generated locals */
  int abf_dim1, abf_offset, i__1, i__2;

  /* Local variables */
  int ibar, icol, itau;
  double temp;
  int zero;
  int irow, numu, i__, j;
  double s;
  int i1, i2, m1, n1;
  int mm1, mn1;
  int irj, mnu;
  double sum;
  int iro1;

  /*%calling sequence 
   *      subroutine preduc(abf,naf,mplusn,m,n,p,heps,iro,isigma,mu,nu, 
   *   1                    wrk1,nwrk1,wrk2,nwrk2) 
   *      int naf,mplusn,m,n,p,iro,isigma,mu,nu,nwrk1,nwrk2 
   *      double precision abf(naf,mplusn),wrk1(nwrk1),wrk2(nwrk2) 
   * 
   *%purpose 
   * 
   *    this routine is only to be called from slice routine sszer 
   *% 
   * 
   * 
   *      local variables: 
   * 
   * 
   * 
   * 
   * 
   * 
   */
  /* Parameter adjustments */
  abf_dim1 = *naf;
  abf_offset = abf_dim1 + 1;
  abf -= abf_offset;
  --wrk1;
  --wrk2;

  /* Function Body */
  *mu = *p;
  *nu = *n;
 L10:
  if (*mu == 0)
    {
      return 0;
    }
  iro1 = *iro;
  mnu = *m + *nu;
  numu = *nu + *mu;
  if (*m == 0)
    {
      goto L120;
    }
  ++iro1;
  irow = *nu;
  if (*isigma <= 1)
    {
      goto L40;
    }
  /* 
   *      compress rows of d: first exploit triangular shape 
   * 
   */
  m1 = *isigma - 1;
  i__1 = m1;
  for (icol = 1; icol <= i__1; ++icol)
    {
      i__2 = iro1;
      for (j = 1; j <= i__2; ++j)
	{
	  irj = irow + j;
	  wrk2[j] = abf[irj + icol * abf_dim1];
	  /* L20: */
	}
      /* 
       */
      nsp_ctrlpack_house (&wrk2[1], &iro1, &c__1, heps, &zero, &s);
      /* 
       */
      nsp_ctrlpack_tr1 (&abf[abf_offset], naf, mplusn, &wrk2[1], &s, &irow,
			&iro1, &icol, &mnu);
      /* 
       */
      ++irow;
      /* L30: */
    }
  /* 
   *      continue with householder transformation with pivoting 
   * 
   */
 L40:
  if (*isigma != 0)
    {
      goto L45;
    }
  *isigma = 1;
  --iro1;
 L45:
  if (*isigma == *m)
    {
      goto L60;
    }
  i__1 = *m;
  for (icol = *isigma; icol <= i__1; ++icol)
    {
      sum = 0.;
      i__2 = iro1;
      for (j = 1; j <= i__2; ++j)
	{
	  irj = irow + j;
	  sum += abf[irj + icol * abf_dim1] * abf[irj + icol * abf_dim1];
	  /* L50: */
	}
      wrk1[icol] = sum;
      /* L55: */
    }
  /* 
   */
 L60:
  i__1 = *m;
  for (icol = *isigma; icol <= i__1; ++icol)
    {
      /* 
       *         pivot if necessary 
       * 
       */
      if (icol == *m)
	{
	  goto L80;
	}
      /* 
       */
      nsp_ctrlpack_pivot (&wrk1[1], &temp, &ibar, &icol, m);
      /* 
       */
      if (ibar == icol)
	{
	  goto L80;
	}
      wrk1[ibar] = wrk1[icol];
      wrk1[icol] = temp;
      i__2 = numu;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  temp = abf[i__ + icol * abf_dim1];
	  abf[i__ + icol * abf_dim1] = abf[i__ + ibar * abf_dim1];
	  /* L70: */
	  abf[i__ + ibar * abf_dim1] = temp;
	}
      /* 
       *         perform householder transformation 
       * 
       */
    L80:
      i__2 = iro1;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  irj = irow + i__;
	  /* L90: */
	  wrk2[i__] = abf[irj + icol * abf_dim1];
	}
      /* 
       */
      nsp_ctrlpack_house (&wrk2[1], &iro1, &c__1, heps, &zero, &s);
      /* 
       */
      if (zero)
	{
	  goto L120;
	}
      if (iro1 == 1)
	{
	  return 0;
	}
      /* 
       */
      nsp_ctrlpack_tr1 (&abf[abf_offset], naf, mplusn, &wrk2[1], &s, &irow,
			&iro1, &icol, &mnu);
      /* 
       */
      ++irow;
      --iro1;
      i__2 = *m;
      for (j = icol; j <= i__2; ++j)
	{
	  /* L100: */
	  wrk1[j] -= abf[irow + j * abf_dim1] * abf[irow + j * abf_dim1];
	}
    }
  /* 
   */
 L120:
  itau = iro1;
  *isigma = *mu - itau;
  /* 
   *      compress the columns of c 
   * 
   */
  i1 = *nu + *isigma;
  mm1 = *m + 1;
  n1 = *nu;
  if (itau == 1)
    {
      goto L140;
    }
  i__2 = itau;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      irj = i1 + i__;
      sum = 0.;
      i__1 = mnu;
      for (j = mm1; j <= i__1; ++j)
	{
	  /* L130: */
	  sum += abf[irj + j * abf_dim1] * abf[irj + j * abf_dim1];
	}
      /* L135: */
      wrk1[i__] = sum;
    }
  /* 
   */
 L140:
  i__2 = itau;
  for (iro1 = 1; iro1 <= i__2; ++iro1)
    {
      *iro = iro1 - 1;
      i__ = itau - *iro;
      i2 = i__ + i1;
      /* 
       *         pivot if necessary 
       * 
       */
      if (i__ == 1)
	{
	  goto L160;
	}
      /* 
       */
      nsp_ctrlpack_pivot (&wrk1[1], &temp, &ibar, &c__1, &i__);
      /* 
       */
      if (ibar == i__)
	{
	  goto L160;
	}
      wrk1[ibar] = wrk1[i__];
      wrk1[i__] = temp;
      irj = ibar + i1;
      i__1 = mnu;
      for (j = mm1; j <= i__1; ++j)
	{
	  temp = abf[i2 + j * abf_dim1];
	  abf[i2 + j * abf_dim1] = abf[irj + j * abf_dim1];
	  /* L150: */
	  abf[irj + j * abf_dim1] = temp;
	}
      /* 
       *         perform householder transformation 
       * 
       */
    L160:
      i__1 = n1;
      for (j = 1; j <= i__1; ++j)
	{
	  irj = *m + j;
	  /* L170: */
	  wrk2[j] = abf[i2 + irj * abf_dim1];
	}
      /* 
       */
      nsp_ctrlpack_house (&wrk2[1], &n1, &n1, heps, &zero, &s);
      /* 
       */
      if (zero)
	{
	  goto L210;
	}
      if (n1 == 1)
	{
	  goto L220;
	}
      /* 
       */
      nsp_ctrlpack_tr2 (&abf[abf_offset], naf, mplusn, &wrk2[1], &s, &c__1,
			&i2, m, &n1);
      /* 
       */
      mn1 = *m + n1;
      /* 
       */
      nsp_ctrlpack_tr1 (&abf[abf_offset], naf, mplusn, &wrk2[1], &s, &c__0,
			&n1, &c__1, &mn1);
      /* 
       */
      i__1 = i__;
      for (j = 1; j <= i__1; ++j)
	{
	  irj = i1 + j;
	  /* L190: */
	  wrk1[j] -= abf[irj + mn1 * abf_dim1] * abf[irj + mn1 * abf_dim1];
	}
      --mnu;
      /* L200: */
      --n1;
    }
  /* 
   */
  *iro = itau;
 L210:
  *nu -= *iro;
  *mu = *isigma + *iro;
  if (*iro == 0)
    {
      return 0;
    }
  goto L10;
  /* 
   */
 L220:
  *mu = *isigma;
  *nu = 0;
  /* 
   */
  return 0;
}				/* preduc_ */

/* 
 * warning - this routine is only to be called from slice routine 
 *           sszer 
 * 
 *% purpose 
 *      this routine constructs a householder transformation h = i-s.uu 
 *      that 'mirrors' a vector wrk2(1,...,k) to the j-th unit vector. 
 *      if norm(wrk2) < heps, zero is put equal to .true. 
 *      upon return, u is stored in wrk2 
 * 
 */

static int
nsp_ctrlpack_house (double *wrk2, int *k, int *j, double *heps, int *zero, double *s)
{
  /* System generated locals */
  int i__1;
  /* Local variables */
  double alfa;
  int i__;
  double sum, dum1;

  /* Parameter adjustments */
  --wrk2;

  /* Function Body */
  *zero = TRUE;
  sum = 0.;
  i__1 = *k;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L10: */
      sum += wrk2[i__] * wrk2[i__];
    }
  /* 
   */
  alfa = sqrt (sum);
  if (alfa <= *heps)
    {
      return 0;
    }
  /* 
   */
  *zero = FALSE;
  dum1 = wrk2[*j];
  if (dum1 > 0.)
    {
      alfa = -alfa;
    }
  wrk2[*j] = dum1 - alfa;
  *s = 1. / (sum - alfa * dum1);
  /* 
   */
  return 0;
}

static int
nsp_ctrlpack_tr1 (double *a,const int *na,const int *n, double *u, double *s,const int *i1,
		  const int *i2, const int *j1,const int *j2)
{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;

  /* Local variables */
  int i__, j;
  double y;
  int irj;
  double sum;

  /*% calling sequence 
   * 
   *       subroutine tr1(a,na,n,u,s,i1,i2,j1,j2) 
   * 
   *%purpose 
   * 
   *      this subroutine performs the householder transformation 
   *                      h = i - s.uu 
   *      on the rows i1 + 1 to i1 + i2 of a, this from columns j1 to j2. 
   *% comments 
   * 
   * warning - this routine is only to be called from slice routine 
   *           sszer 
   * 
   *% 
   * 
   * 
   *      local variables: 
   * 
   * 
   * 
   * 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --u;

  /* Function Body */
  i__1 = *j2;
  for (j = *j1; j <= i__1; ++j)
    {
      sum = 0.;
      i__2 = *i2;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  irj = *i1 + i__;
	  /* L10: */
	  sum += u[i__] * a[irj + j * a_dim1];
	}
      /* 
       */
      y = sum * *s;
      /* 
       */
      i__2 = *i2;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  irj = *i1 + i__;
	  /* L20: */
	  a[irj + j * a_dim1] -= u[i__] * y;
	}
    }
  /* 
   */
  return 0;
}				/* tr1_ */

static int
nsp_ctrlpack_tr2 (double *a, const int *na,const int *n, double *u, double *s,const int *i1,
		  const int *i2,const int *j1,const  int *j2)

{
  /* System generated locals */
  int a_dim1, a_offset, i__1, i__2;

  /* Local variables */
  int i__, j;
  double y;
  int irj;
  double sum;

  /*% calling sequence 
   * 
   *       subroutine tr2(a,na,n,u,s,i1,i2,j1,j2) 
   *%purpose 
   * 
   *      this routine performs the householder transformation h = i-s.uu 
   *      on the columns j1 + 1 to j1 + j2 of a, this from rows i1 to i2. 
   * 
   */
  /* Parameter adjustments */
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --u;

  /* Function Body */
  i__1 = *i2;
  for (i__ = *i1; i__ <= i__1; ++i__)
    {
      sum = 0.;
      i__2 = *j2;
      for (j = 1; j <= i__2; ++j)
	{
	  irj = *j1 + j;
	  /* L10: */
	  sum += u[j] * a[i__ + irj * a_dim1];
	}
      /* 
       */
      y = sum * *s;
      /* 
       */
      i__2 = *j2;
      for (j = 1; j <= i__2; ++j)
	{
	  irj = *j1 + j;
	  /* L20: */
	  a[i__ + irj * a_dim1] -= u[j] * y;
	}
    }
  /* 
   */
  return 0;
}				/* tr2_ */


/*% calling sequence 
 *   nsp_ctrlpack_pivot (const double *vec, double *vmax, int *ibar,const int *i1, const int *i2)
 *   double vec(i2) 
 * 
 *% purpose 
 * 
 *      this subroutine computes the maximal norm element in vmax
 *      of the vector vec(i1,...,i2), and its location ibar 
 */

static int nsp_ctrlpack_pivot (const double *vec, double *vmax, int *ibar,const int *i1, const int *i2)
{
  double d1;
  int i;
  
  --vec;

  *ibar = *i1;
  *vmax = vec[*i1];
  if (*i1 >= *i2)
    {
      goto L20;
    }
  
  for (i = (*i1 + 1) ; i <= *i2 ; ++i)
    {
      if ((d1 = vec[i], Abs (d1)) < *vmax)
	{
	  goto L10;
	}
      *vmax = (d1 = vec[i], Abs (d1));
      *ibar = i;
    L10:
      ;
    }
 L20:
  if (vec[*ibar] < 0.)
    {
      *vmax = -(*vmax);
    }
  return 0;
}

