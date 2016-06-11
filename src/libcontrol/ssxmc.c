#include "ctrlpack.h"

/*! calling sequence 
 *      n        int 
 *               -the order of original state-space representation; 
 *               declared first dimension of nblk,wrka; declared 
 *               second dimension of a (and z, if mode .ne. 0) 
 *      m        int 
 *               -the number of system inputs; declared first dimension 
 *               of iwrk,wrk1,wrk2; declared second dimension of b,wrka 
 *      a        double precision(n,n) 
 *               -the original state dynamics matrix.  note that this 
 *               matrix is overwritten here 
 *      na       int 
 *               -the declared first dimension of a,b (and z, if 
 *               mode .ne. 0).  note that na .ge. n 
 *      b        double precision(n,m) 
 *               -the original input/state matrix.  note that this 
 *               matrix is overwritten here 
 *      tol      double precision 
 *               -if greater than the machine precision, tol is used 
 *               as zero tolerance in rank determination when trans- 
 *               forming (a,b,c): otherwise (eg tol = 0.0d+0), the 
 *               machine precision is used 
 *      mode     int 
 *               -mode = 0 if accumulation of the orthogonal trans- 
 *               formation z is not required, and non-zero if this 
 *               matrix is required 
 *arguments out 
 * 
 *      a        double precision(ncont,ncont) 
 *               -the upper block hessenberg state dynamics matrix of 
 *               a controllable realization for the original system 
 *      b        double precision(ncont,m) 
 *               -the transformed input/state matrix 
 *      ncont    int 
 *               -the order of controllable state-space representation 
 *      indcon   int 
 *               -the controllability index of transformed 
 *               system representation 
 *      nblk     int(indcon) 
 *               -the dimensions of the diagonal blocks of the trans- 
 *               formed a 
 *      z        double precision(n,n) 
 *               -the orthogonal similarity transformation which 
 *               reduces the given system to orthogonal canonical 
 *               form.  note that, if mode .eq. 0, z is not referenced 
 *               and so can be a scalar dummy variable 
 *!working space 
 *      wrka     double precision(n,m) 
 *      wrk1     double precision(m) 
 *      wrk2     double precision(m) 
 *      iwrk     int(m) 
 *!purpose 
 * 
 *       to reduce the linear time-invariant multi-input system 
 * 
 *            dx/dt = a * x + b * u, 
 * 
 *       where a and b are (n x n) and (n x m) matrices respectively, 
 *       to orthogonal canonical form using (and optionally accum- 
 *       ulating) orthogonal similarity transformations. 
 * 
 *!method 
 * 
 *       b is first qr-decomposed and the appropriate orthogonal 
 *       similarity transformation applied to a.  leaving the first 
 *       rank(b) states unchanged, the resulting lower left block 
 *       of a is now itself qr-decomposed and this new orthogonal 
 *       similarity transformation applied.  continuing in this 
 *       manner, a completely controllable state-space pair (acont, 
 *       bcont) is found for the given (a,b), where acont is upper 
 *       block hessenberg with each sub-diagonal block of full row 
 *       rank, and bcont is zero apart from its (independent) first 
 *       rank(b) rows.  note finally that the system controllability 
 *       indices are easily calculable from the dimensions of the 
 *       blocks of acont. 
 * 
 *!reference 
 * 
 *       konstantinov, m.m., petkov, p.hr. and christov, n.d. 
 *       "orthogonal invariants and canonical forms for linear 
 *       controllable systems" 
 *       proc. ifac 8th world congress, 1981. 
 * 
 *!auxiliary routines 
 * 
 *       dqrdc (linpack) 
 * 
 *!originator 
 * 
 *               p.hr.petkov, higher institute of mechanical and 
 *               electrical engineering, sofia, bulgaria, april 1981 
 *    Copyright SLICOT 
 * 
 *!comments 
 * 
 *               none 
 * 
 *!user-supplied routines 
 * 
 *               none 
 *! 
 ******************************************************************** 
 * 
 * 
 * 
 * 
 *    local variables: 
 * 
 * 
 * 
 *     common /smprec/eps 
 * 
 *    common block smprec is shared with routine ddata which provides 
 *    a value for eps, a machine-dependent parameter which specifies 
 *    the relative precision of drealing-point arithmetic 
 * 
 * 
 *     call ddata 
 * 
 */


static int c__1 = 1;
static int c__11 = 11;
static int c__0 = 0;

int
nsp_ctrlpack_ssxmc ( int *n, int *m, double *a, int *na, double *b, int *ncont, int *indcon, int *nblk, double *z__,
		     double *wrka, double *wrk1, double *wrk2, int *iwrk, double *tol, int *mode)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset, wrka_dim1,
    wrka_offset, i__1, i__2;
  double d__1, d__2;
  /* Local variables */
  int ierr, irnk;
  double temp;
  int i__, j, k;
  int ia, ja, nb, mb, ni, nj, mm, lu;
  double abnorm;
  int im1;
  double thrtol;
  int ist;

  /* Parameter adjustments */
  --nblk;
  --iwrk;
  --wrk2;
  --wrk1;
  wrka_dim1 = *n;
  wrka_offset = wrka_dim1 + 1;
  wrka -= wrka_offset;
  z_dim1 = *na;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  b_dim1 = *na;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  abnorm = 0.;
  ist = 0;
  *ncont = 0;
  *indcon = 0;
  ni = 0;
  nb = *n;
  mb = *m;
  /* 
   *    use the larger of tol, eps in rank determination 
   * 
   *     toleps = dble(n * n) * Max(tol,eps) 
   * 
   */
  if (*mode == 0)
    {
      goto L30;
    }
  /* 
   *    initialize  z  to identity matrix 
   * 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L10: */
	  z__[i__ + j * z_dim1] = 0.;
	}
      /* 
       */
      z__[i__ + i__ * z_dim1] = 1.;
      /* L20: */
    }
  /* 
   */
 L30:
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  wrka[i__ + j * wrka_dim1] = b[i__ + j * b_dim1];
	  b[i__ + j * b_dim1] = 0.;
	  /* L40: */
	}
      /* 
       */
      /* L50: */
    }
  /* 
   */
 L60:
  ++ist;
  /* 
   *    qr decomposition with column pivoting 
   * 
   */
  i__1 = mb;
  for (j = 1; j <= i__1; ++j)
    {
      /* L70: */
      iwrk[j] = 0;
    }
  /* 
   */
  nsp_ctrlpack_dqrdc (&wrka[wrka_offset], n, &nb, &mb, &wrk1[1], &iwrk[1],
		      &wrk2[1], &c__1);
  /* 
   */
  irnk = 0;
  mm = Min (nb, mb);
  if ((d__1 = wrka[wrka_dim1 + 1], Abs (d__1)) > abnorm)
    {
      abnorm = (d__2 = wrka[wrka_dim1 + 1], Abs (d__2));
    }
  /*     thresh = toleps * abnorm 
   * 
   *    rank determination 
   * 
   */
  thrtol = *tol * abnorm * (double) (*n * *n);
  i__1 = mm;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      temp = (d__1 = wrka[i__ + i__ * wrka_dim1], Abs (d__1));
      if (temp > thrtol && temp + 1. > 1.)
	{
	  irnk = i__;
	}
      /* L100: */
    }
  /* 
   */
  if (irnk == 0)
    {
      goto L360;
    }
  nj = ni;
  ni = *ncont;
  *ncont += irnk;
  ++(*indcon);
  nblk[*indcon] = irnk;
  /*Computing MIN 
   */
  i__1 = irnk, i__2 = nb - 1;
  lu = Min (i__1, i__2);
  if (lu == 0)
    {
      goto L200;
    }
  /* 
   *    premultiply appropriate row block of a by qtrans 
   * 
   */
  nsp_ctrlpack_hhdml (&lu, n, n, &ni, &ni, &nb, &nb, &wrka[wrka_offset], n,
		      &wrk1[1], &a[a_offset], na, &c__11, &ierr);
  /* 
   *    postmultiply appropriate column block of a by q 
   * 
   */
  nsp_ctrlpack_hhdml (&lu, n, n, &c__0, &ni, n, &nb, &wrka[wrka_offset], n,
		      &wrk1[1], &a[a_offset], na, &c__0, &ierr);
  /* 
   *    if required, accumulate transformations 
   * 
   */
  if (*mode != 0)
    {
      nsp_ctrlpack_hhdml (&lu, n, n, &c__0, &ni, n, &nb, &wrka[wrka_offset],
			  n, &wrk1[1], &z__[z_offset], na, &c__0, &ierr);
    }
  /* 
   */
 L200:
  if (irnk < 2)
    {
      goto L230;
    }
  /* 
   */
  i__1 = irnk;
  for (i__ = 2; i__ <= i__1; ++i__)
    {
      im1 = i__ - 1;
      /* 
       */
      i__2 = im1;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L210: */
	  wrka[i__ + j * wrka_dim1] = 0.;
	}
      /* 
       */
      /* L220: */
    }
  /* 
   *    backward permutation of the columns 
   * 
   */
 L230:
  i__1 = mb;
  for (j = 1; j <= i__1; ++j)
    {
      if (iwrk[j] < 0)
	{
	  goto L270;
	}
      k = iwrk[j];
      iwrk[j] = -k;
    L240:
      if (k == j)
	{
	  goto L260;
	}
      /* 
       */
      i__2 = irnk;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  temp = wrka[i__ + k * wrka_dim1];
	  wrka[i__ + k * wrka_dim1] = wrka[i__ + j * wrka_dim1];
	  wrka[i__ + j * wrka_dim1] = temp;
	  /* L250: */
	}
      /* 
       */
      iwrk[k] = -iwrk[k];
      k = -iwrk[k];
      goto L240;
    L260:
    L270:
      ;
    }
  /* 
   */
  if (ist > 1)
    {
      goto L300;
    }
  /* 
   *    form  b 
   * 
   */
  i__1 = irnk;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L280: */
	  b[i__ + j * b_dim1] = wrka[i__ + j * wrka_dim1];
	}
      /* 
       */
      /* L290: */
    }
  /* 
   */
  goto L330;
  /* 
   *    form  a 
   * 
   */
 L300:
  i__1 = irnk;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ia = ni + i__;
      /* 
       */
      i__2 = mb;
      for (j = 1; j <= i__2; ++j)
	{
	  ja = nj + j;
	  /* L310: */
	  a[ia + ja * a_dim1] = wrka[i__ + j * wrka_dim1];
	}
      /* 
       */
      /* L320: */
    }
  /* 
   */
 L330:
  if (irnk == nb)
    {
      goto L360;
    }
  /* 
   */
  mb = irnk;
  nb -= irnk;
  /* 
   */
  i__1 = nb;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ia = *ncont + i__;
      /* 
       */
      i__2 = mb;
      for (j = 1; j <= i__2; ++j)
	{
	  ja = ni + j;
	  wrka[i__ + j * wrka_dim1] = a[ia + ja * a_dim1];
	  a[ia + ja * a_dim1] = 0.;
	  /* L340: */
	}
      /* 
       */
      /* L350: */
    }
  goto L60;
  /* 
   */
 L360:
  /* 
   */
  return 0;
}

