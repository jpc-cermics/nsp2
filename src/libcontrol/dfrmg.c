/* dfrmg.f -- translated by f2c (version 19961017).  */

#include "ctrlpack.h"

static double c_b11 = 0.;
static int c__1 = 1;
static int c__0 = 0;

/* 
 *    sfrmg takes real matrices a (n x n), b (n x m), and c (l x n) 
 *    and forms the complex frequency response matrix 
 *          g(freq) := c * (((freq * i) - a)-inverse) * b 
 *    where  i = (n x n) identity matrix and  freq  is a complex 
 *    scalar parameter taking values along the imaginary axis for 
 *    continuous-time systems and on the unit circle for discrete- 
 *    time systems. 
 * 
 *    on entry: 
 *       job    int 
 *                set = 0.  for the first call of dfrmg whereupon 
 *                it is set to 1 for all subsequent calls; 
 *       na       int 
 *                the leading or row dimension of the real array a 
 *                (and the complex array h) as declared in the main 
 *                calling program. 
 *       nb       int 
 *                the leading or row dimension of the real array b 
 *                (and the complex array ainvb) as declared in the main 
 *                calling program. 
 *       nc       int 
 *                the leading or row dimension of the real array c 
 *                (and the complex array g) as declared in the main 
 *                calling program. 
 *       l        int 
 *                the number of rows of c (the number of outputs). 
 *       m        int 
 *                the number of columns of b (the number of inputs). 
 *       n        int 
 *                the order of the matrix a (the number of states); 
 *                also = number of columns of c = number of rows of b. 
 *       a        real(na,n) 
 *                a real n x n matrix (the system matrix); not needed as 
 *                input if job .eq. .false. 
 *       b        real(nb,m) 
 *                a real n x m matrix (the input matrix); not needed as 
 *                input if job .eq. 1 
 *       c        real(nc,n) 
 *                a real l x n matrix (the output matrix); not needed as 
 *                input if job .eq. 1 
 *       freq     complex 
 *                a complex scalar (the frequency parameter). 
 *    on return: 
 * 
 *       g        complex(nc,m) 
 *                the frequency response matrix g(freq). 
 * 
 *       a,b,c    a is in upper hessenberg form while b and c have been 
 *                arrays are not further modified. 

 *       rcond    real 
 *                parameter of subroutine checo (checo may be consulted 
 *                for details); normal return is then 
 *                     (1.0 + rcond) .gt. 1.0. 
 * 
 *      w (2*(n*n)+2*n)    tableau de travail 
 * 
 *       ipvt(n)       tableau de travail entier 
 *    this version dated june 1982. 
 *    alan j. laub, university of southern california. 
 * 
 *    subroutines and functions called: 
 *    balanc(eispack) ,checo,chefa,chesl,hqr(eispack),shetr 
 */

int
nsp_ctrlpack_dfrmg (int *job, int *na, int *nb, int *nc, int *l, int *m,
		    int *n, double *a, double *b, double *c__, double *freqr,
		    double *freqi, double *gr, double *gi, double *rcond,
		    double *w, int *ipvt)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, gr_dim1,
    gr_offset, gi_dim1, gi_offset, i__1, i__2, i__3;
  double d__1;

  /* Local variables */
  int i__, j, k;
  double t;
  int j1;
  int kk, kp, nn, igh, izi, low, izr;

  /* Parameter adjustments */
  gi_dim1 = *nc;
  gi_offset = gi_dim1 + 1;
  gi -= gi_offset;
  gr_dim1 = *nc;
  gr_offset = gr_dim1 + 1;
  gr -= gr_offset;
  b_dim1 = *nb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  --ipvt;
  c_dim1 = *nc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --w;

  /* Function Body */
  if (*job != 0)
    {
      goto L150;
    }
  nsp_ctrlpack_balanc (na, n, &a[a_offset], &low, &igh, &w[1]);
  /* 
   *    adjust b and c matrices based on information in the vector 
   *    w which describes the balancing of a and is defined in the 
   *    subroutine balanc 
   * 
   */
  i__1 = *n;
  for (k = 1; k <= i__1; ++k)
    {
      kk = *n - k + 1;
      if (kk >= low && kk <= igh)
	{
	  goto L40;
	}
      if (kk < low)
	{
	  kk = low - kk;
	}
      kp = (int) w[kk];
      if (kp == kk)
	{
	  goto L40;
	}
      /* 
       *       permute rows of b 
       * 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  t = b[kk + j * b_dim1];
	  b[kk + j * b_dim1] = b[kp + j * b_dim1];
	  b[kp + j * b_dim1] = t;
	  /* L20: */
	}
      /* 
       *       permute columns of c 
       * 
       */
      i__2 = *l;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  t = c__[i__ + kk * c_dim1];
	  c__[i__ + kk * c_dim1] = c__[i__ + kp * c_dim1];
	  c__[i__ + kp * c_dim1] = t;
	  /* L30: */
	}
      /* 
       */
    L40:
      ;
    }
  if (igh == low)
    {
      goto L80;
    }
  i__1 = igh;
  for (k = low; k <= i__1; ++k)
    {
      t = w[k];
      /* 
       *       scale columns of permuted c 
       * 
       */
      i__2 = *l;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  c__[i__ + k * c_dim1] *= t;
	  /* L50: */
	}
      /* 
       *       scale rows of permuted b 
       * 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  b[k + j * b_dim1] /= t;
	  /* L60: */
	}
      /* 
       */
      /* L70: */
    }
 L80:
  /* 
   *    reduce a to hessenberg form by orthogonal similarities and 
   *    accumulate the orthogonal transformations into b and c 
   * 
   */
  nsp_ctrlpack_dhetr (na, nb, nc, l, m, n, &low, &igh, &a[a_offset],
		      &b[b_offset], &c__[c_offset], &w[1]);
  /* 
   */
  /* L140: */
  *job = 1;
  /* 
   *    update  h := (freq *i) - a  with appropriate value of freq 
   * 
   */
 L150:
  nn = *n * *n;
  j1 = 1 - *n;
  i__1 = nn << 1;
  nsp_dset (&i__1, &c_b11, &w[1], &c__1);
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      j1 += *n;
      /*Computing MIN 
       */
      i__3 = j + 1;
      i__2 = Min (i__3, *n);
      C2F (dcopy) (&i__2, &a[j * a_dim1 + 1], &c__1, &w[j1], &c__1);
      /* L170: */
      w[j1 + j - 1] -= *freqr;
    }
  d__1 = -(*freqi);
  i__1 = *n + 1;
  nsp_dset (n, &d__1, &w[nn + 1], &i__1);
  /* 
   *    factor the complex hessenberg matrix and estimate its 
   *    condition 
   * 
   */
  izr = nn + nn + 1;
  izi = izr + *n;
  nsp_ctrlpack_wgeco (&w[1], &w[nn + 1], n, n, &ipvt[1], rcond, &w[izr],
		      &w[izi]);
  t = *rcond + 1.;
  if (t == 1.)
    {
      return 0;
    }
  /* L190: */
  /* 
   *    compute  c*(h-inverse)*b 
   * 
   */
  i__1 = *m;
  for (j = 1; j <= i__1; ++j)
    {
      C2F (dcopy) (n, &b[j * b_dim1 + 1], &c__1, &w[izr], &c__1);
      nsp_dset (n, &c_b11, &w[izi], &c__1);
      nsp_ctrlpack_wgesl (&w[1], &w[nn + 1], n, n, &ipvt[1], &w[izr], &w[izi],
			  &c__0);
      i__2 = *l;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  gr[i__ + j * gr_dim1] =
	    -C2F (ddot) (n, &c__[i__ + c_dim1], nc, &w[izr], &c__1);
	  gi[i__ + j * gi_dim1] =
	    -C2F (ddot) (n, &c__[i__ + c_dim1], nc, &w[izi], &c__1);
	  /* L240: */
	}
      /* L220: */
    }
  return 0;
}				/* dfrmg_ */
