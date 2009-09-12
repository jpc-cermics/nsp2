#include "integ.h"
#include "nsp/cnumeric.h"

#define ls0001_1 ls0001_._1

static int c__30 = 30;
static int c__51 = 51;
static int c__1 = 1;
static int c__0 = 0;
static double c_b20 = 0.;
static int c__52 = 52;
static int c__60 = 60;
static int c__2 = 2;

/*
 *intdy computes interpolated values of the k-th derivative of the 
 *dependent variable vector y, and stores it in dky.  this routine 
 *is called within the package with k = 0 and t = tout, but may 
 *also be called by the user for any k up to the current order. 
 *(see detailed instructions in the usage documentation.) 
 *%method 
 *the computed values in dky are gotten by interpolation using the 
 *nordsieck history array yh.  this array corresponds uniquely to a 
 *vector-valued polynomial of degree nqcur or less, and dky is set 
 *to the k-th derivative of this polynomial at t. 
 *the formula for dky is.. 
 *             q 
 * dky(i)  =  sum  c(j,k) * (t - tn)**(j-k) * h**(-j) * yh(i,j+1) 
 *            j=k 
 *where  c(j,k) = j*(j-1)*...*(j-k+1), q = nqcur, tn = tcur, h = hcur. 
 *the quantities  nq = nqcur, l = nq+1, n = neq, tn, and h are 
 *communicated by common.  the above sum is done in reverse order. 
 *iflag is returned negative if either k or t is out of bounds. 
 */

int nsp_ode_intdy (double *t, int *k, double *yh, int *nyh, double *dky,
		      int *iflag)
{
  int yh_dim1, yh_offset, i__1, i__2;
  /* Local variables */
  double c__;
  int i__, j;
  double r__, s;
  int ic, jb, jj;
  double tp;
  int jb2, jj1, jp1;

  /* Parameter adjustments */
  yh_dim1 = *nyh;
  yh_offset = yh_dim1 + 1;
  yh -= yh_offset;
  --dky;

  /* Function Body */
  *iflag = 0;
  if (*k < 0 || *k > ls0001_1.nq)
    {
      goto L80;
    }
  tp = ls0001_1.tn - ls0001_1.hu * (ls0001_1.uround * 100. + 1.);
  if ((*t - tp) * (*t - ls0001_1.tn) > 0.)
    {
      goto L90;
    }
  /* 
   */
  s = (*t - ls0001_1.tn) / ls0001_1.h__;
  ic = 1;
  if (*k == 0)
    {
      goto L15;
    }
  jj1 = ls0001_1.l - *k;
  i__1 = ls0001_1.nq;
  for (jj = jj1; jj <= i__1; ++jj)
    {
      /* L10: */
      ic *= jj;
    }
 L15:
  c__ = (double) ic;
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L20: */
      dky[i__] = c__ * yh[i__ + ls0001_1.l * yh_dim1];
    }
  if (*k == ls0001_1.nq)
    {
      goto L55;
    }
  jb2 = ls0001_1.nq - *k;
  i__1 = jb2;
  for (jb = 1; jb <= i__1; ++jb)
    {
      j = ls0001_1.nq - jb;
      jp1 = j + 1;
      ic = 1;
      if (*k == 0)
	{
	  goto L35;
	}
      jj1 = jp1 - *k;
      i__2 = j;
      for (jj = jj1; jj <= i__2; ++jj)
	{
	  /* L30: */
	  ic *= jj;
	}
    L35:
      c__ = (double) ic;
      i__2 = ls0001_1.n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  /* L40: */
	  dky[i__] = c__ * yh[i__ + jp1 * yh_dim1] + s * dky[i__];
	}
      /* L50: */
    }
  if (*k == 0)
    {
      return 0;
    }
 L55:
  i__1 = -(*k);
  r__ = nsp_pow_di (ls0001_1.h__, i__1);
  i__1 = ls0001_1.n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* L60: */
      dky[i__] = r__ * dky[i__];
    }
  return 0;
  /* 
   */
 L80:
  C2F (xerrwv) ("intdy--  k (=i1) illegal      ", &c__30, &c__51, &c__1,
		&c__1, k, &c__0, &c__0, &c_b20, &c_b20, 30L);
  *iflag = -1;
  return 0;
 L90:
  C2F (xerrwv) ("intdy--  t (=r1) illegal      ", &c__30, &c__52, &c__1,
		&c__0, &c__0, &c__0, &c__1, t, &c_b20, 30L);
  C2F (xerrwv) ("      t is not between tcur - hu (= r1) and tcur (=r2)",
		&c__60, &c__52, &c__1, &c__0, &c__0, &c__0, &c__2, &tp,
		&ls0001_1.tn, 54L);
  *iflag = -2;
  return 0;
}

