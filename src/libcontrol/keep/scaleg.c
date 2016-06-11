/* scaleg.f -- translated by f2c (version 19961017).  */

/* 
 *    scales the matrices a and b in the generalized eigenvalue 
 *    problem a*x = (lambda)*b*x such that the magnitudes of the 
 *    elements of the submatrices of a and b (as specified by low 
 *    and igh) are close to unity in the least squares sense. 
 *    ref.:  ward, r. c., balancing the generalized eigenvalue 
 *    problem, siam j. sci. stat. comput., vol. 2, no. 2, june 1981, 
 *    141-152. 
 *    on input: 
 * 
 *      ma,mb   int 
 *              row dimensions of the arrays containing matrices 
 *              a and b respectively, as declared in the main calling 
 *              program dimension statement; 
 * 
 *      n       int 
 *              order of the matrices a and b; 
 * 
 *      a       real(ma,n) 
 *              contains the a matrix of the generalized eigenproblem 
 *              defined above; 
 * 
 *      b       real(mb,n) 
 *              contains the b matrix of the generalized eigenproblem 
 *              defined above; 
 * 
 *      low     int 
 *              specifies the beginning -1 for the rows and 
 *              columns of a and b to be scaled; 
 * 
 *      igh     int 
 *              specifies the ending -1 for the rows and columns 
 *              of a and b to be scaled; 
 * 
 *      cperm   real(n) 
 *              work array.  only locations low through igh are 
 *              referenced and altered by this subroutine; 
 * 
 *      wk      real(n,6) 
 *              work array that must contain at least 6*n locations. 
 *              only locations low through igh, n+low through n+igh, 
 *              ..., 5*n+low through 5*n+igh are referenced and 
 *              altered by this subroutine. 
 * 
 *    on output: 
 * 
 *      a,b     contain the scaled a and b matrices; 
 * 
 *      cscale  real(n) 
 *              contains in its low through igh locations the int 
 *              exponents of 2 used for the column scaling factors. 
 *              the other locations are not referenced; 
 * 
 *      wk      contains in its low through igh locations the int 
 *              exponents of 2 used for the row scaling factors. 
 * 
 *    *****algorithm notes: 
 *    none. 
 * 
 *    *****history: 
 *    written by r. c. ward....... 
 *    modified 8/86 by bobby bodenheimer so that if 
 *      sum = 0 (corresponding to the case where the matrix 
 *      doesn't need to be scaled) the routine returns. 
 */

#include "ctrlpack.h"

#define d_lg10(x) (0.43429448190325182765*log(x))

static double c_b4 = 2.;
static double c_b22 = .5;

int nsp_ctrlpack_scaleg (int *n, int *ma, double *a, int *mb, double *b, int *low,
			 int *igh, double *cscale, double *cperm, double *wk)
{
  int a_dim1, a_offset, b_dim1, b_offset, wk_dim1, wk_offset, i__1, i__2;
  double d__1, d__2, d__3;

  /* Local variables */
  double beta, coef, basl, cmax, coef2, coef5;
  int i__, j;
  double gamma, t, alpha;
  int kount, jc;
  double fi, fj, ta, tb, tc;
  int ir;
  double ew;
  int it, nr;
  double pgamma, ewc, cor, sum;
  int nrp2;


  /* Parameter adjustments */
  wk_dim1 = *n;
  wk_offset = wk_dim1 + 1;
  wk -= wk_offset;
  --cperm;
  --cscale;
  a_dim1 = *ma;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  b_dim1 = *mb;
  b_offset = b_dim1 + 1;
  b -= b_offset;

  /* Function Body */
  if (*low == *igh)
    {
      goto L410;
    }
  i__1 = *igh;
  for (i__ = *low; i__ <= i__1; ++i__)
    {
      wk[i__ + wk_dim1] = 0.;
      wk[i__ + (wk_dim1 << 1)] = 0.;
      wk[i__ + wk_dim1 * 3] = 0.;
      wk[i__ + (wk_dim1 << 2)] = 0.;
      wk[i__ + wk_dim1 * 5] = 0.;
      wk[i__ + wk_dim1 * 6] = 0.;
      cscale[i__] = 0.;
      cperm[i__] = 0.;
      /* L210: */
    }
  /* 
   *    compute right side vector in resulting linear equations 
   * 
   */
  basl = d_lg10 (c_b4);
  i__1 = *igh;
  for (i__ = *low; i__ <= i__1; ++i__)
    {
      i__2 = *igh;
      for (j = *low; j <= i__2; ++j)
	{
	  tb = b[i__ + j * b_dim1];
	  ta = a[i__ + j * a_dim1];
	  if (ta == 0.)
	    {
	      goto L220;
	    }
	  d__1 = Abs (ta);
	  ta = d_lg10 (d__1) / basl;
	L220:
	  if (tb == 0.)
	    {
	      goto L230;
	    }
	  d__1 = Abs (tb);
	  tb = d_lg10 (d__1) / basl;
	L230:
	  wk[i__ + wk_dim1 * 5] = wk[i__ + wk_dim1 * 5] - ta - tb;
	  wk[j + wk_dim1 * 6] = wk[j + wk_dim1 * 6] - ta - tb;
	  /* L240: */
	}
    }
  nr = *igh - *low + 1;
  coef = 1. / (double) (nr << 1);
  coef2 = coef * coef;
  coef5 = coef2 * .5;
  nrp2 = nr + 2;
  beta = 0.;
  it = 1;
  /* 
   *    start generalized conjugate gradient iteration 
   * 
   */
 L250:
  ew = 0.;
  ewc = 0.;
  gamma = 0.;
  i__2 = *igh;
  for (i__ = *low; i__ <= i__2; ++i__)
    {
      gamma =
	gamma + wk[i__ + wk_dim1 * 5] * wk[i__ + wk_dim1 * 5] + wk[i__ +
								   wk_dim1 *
								   6] *
	wk[i__ + wk_dim1 * 6];
      ew += wk[i__ + wk_dim1 * 5];
      ewc += wk[i__ + wk_dim1 * 6];
      /* L260: */
    }
  /*Computing 2nd power 
   */
  d__1 = ew;
  /*Computing 2nd power 
   */
  d__2 = ewc;
  /*Computing 2nd power 
   */
  d__3 = ew - ewc;
  gamma =
    coef * gamma - coef2 * (d__1 * d__1 + d__2 * d__2) -
    coef5 * (d__3 * d__3);
  if (it != 1)
    {
      beta = gamma / pgamma;
    }
  t = coef5 * (ewc - ew * 3.);
  tc = coef5 * (ew - ewc * 3.);
  i__2 = *igh;
  for (i__ = *low; i__ <= i__2; ++i__)
    {
      wk[i__ + (wk_dim1 << 1)] =
	beta * wk[i__ + (wk_dim1 << 1)] + coef * wk[i__ + wk_dim1 * 5] + t;
      cperm[i__] = beta * cperm[i__] + coef * wk[i__ + wk_dim1 * 6] + tc;
      /* L270: */
    }
  /* 
   *    apply matrix to vector 
   * 
   */
  i__2 = *igh;
  for (i__ = *low; i__ <= i__2; ++i__)
    {
      kount = 0;
      sum = 0.;
      i__1 = *igh;
      for (j = *low; j <= i__1; ++j)
	{
	  if (a[i__ + j * a_dim1] == 0.)
	    {
	      goto L280;
	    }
	  ++kount;
	  sum += cperm[j];
	L280:
	  if (b[i__ + j * b_dim1] == 0.)
	    {
	      goto L290;
	    }
	  ++kount;
	  sum += cperm[j];
	L290:
	  ;
	}
      wk[i__ + wk_dim1 * 3] = (double) kount *wk[i__ + (wk_dim1 << 1)] + sum;
      /* L300: */
    }
  i__2 = *igh;
  for (j = *low; j <= i__2; ++j)
    {
      kount = 0;
      sum = 0.;
      i__1 = *igh;
      for (i__ = *low; i__ <= i__1; ++i__)
	{
	  if (a[i__ + j * a_dim1] == 0.)
	    {
	      goto L310;
	    }
	  ++kount;
	  sum += wk[i__ + (wk_dim1 << 1)];
	L310:
	  if (b[i__ + j * b_dim1] == 0.)
	    {
	      goto L320;
	    }
	  ++kount;
	  sum += wk[i__ + (wk_dim1 << 1)];
	L320:
	  ;
	}
      wk[j + (wk_dim1 << 2)] = (double) kount *cperm[j] + sum;
      /* L330: */
    }
  sum = 0.;
  i__2 = *igh;
  for (i__ = *low; i__ <= i__2; ++i__)
    {
      sum =
	sum + wk[i__ + (wk_dim1 << 1)] * wk[i__ + wk_dim1 * 3] +
	cperm[i__] * wk[i__ + (wk_dim1 << 2)];
      /* L340: */
    }
  if (sum == 0.)
    {
      return 0;
    }
  alpha = gamma / sum;
  /* 
   *    determine correction to current iterate 
   * 
   */
  cmax = 0.;
  i__2 = *igh;
  for (i__ = *low; i__ <= i__2; ++i__)
    {
      cor = alpha * wk[i__ + (wk_dim1 << 1)];
      if (Abs (cor) > cmax)
	{
	  cmax = Abs (cor);
	}
      wk[i__ + wk_dim1] += cor;
      cor = alpha * cperm[i__];
      if (Abs (cor) > cmax)
	{
	  cmax = Abs (cor);
	}
      cscale[i__] += cor;
      /* L350: */
    }
  if (cmax < .5)
    {
      goto L370;
    }
  i__2 = *igh;
  for (i__ = *low; i__ <= i__2; ++i__)
    {
      wk[i__ + wk_dim1 * 5] -= alpha * wk[i__ + wk_dim1 * 3];
      wk[i__ + wk_dim1 * 6] -= alpha * wk[i__ + (wk_dim1 << 2)];
      /* L360: */
    }
  pgamma = gamma;
  ++it;
  if (it <= nrp2)
    {
      goto L250;
    }
  /* 
   *    end generalized conjugate gradient iteration 
   * 
   */
 L370:
  i__2 = *igh;
  for (i__ = *low; i__ <= i__2; ++i__)
    {
      ir = (int) (wk[i__ + wk_dim1] + d_sign (&c_b22, &wk[i__ + wk_dim1]));
      wk[i__ + wk_dim1] = (double) ir;
      jc = (int) (cscale[i__] + d_sign (&c_b22, &cscale[i__]));
      cscale[i__] = (double) jc;
      /* L380: */
    }
  /* 
   *    scale a and b 
   * 
   */
  i__2 = *igh;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      ir = (int) wk[i__ + wk_dim1];
      fi = pow_di (c_b4, ir);
      if (i__ < *low)
	{
	  fi = 1.;
	}
      i__1 = *n;
      for (j = *low; j <= i__1; ++j)
	{
	  jc = (int) cscale[j];
	  fj = pow_di (c_b4, jc);
	  if (j <= *igh)
	    {
	      goto L390;
	    }
	  if (i__ < *low)
	    {
	      goto L400;
	    }
	  fj = 1.;
	L390:
	  a[i__ + j * a_dim1] = a[i__ + j * a_dim1] * fi * fj;
	  b[i__ + j * b_dim1] = b[i__ + j * b_dim1] * fi * fj;
	L400:
	  ;
	}
    }
 L410:
  return 0;

}

