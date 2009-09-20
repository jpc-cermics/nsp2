#include "integ.h"

/*!purpose 
 * 
 *    dgbsl solves the double precision band system 
 *    a * x = b  or  trans(a) * x = b 
 *    using the factors computed by dgbco or dgbfa. 
 * 
 *!calling sequence 
 * 
 *     subroutine dgbsl(abd,lda,n,ml,mu,ipvt,b,job) 
 *    on entry 
 * 
 *       abd     double precision(lda, n) 
 *               the output from dgbco or dgbfa. 
 *       lda     int 
 *               the leading dimension of the array  abd . 
 *       n       int 
 *               the order of the original matrix. 
 *       ml      int 
 *               number of diagonals below the main diagonal. 
 *       mu      int 
 *               number of diagonals above the main diagonal. 
 *       ipvt    int(n) 
 *               the pivot vector from dgbco or dgbfa. 
 *       b       double precision(n) 
 *               the right hand side vector. 
 *       job     int 
 *               = 0         to solve  a*x = b , 
 *               = nonzero   to solve  trans(a)*x = b , where 
 *                           trans(a)  is the transpose. 
 *    on return 
 *       b       the solution vector  x . 
 *    error condition 
 * 
 *       a division by zero will occur if the input factor contains a 
 *       zero on the diagonal.  technically this indicates singularity 
 *       but it is often caused by improper arguments or improper 
 *       setting of lda .  it will not occur if the subroutines are 
 *       called correctly and if dgbco has set rcond .gt. 0.0 
 *       or dgbfa has set info .eq. 0 . 
 * 
 *    to compute  inverse(a) * c  where  c  is a matrix 
 *    with  p  columns 
 *          call dgbco(abd,lda,n,ml,mu,ipvt,rcond,z) 
 *          if (rcond is too small) go to ... 
 *          do 10 j = 1, p 
 *             call dgbsl(abd,lda,n,ml,mu,ipvt,c(1,j),0) 
 *       10 continue 
 * 
 *    linpack. this version dated 08/14/78 . 
 *    cleve moler, university of new mexico, argonne national lab. 
 */

int
nsp_ode_dgbsl (double *abd, int *lda, const int *n, const int *ml,
		  const int *mu, int *ipvt, double *b, const int *job)
{
  int c__1 = 1;

  /* System generated locals */
  int abd_dim1, abd_offset, i1, i2, i3;

  /* Local variables */
  double t;
  int k, l, m;
  int kb, la, lb, lm, nm1;
  /* Parameter adjustments */
  abd_dim1 = *lda;
  abd_offset = abd_dim1 + 1;
  abd -= abd_offset;
  --ipvt;
  --b;

  /* Function Body */
  m = *mu + *ml + 1;
  nm1 = *n - 1;
  if (*job != 0)
    {
      goto L50;
    }
  /* 
   *       job = 0 , solve  a * x = b 
   *       first solve l*y = b 
   * 
   */
  if (*ml == 0)
    {
      goto L30;
    }
  if (nm1 < 1)
    {
      goto L30;
    }
  i1 = nm1;
  for (k = 1; k <= i1; ++k)
    {
      /*Computing MIN 
       */
      i2 = *ml, i3 = *n - k;
      lm = Min (i2, i3);
      l = ipvt[k];
      t = b[l];
      if (l == k)
	{
	  goto L10;
	}
      b[l] = b[k];
      b[k] = t;
    L10:
      C2F (daxpy) (&lm, &t, &abd[m + 1 + k * abd_dim1], &c__1, &b[k + 1],
		   &c__1);
      /* L20: */
    }
 L30:
  /* 
   *       now solve  u*x = y 
   * 
   */
  i1 = *n;
  for (kb = 1; kb <= i1; ++kb)
    {
      k = *n + 1 - kb;
      b[k] /= abd[m + k * abd_dim1];
      lm = Min (k, m) - 1;
      la = m - lm;
      lb = k - lm;
      t = -b[k];
      C2F (daxpy) (&lm, &t, &abd[la + k * abd_dim1], &c__1, &b[lb], &c__1);
      /* L40: */
    }
  goto L100;
 L50:
  /* 
   *       job = nonzero, solve  trans(a) * x = b 
   *       first solve  trans(u)*y = b 
   * 
   */
  i1 = *n;
  for (k = 1; k <= i1; ++k)
    {
      lm = Min (k, m) - 1;
      la = m - lm;
      lb = k - lm;
      t = C2F (ddot) (&lm, &abd[la + k * abd_dim1], &c__1, &b[lb], &c__1);
      b[k] = (b[k] - t) / abd[m + k * abd_dim1];
      /* L60: */
    }
  /* 
   *       now solve trans(l)*x = y 
   * 
   */
  if (*ml == 0)
    {
      goto L90;
    }
  if (nm1 < 1)
    {
      goto L90;
    }
  i1 = nm1;
  for (kb = 1; kb <= i1; ++kb)
    {
      k = *n - kb;
      /*Computing MIN 
       */
      i2 = *ml, i3 = *n - k;
      lm = Min (i2, i3);
      b[k] +=
	C2F (ddot) (&lm, &abd[m + 1 + k * abd_dim1], &c__1, &b[k + 1], &c__1);
      l = ipvt[k];
      if (l == k)
	{
	  goto L70;
	}
      t = b[l];
      b[l] = b[k];
      b[k] = t;
    L70:
      /* L80: */
      ;
    }
 L90:
 L100:
  return 0;
}

