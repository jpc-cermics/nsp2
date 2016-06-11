/* cbal.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* 
 * 
 *    this subroutine balances a complex matrix and isolates 
 *    eigenvalues whenever possible. 
 *
 *    subroutine cbal(nm,n,ar,ai,low,igh,scale) 
 * 
 *    int n,nm,igh,low 
 *    double precision ar(nm,n),ai(nm,n),scale(n) 
 *    int noconv 
 * 
 *    on input: 
 * 
 *       nm must be set to the row dimension of two-dimensional 
 *         array parameters as declared in the calling program 
 *         dimension statement; 
 * 
 *       n is the order of the matrix; 
 * 
 *       ar and ai contain the real and imaginary parts, 
 *         respectively, of the complex matrix to be balanced. 
 * 
 *    on output: 
 * 
 *       ar and ai contain the real and imaginary parts, 
 *         respectively, of the balanced matrix; 
 * 
 *       low and igh are two ints such that ar(i,j) and ai(i,j) 
 *         are equal to zero if 
 *          (1) i is greater than j and 
 *          (2) j=1,...,low-1 or i=igh+1,...,n; 
 * 
 *       scale contains information determining the 
 *          permutations and scaling factors used. 
 * 
 *    suppose that the principal submatrix in rows low through igh 
 *    has been balanced, that p(j) denotes the index interchanged 
 *    with j during the permutation step, and that the elements 
 *    of the diagonal matrix used are denoted by d(i,j).  then 
 *       scale(j) = p(j),    for j = 1,...,low-1 
 *                = d(j,j)       j = low,...,igh 
 *                = p(j)         j = igh+1,...,n. 
 *    the order in which the interchanges are made is n to igh+1, 
 *    then 1 to low-1. 
 * 
 *    note that 1 is returned for igh if igh is zero formally. 
 * 
 *    the algol procedure exc contained in cbalance appears in 
 *    cbal  in line.  (note that the algol roles of identifiers 
 *    k,l have been reversed.) 
 * 
 *    arithmetic is real throughout. 
 * 
 *!originator 
 *    this subroutine is a translation of the algol procedure 
 *    cbalance, which is a complex version of balance, 
 *    num. math. 13, 293-304(1969) by parlett and reinsch. 
 *    handbook for auto. comp., vol.ii-linear algebra, 315-326(1971). 
 *    questions and comments should be directed to b. s. garbow, 
 *    applied mathematics division, argonne national laboratory 
 *    :::::::::: radix is a machine dependent parameter specifying 
 *               the base of the machine floating point representation. 
 *               radix = 16.0d+0 for long form arithmetic 
 *               on s360 :::::::::: 
 */

int
nsp_ctrlpack_cbal (int *nm, int *n, double *ar, double *ai, int *low,
		   int *igh, double *scale)
{
  static double radix = 2.;
  int ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2;
  double d__1, d__2;
  int iexc;
  double c__, f, g;
  int i__, j, k, l, m;
  double r__, s, b2;
  int jj;
  int noconv;


  --scale;
  ai_dim1 = *nm;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *nm;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;

  /* Function Body */
  /* 
   */
  b2 = radix * radix;
  k = 1;
  l = *n;
  goto L100;
  /*    :::::::::: in-line procedure for row and 
   *               column exchange :::::::::: 
   */
 L20:
  scale[m] = (double) j;
  if (j == m)
    {
      goto L50;
    }
  /* 
   */
  i__1 = l;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      f = ar[i__ + j * ar_dim1];
      ar[i__ + j * ar_dim1] = ar[i__ + m * ar_dim1];
      ar[i__ + m * ar_dim1] = f;
      f = ai[i__ + j * ai_dim1];
      ai[i__ + j * ai_dim1] = ai[i__ + m * ai_dim1];
      ai[i__ + m * ai_dim1] = f;
      /* L30: */
    }
  /* 
   */
  i__1 = *n;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      f = ar[j + i__ * ar_dim1];
      ar[j + i__ * ar_dim1] = ar[m + i__ * ar_dim1];
      ar[m + i__ * ar_dim1] = f;
      f = ai[j + i__ * ai_dim1];
      ai[j + i__ * ai_dim1] = ai[m + i__ * ai_dim1];
      ai[m + i__ * ai_dim1] = f;
      /* L40: */
    }
  /* 
   */
 L50:
  switch (iexc)
    {
    case 1:
      goto L80;
    case 2:
      goto L130;
    }
  /*    :::::::::: search for rows isolating an eigenvalue 
   *               and push them down :::::::::: 
   */
 L80:
  if (l == 1)
    {
      goto L280;
    }
  --l;
  /*    :::::::::: for j=l step -1 until 1 do -- :::::::::: 
   */
 L100:
  i__1 = l;
  for (jj = 1; jj <= i__1; ++jj)
    {
      j = l + 1 - jj;
      /* 
       */
      i__2 = l;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  if (i__ == j)
	    {
	      goto L110;
	    }
	  if (ar[j + i__ * ar_dim1] != 0. || ai[j + i__ * ai_dim1] != 0.)
	    {
	      goto L120;
	    }
	L110:
	  ;
	}
      /* 
       */
      m = l;
      iexc = 1;
      goto L20;
    L120:
      ;
    }
  /* 
   */
  goto L140;
  /*    :::::::::: search for columns isolating an eigenvalue 
   *               and push them left :::::::::: 
   */
 L130:
  ++k;
  /* 
   */
 L140:
  i__1 = l;
  for (j = k; j <= i__1; ++j)
    {
      /* 
       */
      i__2 = l;
      for (i__ = k; i__ <= i__2; ++i__)
	{
	  if (i__ == j)
	    {
	      goto L150;
	    }
	  if (ar[i__ + j * ar_dim1] != 0. || ai[i__ + j * ai_dim1] != 0.)
	    {
	      goto L170;
	    }
	L150:
	  ;
	}
      /* 
       */
      m = k;
      iexc = 2;
      goto L20;
    L170:
      ;
    }
  /*    :::::::::: now balance the submatrix in rows k to l :::::::::: 
   */
  i__1 = l;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      /* L180: */
      scale[i__] = 1.;
    }
  /*    :::::::::: iterative loop for norm reduction :::::::::: 
   */
 L190:
  noconv = FALSE;
  /* 
   */
  i__1 = l;
  for (i__ = k; i__ <= i__1; ++i__)
    {
      c__ = 0.;
      r__ = 0.;
      /* 
       */
      i__2 = l;
      for (j = k; j <= i__2; ++j)
	{
	  if (j == i__)
	    {
	      goto L200;
	    }
	  c__ = c__ + (d__1 = ar[j + i__ * ar_dim1], Abs (d__1)) + (d__2 =
								    ai[j +
								       i__ *
								       ai_dim1],
								    Abs
								    (d__2));
	  r__ = r__ + (d__1 = ar[i__ + j * ar_dim1], Abs (d__1)) + (d__2 =
								    ai[i__ +
								       j *
								       ai_dim1],
								    Abs
								    (d__2));
	L200:
	  ;
	}
      /*    :::::::::: guard against zero c or r due to underflow :::::::::: 
       */
      if (c__ == 0. || r__ == 0.)
	{
	  goto L270;
	}
      g = r__ / radix;
      f = 1.;
      s = c__ + r__;
    L210:
      if (c__ >= g)
	{
	  goto L220;
	}
      f *= radix;
      c__ *= b2;
      goto L210;
    L220:
      g = r__ * radix;
    L230:
      if (c__ < g)
	{
	  goto L240;
	}
      f /= radix;
      c__ /= b2;
      goto L230;
      /*    :::::::::: now balance :::::::::: 
       */
    L240:
      if ((c__ + r__) / f >= s * .95)
	{
	  goto L270;
	}
      g = 1. / f;
      scale[i__] *= f;
      noconv = TRUE;
      /* 
       */
      i__2 = *n;
      for (j = k; j <= i__2; ++j)
	{
	  ar[i__ + j * ar_dim1] *= g;
	  ai[i__ + j * ai_dim1] *= g;
	  /* L250: */
	}
      /* 
       */
      i__2 = l;
      for (j = 1; j <= i__2; ++j)
	{
	  ar[j + i__ * ar_dim1] *= f;
	  ai[j + i__ * ai_dim1] *= f;
	  /* L260: */
	}
      /* 
       */
    L270:
      ;
    }
  /* 
   */
  if (noconv)
    {
      goto L190;
    }
  /* 
   */
 L280:
  *low = k;
  *igh = l;
  return 0;
}

