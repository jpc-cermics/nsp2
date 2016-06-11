/* ereduc.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 *    Given an M x N matrix E (not necessarily regular) the subroutine 
 *    EREDUC computes a unitary transformed matrix Q*E*Z which is in 
 *    column echelon form (trapezoidal form). Furthermore the rank of 
 *    matrix E is determined. 
 * 
 *    Th.G.J. Beelen (Philips Glass Eindhoven).  Copyright SLICOT 
 * 
 *    REVISIONS: 1988, January 29. 
 */

static int c__1 = 1;

int
nsp_ctrlpack_ereduc (double *e, int *m, int *n, double *q, double *z__,
		     int *istair, int *ranke, double *tol)
{
  /* System generated locals */
  int e_dim1, e_offset, q_dim1, q_offset, z_dim1, z_offset, i__1, i__2;
  double d__1;

  /* Local variables */
  int i__, j, k, l;
  int lzero;
  double sc;
  int lk;
  double ss, emxnrm;
  int km1, nr1, lde, ldq, mnk;
  double emx;
  int jmx;

  /* Parameter adjustments */
  --istair;
  q_dim1 = *m;
  q_offset = q_dim1 + 1;
  q -= q_offset;
  z_dim1 = *n;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  e_dim1 = *m;
  e_offset = e_dim1 + 1;
  e -= e_offset;

  /* Function Body */
  lde = *m;
  ldq = *m;
  /* ldz = *n; */
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  q[i__ + j * q_dim1] = 0.;
	  /* L991: */
	}
    }
  i__2 = *m;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      q[i__ + i__ * q_dim1] = 1.;
      /* L992: */
    }
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      i__1 = *n;
      for (j = 1; j <= i__1; ++j)
	{
	  z__[i__ + j * z_dim1] = 0.;
	  /* L993: */
	}
    }
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      z__[i__ + i__ * z_dim1] = 1.;
      /* L994: */
    }
  *ranke = Min (*m, *n);
  /* 
   */
  k = *n;
  lzero = FALSE;
  /* 
   *    WHILE ((K > 0) AND (NOT a zero submatrix encountered) DO 
   */
 L10:
  if (k > 0 && !lzero)
    {
      /* 
       * 
       */
      mnk = *m - *n + k;
      emxnrm = 0.;
      lk = mnk;
      for (l = mnk; l >= 1; --l)
	{
	  jmx = C2F (idamax) (&k, &e[l + e_dim1], &lde);
	  emx = (d__1 = e[l + jmx * e_dim1], Abs (d__1));
	  if (emx > emxnrm)
	    {
	      emxnrm = emx;
	      lk = l;
	    }
	  /* L20: */
	}
      /* 
       */
      if (emxnrm < *tol)
	{
	  /* 
	   *          Set submatrix Ek to zero. 
	   * 
	   */
	  i__1 = k;
	  for (j = 1; j <= i__1; ++j)
	    {
	      i__2 = mnk;
	      for (l = 1; l <= i__2; ++l)
		{
		  e[l + j * e_dim1] = 0.;
		  /* L30: */
		}
	      /* L40: */
	    }
	  lzero = TRUE;
	  *ranke = *n - k;
	}
      else
	{
	  /* 
	   *          Submatrix Ek is not considered to be identically zero. 
	   *          Check whether rows have to be interchanged. 
	   * 
	   */
	  if (lk != mnk)
	    {
	      /* 
	       *             Interchange rows lk and m-n+k in whole E-matrix and 
	       *             update the row transformation matrix Q. 
	       *             (# elements involved = m) 
	       * 
	       */
	      C2F (dswap) (n, &e[lk + e_dim1], &lde, &e[mnk + e_dim1], &lde);
	      C2F (dswap) (m, &q[lk + q_dim1], &ldq, &q[mnk + q_dim1], &ldq);
	    }
	  /* 
	   */
	  km1 = k - 1;
	  i__1 = km1;
	  for (j = 1; j <= i__1; ++j)
	    {
	      /* 
	       *             Determine the column Givens transformation to annihilate 
	       *             E(m-n+k,j) using E(m-n+k,k) as pivot. 
	       *             Apply the transformation to the columns of Ek. 
	       *             (# elements involved = m-n+k) 
	       *             Update the column transformation matrix Z. 
	       *             (# elements involved = n) 
	       * 
	       */
	      nsp_ctrlpack_dgiv (&e[mnk + k * e_dim1], &e[mnk + j * e_dim1],
				 &sc, &ss);
	      C2F (drot) (&mnk, &e[k * e_dim1 + 1], &c__1,
			  &e[j * e_dim1 + 1], &c__1, &sc, &ss);
	      e[mnk + j * e_dim1] = 0.;
	      C2F (drot) (n, &z__[k * z_dim1 + 1], &c__1,
			  &z__[j * z_dim1 + 1], &c__1, &sc, &ss);
	      /* L50: */
	    }
	  /* 
	   */
	  --k;
	}
      goto L10;
    }
  /*    END WHILE 10 
   * 
   *    Initialise administration staircase form, i.e., 
   *    ISTAIR(i) =  j  if E(i,j) is a nonzero corner point 
   *              = -j  if E(i,j) is on the boundary but is no corner pt. 
   *    Thus, 
   *    ISTAIR(m-k) =   n-k           for k=0,...,rank(E)-1 
   *                = -(n-rank(E)+1)  for k=rank(E),...,m-1. 
   * 
   */
  i__1 = *ranke;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      istair[*m - i__ + 1] = *n - i__ + 1;
      /* L60: */
    }
  /* 
   */
  nr1 = *n - *ranke + 1;
  i__1 = *m - 1;
  for (i__ = *ranke; i__ <= i__1; ++i__)
    {
      istair[*m - i__] = -nr1;
      /* L70: */
    }
  return 0;
}

