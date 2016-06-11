/* polmc.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__2 = 2;

int
nsp_ctrlpack_polmc (int *nm, int *ng, int *n, int *m, double *a, double *b,
		    double *g, double *wr, double *wi, double *z__, int *inc,
		    int *invr, int *ierr, int *jpvt, double *rm1, double *rm2,
		    double *rv1, double *rv2, double *rv3, double *rv4)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, g_dim1, g_offset, z_dim1, z_offset,
    rm1_dim1, rm1_offset, rm2_dim1, rm2_offset, i__1, i__2, i__3;
  double d__1;

  /* Builtin functions */
  double sqrt (double);

  /* Local variables */
  int i__, j, k, l;
  double p, q, r__, s, t;
  int m1, nc, ii, jj, kk, ni, nj, ip, ll, ir, mr, nr;
  double zz;
  int complx;
  int lp1, mp1, np1, mr1, nr1, kmr, inc1, inc2;

  /* 
   *!purpose 
   *    this subroutine determines the state feedback matrix  g  of the 
   *    linear time-invariant multi-input system 
   * 
   *       dx / dt = a * x + b * u, 
   * 
   *    where  a  is a  nxn  and  b  is a  nxm  matrix, such that the 
   *    closed-loop system 
   * 
   *       dx / dt = (a - b * g) * x 
   * 
   *    has desired poles. the system must be preliminary reduced into 
   *    orthogonal canonical form using the subroutine  trmcf. 
   *!calling sequence 
   * 
   *    subroutine polmc(nm,ng,n,m,a,b,g,wr,wi,z,inc,invr,ierr,jpvt, 
   *   x                  rm1,rm2,rv1,rv2,rv3,rv4) 
   * 
   *    on input- 
   * 
   *       nm    is an int variable set equal to the row dimension 
   *             of the two-dimensional arrays  a,  b  and  z  as 
   *             specified in the dimension statements for  a,  b  and  z 
   *             in the calling program, 
   * 
   *       ng    is an int variable set equal to the row dimension 
   *             of the two-dimensional array  g  as specified in the 
   *             dimension statement for  g  in the calling program, 
   * 
   *       n     is an int variable set equal to the order of the 
   *             matrices  a  and  z.  n  must be not greater than  nm, 
   * 
   *       m     is an int variable set equal to the number of the 
   *             columns of the matrix  b.  m  must be not greater than 
   *             ng, 
   * 
   *       a     is a working precision real two-dimensional variable with 
   *             row dimension  nm  and column dimension at least  n 
   *             containing the block-hessenberg canonical form of the 
   *             matrix  a.  the elements below the subdiagonal blocks 
   *             must be equal to zero, 
   * 
   *       b     is a working precision real two-dimensional variable with 
   *             row dimension  nm  and column dimension at least  m 
   *             containing the canonical form of the matrix  b. the 
   *             elements below the  invr(1)-th row must be equal to zero, 
   * 
   *       wr,wi are working precision real one-dimensional variables 
   *             of dimension at least  n  containing the real and 
   *             imaginery parts, respectively, of the desired poles, 
   *             the poles can be unordered except that the complex 
   *             conjugate pairs of poles must appfar consecutively. 
   *             note that on output the imaginery parts of the poles 
   *             may be modified, 
   * 
   *       z     is a working precision real two-dimensonal variale with 
   *             row dimension  nm  and column dimension at least  n 
   *             containing the orthogonal transformation matrix produced 
   *             in  trmcf  which reduces the system into canonical form, 
   * 
   *       inc   is an int variable set equal to the controllability 
   *             index of the system, 
   * 
   *       invr  is an int one-dimensional variable of dimension at 
   *             least  inc  containing the dimensons of the 
   *             controllable subsystems in the canonical form. 
   * 
   *    on output- 
   * 
   *       a     contains the upper quast-triangular form of the closed- 
   *             loop system matrix  a - b * g,  that is triangular except 
   *             of possible  2x2  blocks on the diagonal, 
   * 
   *       b     contains the transformed matrix  b, 
   * 
   *       g     is a working precision real two-dimensional variable with 
   *             row dimension  ng  and column dimension at least  n 
   *             containing the state feedback matrix  g  of the original 
   *             system, 
   * 
   *       z     contains the orthogonal matrix which reduces the closed- 
   *             loop system matrix  a - b * g  to the upper quasi- 
   *             triangular form, 
   * 
   *       ierr  is an int variable set equal to 
   *             zero  for normal return, 
   *             1     if the system is not completely controllable, 
   * 
   *       jpvt  is an int temporary one-dimensonal array of 
   *             dimension at least  m  used in the solution of linear 
   *             equations, 
   * 
   *       rm1   is a working precision real temporary two-dimensonal 
   *             array of dimension at least  mxm  used in the solution 
   *             of linear equations, 
   * 
   *       rm2   is a working precision real temporary two-dimensional 
   *             array od dimension at least  mxmax(2,m)  used in the 
   *             solution of linear equations, 
   * 
   *       rv1,  are working precision real temporary one-dimensional 
   *         rv2 arrays of dimension at least  n  used to hold the 
   *             real and imaginery parts, respectively, of the 
   *             eigenvectors during the reduction, 
   * 
   *       rv3,  are working precision real temporary one-dimensional 
   *         rv4 arrays of dimension at least  m  used in the solution 
   *             of linear equations. 
   * 
   *!auxiliary routines 
   * 
   *       sqrsm 
   *       fortran  abs,min,sqrt 
   *!originator 
   *    p.hr.petkov, higher institute of mechanical and electrical 
   *    engineering, sofia, bulgaria. 
   *    modified by serge Steer INRIA 
   *    Copyright SLICOT 
   *! 
   * 
   */
  /* Parameter adjustments */
  --rv2;
  --rv1;
  --invr;
  z_dim1 = *nm;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  --wi;
  --wr;
  g_dim1 = *ng;
  g_offset = g_dim1 + 1;
  g -= g_offset;
  a_dim1 = *nm;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --rv4;
  --rv3;
  rm2_dim1 = *m;
  rm2_offset = rm2_dim1 + 1;
  rm2 -= rm2_offset;
  rm1_dim1 = *m;
  rm1_offset = rm1_dim1 + 1;
  rm1 -= rm1_offset;
  --jpvt;
  b_dim1 = *nm;
  b_offset = b_dim1 + 1;
  b -= b_offset;

  /* Function Body */
  *ierr = 0;
  m1 = invr[1];
  l = 0;
 L10:
  ++l;
  mr = invr[*inc];
  if (*inc == 1)
    {
      goto L350;
    }
  lp1 = l + m1;
  inc1 = *inc - 1;
  mr1 = invr[inc1];
  nr = *n - mr + 1;
  nr1 = nr - mr1;
  complx = wi[l] != 0.;
  i__1 = *n;
  for (i__ = nr; i__ <= i__1; ++i__)
    {
      rv1[i__] = 0.;
      if (complx)
	{
	  rv2[i__] = 0.;
	}
      /* L15: */
    }
  /* 
   */
  rv1[nr] = 1.;
  if (!complx)
    {
      goto L20;
    }
  if (mr == 1)
    {
      rv2[nr] = 1.;
    }
  if (mr > 1)
    {
      rv2[nr + 1] = 1.;
    }
  t = wi[l];
  wi[l] = 1.;
  wi[l + 1] = t * wi[l + 1];
  /* 
   *      compute and transform eigenvector 
   * 
   */
 L20:
  i__1 = *inc;
  for (ip = 1; ip <= i__1; ++ip)
    {
      if (ip == *inc && *inc == 2)
	{
	  goto L200;
	}
      if (ip == *inc)
	{
	  goto L120;
	}
      /* 
       */
      i__2 = mr;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  i__ = nr + ii - 1;
	  /* 
	   */
	  i__3 = mr1;
	  for (jj = 1; jj <= i__3; ++jj)
	    {
	      j = nr1 + jj - 1;
	      rm1[ii + jj * rm1_dim1] = a[i__ + j * a_dim1];
	      /* L30: */
	    }
	  /* 
	   */
	  /* L40: */
	}
      /* 
       */
      if (ip == 1)
	{
	  goto L70;
	}
      /* 
       *         scaling 
       * 
       */
      s = 0.;
      mp1 = mr + 1;
      np1 = nr + mp1;
      /* 
       */
      i__2 = mp1;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  i__ = nr + ii - 1;
	  s += (d__1 = rv1[i__], Abs (d__1));
	  if (complx)
	    {
	      s += (d__1 = rv2[i__], Abs (d__1));
	    }
	  /* L50: */
	}
      /* 
       */
      i__2 = mp1;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  i__ = nr + ii - 1;
	  rv1[i__] /= s;
	  if (complx)
	    {
	      rv2[i__] /= s;
	    }
	  /* L60: */
	}
      /* 
       */
      if (complx && np1 <= *n)
	{
	  rv2[np1] /= s;
	}
    L70:
      if (ip == 1)
	{
	  mp1 = 1;
	}
      np1 = nr + mp1;
      /* 
       */
      i__2 = mr;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  i__ = nr + ii - 1;
	  s = wr[l] * rv1[i__];
	  /* 
	   */
	  i__3 = mp1;
	  for (jj = 1; jj <= i__3; ++jj)
	    {
	      j = nr + jj - 1;
	      s -= a[i__ + j * a_dim1] * rv1[j];
	      /* L80: */
	    }
	  /* 
	   */
	  rm2[ii + rm2_dim1] = s;
	  if (!complx)
	    {
	      goto L100;
	    }
	  rm2[ii + rm2_dim1] += wi[l + 1] * rv2[i__];
	  s = wr[l + 1] * rv2[i__] + wi[l] * rv1[i__];
	  /* 
	   */
	  i__3 = mp1;
	  for (jj = 1; jj <= i__3; ++jj)
	    {
	      /*la ligne suivante a ete rajoutee par mes soins 
	       */
	      j = nr + jj - 1;
	      s -= a[i__ + j * a_dim1] * rv2[j];
	      /* L90: */
	    }
	  /* 
	   */
	  if (np1 <= *n)
	    {
	      s -= a[i__ + np1 * a_dim1] * rv2[np1];
	    }
	  rm2[ii + (rm2_dim1 << 1)] = s;
	L100:
	  ;
	}
      /* 
       *         solving linear equations for the eigenvector elements 
       * 
       */
      nc = 1;
      if (complx)
	{
	  nc = 2;
	}
      nsp_ctrlpack_dqrsm (&rm1[rm1_offset], m, &mr, &mr1, &rm2[rm2_offset], m,
			  &nc, &rm2[rm2_offset], m, &ir, &jpvt[1], &rv3[1],
			  &rv4[1]);
      if (ir < mr)
	{
	  goto L600;
	}
      /* 
       */
      i__2 = mr1;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  i__ = nr1 + ii - 1;
	  rv1[i__] = rm2[ii + rm2_dim1];
	  if (complx)
	    {
	      rv2[i__] = rm2[ii + (rm2_dim1 << 1)];
	    }
	  /* L110: */
	}
      /* 
       */
      if (ip == 1 && *inc > 2)
	{
	  goto L195;
	}
    L120:
      nj = nr;
      if (ip < *inc)
	{
	  nj = nr1;
	}
      ni = nr + mr - 1;
      inc2 = *inc - ip + 2;
      if (ip > 1)
	{
	  ni += invr[inc2];
	}
      if (ip > 2)
	{
	  ++ni;
	}
      if (complx && ip > 2)
	{
	  /*Computing MIN 
	   */
	  i__2 = ni + 1;
	  ni = Min (i__2, *n);
	}
      kmr = mr1;
      if (ip > 1)
	{
	  kmr = mr;
	}
      /* 
       */
      i__2 = kmr;
      for (kk = 1; kk <= i__2; ++kk)
	{
	  ll = 1;
	  k = nr + mr - kk;
	  if (ip == 1)
	    {
	      k = nr - kk;
	    }
	L130:
	  p = rv1[k];
	  if (ll == 2)
	    {
	      p = rv2[k];
	    }
	  q = rv1[k + 1];
	  if (ll == 2)
	    {
	      q = rv2[k + 1];
	    }
	  s = Abs (p) + Abs (q);
	  p /= s;
	  q /= s;
	  r__ = sqrt (p * p + q * q);
	  t = s * r__;
	  rv1[k] = t;
	  if (ll == 2)
	    {
	      rv2[k] = t;
	    }
	  rv1[k + 1] = 0.;
	  if (ll == 2)
	    {
	      rv2[k + 1] = 0.;
	    }
	  p /= r__;
	  q /= r__;
	  /* 
	   *            transform  a 
	   * 
	   */
	  i__3 = *n;
	  for (j = nj; j <= i__3; ++j)
	    {
	      zz = a[k + j * a_dim1];
	      a[k + j * a_dim1] = p * zz + q * a[k + 1 + j * a_dim1];
	      a[k + 1 + j * a_dim1] = p * a[k + 1 + j * a_dim1] - q * zz;
	      /* L140: */
	    }
	  /* 
	   */
	  i__3 = ni;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      zz = a[i__ + k * a_dim1];
	      a[i__ + k * a_dim1] = p * zz + q * a[i__ + (k + 1) * a_dim1];
	      a[i__ + (k + 1) * a_dim1] =
		p * a[i__ + (k + 1) * a_dim1] - q * zz;
	      /* L150: */
	    }
	  /* 
	   */
	  if ((k == lp1 && ll == 1) || k > lp1)
	    {
	      goto L170;
	    }
	  /* 
	   *       transform b 
	   * 
	   */
	  i__3 = *m;
	  for (j = 1; j <= i__3; ++j)
	    {
	      zz = b[k + j * b_dim1];
	      b[k + j * b_dim1] = p * zz + q * b[k + 1 + j * b_dim1];
	      b[k + 1 + j * b_dim1] = p * b[k + 1 + j * b_dim1] - q * zz;
	      /* L160: */
	    }
	  /* 
	   *            accumulate transformations 
	   * 
	   */
	L170:
	  i__3 = *n;
	  for (i__ = 1; i__ <= i__3; ++i__)
	    {
	      zz = z__[i__ + k * z_dim1];
	      z__[i__ + k * z_dim1] =
		p * zz + q * z__[i__ + (k + 1) * z_dim1];
	      z__[i__ + (k + 1) * z_dim1] =
		p * z__[i__ + (k + 1) * z_dim1] - q * zz;
	      /* L180: */
	    }
	  /* 
	   */
	  if (!complx || ll == 2)
	    {
	      goto L190;
	    }
	  zz = rv2[k];
	  rv2[k] = p * zz + q * rv2[k + 1];
	  rv2[k + 1] = p * rv2[k + 1] - q * zz;
	  if (k + 2 > *n)
	    {
	      goto L190;
	    }
	  ++k;
	  ll = 2;
	  goto L130;
	L190:
	  ;
	}
      /* 
       */
      if (ip == *inc)
	{
	  goto L200;
	}
    L195:
      mr = mr1;
      nr = nr1;
      if (ip == inc1)
	{
	  goto L200;
	}
      inc2 = *inc - ip - 1;
      mr1 = invr[inc2];
      nr1 -= mr1;
    L200:
      ;
    }
  /* 
   */
  if (complx)
    {
      goto L250;
    }
  /* 
   *       find one column of  g 
   * 
   */
  i__1 = m1;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = l + ii;
      /* 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L210: */
	  rm1[ii + j * rm1_dim1] = b[i__ + j * b_dim1];
	}
      /* 
       */
      rm2[ii + rm2_dim1] = a[i__ + l * a_dim1];
      /* L220: */
    }
  /* 
   */
  nsp_ctrlpack_dqrsm (&rm1[rm1_offset], m, &m1, m, &rm2[rm2_offset], m, &c__1,
		      &g[l * g_dim1 + 1], ng, &ir, &jpvt[1], &rv3[1],
		      &rv4[1]);
  if (ir < m1)
    {
      goto L600;
    }
  /* 
   */
  i__1 = lp1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L230: */
	  a[i__ + l * a_dim1] -= b[i__ + j * b_dim1] * g[j + l * g_dim1];
	}
      /* 
       */
      /* L240: */
    }
  /* 
   */
  goto L330;
  /* 
   *       find two columns of  g 
   * 
   */
 L250:
  ++l;
  if (lp1 < *n)
    {
      ++lp1;
    }
  /* 
   */
  i__1 = m1;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = l + ii;
      if (l + m1 > *n)
	{
	  --i__;
	}
      /* 
       *la ligne suivante a ete rajoutee par mes soins 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  /*xxx        if(Abs(b(i,j)).le.abs(b(l,j))) i=i-1 
	   */
	  /* L260: */
	  rm1[ii + j * rm1_dim1] = b[i__ + j * b_dim1];
	}
      /* 
       */
      p = a[i__ + (l - 1) * a_dim1];
      if (i__ == l)
	{
	  p -= rv2[i__] / rv1[i__ - 1] * wi[i__];
	}
      rm2[ii + rm2_dim1] = p;
      q = a[i__ + l * a_dim1];
      if (i__ == l)
	{
	  q = q - wr[i__] + rv2[i__ - 1] / rv1[i__ - 1] * wi[i__];
	}
      rm2[ii + (rm2_dim1 << 1)] = q;
      /* L270: */
    }
  /* 
   */
  nsp_ctrlpack_dqrsm (&rm1[rm1_offset], m, &m1, m, &rm2[rm2_offset], m, &c__2,
		      &rm2[rm2_offset], m, &ir, &jpvt[1], &rv3[1], &rv4[1]);
  if (ir < m1)
    {
      goto L600;
    }
  /* 
   */
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      for (jj = 1; jj <= 2; ++jj)
	{
	  j = l + jj - 2;
	  g[i__ + j * g_dim1] = rm2[i__ + jj * rm2_dim1];
	  /* L280: */
	}
      /* 
       */
      /* L290: */
    }
  /* 
   */
  i__1 = lp1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      for (jj = 1; jj <= 2; ++jj)
	{
	  j = l + jj - 2;
	  /* 
	   */
	  i__2 = *m;
	  for (k = 1; k <= i__2; ++k)
	    {
	      /* L300: */
	      a[i__ + j * a_dim1] -= b[i__ + k * b_dim1] * g[k + j * g_dim1];
	    }
	  /* 
	   */
	  /* L310: */
	}
      /* 
       */
      /* L320: */
    }
  /* 
   */
  if (l == *n)
    {
      goto L500;
    }
 L330:
  --invr[*inc];
  if (invr[*inc] == 0)
    {
      --(*inc);
    }
  if (complx)
    {
      --invr[*inc];
    }
  if (invr[*inc] == 0)
    {
      --(*inc);
    }
  goto L10;
  /* 
   *      find the rest columns of  g 
   * 
   */
 L350:
  i__1 = mr;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = l + ii - 1;
      /* 
       */
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L360: */
	  rm1[ii + j * rm1_dim1] = b[i__ + j * b_dim1];
	}
      /* 
       */
      /* L370: */
    }
  /* 
   */
  i__1 = mr;
  for (ii = 1; ii <= i__1; ++ii)
    {
      i__ = l + ii - 1;
      /* 
       */
      i__2 = mr;
      for (jj = 1; jj <= i__2; ++jj)
	{
	  j = l + jj - 1;
	  if (ii < jj)
	    {
	      rm2[ii + jj * rm2_dim1] = 0.;
	    }
	  if (ii > jj)
	    {
	      rm2[ii + jj * rm2_dim1] = a[i__ + j * a_dim1];
	    }
	  /* L380: */
	}
      /* 
       */
      /* L400: */
    }
  /* 
   */
  ii = 0;
 L410:
  ++ii;
  i__ = l + ii - 1;
  if (wi[i__] != 0.)
    {
      goto L420;
    }
  rm2[ii + ii * rm2_dim1] = a[i__ + i__ * a_dim1] - wr[i__];
  if (ii == mr)
    {
      goto L430;
    }
  /*la ligne suivante a ete rajoutee par mes soins 
   */
  goto L410;
 L420:
  rm2[ii + ii * rm2_dim1] = a[i__ + i__ * a_dim1] - wr[i__];
  rm2[ii + (ii + 1) * rm2_dim1] = a[i__ + (i__ + 1) * a_dim1] - wi[i__];
  rm2[ii + 1 + ii * rm2_dim1] = a[i__ + 1 + i__ * a_dim1] - wi[i__ + 1];
  rm2[ii + 1 + (ii + 1) * rm2_dim1] =
    a[i__ + 1 + (i__ + 1) * a_dim1] - wr[i__ + 1];
  ++ii;
  if (ii < mr)
    {
      goto L410;
    }
 L430:
  nsp_ctrlpack_dqrsm (&rm1[rm1_offset], m, &mr, m, &rm2[rm2_offset], m, m,
		      &rm2[rm2_offset], m, &ir, &jpvt[1], &rv3[1], &rv4[1]);
  if (ir < mr)
    {
      goto L600;
    }
  /* 
   */
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = mr;
      for (jj = 1; jj <= i__2; ++jj)
	{
	  j = l + jj - 1;
	  g[i__ + j * g_dim1] = rm2[i__ + jj * rm2_dim1];
	  /* L440: */
	}
      /* 
       */
      /* L450: */
    }
  /* 
   */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *n;
      for (j = l; j <= i__2; ++j)
	{
	  /* 
	   */
	  i__3 = *m;
	  for (k = 1; k <= i__3; ++k)
	    {
	      /* L460: */
	      a[i__ + j * a_dim1] -= b[i__ + k * b_dim1] * g[k + j * g_dim1];
	    }
	  /* 
	   */
	  /* L470: */
	}
      /* 
       */
      /* L480: */
    }
  /* 
   *    transform  g 
   * 
   */
 L500:
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      /* 
       */
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  s = 0.;
	  /* 
	   */
	  i__3 = *n;
	  for (k = 1; k <= i__3; ++k)
	    {
	      /* L510: */
	      s += g[i__ + k * g_dim1] * z__[j + k * z_dim1];
	    }
	  /* 
	   */
	  rv1[j] = s;
	  /* L520: */
	}
      /* 
       */
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  /* L530: */
	  g[i__ + j * g_dim1] = rv1[j];
	}
      /* 
       */
      /* L540: */
    }
  /* 
   */
  goto L610;
  /* 
   *    set error -- the system is not completely controllable 
   * 
   */
 L600:
  *ierr = 1;
 L610:
  return 0;
  /* 
   *    last card of subroutine polmc 
   * 
   */
}				/* polmc_ */
