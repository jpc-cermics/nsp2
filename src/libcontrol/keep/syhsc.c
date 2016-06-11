/* syhsc.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__11 = 11;
static int c__0 = 0;

int
nsp_ctrlpack_syhsc (int *n, int *m, double *a, int *na, double *b, int *mb,
		    double *c__, double *z__, double *eps, double *wrk1,
		    int *nwrk1, double *wrk2, int *nwrk2, int *iwrk,
		    int *niwrk, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, z_dim1, z_offset,
    i__1, i__2;
  double d__1;

  /* Local variables */
  double reps, swop;
  int i__, j;
  double t[1];
  int ind;

  /* 
   *! calling sequence 
   *       subroutine syhsc(n,m,a,na,b,mb,c,z,wrk1,nwrk1,wrk2,nwrk2, 
   *    +                   iwrk,niwrk,ierr) 
   * 
   *       int n,m,na,mb,nwrk1,nwrk2,niwrk,ierr 
   *       int iwrk(niwrk) 
   *       double precision a(na,n),b(mb,m),c(na,m) 
   *       double precision z(mb,m),wrk1(nwrk1),wrk2(nwrk2) 
   * 
   *arguments in 
   * 
   *       n       int 
   *               -order of the matrix  a. 
   * 
   *       m       int 
   *               -order of the matrix  b. 
   * 
   *       a       double precision(n,n) 
   *               -the coefficient matrix  a  of the equation. 
   *               n.b.  a  is overwritten by this routine. 
   * 
   *       na      int 
   *               -the declared first dimension of  a and c . 
   * 
   *       b       double precision(m,m) 
   *               -the coefficient matrix  b  of the equation. 
   *               n.b.  b  is overwritten by this routine. 
   * 
   *       mb      int 
   *               -the declared first dimension of  b . 
   * 
   *       c       double precision(n,m) 
   *               -the coefficient matrix  c  of the equation. 
   *               n.b.  c  is overwritten by this routine. 
   * 
   *       nwrk1   int 
   *               -the length of the internal vector  wrk1 
   *               nwrk1  must be at least  2*n**2  +  7*n 
   * 
   *       nwrk2   int 
   *               -the length of the internal vector  wrk2 
   *               nwrk2  must be at least  Max(n,m) 
   * 
   *       niwrk   int 
   *               -the length of the internal vector  iwrk 
   *               niwrk  must be at least  4*n 
   * 
   *arguments out 
   * 
   *       c       double precision(n,m) 
   *               -on return, the solution matrix, x  is contained in c 
   * 
   *       z       double precision(m,m) 
   *               -on return, z  contains the orthogonal matrix used 
   *               to transform  transpose(b) to real upper schur form. 
   * 
   *       ierr    int 
   *               -error indicator 
   * 
   *               ierr = 0        successful return 
   * 
   *               ierr in (1,m   ierr-th eigenvalue of  b  has not been 
   *                               determined after 30 iterations. 
   * 
   *               ierr > m   a singular matrix was encountered whilst 
   *                          solving for the (ierr-m)th column of  x 
   * 
   *!working space 
   * 
   *       wrk1    double precision(nwrk1) 
   *               -where  nwrk1 .ge. 2*n**2 + 7*n 
   * 
   *       wrk2    double precision(nwrk2) 
   *               -where  nwrk2 .ge. Max(n,m) 
   * 
   *       iwrk    int(niwrk) 
   *               -where  niwrk .ge. 4*n 
   * 
   *!purpose 
   * 
   *       to solve the continuous-time sylvester equation 
   *               ax + xb = c 
   * 
   *!method 
   * 
   *       a  is transformed to upper hessenberg form, and the transpose 
   *       of  b is transformed to real upper schur form using orthogonal 
   *       transformations. the matrix  c  is also multiplied by these 
   *       transformation matrices, and the solution of this transformed 
   *       system is computed. this solution is then multiplied by the 
   *       transformation matrices in order to obtain the solution to 
   *       the original problem. 
   * 
   *!reference 
   * 
   *       g.golub, s.nash, and c.f.vanloan," a hessenberg-schur method 
   *       for the problem  ax + xb = c ",ieee trans. auto. control, 
   *       vol. ac-24, no. 6, pp. 909-912 (1979). 
   * 
   *!auxiliary routines 
   * 
   *      orthes,ortran (eispack) 
   *      hqror2,transf,nsolve,hesolv,backsb,n2solv,h2solv,backs2 
   *      irow1,irow2,lrow2 
   * 
   *!origin: 
   * 
   *               g.golub,s.nash,c.van loan, dept.comp.sci.,stanford 
   *               university,california                january 1982 
   * 
   *! 
   ********************************************************************* 
   * 
   * 
   * 
   */
  /* Parameter adjustments */
  c_dim1 = *na;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  z_dim1 = *mb;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  b_dim1 = *mb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  --wrk1;
  --wrk2;
  --iwrk;

  /* Function Body */
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = *m;
      for (j = i__; j <= i__2; ++j)
	{
	  swop = b[i__ + j * b_dim1];
	  b[i__ + j * b_dim1] = b[j + i__ * b_dim1];
	  b[j + i__ * b_dim1] = swop;
	  /* L5: */
	}
    }
  /* 
   */
  nsp_ctrlpack_orthes (mb, m, &c__1, m, &b[b_offset], &wrk2[1]);
  nsp_ctrlpack_ortran (mb, m, &c__1, m, &b[b_offset], &wrk2[1],
		       &z__[z_offset]);
  nsp_ctrlpack_hqror2 (mb, m, &c__1, m, &b[b_offset], t, t, &z__[z_offset],
		       ierr, &c__11);
  /* 
   */
  nsp_ctrlpack_orthes (na, n, &c__1, n, &a[a_offset], &wrk2[1]);
  /* 
   */
  nsp_ctrlpack_transf (&a[a_offset], &wrk2[1], &c__1, &c__[c_offset],
		       &z__[z_offset], &c__0, n, m, na, mb, &wrk1[1], nwrk1);
  /* 
   */
  reps = *eps * *m * *m * *n * *n;
  ind = *m - 1;
  if (ind == 0)
    {
      goto L40;
    }
 L10:
  if ((d__1 = b[ind + 1 + ind * b_dim1], Abs (d__1)) <= reps)
    {
      goto L20;
    }
  /* 
   */
  nsp_ctrlpack_n2solv (&a[a_offset], &b[b_offset], &c__[c_offset], &wrk1[1],
		       nwrk1, mb, m, na, n, &ind, &iwrk[1], niwrk, &reps,
		       ierr);
  /* 
   */
  if (*ierr != 0)
    {
      return 0;
    }
  goto L30;
  /* 
   */
 L20:
  nsp_ctrlpack_nsolve (&a[a_offset], &b[b_offset], &c__[c_offset], &wrk1[1],
		       nwrk1, mb, m, na, n, &ind, &iwrk[1], niwrk, &reps,
		       ierr);
  /* 
   */
  if (*ierr != 0)
    {
      return 0;
    }
 L30:
  if (ind > 0)
    {
      goto L10;
    }
  /* 
   */
 L40:
  if (ind == 0)
    {
      nsp_ctrlpack_nsolve (&a[a_offset], &b[b_offset], &c__[c_offset],
			   &wrk1[1], nwrk1, mb, m, na, n, &ind, &iwrk[1],
			   niwrk, &reps, ierr);
    }
  /* 
   */
  nsp_ctrlpack_transf (&a[a_offset], &wrk2[1], &c__0, &c__[c_offset],
		       &z__[z_offset], &c__1, n, m, na, mb, &wrk1[1], nwrk1);
  /* 
   */
  return 0;
}				/* syhsc_ */

int
nsp_ctrlpack_transf (double *a, double *ort, int *it1, double *c__, double *v,
		     int *it2, int *m, int *n, int *mdim, int *ndim,
		     double *d__, int *nwrk1)
{
  /* System generated locals */
  int v_dim1, v_offset, c_dim1, c_offset, a_dim1, a_offset, i__1, i__2, i__3;

  /* Local variables */
  double g;
  int i__, j, k, k1, k2, m2, kk;

  /*! 
   */
  /* Parameter adjustments */
  --ort;
  c_dim1 = *mdim;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *mdim;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  v_dim1 = *ndim;
  v_offset = v_dim1 + 1;
  v -= v_offset;
  --d__;

  /* Function Body */
  m2 = *m - 2;
  if (m2 <= 0)
    {
      goto L45;
    }
  i__1 = m2;
  for (kk = 1; kk <= i__1; ++kk)
    {
      k = m2 - kk + 1;
      if (*it1 == 1)
	{
	  k = kk;
	}
      k1 = k + 1;
      if (a[k1 + k * a_dim1] == 0.)
	{
	  goto L40;
	}
      d__[k1] = ort[k1];
      k2 = k + 2;
      i__2 = *m;
      for (i__ = k2; i__ <= i__2; ++i__)
	{
	  d__[i__] = a[i__ + k * a_dim1];
	  /* L10: */
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  g = 0.;
	  i__3 = *m;
	  for (i__ = k1; i__ <= i__3; ++i__)
	    {
	      g += d__[i__] * c__[i__ + j * c_dim1];
	      /* L20: */
	    }
	  g = g / ort[k1] / a[k1 + k * a_dim1];
	  i__3 = *m;
	  for (i__ = k1; i__ <= i__3; ++i__)
	    {
	      c__[i__ + j * c_dim1] += g * d__[i__];
	      /* L30: */
	    }
	}
    L40:
      ;
    }
 L45:
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__3 = *n;
      for (j = 1; j <= i__3; ++j)
	{
	  d__[j] = 0.;
	  i__2 = *n;
	  for (k = 1; k <= i__2; ++k)
	    {
	      if (*it2 == 0)
		{
		  d__[j] += c__[i__ + k * c_dim1] * v[k + j * v_dim1];
		}
	      if (*it2 == 1)
		{
		  d__[j] += c__[i__ + k * c_dim1] * v[j + k * v_dim1];
		}
	      /* L50: */
	    }
	}
      i__2 = *n;
      for (j = 1; j <= i__2; ++j)
	{
	  c__[i__ + j * c_dim1] = d__[j];
	  /* L60: */
	}
    }
  return 0;
}				/* transf_ */

int
nsp_ctrlpack_nsolve (double *a, double *b, double *c__, double *d__,
		     int *nwrk1, int *ndim, int *n, int *mdim, int *m,
		     int *ind, int *ipr, int *niwrk, double *reps, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2;

  /* Local variables */
  int mfin;
  int i__, j, i1, m1;
  int ip;

  /*% calling sequence 
   *      subroutine nsolve(a,b,c,d,nwrk1,ndim,n,mdim,m,ind,ipr,niwrk, 
   *    +           reps,ierr) 
   *      int nwrk1,niwrk 
   *      int i,i1,ierr,ind,ipr(niwrk),irow1,j,m,m1,mdim,n,ndim,mfin 
   *      double precision a(mdim,m),b(ndim,n),c(mdim,n),d(nwrk1),reps 
   *% purpose 
   *     this routine is only to be called from syhsc 
   *% 
   * 
   */
  /* Parameter adjustments */
  --d__;
  b_dim1 = *ndim;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  c_dim1 = *mdim;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *mdim;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipr;

  /* Function Body */
  if (*ind < *n - 1)
    {
      nsp_ctrlpack_backsb (&c__[c_offset], &b[b_offset], ind, n, m, mdim,
			   ndim);
    }
  /* 
   */
  mfin = *m * (*m + 1) / 2 + *m;
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      m1 = nsp_ctrlpack_irow1 (&i__, m);
      i1 = i__ - 1;
      if (i__ == 1)
	{
	  i1 = 1;
	}
      i__2 = *m;
      for (j = i1; j <= i__2; ++j)
	{
	  ip = m1 + j - i1 + 1;
	  d__[ip] = a[i__ + j * a_dim1];
	  /* L10: */
	}
      j = m1 + 2;
      if (i__ == 1)
	{
	  --j;
	}
      d__[j] += b[*ind + 1 + (*ind + 1) * b_dim1];
      ip = mfin + i__;
      d__[ip] = c__[i__ + (*ind + 1) * c_dim1];
      /* L20: */
    }
  /* 
   */
  nsp_ctrlpack_hesolv (&d__[1], nwrk1, &ipr[1], niwrk, m, reps, ierr);
  /* 
   */
  if (*ierr != 0)
    {
      goto L40;
    }
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ip = ipr[i__];
      c__[i__ + (*ind + 1) * c_dim1] = d__[ip];
      /* L30: */
    }
  --(*ind);
  return 0;
 L40:
  *ierr = *n + *ind - 1;
  return 0;
}				/* nsolve_ */

int
nsp_ctrlpack_hesolv (double *d__, int *nwrk1, int *ipr, int *niwrk, int *m,
		     double *reps, int *ierr)
{
  /* System generated locals */
  int i__1, i__2;
  double d__1, d__2;

  /* Local variables */
  int mfin;
  double mult;
  int i__, j, k, i1, i2, j1, m1, ip, ipi, ipl;

  /* Parameter adjustments */
  --d__;
  --ipr;

  /* Function Body */
  *ierr = 0;
  mfin = *m * (*m + 1) / 2 + *m;
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ip = *m + i__;
      ipr[ip] = nsp_ctrlpack_irow1 (&i__, m);
      ipr[i__] = i__ + mfin;
      /* L10: */
    }
  m1 = *m - 1;
  if (*m == 1)
    {
      goto L35;
    }
  i__1 = m1;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ip = *m + i__;
      ipl = ipr[ip];
      ipi = ipr[ip + 1];
      if ((d__1 = d__[ipl + 1], Abs (d__1)) > (d__2 =
					       d__[ipi + 1], Abs (d__2)))
	{
	  goto L20;
	}
      ipr[ip] = ipr[ip + 1];
      ipr[ip + 1] = ipl;
      k = ipr[i__];
      ipr[i__] = ipr[i__ + 1];
      ipr[i__ + 1] = k;
    L20:
      if ((d__1 = d__[ipr[*m + i__] + 1], Abs (d__1)) < *reps)
	{
	  goto L60;
	}
      ++ipr[*m + i__ + 1];
      mult = d__[ipr[*m + i__ + 1]] / d__[ipr[*m + i__] + 1];
      d__[ipr[i__ + 1]] -= mult * d__[ipr[i__]];
      i1 = i__ + 1;
      i__2 = *m;
      for (j = i1; j <= i__2; ++j)
	{
	  d__[ipr[*m + i__ + 1] + j - i__] -=
	    mult * d__[ipr[*m + i__] + j + 1 - i__];
	  /* L30: */
	}
    }
 L35:
  if ((d__1 = d__[ipr[*m + *m] + 1], Abs (d__1)) < *reps)
    {
      goto L60;
    }
  d__[ipr[*m]] /= d__[ipr[*m + *m] + 1];
  if (m1 == 0)
    {
      return 0;
    }
  i__2 = m1;
  for (i1 = 1; i1 <= i__2; ++i1)
    {
      i__ = *m - i1;
      i2 = i__ + 1;
      mult = 0.;
      i__1 = *m;
      for (j1 = i2; j1 <= i__1; ++j1)
	{
	  j = j1 - i2 + 2;
	  mult += d__[ipr[j1]] * d__[ipr[*m + i__] + j];
	  /* L40: */
	}
      d__[ipr[i__]] = (d__[ipr[i__]] - mult) / d__[ipr[*m + i__] + 1];
      /* L50: */
    }
  return 0;
 L60:
  *ierr = -1;
  return 0;
}				/* hesolv_ */

int
nsp_ctrlpack_backsb (double *c__, double *b, int *ind, int *n, int *m,
		     int *mdim, int *ndim)
{
  /* System generated locals */
  int b_dim1, b_offset, c_dim1, c_offset, i__1, i__2;

  /* Local variables */
  int i__, j, ind1, ind2;

  /* Parameter adjustments */
  c_dim1 = *mdim;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  b_dim1 = *ndim;
  b_offset = b_dim1 + 1;
  b -= b_offset;

  /* Function Body */
  ind1 = *ind + 1;
  ind2 = *ind + 2;
  i__1 = *n;
  for (i__ = ind2; i__ <= i__1; ++i__)
    {
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  c__[j + ind1 * c_dim1] -=
	    b[ind1 + i__ * b_dim1] * c__[j + i__ * c_dim1];
	  /* L10: */
	}
    }
  return 0;
}				/* backsb_ */

int
nsp_ctrlpack_n2solv (double *a, double *b, double *c__, double *d__,
		     int *nwrk1, int *ndim, int *n, int *mdim, int *m,
		     int *ind, int *ipr, int *niwrk, double *reps, int *ierr)
{
  /* System generated locals */
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset, i__1, i__2;

  /* Local variables */
  int mfin;
  int i__, j, k, i1, j1, j2, m1, m2;
  int ip;

  /*% calling sequence 
   *      subroutine n2solv(a,b,c,d,nwrk1,ndim,n,mdim,m,ind,ipr,niwrk, 
   *   +                    reps,ierr) 
   *      int i,i1,ierr,ind,nwrk1,niwrk,irow2,j,j1,j2,k,lrow2,m,m1 
   *      int mdim,n,ndim,mfin,ipr(niwrk) 
   *      double precision a(mdim,m),b(ndim,n),c(mdim,n),d(nwrk1),reps 
   *%purpose 
   *     this routine is only to be called from syhsc 
   *% 
   * 
   * 
   */
  /* Parameter adjustments */
  --d__;
  b_dim1 = *ndim;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  c_dim1 = *mdim;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  a_dim1 = *mdim;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  --ipr;

  /* Function Body */
  if (*ind < *n - 1)
    {
      nsp_ctrlpack_backs2 (&c__[c_offset], &b[b_offset], ind, n, m, mdim,
			   ndim);
    }
  /* 
   */
  m2 = *m << 1;
  mfin = m2 * (m2 + 1) / 2 + (*m << 2);
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i__2 = (i__ << 1) - 1;
      m1 = nsp_ctrlpack_irow2 (&i__2, m);
      i__2 = (i__ << 1) - 1;
      k = nsp_ctrlpack_lrow2 (&i__2, m);
      i1 = i__ - 1;
      if (i__ == 1)
	{
	  i1 = 1;
	}
      i__2 = *m;
      for (j = i1; j <= i__2; ++j)
	{
	  j1 = ((j - i1 + 1) << 1) + m1;
	  j2 = 1;
	  if (m1 == 0)
	    {
	      j2 = 0;
	    }
	  j2 = j1 + k - j2;
	  d__[j1 - 1] = a[i__ + j * a_dim1];
	  d__[j1] = 0.;
	  d__[j2] = a[i__ + j * a_dim1];
	  d__[j2 - 1] = 0.;
	  /* L10: */
	}
      j1 = m1 + 3;
      if (i__ == 1)
	{
	  j1 += -2;
	}
      d__[j1] += b[*ind + *ind * b_dim1];
      d__[j1 + 1] += b[*ind + (*ind + 1) * b_dim1];
      if (i__ != 1)
	{
	  j1 = m1 + 2;
	}
      j1 += k;
      d__[j1] += b[*ind + 1 + *ind * b_dim1];
      d__[j1 + 1] += b[*ind + 1 + (*ind + 1) * b_dim1];
      ip = (i__ << 1) + mfin;
      d__[ip] = c__[i__ + (*ind + 1) * c_dim1];
      d__[ip - 1] = c__[i__ + *ind * c_dim1];
      /* L20: */
    }
  /* 
   */
  nsp_ctrlpack_h2solv (&d__[1], nwrk1, &ipr[1], niwrk, m, reps, ierr);
  /* 
   */
  if (*ierr != 0)
    {
      goto L40;
    }
  i__1 = *m;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      c__[i__ + *ind * c_dim1] = d__[ipr[(i__ << 1) - 1]];
      c__[i__ + (*ind + 1) * c_dim1] = d__[ipr[i__ * 2]];
      /* L30: */
    }
  *ind += -2;
  return 0;
 L40:
  *ierr = -(*ind) - 1;
  return 0;
}				/* n2solv_ */

int
nsp_ctrlpack_h2solv (double *d__, int *nwrk1, int *ipr, int *niwrk, int *m,
		     double *reps, int *ierr)
{
  /* System generated locals */
  int i__1, i__2, i__3;
  double d__1;

  /* Local variables */
  int mfin;
  int i__, j, k, l;
  double ddmax;
  int i1, i2, j1, k1, m2, m21, ip1;

  /* Parameter adjustments */
  --d__;
  --ipr;

  /* Function Body */
  *ierr = 0;
  m2 = *m << 1;
  mfin = m2 * (m2 + 1) / 2 + (*m << 2);
  i__1 = m2;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      ipr[m2 + i__] = nsp_ctrlpack_irow2 (&i__, m);
      ipr[i__] = i__ + mfin;
      /* L10: */
    }
  m21 = m2 - 1;
  i__1 = m21;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      i1 = 2;
      if (i__ == m21)
	{
	  i1 = 1;
	}
      l = 0;
      ddmax = (d__1 = d__[ipr[m2 + i__] + 1], Abs (d__1));
      i__2 = i1;
      for (j = 1; j <= i__2; ++j)
	{
	  if ((d__1 = d__[ipr[m2 + j + i__] + 1], Abs (d__1)) <= ddmax)
	    {
	      goto L20;
	    }
	  ddmax = (d__1 = d__[ipr[m2 + j + i__] + 1], Abs (d__1));
	  l = j;
	L20:
	  ;
	}
      if (ddmax <= *reps)
	{
	  goto L80;
	}
      if (l == 0)
	{
	  goto L30;
	}
      k = ipr[m2 + i__];
      ipr[m2 + i__] = ipr[m2 + l + i__];
      ipr[m2 + l + i__] = k;
      k = ipr[i__];
      ipr[i__] = ipr[i__ + l];
      ipr[i__ + l] = k;
    L30:
      ++ipr[m2 + i__ + 1];
      if (i__ != m21)
	{
	  ++ipr[m2 + i__ + 2];
	}
      ip1 = i__ + 1;
      i__2 = i1;
      for (j = 1; j <= i__2; ++j)
	{
	  ddmax = d__[ipr[m2 + i__ + j]] / d__[ipr[m2 + i__] + 1];
	  d__[ipr[i__ + j]] -= ddmax * d__[ipr[i__]];
	  i__3 = m2;
	  for (k1 = ip1; k1 <= i__3; ++k1)
	    {
	      k = k1 - i__;
	      d__[ipr[m2 + i__ + j] + k] -=
		ddmax * d__[ipr[m2 + i__] + 1 + k];
	      /* L40: */
	    }
	}
    }
  if ((d__1 = d__[ipr[m2 + m2] + 1], Abs (d__1)) <= *reps)
    {
      goto L80;
    }
  d__[ipr[m2]] /= d__[ipr[m2 + m2] + 1];
  i__3 = m21;
  for (i1 = 1; i1 <= i__3; ++i1)
    {
      i__ = m2 - i1;
      i2 = i__ + 1;
      ddmax = 0.;
      i__2 = m2;
      for (j1 = i2; j1 <= i__2; ++j1)
	{
	  j = j1 - i2 + 2;
	  ddmax += d__[ipr[j1]] * d__[ipr[m2 + i__] + j];
	  /* L50: */
	}
      d__[ipr[i__]] = (d__[ipr[i__]] - ddmax) / d__[ipr[m2 + i__] + 1];
      /* L60: */
    }
 L70:
  return 0;
 L80:
  *ierr = -1;
  goto L70;
}				/* h2solv_ */

int
nsp_ctrlpack_backs2 (double *c__, double *b, int *ind, int *n, int *m,
		     int *mdim, int *ndim)
{
  /* System generated locals */
  int b_dim1, b_offset, c_dim1, c_offset, i__1, i__2;

  /* Local variables */
  int i__, j, ind1, ind2;

  /* Parameter adjustments */
  c_dim1 = *mdim;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  b_dim1 = *ndim;
  b_offset = b_dim1 + 1;
  b -= b_offset;

  /* Function Body */
  ind1 = *ind + 1;
  ind2 = *ind + 2;
  i__1 = *n;
  for (i__ = ind2; i__ <= i__1; ++i__)
    {
      i__2 = *m;
      for (j = 1; j <= i__2; ++j)
	{
	  c__[j + ind1 * c_dim1] -=
	    b[ind1 + i__ * b_dim1] * c__[j + i__ * c_dim1];
	  c__[j + *ind * c_dim1] -=
	    b[*ind + i__ * b_dim1] * c__[j + i__ * c_dim1];
	  /* L10: */
	}
    }
  return 0;
}				/* backs2_ */
