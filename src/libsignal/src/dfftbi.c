/* dfftbi.f -- translated by f2c (version 19961017).
 *   
 *   -lf2c -lm (in that order)
 */

#include "signal.h"

/* Subroutine */ int
signal_dfftbi (double *a, double *b, int *nseg, int *n, int *nspn, int *isn,
	       int *ierr, int *lout, int *lnow, int *lused, int *lmax,
	       int *lbook, double *rstak, int *istak)
{
  /* Initialized data */

  static int isize[5] = { 1, 1, 1, 2, 2 };

  /* System generated locals */
  int i__1, i__2, i__3;

  /* Local variables */
  int nfac[15], maxf, maxp, ntot, i__, j, k, m, nspan, itype, j2, j3, nf, jj,
    in, kt;
  int nitems, istkgt, kkk;

  /*! 
   *arrays a and b originally hold the real and imaginary 
   *     components of the data, and return the real and 
   *     imaginary components of the resulting fourier coefficients. 
   *multivariate data is indexed according to the fortran 
   *     array element successor function, without limit 
   *     on the number of implied multiple subscripts. 
   *     the subroutine is called once for each variate. 
   *     the calls for a multivariate transform may be in any order. 
   * 
   *n is the dimension of the current variable. 
   *nspn is the spacing of consecutive data values 
   *     while indexing the current variable. 
   *nseg*n*nspn is the total number of complex data values. 
   *the sign of isn determines the sign of the complex 
   *     exponential, and the magnitude of isn is normally one. 
   *     the magnitude of isn determines the indexing increment for a&b. 
   * 
   *if fft is called twice, with opposite signs on isn, an 
   *     identity transformation is done...calls can be in either order. 
   *     the results are scaled by 1/n when the sign of isn is positive. 
   * 
   *a tri-variate transform with a(n1,n2,n3), b(n1,n2,n3) 
   *is computed by 
   *       call fft(a,b,n2*n3,n1,1,-1) 
   *       call fft(a,b,n3,n2,n1,-1) 
   *       call fft(a,b,1,n3,n1*n2,-1) 
   * 
   *a single-variate transform of n complex data values is computed by 
   *       call fft(a,b,1,n,1,-1) 
   * 
   *the data may alternatively be stored in a single complex 
   *     array a, then the magnitude of isn changed to two to 
   *     give the correct indexing increment and a(2) used to 
   *     pass the initial address for the sequence of imaginary 
   *     values, e.g. 
   *       call fft(a,a(2),nseg,n,nspn,-2) 
   * 
   *array nfac is working storage for factoring n.  the smallest 
   *     number exceeding the 15 locations provided is 12,754,584. 
   *! 
   * 
   * 
   */
  /* Parameter adjustments */
  --istak;
  --rstak;
  --b;
  --a;

  /* Function Body */
  /* 
   */
  *ierr = 0;
  /* 
   *determine the factors of n 
   * 
   */
  m = 0;
  nf = Abs (*n);
  k = nf;
  if (nf == 1)
    {
      return 0;
    }
  nspan = (i__1 = nf * *nspn, Abs (i__1));
  ntot = (i__1 = nspan * *nseg, Abs (i__1));
  if (*isn * ntot != 0)
    {
      goto L20;
    }
  *ierr = 1;
  return 0;
  /* 
   */
 L10:
  ++m;
  nfac[m - 1] = 4;
  k /= 16;
 L20:
  if (k - (k / 16 << 4) == 0)
    {
      goto L10;
    }
  j = 3;
  jj = 9;
  goto L40;
 L30:
  ++m;
  nfac[m - 1] = j;
  k /= jj;
 L40:
  if (k % jj == 0)
    {
      goto L30;
    }
  j += 2;
  /*Computing 2nd power 
   */
  i__1 = j;
  jj = i__1 * i__1;
  if (jj <= k)
    {
      goto L40;
    }
  if (k > 4)
    {
      goto L50;
    }
  kt = m;
  nfac[m] = k;
  if (k != 1)
    {
      ++m;
    }
  goto L90;
 L50:
  if (k - (k / 4 << 2) != 0)
    {
      goto L60;
    }
  ++m;
  nfac[m - 1] = 2;
  k /= 4;
  /*all square factors out now, but k .ge. 5 still 
   */
 L60:
  kt = m;
  /*Computing MAX 
   */
  i__1 = kt + kt + 2, i__2 = k - 1;
  maxp = Max (i__1, i__2);
  j = 2;
 L70:
  if (k % j != 0)
    {
      goto L80;
    }
  ++m;
  nfac[m - 1] = j;
  k /= j;
 L80:
  j = ((j + 1) / 2 << 1) + 1;
  if (j <= k)
    {
      goto L70;
    }
 L90:
  if (m <= kt + 1)
    {
      maxp = m + kt + 1;
    }
  if (m + kt > 15)
    {
      goto L120;
    }
  if (kt == 0)
    {
      goto L110;
    }
  j = kt;
 L100:
  ++m;
  nfac[m - 1] = nfac[j - 1];
  --j;
  if (j != 0)
    {
      goto L100;
    }
  /* 
   */
 L110:
  maxf = m - kt;
  maxf = nfac[maxf - 1];
  if (kt > 0)
    {
      /*Computing MAX 
       */
      i__1 = nfac[kt - 1];
      maxf = Max (i__1, maxf);
    }
  /*   MAJOR MODIFICATION 
   */
  i__1 = m;
  for (kkk = 1; kkk <= i__1; ++kkk)
    {
      /*Computing MAX 
       */
      i__2 = maxf, i__3 = nfac[kkk - 1];
      maxf = Max (i__2, i__3);
      /* L9999: */
    }
  /*     j = istkgt(maxf*4,3) 
   */
  nitems = maxf << 2;
  /*    following line modified FD & MG 
   */
  itype = 4;
  /*xxxxxxxxxxxxxxxxxxxxxxxxxxx 
   */
  istkgt = (*lnow * isize[1] - 1) / isize[itype - 1] + 2;
  i__ = ((istkgt - 1 + nitems) * isize[itype - 1] - 1) / isize[1] + 3;
  if (i__ > *lmax)
    {
      goto L1010;
    }
  istak[i__ - 1] = itype;
  istak[i__] = *lnow;
  ++(*lout);
  *lnow = i__;
  *lused = Max (*lused, *lnow);
  goto L1300;
  /* 
   */
 L1010:
  *ierr = -i__;
  return 0;
 L1300:
  j = istkgt;
  jj = j + maxf;
  j2 = jj + maxf;
  j3 = j2 + maxf;
  /*     k = istkgt(maxp,2) 
   */
  nitems = maxp;
  itype = 2;
  /*    xxxxxxxxxxxxxxxxxxxxxxxxxx 
   */
  istkgt = (*lnow * isize[1] - 1) / isize[itype - 1] + 2;
  i__ = ((istkgt - 1 + nitems) * isize[itype - 1] - 1) / isize[1] + 3;
  if (i__ > *lmax)
    {
      goto L11;
    }
  istak[i__ - 1] = itype;
  istak[i__] = *lnow;
  ++(*lout);
  *lnow = i__;
  *lused = Max (*lused, *lnow);
  goto L1400;
  /* 
   */
 L11:
  *ierr = -i__;
  return 0;
 L1400:
  k = istkgt;
  /*    la carte suivante est a supprimer si simple precision 
   *    next instruction commented by FD&MG (simulog residue?) 
   *   ******************************************** 
   *     k=2*k-1 
   *   ********************************************* 
   */
  signal_dfftmx (&a[1], &b[1], &ntot, &nf, &nspan, isn, &m, &kt, &rstak[j],
		 &rstak[jj], &rstak[j2], &rstak[j3], &istak[k], nfac);
  /*     call istkrl(2) 
   */
  k = 2;
  /* xxxxxxxxxxxxxxx 
   */
  in = 2;
  /* 
   */
  if (*lbook <= *lnow && *lnow <= *lused && *lused <= *lmax)
    {
      goto L13;
    }
  *ierr = 3;
  return 0;
 L13:
  if (in <= 0)
    {
      goto L1500;
    }
  if (*lbook > istak[*lnow] || istak[*lnow] >= *lnow - 1)
    {
      goto L21;
    }
  --(*lout);
  *lnow = istak[*lnow];
  --in;
  goto L13;
  /* 
   */
 L21:
  *ierr = 4;
  return 0;
 L1500:
  return 0;
  /* 
   */
 L120:
  *ierr = 2;
  return 0;
}				/* dfftbi_ */
