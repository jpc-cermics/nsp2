/* inva.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/*
 * given the upper real schur matrix a 
 * with 1x1 or 2x2 diagonal blocks, this routine reorders the diagonal 
 * blocks along with their generalized eigenvalues by constructing equi- 
 * valence transformation. the transformation is also 
 * performed on the given (initial) transformation z (resulting from a 
 * possible previous step or initialized with the identity matrix). 
 * after reordering, the eigenvalues inside the region specified by the 
 * function ftest appear at the top. if ndim is their number then the 
 * ndim first columns of z span the requested subspace. 
 * 
 *    subroutine inva (nmax,n,a,z,ftest,eps,ndim,fail,ind) 
 *    int nmax,n,ftest,ndim,ind(n) 
 *    int fail 
 *    double precision a(nmax,n),z(nmax,n),eps 
 * 
 *    nmax     the first dimension of a and z 
 *    n        the order of a and z 
 *   *a        the matrix whose blocks are to be reordered. 
 *   *z        upon return this array is multiplied by the column 
 *             transformation z. 
 *    ftest(ls,alpha,beta,s,p) an int function describing the 
 *             spectrum of the invariant subspace to be computed: 
 *             when ls=1 ftest checks if alpha/beta is in that spectrum 
 *             when ls=2 ftest checks if the two complex conjugate 
 *             roots with sum s and product p are in that spectrum 
 *             if the answer is positive, ftest=1, otherwise ftest=-1 
 *    eps      the required absolute accuracy of the result 
 *   *ndim     an int giving the dimension of the computed 
 *             invariant subspace 
 *   *fail     a int variable which is false on a normal return, 
 *             true otherwise (when exchng fails) 
 *   *ind      an int working array of dimension at least n 
 * 
 *Copyright SLICOT 
 */


struct
{
  int iero;
} ierinv_;

#define ierinv_1 ierinv_

static double c_b6 = 1.;

int
nsp_ctrlpack_inva (int *nmax, int *n, double *a, double *z__, ct_Ftest ftest,
		   double *eps, int *ndim, int *fail, int *ind)
{
  /* System generated locals */
  int a_dim1, a_offset, z_dim1, z_offset, i__1, i__2;

  /* Local variables */
  double beta;
  int i__, k, l;
  double p, s, alpha;
  int istep, l1, ii, ll, is, ls, ifirst, l2i, l2k, ls1, ls2, num;

  /* Parameter adjustments */
  --ind;
  z_dim1 = *nmax;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;
  a_dim1 = *nmax;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  /* Function Body */
  ierinv_1.iero = 0;
  *fail = FALSE;
  *ndim = 0;
  num = 0;
  l = 0;
  ls = 1;
  /*** construct array ind(i) where : 
   ***      Abs(ind(i)) is the size of the block i 
   ***     sign(ind(i)) indicates the location of its eigenvalues 
   ***                  (as determined by ftest). 
   *** num is the number of elements in this array 
   */
  i__1 = *n;
  for (ll = 1; ll <= i__1; ++ll)
    {
      l += ls;
      if (l > *n)
	{
	  goto L50;
	}
      l1 = l + 1;
      if (l1 > *n)
	{
	  goto L20;
	}
      if (a[l1 + l * a_dim1] == 0.)
	{
	  goto L20;
	}
      /* here a 2x2  block is checked * 
       */
      ls = 2;
      s = a[l + l * a_dim1] + a[l1 + l1 * a_dim1];
      p =
	a[l + l * a_dim1] * a[l1 + l1 * a_dim1] - a[l + l1 * a_dim1] * a[l1 +
									 l *
									 a_dim1];
      is = (*ftest) (&ls, &alpha, &beta, &s, &p);
      if (ierinv_1.iero > 0)
	{
	  return 0;
	}
      goto L30;
      /* here a 1x1  block is checked * 
       */
    L20:
      ls = 1;
      is = (*ftest) (&ls, &a[l + l * a_dim1], &c_b6, &s, &p);
      if (ierinv_1.iero > 0)
	{
	  return 0;
	}
    L30:
      ++num;
      if (is == 1)
	{
	  *ndim += ls;
	}
      /* L40: */
      ind[num] = ls * is;
    }
  /***  reorder blocks such that those with positive value 
   ***    of ind(.) appear first. 
   */
 L50:
  l2i = 1;
  i__1 = num;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      if (ind[i__] > 0)
	{
	  goto L90;
	}
      /* if a negative ind(i) is encountered, then search for the first 
       * positive ind(k) following on it 
       */
      l2k = l2i;
      i__2 = num;
      for (k = i__; k <= i__2; ++k)
	{
	  if (ind[k] < 0)
	    {
	      goto L60;
	    }
	  goto L70;
	L60:
	  l2k -= ind[k];
	}
      /* if there are no positive indices following on a negative one 
       * then stop 
       */
      goto L100;
      /* if a positive ind(k) follows on a negative ind(i) then 
       * interchange block k before block i by performing k-i swaps 
       */
    L70:
      istep = k - i__;
      ls2 = ind[k];
      l = l2k;
      i__2 = istep;
      for (ii = 1; ii <= i__2; ++ii)
	{
	  ifirst = k - ii;
	  ls1 = -ind[ifirst];
	  l -= ls1;
	  /*         call exchng(a,z,n,l,ls1,ls2,eps,fail,nmax,nmax) 
	   */
	  nsp_ctrlpack_exch (nmax, n, &a[a_offset], &z__[z_offset], &l, &ls1,
			     &ls2);
	  if (*fail)
	    {
	      return 0;
	    }
	  /* L80: */
	  ind[ifirst + 1] = ind[ifirst];
	}
      ind[i__] = ls2;
    L90:
      l2i += ind[i__];
    }
 L100:
  *fail = FALSE;
  return 0;
}				/* inva_ */
