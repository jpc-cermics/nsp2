/* ricd.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "ctrlpack.h"

/* Table of constant values */

static int c__1 = 1;
static int c__11 = 11;

/*/MEMBR ADD NAME=RICD,SSI=0 
 */
int
nsp_ctrlpack_ricd (int *nf, int *nn, double *f, int *n, double *h__,
		   double *g, double *cond, double *x, double *z__, int *nz,
		   double *w, double *eps, int *ipvt, double *wrk1,
		   double *wrk2, int *ierr)
{
  /* System generated locals */
  int f_dim1, f_offset, g_dim1, g_offset, h_dim1, h_offset, z_dim1, z_offset,
    w_dim1, w_offset, x_dim1, x_offset, i__1, i__2;

  /* Local variables */
  int fail;
  int ndim;
  int nlow, i__, j;
  double t[1];
  int igh;
  double det[2];
  int npi, npj, low, nup;

  /*!purpose 
   *    this subroutine solves the discrete-time algebraic matrix 
   *    riccati equation 
   * 
   *               t        t               t      -1    t 
   *          x = f *x*f - f *x*g1*((g2 + g1 *x*g1)  )*g1 *x*f + h 
   * 
   *    by laub's variant of the hamiltonian-eigenvector approach. 
   * 
   *!method 
   *     laub, a.j., a schur method for solving algebraic riccati 
   *          equations, ieee trans. aut. contr., ac-24(1979), 913-921. 
   * 
   *    the matrix f is assumed to be nonsingular and the matrices g1 and 
   *    g2 are assumed to be combined into the square array g as follows: 
   *                                   -1   t 
   *                          g = g1*g2  *g1 
   * 
   *    in case f is singular, see: pappas, t., a.j. laub, and n.r. 
   *      sandell, on the numerical solution of the discrete-time 
   *      algebraic riccati equation, ieee trans. aut. contr., ac-25(1980 
   *      631-641. 
   * 
   *!calling sequence 
   *    subroutine ricd (nf,nn,f,n,h,g,cond,x,z,nz,w,eps 
   *   +                    ipvt,wrk1,wrk2,ierr   ) 
   * 
   *    int nf,ng,nh,nz,n,nn,itype(nn),ipvt(n),ierr 
   *    double precision f(nf,n),g(ng,n),h(nh,n),z(nz,nn),w(nz,nn), 
   *   +                 ,wrk1(nn),wrk2(nn),x(nf,n) 
   *    on input: 
   *       nf,nz      row dimensions of the arrays containing 
   *                        (f,g,h) and (z,w), respectively, as 
   *                        declared in the calling program dimension 
   *                        statement; 
   * 
   *       n                order of the matrices f,g,h; 
   * 
   *       nn               = 2*n = order of the internally generated 
   *                        matrices z and w; 
   * 
   *       f                a nonsingular n x n (real) matrix; 
   * 
   *       g,h              n x n symmetric, nonnegative definite 
   *                        (real) matrices. 
   * 
   *     eps      relative machine precision 
   * 
   * 
   *    on output: 
   * 
   *       x                n x n array containing txe unique positive 
   *                        (or nonnegative) definite solution of the 
   *                        riccati equation; 
   * 
   * 
   *       z,w              2*n x 2*n real scratch arrays used for 
   *                        computations involving the symplectic 
   *                        matrix associated with the riccati equation; 
   * 
   *       wrk1,wrk2       real scratch vectors of lengths  2*n 
   * 
   *     cond 
   *                        condition number estimate for the final nth 
   *                        order linear matrix equation solved; 
   * 
   *       ipvt       int scratch vector of length 2*n 
   * 
   *       ierr             error code 
   *                        ierr=0 : ok 
   *                        ierr=-1 : singular linear system 
   *                        ierr=i : i th eigenvalue is badly calculated 
   * 
   *    ***note:  all scratch arrays must be declared and included 
   *              in the call.*** 
   * 
   *!comments 
   *    it is assumed that: 
   *       (1)  f is nonsingular (can be relaxed; see ref. above   ) 
   *       (2)  g and h are nonnegative definite 
   *       (3)  (f,g1) is stabilizable and (c,f) is detectable where 
   *             t 
   *            c *c = h (c of full rank = rank(h)). 
   *    under these assumptions the solution (returned in the array h) is 
   *    unique and nonnegative definite. 
   * 
   *!originator 
   *    written by alan j. laub (dep't. of elec. engrg. - systems, univ. 
   *    of southern calif., los angeles, ca 90007; ph.: (213) 743-5535), 
   *    sep. 1977. 
   *    most recent version: apr. 15, 1981. 
   * 
   *!auxiliary routines 
   *    hqror2,inva,fout,mulwoa,mulwob 
   *    dgeco,dgesl (linpack   ) 
   *    balanc,balbak,orthes,ortran (eispack   ) 
   *    ddot (blas) 
   *! 
   * 
   *    *****parameters: 
   * 
   *    *****local variables: 
   * 
   * 
   *    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   * 
   *    eps is a  machine dependent parameter 
   *    specifying the relative precision of realing point arithmetic. 
   *    for example, eps = 16.0d+0**(-13) for double precision arithmetic 
   *    on ibm s360/s370. 
   * 
   * 
   *                            ( f**-1            (f**-1)*g             ) 
   * set up symplectic matrix z=(                                        ) 
   *                            ( h*(f**-1)        h*(f**-1)*g+trans(f)  ) 
   * 
   *z11=f**-1 
   */
  /* Parameter adjustments */
  --wrk2;
  --wrk1;
  --ipvt;
  x_dim1 = *nf;
  x_offset = x_dim1 + 1;
  x -= x_offset;
  g_dim1 = *nf;
  g_offset = g_dim1 + 1;
  g -= g_offset;
  h_dim1 = *nf;
  h_offset = h_dim1 + 1;
  h__ -= h_offset;
  f_dim1 = *nf;
  f_offset = f_dim1 + 1;
  f -= f_offset;
  w_dim1 = *nz;
  w_offset = w_dim1 + 1;
  w -= w_offset;
  z_dim1 = *nz;
  z_offset = z_dim1 + 1;
  z__ -= z_offset;

  /* Function Body */
  fail = FALSE;
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  z__[i__ + j * z_dim1] = f[i__ + j * f_dim1];
	  /* L10: */
	}
      /* L20: */
    }
  nsp_ctrlpack_dgeco (&z__[z_offset], nz, n, &ipvt[1], cond, &wrk1[1]);
  if (*cond + 1. <= 1.)
    {
      goto L200;
    }
  nsp_ctrlpack_dgedi (&z__[z_offset], nz, n, &ipvt[1], det, &wrk1[1], &c__1);
  /*z21=h*f**-1; z12=(f**-1)*g 
   */
  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      npj = *n + j;
      i__2 = *n;
      for (i__ = 1; i__ <= i__2; ++i__)
	{
	  npi = *n + i__;
	  z__[i__ + npj * z_dim1] =
	    C2F (ddot) (n, &z__[i__ + z_dim1], nz, &g[j * g_dim1 + 1], &c__1);
	  z__[npi + j * z_dim1] =
	    C2F (ddot) (n, &h__[i__ + h_dim1], nf,
			&z__[j * z_dim1 + 1], &c__1);
	  /* L90: */
	}
    }
  /*z22=transp(f)+h*(f**-1)*g 
   */
  i__2 = *n;
  for (j = 1; j <= i__2; ++j)
    {
      npj = *n + j;
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  npi = *n + i__;
	  z__[npi + npj * z_dim1] =
	    f[j + i__ * f_dim1] + C2F (ddot) (n, &z__[npi + z_dim1],
					      nz, &g[j * g_dim1 + 1], &c__1);
	  /* L130: */
	}
      /* L140: */
    }
  /* 
   *    balance z 
   * 
   */
  nsp_ctrlpack_balanc (nz, nn, &z__[z_offset], &low, &igh, &wrk1[1]);
  /* 
   *    reduce z to real schur form with eigenvalues outside the unit 
   *    disk in the upper left n x n upper quasi-triangular block 
   * 
   */
  nlow = 1;
  nup = *nn;
  nsp_ctrlpack_orthes (nz, nn, &nlow, &nup, &z__[z_offset], &wrk2[1]);
  nsp_ctrlpack_ortran (nz, nn, &nlow, &nup, &z__[z_offset], &wrk2[1],
		       &w[w_offset]);
  nsp_ctrlpack_hqror2 (nz, nn, &c__1, nn, &z__[z_offset], t, t, &w[w_offset],
		       ierr, &c__11);
  if (*ierr != 0)
    {
      goto L210;
    }
  nsp_ctrlpack_inva (nz, nn, &z__[z_offset], &w[w_offset], nsp_ctrlpack_fout,
		     eps, &ndim, &fail, &ipvt[1]);
  if (fail)
    {
      goto L220;
    }
  if (ndim != *n)
    {
      goto L230;
    }
  /* 
   *    compute solution of the riccati equation from the orthogonal 
   *    matrix now in the array w.  store the result in the array h. 
   * 
   */
  nsp_ctrlpack_balbak (nz, nn, &low, &igh, &wrk1[1], nn, &w[w_offset]);
  /*resolution systeme lineaire 
   */
  nsp_ctrlpack_dgeco (&w[w_offset], nz, n, &ipvt[1], cond, &wrk1[1]);
  if (*cond + 1. <= 1.)
    {
      goto L200;
    }
  i__2 = *n;
  for (j = 1; j <= i__2; ++j)
    {
      npj = *n + j;
      i__1 = *n;
      for (i__ = 1; i__ <= i__1; ++i__)
	{
	  x[i__ + j * x_dim1] = w[npj + i__ * w_dim1];
	  /* L150: */
	}
      /* L160: */
    }
  i__2 = *n;
  for (i__ = 1; i__ <= i__2; ++i__)
    {
      /* L165: */
      nsp_ctrlpack_dgesl (&w[w_offset], nz, n, &ipvt[1], &x[i__ * x_dim1 + 1],
			  &c__1);
    }
  return 0;
 L200:
  /*systeme lineaire numeriquement singulier 
   */
  *ierr = -1;
  return 0;
 L210:
  /*  erreur dans hqror2 
   */
  *ierr = i__;
  return 0;
  /* 
   */
 L220:
  /*     erreur dans inva 
   */
  return 0;
  /* 
   */
 L230:
  /*   la matrice symplectique n'a pas le 
   *   bon nombre de val. propres  de module 
   *   inferieur a 1. 
   */
  return 0;
  /* 
   *    last line of ricd 
   * 
   */
}				/* ricd_ */
