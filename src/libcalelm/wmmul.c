#include "calpack.h"

  /* 
   *       computes the matrix product C = A * B where the 
   *       matrices are complex with the scilab storage 
   *           C   =   A   *   B 
   *         (l,n)   (l,m) * (m,n) 
   *       input 
   *       ----- 
   *       Ar, Ai : real and imaginary part of the matrix A 
   *                (double) arrays (l, m) with leading dim na 
   * 
   *       Br, Bi : real and imaginary part of the matrix B 
   *                (double) arrays (m, n) with leading dim nb 
   * 
   *       na, nb, nc, l, m, n : ints 
   * 
   *       output 
   *       ------ 
   *       Cr, Ci : real and imaginary part of the matrix C 
   *                (double) arrays (l, n) with leading dim nc 
   *
   *       Cr = Ar * Br - Ai * Bi 
   *       Ci = Ar * Bi + Ai * Br 
   * 
   *       modification of the old wmmul to use blas calls 
   *    Cr <-  1*Ar*Br + 0*Cr 
   */

static double c_b4 = 1.;
static double c_b5 = 0.;
static double c_b8 = -1.;

int
nsp_calpack_wmmul_void (double *ar, double *ai, int *na, double *br,
			double *bi, int *nb, double *cr, double *ci, int *nc,
			int *l, int *m, int *n)
{
  int ar_dim1, ar_offset, ai_dim1, ai_offset, br_dim1, br_offset, bi_dim1,
    bi_offset, cr_dim1, cr_offset, ci_dim1, ci_offset;

  /* Parameter adjustments */
  ai_dim1 = *na;
  ai_offset = ai_dim1 + 1;
  ai -= ai_offset;
  ar_dim1 = *na;
  ar_offset = ar_dim1 + 1;
  ar -= ar_offset;
  ci_dim1 = *nc;
  ci_offset = ci_dim1 + 1;
  ci -= ci_offset;
  cr_dim1 = *nc;
  cr_offset = cr_dim1 + 1;
  cr -= cr_offset;
  bi_dim1 = *nb;
  bi_offset = bi_dim1 + 1;
  bi -= bi_offset;
  br_dim1 = *nb;
  br_offset = br_dim1 + 1;
  br -= br_offset;

  /* Function Body */
  C2F (dgemm) ("n", "n", l, n, m, &c_b4, &ar[ar_offset], na,
	       &br[br_offset], nb, &c_b5, &cr[cr_offset], nc, 1L, 1L);
  /*    Cr <- -1*Ai*Bi + 1*Cr 
   */
  C2F (dgemm) ("n", "n", l, n, m, &c_b8, &ai[ai_offset], na,
	       &bi[bi_offset], nb, &c_b4, &cr[cr_offset], nc, 1L, 1L);
  /*    Ci <-  1*Ar*Bi + 0*Ci 
   */
  C2F (dgemm) ("n", "n", l, n, m, &c_b4, &ar[ar_offset], na,
	       &bi[bi_offset], nb, &c_b5, &ci[ci_offset], nc, 1L, 1L);
  /*    Ci <-  1*Ai*Br + 1*Ci 
   */
  C2F (dgemm) ("n", "n", l, n, m, &c_b4, &ai[ai_offset], na,
	       &br[br_offset], nb, &c_b4, &ci[ci_offset], nc, 1L, 1L);
  return 0;
}

