#include "nsp/machine.h"
#include "nsp/object.h"
#include "nsp/blas.h"

/*     PURPOSE 
 *        computes the matrix product C = A * B 
 *            C   =   A   *   B 
 *          (l,n)   (l,m) * (m,n) 
 *
 *     PARAMETERS 
 *        input 
 *        ----- 
 *        A : (double) array (l, m) with leading dim na 
 *
 *        B : (double) array (m, n) with leading dim nb 
 *
 *        na, nb, nc, l, m, n : ints 
 *
 *        output 
 *        ------ 
 *        C : (double) array (l, n) with leading dim nc 
 *
 *     NOTE 
 *        (original version substituted by a call to the blas dgemm) 
 */

int dmmul_scicos(double *a, int *na, double *b, int *nb, double *c__, int *nc, int *l, int *m, int *n)
{
  double c_b4 = 1.0, c_b5 = 0.0;
  int a_dim1, a_offset, b_dim1, b_offset, c_dim1, c_offset;
  a_dim1 = *na;
  a_offset = a_dim1 + 1;
  a -= a_offset;
  c_dim1 = *nc;
  c_offset = c_dim1 + 1;
  c__ -= c_offset;
  b_dim1 = *nb;
  b_offset = b_dim1 + 1;
  b -= b_offset;
  /* Function Body */
  C2F(dgemm)("n", "n", l, n, m, &c_b4, &a[a_offset], na, &b[b_offset], nb, &c_b5, &c__[c_offset], nc, 1L, 1L);
  return 0;
} 

/* c=c+a*b . 
 *     a            tableau de taille na*m contenant la matrice a 
 *     na           nombre de lignes du tableau a dans le programme appel 
 *     b,nb,c,nc    definitions similaires a celles de a,na 
 *     l            nombre de ligne des matrices a et c 
 *     m            nombre de colonnes de a et de lignes de b 
 *     n            nombre de colonnes de b et c 
 *     Copyright INRIA 
 */

int dmmul1_scicos(double *a, int *na, double *b, int *nb, double *c, int *nc, int *l, int *m, int *n)
{
  int i1=*n , i2=*l , i, j, ib=0, ic=0 , c1 = 1;
  for (j = 0 ; j < i1; ++j) 
    {
      for (i = 0; i < i2; ++i) {
	c[ic + i] += ddot_(m, &a[i], na, &b[ib], &c1);
      }
      ic += *nc;
      ib += *nb;
    }
  return 0;
} 




