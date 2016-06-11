/* expan.f -- translated by f2c (version 19961017).
 *    F Delebecque INRIA 1986, Copyright INRIA 
 */

#include "ctrlpack.h"

/*
 *calcul des nmax premiers coefficients de la longue division de 
 * b par a .On suppose a(1) non nul. 
 *
 *      subroutine expan(a,la,b,lb,c,nmax) 
 * a vecteur de longueur la des coeffs par puissances croissantes 
 * b   "           "     lb        "                "          " 
 * c                     nmax   des coeffs de  a/b 
 */

int
nsp_ctrlpack_expan (const double *a,const int *la, const double *b,const int *lb, double *c, const int *nmax)
{
  int j, k, m= *la, n = *lb;
  double s, a0;
  --a;
  --b;
  --c;

  a0 = a[1];
  if (a0 == 0.)
    {
      return 0;
    }
  k = 1;
 L2:
  s = 0.;
  if (k == 1)
    {
      goto L8;
    }
  j = 1;
 L5:
  ++j;
  if (j > Min (m, k))
    {
      goto L8;
    }
  s += a[j] * c[k - j + 1];
  goto L5;
 L8:
  if (k <= n)
    {
      c[k] = (b[k] - s) / a0;
    }
  else
    {
      c[k] = -s / a0;
    }
  if (k == *nmax)
    {
      return 0;
    }
  ++k;
  goto L2;
}

/*
 * similar to expan but the arguments a and b are 
 * explored in reverse order 
 */

int
nsp_ctrlpack_rexpan (const double *a,const int *la, const double *b,const int *lb, double *c, const int *nmax)
{
  int j, k, m= *la, n = *lb;
  double s, a0;

  --a;
  --b;
  --c;
  
  a0 = a[m];
  if (a0 == 0.)
    {
      return 0;
    }
  k = 1;
 L2:
  s = 0.;
  if (k == 1)
    {
      goto L8;
    }
  j = 1;
 L5:
  ++j;
  if (j > Min (m, k))
    {
      goto L8;
    }
  s += a[m-j+1] * c[k - j + 1];
  goto L5;
 L8:
  if (k <= n)
    {
      c[k] = (b[n-k+1] - s) / a0;
    }
  else
    {
      c[k] = -s / a0;
    }
  if (k == *nmax)
    {
      return 0;
    }
  ++k;
  goto L2;
}



