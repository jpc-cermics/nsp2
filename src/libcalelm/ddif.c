#include "calpack.h"

/*
 *    ddif effectue l'operation vectorielle b=b-a 
 *
 *    subroutine ddif(n,a,na,b,nb) 
 *    double precision a(*),b(*) 
 *    int n,na,nb 
 * 
 *    n : nombre d'elements des vecteurs a et b 
 *    a : tableau contenant a 
 *    na : increment entre deux elements consecutifs de a 
 *         na > 0 
 *    b,nb : definitions similaires a celles de a et na 
 */

int nsp_calpack_ddif (const int *n,const double *a,const int *na, double *b,const int *nb)
{
  int i, ia=0, ib=0;
  for (i = 0; i < *n; ++i)
    {
      b[ib] -= a[ia];
      ia += *na;
      ib += *nb;
     }
  return 0;
}
