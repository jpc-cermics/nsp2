#include "grand.h"

/* random permutation of a double array */
/* I (bp) have rewrote it to handle changes in rand_ignuin */
/* and to simplify the f2c code... */

void rand_genprm (double *array, int n)
{
  int i, iwhich;
  double elt;

  for ( i = 0 ; i < n-1 ; ++i )
    {
      iwhich = rand_ignuin (i, n-1);
      elt = array[iwhich];
      array[iwhich] = array[i];
      array[i] = elt;
    }
}
