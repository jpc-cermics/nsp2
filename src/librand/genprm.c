#include "grand.h"


/**
 * rand_genprm:
 * @array:  array if double.
 * @larray: length of array
 *
 * generates random permutation of @array.
 *
 **/

/* I (bp) have rewrote it to handle changes in rand_ignuin */

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
