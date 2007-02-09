
#include "grand.h"

/**
 * rand_genprm:
 * @array:  array if double.
 * @larray: length of array 
 * 
 * generates random permutation of @array. 
 * 
 **/

void rand_genprm (double *array, int larray)
{
  int i, iwhich;
  double elt;
  --array;
  for (i = 1; i <= larray ; ++i)
    {
      iwhich = (int) rand_ignuin (i,larray);
      elt = array[iwhich];
      array[iwhich] = array[i];
      array[i] = elt;
    }
}			

