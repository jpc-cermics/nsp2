/* genprm.f -- translated by f2c (version 19961017).
   
	
*/

#include "grand.h"

/* Subroutine */ int
rand_genprm (double *array, int *larray)
{
  /* System generated locals */
  int i__1;
  double d__1;

  /* Local variables */
  int i__, iwhich;
  double elt, llarray;

/* ********************************************************************** */

/*    SUBROUTINE GENPRM( ARRAY, LARRAY ) */
/*               GENerate random PeRMutation of array */


/*                              Arguments */


/*     ARRAY <--> On output ARRAY is a random permutation of its */
/*                 value on input */
/*                         DOUBLE PRECISION ARRAY( LARRAY ) */

/*     LARRAY <--> Length of ARRAY */
/*                         INT LARRAY */

/*     Modification by Bruno to handle permutation of a double precision */
/*     array (15/11/2001) and to handle the changes in ignuin */

/* ********************************************************************** */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Functions .. */
/*     .. */
/*     .. Executable Statements .. */
  /* Parameter adjustments */
  --array;

  /* Function Body */
  llarray = (double) (*larray);
  i__1 = *larray;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      d__1 = (double) i__;
      iwhich = (int) rand_ignuin (&d__1, &llarray);
      elt = array[iwhich];
      array[iwhich] = array[i__];
      array[i__] = elt;
    }
  return 0;
}				/* genprm_ */
