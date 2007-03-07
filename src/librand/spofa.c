#include "grand.h"


/*     SPOFA FACTORS A DOUBLE PRECISION SYMMETRIC POSITIVE DEFINITE MATRIX. 
 *     SPOFA IS USUALLY CALLED BY SPOCO, BUT IT CAN BE CALLED 
 *     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED. 
 *     (TIME FOR SPOCO) = (1 + 18/N)*(TIME FOR SPOFA) . 
 *     ON ENTRY 
 *        A       DOUBLE PRECISION(LDA, N) 
 *                THE SYMMETRIC MATRIX TO BE FACTORED.  ONLY THE 
 *                DIAGONAL AND UPPER TRIANGLE ARE USED. 
 *        LDA     INT 
 *                THE LEADING DIMENSION OF THE ARRAY  A . 
 *        N       INT 
 *                THE ORDER OF THE MATRIX  A . 
 *     ON RETURN 
 *        A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A = TRANS(R)*R 
 *                WHERE  TRANS(R)  IS THE TRANSPOSE. 
 *                THE STRICT LOWER TRIANGLE IS UNALTERED. 
 *                IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE. 
 *        INFO    INT 
 *                = 0  FOR NORMAL RETURN. 
 *                = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR 
 *                     OF ORDER  K  IS NOT POSITIVE DEFINITE. 
 *     LINPACK.  THIS VERSION DATED 08/14/78 . 
 *     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB. 
 *     SUBROUTINES AND FUNCTIONS 
 */

int rand_spofa (double *a, int *lda, int *n, int *info)
{
  int c__1 = 1;
  int a_dim1, a_offset, i__1, i__2, i__3;
  int j, k;
  double s, t;
  int jm1;

  a_dim1 = *lda;
  a_offset = a_dim1 + 1;
  a -= a_offset;

  i__1 = *n;
  for (j = 1; j <= i__1; ++j)
    {
      *info = j;
      s = 0.;
      jm1 = j - 1;
      if (jm1 < 1)
	{
	  goto L20;
	}
      i__2 = jm1;
      for (k = 1; k <= i__2; ++k)
	{
	  i__3 = k - 1;
	  t =
	    a[k + j * a_dim1] - rand_sdot (&i__3, &a[k * a_dim1 + 1], &c__1,
					   &a[j * a_dim1 + 1], &c__1);
	  t /= a[k + k * a_dim1];
	  a[k + j * a_dim1] = t;
	  s += t * t;
	  /* L10: */
	}
    L20:
      s = a[j + j * a_dim1] - s;
      /*     ......EXIT */
      if (s <= 0.)
	{
	  goto L40;
	}
      a[j + j * a_dim1] = sqrt (s);
      /* L30: */
    }
  *info = 0;
 L40:
  return 0;
}	

