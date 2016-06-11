
  /*
   *      decomposition of an int n in a base tw0. 
   *      n=a1+a2*2+a3*2**2+.........+am*2**(m-1). 
   *
   *    subroutine gdcp2i(n,itab,m) 
   *    int n,itab,m 
   * 
   *       n     : int to be decomposed (n.le.32767) 
   *       itab  :int vector of dimension 15. 
   *              in output: if(a(i-1).ne.0)then itab(i)=.true. 
   *              else itab(i)=.false. 
   *       m     :the number of itab elements to be consider in output. 
   * j. hanen -september 1978-ensm-nantes. 
   */

#include "calpack.h"

int nsp_calpack_gdcp2i (int *n, int *itab, int *m)
{
  /* Initialized data */

  static int ipow2[15] =
    { 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1 };

  int i__, k, nn;

  /* Parameter adjustments */
  --itab;

  /* Function Body */
  /* 
   */
  *m = 0;
  k = 15;
  nn = Abs (*n);
  if (nn > 32767)
    {
      nn %= 32767;
    }
  for (i__ = 1; i__ <= 15; ++i__)
    {
      if (nn < ipow2[i__ - 1])
	{
	  goto L10;
	}
      itab[k] = TRUE;
      nn -= ipow2[i__ - 1];
      if (*m == 0)
	{
	  *m = k;
	}
      goto L20;
    L10:
      itab[k] = FALSE;
    L20:
      --k;
      /* L30: */
    }
  /* 
   */
  return 0;
}				/* gdcp2i_ */
