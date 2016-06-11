/* dsort.f -- translated by f2c (version 19961017).
 *
 *
 */

#include "calpack.h"

/*/MEMBR ADD NAME=DSORT,SSI=0 
 *    Copyright INRIA 
 */
int nsp_calpack_dsort (double *count, int *n, int *index)
{
  /* System generated locals */
  int i__1, i__2, i__3;

  /* Local variables */
  int ifka, mark[50], i__, j, k, m;
  double x;
  int lngth, mloop, k1, la, if__;
  double av;
  int ip, is, iy, intest, is1, ifk, int__;

  /* 
   *!purpose 
   *    dsort sort double precision array,maintaining an index array 
   * 
   *!calling sequence 
   *    subroutine dsort(count,n,index) 
   *    int n,index(n) 
   *    double precision count(n) 
   * 
   *    count   : array to be sorted 
   *    n       :size of count and index 
   *    index   : array containing on return index of sorted array 
   * 
   *!method 
   *    quick sort metjod is used 
   *!restriction 
   *    n must be less than 2**(50/2) ! due to lengh of work space mark 
   *! 
   * set index array to original order . 
   */
  /* Parameter adjustments */
  --index;
  --count;

  /* Function Body */
  i__1 = *n;
  for (i__ = 1; i__ <= i__1; ++i__)
    {
      index[i__] = i__;
      /* L10: */
    }
  /* check that a trivial case has not been entered . 
   */
  if (*n == 1)
    {
      goto L200;
    }
  if (*n >= 1)
    {
      goto L30;
    }
  goto L200;
  /* 'm' is the length of segment which is short enough to enter 
   * the final sorting routine. it may be easily changed. 
   */
L30:
  m = 12;
  /* set up initial values. 
   */
  la = 2;
  is = 1;
  if__ = *n;
  i__1 = *n;
  for (mloop = 1; mloop <= i__1; ++mloop)
    {
      /* if segment is short enough sort with final sorting routine . 
       */
      ifka = if__ - is;
      if (ifka + 1 > m)
	{
	  goto L70;
	}
      /********** final sorting *** 
       * ( a simple bubble sort ) 
       */
      is1 = is + 1;
      i__2 = if__;
      for (j = is1; j <= i__2; ++j)
	{
	  i__ = j;
	L40:
	  if (count[i__ - 1] > count[i__])
	    {
	      goto L60;
	    }
	  if (count[i__ - 1] < count[i__])
	    {
	      goto L50;
	    }
	  if (index[i__ - 1] < index[i__])
	    {
	      goto L60;
	    }
	L50:
	  av = count[i__ - 1];
	  count[i__ - 1] = count[i__];
	  count[i__] = av;
	  int__ = index[i__ - 1];
	  index[i__ - 1] = index[i__];
	  index[i__] = int__;
	  --i__;
	  if (i__ > is)
	    {
	      goto L40;
	    }
	L60:
	  ;
	}
      la += -2;
      goto L170;
      /*            *******  quicksort  ******** 
       * select the number in the central position in the segment as 
       * the test number.replace it with the number from the segment's 
       * highest address. 
       */
    L70:
      iy = (is + if__) / 2;
      x = count[iy];
      intest = index[iy];
      count[iy] = count[if__];
      index[iy] = index[if__];
      /* the markers 'i' and 'ifk' are used for the beginning and end 
       * of the section not so far tested against the present value 
       * of x . 
       */
      k = 1;
      ifk = if__;
      /* we alternate between the outer loop that increases i and the 
       * inner loop that reduces ifk, moving numbers and indices as 
       * necessary, until they meet . 
       */
      i__2 = if__;
      for (i__ = is; i__ <= i__2; ++i__)
	{
	  if (x < count[i__])
	    {
	      goto L110;
	    }
	  if (x > count[i__])
	    {
	      goto L80;
	    }
	  if (intest > index[i__])
	    {
	      goto L110;
	    }
	L80:
	  if (i__ >= ifk)
	    {
	      goto L120;
	    }
	  count[ifk] = count[i__];
	  index[ifk] = index[i__];
	  k1 = k;
	  i__3 = ifka;
	  for (k = k1; k <= i__3; ++k)
	    {
	      ifk = if__ - k;
	      if (count[ifk] < x)
		{
		  goto L100;
		}
	      if (count[ifk] > x)
		{
		  goto L90;
		}
	      if (intest <= index[ifk])
		{
		  goto L100;
		}
	    L90:
	      if (i__ >= ifk)
		{
		  goto L130;
		}
	      count[i__] = count[ifk];
	      index[i__] = index[ifk];
	      goto L110;
	    L100:
	      ;
	    }
	  goto L120;
	L110:
	  ;
	}
      /* return the test number to the position marked by the marker 
       * which did not move last. it divides the initial segment into 
       * 2 parts. any element in the first part is less than or equal 
       * to any element in the second part, and they may now be sorted 
       * independently . 
       */
    L120:
      count[ifk] = x;
      index[ifk] = intest;
      ip = ifk;
      goto L140;
    L130:
      count[i__] = x;
      index[i__] = intest;
      ip = i__;
      /* store the longer subdivision in workspace. 
       */
    L140:
      if (ip - is > if__ - ip)
	{
	  goto L150;
	}
      mark[la - 1] = if__;
      mark[la - 2] = ip + 1;
      if__ = ip - 1;
      goto L160;
    L150:
      mark[la - 1] = ip - 1;
      mark[la - 2] = is;
      is = ip + 1;
      /* find the length of the shorter subdivision. 
       */
    L160:
      lngth = if__ - is;
      if (lngth <= 0)
	{
	  goto L180;
	}
      /* if it contains more than one element supply it with workspace . 
       */
      la += 2;
      goto L190;
    L170:
      if (la <= 0)
	{
	  goto L200;
	}
      /* obtain the address of the shortest segment awaiting quicksort 
       */
    L180:
      if__ = mark[la - 1];
      is = mark[la - 2];
    L190:
      ;
    }
L200:
  return 0;
}				/* dsort_ */
