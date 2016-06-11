/* rcsort.f -- translated by f2c (version 19961017).
 *
 *
 */

#if 0
#include "calpack.h"

int
nsp_calpack_rcsort (intg_f test, int *isz, int *iptr, int *iv, int *n,
		    int *index)
{
  /* System generated locals */
  int i__1, i__2, i__3;

  /* Local variables */
  int ifka, mark[50], i__, j, k, m, s, x, lngth, mloop, k1, la, if__, as, av,
    ip, is, it, iy, intest, is1, ifk, int__;

  /* 
   *!purpose 
   *    rcsort sort a set of int records ,maintaining an index array 
   * 
   *!calling sequence 
   *    subroutine rcsort(test,isz,iptr,iv,n,index) 
   *    int n,index(n),iv(*),isz(n) 
   *    int iptr(n+1) 
   * 
   *    test    : external int function which define formal order for 
   *              records 
   *              test(r1,l1,r2,l2) 
   *              where 
   *              l1 is the length or record r1 
   *              l2 is the length or record r2 
   *              returns 
   *               1 :if record r1 is greater than r2 
   *              -1 :if record r1 is less than r2 
   *               0 :if record r1 is equal to r2 
   *    isz     : vector of records sizes 
   *    iptr    : table of records adresses in iv 
   *    iv      : table of records values 
   *    n       : size of vector of record and index 
   *    index   : array containing on return index of sorted array 
   * 
   *!method 
   *    quick sort method is used 
   *!restriction 
   *    n must be less than 2**(50/2) ! due to lengh of work space mark 
   *! 
   *    Copyright INRIA 
   * 
   * set index array to original order . 
   */
  /* Parameter adjustments */
  --iptr;
  --iv;
  --index;
  --isz;

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
  /*    'm' is the length of segment which is short enough to enter 
   *    the final sorting routine. it may be easily changed. 
   */
L30:
  m = 12;
  /*    set up initial values. 
   */
  la = 2;
  is = 1;
  if__ = *n;
  i__1 = *n;
  for (mloop = 1; mloop <= i__1; ++mloop)
    {
      /*    if segment is short enough sort with final sorting routine . 
       */
      ifka = if__ - is;
      if (ifka + 1 > m)
	{
	  goto L70;
	}
      /**********final sorting *** 
       *    ( a simple bubble sort ) 
       */
      is1 = is + 1;
      i__2 = if__;
      for (j = is1; j <= i__2; ++j)
	{
	  i__ = j;
	L40:
	  it =
	    (*test) (&iv[iptr[i__ - 1]], &isz[i__ - 1], &iv[iptr[i__]],
		     &isz[i__]);
	  if (it == 1)
	    {
	      goto L60;
	    }
	  if (it == -1)
	    {
	      goto L50;
	    }
	  if (index[i__ - 1] < index[i__])
	    {
	      goto L60;
	    }
	L50:
	  av = iptr[i__ - 1];
	  iptr[i__ - 1] = iptr[i__];
	  iptr[i__] = av;
	  /* 
	   */
	  as = isz[i__ - 1];
	  isz[i__ - 1] = isz[i__];
	  isz[i__] = as;
	  /* 
	   */
	  int__ = index[i__ - 1];
	  index[i__ - 1] = index[i__];
	  index[i__] = int__;
	  /* 
	   */
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
      /*    *******  quicksort  ******** 
       *    select the number in the central position in the segment as 
       *    the test number.replace it with the number from the segment's 
       *    highest address. 
       */
    L70:
      iy = (is + if__) / 2;
      x = iptr[iy];
      intest = index[iy];
      s = isz[iy];
      iptr[iy] = iptr[if__];
      isz[iy] = isz[if__];
      index[iy] = index[if__];
      /*    the markers 'i' and 'ifk' are used for the beginning and end 
       *    of the section not so far tested against the present value 
       *    of x . 
       */
      k = 1;
      ifk = if__;
      /*    we alternate between the outer loop that increases i and the 
       *    inner loop that reduces ifk, moving numbers and indices as 
       *    necessary, until they meet . 
       */
      i__2 = if__;
      for (i__ = is; i__ <= i__2; ++i__)
	{
	  it = (*test) (&iv[x], &s, &iv[iptr[i__]], &isz[i__]);
	  if (it < 0)
	    {
	      goto L110;
	    }
	  if (it > 0)
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
	  iptr[ifk] = iptr[i__];
	  index[ifk] = index[i__];
	  isz[ifk] = isz[i__];
	  k1 = k;
	  i__3 = ifka;
	  for (k = k1; k <= i__3; ++k)
	    {
	      ifk = if__ - k;
	      it = (*test) (&iv[iptr[ifk]], &isz[ifk], &iv[x], &s);
	      if (it < 0)
		{
		  goto L100;
		}
	      if (it > 0)
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
	      iptr[i__] = iptr[ifk];
	      index[i__] = index[ifk];
	      isz[i__] = isz[ifk];
	      goto L110;
	    L100:
	      ;
	    }
	  goto L120;
	L110:
	  ;
	}
      /*    return the test number to the position marked by the marker 
       *    which did not move last. it divides the initial segment into 
       *    2 parts. any element in the first part is less than or equal 
       *    to any element in the second part, and they may now be sorted 
       *    independently . 
       */
    L120:
      iptr[ifk] = x;
      index[ifk] = intest;
      isz[ifk] = s;
      ip = ifk;
      goto L140;
    L130:
      iptr[i__] = x;
      index[i__] = intest;
      isz[i__] = s;
      ip = i__;
      /*    store the longer subdivision in workspace. 
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
      /*    find the length of the shorter subdivision. 
       */
    L160:
      lngth = if__ - is;
      if (lngth <= 0)
	{
	  goto L180;
	}
      /*    if it contains more than one element supply it with workspace . 
       */
      la += 2;
      goto L190;
    L170:
      if (la <= 0)
	{
	  goto L200;
	}
      /*    obtain the address of the shortest segment awaiting quicksort 
       */
    L180:
      if__ = mark[la - 1];
      is = mark[la - 2];
    L190:
      ;
    }
L200:
  return 0;
}				/* rcsort_ */
#endif
