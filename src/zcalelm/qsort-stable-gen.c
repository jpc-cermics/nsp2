/*
 * this file is not used directly but inserted in qsort1.c 
 */ 

/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * A stable qsort for which is specialized to int, double, nsp_string 
 * by include 
 *
 */

/* Arrays.java -- Utility class with methods to operate on arrays
 *  Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
 *  Free Software Foundation, Inc.
 *
 *  This file is part of GNU Classpath.
 *
 *  GNU Classpath is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 */


/**
 * Compares two integers in natural order, since a - b is inadequate.
 *
 * @param a the first int
 * @param b the second int
 * @return &lt; 0, 0, or &gt; 0 accorting to the comparison
 */

#ifdef STABLE_INCREASING
#undef DIR
#define DIR incr_
#undef compare
#define compare(a,b) ((a) < (b) ? -1 : (a) == (b) ? 0 : 1)
#else 
#undef DIR
#define DIR decr_
#undef compare
#define compare(a,b) ((a) < (b) ? 1 : (a) == (b) ? 0 : -1)
#endif 

/* we want y to be expanded in CNAME thus we use XCNAME ! */

#define C3NAME(x,y,z) x##y##z 
#define XCNAME(x,y,z) C3NAME(x,y,z)

static void XCNAME(nsp_internal_qsort_stable_,DIR,ELT_TYPE)(ELT_TYPE *array,int *index, int flag, int from, int count);

/**
 * Finds the index of the median of three array elements.
 *
 * @param a the first index
 * @param b the second index
 * @param c the third index
 * @param d the array
 * @return the index (a, b, or c) which has the middle value of the three
 */

#define med3(a,b,c,d) (compare(d[a], d[b]) < 0 \
	  ? (compare(d[b], d[c]) < 0 ? (b) \
	     : compare(d[a], d[c]) < 0 ? (c) : (a)) \
	  : (compare(d[b], d[c]) > 0 ? (b) \
	     : compare(d[a], d[c]) > 0 ? (c) : (a)));

/**
 * Swaps the elements at two locations of an array
 *
 * @param i the first index
 * @param j the second index
 * @param a the array
 */

#define swap(i,j,a,index,flag) temp = (a)[i];(a)[i] = (a)[j]; (a)[j] = temp; if ( flag == TRUE ) \
 { int c1= (index)[i]; (index)[i] = (index)[j];  (index)[j] = c1; }

/**
 * Swaps two ranges of an array.
 *
 * @param i the first range start
 * @param j the second range start
 * @param n the element count
 * @param a the array
 */

static  void XCNAME(vecswap,DIR,ELT_TYPE)(int i, int j, int n, ELT_TYPE *a,int *index,int flag)
{
  ELT_TYPE temp;
  for ( ; n > 0; i++, j++, n--)
    {
      swap(i, j, a,index,flag);
    }
}

/**
 * Performs a stable sort on the elements, arranging them according to their
 * natural order.
 *
 * @param a the ELT_TYPE array to sort
 * @param fromIndex the first index to sort (inclusive)
 * @param toIndex the last index to sort (exclusive)
 */

void XCNAME(nsp_qsort_stable_,DIR,ELT_TYPE)(ELT_TYPE *a,int *index,int flag, int fromIndex, int toIndex,char dir)
{
  int i,n = toIndex - fromIndex;
  if ( flag == TRUE) for ( i = fromIndex ; i < n  ; i++) index[i]= i+1;
  XCNAME(nsp_internal_qsort_stable_,DIR,ELT_TYPE)(a,index,flag, fromIndex, n);
}

/**
 * Performs a recursive modified quicksort.
 *
 * @param array the array to sort
 * @param from the start index (inclusive)
 * @param count the number of elements to sort
 */

static void XCNAME(nsp_internal_qsort_stable_,DIR,ELT_TYPE)(ELT_TYPE *array,int *index,int flag, int from, int count)
{
  ELT_TYPE temp;
  int i,j;
  /* Use an insertion sort on small arrays. */
  if (count <= 7)
    {
      for ( i = from + 1; i < from + count; i++)
	for ( j = i;
	      j > from && compare(array[j - 1], array[j]) > 0;
	      j--)
	  {
	    swap(j, j - 1, array,index,flag);
	  }
      return;
    }
  /* Determine a good median element. */
  int mid = count / 2;
  int lo = from;
  int hi = from + count - 1;

  if (count > 40)
    { /* big arrays, pseudomedian of 9 */
      int s = count / 8;
      lo = med3(lo, lo + s, lo + 2 * s, array);
      mid = med3(mid - s, mid, mid + s, array);
      hi = med3(hi - 2 * s, hi - s, hi, array);
    }
  mid = med3(lo, mid, hi, array);

  int a, b, c, d;
  int comp;

  /* Pull the median element out of the fray, and use it as a pivot. */
  swap(from, mid, array,index,flag);
  a = b = from;
  c = d = from + count - 1;

  /* Repeatedly move b and c to each other, swapping elements so */
  /* that all elements before index b are less than the pivot, and all */
  /* elements after index c are greater than the pivot. a and b track */
  /* the elements equal to the pivot. */
  while (1)
    {
      while (b <= c && (comp = compare(array[b], array[from])) <= 0)
	{
	  if (comp == 0)
	    {
	      swap(a, b, array,index,flag);
	      a++;
	    }
	  b++;
	}
      while (c >= b && (comp = compare(array[c], array[from])) >= 0)
	{
	  if (comp == 0)
	    {
	      swap(c, d, array,index,flag);
	      d--;
	    }
	  c--;
	}
      if (b > c)
	break;
      swap(b, c, array,index,flag);
      b++;
      c--;
    }

  /* Swap pivot(s) back in place, the recurse on left and right sections. */
  hi = from + count;
  int span;
  span = Min(a - from, b - a);
  XCNAME(vecswap,DIR,ELT_TYPE)(from, b - span, span, array,index,flag);

  span = Min(d - c, hi - d - 1);
  XCNAME(vecswap,DIR,ELT_TYPE)(b, hi - span, span, array,index,flag);

  span = b - a;
  if (span > 1)
    XCNAME(nsp_internal_qsort_stable_,DIR,ELT_TYPE)(array,index,flag, from, span);

  span = d - c;
  if (span > 1)
    XCNAME(nsp_internal_qsort_stable_,DIR,ELT_TYPE)(array,index,flag, hi - span, span);
}

