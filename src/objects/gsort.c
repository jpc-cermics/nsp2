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
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "nsp/math.h"
#include "nsp/gsort.h"
#include "nsp/object.h"

/* swapcode for indices : indices are ints **/

/*
 * General sort routine for Scilab 
 * xI is the transmitted table to sort ( if table is int ) 
 * xD is the transmitted table to sort ( if table is double ) 
 * ind is the int table to store the permutation 
 *     (which is to be initialized and changed )
 * iflag == if 1 ind is to be computed if 0 ind is ignored 
 * m,n : matrix size 
 * type : the operation ( see the interface ) 
 * iord : "i" or "d" : increasind or decreasing sort 
 */

static void mergesort(double *a,int *p, int fromIndex, int toIndex);
static void qsort_stable(double *a,int *index, int fromIndex, int toIndex);

int C2F(gsort)(int *xI, double *xD, int *ind, int *iflag, int *m, int *n,nsp_const_string type,nsp_const_string iord)
{
  /* int i; */
  switch ( type[0])
    {
    case 'r' :  CNAME(ColSort,double)(xD,ind,*iflag,*m,*n,iord[0]);break;
    case 'c' :  CNAME(RowSort,double)(xD,ind,*iflag,*m,*n,iord[0]);break;
    case 'l' :  
      if ( type[1] == 'r' ) 
	CNAME(LexiRow,int)(xI,ind,*iflag,*m,*n,iord[0]);
      else
	CNAME(LexiCol,int)(xI,ind,*iflag,*m,*n,iord[0]);
      break;
    case 'i' : CNAME(GlobalSort,int)(xI,ind,*iflag,*m,*n,iord[0]);break;
    case 'g' : 
    default :  
      CNAME(GlobalSort,double)(xD,ind,*iflag,*m,*n,iord[0]);break;
      /* 
      for ( i = 0 ; i < (*m)*(*n) ; i++) ind[i]= i+1;
      qsort_stable(xD,ind,0,(*m)*(*n)); break;
      mergesort(xD,ind,0,(*m)*(*n)); break;
      */
    }
  return(0);
}

/*
 * just used to prevent warnings about unused functions 
 */

int C2F(gsort_uuuu)(xI,xD,ind,iflag,m,n,type,iord)
     int *xI,*ind;
     double *xD;
     int *m,*n,*iflag;
     char *type,*iord;
{
  switch ( type[0])
    {
    case 'r' :  CNAME(ColSort,int)(xI,ind,*iflag,*m,*n,iord[0]);break;
    case 'c' :  CNAME(RowSort,int)(xI,ind,*iflag,*m,*n,iord[0]);break;
    case 'l' :  
      if ( type[1] == 'r' ) 
	CNAME(LexiRow,double)(xD,ind,*iflag,*m,*n,iord[0]);
      else
	CNAME(LexiCol,double)(xD,ind,*iflag,*m,*n,iord[0]);
      break;
    case 'g' : 
    default : CNAME(GlobalSort,int)((int *) xD,ind,*iflag,*m,*n,iord[0]);break;
    }
  return(0);
}

/*
 * General sort routine for Scilab 
 * The version for Scilab strings 
 * iflag == if 1 ind is to be computed if 0 ind is ignored 
 * m,n : matrix size 
 * type : the operation ( see the interface ) 
 * iord : 'i' or 'd' : increasind or decreasing sort 
 */

void C2F(gsorts)(char **data, int *ind, int *iflag, int *m, int *n,nsp_const_string type,nsp_const_string iord)
{
  switch ( type[0])
    {
    case 'r' :  CNAME(ColSort,char)(data,ind,*iflag,*m,*n,iord[0]);break;
    case 'c' :  CNAME(RowSort,char)(data,ind,*iflag,*m,*n,iord[0]);break;
    case 'l' :  
      if ( type[1] == 'r' ) 
	CNAME(LexiRow,char)((int **)data,ind,*iflag,*m,*n,iord[0]);
      else
	CNAME(LexiCol,char)(data,ind,*iflag,*m,*n,iord[0]);
      break;
    case 'g' : 
    default :  CNAME(GlobalSort,char)(data,ind,*iflag,*m,*n,iord[0]);break;
    }
}

/* Arrays.java -- Utility class with methods to operate on arrays
 *   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004
 *  Free Software Foundation, Inc.
 *
 *  This file is part of GNU Classpath.
 *
 *  GNU Classpath is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 */

static void qsort__(double *array,int *index, int from, int count);

/**
 * Performs a stable sort on the elements, arranging them according to their
 * natural order.
 *
 * @param a the double array to sort
 * @param fromIndex the first index to sort (inclusive)
 * @param toIndex the last index to sort (exclusive)
 */

static void qsort_stable(double *a,int *index, int fromIndex, int toIndex)
{
  qsort__(a,index, fromIndex, toIndex - fromIndex);
}

/**
 * Compares two integers in natural order, since a - b is inadequate.
 *
 * @param a the first int
 * @param b the second int
 * @return &lt; 0, 0, or &gt; 0 accorting to the comparison
 */

static int compare(double a,double b)
{
  return a < b ? -1 : a == b ? 0 : 1;
}

/**
 * Finds the index of the median of three array elements.
 *
 * @param a the first index
 * @param b the second index
 * @param c the third index
 * @param d the array
 * @return the index (a, b, or c) which has the middle value of the three
 */

static int med3(int a, int b, int c, double *d)
{
  return (compare(d[a], d[b]) < 0
	  ? (compare(d[b], d[c]) < 0 ? b
	     : compare(d[a], d[c]) < 0 ? c : a)
	  : (compare(d[b], d[c]) > 0 ? b
	     : compare(d[a], d[c]) > 0 ? c : a));
}

/**
 * Swaps the elements at two locations of an array
 *
 * @param i the first index
 * @param j the second index
 * @param a the array
 */

static void swap(int i, int j, double *a,int *index)
{
  int c1= index[i];
  double c = a[i];
  a[i] = a[j];
  a[j] = c;
  index[i] = index[j];
  index[j] = c1;
}

/**
 * Swaps two ranges of an array.
 *
 * @param i the first range start
 * @param j the second range start
 * @param n the element count
 * @param a the array
 */

static void vecswap(int i, int j, int n, double *a,int *index)
{
  for ( ; n > 0; i++, j++, n--)
    swap(i, j, a,index);
}

/**
 * Performs a recursive modified quicksort.
 *
 * @param array the array to sort
 * @param from the start index (inclusive)
 * @param count the number of elements to sort
 */

static void qsort__(double *array,int *index, int from, int count)
{
  int i,j;
  /* Use an insertion sort on small arrays. */
  if (count <= 7)
    {
      for ( i = from + 1; i < from + count; i++)
	for ( j = i;
	     j > from && compare(array[j - 1], array[j]) > 0;
	     j--)
	  {
	    swap(j, j - 1, array,index);
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
  swap(from, mid, array,index);
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
	      swap(a, b, array,index);
	      a++;
	    }
	  b++;
	}
      while (c >= b && (comp = compare(array[c], array[from])) >= 0)
	{
	  if (comp == 0)
	    {
	      swap(c, d, array,index);
	      d--;
	    }
	  c--;
	}
      if (b > c)
	break;
      swap(b, c, array,index);
      b++;
      c--;
    }

  /* Swap pivot(s) back in place, the recurse on left and right sections. */
  hi = from + count;
  int span;
  span = Min(a - from, b - a);
  vecswap(from, b - span, span, array,index);

  span = Min(d - c, hi - d - 1);
  vecswap(b, hi - span, span, array,index );

  span = b - a;
  if (span > 1)
    qsort__(array,index, from, span);

  span = d - c;
  if (span > 1)
    qsort__(array,index, hi - span, span);
}



#define arraycopy(src,isrc,dest,idest,n) memcpy(dest+idest,src+isrc,(n)*sizeof(double)) 
#define iarraycopy(src,isrc,dest,idest,n) memcpy(dest+idest,src+isrc,(n)*sizeof(int)) 

void mergesort(double *a,int *p, int fromIndex, int toIndex)
  {
    NspMatrix *M,*IM;
    double *src,*dest,*t;
    int *psrc,*pdest,*pt;
    int chunk,i,size,start;
    /*
     * In general, the code attempts to be simple rather than fast, the
     * idea being that a good optimising JIT will be able to optimise it
     * better than I can, and if I try it will make it more confusing for
     * the JIT. First presort the array in chunks of length 6 with insertion
     * sort. A mergesort would give too much overhead for this length.
     */
    for (chunk = fromIndex; chunk < toIndex; chunk += 6)
      {
        int end = Min(chunk + 6, toIndex);
        for (i = chunk + 1; i < end; i++)
          {
            if ( a[i - 1] > a[i] )
              {
                /* not already sorted */
                int j = i,jinit=i;
                double elem = a[j];
                do
                  {
                    a[j] = a[j - 1];
		    p[j] = j; /* start at 1 */
                    j--;
                  }
                while (j > chunk  && a[j - 1] > elem );
                a[j] = elem;
		p[j] = jinit;
              }
          }
      }
    int len = toIndex - fromIndex;
    /* If length is smaller or equal 6 we are done. */
    if (len <= 6)   return;

    src = a;
    if ((M = nsp_matrix_create(NVOID,'r',1,len) ) == NULLMAT ) return ;
    dest = M->R;
    t = NULL; /* t is used for swapping src and dest */

    /* same for p */
    psrc = p;
    if ((IM = nsp_matrix_create(NVOID,'r',1,len) ) == NULLMAT ) return ;
    pdest = IM->I;
    pt = NULL; /* t is used for swapping src and dest */

    /* The difference of the fromIndex of the src and dest array. */
    int srcDestDiff = -fromIndex;

    /* The merges are done in this loop */
    for ( size = 6; size < len; size <<= 1)
      {
        for ( start = fromIndex; start < toIndex; start += size << 1)
          {
	    /* 
	     * mid is the start of the second sublist;
	     * end the start of the next sublist (or end of array).
	     */
            int mid = start + size;
            int end = Min(toIndex, mid + size);
	    /*
	     * The second list is empty or the elements are already in
	     * order - no need to merge
	     */
            if (mid >= end  || src[mid - 1] <=  src[mid])
              {
                arraycopy(src,start, dest, start + srcDestDiff, end - start);
                iarraycopy(psrc,start, pdest, start + srcDestDiff, end - start);
                /* The two halves just need swapping - no need to merge */
              }
            else if ( src[start] > src[end - 1] )
              {
                arraycopy(src, start, dest, end - size + srcDestDiff, size);
                arraycopy(src, mid,dest, start + srcDestDiff, end - mid);
                iarraycopy(psrc, start, pdest, end - size + srcDestDiff, size);
                iarraycopy(psrc, mid,pdest, start + srcDestDiff, end - mid);
              }
            else
              {
                int p1 = start;
                int p2 = mid;
                int i = start + srcDestDiff;
		/*
		 * The main merge loop; terminates as soon as either
		 * half is ended
		 */
                while (p1 < mid && p2 < end)
                  {
		    int is=(src[p1] <= src[p2]) ? p1++ : p2++;
                    dest[i] = src[is];
                    pdest[i++] = psrc[is];
                  }
		/*
		 * Finish up by copying the remainder of whichever half
		 * wasn't finished.
		 */
                if (p1 < mid)
		  {
		    arraycopy(src, p1, dest, i, mid - p1);
		    iarraycopy(psrc, p1, pdest, i, mid - p1);
		  }
                else
		  {
		    arraycopy(src, p2, dest, i, end - p2);
		    iarraycopy(psrc, p2, pdest, i, end - p2);
		  }
              }
	  }
        /* swap src and dest ready for the next merge */
        t = src;
        src = dest;
        dest = t;
        fromIndex += srcDestDiff;
        toIndex += srcDestDiff;
        srcDestDiff = -srcDestDiff;
      }
    /*
     * make sure the result ends up back in the right place.  Note
     * that src and dest may have been swapped above, so src
     * contains the sorted array.
     */
    if (src != a)
      {
        /* Note that fromIndex == 0. */
        arraycopy(src, 0, a, srcDestDiff, toIndex);
        iarraycopy(psrc, 0, p, srcDestDiff, toIndex);
      }
  }







/*

When copying the contents of one array into another, use the System.arraycopy method instead of an iterative loop. For example, consider the following arrays:

 
int[] first = {1, 2, 3};
int[] second = {4, 5, 6};
int[] third = new int[first.length + second.length];

One possible way of copying the contents of the first two arrays into the third one is to use loops, such as:

 
for (int i = 0; i < first.length; i++)
third[i] = first[i];
for (int i = 0; i < second.length; i++)
third[first.length + i] = second[i];

However, a better way of accomplishing the same task is to use the System.arraycopy method as in the following example:

 
System.arraycopy(first, 0, third, 0, first.length);
System.arraycopy(second, 0, third, first.length, second.length); 

*/
