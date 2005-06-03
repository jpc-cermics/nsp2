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

/*
 * General sorting routines 
 * xI is the transmitted table to sort ( if table is int ) 
 * xD is the transmitted table to sort ( if table is double ) 
 * ind is the int table to store the permutation 
 *     (which is to be initialized and changed )
 * iflag == if 1 ind is to be computed if 0 ind is ignored 
 * m,n : matrix size 
 * type : the operation ( see the interface ) 
 * iord : "i" or "d" : increasind or decreasing sort 
 */

static int nsp_mergesort(double *a,int *p,int flag, int fromIndex, int toIndex,char dir);
static void nsp_qsort_stable(double *a,int *index,int flag, int fromIndex, int toIndex,char dir);
static void nsp_qsort_bp(double x[], int n, int p[],int flag, char dir);
static void nsp_qsort_double(double *a,int *tab, int flag, int n,char dir);

/**
 * nsp_matrix_sort:
 * @A: 
 * @Ind: 
 * @ind_flag: 
 * @dir: direction 'i' for increasing 'd' for decreasing 
 * 
 * global sort of the elements of a matrix. If flag is %TRUE 
 * Index is computed and returned 
 * 
 **/


int nsp_matrix_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir, nsp_sort type)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',1,A->mn) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  switch (type)
    {
    case sort_gb: 
      /* qsort Bruno */
      nsp_qsort_bp(A->R,A->mn,index,ind_flag,dir);break;
    case sort_gs:
      /* stable quick sort */
      nsp_qsort_stable(A->R,index,ind_flag,0,A->mn,dir); break;
    case sort_gm:
      /* merge sort */
      if ( nsp_mergesort(A->R,index,ind_flag,0,A->mn,dir)==FAIL) return FAIL;
      break;
    case sort_gd :
      /* non stable qsort */
      nsp_qsort_double(A->R,index,ind_flag,A->mn,dir);break;      
    default: 
      /* generic non stable qsort */
      CNAME(GlobalSort,double)(A->R,index,ind_flag,A->m,A->n,dir);break;
    }
  return OK;
}

int nsp_matrix_column_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return FAIL ;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  CNAME(ColSort,double)(A->R,index,ind_flag,A->m,A->n,dir);
  return OK;
}

int nsp_matrix_row_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,A->n) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  CNAME(RowSort,double)(A->R,index,ind_flag,A->m,A->n,dir);
  return OK;
}


int nsp_matrix_lexical_column_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir,char mode)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',1,A->n) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  if ( mode == 'i') 
    {
      A = Mat2int(A);
      CNAME(LexiCol,int)(A->I,index,ind_flag,A->m,A->n,dir);
    }
  else 
    {
      CNAME(LexiCol,double)(A->R,index,ind_flag,A->m,A->n,dir);
    }
  return OK;
}

int nsp_matrix_lexical_row_sort(NspMatrix *A,NspMatrix **Index,int ind_flag,char dir,char mode)
{
  int *index = NULL;
  if ( ind_flag == TRUE ) 
    {
      if (((*Index) = nsp_matrix_create(NVOID,'r',A->m,1) ) == NULLMAT ) return FAIL;
      (*Index)->convert='i';
      index = (*Index)->I;
    }
  if ( mode == 'i') 
    {
      A = Mat2int(A);
      CNAME(LexiRow,int)(A->I,index,ind_flag,A->m,A->n,dir);
    }
  else 
    {
      CNAME(LexiRow,double)(A->R,index,ind_flag,A->m,A->n,dir);
    }
  return OK;
}

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

static void qsort__(double *array,int *index, int flag, int from, int count);

/**
 * Compares two integers in natural order, since a - b is inadequate.
 *
 * @param a the first int
 * @param b the second int
 * @return &lt; 0, 0, or &gt; 0 accorting to the comparison
 */
/* #define WITHOUT_MACROS */

#ifdef WITHOUT_MACROS
static  int compare(double a,double b)
{
  return a < b ? -1 : a == b ? 0 : 1;
}
#else 
#define compare(a,b) ((a) < (b) ? -1 : (a) == (b) ? 0 : 1)
#endif 

/**
 * Finds the index of the median of three array elements.
 *
 * @param a the first index
 * @param b the second index
 * @param c the third index
 * @param d the array
 * @return the index (a, b, or c) which has the middle value of the three
 */

#ifdef WITHOUT_MACROS
static  int med3(int a, int b, int c, double *d)
{
  return (compare(d[a], d[b]) < 0
	  ? (compare(d[b], d[c]) < 0 ? b
	     : compare(d[a], d[c]) < 0 ? c : a)
	  : (compare(d[b], d[c]) > 0 ? b
	     : compare(d[a], d[c]) > 0 ? c : a));
}
#else 
#define med3(a,b,c,d) (compare(d[a], d[b]) < 0 \
	  ? (compare(d[b], d[c]) < 0 ? (b) \
	     : compare(d[a], d[c]) < 0 ? (c) : (a)) \
	  : (compare(d[b], d[c]) > 0 ? (b) \
	     : compare(d[a], d[c]) > 0 ? (c) : (a)));
#endif 

/**
 * Swaps the elements at two locations of an array
 *
 * @param i the first index
 * @param j the second index
 * @param a the array
 */

#ifdef WITHOUT_MACROS
static  void swap(int i, int j, double *a,int *index,int flag)
{
  double c = a[i];
  a[i] = a[j];
  a[j] = c;
  if ( index != NULL) 
    {
      int c1= index[i];
      index[i] = index[j];
      index[j] = c1;
    }
}
#else 
#define swap(i,j,a,index,flag) temp = (a)[i];(a)[i] = (a)[j]; (a)[j] = temp; if ( flag == TRUE ) \
 { int c1= (index)[i]; (index)[i] = (index)[j];  (index)[j] = c1; }
#endif 

/**
 * Swaps two ranges of an array.
 *
 * @param i the first range start
 * @param j the second range start
 * @param n the element count
 * @param a the array
 */

static  void vecswap(int i, int j, int n, double *a,int *index,int flag)
{
#ifndef WITHOUT_MACROS
  double temp;
#endif
  for ( ; n > 0; i++, j++, n--)
    {
      swap(i, j, a,index,flag);
    }
}


/**
 * Performs a stable sort on the elements, arranging them according to their
 * natural order.
 *
 * @param a the double array to sort
 * @param fromIndex the first index to sort (inclusive)
 * @param toIndex the last index to sort (exclusive)
 */

static void nsp_qsort_stable(double *a,int *index,int flag, int fromIndex, int toIndex,char dir)
{
#ifndef WITHOUT_MACROS
  double temp;
#endif
  int i,n = toIndex - fromIndex;
  if ( flag == TRUE) for ( i = fromIndex ; i < n  ; i++) index[i]= i+1;

  qsort__(a,index,flag, fromIndex, n);
  if ( dir == 'd' ) 
    {
      for ( i =fromIndex  ; i < n/2 ; i++) 
	{
	  swap(i,n-i-1,a,index,flag);
	}
    }
}

/**
 * Performs a recursive modified quicksort.
 *
 * @param array the array to sort
 * @param from the start index (inclusive)
 * @param count the number of elements to sort
 */

static void qsort__(double *array,int *index,int flag, int from, int count)
{
#ifndef WITHOUT_MACROS
  double temp;
#endif
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
  vecswap(from, b - span, span, array,index,flag);

  span = Min(d - c, hi - d - 1);
  vecswap(b, hi - span, span, array,index,flag);

  span = b - a;
  if (span > 1)
    qsort__(array,index,flag, from, span);

  span = d - c;
  if (span > 1)
    qsort__(array,index,flag, hi - span, span);
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

#define arraycopy(src,isrc,dest,idest,n) memcpy(dest+idest,src+isrc,(n)*sizeof(double)) 
#define iarraycopy(src,isrc,dest,idest,n) memcpy(dest+idest,src+isrc,(n)*sizeof(int)) 

static int nsp_mergesort(double *a,int *p,int flag, int fromIndex, int toIndex,char dir)
{
  NspMatrix *M=NULLMAT,*IM=NULLMAT;
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

  if ( flag == TRUE) for ( i = fromIndex ; i < toIndex -fromIndex  ; i++) p[i]=i+1;

  for (chunk = fromIndex; chunk < toIndex; chunk += 6)
    {
      int end = Min(chunk + 6, toIndex);
      for (i = chunk + 1; i < end; i++)
	{
	  if ( a[i - 1] > a[i] )
	    {
	      /* not already sorted */
	      int j = i, ielem;
	      double elem = a[j];
	      if ( flag == TRUE) ielem = p[j];
	      do
		{
		  a[j] = a[j - 1];
		  if ( flag == TRUE) p[j] = p[j-1];
		  j--;
		}
	      while (j > chunk  && a[j - 1] > elem );
	      a[j] = elem;
	      if ( flag == TRUE) p[j] = ielem; /* start at 1 */
	    }
	}
    }
  int len = toIndex - fromIndex;
  /* If length is smaller or equal 6 we are done. */
  if (len <= 6) goto end;

  src = a;
  if ((M = nsp_matrix_create(NVOID,'r',1,len) ) == NULLMAT ) return FAIL;
  dest = M->R;
  t = NULL; /* t is used for swapping src and dest */

  /* same for p */
  if ( flag == TRUE ) 
    {
      psrc = p;
      if ((IM = nsp_matrix_create(NVOID,'r',1,len) ) == NULLMAT ) return FAIL ;
      pdest = IM->I;
      pt = NULL; /* t is used for swapping src and dest */
    }

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
	      if ( flag == TRUE) iarraycopy(psrc,start, pdest, start + srcDestDiff, end - start);
	      /* The two halves just need swapping - no need to merge */
	    }
	  else if ( src[start] > src[end - 1] )
	    {
	      arraycopy(src, start, dest, end - size + srcDestDiff, size);
	      arraycopy(src, mid,dest, start + srcDestDiff, end - mid);
	      if ( flag == TRUE)
		{
		  iarraycopy(psrc, start, pdest, end - size + srcDestDiff, size);
		  iarraycopy(psrc, mid,pdest, start + srcDestDiff, end - mid);
		}
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
		  if (flag == TRUE ) pdest[i] = psrc[is];
		  i++;
		}
	      /*
	       * Finish up by copying the remainder of whichever half
	       * wasn't finished.
	       */
	      if (p1 < mid)
		{
		  arraycopy(src, p1, dest, i, mid - p1);
		  if (flag == TRUE ) iarraycopy(psrc, p1, pdest, i, mid - p1);
		}
	      else
		{
		  arraycopy(src, p2, dest, i, end - p2);
		  if (flag == TRUE ) iarraycopy(psrc, p2, pdest, i, end - p2);
		}
	    }
	}
      /* swap src and dest ready for the next merge */
      t = src;
      src = dest;
      dest = t;
      if ( flag == TRUE)
	{
	  pt=psrc;
	  psrc=pdest;
	  pdest=pt;
	}
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
      if (flag == TRUE ) iarraycopy(psrc, 0, p, srcDestDiff, toIndex);
    }

 end:
  if ( dir == 'd' ) 
    {
      double temp;
      int itemp,n=toIndex-fromIndex;
      for ( i =fromIndex  ; i < (n)/2 ; i++) 
	{
	  register int j= n-i-1; 
	  temp = (a)[i];(a)[i] = (a)[j]; (a)[j] = temp;
	  if ( flag  == TRUE )  {
	    itemp= p[i]; p[i] = p[j];  p[j] = itemp; 
	  }
	}
    }

  if (M != NULL)  nsp_matrix_destroy(M);
  if (flag == TRUE && IM != NULL)  nsp_matrix_destroy(IM);
  return OK;
}

/*
 * sort the double precision array x(1..n) in increasing order
 * and computes (if perm == 1) the permutation p of the sort :
 *
 *  x_sorted(i) = x(p(i))  1<=i<=n
 *
 * Author: B. Pincon (trying to accelerate the initial scilab dsort.f)
 * (i) n must be less than 2**(25) ! due to lengh of work space (ileft, iright)
 * (ii) quicksort is used with Sedgewick tricks
 */

#define SWAP(i,j)  temp = x[i]; x[i] = x[j]; x[j] = temp; \
                   if ( flag == TRUE) {itemp = p[i]; p[i] = p[j]; p[j] = itemp;}

#define SWITCH_VALUE 20

#define POP_segment(ia,ib) la--; if ( la >= 0 ) { ia = ileft[la]; ib = iright[la]; }
#define PUSH_segment(ia,ib) ileft[la] = (ia); iright[la] = (ib); la++

static void nsp_qsort_bp(double x[], int n, int p[],int flag,char dir )
{
  int ileft[25], iright[25]; /* to store parts (segments) of the array which stay to sort */
  int i, ia, ib, im, la, j, itemp;
  double temp, pivot;

  if ( flag == TRUE) for ( i = 0 ; i < n ; i++) p[i]=i+1;

  if ( n == 1 ) return;

  ia = 0; ib = n-1;  /* ia..ib is the current part (segment) of the array to sort */
  la = 0;

  while (la >= 0)   /* la >= 0  <=> stay one or some segments to sort */
    {
      if ( ib-ia < SWITCH_VALUE ) /* segment is short enough => insertion sort */
	{
	  for ( i = ia+1 ; i <= ib ; i++ )
	    {
	      j = i;
	      while ( j > ia  &&  x[j] < x[j-1] )
		{
		  SWAP(j,j-1);
		  j--;
		}
	    };
	  POP_segment(ia,ib);  /* get the next segment to sort if any */
	}
      else    /* quicksort */
	{
	  im = (ia+ib)/2;
	  SWAP(ia, im);
	  i = ia+1; j = ib;
	  if (x[i] > x[j])  { SWAP(i, j); }
	  if (x[ia] > x[j]) { SWAP(ia, j); }
	  else if (x[i] > x[ia]) { SWAP(ia, i); }
	  pivot = x[ia];
          /* at this point we have  x[i=ia+1] >= pivot (=x[ia]) >= x[j=ib]  */
	  while (1)
	    {
	      do i++;  while ( x[i] < pivot );
	      do j--;  while ( x[j] > pivot );
	      if (i >= j) break;
	      SWAP(i, j);
	    }
	  SWAP(ia, j);

	  /*  store the longer subdivision in workspace and    */
          /*  update the current segment to be sorted [ia..ib] */
	  if ( j-ia > ib-j )
	    { PUSH_segment(ia,j-1); ia = j+1; }
	  else
	    { PUSH_segment(j+1,ib); ib = j-1; }
	  if ( ib-ia <= 0)
	    { POP_segment(ia,ib); }
	}
    }
  if ( dir == 'd' ) 
    {
      for ( i =0   ; i < n/2 ; i++) 
	{
	  SWAP(i,n-i-1);
	}
    }

}


/*	$NetBSD: qsort.c,v 1.5 1995/12/28 08:52:36 thorpej Exp $	*/
/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *  Modified for Scilab Jean-Philippe Chancelier 
 *  to keep a permutation index 
 */

/*
 * Qsort routine from Bentley & McIlroy's "Engineering a Sort Function".
 * version for double without function 
 */

#define qs_swap(a,b) temp = *(a);*(a)=*(b),*(b)=temp;
#define qs_swapind(a,b) if ( flag== TRUE) {itemp = *(a);*(a)=*(b),*(b)=itemp;}
#define qs_cmp(a,b) (((*(a)) < (*(b))) ? -1 : ((*(a)) == (*(b)) ? 0 : 1)) 
#define qs_vecswap(a, b, n) if ((n) > 0) qs_swapcodedouble(a, b, n)
#define qs_vecswapind(a, b, n) if ((n) > 0 && flag == TRUE) qs_swapcodeint(a,b,n) 


#define qs_med3(res,tabres,a, b, c, xa,xb,xc) qs_cmp(a, b) < 0 ? \
         (qs_cmp(b, c) < 0 ? (res=b,tabres=xb) :  \
	  (qs_cmp(a, c) < 0 ? (res=c,tabres=xc) : (res=a,tabres=xa) )) \
	 :(qs_cmp(b, c) > 0 ? (res=b,tabres=xb) : (qs_cmp(a, c) < 0 ? (res=a,tabres=xa) : (res=c,tabres=xc) ))

static void qs_swapcodedouble(double *pi,double* pj,int n) 
{ 		
  do { 						
    register double t = *pi;		
    *pi++ = *pj;				
    *pj++ = t;				
  } while (--n > 0);				
}

static void qs_swapcodeint(int *pi,int* pj,int n) 
{ 		
  do { 						
    register int t = *pi;		
    *pi++ = *pj;				
    *pj++ = t;				
  } while (--n > 0);				
}

static void nsp_qsort_double__(double *a,int *tab, int flag, int n);

static void nsp_qsort_double(double *a,int *tab, int flag, int n,char dir)
{
  double temp;
  int i,itemp;
  nsp_qsort_double__(a,tab,flag,n);
  if ( dir == 'd' ) 
    {
      for ( i =0   ; i < n/2 ; i++) 
	{
	  qs_swap(a+i,a+n-i-1);
	  qs_swapind(tab+i,tab+n-i-1);
	}
    }
}

static void nsp_qsort_double__(double *a,int *tab, int flag, int n)
{
  double temp;
  int itemp;
  const int es=1,es1=1;
  double *pa, *pb, *pc, *pd, *pl, *pm, *pn;
  int *taba, *tabb, *tabc, *tabd, *tabl, *tabm, *tabn;
  int d,dind, r,r1,  swap_cnt,i;

  if ( flag == TRUE) for ( i = 0 ; i < n ; i++) tab[i]=i+1;
 
 loop:	
  swap_cnt = 0;
  if (n < 7) {
    for (pm = a + es, tabm= tab + es1 ; pm < a + n * es; pm += es, tabm +=es1 )
      {
	for (pl = pm, tabl= tabm ; pl > a && qs_cmp(pl - es, pl) > 0;  pl -= es, tabl -=es1)
	  {
	    qs_swapind(tabl,tabl- es1);
	    qs_swap(pl, pl - es);
	  }
      }
    return;
  }
  pm = a + (n / 2) * es;
  tabm = tab + (n / 2)*es1 ;
  if (n > 7) {
    pl = a; 
    tabl = tab;
    pn = a + (n - 1) * es; 
    tabn = tab + (n-1) *es1;
    if (n > 40) {
      dind= (n/8) *es1;
      d =   (n/8) *es;
      qs_med3(pl,tabl,pl, pl + d, pl + 2 * d, tabl, tabl + dind, tabl + 2 * dind);
      qs_med3(pm,tabm,pm - d, pm, pm + d, tabm - dind, tabm, tabm + dind);
      qs_med3(pn,tabn,pn - 2 * d, pn - d, pn, tabn - 2 * dind, tabn - dind, tabn);
    }
    qs_med3(pm,tabm,pl, pm, pn, tabl, tabm, tabn);
  }
  qs_swapind(tab,tabm);
  qs_swap(a, pm);

  pa = pb = a + es;
  pc = pd = a + (n - 1) * es;
  
  taba = tabb = tab + es1;
  tabc = tabd = tab + (n - 1) * es1;

  for (;;) {
    while (pb <= pc && (r = qs_cmp(pb, a)) <= 0) {
      if (r == 0) {
	swap_cnt = 1;
	qs_swapind(taba,tabb);
	taba +=es1;
	qs_swap(pa, pb);
	pa += es;
      }
      pb += es;
      tabb += es1;
    }
    while (pb <= pc && (r = qs_cmp(pc, a)) >= 0) {
      if (r == 0) {
	swap_cnt = 1;
	qs_swapind(tabc,tabd);
	tabd -= es1;
	qs_swap(pc, pd);
	pd -= es;
      }
      pc -= es;
      tabc -= es1;
    }
    if (pb > pc)
      break;
    qs_swapind(tabb,tabc);
    tabb += es1;
    tabc -= es1;
    qs_swap(pb, pc);
    swap_cnt = 1;
    pb += es;
    pc -= es;
  }

  if (swap_cnt == 0) {  /* Switch to insertion sort */
    for (pm = a + es, tabm= tab + es1 ; pm < a + n * es; pm += es, tabm +=es1)
      {
	for (pl = pm, tabl= tabm ; pl > a && qs_cmp(pl - es, pl) > 0;  pl -= es, tabl -=es1)
	  {
	    qs_swapind(tabl,tabl- es1);
	    qs_swap(pl, pl - es);
	  }
      }
    return;
  }

  pn = a + n * es;
  r = Min(pa - a, pb - pa);
  qs_vecswap(a, pb - r, r);

  tabn = tab + n*es1 ;
  r1 = Min(taba - tab, tabb - taba);
  qs_vecswapind(tab, tabb - r1, r1);

  r = Min(pd - pc, pn - pd - es);
  qs_vecswap(pb, pn - r, r);

  r1 = Min(tabd - tabc, tabn - tabd - es1 );
  qs_vecswapind(tabb, tabn - r1, r1);

  if ((r = pb - pa) > es )
    nsp_qsort_double__(a, tab,flag, r / es);
  if ((r = pd - pc) > es) { 
    /* Iterate rather than recurse to save stack space */
    a = pn - r;
    tab = tabn - (tabd - tabc);
    n = r / es;
    goto loop;
  }
  /*		qsort(pn - r, r / es, es, cmp);*/
}



