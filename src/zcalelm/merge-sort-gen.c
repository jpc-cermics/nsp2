/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 *
 * Generic code for Sorting Matrices a[i+n*j] 
 * This code is inserted in qsort.c 
 */

/* we want y to be expanded in CNAME thus we use XCNAME ! */

#define XCNAME(x,y) CNAME(x,y)

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

#undef arraycopy
#undef iarraycopy
#define arraycopy(src,isrc,dest,idest,n) memcpy(dest+idest,src+isrc,(n)*sizeof(ELT_TYPE)) 
#define iarraycopy(src,isrc,dest,idest,n) memcpy(dest+idest,src+isrc,(n)*sizeof(int)) 

#ifdef qs_swap
#undef qs_swap
#endif 
#define qs_swap(a,b) temp = *(a);*(a)=*(b),*(b)=temp;

#ifdef qs_swapind
#undef qs_swapind
#endif 
#define qs_swapind(a,b) if ( flag== TRUE) {itemp = *(a);*(a)=*(b),*(b)=itemp;}

#define SWITCH_VALUE 18

int XCNAME(nsp_mergesort_,ELT_TYPE)(ELT_TYPE *a,int *p,int flag, int fromIndex, int toIndex,char dir)
{
  NspMatrix *M=NULLMAT,*IM=NULLMAT;
  ELT_TYPE *src,*dest,*t;
  int *psrc=NULL,*pdest=NULL,*pt;
  int chunk,i,size,start;
#ifdef DOUBLE_ONLY
  int toIndex_i,n,j,itemp;
  ELT_TYPE temp;
#endif 
  /*
   * In general, the code attempts to be simple rather than fast, the
   * idea being that a good optimising JIT will be able to optimise it
   * better than I can, and if I try it will make it more confusing for
   * the JIT. First presort the array in chunks of length SWITCH_VALUE with insertion
   * sort. A mergesort would give too much overhead for this length.
   */

  if ( flag == TRUE) for ( i = fromIndex ; i < toIndex  ; i++) p[i]=i+1;

#ifdef DOUBLE_ONLY
  toIndex_i = toIndex;
  n = toIndex -fromIndex;
  for ( i = toIndex-1 , j = n ; i >= fromIndex ; i-- )
    if ( ISNAN(a[i]) )
      {
	j--; qs_swap(a+i,a+j); 
	if ( flag == TRUE) qs_swapind(p+i,p+j);
      } 
  toIndex = fromIndex + j ;
#endif 

  for (chunk = fromIndex; chunk < toIndex; chunk += SWITCH_VALUE)
    {
      int end = Min(chunk + SWITCH_VALUE, toIndex);
      for (i = chunk + 1; i < end; i++)
	{
	  if ( a[i - 1] > a[i] )
	    {
	      /* not already sorted */
	      int j = i, ielem=0;
	      ELT_TYPE elem = a[j];
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
  /* If length is smaller or equal SWITCH_VALUE we are done. */
  if (len <= SWITCH_VALUE) goto end;

  src = a;
  if ((M = nsp_matrix_create(NVOID,'r',1,len) ) == NULLMAT ) return FAIL;
  dest = (ELT_TYPE *) M->R;
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
  for ( size = SWITCH_VALUE; size < len; size <<= 1)
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
      ELT_TYPE temp;
      int itemp1,n;
#ifdef DOUBLE_ONLY
      n = toIndex_i -fromIndex; 
#else 
      n = toIndex-fromIndex;
#endif 
      for ( i =fromIndex  ; i < (n)/2 ; i++) 
	{
	  register int j= n-i-1; 
	  temp = (a)[i];(a)[i] = (a)[j]; (a)[j] = temp;
	  if ( flag  == TRUE )  {
	    itemp1= p[i]; p[i] = p[j];  p[j] = itemp1; 
	  }
	}
    }

  if (M != NULL)  nsp_matrix_destroy(M);
  if (flag == TRUE && IM != NULL)  nsp_matrix_destroy(IM);
  return OK;
}
