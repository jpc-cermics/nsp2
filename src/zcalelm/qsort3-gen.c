/* Nsp
 * Copyright (C) 1998-2019 B. Pincon 
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
 * sort the ELT_TYPE array x(1..n) in increasing or decreasing order
 * and computes the permutation p of the sort:
 *
 * x_sorted(i) = x(p(i))  1<=i<=n
 *
 * this is a stable qsort with the easy way of using
 * the index array p so p must be always provided. 
 *
 * Author: B. Pincon
 * (i) n must be less than 2**(25) ! due to lengh of work space (ileft, iright)
 * (ii) quicksort is used with Sedgewick tricks
 *
 *
 * This file is not used directly but through include in qsort3.c 
 */

#define XCNAME(x,y) CNAME(x,y)


#ifdef qs_cmp
#undef qs_cmp
#endif 
#ifdef qs_cmp_pivot
#undef qs_cmp_pivot
#endif 

#ifdef STRING_ONLY 
#define qs_cmp(i,j) ( (c=strcmp(x[(i)],x[(j)])) < 0 || (c==0 && p[(i)]<p[(j)]) )
#define qs_cmp_pivot(j) ( (c=strcmp(pivot,x[(j)])) < 0 || (c==0 && ipiv<p[(j)]) )
#else
#ifdef COUPLE_ONLY
#define qs_cmp(ii,jj) ( x[(ii)].j<x[(jj)].j || (x[(ii)].j==x[(jj)].j && x[(ii)].i<x[(jj)].i) )
#define qs_cmp_pivot(jj) ( pivot.j<x[(jj)].j || (pivot.j==x[(jj)].j && pivot.i<x[(jj)].i) )
#else
#define qs_cmp(i,j) ( x[(i)]<x[(j)] || (x[(i)]==x[(j)] && p[(i)]<p[(j)]) )
#define qs_cmp_pivot(j) ( pivot<x[(j)] || (pivot==x[(j)] && ipiv<p[(j)]) )
#endif
#endif

#define SWAP(i,j)  temp = x[i]; x[i] = x[j]; x[j] = temp;	\
                  itemp = p[i]; p[i] = p[j]; p[j] = itemp;


#define SWITCH_VALUE 20

#define POP_segment(ia,ib) la--; if ( la >= 0 ) { ia = ileft[la]; ib = iright[la]; }
#define PUSH_segment(ia,ib) ileft[la] = (ia); iright[la] = (ib); la++

void XCNAME(nsp_sqsort_bp_,ELT_TYPE)(ELT_TYPE x[], int n, int p[],char dir )
{
  int ileft[25], iright[25]; /* to store parts (segments) of the array which stay to sort */
  int i, ia, ib, im, la, j, itemp, n_init = n;
  ELT_TYPE temp, pivot;
#ifdef STRING_ONLY 
  int c;
#endif
#ifndef COUPLE_ONLY
  int ipiv;
#endif 

  if ( dir == 'd' ) 
    for ( i = 0 ; i < n ; i++) p[i]=n - i;
  else
    for ( i = 0 ; i < n ; i++) p[i]=i+1;

#ifdef DOUBLE_ONLY
  for ( i = n-1 , j = n ; i >= 0 ; i-- )
    if ( ISNAN(x[i]) ) { j--; SWAP(i,j); } 
  n = j;
#endif

  if ( n > 1 )
    {
      ia = 0; ib = n-1;  /* ia..ib is the current part (segment) of the array to sort */
      la = 0;

      while (la >= 0)   /* la >= 0  <=> stay one or some segments to sort */
	{
	  if ( ib-ia < SWITCH_VALUE ) /* segment is short enough => insertion sort */
	    {
	      for ( i = ia+1 ; i <= ib ; i++ )
		{
		  j = i;
		  while ( j > ia  && qs_cmp(j,j-1) ) 
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
	      if ( qs_cmp(j,i) ) { SWAP(i, j); }
	      if ( qs_cmp(j,ia)) { SWAP(ia, j); }
	      else if ( qs_cmp(ia,i) ) { SWAP(ia, i); }
	      pivot = x[ia]; 
#ifndef COUPLE_ONLY
	      ipiv = p[ia];
#endif
	      /* at this point we have  x[i=ia+1] <= pivot (=x[ia]) <= x[j=ib]  */
	      while (1)
		{
		  do i++;  while ( i< n && ! qs_cmp_pivot(i) );
		  do j--;  while ( j >= 0 && qs_cmp_pivot(j) );
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
    }
  if ( dir == 'd' ) 
    {
      for ( i =0   ; i < n_init/2 ; i++) 
	{
	  SWAP(i,n-i-1);
	}
      for ( i = 0 ; i < n ; i++) p[i]=n - p[i] + 1;
    }
}
