/* Nsp
 * Copyright (C) 1998-2005 B. Pincon 
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
 * sort the double precision array x(1..n) in increasing order
 * and computes (if perm == 1) the permutation p of the sort :
 *
 * x_sorted(i) = x(p(i))  1<=i<=n
 *
 * Author: B. Pincon (trying to accelerate the initial scilab dsort.f)
 * (i) n must be less than 2**(25) ! due to lengh of work space (ileft, iright)
 * (ii) quicksort is used with Sedgewick tricks
 */

#define XCNAME(x,y) CNAME(x,y)

#define SWAP(i,j)  temp = x[i]; x[i] = x[j]; x[j] = temp; \
                   if ( flag == TRUE) {itemp = p[i]; p[i] = p[j]; p[j] = itemp;}

#define SWITCH_VALUE 20

#define POP_segment(ia,ib) la--; if ( la >= 0 ) { ia = ileft[la]; ib = iright[la]; }
#define PUSH_segment(ia,ib) ileft[la] = (ia); iright[la] = (ib); la++

void XCNAME(nsp_qsort_bp_,ELT_TYPE)(ELT_TYPE x[], int n, int p[],int flag,char dir )
{
  int ileft[25], iright[25]; /* to store parts (segments) of the array which stay to sort */
  int i, ia, ib, im, la, j, itemp;
  ELT_TYPE temp, pivot;

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
