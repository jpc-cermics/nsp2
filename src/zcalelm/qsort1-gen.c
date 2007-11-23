/*
 * this file is not used directly but inserted in qsort1.c 
 */ 

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
 *  Modified to deal with nan by Bruno Pincon (Nov 21, 2007)
 */

/*
 * Qsort routine from Bentley & McIlroy's "Engineering a Sort Function".
 * version for ELT_TYPE without function 
 */

/* we want y to be expanded in CNAME thus we use XCNAME ! */

#define XCNAME(x,y) CNAME(x,y)

#ifdef qs_swap
#undef qs_swap
#endif 
#define qs_swap(a,b) temp = *(a);*(a)=*(b),*(b)=temp;

#ifdef qs_swapind
#undef qs_swapind
#endif 
#define qs_swapind(a,b) if ( flag== TRUE) {itemp = *(a);*(a)=*(b),*(b)=itemp;}

#ifdef qs_cmp
#undef qs_cmp
#endif 
#ifdef STRING_ONLY 
#define qs_cmp(a,b) ( strcmp((*(a)),(*(b))) )
#else 
/* #ifdef DOUBLE_ONLY */
/* #define qs_cmp(a,b) ISNAN(*(a)) ? ( ISNAN(*(b)) ? 0 : 1 ) : ( ISNAN(*(b)) ? -1 : (((*(a)) < (*(b))) ? -1 : ((*(a)) == (*(b)) ? 0 : 1)) )  */
/* #else  */
#define qs_cmp(a,b) (((*(a)) < (*(b))) ? -1 : ((*(a)) == (*(b)) ? 0 : 1)) 
/* #endif  */
#endif

#ifdef qs_vecswap
#undef qs_vecswap
#endif 
#define qs_vecswap(a, b, n) if ((n) > 0) XCNAME(qs_swapcode_,ELT_TYPE)(a, b, n)
#ifdef qs_vecswapind
#undef qs_vecswapind
#endif 
#define qs_vecswapind(a, b, n) if ((n) > 0 && flag == TRUE) XCNAME(qs_swapcode_ind_,ELT_TYPE)(a,b,n) 

#ifdef qs_med3
#undef qs_med3
#endif 
#define qs_med3(res,tabres,a, b, c, xa,xb,xc) qs_cmp(a, b) < 0 ?	\
  (qs_cmp(b, c) < 0 ? (res=b,tabres=xb) :				\
   (qs_cmp(a, c) < 0 ? (res=c,tabres=xc) : (res=a,tabres=xa) ))		\
    :(qs_cmp(b, c) > 0 ? (res=b,tabres=xb) : (qs_cmp(a, c) < 0 ? (res=a,tabres=xa) : (res=c,tabres=xc) ))

static void XCNAME(qs_swapcode_,ELT_TYPE)(ELT_TYPE *pi,ELT_TYPE *pj,int n) 
{ 		
  do { 						
    register ELT_TYPE t = *pi;		
    *pi++ = *pj;				
    *pj++ = t;				
  } while (--n > 0);				
}

static void XCNAME(qs_swapcode_ind_,ELT_TYPE)(int *pi,int* pj,int n) 
{ 		
  do { 						
    register int t = *pi;		
    *pi++ = *pj;				
    *pj++ = t;				
  } while (--n > 0);				
}

static void XCNAME(nsp_internal_qsort_,ELT_TYPE)(ELT_TYPE *a,int *tab, int flag, int n);

/* exported one */
void XCNAME(nsp_qsort_,ELT_TYPE)(ELT_TYPE *a,int *tab, int flag, int n,char dir)
{
  ELT_TYPE temp;
  int i,itemp;
#ifdef DOUBLE_ONLY
  int j;
#endif

  if ( flag == TRUE) for ( i = 0 ; i < n ; i++) tab[i]=i+1;
#ifdef DOUBLE_ONLY
  for ( i = n-1 , j = n ; i >= 0 ; i-- )
    if ( ISNAN(a[i]) )
      {
	j--; qs_swap(a+i,a+j); 
	if ( flag == TRUE) qs_swapind(tab+i,tab+j);
      } 
  XCNAME(nsp_internal_qsort_,ELT_TYPE)(a,tab,flag,j);
#else
  XCNAME(nsp_internal_qsort_,ELT_TYPE)(a,tab,flag,n);
#endif
  if ( dir == 'd' ) 
    {
      for ( i =0   ; i < n/2 ; i++) 
	{
	  qs_swap(a+i,a+n-i-1);
	  if ( flag == TRUE) qs_swapind(tab+i,tab+n-i-1);
	}
    }
}

static void XCNAME(nsp_internal_qsort_,ELT_TYPE) (ELT_TYPE *a,int *tab, int flag, int n)
{
  ELT_TYPE temp;
  int itemp;
  const int es=1,es1=1;
  ELT_TYPE *pa, *pb, *pc, *pd, *pl, *pm, *pn;
  int *taba, *tabb, *tabc, *tabd, *tabl, *tabm, *tabn;
  int d,dind, r,r1,  swap_cnt;
 
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
    XCNAME(nsp_internal_qsort_,ELT_TYPE)(a, tab,flag, r / es);
  if ((r = pd - pc) > es) { 
    /* Iterate rather than recurse to save stack space */
    a = pn - r;
    tab = tabn - (tabd - tabc);
    n = r / es;
    goto loop;
  }
  /*		qsort(pn - r, r / es, es, cmp);*/
}



