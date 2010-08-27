/* Nsp
 * Copyright (C) 1998-2010 Jean-Philippe Chancelier Enpc/Cermics
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
 * A generic qsort routine nsp_qsort 
 * which is specialize by including qsort-gen.c 
 * to matrix of double, int, string 
 *
 * The quicksort in this file is 
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 */

#include <stdlib.h>
#include <string.h>
#include <glib.h> /* for int64 */

#include <nsp/math.h> 
#include <nsp/gsort-p.h>

typedef int (*Swapf) (char *parmi,char* parmj,int n, int inc);

extern void nsp_qsort(char *a, char *tab,int flag, int n, int es, int es1, 
		      int (*cmp) (), Swapf swapcode,Swapf swapcodeind);

#define swapcodeind CNAME(swapcode,int)

/*
 * int arrays or matrices 
 */
                                                                                      
#define ELT_TYPE int
#include "qsort-gen.c"
#undef  ELT_TYPE

/*
 * double arrays or matrices 
 */

#define ELT_TYPE double
#define DOUBLE_ONLY
#include "qsort-gen.c"
#undef DOUBLE_ONLY
#undef  ELT_TYPE


/*
 * all the imatrix sub-types 
 */

#define ELT_TYPE gint
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gshort
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gushort
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE glong
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gulong
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gint8
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint8
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gint16
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint16
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gint32
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint32
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE gint64
#include "qsort-gen.c"
#undef  ELT_TYPE

#define ELT_TYPE guint64
#include "qsort-gen.c"
#undef  ELT_TYPE


  
/*
 * string matrices 
 */

#define ELT_TYPE nsp_string
#define STRING_ONLY
#include "qsort-gen.c"
#undef STRING_ONLY
#undef  ELT_TYPE

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
 */
#undef swapcodeind 
#define swap(a, b) swapcode(a, b, 1,es )
#define swapind(a, b)  if ( flag==1) swapcodeind(a,b,1, es1 )
#define vecswap(a, b, n) if ((n) > 0) swapcode(a, b, n/es,es)
#define vecswapind(a, b, n) if ((n) > 0 && flag == 1) swapcodeind(a,b,n/es1, es1) 

#define med3(res,tabres,a, b, c, xa,xb,xc,cmp) cmp(a, b) < 0 ?		\
  (cmp(b, c) < 0 ? (res=b,tabres=xb) :					\
   (cmp(a, c) < 0 ? (res=c,tabres=xc) : (res=a,tabres=xa) ))		\
    :(cmp(b, c) > 0 ? (res=b,tabres=xb) : (cmp(a, c) < 0 ? (res=a,tabres=xa) : (res=c,tabres=xc) ))

/*
 * L'index de permutation est gard'e dans tab avec size ds es1
 * on utilise aussi escode et es1code qui peuvent etre differents de 
 * es et es1 
 * par exemple si on veut pour une matrice faire un sort de chacune 
 * de ces lignes la matrice etant stockee par colonne a[i+n*j] 
 * on va faire n appels a nsp_qsort((char *) &a[i],....)
 * les increments es et es1 doivent etre de la taille n*sizeof(elt-de-a)
 * par contre swap code ne doit permuter que les elements de la ligne i
 * dans ce cas on donnera a escode et es1code la valeur N
 * sinon cas standard on leur donnera la valeur 1;
 */

void nsp_qsort(char *a, char *tab, int flag, int n, int es, int es1, int (*cmp)(),
	       Swapf swapcode,Swapf swapcodeind)
{
  char *pa, *pb, *pc, *pd, *pl, *pm, *pn;
  char *taba, *tabb, *tabc, *tabd, *tabl, *tabm, *tabn;
  int d,dind, r,r1,  swap_cnt;
 
 loop:	
  swap_cnt = 0;
  if (n < 7) {
    for (pm = a + es, tabm= tab + es1 ; pm < (char *) a + n * es; pm += es, tabm +=es1 )
      {
	for (pl = pm, tabl= tabm ; pl > (char *) a && cmp(pl - es, pl) > 0;  pl -= es, tabl -=es1)
	  {
	    swapind(tabl,tabl- es1);
	    swap(pl, pl - es);
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
      med3(pl,tabl,pl, pl + d, pl + 2 * d, tabl, tabl + dind, tabl + 2 * dind, cmp);
      med3(pm,tabm,pm - d, pm, pm + d, tabm - dind, tabm, tabm + dind, cmp);
      med3(pn,tabn,pn - 2 * d, pn - d, pn, tabn - 2 * dind, tabn - dind, tabn, cmp);
    }
    med3(pm,tabm,pl, pm, pn, tabl, tabm, tabn, cmp);
  }
  swapind(tab,tabm);
  swap(a, pm);

  pa = pb = a + es;
  pc = pd = a + (n - 1) * es;
  
  taba = tabb = tab + es1;
  tabc = tabd = tab + (n - 1) * es1;

  for (;;) {
    while (pb <= pc && (r = cmp(pb, a)) <= 0) {
      if (r == 0) {
	swap_cnt = 1;
	swapind(taba,tabb);
	taba +=es1;
	swap(pa, pb);
	pa += es;
      }
      pb += es;
      tabb += es1;
    }
    while (pb <= pc && (r = cmp(pc, a)) >= 0) {
      if (r == 0) {
	swap_cnt = 1;
	swapind(tabc,tabd);
	tabd -= es1;
	swap(pc, pd);
	pd -= es;
      }
      pc -= es;
      tabc -= es1;
    }
    if (pb > pc)
      break;
    swapind(tabb,tabc);
    tabb += es1;
    tabc -= es1;
    swap(pb, pc);
    swap_cnt = 1;
    pb += es;
    pc -= es;
  }

  if (swap_cnt == 0) {  /* Switch to insertion sort */
    for (pm = a + es, tabm= tab + es1 ; pm < (char *) a + n * es; pm += es, tabm +=es1)
      {
	for (pl = pm, tabl= tabm ; pl > (char *) a && cmp(pl - es, pl) > 0;  pl -= es, tabl -=es1)
	  {
	    swapind(tabl,tabl- es1);
	    swap(pl, pl - es);
	  }
      }
    return;
  }

  pn = a + n * es;
  r = Min(pa - (char *)a, pb - pa);
  vecswap(a, pb - r, r);

  tabn = tab + n*es1 ;
  r1 = Min(taba - (char *) tab, tabb - taba);
  vecswapind(tab, tabb - r1, r1);

  r = Min(pd - pc, pn - pd - es);
  vecswap(pb, pn - r, r);

  r1 = Min(tabd - tabc, tabn - tabd - es1 );
  vecswapind(tabb, tabn - r1, r1);

  if ((r = pb - pa) > es )
    nsp_qsort(a, tab,flag, r / es, es, es1, cmp,swapcode,swapcodeind);
  if ((r = pd - pc) > es) { 
    /* Iterate rather than recurse to save stack space */
    a = pn - r;
    tab = tabn - (tabd - tabc);
    n = r / es;
    goto loop;
  }
  /*		qsort(pn - r, r / es, es, cmp);*/
}



