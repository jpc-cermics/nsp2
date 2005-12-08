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

/* The hash table code at the end of the file is Copyrighted
 * See Copyright (c) 1990, 1993 The Regents of the University of California.
 * in the file. 
 */

/*
 * FIXME : check increasing size criteria 
 */

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "nsp/object.h"

/*
 * HashTable Object in Scilab : 
 *    store (key,value) in a hash table where key is always a string 
 *    and value is a pointer to a Scilab Object Obj
 */


/**
 * nsp_hcreate_from_list:
 * @name: Name of hash table or %NVOID
 * @nel: majorant of the table size or (-1)
 * @L: List from which to store hash table objects 
 * 
 * Create a hash table of initial size (>= nel) 
 * and stores the list objects in the table if they are 
 * named objects. If nel == -1 the list size is used.
 * 
 * Return value: a #NspHash 
 **/

NspHash *nsp_hcreate_from_list(char *name,unsigned int nel, NspList *L)
{
  NspHash *H;
  Cell *C;
  if ( L == NULLLIST ) return NULLHASH;
  if (nel == -1 ) nel = nsp_list_length(L);
  if(( H = nsp_hash_create(name,nel)) == NULLHASH) return NULLHASH;
  C= L->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ && strcmp(NSP_OBJECT(C->O)->name,NVOID) != 0)
	{
	  NspObject *Ob;
	  if (( Ob =nsp_object_copy_with_name(C->O)) == NULLOBJ ) return NULLHASH;
	  /* A copy of object is added in the hash table **/
	  if (nsp_hash_enter(H,Ob) == FAIL) return NULLHASH;
	}
      C = C->next ;
    }
  return H;
}  

/**
 *nsp_hash_resize:
 * @H: 
 * @new_size: 
 * 
 * Resizes a  HashTable
 *
 * Return value: %OK or %FAIL
 **/

int nsp_hash_resize(NspHash *H, unsigned int new_size)
{
  unsigned int i;
  NspHash *Loc;
  if ((Loc = nsp_hcreate(NVOID,new_size)) == NULLHASH ) return FAIL;
  for ( i =0 ; i <= H->hsize ; i++) 
    {
      Hash_Entry *loc = ((Hash_Entry *)H->htable) + i;
      if ( loc->used )
	{
	  if ( nsp_hsearch(Loc,nsp_object_get_name(loc->data),&loc->data,H_ENTER) == FAIL) 
	    return FAIL;
	}
    }
  FREE(H->htable);
  H->htable = Loc->htable;
  H->hsize  = Loc->hsize;
  H->filled = Loc->filled;
  FREE(NSP_OBJECT(Loc)->name);
  FREE(Loc);
  return OK;
}

/**
 *nsp_hash_merge:
 * @H1: 
 * @H2: 
 * 
 * Insert Copies of elements of has table H2 in hash table H1
 * 
 * Return value: %OK or %FAIL 
 **/

int nsp_hash_merge(NspHash *H1,NspHash *H2)
{
  unsigned int i;
  if ( H2 == NULLHASH ||  H2->filled == 0 ) return OK;
  if ( (i=H1->filled + H2->filled) >= 2*(H1->hsize/3) ) 
    {
      if (nsp_hash_resize(H1,2*i) == FAIL ) 
	{
	  Scierror("Error: running out of memory");
	  return FAIL;
	}
    }
  for ( i =0 ; i <= H2->hsize ; i++) 
    {
      Hash_Entry *loc = ((Hash_Entry *)H2->htable) + i;
      if ( loc->used )
	{
	  if ( nsp_hsearch(H1,nsp_object_get_name(loc->data),&loc->data,H_ENTER_COPY) 
	       == FAIL) 
	    return FAIL;
	}
    }
  return OK;
}

/**
 *nsp_hash_get_next_object:
 * @H: 
 * @i: 
 * @O: 
 * 
 * Used to walk through all the elements of the hash table 
 * return %FAIL when the end of the hash table is reached
 * The values present in the Hash table are returned 
 * in sequence (note that the key value is stored in the object) 
 * i is incremented at each call
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_hash_get_next_object(NspHash *H, int *i, NspObject **O)
{
  Hash_Entry *loc = ((Hash_Entry *) H->htable) + *i;  
  if ( loc->used ) 
    *O = loc->data ;
  else
    *O = NULLOBJ ;
  (*i)++;
  return ( (*i) >= (int) H->hsize +1   ) ? FAIL: OK ;
}

/**
 *nsp_hash_enter_copy:
 * @H: 
 * @O: 
 * 
 * Enters a copy of #NspObject O in the hash table
 * 
 * Return value: %OK or %FAIL
 **/
  
int nsp_hash_enter_copy(NspHash *H, NspObject *O)
{
  if ( O == NULLOBJ) return FAIL; 
  if ( H->filled >= 2*(H->hsize/3) ) 
    {
      if (nsp_hash_resize(H,2*H->hsize) == FAIL ) 
	{
	  Scierror("Error: running out of memory");
	  return FAIL;
	}
    }
  return( nsp_hsearch(H,nsp_object_get_name(O),&O,H_ENTER_COPY));
}

#define FAIL_FULL -2

/**
 *nsp_hash_enter:
 * @H: 
 * @O: 
 * 
 * Enters #NspObject O in the hash table
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_hash_enter(NspHash *H, NspObject *O)
{
  if ( O == NULLOBJ) return FAIL; 
  if ( H->filled >= 2*(H->hsize/3) ) 
    {
      if (nsp_hash_resize(H,2*H->hsize) == FAIL ) 
	{
	  Scierror("Error: running out of memory");
	  return FAIL;
	}
    }
  return( nsp_hsearch(H,nsp_object_get_name(O),&O,H_ENTER));
}

/**
 *nsp_hash_remove:
 * @H: 
 * @str: 
 * 
 * Remove entry with key str from Hash Table
 * 
 **/

void nsp_hash_remove(NspHash *H, char *str)
{
  NspObject *O= NULLOBJ;
  nsp_hsearch(H,str,&O,H_REMOVE);
}

/**
 *nsp_hash_find_and_copy:
 * @H: 
 * @str: 
 * @O: 
 * 
 * Search hash table entry with key str and returns a copy of it in #NspObject O
 *
 * Return value: %OK or %FAIL
 **/

int nsp_hash_find_and_copy(NspHash *H, char *str, NspObject **O)
{
  *O = NULLOBJ;
  return( nsp_hsearch(H,str,O,H_FIND_COPY));
}


/**
 *nsp_hash_find:
 * @H: 
 * @str: 
 * @O: 
 * 
 * Search hash table entry with key str and returns it in #NspObject O
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_hash_find(NspHash *H,const char *str, NspObject **O)
{
  *O = NULLOBJ;
  return( nsp_hsearch(H,str,O,H_FIND));
}


/**
 *nsp_hash_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * Return value: 
 **/

NspBMatrix  *nsp_hash_equal(NspHash *L1, NspHash *L2)
{
  NspBMatrix *B;
  NspObject *O1,*O2;
  int i=0, count=0;
  if ( L1->filled != L2->filled ) 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,1))== NULLBMAT) return NULLBMAT;
      B->B[0]=FALSE; 
      return B;
    }
  else 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,L1->filled))== NULLBMAT) return NULLBMAT;
      while (1) 
	{
	  if (nsp_hash_get_next_object(L1,&i,&O1) == FAIL ) break;
	  if ( O1 != NULLOBJ )
	    { 
	      if ( nsp_hash_find(L2,NSP_OBJECT(O1)->name,&O2) == FAIL)
		{
		  B->B[count]= FALSE;
		}
	      else 
		{
		  if ( O1->type->eq != NULL) 
		    B->B[count]= O1->type->eq(O1,O2);
		  else 		    
		    B->B[count]= FALSE;
		}
	      count++;
	    }
	}
    }
  return  B;
}


/**
 *nsp_hash_not_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
 **/


NspBMatrix  *nsp_hash_not_equal(NspHash *L1, NspHash *L2)
{
  NspBMatrix *B;
  NspObject *O1,*O2;
  int i=0,count=0;
  if ( L1->filled != L2->filled ) 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,1))== NULLBMAT) return NULLBMAT;
      B->B[0]=TRUE; 
      return B;
    }
  else 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,L1->filled))== NULLBMAT) return NULLBMAT;
      while (1) 
	{
	  if (nsp_hash_get_next_object(L1,&i,&O1) == FAIL ) break;
	  if ( O1 != NULLOBJ )
	    { 
	      if ( nsp_hash_find(L2,NSP_OBJECT(O1)->name,&O2) == FAIL)
		{
		  B->B[count]= TRUE;
		}
	      else 
		{
		  if ( O1->type->neq != NULL) 
		    B->B[count]= O1->type->neq(O1,O2);
		  else 		    
		    B->B[count]= TRUE;
		}
	      count++;
	    }
	}
    }
  return B;
} 


/**
 *nsp_hash_full_equal:
 * @L1: 
 * @L2: 
 * 
 * nsp_hash_equal(L1,L2)
 * if the two tables do not have the same length returns FALSE 
 * else returns and(L1(i)== L2(i)) 
 * 
 * 
 * Return value: 
 **/

int nsp_hash_full_equal(NspHash *L1, NspHash *L2)
{
  NspObject *O1,*O2;
  int i=0,rep=TRUE;
  if ( L1->filled != L2->filled ) return FALSE;
  while (1) 
    {
      if (nsp_hash_get_next_object(L1,&i,&O1) == FAIL ) break;
      if ( O1 != NULLOBJ )
	{ 
	  if ( nsp_hash_find(L2,NSP_OBJECT(O1)->name,&O2) == FAIL)
	    {
	      return FALSE;
	    }
	  else 
	    {
	      if ( O1->type->eq != NULL) 
		rep = O1->type->eq(O1,O2);
	      else 		    
		rep = FALSE;
	      if ( rep == FALSE) return rep;
	    }
	}
    }
  return rep;
} 

/**
 *nsp_hash_full_not_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_hash_full_not_equal(NspHash *L1, NspHash *L2)
{
  NspObject *O1,*O2;
  int i=0,rep=FALSE;
  if ( L1->filled != L2->filled ) return TRUE;
  while (1) 
    {
      if (nsp_hash_get_next_object(L1,&i,&O1) == FAIL ) break;
      if ( O1 != NULLOBJ )
	{ 
	  if ( nsp_hash_find(L2,NSP_OBJECT(O1)->name,&O2) == FAIL)
	    {
	      return TRUE;
	    }
	  else 
	    {
	      if ( O1->type->neq != NULL) 
		rep = O1->type->neq(O1,O2);
	      else 		    
		rep = TRUE;
	      if ( rep == TRUE) return rep;
	    }
	}
    }
  return rep;
} 



/*
 * Hashtable code : 
 * slightly modified to add REMOVE 
 * Jean-Philippe Chancelier ( Scilab Group )
 * 1998-1999
 */

/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
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
 *	@(#)search.h	8.1 (Berkeley) 6/4/93
 */

/* Copyright (C) 1993 Free Software Foundation, Inc.
 *  This file is part of the GNU C Library.
 *  Contributed by Ulrich Drepper <drepper@ira.uka.de>
 *
 *  The GNU C Library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *   License, or (at your option) any later version.
 *
 *  The GNU C Library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with the GNU C Library; see the file COPYING.LIB.  If
 *  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
 *  Cambridge, MA 02139, USA.  
 */


/*
 * [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
 * [Knuth]            The Art of Computer Programming, part 3 (6.4)
 */


/* 
 * For the used double hash method the table size has to be a prime. To
 * correct the user given table size we need a prime test.  This trivial
 * algorithm is adequate because
 * a)  the code is (most probably) only called once per program run and
 * b)  the number is small because the table must fit in the core
 */

static int isprime(unsigned int number)
{
  /* no even number will be passed */
  unsigned div = 3;
  
  while (div*div < number && number%div != 0)
    div += 2;
  
  return number%div != 0;
}

/**
 * nsp_hcreate:
 * @name: #NspHash object name 
 * @nel: initial size of the hash table object.
 * 
 * creates a #NspHash object with initial size greater than 
 * #nel. 
 * Before using the hash table we must allocate memory for it.
 * Test for an existing table are done. We allocate one element
 * more as the found prime number says. This is done for more effective
 * indexing as explained in the comment for the hsearch function.
 * The contents of the table is zeroed, especially the field used 
 * becomes zero.
 *  
 * Return value: a #NspHash object or %NULLHASH
 **/

NspHash *nsp_hcreate(char *name, unsigned int nel)
{
  NspHash *H = new_hash();
  Hash_Entry *htable;
  /* Change nel to the first prime number not smaller as nel. */
  nel |= 1;      /* make odd */
  while (!isprime(nel)) nel += 2;
  if ( H == NULLHASH)
    {
      Sciprintf("No more memory\n");
      return NULLHASH;
    }
  if ((NSP_OBJECT(H)->name =new_nsp_string(name))== NULLSTRING) return NULLHASH;
  NSP_OBJECT(H)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  H->hsize  = nel;
  H->filled = 0;
  if (( htable = (Hash_Entry *)calloc(H->hsize+1, sizeof(Hash_Entry))) == NULL)
    {
      Sciprintf("No more memory\n");
      return NULLHASH;
    }
  H->htable = htable ;
  return H;
}

/**
 * nsp_hdestroy:
 * @H: a #NspHash object 
 * 
 * free a #NspHash object but not the elements which were 
 * stored in the hash table. This function is not to be used 
 * directly but through nsp_hash_destroy() call.
 * 
 **/

void nsp_hdestroy(NspHash *H)
{
  /* free used memory */
  FREE(H->htable);
  FREE(NSP_OBJECT(H)->name);
  FREE(H);
}

/**
 * nsp_hsearch:
 * @H: #NspHash object 
 * @key: key to search in the hash table 
 * @data: a #NspObject pointer to be set with the searched object
 * @action: action to perform.
 * 
 * This is the search function. It uses double hashing with open adressing.
 * The argument item.key has to be a pointer to an zero terminated, most
 * probably strings of chars. The function for generating a number of the
 * strings is simple but fast. It can be replaced by a more complex function
 * like ajw (see [Aho,Sethi,Ullman]) if the needs are shown.
 *
 * We use an trick to speed up the lookup. The table is created by hcreate
 * with one more element available. This enables us to use the index zero
 * special. This index will never be used because we store the first hash
 * index in the field used where zero means not used. Every other value
 * means used. The used field can be used as a first fast comparison for
 * equality of the stored and the parameter value. This helps to prevent
 * unnecessary expensive calls of strcmp.
 * 
 * 
 * Return value: %OK, %FAIL. 
 **/

int nsp_hsearch(NspHash *H,const char *key, NspObject **data, HashOperation action)
{
  register unsigned hval;
  register unsigned hval2;
  register unsigned idx;
  register const char *str;
  Hash_Entry *htable = H->htable;

  /*
   * If table is full and another entry should be entered return with 
   * error.
   */
  if (action == H_ENTER && H->filled == H->hsize ) 
    {
      Scierror("Hash Table %s is full\n",NSP_OBJECT(H)->name);
      return FAIL_FULL;
    }

  /* Compute a value for the given string. Perhaps use a better method. */
  /* modifs (bruno) : avoid the call to strlen and put the modulo outside the loop */
  hval  = 33;
  str = key;
  while (*str != '\0') { hval += *str ; str++; }
  hval %= H->hsize;

  /* First hash function: simply take the modulo but prevent zero. */
  if (hval == 0) hval++;

  /* The first index tried. */
  idx = hval;

  if (htable[idx].used) 
    {
      /* Further action might be required according to the action value. */
      /* Sciprintf("First  hash Testing idx=%d\n",idx); */
      if (htable[idx].used == hval )
	{
	  if ( Ocheckname(htable[idx].data,key) ) 
	    {
	      switch (action) 
		{
		case H_REMOVE :
		  htable[idx].used = 0;
		  nsp_object_destroy(&htable[idx].data);
		  htable[idx].data = NULLOBJ;
		  (H->filled)--;
		  return OK ;
		  break;
		case H_ENTER_COPY :
		  if (htable[idx].data != NULLOBJ) 
		    nsp_object_destroy(&htable[idx].data);
		  htable[idx].data =nsp_object_copy_with_name(*data);
		  if (htable[idx].data == NULLOBJ) 
		    {
		      Sciprintf("Error: No more memory\n");
		      htable[idx].used = 0;
		      (H->filled)--;
		      return FAIL;
		    }
		  return OK;
		case H_ENTER: 
		  if (htable[idx].data != NULLOBJ) 
		    nsp_object_destroy(&htable[idx].data);
		  htable[idx].data = *data;
		  return OK;
		case H_FIND_COPY :
		  *data=nsp_object_copy(htable[idx].data);
		  return OK;
		case H_FIND :
		  *data= htable[idx].data;
		  return OK;
		}
	    }
	}
	
      /* Second hash function, as suggested in [Knuth] */

      hval2 = 1 + hval % (H->hsize-2);
	
      do {
	/* 
	 * Because hsize is prime this guarantees to step through all
	 * available indeces.
	 */
	if (idx <= hval2)
	  idx = H->hsize+idx-hval2;
	else
	  idx -= hval2;

	/* Sciprintf("2nd hash Testing idx=%d\n",idx); */
	/* If entry is found use it. */
	if (htable[idx].used == hval ) 
	  {
	    if ( Ocheckname(htable[idx].data,key)) 
	      {
		switch (action) 
		  {
		  case H_REMOVE :
		    htable[idx].used = 0;
		    nsp_object_destroy(&htable[idx].data);
		    htable[idx].data = NULLOBJ;
		    (H->filled)--;
		    return OK;
		    break;
		  case H_ENTER_COPY :
		    if (htable[idx].data != NULLOBJ) 
		      nsp_object_destroy(&htable[idx].data);
		    htable[idx].data =nsp_object_copy_with_name(*data);
		    if (htable[idx].data == NULLOBJ) 
		      {
			Sciprintf("No more memory\n");
			htable[idx].used =0;
			(H->filled)--;
			return FAIL;
		      }
		    return OK;
		  case H_ENTER :
		    if (htable[idx].data != NULLOBJ) 
		      nsp_object_destroy(&htable[idx].data);
		    htable[idx].data = *data;
		    return OK;
		  case H_FIND_COPY :
		    *data=nsp_object_copy(htable[idx].data);
		    return OK;
		  case H_FIND :
		    *data= htable[idx].data;
		    return OK;
		  }
	      }
	  }
      } while (htable[idx].used);
	
    }

  /* Sciprintf("End of hash search idx=%d must be free \n",idx); **/
    
  /* An empty bucket has been found. */
  
  if (action == H_ENTER_COPY ) 
    {
      htable[idx].data =nsp_object_copy_with_name(*data);
      if (htable[idx].data == NULLOBJ) 
	{
	  Sciprintf("No more memory\n");
	  return FAIL;
	}
      htable[idx].used  = hval;
      (H->filled)++;
      return OK ;
    }
  else if (action == H_ENTER )
    {
      htable[idx].data = *data;
      htable[idx].used  = hval;
      (H->filled)++;
      return OK ;
    }
  else 
    return FAIL;
}








