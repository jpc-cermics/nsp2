/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
 *
 * See also the Copyright below for hash table routines 
 *
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

/************************************************************
 * Hash Table for storing scilab function informations 
 * The data associated to a function is the couple (Int,Num) 
 *     where Int is an interface number and Num the id of 
 *     the function in interface number Int.
 * 
 *     InitFunctionTable() : initialize Hash Table storing 
 *                  initial set of function.
 *     int EnterFunction(str,Int,Num) : add new function in hash table.
 *                  or change data if function str was already in the table
 *     void DeleteFunction(str) : delete entry from its name. 
 *     void DeleteFunctionS(Int) : delete functions from interface Int
 *     FindFunction(str,Int,Num) : find entry from its name.
 *     int FindFunctionB(key,Int, Num) : find entry from (Int,Num) 
 *                          by traversal of the whole table.
 * 
 * MAXTAB must be set to 2*(the number of primitives)
 ************************************************************/

#include <string.h>
#include <stdio.h>

#include "nsp/machine.h"
#include "nsp/math.h" 
#include "nsp/plisttoken.h" /** for name_maxl **/
#include "FunTab.h"
#include "callfunc.h" 

/********************************************
 * MAXTAB : maximum number of entries in the htable 
 * in fact  myhcreate use a prime > MAXTAB
 * WARNING : MAXTAB must be chosen > 2* the number of 
 * expected entries for good efficiency of the hash code 
 ********************************************/

#define MAXTAB 2048

typedef enum {
  FIND, ENTER,REMOVE
} ACTION;

typedef struct fdata {
  int Int;
  int Num;
} Fdata;

static int   scifunc_hcreate (unsigned int);
/* static void	 scifunc_hdestroy(); */
static int   scifunc_hsearch (char *str,Fdata *d,ACTION);
static int   Eqid (char *x,char *y);
void  InitFunctionTable  (void);

#ifdef TEST /********************* test part ***/

void test1()
{
  int j=0,Int,Num;
  printf("Testing \n");
  printf("===================== \n");
  while ( SciFuncs[j].name != (char *) 0 && j < 10 )
    {
      if ( FindFunction(SciFuncs[j].name,&Int,&Num) == FAIL )
	printf(" %s not found \n",SciFuncs[j].name);
      else
	printf(" %s found [%d,%d] \n",SciFuncs[j].name,Int,Num);
      j++;
    }
}

int test_hash()
{
  int j=0,k;
  InitFunctionTable();
  test1();
  for ( k = 1; k < 2 ; k++) 
    {
      printf("performing delete for the first five\n");
      printf("===================== \n");
      j=0;
      while ( SciFuncs[j].name != (char *) 0 && j < 5 )
	{
	  DeleteFunction(SciFuncs[j].name);
	  j++;
	}
      test1();
      printf("Restore functions\n");
      printf("===================== \n");
      j=0;k=0;
      while ( SciFuncs[j].name != (char *) 0 && j < 10 )
	{
	  EnterFunction(SciFuncs[j].name,SciFuncs[j].Int,k);k++;
	  j++;
	}
      test1();
      printf("entering functions with new data\n");
      printf("===================== \n");
      j=0;k=0;
      while ( SciFuncs[j].name != (char *) 0 && j < 10 )
	{
	  EnterFunction(SciFuncs[j].name,-23,k);k++;
	  j++;
	}
      test1();
      printf("Remove interface\n");
      printf("===================== \n");
      DeleteFunctionS(-23);
      test1();
    }
  return(0);
}

void ShowTable();

int main()
{
  test_hash();
  ShowTable();
  return 0;
}


#endif  /********************* end of test part ***/

/***************************************
 * Enter function in Hash Table   
 * or change data if function was already in the table
 ***************************************/

int EnterFunction(char *str, int Int, int Num)
{
  Fdata data;
  data.Int = Int ;
  data.Num = Num ;
  return( scifunc_hsearch(str,&data,ENTER));
}

/***************************************
 * Remove function from Hash Table 
 ***************************************/

void DeleteFunction(char *str)
{
  Fdata data;
  scifunc_hsearch(str,&data,REMOVE);
}

/***************************************
 * Search function in Hast table given key str
 ***************************************/

int FindFunction(char *str, int *Int, int *Num)
{
  int r;
  Fdata data;
  r= scifunc_hsearch(str,&data,FIND);
  *Int = data.Int;
  *Num = data.Num;
  return r;
}

/***************************************
 * Initialize Hash Table on first call 
 * or reset htable to its initial state on 
 ***************************************/

void  InitFunctionTable(void)
{
  static int firstentry = 0;
  int i=0,k=0;
  if ( firstentry == 1 ) return ;
  /** first call **/
  if ( scifunc_hcreate(MAXTAB) == 0 ) 
    {
      printf("Fatal Error: Can't create table for scilab functions (not enough memory)\n");
      exit(1);
    }
  while (1) 
    {
      /** interfaces **/
      interface_info *info = Interfaces[i].info;
      if ( info == NULL) break;
      k=0;
      while (1) 
	{
	  /** function in the interface **/
	  char *fname;
	  function *f;
	  (*info)(k,&fname,&f);
	  if ( fname == NULL) break;
	  if ( EnterFunction(fname,i,k) == FAIL)
	    {
	      printf("Fatal Error : Table for scilab functions is too small \n");
	      exit(1);
	    }	  
	  k++;
	}
      i++;
    }
  firstentry = 1;
}

/************************************************
 * Hashtable code : 
 * slightly modified to add DELETE 
 * Jean-Philippe Chancelier Cermics/enpc
 ************************************************/

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


/* Backward compatibility to hsearch interface. */

typedef struct entry {
  char key[NAME_MAXL];
  Fdata data;
} ENTRY;

/* Copyright (C) 1993 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@ira.uka.de>

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */


/*
 * [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
 * [Knuth]            The Art of Computer Programming, part 3 (6.4)
 */


/*
 * We need a local static variable which contains the pointer to the
 * allocated memory for the hash table. An entry in this table contains
 * an ENTRY and a flag for usage.
 */

typedef struct { 
    unsigned int   used;
    ENTRY entry;
} _ENTRY;

static _ENTRY   * htable = NULL;
static unsigned   hsize;
static unsigned   filled;


/* 
 * For the used double hash method the table size has to be a prime. To
 * correct the user given table size we need a prime test.  This trivial
 * algorithm is adequate because
 * a)  the code is (most probably) only called once per program run and
 * b)  the number is small because the table must fit in the core
 */

static int
isprime(unsigned int number)
{
    /* no even number will be passed */
    unsigned div = 3;

    while (div*div < number && number%div != 0)
        div += 2;

    return number%div != 0;
}

/*
 * Before using the hash table we must allocate memory for it.
 * Test for an existing table are done. We allocate one element
 * more as the found prime number says. This is done for more effective
 * indexing as explained in the comment for the hsearch function.
 * The contents of the table is zeroed, especially the field used 
 * becomes zero.
 */

static int scifunc_hcreate(unsigned int nel)
{
    /* There is still another table active. Return with error. */
    if (htable != NULL)
	return 0;

    /* Change nel to the first prime number not smaller as nel. */
    nel |= 1;      /* make odd */
    while (!isprime(nel)) nel += 2;

    hsize  = nel;
    filled = 0;
    /* printf(" Size of hTable %d\n",nel); */
    /* allocate memory and zero out */
    /* note that htable has hsize +1 entries */
    if ((htable = (_ENTRY *)calloc(hsize+1, sizeof(_ENTRY))) == NULL)
	return 0;

    /* everything went alright */
    return 1;
}

/*
 * After using the hash table it has to be destroyed. The used memory can
 * be freed and the local static variable can be marked as not used.
 */
/** Unused : cleaned at scilab exit 

static void
scifunc_hdestroy()
{
    / * free used memory * /
    free(htable);
    / * the sign for an existing table is a value != NULL in htable * / 
    htable = NULL;
}
**/

/** from data to key : find function given (Int and Num ) **/

int FindFunctionB(char *key, int Int, int Num)
{
  unsigned int i;
  for ( i = 0 ; i <= hsize ; i++ ) 
    if ( htable[i].used 
	 &&  htable[i].entry.data.Int == Int
	 &&  htable[i].entry.data.Num == Num
	 )
      {
	strncpy(key,htable[i].entry.key,NAME_MAXL);
	return(1);
      }
  return(0);
}

/** print the whole table : used for testing **/

void ShowTable(void)
{
  unsigned int i;
  printf("Whole Table\n");
  for ( i = 0 ; i <= hsize ; i++ ) 
    if ( htable[i].used )
      printf("%s %d %d \n",htable[i].entry.key,
	     htable[i].entry.data.Int,htable[i].entry.data.Num);
}

/*
 * Delete entries associated to interface number Int : by walking through 
 *  the whole hash table 
 */

void DeleteFunctionS(int Int)
{
  unsigned int i;
  for ( i = 0 ; i <= hsize ; i++ ) 
    if ( htable[i].used 
	 &&  htable[i].entry.data.Int == Int
	 )
      {
	htable[i].used = 0;
	filled--;
      }
}


/*****************************************************************************
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
 ******************************************************************************/


static int scifunc_hsearch(char *key, Fdata *data, ACTION action)
{
  register unsigned hval;
  register unsigned hval2;
  register unsigned len = NAME_MAXL;
  register unsigned idx;
  register char *str;

    /*
     * If table is full and another entry should be entered return with 
     * error.
     */
    if (action == ENTER && filled == hsize) 
        return FAIL;

    /* Compute a value for the given string. Perhaps use a better method. */
    /* modifs (bruno) : avoid the call to strlen and put the modulo outside the loop */
    /*                  then compute hval % hsize efficiently */
    hval  = len;
    str = key;
    while (*str != '\0') { hval += *str ; str++; }
    /* compute hval %= hsize efficiently */
    if ( hval >= hsize)
      {
	hval -= hsize;
	if ( hval >= hsize)
	  hval %= hsize;
      }

    /* First hash function: simply take the modul but prevent zero. */
    if (hval == 0) hval++;

    /* The first index tried. */
    idx = hval;

    if (htable[idx].used) 
      {
	/* Further action might be required according to the action value. */
	
	if (htable[idx].used == hval )
	  {
	    if ( Eqid(key, htable[idx].entry.key) == 0) 
	      {
		switch (action) 
		  {
		  case REMOVE :
		    htable[idx].used = 0;
		    filled--;
		    return OK ;
		    break;
		  case ENTER :
		    htable[idx].entry.data.Num = data->Num; 
		    htable[idx].entry.data.Int = data->Int; 
		    return OK;
		  case FIND :
		    data->Num = htable[idx].entry.data.Num;
		    data->Int = htable[idx].entry.data.Int;
		    return OK;
		  }
	      }
	  }
	
	/* Second hash function, as suggested in [Knuth] */

        hval2 = 1 + hval % (hsize-2);
	
        do {
	    /* 
	     * Because hsize is prime this guarantees to step through all
             * available indeces.
	     */
            if (idx <= hval2)
	        idx = hsize+idx-hval2;
	    else
	        idx -= hval2;

            /* If entry is found use it. */
            if (htable[idx].used == hval ) 
	      {
                if ( Eqid(key, htable[idx].entry.key) == 0) 
		  {
		    switch (action) 
		      {
		      case REMOVE :
			htable[idx].used = 0;
			filled--;
			return OK;
			break;
		      case ENTER :
			htable[idx].entry.data.Num = data->Num; 
			htable[idx].entry.data.Int = data->Int; 
			return OK;
		      case FIND :
			data->Num = htable[idx].entry.data.Num;
			data->Int = htable[idx].entry.data.Int;
			return OK;
		      }
		  }
	      }
	  } while (htable[idx].used);
      }
    
    /* An empty bucket has been found. */

    if (action == ENTER) 
      {
        htable[idx].used  = hval;
	strncpy(htable[idx].entry.key,key,NAME_MAXL);
	htable[idx].entry.data.Num = data->Num; 
	htable[idx].entry.data.Int = data->Int; 
	filled++;
        return OK ;
      }
    else
      return FAIL;
}


static int Eqid(char *x, char *y)
{
  return strncmp(x,y,NAME_MAXL);
}

