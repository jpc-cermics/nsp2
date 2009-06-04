/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * 
 * Htable for macros 
 * Htable for functions
 * FIXME: work in progress 
 *********************************************************************/

#include <string.h>
#include <math.h>
#include <stdio.h>
#include <glib.h>

#include "nsp/machine.h"
#include "nsp/math.h" 
#include "nsp/plisttoken.h" /** for name_maxl **/
#include "nsp/object.h" 
#include "nsp/interf.h"
#include "nsp/datas.h"
#include "../system/files.h" /* FSIZE+1 */
#include "../functions/callfunc.h" 
#include "../functions/addinter.h" 
#include "Functions.h"
#include "Eval.h"

/*
 * maximum number of entries in the htable 
 * in fact myhcreate use a prime > MAXTAB
 * WARNING : MAXTAB must be chosen > 2* the number of 
 * expected entries 
 * for good efficiency of the hash code 
 */

#define MAXTAB 4096

typedef enum {
  FIND, ENTER,REMOVE
} ACTION;

typedef struct mdata {
  int Int;
} Mdata;

static int   myhcreate (unsigned int);
/* static void	 myhdestroy(); */
static int   myhsearch (char *str,Mdata *d,ACTION);

/* table of directory that contains searched 
 * macros
 */

static NspSMatrix *LibDirs = NULLSMAT;

/* hash table of preloaded macros.
 */

static NspHash* macros_cache = NULLHASH;

/**
 * nsp_get_libdir:
 * @num: 
 * 
 * get the directory name corresponding to id @num.
 * 
 * Return value: %NULL or a pointer to the directory name.
 **/

const char *nsp_get_libdir(int num)
{
  if ( LibDirs != NULLSMAT && num >=0 && num < LibDirs->mn -1 )
    return LibDirs->S[num];
  else 
    return NULL;
}


/**
 *nsp_enter_macros:
 * @dirname: a string giving an absolute dire name
 * @recursive: a flag %TRUE or %FALSE 
 * @compile: a flag %TRUE or %FALSE 
 * 
 * Insert @dirname in the search list for macros 
 * file *.bin are searched in dirname and inserted 
 * in a hash table. If @compile is %TRUE file with a .sci 
 * suffix are first parsed and saved as binary files *.bin 
 * (one file for each function). 
 * If @recursive is %TRUE directories are recursively added. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_enter_macros(const char *dir_name,int recursive,int compile)
{
  char dirname[FSIZE+1];
  Mdata data;
  char filename[FSIZE+1];
  int  flen, flag=-1,i;
  nsp_macro_table_reset_cache();
  /* recursively add search directories */
  /* expand macros in dir_name -> dirname */
  nsp_path_expand(dir_name,dirname,FSIZE);
  GDir *dir =  g_dir_open(dirname,0,NULL);
  if ( dir == NULL) 
    {
      Scierror("Error:\t:Can't open directory %s\n",dirname);
      return FAIL;
    }

  /* is dirname in the directory array
   */
  for ( i = 0 ; i < LibDirs->mn -1 ; i++) 
    {
      if (strcmp(dirname,LibDirs->S[i])==0) 
	{
	  flag=i;break;
	}
    }
  
  /* Create a new entry for dir if not already present */
  if ( flag == -1 ) 
    {
      flag = LibDirs->mn -1 ;
      if (nsp_smatrix_add_rows(LibDirs,1)==FAIL) return FAIL;
      if (nsp_string_resize(&(LibDirs->S[flag]),strlen(dirname)) == FAIL)
	return FAIL;
      strcpy(LibDirs->S[flag],dirname);
    }

  /* update binaries if requested 
   */
  if ( compile == TRUE )nsp_parse_eval_dir_full(dirname);
  
  strcpy(filename,dirname);
  flen=strlen(filename);
  while (1) 
    {
      const gchar *fname=  g_dir_read_name(dir);
      if (fname == NULL) break;
      filename[flen]='/'; 
      filename[flen+1]='\0'; 
      strcat(filename,fname);
      if (g_file_test (filename, G_FILE_TEST_IS_DIR))
	{
	  if ( recursive == TRUE ) 
	    {
	      /* Sciprintf("%s visited\n",filename); */
	      nsp_enter_macros(filename,recursive,compile);
	    }
	}
      else 
	{
	  if ( strlen(fname) >= 4 && strncmp(".bin",fname + strlen(fname)-4,4)==0)
	    {
	      char name[NAME_MAXL];
	      strcpy(name,fname);
	      name[strlen(fname)-4]='\0';
	      data.Int = flag;
	      if ( myhsearch(name,&data,ENTER) == FAIL ) 
		{
		  Scierror("Error:\t: htable is full increase default size in LibsTab.c\n");
		  g_dir_close (dir);
		  return FAIL;
		}
	    }
	}
    } 
  g_dir_close (dir);
  return OK;
}

/**
 * nsp_delete_macros:
 * @Dir: directory name (absolute path).
 * 
 * Remove all the macros associated to directory @dir from the macro hash table.
 *
 * Return value: %OK or %FAIL
 **/

int nsp_delete_macros(const char *Dir)
{
  char F[FSIZE+1];
  FILE *f;
  int i,flag=-1;
  /* Search if we already know directory Dir **/
  for ( i = 0 ; i < LibDirs->mn -1 ; i++) 
    {
      if (strcmp(Dir,LibDirs->S[i])==0) 
	{
	  flag=i;break;
	}
    }
  if ( flag == -1 ) 
    {
      /* nothing to do */
      return OK;
    }
  /* if flag != -1 : we keep the dir name in LibDirs */
  /* Open the file Dir/names  **/
  strcpy(F,Dir); strcat(F,"/names");
  if (( f= fopen(F,"r") ) == (FILE *)0 )
    {
      Scierror("Error:\t:Can't open file %s\n",F);
      return FAIL;
    }
  while (1) 
    {
      Mdata data;
      int rep;
      char name[NAME_MAXL];
      rep = fscanf(f,"%s",name);
      if ( rep == 0 || rep == EOF ) break;
      myhsearch(name,&data,REMOVE);
    }
  fclose(f);
  return OK;
}

/**
 * nsp_find_macro:
 * @str: 
 * 
 * tries to find a macros named @str in the macros table. 
 * If found the macro code is preloaded in a cache and the 
 * macros is returned.
 * 
 * Return value: %NULLOBJ or an %NspObject filled with the macro code.
 **/

NspObject *nsp_find_macro(char *str)
{
  NspObject *Ob;
  Mdata data;
  if ( nsp_hash_find(macros_cache,str,&Ob) == OK) 
    {
      /* Sciprintf("Macro %s found in the cache\n"); */
      return Ob;
    }
  if ( myhsearch(str,&data,FIND) == OK ) 
    {
      int found=FALSE;
      NspFile *F;
      char Name[FSIZE+1];
      if ( data.Int < 0 ) return NULLOBJ;
      sprintf(Name,"%s/%s.bin",LibDirs->S[data.Int],str);
      if (( F =nsp_file_open_xdr_r(Name)) == NULLSCIFILE) return NULLOBJ;
      /* bin files are supposed to contain only one object */
      while (1) 
	{
	  NspObject *Ob1;
	  if ((Ob1=nsp_object_xdr_load(F->obj->xdrs))== NULLOBJ ) break;
	  if ( strcmp(nsp_object_get_name(Ob1),str)== 0)
	    {
	      found = TRUE;
	      ((NspPList *) Ob1)->dir = data.Int;
	      Ob = Ob1;
	    }
	}
      if (nsp_file_close_xdr_r(F) == FAIL) 
	{
	  nsp_file_destroy(F);
	  return NULLOBJ;
	}
      nsp_file_destroy(F);

      /* Sciprintf("Macro %s found in %s/%s.bin\n",str,LibDirs->S[data.Int],str);
	 Sciprintf("insert %s in a cache \n",str);
      */
      /* enter macro  in the cache */
      if ( found == TRUE )
	{
	  nsp_hash_enter(macros_cache,Ob);
	  return Ob; 
	}
      else 
	{
	  Sciprintf("Macro %s not found in %s/%s.bin !!!!\n",str,LibDirs->S[data.Int],str);
	  return NULLOBJ;
	}
    }
  return NULLOBJ;
}

/**
 * nsp_init_macro_table:
 * @void: 
 * 
 * initialize macro table.
 **/

void nsp_init_macro_table(void)
{
  static int firstentry = 0;
  if ( firstentry != 0 ) return;
  /* a cache */
  if ( (macros_cache = nsp_hcreate("cache",100)) == NULLHASH ) 
    {
      Sciprintf("Fatal Error:\tCan't create table for Scilab libraries\n");
      exit(1);
    }
  if ( myhcreate(MAXTAB) == 0 ) 
    {
      Sciprintf("Fatal Error:\tCan't create table for Scilab libraries\n");
      exit(1);
    }
  if ((LibDirs =nsp_smatrix_create("libs",1,1,(char *)0,0))== NULLSMAT) 
    {
      Sciprintf("Fatal Error:\tCan't create table for Scilab libraries\n");
      exit(1);
    }
  firstentry = 1;
}

/**
 * nsp_macro_table_reset_cache:
 * 
 * reset the macros cache 
 */

void nsp_macro_table_reset_cache(void)
{
  NspHash *new_cache;
  if (macros_cache == NULLHASH) return;
  if ((new_cache = nsp_hcreate("cache",100)) == NULLHASH ) 
    {
      Sciprintf("Error: cannot reset the macros cache, no more space available\n");
      return;
    }
  nsp_hash_destroy(macros_cache);
  macros_cache = new_cache;
}



/*
 * Hashtable code : 
 * slightly modified to add REMOVE (Jean-Philippe Chancelier)
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


/* Backward compatibility to hsearch interface. */

typedef struct entry {
  char key[NAME_MAXL];
  Mdata data;
} ENTRY;

/* Copyright (C) 1993 Free Software Foundation, Inc.
 * This file is part of the GNU C Library.
 * Contributed by Ulrich Drepper <drepper@ira.uka.de>
 *
 * The GNU C Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *  The GNU C Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *  You should have received a copy of the GNU Library General Public
 * License along with the GNU C Library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.  
 */
 
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
static unsigned   hsize = 0;
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

static int myhcreate(unsigned int nel)
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
#if 0  
  if ((htable = (_ENTRY *)calloc(hsize+1, sizeof(_ENTRY))) == NULL)
    return 0;
#else 
  if ((htable = (_ENTRY *) malloc((hsize+1)*sizeof(_ENTRY))) == NULL)
    return 0;
  memset(htable,0,(hsize+1)*sizeof(_ENTRY));
#endif 
  /* everything went alright */
  return 1;
}


/*
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
 */

#define HSEARCH_DEBUG(x) 
#define Eqid(x,y) strncmp(x,y,NAME_MAXL) 

static int myhsearch(char *key, Mdata *data, ACTION action)
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
  if (action == ENTER && filled == hsize-1 ) 
    {
      Sciprintf("htable is full edit LibsTab.c and increase default size");
      return FAIL;    
    }

  HSEARCH_DEBUG(printf("In hsearch with hsize =%d key = %s filled = %d\n",hsize,key,filled);)

    /* Compute a value for the given string. Perhaps use a better method. */
    /* modifs (bruno) : avoid the call to strlen and put the modulo outside the loop */
    /*                  then compute hval % hsize efficiently */
    hval  = len;
  str = key;
  while (*str != '\0') { hval += *str ; str++; }
  /* compute hval %= hsize efficiently */
  hval %= hsize;
  /* 
   * if ( hval >= hsize){ hval -= hsize; if ( hval >= hsize) hval %= hsize; }
   */

  /* First hash function: simply take the modul but prevent zero. */
  if (hval == 0) hval++;

  /* The first index tried. */
  idx = hval;

  if (htable[idx].used) 
    {

      HSEARCH_DEBUG(printf("idx %d is used \n",idx);)

	/* Further action might be required according to the action value. */
	
	if (htable[idx].used == hval )
	  {
	    if ( htable[idx].entry.data.Int == -1 )
	      {
		HSEARCH_DEBUG(printf("idx %d is not really used \n",idx);)
		  /* not used */
		  switch (action) 
		    {
		    case ENTER :
		      strncpy(htable[idx].entry.key,key,NAME_MAXL);
		      htable[idx].entry.data.Int = data->Int; 
		      filled++;
		      return OK;
		    default : break;
		    }
	      }
	    else if ( Eqid(key, htable[idx].entry.key) == 0) 
	      {
		HSEARCH_DEBUG(printf("idx %d is really used \n",idx);)
		  switch (action) 
		    {
		    case REMOVE :
		      /* because of the second hash we cannot remove */
		      /* htable[idx].used = 0;*/
		      htable[idx].entry.data.Int = -1;
		      filled--;
		      return OK ;
		      break;
		    case ENTER :
		      htable[idx].entry.data.Int = data->Int; 
		      return OK;
		    case FIND :
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

	HSEARCH_DEBUG(printf("chaining to idx %d \n",idx);)
	  /* If entry is found use it. */
	  if (htable[idx].used == hval ) 
	    {

	      HSEARCH_DEBUG(printf("idx %d is  used \n",idx);)
		if ( htable[idx].entry.data.Int == -1 )
		  {
		    HSEARCH_DEBUG(printf("idx %d is not really used \n",idx);)
		      /* not used */
		      switch (action) 
			{
			case ENTER :
			  strncpy(htable[idx].entry.key,key,NAME_MAXL);
			  htable[idx].entry.data.Int = data->Int; 
			  filled++;
			  return OK;
			default : break;
			}
		  }
		else if ( Eqid(key, htable[idx].entry.key) == 0) 
		  {
		    HSEARCH_DEBUG(printf("idx %d is really used \n",idx);)
		      switch (action) 
			{
			case REMOVE :
			  /* because of the second hash we cannot remove */
			  /* htable[idx].used = 0;*/
			  htable[idx].entry.data.Int = -1;
			  filled--;
			  return OK;
			  break;
			case ENTER :
			  /* replace */
			  htable[idx].entry.data.Int = data->Int; 
			  return OK;
			case FIND :
			  data->Int = htable[idx].entry.data.Int;
			  return OK;
			}
		  }
	    }
      } while (htable[idx].used);
    }
    
  /* An empty bucket has been found. */

  HSEARCH_DEBUG(printf("found a free idx %d \n",idx);)
    if (action == ENTER) 
      {
	htable[idx].used  = hval;
	strncpy(htable[idx].entry.key,key,NAME_MAXL);
	htable[idx].entry.data.Int = data->Int; 
	filled++;
	return OK ;
      }
    else
      return FAIL;
}



/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * 
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
 * FMAXTAB must be set to 2*(the number of primitives)
 */

/*
 * FMAXTAB : maximum number of entries in the htable 
 * in fact  myhcreate use a prime > FMAXTAB
 * WARNING : FMAXTAB must be chosen > 2* the number of 
 * expected entries for good efficiency of the hash code 
 */

#define FMAXTAB 4096

typedef struct fdata {
  int Int;
  int Num;
} Fdata;

static int   nsp_hash_func_hcreate (unsigned int);
static int   nsp_hash_func_hsearch (const char *str,Fdata *d,ACTION);

/**
 * nsp_enter_function:
 * @str: name to be searched 
 * @Int: interface number 
 * @Num: function number in interface 
 * 
 * Enter function in Hash Table or change data if function 
 * was already in the table
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_enter_function(const char *str, int Int, int Num)
{
  Fdata data;
  data.Int = Int ;
  data.Num = Num ;
  return( nsp_hash_func_hsearch(str,&data,ENTER));
}

/**
 * nsp_delete_function:
 * @str: 
 * 
 * remove function name @str from function table.
 **/

void nsp_delete_function(const char *str)
{
  Fdata data;
  nsp_hash_func_hsearch(str,&data,REMOVE);
}


/**
 * nsp_find_function:
 * @str: 
 * @Int: 
 * @Num: 
 * 
 * searches function  named @str in function table. 
 * In case of success %OK is returned and the function id is returned 
 * in the pair @Int, @Num.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_find_function(const char *str, int *Int, int *Num)
{
  int r;
  Fdata data;
  r= nsp_hash_func_hsearch(str,&data,FIND);
  *Int = data.Int;
  *Num = data.Num;
  return r;
}

/**
 * nsp_init_function_table:
 * @void: 
 * 
 * Initialize the function table.
 **/

void nsp_init_function_table(void)
{
  static int firstentry = 0;
  int i=0,k=0;
  if ( firstentry == 1 ) return ;
  /** first call **/
  if ( nsp_hash_func_hcreate(FMAXTAB) == 0 ) 
    {
      printf("Fatal Error: Can't create table for scilab functions (not enough memory)\n");
      exit(1);
    }
  while (1) 
    {
      /* interfaces */
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
	  if ( nsp_enter_function(fname,i,k) == FAIL)
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

/*
 * Hashtable code : 
 * slightly modified to add DELETE 
 * Jean-Philippe Chancelier Cermics/enpc
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


/* Backward compatibility to hsearch interface. */

typedef struct fentry {
  char key[NAME_MAXL];
  Fdata data;
} FENTRY;

/* Copyright (C) 1993 Free Software Foundation, Inc.
 *   This file is part of the GNU C Library.
 *   Contributed by Ulrich Drepper <drepper@ira.uka.de>
 *
 * The GNU C Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The GNU C Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the GNU C Library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.  
 */

/*
 * [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
 * [Knuth]            The Art of Computer Programming, part 3 (6.4)
 */

/*
 * We need a local static variable which contains the pointer to the
 * allocated memory for the hash table. An entry in this table contains
 * an FENTRY and a flag for usage.
 */

typedef struct { 
    unsigned int   used;
    FENTRY entry;
} _FENTRY;

static _FENTRY   *f_htable = NULL;
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
isprime1(unsigned int number)
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

static int nsp_hash_func_hcreate(unsigned int nel)
{
    /* There is still another table active. Return with error. */
    if (f_htable != NULL)
	return 0;

    /* Change nel to the first prime number not smaller as nel. */
    nel |= 1;      /* make odd */
    while (!isprime1(nel)) nel += 2;

    hsize  = nel;
    filled = 0;
    /* printf(" Size of hTable %d\n",nel); */
    /* allocate memory and zero out */
    /* note that f_htable has hsize +1 entries */
#if 0  
    if ((f_htable = (_FENTRY *)calloc(hsize+1, sizeof(_FENTRY))) == NULL)
      return 0;
#else 
    if ((f_htable = (_FENTRY *) malloc((hsize+1)*sizeof(_FENTRY))) == NULL)
      return 0;
    memset(f_htable,0,(hsize+1)*sizeof(_FENTRY));
#endif 
    /* everything went alright */
    return 1;
}

/*
 * After using the hash table it has to be destroyed. The used memory can
 * be freed and the local static variable can be marked as not used.
 */
/* Unused : cleaned at scilab exit 

static void
nsp_hash_func_hdestroy()
{
    / * free used memory * /
    free(f_htable);
    / * the sign for an existing table is a value != NULL in f_htable * / 
    f_htable = NULL;
}
**/

/** from data to key : find function given (Int and Num ) **/

int FindFunctionB(char *key, int Int, int Num)
{
  unsigned int i;
  for ( i = 0 ; i <= hsize ; i++ ) 
    if ( f_htable[i].used 
	 &&  f_htable[i].entry.data.Int == Int
	 &&  f_htable[i].entry.data.Num == Num
	 )
      {
	strncpy(key,f_htable[i].entry.key,NAME_MAXL);
	return(1);
      }
  return(0);
}

/**
 * ShowTable:
 * @void: 
 * 
 * print the function table.
 * 
 **/

void nsp_print_function_table(void)
{
  unsigned int i;
  Sciprintf("Whole Table\n");
  for ( i = 0 ; i <= hsize ; i++ ) 
    if ( f_htable[i].used )
      Sciprintf("%s %d %d \n",f_htable[i].entry.key,
	     f_htable[i].entry.data.Int,f_htable[i].entry.data.Num);
}

/**
 * nsp_delete_interface_functions:
 * @Int: interface number.
 * 
 * deletes entries associated to interface number @Int by walking through 
 * the whole hash table. 
 * 
 **/

void nsp_delete_interface_functions(int Int)
{
  unsigned int i;
  for ( i = 0 ; i <= hsize ; i++ ) 
    if ( f_htable[i].used 
	 &&  f_htable[i].entry.data.Int == Int+ DYN_INTERF_START )
      {
	f_htable[i].used = 0;
	filled--;
      }
}

/*
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
 */

#define  Eqid(x,y) strncmp(x,y,NAME_MAXL) 

static int nsp_hash_func_hsearch(const char *key, Fdata *data, ACTION action)
{
  register unsigned hval;
  register unsigned hval2;
  register unsigned len = NAME_MAXL;
  register unsigned idx;
  register const char *str;

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

    if (f_htable[idx].used) 
      {
	/* Further action might be required according to the action value. */
	
	if (f_htable[idx].used == hval )
	  {
	    if ( Eqid(key, f_htable[idx].entry.key) == 0) 
	      {
		switch (action) 
		  {
		  case REMOVE :
		    f_htable[idx].used = 0;
		    filled--;
		    return OK ;
		    break;
		  case ENTER :
		    f_htable[idx].entry.data.Num = data->Num; 
		    f_htable[idx].entry.data.Int = data->Int; 
		    return OK;
		  case FIND :
		    data->Num = f_htable[idx].entry.data.Num;
		    data->Int = f_htable[idx].entry.data.Int;
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
            if (f_htable[idx].used == hval ) 
	      {
                if ( Eqid(key, f_htable[idx].entry.key) == 0) 
		  {
		    switch (action) 
		      {
		      case REMOVE :
			f_htable[idx].used = 0;
			filled--;
			return OK;
			break;
		      case ENTER :
			f_htable[idx].entry.data.Num = data->Num; 
			f_htable[idx].entry.data.Int = data->Int; 
			return OK;
		      case FIND :
			data->Num = f_htable[idx].entry.data.Num;
			data->Int = f_htable[idx].entry.data.Int;
			return OK;
		      }
		  }
	      }
	  } while (f_htable[idx].used);
      }
    
    /* An empty bucket has been found. */

    if (action == ENTER) 
      {
        f_htable[idx].used  = hval;
	strncpy(f_htable[idx].entry.key,key,NAME_MAXL);
	f_htable[idx].entry.data.Num = data->Num; 
	f_htable[idx].entry.data.Int = data->Int; 
	filled++;
        return OK ;
      }
    else
      return FAIL;
}


#ifdef TEST 
/* tests */

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
	  nsp_enter_function(SciFuncs[j].name,SciFuncs[j].Int,k);k++;
	  j++;
	}
      test1();
      printf("entering functions with new data\n");
      printf("===================== \n");
      j=0;k=0;
      while ( SciFuncs[j].name != (char *) 0 && j < 10 )
	{
	  nsp_enter_function(SciFuncs[j].name,-23,k);k++;
	  j++;
	}
      test1();
      printf("Remove interface\n");
      printf("===================== \n");
      DelseteFunctionS(-23);
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

/* end of test part */
#endif 
