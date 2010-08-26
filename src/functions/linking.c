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
 */

/*
 * A set of routines for 					       
 * dynamic linking facilities.					       
 */

#include <string.h> 
#include <stdio.h>

#include "nsp/math.h"
#include "nsp/plisttoken.h" /* for  name_maxl 52 */
#include "nsp/sciio.h"
#include "nsp/string.h"
#include <nsp/object.h>
#include <nsp/hash.h>
#include <nsp/nsptcl.h>

#include "linking.h"
#include "../system/files.h" /* FSIZE */

static void Underscores(int isfor,nsp_const_string ename, char *ename1);
static int SearchFandS(nsp_const_string op, int ilib);
static int nsp_find_shared(nsp_const_string shared_path);
static int nsp_link_status (void) ;

/* sructure used to store entry points 
 * i.e a function pointer, a name and a shared lib id 
 */

#define ENTRYMAX 512         /* maximum number of loaded shared libary 
			      * and maximum number of entries in a shared library
			      */
typedef int (*l_function) ();

typedef char Name[NAME_MAXL];   /* could be changed to dynamic structure */

typedef struct { 
  l_function epoint;            /* the entry point */ 
  Name     name;              /* entry point name */
  int      Nshared;           /* number of the shared file */
} Epoints;

/* structure used to store information 
 * about shared libraries 
 */

typedef struct {
  int ok;                 /* if ok == FAIL then inactive */
  char tmp_file[FSIZE+1]; /* name of the shared library */
  unsigned long  shl;     /* dlopen stuffs */
} Hd;

static Hd  hd[ENTRYMAX];      /* shared libs handler */
static int Nshared  = 0   ;
static Epoints EP[ENTRYMAX];  /* entryPoints */
static int NEpoints = 0   ;   /* Number of Linked names */

/*
 * Dynamically Link entry points given in en_names 
 * from shared library given by its path in 
 * @shared_path. 
 * strf : 'f' or 'c' ( to decide trailing _ action )
 * ilib : integer (in/out) value 
 * iflag: 0 if files is used 1 ilib is used
 * rhs: number of rhs arguments in link(...)
 * Warning: en_names should be a  null terminated string arrays
 */

void nsp_dynamic_load(nsp_const_string shared_path,char **en_names,char strf, int *ilib, int iflag, int *rhs)
{
  int lib;
  nsp_link_initialize(); 
  if ( iflag== 0 && (lib = nsp_find_shared(shared_path)) != -1 ) 
    {
      /* Sciprintf("shared library already loaded\n"); */
      if  ( strcmp(shared_path,"nsp") != 0 || strcmp(shared_path,"scilab") != 0 )
	nsp_unlink_shared(lib);
    }

  if ( iflag== 0 && strncmp(shared_path,"show",4)==0) 
    {
      ShowDynLinks();
      *ilib = nsp_link_status();  /* return value for Scilab */
      return;
    }

  /* calling the linker */
  nsp_link_library(iflag,rhs,ilib,shared_path,en_names,strf);
}

#if defined(netbsd) || defined(freebsd) || defined(sun) || defined(__alpha) || defined(sgi) || (!defined(hppa_old) && defined(hppa))  || defined(__APPLE__) || defined(__CYGWIN__) 
#include "link_SYSV.c"
#else
/** no more used on sun */
#if defined(sun_old) ||  (defined(mips) && !defined(netbsd)) || defined(_IBMR2) || defined(hppa_old)
#ifdef SUNOSDLD 
#include "link_linux.c"
#else 
#include "link_std.c"
#endif /* end of SUNOSDLD */
#else
#if defined(linux)
#ifdef __ELF__
#include "link_SYSV.c"
#else
#include "link_linux.c"
#endif /* end of __ELF__ */
#else
#if defined(WIN32)
#include "link_W95.c"
#else
#include "link_empty.c"
#endif
#endif
#endif 
#endif 

#ifdef WLU
#if (!defined(DLDLINK) && !defined(WIN32) && !defined(__APPLE_CC__))
#define WLU1 /* dld will add the leading _ itself, win32 too*/
#endif 
#endif

/*
 * utility function : 
 * add trailing and leading _ when needed 
 * this information is gathered by configure.
 */

static void Underscores(int isfor,nsp_const_string ename, char *ename1)
{
#ifdef WLU1
  *ename1='_'; ename1++;
#endif
  strcpy(ename1,ename);
#ifdef WTU
  if (isfor==1) strcat(ename1,"_");
#endif
  return;
}

/*
 * Initialize tables 
 */

void nsp_link_initialize(void)
{
  static int first_entry = 0;
  int i;
  if ( first_entry == 0)
    {
      for ( i = 0 ; i < ENTRYMAX ; i++) 
	{
	  hd[i].ok= FAIL;
	  EP[i].Nshared = -1;
	}
      first_entry++;
    }
}

/* 
 * checks if @name is in the dynamically 
 * linked entry points. 
 * if @ilib == -1 the search is performed in 
 * the whole table else the search is restricted to 
 * shared library number @ilib.
 * the returned value is -1 or the indice of 
 * @name in the entry point table. if 
 *
 * @ilib == -1 then on output when the symbol is found 
 * the library number where the symbol was found is returned. 
 */

int nsp_is_linked(nsp_const_string name,int *ilib)
{
  int (*loc)();
  if ( *ilib  != -1 ) 
    return SearchFandS(name,*ilib);
  else
    {
      int rep= SearchInDynLinks(name,&loc);
      if (rep != -1 ) 
	*ilib = EP[rep].Nshared;
      return rep;
    }
}


/*
 * Search an entry point named @op in the dynamically 
 * linked entry points. Search is performed from end to top 
 * returns -1 in case of failure or the entry point 
 * indice in the entry points table. In case of success 
 * the associated function is returned in @realop.
 */

int SearchInDynLinks(nsp_const_string op, int (**realop) ())
{
  int i=0;
  for ( i = NEpoints-1 ; i >=0 ; i--) 
    {
      if ( strcmp(op,EP[i].name) == 0) 
	{
           *realop = EP[i].epoint;
	   return(i);
	 }
    }
  return(-1);
}

/*
 * Search an entry point named @op in the shared 
 * library ilib. Search is performed from end to top 
 * returns -1 in case of failure or the entry point 
 * indice in the entry points table 
 */

static int SearchFandS(nsp_const_string op, int ilib)
{
  int i=0;
  for ( i = NEpoints-1 ; i >=0 ; i--) 
    {
      if ( strcmp(op,EP[i].name) == 0 && EP[i].Nshared == ilib)
	{
	  return(i);
	}
    }
  return(-1);
}

/*
 * Show the linked files 
 */

void  ShowDynLinks(void)
{
  int i=0,count=0;
  Sciprintf("Number of entry points %d\n",NEpoints);
  Sciprintf("Shared libs : [");
  for ( i = 0 ; i < Nshared ; i++) 
    if ( hd[i].ok == OK) { Sciprintf("%d ",i);count++;}
  Sciprintf("] : %d libs (%d)\n",count,Nshared);
  for ( i = NEpoints-1 ; i >=0 ; i--) 
    {
      int ish = Min(Max(0,EP[i].Nshared),ENTRYMAX-1);
      if ( hd[ish].ok == OK) 
	Sciprintf("Entry point %s in shared lib %d (%s)\n",
		  EP[i].name,EP[i].Nshared, hd[ish].tmp_file);
      else 
	Sciprintf("Entry point %s in shared lib %d (void)\n",
		  EP[i].name,EP[i].Nshared);
    }
}

/* get all entries as a hash table 
 * names : shared library number
 */

NspHash *nsp_get_dlsymbols(void)
{
  int i;
  NspHash *H;
  if(( H = nsp_hash_create(NVOID,ENTRYMAX)) == NULLHASH) return NULLHASH;
  for ( i = NEpoints-1 ; i >=0 ; i--) 
    {
      NspObject *obj;
      if ( EP[i].Nshared != -1)
	{
	  if ((obj=nsp_create_object_from_double(EP[i].name,EP[i].Nshared))==NULLOBJ) goto clean;
	  if (nsp_hash_enter(H,obj) == FAIL) goto clean;
	}
    }
  return H;
 clean:
  nsp_hash_destroy(H);
  return NULLHASH;
} 


/* 
 * search linked libraries for a given name 
 *
 */

static int nsp_find_shared(nsp_const_string shared_path)
{
  int i;
  for ( i = 0 ; i < ENTRYMAX ; i++) 
    {
      if ( hd[i].ok != FAIL && strcmp(hd[i].tmp_file,shared_path)==0) 
	return i;
    }
  return -1;
}


/* remove the entry points associated to 
 * a shared library and the associated interfaced 
 * functions if it was an addinter 
 */

void nsp_unlink_shared(int ilib)
{
  /* be sure that dynamic link tables are initialized */
  nsp_link_initialize(); 
  /* delete entry points in shared lib *i */
  nsp_delete_symbols(ilib);
  /* delete entry points used in addinter in shared lib *i */
  nsp_delete_interface_functions(ilib);
}
