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
 * A set of routines for dynamic linking facilities.					       
 */

#include <nsp/nsp.h> 
#include <nsp/sciio.h>
#include <nsp/hash.h>
#include <nsp/epoints.h>
#include <nsp/sharedlib.h>
#include <nsp/nsptcl.h>
#include <nsp/system.h> 
#include <nsp/linking.h>

static void nsp_check_underscores(int isfor,nsp_const_string ename, char *ename1);
static int nsp_find_shared(nsp_const_string shared_path);
static int nsp_link_status (void) ;
static void nsp_delete_symbols (int );
static void *nsp_dlopen(nsp_const_string shared_path,int global);
static int nsp_dlsym(NspSharedlib *sh, nsp_const_string ename, char strf);
static void nsp_dlclose(void *shd) ;

#if defined(WIN32) 
#include "link_W95.c"
#else 
#include "link_SYSV.c"
#endif

/**
 * nsp_dynamic_load:
 * @shared_path: 
 * @en_names: 
 * @strf: 
 * @ilib: 
 * @iflag: 
 * @rhs: 
 * 
 * Dynamically Link entry points given in en_names 
 * from shared library given by its path in 
 * @shared_path. 
 * strf : 'f' or 'c' ( to decide trailing _ action )
 * ilib : integer (in/out) value 
 * iflag: 0 if files is used 1 ilib is used
 * rhs: number of rhs arguments in link(...)
 * Warning: en_names should be a  null terminated string arrays
 * 
 **/

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


/**
 * nsp_dlsym:
 * @ename: a string giving a symbol name 
 * @ishared: the id of a previously loaded shared library 
 * @strf: 'c' or 'f' 
 * 
 * Using the id @ishared of a dynamic library returned  by  nsp_dlopen and a symbol 
 * name, this function gets the address where that symbol is loaded into memory 
 * and store the symbol in the link table.
 * 
 * Returns: %OK or %FAIL 
 **/

static int nsp_dlsym(NspSharedlib *sh, nsp_const_string ename, char strf)
{
  void *func;
  char enamebuf[NAME_MAXL];
  nsp_check_underscores(( strf == 'f' ) ? 1: 0,ename,enamebuf);
  /* get address of entry point */
  if ((func = nsp_dlsym_os(sh , enamebuf)) == NULL ) return FAIL;
  /* insert in the table */
  if ( nsp_epoints_table_insert(ename,enamebuf,func,sh->obj->id) == FAIL )
    return FAIL;
  return OK;
}


NspSharedlib *nsp_sharedlib_dlopen(nsp_const_string shared_path, int global) 
{
  static int k=1;
  NspSharedlib *sh;
  void *hd = nsp_dlopen(shared_path, global ) ;
  if ( hd == NULL) return NULL ;
  sh= nsp_sharedlib_table_insert(hd,k,shared_path);
  if ( sh == NULL) 
    {
      nsp_dlclose(hd);
      return NULL;
    }
  k++;
  return sh;
}

/**
 * nsp_link_library:
 * @iflag: 
 * @rhs: 
 * @ilib: 
 * @shared_path: 
 * @en_names: 
 * @strf: 
 * 
 * return in ilib the number of the shared archive or -1 or -5
 *   -1 : the shared archive was not loaded 
 *   -5 : pb with one of the entry point 
 * 
 **/

void nsp_link_library(int iflag, int *rhs,int *ilib,nsp_const_string shared_path, 
		      char **en_names, char strf)
{
  NspSharedlib *sh = NULL;
  int i;
  if ( iflag == 0 )
    {
      int global = ( *rhs == 1 ) ? TRUE : FALSE;
      sh = nsp_sharedlib_dlopen(shared_path, global);
      if ( sh == NULL ) return;
      *ilib = sh->obj->id; 
    }
  else 
    {
      /* get the shared lib object */
      if ((sh =nsp_sharedlib_table_find(*ilib)) == NULL) 
	{
	  Scierror("Error: Shared library %d does not exists\n",*ilib);
	  return;
	}
    }
  /* now load the entry points from sh */
  if ( *rhs >= 2) 
    {
      i=0 ;
      while ( en_names[i] != (char *) 0)
	{
	  if ( nsp_dlsym(sh,en_names[i],strf) == FAIL) 
	    {
	      *ilib=-5; return;
	    }
	  i++;
	}
    }
}

/*
 * utility function : 
 * add trailing and leading _ when needed 
 * this information is gathered by configure.
 */

static void nsp_check_underscores(int isfor,nsp_const_string ename, char *ename1)
{
#ifdef WLU
#if (!defined(DLDLINK) && !defined(WIN32) && !defined(__APPLE_CC__))
#define WLU1 /* dld will add the leading _ itself, win32 too*/
#endif 
#endif
#ifdef WLU1
  *ename1='_'; ename1++;
#endif
  strcpy(ename1,ename);
#ifdef WTU
  if (isfor==1) strcat(ename1,"_");
#endif
  return;
}

/**
 * nsp_link_initialize:
 * @void: 
 * 
 * 
 **/

void nsp_link_initialize(void)
{
  
}


/**
 * nsp_is_linked:
 * @name: a string 
 * @ilib: an integer 
 * 
 * checks if @name is in the dynamically linked entry points table.
 * if @ilib == -1 the search is performed in 
 * the whole table else the search is restricted to 
 * shared library number @ilib.
 * the returned value is -1 or the indice of 
 * @name in the entry point table. if 
 *
 * @ilib == -1 then on output when the symbol is found 
 * the library number where the symbol was found is returned. 
 * 
 * Returns: an integer 
 **/

int nsp_is_linked(nsp_const_string name,int *ilib)
{
  int (*loc)();
  return  nsp_link_search(name,*ilib,&loc);
}

/**
 * nsp_link_search:
 * @op: name of entry point 
 * @ilib: index of shared library 
 * 
 * Search an entry point named @op. If @ilib is equal 
 * to -1 (or strictly negative) the search is performed in all 
 * the shared library table. If @ilib is greater or equal than zero 
 * the search is restricted to library @ilib. 
 * 
 * returns -1 in case of failure and the @ilib in which the 
 * entry was found  in case of success. The associated function is 
 * returned in @realop.
 * 
 * Returns: %OK or %FAIL.
 **/

int  nsp_link_search(nsp_const_string op, int ilib, int (**realop) ())
{
  NspEpoints *ep =nsp_epoints_table_find(op);
  *realop = NULL;
  if ( ep  == NULL ) return -1 ;
  if ( ilib == -1 || ilib == ep->obj->shid )
    {
      *realop = ep->obj->func;
      ilib = ep->obj->shid;
      return ilib;
    }
  return -1 ;
}

/*
 * Show the linked files 
 */

void  ShowDynLinks(void)
{
  nsp_epoints_table_show();
  /* 
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
  */
}

/* get all entries as a hash table 
 * names : shared library number
 */

NspHash *nsp_get_dlsymbols(void)
{
  NspHash *H;
  if(( H = nsp_hash_create(NVOID,10)) == NULLHASH) return NULLHASH;
  return H;
} 

/* 
 * search linked libraries for a given name 
 *
 */

static int nsp_find_shared(nsp_const_string shared_path)
{
  NspSharedlib *sh = nsp_sharedlib_table_find_by_path(shared_path);
  if ( sh == NULL) return -1;
  return sh->obj->id ;
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
  nsp_remove_interface(ilib);
}

/**
 * nsp_delete_symbols:
 * @ishared: integer 
 * 
 * remove from link table the entries which were 
 * linked from shared library @ishared.
 *
 **/

static void nsp_delete_symbols(int ishared)
{
  NspSharedlib *sh; 
  nsp_epoints_table_remove_entries(ishared);
  sh = nsp_sharedlib_table_find(ishared);
  if ( sh != NULL)  nsp_dlclose( sh->obj->shd);
  nsp_sharedlib_table_remove_lib(ishared);
}
