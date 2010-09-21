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
 *  Link version for SYSV machine 
 *  using dlopen 
 *
 */

#ifndef hppa
#include <dlfcn.h>
#else
#include <dl.h>
#endif

/**
 * nsp_link_status:
 * @void: 
 * 
 * 
 * Returns: 
 **/

int nsp_link_status(void)
{
  return(1);
}

/*
 * a set of macros for hppa to emulate dl functions 
 * should be obsolete i.e dl family should be used on hp 
 */

#ifdef hppa 
static function dlsym(void *handle, const char *symbol)
{
  l_function f;
  dl handle hd1 = (shl_t) handle;
  irep= shl_findsym(&hd1,symbol,TYPE_PROCEDURE,&f);
  return ( irep == -1 ) ? NULL: f;
}

#define dlopen(x,y) ((x)== NULL) ? PROG_HANDLE : shl_load(x, BIND_IMMEDIATE | BIND_VERBOSE ,0L) 
#define dlclose(x) shl_unload((shl_t)(x))
#define dlhandle  shl_t 
#else /* hppa */
#define dlhandle  void *
#endif /* hppa */


/**
 * nsp_dlopen:
 * @shared_path: a null-terminated string giving a pathname 
 * @global: %TRUE or %FALSE 
 * 
 * loads  the dynamic library file named by @shared_path.
 * If @global is %TRUE then (RTLD_NOW| RTLD_GLOBAL) is passed to 
 * dlopen else RTLD_NOW is used. 
 * Note that @shared_path can be set to "nsp". In that case symbols 
 * from nsp executable can de searched. 
 * 
 * 
 * Returns: -1 in case of failure or the id as an integer of the 
 * loaded  dynamic library 
 **/

static void * nsp_dlopen(nsp_const_string shared_path,int global)
{
  int rep ;
  dlhandle hd1;
  if ( strncmp(shared_path,"nsp",3) ==0 
       || strncmp(shared_path,"scilab",6) ==0  /* backward comp */
       )
    {
      /* try to open symbols from nsp executable 
       * does not work on all architectures 
       */
      hd1 = dlopen(NULL, RTLD_NOW);
    }
  else
    {
      int flag = ( global == TRUE) ? (RTLD_NOW| RTLD_GLOBAL) : RTLD_NOW;
      /* this will load the shared library */
      hd1 = dlopen(shared_path,flag);
    }
  if ( hd1 == NULL ) 
    {
#ifndef hppa
      char *loc = dlerror();
      if ( loc != NULL) Scierror("%s\n",loc);
      return hd1;
#else
      Scierror("link error\n");
      return hd1;
#endif
    }
  return hd1;
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

static int nsp_dlsym(nsp_const_string ename, int ishared, char strf)
{
  NspSharedlib *sh = NULL;
  void *func;
  char enamebuf[NAME_MAXL];
  nsp_check_underscores(( strf == 'f' ) ? 1: 0,ename,enamebuf);
    
  if ((sh =nsp_sharedlib_table_find(ishared)) == NULL)
    {
      Scierror("Error: Shared library %d does not exists\n",ishared);
      return(FAIL);
    }
  
  /* XXX entry was previously loaded 
  if (  nsp_link_search(ename,ish,&loc) >= 0 ) 
    {
      Scierror("Warning: Entry name %s is already loaded from lib %d\n",ename,ish);
      return(OK);
    }
  */

  func = dlsym( sh->obj->shd, enamebuf);
  if ( func ==  NULL) 
    {
#ifndef hppa
      const char *loc = dlerror();
      if ( loc != NULL) Scierror("Error: %s\n",loc);
#else
      Scierror("Error: %s is not an entry point\n",enamebuf);
#endif
      return FAIL;
    }
  /* insert in the table */
  if ( nsp_epoints_table_insert(ename,func,ishared) == FAIL )
    return FAIL;
  return OK;
}


static void nsp_dlclose(void *shd) 
{
  dlclose(shd);
}


  

