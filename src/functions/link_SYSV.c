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
  const char *sh = shared_path;
  int flag = ( global == TRUE) ? (RTLD_NOW| RTLD_GLOBAL) : RTLD_NOW;
  dlhandle hd1;
  if ( strncmp(shared_path,"nsp",3) ==0 ) sh = NULL;
  if ( strncmp(shared_path,"scilab",6) ==0 ) sh = NULL;
  /* this will load the shared library or nsp itself is sh is NULL */
  hd1 = dlopen(sh,flag);
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
 * nsp_dlsym_os:
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

static void *nsp_dlsym_os(NspSharedlib *sh, nsp_const_string ename)
{
  void *func = dlsym( sh->obj->shd, ename);
  if ( func ==  NULL) 
    {
#ifndef hppa
      const char *loc = dlerror();
      if ( loc != NULL) Scierror("Error: %s\n",loc);
#else
      Scierror("Error: %s is not an entry point\n",ename);
#endif
    }
  return func; 
}

static void nsp_dlclose(void *shd) 
{
  dlclose(shd);
}


  

