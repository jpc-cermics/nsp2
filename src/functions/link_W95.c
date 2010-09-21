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
 * Link version for Win32 
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <windows.h>

/**
 * nsp_link_status:
 * @void: 
 * 
 * 
 * 
 * Returns: 
 **/

int nsp_link_status(void)
{
  return(1);
}

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


static void *nsp_dlopen(nsp_const_string shared_path,int global)
{
  HINSTANCE  hd1 = NULL;
  /* expand filename in buf1 */

  if ( strncmp(shared_path,"nsp",3) ==0 
       || strncmp(shared_path,"scilab",6) ==0  /* backward comp */
       )
    {
      char buf1[FSIZE+1];
      nsp_path_expand("SCI/bin/libnsp.dll",buf1,FSIZE);
      hd1 =   LoadLibrary (buf1);
    }
  else
    {
      hd1 =   LoadLibrary (shared_path);
    }
  if ( hd1 == NULL ) 
    {
      Scierror("Error: link failed for dll %s\n",shared_path);
      return NULL;
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
  func = GetProcAddress (sh->obj->shd , enamebuf);
  if ( func == NULL )
    {
      Sciprintf("%s is not an entry point \n",enamebuf);
      return FAIL;
    }
  /* insert in the table */
  if (nsp_epoints_table_insert(ename,func,ishared) == FAIL )
    return FAIL;
  return OK;
  
}


static void nsp_dlclose(void *shd) 
{
  FreeLibrary  ((HINSTANCE) shd);
}
