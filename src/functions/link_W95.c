/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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

#define Min(x,y)	(((x)<(y))?(x):(y))
#define Max(x,y)	(((x)>(y))?(x):(y))

extern char *strchr();

static void nsp_delete_symbols (int );
static int nsp_dlopen(nsp_const_string shared_path,int global);
static int nsp_dlsym(nsp_const_string ename, int ishared, char strf);

/*************************************
 * New version : link entry names 
 *   from new shared lib created with 
 *   files.
 *   -1 : the shared archive was not loaded 
 *   -5 : pb with one of the entry point 
 *************************************/

void nsp_link_library(int iflag, int *rhs,int *ilib,nsp_const_string shared_path, char **en_names, char strf)
{
  int i;
  if ( iflag == 0 )
    {
      *ilib  = nsp_dlopen(shared_path,( *rhs == 1 ) ? TRUE : FALSE);
    }
  if (*ilib  == -1 ) return;

  if ( *rhs >= 2) 
    {
      i=0 ;
      while ( en_names[i] != (char *) 0)
	{
	  if ( nsp_dlsym(en_names[i],*ilib,strf) == FAIL) 
	    *ilib=-5;
	  i++;
	}
    }
}


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


static int nsp_dlopen(nsp_const_string shared_path,int global)
{
  static HINSTANCE  hd1 = NULL;
  int   i;
  hd1 =   LoadLibrary (shared_path);
  if ( hd1 == NULL ) 
    {
      Scierror("Error: link failed for dll %s\n",shared_path);
      return(-1);
    }

  /* store the shared library in table 
   * first try to detect an unoccupied zone
   */
  for ( i = 0 ; i < Nshared ; i++ ) 
    {
      if ( hd[i].ok == FAIL) 
	{
	  hd[i].shl =  (unsigned long)hd1; 
	  strcpy(hd[i].tmp_file,shared_path); 
	  hd[i].ok = OK; 
	  /* Ok we stop */
	  return(i); 
	} 
    }
  /* we use the last position */
  if ( Nshared == ENTRYMAX ) 
    {
      Scierror("Error: cannot open shared library maxentry %d is reached\n",ENTRYMAX);
      return -1;
    }
  strcpy(hd[Nshared].tmp_file,shared_path);
  hd[Nshared].shl = (unsigned long)hd1;
  hd[Nshared].ok = OK;
  Nshared ++;
  return (Nshared-1);
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
  HINSTANCE  hd1 = NULL;
  int ish = Min(Max(0,ishared),ENTRYMAX-1);
  char enamebuf[NAME_MAXL];
  if ( strf == 'f' )
    Underscores(1,ename,enamebuf);
  else 
    Underscores(0,ename,enamebuf);

  /* lookup the address of the function to be called */
  if ( NEpoints == ENTRYMAX ) 
    {
      Sciprintf("You can't link more functions maxentry %d reached\n",ENTRYMAX);
      return(FAIL);
    }
  if ( hd[ish].ok == FAIL ) 
    {
      Sciprintf("Shared lib %d does not exists\n",ish);
      return(FAIL);
    }
  /** entry was previously loaded **/
  if ( SearchFandS(ename,ish) >= 0 ) 
    {
      Sciprintf("Entry name %s is already loaded from lib %d\n",ename,ish);
      return(OK);
    }
  hd1 = (HINSTANCE)  hd[ish].shl;
  EP[NEpoints].epoint = (l_function) GetProcAddress (hd1,enamebuf);
  if ( EP[NEpoints].epoint == NULL )
    {
      Sciprintf("%s is not an entry point \n",enamebuf);
      return(FAIL);
    }
  else 
    {
      /* we don't add the _ in the table */
      Sciprintf("Linking %s (in fact %s)\n",ename,enamebuf);
      strncpy(EP[NEpoints].name,ename,NAME_MAXL);
      EP[NEpoints].Nshared = ish;
      NEpoints++;
    }
  return(OK);  
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
  int ish = Min(Max(0,ishared),ENTRYMAX-1);
  int i=0;
  for ( i = NEpoints-1 ; i >=0 ; i--) 
    {
      if ( EP[i].Nshared == ish )
	{
	  int j;
	  for ( j = i ; j <= NEpoints - 2 ; j++ )
	    {
	      EP[j].epoint = EP[j+1].epoint;
	      EP[j].Nshared = EP[j+1].Nshared;
	      strcpy(EP[j].name,EP[j+1].name);
	    }
	  NEpoints--;
	}
    }

  if ( hd[ish].ok != FAIL)
    {
      FreeLibrary ((HINSTANCE) hd[ish].shl);
      hd[ish].ok = FAIL;
    }
}
