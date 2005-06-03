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
#include "linking.h"
#include "../system/files.h" /* FSIZE */

static void Underscores(int isfor,nsp_const_string ename, char *ename1);
static int SearchFandS(nsp_const_string op, int ilib);
int LinkStatus (void) ;

/*********************************************
 * Structure to keep the entry points 
 *********************************************/

#define ENTRYMAX 200         /* maximum number of loaded shared libary */

typedef int (*function) ();

typedef char Name[NAME_MAXL];   /* could be changed to dynamic structure */

typedef struct { 
  function epoint;            /* the entry point */ 
  Name     name;              /* entry point name */
  int      Nshared;           /* number of the shared file */
} Epoints;

typedef struct {
  int ok;
  char tmp_file[FSIZE+1];
  unsigned long  shl;
} Hd;

static Hd  hd[ENTRYMAX]; /* shared libs handler */
static int Nshared  = 0   ;
static Epoints EP[ENTRYMAX];  /* entryPoints */
static int NEpoints = 0   ;        /* Number of Linked names */

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

void SciDynLoad(nsp_const_string shared_path,char **en_names,char strf, int *ilib, int iflag, int *rhs)
{
  SciLinkInit(); 
  if ( iflag== 0 && strncmp(shared_path,"show",4)==0) 
    {
      ShowDynLinks();
      *ilib = LinkStatus();  /* return value for Scilab */
      return;
    }

  /* calling the linker */
  SciLink(iflag,rhs,ilib,shared_path,en_names,strf);
  if (*ilib >= 0) Sciprintf("Link done\n");
}

#if defined(netbsd) || defined(freebsd) || defined(sun) || defined(__alpha) || defined(sgi) || (!defined(hppa_old) && defined(hppa))  || defined(__APPLE__)
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
#ifndef DLDLINK
#ifndef WIN32
#define WLU1 /* dld will add the leading _ itself, win32 too*/
#endif 
#endif
#endif 

/********************************************
 * Underscores : deals with the trailing _ 
 * in entry names 
 ********************************************/

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

/**************************************
 * Initialize tables 
 *************************************/

void SciLinkInit(void)
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

/**************************************
 * if *irep == -1 
 *    checks if buf is a loaded
 *    entry point 
 *    the result is -1 if false 
 *               or the number in the function table 
 * 
 *    
 * if *irep != -1 : 
 *    checks if buf is a loaded
 *    entry point from shared lib *irep
 *    the result is -1 if false 
 *               or the number in the function table 
 * 
 *************************************/

void C2F(iislink)(buf,irep)
     char *buf;
     integer *irep;
{
  int (*loc)();
  if ( *irep != -1 ) 
    *irep=SearchFandS(buf,*irep);
  else
    *irep=SearchInDynLinks(buf,&loc);
}


/**************************************
 * returns the ii functions 
 *************************************/

void GetDynFunc(int ii, int (**realop)())
{
  if ( EP[ii].Nshared != -1 ) 
    *realop = EP[ii].epoint;
  else
    *realop = (function) 0;
}

/**************************************
 * Search a function in the table 
 * Search from end to top 
 *************************************/

int SearchInDynLinks(char *op, int (**realop) ())
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

/**************************************
 * Search a (function,libid) in the table 
 * Search from end to top 
 *************************************/

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

/**************************************
 * Show the linked files 
 *************************************/

void  ShowDynLinks(void)
{
  int i=0,count=0;
  Sciprintf("Number of entry points %d\r\n",NEpoints);
  Sciprintf("Shared libs : [");
  for ( i = 0 ; i < Nshared ; i++) 
    if ( hd[i].ok == OK) { Sciprintf("%d ",i);count++;}
  Sciprintf("] : %d libs\r\n",count);
  for ( i = NEpoints-1 ; i >=0 ; i--) 
    {
      Sciprintf("Entry point %s in shared lib %d\r\n",
	       EP[i].name,EP[i].Nshared);
    }
}






