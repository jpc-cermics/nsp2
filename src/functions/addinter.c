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

/************************************************************
 * addinter related functions : used to dynamically link functions 
 * from an interface in Scilab function table. 
 * An interface is characterized by two functions:
 *   a function F(i,stack,rhs,opt,lhs) which is used for function call
 *   and a function F_info which is used to find entry names 
 *   
 *   void AddInter(files,iname,enames,err)
 *   void RemoveInterf(Nshared)
 *   void ShowInterf()
 ************************************************************/

#include <string.h> 
#include <stdio.h>

#include "nsp/math.h"
#include "nsp/plisttoken.h" /* for  name_maxl 52 */
#include "nsp/sciio.h"
#include "nsp/interf.h"

#include "FunTab.h"
#include "linking.h"
#include "addinter.h"

Iel DynInterf[MAXINTERF];
int LastInterf=0;

static void SciInterInit (void);
static int DynFuntab  (char **e_names, int N);
static void ShowInterf  (void);

/************************************************
 * Dynamically added interface to Scilab 
 * files and enames are null terminated String arrays 
 ************************************************/

void AddInter(char **files, char *iname, char **enames, int *err)
                                     /* interface name */
                           
              
{
  int i,rhs=2,ilib=0,inum;
  char *names[2];

  names[0]=iname;
  names[1]=(char *)0;

  SciLinkInit();
  SciInterInit();

  /** Try to unlink the interface if it was previously linked **/
  
  for ( i = 0 ; i < LastInterf ; i++) 
    {
      if (strcmp(iname,DynInterf[i].name)==0) 
	{
	  /** check if my os accepts unlink **/
	  if ( LinkStatus() == 1) 
	    {
	      C2F(isciulink)(&DynInterf[i].Nshared);
	    }
	  break;
	}
    }

  /** Try to find a free position in the interface table : inum **/
  inum=-1;
  for ( i = 0 ; i < LastInterf ; i++) 
    {
      if ( DynInterf[i].ok == 0 ) inum= i;
    }
  inum = ( inum == -1 ) ? LastInterf : inum ;
  
  /** Linking Files and add entry point name iname */
  
  if ( inum >=  MAXINTERF ) 
    {
      Scierror("Error: Maximum number of dynamic interfaces %d\n",MAXINTERF);
      Scierror("has been reached\r\n");
      *err=1;
      return;
    }

  SciLink(0,&rhs,&ilib,files,names,"f");

  if ( ilib < 0 ) 
    {
      *err=ilib;  return;
    }

  /** store the linked function in the interface function table DynInterf **/
  DynInterf[inum].Nshared = ilib;

  if ( SearchInDynLinks(names[0],&DynInterf[inum].func) < 0 ) 
    {
      Scierror("Error: addinter failed, %s not  found!\n",iname);
      *err=2;
      return;
    }
  else
    {
      strncpy(DynInterf[inum].name,iname,NAME_MAXL);
      DynInterf[inum].ok = 1;
    }
  if ( inum == LastInterf ) LastInterf++;

  /** we add all the Scilab new entry names 
    in the scilab function table funtab **/

  if ( (*err=DynFuntab(enames,DYN_INTERF_START+inum+1)) != 0 ) 
    {
      Scierror("Error: addinter failed while trying to add entries in\n");
      Scierror("\t in function table\n");
      return ;
    }
  ShowInterf();
}


/************************************************
 * Add the set of functions in Scilab Function 
 *   Hash Table with interface number N;
 ************************************************/

static int DynFuntab(char **e_names, int N)
{
  int i=0;
  while ( e_names[i] != NULL) 
    {
      if ( EnterFunction(e_names[i],N,i)== FAIL) return 3;
      i++;
    }
  return 0;
}

/*****************************************
 * A blank interface used as default value 
 *****************************************/

int BlankInterface(int i, char *fname, int first, int rhs, int opt, int lhs)
{
  Sciprintf("Error: Interface for function %s is not linked\r\n",fname);
  return RET_BUG;
}

/*********************************
 * used in C2F(isciulink)(i) 
 *********************************/

static void SciInterInit(void)
{
  static int first_entry=0;
  if ( first_entry == 0) 
    {
      int i;
      for ( i= 0 ; i < MAXINTERF ; i++) 
	{ 
	  DynInterf[i].ok=0;
	  DynInterf[i].func = BlankInterface;
	}
      first_entry++;
    }
}

/*********************************
 * used in C2F(isciulink)(i) 
 * Revoir ici comment on enleve un interface 
 *********************************/

void RemoveInterf(int Nshared)
{
  int i;
  for ( i = 0 ; i < LastInterf ; i++ ) 
    {
      if ( DynInterf[i].Nshared == Nshared ) 
	{
	  DynInterf[i].ok = 0;
	  DynInterf[i].func = BlankInterface;
	  break;
	}
    }
}

/*********************************
 * show the interface table 
 *********************************/

static void ShowInterf(void)
{
  int i;
  for ( i = 0 ; i < LastInterf ; i++ ) 
    {
      if ( DynInterf[i].ok == 1 ) 
	Sciprintf("Interface %d %s\r\n",i,DynInterf[i].name);
    }
}








