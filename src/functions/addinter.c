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

/************************************************
 * Dynamically added interface to Scilab 
 * files and enames are null terminated String arrays 
 ************************************************/

int nsp_dynamic_interface(nsp_const_string shared_lib,nsp_const_string interface)
{
  const char interf[]="_Interf";
  const char interf_info[]="_Interf_Info";
  int n=strlen(interface),k;
  int i,rhs=2,ilib=0,inum,ninterf;
  char *names[3];
  int (*info)();
  
  if ((names[0] =new_nsp_string_n(n+strlen(interf))) == (nsp_string) 0) return FAIL;
  if ((names[1] =new_nsp_string_n(n+strlen(interf_info))) == (nsp_string) 0) return FAIL;
  sprintf(names[0],"%s%s",interface,interf);
  sprintf(names[1],"%s%s",interface,interf_info);
  names[2]= NULL;

  SciLinkInit();
  SciInterInit();
  
  /* Try to find a free position in the interface table : inum **/
  inum=-1;
  for ( i = 0 ; i < LastInterf ; i++) 
    {
      if ( DynInterf[i].ok == 0 ) inum= i;
    }
  inum = ( inum == -1 ) ? LastInterf : inum ;
  
  /* link shared library and search names[0] and names[1] interfaces */
  
  if ( inum >=  MAXINTERF ) 
    {
      Scierror("Error: Maximum number of dynamic interfaces %d\n",MAXINTERF);
      Scierror("has been reached\n");
      return 1;
    }

  SciDynLoad(shared_lib,names,'c',&ilib,0,&rhs);

  if ( ilib < 0 ) return ilib;

  /* store the linked function in the interface function table DynInterf */
  DynInterf[inum].Nshared = ilib;

  if ( SearchInDynLinks(names[0],&DynInterf[inum].func) < 0 ) 
    {
      Scierror("Error: addinter failed, %s not  found!\n",names[0]);
      return -5;
    }
  if ( SearchInDynLinks(names[1],&DynInterf[inum].func_info) < 0 ) 
    {
      Scierror("Error: addinter failed, %s not  found!\n",names[1]);
      return -5;
    }
  strncpy(DynInterf[inum].name,names[0],NAME_MAXL);
  DynInterf[inum].ok = 1;
  
  if ( inum == LastInterf ) LastInterf++;

  /*
   * we add all the new entry names 
   * in the nsp function table funtab 
   */

  info= DynInterf[inum].func_info;
  k=0;
  ninterf= inum +  DYN_INTERF_START ;
  while (1) 
    {
      char *fname;
      function *f;
      info(k,&fname,&f);
      if ( fname == NULL) break;
      if ( EnterFunction(fname,ninterf,k) == FAIL)
	{
	  printf("Error: Table for nsp functions is too small \n");
	}	  
      k++;
    }
  /* 
   * ShowInterf();
   */
  return inum;
}


/*****************************************
 * A blank interface used as default value 
 *****************************************/

int BlankInterface(int i, char *fname, int first, int rhs, int opt, int lhs)
{
  Sciprintf("Error: Interface for function %s is not linked\n",fname);
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

/*
 * remove entries associated to shared lib Nshared 
 * when It is an interface 
 */ 

void RemoveInterf(int Nshared)
{
  int i;
  for ( i = 0 ; i < LastInterf ; i++ ) 
    {
      if (DynInterf[i].ok == 1 &&  DynInterf[i].Nshared == Nshared ) 
	{
	  DynInterf[i].ok = 0;
	  DynInterf[i].func = BlankInterface;
	  DeleteFunctionS(i +  DYN_INTERF_START );
	  break;
	}
    }
}

/*********************************
 * show the interface table 
 *********************************/

/* static void ShowInterf(void) */
/* { */
/*   int i; */
/*   for ( i = 0 ; i < LastInterf ; i++ )  */
/*     { */
/*       if ( DynInterf[i].ok == 1 )  */
/* 	Sciprintf("Interface %d %s\n",i,DynInterf[i].name); */
/*     } */
/* } */








