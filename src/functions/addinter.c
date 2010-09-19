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
 * addinter related functions : used to dynamically link functions 
 * from an interface in Scilab function table. 
 * An interface is characterized by two functions:
 *   a function F(i,stack,rhs,opt,lhs) which is used for function call
 *   and a function F_info which is used to find entry names 
 *   
 *   void AddInter(files,iname,enames,err)
 *   void RemoveInterf(Nshared)
 *   void ShowInterf()
 */

#include <nsp/nsp.h>
#include <nsp/sciio.h>
#include <nsp/interf.h>
#include <nsp/epoints.h>
#include <nsp/linking.h> 

#include "FunTab.h"
#include "addinter.h"

Iel DynInterf[MAXINTERF];
int LastInterf=0;

static void nsp_interfaces_initialize(void);

/**
 * nsp_dynamic_interface:
 * @shared_lib: a string which gives the path to a shared library 
 * @interface: name of the interface to be searched in @shared_lib
 * @ilib: 
 * 
 * if @shared_lib is non null it is dynamically linked 
 * and @interface symbols are searched in the library and added in 
 * the function table. 
 * if @shared_lib is NULL then ilib gives an already 
 * linked shared library in which symbols are searched.
 * 
 * ilib is also used to return the integer id associated to the shared 
 * library in case of successful link.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_dynamic_interface(nsp_const_string shared_lib,nsp_const_string interface,int *ilib)
{
  const char interf[]="_Interf";
  const char interf_info[]="_Interf_Info";
  int n=strlen(interface),k,ret=FAIL;
  int i,rhs=2,inum,ninterf;
  char *names[3];
  int (*info)();
  
  if ((names[0] =new_nsp_string_n(n+strlen(interf))) == (nsp_string) 0) return FAIL;
  if ((names[1] =new_nsp_string_n(n+strlen(interf_info))) == (nsp_string) 0) 
    {
      nsp_string_destroy(&names[0]);
      return FAIL;
    }
  sprintf(names[0],"%s%s",interface,interf);
  sprintf(names[1],"%s%s",interface,interf_info);
  names[2]= NULL;

  nsp_link_initialize(); 
  nsp_interfaces_initialize();

  /* Try to find a free position in the interface table : inum */
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
      Scierror("       has been reached\n");
      goto err;
    }
  
  /* using shared lib with id ilib if shared_lib is null 
   * or try to link a new shared library given by its name 
   * shared_lib
   */
  
  nsp_dynamic_load(shared_lib,names,'c',ilib,( shared_lib == NULL) ? 1 : 0,&rhs);
  
  if ( ilib < 0 )
    {
      goto err;
    }
  
  /* store the linked function in the interface function table DynInterf */
  DynInterf[inum].Nshared = *ilib;
  if ( nsp_link_search(names[0],*ilib,&DynInterf[inum].func) < 0 ) 
    {
      Scierror("Error: addinter failed, %s not  found!\n",names[0]);
      goto err;
    }
  if ( nsp_link_search(names[1],*ilib,&DynInterf[inum].func_info) < 0 ) 
    {
      Scierror("Error: addinter failed, %s not  found!\n",names[1]);
      goto err;
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
      if ( nsp_enter_function(fname,ninterf,k) == FAIL)
	{
	  Scierror("Error: Table for nsp functions is too small \n");
	  goto err;
	}	  
      k++;
    }
  /* 
   * ShowInterf();
   */
  ret = OK;
 err:
  nsp_string_destroy(&names[0]);
  nsp_string_destroy(&names[1]);
  return ret;
}


/*
 * A blank interface used as default value 
 */

static int nsp_default_interface(int i, char *fname, int first, int rhs, int opt, int lhs)
{
  Sciprintf("Error: Interface for function %s is not linked\n",fname);
  return RET_BUG;
}

/* Initialize the DynInterf array. 
 * 
 */

static void nsp_interfaces_initialize(void) 
{
  static int first_entry=0;
  if ( first_entry == 0) 
    {
      int i;
      for ( i= 0 ; i < MAXINTERF ; i++) 
	{ 
	  DynInterf[i].ok=0;
	  DynInterf[i].func = nsp_default_interface;
	}
      first_entry++;
    }
}

/**
 * RemoveInterf:
 * @Nshared: integer 
 * 
 * removes entries associated to the shared/dynamic library 
 * which is associated to id @Nshared and which is a Nsp interface. 
 * 
 **/

void nsp_remove_interface(int Nshared)
{
  int i;
  for ( i = 0 ; i < LastInterf ; i++ ) 
    {
      if (DynInterf[i].ok == 1 &&  DynInterf[i].Nshared == Nshared ) 
	{
	  DynInterf[i].ok = 0;
	  DynInterf[i].func = nsp_default_interface;
	  nsp_delete_interface_functions(i +  DYN_INTERF_START );
	  break;
	}
    }
}




