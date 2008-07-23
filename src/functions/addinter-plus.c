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

/******************************************************
 * Test for scilab library loaded when needed 
 * similar to addinter but we do not add the function list 
 * in fundef since it is already present 
 * sometimes we need to link several interfaces with the same code 
 * XXXXXXX : A revoir 
 * XXXXXX
 ******************************************************/

int  zzSciLibLoad(int num_names,char *names[],char **files,int nums[],int *err)
{
  int i,rhs=2,inum,ilib=0,j;
  SciLinkInit();
  SciInterInit();
  *err=0;
  
  for ( j=0 ; j < num_names ; j++) 
    {
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
	  Sciprintf("Maximum number of dynamic interfaces %d\r\n",MAXINTERF);
	  Sciprintf("has been reached\r\n");
	  *err=1;
	  return -1 ;
	}
      else 
	nums[j]=inum;
      if ( inum == LastInterf ) LastInterf++;
    }
  nsp_link_library(0,&rhs,&ilib,files,names,"f");
  if ( ilib < 0 ) 
    {
      *err=1;  return -1;
    }
  /* store the linked function in the interface function table DynInterf 
   */

  for ( j=0 ; j < num_names ; j++) 
    {
      DynInterf[nums[j]].Nshared = ilib;
      if ( SearchInDynLinks(names[0],&DynInterf[nums[j]].func) < 0 ) 
	{
	  Sciprintf("addinter failed for %s Not  found!\r\n",names[j]);
	  return -1;
	}
      else
	{
	  strncpy(DynInterf[nums[j]].name,names[j],NAME_MAXL);
	  DynInterf[nums[j]].ok = 1;
	}
    }
  ShowInterf();
  return 0;
}

#define MAX_ENV 256 

void zzBuildName(char *name,char *str)
{
  int  nc= MAX_ENV;
  GetenvB("SCI",name,nc);
  strcat(name,"/libs/");
  strcat(name,str);
}

void zzCallDynInterf(int *pos,int num_names,int namepos,char *names[],int nums[],char *files[])
{
  int imes = 9999;
  if ( *pos == -1 || DynInterf[*pos].ok == 0) 
    {
      /** need to load or reload the interface **/
      int pos1, err=0;
      SciLibLoad(num_names,names,files,nums,&err);
      if (err != 1) *pos = nums[namepos];
    }
  if ( DynInterf[*pos].ok == 1 ) 
    (*DynInterf[*pos].func)();
  else 
    {
      Sciprintf("Interface %s not linked\r\n",DynInterf[*pos].name);
      C2F(error)(&imes);
      return;
    }
}  

