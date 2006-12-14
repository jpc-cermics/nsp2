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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/machine.h"

#include "nsp/object.h"

/*
 * Creation of a NspPList 
 * returns NULLP_PLIST on failure 
 * The given PList is stored inside the NspPList
 */

NspPList *NspPListCreate(char *name, PList L,char *filename)
{
  NspPList *P_L = new_plist();

  if ( P_L == NULLP_PLIST ) 
    {
      Scierror("No more space\n");
      return(NULLP_PLIST);
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(P_L),name) == NULL)
    return NULLP_PLIST;
  if ( filename != NULL)
    {
      if ((P_L->file_name =new_nsp_string(filename))== NULLSTRING) return NULLP_PLIST;
    }
  else 
    {
      P_L->file_name = NULL;
    }
  NSP_OBJECT(P_L)->ret_pos = -1 ;
  P_L->D = L;
  P_L->dir = -1;
  return(P_L);
}

/*
 * Copy of a NspPList 
 * The copy has  name NVOID
 * returns NULLP_PLIST on failure 
 */

NspPList *NspPListCopy(NspPList *A)
{
  NspPList *P_L;
  P_L = NspPListCreate(NVOID,NULLPLIST,A->file_name);
  if (( P_L->D =nsp_plist_copy(A->D)) == NULLPLIST ) 
    return (NULLP_PLIST);
  return(P_L);
}

/*
 * Delete the NspPList NspPList 
 */

void NspPListDestroy(NspPList *P_L)
{
  if ( P_L!= NULLP_PLIST) 
    {
      nsp_object_destroy_name(NSP_OBJECT(P_L));
      FREE(P_L->file_name) ;
      nsp_plist_destroy(&P_L->D);
      FREE(P_L) ;
    };
}

/*
 * NspPListPrInt : display Info on NspPList P_L 
 */

void NspPListPrInt(NspPList *P_L)
{
  if ( P_L == NULLP_PLIST) 
    {
      Sciprintf("Null Pointer NspPList \n");
      return;
    }
  Sciprintf("NspPList %s\n",NSP_OBJECT(P_L)->name);
  nsp_plist_print_internal(P_L->D);
}

/*
 * NspPListInfo : display Info on NspPList P_L 
 */

/* XXXX */
extern const char *nsp_get_libdir(int num);

void NspPListInfo(NspPList *P_L, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(P_L)->name;
  const char *dir= nsp_get_libdir(P_L->dir),*dir1;
  dir1 = (dir != NULL) ? dir : P_L->file_name;
  if ( dir1 != NULL) 
    Sciprintf1(indent,"%s\t=\t\tpl (file='%s')\n",pname,dir1);
  else 
    Sciprintf1(indent,"%s\t=\t\tpl\n",pname);
}

/*
 * NspPListPrint : writes P_L Objet 
 */

void NspPListPrint(NspPList *P_L, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(P_L)->name;
  const char *dir= nsp_get_libdir(P_L->dir),*dir1;
  dir1 = (dir != NULL) ? dir : P_L->file_name;
  if (user_pref.pr_as_read_syntax)
    {
      nsp_plist_pretty_print(P_L->D,indent+2);
    }
  else
    {
      if ( dir1 != NULL )
	Sciprintf1(indent,"%s\t=\t\tpl (file='%s')\n",pname,dir1);
      else 
	Sciprintf1(indent,"%s\t=\t\tpl\n",pname);
      if ( user_pref.pr_depth  <= rec_level -1 ) return;
    }
  nsp_plist_pretty_print(P_L->D,indent+2);
  Sciprintf("\n");
}


/*
 * NspPList2SMatrix 
 */

NspSMatrix * NspPList2SMatrix(NspPList *P_L, int indent)
{
  return nsp_plist2smatrix(P_L->D,indent);
}



