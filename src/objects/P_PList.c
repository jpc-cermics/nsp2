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
  if (( NSP_OBJECT(P_L)->name =new_nsp_string(name))== NULLSTRING) return NULLP_PLIST;
  if ( filename != NULL)
    {
      if ((P_L->file_name =new_nsp_string(filename))== NULLSTRING) return NULLP_PLIST;
    }
  else 
    P_L->file_name = NULL;
  NSP_OBJECT(P_L)->ret_pos = -1 ;
  P_L->D = L;
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
  if (( P_L->D = PListCopy(A->D)) == NULLPLIST ) 
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
      FREE(NSP_OBJECT(P_L)->name) ;
      PListDestroy(&P_L->D);
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
  PListPrInt(P_L->D);
}

/*
 * NspPListInfo : display Info on NspPList P_L 
 */

void NspPListInfo(NspPList *P_L, int indent)
{
  int j;
  if ( P_L == NULLP_PLIST) 
    {
      Sciprintf("Null Pointer NspPList \n");
      return;
    }
  for ( j=0 ; j < indent ; j++) Sciprintf(" ");
  Sciprintf("NspPList %s\n",NSP_OBJECT(P_L)->name);
  PListInfo(P_L->D,indent);
}

/*
 * NspPListPrint : writes P_L Objet 
 */

void NspPListPrint(NspPList *P_L, int indent)
{
  int j;
  for ( j=0 ; j < indent ; j++) Sciprintf(" ");
  Sciprintf("%s",NSP_OBJECT(P_L)->name);
  if ( P_L->file_name != NULL) 
    Sciprintf("\t=\t\tpl (file='%s')\n",P_L->file_name);
  else 
    Sciprintf("\t=\t\tpl\n");
  PListPrettyPrint(P_L->D,indent);
  Sciprintf("\n");
}

/*
 * NspPList2SMatrix 
 */

NspSMatrix * NspPList2SMatrix(NspPList *P_L, int indent)
{
  return PList2SMatrix(P_L->D,indent);
}

/*
 * NspPListSave : Saves P_L Objet 
 */

int NspPListSave(NspPList *P_L)
{
  Sciprintf("[%s ]\n",NSP_OBJECT(P_L)->name);
  return(PListSave(P_L->D));
}




