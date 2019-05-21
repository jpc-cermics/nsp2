/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/plist.h> 
#include <nsp/system.h>
#include <nsp/nsptcl.h> /* for nsp_tail */

/* XXXX */
extern const char *nsp_get_libdir(int num);

/*
 * Creation of a NspPList 
 * returns NULLP_PLIST on failure 
 * The given PList is stored inside the NspPList
 */

NspPList *NspPListCreate(const char *name, PList L,char *filename)
{
  NspPList *P_L = new_plist();

  if ( P_L == NULLP_PLIST ) 
    {
      Scierror("Error: No more space\n");
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
  P_L->cpu = 0;
  P_L->counter = 0;
  P_L->trace = FALSE;
  return(P_L);
}

/*
 * Copy of a NspPList 
 * The copy has  name NVOID
 * returns NULLP_PLIST on failure 
 */

NspPList *NspPListCopy(NspPList *A)
{
  NspPList *P_L = NspPListCreate(NVOID,NULLPLIST,A->file_name);
  if ( P_L == NULL) return P_L;
  if ( A->D != NULL )
    {
      if ( ( P_L->D =nsp_plist_copy(A->D)) == NULLPLIST ) 
	{
	  NspPListDestroy(P_L);
	  return (NULLP_PLIST);
	}
    }
  P_L->dir = A->dir;
  return(P_L);
}

/* the same but without local variables */

NspPList *NspPListCopy_no_local_vars(NspPList *A)
{
  NspPList *P_L;
  P_L = NspPListCreate(NVOID,NULLPLIST,A->file_name);
  if (( P_L->D =nsp_plist_copy_no_local_vars(A->D)) == NULLPLIST ) 
    return (NULLP_PLIST);
  return(P_L);
}

/*
 * Delete the NspPList
 */

void NspPListDestroy(NspPList *P_L)
{
  if ( P_L!= NULLP_PLIST) 
    {
      nsp_object_destroy_name(NSP_OBJECT(P_L));
      FREE(P_L->file_name) ;
      if ( P_L->D != NULL) nsp_plist_destroy(&P_L->D);
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


int nsp_plist_get_path(char *fname,NspPList *P_L)
{
  int rep = OK;
  nsp_string tail;
  const char *dir= nsp_get_libdir(P_L->dir);
  /* get the name of the source file */
  if ( dir != NULL) 
    {
      /* if dir is non-null then the macros is from library */
      if ((tail = nsp_tail(P_L->file_name)) !=NULL) 
	{
	  snprintf(fname,FSIZE,"%s/%s",dir,tail);
	  nsp_string_destroy(&tail);
	}
      else 
	{
	  strcpy(fname,"");
	  rep = FAIL;
	}
    }
  else if ( P_L->file_name != NULL) 
    {
      /* interactive macro or loaded by exec */
      strncpy(fname,P_L->file_name,FSIZE);
    }
  else 
    {
      strcpy(fname,"");
      rep = FAIL;
    }
  return rep;
}

void NspPListPrint(NspPList *P_L, int indent,const char *name, int rec_level)
{
  int color=TRUE, target = 4 /* terminal */, space=FALSE, columns=90;
  int ok;
  char fname[FSIZE+1];
  const char *pname = (name != NULL) ? name : NSP_OBJECT(P_L)->name;
  ok = nsp_plist_get_path(fname,P_L);
  if (user_pref.pr_as_read_syntax ) 
    {
      if ( P_L->D != NULL )
	nsp_plist_pretty_print(P_L->D,indent+2,color,target,space,columns);
    }
  else
    {
      if ( ok == OK ) 
	Sciprintf1(indent,"%s\t=\t\tpl (file='%s')\n",pname,fname);
      else 
	Sciprintf1(indent,"%s\t=\t\tpl\n",pname);
      if ( user_pref.pr_depth  <= rec_level -1 ) return;
      if ( P_L->D != NULL )
	nsp_plist_pretty_print(P_L->D,indent+2,color,target,space,columns);
    }
  Sciprintf("\n");
}

void NspPListPrint_latex(NspPList *P_L) // , int indent,const char *name, int rec_level)
{
  int color=TRUE, target = 3 /* latex */, space=FALSE, columns=90;
  int ok, indent=0;
  char fname[FSIZE+1];
  const char *pname =  NSP_OBJECT(P_L)->name;
  ok = nsp_plist_get_path(fname,P_L);
  if ( ok == OK ) 
    Sciprintf1(indent,"%s\t=\t\tpl (file='%s')\n",pname,fname);
  else 
    Sciprintf1(indent,"%s\t=\t\tpl\n",pname);
  // if ( user_pref.pr_depth  <= rec_level -1 ) return;
  if ( P_L->D != NULL )
    nsp_plist_pretty_print(P_L->D,indent+2,color,target,space,columns);
  Sciprintf("\n");
}






