/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )
 * Jean-Philippe Chancelier Enpc/Cergrene       
 *********************************************************************/

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "nsp/math.h" 
#include "nsp/plisttoken.h" /** for name_maxl **/
#include "nsp/object.h" 
#include "nsp/interf.h"
#include "nsp/objxdr.h"
#include "nsp/datas.h"

/*********************************************************************
 * Module Object in Scilab : 
 *********************************************************************/

/*********************************************************************
 * Create a Module 
 *    mname is the module name 
 *    path the path leading to the module (it should be an absolute path)
 *    The table is filled with the contents of names 
 *    The list of sub modules is empty 
 *********************************************************************/

NspMod *ModCreate(char *name,char *path,char *mname)
{
  NspMod *M = new_mod();
  if ( M == NULLMOD ) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMOD);
    }
  /* shared by all objects */
  if ((NSP_OBJECT(M)->name = NewString(name))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMOD);
    }
  NSP_OBJECT(M)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /*
    M->otype =  MODULE;
    M->ftype = Mod_Type;
  */
  /* specific for Module */
  if ((M->path = NewString(path))== NULLSTRING)
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMOD);
    }
  if ((M->mname = NewString(mname))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMOD);
    }
  if ((M->T= hash_create(NVOID,10))== NULLHASH) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMOD);
    }
  /* fill Mod->T with names contained in file 
   * dir/names 
   * XXX : could also scan dir in the future 
   */
  if ( ModFill(M) == FAIL) return NULLMOD;
  if ((M->L = ELmoCreate(NVOID))== NULLLMO) return NULLMOD;
  M->flag=0;
  return(M);
} 

/*************************************************
 * Copy of a Module (the copy has  name NVOID)
 * note that T and L are not copied XXXXXXX
 ***************************************************/

NspMod *ModCopy(NspMod *Mo)
{
  NspMod *M = new_mod();
  if ( M == NULLMOD ) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMOD);
    }
  /* shared by all objects */
  if ((NSP_OBJECT(M)->name = NewString(NVOID))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMOD);
    }
  NSP_OBJECT(M)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  /* 
     M->otype = MODULE;
     M->ftype = Mod_Type;
  */
  /* specific for Module */
  M->path = Mo->path;
  if ((M->mname = NewString(NVOID))== NULLSTRING) 
    {
      Scierror("Error:\tRunning out of memory\n");
      return(NULLMOD);
    }
  M->T = Mo->T;
  M->L = Mo->L;
  M->flag=1;
  return(M);
}

/*****************************************************
 * Delete ModTable and the entries stored in the hash table
 *****************************************************/

void ModDestroy(NspMod *Mo)
{
  if ( Mo == NULLMOD ) return ; 
  FREE(NSP_OBJECT(Mo)->name);
  if ( Mo->flag  == 0) 
    {
      hash_destroy(Mo->T);
      ListDestroy(Mo->L);
      FREE(Mo->path);
    }
  FREE(Mo) ;
}

/*********************************************
 * ModInfo 
 *********************************************/

void ModInfo(NspMod *Mo, int indent)
{
  int i;
  if ( Mo == NULLMOD) 
    {
      Sciprintf("Null Pointer Module \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("Module name=%s path=%s \n",NSP_OBJECT(Mo)->name,Mo->path);
  for ( i=0 ; i < indent ; i++) Sciprintf(" [\n");
  hash_info(Mo->T,indent+2);
  ListInfo(Mo->L,indent+2);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ]\n");
}

/**************************************************
 * ModPrint 
 **************************************************/

void ModPrint(NspMod *Mo, int indent)
{
  int i;
  if ( Mo == NULLMOD) 
    {
      Sciprintf("Null Pointer Module \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s =\tmodule\n",NSP_OBJECT(Mo)->name); 
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("<Module name=%s path=%s\n",Mo->mname,Mo->path);
  hash_print(Mo->T,indent+2);
  ListPrint(Mo->L,indent+2);
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf(">\n");
}

/*********************************************************************
 * fill a module hash table 
 * using file: names 
 * or scanning the directory for .bin files  XXXXX 
 *********************************************************************/

/** XXXXX remplacer 1048 par une macro **/

#define F_SIZE 1048

int ModFill(NspMod *Mo)
{
  char F[F_SIZE];
  FILE *f;
  strcpy(F,Mo->path); 
  strcat(F,"/names");
  if (( f= fopen(F,"r") ) == (FILE *)0 )
    {
      Scierror("Error:\t:Can't open file %s\n",F);
      return FAIL;
    }
  while (1) 
    {
      NspMe *elt;
      int rep;
      char name[NAME_MAXL];
      rep = fscanf(f,"%s",name);
      if ( rep == 0 || rep == EOF ) break;
      if ((elt = MeCreate(name))== NULLME ) 
	{
	  fclose(f); return FAIL;
	}
      if (nsp_hash_enter(Mo->T,(NspObject *) elt) == FAIL) 
	{
	  fclose(f); return FAIL;
	}
    }
  fclose(f);
  return OK;
}


