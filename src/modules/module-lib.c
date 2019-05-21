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
 *
 *
 * Call a function given (Int,Num) 
 */

#include <math.h>
#include <stdio.h>
#include <string.h>

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/matint.h> 
#include <nsp/list.h> 
#include <nsp/hobj.h> 
#include <nsp/type.h> 
#include <nsp/hash.h> 
#include <nsp/file.h> 
#include <nsp/system.h> 

#include <nsp/datas.h>
#include <nsp/interf.h> 

#include <nsp/mod.h> 
#include <nsp/module.h> 
#include <nsp/modulelt.h> 
#include <nsp/me.h> 

/*********************************************
 * Search module mname in module list L 
 *********************************************/

NspModule *nsp_search_module(NspList *L, nsp_const_string mname)
{
  Cell *C = ((NspList *) L)->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ && strcmp(((NspModule *) C->O)->mname,mname)==0 ) 
	return((NspModule *) C->O);
      C = C->next ;
    }
  return NULLMODULE;
} 

/*********************************************
 * insert the module mname[0]#mname[1]#etc... 
 * in the module tree L
 * Warning Mname is NULL terminated 
 *********************************************/

static int nsp_insert_module_last_i(NspList *L,char *dir,char **Mname);

int nsp_insert_module_last(NspList *L,char *dir,char **Mname)
{
  char buf[FSIZE];
  strcpy(buf,dir);
  if ( Mname != NULL && Mname[0][0] != '\0')
    { 
      strcat(buf,"/"); /* XXX */
      strcat(buf,Mname[0]);
    }
  return nsp_insert_module_last_i(L,buf,Mname);
}


/* dir should be big enought to contain a path */ 

static int nsp_insert_module_last_i(NspList *L,char *dir,char **Mname)
{
  NspModule *Loc; 
  if ( Mname[0] == NULL ) return OK;
  Loc = nsp_search_module(L,Mname[0]); 
  if ( Loc == NULLMODULE ) 
    {
      /* if module does not exist create module  */ 
      if ((Loc = module_create(NVOID,dir,Mname[0],NULL)) == NULLMODULE) return FAIL;
      if ( nsp_list_end_insert((NspList *)L,(NspObject *) Loc)== FAIL) return FAIL;
    }
  if ( Mname[1] != NULL ) 
    {
      strcat(dir,"/");
      strcat(dir,Mname[1]);
      /* remove the first entry in SMatrix */ 
      return nsp_insert_module_last_i(Loc->L,dir,++Mname);
    }
  else 
    return OK;
}


/*********************************************
 * search a name in a Lmo 
 *      during the search L can grow 
 * XXXX The returned object is not copied 
 *********************************************/

NspObject * nsp_module_search_name(NspList *L,char **Mname)
{
  NspModule *Loc; 
  if ( L == NULLLIST )  return NULLOBJ;
  if ( Mname[0] == NULL) return NULLOBJ;
  if ( Mname[1] == NULL) 
    {
      /* search all the modules with name="" */
      Cell *C = ((NspList *) L)->first;
      while ( C != NULLCELL) 
	{
	  NspModule *mo;
	  if ((mo=(NspModule *) C->O) != NULLMODULE && strcmp(mo->mname,"")==0 ) 
	    { 
	      NspObject *Ob;
	      if (mo->T != NULL && nsp_hash_find(mo->T,Mname[0],&Ob) == OK) 
		{
		  /* Object found is to be a Me */
		  ((NspMe *) Ob)->path = mo->path;
		  ((NspMe *) Ob)->module = mo->mname;
		  return Ob;
		}
	    }
	  C = C->next ;
	}
      return NULLOBJ;
    }
  
  if (( Loc = nsp_search_module(L,Mname[0]))== NULLMODULE ) return NULLOBJ;
  
  if ( Mname[2] == NULL ) 
    {
      NspObject *Ob;
      /* search Mname[1] in The hash table of Loc */
      if (Loc->T == NULL || nsp_hash_find(Loc->T,Mname[1],&Ob)== FAIL) return NULLOBJ;
      /* Object found is to be a Me */
      ((NspMe *) Ob)->path = Loc->path;
      ((NspMe *) Ob)->module = Loc->mname;
      return Ob;
    }
  else 
    {
      /* search in the submodules */ 
      return  nsp_module_search_name(Loc->L,++Mname);
    }
  return OK;
}

/*********************************************
 * LmoImport(L,SMatrix *Mname) 
 *   add a new module at the head of L with emptyname
 *   which contain links to the module 
 *   given by Mname (if Mname leads to a module)
 *   we assume here that Mname do not contain
 *   empty strings 
 *********************************************/

static NspModule *nsp_module_rec_search(NspList *L,char **Mname)
{
  NspModule *Loc; 
  if ( Mname[0] == NULL) return NULLMODULE;
  if ( ( Loc = nsp_search_module(L,Mname[0]))  == NULLMODULE)  return NULLMODULE;
  if ( Mname[1] == NULL) return Loc; 
  return nsp_module_rec_search(Loc->L,++Mname);
}

int nsp_module_import(NspList *L,char *dir,char **Mname)
{
  NspModule *Loc ;
  Loc = nsp_module_rec_search(L,Mname);
  if ( Loc == NULLMODULE ) 
    {
      /* try to load the module */
      if ( nsp_insert_module_last(L,dir,Mname) == FAIL) return FAIL;
      /* search again */ 
      Loc = nsp_module_rec_search(L,Mname);
      if ( Loc == NULLMODULE ) return FAIL;
    }
  /* this is not a full copy XXX */
  if (( Loc = module_copy_ref(Loc))  == NULLMODULE)  return FAIL;
  /* insert new module at the begining */ 
  if (nsp_list_insert((NspList *)L,(NspObject *) Loc,0) ==  FAIL) return FAIL;
  return OK;
}

/*********************************************
 * Search with %path 
 *********************************************/

#include <sys/types.h> 
#include <sys/stat.h>
#include <unistd.h> 

NspObject *module_path_search_name(NspList *L,NspSMatrix *Sm,char **oname)
{
  struct stat stat_buf;
  int i;
  char buf[FSIZE];
  NspObject *Ob;
  if ( L == NULLLIST) return NULLOBJ;
  /* search oname in L */
  if (( Ob= nsp_module_search_name(L,oname)) != NULLOBJ) return Ob;
  /* search for a file in nsp_path+oname */ 
  for(i= 0 ; i < Sm->mn ; i++) 
    {
      char **loname= oname;
      char **pname = oname;
      strcpy(buf,Sm->S[i]);
      while ( *loname != NULL)  
	{
	  strcat(buf,"/") ; /* XXXXX */ 
	  strcat(buf,*loname);
	  pname= loname;
	  loname++;
	}
      strcat(buf,".bin");
      if ( stat(buf,&stat_buf) == 0 ) 
	{
	  /* file exists */
	  if ( oname[1]== NULL) 
	    {
	      char *path[]={NVOID,NULL};
	      if ( nsp_insert_module_last(L,Sm->S[i],path) == FAIL) return NULLOBJ;
	      Ob= nsp_module_search_name(L,oname);
	      return Ob;
	    }
	  else 
	    {
	      char *name = *pname;
	      *pname = NULL;
	      if ( nsp_insert_module_last(L,Sm->S[i],oname) == FAIL) 
		{
		  *pname = name;
		  return NULLOBJ;
		}
	      *pname = name;
	      Ob= nsp_module_search_name(L,oname);
	      return Ob;
	    }
	}
    }
  return NULLOBJ ; 
}


/*********************************************
 * Search with %path 
 * then load the object if found in the 
 * current frame and also returns it 
 *********************************************/

NspObject *module_path_search_object(NspList *L,NspSMatrix *Sm,char **oname)
{
  NspFile *F;
  char buf[FSIZE], *loc;
  /* is object in the module search path */ 
  NspObject *Ob=module_path_search_name(L,Sm,oname);
  if ( Ob == NULLOBJ) return NULLOBJ; 
  /* object is supposed to be a Me object 
   * 
   */ 
  strcpy(buf,((NspMe*)Ob)->path);
  strcat(buf,"/"); 
  strcat(buf,NSP_OBJECT((NspMe*)Ob)->name);
  strcat(buf,".bin");
  /* XXX : must clean Ob ? */
  /* we load the binary object found in buf */ 
  if (( F =nsp_file_open_xdr_r(buf)) == NULLSCIFILE) return NULLOBJ;
  Ob=nsp_object_xdr_load(F->obj->xdrs);
  nsp_object_xdr_load(F->obj->xdrs); /* not to have a warning when closing */
  if (nsp_file_close_xdr_r(F) == FAIL)
    {
      nsp_void_object_destroy(&Ob);
      return NULLOBJ;
    }
  strcpy(buf,oname[0]);
  oname++;
  while ( *oname != NULL) {
    strcat(buf,"#"); /* XXXXX : should be in .h MOD_SEP # */
    strcat(buf,*oname++);
  }
  if ((loc = Ob->type->set_name(Ob,buf))== NULL)
    {
      nsp_void_object_destroy(&Ob);
      return NULLOBJ;
    }
  nsp_frame_replace_object(Ob,-1);
  return Ob;
}

  
