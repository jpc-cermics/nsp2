/*********************************************************************
 * This Software is ( Copyright ENPC 1998 )                          *
 * Jean-Philippe Chancelier Enpc/Cergrene                            *
 *********************************************************************/

#include <math.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/objxdr.h"
#include "nsp/datas.h"
#include "nsp/interf.h" 

/**********************************************************
 * Lmo sepcific functions ....
 * Lmo: list of modules can be casted to a List 
 *******************************************************/

/******************************************
 * Res=ELmoCreate 
 * Creates a new empty list with name name 
 * and type tname if we want to define a tlist 
 ******************************************/

NspLmo *ELmoCreate(char *name)
{
  NspList *L = EListCreate(name,NULLSTRING);
  if ( L == NULLLIST ) return   NULLLMO; 
  /* 
     L->otype = LMO;
     L->ftype = Lmo_Type;
  */
  L->first = NULLCELL;
  return((NspLmo *) L);
}

/*************************************************
 * Delete the Lmo and all its elements 
 *************************************************/

void LmoDestroy(NspLmo *l)
{
  ListDestroy((NspList*) l);
} 

/******************************
 * Res=LmoCopy(L)
 * returns in Res a copy of the Lmo L 
 * elements inside the list are copied too
 *******************************/

NspLmo *LmoCopy(NspLmo *lmo)
{
  NspList *L = ListCopy((NspList *) lmo);
  if ( L == NULLLIST ) return(NULLLMO) ;
  /*
    L->otype = LMO;
    L->ftype = Lmo_Type;
  */
  return(L);
} 

/******************************************
 *Scilab Display of an Object of type Lmo 
 ******************************************/

void LmoInfo(NspLmo *L, int indent)
{
  ListInfo((NspList *)L,indent);
} 

/*********************************************
 *  Scilab Display of an Object of type Lmo 
 *********************************************/

void LmoPrint(NspLmo *L, int indent)
{
  ListPrint((NspList *)L,indent);
} 

/*********************************************
 * Search module mname in module list L 
 *********************************************/

/* XXXX should add in list a Search with prédicate */ 

NspMod *LmoSearchMod(NspLmo *L, String *mname)
{
  Cell *C = ((NspList *) L)->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ && strcmp(((NspMod *) C->O)->mname,mname)==0 ) 
	return((NspMod *) C->O);
      C = C->next ;
    }
  return NULLMOD;
} 

/*********************************************
 * insert the module mname[0]#mname[1]#etc... 
 * in the module tree L
 * Warning Mname is NULL terminated 
 *********************************************/

static int LmoInsertLast1(NspLmo *L,char *dir,char **Mname);

/* XXXXX a factoriser quelque part */ 
#define MAX_PATH 1024 

int LmoInsertLast(NspLmo *L,char *dir,char **Mname)
{
  char buf[MAX_PATH];
  strcpy(buf,dir);
  if ( Mname != NULL && Mname[0][0] != '\0')
    { 
      strcat(buf,"/"); /* XXX */
      strcat(buf,Mname[0]);
    }
  return LmoInsertLast1(L,buf,Mname);
}


/* dir should be big enought to contain a path */ 

static int LmoInsertLast1(NspLmo *L,char *dir,char **Mname)
{
  NspMod *Loc; 
  if ( Mname[0] == NULL ) return OK;
  Loc = LmoSearchMod(L,Mname[0]); 
  if ( Loc == NULLMOD ) 
    {
      /* if module does not exist create module  */ 
      if ((Loc = ModCreate(NVOID,dir,Mname[0])) == NULLMOD) return FAIL;
      if ( EndInsert((NspList *)L,(NspObject *) Loc)== FAIL) return FAIL;
    }
  if ( Mname[1] != NULL ) 
    {
      strcat(dir,"/");
      strcat(dir,Mname[1]);
      /* remove the first entry in SMatrix */ 
      return LmoInsertLast1(Loc->L,dir,++Mname);
    }
  else 
    return OK;
}


/*********************************************
 * search a name in a Lmo 
 *      during the search L can grow 
 * XXXX The returned object is not copied 
 *********************************************/

NspObject * LmoSearchName(NspLmo *L,char **Mname)
{
  NspMod *Loc; 
  if ( L == NULLLMO)  return NULLOBJ;
  if ( Mname[0] == NULL) return NULLOBJ;
  if ( Mname[1] == NULL) 
    {
      /* search all the modules with name="" */
      Cell *C = ((NspList *) L)->first;
      while ( C != NULLCELL) 
	{
	  NspMod *mo;
	  if ((mo=(NspMod *) C->O) != NULLMOD && strcmp(mo->mname,"")==0 ) 
	    { 
	      NspObject *Ob;
	      if ( HashFind(mo->T,Mname[0],&Ob) == OK) 
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
  
  if (( Loc = LmoSearchMod(L,Mname[0]))== NULLMOD ) return NULLOBJ;
  
  if ( Mname[2] == NULL ) 
    {
      NspObject *Ob;
      /* search Mname[1] in The hash table of Loc */
      if (  HashFind(Loc->T,Mname[1],&Ob)== FAIL) return NULLOBJ;
      /* Object found is to be a Me */
      ((NspMe *) Ob)->path = Loc->path;
      ((NspMe *) Ob)->module = Loc->mname;
      return Ob;
    }
  else 
    {
      /* search in the submodules */ 
      return  LmoSearchName(Loc->L,++Mname);
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

static NspMod *LmoRecSearch(NspLmo *L,char **Mname)
{
  NspMod *Loc; 
  if ( Mname[0] == NULL) return NULLMOD;
  if ( ( Loc = LmoSearchMod(L,Mname[0]))  == NULLMOD)  return NULLMOD;
  if ( Mname[1] == NULL) return Loc; 
  return LmoRecSearch(Loc->L,++Mname);
}

int LmoImport(NspLmo *L,char *dir,char **Mname)
{
  NspMod *Loc ;
  Loc = LmoRecSearch(L,Mname);
  if ( Loc == NULLMOD ) 
    {
      /* try to load the module */
      if ( LmoInsertLast(L,dir,Mname) == FAIL) return FAIL;
      /* search again */ 
      Loc = LmoRecSearch(L,Mname);
      if ( Loc == NULLMOD ) return FAIL;
    }
  /* this is not a full copy XXX */
  if (( Loc = ModCopy(Loc))  == NULLMOD)  return FAIL;
  /* insert new module at the begining */ 
  if ( ListInsert((NspList *)L,(NspObject *) Loc,0) ==  FAIL) return FAIL;
  return OK;
}

/*********************************************
 * Search with %path 
 *********************************************/

#include <sys/types.h> 
#include <sys/stat.h>
#include <unistd.h> 

NspObject *lmo_path_search_name(NspLmo *L,NspSMatrix *Sm,char **oname)
{
  struct stat stat_buf;
  int i;
  char buf[MAX_PATH];
  NspObject *Ob;
  if ( L == NULLLMO) return NULLOBJ;
  /* search oname in L */
  if (( Ob= LmoSearchName(L,oname)) != NULLOBJ) return Ob;
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
	      if ( LmoInsertLast(L,Sm->S[i],path) == FAIL) return NULLOBJ;
	      Ob= LmoSearchName(L,oname);
	      return Ob;
	    }
	  else 
	    {
	      char *name = *pname;
	      *pname = NULL;
	      if ( LmoInsertLast(L,Sm->S[i],oname) == FAIL) 
		{
		  *pname = name;
		  return NULLOBJ;
		}
	      *pname = name;
	      Ob= LmoSearchName(L,oname);
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

NspObject *lmo_path_search_object(NspLmo *L,NspSMatrix *Sm,char **oname)
{
  NspFile *F;
  char buf[MAX_PATH], *loc;
  /* is object in the module search path */ 
  NspObject *Ob=lmo_path_search_name(L,Sm,oname);
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
  if (( F = SciFileOpenXdrR(buf)) == NULLSCIFILE) return NULLOBJ;
  Ob= ObjXdrLoad(F);
  ObjXdrLoad(F); /* not to have a warning when closing */
  if ( SciFileCloseXdrR(F) == FAIL)
    {
      VoidObjDestroy(&Ob);
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
      VoidObjDestroy(&Ob);
      return NULLOBJ;
    }
  FrameReplaceObj(Ob);
  return Ob;
}

  
