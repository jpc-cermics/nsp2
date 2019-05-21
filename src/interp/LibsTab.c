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
 * Hash table for accessing macros or ptimitives. 
 * This hash table is used as a cache for macros
 *********************************************************************/

#include <string.h>
#include <math.h>
#include <stdio.h>
#include <glib.h>

#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/function.h>
#include <nsp/hash.h> 

#include <nsp/plisttoken.h> /** for name_maxl **/
#include <nsp/interf.h>
#include <nsp/datas.h>
#include <nsp/system.h> /* FSIZE+1 */
#include <nsp/seval.h>
#include <nsp/funtab.h>
#include <nsp/callfunc.h> 
#include <nsp/addinter.h> 

NspObject *nsp_find_macro_or_func(const char *str,int *type);
int nsp_delete_macros(const char *dirname);
static void nsp_remove_macro_or_func(const char *name,int type);
static int nsp_hash_enter_multiple(NspHash *H, NspObject *Obj);

static NspSMatrix *LibDirs = NULLSMAT;
static NspHash* nsp_functions_table = NULLHASH;

/**
 * nsp_get_libdir:
 * @num: 
 * 
 * get the directory name corresponding to id @num.
 * 
 * Return value: %NULL or a pointer to the directory name.
 **/

const char *nsp_get_libdir(int num)
{
  if ( LibDirs != NULLSMAT && num >=0 && num < LibDirs->mn -1 )
    return LibDirs->S[num];
  else 
    return NULL;
}


/**
 * nsp_enter_macro:
 * @str: a string 
 * @dir: an integer 
 * 
 * creates a #NspPList object with name @str and inserts 
 * the object in the function table.
 * 
 * Returns: %OK or %FAIL.
 **/

int nsp_enter_macro(const char *str,int dir)
{
  NspPList *loc; 
  if ((loc = NspPListCreate(str,NULL,NULL)) == NULL) return FAIL;
  loc->dir = dir;
  return  nsp_hash_enter_multiple(nsp_functions_table,NSP_OBJECT(loc));
}


/**
 *nsp_enter_macros:
 * @dirname: a string giving an absolute dire name
 * @recursive: a flag %TRUE or %FALSE 
 * @compile: a flag %TRUE or %FALSE 
 * 
 * Insert @dirname in the search list for macros. 
 * file with suffix bin i.e *.bin are searched in @dirname 
 * and inserted in the function hash table. 
 * If @compile is %TRUE files with a .sci 
 * suffix are first parsed and saved as binary files *.bin 
 * (one file for each function). 
 * If @recursive is %TRUE directories are recursively scaned and 
 * added to the function table.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_enter_macros(const char *dir_name,int recursive,int compile)
{
  char dirname[FSIZE+1];
  char filename[FSIZE+1];
  int  flen, flag=-1,i;
  /* recursively add search directories */
  /* expand macros in dir_name -> dirname */
  nsp_path_expand(dir_name,dirname,FSIZE);
  GDir *dir =  g_dir_open(dirname,0,NULL);
  if ( dir == NULL) 
    {
      Scierror("Error: Cannot open directory %s\n",dirname);
      return FAIL;
    }

  /* is dirname in the directory array
   */
  for ( i = 0 ; i < LibDirs->mn -1 ; i++) 
    {
      if (strcmp(dirname,LibDirs->S[i])==0) 
	{
	  flag=i;break;
	}
    }
  
  /* Create a new entry for dir if not already present */
  if ( flag == -1 ) 
    {
      flag = LibDirs->mn -1 ;
      if (nsp_smatrix_add_rows(LibDirs,1)==FAIL) return FAIL;
      if (nsp_string_resize(&(LibDirs->S[flag]),strlen(dirname)) == FAIL)
	return FAIL;
      strcpy(LibDirs->S[flag],dirname);
    }
  else 
    {
      /* delete entries in macro_table before re-entering */
      if ( nsp_delete_macros(dirname) == FAIL) 
	return FAIL;
    }
  
  /* update binaries if requested 
   */
  if ( compile == TRUE )nsp_parse_eval_dir_full(dirname);
  
  strcpy(filename,dirname);
  flen=strlen(filename);
  while (1) 
    {
      const gchar *fname=  g_dir_read_name(dir);
      if (fname == NULL) break;
      filename[flen]='/'; 
      filename[flen+1]='\0'; 
      strcat(filename,fname);
      if (g_file_test (filename, G_FILE_TEST_IS_DIR))
	{
	  if ( recursive == TRUE ) 
	    {
	      /* Sciprintf("%s visited\n",filename); */
	      nsp_enter_macros(filename,recursive,compile);
	    }
	}
      else 
	{
	  if ( strlen(fname) >= 4 && strncmp(".bin",fname + strlen(fname)-4,4)==0)
	    {
	      char name[NAME_MAXL];
	      strcpy(name,fname);
	      name[strlen(fname)-4]='\0';
	      /* 
	       */
	      if ( nsp_enter_macro(name,flag) == FAIL ) 
		{
		  Scierror("Error:\t: htable is full increase default size in LibsTab.c\n");
		  g_dir_close (dir);
		  return FAIL;
		}
	    }
	}
    } 
  g_dir_close (dir);
  return OK;
}

/**
 * nsp_delete_macros:
 * @Dir: directory name (absolute path).
 * 
 * Remove all the macros associated to directory @dir from the macro hash table.
 *
 * Return value: %OK or %FAIL
 **/


int nsp_delete_macros(const char *dirname)
{
  int  flen;
  GDir *dir;
  char filename[FSIZE+1];
  int i,flag=-1;
  /* Search if we already know directory dirname **/
  for ( i = 0 ; i < LibDirs->mn -1 ; i++) 
    {
      if (strcmp(dirname,LibDirs->S[i])==0) 
	{
	  flag=i;break;
	}
    }
  if ( flag == -1 ) 
    {
      /* nothing to do */
      return OK;
    }

  /* if flag != -1 : we keep the dir name in LibDirs */
  dir =  g_dir_open(dirname,0,NULL);
  if ( dir == NULL) 
    {
      Scierror("Error: Cannot open directory %s\n",dirname);
      return FAIL;
    }
  strcpy(filename,dirname);
  flen=strlen(filename);
  while (1) 
    {
      const gchar *fname=  g_dir_read_name(dir);
      if (fname == NULL) break;
      filename[flen]='/'; 
      filename[flen+1]='\0'; 
      strcat(filename,fname);
      if (g_file_test (filename, G_FILE_TEST_IS_DIR))
	{
	  nsp_delete_macros(filename);
	}
      else 
	{
	  if ( strlen(fname) >= 4 && strncmp(".bin",fname + strlen(fname)-4,4)==0)
	    {
	      char name[NAME_MAXL];
	      strcpy(name,fname);
	      name[strlen(fname)-4]='\0';
	      nsp_remove_macro_or_func(name,0);
	    }
	}
    }
  g_dir_close (dir);
  return OK;
}

/**
 * nsp_find_macro:
 * @str: 
 * 
 * tries to find a macros named @str in the macros table. 
 * If found the macro code is preloaded in a cache and the 
 * macros is returned.
 * 
 * Return value: %NULLOBJ or an %NspObject filled with the macro code.
 **/

NspObject *nsp_find_macro(char *str)
{
  int type;
  NspObject *Obj;
  Obj = nsp_find_macro_or_func(str,&type);
  if ( Obj != NULL && type == 1 ) return NULL;
  return Obj;
}


/**
 * nsp_init_macro_table:
 * @void: 
 * 
 * initialize macro table.
 **/

void nsp_init_macro_table(void)
{
  static int firstentry = 0;
  if ( firstentry != 0 ) return;
  if ((LibDirs =nsp_smatrix_create("libs",1,1,(char *)0,0))== NULLSMAT) 
    {
      Sciprintf("Fatal Error:\tCan't create table for Scilab libraries\n");
      exit(1);
    }
  firstentry = 1;
}



/* 
 * Hash Table for storing scilab function informations 
 * The data associated to a function is the couple (Int,Num) 
 *     where Int is an interface number and Num the id of 
 *     the function in interface number Int.
 * 
 *     InitFunctionTable() : initialize Hash Table storing 
 *                  initial set of function.
 *     int nsp_enter_function(str,Int,Num) : add new function in hash table.
 *                  or change data if function str was already in the table
 *     void nsp_delete_function(str) : delete entry from its name. 
 *     void DeleteFunctionS(Int) : delete functions from interface Int
 *     FindFunction(str,Int,Num) : find entry from its name.
 *     int FindFunctionB(key,Int, Num) : find entry from (Int,Num) 
 *                          by traversal of the whole table.
 * 
 * MAXTAB must be set to 2*(the number of primitives + macros )
 */


/*
 * MAXTAB : maximum number of entries in the htable 
 * in fact  myhcreate use a prime > MAXTAB
 * WARNING : MAXTAB must be chosen > 2* the number of 
 * expected entries for good efficiency of the hash code 
 */

#define MAXTAB 8192

/**
 * nsp_enter_function:
 * @str: name to be searched 
 * @Int: interface number 
 * @Num: function number in interface 
 * 
 * Enter function in Hash Table or change data if function 
 * was already in the table
 * 
 * Return value: %OK or %FAIL.
 **/

int  nsp_enter_function(const char *str, int Int, int Num)
{
  NspFunction *loc;
  if ((loc = function_create(str,str,Int,Num,0,NULL))== NULL)
    return FAIL;
  return  nsp_hash_enter_multiple(nsp_functions_table,NSP_OBJECT(loc));
}

/**
 * nsp_delete_function:
 * @str: 
 * 
 * remove function name @str from function table.
 **/

void nsp_delete_function(const char *str)
{
  nsp_remove_macro_or_func(str,1);
}

/**
 * nsp_delete_interface_functions:
 * @interface: dynamic interface number.
 * 
 * deletes entries associated to interface number @Int by walking through 
 * the whole hash table. 
 * 
 **/

void nsp_delete_interface_functions(int interface)
{
  NspObject *Obj;
  int i=0;
  int iface = interface;
  while (1) 
    {
      int rep = nsp_hash_get_next_object(nsp_functions_table,&i,&Obj);
      if ( Obj != NULLOBJ )
	{ 
	  if ( IsFunction(Obj)) 
	    {
	      /* function */
	      if ( iface == ((NspFunction *) Obj)->iface ) 
		{
		  /* Sciprintf("removing %s in interface %d\n",nsp_object_get_name(Obj),interface); */
		  nsp_hash_remove(nsp_functions_table,nsp_object_get_name(Obj));
		}
	    }
	  else if (IsNspPList(Obj))
	    {
	      /* macro */
	    }
	  else if ( IsList(Obj) )
	    {
	      /* multiple */
	      NspList *L = (NspList *) Obj;
	      NspObject *Elt;
	      int l = nsp_list_length(L),i=1;
	      while (1)
		{
		  if ( i > l ) break;
		  Elt = nsp_list_get_element(L,i);
		  if ( Elt != NULL &&  IsFunction(Elt) ) 
		    {
		      if ( iface == ((NspFunction *) Elt)->iface ) 
			{
			  nsp_list_delete_elt(L,i);
			  l--;i--;
			}
		    }
		  i++;
		}
	      if ( nsp_list_length(L) == 0 )
		{
		  nsp_hash_remove(nsp_functions_table,nsp_object_get_name(Obj));
		}
	    }
	}
      if ( rep == FAIL) break;
    }
}



/**
 * nsp_find_function:
 * @str: 
 * @Int: 
 * @Num: 
 * 
 * searches function  named @str in function table. 
 * In case of success %OK is returned and the function id is returned 
 * in the pair @Int, @Num.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_find_function(const char *str, int *Int, int *Num)
{
  NspObject *Obj;
  int type;
  Obj = nsp_find_macro_or_func(str,&type);
  if ( Obj != NULL &&  type == 1) 
    {
      *Int = ((NspFunction *)Obj)->iface;
      *Num = ((NspFunction *)Obj)->pos;
      return OK;
    }
  return FAIL;
}

/**
 * nsp_find_function_by_id:
 * @key: 
 * @Int: 
 * @Num: 
 * 
 * find a function given @Int and @Num
 * 
 * Returns: %OK or %FAIL 
 **/

int nsp_find_function_by_id(char *key, int Int, int Num)
{
  NspObject *Obj;
  int i=0;
  while (1) 
    {
      int rep = nsp_hash_get_next_object(nsp_functions_table,&i,&Obj);
      if ( Obj != NULLOBJ )
	{ 
	  if ( IsFunction(Obj)) 
	    {
	      /* function */
	      if (Int == ((NspFunction *)Obj)->iface && Num == ((NspFunction *)Obj)->pos) 
		{
		  strncpy(key,nsp_object_get_name(Obj),NAME_MAXL);
		  return 1;
		}
	    }
	  else if (IsNspPList(Obj))
	    {
	      /* macro */
	    }
	  else if ( IsList(Obj) )
	    {
	      /* multiple definitions */
	      NspList *L = (NspList *) Obj;
	      NspObject *Elt;
	      int l = nsp_list_length(L),i=1;
	      while (1)
		{
		  if ( i > l ) break;
		  Elt = nsp_list_get_element(L,i);
		  if ( Elt != NULL &&  IsFunction(Elt) ) 
		    {
		      if (Int == ((NspFunction *) Elt)->iface && Num == ((NspFunction *) Elt)->pos) 
			{
			  strncpy(key,nsp_object_get_name(Obj),NAME_MAXL);
			  return 1;
			}
		    }
		  i++;
		}
	    }
	}
      if ( rep == FAIL) break;
    }
  return 0;
}

  
/**
 * nsp_init_function_table:
 * @void: 
 * 
 * Initialize the function table.
 *
 **/

void nsp_init_function_table(void)
{
  int i=0,k=0;
  if ( nsp_functions_table != NULLHASH) 
    {
      nsp_hash_destroy(nsp_functions_table);
      nsp_functions_table=NULLHASH;
    }

  if (( nsp_functions_table= nsp_hash_create("functions",MAXTAB)) == NULLHASH) 
    {
      printf("Fatal Error: Can't create table for scilab functions (not enough memory)\n");
      exit(1);
    }
  while (1) 
    {
      /* interfaces */
      interface_info *info = Interfaces[i].info;
      if ( info == NULL) break;
      k=0;
      while (1) 
	{
	  /* function in the interface */
	  char *fname;
	  function *f;
	  (*info)(k,&fname,&f);
	  if ( fname == NULL) break;
	  if ( nsp_enter_function(fname,i,k) == FAIL)
	    {
	      printf("Fatal Error : Table for nsp functions is too small \n");
	      exit(1);
	    }	  
	  k++;
	}
      i++;
    }
}





/**
 * nsp_find_macro_or_func:
 * @str: a string 
 * @type: an integer pointer 
 * 
 * searches the function table for a function or a macro named 
 * @str. If multiple definitions exists for a function only 
 * the first one is returned. @type is set to 1 for functions 
 * and to zero for macros.
 * 
 * Returns: a #NspObject or %NULLOBJ
 **/

NspObject *nsp_find_macro_or_func(const char *str,int *type)
{
  NspObject *Ob;
  NspPList *M;
  int found=FALSE;
  NspFile *F;
  char Name[FSIZE+1];
  if ( nsp_hash_find(nsp_functions_table,str,&Ob) == FAIL) 
    {
      return NULLOBJ;
    }
  if ( IsList(Ob) ) 
    {
      /* multiple definitions we return the first one */
      Ob = nsp_list_get_element((NspList *) Ob,1);
      if ( Ob == NULLOBJ ) return NULLOBJ;
    }
  if ( IsFunction(Ob) )
    {
      *type=1;
      return Ob;
    }
  *type =0;
  M= (NspPList *) Ob;
  if ( M->D != NULL )
    {
      /* Sciprintf("Macro %s found in the cache\n",str); */
      return Ob;
    }
  if ( M->dir < 0 ) 
    {
      /* Sciprintf("Macro %s found but dir is wrong\n",str); */
      return NULLOBJ;
    }
  sprintf(Name,"%s/%s.bin",LibDirs->S[M->dir],str);
  if (( F =nsp_file_open_xdr_r(Name)) == NULLSCIFILE) return NULLOBJ;
  /* bin files are supposed to contain only one object */
  while (1) 
    {
      NspObject *Ob1;
      if ((Ob1=nsp_object_xdr_load(F->obj->xdrs))== NULLOBJ ) break;
      if ( strcmp(nsp_object_get_name(Ob1),str)== 0)
	{
	  found = TRUE;
	  /* switch values */
	  M->D = ((NspPList *) Ob1)->D;
	  M->file_name = ((NspPList *) Ob1)->file_name;
	  ((NspPList *) Ob1)->D = NULL;
	  ((NspPList *) Ob1)->file_name= NULL;
	  /* we need to destroy Ob1 */
	  nsp_object_destroy(&Ob1);
	  /* Sciprintf("Macro %s found in %s/%s.bin\n",str,LibDirs->S[M->dir],str); */
	  M->cpu = 0;
	  M->trace = FALSE;
	  M->counter = 0;
	}
    }
  if (nsp_file_close_xdr_r(F) == FAIL) 
    {
      nsp_file_destroy(F);
      return NULLOBJ;
    }
  nsp_file_destroy(F);
  if ( found == TRUE ) 
    return Ob;
  else 
    {
      Sciprintf("Macro %s NOT found in %s/%s.bin !\n",str,LibDirs->S[M->dir],str);
    }
  return NULLOBJ;
}

/* XXX: should be improved not to insert twice the same def 
 *
 */

static int nsp_hash_enter_multiple(NspHash *H, NspObject *Obj) 
{
  NspObject *O1;
  const char *name= nsp_object_get_name(Obj);
  if ( nsp_hash_find(nsp_functions_table,name,&O1) == FAIL)
    {
      return nsp_hash_enter(nsp_functions_table,NSP_OBJECT(Obj));
    }
#ifdef DEBUG   
  Sciprintf("Already present in the table  %s\n",name);
#endif 
  if ( IsList(O1) )
    {
      /* just add the new definition */
#ifdef DEBUG   
      Sciprintf("\t add a new definition to a list\n"); 
#endif 
      if ( nsp_list_insert((NspList *) O1,Obj,0) == FAIL) return FAIL;
    }
  else
    {
      /* create a list and insert old and new definition */
      NspList* L=nsp_list_create(name);
#ifdef DEBUG   
      Sciprintf("\t list create\n");
#endif 
      if ( L == NULL) return FAIL;
      if ((O1 = nsp_object_copy_with_name(O1)) == NULL) return FAIL;
      if ( nsp_list_insert(L,O1,0) == FAIL) return FAIL;
      if ( nsp_list_insert(L,Obj,0) == FAIL) return FAIL;
#ifdef DEBUG   
      Sciprintf("\t add a new %s previous was %s\n", 
		IsFunction(Obj) ? "a function" : " a macro",
		IsFunction(O1) ? "a function" : " a macro");
#endif 
      return nsp_hash_enter(H,NSP_OBJECT(L));
    }
  return OK;
}


/**
 * nsp_remove_macro_or_func:
 * @name: 
 * @type: 
 * 
 * 
 **/

static void nsp_remove_macro_or_func(const char *name,int type)
{
  NspObject *O1;
  if ( nsp_hash_find(nsp_functions_table,name,&O1) == FAIL)
    return;
  if ( IsFunction(O1)) 
    {
      /* function */
      if ( type == 1 ) 
	nsp_hash_remove(nsp_functions_table,name);
    }
  else if (IsNspPList(O1))
    {
      /* macro */
      if ( type == 0) 
	nsp_hash_remove(nsp_functions_table,name);
    }
  else if ( IsList(O1) )
    {
      NspObject *Ob;
      Ob = nsp_list_get_element((NspList *) O1,1);
      if ( IsFunction(Ob) ) 
	{
	  if ( type == 1) 
	    nsp_list_remove_first((NspList *) O1);
	}
      else
	{
	  if ( type == 0 )
	    nsp_list_remove_first((NspList *) O1);
	}
      if ( nsp_list_length((NspList *) O1) == 0 )
	{
	  nsp_hash_remove(nsp_functions_table,name);
	}
    }
}

/*
 */

NspHash *nsp_collect_cpu()
{
  NspHash *H;
  int i=0;
  if(( H = nsp_hash_create(NVOID,1000)) == NULLHASH) return NULLHASH;
  while (1) 
    {
      NspObject *Obj;
      NspPList *P=NULL;
      int rep = nsp_hash_get_next_object(nsp_functions_table,&i,&Obj);
      if ( Obj != NULLOBJ && IsList(Obj) ) 
	{
	  /* multiple definitions we use the first one */
	  Obj = nsp_list_get_element((NspList *) Obj,1);
	}
      if ( Obj != NULLOBJ )
	{ 
	  if ( IsFunction(Obj)) 
	    {
	      /* function */
	      P= NULL;
	    }
	  else if (IsNspPList(Obj))
	    {
	      /* macro */
	      P = (NspPList *) Obj;
	    }
	  if ( P != NULL && P->D != NULL )
	    {
	      const char *fname = ((PList) ((PList) P->D->next->O)->next->next->O)->next->O;
	      NspMatrix *M;
	      if (( M = nsp_matrix_create(fname,'r',1,2) ) == NULLMAT ) return NULLHASH;
	      M->R[0] = P->cpu;
	      M->R[1] = P->counter;
	      if (nsp_hash_enter(H,(NspObject *) M) == FAIL) return NULLHASH;
	    }
	}
      if ( rep == FAIL) break;
    }
  return H;
}

void nsp_init_cpu()
{
  int i=0;
  while (1) 
    {
      NspObject *Obj;
      NspPList *P=NULL;
      int rep = nsp_hash_get_next_object(nsp_functions_table,&i,&Obj);
      if ( Obj != NULLOBJ && IsList(Obj) ) 
	{
	  /* multiple definitions we use the first one */
	  Obj = nsp_list_get_element((NspList *) Obj,1);
	}
      if ( Obj != NULLOBJ )
	{ 
	  if ( IsFunction(Obj)) 
	    {
	      P=NULL;
	      /* function */
	    }
	  else if (IsNspPList(Obj))
	    {
	      /* macro */
	      P = (NspPList *) Obj;
	    }
	  if ( P != NULL &&  P->D != NULL )
	    {
	      P->cpu = 0;
	    }
	}
      if ( rep == FAIL) break;
    }
}
