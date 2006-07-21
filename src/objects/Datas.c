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
#include "nsp/math.h"
#include "nsp/object.h"
#include "nsp/datas.h"
#include "nsp/shext.h"
#include "../functions/FunTab.h"
#include "../interp/LibsTab.h"

/* inhibit the search fro symbols in calling frames */

extern int frames_search_inhibit;

/* use NspList or NspHash to implement frames */

#define FRAME_AS_LIST

/*
 * Used to store the calling frames for function invocation 
 * It is a list of list each list is called a frame here 
 */
NspList  *Datas = NULLLIST;
NspObject   *Reserved= NULLOBJ;/* used to fill stack with non empty object */
NspMatrix *Dollar = NULLMAT; /* Direct access to $ **/
NspObject *Null = NULLOBJ;    /* Direct access to %null **/
#ifdef FRAME_AS_LIST
NspList  *GlobalFrame = NULLLIST; /* Direct access to GlobalFrame **/
#else 
NspHash  *GlobalFrame = NULLHASH; /* Direct access to GlobalFrame **/
#endif 


/**
 * nsp_init_frames:
 * @argc: length of @argv 
 * @argv: array of characters
 * 
 * Initialize frame data structure which is used for function invocation.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_init_frames(int argc, char **argv)
{
  double d;
  NspObject *O;
#ifdef FRAME_AS_LIST
  NspList *frame = NULLLIST ;
  if ((Datas =nsp_list_create("datas")) ==NULLLIST ) return(FAIL);
  if ((frame =nsp_list_create("global")) == NULLLIST) return(FAIL);
#else 
  NspHash *frame = NULLHASH ;
  if ((Datas =nsp_list_create("datas",NULLSTRING)) ==NULLLIST ) return(FAIL);
  if ((frame =nsp_hash_create("global",20)) == NULLHASH) return(FAIL);
#endif
  if ( nsp_list_store(Datas,(NspObject *)frame,1) == FAIL) return(FAIL);
  /* this first frame is the global one */
  GlobalFrame = frame;
  /* Create first Object in the initial frame **/
  if ((O= (NspObject *) nsp_smatrix_create_from_array("%argv",argc,(const char **) argv))
      == NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  d=0;d=1/d;
  if ((O=nsp_create_object_from_double("%inf",d))==NULLOBJ)return FAIL;
  nsp_frame_replace_object(O);
  d=0;d=d/d;
  if ((O=nsp_create_object_from_double("%nan",d))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  if ((O=nsp_create_object_from_double("%pi",M_PI))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  if ((O=nsp_create_object_from_double("%e",M_E))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  if ((O =nsp_create_object_from_double("$", -1.0 ))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  Dollar = (NspMatrix *) O;
  if ((O =nsp_create_true_object("%t"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  if ((O =nsp_create_false_object("%f"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  if ((O =nsp_create_object_from_double("%eps",nsp_dlamch("e")))== NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  /* used for list element deletion **/
  if ((O =nsp_create_empty_matrix_object("%null"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  Null = O;
  /* %i **/
  if ((O =nsp_complexi_object_("%i"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  /* shared lib suffix */
  if ((O =(NspObject *) nsp_smatrix_create("%shext",1,1,SHREXT_NAME,1) ) == NULLOBJ )
    return FAIL;
  nsp_frame_replace_object(O);
  /* reserved */
  if ((Reserved =nsp_create_empty_matrix_object("@keep@"))==NULLOBJ) return FAIL;
  /* flag to know that we are using nsp !! */
  if ((O =nsp_create_true_object("%nsp"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O);
  /* types */ 
  nsp_frame_replace_object((NspObject *)nsp_types_hash_table); 
  /* gtk */
  nsp_frame_replace_object((NspObject *)nsp_gtk_hash_table); 
  nsp_frame_replace_object((NspObject *)nsp_gdk_hash_table); 
  nsp_frame_replace_object((NspObject *)nsp_atk_hash_table); 
  nsp_frame_replace_object((NspObject *)nsp_pango_hash_table); 
  return(OK);
}


/**
 *nsp_new_frame:
 * 
 * Allocate a new frame for function invocation. The frames are 
 * stored in a linked list. If this linked list is not initialized then 
 * the call to nsp_new_frame() will perform initialization. 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_new_frame(void)
{
  NspObject *frame;
  if ( Datas == NULLLIST ) return FAIL;
#ifdef FRAME_AS_LIST
  if ((frame =(NspObject *) nsp_list_create("datas")) == NULLOBJ) return(FAIL);
#else 
  if(( frame =(NspObject *) nsp_hash_create("datas",11)) == NULLOBJ) return FAIL;
#endif
  if (nsp_list_store(Datas,frame,1) == FAIL) return(FAIL);
  return(OK);
}

/**
 *nsp_frame_delete:
 * @void: 
 * 
 * Delete the first frame 
 * This routines could check that the last frame is preserved 
 * and never destroyed. 
 **/

void nsp_frame_delete(void)
{
  if ( Datas == NULLLIST ) 
    return ;
  nsp_list_delete_elt(Datas,1) ;
}

/**
 *nsp_frames_info:
 * @void: 
 * 
 * Display information on allocated frames. 
 **/

void nsp_frames_info(void)
{
  if ( Datas == NULLLIST ) 
    Sciprintf("Empty Datas\n");
  else 
    nsp_list_info(Datas,0,NULL,0) ;
}

/**
 *nsp_frame_info:
 * @void: 
 * 
 * Display information on the first frame 
 **/

void nsp_frame_info(void)
{
  if ( Datas == NULLLIST ) 
    Sciprintf("Empty Datas\n");
  else 
    nsp_list_info((NspList *) Datas->first->O,0,NULL,0) ;
}

/**
 *nsp_frames_print:
 * @void: 
 * 
 * Display frames content
 * 
 **/

void nsp_frames_print(void)
{
  if ( Datas == NULLLIST ) 
    Sciprintf("Empty Datas\n");
  else 
    nsp_list_print(Datas,0,NULL,0);
}

/**
 *nsp_frame_print:
 * @void: 
 * 
 * Display first frame content.
 * 
 **/

void nsp_frame_print(void)
{
  if ( Datas == NULLLIST ) 
    Sciprintf("Empty Datas\n");
  else 
    nsp_list_print((NspList *)Datas->first->O,0,NULL,0);
}


/**
 *nsp_frame_replace_object:
 * @A: object to be inserted
 * 
 * Inserts #NspObject @A in the first frame.  
 * If an object with the same name as @A exists 
 * It is replaced by @A. 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_frame_replace_object( NspObject *A)
{
  if (  A == NULLOBJ ) return(OK);
  if (  Datas == NULLLIST ) 
    {
      Scierror("Error: Can't insert obj in empty Data frame\n");
      return(FAIL);
    }
  else 
    {
      /* 
       * int Int,Num;
       * if ( FindFunction(nsp_object_get_name(A),&Int,&Num) == OK 
       *	   || FindMacro(nsp_object_get_name(A)) != NULLOBJ)
       *	{
       *	  Sciprintf("Warning: variable %s will hide a primitive name\n",
       *		    nsp_object_get_name(A));
       * }
       */
#ifdef FRAME_AS_LIST
      return nsp_sorted_list_insert((NspList *) Datas->first->O, A);
#else 
      return nsp_hash_enter((NspHash *) Datas->first->O,A);
#endif
    }
  return(OK);
} 



/**
 *nsp_global_frame_replace_object:
 * @A:  object to be inserted
 * 
 * Insert Object A in the global frame 
 * If an object with the same name as A exists 
 * It is replaced by A
 * 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_global_frame_replace_object(NspObject *A)
{
  int Int,Num;
  if (  A == NULLOBJ ) return(OK);
  if (  GlobalFrame == NULL ) return(FAIL);
  if ( FindFunction(nsp_object_get_name(A),&Int,&Num) == OK 
       || FindMacro(nsp_object_get_name(A)) != NULLOBJ)
    {
      Sciprintf("Warning: variable %s will hide a function name\n",
		nsp_object_get_name(A));
      return(FAIL);
    }
#ifdef FRAME_AS_LIST
  return nsp_sorted_list_insert(GlobalFrame,A);
#else 
  return nsp_hash_enter(GlobalFrame,A);
#endif
} 

/**
 *nsp_frames_search_object:
 * @str: an object name. 
 *
 * Search for an object with name @str in all the frames 
 * 
 * Return value:  the #NspObject or #NULLOBJ.
 **/

NspObject *nsp_frames_search_object(const char *str)
{
  NspObject *O;
  Cell *C;
  if ( Datas == NULLLIST ) 
    return NULLOBJ;
  else 
    {
      C= Datas->first;
      while ( C != NULLCELL) 
	{
#ifdef FRAME_AS_LIST
	  if ( ( O =nsp_sorted_list_search((NspList *) C->O,str)) != NULLOBJ ) 
	    return O;
#else 
	  if ( nsp_hash_find((NspHash *) C->O,str,&O)== OK) 
	    return O;
#endif
	  if ( frames_search_inhibit == TRUE )
	    {
	      /* FIXME: to find variables which are in globalframe 
	       * but which are not really global %t %e etc...
	       */
	      return ((NspList *) C->O != GlobalFrame ) ? nsp_global_frame_search_object(str) : NULLOBJ;
	    }
	  else 
	    C = C->next ;
	}
      return NULLOBJ ;
    }
} 

/**
 *nsp_frame_search_object:
 * @str: an object name. 
 * 
 * Search for an object with name @str in the first frame
 * 
 * Return value: the #NspObject or #NULLOBJ.
 **/

NspObject *nsp_frame_search_object(nsp_const_string str)
{
#ifndef FRAME_AS_LIST
  NspObject *O;
#endif
  Cell *C;
  if ( Datas == NULLLIST ) 
    return NULLOBJ;
  else 
    {
      C= Datas->first;
#ifdef FRAME_AS_LIST
      return (nsp_sorted_list_search((NspList *) C->O,str) );
#else 
      return ( nsp_hash_find((NspHash *) C->O,str,&O)== OK) ? O : NULLOBJ;
#endif
    }
} 


/**
 *nsp_global_frame_search_object:
 * @str: an object name. 
 * 
 * Search for an object with name @str in the global frame
 * 
 * Return value:  the #NspObject or #NULLOBJ.
 **/

NspObject *nsp_global_frame_search_object(nsp_const_string str)
{
#ifndef FRAME_AS_LIST
  NspObject *Ob;
#endif
  if ( GlobalFrame == NULL )  return NULLOBJ;
#ifdef FRAME_AS_LIST
  return (nsp_sorted_list_search(GlobalFrame,str) );
#else 
  if ( nsp_hash_find(GlobalFrame,str,&Ob)== OK) 
    return Ob;
  else 
    return NULLOBJ;
#endif
} 

/**
 * GlobalFrameObjRemove:
 * @str: an object name. 
 * 
 * Remove object with name @str in the global frame
 * 
 **/

void nsp_global_frame_remove_object(nsp_const_string str)
{
  NspObject *Ob=NULLOBJ;
  if ( GlobalFrame == NULL )  return ;
#ifdef FRAME_AS_LIST
  Ob = nsp_sorted_list_search_and_remove(GlobalFrame,str);
  nsp_object_destroy(&Ob);
#else 
  /* FIXME: we need here a unique option nsp_hash_find_and_remove */
  if ( nsp_hash_find_and_copy(GlobalFrame,str,&Ob)== OK)
    {
      nsp_hash_remove(GlobalFrame,str);
      nsp_object_destroy(&Ob);
    }
#endif
} 



/**
 *nsp_frame_search_and_remove_object:
 * @str:  an object name. 
 * 
 * Search for an object  with name @str in the first frame and returns the object if found.
 * If the object is found it is also removed from the frame.
 * 
 * Return value: the #NspObject or #NULLOBJ.
 **/

NspObject *nsp_frame_search_and_remove_object(nsp_const_string str)
{
#ifndef FRAME_AS_LIST
  NspObject *Ob;
#endif
  Cell *C;
  if ( Datas == NULLLIST ) 
    return NULLOBJ;
  else 
    {
      C= Datas->first;
#ifdef FRAME_AS_LIST
      return (nsp_sorted_list_search_and_remove((NspList *) C->O,str) );
#else 
      /* FIXME: we need here a unique option nsp_hash_find_and_remove */
      if ( nsp_hash_find_and_copy((NspHash *) C->O,str,&Ob)== OK)
	{
	  nsp_hash_remove((NspHash *) C->O,str);
	  return Ob;
	}
      return NULLOBJ;
#endif
    }
} 


/**
 *nsp_frame_remove_object:
 * @str:  an object name. 
 * 
 * Search for an object  with name @str in the first frame and 
 * remove the object 
 **/

void nsp_frame_remove_object(nsp_const_string str)
{
  Cell *C;
  if ( Datas == NULLLIST ) return;
  else 
    {
#ifdef FRAME_AS_LIST
      NspObject *O;
#endif
      C= Datas->first;
#ifdef FRAME_AS_LIST
      O=nsp_sorted_list_search_and_remove((NspList *) C->O,str);
      nsp_object_destroy(&O);
#else 
      nsp_hash_remove((NspHash *) C->O,str);
#endif
    }
} 

/**
 *nsp_frame_search_and_move_up_object:
 * @str:  an object name. 
 * 
 * Search for an object with name @str in the first frame and  if found 
 * move it to the next frame. 
 *
 * Return value:  %OK or %FAIL.
 **/

int nsp_frame_search_and_move_up_object(nsp_const_string str)
{
  NspObject *O;
  Cell *C;
  if ( Datas == NULLLIST ) 
    return FAIL;
  else 
    {
      C= Datas->first;
      if ( C->next == NULLCELL) 
	{
	  Scierror("Error: cannot move %s in an upper frame, we are on top\n",str);
	  return FAIL; 
	}
#ifdef FRAME_AS_LIST
      if (( O=nsp_sorted_list_search_and_remove((NspList *) C->O,str)) == NULLOBJ )
	{
	  Scierror("Error: %s object not found\n",str);
	  return FAIL;
	}
      return nsp_sorted_list_insert((NspList *) Datas->first->next->O, O);
#else 
      if ( nsp_hash_find_and_copy((NspHash *) C->O,str,&O)== FAIL)
	{
	  Scierror("Error: %s object not found\n",str);
	  return FAIL;
	}
      nsp_hash_remove((NspHash *) C->O,str);
      return nsp_hash_enter((NspHash *) Datas->first->next->O,O);
#endif 
    }
}

/**
 *nsp_frame_move_up_object:
 * @O: object to be moved. 
 * 
 * Insert object @O in the frame next to the first one.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_frame_move_up_object(NspObject *O)
{
  Cell *C;
  if (  O == NULLOBJ ) return(OK);
  if ( Datas == NULLLIST ) 
    {
      Scierror("Error: Can't insert object %s in empty Data frame\n",nsp_object_get_name(O));
      return FAIL;
    }
  else 
    {
      C= Datas->first;
      if ( C->next == NULLCELL) 
	{
	  Scierror("Error: cannot move %s in an upper frame, we are on top\n",nsp_object_get_name(O));
	  return FAIL; 
	}
#ifdef FRAME_AS_LIST
      return nsp_sorted_list_insert((NspList *) Datas->first->next->O, O);
#else 
      return nsp_hash_enter((NspHash *) Datas->first->next->O,O);
#endif
    }
}


/** 
 *nsp_declare_global: 
 * @name: a name to be considered as a global variable name.
 * 
 * Declares that variable @name is to be considered as a global variable.
 * If @name is already a global variable then a pointer to that variable 
 * is inserted in the local frame. 
 * If @name is not a global variable: 
 *    If @name exists in the local frame, this value is used 
 *    to create the global variable with the same name 
 *    and the local @name variable is replaced by a pointer to the global
 *     variable 
 *    If @name does not exists then the global variable is initialized 
 *    with an empty scalar matrix. 
 * 
 * FIXME : a type should be added as optional second argument in order to 
 *    create a global var with appropriate type. 
 *
 * Return value:  %OK or %FAIL.
 */


int nsp_declare_global(char *name) 
{
  Cell *C;
  NspObject *O;
  NspHobj *O1;
  if ( Datas == NULLLIST ) return FAIL;
  if ( name == NULL) return FAIL;
  C = Datas->first ;
  /* if current frame is the global frame we have nothing to do */
  if ( C->O == (NspObject *) GlobalFrame ) return OK;
  /* search in the global frame */
  O=nsp_global_frame_search_object(name);
  if ( O == NULLOBJ ) 
    {
      NspObject *O1= NULLOBJ;
      /* FIXME : if O1 is already a pointer to a global variable 
       * we have nothing to do 
       *  NspObject *O1= nsp_frame_search_object(name);
       */ 
      if ( O1 != NULLOBJ) 
	{
	  if ((O =nsp_object_copy(O1))== NULLOBJ) return FAIL;
	}
      else 
	{
	  /* create an empty matrix */
	  if ((O =nsp_create_empty_matrix_object(name))== NULLOBJ) return FAIL;
	}
      if (nsp_object_set_name(O,name) == FAIL) return FAIL;
      /* store it in the global frame */
      if (nsp_global_frame_replace_object(O)  == FAIL ) return FAIL;
    }
  /* we create a pointer to O in the local frame */
  if ((O1= GobjCreate(name,O)) == NULLHOBJ) return FAIL;
  return nsp_frame_replace_object((NspObject *) O1);
}


/**
 * nsp_frame_insert_hash_contents:
 * @H: 
 * 
 * insert copies of object contained in the hashtable @H 
 * in the current frame 
 * 
 * Return value: 
 **/

int nsp_frame_insert_hash_contents(NspHash *H)
{
  int i1;
  for ( i1 =0 ; i1 < H->hsize+1 ; i1++) 
    {
      Hash_Entry *loc = ((Hash_Entry *) H->htable) + i1;
      if ( loc->used) 
	{
	  NspObject *Ob;
	  if ((Ob = nsp_object_copy_with_name(loc->data))== NULLOBJ) return FAIL;
	  if ( nsp_frame_replace_object(Ob)== FAIL) return FAIL;
	}
    }
  return OK;
}
