/* Nsp
 * Copyright (C) 1998-2006 Jean-Philippe Chancelier Enpc/Cermics
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
#include "nsp/plistc.h"
#include "../functions/FunTab.h"
#include "../interp/LibsTab.h"

#include "frame.h"

/* inhibit the search fro symbols in calling frames */

extern int frames_search_inhibit;

/* use NspList or NspHash to implement frames */

#define FRAME_AS_LIST

/*
 * Used to store the calling frames for function invocation 
 * It is a list of list each list is called a frame here 
 */

NspList   *Datas = NULLLIST; /* contains the list of frames */
NspObject *Reserved= NULLOBJ;/* used to fill stack with non empty object */
NspMatrix *Dollar = NULLMAT; /* Direct access to $ **/
NspObject *Null = NULLOBJ;   /* Direct access to %null **/
NspFrame  *GlobalFrame = NULLFRAME; /* Direct access to GlobalFrame **/
NspFrame  *ConstantFrame = NULLFRAME; /* Direct access to constants **/


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
  NspFrame *frame; 
  if ((Datas =nsp_list_create("datas")) ==NULLLIST ) return(FAIL);
  if ((GlobalFrame=nsp_frame_create("global",NULL))== NULLFRAME) return FAIL;
  /* The GlobalFrame is not stored in Datas */
  if (( frame=nsp_frame_create("constants",NULL))== NULLFRAME) return FAIL;
  ConstantFrame = frame; /* Direct access to constants **/
  /* store the new frame in Datas */
  if ( nsp_list_store(Datas,(NspObject *)frame,1) == FAIL) return(FAIL);
  /* Create first Object in the initial frame **/
  if ((O= (NspObject *) nsp_smatrix_create_from_array("%argv",argc,(const char **) argv))
      == NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  d=0;d=1/d;
  if ((O=nsp_create_object_from_double("%inf",d))==NULLOBJ)return FAIL;
  nsp_frame_replace_object(O,-1);
  d=0;d=d/d;
  if ((O=nsp_create_object_from_double("%nan",d))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  if ((O=nsp_create_object_from_double("%pi",M_PI))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  if ((O=nsp_create_object_from_double("%e",M_E))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  if ((O =nsp_create_object_from_double("$", -1.0 ))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  Dollar = (NspMatrix *) O;
  if ((O =nsp_create_true_object("%t"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  if ((O =nsp_create_false_object("%f"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  if ((O =nsp_create_object_from_double("%eps",nsp_dlamch("e")))== NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  /* used for list element deletion **/
  if ((O =nsp_create_empty_matrix_object("%null"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  Null = O;
  /* %i **/
  if ((O =nsp_complexi_object_("%i"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  /* shared lib suffix */
  if ((O =(NspObject *) nsp_smatrix_create("%shext",1,1,SHREXT_NAME,1) ) == NULLOBJ )
    return FAIL;
  nsp_frame_replace_object(O,-1);
  /* reserved */
  if ((Reserved =nsp_create_empty_matrix_object("@keep@"))==NULLOBJ) return FAIL;
  /* flag to know that we are using nsp !! */
  if ((O =nsp_create_true_object("%nsp"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  /* flag to detect if we have umfpack */
#ifdef WITH_UMFPACK
  if ((O =nsp_create_true_object("%umfpack"))==NULLOBJ) return FAIL;
#else 
  if ((O =nsp_create_false_object("%umfpack"))==NULLOBJ) return FAIL;
#endif 
  nsp_frame_replace_object(O,-1);
  /* flag to detect if we have cholmod */
#ifdef WITH_CHOLMOD
  if ((O =nsp_create_true_object("%cholmod"))==NULLOBJ) return FAIL;
#else 
  if ((O =nsp_create_false_object("%cholmod"))==NULLOBJ) return FAIL;
#endif 
  nsp_frame_replace_object(O,-1);
#ifdef WIN32 
  if ((O =nsp_create_true_object("%win32"))==NULLOBJ) return FAIL;
#else 
  if ((O =nsp_create_false_object("%win32"))==NULLOBJ) return FAIL;
#endif
  nsp_frame_replace_object(O,-1);
  /* types */ 
  nsp_frame_replace_object((NspObject *)nsp_types_hash_table,-1); 
  /* gtk */
  nsp_frame_replace_object((NspObject *)nsp_gtk_hash_table,-1); 
  nsp_frame_replace_object((NspObject *)nsp_gdk_hash_table,-1); 
  nsp_frame_replace_object((NspObject *)nsp_atk_hash_table,-1); 
  nsp_frame_replace_object((NspObject *)nsp_pango_hash_table,-1); 
  /* ast */
  if ((O = (NspObject *) nsp_ast_hash_create()) ==NULLOBJ) return FAIL;
  nsp_frame_replace_object(O,-1);
  
  /* the top level frame */
  if (( frame=nsp_frame_create("top",NULL))== NULLFRAME) return FAIL;
  if ( nsp_list_store(Datas,(NspObject *)frame,1) == FAIL) return(FAIL);
  return(OK);
}


/**
 *nsp_new_frame:
 * 
 * Allocate a new frame for function invocation. The frames are 
 * stored in a linked list. If this linked list is not initialized then 
 * the call to nsp_new_frame() will fail.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_new_frame(void)
{
  NspFrame *frame;
  if ( Datas == NULLLIST ) return FAIL;
  if (( frame=nsp_frame_create("datas",NULL))== NULLFRAME) return FAIL;
  if (nsp_list_store(Datas,NSP_OBJECT(frame),1) == FAIL) return FAIL;
  return OK;
}

/**
 * nsp_new_frame_with_local_vars:
 * @table: 
 * 
 * Allocate a new frame for function invocation. The frames are 
 * stored in a linked list. If this linked list is not initialized then 
 * the call to nsp_new_frame() will fail. The frame can contain an 
 * a table used for local variables and a table for dynamically 
 * added variables (a sorted list or a hash table, default value is a sorted list).
 * 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_new_frame_with_local_vars(NspCells *table)
{
  NspFrame *frame;
  if ( Datas == NULLLIST ) return FAIL;
  if ((frame=nsp_frame_create("datas",table))== NULLFRAME) return FAIL;
  if (nsp_list_store(Datas,NSP_OBJECT(frame),1) == FAIL) return FAIL;
  return OK;
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
  if ( Datas == NULLLIST )   return ;
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
 *nsp_frame_info_obsolete:
 * @void: 
 * 
 * Display information on the first frame 
 **/

void nsp_frame_info_obsolete(void)
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
 *nsp_frame_print_obsolete:
 * @void: 
 * 
 * Display first frame content.
 * 
 **/

void nsp_frame_print_obsolete(void)
{
  if ( Datas == NULLLIST ) 
    Sciprintf("Empty Datas\n");
  else 
    nsp_list_print((NspList *)Datas->first->O,0,NULL,0);
}

/**
 *nsp_frame_replace_object:
 * @A: object to be inserted
 * @local_id: an integer 
 * 
 * Inserts #NspObject @A in the current evaluation frame.  
 * If an object with the same name as @A exists it is replaced by @A in 
 * the current frame and old object is destroyed. If @local_id is not equal 
 * to -1 then @A is assumed to be a local variable and it is thus inserted 
 * in the symbol table of the current frame directly the id of the object being 
 * given by @local_id.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_frame_replace_object( NspObject *A,int local_id)
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
       * if ( nsp_find_function(nsp_object_get_name(A),&Int,&Num) == OK 
       *	   || nsp_find_macro(nsp_object_get_name(A)) != NULLOBJ)
       *	{
       *	  Sciprintf("Warning: variable %s will hide a primitive name\n",
       *		    nsp_object_get_name(A));
       * }
       */
      if ( local_id == -1 ) 
	{
	  /* insert in current frame */
	  return nsp_eframe_replace_object((NspFrame *) Datas->first->O,A);
	}
      else 
	{
	  /* insert as a local variable */
	  NspObject *O1;
	  local_id = VAR_ID(local_id);
	  O1 = ((NspFrame *) Datas->first->O)->table->objs[local_id];
	  if ( O1 != NULL )  nsp_object_destroy(&O1);
	  ((NspFrame *) Datas->first->O)->table->objs[local_id]=A;
	  return OK;
	}
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
  if ( nsp_find_function(nsp_object_get_name(A),&Int,&Num) == OK 
       || nsp_find_macro(nsp_object_get_name(A)) != NULLOBJ)
    {
      Sciprintf("Warning: variable %s will hide a function name\n",
		nsp_object_get_name(A));
      return(FAIL);
    }
  return nsp_eframe_replace_object((NspFrame *) GlobalFrame,A);
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
  NspObject *Obj;
  Cell *C;
  if ( Datas == NULLLIST ) 
    return NULLOBJ;
  else 
    {
      C= Datas->first;
      while ( C != NULLCELL) 
	{
	  if ((Obj = nsp_eframe_search_object((NspFrame *) C->O,str,TRUE)) != NULLOBJ ) 
	    return Obj;
	  if ( frames_search_inhibit == TRUE )
	    {
	      /* FIXME: to find variables which are in globalframe 
	       * but which are not really global %t %e etc...
	       */
	      return ((NspFrame *) C->O != GlobalFrame ) ? nsp_global_frame_search_object(str) : NULLOBJ;
	    }
	  else 
	    C = C->next ;
	}
      return NULLOBJ ;
    }
} 



/**
 * nsp_frames_search_local_in_calling:
 * @str: 
 * 
 * This function is used when a local variable has no 
 * value and we want to see if the local variable can 
 * be initialized by a variable with same name in calling stack. 
 * for example function f();if y >=0; y=y+1;endfunction 
 * y is considered local because of y=y+1 but can be inherited. 
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_frames_search_local_in_calling(const char *str)
{
  NspObject *Obj;
  Cell *C = Datas->first->next;
  if ( frames_search_inhibit == TRUE ) return NULLOBJ;
  /* we assume here that str was already found in Datas->first but with no value 
   */
  while ( C != NULLCELL) 
    {
      /* subsequent call of nsp_eframe_search_object must not re-enter 
       * in nsp_frames_search_local_in_calling thus tag is set to FALSE
       */
      if ((Obj = nsp_eframe_search_object((NspFrame *) C->O,str,FALSE)) != NULLOBJ ) 
	return Obj;
      C = C->next ;
    }
  return NULLOBJ ;
}

/**
 *nsp_frame_search_object:
 * @str: an object name. 
 * 
 * Search for an object with name @str in the first frame. 
 * If the object is a local variable with no value the 
 * object is also searched in the calling frames. 
 * In that case the returned object should not be modified.
 * 
 * Return value: the #NspObject or #NULLOBJ.
 **/

NspObject *nsp_frame_search_object(nsp_const_string str)
{
  if ( Datas == NULLLIST )   return NULLOBJ;
  return nsp_eframe_search_object((NspFrame *) Datas->first->O,str,TRUE) ;
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
  if ( GlobalFrame == NULL )  return NULLOBJ;
  return nsp_eframe_search_object((NspFrame *)GlobalFrame ,str,TRUE) ;
} 

/**
 * GlobalFrameObjRemove:
 * @str: an object name. 
 * 
 * Remove object with name @str in the global frame
 * XXXX : here it should be good to also remove object from 
 * the local variables or frame if object is a pointer to 
 * a global var to avoid the message 
 * Pointer to a global non existant variable
 * 
 **/

void nsp_global_frame_remove_object(nsp_const_string str)
{
  if ( GlobalFrame == NULL )  return ;
  nsp_eframe_remove_object((NspFrame *)GlobalFrame,str);
}

/**
 * nsp_global_frame_remove_all_objects:
 * 
 * remove all objects from global frame 
 **/

int nsp_global_frame_remove_all_objects(void)
{
  if ( GlobalFrame == NULL ) return OK;
  nsp_frame_destroy((NspFrame *)GlobalFrame);
  if ((GlobalFrame = nsp_frame_create("global",NULL)) == NULLFRAME)
    return FAIL;
  else
    return OK;
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
  if ( Datas == NULLLIST ) return NULLOBJ;
  return nsp_eframe_search_and_remove_object((NspFrame *) Datas->first->O ,str);
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
  if ( Datas == NULLLIST ) return;
  nsp_eframe_remove_object((NspFrame *)Datas->first->O,str);
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
      if ((O= nsp_eframe_search_and_remove_object((NspFrame *) C->O ,str))== NULLOBJ )
	{
	  Scierror("Error: %s object not found\n",str);
	  return FAIL;
	}
      return nsp_eframe_replace_object((NspFrame *)Datas->first->next->O,O);
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
      return nsp_eframe_replace_object((NspFrame *)Datas->first->next->O,O);
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


int nsp_declare_global(char *name, int id) 
{
  NspObject *O;
  NspHobj *O1;
  if ( Datas == NULLLIST ) return FAIL;
  if ( name == NULL) return FAIL;
  /* if current frame is the global frame we have nothing to do */
  if ( Datas->first->O == (NspObject *) GlobalFrame ) return OK;
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
  return nsp_frame_replace_object((NspObject *) O1,id);
}


/**
 * nsp_frame_insert_hash_contents:
 * @H: 
 * 
 * insert copies of object contained in the hashtable @H 
 * in the current frame 
 * 
 * Return value: %OK or %FAIL
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
	  if ( nsp_frame_replace_object(Ob,-1)== FAIL) return FAIL;
	}
    }
  return OK;
}
 


/**
 * nsp_current_frame_to_hash:
 * @void: 
 * 
 * return the current frame contents in a new hash table 
 * 
 * Return value: a new #NspHash or %NULLHASH
 **/

NspHash *nsp_current_frame_to_hash(void)
{
  if ( Datas == NULLLIST ) return NULLHASH;
  return nsp_eframe_to_hash((NspFrame *) Datas->first->O);
}
