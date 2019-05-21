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
#include <nsp/type.h>
#include <nsp/hobj.h>
#include <nsp/list.h>
#include <nsp/smatrix.h>
#include <nsp/matrix.h>
#include <nsp/imatrix.h>
#include <nsp/hash.h>
#include <nsp/cells.h>
#include <nsp/interf.h>
#include <nsp/datas.h>
#include <nsp/shext.h>
#include <nsp/plistc.h>
#include <nsp/version.h>
#include <nsp/funtab.h>
#include <nsp/libstab.h>
#include <nsp/frame.h>
#include <nsp/nspthreads.h>
#include <nsp/nspdatas.h>

/* inhibit the search for symbols in calling frames */

extern int frames_search_inhibit;

#ifndef NSP_WITH_MAIN_GTK_THREAD
NspList   *Datas = NULLLIST; /* contains the list of frames */
NspObject *Reserved= NULLOBJ;/* used to fill stack with non empty object */
NspObject *Null = NULLOBJ;   /* Direct access to %null */
NspFrame  *GlobalFrame = NULLFRAME; /* Direct access to GlobalFrame */
NspFrame  *ConstantFrame = NULLFRAME; /* Direct access to constants */
NspFrame  *TopFrame = NULLFRAME; /* Direct access to top */
#endif

static int nsp_constant_frame(nsp_datas *data,int argc, char **argv);
static int nsp_frame_replace_object1(nsp_datas *data, NspObject *A,int local_id);

/**
 * nsp_init_frames:
 * @nspdata: a pointer of type #nspdata
 * @argc: length of @argv
 * @argv: array of characters
 *
 * Initialize frame data structure which is used for function invocation.
 *
 * Return value: %OK or %FAIL.
 **/

int nsp_init_frames(void *user_data,int argc, char **argv)
{
  nsp_datas **data = user_data;
  NspFrame *frame;
  /* A List of frames */
  if (((*data) = calloc(1, sizeof(nsp_datas)))== NULL) return FAIL;
  if (((*data)->L =nsp_list_create("datas")) ==NULLLIST ) return FAIL;
#ifndef NSP_WITH_MAIN_GTK_THREAD
  Datas = (*data)->L;
#endif
  /* The GlobalFrame is not stored in Datas */
  if (((*data)->GlobalFrame=nsp_frame_create("global",NULL))== NULLFRAME) return FAIL;
#ifndef NSP_WITH_MAIN_GTK_THREAD
  GlobalFrame=(*data)->GlobalFrame;
#endif
  /* The constant frame: ConstantFramecan be used for direct access */
  if ( nsp_constant_frame(*data,argc,argv) == FAIL)  return FAIL;
  /* the top level frame */
  if (( frame=nsp_frame_create("top",NULL))== NULLFRAME) return FAIL;
  /* Direct access to toplevel frame */
  (*data)->TopFrame = frame;
#ifndef NSP_WITH_MAIN_GTK_THREAD
  TopFrame = (*data)->TopFrame;
#endif
  if ( nsp_list_store((*data)->L,(NspObject *) (*data)->TopFrame,1) == FAIL) return FAIL;
  return OK;
}

/**
 * nsp_constant_frame:
 * @argc: length of @argv
 * @argv: array of characters
 *
 * Initialize a constant frame data structure with constants
 *
 * Return value: a new #NspFrame or %NULL
 **/

static int nsp_constant_frame(nsp_datas *data,int argc, char **argv)
{
  double d;
  NspObject *O;
  NspFrame *frame;
  /* The constant frame: be used for direct access */
  if (( frame=nsp_frame_create("constants",NULL))== NULLFRAME) return FAIL;
  /* Direct access to constants */
  data->ConstantFrame=frame;
#ifndef NSP_WITH_MAIN_GTK_THREAD
  ConstantFrame = data->ConstantFrame;
#endif
  /* store the new frame in Datas */
  if ( nsp_list_store(data->L,(NspObject *)data->ConstantFrame,1) == FAIL ) return FAIL;
  /* Create first Object in the initial frame */
  if ((O= (NspObject *) nsp_smatrix_create_from_array("%argv",argc,(const char **) argv))
      == NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  d=0;d=1/d;  if ((O=nsp_create_object_from_double("%inf",d))==NULLOBJ)return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  d=0;d=d/d;  if ((O=nsp_create_object_from_double("%nan",d))==NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  if ((O=nsp_create_object_from_double("%pi",M_PI))==NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  if ((O=nsp_create_object_from_double("%e",M_E))==NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  if ((O =nsp_create_true_object("%t"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  if ((O =nsp_create_false_object("%f"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  if ((O =nsp_create_object_from_double("%eps",nsp_dlamch("e")))== NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  /* used for list element deletion */
  if ((O =nsp_create_empty_matrix_object("%null"))==NULLOBJ) return FAIL;
  data->Null = O; nsp_frame_replace_object1(data,O,-1);
#ifndef NSP_WITH_MAIN_GTK_THREAD
  Null = data->Null;
#endif
  /* %i */
  if ((O =nsp_complexi_object_("%i"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  /* shared lib suffix */
  if ((O =(NspObject *) nsp_smatrix_create("%shext",1,1,SHREXT_NAME,1) ) == NULLOBJ )
    return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  /* reserved */
  if ((data->Reserved =nsp_create_empty_matrix_object("@keep@"))==NULLOBJ) return FAIL;
#ifndef NSP_WITH_MAIN_GTK_THREAD
  Reserved = data->Reserved;
#endif
  /* flag to know that we are using nsp !! */
  if ((O =nsp_create_true_object("%nsp"))==NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  /* flag to detect if we have fftw */
#ifdef WITH_FFTW3
  if ((O =nsp_create_true_object("%fftw"))==NULLOBJ) return FAIL;
#else
  if ((O =nsp_create_false_object("%fftw"))==NULLOBJ) return FAIL;
#endif
  nsp_frame_replace_object1(data,O,-1);
  /* flag to detect if we have glpk */
#ifdef WITH_GLPK
  if ((O =nsp_create_true_object("%glpk"))==NULLOBJ) return FAIL;
#else
  if ((O =nsp_create_false_object("%glpk"))==NULLOBJ) return FAIL;
#endif
  nsp_frame_replace_object1(data,O,-1);
  /* flag to detect if we have umfpack */
#ifdef WITH_UMFPACK
  if ((O =nsp_create_true_object("%umfpack"))==NULLOBJ) return FAIL;
#else
  if ((O =nsp_create_false_object("%umfpack"))==NULLOBJ) return FAIL;
#endif
  nsp_frame_replace_object1(data,O,-1);
  /* flag to detect if we have cholmod */
#ifdef WITH_CHOLMOD
  if ((O =nsp_create_true_object("%cholmod"))==NULLOBJ) return FAIL;
#else
  if ((O =nsp_create_false_object("%cholmod"))==NULLOBJ) return FAIL;
#endif
  nsp_frame_replace_object1(data,O,-1);
  /* flag to detect if we have spqr */
#ifdef WITH_SPQR
  if ((O =nsp_create_true_object("%spqr"))==NULLOBJ) return FAIL;
#else
  if ((O =nsp_create_false_object("%spqr"))==NULLOBJ) return FAIL;
#endif
  nsp_frame_replace_object1(data,O,-1);
  /* are we a windows version */
#ifdef WIN32
  if ((O =nsp_create_true_object("%win32"))==NULLOBJ) return FAIL;
#else
  if ((O =nsp_create_false_object("%win32"))==NULLOBJ) return FAIL;
#endif
  nsp_frame_replace_object1(data,O,-1);
  /* are we a windows version which is a 64 bits version ? */
#ifdef WIN32
#ifdef TARGET_W64
  if ((O =nsp_create_true_object("%win64"))==NULLOBJ) return FAIL;
#else
  if ((O =nsp_create_false_object("%win64"))==NULLOBJ) return FAIL;
#endif
#else
  if ((O =nsp_create_false_object("%win64"))==NULLOBJ) return FAIL;
#endif
  nsp_frame_replace_object1(data,O,-1);

  /* which nsp version */
  if ((O= nsp_create_object_from_str("%nsp_version",NSP_VERSION))==NULLOBJ)
    return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  /* which host */
  if ((O= nsp_create_object_from_str("%host",HOST_TYPE))==NULLOBJ)
    return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  /* types */
  nsp_frame_replace_object1(data,(NspObject *)nsp_types_hash_table,-1);
  /* gtk */
  nsp_frame_replace_object1(data,(NspObject *)nsp_gtk_hash_table,-1);
  nsp_frame_replace_object1(data,(NspObject *)nsp_gdk_hash_table,-1);
  nsp_frame_replace_object1(data,(NspObject *)nsp_atk_hash_table,-1);
  nsp_frame_replace_object1(data,(NspObject *)nsp_pango_hash_table,-1);
  nsp_frame_replace_object1(data,(NspObject *)nsp_cairo_hash_table,-1);
  nsp_frame_replace_object1(data,(NspObject *)nsp_gio_hash_table,-1);
  nsp_frame_replace_object1(data,(NspObject *)nsp_glib_hash_table,-1);
  /* ast */
  if ((O = (NspObject *) nsp_ast_hash_create()) ==NULLOBJ) return FAIL;
  nsp_frame_replace_object1(data,O,-1);
  /* the top level frame */
  return OK;
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

int nsp_new_frame(const char *name)
{
  nsp_datas *data = nsp_get_datas();
  NspFrame *frame;
  if ( data->L == NULLLIST ) return FAIL;
  if (( frame=nsp_frame_create(name,NULL))== NULLFRAME) return FAIL;
  if (nsp_list_store(data->L,NSP_OBJECT(frame),1) == FAIL) return FAIL;
  return OK;
}

/**
 * nsp_new_frame_with_local_vars:
 * @table:
 *
 * Allocate a new frame for function invocation. The frames are
 * stored in a linked list. If this linked list is not initialized then
 * the call to nsp_new_frame() will fail. The frame can contain
 * a table used for local variables and a table for dynamically
 * added variables (a sorted list or a hash table, default value is a sorted list).
 *
 * Return value: %OK or %FAIL.
 **/

int nsp_new_frame_with_local_vars(const char *name,NspCells *table)
{
  nsp_datas *data = nsp_get_datas();
  NspFrame *frame;
  if ( data->L == NULLLIST ) return FAIL;
  if ((frame=nsp_frame_create(name,table))== NULLFRAME) return FAIL;
  if (nsp_list_store(data->L,NSP_OBJECT(frame),1) == FAIL) return FAIL;
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
  nsp_datas *data = nsp_get_datas();
  if ( data->L == NULLLIST )   return ;
  nsp_list_delete_elt(data->L,1) ;
}

/**
 *nsp_frames_info:
 * @void:
 *
 * Display information on allocated frames.
 **/

void nsp_frames_info(void)
{
  nsp_datas *data = nsp_get_datas();
  if ( data->L == NULLLIST )
    Sciprintf("Empty Datas\n");
  else
    nsp_list_info(data->L,0,NULL,0) ;

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
  nsp_datas *data = nsp_get_datas();
  if ( data->L == NULLLIST )
    Sciprintf("Empty Datas\n");
  else
    nsp_list_print(data->L,0,NULL,0);
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
  nsp_datas *data;
  if (  A == NULLOBJ ) return(OK);
  data = nsp_get_datas();
  return nsp_frame_replace_object1(data, A,local_id);
}

static int nsp_frame_replace_object1(nsp_datas *data, NspObject *A,int local_id)
{
  if (  A == NULLOBJ ) return(OK);
  if (  data->L == NULLLIST )
    {
      Scierror("Error: Can't insert obj in empty Data frame\n");
      return FAIL;
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
	  return nsp_eframe_replace_object((NspFrame *) data->L->first->O,A);
	}
      else
	{
	  /* insert as a local variable */
	  int tag = VAR_IS_PERSISTENT(local_id) ? 2 : 1;
	  NspCells *C= (NspCells *) ((NspFrame *) data->L->first->O)->locals->objs[tag];
	  local_id = VAR_ID(local_id);
	  if ( C->objs[local_id] != NULL )  nsp_object_destroy(&C->objs[local_id]);
	  C->objs[local_id]=A;
	  return OK;
	}
    }
  return(OK);
}

/**
 *nsp_frame_save:
 * @file: a #NspFile
 *
 * save the objects of the current evaluation frame.
 *
 * Return value: %OK or %FAIL.
 **/

int nsp_frame_save(NspFile *F)
{
  nsp_datas *data = nsp_get_datas();
  if (  data->L == NULLLIST )  return FAIL;
  return nsp_eframe_to_save(F,(NspFrame *) data->L->first->O);
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
  nsp_datas *data;
  int Int,Num;
  if (  A == NULLOBJ ) return(OK);
  data = nsp_get_datas();
  if (  data->GlobalFrame == NULL ) return FAIL;
  if ( nsp_find_function(nsp_object_get_name(A),&Int,&Num) == OK
       || nsp_find_macro(nsp_object_get_name(A)) != NULLOBJ)
    {
      Sciprintf("Warning: variable %s will hide a function name\n",
		nsp_object_get_name(A));
      return FAIL;
    }
  return nsp_eframe_replace_object((NspFrame *)  data->GlobalFrame,A);
}

/**
 *nsp_toplevel_frame_replace_object:
 * @A:  object to be inserted
 *
 * Insert Object A in the toplevel frame
 * If an object with the same name as A exists
 * It is replaced by A
 *
 *
 * Return value: %OK or %FAIL.
 **/

int nsp_toplevel_frame_replace_object(NspObject *A)
{
  nsp_datas *data;
  if (  A == NULLOBJ ) return(OK);
  data = nsp_get_datas();
  if (  data->TopFrame == NULL ) return FAIL;
  return nsp_eframe_replace_object((NspFrame *) data->TopFrame,A);
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
  nsp_datas *data;
  NspObject *Obj;
  Cell *C;
  data = nsp_get_datas();
  if ( data->L == NULLLIST )
    return NULLOBJ;
  else
    {
      C= data->L->first;
      while ( C != NULLCELL)
	{
	  if ((Obj = nsp_eframe_search_object((NspFrame *) C->O,str,TRUE)) != NULLOBJ )
	    return Obj;
	  if ( frames_search_inhibit == TRUE )
	    {
	      /* FIXME: to find variables which are in globalframe
	       * but which are not really global %t %e etc...
	       */
	      return ((NspFrame *) C->O != data->GlobalFrame ) ? nsp_global_frame_search_object(str) : NULLOBJ;
	    }
	  else
	    C = C->next ;
	}
      return NULLOBJ ;
    }
}



/**
 * nsp_frames_search_local_in_calling:
 * @str: a string
 * @caller_flag: %TRUE or %FALSE
 *
 * This function is used when a local variable has no
 * value and we want to see if the local variable can
 * be initialized by a variable with same name in calling stack.
 * for example function f();if y >=0; y=y+1;endfunction
 * y is considered local because of y=y+1 but can be inherited.
 * If @caller_flag is set to %TRUE then the value is only searched in
 * the caller frame.
 *
 * Return value: a #NspObject
 **/

NspObject *nsp_frames_search_local_in_calling(const char *str, int caller_flag)
{
  nsp_datas *data;
  NspObject *Obj;
  Cell *C ;
  data = nsp_get_datas();
  C = data->L->first->next;
  if ( frames_search_inhibit == TRUE ) return NULLOBJ;
  /* we assume here that str was already found in data->L->first but with no value
   */
  while ( C != NULLCELL)
    {
      /* subsequent call of nsp_eframe_search_object must not re-enter
       * in nsp_frames_search_local_in_calling thus tag is set to FALSE
       */
      if ((Obj = nsp_eframe_search_object((NspFrame *) C->O,str,FALSE)) != NULLOBJ )
	return Obj;
      if ( caller_flag == TRUE ) break; /* stop in the caller */
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
  nsp_datas *data;
  data = nsp_get_datas();
  if ( data->L == NULLLIST )   return NULLOBJ;
  return nsp_eframe_search_object((NspFrame *) data->L->first->O,str,TRUE) ;
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
  nsp_datas *data;
  data = nsp_get_datas();
  if ( data->GlobalFrame == NULL )  return NULLOBJ;
  return nsp_eframe_search_object((NspFrame *)data->GlobalFrame ,str,TRUE) ;
}

/**
 * nsp_global_frame_remove_object:
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
  nsp_datas *data;
  data = nsp_get_datas();
  if ( data->GlobalFrame == NULL )  return ;
  nsp_eframe_remove_object((NspFrame *)data->GlobalFrame,str);
}

/**
 * nsp_global_frame_remove_all_objects:
 *
 * remove all objects from global frame
 **/

int nsp_global_frame_remove_all_objects(void)
{
  nsp_datas *data;
  data = nsp_get_datas();
  if ( data->GlobalFrame == NULL ) return OK;
  nsp_frame_destroy((NspFrame *)data->GlobalFrame);
  if ((data->GlobalFrame = nsp_frame_create("global",NULL)) == NULLFRAME)
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
  nsp_datas *data;
  data = nsp_get_datas();
  if ( data->L == NULLLIST ) return NULLOBJ;
  return nsp_eframe_search_and_remove_object((NspFrame *) data->L->first->O ,str);
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
  nsp_datas *data;
  data = nsp_get_datas();
  if ( data->L == NULLLIST ) return;
  nsp_eframe_remove_object((NspFrame *)data->L->first->O,str);
}

/**
 *nsp_frame_remove_all_objects:
 *
 * Search for an object  with name @str in the first frame and
 * remove the object
 **/

void nsp_frame_remove_all_objects(void)
{
  nsp_datas *data;
  data = nsp_get_datas();
  if ( data->L == NULLLIST ) return;
  nsp_eframe_remove_all_objects((NspFrame *)data->L->first->O);
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
  nsp_datas *data;
  NspObject *O;
  Cell *C;
  data = nsp_get_datas();
  if ( data->L == NULLLIST )
    return FAIL;
  else
    {
      C= data->L->first;
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
      return nsp_eframe_replace_object((NspFrame *)data->L->first->next->O,O);
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
  nsp_datas *data;
  Cell *C;
  if (  O == NULLOBJ ) return(OK);
  data = nsp_get_datas();
  if ( data->L == NULLLIST )
    {
      Scierror("Error: Can't insert object %s in empty Data frame\n",nsp_object_get_name(O));
      return FAIL;
    }
  else
    {
      C= data->L->first;
      if ( C->next == NULLCELL)
	{
	  Scierror("Error: cannot move %s in an upper frame, we are on top\n",nsp_object_get_name(O));
	  return FAIL;
	}
      return nsp_eframe_replace_object((NspFrame *)data->L->first->next->O,O);
    }
}

/**
 *nsp_declare_global:
 * @name: a name to be considered as a global variable name.
 * @int: an integer giving the id of a variable in a local frame (can be set to -1).
 * @value: an object to be used as value if the variable does not exists. It can be NULL.
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


int nsp_declare_global(const char *name, int id, NspObject *value)
{
  nsp_datas *data;
  NspObject *O;
  NspHobj *O1;
  data = nsp_get_datas();
  if ( data->L == NULLLIST ) return FAIL;
  if ( name == NULL) return FAIL;
  /* if current frame is the global frame we have nothing to do */
  if ( data->L->first->O == (NspObject *) data->GlobalFrame ) return OK;
  /* search in the global frame */
  O=nsp_global_frame_search_object(name);
  if ( O == NULLOBJ )
    {
      if ( value == NULL)
	{
	  /* create an empty matrix */
	  if ((O =nsp_create_empty_matrix_object(name))== NULLOBJ) return FAIL;
	}
      else
	{
	  if ((O = nsp_object_copy_and_name(name,value)) == NULLOBJ) return FAIL;
	}
      /* if (nsp_object_set_name(O,name) == FAIL) return FAIL; */
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
#if 0
	  if ( nsp_frame_replace_object(Ob,-1)== FAIL) return FAIL;
#else
	  /* if some object are global in the frame, keep them global */
	  if ( nsp_store_object(Ob) == FAIL) return FAIL;
#endif
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
  nsp_datas *data;
  data = nsp_get_datas();
  if ( data->L == NULLLIST ) return NULLHASH;
  return nsp_eframe_to_hash((NspFrame *) data->L->first->O);
}


/**
 *nsp_frame_set_persistent_value:
 * @Obj: nsp object
 *
 * set the value of persistent variable to the value of #Obj if the persistent
 * value has no value. The name of the persistent value to change is the
 * object name. When the value of #Obj is used the used variable is set to %TRUE else to %FALSE.
 *
 * Return value: %OK or %FAIL
 **/

int nsp_frame_set_persistent_value(NspObject *Obj, int *used)
{
  nsp_datas *data = nsp_get_datas();
  if ( data->L == NULLLIST ) return FAIL;
  return nsp_eframe_set_persistent_value((NspFrame *)data->L->first->O,Obj,used);
}

/* */

int nsp_set_matrix(const char *name, char type, int m, int n, void *val)
{
  int i;
  double *dval = val;
  NspMatrix *M;
  nsp_datas *data = nsp_get_datas();
  if (( M = nsp_matrix_create(name,type,m,n)) == NULLMAT) return FAIL;
  if ( type == 'r' )
    for ( i= 0; i < M->mn; i++) M->R[i]= dval[i];
  else
    for ( i= 0; i < M->mn; i++) { M->C[i].r = dval[i];M->C[i].i = dval[i+ M->mn];}
  nsp_frame_replace_object1(data,(NspObject *) M,-1);
  return OK;
}

/* */

int nsp_set_imatrix(const char *name, int itype, int m, int n, void *val)
{
  NspIMatrix *M;
  nsp_datas *data = nsp_get_datas();
  if (( M = nsp_imatrix_create(name, m, n, itype)) == NULLIMAT) return FAIL;
  memcpy(M->Iv,val,M->mn*sizeof(M->eltsize));
  nsp_frame_replace_object1(data,(NspObject *) M,-1);
  return OK;
}
