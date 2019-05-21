/* Nsp
 * Copyright (C) 2006-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 * frames for storing evaluation environments.
 */


#define  Frame_Private 
#include <nsp/object.h> 
#include <nsp/type.h> 
#include <nsp/file.h> 
#include <nsp/hobj.h> 
#include <nsp/cells.h> 
#include <nsp/list.h> 
#include <nsp/smatrix.h> 
#include <nsp/hash.h> 
#include <nsp/bhash.h> 
#include <nsp/frame.h>
#include <nsp/interf.h>
#include <nsp/datas.h>

#ifdef SMAT_SYMB_TABLE
static int nsp_bsearch_string(NspSMatrix *S,const char *x,int *val);
#endif 

static int nsp_eframe_setup_local_vars(NspFrame *H, NspCells *C);
static void nsp_eframe_unsetup_local_vars(NspFrame *H);

/* 
 * NspFrame inherits from NspObject
 */

int nsp_type_frame_id=0;
NspTypeFrame *nsp_type_frame=NULL;

/*
 * Type object for Frame 
 * all the instance of NspTypeFrame share the same id. 
 * nsp_type_frame: is an instance of NspTypeFrame 
 *    used for objects of NspFrame type (i.e built with new_frame) 
 * other instances are used for derived classes 
 */

NspTypeFrame *new_type_frame(type_mode mode)
{
  NspTypeFrame *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_frame != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_frame;
    }
  
  if ((type =  malloc(sizeof(NspTypeFrame))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = frame_get_methods; 
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_frame;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for frame */ 
  
  top->pr = (print_func *) nsp_frame_print;                    
  top->dealloc = (dealloc_func *) nsp_frame_destroy;
  top->copy  =  (copy_func *) nsp_frame_copy;                   
  top->size  = (size_func *) nsp_frame_size;                  
  top->s_type =  (s_type_func *) nsp_frame_type_as_string;    
  top->sh_type = (sh_type_func *) nsp_frame_type_short_string;
  top->info = (info_func *) nsp_frame_info ;                    
  /* top->is_true = (is_true_func  *) FrameIsTrue; */
  /* top->loop =(loop_func *) frame_loop;*/
  top->path_extract = (path_func *)  object_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) nsp_frame_object;
  top->eq  = (eq_func *) nsp_frame_eq;
  top->neq  = (eq_func *) nsp_frame_neq;
  /* Not implemented 
  top->save  = (save_func *) nsp_frame_xdr_save;
  top->load  = (load_func *) nsp_frame_xdr_load;
  */
  /* specific methods for frame */
      
  type->init = (init_func *) init_frame;
      
  /* 
   * Frame interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_frame_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeFrame called nsp_type_frame
       */
      type->id =  nsp_type_frame_id = nsp_new_type_id();
      nsp_type_frame = type;
      if ( nsp_register_type(nsp_type_frame) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_frame(mode);
    }
  else 
    {
      type->id = nsp_type_frame_id;
      return type;
    }
}

/*
 * initialize Frame instances 
 * locally and by calling initializer on parent class 
 */

static int init_frame(NspFrame *o,NspTypeFrame *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  o->local_vars = NULL;
  o->locals = NULL;
  o->vars = NULL;
  return OK;
}

/*
 * new instance of Frame 
 */

NspFrame *new_frame() 
{
  NspFrame *loc; 
  /* type must exists */
  nsp_type_frame = new_type_frame(T_BASE);
  if ( (loc = malloc(sizeof(NspFrame)))== NULLFRAME) return loc;
  /* initialize object */
  if ( init_frame(loc,nsp_type_frame) == FAIL) return NULLFRAME;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Frame 
 *-----------------------------------------------*/

/*
 * size 
 */

static int nsp_frame_size(NspFrame *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char frame_type_name[]="Frame";
static char frame_short_type_name[]="frame";

static char *nsp_frame_type_as_string(void)
{
  return(frame_type_name);
}

static char *nsp_frame_type_short_string(NspObject *v)
{
  return(frame_short_type_name);
}

static int nsp_frame_full_comp(NspFrame * A,NspFrame * B,char *op,int *err)
{
  Scierror("frame_full_comp: to be implemented \n");
  return FALSE;
}

/*
 * A == B 
 */

static int nsp_frame_eq(NspFrame *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_frame_id) == FALSE) return FALSE ;
  rep = nsp_frame_full_comp(A,(NspFrame *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

/*
 * A != B 
 */

static int nsp_frame_neq(NspFrame *A, NspObject *B)
{
  int err=0,rep;
  if ( check_cast(B,nsp_type_frame_id) == FALSE) return TRUE;
  rep = nsp_frame_full_comp(A,(NspFrame *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

/* 
static int nsp_frame_xdr_save(XDR  *xdrs, NspFrame *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->nbytes) == FAIL) return FAIL;
  if (nsp_xdr_save_array_c(xdrs,M->val,M->nbytes) == FAIL) return FAIL;
  return OK;
}
*/
/*
 * load 
 */
/* 
static NspFrame  *nsp_frame_xdr_load(XDR *xdrs)
{
  int nbytes;
  NspFrame *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLFRAME;
  if (nsp_xdr_load_i(xdrs,&nbytes) == FAIL) return NULLFRAME;
  / * nbytes here is all the bytes i.e + the frame header * /
  if ((M = nsp_frame_create(name,NULL,nbytes-strlen(nsp_frame_header)))== NULLFRAME) return NULLFRAME;
  if (nsp_xdr_load_array_c(xdrs,M->val,M->nbytes) == FAIL) return  NULLFRAME;
  return M;
}
*/

/*
 * delete 
 */

void nsp_frame_destroy(NspFrame *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
#ifdef WITH_SYMB_TABLE 
  nsp_eframe_unsetup_local_vars(H);
#endif 
#ifdef FRAME_AS_LIST
  nsp_list_destroy(H->vars);
#else 
  nsp_hash_destroy(H->vars);
#endif 
  FREE(H);
}

/*
 * info 
 */

void nsp_frame_info(NspFrame *H, int indent,const char *name, int rec_level)
{
  int i;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t=\t\t frame\n",pname);
}

/*
 * print 
 */

int nsp_frame_print(NspFrame *H, int indent,const char *name, int rec_level)
{
  nsp_frame_info(H,indent,name,rec_level);
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Frame objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspFrame   *nsp_frame_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast(O,nsp_type_frame_id) == TRUE) return ((NspFrame *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_frame));
  return(NULL);
}

int IsFrameObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_frame_id);
}

int IsFrame(NspObject *O)
{
  return nsp_object_type(O,nsp_type_frame_id);
}

NspFrame  *GetFrameCopy(Stack stack, int i)
{
  if (  GetFrame(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspFrame  *GetFrame(Stack stack, int i)
{
  NspFrame *M;
  if (( M = nsp_frame_object(NthObj(i))) == NULLFRAME)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspFrame instance 
 *
 * creates a frame buffer which can store nbytes 
 * If buf is present buf is copied into the frame buffer 
 * When filled we start val whith a string to get the 
 * opportunity of checking an incorrect frame.
 *-----------------------------------------------------*/

static NspFrame *_nsp_frame_create(const char *name, NspCells *C,NspTypeBase *type)
{
  NspFrame *H  = (type == NULL) ? new_frame() : type->new();
  if ( H ==  NULLFRAME)
    {
      Sciprintf("No more memory\n");
      return NULLFRAME;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return NULLFRAME;
#ifdef FRAME_AS_LIST
  if ((H->vars = nsp_list_create(NVOID)) == NULLLIST) return NULLFRAME;
#else 
  if(( H->vars = nsp_hash_create(NVOID,11)) == NULLHASH) return NULLFRAME;
#endif
  if ( C != NULL )
    {
      if ( nsp_eframe_setup_local_vars(H,C) == FAIL ) return NULLFRAME;
    }
  NSP_OBJECT(H)->ret_pos = -1 ;
  return H;
}

NspFrame *nsp_frame_create(const char *name, NspCells *C)
{
  return _nsp_frame_create(name,C,NULL);
}

/*
 * copy 
 */

NspFrame *nsp_frame_copy(const NspFrame *H)
{
  return _nsp_frame_create(NVOID,H->locals,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the Frame
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

static int int_frame_meth_unframeize(void *a,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1);
  return 1;
}

static NspMethods frame_methods[] = {
  { "unframeize", int_frame_meth_unframeize},
  { (char *) 0, NULL}
};

static NspMethods *frame_get_methods(void) { return frame_methods;};



/*-------------------------------------------
 * function 
 *-------------------------------------------*/

/* inhibit the search fro symbols in calling frames */
extern int frames_search_inhibit;

/**
 * nsp_eframe_search_object:
 * @F: a #Nspframe 
 * @name: a string 
 * @tag: %TRUE or %FALSE
 * 
 * search object @name in the frame given by @F. Note that 
 * If an object has no value but is a local variable and @tag is %TRUE,
 * then the object is searched in calling stacks and if found 
 * the object is returned.
 * Note that in that case the local variable table is not modified
 * and that the returned object should not be changed since it is 
 * not a @F variable. 
 * 
 * Return value: the #NspObject if found or  %NULLOBJ 
 **/

#ifdef SMAT_SYMB_TABLE
#define VARS_LOCAL(name) (nsp_bsearch_string((NspSMatrix *) F->local_vars,name,&val) == OK)
#else 
#define VARS_LOCAL(name) (nsp_bhash_find(F->local_vars,name,&val) == OK)
#endif 


NspObject *nsp_eframe_search_object(NspFrame *F,const char *name,int tag )
{
  NspObject *Obj=NULLOBJ ;
#ifdef FRAME_AS_LIST
  if (( Obj = nsp_sorted_list_search(F->vars,name)) != NULLOBJ )
    return Obj;
#else 
  if ( nsp_hash_find(F->vars,name,&Obj)== OK) 
    return Obj;
#endif
  /* then search in local variables hash-table 
   * Note that most of the time local variables are not searched 
   * that way but by direct access.
   */
  if ( F->local_vars != NULL ) 
    {
      int val;
      /* Sciprintf("searching a local object %s with nsp_eframe_search_object\n",name); */
      if ( VARS_LOCAL(name) )
	{
	  int tag1 = VAR_IS_PERSISTENT(val) ? 2 : 1;
	  val = VAR_ID(val);
	  if ( ((NspCells *)F->locals->objs[tag1])->objs[val] == NULLOBJ ) 
	    {
	      /* Sciprintf("local object %s found but has no value\n",name); */
	      /* search in calling frames */
	      if ( tag == TRUE )
		return nsp_frames_search_local_in_calling(name,FALSE);
	      else 
		return NULLOBJ;
	    }
	  else 
	    {
	      /* Sciprintf("\tobject %s found\n",name);*/
	      return ((NspCells *)F->locals->objs[tag1])->objs[val];
	    }
	}
    }
  return NULLOBJ;
}

/**
 * nsp_eframe_replace_object:
 * @F: 
 * @A: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_eframe_replace_object(NspFrame *F, NspObject *A)
{
  if (  A == NULLOBJ ) return(OK);
  if ( F->local_vars != NULL ) 
    {
      /* first search in local variables */
      int val; 
      /* Sciprintf("Replace a local object %s with nsp_eframe_replace_object\n",nsp_object_get_name(A));*/
      if ( VARS_LOCAL(nsp_object_get_name(A))) 
	{
	  int tag = VAR_IS_PERSISTENT(val) ? 2 : 1;
	  val = VAR_ID(val);
	  /* object is a local variable */
	  nsp_object_destroy(&((NspCells *)F->locals->objs[tag])->objs[val]);
	  ((NspCells *)F->locals->objs[tag])->objs[val] = A;
	  /* Sciprintf("\t replacement done for %s \n",nsp_object_get_name(A)); */
	  return OK;
	}
    }
#ifdef FRAME_AS_LIST
  return nsp_sorted_list_insert(F->vars, A);
#else 
  return nsp_hash_enter(F->vars,A);
#endif
} 


/**
 * nsp_eframe_search_and_remove_object:
 * @F: 
 * @str: 
 * 
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_eframe_search_and_remove_object(NspFrame *F,nsp_const_string str)
{
#ifndef FRAME_AS_LIST
  NspObject *Ob;
#endif
  if ( F->local_vars != NULL ) 
    {
      /* first search in local variables */
      int val; 
      /* Sciprintf("Trying a search and remove for %s \n",str); */
      if ( VARS_LOCAL(str)) 
	{
	  NspObject *O1;
	  int tag = VAR_IS_PERSISTENT(val) ? 2 : 1;
	  val = VAR_ID(val);
	  O1 = ((NspCells*) F->locals->objs[tag])->objs[val];
	  ((NspCells*) F->locals->objs[tag])->objs[val] = NULL;
	  /* Sciprintf("\tsearch and remove ok %s \n",str); */
	  return O1;
	}
    }
#ifdef FRAME_AS_LIST
  return nsp_sorted_list_search_and_remove(F->vars,str) ;
#else 
  /* FIXME: we need here a unique option nsp_hash_find_and_remove */
  if ( nsp_hash_find_and_copy(F,str,&Ob)== OK)
    {
      nsp_hash_remove(F,str);
      return Ob;
    }
  return NULLOBJ;
#endif
} 

/**
 * nsp_eframe_to_hash:
 * @F: a #NspFrame 
 * 
 * returns a copy of the content of the frame @F 
 * in a #NspHash object. 
 * 
 * Return value: a new #NspHash or %NULLHASH
 **/

NspHash *nsp_eframe_to_hash(NspFrame *F)
{
  int j;
  NspHash  *Obj;
  NspObject *Elt;
#ifdef FRAME_AS_LIST
  Obj= nsp_hcreate_from_list(NVOID,nsp_list_length(F->vars),F->vars);
#else 
  Obj= nsp_hash_copy(F->vars);
#endif
  /* if we only have local variables this is wrong ? XXXXX */
  if ( Obj == NULLHASH) return Obj;
  if ( F->locals == NULL) return Obj;
  for ( j = 1 ; j < F->locals->mn ; j++) 
    {
      int i;
      NspCells *Table = (NspCells *) F->locals->objs[j];
      for ( i = 0 ; i < Table->mn ; i++) 
	{
	  Elt= Table->objs[i];
	  if ( Elt != NULL && Ocheckname(Elt,NVOID)== FALSE)
	    {
	      /* A copy of object is added in the hash table *
	       * take care of Hobj pointers 
	       */
	      HOBJ_GET_OBJECT(Elt,NULLHASH);
	      if (( Elt =nsp_object_copy_with_name(Elt)) == NULLOBJ )
		return NULLHASH;
	      if (nsp_hash_enter( Obj,Elt) == FAIL) return NULLHASH;
	    }
	}
    }
  return Obj;
} 

/**
 * nsp_eframe_to_smat:
 * @F: a #NspFrame 
 * 
 * returns the names contained in frame @F 
 * in a #NspSMatrix object. 
 * 
 * Return value: a new #NspSMatrix or %NULLHASH
 **/

NspSMatrix *nsp_eframe_to_smat(NspFrame *F)
{
  int j;
  NspSMatrix  *Obj;
  NspObject *Elt;
  /* first insert the names in the list part of Frame */
#ifdef FRAME_AS_LIST
  /* XXXX:  This should be turned in a function in List.c */
  if ((Obj = nsp_smatrix_create(NVOID,0,0,"",0))== NULLSMAT) 
    return NULLSMAT;
  if (F->vars != NULLLIST ) 
    {
      Cell *C = F->vars->first;
      while ( C != NULLCELL) 
	{
	  if ( C->O != NULLOBJ ) 
	    {
	      const char *str = nsp_object_get_name(NSP_OBJECT(C->O));
	      if ( strcmp(str,NVOID) != 0) 
		{
		  if ( nsp_row_smatrix_append_string(Obj,str) == FAIL) 
		    return NULLSMAT;
		}
	    }
	  C = C->next ;
	}
    }
#else
  Obj = nsp_hash_get_keys(F->vars);
#endif
  /* if we only have local variables this is wrong ? XXXXX */
  if ( Obj == NULLSMAT) return Obj;
  /* then insert the names from local variables */
  if ( F->locals == NULL) return Obj;
  for ( j = 1 ; j < F->locals->mn ; j++) 
    {
      int i;
      NspCells *Table = (NspCells *) F->locals->objs[j];
      for ( i = 0 ; i < Table->mn ; i++) 
	{
	  Elt= Table->objs[i];
	  if ( Elt != NULL && Ocheckname(Elt,NVOID)== FALSE)
	    {
	      const char *str = nsp_object_get_name(NSP_OBJECT(Elt));
	      if ( nsp_row_smatrix_append_string(Obj,str) == FAIL) 
	    return NULLSMAT;
	    }
	}
    }
  return Obj;
} 
/**
 * nsp_eframe_to_save:
 * @file: a #NspFile
 * @F: a #NspFrame 
 * 
 * returns the names contained in frame @F 
 * in a #NspSMatrix object. 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_eframe_to_save( NspFile *file,NspFrame *F)
{
  int j;
  NspObject *Elt;
  /* first insert the names in the list part of Frame */
#ifdef FRAME_AS_LIST
  if (F->vars != NULLLIST ) 
    {
      Cell *C = F->vars->first;
      while ( C != NULLCELL) 
	{
	  if ( C->O != NULLOBJ ) 
	    {
	      const char *str = nsp_object_get_name(NSP_OBJECT(C->O));
	      if ( strcmp(str,NVOID) != 0) 
		{
		  if (nsp_object_xdr_save(file->obj->xdrs,NSP_OBJECT(C->O)) == FAIL) return FAIL;
		}
	    }
	  C = C->next ;
	}
    }
#else
  Scierror("nsp_eframe_to_save unfinished \n");
  return FAIL;
#endif
  /* if we only have local variables this is wrong ? XXXXX */
  /* then insert the names from local variables */
  if ( F->locals == NULL) return OK;
  for ( j = 1 ; j < F->locals->mn ; j++) 
    {
      int i;
      NspCells *Table = (NspCells *) F->locals->objs[j];
      for ( i = 0 ; i < Table->mn ; i++) 
	{
	  Elt= Table->objs[i];
	  if ( Elt != NULL && Ocheckname(Elt,NVOID)== FALSE)
	    {
	      if (nsp_object_xdr_save(file->obj->xdrs,Elt) == FAIL) return FAIL;
	    }
	}
    }
  return OK;
} 

/**
 * nsp_eframe_remove_object:
 * @F: 
 * @str: 
 * 
 * 
 **/
    
void nsp_eframe_remove_object(NspFrame *F,nsp_const_string str)
{
  NspObject *O;
  if ( F->local_vars != NULL ) 
    {
      /* first search in local variables */
      int val; 
      /* Sciprintf("Trying a search and remove for %s \n",str); */
      if ( VARS_LOCAL(str)) 
	{
	  NspObject *O1;
	  int tag = VAR_IS_PERSISTENT(val) ? 2 : 1;	  
	  val = VAR_ID(val);
	  O1 = ((NspCells *) F->locals->objs[tag])->objs[val];
	  if ( O1 != NULL &&  O1->flag == TRUE ) return;
	  nsp_object_destroy(&O1);
	  ((NspCells *) F->locals->objs[tag])->objs[val]= NULL;
	  /* Sciprintf("\tsearch and remove ok %s \n",str); */
	  return ;
	}
    }
#ifdef FRAME_AS_LIST
  /* Sciprintf("Trying a remove for %s \n",str); */
  O=nsp_sorted_list_search_and_remove(F->vars,str);
  nsp_object_destroy(&O);
#else 
  nsp_hash_remove(F,str);
#endif
}

/**
 * nsp_eframe_remove_all_objects:
 * @F: 
 * 
 * 
 **/
    
void nsp_eframe_remove_all_objects(NspFrame *F)
{
  if ( F->local_vars != NULL ) 
    {
      /* first search in local variables */
      if ( F->locals != NULL) 
	{
	  int j;
	  for ( j = 1 ; j < F->locals->mn ; j++) 
	    {
	      int i;
	      NspCells *Table = (NspCells *) F->locals->objs[j];
	      for ( i = 0 ; i < Table->mn ; i++) 
		{
		  NspObject *Elt= Table->objs[i];
		  if ( Elt != NULL && Ocheckname(Elt,NVOID)== FALSE)
		    {
		      nsp_object_destroy(&Elt);
		      Table->objs[i]= NULL;
		    }
		}
	    }
	}
    }
#ifdef FRAME_AS_LIST
  if ( F->vars != NULLLIST)
    {
      Cell *loc= F->vars->first,*loc1;
      while ( loc != NULLCELL) 
	{
	  loc1= loc->next ;
	  nsp_cell_destroy(&loc);
	  loc = loc1;
	}
      F->vars->first =F->vars->last =F->vars->current= NULLCELL;
    }
#else 
  /* */
  if ( F->vars != NULLHASH )
    {
      for ( i =0 ; i <= F->vars->hsize ; i++) 
	{
	  Hash_Entry *loc = ((Hash_Entry *) H->htable) + i;
	  if ( loc->used && loc->data != NULLOBJ ) 
	    nsp_object_destroy( &loc->data);
	  loc->used = 0; 
	  loc->data =  NULLOBJ;
	}
    }
#endif
}




/* utility */

#ifdef SMAT_SYMB_TABLE
static int nsp_bsearch_string(NspSMatrix *S,const char *x,int *val)
{
  int j, j1, j2,n= S->mn;
  if ( strcmp(S->S[0],x) <= 0  &&  strcmp(x,S->S[n-1]) <= 0 ) 
    {
      /* find j such that x = S->S(j) by a dicho search */
      j1 = 0;
      j2 = n-1;
      while(j2 - j1 > 1) 
	{
	  j = (j1 + j2) / 2;
	  if ( strcmp(x,S->S[j]) < 0 ) j2 = j; else j1 = j;
	}
      /*  here we know that S->S(j1) <= x <= S->S(j2)  with j2 = j1 + 1
       *  (in fact we have exactly  S->S(j1) <= x < S->S(j2) if j2 < n-1) 
       */
      if ( strcmp(x,S->S[j1]) == 0 ) 
	{
	  *val=  j1+1;
	  return OK;
	} 
      else if ( j2 == n-1  &&  strcmp(x,S->S[j2]) == 0 )  /* this case may happen only for j2=n-1 */
	{
	  *val=  j2+1;
	  return OK;
	} 
      else 
	{
	  /* x[i] is not in {S->S(1), S->S(2),..., S->S(n)} */ 
	  return FAIL;
	}
    }
  return FAIL;
} 
#endif 

/**
 * nsp_eframe_set_persistent_value:
 * @F: a #NspFrame
 * @Obj: a nsp object
 * @used: an int pointer
 * 
 * set the value of a persistent variable to #Obj if the 
 * persistent variable has no value. When the value of #Obj 
 * is used the used variable is set to %TRUE else to %FALSE.
 *
 * Return value: %OK or %FAIL
 **/
    
int nsp_eframe_set_persistent_value(NspFrame *F,NspObject *Obj, int *used)
{
  const char *str =  nsp_object_get_name(Obj);
  int val;
  *used = FALSE;
  if ( F->local_vars != NULL && VARS_LOCAL(str) )
    {
      /* Only search in local variables */
      NspObject *O1;
      val = VAR_ID(val);
      O1 = ((NspCells *) F->locals->objs[2])->objs[val];
      if ( O1 != NULL ) 
	{
	  return OK;
	}
      else
	{
	  *used = TRUE;
	  ((NspCells *) F->locals->objs[2])->objs[val] = Obj;
	}
    }
  else 
    {
      return FAIL;
    }
  return OK;
}


/* Copy a table of local variables in H->table 
 * persistent variables are not copied 
 */

static int nsp_eframe_setup_local_vars(NspFrame *H, NspCells *C)
{
  NspCells *Cl;
  if (( H->locals = nsp_cells_create(NVOID, 3,1)) == NULL) goto fail;
  H->locals->objs[0] = C->objs[0];/* this one is always shared: it is the hash table of local variable names */
  H->locals->objs[2] = C->objs[2];/* this one is always shared: it is the cell array for persistent variables  */
  H->local_vars = (NspBHash *) C->objs[0]; /* faster access */
  if ( C->objs[1] != NULL )
    {
      if (( Cl = nsp_cells_copy((NspCells *) C->objs[1]))  == NULL) goto fail;
      H->locals->objs[1] = (NspObject *) Cl;
    }
  else
    {
      H->locals->objs[1] = NULL;
    }
  return OK;
 fail:
  H->locals = NULL;
  H->local_vars = NULL;
  return FAIL;
}


static void nsp_eframe_unsetup_local_vars(NspFrame *H)
{
  if ( H->locals == NULL) return ;
  H->locals->objs[0] = NULL;
  H->locals->objs[2] = NULL;
  nsp_cells_destroy(H->locals);
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab Frame_func[]={
  /* {"unframeize_frame",int_frame_unframeize}, moved in object.c */
  {(char *) 0, NULL}
};

/* call ith function in the Frame interface */

int Frame_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Frame_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Frame_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Frame_func[i].name;
  *f = Frame_func[i].fonc;
}





