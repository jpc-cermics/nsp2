/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/**
 * SECTION:object
 * @title: #NspObject is the base class ob nsp objects 
 * @short_description: The class shared by all nsp objects.
 * @see_also: 
 *
 * <para>
 * All nsp objects inherit from  #NspObject.
 * </para>
 **/


/* 
 * NspObject 
 * all objects can be casted to a NspObject 
 */

#include <stdio.h> 
#include <stdlib.h> 
#include <glib.h>

#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/interf.h"
#include "../system/files.h" /* FSIZE */
#include "nsp/plistc.h" /* scigetline */
#include  "nsp/datas.h" 


/* FIXME: to be moved in object.h private zone */
static int object_size(NspObject *self, int flag);
static const char *get_name(NspObject *ob) ;
static const char *set_name(NspObject *ob,const char *name);
static int object_is_true_def(NspObject *ob);
static NspObject *object_loop_def(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int init_object(NspObject *ob,NspTypeObject *type);
static char *object_type_as_string(void);
static char *object_type_short_string(NspObject *obj);
static NspMethods *object_get_methods(void);
static int int_object_create(Stack stack, int rhs, int opt, int lhs);
static void nsp_object_latex_def(NspObject * M, int indent,char *name, int rec_level);
static int nsp_object_as_index_def(NspObject * M, index_vector *index);

/*
 * base object : NspObject 
 */

int nsp_type_object_id =0;
NspTypeObject  *nsp_type_object= NULL;

/**
 * new_type_object:
 * @mode: %T_BASE or %T_DERIVED 
 * 
 * used to create type instances for #NspObject object 
 * alls the instance of the #NspObject class share the 
 * same #NspTypeObject instance. 
 * 
 * Return value: a #NspTypeObject or %NULL
 **/

NspTypeObject *new_type_object(type_mode mode)
{
  NspTypeObject *type= NULL;
  if (  nsp_type_object != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_object;
    }
  
  if ((type =  malloc(sizeof(NspTypeObject))) == NULL) return NULL;
  type->surtype   = NULL;
  type->interface = NULL;
  type->methods = object_get_methods; 
  type->new = (new_func *) new_object;
  
  type->s_type =  (s_type_func *) object_type_as_string;    
  type->sh_type = (sh_type_func *) object_type_short_string;
  type->set_name = (set_name_func *) set_name;
  type->new = (new_func *) new_object;
  type->get_name = (get_name_func *) get_name;
  type->is_true = (is_true_func *) object_is_true_def ;
  type->init = (init_func *) init_object;
  type->size = (size_func *) object_size;
  type->loop =(loop_func *) object_loop_def;
  type->attrs = NULL;
  type->get_attrs = (attrs_func*) int_get_attribute ;  
  type->set_attrs = (attrs_func *) int_set_attribute ; 
  type->create = (create_func*) int_object_create;
  type->save = (save_func *) nsp_object_save_def;
  type->load = (load_func *) nsp_object_load_def;
  type->latex = (print_func *) nsp_object_latex_def;
  type->as_index  = (get_index_vector_func *) nsp_object_as_index_def;

  if ( nsp_type_object_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeObject called nsp_type_object
       */
      type->id =  nsp_type_object_id = nsp_new_type_id();
      nsp_type_object = type;
      if ( nsp_register_type(nsp_type_object) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_object(mode);
    }
  else 
    {
      type->id = nsp_type_object_id;
      return type;
    }
}

/*
 * initialize Object instances 
 * locally and by calling initializer on parent class 
 */

static int init_object(NspObject *o,NspTypeObject *type)
{
  /* to be done always */ 
  o->type = type; 
  o->basetype = (NspTypeBase *)type;
  /* specific */
  o->flag = 0;
  return OK;
}

/**
 * new_object:
 * 
 * Creates a new instance of #NspObject. 
 * Not used directly since  #NspObject is an abstract class.
 * 
 * Return value: a #NspObject or %NULLOBJ 
 **/

NspObject *new_object(void) 
{
  NspObject *loc; 
  /* type must exists */
  nsp_type_object = new_type_object(T_BASE);
  if ( (loc = malloc(sizeof(NspObject)))== NULLOBJ) return loc;
  /* initialize object */
  if ( init_object(loc,nsp_type_object) == FAIL) return NULLOBJ;
  return loc;
}

/*
 * check that o can be casted to an object of type id 
 */

/**
 * check_cast:
 * @obj: any object to be checked 
 * @id: an type instance id.
 * 
 * checks that object given by @o inherits 
 * from class type with @id signature.
 * 
 * Return value: %TRUE or %FALSE.
 **/

#ifndef HAVE_INLINEXX 
/* we need here to insert code */
#define NSP_OBJECT_INLINED 
#include "nsp/object-inlined.h"
#endif

/**
 * check_implements:
 * @obj: any object to be checked 
 * @id: an type instance id.
 * 
 * checks that object @o implements an interface 
 * type with @id signature
 * Return value: a #NspTypeBase which contains the 
 * interface instance for object @obj or %NULL.
 **/

NspTypeBase *check_implements(const void *obj,NspTypeId id)
{
  const NspObject *ob=obj;
  /* down to basetype */
  NspTypeBase *type = ob->basetype;
  /* now walk up and search interfaces */
  while ( type != NULL) 
    {
      NspTypeBase *ob_interf = type->interface;
      while ( ob_interf  != NULL) 
	{
	  if ( ob_interf->id == id ) return ob_interf;
	  ob_interf = ob_interf->interface;
	}
      type = type->surtype;
    }
  return NULL;
}

/**
 * object_size:
 * @self: a #NspObject. 
 * @flag: an int for selecting size to be returned.
 * 
 * a default size method which always returns 0.
 * This method is redefined in each concret class.
 * 
 * Return value: an integer.
 **/
static int object_size(NspObject *self, int flag)
{
  return 0;
}

static char object_type_name[]="Object";
static char object_short_type_name[]="obj";

/**
 * object_type_as_string:
 * 
 * a unique identifier for #NspObject objects 
 * as a short string or long string. 
 * This method is redefined for each concrete class 
 * which inherits from #NspObject.
 * 
 * Return value: a string 
 **/

static char *object_type_as_string(void)
{
  return(object_type_name);
}

static char *object_type_short_string(NspObject *obj)
{
  return(object_short_type_name);
}

/**
 * set_name:
 * @ob: a #NspObject 
 * @name: a string 
 * 
 * sets the name ob object #NspObject. Previous name 
 * is destroyed if it was not NVOID.
 * 
 * Return value: returns a pointer to the name or %NULLSTRING.
 **/

static const char named_void[]="";
#ifdef USE_CHUNKS 
static GStringChunk *obj_names=NULL;
#endif 

static const char *set_name(NspObject *ob,const char *name)
{
  const char *name1 = named_void;
#ifdef USE_CHUNKS 
  if ( name[0] !='\0' ) 
    {
      if (( name1 =g_string_chunk_insert_const(obj_names,name)) == NULLSTRING)
	return NULLSTRING;
    }
#else 
  if ( name[0] !='\0' ) 
    {
      if (( name1 =new_nsp_string(name)) == NULLSTRING)
	return NULLSTRING;
    }
  if (ob->name != NULL && ob->name != named_void) FREE(ob->name) ;
#endif 
  return ob->name = name1;
}

/**
 * nsp_object_set_initial_name:
 * @ob: a #NspObject 
 * @name: a string 
 * 
 * sets the name of object #NspObject. This function is to be called
 * the first time the name of object is set. 
 * If the name is NVOID then the string is not allocated but a pointer 
 * to a shared value is returned.
 * 
 * Return value: returns a pointer to the name or %NULLSTRING.
 **/

const char *nsp_object_set_initial_name(NspObject *ob,const char *name)
{
  const char *name1 = named_void;
#ifdef USE_CHUNKS 
  static int init=0;
  if ( init == 0 )
    {
      obj_names = g_string_chunk_new(1024);
      init =1;
    }
  if ( name[0] !='\0' ) 
    {
      if ((name1 = g_string_chunk_insert_const(obj_names,name)) == NULLSTRING)
	return NULLSTRING;
    }
#else 
  if ( name[0] !='\0' ) 
    {
      if (( name1 =new_nsp_string(name)) == NULLSTRING)
	return NULLSTRING;
    }
#endif 
  return ob->name = name1;
}

/**
 * nsp_object_destroy_name:
 * @ob: a #NspObject 
 * 
 * free the memory used by object name. Note that 
 * a direct call to free is not good since unnamed 
 * objects share the same empty name. 
 **/

void nsp_object_destroy_name(NspObject *ob)
{
#ifndef USE_CHUNKS 
  if ( ob->name[0] !='\0' )  FREE(ob->name);
#endif
}

/**
 * get_name:
 * @ob:  a #NspObject 
 * 
 * gets the name of object @ob.
 * 
 * Return value: a string. 
 **/

static const char *get_name(NspObject *ob) 
{
  return ob->name;
}

/**
 * object_is_true_def:
 * @self: a #NspObject.
 * 
 * can be redefined for each concrete class 
 * which inherits from #NspObject. It is used 
 * in if A then to check if A can be considered as 
 * a %TRUE value. The default method implemented 
 * here returns %FALSE which an error message at 
 * nsp level.
 * 
 * Return value: %FALSE.
 **/

static int object_is_true_def(NspObject *self)
{
  Scierror("Error: is_true not implemented for value of type %s\n",
	   self->type->s_type());  
  return FALSE;
}

/**
 * object_loop_def:
 * @str: a string 
 * @O: a #NspObject 
 * @O1: a #NspObject 
 * @i: an integer 
 * @rep: int pointer 
 * 
 * can be redefined for each concrete class 
 * which inherits from #NspObject. 
 * Default method for the loop iterator if x=A 
 * to iterate throught the columns of A.
 * The default method implemented 
 * here returns %FALSE which an error message at 
 * nsp level.
 * 
 * Return value: %NULLOBJ.
 **/
static NspObject *object_loop_def(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  Scierror("Error: %s=val not implemented for val of type %s\n",
	   str,O1->type->s_type());
  return NULLOBJ;
}


/*
 * save and load when not redefined locally 
 */

/**
 * nsp_object_save_def:
 * @F: a void pointer 
 * @M: a nsp object 
 * 
 * This is the default handler for saving a nsp object in 
 * a file. Each class has to redefine this function.
 * 
 * Returns: %FAIL
 **/
int  nsp_object_save_def(void * F, NspObject * M)
{
  Scierror("Error: save not implemented for objects of type %s\n",M->type->s_type());
  return FAIL;
}


/**
 * nsp_object_load_def:
 * @F: a void pointer 
 * 
 * This is the default handler for loading a nsp object from 
 * a file. Each class has to redefine this function.
 * 
 * Returns: %NULL
 **/

NspObject *nsp_object_load_def(void  * F)
{
  Scierror("Error: should not get there, trying to load an object without a load method\n");
  return NULL;
}

/**
 * nsp_object_latex_def:
 * @M: an object 
 * @indent: an integer 
 * @name: a string or %NULL
 * @rec_level: an integer 
 * 
 * default function for printing an object in latex syntax.
 * Each class has to properly redefine this function.
 *
 **/

static void nsp_object_latex_def(NspObject * M, int indent,char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : M->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[\n");
  if ( strcmp(pname,NVOID) != 0) 
    Sciprintf("%s : \\mbox{latex print not implemented for %s}\n",pname,M->type->s_type());
  else 
    Sciprintf("\\mbox{latex print not implemented for %s}\n",M->type->s_type());
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}

/**
 * nsp_object_as_index_def:
 * @M: an object 
 * @index: an #index_vector
 * 
 * default function for checking if object can be used as an index 
 * vector 
 *
 * Return value: %TRUE or %FALSE 
 **/


static int nsp_object_as_index_def(NspObject * M, index_vector *index)
{
  index->error = index_wrong_object;
  Scierror("Error: object of type %s cannot be used as index vector\n",M->type->s_type());
  return FAIL;
}

/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/

/**
 * int_object_create:
 * @stack: a #Stack
 * @rhs: an int the number of right hand side arguments 
 * @opt: the number of optional named arguments 
 * @lhs: the requested number of arguments to return 
 * 
 * deprecated ? 
 * A defaut interface for create method at nsp level. 
 * This method can be redefined for certain types instance 
 * in order to give a defaut create method.
 * 
 * Return value: 
 **/

static int int_object_create(Stack stack, int rhs, int opt, int lhs)
{
  Scierror("Cannot create object \n");
  return RET_BUG;
}

/* set method common to all objects object.set[attr=val,attr=val,....] */

/**
 * int_meth_object_equal:
 * @self: an instance of a nsp object.
 * @stack: a #Stack
 * @rhs: an int the number of right hand side arguments 
 * @opt: the number of optional named arguments 
 * @lhs: the requested number of arguments to return 
 * 
 * a nsp method for checking object equality i.e checks if 
 * @self is a copy of the first object stored in the stack @stack.
 * x.equal[y]. The answer is stored in the calling stack @stack.
 * 
 * Return value: 1 
 **/

static int int_meth_object_equal(void *self,Stack stack,int rhs,int opt,int lhs)
{
  int rep;
  NspObject *O;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /* nsp_get_object takes care of Hobj pointers **/
  if (( O =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  rep = NSP_OBJECT(self)->type->eq(self,O);
  nsp_move_boolean(stack,1,rep);
  return 1;
}

static int int_meth_object_not_equal(void *self,Stack stack,int rhs,int opt,int lhs)
{
  int rep;
  NspObject *O;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /* nsp_get_object takes care of Hobj pointers **/
  if (( O =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  rep = NSP_OBJECT(self)->type->neq(self,O);
  nsp_move_boolean(stack,1,rep);
  return 1;
}

/**
 * int_object_get_name:
 * @self: an instance of a nsp object.
 * @stack: a #Stack
 * @rhs: an int the number of right hand side arguments 
 * @opt: the number of optional named arguments 
 * @lhs: the requested number of arguments to return 
 * 
 * a nsp method for getting the name of @self. 
 * The answer is stored in the calling stack @stack.
 * 
 * Return value: 1 or %RET_BUG.
 **/

static int int_meth_object_get_name(void *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(-1,0);
  CheckLhs(1,1);
  /* nsp_get_object takes care of Hobj pointers **/
  if ( NSP_OBJECT(self)->name == NULL ) 
    {
      Scierror("Error: given object has no name !\n");
      return RET_BUG;
    }
  nsp_move_string(stack,1,NSP_OBJECT(self)->name,-1);
  return 1;
}


/**
 * int_object_protect:
 * @self: an instance of a nsp object.
 * @stack: a #Stack
 * @rhs: an int the number of right hand side arguments 
 * @opt: the number of optional named arguments 
 * @lhs: the requested number of arguments to return 
 * 
 * a nsp method for getting the flag field of @self. 
 * 
 * Return value: 1 or %RET_BUG.
 **/

static int int_meth_object_protect(void *self,Stack stack,int rhs,int opt,int lhs)
{
  int protect; 
  CheckRhs(0,1);
  CheckLhs(1,1);
  if ( rhs == 1 ) 
    {
       if ( GetScalarBool (stack,1,&protect) == FAIL) return RET_BUG;
       NSP_OBJECT(self)->flag = protect;
    }
  else 
    {
      protect = NSP_OBJECT(self)->flag;
    }
  nsp_move_boolean(stack,1,protect);
  return 1;
}


/**
 * int_meth_object_set_attributes:
 * @ob: a nsp object 
 * @stack: evaluation stack 
 * @rhs: an integer 
 * @opt: an integer 
 * @lhs: an integer 
 * 
 * This interface is called when the set method 
 * is activated ob.set[ attr1=val1, attr2 = val2 ,....]
 * 
 * Returns: 0 
 **/

static int int_meth_object_set_attributes(void *ob,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ob=ob;
  int i;
  if ( rhs - opt > 0 ) 
    {
      Scierror("%s only accept optional arguments \n",NspFname(stack));
      return RET_BUG;
    }
  CheckLhs(1,1); 
  for ( i = 1 ; i <= rhs ; i++) 
    {
      NspObject *val = ((NspHobj *) NthObj(i))->O;
      if ( nsp_set_attribute_util(Ob,Ob->basetype,NthObj(i)->name,val) == FAIL) return RET_BUG;
    }
  return 0;
}

 

/**
 * int_meth_object_get_attributes:
 * @ob: a nsp object 
 * @stack: evaluation stack 
 * @rhs: an integer 
 * @opt: an integer 
 * @lhs: an integer 
 * 
 * This interface is called when a get method 
 * is activated. ob.get[smat1,smat2,...] and the function 
 * returns as many object as requested by given arguments.
 * 
 * Returns: an integer  
 **/

int int_meth_object_get_attributes(void *ob,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ob=ob,*Ret;
  NspSMatrix *S;
  int i,j,count=0;
  CheckRhs(1,1000);
  CheckLhs(1,1000);
  lhs=Max(lhs,1);
  for ( j = 1 ; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	{
	  Ret = nsp_get_attribute_util(Ob,Ob->basetype,S->S[i]);
	  if ( Ret == NULL) return RET_BUG;
	  NthObj(rhs+ ++count) = Ret ;
	  NSP_OBJECT(Ret)->ret_pos = count;
	  if (count == lhs) break;
	}
      if (count == lhs) break;
    }
  return count;
}


/**
 * int_meth_object_get_attribute_names:
 * @ob: a nsp object 
 * @stack: evaluation stack 
 * @rhs: an integer 
 * @opt: an integer 
 * @lhs: an integer 
 * 
 * This interface is called when the get_attribute_names method 
 * is activated ob.get_attribute_names[]
 * 
 * Returns: 1 or %RET_BUG.
 **/

static int int_meth_object_get_attribute_names(void *ob,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ob=ob,*Res;
  CheckRhs(0,0);
  if ((Res= nsp_get_attribute_util(Ob,Ob->basetype,"__attrs"))== NULL)
    return RET_BUG;  
  MoveObj(stack,1,Res);
  return 1;
}

/**
 * nsp_get_methods:
 * @ob: a nsp object 
 * @type: a nsp type 
 * 
 * get available methods in the given type. The @ob
 * argument is not used. 
 * 
 * 
 * Returns: a string matrix 
 **/

static NspSMatrix *nsp_get_methods(NspObject *ob,NspTypeBase *type,int level)
{
  int cu_level = 0;
  NspMethods *methods;
  NspSMatrix *sm=NULLSMAT,*sm1;
  NspTypeBase *interf ;
  /* build a string matrix with all methods */
  while ( type != NULL) 
    {
      if ( level >= 0 && cu_level !=  level)
	{
	  type = type->surtype;cu_level++;continue;
	}
      methods = (type->methods != NULL) ? type->methods(): NULL;
      /* return attributes as a String Matrix */
      if ( methods != NULL)
	{
	  if ( ( sm1 =nsp_smatrix_create_from_struct(NVOID,methods,sizeof(NspMethods))) == NULLSMAT) return NULL;
	  sm1->n=sm1->m;sm1->m=1;/* transpose vector */
	  if ( sm != NULL) 
	    {
	      if (nsp_smatrix_concat_right(sm, sm1) == FAIL) return NULLSMAT;
	      nsp_smatrix_destroy(sm1);
	    }
	  else 
	    {
	      sm=sm1;
	    }
	}
      /* explore interfaces */
      interf = type->interface;
      while ( interf != NULL) 
	{
	  methods = (interf->methods != NULL) ? interf->methods(): NULL;
	  if ( methods != NULL ) 
	    {
	      if ( ( sm1 =nsp_smatrix_create_from_struct(NVOID,methods,sizeof(NspMethods))) == NULLSMAT) return NULL;
	      sm1->n=sm1->m;sm1->m=1;/* transpose vector */
	      if ( sm != NULL) 
		{
		  if (nsp_smatrix_concat_right(sm, sm1) == FAIL) return NULLSMAT;
		  nsp_smatrix_destroy(sm1);
		}
	      else 
		{
		  sm=sm1;
		}

	    }
	  interf = interf->interface;
	} 
      type = type->surtype;
      cu_level++;
    }
  if ( sm == NULL) 
    {
      if (( sm =nsp_smatrix_create(NVOID,0,0,NULL,0))  == NULLSMAT) return NULLSMAT;
    }
  else
    {
      sm->m=sm->n,sm->n=1;
    }
  return sm;
}



/**
 * int_meth_object_get_methods:
 * @ob: a nsp object 
 * @stack: evaluation stack 
 * @rhs: an integer 
 * @opt: an integer 
 * @lhs: an integer 
 * 
 * This interface is called when the get_method_names method 
 * is activated ob.get_method_names[] or ob.get_method_names[level];
 * Whith no arguments we obtain all the method names that can be used on 
 * object @ob. When level is given the methods of the base class is given 
 * for level=0 then the methods of father class for level=1 and so on. 
 * 
 * Returns: 1 or %RET_BUG.
 **/

static int int_meth_object_get_methods(void *ob,Stack stack, int rhs, int opt, int lhs)
{
  int level = -1;
  NspObject *Ob=ob;
  NspSMatrix *S;
  CheckRhs(0,1);
  if ( rhs==1) 
    {
      if (GetScalarInt(stack,1,&level) == FAIL) return RET_BUG;
    }
  if ((S = nsp_get_methods(Ob,Ob->basetype,level)) == NULL) 
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}


static NspMethods object_methods[] = {
  { "set",  int_meth_object_set_attributes}, /* set attribute of object the get is given by . */
  { "get",  int_meth_object_get_attributes},  /* get attribute is also given by . */
  { "get_name", int_meth_object_get_name},
  { "get_method_names", int_meth_object_get_methods},
  { "get_attribute_names", int_meth_object_get_attribute_names},
  { "equal",  int_meth_object_equal},
  { "not_equal",  int_meth_object_not_equal},
  { "protect",  int_meth_object_protect},
  { (char *) 0, NULL}
};

static NspMethods *object_get_methods(void) { return object_methods;};

/*---------------------------------------------------
 * set of function for dealing with  object attributes
 *--------------------------------------------------*/

/* default interface for set or get */ 

NspObject * int_get_failed(NspObject *self, char *attr)
{
  Scierror("set attribute %s for type %s not implemented\n",attr,self->type->s_type());
  return NULL;
}

NspObject * int_get_object_failed(NspObject *self, char *attr, int *copy)
{
  Scierror("get attribute %s (as object) for type %s not implemented\n",attr,self->type->s_type());
  return NULL;
}

NspObject * int_set_object_failed(NspObject *self, NspObject *val)
{
  Scierror("set attribute %s (as object) for type %s not implemented\n",
	   nsp_object_get_name(NSP_OBJECT(val)),self->type->s_type());
  return NULL;
}

int int_set_failed(NspObject *self,char *attr, NspObject *val)
{
  Scierror("set attribute %s for type %s not implemented\n",attr,self->type->s_type());
  return FAIL;
}

/**
 * int_set_attribute:
 * @stack: a #Stack
 * @rhs: an int the number of right hand side arguments 
 * @opt: the number of optional named arguments 
 * @lhs: the requested number of arguments to return 
 * 
 * an interface which is used when settin a nsp object attribute 
 * in R.exp = b expressions.
 *
 * Return value: 1 or %RET_BUG.
 **/

int int_set_attribute(Stack stack, int rhs, int opt, int lhs)
{
  char *attr;
  NspObject *ob;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((ob =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  if ((attr = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ( nsp_set_attribute_util(ob,ob->basetype,attr,NthObj(3)) == FAIL) return RET_BUG;
  NthObj(1)->ret_pos = 1;
  return 1;
}

/*
 * ob.set[ attr1=val1, attr2 = val2 ,....]
 * FIXME ? obsolete and replaced by the next one 
 */

int int_set_attributes(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspObject *ob;
  if ((ob =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  if ( rhs - opt > 1 ) 
    {
      Scierror("%s only accept optional arguments \n",NspFname(stack));
      return RET_BUG;
    }
  CheckLhs(1,1); 
  for ( i = rhs-opt+1 ; i <= rhs ; i++) 
    {
      NspObject *val = ((NspHobj *) NthObj(i))->O;
      if ( nsp_set_attribute_util(ob,ob->basetype,NthObj(i)->name,val) == FAIL) return RET_BUG;
    }
  NthObj(1)->ret_pos = 1;
  return 1;
}

/**
 * int_create_with_attributes:
 * @ob: a nsp object 
 * @stack: evaluation stack 
 * @rhs: an integer 
 * @opt: an integer 
 * @lhs: an integer 
 * 
 * utility function that can be used in the constructor of a class
 * to walk through optional named arguments and use them to initialize 
 * fields of an object.
 * 
 * Returns: 1 or %RET_BUG
 **/

int int_create_with_attributes(NspObject *ob,Stack stack, int rhs, int opt, int lhs)
{
  int i;
  if ( rhs - opt > 1 ) 
    {
      Scierror("%s only accept optional arguments \n",NspFname(stack));
      return RET_BUG;
    }
  CheckLhs(1,1); 
  for ( i = rhs-opt+1 ; i <= rhs ; i++) 
    {
      NspObject *val = ((NspHobj *) NthObj(i))->O;
      if ( nsp_set_attribute_util(ob,ob->basetype,NthObj(i)->name,val) == FAIL) return RET_BUG;
    }
  return 1;
}

/**
 * nsp_set_attribute_util:
 * @ob: an object 
 * @type: a type 
 * @attr: a string giving an attribute name 
 * @val: a nsp object.
 * 
 * set the field @attr of object @ob with value @val. If the 
 * field @attr is not a correct field then %FAIL is returned. 
 * Note that most of the time this function is called with 
 * type set to ob->basetype.
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_set_attribute_util(NspObject *ob, NspTypeBase *type,const char *attr,NspObject *val)
{
  AttrTab *attrs;
  int item,ok=0;
  while (type != NULL)
    {
      attrs = type->attrs ;
      if ( attrs != NULL &&  (item=attr_search(attr,attrs)) >=0 )
	{
	  if ( attrs[item].set(ob,attr,val) == FAIL) 
	    {
	      return FAIL;
	    }
	  else 
	    {
	      ok=1;
	      break;
	    }
	}
      type = type->surtype;
    }
  if ( ok == 0) 
    {
      Scierror("Error: attribute %s does not exists for instance of %s\n",
	       attr,
	       ob->type->s_type());
      return FAIL;
    }
  return OK;
}

/**
 * int_get_attribute:
 * @stack: evaluation stack 
 * @rhs: an integer 
 * @opt: an integer 
 * @lhs: an integer 
 * 
 * This interface is called when trying to get an attribute value 
 * through the use of the dot operator: ob.attr
 * 
 * Returns: 1 or %RET_BUG.
 **/

int int_get_attribute(Stack stack, int rhs, int opt, int lhs)
{
  char *attr;
  NspObject *ob;
  NspTypeBase *type;
  CheckRhs(2,100); /* XXXXXX */
  CheckLhs(-1,1);
  if ((ob =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  type = ob->basetype;
  if ((attr = GetString(stack,2)) == (char*)0) return RET_BUG;  
  ob = nsp_get_attribute_util(ob,ob->basetype,attr);
  if ( ob == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,ob);
  return 1;
}

/**
 * nsp_get_attribute_util:
 * @ob: an object 
 * @type: a type 
 * @attr: a string 
 * 
 * returns if it exists the field @attr of object @ob. 
 * fields are searched in the given type. Note that most 
 * of the time @type is set to ob->basetype when calling this 
 * function. Note also that this function returns a copy of the 
 * field object.
 * 
 * Returns: a #NspObject. or %NULLOBJ.
 **/

NspObject *nsp_get_attribute_util(NspObject *ob,NspTypeBase *type,const char *attr) 
{
  int item; 
  AttrTab *attrs;
  NspSMatrix *sm=NULLSMAT,*sm1;
  /* Check now if key is an attribute of object Object */
  if ( strcmp(attr,"__attrs")==0) 
    {
      /* build a string matrix with all attributes */
      while ( type != NULL) 
	{
	  attrs = type->attrs ;
	  /* return attributes as a String Matrix */
	  if ( attrs != NULL)
	    {
	      if ( ( sm1 =nsp_smatrix_create_from_struct(NVOID,attrs,sizeof(AttrTab)) ) == NULLSMAT) return NULL;
	      sm1->n=sm1->m;sm1->m=1;/* transpose vector */
	      if ( sm != NULL) 
		{
		  if (nsp_smatrix_concat_right(sm, sm1) == FAIL) return NULL;
		  nsp_smatrix_destroy(sm1);
		}
	      else 
		{
		  sm=sm1;
		}
	    }
	  type = type->surtype;
	}
      if ( sm == NULL) 
	{
	  if (( sm =nsp_smatrix_create(NVOID,0,0,NULL,0))  == NULLSMAT) return NULL;
	}
      else
	{
	  sm->m=sm->n,sm->n=1;
	}
      return (NspObject *) sm;
    }
  else
    {
      while ( type != NULL) 
	{
	  if ((attrs = type->attrs) != NULL)
	    {
	      if (( item=attr_search(attr,attrs)) >=0 )
		{
		  return attrs[item].get(ob,attr);
		}
	    }
	  type = type->surtype;
	}

      Scierror("Error: attribute %s not found for instances of %s\n",attr,ob->type->s_type());
    }
  return NULL ;
}



/**
 * object_path_extract:
 * @a: a nsp object
 * @n: an integer
 * @ob: a pointer to a string object. 
 * 
 * This function is used when a field of an object is 
 * requested for modification more complex than the simple 
 * affectation. For example in obj.attr(4,5) = 7.
 * 
 * Returns: a #NspObject. 
 *
 **/

NspObject *object_path_extract(NspObject *a,int n, NspObject **ob, int *copy)
{
  char *str;
  *copy= FALSE;
  if ( n != 1 ) return NULLOBJ;
  if ((str=nsp_string_object(*ob)) == NULL ) return NULLOBJ;
  return nsp_get_attribute_object((NspObject *) a,((NspObject *)a)->basetype,str,copy) ;
}


/**
 * nsp_get_attribute_object:
 * @ob: an object 
 * @type: a type 
 * @attr: a string 
 * 
 * utility function for #object_path_extract or similar functions. 
 * Returns the field @attr for object @ob for modifications by calling 
 * the get_object method of the object class. 
 * 
 * Returns: a #NspObject. 
 **/

NspObject *nsp_get_attribute_object(NspObject *ob,NspTypeBase *type,const char *attr, int *copy) 
{
  int item; 
  AttrTab *attrs;
  while ( type != NULL) 
    {
      if (( attrs = type->attrs) != NULL)
	{
	  if (( item=attr_search(attr,attrs)) >=0 )
	    {
	      return (attrs[item].get_object != NULL) ? attrs[item].get_object(ob,attr,copy): NULLOBJ;
	    }
	}
      type = type->surtype;
    }
  return NULLOBJ ;
}


/**
 * nsp_get_attribute_object:
 * @ob: an object 
 * @type: a type 
 * @val: an object
 * 
 * utility function which is used to set a field of object @ob to value @val. 
 * The name of the field is given by the name of @val. This function is used after 
 * an object has been selected for modification by object_path_extract with a copy 
 * flag set to %TRUE. A copy of the attribute value is modified and at the end 
 * nsp_set_attribute_object is called to perform verifications before setting the 
 * attribute. 
 * 
 * Returns: %OK or %FAIL
 **/

int nsp_set_attribute_object(NspObject *ob,NspTypeBase *type,NspObject *val)
{
  int item; 
  AttrTab *attrs;
  const char *attr = nsp_object_get_name(val);
  while ( type != NULL) 
    {
      if (( attrs = type->attrs) != NULL)
	{
	  if (( item=attr_search(attr,attrs)) >=0 )
	    {
	      return (attrs[item].set_object != NULL) ? attrs[item].set_object(ob,val): OK;
	    }
	}
      type = type->surtype;
    }
  return OK ;
}


/*---------------------------------------------------
 * set of function for dealing with  object methods 
 *--------------------------------------------------*/

/*
 * the first stack element is the object the method is to be applied to (==ob)
 * type is the Type in which methods are to be searched (ob->basetype most of the time)
 * methods are searched from bottom to top and at each level implemented interfaces are 
 * searched too 
 */

int nsp_exec_method_util(NspObject *ob,NspTypeBase *type,char *method, Stack stack, int rhs, int opt, int lhs)
{
  NspMethods *methods;
  int item;
  while (type != NULL)
    {
      NspTypeBase *interf = type->interface;
      /* explore methods */
      methods = (type->methods != NULL) ? type->methods(): NULL;
      if ( methods != NULL &&  (item=method_search(method,methods)) >=0 )
	{
	  /* execute the method */
	  return  methods[item].meth(ob,stack,rhs,opt,lhs);
	}
      /* explore interfaces */
      while ( interf != NULL) 
	{
	  methods = (interf->methods != NULL) ? interf->methods(): NULL;
	  if ( methods != NULL &&  (item=method_search(method,methods)) >=0 )
	    return  methods[item].meth(ob,stack,rhs,opt,lhs);
	  interf = interf->interface;
	} 
      type = type->surtype;
    }
  Scierror("Warning: method %s does not exists for instance of %s\n",method, ob->type->s_type());
  return RET_BUG;
}


/*---------------------------------------------------
 * set of interfaced functions 
 *--------------------------------------------------*/

/*
 * interface for operator \n 
 * used when diplaying the result of an evaluation
 * x \n  or  
 */

int int_object_ret(Stack stack, int rhs, int opt, int lhs)
{
  NspObject **Ob = stack.val->S + stack.first;
  int i;
  for ( i= 0 ; i < rhs ; i++) 
    {
      (*Ob)->type->pr(*Ob,0,NULL,0);
      Ob++;
    }
  return 0;
}

/*
 * interface for operator , 
 * used when diplaying the result of an evaluation
 * x ,   or  
 */

int int_object_virg(Stack stack, int rhs, int opt, int lhs)
{
  return int_object_ret(stack,rhs,opt,lhs) ;
}

/*
 * interface for operator ; 
 * x ;
 */

int int_object_pvirg(Stack stack, int rhs, int opt, int lhs)
{
  /* default job is to ignore */
  return 0;
}

/*
 * type(obj , 'short'|'string' ) return the type of object obj 
 *    Note that if obj is a pointer the type of object it points to 
 *    is returned 
 */

int int_object_type(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,2);
  CheckLhs(0,1);
  NspObject *Ob, *ret=NULL;
  if ((Ob =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if (rhs == 2 ) 
    {
      int mode;
      char *mode_Table[] = {  "short", "string", NULL};
      if ((mode= GetStringInArray(stack,2,mode_Table,1)) == -1) return RET_BUG; 
      switch (mode )
	{
	case 0: if ((ret =nsp_create_object_from_str(NVOID,(Ob)->type->sh_type(Ob)))== NULLOBJ) return RET_BUG;break;
	case 1: if ((ret =nsp_create_object_from_str(NVOID,(Ob)->type->s_type()))== NULLOBJ) return RET_BUG;break;
	}
    }
  else 
    {
      if ((ret = (NspObject *) type_create(NVOID,Ob->basetype,NULL))== NULLOBJ) return RET_BUG; 
    }
  MoveObj(stack,1,ret);
  return 1;
}  

/*
 * is( obj , type ) 
 *   check if obj is of type type or is a subtype of type 
 */

int int_object_is(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ob;
  NspType *type;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((Ob =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ((type = GetType(stack,2))== NULLTYPE) return RET_BUG; 
  if ( nsp_move_boolean(stack,1, check_cast(Ob, type->nsp_type->id ))== FAIL) return RET_BUG;
  return 1;
}

/*
 * implements( obj , type ) 
 *   check if obj implements type type. Where type is the 
 *   type of an interface.
 */

int int_object_implements(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Ob;
  NspType *type;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((Ob =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ((type = GetType(stack,2))== NULLTYPE) return RET_BUG; 
  if ( nsp_move_boolean(stack,1, check_implements(Ob, type->nsp_type->id ) != NULL )== FAIL) return RET_BUG;
  return 1;
}



/*
 * info(obj) 
 * info on object. 
 * 
 */

int int_object_info_obsolete(Stack stack, int rhs, int opt, int lhs)
{
  int dp=user_pref.pr_depth;
  int at=user_pref.list_as_tree;
  int depth=INT_MAX,indent=0,tree=FALSE;
  char *name = NULL;
  nsp_option opts[] ={{ "depth", s_int,NULLOBJ,-1},
		      { "indent",s_int,NULLOBJ,-1},
		      { "name",string,NULLOBJ,-1},
		      { "tree",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspObject *object;
  CheckStdRhs(1,1);
  CheckLhs(0,1);
  if ((object =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ( get_optional_args(stack, rhs, opt, opts,&depth,
			 &indent,&name,&tree) == FAIL) 
    return RET_BUG;
  user_pref.pr_depth= depth;
  user_pref.list_as_tree= tree;
  object->type->info(object,indent,name,0);
  user_pref.pr_depth= dp;
  user_pref.list_as_tree= at;
  return 0;
}


/* generic function for printing objects and 
 * redirection of output to string, file or stdout
 */

typedef enum { string_out, stdout_out, file_out } print_mode; 

static int int_object_print_gen(Stack stack, int rhs, int opt, int lhs, print_mode mode, int info_only)
{
  NspFile *F=NULL;
  FILE *f=NULL;
  IOVFun def=NULL ;
  MoreFun mf=NULL; 
  
  NspObject *res, *object;
  print_func *pr;
  int dp=user_pref.pr_depth;
  int at=user_pref.list_as_tree;
  int as_read=FALSE,latex=FALSE,table=FALSE,depth=INT_MAX,indent=0,tree=FALSE;
  char *name = NULL;
  nsp_option print_opts[] ={{ "as_read",s_bool,NULLOBJ,-1},
			    { "depth", s_int,NULLOBJ,-1},
			    { "indent",s_int,NULLOBJ,-1},
			    { "latex",s_bool,NULLOBJ,-1},
			    { "name",string,NULLOBJ,-1},
			    { "table",s_bool,NULLOBJ,-1},
			    { NULL,t_end,NULLOBJ,-1}};

  nsp_option info_opts[] ={{ "depth", s_int,NULLOBJ,-1},
			   { "indent",s_int,NULLOBJ,-1},
			   { "name",string,NULLOBJ,-1},
			   { "tree",s_bool,NULLOBJ,-1},
			   { NULL,t_end,NULLOBJ,-1}};

  if ( mode == file_out ) 
    {
      CheckStdRhs(2,2);
      if ((F= GetSciFile(stack,1))== NULL) return RET_BUG; 
      if ((object =nsp_get_object(stack,2))== NULLOBJ) return RET_BUG; 

    }
  else 
    {
      CheckStdRhs(1,1);
      if ((object =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
    }
  CheckLhs(0,1);

  if (info_only == TRUE ) 
    {
      if ( get_optional_args(stack, rhs, opt, info_opts,&depth,
			     &indent,&name,&tree) == FAIL) 
	return RET_BUG;
    }
  else 
    {
      if ( get_optional_args(stack, rhs, opt, print_opts,&as_read,&depth,
			     &indent,&latex,&name,&table) == FAIL) 
	return RET_BUG;
    }

  /* initialize according to mode */
  switch ( mode ) 
    {
    case string_out: 
      def = SetScilabIO(Sciprint2string);
      mf =nsp_set_nsp_more(scimore_void);
      break;
    case stdout_out:
      break;
    case file_out : 
      /* changes io in order to write to file F */
      if ( !IS_OPENED(F->obj->flag))
	{
	  Scierror("Warning:\tfile %s is already closed\n",F->obj->fname);
	  return RET_BUG;
	}
      f=Sciprint_file(F->obj->file); 
      def = SetScilabIO(Sciprint2file);
      mf =nsp_set_nsp_more(scimore_void);
      break;
    }
  /* print object */
  user_pref.pr_depth= depth;
  user_pref.list_as_tree=tree;
  pr = ( latex == TRUE) ?  object->type->latex :  object->type->pr ;
  if (info_only == TRUE ) pr = object->type->info;

  if ( as_read == TRUE ) 
    {
      int kp=user_pref.pr_as_read_syntax;
      user_pref.pr_as_read_syntax= 1;
      if ( latex == TRUE ) 
	{
	  Sciprintf("Warning: you cannot select both as_read and latex, latex ignored\n");
	}
      pr(object,indent,name,0);
      user_pref.pr_as_read_syntax= kp;
      user_pref.pr_depth= dp;
      user_pref.list_as_tree=at;
    }
  else 
    {
      pr(object,indent,name,0);
    }
  user_pref.pr_depth= dp;
  user_pref.list_as_tree=at;
  /* restore to default values */
  switch ( mode ) 
    {
    case string_out: 
      res = Sciprint2string_reset(); 
      SetScilabIO(def);
      nsp_set_nsp_more(mf);
      if ( res == NULL) return RET_BUG; 
      MoveObj(stack,1, res);
      return 1;
    case stdout_out: 
      return 0;
    case file_out:
      SetScilabIO(def);
      nsp_set_nsp_more(mf);
      Sciprint_file(f); 
      return 0;
    }
  return 0;
}

/*
 *   display object using it's standard print function 
 */

static int int_object_print(Stack stack, int rhs, int opt, int lhs)
{
  return int_object_print_gen(stack,rhs,opt,lhs,stdout_out,FALSE);
}

/*
 *   display object using it's standard print function 
 *   and redirect output to a string matrix 
 */

static int int_object_sprint(Stack stack, int rhs, int opt, int lhs)
{
  return int_object_print_gen(stack,rhs,opt,lhs,string_out,FALSE);
}

/*
 *   display object using it's standard print function 
 *   and redirect output to a file
 */

static int int_object_fprint(Stack stack, int rhs, int opt, int lhs)
{
  return int_object_print_gen(stack,rhs,opt,lhs,file_out,FALSE);
}
/*
 *   display object using it's standard print function 
 */

static int int_object_info(Stack stack, int rhs, int opt, int lhs)
{
  return int_object_print_gen(stack,rhs,opt,lhs,stdout_out,TRUE);
}

/*
 *   display object using it's standard print function 
 *   and redirect output to a string matrix 
 */

static int int_object_sinfo(Stack stack, int rhs, int opt, int lhs)
{
  return int_object_print_gen(stack,rhs,opt,lhs,string_out,TRUE);
}

/*
 *   display object using it's standard print function 
 *   and redirect output to a file
 */

static int int_object_finfo(Stack stack, int rhs, int opt, int lhs)
{
  return int_object_print_gen(stack,rhs,opt,lhs,file_out,TRUE);
}

/*
 *
 */

static int int_object_diary(Stack stack, int rhs, int opt, int lhs)
{
  static NspFile *F= NULL;
  static IOVFun def = NULL;
  CheckRhs(0,2);
  CheckLhs(0,1);
  if ( rhs >= 1) 
    {
      int diary_echo = TRUE;
      char *fname;
      if ((fname= GetString(stack,1))== NULL) return RET_BUG; 
      if ((F=nsp_file_open(fname,"w",FALSE,FALSE))== NULL) return RET_BUG;
      if (rhs >= 2 ) 
	{
	  if ( GetScalarBool (stack,2,&diary_echo) == FAIL) return RET_BUG;
	}
      /* changes io in order to write to file F */
      Sciprint_set_diary(F->obj->file,diary_echo);
      def = SetScilabIO(Sciprint_diary);
    }
  else 
    {
      /* end of diary */
      if ( F != NULL) 
	{ 
	  nsp_file_close(F);
	  nsp_file_destroy(F);
	  F=NULL;
	}
      Sciprint_set_diary(NULL,TRUE);
      if ( def != NULL) SetScilabIO(def);
      def = NULL;
    }
  return 0;
}


/*
 * Nsp printf(format,....) function 
 */

int print_count_rows(Stack stack,int first_arg,int last_arg)
{
  NspObject *obj=NULL; 
  int i, rows=0;
  if ( first_arg <= last_arg )
    {
      if ((obj =nsp_get_object(stack,first_arg))== NULL) return FAIL;
      rows =nsp_object_get_size(obj,1);
    }
  for ( i= first_arg +1 ; i <= last_arg ; i++) 
    {
      if ((obj =nsp_get_object(stack,i))== NULL) return FAIL;
      rows =Min(rows,nsp_object_get_size(obj,1));
    }
  return rows;
}

int int_object_printf(Stack stack, int rhs, int opt, int lhs)
{
  int i=0,rows;
  char *Format;
  if ( rhs < 1 ) 
    { Scierror("Error:\tRhs must be > 0\n",rhs);return RET_BUG;}
  CheckLhs(0,1);
  if ((Format = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ( rhs >= 2 ) 
    {
      rows = print_count_rows(stack,2,rhs); 
      for ( i= 0 ; i < rows ; i++)
	{
	  if ( do_printf("printf",stdout,Format,stack,rhs,1,i,(char **) 0) < 0) 
	    return RET_BUG;
	}
    }
  else
    {
      if ( do_printf("printf",stdout,Format,stack,rhs,1,i,(char **) 0) < 0) 
	return RET_BUG;
    }
  return 0;
}  

/*
 * Nsp fprintf function 
 */

int int_object_fprintf(Stack stack, int rhs, int opt, int lhs)
{
  int i=0,rows;
  NspFile *F;
  char *Format;
  CheckLhs(0,1);
  if ( rhs>= 1 &&  IsSMatObj(stack,1) )
    return int_object_printf(stack,rhs,opt,lhs);
  if ( rhs < 2 ) 
    { Scierror("Error:\tRhs must be >= 2 when first argument is not a string\n",rhs);return RET_BUG;}
  if ((F = GetSciFile(stack,1)) == NULLSCIFILE) return RET_BUG;
  if ( !IS_OPENED(F->obj->flag))
    {
      Scierror("Error:\tfile %s is not opened \n",F->obj->fname);
      return RET_BUG;
    }
  if ((Format = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ( rhs >= 3 ) 
    {
      rows = print_count_rows(stack,3,rhs); 
      for ( i= 0 ; i < rows ; i++)
	{
	  if ( do_printf("printf",F->obj->file,Format,stack,rhs ,2,i,(char **) 0) < 0) 
	    return RET_BUG;
	}
    }
  else 
    {
      if ( do_printf("printf",F->obj->file,Format,stack,rhs ,2,i,(char **) 0) < 0) 
	return RET_BUG;
    }
  return 0;
}  

/*
 * Nsp eye(x) where x is an object 
 * return a Matrix 
 * idem for ones(x)
 */

typedef NspMatrix* (*Mmn) (int m,int n);

int int_obj_gen_o2m(Stack stack, int rhs, int opt, int lhs, Mmn F)
{
  NspMatrix *A;
  int m,n;
  CheckRhs(1,1);
  CheckLhs(0,1);
  m=nsp_object_get_size(NthObj(1),1);
  n=nsp_object_get_size(NthObj(1),2);
  if ((A = (*F)(m,n) ) == NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)A);
  return 1;
}  

int int_object_eye(Stack stack, int rhs, int opt, int lhs)
{
  return int_obj_gen_o2m(stack,rhs,opt,lhs,nsp_mat_eye);
}

int int_object_ones_deprecated(Stack stack, int rhs, int opt, int lhs)
{
  return int_obj_gen_o2m(stack,rhs,opt,lhs,nsp_mat_ones);
}

int int_object_zeros(Stack stack, int rhs, int opt, int lhs)
{
  return int_obj_gen_o2m(stack,rhs,opt,lhs,nsp_mat_zeros);
}

/*
 * Nsp sprintf function 
 */

int int_object_sprintf(Stack stack, int rhs, int opt, int lhs)
{
  int i=0,rows=1;
  NspSMatrix *obj;
  char *str;
  char *Format;
  if ( rhs < 1 ) 
    { Scierror("Error:\tRhs must be >= 1\n",rhs);return RET_BUG;}
  CheckLhs(1,1);
  if ( rhs >= 2 )  rows = print_count_rows(stack,2,rhs); 
  if ((obj=nsp_smatrix_create_with_length(NVOID,rows,1,-1))== NULL) return RET_BUG;
  if ((Format = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ( rhs >= 2 ) 
    {
      for ( i= 0 ; i < rows ; i++)
	{
	  if ( do_printf("printf",(FILE*)0,Format,stack,rhs,1,i,&str) < 0) return RET_BUG;
	  if ((obj->S[i] =nsp_string_copy(str)) == (nsp_string) 0) return RET_BUG;
	}
    }
  else
    {
      if ( do_printf("printf",(FILE*)0,Format,stack,rhs,1,i,&str) < 0) return RET_BUG;
      if ((obj->S[i] =nsp_string_copy(str)) == (nsp_string) 0) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) obj);
  return 1;
}  

/*
 * Nsp scanf function
 * scanf(n,"format") or scanf("format")
 */


extern void scanf_get_line(char *prompt, char *buffer, int buf_size, int *eof);

int int_object_scanf(Stack stack, int rhs, int opt, int lhs)
{
  char buf[256];
  int buf_size= 256 -3, eof; 
  int args,i,ret,rep,iter = 1,iof=0;
  char *Format;
  CheckRhs(1,2);
  if ( lhs < 0  ) return 0;
  if ( rhs == 2)
    {
      if (GetScalarInt(stack,1,&iter) == FAIL) return RET_BUG;
      iof=1;
    }
  if ((Format = GetString(stack,iof+1)) == (char*)0) return RET_BUG;
  /*
   * If we are in a window based Nsp we cannot use stdin 
   * for scanning : we use Nsp function SciGetLine to 
   * get a line of input and use this buffer for performing 
   * a sscanf 
   */
  
  scanf_get_line("==>",buf,buf_size,&eof);
  stack.first += iof+1;
  rep = do_scanf("scanf",(FILE *) 0,Format,stack,0,&args,buf,&ret);
  stack.first -= iof+1;
  if ( rep == FAIL ) return RET_BUG; 
  rep = Min(args,lhs);
  for ( i = 1 ; i <= rep ; i++) 
    {
      if ( NthObj(i+iof+1) == NULLOBJ ) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return RET_BUG;
	}
      NthObj(i+iof+1)->ret_pos = i; 
    }
  if ( iter > 0 ) 
    {
      /* first pass to give proper dimensions to matrices */
      for ( i = 1 ; i <= rep ; i++) 
	{
	  NspObject *obj = NthObj(i+iof+1);
	  if ( IsMat(obj) )
	    {
	      if ( nsp_matrix_resize((NspMatrix *) obj,iter,1) == FAIL) return RET_BUG;
	    }
	  else if ( IsSMat(obj)) 
	    {
	      /* XXXX A FAIRE */
	    }
	}
      /* read each line */
      for ( i= 1 ; i < iter ; i++) 
	{
	  int rep1;
	  scanf_get_line("==>",buf,buf_size,&eof);
	  stack.first += iof+1;
	  rep1 = do_scanf("scanf",(FILE *) 0,Format,stack,i,&args,buf,&ret);
	  stack.first -= iof+1;
	  if ( rep1 == FAIL ) return RET_BUG; 
	}
    }
  else if ( iter < 0 ) 
    {
      /* we must evaluate the number of lines */
      Scierror("%s: iter < 0 is invalid \n",NspFname(stack));
      return RET_BUG;
    }
  return rep;
} 

/*
 * Nsp sscanf function
 */

int int_object_sscanf(Stack stack, int rhs, int opt, int lhs)
{
  int args,i,ret,iof=0,iter=1,rep;
  char *Format;
  NspSMatrix *Sm;

  CheckRhs(2,3);
  if ( lhs < 0  ) return 0;

  if ( rhs == 3 ) 
    {
      if (GetScalarInt(stack,1,&iter) == FAIL) return RET_BUG;
      iof=1;
    }
  if ((Sm = GetSMat(stack,iof+1)) == NULLSMAT) return RET_BUG;
  if ((Format = GetString(stack,iof+2)) == (char*)0) return RET_BUG;
  stack.first += iof+2;
  rep = do_scanf("sscanf",(FILE *)0,Format,stack,0,&args,Sm->S[0],&ret);
  stack.first -= iof+2;
  if ( rep == FAIL ) return RET_BUG; 
  rep = Min(args,lhs);
  for ( i = 1 ; i <= rep ; i++) 
    {
      if ( NthObj(i+iof+2) == NULLOBJ ) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return RET_BUG;
	}
      NthObj(i+iof+2)->ret_pos = i; 
    }

  
  if ( iter < 0 ) iter = Sm->m; 
  else if ( iter == 0) iter = 1;
  else iter = Min(iter,Sm->m);
  
  /* first pass to give proper dimensions to matrices */
  for ( i = 1 ; i <= rep ; i++) 
    {
      NspObject *obj = NthObj(i+iof+2);
      if ( IsMat(obj) )
	{
	  if ( nsp_matrix_resize((NspMatrix *)obj,iter,1) == FAIL) return RET_BUG;
	}
      else if ( IsSMat(obj)) 
	{
	  /* XXXX A FAIRE */
	}
    }
  /* read each line */
  for ( i= 1 ; i < iter ; i++) 
    {
      int rep1;
      stack.first += iof+2;
      rep1 = do_scanf("sscanf",(FILE *) 0,Format,stack,i,&args,Sm->S[i],&ret);
      stack.first -= iof+2;
      if ( rep1 == FAIL ) return RET_BUG; 
    }
  return rep;
}  

/*
 * Nsp fscanf function
 */

static int count_lines(FILE *f);

int int_object_fscanf(Stack stack, int rhs, int opt, int lhs)
{
  int args,i,ret,iof=0,iter=1,rep;
  NspFile *F;
  char *Format;

  CheckRhs(2,3);
  if ( lhs < 0  ) return 0;

  if ( rhs == 3 ) 
    {
      if (GetScalarInt(stack,1,&iter) == FAIL) return RET_BUG;
      iof=1;
    }
  if ((F = GetSciFile(stack,iof+1)) == NULLSCIFILE) return RET_BUG;
  if ( !IS_OPENED(F->obj->flag))
    {
      Scierror("Error:\tfile %s is not opened \n",F->obj->fname);
      return RET_BUG;
    }
  if ((Format = GetString(stack,iof+2)) == (char*)0) return RET_BUG;
  stack.first += iof+2;
  rep = do_scanf("fscanf",F->obj->file,Format,stack,0,&args,NULL,&ret);
  stack.first -= iof+2;
  if ( rep == FAIL ) return RET_BUG; 
  rep = Min(args,lhs);
  for ( i = 1 ; i <= rep ; i++) 
    {
      if ( NthObj(i+iof+2) == NULLOBJ ) 
	{
	  Scierror("Error:\tRunning out of memory\n");
	  return RET_BUG;
	}
      NthObj(i+iof+2)->ret_pos = i; 
    }
  
  if ( iter < 0 ) 
    {
      /* estimates the number of lines from here to 
       * the end of file 
       */
      if ( Format[strlen(Format)-1] == '\n')
	iter = count_lines(F->obj->file)+1;
      else 
	iter = count_lines(F->obj->file);
      /* Scierror("Error:\tfound %d lines\n",iter);*/
    }
  else if ( iter == 0) iter = 1;
  
  /* first pass to give proper dimensions to matrices */
  for ( i = 1 ; i <= rep ; i++) 
    {
      NspObject *obj = NthObj(i+iof+2);
      if ( IsMat(obj) )
	{
	  if ( nsp_matrix_resize((NspMatrix *)obj,iter,1) == FAIL) return RET_BUG;
	}
      else if ( IsSMat(obj)) 
	{
	  /* XXXX A FAIRE */
	}
    }
  /* read each line */
  for ( i= 1 ; i < iter ; i++) 
    {
      int rep1;
      stack.first += iof+2;
      rep1 = do_scanf("fscanf",F->obj->file,Format,stack,i,&args,NULL,&ret);
      stack.first -= iof+2;
      if ( rep1 == FAIL ) return RET_BUG; 
    }
  return rep;
}  

static int count_lines(FILE *f)
{
  int n=0,c;
  long pos; 
  pos= ftell(f);
  while ((c = fgetc(f)) != EOF)
    {
      if ( c == '\n' ) n++;
    }
  /* back to current position */
  fseek(f,pos,SEEK_SET);
  return n;
}

/* size(Obj,..)
 * Nsp size function 
 */

int int_object_size(Stack stack, int rhs, int opt, int lhs)
{ 
  double d;
  static char *Table[]={ "*","r","c",NULL }; 
  NspMatrix *Loc1;
  NspObject *O1,*O2;
  int size_f=-1;
  CheckRhs(1,2);
  CheckLhs(1,2);
  if ( rhs == 2) 
    {
      if ( lhs > 1 ) 
	{
	  Scierror("Error: too many lhs (%d) for function %s with two rhs \n",lhs,NspFname(stack));
	  return RET_BUG;	 
	} 
      if ( IsSMatObj(stack,2)  ) 
	{
	  if (( size_f= GetStringInArray(stack,2,Table,1))== -1 ) return RET_BUG;
	}
      else
	{
	  if (GetScalarInt(stack,2,&size_f) == FAIL) return RET_BUG;
	  if ( size_f < 0 || size_f > 2 ) 
	    {
	      Scierror("Error: second argument of function size should be 0 or 1 not %d\n",size_f);
	      return RET_BUG;
	    }
	}
    }
  
  if ( size_f == -1 ) 
    { 
      if ( lhs == 2 )
	{
	  if (( O1 =nsp_create_object_from_int(NVOID,nsp_object_get_size(NthObj(1),1))) == NULLOBJ ) 
	    return RET_BUG;
	  if (( O2 =nsp_create_object_from_int(NVOID,nsp_object_get_size(NthObj(1),2))) == NULLOBJ ) 
	    return RET_BUG;
	  MoveObj(stack,1,O1);
	  if ( rhs == 2 )
	    MoveObj(stack,2,O2);
	  else 
	    {NthObj(2) = O2; NSP_OBJECT(NthObj(2))->ret_pos = 2;}
	  return 2;
	}
      else
	{
	  if ((Loc1 = nsp_matrix_create(NVOID,'r',1,2))==  NULLMAT)  return RET_BUG;
	  Loc1->R[0]=nsp_object_get_size(NthObj(1),1);
	  Loc1->R[1]=nsp_object_get_size(NthObj(1),2);
	  MoveObj(stack,1,(NspObject *)Loc1);
	  return 1;
	}
    }
  else
    {
      /* just return one argument */
      d =nsp_object_get_size(NthObj(1),size_f);
      if (( O1 =nsp_create_object_from_double(NVOID,d)) == NULLOBJ ) return RET_BUG;
      MoveObj(stack,1,O1);
      return 1;
    }
  return lhs;
}  



/* save(...) save objects in a file ( xdr format )
 * each object is saved using its own function 
 */

static int int_object_xdr_save(Stack stack, int rhs, int opt, int lhs)
{
  char *fname;
  char buf[FSIZE+1];
  NspFile *F;
  int i,rep=0;
  CheckStdRhsMin(1);
  CheckLhs(1,1);
  if (( fname = GetString(stack,1)) == (char*)0) return RET_BUG;
  /* expand keys in path name result in buf */
  nsp_expand_file_with_exec_dir(&stack,fname,buf);
  /* nsp_path_expand(fname,buf,FSIZE); */
  if (( F =nsp_file_open_xdr_w(buf)) == NULLSCIFILE) return RET_BUG;
  if ( rhs == 1) 
    {
      if (nsp_frame_save(F)== FAIL) rep = RET_BUG;
    }
  else 
    {
      for ( i = 2 ; i <= rhs ; i++ )
	{
	  if (nsp_object_xdr_save(F->obj->xdrs,NthObj(i))== FAIL) 
	    {
	      rep = RET_BUG;
	      break;
	    }
	}

    }

  nsp_xdr_save_i(F->obj->xdrs,nsp_no_type_id); /* flag for detecting end of obj at reload */
  if (nsp_file_close_xdr_w(F) == FAIL) 
    {
      nsp_file_destroy(F);
      return RET_BUG;
    }
  nsp_file_destroy(F);
  return rep;
}

/* load(Obj)
 * read saved object from a file 
 */

static int int_object_xdr_load(Stack stack, int rhs, int opt, int lhs) 
{
  char buf[FSIZE+1];
  char *fname;
  NspObject *O; 
  NspFile *F;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if (( fname = GetString(stack,1)) == (char*)0) return RET_BUG;
  /* expand keys in path name result in buf */
  nsp_expand_file_with_exec_dir(&stack,fname,buf);
  /* nsp_path_expand(fname,buf,FSIZE); */
  if (( F =nsp_file_open_xdr_r(buf)) == NULLSCIFILE) return RET_BUG;
  while (1) 
    {
      if ((O=nsp_object_xdr_load(F->obj->xdrs))== NULLOBJ ) 
	break;
      nsp_frame_replace_object(O,-1);
    }
  if (nsp_file_close_xdr_r(F) == FAIL)
    {
      nsp_file_destroy(F);
      return RET_BUG;
    }
  nsp_file_destroy(F);
  return 0;
}

/* generic interface for logical operations on objets.
 *
 */

static int int_object_log_gen(Stack stack, int rhs, int opt, int lhs,char *mes) 
{
  NspObject *O1,*O2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  /* nsp_get_object takes care of Hobj pointers **/
  if (( O1 =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  if (( O2 =nsp_get_object(stack,2)) == NULLOBJ ) return RET_BUG;
  if ( NSP_OBJECT(O1)->type == NSP_OBJECT(O2)->type )
    {
      /* should never get there */
      Scierror("Error: a specialized function %s_%s_%s is missing\n",mes,
	       NSP_OBJECT(O1)->type->sh_type(O1),NSP_OBJECT(O2)->type->sh_type(O2));
      return RET_BUG;
    }
  else
    {
#if 0 
      Scierror("Warning: mixed unimplemented comparison %s %s %s\n",mes,
		NSP_OBJECT(O1)->type->sh_type(O1),NSP_OBJECT(O2)->type->sh_type(O2));
      return RET_BUG;
#else 
      Sciprintf("Warning: mixed unimplemented comparison %s %s %s\n",mes,
		NSP_OBJECT(O1)->type->sh_type(O1),NSP_OBJECT(O2)->type->sh_type(O2));
#endif
    }
  nsp_move_boolean(stack,1,FALSE); 
  return 1; 
} 

/*
 * A == B 
 *  when A and B do not have the same type 
 *  and when a specialized function do not exists 
 *  when type(A) == type(B) a specialized function should 
 *  exists if not a warning message is displayed
 */

int int_object_eq(Stack stack, int rhs, int opt, int lhs) 
{
  return  int_object_log_gen(stack,rhs,opt,lhs,"eq");
}

/*
 * the same for >= <= > < 
 */

int int_object_le(Stack stack, int rhs, int opt, int lhs) 
{
  return  int_object_log_gen(stack,rhs,opt,lhs,"le");
}

int int_object_lt(Stack stack, int rhs, int opt, int lhs) 
{
  return  int_object_log_gen(stack,rhs,opt,lhs,"le");
}

int int_object_ge(Stack stack, int rhs, int opt, int lhs) 
{
  return  int_object_log_gen(stack,rhs,opt,lhs,"le");
}

int int_object_gt(Stack stack, int rhs, int opt, int lhs) 
{
  return  int_object_log_gen(stack,rhs,opt,lhs,"le");
}

/*
 * A <> B 
 * when A and B do not have the same type 
 * and when a specialized function do not exists 
 * when type(A) == type(B) a specialized function should 
 * exists if not a warning message is displayed
 * 
 */

int int_object_neq(Stack stack, int rhs, int opt, int lhs) 
{
  NspObject *O1,*O2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  /* nsp_get_object takes care of Hobj pointers **/
  if (( O1 =nsp_get_object(stack,1)) == NULLOBJ ) return RET_BUG;
  if (( O2 =nsp_get_object(stack,2)) == NULLOBJ ) return RET_BUG;
  if ( NSP_OBJECT(O1)->type == NSP_OBJECT(O2)->type )
    {
      /* should never get there */
      Scierror("Error: a specialized function eq_%s_%s is missing\n",
	       NSP_OBJECT(O1)->type->s_type(),NSP_OBJECT(O1)->type->s_type());
      return RET_BUG;
    }
  else 
    {
#if 0 
      Scierror("Warning: mixed unimplemented comparison %s %s %s\n","<>",
	       NSP_OBJECT(O1)->type->sh_type(O1),NSP_OBJECT(O2)->type->sh_type(O2));
      return RET_BUG;
#else 
      Sciprintf("Warning: mixed unimplemented comparison %s %s %s returning TRUE\n","<>",
		NSP_OBJECT(O1)->type->sh_type(O1),NSP_OBJECT(O2)->type->sh_type(O2));
#endif
      
    }
  nsp_move_boolean(stack,1,TRUE); 
  return 1; 
} 

/*
 * length(A) or numel(A) for all objects. length(A) or numel(A) 
 * give to the number of total elements of the object A (for a 
 * matrix this is the product m*n of the sizes of the 2 dimensions). 
 * BUT this rule have an exception for string matrix (where length 
 * returns a matrix with the length of each strings) and so length 
 * is redefined in SMatObj.c. numel have not this exception.
 *
 * Note that length and numel are redefined for sparse matrix because
 * the method uses here may be "corrupted" for such matrices (the
 * product m*n which is stored in a int variable could overflow).  
 */

static int int_object_length(Stack stack, int rhs, int opt, int lhs) 
{
  CheckRhs(1,1);
  CheckLhs(1,1);
  nsp_move_double(stack,1,nsp_object_get_size(NthObj(1),0)); 
  return 1; 
} 

/*
 * isempty(A) for all objects
 */

static int int_object_isempty(Stack stack, int rhs, int opt, int lhs) 
{
  CheckRhs(1,1);
  CheckLhs(1,1);
  nsp_move_boolean(stack,1,nsp_object_get_size(NthObj(1),0)==0); 
  return 1; 
} 

/*
 * isscalar(A) for all objects
 */

static int int_object_isscalar(Stack stack, int rhs, int opt, int lhs) 
{
  CheckRhs(1,1);
  CheckLhs(1,1);
  nsp_move_boolean(stack,1,nsp_object_get_size(NthObj(1),0)==1); 
  return 1; 
} 

/* serialize(A).
 */

static int int_object_serialize(Stack stack, int rhs, int opt, int lhs) 
{
  char *str = "s";/* s for serial , m for matrix */
  NspObject *Obj,*Obj1;;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if (rhs == 2)
    {
      if ((str = GetString (stack, 2)) == (char *) 0)
	return RET_BUG;
    }
  Obj = nsp_object_serialize(NthObj(1));
  if ( Obj == NULLOBJ) return RET_BUG;
  if ( strncmp(str,"m",1)==0) 
    {
      /* serialize in a matrix */
      Obj1 = (NspObject *) nsp_serial_to_matrix((NspSerial *) Obj);
      nsp_object_destroy(&Obj);
      if ( Obj1 == NULLOBJ) return RET_BUG;
      Obj = Obj1;
    }
  MoveObj(stack,1,Obj);      
  return 1;
}

/* unserialize(S) or unserialize(M)
 */

static int int_serial_unserialize(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  CheckRhs(1,1);
  CheckLhs(0,1);
  NspSerial *a;
  if (( a= GetSerial(stack,1))== NULLSERIAL ) return RET_BUG;
  if ((Obj = nsp_object_unserialize(a))==NULLOBJ)
    return RET_BUG;
  /* take care that nsp_object_unserialize returns a new object 
   * but with a name, we have to delete the name here otherwise 
   * Obj will be copied at return and we will have unfreed memory.
   */
  if (nsp_object_set_name(Obj,NVOID) == FAIL) return RET_BUG;
  MoveObj(stack,1,Obj);
  return 1;
}

/* unserialize a serialized object stored in a numeric matrix 
 */

static int int_serial_munserialize(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(0,1);
  NspSerial *a=NULL;
  if (( A= GetRealMat(stack,1))== NULLMAT ) return RET_BUG;
  if (( a = nsp_matrix_to_serial(A)) == NULLSERIAL ) return RET_BUG;
  Obj = nsp_object_unserialize(a);
  nsp_serial_destroy(a);
  if ( Obj == NULLOBJ ) return RET_BUG;
  /* take care that nsp_object_unserialize returns a new object 
   * but with a name, we have to delete the name here otherwise 
   * Obj will be copied at return and we will have unfreed memory.
   */
  if (nsp_object_set_name(Obj,NVOID) == FAIL) return RET_BUG;
  MoveObj(stack,1,Obj);
  return 1;
}

extern function int_serial_create;

/*
 * The Interface for basic object operations
 */

static OpTab Obj_func[]={
  {"eq",int_object_eq},
  {"ne",int_object_neq},
  {"le",int_object_le},
  {"lt",int_object_lt},
  {"ge",int_object_ge},
  {"gt",int_object_gt},
  {"eye", int_object_eye},
  {"eye_deprecated", int_object_eye},
  {"ones", int_object_ones_deprecated },
  {"ones_deprecated", int_object_ones_deprecated },
  {"zeros", int_object_zeros},
  {"zeros_deprecated", int_object_zeros},
  {"save", int_object_xdr_save},
  {"load", int_object_xdr_load},
  {"printf",int_object_printf},
  {"sprintf",int_object_sprintf},
  {"fprintf",int_object_fprintf},
  {"scanf",int_object_scanf},
  {"sscanf",int_object_sscanf},
  {"fscanf",int_object_fscanf},
  {"size",int_object_size},
  {"ret",int_object_ret},
  {"virg",int_object_virg},
  {"pvirg",int_object_pvirg},
  {"type",int_object_type},
  {"is",int_object_is},
  {"implements",int_object_implements},
  {"info",int_object_info},
  {"sinfo",int_object_sinfo},
  {"finfo",int_object_finfo},
  {"print",int_object_print},
  {"sprint",int_object_sprint},
  {"fprint",int_object_fprint},
  {"diary",int_object_diary},
  {"length",int_object_length},
  {"numel",int_object_length},
  {"isempty",int_object_isempty},
  {"isscalar",int_object_isscalar},
  {"serialize",int_object_serialize},
  {"unserialize",int_serial_unserialize},
  {"unserialize_m",int_serial_munserialize},
  {"serial_create",int_serial_create} ,
  {(char *) 0, NULL}
};

int Obj_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Obj_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
   (for adding or removing functions) */

void Obj_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Obj_func[i].name;
  *f = Obj_func[i].fonc;
}





