/* Nsp
 * Copyright (C) 1998-2003 Jean-Philippe Chancelier Enpc/Cermics
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

/* 
 * NspObject 
 * all objects can be casted to a NspObject 
 */

#include <stdio.h> 
#include <stdlib.h> 

#include "nsp/object.h"
#include "nsp/interf.h"
#include "../system/files.h" /* FSIZE */
#include "nsp/plistc.h" /* scigetline */
#include  "nsp/datas.h" 

/* FIXME: to be moved in object.h private zone */
static int object_size(NspObject *self, int flag);
static char *get_name(NspObject *ob) ;
static char *set_name(NspObject *ob,const char *name);
static int object_is_true_def(NspObject *ob);
static NspObject *object_loop_def(char *str, NspObject *O, NspObject *O1, int i, int *rep);
static int init_object(NspObject *ob,NspTypeObject *type);
static char *object_type_as_string(void);
static char *object_type_short_string(void);
static NspMethods *object_get_methods(void);
static int int_object_create(Stack stack, int rhs, int opt, int lhs);

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

NspTypeBase *check_implements(void *obj,NspTypeId id)
{
  NspObject *ob=obj;
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
 * @void: 
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

static char *object_type_short_string(void)
{
  return(object_short_type_name);
}

/**
 * set_name:
 * @ob: a #NspObject 
 * @name: a string 
 * 
 * sets the name ob object #NspObject. 
 * 
 * Return value: returns a pointer to the name or %NULLSTRING.
 **/

static char *set_name(NspObject *ob,const char *name)
{
  char *name1 =new_nsp_string(name);
  if ( name1 == NULLSTRING) return NULLSTRING;
  FREE(ob->name) ;
  return ob->name = name1;
}

/**
 * get_name:
 * @ob: 
 * 
 * gets the name of object @ob.
 * 
 * Return value: a string. 
 **/
static char *get_name(NspObject *ob) 
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
 * @str: 
 * @O: 
 * @O1: 
 * @i: 
 * @rep: 
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

int  nsp_object_save_def(void * F, NspObject * M)
{
  Scierror("Error: save not implemented for objects of type %s\n",M->type->s_type());
  return FAIL;
}

NspObject *nsp_object_load_def(void  * F)
{
  Scierror("Error: should not get there, trying to load an object without a load method\n");
  return NULL;
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

/*
 */

/**
 * int_object_equal:
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

static int int_object_equal(void *self,Stack stack,int rhs,int opt,int lhs)
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

static int int_object_not_equal(void *self,Stack stack,int rhs,int opt,int lhs)
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
static int int_object_get_name(void *self,Stack stack,int rhs,int opt,int lhs)
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



static NspMethods object_methods[] = {
  { "set",  int_set_attributes1}, /* set attribute of object the get is given by . */
  { "get_name", int_object_get_name},
  { "equal",  int_object_equal},
  { "not_equal",  int_object_not_equal},
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

NspObject * int_get_object_failed(NspObject *self, char *attr)
{
  Scierror("get attribute %s (as object) for type %s not implemented\n",attr,self->type->s_type());
  return NULL;
}

int int_set_failed(NspObject *self,char *attr, NspObject *val)
{
  Scierror("set attribute %s for type %s not implemented\n",attr,self->type->s_type());
  return RET_BUG;
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

/*
 * ob.set[ attr1=val1, attr2 = val2 ,....]
 */

int int_set_attributes1(void *ob,Stack stack, int rhs, int opt, int lhs)
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


/*
 * can be used in the constructor 
 * type.new[ attr1=val1, attr2 = val2 ,....]
 */

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

int nsp_set_attribute_util(NspObject *ob, NspTypeBase *type, char *attr,NspObject *val)
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


/*
 * get attributes or R.exp
 */

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

NspObject *nsp_get_attribute_util(NspObject *ob,NspTypeBase *type,char *attr) 
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


/* Used in 
 *     obj.attr(xx) = val 
 *     obj(attr)(xx) = val 
 */

NspObject *object_path_extract(NspObject *a, NspObject *ob)
{
  char *str;
  if ((str=nsp_string_object(ob)) == NULL ) return NULLOBJ;
  return nsp_get_attribute_object((NspObject *) a,((NspObject *)a)->basetype,str) ;
}

/*
 * get attributes object used when we want to modify object attributes 
 * as in a.val(...) = 
 */

NspObject *nsp_get_attribute_object(NspObject *ob,NspTypeBase *type, char *attr) 
{
  int item; 
  AttrTab *attrs;
  while ( type != NULL) 
    {
      if (( attrs = type->attrs) != NULL)
	{
	  if (( item=attr_search(attr,attrs)) >=0 )
	    {
	      return (attrs[item].get_object != NULL) ? attrs[item].get_object(ob,attr): NULLOBJ;
	    }
	}
      type = type->surtype;
    }
  return NULLOBJ ;
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

NspSMatrix *nsp_get_methods(NspObject *ob,NspTypeBase *type)
{
  NspMethods *methods;
  NspSMatrix *sm=NULLSMAT,*sm1;
  /* build a string matrix with all methods */
  while ( type != NULL) 
    {
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
      type = type->surtype;
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
	case 0: if ((ret =nsp_create_object_from_str((Ob)->type->sh_type(Ob)))== NULLOBJ) return RET_BUG;break;
	case 1: if ((ret =nsp_create_object_from_str((Ob)->type->s_type()))== NULLOBJ) return RET_BUG;break;
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
 * info(obj) 
 * info on object. 
 * 
 */


int int_object_info(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *object;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((object =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  object->type->info(object,0,NULL,0);
  return 0;
}

/*
 * FIXME: 
 *   must be changed to accept 
 *   more than one object 
 *   and optional arguments 
 *   indent = N, mode = 'as_read' | 'latex' , latex_mode = 'mat'|'tab', header = %t | %f 
 * 
 *   display object using it's standard print function 
 * 
 */

int int_object_print(Stack stack, int rhs, int opt, int lhs)
{
  int rep=-1;
  char *Table[] = {"as_read", NULL};
  NspObject *object;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((object =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ( rhs == 2 ) 
    {
      if ((rep= GetStringInArray(stack,2,Table,1)) == -1) return RET_BUG;
    }
  if ( rep == 0 ) 
    {
      int kp=user_pref.pr_as_read_syntax;
      user_pref.pr_as_read_syntax= 1;
      object->type->pr(object,0,NULL,0);
      user_pref.pr_as_read_syntax= kp;
    }
  else 
    {
      object->type->pr(object,0,NULL,0);
    }
  return 0;
}

/* XXXXXX  FIXME 
 * reste a regler le pb de more 
 * - qui doit pouvoir etre inhibe 
 * - le probleme de l'entete que l'on doit pouvoir supprimer 
 * - le mode TeX si on veut 
 * etc....
 */

int int_object_sprint(Stack stack, int rhs, int opt, int lhs)
{
  int rep=-1;
  NspObject *res, *object ; 
  char *Table[] = {"as_read", NULL};
  IOVFun def ;
  MoreFun mf; 
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((object =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ( rhs == 2 ) 
    {
      if ((rep= GetStringInArray(stack,2,Table,1)) == -1) return RET_BUG;
    }
  /* changes io in order to write in a string matrix */
  def = SetScilabIO(Sciprint2string);
  mf =  SetScilabMore(scimore_void);
  if ( rep == 0 ) 
    {
      int kp=user_pref.pr_as_read_syntax;
      user_pref.pr_as_read_syntax= 1;
      object->type->pr(object,0,NULL,0);
      user_pref.pr_as_read_syntax= kp;
    }
  else 
    {
      object->type->pr(object,0,NULL,0);
    }
  res = Sciprint2string_reset(); 
  SetScilabIO(def);
  SetScilabMore(mf);
  if ( res == NULL) return RET_BUG; 
  MoveObj(stack,1, res);
  return 1;
}

int int_object_fprint(Stack stack, int rhs, int opt, int lhs)
{
  FILE *f;
  int rep=-1;
  char *Table[] = {"as_read", NULL};
  NspObject *object ; 
  NspFile *F;
  IOVFun def ;
  MoreFun mf; 
  CheckRhs(2,3);
  CheckLhs(0,1);
  if ((F= GetSciFile(stack,1))== NULL) return RET_BUG; 
  if ((object =nsp_get_object(stack,2))== NULLOBJ) return RET_BUG; 
  if ( rhs == 3 ) 
    {
      if ((rep= GetStringInArray(stack,3,Table,1)) == -1) return RET_BUG;
    }

  /* changes io in order to write to file F */
  if ( !IS_OPENED(F->flag))
    {
      Scierror("Warning:\tfile %s is already closed\n",F->fname);
      return RET_BUG;
    }
  f=Sciprint_file(F->file); 
  def = SetScilabIO(Sciprint2file);
  mf =  SetScilabMore(scimore_void);
  if ( rep == 0 ) 
    {
      int kp=user_pref.pr_as_read_syntax;
      user_pref.pr_as_read_syntax= 1;
      object->type->pr(object,0,NULL,0);
      user_pref.pr_as_read_syntax= kp;
    }
  else 
    {
      object->type->pr(object,0,NULL,0);
    }
  SetScilabIO(def);
  SetScilabMore(mf);
  Sciprint_file(f); 
  return 0;
}

int int_object_diary(Stack stack, int rhs, int opt, int lhs)
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
      Sciprint_set_diary(F->file,diary_echo);
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
 * Scilab printf(format,....) function 
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
 * Scilab fprintf function 
 */

int int_object_fprintf(Stack stack, int rhs, int opt, int lhs)
{
  int i=0,rows;
  NspFile *F;
  char *Format;
  if ( rhs < 2 ) 
    { Scierror("Error:\tRhs must be >= 2\n",rhs);return RET_BUG;}
  CheckLhs(0,1);
  if ((F = GetSciFile(stack,1)) == NULLSCIFILE) return RET_BUG;
  if ( !IS_OPENED(F->flag))
    {
      Scierror("Error:\tfile %s is not opened \n",F->fname);
      return RET_BUG;
    }
  if ((Format = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ( rhs >= 3 ) 
    {
      rows = print_count_rows(stack,3,rhs); 
      for ( i= 0 ; i < rows ; i++)
	{
	  if ( do_printf("printf",F->file,Format,stack,rhs ,2,i,(char **) 0) < 0) 
	    return RET_BUG;
	}
    }
  else 
    {
      if ( do_printf("printf",F->file,Format,stack,rhs ,2,i,(char **) 0) < 0) 
	return RET_BUG;
    }
  return 0;
}  

/*
 * Scilab eye(x) where x is an object 
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

int int_object_ones(Stack stack, int rhs, int opt, int lhs)
{
  return int_obj_gen_o2m(stack,rhs,opt,lhs,nsp_mat_ones);
}

int int_object_zeros(Stack stack, int rhs, int opt, int lhs)
{
  return int_obj_gen_o2m(stack,rhs,opt,lhs,nsp_mat_zeros);
}

/*
 * Scilab sprintf function 
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
 * Scilab scanf function
 */


extern void scanf_get_line(char *prompt, char *buffer, int buf_size, int *eof);

int int_object_scanf(Stack stack, int rhs, int opt, int lhs)
{
  char buf[256];
  int buf_size= 256 -3, eof; 
  int args,i,ret,rep,iter = 1,iof=0;
  char *Format,*String=NULL;
  CheckRhs(1,2);
  if ( lhs < 0  ) return 0;
  if ( rhs == 2)
    {
      if (GetScalarInt(stack,1,&iter) == FAIL) return RET_BUG;
      iof=1;
    }
  if ((Format = GetString(stack,iof+1)) == (char*)0) return RET_BUG;
  /*
   * If we are in a window based Scilab we cannot use stdin 
   * for scanning : we use Scilab function SciGetLine to 
   * get a line of input and use this buffer for performing 
   * a sscanf 
   */
  
  scanf_get_line("==>",buf,buf_size,&eof);
  stack.first += iof+1;
  rep = do_scanf("scanf",(FILE *) 0,Format,stack,0,&args,String,&ret);
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
	  rep1 = do_scanf("scanf",(FILE *) 0,Format,stack,i,&args,String,&ret);
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
 * Scilab sscanf function
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
 * Scilab fscanf function
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
  if ( !IS_OPENED(F->flag))
    {
      Scierror("Error:\tfile %s is not opened \n",F->fname);
      return RET_BUG;
    }
  if ((Format = GetString(stack,iof+2)) == (char*)0) return RET_BUG;
  stack.first += iof+2;
  rep = do_scanf("fscanf",F->file,Format,stack,0,&args,NULL,&ret);
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
	iter = count_lines(F->file)+1;
      else 
	iter = count_lines(F->file);
      Scierror("Error:\tfound %d lines\n",iter);
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
      rep1 = do_scanf("fscanf",F->file,Format,stack,i,&args,NULL,&ret);
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

/*
 * Scilab size function 
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



/*
 * Save Objects in a file ( xdr format )
 *  each object is saved using its own function 
 */

int int_object_xdrsave(Stack stack, int rhs, int opt, int lhs)
{
  char *fname;
  char buf[FSIZE+1];
  NspFile *F;
  int i,rep=0;
  if ( rhs < 2 ) 
    { Scierror("Error:\tRhs must be > 1\n",rhs);return RET_BUG;}
  CheckLhs(1,1);
  if (( fname = GetString(stack,1)) == (char*)0) return RET_BUG;
  /* expand keys in path name result in buf */
  nsp_path_expand(fname,buf,FSIZE);
  if (( F =nsp_file_open_xdr_w(buf)) == NULLSCIFILE) return RET_BUG;
  for ( i = 2 ; i <= rhs ; i++ )
    {
      if (nsp_object_xdr_save(F->xdrs,NthObj(i))== FAIL) 
	{
	  rep = RET_BUG;
	  break;
	}
    }
  nsp_xdr_save_i(F->xdrs,nsp_no_type_id); /* flag for detecting end of obj at reload */
  if (nsp_file_close_xdr_w(F) == FAIL) 
    {
      nsp_file_destroy(F);
      return RET_BUG;
    }
  nsp_file_destroy(F);
  return rep;
}

/*
 * read saved object from a file 
 */


int int_object_xdrload(Stack stack, int rhs, int opt, int lhs) 
{
  char buf[FSIZE+1];
  char *fname;
  NspObject *O; 
  NspFile *F;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if (( fname = GetString(stack,1)) == (char*)0) return RET_BUG;
  /* expand keys in path name result in buf */
  nsp_path_expand(fname,buf,FSIZE);
  if (( F =nsp_file_open_xdr_r(buf)) == NULLSCIFILE) return RET_BUG;
  while (1) 
    {
      if ((O=nsp_object_xdr_load(F->xdrs))== NULLOBJ ) 
	break;
      nsp_frame_replace_object(O);
    }
  if (nsp_file_close_xdr_r(F) == FAIL)
    {
      nsp_file_destroy(F);
      return RET_BUG;
    }
  nsp_file_destroy(F);
  return 0;
}


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
  nsp_move_boolean(stack,1,TRUE); 
  return 1; 
} 

/*
 * length(A) for all objects except for string matrices 
 */

static int int_object_length(Stack stack, int rhs, int opt, int lhs) 
{
  CheckRhs(1,1);
  CheckLhs(1,1);
  nsp_move_double(stack,1,nsp_object_get_size(NthObj(1),0)); 
  return 1; 
} 

/*
 * FIXME: just here to test the matrix interface 
 * the redim function can be factorized here 
 * it calls the correct redim function for objects 
 * which implements the matint interface.
 */

#include <nsp/matint.h>

int int_object_testredim(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspTypeBase *type;
  NspObject *Ob;
  CheckRhs(3,3);
  if ((Ob= nsp_get_object(stack,1))== NULLOBJ) return RET_BUG;
  if (GetScalarInt (stack, 2, &m1) == FAIL)    return RET_BUG;
  if (GetScalarInt (stack, 3, &n1) == FAIL)    return RET_BUG;
  /* be sure that interface is initialized : FIXME */
  nsp_type_matint= new_type_matint(T_BASE);
  type = check_implements(Ob,nsp_type_matint_id);
  if ( type != NULL) 
    {
      if ( MAT_INT(type)->redim(Ob,m1,n1) != OK) return RET_BUG;
    }
  else 
    {
      Scierror("Object do not implements matint\n");
      return RET_BUG;
    }
  return 0;
}

int int_matrix_testredim(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspMatrix *A;
  CheckRhs (3,3);
  CheckLhs (0,0);
  if ((A = GetMat(stack, 1)) == NULLMAT) return RET_BUG;
  if (GetScalarInt (stack, 2, &m1) == FAIL)    return RET_BUG;
  if (GetScalarInt (stack, 3, &n1) == FAIL)    return RET_BUG;
  if ( nsp_matrix_redim (A,m1,n1) != OK) return RET_BUG;
  return 0;
}

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
  {"ones", int_object_ones},
  {"zeros", int_object_zeros},
  {"save", int_object_xdrsave},
  {"load", int_object_xdrload},
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
  {"info",int_object_info},
  {"print",int_object_print},
  {"sprint",int_object_sprint},
  {"fprint",int_object_fprint},
  {"diary",int_object_diary},
  {"length",int_object_length},
  {"REDIM",int_object_testredim}, /* FIXME: testing */
  {"REDIM1",int_matrix_testredim}, /* FIXME: testing */
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





