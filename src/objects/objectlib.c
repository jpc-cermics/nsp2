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

/*
 * generic routines for NspObjects 
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/math.h"
#include "nsp/object.h"
#include "nsp/interf.h"

/**
 *nsp_object_destroy:
 * @O: 
 * 
 * Deletes an object by calling the specialized routine of the 
 * given object 
 * 
 **/

void nsp_object_destroy(NspObject **O)
{
  if ((*O) == NULLOBJ) return;
  /*Scierror("Warning: Destroy Object \n"); */
  /*nsp_object_print(*O,0); */
  (*O)->type->dealloc(((void *) *O));
  *O = NULLOBJ;
}

/**
 *nsp_void_object_destroy:
 * @O: 
 * 
 * if object name is NVOID and the ret_pos field is < 0 
 * deletes the object by calling the specialized routine of the 
 * given object. 
 * 
 **/

void nsp_void_object_destroy(NspObject **O)
{
  if (*O != NULLOBJ && Ocheckname(*O,NVOID) && (*O)->ret_pos < 0 ) 
    nsp_object_destroy(O);
}

/**
 *nsp_object_copy:
 * @O: 
 * 
 * Returns a new #NspObject which contains a copy of Object @O
 * with name set to NVOID 
 * 
 * Return value: a new #NspObject or %NULLOBJ
 **/

NspObject *nsp_object_copy(NspObject *O)
{
  return O->type->copy(O);
}


/**
 *nsp_object_get_size:
 * @O: 
 * @j: 
 * 
 * Returns the <<size>> of an object 
 * The semantic depends on objects
 * 
 * Return value: an integer 
 **/

int nsp_object_get_size(NspObject *O, int j)
{
  return O->type->size(O,j);
}

/**
 *nsp_object_copy_with_name:
 * @O: 
 * 
 * returns a copy of object @O with the same name. 
 * 
 * Return value: a new #NspObject or %NULLOBJ
 **/

NspObject *nsp_object_copy_with_name(NspObject *O)
{
  NspObject *l;
  if ( ( l =nsp_object_copy(O)) == NULLOBJ ) return NULLOBJ;
  if (nsp_object_set_name(l,nsp_object_get_name(O)) == FAIL)
    {
      nsp_object_destroy(&l);
      return(NULLOBJ);
    }
  return l;
}

/*
 */

/**
 *nsp_object_copy_and_name:
 * @name: 
 * @O: 
 *
 * returns a copy of object @O with name given by @name.
 * 
 * 
 * Return value:  a new #NspObject or %NULLOBJ
 **/

NspObject *nsp_object_copy_and_name(char *name, NspObject *O)
{
  NspObject *l;
  if ( ( l =nsp_object_copy(O)) == NULLOBJ ) return NULLOBJ;
  if (nsp_object_set_name(l,name) == FAIL)
    {
 nsp_object_destroy(&l);
      return(NULLOBJ);
    }
  return l;
}

/**
 *nsp_object_type_as_string:
 * @O: 
 * 
 * returns the type of Object @O as a string. 
 * 
 * 
 * Return value: a char pointer
 **/

char *nsp_object_type_as_string(NspObject *O)
{
  return O->type->s_type();
}


/**
 *nsp_object_type_short:
 * @O: 
 * 
 * Returns a short string describing the object type. 
 * Note that for objects of pointer type #Hobj the type of the 
 * object it points to is returned. 
 * 
 * 
 * Return value: a char pointer 
 **/

char *nsp_object_type_short(NspObject *O)
{
  return O->type->sh_type(O);
}


/**
 *nsp_object_type:
 * @O: an object 
 * @id: a type id 
 * 
 * Checks that @O is an object or a pointer to an object 
 * which can be casted to an object of type @id 
 *
 * 
 * Return value: %TRUE or %FALSE
 **/

/* FIXME : changer le nom qui est impropre */

int nsp_object_type(NspObject *O,NspTypeId id)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O;
  /* Check type **/
  return  check_cast(O,id) ;
}

/**
 *nsp_object_implements:
 * @O: 
 * @id: 
 * 
 * Checks if object @O implements the interface which type is 
 * given by @id
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_object_implements(NspObject *O,NspTypeId id)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O;
  /* Check type **/
  return  check_implements(O,id) != NULL ;
}


/**
 *nsp_object_info:
 * @O: 
 * @indent: 
 * 
 * Prints information on object @O by calling its specialized 
 * info function.
 * 
 **/

void nsp_object_info(NspObject *O, int indent)
{
  O->type->info(O,indent);
}


/**
 *nsp_object_print:
 * @O: 
 * @indent: 
 * 
 * Prints object @O by calling its specialized 
 * info function.
 * 
 **/

void nsp_object_print(NspObject *O, int indent)
{
  O->type->pr(O,indent,TRUE);
}

/**
 *nsp_object_is_true:
 * @O: 
 * 
 * Check if object @O can be considered as a boolean a TRUE Object 
 * in a logical statement.
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_object_is_true(NspObject *O)
{
  return O->type->is_true(O);
}

/**
 *nsp_object_xdr_save:
 * @F: a #NspFile Object 
 * @O: an object 
 * 
 * Saves object in the stream given by @F with an xdr format.
 * 
 * Return value: %OK or %FALSE 
 **/

int nsp_object_xdr_save(NspFile  *F, NspObject *O)
{
  if ( O  == NULLOBJ) return OK;
  if ( Ocheckname(O,NVOID) )  
    {
      Scierror("Warning:\t trying to save an object without name\n");
      return OK;
    }
  return O->type->save(F,O);
}

/**
 *nsp_object_xdr_load:
 * @F: a #NspFile Object 
 * 
 * Reads one object in the stream given by @F with an xdr format.
 * 
 * 
 * Return value: a new #NspObject
 **/

NspObject *nsp_object_xdr_load(NspFile  *F)
{
  int id;
  NspTypeObject *type;
 nsp_xdr_load_i(F->xdrs,&id);
  if ( id == nsp_no_type_id ) return NULLOBJ; /* end of saved objects  */
  type = nsp_get_type_from_id(id);
  if ( type == NULL) 
    {
      Scierror("xdrLoad not implemented for type %d\n",id);
      return NULLOBJ;
    }
  while ( type->surtype != NULL ) type= NSP_TYPE_OBJECT(type->surtype);
  return type->load(F);
}

/**
 *nsp_object_loop_extract:
 * @str: 
 * @O: 
 * @O1: 
 * @i: 
 * @rep: 
 * 
 * O=nsp_object_loop_extract(str,O,O1,i,rep)
 * extract in O the ith column of O1 
 * if O is != NULL O is used to store the new column 
 * if O is null returned NspObject is created with name str
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_object_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  return O1->type->loop(str,O,O1,i,rep);
}

/*
 * Basic Object creation used for interface creation
 */

/**
 *nsp_get_object:
 * @stack: calling stack 
 * @i: object position 
 * 
 * Returns Object which is at stack position i 
 * if Object is itself an Hobj the object it points to 
 * is returned. 
 * 
 * 
 * Return value: a #NspObject
 **/

NspObject *nsp_get_object(Stack stack, int i)
{
  NspObject *ob = NthObj(i);
  /* Follow pointer **/
  if ( check_cast(ob,nsp_type_hobj_id) == TRUE)  ob = ((NspHobj *) ob)->O ;
  return ob;
}

/**
 *nsp_get_object_copy:
 * @stack:  calling stack 
 * @i: object position 
 * 
 * Returns a copy of the object which is at stack position @i 
 * if object is itself an #Hobj a copy of the object it points to 
 * is returned. 
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_get_object_copy(Stack stack, int i)
{
  NspObject *O;
  if ( (O=nsp_get_object(stack,i)) == NULLOBJ ) return NULLOBJ;
  return nsp_object_copy(O);
}


/**
 *nsp_create_object_from_double:
 * @str: the object name or %NVOID
 * @dval:  value to store in the 1x1 matrix 
 * 
 * Creates an Object containing a 1x1 real matrix 
 * with value @dval and name @str
 * 
 * 
 * Return value: a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_object_from_double(char *str, double dval)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create(str,'r',(integer)1,(integer)1))==NULLMAT) return(NULLOBJ);
  A->R[0] = dval ;
  return (NspObject *) A;
}

/**
 *nsp_create_object_from_int:
 * @str:  the object name or %NVOID
 * @ival: value to store in the 1x1 matrix 
 * 
 * 
 * Creates an Object containing a 1x1 real matrix 
 * with value @ival and name @str
 * 
 * Return value: a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_object_from_int(char *str, int ival)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create(str,'r',(integer)1,(integer)1))==NULLMAT) return(NULLOBJ);
  A->R[0] = (double) ival ;
  return (NspObject *) A;
}

/**
 *nsp_complexi_object_:
 * @str:  the object name or %NVOID
 * 
 * Creates an Object containing a 1x1 complex matrix 
 * with value %i and name @str
 * 
 * Return value:  a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_complexi_object_(char *str)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create(str,'i',(integer)1,(integer)1))==NULLMAT) return(NULLOBJ);
  A->C[0].r = 0;
  A->C[0].i = 1;
  return (NspObject *) A;
}

/*
 */

/**
 *nsp_create_object_from_str:
 * @str:  string to use to fill the object 
 * 
 * Creates a 1x1 string matrix filled with string @str.
 * 
 * Return value:  a new #NspObject ( in fact a #NspSMatrix)
 **/

NspObject *nsp_create_object_from_str(char *str)
{
  return (NspObject *) nsp_smatrix_create(NVOID,1,1,str,1);
}

/**
 *nsp_create_object_from_str_and_size:
 * @str:  the string to use to fille the object 
 * @lstr: the size of the created string
 * 
 * Creates a 1x1 string matrix of lenght @lstr and filled 
 * with the first @lstr characters from @str.
 * 
 * 
 * Return value:  a new #NspObject ( in fact a #NspSMatrix)
 **/

NspObject *nsp_create_object_from_str_and_size(char *str, int lstr)
{
  NspSMatrix *A;
  lstr = Max(0,lstr);
  if ((A=nsp_smatrix_create_with_length(NVOID,1,1,lstr)) ==NULLSMAT) return(NULLOBJ);
  if ( lstr > 0 ) 
    {
      strncpy(A->S[0],str,lstr);
      A->S[0][lstr]='\0';
    }
  else 
    /* since lstr >= 0 we can store at least one character */
    A->S[0][0] = '\0';
  return (NspObject *) A;
}

/*
 */

/**
 *nsp_create_object_from_doubles:
 * @m: 
 * @n: 
 * @it: 
 * @rtab: 
 * @itab: 
 * @name: 
 * 
 * Create a mxn matrix from data stored 
 * in arrays 
 * 
 * Return value:  a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_object_from_doubles(integer m, integer n, integer it, double *rtab, double *itab, char *name)
{
  char type; 
  NspMatrix *A;
  int i;
  type = ( it == 1) ? 'i' : 'r' ;
  if ( (A= nsp_matrix_create(name,type,m,n)) ==NULLMAT) return NULLOBJ;
  if ( type == 'r' ) 
    {
      for ( i=0 ; i < A->mn ; i++)
	{
	  A->R[i]= rtab[i];
	}
    }
  else 
    {
      for ( i=0 ; i < A->mn ; i++)
	{
	  A->C[i].r = rtab[i];
	  A->C[i].i = itab[i];
	}
    }
  return (NspObject *) A;
}

/**
 *nsp_create_empty_matrix_object:
 * @str: 
 * 
 * Creates an empty matrix with name @str
 * 
 * 
 * Return value:  a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_empty_matrix_object(char *str)
{
  return (NspObject *) nsp_matrix_create(str,'r',0,0);
}

/**
 *nsp_create_true_object:
 * @str: 
 * 
 * Creates a 1x1 boolean matrix filled with %TRUE
 * 
 * 
 * Return value:  a new #NspObject ( in fact a #NspBMatrix)
 **/

NspObject *nsp_create_true_object(char *str)
{
  NspBMatrix *A;
  if ((A=nsp_bmatrix_create(str,1,1))==NULLBMAT) return(NULLOBJ);
  A->B[0]=TRUE;
  return (NspObject *) A;
}

/**
 *nsp_create_false_object:
 * @str: 
 * 
 * 
 * Creates a 1x1 boolean matrix filled with %FALSE
 * 
 * Return value:  a new #NspObject ( in fact a #NspBMatrix)
 **/

NspObject *nsp_create_false_object(char *str)
{
  NspBMatrix *A;
  if ((A=nsp_bmatrix_create(str,1,1))==NULLBMAT) return(NULLOBJ);
  A->B[0]=FALSE;
  return (NspObject *) A;
}

/**
 *nsp_create_boolean_object:
 * @str: Object name or #NVOID
 * @val: boolean value #TRUE or #FALSE
 * 
 * Creates a 1x1 boolean matrix.
 * 
 * Return value:  a new #NspObject ( in fact a #NspBMatrix)
 **/

NspObject *nsp_create_boolean_object(char *str,int val)
{
  NspBMatrix *A;
  if ((A=nsp_bmatrix_create(str,1,1))==NULLBMAT) return(NULLOBJ);
  A->B[0]=val;
  return (NspObject *) A;
}

/**
 *nsp_object_get_name:
 * @O: 
 * 
 * Returns the name of an object 
 * 
 * 
 * Return value: 
 **/

char *nsp_object_get_name(NspObject *O)
{
  return NSP_OBJECT(O)->name ;
}

/**
 *nsp_object_set_name:
 * @O: 
 * @str: 
 * 
 * sets an object name with char @str
 * 
 * 
 * Return value: 
 **/

int nsp_object_set_name(NspObject *O, char *str)
{
  char *loc;
  if ((loc = O->type->set_name(O,str))== NULL) return FAIL;
  return OK;
}



