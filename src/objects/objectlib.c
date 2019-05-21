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

/*
 * generic routines for NspObjects 
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/smatrix.h> 
#include <nsp/imatrix.h> 
#include <nsp/sprowmatrix.h> 
#include <nsp/spcolmatrix.h> 
#include <nsp/matint.h> 
#include <nsp/file.h> 
#include <nsp/type.h> 
#include <nsp/hobj.h> 
#include <nsp/list.h> 
#include <nsp/none.h> 
#include <nsp/hash.h> 
#include <nsp/cells.h> 
#include <nsp/plist.h> 
#include <nsp/serial.h> 

#include <nsp/math.h>
#include <nsp/interf.h>

/**
 *nsp_object_destroy:
 * @O: a pointer to a #NspObject pointer
 * 
 * Deletes an object by calling the specialized routine of the 
 * given object. 
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
 * @O: a pointer to a #NspObject pointer
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
 * @O: a pointer to a #NspObject 
 * 
 * Returns a new #NspObject which contains a copy of Object @O
 * with name set to NVOID 
 * 
 * Return value: a new #NspObject or %NULLOBJ
 **/

NspObject *nsp_object_copy(const NspObject *O)
{
  return O->type->copy(O);
}

/**
 *nsp_object_full_copy:
 * @O: a pointer to a #NspObject 
 * 
 * Returns a new #NspObject which contains a full copy of Object @O
 * with name set to NVOID 
 * 
 * Return value: a new #NspObject or %NULLOBJ
 **/

NspObject *nsp_object_full_copy(const NspObject *O)
{
  return O->type->full_copy(O);
}



/**
 *nsp_object_get_size:
 * @O: a #NspObject pointer 
 * @j: an integer used to specify requested size 
 * 
 * Returns the size of an object. The semantic depends on objects
 * 
 * Return value: an integer 
 **/

int nsp_object_get_size(const NspObject *O, int j)
{
  return O->type->size(O,j);
}

/**
 *nsp_object_copy_with_name:
 * @O: a #NspObject pointer 
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

/**
 *nsp_object_copy_and_name:
 * @name: string for the name to give to the copy 
 * @O: object to be copied a #NspObject pointer 
 *
 * returns a copy of object @O with name given by @name.
 * 
 * 
 * Return value:  a new #NspObject or %NULLOBJ
 **/

NspObject *nsp_object_copy_and_name(const char *name, NspObject *O)
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
 *nsp_object_full_copy_and_name:
 * @name: string for the name to give to the copy 
 * @O: object to be copied a #NspObject pointer 
 *
 * returns a copy of object @O with name given by @name.
 * 
 * 
 * Return value:  a new #NspObject or %NULLOBJ
 **/

NspObject *nsp_object_full_copy_and_name(const char *name, NspObject *O)
{
  NspObject *l;
  if ( ( l =nsp_object_full_copy(O)) == NULLOBJ ) return NULLOBJ;
  if (nsp_object_set_name(l,name) == FAIL)
    {
      nsp_object_destroy(&l);
      return(NULLOBJ);
    }
  return l;
}


/**
 *nsp_object_type_as_string:
 * @O: a #NspObject pointer 
 * 
 * returns the type of Object @O as a string. 
 * 
 * 
 * Return value: a char pointer
 **/

char *nsp_object_type_as_string(const NspObject *O)
{
  return O->type->s_type();
}


/**
 *nsp_object_type_short:
 * @O: a #NspObject pointer 
 * 
 * Returns a short string describing the object type. 
 * Note that for objects of pointer type #Hobj the type of the 
 * object it points to is returned. 
 * 
 * 
 * Return value: a char pointer 
 **/

char *nsp_object_type_short( NspObject *O)
{
  return O->type->sh_type(O);
}


/**
 *nsp_object_type:
 * @O: a #NspObject pointer 
 * @id: a type id 
 * 
 * Checks that @O is an object or a pointer to an object 
 * which can be casted to an object of type @id 
 *
 * 
 * Return value: %TRUE or %FALSE
 **/

/* FIXME : changer le nom qui est impropre */

int nsp_object_type(const NspObject *O,NspTypeId id)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,FALSE);
  /* Check type **/
  return  check_cast(O,id) ;
}

/**
 *nsp_object_implements:
 * @O: a #NspObject pointer 
 * @id: a type id (#NspTypeId)
 * 
 * Checks if object @O implements the interface which type is 
 * given by @id
 * 
 * Return value: %TRUE or %FALSE
 **/

int nsp_object_implements(NspObject *O,NspTypeId id)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,FALSE);
  /* Check type **/
  return  check_implements(O,id) != NULL ;
}


/**
 *nsp_object_info:
 * @O: a #NspObject pointer 
 * @indent: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * Prints information on object @O by calling its specialized 
 * info function.
 * 
 **/

int nsp_object_info(NspObject *O, int indent,char *name,int rec_level)
{
  return O->type->info(O,indent,name,rec_level);
}


/**
 *nsp_object_print:
 * @O: #NspObject to be printed
 * @indent: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * Prints object @O by calling its specialized 
 * info function.
 * 
 **/

int nsp_object_print(NspObject *O, int indent,char *name, int rec_level)
{
  return O->type->pr(O,indent,name,rec_level);
}

/**
 *nsp_object_latex:
 * @O: #NspObject to be printed
 * @use_math: an int
 * @name: %NULL or name to be used. 
 * @rec_level: deph level of the print.
 * 
 * Prints object @O by calling its specialized 
 * info function.
 * 
 **/

int nsp_object_latex(NspObject *O, int use_math,char *name, int rec_level)
{
  return O->type->latex(O, use_math, name,rec_level);
}

/**
 *nsp_object_is_true:
 * @O: a #NspObject pointer 
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

int nsp_object_xdr_save(XDR *xdrs, NspObject *O)
{
  if ( O  == NULLOBJ) return OK;
  if ( Ocheckname(O,NVOID) )  
    {
      Sciprintf("Warning:\t trying to save an object without name\n");
      return OK;
    }
  return O->type->save(xdrs,O);
}

/**
 *nsp_xdr_save_id:
 * @F: a #NspFile Object 
 * @type: atype
 * 
 * Saves object id in the stream given by @F with an xdr format.
 * The id is now saved as a fixed tag id  #nsp_dynamic_id followed by 
 * the type name. This function should be the first 
 * function called in each save function of new classes. 
 * Saving a specific id should be only used for classes with static 
 * id's and even in those classes this function could replace the old 
 * way.
 * 
 * Return value: %OK or %FALSE 
 **/

int nsp_xdr_save_id(XDR *xdrs, NspTypeBase *type)
{
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(type)) == FAIL) return FAIL;
  return OK;
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

NspObject *nsp_object_xdr_load(XDR *xdrs)
{
  int id;
  NspTypeObject *type;
  nsp_xdr_load_i(xdrs,&id);
  if ( id == nsp_no_type_id ) return NULLOBJ; /* end of saved objects  */
  if ( id == nsp_dynamic_id ) 
    {
      /* new way of dealing with save: id not registered in 
       * primitive_types_register aresave with  nsp_dynamic_id followed 
       * by their name.
       */
      char name[NAME_MAXL];
      if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLOBJ;
      type = nsp_get_type_from_name(name);
      if ( type == NULL) 
	{
	  Scierror("xdrLoad not implemented for type %s\n",name);
	  return NULLOBJ;
	}
    }
  else 
    {
      /* for backward compatibility we accept that types for id's >
       *  > nsp_last_static_id() are recovered from id. 
       */
      type = nsp_get_type_from_id(id);
      if ( type == NULL) 
	{
	  Scierror("xdrLoad not implemented for type %d\n",id);
	  return NULLOBJ;
	}
    }

  while ( type->surtype != NULL ) type= NSP_TYPE_OBJECT(type->surtype);
  return type->load(xdrs);
}

/**
 *nsp_object_loop_extract:
 * @str: name to give to created object 
 * @O: a #NspObject pointer to use to store extracted column or a NULL pointer 
 * @O1: a #NspObject from which we must extract columns
 * @i: undice of the column to be extracted 
 * @rep: returned error value.
 * 
 * O=nsp_object_loop_extract(str,O,O1,i,rep)
 * extract in @O the @i-th column of @O1 
 * if @O is non null on entry it is used to store the new column 
 * if @O is null on entry, a #NspObject is created with name @str and 
 * returned 
 *
 * Return value: a #NspObject or %NULLOBJ
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
  HOBJ_GET_OBJECT(ob,NULL);
  return ob;
}

/**
 *nsp_get_object_copy:
 * @stack:  calling stack 
 * @i: object position 
 * 
 * Returns a copy of the object which is at stack position @i 
 * if object is itself an #Hobj a copy of the object it points to 
 * is returned. Note that the copy is not put on the stack.
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
 * @name: the object name or %NVOID
 * @dval:  value to store in the 1x1 matrix 
 * 
 * Creates an Object containing a 1x1 real matrix 
 * with value @dval and name @name
 * 
 * 
 * Return value: a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_object_from_double(const char *name, double dval)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create(name,'r',(int)1,(int)1))==NULLMAT) return(NULLOBJ);
  A->R[0] = dval ;
  return (NspObject *) A;
}

/**
 *nsp_create_object_from_int:
 * @name:  the object name or %NVOID
 * @ival: value to store in the 1x1 matrix 
 * 
 * 
 * Creates an Object containing a 1x1 real matrix 
 * with value @ival and name @name
 * 
 * Return value: a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_object_from_int(const char *name, int ival)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create(name,'r',(int)1,(int)1))==NULLMAT) return(NULLOBJ);
  A->R[0] = (double) ival ;
  return (NspObject *) A;
}



/**
 *nsp_create_object_from_tint:
 * @name:  the object name or %NVOID
 * @ival: value to store in the 1x1 matrix 
 * @type: type of the given int 
 * 
 * Creates an Object containing a 1x1 int matrix 
 * with value @ival, name @name and type @type
 * 
 * Return value: a new #NspObject ( in fact a #NspIMatrix)
 **/

NspObject *nsp_create_object_from_tint(const char *name, nsp_int_union val, int type)
{
  NspIMatrix *A;
  if ((A= nsp_imatrix_create(name,(int)1,(int)1,type))==NULLIMAT) return(NULLOBJ);
#define IMAT_INTM(name,type,arg) A->name[0] = val.name;break; 
  NSP_ITYPE_SWITCH(A->itype,IMAT_INTM,"");
#undef IMAT_INTM
  return (NspObject *) A;
}

/**
 *nsp_complexi_object_:
 * @name:  the object name or %NVOID
 * 
 * Creates an Object containing a 1x1 complex matrix 
 * with value %i and name @str
 * 
 * Return value:  a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_complexi_object_(const char *name)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create(name,'c',(int)1,(int)1))==NULLMAT) return(NULLOBJ);
  A->C[0].r = 0;
  A->C[0].i = 1;
  return (NspObject *) A;
}

/**
 *nsp_create_object_from_complex:
 * @name: the object name or %NVOID
 * @cval:  value to store in the 1x1 matrix 
 * 
 * Creates an Object containing a 1x1 complex matrix 
 * with value @dval and name @name
 * 
 * 
 * Return value: a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_object_from_complex(const char *name,const doubleC *d)
{
  NspMatrix *A;
  if ((A= nsp_matrix_create(name,'c',(int)1,(int)1))==NULLMAT) return(NULLOBJ);
  A->C[0].r = d->r;
  A->C[0].i = d->i;
  return (NspObject *) A;
}

/*
 */

/**
 *nsp_create_object_from_str:
 * @name: name of object
 * @str:  string to use to fill the object 
 * 
 * Creates a 1x1 string matrix filled with string @str.
 * 
 * Return value:  a new #NspObject ( in fact a #NspSMatrix)
 **/

NspObject *nsp_create_object_from_str(const char *name,const char *str)
{
  return (NspObject *) nsp_smatrix_create(name,1,1,str,1);
}

/**
 *nsp_create_object_from_str_and_size:
 * @name: name of object or %NVOID
 * @str:  the string to use to fille the object 
 * @lstr: the size of the created string
 * 
 * Creates a 1x1 string matrix of lenght @lstr and filled 
 * with the first @lstr characters from @str.
 * 
 * 
 * Return value:  a new #NspObject ( in fact a #NspSMatrix)
 **/

NspObject *nsp_create_object_from_str_and_size(const char *name,const char *str, int lstr)
{
  NspSMatrix *A;
  lstr = Max(0,lstr);
  if ((A=nsp_smatrix_create_with_length(name,1,1,lstr)) ==NULLSMAT) return(NULLOBJ);
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


/**
 * nsp_create_object_from_doubles:
 * @name: name of object or %NVOID
 * @m:  number of rows 
 * @n: number of columns
 * @rtab: real part 
 * @itab: complex part. The matrix is real if @itab is a null pointer.
 * 
 * Create an mxn numeric matrix (#NspMatrix) from data stored in arrays 
 * 
 * Return value:  a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_object_from_doubles(const char *name,int m, int n,double *rtab, double *itab)
{
  NspMatrix *A;
  char type =  (itab != NULL ) ? 'c' : 'r' ;
  int i;
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
 * @name: name of object or %NVOID
 * 
 * Creates an empty matrix with name @name
 * 
 * 
 * Return value:  a new #NspObject ( in fact a #NspMatrix)
 **/

NspObject *nsp_create_empty_matrix_object(const char *name)
{
  return (NspObject *) nsp_matrix_create(name,'r',0,0);
}

/**
 *nsp_create_true_object:
 * @name: name of object or %NVOID
 * 
 * Creates a 1x1 boolean matrix filled with %TRUE
 * 
 * Return value:  a new #NspObject ( in fact a #NspBMatrix)
 **/

NspObject *nsp_create_true_object(const char *name)
{
  NspBMatrix *A;
  if ((A=nsp_bmatrix_create(name,1,1))==NULLBMAT) return(NULLOBJ);
  A->B[0]=TRUE;
  return (NspObject *) A;
}

/**
 *nsp_create_false_object:
 * @name: name of object or %NVOID
 * 
 * Creates a 1x1 boolean matrix filled with %FALSE
 * 
 * Return value:  a new #NspObject ( in fact a #NspBMatrix)
 **/

NspObject *nsp_create_false_object(const char *name)
{
  NspBMatrix *A;
  if ((A=nsp_bmatrix_create(name,1,1))==NULLBMAT) return(NULLOBJ);
  A->B[0]=FALSE;
  return (NspObject *) A;
}

/**
 *nsp_create_boolean_object:
 * @name: name of object or %NVOID
 * @val: boolean value #TRUE or #FALSE
 * 
 * Creates a 1x1 boolean matrix.
 * 
 * Return value:  a new #NspObject ( in fact a #NspBMatrix)
 **/

NspObject *nsp_create_boolean_object(const char *name,int val)
{
  NspBMatrix *A;
  if ((A=nsp_bmatrix_create(name,1,1))==NULLBMAT) return(NULLOBJ);
  A->B[0]=val;
  return (NspObject *) A;
}

/**
 *nsp_object_get_name:
 * @O: a #NspObject.
 * 
 * Returns the name of an nsp object. Note that unnamed object 
 * have name %NVOID. 
 * 
 * Return value: a constant char pointer
 **/

const char *nsp_object_get_name(const NspObject *O)
{
  return O->name ;
}

/**
 *nsp_object_set_name:
 * @O:  a #NspObject.
 * @str: new name to give to object?
 * 
 * sets an object name with char @str
 * 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_object_set_name(NspObject *O,const char *str)
{
  return (O->type->set_name(O,str)== NULL) ? FAIL : OK;
}

/**
 * nsp_object_serialize:
 * 
 **/

#define LAST_FRAG ((u_long)(1 << 31))
#define _writeproc_buf_size 24

typedef struct _writeproc_buf writeproc_buf ;

struct _writeproc_buf {
  int pos,status;
  NspList *L;
  char *current;
};

static int writeproc_increase_buf(writeproc_buf *buf);

#ifdef HAVE_TIRPC 
#define T_IOHANDLE void *
#define T_BUF void *
#else 
#define T_IOHANDLE char *
#define T_BUF char *
#endif 

/* returns the actual number of bytes transferred 
 * -1 is an error */ 

static int writeproc(T_IOHANDLE iohandle,T_BUF c_buf,int len) 
{
  writeproc_buf *buf =(writeproc_buf *) iohandle;
  u_long *header = (u_long *) c_buf ;
  int nbytes = ntohl(*header) & (~LAST_FRAG);
  int ok;
  /* 
   * int last_frag = ((ntohl(*header) & LAST_FRAG) == 0) ? FALSE : TRUE;
   * printf("last fragment = %d, nbytes=%d len=%d\n",last_frag,nbytes,len); 
   */

  while ( nbytes > 0 ) 
    {
      ok = Min(nbytes,_writeproc_buf_size-buf->pos);
      /* printf("print %d out of  %d\n",ok,nbytes); */
      nbytes -= ok;
      memcpy(buf->current + buf->pos,c_buf+4,ok);
      buf->pos +=ok;
      c_buf += ok;
      if ( buf->pos == _writeproc_buf_size )
	{
	  if ( writeproc_increase_buf(buf) == FAIL) 
	    return -1;
	}
    }
  return len;
}

static int writeproc_increase_buf(writeproc_buf *buf)
{
  NspSMatrix *next;
  if ((next= nsp_smatrix_create_with_length("w",1,1,_writeproc_buf_size+1))== NULL) 
    {
      buf->status = FAIL;
      return FAIL;
    }
  if ( nsp_list_end_insert(buf->L, NSP_OBJECT(next))== FAIL) 
    {
      buf->status = FAIL;
      return FAIL;
    }
  buf->current = next->S[0];
  buf->pos = 0;
  return OK;
}

static int readproc(T_IOHANDLE iohandle,T_BUF buf,int len) 
{
  return 0;
}

/* returns a 1x1 string matrix filled with the 
 * list of strings stored in buf;
 */

static NspSMatrix * writeproc_string(writeproc_buf *buf,int *ntot)
{
  int i;
  char *str;
  NspSMatrix *A;
  int n = nsp_list_length(buf->L);
  *ntot= (n-1)*_writeproc_buf_size + buf->pos;
  if ((A=nsp_smatrix_create_with_length("XX",1,1,*ntot+1)) ==NULLSMAT) 
    return(NULLSMAT);
  str = A->S[0];
  for ( i = 1 ; i <= n ; i++)
    {
      NspSMatrix *Obj=(NspSMatrix *)nsp_list_get_element(buf->L, i);
      if ( i != n )
	{
	  memcpy(str,Obj->S[0],_writeproc_buf_size);
	  str+=_writeproc_buf_size;
	}
      else 
	memcpy(str,Obj->S[0],buf->pos);
    }
  return A;
}



/**
 * nsp_object_serialize:
 * @O: a #NspObject.
 * 
 * Create a #NspSerial object filled with the serialized version of 
 * Object @O. 
 * 
 * Return value: a new #NspObject (in fact a #NspSerial) or %NULLOBJ
 **/

NspObject *nsp_object_serialize(const NspObject *O)
{
  NspObject *Obj=NULLOBJ;
  NspSMatrix *Res=NULLSMAT;
  writeproc_buf buf={0,OK, NULL,NULL};
  int rep,n;
  XDR xdrs;
  xdrs.x_op = XDR_ENCODE;

  if ( O  == NULLOBJ) return NULLOBJ;

  if ((buf.L=nsp_list_create(NVOID)) == NULLLIST ) return NULLOBJ;
  
  if ( writeproc_increase_buf(&buf)==FAIL) 
    {
      nsp_list_destroy(buf.L);
      return NULLOBJ;
    }
#if defined(__APPLE__)
  typedef int (*readproc_f)(void *, void *, int);
  typedef int (*writeproc_f)(void *, void *, int);
  xdrrec_create(&xdrs,0,0,(char *) &buf,(readproc_f) readproc,(writeproc_f) writeproc);
#else
  xdrrec_create(&xdrs,0,0,(char *) &buf,readproc,writeproc);
#endif
  rep= O->type->save(&xdrs,O);
  if ( rep != FAIL) 
    {
      xdrrec_endofrecord(&xdrs,1);
    }
  /* XXXX here we could allocate first the serial 
   * object and then fill it with buf 
   * without using an intermediate SMatrix 
   *
   */
  if ((Res = writeproc_string(&buf,&n))== NULLSMAT) 
    {
      xdr_destroy(&xdrs);
      nsp_list_destroy(buf.L);
      return NULLOBJ;
    }
  xdr_destroy(&xdrs);
  Obj =(NspObject *) nsp_serial_create(NVOID,Res->S[0],n);
  nsp_list_destroy(buf.L);
  nsp_smatrix_destroy(Res);
  return Obj;
}


/**
 * nsp_object_unserialize:
 * @S: a #NspSerial object. 
 * 
 * unserialize object @S.
 * 
 * Return value: a new #NspObject  or %NULLOBJ
 **/

NspObject *nsp_object_unserialize(const NspSerial *S)
{
  NspSerial *Sl=NULL;
  NspObject *Obj=NULL;
  int hs= strlen(nsp_serial_header);
  XDR xdrs;

  if ( S->nbytes > strlen(nsp_zserial_header) 
       && strncmp(S->val,nsp_zserial_header,strlen(nsp_zserial_header)) == 0)
    {
      /* decompress first */
      Sl = nsp_serial_uncompress(S);
      if (Sl == NULL) return NULLOBJ;
      if ( Sl->nbytes < hs || strncmp(Sl->val,nsp_serial_header,hs) != 0)
	{
	  Scierror("Error: serial object does not contain a serialized Nsp object\n");
	  return NULLOBJ;
	}
      xdrmem_create(&xdrs,Sl->val+hs,Sl->nbytes-hs,XDR_DECODE);
      Obj= nsp_object_xdr_load(&xdrs); 
      nsp_serial_destroy(Sl);
    }
  else
    {
      if ( S->nbytes < hs || strncmp(S->val,nsp_serial_header,hs) != 0)
	{
	  Scierror("Error: serial object does not contain a serialized Nsp object\n");
	  return NULLOBJ;
	}
      xdrmem_create(&xdrs,S->val+hs,S->nbytes-hs,XDR_DECODE);
      Obj= nsp_object_xdr_load(&xdrs); 
    }
  return Obj;
}

/**
 * nsp_get_short_string_from_id:
 * @id: and integer 
 * 
 * get the short string which describes an objet type 
 * from its id.
 * 
 * Return value: a pointer to the requested string or NULL.
 **/

const char *nsp_get_short_string_from_id(int id)
{
  NspTypeObject *type = nsp_get_type_from_id(id);
  while (type->surtype != NULL) 
    type = (NspTypeObject *) type->surtype;
  return type->sh_type(NULL);
}
