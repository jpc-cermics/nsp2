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

#include <ctype.h> /* isxxxx */
#include <nsp/nsp.h>

#define SMatrix_Private
#include <nsp/objects.h>
#include <nsp/matint.h>
#include <nsp/hobj.h>

#include <nsp/pr-output.h>
#include <nsp/matutil.h>
#include <nsp/smatrix-in.h>
#include <nsp/datas.h>
#include <nsp/gsort-p.h>

static int int_smatrix_create(Stack stack, int rhs, int opt, int lhs);

/**
 * SECTION:smatrix
 * @title: NspSMatrix
 * @short_description: An object used to implement string matrices.
 * @see_also:
 *
 * <para>
 * A #NspSMatrix is used to represent a string matrix.
 * It can be filled with strings and string pointers are stored
 * in a one dimensionnal array (column order). It implements the
 * matint interface which is used for generic matrice operations.
 * </para>
 **/

/*
 * NspSMatrix inherits from NspObject
 */

int nsp_type_smatrix_id=0;
NspTypeSMatrix *nsp_type_smatrix=NULL;
int nsp_type_smatrix_init();

static NspMethods *smatrix_get_methods(void);


NspTypeSMatrix *new_type_smatrix(type_mode mode)
{
  NspTypeMatint *mati;/* interface */
  NspTypeSMatrix *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_smatrix != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_smatrix;
    }
  if ((type =  malloc(sizeof(NspTypeSMatrix))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* smatrix_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods =  smatrix_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_smatrix;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for smatrix */

  top->pr = (print_func *)nsp_smatrix_print;                    /* printing*/
  top->dealloc = (dealloc_func *)nsp_smatrix_destroy;           /* dealloc */
  top->copy  =  (copy_func *)nsp_smatrix_copy;                  /* copy object */
  top->size  = (size_func *)nsp_smatrix_size;                   /* m,n or m*n  */
  top->s_type =  (s_type_func *)nsp_smatrix_type_as_string;     /* type as a String */
  top->sh_type = (sh_type_func *)nsp_smatrix_type_short_string; /* type as a short string */
  top->info = (info_func *)nsp_smatrix_info;                    /* info */
  top->is_true = (is_true_func  *)nsp_smatrix_is_true;          /* check if object can be considered as true */
  top->loop =(loop_func *)nsp_smatrix_loop_extract;             /* for loops */
  top->path_extract =  NULL;                                    /* used for x(1)(2)(...) */
  top->get_from_obj = (get_from_obj_func *)nsp_smatrix_object;  /* get object stored in SciObj */
  top->eq  = (eq_func *)nsp_smatrix_eq;                         /* equality check */
  top->neq  = (eq_func *)nsp_smatrix_neq;                       /* non-equality check */

  top->save  = (save_func *)nsp_smatrix_xdr_save;
  top->load  = (load_func *)nsp_smatrix_xdr_load;
  top->create = (create_func*) int_smatrix_create;
  top->latex = (print_func *) nsp_smatrix_latex_print;
  top->full_copy  =  (copy_func *)nsp_smatrix_copy;                  /* copy object */

  /* specific methods for smatrix */
  type->init = (init_func *) init_smatrix;
  /*
   * interfaces can be added here
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */

  /*
   * Matrix implements Matint the matrix interface
   * which is common to object that behaves like matrices.
   */

  mati = new_type_matint(T_DERIVED);
  mati->methods = matint_get_methods;
  /* mati->redim = (matint_redim *) nsp_smatrix_redim; use default value */
  mati->resize = (matint_resize  *) nsp_smatrix_resize;
  mati->free_elt = (matint_free_elt *) nsp_string_destroy;
  mati->elt_size = (matint_elt_size *) nsp_smatrix_elt_size ;
  mati->clone = (matint_clone *) nsp_smatrix_clone;
  mati->copy_elt = (matint_copy_elt *) nsp_string_copy;
  mati->enlarge = (matint_enlarge *) nsp_smatrix_enlarge;
  mati->canonic =  nsp_matint_canonic;
  mati->copy_ind = nsp_matint_basic_copy_pointer;
  type->interface = (NspTypeBase *) mati;

  if ( nsp_type_smatrix_id == 0 )
    {
      /*
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_smatrix
       */
      type->id =  nsp_type_smatrix_id = nsp_new_type_id();
      nsp_type_smatrix = type;
      if ( nsp_register_type(nsp_type_smatrix) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_smatrix(mode);
    }
  else
    {
      type->id = nsp_type_smatrix_id;
      return type;
    }

}

/*
 * initialize Smatrix instances
 * locally and by calling initializer on parent class
 */

static int init_smatrix(NspSMatrix *o,NspTypeSMatrix *type)
{
  /* to be done always */
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type;
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Smatrix
 */

NspSMatrix *new_smatrix()
{
  NspSMatrix *loc;
  /* type must exists */
  nsp_type_smatrix = new_type_smatrix(T_BASE);
  if ( (loc = malloc(sizeof(NspSMatrix)))== NULLSMAT) return loc;
  /* initialize object */
  if ( init_smatrix(loc,nsp_type_smatrix) == FAIL) return NULLSMAT;
  return loc;
}


/*
 * MatSize : returns m,n,or m*n
 */

int nsp_smatrix_size(NspSMatrix*Mat, int flag)
{
  switch (flag)
    {
    case 0: return Mat->mn;
    case 1: return Mat->m;
    case 2: return Mat->n;
    }
  return 0;
}

/*
 * MatType
 */

static char smat_type_name[]="SMat";
static char smat_short_type_name[]="s";

char *nsp_smatrix_type_as_string(void)
{
  return(smat_type_name);
}

char *nsp_smatrix_type_short_string(NspObject *v)
{
  return(smat_short_type_name);
}

NspObject *nsp_smatrix_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  NspSMatrix *M= (NspSMatrix *) O1,*M1=NULLSMAT;
  if ( O == NULLOBJ )
    {
      if (( M1= SMatLoopCol(str,NULLSMAT,M,i,rep))==NULLSMAT) return NULLOBJ;
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return (NspObject *) M1 ;
    }
  else
    {
      if (( M1 =nsp_smatrix_object(O)) == NULLSMAT ) return NULLOBJ;
      M1= SMatLoopCol(str,M1,M,i,rep);
      if (( *rep == RET_ENDFOR )) return NULLOBJ;
      return O;
    }
}

int nsp_smatrix_eq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_smatrix_id) == FALSE) return FALSE ;
  if ( ! ( ((NspSMatrix *) A)->m == ((NspSMatrix *) B)->m
	   && ((NspSMatrix *) A)->n == ((NspSMatrix *) B)->n)) return FALSE;
  rep = SMatFullComp((NspSMatrix *)A, (NspSMatrix *) B, "==", &err);
  if (err == TRUE)
    return FALSE;
  return rep;

}

int nsp_smatrix_neq(NspObject *A, NspObject *B)
{
  return ( nsp_smatrix_eq(A,B) == TRUE ) ? FALSE : TRUE ;
}

/*
 * SMat == TRUE ?
 * if Mat != [] and all the elements of Mat are != ""
 */

int nsp_smatrix_is_true(NspSMatrix *M)
{
  int i;
  if ( M->mn == 0) return FALSE;
  for ( i = 0 ; i < M->mn ; i++ )
    if ( strcmp(M->S[i],"") == 0) return FALSE;
  return TRUE;
}


/*
 * Save a String matrix NspSMatrix
 */

const int BUF_LEN=1024;

int nsp_smatrix_xdr_save(XDR *xdrs, NspSMatrix *M)
{
  int i;
#if 1
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_smatrix)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->n) == FAIL) return FAIL;
  for ( i= 0 ; i < M->mn ; i++)
    {
      if (0 &&  strlen(M->S[i]) > BUF_LEN )
	{
	  Scierror("XXX Lengh of element %d is too big (max=%d)\n",i, BUF_LEN ) ;
	  return FAIL;
	}
      nsp_xdr_save_string(xdrs,M->S[i]);
    }
  return OK;
}

/*
 * Load a NspSMatrix
 */

NspSMatrix *nsp_smatrix_xdr_load(XDR *xdrs)
{
  int m,n,i;
  NspSMatrix *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSMAT;
  if (nsp_xdr_load_i(xdrs,&m) == FAIL) return NULLSMAT;
  if (nsp_xdr_load_i(xdrs,&n) == FAIL) return NULLSMAT ;
  /* initial mxn matrix with unallocated elements **/
  if ( ( M =nsp_smatrix_create_with_length(name,m,n,-1) ) == NULLSMAT) return(NULLSMAT);
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < m*n ; i++ )
    {
#if 0
      char s_buf[BUF_LEN+1];
      if (nsp_xdr_load_string(xdrs,s_buf,BUF_LEN)== FAIL)
	{
	  Scierror("Warning: Lengh of element %d is too big (max=%d)\n",i, BUF_LEN ) ;
	  return NULLSMAT;
	}
      if ((M->S[i] =nsp_string_copy(s_buf)) == NULLSTRING ) return(NULLSMAT);
#else
      if (nsp_xdr_load_new_string(xdrs,&M->S[i]) == FAIL)
	return NULLSMAT;
#endif
    }
  return M;
}


/*
 * A =nsp_smatrix_object(O);
 * checks that O is an object of NspSMatrix type.
 * or a Hobj which points to an object of type SMatrix
 * if so, returns a pointer to that NspSMatrix and else returns NULL
 */

NspSMatrix   *nsp_smatrix_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_smatrix_id) == TRUE) return ((NspSMatrix *) O);
  else
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_smatrix));
  return(NULL);
}

/*
 * A =nsp_string_object(O);
 * checks that O is an object of type NspSMatrix (1x1)
 * or a Hobj which points to an object of type NspSMatrix
 *    if so, returns a pointer to that Matrix and else returns
 *    NULL
 */

char *nsp_string_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_smatrix_id) == TRUE
       && ((NspSMatrix *) O)->mn == 1 )
    return ((NspSMatrix *) O)->S[0];
  else
    Scierror("Error:\tArgument should be a string \n");
  return(NULL);
}

/*
 * IsSMatObj(stack,i)
 * only checks that object at position
 * first + i -1  is an object of type  SMatrix
 * or a Hobj which points to an object of type SMatrix
 */

int IsSMatObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_smatrix_id);
}

/*
 * IsSMat(O)
 * only checks that object is an object of type  SMatrix
 * or a Hobj which points to an object of type SMatrix
 */

int IsSMat(const NspObject *O)
{
  return nsp_object_type(O , nsp_type_smatrix_id);
}

int IsString(const NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,FALSE);
  /* Check type **/
  if ( check_cast(O,nsp_type_smatrix_id ) == TRUE
       && ((NspSMatrix *)O)->mn == 1)
    return TRUE;
  else
    return FALSE;
}

/**
 * GetSMatCopy:
 * @stack: calling stack
 * @i: an integer
 *
 * Checks that object @i on the stack
 * is a #NspSMatrix and returns that #NspSMatrix
 * or a copy of that #NspSMatrix if its name
 * is not NVOID
 *
 * Returns: a #NspMatrix
 **/

NspSMatrix*GetSMatCopy(Stack stack, int i)
{
  if (  GetSMat(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/**
 * GetSMat:
 * @stack: calling stack
 * @i: an integer
 *
 * Checks that first+i object on the stack
 * is a NspSMatrix and returns that NspSMatrix
 *
 * Returns: a #NspMatrix
 **/

NspSMatrix*GetSMat(Stack stack, int i)
{
  NspSMatrix *M;
  if (( M =nsp_smatrix_object(NthObj(i))) == NULLSMAT  )
    ArgMessage(stack,i);
  return M;
}

/**
 * GetString:
 * @stack: calling stack
 * @i: an integer
 *
 * Checks that first+i objects on the stack
 * is a string and returns a pointer to that string
 *
 * Returns: a string
 **/

char *GetString(Stack stack, int i)
{
  NspSMatrix *M;
  if (( M =nsp_smatrix_object(NthObj(i))) == NULLSMAT
      || ( M->mn != 1 ))
    {
      Scierror("Error:\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should be a string\n",NspFname(stack));
      return (char *) 0;
    }
  return M->S[0];
}

/**
 * GetStringInArray:
 * @stack:  calling stack
 * @ith: an integer
 * @Table: an null terminated array of string
 * @flag: an integer
 *
 * Checks that first+i objects on the stack
 * is a string which is in the array Table
 * Table: last entry must be NULL and there must not be duplicate entries.
 * this function returns the index of string in the array A or -1
 * Note that if flag == 0 abbreviation are accepted
 * if they do not lead to ambiguity
 *
 * Returns: an integer
 **/

int GetStringInArray(Stack stack, int ith,const nsp_const_string *Table, int flag)
{
  int rep ;
  char *key;
  const nsp_const_string *entry;
  if ((key = GetString(stack,ith)) == ((char *) 0) ) return -1;
  rep = is_string_in_array(key,Table,flag);
  if ( rep < 0 )
    {
      Scierror("Error:\t%s", ArgPosition(ith));
      ArgName(stack,ith);
      Scierror(" of function %s is %s\n",NspFname(stack),
	       ( rep == -2 ) ? "ambiguous " : "bad ") ;
      Scierror("\tmust be '%s'", *Table);
      for (entry = Table+1 ; *entry != NULL; entry++) {
	if (entry[1] == NULL) {
	  Scierror(", or '%s'",*entry);
	} else {
	  Scierror(", '%s'",*entry);
	}
      }
      if ( flag == 0 )
	Scierror("\n\tor an non ambiguous abbreviation\n");
      else
	Scierror("\n");
      return -1;
    }
  return rep;
}

/**
 * GetStringInStruct:
 * @stack:  calling stack
 * @ith: an integer
 * @T: a void *
 * @size: an integer
 * @flag: an integer
 *
 * Checks that object at position @i on the stack @stack
 * is a string which is in the array struct @T. We assume that
 * the elements of array @T are of size @size and the first element
 * of the struct is a string.
 * This function returns the index of string in the struct @T or -1
 * Note that if flag == 0 abbreviation are accepted
 * if they do not lead to ambiguity.
 *
 * Returns: an integer
 **/

int GetStringInStruct(Stack stack, int ith,void *T,unsigned int size, int flag)
{
  char **Table =(char **) T;
  int rep ;
  char *key, **entry;
  if ((key = GetString(stack,ith)) == ((char *) 0) ) return -1;
  rep = is_string_in_struct(key,(const void **)T,size,flag);
  if ( rep < 0 )
    {
      Scierror("Error:\t%s", ArgPosition(ith));
      ArgName(stack,ith);
      Scierror(" of function %s is %s\n",NspFname(stack),
	       ( rep == -2 ) ? "ambiguous " : "bad ") ;
      Scierror("\tmust be '%s'", *Table);
      for (entry = ((char **) (((char *) Table)+ size)) ; *entry != NULL ;
	   entry = ((char **) (((char *) entry)+ size))) {
	if (entry[1] == NULL) {
	  Scierror(", or '%s'",*entry);
	} else {
	  Scierror(", '%s'",*entry);
	}
      }
      if ( flag == 0 )
	Scierror("\n\tor a non ambiguous abbreviation\n");
      else
	Scierror("\n");
      return -1;
    }
  return rep;
}

/**
 * GetSMatUtf8:
 * @stack:  calling stack
 * @pos: an integer
 *
 * checks if object at position @pos on the stack @stack is an
 * utf8 string matrix or copy and convert the matrix if it is
 * a non utf8 string matrix.
 *
 * Returns: a new #NspSMatrix
 **/

NspSMatrix *GetSMatUtf8(Stack stack,int pos)
{
  NspSMatrix *Sm;
  if ((Sm = GetSMat(stack,pos)) == NULLSMAT) return NULLSMAT;
  if ( nsp_smatrix_utf8_validate(Sm) == FALSE )
    {
      /* need to copy first */
      if ((Sm = GetSMatCopy(stack,pos)) == NULLSMAT) return NULLSMAT;
      if ( nsp_smatrix_to_utf8(Sm) == FAIL)
 	{
 	  Scierror("%s: failed to convert %s to utf8\n",NspFname(stack),ArgPosition(pos));
 	  return NULLSMAT;
 	}
    }
  return Sm;
}

/**
 * GetStringUtf8:
 * @stack:  calling stack
 * @pos: an integer
 *
 * checks if object at position @pos on the stack @stack is an utf8 string
 * or copy and convert the string if it is a non utf8 string.
 *
 * Returns: a string pointer.
 **/

char *GetStringUtf8(Stack stack,int pos)
{
  NspSMatrix *Sm;
  if ((Sm = GetSMatUtf8(stack,pos)) == NULLSMAT) return NULL;
  if ( Sm->mn != 1 )
    {
      Scierror("%s: %s should be a string\n",NspFname(stack),ArgPosition(pos));
      return NULL;
    }
  return Sm->S[0];
}

/**
 * GetSMatCopyUtf8:
 * @stack:  calling stack
 * @pos: an integer
 *
 * returns a copy of a string matrix converted to utf8
 * or %NULL if object at position @pos on the stack is
 * not a string matrix
 *
 * Returns: a #NspSMatrix
 **/

NspSMatrix *GetSMatCopyUtf8(Stack stack,int pos)
{
  NspSMatrix *Sm;
  if ((Sm = GetSMatCopy(stack,pos)) == NULLSMAT) return NULLSMAT;
  if ( nsp_smatrix_utf8_validate(Sm) == FALSE )
    {
      if ( nsp_smatrix_to_utf8(Sm) == FAIL)
	{
	  Scierror("%s: failed to convert %s to utf8\n",NspFname(stack),ArgPosition(pos));
	  return NULLSMAT;
	}
    }
  return Sm;
}


/**
 * is_string_in_array:
 * @key: a constant string
 * @Table: an null terminated array of constant strings
 * @flag: an integer
 *
 * Lookups @key in the table @Table and accept unique
 * abbreviations unless flag == 1;
 * return value is >=0  -> index in table
 *              is  -1  -> bad key
 *              is  -2  -> ambiguous key
 * Note that if flag == 0 abbreviation are accepted
 * if they do not lead to ambiguity.
 *
 * Returns: an integer
 **/

int is_string_in_array(const char *key,const nsp_const_string Table[], int flag)
{
  int index = -1, numAbbrev=0, i;
  const char *p1, *p2;
  const nsp_const_string *entry;
  /*
   * Lookup the value of the object in the table.  Accept unique
   * abbreviations unless flag == 1;
   */
  for (entry = Table, i = 0; *entry != NULL; entry++, i++) {
    for (p1 = key, p2 = *entry; *p1 == *p2; p1++, p2++) {
      if (*p1 == 0) {
	/* exact match */
	index = i;
	return index;
      }
    }
    if (*p1 == 0) {
      /*
       * The value is an abbreviation for this entry.  Continue
       * checking other entries to make sure it's unique.  If we
       * get more than one unique abbreviation, keep searching to
       * see if there is an exact match, but remember the number
       * of unique abbreviations and don't allow either.
       */
      numAbbrev++;
      index = i;
    }
  }

  if ((flag == 1) || (numAbbrev != 1)) {
    /* error: no match or ambiguous match */
    if ( numAbbrev > 1)  return -2; else return -1;
  }
  return index;
}


/**
 * string_not_in_array:
 * @stack: nsp calling stack
 * @key:   key to search
 * @Table: array of char *
 * @message: error message
 *
 * used in conjunction with is_string_in_array for error
 * message.
 *
 **/

void string_not_in_array(Stack stack,const char *key,const nsp_const_string *Table,char *message)
{
  const nsp_const_string *entry;
  Scierror("Error:\t%s of function %s has a wrong value '%s'\n",message,NspFname(stack),key);
  Scierror("\texpected values are '%s'", *Table);
  for (entry = Table+1 ; *entry != NULL; entry++) {
    if (entry[1] == NULL) {
      Scierror(", or '%s'\n",*entry);
    } else {
      Scierror(", '%s'",*entry);
    }
  }
}


/**
 * is_string_in_struct:
 * @key: key to search
 * @Table: and array of struct, each element is of size @size and the last one must be %NULL.
 * @size: size of array elements in @Table
 * @flag: 1 for exact match, 0 to accept prefix match.
 *
 * returns the index of @key in table @Table. @Table is an array
 * of structs of size @size and each struct must start with a string
 * in order to be casted to a string. Thus each element in @Table can be
 * compared against @key. @Table should be ended by a %NULL element.
 *
 * Returns: -1 if fail or a non negative integer
 **/

int is_string_in_struct(const char *key,const void **Table,unsigned int size, int flag)
{
  int index = -1, numAbbrev=0, i;
  const char *p1, *p2;
  char **entry;
  /*
   * Lookup the value of the object in the table.  Accept unique
   * abbreviations unless flag == 1;
   */
  for (entry = (char **) Table, i = 0; *entry != NULL; entry = ((char **) (((char *) entry)+ size)), i++) {
    for (p1 = key, p2 = *entry; *p1 == *p2; p1++, p2++) {
      if (*p1 == 0) {
	/* exact match */
	index = i;
	return index;
      }
    }
    if (*p1 == 0) {
      /*
       * The value is an abbreviation for this entry.  Continue
       * checking other entries to make sure it's unique.  If we
       * get more than one unique abbreviation, keep searching to
       * see if there is an exact match, but remember the number
       * of unique abbreviations and don't allow either.
       */
      numAbbrev++;
      index = i;
    }
  }

  if ((flag == 1) || (numAbbrev != 1)) {
    /* error: no match or ambiguous match */
    if ( numAbbrev > 1)  return -2; else return -1;
  }
  return index;
}



/*
 * concat down: just a test
 * should add a flag to nsp_smatrix_resize
 * to decide if new strings are to be allocated or not
 * here they should not since Scopy will do the job
 */

extern int nsp_smatrix_concat_down1(NspSMatrix *A,NspSMatrix *B,int flag);

static int int_smatrix_concat_down(NspSMatrix *self,Stack stack,int rhs,int opt,int lhs)
{
  int flag = FALSE;
  NspSMatrix *A=self,*B;
  CheckRhs (1,1);
  CheckLhs (0,0);
  if ((B = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( A->n != B->n && A->mn != 0 )
    {
      Scierror("Error: [.;.] incompatible dimensions\n");
      return RET_BUG;
    }
  if ( B->mn == 0 )
    {
      MoveObj(stack,1,NSP_OBJECT(A));
      return 1;
    }
  if ( A == B )
    {
      if ((B = GetSMatCopy(stack,1)) == NULLSMAT) return RET_BUG;
    }
  if (strcmp(nsp_object_get_name(NSP_OBJECT(B)),NVOID) == 0)
    {
      flag = TRUE;
      NthObj(1)= NULLOBJ; /* B will be used and destroyed in nsp_smatrix_concat_down1 */
    }
  if ( nsp_smatrix_concat_down1(A,B,flag)== FAIL) return RET_BUG;
  /* if B was destroyed by nsp_smatrix_concat_down1 we must remove it from the
   * calling stack
   */
  MoveObj(stack,1,NSP_OBJECT(A));
  return 1;
}

static int int_meth_smatrix_has(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A = (NspSMatrix *) self, *x;
  NspBMatrix *B;
  NspMatrix *Ind,*Ind2;

  CheckRhs(1,1);
  CheckLhs(1,3);

  if ((x = GetSMat (stack, 1)) == NULLSMAT) return RET_BUG;

  if ( (B = nsp_smatrix_has(A, x, lhs, &Ind, &Ind2)) == NULLBMAT )
    return RET_BUG;

  MoveObj(stack,1,NSP_OBJECT(B));
  if ( lhs >= 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(Ind));
      if ( lhs == 3 )
	MoveObj(stack,3,NSP_OBJECT(Ind2));
    }

  return Max(lhs,1);
}

static int int_meth_smatrix_to_utf8(void *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( nsp_smatrix_to_utf8(self) == FAIL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(self));
  return Max(lhs,1);
}

static int int_meth_smatrix_to_latin1(void *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( nsp_smatrix_to_latin1(self) == FAIL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(self));
  return Max(lhs,1);
}

static int int_meth_smatrix_is_utf8(void *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1);
  if (nsp_move_boolean(stack,1,nsp_smatrix_utf8_validate(self))== FAIL)
    return RET_BUG;
  return Max(lhs,1);
}


static int int_meth_smatrix_split_nc(void *self, Stack stack, int rhs, int opt, int lhs)
{
  int nc;
  NspSMatrix *S,*A=self;
  CheckStdRhs(1,1);
  CheckLhs(0,1);
  if (GetScalarInt(stack,1,&nc) == FAIL) return RET_BUG;
  if ( A->mn != 1)
    {
      Scierror("Error: Object must be of size 1x1 for splitnc method\n");
      return RET_BUG;
    }
  if ((S= nsp_smatrix_split_nc( A->S[0],nc)) == NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}


static NspMethods smatrix_methods[] = {
  {"concatd",(nsp_method *) int_smatrix_concat_down},
  {"has",(nsp_method *) int_meth_smatrix_has},
  {"to_utf8",(nsp_method *) int_meth_smatrix_to_utf8},
  {"is_utf8",(nsp_method *) int_meth_smatrix_is_utf8},
  {"to_latin1",(nsp_method *) int_meth_smatrix_to_latin1},
  {"splitnc",(nsp_method *) int_meth_smatrix_split_nc},
  { NULL, NULL}
};


NspMethods *smatrix_get_methods(void) { return smatrix_methods;};


/*
 * Now the interfaced function for basic matrices operations
 */

/*
 * Creation of a NspSMatrix all the strings are created with "." value
 */

static int int_smatrix_create(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat;
  int m1,n1;
  char *str=0;
  int flag = 0;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if (GetScalarInt(stack,1,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( rhs == 3)
    {
      if ((str = GetString(stack,3)) == (char*)0) return RET_BUG;
      flag =1;
    }
  if ((HMat =nsp_smatrix_create(NVOID,Max(m1,0),Max(n1,0),str,flag))
      == NULLSMAT)
    return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat);
  return 1;
}

/*
 * Right Concatenation
 * A= [A,B]
 * return 0 on failure ( incompatible size or No more space )
 * provided through matint
 */


/*
 * Right Concatenation
 * Res = [A,B]  when A is a scalar matrix
 * A is converted to SMatrix.
 */

int int_smxconcatr_m_s(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_smatrix(HMat1,NULL,0)) == NULLSMAT) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if (nsp_smatrix_concat_right(Res,HMat2)!= OK) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Right Concatenation
 * Res = [A,B]  when B is a scalar matrix
 */

static int int_smxconcatr_s_m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat2;
  NspSMatrix * HMat1, *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSMat(stack,1))  == NULLSMAT) return RET_BUG;
  if ((HMat2 = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(NthObj(1))->ret_pos = 1;
      return 1;
    }
  if (( Res=nsp_matrix_to_smatrix(HMat2,NULL,0)) == NULLSMAT) return RET_BUG;
  if ( HMat1->mn != 0)
    {
      if (nsp_smatrix_concat_right(HMat1,Res)!= OK) return RET_BUG;
      nsp_smatrix_destroy(Res);
      NSP_OBJECT(NthObj(1))->ret_pos = 1;
    }
  else
    {
      MoveObj(stack,1,NSP_OBJECT(Res));
    }
  return 1;
}

/*
 * Down Concatenation
 * Res = [A;B]
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged
 * provided by matint
 */

/*
 * Down Concatenation
 * Res = [A;B]  when A is a scalar matrix
 * usefull when A=[]
 */

int int_smxconcatd_m_s(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat2,*Res;
  NspMatrix * HMat1;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( HMat1->mn == 0)
    {
      NSP_OBJECT(NthObj(2))->ret_pos = 1;
      return 1;
    }
  if ((HMat2 = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if (( Res=nsp_matrix_to_smatrix(HMat1,NULL,0)) == NULLSMAT) return RET_BUG;

  if ( HMat2->mn != 0)
    {
      if (nsp_smatrix_concat_down1(Res,HMat2,FALSE) != OK) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Down Concatenation
 * Res = [A;B]  when B is a scalar matrix
 */

static int int_smxconcatd_s_m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *HMat2;
  NspSMatrix * HMat1, *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((HMat1 = GetSMat(stack,1))  == NULLSMAT) return RET_BUG;
  if ((HMat2 = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  if ( HMat2->mn == 0)
    {
      NSP_OBJECT(NthObj(1))->ret_pos = 1;
      return 1;
    }
  if (( Res=nsp_matrix_to_smatrix(HMat2,NULL,0)) == NULLSMAT) return RET_BUG;
  if ( HMat1->mn != 0)
    {
      /* Res is used and destroyed */
      if (nsp_smatrix_concat_down1(HMat1,Res,TRUE)!= OK) return RET_BUG;
      NSP_OBJECT(NthObj(1))->ret_pos = 1;
    }
  else
    {
      MoveObj(stack,1,NSP_OBJECT(Res));
    }
  return 1;
}

/*
 *nsp_smatrix_add_columns: add n cols of zero to NspSMatrix A
 * A= [A,ones(m,n)]
 * return 0 on failure ( No more space )
 * A is changed
 */

static int int_smxaddcols(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspSMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetSMatCopy(stack,1))== NULLSMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ( nsp_smatrix_add_columns(HMat,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * AddRows : Add m rows of zero to a NspSMatrix A
 * A = [A;ones(m,n)]
 * return NULLSMat on failure ( incompatible size or No more space )
 * A and B are left unchanged
 */

static int int_smxaddrows(Stack stack, int rhs, int opt, int lhs)
{
  int m1;
  NspSMatrix *HMat;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( (HMat=GetSMatCopy(stack,1))== NULLSMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( nsp_smatrix_add_rows(HMat,m1) != OK) return RET_BUG; ;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}


/*
 * columns extraction for do loop
 * Cols A --> (Cols,A,Cols(A))
 * FIXME ne sert plus ?
 */

#if 0
static int int_smxextractcolforloop(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*Res;
  NspMatrix *Cols;
  int err=0;
  CheckRhs(2,2);
  CheckLhs(3,3);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((Cols = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  Res =nsp_smatrix_extract_columns( A,Cols,&err);
  if ( err == 1) return RET_ENDFOR;
  if ( Res == NULLSMAT) return RET_BUG;
  NthObj(3) = (NspObject *) Res;
  return 3;
}
#endif


/*
 *  diag function
 */

static int int_smatrix_diag (Stack stack, int rhs, int opt, int lhs)
{
  int k1 = 0;
  NspSMatrix *A, *Res;
  CheckRhs (1, 2);
  CheckLhs (1, 1);
  if (rhs == 2)
    {
      if (GetScalarInt (stack, 2, &k1) == FAIL)
	return RET_BUG;
    }
  if ((A = GetSMat (stack, 1)) == NULLSMAT)
    return RET_BUG;
  if (A->m == 1 || A->n == 1)
    Res = nsp_smatrix_create_diag (A, k1);
  else
    Res = nsp_smatrix_extract_diag (A, k1);

  if (Res == NULLSMAT)
    return RET_BUG;
  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

/*
 * Returns the kthe diag of a NspSMatrix
 */

static int int_smatrix_diage(Stack stack, int rhs, int opt, int lhs)
{
  int k1;
  NspSMatrix *A,*Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  Res =nsp_smatrix_extract_diag( A,k1);
  if ( Res == NULLSMAT)  return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 *  Creates a NspSMatrix with kth diag set to Diag
 */

static int int_smatrix_diagcre(Stack stack, int rhs, int opt, int lhs)
{
  int k1=0;
  NspSMatrix *Diag,*Res;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((Diag = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 2 )
    {
      if ( GetScalarInt(stack,2,&k1) == FAIL) return RET_BUG;
    }
  if ( (Res =nsp_smatrix_create_diag(Diag,k1)) == NULLSMAT ) return RET_BUG ;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 *nsp_smatrix_resize: Changes NspSMatrix dimensions
 * Warning : this routine only enlarges the array
 * of the NspSMatrix storage so as to contain mxn
 * elements : the previous datas are not moved and
 * occupy the first array cells
 * The NspSMatrix is changed
 * return 0 on failure
 */

static int int_smxresize(Stack stack, int rhs, int opt, int lhs)
{
  int m1,n1;
  NspSMatrix  *HMat;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ( (HMat=GetSMatCopy(stack,1))== NULLSMAT) return RET_BUG;
  if ( GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_smatrix_resize(HMat,m1,n1) != OK) return RET_BUG;
  NSP_OBJECT(HMat)->ret_pos = 1;
  return 1;
}

/*
 * int = SMatConcatTT
 * Term to term concatenation
 * A(i;j) = "A(i;j)cB(i;j)" : A is changed  B unchanged
 * C unchanged : c is a 1x1 string
 * C is used if flag == 1
 * here we need a column or row option XXXX
 *
 */

static int int_smxconcattt(Stack stack, int rhs, int opt, int lhs)
{
  char *str=NULL;
  int flag = 0;
  NspSMatrix *A,*B;
  CheckRhs(2,3);
  CheckLhs(1,1);
  if ((A = GetSMatCopy(stack,1))  == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if ( rhs == 3)
    {
      if ((str = GetString(stack,3)) == (char*)0) return RET_BUG;
      flag =1;
    }
  if ( B->mn == 0)
    {
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if ( A->mn == 0)
    {
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }

  if ( A->mn == 1 && B->mn != 1)
    {
      /* must copy B */
      if ((B = GetSMatCopy(stack,2)) == NULLSMAT) return RET_BUG;
      if (nsp_smatrix_concat_string_left(B,A,str,flag)== FAIL) return RET_BUG;
      NSP_OBJECT(B)->ret_pos = 1;
      return 1;
    }
  if ( B->mn == 1 && A->mn != 1)
    {
      if (nsp_smatrix_concat_string_right(A,B,str,flag)== FAIL) return RET_BUG;
      NSP_OBJECT(A)->ret_pos = 1;
      return 1;
    }
  if (nsp_smatrix_concat_strings(A,B,str,flag) == FAIL ) return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res =nsp_smatrix_strcmp(A,B) A and B are not changed
 *  Res[i;j] = strcmp(A[i;j],B[i;j])
 * XXXX strcmp
 */

static int int_smxcomp(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Res;
  NspSMatrix *A,*B;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1))  == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if (( Res=nsp_smatrix_strcmp(A,B) )== NULLMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * concat(A,[row='sep',col='sep',sep='sep'])
 */

static int int_smxconcat(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *rep ;
  NspSMatrix *A;
  char *col=NULL,*row=NULL,*sep=NULL;
  int_types T[] = {smat,new_opts, t_end} ;

  nsp_option opts[] ={{ "col",string,NULLOBJ,-1},
		      { "row",string,NULLOBJ,-1},
		      { "sep",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&A,&opts,&col,&row,&sep) == FAIL) return RET_BUG;
  if ( col  != NULL )
    {
      if ( row != NULL)
	{
	  nsp_string str;
	  if ((str=nsp_smatrix_elts_concat(A,row,1,col,1)) == NULL) return RET_BUG;
	  rep =nsp_create_object_from_str(NVOID,str);
	  nsp_string_destroy(&str);
	}
      else
	rep= (NspObject *)nsp_smatrix_column_concat(A,col,1);
    }
  else if ( row != NULL)
    {
      rep=(NspObject *)nsp_smatrix_row_concat(A,row,1);
    }
  else if ( sep != NULL)
    {
      nsp_string str;
      if ((str=nsp_smatrix_elts_concat(A,sep,1,sep,1)) == NULL) return RET_BUG;
      rep =nsp_create_object_from_str(NVOID,str);
      nsp_string_destroy(&str);
    }
  else
    {
      nsp_string str;
      if ((str=nsp_smatrix_elts_concat(A,sep,0,sep,0)) == NULL) return RET_BUG;
      rep =nsp_create_object_from_str(NVOID,str);
      nsp_string_destroy(&str);
    }

  if ( rep == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,rep);
  return 1;
}

/*
 * Res= Part(A,Ind)
 * part function of Scilab A is  unchanged
 * Ind unchanged
 */

static int int_smxpart(Stack stack, int rhs, int opt, int lhs)
{
  int rep=0;
  char *mode = "bytes";
  const char *modes[]={"bytes", "utf8",NULL};
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  int alloc = FALSE;
  NspMatrix *Ind;
  NspBMatrix *BElts=NULLBMAT;
  NspSMatrix *A;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if ((A= GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( IsBMatObj(stack,2)  )
    {
      /* Elts is boolean: use find(Elts) **/
      if ((BElts = GetBMat(stack,2)) == NULLBMAT)
	return RET_BUG;
      if ((Ind =nsp_bmatrix_find(BElts)) == NULLMAT)
	return RET_BUG;
      alloc = TRUE;
    }
  else
    {
      if ((Ind = GetRealMat(stack,2)) == NULLMAT)
	return RET_BUG;
    }

  if ( get_optional_args(stack, rhs, opt, opts, &mode) == FAIL )
    return RET_BUG;
  if ( rhs == 3 )
    {
      if ( (rep= is_string_in_array(mode,modes,0)) == -1 )
	{
	  string_not_in_array(stack, mode, modes, "optional argument mode");
	  return RET_BUG;
	}
    }
  if ( rep == 0 )
    {
      if (( A =nsp_smatrix_part(A,Ind)) == NULLSMAT)
	{
	  if ( alloc ) nsp_matrix_destroy(Ind) ;
	  return RET_BUG;
	}
    }
  else
    {
      if (( A =nsp_smatrix_part_utf8(A,Ind)) == NULLSMAT)
	{
	  if ( alloc ) nsp_matrix_destroy(Ind) ;
	  return RET_BUG;
	}
    }
  MoveObj(stack,1,(NspObject *) A);
  if ( alloc ) nsp_matrix_destroy(Ind) ;
  return 1;
}

/*
 * Res= length(A)
 * return a matrix which contains the length of the strings
 * contained in A
 * A unchanged
 */

static int int_smxlength(Stack stack, int rhs, int opt, int lhs)
{
  int rep=0;
  char *mode = "bytes";
  const char *modes[]={"bytes", "utf8",NULL};
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  NspMatrix *Res;
  NspSMatrix *A;
  CheckStdRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;

  if ( get_optional_args(stack, rhs, opt, opts, &mode) == FAIL )
    return RET_BUG;
  if ( rhs == 2 )
    {
      if ( (rep= is_string_in_array(mode,modes,0)) == -1 )
	{
	  string_not_in_array(stack, mode, modes, "optional argument mode");
	  return RET_BUG;
	}
    }
  if ( rep == 0 )
    {
      if (( Res=nsp_smatrix_elts_length(A)) == NULLMAT) return RET_BUG;
    }
  else
    {
      if (( Res=nsp_smatrix_elts_length_utf8(A)) == NULLMAT) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}

/*
 * Res=nsp_matrix_to_smatrix(A)
 * A is not changed
 */

static int int_smxm2sm(Stack stack, int rhs, int opt, int lhs)
{
  char *Format=NULL;
  NspMatrix *A;
  NspSMatrix *Res;
  int flag = 0;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if ((A = GetMat(stack,1))  == NULLMAT) return RET_BUG;
  if ( rhs == 2)
    {
      if ((Format = GetString(stack,2)) == (char*)0) return RET_BUG;
      flag =1;
    }
  if (( Res=nsp_matrix_to_smatrix(A,Format,flag)) == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) Res);
  return 1;
}


/*
 * Res= Mattoupper(A)
 */

static int int_smxtoupper(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSMatCopy(stack,1))  == NULLSMAT) return RET_BUG;
  nsp_smatrix_toupper(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}


/*
 * Res= Mattolower(A)
 */

static int int_smxtolower(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSMatCopy(stack,1))  == NULLSMAT) return RET_BUG;
  nsp_smatrix_tolower(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res= capitalize(A)
 */

static int int_smxcapitalize(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((A = GetSMatCopy(stack,1))  == NULLSMAT) return RET_BUG;
  nsp_smatrix_capitalize(A);
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}

/*
 * Res= isxxxx(A)
 */

typedef int (*IsF) (int c);

static int int_smx_isxxx(Stack stack, int rhs, int opt, int lhs, IsF F)
{
  int i;
  char *Str;
  NspBMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((Str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((A=nsp_bmatrix_create(NVOID,1,strlen(Str))) == NULLBMAT ) return RET_BUG;
  for ( i = 0 ; i < (int) strlen(Str) ; i++ )
    A->B[i] = (*F)( Str[i]) != 0 ? TRUE : FALSE ;
  MoveObj(stack,1,(NspObject *) A);
  return 1;
}


static int int_smxisalnum(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isalnum) ;
}

static int int_smxisalpha(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isalpha) ;
}

static int int_smxisascii(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isascii) ;
}

static int int_smxisdigit(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isdigit) ;
}

static int int_smxisgraph(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isgraph) ;
}

static int int_smxislower(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,islower) ;
}

static int int_smxisprint(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isprint) ;
}

static int int_smxispunct(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,ispunct) ;
}

static int int_smxisspace(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isspace) ;
}

static int int_smxisupper(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isupper) ;
}

static int int_smxisxdigit(Stack stack, int rhs, int opt, int lhs)
{
  return int_smx_isxxx(stack,rhs,opt,lhs,isxdigit) ;
}

/*
 * Res = strstr(A,str,mode=)
 * strstr(A,str)
 */

static int int_smxstrstr(Stack stack, int rhs, int opt, int lhs)
{
  int rep=0;
  char *mode = "bytes";
  const char *modes[]={"bytes", "utf8",NULL};
  nsp_option opts[] ={{"mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int i;
  char *Str;
  NspSMatrix *A;
  NspMatrix *B;
  CheckStdRhs(2,2);
  CheckLhs(1,1);
  if (( A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (( Str = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ( get_optional_args(stack, rhs, opt, opts, &mode) == FAIL )
    return RET_BUG;
  if ( rhs == 3 )
    {
      if ( (rep= is_string_in_array(mode,modes,0)) == -1 )
	{
	  string_not_in_array(stack, mode, modes, "optional argument mode");
	  return RET_BUG;
	}
    }

  if (( B =nsp_smatrix_strstr(A,Str)) == NULLMAT ) return RET_BUG;
  if ( rep == 1)
    {
      for (i=0; i < B->mn ; i++)
	{
	  if (B->R[i] != 0)
	    B->R[i] = nsp_string_utf8_pos(A->S[i], B->R[i]-1) +1;
	}
    }
  MoveObj(stack,1,(NspObject *) B);
  return 1;
}


/*
 * [index,pos) = strindex(str1,str2)
 * strindex(A,str)
 * str2 can be a string matrix.
 * same as in Scilab
 *
 */

static int int_smxstrindex(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  char *Str1;
  NspMatrix *ind=NULLMAT,*pos = NULLMAT;
  NspSMatrix *S;
  CheckRhs(2,2);
  CheckLhs(1,2);
  if ((Str1 = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((S=GetSMat(stack,2))== NULLSMAT) return RET_BUG;
  for ( i = 0 ; i < S->mn ; i++)
    {
      NspMatrix *ind1;
      if (( ind1 = nsp_smatrix_strindex(Str1,S->S[i])) == NULLMAT ) goto bug;
      if ( ind == NULLMAT )
	{
	  /* first call */
	  ind = ind1 ;
	  if ( lhs == 2 )
	    {
	      if ((pos = nsp_matrix_create(NVOID,'r',1,ind1->mn)) == NULLMAT) goto bug;
	      nsp_mat_set_rval(pos,(double) 1.00);
	    }
	}
      else
	{
	  /* add ind1 */
	  int n = ind1->mn,xof=ind->mn,j;
	  if ( n !=0 )
	    {
	      if ( nsp_matrix_concat_right(ind,ind1) == FAIL)
		{
		  nsp_matrix_destroy(ind1);
		  goto bug;
		}
	      if ( lhs == 2)
		{
		  if ( nsp_matrix_resize(pos,1,xof+n) == FAIL) goto bug;
		  for ( j=0; j < n ; j++) pos->R[j+xof]=(double)i+1;
		}
	    }
	  nsp_matrix_destroy(ind1);
	}
    }
  MoveObj(stack,1,(NspObject *) ind);
  if ( lhs == 2 )  MoveObj(stack,2,(NspObject *) pos);
  return Max(lhs,1);
 bug:
  if ( ind != NULLMAT) nsp_matrix_destroy(ind);
  if ( pos != NULLMAT) nsp_matrix_destroy(pos);
  return RET_BUG;
}


/*
 *nsp_smatrix_enlarge(A,m,n)
 *  changes A to B= [ A , 0; 0,0 ]  where 0 stands for "." strings
 *  in such a way that B (max(A->m,m)xmax(A->n,n));
 *  The result is stored in A
 * WARNING : no copy
 */

static int int_smxenlarge(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A;
  int m1,n1;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (GetScalarInt(stack,2,&m1) == FAIL) return RET_BUG;
  if (GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if ( nsp_smatrix_enlarge(A,m1,n1)== FAIL)  return RET_BUG;
  NSP_OBJECT(A)->ret_pos = 1;
  return 1;
}



/*
 * Ascii   txt <-> ascii
 */

static int int_smatrix_ascii_to_smatrix(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *A;
  NspSMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ( A->rc_type == 'c' )
    {
      Scierror("\t%s", ArgPosition(1));
      ArgName(stack,1);
      Scierror(" of function %s should not be complex\n",NspFname(stack));
      return RET_BUG;
    }
  if (( B =nsp_ascii_to_smatrix(A)) == NULLSMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;
}

static int int_smatrix_smatrix_to_ascii(Stack stack, int rhs, int opt, int lhs)
{
  char *Str;
  NspMatrix *B;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((Str = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((B =nsp_string_to_ascii(Str)) == NULLMAT ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;
}

static int int_smatrix_ascii(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ( IsMatObj(stack,1 ))
    return int_smatrix_ascii_to_smatrix(stack,rhs,opt,lhs);
  else
    return int_smatrix_smatrix_to_ascii(stack,rhs,opt,lhs);
}

/* FIXME
 * SMatSort
 * [A_sorted,Index]=sort(A, 'r'| 'c' | 'g' | 'lr'| 'lc' ,'i'|'d')
 */

static int int_smatrix_sort(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *M=NULL;
  NspObject *Index=NULLOBJ;
  const char *type_possible_choices[]={ "g", "gs", "c", "r", "lr" , "lc", NULL };
  char *type=NULL;
  const char *dir_possible_choices[]={ "i", "d",  NULL };
  char *dir=NULL;
  const char *ind_type_possible_choices[]={ "double", "int",  NULL };
  char *ind_type=NULL;
  int iflag = FALSE;
  char direction = 'd', itype = 'd';
  int rep_type= sort_g, rep_dir, rep_ind_type;

  CheckRhs(1,4);
  CheckOptRhs(0,3)
  CheckLhs(0,2);
   if ((M=GetSMatCopy(stack,1)) == NULLSMAT ) return RET_BUG;

  if ( rhs > 1 )
    {
      if ( rhs - opt >= 2  && opt >= 1 )
	{
	  Scierror ("Error:\t don't use both usual and named optional arguments (in function %s)\n", NspFname(stack));
	  return RET_BUG;
	}

      if ( opt == 0 )
	{
	  if ((type = GetString(stack,2)) == NULLSTRING) return RET_BUG;
	  if (rhs >= 3)
	    {
	      if ((dir = GetString(stack,3)) == NULLSTRING) return RET_BUG;
	      if ( rhs >= 4 )
		{
		  if ((ind_type = GetString(stack,4)) == NULLSTRING) return RET_BUG;
		}
	    }
	}
      else
	{
	  nsp_option opts[] ={{"type",string,NULLOBJ,-1},
			      {"dir",string,NULLOBJ,-1},
			      {"ind_type",string,NULLOBJ,-1},
			      { NULL,t_end,NULLOBJ,-1}};
	  if ( get_optional_args(stack, rhs, opt, opts, &type, &dir, &ind_type) == FAIL )
	    return RET_BUG;
	}

      /* verify optional arg*/
      if ( type != NULL )
	{
	  if ( (rep_type= is_string_in_array(type, type_possible_choices,1)) == -1 )
	    {
	      string_not_in_array(stack, type, type_possible_choices, "optional argument type");
	      return RET_BUG;
	    }
	}
      if ( dir != NULL )
	{
	  if ( (rep_dir= is_string_in_array(dir, dir_possible_choices,1)) == -1 )
	    {
	      string_not_in_array(stack, dir, dir_possible_choices, "optional argument dir");
	      return RET_BUG;
	    }
	  direction = dir_possible_choices[rep_dir][0];
	}
      if ( ind_type != NULL )
	{
	  if ( (rep_ind_type= is_string_in_array(ind_type, ind_type_possible_choices,1)) == -1 )
	    {
	      string_not_in_array(stack, ind_type, ind_type_possible_choices, "optional argument ind_type");
	      return RET_BUG;
	    }
	  itype = ind_type_possible_choices[rep_ind_type][0];
	}
    }

  if (lhs  == 2 || rep_type == 1 )  /* force index allocation for stable quick sort */
    {
      iflag = TRUE;
    }

  switch ( rep_type  )
    {
    case 0:
    case 1:
      nsp_smatrix_sort(M,&Index,iflag,direction,rep_type,itype);break;
    case 2:
      nsp_smatrix_row_sort(M,&Index,iflag,direction,itype);break;
    case 3:
      nsp_smatrix_column_sort(M,&Index,iflag,direction,itype);break;
    case 4:
      nsp_smatrix_lexical_row_sort(M,&Index,iflag,direction,itype);break;
    case 5:
      nsp_smatrix_lexical_column_sort(M,&Index,iflag,direction,itype);break;
    }

  if ( iflag == TRUE && Index == NULL) return RET_BUG;
  NSP_OBJECT(M)->ret_pos = 1;

  if ( lhs == 2 )
    {
      if ( itype == 'd' ) /* back convert */
	Index = (NspObject *) Mat2double( (NspMatrix *) Index);
      MoveObj(stack,2,Index);
    }
  return Max(lhs,1);
}


/*
 * SMatSplit
 * [A]=split(str,sep='splitchars',msep=bool)
 */

static int int_smxsplit(Stack stack, int rhs, int opt, int lhs)
{
  char *defsplit = " \n\t\r";
  char *sep=NULL;
  Boolean msep=FALSE;
  NspSMatrix *A;
  NspSMatrix *Src;
  int_types T[] = {smat, new_opts, t_end};
  nsp_option opts[] ={{ "msep",s_bool,NULLOBJ,-1},
		      { "sep",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&Src,&opts,&msep,&sep) == FAIL)
    return RET_BUG;
  if ( sep  == NULL )
    sep = defsplit;

  if ( (A=nsp_smatrix_split(Src,sep,msep)) == NULLSMAT )
    return RET_BUG;
  MoveObj(stack,1,(NspObject *)A);
  return 1;
}

/*
 * Operation leading to Boolean result
 */

/* A < B */

int int_smxlt(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,"<");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_smxle(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,"<=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_smxneq(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,"<>");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_smxeq(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,"==");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_smxgt(Stack stack, int rhs, int opt, int lhs)
{

  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,">");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}


static int int_smxge(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *A,*B; NspBMatrix *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  Res = SMatCompOp(A,B,">=");
  if ( Res == NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

/*
 * Same but returns a unique boolean
 */

static int int_smxf_gen(Stack stack, int rhs, int opt, int lhs,char *op)
{
  int rep,err;
  NspSMatrix *A,*B; NspObject *Res;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((A = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((B = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  rep = SMatFullComp(A,B,op,&err);
  if ( err == 1)
    {
      Scierror("Error: operator %s , arguments with incompatible dimensions\n",op);
      return RET_BUG;
    }
  if ( rep == TRUE )
    {
      if (( Res =nsp_create_true_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  else
    {
      if (( Res =nsp_create_false_object(NVOID)) == NULLOBJ) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *)Res);
  return 1;
}

static int int_smxflt(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,"<");
}

static int int_smxfle(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,"<=");
}


static int int_smxfneq(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,"<>");
}

static int int_smxfeq(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,"==");
}

static int int_smxfgt(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,">");
}

static int int_smxfge(Stack stack, int rhs, int opt, int lhs)
{
  return int_smxf_gen(stack,rhs,opt,lhs,">=");
}



/*
 * Res =nsp_smatrix_copy(A)
 * Creates a Copy of NspSMatrix A : A is not checked
 */

static int int_smxtranspose(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat1,*HMat2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( HMat1 = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (( HMat2 =nsp_smatrix_transpose(HMat1))  == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) HMat2);
  return 1;
}


/*
 *
 */

static int int_smatrix_2latexmat(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  nsp_smatrix_latex_print(HMat,0,NULL,0);
  return 0;
}

/*
 *  BMat2LaTeXTab: writes BMat Objet on fd in TeX language
 */

static int int_smatrix_2latextab(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((HMat = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  nsp_smatrix_latex_tab_print(HMat,0,NULL,0);
  return 0;
}

/*
 * Res =  subst(A,str,rep)
 */

static int int_smxsubst(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat1,*Res1=NULL,*Res2=NULL,*Str,*Rep;
  int i;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if (( HMat1 = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (( Str =  GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if (( Rep =  GetSMat(stack,3)) == NULLSMAT) return RET_BUG;
  if ( Str->mn != Rep->mn )
    {
      Scierror("Error: second and third arguments should have the same size\n");
      return RET_BUG;
    }
  Res1=HMat1;
  for ( i = 0 ; i < Str->mn ; i++)
    {
      if ((Res2 =nsp_smatrix_subst(Res1,Str->S[i],Rep->S[i])) == NULLSMAT) return RET_BUG;
      if ( Res1 != HMat1 ) nsp_smatrix_destroy(Res1);
      Res1 = Res2;
    }
  if ( Str->mn == 0 )
    {
      ((NspObject *) HMat1)->ret_pos = 1;
    }
  else
    MoveObj(stack,1,(NspObject *) Res2);
  return 1;
}

/*
 * Res =  stripblanks(A [, bool])
 */

static int int_smxstripblanks(Stack stack, int rhs, int opt, int lhs)
{
  int tab=TRUE;
  NspSMatrix *HMat1;
  CheckRhs(1,2);
  CheckLhs(1,1);
  if (( HMat1 = GetSMatCopy(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 2 )
    {
      if ( GetScalarBool(stack,2,&tab) == FAIL )
	return RET_BUG;
    }
  NSP_OBJECT(HMat1)->ret_pos = 1;
  if (nsp_smatrix_strip_blanks(HMat1,tab) == FAIL) return RET_BUG;
  return 1;
}

/*
 *
 */

static int int_smatrix_utf8_from_unichar(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *loc;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( A = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if (( loc = nsp_smatrix_utf8_from_unichar(A)) == NULLSMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(loc));
  return 1;
}


/*
 *
 */

static int int_smatrix_strtod(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *loc;
  NspMatrix *A;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( loc = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if (( A = nsp_smatrix_strtod(loc))== NULLMAT) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(A));
  return 1;
}

static int int_smatrix_protect(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *rep ;
  nsp_string str1,str2;
  if ((str1 = GetString(stack,1)) == ((char *) 0) )
    return RET_BUG;
  if ((str2 =nsp_string_protect(str1)) == NULL)
    return RET_BUG;
  rep =nsp_create_object_from_str(NVOID,str2);
  MoveObj(stack,1,rep);
  return 1;
}

static int int_bpsqsort( Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *x;
  NspMatrix *ind=NULLMAT;
  int *index;

  if (( x = GetSMatCopy(stack,1)) == NULLSMAT) return RET_BUG;

  CheckLhs(1,2);

  if ( (ind = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
    return RET_BUG;

  index = (int *) ind->R;

  nsp_sqsort_bp_nsp_string( x->S,  x->mn, index, 'i');

  NSP_OBJECT(x)->ret_pos = 1;

  if ( lhs >= 2 )
    {
      ind->convert = 'i';
      ind = Mat2double(ind);
      MoveObj(stack,2,NSP_OBJECT(ind));
    }
  else
    nsp_matrix_destroy(ind);

  return Max(lhs,1);
}


static int int_smatrix_unique( Stack stack, int rhs, int opt, int lhs)
{
  Boolean first_ind;
  NspSMatrix *x;
  NspMatrix *occ, **Occ=NULL;
  NspObject *ind, **Ind=NULL;
  int_types T[] = {smatcopy,new_opts,t_end} ;
  nsp_option opts[] ={{ "first_ind",s_bool,NULLOBJ,-1},
		      { "ind_type",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  char *ind_type=NULL, itype='d';
  const char *ind_type_possible_choices[]={ "double", "int",  NULL };
  int rep_ind_type;

  if ( GetArgs(stack,rhs,opt,T,&x,&opts,&first_ind,&ind_type) == FAIL )
    return RET_BUG;

  if ( opts[0].obj == NULLOBJ) first_ind = FALSE;

  if ( ind_type != NULL )
    {
      if ( (rep_ind_type= is_string_in_array(ind_type, ind_type_possible_choices,1)) == -1 )
	{
	  string_not_in_array(stack, ind_type, ind_type_possible_choices, "optional argument ind_type");
	  return RET_BUG;
	}
      itype = ind_type_possible_choices[rep_ind_type][0];
    }

  CheckLhs(1,3);

  if ( lhs >= 2 )
    {
      Ind = &ind;
      if ( lhs == 3 ) Occ = &occ;
    }

  if ( nsp_smatrix_unique(x, Ind, Occ, first_ind, itype) == FAIL )
    return RET_BUG;

  NSP_OBJECT(x)->ret_pos = 1;
  if ( lhs >= 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(ind));
      if ( lhs >= 3 )
	MoveObj(stack,3,NSP_OBJECT(occ));
    }

  return Max(lhs,1);
}


static int
int_smatrix_issorted (Stack stack, int rhs, int opt, int lhs)
{
  char *flag=NULL;
  int rep = test_sort_g;
  const char *flags_list[]={ "g", "c", "r", "lc", "lr", NULL};
  Boolean strict_order = FALSE;
  NspSMatrix *A;
  NspBMatrix *Res;
  nsp_option opts[] ={{"flag",string,NULLOBJ,-1},
		      {"strict_order",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(1, 1);
  CheckOptRhs(0, 2)
  CheckLhs(1, 1);

  if ((A = GetSMat (stack, 1)) == NULLSMAT)
    return RET_BUG;

  if ( get_optional_args(stack, rhs, opt, opts, &flag, &strict_order) == FAIL )
    return RET_BUG;

  if ( flag != NULL)
    {
      rep = is_string_in_array(flag, flags_list, 1);
      if ( rep < 0 )
	{
	  string_not_in_array(stack, flag, flags_list, "optional argument flag");
	  return RET_BUG;
	}
    }

  if ( (Res = nsp_smatrix_issorted(A, rep, strict_order)) == NULLBMAT )
    return RET_BUG;

  MoveObj (stack, 1, (NspObject *) Res);
  return 1;
}

static int
int_is_string_in_array (Stack stack, int rhs, int opt, int lhs)
{
  Boolean abbrev = TRUE;
  char *key = NULL;
  NspSMatrix *Table, *names=NULLSMAT;
  nsp_option opts[] ={{"abbrev",s_bool,NULLOBJ,-1},
		      {"names",smat,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  int rep, k;
  CheckStdRhs(2, 2);
  CheckOptRhs(0, 3)
  CheckLhs(1, 1);

  if ((key = GetString(stack,1)) == ((char *) 0) )
    return RET_BUG;
  if ((Table = GetSMat(stack, 2)) == NULLSMAT)
    return RET_BUG;

  if ( get_optional_args(stack, rhs, opt, opts, &abbrev, &names) == FAIL )
    return RET_BUG;
  if ( names != NULLSMAT &&  names->mn != 2 )
    {
      Scierror("Error:\t optional argument 'names' of function is_string_in_array should have 2 strings\n");
      return RET_BUG;
    }

  rep = is_string_in_array(key,(const char **) Table->S, !abbrev);
  if ( rep < 0 && names != NULLSMAT )  /* in this case an automatic treatment of the error is done */
    {
      Scierror("Error:\t argument %s of function %s has a wrong value '%s'\n",names->S[0],names->S[1],key);
      Scierror("\texpected values are ");
      for (k = 0 ; k < Table->mn ; k++ )
	Scierror(" '%s' ",Table->S[k]);
      Scierror("\n");
      return RET_BUG;
    }

  if ( nsp_move_double(stack,1, (double) rep+1) == FAIL )
    return RET_BUG;
  return 1;
}

static int
int_parse_dim_arg(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Obj = NULLOBJ;
  int dim;
  NspSMatrix *names=NULLSMAT;
  nsp_option opts[] ={{"names",smat,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckStdRhs(1,1);
  CheckOptRhs(0, 3)
  CheckLhs(1,1);

  if ( (Obj = nsp_get_object(stack, 1)) == NULLOBJ )
    return RET_BUG;

  if ( get_optional_args(stack, rhs, opt, opts, &names) == FAIL )
    return RET_BUG;

  if ( names != NULLSMAT &&  names->mn != 2 )
    {
      Scierror("Error:\t optional argument 'names' of function parse_dim_arg should have 2 strings\n");
      return RET_BUG;
    }

  if ( DimArg(Obj, &dim) == FAIL )
    {
      if ( names != NULLSMAT )
	{
	  /* add an error message on the error message sends by DimArg func */
	  Scierror("\terror concerns argument %s of function %s\n",names->S[0],names->S[1]);
	}
      return RET_BUG;
    }

  if ( nsp_move_double(stack,1, (double) dim) == FAIL )
    return RET_BUG;

  return 1;
}

static int int_latex_codes(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *Res;
  CheckStdRhs(0,0);
  CheckLhs(1,1);
  if ((Res = nsp_latex_utf8_symbols()) == NULL)
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

static int int_smatrix_to_base64(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspSMatrix *A,*Res;
  CheckStdRhs(1, 1);
  CheckLhs(1, 1);
  if ((A = GetSMat (stack, 1)) == NULLSMAT)
    return RET_BUG;
  if ((Res = nsp_smatrix_create_with_length(NVOID,A->m,A->n,-1))== NULL)
    return RET_BUG;
  for ( i = 0 ; i < A->mn ; i++)
    {
      Res->S[i]=nsp_string_to_base64string(A->S[i],strlen(A->S[i]));
      if ( Res->S[i] == NULL )
	{
	  nsp_smatrix_destroy(A);
	  return RET_BUG;
	}
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}

static int int_base64_to_smatrix(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspSMatrix *A,*Res;
  CheckStdRhs(1, 1);
  CheckLhs(1, 1);
  if ((A = GetSMat (stack, 1)) == NULLSMAT)
    return RET_BUG;
  if ((Res = nsp_smatrix_create_with_length(NVOID,A->m,A->n,-1))== NULL)
    return RET_BUG;
  for ( i = 0 ; i < A->mn ; i++)
    {
      int out;
      Res->S[i]=nsp_base64string_to_nsp_string(A->S[i],&out);
      if ( Res->S[i] == NULL )
	{
	  nsp_smatrix_destroy(A);
	  return RET_BUG;
	}
    }
  MoveObj(stack,1,NSP_OBJECT(Res));
  return 1;
}


/*
 * push the smatrix  elements on the stack
 */

static int int_smatrix_to_seq (Stack stack, int rhs, int opt, int lhs)
{
  int i,j;
  NspSMatrix *M;
  CheckRhs (1, 1);
  if ((M = GetSMat(stack, 1)) == NULLSMAT ) return RET_BUG;
  for ( i=0 ; i < M->mn ; i++)
    {
      NthObj(i+2)= nsp_create_object_from_str(NVOID,M->S[i]);
      NthObj(i+2)->ret_pos = i+1;
      if ( NthObj(i+2) == NULLOBJ ) { goto bug;}
    }
  return M->mn ;
 bug:
  for ( j= 2 ; j <= i +1  ; j++)
    {
      M = (NspSMatrix *) NthObj(j);
      nsp_smatrix_destroy(M);
    }
  return RET_BUG;
}


/*
 * push the smatrix  elements on the stack
 */

static int int_print_string_as_read (Stack stack, int rhs, int opt, int lhs)
{
  char *str;
  CheckRhs (1, 1);
  if ((str = GetString(stack,1)) == ((char *) 0) )
    return RET_BUG;
  nsp_print_string_as_read(str,'\"');
  return 0;
}


/* code a nsp matrix in C 
 * the code was obtained with 
 * printf(f,as_read=%t,base64=%t);
 */
/* 
static char *x[]=
  { "QG5zcDAxAAAADgAAAAJmAAAAAAAAAQAAAABMAAAATwAAAAAAAAP///9jAAAAAkwAAABPAA",
    "AAAAAAAv///00AAAACTAAAAE8AAAAAAAAB////PAAAAAJOAAAAAAAAAnkAAAAAAAAFRQAA",
    "AEwAAABPAAAAAAAAAv///zgAAAACTgAAAAAAAAJmAAAA/////04AAAAAAAACeAAAAAAAAA",
    "ZFAAAARQAAAEwAAABPAAAAAAAAAf///z4AAAACTAAAAE8AAAAAAAABAAAAjAAAAAJMAAAA",
    "TwAAAAAAAAL///9NAAAAAkwAAABPAAAAAAAAAf///zwAAAACTgAAAAAAAAJ5AAAAAAAABU",
    "UAAABMAAAATwAAAAAAAAL///9MAAAAAk4AAAAAAAAEc2luAP////9MAAAATwAAAAAAAAH/",
    "//9DAAAAAk4AAAAAAAACeAAAAAAAAAZFAAAARQAAAEUAAABFAAAARQAAAEIAAAAAAH//AA",
    "AABkNlbGxzAAAAAAAACHN5bWJvbHMAAAAAAwAAAAFZAAAAAAB//wAAAAZCSGFzaAAAAAAA",
    "AANzdAAAAAAACwAAAAYAAAACeQAAAAAAAAUAAAAEYW5zAAAAAAEAAAAIbmFyZ29wdAAAAA",
    "AEAAAAAngAAAAAAAAGAAAACG5hcmdvdXQAAAAAAwAAAAduYXJnaW4AAAAAAAJZAAAAAAB/",
    "/wAAAAZDZWxscwAAAAAAAAdsb2NhbHMAAAAAAAcAAAABTgAAAE4AAABOAAAATgAAAE4AAA",
    "BOAAAATgAAAFkAAAAAAH//AAAABkNlbGxzAAAAAAAADHBlcnNpc3RlbnRzAAAAAAcAAAAB",
    "TgAAAE4AAABOAAAATgAAAE4AAABOAAAATgAAAEUAAABaAAAA" };

static int int_decode (Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *res;
  if ((res = nsp_smatrix_create_from_const_table( (const char * const *) x))==NULL)
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(res));
  return 1;
}
*/

/*
 * The Interface for basic matrices operation
 */

static OpTab SMatrix_func[]={
  {"latex_codes", int_latex_codes},
  {"extract_s", int_matint_extract},
  {"extractelts_s", int_matint_extractelts},
  {"extractcols_s", int_matint_extractcols},
  {"extractrows_s", int_matint_extractrows_pointer},
  {"resize2vect_s", int_matint_resize2vect},
  {"setrowscols_s", int_matint_setrowscols},
  {"deleteelts_s", int_matint_deleteelts},
  {"deleterows_s", int_matint_deleterows},
  {"deletecols_s", int_matint_deletecols},
  {"tozero_s", int_matint_tozero},
  {"repmat_s_m", int_matint_repmat},
  {"latexmat_s",int_smatrix_2latexmat},
  {"latextab_s",int_smatrix_2latextab},
  /* {"loopextract_m_s",int_smxextractcolforloop}, */
  {"smat_create_m",int_smatrix_create},
  {"redim_s",int_matint_redim},
  {"matrix_s", int_matint_redim},
  {"reshape_s", int_matint_redim},
  {"concatr_s_s",int_matint_concatr},
  {"concatr_m_s",int_smxconcatr_m_s},
  {"concatr_s_m",int_smxconcatr_s_m},
  {"addcols_s_m",int_smxaddcols},
  {"concatd_s_s", int_matint_concatd}, /*  int_smxconcatd}, */
  {"concatd_s_m",  int_smxconcatd_s_m}, /*  int_smxconcatd}, */
  {"concatd_m_s", int_smxconcatd_m_s},
  {"concatdiag_s_s",int_matint_concat_diag}, /* int_mxconcatdiag}, */
  {"isvector_s", int_matint_isvector},
  {"addrows_s",int_smxaddrows},
  {"resize_s",int_smxresize},
  {"concat_s_s", int_smxconcattt },
  {"plus_s_s", int_smxconcattt },
  {"comp_s_s", int_smxcomp },
  {"strcmp_s_s", int_smxcomp },
  {"catenate", int_smxconcat },
  {"part", int_smxpart },
  {"length_s", int_smxlength },
  {"m2s", int_smxm2sm },
  {"enlarge_s", int_smxenlarge },
  {"isalnum", int_smxisalnum},
  {"isalpha",int_smxisalpha},
  {"isascii",int_smxisascii},
  {"isdigit",int_smxisdigit},
  {"isgraph",int_smxisgraph},
  {"islower",int_smxislower},
  {"isprint",int_smxisprint},
  {"ispunct",int_smxispunct},
  {"isspace",int_smxisspace},
  {"isupper",int_smxisupper},
  {"isxdigit",int_smxisxdigit},
  {"tolower",int_smxtolower},
  {"toupper",int_smxtoupper},
  {"capitalize",int_smxcapitalize},
  {"strstr",int_smxstrstr},
  {"strindex",int_smxstrindex},
  {"ascii",int_smatrix_ascii},
  {"split",int_smxsplit},
  {"eq_s_s" ,  int_smxeq },
  {"feq_s_s" ,  int_smxfeq },
  {"fge_s_s" ,  int_smxfge },
  {"fgt_s_s" ,  int_smxfgt },
  {"fle_s_s" ,  int_smxfle },
  {"flt_s_s" ,  int_smxflt },
  {"fneq_s_s" ,  int_smxfneq },
  {"ge_s_s" ,  int_smxge },
  {"gt_s_s" ,  int_smxgt },
  {"le_s_s" ,  int_smxle },
  {"lt_s_s" ,  int_smxlt },
  {"ne_s_s" ,  int_smxneq },
  {"quote_s", int_smxtranspose},
  {"strsubst",int_smxsubst},
  {"stripblanks",int_smxstripblanks},
  {"sort_s", int_smatrix_sort},
  {"gsort_s", int_smatrix_sort},
  {"new_sort_s", int_smatrix_sort },
  {"unichar_to_utf8", int_smatrix_utf8_from_unichar},
  {"strtod",int_smatrix_strtod},
  {"protect",int_smatrix_protect}, /* test */
  {"sqsort_s", int_bpsqsort},
  {"unique_s",int_smatrix_unique},
  {"issorted_s",int_smatrix_issorted},
  {"is_string_in_array_s",int_is_string_in_array},
  {"parse_dim_arg",int_parse_dim_arg},
  {"diag_s", int_smatrix_diag},
  {"diag_s_m", int_smatrix_diag},
  {"diagcre_s",int_smatrix_diagcre},
  {"diage_s",int_smatrix_diage},
  {"object2seq_s",int_smatrix_to_seq}, /* A{...} on rhs  */
  {"s2base64",int_smatrix_to_base64},
  {"base642s",int_base64_to_smatrix},
  {"print_string_as_read",int_print_string_as_read},
  /* {"decode", int_decode}, */
  {(char *) 0, NULL}
};

int SMatrix_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SMatrix_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table
   (for adding or removing functions) */

void SMatrix_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SMatrix_func[i].name;
  *f = SMatrix_func[i].fonc;
}
