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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SMatrix_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/matint.h"

/*
 * NspSMatrix inherits from NspObject 
 */

int nsp_type_smatrix_id=0;
NspTypeSMatrix *nsp_type_smatrix=NULL;
int nsp_type_smatrix_init();

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
  type->methods = NULL ; /* smatrix_get_methods; */
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
  mati->redim = (matint_redim *) nsp_smatrix_redim; 
  mati->resize = (matint_resize  *) nsp_smatrix_resize; 
  mati->free_elt = (matint_free_elt *) nsp_string_destroy;
  mati->elt_size = (matint_elt_size *) nsp_smatrix_elt_size ;

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

char *nsp_smatrix_type_short_string(void)
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
  rep = SMatFullComp((NspSMatrix *) A,(NspSMatrix *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

int nsp_smatrix_neq(NspObject *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_smatrix_id) == FALSE) return TRUE;
  rep = SMatFullComp((NspSMatrix *) A,(NspSMatrix *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
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
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->m) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->n) == FAIL) return FAIL;
  for ( i= 0 ; i < M->mn ; i++) 
    {
      if ( strlen(M->S[i]) > BUF_LEN ) 
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
  char s_buf[BUF_LEN+1];
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
      if (nsp_xdr_load_string(xdrs,s_buf,BUF_LEN)== FAIL) 
	{
	  Scierror("Warning: Lengh of element %d is too big (max=%d)\n",i, BUF_LEN ) ;
	  return NULLSMAT;
	}
      if ((M->S[i] =nsp_string_copy(s_buf)) == NULLSTRING ) return(NULLSMAT);
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

int IsSMat(NspObject *O)
{
  return nsp_object_type(O , nsp_type_smatrix_id);
}

int IsString(NspObject *O)
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

/*
 * Checks that first+i object on the stack 
 * is a NspSMatrix and returns that NspSMatrix  
 * or a copy of that NspSMatrix if its name 
 * is != NVOID 
 */

NspSMatrix*GetSMatCopy(Stack stack, int i)
{
  if (  GetSMat(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that first+i object on the stack 
 * is a NspSMatrix and returns that NspSMatrix  
 */

NspSMatrix*GetSMat(Stack stack, int i)
{
  NspSMatrix *M;
  if (( M =nsp_smatrix_object(NthObj(i))) == NULLSMAT  )
    ArgMessage(stack,i);
  return M;
}

/*
 * Checks that first+i objects on the stack 
 * is a string and returns a pointer to that string
 */

char *GetString(Stack stack, int i)
{
  NspSMatrix *M;
  if (( M =nsp_smatrix_object(NthObj(i))) == NULLSMAT 
      || ( M->mn != 1 ))
    {
      Scierror("Error:\t%s", ArgPosition(i));
      ArgName(stack,i);
      Scierror(" of function %s should be a string\n",stack.fname);
      return (char *) 0;
    }
  return M->S[0];
}

/*
 * Checks that first+i objects on the stack 
 * is a string which is in the array Table
 * Table: last entry must be NULL and there must not be duplicate entries.
 * this function returns the index of string in the array A or -1 
 * Note that if flag == 0 abbreviation are accepted 
 * if they do not lead to ambiguity 
 */

int GetStringInArray(Stack stack, int ith, char **Table, int flag)
{
  int rep ;
  char *key, **entry;
  if ((key = GetString(stack,ith)) == ((char *) 0) ) return -1;
  rep = is_string_in_array(key,Table,flag);
  if ( rep < 0 ) 
    {
      Scierror("Error:\t%s", ArgPosition(ith));
      ArgName(stack,ith);
      Scierror(" of function %s is %s\n",stack.fname,
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

int GetStringInStruct(Stack stack, int ith,void *T,unsigned int size, int flag) 
{
  char **Table =(char **) T;
  int rep ;
  char *key, **entry;
  if ((key = GetString(stack,ith)) == ((char *) 0) ) return -1;
  rep = is_string_in_struct(key,(void **)T,size,flag);
  if ( rep < 0 ) 
    {
      Scierror("Error:\t%s", ArgPosition(ith));
      ArgName(stack,ith);
      Scierror(" of function %s is %s\n",stack.fname,
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


/*
 * Lookup str in the table Table (null terminated).  Accept unique
 * abbreviations unless flag == 1;
 * return value is >=0  -> index in table 
 *              is  -1  -> bad key 
 *              is  -2  -> ambiguous key 
 * Note that if flag == 0 abbreviation are accepted 
 * if they do not lead to ambiguity. 
 */

int is_string_in_array(const char *key, char **Table, int flag)
{
  int index = -1, numAbbrev=0, i;
  const char *p1, *p2;
  char **entry;
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

void string_not_in_array(Stack stack,const char *key, char **Table,char *message)
{
  char **entry;
  Scierror("Error:\t%s of function %s has a wrong value %s\n",message,stack.fname,key);
  Scierror("\texpected values are '%s'", *Table);
  for (entry = Table+1 ; *entry != NULL; entry++) {
    if (entry[1] == NULL) {
      Scierror(", or '%s'\n",*entry);
    } else {
      Scierror(", '%s'",*entry);
    }
  }
}

/* Table : Array of object which can be casted to string  to compare against str
 * size gives the object size 
 * last entry must be NULL
 * and there must not be duplicate entries.
 * 0 or 1, 1 for exact match 
 */

int is_string_in_struct(const char *key,void **Table,unsigned int size, int flag)
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


