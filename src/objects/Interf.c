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

/*--------------------------------------------------------------------
 * This file contains utility functions for writting interfaces 
 * some macros are also provided in Interf.h 
 * 
 * GetArgs: checks calling sequence using a table of types 
 * GetListArgs(NspList *L,int pos,int_types *T,...);
 * BuildListFromArgs(int_types *T,...);
 * int get_optional_args(Stack stack,int rhs,int opt,nsp_option opts[],...)
 *
 * MoveObj(Stack stack, int j, NspObject *O)
 * int nsp_move_string(Stack stack,int n,char *bytes,int length)
 * int nsp_move_double(Stack stack,int n,double d)
 * int nsp_move_boolean(Stack stack,int n,int ival)
 * ObjConvert(NspObject *O)
 * MaybeObjCopy(NspObject **O)
 * ArgPosition(int i)
 * ArgName(Stack stack, int i)
 * ArgMessage(Stack stack, int i)
 * attr_search(char *key, AttrTab *Table)
 * attrs_to_stack(char *key, AttrTab *attrs, Stack stack, int pos)
 * 
 * private or not really importants : XXXX
 *
 * GetFromTable: check types in Object array 
 * SwapObjs(Stack stack, int i, int j)
 * ObjPerm(Stack stack, int nv, int *ind)
 * PutLhsObj(Stack stack, int nv, int *ind)
 * OptCheck(NspObject **Os, NspObject **DefO, char **Names, int n, Stack stack, int nopt)
 * OptCheck1(Stack stack,int rhs, int nopt, named_opts *Opts)
 *--------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/object.h" 
#include "nsp/matutil.h"

static int OptCheck1(Stack stack,int rhs, int nopt, named_opts *Opts);
static int GetFromTable_1(NspObject **Objs,int_types *T,va_list *ap,char *format);
static int extract_one_argument(NspObject *Ob,int_types *T,va_list *ap,char Type,int pos, char *arg_message,char *list_end_message);

static int options_check(Stack stack,int rhs, int opt,nsp_option Opts[]);
static int get_from_options(nsp_option Opts[],va_list *ap,char *format);
static int GetListArgs_1(NspList *L,int pos,int_types *T,va_list *ap);

/**
 * GetArgs:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @T: 
 * @Varargs: 
 * 
 * Collects arguments checking their types. 
 * 
 * Return value: %OK or %FAIL
 **/

int  GetArgs(Stack stack,int rhs,int opt,int_types *T,...) 
{
  int rep;
  named_opts *Opts;
  NspList *L;
  int count = 0;
  void **Foo;
  NspTypeObject **Foo1,*type;
  va_list ap;
  va_start(ap,T);

  /* we first loop through the given arguments not considering 
   * named optional arguments (x=val)
   */
  for ( count = 1 ; count <= rhs -opt ; count++) 
    {
      switch ( *T  ) {
      case s_int : Foo = (void*) va_arg(ap, int *) ;
	if ( GetScalarInt(stack,count,(int *) Foo) == FAIL) { va_end(ap);return FAIL;}
	break;
      case s_double : Foo = (void *) va_arg(ap, double *) ;
	if (  GetScalarDouble(stack,count,(double *) Foo) == FAIL) { va_end(ap);return FAIL;}
	break;
      case s_bool : Foo = (void *) va_arg(ap, int *) ;
	if (  GetScalarBool(stack,count,(int *) Foo) == FAIL) { va_end(ap);return FAIL;}
	break;
      case string :   Foo = (void **) va_arg(ap, char **) ;
	if ( ( *((char **) Foo)= GetString(stack,count)) == NULL ) { va_end(ap);return FAIL;}
	break;
      case stringcopy :   Foo = (void **) va_arg(ap, char **) ;
	if ( ( *((char **) Foo)= GetString(stack,count)) == NULL ) { va_end(ap);return FAIL;}
	if ( ( *((char **) Foo)= NewString(( *((char **) Foo)))) == NULL ) { va_end(ap);return FAIL;}
	break;
      case mat :  Foo = (void **)  va_arg(ap, NspMatrix **) ;
	if ( ( *((NspMatrix **) Foo)=GetMat(stack,count) )== NULLMAT) { va_end(ap);return FAIL;}
	break;
      case matcopy : Foo = (void **)  va_arg(ap, NspMatrix **) ;
	if ( ( *((NspMatrix **) Foo)=GetMatCopy(stack,count) )== NULLMAT) { va_end(ap);return FAIL;}
	break;
      case bmat :  Foo = (void **)  va_arg(ap, NspBMatrix **) ;
	if ( ( *((NspBMatrix **) Foo)=GetBMat(stack,count) )== NULLBMAT) { va_end(ap);return FAIL;}
	break;
      case bmatcopy : Foo = (void **)  va_arg(ap, NspBMatrix **) ;
	if ( ( *((NspBMatrix **) Foo)=GetBMatCopy(stack,count) )== NULLBMAT) { va_end(ap);return FAIL;}
	break;
      case realmat :  Foo = (void **)  va_arg(ap, NspMatrix **) ;
	if ( ( *((NspMatrix **) Foo)=GetRealMat(stack,count) )== NULLMAT) { va_end(ap);return FAIL;}
	break;
      case realmatcopy : Foo = (void **)  va_arg(ap, NspMatrix **) ;
	if ( ( *((NspMatrix **) Foo)=GetRealMatCopy(stack,count) )== NULLMAT) { va_end(ap);return FAIL;}
	break;
      case mat_int :  Foo = (void **)  va_arg(ap, NspMatrix **) ;
	if ( ( *((NspMatrix **) Foo)=GetRealMatInt(stack,count) )== NULLMAT) { va_end(ap);return FAIL;}
	break;
      case matcopy_int :  Foo = (void **)  va_arg(ap, NspMatrix **) ;
	if ( ( *((NspMatrix **) Foo)=GetRealMatCopyInt(stack,count) )== NULLMAT) { va_end(ap);return FAIL;}
	break;
      case smat : Foo = (void **)  va_arg(ap, NspSMatrix **) ;
	if ( ( *((NspSMatrix**) Foo)=GetSMat(stack,count) )== NULLSMAT) { va_end(ap);return FAIL;}
	break;
      case smatcopy : Foo = (void **)  va_arg(ap, NspSMatrix **) ;
	if ( ( *((NspSMatrix**) Foo)=GetSMatCopy(stack,count) )== NULLSMAT) { va_end(ap);return FAIL;}
	break;
      case hash : Foo = (void **)  va_arg(ap,NspHash  **) ;
	if ( ( *((NspHash **) Foo)=GetHash(stack,count) )== NULLHASH) { va_end(ap);return FAIL;}
	break;
      case hashcopy : Foo = (void **)  va_arg(ap,NspHash  **) ;
	if ( ( *((NspHash **) Foo)=GetHashCopy(stack,count) )== NULLHASH) { va_end(ap);return FAIL;}
	break;
      case list : Foo = (void **)  va_arg(ap, NspList **) ;
	if ( ( *((NspList **) Foo)=GetList(stack,count) )== NULLLIST) { va_end(ap);return FAIL;}
	break;
      case obj :  Foo = (void **)  va_arg(ap, NspObject **) ;
	if (( *((NspObject **) Foo) =nsp_get_object(stack,count)) == NULLOBJ) { va_end(ap);return FAIL;}
	break;
      case objcopy :  Foo = (void **)  va_arg(ap, NspObject **) ;
	if ((*((NspObject **) Foo) =nsp_object_copy(NthObj(count))) == NULLOBJ) { va_end(ap);return FAIL;}
	break;
      case obj_check :  
	/* obj_check --> first a type then an object */
	Foo1 = (NspTypeObject **)  va_arg(ap, NspTypeObject **) ;
	type = *Foo1; 
	while ( type->surtype != NULL ) type= NSP_TYPE_OBJECT(type->surtype);
	Foo = (void **)  va_arg(ap, NspObject **) ; 
	if (( *Foo = type->get_from_obj(NthObj(count))) == NULL) 
	  {
	    Scierror("%s has a wrong type, expecting a %s\n",
		     ArgPosition(count),
		     type->s_type());
	    va_end(ap);
	    return FAIL;
	  }
	break;
      case list_begin : 
	if ( (L=GetList(stack,count) )== NULLLIST) return FAIL;
	T++;
	if ( GetListArgs_1(L,count,T,&ap) == FAIL)
	  {
	    Scierror("\t%s", ArgPosition(count));
	    ArgName(stack,count);
	    Scierror(" of function %s\n",stack.fname);
	    va_end(ap);
	    return FAIL;
	  }
	break;
      case list_end :
	/* list end will be decoded by GetListArgs */
	/* if we get here : it's certainly an error */
	Scierror("Error: found a list_end in %s while decoding arguments\n", stack.fname);
	Scierror("\twith no matching list_begin\n");
	va_end(ap);
	return FAIL;
	break;
      case t_end : 
	Scierror("Error: function %s, too many arguments (%d) given (%d requested)\n", stack.fname,rhs-opt,count-1);
	va_end(ap);
	return FAIL;
	break;
      case opts : 
      case new_opts:
	Scierror("\t%s", ArgPosition(count));
	ArgName(stack,count);
	Scierror(" of function %s should be given as name=val\n",stack.fname);
	va_end(ap);
	return FAIL;
	break;
      }
      T++;
    }
  
  count++;
  if ( *T == t_end ) 
    {
      va_end(ap);
      return OK ;
    }
  if ( *T != opts && *T != new_opts ) 
    {
      Scierror("Error: Not enough arguments (%d) given to function %s\n",rhs,stack.fname);
      va_end(ap);
      return FAIL;
    }
  /* now we are done with standard arguments 
   * take care of optional arguments. 
   */
  if ( opt == 0) {
    va_end(ap);
    return OK;
  }

  if ( *T == opts ) 
    {
      Opts = va_arg(ap, named_opts *);
      /* reorder optional arguments in Opts->objs */
      if ( OptCheck1(stack,rhs,opt,Opts)== FAIL) {
	va_end(ap);
	return FAIL;
      }
      /* optional argument extraction **/ 
      rep = GetFromTable_1(Opts->objs,Opts->types,&ap,"\twhile extracting optional argument number %d ");
      if ( rep == FAIL)
	Scierror(" of function %s\n",stack.fname);
      va_end(ap);
    }
  else
    {
      nsp_option *opts;
      opts = va_arg(ap, nsp_option *);
      /* reorder optional arguments in Opts->objs */
      if ( options_check(stack,rhs,opt,opts)== FAIL) {
	va_end(ap);
	return FAIL;
      }
      /* optional argument extraction **/ 
      rep = get_from_options(opts,&ap,"\twhile extracting optional argument %s");
      if ( rep == FAIL)
	Scierror(" of function %s\n",stack.fname);
      va_end(ap);
    }
    return rep;
}


/**
 * GetFromTable:
 * @Objs: Table objects 
 * @T: array of type description
 * @Varargs: Object to parse 
 * 
 * Decode the objects stored in Objs according to type stored in T : 
 * T must be terminated with the end tag (t_end) and 
 * the size of Objs must fit the number of arguments given in T 
 * Elements of Objs can be NULLOBJ and in that case 
 * the associated element is left unchanged 
 * 
 * Return value: 
 **/

int  GetFromTable(NspObject **Objs,int_types *T,...) 
{
  va_list ap;
  va_start(ap,T);
  return GetFromTable_1(Objs,T,&ap,"\twhile extracting argument (%d) from table\n");
}

static int  GetFromTable_1(NspObject **Objs,int_types *T,va_list *ap,char *format)
{
  int count = 0;
  while (1) 
    {
      if ( *T == t_end ) { va_end(*ap);	  return OK ;}
      if ( Objs[count] == NULLOBJ) 
	{
	  /* do nothing if Obj[count] is empty: associated argument will be left unchanged */
	  void **Foo ;
	  Foo=va_arg(*ap,void *);
	  if ( *T == obj_check )  Foo=va_arg(*ap,void *);
	}
      else 
	{
	  /* extract argument according to Type T */
 	  static char mes[]="Error: found a list_end in while decoding arguments\n";
	  if ( extract_one_argument(Objs[count],T,ap,'T',count, "argument",mes) == FAIL) 
	    {
	      Scierror(format,count+1);
	      return(FAIL) ;
	      break;
	    }
	}
      T++;
      count++;
    }
  return OK;
}

/* utility function */

static int  extract_one_argument(NspObject *Ob,int_types *T,va_list *ap,char Type,int pos, char *arg_message,char *list_end_message)
{
  NspList *L1;
  void **Foo;
  NspTypeObject **Foo1,*type;
  switch ( *T )     {
  case s_int : Foo = (void *) va_arg(*ap,int *) ;
    if ( IntScalar(Ob,(int *) Foo) == FAIL) return FAIL;
    break;
  case s_double : Foo = (void *) va_arg(*ap, double *) ;
    if ( DoubleScalar(Ob,(double *) Foo) == FAIL) return FAIL;
    break;
  case s_bool : Foo = (void *) va_arg(*ap, int *) ;
    if ( BoolScalar(Ob,(int *) Foo) == FAIL) return FAIL;
    break;
  case string :   Foo = (void **) va_arg(*ap, char **) ;
    if ( ( *((char **) Foo)=nsp_string_object(Ob)) == NULL ) return FAIL;
    break;
  case stringcopy :   Foo = (void **) va_arg(*ap, char **) ;
    if ( ( *((char **) Foo)=nsp_string_object(Ob)) == NULL ) return FAIL;
    if ( ( *((char **) Foo)= NewString(( *((char **) Foo)))) == NULL ) return FAIL;
    break;
  case mat :  Foo = (void **)  va_arg(*ap, NspMatrix **) ;
    if ( ( *((NspMatrix **) Foo)= Mat2double(matrix_object(Ob)))== NULLMAT) return FAIL;
    break;
  case matcopy : Foo = (void **)  va_arg(*ap, NspMatrix **) ;
    if ( ( *((NspMatrix **) Foo)= matrix_object(Ob))== NULLMAT) return FAIL;
    if ( ( *((NspMatrix **) Foo)= MaybeObjCopy((NspObject **)Foo)) == NULLMAT) return FAIL;
    if ( ( *((NspMatrix **) Foo)= Mat2double(*((NspMatrix **) Foo)))== NULLMAT) return FAIL;
    break;
  case bmat :  Foo = (void **)  va_arg(*ap, NspBMatrix **) ;
    if ( ( *((NspBMatrix **) Foo)= BMatObj(Ob))== NULLBMAT) return FAIL;
    break;
  case bmatcopy : Foo = (void **)  va_arg(*ap, NspBMatrix **) ;
    if ( ( *((NspBMatrix **) Foo)= MaybeObjCopy((NspObject **)Foo)) == NULLBMAT) return FAIL;
    break;
  case realmat :  Foo = (void **)  va_arg(*ap, NspMatrix **) ;
    if ( ( *((NspMatrix **) Foo)= Mat2double(matrix_object(Ob)))== NULLMAT) return FAIL;
    if ( ( *((NspMatrix **) Foo))->rc_type != 'r' ) return FAIL;
    break;
  case realmatcopy : Foo = (void **)  va_arg(*ap, NspMatrix **) ;
    if ( ( *((NspMatrix **) Foo)= matrix_object(Ob))== NULLMAT) return FAIL;
    if ( ( *((NspMatrix **) Foo))->rc_type != 'r' ) return FAIL;
    if ( ( *((NspMatrix **) Foo)= MaybeObjCopy((NspObject **)Foo)) == NULLMAT) return FAIL;
    if ( ( *((NspMatrix **) Foo)= Mat2double(*((NspMatrix **) Foo)))== NULLMAT) return FAIL;
    break;
  case smat : Foo = (void **)  va_arg(*ap, NspSMatrix **) ;
    if ( ( *((NspSMatrix**) Foo)=nsp_smatrix_object(Ob) )== NULLSMAT)  return FAIL;
    break;
  case smatcopy : Foo = (void **)  va_arg(*ap, NspSMatrix **) ;
    if ( ( *((NspSMatrix**) Foo)=nsp_smatrix_object(Ob) )== NULLSMAT) return FAIL;
    if ( ( *((NspSMatrix**) Foo)=MaybeObjCopy((NspObject **)Foo)) == NULLSMAT) return FAIL;
      return FAIL;
    break;
  case list_end :
    Scierror(list_end_message) ;
    return FAIL;
    break;
  case mat_int : Foo = (void **)  va_arg(*ap, NspMatrix **) ;
    if ( ( *((NspMatrix **) Foo)= Mat2int(matrix_object(Ob)))== NULLMAT) return FAIL;
    break;
  case matcopy_int : Foo = (void **)  va_arg(*ap, NspMatrix **) ;
    if ( ( *((NspMatrix **) Foo)= matrix_object(Ob))== NULLMAT) return FAIL;
    if ( ( *((NspMatrix **) Foo)= MaybeObjCopy((NspObject **)Foo)) == NULLMAT) return FAIL;
    if ( ( *((NspMatrix **) Foo)= Mat2int(*((NspMatrix **) Foo)))== NULLMAT) return FAIL;
    break;
  case hash : Foo = (void **)  va_arg(*ap,NspHash  **) ;
    if ( ( *((NspHash **) Foo)= hash_object(Ob))== NULLHASH) return FAIL;
    break;
  case hashcopy : Foo = (void **)  va_arg(*ap,NspHash  **) ;
    if ( ( *((NspHash **) Foo)= hash_object(Ob))== NULLHASH) return FAIL;
    if ( ( *((NspHash **) Foo)= MaybeObjCopy((NspObject **)Foo)) == NULLHASH) return FAIL;
    break;
  case list : Foo = (void **)  va_arg(*ap, NspList **) ;
    if ( ( *((NspList **) Foo)=nsp_list_object(Ob))== NULLLIST) return FAIL;
    break;
  case list_begin : 
    if ( (L1=nsp_list_object(Ob)) == NULLLIST) return FAIL;
    T++;
    if ( GetListArgs_1(L1,pos,T,ap) == FAIL) return FAIL;
    break;
  case obj : Foo = (void **)  va_arg(*ap, NspObject **) ;
    if ( check_cast(Ob,nsp_type_hobj_id) == TRUE)  Ob = ((NspHobj *) Ob)->O ;
    *((NspObject **) Foo) = Ob;
    break;
  case objcopy : Foo = (void **)  va_arg(*ap, NspObject **) ;
    if (( *((NspObject **) Foo) = MaybeObjCopy(&Ob)) == NULLOBJ) return FAIL;
    break;
  case obj_check:
    Foo1 = (NspTypeObject **)  va_arg(*ap, NspTypeObject **) ;
    type = *Foo1; 
    while ( type->surtype != NULL ) type= NSP_TYPE_OBJECT(type->surtype);
    Foo = (void **)  va_arg(*ap, NspObject **) ;
    /* Could be changed if some compilers complains with *x and x a void ** **/
    if (( *Foo = type->get_from_obj(Ob)) == NULL) 
      {
	Scierror("%s has a wrong type, expecting a %s\n",arg_message,type->s_type());
	return FAIL;
      }
    break;
  case opts : 
  case new_opts : 
    Scierror("Error: unexpected 'opts' key found while parsing arguments\n");
    return FAIL;
    break;
  case t_end : 
    if ( Type == 'L') 
      {
	va_end(*ap);
	Scierror("Error: %s an end key was found while expecting a list_end\n", arg_message);
	return FAIL ;
      }
    else 
      {
	va_end(*ap);
	return OK ;
      }
    break;
  }
  return OK;
}


/**
 * GetListArgs:
 * @L: a #NspList 
 * @pos: position in the calling stack 
 * @T: an array giving expected typed 
 * @Varargs: object to parse from the list 
 * 
 * Decodes a list using types given in @T 
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int  GetListArgs(NspList *L,int pos,int_types *T,...) 
{
  va_list ap;
  va_start(ap,T);
  return GetListArgs_1(L,pos,T,&ap);
}

static int  GetListArgs_1(NspList *L,int pos,int_types *T,va_list *ap)
{
  int count = 0;
  Cell *cell =  L->first;
  while ( 1 )
    {
      char mes[256];
      sprintf(mes,"%s is a list with more than %d elements\n", ArgPosition(pos),count);
      if ( cell == NULLCELL || cell->O == NULLOBJ ) 
	{
	  if (*T == list_end ) 
	    return OK;
	  else {
	    Scierror("Error %s is a too small list\n",ArgPosition(pos));
	    return FAIL;
	  }
	}
      /* extract argument */
      if ( extract_one_argument(cell->O,T,ap,'L',count,"list argument",mes) == FAIL) 
	{
	  Scierror("\twhile parsing a list argument (element %d)\n",count+1);
	  return(FAIL) ;
	  break;
	}
      T++; cell = cell->next;      count++;
    }
  return OK;
}


/**
 * BuildListFromArgs:
 * @T: 
 * @Varargs: 
 * 
 * Builds a #NspList from a set of arguments
 * Reste un probleme a regler : le nom des elements de la liste XXXXXXXXX
 * should be changed to remove the EndInsert using cells ...
 * 
 * Return value: a #NspList or %NULL
 **/

static NspList *BuildListFromArgs_1(int_types *T,va_list *ap);

NspList*BuildListFromArgs(int_types *T,...) 
{
  va_list ap;
  va_start(ap,T);
  return BuildListFromArgs_1(T,&ap);
}

static NspList *BuildListFromArgs_1(int_types *T,va_list *ap)
{
  int bval;
  NspList *L,*L1;
  NspObject *O;
  if (( L=nsp_list_create(NVOID,NULL)) == NULLLIST ) return NULLLIST;
  while ( 1 )
    {
      switch ( *T  ) 
	{
	case s_int : 
	  if (( O =nsp_create_object_from_int("lel",va_arg(*ap,int))) == NULLOBJ ) return NULLLIST;
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case s_double : 
	  if (( O =nsp_create_object_from_double("lel",va_arg(*ap,double))) == NULLOBJ ) return NULLLIST;
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case s_bool : 
	  bval = va_arg(*ap,int);
	  if ( bval == TRUE) 
	    { 
	      if (( O =nsp_create_true_object("lel")) == NULLOBJ ) return NULLLIST;
	    }
	  else 
	    { 
	      if (( O =nsp_create_false_object("lel")) == NULLOBJ ) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case string :  
	case stringcopy:
	  if ((O =nsp_create_object_from_str(va_arg(*ap, char *))) == NULLOBJ ) return NULLLIST;
	  if ( Ocheckname(O,NVOID) )
	    {
	      if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case mat_int :
	case realmat :
	case mat :  
	  O = (NspObject *) va_arg(*ap, NspMatrix *);
	  if ( Ocheckname(O,NVOID) )
	    {
	      if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,0) == FAIL ) return NULLLIST;
	  break;
	case matcopy_int:
	case matcopy :
	case realmatcopy :
	  if ((O = (NspObject *) nsp_matrix_copy(va_arg(*ap, NspMatrix *)))== NULLOBJ) return NULLLIST ;
	  if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case bmat :  
	  O = (NspObject *) va_arg(*ap, NspBMatrix *);
	  if ( Ocheckname(O,NVOID) )
	    {
	      if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case bmatcopy :  
	  if ((O = (NspObject *)nsp_bmatrix_copy(va_arg(*ap, NspBMatrix *)))== NULLOBJ) return NULLLIST ;
	  if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	  if (nsp_list_end_insert( L,(NspObject *) va_arg(*ap, NspBMatrix *)) == FAIL ) return NULLLIST;
	  break;
	case hash :
	  O = (NspObject *) va_arg(*ap, NspHash *);
	  if ( Ocheckname(O,NVOID) )
	    {
	      if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,O ) == FAIL ) return NULLLIST;
	  break;
	case hashcopy :
	  if ((O = (NspObject *) hash_copy(va_arg(*ap,NspHash *)))== NULLOBJ) return NULLLIST ;
	  if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case smat : 
	  O = (NspObject *) va_arg(*ap, NspSMatrix *);
	  if ( Ocheckname(O,NVOID) )
	    {
	      if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,O ) == FAIL ) return NULLLIST;
	  break;
	case smatcopy : 
	  if ((O = (NspObject *)nsp_smatrix_copy(va_arg(*ap, NspSMatrix *)))== NULLOBJ) return NULLLIST ;
	  if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case list :
	  O = (NspObject *) va_arg(*ap, NspList *);
	  if ( Ocheckname(O,NVOID) )
	    {
	      if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case list_begin : 
	  (*T)++;
	  if ((L1=BuildListFromArgs_1(T,ap))== NULLLIST) return NULLLIST;
	  if ( Ocheckname((NspObject *)L1,NVOID) )
	    {
	      if (nsp_object_set_name((NspObject *)L1,"lel") == FAIL) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,(NspObject *)L1) == FAIL ) return NULLLIST;
	  break;
	case obj : 
	  O= va_arg(*ap, NspObject *);
	  if ( Ocheckname(O,NVOID) )
	    {
	      if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	    }
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case objcopy: 
	  if ((O=nsp_object_copy(va_arg(*ap, NspObject *)))== NULLOBJ ) return NULLLIST;
	  if (nsp_object_set_name(O,"lel") == FAIL) return NULLLIST;
	  if (nsp_list_end_insert( L,O) == FAIL ) return NULLLIST;
	  break;
	case opts : 
	case new_opts : 
	  Scierror("do not use opts in BuildListFromArgs\n");
	  return NULLLIST;
	  break ; 
	case obj_check:
	  Scierror("do not use obj_check in BuildListFromArgs\n");
	  return NULLLIST;
	  break;
	case list_end: 
	case t_end : 
	  return L;
	  break;
	}
      T++; 
    }
}


/**
 * RetArgs:
 * @stack: calling stack 
 * @lhs: expected number of returned arguments 
 * @T: an array describing types 
 * @Varargs: list of arguments to return
 * 
 * Returns arguments on the Stack 
 *
 * Return value: %RET_BUG or an integer
 **/

static int RetArgs_1(Stack stack,int lhs,int_types *T,va_list *ap);

int RetArgs(Stack stack,int lhs,int_types *T,...) 
{
  va_list ap;
  va_start(ap,T);
  return RetArgs_1(stack,lhs,T,&ap);
}

static int RetArgs_1(Stack stack,int lhs,int_types *T,va_list *ap)
{
  int count = 1;
  NspObject *O;
  NspList *L1;
  lhs = Max(lhs,0);
  while ( count <= lhs  )
    {
      switch ( *T  ) 
	{
	case s_int :
	  if ( nsp_move_double(stack,count++,(double) va_arg(*ap,int) )== FAIL) return RET_BUG;
	  break;
	case s_double : 
	  if ( nsp_move_double(stack,count++,(double) va_arg(*ap,double) )== FAIL) return RET_BUG;
	  break;
	case s_bool : 
	  if ( nsp_move_boolean(stack,count++,(double) va_arg(*ap,int) )== FAIL) return RET_BUG;
	  break;
	case string :  
	case stringcopy:
	  if ( nsp_move_string(stack,count++,va_arg(*ap, char *),-1)== FAIL) return RET_BUG;
	  break;
	case mat_int :
	case realmat :
	case mat :
	  if (( O = (NspObject *) va_arg(*ap, NspMatrix *)) == NULLOBJ) return RET_BUG; 
	  MoveObj(stack,count++,O);
	  break;
	case matcopy_int:
	case matcopy :
	case realmatcopy :
	  if (( O = (NspObject *) va_arg(*ap, NspMatrix *)) == NULLOBJ) return RET_BUG; 
	  if ((O = (NspObject *) nsp_matrix_copy((NspMatrix *) O))== NULLOBJ) return RET_BUG ;
	  MoveObj(stack,count++,O);
	  break;
	case bmat :  
	  if (( O = (NspObject *) va_arg(*ap, NspBMatrix *)) == NULLOBJ) return RET_BUG; 
	  MoveObj(stack,count++, O);
	  break;
	case bmatcopy :  
	  if (( O = (NspObject *) va_arg(*ap, NspBMatrix *)) == NULLOBJ) return RET_BUG; 
	  if ((O = (NspObject *)nsp_bmatrix_copy((NspBMatrix *) O))== NULLOBJ) return RET_BUG ;
	  MoveObj(stack,count++,O);
	  break;
	case hash :
	  if (( O = (NspObject *) va_arg(*ap, NspHash *)) == NULLOBJ) return RET_BUG; 
	  MoveObj(stack,count++,O);
	  break;
	case hashcopy :
	  if (( O = (NspObject *) va_arg(*ap, NspHash *)) == NULLOBJ) return RET_BUG; 
	  if ((O = (NspObject *) hash_copy((NspHash *) O))== NULLOBJ) return RET_BUG ;
	  MoveObj(stack,count++,O);
	  break;
	case smat : 
	  if (( O = (NspObject *) va_arg(*ap, NspSMatrix *)) == NULLOBJ) return RET_BUG; 
	  MoveObj(stack,count++,O);
	  break;
	case smatcopy : 
	  if (( O = (NspObject *) va_arg(*ap, NspSMatrix *)) == NULLOBJ) return RET_BUG; 
	  if ((O = (NspObject *)nsp_smatrix_copy((NspSMatrix *) O))== NULLOBJ) return RET_BUG ;
	  MoveObj(stack,count++,O);
	  break;
	case list :
	  if (( O = (NspObject *) va_arg(*ap, NspList *)) == NULLOBJ) return RET_BUG; 
	  MoveObj(stack,count++,O);
	  break;
	case list_begin : 
	  T++;
	  if ((L1=BuildListFromArgs_1(T,ap))== NULLLIST) return RET_BUG;
	  while ( *T != list_end )  T++; /* walk till end of list */
	  MoveObj(stack,count++,(NspObject *) L1);
	  break;
	case obj : 
	  if ((O= va_arg(*ap, NspObject *))== NULLOBJ) return RET_BUG;
	  MoveObj(stack,count++,(NspObject *) O);
	  break;
	case objcopy: 
	  if ((O= va_arg(*ap, NspObject *))== NULLOBJ) return RET_BUG;
	  if ((O=nsp_object_copy(O))== NULLOBJ ) return RET_BUG;
	  MoveObj(stack,count++,(NspObject *) O);
	  break;
	case opts : 
	case new_opts : 
	  Scierror("do not use opts in RetArgs\n");
	  return RET_BUG;
	  break ; 
	case obj_check:
	  Scierror("do not use obj_check in RetArgs\n");
	  return RET_BUG;
	  break;
	case list_end: 
	  T++; 
	  break;
	case t_end : 
	  return count-1;
	  break;
	}
      T++; 
    }
  if ( count >= lhs ) return count-1;
  return RET_BUG;
}



/**
 * SwapObjs:
 * @stack: 
 * @i: 
 * @j: 
 * 
 * swap two object on the stack 
 * first + i -1  and first + j -1
 * XXXX: obsolete 
 * 
 **/

void SwapObjs(Stack stack, int i, int j)
{
  NspObject *O = NthObj(i);
  NthObj(i)= NthObj(j);
  NthObj(j) = O;
}

/**
 * MoveObj:
 * @stack: calling stack 
 * @j: position on the stack 
 * @O: Object which is to be placed at position @j
 * 
 * Move an object to an other position, cleaning the position first
 * the moved position is assumed to be a return position 
 * 
 **/

void MoveObj(Stack stack, int j, NspObject *O)
{
  /* XXXXX attention si l'objet a detruire est une variable optionnelle 
   *  c'est plus compliqué 
   *  voir le code standard 
   */
  nsp_void_object_destroy(&NthObj(j));
  NthObj(j)= O;    
  NSP_OBJECT(O)->ret_pos =j;
}

/**
 * ObjPerm:
 * @stack: 
 * @nv: 
 * @ind: 
 * 
 * 
 * Permutation of Stack Objects 
 * Note that permutation is given by a function [1,nv]-->[1,nv] 
 * (i.e first indice is zero)  
 * ind is left unchanged at the end of execution 
 * ind(i)=j means that NthObj(i) must be moved to NthObj(j) position 
 * XXXX ret_pos must be set 
 * obsolete ? 
 **/

void ObjPerm(Stack stack, int nv, int *ind)
{
  NspObject *x;
  int i,i0,i1;
  i0=0; i=i0; x= NthObj(1);
  while (1) 
    {
      if ( ind[i]-1 == i0) 
	{ 
	  ind[i]=-ind[i]-1;
	  NthObj(i+1)=x;
	  i1=-1;
	  while (1) 
	    {
	      i1=i1+1;
	      if (i1 >= nv) goto end;
	      if (ind[i1] < 0) continue;
	      i0=i1; i=i0; x= NthObj(i0+1);
	    }
	}
      else
	{
	  NthObj(i+1)= NthObj(ind[i]);
	  i1=ind[i]-1;
	  ind[i]=-ind[i]-1;
	  i=i1;
	}
    }
 end:
  for ( i = 0 ; i < nv ; i++) 
    ind[i]=-ind[i]-1;
}


/**
 * PutLhsObj:
 * @stack: 
 * @nv: 
 * @ind: 
 * 
 * 
 * PutLhsObj : the nv objets at position ind(i) 
 *    are to be moved at the first 1,...,nv position 
 *    Objects which where at position 1,...,nv are moved 
 *    Warning : ind is changed after the call 
 *    =======
 *    Warning : all the ind(i) must be different
 *    =======
 **/

void PutLhsObj(Stack stack, int nv, int *ind)
{
  NspObject *O;
  int i,j;
  /* Moving Objects **/
  for ( i= 0 ; i < nv ; i++) 
    {
      O = NthObj(i+1);
      if ( ind[i] != i+1 ) 
	{
	  NthObj(i+1) = NthObj(ind[i]);
	  NSP_OBJECT(NthObj(i+1))->ret_pos= i+1;
	  NthObj(ind[i])= O;
	  /* the next loop checks if O is among output variable **/
	  for ( j = 0 ; j < nv ; j++) 
	    if ( ind[j]== i+1) { ind[j] = ind[i];}
	}
    }
}


/*
 * Utility function to deal with Optional parameters 
 * FIXME: Obsolete ? 
 */

void OptCheck(NspObject **Os, NspObject **DefO, char **Names, int n, Stack stack, int nopt)
{
  int i,j;
  for ( i = 0 ; i < n ; i++)
    {
      int ok=0;
      for ( j = 0 ; j < nopt ; j++) 
	{
	  if ( Ocheckname(stack.S[stack.first+j],Names[i]))
	    {
	      Os[i] = stack.S[stack.first+j];
	      ok = 1;break;
	    }
	}
      if ( ok == 0 ) Os[i] = DefO[i];
    }
}

/* utility function used in GetArgs */

static int OptCheck1(Stack stack,int rhs, int nopt, named_opts *Opts)
{
  int j,i;
  for ( i = 0 ; i < Opts->n ; i++) 
    { Opts->objs[i] = NULLOBJ; Opts->posi[i]=-1; };
  for ( j = 0 ; j < nopt ; j++) 
    {
      int rep=is_string_in_array(nsp_object_get_name(stack.S[stack.first+rhs-nopt+j]),
				 Opts->names,1);
      if ( rep >= 0) 
	{
	  Opts->objs[rep] = stack.S[stack.first+rhs-nopt+j];
	  Opts->posi[rep] = rhs-nopt+j+1;
	}
      else 
	{
	  char **entry;
	  Scierror("Error:\t%s", ArgPosition(rhs-nopt+j+1));
	  Scierror(" of function %s has a wrong option name ",
		   stack.fname);
	  ArgName(stack,rhs-nopt+j+1);
	  Scierror(".\n\tIt should be '%s'", *Opts->names);
	  for (entry = Opts->names+1 ; *entry != NULL; entry++) {
	    if (entry[1] == NULL) {
	      Scierror(", or '%s'",*entry);
	    } else {
	      Scierror(", '%s'",*entry);
	    }
	  }
	  Scierror("\n");
	  return FAIL;
	}
    }
  return OK ;
}

/**
 * get_optional_args:
 * @stack: calling stack 
 * @rhs: number of given arguments 
 * @opt: number of optional arguments in the @rhs
 * @opts: array describing the optional arguments
 * @Varargs: the object to be parsed according to @opts
 * 
 * Utility function to deal with Optional parameters 
 * 
 * Return value: 
 **/

int  get_optional_args(Stack stack,int rhs,int opt,nsp_option opts[],...)
{
  int rep;
  va_list ap;
  va_start(ap,opts);

  /* reorder optional arguments in Opts->objs */
  if ( options_check(stack,rhs,opt,opts)== FAIL) {
    va_end(ap);
    return FAIL;
  }
  /* optional argument extraction **/ 
  rep = get_from_options(opts,&ap,"\twhile extracting optional argument %s");
  if ( rep == FAIL)
    Scierror(" of function %s\n",stack.fname);
  va_end(ap);
  return rep;
}

static int is_string_in_options(char *key, nsp_option Opts[], int flag);

static int options_check(Stack stack,int rhs, int opt,nsp_option Opts[])
{
  int j;
  nsp_option *option = Opts;
  while ( option->name != NULL) { option->obj = NULLOBJ; option->position=-1; option++ ;};
  for ( j = 0 ; j < opt ; j++) 
    {
      int rep=is_string_in_options(nsp_object_get_name(stack.S[stack.first+rhs-opt+j]),Opts,1);
      if ( rep >= 0) 
	{
	  Opts[rep].obj = stack.S[stack.first+rhs-opt+j];
	  Opts[rep].position = rhs-opt+j+1;
	}
      else 
	{
	  Scierror("Error:\t%s", ArgPosition(rhs-opt+j+1));
	  Scierror(" of function %s has a wrong option name ",stack.fname);
	  ArgName(stack,rhs-opt+j+1);
	  Scierror(".\n\tIt should be '%s'", Opts[0].name);
	  for (option = Opts+1 ; option->name != NULL; option++) {
	    if ( option[1].name == NULL) {
	      Scierror(", or '%s'",option->name);
	    } else {
	      Scierror(", '%s'",option->name);
	    }
	  }
	  Scierror("\n");
	  return FAIL;
	}
    }
  return OK ;
}

/* Table : Array of strings to compare against str
 * last entry must be NULL
 * and there must not be duplicate entries. */
/* 0 or 1, 1 for exact match */

static int is_string_in_options(char *key, nsp_option Opts[], int flag)
{
  int index = -1, numAbbrev=0, i;
  char *p1, *p2;
  nsp_option *entry;
  /*
   * Lookup the value of the object in the table.  Accept unique
   * abbreviations unless flag == 1;
   */
  for (entry = Opts , i = 0; entry->name != NULL; entry++, i++) {
    for (p1 = key, p2 = entry->name ; *p1 == *p2; p1++, p2++) {
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
    if ( numAbbrev > 1)  return -1; else return -2;
  }
  return index;
}

/* to be used after options_check */

static int get_from_options(nsp_option Opts[],va_list *ap,char *format)
{
  int count = 0;
  while (1) 
    {
      if ( Opts[count].name == NULL) { va_end(*ap);  return OK ;}
      if ( Opts[count].obj == NULL) 
	{
	  /* do nothing if Obj[count] is empty: associated argument will be left unchanged */
	  void **Foo ;
	  Foo=va_arg(*ap,void *);
	  if ( Opts[count].type == obj_check )  Foo=va_arg(*ap,void *);
	}
      else 
	{
	  /* extract argument according to Type T */
 	  static char mes[]="Error: found a list_end in while decoding arguments\n";
	  if ( extract_one_argument(Opts[count].obj,&Opts[count].type,ap,'T',count, "argument",mes) == FAIL) 
	    {
	      Scierror(format,Opts[count].name); /* count+1); */
	      return(FAIL) ;
	      break;
	    }
	}
      count++;
    }
  return OK;
}


/**
 * ObjConvert:
 * @O: 
 * 
 * Utility function : Check if object need 
 * a type conversion --> double 
 * 
 **/

void ObjConvert(NspObject *O)
{
  if (nsp_object_type(O , nsp_type_matrix_id) ) Mat2double(( NspMatrix *) O);
}

/**
 * MaybeObjCopy:
 * @O: 
 * 
 * 
 * Makes a copy of O is O has name != void 
 * this copy is returned in O
 * the element stored in O is returned as a (void *)
 * if O is an Hobj object : the object it points to 
 * is copied here 
 *
 **/

void *MaybeObjCopy(NspObject **O)
{
  if ( Ocheckname(*O,NVOID) ) return *O;
  if ( check_cast(*O,nsp_type_hobj_id) == TRUE)  *O= ((NspHobj *) *O)->O ;
  *O =nsp_object_copy(*O);
  return *O;
}

static char first[]="First argument";
static char second[]="Second argument";
static char third[]="Third argument";

/**
 * ArgPosition:
 * @i: position of a parameter 
 * 
 * Returns an argument position as a string 
 * (utility for error message)
 * 
 * 
 * Return value: a char * describing a position
 **/

static char Name[32];

char *ArgPosition(int i)
{
  switch (i) 
    {
    case 1: return first;
    case 2 : return second;
    case 3: return third;
    default : 
      sprintf(Name,"%dth argument ",i);
      return Name;
    }
}


/**
 * ArgName:
 * @stack: calling stack 
 * @i: position on the stack 
 * 
 * Gets an argument name 
 * (utility for error message)
 * 
 **/

void ArgName(Stack stack, int i)
{
  char *arg =nsp_object_get_name((NthObj(i)));
  if ( strcmp(arg ,NVOID) != 0 ) Scierror(" (%s)",arg);
}


/**
 * ArgMessage:
 * @stack: calling stack 
 * @i: position on the stack 
 * 
 * Builds an error message for argument i
 * (utility for error message)
 * 
 **/

void ArgMessage(Stack stack, int i)
{
  Scierror("\t%s", ArgPosition(i));
  ArgName(stack,i);
  Scierror(" of function %s\n",stack.fname);
}

/**
 * attr_search:
 * @key: key to be searched 
 * @Table: table 
 * 
 * Return value: 
 **/

int attr_search(char *key, AttrTab *Table)
{
  int i;
  char *p1, *p2;
  AttrTab *entry;
  for (entry = Table, i = 0; entry->name != NULL; entry++, i++) {
    for (p1 = key, p2 = entry->name; *p1 == *p2; p1++, p2++) {
      if (*p1 == 0) return i;
    }
  }
  return -1;
}

/**
 * attrs_to_stack:
 * @key: key to be searched 
 * @attrs: 
 * @stack: 
 * @pos: 
 * 
 * 
 * Return value: 
 **/

int attrs_to_stack(char *key, AttrTab *attrs, Stack stack, int pos)
{
  NspObject *O;
  if ( (O = (NspObject *)nsp_smatrix_create_from_struct(NVOID,attrs,sizeof(AttrTab)) ) == NULLOBJ) return RET_BUG;
  MoveObj(stack,pos,O);
  return 1;
}


/**
 * method_search:
 * @key: 
 * @Table: 
 * 
 * Utilities for object method search 
 * 
 * Return value: 
 **/

int method_search(char *key, NspMethods *Table)
{
  int i;
  char *p1, *p2;
  NspMethods *entry;
  for (entry = Table, i = 0; entry->name != NULL; entry++, i++) {
    for (p1 = key, p2 = entry->name; *p1 == *p2; p1++, p2++) {
      if (*p1 == 0) return i;
    }
  }
  return -1;
}

/*----------------------------------------------------------------------
 * Interface Scilab : 
 * utility function for interfaces 
 *----------------------------------------------------------------------*/

/*
 *----------------------------------------------------------------------
 */

/**
 * nsp_move_string:
 * @stack: 
 * @n: 
 * @bytes:  Points to the first of the length bytes used to initialize the object.
 * @length: The number of bytes to copy from "bytes" when initializing the object. If  
 *          negative, use bytes up to the first  NULL byte.
 * 
 * 
 * Replace object at position n on the stack by a new string matrix object 
 * filled with bytes 
 *	The object's string representation will be set to a copy of
 *	the "length" bytes starting at "bytes". If "length" is negative, use
 *	bytes up to the first NULL byte; i.e., assume "bytes" points to a
 *	C-style NULL-terminated string. The object's old string and internal
 *	representations are freed and the object's type is set NULL.
 * 
 * Return value: 1 or %RET_BUG 
 **/

int nsp_move_string(Stack stack,int n,const char *bytes,int length)
{
  NspObject *O;
  if (( O = nsp_new_string_obj(NVOID,bytes, length)) == NULLOBJ ) return FAIL;
  MoveObj(stack,n,O);
  return OK ;
}

/**
 * nsp_new_string_obj:
 * @name: 
 * @bytes: 
 * @length: 
 * 
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_new_string_obj(char *name,const char *bytes,int length)
{
  NspSMatrix *S;
  if ( length < 0  ) 
    {
      if (( S =nsp_smatrix_create(name,1,1,bytes,1) ) == NULLSMAT ) return NULLOBJ;
    }
  else
    {
      if (( S =nsp_smatrix_create_with_length(name,1,1,length) ) == NULLSMAT ) return NULLOBJ;
      strncpy(S->S[0],bytes,length);
      S->S[length] ='\0';
    }
  return (NspObject *) S;
}


/**
 * nsp_move_double:
 * @stack: 
 * @n: 
 * @d: 
 * 
 *      Replace object at position n on the stack by a new matrix object 
 *      filled with a double. 
 * 
 * Return value: 1 or %RET_BUG 
 **/

int nsp_move_double(Stack stack,int n,double d)
{
  NspMatrix *M;
  if (( M = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) return FAIL;
  M->R[0] = d;
  MoveObj(stack,n,(NspObject *) M);
  return OK;
}

/**
 * nsp_new_double_obj:
 * @d: 
 * 
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_new_double_obj(double d)
{
  NspMatrix *M;
  if (( M = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT ) return NULLOBJ;
  M->R[0] = d;
  return (NspObject *) M;
}


/**
 * nsp_move_doubles:
 * @stack: calling stack 
 * @pos: position of returned argument 
 * @m: number of row
 * @n: number of columns 
 * @Varargs: optional @mx@n double 
 * 
 *  Replace object at position n on the stack by a new matrix object 
 *  filled with a set of doubles. 
 * 
 * Return value: 1 or %RET_BUG 
 **/

int nsp_move_doubles(Stack stack,int pos,int m,int n,...)
{
  int i;
  NspMatrix *Loc;
  if (( Loc = nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT) return FAIL;
  va_list ap;
  va_start(ap,n);
  for ( i= 0 ; i < m*n ; i++) 
    Loc->R[i] = (double) va_arg(ap,double);
  va_end(ap);
  MoveObj(stack,pos,(NspObject *) Loc);
  return OK;
}

/**
 * nsp_move_boolean:
 * @stack: 
 * @n: 
 * @ival: 
 * 
 *      Replace object at position n on the stack by a new boolean matrix object 
 *      filled with a boolean value
 * Return value: 1 or %RET_BUG 
 **/

int nsp_move_boolean(Stack stack,int n,int ival)
{

  NspObject *O;
  if ((O = (ival != FALSE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID) )==NULLOBJ) return FAIL;
  MoveObj(stack,n,O);
  return OK;
}

/**
 * nsp_new_boolean_obj:
 * @ival: 
 * 
 * 
 * 
 * Return value: 
 **/

NspObject *nsp_new_boolean_obj(int ival)
{
  return (ival != 0) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID); 
}







