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

#define List_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/plisttoken.h" /* for name_maxl **/
#include "../interp/Eval.h"

/*
 * NspList inherits from NspObject 
 */

int nsp_type_list_id=0;
NspTypeList *nsp_type_list=NULL;

NspTypeList *new_type_list(type_mode mode)
{
  NspTypeList *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_list != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_list;
    }
  if ((type =  malloc(sizeof(NspTypeList))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = NULL; /* list_attrs ; */
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = nsp_list_get_methods; 
  type->new = (new_func *) new_list;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for list */ 

  top->pr = (print_func *)nsp_list_print;                    /* printing*/   
  top->dealloc = (dealloc_func *)nsp_list_destroy;              /* dealloc */  
  top->copy  =  (copy_func *)nsp_list_copy;                   /* copy object */  
  top->size  = (size_func *)nsp_list_size;                   /* m,n or m*n  */  
  top->s_type =  (s_type_func *)nsp_list_type_as_string;                /* type as a String */  
  top->sh_type = (sh_type_func *)nsp_list_type_short_string;              /* type as a short string */  
  top->info = (info_func *)nsp_list_info;                    /* info */  
  top->is_true = (is_true_func  *)nsp_list_is_true;             /* check if object can be considered as true */  
  top->loop =(loop_func *) list_loop_extract ;               /* for loops */
  top->path_extract = (path_func *)nsp_list_path_extract;        /* used for x(1)(2)(...) */  
  top->get_from_obj = (get_from_obj_func *)nsp_list_object;    /* get object stored in SciObj */  
  top->eq  = (eq_func *)nsp_list_eq;                       /* equality check */  
  top->neq  = (eq_func *)nsp_list_neq;                      /* non-equality check */

  top->save  = (save_func *)nsp_list_xdr_save;
  top->load  = (load_func *)nsp_list_xdr_load;

  /* specific methods for list */
  type->init = (init_func *) init_list;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_list_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_list
       */
      type->id =  nsp_type_list_id = nsp_new_type_id();
      nsp_type_list = type;
      if ( nsp_register_type(nsp_type_list) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_list(mode);
    }
  else 
    {
      type->id = nsp_type_list_id;
      return type;
    }
}
/*
 * initialize List instances 
 * locally and by calling initializer on parent class 
 */

static int init_list(NspList *o,NspTypeList *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  o->first = NULL;
  o->tname = NULL;
  return OK;
}

/*
 * new instance of List 
 */

NspList *new_list() 
{
  NspList *loc; 
  /* type must exists */
  nsp_type_list = new_type_list(T_BASE);
  if ( (loc = malloc(sizeof(NspList)))== NULLLIST) return loc;
  /* initialize object */
  if ( init_list(loc,nsp_type_list) == FAIL) return NULLLIST;
  return loc;
}


/*----------------------------------------------
 * Object method redefined 
 *-----------------------------------------------*/

/*
 *nsp_list_size: returns filled,hsize,or hsize 
 */

int nsp_list_size(NspList *L, int flag)
{
  return nsp_list_length(L);
}

/*
 * ListType 
 */

static char list_type_name[]="List";
static char list_short_type_name[]="l";

char *nsp_list_type_as_string(void)
{
  return(list_type_name);
}

char *nsp_list_type_short_string(NspList *L)
{
  return(list_short_type_name);
}

/* used in for x=list(....) ... **/

NspObject *list_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep)
{
  static Cell *cell; 
  NspObject *ret; 
  if ( O == NULLOBJ ) 
    {
      /* new iteration on O1 */
      cell =nsp_list_object(O1)->first; 
    }
  else 
    {
      /* XXXX destroy O */
    }
  /* ignore empty cells */
  while (cell != NULLCELL &&  cell->O == NULLOBJ ) cell = cell->next ; 
  if ( cell == NULLCELL ) 
    {
      *rep = RET_ENDFOR; 
      return NULLOBJ;
    }
  if ((ret =nsp_object_copy_and_name(str,cell->O))== NULL) return NULL;
  cell = cell->next ;
  return ret; 
}

/* used for L(1)(2)....(expn) evaluation */

NspObject *nsp_list_path_extract(NspList *L, NspObject *O)
{
  NspSMatrix *M;
  int ival;
  if ( IsMat(O)  ) 
    {
      if ( IntScalar(O,&ival) == FAIL ) return NULLOBJ ;
      return nsp_list_get_element(L,ival);
    }
  else if ( IsSMat(O) ) 
    {
      if (( M =nsp_smatrix_object(O)) == NULLSMAT || M->mn != 1) return NULLOBJ ;
      return nsp_list_search(L,M->S[0]);
    }
  return  NULLOBJ;
}


int nsp_list_eq(NspObject *A, NspObject *B)
{
  int rep;
  if ( check_cast(B,nsp_type_list_id) == FALSE) return FALSE ;
  rep =nsp_list_full_equal((NspList *) A,(NspList *) B);
  return rep;
}

int nsp_list_neq(NspObject *A, NspObject *B)
{
  int rep;
  if ( check_cast(B,nsp_type_list_id) == FALSE) return TRUE;
  rep =nsp_list_full_not_equal((NspList *) A,(NspList *) B);
  return rep;
}

/*
 * List == TRUE ? 
 * if List != emptylist and all the elements of List are true 
 */

static int nsp_list_is_true(NspList *l)
{
  Scierror("List is true must be implemented\n");
  return FALSE;
}


/*
 * Save a NspList 
 */

int nsp_list_xdr_save(NspFile  *F, NspList *L)
{
  Cell *cell = L->first;
  int length;
  if (nsp_xdr_save_i(F->xdrs,L->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(F->xdrs, NSP_OBJECT(L)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(F->xdrs,L->tname == NULLSTRING ) == FAIL) return FAIL;
  if ( L->tname != NULLSTRING) 
    {  if (nsp_xdr_save_string(F->xdrs, NSP_OBJECT(L)->name) == FAIL) return FAIL;}
  length =nsp_list_length(L);
  if (nsp_xdr_save_i(F->xdrs,length) == FAIL) return FAIL;
  while ( cell != NULLCELL ) 
    {
      if (nsp_object_xdr_save(F, cell->O) == FAIL) return FAIL;
      cell = cell->next;
    }
  return OK;
}

/*
 * Load a NspList 
 */

NspList*nsp_list_xdr_load(NspFile  *F)
{
  Cell *C;
  NspList *L;
  int flag,length,count;
  static char name[NAME_MAXL];
  static char tname[NAME_MAXL];
  if (nsp_xdr_load_string(F->xdrs,name,NAME_MAXL) == FAIL) return NULLLIST;
  if (nsp_xdr_load_i(F->xdrs,&flag) == FAIL) return NULLLIST;
  if ( flag ) 
    {
      if (( L =nsp_list_create(name,NULLSTRING)) == NULLLIST ) return NULLLIST;
    }
  else 
    {
      if (nsp_xdr_load_string(F->xdrs,tname,NAME_MAXL) == FAIL) return NULLLIST;
      if (( L =nsp_list_create(name,tname)) == NULLLIST ) return NULLLIST;
    }
  if (nsp_xdr_load_i(F->xdrs,&length) == FAIL) return NULLLIST;
  C = L->first;
  count = 0;
  while ( count < length ) 
    {
      Cell *Loc;
      NspObject *O =nsp_object_xdr_load(F); 
      if ( O == NULLOBJ ) return NULLLIST;
      if (( Loc =nsp_cell_create( NULLSTRING,O))== NULLCELL) return NULLLIST;
      if ( C == NULLCELL) 
	{
	  L->first =C =  Loc; 
	}
      else 
	{
	  Loc->prev = C;
	  C->next = Loc; 
	  C = Loc;
	}
      count++;
    }
  return L;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces
 * for BMatrix objects
 * Note that some of these functions could become MACROS XXXXX
 *-----------------------------------------------------*/


/*
 * A =nsp_list_object(O);
 * checks that O is an object of NspList type. 
 * or a Hobj which points to an object of type List
 * if so, returns a pointer to that NspList and else returns NULL
 */

NspList   *nsp_list_object(NspObject *O)
{
  /* Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type **/
  if ( check_cast(O,nsp_type_list_id) == TRUE) return ((NspList *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_list));
  return(NULL);
}


/*
 * IsListObj(stack,i)
 * only checks that object at position 
 * first + i -1  is an object of type  List 
 * or a Hobj which points to an object of type List
 */

int IsListObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_list_id);
}

/*
 * IsList(O)
 * only checks that object is an object of type  List 
 * or a Hobj which points to an object of type List
 */

int IsList(NspObject *O)
{
  return nsp_object_type(O , nsp_type_list_id);
}

/*
 * Checks that first+i object on the stack 
 * is a LIST and returns that LIST  
 * or a copy of that LIST if its name 
 * is != NVOID 
 */

NspList*GetListCopy(Stack stack, int i)
{
  if (  GetList(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

/*
 * Checks that i-th object on the stack 
 * is a NspList and returns that NspList or NULLLIST 
 */

NspList*GetList(Stack stack, int i)
{
  NspList *M;
  if (( M =nsp_list_object(NthObj(i))) == NULLLIST)
    ArgMessage(stack,i);
  return M;
}

/*-------------------------------------------------------------------
 * wrappers for the BMatrix
 * i.e functions at Nsp level
 *-------------------------------------------------------------------*/
 
/*------------------------------------------------------
 * attributes  (set/get methods)
 *------------------------------------------------------*/
 
/*------------------------------------------------------
 * methods
 *------------------------------------------------------*/
 
/*
 * Extract requested elements of the list 
 * an create a New list with copy of them
 */

static int int_list_sublist(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspList *L=self , *Le;
  NspMatrix *Elts;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((Elts = GetMat(stack,1)) == NULLMAT) return RET_BUG;
  if ((Le =nsp_list_extract(L,Elts)) == NULLLIST ) return RET_BUG;
  NthObj(3) = NSP_OBJECT(Le) ;
  NthObj(3)->ret_pos = 1;
  return 1;
}


static NspMethods nsp_list_methods[] = {
  { "sublist", int_list_sublist },
  { (char *) 0, NULL}
};

static NspMethods *nsp_list_get_methods(void) { return nsp_list_methods;};


/*----------------------------------------------------
 * Interface
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/
 
/*
 * create a NspList inserting the rhs object 
 * on the stack in the list 
 * if rhs == 0 an empty list is created 
 * XXXXX : attention au HOPT ici 
 */

static int int_lxlist(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspList *L;
  CheckLhs(1,1);
  if ((L =nsp_list_create(NVOID,NULLSTRING))==NULLLIST) return RET_BUG;
  for ( i = 1; i <= rhs ; i++ )
    {
      if ( MaybeObjCopy(&NthObj(i)) == NULL)  return RET_BUG;
      if (nsp_object_set_name(NthObj(i),"lel") == FAIL) return RET_BUG;
      if (nsp_list_end_insert(L,NthObj(i)) == FAIL ) return RET_BUG;
      /* If NthObj(i) is not copied it is inserted in the list 
      we must set then NthObj(i) to NULLOBJ 
      to prevent the cleaning process to clean the object 
      that we have inserted in our list **/
      NthObj(i) = NULLOBJ ;
    }
  NthObj(1)=(NspObject *) L;
  NSP_OBJECT(NthObj(1))->ret_pos = 1;
  return 1;
}

/*
 * create a NspList inserting the rhs object 
 * which must have names in lexicographic order 
 * if rhs == 0 an empty list is created 
 */

static int int_lxsortedlist(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O1;
  int i;
  NspList *L;
  CheckLhs(1,1);
  if ((L =nsp_list_create(NVOID,NULLSTRING))==NULLLIST) return RET_BUG;
  for ( i = 1; i <= rhs ; i++ )
    {
      if ( Ocheckname(NthObj(i),NVOID)) 
	{
	  Scierror("Error: arguments must have name for %s function\n",stack.fname);
	  /* Netoyer **/
	  return RET_BUG;
	}
      if ( (O1 =nsp_object_copy_with_name(NthObj(i))) == NULLOBJ)  return RET_BUG;
      if (nsp_sorted_list_insert(L,O1) == FAIL ) return RET_BUG;
      /* If NthObj(i) is not copied it is inserted in the list 
      we must set then NthObj(i) to NULLOBJ 
      to prevent the cleaning process to clean the object 
      that we have inserted in our list **/
      NthObj(i) = NULLOBJ ;
    }
  NthObj(1)=(NspObject *) L;
  NSP_OBJECT(NthObj(1))->ret_pos = 1;
  return 1;
}


/*
 * search a sorted NspList 					  
 */

static int int_lxsortedsearch(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  NspList *L;
  char *str;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;;
  O =nsp_sorted_list_search(L,str);
  if ( O == NULLOBJ ) return 0;
  else 
    {
      MoveObj(stack,1,O);
      return 1;
    }
}

/*
 * search a sorted NspList 					  
 */

static int int_lxsortedsearchandremove(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  NspList *L;
  char *str;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if ((L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;;
  O =nsp_sorted_list_search_and_remove(L,str);
  if ( O == NULLOBJ ) return 0;
  else 
    {
      MoveObj(stack,1,O);
      return 1;
    }
}

/*
 * create a tlist inserting the rhs object 
 * on the stack in the list 
 * if rhs == 0 an empty list is created 
 */


static int int_lx_tlist_as_list(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat;
  int i;
  NspList *L;
  if ( rhs < 1 ) 
    {
      Scierror("Error:\tNeed at least one argument for tlist creation");
      return RET_BUG;
    }
  CheckLhs(1,1);
  if ((HMat = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( HMat->mn == 0 )
    {
      Scierror("Error:\t%s", ArgPosition(1));
      ArgName(stack,1);
      Scierror(" of function %s is an empty string matrix\n",stack.fname);
      return RET_BUG;
    }
  if ((L =nsp_list_create(NVOID,HMat->S[0]))==NULLLIST) return RET_BUG;
  for ( i = 1; i <= rhs ; i++ )
    {
      char *oname = (i-1 < HMat->mn ) ? HMat->S[i-1] : "lel";
      if ( MaybeObjCopy(&NthObj(i)) == NULL)  return RET_BUG;
      if (nsp_object_set_name(NthObj(i),oname) == FAIL) return FAIL;
      if (nsp_list_end_insert(L,NthObj(i)) == FAIL ) return RET_BUG;
      /* If NthObj(i) is not copied it is inserted in the list 
      we must set then NthObj(i) to NULLOBJ 
      to prevent the cleaning process to clean the object 
      that we have inserted in our list **/
      NthObj(i) = NULLOBJ ;
    }
  NthObj(1)=(NspObject *) L;
  NSP_OBJECT(NthObj(1))->ret_pos = 1;
  return 1;
}


/*
 * FIXME temp for scicos 
 * here tlist is a hash table 
 */

static int int_lx_tlist_as_hash(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *HMat;
  int i;
  NspHash *H;
  NspSMatrix *Tname;
  if ( rhs < 1 ) 
    {
      Scierror("Error:\tNeed at least one argument for tlist creation");
      return RET_BUG;
    }
  CheckLhs(1,1);
  CheckRhs(1,1000);
  if ((HMat = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( HMat->mn == 0 )
    {
      Scierror("Error:\t%s", ArgPosition(1));
      ArgName(stack,1);
      Scierror(" of function %s is an empty string matrix\n",stack.fname);
      return RET_BUG;
    }
  if(( H = nsp_hash_create(NVOID,rhs)) == NULLHASH) return RET_BUG;
  if ( ( Tname = nsp_smatrix_create("type",1,1,HMat->S[0],1))== NULLSMAT) return RET_BUG;
  if (nsp_hash_enter(H,NSP_OBJECT(Tname)) == FAIL) return RET_BUG;
  for ( i = 2; i <= rhs ; i++ )
    {
      char *oname = (i-1 < HMat->mn ) ? HMat->S[i-1] : "lel";
      if ( MaybeObjCopy(&NthObj(i)) == NULL)  return RET_BUG;
      if (nsp_object_set_name(NthObj(i),oname) == FAIL) return FAIL;
      if (nsp_hash_enter(H,NthObj(i)) == FAIL) return RET_BUG;
      /* If NthObj(i) is not copied it is inserted in the list 
       * we must set then NthObj(i) to NULLOBJ 
       * to prevent the cleaning process to clean the object 
       * that we have inserted in our list 
       */
      NthObj(i) = NULLOBJ ;
    }
  NthObj(1)=(NspObject *) H;
  NSP_OBJECT(NthObj(1))->ret_pos = 1;
  return 1;
}


/*
 * return %null on the stack
 */

extern NspObject *Null;

static int int_lxnull(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(1,1);
  NthObj(1) = Null ;
  NSP_OBJECT(NthObj(1))->ret_pos = 1;
  return 1;
}


/*
 * Extract requested elements of the list 
 * an store them on the stack 
 */

static int int_lxextract_m(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *Elts;
  int rmin,rmax,i,l,nret,L_name;
  NspObject *O;
  NspList *L;
  CheckRhs (2,2);
  if ((L    = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((Elts = GetMat(stack,2)) == NULLMAT) return RET_BUG;
  l =nsp_list_length(L);
  Bounds(Elts,&rmin,&rmax);
  if ( rmin < 1  || rmax > l) 
    {
      Scierror("Error:\tIndices out of bounds\n"); return RET_BUG;
    }
  nret = Elts->mn;
  L_name=Ocheckname(L,NVOID);

  for ( i = 0 ; i < nret ; i++ )
    {
      O =nsp_list_get_element(L,((int) Elts->R[i]));
      if ( O == NULLOBJ ) 
	{
	  Scierror("Error:\t%s does not exists\n",
		   ArgPosition((int) Elts->R[i]));
	  return RET_BUG;
	}
      if ((O=nsp_object_copy(O)) == NULLOBJ) return RET_BUG;
      NthObj(rhs+i+1)= O;
    }
  /* 
   * 
   */
  nsp_void_object_destroy(&NthObj(1));
  nsp_void_object_destroy(&NthObj(2));
  for ( i = 0 ; i < nret ; i++) 
    {
      NthObj(i+1)= NthObj(i+rhs+1);
      NSP_OBJECT(NthObj(i+1))->ret_pos = i+1;
      NthObj(i+rhs+1)= NULLOBJ;
    }
  return nret;
}

/* Un Pb a régler : la variable retournée a un nom .... */
/* XXXXX */

static int int_lxextract_s(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *Elts;
  int i,L_name;
  NspObject *O;
  NspList *L;
  if ((L    = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((Elts = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  L_name=Ocheckname(L,NVOID);
  for ( i = 0 ; i < Elts->mn ; i++ )
    {
      if ((O =nsp_list_search(L,Elts->S[i])) == NULLOBJ ) 
	{
	  Scierror("Error:\tindice %s does not exists\n",
		   Elts->S[i]);
	  return RET_BUG;
	}
      /* If NspList has no name we must copy */
      if ( L_name ) 
	{
	  if ((O=nsp_object_copy(O)) == NULLOBJ) return RET_BUG;
	}
      NthObj(rhs+1+i)= O;
    }
  nsp_void_object_destroy(&NthObj(1));
  nsp_void_object_destroy(&NthObj(2));
  for ( i = 0 ; i < Elts->mn ; i++) 
    {
      NthObj(i+1)= NthObj(i+rhs+1);
      NSP_OBJECT(NthObj(i+1))->ret_pos = i+1;
      NthObj(i+rhs+1)= NULLOBJ;
    }
  return Elts->mn;
}

/* extract list element with a list argument : L(List(....))  */ 
/* first step :  At the end of this step 
 * first argument is the list 
 * next is the extracted list element 
 * next is the last extraction indices 
 */ 

/* XXXX : améliorer le netoyage en cas d'erreur 
 * following path for a recursive object with list or hash tables 
 * as sucessive nodes 
 */ 

int ListFollowExtract(Stack stack, int rhs, int opt, int lhs)
{
  int count=1,n,L_name;
  NspObject *O = NULLOBJ, *L=NULLOBJ;
  Cell *C;
  NspList *Lind;
  if ( IsListObj(stack,1 ) ) 
    {
      if ((L =(NspObject *) GetList(stack,1 )) == NULLOBJ ) return RET_BUG;
    }
  else if ( IsHashObj(stack,1 ) )
    {
      if ((L =(NspObject *) GetHash(stack,1 )) == NULLOBJ ) return RET_BUG;
    }
  L_name=Ocheckname(L,NVOID);
  if ((Lind = GetList(stack,2 )) == NULLLIST) return RET_BUG;
  C= Lind->first;
  O= (NspObject *) L;
  while ( C != NULLCELL) 
    {
      char *str ; 
      if ( C->O == NULLOBJ )
	{
	  Scierror("Error: \t%s of list does not exists\n",ArgPosition(count));
	  return RET_BUG;
	}
      if ( C->next != NULL) 
	{
	  /* extraction of list element i.e int or string */
	  if ( IsMat(C->O)) 
	    {
	      if ( IntScalar(C->O,&n)== FAIL)  return RET_BUG;
	      if ( ! IsList(O) )
		{
		  Scierror("Errro:\t, error in list extraction which does not give a list\n");
		  return RET_BUG;
		}
	      if ( ( O =nsp_list_get_element((NspList *) O,n)) == NULLOBJ) return RET_BUG;
	    }
	  else if ((str=nsp_string_object(C->O)) != NULL) 
	    {
	      if ( IsList(O) )
		{
		  if ((O =nsp_list_search((NspList *)O,str)) == NULLOBJ ) return RET_BUG;		
		}
	      else if ( IsHash(O) )
		{
		  NspObject *O1;
		  if (nsp_hash_find((NspHash *) O,str,&O1) == FAIL) return RET_BUG ;
		  O= O1;
		}
	      else
		{
		  Scierror("Errro:\t,error in list extraction which does not give a list\n");
		  return RET_BUG;
		}
	    }
	  else 
	    {
	      Scierror("Errro:\t,error in list extraction indice should be an int or a string \n");
	      return RET_BUG;
	    }
	  /* If NspList has no name we must copy */
	  if ( L_name ) 
	    {
	      if ((O=nsp_object_copy(O)) == NULLOBJ) return RET_BUG;
	    }
	}
      else
	{
	  NspObject *CO=C->O;
	  /* last extraction : here O can be anything */ 
	  L_name=Ocheckname(NthObj(2),NVOID);
	  if ( L_name) 
	    { 
	      if ((CO =nsp_object_copy(C->O)) == NULLOBJ) return RET_BUG;
	      nsp_object_destroy(&NthObj(2));
	    }
	  NthObj(2)=O;
	  NthObj(3)=CO;
	  break; 
	}
      C = C->next ;
    }
  /* XXXX : pas clair **/ 
  return 3;
}

/* performs the list extraction */ 

static int int_lxextract_l(Stack stack, int rhs, int opt, int lhs)
{
  char name[NAME_MAXL];
  int rep,n ;
  if ( (rep = ListFollowExtract(stack,rhs,opt,lhs)) < 0 ) return rep; 
  /* last extraction : here O can be anything */ 
  FuncEvalBuildName("extractelts",stack,stack.first+1,1,name);
  if ((n=FuncEval(NULLOBJ,name,stack,stack.first+1,2,0,1)) < 0) 
    {
      return RET_BUG;
    }
  nsp_void_object_destroy(&NthObj(1));
  NSP_OBJECT(NthObj(2))->ret_pos = 1;
  return 1;
}

static int int_lxextract(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(2,2);
  if (IsMatObj(stack,2)) 
    {
      return int_lxextract_m(stack,rhs,opt,lhs);
    }
  else if ( IsSMatObj(stack,2) ) 
    {
      return int_lxextract_s(stack,rhs,opt,lhs);
    }
  else if ( IsListObj(stack,2) ) 
    {
      return int_lxextract_l(stack,rhs,opt,lhs);
    }
  else 
    {
      Scierror("Error: Wrong type for argument in list extraction int, string or list required\n");
      return RET_BUG;
    }
  return 1;
}

/*
 * Extract all the elements of the list 
 * an store them on the stack 
 */ 

static int int_lxextractall(Stack stack, int rhs, int opt, int lhs)
{
  int count =1,i,delete=FALSE;
  Cell *C;
  NspList *L;
  CheckRhs(1,1);

  if ((L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ( Ocheckname(NthObj(1),NVOID) ) delete = TRUE;

  C= L->first;

  while ( C != NULLCELL) 
    {
      if ( C->O == NULLOBJ )
	{
	  Scierror("Error:\t%s does not exists\n",ArgPosition(count));
	  return RET_BUG;
	}
      NthObj(count+1)= C->O;
      if ( delete == TRUE) C->O = NULLOBJ;
      C = C->next ;
      count++;
    }

  /* we can destroy the list if it has no name since 
   * in the preceding list loop C->O were set to 
   * NULLOBJ in that case. This destroying variable 1 
   * will not alter the extracted elements 
   * typical case not very usefull but for which 
   * first element has to be destroyed : list(1,2,3)(:) 
   */
  nsp_void_object_destroy(&NthObj(1));

  for ( i = 1 ; i < count ; i++) 
    {
      NthObj(i)= NthObj(i+1);
      NSP_OBJECT(NthObj(i))->ret_pos = i;
    }
  NthObj(count)=NULLOBJ;
  return count-1;
}

/*
 * Extract a sublist by following a path
 * an create a New list with copy of them
 */
#if 0 
static int int_lxpath(Stack stack, int rhs, int opt, int lhs)
{
  int i;
  NspObject *O = NULLOBJ;
  NspList *L;
  if ( rhs <= 1 ) 
    {
      Scierror("Error:\ttoo few arguments %d for list extraction\n",rhs);
      return RET_BUG;
    }
  CheckLhs(1,1);
  if ((L = GetList(stack,rhs)) == NULLLIST) return RET_BUG;
  for ( i = 1; i < rhs ; i++) 
    {
      int n1;
      if ( GetScalarInt(stack,i,&n1) == FAIL) return RET_BUG;
      if ( ( O =nsp_list_get_element(L,n1)) == NULLOBJ) return RET_BUG;
      if ( i != rhs -1 &&  ! check_cast(O,nsp_type_list_id))
	{
	  Scierror("Errro:\t,error in list extraction which does not give a list\n");
	  return RET_BUG;
	}
      L= (NspList *) O;
    }
  /* XXXX : pas clair **/ 
  nsp_void_object_destroy(&NthObj(1));
  NthObj(1) = O;
  NthObj(1) = O;
  return 1;
}
#endif 

/*
 * compact the list 
 * Attention XXXX en faire une méthode 
 * qui modifie son argument 
 */

static int int_lxcompact(Stack stack, int rhs, int opt, int lhs)
{
  char c='c';
  NspList *L;
  CheckRhs(1,2); 
  CheckLhs(1,1);
  if ((L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ( rhs == 2 ) 
    {
      char *str;
      if ((str = GetString(stack,2)) == (char*)0) return RET_BUG;;
      if ( strlen(str) > 0 && (str[0]== 'c' || str[0]== 'r')) c=str[0];
      else 
	{
	  Scierror("%s: second argument must be 'c' or 'r'\n",stack.fname);
	  return RET_BUG;
	}
    }
  if (nsp_list_compact(L,c) == FAIL) return RET_BUG; 
  NthObj(1)->ret_pos = 1;
  return 1;
}


/*
 * Delete or Insert New elements in a NspList 
 * The Argument L MUST be changed by this function 
 * Arguments : L, Mat, val1,...,valn 
 * where Mat is of size n 
 */

static int int_lxsetrc(Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  NspSMatrix *S;
  int i;
  NspList *L;
  if ( rhs < 3 ) 
    {
      Scierror("Error:\tNot enough argument for list insertion\n");
      return RET_BUG;
    }
  CheckLhs(1,1);
  if ((L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  
  if (IsMatObj(stack,2)  ) 
    {
      if (( M=GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
      if ( M->mn != rhs - 2) 
	{
	  Scierror("Error:\tNot enought rhs (%d) for list insertion \n",rhs-2);
	  Scierror("\texpecting %d rhs arguments\n",M->mn);
	  ArgMessage(stack,1);
	  return RET_BUG;
	}
      for ( i = 0 ; i < M->mn ; i++) 
	{
	  if (Ocheckname(NthObj(3+i),"%null"))
	    {
	      if (nsp_list_delete_elt(L,(int) M->R[i]) == FAIL ) return RET_BUG;
	    }
	  else
	    {
	      if ( MaybeObjCopy(&NthObj(3+i)) == NULL)  return RET_BUG;
	      if (nsp_object_set_name(NthObj(3+i),"lel") == FAIL) return RET_BUG;
	      if (nsp_list_insert(L,NthObj(3+i),(int) M->R[i]) == FAIL ) return RET_BUG;
	      /* since NthObj(3) has a name it won't be cleaned up **/
	    }
	}
    }
  else if ( IsSMatObj(stack,2)  ) 
    {
      if (( S=GetSMat(stack,2)) == NULLSMAT ) return RET_BUG;
      if ( S->mn != rhs - 2) 
	{
	  Scierror("Error:\tNot enought rhs arguments (%d) for list insertion \n",rhs-2);
	  Scierror("\texpecting %d rhs arguments\n",S->mn);
	  ArgMessage(stack,1);
	  return RET_BUG;
	}
      for ( i = 0 ; i < S->mn ; i++) 
	{
	  if (Ocheckname(NthObj(3+i),"%null"))
	    {
	      Scierror("Error: You are not allowed to delete entries in a tlist\n");
	      return RET_BUG;
	      /*nsp_list_delete_elt_by_name(L,S->S[i]); **/
	    }
	  else
	    {
	      if ( MaybeObjCopy(&NthObj(3+i)) == NULL)  return RET_BUG;
	      if (nsp_object_set_name(NthObj(3+i),S->S[i]) == FAIL) return RET_BUG;
	      if (nsp_list_search_and_replace(L,NthObj(3+i)) == FAIL ) 
		{
		  Scierror("Error: %s not valid as a NspList extraction indice\n",S->S[i]);
		  return RET_BUG;
		}
	      /* since NthObj(3) has a name it won't be cleaned up **/
	    }
	}
    }
  else 
    {
      Scierror("Error: Wrong type for argument in list extraction int or string\n");
      Scierror("\trequired\n");
      return RET_BUG;
    }
  NSP_OBJECT(L)->ret_pos = 1;
  return 1;
}

/*
 * Res=nsp_list_create(str)
 * create an empty list 
 */

static int int_lxcreate(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L;
  CheckRhs(0,0);
  CheckLhs(1,1);
  if ((L =nsp_list_create(NVOID,NULLSTRING))==NULLLIST) return RET_BUG;
  NthObj(1)= (NspObject *) L;
  NSP_OBJECT(NthObj(1))->ret_pos = 1;
  return 1;
}

/*
 * delete List
 */

static int int_lxdestroy(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L;
  CheckRhs(1,1);
  CheckLhs(0,0);
  if ((L = GetList(stack,1)) == NULLLIST) return RET_BUG;
 nsp_object_destroy(&NthObj(1));
  return 0;
}

/*
 * Res=nsp_list_copy(L)
 * Copy NspList L : the elements are copied 
 */

static int int_lxcopy(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L1,*L2;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( L1 = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if (( L2 =nsp_list_copy(L1)) == NULLLIST) return RET_BUG;
  StackStore(stack,(NspObject *)L2,2) ; 
  NSP_OBJECT(L2)->ret_pos = 1;
  return 1;  
}

/*
 * returns a pointer to the Nth (=nel) NspObject  of a NspList 
 */

static int int_lxnthel(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  NspList *L;
  int n1;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if ((O =nsp_list_get_element(L,n1)) == NULLOBJ) return RET_BUG;
  /* XXXX pas clair est-ce que O est une copie */ 
 nsp_void_object_destroy(&NthObj(1));
  NthObj(1) = O;
  NSP_OBJECT(NthObj(1))->ret_pos = 1;
  return 1;  
}

/*
 * insert Object A at end of NspList  
 * NspList must exists  
 * A is copied (if necessary)
 */

static int int_lxendi(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (( L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ( MaybeObjCopy(&NthObj(2)) == NULL)  return RET_BUG;
  if (nsp_list_end_insert(L,NthObj(2)) == FAIL ) return RET_BUG;
  /* If NthObj(2) is not copied it is inserted in the list 
      we must set then NthObj(2) to NULLOBJ 
      to prevent the cleaning process to clean the object 
      that we have inserted in our list **/
  NthObj(2) = NULLOBJ ;
  NSP_OBJECT(L)->ret_pos = 1;
  return 1;
}

/*
 *insert Object A in a NspList at position n 
 * a copy of object A is inserted if necessary 
 */

static int int_lxni(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspList *L;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if (( L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ( MaybeObjCopy(&NthObj(2))== NULL )  return RET_BUG;
  if ( GetScalarInt(stack,3,&n1) == FAIL) return RET_BUG;
  if (nsp_list_store(L,NthObj(2),n1) == FAIL ) return RET_BUG;
  /* If NthObj(2) is not copied it is inserted in the list 
      we must set then NthObj(2) to NULLOBJ 
      to prevent the cleaning process to clean the object 
      that we have inserted in our list **/
  NthObj(2) = NULLOBJ ;
  NSP_OBJECT(L)->ret_pos = 1;
  return 1;
}

/*
  supresses the nel th element of an Object of 
  type NspList ( the nth element is freed
*/

static int int_lxnthdel(Stack stack, int rhs, int opt, int lhs)
{
  int n1;
  NspList *L;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (( L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ( GetScalarInt(stack,2,&n1) == FAIL) return RET_BUG;
  if (nsp_list_delete_elt(L,n1) == FAIL ) return RET_BUG;
  NSP_OBJECT(L)->ret_pos = 1;
  return 1;
}

/*
  return the length of a list L
  L is not changed 
*/

static int int_lxlength(Stack stack, int rhs, int opt, int lhs)
{
  double d;
  NspObject *O;
  NspList *L;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if (( L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  d = (double)nsp_list_length(L);
  if (( O =nsp_create_object_from_double(NVOID,d)) == NULLOBJ ) return RET_BUG;
  StackStore(stack,O,2); 
  NSP_OBJECT(O)->ret_pos = 1;
  return 1;
}

/*
  Concatenation of two Object of type NspList 
  O2 is unchanged O1 is changed 
*/

static int int_lxconcat(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L1,*L2;
  CheckRhs(2,2);
  CheckLhs(1,1);
  if (( L1 = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if (( L2 = GetList(stack,2)) == NULLLIST) return RET_BUG;
  if (nsp_list_concat(L1,L2) < 0 ) return RET_BUG;
  /* est-ce que L2 est copiee ? XXXXX 
   * 
   */ 
  NthObj(2) = NULLOBJ; 
  NSP_OBJECT(L1)->ret_pos = 1;
  return 1;
}


/*
 * map function 
 */

static int int_lxmap(Stack stack, int rhs, int opt, int lhs)
{
  NspPList *PL;
  NspList *L,*args = NULLLIST ; 
  CheckRhs(2,3);
  CheckLhs(-1,1);
  if ((L = GetList(stack,1)) == NULLLIST ) return RET_BUG;
  if ((PL = GetNspPList(stack,2)) == NULLP_PLIST ) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if ((args = GetList(stack,3)) == NULLLIST ) return RET_BUG;
    }
  if ( ( L=nsp_list_map(L,PL,args)) == NULLLIST ) return RET_BUG;
  MoveObj(stack,1,(NspObject *) L);
  return 1;      
}

/*
 * foldr 
 */

static int int_lxfoldr(Stack stack, int rhs, int opt, int lhs)
{
  NspPList *PL;
  NspObject *O;
  NspList *L,*args = NULLLIST ; 
  CheckRhs(2,3);
  CheckLhs(-1,1);
  if ((L = GetList(stack,1)) == NULLLIST ) return RET_BUG;
  if ((PL = GetNspPList(stack,2)) == NULLP_PLIST ) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if ((args = GetList(stack,3)) == NULLLIST ) return RET_BUG;
    }
  if ( ( O =nsp_list_fold_right(L,PL,args)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;      
}

/*
 * used for L.xx 
 */

static int int_lxdot(Stack stack, int rhs, int opt, int lhs)
{
  char *key;
  NspList *L;
  NspObject *O;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L = GetList(stack,1)) == NULLLIST ) return RET_BUG;
  if ((key = GetString(stack,2)) == (char*)0) return RET_BUG;  
  if ((O =nsp_list_search(L,key)) == NULLOBJ ) 
    {
      Scierror("Error:\tindice %s does not exists\n",key);
      ArgMessage(stack,1);
      return RET_BUG;  
    }
  MoveObj(stack,1,O);
  return 1;      
}

/*
 * L1 == L2 
 */

static int int_lxeq(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L1,*L2;
  NspBMatrix *B;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L1 = GetList(stack,1)) == NULLLIST ) return RET_BUG;
  if ((L2 = GetList(stack,2)) == NULLLIST ) return RET_BUG;
  if (( B =nsp_list_equal(L1,L2))==NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;      
}


/*
 * L1 <> L2 
 */

static int int_lxneq(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L1,*L2;
  NspBMatrix *B;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L1 = GetList(stack,1)) == NULLLIST ) return RET_BUG;
  if ((L2 = GetList(stack,2)) == NULLLIST ) return RET_BUG;
  if (( B =nsp_list_not_equal(L1,L2))==NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;      
}

/*
 * The Interface for basic lists operations 
 */

static OpTab List_func[]={
  {"list",int_lxlist},
  {"tlist",int_lx_tlist_as_hash}, /* FIXME: FIXME: hash tables */
  {"mlist",int_lx_tlist_as_hash}, /* FIXME: hash tables */
  {"namedlist",int_lx_tlist_as_list}, /* list with names elements ? */
  {"null",int_lxnull},
  {"resize2vect_l",int_lxextractall},
  {"extract_l",int_lxextract},
  {"extractelts_l",int_lxextract}, 
  {"setrowscols_l",int_lxsetrc},
  {"lxcreate",int_lxcreate},
  {"lxdestroy",int_lxdestroy},
  {"lxcopy",int_lxcopy},
  {"lxnthel",int_lxnthel},
  {"lxendi",int_lxendi},
  {"lxni",int_lxni},
  {"lxnthdel",int_lxnthdel},
  {"lxlength",int_lxlength},
  {"lxconcat",int_lxconcat},
  {"sorted_list", int_lxsortedlist},
  {"sorted_list_search", int_lxsortedsearch},
  {"sorted_list_search_and_remove", int_lxsortedsearchandremove},
  {"$dot_l",int_lxdot },	          	/* L.a  */
  {"map",int_lxmap },	          	/* L.a  */
  {"foldr",int_lxfoldr },	          	/* L.a  */
  {"eq_l_l",int_lxeq},
  {"ne_l_l",int_lxneq},
  {"compact",int_lxcompact},
  {(char *) 0, NULL}
};

int List_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(List_func[i].fonc))(stack,rhs,opt,lhs);
}


/* used to walk through the interface table 
    (for adding or removing functions) **/

void List_Interf_Info(int i, char **fname, function (**f))
{
  *fname = List_func[i].name;
  *f = List_func[i].fonc;
}









