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
 *
 * basic hash tables for (key=name,int value). 
 * Only positive int can be stored, negative values are kept for 
 * internal use.
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>

#define  BHash_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "../interp/Eval.h"

#include "nsp/bhash.h"

/* 
 * NspBHash inherits from NspObject 
 */

int nsp_type_bhash_id=0;
NspTypeBHash *nsp_type_bhash=NULL;

NspTypeBHash *new_type_bhash(type_mode mode)
{
  NspTypeBHash *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_bhash != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_bhash;
    }
  if ((type =  malloc(sizeof(NspTypeBHash))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = bhash_attrs ; 
  type->get_attrs = (attrs_func *) int_bhash_get_attribute; 
  type->set_attrs = (attrs_func *) int_bhash_set_attribute;
  type->methods = bhash_get_methods; 
  type->new = (new_func *) new_bhash;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for bhash */ 

  top->pr = (print_func *) nsp_bhash_print;                    
  top->dealloc = (dealloc_func *) nsp_bhash_destroy;
  top->copy  =  (copy_func *) nsp_bhash_copy;                   
  top->size  = (size_func *) bhash_size;                  
  top->s_type =  (s_type_func *) bhash_type_as_string;    
  top->sh_type = (sh_type_func *) bhash_type_short_string;
  top->info = (info_func *) nsp_bhash_info ;                    
  /* top->is_true = (is_true_func  *) BHashIsTrue; */
  /* top->loop = (loop_func *) bhash_loop_extract ; */
  top->path_extract = (path_func *) bhash_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) nsp_bhash_object;
  top->eq  = (eq_func *) bhash_eq;
  top->neq  = (eq_func *) bhash_neq;
  top->save  = (save_func *) bhash_xdr_save;
  top->load  = (load_func *) bhash_xdr_load;

  /* specific methods for bhash */

  type->init = (init_func *) init_bhash;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_bhash_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_bhash
       */
      type->id =  nsp_type_bhash_id = nsp_new_type_id();
      nsp_type_bhash = type;
      if ( nsp_register_type(nsp_type_bhash) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_bhash(mode);
    }
  else 
    {
      type->id = nsp_type_bhash_id;
      return type;
    }
}
/*
 * initialize BHash instances 
 * locally and by calling initializer on parent class 
 */

static int init_bhash(NspBHash *o,NspTypeBHash *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of BHash 
 */

NspBHash *new_bhash() 
{
  NspBHash *loc; 
  /* type must exists */
  nsp_type_bhash = new_type_bhash(T_BASE);
  if ( (loc = malloc(sizeof(NspBHash)))== NULLBHASH) return loc;
  /* initialize object */
  if ( init_bhash(loc,nsp_type_bhash) == FAIL) return NULLBHASH;
  return loc;
}


/*----------------------------------------------
 * Object method redefined for BHash 
 *-----------------------------------------------*/

/*
 * size 
 */

static int bhash_size(NspBHash *H, int flag)
{
  switch (flag) 
    {
    case 0: return H->filled;
    case 1: return H->filled;
    case 2: return H->hsize;
    }
  return 0;
}


/*
 * type as string 
 */

static char bhash_type_name[]="BHash";
static char bhash_short_type_name[]="bh";

static char *bhash_type_as_string(void)
{
  return(bhash_type_name);
}

static char *bhash_type_short_string(void)
{
  return(bhash_short_type_name);
}



/*
 * A == B 
 */

static int bhash_eq(NspBHash *A, NspObject *B)
{
  int rep;
  if ( check_cast(B,nsp_type_bhash_id) == FALSE) return FALSE ;
  rep = nsp_bhash_full_equal(A,(NspBHash *) B);
  return rep;
}

/*
 * A != B 
 */

static int bhash_neq(NspBHash *A, NspObject *B)
{
  int rep;
  if ( check_cast(B,nsp_type_bhash_id) == FALSE) return TRUE;
  rep = nsp_bhash_full_not_equal(A,(NspBHash *) B);
  return rep;
}

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val 
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then 
 *       iterate on the result 
 */

static NspObject *bhash_path_extract(NspBHash *H, NspObject *O)
{
  int val;
  NspSMatrix *M;
  /* faire une fonction ou macros pour un string XXXX **/
  if (( M =nsp_smatrix_object(O)) == NULLSMAT || M->mn != 1) return NULLOBJ ;
  if (nsp_bhash_find(H,M->S[0],&val) == FAIL) return NULLOBJ ;
  return NULLOBJ;
}

/*
 * save 
 */

static int bhash_xdr_save(XDR *xdrs, NspBHash *M)
{
  int i;
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->hsize) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->filled) == FAIL) return FAIL;
  /* last entry is at M->hsize ! */
  for ( i =0 ; i <= M->hsize ; i++) 
    {
      BHash_Entry *loc = ((BHash_Entry *)M->htable) + i;
      if ( loc->used && loc->key != NULL )
	{
	  if (nsp_xdr_save_string(xdrs, loc->key)== FAIL) return FAIL;
	  if (nsp_xdr_save_i(xdrs, loc->val ) == FAIL) return FAIL;
	}
    }
  return OK;
}

/*
 * load 
 */

static NspBHash  *bhash_xdr_load(XDR *xdrs)
{
  int hsize,filled,i,val;
  NspBHash *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLBHASH;
  if (nsp_xdr_load_i(xdrs,&hsize) == FAIL) return NULLBHASH;
  if (nsp_xdr_load_i(xdrs,&filled) == FAIL) return NULLBHASH;
  /* initial mxn matrix with unallocated elements **/
  if ( ( M = nsp_bhash_create(name,hsize) ) == NULLBHASH) return  NULLBHASH;
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < filled ; i++ )
    {
      if (nsp_xdr_load_string(xdrs,name,NAME_MAXL)== FAIL) return NULLBHASH;
      if (nsp_xdr_load_i(xdrs, &val) == FAIL) return NULLBHASH;
      if (nsp_bhash_enter(M,name,val)== FAIL) return NULLBHASH;
    }
  return M;
}


/*
 * delete 
 */


void nsp_bhash_destroy(NspBHash *H)
{
  unsigned int i;
  /* last entry is at M->hsize ! */
  if ( H != NULLBHASH )
    {
      for ( i =0 ; i <= H->hsize ; i++) 
	{
	  BHash_Entry *loc = ((BHash_Entry *) H->htable) + i;
	  if ( loc->used && loc->key != NULL ) free(loc->key);
	}
      nsp_bhdestroy(H);
    }
}

/*
 * info 
 */

static void nsp_bhash_info_tree(NspBHash *H, int indent,char *name,int rec_level);

void nsp_bhash_info(NspBHash *H, int indent,char *name,int rec_level)
{
  int colors[]={ 34,32,31,35,36};
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;

  if ( user_pref.list_as_tree == TRUE ) 
    {
      nsp_bhash_info_tree(H,indent,name,rec_level);
      return;
    }

  if ( rec_level <= user_pref.pr_depth ) 
    {
      int i1;
      /* recursively call info on elements */
      Sciprintf1(indent,"%s\t=\t\th (%d/%d)\n",(strcmp(pname,NVOID) != 0) ? pname : "",H->filled,H->hsize);
      for ( i1 =0 ; i1 <= H->hsize  ; i1++) 
	{
	  BHash_Entry *loc = ((BHash_Entry *) H->htable) + i1;
	  if ( loc->used && loc->key != NULL) 
	    {
	      if ( rec_level >= 0 && rec_level <= 4) 
		{
		  int col=colors[rec_level];
		  Sciprintf1(indent+2,"\033[%dm%s\033[0m = %d\n",col,loc->key,loc->val);
		}
	      else 
		{
		  Sciprintf1(indent+2,"%s\t= %d\n",loc->key,loc->val);
		}
	    }
	}
    }
  else
    {
      Sciprintf1(indent,"%s\t= ...\t\th (%d/%d)\n",(strcmp(pname,NVOID) != 0) ? pname : "",H->filled,H->hsize);
    }
} 

static void nsp_bhash_info_tree(NspBHash *H, int indent,char *name,int rec_level)
{
  const int name_len=128;
  char epname[name_len];
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;

  if ( rec_level <= user_pref.pr_depth ) 
    {
      int i1,count=0;
      /* recursively call print on elements */
      Sciprintf1(indent,"%s+\n",(strcmp(pname,NVOID) != 0) ? pname : "");
      
      for ( i1 =0 ; i1 <= H->hsize  ; i1++) 
	{
	  BHash_Entry *loc = ((BHash_Entry *) H->htable) + i1;
	  if ( loc->used && loc->key != NULL) 
	    {
	      int j;
	      count++;
	      sprintf(epname,"%s",pname);
	      for ( j = 0 ; j < strlen(epname);j++) 
		{
		  if (epname[j] !='-' && epname[j] != '`' && epname[j] != ' ' && epname[j] != '|') epname[j]=' ';
		}
	      for ( j = 0 ; j < strlen(epname);j++) if (epname[j]=='-' || epname[j] == '`' ) epname[j]=' ';
	      if ( count == H->filled ) 
		strcat(epname,"`-");
	      else 
		strcat(epname,"|-");
	      strcat(epname,loc->key);
	      Sciprintf1(indent,"%s\t=%d\n",epname,loc->val);
	    }
	}
    }
  else
    {
      Sciprintf1(indent,"%s\t= ...\t\th (%d/%d)\n",(strcmp(pname,NVOID) != 0) ? pname : "",H->filled,H->hsize);
    }
}


/*
 * print 
 */

void nsp_bhash_print(NspBHash *H, int indent,char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;
  int count = 0;
  unsigned int i1;
  if (user_pref.pr_as_read_syntax)
    {
      const int name_len=128;
      char epname[name_len];
      sprintf(epname,"H__%d",rec_level);
      Sciprintf1(indent,"%s=hcreate(%d);\n",epname,H->hsize);
      /* last entry is at M->hsize ! */
      for ( i1 =0 ; i1 <= H->hsize ; i1++) 
	{
	  BHash_Entry *loc = ((BHash_Entry *) H->htable) + i1;
	  if ( loc->used && loc->key != NULL) 
	    {
	      Sciprintf("%s('%s')\t=%d\n",epname,loc->key,loc->val);
	      count++;
	    }
	}
      if ( strcmp(pname,NVOID) != 0) Sciprintf1(indent+1,"%s=%s;\n",pname,epname);

    }
  else 
    {
      if ( rec_level <= user_pref.pr_depth ) 
	{
	  Sciprintf1(indent,"%s\t=\t\th (%d/%d)\n",pname,H->filled,H->hsize);
	  for ( i1 =0 ; i1 <= H->hsize ; i1++) 
	    {
	      BHash_Entry *loc = ((BHash_Entry *) H->htable) + i1;
	      if ( loc->used && loc->key != NULL) 
		{
		  Sciprintf1(indent+2,"%s\t= %d\n",loc->key,loc->val);
		}
	    }
	}
      else
	{
	  Sciprintf1(indent,"%s\t= ...\t\th (%d/%d)\n",pname,H->filled,H->hsize);
	}
    }
}



/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for BHash objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspBHash   *nsp_bhash_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_bhash_id) == TRUE) return ((NspBHash *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_bhash));
  return(NULL);
}

int IsBHashObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_bhash_id);
}

int IsBHash(const NspObject *O)
{
  return nsp_object_type(O,nsp_type_bhash_id);
}

NspBHash  *GetBHashCopy(Stack stack, int i)
{
  if (  GetBHash(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspBHash  *GetBHash(Stack stack, int i)
{
  NspBHash *M;
  if (( M = nsp_bhash_object(NthObj(i))) == NULLBHASH)
    ArgMessage(stack,i);
  return M;
}


/*-----------------------------------------------------
 * constructor 
 *-----------------------------------------------------*/

NspBHash *nsp_bhash_create(char *name, unsigned int size)
{
  return(nsp_bhcreate(name,size));
} 

/*
 * copy 
 */

NspBHash *nsp_bhash_copy(NspBHash *H)
{
  unsigned int i;
  NspBHash *Loc;
  Loc = nsp_bhcreate(NVOID,H->hsize);
  if ( Loc == NULLBHASH ) return NULLBHASH;
  for ( i =0 ; i <= H->hsize ; i++) 
    {
      BHash_Entry *loc = ((BHash_Entry *)H->htable) + i;
      if ( loc->used && loc->key != NULL )
	{
	  if ( nsp_bhsearch(Loc,loc->key,&loc->val,BH_ENTER) == FAIL) 
	    return NULLBHASH;
	}
    }
  return(Loc);
}

/*-------------------------------------------------------------------
 * wrappers for the BHash
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 * Create a BHash Table 
 * hcreate(size, x1=val1,.....,xn=valn)
 * or 
 * hcreate( x1=val1,.....,xn=valn)
 */

int int_bhtcreate(Stack stack, int rhs, int opt, int lhs)
{
  int n,first,i;
  NspBHash *H;
  if ( rhs - opt == 0 ) 
    {
      if ( opt == 0 ) {
	Scierror("Error: size of htable is null\n");
	return RET_BUG;
      }
      /* Only optional arguments are given */ 
      if(( H = nsp_bhash_create(NVOID,opt)) == NULLBHASH) return RET_BUG;
      first = 1;
    }
  else 
    {
      if (GetScalarInt(stack,1,&n) == FAIL) return RET_BUG;           
      if(( H = nsp_bhash_create(NVOID,n)) == NULLBHASH) return RET_BUG;
      first = 2;
    }
  for ( i = first ; i <= rhs ; i++) 
    {
      int val;
      if (GetScalarInt(stack,i,&val) == FAIL) goto bug;
      if (Ocheckname(NthObj(i),NVOID) ) 
	{
	  Scierror("Error: Cannot add unamed variable in bhash table\n");
	  Scierror("\t%s of function %s\n",ArgPosition(rhs),NspFname(stack));
	  return RET_BUG;
	}
      if (nsp_bhash_enter(H,nsp_object_get_name(NthObj(i)),val) == FAIL) return RET_BUG;
    }
  MoveObj(stack,1,(NspObject *) H);
  return 1;
 bug :
  nsp_bhash_destroy(H);
  return RET_BUG;
} 

/*------------------------------------------------------
 * attributes  
 *------------------------------------------------------*/

/* return all the keys H.keys entered in the bhash table as a string matrice  */

static NspObject * int_bhash_get_keys(void *Hv, char *attr)
{
  NspSMatrix *Loc;
  NspBHash *H = Hv;
  int i=0,count =0;
  if ( H->filled == 0) 
    {
      if ( ( Loc =nsp_smatrix_create_with_length(NVOID,0,0,-1) ) == NULLSMAT) return NULLOBJ;
    }
  else 
    {
      if ( ( Loc =nsp_smatrix_create_with_length(NVOID,H->filled,1,-1) ) == NULLSMAT) return NULLOBJ;
      /* allocate elements and store keys **/
      while (1) 
	{
	  char *str=NULL;
	  int val;
	  int rep = nsp_bhash_get_next_object(H,&i,&str,&val);
	  if ( str != NULL )
	    { 
	      if (( Loc->S[count++] =nsp_string_copy(str)) == (nsp_string) 0)
		return NULLOBJ;
	    }
	  if (rep == FAIL) break;
	}
      if ( count != H->filled )
	{
	  int i;
	  Sciprintf("Warning: less objects (%d) in bhash table than expected (%d) !\n",count,H->filled);
	  for ( i = count ; i < H->filled ; i++) Loc->S[i]=NULL;
	  if ( nsp_smatrix_resize(Loc,count,1) == FAIL) return NULLOBJ;
	}
    }
  return (NspObject *) Loc;
}

static int int_bhash_set_keys(void *Hv,const char *attr, NspObject *O)
{
  Scierror("attribute __keys of bhash instances cannot be set !\n");
  return FAIL;
}

static AttrTab bhash_attrs[] = {
  { "__keys", 	int_bhash_get_keys , 	int_bhash_set_keys , 	NULL },
  { (char *) 0, NULL}
};

/* 
 * get and set attributes are redefined for BHash Tables 
 * to access data stored in the bhash table 
 */

static int int_bhash_get_attribute(Stack stack, int rhs, int opt, int lhs)
{
  char *key;
  NspBHash *H;
  int val;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((H = GetBHash(stack,1)) == NULLBHASH) return RET_BUG;
  if ((key = GetString(stack,2)) == (char*)0) return RET_BUG;  
  /* if (nsp_bhash_find_and_copy(H,key,&O) == OK )  */
  /* here we do not copy the object since we can be in the 
   * middle of the evaluation of an expression like H.x in 
   * H.x.y= 6;
   * XXX pb ça donne un plantage 
   */
  if (nsp_bhash_find(H,key,&val) == OK )
    {
      /* H('key') **/
      if ( nsp_move_double(stack,1,(double) val)==FAIL) return RET_BUG;
      return 1;      
    }
  /* Check now if key is an attribute of object BHash **/
  return int_get_attribute(stack,rhs,opt,lhs);
}

static int int_bhash_set_attribute(Stack stack, int rhs, int opt, int lhs)
{
  int val;
  NspMatrix *M;
  NspObject *O;
  char *name;
  NspBHash *H;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H    = GetBHash(stack,1)) == NULLBHASH) return RET_BUG;
  if ((name = GetString(stack,2)) == (char*)0) return RET_BUG;  
  if ( strcmp(name,"__attrs") == 0 || strcmp(name,"__keys") == 0 ) 
    {
      Scierror("%s should not be used as a bhash table entry\n",name);
      return RET_BUG;
    }
  if ((  O = nsp_get_object(stack,3)) == NULLOBJ ) return RET_BUG;
  M = (NspMatrix *) O;
  if ( IsMat(O) && M->mn == 1 && M->rc_type == 'r')
    {
      val = (int) M->R[0];
    }
  else
    {
      Scierror("Attribute value should be an int\n");
      return RET_BUG;
    }
  if (nsp_bhash_enter(H,name,val) == FAIL) return RET_BUG;
  NSP_OBJECT(H)->ret_pos = 1;
  return 1;
}


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/
 
/*
 * Enters a copy of objects O in BHash Table
 * enter(H,O1,...,On)  or H.enter[O1,...,On]
 * H is changed 
 */

static int int_bhtenter(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  NspMatrix *M;
  int i,val;
  NspBHash *H = self ;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( i = 1 ; i <= rhs ; i++) 
    {
      if ( Ocheckname(NthObj(i),NVOID) ) 
	{
	  Scierror("Error: Cannot add unamed variable in bhash table\n");
	  Scierror("\t%s of function %s\n",ArgPosition(rhs),NspFname(stack));
	  return RET_BUG;
	}
      if (( O =nsp_get_object(stack,i)) == NULLOBJ ) return RET_BUG;
      M= (NspMatrix *) O;
      if ( IsMat(O) && M->mn == 1 && M->rc_type == 'r')
	{
	  val = (int) M->R[0];
	}
      else
	{
	  Scierror("Argument %d is not a scalar int\n",i);
	  return RET_BUG;
	}
      if (nsp_bhash_enter(H,nsp_object_get_name(NthObj(i)),val) == FAIL) return RET_BUG;
    }
  return 0;
}
 
/*
 * Insert Copies of elements of Htable 2 in BHash table 1
 */

static int int_bhtmerge(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspBHash *H1=self,*H2;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((H2 = GetBHash(stack,1)) == NULLBHASH) return RET_BUG;
  if (nsp_bhash_merge(H1,H2) == FAIL) return RET_BUG;
  return 0;
}

/*
 * Delete entries given by string matrix S str from BHash Table
 * delete(H,['arg1',....,'argn']) or H.delete[['arg1',...,'argn']]
 */

static int int_bhtdelete(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int i,j;
  NspSMatrix *S;
  NspBHash *H = self ;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( j = 1; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	nsp_bhash_remove(H,S->S[i]);
    }
  return 0;
}

/*
 * Find Object with key str in BHashTable
 * Warning the object is returned (not a copy 
 *         of it) since this function is used 
 *         to change the BHashtable 
 */

static int int_meth_bhtfind(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  int i,j,count=0;
  NspBHash *H=self;
  NspObject *O;
  int val; 
  CheckRhs(1,1000);
  CheckLhs(1,1000);
  lhs=Max(lhs,1);
  for ( j = 1 ; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	{
	  if (nsp_bhash_find(H,S->S[i],&val) == FAIL)   
	    {
	      Scierror("%s: key %s not found in bhash table \n",NspFname(stack));
	      return RET_BUG  ;
	    }
	  else
	    {
	      if ((O=nsp_create_object_from_double(NVOID,val))==NULLOBJ) return RET_BUG;    
	      NthObj(rhs+ ++count) = O ;
	      NSP_OBJECT(O)->ret_pos = count;
	    }
	  if (count == lhs) break;
	}
      if (count == lhs) break;
    }
  return count;
}

/*
 * checks if keys are present in bhash table 
 * results are returned in boolean matrice 
 */

static int int_meth_iskey(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  int i,j,count=0;
  NspBHash *H=self;
  int val;
  NspBMatrix *Res;
  CheckRhs(1,1000);
  CheckLhs(1,1000);
  lhs=Max(lhs,1);
  for ( j = 1 ; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      if ((Res = nsp_bmatrix_create(NVOID,S->m,S->n))== NULLBMAT) return RET_BUG;
      for ( i = 0 ; i < S->mn ; i++ ) 
	{
	  Res->B[i] = (nsp_bhash_find(H,S->S[i],&val) == FAIL)? FALSE:TRUE;
	}
      NthObj(rhs+ ++count) = NSP_OBJECT(Res) ;
      NSP_OBJECT(Res)->ret_pos = count;
      if (count == lhs) break;
    }
  return count;
}



static NspMethods bhash_methods[] = {
  { "delete", int_bhtdelete},
  { "enter", int_bhtenter},
  { "iskey", int_meth_iskey},
  { "find", int_meth_bhtfind},
  { "merge", int_bhtmerge},
  { (char *) 0, NULL}
};

static NspMethods *bhash_get_methods(void) { return bhash_methods;};

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 *  H(a1,a2,....) les ai peuvent etre ['a','b']
 *  H('a') , H(['a','b']) 
 */ 

static int int_bhtfind(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  int i,j,count=0;
  NspObject *O;
  int val;
  NspBHash *H;
  CheckRhs(2,1000);
  CheckLhs(1,1);
  
  if ((H   = GetBHash(stack,1)) == NULLBHASH) return RET_BUG;

  for ( j = 2 ; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	{
	  if (nsp_bhash_find(H,S->S[i],&val) == FAIL)   
	    {
	      Scierror("%s: key %s not found in bhash table \n",NspFname(stack),S->S[i]);
	      return RET_BUG  ;
	    }
	  else
	    {
	      if ((O=nsp_create_object_from_double(NVOID,val))==NULLOBJ) return RET_BUG;    
	      NthObj(rhs+ ++count) = O;
	      NSP_OBJECT(O)->ret_pos = count;
	    }
	}
    }
  return count;
}

/*
 * H(list(....))
 */

static int int_bht_extract_l(Stack stack, int rhs, int opt, int lhs) 
{
  char name[NAME_MAXL];
  int rep,n ;
  if ( (rep = ListFollowExtract(stack,rhs,opt,lhs)) < 0 ) return rep; 
  /* last extraction : here O can be anything */ 
  nsp_build_funcname("extractelts",stack,stack.first+1,1,name);
  if ((n=nsp_eval_func(NULLOBJ,name,stack,stack.first+1,2,0,1)) < 0) 
    {
      return RET_BUG;
    }
  nsp_void_object_destroy(&NthObj(1));
  NSP_OBJECT(NthObj(2))->ret_pos = 1;
  return 1;
}

/*
 * H(x)
 */

static int int_bhtfind_elt(Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs == 2 && IsListObj(stack,2) ) 
    {
      return int_bht_extract_l(stack,rhs,opt,lhs);
    }
  else 
    {
      return int_bhtfind(stack,rhs,opt,lhs);
    }
}


/*
 * L1 == L2 
 */

static int int_bht_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspBHash *L1,*L2;
  NspBMatrix *B;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L1 = GetBHash(stack,1)) == NULLBHASH ) return RET_BUG;
  if ((L2 = GetBHash(stack,2)) == NULLBHASH ) return RET_BUG;
  if (( B =nsp_bhash_equal(L1,L2))==NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;      
}


/*
 * L1 <> L2 
 */

static int int_bht_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspBHash *L1,*L2;
  NspBMatrix *B;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L1 = GetBHash(stack,1)) == NULLBHASH ) return RET_BUG;
  if ((L2 = GetBHash(stack,2)) == NULLBHASH ) return RET_BUG;
  if (( B =nsp_bhash_not_equal(L1,L2))==NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;      
}


/*
 *
 */

static OpTab BHash_func[]={
  {"bhcreate",int_bhtcreate},  		/* could be renamed hcreate(10)
                                         * hcreate(x=67,y=89,...) 
                                         * hcreate(n,x=67,y=89,...) 
                                         * hcreate(n) 
					 */
  {"extract_bh",int_bhtfind},    		/* H(a1,a2,....) les ai peuvent etre ['a','b'] */
  {"extractelts_bh",int_bhtfind_elt},     /* H('a') , H(['a','b']) */
  {"setrowscols_bh",int_bhash_set_attribute},/* H('a')= o or H.a = o */
  {"eq_bh_bh",int_bht_eq},
  {"ne_bh_bh",int_bht_neq},
  {(char *) 0, NULL}
};

/* call ith function in the BHash interface */

int BHash_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(BHash_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void BHash_Interf_Info(int i, char **fname, function (**f))
{
  *fname = BHash_func[i].name;
  *f = BHash_func[i].fonc;
}


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

/* The bhash table code at the end of the file is Copyrighted
 * See Copyright (c) 1990, 1993 The Regents of the University of California.
 * in the file. 
 */

/*
 * FIXME : check increasing size criteria 
 */

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "nsp/object.h"

/*
 * BHashTable Object in Scilab : 
 *    store (key,value) in a bhash table where key is always a string 
 *    and value is a pointer to a Scilab Object Obj
 */

/**
 *nsp_bhash_resize:
 * @H: 
 * @new_size: 
 * 
 * Resizes a  BHashTable
 *
 * Return value: %OK or %FAIL
 **/

int nsp_bhash_resize(NspBHash *H, unsigned int new_size)
{
  unsigned int i;
  NspBHash *Loc;
  if ((Loc = nsp_bhcreate(NVOID,new_size)) == NULLBHASH ) return FAIL;
  for ( i =0 ; i <= H->hsize ; i++) 
    {
      BHash_Entry *loc = ((BHash_Entry *)H->htable) + i;
      if ( loc->used && loc->key != NULL)
	{
	  if ( nsp_bhsearch(Loc,loc->key,&loc->val,BH_ENTER) == FAIL) 
	    return FAIL;
	}
    }
  FREE(H->htable);
  H->htable = Loc->htable;
  H->hsize  = Loc->hsize;
  H->filled = Loc->filled;
  FREE(NSP_OBJECT(Loc)->name);
  FREE(Loc);
  return OK;
}

/**
 *nsp_bhash_merge:
 * @H1: 
 * @H2: 
 * 
 * Insert Copies of elements of has table H2 in bhash table H1
 * 
 * Return value: %OK or %FAIL 
 **/

int nsp_bhash_merge(NspBHash *H1,NspBHash *H2)
{
  unsigned int i;
  if ( H2 == NULLBHASH ||  H2->filled == 0 ) return OK;
  if ( (i=H1->filled + H2->filled) >= 2*(H1->hsize/3) ) 
    {
      if (nsp_bhash_resize(H1,2*i) == FAIL ) 
	{
	  Scierror("Error: running out of memory");
	  return FAIL;
	}
    }
  for ( i =0 ; i <= H2->hsize ; i++) 
    {
      BHash_Entry *loc = ((BHash_Entry *)H2->htable) + i;
      if ( loc->used && loc->key != NULL )
	{
	  if ( nsp_bhsearch(H1,loc->key,&loc->val,BH_ENTER)  == FAIL) 
	    return FAIL;
	}
    }
  return OK;
}

/**
 *nsp_bhash_get_next_object:
 * @H: 
 * @i: 
 * @O: 
 * 
 * Used to walk through all the elements of the bhash table 
 * return %FAIL when the end of the bhash table is reached
 * and nsp_bhash_get_next_object() is not to be called again 
 * The values present in the BHash table are returned 
 * in sequence (note that the key value is stored in the object) 
 * @i is incremented at each call. 
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_bhash_get_next_object(NspBHash *H, int *i,char **str,int *val)
{
  BHash_Entry *loc = ((BHash_Entry *) H->htable) + *i;  
  if ( loc->used && loc->key != NULL) 
    {
      *val = loc->val ;
      *str = loc->key;
    }

  else
    {
      *val = 0;
      *str = NULL;
    }
  (*i)++;
  return ( (*i) >= (int) H->hsize +1   ) ? FAIL: OK ;
}


#define FAIL_FULL -2

/**
 *nsp_bhash_enter:
 * @H: 
 * @O: 
 * 
 **/

/**
 * nsp_bhash_enter:
 * @H: 
 * @str: a string 
 * @val: an integer
 * 
 * Enters (copy(@str),@val) in the hash table @H. The 
 * string @str is copied.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_bhash_enter(NspBHash *H,const char *str,int val)
{
  char *str1;
  if ( str == NULL) return FAIL;
  if ((str1 =new_nsp_string(str)) == NULLSTRING) return FAIL;
  if ( H->filled >= 2*(H->hsize/3) ) 
    {
      if (nsp_bhash_resize(H,2*H->hsize) == FAIL ) 
	{
	  Scierror("Error: running out of memory");
	  nsp_string_destroy(&str1);
	  return FAIL;
	}
    }
  if ( nsp_bhsearch(H,str1,&val,BH_ENTER) == FAIL )
    {
      nsp_string_destroy(&str1);
      return FAIL;
    }
  return OK;
}

/**
 *nsp_bhash_remove:
 * @H: 
 * @str: 
 * 
 * Remove entry with key str from BHash Table
 * 
 **/

void nsp_bhash_remove(NspBHash *H, char *str)
{
  int val;
  nsp_bhsearch(H,str,&val,BH_REMOVE);
}

/**
 *nsp_bhash_find:
 * @H: 
 * @str: 
 * @O: 
 * 
 * Search bhash table entry with key str and returns it in #NspObject O
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_bhash_find(NspBHash *H,char *str, int *val)
{
  return( nsp_bhsearch(H,str,val,BH_FIND));
}


/**
 *nsp_bhash_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * Return value: 
 **/

NspBMatrix  *nsp_bhash_equal(NspBHash *L1, NspBHash *L2)
{
  NspBMatrix *B;
  int i=0, count=0;
  if ( L1->filled != L2->filled ) 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,1))== NULLBMAT) return NULLBMAT;
      B->B[0]=FALSE; 
      return B;
    }
  else 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,L1->filled))== NULLBMAT) return NULLBMAT;
      while (1) 
	{
	  char *str=NULL;
	  int val,val2;
	  int rep =nsp_bhash_get_next_object(L1,&i,&str,&val);
	  if ( str != NULL )
	    { 
	      if ( nsp_bhash_find(L2,str,&val2) == FAIL)
		{
		  B->B[count]= FALSE;
		}
	      else 
		{
		  B->B[count]= (val == val2 );
		}
	      count++;
	    }
	  if ( rep == FAIL) break;
	}
    }
  return  B;
}


/**
 *nsp_bhash_not_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
 **/


NspBMatrix  *nsp_bhash_not_equal(NspBHash *L1, NspBHash *L2)
{
  NspBMatrix *B;
  int i=0,count=0;
  if ( L1->filled != L2->filled ) 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,1))== NULLBMAT) return NULLBMAT;
      B->B[0]=TRUE; 
      return B;
    }
  else 
    {
      if (( B = nsp_bmatrix_create(NVOID,1,L1->filled))== NULLBMAT) return NULLBMAT;
      while (1) 
	{
	  char *str=NULL;
	  int val,val2;
	  int rep =nsp_bhash_get_next_object(L1,&i,&str,&val);
	  if ( str != NULL )
	    { 
	      if ( nsp_bhash_find(L2,str,&val2) == FAIL)
		{
		  B->B[count]= TRUE;
		}
	      else 
		{
		  B->B[count]= (val != val2);
		}
	      count++;
	    }
	  if ( rep == FAIL) break;
	}
    }
  return B;
} 


/**
 *nsp_bhash_full_equal:
 * @L1: 
 * @L2: 
 * 
 * nsp_bhash_equal(L1,L2)
 * if the two tables do not have the same length returns FALSE 
 * else returns and(L1(i)== L2(i)) 
 * 
 * 
 * Return value: 
 **/

int nsp_bhash_full_equal(NspBHash *L1, NspBHash *L2)
{
  int i=0,rep=TRUE;
  if ( L1->filled != L2->filled ) return FALSE;
  while (1) 
    {
      char *str=NULL;
      int val,val2;
      int rep1 = nsp_bhash_get_next_object(L1,&i,&str,&val);
      if ( str != NULL )
	{ 
	  if ( nsp_bhash_find(L2,str,&val2) == FAIL)
	    {
	      return FALSE;
	    }
	  else 
	    {
	      rep = (val == val2 );
	      if ( rep == FALSE) return rep;
	    }
	}
      if ( rep1 == FAIL) break;
    }
  return rep;
} 

/**
 *nsp_bhash_full_not_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_bhash_full_not_equal(NspBHash *L1, NspBHash *L2)
{
  int i=0,rep=FALSE;
  if ( L1->filled != L2->filled ) return TRUE;
  while (1) 
    {
      char *str=NULL;
      int val,val2;
      int rep1 = nsp_bhash_get_next_object(L1,&i,&str,&val);
      if ( str != NULL )
	{ 
	  if ( nsp_bhash_find(L2,str,&val2) == FAIL)
	    {
	      return TRUE;
	    }
	  else 
	    {
	      rep = (val != val2 );
	      if ( rep == TRUE) return rep;
	    }
	}
      if (rep1 == FAIL) break;
    }
  return rep;
} 



/*
 * BHashtable code : 
 * slightly modified to add REMOVE 
 * Jean-Philippe Chancelier 
 * 1998-1999
 */

/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)search.h	8.1 (Berkeley) 6/4/93
 */

/* Copyright (C) 1993 Free Software Foundation, Inc.
 *  This file is part of the GNU C Library.
 *  Contributed by Ulrich Drepper <drepper@ira.uka.de>
 *
 *  The GNU C Library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *   License, or (at your option) any later version.
 *
 *  The GNU C Library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with the GNU C Library; see the file COPYING.LIB.  If
 *  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
 *  Cambridge, MA 02139, USA.  
 */


/*
 * [Aho,Sethi,Ullman] Compilers: Principles, Techniques and Tools, 1986
 * [Knuth]            The Art of Computer Programming, part 3 (6.4)
 */


/* 
 * For the used double bhash method the table size has to be a prime. To
 * correct the user given table size we need a prime test.  This trivial
 * algorithm is adequate because
 * a)  the code is (most probably) only called once per program run and
 * b)  the number is small because the table must fit in the core
 */

static int isprime(unsigned int number)
{
  /* no even number will be passed */
  unsigned div = 3;
  
  while (div*div < number && number%div != 0)
    div += 2;
  
  return number%div != 0;
}

/**
 * nsp_bhcreate:
 * @name: #NspBHash object name 
 * @nel: initial size of the bhash table object.
 * 
 * creates a #NspBHash object with initial size greater than 
 * #nel. 
 * Before using the bhash table we must allocate memory for it.
 * Test for an existing table are done. We allocate one element
 * more as the found prime number says. This is done for more effective
 * indexing as explained in the comment for the hsearch function.
 * The contents of the table is zeroed, especially the field used 
 * becomes zero.
 *  
 * Return value: a #NspBHash object or %NULLBHASH
 **/

NspBHash *nsp_bhcreate(char *name, unsigned int nel)
{
  NspBHash *H = new_bhash();
  BHash_Entry *htable;
  /* Change nel to the first prime number not smaller as nel. */
  nel |= 1;      /* make odd */
  while (!isprime(nel)) nel += 2;
  if ( H == NULLBHASH)
    {
      Sciprintf("No more memory\n");
      return NULLBHASH;
    }
  if ((NSP_OBJECT(H)->name =new_nsp_string(name))== NULLSTRING) return NULLBHASH;
  NSP_OBJECT(H)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  H->hsize  = nel;
  H->filled = 0;
  if (( htable = (BHash_Entry *)calloc(H->hsize+1, sizeof(BHash_Entry))) == NULL)
    {
      Sciprintf("No more memory\n");
      return NULLBHASH;
    }
  H->htable = htable ;
  return H;
}

/**
 * nsp_bhdestroy:
 * @H: a #NspBHash object 
 * 
 * free a #NspBHash object but not the elements which were 
 * stored in the bhash table. This function is not to be used 
 * directly but through nsp_bhash_destroy() call.
 * 
 **/

void nsp_bhdestroy(NspBHash *H)
{
  /* free used memory */
  if ( H != NULLBHASH )
    {
      FREE(H->htable);
      FREE(NSP_OBJECT(H)->name);
      FREE(H);
    }
}

/**
 * nsp_bhsearch:
 * @H: #NspBHash object 
 * @key: key to search in the bhash table 
 * @data: a #NspObject pointer to be set with the searched object
 * @action: action to perform.
 * 
 * This is the search function. It uses double bhashing with open adressing.
 * The argument item.key has to be a pointer to an zero terminated, most
 * probably strings of chars. The function for generating a number of the
 * strings is simple but fast. It can be replaced by a more complex function
 * like ajw (see [Aho,Sethi,Ullman]) if the needs are shown.
 *
 * We use an trick to speed up the lookup. The table is created by hcreate
 * with one more element available. This enables us to use the index zero
 * special. This index will never be used because we store the first bhash
 * index in the field used where zero means not used. Every other value
 * means used. The used field can be used as a first fast comparison for
 * equality of the stored and the parameter value. This helps to prevent
 * unnecessary expensive calls of strcmp.
 * 
 * 
 * Return value: %OK, %FAIL. 
 **/

#define ACTION1								\
  switch (action)							\
    {									\
    case BH_REMOVE :							\
      break;								\
    case BH_ENTER :							\
      htable[idx].key = key;						\
      htable[idx].val = *val;						\
      (H->filled)++;							\
      return OK;							\
    case BH_FIND:							\
      break;								\
    }

#define ACTION2								\
  switch (action)							\
    {									\
    case BH_REMOVE :							\
      /* since other objects can be present with second			\
       * level keys with same bhash value we must keep the cell in use	\
       */								\
      /* htable[idx].used = 0; */					\
      free(htable[idx].key);						\
      htable[idx].key = NULL;						\
      (H->filled)--;							\
      return OK ;							\
      break;								\
    case BH_ENTER:							\
      free(htable[idx].key);						\
      htable[idx].key = key;						\
      htable[idx].val = *val;						\
      /* we are just replacing an existing object (H->filled)++;*/	\
      return OK;							\
    case BH_FIND :							\
      *val= htable[idx].val;						\
      return OK;							\
    }									



int nsp_bhsearch(NspBHash *H, char *key,int *val, BHashOperation action)
{
  register unsigned hval;
  register unsigned hval2;
  register unsigned idx;
  register const char *str;
  BHash_Entry *htable = H->htable;

  /*
   * If table is full and another entry should be entered return with 
   * error. We keep one free position to let the H_FIND, H_REMOVE work.
   */
  if (action == BH_ENTER && H->filled == H->hsize -1 ) 
    {
      Scierror("BHash Table %s is full\n",NSP_OBJECT(H)->name);
      return FAIL_FULL;
    }

  /* Compute a value for the given string. Perhaps use a better method. */
  /* modifs (bruno) : avoid the call to strlen and put the modulo outside the loop */
  hval  = 33;
  str = key;
  while (*str != '\0') { hval += *str ; str++; }
  hval %= H->hsize;

  /* First bhash function: simply take the modulo but prevent zero. */
  if (hval == 0) hval++;

  /* The first index tried. */
  idx = hval;

  if (htable[idx].used) 
    {
      /* Further action might be required according to the action value. */
      /* Sciprintf("First  bhash Testing idx=%d\n",idx); */
      if (htable[idx].used == hval )
	{
	  if (htable[idx].key == NULL ) 
	    {
	      ACTION1;
	    }
	  else if ( strcmp(htable[idx].key,key)==0 ) 
	    {
	      ACTION2;
	    }
	}
      
      /* Second bhash function, as suggested in [Knuth] */

      hval2 = 1 + hval % (H->hsize-2);
	
      do {
	/* 
	 * Because hsize is prime this guarantees to step through all
	 * available indeces.
	 */
	if (idx <= hval2)
	  idx = H->hsize+idx-hval2;
	else
	  idx -= hval2;

	/* Sciprintf("2nd bhash Testing idx=%d\n",idx); */
	/* If entry is found use it. */
	if (htable[idx].used == hval ) 
	  {
	    if (htable[idx].key == NULL )
	      {
		ACTION1;
	      }
	    else if ( strcmp(htable[idx].key,key)==0 )
	      {
		ACTION2;
	      }
	  }
      } while (htable[idx].used);
	
    }

  /* Sciprintf("End of bhash search idx=%d must be free \n",idx); **/
    
  /* An empty bucket has been found. */
  
  if (action == BH_ENTER )
    {
      htable[idx].key = key;
      htable[idx].used  = hval;
      htable[idx].val = *val;
      (H->filled)++;
      return OK ;
    }
  else 
    return FAIL;
}


