/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
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

#define  Hash_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "../interp/Eval.h"

/**
 * SECTION:hash
 * @title: Hash tables
 * @short_description: Hash tables with open addressing and double hashing
 * @see_also: 
 *
 * <para>
 * A #NspHash object is a hash table which uses open adressing and double 
 * hashing to access data. The hash keys are strings and the size of the 
 * hash table increases dynamically.
 * </para>
 **/

/* 
 * NspHash inherits from NspObject 
 */

int nsp_type_hash_id=0;
NspTypeHash *nsp_type_hash=NULL;

NspTypeHash *new_type_hash(type_mode mode)
{
  NspTypeHash *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_hash != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_hash;
    }
  if ((type =  malloc(sizeof(NspTypeHash))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = hash_attrs ; 
  type->get_attrs = (attrs_func *) int_hash_get_attribute; 
  type->set_attrs = (attrs_func *) int_hash_set_attribute;
  type->methods = hash_get_methods; 
  type->new = (new_func *) new_hash;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for hash */ 

  top->pr = (print_func *) nsp_hash_print;                    
  top->dealloc = (dealloc_func *) nsp_hash_destroy;
  top->copy  =  (copy_func *) nsp_hash_copy;                   
  top->size  = (size_func *) hash_size;                  
  top->s_type =  (s_type_func *) hash_type_as_string;    
  top->sh_type = (sh_type_func *) hash_type_short_string;
  top->info = (info_func *) nsp_hash_info ;                    
  /* top->is_true = (is_true_func  *) HashIsTrue; */
  /* top->loop = (loop_func *) hash_loop_extract ; */
  top->path_extract = (path_func *) hash_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) nsp_hash_object;
  top->eq  = (eq_func *) hash_eq;
  top->neq  = (eq_func *) hash_neq;
  top->save  = (save_func *) hash_xdr_save;
  top->load  = (load_func *) hash_xdr_load;

  /* specific methods for hash */

  type->init = (init_func *) init_hash;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_hash_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_hash
       */
      type->id =  nsp_type_hash_id = nsp_new_type_id();
      nsp_type_hash = type;
      if ( nsp_register_type(nsp_type_hash) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_hash(mode);
    }
  else 
    {
      type->id = nsp_type_hash_id;
      return type;
    }
}
/*
 * initialize Hash instances 
 * locally and by calling initializer on parent class 
 */

static int init_hash(NspHash *o,NspTypeHash *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Hash 
 */

NspHash *new_hash() 
{
  NspHash *loc; 
  /* type must exists */
  nsp_type_hash = new_type_hash(T_BASE);
  if ( (loc = malloc(sizeof(NspHash)))== NULLHASH) return loc;
  /* initialize object */
  if ( init_hash(loc,nsp_type_hash) == FAIL) return NULLHASH;
  return loc;
}


/*----------------------------------------------
 * Object method redefined for Hash 
 *-----------------------------------------------*/

/*
 * size 
 */

static int hash_size(NspHash *H, int flag)
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

static char hash_type_name[]="Hash";
static char hash_short_type_name[]="h";

static char *hash_type_as_string(void)
{
  return(hash_type_name);
}

static char *hash_type_short_string(NspObject *v)
{
  return(hash_short_type_name);
}



/*
 * A == B 
 */

static int hash_eq(NspHash *A, NspObject *B)
{
  int rep;
  if ( check_cast(B,nsp_type_hash_id) == FALSE) return FALSE ;
  rep = nsp_hash_full_equal(A,(NspHash *) B);
  return rep;
}

/*
 * A != B 
 */

static int hash_neq(NspHash *A, NspObject *B)
{
  int rep;
  if ( check_cast(B,nsp_type_hash_id) == FALSE) return TRUE;
  rep = nsp_hash_full_not_equal(A,(NspHash *) B);
  return rep;
}

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val 
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then 
 *       iterate on the result 
 */

static NspObject *hash_path_extract(NspHash *H,int n, NspObject **Objs, int *copy)
{
  NspObject *O1;
  NspSMatrix *M;
  *copy=FALSE;
  /* faire une fonction ou macros pour un string XXXX **/
  if ( n != 1 ) return NULLOBJ ;
  if (( M =nsp_smatrix_object(Objs[0])) == NULLSMAT || M->mn != 1) return NULLOBJ ;
  if (nsp_hash_find(H,M->S[0],&O1) == FAIL) return NULLOBJ ;
  return O1;
}

/*
 * save 
 */

static int hash_xdr_save(XDR *xdrs, NspHash *M)
{
  int i;
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->hsize) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->filled) == FAIL) return FAIL;
  /* last entry is at M->hsize ! */
  for ( i =0 ; i <= M->hsize ; i++) 
    {
      Hash_Entry *loc = ((Hash_Entry *)M->htable) + i;
      if ( loc->used && loc->data != NULLOBJ )
	{
	  if (nsp_object_xdr_save(xdrs, loc->data ) == FAIL) return FAIL;
	}
    }
  return OK;
}

/*
 * load 
 */

static NspHash  *hash_xdr_load(XDR *xdrs)
{
  int hsize,filled,i;
  NspHash *M;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLHASH;
  if (nsp_xdr_load_i(xdrs,&hsize) == FAIL) return NULLHASH;
  if (nsp_xdr_load_i(xdrs,&filled) == FAIL) return NULLHASH;
  /* initial mxn matrix with unallocated elements **/
  if ( ( M = nsp_hash_create(name,hsize) ) == NULLHASH) return  NULLHASH;
  /* allocate elements and store copies of A elements **/
  for ( i = 0 ; i < filled ; i++ )
    {
      NspObject *O =nsp_object_xdr_load(xdrs); 
      if ( O == NULLOBJ ) return NULLHASH;
      if (nsp_hash_enter(M,O)== FAIL) return NULLHASH;
    }
  return M;
}


/*
 * delete 
 */


void nsp_hash_destroy(NspHash *H)
{
  unsigned int i;
  /* last entry is at M->hsize ! */
  if ( H != NULLHASH )
    {
      for ( i =0 ; i <= H->hsize ; i++) 
	{
	  Hash_Entry *loc = ((Hash_Entry *) H->htable) + i;
	  if ( loc->used && loc->data != NULLOBJ ) 
	    nsp_object_destroy( &loc->data);
	}
      nsp_hdestroy(H);
    }
}

/*
 * info 
 */

static void nsp_hash_info_tree(NspHash *H, int indent,char *name,int rec_level);

int nsp_hash_info(NspHash *H, int indent,char *name,int rec_level)
{
  int colors[]={ 34,32,31,35,36};
  const int name_len=128;
  char epname[name_len];
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;

  if ( user_pref.list_as_tree == TRUE ) 
    {
      nsp_hash_info_tree(H,indent,name,rec_level);
      return TRUE;
    }

  if ( rec_level <= user_pref.pr_depth ) 
    {
      int i1;
      /* recursively call info on elements */
      Sciprintf1(indent,"%s\t=\t\th (%d/%d)\n",(strcmp(pname,NVOID) != 0) ? pname : "",H->filled,H->hsize);
      for ( i1 =0 ; i1 <= H->hsize  ; i1++) 
	{
	  Hash_Entry *loc = ((Hash_Entry *) H->htable) + i1;
	  if ( loc->used && loc->data != NULLOBJ) 
	    {
	      if ( rec_level >= 0 && rec_level <= 4) 
		{
		  int col=colors[rec_level];
		  sprintf(epname,"\033[%dm%s\033[0m",col,loc->data->name);
		  nsp_object_info(loc->data,indent+2,epname,rec_level+1);
		}
	      else 
		{
		  nsp_object_info(loc->data,indent+2,NULL,rec_level+1);
		}
	    }
	}
    }
  else
    {
      Sciprintf1(indent,"%s\t= ...\t\th (%d/%d)\n",(strcmp(pname,NVOID) != 0) ? pname : "",H->filled,H->hsize);
    }
  return TRUE;
} 

static void nsp_hash_info_tree(NspHash *H, int indent,char *name,int rec_level)
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
	  Hash_Entry *loc = ((Hash_Entry *) H->htable) + i1;
	  if ( loc->used && loc->data != NULLOBJ) 
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
	      strcat(epname,loc->data->name);
	      nsp_object_info(loc->data,indent,epname,rec_level+1);      
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

int nsp_hash_print(NspHash *H, int indent,char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;
  int count = 0;
  unsigned int i1;
  if (user_pref.pr_as_read_syntax)
    {
      const int name_len=128;
      char epname[name_len];
      char epname1[name_len];
      sprintf(epname,"H__%d",rec_level);
      Sciprintf1(indent,"%s=hash_create(%d);\n",epname,H->hsize);
      /* last entry is at M->hsize ! */
      for ( i1 =0 ; i1 <= H->hsize ; i1++) 
	{
	  Hash_Entry *loc = ((Hash_Entry *) H->htable) + i1;
	  if ( loc->used && loc->data != NULLOBJ) 
	    {
	      sprintf(epname1,"%s('%s')",epname,nsp_object_get_name(loc->data));
	      nsp_object_print(loc->data,indent+2,epname1,rec_level+1);
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
	      Hash_Entry *loc = ((Hash_Entry *) H->htable) + i1;
	      if ( loc->used && loc->data != NULLOBJ) 
		{
		  if ( nsp_object_print(loc->data,indent+2,NULL,rec_level+1)== FALSE) 
		    return FALSE;
		}
	    }
	}
      else
	{
	  Sciprintf1(indent,"%s\t= ...\t\th (%d/%d)\n",pname,H->filled,H->hsize);
	}
    }
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for Hash objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspHash   *nsp_hash_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_hash_id) == TRUE) return ((NspHash *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_hash));
  return(NULL);
}

int IsHashObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_hash_id);
}

int IsHash(const NspObject *O)
{
  return nsp_object_type(O,nsp_type_hash_id);
}

NspHash  *GetHashCopy(Stack stack, int i)
{
  if (  GetHash(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspHash  *GetHash(Stack stack, int i)
{
  NspHash *M;
  if (( M = nsp_hash_object(NthObj(i))) == NULLHASH)
    ArgMessage(stack,i);
  return M;
}


/*-----------------------------------------------------
 * constructor 
 *-----------------------------------------------------*/

NspHash *nsp_hash_create(char *name, unsigned int size)
{
  return(nsp_hcreate(name,size));
} 

/*
 * copy 
 */

NspHash *nsp_hash_copy(NspHash *H)
{
  unsigned int i;
  NspHash *Loc;
  Loc = nsp_hcreate(NVOID,H->hsize);
  if ( Loc == NULLHASH ) return NULLHASH;
  for ( i =0 ; i <= H->hsize ; i++) 
    {
      Hash_Entry *loc = ((Hash_Entry *)H->htable) + i;
      if ( loc->used && loc->data != NULLOBJ )
	{
	  if ( nsp_hsearch(Loc,nsp_object_get_name(loc->data),&loc->data,H_ENTER_COPY) == FAIL) 
	    return NULLHASH;
	}
    }
  return(Loc);
}

/*-------------------------------------------------------------------
 * wrappers for the Hash
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 * Create a Hash Table 
 * hcreate(size, x1=val1,.....,xn=valn)
 * or 
 * hcreate( x1=val1,.....,xn=valn)
 */

int int_htcreate(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int n,first,i;
  NspHash *H;
  if ( rhs - opt == 0 ) 
    {
      if ( opt == 0 ) {
	Scierror("Error: size of htable is null\n");
	return RET_BUG;
      }
      /* Only optional arguments are given */ 
      if(( H = nsp_hash_create(NVOID,opt)) == NULLHASH) return RET_BUG;
      first = 1;
    }
  else 
    {
      if (GetScalarInt(stack,1,&n) == FAIL) return RET_BUG;           
      if(( H = nsp_hash_create(NVOID,n)) == NULLHASH) return RET_BUG;
      first = 2;
    }
  for ( i = first ; i <= rhs ; i++) 
    {
      if ( Ocheckname(NthObj(i),NVOID) ) 
	{
	  Scierror("Error: Cannot add unamed variable in hash table\n");
	  Scierror("\t%s of function %s\n",ArgPosition(rhs),NspFname(stack));
	  return RET_BUG;
	}
      /* A copy of object is added in the hash table **/
      /* GetObj takes care of Hobj pointers **/
      if (( O =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
      if (nsp_object_set_name(O,nsp_object_get_name(NthObj(i))) == FAIL) return RET_BUG;
      if (nsp_hash_enter(H,O) == FAIL) return RET_BUG;
      /* A copy of object is added in the hash table **/
    }
  MoveObj(stack,1,(NspObject *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  
 *------------------------------------------------------*/

/* return all the keys H.keys entered in the hash table as a string matrice  */

static NspObject * int_hash_get_keys(void *Hv,const char *attr)
{
  return (NspObject *) nsp_hash_get_keys(Hv);
}

static int int_hash_set_keys(void *Hv,const char *attr, NspObject *O)
{
  Scierror("attribute __keys of hash instances cannot be set !\n");
  return FAIL;
}

static AttrTab hash_attrs[] = {
  { "__keys", 	int_hash_get_keys , 	int_hash_set_keys , 	NULL, NULL  },
  { (char *) 0, NULL, NULL , NULL , NULL }
};

/* 
 * get and set attributes are redefined for Hash Tables 
 * to access data stored in the hash table 
 */

static int int_hash_get_attribute(Stack stack, int rhs, int opt, int lhs)
{
  char *key;
  NspHash *H;
  NspObject *O;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((H = GetHash(stack,1)) == NULLHASH) return RET_BUG;
  if ((key = GetString(stack,2)) == (char*)0) return RET_BUG;  
  /* if (nsp_hash_find_and_copy(H,key,&O) == OK )  */
  /* here we do not copy the object since we can be in the 
   * middle of the evaluation of an expression like H.x in 
   * H.x.y= 6;
   * XXX pb ça donne un plantage 
   */
  if (nsp_hash_find(H,key,&O) == OK )
    {
      /* H('key') **/
      MoveObj(stack,1,O);
      return 1;      
    }
  /* Check now if key is an attribute of object Hash **/
  return int_get_attribute(stack,rhs,opt,lhs);
}

static int int_hash_set_attribute(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  const char *name;
  NspHash *H;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H    = GetHash(stack,1)) == NULLHASH) return RET_BUG;
  if (IsMatObj(stack,2)) 
    {
      int k;
      if ( GetScalarInt(stack,2,&k) == FAIL) return RET_BUG;
      if ( nsp_hash_find_by_number(H,k,&O) == FAIL) 
	{
	  Scierror("%s: key number %d not found in hash table \n",NspFname(stack),k);
	  return RET_BUG;
	}
      name = nsp_object_get_name(O);
    }
  else if ( IsSMatObj(stack,2) )
    {
      if ((name = GetString(stack,2)) == (char*)0) return RET_BUG;  
      if ( strcmp(name,"__attrs") == 0 || strcmp(name,"__keys") == 0 ) 
	{
	  Scierror("%s should not be used as a hash table entry\n",name);
	  return RET_BUG;
	}
    }
  else 
    {
      Scierror("%s: indice for extraction should be a string or an integer\n",NspFname(stack));
      return RET_BUG;
    }
  if ((  O = nsp_get_object(stack,3)) == NULLOBJ ) return RET_BUG;
  if ( Ocheckname(O,NVOID)==FALSE )
    {
      if (( O =nsp_object_copy(O)) == NULLOBJ ) return RET_BUG;
      /* Object at position 3 had a name and is not an object pointed to 
       * we remove it from the stack since it could be destroyed 
       * during nsp_hash_enter (ex in H;a = H.a) and should not be kept 
       * on the stack. 
       */
      if( IsHobj(NthObj(3))== FALSE )
	{
	  NthObj(3) = NULLOBJ;
	}
    }
  if (nsp_object_set_name(O,name) == FAIL) return RET_BUG;
  if (nsp_hash_enter(H,O) == FAIL) return RET_BUG;
  NSP_OBJECT(H)->ret_pos = 1;
  return 1;
}


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/
 
/*
 * Enters a copy of objects O in Hash Table
 * enter(H,O1,...,On)  or H.enter[O1,...,On]
 * H is changed 
 */

static int int_htenter(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int i;
  NspHash *H = self ;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( i = 1 ; i <= rhs ; i++) 
    {
      if ( Ocheckname(NthObj(i),NVOID) ) 
	{
	  Scierror("Error: Cannot add unamed variable in hash table\n");
	  Scierror("\t%s of function %s\n",ArgPosition(rhs),NspFname(stack));
	  return RET_BUG;
	}
      /* A copy of object is added in the hash table **/
      /* GetObj takes care of Hobj pointers **/
      if (( O =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
      if (nsp_object_set_name(O,nsp_object_get_name(NthObj(i))) == FAIL) return RET_BUG;
      if (nsp_hash_enter(H,O) == FAIL) return RET_BUG;
      /* A copy of object is added in the hash table **/
    }
  return 0;
}
 
/*
 * Insert Copies of elements of Htable 2 in Hash table 1
 */

static int int_htmerge(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H1=self,*H2;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((H2 = GetHash(stack,1)) == NULLHASH) return RET_BUG;
  if (nsp_hash_merge(H1,H2) == FAIL) return RET_BUG;
  return 0;
}

/*
 * Delete entries given by string matrix S str from Hash Table
 * delete(H,['arg1',....,'argn']) or H.delete[['arg1',...,'argn']]
 */

static int int_htdelete(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int i,j;
  NspSMatrix *S;
  NspHash *H = self ;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( j = 1; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	nsp_hash_remove(H,S->S[i]);
    }
  return 0;
}

/*
 * Find Object with key str in HashTable
 * Warning the object is returned (not a copy 
 *         of it) since this function is used 
 *         to change the Hashtable 
 */

static int int_hash_find_gen(NspHash *H,Stack stack, int rhs, int opt, int lhs, int j_init);

static int int_meth_htfind(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H=self;
  CheckRhs(1,1000);
  CheckLhs(1,1000);
  return int_hash_find_gen(H,stack,rhs,opt,lhs,1);
}

/*
 * checks if keys are present in hash table 
 * results are returned in boolean matrice 
 */

static int int_meth_iskey(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  int i,j,count=0;
  NspHash *H=self;
  NspObject *Ob;
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
	  Res->B[i] = (nsp_hash_find(H,S->S[i],&Ob) == FAIL)? FALSE:TRUE;
	}
      NthObj(rhs+ ++count) = NSP_OBJECT(Res) ;
      NSP_OBJECT(Res)->ret_pos = count;
      if (count == lhs) break;
    }
  return count;
}



static NspMethods hash_methods[] = {
  { "delete", int_htdelete},
  { "remove", int_htdelete},
  { "enter", int_htenter},
  { "iskey", int_meth_iskey},
  { "find", int_meth_htfind},
  { "merge", int_htmerge},
  { (char *) 0, NULL}
};

static NspMethods *hash_get_methods(void) { return hash_methods;};

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

/*
 *  H(a1,a2,....) les ai peuvent etre ['a','b']
 *  H('a') , H(['a','b']) 
 */ 

static int int_htfind(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H;
  CheckRhs(2,1000);
  CheckLhs(1,1);
  if ((H   = GetHash(stack,1)) == NULLHASH) return RET_BUG;
  return int_hash_find_gen(H,stack,rhs,opt,lhs,2);
}

/* utility function 
 */

static int int_hash_find_gen(NspHash *H,Stack stack, int rhs, int opt, int lhs, int j_init)
{
  NspSMatrix *S;
  int i,j,count=0;
  NspObject *O=NULLOBJ;
  lhs=Max(lhs,1);
  for ( j = j_init ; j <= rhs ; j++ )
    {
      if ( IsSMatObj(stack,j) ) 
	{
	  if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
	  for ( i = 0 ; i < S->mn ; i++ ) 
	    {
	      /* 
	      if (nsp_hash_find_and_copy(H,S->S[i],&O) == FAIL)   
		{
		  Scierror("%s: key %s not found in hash table \n",NspFname(stack),S->S[i]);
		  nsp_void_object_destroy(&O);
		  return RET_BUG  ;
		}
	      */
	      if (nsp_hash_find(H,S->S[i],&O) == FAIL)   
		{
		  Scierror("%s: key %s not found in hash table \n",NspFname(stack),S->S[i]);
		  return RET_BUG  ;
		}
	      else
		{
		  NthObj(rhs+ ++count) = O ;
		  NSP_OBJECT(O)->ret_pos = count;
		}
	      if (count == lhs) break;
	    }
	  if (count == lhs) break;
	}
      else if (IsMatObj (stack, j)) 
	{
	  int k;
	  if ( GetScalarInt(stack,j,&k) == FAIL) return RET_BUG;
	  /* we check here for object at position k in __keys */
	  if ( nsp_hash_find_by_number(H,k,&O) == FAIL) 
	    {
	      Scierror("%s: key number %d not found in hash table \n",NspFname(stack),k);
	      return RET_BUG;
	    }
	  /* 	  if ((O=nsp_object_copy(O))== NULLOBJ) return RET_BUG; */
	  NthObj(rhs+ ++count) = O ;
	  NSP_OBJECT(O)->ret_pos = count;	  
	  if (count == lhs) break;
	}
      else
	{
	  Scierror("%s: waiting for a string or an integer\n",NspFname(stack));
	  return RET_BUG; 
	}
    }
  return count;
}





/*
 * H(list(....))
 */

static int int_ht_extract_l(Stack stack, int rhs, int opt, int lhs) 
{
  char name[NAME_MAXL];
  int rep,n ;
  if ( (rep = ListFollowExtract(stack,rhs,opt,lhs)) < 0 ) return rep; 
  if ( rep == 3 ) 
    {
      /* last extraction : here O can be anything */ 
      nsp_build_funcname("extractelts",&stack,stack.first+1,1,name);
      if ((n=nsp_eval_func(NULLOBJ,name,2,stack,stack.first+1,2,0,1)) < 0)
	{
	  return RET_BUG;
	}
    }
  nsp_void_object_destroy(&NthObj(1));
  NSP_OBJECT(NthObj(2))->ret_pos = 1;
  return 1;
}

/*
 * H(x)
 */

static int int_htfind_elt(Stack stack, int rhs, int opt, int lhs)
{
  if ( rhs == 2 && IsListObj(stack,2) ) 
    {
      return int_ht_extract_l(stack,rhs,opt,lhs);
    }
  else 
    {
      return int_htfind(stack,rhs,opt,lhs);
    }
}


/*
 * L1 == L2 
 */

static int int_ht_eq(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *L1,*L2;
  NspBMatrix *B;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L1 = GetHash(stack,1)) == NULLHASH ) return RET_BUG;
  if ((L2 = GetHash(stack,2)) == NULLHASH ) return RET_BUG;
  if (( B =nsp_hash_equal(L1,L2))==NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;      
}


/*
 * L1 <> L2 
 */

static int int_ht_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *L1,*L2;
  NspBMatrix *B;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L1 = GetHash(stack,1)) == NULLHASH ) return RET_BUG;
  if ((L2 = GetHash(stack,2)) == NULLHASH ) return RET_BUG;
  if (( B =nsp_hash_not_equal(L1,L2))==NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;      
}

/*
 * l2h(L)
 */

int int_hcreate_from_list(Stack stack, int rhs, int opt, int lhs)
{
  NspList *L;
  NspHash *H;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((L = GetList(stack,1)) == NULLLIST) return RET_BUG;
  if ((H = nsp_hcreate_from_list(NVOID,-1,L))== NULLHASH) return RET_BUG;
  MoveObj(stack,1,(NspObject *) H);
  return 1;
} 


/*
 * Extract all the elements of the hash table 
 * an store them on the stack as hopts 
 * this is usefull for passing optionnal arguments 
 * H=hcreate(opt1=,...)
 * f(....,H(:))
 */ 

static int int_hash_as_options(Stack stack, int rhs, int opt, int lhs)
{
  int count =0,i=0;
  NspHash *H;
  NspObject *O;
  CheckRhs(1,1);
  if ((H = GetHash(stack,1)) == NULLHASH) return RET_BUG;
  if ( Ocheckname(NthObj(1),NVOID) ) 
    {
      Scierror("Error:\t%s must have a name\n",ArgPosition(count));
      return RET_BUG;
    }
  while (1) 
    {
      int rep = nsp_hash_get_next_object(H,&i,&O);
      if ( O != NULLOBJ )
	{ 
	  NspHobj *Opt = HoptCreate(nsp_object_get_name(O),O);
	  if ( Opt == NULL) return RET_BUG;
	  NthObj(count+1)= NSP_OBJECT(Opt);
	  count++;
	}
      if ( rep == FAIL) break;
    }
  for ( i = 1 ; i <= count ; i++) 
    {
      NSP_OBJECT(NthObj(i))->ret_pos = i;
    }
  return count;
}


/*
 *
 */

static OpTab Hash_func[]={
  {"hash",int_htcreate},  
  {"hash_create",int_htcreate},  		/* could be renamed hcreate(10)
                                         * hcreate(x=67,y=89,...) 
                                         * hcreate(n,x=67,y=89,...) 
                                         * hcreate(n) 
					 */
  {"extract_h",int_htfind},    		/* H(a1,a2,....) les ai peuvent etre ['a','b'] */
  {"extractelts_h",int_htfind_elt},     /* H('a') , H(['a','b']) */
  {"setrowscols_h",int_hash_set_attribute},/* H('a')= o or H.a = o */
  {"eq_h_h",int_ht_eq},
  {"ne_h_h",int_ht_neq},
  {"l2h",int_hcreate_from_list},
  {"resize2vect_h", int_hash_as_options}, /* H(:) */
  {(char *) 0, NULL}
};

/* call ith function in the Hash interface */

int Hash_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Hash_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void Hash_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Hash_func[i].name;
  *f = Hash_func[i].fonc;
}


