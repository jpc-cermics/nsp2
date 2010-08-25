/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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

#define  DClass_Private 
#include "nsp/object.h"
#include "dclass.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/seval.h"

/**
 * SECTION:dclass
 * @title: Hash tables
 * @short_description: Hash tables with open addressing and double dclassing
 * @see_also: 
 *
 * <para>
 * A #NspDClass object is a dclass table which uses open adressing and double 
 * dclassing to access data. The dclass keys are strings and the size of the 
 * dclass table increases dynamically.
 * </para>
 **/

/* 
 * NspDClass inherits from NspObject 
 */

int nsp_type_dclass_id=0;
NspTypeDClass *nsp_type_dclass=NULL;

NspTypeDClass *new_type_dclass(type_mode mode)
{
  NspTypeDClass *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_dclass != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_dclass;
    }
  if ((type =  malloc(sizeof(NspTypeDClass))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = dclass_attrs ; 
  type->get_attrs = (attrs_func *) int_dclass_get_attribute; 
  type->set_attrs = (attrs_func *) int_dclass_set_attribute;
  type->methods = dclass_get_methods; 
  type->new = (new_func *) new_dclass;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for dclass */ 

  top->pr = (print_func *) nsp_dclass_print;                    
  top->dealloc = (dealloc_func *) nsp_dclass_destroy;
  top->copy  =  (copy_func *) nsp_dclass_copy;                   
  top->size  = (size_func *) dclass_size;                  
  top->s_type =  (s_type_func *) dclass_type_as_string;    
  top->sh_type = (sh_type_func *) dclass_type_short_string;
  top->info = (info_func *) nsp_dclass_info ;                    
  /* top->is_true = (is_true_func  *) DClassIsTrue; */
  /* top->loop = (loop_func *) dclass_loop_extract ; */
  top->path_extract = (path_func *) dclass_path_extract ; 
  top->get_from_obj = (get_from_obj_func *) nsp_dclass_object;
  top->eq  = (eq_func *) dclass_eq;
  top->neq  = (eq_func *) dclass_neq;
  top->save  = (save_func *) dclass_xdr_save;
  top->load  = (load_func *) dclass_xdr_load;

  /* specific methods for dclass */

  type->init = (init_func *) init_dclass;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_dclass_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_dclass
       */
      type->id =  nsp_type_dclass_id = nsp_new_type_id();
      nsp_type_dclass = type;
      if ( nsp_register_type(nsp_type_dclass) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_dclass(mode);
    }
  else 
    {
      type->id = nsp_type_dclass_id;
      return type;
    }
}
/*
 * initialize DClass instances 
 * locally and by calling initializer on parent class 
 */

static int init_dclass(NspDClass *o,NspTypeDClass *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of DClass 
 */

NspDClass *new_dclass() 
{
  NspDClass *loc; 
  /* type must exists */
  nsp_type_dclass = new_type_dclass(T_BASE);
  if ( (loc = malloc(sizeof(NspDClass)))== NULLDCLASS) return loc;
  /* initialize object */
  if ( init_dclass(loc,nsp_type_dclass) == FAIL) return NULLDCLASS;
  return loc;
}


/*----------------------------------------------
 * Object method redefined for DClass 
 *-----------------------------------------------*/

/*
 * size 
 */

static int dclass_size(NspDClass *H, int flag)
{
  switch (flag) 
    {
    case 0: return H->hash->filled;
    case 1: return H->hash->filled;
    case 2: return H->hash->hsize;
    }
  return 0;
}


/*
 * type as string 
 */

static char dclass_type_name[]="DClass";
/* static char dclass_short_type_name[]="h"; */

static char *dclass_type_as_string(void)
{
  return(dclass_type_name);
}

static char *dclass_type_short_string(NspObject *v)
{
  return ((NspDClass *)v)->type_name;
  /* return(dclass_short_type_name); */
}



/*
 * A == B 
 */

static int dclass_eq(NspDClass *A, NspObject *B)
{
  int rep;
  if ( check_cast(B,nsp_type_dclass_id) == FALSE) return FALSE ;
  rep = nsp_dclass_full_equal(A,(NspDClass *) B);
  return rep;
}

/*
 * A != B 
 */

static int dclass_neq(NspDClass *A, NspObject *B)
{
  int rep;
  if ( check_cast(B,nsp_type_dclass_id) == FALSE) return TRUE;
  rep = nsp_dclass_full_not_equal(A,(NspDClass *) B);
  return rep;
}

/* used for evaluation of H(exp1) in exps like H(exp1)(exp2)....(expn)= val 
 * note that H(exp1)= val          -> setrowscols
 *       and H(exp1)(.....) = val  -> pathextract(H,exp1) and then 
 *       iterate on the result 
 */

static NspObject *dclass_path_extract(NspDClass *H,int n, NspObject **Objs, int *copy)
{
  NspObject *O1;
  NspSMatrix *M;
  *copy = FALSE;
  /* faire une fonction ou macros pour un string XXXX **/
  if ( n != 1 ) return NULLOBJ ;
  if (( M =nsp_smatrix_object(Objs[0])) == NULLSMAT || M->mn != 1) return NULLOBJ ;
  if (nsp_hash_find(H->hash,M->S[0],&O1) == FAIL) return NULLOBJ ;
  return O1;
}

/*
 * save 
 */

static int dclass_xdr_save(XDR *xdrs, NspDClass *M)
{
#if 1 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_zz)) == FAIL) return FAIL;
#else
  if (nsp_xdr_save_i(xdrs, M->type->id) == FAIL)    return FAIL;
#endif 
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, M->type_name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs, NSP_OBJECT(M->hash) ) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspDClass  *dclass_xdr_load(XDR *xdrs)
{
  NspHash *H;
  static char name[NAME_MAXL];
  static char type_name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLDCLASS;
  if (nsp_xdr_load_string(xdrs,type_name,NAME_MAXL) == FAIL) return NULLDCLASS;
  /* initial mxn matrix with unallocated elements **/
  if ((H = (NspHash *) nsp_object_xdr_load(xdrs)) == NULLHASH )
    return NULLDCLASS;
  return nsp_dclass_create(name,H,type_name);
}

/*
 * delete 
 */


void nsp_dclass_destroy(NspDClass *H)
{
  /* last entry is at M->hsize ! */
  if ( H != NULLDCLASS )
    {
      nsp_string_destroy(&H->type_name);
      nsp_hash_destroy(H->hash);
    }
  free(H);
}

/*
 * info 
 */

void nsp_dclass_info(NspDClass *H, int indent,char *name,int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;

  if ( rec_level <= user_pref.pr_depth ) 
    {
      /* recursively call info on elements */
      Sciprintf1(indent,"%s\t=\t\t%s\n",(strcmp(pname,NVOID) != 0) ? pname : "",
		 H->type_name);
      nsp_hash_info(H->hash,indent+2,NULL,rec_level+1);
    }
  else
    {
      Sciprintf1(indent,"%s\t= ...\t\t%s\n",(strcmp(pname,NVOID) != 0) ? pname : "",
		 H->type_name);
    }
} 

/*
 * print 
 */

int nsp_dclass_print(NspDClass *H, int indent,char *name, int rec_level)
{
  int rep = TRUE;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(H)->name;

  if (user_pref.pr_as_read_syntax)
    {
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    }
  else 
    {
      if ( rec_level <= user_pref.pr_depth ) 
	{
	  Sciprintf1(indent,"%s\t=\t\t%s\n",pname,H->type_name);
	  rep = nsp_hash_print(H->hash,indent+2,NULL,rec_level+1);
	}
      else
	{
	  Sciprintf1(indent,"%s\t= ...\t\t%s\n",pname,H->type_name);
	}
    }
  return rep;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for DClass objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspDClass   *nsp_dclass_object(NspObject *O)
{
  /* Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type **/
  if ( check_cast(O,nsp_type_dclass_id) == TRUE) return ((NspDClass *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_dclass));
  return(NULL);
}

int IsDClassObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_dclass_id);
}

int IsDClass(const NspObject *O)
{
  return nsp_object_type(O,nsp_type_dclass_id);
}

NspDClass  *GetDClassCopy(Stack stack, int i)
{
  if (  GetDClass(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspDClass  *GetDClass(Stack stack, int i)
{
  NspDClass *M;
  if (( M = nsp_dclass_object(NthObj(i))) == NULLDCLASS)
    ArgMessage(stack,i);
  return M;
}


/*-----------------------------------------------------
 * constructor 
 *-----------------------------------------------------*/

static NspDClass *nsp_dclass_create_void(char *name,NspTypeBase *type)
{
  NspDClass *H  = (type == NULL) ? new_dclass() : type->new();
  if ( H ==  NULLDCLASS)
  {
    Sciprintf("No more memory\n");
    return NULLDCLASS;
  }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLDCLASS;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->hash = NULL;
 H->type_name= NULL;
 return H;
}



NspDClass *nsp_dclass_create(char *name,NspHash *H, const char *type_name)
{
  NspDClass *loc = nsp_dclass_create_void(name,NULL);
  if ( loc== NULL) return NULLDCLASS;
  if (( loc->type_name =nsp_string_copy(type_name))== NULL) goto err;
  if ( (loc->hash =(NspHash *) nsp_object_copy_and_name("H",(NspObject *) H)) == NULLHASH )goto err;
  return loc;
 err:
  if (loc->type_name != NULL)  nsp_string_destroy(&loc->type_name);
  if (loc->hash != NULL) nsp_hash_destroy(loc->hash);
  free(loc);
  return NULL;
} 

/*
 * copy 
 */

NspDClass *nsp_dclass_copy(NspDClass *H)
{
  NspDClass *Loc;
  Loc = nsp_dclass_create_void(NVOID,NULL);
  if ( Loc == NULLDCLASS ) return NULLDCLASS;
  if (( Loc->type_name =nsp_string_copy(H->type_name))== NULL)
    goto err;
  if ( (Loc->hash =(NspHash *) nsp_object_copy_and_name("H",(NspObject *) H->hash)) == NULLHASH )
    goto err;
  return(Loc);
 err:
  if (Loc->type_name != NULL)  nsp_string_destroy(&Loc->type_name);
  if (Loc->hash != NULL) nsp_hash_destroy(Loc->hash);
  free(Loc);
  return NULL;
}

/*-------------------------------------------------------------------
 * wrappers for the DClass
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/*
 * Create a DClass Table 
 * hcreate(size, x1=val1,.....,xn=valn)
 * or 
 * hcreate( x1=val1,.....,xn=valn)
 */

static int int_dclass_create(Stack stack, int rhs, int opt, int lhs)
{
  char *type_name;
  NspDClass *Obj;
  NspHash *H;
  CheckStdRhs(2,100);
  if ( (H= GetHash(stack,1)) == NULLHASH ) return RET_BUG;
  if (( type_name = GetString(stack,2)) == ((char *) 0) ) return RET_BUG;
  /* Only optional arguments are given */ 
  if(( Obj = nsp_dclass_create(NVOID,H,type_name)) == NULLDCLASS) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(Obj));
  return 1;
} 

/*------------------------------------------------------
 * attributes  
 *------------------------------------------------------*/

/* return all the keys H.keys entered in the dclass table as a string matrice  */

static NspObject * int_dclass_get_keys(void *Hv, char *attr)
{
  return NULL;
}

static int int_dclass_set_keys(void *Hv,const char *attr, NspObject *O)
{
  Scierror("attribute __keys of dclass instances cannot be set !\n");
  return FAIL;
}

static AttrTab dclass_attrs[] = {
  { "__keys", 	int_dclass_get_keys , 	int_dclass_set_keys , 	NULL, NULL  },
  { (char *) 0, NULL, NULL , NULL , NULL }
};

/* 
 * get and set attributes are redefined for DClass Tables 
 * to access data stored in the dclass table 
 */

static int int_dclass_get_attribute(Stack stack, int rhs, int opt, int lhs)
{
  char *key;
  NspDClass *H;
  NspObject *O;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((H = GetDClass(stack,1)) == NULLDCLASS) return RET_BUG;
  if ((key = GetString(stack,2)) == (char*)0) return RET_BUG;  
  /* if (nsp_dclass_find_and_copy(H,key,&O) == OK )  */
  /* here we do not copy the object since we can be in the 
   * middle of the evaluation of an expression like H.x in 
   * H.x.y= 6;
   * XXX pb ça donne un plantage 
   */
  if (nsp_hash_find(H->hash,key,&O) == OK )
    {
      /* H('key') **/
      MoveObj(stack,1,O);
      return 1;      
    }
  /* Check now if key is an attribute of object DClass **/
  return int_get_attribute(stack,rhs,opt,lhs);
}

static int int_dclass_set_attribute(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  char *name;
  NspDClass *H;
  CheckRhs(3,3);
  CheckLhs(1,1);
  if ((H    = GetDClass(stack,1)) == NULLDCLASS) return RET_BUG;
  if ((name = GetString(stack,2)) == (char*)0) return RET_BUG;  
  if ( strcmp(name,"__attrs") == 0 || strcmp(name,"__keys") == 0 ) 
    {
      Scierror("%s should not be used as a dclass table entry\n",name);
      return RET_BUG;
    }
  if ((  O = nsp_get_object(stack,3)) == NULLOBJ ) return RET_BUG;
  if ( Ocheckname(O,NVOID)==FALSE )
    {
      if (( O =nsp_object_copy(O)) == NULLOBJ ) return RET_BUG;
      /* Object at position 3 had a name and is not an object pointed to 
       * we remove it from the stack since it could be destroyed 
       * during nsp_dclass_enter (ex in H;a = H.a) and should not be kept 
       * on the stack. 
       */
      if( IsHobj(NthObj(3))== FALSE )
	{
	  NthObj(3) = NULLOBJ;
	}
    }
  if (nsp_object_set_name(O,name) == FAIL) return RET_BUG;
  if (nsp_hash_enter(H->hash,O) == FAIL) return RET_BUG;
  NSP_OBJECT(H)->ret_pos = 1;
  return 1;
}


/*------------------------------------------------------
 * methods 
 *------------------------------------------------------*/
 
/*
 * Enters a copy of objects O in DClass Table
 * enter(H,O1,...,On)  or H.enter[O1,...,On]
 * H is changed 
 */

static int int_htenter(void *self, Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int i;
  NspDClass *H = self ;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( i = 1 ; i <= rhs ; i++) 
    {
      if ( Ocheckname(NthObj(i),NVOID) ) 
	{
	  Scierror("Error: Cannot add unamed variable in dclass table\n");
	  Scierror("\t%s of function %s\n",ArgPosition(rhs),NspFname(stack));
	  return RET_BUG;
	}
      /* A copy of object is added in the dclass table **/
      /* GetObj takes care of Hobj pointers **/
      if (( O =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
      if (nsp_object_set_name(O,nsp_object_get_name(NthObj(i))) == FAIL) return RET_BUG;
      if (nsp_hash_enter(H->hash,O) == FAIL) return RET_BUG;
      /* A copy of object is added in the dclass table **/
    }
  return 0;
}
 

/*
 * Delete entries given by string matrix S str from DClass Table
 * delete(H,['arg1',....,'argn']) or H.delete[['arg1',...,'argn']]
 */

static int int_htdelete(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int i,j;
  NspSMatrix *S;
  NspDClass *H = self ;
  CheckRhs(1,1000);
  CheckLhs(1,1);
  for ( j = 1; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	nsp_hash_remove(H->hash,S->S[i]);
    }
  return 0;
}

/*
 * Find Object with key str in DClassTable
 * Warning the object is returned (not a copy 
 *         of it) since this function is used 
 *         to change the DClasstable 
 */

static int int_meth_htfind(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  int i,j,count=0;
  NspDClass *H=self;
  NspObject *O;
  CheckRhs(1,1000);
  CheckLhs(1,1000);
  lhs=Max(lhs,1);
  for ( j = 1 ; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	{
	  if (nsp_hash_find_and_copy(H->hash,S->S[i],&O) == FAIL)   
	    {
	      Scierror("%s: key %s not found in dclass table \n",NspFname(stack));
	      nsp_void_object_destroy(&O);
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
  return count;
}

/*
 * checks if keys are present in dclass table 
 * results are returned in boolean matrice 
 */

static int int_meth_iskey(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *S;
  int i,j,count=0;
  NspDClass *H=self;
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
	  Res->B[i] = (nsp_hash_find(H->hash,S->S[i],&Ob) == FAIL)? FALSE:TRUE;
	}
      NthObj(rhs+ ++count) = NSP_OBJECT(Res) ;
      NSP_OBJECT(Res)->ret_pos = count;
      if (count == lhs) break;
    }
  return count;
}



static NspMethods dclass_methods[] = {
  { "delete", int_htdelete},
  { "enter", int_htenter},
  { "iskey", int_meth_iskey},
  { "find", int_meth_htfind},
  { (char *) 0, NULL}
};

static NspMethods *dclass_get_methods(void) { return dclass_methods;};

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
  NspSMatrix *S;
  int i,j,count=0;
  NspObject *O;

  NspDClass *H;
  CheckRhs(2,1000);
  CheckLhs(1,1);
  
  if ((H   = GetDClass(stack,1)) == NULLDCLASS) return RET_BUG;

  for ( j = 2 ; j <= rhs ; j++ )
    {
      if ((S = GetSMat(stack,j)) == NULLSMAT) return RET_BUG;        
      for ( i = 0 ; i < S->mn ; i++ ) 
	{
	  if (nsp_hash_find_and_copy(H->hash,S->S[i],&O) == FAIL)   
	    {
	      Scierror("%s: key %s not found in dclass table \n",NspFname(stack),S->S[i]);
	      nsp_void_object_destroy(&O);
	      return RET_BUG  ;
	    }
	  else
	    {
	      NthObj(rhs+ ++count) = O ;
	      NSP_OBJECT(O)->ret_pos = count;
	    }
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
  NspDClass *L1,*L2;
  NspBMatrix *B;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L1 = GetDClass(stack,1)) == NULLDCLASS ) return RET_BUG;
  if ((L2 = GetDClass(stack,2)) == NULLDCLASS ) return RET_BUG;
  if (( B =nsp_dclass_equal(L1,L2))==NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;      
}


/*
 * L1 <> L2 
 */

static int int_ht_neq(Stack stack, int rhs, int opt, int lhs)
{
  NspDClass *L1,*L2;
  NspBMatrix *B;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((L1 = GetDClass(stack,1)) == NULLDCLASS ) return RET_BUG;
  if ((L2 = GetDClass(stack,2)) == NULLDCLASS ) return RET_BUG;
  if (( B =nsp_dclass_not_equal(L1,L2))==NULLBMAT) return RET_BUG;
  MoveObj(stack,1,(NspObject *) B);
  return 1;      
}

/*
 * Extract all the elements of the dclass table 
 * an store them on the stack as hopts 
 * this is usefull for passing optionnal arguments 
 * H=hcreate(opt1=,...)
 * f(....,H(:))
 */ 

static int int_dclass_as_options(Stack stack, int rhs, int opt, int lhs)
{
  int count =0,i=0;
  NspDClass *H;
  NspObject *O;
  CheckRhs(1,1);
  if ((H = GetDClass(stack,1)) == NULLDCLASS) return RET_BUG;
  if ( Ocheckname(NthObj(1),NVOID) ) 
    {
      Scierror("Error:\t%s must have a name\n",ArgPosition(count));
      return RET_BUG;
    }
  while (1) 
    {
      int rep = nsp_hash_get_next_object(H->hash,&i,&O);
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

static OpTab DClass_func[]={
  {"class",int_dclass_create},  
  {"class_create",int_dclass_create},  		/* could be renamed hcreate(10)
                                         * hcreate(x=67,y=89,...) 
                                         * hcreate(n,x=67,y=89,...) 
                                         * hcreate(n) 
					 */
  {"extract_h",int_htfind},    		/* H(a1,a2,....) les ai peuvent etre ['a','b'] */
  {"extractelts_h",int_htfind_elt},     /* H('a') , H(['a','b']) */
  {"setrowscols_h",int_dclass_set_attribute},/* H('a')= o or H.a = o */
  {"eq_h_h",int_ht_eq},
  {"ne_h_h",int_ht_neq},
  {"resize2vect_h", int_dclass_as_options}, /* H(:) */
  {(char *) 0, NULL}
};

/* call ith function in the DClass interface */

int DClass_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(DClass_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
   (for adding or removing functions) **/

void DClass_Interf_Info(int i, char **fname, function (**f))
{
  *fname = DClass_func[i].name;
  *f = DClass_func[i].fonc;
}


/*
 * DClassTable Object in Scilab : 
 *    store (key,value) in a dclass table where key is always a string 
 *    and value is a pointer to a Scilab Object Obj
 */


/**
 *nsp_dclass_resize:
 * @H: 
 * @new_size: 
 * 
 * Resizes a  DClassTable
 *
 * Return value: %OK or %FAIL
 **/

int nsp_dclass_resize(NspDClass *H, unsigned int new_size)
{
  unsigned int i;
  NspHash *Loc;
  if ((Loc = nsp_hcreate(NVOID,new_size)) == NULLHASH ) return FAIL;
  for ( i =0 ; i <= H->hash->hsize ; i++) 
    {
      Hash_Entry *loc = ((Hash_Entry *)H->hash->htable) + i;
      if ( loc->used && loc->data != NULL)
	{
	  if ( nsp_hsearch(Loc,nsp_object_get_name(loc->data),&loc->data,H_ENTER) == FAIL) 
	    return FAIL;
	}
    }
  FREE(H->hash->htable);
  H->hash->htable = Loc->htable;
  H->hash->hsize  = Loc->hsize;
  H->hash->filled = Loc->filled;
  nsp_object_destroy_name(NSP_OBJECT(Loc));
  FREE(Loc);
  return OK;
}




/**
 *nsp_dclass_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * Return value: 
 **/

NspBMatrix  *nsp_dclass_equal(NspDClass *L1, NspDClass *L2)
{
  return nsp_hash_equal(L1->hash,L2->hash);
}


/**
 *nsp_dclass_not_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
 **/


NspBMatrix  *nsp_dclass_not_equal(NspDClass *L1, NspDClass *L2)
{
  return nsp_hash_not_equal(L1->hash,L2->hash);
} 


/**
 *nsp_dclass_full_equal:
 * @L1: 
 * @L2: 
 * 
 * nsp_dclass_equal(L1,L2)
 * if the two tables do not have the same length returns FALSE 
 * else returns and(L1(i)== L2(i)) 
 * 
 * 
 * Return value: 
 **/

int nsp_dclass_full_equal(NspDClass *L1, NspDClass *L2)
{
  return nsp_hash_full_equal(L1->hash,L2->hash);
} 

/**
 *nsp_dclass_full_not_equal:
 * @L1: 
 * @L2: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_dclass_full_not_equal(NspDClass *L1, NspDClass *L2)
{
  return nsp_hash_full_not_equal(L1->hash,L2->hash);
} 

